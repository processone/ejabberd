%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Route local packets
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_local).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([route/3, route_iq/4, route_iq/5,
	 process_iq_reply/3, register_iq_handler/4,
	 register_iq_handler/5, register_iq_response_handler/4,
	 register_iq_response_handler/5, unregister_iq_handler/2,
	 unregister_iq_response_handler/2, refresh_iq_handlers/0,
	 bounce_resource_packet/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-record(state, {}).

-record(iq_response, {id = <<"">> :: binary(),
                      module :: atom(),
                      function :: atom() | fun(),
                      timer = make_ref() :: reference()}).

-define(IQTABLE, local_iqtable).

%% This value is used in SIP and Megaco for a transaction lifetime.
-define(IQ_TIMEOUT, 32000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
      #iq{xmlns = XMLNS} ->
	  Host = To#jid.lserver,
	  case ets:lookup(?IQTABLE, {XMLNS, Host}) of
	    [{_, Module, Function}] ->
		ResIQ = Module:Function(From, To, IQ),
		if ResIQ /= ignore ->
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
		   true -> ok
		end;
	    [{_, Module, Function, Opts}] ->
		gen_iq_handler:handle(Host, Module, Function, Opts,
				      From, To, IQ);
	    [] ->
		Err = jlib:make_error_reply(Packet,
					    ?ERR_FEATURE_NOT_IMPLEMENTED),
		ejabberd_router:route(To, From, Err)
	  end;
      reply ->
	  IQReply = jlib:iq_query_or_response_info(Packet),
	  process_iq_reply(From, To, IQReply);
      _ ->
	  Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	  ejabberd_router:route(To, From, Err),
	  ok
    end.

process_iq_reply(From, To, #iq{id = ID} = IQ) ->
    case get_iq_callback(ID) of
      {ok, undefined, Function} -> Function(IQ), ok;
      {ok, Module, Function} ->
	  Module:Function(From, To, IQ), ok;
      _ -> nothing
    end.

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end.

route_iq(From, To, IQ, F) ->
    route_iq(From, To, IQ, F, undefined).

route_iq(From, To, #iq{type = Type} = IQ, F, Timeout)
    when is_function(F) ->
    Packet = if Type == set; Type == get ->
		     ID = randoms:get_string(),
		     Host = From#jid.lserver,
		     register_iq_response_handler(Host, ID, undefined, F, Timeout),
		     jlib:iq_to_xml(IQ#iq{id = ID});
		true ->
		     jlib:iq_to_xml(IQ)
	     end,
    ejabberd_router:route(From, To, Packet).

register_iq_response_handler(Host, ID, Module,
			     Function) ->
    register_iq_response_handler(Host, ID, Module, Function,
				 undefined).

register_iq_response_handler(_Host, ID, Module,
			     Function, Timeout0) ->
    Timeout = case Timeout0 of
		undefined -> ?IQ_TIMEOUT;
		N when is_integer(N), N > 0 -> N
	      end,
    TRef = erlang:start_timer(Timeout, ejabberd_local, ID),
    mnesia:dirty_write(#iq_response{id = ID,
				    module = Module,
				    function = Function,
				    timer = TRef}).

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_local !
      {register_iq_handler, Host, XMLNS, Module, Fun}.

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_local !
      {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

unregister_iq_response_handler(_Host, ID) ->
    catch get_iq_callback(ID), ok.

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_local ! {unregister_iq_handler, Host, XMLNS}.

refresh_iq_handlers() ->
    ejabberd_local ! refresh_iq_handlers.

bounce_resource_packet(From, To, Packet) ->
    Err = jlib:make_error_reply(Packet,
				?ERR_ITEM_NOT_FOUND),
    ejabberd_router:route(To, From, Err),
    stop.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    lists:foreach(fun (Host) ->
			  ejabberd_router:register_route(Host,
							 {apply, ?MODULE,
							  route}),
			  ejabberd_hooks:add(local_send_to_resource_hook, Host,
					     ?MODULE, bounce_resource_packet,
					     100)
		  end,
		  ?MYHOSTS),
    catch ets:new(?IQTABLE, [named_table, public]),
    update_table(),
    mnesia:create_table(iq_response,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, iq_response)}]),
    mnesia:add_table_copy(iq_response, node(), ram_copies),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
    Reply = ok, {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end,
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module,
	     Function},
	    State) ->
    ets:insert(?IQTABLE, {{XMLNS, Host}, Module, Function}),
    catch mod_disco:register_feature(Host, XMLNS),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module,
	     Function, Opts},
	    State) ->
    ets:insert(?IQTABLE,
	       {{XMLNS, Host}, Module, Function, Opts}),
    catch mod_disco:register_feature(Host, XMLNS),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS},
	    State) ->
    case ets:lookup(?IQTABLE, {XMLNS, Host}) of
      [{_, Module, Function, Opts}] ->
	  gen_iq_handler:stop_iq_handler(Module, Function, Opts);
      _ -> ok
    end,
    ets:delete(?IQTABLE, {XMLNS, Host}),
    catch mod_disco:unregister_feature(Host, XMLNS),
    {noreply, State};
handle_info(refresh_iq_handlers, State) ->
    lists:foreach(fun (T) ->
			  case T of
			    {{XMLNS, Host}, _Module, _Function, _Opts} ->
				catch mod_disco:register_feature(Host, XMLNS);
			    {{XMLNS, Host}, _Module, _Function} ->
				catch mod_disco:register_feature(Host, XMLNS);
			    _ -> ok
			  end
		  end,
		  ets:tab2list(?IQTABLE)),
    {noreply, State};
handle_info({timeout, _TRef, ID}, State) ->
    process_iq_timeout(ID),
    {noreply, State};
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_route(From, To, Packet) ->
    ?DEBUG("local route~n\tfrom ~p~n\tto ~p~n\tpacket "
	   "~P~n",
	   [From, To, Packet, 8]),
    if To#jid.luser /= <<"">> ->
	   ejabberd_sm:route(From, To, Packet);
       To#jid.lresource == <<"">> ->
	   #xmlel{name = Name} = Packet,
	   case Name of
	     <<"iq">> -> process_iq(From, To, Packet);
	     <<"message">> -> ok;
	     <<"presence">> -> ok;
	     _ -> ok
	   end;
       true ->
	   #xmlel{attrs = Attrs} = Packet,
	   case xml:get_attr_s(<<"type">>, Attrs) of
	     <<"error">> -> ok;
	     <<"result">> -> ok;
	     _ ->
		 ejabberd_hooks:run(local_send_to_resource_hook,
				    To#jid.lserver, [From, To, Packet])
	   end
    end.

update_table() ->
    case catch mnesia:table_info(iq_response, attributes) of
	[id, module, function] ->
	    mnesia:delete_table(iq_response);
	[id, module, function, timer] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end.

get_iq_callback(ID) ->
    case mnesia:dirty_read(iq_response, ID) of
	[#iq_response{module = Module, timer = TRef,
		      function = Function}] ->
	    cancel_timer(TRef),
	    mnesia:dirty_delete(iq_response, ID),
	    {ok, Module, Function};
	_ ->
	    error
    end.

process_iq_timeout(ID) ->
    spawn(fun process_iq_timeout/0) ! ID.

process_iq_timeout() ->
    receive
	ID ->
	    case get_iq_callback(ID) of
		{ok, undefined, Function} ->
		    Function(timeout);
		_ ->
		    ok
	    end
    after 5000 ->
	    ok
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.
