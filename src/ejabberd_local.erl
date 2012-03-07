%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Route local packets
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_local).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([route/3,
	 route_iq/4,
	 route_iq/5,
	 process_iq_reply/3,
	 register_iq_handler/4,
	 register_iq_handler/5,
	 register_iq_response_handler/4,
	 register_iq_response_handler/5,
	 unregister_iq_handler/2,
	 unregister_iq_response_handler/2,
	 refresh_iq_handlers/0,
	 bounce_resource_packet/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

-record(state, {}).

-record(iq_response, {id, module, function, timer}).

-define(IQTABLE, local_iqtable).

%% This value is used in SIP and Megaco for a transaction lifetime.
-define(IQ_TIMEOUT, 32000).

% These are the namespace already declared by the stream opening. This is
% used at serialization time.
-define(DEFAULT_NS, ?NS_JABBER_CLIENT).
-define(PREFIXED_NS,
        [{?NS_XMPP, ?NS_XMPP_pfx}, {?NS_DIALBACK, ?NS_DIALBACK_pfx}]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_iq(From, To, Packet) ->
    case exmpp_iq:xmlel_to_iq(Packet) of
	#iq{kind = request, ns = XMLNS} = IQ_Rec ->
	    Host = exmpp_jid:prep_domain(To),
	    case ets:lookup(?IQTABLE, {XMLNS, ejabberd:normalize_host(Host)}) of
		[{_, Module, Function}] ->
		    ResIQ = Module:Function(From, To, IQ_Rec),
		    if
			ResIQ /= ignore ->
			    Reply = exmpp_iq:iq_to_xmlel(ResIQ, To, From),
			    ejabberd_router:route(To, From, Reply);
			true ->
			    ok
		    end;
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:handle(Host, Module, Function, Opts,
					  From, To, IQ_Rec);
		[] ->
		    case ets:lookup(?IQTABLE, {XMLNS, global}) of
			[{_, Module, Function, Opts}] ->
			    gen_iq_handler:handle(
			      global, Module, Function, Opts,
			      From, To, IQ_Rec);
			[] ->
			    Err = exmpp_iq:error(Packet, 'feature-not-implemented'),
			    ejabberd_router:route(To, From, Err)
		    end
	    end;
        #iq{kind = response} = IQReply ->
 	    %%IQReply = jlib:iq_query_or_response_info(IQ_Rec),
 	    process_iq_reply(From, To, IQReply);
	_ ->
	    Err = exmpp_iq:error(Packet, 'bad-request'),
	    ejabberd_router:route(To, From, Err),
	    ok
    end.

process_iq_reply(From, To, #iq{id = ID} = IQ) ->
    case get_iq_callback(ID) of
	{ok, undefined, Function} ->
	    Function(IQ),
	    ok;
	{ok, Module, Function} ->
	    Module:Function(From, To, IQ),
	    ok;
	_ ->
	    nothing
    end.
  
%% #xmlelement{} used for retro-compatibility
route(FromOld, ToOld, #xmlelement{} = PacketOld) ->
    catch throw(for_stacktrace), % To have a stacktrace.
    io:format("~nLOCAL: old #xmlelement:~n~p~n~p~n~n",
      [PacketOld, erlang:get_stacktrace()]),
    % XXX OLD FORMAT: From, To, Packet.
    From = jlib:from_old_jid(FromOld),
    To = jlib:from_old_jid(ToOld),
    Packet = exmpp_xml:xmlelement_to_xmlel(PacketOld, [?NS_JABBER_CLIENT],
      [{?NS_XMPP, ?NS_XMPP_pfx}]),
    route(From, To, Packet);
route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end.

route_iq(From, To, IQ, F) ->
    route_iq(From, To, IQ, F, undefined).

route_iq(From, To, #iq{type = Type} = IQ, F, Timeout) when is_function(F) ->
    Packet = if Type == set; Type == get ->
		     ID = list_to_binary(ejabberd_router:make_id()),
		     Host = exmpp_jid:prep_domain(From),
		     register_iq_response_handler(Host, ID, undefined, F, Timeout),
		     exmpp_iq:iq_to_xmlel(IQ#iq{id = ID});
		true ->
		     exmpp_iq:iq_to_xmlel(IQ)
	     end,
    ejabberd_router:route(From, To, Packet).

register_iq_response_handler(Host, ID, Module, Function) ->
    register_iq_response_handler(Host, ID, Module, Function, undefined).

register_iq_response_handler(_Host, ID, Module, Function, Timeout0) ->
    Timeout = case Timeout0 of
		  undefined ->
		      ?IQ_TIMEOUT;
		  N when is_integer(N), N > 0 ->
		      N
	      end,
    TRef = erlang:start_timer(Timeout, ejabberd_local, ID),
    ets:insert(iq_response, #iq_response{id = ID,
					 module = Module,
					 function = Function,
					 timer = TRef}).

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_local ! {register_iq_handler, Host, XMLNS, Module, Fun}.

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_local ! {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

unregister_iq_response_handler(_Host, ID) ->
    catch get_iq_callback(ID),
    ok.

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_local ! {unregister_iq_handler, Host, XMLNS}.

refresh_iq_handlers() ->
    ejabberd_local ! refresh_iq_handlers.

bounce_resource_packet(From, To, Packet) ->
    Err = exmpp_stanza:reply_with_error(Packet, 'item-not-found'),
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
    lists:foreach(
      fun(Host) ->
	      ejabberd_router:register_route(Host, {apply, ?MODULE, route})
      end, ?MYHOSTS),
    ejabberd_hooks:add(local_send_to_resource_hook, global,
			?MODULE, bounce_resource_packet, 100),
    catch ets:new(?IQTABLE, [named_table, public]),
    mnesia:delete_table(iq_response),
    catch ets:new(iq_response, [named_table, public,
				{keypos, #iq_response.id}]),
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
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

%% #xmlelement{} used for retro-compatibility
handle_info({route, FromOld, ToOld, #xmlelement{} = PacketOld}, State) ->
    catch throw(for_stacktrace), % To have a stacktrace.
    io:format("~nLOCAL: old #xmlelement:~n~p~n~p~n~n",
      [PacketOld, erlang:get_stacktrace()]),
    % XXX OLD FORMAT: From, To, Packet.
    From = jlib:from_old_jid(FromOld),
    To = jlib:from_old_jid(ToOld),
    Packet = exmpp_xml:xmlelement_to_xmlel(PacketOld, [?NS_JABBER_CLIENT],
      [{?NS_XMPP, ?NS_XMPP_pfx}]),
    handle_info({route, From, To, Packet}, State);
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(?IQTABLE, {{XMLNS, ejabberd:normalize_host(Host)}, Module, Function}),
    catch mod_disco:register_feature(Host, XMLNS),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function, Opts}, State) ->
    ets:insert(?IQTABLE, {{XMLNS, ejabberd:normalize_host(Host)}, Module, Function, Opts}),
    catch mod_disco:register_feature(Host, XMLNS),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS}, State) ->
    case ets:lookup(?IQTABLE, {XMLNS, ejabberd:normalize_host(Host)}) of
	[{_, Module, Function, Opts}] ->
	    gen_iq_handler:stop_iq_handler(Module, Function, Opts);
	_ ->
	    ok
    end,
    ets:delete(?IQTABLE, {XMLNS, ejabberd:normalize_host(Host)}),
    catch mod_disco:unregister_feature(Host, XMLNS),
    {noreply, State};
handle_info(refresh_iq_handlers, State) ->
    lists:foreach(
      fun(T) ->
	      case T of
		  {{XMLNS, Host}, _Module, _Function, _Opts} ->
		      catch mod_disco:register_feature(Host, XMLNS);
		  {{XMLNS, Host}, _Module, _Function} ->
		      catch mod_disco:register_feature(Host, XMLNS);
		  _ ->
		      ok
	      end
      end, ets:tab2list(?IQTABLE)),
    {noreply, State};
handle_info({timeout, _TRef, ID}, State) ->
    spawn(fun() -> process_iq_timeout(ID) end),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_route(From, To, Packet) ->
    ?DEBUG("local route~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    
    LNode = exmpp_jid:prep_node(To),
    LResource = exmpp_jid:prep_resource(To),
    if
	LNode /= undefined ->
	    ejabberd_sm:route(From, To, Packet);
	LResource == undefined ->
	    case Packet of
		_ when ?IS_IQ(Packet) ->
		    process_iq(From, To, Packet);
		_ when ?IS_MESSAGE(Packet) ->
		    ok;
		_ when ?IS_PRESENCE(Packet) ->
		    ok;
		_ ->
		    ok
	    end;
	true ->
            case exmpp_stanza:get_type(Packet) of
		<<"error">> -> ok;
		<<"result">> -> ok;
		_ ->
		    ejabberd_hooks:run(local_send_to_resource_hook,
				       exmpp_jid:prep_domain(To),
				       [From, To, Packet])
	    end
	end.

get_iq_callback(ID) ->
    case ets:lookup(iq_response, ID) of
	[#iq_response{module = Module, timer = TRef,
		      function = Function}] ->
	    cancel_timer(TRef),
	    ets:delete(iq_response, ID),
	    {ok, Module, Function};
	_ ->
	    error
    end.

process_iq_timeout(ID) ->
    case get_iq_callback(ID) of
	{ok, undefined, Function} ->
	    Function(timeout);
	_ ->
	    ok
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
	false ->
	    receive
                {timeout, TRef, _} ->
                    ok
            after 0 ->
                    ok
            end;
        _ ->
            ok
    end.
