%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Route local packets
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
-export([start/0, start_link/0]).

-export([route/1, route_iq/2, route_iq/3, process_iq/1,
	 process_iq_reply/1, get_features/1,
	 register_iq_handler/5, register_iq_response_handler/4,
	 register_iq_response_handler/5, unregister_iq_handler/2,
	 unregister_iq_response_handler/2, bounce_resource_packet/1,
	 host_up/1, host_down/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("xmpp.hrl").

-record(state, {}).

-record(iq_response, {id = <<"">> :: binary(),
                      module :: atom(),
                      function :: atom() | fun(),
                      timer = make_ref() :: reference()}).

-define(IQTABLE, local_iqtable).

%% This value is used in SIP and Megaco for a transaction lifetime.
-define(IQ_TIMEOUT, 32000).

-type ping_timeout() :: non_neg_integer() | undefined.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    ChildSpec = {?MODULE, {?MODULE, start_link, []},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec process_iq(iq()) -> any().
process_iq(#iq{to = To, type = T, lang = Lang, sub_els = [El]} = Packet)
  when T == get; T == set ->
    XMLNS = xmpp:get_ns(El),
    Host = To#jid.lserver,
    case ets:lookup(?IQTABLE, {Host, XMLNS}) of
	[{_, Module, Function, Opts}] ->
	    gen_iq_handler:handle(Host, Module, Function, Opts, Packet);
	[] ->
	    Txt = <<"No module is handling this query">>,
	    Err = xmpp:err_service_unavailable(Txt, Lang),
	    ejabberd_router:route_error(Packet, Err)
    end;
process_iq(#iq{type = T, lang = Lang, sub_els = SubEls} = Packet)
  when T == get; T == set ->
    Txt = case SubEls of
	      [] -> <<"No child elements found">>;
	      _ -> <<"Too many child elements">>
	  end,
    Err = xmpp:err_bad_request(Txt, Lang),
    ejabberd_router:route_error(Packet, Err);
process_iq(#iq{type = T} = Packet) when T == result; T == error ->
    process_iq_reply(Packet).

-spec process_iq_reply(iq()) -> any().
process_iq_reply(#iq{id = ID} = IQ) ->
    case get_iq_callback(ID) of
      {ok, undefined, Function} -> Function(IQ), ok;
      {ok, Module, Function} ->
	  Module:Function(IQ), ok;
      _ -> nothing
    end.

-spec route(stanza()) -> any().
route(Packet) ->
    try do_route(Packet)
    catch E:R ->
	    ?ERROR_MSG("failed to route packet:~n~s~nReason = ~p",
		       [xmpp:pp(Packet), {E, {R, erlang:get_stacktrace()}}])
    end.

-spec route_iq(iq(), function()) -> any().
route_iq(IQ, F) ->
    route_iq(IQ, F, undefined).

-spec route_iq(iq(), function(), ping_timeout()) -> any().
route_iq(#iq{from = From, type = Type} = IQ, F, Timeout)
    when is_function(F) ->
    Packet = if Type == set; Type == get ->
		     ID = randoms:get_string(),
		     Host = From#jid.lserver,
		     register_iq_response_handler(Host, ID, undefined, F, Timeout),
		     IQ#iq{id = ID};
		true ->
		     IQ
	     end,
    ejabberd_router:route(Packet).

-spec register_iq_response_handler(binary(), binary(), module(),
				   atom() | function()) -> any().
register_iq_response_handler(Host, ID, Module,
			     Function) ->
    register_iq_response_handler(Host, ID, Module, Function,
				 undefined).

-spec register_iq_response_handler(binary(), binary(), module(),
				   atom() | function(), ping_timeout()) -> any().
register_iq_response_handler(_Host, ID, Module,
			     Function, Timeout0) ->
    Timeout = case Timeout0 of
		undefined -> ?IQ_TIMEOUT;
		N when is_integer(N), N > 0 -> N
	      end,
    TRef = erlang:start_timer(Timeout, ?MODULE, ID),
    mnesia:dirty_write(#iq_response{id = ID,
				    module = Module,
				    function = Function,
				    timer = TRef}).

-spec register_iq_handler(binary(), binary(), module(), function(),
			  gen_iq_handler:opts()) -> ok.
register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    gen_server:cast(?MODULE,
		    {register_iq_handler, Host, XMLNS, Module, Fun, Opts}).

-spec unregister_iq_response_handler(binary(), binary()) -> ok.
unregister_iq_response_handler(_Host, ID) ->
    catch get_iq_callback(ID), ok.

-spec unregister_iq_handler(binary(), binary()) -> ok.
unregister_iq_handler(Host, XMLNS) ->
    gen_server:cast(?MODULE, {unregister_iq_handler, Host, XMLNS}).

-spec bounce_resource_packet(stanza()) -> ok | stop.
bounce_resource_packet(#presence{to = #jid{lresource = <<"">>}}) ->
    ok;
bounce_resource_packet(#message{to = #jid{lresource = <<"">>}, type = headline}) ->
    ok;
bounce_resource_packet(Packet) ->
    Lang = xmpp:get_lang(Packet),
    Txt = <<"No available resource found">>,
    Err = xmpp:err_item_not_found(Txt, Lang),
    ejabberd_router:route_error(Packet, Err),
    stop.

-spec get_features(binary()) -> [binary()].
get_features(Host) ->
    get_features(ets:next(?IQTABLE, {Host, <<"">>}), Host, []).

get_features({Host, XMLNS}, Host, XMLNSs) ->
    get_features(ets:next(?IQTABLE, {Host, XMLNS}), Host, [XMLNS|XMLNSs]);
get_features(_, _, XMLNSs) ->
    XMLNSs.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    lists:foreach(fun host_up/1, ?MYHOSTS),
    ejabberd_hooks:add(host_up, ?MODULE, host_up, 10),
    ejabberd_hooks:add(host_down, ?MODULE, host_down, 100),
    catch ets:new(?IQTABLE, [named_table, public, ordered_set,
			     {read_concurrency, true}]),
    update_table(),
    ejabberd_mnesia:create(?MODULE, iq_response,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, iq_response)}]),
    mnesia:add_table_copy(iq_response, node(), ram_copies),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_cast({register_iq_handler, Host, XMLNS, Module,
	     Function, Opts},
	    State) ->
    ets:insert(?IQTABLE,
	       {{Host, XMLNS}, Module, Function, Opts}),
    {noreply, State};
handle_cast({unregister_iq_handler, Host, XMLNS},
	    State) ->
    case ets:lookup(?IQTABLE, {Host, XMLNS}) of
      [{_, Module, Function, Opts}] ->
	  gen_iq_handler:stop_iq_handler(Module, Function, Opts);
      _ -> ok
    end,
    ets:delete(?IQTABLE, {Host, XMLNS}),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, Packet}, State) ->
    route(Packet),
    {noreply, State};
handle_info({timeout, _TRef, ID}, State) ->
    process_iq_timeout(ID),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lists:foreach(fun host_down/1, ?MYHOSTS),
    ejabberd_hooks:delete(host_up, ?MODULE, host_up, 10),
    ejabberd_hooks:delete(host_down, ?MODULE, host_down, 100),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec do_route(stanza()) -> any().
do_route(Packet) ->
    ?DEBUG("local route:~n~s", [xmpp:pp(Packet)]),
    Type = xmpp:get_type(Packet),
    To = xmpp:get_to(Packet),
    if To#jid.luser /= <<"">> ->
	    ejabberd_sm:route(Packet);
       is_record(Packet, iq), To#jid.lresource == <<"">> ->
	    process_iq(Packet);
       Type == result; Type == error ->
	    ok;
       true ->
	    ejabberd_hooks:run(local_send_to_resource_hook,
			       To#jid.lserver, [Packet])
    end.

-spec update_table() -> ok.
update_table() ->
    case catch mnesia:table_info(iq_response, attributes) of
	[id, module, function] ->
	    mnesia:delete_table(iq_response),
	    ok;
	[id, module, function, timer] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end.

host_up(Host) ->
    Owner = case whereis(?MODULE) of
		undefined -> self();
		Pid -> Pid
	    end,
    ejabberd_router:register_route(Host, Host, {apply, ?MODULE, route}, Owner),
    ejabberd_hooks:add(local_send_to_resource_hook, Host,
		       ?MODULE, bounce_resource_packet, 100).

host_down(Host) ->
    Owner = case whereis(?MODULE) of
		undefined -> self();
		Pid -> Pid
	    end,
    ejabberd_router:unregister_route(Host, Owner),
    ejabberd_hooks:delete(local_send_to_resource_hook, Host,
			  ?MODULE, bounce_resource_packet, 100).

-spec get_iq_callback(binary()) -> {ok, module(), atom() | function()} | error.
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

-spec process_iq_timeout(binary()) -> any().
process_iq_timeout(ID) ->
    spawn(fun process_iq_timeout/0) ! ID.

-spec process_iq_timeout() -> any().
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

-spec cancel_timer(reference()) -> ok.
cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.
