%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Route local packets
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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

-export([route/1, process_iq/1,
	 get_features/1,
	 register_iq_handler/4,
	 unregister_iq_handler/2,
	 bounce_resource_packet/1,
	 host_up/1, host_down/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

%% deprecated functions: use ejabberd_router:route_iq/3,4
-export([route_iq/2, route_iq/3]).
-deprecated([{route_iq, 2}, {route_iq, 3}]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("xmpp.hrl").

-record(state, {}).

-define(IQTABLE, local_iqtable).

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
	[{_, Module, Function}] ->
	    gen_iq_handler:handle(Host, Module, Function, Packet);
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
process_iq(#iq{type = T}) when T == result; T == error ->
    ok.

-spec route(stanza()) -> any().
route(Packet) ->
    try do_route(Packet)
    catch E:R ->
	    ?ERROR_MSG("failed to route packet:~n~s~nReason = ~p",
		       [xmpp:pp(Packet), {E, {R, erlang:get_stacktrace()}}])
    end.

-spec route_iq(iq(), function()) -> ok.
route_iq(IQ, Fun) ->
    route_iq(IQ, Fun, undefined).

-spec route_iq(iq(), function(), undefined | non_neg_integer()) -> ok.
route_iq(IQ, Fun, Timeout) ->
    ejabberd_router:route_iq(IQ, Fun, undefined, Timeout).

-spec register_iq_handler(binary(), binary(), module(), function()) -> ok.
register_iq_handler(Host, XMLNS, Module, Fun) ->
    gen_server:cast(?MODULE,
		    {register_iq_handler, Host, XMLNS, Module, Fun}).

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
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_cast({register_iq_handler, Host, XMLNS, Module, Function},
	    State) ->
    ets:insert(?IQTABLE,
	       {{Host, XMLNS}, Module, Function}),
    {noreply, State};
handle_cast({unregister_iq_handler, Host, XMLNS},
	    State) ->
    ets:delete(?IQTABLE, {Host, XMLNS}),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, Packet}, State) ->
    route(Packet),
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
    catch mnesia:delete_table(iq_response),
    ok.

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
