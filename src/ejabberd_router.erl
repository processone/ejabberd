%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-module(ejabberd_router).

-author('alexey@process-one.net').

-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).

%% API
-export([route/1,
	 route_error/2,
	 route_iq/2,
	 route_iq/3,
	 route_iq/4,
	 register_route/2,
	 register_route/3,
	 register_route/4,
	 register_routes/1,
	 host_of_route/1,
	 process_iq/1,
	 unregister_route/1,
	 unregister_route/2,
	 unregister_routes/1,
	 get_all_routes/0,
	 is_my_route/1,
	 is_my_host/1,
	 clean_cache/1,
	 config_reloaded/0,
	 get_backend/0]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

%% Deprecated functions
-export([route/3, route_error/4]).
-deprecated([{route, 3}, {route_error, 4}]).

%% This value is used in SIP and Megaco for a transaction lifetime.
-define(IQ_TIMEOUT, 32000).
-define(CALL_TIMEOUT, timer:minutes(10)).

-include("logger.hrl").
-include("ejabberd_router.hrl").
-include_lib("xmpp/include/xmpp.hrl").



-callback init() -> any().
-callback register_route(binary(), binary(), local_hint(),
			 undefined | pos_integer(), pid()) -> ok | {error, term()}.
-callback unregister_route(binary(), undefined | pos_integer(), pid()) -> ok | {error, term()}.
-callback find_routes(binary()) -> {ok, [#route{}]} | {error, any()}.
-callback get_all_routes() -> {ok, [binary()]} | {error, any()}.

-record(state, {route_monitors = #{} :: #{{binary(), pid()} => reference()}}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    ?GEN_SERVER:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec route(stanza()) -> ok.
route(Packet) ->
    try do_route(Packet)
    catch
        Class:Reason:StackTrace ->
            ?ERROR_MSG("Failed to route packet:~n~ts~n** ~ts",
                       [xmpp:pp(Packet),
                        misc:format_exception(2, Class, Reason, StackTrace)])
    end.

-spec route(jid(), jid(), xmlel() | stanza()) -> ok.
route(#jid{} = From, #jid{} = To, #xmlel{} = El) ->
    try xmpp:decode(El, ?NS_CLIENT, [ignore_els]) of
	Pkt -> route(From, To, Pkt)
    catch _:{xmpp_codec, Why} ->
	    ?ERROR_MSG("Failed to decode xml element ~p when "
		       "routing from ~ts to ~ts: ~ts",
		       [El, jid:encode(From), jid:encode(To),
			xmpp:format_error(Why)])
    end;
route(#jid{} = From, #jid{} = To, Packet) ->
    route(xmpp:set_from_to(Packet, From, To)).

-spec route_error(stanza(), stanza_error()) -> ok.
route_error(Packet, Err) ->
    Type = xmpp:get_type(Packet),
    if Type == error; Type == result ->
	    ok;
       true ->
	    route(xmpp:make_error(Packet, Err))
    end.

%% Route the error packet only if the originating packet is not an error itself.
%% RFC3920 9.3.1
-spec route_error(jid(), jid(), xmlel(), xmlel()) -> ok;
		 (jid(), jid(), stanza(), stanza_error()) -> ok.
route_error(From, To, #xmlel{} = ErrPacket, #xmlel{} = OrigPacket) ->
    #xmlel{attrs = Attrs} = OrigPacket,
    case <<"error">> == fxml:get_attr_s(<<"type">>, Attrs) of
      false -> route(From, To, ErrPacket);
      true -> ok
    end;
route_error(From, To, Packet, #stanza_error{} = Err) ->
    Type = xmpp:get_type(Packet),
    if Type == error; Type == result ->
	    ok;
       true ->
	    route(From, To, xmpp:make_error(Packet, Err))
    end.

-spec route_iq(iq(), fun((iq() | timeout) -> any())) -> ok.
route_iq(IQ, Fun) ->
    route_iq(IQ, Fun, undefined, ?IQ_TIMEOUT).

-spec route_iq(iq(), term(), pid() | atom()) -> ok.
route_iq(IQ, State, Proc) ->
    route_iq(IQ, State, Proc, ?IQ_TIMEOUT).

-spec route_iq(iq(), term(), pid() | atom(), undefined | non_neg_integer()) -> ok.
route_iq(IQ, State, Proc, undefined) ->
    route_iq(IQ, State, Proc, ?IQ_TIMEOUT);
route_iq(IQ, State, Proc, Timeout) ->
    ejabberd_iq:route(IQ, Proc, State, Timeout).

-spec register_route(binary(), binary()) -> ok.
register_route(Domain, ServerHost) ->
    register_route(Domain, ServerHost, undefined).

-spec register_route(binary(), binary(), local_hint() | undefined) -> ok.
register_route(Domain, ServerHost, LocalHint) ->
    register_route(Domain, ServerHost, LocalHint, self()).

-spec register_route(binary(), binary(), local_hint() | undefined, pid()) -> ok.
register_route(Domain, ServerHost, LocalHint, Pid) ->
    case {jid:nameprep(Domain), jid:nameprep(ServerHost)} of
	{error, _} ->
	    erlang:error({invalid_domain, Domain});
	{_, error} ->
	    erlang:error({invalid_domain, ServerHost});
	{LDomain, LServerHost} ->
	    Mod = get_backend(),
	    case Mod:register_route(LDomain, LServerHost, LocalHint,
				    get_component_number(LDomain), Pid) of
		ok ->
		    ?DEBUG("Route registered: ~ts", [LDomain]),
		    monitor_route(LDomain, Pid),
		    ejabberd_hooks:run(route_registered, [LDomain]),
		    delete_cache(Mod, LDomain);
		{error, Err} ->
		    ?ERROR_MSG("Failed to register route ~ts: ~p",
			       [LDomain, Err])
	    end
    end.

-spec register_routes([{binary(), binary()}]) -> ok.
register_routes(Domains) ->
    lists:foreach(fun ({Domain, ServerHost}) -> register_route(Domain, ServerHost)
		  end,
		  Domains).

-spec unregister_route(binary()) -> ok.
unregister_route(Domain) ->
    unregister_route(Domain, self()).

-spec unregister_route(binary(), pid()) -> ok.
unregister_route(Domain, Pid) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    Mod = get_backend(),
	    case Mod:unregister_route(
		   LDomain, get_component_number(LDomain), Pid) of
		ok ->
		    ?DEBUG("Route unregistered: ~ts", [LDomain]),
		    demonitor_route(LDomain, Pid),
		    ejabberd_hooks:run(route_unregistered, [LDomain]),
		    delete_cache(Mod, LDomain);
		{error, Err} ->
		    ?ERROR_MSG("Failed to unregister route ~ts: ~p",
			       [LDomain, Err])
	    end
    end.

-spec unregister_routes([binary()]) -> ok.
unregister_routes(Domains) ->
    lists:foreach(fun (Domain) -> unregister_route(Domain)
		  end,
		  Domains).

-spec find_routes(binary()) -> [#route{}].
find_routes(Domain) ->
    Mod = get_backend(),
    case use_cache(Mod) of
	true ->
	    case ets_cache:lookup(
		   ?ROUTES_CACHE, {route, Domain},
		   fun() ->
			   case Mod:find_routes(Domain) of
			       {ok, Rs} when Rs /= [] ->
				   {ok, Rs};
			       _ ->
				   error
			   end
		   end) of
		{ok, Rs} -> Rs;
		error -> []
	    end;
	false ->
	    case Mod:find_routes(Domain) of
		{ok, Rs} -> Rs;
		_ -> []
	    end
    end.

-spec get_all_routes() -> [binary()].
get_all_routes() ->
    Mod = get_backend(),
    case use_cache(Mod) of
	true ->
	    case ets_cache:lookup(
		   ?ROUTES_CACHE, routes,
		   fun() ->
			   case Mod:get_all_routes() of
			       {ok, Rs} when Rs /= [] ->
				   {ok, Rs};
			       _ ->
				   error
			   end
		   end) of
		{ok, Rs} -> Rs;
		error -> []
	    end;
	false ->
	    case Mod:get_all_routes() of
		{ok, Rs} -> Rs;
		_ -> []
	    end
    end.

-spec host_of_route(binary()) -> binary().
host_of_route(Domain) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    case find_routes(LDomain) of
		[#route{server_host = ServerHost}|_] ->
		    ServerHost;
		_ ->
		    erlang:error({unregistered_route, Domain})
	    end
    end.

-spec is_my_route(binary()) -> boolean().
is_my_route(Domain) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    find_routes(LDomain) /= []
    end.

-spec is_my_host(binary()) -> boolean().
is_my_host(Domain) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    case find_routes(LDomain) of
		[#route{server_host = LDomain}|_] -> true;
		_ -> false
	    end
    end.

-spec process_iq(iq()) -> any().
process_iq(IQ) ->
    gen_iq_handler:handle(IQ).

-spec config_reloaded() -> ok.
config_reloaded() ->
    Mod = get_backend(),
    init_cache(Mod).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50),
    Mod = get_backend(),
    init_cache(Mod),
    Mod:init(),
    clean_cache(),
    {ok, #state{}}.

handle_call({monitor, Domain, Pid}, _From, State) ->
    MRefs = State#state.route_monitors,
    MRefs1 = case maps:is_key({Domain, Pid}, MRefs) of
		 true -> MRefs;
		 false ->
		     MRef = erlang:monitor(process, Pid),
		     MRefs#{{Domain, Pid} => MRef}
	     end,
    {reply, ok, State#state{route_monitors = MRefs1}};
handle_call({demonitor, Domain, Pid}, _From, State) ->
    MRefs = State#state.route_monitors,
    MRefs1 = case maps:find({Domain, Pid}, MRefs) of
		 {ok, MRef} ->
		     erlang:demonitor(MRef, [flush]),
		     maps:remove({Domain, Pid}, MRefs);
		 error ->
		     MRefs
	     end,
    {reply, ok, State#state{route_monitors = MRefs1}};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({route, Packet}, State) ->
    route(Packet),
    {noreply, State};
handle_info({'DOWN', MRef, _, Pid, Info}, State) ->
    MRefs = maps:filter(
	      fun({Domain, P}, M) when P == Pid, M == MRef ->
		      ?DEBUG("Process ~p with route registered to ~ts "
			     "has terminated unexpectedly with reason: ~p",
			     [P, Domain, Info]),
		      try unregister_route(Domain, Pid)
		      catch _:_ -> ok
		      end,
		      false;
		 (_, _) ->
		      true
	      end, State#state.route_monitors),
    {noreply, State#state{route_monitors = MRefs}};
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 50).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec do_route(stanza()) -> ok.
do_route(OrigPacket1) ->
    ?DEBUG("Route:~n~ts", [xmpp:pp(OrigPacket1)]),
    OrigPacket = process_privilege_iq(OrigPacket1),
    case ejabberd_hooks:run_fold(filter_packet, OrigPacket, []) of
	drop ->
	    ok;
	Packet ->
	    case ejabberd_iq:dispatch(Packet) of
		true ->
		    ok;
		false ->
		    To = xmpp:get_to(Packet),
		    LDstDomain = To#jid.lserver,
		    case find_routes(LDstDomain) of
			[] ->
			    ejabberd_s2s:route(Packet);
			[Route] ->
			    do_route(Packet, Route);
			Routes ->
			    From = xmpp:get_from(Packet),
			    balancing_route(From, To, Packet, Routes)
		    end,
		    ok
	    end
    end.

%% @format-begin
process_privilege_iq(Packet) ->
    Type = xmpp:get_type(Packet),
    case xmpp:get_meta(Packet, privilege_iq, none) of
        {OriginalId, OriginalHost, ReplacedJid} when (Type == result) or (Type == error) ->
            Privilege = #privilege{forwarded = #forwarded{sub_els = [Packet]}},
            #iq{type = xmpp:get_type(Packet),
                id = OriginalId,
                to = jid:make(OriginalHost),
                from = ReplacedJid,
                sub_els = [Privilege]};
        _ ->
            Packet
    end.
%% @format-end

-spec do_route(stanza(), #route{}) -> any().
do_route(Pkt, #route{local_hint = LocalHint,
		     pid = Pid}) when is_pid(Pid) ->
    case LocalHint of
	{apply, Module, Function} when node(Pid) == node() ->
	    Module:Function(Pkt);
	_ ->
	    ejabberd_cluster:send(Pid, {route, Pkt})
    end;
do_route(_Pkt, _Route) ->
    ok.

-spec balancing_route(jid(), jid(), stanza(), [#route{}]) -> any().
balancing_route(From, To, Packet, Rs) ->
    case get_domain_balancing(From, To, To#jid.lserver) of
	undefined ->
	    Value = erlang:system_time(),
	    case [R || R <- Rs, node(R#route.pid) == node()] of
		[] ->
		    R = lists:nth(erlang:phash2(Value, length(Rs))+1, Rs),
		    do_route(Packet, R);
		LRs ->
		    R = lists:nth(erlang:phash2(Value, length(LRs))+1, LRs),
		    do_route(Packet, R)
	    end;
	Value ->
	    SRs = lists:ukeysort(#route.local_hint, Rs),
	    R = lists:nth(erlang:phash2(Value, length(SRs))+1, SRs),
	    do_route(Packet, R)
    end.

-spec get_component_number(binary()) -> pos_integer() | undefined.
get_component_number(LDomain) ->
    M = ejabberd_option:domain_balancing(),
    case maps:get(LDomain, M, undefined) of
	undefined -> undefined;
	Opts -> maps:get(component_number, Opts)
    end.

-spec get_domain_balancing(jid(), jid(), binary()) -> integer() | ljid() | undefined.
get_domain_balancing(From, To, LDomain) ->
    M = ejabberd_option:domain_balancing(),
    case maps:get(LDomain, M, undefined) of
	undefined -> undefined;
	Opts ->
	    case maps:get(type, Opts, random) of
		random -> erlang:system_time();
		source -> jid:tolower(From);
		destination -> jid:tolower(To);
		bare_source -> jid:remove_resource(jid:tolower(From));
		bare_destination -> jid:remove_resource(jid:tolower(To))
	    end
    end.

-spec monitor_route(binary(), pid()) -> ok.
monitor_route(Domain, Pid) ->
    ?GEN_SERVER:call(?MODULE, {monitor, Domain, Pid}, ?CALL_TIMEOUT).

-spec demonitor_route(binary(), pid()) -> ok.
demonitor_route(Domain, Pid) ->
    case whereis(?MODULE) == self() of
	true ->
	    ok;
	false ->
	    ?GEN_SERVER:call(?MODULE, {demonitor, Domain, Pid}, ?CALL_TIMEOUT)
    end.

-spec get_backend() -> module().
get_backend() ->
    DBType = ejabberd_option:router_db_type(),
    list_to_existing_atom("ejabberd_router_" ++ atom_to_list(DBType)).

-spec cache_nodes(module()) -> [node()].
cache_nodes(Mod) ->
    case erlang:function_exported(Mod, cache_nodes, 0) of
	true -> Mod:cache_nodes();
	false -> ejabberd_cluster:get_nodes()
    end.

-spec use_cache(module()) -> boolean().
use_cache(Mod) ->
    case erlang:function_exported(Mod, use_cache, 0) of
	true -> Mod:use_cache();
	false -> ejabberd_option:router_use_cache()
    end.

-spec delete_cache(module(), binary()) -> ok.
delete_cache(Mod, Domain) ->
    case use_cache(Mod) of
	true ->
	    ets_cache:delete(?ROUTES_CACHE, {route, Domain}, cache_nodes(Mod)),
	    ets_cache:delete(?ROUTES_CACHE, routes, cache_nodes(Mod));
	false ->
	    ok
    end.

-spec init_cache(module()) -> ok.
init_cache(Mod) ->
    case use_cache(Mod) of
	true ->
	    ets_cache:new(?ROUTES_CACHE, cache_opts());
	false ->
	    ets_cache:delete(?ROUTES_CACHE)
    end.

-spec cache_opts() -> [proplists:property()].
cache_opts() ->
    MaxSize = ejabberd_option:router_cache_size(),
    CacheMissed = ejabberd_option:router_cache_missed(),
    LifeTime = ejabberd_option:router_cache_life_time(),
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec clean_cache(node()) -> non_neg_integer().
clean_cache(Node) ->
    ets_cache:filter(
      ?ROUTES_CACHE,
      fun(_, error) ->
	      false;
	 (routes, _) ->
	      false;
	 ({route, _}, {ok, Rs}) ->
	      not lists:any(
		    fun(#route{pid = Pid}) ->
			    node(Pid) == Node
		    end, Rs)
      end).

-spec clean_cache() -> ok.
clean_cache() ->
    ejabberd_cluster:eval_everywhere(?MODULE, clean_cache, [node()]).
