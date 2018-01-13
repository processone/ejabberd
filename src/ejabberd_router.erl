%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_router).

-behaviour(ejabberd_config).

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
	 handle_info/2, terminate/2, code_change/3, opt_type/1]).

%% Deprecated functions
-export([route/3, route_error/4]).
-deprecated([{route, 3}, {route_error, 4}]).

%% This value is used in SIP and Megaco for a transaction lifetime.
-define(IQ_TIMEOUT, 32000).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_router.hrl").
-include("xmpp.hrl").

-callback init() -> any().
-callback register_route(binary(), binary(), local_hint(),
			 undefined | pos_integer(), pid()) -> ok | {error, term()}.
-callback unregister_route(binary(), undefined | pos_integer(), pid()) -> ok | {error, term()}.
-callback find_routes(binary()) -> {ok, [#route{}]} | {error, any()}.
-callback get_all_routes() -> {ok, [binary()]} | {error, any()}.

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    ?GEN_SERVER:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec route(stanza()) -> ok.
route(Packet) ->
    try do_route(Packet)
    catch E:R ->
	    ?ERROR_MSG("failed to route packet:~n~s~nReason = ~p",
		       [xmpp:pp(Packet), {E, {R, erlang:get_stacktrace()}}])
    end.

-spec route(jid(), jid(), xmlel() | stanza()) -> ok.
route(#jid{} = From, #jid{} = To, #xmlel{} = El) ->
    try xmpp:decode(El, ?NS_CLIENT, [ignore_els]) of
	Pkt -> route(From, To, Pkt)
    catch _:{xmpp_codec, Why} ->
	    ?ERROR_MSG("failed to decode xml element ~p when "
		       "routing from ~s to ~s: ~s",
		       [El, jid:encode(From), jid:encode(To),
			xmpp:format_error(Why)])
    end;
route(#jid{} = From, #jid{} = To, Packet) ->
    case catch do_route(xmpp:set_from_to(Packet, From, To)) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end.

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
		    ?DEBUG("Route registered: ~s", [LDomain]),
		    ejabberd_hooks:run(route_registered, [LDomain]),
		    delete_cache(Mod, LDomain);
		{error, Err} ->
		    ?ERROR_MSG("Failed to register route ~s: ~p",
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
		    ?DEBUG("Route unregistered: ~s", [LDomain]),
		    ejabberd_hooks:run(route_unregistered, [LDomain]),
		    delete_cache(Mod, LDomain);
		{error, Err} ->
		    ?ERROR_MSG("Failed to unregister route ~s: ~p",
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
process_iq(#iq{to = To} = IQ) ->
    if To#jid.luser == <<"">> ->
	    ejabberd_local:process_iq(IQ);
       true ->
	    ejabberd_sm:process_iq(IQ)
    end.

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

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({route, Packet}, State) ->
    route(Packet),
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec do_route(stanza()) -> ok.
do_route(OrigPacket) ->
    ?DEBUG("route:~n~s", [xmpp:pp(OrigPacket)]),
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

-spec do_route(stanza(), #route{}) -> any().
do_route(Pkt, #route{local_hint = LocalHint,
		     pid = Pid}) when is_pid(Pid) ->
    case LocalHint of
	{apply, Module, Function} when node(Pid) == node() ->
	    Module:Function(Pkt);
	_ ->
	    Pid ! {route, Pkt}
    end;
do_route(_Pkt, _Route) ->
    ok.

-spec balancing_route(jid(), jid(), stanza(), [#route{}]) -> any().
balancing_route(From, To, Packet, Rs) ->
    LDstDomain = To#jid.lserver,
    Value = get_domain_balancing(From, To, LDstDomain),
    case get_component_number(LDstDomain) of
	undefined ->
	    case [R || R <- Rs, node(R#route.pid) == node()] of
		[] ->
		    R = lists:nth(erlang:phash(Value, length(Rs)), Rs),
		    do_route(Packet, R);
		LRs ->
		    R = lists:nth(erlang:phash(Value, length(LRs)), LRs),
		    do_route(Packet, R)
	    end;
	_ ->
	    SRs = lists:ukeysort(#route.local_hint, Rs),
	    R = lists:nth(erlang:phash(Value, length(SRs)), SRs),
	    do_route(Packet, R)
    end.

-spec get_component_number(binary()) -> pos_integer() | undefined.
get_component_number(LDomain) ->
    ejabberd_config:get_option({domain_balancing_component_number, LDomain}).

-spec get_domain_balancing(jid(), jid(), binary()) -> any().
get_domain_balancing(From, To, LDomain) ->
    case ejabberd_config:get_option({domain_balancing, LDomain}) of
	undefined -> p1_time_compat:system_time();
	random -> p1_time_compat:system_time();
	source -> jid:tolower(From);
	destination -> jid:tolower(To);
	bare_source -> jid:remove_resource(jid:tolower(From));
	bare_destination -> jid:remove_resource(jid:tolower(To))
    end.

-spec get_backend() -> module().
get_backend() ->
    DBType = ejabberd_config:get_option(
	       router_db_type,
	       ejabberd_config:default_ram_db(?MODULE)),
    list_to_atom("ejabberd_router_" ++ atom_to_list(DBType)).

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
	false ->
	    ejabberd_config:get_option(
	      router_use_cache,
	      ejabberd_config:use_cache(global))
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
    MaxSize = ejabberd_config:get_option(
		router_cache_size,
		ejabberd_config:cache_size(global)),
    CacheMissed = ejabberd_config:get_option(
		    router_cache_missed,
		    ejabberd_config:cache_missed(global)),
    LifeTime = case ejabberd_config:get_option(
		      router_cache_life_time,
		      ejabberd_config:cache_life_time(global)) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec clean_cache(node()) -> ok.
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

-type domain_balancing() :: random | source | destination |
			    bare_source | bare_destination.
-spec opt_type(domain_balancing) -> fun((domain_balancing()) -> domain_balancing());
	      (domain_balancing_component_number) -> fun((pos_integer()) -> pos_integer());
	      (router_db_type) -> fun((atom()) -> atom());
	      (router_use_cache) -> fun((boolean()) -> boolean());
	      (router_cache_missed) -> fun((boolean()) -> boolean());
	      (router_cache_size) -> fun((timeout()) -> timeout());
	      (router_cache_life_time) -> fun((timeout()) -> timeout());
	      (atom()) -> [atom()].
opt_type(domain_balancing) ->
    fun (random) -> random;
	(source) -> source;
	(destination) -> destination;
	(bare_source) -> bare_source;
	(bare_destination) -> bare_destination
    end;
opt_type(domain_balancing_component_number) ->
    fun (N) when is_integer(N), N > 1 -> N end;
opt_type(router_db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
opt_type(O) when O == router_use_cache; O == router_cache_missed ->
    fun(B) when is_boolean(B) -> B end;
opt_type(O) when O == router_cache_size; O == router_cache_life_time ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
opt_type(_) ->
    [domain_balancing, domain_balancing_component_number,
     router_db_type, router_use_cache, router_cache_size,
     router_cache_missed, router_cache_life_time].
