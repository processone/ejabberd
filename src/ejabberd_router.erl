%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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
	 find_routes/0,
	 get_backend/0]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3, opt_type/1]).

%% Deprecated functions
-export([route/3, route_error/4]).
-deprecated([{route, 3}, {route_error, 4}]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_router.hrl").
-include("xmpp.hrl").

-callback init() -> any().
-callback register_route(binary(), binary(), local_hint(),
			 undefined | pos_integer(), pid()) -> ok | {error, term()}.
-callback unregister_route(binary(), undefined | pos_integer(), pid()) -> ok | {error, term()}.
-callback find_routes(binary()) -> [#route{}].
-callback find_routes() -> [#route{}].
-callback host_of_route(binary()) -> {ok, binary()} | error.
-callback is_my_route(binary()) -> boolean().
-callback is_my_host(binary()) -> boolean().
-callback get_all_routes() -> [binary()].

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
		    ?DEBUG("Route registered: ~s", [LDomain]);
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
		    ?DEBUG("Route unregistered: ~s", [LDomain]);
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

-spec get_all_routes() -> [binary()].
get_all_routes() ->
    Mod = get_backend(),
    Mod:get_all_routes().

-spec find_routes() -> [#route{}].
find_routes() ->
    Mod = get_backend(),
    Mod:find_routes().

-spec host_of_route(binary()) -> binary().
host_of_route(Domain) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    Mod = get_backend(),
	    case Mod:host_of_route(LDomain) of
		{ok, ServerHost} -> ServerHost;
		error -> erlang:error({unregistered_route, Domain})
	    end
    end.

-spec is_my_route(binary()) -> boolean().
is_my_route(Domain) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    Mod = get_backend(),
	    Mod:is_my_route(LDomain)
    end.

-spec is_my_host(binary()) -> boolean().
is_my_host(Domain) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    Mod = get_backend(),
	    Mod:is_my_host(LDomain)
    end.

-spec process_iq(iq()) -> any().
process_iq(#iq{to = To} = IQ) ->
    if To#jid.luser == <<"">> ->
	    ejabberd_local:process_iq(IQ);
       true ->
	    ejabberd_sm:process_iq(IQ)
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    Mod = get_backend(),
    Mod:init(),
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
    ok.

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
	    To = xmpp:get_to(Packet),
	    LDstDomain = To#jid.lserver,
	    Mod = get_backend(),
	    case Mod:find_routes(LDstDomain) of
		[] ->
		    ejabberd_s2s:route(Packet);
		[Route] ->
		    do_route(Packet, Route);
		Routes ->
		    From = xmpp:get_from(Packet),
		    balancing_route(From, To, Packet, Routes)
	    end,
	    ok
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
    ejabberd_config:get_option(
      {domain_balancing_component_number, LDomain},
      fun(N) when is_integer(N), N > 1 -> N end,
      undefined).

-spec get_domain_balancing(jid(), jid(), binary()) -> any().
get_domain_balancing(From, To, LDomain) ->
    case ejabberd_config:get_option(
	   {domain_balancing, LDomain}, fun(D) when is_atom(D) -> D end) of
	undefined -> p1_time_compat:system_time();
	random -> p1_time_compat:system_time();
	source -> jid:tolower(From);
	destination -> jid:tolower(To);
	bare_source -> jid:remove_resource(jid:tolower(From));
	bare_destination -> jid:remove_resource(jid:tolower(To))
    end.

-spec get_backend() -> module().
get_backend() ->
    DBType = case ejabberd_config:get_option(
		    router_db_type,
		    fun(T) -> ejabberd_config:v_db(?MODULE, T) end) of
		 undefined ->
		     ejabberd_config:default_ram_db(?MODULE);
		 T ->
		     T
	     end,
    list_to_atom("ejabberd_router_" ++ atom_to_list(DBType)).

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
opt_type(_) ->
    [domain_balancing, domain_balancing_component_number,
     router_db_type].
