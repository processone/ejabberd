%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 28 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_router_redis).
-behaviour(ejabberd_router).

%% API
-export([init/0, register_route/5, unregister_route/3, find_routes/1,
	 host_of_route/1, is_my_route/1, is_my_host/1, get_all_routes/0,
	 find_routes/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_router.hrl").

-define(ROUTES_KEY, "ejabberd:routes").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    clean_table().

register_route(Domain, ServerHost, LocalHint, _, Pid) ->
    DomKey = domain_key(Domain),
    PidKey = term_to_binary(Pid),
    T = term_to_binary({ServerHost, LocalHint}),
    case ejabberd_redis:qp([["HSET", DomKey, PidKey, T],
			    ["SADD", ?ROUTES_KEY, Domain]]) of
	[{ok, _}, {ok, _}] ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to register route in redis: ~p", [Err]),
	    Err
    end.

unregister_route(Domain, _, Pid) ->
    DomKey = domain_key(Domain),
    PidKey = term_to_binary(Pid),
    try
	{ok, _} = ejabberd_redis:q(["HDEL", DomKey, PidKey]),
	{ok, Num} = ejabberd_redis:q(["HLEN", DomKey]),
	case binary_to_integer(Num) of
	    0 ->
		{ok, _} = ejabberd_redis:q(["SREM", ?ROUTES_KEY, Domain]),
		ok;
	    _ ->
		ok
	end
    catch _:{badmatch, Err} ->
	    ?ERROR_MSG("failed to unregister route in redis: ~p", [Err]),
	    Err
    end.

find_routes(Domain) ->
    DomKey = domain_key(Domain),
    case ejabberd_redis:q(["HGETALL", DomKey]) of
	{ok, Vals} ->
	    decode_routes(Domain, Vals);
	Err ->
	    ?ERROR_MSG("failed to find routes in redis: ~p", [Err]),
	    []
    end.

host_of_route(Domain) ->
    DomKey = domain_key(Domain),
    case ejabberd_redis:q(["HGETALL", DomKey]) of
	{ok, [_, Data|_]} ->
	    {ServerHost, _} = binary_to_term(Data),
	    {ok, ServerHost};
	{ok, []} ->
	    error;
	Err ->
	    ?ERROR_MSG("failed to get host of route in redis: ~p", [Err]),
	    error
    end.

is_my_route(Domain) ->
    case ejabberd_redis:q(["SISMEMBER", ?ROUTES_KEY, Domain]) of
	{ok, <<"1">>} -> true;
	{ok, _} -> false;
	Err ->
	    ?ERROR_MSG("failed to check route in redis: ~p", [Err]),
	    false
    end.

is_my_host(Domain) ->
    {ok, Domain} == host_of_route(Domain).

get_all_routes() ->
    case ejabberd_redis:q(["SMEMBERS", ?ROUTES_KEY]) of
	{ok, Routes} ->
	    Routes;
	Err ->
	    ?ERROR_MSG("failed to fetch routes from redis: ~p", [Err]),
	    []
    end.

find_routes() ->
    lists:flatmap(
      fun(Domain) ->
	      DomKey = domain_key(Domain),
	      case ejabberd_redis:q(["HGETALL", DomKey]) of
		  {ok, Vals} ->
		      decode_routes(Domain, Vals);
		  Err ->
		      ?ERROR_MSG("failed to fetch routes from redis: ~p",
				 [Err]),
		      []
	      end
      end, get_all_routes()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_table() ->
    lists:foreach(
      fun(#route{domain = Domain, pid = Pid}) when node(Pid) == node() ->
	      unregister_route(Domain, undefined, Pid);
	 (_) ->
	      ok
      end, find_routes()).

domain_key(Domain) ->
    <<"ejabberd:route:", Domain/binary>>.

decode_routes(Domain, [Pid, Data|Vals]) ->
    {ServerHost, LocalHint} = binary_to_term(Data),
    [#route{domain = Domain,
	    pid = binary_to_term(Pid),
	    server_host = ServerHost,
	    local_hint = LocalHint}|
     decode_routes(Domain, Vals)];
decode_routes(_, []) ->
    [].
