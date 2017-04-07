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
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hset(DomKey, PidKey, T),
		   ejabberd_redis:sadd(?ROUTES_KEY, [Domain])
	   end) of
	{ok, _} ->
	    ok;
	{error, _} ->
	    {error, db_failure}
    end.

unregister_route(Domain, _, Pid) ->
    DomKey = domain_key(Domain),
    PidKey = term_to_binary(Pid),
    try
	{ok, Num} = ejabberd_redis:hdel(DomKey, [PidKey]),
	if Num > 0 ->
		{ok, Len} = ejabberd_redis:hlen(DomKey),
		if Len == 0 ->
			{ok, _} = ejabberd_redis:multi(
				    fun() ->
					    ejabberd_redis:del([DomKey]),
					    ejabberd_redis:srem(?ROUTES_KEY, [Domain])
				    end),
			ok;
		   true ->
			ok
		end;
	   true ->
		ok
	end
    catch _:{badmatch, {error, _}} ->
	    {error, db_failure}
    end.

find_routes(Domain) ->
    DomKey = domain_key(Domain),
    case ejabberd_redis:hgetall(DomKey) of
	{ok, Vals} ->
	    decode_routes(Domain, Vals);
	{error, _} ->
	    []
    end.

host_of_route(Domain) ->
    DomKey = domain_key(Domain),
    case ejabberd_redis:hgetall(DomKey) of
	{ok, [{_Pid, Data}|_]} ->
	    {ServerHost, _} = binary_to_term(Data),
	    {ok, ServerHost};
	_ ->
	    error
    end.

is_my_route(Domain) ->
    case ejabberd_redis:sismember(?ROUTES_KEY, Domain) of
	{ok, Bool} ->
	    Bool;
	{error, _} ->
	    false
    end.

is_my_host(Domain) ->
    {ok, Domain} == host_of_route(Domain).

get_all_routes() ->
    case ejabberd_redis:smembers(?ROUTES_KEY) of
	{ok, Routes} ->
	    Routes;
	{error, _} ->
	    []
    end.

find_routes() ->
    lists:flatmap(fun find_routes/1, get_all_routes()).

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

decode_routes(Domain, Vals) ->
    lists:map(
      fun({Pid, Data}) ->
	      {ServerHost, LocalHint} = binary_to_term(Data),
	      #route{domain = Domain,
		     pid = binary_to_term(Pid),
		     server_host = ServerHost,
		     local_hint = LocalHint}
      end, Vals).
