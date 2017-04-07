%%%-------------------------------------------------------------------
%%% File    : ejabberd_sm_redis.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 11 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(ejabberd_sm_redis).

-behaviour(ejabberd_config).

-behaviour(ejabberd_sm).

-export([init/0, set_session/1, delete_session/4,
	 get_sessions/0, get_sessions/1, get_sessions/2,
	 get_sessions/3, opt_type/1]).

-include("ejabberd.hrl").
-include("ejabberd_sm.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    clean_table().

-spec set_session(#session{}) -> ok.
set_session(Session) ->
    T = term_to_binary(Session),
    USKey = us_to_key(Session#session.us),
    SIDKey = sid_to_key(Session#session.sid),
    ServKey = server_to_key(element(2, Session#session.us)),
    USSIDKey = us_sid_to_key(Session#session.us, Session#session.sid),
    ejabberd_redis:multi(
      fun() ->
	      ejabberd_redis:hset(USKey, SIDKey, T),
	      ejabberd_redis:hset(ServKey, USSIDKey, T)
      end),
    ok.

-spec delete_session(binary(), binary(), binary(), sid()) ->
			    {ok, #session{}} | {error, notfound}.
delete_session(LUser, LServer, _LResource, SID) ->
    USKey = us_to_key({LUser, LServer}),
    case ejabberd_redis:hgetall(USKey) of
	{ok, Vals} ->
	    Ss = decode_session_list(Vals),
	    case lists:keyfind(SID, #session.sid, Ss) of
		false ->
		    {error, notfound};
		Session ->
		    SIDKey = sid_to_key(SID),
		    ServKey = server_to_key(element(2, Session#session.us)),
		    USSIDKey = us_sid_to_key(Session#session.us, SID),
		    ejabberd_redis:multi(
		      fun() ->
			      ejabberd_redis:hdel(USKey, [SIDKey]),
			      ejabberd_redis:hdel(ServKey, [USSIDKey])
		      end),
		    {ok, Session}
	    end;
	{error, _} ->
	    {error, notfound}
    end.

-spec get_sessions() -> [#session{}].
get_sessions() ->
    lists:flatmap(
      fun(LServer) ->
	      get_sessions(LServer)
      end, ejabberd_sm:get_vh_by_backend(?MODULE)).

-spec get_sessions(binary()) -> [#session{}].
get_sessions(LServer) ->
    ServKey = server_to_key(LServer),
    case ejabberd_redis:hgetall(ServKey) of
	{ok, Vals} ->
	    decode_session_list(Vals);
	{error, _} ->
	    []
    end.

-spec get_sessions(binary(), binary()) -> [#session{}].
get_sessions(LUser, LServer) ->
    USKey = us_to_key({LUser, LServer}),
    case ejabberd_redis:hgetall(USKey) of
	{ok, Vals} ->
	    decode_session_list(Vals);
	{error, _} ->
	    []
    end.

-spec get_sessions(binary(), binary(), binary()) ->
    [#session{}].
get_sessions(LUser, LServer, LResource) ->
    USKey = us_to_key({LUser, LServer}),
    case ejabberd_redis:hgetall(USKey) of
	{ok, Vals} ->
	    [S || S <- decode_session_list(Vals),
		  element(3, S#session.usr) == LResource];
	{error, _} ->
	    []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
iolist_to_list(IOList) ->
    binary_to_list(iolist_to_binary(IOList)).

us_to_key({LUser, LServer}) ->
    <<"ejabberd:sm:", LUser/binary, "@", LServer/binary>>.

server_to_key(LServer) ->
    <<"ejabberd:sm:", LServer/binary>>.

us_sid_to_key(US, SID) ->
    term_to_binary({US, SID}).

sid_to_key(SID) ->
    term_to_binary(SID).

decode_session_list(Vals) ->
  [binary_to_term(Val) || {_, Val} <- Vals].

clean_table() ->
    ?INFO_MSG("Cleaning Redis SM table...", []),
    try
	lists:foreach(
	  fun(LServer) ->
		  ServKey = server_to_key(LServer),
		  {ok, Vals} = ejabberd_redis:hkeys(ServKey),
		  {ok, _} =
		      ejabberd_redis:multi(
			fun() ->
				lists:foreach(
				  fun(USSIDKey) ->
					  {US, SID} = binary_to_term(USSIDKey),
					  if node(element(2, SID)) == node() ->
						  USKey = us_to_key(US),
						  SIDKey = sid_to_key(SID),
						  ejabberd_redis:hdel(ServKey, [USSIDKey]),
						  ejabberd_redis:hdel(USKey, [SIDKey]);
					     true ->
						  ok
					  end
				  end, Vals)
			end)
	  end, ejabberd_sm:get_vh_by_backend(?MODULE))
    catch _:{badmatch, {error, _}} ->
	    ?ERROR_MSG("failed to clean redis c2s sessions", [])
    end.

opt_type(redis_connect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_db) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
opt_type(redis_password) -> fun iolist_to_list/1;
opt_type(redis_port) ->
    fun (P) when is_integer(P), P > 0, P < 65536 -> P end;
opt_type(redis_reconnect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_server) -> fun iolist_to_list/1;
opt_type(_) ->
    [redis_connect_timeout, redis_db, redis_password,
     redis_port, redis_reconnect_timeout, redis_server].
