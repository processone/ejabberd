%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 30 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(mod_carboncopy_redis).
-behaviour(mod_carboncopy).

%% API
-export([init/2, enable/4, disable/3, list/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    clean_table().

enable(LUser, LServer, LResource, NS) ->
    USKey = us_key(LUser, LServer),
    NodeKey = node_key(),
    JID = jid:encode({LUser, LServer, LResource}),
    case ejabberd_redis:qp([["HSET", USKey, LResource, NS],
			    ["SADD", NodeKey, JID]]) of
	[{ok, _}, {ok, _}] ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to write in redis: ~p", [Err]),
	    Err
    end.

disable(LUser, LServer, LResource) ->
    USKey = us_key(LUser, LServer),
    NodeKey = node_key(),
    JID = jid:encode({LUser, LServer, LResource}),
    case ejabberd_redis:qp([["HDEL", USKey, LResource],
			    ["SREM", NodeKey, JID]]) of
	[{ok, _}, {ok, _}] ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to delete from redis: ~p", [Err]),
	    Err
    end.

list(LUser, LServer) ->
    USKey = us_key(LUser, LServer),
    case ejabberd_redis:q(["HGETALL", USKey]) of
	{ok, Vals} ->
	    decode_vals(Vals);
	Err ->
	    ?ERROR_MSG("failed to read from redis: ~p", [Err]),
	    []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_table() ->
    ?INFO_MSG("Cleaning Redis 'carboncopy' table...", []),
    NodeKey = node_key(),
    case ejabberd_redis:q(["SMEMBERS", NodeKey]) of
	{ok, JIDs} ->
	    lists:foreach(
	      fun(JID) ->
		      {U, S, R} = jid:split(jid:decode(JID)),
		      USKey = us_key(U, S),
		      case ejabberd_redis:q(["HDEL", USKey, R]) of
			  {ok, _} ->
			      ok;
			  Err ->
			      ?ERROR_MSG("failed to delete from redis: ~p",
					 [Err])
		      end
	      end, JIDs);
	Err ->
	    ?ERROR_MSG("failed to read from redis: ~p", [Err])
    end,
    case ejabberd_redis:q(["DEL", NodeKey]) of
	{ok, _} -> ok;
	Error -> ?ERROR_MSG("failed to delete from redis: ~p", [Error])
    end.

us_key(LUser, LServer) ->
    <<"ejabberd:carboncopy:users:", LUser/binary, $@, LServer/binary>>.

node_key() ->
    Node = erlang:atom_to_binary(node(), latin1),
    <<"ejabberd:carboncopy:nodes:", Node/binary>>.

decode_vals([Resource, NS|Vals]) ->
    [{Resource, NS}|decode_vals(Vals)];
decode_vals([]) ->
    [].
