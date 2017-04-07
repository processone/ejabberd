%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 31 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(mod_proxy65_redis).
-behaviour(mod_proxy65).

%% API
-export([init/0, register_stream/2, unregister_stream/1, activate_stream/4]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(proxy65, {pid_t :: pid(),
		  pid_i :: pid() | undefined,
		  jid_i :: binary() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    ?INFO_MSG("Cleaning Redis 'proxy65' table...", []),
    NodeKey = node_key(),
    case ejabberd_redis:smembers(NodeKey) of
	{ok, SIDs} ->
	    SIDKeys = [sid_key(S) || S <- SIDs],
	    JIDs = lists:flatmap(
		     fun(SIDKey) ->
			     case ejabberd_redis:get(SIDKey) of
				 {ok, Val} ->
				     try binary_to_term(Val) of
					 #proxy65{jid_i = J} when is_binary(J) ->
					     [jid_key(J)];
					 _ ->
					     []
				     catch _:badarg ->
					     []
				     end;
				 _ ->
				     []
			     end
		     end, SIDKeys),
	    ejabberd_redis:multi(
	      fun() ->
		      if SIDs /= [] ->
			      ejabberd_redis:del(SIDKeys),
			      if JIDs /= [] ->
				      ejabberd_redis:del(JIDs);
				 true ->
				      ok
			      end;
			 true ->
			      ok
		      end,
		      ejabberd_redis:del([NodeKey])
	      end),
	    ok;
	{error, _} ->
	    {error, db_failure}
    end.

register_stream(SID, Pid) ->
    SIDKey = sid_key(SID),
    try
	{ok, Val} = ejabberd_redis:get(SIDKey),
	try binary_to_term(Val) of
	    #proxy65{pid_i = undefined} = R ->
		NewVal = term_to_binary(R#proxy65{pid_i = Pid}),
		ok = ejabberd_redis:set(SIDKey, NewVal);
	    _ ->
		{error, conflict}
	catch _:badarg when Val == undefined ->
		NewVal = term_to_binary(#proxy65{pid_t = Pid}),
		{ok, _} = ejabberd_redis:multi(
			    fun() ->
				    ejabberd_redis:set(SIDKey, NewVal),
				    ejabberd_redis:sadd(node_key(), [SID])
			    end),
		ok;
	      _:badarg ->
		?ERROR_MSG("malformed data in redis (key = '~s'): ~p",
			   [SIDKey, Val]),
		{error, db_failure}
	end
    catch _:{badmatch, {error, _}} ->
	    {error, db_failure}
    end.

unregister_stream(SID) ->
    SIDKey = sid_key(SID),
    NodeKey = node_key(),
    try
	{ok, Val} = ejabberd_redis:get(SIDKey),
	try binary_to_term(Val) of
	    #proxy65{jid_i = JID} when is_binary(JID) ->
		JIDKey = jid_key(JID),
		{ok, _} = ejabberd_redis:multi(
			    fun() ->
				    ejabberd_redis:del([SIDKey]),
				    ejabberd_redis:srem(JIDKey, [SID]),
				    ejabberd_redis:srem(NodeKey, [SID])
			    end),
		ok;
	    _ ->
		{ok, _} = ejabberd_redis:multi(
			    fun() ->
				    ejabberd_redis:del([SIDKey]),
				    ejabberd_redis:srem(NodeKey, [SID])
			    end),
		ok
	catch _:badarg when Val == undefined ->
		ok;
	      _:badarg ->
		?ERROR_MSG("malformed data in redis (key = '~s'): ~p",
			   [SIDKey, Val]),
		{error, db_failure}
	end
    catch _:{badmatch, {error, _}} ->
	    {error, db_failure}
    end.

activate_stream(SID, IJID, MaxConnections, _Node) ->
    SIDKey = sid_key(SID),
    JIDKey = jid_key(IJID),
    try
	{ok, Val} = ejabberd_redis:get(SIDKey),
	try binary_to_term(Val) of
	    #proxy65{pid_t = TPid, pid_i = IPid,
		     jid_i = undefined} = R when is_pid(IPid) ->
		{ok, Num} = ejabberd_redis:scard(JIDKey),
		if Num >= MaxConnections ->
			{error, {limit, IPid, TPid}};
		   true ->
			NewVal = term_to_binary(R#proxy65{jid_i = IJID}),
			{ok, _} = ejabberd_redis:multi(
				    fun() ->
					    ejabberd_redis:sadd(JIDKey, [SID]),
					    ejabberd_redis:set(SIDKey, NewVal)
				    end),
			{ok, IPid, TPid}
		end;
	    #proxy65{jid_i = JID} when is_binary(JID) ->
		{error, conflict};
	    _ ->
		{error, notfound}
	catch _:badarg when Val == undefined ->
		{error, notfound};
	      _:badarg ->
		?ERROR_MSG("malformed data in redis (key = '~s'): ~p",
			   [SIDKey, Val]),
		{error, db_failure}
	end
    catch _:{badmatch, {error, _}} ->
	    {error, db_failure}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sid_key(SID) ->
    <<"ejabberd:proxy65:sid:", SID/binary>>.

jid_key(JID) ->
    <<"ejabberd:proxy65:initiator:", JID/binary>>.

node_key() ->
    Node = erlang:atom_to_binary(node(), latin1),
    <<"ejabberd:proxy65:node:", Node/binary>>.
