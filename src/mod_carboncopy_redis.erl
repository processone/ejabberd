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
-behaviour(gen_server).

%% API
-export([init/2, enable/4, disable/3, list/2, cache_nodes/1]).
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("mod_carboncopy.hrl").

-define(CARBONCOPY_KEY, <<"ejabberd:carboncopy">>).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    Spec = {?MODULE, {?MODULE, start_link, []},
	    transient, 5000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_backend_sup, Spec) of
	{ok, _Pid} -> ok;
	Err -> Err
    end.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cache_nodes(_LServer) ->
    [node()].

enable(LUser, LServer, LResource, NS) ->
    USKey = us_key(LUser, LServer),
    NodeKey = node_key(),
    JID = jid:encode({LUser, LServer, LResource}),
    Data = term_to_binary({NS, node()}),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hset(USKey, LResource, Data),
		   ejabberd_redis:sadd(NodeKey, [JID]),
		   ejabberd_redis:publish(
		     ?CARBONCOPY_KEY,
		     term_to_binary({delete, {LUser, LServer}}))
	   end) of
	{ok, _} ->
	    ok;
	{error, _} ->
	    {error, db_failure}
    end.

disable(LUser, LServer, LResource) ->
    USKey = us_key(LUser, LServer),
    NodeKey = node_key(),
    JID = jid:encode({LUser, LServer, LResource}),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hdel(USKey, [LResource]),
		   ejabberd_redis:srem(NodeKey, [JID]),
		   ejabberd_redis:publish(
		     ?CARBONCOPY_KEY,
		     term_to_binary({delete, {LUser, LServer}}))
	   end) of
	{ok, _} ->
	    ok;
	{error, _} ->
	    {error, db_failure}
    end.

list(LUser, LServer) ->
    USKey = us_key(LUser, LServer),
    case ejabberd_redis:hgetall(USKey) of
	{ok, Pairs} ->
	    {ok, lists:flatmap(
		   fun({Resource, Data}) ->
			   try
			       {NS, Node} = binary_to_term(Data),
			       [{Resource, NS, Node}]
			   catch _:_ ->
				   ?ERROR_MSG("invalid term stored in Redis "
					      "(key = ~s): ~p",
					      [USKey, Data]),
				   []
			   end
		   end, Pairs)};
	{error, _} ->
	    {error, db_failure}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ejabberd_redis:subscribe([?CARBONCOPY_KEY]),
    clean_table(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({redis_message, ?CARBONCOPY_KEY, Data}, State) ->
    case binary_to_term(Data) of
	{delete, Key} ->
	    ets_cache:delete(?CARBONCOPY_CACHE, Key);
	Msg ->
	    ?WARNING_MSG("unexpected redis message: ~p", [Msg])
    end,
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_table() ->
    ?DEBUG("Cleaning Redis 'carboncopy' table...", []),
    NodeKey = node_key(),
    case ejabberd_redis:smembers(NodeKey) of
	{ok, JIDs} ->
	    ejabberd_redis:multi(
	      fun() ->
		      lists:foreach(
			fun(JID) ->
				{U, S, R} = jid:split(jid:decode(JID)),
				USKey = us_key(U, S),
				ejabberd_redis:hdel(USKey, [R])
			end, JIDs)
	      end);
	{error, _} ->
	    ok
    end,
    ejabberd_redis:del([NodeKey]),
    ok.

us_key(LUser, LServer) ->
    <<"ejabberd:carboncopy:users:", LUser/binary, $@, LServer/binary>>.

node_key() ->
    Node = erlang:atom_to_binary(node(), latin1),
    <<"ejabberd:carboncopy:nodes:", Node/binary>>.
