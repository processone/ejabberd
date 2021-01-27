%%%----------------------------------------------------------------------
%%% File    : mod_bosh_redis.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose :
%%% Created : 28 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2017-2021   ProcessOne
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

-module(mod_bosh_redis).
-behaviour(mod_bosh).
-behaviour(gen_server).

%% API
-export([init/0, open_session/2, close_session/1, find_session/1,
	 cache_nodes/0]).
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("logger.hrl").
-include("bosh.hrl").

-record(state, {}).

-define(BOSH_KEY, <<"ejabberd:bosh">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    Spec = {?MODULE, {?MODULE, start_link, []},
	    transient, 5000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_backend_sup, Spec) of
	{ok, _Pid} -> ok;
	Err -> Err
    end.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open_session(SID, Pid) ->
    PidBin = term_to_binary(Pid),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hset(?BOSH_KEY, SID, PidBin),
		   ejabberd_redis:publish(?BOSH_KEY, SID)
	   end) of
	{ok, _} ->
	    ok;
	{error, _} ->
	    {error, db_failure}
    end.

close_session(SID) ->
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hdel(?BOSH_KEY, [SID]),
		   ejabberd_redis:publish(?BOSH_KEY, SID)
	   end) of
	{ok, _} ->
	    ok;
	{error, _} ->
	    {error, db_failure}
    end.

find_session(SID) ->
    case ejabberd_redis:hget(?BOSH_KEY, SID) of
	{ok, undefined} ->
	    {error, notfound};
	{ok, Pid} ->
	    try
		{ok, binary_to_term(Pid)}
	    catch _:badarg ->
		    ?ERROR_MSG("Malformed data in redis (key = '~ts'): ~p",
			       [SID, Pid]),
		    {error, db_failure}
	    end;
	{error, _} ->
	    {error, db_failure}
    end.

cache_nodes() ->
    [node()].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    clean_table(),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({redis_message, ?BOSH_KEY, SID}, State) ->
    ets_cache:delete(?BOSH_CACHE, SID),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_table() ->
    ?DEBUG("Cleaning Redis BOSH sessions...", []),
    case ejabberd_redis:hgetall(?BOSH_KEY) of
	{ok, Vals} ->
	    ejabberd_redis:multi(
	      fun() ->
		      lists:foreach(
			fun({SID, Pid}) when node(Pid) == node() ->
				ejabberd_redis:hdel(?BOSH_KEY, [SID]);
			   (_) ->
				ok
			end, Vals)
	      end),
	    ok;
	{error, _} ->
	    ?ERROR_MSG("Failed to clean bosh sessions in redis", [])
    end.
