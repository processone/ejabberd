%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2017, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 28 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_bosh_redis).
-behaviour(mod_bosh).

%% API
-export([init/0, open_session/2, close_session/1, find_session/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(BOSH_KEY, "ejabberd:bosh").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    clean_table().

open_session(SID, Pid) ->
    PidBin = term_to_binary(Pid),
    case ejabberd_redis:q(["HSET", ?BOSH_KEY, SID, PidBin]) of
	{ok, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to register bosh session in redis: ~p", [Err]),
	    Err
    end.

close_session(SID) ->
    case ejabberd_redis:q(["HDEL", ?BOSH_KEY, SID]) of
	{ok, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to delete bosh session in redis: ~p", [Err])
    end.

find_session(SID) ->
    case ejabberd_redis:q(["HGET", ?BOSH_KEY, SID]) of
	{ok, Pid} when is_binary(Pid) ->
	    {ok, binary_to_term(Pid)};
	{ok, _} ->
	    error;
	Err ->
	    ?ERROR_MSG("failed to lookup bosh session in redis: ~p", [Err]),
	    error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_table() ->
    ?INFO_MSG("Cleaning Redis BOSH table...", []),
    case ejabberd_redis:q(["HGETALL", ?BOSH_KEY]) of
	{ok, Vals} ->
	    clean_table(Vals);
	Err ->
	    ?ERROR_MSG("failed to clean bosh table in redis: ~p", [Err])
    end.

clean_table([SID, PidBin|Vals]) ->
    case binary_to_term(PidBin) of
	Pid when node(Pid) == node() ->
	    close_session(SID);
	_ ->
	    ok
    end,
    clean_table(Vals);
clean_table([]) ->
    ok.
