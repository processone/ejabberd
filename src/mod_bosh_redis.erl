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
    case ejabberd_redis:hset(?BOSH_KEY, SID, PidBin) of
	{ok, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to register bosh session in redis: ~p", [Err]),
	    Err
    end.

close_session(SID) ->
    case ejabberd_redis:hdel(?BOSH_KEY, [SID]) of
	{ok, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to delete bosh session in redis: ~p", [Err])
    end.

find_session(SID) ->
    case ejabberd_redis:hget(?BOSH_KEY, SID) of
	{ok, Pid} when is_binary(Pid) ->
	    try
		{ok, binary_to_term(Pid)}
	    catch _:badarg ->
		    ?ERROR_MSG("malformed data in redis (key = '~s'): ~p",
			       [SID, Pid]),
		    error
	    end;
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
    ?INFO_MSG("Cleaning Redis BOSH sessions...", []),
    case ejabberd_redis:hgetall(?BOSH_KEY) of
	{ok, Vals} ->
	    case ejabberd_redis:multi(
		   fun() ->
			   lists:foreach(
			     fun({SID, Pid}) when node(Pid) == node() ->
				     ejabberd_redis:hdel(?BOSH_KEY, [SID]);
				(_) ->
				     ok
			     end, Vals)
		   end) of
		{ok, _} ->
		    ok;
		Err ->
		    ?ERROR_MSG("failed to clean bosh sessions in redis: ~p", [Err])
	    end;
	Err ->
	    ?ERROR_MSG("failed to clean bosh sessions in redis: ~p", [Err])
    end.
