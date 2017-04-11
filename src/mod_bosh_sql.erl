%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2017, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 28 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_bosh_sql).
-behaviour(mod_bosh).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% API
-export([init/0, open_session/2, close_session/1, find_session/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    Node = erlang:atom_to_binary(node(), latin1),
    ?INFO_MSG("Cleaning SQL 'bosh' table...", []),
    case ejabberd_sql:sql_query(
	   ?MYNAME, ?SQL("delete from bosh where node=%(Node)s")) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to clean 'route' table: ~p", [Err]),
	    Err
    end.

open_session(SID, Pid) ->
    PidS = misc:encode_pid(Pid),
    Node = erlang:atom_to_binary(node(Pid), latin1),
    case ?SQL_UPSERT(?MYNAME, "bosh",
		     ["!sid=%(SID)s",
		      "node=%(Node)s",
		      "pid=%(PidS)s"]) of
	ok ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to update 'bosh' table: ~p", [Err]),
	    {error, Err}
    end.

close_session(SID) ->
    %% TODO: report errors
    ejabberd_sql:sql_query(
      ?MYNAME, ?SQL("delete from bosh where sid=%(SID)s")).

find_session(SID) ->
    case ejabberd_sql:sql_query(
	   ?MYNAME,
	   ?SQL("select @(pid)s, @(node)s from bosh where sid=%(SID)s")) of
	{selected, [{Pid, Node}]} ->
	    try	{ok, misc:decode_pid(Pid, Node)}
	    catch _:{bad_node, _} -> error
	    end;
	{selected, []} ->
	    error;
	Err ->
	    ?ERROR_MSG("failed to select 'bosh' table: ~p", [Err]),
	    error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
