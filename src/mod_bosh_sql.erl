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
    PidS = enc_pid(Pid),
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
	    try	{ok, dec_pid(Pid, Node)}
	    catch _:{node_down, _} -> error
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
-spec enc_pid(pid()) -> binary().
enc_pid(Pid) ->
    list_to_binary(erlang:pid_to_list(Pid)).

-spec dec_pid(binary(), binary()) -> pid().
dec_pid(PidBin, NodeBin) ->
    PidStr = binary_to_list(PidBin),
    Pid = erlang:list_to_pid(PidStr),
    case erlang:binary_to_atom(NodeBin, latin1) of
	Node when Node == node() ->
	    Pid;
	Node ->
	    try set_node_id(PidStr, NodeBin)
	    catch _:badarg ->
		    erlang:error({node_down, Node})
	    end
    end.

-spec set_node_id(string(), binary()) -> pid().
set_node_id(PidStr, NodeBin) ->
    ExtPidStr = erlang:pid_to_list(
		  binary_to_term(
		    <<131,103,100,(size(NodeBin)):16,NodeBin/binary,0:72>>)),
    [H|_] = string:tokens(ExtPidStr, "."),
    [_|T] = string:tokens(PidStr, "."),
    erlang:list_to_pid(string:join([H|T], ".")).
