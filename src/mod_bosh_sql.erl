%%%----------------------------------------------------------------------
%%% File    : mod_bosh_sql.erl
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

-module(mod_bosh_sql).
-behaviour(mod_bosh).


%% API
-export([init/0, open_session/2, close_session/1, find_session/1]).

-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    Node = erlang:atom_to_binary(node(), latin1),
    ?DEBUG("Cleaning SQL 'bosh' table...", []),
    case ejabberd_sql:sql_query(
	   ejabberd_config:get_myname(), ?SQL("delete from bosh where node=%(Node)s")) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("Failed to clean 'route' table: ~p", [Err]),
	    Err
    end.

open_session(SID, Pid) ->
    PidS = misc:encode_pid(Pid),
    Node = erlang:atom_to_binary(node(Pid), latin1),
    case ?SQL_UPSERT(ejabberd_config:get_myname(), "bosh",
		     ["!sid=%(SID)s",
		      "node=%(Node)s",
		      "pid=%(PidS)s"]) of
	ok ->
	    ok;
	_Err ->
	    {error, db_failure}
    end.

close_session(SID) ->
    case ejabberd_sql:sql_query(
	   ejabberd_config:get_myname(), ?SQL("delete from bosh where sid=%(SID)s")) of
	{updated, _} ->
	    ok;
	_Err ->
	    {error, db_failure}
    end.

find_session(SID) ->
    case ejabberd_sql:sql_query(
	   ejabberd_config:get_myname(),
	   ?SQL("select @(pid)s, @(node)s from bosh where sid=%(SID)s")) of
	{selected, [{Pid, Node}]} ->
	    try	{ok, misc:decode_pid(Pid, Node)}
	    catch _:{bad_node, _} -> {error, notfound}
	    end;
	{selected, []} ->
	    {error, notfound};
	_Err ->
	    {error, db_failure}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
