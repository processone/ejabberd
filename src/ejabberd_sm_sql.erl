%%%-------------------------------------------------------------------
%%% File    : ejabberd_sm_sql.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  9 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(ejabberd_sm_sql).


-behaviour(ejabberd_sm).

%% API
-export([init/0,
	 set_session/1,
	 delete_session/1,
	 get_sessions/0,
	 get_sessions/1,
	 get_sessions/2]).

-include("ejabberd_sm.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    Node = erlang:atom_to_binary(node(), latin1),
    ?DEBUG("Cleaning SQL SM table...", []),
    lists:foldl(
      fun(Host, ok) ->
              ejabberd_sql_schema:update_schema(Host, ?MODULE, schemas()),
	      case ejabberd_sql:sql_query(
		     Host, ?SQL("delete from sm where node=%(Node)s")) of
		  {updated, _} ->
		      ok;
		  Err ->
		      ?ERROR_MSG("Failed to clean 'sm' table: ~p", [Err]),
		      {error, db_failure}
	      end;
	 (_, Err) ->
	      Err
      end, ok, ejabberd_sm:get_vh_by_backend(?MODULE)).

schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"sm">>,
                columns =
                    [#sql_column{name = <<"usec">>, type = bigint},
                     #sql_column{name = <<"pid">>, type = text},
                     #sql_column{name = <<"node">>, type = text},
                     #sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"resource">>, type = text},
                     #sql_column{name = <<"priority">>, type = text},
                     #sql_column{name = <<"info">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"usec">>, <<"pid">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"node">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"username">>]}]}]}].

set_session(#session{sid = {Now, Pid}, usr = {U, LServer, R},
		     priority = Priority, info = Info}) ->
    InfoS = misc:term_to_expr(Info),
    PrioS = enc_priority(Priority),
    TS = now_to_timestamp(Now),
    PidS = misc:encode_pid(Pid),
    Node = erlang:atom_to_binary(node(Pid), latin1),
    case ?SQL_UPSERT(LServer, "sm",
                     ["!usec=%(TS)d",
                      "!pid=%(PidS)s",
                      "node=%(Node)s",
                      "username=%(U)s",
                      "server_host=%(LServer)s",
                      "resource=%(R)s",
                      "priority=%(PrioS)s",
                      "info=%(InfoS)s"]) of
	ok ->
	    ok;
	_Err ->
	    {error, db_failure}
    end.

delete_session(#session{usr = {_, LServer, _}, sid = {Now, Pid}}) ->
    TS = now_to_timestamp(Now),
    PidS = list_to_binary(erlang:pid_to_list(Pid)),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from sm where usec=%(TS)d and pid=%(PidS)s")) of
	{updated, _} ->
	    ok;
	_Err ->
	    {error, db_failure}
    end.

get_sessions() ->
    lists:flatmap(
      fun(LServer) ->
	      get_sessions(LServer)
      end, ejabberd_sm:get_vh_by_backend(?MODULE)).

get_sessions(LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
           ?SQL("select @(usec)d, @(pid)s, @(node)s, @(username)s,"
                " @(resource)s, @(priority)s, @(info)s from sm"
                " where %(LServer)H")) of
	{selected, Rows} ->
	    lists:flatmap(
	      fun(Row) ->
		      try [row_to_session(LServer, Row)]
		      catch _:{bad_node, _} -> []
		      end
	      end, Rows);
	_Err ->
	    []
    end.

get_sessions(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
           ?SQL("select @(usec)d, @(pid)s, @(node)s, @(username)s,"
                " @(resource)s, @(priority)s, @(info)s from sm"
                " where username=%(LUser)s and %(LServer)H")) of
	{selected, Rows} ->
	    {ok, lists:flatmap(
		   fun(Row) ->
			   try [row_to_session(LServer, Row)]
			   catch _:{bad_node, _} -> []
			   end
		   end, Rows)};
	_Err ->
	    {error, db_failure}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
now_to_timestamp({MSec, Sec, USec}) ->
    (MSec * 1000000 + Sec) * 1000000 + USec.

timestamp_to_now(I) ->
    Head = I div 1000000,
    USec = I rem 1000000,
    MSec = Head div 1000000,
    Sec = Head rem 1000000,
    {MSec, Sec, USec}.

dec_priority(Prio) ->
    case catch binary_to_integer(Prio) of
	{'EXIT', _} ->
	    undefined;
	Int ->
	    Int
    end.

enc_priority(undefined) ->
    <<"">>;
enc_priority(Int) when is_integer(Int) ->
    integer_to_binary(Int).

row_to_session(LServer, {USec, PidS, NodeS, User, Resource, PrioS, InfoS}) ->
    Now = timestamp_to_now(USec),
    Pid = misc:decode_pid(PidS, NodeS),
    Priority = dec_priority(PrioS),
    Info = ejabberd_sql:decode_term(InfoS),
    #session{sid = {Now, Pid}, us = {User, LServer},
	     usr = {User, LServer, Resource},
	     priority = Priority,
	     info = Info}.
