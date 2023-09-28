%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 28 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_router_sql).
-behaviour(ejabberd_router).


%% API
-export([init/0, register_route/5, unregister_route/3, find_routes/1,
	 get_all_routes/0]).

-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("ejabberd_router.hrl").
-include("ejabberd_stacktrace.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    ejabberd_sql_schema:update_schema(
      ejabberd_config:get_myname(), ?MODULE, schemas()),
    Node = erlang:atom_to_binary(node(), latin1),
    ?DEBUG("Cleaning SQL 'route' table...", []),
    case ejabberd_sql:sql_query(
	   ejabberd_config:get_myname(), ?SQL("delete from route where node=%(Node)s")) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("Failed to clean 'route' table: ~p", [Err]),
	    Err
    end.

schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"route">>,
                columns =
                    [#sql_column{name = <<"domain">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"node">>, type = text},
                     #sql_column{name = <<"pid">>, type = text},
                     #sql_column{name = <<"local_hint">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"domain">>, <<"server_host">>,
                                         <<"node">>, <<"pid">>],
                              unique = true}]}]}].

register_route(Domain, ServerHost, LocalHint, _, Pid) ->
    PidS = misc:encode_pid(Pid),
    LocalHintS = enc_local_hint(LocalHint),
    Node = erlang:atom_to_binary(node(Pid), latin1),
    case ?SQL_UPSERT(ejabberd_config:get_myname(), "route",
		     ["!domain=%(Domain)s",
		      "!server_host=%(ServerHost)s",
		      "!node=%(Node)s",
		      "!pid=%(PidS)s",
		      "local_hint=%(LocalHintS)s"]) of
	ok ->
	    ok;
	_ ->
	    {error, db_failure}
    end.

unregister_route(Domain, _, Pid) ->
    PidS = misc:encode_pid(Pid),
    Node = erlang:atom_to_binary(node(Pid), latin1),
    case ejabberd_sql:sql_query(
	   ejabberd_config:get_myname(),
	   ?SQL("delete from route where domain=%(Domain)s "
		"and pid=%(PidS)s and node=%(Node)s")) of
	{updated, _} ->
	    ok;
	_ ->
	    {error, db_failure}
    end.

find_routes(Domain) ->
    case ejabberd_sql:sql_query(
	   ejabberd_config:get_myname(),
	   ?SQL("select @(server_host)s, @(node)s, @(pid)s, @(local_hint)s "
		"from route where domain=%(Domain)s")) of
	{selected, Rows} ->
	    {ok, lists:flatmap(
		   fun(Row) ->
			   row_to_route(Domain, Row)
		   end, Rows)};
	_ ->
	    {error, db_failure}
    end.

get_all_routes() ->
    case ejabberd_sql:sql_query(
	   ejabberd_config:get_myname(),
	   ?SQL("select @(domain)s from route where domain <> server_host")) of
	{selected, Domains} ->
	    {ok, [Domain || {Domain} <- Domains]};
	_ ->
	    {error, db_failure}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
enc_local_hint(undefined) ->
    <<"">>;
enc_local_hint(LocalHint) ->
    misc:term_to_expr(LocalHint).

dec_local_hint(<<"">>) ->
    undefined;
dec_local_hint(S) ->
    ejabberd_sql:decode_term(S).

row_to_route(Domain, {ServerHost, NodeS, PidS, LocalHintS} = Row) ->
    try	[#route{domain = Domain,
		server_host = ServerHost,
		pid = misc:decode_pid(PidS, NodeS),
		local_hint = dec_local_hint(LocalHintS)}]
    catch _:{bad_node, _} ->
	    [];
	  ?EX_RULE(Class, Reason, St) ->
	    StackTrace = ?EX_STACK(St),
	    ?ERROR_MSG("Failed to decode row from 'route' table:~n"
		       "** Row = ~p~n"
		       "** Domain = ~ts~n"
		       "** ~ts",
		       [Row, Domain,
			misc:format_exception(2, Class, Reason, StackTrace)]),
	    []
    end.
