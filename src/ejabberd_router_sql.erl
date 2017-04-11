%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 28 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(ejabberd_router_sql).
-behaviour(ejabberd_router).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% API
-export([init/0, register_route/5, unregister_route/3, find_routes/1,
	 host_of_route/1, is_my_route/1, is_my_host/1, get_all_routes/0,
	 find_routes/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("ejabberd_router.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    Node = erlang:atom_to_binary(node(), latin1),
    ?INFO_MSG("Cleaning SQL 'route' table...", []),
    case ejabberd_sql:sql_query(
	   ?MYNAME, ?SQL("delete from route where node=%(Node)s")) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to clean 'route' table: ~p", [Err]),
	    Err
    end.

register_route(Domain, ServerHost, LocalHint, _, Pid) ->
    PidS = misc:encode_pid(Pid),
    LocalHintS = enc_local_hint(LocalHint),
    Node = erlang:atom_to_binary(node(Pid), latin1),
    case ?SQL_UPSERT(?MYNAME, "route",
		     ["!domain=%(Domain)s",
		      "!server_host=%(ServerHost)s",
		      "!node=%(Node)s",
		      "!pid=%(PidS)s",
		      "local_hint=%(LocalHintS)s"]) of
	ok ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to update 'route' table: ~p", [Err]),
	    {error, Err}
    end.

unregister_route(Domain, _, Pid) ->
    PidS = misc:encode_pid(Pid),
    Node = erlang:atom_to_binary(node(Pid), latin1),
    ejabberd_sql:sql_query(
      ?MYNAME,
      ?SQL("delete from route where domain=%(Domain)s "
	   "and pid=%(PidS)s and node=%(Node)s")),
    %% TODO: return meaningful error
    ok.

find_routes(Domain) ->
    case ejabberd_sql:sql_query(
	   ?MYNAME,
	   ?SQL("select @(server_host)s, @(node)s, @(pid)s, @(local_hint)s "
		"from route where domain=%(Domain)s")) of
	{selected, Rows} ->
	    lists:flatmap(
	      fun(Row) ->
		      row_to_route(Domain, Row)
	      end, Rows);
	Err ->
	    ?ERROR_MSG("failed to select from 'route' table: ~p", [Err]),
	    {error, Err}
    end.

host_of_route(Domain) ->
    case ejabberd_sql:sql_query(
	   ?MYNAME,
	   ?SQL("select @(server_host)s from route where domain=%(Domain)s")) of
	{selected, [{ServerHost}|_]} ->
	    {ok, ServerHost};
	{selected, []} ->
	    error;
	Err ->
	    ?ERROR_MSG("failed to select from 'route' table: ~p", [Err]),
	    error
    end.

is_my_route(Domain) ->
    case host_of_route(Domain) of
	{ok, _} -> true;
	_ -> false
    end.

is_my_host(Domain) ->
    {ok, Domain} == host_of_route(Domain).

get_all_routes() ->
    case ejabberd_sql:sql_query(
	   ?MYNAME,
	   ?SQL("select @(domain)s from route where domain <> server_host")) of
	{selected, Domains} ->
	    [Domain || {Domain} <- Domains];
	Err ->
	    ?ERROR_MSG("failed to select from 'route' table: ~p", [Err]),
	    []
    end.

find_routes() ->
    case ejabberd_sql:sql_query(
	   ?MYNAME,
	   ?SQL("select @(domain)s, @(server_host)s, @(node)s, @(pid)s, "
		"@(local_hint)s from route")) of
	{selected, Rows} ->
	    lists:flatmap(
	      fun({Domain, ServerHost, Node, Pid, LocalHint}) ->
		      row_to_route(Domain, {ServerHost, Node, Pid, LocalHint})
	      end, Rows);
	Err ->
	    ?ERROR_MSG("failed to select from 'route' table: ~p", [Err]),
	    []
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
	  E:R ->
	    ?ERROR_MSG("failed to decode row from 'route' table:~n"
		       "Row = ~p~n"
		       "Domain = ~s~n"
		       "Reason = ~p",
		       [Row, Domain, {E, {R, erlang:get_stacktrace()}}]),
	    []
    end.
