%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 29 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(mod_carboncopy_sql).
-behaviour(mod_carboncopy).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% API
-export([init/2, enable/4, disable/3, list/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(Host, _Opts) ->
    clean_table(Host).

enable(LUser, LServer, LResource, NS) ->
    NodeS = erlang:atom_to_binary(node(), latin1),
    case ?SQL_UPSERT(LServer, "carboncopy",
		     ["!username=%(LUser)s",
		      "!resource=%(LResource)s",
		      "namespace=%(NS)s",
		      "node=%(NodeS)s"]) of
	ok ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to update 'carboncopy' table: ~p", [Err]),
	    Err
    end.

disable(LUser, LServer, LResource) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from carboncopy where username=%(LUser)s "
		"and resource=%(LResource)s")) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to delete from 'carboncopy' table: ~p", [Err]),
	    Err
    end.

list(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(resource)s, @(namespace)s from carboncopy "
		"where username=%(LUser)s")) of
	{selected, Rows} ->
	    Rows;
	Err ->
	    ?ERROR_MSG("failed to select from 'carboncopy' table: ~p", [Err]),
	    []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_table(LServer) ->
    NodeS = erlang:atom_to_binary(node(), latin1),
    ?INFO_MSG("Cleaning SQL 'carboncopy' table...", []),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from carboncopy where node=%(NodeS)s")) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to clean 'carboncopy' table: ~p", [Err]),
	    Err
    end.
