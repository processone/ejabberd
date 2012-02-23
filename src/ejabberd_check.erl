%%%----------------------------------------------------------------------
%%% File    : ejabberd_check.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Check ejabberd configuration and 
%%% Created : 27 Feb 2008 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_check).

-export([libs/0, config/0]).

-include("ejabberd.hrl").
-include("ejabberd_config.hrl").

-compile([export_all]).

%% TODO:
%% We want to implement library checking at launch time to issue
%% human readable user messages.
libs() ->
    ok.

%% @doc Consistency check on ejabberd configuration
config() ->
    check_database_modules().

check_database_modules() ->
     [check_database_module(M)||M<-get_db_used()].

check_database_module(odbc) ->
    check_modules(odbc, [odbc, odbc_app, odbc_sup, ejabberd_odbc, ejabberd_odbc_sup, odbc_queries]);
check_database_module(mysql) ->
    check_modules(mysql, [mysql, mysql_auth, mysql_conn, mysql_recv]);
check_database_module(pgsql) ->
    check_modules(pgsql, [pgsql, pgsql_proto, pgsql_tcp, pgsql_util]).

%% @doc Issue a critical error and throw an exit if needing module is
%% missing.
check_modules(DB, Modules) ->
    case get_missing_modules(Modules) of
	[] ->
	    ok;
	MissingModules when is_list(MissingModules) ->
	    ?CRITICAL_MSG("ejabberd is configured to use '~p', but the following Erlang modules are not installed: '~p'", [DB, MissingModules]),
	    exit(database_module_missing)
    end.


%% @doc Return the list of undefined modules
get_missing_modules(Modules) ->
    lists:filter(fun(Module) ->
			 case catch Module:module_info() of
			     {'EXIT', {undef, _}} ->
				 true;
			     _ -> false
			 end
		 end, Modules).

%% @doc Return the list of databases used
get_db_used() ->
    %% Retrieve domains with a database configured:
    Domains = 
	ets:match(local_config, #local_config{key={odbc_server, '$1'},
					      value='$2'}),
    %% Check that odbc is the auth method used for those domains:
    %% and return the database name
    DBs = lists:foldr(
	    fun([Domain, DB], Acc) ->
		    case check_odbc_option(
			   ejabberd_config:get_local_option(
			     {auth_method, Domain})) of
			true -> [get_db_type(DB)|Acc];
			_ -> Acc
		    end
	    end,
	    [], Domains),
    lists:usort(DBs).

%% @doc Depending in the DB definition, return which type of DB this is.
%% Note that MSSQL is detected as ODBC.
%% @spec (DB) -> mysql | pgsql | odbc
get_db_type(DB) when is_tuple(DB) ->
    element(1, DB);
get_db_type(DB) when is_list(DB) ->
    odbc.

%% @doc Return true if odbc option is used
check_odbc_option(odbc) ->
    true;
check_odbc_option(AuthMethods) when is_list(AuthMethods) ->
    lists:member(odbc, AuthMethods);
check_odbc_option(_) ->
    false.
