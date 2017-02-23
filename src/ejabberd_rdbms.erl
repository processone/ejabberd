%%%----------------------------------------------------------------------
%%% File    : ejabberd_rdbms.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Manage the start of the database modules when needed
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_rdbms).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start/0, opt_type/1, start_hosts/0, start_host/1, stop_host/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

start() ->
    file:delete(ejabberd_sql:freetds_config()),
    file:delete(ejabberd_sql:odbc_config()),
    file:delete(ejabberd_sql:odbcinst_config()),
    ejabberd_hooks:add(host_up, ?MODULE, start_host, 20),
    ejabberd_hooks:add(host_down, ?MODULE, stop_host, 90),
    ejabberd_hooks:add(config_reloaded, ?MODULE, start_hosts, 20),
    case lists:any(fun(H) -> needs_sql(H) /= false end,
                   ?MYHOSTS) of
        true ->
            start_hosts();
        false ->
            ok
    end.

%% Start relationnal DB module on the nodes where it is needed
start_hosts() ->
    lists:foreach(fun start_host/1, ?MYHOSTS).

-spec start_host(binary()) -> ok.
start_host(Host) ->
    case needs_sql(Host) of
	{true, App} -> start_sql(Host, App);
	false -> ok
    end.

-spec stop_host(binary()) -> ok.
stop_host(Host) ->
    SupName = gen_mod:get_module_proc(Host, ejabberd_sql_sup),
    supervisor:terminate_child(ejabberd_sup, SupName),
    supervisor:delete_child(ejabberd_sup, SupName),
    ok.

%% Start the SQL module on the given host
start_sql(Host, App) ->
    ejabberd:start_app(App),
    Supervisor_name = gen_mod:get_module_proc(Host,
					      ejabberd_sql_sup),
    ChildSpec = {Supervisor_name,
		 {ejabberd_sql_sup, start_link, [Host]}, transient,
		 infinity, supervisor, [ejabberd_sql_sup]},
    case supervisor:start_child(ejabberd_sup, ChildSpec) of
      {ok, _PID} -> ok;
      {error, {already_started, _}} -> ok;
      _Error ->
	  ?ERROR_MSG("Start of supervisor ~p failed:~n~p~nRetrying."
		     "..~n",
		     [Supervisor_name, _Error]),
	  timer:sleep(5000),
	  start_sql(Host, App)
    end.

%% Returns {true, App} if we have configured sql for the given host
needs_sql(Host) ->
    LHost = jid:nameprep(Host),
    case ejabberd_config:get_option({sql_type, LHost},
                                    fun(mysql) -> mysql;
                                       (pgsql) -> pgsql;
                                       (sqlite) -> sqlite;
				       (mssql) -> mssql;
                                       (odbc) -> odbc
                                    end, undefined) of
        mysql -> {true, p1_mysql};
        pgsql -> {true, p1_pgsql};
        sqlite -> {true, sqlite3};
	mssql -> {true, odbc};
        odbc -> {true, odbc};
        undefined -> false
    end.

opt_type(sql_type) ->
    fun (mysql) -> mysql;
	(pgsql) -> pgsql;
	(sqlite) -> sqlite;
	(mssql) -> mssql;
	(odbc) -> odbc
    end;
opt_type(_) -> [sql_type].
