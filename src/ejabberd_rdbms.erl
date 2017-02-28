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

-behaviour(supervisor).
-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start_link/0, init/1, opt_type/1,
	 config_reloaded/0, start_host/1, stop_host/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    file:delete(ejabberd_sql:freetds_config()),
    file:delete(ejabberd_sql:odbc_config()),
    file:delete(ejabberd_sql:odbcinst_config()),
    ejabberd_hooks:add(host_up, ?MODULE, start_host, 20),
    ejabberd_hooks:add(host_down, ?MODULE, stop_host, 90),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),
    {ok, {{one_for_one, 10, 1}, get_specs()}}.

-spec get_specs() -> [supervisor:child_spec()].
get_specs() ->
    lists:flatmap(
      fun(Host) ->
	      case get_spec(Host) of
		  {ok, Spec} -> [Spec];
		  undefined -> []
	      end
      end, ?MYHOSTS).

-spec get_spec(binary()) -> {ok, supervisor:child_spec()} | undefined.
get_spec(Host) ->
    case needs_sql(Host) of
	{true, App} ->
	    ejabberd:start_app(App),
	    SupName = gen_mod:get_module_proc(Host, ejabberd_sql_sup),
	    {ok, {SupName, {ejabberd_sql_sup, start_link, [Host]},
		  transient, infinity, supervisor, [ejabberd_sql_sup]}};
	false ->
	    undefined
    end.

-spec config_reloaded() -> ok.
config_reloaded() ->
    lists:foreach(fun start_host/1, ?MYHOSTS).

-spec start_host(binary()) -> ok.
start_host(Host) ->
    case get_spec(Host) of
	{ok, Spec} ->
	    case supervisor:start_child(?MODULE, Spec) of
		{ok, _PID} ->
		    ok;
		{error, {already_started, _}} ->
		    ok;
		{error, _} = Err ->
		    erlang:error(Err)
	    end;
	undefined ->
	    ok
    end.

-spec stop_host(binary()) -> ok.
stop_host(Host) ->
    SupName = gen_mod:get_module_proc(Host, ejabberd_sql_sup),
    supervisor:terminate_child(?MODULE, SupName),
    supervisor:delete_child(?MODULE, SupName),
    ok.

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
