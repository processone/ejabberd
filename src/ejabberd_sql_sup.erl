%%%----------------------------------------------------------------------
%%% File    : ejabberd_sql_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : SQL connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_sql_sup).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start_link/1, init/1, add_pid/2, remove_pid/2,
	 get_pids/1, get_random_pid/1, transform_options/1,
	 opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(PGSQL_PORT, 5432).

-define(MYSQL_PORT, 3306).

-define(DEFAULT_POOL_SIZE, 10).

-define(DEFAULT_SQL_START_INTERVAL, 30).

-define(CONNECT_TIMEOUT, 500).

-record(sql_pool, {host, pid}).

start_link(Host) ->
    ejabberd_mnesia:create(?MODULE, sql_pool,
			[{ram_copies, [node()]}, {type, bag},
			 {local_content, true},
			 {attributes, record_info(fields, sql_pool)}]),
    mnesia:add_table_copy(sql_pool, node(), ram_copies),
    F = fun () -> mnesia:delete({sql_pool, Host}) end,
    mnesia:ets(F),
    supervisor:start_link({local,
			   gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, [Host]).

init([Host]) ->
    StartInterval = ejabberd_config:get_option(
                      {sql_start_interval, Host},
                      fun(I) when is_integer(I), I>0 -> I end,
                      ?DEFAULT_SQL_START_INTERVAL),
    Type = ejabberd_config:get_option({sql_type, Host},
                                      fun(mysql) -> mysql;
                                         (pgsql) -> pgsql;
                                         (sqlite) -> sqlite;
					 (mssql) -> mssql;
                                         (odbc) -> odbc
                                      end, odbc),
    PoolSize = get_pool_size(Type, Host),
    case Type of
        sqlite ->
            check_sqlite_db(Host);
	mssql ->
	    ejabberd_sql:init_mssql(Host);
        _ ->
            ok
    end,

    {ok,
     {{one_for_one, PoolSize * 10, 1},
      lists:map(fun (I) ->
			{I,
			 {ejabberd_sql, start_link,
			  [Host, StartInterval * 1000]},
			 transient, 2000, worker, [?MODULE]}
		end,
		lists:seq(1, PoolSize))}}.

get_pids(Host) ->
    Rs = mnesia:dirty_read(sql_pool, Host),
    [R#sql_pool.pid || R <- Rs].

get_random_pid(Host) ->
    case get_pids(Host) of
      [] -> none;
      Pids -> lists:nth(erlang:phash(p1_time_compat:unique_integer(), length(Pids)), Pids)
    end.

add_pid(Host, Pid) ->
    F = fun () ->
		mnesia:write(#sql_pool{host = Host, pid = Pid})
	end,
    mnesia:ets(F).

remove_pid(Host, Pid) ->
    F = fun () ->
		mnesia:delete_object(#sql_pool{host = Host, pid = Pid})
	end,
    mnesia:ets(F).

-spec get_pool_size(atom(), binary()) -> pos_integer().
get_pool_size(SQLType, Host) ->
    PoolSize = ejabberd_config:get_option(
                 {sql_pool_size, Host},
                 fun(I) when is_integer(I), I>0 -> I end,
		 case SQLType of
		     sqlite -> 1;
		     _ -> ?DEFAULT_POOL_SIZE
		 end),
    if PoolSize > 1 andalso SQLType == sqlite ->
	    ?WARNING_MSG("it's not recommended to set sql_pool_size > 1 for "
			 "sqlite, because it may cause race conditions", []);
       true ->
	    ok
    end,
    PoolSize.

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({odbc_server, {Type, Server, Port, DB, User, Pass}}, Opts) ->
    [{sql_type, Type},
     {sql_server, Server},
     {sql_port, Port},
     {sql_database, DB},
     {sql_username, User},
     {sql_password, Pass}|Opts];
transform_options({odbc_server, {mysql, Server, DB, User, Pass}}, Opts) ->
    transform_options({odbc_server, {mysql, Server, ?MYSQL_PORT, DB, User, Pass}}, Opts);
transform_options({odbc_server, {pgsql, Server, DB, User, Pass}}, Opts) ->
    transform_options({odbc_server, {pgsql, Server, ?PGSQL_PORT, DB, User, Pass}}, Opts);
transform_options({odbc_server, {sqlite, DB}}, Opts) ->
    transform_options({odbc_server, {sqlite, DB}}, Opts);
transform_options(Opt, Opts) ->
    [Opt|Opts].

check_sqlite_db(Host) ->
    DB = ejabberd_sql:sqlite_db(Host),
    File = ejabberd_sql:sqlite_file(Host),
    Ret = case filelib:ensure_dir(File) of
	      ok ->
		  case sqlite3:open(DB, [{file, File}]) of
		      {ok, _Ref} -> ok;
		      {error, {already_started, _Ref}} -> ok;
		      {error, R} -> {error, R}
		  end;
	      Err ->
		  Err
	  end,
    case Ret of
        ok ->
	    sqlite3:sql_exec(DB, "pragma foreign_keys = on"),
            case sqlite3:list_tables(DB) of
                [] ->
                    create_sqlite_tables(DB),
                    sqlite3:close(DB),
                    ok;
                [_H | _] ->
                    ok
            end;
        {error, Reason} ->
            ?INFO_MSG("Failed open sqlite database, reason ~p", [Reason])
    end.

create_sqlite_tables(DB) ->
    SqlDir = case code:priv_dir(ejabberd) of
                 {error, _} ->
                     ?SQL_DIR;
                 PrivDir ->
                     filename:join(PrivDir, "sql")
             end,
    File = filename:join(SqlDir, "lite.sql"),
    case file:open(File, [read, binary]) of
        {ok, Fd} ->
            Qs = read_lines(Fd, File, []),
            ok = sqlite3:sql_exec(DB, "begin"),
            [ok = sqlite3:sql_exec(DB, Q) || Q <- Qs],
            ok = sqlite3:sql_exec(DB, "commit");
        {error, Reason} ->
            ?INFO_MSG("Failed to read SQLite schema file: ~s",
		      [file:format_error(Reason)])
    end.

read_lines(Fd, File, Acc) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            NewAcc = case str:strip(str:strip(Line, both, $\r), both, $\n) of
                         <<"--", _/binary>> ->
                             Acc;
                         <<>> ->
                             Acc;
                         _ ->
                             [Line|Acc]
                     end,
            read_lines(Fd, File, NewAcc);
        eof ->
            QueryList = str:tokens(list_to_binary(lists:reverse(Acc)), <<";">>),
            lists:flatmap(
              fun(Query) ->
                      case str:strip(str:strip(Query, both, $\r), both, $\n) of
                          <<>> ->
                              [];
                          Q ->
                              [<<Q/binary, $;>>]
                      end
              end, QueryList);
        {error, _} = Err ->
            ?ERROR_MSG("Failed read from lite.sql, reason: ~p", [Err]),
            []
    end.

opt_type(sql_pool_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(sql_start_interval) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(sql_type) ->
    fun (mysql) -> mysql;
	(pgsql) -> pgsql;
	(sqlite) -> sqlite;
	(mssql) -> mssql;
	(odbc) -> odbc
    end;
opt_type(_) ->
    [sql_pool_size, sql_start_interval, sql_type].
