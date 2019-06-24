%%%----------------------------------------------------------------------
%%% File    : ejabberd_sql_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : SQL connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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

-author('alexey@process-one.net').

-export([start_link/1, init/1, add_pid/2, remove_pid/2,
	 get_pids/1, get_random_pid/1, reload/1]).

-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(sql_pool, {host :: binary(),
		   pid  :: pid()}).

start_link(Host) ->
    ejabberd_mnesia:create(?MODULE, sql_pool,
			[{ram_copies, [node()]}, {type, bag},
			 {local_content, true},
			 {attributes, record_info(fields, sql_pool)}]),
    F = fun () -> mnesia:delete({sql_pool, Host}) end,
    mnesia:ets(F),
    supervisor:start_link({local,
			   gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, [Host]).

init([Host]) ->
    Type = ejabberd_option:sql_type(Host),
    PoolSize = get_pool_size(Type, Host),
    case Type of
        sqlite ->
            check_sqlite_db(Host);
	mssql ->
	    ejabberd_sql:init_mssql(Host);
        _ ->
            ok
    end,
    {ok, {{one_for_one, PoolSize * 10, 1},
	  [child_spec(I, Host) || I <- lists:seq(1, PoolSize)]}}.

reload(Host) ->
    Type = ejabberd_option:sql_type(Host),
    NewPoolSize = get_pool_size(Type, Host),
    OldPoolSize = ets:select_count(
		    sql_pool,
		    ets:fun2ms(
		      fun(#sql_pool{host = H}) when H == Host ->
			      true
		      end)),
    reload(Host, NewPoolSize, OldPoolSize).

reload(Host, NewPoolSize, OldPoolSize) ->
    Sup = gen_mod:get_module_proc(Host, ?MODULE),
    if NewPoolSize == OldPoolSize ->
	    ok;
       NewPoolSize > OldPoolSize ->
	    lists:foreach(
	      fun(I) ->
		      Spec = child_spec(I, Host),
		      supervisor:start_child(Sup, Spec)
	      end, lists:seq(OldPoolSize+1, NewPoolSize));
       OldPoolSize > NewPoolSize ->
	    lists:foreach(
	      fun(I) ->
		      supervisor:terminate_child(Sup, I),
		      supervisor:delete_child(Sup, I)
	      end, lists:seq(NewPoolSize+1, OldPoolSize))
    end.

get_pids(Host) ->
    Rs = mnesia:dirty_read(sql_pool, Host),
    [R#sql_pool.pid || R <- Rs, is_process_alive(R#sql_pool.pid)].

get_random_pid(Host) ->
    case get_pids(Host) of
      [] -> none;
      Pids ->
	    I = p1_rand:round_robin(length(Pids)) + 1,
	    lists:nth(I, Pids)
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
    PoolSize = ejabberd_option:sql_pool_size(Host),
    if PoolSize > 1 andalso SQLType == sqlite ->
	    ?WARNING_MSG("It's not recommended to set sql_pool_size > 1 for "
			 "sqlite, because it may cause race conditions", []);
       true ->
	    ok
    end,
    PoolSize.

child_spec(I, Host) ->
    StartInterval = ejabberd_option:sql_start_interval(Host),
    {I, {ejabberd_sql, start_link, [Host, StartInterval]},
     transient, 2000, worker, [?MODULE]}.

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
            ?WARNING_MSG("Failed open sqlite database, reason ~p", [Reason])
    end.

create_sqlite_tables(DB) ->
    SqlDir = misc:sql_dir(),
    File = filename:join(SqlDir, "lite.sql"),
    case file:open(File, [read, binary]) of
        {ok, Fd} ->
            Qs = read_lines(Fd, File, []),
            ok = sqlite3:sql_exec(DB, "begin"),
            [ok = sqlite3:sql_exec(DB, Q) || Q <- Qs],
            ok = sqlite3:sql_exec(DB, "commit");
        {error, Reason} ->
            ?WARNING_MSG("Failed to read SQLite schema file: ~s",
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
