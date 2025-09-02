%%%----------------------------------------------------------------------
%%% File    : ejabberd_sql_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : SQL connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-export([start/1, stop/1, stop/0]).
-export([start_link/0, start_link/1]).
-export([init/1, reload/1, config_reloaded/0, is_started/1]).

-include("logger.hrl").

start(Host) ->
    case is_started(Host) of
	true -> ok;
	false ->
	    case lists:member(Host, ejabberd_option:hosts()) of
		false ->
		    ?WARNING_MSG("Rejecting start of sql worker for unknown host: ~ts", [Host]),
		    {error, invalid_host};
		true ->
		    App = case ejabberd_option:sql_type(Host) of
			      mysql -> p1_mysql;
			      pgsql -> p1_pgsql;
			      sqlite -> sqlite3;
			      _ -> odbc
			  end,
		    ejabberd:start_app(App),
		    Spec = #{id => gen_mod:get_module_proc(Host, ?MODULE),
			start => {ejabberd_sql_sup, start_link, [Host]},
			restart => transient,
			shutdown => infinity,
			type => supervisor,
			modules => [?MODULE]},
		    case supervisor:start_child(ejabberd_db_sup, Spec) of
			{ok, _} ->
			    ejabberd_sql_schema:start(Host),
			    ok;
			{error, {already_started, Pid}} ->
			    %% Wait for the supervisor to fully start
			    _ = supervisor:count_children(Pid),
			    ok;
			{error, Why} = Err ->
			    ?ERROR_MSG("Failed to start ~ts: ~p", [?MODULE, Why]),
			    Err
		    end
	    end
    end.

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    case supervisor:terminate_child(ejabberd_db_sup, Proc) of
	ok -> supervisor:delete_child(ejabberd_db_sup, Proc);
	Err -> Err
    end.


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Host) ->
    supervisor:start_link({local,
			   gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, [Host]).

stop() ->
    ejabberd_hooks:delete(host_up, ?MODULE, start, 20),
    ejabberd_hooks:delete(host_down, ?MODULE, stop, 90),
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 20).

init([]) ->
    file:delete(ejabberd_sql:odbcinst_config()),
    ejabberd_hooks:add(host_up, ?MODULE, start, 20),
    ejabberd_hooks:add(host_down, ?MODULE, stop, 90),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),
    ignore;
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
    {ok, {{one_for_one, PoolSize * 10, 1}, child_specs(Host, PoolSize)}}.

-spec config_reloaded() -> ok.
config_reloaded() ->
    lists:foreach(fun reload/1, ejabberd_option:hosts()).

-spec reload(binary()) -> ok.
reload(Host) ->
    case is_started(Host) of
	true ->
	    Sup = gen_mod:get_module_proc(Host, ?MODULE),
	    Type = ejabberd_option:sql_type(Host),
	    PoolSize = get_pool_size(Type, Host),
	    lists:foreach(
	      fun(Spec) ->
		      supervisor:start_child(Sup, Spec)
	      end, child_specs(Host, PoolSize)),
	    lists:foreach(
	      fun({Id, _, _, _}) when Id > PoolSize ->
		      case supervisor:terminate_child(Sup, Id) of
			  ok -> supervisor:delete_child(Sup, Id);
			  _ -> ok
		      end;
		 (_) ->
		      ok
	      end, supervisor:which_children(Sup));
	false ->
	    ok
    end.

-spec is_started(binary()) -> boolean().
is_started(Host) ->
    whereis(gen_mod:get_module_proc(Host, ?MODULE)) /= undefined.

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

-spec child_spec(binary(), pos_integer()) -> supervisor:child_spec().
child_spec(Host, I) ->
    #{id => I,
      start => {ejabberd_sql, start_link, [Host, I]},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [?MODULE]}.

-spec child_specs(binary(), pos_integer()) -> [supervisor:child_spec()].
child_specs(Host, PoolSize) ->
    [child_spec(Host, I) || I <- lists:seq(1, PoolSize)].

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
    Filename = case ejabberd_sql:use_multihost_schema() of
        true -> "lite.new.sql";
        false -> "lite.sql"
    end,
    File = filename:join(SqlDir, Filename),
    case file:open(File, [read, binary]) of
        {ok, Fd} ->
            Qs = read_lines(Fd, File, []),
            ok = sqlite3:sql_exec(DB, "begin"),
            [ok = sqlite3:sql_exec(DB, Q) || Q <- Qs],
            ok = sqlite3:sql_exec(DB, "commit");
        {error, Reason} ->
            ?WARNING_MSG("Failed to read SQLite schema file: ~ts",
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
