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

-export([start_link/1, init/1, reload/1, is_started/1]).

-include("logger.hrl").

start_link(Host) ->
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
    {ok, {{one_for_one, PoolSize * 10, 1}, child_specs(Host, PoolSize)}}.

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
