%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 July 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-module(ejabberd_sql_schema).

%% API
-export([load/3, list_tables/2]).

-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
load(Host, Mod, Type) when Type == mysql; Type == pgsql; Type == sqlite ->
    Dir = misc:sql_dir(),
    File = filename:join([Dir, Type, atom_to_list(Mod) ++ ".sql"]),
    case read_queries(File) of
	{ok, Queries} ->
	    case exec_queries(Host, Mod, Type, Queries) of
		ok -> ok;
		{error, Why} ->
		    ?CRITICAL_MSG("Failed to create tables for ~s: ~p",
				  [Mod, Why]),
		    {error, db_failure}
	    end;
	{error, _} = Err ->
	    Err
    end;
load(_, _, _) ->
    ok.

list_tables(Host, Type) ->
    Query = list_tables_query(Type),
    case ejabberd_sql:sql_query(Host, Query) of
	{selected, _, Res} ->
	    {ok, [T || [T|_] <- Res]};
	{error, _} = Err ->
	    Err
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
format_syntax_error({Line, Mod, Reason}) ->
    io_lib:format("at line ~B: ~s", [Line, Mod:format_error(Reason)]).

read_queries(File) ->
    case file:read_file(File) of
	{ok, Data} ->
	    case parse_queries(binary_to_list(Data)) of
		{error, Why} ->
		    ?ERROR_MSG("Failed to parse SQL file ~s: ~s",
			       [File, format_syntax_error(Why)]),
		    {error, parse_error};
		{ok, Queries} ->
		    RawQueries = re:split(Data, ";\\s*", [trim]),
		    {ok, group_queries(lists:zip(Queries, RawQueries))}
	    end;
	{error, Why} = Err ->
	    ?ERROR_MSG("Failed to read SQL queries from ~s: ~s",
		       [File, file:format_error(Why)]),
	    Err
    end.

parse_queries(Queries) ->
    case sql_lexer:string(Queries) of
	{ok, Tokens, _} ->
	    sql_codec:parse(Tokens);
	{error, Why, _} ->
	    {error, Why}
    end.

group_queries(Queries) ->
    lists:foldl(
      fun({Query, Raw}, Acc) when element(1, Query) == create_table ->
	      Table = element(2, Query),
	      Columns = [element(1, Col) || Col <- element(3, Query)],
	      maps:put(Table, {Columns, [Raw]}, Acc);
	 ({Query, Raw}, Acc) when element(1, Query) == create_index ->
	      Table = element(4, Query),
	      {Cols, Qs} = maps:get(Table, Acc),
	      maps:put(Table, {Cols, Qs++[Raw]}, Acc)
      end, #{}, Queries).

exec_queries(Host, Mod, Type, Map) ->
    ?DEBUG("Creating/altering tables for ~s", [Mod]),
    Escape = case Type of
		 mssql -> fun ejabberd_sql:standard_escape/1;
		 sqlite -> fun ejabberd_sql:standard_escape/1;
		 _ -> fun ejabberd_sql:escape/1
	     end,
    F = fun() ->
		lists:foreach(
		  fun({Tab, {NewCols, Queries}}) ->
			  case table_exists(Type, Tab) of
			      {true, OldCols} ->
				  alter_table(Host, Type, Tab, Escape,
					      NewCols, OldCols);
			      false ->
				  ?INFO_MSG("Creating SQL table: ~s", [Tab]),
				  lists:foreach(
				    fun ejabberd_sql:sql_query_t/1,
				    Queries)
			  end
		  end, maps:to_list(Map))
	end,
    case ejabberd_sql:sql_transaction(Host, F) of
	{atomic, _} -> ok;
	{aborted, Reason} -> {error, Reason}
    end.

table_exists(Type, Table) ->
    Q1 = table_exists_query(Type, Table),
    case sql_query(Q1) of
	{selected, _, [_|_]} ->
	    Q2 = table_columns_query(Type, Table),
	    case sql_query(Q2) of
		{selected, Columns, _} ->
		    {true, Columns};
		Err ->
		    Err
	    end;
	{selected, _, []} -> false;
	Err -> Err
    end.

list_tables_query(sqlite) ->
    ["SELECT name FROM sqlite_master WHERE type='table'"];
list_tables_query(mysql) ->
    ["SHOW TABLES"];
list_tables_query(pgsql) ->
    ["SELECT tablename FROM pg_catalog.pg_tables where schemaname NOT IN ",
     "('pg_catalog', 'information_schema')"].

table_exists_query(pgsql, T) ->
    ["SELECT * FROM pg_catalog.pg_tables where tablename='", T, "'"];
table_exists_query(mysql, T) ->
    ["SHOW TABLES LIKE '", T, "'"];
table_exists_query(sqlite, T) ->
    ["SELECT * FROM sqlite_master WHERE type='table' and name='", T, "'"].

table_columns_query(_, T) ->
    ["SELECT * FROM ", T, " where 0=1"].

alter_table(Host, Type, Tab, Escape, NewCols, OldCols) ->
    Add = NewCols -- OldCols,
    alter_server_host(Host, Type, Tab, Escape, Add).

alter_server_host(Host, Type, Tab, Escape, Add) ->
    case lists:member(<<"server_host">>, Add) of
	true ->
	    ?WARNING_MSG("Upgrading table ~s to multi-domain schema", [Tab]),
	    add_server_host(binary_to_list(Tab), Type, Host, Escape);
	false ->
	    ok
    end.

%%%===================================================================
%%% SQL queries to upgrade to the New(R)(TM) Schema
%%%===================================================================
add_server_host("users", Type , Host, Escape) ->
    add_sh_column(Type, "users", Host, Escape),
    drop_pkey(Type, "users"),
    add_pkey(Type, "users", ["server_host", "username"]),
    set_sh(Type, "users", Host, Escape);
add_server_host("last", Type, Host, Escape) ->
    add_sh_column(Type, "last", Host, Escape),
    drop_pkey(Type, "last"),
    add_pkey(Type, "last", ["server_host", "username"]),
    set_sh(Type, "last", Host, Escape);
add_server_host("rosterusers", Type, Host, Escape) ->
    add_sh_column(Type, "rosterusers", Host, Escape),
    drop_index(Type, "i_rosteru_user_jid"),
    drop_index(Type, "i_rosteru_username"),
    drop_index(Type, "i_rosteru_jid"),
    create_unique_index(Type, "rosterusers", "i_rosteru_sh_user_jid", ["server_host", "username", "jid"]),
    create_index(Type, "rosterusers", "i_rosteru_sh_username", ["server_host", "username"]),
    create_index(Type, "rosterusers", "i_rosteru_sh_jid", ["server_host", "jid"]),
    set_sh(Type, "rosterusers", Host, Escape);
add_server_host("rostergroups", Type, Host, Escape) ->
    add_sh_column(Type, "rostergroups", Host, Escape),
    drop_index(Type, "pk_rosterg_user_jid"),
    create_index(Type, "rostergroups", "i_rosterg_sh_user_jid", ["server_host", "username", "jid"]),
    set_sh(Type, "rostergroups", Host, Escape);
add_server_host("roster_version", Type, Host, Escape) ->
    add_sh_column(Type, "roster_version", Host, Escape),
    drop_pkey(Type, "roster_version"),
    add_pkey(Type, "roster_version", ["server_host", "username"]),
    set_sh(Type, "roster_version", Host, Escape);
add_server_host("sr_group", Type, Host, Escape) ->
    add_sh_column(Type, "sr_group", Host, Escape),
    add_pkey(Type, "sr_group", ["server_host", "name"]),
    set_sh(Type, "sr_group", Host, Escape);
add_server_host("sr_user", Type, Host, Escape) ->
    add_sh_column(Type, "sr_user", Host, Escape),
    drop_index(Type, "i_sr_user_jid_grp"),
    drop_index(Type, "i_sr_user_jid"),
    drop_index(Type, "i_sr_user_grp"),
    add_pkey(Type, "sr_user", ["server_host", "jid", "grp"]),
    create_index(Type, "sr_user", "i_sr_user_sh_jid", ["server_host", "jid"]),
    create_index(Type, "sr_user", "i_sr_user_sh_grp", ["server_host", "grp"]),
    set_sh(Type, "sr_user", Host, Escape);
add_server_host("spool", Type, Host, Escape) ->
    add_sh_column(Type, "spool", Host, Escape),
    drop_index(Type, "i_despool"),
    create_index(Type, "spool", "i_spool_sh_username", ["server_host", "username"]),
    set_sh(Type, "spool", Host, Escape);
add_server_host("archive", Type, Host, Escape) ->
    add_sh_column(Type, "archive", Host, Escape),
    drop_index(Type, "i_username"),
    drop_index(Type, "i_username_timestamp"),
    drop_index(Type, "i_timestamp"),
    drop_index(Type, "i_peer"),
    drop_index(Type, "i_bare_peer"),
    create_index(Type, "archive", "i_archive_sh_username_timestamp", ["server_host", "username", "timestamp"]),
    create_index(Type, "archive", "i_archive_sh_timestamp", ["server_host", "timestamp"]),
    create_index(Type, "archive", "i_archive_sh_peer", ["server_host", "peer"]),
    create_index(Type, "archive", "i_archive_sh_bare_peer", ["server_host", "bare_peer"]),
    set_sh(Type, "archive", Host, Escape);
add_server_host("archive_prefs", Type, Host, Escape) ->
    add_sh_column(Type, "archive_prefs", Host, Escape),
    drop_pkey(Type, "archive_prefs"),
    add_pkey(Type, "archive_prefs", ["server_host", "username"]),
    set_sh(Type, "archive_prefs", Host, Escape);
add_server_host("vcard", Type, Host, Escape) ->
    add_sh_column(Type, "vcard", Host, Escape),
    drop_pkey(Type, "vcard"),
    add_pkey(Type, "vcard", ["server_host", "username"]),
    set_sh(Type, "vcard", Host, Escape);
add_server_host("vcard_search", Type, Host, Escape) ->
    add_sh_column(Type, "vcard_search", Host, Escape),
    drop_pkey(Type, "vcard_search"),
    drop_index(Type, "i_vcard_search_lfn"),
    drop_index(Type, "i_vcard_search_lfamily"),
    drop_index(Type, "i_vcard_search_lgiven"),
    drop_index(Type, "i_vcard_search_lmiddle"),
    drop_index(Type, "i_vcard_search_lnickname"),
    drop_index(Type, "i_vcard_search_lbday"),
    drop_index(Type, "i_vcard_search_lctry"),
    drop_index(Type, "i_vcard_search_llocality"),
    drop_index(Type, "i_vcard_search_lemail"),
    drop_index(Type, "i_vcard_search_lorgname"),
    drop_index(Type, "i_vcard_search_lorgunit"),
    add_pkey(Type, "vcard_search", ["server_host", "username"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lfn",       ["server_host", "lfn"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lfamily",   ["server_host", "lfamily"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lgiven",    ["server_host", "lgiven"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lmiddle",   ["server_host", "lmiddle"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lnickname", ["server_host", "lnickname"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lbday",     ["server_host", "lbday"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lctry",     ["server_host", "lctry"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_llocality", ["server_host", "llocality"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lemail",    ["server_host", "lemail"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lorgname",  ["server_host", "lorgname"]),
    create_index(Type, "vcard_search", "i_vcard_search_sh_lorgunit",  ["server_host", "lorgunit"]),
    set_sh(Type, "vcard_search", Host, Escape);
add_server_host("privacy_default_list", Type, Host, Escape) ->
    add_sh_column(Type, "privacy_default_list", Host, Escape),
    drop_pkey(Type, "privacy_default_list"),
    add_pkey(Type, "privacy_default_list", ["server_host", "username"]),
    set_sh(Type, "privacy_default_list", Host, Escape);
add_server_host("privacy_list", Type, Host, Escape) ->
    add_sh_column(Type, "privacy_list", Host, Escape),
    drop_index(Type, "i_privacy_list_username"),
    drop_index(Type, "i_privacy_list_username_name"),
    create_index(Type, "privacy_list", "i_privacy_list_sh_username", ["server_host", "username"]),
    create_unique_index(Type, "privacy_list", "i_privacy_list_sh_username_name",
			["server_host", "username", "name"]),
    set_sh(Type, "privacy_list", Host, Escape);
add_server_host("private_storage", Type, Host, Escape) ->
    add_sh_column(Type, "private_storage", Host, Escape),
    drop_index(Type, "i_private_storage_username"),
    drop_index(Type, "i_private_storage_username_namespace"),
    add_pkey(Type, "private_storage", ["server_host", "username", "namespace"]),
    create_index(Type, "private_storage", "i_private_storage_sh_username", ["server_host", "username"]),
    set_sh(Type, "private_storage", Host, Escape);
add_server_host(Tab, Type, Host, Escape) when Tab == "muc_room";
						   Tab == "muc_registered";
						   Tab == "muc_online_room";
						   Tab == "muc_online_users" ->
    add_sh_column(Type, Tab, Host, Escape),
    set_sh(Type, Tab, Host, Escape);
add_server_host("motd", Type, Host, Escape) ->
    add_sh_column(Type, "motd", Host, Escape),
    drop_pkey(Type, "motd"),
    add_pkey(Type, "motd", ["server_host", "username"]),
    set_sh(Type, "motd", Host, Escape);
add_server_host("sm", Type, Host, Escape) ->
    add_sh_column(Type, "sm", Host, Escape),
    drop_index(Type, "i_sm_sid"),
    drop_index(Type, "i_sm_username"),
    add_pkey(Type, "sm", ["usec", "pid"]),
    create_index(Type, "sm", "i_sm_sh_username", ["server_host", "username"]),
    set_sh(Type, "sm", Host, Escape);
add_server_host("carboncopy", Type, Host, Escape) ->
    add_sh_column(Type, "carboncopy", Host, Escape),
    drop_index(Type, "i_carboncopy_ur"),
    drop_index(Type, "i_carboncopy_user"),
    add_pkey(Type, "carboncopy", ["server_host", "username", "resource"]),
    create_index(Type, "carboncopy", "i_carboncopy_sh_user", ["server_host", "username"]),
    set_sh(Type, "carboncopy", Host, Escape);
add_server_host("push_session", Type, Host, Escape) ->
    add_sh_column(Type, "push_session", Host, Escape),
    drop_index(Type, "i_push_usn"),
    drop_index(Type, "i_push_ut"),
    add_pkey(Type, "push_session", ["server_host", "username", "timestamp"]),
    create_index(Type, "push_session", "i_push_session_susn", ["server_host", "username", "service", "node"]),
    set_sh(Type, "push_session", Host, Escape);
add_server_host(Tab, _, _, _) ->
    ?WARNING_MSG("Unknown table to convert: ~s", [Tab]).

add_sh_column(mysql, Table, Host, Escape) ->
    sql_query(
      ["ALTER TABLE ", Table, " ADD COLUMN server_host varchar(191) NOT NULL DEFAULT '",
       Escape(Host), "'"]);
add_sh_column(pgsql, Table, Host, Escape) ->
    sql_query(
      ["ALTER TABLE ", Table, " ADD COLUMN server_host text NOT NULL DEFAULT '",
       Escape(Host), "'"]);
add_sh_column(sqlite, Table, _Host, _Escape) ->
    sql_query(
      ["ALTER TABLE ", Table, " ADD COLUMN server_host text NOT NULL DEFAULT ''"]).

drop_pkey(mysql, Table) ->
    sql_query(["ALTER TABLE ", Table, " DROP PRIMARY KEY"]);
drop_pkey(pgsql, Table) ->
    sql_query(["ALTER TABLE ", Table, " DROP CONSTRAINT ", Table, "_pkey"]);
drop_pkey(sqlite, Table) ->
    sql_query(["ALTER TABLE ", Table, " DROP PRIMARY KEY"]).

add_pkey(mysql, Table, Cols) ->
    SCols = string:join(Cols, ", "),
    sql_query(["ALTER TABLE ", Table, " ADD PRIMARY KEY (", SCols, ")"]);
add_pkey(pgsql, Table, Cols) ->
    SCols = string:join(Cols, ", "),
    sql_query(["ALTER TABLE ", Table, " ADD PRIMARY KEY (", SCols, ")"]);
add_pkey(sqlite, Table, Cols) ->
    create_unique_index(sqlite, Table, string:join(["i"|Cols], "_"), Cols).

set_sh(sqlite, Table, Host, Escape) ->
    sql_query(["UPDATE ", Table, " SET server_host='", Escape(Host), "'"]);
set_sh(_, Table, _Host, _Escape) ->
    sql_query(["ALTER TABLE ", Table, " ALTER COLUMN server_host DROP DEFAULT"]).

drop_index(mysql, Index) ->
    sql_query(["DROP INDEX IF EXISTS ", Index]);
drop_index(_pgsql, Index) ->
    sql_query(["DROP INDEX IF EXISTS ", Index]).

create_unique_index(mysql, Table, Index, Cols) ->
    Cols2 = [C ++ "(75)" || C <- Cols],
    SCols = string:join(Cols2, ", "),
    sql_query(["CREATE UNIQUE INDEX ", Index, " ON ", Table, "(", SCols, ")"]);
create_unique_index(_pgsql, Table, Index, Cols) ->
    SCols = string:join(Cols, ", "),
    sql_query(["CREATE UNIQUE INDEX ", Index, " ON ", Table, " (", SCols, ")"]).

create_index(mysql, Table, Index, Cols) ->
    Cols2 = [C ++ "(75)" || C <- Cols],
    SCols = string:join(Cols2, ", "),
    sql_query(["CREATE INDEX ", Index, " ON ", Table, "(", SCols, ")"]);
create_index(_pgsql, Table, Index, Cols) ->
    SCols = string:join(Cols, ", "),
    sql_query(["CREATE INDEX ", Index, " ON ", Table, " (", SCols, ")"]).

sql_query(Query) ->
    ejabberd_sql:sql_query_t(iolist_to_binary(Query)).
