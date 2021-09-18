%%%-------------------------------------------------------------------
%%% File    : mod_admin_update_sql.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Convert SQL DB to the new format
%%% Created :  9 Aug 2017 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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

-module(mod_admin_update_sql).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, mod_options/1,
         get_commands_spec/0, depends/2, mod_doc/0]).

% Commands API
-export([update_sql/0]).


-include("logger.hrl").
-include("ejabberd_commands.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_sql_pt.hrl").
-include("translate.hrl").

%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(?MODULE, get_commands_spec()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(get_commands_spec()).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

%%%
%%% Register commands
%%%

get_commands_spec() ->
    [#ejabberd_commands{name = update_sql, tags = [sql],
                        desc = "Convert PostgreSQL DB to the new format",
                        module = ?MODULE, function = update_sql,
                        args = [],
                        args_example = [],
                        args_desc = [],
                        result = {res, rescode},
                        result_example = ok,
                        result_desc = "Status code: 0 on success, 1 otherwise"}
    ].

update_sql() ->
    lists:foreach(
      fun(Host) ->
              case ejabberd_sql_sup:is_started(Host) of
                  false ->
                      ok;
                  true ->
                      update_sql(Host)
              end
      end, ejabberd_option:hosts()).

-record(state, {host :: binary(),
                dbtype :: mysql | pgsql | sqlite | mssql | odbc,
                escape}).

update_sql(Host) ->
    LHost = jid:nameprep(Host),
    DBType = ejabberd_option:sql_type(LHost),
    IsSupported =
        case DBType of
            pgsql -> true;
            _ -> false
        end,
    if
        not IsSupported ->
            io:format("Converting ~p DB is not supported~n", [DBType]),
            error;
        true ->
            Escape =
                case DBType of
                    mssql -> fun ejabberd_sql:standard_escape/1;
                    sqlite -> fun ejabberd_sql:standard_escape/1;
                    _ -> fun ejabberd_sql:escape/1
                end,
            State = #state{host = LHost,
                           dbtype = DBType,
                           escape = Escape},
            update_tables(State)
    end.

update_tables(State) ->
    add_sh_column(State, "users"),
    drop_pkey(State, "users"),
    add_pkey(State, "users", ["server_host", "username"]),
    drop_sh_default(State, "users"),

    add_sh_column(State, "last"),
    drop_pkey(State, "last"),
    add_pkey(State, "last", ["server_host", "username"]),
    drop_sh_default(State, "last"),

    add_sh_column(State, "rosterusers"),
    drop_index(State, "i_rosteru_user_jid"),
    drop_index(State, "i_rosteru_username"),
    drop_index(State, "i_rosteru_jid"),
    create_unique_index(State, "rosterusers", "i_rosteru_sh_user_jid", ["server_host", "username", "jid"]),
    create_index(State, "rosterusers", "i_rosteru_sh_username", ["server_host", "username"]),
    create_index(State, "rosterusers", "i_rosteru_sh_jid", ["server_host", "jid"]),
    drop_sh_default(State, "rosterusers"),

    add_sh_column(State, "rostergroups"),
    drop_index(State, "pk_rosterg_user_jid"),
    create_index(State, "rostergroups", "i_rosterg_sh_user_jid", ["server_host", "username", "jid"]),
    drop_sh_default(State, "rostergroups"),

    add_sh_column(State, "sr_group"),
    add_pkey(State, "sr_group", ["server_host", "name"]),
    drop_sh_default(State, "sr_group"),

    add_sh_column(State, "sr_user"),
    drop_index(State, "i_sr_user_jid_grp"),
    drop_index(State, "i_sr_user_jid"),
    drop_index(State, "i_sr_user_grp"),
    add_pkey(State, "sr_user", ["server_host", "jid", "grp"]),
    create_index(State, "sr_user", "i_sr_user_sh_jid", ["server_host", "jid"]),
    create_index(State, "sr_user", "i_sr_user_sh_grp", ["server_host", "grp"]),
    drop_sh_default(State, "sr_user"),

    add_sh_column(State, "spool"),
    drop_index(State, "i_despool"),
    create_index(State, "spool", "i_spool_sh_username", ["server_host", "username"]),
    drop_sh_default(State, "spool"),

    add_sh_column(State, "archive"),
    drop_index(State, "i_username"),
    drop_index(State, "i_username_timestamp"),
    drop_index(State, "i_timestamp"),
    drop_index(State, "i_peer"),
    drop_index(State, "i_bare_peer"),
    create_index(State, "archive", "i_archive_sh_username_timestamp", ["server_host", "username", "timestamp"]),
    create_index(State, "archive", "i_archive_sh_timestamp", ["server_host", "timestamp"]),
    create_index(State, "archive", "i_archive_sh_peer", ["server_host", "peer"]),
    create_index(State, "archive", "i_archive_sh_bare_peer", ["server_host", "bare_peer"]),
    drop_sh_default(State, "archive"),

    add_sh_column(State, "archive_prefs"),
    drop_pkey(State, "archive_prefs"),
    add_pkey(State, "archive_prefs", ["server_host", "username"]),
    drop_sh_default(State, "archive_prefs"),

    add_sh_column(State, "vcard"),
    drop_pkey(State, "vcard"),
    add_pkey(State, "vcard", ["server_host", "username"]),
    drop_sh_default(State, "vcard"),

    add_sh_column(State, "vcard_search"),
    drop_pkey(State, "vcard_search"),
    drop_index(State, "i_vcard_search_lfn"),
    drop_index(State, "i_vcard_search_lfamily"),
    drop_index(State, "i_vcard_search_lgiven"),
    drop_index(State, "i_vcard_search_lmiddle"),
    drop_index(State, "i_vcard_search_lnickname"),
    drop_index(State, "i_vcard_search_lbday"),
    drop_index(State, "i_vcard_search_lctry"),
    drop_index(State, "i_vcard_search_llocality"),
    drop_index(State, "i_vcard_search_lemail"),
    drop_index(State, "i_vcard_search_lorgname"),
    drop_index(State, "i_vcard_search_lorgunit"),
    add_pkey(State, "vcard_search", ["server_host", "username"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lfn",       ["server_host", "lfn"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lfamily",   ["server_host", "lfamily"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lgiven",    ["server_host", "lgiven"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lmiddle",   ["server_host", "lmiddle"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lnickname", ["server_host", "lnickname"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lbday",     ["server_host", "lbday"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lctry",     ["server_host", "lctry"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_llocality", ["server_host", "llocality"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lemail",    ["server_host", "lemail"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lorgname",  ["server_host", "lorgname"]),
    create_index(State, "vcard_search", "i_vcard_search_sh_lorgunit",  ["server_host", "lorgunit"]),
    drop_sh_default(State, "vcard_search"),

    add_sh_column(State, "privacy_default_list"),
    drop_pkey(State, "privacy_default_list"),
    add_pkey(State, "privacy_default_list", ["server_host", "username"]),
    drop_sh_default(State, "privacy_default_list"),

    add_sh_column(State, "privacy_list"),
    drop_index(State, "i_privacy_list_username"),
    drop_index(State, "i_privacy_list_username_name"),
    create_index(State, "privacy_list", "i_privacy_list_sh_username", ["server_host", "username"]),
    create_unique_index(State, "privacy_list", "i_privacy_list_sh_username_name", ["server_host", "username", "name"]),
    drop_sh_default(State, "privacy_list"),

    add_sh_column(State, "private_storage"),
    drop_index(State, "i_private_storage_username"),
    drop_index(State, "i_private_storage_username_namespace"),
    add_pkey(State, "private_storage", ["server_host", "username", "namespace"]),
    create_index(State, "private_storage", "i_private_storage_sh_username", ["server_host", "username"]),
    drop_sh_default(State, "private_storage"),

    add_sh_column(State, "roster_version"),
    drop_pkey(State, "roster_version"),
    add_pkey(State, "roster_version", ["server_host", "username"]),
    drop_sh_default(State, "roster_version"),

    add_sh_column(State, "muc_room"),
    drop_sh_default(State, "muc_room"),

    add_sh_column(State, "muc_registered"),
    drop_sh_default(State, "muc_registered"),

    add_sh_column(State, "muc_online_room"),
    drop_sh_default(State, "muc_online_room"),

    add_sh_column(State, "muc_online_users"),
    drop_sh_default(State, "muc_online_users"),

    add_sh_column(State, "motd"),
    drop_pkey(State, "motd"),
    add_pkey(State, "motd", ["server_host", "username"]),
    drop_sh_default(State, "motd"),

    add_sh_column(State, "sm"),
    drop_index(State, "i_sm_sid"),
    drop_index(State, "i_sm_username"),
    add_pkey(State, "sm", ["usec", "pid"]),
    create_index(State, "sm", "i_sm_sh_username", ["server_host", "username"]),
    drop_sh_default(State, "sm"),

    add_sh_column(State, "carboncopy"),
    drop_index(State, "i_carboncopy_ur"),
    drop_index(State, "i_carboncopy_user"),
    add_pkey(State, "carboncopy", ["server_host", "username", "resource"]),
    create_index(State, "carboncopy", "i_carboncopy_sh_user", ["server_host", "username"]),
    drop_sh_default(State, "carboncopy"),

    add_sh_column(State, "push_session"),
    drop_index(State, "i_push_usn"),
    drop_index(State, "i_push_ut"),
    add_pkey(State, "push_session", ["server_host", "username", "timestamp"]),
    create_index(State, "push_session", "i_push_session_susn", ["server_host", "username", "service", "node"]),
    drop_sh_default(State, "push_session"),

    ok.

add_sh_column(#state{dbtype = pgsql} = State, Table) ->
    sql_query(
      State#state.host,
      ["ALTER TABLE ", Table, " ADD COLUMN server_host text NOT NULL DEFAULT '",
       (State#state.escape)(State#state.host),
       "';"]);
add_sh_column(#state{dbtype = mysql} = State, Table) ->
    sql_query(
      State#state.host,
      ["ALTER TABLE ", Table, " ADD COLUMN server_host text NOT NULL DEFAULT '",
       (State#state.escape)(State#state.host),
       "';"]).

drop_pkey(#state{dbtype = pgsql} = State, Table) ->
    sql_query(
      State#state.host,
      ["ALTER TABLE ", Table, " DROP CONSTRAINT ", Table, "_pkey;"]);
drop_pkey(#state{dbtype = mysql} = State, Table) ->
    sql_query(
      State#state.host,
      ["ALTER TABLE ", Table, " DROP PRIMARY KEY;"]).

add_pkey(#state{dbtype = pgsql} = State, Table, Cols) ->
    SCols = string:join(Cols, ", "),
    sql_query(
      State#state.host,
      ["ALTER TABLE ", Table, " ADD PRIMARY KEY (", SCols, ");"]);
add_pkey(#state{dbtype = mysql} = State, Table, Cols) ->
    SCols = string:join(Cols, ", "),
    sql_query(
      State#state.host,
      ["ALTER TABLE ", Table, " ADD PRIMARY KEY (", SCols, ");"]).

drop_sh_default(#state{dbtype = pgsql} = State, Table) ->
    sql_query(
      State#state.host,
      ["ALTER TABLE ", Table, " ALTER COLUMN server_host DROP DEFAULT;"]);
drop_sh_default(#state{dbtype = mysql} = State, Table) ->
    sql_query(
      State#state.host,
      ["ALTER TABLE ", Table, " ALTER COLUMN server_host DROP DEFAULT;"]).

drop_index(#state{dbtype = pgsql} = State, Index) ->
    sql_query(
      State#state.host,
      ["DROP INDEX ", Index, ";"]);
drop_index(#state{dbtype = mysql} = State, Index) ->
    sql_query(
      State#state.host,
      ["DROP INDEX ", Index, ";"]).

create_unique_index(#state{dbtype = pgsql} = State, Table, Index, Cols) ->
    SCols = string:join(Cols, ", "),
    sql_query(
      State#state.host,
      ["CREATE UNIQUE INDEX ", Index, " ON ", Table, " USING btree (",
       SCols, ");"]);
create_unique_index(#state{dbtype = mysql} = State, Table, Index, Cols) ->
    Cols2 = [C ++ "(75)" || C <- Cols],
    SCols = string:join(Cols2, ", "),
    sql_query(
      State#state.host,
      ["CREATE UNIQUE INDEX ", Index, " ON ", Table, "(",
       SCols, ");"]).

create_index(#state{dbtype = pgsql} = State, Table, Index, Cols) ->
    SCols = string:join(Cols, ", "),
    sql_query(
      State#state.host,
      ["CREATE INDEX ", Index, " ON ", Table, " USING btree (",
       SCols, ");"]);
create_index(#state{dbtype = mysql} = State, Table, Index, Cols) ->
    Cols2 = [C ++ "(75)" || C <- Cols],
    SCols = string:join(Cols2, ", "),
    sql_query(
      State#state.host,
      ["CREATE INDEX ", Index, " ON ", Table, "(",
       SCols, ");"]).

sql_query(Host, Query) ->
    io:format("executing \"~ts\" on ~ts~n", [Query, Host]),
    case ejabberd_sql:sql_query(Host, Query) of
        {error, Error} ->
            io:format("error: ~p~n", [Error]),
            ok;
        _ ->
            ok
    end.

mod_options(_) -> [].

mod_doc() ->
    #{desc =>
          ?T("This module can be used to update existing SQL database "
             "from the default to the new schema. Check the section "
             "http://../database/#default-and-new-schemas[Default and New Schemas] for details. "
             "Please note that only PostgreSQL is supported. "
             "When the module is loaded use _`update_sql`_ API.")}.
