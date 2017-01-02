%%%----------------------------------------------------------------------
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

-define(SQL_MARK, sql__mark_).
-define(SQL(SQL), ?SQL_MARK(SQL)).

-define(SQL_UPSERT_MARK, sql_upsert__mark_).
-define(SQL_UPSERT(Host, Table, Fields),
        ejabberd_sql:sql_query(Host, ?SQL_UPSERT_MARK(Table, Fields))).
-define(SQL_UPSERT_T(Table, Fields),
        ejabberd_sql:sql_query_t(?SQL_UPSERT_MARK(Table, Fields))).

-record(sql_query, {hash, format_query, format_res, args, loc}).

-record(sql_escape, {string, integer, boolean}).

