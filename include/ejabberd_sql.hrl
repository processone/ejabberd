%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-define(SQL_INSERT_MARK, sql_insert__mark_).
-define(SQL_INSERT(Table, Fields), ?SQL_INSERT_MARK(Table, Fields)).

-ifdef(COMPILER_REPORTS_ONLY_LINES).
-record(sql_query, {hash :: binary(),
		    format_query :: fun(),
		    format_res :: fun(),
		    args :: fun(),
		    flags :: non_neg_integer(),
		    loc :: {module(), pos_integer()}}).
-else.
-record(sql_query, {hash :: binary(),
		    format_query :: fun(),
		    format_res :: fun(),
		    args :: fun(),
		    flags :: non_neg_integer(),
		    loc :: {module(), {pos_integer(), pos_integer()}}}).
-endif.

-record(sql_escape, {string :: fun((binary()) -> binary()),
		     integer :: fun((integer()) -> binary()),
		     boolean :: fun((boolean()) -> binary()),
		     in_array_string :: fun((binary()) -> binary()),
		     like_escape :: fun(() -> binary())}).


-record(sql_index, {columns,
                    unique = false :: boolean(),
                    meta = #{}}).
-record(sql_column, {name :: binary(),
                     type,
                     default = false,
                     opts = []}).
-record(sql_table, {name :: binary(),
                    columns :: [#sql_column{}],
                    indices = [] :: [#sql_index{}],
                    post_create}).
-record(sql_schema, {version :: integer(),
                     tables :: [#sql_table{}],
                     update = []}).
-record(sql_references, {table :: binary(),
                         column :: binary()}).
