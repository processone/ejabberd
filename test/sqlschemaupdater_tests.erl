%%%-------------------------------------------------------------------
%%% Author  : Pawel Chmielowski <pawel@process-one.net>
%%% Created : 20 Jul 2026 by Pawel Chmielowski <pawel@process-one.net>p
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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

-module(sqlschemaupdater_tests).

-compile(export_all).

-include("suite.hrl").
-include("ejabberd_sql_pt.hrl").

%%%==================================


single_cases() ->
    {sqlschemaupdater_single,
     [sequence],
     [single_test(add_column),
      single_test(change_column)]}.


%% Interactions


apply_schemas(Config, Schemas) ->
    Server = ?config(server, Config),
    ejabberd_sql:sql_query(Server, <<"drop table if exists schemaupdate;">>),
    ejabberd_sql:sql_query(Server, <<"delete from schema_version where module='schemaupdate';">>),
    lists:foreach(
      fun(#sql_schema{} = Schema) ->
              ?match(ok, ejabberd_sql_schema:update_schema(Server, schemaupdate, [Schema]));
         (ToExec) when is_binary(ToExec) ->
              ejabberd_sql:sql_query(Server, ToExec);
         ([ToExec, Result]) when is_binary(ToExec) ->
              ?match({selected, _, Result}, ejabberd_sql:sql_query(Server, ToExec))
      end,
      Schemas).


add_column(Config) ->
    Schemas = [#sql_schema{
                 version = 1,
                 tables = [#sql_table{
                             name = <<"schemaupdate">>,
                             columns = [#sql_column{name = <<"first">>, type = integer}]
                            }]
                },
               <<"insert into schemaupdate (first) values (1)">>,
               #sql_schema{
                 version = 2,
                 tables = [#sql_table{
                             name = <<"schemaupdate">>,
                             columns = [#sql_column{name = <<"first">>, type = integer},
                                        #sql_column{name = <<"second">>, type = integer}]
                            }],
                 update = [{add_column, <<"schemaupdate">>, <<"second">>}]
                },
               <<"insert into schemaupdate (first, second) values (1, 2)">>,
               [<<"select first, second from schemaupdate order by first, second">>, [[<<"1">>, <<"0">>], [<<"1">>, <<"2">>]]]],
    apply_schemas(Config, Schemas).


change_column(Config) ->
    Schemas = [#sql_schema{
                 version = 1,
                 tables = [#sql_table{
                             name = <<"schemaupdate">>,
                             columns = [#sql_column{name = <<"first">>, type = boolean}]
                            }]
                },
               <<"insert into schemaupdate (first) values (false)">>,
               #sql_schema{
                 version = 2,
                 tables = [#sql_table{
                             name = <<"schemaupdate">>,
                             columns = [#sql_column{name = <<"first">>, type = smallint}]
                            }],
                 update = [{change_column_type, <<"schemaupdate">>, <<"first">>}]
                },
               [<<"select first from schemaupdate">>, [[<<"0">>]]],
               <<"update schemaupdate set first=2">>,
               [<<"select first from schemaupdate">>, [[<<"2">>]]]],
    apply_schemas(Config, Schemas).


single_test(T) ->
    list_to_atom("sqlschemaupdater_" ++ atom_to_list(T)).
