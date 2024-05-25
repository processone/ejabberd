%%%----------------------------------------------------------------------
%%% File    : ejabberd_sql.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : SQL schema versioning
%%% Created : 15 Aug 2023 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

-module(ejabberd_sql_schema).

-author('alexey@process-one.net').

-export([start/1, update_schema/3,
         get_table_schema/2, get_table_indices/2, print_schema/3,
         test/0]).

-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("ejabberd_ctl.hrl").

start(Host) ->
    case should_update_schema(Host) of
        true ->
            case table_exists(Host, <<"schema_version">>) of
                true ->
                    ok;
                false ->
                    SchemaInfo =
                        ejabberd_sql:sql_query(
                          Host,
                          fun(DBType, DBVersion) ->
                                  #sql_schema_info{
                                     db_type = DBType,
                                     db_version = DBVersion,
                                     new_schema = ejabberd_sql:use_new_schema()}
                          end),
                    Table = filter_table_sh(SchemaInfo, schema_table()),
                    Res = create_table(Host, SchemaInfo, Table),
                    case Res of
                        {error, Error} ->
                            ?ERROR_MSG("Failed to create table ~s: ~p",
                                       [Table#sql_table.name, Error]),
                            {error, Error};
                        _ ->
                            ok
                    end
            end;
        false ->
            ok
    end.

schema_table() ->
    #sql_table{
       name = <<"schema_version">>,
       columns = [#sql_column{name = <<"module">>, type = text},
                  #sql_column{name = <<"version">>, type = bigint}],
       indices = [#sql_index{
                     columns = [<<"module">>],
                     unique = true}]}.

get_table_schema(Host, Table) ->
    ejabberd_sql:sql_query(
      Host,
      fun(pgsql, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select "
                         "  @(a.attname)s, "
                         "  @(pg_catalog.format_type(a.atttypid, a.atttypmod))s "
                         "  from "
                         "    pg_class t, "
                         "    pg_attribute a "
                         "  where "
                         "    a.attrelid = t.oid and "
                         "    a.attnum > 0 and "
                         "    a.atttypid > 0 and "
                         "    t.relkind = 'r' and "
                         "    t.relname=%(Table)s"))
              of
                  {selected, Cols} ->
                      [{Col, string_to_type(SType)} || {Col, SType} <- Cols]
              end;
         (sqlite, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select @(i.name)s, @(i.type)s"
                         "  from pragma_table_info(%(Table)s) as i"))
              of
                  {selected, Cols} ->
                      [{Col, string_to_type(SType)} || {Col, SType} <- Cols]
              end;
         (mysql, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select @(column_name)s, @(column_type)s"
                         "  from information_schema.columns"
                         "  where table_name=%(Table)s and"
                         "        table_schema=schema()"
                         "  order by ordinal_position"))
              of
                  {selected, Cols} ->
                      [{Col, string_to_type(SType)} || {Col, SType} <- Cols]
              end
      end).

get_table_indices(Host, Table) ->
    ejabberd_sql:sql_query(
      Host,
      fun(pgsql, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select "
                         "  @(i.relname)s, "
                         "  @(a.attname)s "
                         "  from "
                         "    pg_class t, "
                         "    pg_class i, "
                         "    pg_index ix, "
                         "    pg_attribute a "
                         "  where "
                         "    t.oid = ix.indrelid and "
                         "    i.oid = ix.indexrelid and "
                         "    a.attrelid = t.oid and "
                         "    a.attnum = ANY(ix.indkey) and "
                         "    t.relkind = 'r' and "
                         "    t.relname=%(Table)s "
                         "  order by "
                         "    i.relname, "
                         "    array_position(ix.indkey, a.attnum)"))
              of
                  {selected, Cols} ->
                      Indices =
                          lists:foldr(
                            fun({IdxName, ColName}, Acc) ->
                                    maps:update_with(
                                      IdxName,
                                      fun(Cs) -> [ColName | Cs] end,
                                      [ColName],
                                      Acc)
                            end, #{}, Cols),
                      maps:to_list(Indices)
              end;
         (sqlite, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select @(i.name)s, @(c.name)s "
                         "  from pragma_index_list(%(Table)s) as i,"
                         "       pragma_index_xinfo(i.name) as c"
                         "  where c.cid >= 0"
                         "  order by i.name, c.seqno"))
              of
                  {selected, Cols} ->
                      Indices =
                          lists:foldr(
                            fun({IdxName, ColName}, Acc) ->
                                    maps:update_with(
                                      IdxName,
                                      fun(Cs) -> [ColName | Cs] end,
                                      [ColName],
                                      Acc)
                            end, #{}, Cols),
                      maps:to_list(Indices)
              end;
         (mysql, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select @(index_name)s, @(column_name)s"
                         "  from information_schema.statistics"
                         "  where table_name=%(Table)s and"
                         "        table_schema=schema()"
                         "  order by index_name, seq_in_index"))
              of
                  {selected, Cols} ->
                      Indices =
                          lists:foldr(
                            fun({IdxName, ColName}, Acc) ->
                                    maps:update_with(
                                      IdxName,
                                      fun(Cs) -> [ColName | Cs] end,
                                      [ColName],
                                      Acc)
                            end, #{}, Cols),
                      maps:to_list(Indices)
              end
      end).

find_index_name(Host, Table, Columns) ->
    Indices = get_table_indices(Host, Table),
    case lists:keyfind(Columns, 2, Indices) of
        false ->
            false;
        {Name, _} ->
            {ok, Name}
    end.

get_version(Host, Module) ->
    SModule = misc:atom_to_binary(Module),
    ejabberd_sql:sql_query(
      Host,
      ?SQL("select @(version)d"
           " from schema_version"
           " where module=%(SModule)s")).

store_version(Host, Module, Version) ->
    SModule = misc:atom_to_binary(Module),
    ?SQL_UPSERT(
       Host,
       "schema_version",
       ["!module=%(SModule)s",
        "version=%(Version)d"]).

table_exists(Host, Table) ->
    ejabberd_sql:sql_query(
      Host,
      fun(pgsql, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select @()b exists (select * from pg_tables "
                         "  where tablename=%(Table)s)"))
              of
                  {selected, [{Res}]} ->
                      Res
              end;
         (sqlite, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select @()b exists"
                         " (select 0 from pragma_table_info(%(Table)s))"))
              of
                  {selected, [{Res}]} ->
                      Res
              end;
         (mysql, _) ->
              case
                  ejabberd_sql:sql_query_t(
                    ?SQL("select @()b exists"
                         " (select 0 from information_schema.tables"
                         "  where table_name=%(Table)s and"
                         "        table_schema=schema())"))
              of
                  {selected, [{Res}]} ->
                      Res
              end
      end).

filter_table_sh(SchemaInfo, Table) ->
    case {SchemaInfo#sql_schema_info.new_schema, Table#sql_table.name} of
        {true, _} ->
            Table;
        {_, <<"route">>} ->
            Table;
        {false, _} ->
            Table#sql_table{
              columns =
                  lists:keydelete(
                    <<"server_host">>, #sql_column.name, Table#sql_table.columns),
              indices =
                  lists:map(
                    fun(Idx) ->
                            Idx#sql_index{
                              columns =
                                  lists:delete(
                                    <<"server_host">>, Idx#sql_index.columns)
                             }
                    end, Table#sql_table.indices)
             }
    end.

string_to_type(SType) ->
    case string:lowercase(SType) of
        <<"text">> -> text;
        <<"mediumtext">> -> text;
        <<"bigint">> -> bigint;
        <<"bigint ", _/binary>> -> bigint;
        <<"bigint(", _/binary>> -> bigint;
        <<"integer">> -> integer;
        <<"int">> -> integer;
        <<"int(", _/binary>> -> integer;
        <<"int ", _/binary>> -> integer;
        <<"smallint">> -> smallint;
        <<"smallint(", _/binary>> -> smallint;
        <<"numeric">> -> numeric;
        <<"decimal", _/binary>> -> numeric;
        <<"bigserial">> -> bigserial;
        <<"boolean">> -> boolean;
        <<"tinyint(1)">> -> boolean;
        <<"tinyint", _/binary>> -> smallint;
        <<"bytea">> -> blob;
        <<"blob">> -> blob;
        <<"timestamp", _/binary>> -> timestamp;
        <<"character(", R/binary>> ->
            {ok, [N], []} = io_lib:fread("~d)", binary_to_list(R)),
            {char, N};
        <<"char(", R/binary>> ->
            {ok, [N], []} = io_lib:fread("~d)", binary_to_list(R)),
            {char, N};
        <<"varchar(", _/binary>> -> text;
        <<"character varying(", _/binary>> -> text;
        T ->
            ?ERROR_MSG("Unknown SQL type '~s'", [T]),
            {undefined, T}
    end.

check_columns_compatibility(RequiredColumns, Columns) ->
    lists:all(
      fun(#sql_column{name = Name, type = Type}) ->
              %io:format("col ~p~n", [{Name, Type}]),
              case lists:keyfind(Name, 1, Columns) of
                  false ->
                      false;
                  {_, Type2} ->
                      %io:format("tt ~p~n", [{Type, Type2}]),
                      case {Type, Type2} of
                          {T, T} -> true;
                          {text, blob} -> true;
                          {{text, _}, blob} -> true;
                          {{text, _}, text} -> true;
                          {{text, _}, {varchar, _}} -> true;
                          {text, {varchar, _}} -> true;
                          {{char, _}, text} -> true;
                          {{varchar, _}, text} -> true;
                          {smallint, integer} -> true;
                          {smallint, bigint} -> true;
                          {smallint, numeric} -> true;
                          {integer, bigint} -> true;
                          {integer, numeric} -> true;
                          {bigint, numeric} -> true;
                          %% a workaround for MySQL definition of mqtt_pub
                          {bigint, integer} -> true;
                          {bigserial, integer} -> true;
                          {bigserial, bigint} -> true;
                          {bigserial, numeric} -> true;
                          _ -> false
                      end
              end
      end, RequiredColumns).

guess_version(Host, Schemas) ->
    LastSchema = lists:max(Schemas),
    SomeTablesExist =
        lists:any(
          fun(Table) ->
                  table_exists(Host, Table#sql_table.name)
          end, LastSchema#sql_schema.tables),
    if
        SomeTablesExist ->
            CompatibleSchemas =
                lists:filter(
                  fun(Schema) ->
                          lists:all(
                            fun(Table) ->
                                    CurrentColumns =
                                        get_table_schema(
                                          Host, Table#sql_table.name),
                                    check_columns_compatibility(
                                      Table#sql_table.columns,
                                      CurrentColumns)
                            end, Schema#sql_schema.tables)
                  end, Schemas),
            case CompatibleSchemas of
                [] -> -1;
                _ ->
                    (lists:max(CompatibleSchemas))#sql_schema.version
            end;
        true ->
            0
    end.

get_current_version(Host, Module, Schemas) ->
    case get_version(Host, Module) of
        {selected, [{Version}]} ->
            Version;
        {selected, []} ->
            Version = guess_version(Host, Schemas),
            if
                Version > 0 ->
                    store_version(Host, Module, Version);
                true ->
                    ok
            end,
            Version
    end.

sqlite_table_copy(Host, SchemaInfo, Table) ->
    ejabberd_sql:sql_transaction(Host,
        fun() ->
            TableName = Table#sql_table.name,
            NewTableName = <<"new_", TableName/binary>>,
            NewTable = Table#sql_table{name = NewTableName},
            create_table_t(SchemaInfo, NewTable),
            SQL2 = <<"INSERT INTO ", NewTableName/binary,
                " SELECT * FROM ", TableName/binary>>,
            ?INFO_MSG("Copying table ~s to ~s:~n~s~n",
                [TableName, NewTableName, SQL2]),
            ejabberd_sql:sql_query_t(SQL2),
            SQL3 = <<"DROP TABLE ", TableName/binary>>,
            ?INFO_MSG("Droping old table ~s:~n~s~n",
                [TableName, SQL2]),
            ejabberd_sql:sql_query_t(SQL3),
            SQL4 = <<"ALTER TABLE ", NewTableName/binary,
                " RENAME TO ", TableName/binary>>,
            ?INFO_MSG("Renameing table ~s to ~s:~n~s~n",
                [NewTableName, TableName, SQL4]),
            ejabberd_sql:sql_query_t(SQL4)
        end).

format_type(#sql_schema_info{db_type = pgsql}, Column) ->
    case Column#sql_column.type of
        text -> <<"text">>;
        {text, _} -> <<"text">>;
        bigint -> <<"bigint">>;
        integer -> <<"integer">>;
        smallint -> <<"smallint">>;
        numeric -> <<"numeric">>;
        boolean -> <<"boolean">>;
        blob -> <<"bytea">>;
        timestamp -> <<"timestamp">>;
        {char, N} -> [<<"character(">>, integer_to_binary(N), <<")">>];
        bigserial -> <<"bigserial">>
    end;
format_type(#sql_schema_info{db_type = sqlite}, Column) ->
    case Column#sql_column.type of
        text -> <<"text">>;
        {text, _} -> <<"text">>;
        bigint -> <<"bigint">>;
        integer -> <<"integer">>;
        smallint -> <<"smallint">>;
        numeric -> <<"numeric">>;
        boolean -> <<"boolean">>;
        blob -> <<"blob">>;
        timestamp -> <<"timestamp">>;
        {char, N} -> [<<"character(">>, integer_to_binary(N), <<")">>];
        bigserial -> <<"integer primary key autoincrement">>
    end;
format_type(#sql_schema_info{db_type = mysql}, Column) ->
    case Column#sql_column.type of
        text -> <<"text">>;
        {text, big} -> <<"mediumtext">>;
        {text, N} when is_integer(N), N < 191 ->
            [<<"varchar(">>, integer_to_binary(N), <<")">>];
        {text, _} -> <<"text">>;
        bigint -> <<"bigint">>;
        integer -> <<"integer">>;
        smallint -> <<"smallint">>;
        numeric -> <<"numeric">>;
        boolean -> <<"boolean">>;
        blob -> <<"blob">>;
        timestamp -> <<"timestamp">>;
        {char, N} -> [<<"character(">>, integer_to_binary(N), <<")">>];
        bigserial -> <<"bigint auto_increment primary key">>
    end.

format_default(#sql_schema_info{db_type = pgsql}, Column) ->
    case Column#sql_column.type of
        text -> <<"''">>;
        {text, _} -> <<"''">>;
        bigint -> <<"0">>;
        integer -> <<"0">>;
        smallint -> <<"0">>;
        numeric -> <<"0">>;
        boolean -> <<"false">>;
        blob -> <<"''">>;
        timestamp -> <<"now()">>
        %{char, N} -> <<"''">>;
        %bigserial -> <<"0">>
    end;
format_default(#sql_schema_info{db_type = sqlite}, Column) ->
    case Column#sql_column.type of
        text -> <<"''">>;
        {text, _} -> <<"''">>;
        bigint -> <<"0">>;
        integer -> <<"0">>;
        smallint -> <<"0">>;
        numeric -> <<"0">>;
        boolean -> <<"false">>;
        blob -> <<"''">>;
        timestamp -> <<"CURRENT_TIMESTAMP">>
        %{char, N} -> <<"''">>;
        %bigserial -> <<"0">>
    end;
format_default(#sql_schema_info{db_type = mysql}, Column) ->
    case Column#sql_column.type of
        text -> <<"('')">>;
        {text, _} -> <<"('')">>;
        bigint -> <<"0">>;
        integer -> <<"0">>;
        smallint -> <<"0">>;
        numeric -> <<"0">>;
        boolean -> <<"false">>;
        blob -> <<"('')">>;
        timestamp -> <<"CURRENT_TIMESTAMP">>
        %{char, N} -> <<"''">>;
        %bigserial -> <<"0">>
    end.

escape_name(#sql_schema_info{db_type = pgsql}, <<"type">>) ->
    <<"\"type\"">>;
escape_name(_SchemaInfo, ColumnName) ->
    ColumnName.

format_column_def(SchemaInfo, Column) ->
    [<<"    ">>,
     escape_name(SchemaInfo, Column#sql_column.name), <<" ">>,
     format_type(SchemaInfo, Column),
     <<" NOT NULL">>,
     case Column#sql_column.default of
         false -> [];
         true ->
             [<<" DEFAULT ">>, format_default(SchemaInfo, Column)]
     end,
     case lists:keyfind(sql_references, 1, Column#sql_column.opts) of
         false -> [];
         #sql_references{table = T, column = C} ->
             [<<" REFERENCES ">>, T, <<"(">>, C, <<") ON DELETE CASCADE">>]
     end].

format_mysql_index_column(Table, ColumnName) ->
    {value, Column} =
        lists:keysearch(
          ColumnName, #sql_column.name, Table#sql_table.columns),
    NeedsSizeLimit =
        case Column#sql_column.type of
            {text, N} when is_integer(N), N < 191 -> false;
            {text, _} -> true;
            text -> true;
            _ -> false
        end,
    if
        NeedsSizeLimit ->
            [ColumnName, <<"(191)">>];
        true ->
            ColumnName
    end.

format_create_index(#sql_schema_info{db_type = pgsql}, Table, Index) ->
    TableName = Table#sql_table.name,
    Unique =
        case Index#sql_index.unique of
            true -> <<"UNIQUE ">>;
            false -> <<"">>
        end,
    Name = [<<"i_">>, TableName, <<"_">>,
            lists:join(
              <<"_">>,
              Index#sql_index.columns)],
    [<<"CREATE ">>, Unique, <<"INDEX ">>, Name, <<" ON ">>, TableName,
     <<" USING btree (">>,
     lists:join(
       <<", ">>,
       Index#sql_index.columns),
     <<");">>];
format_create_index(#sql_schema_info{db_type = sqlite}, Table, Index) ->
    TableName = Table#sql_table.name,
    Unique =
        case Index#sql_index.unique of
            true -> <<"UNIQUE ">>;
            false -> <<"">>
        end,
    Name = [<<"i_">>, TableName, <<"_">>,
            lists:join(
              <<"_">>,
              Index#sql_index.columns)],
    [<<"CREATE ">>, Unique, <<"INDEX ">>, Name, <<" ON ">>, TableName,
     <<"(">>,
     lists:join(
       <<", ">>,
       Index#sql_index.columns),
     <<");">>];
format_create_index(#sql_schema_info{db_type = mysql}, Table, Index) ->
    TableName = Table#sql_table.name,
    Unique =
        case Index#sql_index.unique of
            true -> <<"UNIQUE ">>;
            false -> <<"">>
        end,
    Name = [<<"i_">>, TableName, <<"_">>,
            lists:join(
              <<"_">>,
              Index#sql_index.columns)],
    [<<"CREATE ">>, Unique, <<"INDEX ">>, Name,
     <<" USING BTREE ON ">>, TableName,
     <<"(">>,
     lists:join(
       <<", ">>,
       lists:map(
         fun(Col) ->
                 format_mysql_index_column(Table, Col)
         end, Index#sql_index.columns)),
     <<");">>].

format_primary_key(#sql_schema_info{db_type = mysql}, Table) ->
    case lists:filter(
           fun(#sql_index{meta = #{primary_key := true}}) -> true;
              (_) -> false
           end, Table#sql_table.indices) of
        [] -> [];
        [I] ->
            [[<<"    ">>,
              <<"PRIMARY KEY (">>,
              lists:join(
                <<", ">>,
                lists:map(
                  fun(Col) ->
                          format_mysql_index_column(Table, Col)
                  end, I#sql_index.columns)),
              <<")">>]]
    end;
format_primary_key(_SchemaInfo, Table) ->
    case lists:filter(
           fun(#sql_index{meta = #{primary_key := true}}) -> true;
              (_) -> false
           end, Table#sql_table.indices) of
        [] -> [];
        [I] ->
            [[<<"    ">>,
              <<"PRIMARY KEY (">>,
              lists:join(<<", ">>, I#sql_index.columns),
              <<")">>]]
    end.

format_add_primary_key(#sql_schema_info{db_type = sqlite} = SchemaInfo,
                       Table, Index) ->
    format_create_index(SchemaInfo, Table, Index);
format_add_primary_key(#sql_schema_info{db_type = pgsql}, Table, Index) ->
    TableName = Table#sql_table.name,
    [<<"ALTER TABLE ">>, TableName, <<" ADD PRIMARY KEY (">>,
     lists:join(
       <<", ">>,
       Index#sql_index.columns),
     <<");">>];
format_add_primary_key(#sql_schema_info{db_type = mysql}, Table, Index) ->
    TableName = Table#sql_table.name,
    [<<"ALTER TABLE ">>, TableName, <<" ADD PRIMARY KEY (">>,
     lists:join(
       <<", ">>,
       lists:map(
         fun(Col) ->
                 format_mysql_index_column(Table, Col)
         end, Index#sql_index.columns)),
     <<");">>].

format_create_table(#sql_schema_info{db_type = pgsql} = SchemaInfo, Table) ->
    TableName = Table#sql_table.name,
    [iolist_to_binary(
       [<<"CREATE TABLE ">>, TableName, <<" (\n">>,
        lists:join(
          <<",\n">>,
          lists:map(
            fun(C) -> format_column_def(SchemaInfo, C) end,
            Table#sql_table.columns) ++
              format_primary_key(SchemaInfo, Table)),
        <<"\n);\n">>])] ++
        lists:flatmap(
          fun(#sql_index{meta = #{primary_key := true}}) ->
                  [];
             (#sql_index{meta = #{ignore := true}}) ->
                  [];
             (I) ->
                  [iolist_to_binary(
                     [format_create_index(SchemaInfo, Table, I),
                      <<"\n">>])]
          end,
          Table#sql_table.indices);
format_create_table(#sql_schema_info{db_type = sqlite} = SchemaInfo, Table) ->
    TableName = Table#sql_table.name,
    [iolist_to_binary(
       [<<"CREATE TABLE ">>, TableName, <<" (\n">>,
        lists:join(
          <<",\n">>,
          lists:map(
            fun(C) -> format_column_def(SchemaInfo, C) end,
            Table#sql_table.columns) ++
              format_primary_key(SchemaInfo, Table)),
        <<"\n);\n">>])] ++
        lists:flatmap(
          fun(#sql_index{meta = #{primary_key := true}}) ->
                  [];
             (#sql_index{meta = #{ignore := true}}) ->
                  [];
             (I) ->
                  [iolist_to_binary(
                     [format_create_index(SchemaInfo, Table, I),
                      <<"\n">>])]
          end,
          Table#sql_table.indices);
format_create_table(#sql_schema_info{db_type = mysql} = SchemaInfo, Table) ->
    TableName = Table#sql_table.name,
    [iolist_to_binary(
      [<<"CREATE TABLE ">>, TableName, <<" (\n">>,
       lists:join(
         <<",\n">>,
         lists:map(
           fun(C) -> format_column_def(SchemaInfo, C) end,
           Table#sql_table.columns) ++
             format_primary_key(SchemaInfo, Table)),
       <<"\n) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;\n">>])] ++
        lists:flatmap(
          fun(#sql_index{meta = #{primary_key := true}}) ->
                  [];
             (#sql_index{meta = #{ignore := true}}) ->
                  [];
             (I) ->
                  [iolist_to_binary(
                     [format_create_index(SchemaInfo, Table, I),
                      <<"\n">>])]
          end,
          Table#sql_table.indices).

create_table(Host, SchemaInfo, Table) ->
    ejabberd_sql:sql_query(Host,
        fun() ->
            create_table_t(SchemaInfo, Table)
        end).

create_table_t(SchemaInfo, Table) ->
    SQLs = format_create_table(SchemaInfo, Table),
    ?INFO_MSG("Creating table ~s:~n~s~n",
        [Table#sql_table.name, SQLs]),
    lists:foreach(
        fun(SQL) -> ejabberd_sql:sql_query_t(SQL) end, SQLs),
    case Table#sql_table.post_create of
        undefined ->
            ok;
        F when is_function(F, 1) ->
            PostSQLs = F(SchemaInfo),
            lists:foreach(
                fun(SQL) -> ejabberd_sql:sql_query_t(SQL) end,
                PostSQLs)
    end.

create_tables(Host, Module, SchemaInfo, Schema) ->
    lists:foreach(
      fun(Table) ->
              Res = create_table(Host, SchemaInfo, Table),
              case Res of
                  {error, Error} ->
                      ?ERROR_MSG("Failed to create table ~s: ~p",
                                 [Table#sql_table.name, Error]),
                      error(Error);
                  _ ->
                      ok
              end
      end, Schema#sql_schema.tables),
    store_version(Host, Module, Schema#sql_schema.version).

should_update_schema(Host) ->
    SupportedDB =
        case ejabberd_option:sql_type(Host) of
            pgsql -> true;
            sqlite -> true;
            mysql -> true;
            _ -> false
        end,
    case ejabberd_option:update_sql_schema() andalso SupportedDB of
        true ->
            case ejabberd_sql:use_new_schema() of
                true ->
                    lists:member(sql, ejabberd_option:auth_method(Host));
                false ->
                    true
            end;
        false ->
            false
    end.

preprocess_table(SchemaInfo, Table) ->
    Table1 = filter_table_sh(SchemaInfo, Table),
    ImplicitPK =
        case SchemaInfo#sql_schema_info.db_type of
            pgsql -> false;
            sqlite ->
                case lists:keyfind(bigserial, #sql_column.type,
                                   Table1#sql_table.columns) of
                    false -> false;
                    #sql_column{name = Name} -> {ok, Name}
                end;
            mysql ->
                case lists:keyfind(bigserial, #sql_column.type,
                                   Table1#sql_table.columns) of
                    false -> false;
                    #sql_column{name = Name} -> {ok, Name}
                end
        end,
    Indices =
        case ImplicitPK of
            false ->
                {Inds, _} =
                    lists:mapfoldl(
                      fun(#sql_index{unique = true} = I, false) ->
                              {I#sql_index{
                                 meta = (I#sql_index.meta)#{primary_key => true}},
                               true};
                         (I, Acc) ->
                              {I, Acc}
                      end, false, Table1#sql_table.indices),
                Inds;
            {ok, CN} ->
                lists:map(
                  fun(#sql_index{columns = [CN1]} = I) when CN == CN1 ->
                          I#sql_index{
                            meta = (I#sql_index.meta)#{ignore => true}};
                     (I) -> I
                  end,
                  Table1#sql_table.indices)
        end,
    Table1#sql_table{indices = Indices}.

preprocess_schemas(SchemaInfo, Schemas) ->
    lists:map(
      fun(Schema) ->
              Schema#sql_schema{
                tables = lists:map(
                           fun(T) ->
                                   preprocess_table(SchemaInfo, T)
                           end,
                           Schema#sql_schema.tables)}
      end, Schemas).

update_schema(Host, Module, RawSchemas) ->
    case should_update_schema(Host) of
        true ->
            SchemaInfo =
                ejabberd_sql:sql_query(
                  Host,
                  fun(DBType, DBVersion) ->
                          #sql_schema_info{
                             db_type = DBType,
                             db_version = DBVersion,
                             new_schema = ejabberd_sql:use_new_schema()}
                  end),
            Schemas = preprocess_schemas(SchemaInfo, RawSchemas),
            Version = get_current_version(Host, Module, Schemas),
            LastSchema = lists:max(Schemas),
            LastVersion = LastSchema#sql_schema.version,
            case Version of
                _ when Version < 0 ->
                    ?ERROR_MSG("Can't update SQL schema for module ~p, please do it manually", [Module]);
                0 ->
                    create_tables(Host, Module, SchemaInfo, LastSchema);
                LastVersion ->
                    ok;
                _ when LastVersion < Version ->
                    ?ERROR_MSG("The current SQL schema for module ~p is ~p, but the latest known schema in the module is ~p", [Module, Version, LastVersion]);
                _ ->
                    lists:foreach(
                      fun(Schema) ->
                              if
                                  Schema#sql_schema.version > Version ->
                                      do_update_schema(Host, Module,
                                                       SchemaInfo, Schema);
                                  true ->
                                      ok
                              end
                      end, lists:sort(Schemas))
            end;
        false ->
            ok
    end.

do_update_schema(Host, Module, SchemaInfo, Schema) ->
    lists:foreach(
      fun({add_column, TableName, ColumnName}) ->
              {value, Table} =
                  lists:keysearch(
                    TableName, #sql_table.name, Schema#sql_schema.tables),
              {value, Column} =
                  lists:keysearch(
                    ColumnName, #sql_column.name, Table#sql_table.columns),
              Res =
                  ejabberd_sql:sql_query(
                    Host,
                    fun(DBType, _DBVersion) ->
                            Def = format_column_def(SchemaInfo, Column),
                            Default = format_default(SchemaInfo, Column),
                            SQLs =
                                [[<<"ALTER TABLE ">>,
                                  TableName,
                                  <<" ADD COLUMN\n">>,
                                  Def,
                                  <<" DEFAULT ">>,
                                  Default, <<";\n">>]] ++
                                case Column#sql_column.default of
                                    false when DBType /= sqlite ->
                                        [[<<"ALTER TABLE ">>,
                                          TableName,
                                          <<" ALTER COLUMN ">>,
                                          ColumnName,
                                          <<" DROP DEFAULT;">>]];
                                    _ ->
                                        []
                                end,
                            ?INFO_MSG("Add column ~s/~s:~n~s~n",
                                      [TableName,
                                       ColumnName,
                                       SQLs]),
                            lists:foreach(
                              fun(SQL) -> ejabberd_sql:sql_query_t(SQL) end,
                              SQLs)
                    end),
              case Res of
                  {error, Error} ->
                      ?ERROR_MSG("Failed to update table ~s: ~p",
                                 [TableName, Error]),
                      error(Error);
                  _ ->
                      ok
              end;
         ({drop_column, TableName, ColumnName}) ->
              Res =
                  ejabberd_sql:sql_query(
                    Host,
                    fun(_DBType, _DBVersion) ->
                            SQL = [<<"ALTER TABLE ">>,
                                   TableName,
                                   <<" DROP COLUMN ">>,
                                   ColumnName,
                                   <<";">>],
                            ?INFO_MSG("Drop column ~s/~s:~n~s~n",
                                      [TableName,
                                       ColumnName,
                                       SQL]),
                            ejabberd_sql:sql_query_t(SQL)
                    end),
              case Res of
                  {error, Error} ->
                      ?ERROR_MSG("Failed to update table ~s: ~p",
                                 [TableName, Error]),
                      error(Error);
                  _ ->
                      ok
              end;
         ({create_index, TableName, Columns1}) ->
              Columns =
                  case ejabberd_sql:use_new_schema() of
                      true ->
                          Columns1;
                      false ->
                          lists:delete(
                            <<"server_host">>, Columns1)
                  end,
              {value, Table} =
                  lists:keysearch(
                    TableName, #sql_table.name, Schema#sql_schema.tables),
              {value, Index} =
                  lists:keysearch(
                    Columns, #sql_index.columns, Table#sql_table.indices),
              case Index#sql_index.meta of
                  #{ignore := true} -> ok;
                  _ ->
                      Res =
                          ejabberd_sql:sql_query(
                            Host,
                            fun() ->
                                    case Index#sql_index.meta of
                                        #{primary_key := true} ->
                                            SQL1 = format_add_primary_key(
                                                     SchemaInfo, Table, Index),
                                            SQL = iolist_to_binary(SQL1),
                                            ?INFO_MSG("Add primary key ~s/~p:~n~s~n",
                                                      [Table#sql_table.name,
                                                       Index#sql_index.columns,
                                                       SQL]),
                                            ejabberd_sql:sql_query_t(SQL);
                                        _ ->
                                            SQL1 = format_create_index(
                                                     SchemaInfo, Table, Index),
                                            SQL = iolist_to_binary(SQL1),
                                            ?INFO_MSG("Create index ~s/~p:~n~s~n",
                                                      [Table#sql_table.name,
                                                       Index#sql_index.columns,
                                                       SQL]),
                                            ejabberd_sql:sql_query_t(SQL)
                                    end
                            end),
                      case Res of
                          {error, Error} ->
                              ?ERROR_MSG("Failed to update table ~s: ~p",
                                         [TableName, Error]),
                              error(Error);
                          _ ->
                              ok
                      end
              end;
         ({update_primary_key, TableName, Columns1}) ->
             Columns =
                 case ejabberd_sql:use_new_schema() of
                     true ->
                         Columns1;
                     false ->
                         lists:delete(
                             <<"server_host">>, Columns1)
                 end,
             {value, Table} =
                 lists:keysearch(
                     TableName, #sql_table.name, Schema#sql_schema.tables),
             {value, Index} =
                 lists:keysearch(
                     Columns, #sql_index.columns, Table#sql_table.indices),
             Res =
             case SchemaInfo#sql_schema_info.db_type of
                 sqlite ->
                     sqlite_table_copy(Host, SchemaInfo, Table);
                 pgsql ->
                     TableName = Table#sql_table.name,
                     SQL1 = [<<"ALTER TABLE ">>, TableName, <<" DROP CONSTRAINT ",
                         TableName/binary,"_pkey, ",
                         "ADD PRIMARY KEY (">>,
                         lists:join(
                             <<", ">>,
                             Index#sql_index.columns),
                         <<");">>],
                     SQL = iolist_to_binary(SQL1),
                     ?INFO_MSG("Update primary key ~s/~p:~n~s~n",
                         [Table#sql_table.name,
                             Index#sql_index.columns,
                             SQL]),
                     ejabberd_sql:sql_query(
                         Host,
                         fun(_DBType, _DBVersion) ->
                             ejabberd_sql:sql_query_t(SQL)
                         end);
                 mysql ->
                     TableName = Table#sql_table.name,
                     SQL1 = [<<"ALTER TABLE ">>, TableName, <<" DROP PRIMARY KEY, "
                     "ADD PRIMARY KEY (">>,
                         lists:join(
                             <<", ">>,
                             lists:map(
                                 fun(Col) ->
                                     format_mysql_index_column(Table, Col)
                                 end, Index#sql_index.columns)),
                         <<");">>],
                     SQL = iolist_to_binary(SQL1),
                     ?INFO_MSG("Update primary key ~s/~p:~n~s~n",
                         [Table#sql_table.name,
                             Index#sql_index.columns,
                             SQL]),
                     ejabberd_sql:sql_query(
                         Host,
                         fun(_DBType, _DBVersion) ->
                             ejabberd_sql:sql_query_t(SQL)
                         end)
             end,
             case Res of
                 {error, Error} ->
                     ?ERROR_MSG("Failed to update table ~s: ~p",
                         [TableName, Error]),
                     error(Error);
                 _ ->
                     ok
             end;
         ({drop_index, TableName, Columns1}) ->
              Columns =
                  case ejabberd_sql:use_new_schema() of
                      true ->
                          Columns1;
                      false ->
                          lists:delete(
                            <<"server_host">>, Columns1)
                  end,
              case find_index_name(Host, TableName, Columns) of
                  false ->
                      ?ERROR_MSG("Can't find an index to drop for ~s/~p",
                                 [TableName, Columns]);
                  {ok, IndexName} ->
                      Res =
                          ejabberd_sql:sql_query(
                            Host,
                            fun(DBType, _DBVersion) ->
                                    SQL =
                                        case DBType of
                                            mysql ->
                                                [<<"DROP INDEX ">>,
                                                 IndexName,
                                                 <<" ON ">>,
                                                 TableName,
                                                 <<";">>];
                                            _ ->
                                                [<<"DROP INDEX ">>,
                                                 IndexName, <<";">>]
                                        end,
                                    ?INFO_MSG("Drop index ~s/~p:~n~s~n",
                                              [TableName,
                                               Columns,
                                               SQL]),
                                    ejabberd_sql:sql_query_t(SQL)
                            end),
                      case Res of
                          {error, Error} ->
                              ?ERROR_MSG("Failed to update table ~s: ~p",
                                         [TableName, Error]),
                              error(Error);
                          _ ->
                              ok
                      end
              end
      end, Schema#sql_schema.update),
    store_version(Host, Module, Schema#sql_schema.version).


print_schema(SDBType, SDBVersion, SNewSchema) ->
    {DBType, DBVersion} =
        case SDBType of
            "pgsql" ->
                case string:split(SDBVersion, ".") of
                    [SMajor, SMinor] ->
                        try {list_to_integer(SMajor), list_to_integer(SMinor)} of
                            {Major, Minor} ->
                                {pgsql, Major * 10000 + Minor}
                        catch _:_ ->
                                io:format("pgsql version be in the form of "
                                          "Major.Minor, e.g. 16.1~n"),
                                {error, error}
                        end;
                    _ ->
                        io:format("pgsql version be in the form of "
                                  "Major.Minor, e.g. 16.1~n"),
                        {error, error}
                end;
            "mysql" ->
                case ejabberd_sql:parse_mysql_version(SDBVersion, 0) of
                    {ok, V} ->
                        {mysql, V};
                    error ->
                        io:format("mysql version be in the same form as "
                                  "SELECT VERSION() returns, e.g. 8.2.0~n"),
                        {error, error}
                end;
            "sqlite" ->
                {sqlite, undefined};
            _ ->
                io:format("db_type must be one of the following: "
                          "'pgsql', 'mysql', 'sqlite'~n"),
                {error, error}
        end,
    NewSchema =
        case SNewSchema of
            "0" -> false;
            "1" -> true;
            "false" -> false;
            "true" -> true;
            _ ->
                io:format("new_schema must be one of the following: "
                          "'0', '1', 'false', 'true'~n"),
                error
        end,
    case {DBType, NewSchema} of
        {error, _} -> ?STATUS_ERROR;
        {_, error} -> ?STATUS_ERROR;
        _ ->
            SchemaInfo =
                #sql_schema_info{
                   db_type = DBType,
                   db_version = DBVersion,
                   new_schema = NewSchema},
            Mods = ejabberd_config:beams(all),
            lists:foreach(
              fun(Mod) ->
                      case erlang:function_exported(Mod, sql_schemas, 0) of
                          true ->
                              Schemas = Mod:sql_schemas(),
                              Schemas2 = preprocess_schemas(SchemaInfo, Schemas),
                              Schema = lists:max(Schemas2),
                              SQLs =
                                  lists:flatmap(
                                    fun(Table) ->
                                            SQLs = format_create_table(SchemaInfo, Table),
                                            PostSQLs =
                                                case Table#sql_table.post_create of
                                                    undefined ->
                                                        [];
                                                    F when is_function(F, 1) ->
                                                        PSQLs = F(SchemaInfo),
                                                        lists:map(
                                                          fun(S) ->
                                                                  [S, <<"\n">>]
                                                          end, PSQLs)
                                                end,
                                            SQLs ++ PostSQLs
                                    end, Schema#sql_schema.tables),
                              io:format("~s~n", [SQLs]);
                          false ->
                              ok
                      end
              end, Mods),
            ?STATUS_SUCCESS
    end.


test() ->
    Schemas =
        [#sql_schema{
            version = 2,
            tables =
                [#sql_table{
                    name = <<"archive2">>,
                    columns = [#sql_column{name = <<"username">>, type = text},
                               #sql_column{name = <<"server_host">>, type = text},
                               #sql_column{name = <<"timestamp">>, type = bigint},
                               #sql_column{name = <<"peer">>, type = text},
                               #sql_column{name = <<"bare_peer">>, type = text},
                               #sql_column{name = <<"xml">>, type = {text, big}},
                               #sql_column{name = <<"txt">>, type = {text, big}},
                               #sql_column{name = <<"id">>, type = bigserial},
                               #sql_column{name = <<"kind">>, type = text},
                               #sql_column{name = <<"nick">>, type = text},
                               #sql_column{name = <<"origin_id">>, type = text},
                               #sql_column{name = <<"type">>, type = text},
                               #sql_column{name = <<"created_at">>, type = timestamp,
                                           default = true}],
                    indices = [#sql_index{columns = [<<"id">>],
                                          unique = true},
                               #sql_index{
                                  columns = [<<"server_host">>, <<"username">>, <<"timestamp">>]},
                               #sql_index{
                                  columns = [<<"server_host">>, <<"username">>, <<"peer">>]},
                               #sql_index{
                                  columns = [<<"server_host">>, <<"username">>, <<"bare_peer">>]},
                               #sql_index{
                                  columns = [<<"server_host">>, <<"origin_id">>]},
                               #sql_index{
                                  columns = [<<"server_host">>, <<"timestamp">>]}
                              ]}],
           update =
                [{add_column, <<"archive2">>, <<"origin_id">>},
                 {create_index, <<"archive2">>,
                  [<<"server_host">>, <<"origin_id">>]},
                 {drop_index, <<"archive2">>,
                  [<<"server_host">>, <<"origin_id">>]},
                 {drop_column, <<"archive2">>, <<"origin_id">>},
                 {create_index, <<"archive2">>, [<<"id">>]}
                ]},
         #sql_schema{
            version = 1,
            tables =
                [#sql_table{
                    name = <<"archive2">>,
                    columns = [#sql_column{name = <<"username">>, type = text},
                               #sql_column{name = <<"server_host">>, type = text},
                               #sql_column{name = <<"timestamp">>, type = bigint},
                               #sql_column{name = <<"peer">>, type = text},
                               #sql_column{name = <<"bare_peer">>, type = text},
                               #sql_column{name = <<"xml">>, type = {text, big}},
                               #sql_column{name = <<"txt">>, type = {text, big}},
                               #sql_column{name = <<"id">>, type = bigserial},
                               #sql_column{name = <<"kind">>, type = {text, 10}},
                               #sql_column{name = <<"nick">>, type = text},
                               #sql_column{name = <<"created_at">>, type = timestamp,
                                           default = true}],
                    indices = [#sql_index{
                                  columns = [<<"server_host">>, <<"username">>, <<"timestamp">>]},
                               #sql_index{
                                  columns = [<<"server_host">>, <<"username">>, <<"peer">>]},
                               #sql_index{
                                  columns = [<<"server_host">>, <<"username">>, <<"bare_peer">>]},
                               #sql_index{
                                  columns = [<<"server_host">>, <<"timestamp">>]}
                              ]}]}],
    update_schema(<<"localhost">>, mod_foo, Schemas).
