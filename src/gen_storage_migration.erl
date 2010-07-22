-module(gen_storage_migration).

-export([migrate_mnesia/3, migrate_odbc/3]).

-include("ejabberd.hrl").

%% @type Migrations = [{OldTable, OldAttributes, MigrateFun}]
migrate_mnesia(Host, Table, Migrations) ->
    SameTableName = [Migration
		     || {OldTable, _, _} = Migration <- Migrations,
			OldTable =:= Table],
    lists:foreach(fun(Migration) ->
			  case (catch migrate_mnesia1(Host, Table, Migration)) of
			      ok -> ok;
			      ignored -> ok;
			      R ->
				  ?ERROR_MSG("Error performing migration ~p:~n~p",
					     [Migration, R])
			  end
		  end, SameTableName),
    DifferentTableName = [Migration
			  || {OldTable, _, _} = Migration <- Migrations,
			     OldTable =/= Table],
    lists:foreach(fun(Migration) ->
			  case (catch migrate_mnesia1(Host, Table, Migration)) of
			      ok -> ok;
			      ignored -> ok;
			      R ->
				  ?ERROR_MSG("Error performing migration ~p:~n~p",
					     [Migration, R])
			  end
		  end, DifferentTableName).

migrate_mnesia1(Host, Table, {OldTable, OldAttributes, MigrateFun}) ->
    case (catch mnesia:table_info(OldTable, attributes)) of
	OldAttributes ->
	    if
		Table =:= OldTable ->
		    %% TODO: move into transaction
		    TmpTable = list_to_atom(atom_to_list(Table) ++ "_tmp"),
		    NewRecordName = gen_storage:table_info(Host, Table, record_name),
		    NewAttributes = gen_storage:table_info(Host, Table, attributes),
		    ?INFO_MSG("Migrating mnesia table ~p via ~p~nfrom ~p~nto ~p",
			      [Table, TmpTable, OldAttributes, NewAttributes]),

		    {atomic, ok} = mnesia:create_table(
				     TmpTable,
				     [{disc_only_copies, [node()]},
				      {type, bag},
				      {local_content, true},
				      {record_name, NewRecordName},
				      {attributes, NewAttributes}]),
		    F1 = fun() ->
				 mnesia:write_lock_table(TmpTable),
				 mnesia:foldl(
				   fun(OldRecord, _) ->
					   NewRecord = MigrateFun(OldRecord),
					   ?DEBUG("~p-~p: ~p -> ~p~n",[OldTable, Table, OldRecord, NewRecord]),
					   if
					       is_tuple(NewRecord) ->
						   mnesia:write(TmpTable, NewRecord, write);
					       true ->
						   ignored
					   end
				   end, ok, OldTable)
			 end,
		    {atomic, ok} = mnesia:transaction(F1),
		    mnesia:delete_table(OldTable),
		    TableInfo = gen_storage:table_info(Host, Table, all),
		    {value, {_, Backend}} = lists:keysearch(backend, 1, TableInfo),
		    gen_storage:create_table(Backend, Host, Table, TableInfo),
		    F2 = fun() ->
				 mnesia:write_lock_table(Table),
				 mnesia:foldl(
				   fun(NewRecord, _) ->
					   ?DEBUG("~p-~p: ~p~n",[OldTable, Table, NewRecord]),
					   gen_storage:write(Host, Table, NewRecord, write)
				   end, ok, TmpTable)
			 end,
		    {atomic, ok} = mnesia:transaction(F2),
		    mnesia:delete_table(TmpTable),
		    ?INFO_MSG("Migration of mnesia table ~p successfully finished", [Table]);

		Table =/= OldTable ->
		    ?INFO_MSG("Migrating mnesia table ~p to ~p~nfrom ~p",
			      [OldTable, Table, OldAttributes]),
		    F1 = fun() ->
				 mnesia:write_lock_table(Table),
				 mnesia:foldl(
				   fun(OldRecord, _) ->
					   NewRecord = MigrateFun(OldRecord),
					   ?DEBUG("~p-~p: ~p -> ~p~n",[OldTable, Table, OldRecord, NewRecord]),
					   if
					       is_tuple(NewRecord) ->
						   gen_storage:write(Host, Table, NewRecord, write);
					       true ->
						   ignored
					   end
				   end, ok, OldTable)
			 end,
		    {atomic, ok} = mnesia:transaction(F1),
		    mnesia:delete_table(OldTable),
		    ?INFO_MSG("Migration of mnesia table ~p successfully finished", [Table]),
		    ok
	    end;
	_ ->
	    ignored
    end.


migrate_odbc(Host, Tables, Migrations) ->
    try ejabberd_odbc:sql_transaction(
	  Host,
	  fun() ->
		  lists:foreach(
		    fun(Migration) ->
			    case (catch migrate_odbc1(Host, Tables, Migration)) of
				ok -> ok;
				ignored -> ok;
				R ->
				    ?ERROR_MSG("Error performing migration ~p:~n~p",
					       [Migration, R])
			    end
		    end, Migrations)
	  end)
    catch exit:{noproc, _Where} ->
	    ?INFO_MSG("Not migrating ODBC on host ~p because no ODBC was configured.", [Host]),
	    ok
    end.

migrate_odbc1(Host, Tables, {OldTable, OldColumns, MigrateFun}) ->
    migrate_odbc1(Host, Tables, {[{OldTable, OldColumns}], MigrateFun});

migrate_odbc1(Host, Tables, {OldTablesColumns, MigrateFun}) ->
    {[OldTable | _] = OldTables,
     [OldColumns | _] = OldColumnsAll} = lists:unzip(OldTablesColumns),
    OldTablesA = [list_to_atom(Table) || Table <- OldTables],
    case [odbc_table_columns_t(OldTable1)
	  || OldTable1 <- OldTables] of
	OldColumnsAll ->
	    ?INFO_MSG("Migrating ODBC table ~p to gen_storage tables ~p", [OldTable, Tables]),

	    %% rename old tables to *_old
	    lists:foreach(fun(OldTable1) ->
				  {updated, _} =
				      ejabberd_odbc:sql_query_t("alter table " ++ OldTable1 ++
								" rename to " ++ OldTable1 ++ "_old")
			  end, OldTables),
	    %% recreate new tables
	    lists:foreach(fun(NewTable) ->
				  case lists:member(NewTable, OldTablesA) of
				      true ->
					  TableInfo =
					      gen_storage:table_info(Host, NewTable, all),
					  {value, {_, Backend}} =
					      lists:keysearch(backend, 1, TableInfo),
					  gen_storage:create_table(Backend, Host,
								   NewTable, TableInfo);
				      false -> ignored
				  end
			  end, Tables),

	    SELECT =
		fun(Columns, Table, Keys) ->
			Table1 = case lists:member(Table, OldTables) of
				     true -> Table ++ "_old";
				     false -> Table
				 end,
			WherePart = case Keys of
					[] -> "";
					_ -> " WHERE " ++
						 string:join([K ++ "=" ++
							      if
								  is_list(V) ->
								      "\"" ++ ejabberd_odbc:escape(V) ++ "\"";
								  is_integer(V) ->
								      integer_to_list(V)
							      end
							      || {K, V} <- Keys],
							     " AND ")
				    end,
			{selected, _, Rows} =
			    ejabberd_odbc:sql_query_t("SELECT " ++ string:join(Columns, ", ") ++
						      " FROM " ++ Table1 ++
						      WherePart),
			[tuple_to_list(Row) || Row <- Rows]
		end,

	    %% TODO: this will need lots of RAM, make it batched
	    OldRows = SELECT(OldColumns, OldTable, []),
	    NRows =
		lists:foldl(fun(OldRow, NRow) ->
				    NewRecords = apply(MigrateFun, [SELECT | OldRow]),
				    if
					is_list(NewRecords) ->
					    lists:foreach(
					      fun(NewRecord) ->
						      %% TODO: gen_storage transaction?
						      gen_storage:dirty_write(Host, NewRecord)
					      end, NewRecords);
					is_tuple(NewRecords) ->
					    gen_storage:dirty_write(Host, NewRecords)
				    end,
				    NRow + 1
			    end, 0, OldRows),

	    lists:foreach(fun(OldTable1) ->
				  {updated, _} = ejabberd_odbc:sql_query_t("drop table " ++ OldTable1 ++ "_old")
			  end, OldTables),

	    ?INFO_MSG("Migrated ODBC table ~p to gen_storage tables ~p (~p rows)", [OldTable, Tables, NRows]);
	_ ->
	    ignored
    end.


odbc_table_columns_t(Table) ->
    case ejabberd_odbc:sql_query_t("select column_name from information_schema.columns where table_name='" ++ Table ++ "'") of
	{selected, _, Columns1} ->
	    Columns2 = lists:map(fun({C}) -> C end, Columns1),
	    Columns2
    end.

