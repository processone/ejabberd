-module(gen_storage_odbc).
-author('stephan@spaceboyz.net').
-behaviour(gen_storage).


-export([table_info/1, prepare_tabdef/2,
	 create_table/1, delete_table/1,
	 add_table_copy/3, add_table_index/2,
	 read/3, select/3, count_records/2, write/3,
	 delete/3, delete_object/3,
	 dirty_read/2, dirty_select/2, dirty_count_records/2, dirty_write/2,
	 dirty_delete/2, dirty_delete_object/2,
	 delete_where/2, dirty_delete_where/2,
	 async_dirty/2, sync_dirty/2,
	 transaction/2]).

%% TODO: append 's' to table names in SQL?

-record(tabdef, {name :: atom(), % Table name
		 record_name :: atom(), % Record name
		 table_type :: 'set' | 'bag', % atom() = set | bag
		 attributes :: [string()], % Columns
		 columns :: string(), % "\"col1\", \"col2\" ,..."
		 column_names :: [{string(), [string()]}], % [{string(), [string()]}] (already quoted)
		 types :: [{string(), atom() | tuple()}],
		 host :: string()
			 }).
-record(odbc_cont, {tabdef, sql, offset = 0, limit}).

-include("ejabberd.hrl"). % for ?DEBUG macro
-include_lib("exmpp/include/exmpp.hrl").  % for #jid{} and #xmlel{}


table_info(#tabdef{record_name = RecordName,
		   table_type = TableType,
		   attributes = Attributes,
		   types = Types,
		   host = Host}) ->
    [{record_name, RecordName},
     {table_type, TableType},
     {attributes, lists:map(fun erlang:list_to_atom/1, Attributes)},
     {types, [{list_to_atom(A), T}
	      || {A, T} <- Types]},
     {odbc_host, Host}].

%%% Def preparation %%%

prepare_tabdef(Name, TabOpts) ->
    {value, {_, Host}} = lists:keysearch(odbc_host, 1, TabOpts),
    RecordName =
	case lists:keysearch(record_name, 1, TabOpts) of
	    {value, {_, RecordName1}} -> RecordName1;
	    false -> Name
	end,
    TableType =
	case lists:keysearch(type, 1, TabOpts) of
	    {value, {_, TableType1}} -> TableType1;
	    false -> set
	end,
    Types =
	case lists:keysearch(types, 1, TabOpts) of
	    {value, {_, Types1}} ->
		[{atom_to_list(A), T} || {A, T} <- Types1];
	    false ->
		[]
	end,
    {value, {_, Attributes1}} = lists:keysearch(attributes, 1, TabOpts),
    Attributes = lists:map(fun atom_to_list/1, Attributes1),
    ColumnQuote = case ejabberd_odbc:db_type(Host) of
		      mysql -> "`";
		      _ -> "\""
		  end,
    ColumnNames =
	lists:map(
	  fun(Attribute) ->
		  case lists:keysearch(Attribute, 1, Types) of
		      {value, {_, Type}} when is_tuple(Type) ->
			  Tokens = string:tokens(Attribute, "_"),
			  if
			      length(Tokens) == size(Type) ->
				  TokensQuoted = [ColumnQuote ++ Token ++ ColumnQuote ||
						     Token <- Tokens],
				  {Attribute, TokensQuoted};
			      true ->
				  {Attribute,
				   fold_decrementing(
				     fun(N, A) ->
					     [ColumnQuote ++
					      Attribute ++ integer_to_list(N) ++
					      ColumnQuote | A]
				     end, [], size(Type))}
			  end;
		      _ ->
			  {Attribute, [ColumnQuote ++ Attribute ++ ColumnQuote]}
		  end
	  end, Attributes),
    AttributesFull =
	lists:foldr(
	  fun(Attribute, A) ->
		  T = tabdef_column_names(#tabdef{column_names = ColumnNames},
					  Attribute),
		  T ++ A
	  end, [], Attributes),
    Columns = string:join(AttributesFull, ", "),
    #tabdef{name = Name,
	    record_name = RecordName,
	    table_type = TableType,
	    attributes = Attributes,
	    columns = Columns,
	    column_names = ColumnNames,
	    types = Types,
	    host = Host}.


create_table(#tabdef{name = Tab,
		     host = Host,
		     table_type = TableType,
		     attributes = Attributes = [KeyName | _],
		     types = Types} = TabDef) ->
    {A, K} =
	lists:foldr(
	  fun(Attribute, {Q, K}) ->
		  IsKey = TableType =:= bag orelse
		      Attribute =:= KeyName,
		  %% The "packet" column in the table offline_msg,
		  %% must be "text" in order to be large enough to
		  %% contain a full stanza, not limited to a small VARCHAR():
		  NoTextKeys = IsKey andalso ejabberd_odbc:db_type(Host) =:= mysql
		      andalso Attribute /= "packet",
		  KN = tabdef_column_names(TabDef, Attribute),
		  case lists:keysearch(Attribute, 1, Types) of
		      {value, {_, Tuple}} when is_tuple(Tuple) ->
			  {0, [], T} =
			      lists:foldr(
				fun(TT, {N, [Name | Names], T}) ->
					A = Name ++ " " ++
					    type_to_sql_type(TT, NoTextKeys) ++
					    case T of
						"" -> "";
						_ -> ", " ++ T
					    end,
					{N - 1, Names, A}
				end, {size(Tuple), lists:reverse(KN), ""}, tuple_to_list(Tuple));
		      {value, {_, T1}} ->
			  [Column] = KN,
			  T = [Column, $ , type_to_sql_type(T1, NoTextKeys)];
		      false ->
			  [Column] = KN,
			  T = [Column, $ , type_to_sql_type(text, NoTextKeys)]
		  end,
		  K2 = if
			   IsKey ->
			       KN ++ K;
			   true ->
			       K
		       end,
		  Q2 = case Q of
			   "" -> T;
			   _ -> [T, ", ", Q]
		       end,
		  {Q2, K2}
	  end, {"", []}, Attributes),
    TabS = atom_to_list(Tab),
    PKey = case TableType of
	       set -> primary_key_string(Host, K);
	       bag -> []
	   end,
    case odbc_command(Host,
		      ["CREATE TABLE ", TabS,
		       " (", A, PKey, ")"]) of
	ok ->
	    case TableType of
		bag ->
		    KeyColumns = tabdef_column_names(TabDef, KeyName),
		    Q = ["CREATE INDEX ", TabS, "_bag ON ",
			 TabS, " (", string:join(KeyColumns, "(75), "), "(75))"],
		    case odbc_command(Host, Q) of
			ok ->
			    {atomic, ok};
			{error, Reason} ->
			    {aborted, Reason}
		    end;
		_ ->
		    {atomic, ok}
	    end;
	{error, Reason} ->
	    {aborted, Reason}
    end.

%% This 105 limits the size of fields in the primary key.
%% That prevents MySQL from complaining when setting the
%% last_activity key (text, text) with this error:
%% #42000Specified key was too long; max key length is 1000bytes"
%% Similarly for rosteritem and other tables.
primary_key_string(Host, K) ->
    Dbtype = ejabberd_odbc:db_type(Host),
    case Dbtype of
	mysql ->
	    [", PRIMARY KEY (", string:join(K, "(105), "), "(105))"];
	odbc ->
	    [", PRIMARY KEY (", string:join(K, ", "), ")"];
	pgsql ->
	    [", PRIMARY KEY (", string:join(K, ", "), ")"]
    end.

type_to_sql_type(Type, false = _NoTextKeys) ->
    type_to_sql_type(Type);
type_to_sql_type(Type, true = _NoTextKeys) ->
    case type_to_sql_type(Type) of
	"TEXT" -> "VARCHAR(255)";
	"text" -> "VARCHAR(255)";
	R -> R
    end.

type_to_sql_type(pid) -> "TEXT";
type_to_sql_type(xmlel) -> "TEXT";
type_to_sql_type(jid) -> "TEXT";
type_to_sql_type(ljid) -> "TEXT";
type_to_sql_type(atom) -> "TEXT";
type_to_sql_type(binary) -> "TEXT";
type_to_sql_type(A) when is_atom(A) -> atom_to_list(A).


delete_table(#tabdef{name = Tab, host = Host}) ->
    case odbc_command(Host, ["DROP TABLE ", atom_to_list(Tab)]) of
	ok ->
	    {atomic, ok};
	{error, Reason} ->
	    {aborted, Reason}
    end.

add_table_copy(_, _, _) ->
    ignored.

add_table_index(#tabdef{name = Tab, host = Host} = TabDef, Attribute) ->
    TabS = atom_to_list(Tab),
    AttributeS = atom_to_list(Attribute),
    A = tabdef_column_names(TabDef, AttributeS),
    Q = ["CREATE INDEX ", TabS, $_, AttributeS,
	 " ON ", TabS, " (", string:join(A, "(75), "), "(75))"],
    case odbc_command(Host, Q) of
	ok ->
	    {atomic, ok};
	{error, Reason} ->
	    {aborted, Reason}
    end.

dirty_read(#tabdef{host = Host} = TabDef,
	   Key) ->
    Q = prepare_select_query(TabDef, Key),
    rows_to_result(TabDef, odbc_query(Host, Q)).

read(TabDef, Key, _LockKind) ->
    Q = prepare_select_query(TabDef, Key),
    rows_to_result(TabDef, odbc_query_t(Q)).

prepare_select_query(#tabdef{name = Tab,
			     columns = Columns} = TabDef,
		     Key) ->
    ["SELECT ", Columns,
     " FROM ", atom_to_list(Tab),
     " WHERE ", prepare_where_rule(TabDef, Key)].

prepare_where_rule(#tabdef{attributes = [KeyAttr | _]} = TabDef,
		   Key) ->
    prepare_where_rule(TabDef, KeyAttr, Key).

prepare_where_rule(#tabdef{name = Tab} = TabDef,
		   Attribute, Value) ->
    Columns = tabdef_column_names(TabDef, Attribute),
    TabS = atom_to_list(Tab),
    if
	is_tuple(Value) andalso
	length(Columns) > 1 ->
	    string:join(
	      lists:zipwith(
		fun(C, V) ->
			[TabS, $., C, " = ", format(V)]
		end, Columns, tuple_to_list(Value)),
	      " AND ");
	true ->
	    [Column] = Columns,
	    [TabS, $., Column, " = ", format(Value)]
    end.

select(#odbc_cont{tabdef = TabDef,
		  sql = SQL,
		  limit = Limit,
		  offset = Offset} = Cont) ->
    Q = [SQL, " LIMIT ", integer_to_list(Limit), " OFFSET ", integer_to_list(Offset)],
    Results = rows_to_result(TabDef, odbc_query_t(Q)),
    if
	length(Results) == 0 ->
	    '$end_of_table';
	true ->
	    {Results, Cont#odbc_cont{offset = Offset + length(Results)}}
    end.

select(TabDef, MatchRules, N) ->
    Q = prepare_select_rules_query(TabDef, MatchRules),
    case N of
	undefined ->
	    rows_to_result(TabDef, odbc_query_t(Q));
	_ when is_integer(N) ->
	    #tabdef{attributes = [KeyAttr | _]} = TabDef,
	    KeyColumns = tabdef_column_names(TabDef, KeyAttr),
	    %% Use ordering!
	    Q2 = [Q, " ORDER BY ", string:join(KeyColumns, ",")],
	    %% TODO: correct for bag tables
	    Cont = #odbc_cont{tabdef = TabDef,
			      sql = Q2, limit = N},
	    select(Cont)
    end.


dirty_select(#tabdef{host = Host} = TabDef, MatchRules) ->
    Q = prepare_select_rules_query(TabDef, MatchRules),
    rows_to_result(TabDef, odbc_query(Host, Q)).


prepare_select_rules_query(#tabdef{name = Tab,
				   columns = Columns} = TabDef,
			   MatchRules) ->
    WherePart = prepare_where_match_rules(TabDef, MatchRules),
    ["SELECT ", Columns,
     " FROM ", atom_to_list(Tab),
     WherePart].


prepare_where_match_rules(TabDef, MatchRules) ->
    W1 = [prepare_match_rule(TabDef, Rule) || Rule <- MatchRules],
    W2 = remove_omits(W1),
    case W2 of
	[] -> "";
	_ -> [" WHERE ", string:join(W2, " AND ")]
    end.


%% TODO: {not, R}

prepare_match_rule(TabDef, T)
  when element(1, T) =:= 'and'; element(1, T) =:= 'andalso';
       element(1, T) =:= 'or'; element(1, T) =:= 'orelse' ->
    [Op | Rules] = tuple_to_list(T),
    W1 = lists:map(
	   fun(Rule) ->
		   prepare_match_rule(TabDef, Rule)
	   end, Rules),
    if
	Op =:= 'and' orelse Op =:= 'andalso' ->
	    W2 = remove_omits(W1),
	    string:join(W2, " AND ");
	Op =:= 'or' orelse Op =:= 'orelse' ->
	    AlwaysTrue = lists:member(omit, W1),
	    if
		AlwaysTrue -> omit;
		true -> string:join(W1, " OR ")
	    end
    end;

prepare_match_rule(#tabdef{name = Tab} = TabDef,
		   {Op, Attribute, Value}) ->
    case tabdef_column_names(TabDef, Attribute) of
	[Column] ->
	    prepare_match_op(Tab, Op, Column, Value);
	Columns ->
	    W1 = lists:zipwith(
		   fun(C, V) ->
			   prepare_match_op(Tab, Op, C, V)
		   end, Columns, tuple_to_list(Value)),
	    W2 = remove_omits(W1),
	    string:join(W2, " AND ")
    end.

prepare_match_op(_Tab, _Op, _Column, '_') ->
    omit;
prepare_match_op(Tab, Op, Column, Value)
  when Op =:= '=='; Op =:= '=:=' ->
    prepare_match_op(Tab, '=', Column, Value);
prepare_match_op(Tab, '=<', Column, Value) ->
    prepare_match_op(Tab, '<=', Column, Value);
prepare_match_op(Tab, '=/=', Column, Value) ->
    prepare_match_op(Tab, '!=', Column, Value);
prepare_match_op(Tab, like, Column, Value) ->
    io_lib:format("~s.~s LIKE ~s", [Tab, Column, format(make_pattern(Value))]);
prepare_match_op(Tab, Op, Column, Value) ->
    io_lib:format("~s.~s ~s ~s", [Tab, Column, Op, format(Value)]).

make_pattern(S) ->
    make_pattern(S, []).
make_pattern([], R) ->
    lists:reverse(R);
make_pattern(['_' | S], R) ->
    make_pattern(S, [$% | R]);
make_pattern([C | S], R) ->
    make_pattern(S, [C | R]).

remove_omits(L) ->
    lists:filter(fun(E) ->
			 E =/= omit
		 end, L).

rows_to_result(#tabdef{record_name = RecordName,
		       attributes = Attributes,
		       types = Types}, Rows) ->
    %% TODO: this can be cached per-table in the tabdef
    TypesList =
	lists:map(
	  fun(Attribute) ->
		  case lists:keysearch(Attribute, 1, Types) of
		      {value, {_, T}} -> T;
		      false -> text
		  end
	  end, Attributes),
    lists:map(
      fun(RowTuple) ->
	      {_, Row} =
		  row_to_result(tuple_to_list(RowTuple),
				TypesList, []),
	      list_to_tuple([RecordName | Row])
      end, Rows).


row_to_result(Row, [], Result) ->
    {Row, lists:reverse(Result)};

row_to_result([null | Row], [_ | Types], Result) ->
    row_to_result(Row, Types, [undefined | Result]);
row_to_result([Field | Row], [Type | Types], Result) ->
    case Type of
	int ->
	    Row2 = Row,
	    R = list_to_integer(Field);
	bigint ->
	    Row2 = Row,
	    R = list_to_integer(Field);
	text ->
	    Row2 = Row,
	    R = Field;
	binary ->
	    Row2 = Row,
	    R = list_to_binary(Field);
	pid ->
	    Row2 = Row,
	    R = list_to_pid(Field);
	xmlel ->
	    Row2 = Row,
	    [R] = exmpp_xml:parse_document(Field, [names_as_atom]);
	jid ->
	    Row2 = Row,
	    R = exmpp_jid:parse(Field);
	ljid ->
	    Row2 = Row,
	    R = jlib:short_prepd_jid(exmpp_jid:parse(Field));
	atom ->
	    Row2 = Row,
	    R = list_to_atom(Field);
	_ when is_tuple(Type) ->
	    {Row2, R1} = row_to_result([Field | Row], tuple_to_list(Type), []),
	    R = list_to_tuple(R1)
    end,
    row_to_result(Row2, Types, [R | Result]).


dirty_count_records(#tabdef{host = Host,
			    attributes = [KeyAttr | _],
			    name = Tab} = TabDef, MatchRules) ->
    WherePart = prepare_where_match_rules(TabDef, MatchRules),
    [Column | _] = tabdef_column_names(TabDef, KeyAttr),
    Q = ["SELECT count(", Column, ") FROM ", atom_to_list(Tab),
	 WherePart],
    [{Count}] = odbc_query(Host, Q),
    list_to_integer(Count).


count_records(#tabdef{attributes = [KeyAttr | _],
		      name = Tab} = TabDef, MatchRules) ->
    WherePart = prepare_where_match_rules(TabDef, MatchRules),
    [Column | _] = tabdef_column_names(TabDef, KeyAttr),
    Q = ["SELECT count(", Column, ") FROM ", atom_to_list(Tab),
	 WherePart],
    [{Count}] = odbc_query_t(Q),
    list_to_integer(Count).


dirty_write(#tabdef{table_type = TableType} = TabDef, Rec) ->
    case TableType of
	bag ->
	    F = fun() ->
			delete_object(TabDef, Rec),
			insert(TabDef, Rec)
		end;
	set ->
	    Key = element(2, Rec),
	    F = fun() ->
			delete(TabDef, Key),
			insert(TabDef, Rec)
		end
    end,
    case transaction(TabDef, F) of
	{atomic, ok} -> ok;
	{aborted, Reason} -> exit(Reason)
    end.

write(#tabdef{table_type = TableType} = TabDef, Rec, _LockKind) ->
    case TableType of
	bag ->
	    delete_object(TabDef, Rec);
	set ->
	    Key = element(2, Rec),
	    delete(TabDef, Key)
    end,
    insert(TabDef, Rec).


dirty_delete(#tabdef{host = Host} = TabDef, Key) ->
    Q = prepare_delete_command(TabDef, Key),
    case odbc_command(Host, Q) of
	ok -> ok;
	{error, Reason} -> exit(Reason)
    end.

delete(TabDef, Key, _LockKind) ->
    delete(TabDef, Key).

delete(TabDef, Key) ->
    Q = prepare_delete_command(TabDef, Key),
    case odbc_command_t(Q) of
	ok -> ok;
	{error, Reason} -> exit(Reason)
    end.

prepare_delete_command(#tabdef{name = Tab} = TabDef,
		       Key) ->
    ["DELETE FROM ", atom_to_list(Tab),
     " WHERE ", prepare_where_rule(TabDef, Key)].

%% TODO: branch to delete if table_type == set (less overhead)
dirty_delete_object(#tabdef{host = Host} = TabDef, Rec) ->
    Q = prepare_delete_object_command(TabDef, Rec),
    case odbc_command(Host, Q) of
	ok -> ok;
	{error, Reason} -> exit(Reason)
    end.

delete_object(TabDef, Rec, _LockKind) ->
    delete_object(TabDef, Rec).

delete_object(TabDef, Rec) ->
    Q = prepare_delete_object_command(TabDef, Rec),
    case odbc_command_t(Q) of
	ok -> ok;
	{error, Reason} -> exit(Reason)
    end.

prepare_delete_object_command(#tabdef{name = Tab,
				      attributes = Attributes} = TabDef,
			      Rec) ->
    [_ | Values] = tuple_to_list(Rec),
    W = lists:zipwith(
	  fun(Attribute, Value) ->
		  prepare_where_rule(TabDef, Attribute, Value)
	  end, Attributes, Values),
    io_lib:format("DELETE FROM ~s WHERE ~s",
		  [Tab, string:join(W, " AND ")]).


delete_where(#tabdef{name = Tab} = TabDef, MatchRules) ->
    WherePart = prepare_where_match_rules(TabDef, MatchRules),
    Q = io_lib:format("DELETE FROM ~s ~s",
		      [Tab, WherePart]),
    case odbc_command_t(Q) of
	ok -> ok;
	{error, Reason} -> exit(Reason)
    end.

dirty_delete_where(#tabdef{host = Host,name = Tab} = TabDef, MatchRules) ->
    WherePart = prepare_where_match_rules(TabDef, MatchRules),
    Q = io_lib:format("DELETE FROM ~s ~s",
		      [Tab, WherePart]),
    case odbc_command(Host, Q) of
	ok -> ok;
	{error, Reason} -> exit(Reason)
    end.


insert(TabDef, Rec) ->
    [_ | Values] = tuple_to_list(Rec),
    Q = prepare_insert_command(TabDef, Values),
    case odbc_command_t(Q) of
	ok -> ok;
	{error, Reason} -> exit(Reason)
    end.

prepare_insert_command(#tabdef{name = Tab,
			       columns = Columns,
			       attributes = Attributes,
			       types = Types},
		       Values) ->
    {V, []} =
	lists:foldl(
	  fun(Attribute, {V, [Value | Values1]}) ->
		  case lists:keysearch(Attribute, 1, Types) of
		      {value, {_, Type}} when is_tuple(Type) ->
			  ?DEBUG("Type for ~p: ~p = ~p~n",[Attribute, Type, Value]),
			  ValueL = tuple_to_list(Value),
			  if
			      length(ValueL) == size(Type) ->
				  ok;
			      true ->
				  exit(tuple_malformed)
			  end,
			  F = lists:reverse(
				lists:map(
				  fun format/1,
				  ValueL)),
			  {F ++ V, Values1};
		      _ ->
			  {[format(Value) | V], Values1}
		  end
	  end, {[], Values}, Attributes),
    ["INSERT INTO ", atom_to_list(Tab),
     " (", Columns, ") VALUES (",
     string:join(lists:reverse(V), ","), $)].


transaction(#tabdef{host = Host}, Fun) ->
    %% ejabberd_odbc already returns mnesia-style tuples
    ejabberd_odbc:sql_transaction(Host, Fun).

%% Mnesia has async_dirty, maybe ODBC has something similar: 
%% "Call the Fun in a context which is not protected by a transaction."
async_dirty(Tab, Fun) ->
    transaction(Tab, Fun).

%% Mnesia has sync_dirty, maybe ODBC has something similar: 
%% "Call the Fun in a context which is not protected by a transaction."
%% "The difference [with async_dirty] is that the operations are performed
%%  synchronously. The caller waits for the updates to be performed on all
%%  active replicas before the Fun returns."
sync_dirty(Tab, Fun) ->
    transaction(Tab, Fun).

tabdef_column_names(TabDef, Attribute) when is_atom(Attribute) ->
    tabdef_column_names(TabDef, atom_to_list(Attribute));
tabdef_column_names(#tabdef{column_names = ColumnNames}, Attribute) ->
    {value, {_, AttributeColumnNames}} = lists:keysearch(Attribute, 1, ColumnNames),
    AttributeColumnNames.


format(I) when is_integer(I) ->
    %% escaping not needed
    integer_to_list(I);


format(undefined) ->
    "NULL";

format(A) when is_atom(A) ->
    %% escaping usually not needed, watch atom() usage
    "'" ++ atom_to_list(A) ++ "'";

format(P) when is_pid(P) ->
    %% escaping not needed
    "'" ++ pid_to_list(P) ++ "'";

format({jid, _, _, _, _} = JID) ->
    format(exmpp_jid:to_list(JID));

format({N, D, R}) when (R==undefined) or (not is_atom(R)) ->
    format(exmpp_jid:to_list(N, D, R));

format(Xmlel) when is_record(Xmlel, xmlel) ->
    format(exmpp_xml:document_to_list(Xmlel));

format(B) when is_binary(B) ->
    format(binary_to_list(B));

format(S) when is_list(S) ->
    "'" ++ lists:flatten(lists:map(fun odbc_queries:escape/1, S)) ++ "'".


odbc_command(Host, Q) ->
    case ejabberd_odbc:sql_query(Host, Q) of
	{error, Reason} ->
	    {error, Reason};
	{updated, _} ->
	    ok
    end.

odbc_command_t(Q) ->
    case ejabberd_odbc:sql_query_t(Q) of
	{error, Reason} ->
	    {error, Reason};
	{updated, _} ->
	    ok
    end.


odbc_query(Host, Q) ->
    case ejabberd_odbc:sql_query(Host, Q) of
	{selected, _Cols, Res} ->
	    Res;
	{error, Reason} ->
	    exit(Reason)
    end.

odbc_query_t(Q) ->
    case ejabberd_odbc:sql_query_t(Q) of
	{selected, _Cols, Res} ->
	    Res;
	{error, Reason} ->
	    exit(Reason)
    end.


fold_decrementing(_, Arg, N) when N =< 0 ->
    Arg;
fold_decrementing(Fun, Arg, N) ->
    Arg2 = Fun(N, Arg),
    fold_decrementing(Fun, Arg2, N - 1).
