-module(gen_storage).
-author('stephan@spaceboyz.net').

-export([behaviour_info/1]).

-export([all_table_hosts/1,
	 table_info/3,
	 create_table/4, delete_table/2,
	 add_table_copy/4, add_table_index/3,
	 read/2, write/2, delete/2, delete_object/2,
	 read/4, write/4, delete/4, delete_object/4,
	 select/2, select/3, select/4, select/5,
	 count_records/2, count_records/3, delete_where/3,
	 dirty_read/2, dirty_write/2, dirty_delete/2, dirty_delete_object/2,
	 dirty_read/3, dirty_write/3, dirty_delete/3, dirty_delete_object/3,
	 dirty_select/3,
	 dirty_count_records/2, dirty_count_records/3, dirty_delete_where/3,
	 async_dirty/3, sync_dirty/3,
	 transaction/3,
	 write_lock_table/2]).

behaviour_info(callbacks) ->
    [{table_info, 1},
     {prepare_tabdef, 2},
     {create_table, 1},
     {delete_table, 1},
     {add_table_copy, 3},
     {add_table_index, 2},
     {dirty_read, 2},
     {read, 3},
     {dirty_select, 2},
     {select, 3},
     {dirty_count_records, 2},
     {count_records, 2},
     {dirty_write, 2},
     {write, 3},
     {dirty_delete, 2},
     {delete, 3},
     {dirty_delete_object, 2},
     {delete_object, 3},
     {delete_where, 2},
     {dirty_delete_where, 2},
     {async_dirty, 2},
     {sync_dirty, 2},
     {transaction, 2}];
behaviour_info(_) ->
    undefined.

-type storage_host() :: binary().
-type storage_table() :: atom().
-type lock_kind() :: read | write | sticky_write.
-record(table, {host_name :: {storage_host(), storage_table()},
		backend :: atom(),
		def :: any()}).
-record(mnesia_def, {table :: atom(),
		     tabdef :: list()}).

-include("ejabberd.hrl"). % This is used for ERROR_MSG

%% Returns all hosts where the table Tab is defined
-spec all_table_hosts(atom()) ->
			     [storage_host()].
all_table_hosts(Tab) ->
    mnesia:dirty_select(table, [{#table{host_name = '$1',
					_ = '_'},
				 [{'=:=', {element, 2, '$1'}, {const, Tab}}],
				 [{element, 1, '$1'}]}]).

-spec table_info(storage_host, storage_table, atom()) ->
			any().
table_info(Host, Tab, InfoKey) ->
    Info =
	case get_table(Host, Tab) of
	    #table{backend = mnesia,
		   def = #mnesia_def{tabdef = Def}} ->
		[{backend, mnesia} | Def];
	    #table{backend = Backend,
		   def = Def} ->
		Info1 = Backend:table_info(Def),
		BackendName = case Backend of
				  gen_storage_odbc -> odbc
			      end,
		[{backend, BackendName} | Info1]
	end,
    case InfoKey of
	all -> Info;
	_ ->
	    case lists:keysearch(InfoKey, 1, Info) of
		{value, {_, Value}} ->
		    Value;
		false when InfoKey =:= record_name ->
		    Tab
	    end
    end.

%% @spec create_table(backend(), Host::binary(), Name::atom(), options()) -> {atomic, ok} | {aborted, Reason}
%% @type options() = [option()]
%% @type option() = {odbc_host, string()}
%%                | {Table::atom(), [tabdef()]}
%% @type tabdef() = {attributes, AtomList}
%%                | {record_name, atom()}
%%                | {types, attributedef()}
%% @type attributedef() = [{Column::atom(), columndef()}]
%% @type columndef() = text
%%                   | int
%%                   | tuple()

%% With an arbitrary number of columndef()
%% option() is any mnesia option
%% columndef() defaults to text for all unspecified attributes

-spec create_table(atom(), storage_host(), storage_table(), #table{}) ->
			  {atomic, ok}.

create_table(mnesia, Host, Tab, Def) ->
    MDef = filter_mnesia_tabdef(Def),
    define_table(mnesia, Host, Tab, #mnesia_def{table = Tab,
						tabdef = MDef}),
    mnesia:create_table(Tab, MDef);

create_table(odbc, Host, Tab, Def) ->
    ODef = gen_storage_odbc:prepare_tabdef(Tab, Def),
    define_table(gen_storage_odbc, Host, Tab, ODef),
    gen_storage_odbc:create_table(ODef).

-spec define_table(atom(), storage_host(), storage_table(), #table{}) ->
			  ok.
define_table(Backend, Host, Name, Def) ->
    mnesia:create_table(table, [{attributes, record_info(fields, table)}]),
    mnesia:dirty_write(#table{host_name = {Host, Name},
			      backend = Backend,
			      def = Def}).

-spec filter_mnesia_tabdef(#table{}) ->
				  [{atom(), any()}].

filter_mnesia_tabdef(TabDef) ->
    lists:filter(fun filter_mnesia_tabdef_/1, TabDef).

filter_mnesia_tabdef_({access_mode, _}) -> true;
filter_mnesia_tabdef_({attributes, _}) -> true;
filter_mnesia_tabdef_({disc_copies, _}) -> true;
filter_mnesia_tabdef_({disc_only_copies, _}) -> true;
filter_mnesia_tabdef_({index, _}) -> true;
filter_mnesia_tabdef_({load_order, _}) -> true;
filter_mnesia_tabdef_({ram_copies, _}) -> true;
filter_mnesia_tabdef_({record_name, _}) -> true;
filter_mnesia_tabdef_({snmp, _}) -> true;
filter_mnesia_tabdef_({type, _}) -> true;
filter_mnesia_tabdef_({local_content, _}) -> true;
filter_mnesia_tabdef_(_) -> false.


-spec delete_table(storage_host(), storage_table()) ->
			  {atomic, ok}.
delete_table(Host, Tab) ->
    backend_apply(delete_table, Host, Tab).


-spec add_table_copy(storage_host(), storage_table(), node(), atom()) ->
			    {atomic, ok}.
add_table_copy(Host, Tab, Node, Type) ->
    backend_apply(add_table_copy, Host, Tab, [Node, Type]).

-spec add_table_index(storage_host(), storage_table(), atom()) ->
			     {atomic, ok}.
add_table_index(Host, Tab, Attribute) ->
    backend_apply(add_table_index, Host, Tab, [Attribute]).


-spec read(storage_host(), {storage_table(), any()}) ->
		  [tuple()].
read(Host, {Tab, Key}) ->
    backend_apply(read, Host, Tab, [Key, read]).

-spec read(storage_host(), storage_table(), any(), lock_kind()) ->
		  [tuple()].
read(Host, Tab, Key, LockKind) ->
    backend_apply(read, Host, Tab, [Key, LockKind]).


-spec dirty_read(storage_host(), {storage_table(), any()}) ->
			[tuple()].
dirty_read(Host, {Tab, Key}) ->
    backend_apply(dirty_read, Host, Tab, [Key]).

-spec dirty_read(storage_host(), storage_table(), any()) ->
			[tuple()].
dirty_read(Host, Tab, Key) ->
    backend_apply(dirty_read, Host, Tab, [Key]).


%% select/3

-type matchvalue() :: '_'
		      | integer()
		      | string().
%%                    | {matchvalue(), matchrule()}.
-type matchrule() :: {'and', matchrule(), matchrule()}
		     | {'andalso', matchrule(), matchrule()}
		     | {'or', matchrule(), matchrule()}
		     | {'orelse', matchrule(), matchrule()}
		     | {'=', Attribute::atom(), matchvalue()}
		     | {'=/=', Attribute::atom(), matchvalue()}
		     | {like, Attribute::atom(), matchvalue()}.

%% For the like operator the last element (not the tail as in
%% matchspecs) may be '_'.
-spec select(storage_host(), storage_table(), [matchrule()]) ->
		    [record()].
select(Host, Tab, MatchRules) ->
    select(Host, Tab, MatchRules, read).

-spec select(storage_host(), storage_table(), [matchrule()], lock_kind()) ->
		    [record()].
select(Host, Tab, MatchRules, Lock) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia}->
	    MatchSpec = matchrules_to_mnesia_matchspec(Tab, MatchRules),
	    mnesia:select(Tab, MatchSpec, Lock);
	#table{backend = Backend,
	       def = Def}->
	    Backend:select(Def, MatchRules, undefined)
    end.

-spec select(storage_host(), storage_table(), [matchrule()], integer(), lock_kind()) ->
		    {[record()], any()} | '$end_of_table'.
select(Host, Tab, MatchRules, N, Lock) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia} ->
	    MatchSpec = matchrules_to_mnesia_matchspec(Tab, MatchRules),
	    mnesia:select(Tab, MatchSpec, N, Lock);
	#table{backend = Backend,
	       def = Def} ->
	    Backend:select(Def, MatchRules, N)
    end.

-spec select({storage_host(), storage_table()}, any()) ->
		    {[record()], any()} | '$end_of_table'.
select({Host, Tab}, Cont) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia} ->
	    mnesia:select(Cont);
	#table{backend = Backend} ->
	    Backend:select(Cont)
    end.

-spec dirty_select(storage_host(), storage_table(), [matchrule()]) ->
			  [record()].
dirty_select(Host, Tab, MatchRules) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia}->
	    MatchSpec = matchrules_to_mnesia_matchspec(Tab, MatchRules),
	    mnesia:dirty_select(Tab, MatchSpec);
	#table{backend = Backend,
	       def = Def}->
	    Backend:dirty_select(Def, MatchRules)
    end.


matchrules_to_mnesia_matchspec(Tab, MatchRules) ->
    RecordName = mnesia:table_info(Tab, record_name),
    Attributes = mnesia:table_info(Tab, attributes),
    %% Build up {record_name, '$1', '$2', ...}
    MatchHead = list_to_tuple([RecordName |
			       lists:reverse(
				 lists:foldl(
				   fun(_, L) ->
					   A = list_to_atom(
						 [$$ | integer_to_list(
							 length(L) + 1)]),
					   [A | L]
				   end, [], Attributes))]),
    %% Transform conditions
    MatchConditions =
	[matchrules_transform_conditions(Attributes, Rule) ||
	    Rule <- MatchRules],
    %% Always full records
    MatchBody = ['$_'],

    [{MatchHead,
      MatchConditions,
      MatchBody}].


%% TODO: special handling for '=='?
matchrules_transform_conditions(Attributes, {Op, Attribute, Value})
  when Op =:= '='; Op =:= '=='; Op =:= '=:='; Op =:= like;
       Op =:= '<'; Op =:= '>'; Op =:= '>='; Op =:= '=<' ->
    Var = case list_find(Attribute, Attributes) of
	      false -> exit(unknown_attribute);
	      N -> list_to_atom([$$ | integer_to_list(N)])
	  end,
    if
	is_tuple(Value) ->
	    {Expr, _} =
		lists:foldl(
		  fun('_', {R, N}) ->
			  {R, N + 1};
		     (V, {R, N}) ->
			  {[matchrules_transform_column_op(Op, {element, N, Var}, {const, V}) | R],
			   N + 1}
		  end, {[], 1}, tuple_to_list(Value)),
	    case Expr of
		[E] -> E;
		_ -> list_to_tuple(['andalso' | Expr])
	    end;
	true ->
	    matchrules_transform_column_op(Op, Var, Value)
    end;

matchrules_transform_conditions(Attributes, T) when is_tuple(T) ->
    L = tuple_to_list(T),
    L2 = [matchrules_transform_conditions(Attributes, E) || E <- L],
    list_to_tuple(L2).


matchrules_transform_column_op(like, Expression, Pattern) ->
    case lists:foldl(fun('_', {R, E1}) ->
			     {R, E1};
			(P, {R, E1}) ->
			     Comparision = {'=:=', {hd, E1}, {const, P}},
			     {[Comparision | R], {tl, E1}}
		     end,
		     {[], Expression}, Pattern) of
	{[Comparision], _} ->
	    Comparision;
	{Comparisions, _} ->
	    list_to_tuple(['andalso' | lists:reverse(Comparisions)])
    end;

matchrules_transform_column_op(Op, Expression, Pattern)
  when Op =:= '='; Op =:= '=:=' ->
    {'=:=', Expression, Pattern};

matchrules_transform_column_op(Op, Expression, Pattern) ->
    {Op, Expression, Pattern}.


%% Finds the first occurence of an element in a list
list_find(E, L) ->
    list_find(E, L, 1).

list_find(_, [], _) ->
    false;
list_find(E, [E | _], N) ->
    N;
list_find(E, [_ | L], N) ->
    list_find(E, L, N + 1).


-spec dirty_count_records(storage_host(), storage_table()) ->
				 integer().
dirty_count_records(Host, Tab) ->
    dirty_count_records(Host, Tab, []).

-spec dirty_count_records(storage_host(), storage_table(), [matchrule()]) ->
				 integer().
dirty_count_records(Host, Tab, MatchRules) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia}->
	    [{MatchHead, MatchConditions, _}] = matchrules_to_mnesia_matchspec(Tab, MatchRules),
	    MatchSpec = [{MatchHead, MatchConditions, [[]]}],
	    length(mnesia:dirty_select(Tab, MatchSpec));
	#table{backend = Backend,
	       def = Def}->
	    Backend:dirty_count_records(Def, MatchRules)
    end.


-define(COUNT_RECORDS_BATCHSIZE, 100).
-spec count_records(storage_host(), storage_table()) ->
			   integer().
count_records(Host, Tab) ->
    count_records(Host, Tab, []).

-spec count_records(storage_host(), storage_table(), [matchrule()]) ->
			   integer().
count_records(Host, Tab, MatchRules) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia}->
	    [{MatchHead, MatchConditions, _}] = matchrules_to_mnesia_matchspec(Tab, MatchRules),
	    MatchSpec = [{MatchHead, MatchConditions, [[]]}],
	    case mnesia:select(Tab, MatchSpec,
			       ?COUNT_RECORDS_BATCHSIZE, read) of
		{Result, Cont} ->
		    Count = length(Result),
		    mnesia_count_records_cont(Cont, Count);
		'$end_of_table' ->
		    0
	    end;
	#table{backend = Backend,
	       def = Def}->
	    Backend:count_records(Def, MatchRules)
    end.

mnesia_count_records_cont(Cont, Count) ->
    case mnesia:select(Cont) of
	{Result, Cont} ->
	    NewCount = Count + length(Result),
	    mnesia_count_records_cont(Cont, NewCount);
	'$end_of_table' ->
	    Count
    end.


-spec write(storage_host(), tuple()) ->
		   ok.
write(Host, Rec) ->
    Tab = element(1, Rec),
    backend_apply(write, Host, Tab, [Rec, write]).

-spec write(storage_host(), storage_table(), tuple(), lock_kind()) ->
		   ok.
write(Host, Tab, Rec, LockKind) ->
    backend_apply(write, Host, Tab, [Rec, LockKind]).


-spec dirty_write(storage_host(), tuple()) ->
			 ok.
dirty_write(Host, Rec) ->
    Tab = element(1, Rec),
    backend_apply(dirty_write, Host, Tab, [Rec]).

-spec dirty_write(storage_host(), storage_table(), tuple()) ->
			 ok.
dirty_write(Host, Tab, Rec) ->
    backend_apply(dirty_write, Host, Tab, [Rec]).


-spec delete(storage_host(), {storage_table(), any()}) ->
		    ok.
delete(Host, {Tab, Key}) ->
    backend_apply(delete, Host, Tab, [Key, write]).

-spec delete(storage_host(), storage_table(), any(), lock_kind()) ->
		    ok.
delete(Host, Tab, Key, LockKind) ->
    backend_apply(delete, Host, Tab, [Key, LockKind]).


-spec dirty_delete(storage_host(), {storage_table(), any()}) ->
			  ok.
dirty_delete(Host, {Tab, Key}) ->
    backend_apply(dirty_delete, Host, Tab, [Key]).

-spec dirty_delete(storage_host(), storage_table(), any()) ->
			  ok.
dirty_delete(Host, Tab, Key) ->
    backend_apply(dirty_delete, Host, Tab, [Key]).


-spec delete_object(storage_host(), tuple()) ->
			   ok.
delete_object(Host, Rec) ->
    Tab = element(1, Rec),
    backend_apply(delete_object, Host, Tab, [Rec, write]).

-spec delete_object(storage_host(), storage_table(), tuple(), lock_kind()) ->
			   ok.
delete_object(Host, Tab, Rec, LockKind) ->
    backend_apply(delete_object, Host, Tab, [Rec, LockKind]).


-spec dirty_delete_object(storage_host(), tuple()) ->
				 ok.
dirty_delete_object(Host, Rec) ->
    Tab = element(1, Rec),
    backend_apply(delete_object, Host, Tab, [Rec]).

-spec dirty_delete_object(storage_host(), storage_table(), tuple()) ->
				 ok.
dirty_delete_object(Host, Tab, Rec) ->
    backend_apply(delete_object, Host, Tab, [Rec]).


-define(DELETE_WHERE_BATCH_SIZE, 100).

-spec delete_where(storage_host(), storage_table(), [matchrule()]) ->
			  ok.
delete_where(Host, Tab, MatchRules) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia} ->
	    MatchSpec = matchrules_to_mnesia_matchspec(Tab, MatchRules),
	    mnesia:write_lock_table(Tab),
	    SR = mnesia:select(Tab, MatchSpec, ?DELETE_WHERE_BATCH_SIZE, write),
	    delete_where_mnesia1(SR);
	#table{backend = Backend,
	       def = Def} ->
	    Backend:delete_where(Def, MatchRules)
    end.

delete_where_mnesia1('$end_of_table') ->
    ok;
delete_where_mnesia1({Objects, Cont}) ->
    lists:foreach(fun(Object) ->
			  mnesia:delete_object(Object)
		  end, Objects),
    delete_where_mnesia1(mnesia:select(Cont)).


-spec dirty_delete_where(storage_host(), storage_table(), [matchrule()]) ->
				ok.
dirty_delete_where(Host, Tab, MatchRules) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia} ->
	    MatchSpec = matchrules_to_mnesia_matchspec(Tab, MatchRules),
	    F = fun() ->
			mnesia:write_lock_table(Tab),
			Objects = mnesia:select(Tab, MatchSpec, write),
			lists:foreach(fun(Object) ->
					      mnesia:delete_object(Object)
				      end, Objects)
		end,
	    {atomic, _} = mnesia:transaction(F);
	#table{backend = Backend,
	       def = Def} ->
	    Backend:dirty_delete_where(Def, MatchRules)
    end.


-spec write_lock_table(storage_host(), storage_table()) ->
			      ok.
write_lock_table(Host, Tab) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia} ->
	    mnesia:write_lock_table(Tab);
	_ ->
	    ignored
    end.


-spec transaction(storage_host(), storage_table(), fun()) ->
    {atomic, any()}.
%% Warning: all tabs touched by the transaction must use the same
%% storage backend!
transaction(Host, Tab, Fun) ->
    %% This is just to ensure an error is logged when error appears:
    case transaction2(Host, Tab, Fun) of
	{atomic, _} = Good ->
	    Good;
	{aborted, Reason} = Bad ->
	    ?ERROR_MSG("Transaction failed for host ~p in tab ~p with fun ~p:~n~p", [Host, Tab, Fun, Reason]),
	    Bad
    end.
transaction2(Host, Tab, Fun) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia} ->
	    mnesia:transaction(Fun);
	#table{backend = Backend,
	       def = Def} ->
	    Backend:transaction(Def, Fun)
    end.

-spec sync_dirty(storage_host(), storage_table(), fun()) ->
    {atomic, any()}.
%% Warning: all tabs touched by the sync_dirty must use the same
%% storage backend!
sync_dirty(Host, Tab, Fun) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia} ->
	    mnesia:sync_dirty(Fun);
	#table{backend = Backend,
	       def = Def} ->
	    Backend:sync_dirty(Def, Fun)
    end.

-spec async_dirty(storage_host(), storage_table(), fun()) ->
    {atomic, any()}.
%% Warning: all tabs touched by the async_dirty must use the same
%% storage backend!
async_dirty(Host, Tab, Fun) ->
    case get_table(Host, Tab) of
	#table{backend = mnesia} ->
	    mnesia:async_dirty(Fun);
	#table{backend = Backend,
	       def = Def} ->
	    Backend:async_dirty(Def, Fun)
    end.


%% TODO: fix the calling code so this function clause isn't needed
get_table(Host, Tab) when is_list(Host) ->
    get_table(list_to_binary(Host), Tab);
get_table(Host, Tab) ->
    case mnesia:dirty_read(table, {Host, Tab}) of
	[T] ->
	    T;
	_ ->
	    catch throw(error123),
	    Stacktrace = erlang:get_stacktrace(),
	    error_logger:error_msg("gen_storage: Table ~p not found on ~p~nStacktrace: ~p", [Tab, Host, Stacktrace]),
	    exit(table_not_found)
    end.

backend_apply(F, Host, Tab) ->
    backend_apply(F, Host, Tab, []).

backend_apply(F, Host, Tab, A) ->
    #table{backend = Backend,
	   def = Def} = get_table(Host, Tab),
    case Def of
	#mnesia_def{table = Tab} ->
	    apply(Backend, F, [Tab | A]);
	_ ->
	    apply(Backend, F, [Def | A])
    end.

