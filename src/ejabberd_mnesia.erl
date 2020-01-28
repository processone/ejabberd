%%%----------------------------------------------------------------------
%%% File    : mnesia_mnesia.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Handle configurable mnesia schema
%%% Created : 17 Nov 2016 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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

%%% This module should be used everywhere ejabberd creates a mnesia table
%%% to make the schema customizable without code change
%%% Just apply this change in ejabberd modules
%%% s/ejabberd_mnesia:create(?MODULE, /ejabberd_mnesia:create(?MODULE, /

-module(ejabberd_mnesia).
-author('christophe.romain@process-one.net').

-behaviour(gen_server).

-export([start/0, create/3, update/2, transform/2, transform/3,
	 dump_schema/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(STORAGE_TYPES, [disc_copies, disc_only_copies, ram_copies]).
-define(NEED_RESET, [local_content, type]).

-include("logger.hrl").
-include("ejabberd_stacktrace.hrl").

-record(state, {tables = #{} :: tables(),
		schema = [] :: [{atom(), custom_schema()}]}).

-type tables() :: #{atom() => {[{atom(), term()}], term()}}.
-type custom_schema() :: [{ram_copies | disc_copies | disc_only_copies, [node()]} |
			  {local_content, boolean()} |
			  {type, set | ordered_set | bag} |
			  {attributes, [atom()]} |
			  {index, [atom()]}].

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec create(module(), atom(), list()) -> any().
create(Module, Name, TabDef) ->
    gen_server:call(?MODULE, {create, Module, Name, TabDef},
		    %% Huge timeout is need to have enough
		    %% time to transform huge tables
		    timer:minutes(30)).

init([]) ->
    ejabberd_config:env_binary_to_list(mnesia, dir),
    MyNode = node(),
    DbNodes = mnesia:system_info(db_nodes),
    case lists:member(MyNode, DbNodes) of
	true ->
	    case mnesia:system_info(extra_db_nodes) of
		[] -> mnesia:create_schema([node()]);
		_ -> ok
	    end,
	    ejabberd:start_app(mnesia, permanent),
	    ?DEBUG("Waiting for Mnesia tables synchronization...", []),
	    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
	    Schema = read_schema_file(),
	    {ok, #state{schema = Schema}};
	false ->
	    ?CRITICAL_MSG("Node name mismatch: I'm [~ts], "
			  "the database is owned by ~p", [MyNode, DbNodes]),
	    ?CRITICAL_MSG("Either set ERLANG_NODE in ejabberdctl.cfg "
			  "or change node name in Mnesia", []),
	    {stop, node_name_mismatch}
    end.

handle_call({create, Module, Name, TabDef}, _From, State) ->
    case maps:get(Name, State#state.tables, undefined) of
	{TabDef, Result} ->
	    {reply, Result, State};
	_ ->
	    Result = do_create(Module, Name, TabDef, State#state.schema),
	    Tables = maps:put(Name, {TabDef, Result}, State#state.tables),
	    {reply, Result, State#state{tables = Tables}}
    end;
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_create(Module, Name, TabDef, TabDefs) ->
    code:ensure_loaded(Module),
    Schema = schema(Name, TabDef, TabDefs),
    {attributes, Attrs} = lists:keyfind(attributes, 1, Schema),
    case catch mnesia:table_info(Name, attributes) of
	{'EXIT', _} ->
	    create(Name, TabDef);
	Attrs ->
	    case need_reset(Name, Schema) of
		true ->
		    reset(Name, Schema);
		false ->
		    case update(Name, Attrs, Schema) of
			{atomic, ok} ->
			    transform(Module, Name, Attrs, Attrs);
			Err ->
			    Err
		    end
	    end;
	OldAttrs ->
	    transform(Module, Name, OldAttrs, Attrs)
    end.

reset(Name, TabDef) ->
    ?INFO_MSG("Deleting Mnesia table '~ts'", [Name]),
    mnesia_op(delete_table, [Name]),
    create(Name, TabDef).

update(Name, TabDef) ->
    {attributes, Attrs} = lists:keyfind(attributes, 1, TabDef),
    update(Name, Attrs, TabDef).

update(Name, Attrs, TabDef) ->
    case change_table_copy_type(Name, TabDef) of
	{atomic, ok} ->
	    CurrIndexes = [lists:nth(N-1, Attrs) ||
			      N <- mnesia:table_info(Name, index)],
	    NewIndexes = proplists:get_value(index, TabDef, []),
	    case delete_indexes(Name, CurrIndexes -- NewIndexes) of
		{atomic, ok} ->
		    add_indexes(Name, NewIndexes -- CurrIndexes);
		Err ->
		    Err
	    end;
	Err ->
	    Err
    end.

change_table_copy_type(Name, TabDef) ->
    CurrType = mnesia:table_info(Name, storage_type),
    NewType = case lists:filter(fun is_storage_type_option/1, TabDef) of
		  [{Type, _}|_] -> Type;
		  [] -> CurrType
	      end,
    if NewType /= CurrType ->
	    ?INFO_MSG("Changing Mnesia table '~ts' from ~ts to ~ts",
		      [Name, CurrType, NewType]),
	    mnesia_op(change_table_copy_type, [Name, node(), NewType]);
       true ->
	    {atomic, ok}
    end.

delete_indexes(Name, [Index|Indexes]) ->
    ?INFO_MSG("Deleting index '~ts' from Mnesia table '~ts'", [Index, Name]),
    case mnesia_op(del_table_index, [Name, Index]) of
	{atomic, ok} ->
	    delete_indexes(Name, Indexes);
	Err ->
	    Err
    end;
delete_indexes(_Name, []) ->
    {atomic, ok}.

add_indexes(Name, [Index|Indexes]) ->
    ?INFO_MSG("Adding index '~ts' to Mnesia table '~ts'", [Index, Name]),
    case mnesia_op(add_table_index, [Name, Index]) of
	{atomic, ok} ->
	    add_indexes(Name, Indexes);
	Err ->
	    Err
    end;
add_indexes(_Name, []) ->
    {atomic, ok}.

%
% utilities
%

schema(Name, Default, Schema) ->
    case lists:keyfind(Name, 1, Schema) of
	{_, Custom} ->
	    TabDefs = merge(Custom, Default),
	    ?DEBUG("Using custom schema for table '~ts': ~p",
		   [Name, TabDefs]),
	    TabDefs;
	false ->
	    Default
    end.

-spec read_schema_file() -> [{atom(), custom_schema()}].
read_schema_file() ->
    File = schema_path(),
    case fast_yaml:decode_from_file(File, [plain_as_atom]) of
	{ok, Y} ->
	    case econf:validate(validator(), lists:flatten(Y)) of
		{ok, []} ->
		    ?WARNING_MSG("Mnesia schema file ~ts is empty", [File]),
		    [];
		{ok, Config} ->
		    lists:map(
		      fun({Tab, Opts}) ->
			      {Tab, lists:map(
				      fun({storage_type, T}) -> {T, [node()]};
					 (Other) -> Other
				      end, Opts)}
		      end, Config);
		{error, Reason, Ctx} ->
		    ?ERROR_MSG("Failed to read Mnesia schema from ~ts: ~ts",
			       [File, econf:format_error(Reason, Ctx)]),
		    []
	    end;
	{error, enoent} ->
	    ?DEBUG("No custom Mnesia schema file found at ~ts", [File]),
            [];
	{error, Reason} ->
	    ?ERROR_MSG("Failed to read Mnesia schema file ~ts: ~ts",
		       [File, fast_yaml:format_error(Reason)])
    end.

-spec validator() -> econf:validator().
validator() ->
    econf:map(
      econf:atom(),
      econf:options(
	#{storage_type => econf:enum([ram_copies, disc_copies, disc_only_copies]),
	  local_content => econf:bool(),
	  type => econf:enum([set, ordered_set, bag]),
	  attributes => econf:list(econf:atom()),
	  index => econf:list(econf:atom())},
	[{return, orddict}, unique]),
      [unique]).

create(Name, TabDef) ->
    Type = lists:foldl(
	     fun({ram_copies, _}, _) -> " ram ";
		({disc_copies, _}, _) -> " disc ";
		({disc_only_copies, _}, _) -> " disc_only ";
		(_, Acc) -> Acc
	     end, " ", TabDef),
    ?INFO_MSG("Creating Mnesia~tstable '~ts'", [Type, Name]),
    case mnesia_op(create_table, [Name, TabDef]) of
	{atomic, ok} ->
	    add_table_copy(Name);
	Err ->
	    Err
    end.

%% The table MUST exist, otherwise the function would fail
add_table_copy(Name) ->
    Type = mnesia:table_info(Name, storage_type),
    Nodes = mnesia:table_info(Name, Type),
    case lists:member(node(), Nodes) of
	true ->
	    {atomic, ok};
	false ->
	    mnesia_op(add_table_copy, [Name, node(), Type])
    end.

merge(Custom, Default) ->
    NewDefault = case lists:any(fun is_storage_type_option/1, Custom) of
		     true ->
			 lists:filter(
			   fun(O) ->
				   not is_storage_type_option(O)
			   end, Default);
		     false ->
			 Default
		 end,
    lists:ukeymerge(1, Custom, lists:ukeysort(1, NewDefault)).

need_reset(Table, TabDef) ->
    ValuesF = [mnesia:table_info(Table, Key) || Key <- ?NEED_RESET],
    ValuesT = [proplists:get_value(Key, TabDef) || Key <- ?NEED_RESET],
    lists:foldl(
      fun({Val, Val}, Acc) -> Acc;
	 ({_, undefined}, Acc) -> Acc;
	 ({_, _}, _) -> true
      end, false, lists:zip(ValuesF, ValuesT)).

transform(Module, Name) ->
    try mnesia:table_info(Name, attributes) of
	Attrs ->
	    transform(Module, Name, Attrs, Attrs)
    catch _:{aborted, _} = Err ->
	    Err
    end.

transform(Module, Name, NewAttrs) ->
    try mnesia:table_info(Name, attributes) of
	OldAttrs ->
	    transform(Module, Name, OldAttrs, NewAttrs)
    catch _:{aborted, _} = Err ->
	    Err
    end.

transform(Module, Name, Attrs, Attrs) ->
    case need_transform(Module, Name) of
	true ->
	    ?INFO_MSG("Transforming table '~ts', this may take a while", [Name]),
	    transform_table(Module, Name);
	false ->
	    {atomic, ok}
    end;
transform(Module, Name, OldAttrs, NewAttrs) ->
    Fun = case erlang:function_exported(Module, transform, 1) of
	      true -> transform_fun(Module, Name);
	      false -> fun(Old) -> do_transform(OldAttrs, NewAttrs, Old) end
	  end,
    mnesia_op(transform_table, [Name, Fun, NewAttrs]).

-spec need_transform(module(), atom()) -> boolean().
need_transform(Module, Name) ->
    case erlang:function_exported(Module, need_transform, 1) of
	true ->
	    do_need_transform(Module, Name, mnesia:dirty_first(Name));
	false ->
	    false
    end.

do_need_transform(_Module, _Name, '$end_of_table') ->
    false;
do_need_transform(Module, Name, Key) ->
    Objs = mnesia:dirty_read(Name, Key),
    case lists:foldl(
	   fun(_, true) -> true;
	      (Obj, _) -> Module:need_transform(Obj)
	   end, undefined, Objs) of
	true -> true;
	false -> false;
	_ ->
	    do_need_transform(Module, Name, mnesia:dirty_next(Name, Key))
    end.

do_transform(OldAttrs, Attrs, Old) ->
    [Name|OldValues] = tuple_to_list(Old),
    Before = lists:zip(OldAttrs, OldValues),
    After = lists:foldl(
	      fun(Attr, Acc) ->
		      case lists:keyfind(Attr, 1, Before) of
			  false -> [{Attr, undefined}|Acc];
			  Value -> [Value|Acc]
		      end
	      end, [], lists:reverse(Attrs)),
    {Attrs, NewRecord} = lists:unzip(After),
    list_to_tuple([Name|NewRecord]).

transform_fun(Module, Name) ->
    fun(Obj) ->
	    try Module:transform(Obj)
	    catch ?EX_RULE(Class, Reason, St) ->
		    StackTrace = ?EX_STACK(St),
		    ?ERROR_MSG("Failed to transform Mnesia table ~ts:~n"
			       "** Record: ~p~n"
			       "** ~ts",
			       [Name, Obj,
				misc:format_exception(2, Class, Reason, StackTrace)]),
		    erlang:raise(Class, Reason, StackTrace)
	    end
    end.

transform_table(Module, Name) ->
    Type = mnesia:table_info(Name, type),
    Attrs = mnesia:table_info(Name, attributes),
    TmpTab = list_to_atom(atom_to_list(Name) ++ "_backup"),
    StorageType = if Type == ordered_set -> disc_copies;
		     true -> disc_only_copies
		  end,
    mnesia:create_table(TmpTab,
			[{StorageType, [node()]},
			 {type, Type},
			 {local_content, true},
			 {record_name, Name},
			 {attributes, Attrs}]),
    mnesia:clear_table(TmpTab),
    Fun = transform_fun(Module, Name),
    Res = mnesia_op(
	    transaction,
	    [fun() -> do_transform_table(Name, Fun, TmpTab, mnesia:first(Name)) end]),
    mnesia:delete_table(TmpTab),
    Res.

do_transform_table(Name, _Fun, TmpTab, '$end_of_table') ->
    mnesia:foldl(
      fun(Obj, _) ->
	      mnesia:write(Name, Obj, write)
      end, ok, TmpTab);
do_transform_table(Name, Fun, TmpTab, Key) ->
    Next = mnesia:next(Name, Key),
    Objs = mnesia:read(Name, Key),
    lists:foreach(
      fun(Obj) ->
	      mnesia:write(TmpTab, Fun(Obj), write),
	      mnesia:delete_object(Obj)
      end, Objs),
    do_transform_table(Name, Fun, TmpTab, Next).

mnesia_op(Fun, Args) ->
    case apply(mnesia, Fun, Args) of
	{atomic, ok} ->
	    {atomic, ok};
	Other ->
	    ?ERROR_MSG("Failure on mnesia ~ts ~p: ~p",
		      [Fun, Args, Other]),
	    Other
    end.

schema_path() ->
    Dir = case os:getenv("EJABBERD_MNESIA_SCHEMA") of
	      false -> mnesia:system_info(directory);
	      Path -> Path
	  end,
    filename:join(Dir, "ejabberd.schema").

is_storage_type_option({O, _}) ->
    O == ram_copies orelse O == disc_copies orelse O == disc_only_copies.

dump_schema() ->
    File = schema_path(),
    Schema = lists:flatmap(
	       fun(schema) ->
		       [];
		  (Tab) ->
		       [{Tab, [{storage_type,
				mnesia:table_info(Tab, storage_type)},
			       {local_content,
				mnesia:table_info(Tab, local_content)}]}]
	       end, mnesia:system_info(tables)),
    case file:write_file(File, [fast_yaml:encode(Schema), io_lib:nl()]) of
	ok ->
	    io:format("Mnesia schema is written to ~ts~n", [File]);
	{error, Reason} ->
	    io:format("Failed to write Mnesia schema to ~ts: ~ts",
		      [File, file:format_error(Reason)])
    end.
