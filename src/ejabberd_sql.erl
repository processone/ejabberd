%%%----------------------------------------------------------------------
%%% File    : ejabberd_sql.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve SQL connection
%%% Created :  8 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_sql).

-author('alexey@process-one.net').

-behaviour(p1_fsm).

%% External exports
-export([start_link/2,
	 sql_query/2,
	 sql_query_t/1,
	 sql_transaction/2,
	 sql_bloc/2,
	 abort/1,
	 restart/1,
	 use_new_schema/0,
	 sql_query_to_iolist/1,
	 sql_query_to_iolist/2,
	 escape/1,
	 standard_escape/1,
	 escape_like/1,
	 escape_like_arg/1,
	 escape_like_arg_circumflex/1,
         to_string_literal/2,
         to_string_literal_t/1,
	 to_bool/1,
	 sqlite_db/1,
	 sqlite_file/1,
	 encode_term/1,
	 decode_term/1,
	 odbcinst_config/0,
	 init_mssql/1,
	 keep_alive/2,
	 to_list/2,
	 to_array/2,
         parse_mysql_version/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, print_state/1,
	 code_change/4]).

-export([connecting/2, connecting/3,
	 session_established/2, session_established/3]).

-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("ejabberd_stacktrace.hrl").

-record(state,
	{db_ref               :: undefined | pid(),
	 db_type = odbc       :: pgsql | mysql | sqlite | odbc | mssql,
	 db_version           :: undefined | non_neg_integer() | {non_neg_integer(), atom(), non_neg_integer()},
	 reconnect_count = 0  :: non_neg_integer(),
	 host                 :: binary(),
	 pending_requests     :: p1_queue:queue(),
	 overload_reported    :: undefined | integer()}).

-define(STATE_KEY, ejabberd_sql_state).
-define(NESTING_KEY, ejabberd_sql_nesting_level).
-define(TOP_LEVEL_TXN, 0).
-define(MAX_TRANSACTION_RESTARTS, 10).
-define(KEEPALIVE_QUERY, [<<"SELECT 1;">>]).
-define(PREPARE_KEY, ejabberd_sql_prepare).
%%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-type state() :: #state{}.
-type sql_query_simple(T) :: [sql_query(T) | binary()] | binary() |
                             #sql_query{} |
                             fun(() -> T) | fun((atom(), _) -> T).
-type sql_query(T) :: sql_query_simple(T) |
                      [{atom() | {atom(), any()}, sql_query_simple(T)}].
-type sql_query_result(T) :: {updated, non_neg_integer()} |
                             {error, binary() | atom()} |
                             {selected, [binary()], [[binary()]]} |
                             {selected, [any()]} |
                             T.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start_link(binary(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Host, I) ->
    Proc = binary_to_atom(get_worker_name(Host, I), utf8),
    p1_fsm:start_link({local, Proc}, ?MODULE, [Host],
		      fsm_limit_opts() ++ ?FSMOPTS).

-spec sql_query(binary(), sql_query(T)) -> sql_query_result(T).
sql_query(Host, Query) ->
    sql_call(Host, {sql_query, Query}).

%% SQL transaction based on a list of queries
%% This function automatically
-spec sql_transaction(binary(), [sql_query(T)] | fun(() -> T)) ->
                             {atomic, T} |
                             {aborted, any()}.
sql_transaction(Host, Queries)
    when is_list(Queries) ->
    F = fun () ->
		lists:foreach(fun (Query) -> sql_query_t(Query) end,
			      Queries)
	end,
    sql_transaction(Host, F);
%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction(Host, F) when is_function(F) ->
    case sql_call(Host, {sql_transaction, F}) of
	{atomic, _} = Ret -> Ret;
	{aborted, _} = Ret -> Ret;
	Err -> {aborted, Err}
    end.

%% SQL bloc, based on a erlang anonymous function (F = fun)
sql_bloc(Host, F) -> sql_call(Host, {sql_bloc, F}).

sql_call(Host, Msg) ->
    Timeout = query_timeout(Host),
    case get(?STATE_KEY) of
	undefined ->
	    sync_send_event(Host,
			    {sql_cmd, Msg, current_time() + Timeout},
			    Timeout);
	_State ->
	    nested_op(Msg)
    end.

keep_alive(Host, Proc) ->
    Timeout = query_timeout(Host),
    case sync_send_event(
	   Proc,
	   {sql_cmd, {sql_query, ?KEEPALIVE_QUERY}, current_time() + Timeout},
	   Timeout) of
	{selected,_,[[<<"1">>]]} ->
	    ok;
	_Err ->
	    ?ERROR_MSG("Keep alive query failed, closing connection: ~p", [_Err]),
	    sync_send_event(Proc, force_timeout, Timeout)
    end.

sync_send_event(Host, Msg, Timeout) when is_binary(Host) ->
    case ejabberd_sql_sup:start(Host) of
	ok ->
	    Proc = get_worker(Host),
	    sync_send_event(Proc, Msg, Timeout);
	{error, _} = Err ->
	    Err
    end;
sync_send_event(Proc, Msg, Timeout) ->
    try p1_fsm:sync_send_event(Proc, Msg, Timeout)
    catch _:{Reason, {p1_fsm, _, _}} ->
	    {error, Reason}
    end.

-spec sql_query_t(sql_query(T)) -> sql_query_result(T).
%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    QRes = sql_query_internal(Query),
    case QRes of
      {error, Reason} -> restart(Reason);
      Rs when is_list(Rs) ->
	  case lists:keysearch(error, 1, Rs) of
	    {value, {error, Reason}} -> restart(Reason);
	    _ -> QRes
	  end;
      _ -> QRes
    end.

abort(Reason) ->
    exit(Reason).

restart(Reason) ->
    throw({aborted, Reason}).

-spec escape_char(char()) -> binary().
escape_char($\000) -> <<"\\0">>;
escape_char($\n) -> <<"\\n">>;
escape_char($\t) -> <<"\\t">>;
escape_char($\b) -> <<"\\b">>;
escape_char($\r) -> <<"\\r">>;
escape_char($') -> <<"''">>;
escape_char($") -> <<"\\\"">>;
escape_char($\\) -> <<"\\\\">>;
escape_char(C) -> <<C>>.

-spec escape(binary()) -> binary().
escape(S) ->
	<<  <<(escape_char(Char))/binary>> || <<Char>> <= S >>.

%% Escape character that will confuse an SQL engine
%% Percent and underscore only need to be escaped for pattern matching like
%% statement
escape_like(S) when is_binary(S) ->
    << <<(escape_like(C))/binary>> || <<C>> <= S >>;
escape_like($%) -> <<"\\%">>;
escape_like($_) -> <<"\\_">>;
escape_like($\\) -> <<"\\\\\\\\">>;
escape_like(C) when is_integer(C), C >= 0, C =< 255 -> escape_char(C).

escape_like_arg(S) when is_binary(S) ->
    << <<(escape_like_arg(C))/binary>> || <<C>> <= S >>;
escape_like_arg($%) -> <<"\\%">>;
escape_like_arg($_) -> <<"\\_">>;
escape_like_arg($\\) -> <<"\\\\">>;
escape_like_arg($[) -> <<"\\[">>;     % For MSSQL
escape_like_arg($]) -> <<"\\]">>;
escape_like_arg(C) when is_integer(C), C >= 0, C =< 255 -> <<C>>.

escape_like_arg_circumflex(S) when is_binary(S) ->
    << <<(escape_like_arg_circumflex(C))/binary>> || <<C>> <= S >>;
escape_like_arg_circumflex($%) -> <<"^%">>;
escape_like_arg_circumflex($_) -> <<"^_">>;
escape_like_arg_circumflex($^) -> <<"^^">>;
escape_like_arg_circumflex($[) -> <<"^[">>;     % For MSSQL
escape_like_arg_circumflex($]) -> <<"^]">>;
escape_like_arg_circumflex(C) when is_integer(C), C >= 0, C =< 255 -> <<C>>.

to_bool(<<"t">>) -> true;
to_bool(<<"true">>) -> true;
to_bool(<<"1">>) -> true;
to_bool(true) -> true;
to_bool(1) -> true;
to_bool(_) -> false.

to_list(EscapeFun, Val) ->
    Escaped = lists:join(<<",">>, lists:map(EscapeFun, Val)),
    [<<"(">>, Escaped, <<")">>].

to_array(EscapeFun, Val) ->
    Escaped = lists:join(<<",">>, lists:map(EscapeFun, Val)),
    lists:flatten([<<"{">>, Escaped, <<"}">>]).

to_string_literal(odbc, S) ->
    <<"'", (escape(S))/binary, "'">>;
to_string_literal(mysql, S) ->
    <<"'", (escape(S))/binary, "'">>;
to_string_literal(mssql, S) ->
    <<"'", (standard_escape(S))/binary, "'">>;
to_string_literal(sqlite, S) ->
    <<"'", (standard_escape(S))/binary, "'">>;
to_string_literal(pgsql, S) ->
    <<"E'", (escape(S))/binary, "'">>.

to_string_literal_t(S) ->
    State = get(?STATE_KEY),
    to_string_literal(State#state.db_type, S).

encode_term(Term) ->
    escape(list_to_binary(
             erl_prettypr:format(erl_syntax:abstract(Term),
                                 [{paper, 65535}, {ribbon, 65535}]))).

decode_term(Bin) ->
    Str = binary_to_list(<<Bin/binary, ".">>),
    try
	{ok, Tokens, _} = erl_scan:string(Str),
	{ok, Term} = erl_parse:parse_term(Tokens),
	Term
    catch _:{badmatch, {error, {Line, Mod, Reason}, _}} ->
	    ?ERROR_MSG("Corrupted Erlang term in SQL database:~n"
		       "** Scanner error: at line ~B: ~ts~n"
		       "** Term: ~ts",
		       [Line, Mod:format_error(Reason), Bin]),
	    erlang:error(badarg);
	  _:{badmatch, {error, {Line, Mod, Reason}}} ->
	    ?ERROR_MSG("Corrupted Erlang term in SQL database:~n"
		       "** Parser error: at line ~B: ~ts~n"
		       "** Term: ~ts",
		       [Line, Mod:format_error(Reason), Bin]),
	    erlang:error(badarg)
    end.

-spec sqlite_db(binary()) -> atom().
sqlite_db(Host) ->
    list_to_atom("ejabberd_sqlite_" ++ binary_to_list(Host)).

-spec sqlite_file(binary()) -> string().
sqlite_file(Host) ->
    case ejabberd_option:sql_database(Host) of
	undefined ->
	    Path = ["sqlite", atom_to_list(node()),
		    binary_to_list(Host), "ejabberd.db"],
	    case file:get_cwd() of
		{ok, Cwd} ->
		    filename:join([Cwd|Path]);
		{error, Reason} ->
		    ?ERROR_MSG("Failed to get current directory: ~ts",
			       [file:format_error(Reason)]),
		    filename:join(Path)
	    end;
	File ->
	    binary_to_list(File)
    end.

use_new_schema() ->
    ejabberd_option:new_sql_schema().

-spec get_worker(binary()) -> atom().
get_worker(Host) ->
    PoolSize = ejabberd_option:sql_pool_size(Host),
    I = p1_rand:round_robin(PoolSize) + 1,
    binary_to_existing_atom(get_worker_name(Host, I), utf8).

-spec get_worker_name(binary(), pos_integer()) -> binary().
get_worker_name(Host, I) ->
    <<"ejabberd_sql_", Host/binary, $_, (integer_to_binary(I))/binary>>.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------
init([Host]) ->
    process_flag(trap_exit, true),
    case ejabberd_option:sql_keepalive_interval(Host) of
        undefined ->
            ok;
        KeepaliveInterval ->
            timer:apply_interval(KeepaliveInterval, ?MODULE,
                                 keep_alive, [Host, self()])
    end,
    [DBType | _] = db_opts(Host),
    p1_fsm:send_event(self(), connect),
    QueueType = ejabberd_option:sql_queue_type(Host),
    {ok, connecting,
     #state{db_type = DBType, host = Host,
	    pending_requests = p1_queue:new(QueueType, max_fsm_queue())}}.

connecting(connect, #state{host = Host} = State) ->
    ConnectRes = case db_opts(Host) of
		     [mysql | Args] -> apply(fun mysql_connect/8, Args);
		     [pgsql | Args] -> apply(fun pgsql_connect/8, Args);
		     [sqlite | Args] -> apply(fun sqlite_connect/1, Args);
		     [mssql | Args] -> apply(fun odbc_connect/2, Args);
		     [odbc | Args] -> apply(fun odbc_connect/2, Args)
		 end,
    case ConnectRes of
        {ok, Ref} ->
	    try link(Ref) of
		_ ->
		    lists:foreach(
		      fun({{?PREPARE_KEY, _} = Key, _}) ->
			      erase(Key);
			 (_) ->
			      ok
		      end, get()),
		    PendingRequests =
			p1_queue:dropwhile(
			  fun(Req) ->
				  p1_fsm:send_event(self(), Req),
				  true
			  end, State#state.pending_requests),
		    State1 = State#state{db_ref = Ref,
					 pending_requests = PendingRequests},
		    State2 = get_db_version(State1),
		    {next_state, session_established, State2#state{reconnect_count = 0}}
	    catch _:Reason ->
		    handle_reconnect(Reason, State)
	    end;
	{error, Reason} ->
	    handle_reconnect(Reason, State)
    end;
connecting(Event, State) ->
    ?WARNING_MSG("Unexpected event in 'connecting': ~p",
		 [Event]),
    {next_state, connecting, State}.

connecting({sql_cmd, {sql_query, ?KEEPALIVE_QUERY}, Timestamp},
	   From, State) ->
    reply(From, {error, <<"SQL connection failed">>}, Timestamp),
    {next_state, connecting, State};
connecting({sql_cmd, Command, Timestamp} = Req, From,
	   State) ->
    ?DEBUG("Queuing pending request while connecting:~n\t~p",
	   [Req]),
    PendingRequests =
	try p1_queue:in({sql_cmd, Command, From, Timestamp},
			State#state.pending_requests)
	catch error:full ->
		Err = <<"SQL request queue is overfilled">>,
		?ERROR_MSG("~ts, bouncing all pending requests", [Err]),
		Q = p1_queue:dropwhile(
		      fun({sql_cmd, _, To, TS}) ->
			      reply(To, {error, Err}, TS),
			      true
		      end, State#state.pending_requests),
		p1_queue:in({sql_cmd, Command, From, Timestamp}, Q)
	end,
    {next_state, connecting,
     State#state{pending_requests = PendingRequests}};
connecting(Request, {Who, _Ref}, State) ->
    ?WARNING_MSG("Unexpected call ~p from ~p in 'connecting'",
		 [Request, Who]),
    {next_state, connecting, State}.

session_established({sql_cmd, Command, Timestamp}, From,
		    State) ->
    run_sql_cmd(Command, From, State, Timestamp);
session_established(Request, {Who, _Ref}, State) ->
    ?WARNING_MSG("Unexpected call ~p from ~p in 'session_established'",
		 [Request, Who]),
    {next_state, session_established, State}.

session_established({sql_cmd, Command, From, Timestamp},
		    State) ->
    run_sql_cmd(Command, From, State, Timestamp);
session_established(force_timeout, State) ->
    {stop, timeout, State};
session_established(Event, State) ->
    ?WARNING_MSG("Unexpected event in 'session_established': ~p",
		 [Event]),
    {next_state, session_established, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_info({'EXIT', _Pid, _Reason}, connecting, State) ->
    {next_state, connecting, State};
handle_info({'EXIT', _Pid, Reason}, _StateName, State) ->
    handle_reconnect(Reason, State);
handle_info(Info, StateName, State) ->
    ?WARNING_MSG("Unexpected info in ~p: ~p",
		 [StateName, Info]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    case State#state.db_type of
        mysql -> catch p1_mysql_conn:stop(State#state.db_ref);
        sqlite -> catch sqlite3:close(sqlite_db(State#state.host));
        _ -> ok
    end,
    ok.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) -> State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
handle_reconnect(Reason, #state{host = Host, reconnect_count = RC} = State) ->
    StartInterval0 = ejabberd_option:sql_start_interval(Host),
    StartInterval = case RC of
			0 -> erlang:min(5000, StartInterval0);
			_ -> StartInterval0
		    end,
    ?WARNING_MSG("~p connection failed:~n"
		 "** Reason: ~p~n"
		 "** Retry after: ~B seconds",
		 [State#state.db_type, Reason,
		  StartInterval div 1000]),
    case State#state.db_type of
	mysql -> catch p1_mysql_conn:stop(State#state.db_ref);
	sqlite -> catch sqlite3:close(sqlite_db(State#state.host));
	pgsql -> catch pgsql:terminate(State#state.db_ref);
	_ -> ok
    end,
    p1_fsm:send_event_after(StartInterval, connect),
    {next_state, connecting, State#state{reconnect_count = RC + 1}}.

run_sql_cmd(Command, From, State, Timestamp) ->
    case current_time() >= Timestamp of
	true ->
	    State1 = report_overload(State),
	    {next_state, session_established, State1};
	false ->
	    receive
		{'EXIT', _Pid, Reason} ->
		    PR = p1_queue:in({sql_cmd, Command, From, Timestamp},
				     State#state.pending_requests),
		    handle_reconnect(Reason, State#state{pending_requests = PR})
	    after 0 ->
		put(?NESTING_KEY, ?TOP_LEVEL_TXN),
		put(?STATE_KEY, State),
		abort_on_driver_error(outer_op(Command), From, Timestamp)
	    end
    end.

%% @doc Only called by handle_call, only handles top level operations.
-spec outer_op(Op::{atom(), binary()}) ->
    {error, Reason::binary()} | {aborted, Reason::binary()} | {atomic, Result::any()}.
outer_op({sql_query, Query}) ->
    sql_query_internal(Query);
outer_op({sql_transaction, F}) ->
    outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, <<"">>);
outer_op({sql_bloc, F}) -> execute_bloc(F).

%% Called via sql_query/transaction/bloc from client code when inside a
%% nested operation
nested_op({sql_query, Query}) ->
    sql_query_internal(Query);
nested_op({sql_transaction, F}) ->
    NestingLevel = get(?NESTING_KEY),
    if NestingLevel =:= (?TOP_LEVEL_TXN) ->
	   outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, <<"">>);
       true -> inner_transaction(F)
    end;
nested_op({sql_bloc, F}) -> execute_bloc(F).

%% Never retry nested transactions - only outer transactions
inner_transaction(F) ->
    PreviousNestingLevel = get(?NESTING_KEY),
    case get(?NESTING_KEY) of
      ?TOP_LEVEL_TXN ->
	  {backtrace, T} = process_info(self(), backtrace),
	  ?ERROR_MSG("Inner transaction called at outer txn "
		     "level. Trace: ~ts",
		     [T]),
	  erlang:exit(implementation_faulty);
      _N -> ok
    end,
    put(?NESTING_KEY, PreviousNestingLevel + 1),
    Result = (catch F()),
    put(?NESTING_KEY, PreviousNestingLevel),
    case Result of
      {aborted, Reason} -> {aborted, Reason};
      {'EXIT', Reason} -> {'EXIT', Reason};
      {atomic, Res} -> {atomic, Res};
      Res -> {atomic, Res}
    end.

outer_transaction(F, NRestarts, _Reason) ->
    PreviousNestingLevel = get(?NESTING_KEY),
    case get(?NESTING_KEY) of
      ?TOP_LEVEL_TXN -> ok;
      _N ->
	  {backtrace, T} = process_info(self(), backtrace),
	  ?ERROR_MSG("Outer transaction called at inner txn "
		     "level. Trace: ~ts",
		     [T]),
	  erlang:exit(implementation_faulty)
    end,
    case sql_begin() of
	{error, Reason} ->
	    maybe_restart_transaction(F, NRestarts, Reason, false);
	_ ->
	    put(?NESTING_KEY, PreviousNestingLevel + 1),
	    try F() of
		Res ->
		    case sql_commit() of
			{error, Reason} ->
			    restart(Reason);
			_ ->
			    {atomic, Res}
		    end
	    catch
		?EX_RULE(throw, {aborted, Reason}, _) when NRestarts > 0 ->
		    maybe_restart_transaction(F, NRestarts, Reason, true);
		?EX_RULE(throw, {aborted, Reason}, Stack) when NRestarts =:= 0 ->
		    StackTrace = ?EX_STACK(Stack),
		    ?ERROR_MSG("SQL transaction restarts exceeded~n** "
			       "Restarts: ~p~n** Last abort reason: "
			       "~p~n** Stacktrace: ~p~n** When State "
			       "== ~p",
			       [?MAX_TRANSACTION_RESTARTS, Reason,
				StackTrace, get(?STATE_KEY)]),
		    maybe_restart_transaction(F, NRestarts, Reason, true);
		?EX_RULE(exit, Reason, _) ->
		    maybe_restart_transaction(F, 0, Reason, true)
	    end
    end.

maybe_restart_transaction(F, NRestarts, Reason, DoRollback) ->
    Res = case driver_restart_required(Reason) of
	      true ->
		  {aborted, Reason};
	      _ when DoRollback ->
		  case sql_rollback() of
		      {error, Reason2} ->
			  case driver_restart_required(Reason2) of
			      true ->
				  {aborted, Reason2};
			      _ ->
				  continue
			  end;
		      _ ->
			  continue
		  end;
	      _ ->
		  continue
    end,
    case Res of
	continue when NRestarts > 0 ->
	    put(?NESTING_KEY, ?TOP_LEVEL_TXN),
	    outer_transaction(F, NRestarts - 1, Reason);
	continue ->
	    {aborted, Reason};
	Other ->
	    Other
    end.

execute_bloc(F) ->
    case catch F() of
      {aborted, Reason} -> {aborted, Reason};
      {'EXIT', Reason} -> {aborted, Reason};
      Res -> {atomic, Res}
    end.

execute_fun(F) when is_function(F, 0) ->
    F();
execute_fun(F) when is_function(F, 2) ->
    State = get(?STATE_KEY),
    F(State#state.db_type, State#state.db_version).

sql_query_internal([{_, _} | _] = Queries) ->
    State = get(?STATE_KEY),
    case select_sql_query(Queries, State) of
        undefined ->
            {error, <<"no matching query for the current DBMS found">>};
        Query ->
            sql_query_internal(Query)
    end;
sql_query_internal(#sql_query{} = Query) ->
    State = get(?STATE_KEY),
    Res =
        try
            case State#state.db_type of
                odbc ->
                    generic_sql_query(Query);
		mssql ->
		    mssql_sql_query(Query);
                pgsql ->
                    Key = {?PREPARE_KEY, Query#sql_query.hash},
                    case get(Key) of
                        undefined ->
                            Host = State#state.host,
                            PreparedStatements =
                                ejabberd_option:sql_prepared_statements(Host),
                            case PreparedStatements of
                                false ->
                                    put(Key, ignore);
                                true ->
                                    case pgsql_prepare(Query, State) of
                                        {ok, _, _, _} ->
                                            put(Key, prepared);
                                        {error, Error} ->
                                            ?ERROR_MSG(
                                               "PREPARE failed for SQL query "
                                               "at ~p: ~p",
                                               [Query#sql_query.loc, Error]),
                                            put(Key, ignore)
                                    end
                            end;
                        _ ->
                            ok
                    end,
                    case get(Key) of
                        prepared ->
                            pgsql_execute_sql_query(Query, State);
                        _ ->
                            pgsql_sql_query(Query)
                    end;
                mysql ->
		    case {Query#sql_query.flags, ejabberd_option:sql_prepared_statements(State#state.host)} of
			{1, _} ->
			    generic_sql_query(Query);
			{_, false} ->
			    generic_sql_query(Query);
			_ ->
			    mysql_prepared_execute(Query, State)
		    end;
                sqlite ->
                    sqlite_sql_query(Query)
            end
        catch exit:{timeout, _} ->
		{error, <<"timed out">>};
	      exit:{killed, _} ->
		{error, <<"killed">>};
	      exit:{normal, _} ->
		{error, <<"terminated unexpectedly">>};
	      exit:{shutdown, _} ->
		{error, <<"shutdown">>};
	      ?EX_RULE(Class, Reason, Stack) ->
		StackTrace = ?EX_STACK(Stack),
                ?ERROR_MSG("Internal error while processing SQL query:~n** ~ts",
			   [misc:format_exception(2, Class, Reason, StackTrace)]),
                {error, <<"internal error">>}
        end,
    check_error(Res, Query);
sql_query_internal(F) when is_function(F) ->
    case catch execute_fun(F) of
        {aborted, Reason} -> {error, Reason};
        {'EXIT', Reason} -> {error, Reason};
        Res -> Res
    end;
sql_query_internal(Query) ->
    State = get(?STATE_KEY),
    ?DEBUG("SQL: \"~ts\"", [Query]),
    QueryTimeout = query_timeout(State#state.host),
    Res = case State#state.db_type of
	    odbc ->
		to_odbc(odbc:sql_query(State#state.db_ref, [Query],
                                       QueryTimeout - 1000));
	    mssql ->
		to_odbc(odbc:sql_query(State#state.db_ref, [Query],
                                       QueryTimeout - 1000));
	    pgsql ->
		pgsql_to_odbc(pgsql:squery(State#state.db_ref, Query,
					   QueryTimeout - 1000));
	    mysql ->
		mysql_to_odbc(p1_mysql_conn:squery(State#state.db_ref,
						   [Query], self(),
						   [{timeout, QueryTimeout - 1000},
						    {result_type, binary}]));
	      sqlite ->
		  Host = State#state.host,
		  sqlite_to_odbc(Host, sqlite3:sql_exec(sqlite_db(Host), Query))
	  end,
    check_error(Res, Query).

select_sql_query(Queries, State) ->
    select_sql_query(
      Queries, State#state.db_type, State#state.db_version, undefined).

select_sql_query([], _Type, _Version, undefined) ->
    undefined;
select_sql_query([], _Type, _Version, Query) ->
    Query;
select_sql_query([{any, Query} | _], _Type, _Version, _) ->
    Query;
select_sql_query([{Type, Query} | _], Type, _Version, _) ->
    Query;
select_sql_query([{{Type, _Version1}, Query1} | Rest], Type, undefined, _) ->
    select_sql_query(Rest, Type, undefined, Query1);
select_sql_query([{{Type, Version1}, Query1} | Rest], Type, Version, Query) ->
    if
        Version >= Version1 ->
            Query1;
        true ->
            select_sql_query(Rest, Type, Version, Query)
    end;
select_sql_query([{_, _} | Rest], Type, Version, Query) ->
    select_sql_query(Rest, Type, Version, Query).

generic_sql_query(SQLQuery) ->
    sql_query_format_res(
      sql_query_internal(generic_sql_query_format(SQLQuery)),
      SQLQuery).

generic_sql_query_format(SQLQuery) ->
    Args = (SQLQuery#sql_query.args)(generic_escape()),
    (SQLQuery#sql_query.format_query)(Args).

generic_escape() ->
    #sql_escape{string = fun(X) -> <<"'", (escape(X))/binary, "'">> end,
		integer = fun(X) -> misc:i2l(X) end,
		boolean = fun(true) -> <<"1">>;
                             (false) -> <<"0">>
                          end,
		in_array_string = fun(X) -> <<"'", (escape(X))/binary, "'">> end,
                like_escape = fun() -> <<"">> end
               }.

pgsql_sql_query(SQLQuery) ->
    sql_query_format_res(
      sql_query_internal(pgsql_sql_query_format(SQLQuery)),
      SQLQuery).

pgsql_sql_query_format(SQLQuery) ->
    Args = (SQLQuery#sql_query.args)(pgsql_escape()),
    (SQLQuery#sql_query.format_query)(Args).

pgsql_escape() ->
    #sql_escape{string = fun(X) -> <<"E'", (escape(X))/binary, "'">> end,
		integer = fun(X) -> misc:i2l(X) end,
		boolean = fun(true) -> <<"'t'">>;
                             (false) -> <<"'f'">>
                          end,
		in_array_string = fun(X) -> <<"E'", (escape(X))/binary, "'">> end,
                like_escape = fun() -> <<"ESCAPE E'\\\\'">> end
               }.

sqlite_sql_query(SQLQuery) ->
    sql_query_format_res(
      sql_query_internal(sqlite_sql_query_format(SQLQuery)),
      SQLQuery).

sqlite_sql_query_format(SQLQuery) ->
    Args = (SQLQuery#sql_query.args)(sqlite_escape()),
    (SQLQuery#sql_query.format_query)(Args).

sqlite_escape() ->
    #sql_escape{string = fun(X) -> <<"'", (standard_escape(X))/binary, "'">> end,
		integer = fun(X) -> misc:i2l(X) end,
		boolean = fun(true) -> <<"1">>;
                             (false) -> <<"0">>
                          end,
		in_array_string = fun(X) -> <<"'", (standard_escape(X))/binary, "'">> end,
                like_escape = fun() -> <<"ESCAPE '\\'">> end
               }.

standard_escape(S) ->
    << <<(case Char of
              $' -> << "''" >>;
              _ -> << Char >>
          end)/binary>> || <<Char>> <= S >>.

mssql_sql_query(SQLQuery) ->
    sqlite_sql_query(SQLQuery).

pgsql_prepare(SQLQuery, State) ->
    Escape = #sql_escape{_ = fun(_) -> arg end,
                         like_escape = fun() -> escape end},
    {RArgs, _} =
        lists:foldl(
	    fun(arg, {Acc, I}) ->
		{[<<$$, (integer_to_binary(I))/binary>> | Acc], I + 1};
	       (escape, {Acc, I}) ->
		   {[<<"ESCAPE E'\\\\'">> | Acc], I};
	       (List, {Acc, I}) when is_list(List) ->
		   {[<<$$, (integer_to_binary(I))/binary>> | Acc], I + 1}
	    end, {[], 1}, (SQLQuery#sql_query.args)(Escape)),
    Args = lists:reverse(RArgs),
    %N = length((SQLQuery#sql_query.args)(Escape)),
    %Args = [<<$$, (integer_to_binary(I))/binary>> || I <- lists:seq(1, N)],
    Query = (SQLQuery#sql_query.format_query)(Args),
    pgsql:prepare(State#state.db_ref, SQLQuery#sql_query.hash, Query).

pgsql_execute_escape() ->
    #sql_escape{string = fun(X) -> X end,
		integer = fun(X) -> [misc:i2l(X)] end,
		boolean = fun(true) -> "1";
                             (false) -> "0"
                          end,
		in_array_string = fun(X) -> <<"\"", (escape(X))/binary, "\"">> end,
                like_escape = fun() -> ignore end
               }.

pgsql_execute_sql_query(SQLQuery, State) ->
    Args = (SQLQuery#sql_query.args)(pgsql_execute_escape()),
    Args2 = lists:filter(fun(ignore) -> false; (_) -> true end, Args),
    ExecuteRes =
        pgsql:execute(State#state.db_ref, SQLQuery#sql_query.hash, Args2),
%    {T, ExecuteRes} =
%        timer:tc(pgsql, execute, [State#state.db_ref, SQLQuery#sql_query.hash, Args]),
%    io:format("T ~ts ~p~n", [SQLQuery#sql_query.hash, T]),
    Res = pgsql_execute_to_odbc(ExecuteRes),
    sql_query_format_res(Res, SQLQuery).

mysql_prepared_execute(#sql_query{hash = Hash} = Query, State) ->
    ValEsc = #sql_escape{like_escape = fun() -> ignore end, _ = fun(X) -> X end},
    TypesEsc = #sql_escape{string = fun(_) -> string end,
			   integer = fun(_) -> integer end,
			   boolean = fun(_) -> bool end,
			   in_array_string = fun(_) -> string end,
			   like_escape = fun() -> ignore end},
    Val = [X || X <- (Query#sql_query.args)(ValEsc), X /= ignore],
    Types = [X || X <- (Query#sql_query.args)(TypesEsc), X /= ignore],
    QueryFn = fun() ->
	PrepEsc = #sql_escape{like_escape = fun() -> <<>> end, _ = fun(_) -> <<"?">> end},
	(Query#sql_query.format_query)((Query#sql_query.args)(PrepEsc))
	end,
    QueryTimeout = query_timeout(State#state.host),
    Res = p1_mysql_conn:prepared_query(State#state.db_ref, QueryFn, Hash, Val, Types,
				       self(), [{timeout, QueryTimeout - 1000}]),
    Res2 = mysql_to_odbc(Res),
    sql_query_format_res(Res2, Query).

sql_query_format_res({selected, _, Rows}, SQLQuery) ->
    Res =
        lists:flatmap(
          fun(Row) ->
                  try
                      [(SQLQuery#sql_query.format_res)(Row)]
                  catch
		      ?EX_RULE(Class, Reason, Stack) ->
			  StackTrace = ?EX_STACK(Stack),
                          ?ERROR_MSG("Error while processing SQL query result:~n"
                                     "** Row: ~p~n** ~ts",
                                     [Row,
				      misc:format_exception(2, Class, Reason, StackTrace)]),
                          []
                  end
          end, Rows),
    {selected, Res};
sql_query_format_res(Res, _SQLQuery) ->
    Res.

sql_query_to_iolist(SQLQuery) ->
    generic_sql_query_format(SQLQuery).

sql_query_to_iolist(sqlite, SQLQuery) ->
    sqlite_sql_query_format(SQLQuery);
sql_query_to_iolist(_DbType, SQLQuery) ->
    generic_sql_query_format(SQLQuery).

sql_begin() ->
    sql_query_internal(
      [{mssql, [<<"begin transaction;">>]},
       {any, [<<"begin;">>]}]).

sql_commit() ->
    sql_query_internal(
      [{mssql, [<<"commit transaction;">>]},
       {any, [<<"commit;">>]}]).

sql_rollback() ->
    sql_query_internal(
      [{mssql, [<<"rollback transaction;">>]},
       {any, [<<"rollback;">>]}]).

driver_restart_required(<<"query timed out">>) -> true;
driver_restart_required(<<"connection closed">>) -> true;
driver_restart_required(<<"Failed sending data on socket", _/binary>>) -> true;
driver_restart_required(<<"SQL connection failed">>) -> true;
driver_restart_required(<<"Communication link failure">>) -> true;
driver_restart_required(_) -> false.

%% Generate the OTP callback return tuple depending on the driver result.
abort_on_driver_error({Tag, Msg} = Reply, From, Timestamp) when Tag == error; Tag == aborted ->
    reply(From, Reply, Timestamp),
    case driver_restart_required(Msg) of
	true ->
	    handle_reconnect(Msg, get(?STATE_KEY));
	_ ->
	    {next_state, session_established, get(?STATE_KEY)}
    end;
abort_on_driver_error(Reply, From, Timestamp) ->
    reply(From, Reply, Timestamp),
    {next_state, session_established, get(?STATE_KEY)}.

-spec report_overload(state()) -> state().
report_overload(#state{overload_reported = PrevTime} = State) ->
    CurrTime = current_time(),
    case PrevTime == undefined orelse (CurrTime - PrevTime) > timer:seconds(30) of
	true ->
	    ?ERROR_MSG("SQL connection pool is overloaded, "
		       "discarding stale requests", []),
	    State#state{overload_reported = current_time()};
	false ->
	    State
    end.

-spec reply({pid(), term()}, term(), integer()) -> term().
reply(From, Reply, Timestamp) ->
    case current_time() >= Timestamp of
	true -> ok;
	false -> p1_fsm:reply(From, Reply)
    end.

%% == pure ODBC code

%% part of init/1
%% Open an ODBC database connection
odbc_connect(SQLServer, Timeout) ->
    ejabberd:start_app(odbc),
    odbc:connect(binary_to_list(SQLServer),
		 [{scrollable_cursors, off},
		  {extended_errors, on},
		  {tuple_row, off},
		  {timeout, Timeout},
		  {binary_strings, on}]).

%% == Native SQLite code

%% part of init/1
%% Open a database connection to SQLite

sqlite_connect(Host) ->
    File = sqlite_file(Host),
    case filelib:ensure_dir(File) of
	ok ->
	    case sqlite3:open(sqlite_db(Host), [{file, File}]) of
		{ok, Ref} ->
		    sqlite3:sql_exec(
		      sqlite_db(Host), "pragma foreign_keys = on"),
		    {ok, Ref};
		{error, {already_started, Ref}} ->
		    {ok, Ref};
		{error, Reason} ->
		    {error, Reason}
	    end;
	Err ->
	    Err
    end.

%% Convert SQLite query result to Erlang ODBC result formalism
sqlite_to_odbc(Host, ok) ->
    {updated, sqlite3:changes(sqlite_db(Host))};
sqlite_to_odbc(Host, {rowid, _}) ->
    {updated, sqlite3:changes(sqlite_db(Host))};
sqlite_to_odbc(_Host, [{columns, Columns}, {rows, TRows}]) ->
    Rows = [lists:map(
	      fun(I) when is_integer(I) ->
		      integer_to_binary(I);
		 (B) ->
		      B
	      end, tuple_to_list(Row)) || Row <- TRows],
    {selected, [list_to_binary(C) || C <- Columns], Rows};
sqlite_to_odbc(_Host, {error, _Code, Reason}) ->
    {error, Reason};
sqlite_to_odbc(_Host, _) ->
    {updated, undefined}.

%% == Native PostgreSQL code

%% part of init/1
%% Open a database connection to PostgreSQL
pgsql_connect(Server, Port, DB, Username, Password, ConnectTimeout,
	      Transport, SSLOpts) ->
    pgsql:connect([{host, Server},
                   {database, DB},
                   {user, Username},
                   {password, Password},
                   {port, Port},
                   {transport, Transport},
                   {connect_timeout, ConnectTimeout},
                   {as_binary, true}|SSLOpts]).

%% Convert PostgreSQL query result to Erlang ODBC result formalism
pgsql_to_odbc({ok, PGSQLResult}) ->
    case PGSQLResult of
      [Item] -> pgsql_item_to_odbc(Item);
      Items -> [pgsql_item_to_odbc(Item) || Item <- Items]
    end.

pgsql_item_to_odbc({<<"SELECT", _/binary>>, Rows,
		    Recs}) ->
    {selected, [element(1, Row) || Row <- Rows], Recs};
pgsql_item_to_odbc({<<"FETCH", _/binary>>, Rows,
		    Recs}) ->
    {selected, [element(1, Row) || Row <- Rows], Recs};
pgsql_item_to_odbc(<<"INSERT ", OIDN/binary>>) ->
    [_OID, N] = str:tokens(OIDN, <<" ">>),
    {updated, binary_to_integer(N)};
pgsql_item_to_odbc(<<"DELETE ", N/binary>>) ->
    {updated, binary_to_integer(N)};
pgsql_item_to_odbc(<<"UPDATE ", N/binary>>) ->
    {updated, binary_to_integer(N)};
pgsql_item_to_odbc({error, Error}) -> {error, Error};
pgsql_item_to_odbc(_) -> {updated, undefined}.

pgsql_execute_to_odbc({ok, {<<"SELECT", _/binary>>, Rows}}) ->
    {selected, [], [[Field || {_, Field} <- Row] || Row <- Rows]};
pgsql_execute_to_odbc({ok, {'INSERT', N}}) ->
    {updated, N};
pgsql_execute_to_odbc({ok, {'DELETE', N}}) ->
    {updated, N};
pgsql_execute_to_odbc({ok, {'UPDATE', N}}) ->
    {updated, N};
pgsql_execute_to_odbc({error, Error}) -> {error, Error};
pgsql_execute_to_odbc(_) -> {updated, undefined}.


%% == Native MySQL code

%% part of init/1
%% Open a database connection to MySQL
mysql_connect(Server, Port, DB, Username, Password, ConnectTimeout, Transport, SSLOpts0) ->
    SSLOpts = case Transport of
		  ssl ->
		      [ssl_required|SSLOpts0];
		  _ ->
		      []
	      end,
    case p1_mysql_conn:start(binary_to_list(Server), Port,
			     binary_to_list(Username),
			     binary_to_list(Password),
			     binary_to_list(DB),
			     ConnectTimeout, fun log/3, SSLOpts)
	of
	{ok, Ref} ->
	    p1_mysql_conn:fetch(
		Ref, [<<"set names 'utf8mb4' collate 'utf8mb4_bin';">>], self()),
	    {ok, Ref};
	Err -> Err
    end.

%% Convert MySQL query result to Erlang ODBC result formalism
mysql_to_odbc({updated, MySQLRes}) ->
    {updated, p1_mysql:get_result_affected_rows(MySQLRes)};
mysql_to_odbc({data, MySQLRes}) ->
    mysql_item_to_odbc(p1_mysql:get_result_field_info(MySQLRes),
		       p1_mysql:get_result_rows(MySQLRes));
mysql_to_odbc({error, MySQLRes})
  when is_binary(MySQLRes) ->
    {error, MySQLRes};
mysql_to_odbc({error, MySQLRes})
  when is_list(MySQLRes) ->
    {error, list_to_binary(MySQLRes)};
mysql_to_odbc({error, MySQLRes}) ->
    mysql_to_odbc({error, p1_mysql:get_result_reason(MySQLRes)});
mysql_to_odbc(ok) ->
    ok.


%% When tabular data is returned, convert it to the ODBC formalism
mysql_item_to_odbc(Columns, Recs) ->
    {selected, [element(2, Column) || Column <- Columns], Recs}.

to_odbc({selected, Columns, Recs}) ->
    Rows = [lists:map(
	      fun(I) when is_integer(I) ->
		      integer_to_binary(I);
		 (B) ->
		      B
	      end, Row) || Row <- Recs],
    {selected, [list_to_binary(C) || C <- Columns], Rows};
to_odbc({error, Reason}) when is_list(Reason) ->
    {error, list_to_binary(Reason)};
to_odbc(Res) ->
    Res.

parse_mysql_version(SVersion, DefaultUpsert) ->
    case re:run(SVersion, <<"(\\d+)\\.(\\d+)(?:\\.(\\d+))?(?:-([^-]*))?">>,
                [{capture, all_but_first, binary}]) of
        {match, [V1, V2, V3, Type]} ->
            V = ((bin_to_int(V1)*1000)+bin_to_int(V2))*1000+bin_to_int(V3),
            TypeA = binary_to_atom(Type, utf8),
            Flags = case TypeA of
                        'MariaDB' -> DefaultUpsert;
                        _ when V >= 5007026 andalso V < 8000000 -> 1;
                        _ when V >= 8000020 -> 1;
                        _ -> DefaultUpsert
                    end,
            {ok, {V, TypeA, Flags}};
        {match, [V1, V2, V3]} ->
            V = ((bin_to_int(V1)*1000)+bin_to_int(V2))*1000+bin_to_int(V3),
            Flags = case V of
                        _ when V >= 5007026 andalso V < 8000000 -> 1;
                        _ when V >= 8000020 -> 1;
                        _ -> DefaultUpsert
                    end,
            {ok, {V, unknown, Flags}};
        _ ->
            error
    end.

get_db_version(#state{db_type = pgsql} = State) ->
    case pgsql:squery(State#state.db_ref,
                      <<"select current_setting('server_version_num')">>) of
        {ok, [{_, _, [[SVersion]]}]} ->
            case catch binary_to_integer(SVersion) of
                Version when is_integer(Version) ->
                    State#state{db_version = Version};
                Error ->
                    ?WARNING_MSG("Error getting pgsql version: ~p", [Error]),
                    State
            end;
        Res ->
            ?WARNING_MSG("Error getting pgsql version: ~p", [Res]),
            State
    end;
get_db_version(#state{db_type = mysql, host = Host} = State) ->
    DefaultUpsert = case lists:member(mysql_alternative_upsert, ejabberd_option:sql_flags(Host)) of
			true -> 1;
			_ -> 0
		    end,
    case mysql_to_odbc(p1_mysql_conn:squery(State#state.db_ref,
					    [<<"select version();">>], self(),
					    [{timeout, 5000},
					     {result_type, binary}])) of
	{selected, _, [SVersion]} ->
            case parse_mysql_version(SVersion, DefaultUpsert) of
                {ok, V} ->
                    State#state{db_version = V};
                error ->
		    ?WARNING_MSG("Error parsing mysql version: ~p", [SVersion]),
		    State
	    end;
	Res ->
	    ?WARNING_MSG("Error getting mysql version: ~p", [Res]),
	    State
    end;
get_db_version(State) ->
    State.

bin_to_int(<<>>) -> 0;
bin_to_int(V) -> binary_to_integer(V).

log(Level, Format, Args) ->
    case Level of
      debug -> ?DEBUG(Format, Args);
      info -> ?INFO_MSG(Format, Args);
      normal -> ?INFO_MSG(Format, Args);
      error -> ?ERROR_MSG(Format, Args)
    end.

db_opts(Host) ->
    Type = ejabberd_option:sql_type(Host),
    Server = ejabberd_option:sql_server(Host),
    Timeout = ejabberd_option:sql_connect_timeout(Host),
    Transport = case ejabberd_option:sql_ssl(Host) of
		    false -> tcp;
		    true -> ssl
		end,
    warn_if_ssl_unsupported(Transport, Type),
    case Type of
        odbc ->
            [odbc, Server, Timeout];
        sqlite ->
            [sqlite, Host];
        _ ->
            Port = ejabberd_option:sql_port(Host),
            DB = case ejabberd_option:sql_database(Host) of
		     undefined -> <<"ejabberd">>;
		     D -> D
		 end,
            User = ejabberd_option:sql_username(Host),
            Pass = ejabberd_option:sql_password(Host),
	    SSLOpts = get_ssl_opts(Transport, Host),
	    case Type of
		mssql ->
                    case odbc_server_is_connstring(Server) of
                        true ->
                            [mssql, Server, Timeout];
                        false ->
                            Encryption = case Transport of
                                tcp -> <<"">>;
                                ssl -> <<";ENCRYPTION=require;ENCRYPT=yes">>
                            end,
                            [mssql, <<"DRIVER=ODBC;SERVER=", Server/binary, ";DATABASE=", DB/binary,
                                      ";UID=", User/binary, ";PWD=", Pass/binary,
                                      ";PORT=", (integer_to_binary(Port))/binary, Encryption/binary,
                                      ";CLIENT_CHARSET=UTF-8;">>, Timeout]
                    end;
		_ ->
		    [Type, Server, Port, DB, User, Pass, Timeout, Transport, SSLOpts]
	    end
    end.

warn_if_ssl_unsupported(tcp, _) ->
    ok;
warn_if_ssl_unsupported(ssl, pgsql) ->
    ok;
warn_if_ssl_unsupported(ssl, mssql) ->
    ok;
warn_if_ssl_unsupported(ssl, mysql) ->
    ok;
warn_if_ssl_unsupported(ssl, Type) ->
    ?WARNING_MSG("SSL connection is not supported for ~ts", [Type]).

get_ssl_opts(ssl, Host) ->
    Opts1 = case ejabberd_option:sql_ssl_certfile(Host) of
		undefined -> [];
		CertFile -> [{certfile, CertFile}]
	    end,
    Opts2 = case ejabberd_option:sql_ssl_cafile(Host) of
		undefined -> Opts1;
		CAFile -> [{cacertfile, CAFile}|Opts1]
	    end,
    case ejabberd_option:sql_ssl_verify(Host) of
	true ->
	    case lists:keymember(cacertfile, 1, Opts2) of
		true ->
		    [{verify, verify_peer}|Opts2];
		false ->
		    ?WARNING_MSG("SSL verification is enabled for "
				 "SQL connection, but option "
				 "'sql_ssl_cafile' is not set; "
				 "verification will be disabled", []),
		    Opts2
	    end;
	false ->
	    [{verify, verify_none}|Opts2]
    end;
get_ssl_opts(tcp, _) ->
    [].

init_mssql_odbcinst(Host) ->
    Driver = ejabberd_option:sql_odbc_driver(Host),
    ODBCINST = io_lib:fwrite("[ODBC]~n"
			     "Driver = ~s~n", [Driver]),
    ?DEBUG("~ts:~n~ts", [odbcinst_config(), ODBCINST]),
    case filelib:ensure_dir(odbcinst_config()) of
	ok ->
	    try
		ok = write_file_if_new(odbcinst_config(), ODBCINST),
		os:putenv("ODBCSYSINI", tmp_dir()),
		ok
	    catch error:{badmatch, {error, Reason} = Err} ->
		    ?ERROR_MSG("Failed to create temporary files in ~ts: ~ts",
			       [tmp_dir(), file:format_error(Reason)]),
		    Err
	    end;
	{error, Reason} = Err ->
	    ?ERROR_MSG("Failed to create temporary directory ~ts: ~ts",
		       [tmp_dir(), file:format_error(Reason)]),
	    Err
    end.

init_mssql(Host) ->
    Server = ejabberd_option:sql_server(Host),
    case odbc_server_is_connstring(Server) of
        true -> ok;
        false -> init_mssql_odbcinst(Host)
    end.

odbc_server_is_connstring(Server) ->
    case binary:match(Server, <<"=">>) of
        nomatch -> false;
        _ -> true
    end.

write_file_if_new(File, Payload) ->
    case filelib:is_file(File) of
	true -> ok;
	false -> file:write_file(File, Payload)
    end.

tmp_dir() ->
    case os:type() of
	{win32, _} -> filename:join([os:getenv("HOME"), "conf"]);
	_ -> filename:join(["/tmp", "ejabberd"])
    end.

odbcinst_config() ->
    filename:join(tmp_dir(), "odbcinst.ini").

max_fsm_queue() ->
    proplists:get_value(max_queue, fsm_limit_opts(), unlimited).

fsm_limit_opts() ->
    ejabberd_config:fsm_limit_opts([]).

query_timeout(LServer) ->
    ejabberd_option:sql_query_timeout(LServer).

current_time() ->
    erlang:monotonic_time(millisecond).

%% ***IMPORTANT*** This error format requires extended_errors turned on.
extended_error({"08S01", _, Reason}) ->
    % TCP Provider: The specified network name is no longer available
    ?DEBUG("ODBC Link Failure: ~ts", [Reason]),
    <<"Communication link failure">>;
extended_error({"08001", _, Reason}) ->
    % Login timeout expired
    ?DEBUG("ODBC Connect Timeout: ~ts", [Reason]),
    <<"SQL connection failed">>;
extended_error({"IMC01", _, Reason}) ->
    % The connection is broken and recovery is not possible
    ?DEBUG("ODBC Link Failure: ~ts", [Reason]),
    <<"Communication link failure">>;
extended_error({"IMC06", _, Reason}) ->
    % The connection is broken and recovery is not possible
    ?DEBUG("ODBC Link Failure: ~ts", [Reason]),
    <<"Communication link failure">>;
extended_error({Code, _, Reason}) ->
    ?DEBUG("ODBC Error ~ts: ~ts", [Code, Reason]),
    iolist_to_binary(Reason);
extended_error(Error) ->
    Error.

check_error({error, Why} = Err, _Query) when Why == killed ->
    Err;
check_error({error, Why}, #sql_query{} = Query) ->
    Err = extended_error(Why),
    ?ERROR_MSG("SQL query '~ts' at ~p failed: ~p",
               [Query#sql_query.hash, Query#sql_query.loc, Err]),
    {error, Err};
check_error({error, Why}, Query) ->
    Err = extended_error(Why),
    case catch iolist_to_binary(Query) of
        SQuery when is_binary(SQuery) ->
            ?ERROR_MSG("SQL query '~ts' failed: ~p", [SQuery, Err]);
        _ ->
            ?ERROR_MSG("SQL query ~p failed: ~p", [Query, Err])
    end,
    {error, Err};
check_error(Result, _Query) ->
    Result.
