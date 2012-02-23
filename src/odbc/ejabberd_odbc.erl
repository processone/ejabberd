%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve ODBC connection
%%% Created :  8 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_odbc).
-author('alexey@process-one.net').

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% External exports
-export([start/1, start_link/2,
         running/1,
	 sql_query/2,
	 sql_query_t/1,
	 sql_transaction/2,
	 sql_bloc/2,
	 db_type/1,
	 escape/1,
	 escape_like/1,
	 to_bool/1,
	 keep_alive/1,
	 sql_query_on_all_connections/2]).

%% gen_fsm callbacks
-export([init/1,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 print_state/1,
	 code_change/4]).

%% gen_fsm states
-export([connecting/2,
	 connecting/3,
	 session_established/2,
	 session_established/3]).

-include("ejabberd.hrl").

-record(state, {db_ref,
		db_type,
		start_interval,
		host,
		max_pending_requests_len,
		pending_requests}).

-define(STATE_KEY, ejabberd_odbc_state).
-define(NESTING_KEY, ejabberd_odbc_nesting_level).
-define(TOP_LEVEL_TXN, 0).
-define(MAX_TRANSACTION_RESTARTS, 10).
-define(PGSQL_PORT, 5432).
-define(MYSQL_PORT, 3306).

-define(TRANSACTION_TIMEOUT, 60000). % milliseconds
-define(KEEPALIVE_TIMEOUT, 60000).
-define(KEEPALIVE_QUERY, "SELECT 1;").

%%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    ?GEN_FSM:start(ejabberd_odbc, [Host], fsm_limit_opts() ++ ?FSMOPTS).

start_link(Host, StartInterval) ->
    ?GEN_FSM:start_link(ejabberd_odbc, [Host, StartInterval],
			fsm_limit_opts() ++ ?FSMOPTS).

sql_query(Host, Query) ->
    sql_call(Host, {sql_query, Query}).

%% Issue an SQL query on all the connections
sql_query_on_all_connections(Host, Query) ->
    F = fun(Pid) -> ?GEN_FSM:sync_send_event(Pid, {sql_cmd,
						   {sql_query, Query},
						   erlang:now()}, ?TRANSACTION_TIMEOUT) end,
    lists:map(F, ejabberd_odbc_sup:get_pids(Host)).

%% Predicate returning true if there is an odbc process running for
%% host Host, false otherwise.
running(Host) ->
    case catch ejabberd_odbc_sup:get_random_pid(Host) of
        P when is_pid(P) -> true;
        {'EXIT', {noproc, _}} -> false
    end.

%% SQL transaction based on a list of queries
%% This function automatically
sql_transaction(Host, Queries) when is_list(Queries) ->
    F = fun() ->
		lists:foreach(fun(Query) ->
				      sql_query_t(Query)
			      end,
			      Queries)
	end,
    sql_transaction(Host, F);
%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction(Host, F) when is_function(F) ->
    sql_call(Host, {sql_transaction, F}).

%% SQL bloc, based on a erlang anonymous function (F = fun)
sql_bloc(Host, F) ->
    sql_call(Host, {sql_bloc, F}).

sql_call(Host, Msg) ->
    case get(?STATE_KEY) of
        undefined ->
            ?GEN_FSM:sync_send_event(ejabberd_odbc_sup:get_random_pid(Host),
				     {sql_cmd, Msg, now()}, ?TRANSACTION_TIMEOUT);
        _State ->
            nested_op(Msg)
    end.

% perform a harmless query on all opened connexions to avoid connexion close.
keep_alive(PID) ->
    ?GEN_FSM:sync_send_event(PID, {sql_cmd, {sql_query, ?KEEPALIVE_QUERY}, now()},
			     ?KEEPALIVE_TIMEOUT).

%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    QRes = sql_query_internal(Query),
    case QRes of
	{error, Reason} ->
	    throw({aborted, Reason});
	Rs when is_list(Rs) ->
	    case lists:keysearch(error, 1, Rs) of
		{value, {error, Reason}} ->
		    throw({aborted, Reason});
		_ ->
		    QRes
	    end;
	_ ->
	    QRes
    end.

db_type(Host) ->
    ejabberd_odbc_sup:get_dbtype(Host).

%% Escape character that will confuse an SQL engine
escape(S) when is_list(S) ->
    [odbc_queries:escape(C) || C <- S];

escape(S) when is_binary(S) ->
    [odbc_queries:escape(C) || <<C>> <= S].

%% Escape character that will confuse an SQL engine
%% Percent and underscore only need to be escaped for pattern matching like
%% statement
escape_like(S) when is_list(S) ->
    [escape_like(C) || C <- S];
escape_like($%) -> "\\%";
escape_like($_) -> "\\_";
escape_like(C)  -> odbc_queries:escape(C).

to_bool("t") -> true;
to_bool("true") -> true;
to_bool("1") -> true;
to_bool(true) -> true;
to_bool(1) -> true;
to_bool(_) -> false.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------
init([Host, StartInterval]) ->
    case ejabberd_config:get_local_option({odbc_keepalive_interval, Host}) of
	KeepaliveInterval when is_integer(KeepaliveInterval) ->
	    timer:apply_interval(KeepaliveInterval*1000, ?MODULE,
				 keep_alive, [self()]);
	undefined ->
	    ok;
	_Other ->
	    ?ERROR_MSG("Wrong odbc_keepalive_interval definition '~p'"
		       " for host ~p.~n", [_Other, Host])
    end,
    [DBType | _] = db_opts(Host),
    ?GEN_FSM:send_event(self(), connect),
    ejabberd_odbc_sup:add_pid(Host, self(), DBType),
    {ok, connecting, #state{db_type = DBType,
			    host = Host,
			    max_pending_requests_len = max_fsm_queue(),
			    pending_requests = {0, queue:new()},
			    start_interval = StartInterval}}.

connecting(connect, #state{host = Host} = State) ->
    ConnectRes = case db_opts(Host) of
		     [mysql | Args] ->
			 apply(fun mysql_connect/5, Args);
		     [pgsql | Args] ->
			 apply(fun pgsql_connect/5, Args);
		     [odbc | Args] ->
			 apply(fun odbc_connect/1, Args)
		 end,
    {_, PendingRequests} = State#state.pending_requests,
    case ConnectRes of
	{ok, Ref} ->
	    erlang:monitor(process, Ref),
	    lists:foreach(
	      fun(Req) ->
		      ?GEN_FSM:send_event(self(), Req)
	      end, queue:to_list(PendingRequests)),
	    {next_state, session_established,
	     State#state{db_ref = Ref,
			 pending_requests = {0, queue:new()}}};
	{error, Reason} ->
	    ?INFO_MSG("~p connection failed:~n"
		      "** Reason: ~p~n"
		      "** Retry after: ~p seconds",
		      [State#state.db_type, Reason,
		       State#state.start_interval div 1000]),
	    ?GEN_FSM:send_event_after(State#state.start_interval,
				      connect),
	    {next_state, connecting, State}
    end;
connecting(Event, State) ->
    ?WARNING_MSG("unexpected event in 'connecting': ~p", [Event]),
    {next_state, connecting, State}.

connecting({sql_cmd, {sql_query, ?KEEPALIVE_QUERY}, _Timestamp}, From, State) ->
    ?GEN_FSM:reply(From, {error, "SQL connection failed"}),
    {next_state, connecting, State};
connecting({sql_cmd, Command, Timestamp} = Req, From, State) ->
    ?DEBUG("queuing pending request while connecting:~n\t~p", [Req]),
    {Len, PendingRequests} = State#state.pending_requests,
    NewPendingRequests =
	if Len < State#state.max_pending_requests_len ->
		{Len + 1, queue:in({sql_cmd, Command, From, Timestamp}, PendingRequests)};
	   true ->
		lists:foreach(
		  fun({sql_cmd, _, To, _Timestamp}) ->
			  ?GEN_FSM:reply(
			     To, {error, "SQL connection failed"})
		  end, queue:to_list(PendingRequests)),
		{1, queue:from_list([{sql_cmd, Command, From, Timestamp}])}
	end,
    {next_state, connecting,
     State#state{pending_requests = NewPendingRequests}};
connecting(Request, {Who, _Ref}, State) ->
    ?WARNING_MSG("unexpected call ~p from ~p in 'connecting'",
		 [Request, Who]),
    {reply, {error, badarg}, connecting, State}.

session_established({sql_cmd, Command, Timestamp}, From, State) ->
    run_sql_cmd(Command, From, State, Timestamp);
session_established(Request, {Who, _Ref}, State) ->
    ?WARNING_MSG("unexpected call ~p from ~p in 'session_established'",
		 [Request, Who]),
    {reply, {error, badarg}, session_established, State}.

session_established({sql_cmd, Command, From, Timestamp}, State) ->
    run_sql_cmd(Command, From, State, Timestamp);
session_established(Event, State) ->
    ?WARNING_MSG("unexpected event in 'session_established': ~p", [Event]),
    {next_state, session_established, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% We receive the down signal when we loose the MySQL connection (we are
%% monitoring the connection)
handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, _StateName, State) ->
    ?GEN_FSM:send_event(self(), connect),
    {next_state, connecting, State};
handle_info(Info, StateName, State) ->
    ?WARNING_MSG("unexpected info in ~p: ~p", [StateName, Info]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    DBType = State#state.db_type,
    ejabberd_odbc_sup:remove_pid(State#state.host, self(), DBType),
    case DBType of
	mysql ->
	    %% old versions of mysql driver don't have the stop function
	    %% so the catch
	    catch mysql_conn:stop(State#state.db_ref);
	_ ->
	    ok
    end,
    ok.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) ->
   State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

run_sql_cmd(Command, From, State, Timestamp) ->
    case timer:now_diff(now(), Timestamp) div 1000 of
	Age when Age  < ?TRANSACTION_TIMEOUT ->
	    put(?NESTING_KEY, ?TOP_LEVEL_TXN),
	    put(?STATE_KEY, State),
	    abort_on_driver_error(outer_op(Command), From);
	Age ->
	    ?ERROR_MSG("Database was not available or too slow,"
		       " discarding ~p milliseconds old request~n~p~n",
		       [Age, Command]),
	    {next_state, session_established, State}
    end.

%% Only called by handle_call, only handles top level operations.
%% @spec outer_op(Op) -> {error, Reason} | {aborted, Reason} | {atomic, Result}
outer_op({sql_query, Query}) ->
    sql_query_internal(Query);
outer_op({sql_transaction, F}) ->
    outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, "");
outer_op({sql_bloc, F}) ->
    execute_bloc(F).

%% Called via sql_query/transaction/bloc from client code when inside a
%% nested operation
nested_op({sql_query, Query}) ->
    %% XXX - use sql_query_t here insted? Most likely would break
    %% callers who expect {error, _} tuples (sql_query_t turns
    %% these into throws)
    sql_query_internal(Query);
nested_op({sql_transaction, F}) ->
    NestingLevel = get(?NESTING_KEY),
    if NestingLevel =:= ?TOP_LEVEL_TXN ->
            %% First transaction inside a (series of) sql_blocs
            outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, "");
       true ->
            %% Transaction inside a transaction
            inner_transaction(F)
    end;
nested_op({sql_bloc, F}) ->
    execute_bloc(F).

%% Never retry nested transactions - only outer transactions
inner_transaction(F) ->
    PreviousNestingLevel = get(?NESTING_KEY),
    case get(?NESTING_KEY) of
        ?TOP_LEVEL_TXN ->
            {backtrace, T} = process_info(self(), backtrace),
            ?ERROR_MSG("inner transaction called at outer txn level. Trace: ~s",
		       [T]),
            erlang:exit(implementation_faulty);
        _N -> ok
    end,
    put(?NESTING_KEY, PreviousNestingLevel + 1),
    Result = (catch F()),
    put(?NESTING_KEY, PreviousNestingLevel),
    case Result of
        {aborted, Reason} ->
            {aborted, Reason};
        {'EXIT', Reason} ->
            {'EXIT', Reason};
        {atomic, Res} ->
            {atomic, Res};
        Res ->
            {atomic, Res}
    end.

outer_transaction(F, NRestarts, _Reason) ->
    PreviousNestingLevel = get(?NESTING_KEY),
    case get(?NESTING_KEY) of
        ?TOP_LEVEL_TXN ->
            ok;
        _N ->
            {backtrace, T} = process_info(self(), backtrace),
            ?ERROR_MSG("outer transaction called at inner txn level. Trace: ~s",
		       [T]),
            erlang:exit(implementation_faulty)
    end,
    sql_query_internal("begin;"),
    put(?NESTING_KEY, PreviousNestingLevel + 1),
    Result = (catch F()),
    put(?NESTING_KEY, PreviousNestingLevel),
    case Result of
        {aborted, Reason} when NRestarts > 0 ->
            %% Retry outer transaction upto NRestarts times.
            sql_query_internal("rollback;"),
            outer_transaction(F, NRestarts - 1, Reason);
        {aborted, Reason} when NRestarts =:= 0 ->
            %% Too many retries of outer transaction.
            ?ERROR_MSG("SQL transaction restarts exceeded~n"
                       "** Restarts: ~p~n"
                       "** Last abort reason: ~p~n"
                       "** Stacktrace: ~p~n"
                       "** When State == ~p",
                       [?MAX_TRANSACTION_RESTARTS, Reason,
                        erlang:get_stacktrace(), get(?STATE_KEY)]),
            sql_query_internal("rollback;"),
            {aborted, Reason};
        {'EXIT', Reason} ->
            %% Abort sql transaction on EXIT from outer txn only.
            sql_query_internal("rollback;"),
            {aborted, Reason};
        Res ->
            %% Commit successful outer txn
            sql_query_internal("commit;"),
            {atomic, Res}
    end.

execute_bloc(F) ->
    %% We don't alter ?NESTING_KEY here as only SQL transactions alter
    %% txn nesting
    case catch F() of
        {aborted, Reason} ->
            {aborted, Reason};
        {'EXIT', Reason} ->
            {aborted, Reason};
        Res ->
            {atomic, Res}
    end.

sql_query_internal(Query) ->
    State = get(?STATE_KEY),
    Res = case State#state.db_type of
              odbc ->
                  odbc:sql_query(State#state.db_ref, Query, ?TRANSACTION_TIMEOUT - 1000);
              pgsql ->
                  %% TODO: We need to propagate the TRANSACTION_TIMEOUT to pgsql driver, but no yet supported in driver.
                  %% See EJAB-1266
                  pgsql_to_odbc(pgsql:squery(State#state.db_ref, Query));
              mysql ->
                  ?DEBUG("MySQL, Send query~n~p~n", [Query]),
                  R = mysql_to_odbc(mysql_conn:fetch(State#state.db_ref,
						     Query, self(), ?TRANSACTION_TIMEOUT - 1000)),
                  %% ?INFO_MSG("MySQL, Received result~n~p~n", [R]),
                  R
          end,
    case Res of
	{error, "No SQL-driver information available."} ->
	    % workaround for odbc bug
	    {updated, 0};
        _Else -> Res
    end.

%% Generate the OTP callback return tuple depending on the driver result.
abort_on_driver_error({error, "query timed out"} = Reply, From) ->
    %% mysql driver error
    ?GEN_FSM:reply(From, Reply),
    {stop, timeout, get(?STATE_KEY)};
abort_on_driver_error({error, "Failed sending data on socket" ++ _} = Reply,
		      From) ->
    %% mysql driver error
    ?GEN_FSM:reply(From, Reply),
    {stop, closed, get(?STATE_KEY)};
abort_on_driver_error(Reply, From) ->
    ?GEN_FSM:reply(From, Reply),
    {next_state, session_established, get(?STATE_KEY)}.


%% == pure ODBC code

%% part of init/1
%% Open an ODBC database connection
odbc_connect(SQLServer) ->
    application:start(odbc),
    odbc:connect(SQLServer, [{scrollable_cursors, off}]).

%% == Native PostgreSQL code

%% part of init/1
%% Open a database connection to PostgreSQL
pgsql_connect(Server, Port, DB, Username, Password) ->
    pgsql:connect(Server, DB, Username, Password, Port).

%% Convert PostgreSQL query result to Erlang ODBC result formalism
pgsql_to_odbc({ok, PGSQLResult}) ->
    case PGSQLResult of
	[Item] ->
	    pgsql_item_to_odbc(Item);
	Items ->
	    [pgsql_item_to_odbc(Item) || Item <- Items]
    end.

pgsql_item_to_odbc({"SELECT" ++ _, Rows, Recs}) ->
    {selected,
     [case Row of 
          {desc, _, Col, _, _, _, _, _} -> Col; % Recent pgsql driver API change.
          _ -> element(1, Row)
      end
      || Row <- Rows],
     [list_to_tuple(Rec) || Rec <- Recs]};
pgsql_item_to_odbc("INSERT " ++ OIDN) ->
    [_OID, N] = string:tokens(OIDN, " "),
    {updated, list_to_integer(N)};
pgsql_item_to_odbc("DELETE " ++ N) ->
    {updated, list_to_integer(N)};
pgsql_item_to_odbc("UPDATE " ++ N) ->
    {updated, list_to_integer(N)};
pgsql_item_to_odbc({error, Error}) ->
    {error, Error};
pgsql_item_to_odbc(_) ->
    {updated,undefined}.

%% == Native MySQL code

%% part of init/1
%% Open a database connection to MySQL
mysql_connect(Server, Port, DB, Username, Password) ->
    case mysql_conn:start(Server, Port, Username, Password, DB, fun log/3) of
	{ok, Ref} ->
            mysql_conn:fetch(Ref, ["set names 'utf8';"], self()),
            mysql_conn:fetch(Ref, ["SET SESSION query_cache_type=1;"], self()),
	    {ok, Ref};
	Err ->
	    Err
    end.

%% Convert MySQL query result to Erlang ODBC result formalism
mysql_to_odbc({updated, MySQLRes}) ->
    {updated, mysql:get_result_affected_rows(MySQLRes)};
mysql_to_odbc({data, MySQLRes}) ->
    mysql_item_to_odbc(mysql:get_result_field_info(MySQLRes),
		       mysql:get_result_rows(MySQLRes));
mysql_to_odbc({error, MySQLRes}) when is_list(MySQLRes) ->
    {error, MySQLRes};
mysql_to_odbc({error, MySQLRes}) ->
    {error, mysql:get_result_reason(MySQLRes)}.

%% When tabular data is returned, convert it to the ODBC formalism
mysql_item_to_odbc(Columns, Recs) ->
    %% For now, there is a bug and we do not get the correct value from MySQL
    %% module:
    {selected,
     [element(2, Column) || Column <- Columns],
     [list_to_tuple(Rec) || Rec <- Recs]}.

% log function used by MySQL driver
log(Level, Format, Args) ->
    case Level of
	debug ->
	    ?DEBUG(Format, Args);
	normal ->
	    ?INFO_MSG(Format, Args);
	error ->
	    ?ERROR_MSG(Format, Args)
    end.

%% TODO: update this function to handle the case clase {host, VhostName}
db_opts(Host) ->
    case ejabberd_config:get_local_option({odbc_server, Host}) of
	%% Default pgsql port
	{pgsql, Server, DB, User, Pass} ->
	    [pgsql, Server, ?PGSQL_PORT, DB, User, Pass];
	{pgsql, Server, Port, DB, User, Pass} when is_integer(Port) ->
	    [pgsql, Server, Port, DB, User, Pass];
	%% Default mysql port
	{mysql, Server, DB, User, Pass} ->
	    [mysql, Server, ?MYSQL_PORT, DB, User, Pass];
	{mysql, Server, Port, DB, User, Pass} when is_integer(Port) ->
	    [mysql, Server, Port, DB, User, Pass];
	SQLServer when is_list(SQLServer) ->
	    [odbc, SQLServer]
    end.

max_fsm_queue() ->
    case ejabberd_config:get_local_option(max_fsm_queue) of
	N when is_integer(N), N>0 ->
	    N;
	_ ->
	    undefined
    end.

fsm_limit_opts() ->
    case max_fsm_queue() of
	N when is_integer(N) ->
	    [{max_queue, N}];
	_ ->
	    []
    end.
