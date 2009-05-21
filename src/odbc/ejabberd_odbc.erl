%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve ODBC connection
%%% Created :  8 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

-behaviour(gen_server).

%% External exports
-export([start/1, start_link/2,
	 sql_query/2,
	 sql_query_t/1,
	 sql_transaction/2,
	 sql_bloc/2,
	 escape/1,
	 escape_like/1,
	 keep_alive/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-include("ejabberd.hrl").

-record(state, {db_ref, db_type}).

-define(STATE_KEY, ejabberd_odbc_state).
-define(MAX_TRANSACTION_RESTARTS, 10).
-define(PGSQL_PORT, 5432).
-define(MYSQL_PORT, 3306).

-define(TRANSACTION_TIMEOUT, 60000).
-define(KEEPALIVE_TIMEOUT, 60000).
-define(KEEPALIVE_QUERY, "SELECT 1;").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    gen_server:start(ejabberd_odbc, [Host], []).

start_link(Host, StartInterval) ->
    gen_server:start_link(ejabberd_odbc, [Host, StartInterval], []).

sql_query(Host, Query) ->
    Msg = {sql_query, Query},
    sql_call(Host, Msg,).

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
sql_transaction(Host, F) ->
    Msg = {sql_transaction, F},
    sql_call(Host, Msg).

%% SQL bloc, based on a erlang anonymous function (F = fun)
sql_bloc(Host, F) ->
    Msg = {sql_bloc, F},
    sql_call(Host, Msg).

sql_call(Host, Msg) ->
    case get(?STATE_KEY) of
        undefined ->
            gen_server:call(ejabberd_odbc_sup:get_random_pid(Host),
                            Msg, ?TRANSACTION_TIMEOUT);
        State ->
            %% Query, Transaction or Bloc nested inside transaction
            nested_op(Msg, State)
    end.


%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    State = get(?STATE_KEY),
    QRes = sql_query_internal(State, Query),
    case QRes of
	{error, "No SQL-driver information available."} ->
	    % workaround for odbc bug
	    {updated, 0};
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


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([Host, StartInterval]) ->
    case ejabberd_config:get_local_option({odbc_keepalive_interval, Host}) of
	KeepaliveInterval when is_integer(KeepaliveInterval) ->
	    timer:apply_interval(KeepaliveInterval*1000, ?MODULE, keep_alive, [self()]);
	undefined ->
	    ok;
	_Other ->
	    ?ERROR_MSG("Wrong odbc_keepalive_interval definition '~p' for host ~p.~n", [_Other, Host])
    end,
    SQLServer = ejabberd_config:get_local_option({odbc_server, Host}),
    case SQLServer of
	%% Default pgsql port
	{pgsql, Server, DB, Username, Password} ->
	    pgsql_connect(Server, ?PGSQL_PORT, DB, Username, Password, StartInterval);
	{pgsql, Server, Port, DB, Username, Password} when is_integer(Port) ->
	    pgsql_connect(Server, Port, DB, Username, Password, StartInterval);
	%% Default mysql port
	{mysql, Server, DB, Username, Password} ->
	    mysql_connect(Server, ?MYSQL_PORT, DB, Username, Password, StartInterval);
	{mysql, Server, Port, DB, Username, Password} when is_integer(Port) ->
	    mysql_connect(Server, Port, DB, Username, Password, StartInterval);
	_ when is_list(SQLServer) ->
	    odbc_connect(SQLServer, StartInterval)
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Command, _From, State) ->
    dispatch_sql_command(Command, State).

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% We receive the down signal when we loose the MySQL connection (we are
%% monitoring the connection)
%% => We exit and let the supervisor restart the connection.
handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, State) ->
    {stop, connection_dropped, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, State) ->
    case State#state.db_type of
	mysql ->
	    % old versions of mysql driver don't have the stop function
	    % so the catch
	    catch mysql_conn:stop(State#state.db_ref);
	_ ->
	    ok
    end,
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
dispatch_sql_command({sql_query, Query}, State) ->
    abort_on_driver_error(sql_query_internal(State, Query), State);
dispatch_sql_command({sql_transaction, F}, State) ->
    abort_on_driver_error(
      execute_transaction(State, F, ?MAX_TRANSACTION_RESTARTS, ""), State);
dispatch_sql_command({sql_bloc, F}, State) ->
    abort_on_driver_error(execute_bloc(State, F), State);
dispatch_sql_command(Request, State) ->
    ?WARNING_MSG("Unexpected call ~p.", [Request]),
    {reply, ok, State}.

sql_query_internal(State, Query) ->
    Nested = case get(?STATE_KEY) of
		 undefined -> put(?STATE_KEY, State), false;
		 _State -> true
	     end,
    Result = case State#state.db_type of
		 odbc ->
		     odbc:sql_query(State#state.db_ref, Query);
		 pgsql ->
		     pgsql_to_odbc(pgsql:squery(State#state.db_ref, Query));
		 mysql ->
		     ?DEBUG("MySQL, Send query~n~p~n", [Query]),
		     R = mysql_to_odbc(mysql_conn:fetch(State#state.db_ref, Query, self())),
		     ?DEBUG("MySQL, Received result~n~p~n", [R]),
		     R
	     end,
    case Nested of
	true -> Result;
	false -> erase(?STATE_KEY), Result
    end.

execute_transaction(State, _F, 0, Reason) ->
    ?ERROR_MSG("SQL transaction restarts exceeded~n"
	       "** Restarts: ~p~n"
	       "** Last abort reason: ~p~n"
	       "** Stacktrace: ~p~n"
	       "** When State == ~p",
	       [?MAX_TRANSACTION_RESTARTS, Reason,
		erlang:get_stacktrace(), State]),
    sql_query_internal(State, "rollback;"),
    {aborted, restarts_exceeded};
execute_transaction(State, F, NRestarts, _Reason) ->
    Nested = case get(?STATE_KEY) of
		 undefined ->
		     put(?STATE_KEY, State),
		     sql_query_internal(State, "begin;"),
		     false;
		 _State ->
		     true
	     end,
    Result = case catch F() of
		 {aborted, Reason} ->
		     execute_transaction(State, F, NRestarts - 1, Reason);
		 {'EXIT', Reason} ->
		     sql_query_internal(State, "rollback;"),
		     {aborted, Reason};
		 Res  ->
		     {atomic, Res}
			     end,
    case Nested of
	true -> Result;
	false -> sql_query_internal(State, "commit;"), erase(?STATE_KEY), Result
    end.

execute_bloc(State, F) ->
    Nested = case get(?STATE_KEY) of
		 undefined -> put(?STATE_KEY, State), false;
		 _State -> true
	     end,
    Result = case catch F() of
		 {aborted, Reason} ->
		     {aborted, Reason};
		 {'EXIT', Reason} ->
		     {aborted, Reason};
		 Res ->
		     {atomic, Res}
	     end,
    case Nested of
	true -> Result;
	false -> erase(?STATE_KEY), Result
    end.

nested_op(Op, State) ->
    case dispatch_sql_command(Op, State) of
	{reply, Res, NewState} ->
	    put(?STATE_KEY, NewState),
	    Res;
	{stop, _Reason, Reply, NewState} ->
	    put(?STATE_KEY, NewState),
	    throw({aborted, Reply});
	{noreply, NewState} ->
	    put(?STATE_KEY, NewState),
	    exit({bad_op_in_nested_txn, Op})
    end.

abort_on_driver_error({error, "query timed out"} = Reply, State) ->
    %% mysql driver error
    {stop, timeout, Reply, State};
abort_on_driver_error({error, "Failed sending data on socket"++_} = Reply, State) ->
    %% mysql driver error
    {stop, closed, Reply, State};
abort_on_driver_error(Reply, State) ->
    {reply, Reply, State}.


%% == pure ODBC code

%% part of init/1
%% Open an ODBC database connection
odbc_connect(SQLServer, StartInterval) ->
    application:start(odbc),
    case odbc:connect(SQLServer,[{scrollable_cursors, off}]) of
	{ok, Ref} ->
	    erlang:monitor(process, Ref),
	    {ok, #state{db_ref = Ref, db_type = odbc}};
	{error, Reason} ->
	    ?ERROR_MSG("ODBC connection (~s) failed: ~p~n",
		       [SQLServer, Reason]),
	    %% If we can't connect we wait before retrying
	    timer:sleep(StartInterval),
	    {stop, odbc_connection_failed}
    end.


%% == Native PostgreSQL code

%% part of init/1
%% Open a database connection to PostgreSQL
pgsql_connect(Server, Port, DB, Username, Password, StartInterval) ->
    case pgsql:connect(Server, DB, Username, Password, Port) of
	{ok, Ref} ->
	    erlang:monitor(process, Ref),
	    {ok, #state{db_ref = Ref, db_type = pgsql}};
	{error, Reason} ->
	    ?ERROR_MSG("PostgreSQL connection failed: ~p~n", [Reason]),
	    %% If we can't connect we wait before retrying
	    timer:sleep(StartInterval),
	    {stop, pgsql_connection_failed}
    end.

%% Convert PostgreSQL query result to Erlang ODBC result formalism
pgsql_to_odbc({ok, PGSQLResult}) ->
    case PGSQLResult of
	[Item] ->
	    pgsql_item_to_odbc(Item);
	Items ->
	    [pgsql_item_to_odbc(Item) || Item <- Items]
    end.

pgsql_item_to_odbc({"SELECT", Rows, Recs}) ->
    {selected,
     [element(1, Row) || Row <- Rows],
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
mysql_connect(Server, Port, DB, Username, Password, StartInterval) ->
    case mysql_conn:start(Server, Port, Username, Password, DB, fun log/3) of
	{ok, Ref} ->
	    erlang:monitor(process, Ref),
            mysql_conn:fetch(Ref, ["set names 'utf8';"], self()),
	    {ok, #state{db_ref = Ref, db_type = mysql}};
	{error, Reason} ->
	    ?ERROR_MSG("MySQL connection failed: ~p~nWaiting ~p seconds before retrying...~n",
		       [Reason, StartInterval div 1000]),
	    %% If we can't connect we wait before retrying
	    timer:sleep(StartInterval),
	    {stop, mysql_connection_failed}
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

% perform a harmless query on all opened connexions to avoid connexion close.
keep_alive(PID) ->
    gen_server:call(PID, {sql_query, ?KEEPALIVE_QUERY}, ?KEEPALIVE_TIMEOUT).

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
