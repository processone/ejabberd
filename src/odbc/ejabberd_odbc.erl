%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Serve ODBC connection
%%% Created :  8 Dec 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_odbc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_server).

%% External exports
-export([start/1, start_link/1,
	 sql_query/2,
	 sql_query_t/1,
	 sql_transaction/2,
	 escape/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-record(state, {db_ref, db_type}).

-define(STATE_KEY, ejabberd_odbc_state).
-define(MAX_TRANSACTION_RESTARTS, 10).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    gen_server:start(ejabberd_odbc, [Host], []).

start_link(Host) ->
    gen_server:start_link(ejabberd_odbc, [Host], []).

sql_query(Host, Query) ->
    gen_server:call(ejabberd_odbc_sup:get_random_pid(Host),
		    {sql_query, Query}, 60000).

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
    gen_server:call(ejabberd_odbc_sup:get_random_pid(Host),
		    {sql_transaction, F}, 60000).

%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    State = get(?STATE_KEY),
    QRes = sql_query_internal(State, Query),
    case QRes of
	{error, "No SQL-driver information available."} ->
	    % workaround for odbc bug
	    {updated, 0};
	{error, _} ->
	    throw(aborted);
	Rs when is_list(Rs) ->
	    case lists:keymember(error, 1, Rs) of
		true ->
		    throw(aborted);
		_ ->
		    QRes
	    end;
	_ ->
	    QRes
    end.

escape(S) ->
    [case C of
	 $\0 -> "\\0";
	 $\n -> "\\n";
	 $\t -> "\\t";
	 $\b -> "\\b";
	 $\r -> "\\r";
	 $'  -> "\\'";
	 $"  -> "\\\"";
	 $%  -> "\\%";
	 $_  -> "\\_";
	 $\\ -> "\\\\";
	 _ -> C
     end || C <- S].


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
init([Host]) ->
    SQLServer = ejabberd_config:get_local_option({odbc_server, Host}),
    case SQLServer of
	{pgsql, Server, DB, Username, Password} ->
	    {ok, Ref} = pgsql:connect(Server, DB, Username, Password),
	    {ok, #state{db_ref = Ref,
			db_type = pgsql}};
	_ when is_list(SQLServer) ->
	    {ok, Ref} = odbc:connect(SQLServer,
				     [{scrollable_cursors, off}]),
	    {ok, #state{db_ref = Ref, 
			db_type = odbc}}
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
handle_call({sql_query, Query}, _From, State) ->
    Reply = sql_query_internal(State, Query),
    {reply, Reply, State};

handle_call({sql_transaction, F}, _From, State) ->
    Reply = execute_transaction(State, F, ?MAX_TRANSACTION_RESTARTS),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

sql_query_internal(State, Query) ->
    case State#state.db_type of
	odbc ->
	    odbc:sql_query(State#state.db_ref, Query);
	pgsql ->
	    pgsql_to_odbc(pgsql:squery(State#state.db_ref, Query))
    end.

execute_transaction(_State, _F, 0) ->
    {aborted, restarts_exceeded};
execute_transaction(State, F, NRestarts) ->
    put(?STATE_KEY, State),
    sql_query_internal(State, "begin;"),
    case catch F() of
	aborted ->
	    execute_transaction(State, F, NRestarts - 1);
	{'EXIT', Reason} ->
	    sql_query_internal(State, "rollback;"),
	    {aborted, Reason};
	Res ->
	    sql_query_internal(State, "commit;"),
	    {atomic, Res}
    end.

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
pgsql_item_to_odbc({error, Error}) ->
    {error, Error};
pgsql_item_to_odbc(_) ->
    {updated,undefined}.

