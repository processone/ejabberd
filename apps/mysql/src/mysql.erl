%%%-------------------------------------------------------------------
%%% File    : mysql.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: MySQL client.
%%%
%%% Created :  4 Aug 2005 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%
%%% Copyright (c) 2001-2004 Kungliga Tekniska Högskolan
%%% See the file COPYING
%%%
%%% Usage:
%%%
%%%
%%% Call one of the start-functions before any call to fetch/2
%%%
%%%   start_link(Id, Host, User, Password, Database)
%%%   start_link(Id, Host, Port, User, Password, Database)
%%%   start_link(Id, Host, User, Password, Database, LogFun)
%%%   start_link(Id, Host, Port, User, Password, Database, LogFun)
%%%
%%% Id is a connection group identifier. If you want to have more
%%% than one connection to a server (or a set of MySQL replicas),
%%% add more with
%%%
%%%   connect(Id, Host, Port, User, Password, Database, Reconnect)
%%%
%%% use 'undefined' as Port to get default MySQL port number (3306).
%%% MySQL querys will be sent in a per-Id round-robin fashion.
%%% Set Reconnect to 'true' if you want the dispatcher to try and
%%% open a new connection, should this one die.
%%%
%%% When you have a mysql_dispatcher running, this is how you make a
%%% query :
%%%
%%%   fetch(Id, "select * from hello") -> Result
%%%     Result = {data, MySQLRes} | {updated, MySQLRes} |
%%%              {error, MySQLRes}
%%%
%%% Actual data can be extracted from MySQLRes by calling the following API
%%% functions:
%%%     - on data received:
%%%          FieldInfo = mysql:get_result_field_info(MysqlRes)
%%%          AllRows   = mysql:get_result_rows(MysqlRes)
%%%         with FieldInfo = list() of {Table, Field, Length, Name}
%%%          and AllRows   = list() of list() representing records
%%%     - on update:
%%%          Affected  = mysql:get_result_affected_rows(MysqlRes)
%%%         with Affected  = integer()
%%%     - on error:
%%%          Reason    = mysql:get_result_reason(MysqlRes)
%%%         with Reason    = string()
%%%
%%% If you just want a single MySQL connection, or want to manage your
%%% connections yourself, you can use the mysql_conn module as a
%%% stand-alone single MySQL connection. See the comment at the top of
%%% mysql_conn.erl.
%%%
%%%-------------------------------------------------------------------
-module(mysql).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/5,
	 start_link/6,
	 start_link/7,

	 fetch/2,
	 fetch/3,

	 get_result_field_info/1,
	 get_result_rows/1,
	 get_result_affected_rows/1,
	 get_result_reason/1,

	 quote/1,
	 asciz_binary/2,

	 connect/7,
	 stop/0,

     gc_each/1
	]).

%%--------------------------------------------------------------------
%% Internal exports - just for mysql_* modules
%%--------------------------------------------------------------------
-export([log/3,
	 log/4
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-include("mysql.hrl").
-record(state, {
	  conn_list,	%% list() of mysql_connection record()
	  log_fun,	%% undefined | function for logging,
      gc_tref   %% undefined | timer:TRef
	 }).

-record(mysql_connection, {
	  id,		%% term(), user of 'mysql' modules id of this socket group
	  conn_pid,	%% pid(), mysql_conn process
	  reconnect,	%% true | false, should mysql_dispatcher try to reconnect if this connection dies?
	  host,		%% undefined | string()
	  port,		%% undefined | integer()
	  user,		%% undefined | string()
	  password,	%% undefined | string()
	  database	%% undefined | string()
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, mysql_dispatcher).
-define(CONNECT_TIMEOUT, 5000).
-define(LOCAL_FILES, 128).

-define(PORT, 3306).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link(Id, Host, User, Password, Database)
%%           start_link(Id, Host, Port, User, Password, Database)
%%           start_link(Id, Host, User, Password, Database, LogFun)
%%           start_link(Id, Host, Port, User, Password, Database,
%%                      LogFun)
%%           Id       = term(), first connection-group Id
%%           Host     = string()
%%           Port     = integer()
%%           User     = string()
%%           Password = string()
%%           Database = string()
%%           LogFun   = undefined | function() of arity 3
%% Descrip.: Starts the MySQL client gen_server process.
%% Returns : {ok, Pid} | ignore | {error, Error}
%%--------------------------------------------------------------------
start_link(Id, Host, User, Password, Database) when is_list(Host), is_list(User), is_list(Password),
						    is_list(Database) ->
    start_link(Id, Host, ?PORT, User, Password, Database, undefined).

start_link(Id, Host, Port, User, Password, Database) when is_list(Host), is_integer(Port), is_list(User),
							  is_list(Password), is_list(Database) ->
    start_link(Id, Host, Port, User, Password, Database, undefined);

start_link(Id, Host, User, Password, Database, LogFun) when is_list(Host), is_list(User), is_list(Password),
							    is_list(Database) ->
    start_link(Id, Host, ?PORT, User, Password, Database, LogFun).

start_link(Id, Host, Port, User, Password, Database, LogFun) when is_list(Host), is_integer(Port), is_list(User),
								  is_list(Password), is_list(Database) ->
    crypto:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Id, Host, Port, User, Password, Database, LogFun], []).

stop() ->
    gen_server:call(?SERVER, stop).

gc_each(Millisec) ->
    gen_server:call(?SERVER, {gc_each, Millisec}).

%%--------------------------------------------------------------------
%% Function: fetch(Id, Query)
%%           fetch(Id, Query, Timeout)
%%           Id      = term(), connection-group Id
%%           Query   = string(), MySQL query in verbatim
%%           Timeout = integer() | infinity, gen_server timeout value
%% Descrip.: Send a query and wait for the result.
%% Returns : {data, MySQLRes}    |
%%           {updated, MySQLRes} |
%%           {error, MySQLRes}
%%           MySQLRes = term()
%%--------------------------------------------------------------------
fetch(Id, Query) when is_list(Query) ->
    gen_server:call(?SERVER, {fetch, Id, Query}).
fetch(Id, Query, Timeout) when is_list(Query) ->
    gen_server:call(?SERVER, {fetch, Id, Query}, Timeout).

%%--------------------------------------------------------------------
%% Function: get_result_field_info(MySQLRes)
%%           MySQLRes = term(), result of fetch function on "data"
%% Descrip.: Extract the FieldInfo from MySQL Result on data received
%% Returns : FieldInfo
%%           FieldInfo = list() of {Table, Field, Length, Name}
%%--------------------------------------------------------------------
get_result_field_info(#mysql_result{fieldinfo = FieldInfo}) ->
    FieldInfo.

%%--------------------------------------------------------------------
%% Function: get_result_rows(MySQLRes)
%%           MySQLRes = term(), result of fetch function on "data"
%% Descrip.: Extract the Rows from MySQL Result on data received
%% Returns : Rows
%%           Rows = list() of list() representing records
%%--------------------------------------------------------------------
get_result_rows(#mysql_result{rows=AllRows}) ->
    AllRows.

%%--------------------------------------------------------------------
%% Function: get_result_affected_rows(MySQLRes)
%%           MySQLRes = term(), result of fetch function on "updated"
%% Descrip.: Extract the Rows from MySQL Result on update
%% Returns : AffectedRows
%%           AffectedRows = integer()
%%--------------------------------------------------------------------
get_result_affected_rows(#mysql_result{affectedrows=AffectedRows}) ->
    AffectedRows.

%%--------------------------------------------------------------------
%% Function: get_result_reason(MySQLRes)
%%           MySQLRes = term(), result of fetch function on "error"
%% Descrip.: Extract the error Reason from MySQL Result on error
%% Returns : Reason
%%           Reason    = string()
%%--------------------------------------------------------------------
get_result_reason(#mysql_result{error=Reason}) ->
    Reason.

%%--------------------------------------------------------------------
%% Function: quote(String)
%%           String = string()
%% Descrip.: Quote a string so that it can be included safely in a
%%           MySQL query.
%% Returns : Quoted = string()
%%--------------------------------------------------------------------
quote(String) when is_list(String) ->
    [34 | lists:reverse([34 | quote(String, [])])].	%% 34 is $"

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->		%% 39 is $'
    quote(Rest, [39, $\\ | Acc]);	%% 39 is $'
quote([34 | Rest], Acc) ->		%% 34 is $"
    quote(Rest, [34, $\\ | Acc]);	%% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

%%--------------------------------------------------------------------
%% Function: asciz_binary(Data, Acc)
%%           Data = binary()
%%           Acc  = list(), input accumulator
%% Descrip.: Find the first zero-byte in Data and add everything
%%           before it to Acc, as a string.
%% Returns : {NewList, Rest}
%%           NewList = list(), Acc plus what we extracted from Data
%%           Rest    = binary(), whatever was left of Data, not
%%                     including the zero-byte
%%--------------------------------------------------------------------
asciz_binary(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
asciz_binary(<<0:8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) ->
    asciz_binary(Rest, [C | Acc]).

%%--------------------------------------------------------------------
%% Function: connect(Id, Host, Port, User, Password, Database,
%%                   Reconnect)
%%           Id        = term(), connection-group Id
%%           Host      = string()
%%           Port      = undefined | integer()
%%           User      = string()
%%           Password  = string()
%%           Database  = string()
%%           Reconnect = true | false
%% Descrip.: Starts a MySQL connection and, if successfull, registers
%%           it with the mysql_dispatcher.
%% Returns : {ok, ConnPid} | {error, Reason}
%%--------------------------------------------------------------------
connect(Id, Host, undefined, User, Password, Database, Reconnect) ->
    connect(Id, Host, ?PORT, User, Password, Database, Reconnect);
connect(Id, Host, Port, User, Password, Database, Reconnect) ->
    {ok, LogFun} = gen_server:call(?SERVER, get_logfun),
    case mysql_conn:start(Host, Port, User, Password, Database, LogFun) of
	{ok, ConnPid} ->
	    MysqlConn =
		case Reconnect of
		    true ->
			#mysql_connection{id        = Id,
					  conn_pid  = ConnPid,
					  reconnect = true,
					  host      = Host,
					  port      = Port,
					  user      = User,
					  password  = Password,
					  database  = Database
					 };
		    false ->
			#mysql_connection{id        = Id,
					  conn_pid  = ConnPid,
					  reconnect = false
					 }
		end,
	    case gen_server:call(?SERVER, {add_mysql_connection, MysqlConn}) of
		ok ->
		    {ok, ConnPid};
		Res ->
		    Res
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: log(LogFun, Level, Format)
%%           log(LogFun, Level, Format, Arguments)
%%           LogFun    = undefined | function() with arity 3
%%           Level     = debug | normal | error
%%           Format    = string()
%%           Arguments = list() of term()
%% Descrip.: Either call the function LogFun with the Level, Format
%%           and Arguments as parameters or log it to the console if
%%           LogFun is undefined.
%% Returns : void()
%%
%% Note    : Exported only for use by the mysql_* modules.
%%
%%--------------------------------------------------------------------
log(LogFun, Level, Format) ->
    log(LogFun, Level, Format, []).

log(LogFun, Level, Format, Arguments) when is_function(LogFun) ->
    LogFun(Level, Format, Arguments);
log(undefined, _Level, Format, Arguments) ->
    %% default is to log to console
    io:format(Format, Arguments),
    io:format("~n", []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%%           Args = [Id, Host, Port, User, Password, Database, LogFun]
%%             Id       = term(), connection-group Id
%%             Host     = string()
%%             Port     = integer()
%%             User     = string()
%%             Password = string()
%%             Database = string()
%%             LogFun   = undefined | function() with arity 3
%% Descrip.: Initiates the gen_server (MySQL dispatcher).
%%--------------------------------------------------------------------
init([Id, Host, Port, User, Password, Database, LogFun]) ->
    case mysql_conn:start(Host, Port, User, Password, Database, LogFun) of
	{ok, ConnPid} ->
	    MysqlConn = #mysql_connection{id        = Id,
					  conn_pid  = ConnPid,
					  reconnect = true,
					  host	    = Host,
					  port      = Port,
					  user      = User,
					  password  = Password,
					  database  = Database
					 },
	    case add_mysql_conn(MysqlConn, []) of
		{ok, ConnList} ->
		    {ok, #state{log_fun    = LogFun,
				conn_list = ConnList,
                gc_tref = undefined
			       }};
		error ->
		    Msg = "mysql: Failed adding first MySQL connection handler to my list, exiting",
		    log(LogFun, error, Msg),
		    {error, Msg}
	    end;
	{error, Reason} ->
	    log(LogFun, error, "mysql: Failed starting first MySQL connection handler, exiting"),
	    {stop, {error, Reason}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages.
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: handle_call({fetch, Id, Query}, From, State)
%%           Id    = term(), connection-group id
%%           Query = string(), MySQL query
%% Descrip.: Make a MySQL query. Use the first connection matching Id
%%           in our connection-list. Don't block the mysql_dispatcher
%%           by returning {noreply, ...} here and let the mysql_conn
%%           do gen_server:reply(...) when it has an answer.
%% Returns : {noreply, NewState}             |
%%           {reply, {error, Reason}, State}
%%           NewState = state record()
%%           Reason   = atom() | string()
%%--------------------------------------------------------------------
handle_call({fetch, Id, Query}, From, State) ->
    log(State#state.log_fun, debug, "mysql: fetch ~p (id ~p)", [Query, Id]),
    case get_next_mysql_connection_for_id(Id, State#state.conn_list) of
	{ok, MysqlConn, RestOfConnList} when is_record(MysqlConn, mysql_connection) ->
	    mysql_conn:fetch(MysqlConn#mysql_connection.conn_pid, Query, From),
	    %% move this mysql socket to the back of the list
	    NewConnList = RestOfConnList ++ [MysqlConn],
	    %% The ConnPid process does a gen_server:reply() when it has an answer
	    {noreply, State#state{conn_list = NewConnList}};
	nomatch ->
	    %% we have no active connection matching Id
	    {reply, {error, no_connection}, State}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call({add_mysql_connection, Conn}, From, State)
%%           Conn = mysql_connection record()
%% Descrip.: Add Conn to our list of connections.
%% Returns : {reply, Reply, NewState}
%%           Reply = ok | {error, Reason}
%%           NewState = state record()
%%           Reason   = string()
%%--------------------------------------------------------------------
handle_call({add_mysql_connection, Conn}, _From, State) when is_record(Conn, mysql_connection) ->
    case add_mysql_conn(Conn, State#state.conn_list) of
	{ok, NewConnList} ->
	    {Id, ConnPid} = {Conn#mysql_connection.id, Conn#mysql_connection.conn_pid},
	    log(State#state.log_fun, normal, "mysql: Added connection with id '~p' (pid ~p) to my list",
		[Id, ConnPid]),
	    {reply, ok, State#state{conn_list = NewConnList}};
	error ->
	    {reply, {error, "failed adding MySQL connection to my list"}, State}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call(get_logfun, From, State)
%% Descrip.: Fetch our logfun.
%% Returns : {reply, {ok, LogFun}, State}
%%           LogFun = undefined | function() with arity 3
%%--------------------------------------------------------------------
handle_call(get_logfun, _From, State) ->
    {reply, {ok, State#state.log_fun}, State};

handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call({gc_each, Millisec}, _From, State) ->
    case State#state.gc_tref of
        undefined -> ok;
        TRef ->
            timer:cancel(TRef)
    end,
    case timer:send_interval(Millisec, gc) of
        {ok, NewTRef} ->
            {reply, ok, State#state{gc_tref = NewTRef}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(Unknown, _From, State) ->
    log(State#state.log_fun, error, "mysql: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "unknown gen_server call in mysql client"}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    log(State#state.log_fun, error, "mysql: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info({'DOWN', ...}, State)
%% Descrip.: Handle a message that one of our monitored processes
%%           (mysql_conn processes in our connection list) has exited.
%%           Remove the entry from our list.
%% Returns : {noreply, NewState}   |
%%           {stop, normal, State}
%%           NewState = state record()
%%
%% Note    : For now, we stop if our connection list becomes empty.
%%           We should try to reconnect for a while first, to not
%%           eventually stop the whole OTP application if the MySQL-
%%           server is shut down and the mysql_dispatcher was super-
%%           vised by an OTP supervisor.
%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, process, Pid, Info}, State) ->
    LogFun = State#state.log_fun,
    case remove_mysql_connection_using_pid(Pid, State#state.conn_list, []) of
	{ok, Conn, NewConnList} ->
	    LogLevel = case Info of
			   normal -> normal;
			   _ -> error
		       end,
	    log(LogFun, LogLevel, "mysql: MySQL connection pid ~p exited : ~p", [Pid, Info]),
	    log(LogFun, normal, "mysql: Removed MySQL connection with pid ~p from list",
		[Pid]),
	    case Conn#mysql_connection.reconnect of
		true ->
		    start_reconnect(Conn, LogFun);
		false ->
		    ok
	    end,
	    {noreply, State#state{conn_list = NewConnList}};
	nomatch ->
	    log(LogFun, error, "mysql: Received 'DOWN' signal from pid ~p not in my list", [Pid]),
	    {noreply, State}
    end;

handle_info(gc, #state{conn_list = Connections} = State) ->
    [erlang:garbage_collect(C#mysql_connection.conn_pid) || C <- Connections],
    erlang:garbage_collect(self()),
    {noreply, State};


handle_info(Info, State) ->
    log(State#state.log_fun, error, "mysql: Received unknown signal : ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : Reason
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    LogFun = State#state.log_fun,
    LogLevel = case Reason of
		   normal -> debug;
		   _ -> error
	       end,
    log(LogFun, LogLevel, "mysql: Terminating with reason : ~p", [Reason]),
    lists:foreach(fun(MysqlConn) ->
			  MysqlConn#mysql_connection.conn_pid ! close
		  end, State#state.conn_list),
    Reason.

%%--------------------------------------------------------------------
%% Function: code_change(_OldVsn, State, _Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, State}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add_mysql_conn(Conn, ConnList)
%%           Conn     = mysql_connection record()
%%           ConnList = list() of mysql_connection record()
%% Descrip.: Set up process monitoring of the mysql_conn process and
%%           then add it (first) to ConnList.
%% Returns : NewConnList = list() of mysql_connection record()
%%--------------------------------------------------------------------
add_mysql_conn(Conn, ConnList) when is_record(Conn, mysql_connection), is_list(ConnList) ->
    erlang:monitor(process, Conn#mysql_connection.conn_pid),
    {ok, [Conn | ConnList]}.

%%--------------------------------------------------------------------
%% Function: remove_mysql_connection_using_pid(Pid, ConnList)
%%           Pid      = pid()
%%           ConnList = list() of mysql_connection record()
%% Descrip.: Removes the first mysql_connection in ConnList that has
%%           a pid matching Pid.
%% Returns : {ok, Conn, NewConnList} | nomatch
%%           Conn        = mysql_connection record()
%%           NewConnList = list() of mysql_connection record()
%%--------------------------------------------------------------------
remove_mysql_connection_using_pid(Pid, [#mysql_connection{conn_pid = Pid} = H | T], Res) ->
    {ok, H, lists:reverse(Res) ++ T};
remove_mysql_connection_using_pid(Pid, [H | T], Res) when is_record(H, mysql_connection) ->
    remove_mysql_connection_using_pid(Pid, T, [H | Res]);
remove_mysql_connection_using_pid(_Pid, [], _Res) ->
    nomatch.

%%--------------------------------------------------------------------
%% Function: get_next_mysql_connection_for_id(Id, ConnList)
%%           Id       = term(), connection-group id
%%           ConnList = list() of mysql_connection record()
%% Descrip.: Find the first mysql_connection in ConnList that has an
%%           id matching Id.
%% Returns : {ok, Conn, NewConnList} | nomatch
%%           Conn        = mysql_connection record()
%%           NewConnList = list() of mysql_connection record(), same
%%                         as ConnList but without Conn
%%--------------------------------------------------------------------
get_next_mysql_connection_for_id(Id, ConnList) ->
    get_next_mysql_connection_for_id(Id, ConnList, []).

get_next_mysql_connection_for_id(Id, [#mysql_connection{id = Id} = H | T], Res) ->
    {ok, H, lists:reverse(Res) ++ T};
get_next_mysql_connection_for_id(Id, [H | T], Res) when is_record(H, mysql_connection) ->
    get_next_mysql_connection_for_id(Id, T, [H | Res]);
get_next_mysql_connection_for_id(_Id, [], _Res) ->
    nomatch.

%%--------------------------------------------------------------------
%% Function: start_reconnect(Conn, LogFun)
%%           Conn   = mysql_connection record()
%%           LogFun = undefined | function() with arity 3
%% Descrip.: Spawns a process that will try to re-establish a new
%%           connection instead of the one in Conn which has just
%%           died.
%% Returns : ok
%%--------------------------------------------------------------------
start_reconnect(Conn, LogFun) when is_record(Conn, mysql_connection) ->
    Pid = spawn(fun () ->
			reconnect_loop(Conn#mysql_connection{conn_pid = undefined}, LogFun, 0)
		end),
    {Id, Host, Port} = {Conn#mysql_connection.id, Conn#mysql_connection.host, Conn#mysql_connection.port},
    log(LogFun, debug, "mysql: Started pid ~p to try and reconnect to ~p:~s:~p (replacing "
	"connection with pid ~p)", [Pid, Id, Host, Port, Conn#mysql_connection.conn_pid]),
    ok.

%%--------------------------------------------------------------------
%% Function: reconnect_loop(Conn, LogFun, 0)
%%           Conn   = mysql_connection record()
%%           LogFun = undefined | function() with arity 3
%% Descrip.: Loop indefinately until we are able to reconnect to the
%%           server specified in the now dead connection Conn.
%% Returns : ok
%%--------------------------------------------------------------------
reconnect_loop(Conn, LogFun, N) when is_record(Conn, mysql_connection) ->
    {Id, Host, Port} = {Conn#mysql_connection.id, Conn#mysql_connection.host, Conn#mysql_connection.port},
    case connect(Id,
		 Host,
		 Port,
		 Conn#mysql_connection.user,
		 Conn#mysql_connection.password,
		 Conn#mysql_connection.database,
		 Conn#mysql_connection.reconnect) of
	{ok, ConnPid} ->
	    log(LogFun, debug, "mysql_reconnect: Managed to reconnect to ~p:~s:~p (connection pid ~p)",
		[Id, Host, Port, ConnPid]),
	    ok;
	{error, Reason} ->
	    %% log every once in a while
	    NewN = case N of
		       10 ->
			   log(LogFun, debug, "mysql_reconnect: Still unable to connect to ~p:~s:~p (~p)",
			       [Id, Host, Port, Reason]),
			   0;
		       _ ->
			   N + 1
		   end,
	    %% sleep between every unsuccessfull attempt
	    timer:sleep(20 * 1000),
	    reconnect_loop(Conn, LogFun, NewN)
    end.
