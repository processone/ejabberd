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
-export([start/0, start_link/0,
	 sql_query/1,
	 escape/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-record(state, {odbc_ref}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ejabberd_odbc}, ejabberd_odbc, [], []).

start_link() ->
    gen_server:start_link({local, ejabberd_odbc}, ejabberd_odbc, [], []).

sql_query(Query) ->
    gen_server:call(ejabberd_odbc, {sql_query, Query}, 60000).

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
init([]) ->
    {ok, Ref} = odbc:connect("DSN=ejabberd;UID=ejabberd;PWD=ejabberd",
			     [{scrollable_cursors, off}]),
    {ok, #state{odbc_ref = Ref}}.

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
    Reply = odbc:sql_query(State#state.odbc_ref, Query),
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

