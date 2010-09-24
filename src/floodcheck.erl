%%%-------------------------------------------------------------------
%%% File    : floodcheck.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Description : 
%%%
%%% Created : 11 Sep 2008 by Christophe Romain <christophe.romain@process-one.net>
%%%-------------------------------------------------------------------
-module(floodcheck).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([monitor/5, demonitor/1, interval/1, check/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_INTERVAL, 300). %% check every 5mn
-define(SERVER, ?MODULE).

-record(state, {timer, interval, monitors}).
-record(monitor, {id, pid, ref, info, rule, value, handler}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    case whereis(?SERVER) of
	undefined -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
	Pid -> {ok, Pid}
    end.

stop() ->
    gen_server:call(?SERVER, stop).
    
monitor(Id, Pid, Info, Spec, {Mod, Fun}) ->
    gen_server:cast(?SERVER, {monitor, Id, Pid, Info, Spec, {Mod, Fun}}).

demonitor(Id) ->
    gen_server:cast(?SERVER, {demonitor, Id}).

interval(Value) ->
    gen_server:cast(?SERVER, {interval, Value}).

check() ->
    gen_server:call(?SERVER, check).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Timer = erlang:send_after(?DEFAULT_INTERVAL*1000, ?SERVER, monitor),
    {ok, #state{timer=Timer, interval=?DEFAULT_INTERVAL, monitors=[]}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(check, _From, State) ->
    Reply = lists:map(fun(#monitor{id=Id}=M) ->
			      {Id, check(M)}
		      end, State#state.monitors),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    erlang:cancel_timer(State#state.timer),
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({monitor, Id, Pid, Info, Spec, Handler}, State) ->
    Monitors = State#state.monitors,
    Ref = erlang:monitor(process, Pid),
    {Rule, Value} = case Spec of
			{Op, V} -> {Op, V};
			V -> {'>', V}
		    end,
    Monitor = #monitor{id=Id, pid=Pid, ref=Ref, info=Info, rule=Rule, value=Value, handler=Handler},
    New = case lists:keysearch(Id, #monitor.id, Monitors) of
	      {value, #monitor{ref=OldRef}} ->
		  erlang:demonitor(OldRef),
		  lists:keyreplace(Id, #monitor.id, Monitors, Monitor);
	      _ -> 
		  [Monitor|Monitors]
	  end,
    {noreply,  State#state{monitors=New}};
handle_cast({demonitor, Id}, State) ->
    Monitors = State#state.monitors,
    New = case lists:keysearch(Id, #monitor.id, Monitors) of
	      {value, #monitor{ref=Ref}} ->
		  erlang:demonitor(Ref),
		  lists:keydelete(Id, #monitor.id, Monitors);
	      _ ->
		  Monitors
	  end,
    {noreply, State#state{monitors=New}};
handle_cast({interval, Value}, State) ->
    erlang:cancel_timer(State#state.timer),
    Timer = erlang:send_after(Value*1000, ?SERVER, monitor),
    {noreply, State#state{timer=Timer, interval=Value}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, _Type, _Pid, _Info}, State) ->
    Monitors = State#state.monitors,
    New = lists:keydelete(Ref, #monitor.ref, Monitors),
    {noreply, State#state{monitors=New}};
handle_info(monitor, State) ->
    lists:foreach(fun(#monitor{id=Id, pid=Pid, info=Info, handler={Mod, Fun}}=M) ->
			  case check(M) of
			      ok -> ok;
			      Value -> spawn(Mod, Fun, [Id, Pid, Info, Value])
			  end
		  end, State#state.monitors),
    Timer = erlang:send_after(State#state.interval*1000, ?SERVER, monitor),
    {noreply, State#state{timer=Timer}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

check(#monitor{pid=Pid, info=Info, rule=Rule, value=Value}) ->
    case catch process_info(Pid, Info) of
	{Info, Actual} ->
	    Check = case Info of
			messages -> byte_size(term_to_binary(Actual));
			dictionary -> byte_size(term_to_binary(Actual));
			_ -> Actual
		    end,
	    case Rule of
		'>' -> 
		    if Check > Value -> Value;
		       true -> ok
		    end;
		'<' -> 
		    if Check < Value -> Value;
		       true -> ok
		    end;
		'=' -> 
		    if Check == Value -> Value;
		       true -> ok
		    end;
		 _ -> 
		    ok
	    end;
	_ ->
	    ok
    end.

%%% Documentation
%%% authorized Info
%%%   message_queue_len: number of messages
%%%   messages: messages queue size in bytes
%%%   dictionary: dictionary size in bytes
%%%   total_heap_size: total size in words of all heap fragments
%%%   heap_size: size in words of youngest heap generation
%%%   stack_size: stack size in words
%%%   reductions: number of reductions executed by the process
%%%   memory: process size in bytes
