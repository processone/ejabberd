%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%% The code has been modified and improved by ProcessOne.
%% Copyright 2007-2012, ProcessOne
%%
%%  The change adds the following features:
%%   - You can send exit(priority_shutdown) to the p1_fsm process to
%%   terminate immediatetly. If the fsm trap_exit process flag has been
%%   set to true, the FSM terminate function will called.
%%   - You can pass the gen_fsm options to control resource usage.
%%   {max_queue, N} will exit the process with priority_shutdown
%%   - You can limit the time processing a message (TODO): If the
%%   message processing does not return in a given period of time, the
%%   process will be terminated.
%%   - You might customize the State data before sending it to error_logger
%%   in case of a crash (just export the function print_state/1)
%%
-module(p1_fsm).

%%%-----------------------------------------------------------------
%%%   
%%% This state machine is somewhat more pure than state_lib.  It is
%%% still based on State dispatching (one function per state), but
%%% allows a function handle_event to take care of events in all states.
%%% It's not that pure anymore :(  We also allow synchronized event sending.
%%%
%%% If the Parent process terminates the Module:terminate/2
%%% function is called.
%%%
%%% The user module should export:
%%%
%%%   init(Args)
%%%     ==> {ok, StateName, StateData}
%%%         {ok, StateName, StateData, Timeout}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   StateName(Msg, StateData)
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   StateName(Msg, From, StateData)
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {reply, Reply, NewStateName, NewStateData}
%%%        {reply, Reply, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_event(Msg, StateName, StateData)
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, Reply, NewStateData}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_sync_event(Msg, From, StateName, StateData)
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {reply, Reply, NewStateName, NewStateData}
%%%        {reply, Reply, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, Reply, NewStateData}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_info(Info, StateName) (e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   terminate(Reason, StateName, StateData) Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> the return value is ignored
%%%
%%%
%%% The work flow (of the fsm) can be described as follows:
%%%
%%%   User module                           fsm
%%%   -----------                          -------
%%%     start              ----->             start
%%%     init               <-----              .
%%%
%%%                                           loop
%%%     StateName          <-----              .
%%%
%%%     handle_event       <-----              .
%%%
%%%     handle__sunc_event <-----              .
%%%
%%%     handle_info        <-----              .
%%%
%%%     terminate          <-----              .
%%%
%%%
%%% ---------------------------------------------------

-export([start/3, start/4,
	 start_link/3, start_link/4,
	 send_event/2, sync_send_event/2, sync_send_event/3,
	 send_all_state_event/2,
	 sync_send_all_state_event/2, sync_send_all_state_event/3,
	 reply/2,
	 start_timer/2,send_event_after/2,cancel_timer/1,
	 enter_loop/4, enter_loop/5, enter_loop/6, wake_hib/7]).

-export([behaviour_info/1]).

%% Internal exports
-export([init_it/6, print_event/3,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 format_status/2]).

-import(error_logger , [format/2]).

%%% Internal gen_fsm state
%%% This state is used to defined resource control values:
-record(limits, {max_queue}).

%%% ---------------------------------------------------
%%% Interface functions.
%%% ---------------------------------------------------

behaviour_info(callbacks) ->
    [{init,1},{handle_event,3},{handle_sync_event,4},{handle_info,3},
     {terminate,3},{code_change,4}, {print_state,1}];
behaviour_info(_Other) ->
    undefined.

%%% ---------------------------------------------------
%%% Starts a generic state machine.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, atom()}
%%%    Mod  ::= atom(), callback module implementing the 'real' fsm
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% ---------------------------------------------------
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).


send_event({global, Name}, Event) ->
    catch global:send(Name, {'$gen_event', Event}),
    ok;
send_event(Name, Event) ->
    Name ! {'$gen_event', Event},
    ok.

sync_send_event(Name, Event) ->
    case catch gen:call(Name, '$gen_sync_event', Event) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, sync_send_event, [Name, Event]}})
    end.

sync_send_event(Name, Event, Timeout) ->
    case catch gen:call(Name, '$gen_sync_event', Event, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, sync_send_event, [Name, Event, Timeout]}})
    end.

send_all_state_event({global, Name}, Event) ->
    catch global:send(Name, {'$gen_all_state_event', Event}),
    ok;
send_all_state_event(Name, Event) ->
    Name ! {'$gen_all_state_event', Event},
    ok.

sync_send_all_state_event(Name, Event) ->
    case catch gen:call(Name, '$gen_sync_all_state_event', Event) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, sync_send_all_state_event, [Name, Event]}})
    end.

sync_send_all_state_event(Name, Event, Timeout) ->
    case catch gen:call(Name, '$gen_sync_all_state_event', Event, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, sync_send_all_state_event,
			   [Name, Event, Timeout]}})
    end.

%% Designed to be only callable within one of the callbacks
%% hence using the self() of this instance of the process.
%% This is to ensure that timers don't go astray in global
%% e.g. when straddling a failover, or turn up in a restarted
%% instance of the process.

%% Returns Ref, sends event {timeout,Ref,Msg} after Time 
%% to the (then) current state.
start_timer(Time, Msg) ->
    erlang:start_timer(Time, self(), {'$gen_timer', Msg}).

%% Returns Ref, sends Event after Time to the (then) current state.
send_event_after(Time, Event) ->
    erlang:start_timer(Time, self(), {'$gen_event', Event}).

%% Returns the remaing time for the timer if Ref referred to 
%% an active timer/send_event_after, false otherwise.
cancel_timer(Ref) ->
    case erlang:cancel_timer(Ref) of
	false ->
	    receive {timeout, Ref, _} -> 0
	    after 0 -> false 
	    end;
	RemainingTime ->
	    RemainingTime
    end.

%% enter_loop/4,5,6
%% Makes an existing process into a gen_fsm.
%% The calling process will enter the gen_fsm receive loop and become a
%% gen_fsm process.
%% The process *must* have been started using one of the start functions
%% in proc_lib, see proc_lib(3).
%% The user is responsible for any initialization of the process,
%% including registering a name for it.
enter_loop(Mod, Options, StateName, StateData) ->
    enter_loop(Mod, Options, StateName, StateData, self(), infinity).

enter_loop(Mod, Options, StateName, StateData, ServerName = {_,_}) ->
    enter_loop(Mod, Options, StateName, StateData, ServerName,infinity);
enter_loop(Mod, Options, StateName, StateData, Timeout) ->
    enter_loop(Mod, Options, StateName, StateData, self(), Timeout).

enter_loop(Mod, Options, StateName, StateData, ServerName, Timeout) ->
    Name = get_proc_name(ServerName),
    Parent = get_parent(),
    Debug = gen:debug_options(Options),
    Limits = limit_options(Options),
    Queue = queue:new(),
    QueueLen = 0,
    loop(Parent, Name, StateName, StateData, Mod, Timeout, Debug,
	 Limits, Queue, QueueLen).

get_proc_name(Pid) when is_pid(Pid) ->
    Pid;
get_proc_name({local, Name}) ->
    case process_info(self(), registered_name) of
	{registered_name, Name} ->
	    Name;
	{registered_name, _Name} ->
	    exit(process_not_registered);
	[] ->
	    exit(process_not_registered)
    end;
get_proc_name({global, Name}) ->
    case global:safe_whereis_name(Name) of
	undefined ->
	    exit(process_not_registered_globally);
	Pid when Pid==self() ->
	    Name;
	_Pid ->
	    exit(process_not_registered_globally)
    end.

get_parent() ->
    case get('$ancestors') of
	[Parent | _] when is_pid(Parent) ->
	    Parent;
	[Parent | _] when is_atom(Parent) ->
	    name_to_pid(Parent);
	_ ->
	    exit(process_was_not_started_by_proc_lib)
    end.

name_to_pid(Name) ->
    case whereis(Name) of
	undefined ->
	    case global:safe_whereis_name(Name) of
		undefined ->
		    exit(could_not_find_registerd_name);
		Pid ->
		    Pid
	    end;
	Pid ->
	    Pid
    end.

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = name(Name0),
    Debug = gen:debug_options(Options),
    Limits = limit_options(Options),
    Queue = queue:new(),
    QueueLen = 0,
    case catch Mod:init(Args) of
	{ok, StateName, StateData} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, StateName, StateData, Mod, infinity, Debug, Limits, Queue, QueueLen);
	{ok, StateName, StateData, Timeout} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, StateName, StateData, Mod, Timeout, Debug, Limits, Queue, QueueLen);
	{stop, Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.

name({local,Name}) -> Name;
name({global,Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

%%-----------------------------------------------------------------
%% The MAIN loop
%%-----------------------------------------------------------------
loop(Parent, Name, StateName, StateData, Mod, hibernate, Debug,
     Limits, Queue, QueueLen)
  when QueueLen > 0 ->
    case queue:out(Queue) of
	{{value, Msg}, Queue1} ->
	    decode_msg(Msg, Parent, Name, StateName, StateData, Mod, hibernate,
		       Debug, Limits, Queue1, QueueLen - 1, false);
	{empty, _} ->
	    Reason = internal_queue_error,
	    error_info(Mod, Reason, Name, hibernate, StateName, StateData, Debug),
	    exit(Reason)
    end;
loop(Parent, Name, StateName, StateData, Mod, hibernate, Debug,
     Limits, _Queue, _QueueLen) ->
    proc_lib:hibernate(?MODULE,wake_hib,
		       [Parent, Name, StateName, StateData, Mod,
			Debug, Limits]);
%% First we test if we have reach a defined limit ...
loop(Parent, Name, StateName, StateData, Mod, Time, Debug,
     Limits, Queue, QueueLen) ->
    try 	
	message_queue_len(Limits, QueueLen)
	%% TODO: We can add more limit checking here...
    catch
	{process_limit, Limit} ->
	    Reason = {process_limit, Limit},
	    Msg = {'EXIT', Parent, {error, {process_limit, Limit}}},
	    terminate(Reason, Name, Msg, Mod, StateName, StateData, Debug)
    end,
    process_message(Parent, Name, StateName, StateData,
		    Mod, Time, Debug, Limits, Queue, QueueLen).
%% ... then we can process a new message:
process_message(Parent, Name, StateName, StateData, Mod, Time, Debug,
		Limits, Queue, QueueLen) ->
    {Msg, Queue1, QueueLen1} = collect_messages(Queue, QueueLen, Time),
    decode_msg(Msg,Parent, Name, StateName, StateData, Mod, Time,
	       Debug, Limits, Queue1, QueueLen1, false).

collect_messages(Queue, QueueLen, Time) ->
    receive
	Input ->
	    case Input of
		{'EXIT', _Parent, priority_shutdown} ->
		    {Input, Queue, QueueLen};
		_ ->
		    collect_messages(
		      queue:in(Input, Queue), QueueLen + 1, Time)
	    end
    after 0 ->
	    case queue:out(Queue) of
		{{value, Msg}, Queue1} ->
		    {Msg, Queue1, QueueLen - 1};
		{empty, _} ->
		    receive
			Input ->
			    {Input, Queue, QueueLen}
		    after Time ->
			    {{'$gen_event', timeout}, Queue, QueueLen}
		    end
	    end
    end.


wake_hib(Parent, Name, StateName, StateData, Mod, Debug,
	 Limits) ->
    Msg = receive
	      Input ->
		  Input
	  end,
    Queue = queue:new(),
    QueueLen = 0,
    decode_msg(Msg, Parent, Name, StateName, StateData, Mod, hibernate,
	       Debug, Limits, Queue, QueueLen, true).

decode_msg(Msg,Parent, Name, StateName, StateData, Mod, Time, Debug,
	   Limits, Queue, QueueLen, Hib) ->
    put('$internal_queue_len', QueueLen),
    case Msg of
        {system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, StateName, StateData,
				   Mod, Time, Limits, Queue, QueueLen], Hib);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Name, Msg, Mod, StateName, StateData, Debug);
	_Msg when Debug == [] ->
	    handle_msg(Msg, Parent, Name, StateName, StateData,
		       Mod, Time, Limits, Queue, QueueLen);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
				      {Name, StateName}, {in, Msg}),
	    handle_msg(Msg, Parent, Name, StateName, StateData,
		       Mod, Time, Debug1, Limits, Queue, QueueLen)
    end.

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [Name, StateName, StateData,
				Mod, Time, Limits, Queue, QueueLen]) ->
    loop(Parent, Name, StateName, StateData, Mod, Time, Debug,
	 Limits, Queue, QueueLen).

system_terminate(Reason, _Parent, Debug,
		 [Name, StateName, StateData, Mod, _Time, _Limits]) ->
    terminate(Reason, Name, [], Mod, StateName, StateData, Debug).

system_code_change([Name, StateName, StateData, Mod, Time,
		    Limits, Queue, QueueLen],
		   _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, StateName, StateData, Extra) of
	{ok, NewStateName, NewStateData} ->
	    {ok, [Name, NewStateName, NewStateData, Mod, Time,
		  Limits, Queue, QueueLen]};
	Else -> Else
    end.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, {Name, StateName}) ->
    case Msg of
	{'$gen_event', Event} ->
	    io:format(Dev, "*DBG* ~p got event ~p in state ~w~n",
		      [Name, Event, StateName]);
	{'$gen_all_state_event', Event} ->
	    io:format(Dev,
		      "*DBG* ~p got all_state_event ~p in state ~w~n",
		      [Name, Event, StateName]);
	{timeout, Ref, {'$gen_timer', Message}} ->
	    io:format(Dev,
		      "*DBG* ~p got timer ~p in state ~w~n",
		      [Name, {timeout, Ref, Message}, StateName]);
	{timeout, _Ref, {'$gen_event', Event}} ->
	    io:format(Dev,
		      "*DBG* ~p got timer ~p in state ~w~n",
		      [Name, Event, StateName]);
	_ ->
	    io:format(Dev, "*DBG* ~p got ~p in state ~w~n",
		      [Name, Msg, StateName])
    end;
print_event(Dev, {out, Msg, To, StateName}, Name) ->
    io:format(Dev, "*DBG* ~p sent ~p to ~w~n"
	           "      and switched to state ~w~n",
	      [Name, Msg, To, StateName]);
print_event(Dev, return, {Name, StateName}) ->
    io:format(Dev, "*DBG* ~p switched to state ~w~n",
	      [Name, StateName]).

relay_messages(MRef, TRef, Clone, Queue) ->
    lists:foreach(
      fun(Msg) -> Clone ! Msg end,
      queue:to_list(Queue)),
    relay_messages(MRef, TRef, Clone).

relay_messages(MRef, TRef, Clone) ->
    receive
	{'DOWN', MRef, process, Clone, Reason} ->
	    Reason;
	{'EXIT', _Parent, _Reason} ->
	    {migrated, Clone};
	{timeout, TRef, timeout} ->
	    {migrated, Clone};
	Msg ->
	    Clone ! Msg,
	    relay_messages(MRef, TRef, Clone)
    end.

handle_msg(Msg, Parent, Name, StateName, StateData, Mod, _Time,
	   Limits, Queue, QueueLen) -> %No debug here
    From = from(Msg),
    case catch dispatch(Msg, Mod, StateName, StateData) of
	{next_state, NStateName, NStateData} ->	    
	    loop(Parent, Name, NStateName, NStateData,
		 Mod, infinity, [], Limits, Queue, QueueLen);
	{next_state, NStateName, NStateData, Time1} ->
	    loop(Parent, Name, NStateName, NStateData, Mod, Time1, [],
		 Limits, Queue, QueueLen);
        {reply, Reply, NStateName, NStateData} when From =/= undefined ->
	    reply(From, Reply),
	    loop(Parent, Name, NStateName, NStateData,
		 Mod, infinity, [], Limits, Queue, QueueLen);
        {reply, Reply, NStateName, NStateData, Time1} when From =/= undefined ->
	    reply(From, Reply),
	    loop(Parent, Name, NStateName, NStateData, Mod, Time1, [],
		 Limits, Queue, QueueLen);
	{migrate, NStateData, {Node, M, F, A}, Time1} ->
	    Reason = case catch rpc:call(Node, M, F, A, 5000) of
			 {badrpc, _} = Err ->
			     {migration_error, Err};
			 {'EXIT', _} = Err ->
			     {migration_error, Err};
			 {error, _} = Err ->
			     {migration_error, Err};
			 {ok, Clone} ->
			     process_flag(trap_exit, true),
			     MRef = erlang:monitor(process, Clone),
			     TRef = erlang:start_timer(Time1, self(), timeout),
			     relay_messages(MRef, TRef, Clone, Queue);
			 Reply ->
			     {migration_error, {bad_reply, Reply}}
		     end,
	    terminate(Reason, Name, Msg, Mod, StateName, NStateData, []);
	{stop, Reason, NStateData} ->
	    terminate(Reason, Name, Msg, Mod, StateName, NStateData, []);
	{stop, Reason, Reply, NStateData} when From =/= undefined ->
	    {'EXIT', R} = (catch terminate(Reason, Name, Msg, Mod,
					   StateName, NStateData, [])),
	    reply(From, Reply),
	    exit(R);
	{'EXIT', What} ->
	    terminate(What, Name, Msg, Mod, StateName, StateData, []);
	Reply ->
	    terminate({bad_return_value, Reply},
		      Name, Msg, Mod, StateName, StateData, [])
    end.

handle_msg(Msg, Parent, Name, StateName, StateData,
	   Mod, _Time, Debug, Limits, Queue, QueueLen) ->
    From = from(Msg),
    case catch dispatch(Msg, Mod, StateName, StateData) of
	{next_state, NStateName, NStateData} ->
	    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
				      {Name, NStateName}, return),
	    loop(Parent, Name, NStateName, NStateData,
		 Mod, infinity, Debug1, Limits, Queue, QueueLen);
	{next_state, NStateName, NStateData, Time1} ->
	    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
				      {Name, NStateName}, return),
	    loop(Parent, Name, NStateName, NStateData,
		 Mod, Time1, Debug1, Limits, Queue, QueueLen);
        {reply, Reply, NStateName, NStateData} when From =/= undefined ->
	    Debug1 = reply(Name, From, Reply, Debug, NStateName),
	    loop(Parent, Name, NStateName, NStateData,
		 Mod, infinity, Debug1, Limits, Queue, QueueLen);
        {reply, Reply, NStateName, NStateData, Time1} when From =/= undefined ->
	    Debug1 = reply(Name, From, Reply, Debug, NStateName),
	    loop(Parent, Name, NStateName, NStateData,
		 Mod, Time1, Debug1, Limits, Queue, QueueLen);
	{migrate, NStateData, {Node, M, F, A}, Time1} ->
	    Reason = case catch rpc:call(Node, M, F, A, Time1) of
			 {badrpc, R} ->
			     {migration_error, R};
			 {'EXIT', R} ->
			     {migration_error, R};
			 {error, R} ->
			     {migration_error, R};
			 {ok, Clone} ->
			     process_flag(trap_exit, true),
			     MRef = erlang:monitor(process, Clone),
			     TRef = erlang:start_timer(Time1, self(), timeout),
			     relay_messages(MRef, TRef, Clone, Queue);
			 Reply ->
			     {migration_error, {bad_reply, Reply}}
		     end,
	    terminate(Reason, Name, Msg, Mod, StateName, NStateData, Debug);
	{stop, Reason, NStateData} ->
	    terminate(Reason, Name, Msg, Mod, StateName, NStateData, Debug);
	{stop, Reason, Reply, NStateData} when From =/= undefined ->
	    {'EXIT', R} = (catch terminate(Reason, Name, Msg, Mod,
					   StateName, NStateData, Debug)),
	    reply(Name, From, Reply, Debug, StateName),
	    exit(R);
	{'EXIT', What} ->
	    terminate(What, Name, Msg, Mod, StateName, StateData, Debug);
	Reply ->
	    terminate({bad_return_value, Reply},
		      Name, Msg, Mod, StateName, StateData, Debug)
    end.

dispatch({'$gen_event', Event}, Mod, StateName, StateData) ->
    Mod:StateName(Event, StateData);
dispatch({'$gen_all_state_event', Event}, Mod, StateName, StateData) ->
    Mod:handle_event(Event, StateName, StateData);
dispatch({'$gen_sync_event', From, Event}, Mod, StateName, StateData) ->
    Mod:StateName(Event, From, StateData);
dispatch({'$gen_sync_all_state_event', From, Event},
	 Mod, StateName, StateData) ->
    Mod:handle_sync_event(Event, From, StateName, StateData);
dispatch({timeout, Ref, {'$gen_timer', Msg}}, Mod, StateName, StateData) ->
    Mod:StateName({timeout, Ref, Msg}, StateData);
dispatch({timeout, _Ref, {'$gen_event', Event}}, Mod, StateName, StateData) ->
    Mod:StateName(Event, StateData);
dispatch(Info, Mod, StateName, StateData) ->
    Mod:handle_info(Info, StateName, StateData).

from({'$gen_sync_event', From, _Event}) -> From;
from({'$gen_sync_all_state_event', From, _Event}) -> From;
from(_) -> undefined.

%% Send a reply to the client.
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

reply(Name, {To, Tag}, Reply, Debug, StateName) ->
    reply({To, Tag}, Reply),
    sys:handle_debug(Debug, {?MODULE, print_event}, Name,
		     {out, Reply, To, StateName}).

%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

terminate(Reason, Name, Msg, Mod, StateName, StateData, Debug) ->
    case catch Mod:terminate(Reason, StateName, StateData) of
	{'EXIT', R} ->
	    error_info(Mod, R, Name, Msg, StateName, StateData, Debug),
	    exit(R);
	_ ->
	    case Reason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
		priority_shutdown ->
		    %% Priority shutdown should be considered as
		    %% shutdown by SASL
		    exit(shutdown);
		{process_limit, _Limit} ->
		    exit(Reason);
		{migrated, _Clone} ->
		    exit(normal);
		_ ->
		    error_info(Mod, Reason, Name, Msg, StateName, StateData, Debug),
		    exit(Reason)
	    end
    end.

error_info(Mod, Reason, Name, Msg, StateName, StateData, Debug) ->
    Reason1 = 
	case Reason of
	    {undef,[{M,F,A}|MFAs]} ->
		case code:is_loaded(M) of
		    false ->
			{'module could not be loaded',[{M,F,A}|MFAs]};
		    _ ->
			case erlang:function_exported(M, F, length(A)) of
			    true ->
				Reason;
			    false ->
				{'function not exported',[{M,F,A}|MFAs]}
			end
		end;
	    _ ->
		Reason
	end,
    StateToPrint = case erlang:function_exported(Mod, print_state, 1) of
      true -> (catch Mod:print_state(StateData));
      false -> StateData
    end,
    Str = "** State machine ~p terminating \n" ++
	get_msg_str(Msg) ++
	"** When State == ~p~n"
        "**      Data  == ~p~n"
        "** Reason for termination = ~n** ~p~n",
    format(Str, [Name, get_msg(Msg), StateName, StateToPrint, Reason1]),
    sys:print_log(Debug),
    ok.

get_msg_str({'$gen_event', _Event}) ->
    "** Last event in was ~p~n";
get_msg_str({'$gen_sync_event', _Event}) ->
    "** Last sync event in was ~p~n";
get_msg_str({'$gen_all_state_event', _Event}) ->
    "** Last event in was ~p (for all states)~n";
get_msg_str({'$gen_sync_all_state_event', _Event}) ->
    "** Last sync event in was ~p (for all states)~n";
get_msg_str({timeout, _Ref, {'$gen_timer', _Msg}}) ->
    "** Last timer event in was ~p~n";
get_msg_str({timeout, _Ref, {'$gen_event', _Msg}}) ->
    "** Last timer event in was ~p~n";
get_msg_str(_Msg) ->
    "** Last message in was ~p~n".

get_msg({'$gen_event', Event}) -> Event;
get_msg({'$gen_sync_event', Event}) -> Event;
get_msg({'$gen_all_state_event', Event}) -> Event;
get_msg({'$gen_sync_all_state_event', Event}) -> Event;
get_msg({timeout, Ref, {'$gen_timer', Msg}}) -> {timeout, Ref, Msg};
get_msg({timeout, _Ref, {'$gen_event', Event}}) -> Event;
get_msg(Msg) -> Msg.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, StateName, StateData, Mod, _Time]] =
	StatusData,
    NameTag = if is_pid(Name) ->
		      pid_to_list(Name);
		 is_atom(Name) ->
		      Name
	      end,
    Header = lists:concat(["Status for state machine ", NameTag]),
    Log = sys:get_debug(log, Debug, []),
    Specfic = 
	case erlang:function_exported(Mod, format_status, 2) of
	    true ->
		case catch Mod:format_status(Opt,[PDict,StateData]) of
		    {'EXIT', _} -> [{data, [{"StateData", StateData}]}];
		    Else -> Else
		end;
	    _ ->
		[{data, [{"StateData", StateData}]}]
	end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", Log},
	     {"StateName", StateName}]} |
     Specfic].

%%-----------------------------------------------------------------
%% Resources limit management
%%-----------------------------------------------------------------
%% Extract know limit options
limit_options(Options) ->
    limit_options(Options, #limits{}).
limit_options([], Limits) ->
    Limits;
%% Maximum number of messages allowed in the process message queue
limit_options([{max_queue,N}|Options], Limits) 
  when is_integer(N) ->
    NewLimits = Limits#limits{max_queue=N},
    limit_options(Options, NewLimits);
limit_options([_|Options], Limits) ->
    limit_options(Options, Limits).

%% Throw max_queue if we have reach the max queue size
%% Returns ok otherwise
message_queue_len(#limits{max_queue = undefined}, _QueueLen) ->
    ok;
message_queue_len(#limits{max_queue = MaxQueue}, QueueLen) ->
    Pid = self(),
    case process_info(Pid, message_queue_len) of
        {message_queue_len, N} when N + QueueLen > MaxQueue ->
	    throw({process_limit, {max_queue, N + QueueLen}});
	_ ->
	    ok
    end.
