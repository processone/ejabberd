%%%----------------------------------------------------------------------
%%% File    : ejabberd_batch.erl
%%% Author  : Paweł Chmielowski <pawel@process-one.net>
%%% Purpose : Batch tasks manager
%%% Created : 8 mar 2022 by Paweł Chmielowski <pawel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026  ProcessOne
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

-module(ejabberd_batch).
-author("pawel@process-one.net").

-behaviour(gen_server).

-include("logger.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).
-export([register_task/5, task_status/1, abort_task/1]).

-define(SERVER, ?MODULE).

-record(state, {tasks = #{}}).
-record(task, {state = not_started, pid, steps, done_steps, last_message}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_task(Type, Steps, Rate, JobState, JobFun) ->
    gen_server:call(?MODULE, {register_task, Type, Steps, Rate, JobState, JobFun}).

task_status(Type) ->
    gen_server:call(?MODULE, {task_status, Type}).

abort_task(Type) ->
    gen_server:call(?MODULE, {abort_task, Type}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		  State :: #state{}) ->
		     {reply, Reply :: term(), NewState :: #state{}} |
		     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
		     {noreply, NewState :: #state{}} |
		     {noreply, NewState :: #state{}, timeout() | hibernate} |
		     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
		     {stop, Reason :: term(), NewState :: #state{}}).
handle_call({register_task, Type, Steps, Rate, JobState, JobFun}, _From, #state{tasks = Tasks} = State) ->
    case maps:get(Type, Tasks, #task{}) of
	#task{state = S} when S == completed; S == not_started; S == aborted; S == failed ->
	    Pid = spawn(fun() -> work_loop(Type, JobState, JobFun, Rate, erlang:monotonic_time(second), 0) end),
	    Tasks2 = maps:put(Type, #task{state = working, pid = Pid, steps = Steps, done_steps = 0}, Tasks),
	    {reply, ok, #state{tasks = Tasks2}};
	#task{state = working} ->
	    {reply, {error, in_progress}, State}
    end;
handle_call({task_status, Type}, _From, #state{tasks = Tasks} = State) ->
    case maps:get(Type, Tasks, none) of
	none ->
	    {reply, not_started, State};
	#task{state = not_started} ->
	    {reply, not_started, State};
	#task{state = failed, done_steps = Steps, last_message = Error} ->
	    {reply, {failed, Steps, Error}, State};
	#task{state = aborted, done_steps = Steps, last_message = Msg} ->
	    {reply, {aborted, Steps, Msg}, State};
	#task{state = working, done_steps = Steps, last_message = Msg} ->
	    {reply, {working, Steps, Msg}, State};
	#task{state = completed, done_steps = Steps, last_message = Msg} ->
	    {reply, {completed, Steps, Msg}, State}
    end;
handle_call({abort_task, Type}, _From, #state{tasks = Tasks} = State) ->
    case maps:get(Type, Tasks, none) of
	#task{state = working, pid = Pid} = T ->
	    Pid ! abort,
	    Tasks2 = maps:put(Type, T#task{state = aborted, pid = none}, Tasks),
	    {reply, aborted, State#state{tasks = Tasks2}};
	_ ->
	    {reply, not_started, State}
    end;
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({task_finished, Type, Pid, Msg}, #state{tasks = Tasks} = State) ->
    case maps:get(Type, Tasks, none) of
	#task{state = working, pid = Pid2} = T when Pid == Pid2  ->
	    Tasks2 = maps:put(Type, T#task{state = completed, pid = none, last_message = Msg}, Tasks),
	    {noreply, State#state{tasks = Tasks2}};
	_ ->
	    {noreply, State}
    end;
handle_cast({task_progress, Type, Pid, Count, Msg}, #state{tasks = Tasks} = State) ->
    case maps:get(Type, Tasks, none) of
	#task{state = working, pid = Pid2, done_steps = Steps} = T when Pid == Pid2  ->
	    Tasks2 = maps:put(Type, T#task{done_steps = Steps + Count, last_message = Msg}, Tasks),
	    {noreply, State#state{tasks = Tasks2}};
	_ ->
	    {noreply, State}
    end;
handle_cast({task_error, Type, Pid, Error}, #state{tasks = Tasks} = State) ->
    case maps:get(Type, Tasks, none) of
	#task{state = working, pid = Pid2} = T when Pid == Pid2  ->
	    Tasks2 = maps:put(Type, T#task{state = failed, last_message = Error}, Tasks),
	    {noreply, State#state{tasks = Tasks2}};
	_ ->
	    {noreply, State}
    end;
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		  Extra :: term()) ->
		     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

work_loop(Task, JobState, JobFun, Rate, StartDate, CurrentProgress) ->
    try JobFun(JobState) of
	{ok, _NewState, 0, Msg} ->
	    gen_server:cast(?MODULE, {task_finished, Task, self(), Msg});
	{ok, NewState, Count, Msg} ->
	    gen_server:cast(?MODULE, {task_progress, Task, self(), Count, Msg}),
	    NewProgress = CurrentProgress + Count,
	    SleepTime = case Rate of
			    infinity -> 0;
			    _ ->
				TimeSpent = erlang:monotonic_time(second) - StartDate,
				max(0, NewProgress/Rate*60 - TimeSpent)
			end,
	    receive
		abort -> ok
	    after round(SleepTime*1000) ->
		work_loop(Task, NewState, JobFun, Rate, StartDate, NewProgress)
	    end;
	{error, Error} ->
	    gen_server:cast(?MODULE, {task_error, Task, self(), Error})
    catch T:E:S ->
	gen_server:cast(?MODULE, {task_error, Task, self(), {internal_error, T, E, S}})
    end.
