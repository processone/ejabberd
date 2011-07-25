%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%% This module launch clients (ts_client module) given a number of
%%% clients and the intensity of the arrival process (intensity =
%%% inverse of the mean of inter arrival). The arrival process is a
%%% Poisson Process (ie, inter-arrivals are independant and exponential)


-module(ts_launcher).
-created('Date: 2000/10/23 12:09:57 nniclausse ').
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").

% wait up to 10ms after an error
-define(NEXT_AFTER_FAILED_TIMEOUT, 10).
-define(DIE_DELAY, 5000).

-behaviour(gen_fsm). %% a primitive gen_fsm with two state: launcher and wait

%% External exports
-export([start/0, launch/1, set_static_users/1]).
-export([set_warm_timeout/1]).

%% gen_fsm callbacks
-export([init/1, launcher/2,  wait/2, wait_static/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {nusers,
                phases =[],
                myhostname,
                intensity,
                static_done = false,
                started_users = 0,
                phase_nusers,   % total number of users to start in the current phase
                phase_duration, % expected phase duration
                phase_start,    % timestamp
                start_date,
                short_timeout = ?short_timeout,
                maxusers %% if maxusers are currently active, launch a
                         %% new beam to handle the new users
               }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start/0
%%--------------------------------------------------------------------
start() ->
    ?LOG("starting ~n", ?INFO),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: launch/1
%%--------------------------------------------------------------------
%% Start clients with given interarrival (can be empty list)
launch({Node, Arrivals, Seed}) ->
    ?LOGF("starting on node ~p~n",[[Node]], ?INFO),
    gen_fsm:send_event({?MODULE, Node}, {launch, Arrivals, Seed});

% same erlang beam case
launch({Node, Host, Arrivals, Seed}) ->
    ?LOGF("starting on node ~p~n",[[Node]], ?INFO),
    gen_fsm:send_event({?MODULE, Node}, {launch, Arrivals, atom_to_list(Host), Seed}).

%% Start clients with given interarrival (can be empty list)
set_static_users({Node,Value}) ->
    ?LOGF("Substract static users number to max: ~p~n",[Value], ?DEB),
    gen_fsm:send_event({?MODULE, Node}, {static, Value}).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, MyHostName} = ts_utils:node_to_hostname(node()),
    ts_launcher_mgr:alive(dynamic),
    {ok, wait, #state{myhostname=MyHostName}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait({launch, Args, Hostname, Seed}, State) ->
    wait({launch, Args, Seed}, State#state{myhostname = Hostname});

%% starting without configuration. We must ask the config server for
%% the configuration of this launcher.
wait({launch, [], Seed}, State=#state{static_done=Static_done}) ->
    ts_utils:init_seed(Seed),
    MyHostName = State#state.myhostname,
    ?LOGF("Launch msg receive (~p)~n",[MyHostName], ?NOTICE),
    ts_launcher_mgr:check_registered(),
    case ts_config_server:get_client_config(MyHostName) of
        {ok, {[{Intensity, Users}| Rest], StartDate, Max}} ->
            Duration = Users/Intensity,
            ?LOGF("Expected duration of first phase: ~p sec ~n",[Duration/1000], ?NOTICE),
            NewState = State#state{phases         = Rest,
                                   nusers         = Users,
                                   phase_nusers   = Users,
                                   start_date     = StartDate,
                                   phase_duration = Duration,
                                   intensity      = Intensity, maxusers=Max },
            case Static_done of
                true  ->
                    wait_static({static, 0}, NewState);
                false ->
                    {next_state,wait_static,NewState}
            end;
        {ok,{[],_,_}} -> % no random users, only static.
            {stop, normal, State}
    end;

%% start with a already known configuration. This case occurs when a
%% beam is started by a launcher (maxclients reached)
wait({launch, {[{Intensity, Users}| Rest], Max}, Seed}, State) ->
    ?LOGF("Starting with ~p users to do in the current phase (max is ~p)~n",
          [Users, Max],?DEB),
    ts_utils:init_seed(Seed),
    Duration = Users/Intensity,
    ?LOGF("Expected duration of phase: ~p sec ~n",[Duration/1000], ?NOTICE),
    ts_launcher_mgr:check_registered(),
    {next_state, launcher, State#state{phases = Rest, nusers = Users,
                                       phase_nusers = Users,
                                       phase_duration=Duration,
                                       phase_start = now(),
                                       intensity = Intensity, maxusers=Max},
     State#state.short_timeout};
wait({static,0}, State) ->
    %% static launcher has no work to do, do not wait for him.
    ?LOG("Wow, static launcher is already sending me a msg, don't forget it ~n", ?INFO),
    {next_state, wait, State#state{static_done=true}}.

wait_static({static, Static}, State=#state{maxusers=Max,intensity=Intensity,
                                           nusers=Users,start_date=StartDate}) when is_integer(Static) ->
    %% add ts_stats:exponential(Intensity) to start time to avoid
    %% simultaneous start of users when a lot of client beams is
    %% used. Also, avoid too long delay, so use a maximum delay
    WarmTimeout = set_warm_timeout(StartDate)+round(ts_stats:exponential(Intensity)),
    Warm = lists:min([WarmTimeout,?config(max_warm_delay)]),
    ?LOGF("Activate launcher (~p users) in ~p msec ~n",[Users, Warm], ?NOTICE),
    PhaseStart = ts_utils:add_time(now(), Warm div 1000),
    NewMax = case Max > Static of
               true  ->
                     Max-Static;
               false ->
                   ?LOG("Warning: more static users than maximum users per beam !~n",?WARN),
                   1 % will fork a new beam as soon a one user is started
           end,
    ?LOGF("Set maximum users per beam to ~p~n",[NewMax],?DEB),
    {next_state,launcher,State#state{ phase_start = PhaseStart,
                                      maxusers = NewMax }, Warm}.


launcher(_Event, State=#state{nusers = 0, phases = [] }) ->
    ?LOG("no more clients to start, stop  ~n",?INFO),
    {stop, normal, State};

launcher(timeout, State=#state{nusers    = Users,
                               phase_nusers = PhaseUsers,
                               phases     = Phases,
                               started_users = Started,
                               intensity  = Intensity}) ->
    BeforeLaunch = now(),
    case do_launch({Intensity,State#state.myhostname}) of
        {ok, Wait} ->
            case check_max_raised(State) of
                true ->
                    {stop, normal, State};
                false->
                    Duration = ts_utils:elapsed(State#state.phase_start, BeforeLaunch),
                    case change_phase(Users-1, Phases, Duration,
                                      {State#state.phase_duration, PhaseUsers}) of
                        {change, NewUsers, NewIntensity, Rest} ->
                            ts_mon:add({ count, newphase }),
                            PhaseLength = NewUsers/NewIntensity,
                            ?LOGF("Start a new arrival phase (~p ~p); expected duration=~p sec~n",
                                  [NewUsers, NewIntensity, Duration/1000], ?NOTICE),
                            {next_state,launcher,State#state{phases = Rest,
                                                             nusers = NewUsers,
                                                             phase_nusers = NewUsers,
                                                             phase_duration=PhaseLength,
                                                             phase_start = now(),
                                                             intensity = NewIntensity},
                             round(Wait)};
                        {stop} ->
                            {stop, normal, State};
                        {continue} ->
                            Now=now(),
                            LaunchDuration = ts_utils:elapsed(BeforeLaunch, Now),
                            %% to keep the rate of new users as expected,
                            %% remove the time to launch a client to the next
                            %% wait.
                            NewWait = case Wait > LaunchDuration of
                                          true -> round(Wait - LaunchDuration);
                                          false -> 0
                                      end,
                            ?DebugF("Real Wait =~p ~n", [NewWait]),
                            {next_state,launcher,State#state{nusers = Users-1, started_users=Started+1} , NewWait}
                    end
            end;
        error ->
            % retry with the same user, wait randomly a few msec
            RndWait = random:uniform(?NEXT_AFTER_FAILED_TIMEOUT),
            {next_state,launcher,State , RndWait}
    end.


%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, _StateName, _StateData) ->
    ?LOGF("launcher terminating for reason~p~n",[Reason], ?INFO),
    ts_launcher_mgr:die(dynamic),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Func: change_phase/4
%%% Purpose: decide if we need to change phase (if current users is
%%%          reached or if max duration is reached)
%%% ----------------------------------------------------------------------
change_phase(N, [{NewIntensity, NewUsers}|Rest], _, _)  when N < 1 ->
    {change, NewUsers, NewIntensity, Rest};
change_phase(N, [], _, _) when N < 1 ->
    ?LOG("This was the last phase, wait for connected users to finish their session~n",?NOTICE),
    {stop};
change_phase(N,NewPhases,Current,{Total, PhaseUsers}) when Current>Total ->
    ?LOGF("Check phase: ~p ~p~n",[N,PhaseUsers],?DEB),
    Percent = 100*N/PhaseUsers,
    case {Percent > ?MAX_PHASE_EXCEED_PERCENT, N > ?MAX_PHASE_EXCEED_NUSERS} of
        {true,true} ->
            ?LOGF("Phase duration exceeded, more than ~p% (~.1f%) of users were not launched in time (~p users), tsung may be overloaded !~n",
                  [?MAX_PHASE_EXCEED_PERCENT,Percent,N],?WARN);
        {_,_} ->
            ?LOGF("Phase duration exceeded, but not all users were launched (~p users, ~.1f% of phase)~n",
                  [N, Percent],?NOTICE)
    end,
    case NewPhases of
        [{NewIntensity,NewUsers}|Rest] ->
            {change, NewUsers, NewIntensity, Rest};
        [] ->
            ?LOG("This was the last phase, wait for connected users to finish their session~n",?NOTICE),
            {stop}
    end;
change_phase(_N, _, _Current, {_Total, _}) ->
    {continue}.

%%%----------------------------------------------------------------------
%%% Func: check_max_raised/1
%%%----------------------------------------------------------------------
check_max_raised(State=#state{phases=Phases,maxusers=Max,nusers=Users,
                              started_users=Started,
                              intensity=Intensity}) when Started >= Max ->
    ActiveClients =  ts_client_sup:active_clients(),
    ?DebugF("Current active clients on beam: ~p (max is ~p)~n", [ActiveClients, State#state.maxusers]),
    case ActiveClients >= Max of
        true ->
            ?LOG("Max number of concurrent clients reached, must start a new beam~n", ?NOTICE),
            Args = case Users of
                       0 ->  Phases;
                       _ -> [{Intensity,Users-1}|Phases]
                   end,
            ts_config_server:newbeam(list_to_atom(State#state.myhostname), {Args, Max}),
            true;
        false ->
            ?DebugF("Current clients on beam: ~p~n", [ActiveClients]),
            false
    end;
check_max_raised(_State) -> % number of started users less than max, no need to check
    ?DebugF("Current started clients on beam: ~p (max is ~p)~n", [_State#state.started_users, _State#state.maxusers]),
    false.

%%%----------------------------------------------------------------------
%%% Func: do_launch/1
%%%----------------------------------------------------------------------
do_launch({Intensity, MyHostName})->
    %%Get one client
    %%set the profile of the client
    case catch ts_config_server:get_next_session(MyHostName) of
        {timeout, _ } ->
            ?LOG("get_next_session failed (timeout), skip this session !~n", ?ERR),
            ts_mon:add({ count, error_next_session }),
            error;
        {ok, Session} ->
            ts_client_sup:start_child(Session),
            X = ts_stats:exponential(Intensity),
            ?DebugF("client launched, wait ~p ms before launching next client~n",[X]),
            {ok, X};
        Error ->
            ?LOGF("get_next_session failed [~p], skip this session !~n", [Error],?ERR),
            error
    end.


set_warm_timeout(StartDate)->
    case ts_utils:elapsed(now(), StartDate) of
        WaitBeforeStart when WaitBeforeStart>0 ->
            round(WaitBeforeStart);
        _Neg ->
            ?LOG("Negative Warm timeout !!! Check if client "++
                 " machines are synchronized (ntp ?)~n"++
                 "Anyway, start launcher NOW! ~n", ?WARN),
            1
    end.
