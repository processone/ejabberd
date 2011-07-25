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

-module(ts_timer).
-vc('$Id$ ').
-author('jflecomte@IDEALX.com').
-modifiedby('nicolas@niclux.org').

-include("ts_profile.hrl").

-behaviour(gen_fsm).

%% Puropose:
%%  gen_fsm with 3 states:  receiver, ack, initialize
%%  External events: connected

%% External exports
-export([start/1, connected/1, config/1]).

%% gen_fsm callbacks
-export([init/1, initialize/2, receiver/2, ack/2, handle_event/3,
		 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {nclient=0, pidlist = []}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(NClients) ->
	?LOG("starting fsm timer",?INFO),
	gen_fsm:start_link({global, ?MODULE}, ?MODULE, [NClients], []).

config(NClients) ->
	?LOG("Configure fsm timer",?INFO),
	gen_fsm:send_event({global, ?MODULE}, {config, NClients}).

connected(Pid) ->
	gen_fsm:send_event({global, ?MODULE}, {connected, Pid}).

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
init(_Args) ->
	?LOG("starting timer",?INFO),
	{ok, initialize, #state{}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
initialize({config, Val}, State) ->
	{next_state, receiver, State#state{nclient=Val}}.


%% now all the clients are connected, let's start to ack them
receiver({connected, Pid}, #state{pidlist=List, nclient=1}) ->
	?LOG("All connected, global ack!",?NOTICE),
	{next_state, ack, #state{pidlist=[Pid|List],nclient=0}, 1};

%% receive a new connected mes
receiver({connected, Pid}, #state{pidlist=List, nclient=N}) ->
	?LOGF("New connected ~p (nclient=~p)",[Pid, N],?DEB),
	{next_state, receiver, #state{pidlist=List ++ [Pid], nclient=N-1},
	 ?config(clients_timeout)};

%% timeout event, now we start to send ack, by sending a timeout event immediatly
receiver(timeout, StateData) ->
	{next_state, ack, StateData,1}.

%% no more ack to send, stop
ack(timeout, #state{pidlist=[]}) ->
	{stop, normal, #state{}};

%% ack all pids
ack(timeout, #state{pidlist=L}) ->
	lists:foreach(fun(A)->ts_client:next({A}) end, L),
	{next_state, receiver, #state{pidlist=[]}}.
												

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
terminate(_Reason, _StateName, _StateData) ->
	?LOG("terminate timer",?INFO),
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



