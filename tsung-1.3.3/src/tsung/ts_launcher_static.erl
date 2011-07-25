%%%  Copyright (C) 2009 Nicolas Niclausse
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

-module(ts_launcher_static).
-created('Date: 2009/03/10 19:09:57 nniclausse ').
-vc('$Id: ts_launcher.erl 968 2008-12-16 12:51:28Z nniclausse $ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").

-behaviour(gen_fsm). %% a primitive gen_fsm with two state: launcher and wait

%% External exports
-export([start/0, launch/1]).

%% gen_fsm callbacks
-export([init/1, launcher/2,  wait/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
                myhostname,
                users
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
%% Start clients with given session list
launch({Node, Sessions}) ->
    ?LOGF("starting on node ~p~n",[[Node]], ?INFO),
    gen_fsm:send_event({?MODULE, Node}, {launch, Sessions});

% same erlang beam case
launch({Node, Host, Sessions}) ->
    ?LOGF("starting on node ~p~n",[[Node]], ?INFO),
    gen_fsm:send_event({?MODULE, Node}, {launch, Sessions, atom_to_list(Host)}).


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
    ts_utils:init_seed(),
    {ok, MyHostName} = ts_utils:node_to_hostname(node()),
    ts_launcher_mgr:alive(static),
    {ok, wait, #state{myhostname=MyHostName}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
wait({launch, Args, Hostname}, State) ->
    wait({launch, Args}, State#state{myhostname = Hostname});
%% starting without configuration. We must ask the config server for
%% the configuration of this launcher.
wait({launch, []}, State) ->
    MyHostName = State#state.myhostname,
    ?LOGF("Launch msg receive (~p)~n",[MyHostName], ?NOTICE),
    ts_launcher_mgr:check_registered(),
    {ok,Users,Start} = ts_config_server:get_client_config(static,MyHostName),
    case Users of
        [{Wait,_}|_] ->
            Warm = ts_launcher:set_warm_timeout(Start),
            ts_launcher:set_static_users({node(),length(Users)}),
            ?LOGF("Activate launcher (~p static users) in ~p msec, first user after ~p ms ~n",[length(Users), Warm, Wait], ?NOTICE),
            {next_state,launcher,State#state{users = Users}, Warm + Wait};
        [] ->
            ?LOG("No static users, stop",?INFO),
            ts_launcher:set_static_users({node(),0}),
            {stop, normal, State}
    end.

launcher(timeout, State=#state{ users = [{OldWait,Session}|Users]}) ->
    BeforeLaunch = now(),
    ?LOGF("Launch static user using session ~p ~n", [Session],?DEB),
    do_launch({Session,State#state.myhostname}),
    Wait = set_waiting_time(BeforeLaunch, Users, OldWait),
    ?DebugF("Real Wait =~p ~n", [Wait]),
    case Users of
        [] ->
            ?LOG("no more clients to start ~n",?INFO),
            {stop, normal, State};
        _ ->
            {next_state,launcher,State#state{users=Users},Wait}
    end.

set_waiting_time(_Before, []        , _Previous) -> 0; % last user
set_waiting_time(Before , [{Next,_}|_], Previous)  ->
    LaunchDuration = ts_utils:elapsed(now(), Before),
    %% to keep the rate of new users as expected, remove the time to
    %% launch a client to the next wait.
    NewWait = Next - Previous - LaunchDuration,
    case NewWait > 0 of
        true  -> round(NewWait);
        false -> 0
    end.

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
    ?LOGF("launcher terminating for reason ~p~n",[Reason], ?INFO),
    ts_launcher_mgr:die(static),
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
%%% Func: do_launch/1
%%%----------------------------------------------------------------------
do_launch({ Session, MyHostName})->
    case catch ts_config_server:get_user_param(MyHostName) of
        {ok, {IPParam, Server, UserId}} ->
            ts_client_sup:start_child({Session, IPParam, Server, UserId}),
            ok;
        Error ->
            ?LOGF("get_next_session failed [~p], skip this session !~n", [Error],?ERR),
            ts_mon:add({ count, error_next_session }),
            error
    end.

