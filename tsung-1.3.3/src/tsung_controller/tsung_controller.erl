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

-module(tsung_controller).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([start/2, start_phase/3, stop/1, stop_all/1, status/1]).
-behaviour(application).

-include("ts_profile.hrl").

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%%----------------------------------------------------------------------
start(_Type, _StartArgs) ->
    error_logger:tty(false),
    {ok, {LogDir, _Name}} = ts_utils:setsubdir(?config(log_file)),
    erlang:display("Log directory is: " ++ LogDir),
    LogFile = filename:join(LogDir, atom_to_list(node()) ++ ".log"),
    case  error_logger:logfile({open, LogFile }) of
        ok ->
            case ts_controller_sup:start_link(LogDir) of
                {ok, Pid} ->
                    {ok, Pid};
                Error ->
                    ?LOGF("Can't start ! ~p ~n",[Error], ?ERR),
                    Error
            end;
        {error, Reason} ->
            Msg = "Error while opening log file: " ,
            ?LOGF(Msg ++ " ~p ~n",[Reason], ?ERR),
            erlang:display(Msg ++ Reason),
            {error, Reason}
    end.

start_phase(load_config, _StartType, _PhaseArgs) ->
    case ts_config_server:read_config(?config(config_file)) of
        {error,Reason}->
            erlang:display(["Config Error, aborting ! ", Reason]),
            init:stop();
        ok -> ok
    end;
start_phase(start_os_monitoring, _StartType, _PhaseArgs) ->
    ts_os_mon:activate();
start_phase(start_clients, _StartType, _PhaseArgs) ->
    ts_mon:start_clients({?config(clients),
                          ?config(dump),
                          ?config(stats_backend)}).
%%----------------------------------------------------------------------
%% Func: status/1
%% Returns: any
%%----------------------------------------------------------------------
status([Host]) when is_atom(Host)->
    _List = net_adm:world_list([Host]),
    global:sync(),
    Msg = case catch ts_mon:status() of
              {Clients, Count, Connected, Interval, Phase} ->

                  S1 = io_lib:format("Tsung is running [OK]~n" ++
                                     " Current request rate:    ~p req/sec~n" ++
                                     " Current users:           ~p~n" ++
                                     " Current connected users: ~p ~n",
                                     [Count/Interval, Clients, Connected]),
                  {ok, Nodes, Ended_Beams} = ts_config_server:status(),
                  case {Phase, Nodes == Ended_Beams} of
                      {error, _} -> % newphase not initialised, first phase
                          S1 ++ " Current phase:           1";
                      {_, true} ->
                          S1 ++ " Current phase:           last, waiting for pending clients";
                      {{ok,P}, _} ->
                          NPhases = (P div Nodes) + 1,
                          io_lib:format("~s Current phase:        ~p",[S1,NPhases])
                  end;
              {'EXIT', {noproc, _}} ->
                  "Tsung is not started"
          end,
    io:format("~s~n",[Msg]).

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) ->
    stop.

%%----------------------------------------------------------------------
%% Func: stop_all/0
%% Returns: any
%%----------------------------------------------------------------------
stop_all(Arg) ->
    ts_utils:stop_all(Arg,'ts_mon').
