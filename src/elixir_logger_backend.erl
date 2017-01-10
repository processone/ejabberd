%%%-------------------------------------------------------------------
%%% File    : elixir_logger_backend.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : This module bridges lager logs to Elixir Logger.
%%% Created : 9 March 2016 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%%-------------------------------------------------------------------

-module(elixir_logger_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {level = debug}).

init(Opts) ->
    Level = proplists:get_value(level, Opts, debug),
    State = #state{level = Level},
    {ok, State}.

%% @private
handle_event({log, LagerMsg}, State) ->
    #{mode := Mode, truncate := Truncate, level := MinLevel, utc_log := UTCLog} =  'Elixir.Logger.Config':'__data__'(),
    MsgLevel = severity_to_level(lager_msg:severity(LagerMsg)),
    case {lager_util:is_loggable(LagerMsg, lager_util:level_to_num(State#state.level), ?MODULE),
          'Elixir.Logger':compare_levels(MsgLevel, MinLevel)} of
        {_, lt}->
            {ok, State};
        {true, _} ->
            Metadata = normalize_pid(lager_msg:metadata(LagerMsg)),
            Message = 'Elixir.Logger.Utils':truncate(lager_msg:message(LagerMsg), Truncate),
            Timestamp = timestamp(lager_msg:timestamp(LagerMsg), UTCLog),
            GroupLeader = case proplists:get_value(pid, Metadata, self()) of
                              Pid when is_pid(Pid) ->
                                  erlang:process_info(self(), group_leader);
                              _ -> {group_leader, self()}
                          end,
            notify(Mode, {MsgLevel, GroupLeader, {'Elixir.Logger', Message, Timestamp, Metadata}}),
            {ok, State};
        _ ->
            {ok, State}            
    end;
handle_event(_Msg, State) ->
    {ok, State}.

%% @private
%% TODO Handle loglevels
handle_call(get_loglevel, State) ->
    {ok, lager_util:config_to_mask(State#state.level), State};
handle_call({set_loglevel, Config}, State) ->
    {ok, ok, State#state{level = Config}}.

%% @private
handle_info(_Msg, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

notify(sync, Msg)  ->
    gen_event:sync_notify('Elixir.Logger', Msg);
notify(async, Msg) ->
    gen_event:notify('Elixir.Logger', Msg).

normalize_pid(Metadata) ->
    case proplists:get_value(pid, Metadata) of
        Pid when is_pid(Pid) -> Metadata;
        Pid when is_list(Pid) ->
            M1 = proplists:delete(pid, Metadata),
            case catch erlang:list_to_pid(Pid) of
                {'EXIT', _} ->
                    M1;
                PidAsPid ->
                    [{pid, PidAsPid}|M1]
            end;
        _ ->
            proplists:delete(pid, Metadata)
    end.

%% Return timestamp with milliseconds
timestamp(Time, UTCLog) ->
    {_, _, Micro} = p1_time_compat:timestamp(),
    {Date, {Hours, Minutes, Seconds}} =
        case UTCLog of
            true  -> calendar:now_to_universal_time(Time);
            false -> calendar:now_to_local_time(Time)
        end,
    {Date, {Hours, Minutes, Seconds, Micro div 1000}}.
    

severity_to_level(debug) -> debug;
severity_to_level(info) -> info;
severity_to_level(notice) -> info;
severity_to_level(warning) -> warn;
severity_to_level(error) -> error;
severity_to_level(critical) -> error;
severity_to_level(alert) -> error;
severity_to_level(emergency) -> error.
