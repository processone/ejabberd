%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2003 IDEALX
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

-module(ts_controller_sup).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(LogDir) ->
    ?LOG("starting supervisor ...~n",?INFO),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LogDir]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
init([LogDir]) ->
    ?LOG("starting",?INFO),
    Config = {ts_config_server, {ts_config_server, start_link,
                                 [LogDir]}, transient, 2000,
              worker, [ts_config_server]},
    Mon = {ts_mon, {ts_mon, start, [LogDir]}, transient, 2000,
                 worker, [ts_mon]},
    Stats_Mon = {ts_stats_mon, {ts_stats_mon, start, []}, transient, 2000,
                 worker, [ts_stats_mon]},
    Request_Mon = {request, {ts_stats_mon, start, [request]}, transient, 2000,
                 worker, [ts_stats_mon]},
    Page_Mon = {page, {ts_stats_mon, start, [page]}, transient, 2000,
                 worker, [ts_stats_mon]},
    Connect_Mon = {connect, {ts_stats_mon, start, [connect]}, transient, 2000,
                 worker, [ts_stats_mon]},
    Transaction_Mon = {transaction, {ts_stats_mon, start, [transaction]},
                       transient, 2000, worker, [ts_stats_mon]},
    Match_Log = {ts_match_logger, {ts_match_logger, start, [LogDir]}, transient, 2000,
                 worker, [ts_match_logger]},
    ErlangSup   = {ts_erlang_mon_sup, {ts_os_mon_sup, start_link, [erlang]},
                    permanent, 2000, supervisor, [ts_os_mon_sup]},
    MuninSup   = {ts_munin_mon_sup, {ts_os_mon_sup, start_link, [munin]},
                    permanent, 2000, supervisor, [ts_os_mon_sup]},
    SNMPSup   = {ts_snmp_mon_sup, {ts_os_mon_sup, start_link, [snmp]},
                    permanent, 2000, supervisor, [ts_os_mon_sup]},
    Timer = {ts_timer, {ts_timer, start, [?config(nclients)]}, transient, 2000,
               worker, [ts_timer]},
    Msg  = {ts_msg_server, {ts_msg_server, start, []}, transient, 2000,
               worker, [ts_msg_server]},
    UserSup = {ts_user_server_sup,{ts_user_server_sup,start_link,[]},transient,2000,
                 supervisor,[ts_user_server_sup]},
    {ok,{{one_for_one,?retries,10},
         [Config, Mon, Stats_Mon, Request_Mon, Page_Mon, Connect_Mon, Transaction_Mon,
          Match_Log, Timer, Msg, UserSup, ErlangSup, MuninSup,SNMPSup]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

