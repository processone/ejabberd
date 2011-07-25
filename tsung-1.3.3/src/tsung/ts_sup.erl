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

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

-module(ts_sup).

-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    ?LOG("starting supervisor ...~n",?INFO),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
init([]) ->
    ?LOG("starting",?INFO),

    ClientsSup   = {ts_client_sup, {ts_client_sup, start_link, []},
                    permanent, 2000, supervisor, [ts_client_sup]},
    Launcher        = {ts_launcher, {ts_launcher, start, []},
                    transient, 2000, worker, [ts_launcher]},
    StaticLauncher  = {ts_launcher_static, {ts_launcher_static, start, []},
                    transient, 2000, worker, [ts_launcher_static]},
    LauncherManager  = {ts_launcher_mgr, {ts_launcher_mgr, start, []},
                    transient, 2000, worker, [ts_launcher_mgr]},
    SessionCache = {ts_session_cache, {ts_session_cache, start, []},
                    transient, 2000, worker, [ts_session_cache]},
    MonCache = {ts_mon_cache, {ts_mon_cache, start, []},
                    transient, 2000, worker, [ts_mon_cache]},
    {ok,{{one_for_one,?retries,10}, [LauncherManager, SessionCache, MonCache,ClientsSup, StaticLauncher,Launcher  ]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

