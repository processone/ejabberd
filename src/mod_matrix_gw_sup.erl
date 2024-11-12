%%%----------------------------------------------------------------------
%%% Created :  1 May 2022 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
-module(mod_matrix_gw_sup).
-ifndef(OTP_BELOW_25).
-behaviour(supervisor).

%% API
-export([start/1, start_link/1, procname/1]).
%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start(Host) ->
    Spec = #{id => procname(Host),
	     start => {?MODULE, start_link, [Host]},
	     restart => permanent,
	     shutdown => infinity,
	     type => supervisor,
	     modules => [?MODULE]},
    supervisor:start_child(ejabberd_gen_mod_sup, Spec).

start_link(Host) ->
    Proc = procname(Host),
    supervisor:start_link({local, Proc}, ?MODULE, [Host]).

-spec procname(binary()) -> atom().
procname(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([Host]) ->
    S2SName = mod_matrix_gw_s2s:supervisor(Host),
    RoomName = mod_matrix_gw_room:supervisor(Host),
    Specs =
        [#{id => S2SName,
           start => {ejabberd_tmp_sup, start_link, [S2SName, mod_matrix_gw_s2s]},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [ejabberd_tmp_sup]},
         #{id => RoomName,
           start => {ejabberd_tmp_sup, start_link, [RoomName, mod_matrix_gw_room]},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [ejabberd_tmp_sup]},
         #{id => mod_matrix_gw:procname(Host),
           start => {mod_matrix_gw, start_link, [Host]},
           restart => permanent,
           shutdown => timer:minutes(1),
           type => worker,
           modules => [mod_matrix_gw]}],
    {ok, {{one_for_one, 10, 1}, Specs}}.
-endif.
