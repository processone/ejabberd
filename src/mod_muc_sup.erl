%%%----------------------------------------------------------------------
%%% Created : 4 Jul 2019 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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
-module(mod_muc_sup).
-behaviour(supervisor).

%% API
-export([start/2, start_link/2, procname/1]).
%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start(Host, Opts) ->
    Spec = #{id => procname(Host),
	     start => {?MODULE, start_link, [Host, Opts]},
	     restart => permanent,
	     shutdown => infinity,
	     type => supervisor,
	     modules => [?MODULE]},
    supervisor:start_child(ejabberd_gen_mod_sup, Spec).

start_link(Host, Opts) ->
    Proc = procname(Host),
    supervisor:start_link({local, Proc}, ?MODULE, [Host, Opts]).

-spec procname(binary()) -> atom().
procname(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([Host, Opts]) ->
    Cores = erlang:system_info(logical_processors),
    Specs = [#{id => mod_muc:procname(Host, I),
	       start => {mod_muc, start_link, [Host, Opts, I]},
	       restart => permanent,
	       shutdown => timer:minutes(1),
	       type => worker,
	       modules => [mod_muc]}
	     || I <- lists:seq(1, Cores)],
    {ok, {{one_for_one, 10*Cores, 1}, Specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
