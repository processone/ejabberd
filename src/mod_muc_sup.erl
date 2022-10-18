%%%----------------------------------------------------------------------
%%% Created : 4 Jul 2019 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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
    Cores = misc:logical_processors(),
    Specs = lists:foldl(
	      fun(I, Acc) ->
		      [#{id => mod_muc:procname(Host, I),
			 start => {mod_muc, start_link, [Host, I]},
			 restart => permanent,
			 shutdown => timer:minutes(1),
			 type => worker,
			 modules => [mod_muc]}|Acc]
	      end, [room_sup_spec(Host)], lists:seq(1, Cores)),
    {ok, {{one_for_one, 10*Cores, 1}, Specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec room_sup_spec(binary()) -> supervisor:child_spec().
room_sup_spec(Host) ->
    Name = mod_muc_room:supervisor(Host),
    #{id => Name,
      start => {ejabberd_tmp_sup, start_link, [Name, mod_muc_room]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [ejabberd_tmp_sup]}.
