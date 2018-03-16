%%%----------------------------------------------------------------------
%%% File    : ejabberd_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Erlang/OTP supervisor
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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

-module(ejabberd_sup).
-author('alexey@process-one.net').

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SHUTDOWN_TIMEOUT, timer:seconds(30)).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 1},
	  [worker(ejabberd_hooks),
	   worker(ejabberd_cluster),
	   worker(cyrsasl),
	   worker(translate),
	   worker(ejabberd_access_permissions),
	   worker(ejabberd_ctl),
	   worker(ejabberd_commands),
	   worker(ejabberd_admin),
	   worker(ejabberd_pkix),
	   worker(ejabberd_acme),
	   supervisor(ejabberd_listener),
	   worker(ejabberd_s2s),
	   simple_supervisor(ejabberd_s2s_in),
	   simple_supervisor(ejabberd_s2s_out),
	   simple_supervisor(ejabberd_service),
	   worker(acl),
	   worker(shaper),
	   supervisor(ejabberd_backend_sup),
	   supervisor(ejabberd_rdbms),
	   supervisor(ejabberd_riak_sup),
	   supervisor(ejabberd_redis_sup),
	   worker(ejabberd_iq),
	   worker(ejabberd_router),
	   worker(ejabberd_router_multicast),
	   worker(ejabberd_local),
	   worker(ejabberd_sm),
	   worker(ejabberd_captcha),
	   worker(ext_mod),
	   supervisor(ejabberd_gen_mod_sup, gen_mod),
	   worker(ejabberd_auth),
	   worker(ejabberd_oauth)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
worker(Mod) ->
    {Mod, {Mod, start_link, []}, permanent, ?SHUTDOWN_TIMEOUT, worker, [Mod]}.

supervisor(Mod) ->
    supervisor(Mod, Mod).

supervisor(Name, Mod) ->
    {Name, {Mod, start_link, []}, permanent, infinity, supervisor, [Mod]}.

simple_supervisor(Mod) ->
    Name = list_to_atom(atom_to_list(Mod) ++ "_sup"),
    {Name, {ejabberd_tmp_sup, start_link, [Name, Mod]},
     permanent, infinity, supervisor, [ejabberd_tmp_sup]}.
