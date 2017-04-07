%%%----------------------------------------------------------------------
%%% File    : ejabberd_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Erlang/OTP supervisor
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_sup).
-author('alexey@process-one.net').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hooks =
	{ejabberd_hooks,
	 {ejabberd_hooks, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_hooks]},
    SystemMonitor =
	{ejabberd_system_monitor,
	 {ejabberd_system_monitor, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_system_monitor]},
    S2S =
	{ejabberd_s2s,
	 {ejabberd_s2s, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_s2s]},
    Captcha =
	{ejabberd_captcha,
	 {ejabberd_captcha, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_captcha]},
    Listener =
	{ejabberd_listener,
	 {ejabberd_listener, start_link, []},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_listener]},
    S2SInSupervisor =
	{ejabberd_s2s_in_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_s2s_in_sup, ejabberd_s2s_in]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    S2SOutSupervisor =
	{ejabberd_s2s_out_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_s2s_out_sup, ejabberd_s2s_out]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    ServiceSupervisor =
	{ejabberd_service_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_service_sup, ejabberd_service]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    IQSupervisor =
	{ejabberd_iq_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_iq_sup, gen_iq_handler]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    BackendSupervisor = {ejabberd_backend_sup,
			 {ejabberd_backend_sup, start_link, []},
			 permanent, infinity, supervisor,
			 [ejabberd_backend_sup]},
    ACL = {acl, {acl, start_link, []},
	   permanent, 5000, worker, [acl]},
    Shaper = {shaper, {shaper, start_link, []},
	   permanent, 5000, worker, [shaper]},
    SQLSupervisor = {ejabberd_rdbms,
		     {ejabberd_rdbms, start_link, []},
		     permanent, infinity, supervisor, [ejabberd_rdbms]},
    RiakSupervisor = {ejabberd_riak_sup,
		     {ejabberd_riak_sup, start_link, []},
		      permanent, infinity, supervisor, [ejabberd_riak_sup]},
    RedisSupervisor = {ejabberd_redis_sup,
		       {ejabberd_redis_sup, start_link, []},
		       permanent, infinity, supervisor, [ejabberd_redis_sup]},
    Router = {ejabberd_router, {ejabberd_router, start_link, []},
	      permanent, 5000, worker, [ejabberd_router]},
    RouterMulticast = {ejabberd_router_multicast,
		       {ejabberd_router_multicast, start_link, []},
		       permanent, 5000, worker, [ejabberd_router_multicast]},
    Local = {ejabberd_local, {ejabberd_local, start_link, []},
	     permanent, 5000, worker, [ejabberd_local]},
    SM = {ejabberd_sm, {ejabberd_sm, start_link, []},
	  permanent, 5000, worker, [ejabberd_sm]},
    GenModSupervisor = {ejabberd_gen_mod_sup, {gen_mod, start_link, []},
			permanent, infinity, supervisor, [gen_mod]},
    ExtMod = {ext_mod, {ext_mod, start_link, []},
	      permanent, 5000, worker, [ext_mod]},
    Auth = {ejabberd_auth, {ejabberd_auth, start_link, []},
	    permanent, 5000, worker, [ejabberd_auth]},
    OAuth = {ejabberd_oauth, {ejabberd_oauth, start_link, []},
	     permanent, 5000, worker, [ejabberd_oauth]},
    Translation = {translate, {translate, start_link, []},
		   permanent, 5000, worker, [translate]},
    AccessPerms = {ejabberd_access_permissions,
		   {ejabberd_access_permissions, start_link, []},
		   permanent, 5000, worker, [ejabberd_access_permissions]},
    Ctl = {ejabberd_ctl, {ejabberd_ctl, start_link, []},
	   permanent, 5000, worker, [ejabberd_ctl]},
    Commands = {ejabberd_commands, {ejabberd_commands, start_link, []},
		permanent, 5000, worker, [ejabberd_commands]},
    Admin = {ejabberd_admin, {ejabberd_admin, start_link, []},
	     permanent, 5000, worker, [ejabberd_admin]},
    CyrSASL = {cyrsasl, {cyrsasl, start_link, []},
	       permanent, 5000, worker, [cyrsasl]},
    {ok, {{one_for_one, 10, 1},
	  [Hooks,
	   CyrSASL,
	   Translation,
	   AccessPerms,
	   Ctl,
	   Commands,
	   Admin,
	   Listener,
	   SystemMonitor,
	   S2S,
	   Captcha,
	   S2SInSupervisor,
	   S2SOutSupervisor,
	   ServiceSupervisor,
	   IQSupervisor,
	   ACL,
	   Shaper,
	   BackendSupervisor,
	   SQLSupervisor,
	   RiakSupervisor,
	   RedisSupervisor,
	   Router,
	   RouterMulticast,
	   Local,
	   SM,
	   ExtMod,
	   GenModSupervisor,
	   Auth,
	   OAuth]}}.
