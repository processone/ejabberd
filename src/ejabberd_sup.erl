%%%----------------------------------------------------------------------
%%% File    : ejabberd_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Erlang/OTP supervisor
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
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
    GlobalRouter =
	{ejabberd_global_router,
	 {ejabberd_global_router, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_global_router]},
    Router =
	{ejabberd_router,
	 {ejabberd_router, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_router]},
    Router_multicast =
	{ejabberd_router_multicast,
	 {ejabberd_router_multicast, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_router_multicast]},
    SM =
	{ejabberd_sm,
	 {ejabberd_sm, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_sm]},
    S2S =
	{ejabberd_s2s,
	 {ejabberd_s2s, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_s2s]},
    Local =
	{ejabberd_local,
	 {ejabberd_local, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_local]},
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
    ReceiverSupervisor =
	{ejabberd_receiver_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_receiver_sup, ejabberd_receiver]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    C2SSupervisor =
	{ejabberd_c2s_sup,
	 {ejabberd_tmp_sup, start_link, [ejabberd_c2s_sup, ejabberd_c2s]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
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
    Hosts =
	{ejabberd_hosts,
	 {ejabberd_hosts, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_hosts]},
    HTTPSupervisor =
	{ejabberd_http_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_http_sup, ejabberd_http]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    HTTPPollSupervisor =
	{ejabberd_http_poll_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_http_poll_sup, ejabberd_http_poll]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    FrontendSocketSupervisor =
	{ejabberd_frontend_socket_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_frontend_socket_sup, ejabberd_frontend_socket]},
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
    STUNSupervisor =
	{ejabberd_stun_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_stun_sup, ejabberd_stun]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    Cluster =
	{ejabberd_cluster,
	 {ejabberd_cluster, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_cluster]},
    CacheTabSupervisor =
	{cache_tab_sup,
	 {cache_tab_sup, start_link, []},
	 permanent,
	 infinity,
	 supervisor,
	 [cache_tab_sup]},
    {ok, {{one_for_one, 10, 1},
	  [Hooks,
           GlobalRouter,
	   Cluster,
	   Router,
	   Router_multicast,
	   SM,
	   S2S,
	   Local,
	   Captcha,
	   ReceiverSupervisor,
	   C2SSupervisor,
	   S2SInSupervisor,
	   S2SOutSupervisor,
	   ServiceSupervisor,
	   Hosts,
	   HTTPSupervisor,
	   HTTPPollSupervisor,
	   IQSupervisor,
	   STUNSupervisor,
	   FrontendSocketSupervisor,
	   CacheTabSupervisor,
	   Listener]}}.


