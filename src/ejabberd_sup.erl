%%%----------------------------------------------------------------------
%%% File    : ejabberd_sup.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_sup).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

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
    NodeGroups =
	{ejabberd_node_groups,
	 {ejabberd_node_groups, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_node_groups]},
    SystemMonitor =
	{ejabberd_system_monitor,
	 {ejabberd_system_monitor, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_system_monitor]},
    Router =
	{ejabberd_router,
	 {ejabberd_router, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_router]},
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
    {ok, {{one_for_one, 10, 1},
	  [Hooks,
	   NodeGroups,
	   SystemMonitor,
	   Router,
	   SM,
	   S2S,
	   Local,
	   ReceiverSupervisor,
	   C2SSupervisor,
	   S2SInSupervisor,
	   S2SOutSupervisor,
	   ServiceSupervisor,
	   HTTPSupervisor,
	   HTTPPollSupervisor,
	   IQSupervisor,
	   FrontendSocketSupervisor,
	   Listener]}}.


