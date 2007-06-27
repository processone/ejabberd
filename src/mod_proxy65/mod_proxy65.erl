%%%----------------------------------------------------------------------
%%% File    : mod_proxy65.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : Main supervisor.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_proxy65).
-author('xram@jabber.ru').

-behaviour(gen_mod).
-behaviour(supervisor).

%% gen_mod callbacks.
-export([start/2, stop/1]).

%% supervisor callbacks.
-export([init/1]).

%% API.
-export([start_link/2]).

-define(PROCNAME, ejabberd_mod_proxy65).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {
      Proc, {?MODULE, start_link, [Host, Opts]},
      transient, infinity, supervisor, [?MODULE]
     },
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:start_link({local, Proc}, ?MODULE, [Host, Opts]).

init([Host, Opts]) ->
    Service =
	{mod_proxy65_service, {mod_proxy65_service, start_link, [Host, Opts]},
	 transient, 5000, worker, [mod_proxy65_service]},
    StreamSupervisor =
	{ejabberd_mod_proxy65_sup,
	 {ejabberd_tmp_sup, start_link,
	  [gen_mod:get_module_proc(Host, ejabberd_mod_proxy65_sup),
	   mod_proxy65_stream]},
      transient, infinity, supervisor, [ejabberd_tmp_sup]},
    HttpStreamSupervisor =
	{ejabberd_mod_proxy65_http_sup,
	 {ejabberd_tmp_sup, start_link,
	  [gen_mod:get_module_proc(Host, ejabberd_mod_proxy65_http_sup),
	   mod_proxy65_http]},
      transient, infinity, supervisor, [ejabberd_tmp_sup]},
    StreamManager =
	{mod_proxy65_sm, {mod_proxy65_sm, start_link, [Host, Opts]},
	 transient, 5000, worker, [mod_proxy65_sm]},
    {ok, {{one_for_one, 10, 1},
	  [StreamManager, StreamSupervisor, HttpStreamSupervisor, Service]}}.
