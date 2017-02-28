%%%----------------------------------------------------------------------
%%% File    : mod_proxy65.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : Main supervisor.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
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

-module(mod_proxy65).

-author('xram@jabber.ru').

-protocol({xep, 65, '1.8'}).

-behaviour(gen_mod).

-behaviour(supervisor).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, transform_module_options/1]).

%% supervisor callbacks.
-export([init/1]).

-export([start_link/2, mod_opt_type/1, depends/2]).

-define(PROCNAME, ejabberd_mod_proxy65).

-callback init() -> any().
-callback register_stream(binary(), pid()) -> ok | {error, any()}.
-callback unregister_stream(binary()) -> ok | {error, any()}.
-callback activate_stream(binary(), binary(), pos_integer() | infinity, node()) ->
    ok | {error, limit | conflict | notfound | term()}.

start(Host, Opts) ->
    case mod_proxy65_service:add_listener(Host, Opts) of
	{error, _} = Err ->
	    Err;
	_ ->
	    Mod = gen_mod:ram_db_mod(global, ?MODULE),
	    Mod:init(),
	    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
			 transient, infinity, supervisor, [?MODULE]},
	    supervisor:start_child(ejabberd_gen_mod_sup, ChildSpec)
    end.

stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    mod_proxy65_service:delete_listener(Host);
	true ->
	    ok
    end,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:terminate_child(ejabberd_gen_mod_sup, Proc),
    supervisor:delete_child(ejabberd_gen_mod_sup, Proc).

reload(Host, NewOpts, OldOpts) ->
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    Mod:init(),
    mod_proxy65_service:reload(Host, NewOpts, OldOpts).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:start_link({local, Proc}, ?MODULE,
			  [Host, Opts]).

transform_module_options(Opts) ->
    mod_proxy65_service:transform_module_options(Opts).

init([Host, Opts]) ->
    Service = {mod_proxy65_service,
	       {mod_proxy65_service, start_link, [Host, Opts]},
	       transient, 5000, worker, [mod_proxy65_service]},
    StreamSupervisor = {ejabberd_mod_proxy65_sup,
			{ejabberd_tmp_sup, start_link,
			 [gen_mod:get_module_proc(Host,
						  ejabberd_mod_proxy65_sup),
			  mod_proxy65_stream]},
			transient, infinity, supervisor, [ejabberd_tmp_sup]},
    {ok,
     {{one_for_one, 10, 1},
      [StreamSupervisor, Service]}}.

depends(_Host, _Opts) ->
    [].

mod_opt_type(auth_type) ->
    fun (plain) -> plain;
	(anonymous) -> anonymous
    end;
mod_opt_type(recbuf) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(shaper) -> fun acl:shaper_rules_validator/1;
mod_opt_type(sndbuf) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(access) -> fun acl:access_rules_validator/1;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(hostname) -> fun iolist_to_binary/1;
mod_opt_type(ip) ->
    fun (S) ->
	    {ok, Addr} =
		inet_parse:address(binary_to_list(iolist_to_binary(S))),
	    Addr
    end;
mod_opt_type(name) -> fun iolist_to_binary/1;
mod_opt_type(port) ->
    fun (P) when is_integer(P), P > 0, P < 65536 -> P end;
mod_opt_type(max_connections) ->
    fun (I) when is_integer(I), I > 0 -> I;
	(infinity) -> infinity
    end;
mod_opt_type(ram_db_type) ->
    fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(_) ->
    [auth_type, recbuf, shaper, sndbuf,
     access, host, hostname, ip, name, port,
     max_connections, ram_db_type].
