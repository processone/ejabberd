%%%----------------------------------------------------------------------
%%% File    : mod_proxy65.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : Main supervisor.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
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

-module(mod_proxy65).

-author('xram@jabber.ru').

-protocol({xep, 65, '1.8'}).

-behaviour(gen_mod).

-behaviour(supervisor).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3]).

%% supervisor callbacks.
-export([init/1]).

-export([start_link/1, mod_opt_type/1, mod_options/1, depends/2]).

-define(PROCNAME, ejabberd_mod_proxy65).

-include("translate.hrl").

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
	    ChildSpec = {Proc, {?MODULE, start_link, [Host]},
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

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:start_link({local, Proc}, ?MODULE, [Host]).

init([Host]) ->
    Service = {mod_proxy65_service,
	       {mod_proxy65_service, start_link, [Host]},
	       transient, 5000, worker, [mod_proxy65_service]},
    {ok, {{one_for_one, 10, 1}, [Service]}}.

depends(_Host, _Opts) ->
    [].

mod_opt_type(access) ->
    econf:acl();
mod_opt_type(hostname) ->
    econf:host();
mod_opt_type(ip) ->
    econf:ip();
mod_opt_type(name) ->
    econf:binary();
mod_opt_type(port) ->
    econf:port();
mod_opt_type(max_connections) ->
    econf:pos_int(infinity);
mod_opt_type(host) ->
    econf:host();
mod_opt_type(hosts) ->
    econf:hosts();
mod_opt_type(ram_db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(server_host) ->
    econf:binary();
mod_opt_type(auth_type) ->
    econf:enum([plain, anonymous]);
mod_opt_type(recbuf) ->
    econf:pos_int();
mod_opt_type(shaper) ->
    econf:shaper();
mod_opt_type(sndbuf) ->
    econf:pos_int();
mod_opt_type(vcard) ->
    econf:vcard_temp().

mod_options(Host) ->
    [{ram_db_type, ejabberd_config:default_ram_db(Host, ?MODULE)},
     {access, all},
     {host, <<"proxy.", Host/binary>>},
     {hosts, []},
     {hostname, undefined},
     {ip, undefined},
     {port, 7777},
     {name, ?T("SOCKS5 Bytestreams")},
     {vcard, undefined},
     {max_connections, infinity},
     {auth_type, anonymous},
     {recbuf, 65536},
     {sndbuf, 65536},
     {shaper, none}].
