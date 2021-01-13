%%%----------------------------------------------------------------------
%%% File    : mod_proxy65.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : Main supervisor.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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

-export([start_link/1, mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).

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

mod_doc() ->
    #{desc =>
          ?T("This module implements "
             "https://xmpp.org/extensions/xep-0065.html"
             "[XEP-0065: SOCKS5 Bytestreams]. It allows ejabberd "
             "to act as a file transfer proxy between two XMPP clients."),
      opts =>
          [{host,
            #{desc => ?T("Deprecated. Use 'hosts' instead.")}},
           {hosts,
            #{value => ?T("[Host, ...]"),
              desc =>
                  ?T("This option defines the Jabber IDs of the service. "
                     "If the 'hosts' option is not specified, the only Jabber ID will "
                     "be the hostname of the virtual host with the prefix \"proxy.\". "
                     "The keyword '@HOST@' is replaced with the real virtual host name.")}},
           {name,
            #{value => ?T("Name"),
              desc =>
                  ?T("The value of the service name. This name is only visible in some "
                     "clients that support https://xmpp.org/extensions/xep-0030.html"
                     "[XEP-0030: Service Discovery]. The default is \"SOCKS5 Bytestreams\".")}},
           {access,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("Defines an access rule for file transfer initiators. "
                     "The default value is 'all'. You may want to restrict "
                     "access to the users of your server only, in order to "
                     "avoid abusing your proxy by the users of remote "
                     "servers.")}},
           {ram_db_type,
            #{value => "mnesia | redis | sql",
              desc =>
                  ?T("Define the type of volatile (in-memory) storage where the module "
                     "will store room information.")}},
           {ip,
            #{value => ?T("IPAddress"),
              desc =>
                  ?T("This option specifies which network interface to listen "
                     "for. The default value is an IP address of the service's "
                     "DNS name, or, if fails, '127.0.0.1'.")}},
           {hostname,
            #{value => ?T("Host"),
              desc =>
                  ?T("Defines a hostname offered by the proxy when "
                     "establishing a session with clients. This is useful "
                     "when you run the proxy behind a NAT. The keyword "
                     "'@HOST@' is replaced with the virtual host name. "
                     "The default is to use the value of 'ip' option. "
                     "Examples: 'proxy.mydomain.org', '200.150.100.50'.")}},
           {port,
            #{value => "1..65535",
              desc =>
                  ?T("A port number to listen for incoming connections. "
                     "The default value is '7777'.")}},
           {auth_type,
            #{value => "anonymous | plain",
              desc =>
                  ?T("SOCKS5 authentication type. "
                     "The default value is 'anonymous'. "
                     "If set to 'plain', ejabberd will use "
                     "authentication backend as it would "
                     "for SASL PLAIN.")}},
           {max_connections,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Maximum number of active connections per file transfer "
                     "initiator. The default value is 'infinity'.")}},
           {shaper,
            #{value => ?T("Shaper"),
              desc =>
                  ?T("This option defines a shaper for the file transfer peers. "
                     "A shaper with the maximum bandwidth will be selected. "
                     "The default is 'none', i.e. no shaper.")}},
           {recbuf,
            #{value => ?T("Size"),
              desc =>
                  ?T("A size of the buffer for incoming packets. "
                     "If you define a shaper, set the value of this "
                     "option to the size of the shaper in order "
                     "to avoid traffic spikes in file transfers. "
                     "The default value is '65536' bytes.")}},
           {sndbuf,
            #{value => ?T("Size"),
              desc =>
                  ?T("A size of the buffer for outgoing packets. "
                     "If you define a shaper, set the value of this "
                     "option to the size of the shaper in order "
                     "to avoid traffic spikes in file transfers. "
                     "The default value is '65536' bytes.")}},
           {vcard,
            #{value => ?T("vCard"),
              desc =>
                  ?T("A custom vCard of the service that will be displayed "
                     "by some XMPP clients in Service Discovery. The value of "
                     "'vCard' is a YAML map constructed from an XML representation "
                     "of vCard. Since the representation has no attributes, "
                     "the mapping is straightforward."),
              example =>
                  [{?T("For example, the following XML representation of vCard:"),
                    ["<vCard xmlns='vcard-temp'>",
                     "  <FN>Conferences</FN>",
                     "  <ADR>",
                     "    <WORK/>",
                     "    <STREET>Elm Street</STREET>",
                     "  </ADR>",
                     "</vCard>"]},
                   {?T("will be translated to:"),
                    ["vcard:",
                     "  fn: Conferences",
                     "  adr:",
                     "    -",
                     "      work: true",
                     "      street: Elm Street"]}]}}],
      example =>
          ["acl:",
           "  admin:",
           "    user: admin@example.org",
           "  proxy_users:",
           "    server: example.org",
           "",
           "access_rules:",
           "  proxy65_access:",
           "    allow: proxy_users",
           "",
           "shaper_rules:",
           "  proxy65_shaper:",
           "    none: admin",
           "  proxyrate: proxy_users",
           "",
           "shaper:",
           "  proxyrate: 10240",
           "",
           "modules:",
           "  ...",
           "  mod_proxy65:",
           "    host: proxy1.example.org",
           "    name: \"File Transfer Proxy\"",
           "    ip: 200.150.100.1",
           "    port: 7778",
           "    max_connections: 5",
           "    access: proxy65_access",
           "    shaper: proxy65_shaper",
           "    recbuf: 10240",
           "    sndbuf: 10240",
           "  ..."]}.
