%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_proxy65_opt).

-export([access/1]).
-export([auth_type/1]).
-export([host/1]).
-export([hostname/1]).
-export([hosts/1]).
-export([ip/1]).
-export([max_connections/1]).
-export([name/1]).
-export([port/1]).
-export([ram_db_type/1]).
-export([recbuf/1]).
-export([server_host/1]).
-export([shaper/1]).
-export([sndbuf/1]).
-export([vcard/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, access).

-spec auth_type(gen_mod:opts() | global | binary()) -> 'anonymous' | 'plain'.
auth_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(auth_type, Opts);
auth_type(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, auth_type).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, host).

-spec hostname(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
hostname(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hostname, Opts);
hostname(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, hostname).

-spec hosts(gen_mod:opts() | global | binary()) -> [binary()].
hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hosts, Opts);
hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, hosts).

-spec ip(gen_mod:opts() | global | binary()) -> 'undefined' | inet:ip_address().
ip(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ip, Opts);
ip(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, ip).

-spec max_connections(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_connections(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_connections, Opts);
max_connections(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, max_connections).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, name).

-spec port(gen_mod:opts() | global | binary()) -> 1..1114111.
port(Opts) when is_map(Opts) ->
    gen_mod:get_opt(port, Opts);
port(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, port).

-spec ram_db_type(gen_mod:opts() | global | binary()) -> atom().
ram_db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ram_db_type, Opts);
ram_db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, ram_db_type).

-spec recbuf(gen_mod:opts() | global | binary()) -> pos_integer().
recbuf(Opts) when is_map(Opts) ->
    gen_mod:get_opt(recbuf, Opts);
recbuf(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, recbuf).

-spec server_host(gen_mod:opts() | global | binary()) -> binary().
server_host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(server_host, Opts);
server_host(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, server_host).

-spec shaper(gen_mod:opts() | global | binary()) -> atom() | [ejabberd_shaper:shaper_rule()].
shaper(Opts) when is_map(Opts) ->
    gen_mod:get_opt(shaper, Opts);
shaper(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, shaper).

-spec sndbuf(gen_mod:opts() | global | binary()) -> pos_integer().
sndbuf(Opts) when is_map(Opts) ->
    gen_mod:get_opt(sndbuf, Opts);
sndbuf(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, sndbuf).

-spec vcard(gen_mod:opts() | global | binary()) -> 'undefined' | tuple().
vcard(Opts) when is_map(Opts) ->
    gen_mod:get_opt(vcard, Opts);
vcard(Host) ->
    gen_mod:get_module_opt(Host, mod_proxy65, vcard).

