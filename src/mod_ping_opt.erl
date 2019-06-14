%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_ping_opt).

-export([ping_ack_timeout/1]).
-export([ping_interval/1]).
-export([send_pings/1]).
-export([timeout_action/1]).

-spec ping_ack_timeout(gen_mod:opts() | global | binary()) -> 'undefined' | pos_integer().
ping_ack_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ping_ack_timeout, Opts);
ping_ack_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_ping, ping_ack_timeout).

-spec ping_interval(gen_mod:opts() | global | binary()) -> pos_integer().
ping_interval(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ping_interval, Opts);
ping_interval(Host) ->
    gen_mod:get_module_opt(Host, mod_ping, ping_interval).

-spec send_pings(gen_mod:opts() | global | binary()) -> boolean().
send_pings(Opts) when is_map(Opts) ->
    gen_mod:get_opt(send_pings, Opts);
send_pings(Host) ->
    gen_mod:get_module_opt(Host, mod_ping, send_pings).

-spec timeout_action(gen_mod:opts() | global | binary()) -> 'kill' | 'none'.
timeout_action(Opts) when is_map(Opts) ->
    gen_mod:get_opt(timeout_action, Opts);
timeout_action(Host) ->
    gen_mod:get_module_opt(Host, mod_ping, timeout_action).

