%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_echo_opt).

-export([host/1]).
-export([hosts/1]).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_echo, host).

-spec hosts(gen_mod:opts() | global | binary()) -> [binary()].
hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hosts, Opts);
hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_echo, hosts).

