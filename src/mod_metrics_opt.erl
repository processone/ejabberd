%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_metrics_opt).

-export([ip/1]).
-export([port/1]).

-spec ip(gen_mod:opts() | global | binary()) -> {127,0,0,1} | inet:ip4_address().
ip(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ip, Opts);
ip(Host) ->
    gen_mod:get_module_opt(Host, mod_metrics, ip).

-spec port(gen_mod:opts() | global | binary()) -> 1..1114111.
port(Opts) when is_map(Opts) ->
    gen_mod:get_opt(port, Opts);
port(Host) ->
    gen_mod:get_module_opt(Host, mod_metrics, port).

