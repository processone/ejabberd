%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_pres_counter_opt).

-export([count/1]).
-export([interval/1]).

-spec count(gen_mod:opts() | global | binary()) -> pos_integer().
count(Opts) when is_map(Opts) ->
    gen_mod:get_opt(count, Opts);
count(Host) ->
    gen_mod:get_module_opt(Host, mod_pres_counter, count).

-spec interval(gen_mod:opts() | global | binary()) -> pos_integer().
interval(Opts) when is_map(Opts) ->
    gen_mod:get_opt(interval, Opts);
interval(Host) ->
    gen_mod:get_module_opt(Host, mod_pres_counter, interval).

