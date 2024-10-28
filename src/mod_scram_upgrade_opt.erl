%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_scram_upgrade_opt).

-export([offered_upgrades/1]).

-spec offered_upgrades(gen_mod:opts() | global | binary()) -> ['sha256' | 'sha512'].
offered_upgrades(Opts) when is_map(Opts) ->
    gen_mod:get_opt(offered_upgrades, Opts);
offered_upgrades(Host) ->
    gen_mod:get_module_opt(Host, mod_scram_upgrade, offered_upgrades).

