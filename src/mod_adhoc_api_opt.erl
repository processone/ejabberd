%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_adhoc_api_opt).

-export([default_version/1]).

-spec default_version(gen_mod:opts() | global | binary()) -> integer().
default_version(Opts) when is_map(Opts) ->
    gen_mod:get_opt(default_version, Opts);
default_version(Host) ->
    gen_mod:get_module_opt(Host, mod_adhoc_api, default_version).

