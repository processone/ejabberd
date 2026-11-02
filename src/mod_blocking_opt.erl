%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_blocking_opt).

-export([support_reporting/1]).

-spec support_reporting(gen_mod:opts() | global | binary()) -> boolean().
support_reporting(Opts) when is_map(Opts) ->
    gen_mod:get_opt(support_reporting, Opts);
support_reporting(Host) ->
    gen_mod:get_module_opt(Host, mod_blocking, support_reporting).

