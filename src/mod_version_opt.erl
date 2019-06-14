%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_version_opt).

-export([show_os/1]).

-spec show_os(gen_mod:opts() | global | binary()) -> boolean().
show_os(Opts) when is_map(Opts) ->
    gen_mod:get_opt(show_os, Opts);
show_os(Host) ->
    gen_mod:get_module_opt(Host, mod_version, show_os).

