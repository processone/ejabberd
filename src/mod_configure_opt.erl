%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_configure_opt).

-export([access/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'configure' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_configure, access).

