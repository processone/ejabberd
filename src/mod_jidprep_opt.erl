%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_jidprep_opt).

-export([access/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'local' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_jidprep, access).

