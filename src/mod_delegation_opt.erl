%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_delegation_opt).

-export([namespaces/1]).

-spec namespaces(gen_mod:opts() | global | binary()) -> [{binary(),[binary()],acl:acl()}].
namespaces(Opts) when is_map(Opts) ->
    gen_mod:get_opt(namespaces, Opts);
namespaces(Host) ->
    gen_mod:get_module_opt(Host, mod_delegation, namespaces).

