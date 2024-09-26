%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_privilege_opt).

-export([iq/1]).
-export([message/1]).
-export([presence/1]).
-export([roster/1]).

-spec iq(gen_mod:opts() | global | binary()) -> [{binary(),[{'both',acl:acl()} | {'get',acl:acl()} | {'set',acl:acl()}]}].
iq(Opts) when is_map(Opts) ->
    gen_mod:get_opt(iq, Opts);
iq(Host) ->
    gen_mod:get_module_opt(Host, mod_privilege, iq).

-spec message(gen_mod:opts() | global | binary()) -> [{'outgoing','none' | acl:acl()}].
message(Opts) when is_map(Opts) ->
    gen_mod:get_opt(message, Opts);
message(Host) ->
    gen_mod:get_module_opt(Host, mod_privilege, message).

-spec presence(gen_mod:opts() | global | binary()) -> [{'managed_entity','none' | acl:acl()} | {'roster','none' | acl:acl()}].
presence(Opts) when is_map(Opts) ->
    gen_mod:get_opt(presence, Opts);
presence(Host) ->
    gen_mod:get_module_opt(Host, mod_privilege, presence).

-spec roster(gen_mod:opts() | global | binary()) -> [{'both','none' | acl:acl()} | {'get','none' | acl:acl()} | {'set','none' | acl:acl()}].
roster(Opts) when is_map(Opts) ->
    gen_mod:get_opt(roster, Opts);
roster(Host) ->
    gen_mod:get_module_opt(Host, mod_privilege, roster).

