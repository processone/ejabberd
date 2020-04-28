%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_stun_disco_opt).

-export([access/1]).
-export([credentials_lifetime/1]).
-export([offer_local_services/1]).
-export([secret/1]).
-export([services/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'local' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_stun_disco, access).

-spec credentials_lifetime(gen_mod:opts() | global | binary()) -> pos_integer().
credentials_lifetime(Opts) when is_map(Opts) ->
    gen_mod:get_opt(credentials_lifetime, Opts);
credentials_lifetime(Host) ->
    gen_mod:get_module_opt(Host, mod_stun_disco, credentials_lifetime).

-spec offer_local_services(gen_mod:opts() | global | binary()) -> boolean().
offer_local_services(Opts) when is_map(Opts) ->
    gen_mod:get_opt(offer_local_services, Opts);
offer_local_services(Host) ->
    gen_mod:get_module_opt(Host, mod_stun_disco, offer_local_services).

-spec secret(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
secret(Opts) when is_map(Opts) ->
    gen_mod:get_opt(secret, Opts);
secret(Host) ->
    gen_mod:get_module_opt(Host, mod_stun_disco, secret).

-spec services(gen_mod:opts() | global | binary()) -> [tuple()].
services(Opts) when is_map(Opts) ->
    gen_mod:get_opt(services, Opts);
services(Host) ->
    gen_mod:get_module_opt(Host, mod_stun_disco, services).

