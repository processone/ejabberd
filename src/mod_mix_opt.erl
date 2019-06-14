%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_mix_opt).

-export([access_create/1]).
-export([db_type/1]).
-export([host/1]).
-export([hosts/1]).
-export([name/1]).

-spec access_create(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access_create(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_create, Opts);
access_create(Host) ->
    gen_mod:get_module_opt(Host, mod_mix, access_create).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_mix, db_type).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_mix, host).

-spec hosts(gen_mod:opts() | global | binary()) -> [binary()].
hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hosts, Opts);
hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_mix, hosts).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_mix, name).

