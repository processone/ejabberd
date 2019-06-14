%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_roster_opt).

-export([access/1]).
-export([cache_life_time/1]).
-export([cache_missed/1]).
-export([cache_size/1]).
-export([db_type/1]).
-export([store_current_id/1]).
-export([use_cache/1]).
-export([versioning/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_roster, access).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_roster, cache_life_time).

-spec cache_missed(gen_mod:opts() | global | binary()) -> boolean().
cache_missed(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_missed, Opts);
cache_missed(Host) ->
    gen_mod:get_module_opt(Host, mod_roster, cache_missed).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_roster, cache_size).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_roster, db_type).

-spec store_current_id(gen_mod:opts() | global | binary()) -> boolean().
store_current_id(Opts) when is_map(Opts) ->
    gen_mod:get_opt(store_current_id, Opts);
store_current_id(Host) ->
    gen_mod:get_module_opt(Host, mod_roster, store_current_id).

-spec use_cache(gen_mod:opts() | global | binary()) -> boolean().
use_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_cache, Opts);
use_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_roster, use_cache).

-spec versioning(gen_mod:opts() | global | binary()) -> boolean().
versioning(Opts) when is_map(Opts) ->
    gen_mod:get_opt(versioning, Opts);
versioning(Host) ->
    gen_mod:get_module_opt(Host, mod_roster, versioning).

