%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_push_opt).

-export([cache_life_time/1]).
-export([cache_missed/1]).
-export([cache_size/1]).
-export([db_type/1]).
-export([include_body/1]).
-export([include_sender/1]).
-export([notify_on/1]).
-export([use_cache/1]).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_push, cache_life_time).

-spec cache_missed(gen_mod:opts() | global | binary()) -> boolean().
cache_missed(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_missed, Opts);
cache_missed(Host) ->
    gen_mod:get_module_opt(Host, mod_push, cache_missed).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_push, cache_size).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_push, db_type).

-spec include_body(gen_mod:opts() | global | binary()) -> boolean() | binary().
include_body(Opts) when is_map(Opts) ->
    gen_mod:get_opt(include_body, Opts);
include_body(Host) ->
    gen_mod:get_module_opt(Host, mod_push, include_body).

-spec include_sender(gen_mod:opts() | global | binary()) -> boolean().
include_sender(Opts) when is_map(Opts) ->
    gen_mod:get_opt(include_sender, Opts);
include_sender(Host) ->
    gen_mod:get_module_opt(Host, mod_push, include_sender).

-spec notify_on(gen_mod:opts() | global | binary()) -> 'all' | 'messages'.
notify_on(Opts) when is_map(Opts) ->
    gen_mod:get_opt(notify_on, Opts);
notify_on(Host) ->
    gen_mod:get_module_opt(Host, mod_push, notify_on).

-spec use_cache(gen_mod:opts() | global | binary()) -> boolean().
use_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_cache, Opts);
use_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_push, use_cache).

