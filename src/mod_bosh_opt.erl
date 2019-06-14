%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_bosh_opt).

-export([cache_life_time/1]).
-export([cache_missed/1]).
-export([cache_size/1]).
-export([json/1]).
-export([max_concat/1]).
-export([max_inactivity/1]).
-export([max_pause/1]).
-export([prebind/1]).
-export([queue_type/1]).
-export([ram_db_type/1]).
-export([use_cache/1]).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, cache_life_time).

-spec cache_missed(gen_mod:opts() | global | binary()) -> boolean().
cache_missed(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_missed, Opts);
cache_missed(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, cache_missed).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, cache_size).

-spec json(gen_mod:opts() | global | binary()) -> boolean().
json(Opts) when is_map(Opts) ->
    gen_mod:get_opt(json, Opts);
json(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, json).

-spec max_concat(gen_mod:opts() | global | binary()) -> 'unlimited' | pos_integer().
max_concat(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_concat, Opts);
max_concat(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, max_concat).

-spec max_inactivity(gen_mod:opts() | global | binary()) -> pos_integer().
max_inactivity(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_inactivity, Opts);
max_inactivity(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, max_inactivity).

-spec max_pause(gen_mod:opts() | global | binary()) -> pos_integer().
max_pause(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_pause, Opts);
max_pause(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, max_pause).

-spec prebind(gen_mod:opts() | global | binary()) -> boolean().
prebind(Opts) when is_map(Opts) ->
    gen_mod:get_opt(prebind, Opts);
prebind(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, prebind).

-spec queue_type(gen_mod:opts() | global | binary()) -> 'file' | 'ram'.
queue_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(queue_type, Opts);
queue_type(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, queue_type).

-spec ram_db_type(gen_mod:opts() | global | binary()) -> atom().
ram_db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ram_db_type, Opts);
ram_db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, ram_db_type).

-spec use_cache(gen_mod:opts() | global | binary()) -> boolean().
use_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_cache, Opts);
use_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_bosh, use_cache).

