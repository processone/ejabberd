%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_mqtt_opt).

-export([access_publish/1]).
-export([access_subscribe/1]).
-export([cache_life_time/1]).
-export([cache_missed/1]).
-export([cache_size/1]).
-export([db_type/1]).
-export([match_retained_limit/1]).
-export([max_queue/1]).
-export([max_topic_aliases/1]).
-export([max_topic_depth/1]).
-export([queue_type/1]).
-export([ram_db_type/1]).
-export([session_expiry/1]).
-export([use_cache/1]).

-spec access_publish(gen_mod:opts() | global | binary()) -> [{[binary()],acl:acl()}].
access_publish(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_publish, Opts);
access_publish(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, access_publish).

-spec access_subscribe(gen_mod:opts() | global | binary()) -> [{[binary()],acl:acl()}].
access_subscribe(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_subscribe, Opts);
access_subscribe(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, access_subscribe).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, cache_life_time).

-spec cache_missed(gen_mod:opts() | global | binary()) -> boolean().
cache_missed(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_missed, Opts);
cache_missed(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, cache_missed).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, cache_size).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, db_type).

-spec match_retained_limit(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
match_retained_limit(Opts) when is_map(Opts) ->
    gen_mod:get_opt(match_retained_limit, Opts);
match_retained_limit(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, match_retained_limit).

-spec max_queue(gen_mod:opts() | global | binary()) -> 'unlimited' | pos_integer().
max_queue(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_queue, Opts);
max_queue(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, max_queue).

-spec max_topic_aliases(gen_mod:opts() | global | binary()) -> char().
max_topic_aliases(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_topic_aliases, Opts);
max_topic_aliases(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, max_topic_aliases).

-spec max_topic_depth(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_topic_depth(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_topic_depth, Opts);
max_topic_depth(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, max_topic_depth).

-spec queue_type(gen_mod:opts() | global | binary()) -> 'file' | 'ram'.
queue_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(queue_type, Opts);
queue_type(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, queue_type).

-spec ram_db_type(gen_mod:opts() | global | binary()) -> atom().
ram_db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ram_db_type, Opts);
ram_db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, ram_db_type).

-spec session_expiry(gen_mod:opts() | global | binary()) -> non_neg_integer().
session_expiry(Opts) when is_map(Opts) ->
    gen_mod:get_opt(session_expiry, Opts);
session_expiry(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, session_expiry).

-spec use_cache(gen_mod:opts() | global | binary()) -> boolean().
use_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_cache, Opts);
use_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt, use_cache).

