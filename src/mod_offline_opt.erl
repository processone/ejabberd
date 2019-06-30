%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_offline_opt).

-export([access_max_user_messages/1]).
-export([bounce_groupchat/1]).
-export([cache_life_time/1]).
-export([cache_size/1]).
-export([db_type/1]).
-export([store_empty_body/1]).
-export([store_groupchat/1]).
-export([use_cache/1]).
-export([use_mam_for_storage/1]).

-spec access_max_user_messages(gen_mod:opts() | global | binary()) -> atom() | [ejabberd_shaper:shaper_rule()].
access_max_user_messages(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_max_user_messages, Opts);
access_max_user_messages(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, access_max_user_messages).

-spec bounce_groupchat(gen_mod:opts() | global | binary()) -> boolean().
bounce_groupchat(Opts) when is_map(Opts) ->
    gen_mod:get_opt(bounce_groupchat, Opts);
bounce_groupchat(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, bounce_groupchat).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, cache_life_time).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, cache_size).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, db_type).

-spec store_empty_body(gen_mod:opts() | global | binary()) -> 'false' | 'true' | 'unless_chat_state'.
store_empty_body(Opts) when is_map(Opts) ->
    gen_mod:get_opt(store_empty_body, Opts);
store_empty_body(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, store_empty_body).

-spec store_groupchat(gen_mod:opts() | global | binary()) -> boolean().
store_groupchat(Opts) when is_map(Opts) ->
    gen_mod:get_opt(store_groupchat, Opts);
store_groupchat(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, store_groupchat).

-spec use_cache(gen_mod:opts() | global | binary()) -> boolean().
use_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_cache, Opts);
use_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, use_cache).

-spec use_mam_for_storage(gen_mod:opts() | global | binary()) -> boolean().
use_mam_for_storage(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_mam_for_storage, Opts);
use_mam_for_storage(Host) ->
    gen_mod:get_module_opt(Host, mod_offline, use_mam_for_storage).

