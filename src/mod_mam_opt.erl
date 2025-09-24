%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_mam_opt).

-export([access_preferences/1]).
-export([archive_muc_as_mucsub/1]).
-export([assume_mam_usage/1]).
-export([cache_life_time/1]).
-export([cache_missed/1]).
-export([cache_size/1]).
-export([clear_archive_on_room_destroy/1]).
-export([compress_xml/1]).
-export([db_type/1]).
-export([default/1]).
-export([request_activates_archiving/1]).
-export([use_cache/1]).
-export([user_mucsub_from_muc_archive/1]).

-spec access_preferences(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access_preferences(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_preferences, Opts);
access_preferences(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, access_preferences).

-spec archive_muc_as_mucsub(gen_mod:opts() | global | binary()) -> boolean().
archive_muc_as_mucsub(Opts) when is_map(Opts) ->
    gen_mod:get_opt(archive_muc_as_mucsub, Opts);
archive_muc_as_mucsub(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, archive_muc_as_mucsub).

-spec assume_mam_usage(gen_mod:opts() | global | binary()) -> boolean().
assume_mam_usage(Opts) when is_map(Opts) ->
    gen_mod:get_opt(assume_mam_usage, Opts);
assume_mam_usage(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, assume_mam_usage).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, cache_life_time).

-spec cache_missed(gen_mod:opts() | global | binary()) -> boolean().
cache_missed(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_missed, Opts);
cache_missed(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, cache_missed).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, cache_size).

-spec clear_archive_on_room_destroy(gen_mod:opts() | global | binary()) -> boolean().
clear_archive_on_room_destroy(Opts) when is_map(Opts) ->
    gen_mod:get_opt(clear_archive_on_room_destroy, Opts);
clear_archive_on_room_destroy(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, clear_archive_on_room_destroy).

-spec compress_xml(gen_mod:opts() | global | binary()) -> boolean().
compress_xml(Opts) when is_map(Opts) ->
    gen_mod:get_opt(compress_xml, Opts);
compress_xml(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, compress_xml).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, db_type).

-spec default(gen_mod:opts() | global | binary()) -> 'always' | 'never' | 'roster'.
default(Opts) when is_map(Opts) ->
    gen_mod:get_opt(default, Opts);
default(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, default).

-spec request_activates_archiving(gen_mod:opts() | global | binary()) -> boolean().
request_activates_archiving(Opts) when is_map(Opts) ->
    gen_mod:get_opt(request_activates_archiving, Opts);
request_activates_archiving(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, request_activates_archiving).

-spec use_cache(gen_mod:opts() | global | binary()) -> boolean().
use_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_cache, Opts);
use_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, use_cache).

-spec user_mucsub_from_muc_archive(gen_mod:opts() | global | binary()) -> boolean().
user_mucsub_from_muc_archive(Opts) when is_map(Opts) ->
    gen_mod:get_opt(user_mucsub_from_muc_archive, Opts);
user_mucsub_from_muc_archive(Host) ->
    gen_mod:get_module_opt(Host, mod_mam, user_mucsub_from_muc_archive).

