%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_vcard_opt).

-export([allow_return_all/1]).
-export([cache_life_time/1]).
-export([cache_missed/1]).
-export([cache_size/1]).
-export([db_type/1]).
-export([host/1]).
-export([hosts/1]).
-export([matches/1]).
-export([name/1]).
-export([search/1]).
-export([use_cache/1]).
-export([vcard/1]).

-spec allow_return_all(gen_mod:opts() | global | binary()) -> boolean().
allow_return_all(Opts) when is_map(Opts) ->
    gen_mod:get_opt(allow_return_all, Opts);
allow_return_all(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, allow_return_all).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, cache_life_time).

-spec cache_missed(gen_mod:opts() | global | binary()) -> boolean().
cache_missed(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_missed, Opts);
cache_missed(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, cache_missed).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, cache_size).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, db_type).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, host).

-spec hosts(gen_mod:opts() | global | binary()) -> [binary()].
hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hosts, Opts);
hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, hosts).

-spec matches(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
matches(Opts) when is_map(Opts) ->
    gen_mod:get_opt(matches, Opts);
matches(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, matches).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, name).

-spec search(gen_mod:opts() | global | binary()) -> boolean().
search(Opts) when is_map(Opts) ->
    gen_mod:get_opt(search, Opts);
search(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, search).

-spec use_cache(gen_mod:opts() | global | binary()) -> boolean().
use_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_cache, Opts);
use_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, use_cache).

-spec vcard(gen_mod:opts() | global | binary()) -> 'undefined' | tuple().
vcard(Opts) when is_map(Opts) ->
    gen_mod:get_opt(vcard, Opts);
vcard(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, vcard).

