%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_auth_fast_opt).

-export([db_type/1]).
-export([token_lifetime/1]).
-export([token_refresh_age/1]).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_auth_fast, db_type).

-spec token_lifetime(gen_mod:opts() | global | binary()) -> pos_integer().
token_lifetime(Opts) when is_map(Opts) ->
    gen_mod:get_opt(token_lifetime, Opts);
token_lifetime(Host) ->
    gen_mod:get_module_opt(Host, mod_auth_fast, token_lifetime).

-spec token_refresh_age(gen_mod:opts() | global | binary()) -> pos_integer().
token_refresh_age(Opts) when is_map(Opts) ->
    gen_mod:get_opt(token_refresh_age, Opts);
token_refresh_age(Host) ->
    gen_mod:get_module_opt(Host, mod_auth_fast, token_refresh_age).

