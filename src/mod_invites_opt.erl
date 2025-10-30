%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_invites_opt).

-export([access_create_account/1]).
-export([db_type/1]).
-export([landing_page/1]).
-export([max_invites/1]).
-export([site_name/1]).
-export([templates_dir/1]).
-export([token_expire_seconds/1]).

-spec access_create_account(gen_mod:opts() | global | binary()) -> 'none' | acl:acl().
access_create_account(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_create_account, Opts);
access_create_account(Host) ->
    gen_mod:get_module_opt(Host, mod_invites, access_create_account).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_invites, db_type).

-spec landing_page(gen_mod:opts() | global | binary()) -> 'none' | binary().
landing_page(Opts) when is_map(Opts) ->
    gen_mod:get_opt(landing_page, Opts);
landing_page(Host) ->
    gen_mod:get_module_opt(Host, mod_invites, landing_page).

-spec max_invites(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_invites(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_invites, Opts);
max_invites(Host) ->
    gen_mod:get_module_opt(Host, mod_invites, max_invites).

-spec site_name(gen_mod:opts() | global | binary()) -> binary().
site_name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(site_name, Opts);
site_name(Host) ->
    gen_mod:get_module_opt(Host, mod_invites, site_name).

-spec templates_dir(gen_mod:opts() | global | binary()) -> binary().
templates_dir(Opts) when is_map(Opts) ->
    gen_mod:get_opt(templates_dir, Opts);
templates_dir(Host) ->
    gen_mod:get_module_opt(Host, mod_invites, templates_dir).

-spec token_expire_seconds(gen_mod:opts() | global | binary()) -> pos_integer().
token_expire_seconds(Opts) when is_map(Opts) ->
    gen_mod:get_opt(token_expire_seconds, Opts);
token_expire_seconds(Host) ->
    gen_mod:get_module_opt(Host, mod_invites, token_expire_seconds).

