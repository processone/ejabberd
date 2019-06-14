%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_block_strangers_opt).

-export([access/1]).
-export([allow_local_users/1]).
-export([allow_transports/1]).
-export([captcha/1]).
-export([drop/1]).
-export([log/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'none' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_block_strangers, access).

-spec allow_local_users(gen_mod:opts() | global | binary()) -> boolean().
allow_local_users(Opts) when is_map(Opts) ->
    gen_mod:get_opt(allow_local_users, Opts);
allow_local_users(Host) ->
    gen_mod:get_module_opt(Host, mod_block_strangers, allow_local_users).

-spec allow_transports(gen_mod:opts() | global | binary()) -> boolean().
allow_transports(Opts) when is_map(Opts) ->
    gen_mod:get_opt(allow_transports, Opts);
allow_transports(Host) ->
    gen_mod:get_module_opt(Host, mod_block_strangers, allow_transports).

-spec captcha(gen_mod:opts() | global | binary()) -> boolean().
captcha(Opts) when is_map(Opts) ->
    gen_mod:get_opt(captcha, Opts);
captcha(Host) ->
    gen_mod:get_module_opt(Host, mod_block_strangers, captcha).

-spec drop(gen_mod:opts() | global | binary()) -> boolean().
drop(Opts) when is_map(Opts) ->
    gen_mod:get_opt(drop, Opts);
drop(Host) ->
    gen_mod:get_module_opt(Host, mod_block_strangers, drop).

-spec log(gen_mod:opts() | global | binary()) -> boolean().
log(Opts) when is_map(Opts) ->
    gen_mod:get_opt(log, Opts);
log(Host) ->
    gen_mod:get_module_opt(Host, mod_block_strangers, log).

