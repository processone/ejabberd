%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_register_opt).

-export([access/1]).
-export([access_from/1]).
-export([access_remove/1]).
-export([captcha_protected/1]).
-export([ip_access/1]).
-export([password_strength/1]).
-export([redirect_url/1]).
-export([registration_watchers/1]).
-export([welcome_message/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_register, access).

-spec access_from(gen_mod:opts() | global | binary()) -> 'none' | acl:acl().
access_from(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_from, Opts);
access_from(Host) ->
    gen_mod:get_module_opt(Host, mod_register, access_from).

-spec access_remove(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access_remove(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_remove, Opts);
access_remove(Host) ->
    gen_mod:get_module_opt(Host, mod_register, access_remove).

-spec captcha_protected(gen_mod:opts() | global | binary()) -> boolean().
captcha_protected(Opts) when is_map(Opts) ->
    gen_mod:get_opt(captcha_protected, Opts);
captcha_protected(Host) ->
    gen_mod:get_module_opt(Host, mod_register, captcha_protected).

-spec ip_access(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
ip_access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ip_access, Opts);
ip_access(Host) ->
    gen_mod:get_module_opt(Host, mod_register, ip_access).

-spec password_strength(gen_mod:opts() | global | binary()) -> number().
password_strength(Opts) when is_map(Opts) ->
    gen_mod:get_opt(password_strength, Opts);
password_strength(Host) ->
    gen_mod:get_module_opt(Host, mod_register, password_strength).

-spec redirect_url(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
redirect_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(redirect_url, Opts);
redirect_url(Host) ->
    gen_mod:get_module_opt(Host, mod_register, redirect_url).

-spec registration_watchers(gen_mod:opts() | global | binary()) -> [jid:jid()].
registration_watchers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(registration_watchers, Opts);
registration_watchers(Host) ->
    gen_mod:get_module_opt(Host, mod_register, registration_watchers).

-spec welcome_message(gen_mod:opts() | global | binary()) -> {binary(),binary()}.
welcome_message(Opts) when is_map(Opts) ->
    gen_mod:get_opt(welcome_message, Opts);
welcome_message(Host) ->
    gen_mod:get_module_opt(Host, mod_register, welcome_message).

