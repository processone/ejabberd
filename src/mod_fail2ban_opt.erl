%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_fail2ban_opt).

-export([access/1]).
-export([c2s_auth_ban_lifetime/1]).
-export([c2s_max_auth_failures/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'none' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_fail2ban, access).

-spec c2s_auth_ban_lifetime(gen_mod:opts() | global | binary()) -> pos_integer().
c2s_auth_ban_lifetime(Opts) when is_map(Opts) ->
    gen_mod:get_opt(c2s_auth_ban_lifetime, Opts);
c2s_auth_ban_lifetime(Host) ->
    gen_mod:get_module_opt(Host, mod_fail2ban, c2s_auth_ban_lifetime).

-spec c2s_max_auth_failures(gen_mod:opts() | global | binary()) -> pos_integer().
c2s_max_auth_failures(Opts) when is_map(Opts) ->
    gen_mod:get_opt(c2s_max_auth_failures, Opts);
c2s_max_auth_failures(Host) ->
    gen_mod:get_module_opt(Host, mod_fail2ban, c2s_max_auth_failures).

