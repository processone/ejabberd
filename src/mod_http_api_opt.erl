%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_http_api_opt).

-export([admin_ip_access/1]).

-spec admin_ip_access(gen_mod:opts() | global | binary()) -> 'none' | acl:acl().
admin_ip_access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(admin_ip_access, Opts);
admin_ip_access(Host) ->
    gen_mod:get_module_opt(Host, mod_http_api, admin_ip_access).

