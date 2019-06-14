%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_http_upload_quota_opt).

-export([access_hard_quota/1]).
-export([access_soft_quota/1]).
-export([max_days/1]).

-spec access_hard_quota(gen_mod:opts() | global | binary()) -> atom() | [ejabberd_shaper:shaper_rule()].
access_hard_quota(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_hard_quota, Opts);
access_hard_quota(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload_quota, access_hard_quota).

-spec access_soft_quota(gen_mod:opts() | global | binary()) -> atom() | [ejabberd_shaper:shaper_rule()].
access_soft_quota(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_soft_quota, Opts);
access_soft_quota(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload_quota, access_soft_quota).

-spec max_days(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_days(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_days, Opts);
max_days(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload_quota, max_days).

