%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_push_keepalive_opt).

-export([resume_timeout/1]).
-export([wake_on_start/1]).
-export([wake_on_timeout/1]).

-spec resume_timeout(gen_mod:opts() | global | binary()) -> non_neg_integer().
resume_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(resume_timeout, Opts);
resume_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_push_keepalive, resume_timeout).

-spec wake_on_start(gen_mod:opts() | global | binary()) -> boolean().
wake_on_start(Opts) when is_map(Opts) ->
    gen_mod:get_opt(wake_on_start, Opts);
wake_on_start(Host) ->
    gen_mod:get_module_opt(Host, mod_push_keepalive, wake_on_start).

-spec wake_on_timeout(gen_mod:opts() | global | binary()) -> boolean().
wake_on_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(wake_on_timeout, Opts);
wake_on_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_push_keepalive, wake_on_timeout).

