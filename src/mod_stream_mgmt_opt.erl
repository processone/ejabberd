%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_stream_mgmt_opt).

-export([ack_timeout/1]).
-export([cache_life_time/1]).
-export([cache_size/1]).
-export([max_ack_queue/1]).
-export([max_resume_timeout/1]).
-export([queue_type/1]).
-export([resend_on_timeout/1]).
-export([resume_timeout/1]).

-spec ack_timeout(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
ack_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ack_timeout, Opts);
ack_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_stream_mgmt, ack_timeout).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_stream_mgmt, cache_life_time).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_stream_mgmt, cache_size).

-spec max_ack_queue(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_ack_queue(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_ack_queue, Opts);
max_ack_queue(Host) ->
    gen_mod:get_module_opt(Host, mod_stream_mgmt, max_ack_queue).

-spec max_resume_timeout(gen_mod:opts() | global | binary()) -> 'undefined' | non_neg_integer().
max_resume_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_resume_timeout, Opts);
max_resume_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_stream_mgmt, max_resume_timeout).

-spec queue_type(gen_mod:opts() | global | binary()) -> 'file' | 'ram'.
queue_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(queue_type, Opts);
queue_type(Host) ->
    gen_mod:get_module_opt(Host, mod_stream_mgmt, queue_type).

-spec resend_on_timeout(gen_mod:opts() | global | binary()) -> 'false' | 'if_offline' | 'true'.
resend_on_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(resend_on_timeout, Opts);
resend_on_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_stream_mgmt, resend_on_timeout).

-spec resume_timeout(gen_mod:opts() | global | binary()) -> non_neg_integer().
resume_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(resume_timeout, Opts);
resume_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_stream_mgmt, resume_timeout).

