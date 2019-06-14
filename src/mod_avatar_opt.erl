%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_avatar_opt).

-export([convert/1]).
-export([rate_limit/1]).

-spec convert(gen_mod:opts() | global | binary()) -> [mod_avatar:convert_rule()].
convert(Opts) when is_map(Opts) ->
    gen_mod:get_opt(convert, Opts);
convert(Host) ->
    gen_mod:get_module_opt(Host, mod_avatar, convert).

-spec rate_limit(gen_mod:opts() | global | binary()) -> pos_integer().
rate_limit(Opts) when is_map(Opts) ->
    gen_mod:get_opt(rate_limit, Opts);
rate_limit(Host) ->
    gen_mod:get_module_opt(Host, mod_avatar, rate_limit).

