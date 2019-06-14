%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_service_log_opt).

-export([loggers/1]).

-spec loggers(gen_mod:opts() | global | binary()) -> [binary()].
loggers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(loggers, Opts);
loggers(Host) ->
    gen_mod:get_module_opt(Host, mod_service_log, loggers).

