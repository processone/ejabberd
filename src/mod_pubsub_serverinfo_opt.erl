%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_pubsub_serverinfo_opt).

-export([pubsub_host/1]).

-spec pubsub_host(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
pubsub_host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(pubsub_host, Opts);
pubsub_host(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub_serverinfo, pubsub_host).

