%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_host_meta_opt).

-export([bosh_service_url/1]).
-export([websocket_url/1]).

-spec bosh_service_url(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
bosh_service_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(bosh_service_url, Opts);
bosh_service_url(Host) ->
    gen_mod:get_module_opt(Host, mod_host_meta, bosh_service_url).

-spec websocket_url(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
websocket_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(websocket_url, Opts);
websocket_url(Host) ->
    gen_mod:get_module_opt(Host, mod_host_meta, websocket_url).

