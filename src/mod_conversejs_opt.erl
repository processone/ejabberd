%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_conversejs_opt).

-export([bosh_service_url/1]).
-export([conversejs_css/1]).
-export([conversejs_options/1]).
-export([conversejs_resources/1]).
-export([conversejs_script/1]).
-export([default_domain/1]).
-export([websocket_url/1]).

-spec bosh_service_url(gen_mod:opts() | global | binary()) -> 'auto' | binary().
bosh_service_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(bosh_service_url, Opts);
bosh_service_url(Host) ->
    gen_mod:get_module_opt(Host, mod_conversejs, bosh_service_url).

-spec conversejs_css(gen_mod:opts() | global | binary()) -> 'auto' | binary().
conversejs_css(Opts) when is_map(Opts) ->
    gen_mod:get_opt(conversejs_css, Opts);
conversejs_css(Host) ->
    gen_mod:get_module_opt(Host, mod_conversejs, conversejs_css).

-spec conversejs_options(gen_mod:opts() | global | binary()) -> [{binary(),binary() | integer()}].
conversejs_options(Opts) when is_map(Opts) ->
    gen_mod:get_opt(conversejs_options, Opts);
conversejs_options(Host) ->
    gen_mod:get_module_opt(Host, mod_conversejs, conversejs_options).

-spec conversejs_resources(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
conversejs_resources(Opts) when is_map(Opts) ->
    gen_mod:get_opt(conversejs_resources, Opts);
conversejs_resources(Host) ->
    gen_mod:get_module_opt(Host, mod_conversejs, conversejs_resources).

-spec conversejs_script(gen_mod:opts() | global | binary()) -> 'auto' | binary().
conversejs_script(Opts) when is_map(Opts) ->
    gen_mod:get_opt(conversejs_script, Opts);
conversejs_script(Host) ->
    gen_mod:get_module_opt(Host, mod_conversejs, conversejs_script).

-spec default_domain(gen_mod:opts() | global | binary()) -> binary().
default_domain(Opts) when is_map(Opts) ->
    gen_mod:get_opt(default_domain, Opts);
default_domain(Host) ->
    gen_mod:get_module_opt(Host, mod_conversejs, default_domain).

-spec websocket_url(gen_mod:opts() | global | binary()) -> 'auto' | binary().
websocket_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(websocket_url, Opts);
websocket_url(Host) ->
    gen_mod:get_module_opt(Host, mod_conversejs, websocket_url).

