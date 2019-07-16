%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_sip_opt).

-export([always_record_route/1]).
-export([flow_timeout_tcp/1]).
-export([flow_timeout_udp/1]).
-export([record_route/1]).
-export([routes/1]).
-export([via/1]).

-spec always_record_route(gen_mod:opts() | global | binary()) -> boolean().
always_record_route(Opts) when is_map(Opts) ->
    gen_mod:get_opt(always_record_route, Opts);
always_record_route(Host) ->
    gen_mod:get_module_opt(Host, mod_sip, always_record_route).

-spec flow_timeout_tcp(gen_mod:opts() | global | binary()) -> pos_integer().
flow_timeout_tcp(Opts) when is_map(Opts) ->
    gen_mod:get_opt(flow_timeout_tcp, Opts);
flow_timeout_tcp(Host) ->
    gen_mod:get_module_opt(Host, mod_sip, flow_timeout_tcp).

-spec flow_timeout_udp(gen_mod:opts() | global | binary()) -> pos_integer().
flow_timeout_udp(Opts) when is_map(Opts) ->
    gen_mod:get_opt(flow_timeout_udp, Opts);
flow_timeout_udp(Host) ->
    gen_mod:get_module_opt(Host, mod_sip, flow_timeout_udp).

-spec record_route(gen_mod:opts() | global | binary()) -> esip:uri().
record_route(Opts) when is_map(Opts) ->
    gen_mod:get_opt(record_route, Opts);
record_route(Host) ->
    gen_mod:get_module_opt(Host, mod_sip, record_route).

-spec routes(gen_mod:opts() | global | binary()) -> [esip:uri()].
routes(Opts) when is_map(Opts) ->
    gen_mod:get_opt(routes, Opts);
routes(Host) ->
    gen_mod:get_module_opt(Host, mod_sip, routes).

-spec via(gen_mod:opts() | global | binary()) -> [{'tcp' | 'tls' | 'udp',{binary(),1..65535 | 'undefined'}}].
via(Opts) when is_map(Opts) ->
    gen_mod:get_opt(via, Opts);
via(Host) ->
    gen_mod:get_module_opt(Host, mod_sip, via).

