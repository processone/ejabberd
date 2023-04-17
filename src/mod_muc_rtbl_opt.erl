%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_muc_rtbl_opt).

-export([rtbl_node/1]).
-export([rtbl_server/1]).

-spec rtbl_node(gen_mod:opts() | global | binary()) -> binary().
rtbl_node(Opts) when is_map(Opts) ->
    gen_mod:get_opt(rtbl_node, Opts);
rtbl_node(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_rtbl, rtbl_node).

-spec rtbl_server(gen_mod:opts() | global | binary()) -> binary().
rtbl_server(Opts) when is_map(Opts) ->
    gen_mod:get_opt(rtbl_server, Opts);
rtbl_server(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_rtbl, rtbl_server).

