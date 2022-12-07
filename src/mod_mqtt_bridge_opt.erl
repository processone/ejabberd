%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_mqtt_bridge_opt).

-export([replication_user/1]).
-export([servers/1]).

-spec replication_user(gen_mod:opts() | global | binary()) -> jid:jid().
replication_user(Opts) when is_map(Opts) ->
    gen_mod:get_opt(replication_user, Opts);
replication_user(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt_bridge, replication_user).

-spec servers(gen_mod:opts() | global | binary()) -> {[{atom(),'mqtt' | 'mqtts' | 'mqtt5' | 'mqtt5s',binary(),non_neg_integer(),#{binary()=>binary()},#{binary()=>binary()},map()}],#{binary()=>[atom()]}}.
servers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(servers, Opts);
servers(Host) ->
    gen_mod:get_module_opt(Host, mod_mqtt_bridge, servers).

