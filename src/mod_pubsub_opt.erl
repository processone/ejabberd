%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_pubsub_opt).

-export([access_createnode/1]).
-export([db_type/1]).
-export([default_node_config/1]).
-export([force_node_config/1]).
-export([host/1]).
-export([hosts/1]).
-export([ignore_pep_from_offline/1]).
-export([last_item_cache/1]).
-export([max_item_expire_node/1]).
-export([max_items_node/1]).
-export([max_nodes_discoitems/1]).
-export([max_subscriptions_node/1]).
-export([name/1]).
-export([nodetree/1]).
-export([pep_mapping/1]).
-export([plugins/1]).
-export([vcard/1]).

-spec access_createnode(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access_createnode(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_createnode, Opts);
access_createnode(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, access_createnode).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, db_type).

-spec default_node_config(gen_mod:opts() | global | binary()) -> [{atom(),atom() | integer()}].
default_node_config(Opts) when is_map(Opts) ->
    gen_mod:get_opt(default_node_config, Opts);
default_node_config(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, default_node_config).

-spec force_node_config(gen_mod:opts() | global | binary()) -> [{misc:re_mp(),[{atom(),atom() | integer()}]}].
force_node_config(Opts) when is_map(Opts) ->
    gen_mod:get_opt(force_node_config, Opts);
force_node_config(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, force_node_config).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, host).

-spec hosts(gen_mod:opts() | global | binary()) -> [binary()].
hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hosts, Opts);
hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, hosts).

-spec ignore_pep_from_offline(gen_mod:opts() | global | binary()) -> boolean().
ignore_pep_from_offline(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ignore_pep_from_offline, Opts);
ignore_pep_from_offline(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, ignore_pep_from_offline).

-spec last_item_cache(gen_mod:opts() | global | binary()) -> boolean().
last_item_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(last_item_cache, Opts);
last_item_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, last_item_cache).

-spec max_item_expire_node(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_item_expire_node(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_item_expire_node, Opts);
max_item_expire_node(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, max_item_expire_node).

-spec max_items_node(gen_mod:opts() | global | binary()) -> 'unlimited' | non_neg_integer().
max_items_node(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_items_node, Opts);
max_items_node(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, max_items_node).

-spec max_nodes_discoitems(gen_mod:opts() | global | binary()) -> 'infinity' | non_neg_integer().
max_nodes_discoitems(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_nodes_discoitems, Opts);
max_nodes_discoitems(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, max_nodes_discoitems).

-spec max_subscriptions_node(gen_mod:opts() | global | binary()) -> 'undefined' | non_neg_integer().
max_subscriptions_node(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_subscriptions_node, Opts);
max_subscriptions_node(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, max_subscriptions_node).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, name).

-spec nodetree(gen_mod:opts() | global | binary()) -> binary().
nodetree(Opts) when is_map(Opts) ->
    gen_mod:get_opt(nodetree, Opts);
nodetree(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, nodetree).

-spec pep_mapping(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
pep_mapping(Opts) when is_map(Opts) ->
    gen_mod:get_opt(pep_mapping, Opts);
pep_mapping(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, pep_mapping).

-spec plugins(gen_mod:opts() | global | binary()) -> [binary()].
plugins(Opts) when is_map(Opts) ->
    gen_mod:get_opt(plugins, Opts);
plugins(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, plugins).

-spec vcard(gen_mod:opts() | global | binary()) -> 'undefined' | tuple().
vcard(Opts) when is_map(Opts) ->
    gen_mod:get_opt(vcard, Opts);
vcard(Host) ->
    gen_mod:get_module_opt(Host, mod_pubsub, vcard).

