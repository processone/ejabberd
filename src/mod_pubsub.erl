%%%----------------------------------------------------------------------
%%% File    : mod_pubsub.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Publish Subscribe service (XEP-0060)
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

%%% Support for subscription-options and multi-subscribe features was
%%% added by Brian Cully (bjc AT kublai.com). Subscriptions and options are
%%% stored in the pubsub_subscription table, with a link to them provided
%%% by the subscriptions field of pubsub_state. For information on
%%% subscription-options and mulit-subscribe see XEP-0060 sections 6.1.6,
%%% 6.2.3.1, 6.2.3.5, and 6.3. For information on subscription leases see
%%% XEP-0060 section 12.18.

-module(mod_pubsub).
-behaviour(gen_mod).
-behaviour(gen_server).
-author('christophe.romain@process-one.net').
-protocol({xep, 60, '1.14'}).
-protocol({xep, 163, '1.2'}).
-protocol({xep, 248, '0.2'}).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("pubsub.hrl").
-include("mod_roster.hrl").
-include("translate.hrl").
-include("ejabberd_stacktrace.hrl").
-include("ejabberd_commands.hrl").

-define(STDTREE, <<"tree">>).
-define(STDNODE, <<"flat">>).
-define(PEPNODE, <<"pep">>).

%% exports for hooks
-export([presence_probe/3, caps_add/3, caps_update/3,
    in_subscription/2, out_subscription/1,
    on_self_presence/1, on_user_offline/2, remove_user/2,
    disco_local_identity/5, disco_local_features/5,
    disco_local_items/5, disco_sm_identity/5,
    disco_sm_features/5, disco_sm_items/5,
    c2s_handle_info/2]).

%% exported iq handlers
-export([iq_sm/1, process_disco_info/1, process_disco_items/1,
	 process_pubsub/1, process_pubsub_owner/1, process_vcard/1,
	 process_commands/1]).

%% exports for console debug manual use
-export([create_node/5, create_node/7, delete_node/3,
    subscribe_node/5, unsubscribe_node/5, publish_item/6, publish_item/8,
    delete_item/4, delete_item/5, send_items/7, get_items/2, get_item/3,
    get_cached_item/2, get_configure/5, set_configure/5,
    tree_action/3, node_action/4, node_call/4]).

%% general helpers for plugins
-export([extended_error/2, service_jid/1,
    tree/1, tree/2, plugin/2, plugins/1, config/3,
    host/1, serverhost/1]).

%% pubsub#errors
-export([err_closed_node/0, err_configuration_required/0,
	 err_invalid_jid/0, err_invalid_options/0, err_invalid_payload/0,
	 err_invalid_subid/0, err_item_forbidden/0, err_item_required/0,
	 err_jid_required/0, err_max_items_exceeded/0, err_max_nodes_exceeded/0,
	 err_nodeid_required/0, err_not_in_roster_group/0, err_not_subscribed/0,
	 err_payload_too_big/0, err_payload_required/0,
	 err_pending_subscription/0, err_precondition_not_met/0,
	 err_presence_subscription_required/0, err_subid_required/0,
	 err_too_many_subscriptions/0, err_unsupported/1,
	 err_unsupported_access_model/0]).

%% API and gen_server callbacks
-export([start/2, stop/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2, mod_doc/0,
    terminate/2, code_change/3, depends/2, mod_opt_type/1, mod_options/1]).

%% ejabberd commands
-export([get_commands_spec/0, delete_old_items/1, delete_expired_items/0]).

-export([route/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

-export_type([
	host/0,
	hostPubsub/0,
	hostPEP/0,
	%%
	nodeIdx/0,
	nodeId/0,
	itemId/0,
	subId/0,
	payload/0,
	%%
	nodeOption/0,
	nodeOptions/0,
	subOption/0,
	subOptions/0,
	pubOption/0,
	pubOptions/0,
	%%
	affiliation/0,
	subscription/0,
	accessModel/0,
	publishModel/0
	]).

%% -type payload() defined here because the -type xmlel() is not accessible
%% from pubsub.hrl
-type(payload() :: [] | [xmlel(),...]).

-export_type([
	pubsubNode/0,
	pubsubState/0,
	pubsubItem/0,
	pubsubSubscription/0,
	pubsubLastItem/0
	]).

-type(pubsubNode() ::
    #pubsub_node{
	nodeid  :: {Host::mod_pubsub:host(), Node::mod_pubsub:nodeId()},
	id      :: Nidx::mod_pubsub:nodeIdx(),
	parents :: [Node::mod_pubsub:nodeId()],
	type    :: Type::binary(),
	owners  :: [Owner::ljid(),...],
	options :: Opts::mod_pubsub:nodeOptions()
	}
    ).

-type(pubsubState() ::
    #pubsub_state{
	stateid       :: {Entity::ljid(), Nidx::mod_pubsub:nodeIdx()},
	nodeidx       :: Nidx::mod_pubsub:nodeIdx(),
	items         :: [ItemId::mod_pubsub:itemId()],
	affiliation   :: Affs::mod_pubsub:affiliation(),
	subscriptions :: [{Sub::mod_pubsub:subscription(), SubId::mod_pubsub:subId()}]
	}
    ).

-type(pubsubItem() ::
    #pubsub_item{
	itemid       :: {ItemId::mod_pubsub:itemId(), Nidx::mod_pubsub:nodeIdx()},
	nodeidx      :: Nidx::mod_pubsub:nodeIdx(),
	creation     :: {erlang:timestamp(), ljid()},
	modification :: {erlang:timestamp(), ljid()},
	payload      :: mod_pubsub:payload()
	}
    ).

-type(pubsubSubscription() ::
    #pubsub_subscription{
	subid   :: SubId::mod_pubsub:subId(),
	options :: [] | mod_pubsub:subOptions()
	}
    ).

-type(pubsubLastItem() ::
    #pubsub_last_item{
	nodeid   :: {binary(), mod_pubsub:nodeIdx()},
	itemid   :: mod_pubsub:itemId(),
	creation :: {erlang:timestamp(), ljid()},
	payload  :: mod_pubsub:payload()
	}
    ).

-record(state,
    {
	server_host,
	hosts,
	access,
	pep_mapping             = [],
	ignore_pep_from_offline = true,
	last_item_cache         = false,
	max_items_node          = ?MAXITEMS,
	max_subscriptions_node  = undefined,
	default_node_config     = [],
	nodetree                = <<"nodetree_", (?STDTREE)/binary>>,
	plugins                 = [?STDNODE],
	db_type
	}).

-type(state() ::
    #state{
	server_host             :: binary(),
	hosts                   :: [mod_pubsub:hostPubsub()],
	access                  :: atom(),
	pep_mapping             :: [{binary(), binary()}],
	ignore_pep_from_offline :: boolean(),
	last_item_cache         :: boolean(),
	max_items_node          :: non_neg_integer()|unlimited,
	max_subscriptions_node  :: non_neg_integer()|undefined,
	default_node_config     :: [{atom(), binary()|boolean()|integer()|atom()}],
	nodetree                :: binary(),
	plugins                 :: [binary(),...],
	db_type                 :: atom()
	}

    ).

-type subs_by_depth() :: [{integer(), [{#pubsub_node{}, [{ljid(), subId(), subOptions()}]}]}].

start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec init([binary() | [{_,_}],...]) -> {'ok',state()}.

init([ServerHost|_]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(ServerHost, ?MODULE),
    Hosts = gen_mod:get_opt_hosts(Opts),
    Access = mod_pubsub_opt:access_createnode(Opts),
    PepOffline = mod_pubsub_opt:ignore_pep_from_offline(Opts),
    LastItemCache = mod_pubsub_opt:last_item_cache(Opts),
    MaxItemsNode = mod_pubsub_opt:max_items_node(Opts),
    MaxSubsNode = mod_pubsub_opt:max_subscriptions_node(Opts),
    ejabberd_mnesia:create(?MODULE, pubsub_last_item,
			   [{ram_copies, [node()]},
			    {attributes, record_info(fields, pubsub_last_item)}]),
    DBMod = gen_mod:db_mod(Opts, ?MODULE),
    AllPlugins =
	lists:flatmap(
	  fun(Host) ->
		  DBMod:init(Host, ServerHost, Opts),
		  ejabberd_router:register_route(
		    Host, ServerHost, {apply, ?MODULE, route}),
		  {Plugins, NodeTree, PepMapping} = init_plugins(Host, ServerHost, Opts),
		  DefaultModule = plugin(Host, hd(Plugins)),
		  DefaultNodeCfg = merge_config(
				     [mod_pubsub_opt:default_node_config(Opts),
				      DefaultModule:options()]),
		  lists:foreach(
		    fun(H) ->
			    T = gen_mod:get_module_proc(H, config),
			    try
				ets:new(T, [set, named_table]),
				ets:insert(T, {nodetree, NodeTree}),
				ets:insert(T, {plugins, Plugins}),
				ets:insert(T, {last_item_cache, LastItemCache}),
				ets:insert(T, {max_items_node, MaxItemsNode}),
				ets:insert(T, {max_subscriptions_node, MaxSubsNode}),
				ets:insert(T, {default_node_config, DefaultNodeCfg}),
				ets:insert(T, {pep_mapping, PepMapping}),
				ets:insert(T, {ignore_pep_from_offline, PepOffline}),
				ets:insert(T, {host, Host}),
				ets:insert(T, {access, Access})
			    catch error:badarg when H == ServerHost ->
				    ok
			    end
		    end, [Host, ServerHost]),
		  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
						?MODULE, process_disco_info),
		  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
						?MODULE, process_disco_items),
		  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PUBSUB,
						?MODULE, process_pubsub),
		  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PUBSUB_OWNER,
						?MODULE, process_pubsub_owner),
		  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
						?MODULE, process_vcard),
		  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_COMMANDS,
						?MODULE, process_commands),
		  Plugins
	  end, Hosts),
    ejabberd_hooks:add(c2s_self_presence, ServerHost,
	?MODULE, on_self_presence, 75),
    ejabberd_hooks:add(c2s_terminated, ServerHost,
	?MODULE, on_user_offline, 75),
    ejabberd_hooks:add(disco_local_identity, ServerHost,
	?MODULE, disco_local_identity, 75),
    ejabberd_hooks:add(disco_local_features, ServerHost,
	?MODULE, disco_local_features, 75),
    ejabberd_hooks:add(disco_local_items, ServerHost,
	?MODULE, disco_local_items, 75),
    ejabberd_hooks:add(presence_probe_hook, ServerHost,
	?MODULE, presence_probe, 80),
    ejabberd_hooks:add(roster_in_subscription, ServerHost,
	?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, ServerHost,
	?MODULE, out_subscription, 50),
    ejabberd_hooks:add(remove_user, ServerHost,
	?MODULE, remove_user, 50),
    ejabberd_hooks:add(c2s_handle_info, ServerHost,
	?MODULE, c2s_handle_info, 50),
    case lists:member(?PEPNODE, AllPlugins) of
	true ->
	    ejabberd_hooks:add(caps_add, ServerHost,
		?MODULE, caps_add, 80),
	    ejabberd_hooks:add(caps_update, ServerHost,
		?MODULE, caps_update, 80),
	    ejabberd_hooks:add(disco_sm_identity, ServerHost,
		?MODULE, disco_sm_identity, 75),
	    ejabberd_hooks:add(disco_sm_features, ServerHost,
		?MODULE, disco_sm_features, 75),
	    ejabberd_hooks:add(disco_sm_items, ServerHost,
		?MODULE, disco_sm_items, 75),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHost,
		?NS_PUBSUB, ?MODULE, iq_sm),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHost,
		?NS_PUBSUB_OWNER, ?MODULE, iq_sm);
	false ->
	    ok
    end,
    ejabberd_commands:register_commands(?MODULE, get_commands_spec()),
    NodeTree = config(ServerHost, nodetree),
    Plugins = config(ServerHost, plugins),
    PepMapping = config(ServerHost, pep_mapping),
    DBType = mod_pubsub_opt:db_type(ServerHost),
    {ok, #state{hosts = Hosts, server_host = ServerHost,
		access = Access, pep_mapping = PepMapping,
		ignore_pep_from_offline = PepOffline,
		last_item_cache = LastItemCache,
		max_items_node = MaxItemsNode, nodetree = NodeTree,
		plugins = Plugins, db_type = DBType}}.

depends(ServerHost, Opts) ->
    [Host|_] = gen_mod:get_opt_hosts(Opts),
    Plugins = mod_pubsub_opt:plugins(Opts),
    Db = mod_pubsub_opt:db_type(Opts),
    lists:flatmap(
      fun(Name) ->
	      Plugin = plugin(Db, Name),
	      try apply(Plugin, depends, [Host, ServerHost, Opts])
	      catch _:undef -> []
	      end
      end, Plugins).

%% @doc Call the init/1 function for each plugin declared in the config file.
%% The default plugin module is implicit.
%% <p>The Erlang code for the plugin is located in a module called
%% <em>node_plugin</em>. The 'node_' prefix is mandatory.</p>
%% <p>See {@link node_hometree:init/1} for an example implementation.</p>
init_plugins(Host, ServerHost, Opts) ->
    TreePlugin = tree(Host, mod_pubsub_opt:nodetree(Opts)),
    TreePlugin:init(Host, ServerHost, Opts),
    Plugins = mod_pubsub_opt:plugins(Opts),
    PepMapping = mod_pubsub_opt:pep_mapping(Opts),
    PluginsOK = lists:foldl(
	    fun (Name, Acc) ->
		    Plugin = plugin(Host, Name),
		    apply(Plugin, init, [Host, ServerHost, Opts]),
		    [Name | Acc]
	    end,
	    [], Plugins),
    {lists:reverse(PluginsOK), TreePlugin, PepMapping}.

terminate_plugins(Host, ServerHost, Plugins, TreePlugin) ->
    lists:foreach(
	fun (Name) ->
		Plugin = plugin(Host, Name),
		Plugin:terminate(Host, ServerHost)
	end,
	Plugins),
    TreePlugin:terminate(Host, ServerHost),
    ok.

%% -------
%% disco hooks handling functions
%%

-spec disco_local_identity([identity()], jid(), jid(),
			   binary(), binary()) -> [identity()].
disco_local_identity(Acc, _From, To, <<>>, _Lang) ->
    case lists:member(?PEPNODE, plugins(host(To#jid.lserver))) of
	true ->
	    [#identity{category = <<"pubsub">>, type = <<"pep">>} | Acc];
	false ->
	    Acc
    end;
disco_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec disco_local_features({error, stanza_error()} | {result, [binary()]} | empty,
			   jid(), jid(), binary(), binary()) ->
				  {error, stanza_error()} | {result, [binary()]} | empty.
disco_local_features(Acc, _From, To, <<>>, _Lang) ->
    Host = host(To#jid.lserver),
    Feats = case Acc of
	{result, I} -> I;
	_ -> []
    end,
    {result, Feats ++ [?NS_PUBSUB|[feature(F) || F <- features(Host, <<>>)]]};
disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec disco_local_items({error, stanza_error()} | {result, [disco_item()]} | empty,
			jid(), jid(), binary(), binary()) ->
			       {error, stanza_error()} | {result, [disco_item()]} | empty.
disco_local_items(Acc, _From, _To, <<>>, _Lang) -> Acc;
disco_local_items(Acc, _From, _To, _Node, _Lang) -> Acc.

-spec disco_sm_identity([identity()], jid(), jid(),
			binary(), binary()) -> [identity()].
disco_sm_identity(Acc, From, To, Node, _Lang) ->
    disco_identity(jid:tolower(jid:remove_resource(To)), Node, From)
    ++ Acc.

-spec disco_identity(host(), binary(), jid()) -> [identity()].
disco_identity(_Host, <<>>, _From) ->
    [#identity{category = <<"pubsub">>, type = <<"pep">>}];
disco_identity(Host, Node, From) ->
    Action =
	fun(#pubsub_node{id = Nidx, type = Type,
			 options = Options, owners = O}) ->
		Owners = node_owners_call(Host, Type, Nidx, O),
		case get_allowed_items_call(Host, Nidx, From, Type,
					    Options, Owners) of
		    {result, _} ->
			{result, [#identity{category = <<"pubsub">>, type = <<"pep">>},
				  #identity{category = <<"pubsub">>, type = <<"leaf">>,
					    name = get_option(Options, title, <<>>)}]};
		    _ ->
			{result, []}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

-spec disco_sm_features({error, stanza_error()} | {result, [binary()]} | empty,
			jid(), jid(), binary(), binary()) ->
			       {error, stanza_error()} | {result, [binary()]}.
disco_sm_features(empty, From, To, Node, Lang) ->
    disco_sm_features({result, []}, From, To, Node, Lang);
disco_sm_features({result, OtherFeatures} = _Acc, From, To, Node, _Lang) ->
    {result,
     OtherFeatures ++
	 disco_features(jid:tolower(jid:remove_resource(To)), Node, From)};
disco_sm_features(Acc, _From, _To, _Node, _Lang) -> Acc.

-spec disco_features(ljid(), binary(), jid()) -> [binary()].
disco_features(Host, <<>>, _From) ->
    [?NS_PUBSUB | [feature(F) || F <- plugin_features(Host, <<"pep">>)]];
disco_features(Host, Node, From) ->
    Action =
	fun(#pubsub_node{id = Nidx, type = Type,
			 options = Options, owners = O}) ->
		Owners = node_owners_call(Host, Type, Nidx, O),
		case get_allowed_items_call(Host, Nidx, From,
					    Type, Options, Owners) of
		    {result, _} ->
			{result,
			 [?NS_PUBSUB | [feature(F) || F <- plugin_features(Host, <<"pep">>)]]};
		    _ ->
			{result, []}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

-spec disco_sm_items({error, stanza_error()} | {result, [disco_item()]} | empty,
		     jid(), jid(), binary(), binary()) ->
			    {error, stanza_error()} | {result, [disco_item()]}.
disco_sm_items(empty, From, To, Node, Lang) ->
    disco_sm_items({result, []}, From, To, Node, Lang);
disco_sm_items({result, OtherItems}, From, To, Node, _Lang) ->
    {result, lists:usort(OtherItems ++
	    disco_items(jid:tolower(jid:remove_resource(To)), Node, From))};
disco_sm_items(Acc, _From, _To, _Node, _Lang) -> Acc.

-spec disco_items(ljid(), binary(), jid()) -> [disco_item()].
disco_items(Host, <<>>, From) ->
    MaxNodes = mod_pubsub_opt:max_nodes_discoitems(serverhost(Host)),
    Action =
	fun(#pubsub_node{nodeid = {_, Node}, options = Options,
			 type = Type, id = Nidx, owners = O}, Acc) ->
		Owners = node_owners_call(Host, Type, Nidx, O),
		case get_allowed_items_call(Host, Nidx, From,
					    Type, Options, Owners) of
		    {result, _} ->
			[#disco_item{node = Node,
				     jid = jid:make(Host),
				     name = get_option(Options, title, <<>>)} | Acc];
		    _ ->
			Acc
		end
	end,
    NodeBloc = fun() ->
		       case tree_call(Host, get_nodes, [Host, MaxNodes]) of
			   Nodes when is_list(Nodes) ->
			       {result, lists:foldl(Action, [], Nodes)};
			   Error ->
			       Error
		       end
	       end,
    case transaction(Host, NodeBloc, sync_dirty) of
	{result, Items} -> Items;
	_ -> []
    end;
disco_items(Host, Node, From) ->
    Action =
	fun(#pubsub_node{id = Nidx, type = Type,
			 options = Options, owners = O}) ->
		Owners = node_owners_call(Host, Type, Nidx, O),
		case get_allowed_items_call(Host, Nidx, From,
					    Type, Options, Owners) of
		    {result, Items} ->
			{result, [#disco_item{jid = jid:make(Host),
					      name = ItemId}
				  || #pubsub_item{itemid = {ItemId, _}} <- Items]};
		    _ ->
			{result, []}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

%% -------
%% presence and session hooks handling functions
%%

-spec caps_add(jid(), jid(), [binary()]) -> ok.
caps_add(JID, JID, Features) ->
    %% Send the owner his last PEP items.
    send_last_pep(JID, JID, Features);
caps_add(#jid{lserver = S1} = From, #jid{lserver = S2} = To, Features)
  when S1 =/= S2 ->
    %% When a remote contact goes online while the local user is offline, the
    %% remote contact won't receive last items from the local user even if
    %% ignore_pep_from_offline is set to false. To work around this issue a bit,
    %% we'll also send the last items to remote contacts when the local user
    %% connects. That's the reason to use the caps_add hook instead of the
    %% presence_probe_hook for remote contacts: The latter is only called when a
    %% contact becomes available; the former is also executed when the local
    %% user goes online (because that triggers the contact to send a presence
    %% packet with CAPS).
    send_last_pep(To, From, Features);
caps_add(_From, _To, _Features) ->
    ok.

-spec caps_update(jid(), jid(), [binary()]) -> ok.
caps_update(From, To, Features) ->
    send_last_pep(To, From, Features).

-spec presence_probe(jid(), jid(), pid()) -> ok.
presence_probe(#jid{luser = U, lserver = S}, #jid{luser = U, lserver = S}, _Pid) ->
    %% ignore presence_probe from my other resources
    ok;
presence_probe(#jid{lserver = S} = From, #jid{lserver = S} = To, _Pid) ->
    send_last_pep(To, From, unknown);
presence_probe(_From, _To, _Pid) ->
    %% ignore presence_probe from remote contacts, those are handled via caps_add
    ok.

-spec on_self_presence({presence(), ejabberd_c2s:state()})
		    -> {presence(), ejabberd_c2s:state()}.
on_self_presence({_, #{pres_last := _}} = Acc) -> % Just a presence update.
    Acc;
on_self_presence({#presence{type = available}, #{jid := JID}} = Acc) ->
    send_last_items(JID),
    Acc;
on_self_presence(Acc) ->
    Acc.

-spec on_user_offline(ejabberd_c2s:state(), atom()) -> ejabberd_c2s:state().
on_user_offline(#{jid := JID} = C2SState, _Reason) ->
    purge_offline(jid:tolower(JID)),
    C2SState;
on_user_offline(C2SState, _Reason) ->
    C2SState.

%% -------
%% subscription hooks handling functions
%%

-spec out_subscription(presence()) -> any().
out_subscription(#presence{type = subscribed, from = From, to = To}) ->
    if From#jid.lserver == To#jid.lserver ->
	    send_last_pep(jid:remove_resource(From), To, unknown);
       true ->
           ok
    end;
out_subscription(_) ->
    ok.

-spec in_subscription(boolean(), presence()) -> true.
in_subscription(_, #presence{to = To, from = Owner, type = unsubscribed}) ->
    unsubscribe_user(jid:remove_resource(To), Owner),
    true;
in_subscription(_, _) ->
    true.

-spec unsubscribe_user(jid(), jid()) -> ok.
unsubscribe_user(Entity, Owner) ->
    lists:foreach(
      fun(ServerHost) ->
	      unsubscribe_user(ServerHost, Entity, Owner)
      end,
      lists:usort(
	lists:foldl(
	  fun(UserHost, Acc) ->
		  case gen_mod:is_loaded(UserHost, mod_pubsub) of
		      true -> [UserHost|Acc];
		      false -> Acc
		  end
	  end, [], [Entity#jid.lserver, Owner#jid.lserver]))).

-spec unsubscribe_user(binary(), jid(), jid()) -> ok.
unsubscribe_user(Host, Entity, Owner) ->
    BJID = jid:tolower(jid:remove_resource(Owner)),
    lists:foreach(
      fun (PType) ->
	      case node_action(Host, PType,
			       get_entity_subscriptions,
			       [Host, Entity]) of
		  {result, Subs} ->
		      lists:foreach(
			fun({#pubsub_node{options = Options,
					  owners = O,
					  id = Nidx},
			     subscribed, _, JID}) ->
				Unsubscribe = match_option(Options, access_model, presence)
				    andalso lists:member(BJID, node_owners_action(Host, PType, Nidx, O)),
				case Unsubscribe of
				    true ->
					node_action(Host, PType,
						    unsubscribe_node, [Nidx, Entity, JID, all]);
				    false ->
					ok
				end;
			   (_) ->
				ok
			end, Subs);
		  _ ->
		      ok
	      end
      end, plugins(Host)).

%% -------
%% user remove hook handling function
%%

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Entity = jid:make(LUser, LServer),
    Host = host(LServer),
    HomeTreeBase = <<"/home/", LServer/binary, "/", LUser/binary>>,
    lists:foreach(
      fun(PType) ->
	      case node_action(Host, PType,
			       get_entity_subscriptions,
			       [Host, Entity]) of
		  {result, Subs} ->
		      lists:foreach(
			fun({#pubsub_node{id = Nidx}, _, _, JID}) ->
				node_action(Host, PType,
					    unsubscribe_node,
					    [Nidx, Entity, JID, all]);
			   (_) ->
				ok
			end, Subs),
		      case node_action(Host, PType,
				       get_entity_affiliations,
				       [Host, Entity]) of
			  {result, Affs} ->
			      lists:foreach(
				fun({#pubsub_node{nodeid = {H, N}, parents = []}, owner}) ->
					delete_node(H, N, Entity);
				   ({#pubsub_node{nodeid = {H, N}, type = Type}, owner})
				      when N == HomeTreeBase, Type == <<"hometree">> ->
					delete_node(H, N, Entity);
				   ({#pubsub_node{id = Nidx}, _}) ->
					case node_action(Host, PType,
							 get_state,
							 [Nidx, jid:tolower(Entity)]) of
					    {result, #pubsub_state{items = ItemIds}} ->
						node_action(Host, PType,
							    remove_extra_items,
							    [Nidx, 0, ItemIds]),
						node_action(Host, PType,
							    set_affiliation,
							    [Nidx, Entity, none]);
					    _ ->
						ok
					end
				end, Affs);
			  _ ->
			      ok
		      end;
		  _ ->
		      ok
	      end
      end, plugins(Host)).

handle_call(server_host, _From, State) ->
    {reply, State#state.server_host, State};
handle_call(plugins, _From, State) ->
    {reply, State#state.plugins, State};
handle_call(pep_mapping, _From, State) ->
    {reply, State#state.pep_mapping, State};
handle_call(nodetree, _From, State) ->
    {reply, State#state.nodetree, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({route, Packet}, State) ->
    try route(Packet)
    catch ?EX_RULE(Class, Reason, St) ->
	    StackTrace = ?EX_STACK(St),
	    ?ERROR_MSG("Failed to route packet:~n~ts~n** ~ts",
		       [xmpp:pp(Packet),
			misc:format_exception(2, Class, Reason, StackTrace)])
    end,
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason,
	    #state{hosts = Hosts, server_host = ServerHost, nodetree = TreePlugin, plugins = Plugins}) ->
    case lists:member(?PEPNODE, Plugins) of
	true ->
	    ejabberd_hooks:delete(caps_add, ServerHost,
		?MODULE, caps_add, 80),
	    ejabberd_hooks:delete(caps_update, ServerHost,
		?MODULE, caps_update, 80),
	    ejabberd_hooks:delete(disco_sm_identity, ServerHost,
		?MODULE, disco_sm_identity, 75),
	    ejabberd_hooks:delete(disco_sm_features, ServerHost,
		?MODULE, disco_sm_features, 75),
	    ejabberd_hooks:delete(disco_sm_items, ServerHost,
		?MODULE, disco_sm_items, 75),
	    gen_iq_handler:remove_iq_handler(ejabberd_sm,
		ServerHost, ?NS_PUBSUB),
	    gen_iq_handler:remove_iq_handler(ejabberd_sm,
		ServerHost, ?NS_PUBSUB_OWNER);
	false ->
	    ok
    end,
    ejabberd_hooks:delete(c2s_self_presence, ServerHost,
	?MODULE, on_self_presence, 75),
    ejabberd_hooks:delete(c2s_terminated, ServerHost,
	?MODULE, on_user_offline, 75),
    ejabberd_hooks:delete(disco_local_identity, ServerHost,
	?MODULE, disco_local_identity, 75),
    ejabberd_hooks:delete(disco_local_features, ServerHost,
	?MODULE, disco_local_features, 75),
    ejabberd_hooks:delete(disco_local_items, ServerHost,
	?MODULE, disco_local_items, 75),
    ejabberd_hooks:delete(presence_probe_hook, ServerHost,
	?MODULE, presence_probe, 80),
    ejabberd_hooks:delete(roster_in_subscription, ServerHost,
	?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, ServerHost,
	?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(remove_user, ServerHost,
	?MODULE, remove_user, 50),
    ejabberd_hooks:delete(c2s_handle_info, ServerHost,
	?MODULE, c2s_handle_info, 50),
    lists:foreach(
      fun(Host) ->
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PUBSUB),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PUBSUB_OWNER),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_COMMANDS),
	      terminate_plugins(Host, ServerHost, Plugins, TreePlugin),
	      ejabberd_router:unregister_route(Host)
      end, Hosts),
    case gen_mod:is_loaded_elsewhere(ServerHost, ?MODULE) of
	false ->
	    ejabberd_commands:unregister_commands(get_commands_spec());
	true ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec process_disco_info(iq()) -> iq().
process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{from = From, to = To, lang = Lang, type = get,
		       sub_els = [#disco_info{node = Node}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    Info = ejabberd_hooks:run_fold(disco_info, ServerHost,
				   [],
				   [ServerHost, ?MODULE, <<>>, <<>>]),
    case iq_disco_info(ServerHost, Host, Node, From, Lang) of
	{result, IQRes} ->
	    XData = IQRes#disco_info.xdata ++ Info,
	    xmpp:make_iq_result(IQ, IQRes#disco_info{node = Node, xdata = XData});
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end.

-spec process_disco_items(iq()) -> iq().
process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get, from = From, to = To,
                        sub_els = [#disco_items{node = Node} = SubEl]} = IQ) ->
    Host = To#jid.lserver,
    case iq_disco_items(Host, Node, From, SubEl#disco_items.rsm) of
	{result, IQRes} ->
	    xmpp:make_iq_result(IQ, IQRes#disco_items{node = Node});
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end.

-spec process_pubsub(iq()) -> iq().
process_pubsub(#iq{to = To} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    Access = config(ServerHost, access),
    case iq_pubsub(Host, Access, IQ) of
	{result, IQRes} ->
	    xmpp:make_iq_result(IQ, IQRes);
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end.

-spec process_pubsub_owner(iq()) -> iq().
process_pubsub_owner(#iq{to = To} = IQ) ->
    Host = To#jid.lserver,
    case iq_pubsub_owner(Host, IQ) of
	{result, IQRes} ->
	    xmpp:make_iq_result(IQ, IQRes);
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end.

-spec process_vcard(iq()) -> iq().
process_vcard(#iq{type = get, to = To, lang = Lang} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    xmpp:make_iq_result(IQ, iq_get_vcard(ServerHost, Lang));
process_vcard(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)).

-spec process_commands(iq()) -> iq().
process_commands(#iq{type = set, to = To, from = From,
		     sub_els = [#adhoc_command{} = Request]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    Plugins = config(ServerHost, plugins),
    Access = config(ServerHost, access),
    case adhoc_request(Host, ServerHost, From, Request, Access, Plugins) of
	{error, Error} ->
	    xmpp:make_error(IQ, Error);
	Response ->
	    xmpp:make_iq_result(
	      IQ, xmpp_util:make_adhoc_response(Request, Response))
    end;
process_commands(#iq{type = get, lang = Lang} = IQ) ->
    Txt = ?T("Value 'get' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)).

-spec route(stanza()) -> ok.
route(#iq{to = To} = IQ) when To#jid.lresource == <<"">> ->
    ejabberd_router:process_iq(IQ);
route(Packet) ->
    To = xmpp:get_to(Packet),
    case To of
	#jid{luser = <<>>, lresource = <<>>} ->
	    case Packet of
		#message{type = T} when T /= error ->
		    case find_authorization_response(Packet) of
			undefined ->
			    ok;
			{error, Err} ->
			    ejabberd_router:route_error(Packet, Err);
			AuthResponse ->
			    handle_authorization_response(
			      To#jid.lserver, Packet, AuthResponse)
		    end;
		_ ->
		    Err = xmpp:err_service_unavailable(),
		    ejabberd_router:route_error(Packet, Err)
	    end;
	_ ->
	    Err = xmpp:err_item_not_found(),
	    ejabberd_router:route_error(Packet, Err)
    end.

-spec command_disco_info(binary(), binary(), jid()) -> {result, disco_info()}.
command_disco_info(_Host, ?NS_COMMANDS, _From) ->
    {result, #disco_info{identities = [#identity{category = <<"automation">>,
						 type = <<"command-list">>}]}};
command_disco_info(_Host, ?NS_PUBSUB_GET_PENDING, _From) ->
    {result, #disco_info{identities = [#identity{category = <<"automation">>,
						 type = <<"command-node">>}],
			 features = [?NS_COMMANDS]}}.

-spec node_disco_info(binary(), binary(), jid()) -> {result, disco_info()} |
						    {error, stanza_error()}.
node_disco_info(Host, Node, From) ->
    node_disco_info(Host, Node, From, true, true).

-spec node_disco_info(binary(), binary(), jid(), boolean(), boolean()) ->
			     {result, disco_info()} | {error, stanza_error()}.
node_disco_info(Host, Node, _From, _Identity, _Features) ->
    Action =
	fun(#pubsub_node{id = Nidx, type = Type, options = Options}) ->
		NodeType = case get_option(Options, node_type) of
			       collection -> <<"collection">>;
			       _ -> <<"leaf">>
			   end,
		Affs = case node_call(Host, Type, get_node_affiliations, [Nidx]) of
			  {result, As} -> As;
			  _ -> []
		       end,
		Subs = case node_call(Host, Type, get_node_subscriptions, [Nidx]) of
			  {result, Ss} -> Ss;
			  _ -> []
		       end,
		Meta = [{title, get_option(Options, title, <<>>)},
			{description, get_option(Options, description, <<>>)},
			{owner, [jid:make(LJID) || {LJID, Aff} <- Affs, Aff =:= owner]},
			{publisher, [jid:make(LJID) || {LJID, Aff} <- Affs, Aff =:= publisher]},
			{access_model, get_option(Options, access_model, open)},
                        {publish_model, get_option(Options, publish_model, publishers)},
			{num_subscribers, length(Subs)}],
		XData = #xdata{type = result,
			       fields = pubsub_meta_data:encode(Meta)},
		Is = [#identity{category = <<"pubsub">>, type = NodeType}],
		Fs = [?NS_PUBSUB | [feature(F) || F <- plugin_features(Host, Type)]],
		{result, #disco_info{identities = Is, features = Fs, xdata = [XData]}}
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
    end.

-spec iq_disco_info(binary(), binary(), binary(), jid(), binary())
		   -> {result, disco_info()} | {error, stanza_error()}.
iq_disco_info(ServerHost, Host, SNode, From, Lang) ->
    [Node | _] = case SNode of
		     <<>> -> [<<>>];
		     _ -> str:tokens(SNode, <<"!">>)
		 end,
    case Node of
	<<>> ->
	    Name = mod_pubsub_opt:name(ServerHost),
	    {result,
	     #disco_info{
		identities = [#identity{
				 category = <<"pubsub">>,
				 type = <<"service">>,
				 name = translate:translate(Lang, Name)}],
		features = [?NS_DISCO_INFO,
			    ?NS_DISCO_ITEMS,
			    ?NS_PUBSUB,
			    ?NS_COMMANDS,
			    ?NS_VCARD |
			    [feature(F) || F <- features(Host, Node)]]}};
	?NS_COMMANDS ->
	    command_disco_info(Host, Node, From);
	?NS_PUBSUB_GET_PENDING ->
	    command_disco_info(Host, Node, From);
	_ ->
	    node_disco_info(Host, Node, From)
    end.

-spec iq_disco_items(host(), binary(), jid(), undefined | rsm_set()) ->
			    {result, disco_items()} | {error, stanza_error()}.
iq_disco_items(Host, <<>>, _From, _RSM) ->
    MaxNodes = mod_pubsub_opt:max_nodes_discoitems(serverhost(Host)),
    case tree_action(Host, get_subnodes, [Host, <<>>, MaxNodes]) of
	{error, #stanza_error{}} = Err ->
	    Err;
	Nodes when is_list(Nodes) ->
	    Items =
		lists:map(
		  fun(#pubsub_node{nodeid = {_, SubNode}, options = Options}) ->
			  case get_option(Options, title) of
			      false ->
				  #disco_item{jid = jid:make(Host),
					      node = SubNode};
			      Title ->
				  #disco_item{jid = jid:make(Host),
					      name = Title,
					      node = SubNode}
			  end
		  end, Nodes),
	    {result, #disco_items{items = Items}}
    end;
iq_disco_items(Host, ?NS_COMMANDS, _From, _RSM) ->
    {result,
     #disco_items{items = [#disco_item{jid = jid:make(Host),
				       node = ?NS_PUBSUB_GET_PENDING,
				       name = ?T("Get Pending")}]}};
iq_disco_items(_Host, ?NS_PUBSUB_GET_PENDING, _From, _RSM) ->
    {result, #disco_items{}};
iq_disco_items(Host, Item, From, RSM) ->
    case str:tokens(Item, <<"!">>) of
	[_Node, _ItemId] ->
	    {result, #disco_items{}};
	[Node] ->
	    MaxNodes = mod_pubsub_opt:max_nodes_discoitems(serverhost(Host)),
	    Action = fun(#pubsub_node{id = Nidx, type = Type, options = Options, owners = O}) ->
			     Owners = node_owners_call(Host, Type, Nidx, O),
			     {NodeItems, RsmOut} = case get_allowed_items_call(
							  Host, Nidx, From, Type, Options, Owners, RSM) of
						       {result, R} -> R;
						       _ -> {[], undefined}
						   end,
			     case tree_call(Host, get_subnodes, [Host, Node, MaxNodes]) of
				 SubNodes when is_list(SubNodes) ->
				     Nodes = lists:map(
					       fun(#pubsub_node{nodeid = {_, SubNode}, options = SubOptions}) ->
						       case get_option(SubOptions, title) of
							   false ->
							       #disco_item{jid = jid:make(Host),
									   node = SubNode};
							   Title ->
							       #disco_item{jid = jid:make(Host),
									   name = Title,
									   node = SubNode}
						       end
					       end, SubNodes),
				     Items = lists:flatmap(
					       fun(#pubsub_item{itemid = {RN, _}}) ->
						       case node_call(Host, Type, get_item_name, [Host, Node, RN]) of
							   {result, Name} ->
							       [#disco_item{jid = jid:make(Host), name = Name}];
							   _ ->
							       []
						       end
					       end, NodeItems),
				     {result, #disco_items{items = Nodes ++ Items,
							   rsm = RsmOut}};
				 Error ->
				     Error
			     end
		     end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Result}} -> {result, Result};
		Other -> Other
	    end
    end.

-spec iq_sm(iq()) -> iq().
iq_sm(#iq{to = To, sub_els = [SubEl]} = IQ) ->
    LOwner = jid:tolower(jid:remove_resource(To)),
    Res = case xmpp:get_ns(SubEl) of
	      ?NS_PUBSUB ->
		  iq_pubsub(LOwner, all, IQ);
	      ?NS_PUBSUB_OWNER ->
		  iq_pubsub_owner(LOwner, IQ)
	  end,
    case Res of
	{result, IQRes} ->
	    xmpp:make_iq_result(IQ, IQRes);
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end.

-spec iq_get_vcard(binary(), binary()) -> vcard_temp().
iq_get_vcard(ServerHost, Lang) ->
    case mod_pubsub_opt:vcard(ServerHost) of
	undefined ->
	    Desc = misc:get_descr(Lang, ?T("ejabberd Publish-Subscribe module")),
	    #vcard_temp{fn = <<"ejabberd/mod_pubsub">>,
			url = ejabberd_config:get_uri(),
			desc = Desc};
	VCard ->
	    VCard
    end.

-spec iq_pubsub(binary() | ljid(), atom(), iq()) ->
		       {result, pubsub()} | {error, stanza_error()}.
iq_pubsub(Host, Access, #iq{from = From, type = IQType, lang = Lang,
			    sub_els = [SubEl]}) ->
    case {IQType, SubEl} of
	{set, #pubsub{create = Node, configure = Configure,
		      _ = undefined}} when is_binary(Node) ->
	    ServerHost = serverhost(Host),
	    Plugins = config(ServerHost, plugins),
	    Config = case Configure of
			 {_, XData} -> decode_node_config(XData, Host, Lang);
			 undefined -> []
		     end,
	    Type = hd(Plugins),
	    case Config of
		{error, _} = Err ->
		    Err;
		_ ->
		    create_node(Host, ServerHost, Node, From, Type, Access, Config)
	    end;
	{set, #pubsub{publish = #ps_publish{node = Node, items = Items},
		      publish_options = XData, configure = _, _ = undefined}} ->
	    ServerHost = serverhost(Host),
	    case Items of
		[#ps_item{id = ItemId, sub_els = Payload}] ->
		    case decode_publish_options(XData, Lang) of
			{error, _} = Err ->
			    Err;
			PubOpts ->
			    publish_item(Host, ServerHost, Node, From, ItemId,
					 Payload, PubOpts, Access)
		    end;
		[] ->
		    publish_item(Host, ServerHost, Node, From, <<>>, [], [], Access);
		_ ->
		    {error, extended_error(xmpp:err_bad_request(), err_invalid_payload())}
	    end;
	{set, #pubsub{retract = #ps_retract{node = Node, notify = Notify, items = Items},
		      _ = undefined}} ->
	    case Items of
		[#ps_item{id = ItemId}] ->
		    if ItemId /= <<>> ->
			    delete_item(Host, Node, From, ItemId, Notify);
		       true ->
			    {error, extended_error(xmpp:err_bad_request(),
						   err_item_required())}
		    end;
		[] ->
		    {error, extended_error(xmpp:err_bad_request(), err_item_required())};
		_ ->
		    {error, extended_error(xmpp:err_bad_request(), err_invalid_payload())}
	    end;
	{set, #pubsub{subscribe = #ps_subscribe{node = Node, jid = JID},
		      options = Options, _ = undefined}} ->
	    Config = case Options of
			 #ps_options{xdata = XData, jid = undefined, node = <<>>} ->
			     decode_subscribe_options(XData, Lang);
			 #ps_options{xdata = _XData, jid = #jid{}} ->
			     Txt = ?T("Attribute 'jid' is not allowed here"),
			     {error, xmpp:err_bad_request(Txt, Lang)};
			 #ps_options{xdata = _XData} ->
			     Txt = ?T("Attribute 'node' is not allowed here"),
			     {error, xmpp:err_bad_request(Txt, Lang)};
			 _ ->
			     []
		     end,
	    case Config of
		{error, _} = Err ->
		    Err;
		_ ->
		    subscribe_node(Host, Node, From, JID, Config)
	    end;
	{set, #pubsub{unsubscribe = #ps_unsubscribe{node = Node, jid = JID, subid = SubId},
		      _ = undefined}} ->
	    unsubscribe_node(Host, Node, From, JID, SubId);
	{get, #pubsub{items = #ps_items{node = Node,
					max_items = MaxItems,
					subid = SubId,
					items = Items},
		      rsm = RSM, _ = undefined}} ->
	    ItemIds = [ItemId || #ps_item{id = ItemId} <- Items, ItemId /= <<>>],
	    get_items(Host, Node, From, SubId, MaxItems, ItemIds, RSM);
	{get, #pubsub{subscriptions = {Node, _}, _ = undefined}} ->
	    Plugins = config(serverhost(Host), plugins),
	    get_subscriptions(Host, Node, From, Plugins);
	{get, #pubsub{affiliations = {Node, _}, _ = undefined}} ->
	    Plugins = config(serverhost(Host), plugins),
	    get_affiliations(Host, Node, From, Plugins);
	{_, #pubsub{options = #ps_options{jid = undefined}, _ = undefined}} ->
	    {error, extended_error(xmpp:err_bad_request(), err_jid_required())};
	{_, #pubsub{options = #ps_options{node = <<>>}, _ = undefined}} ->
	    {error, extended_error(xmpp:err_bad_request(), err_nodeid_required())};
	{get, #pubsub{options = #ps_options{node = Node, subid = SubId, jid = JID},
		      _ = undefined}} ->
	    get_options(Host, Node, JID, SubId, Lang);
	{set, #pubsub{options = #ps_options{node = Node, subid = SubId,
					    jid = JID, xdata = XData},
		      _ = undefined}} ->
	    case decode_subscribe_options(XData, Lang) of
		{error, _} = Err ->
		    Err;
		Config ->
		    set_options(Host, Node, JID, SubId, Config)
	    end;
	{set, #pubsub{}} ->
	    {error, xmpp:err_bad_request()};
	_ ->
	    {error, xmpp:err_feature_not_implemented()}
    end.

-spec iq_pubsub_owner(binary() | ljid(), iq()) -> {result, pubsub_owner() | undefined} |
						  {error, stanza_error()}.
iq_pubsub_owner(Host, #iq{type = IQType, from = From,
			  lang = Lang, sub_els = [SubEl]}) ->
    case {IQType, SubEl} of
	{get, #pubsub_owner{configure = {Node, undefined}, _ = undefined}} ->
	    ServerHost = serverhost(Host),
	    get_configure(Host, ServerHost, Node, From, Lang);
	{set, #pubsub_owner{configure = {Node, XData}, _ = undefined}} ->
	    case XData of
		undefined ->
		    {error, xmpp:err_bad_request(?T("No data form found"), Lang)};
		#xdata{type = cancel} ->
		    {result, #pubsub_owner{}};
		#xdata{type = submit} ->
		    case decode_node_config(XData, Host, Lang) of
			{error, _} = Err ->
			    Err;
			Config ->
			    set_configure(Host, Node, From, Config, Lang)
		    end;
		#xdata{} ->
		    {error, xmpp:err_bad_request(?T("Incorrect data form"), Lang)}
	    end;
	{get, #pubsub_owner{default = {Node, undefined}, _ = undefined}} ->
	    get_default(Host, Node, From, Lang);
	{set, #pubsub_owner{delete = {Node, _}, _ = undefined}} ->
	    delete_node(Host, Node, From);
	{set, #pubsub_owner{purge = Node, _ = undefined}} when Node /= undefined ->
	    purge_node(Host, Node, From);
	{get, #pubsub_owner{subscriptions = {Node, []}, _ = undefined}} ->
	    get_subscriptions(Host, Node, From);
	{set, #pubsub_owner{subscriptions = {Node, Subs}, _ = undefined}} ->
	    set_subscriptions(Host, Node, From, Subs);
	{get, #pubsub_owner{affiliations = {Node, []}, _ = undefined}} ->
	    get_affiliations(Host, Node, From);
	{set, #pubsub_owner{affiliations = {Node, Affs}, _ = undefined}} ->
	    set_affiliations(Host, Node, From, Affs);
	{_, #pubsub_owner{}} ->
	    {error, xmpp:err_bad_request()};
	_ ->
	    {error, xmpp:err_feature_not_implemented()}
    end.

-spec adhoc_request(binary(), binary(), jid(), adhoc_command(),
		    atom(), [binary()]) -> adhoc_command() | {error, stanza_error()}.
adhoc_request(Host, _ServerHost, Owner,
	      #adhoc_command{node = ?NS_PUBSUB_GET_PENDING, lang = Lang,
			     action = execute, xdata = undefined},
	      _Access, Plugins) ->
    send_pending_node_form(Host, Owner, Lang, Plugins);
adhoc_request(Host, _ServerHost, Owner,
	      #adhoc_command{node = ?NS_PUBSUB_GET_PENDING, lang = Lang,
			     action = execute, xdata = #xdata{} = XData} = Request,
	      _Access, _Plugins) ->
    case decode_get_pending(XData, Lang) of
	{error, _} = Err ->
	    Err;
	Config ->
	    Node = proplists:get_value(node, Config),
	    case send_pending_auth_events(Host, Node, Owner, Lang) of
		ok ->
		    xmpp_util:make_adhoc_response(
		      Request, #adhoc_command{status = completed});
		Err ->
		    Err
	    end
    end;
adhoc_request(_Host, _ServerHost, _Owner,
	      #adhoc_command{action = cancel}, _Access, _Plugins) ->
    #adhoc_command{status = canceled};
adhoc_request(_Host, _ServerHost, _Owner, Other, _Access, _Plugins) ->
    ?DEBUG("Couldn't process ad hoc command:~n~p", [Other]),
    {error, xmpp:err_item_not_found()}.

-spec send_pending_node_form(binary(), jid(), binary(),
			     [binary()]) -> adhoc_command() | {error, stanza_error()}.
send_pending_node_form(Host, Owner, Lang, Plugins) ->
    Filter = fun (Type) ->
	    lists:member(<<"get-pending">>, plugin_features(Host, Type))
    end,
    case lists:filter(Filter, Plugins) of
	[] ->
	    Err = extended_error(xmpp:err_feature_not_implemented(),
				 err_unsupported('get-pending')),
	    {error, Err};
	Ps ->
	    case get_pending_nodes(Host, Owner, Ps) of
		{ok, Nodes} ->
		    Form = [{node, <<>>, lists:zip(Nodes, Nodes)}],
		    XForm = #xdata{type = form,
				   fields = pubsub_get_pending:encode(Form, Lang)},
		    #adhoc_command{status = executing, action = execute,
				   xdata = XForm};
		Err ->
		    Err
	    end
    end.

-spec get_pending_nodes(binary(), jid(), [binary()]) -> {ok, [binary()]} |
							{error, stanza_error()}.
get_pending_nodes(Host, Owner, Plugins) ->
    Tr = fun (Type) ->
	    case node_call(Host, Type, get_pending_nodes, [Host, Owner]) of
		{result, Nodes} -> Nodes;
		_ -> []
	    end
	 end,
    Action = fun() -> {result, lists:flatmap(Tr, Plugins)} end,
    case transaction(Host, Action, sync_dirty) of
	{result, Res} -> {ok, Res};
	Err -> Err
    end.

%% @doc <p>Send a subscription approval form to Owner for all pending
%% subscriptions on Host and Node.</p>
-spec send_pending_auth_events(binary(), binary(), jid(),
			       binary()) -> ok | {error, stanza_error()}.
send_pending_auth_events(Host, Node, Owner, Lang) ->
    ?DEBUG("Sending pending auth events for ~ts on ~ts:~ts",
	   [jid:encode(Owner), Host, Node]),
    Action =
	fun(#pubsub_node{id = Nidx, type = Type}) ->
		case lists:member(<<"get-pending">>, plugin_features(Host, Type)) of
		    true ->
			case node_call(Host, Type, get_affiliation, [Nidx, Owner]) of
			    {result, owner} ->
				node_call(Host, Type, get_node_subscriptions, [Nidx]);
			    _ ->
				{error, xmpp:err_forbidden(
					  ?T("Owner privileges required"), Lang)}
			end;
		    false ->
			{error, extended_error(xmpp:err_feature_not_implemented(),
					       err_unsupported('get-pending'))}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {N, Subs}} ->
	    lists:foreach(
	      fun({J, pending, _SubId}) -> send_authorization_request(N, jid:make(J));
		 ({J, pending}) -> send_authorization_request(N, jid:make(J));
		 (_) -> ok
	      end, Subs);
	Err ->
	    Err
    end.

%%% authorization handling
-spec send_authorization_request(#pubsub_node{}, jid()) -> ok.
send_authorization_request(#pubsub_node{nodeid = {Host, Node},
					type = Type, id = Nidx, owners = O},
			   Subscriber) ->
    %% TODO: pass lang to this function
    Lang = <<"en">>,
    Fs = pubsub_subscribe_authorization:encode(
	   [{node, Node},
	    {subscriber_jid, Subscriber},
	    {allow, false}],
	   Lang),
    X = #xdata{type = form,
	       title = translate:translate(
			 Lang, ?T("PubSub subscriber request")),
	       instructions = [translate:translate(
				 Lang,
				 ?T("Choose whether to approve this entity's "
				    "subscription."))],
	       fields = Fs},
    Stanza = #message{from = service_jid(Host), sub_els = [X]},
    lists:foreach(
      fun (Owner) ->
	      ejabberd_router:route(xmpp:set_to(Stanza, jid:make(Owner)))
      end, node_owners_action(Host, Type, Nidx, O)).

-spec find_authorization_response(message()) -> undefined |
						pubsub_subscribe_authorization:result() |
						{error, stanza_error()}.
find_authorization_response(Packet) ->
    case xmpp:get_subtag(Packet, #xdata{type = form}) of
	#xdata{type = cancel} ->
	    undefined;
	#xdata{type = submit, fields = Fs} ->
	    try pubsub_subscribe_authorization:decode(Fs) of
		Result -> Result
	    catch _:{pubsub_subscribe_authorization, Why} ->
		    Lang = xmpp:get_lang(Packet),
		    Txt = pubsub_subscribe_authorization:format_error(Why),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end;
	#xdata{} ->
	    {error, xmpp:err_bad_request()};
	false ->
	    undefined
    end.

%% @doc Send a message to JID with the supplied Subscription
-spec send_authorization_approval(binary(), jid(), binary(), subscribed | none) -> ok.
send_authorization_approval(Host, JID, SNode, Subscription) ->
    Event = #ps_event{subscription =
			  #ps_subscription{jid = JID,
					   node = SNode,
					   type = Subscription}},
    Stanza = #message{from = service_jid(Host), to = JID, sub_els = [Event]},
    ejabberd_router:route(Stanza).

-spec handle_authorization_response(binary(), message(),
				    pubsub_subscribe_authorization:result()) -> ok.
handle_authorization_response(Host, #message{from = From} = Packet, Response) ->
    Node = proplists:get_value(node, Response),
    Subscriber = proplists:get_value(subscriber_jid, Response),
    Allow = proplists:get_value(allow, Response),
    Lang = xmpp:get_lang(Packet),
    FromLJID = jid:tolower(jid:remove_resource(From)),
    Action =
	fun(#pubsub_node{type = Type, id = Nidx, owners = O}) ->
		Owners = node_owners_call(Host, Type, Nidx, O),
		case lists:member(FromLJID, Owners) of
		    true ->
			case node_call(Host, Type, get_subscriptions, [Nidx, Subscriber]) of
			    {result, Subs} ->
				update_auth(Host, Node, Type, Nidx, Subscriber, Allow, Subs);
			    {error, _} = Err ->
				Err
			end;
		    false ->
			{error, xmpp:err_forbidden(?T("Owner privileges required"), Lang)}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{error, Error} ->
	    ejabberd_router:route_error(Packet, Error);
	{result, {_, _NewSubscription}} ->
	    %% XXX: notify about subscription state change, section 12.11
	    ok
    end.

-spec update_auth(binary(), binary(), _, _, jid() | error, boolean(), _) ->
			 {result, ok} | {error, stanza_error()}.
update_auth(Host, Node, Type, Nidx, Subscriber, Allow, Subs) ->
    Sub= lists:filter(fun
		({pending, _}) -> true;
		(_) -> false
	    end,
	    Subs),
    case Sub of
	[{pending, SubId}|_] ->
	    NewSub = case Allow of
		true -> subscribed;
		false -> none
	    end,
	    node_call(Host, Type, set_subscriptions, [Nidx, Subscriber, NewSub, SubId]),
	    send_authorization_approval(Host, Subscriber, Node, NewSub),
	    {result, ok};
	_ ->
	    Txt = ?T("No pending subscriptions found"),
	    {error, xmpp:err_unexpected_request(Txt, ejabberd_option:language())}
    end.

%% @doc <p>Create new pubsub nodes</p>
%%<p>In addition to method-specific error conditions, there are several general reasons why the node creation request might fail:</p>
%%<ul>
%%<li>The service does not support node creation.</li>
%%<li>Only entities that are registered with the service are allowed to create nodes but the requesting entity is not registered.</li>
%%<li>The requesting entity does not have sufficient privileges to create nodes.</li>
%%<li>The requested Node already exists.</li>
%%<li>The request did not include a Node and "instant nodes" are not supported.</li>
%%</ul>
%%<p>ote: node creation is a particular case, error return code is evaluated at many places:</p>
%%<ul>
%%<li>iq_pubsub checks if service supports node creation (type exists)</li>
%%<li>create_node checks if instant nodes are supported</li>
%%<li>create_node asks node plugin if entity have sufficient privilege</li>
%%<li>nodetree create_node checks if nodeid already exists</li>
%%<li>node plugin create_node just sets default affiliation/subscription</li>
%%</ul>
-spec create_node(host(), binary(), binary(), jid(),
		  binary()) -> {result, pubsub()} | {error, stanza_error()}.
create_node(Host, ServerHost, Node, Owner, Type) ->
    create_node(Host, ServerHost, Node, Owner, Type, all, []).

-spec create_node(host(), binary(), binary(), jid(), binary(),
		  atom(), [{binary(), [binary()]}]) -> {result, pubsub()} | {error, stanza_error()}.
create_node(Host, ServerHost, <<>>, Owner, Type, Access, Configuration) ->
    case lists:member(<<"instant-nodes">>, plugin_features(Host, Type)) of
	true ->
	    Node = p1_rand:get_string(),
	    case create_node(Host, ServerHost, Node, Owner, Type, Access, Configuration) of
		{result, _} ->
		    {result, #pubsub{create = Node}};
		Error ->
		    Error
	    end;
	false ->
	    {error, extended_error(xmpp:err_not_acceptable(), err_nodeid_required())}
    end;
create_node(Host, ServerHost, Node, Owner, GivenType, Access, Configuration) ->
    Type = select_type(ServerHost, Host, Node, GivenType),
    NodeOptions = merge_config(
		    [node_config(Node, ServerHost),
		     Configuration, node_options(Host, Type)]),
    CreateNode =
	fun() ->
		Parent = case node_call(Host, Type, node_to_path, [Node]) of
			     {result, [Node]} ->
				 <<>>;
			     {result, Path} ->
				 element(2, node_call(Host, Type, path_to_node,
						      [lists:sublist(Path, length(Path)-1)]))
			 end,
		Parents = case Parent of
			      <<>> -> [];
			      _ -> [Parent]
			  end,
		case node_call(Host, Type, create_node_permission,
			       [Host, ServerHost, Node, Parent, Owner, Access]) of
		    {result, true} ->
			case tree_call(Host, create_node,
				       [Host, Node, Type, Owner, NodeOptions, Parents])
			of
			    {ok, Nidx} ->
				case get_node_subs_by_depth(Host, Node, Owner) of
				    {result, SubsByDepth} ->
					case node_call(Host, Type, create_node, [Nidx, Owner]) of
					    {result, Result} -> {result, {Nidx, SubsByDepth, Result}};
					    Error -> Error
					end;
				    Error ->
					Error
				end;
			    {error, {virtual, Nidx}} ->
				case node_call(Host, Type, create_node, [Nidx, Owner]) of
				    {result, Result} -> {result, {Nidx, [], Result}};
				    Error -> Error
				end;
			    Error ->
				Error
			end;
		    {result, _} ->
			Txt = ?T("You're not allowed to create nodes"),
			{error, xmpp:err_forbidden(Txt, ejabberd_option:language())};
		    Err ->
			Err
		end
	end,
    Reply = #pubsub{create = Node},
    case transaction(Host, CreateNode, transaction) of
	{result, {Nidx, SubsByDepth, {Result, broadcast}}} ->
	    broadcast_created_node(Host, Node, Nidx, Type, NodeOptions, SubsByDepth),
	    ejabberd_hooks:run(pubsub_create_node, ServerHost,
			       [ServerHost, Host, Node, Nidx, NodeOptions]),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {Nidx, _SubsByDepth, Result}} ->
	    ejabberd_hooks:run(pubsub_create_node, ServerHost,
			       [ServerHost, Host, Node, Nidx, NodeOptions]),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	Error ->
	    %% in case we change transaction to sync_dirty...
	    %%  node_call(Host, Type, delete_node, [Host, Node]),
	    %%  tree_call(Host, delete_node, [Host, Node]),
	    Error
    end.

%% @doc <p>Delete specified node and all children.</p>
%%<p>There are several reasons why the node deletion request might fail:</p>
%%<ul>
%%<li>The requesting entity does not have sufficient privileges to delete the node.</li>
%%<li>The node is the root collection node, which cannot be deleted.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
-spec delete_node(host(), binary(), jid()) -> {result, pubsub_owner()} | {error, stanza_error()}.
delete_node(_Host, <<>>, _Owner) ->
    {error, xmpp:err_not_allowed(?T("No node specified"), ejabberd_option:language())};
delete_node(Host, Node, Owner) ->
    Action =
	fun(#pubsub_node{type = Type, id = Nidx}) ->
		case node_call(Host, Type, get_affiliation, [Nidx, Owner]) of
		    {result, owner} ->
			case get_node_subs_by_depth(Host, Node, service_jid(Host)) of
			    {result, SubsByDepth} ->
				case tree_call(Host, delete_node, [Host, Node]) of
				    Removed when is_list(Removed) ->
					case node_call(Host, Type, delete_node, [Removed]) of
					    {result, Res} -> {result, {SubsByDepth, Res}};
					    Error -> Error
					end;
				    Error ->
					Error
				end;
			    Error ->
				Error
			end;
		    {result, _} ->
			Lang = ejabberd_option:language(),
			{error, xmpp:err_forbidden(?T("Owner privileges required"), Lang)};
		    Error ->
			Error
		end
	end,
    Reply = undefined,
    ServerHost = serverhost(Host),
    case transaction(Host, Node, Action, transaction) of
	{result, {_, {SubsByDepth, {Result, broadcast, Removed}}}} ->
	    lists:foreach(fun ({RNode, _RSubs}) ->
			{RH, RN} = RNode#pubsub_node.nodeid,
			RNidx = RNode#pubsub_node.id,
			RType = RNode#pubsub_node.type,
			ROptions = RNode#pubsub_node.options,
			unset_cached_item(RH, RNidx),
			broadcast_removed_node(RH, RN, RNidx, RType, ROptions, SubsByDepth),
			ejabberd_hooks:run(pubsub_delete_node,
			    ServerHost,
			    [ServerHost, RH, RN, RNidx])
		end,
		Removed),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {_, {_, {Result, Removed}}}} ->
	    lists:foreach(fun ({RNode, _RSubs}) ->
			{RH, RN} = RNode#pubsub_node.nodeid,
			RNidx = RNode#pubsub_node.id,
			unset_cached_item(RH, RNidx),
			ejabberd_hooks:run(pubsub_delete_node,
			    ServerHost,
			    [ServerHost, RH, RN, RNidx])
		end,
		Removed),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {TNode, {_, Result}}} ->
	    Nidx = TNode#pubsub_node.id,
	    unset_cached_item(Host, Nidx),
	    ejabberd_hooks:run(pubsub_delete_node, ServerHost,
		[ServerHost, Host, Node, Nidx]),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	Error ->
	    Error
    end.

%% @see node_hometree:subscribe_node/5
%% @doc <p>Accepts or rejects subcription requests on a PubSub node.</p>
%%<p>There are several reasons why the subscription request might fail:</p>
%%<ul>
%%<li>The bare JID portions of the JIDs do not match.</li>
%%<li>The node has an access model of "presence" and the requesting entity is not subscribed to the owner's presence.</li>
%%<li>The node has an access model of "roster" and the requesting entity is not in one of the authorized roster groups.</li>
%%<li>The node has an access model of "whitelist" and the requesting entity is not on the whitelist.</li>
%%<li>The service requires payment for subscriptions to the node.</li>
%%<li>The requesting entity is anonymous and the service does not allow anonymous entities to subscribe.</li>
%%<li>The requesting entity has a pending subscription.</li>
%%<li>The requesting entity is blocked from subscribing (e.g., because having an affiliation of outcast).</li>
%%<li>The node does not support subscriptions.</li>
%%<li>The node does not exist.</li>
%%</ul>
-spec subscribe_node(host(), binary(), jid(), jid(), [{binary(), [binary()]}]) ->
			    {result, pubsub()} | {error, stanza_error()}.
subscribe_node(Host, Node, From, JID, Configuration) ->
    SubModule = subscription_plugin(Host),
    SubOpts = case SubModule:parse_options_xform(Configuration) of
	{result, GoodSubOpts} -> GoodSubOpts;
	_ -> invalid
    end,
    Subscriber = jid:tolower(JID),
    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx, owners = O}) ->
	    Features = plugin_features(Host, Type),
	    SubscribeFeature = lists:member(<<"subscribe">>, Features),
	    OptionsFeature = lists:member(<<"subscription-options">>, Features),
	    HasOptions = not (SubOpts == []),
	    SubscribeConfig = get_option(Options, subscribe),
	    AccessModel = get_option(Options, access_model),
	    SendLast = get_option(Options, send_last_published_item),
	    AllowedGroups = get_option(Options, roster_groups_allowed, []),
	    CanSubscribe = case get_max_subscriptions_node(Host) of
		Max when is_integer(Max) ->
		    case node_call(Host, Type, get_node_subscriptions, [Nidx]) of
			{result, NodeSubs} ->
			    SubsNum = lists:foldl(
				    fun ({_, subscribed, _}, Acc) -> Acc+1;
					(_, Acc) -> Acc
				    end, 0, NodeSubs),
			    SubsNum < Max;
			_ ->
			    true
		    end;
		_ ->
		    true
	    end,
	    if not SubscribeFeature ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('subscribe'))};
		not SubscribeConfig ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('subscribe'))};
		HasOptions andalso not OptionsFeature ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('subscription-options'))};
		SubOpts == invalid ->
		    {error, extended_error(xmpp:err_bad_request(),
					   err_invalid_options())};
		not CanSubscribe ->
		    %% fallback to closest XEP compatible result, assume we are not allowed to subscribe
		    {error, extended_error(xmpp:err_not_allowed(),
					   err_closed_node())};
		true ->
		    Owners = node_owners_call(Host, Type, Nidx, O),
		    {PS, RG} = get_presence_and_roster_permissions(Host, JID,
			    Owners, AccessModel, AllowedGroups),
		    node_call(Host, Type, subscribe_node,
			[Nidx, From, Subscriber, AccessModel,
			    SendLast, PS, RG, SubOpts])
	    end
    end,
    Reply = fun (Subscription) ->
	    Sub = case Subscription of
		      {subscribed, SubId} ->
			  #ps_subscription{jid = JID, type = subscribed, subid = SubId};
		      Other ->
			  #ps_subscription{jid = JID, type = Other}
		  end,
	    #pubsub{subscription = Sub#ps_subscription{node = Node}}
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, subscribed, SubId, send_last}}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    send_items(Host, Node, Nidx, Type, Options, Subscriber, last),
	    ServerHost = serverhost(Host),
	    ejabberd_hooks:run(pubsub_subscribe_node, ServerHost,
		[ServerHost, Host, Node, Subscriber, SubId]),
	    case Result of
		default -> {result, Reply({subscribed, SubId})};
		_ -> {result, Result}
	    end;
	{result, {_TNode, {default, subscribed, SubId}}} ->
	    {result, Reply({subscribed, SubId})};
	{result, {_TNode, {Result, subscribed, _SubId}}} ->
	    {result, Result};
	{result, {TNode, {default, pending, _SubId}}} ->
	    send_authorization_request(TNode, JID),
	    {result, Reply(pending)};
	{result, {TNode, {Result, pending}}} ->
	    send_authorization_request(TNode, JID),
	    {result, Result};
	{result, {_, Result}} ->
	    {result, Result};
	Error -> Error
    end.

%% @doc <p>Unsubscribe <tt>JID</tt> from the <tt>Node</tt>.</p>
%%<p>There are several reasons why the unsubscribe request might fail:</p>
%%<ul>
%%<li>The requesting entity has multiple subscriptions to the node but does not specify a subscription ID.</li>
%%<li>The request does not specify an existing subscriber.</li>
%%<li>The requesting entity does not have sufficient privileges to unsubscribe the specified JID.</li>
%%<li>The node does not exist.</li>
%%<li>The request specifies a subscription ID that is not valid or current.</li>
%%</ul>
-spec unsubscribe_node(host(), binary(), jid(), jid(), binary()) ->
			      {result, undefined} | {error, stanza_error()}.
unsubscribe_node(Host, Node, From, JID, SubId) ->
    Subscriber = jid:tolower(JID),
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    node_call(Host, Type, unsubscribe_node, [Nidx, From, Subscriber, SubId])
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, default}} ->
	    ServerHost = serverhost(Host),
	    ejabberd_hooks:run(pubsub_unsubscribe_node, ServerHost,
			       [ServerHost, Host, Node, Subscriber, SubId]),
	    {result, undefined};
	Error -> Error
    end.

%% @doc <p>Publish item to a PubSub node.</p>
%% <p>The permission to publish an item must be verified by the plugin implementation.</p>
%%<p>There are several reasons why the publish request might fail:</p>
%%<ul>
%%<li>The requesting entity does not have sufficient privileges to publish.</li>
%%<li>The node does not support item publication.</li>
%%<li>The node does not exist.</li>
%%<li>The payload size exceeds a service-defined limit.</li>
%%<li>The item contains more than one payload element or the namespace of the root payload element does not match the configured namespace for the node.</li>
%%<li>The request does not match the node configuration.</li>
%%</ul>
-spec publish_item(host(), binary(), binary(), jid(), binary(),
		   [xmlel()]) -> {result, pubsub()} | {error, stanza_error()}.
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload) ->
    publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload, [], all).
publish_item(Host, ServerHost, Node, Publisher, <<>>, Payload, PubOpts, Access) ->
    publish_item(Host, ServerHost, Node, Publisher, uniqid(), Payload, PubOpts, Access);
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload, PubOpts, Access) ->
    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx}) ->
	    Features = plugin_features(Host, Type),
	    PublishFeature = lists:member(<<"publish">>, Features),
	    PublishModel = get_option(Options, publish_model),
	    DeliverPayloads = get_option(Options, deliver_payloads),
	    PersistItems = get_option(Options, persist_items),
	    MaxItems = max_items(Host, Options),
	    PayloadCount = payload_xmlelements(Payload),
	    PayloadSize = byte_size(term_to_binary(Payload)) - 2,
	    PayloadMaxSize = get_option(Options, max_payload_size),
	    PreconditionsMet = preconditions_met(PubOpts, Options),
	    if not PublishFeature ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported(publish))};
	        not PreconditionsMet ->
		    {error, extended_error(xmpp:err_conflict(),
					   err_precondition_not_met())};
		PayloadSize > PayloadMaxSize ->
		    {error, extended_error(xmpp:err_not_acceptable(),
					   err_payload_too_big())};
		(DeliverPayloads or PersistItems) and (PayloadCount == 0) ->
		    {error, extended_error(xmpp:err_bad_request(),
					   err_item_required())};
		(DeliverPayloads or PersistItems) and (PayloadCount > 1) ->
		    {error, extended_error(xmpp:err_bad_request(),
					   err_invalid_payload())};
		(not (DeliverPayloads or PersistItems)) and (PayloadCount > 0) ->
		    {error, extended_error(xmpp:err_bad_request(),
					   err_item_forbidden())};
		true ->
		    node_call(Host, Type, publish_item,
			[Nidx, Publisher, PublishModel, MaxItems, ItemId, Payload, PubOpts])
	    end
    end,
    Reply = #pubsub{publish = #ps_publish{node = Node,
					  items = [#ps_item{id = ItemId}]}},
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, Broadcast, Removed}}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    BrPayload = case Broadcast of
		broadcast -> Payload;
		PluginPayload -> PluginPayload
	    end,
	    set_cached_item(Host, Nidx, ItemId, Publisher, BrPayload),
	    case get_option(Options, deliver_notifications) of
		true ->
		    broadcast_publish_item(Host, Node, Nidx, Type, Options, ItemId,
			Publisher, BrPayload, Removed);
		false ->
		    ok
	    end,
	    ejabberd_hooks:run(pubsub_publish_item, ServerHost,
		[ServerHost, Node, Publisher, service_jid(Host), ItemId, BrPayload]),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {TNode, {default, Removed}}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, Nidx, Type, Options, Removed),
	    set_cached_item(Host, Nidx, ItemId, Publisher, Payload),
	    {result, Reply};
	{result, {TNode, {Result, Removed}}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, Nidx, Type, Options, Removed),
	    set_cached_item(Host, Nidx, ItemId, Publisher, Payload),
	    {result, Result};
	{result, {_, default}} ->
	    {result, Reply};
	{result, {_, Result}} ->
	    {result, Result};
	{error, #stanza_error{reason = 'item-not-found'}} ->
	    Type = select_type(ServerHost, Host, Node),
	    case lists:member(<<"auto-create">>, plugin_features(Host, Type)) of
		true ->
		    case create_node(Host, ServerHost, Node, Publisher, Type, Access, PubOpts) of
			{result, #pubsub{create = NewNode}} ->
			    publish_item(Host, ServerHost, NewNode, Publisher, ItemId,
					 Payload, PubOpts, Access);
			_ ->
			    {error, xmpp:err_item_not_found()}
		    end;
		false ->
		    Txt = ?T("Automatic node creation is not enabled"),
		    {error, xmpp:err_item_not_found(Txt, ejabberd_option:language())}
	    end;
	Error ->
	    Error
    end.

%% @doc <p>Delete item from a PubSub node.</p>
%% <p>The permission to delete an item must be verified by the plugin implementation.</p>
%%<p>There are several reasons why the item retraction request might fail:</p>
%%<ul>
%%<li>The publisher does not have sufficient privileges to delete the requested item.</li>
%%<li>The node or item does not exist.</li>
%%<li>The request does not specify a node.</li>
%%<li>The request does not include an <item/> element or the <item/> element does not specify an ItemId.</li>
%%<li>The node does not support persistent items.</li>
%%<li>The service does not support the deletion of items.</li>
%%</ul>
-spec delete_item(host(), binary(), jid(), binary()) -> {result, undefined} |
							{error, stanza_error()}.
delete_item(Host, Node, Publisher, ItemId) ->
    delete_item(Host, Node, Publisher, ItemId, false).
delete_item(_, <<>>, _, _, _) ->
    {error, extended_error(xmpp:err_bad_request(), err_nodeid_required())};
delete_item(Host, Node, Publisher, ItemId, ForceNotify) ->
    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx}) ->
	    Features = plugin_features(Host, Type),
	    PersistentFeature = lists:member(<<"persistent-items">>, Features),
	    DeleteFeature = lists:member(<<"delete-items">>, Features),
	    PublishModel = get_option(Options, publish_model),
	    if %%->   iq_pubsub just does that matches
		%%        %% Request does not specify an item
		%%        {error, extended_error(?ERR_BAD_REQUEST, "item-required")};
		not PersistentFeature ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('persistent-items'))};
		not DeleteFeature ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('delete-items'))};
		true ->
		    node_call(Host, Type, delete_item, [Nidx, Publisher, PublishModel, ItemId])
	    end
    end,
    Reply = undefined,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, broadcast}}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, Nidx, Type, Options, [ItemId], ForceNotify),
	    case get_cached_item(Host, Nidx) of
		#pubsub_item{itemid = {ItemId, Nidx}} -> unset_cached_item(Host, Nidx);
		_ -> ok
	    end,
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {_, default}} ->
	    {result, Reply};
	{result, {_, Result}} ->
	    {result, Result};
	Error ->
	    Error
    end.

%% @doc <p>Delete all items of specified node owned by JID.</p>
%%<p>There are several reasons why the node purge request might fail:</p>
%%<ul>
%%<li>The node or service does not support node purging.</li>
%%<li>The requesting entity does not have sufficient privileges to purge the node.</li>
%%<li>The node is not configured to persist items.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
-spec purge_node(mod_pubsub:host(), binary(), jid()) -> {result, undefined} |
							{error, stanza_error()}.
purge_node(Host, Node, Owner) ->
    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx}) ->
	    Features = plugin_features(Host, Type),
	    PurgeFeature = lists:member(<<"purge-nodes">>, Features),
	    PersistentFeature = lists:member(<<"persistent-items">>, Features),
	    PersistentConfig = get_option(Options, persist_items),
	    if not PurgeFeature ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('purge-nodes'))};
		not PersistentFeature ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('persistent-items'))};
		not PersistentConfig ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('persistent-items'))};
		true -> node_call(Host, Type, purge_node, [Nidx, Owner])
	    end
    end,
    Reply = undefined,
    case transaction(Host, Node, Action, transaction) of
	{result, {TNode, {Result, broadcast}}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_purge_node(Host, Node, Nidx, Type, Options),
	    unset_cached_item(Host, Nidx),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {_, default}} ->
	    {result, Reply};
	{result, {_, Result}} ->
	    {result, Result};
	Error ->
	    Error
    end.

%% @doc <p>Return the items of a given node.</p>
%% <p>The number of items to return is limited by MaxItems.</p>
%% <p>The permission are not checked in this function.</p>
-spec get_items(host(), binary(), jid(), binary(),
		undefined | non_neg_integer(), [binary()], undefined | rsm_set()) ->
		       {result, pubsub()} | {error, stanza_error()}.
get_items(Host, Node, From, SubId, MaxItems, ItemIds, undefined)
  when MaxItems =/= undefined ->
    get_items(Host, Node, From, SubId, MaxItems, ItemIds,
              #rsm_set{max = MaxItems, before = <<>>});
get_items(Host, Node, From, SubId, _MaxItems, ItemIds, RSM) ->
    Action =
	fun(#pubsub_node{options = Options, type = Type,
			 id = Nidx, owners = O}) ->
		Features = plugin_features(Host, Type),
		RetreiveFeature = lists:member(<<"retrieve-items">>, Features),
		PersistentFeature = lists:member(<<"persistent-items">>, Features),
		AccessModel = get_option(Options, access_model),
		AllowedGroups = get_option(Options, roster_groups_allowed, []),
		if not RetreiveFeature ->
			{error, extended_error(xmpp:err_feature_not_implemented(),
					       err_unsupported('retrieve-items'))};
		   not PersistentFeature ->
			{error, extended_error(xmpp:err_feature_not_implemented(),
					       err_unsupported('persistent-items'))};
		   true ->
			Owners = node_owners_call(Host, Type, Nidx, O),
			{PS, RG} = get_presence_and_roster_permissions(
				     Host, From, Owners, AccessModel, AllowedGroups),
			case ItemIds of
			    [ItemId] ->
				NotFound = xmpp:err_item_not_found(),
				case node_call(Host, Type, get_item,
					       [Nidx, ItemId, From, AccessModel, PS, RG, undefined])
				of
				    {error, NotFound} -> {result, {[], undefined}};
				    Result -> Result
				end;
			    _ ->
				node_call(Host, Type, get_items,
					  [Nidx, From, AccessModel, PS, RG, SubId, RSM])
			end
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Items, RsmOut}}} ->
	    SendItems = case ItemIds of
			    [] ->
				Items;
			    _ ->
				lists:filter(
				  fun(#pubsub_item{itemid = {ItemId, _}}) ->
					  lists:member(ItemId, ItemIds)
				  end, Items)
			end,
	    Options = TNode#pubsub_node.options,
	    {result, #pubsub{items = items_els(Node, Options, SendItems),
			     rsm = RsmOut}};
	{result, {TNode, Item}} ->
	    Options = TNode#pubsub_node.options,
	    {result, #pubsub{items = items_els(Node, Options, [Item])}};
	Error ->
	    Error
    end.

%% Seems like this function broken
get_items(Host, Node) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    node_call(Host, Type, get_items, [Nidx, service_jid(Host), undefined])
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, {Items, _}}} -> Items;
	Error -> Error
    end.

%% This function is broken too?
get_item(Host, Node, ItemId) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    node_call(Host, Type, get_item, [Nidx, ItemId])
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Items}} -> Items;
	Error -> Error
    end.

-spec get_allowed_items_call(host(), nodeIdx(), jid(),
			     binary(), nodeOptions(), [ljid()]) -> {result, [#pubsub_item{}]} |
								   {error, stanza_error()}.
get_allowed_items_call(Host, Nidx, From, Type, Options, Owners) ->
    case get_allowed_items_call(Host, Nidx, From, Type, Options, Owners, undefined) of
	{result, {Items, _RSM}} -> {result, Items};
	Error -> Error
    end.

-spec get_allowed_items_call(host(), nodeIdx(), jid(),
			     binary(), nodeOptions(), [ljid()],
			     undefined | rsm_set()) ->
				    {result, {[#pubsub_item{}], undefined | rsm_set()}} |
				    {error, stanza_error()}.
get_allowed_items_call(Host, Nidx, From, Type, Options, Owners, RSM) ->
    AccessModel = get_option(Options, access_model),
    AllowedGroups = get_option(Options, roster_groups_allowed, []),
    {PS, RG} = get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups),
    node_call(Host, Type, get_items, [Nidx, From, AccessModel, PS, RG, undefined, RSM]).

-spec get_last_items(host(), binary(), nodeIdx(), ljid(), last | integer()) -> [#pubsub_item{}].
get_last_items(Host, Type, Nidx, LJID, last) ->
    % hack to handle section 6.1.7 of XEP-0060
    get_last_items(Host, Type, Nidx, LJID, 1);
get_last_items(Host, Type, Nidx, LJID, 1) ->
    case get_cached_item(Host, Nidx) of
	undefined ->
	    case node_action(Host, Type, get_last_items, [Nidx, LJID, 1]) of
		{result, Items} -> Items;
		_ -> []
	    end;
	LastItem ->
	    [LastItem]
    end;
get_last_items(Host, Type, Nidx, LJID, Count) when Count > 1 ->
    case node_action(Host, Type, get_last_items, [Nidx, LJID, Count]) of
	{result, Items} -> Items;
	_ -> []
    end;
get_last_items(_Host, _Type, _Nidx, _LJID, _Count) ->
    [].

-spec get_only_item(host(), binary(), nodeIdx(), ljid()) -> [#pubsub_item{}].
get_only_item(Host, Type, Nidx, LJID) ->
    case get_cached_item(Host, Nidx) of
	undefined ->
	    case node_action(Host, Type, get_only_item, [Nidx, LJID]) of
		{result, Items} when length(Items) < 2 ->
		    Items;
		{result, Items} ->
		    [hd(lists:keysort(#pubsub_item.modification, Items))];
		_ -> []
	    end;
	LastItem ->
	    [LastItem]
    end.

%% @doc <p>Return the list of affiliations as an XMPP response.</p>
-spec get_affiliations(host(), binary(), jid(), [binary()]) ->
			      {result, pubsub()} | {error, stanza_error()}.
get_affiliations(Host, Node, JID, Plugins) when is_list(Plugins) ->
    Result =
	lists:foldl(
	  fun(Type, {Status, Acc}) ->
		  Features = plugin_features(Host, Type),
		  RetrieveFeature = lists:member(<<"retrieve-affiliations">>, Features),
		  if not RetrieveFeature ->
			  {{error, extended_error(xmpp:err_feature_not_implemented(),
						  err_unsupported('retrieve-affiliations'))},
			   Acc};
		     true ->
			  case node_action(Host, Type,
					   get_entity_affiliations,
					   [Host, JID]) of
			      {result, Affs} ->
				  {Status, [Affs | Acc]};
			      {error, _} = Err ->
				  {Err, Acc}
			  end
		  end
	  end, {ok, []}, Plugins),
    case Result of
	{ok, Affs} ->
	    Entities = lists:flatmap(
			 fun({_, none}) ->
				 [];
			    ({#pubsub_node{nodeid = {_, NodeId}}, Aff}) ->
				 if (Node == <<>>) or (Node == NodeId) ->
					 [#ps_affiliation{node = NodeId,
							  type = Aff}];
				    true ->
					 []
				 end;
			    (_) ->
				 []
			 end, lists:usort(lists:flatten(Affs))),
	    {result, #pubsub{affiliations = {<<>>, Entities}}};
	{Error, _} ->
	    Error
    end.

-spec get_affiliations(host(), binary(), jid()) ->
			      {result, pubsub_owner()} | {error, stanza_error()}.
get_affiliations(Host, Node, JID) ->
    Action =
	fun(#pubsub_node{type = Type, id = Nidx}) ->
		Features = plugin_features(Host, Type),
		RetrieveFeature = lists:member(<<"modify-affiliations">>, Features),
		{result, Affiliation} = node_call(Host, Type, get_affiliation, [Nidx, JID]),
		if not RetrieveFeature ->
			{error, extended_error(xmpp:err_feature_not_implemented(),
					       err_unsupported('modify-affiliations'))};
		   Affiliation /= owner ->
			{error, xmpp:err_forbidden(?T("Owner privileges required"), ejabberd_option:language())};
		   true ->
			node_call(Host, Type, get_node_affiliations, [Nidx])
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, []}} ->
	    {error, xmpp:err_item_not_found()};
	{result, {_, Affs}} ->
	    Entities = lists:flatmap(
			 fun({_, none}) ->
				 [];
			    ({AJID, Aff}) ->
				 [#ps_affiliation{jid = AJID, type = Aff}]
			 end, Affs),
	    {result, #pubsub_owner{affiliations = {Node, Entities}}};
	Error ->
	    Error
    end.

-spec set_affiliations(host(), binary(), jid(), [ps_affiliation()]) ->
			      {result, undefined} | {error, stanza_error()}.
set_affiliations(Host, Node, From, Affs) ->
    Owner = jid:tolower(jid:remove_resource(From)),
    Action =
	fun(#pubsub_node{type = Type, id = Nidx, owners = O} = N) ->
		Owners = node_owners_call(Host, Type, Nidx, O),
		case lists:member(Owner, Owners) of
		    true ->
			OwnerJID = jid:make(Owner),
			FilteredAffs =
			    case Owners of
				[Owner] ->
				    [Aff || Aff <- Affs,
					    Aff#ps_affiliation.jid /= OwnerJID];
				_ ->
				    Affs
			    end,
			lists:foreach(
			  fun(#ps_affiliation{jid = JID, type = Affiliation}) ->
				  node_call(Host, Type, set_affiliation, [Nidx, JID, Affiliation]),
				  case Affiliation of
				      owner ->
					  NewOwner = jid:tolower(jid:remove_resource(JID)),
					  NewOwners = [NewOwner | Owners],
					  tree_call(Host,
						    set_node,
						    [N#pubsub_node{owners = NewOwners}]);
				      none ->
					  OldOwner = jid:tolower(jid:remove_resource(JID)),
					  case lists:member(OldOwner, Owners) of
					      true ->
						  NewOwners = Owners -- [OldOwner],
						  tree_call(Host,
							    set_node,
							    [N#pubsub_node{owners = NewOwners}]);
					      _ ->
						  ok
					  end;
				      _ ->
					  ok
				  end
			  end, FilteredAffs),
			{result, undefined};
		    _ ->
			{error, xmpp:err_forbidden(
				  ?T("Owner privileges required"), ejabberd_option:language())}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
    end.

-spec get_options(binary(), binary(), jid(), binary(), binary()) ->
			 {result, xdata()} | {error, stanza_error()}.
get_options(Host, Node, JID, SubId, Lang) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    case lists:member(<<"subscription-options">>, plugin_features(Host, Type)) of
		true ->
		    get_options_helper(Host, JID, Lang, Node, Nidx, SubId, Type);
		false ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('subscription-options'))}
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, XForm}} -> {result, XForm};
	Error -> Error
    end.

-spec get_options_helper(binary(), jid(), binary(), binary(), _, binary(),
			 binary()) -> {result, pubsub()} | {error, stanza_error()}.
get_options_helper(Host, JID, Lang, Node, Nidx, SubId, Type) ->
    Subscriber = jid:tolower(JID),
    case node_call(Host, Type, get_subscriptions, [Nidx, Subscriber]) of
	{result, Subs} ->
	    SubIds = [Id || {Sub, Id} <- Subs, Sub == subscribed],
	    case {SubId, SubIds} of
		{_, []} ->
		    {error, extended_error(xmpp:err_not_acceptable(),
					   err_not_subscribed())};
		{<<>>, [SID]} ->
		    read_sub(Host, Node, Nidx, Subscriber, SID, Lang);
		{<<>>, _} ->
		    {error, extended_error(xmpp:err_not_acceptable(),
					   err_subid_required())};
		{_, _} ->
		    ValidSubId = lists:member(SubId, SubIds),
		    if ValidSubId ->
			    read_sub(Host, Node, Nidx, Subscriber, SubId, Lang);
		       true ->
			    {error, extended_error(xmpp:err_not_acceptable(),
						   err_invalid_subid())}
		    end
	    end;
	{error, _} = Error ->
	    Error
    end.

-spec read_sub(binary(), binary(), nodeIdx(), ljid(), binary(), binary()) -> {result, pubsub()}.
read_sub(Host, Node, Nidx, Subscriber, SubId, Lang) ->
    SubModule = subscription_plugin(Host),
    XData = case SubModule:get_subscription(Subscriber, Nidx, SubId) of
		{error, notfound} ->
		    undefined;
		{result, #pubsub_subscription{options = Options}} ->
		    {result, X} = SubModule:get_options_xform(Lang, Options),
		    X
	    end,
    {result, #pubsub{options = #ps_options{jid = jid:make(Subscriber),
					   subid = SubId,
					   node = Node,
					   xdata = XData}}}.

-spec set_options(binary(), binary(), jid(), binary(),
		  [{binary(), [binary()]}]) ->
			 {result, undefined} | {error, stanza_error()}.
set_options(Host, Node, JID, SubId, Configuration) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    case lists:member(<<"subscription-options">>, plugin_features(Host, Type)) of
		true ->
		    set_options_helper(Host, Configuration, JID, Nidx, SubId, Type);
		false ->
		    {error, extended_error(xmpp:err_feature_not_implemented(),
					   err_unsupported('subscription-options'))}
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, Result}} -> {result, Result};
	Error -> Error
    end.

-spec set_options_helper(binary(), [{binary(), [binary()]}], jid(),
			 nodeIdx(), binary(), binary()) ->
				{result, undefined} | {error, stanza_error()}.
set_options_helper(Host, Configuration, JID, Nidx, SubId, Type) ->
    SubModule = subscription_plugin(Host),
    SubOpts = case SubModule:parse_options_xform(Configuration) of
	{result, GoodSubOpts} -> GoodSubOpts;
	_ -> invalid
    end,
    Subscriber = jid:tolower(JID),
    case node_call(Host, Type, get_subscriptions, [Nidx, Subscriber]) of
	{result, Subs} ->
	    SubIds = [Id || {Sub, Id} <- Subs, Sub == subscribed],
	    case {SubId, SubIds} of
		{_, []} ->
		    {error, extended_error(xmpp:err_not_acceptable(), err_not_subscribed())};
		{<<>>, [SID]} ->
		    write_sub(Host, Nidx, Subscriber, SID, SubOpts);
		{<<>>, _} ->
		    {error, extended_error(xmpp:err_not_acceptable(), err_subid_required())};
		{_, _} ->
		    write_sub(Host, Nidx, Subscriber, SubId, SubOpts)
	    end;
	{error, _} = Err ->
	    Err
    end.

-spec write_sub(binary(), nodeIdx(), ljid(), binary(), _) -> {result, undefined} |
							     {error, stanza_error()}.
write_sub(_Host, _Nidx, _Subscriber, _SubId, invalid) ->
    {error, extended_error(xmpp:err_bad_request(), err_invalid_options())};
write_sub(_Host, _Nidx, _Subscriber, _SubId, []) ->
    {result, undefined};
write_sub(Host, Nidx, Subscriber, SubId, Options) ->
    SubModule = subscription_plugin(Host),
    case SubModule:set_subscription(Subscriber, Nidx, SubId, Options) of
	{result, _} -> {result, undefined};
	{error, _} -> {error, extended_error(xmpp:err_not_acceptable(),
					     err_invalid_subid())}
    end.

%% @doc <p>Return the list of subscriptions as an XMPP response.</p>
-spec get_subscriptions(host(), binary(), jid(), [binary()]) ->
			       {result, pubsub()} | {error, stanza_error()}.
get_subscriptions(Host, Node, JID, Plugins) when is_list(Plugins) ->
    Result = lists:foldl(fun (Type, {Status, Acc}) ->
		    Features = plugin_features(Host, Type),
		    RetrieveFeature = lists:member(<<"retrieve-subscriptions">>, Features),
		    if not RetrieveFeature ->
			    {{error, extended_error(xmpp:err_feature_not_implemented(),
						    err_unsupported('retrieve-subscriptions'))},
				Acc};
			true ->
			    Subscriber = jid:remove_resource(JID),
			    case node_action(Host, Type,
					     get_entity_subscriptions,
					     [Host, Subscriber]) of
				{result, Subs} ->
				    {Status, [Subs | Acc]};
				{error, _} = Err ->
				    {Err, Acc}
			    end
		    end
	    end, {ok, []}, Plugins),
    case Result of
	{ok, Subs} ->
	    Entities = lists:flatmap(fun
			({#pubsub_node{nodeid = {_, SubsNode}}, Sub}) ->
			    case Node of
				<<>> ->
				    [#ps_subscription{jid = jid:remove_resource(JID),
						      node = SubsNode, type = Sub}];
				SubsNode ->
				    [#ps_subscription{jid = jid:remove_resource(JID),
						      type = Sub}];
				_ ->
				    []
			    end;
			({#pubsub_node{nodeid = {_, SubsNode}}, Sub, SubId, SubJID}) ->
			    case Node of
				<<>> ->
				    [#ps_subscription{jid = SubJID,
						      subid = SubId,
						      type = Sub,
						      node = SubsNode}];
				SubsNode ->
				    [#ps_subscription{jid = SubJID,
						      subid = SubId,
						      type = Sub}];
				_ ->
				    []
			    end;
			({#pubsub_node{nodeid = {_, SubsNode}}, Sub, SubJID}) ->
			    case Node of
				<<>> ->
				    [#ps_subscription{jid = SubJID,
						      type = Sub,
						      node = SubsNode}];
				SubsNode ->
				    [#ps_subscription{jid = SubJID, type = Sub}];
				_ ->
				    []
			    end
		    end,
		    lists:usort(lists:flatten(Subs))),
	    {result, #pubsub{subscriptions = {<<>>, Entities}}};
	{Error, _} ->
	    Error
    end.

-spec get_subscriptions(host(), binary(), jid()) -> {result, pubsub_owner()} |
						    {error, stanza_error()}.
get_subscriptions(Host, Node, JID) ->
    Action = fun(#pubsub_node{type = Type, id = Nidx}) ->
		     Features = plugin_features(Host, Type),
		     RetrieveFeature = lists:member(<<"manage-subscriptions">>, Features),
		     case node_call(Host, Type, get_affiliation, [Nidx, JID]) of
			 {result, Affiliation} ->
			     if not RetrieveFeature ->
				     {error, extended_error(xmpp:err_feature_not_implemented(),
							    err_unsupported('manage-subscriptions'))};
				Affiliation /= owner ->
				     Lang = ejabberd_option:language(),
				     {error, xmpp:err_forbidden(?T("Owner privileges required"), Lang)};
				true ->
				     node_call(Host, Type, get_node_subscriptions, [Nidx])
			     end;
			 Error ->
			     Error
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Subs}} ->
	    Entities =
		lists:flatmap(
		  fun({_, none}) ->
			  [];
		     ({_, pending, _}) ->
			  [];
		     ({AJID, Sub}) ->
			  [#ps_subscription{jid = AJID, type = Sub}];
			({AJID, Sub, SubId}) ->
			  [#ps_subscription{jid = AJID, type = Sub, subid = SubId}]
		  end, Subs),
	    {result, #pubsub_owner{subscriptions = {Node, Entities}}};
	Error ->
	    Error
    end.

-spec get_subscriptions_for_send_last(host(), binary(), atom(), jid(), ljid(), ljid()) ->
				      [{#pubsub_node{}, subId(), ljid()}].
get_subscriptions_for_send_last(Host, PType, sql, JID, LJID, BJID) ->
    case node_action(Host, PType,
		     get_entity_subscriptions_for_send_last,
		     [Host, JID]) of
	{result, Subs} ->
	    [{Node, SubId, SubJID}
	     || {Node, Sub, SubId, SubJID} <- Subs,
		Sub =:= subscribed, (SubJID == LJID) or (SubJID == BJID)];
	_ ->
	    []
    end;
%% sql version already filter result by on_sub_and_presence
get_subscriptions_for_send_last(Host, PType, _, JID, LJID, BJID) ->
    case node_action(Host, PType,
		     get_entity_subscriptions,
		     [Host, JID]) of
	{result, Subs} ->
	    [{Node, SubId, SubJID}
	     || {Node, Sub, SubId, SubJID} <- Subs,
		Sub =:= subscribed, (SubJID == LJID) or (SubJID == BJID),
		match_option(Node, send_last_published_item, on_sub_and_presence)];
	_ ->
	    []
    end.

-spec set_subscriptions(host(), binary(), jid(), [ps_subscription()]) ->
			       {result, undefined} | {error, stanza_error()}.
set_subscriptions(Host, Node, From, Entities) ->
    Owner = jid:tolower(jid:remove_resource(From)),
    Notify = fun(#ps_subscription{jid = JID, type = Sub}) ->
		     Stanza = #message{
				 from = service_jid(Host),
				 to = JID,
				 sub_els = [#ps_event{
					       subscription = #ps_subscription{
								 jid = JID,
								 type = Sub,
								 node = Node}}]},
		     ejabberd_router:route(Stanza)
	     end,
    Action =
	fun(#pubsub_node{type = Type, id = Nidx, owners = O}) ->
		Owners = node_owners_call(Host, Type, Nidx, O),
		case lists:member(Owner, Owners) of
		    true ->
			Result =
			    lists:foldl(
			      fun(_, {error, _} = Err) ->
				      Err;
				 (#ps_subscription{jid = JID, type = Sub,
						   subid = SubId} = Entity, _) ->
				      case node_call(Host, Type,
						     set_subscriptions,
						     [Nidx, JID, Sub, SubId]) of
					  {error, _} = Err ->
					      Err;
					  _ ->
					      Notify(Entity)
				      end
			      end, ok, Entities),
			case Result of
			    ok -> {result, undefined};
			    {error, _} = Err -> Err
			end;
		    _ ->
			{error, xmpp:err_forbidden(
				  ?T("Owner privileges required"), ejabberd_option:language())}

		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
    end.

-spec get_presence_and_roster_permissions(
	host(), jid(), [ljid()], accessModel(),
	[binary()]) -> {boolean(), boolean()}.
get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups) ->
    if (AccessModel == presence) or (AccessModel == roster) ->
	    case Host of
		{User, Server, _} ->
		    get_roster_info(User, Server, From, AllowedGroups);
		_ ->
		    [{OUser, OServer, _} | _] = Owners,
		    get_roster_info(OUser, OServer, From, AllowedGroups)
	    end;
	true ->
	    {true, true}
    end.

-spec get_roster_info(binary(), binary(), ljid() | jid(), [binary()]) -> {boolean(), boolean()}.
get_roster_info(_, _, {<<>>, <<>>, _}, _) ->
    {false, false};
get_roster_info(OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, _}, AllowedGroups) ->
    LJID = {SubscriberUser, SubscriberServer, <<>>},
    {Subscription, _Ask, Groups} = ejabberd_hooks:run_fold(roster_get_jid_info,
	    OwnerServer, {none, none, []},
	    [OwnerUser, OwnerServer, LJID]),
    PresenceSubscription = Subscription == both orelse
	Subscription == from orelse
	{OwnerUser, OwnerServer} == {SubscriberUser, SubscriberServer},
    RosterGroup = lists:any(fun (Group) ->
		    lists:member(Group, AllowedGroups)
	    end,
	    Groups),
    {PresenceSubscription, RosterGroup};
get_roster_info(OwnerUser, OwnerServer, JID, AllowedGroups) ->
    get_roster_info(OwnerUser, OwnerServer, jid:tolower(JID), AllowedGroups).

-spec preconditions_met(pubsub_publish_options:result(),
			pubsub_node_config:result()) -> boolean().
preconditions_met(PubOpts, NodeOpts) ->
    lists:all(fun(Opt) -> lists:member(Opt, NodeOpts) end, PubOpts).

-spec service_jid(jid() | ljid() | binary()) -> jid().
service_jid(#jid{} = Jid) -> Jid;
service_jid({U, S, R}) -> jid:make(U, S, R);
service_jid(Host) -> jid:make(Host).

%% @doc <p>Check if a notification must be delivered or not based on
%% node and subscription options.</p>
-spec is_to_deliver(ljid(), items | nodes, integer(), nodeOptions(), subOptions()) -> boolean().
is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) ->
    sub_to_deliver(LJID, NotifyType, Depth, SubOptions)
    andalso node_to_deliver(LJID, NodeOptions).

-spec sub_to_deliver(ljid(), items | nodes, integer(), subOptions()) -> boolean().
sub_to_deliver(_LJID, NotifyType, Depth, SubOptions) ->
    lists:all(fun (Option) ->
		sub_option_can_deliver(NotifyType, Depth, Option)
	end,
	SubOptions).

-spec node_to_deliver(ljid(), nodeOptions()) -> boolean().
node_to_deliver(LJID, NodeOptions) ->
    presence_can_deliver(LJID, get_option(NodeOptions, presence_based_delivery)).

-spec sub_option_can_deliver(items | nodes, integer(), _) -> boolean().
sub_option_can_deliver(items, _, {subscription_type, nodes}) -> false;
sub_option_can_deliver(nodes, _, {subscription_type, items}) -> false;
sub_option_can_deliver(_, _, {subscription_depth, all}) -> true;
sub_option_can_deliver(_, Depth, {subscription_depth, D}) -> Depth =< D;
sub_option_can_deliver(_, _, {deliver, false}) -> false;
sub_option_can_deliver(_, _, {expire, When}) -> erlang:timestamp() < When;
sub_option_can_deliver(_, _, _) -> true.

-spec presence_can_deliver(ljid(), boolean()) -> boolean().
presence_can_deliver(_, false) ->
    true;
presence_can_deliver({User, Server, Resource}, true) ->
    case ejabberd_sm:get_user_present_resources(User, Server) of
	[] ->
	    false;
	Ss ->
	    lists:foldl(fun
		    (_, true) ->
			true;
		    ({_, R}, _Acc) ->
		    	case Resource of
			    <<>> -> true;
			    R -> true;
			    _ -> false
			end
		end,
		false, Ss)
    end.

-spec state_can_deliver(ljid(), subOptions()) -> [ljid()].
state_can_deliver({U, S, R}, []) -> [{U, S, R}];
state_can_deliver({U, S, R}, SubOptions) ->
    case lists:keysearch(show_values, 1, SubOptions) of
	%% If not in suboptions, item can be delivered, case doesn't apply
	false -> [{U, S, R}];
	%% If in a suboptions ...
	{_, {_, ShowValues}} ->
	    Resources = case R of
		%% If the subscriber JID is a bare one, get all its resources
		<<>> -> user_resources(U, S);
		%% If the subscriber JID is a full one, use its resource
		R -> [R]
	    end,
	    lists:foldl(fun (Resource, Acc) ->
			get_resource_state({U, S, Resource}, ShowValues, Acc)
		end,
		[], Resources)
    end.

-spec get_resource_state(ljid(), [binary()], [ljid()]) -> [ljid()].
get_resource_state({U, S, R}, ShowValues, JIDs) ->
    case ejabberd_sm:get_session_pid(U, S, R) of
	none ->
	    %% If no PID, item can be delivered
	    lists:append([{U, S, R}], JIDs);
	Pid ->
	    Show = case ejabberd_c2s:get_presence(Pid) of
		       #presence{type = unavailable} -> <<"unavailable">>;
		       #presence{show = undefined} -> <<"online">>;
		       #presence{show = Sh} -> atom_to_binary(Sh, latin1)
	    end,
	    case lists:member(Show, ShowValues) of
		%% If yes, item can be delivered
		true -> lists:append([{U, S, R}], JIDs);
		%% If no, item can't be delivered
		false -> JIDs
	    end
    end.

-spec payload_xmlelements([xmlel()]) -> non_neg_integer().
payload_xmlelements(Payload) ->
    payload_xmlelements(Payload, 0).

-spec payload_xmlelements([xmlel()], non_neg_integer()) -> non_neg_integer().
payload_xmlelements([], Count) -> Count;
payload_xmlelements([#xmlel{} | Tail], Count) ->
    payload_xmlelements(Tail, Count + 1);
payload_xmlelements([_ | Tail], Count) ->
    payload_xmlelements(Tail, Count).

-spec items_els(binary(), nodeOptions(), [#pubsub_item{}]) -> ps_items().
items_els(Node, Options, Items) ->
    Els = case get_option(Options, itemreply) of
	publisher ->
	    [#ps_item{id = ItemId, sub_els = Payload, publisher = jid:encode(USR)}
	     || #pubsub_item{itemid = {ItemId, _}, payload = Payload, modification = {_, USR}}
		<- Items];
	_ ->
	    [#ps_item{id = ItemId, sub_els = Payload}
	     || #pubsub_item{itemid = {ItemId, _}, payload = Payload}
		<- Items]
    end,
    #ps_items{node = Node, items = Els}.

%%%%%% broadcast functions

-spec broadcast_publish_item(host(), binary(), nodeIdx(), binary(),
			     nodeOptions(), binary(), jid(), [xmlel()], _) ->
				    {result, boolean()}.
broadcast_publish_item(Host, Node, Nidx, Type, NodeOptions, ItemId, From, Payload, Removed) ->
    case get_collection_subscriptions(Host, Node) of
	{result, SubsByDepth} ->
	    ItemPublisher = case get_option(NodeOptions, itemreply) of
				publisher -> jid:encode(From);
				_ -> <<>>
			    end,
	    ItemPayload = case get_option(NodeOptions, deliver_payloads) of
			      true -> Payload;
			      false -> []
			  end,
	    ItemsEls = #ps_items{node = Node,
				 items = [#ps_item{id = ItemId,
						   publisher = ItemPublisher,
						   sub_els = ItemPayload}]},
	    Stanza = #message{ sub_els = [#ps_event{items = ItemsEls}]},
	    broadcast_stanza(Host, From, Node, Nidx, Type,
			     NodeOptions, SubsByDepth, items, Stanza, true),
	    case Removed of
		[] ->
		    ok;
		_ ->
		    case get_option(NodeOptions, notify_retract) of
			true ->
			    RetractStanza = #message{
					       sub_els =
						   [#ps_event{
						       items = #ps_items{
								  node = Node,
								  retract = Removed}}]},
			    broadcast_stanza(Host, Node, Nidx, Type,
				NodeOptions, SubsByDepth,
				items, RetractStanza, true);
			_ ->
			    ok
		    end
	    end,
	    {result, true};
	_ ->
	    {result, false}
    end.

-spec broadcast_retract_items(host(), binary(), nodeIdx(), binary(),
			      nodeOptions(), [itemId()]) -> {result, boolean()}.
broadcast_retract_items(Host, Node, Nidx, Type, NodeOptions, ItemIds) ->
    broadcast_retract_items(Host, Node, Nidx, Type, NodeOptions, ItemIds, false).

-spec broadcast_retract_items(host(), binary(), nodeIdx(), binary(),
			      nodeOptions(), [itemId()], boolean()) -> {result, boolean()}.
broadcast_retract_items(_Host, _Node, _Nidx, _Type, _NodeOptions, [], _ForceNotify) ->
    {result, false};
broadcast_retract_items(Host, Node, Nidx, Type, NodeOptions, ItemIds, ForceNotify) ->
    case (get_option(NodeOptions, notify_retract) or ForceNotify) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		{result, SubsByDepth} ->
		    Stanza = #message{
				sub_els =
				    [#ps_event{
					items = #ps_items{
						   node = Node,
						   retract = ItemIds}}]},
		    broadcast_stanza(Host, Node, Nidx, Type,
			NodeOptions, SubsByDepth, items, Stanza, true),
		    {result, true};
		_ ->
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

-spec broadcast_purge_node(host(), binary(), nodeIdx(), binary(), nodeOptions()) -> {result, boolean()}.
broadcast_purge_node(Host, Node, Nidx, Type, NodeOptions) ->
    case get_option(NodeOptions, notify_retract) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		{result, SubsByDepth} ->
		    Stanza = #message{sub_els = [#ps_event{purge = Node}]},
		    broadcast_stanza(Host, Node, Nidx, Type,
			NodeOptions, SubsByDepth, nodes, Stanza, false),
		    {result, true};
		_ ->
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

-spec broadcast_removed_node(host(), binary(), nodeIdx(), binary(),
			     nodeOptions(), subs_by_depth()) -> {result, boolean()}.
broadcast_removed_node(Host, Node, Nidx, Type, NodeOptions, SubsByDepth) ->
    case get_option(NodeOptions, notify_delete) of
	true ->
	    case SubsByDepth of
		[] ->
		    {result, false};
		_ ->
		    Stanza = #message{sub_els = [#ps_event{delete = {Node, <<>>}}]},
		    broadcast_stanza(Host, Node, Nidx, Type,
			NodeOptions, SubsByDepth, nodes, Stanza, false),
		    {result, true}
	    end;
	_ ->
	    {result, false}
    end.

-spec broadcast_created_node(host(), binary(), nodeIdx(), binary(),
			     nodeOptions(), subs_by_depth()) -> {result, boolean()}.
broadcast_created_node(_, _, _, _, _, []) ->
    {result, false};
broadcast_created_node(Host, Node, Nidx, Type, NodeOptions, SubsByDepth) ->
    Stanza = #message{sub_els = [#ps_event{create = Node}]},
    broadcast_stanza(Host, Node, Nidx, Type, NodeOptions, SubsByDepth, nodes, Stanza, true),
    {result, true}.

-spec broadcast_config_notification(host(), binary(), nodeIdx(), binary(),
				    nodeOptions(), binary()) -> {result, boolean()}.
broadcast_config_notification(Host, Node, Nidx, Type, NodeOptions, Lang) ->
    case get_option(NodeOptions, notify_config) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		{result, SubsByDepth} ->
		    Content = case get_option(NodeOptions, deliver_payloads) of
			true ->
			    #xdata{type = result,
				   fields = get_configure_xfields(
					      Type, NodeOptions, Lang, [])};
			false ->
			    undefined
		    end,
		    Stanza = #message{
				sub_els = [#ps_event{
					      configuration = {Node, Content}}]},
		    broadcast_stanza(Host, Node, Nidx, Type,
			NodeOptions, SubsByDepth, nodes, Stanza, false),
		    {result, true};
		_ ->
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

-spec get_collection_subscriptions(host(), nodeId()) -> {result, subs_by_depth()} |
							{error, stanza_error()}.
get_collection_subscriptions(Host, Node) ->
    Action = fun() -> get_node_subs_by_depth(Host, Node, service_jid(Host)) end,
    transaction(Host, Action, sync_dirty).

-spec get_node_subs_by_depth(host(), nodeId(), jid()) -> {result, subs_by_depth()} |
							 {error, stanza_error()}.
get_node_subs_by_depth(Host, Node, From) ->
    case tree_call(Host, get_parentnodes_tree, [Host, Node, From]) of
	ParentTree when is_list(ParentTree) ->
	    {result,
	     lists:filtermap(
	       fun({Depth, Nodes}) ->
		       case lists:filtermap(
			      fun(N) ->
				      case get_node_subs(Host, N) of
					  {result, Result} -> {true, {N, Result}};
					  _ -> false
				      end
			      end, Nodes) of
			   [] -> false;
			   Subs -> {true, {Depth, Subs}}
		       end
	       end, ParentTree)};
	Error ->
	    Error
    end.

-spec get_node_subs(host(), #pubsub_node{}) -> {result, [{ljid(), subId(), subOptions()}]} |
					       {error, stanza_error()}.
get_node_subs(Host, #pubsub_node{type = Type, id = Nidx}) ->
    WithOptions =  lists:member(<<"subscription-options">>, plugin_features(Host, Type)),
    case node_call(Host, Type, get_node_subscriptions, [Nidx]) of
	{result, Subs} -> {result, get_options_for_subs(Host, Nidx, Subs, WithOptions)};
	Other -> Other
    end.

-spec get_options_for_subs(host(), nodeIdx(),
			   [{ljid(), subscription(), subId()}],
			   boolean()) ->
				  [{ljid(), subId(), subOptions()}].
get_options_for_subs(_Host, _Nidx, Subs, false) ->
    lists:foldl(fun({JID, subscribed, SubID}, Acc) ->
		    [{JID, SubID, []} | Acc];
		   (_, Acc) ->
		    Acc
	end, [], Subs);
get_options_for_subs(Host, Nidx, Subs, true) ->
    SubModule = subscription_plugin(Host),
    lists:foldl(fun({JID, subscribed, SubID}, Acc) ->
		case SubModule:get_subscription(JID, Nidx, SubID) of
		    #pubsub_subscription{options = Options} -> [{JID, SubID, Options} | Acc];
		    {error, notfound} -> [{JID, SubID, []} | Acc]
		end;
	    (_, Acc) ->
		Acc
	end, [], Subs).

-spec broadcast_stanza(host(), nodeId(), nodeIdx(), binary(),
		       nodeOptions(), subs_by_depth(),
		       items | nodes, stanza(), boolean()) -> ok.
broadcast_stanza(Host, _Node, _Nidx, _Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    NotificationType = get_option(NodeOptions, notification_type, headline),
    BroadcastAll = get_option(NodeOptions, broadcast_all_resources), %% XXX this is not standard, but useful
    Stanza = add_message_type(
	       xmpp:set_from(BaseStanza, service_jid(Host)),
	       NotificationType),
    %% Handles explicit subscriptions
    SubIDsByJID = subscribed_nodes_by_jid(NotifyType, SubsByDepth),
    lists:foreach(fun ({LJID, _NodeName, SubIDs}) ->
		LJIDs = case BroadcastAll of
		    true ->
			{U, S, _} = LJID,
			[{U, S, R} || R <- user_resources(U, S)];
		    false ->
			[LJID]
		end,
		%% Determine if the stanza should have SHIM ('SubID' and 'name') headers
		StanzaToSend = case {SHIM, SubIDs} of
		    {false, _} ->
			Stanza;
		    %% If there's only one SubID, don't add it
		    {true, [_]} ->
			Stanza;
		    {true, SubIDs} ->
			add_shim_headers(Stanza, subid_shim(SubIDs))
		end,
		lists:foreach(fun(To) ->
			    ejabberd_router:route(
			      xmpp:set_to(StanzaToSend, jid:make(To)))
		    end, LJIDs)
	end, SubIDsByJID).

-spec broadcast_stanza(host(), jid(), nodeId(), nodeIdx(), binary(),
		       nodeOptions(), subs_by_depth(), items | nodes,
		       stanza(), boolean()) -> ok.
broadcast_stanza({LUser, LServer, LResource}, Publisher, Node, Nidx, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    broadcast_stanza({LUser, LServer, <<>>}, Node, Nidx, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM),
    %% Handles implicit presence subscriptions
    SenderResource = user_resource(LUser, LServer, LResource),
    NotificationType = get_option(NodeOptions, notification_type, headline),
    %% set the from address on the notification to the bare JID of the account owner
    %% Also, add "replyto" if entity has presence subscription to the account owner
    %% See XEP-0163 1.1 section 4.3.1
    Owner = jid:make(LUser, LServer),
    FromBareJid = xmpp:set_from(BaseStanza, Owner),
    Stanza = add_extended_headers(
	       add_message_type(FromBareJid, NotificationType),
	       extended_headers([Publisher])),
    Pred = fun(To) -> delivery_permitted(Owner, To, NodeOptions) end,
    ejabberd_sm:route(jid:make(LUser, LServer, SenderResource),
		      {pep_message, <<((Node))/binary, "+notify">>, Stanza, Pred}),
    ejabberd_router:route(xmpp:set_to(Stanza, jid:make(LUser, LServer)));
broadcast_stanza(Host, _Publisher, Node, Nidx, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    broadcast_stanza(Host, Node, Nidx, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM).

-spec c2s_handle_info(ejabberd_c2s:state(), term()) -> ejabberd_c2s:state().
c2s_handle_info(#{lserver := LServer} = C2SState,
		{pep_message, Feature, Packet, Pred}) when is_function(Pred) ->
    [maybe_send_pep_stanza(LServer, USR, Caps, Feature, Packet)
     || {USR, Caps} <- mod_caps:list_features(C2SState), Pred(USR)],
    {stop, C2SState};
c2s_handle_info(#{lserver := LServer} = C2SState,
		{pep_message, Feature, Packet, {_, _, _} = USR}) ->
    case mod_caps:get_user_caps(USR, C2SState) of
	{ok, Caps} -> maybe_send_pep_stanza(LServer, USR, Caps, Feature, Packet);
	error -> ok
    end,
    {stop, C2SState};
c2s_handle_info(C2SState, _) ->
    C2SState.

-spec send_items(host(), nodeId(), nodeIdx(), binary(),
		 nodeOptions(), ljid(), last | integer()) -> ok.
send_items(Host, Node, Nidx, Type, Options, LJID, Number) ->
    send_items(Host, Node, Nidx, Type, Options, Host, LJID, LJID, Number).
send_items(Host, Node, Nidx, Type, Options, Publisher, SubLJID, ToLJID, Number) ->
    Items = case max_items(Host, Options) of
		1 ->
		    get_only_item(Host, Type, Nidx, SubLJID);
		_ ->
		    get_last_items(Host, Type, Nidx, SubLJID, Number)
	    end,
    case Items of
	[] ->
	    ok;
	Items ->
	    Delay = case Number of
		last -> % handle section 6.1.7 of XEP-0060
		    [Last] = Items,
		    {Stamp, _USR} = Last#pubsub_item.modification,
		    [#delay{stamp = Stamp}];
		_ ->
		    []
	    end,
	    Stanza = #message{
			sub_els = [#ps_event{items = items_els(Node, Options, Items)}
				   | Delay]},
	    NotificationType = get_option(Options, notification_type, headline),
	    send_stanza(Publisher, ToLJID, Node,
			add_message_type(Stanza, NotificationType))
    end.

-spec send_stanza(host(), ljid(), binary(), stanza()) -> ok.
send_stanza({LUser, LServer, _} = Publisher, USR, Node, BaseStanza) ->
    Stanza = xmpp:set_from(BaseStanza, jid:make(LUser, LServer)),
    USRs = case USR of
	       {PUser, PServer, <<>>} ->
		   [{PUser, PServer, PRessource}
		    || PRessource <- user_resources(PUser, PServer)];
	       _ ->
		   [USR]
	   end,
    lists:foreach(
      fun(To) ->
	      ejabberd_sm:route(
		jid:make(Publisher),
		{pep_message, <<((Node))/binary, "+notify">>,
		 add_extended_headers(
		   Stanza, extended_headers([jid:make(Publisher)])),
		 To})
      end, USRs);
send_stanza(Host, USR, _Node, Stanza) ->
    ejabberd_router:route(
      xmpp:set_from_to(Stanza, service_jid(Host), jid:make(USR))).

-spec maybe_send_pep_stanza(binary(), ljid(), caps(), binary(), stanza()) -> ok.
maybe_send_pep_stanza(LServer, USR, Caps, Feature, Packet) ->
    Features = mod_caps:get_features(LServer, Caps),
    case lists:member(Feature, Features) of
	true ->
	    ejabberd_router:route(xmpp:set_to(Packet, jid:make(USR)));
	false ->
	    ok
    end.

-spec send_last_items(jid()) -> ok.
send_last_items(JID) ->
    ServerHost = JID#jid.lserver,
    Host = host(ServerHost),
    DBType = config(ServerHost, db_type),
    LJID = jid:tolower(JID),
    BJID = jid:remove_resource(LJID),
    lists:foreach(
      fun(PType) ->
	      Subs = get_subscriptions_for_send_last(Host, PType, DBType, JID, LJID, BJID),
	      lists:foreach(
		fun({#pubsub_node{nodeid = {_, Node}, type = Type, id = Nidx,
				  options = Options}, _, SubJID})
		      when Type == PType->
			send_items(Host, Node, Nidx, PType, Options, Host, SubJID, LJID, 1);
		   (_) ->
			ok
		end,
		lists:usort(Subs))
      end, config(ServerHost, plugins)).
% pep_from_offline hack can not work anymore, as sender c2s does not
% exists when sender is offline, so we can't get match receiver caps
% does it make sens to send PEP from an offline contact anyway ?
%    case config(ServerHost, ignore_pep_from_offline) of
%	false ->
%	    Roster = ejabberd_hooks:run_fold(roster_get, ServerHost, [],
%					     [{JID#jid.luser, ServerHost}]),
%	    lists:foreach(
%	      fun(#roster{jid = {U, S, R}, subscription = Sub})
%		    when Sub == both orelse Sub == from,
%			 S == ServerHost ->
%		      case user_resources(U, S) of
%			  [] -> send_last_pep(jid:make(U, S, R), JID);
%			  _ -> ok %% this is already handled by presence probe
%		      end;
%		 (_) ->
%		      ok %% we can not do anything in any cases
%	      end, Roster);
%	true ->
%	    ok
%    end.
send_last_pep(From, To, Features) ->
    ServerHost = From#jid.lserver,
    Host = host(ServerHost),
    Publisher = jid:tolower(From),
    Owner = jid:remove_resource(Publisher),
    NotifyNodes =
    case Features of
        _ when is_list(Features) ->
            lists:filtermap(
                fun(V) ->
                    Vs = byte_size(V) - 7,
                    case V of
                        <<NotNode:Vs/binary, "+notify">> ->
                            {true, NotNode};
                        _ ->
                            false
                    end
                end, Features);
        _ ->
            unknown
    end,
    case tree_action(Host, get_nodes, [Owner, infinity]) of
        Nodes when is_list(Nodes) ->
            lists:foreach(
                fun(#pubsub_node{nodeid = {_, Node}, type = Type, id = Nidx, options = Options}) ->
                    MaybeNotify =
                    case NotifyNodes of
                        unknown -> true;
                        _ -> lists:member(Node, NotifyNodes)
                    end,
                    case MaybeNotify andalso match_option(Options, send_last_published_item, on_sub_and_presence) of
                        true ->
                            case delivery_permitted(From, To, Options) of
                                true ->
                                    LJID = jid:tolower(To),
                                    send_items(Owner, Node, Nidx, Type, Options,
                                               Publisher, LJID, LJID, 1);
                                false ->
                                    ok
                            end;
                        _ ->
                            ok
                    end
                end, Nodes);
        _ ->
            ok
    end.

-spec subscribed_nodes_by_jid(items | nodes, subs_by_depth()) -> [{ljid(), binary(), subId()}].
subscribed_nodes_by_jid(NotifyType, SubsByDepth) ->
    NodesToDeliver = fun (Depth, Node, Subs, Acc) ->
	NodeName = case Node#pubsub_node.nodeid of
		       {_, N} -> N;
		       Other -> Other
		   end,
	NodeOptions = Node#pubsub_node.options,
	lists:foldl(fun({LJID, SubID, SubOptions}, {JIDs, Recipients}) ->
	    case is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) of
		true ->
		    case state_can_deliver(LJID, SubOptions) of
			[] -> {JIDs, Recipients};
			[LJID] -> {JIDs, [{LJID, NodeName, [SubID]} | Recipients]};
			JIDsToDeliver ->
			    lists:foldl(
				fun(JIDToDeliver, {JIDsAcc, RecipientsAcc}) ->
					case lists:member(JIDToDeliver, JIDs) of
					    %% check if the JIDs co-accumulator contains the Subscription Jid,
					    false ->
						%%  - if not,
						%%  - add the Jid to JIDs list co-accumulator ;
						%%  - create a tuple of the Jid, Nidx, and SubID (as list),
						%%    and add the tuple to the Recipients list co-accumulator
						{[JIDToDeliver | JIDsAcc],
						    [{JIDToDeliver, NodeName, [SubID]}
							| RecipientsAcc]};
					    true ->
						%% - if the JIDs co-accumulator contains the Jid
						%%   get the tuple containing the Jid from the Recipient list co-accumulator
						{_, {JIDToDeliver, NodeName1, SubIDs}} =
						    lists:keysearch(JIDToDeliver, 1, RecipientsAcc),
						%%   delete the tuple from the Recipients list
						% v1 : Recipients1 = lists:keydelete(LJID, 1, Recipients),
						% v2 : Recipients1 = lists:keyreplace(LJID, 1, Recipients, {LJID, Nidx1, [SubID | SubIDs]}),
						%%   add the SubID to the SubIDs list in the tuple,
						%%   and add the tuple back to the Recipients list co-accumulator
						% v1.1 : {JIDs, lists:append(Recipients1, [{LJID, Nidx1, lists:append(SubIDs, [SubID])}])}
						% v1.2 : {JIDs, [{LJID, Nidx1, [SubID | SubIDs]} | Recipients1]}
						% v2: {JIDs, Recipients1}
						{JIDsAcc,
						    lists:keyreplace(JIDToDeliver, 1,
							RecipientsAcc,
							{JIDToDeliver, NodeName1,
							    [SubID | SubIDs]})}
					end
				end, {JIDs, Recipients}, JIDsToDeliver)
		    end;
		false ->
		    {JIDs, Recipients}
	    end
	end, Acc, Subs)
	end,
    DepthsToDeliver = fun({Depth, SubsByNode}, Acc1) ->
	lists:foldl(fun({Node, Subs}, Acc2) ->
			    NodesToDeliver(Depth, Node, Subs, Acc2)
		    end, Acc1, SubsByNode)
	end,
    {_, JIDSubs} = lists:foldl(DepthsToDeliver, {[], []}, SubsByDepth),
    JIDSubs.

-spec delivery_permitted(jid() | ljid(), jid() | ljid(), nodeOptions()) -> boolean().
delivery_permitted(From, To, Options) ->
    LFrom = jid:tolower(From),
    LTo = jid:tolower(To),
    RecipientIsOwner = jid:remove_resource(LFrom) == jid:remove_resource(LTo),
    %% TODO: Fix the 'whitelist'/'authorize' cases for last PEP notifications.
    %% Currently, only node owners receive those.
    case get_option(Options, access_model) of
	open -> true;
	presence -> true;
	whitelist -> RecipientIsOwner;
	authorize -> RecipientIsOwner;
	roster ->
	   Grps = get_option(Options, roster_groups_allowed, []),
	   {LUser, LServer, _} = LFrom,
	   {_, IsInGrp} = get_roster_info(LUser, LServer, LTo, Grps),
	   IsInGrp
    end.

-spec user_resources(binary(), binary()) -> [binary()].
user_resources(User, Server) ->
    ejabberd_sm:get_user_resources(User, Server).

-spec user_resource(binary(), binary(), binary()) -> binary().
user_resource(User, Server, <<>>) ->
    case user_resources(User, Server) of
	[R | _] -> R;
	_ -> <<>>
    end;
user_resource(_, _, Resource) ->
    Resource.

%%%%%%% Configuration handling
-spec get_configure(host(), binary(), binary(), jid(),
		    binary()) -> {error, stanza_error()} | {result, pubsub_owner()}.
get_configure(Host, ServerHost, Node, From, Lang) ->
    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx}) ->
	    case node_call(Host, Type, get_affiliation, [Nidx, From]) of
		{result, owner} ->
		    Groups = ejabberd_hooks:run_fold(roster_groups, ServerHost, [], [ServerHost]),
		    Fs = get_configure_xfields(Type, Options, Lang, Groups),
		    {result, #pubsub_owner{
				configure =
				    {Node, #xdata{type = form, fields = Fs}}}};
		{result, _} ->
		    {error, xmpp:err_forbidden(?T("Owner privileges required"), Lang)};
		Error ->
		    Error
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
    end.

-spec get_default(host(), binary(), jid(), binary()) -> {result, pubsub_owner()}.
get_default(Host, Node, _From, Lang) ->
    Type = select_type(serverhost(Host), Host, Node),
    Options = node_options(Host, Type),
    Fs = get_configure_xfields(Type, Options, Lang, []),
    {result, #pubsub_owner{default = {<<>>, #xdata{type = form, fields = Fs}}}}.

-spec match_option(#pubsub_node{} | [{atom(), any()}], atom(), any()) -> boolean().
match_option(Node, Var, Val) when is_record(Node, pubsub_node) ->
    match_option(Node#pubsub_node.options, Var, Val);
match_option(Options, Var, Val) when is_list(Options) ->
    get_option(Options, Var) == Val;
match_option(_, _, _) ->
    false.

-spec get_option([{atom(), any()}], atom()) -> any().
get_option([], _) -> false;
get_option(Options, Var) -> get_option(Options, Var, false).

-spec get_option([{atom(), any()}], atom(), any()) -> any().
get_option(Options, Var, Def) ->
    case lists:keysearch(Var, 1, Options) of
	{value, {_Val, Ret}} -> Ret;
	_ -> Def
    end.

-spec node_options(host(), binary()) -> [{atom(), any()}].
node_options(Host, Type) ->
    DefaultOpts = node_plugin_options(Host, Type),
    case config(Host, plugins) of
	[Type|_] -> config(Host, default_node_config, DefaultOpts);
	_ -> DefaultOpts
    end.

-spec node_plugin_options(host(), binary()) -> [{atom(), any()}].
node_plugin_options(Host, Type) ->
    Module = plugin(Host, Type),
    case catch Module:options() of
	{'EXIT', {undef, _}} ->
	    DefaultModule = plugin(Host, ?STDNODE),
	    DefaultModule:options();
	Result ->
	    Result
    end.

-spec node_owners_action(host(), binary(), nodeIdx(), [ljid()]) -> [ljid()].
node_owners_action(Host, Type, Nidx, []) ->
    case node_action(Host, Type, get_node_affiliations, [Nidx]) of
	{result, Affs} -> [LJID || {LJID, Aff} <- Affs, Aff =:= owner];
	_ -> []
    end;
node_owners_action(_Host, _Type, _Nidx, Owners) ->
    Owners.

-spec node_owners_call(host(), binary(), nodeIdx(), [ljid()]) -> [ljid()].
node_owners_call(Host, Type, Nidx, []) ->
    case node_call(Host, Type, get_node_affiliations, [Nidx]) of
	{result, Affs} -> [LJID || {LJID, Aff} <- Affs, Aff =:= owner];
	_ -> []
    end;
node_owners_call(_Host, _Type, _Nidx, Owners) ->
    Owners.

node_config(Node, ServerHost) ->
    Opts = mod_pubsub_opt:force_node_config(ServerHost),
    node_config(Node, ServerHost, Opts).

node_config(Node, ServerHost, [{RE, Opts}|NodeOpts]) ->
    case re:run(Node, RE) of
	{match, _} ->
	    Opts;
	nomatch ->
	    node_config(Node, ServerHost, NodeOpts)
    end;
node_config(_, _, []) ->
    [].

%% @spec (Host, Options) -> MaxItems
%%         Host = host()
%%         Options = [Option]
%%         Option = {Key::atom(), Value::term()}
%%         MaxItems = integer() | unlimited
%% @doc <p>Return the maximum number of items for a given node.</p>
%% <p>Unlimited means that there is no limit in the number of items that can
%% be stored.</p>
-spec max_items(host(), [{atom(), any()}]) -> non_neg_integer() | unlimited.
max_items(Host, Options) ->
    case get_option(Options, persist_items) of
	true ->
	    case get_option(Options, max_items) of
		I when is_integer(I), I < 0 -> 0;
		I when is_integer(I) -> I;
		_ -> get_max_items_node(Host)
	    end;
	false ->
	    case get_option(Options, send_last_published_item) of
		never ->
		    0;
		_ ->
		    case is_last_item_cache_enabled(Host) of
			true -> 0;
			false -> 1
		    end
	    end
    end.

-spec item_expire(host(), [{atom(), any()}]) -> non_neg_integer() | infinity.
item_expire(Host, Options) ->
    case get_option(Options, item_expire) of
	I when is_integer(I), I < 0 -> 0;
	I when is_integer(I) -> I;
	_ -> get_max_item_expire_node(Host)
    end.

-spec get_configure_xfields(_, pubsub_node_config:result(),
			    binary(), [binary()]) -> [xdata_field()].
get_configure_xfields(_Type, Options, Lang, Groups) ->
    pubsub_node_config:encode(
      lists:filtermap(
	fun({roster_groups_allowed, Value}) ->
		{true, {roster_groups_allowed, Value, Groups}};
	   ({sql, _}) -> false;
	   ({rsm, _}) -> false;
	   (_) -> true
	end, Options),
      Lang).

%%<p>There are several reasons why the node configuration request might fail:</p>
%%<ul>
%%<li>The service does not support node configuration.</li>
%%<li>The requesting entity does not have sufficient privileges to configure the node.</li>
%%<li>The request did not specify a node.</li>
%%<li>The node has no configuration options.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
-spec set_configure(host(), binary(), jid(), [{binary(), [binary()]}],
		    binary()) -> {result, undefined} | {error, stanza_error()}.
set_configure(_Host, <<>>, _From, _Config, _Lang) ->
    {error, extended_error(xmpp:err_bad_request(), err_nodeid_required())};
set_configure(Host, Node, From, Config, Lang) ->
    Action =
	fun(#pubsub_node{options = Options, type = Type, id = Nidx} = N) ->
		case node_call(Host, Type, get_affiliation, [Nidx, From]) of
		    {result, owner} ->
			OldOpts = case Options of
				      [] -> node_options(Host, Type);
				      _ -> Options
				  end,
			NewOpts = merge_config(
				    [node_config(Node, serverhost(Host)),
				     Config, OldOpts]),
			case tree_call(Host,
				       set_node,
				       [N#pubsub_node{options = NewOpts}]) of
			    {result, Nidx} -> {result, NewOpts};
			    ok -> {result, NewOpts};
			    Err -> Err
			end;
		    {result, _} ->
			{error, xmpp:err_forbidden(
				  ?T("Owner privileges required"), Lang)};
		    Error ->
			Error
		end
	end,
    case transaction(Host, Node, Action, transaction) of
	{result, {TNode, Options}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    broadcast_config_notification(Host, Node, Nidx, Type, Options, Lang),
	    {result, undefined};
	Other ->
	    Other
    end.

-spec merge_config([[proplists:property()]]) -> [proplists:property()].
merge_config(ListOfConfigs) ->
    lists:ukeysort(1, lists:flatten(ListOfConfigs)).

-spec decode_node_config(undefined | xdata(), binary(), binary()) ->
				pubsub_node_config:result() |
				{error, stanza_error()}.
decode_node_config(undefined, _, _) ->
    [];
decode_node_config(#xdata{fields = Fs}, Host, Lang) ->
    try
	Config = pubsub_node_config:decode(Fs),
	Max = get_max_items_node(Host),
	case {check_opt_range(max_items, Config, Max),
	      check_opt_range(max_payload_size, Config, ?MAX_PAYLOAD_SIZE)} of
	    {true, true} ->
		Config;
	    {true, false} ->
		erlang:error(
		  {pubsub_node_config,
		   {bad_var_value, <<"pubsub#max_payload_size">>,
		    ?NS_PUBSUB_NODE_CONFIG}});
	    {false, _} ->
		erlang:error(
		  {pubsub_node_config,
		   {bad_var_value, <<"pubsub#max_items">>,
		    ?NS_PUBSUB_NODE_CONFIG}})
	end
    catch _:{pubsub_node_config, Why} ->
	    Txt = pubsub_node_config:format_error(Why),
	    {error, xmpp:err_resource_constraint(Txt, Lang)}
    end.

-spec decode_subscribe_options(undefined | xdata(), binary()) ->
				      pubsub_subscribe_options:result() |
				      {error, stanza_error()}.
decode_subscribe_options(undefined, _) ->
    [];
decode_subscribe_options(#xdata{fields = Fs}, Lang) ->
    try pubsub_subscribe_options:decode(Fs)
    catch _:{pubsub_subscribe_options, Why} ->
	    Txt = pubsub_subscribe_options:format_error(Why),
	    {error, xmpp:err_resource_constraint(Txt, Lang)}
    end.

-spec decode_publish_options(undefined | xdata(), binary()) ->
				    pubsub_publish_options:result() |
				    {error, stanza_error()}.
decode_publish_options(undefined, _) ->
    [];
decode_publish_options(#xdata{fields = Fs}, Lang) ->
    try pubsub_publish_options:decode(Fs)
    catch _:{pubsub_publish_options, Why} ->
	    Txt = pubsub_publish_options:format_error(Why),
	    {error, xmpp:err_resource_constraint(Txt, Lang)}
    end.

-spec decode_get_pending(xdata(), binary()) ->
				pubsub_get_pending:result() |
				{error, stanza_error()}.
decode_get_pending(#xdata{fields = Fs}, Lang) ->
    try pubsub_get_pending:decode(Fs)
    catch _:{pubsub_get_pending, Why} ->
	    Txt = pubsub_get_pending:format_error(Why),
	    {error, xmpp:err_resource_constraint(Txt, Lang)}
    end.

-spec check_opt_range(atom(), [proplists:property()],
		      non_neg_integer() | unlimited) -> boolean().
check_opt_range(_Opt, _Opts, unlimited) ->
    true;
check_opt_range(Opt, Opts, Max) ->
    case proplists:get_value(Opt, Opts, Max) of
	max -> true;
	Val -> Val =< Max
    end.

-spec get_max_items_node(host()) -> unlimited | non_neg_integer().
get_max_items_node(Host) ->
    config(Host, max_items_node, ?MAXITEMS).

-spec get_max_item_expire_node(host()) -> infinity | non_neg_integer().
get_max_item_expire_node(Host) ->
    config(Host, max_item_expire_node, infinity).

-spec get_max_subscriptions_node(host()) -> undefined | non_neg_integer().
get_max_subscriptions_node(Host) ->
    config(Host, max_subscriptions_node, undefined).

%%%% last item cache handling
-spec is_last_item_cache_enabled(host()) -> boolean().
is_last_item_cache_enabled(Host) ->
    config(Host, last_item_cache, false).

-spec set_cached_item(host(), nodeIdx(), binary(), jid(), [xmlel()]) -> ok.
set_cached_item({_, ServerHost, _}, Nidx, ItemId, Publisher, Payload) ->
    set_cached_item(ServerHost, Nidx, ItemId, Publisher, Payload);
set_cached_item(Host, Nidx, ItemId, Publisher, Payload) ->
    case is_last_item_cache_enabled(Host) of
	true ->
	    Stamp = {erlang:timestamp(), jid:tolower(jid:remove_resource(Publisher))},
	    Item = #pubsub_last_item{nodeid = {Host, Nidx},
				     itemid = ItemId,
				     creation = Stamp,
				     payload = Payload},
	    mnesia:dirty_write(Item);
	_ ->
	    ok
    end.

-spec unset_cached_item(host(), nodeIdx()) -> ok.
unset_cached_item({_, ServerHost, _}, Nidx) ->
    unset_cached_item(ServerHost, Nidx);
unset_cached_item(Host, Nidx) ->
    case is_last_item_cache_enabled(Host) of
	true -> mnesia:dirty_delete({pubsub_last_item, {Host, Nidx}});
	_ -> ok
    end.

-spec get_cached_item(host(), nodeIdx()) -> undefined | #pubsub_item{}.
get_cached_item({_, ServerHost, _}, Nidx) ->
    get_cached_item(ServerHost, Nidx);
get_cached_item(Host, Nidx) ->
    case is_last_item_cache_enabled(Host) of
	true ->
	    case mnesia:dirty_read({pubsub_last_item, {Host, Nidx}}) of
		[#pubsub_last_item{itemid = ItemId, creation = Creation, payload = Payload}] ->
		    #pubsub_item{itemid = {ItemId, Nidx},
			payload = Payload, creation = Creation,
			modification = Creation};
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.

%%%% plugin handling
-spec host(binary()) -> binary().
host(ServerHost) ->
    config(ServerHost, host, <<"pubsub.", ServerHost/binary>>).

-spec serverhost(host()) -> binary().
serverhost({_U, ServerHost, _R})->
    serverhost(ServerHost);
serverhost(Host) ->
    ejabberd_router:host_of_route(Host).

-spec tree(host()) -> atom().
tree(Host) ->
    case config(Host, nodetree) of
	undefined -> tree(Host, ?STDTREE);
	Tree -> Tree
    end.

-spec tree(host() | atom(), binary()) -> atom().
tree(_Host, <<"virtual">>) ->
    nodetree_virtual;   % special case, virtual does not use any backend
tree(Host, Name) ->
    submodule(Host, <<"nodetree">>, Name).

-spec plugin(host() | atom(), binary()) -> atom().
plugin(Host, Name) ->
    submodule(Host, <<"node">>, Name).

-spec plugins(host()) -> [binary()].
plugins(Host) ->
    case config(Host, plugins) of
	undefined -> [?STDNODE];
	[] -> [?STDNODE];
	Plugins -> Plugins
    end.

-spec subscription_plugin(host() | atom()) -> atom().
subscription_plugin(Host) ->
    submodule(Host, <<"pubsub">>, <<"subscription">>).

-spec submodule(host() | atom(), binary(), binary()) -> atom().
submodule(Db, Type, Name) when is_atom(Db) ->
    case Db of
	mnesia -> ejabberd:module_name([<<"pubsub">>, Type, Name]);
	_ -> ejabberd:module_name([<<"pubsub">>, Type, Name, misc:atom_to_binary(Db)])
    end;
submodule(Host, Type, Name) ->
    Db = mod_pubsub_opt:db_type(serverhost(Host)),
    submodule(Db, Type, Name).

-spec config(binary(), any()) -> any().
config(ServerHost, Key) ->
    config(ServerHost, Key, undefined).

-spec config(host(), any(), any()) -> any().
config({_User, Host, _Resource}, Key, Default) ->
    config(Host, Key, Default);
config(ServerHost, Key, Default) ->
    case catch ets:lookup(gen_mod:get_module_proc(ServerHost, config), Key) of
	[{Key, Value}] -> Value;
	_ -> Default
    end.

-spec select_type(binary(), host(), binary(), binary()) -> binary().
select_type(ServerHost, {_User, _Server, _Resource}, Node, _Type) ->
    case config(ServerHost, pep_mapping) of
	undefined -> ?PEPNODE;
	Mapping -> proplists:get_value(Node, Mapping, ?PEPNODE)
    end;
select_type(ServerHost, _Host, _Node, Type) ->
    case config(ServerHost, plugins) of
	undefined ->
	    Type;
	Plugins ->
	    case lists:member(Type, Plugins) of
		true -> Type;
		false -> hd(Plugins)
	    end
    end.

-spec select_type(binary(), host(), binary()) -> binary().
select_type(ServerHost, Host, Node) ->
    select_type(ServerHost, Host, Node, hd(plugins(Host))).

-spec feature(binary()) -> binary().
feature(<<"rsm">>) -> ?NS_RSM;
feature(Feature) -> <<(?NS_PUBSUB)/binary, "#", Feature/binary>>.

-spec features() -> [binary()].
features() ->
    [% see plugin "access-authorize",   % OPTIONAL
     <<"access-open">>,   % OPTIONAL this relates to access_model option in node_hometree
     <<"access-presence">>,   % OPTIONAL this relates to access_model option in node_pep
     <<"access-whitelist">>,   % OPTIONAL
     <<"collections">>,   % RECOMMENDED
     <<"config-node">>,   % RECOMMENDED
     <<"config-node-max">>,
     <<"create-and-configure">>,   % RECOMMENDED
     <<"item-ids">>,   % RECOMMENDED
     <<"last-published">>,   % RECOMMENDED
     <<"member-affiliation">>,   % RECOMMENDED
     <<"presence-notifications">>,   % OPTIONAL
     <<"presence-subscribe">>,   % RECOMMENDED
     <<"publisher-affiliation">>,   % RECOMMENDED
     <<"publish-only-affiliation">>,   % OPTIONAL
     <<"publish-options">>,   % OPTIONAL
     <<"retrieve-default">>,
     <<"shim">>].   % RECOMMENDED

% see plugin "retrieve-items",   % RECOMMENDED
% see plugin "retrieve-subscriptions",   % RECOMMENDED
% see plugin "subscribe",   % REQUIRED
% see plugin "subscription-options",   % OPTIONAL
% see plugin "subscription-notifications"   % OPTIONAL
-spec plugin_features(host(), binary()) -> [binary()].
plugin_features(Host, Type) ->
    Module = plugin(Host, Type),
    case catch Module:features() of
	{'EXIT', {undef, _}} -> [];
	Result -> Result
    end.

-spec features(binary(), binary()) -> [binary()].
features(Host, <<>>) ->
    lists:usort(lists:foldl(fun (Plugin, Acc) ->
		    Acc ++ plugin_features(Host, Plugin)
	    end,
	    features(), plugins(Host)));
features(Host, Node) when is_binary(Node) ->
    Action = fun (#pubsub_node{type = Type}) ->
	    {result, plugin_features(Host, Type)}
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, Features} -> lists:usort(features() ++ Features);
	_ -> features()
    end.

%% @doc <p>node tree plugin call.</p>
-spec tree_call(host(), atom(), list()) -> {error, stanza_error() | {virtual, nodeIdx()}} | any().
tree_call({_User, Server, _Resource}, Function, Args) ->
    tree_call(Server, Function, Args);
tree_call(Host, Function, Args) ->
    Tree = tree(Host),
    ?DEBUG("Tree_call apply(~ts, ~ts, ~p) @ ~ts", [Tree, Function, Args, Host]),
    case apply(Tree, Function, Args) of
	{error, #stanza_error{}} = Err ->
	    Err;
	{error, {virtual, _}} = Err ->
	    Err;
	{error, _} ->
	    ErrTxt = ?T("Database failure"),
	    Lang = ejabberd_option:language(),
	    {error, xmpp:err_internal_server_error(ErrTxt, Lang)};
	Other ->
	    Other
    end.

-spec tree_action(host(), atom(), list()) -> {error, stanza_error() | {virtual, nodeIdx()}} | any().
tree_action(Host, Function, Args) ->
    ?DEBUG("Tree_action ~p ~p ~p", [Host, Function, Args]),
    ServerHost = serverhost(Host),
    DBType = mod_pubsub_opt:db_type(ServerHost),
    Fun = fun () ->
		  try tree_call(Host, Function, Args)
		  catch ?EX_RULE(Class, Reason, St) when DBType == sql ->
			  StackTrace = ?EX_STACK(St),
			  ejabberd_sql:abort({exception, Class, Reason, StackTrace})
		  end
	  end,
    Ret = case DBType of
	      mnesia ->
		  mnesia:sync_dirty(Fun);
	      sql ->
		  ejabberd_sql:sql_bloc(ServerHost, Fun);
	      _ ->
		  Fun()
	  end,
    get_tree_action_result(Ret).

-spec get_tree_action_result(any()) -> {error, stanza_error() | {virtual, nodeIdx()}} | any().
get_tree_action_result({atomic, Result}) ->
    Result;
get_tree_action_result({aborted, {exception, Class, Reason, StackTrace}}) ->
    ?ERROR_MSG("Transaction aborted:~n** ~ts",
	       [misc:format_exception(2, Class, Reason, StackTrace)]),
    get_tree_action_result({error, db_failure});
get_tree_action_result({aborted, Reason}) ->
    ?ERROR_MSG("Transaction aborted: ~p~n", [Reason]),
    get_tree_action_result({error, db_failure});
get_tree_action_result({error, #stanza_error{}} = Err) ->
    Err;
get_tree_action_result({error, {virtual, _}} = Err) ->
    Err;
get_tree_action_result({error, _}) ->
    ErrTxt = ?T("Database failure"),
    Lang = ejabberd_option:language(),
    {error, xmpp:err_internal_server_error(ErrTxt, Lang)};
get_tree_action_result(Other) ->
    %% This is very risky, but tree plugins design is really bad
    Other.

%% @doc <p>node plugin call.</p>
-spec node_call(host(), binary(), atom(), list()) -> {result, any()} | {error, stanza_error()}.
node_call(Host, Type, Function, Args) ->
    ?DEBUG("Node_call ~p ~p ~p", [Type, Function, Args]),
    Module = plugin(Host, Type),
    case erlang:function_exported(Module, Function, length(Args)) of
	true ->
	    case apply(Module, Function, Args) of
		{result, Result} ->
		    {result, Result};
		#pubsub_state{} = Result ->
		    {result, Result};
		{error, #stanza_error{}} = Err ->
		    Err;
		{error, _} ->
		    ErrTxt = ?T("Database failure"),
		    Lang = ejabberd_option:language(),
		    {error, xmpp:err_internal_server_error(ErrTxt, Lang)}
	    end;
	false when Type /= ?STDNODE ->
	    node_call(Host, ?STDNODE, Function, Args);
	false ->
	    %% Let it crash with the stacktrace
	    apply(Module, Function, Args)
    end.

-spec node_action(host(), binary(), atom(), list()) -> {result, any()} | {error, stanza_error()}.
node_action(Host, Type, Function, Args) ->
    ?DEBUG("Node_action ~p ~p ~p ~p", [Host, Type, Function, Args]),
    transaction(Host, fun() -> node_call(Host, Type, Function, Args) end, sync_dirty).

%% @doc <p>plugin transaction handling.</p>
-spec transaction(host(), binary(), fun((#pubsub_node{}) -> _), transaction | sync_dirty) ->
			 {result, any()} | {error, stanza_error()}.
transaction(Host, Node, Action, Trans) ->
    transaction(
      Host,
      fun() ->
	      case tree_call(Host, get_node, [Host, Node]) of
		  N when is_record(N, pubsub_node) ->
		      case Action(N) of
			  {result, Result} -> {result, {N, Result}};
			  {atomic, {result, Result}} -> {result, {N, Result}};
			  Other -> Other
		      end;
		  Error ->
		      Error
	      end
      end,
      Trans).

-spec transaction(host(), fun(), transaction | sync_dirty) ->
			 {result, any()} | {error, stanza_error()}.
transaction(Host, Fun, Trans) ->
    ServerHost = serverhost(Host),
    DBType = mod_pubsub_opt:db_type(ServerHost),
    do_transaction(ServerHost, Fun, Trans, DBType).

-spec do_transaction(binary(), fun(), transaction | sync_dirty, atom()) ->
			    {result, any()} | {error, stanza_error()}.
do_transaction(ServerHost, Fun, Trans, DBType) ->
    F = fun() ->
		try Fun()
		catch ?EX_RULE(Class, Reason, St) when (DBType == mnesia andalso
							Trans == transaction) orelse
						       DBType == sql ->
			StackTrace = ?EX_STACK(St),
			Ex = {exception, Class, Reason, StackTrace},
			case DBType of
			    mnesia -> mnesia:abort(Ex);
			    sql -> ejabberd_sql:abort(Ex)
			end
		end
	end,
    Res = case DBType of
	      mnesia ->
		  mnesia:Trans(F);
	      sql ->
		  SqlFun = case Trans of
			       transaction -> sql_transaction;
			       _ -> sql_bloc
			   end,
		  ejabberd_sql:SqlFun(ServerHost, F);
	      _ ->
		  F()
	  end,
    get_transaction_response(Res).

-spec get_transaction_response(any()) -> {result, any()} | {error, stanza_error()}.
get_transaction_response({result, _} = Result) ->
    Result;
get_transaction_response({error, #stanza_error{}} = Err) ->
    Err;
get_transaction_response({atomic, Result}) ->
    get_transaction_response(Result);
get_transaction_response({aborted, Err}) ->
    get_transaction_response(Err);
get_transaction_response({error, _}) ->
    Lang = ejabberd_option:language(),
    {error, xmpp:err_internal_server_error(?T("Database failure"), Lang)};
get_transaction_response({exception, Class, Reason, StackTrace}) ->
    ?ERROR_MSG("Transaction aborted:~n** ~ts",
	       [misc:format_exception(2, Class, Reason, StackTrace)]),
    get_transaction_response({error, db_failure});
get_transaction_response(Err) ->
    ?ERROR_MSG("Transaction error: ~p", [Err]),
    get_transaction_response({error, db_failure}).

%%%% helpers

%% Add pubsub-specific error element
-spec extended_error(stanza_error(), ps_error()) -> stanza_error().
extended_error(StanzaErr, PubSubErr) ->
    StanzaErr#stanza_error{sub_els = [PubSubErr]}.

-spec err_closed_node() -> ps_error().
err_closed_node() ->
    #ps_error{type = 'closed-node'}.

-spec err_configuration_required() -> ps_error().
err_configuration_required() ->
    #ps_error{type = 'configuration-required'}.

-spec err_invalid_jid() -> ps_error().
err_invalid_jid() ->
    #ps_error{type = 'invalid-jid'}.

-spec err_invalid_options() -> ps_error().
err_invalid_options() ->
    #ps_error{type = 'invalid-options'}.

-spec err_invalid_payload() -> ps_error().
err_invalid_payload() ->
    #ps_error{type = 'invalid-payload'}.

-spec err_invalid_subid() -> ps_error().
err_invalid_subid() ->
    #ps_error{type = 'invalid-subid'}.

-spec err_item_forbidden() -> ps_error().
err_item_forbidden() ->
    #ps_error{type = 'item-forbidden'}.

-spec err_item_required() -> ps_error().
err_item_required() ->
    #ps_error{type = 'item-required'}.

-spec err_jid_required() -> ps_error().
err_jid_required() ->
    #ps_error{type = 'jid-required'}.

-spec err_max_items_exceeded() -> ps_error().
err_max_items_exceeded() ->
    #ps_error{type = 'max-items-exceeded'}.

-spec err_max_nodes_exceeded() -> ps_error().
err_max_nodes_exceeded() ->
    #ps_error{type = 'max-nodes-exceeded'}.

-spec err_nodeid_required() -> ps_error().
err_nodeid_required() ->
    #ps_error{type = 'nodeid-required'}.

-spec err_not_in_roster_group() -> ps_error().
err_not_in_roster_group() ->
    #ps_error{type = 'not-in-roster-group'}.

-spec err_not_subscribed() -> ps_error().
err_not_subscribed() ->
    #ps_error{type = 'not-subscribed'}.

-spec err_payload_too_big() -> ps_error().
err_payload_too_big() ->
    #ps_error{type = 'payload-too-big'}.

-spec err_payload_required() -> ps_error().
err_payload_required() ->
    #ps_error{type = 'payload-required'}.

-spec err_pending_subscription() -> ps_error().
err_pending_subscription() ->
    #ps_error{type = 'pending-subscription'}.

-spec err_precondition_not_met() -> ps_error().
err_precondition_not_met() ->
    #ps_error{type = 'precondition-not-met'}.

-spec err_presence_subscription_required() -> ps_error().
err_presence_subscription_required() ->
    #ps_error{type = 'presence-subscription-required'}.

-spec err_subid_required() -> ps_error().
err_subid_required() ->
    #ps_error{type = 'subid-required'}.

-spec err_too_many_subscriptions() -> ps_error().
err_too_many_subscriptions() ->
    #ps_error{type = 'too-many-subscriptions'}.

-spec err_unsupported(ps_feature()) -> ps_error().
err_unsupported(Feature) ->
    #ps_error{type = 'unsupported', feature = Feature}.

-spec err_unsupported_access_model() -> ps_error().
err_unsupported_access_model() ->
    #ps_error{type = 'unsupported-access-model'}.

-spec uniqid() -> mod_pubsub:itemId().
uniqid() ->
    {T1, T2, T3} = erlang:timestamp(),
    (str:format("~.16B~.16B~.16B", [T1, T2, T3])).

-spec add_message_type(message(), message_type()) -> message().
add_message_type(#message{} = Message, Type) ->
    Message#message{type = Type}.

%% Place of <headers/> changed at the bottom of the stanza
%% cf. http://xmpp.org/extensions/xep-0060.html#publisher-publish-success-subid
%%
%% "[SHIM Headers] SHOULD be included after the event notification information
%% (i.e., as the last child of the <message/> stanza)".

-spec add_shim_headers(stanza(), [{binary(), binary()}]) -> stanza().
add_shim_headers(Stanza, Headers) ->
    xmpp:set_subtag(Stanza, #shim{headers = Headers}).

-spec add_extended_headers(stanza(), [address()]) -> stanza().
add_extended_headers(Stanza, Addrs) ->
    xmpp:set_subtag(Stanza, #addresses{list = Addrs}).

-spec subid_shim([binary()]) -> [{binary(), binary()}].
subid_shim(SubIds) ->
    [{<<"SubId">>, SubId} || SubId <- SubIds].

%% The argument is a list of Jids because this function could be used
%% with the 'pubsub#replyto' (type=jid-multi) node configuration.

-spec extended_headers([jid()]) -> [address()].
extended_headers(Jids) ->
    [#address{type = replyto, jid = Jid} || Jid <- Jids].

-spec purge_offline(ljid()) -> ok.
purge_offline(LJID) ->
    Host = host(element(2, LJID)),
    Plugins = plugins(Host),
    Result = lists:foldl(
	       fun(Type, {Status, Acc}) ->
		       Features = plugin_features(Host, Type),
		       case lists:member(<<"retrieve-affiliations">>, plugin_features(Host, Type)) of
			   false ->
			       {{error, extended_error(xmpp:err_feature_not_implemented(),
						       err_unsupported('retrieve-affiliations'))},
				Acc};
			   true ->
			       Items = lists:member(<<"retract-items">>, Features)
				   andalso lists:member(<<"persistent-items">>, Features),
			       if Items ->
				       case node_action(Host, Type,
							get_entity_affiliations, [Host, LJID]) of
					   {result, Affs} ->
					       {Status, [Affs | Acc]};
					   {error, _} = Err ->
					       {Err, Acc}
				       end;
				  true ->
				       {Status, Acc}
			       end
		       end
	       end, {ok, []}, Plugins),
    case Result of
	{ok, Affs} ->
	    lists:foreach(
		    fun ({Node, Affiliation}) ->
			    Options = Node#pubsub_node.options,
			    Publisher = lists:member(Affiliation, [owner,publisher,publish_only]),
			    Open = (get_option(Options, publish_model) == open),
			    Purge = (get_option(Options, purge_offline)
				andalso get_option(Options, persist_items)),
			    if (Publisher or Open) and Purge ->
				purge_offline(Host, LJID, Node);
			    true ->
				ok
			    end
		    end, lists:usort(lists:flatten(Affs)));
	_ ->
	    ok
    end.

-spec purge_offline(host(), ljid(), #pubsub_node{}) -> ok | {error, stanza_error()}.
purge_offline(Host, LJID, Node) ->
    Nidx = Node#pubsub_node.id,
    Type = Node#pubsub_node.type,
    Options = Node#pubsub_node.options,
    case node_action(Host, Type, get_items, [Nidx, service_jid(Host), undefined]) of
	{result, {[], _}} ->
	    ok;
	{result, {Items, _}} ->
	    {User, Server, Resource} = LJID,
	    PublishModel = get_option(Options, publish_model),
	    ForceNotify = get_option(Options, notify_retract),
	    {_, NodeId} = Node#pubsub_node.nodeid,
	    lists:foreach(
	      fun(#pubsub_item{itemid = {ItemId, _}, modification = {_, {U, S, R}}})
		    when (U == User) and (S == Server) and (R == Resource) ->
		      case node_action(Host, Type, delete_item, [Nidx, {U, S, <<>>}, PublishModel, ItemId]) of
			  {result, {_, broadcast}} ->
			      broadcast_retract_items(Host, NodeId, Nidx, Type, Options, [ItemId], ForceNotify),
			      case get_cached_item(Host, Nidx) of
				  #pubsub_item{itemid = {ItemId, Nidx}} -> unset_cached_item(Host, Nidx);
				  _ -> ok
			      end;
			  _ ->
			      ok
		      end;
		 (_) ->
		      true
	      end, Items);
	{error, #stanza_error{}} = Err ->
	    Err;
	_ ->
	    Txt = ?T("Database failure"),
	    Lang = ejabberd_option:language(),
	    {error, xmpp:err_internal_server_error(Txt, Lang)}
    end.

-spec delete_old_items(non_neg_integer()) -> ok | error.
delete_old_items(N) ->
    Results = lists:flatmap(
		fun(Host) ->
			case tree_action(Host, get_all_nodes, [Host]) of
			    Nodes when is_list(Nodes) ->
				lists:map(
				  fun(#pubsub_node{id = Nidx, type = Type}) ->
					  case node_action(Host, Type,
							   remove_extra_items,
							   [Nidx , N]) of
					      {result, _} ->
						  ok;
					      {error, _} ->
						  error
					  end
				  end, Nodes);
			    _ ->
				error
			end
		end, ejabberd_option:hosts()),
    case lists:member(error, Results) of
	true ->
	    error;
	false ->
	    ok
    end.

-spec delete_expired_items() -> ok | error.
delete_expired_items() ->
    Results = lists:flatmap(
		fun(Host) ->
			case tree_action(Host, get_all_nodes, [Host]) of
			    Nodes when is_list(Nodes) ->
				lists:map(
				  fun(#pubsub_node{id = Nidx, type = Type,
						   options = Options}) ->
					  case item_expire(Host, Options) of
					      infinity ->
						  ok;
					      Seconds ->
						  case node_action(
							 Host, Type,
							 remove_expired_items,
							 [Nidx, Seconds]) of
						      {result, []} ->
							  ok;
						      {result, [_|_]} ->
							  unset_cached_item(
							    Host, Nidx);
						      {error, _} ->
							  error
						  end
					  end
				  end, Nodes);
			    _ ->
				error
			end
		end, ejabberd_option:hosts()),
    case lists:member(error, Results) of
	true ->
	    error;
	false ->
	    ok
    end.

-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = delete_old_pubsub_items, tags = [purge],
			desc = "Keep only NUMBER of PubSub items per node",
		        note = "added in 21.12",
			module = ?MODULE, function = delete_old_items,
			args_desc = ["Number of items to keep per node"],
			args = [{number, integer}],
			result = {res, rescode},
			result_desc = "0 if command failed, 1 when succeeded",
			args_example = [1000],
			result_example = ok},
     #ejabberd_commands{name = delete_expired_pubsub_items, tags = [purge],
			desc = "Delete expired PubSub items",
		        note = "added in 21.12",
			module = ?MODULE, function = delete_expired_items,
			args = [],
			result = {res, rescode},
			result_desc = "0 if command failed, 1 when succeeded",
			result_example = ok}].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(access_createnode) ->
    econf:acl();
mod_opt_type(name) ->
    econf:binary();
mod_opt_type(ignore_pep_from_offline) ->
    econf:bool();
mod_opt_type(last_item_cache) ->
    econf:bool();
mod_opt_type(max_items_node) ->
    econf:non_neg_int(unlimited);
mod_opt_type(max_item_expire_node) ->
    econf:timeout(second, infinity);
mod_opt_type(max_nodes_discoitems) ->
    econf:non_neg_int(infinity);
mod_opt_type(max_subscriptions_node) ->
    econf:non_neg_int();
mod_opt_type(force_node_config) ->
    econf:map(
      econf:glob(),
      econf:map(
	econf:atom(),
	econf:either(
	  econf:int(),
	  econf:atom()),
	[{return, orddict}, unique]));
mod_opt_type(default_node_config) ->
    econf:map(
      econf:atom(),
      econf:either(
	econf:int(),
	econf:atom()),
      [unique]);
mod_opt_type(nodetree) ->
    econf:binary();
mod_opt_type(pep_mapping) ->
    econf:map(econf:binary(), econf:binary());
mod_opt_type(plugins) ->
    econf:list(
      econf:enum([<<"flat">>, <<"pep">>]),
      [unique]);
mod_opt_type(host) ->
    econf:host();
mod_opt_type(hosts) ->
    econf:hosts();
mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(vcard) ->
    econf:vcard_temp().

mod_options(Host) ->
    [{access_createnode, all},
     {db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {host, <<"pubsub.", Host/binary>>},
     {hosts, []},
     {name, ?T("Publish-Subscribe")},
     {vcard, undefined},
     {ignore_pep_from_offline, true},
     {last_item_cache, false},
     {max_items_node, ?MAXITEMS},
     {max_item_expire_node, infinity},
     {max_nodes_discoitems, 100},
     {nodetree, ?STDTREE},
     {pep_mapping, []},
     {plugins, [?STDNODE]},
     {max_subscriptions_node, undefined},
     {default_node_config, []},
     {force_node_config, []}].

mod_doc() ->
    #{desc =>
	  [?T("This module offers a service for "
	      "https://xmpp.org/extensions/xep-0060.html"
	      "[XEP-0060: Publish-Subscribe]. The functionality in "
	      "'mod_pubsub' can be extended using plugins. "
	      "The plugin that implements PEP "
	      "(https://xmpp.org/extensions/xep-0163.html"
	      "[XEP-0163: Personal Eventing via Pubsub]) "
	      "is enabled in the default ejabberd configuration file, "
	      "and it requires _`mod_caps`_.")],
      opts =>
	  [{access_createnode,
	    #{value => "AccessName",
	      desc =>
		  ?T("This option restricts which users are allowed to "
		     "create pubsub nodes using 'acl' and 'access'. "
		     "By default any account in the local ejabberd server "
		     "is allowed to create pubsub nodes. "
		     "The default value is: 'all'.")}},
	   {db_type,
	    #{value => "mnesia | sql",
	      desc =>
		  ?T("Same as top-level _`default_db`_ option, but applied to "
		     "this module only.")}},
	   {default_node_config,
	    #{value => "List of Key:Value",
	      desc =>
		  ?T("To override default node configuration, regardless "
		     "of node plugin. Value is a list of key-value "
		     "definition. Node configuration still uses default "
		     "configuration defined by node plugin, and overrides "
		     "any items by value defined in this configurable list.")}},
	   {force_node_config,
	    #{value => "List of Node and the list of its Key:Value",
	      desc =>
		  ?T("Define the configuration for given nodes. "
		     "The default value is: '[]'."),
	      example =>
		  ["force_node_config:",
		   "  ## Avoid buggy clients to make their bookmarks public",
		   "  storage:bookmarks:",
		   "    access_model: whitelist"]}},
	   {host,
	    #{desc => ?T("Deprecated. Use 'hosts' instead.")}},
	   {hosts,
	    #{value => ?T("[Host, ...]"),
	      desc =>
		  ?T("This option defines the Jabber IDs of the service. "
		     "If the 'hosts' option is not specified, the only Jabber "
		     "ID will be the hostname of the virtual host with the "
		     "prefix \"pubsub.\". The keyword '@HOST@' is replaced "
		     "with the real virtual host name.")}},
	   {ignore_pep_from_offline,
	    #{value => "false | true",
	      desc =>
		  ?T("To specify whether or not we should get last "
		     "published PEP items from users in our roster which "
		     "are offline when we connect. Value is 'true' or "
		     "'false'. If not defined, pubsub assumes true so we "
		     "only get last items of online contacts.")}},
	   {last_item_cache,
	    #{value => "false | true",
	      desc =>
		  ?T("To specify whether or not pubsub should cache last "
		     "items. Value is 'true' or 'false'. If not defined, "
		     "pubsub does not cache last items. On systems with not"
		     " so many nodes, caching last items speeds up pubsub "
		     "and allows to raise user connection rate. The cost "
		     "is memory usage, as every item is stored in memory.")}},
	   {max_item_expire_node,
	    #{value => "timeout() | infinity",
	      note => "added in 21.12",
	      desc =>
		  ?T("Specify the maximum item epiry time. Default value "
		     "is: 'infinity'.")}},
	   {max_items_node,
	    #{value => "non_neg_integer() | infinity",
	      desc =>
		  ?T("Define the maximum number of items that can be "
		     "stored in a node. Default value is: '1000'.")}},
	   {max_nodes_discoitems,
	    #{value => "pos_integer() | infinity",
	      desc =>
		  ?T("The maximum number of nodes to return in a "
		     "discoitem response. The default value is: '100'.")}},
	   {max_subscriptions_node,
	    #{value => "MaxSubs",
	      desc =>
		  ?T("Define the maximum number of subscriptions managed "
		     "by a node. "
		     "Default value is no limitation: 'undefined'.")}},
	   {name,
	    #{value => ?T("Name"),
	      desc =>
		  ?T("The value of the service name. This name is only visible "
		     "in some clients that support "
		     "https://xmpp.org/extensions/xep-0030.html"
		     "[XEP-0030: Service Discovery]. "
		     "The default is 'vCard User Search'.")}},
	   {nodetree,
	    #{value => "Nodetree",
	      desc =>
		  [?T("To specify which nodetree to use. If not defined, the "
		      "default pubsub nodetree is used: 'tree'. Only one "
		      "nodetree can be used per host, and is shared by all "
		      "node plugins."),
		   ?T("- 'tree' nodetree store node configuration and "
		      "relations on the database. 'flat' nodes are stored "
		      "without any relationship, and 'hometree' nodes can "
		      "have child nodes."),
		   ?T("- 'virtual' nodetree does not store nodes on database. "
		      "This saves resources on systems with tons of nodes. "
		      "If using the 'virtual' nodetree, you can only enable "
		      "those node plugins: '[flat, pep]' or '[flat]'; any "
		      "other plugins configuration will not work. Also, all "
		      "nodes will have the default configuration, and this "
		      "can not be changed. Using 'virtual' nodetree requires "
		      "to start from a clean database, it will not work if "
		      "you used the default 'tree' nodetree before.")]}},
	   {pep_mapping,
	    #{value => "List of Key:Value",
	      desc =>
		  ?T("This allows to define a list of key-value to choose "
		     "defined node plugins on given PEP namespace. "
		     "The following example will use 'node_tune' instead of "
		     "'node_pep' for every PEP node with the tune namespace:"),
	      example =>
		    ["modules:",
		     "  ...",
		     "  mod_pubsub:",
		     "    pep_mapping:",
		     "      http://jabber.org/protocol/tune: tune",
		     "  ..."]
		   }},
	   {plugins,
	    #{value => "[Plugin, ...]",
	      desc => [?T("To specify which pubsub node plugins to use. "
			  "The first one in the list is used by default. "
			  "If this option is not defined, the default plugins "
			  "list is: '[flat]'. PubSub clients can define which "
			  "plugin to use when creating a node: "
			  "add 'type=\'plugin-name\'' attribute "
			  "to the 'create' stanza element."),
		       ?T("- 'flat' plugin handles the default behaviour and "
			  "follows standard XEP-0060 implementation."),
		       ?T("- 'pep' plugin adds extention to handle Personal "
			  "Eventing Protocol (XEP-0163) to the PubSub engine. "
			  "Adding pep allows to handle PEP automatically.")]}},
	   {vcard,
	    #{value => ?T("vCard"),
	      desc =>
		  ?T("A custom vCard of the server that will be displayed by "
		     "some XMPP clients in Service Discovery. The value of "
		     "'vCard' is a YAML map constructed from an XML "
		     "representation of vCard. Since the representation has "
		     "no attributes, the mapping is straightforward."),
	      example =>
		  [{?T("The following XML representation of vCard:"),
		    ["<vCard xmlns='vcard-temp'>",
		     "  <FN>PubSub Service</FN>",
		     "  <ADR>",
		     "    <WORK/>",
		     "    <STREET>Elm Street</STREET>",
		     "  </ADR>",
		     "</vCard>"]},
		   {?T("will be translated to:"),
		    ["vcard:",
		     "  fn: PubSub Service",
		     "  adr:",
		     "    -",
		     "      work: true",
		     "      street: Elm Street"]}]}}
	  ],
      example =>
	  [{?T("Example of configuration that uses flat nodes as default, "
	       "and allows use of flat, hometree and pep nodes:"),
	    ["modules:",
	     "  ...",
	     "  mod_pubsub:",
	     "    access_createnode: pubsub_createnode",
	     "    max_subscriptions_node: 100",
	     "    default_node_config:",
	     "      notification_type: normal",
	     "      notify_retract: false",
	     "      max_items: 4",
	     "    plugins:",
	     "      - flat",
	     "      - pep",
	     "  ..."]},
	   {?T("Using relational database requires using mod_pubsub with "
	       "db_type 'sql'. Only flat, hometree and pep plugins supports "
	       "SQL. The following example shows previous configuration "
	       "with SQL usage:"),
	    ["modules:",
	     "  ...",
	     "  mod_pubsub:",
	     "    db_type: sql",
	     "    access_createnode: pubsub_createnode",
	     "    ignore_pep_from_offline: true",
	     "    last_item_cache: false",
	     "    plugins:",
	     "      - flat",
	     "      - pep",
	     "  ..."]}
	  ]}.
