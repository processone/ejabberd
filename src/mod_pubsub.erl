%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the core of the PubSub
%%% extension. It relies on PubSub plugins for a large part of its functions.
%%%
%%% @headerfile "pubsub.hrl"
%%%
%%% @reference See <a href="http://www.xmpp.org/extensions/xep-0060.html">XEP-0060: Pubsub</a> for
%%% the latest version of the PubSub specification.
%%% This module uses version 1.12 of the specification as a base.
%%% Most of the specification is implemented.
%%% Functions concerning configuration should be rewritten.
%%%
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
-protocol({xep, 60, '1.13-1'}).
-protocol({xep, 163, '1.2'}).
-protocol({xep, 248, '0.2'}).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("adhoc.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").

-define(STDTREE, <<"tree">>).
-define(STDNODE, <<"flat">>).
-define(PEPNODE, <<"pep">>).

%% exports for hooks
-export([presence_probe/3, caps_add/3, caps_update/3,
    in_subscription/6, out_subscription/4,
    on_user_offline/3, remove_user/2,
    disco_local_identity/5, disco_local_features/5,
    disco_local_items/5, disco_sm_identity/5,
    disco_sm_features/5, disco_sm_items/5]).

%% exported iq handlers
-export([iq_sm/3]).

%% exports for console debug manual use
-export([create_node/5, create_node/7, delete_node/3,
    subscribe_node/5, unsubscribe_node/5, publish_item/6,
    delete_item/4, send_items/7, get_items/2, get_item/3,
    get_cached_item/2, get_configure/5, set_configure/5,
    tree_action/3, node_action/4, node_call/4]).

%% general helpers for plugins
-export([subscription_to_string/1, affiliation_to_string/1,
    string_to_subscription/1, string_to_affiliation/1,
    extended_error/2, extended_error/3, service_jid/1,
    tree/1, tree/2, plugin/2, plugins/1, config/3,
    host/1, serverhost/1]).

%% API and gen_server callbacks
-export([start_link/2, start/2, stop/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([send_loop/1, mod_opt_type/1]).

-define(PROCNAME, ejabberd_mod_pubsub).
-define(LOOPNAME, ejabberd_mod_pubsub_loop).

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
	items         :: [ItemId::mod_pubsub:itemId()],
	affiliation   :: Affs::mod_pubsub:affiliation(),
	subscriptions :: [{Sub::mod_pubsub:subscription(), SubId::mod_pubsub:subId()}]
	}
    ).

-type(pubsubItem() ::
    #pubsub_item{
	itemid       :: {ItemId::mod_pubsub:itemId(), Nidx::mod_pubsub:nodeIdx()},
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
	nodeid   :: mod_pubsub:nodeIdx(),
	itemid   :: mod_pubsub:itemId(),
	creation :: {erlang:timestamp(), ljid()},
	payload  :: mod_pubsub:payload()
	}
    ).

-record(state,
    {
	server_host,
	host,
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
	host                    :: mod_pubsub:hostPubsub(),
	access                  :: atom(),
	pep_mapping             :: [{binary(), binary()}],
	ignore_pep_from_offline :: boolean(),
	last_item_cache         :: boolean(),
	max_items_node          :: non_neg_integer(),
	max_subscriptions_node  :: non_neg_integer()|undefined,
	default_node_config     :: [{atom(), binary()|boolean()|integer()|atom()}],
	nodetree                :: binary(),
	plugins                 :: [binary(),...],
	db_type                 :: atom()
	}

    ).


start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
	    transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).

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
-spec(init/1 ::
    (
	[binary() | [{_,_}],...])
    -> {'ok',state()}
    ).

init([ServerHost, Opts]) ->
    ?DEBUG("pubsub init ~p ~p", [ServerHost, Opts]),
    Host = gen_mod:get_opt_host(ServerHost, Opts, <<"pubsub.@HOST@">>),
    Access = gen_mod:get_opt(access_createnode, Opts,
	    fun(A) when is_atom(A) -> A end, all),
    PepOffline = gen_mod:get_opt(ignore_pep_from_offline, Opts,
	    fun(A) when is_boolean(A) -> A end, true),
    IQDisc = gen_mod:get_opt(iqdisc, Opts,
	    fun gen_iq_handler:check_type/1, one_queue),
    LastItemCache = gen_mod:get_opt(last_item_cache, Opts,
	    fun(A) when is_boolean(A) -> A end, false),
    MaxItemsNode = gen_mod:get_opt(max_items_node, Opts,
	    fun(A) when is_integer(A) andalso A >= 0 -> A end, ?MAXITEMS),
    MaxSubsNode = gen_mod:get_opt(max_subscriptions_node, Opts,
	    fun(A) when is_integer(A) andalso A >= 0 -> A end, undefined),
    DefaultNodeCfg = gen_mod:get_opt(default_node_config, Opts,
	    fun(A) when is_list(A) -> filter_node_options(A) end, []),
    pubsub_index:init(Host, ServerHost, Opts),
    ets:new(gen_mod:get_module_proc(ServerHost, config), [set, named_table]),
    {Plugins, NodeTree, PepMapping} = init_plugins(Host, ServerHost, Opts),
    mnesia:create_table(pubsub_last_item,
	[{ram_copies, [node()]},
	    {attributes, record_info(fields, pubsub_last_item)}]),
    mod_disco:register_feature(ServerHost, ?NS_PUBSUB),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {nodetree, NodeTree}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {plugins, Plugins}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {last_item_cache, LastItemCache}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {max_items_node, MaxItemsNode}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {max_subscriptions_node, MaxSubsNode}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {default_node_config, DefaultNodeCfg}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {pep_mapping, PepMapping}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {ignore_pep_from_offline, PepOffline}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {host, Host}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {access, Access}),
    ejabberd_hooks:add(sm_remove_connection_hook, ServerHost,
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
    ejabberd_hooks:add(anonymous_purge_hook, ServerHost,
	?MODULE, remove_user, 50),
    case lists:member(?PEPNODE, Plugins) of
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
		?NS_PUBSUB, ?MODULE, iq_sm, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHost,
		?NS_PUBSUB_OWNER, ?MODULE, iq_sm, IQDisc);
	false ->
	    ok
    end,
    ejabberd_router:register_route(Host),
    pubsub_migrate:update_node_database(Host, ServerHost),
    pubsub_migrate:update_state_database(Host, ServerHost),
    pubsub_migrate:update_lastitem_database(Host, ServerHost),
    {_, State} = init_send_loop(ServerHost),
    {ok, State}.

init_send_loop(ServerHost) ->
    NodeTree = config(ServerHost, nodetree),
    Plugins = config(ServerHost, plugins),
    LastItemCache = config(ServerHost, last_item_cache),
    MaxItemsNode = config(ServerHost, max_items_node),
    PepMapping = config(ServerHost, pep_mapping),
    PepOffline = config(ServerHost, ignore_pep_from_offline),
    Host = config(ServerHost, host),
    Access = config(ServerHost, access),
    DBType = gen_mod:db_type(ServerHost, ?MODULE),
    State = #state{host = Host, server_host = ServerHost,
	    access = Access, pep_mapping = PepMapping,
	    ignore_pep_from_offline = PepOffline,
	    last_item_cache = LastItemCache,
	    max_items_node = MaxItemsNode, nodetree = NodeTree,
	    plugins = Plugins, db_type = DBType},
    Proc = gen_mod:get_module_proc(ServerHost, ?LOOPNAME),
    Pid = case whereis(Proc) of
	undefined ->
	    SendLoop = spawn(?MODULE, send_loop, [State]),
	    register(Proc, SendLoop),
	    SendLoop;
	Loop ->
	    Loop
    end,
    {Pid, State}.

%% @doc Call the init/1 function for each plugin declared in the config file.
%% The default plugin module is implicit.
%% <p>The Erlang code for the plugin is located in a module called
%% <em>node_plugin</em>. The 'node_' prefix is mandatory.</p>
%% <p>The modules are initialized in alphetical order and the list is checked
%% and sorted to ensure that each module is initialized only once.</p>
%% <p>See {@link node_hometree:init/1} for an example implementation.</p>
init_plugins(Host, ServerHost, Opts) ->
    TreePlugin = tree(Host, gen_mod:get_opt(nodetree, Opts,
		fun(A) when is_binary(A) -> A end,
		?STDTREE)),
    ?DEBUG("** tree plugin is ~p", [TreePlugin]),
    TreePlugin:init(Host, ServerHost, Opts),
    Plugins = gen_mod:get_opt(plugins, Opts,
	    fun(A) when is_list(A) -> A end, [?STDNODE]),
    PepMapping = gen_mod:get_opt(pep_mapping, Opts,
	    fun(A) when is_list(A) -> A end, []),
    ?DEBUG("** PEP Mapping : ~p~n", [PepMapping]),
    PluginsOK = lists:foldl(
	    fun (Name, Acc) ->
		    Plugin = plugin(Host, Name),
		    case catch apply(Plugin, init, [Host, ServerHost, Opts]) of
			{'EXIT', _Error} ->
			    Acc;
			_ ->
			    ?DEBUG("** init ~s plugin", [Name]),
			    [Name | Acc]
		    end
	    end,
	    [], Plugins),
    {lists:reverse(PluginsOK), TreePlugin, PepMapping}.

terminate_plugins(Host, ServerHost, Plugins, TreePlugin) ->
    lists:foreach(
	fun (Name) ->
		?DEBUG("** terminate ~s plugin", [Name]),
		Plugin = plugin(Host, Name),
		Plugin:terminate(Host, ServerHost)
	end,
	Plugins),
    TreePlugin:terminate(Host, ServerHost),
    ok.

send_loop(State) ->
    receive
	{presence, JID, Pid} ->
	    Host = State#state.host,
	    ServerHost = State#state.server_host,
	    DBType = State#state.db_type,
	    LJID = jid:tolower(JID),
	    BJID = jid:remove_resource(LJID),
	    lists:foreach(
		fun(PType) ->
			Subs = get_subscriptions_for_send_last(Host, PType, DBType, JID, LJID, BJID),
			lists:foreach(
			    fun({NodeRec, _, _, SubJID}) ->
				    {_, Node} = NodeRec#pubsub_node.nodeid,
				    Nidx = NodeRec#pubsub_node.id,
				    Options = NodeRec#pubsub_node.options,
				    send_items(Host, Node, Nidx, PType, Options, SubJID, last)
			    end,
			    lists:usort(Subs))
		end,
		State#state.plugins),
	    if not State#state.ignore_pep_from_offline ->
		    {User, Server, Resource} = LJID,
		    case catch ejabberd_c2s:get_subscribed(Pid) of
			Contacts when is_list(Contacts) ->
			    lists:foreach(
				fun({U, S, R}) when S == ServerHost ->
					case user_resources(U, S) of
					    [] -> %% offline
						PeerJID = jid:make(U, S, R),
						self() !  {presence, User, Server, [Resource], PeerJID};
					    _ -> %% online
						% this is already handled by presence probe
						ok
					end;
				    (_) ->
					% we can not do anything in any cases
					ok
				end,
				Contacts);
			_ ->
			    ok
		    end;
		true ->
		    ok
	    end,
	    send_loop(State);
	{presence, User, Server, Resources, JID} ->
	    spawn(fun() ->
			Host = State#state.host,
			Owner = jid:remove_resource(jid:tolower(JID)),
			lists:foreach(fun(#pubsub_node{nodeid = {_, Node}, type = Type, id = Nidx, options = Options}) ->
				    case match_option(Options, send_last_published_item, on_sub_and_presence) of
					true ->
					    lists:foreach(fun(Resource) ->
							LJID = {User, Server, Resource},
							Subscribed = case get_option(Options, access_model) of
							    open -> true;
							    presence -> true;
							    whitelist -> false; % subscribers are added manually
							    authorize -> false; % likewise
							    roster ->
								Grps = get_option(Options, roster_groups_allowed, []),
								{OU, OS, _} = Owner,
								element(2, get_roster_info(OU, OS, LJID, Grps))
							end,
							if Subscribed -> send_items(Owner, Node, Nidx, Type, Options, LJID, last);
							    true -> ok
							end
						end,
						Resources);
					_ ->
					    ok
				    end
			    end,
			    tree_action(Host, get_nodes, [Owner, JID]))
		end),
	    send_loop(State);
	stop ->
	    ok
    end.

%% -------
%% disco hooks handling functions
%%

-spec(disco_local_identity/5 ::
    (
	Acc    :: [xmlel()],
	_From  :: jid(),
	To     :: jid(),
	Node   :: <<>> | mod_pubsub:nodeId(),
	Lang   :: binary())
    -> [xmlel()]
    ).
disco_local_identity(Acc, _From, To, <<>>, _Lang) ->
    case lists:member(?PEPNODE, plugins(To#jid.lserver)) of
	true ->
	    [#xmlel{name = <<"identity">>,
		    attrs = [{<<"category">>, <<"pubsub">>},
			{<<"type">>, <<"pep">>}]}
		| Acc];
	false ->
	    Acc
    end;
disco_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec(disco_local_features/5 ::
    (
	Acc    :: [xmlel()],
	_From  :: jid(),
	To     :: jid(),
	Node   :: <<>> | mod_pubsub:nodeId(),
	Lang   :: binary())
    -> [binary(),...]
    ).
disco_local_features(Acc, _From, To, <<>>, _Lang) ->
    Host = To#jid.lserver,
    Feats = case Acc of
	{result, I} -> I;
	_ -> []
    end,
    {result, Feats ++ [feature(F) || F <- features(Host, <<>>)]};
disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_local_items(Acc, _From, _To, <<>>, _Lang) -> Acc;
disco_local_items(Acc, _From, _To, _Node, _Lang) -> Acc.

%disco_sm_identity(Acc, From, To, Node, Lang)
%    when is_binary(Node) ->
%    disco_sm_identity(Acc, From, To, iolist_to_binary(Node),
%                      Lang);
-spec(disco_sm_identity/5 ::
    (
	Acc  :: empty | [xmlel()],
	From :: jid(),
	To   :: jid(),
	Node :: mod_pubsub:nodeId(),
	Lang :: binary())
    -> [xmlel()]
    ).
disco_sm_identity(empty, From, To, Node, Lang) ->
    disco_sm_identity([], From, To, Node, Lang);
disco_sm_identity(Acc, From, To, Node, _Lang) ->
    disco_identity(jid:tolower(jid:remove_resource(To)), Node, From)
    ++ Acc.

disco_identity(_Host, <<>>, _From) ->
    [#xmlel{name = <<"identity">>,
	    attrs = [{<<"category">>, <<"pubsub">>},
		{<<"type">>, <<"pep">>}]}];
disco_identity(Host, Node, From) ->
    Action = fun (#pubsub_node{id = Nidx, type = Type, options = Options, owners = O}) ->
	    Owners = node_owners_call(Host, Type, Nidx, O),
	    case get_allowed_items_call(Host, Nidx, From, Type, Options, Owners) of
		{result, _} ->
		    {result, [#xmlel{name = <<"identity">>,
				attrs = [{<<"category">>, <<"pubsub">>},
				    {<<"type">>, <<"pep">>}]},
			    #xmlel{name = <<"identity">>,
				attrs = [{<<"category">>, <<"pubsub">>},
				    {<<"type">>, <<"leaf">>}
				    | case get_option(Options, title) of
					false -> [];
					[Title] -> [{<<"name">>, Title}]
				    end]}]};
		_ ->
		    {result, []}
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

-spec(disco_sm_features/5 ::
    (
	Acc  :: empty | {result, Features::[Feature::binary()]},
	From :: jid(),
	To   :: jid(),
	Node :: mod_pubsub:nodeId(),
	Lang :: binary())
    -> {result, Features::[Feature::binary()]}
    ).
%disco_sm_features(Acc, From, To, Node, Lang)
%    when is_binary(Node) ->
%    disco_sm_features(Acc, From, To, iolist_to_binary(Node),
%                      Lang);
disco_sm_features(empty, From, To, Node, Lang) ->
    disco_sm_features({result, []}, From, To, Node, Lang);
disco_sm_features({result, OtherFeatures} = _Acc, From, To, Node, _Lang) ->
    {result,
	OtherFeatures ++
	disco_features(jid:tolower(jid:remove_resource(To)), Node, From)};
disco_sm_features(Acc, _From, _To, _Node, _Lang) -> Acc.

disco_features(Host, <<>>, _From) ->
    [?NS_PUBSUB | [feature(F) || F <- plugin_features(Host, <<"pep">>)]];
disco_features(Host, Node, From) ->
    Action = fun (#pubsub_node{id = Nidx, type = Type, options = Options, owners = O}) ->
	    Owners = node_owners_call(Host, Type, Nidx, O),
	    case get_allowed_items_call(Host, Nidx, From, Type, Options, Owners) of
		{result, _} -> {result, [?NS_PUBSUB | [feature(F) || F <- plugin_features(Host, <<"pep">>)]]};
		_ -> {result, []}
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

-spec(disco_sm_items/5 ::
    (
	Acc  :: empty | {result, [xmlel()]},
	From :: jid(),
	To   :: jid(),
	Node :: mod_pubsub:nodeId(),
	Lang :: binary())
    -> {result, [xmlel()]}
    ).
%disco_sm_items(Acc, From, To, Node, Lang)
%    when is_binary(Node) ->
%    disco_sm_items(Acc, From, To, iolist_to_binary(Node),
%                   Lang);
disco_sm_items(empty, From, To, Node, Lang) ->
    disco_sm_items({result, []}, From, To, Node, Lang);
disco_sm_items({result, OtherItems}, From, To, Node, _Lang) ->
    {result, lists:usort(OtherItems ++
	    disco_items(jid:tolower(jid:remove_resource(To)), Node, From))};
disco_sm_items(Acc, _From, _To, _Node, _Lang) -> Acc.

-spec(disco_items/3 ::
    (
	Host :: mod_pubsub:host(),
	Node :: mod_pubsub:nodeId(),
	From :: jid())
    -> [xmlel()]
    ).
disco_items(Host, <<>>, From) ->
    Action = fun (#pubsub_node{nodeid = {_, Node},
			options = Options, type = Type, id = Nidx, owners = O},
		    Acc) ->
	    Owners = node_owners_call(Host, Type, Nidx, O),
	    case get_allowed_items_call(Host, Nidx, From, Type, Options, Owners) of
		{result, _} ->
		    [#xmlel{name = <<"item">>,
			    attrs = [{<<"node">>, (Node)},
				{<<"jid">>, jid:to_string(Host)}
				| case get_option(Options, title) of
				    false -> [];
				    [Title] -> [{<<"name">>, Title}]
				end]}
			| Acc];
		_ ->
		    Acc
	    end
    end,
    NodeBloc = fun() ->
	    {result,
		lists:foldl(Action, [], tree_call(Host, get_nodes, [Host]))}
    end,
    case transaction(Host, NodeBloc, sync_dirty) of
	{result, Items} -> Items;
	_ -> []
    end;
disco_items(Host, Node, From) ->
    Action = fun (#pubsub_node{id = Nidx, type = Type, options = Options, owners = O}) ->
	    Owners = node_owners_call(Host, Type, Nidx, O),
	    case get_allowed_items_call(Host, Nidx, From, Type, Options, Owners) of
		{result, Items} ->
		    {result, [#xmlel{name = <<"item">>,
				attrs = [{<<"jid">>, jid:to_string(Host)},
				    {<<"name">>, ItemId}]}
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
%% presence hooks handling functions
%%

caps_add(#jid{luser = U, lserver = S, lresource = R}, #jid{lserver = Host} = JID, _Features)
	when Host =/= S ->
    %% When a remote contact goes online while the local user is offline, the
    %% remote contact won't receive last items from the local user even if
    %% ignore_pep_from_offline is set to false. To work around this issue a bit,
    %% we'll also send the last items to remote contacts when the local user
    %% connects. That's the reason to use the caps_add hook instead of the
    %% presence_probe_hook for remote contacts: The latter is only called when a
    %% contact becomes available; the former is also executed when the local
    %% user goes online (because that triggers the contact to send a presence
    %% packet with CAPS).
    presence(Host, {presence, U, S, [R], JID});
caps_add(_From, _To, _Feature) ->
    ok.

caps_update(#jid{luser = U, lserver = S, lresource = R}, #jid{lserver = Host} = JID, _Features) ->
    presence(Host, {presence, U, S, [R], JID}).

presence_probe(#jid{luser = U, lserver = S, lresource = R} = JID, JID, Pid) ->
    presence(S, {presence, JID, Pid}),
    presence(S, {presence, U, S, [R], JID});
presence_probe(#jid{luser = U, lserver = S}, #jid{luser = U, lserver = S}, _Pid) ->
    %% ignore presence_probe from my other ressources
    %% to not get duplicated last items
    ok;
presence_probe(#jid{luser = U, lserver = S, lresource = R}, #jid{lserver = S} = JID, _Pid) ->
    presence(S, {presence, U, S, [R], JID});
presence_probe(_Host, _JID, _Pid) ->
    %% ignore presence_probe from remote contacts,
    %% those are handled via caps_add
    ok.

presence(ServerHost, Presence) ->
    {SendLoop, _} = case whereis(gen_mod:get_module_proc(ServerHost, ?LOOPNAME)) of
	undefined -> init_send_loop(ServerHost);
	Pid -> {Pid, undefined}
    end,
    SendLoop ! Presence.

%% -------
%% subscription hooks handling functions
%%

out_subscription(User, Server, JID, subscribed) ->
    Owner = jid:make(User, Server, <<>>),
    {PUser, PServer, PResource} = jid:tolower(JID),
    PResources = case PResource of
	<<>> -> user_resources(PUser, PServer);
	_ -> [PResource]
    end,
    presence(Server, {presence, PUser, PServer, PResources, Owner}),
    true;
out_subscription(_, _, _, _) ->
    true.

in_subscription(_, User, Server, Owner, unsubscribed, _) ->
    unsubscribe_user(jid:make(User, Server, <<>>), Owner),
    true;
in_subscription(_, _, _, _, _, _) ->
    true.

unsubscribe_user(Entity, Owner) ->
    spawn(fun () ->
	    [unsubscribe_user(ServerHost, Entity, Owner) ||
		ServerHost <- lists:usort(lists:foldl(
			fun(UserHost, Acc) ->
				case gen_mod:is_loaded(UserHost, mod_pubsub) of
				    true -> [UserHost|Acc];
				    false -> Acc
				end
			end, [], [Entity#jid.lserver, Owner#jid.lserver]))]
	end).
unsubscribe_user(Host, Entity, Owner) ->
    BJID = jid:tolower(jid:remove_resource(Owner)),
    lists:foreach(fun (PType) ->
		{result, Subs} = node_action(Host, PType,
			get_entity_subscriptions,
			[Host, Entity]),
		lists:foreach(fun
			({#pubsub_node{options = Options,
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
		    end,
		    Subs)
	end,
	plugins(Host)).

%% -------
%% user remove hook handling function
%%

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Entity = jid:make(LUser, LServer, <<>>),
    Host = host(LServer),
    HomeTreeBase = <<"/home/", LServer/binary, "/", LUser/binary>>,
    spawn(fun () ->
		lists:foreach(fun (PType) ->
			    {result, Subs} = node_action(Host, PType,
				    get_entity_subscriptions,
				    [Host, Entity]),
			    lists:foreach(fun
				    ({#pubsub_node{id = Nidx}, _, _, JID}) ->
					node_action(Host, PType,
					    unsubscribe_node,
					    [Nidx, Entity, JID, all]);
				    (_) ->
					ok
				end,
				Subs),
			    {result, Affs} = node_action(Host, PType,
				    get_entity_affiliations,
				    [Host, Entity]),
			    lists:foreach(fun
				    ({#pubsub_node{nodeid = {H, N}, parents = []}, owner}) ->
					delete_node(H, N, Entity);
				    ({#pubsub_node{nodeid = {H, N}, type = Type}, owner})
					    when N == HomeTreeBase, Type == <<"hometree">> ->
					delete_node(H, N, Entity);
				    ({#pubsub_node{id = Nidx}, publisher}) ->
					node_action(Host, PType,
					    set_affiliation,
					    [Nidx, Entity, none]);
				    (_) ->
					ok
				end,
				Affs)
		    end,
		    plugins(Host))
	end).

handle_call(server_host, _From, State) ->
    {reply, State#state.server_host, State};
handle_call(plugins, _From, State) ->
    {reply, State#state.plugins, State};
handle_call(pep_mapping, _From, State) ->
    {reply, State#state.pep_mapping, State};
handle_call(nodetree, _From, State) ->
    {reply, State#state.nodetree, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(_Msg, State) -> {noreply, State}.

-spec(handle_info/2 ::
    (
	_     :: {route, From::jid(), To::jid(), Packet::xmlel()},
	State :: state())
    -> {noreply, state()}
    ).

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({route, From, To, Packet},
	    #state{server_host = ServerHost, access = Access, plugins = Plugins} = State) ->
    case catch do_route(ServerHost, Access, Plugins, To#jid.lserver, From, To, Packet) of
	{'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]);
	_ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%% @private
terminate(_Reason,
	    #state{host = Host, server_host = ServerHost, nodetree = TreePlugin, plugins = Plugins}) ->
    ejabberd_router:unregister_route(Host),
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
    ejabberd_hooks:delete(sm_remove_connection_hook, ServerHost,
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
    ejabberd_hooks:delete(anonymous_purge_hook, ServerHost,
	?MODULE, remove_user, 50),
    mod_disco:unregister_feature(ServerHost, ?NS_PUBSUB),
    case whereis(gen_mod:get_module_proc(ServerHost, ?LOOPNAME)) of
	undefined ->
	    ?ERROR_MSG("~s process is dead, pubsub was broken", [?LOOPNAME]);
	Pid ->
	    Pid ! stop
    end,
    terminate_plugins(Host, ServerHost, Plugins, TreePlugin).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec(do_route/7 ::
    (
	ServerHost :: binary(),
	Access     :: atom(),
	Plugins    :: [binary(),...],
	Host       :: mod_pubsub:hostPubsub(),
	From       :: jid(),
	To         :: jid(),
	Packet     :: xmlel())
    -> ok
    ).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_route(ServerHost, Access, Plugins, Host, From, To, Packet) ->
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case To of
	#jid{luser = <<>>, lresource = <<>>} ->
	    case Name of
		<<"iq">> ->
		    case jlib:iq_query_info(Packet) of
			#iq{type = get, xmlns = ?NS_DISCO_INFO, sub_el = SubEl, lang = Lang} = IQ ->
			    #xmlel{attrs = QAttrs} = SubEl,
			    Node = xml:get_attr_s(<<"node">>, QAttrs),
			    Info = ejabberd_hooks:run_fold(disco_info, ServerHost,
				    [],
				    [ServerHost, ?MODULE, <<>>, <<>>]),
			    Res = case iq_disco_info(Host, Node, From, Lang) of
				{result, IQRes} ->
				    jlib:iq_to_xml(IQ#iq{type = result,
					    sub_el =
					    [#xmlel{name = <<"query">>,
						    attrs = QAttrs,
						    children = IQRes ++ Info}]});
				{error, Error} ->
				    jlib:make_error_reply(Packet, Error)
			    end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS, sub_el = SubEl} = IQ ->
			    #xmlel{attrs = QAttrs} = SubEl,
			    Node = xml:get_attr_s(<<"node">>, QAttrs),
			    Res = case iq_disco_items(Host, Node, From, jlib:rsm_decode(IQ)) of
				{result, IQRes} ->
				    jlib:iq_to_xml(IQ#iq{type = result,
					    sub_el =
					    [#xmlel{name = <<"query">>,
						    attrs = QAttrs,
						    children = IQRes}]})
				    %{error, Error} ->
				    %     jlib:make_error_reply(Packet, Error)
			    end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = IQType, xmlns = ?NS_PUBSUB, lang = Lang, sub_el = SubEl} = IQ ->
			    Res = case iq_pubsub(Host, ServerHost, From, IQType,
				    SubEl, Lang, Access, Plugins)
			    of
				{result, IQRes} ->
				    jlib:iq_to_xml(IQ#iq{type = result, sub_el = IQRes});
				{error, Error} ->
				    jlib:make_error_reply(Packet, Error)
			    end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = IQType, xmlns = ?NS_PUBSUB_OWNER, lang = Lang, sub_el = SubEl} = IQ ->
			    Res = case iq_pubsub_owner(Host, ServerHost, From,
				    IQType, SubEl, Lang)
			    of
				{result, IQRes} ->
				    jlib:iq_to_xml(IQ#iq{type = result, sub_el = IQRes});
				{error, Error} ->
				    jlib:make_error_reply(Packet, Error)
			    end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = get, xmlns = (?NS_VCARD) = XMLNS, lang = Lang, sub_el = _SubEl} = IQ ->
			    Res = IQ#iq{type = result,
				    sub_el =
				    [#xmlel{name = <<"vCard">>,
					    attrs = [{<<"xmlns">>, XMLNS}],
					    children = iq_get_vcard(Lang)}]},
			    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
			#iq{type = set, xmlns = ?NS_COMMANDS} = IQ ->
			    Res = case iq_command(Host, ServerHost, From, IQ, Access, Plugins) of
				{error, Error} ->
				    jlib:make_error_reply(Packet, Error);
				{result, IQRes} ->
				    jlib:iq_to_xml(IQ#iq{type = result, sub_el = IQRes})
			    end,
			    ejabberd_router:route(To, From, Res);
			#iq{} ->
			    Err = jlib:make_error_reply(Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
			    ejabberd_router:route(To, From, Err);
			_ ->
			    ok
		    end;
		<<"message">> ->
		    case xml:get_attr_s(<<"type">>, Attrs) of
			<<"error">> ->
			    ok;
			_ ->
			    case find_authorization_response(Packet) of
				none ->
				    ok;
				invalid ->
				    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
				    ejabberd_router:route(To, From, Err);
				XFields ->
				    handle_authorization_response(Host, From, To, Packet, XFields)
			    end
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    case xml:get_attr_s(<<"type">>, Attrs) of
		<<"error">> ->
		    ok;
		<<"result">> ->
		    ok;
		_ ->
		    Err = jlib:make_error_reply(Packet, ?ERR_ITEM_NOT_FOUND),
		    ejabberd_router:route(To, From, Err)
	    end
    end.

command_disco_info(_Host, ?NS_COMMANDS, _From) ->
    IdentityEl = #xmlel{name = <<"identity">>,
	    attrs = [{<<"category">>, <<"automation">>},
		{<<"type">>, <<"command-list">>}]},
    {result, [IdentityEl]};
command_disco_info(_Host, ?NS_PUBSUB_GET_PENDING, _From) ->
    IdentityEl = #xmlel{name = <<"identity">>,
	    attrs = [{<<"category">>, <<"automation">>},
		{<<"type">>, <<"command-node">>}]},
    FeaturesEl = #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_COMMANDS}]},
    {result, [IdentityEl, FeaturesEl]}.

node_disco_info(Host, Node, From) ->
    node_disco_info(Host, Node, From, true, true).

node_disco_info(Host, Node, _From, _Identity, _Features) ->
    Action = fun (#pubsub_node{type = Type, options = Options}) ->
	    NodeType = case get_option(Options, node_type) of
		collection -> <<"collection">>;
		_ -> <<"leaf">>
	    end,
	    I = #xmlel{name = <<"identity">>,
			attrs = [{<<"category">>, <<"pubsub">>},
				 {<<"type">>, NodeType}]},
	    F = [#xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_PUBSUB}]}
		    | [#xmlel{name = <<"feature">>,
			    attrs = [{<<"var">>, feature(F)}]}
			|| F <- plugin_features(Host, Type)]],
	    {result, [I | F]}
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
    end.

iq_disco_info(Host, SNode, From, Lang) ->
    [Node | _] = case SNode of
	<<>> -> [<<>>];
	_ -> str:tokens(SNode, <<"!">>)
    end,
    %   Node = string_to_node(RealSNode),
    case Node of
	<<>> ->
	    {result, [#xmlel{name = <<"identity">>,
			attrs = [{<<"category">>, <<"pubsub">>},
			    {<<"type">>, <<"service">>},
			    {<<"name">>, translate:translate(Lang, <<"Publish-Subscribe">>)}]},
		    #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_DISCO_INFO}]},
		    #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_DISCO_ITEMS}]},
		    #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_PUBSUB}]},
		    #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_COMMANDS}]},
		    #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_VCARD}]}]
		++ [#xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, feature(F)}]}
		    || F <- features(Host, Node)]};
	?NS_COMMANDS ->
	    command_disco_info(Host, Node, From);
	?NS_PUBSUB_GET_PENDING ->
	    command_disco_info(Host, Node, From);
	_ ->
	    node_disco_info(Host, Node, From)
    end.

-spec(iq_disco_items/4 ::
    (
	Host   :: mod_pubsub:host(),
	Node   :: <<>> | mod_pubsub:nodeId(),
	From   :: jid(),
	Rsm    :: none | rsm_in())
    -> {result, [xmlel()]}
    ).
iq_disco_items(Host, <<>>, From, _RSM) ->
    {result,
	lists:map(fun (#pubsub_node{nodeid = {_, SubNode}, options = Options}) ->
		    Attrs = case get_option(Options, title) of
			false ->
			    [{<<"jid">>, Host}
				| nodeAttr(SubNode)];
			Title ->
			    [{<<"jid">>, Host},
				{<<"name">>, Title}
				| nodeAttr(SubNode)]
		    end,
		    #xmlel{name = <<"item">>, attrs = Attrs}
	    end,
	    tree_action(Host, get_subnodes, [Host, <<>>, From]))};
iq_disco_items(Host, ?NS_COMMANDS, _From, _RSM) ->
    {result, [#xmlel{name = <<"item">>,
		attrs = [{<<"jid">>, Host},
		    {<<"node">>, ?NS_PUBSUB_GET_PENDING},
		    {<<"name">>, <<"Get Pending">>}]}]};
iq_disco_items(_Host, ?NS_PUBSUB_GET_PENDING, _From, _RSM) ->
    {result, []};
iq_disco_items(Host, Item, From, RSM) ->
    case str:tokens(Item, <<"!">>) of
	[_Node, _ItemId] ->
	    {result, []};
	[Node] ->
	    Action = fun (#pubsub_node{id = Nidx, type = Type, options = Options, owners = O}) ->
		    Owners = node_owners_call(Host, Type, Nidx, O),
		    {NodeItems, RsmOut} = case get_allowed_items_call(Host, Nidx,
			    From, Type, Options, Owners, RSM)
		    of
			{result, R} -> R;
			_ -> {[], none}
		    end,
		    Nodes = lists:map(fun (#pubsub_node{nodeid = {_, SubNode}, options = SubOptions}) ->
				    Attrs = case get_option(SubOptions, title) of
					false ->
					    [{<<"jid">>, Host}
						| nodeAttr(SubNode)];
					Title ->
					    [{<<"jid">>, Host},
						{<<"name">>, Title}
						| nodeAttr(SubNode)]
				    end,
				    #xmlel{name = <<"item">>, attrs = Attrs}
			    end,
			    tree_call(Host, get_subnodes, [Host, Node, From])),
		    Items = lists:map(fun (#pubsub_item{itemid = {RN, _}}) ->
				    {result, Name} = node_call(Host, Type, get_item_name, [Host, Node, RN]),
				    #xmlel{name = <<"item">>,
					attrs = [{<<"jid">>, Host}, {<<"name">>, Name}]}
			    end,
			    NodeItems),
		    {result, Nodes ++ Items ++ jlib:rsm_encode(RsmOut)}
	    end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Result}} -> {result, Result};
		Other -> Other
	    end
    end.

-spec(iq_sm/3 ::
    (
	From :: jid(),
	To   :: jid(),
	IQ   :: iq_request())
    -> iq_result() | iq_error()
    ).
iq_sm(From, To, #iq{type = Type, sub_el = SubEl, xmlns = XMLNS, lang = Lang} = IQ) ->
    ServerHost = To#jid.lserver,
    LOwner = jid:tolower(jid:remove_resource(To)),
    Res = case XMLNS of
	?NS_PUBSUB ->
	    iq_pubsub(LOwner, ServerHost, From, Type, SubEl, Lang);
	?NS_PUBSUB_OWNER ->
	    iq_pubsub_owner(LOwner, ServerHost, From, Type, SubEl, Lang)
    end,
    case Res of
	{result, IQRes} -> IQ#iq{type = result, sub_el = IQRes};
	{error, Error} -> IQ#iq{type = error, sub_el = [Error, SubEl]}
    end.

iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>, attrs = [],
	    children = [{xmlcdata, <<"ejabberd/mod_pubsub">>}]},
	#xmlel{name = <<"URL">>, attrs = [],
	    children = [{xmlcdata, ?EJABBERD_URI}]},
	#xmlel{name = <<"DESC">>, attrs = [],
	    children = [{xmlcdata,
		    <<(translate:translate(Lang, <<"ejabberd Publish-Subscribe module">>))/binary,
			"\nCopyright (c) 2004-2015 ProcessOne">>}]}].

-spec(iq_pubsub/6 ::
    (
	Host       :: mod_pubsub:host(),
	ServerHost :: binary(),
	From       :: jid(),
	IQType     :: 'get' | 'set',
	SubEl      :: xmlel(),
	Lang       :: binary())
    -> {result, [xmlel()]}
    %%%
    | {error, xmlel()}
    ).

iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang) ->
    iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, all, plugins(ServerHost)).

-spec(iq_pubsub/8 ::
    (
	Host       :: mod_pubsub:host(),
	ServerHost :: binary(),
	From       :: jid(),
	IQType     :: 'get' | 'set',
	SubEl      :: xmlel(),
	Lang       :: binary(),
	Access     :: atom(),
	Plugins    :: [binary(),...])
    -> {result, [xmlel()]}
    %%%
    | {error, xmlel()}
    ).

iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, Access, Plugins) ->
    #xmlel{children = SubEls} = SubEl,
    case xml:remove_cdata(SubEls) of
	[#xmlel{name = Name, attrs = Attrs, children = Els} | Rest] ->
	    Node = xml:get_attr_s(<<"node">>, Attrs),
	    case {IQType, Name} of
		{set, <<"create">>} ->
		    Config = case Rest of
			[#xmlel{name = <<"configure">>, children = C}] -> C;
			_ -> []
		    end,
		    Type = case xml:get_attr_s(<<"type">>, Attrs) of
			<<>> -> hd(Plugins);
			T -> T
		    end,
		    case lists:member(Type, Plugins) of
			false ->
			    {error,
				extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"create-nodes">>)};
			true ->
			    create_node(Host, ServerHost, Node, From, Type, Access, Config)
		    end;
		{set, <<"publish">>} ->
		    case xml:remove_cdata(Els) of
			[#xmlel{name = <<"item">>, attrs = ItemAttrs,
					children = Payload}] ->
			    ItemId = xml:get_attr_s(<<"id">>, ItemAttrs),
			    publish_item(Host, ServerHost, Node, From, ItemId, Payload, Access);
			[] ->
			    {error,
				extended_error(?ERR_BAD_REQUEST, <<"item-required">>)};
			_ ->
			    {error,
				extended_error(?ERR_BAD_REQUEST, <<"invalid-payload">>)}
		    end;
		{set, <<"retract">>} ->
		    ForceNotify = case xml:get_attr_s(<<"notify">>, Attrs) of
			<<"1">> -> true;
			<<"true">> -> true;
			_ -> false
		    end,
		    case xml:remove_cdata(Els) of
			[#xmlel{name = <<"item">>, attrs = ItemAttrs}] ->
			    ItemId = xml:get_attr_s(<<"id">>, ItemAttrs),
			    delete_item(Host, Node, From, ItemId, ForceNotify);
			_ ->
			    {error,
				extended_error(?ERR_BAD_REQUEST, <<"item-required">>)}
		    end;
		{set, <<"subscribe">>} ->
		    Config = case Rest of
			[#xmlel{name = <<"options">>, children = C}] -> C;
			_ -> []
		    end,
		    JID = xml:get_attr_s(<<"jid">>, Attrs),
		    subscribe_node(Host, Node, From, JID, Config);
		{set, <<"unsubscribe">>} ->
		    JID = xml:get_attr_s(<<"jid">>, Attrs),
		    SubId = xml:get_attr_s(<<"subid">>, Attrs),
		    unsubscribe_node(Host, Node, From, JID, SubId);
		{get, <<"items">>} ->
		    MaxItems = xml:get_attr_s(<<"max_items">>, Attrs),
		    SubId = xml:get_attr_s(<<"subid">>, Attrs),
		    ItemIds = lists:foldl(fun
				(#xmlel{name = <<"item">>, attrs = ItemAttrs}, Acc) ->
				    case xml:get_attr_s(<<"id">>, ItemAttrs) of
					<<>> -> Acc;
					ItemId -> [ItemId | Acc]
				    end;
				(_, Acc) ->
				    Acc
			    end,
			    [], xml:remove_cdata(Els)),
		    get_items(Host, Node, From, SubId, MaxItems, ItemIds, jlib:rsm_decode(SubEl));
		{get, <<"subscriptions">>} ->
		    get_subscriptions(Host, Node, From, Plugins);
		{get, <<"affiliations">>} ->
		    get_affiliations(Host, Node, From, Plugins);
		{get, <<"options">>} ->
		    SubId = xml:get_attr_s(<<"subid">>, Attrs),
		    JID = xml:get_attr_s(<<"jid">>, Attrs),
		    get_options(Host, Node, JID, SubId, Lang);
		{set, <<"options">>} ->
		    SubId = xml:get_attr_s(<<"subid">>, Attrs),
		    JID = xml:get_attr_s(<<"jid">>, Attrs),
		    set_options(Host, Node, JID, SubId, Els);
		_ ->
		    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	    end;
	Other ->
	    ?INFO_MSG("Too many actions: ~p", [Other]),
	    {error, ?ERR_BAD_REQUEST}
    end.


-spec(iq_pubsub_owner/6 ::
    (
	Host       :: mod_pubsub:host(),
	ServerHost :: binary(),
	From       :: jid(),
	IQType     :: 'get' | 'set',
	SubEl      :: xmlel(),
	Lang       :: binary())
    -> {result, [xmlel()]}
    %%%
    | {error, xmlel()}
    ).
iq_pubsub_owner(Host, ServerHost, From, IQType, SubEl, Lang) ->
    #xmlel{children = SubEls} = SubEl,
    Action = xml:remove_cdata(SubEls),
    case Action of
	[#xmlel{name = Name, attrs = Attrs, children = Els}] ->
	    Node = xml:get_attr_s(<<"node">>, Attrs),
	    case {IQType, Name} of
		{get, <<"configure">>} ->
		    get_configure(Host, ServerHost, Node, From, Lang);
		{set, <<"configure">>} ->
		    set_configure(Host, Node, From, Els, Lang);
		{get, <<"default">>} ->
		    get_default(Host, Node, From, Lang);
		{set, <<"delete">>} ->
		    delete_node(Host, Node, From);
		{set, <<"purge">>} ->
		    purge_node(Host, Node, From);
		{get, <<"subscriptions">>} ->
		    get_subscriptions(Host, Node, From);
		{set, <<"subscriptions">>} ->
		    set_subscriptions(Host, Node, From, xml:remove_cdata(Els));
		{get, <<"affiliations">>} ->
		    get_affiliations(Host, Node, From);
		{set, <<"affiliations">>} ->
		    set_affiliations(Host, Node, From, xml:remove_cdata(Els));
		_ ->
		    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	    end;
	_ ->
	    ?INFO_MSG("Too many actions: ~p", [Action]),
	    {error, ?ERR_BAD_REQUEST}
    end.

iq_command(Host, ServerHost, From, IQ, Access, Plugins) ->
    case adhoc:parse_request(IQ) of
	Req when is_record(Req, adhoc_request) ->
	    case adhoc_request(Host, ServerHost, From, Req, Access, Plugins) of
		Resp when is_record(Resp, adhoc_response) ->
		    {result, [adhoc:produce_response(Req, Resp)]};
		Error ->
		    Error
	    end;
	Err -> Err
    end.

%% @doc <p>Processes an Ad Hoc Command.</p>
adhoc_request(Host, _ServerHost, Owner,
	    #adhoc_request{node = ?NS_PUBSUB_GET_PENDING,
		lang = Lang, action = <<"execute">>,
		xdata = false},
	    _Access, Plugins) ->
    send_pending_node_form(Host, Owner, Lang, Plugins);
adhoc_request(Host, _ServerHost, Owner,
	    #adhoc_request{node = ?NS_PUBSUB_GET_PENDING,
		action = <<"execute">>, xdata = XData},
	    _Access, _Plugins) ->
    ParseOptions = case XData of
	#xmlel{name = <<"x">>} = XEl ->
	    case jlib:parse_xdata_submit(XEl) of
		invalid ->
		    {error, ?ERR_BAD_REQUEST};
		XData2 ->
		    case set_xoption(Host, XData2, []) of
			NewOpts when is_list(NewOpts) -> {result, NewOpts};
			Err -> Err
		    end
	    end;
	_ ->
	    ?INFO_MSG("Bad XForm: ~p", [XData]),
	    {error, ?ERR_BAD_REQUEST}
    end,
    case ParseOptions of
	{result, XForm} ->
	    case lists:keysearch(node, 1, XForm) of
		{value, {_, Node}} -> send_pending_auth_events(Host, Node, Owner);
		false -> {error, extended_error(?ERR_BAD_REQUEST, <<"bad-payload">>)}
	    end;
	Error -> Error
    end;
adhoc_request(_Host, _ServerHost, _Owner,
	    #adhoc_request{action = <<"cancel">>}, _Access,
	    _Plugins) ->
    #adhoc_response{status = canceled};
adhoc_request(Host, ServerHost, Owner,
	    #adhoc_request{action = <<>>} = R, Access, Plugins) ->
    adhoc_request(Host, ServerHost, Owner,
	R#adhoc_request{action = <<"execute">>}, Access,
	Plugins);
adhoc_request(_Host, _ServerHost, _Owner, Other, _Access, _Plugins) ->
    ?DEBUG("Couldn't process ad hoc command:~n~p", [Other]),
    {error, ?ERR_ITEM_NOT_FOUND}.

%% @doc <p>Sends the process pending subscriptions XForm for Host to Owner.</p>
send_pending_node_form(Host, Owner, _Lang, Plugins) ->
    Filter = fun (Type) ->
	    lists:member(<<"get-pending">>, plugin_features(Host, Type))
    end,
    case lists:filter(Filter, Plugins) of
	[] ->
	    {error, ?ERR_FEATURE_NOT_IMPLEMENTED};
	Ps ->
	    XOpts = [#xmlel{name = <<"option">>, attrs = [],
			children = [#xmlel{name = <<"value">>,
				attrs = [],
				children = [{xmlcdata, Node}]}]}
		    || Node <- get_pending_nodes(Host, Owner, Ps)],
	    XForm = #xmlel{name = <<"x">>,
		    attrs = [{<<"xmlns">>, ?NS_XDATA},
			{<<"type">>, <<"form">>}],
		    children = [#xmlel{name = <<"field">>,
			    attrs = [{<<"type">>, <<"list-single">>},
				{<<"var">>, <<"pubsub#node">>}],
			    children = lists:usort(XOpts)}]},
	    #adhoc_response{status = executing,
		defaultaction = <<"execute">>, elements = [XForm]}
    end.

get_pending_nodes(Host, Owner, Plugins) ->
    Tr = fun (Type) ->
	    case node_call(Host, Type, get_pending_nodes, [Host, Owner]) of
		{result, Nodes} -> Nodes;
		_ -> []
	    end
    end,
    Action = fun() -> {result, lists:flatmap(Tr, Plugins)} end,
    case transaction(Host, Action, sync_dirty) of
	{result, Res} -> Res;
	Err -> Err
    end.

%% @doc <p>Send a subscription approval form to Owner for all pending
%% subscriptions on Host and Node.</p>
send_pending_auth_events(Host, Node, Owner) ->
    ?DEBUG("Sending pending auth events for ~s on ~s:~s",
	[jid:to_string(Owner), Host, Node]),
    Action = fun (#pubsub_node{id = Nidx, type = Type}) ->
	    case lists:member(<<"get-pending">>, plugin_features(Host, Type)) of
		true ->
		    case node_call(Host, Type, get_affiliation, [Nidx, Owner]) of
			{result, owner} -> node_call(Host, Type, get_node_subscriptions, [Nidx]);
			_ -> {error, ?ERR_FORBIDDEN}
		    end;
		false ->
		    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {N, Subs}} ->
	    lists:foreach(fun
		    ({J, pending, _SubId}) -> send_authorization_request(N, jid:make(J));
		    ({J, pending}) -> send_authorization_request(N, jid:make(J));
		    (_) -> ok
		end,
		Subs),
	    #adhoc_response{};
	Err ->
	    Err
    end.

%%% authorization handling

send_authorization_request(#pubsub_node{nodeid = {Host, Node}, type = Type, id = Nidx, owners = O},
	    Subscriber) ->
    Lang = <<"en">>,
    Stanza = #xmlel{name = <<"message">>, attrs = [],
	    children =
	    [#xmlel{name = <<"x">>,
		    attrs =
		    [{<<"xmlns">>, ?NS_XDATA},
			{<<"type">>, <<"form">>}],
		    children =
		    [#xmlel{name = <<"title">>, attrs = [],
			    children =
			    [{xmlcdata,
				    translate:translate(Lang, <<"PubSub subscriber request">>)}]},
			#xmlel{name = <<"instructions">>,
			    attrs = [],
			    children =
			    [{xmlcdata,
				    translate:translate(Lang,
					<<"Choose whether to approve this entity's "
					    "subscription.">>)}]},
			#xmlel{name = <<"field">>,
			    attrs =
			    [{<<"var">>, <<"FORM_TYPE">>},
				{<<"type">>, <<"hidden">>}],
			    children =
			    [#xmlel{name = <<"value">>,
				    attrs = [],
				    children =
				    [{xmlcdata, ?NS_PUBSUB_SUB_AUTH}]}]},
			#xmlel{name = <<"field">>,
			    attrs =
			    [{<<"var">>, <<"pubsub#node">>},
				{<<"type">>,
				    <<"text-single">>},
				{<<"label">>, translate:translate(Lang, <<"Node ID">>)}],
			    children =
			    [#xmlel{name = <<"value">>,
				    attrs = [],
				    children =
				    [{xmlcdata, Node}]}]},
			#xmlel{name = <<"field">>,
			    attrs =
			    [{<<"var">>,
				    <<"pubsub#subscriber_jid">>},
				{<<"type">>, <<"jid-single">>},
				{<<"label">>,
				    translate:translate(Lang, <<"Subscriber Address">>)}],
			    children =
			    [#xmlel{name = <<"value">>,
				    attrs = [],
				    children =
				    [{xmlcdata, jid:to_string(Subscriber)}]}]},
			#xmlel{name = <<"field">>,
			    attrs =
			    [{<<"var">>,
				    <<"pubsub#allow">>},
				{<<"type">>, <<"boolean">>},
				{<<"label">>,
				    translate:translate(Lang,
					<<"Allow this Jabber ID to subscribe to "
					    "this pubsub node?">>)}],
			    children =
			    [#xmlel{name = <<"value">>,
				    attrs = [],
				    children =
				    [{xmlcdata, <<"false">>}]}]}]}]},
    lists:foreach(fun (Owner) ->
		ejabberd_router:route(service_jid(Host), jid:make(Owner), Stanza)
	end,
	node_owners_action(Host, Type, Nidx, O)).

find_authorization_response(Packet) ->
    #xmlel{children = Els} = Packet,
    XData1 = lists:map(fun
		(#xmlel{name = <<"x">>, attrs = XAttrs} = XEl) ->
		    case xml:get_attr_s(<<"xmlns">>, XAttrs) of
			?NS_XDATA ->
			    case xml:get_attr_s(<<"type">>, XAttrs) of
				<<"cancel">> -> none;
				_ -> jlib:parse_xdata_submit(XEl)
			    end;
			_ ->
			    none
		    end;
		(_) ->
		    none
	    end,
	    xml:remove_cdata(Els)),
    XData = lists:filter(fun (E) -> E /= none end, XData1),
    case XData of
	[invalid] ->
	    invalid;
	[] ->
	    none;
	[XFields] when is_list(XFields) ->
	    ?DEBUG("XFields: ~p", [XFields]),
	    case lists:keysearch(<<"FORM_TYPE">>, 1, XFields) of
		{value, {_, [?NS_PUBSUB_SUB_AUTH]}} -> XFields;
		_ -> invalid
	    end
    end.

%% @doc Send a message to JID with the supplied Subscription
send_authorization_approval(Host, JID, SNode, Subscription) ->
    SubAttrs = case Subscription of
	%{S, SID} ->
	%    [{<<"subscription">>, subscription_to_string(S)},
	%     {<<"subid">>, SID}];
	S -> 
	    [{<<"subscription">>, subscription_to_string(S)}]
    end,
    Stanza = event_stanza(<<"subscription">>, 
	    [{<<"jid">>, jid:to_string(JID)}
		| nodeAttr(SNode)]
	    ++ SubAttrs),
    ejabberd_router:route(service_jid(Host), JID, Stanza).

handle_authorization_response(Host, From, To, Packet, XFields) ->
    case {lists:keysearch(<<"pubsub#node">>, 1, XFields),
	    lists:keysearch(<<"pubsub#subscriber_jid">>, 1, XFields),
	    lists:keysearch(<<"pubsub#allow">>, 1, XFields)}
    of
	{{value, {_, [Node]}},
		    {value, {_, [SSubscriber]}},
		    {value, {_, [SAllow]}}} ->
	    FromLJID = jid:tolower(jid:remove_resource(From)),
	    Subscriber = jid:from_string(SSubscriber),
	    Allow = case SAllow of
		<<"1">> -> true;
		<<"true">> -> true;
		_ -> false
	    end,
	    Action = fun (#pubsub_node{type = Type, id = Nidx, owners = O}) ->
		    Owners = node_owners_call(Host, Type, Nidx, O),
		    case lists:member(FromLJID, Owners) of
			true ->
			    {result, Subs} = node_call(Host, Type, get_subscriptions, [Nidx, Subscriber]),
			    update_auth(Host, Node, Type, Nidx, Subscriber, Allow, Subs);
			false ->
			    {error, ?ERR_FORBIDDEN}
		    end
	    end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{error, Error} ->
		    Err = jlib:make_error_reply(Packet, Error),
		    ejabberd_router:route(To, From, Err);
		{result, {_, _NewSubscription}} ->
		    %% XXX: notify about subscription state change, section 12.11
		    ok;
		_ ->
		    Err = jlib:make_error_reply(Packet, ?ERR_INTERNAL_SERVER_ERROR),
		    ejabberd_router:route(To, From, Err)
	    end;
	_ ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ACCEPTABLE),
	    ejabberd_router:route(To, From, Err)
    end.

update_auth(Host, Node, Type, Nidx, Subscriber, Allow, Subs) ->
    Sub= lists:filter(fun
		({pending, _}) -> true;
		(_) -> false
	    end,
	    Subs),
    case Sub of
	[{pending, SubId}] ->
	    NewSub = case Allow of
		true -> subscribed;
		false -> none
	    end,
	    node_call(Host, Type, set_subscriptions, [Nidx, Subscriber, NewSub, SubId]),
	    send_authorization_approval(Host, Subscriber, Node, NewSub),
	    {result, ok};
	_ ->
	    {error, ?ERR_UNEXPECTED_REQUEST}
    end.

-define(XFIELD(Type, Label, Var, Val),
    #xmlel{name = <<"field">>,
	attrs = [{<<"type">>, Type},
	    {<<"label">>, translate:translate(Lang, Label)},
	    {<<"var">>, Var}],
	children = [#xmlel{name = <<"value">>, attrs = [],
		children = [{xmlcdata, Val}]}]}).

-define(BOOLXFIELD(Label, Var, Val),
    ?XFIELD(<<"boolean">>, Label, Var,
	case Val of
	    true -> <<"1">>;
	    _ -> <<"0">>
	end)).

-define(STRINGXFIELD(Label, Var, Val),
    ?XFIELD(<<"text-single">>, Label, Var, Val)).

-define(STRINGMXFIELD(Label, Var, Vals),
    #xmlel{name = <<"field">>,
	attrs = [{<<"type">>, <<"text-multi">>},
	    {<<"label">>, translate:translate(Lang, Label)},
	    {<<"var">>, Var}],
	children = [#xmlel{name = <<"value">>, attrs = [],
		children = [{xmlcdata, V}]}
	    || V <- Vals]}).

-define(XFIELDOPT(Type, Label, Var, Val, Opts),
    #xmlel{name = <<"field">>,
	attrs = [{<<"type">>, Type},
	    {<<"label">>, translate:translate(Lang, Label)},
	    {<<"var">>, Var}],
	children = [#xmlel{name = <<"option">>, attrs = [],
		children = [#xmlel{name = <<"value">>,
			attrs = [],
			children = [{xmlcdata, Opt}]}]}
	    || Opt <- Opts]
	++
	[#xmlel{name = <<"value">>, attrs = [],
		children = [{xmlcdata, Val}]}]}).

-define(LISTXFIELD(Label, Var, Val, Opts),
    ?XFIELDOPT(<<"list-single">>, Label, Var, Val, Opts)).

-define(LISTMXFIELD(Label, Var, Vals, Opts),
    #xmlel{name = <<"field">>,
	attrs = [{<<"type">>, <<"list-multi">>},
	    {<<"label">>, translate:translate(Lang, Label)},
	    {<<"var">>, Var}],
	children = [#xmlel{name = <<"option">>, attrs = [],
		children = [#xmlel{name = <<"value">>,
			attrs = [],
			children = [{xmlcdata, Opt}]}]}
	    || Opt <- Opts]
	++
	[#xmlel{name = <<"value">>, attrs = [],
		children = [{xmlcdata, Val}]}
	    || Val <- Vals]}).

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
-spec(create_node/7 ::
    (
	Host          :: mod_pubsub:host(),
	ServerHost    :: binary(),
	Node        :: <<>> | mod_pubsub:nodeId(),
	Owner         :: jid(),
	Type          :: binary(),
	Access        :: atom(),
	Configuration :: [xmlel()])
    -> {result, [xmlel(),...]}
    %%%
    | {error, xmlel()}
    ).
create_node(Host, ServerHost, Node, Owner, Type) ->
    create_node(Host, ServerHost, Node, Owner, Type, all, []).
create_node(Host, ServerHost, <<>>, Owner, Type, Access, Configuration) ->
    case lists:member(<<"instant-nodes">>, plugin_features(Host, Type)) of
	true ->
	    Node = randoms:get_string(),
	    case create_node(Host, ServerHost, Node, Owner, Type, Access, Configuration) of
		{result, _} ->
		    {result, [#xmlel{name = <<"pubsub">>,
				attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
				children = [#xmlel{name = <<"create">>,
					attrs = nodeAttr(Node)}]}]};
		Error ->
		    Error
	    end;
	false ->
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, <<"nodeid-required">>)}
    end;
create_node(Host, ServerHost, Node, Owner, GivenType, Access, Configuration) ->
    Type = select_type(ServerHost, Host, Node, GivenType),
    ParseOptions = case xml:remove_cdata(Configuration) of
	[] ->
	    {result, node_options(Host, Type)};
	[#xmlel{name = <<"x">>} = XEl] ->
	    case jlib:parse_xdata_submit(XEl) of
		invalid ->
		    {error, ?ERR_BAD_REQUEST};
		XData ->
		    case set_xoption(Host, XData, node_options(Host, Type)) of
			NewOpts when is_list(NewOpts) -> {result, NewOpts};
			Err -> Err
		    end
	    end;
	_ ->
	    ?INFO_MSG("Node ~p; bad configuration: ~p", [Node, Configuration]),
	    {error, ?ERR_BAD_REQUEST}
    end,
    case ParseOptions of
	{result, NodeOptions} ->
	    CreateNode = fun () ->
		    Parent = case node_call(Host, Type, node_to_path, [Node]) of
			{result, [Node]} ->
			    <<>>;
			{result, Path} ->
			    element(2, node_call(Host, Type, path_to_node, [lists:sublist(Path, length(Path)-1)]))
		    end,
		    Parents = case Parent of
			<<>> -> [];
			_ -> [Parent]
		    end,
		    case node_call(Host, Type, create_node_permission,
			    [Host, ServerHost, Node, Parent, Owner, Access])
		    of
			{result, true} ->
			    case tree_call(Host, create_node,
				    [Host, Node, Type, Owner, NodeOptions, Parents])
			    of
				{ok, Nidx} ->
				    SubsByDepth = get_node_subs_by_depth(Host, Node, Owner),
				    case node_call(Host, Type, create_node, [Nidx, Owner]) of
					{result, Result} -> {result, {Nidx, SubsByDepth, Result}};
					Error -> Error
				    end;
				{error, {virtual, Nidx}} ->
				    case node_call(Host, Type, create_node, [Nidx, Owner]) of
					{result, Result} -> {result, {Nidx, [], Result}};
					Error -> Error
				    end;
				Error ->
				    Error
			    end;
			_ ->
			    {error, ?ERR_FORBIDDEN}
		    end
	    end,
	    Reply = [#xmlel{name = <<"pubsub">>,
			attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
			children = [#xmlel{name = <<"create">>,
				attrs = nodeAttr(Node)}]}],
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
	    end;
	Error ->
	    Error
    end.

%% @doc <p>Delete specified node and all childs.</p>
%%<p>There are several reasons why the node deletion request might fail:</p>
%%<ul>
%%<li>The requesting entity does not have sufficient privileges to delete the node.</li>
%%<li>The node is the root collection node, which cannot be deleted.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
-spec(delete_node/3 ::
    (
	Host  :: mod_pubsub:host(),
	Node  :: mod_pubsub:nodeId(),
	Owner :: jid())
    -> {result, [xmlel(),...]}
    %%%
    | {error, xmlel()}
    ).
delete_node(_Host, <<>>, _Owner) ->
    {error, ?ERR_NOT_ALLOWED};
delete_node(Host, Node, Owner) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    case node_call(Host, Type, get_affiliation, [Nidx, Owner]) of
		{result, owner} ->
		    SubsByDepth = get_node_subs_by_depth(Host, Node, service_jid(Host)),
		    Removed = tree_call(Host, delete_node, [Host, Node]),
		    case node_call(Host, Type, delete_node, [Removed]) of
			{result, Res} -> {result, {SubsByDepth, Res}};
			Error -> Error
		    end;
		_ ->
		    {error, ?ERR_FORBIDDEN}
	    end
    end,
    Reply = [],
    ServerHost = serverhost(Host),
    case transaction(Host, Node, Action, transaction) of
	{result, {_, {SubsByDepth, {Result, broadcast, Removed}}}} ->
	    lists:foreach(fun ({RNode, _RSubs}) ->
			{RH, RN} = RNode#pubsub_node.nodeid,
			RNidx = RNode#pubsub_node.id,
			RType = RNode#pubsub_node.type,
			ROptions = RNode#pubsub_node.options,
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
-spec(subscribe_node/5 ::
    (
	Host          :: mod_pubsub:host(),
	Node          :: mod_pubsub:nodeId(),
	From          :: jid(),
	JID           :: binary(),
	Configuration :: [xmlel()])
    -> {result, [xmlel(),...]}
    %%%
    | {error, xmlel()}
    ).
subscribe_node(Host, Node, From, JID, Configuration) ->
    SubModule = subscription_plugin(Host),
    SubOpts = case SubModule:parse_options_xform(Configuration) of
	{result, GoodSubOpts} -> GoodSubOpts;
	_ -> invalid
    end,
    Subscriber = string_to_ljid(JID),
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
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"subscribe">>)};
		not SubscribeConfig ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"subscribe">>)};
		HasOptions andalso not OptionsFeature ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"subscription-options">>)};
		SubOpts == invalid ->
		    {error,
			extended_error(?ERR_BAD_REQUEST, <<"invalid-options">>)};
		not CanSubscribe ->
		    %% fallback to closest XEP compatible result, assume we are not allowed to subscribe
		    {error,
			extended_error(?ERR_NOT_ALLOWED, <<"closed-node">>)};
		true ->
		    Owners = node_owners_call(Host, Type, Nidx, O),
		    {PS, RG} = get_presence_and_roster_permissions(Host, Subscriber,
			    Owners, AccessModel, AllowedGroups),
		    node_call(Host, Type, subscribe_node,
			[Nidx, From, Subscriber, AccessModel,
			    SendLast, PS, RG, SubOpts])
	    end
    end,
    Reply = fun (Subscription) ->
	    SubAttrs = case Subscription of
		{subscribed, SubId} ->
		    [{<<"subscription">>, subscription_to_string(subscribed)},
			{<<"subid">>, SubId}, {<<"node">>, Node}];
		Other ->
		    [{<<"subscription">>, subscription_to_string(Other)},
			{<<"node">>, Node}]
	    end,
	    [#xmlel{name = <<"pubsub">>,
		    attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
		    children = [#xmlel{name = <<"subscription">>,
			    attrs = [{<<"jid">>, jid:to_string(Subscriber)}
				| SubAttrs]}]}]
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, subscribed, SubId, send_last}}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    send_items(Host, Node, Nidx, Type, Options, Subscriber, last),
	    case Result of
		default -> {result, Reply({subscribed, SubId})};
		_ -> {result, Result}
	    end;
	{result, {_TNode, {default, subscribed, SubId}}} ->
	    {result, Reply({subscribed, SubId})};
	{result, {_TNode, {Result, subscribed, _SubId}}} ->
	    {result, Result};
	{result, {TNode, {default, pending, _SubId}}} ->
	    send_authorization_request(TNode, Subscriber),
	    {result, Reply(pending)};
	{result, {TNode, {Result, pending}}} ->
	    send_authorization_request(TNode, Subscriber),
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
-spec(unsubscribe_node/5 ::
    (
	Host  :: mod_pubsub:host(),
	Node  :: mod_pubsub:nodeId(),
	From  :: jid(),
	JID   :: binary() | ljid(),
	SubId :: mod_pubsub:subId())
    -> {result, []}
    %%%
    | {error, xmlel()}
    ).
unsubscribe_node(Host, Node, From, JID, SubId) when is_binary(JID) ->
    unsubscribe_node(Host, Node, From, string_to_ljid(JID), SubId);
unsubscribe_node(Host, Node, From, Subscriber, SubId) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    node_call(Host, Type, unsubscribe_node, [Nidx, From, Subscriber, SubId])
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, default}} -> {result, []};
	%      {result, {_, Result}} -> {result, Result};
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
-spec(publish_item/6 ::
    (
	Host       :: mod_pubsub:host(),
	ServerHost :: binary(),
	Node       :: mod_pubsub:nodeId(),
	Publisher  :: jid(),
	ItemId     :: <<>> | mod_pubsub:itemId(),
	Payload    :: mod_pubsub:payload())
    -> {result, [xmlel(),...]}
    %%%
    | {error, xmlel()}
    ).
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload) ->
    publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload, all).
publish_item(Host, ServerHost, Node, Publisher, <<>>, Payload, Access) ->
    publish_item(Host, ServerHost, Node, Publisher, uniqid(), Payload, Access);
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload, Access) ->
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
	    if not PublishFeature ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"publish">>)};
		PayloadSize > PayloadMaxSize ->
		    {error,
			extended_error(?ERR_NOT_ACCEPTABLE, <<"payload-too-big">>)};
		(PayloadCount == 0) and (Payload == []) ->
		    {error,
			extended_error(?ERR_BAD_REQUEST, <<"payload-required">>)};
		(PayloadCount > 1) or (PayloadCount == 0) ->
		    {error,
			extended_error(?ERR_BAD_REQUEST, <<"invalid-payload">>)};
		(DeliverPayloads == false) and (PersistItems == false) and
			(PayloadSize > 0) ->
		    {error,
			extended_error(?ERR_BAD_REQUEST, <<"item-forbidden">>)};
		((DeliverPayloads == true) or (PersistItems == true)) and (PayloadSize == 0) ->
		    {error,
			extended_error(?ERR_BAD_REQUEST, <<"item-required">>)};
		true ->
		    node_call(Host, Type, publish_item,
			[Nidx, Publisher, PublishModel, MaxItems, ItemId, Payload])
	    end
    end,
    Reply = [#xmlel{name = <<"pubsub">>,
		attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
		children = [#xmlel{name = <<"publish">>, attrs = nodeAttr(Node),
			children = [#xmlel{name = <<"item">>,
				attrs = itemAttr(ItemId)}]}]}],
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, Broadcast, Removed}}} ->
	    Nidx = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    BrPayload = case Broadcast of
		broadcast -> Payload;
		PluginPayload -> PluginPayload
	    end,
	    ejabberd_hooks:run(pubsub_publish_item, ServerHost,
		[ServerHost, Node, Publisher, service_jid(Host), ItemId, BrPayload]),
	    set_cached_item(Host, Nidx, ItemId, Publisher, BrPayload),
	    case get_option(Options, deliver_notifications) of
		true ->
		    broadcast_publish_item(Host, Node, Nidx, Type, Options, ItemId,
			Publisher, BrPayload, Removed);
		false ->
		    ok
	    end,
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
	{error, ?ERR_ITEM_NOT_FOUND} ->
	    Type = select_type(ServerHost, Host, Node),
	    case lists:member(<<"auto-create">>, plugin_features(Host, Type)) of
		true ->
		    case create_node(Host, ServerHost, Node, Publisher, Type, Access, []) of
			{result,
				    [#xmlel{name = <<"pubsub">>,
					    attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
					    children = [#xmlel{name = <<"create">>,
						    attrs = [{<<"node">>, NewNode}]}]}]} ->
			    publish_item(Host, ServerHost, NewNode, Publisher, ItemId, Payload);
			_ ->
			    {error, ?ERR_ITEM_NOT_FOUND}
		    end;
		false ->
		    {error, ?ERR_ITEM_NOT_FOUND}
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
-spec(delete_item/4 ::
    (
	Host      :: mod_pubsub:host(),
	Node      :: mod_pubsub:nodeId(),
	Publisher :: jid(),
	ItemId    :: mod_pubsub:itemId())
    -> {result, []}
    %%%
    | {error, xmlel()}
    ).
delete_item(Host, Node, Publisher, ItemId) ->
    delete_item(Host, Node, Publisher, ItemId, false).
delete_item(_, <<>>, _, _, _) ->
    {error, extended_error(?ERR_BAD_REQUEST, <<"node-required">>)};
delete_item(Host, Node, Publisher, ItemId, ForceNotify) ->
    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx}) ->
	    Features = plugin_features(Host, Type),
	    PersistentFeature = lists:member(<<"persistent-items">>, Features),
	    DeleteFeature = lists:member(<<"delete-items">>, Features),
	    PublishModel = get_option(Options, publish_model),
	    if %%->   iq_pubsub just does that matchs
		%%        %% Request does not specify an item
		%%        {error, extended_error(?ERR_BAD_REQUEST, "item-required")};
		not PersistentFeature ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"persistent-items">>)};
		not DeleteFeature ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"delete-items">>)};
		true ->
		    node_call(Host, Type, delete_item, [Nidx, Publisher, PublishModel, ItemId])
	    end
    end,
    Reply = [],
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
-spec(purge_node/3 ::
    (
	Host  :: mod_pubsub:host(),
	Node  :: mod_pubsub:nodeId(),
	Owner :: jid())
    -> {result, []}
    %%%
    | {error, xmlel()}
    ).
purge_node(Host, Node, Owner) ->
    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx}) ->
	    Features = plugin_features(Host, Type),
	    PurgeFeature = lists:member(<<"purge-nodes">>, Features),
	    PersistentFeature = lists:member(<<"persistent-items">>, Features),
	    PersistentConfig = get_option(Options, persist_items),
	    if not PurgeFeature ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"purge-nodes">>)};
		not PersistentFeature ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"persistent-items">>)};
		not PersistentConfig ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"persistent-items">>)};
		true -> node_call(Host, Type, purge_node, [Nidx, Owner])
	    end
    end,
    Reply = [],
    case transaction(Host, Node, Action, sync_dirty) of
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
%% @todo We probably need to check that the user doing the query has the right
%% to read the items.
-spec(get_items/7 ::
    (
	Host      :: mod_pubsub:host(),
	Node      :: mod_pubsub:nodeId(),
	From      :: jid(),
	SubId     :: mod_pubsub:subId(),
	SMaxItems :: binary(),
	ItemIds   :: [mod_pubsub:itemId()],
	Rsm       :: none | rsm_in())
    -> {result, [xmlel(),...]}
    %%%
    | {error, xmlel()}
    ).
get_items(Host, Node, From, SubId, SMaxItems, ItemIds, RSM) ->
    MaxItems = if SMaxItems == <<>> ->
	    case get_max_items_node(Host) of
		undefined -> ?MAXITEMS;
		Max -> Max
	    end;
	true ->
	    case catch jlib:binary_to_integer(SMaxItems) of
		{'EXIT', _} -> {error, ?ERR_BAD_REQUEST};
		Val -> Val
	    end
    end,
    case MaxItems of
	{error, Error} ->
	    {error, Error};
	_ ->
	    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx, owners = O}) ->
		    Features = plugin_features(Host, Type),
		    RetreiveFeature = lists:member(<<"retrieve-items">>, Features),
		    PersistentFeature = lists:member(<<"persistent-items">>, Features),
		    AccessModel = get_option(Options, access_model),
		    AllowedGroups = get_option(Options, roster_groups_allowed, []),
		    if not RetreiveFeature ->
			    {error,
				extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"retrieve-items">>)};
			not PersistentFeature ->
			    {error,
				extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"persistent-items">>)};
			true ->
			    Owners = node_owners_call(Host, Type, Nidx, O),
			    {PS, RG} = get_presence_and_roster_permissions(Host, From, Owners,
				    AccessModel, AllowedGroups),
			    node_call(Host, Type, get_items,
				[Nidx, From, AccessModel, PS, RG, SubId, RSM])
		    end
	    end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, {Items, RsmOut}}} ->
		    SendItems = case ItemIds of
			[] ->
			    Items;
			_ ->
			    lists:filter(fun (#pubsub_item{itemid = {ItemId, _}}) ->
					lists:member(ItemId, ItemIds)
				end,
				Items)
		    end,
		    {result,
			[#xmlel{name = <<"pubsub">>,
				attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
				children =
				[#xmlel{name = <<"items">>, attrs = nodeAttr(Node),
					children = itemsEls(lists:sublist(SendItems, MaxItems))}
				    | jlib:rsm_encode(RsmOut)]}]};
		Error ->
		    Error
	    end
    end.

get_items(Host, Node) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    node_call(Host, Type, get_items, [Nidx, service_jid(Host), none]) 
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, {Items, _}}} -> Items;
	Error -> Error
    end.

get_item(Host, Node, ItemId) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    node_call(Host, Type, get_item, [Nidx, ItemId])
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Items}} -> Items;
	Error -> Error
    end.

get_allowed_items_call(Host, Nidx, From, Type, Options, Owners) ->
    case get_allowed_items_call(Host, Nidx, From, Type, Options, Owners, none) of
	{result, {Items, _RSM}} -> {result, Items};
	Error -> Error
    end.
get_allowed_items_call(Host, Nidx, From, Type, Options, Owners, RSM) ->
    AccessModel = get_option(Options, access_model),
    AllowedGroups = get_option(Options, roster_groups_allowed, []),
    {PS, RG} = get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups),
    node_call(Host, Type, get_items, [Nidx, From, AccessModel, PS, RG, undefined, RSM]).

get_last_item(Host, Type, Nidx, LJID) ->
    case get_cached_item(Host, Nidx) of
	undefined -> get_last_item(Host, Type, Nidx, LJID, gen_mod:db_type(serverhost(Host), ?MODULE));
	LastItem -> LastItem
    end.
get_last_item(Host, Type, Nidx, LJID, mnesia) ->
    case node_action(Host, Type, get_items, [Nidx, LJID, none]) of
	{result, {[LastItem|_], _}} -> LastItem;
	_ -> undefined
    end;
get_last_item(Host, Type, Nidx, LJID, odbc) ->
    case node_action(Host, Type, get_last_items, [Nidx, LJID, 1]) of
	{result, [LastItem]} -> LastItem;
	_ -> undefined
    end;
get_last_item(_Host, _Type, _Nidx, _LJID, _) ->
    undefined.

get_last_items(Host, Type, Nidx, LJID, Number) ->
    get_last_items(Host, Type, Nidx, LJID, Number, gen_mod:db_type(serverhost(Host), ?MODULE)).
get_last_items(Host, Type, Nidx, LJID, Number, mnesia) ->
    case node_action(Host, Type, get_items, [Nidx, LJID, none]) of
	{result, {Items, _}} -> lists:sublist(Items, Number);
	_ -> []
    end;
get_last_items(Host, Type, Nidx, LJID, Number, odbc) ->
    case node_action(Host, Type, get_last_items, [Nidx, LJID, Number]) of
	{result, Items} -> Items;
	_ -> []
    end;
get_last_items(_Host, _Type, _Nidx, _LJID, _Number, _) ->
    [].

%% @doc <p>Resend the items of a node to the user.</p>
%% @todo use cache-last-item feature
send_items(Host, Node, Nidx, Type, Options, LJID, last) ->
    case get_last_item(Host, Type, Nidx, LJID) of
	undefined ->
	    ok;
	LastItem ->
	    Stanza = items_event_stanza(Node, [LastItem]),
	    dispatch_items(Host, LJID, Node, Options, Stanza)
    end;
send_items(Host, Node, Nidx, Type, Options, LJID, Number) when Number > 0 ->
    Stanza = items_event_stanza(Node, get_last_items(Host, Type, Nidx, Number, LJID)),
    dispatch_items(Host, LJID, Node, Options, Stanza);
send_items(Host, Node, _Nidx, _Type, Options, LJID, _) ->
    Stanza = items_event_stanza(Node, []),
    dispatch_items(Host, LJID, Node, Options, Stanza).

dispatch_items({FromU, FromS, FromR} = From, {ToU, ToS, ToR} = To, Node,
	    Options, Stanza) ->
    C2SPid = case ejabberd_sm:get_session_pid(ToU, ToS, ToR) of
	ToPid when is_pid(ToPid) -> ToPid;
	_ ->
	    R = user_resource(FromU, FromS, FromR),
	    case ejabberd_sm:get_session_pid(FromU, FromS, R) of
		FromPid when is_pid(FromPid) -> FromPid;
		_ -> undefined
	    end
    end,
    if C2SPid == undefined -> ok;
	true ->
	    NotificationType = get_option(Options, notification_type, headline),
	    Message = add_message_type(Stanza, NotificationType),
	    ejabberd_c2s:send_filtered(C2SPid,
		{pep_message, <<Node/binary, "+notify">>},
		service_jid(From), jid:make(To),
		Message)
    end;
dispatch_items(From, To, _Node, Options, Stanza) ->
    NotificationType = get_option(Options, notification_type, headline),
    Message = add_message_type(Stanza, NotificationType),
    ejabberd_router:route(service_jid(From), jid:make(To), Message).

%% @doc <p>Return the list of affiliations as an XMPP response.</p>
-spec(get_affiliations/4 ::
    (
	Host    :: mod_pubsub:host(),
	Node    :: mod_pubsub:nodeId(),
	JID     :: jid(),
	Plugins :: [binary()])
    -> {result, [xmlel(),...]}
    %%%
    | {error, xmlel()}
    ).
get_affiliations(Host, Node, JID, Plugins) when is_list(Plugins) ->
    Result = lists:foldl( fun (Type, {Status, Acc}) ->
		    Features = plugin_features(Host, Type),
		    RetrieveFeature = lists:member(<<"retrieve-affiliations">>, Features),
		    if not RetrieveFeature ->
			    {{error,
				    extended_error(?ERR_FEATURE_NOT_IMPLEMENTED,
					unsupported, <<"retrieve-affiliations">>)},
				Acc};
			true ->
			    {result, Affs} = node_action(Host, Type,
				    get_entity_affiliations,
				    [Host, JID]),
			    {Status, [Affs | Acc]}
		    end
	    end,
	    {ok, []}, Plugins),
    case Result of
	{ok, Affs} ->
	    Entities = lists:flatmap(fun
			({_, none}) ->
			    [];
			({#pubsub_node{nodeid = {_, NodeId}}, Aff}) ->
			    if (Node == <<>>) or (Node == NodeId) ->
				    [#xmlel{name = <<"affiliation">>,
					    attrs = [{<<"affiliation">>, affiliation_to_string(Aff)}
						| nodeAttr(NodeId)]}];
				true ->
				    []
			    end;
			(_) ->
			    []
		    end,
		    lists:usort(lists:flatten(Affs))),
	    {result,
		[#xmlel{name = <<"pubsub">>,
			attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
			children = [#xmlel{name = <<"affiliations">>, attrs = [],
				children = Entities}]}]};
	{Error, _} ->
	    Error
    end.

-spec(get_affiliations/3 ::
    (
	Host :: mod_pubsub:host(),
	Node :: mod_pubsub:nodeId(),
	JID  :: jid())
    -> {result, [xmlel(),...]}
    %%%
    | {error, xmlel()}
    ).
get_affiliations(Host, Node, JID) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    Features = plugin_features(Host, Type),
	    RetrieveFeature = lists:member(<<"modify-affiliations">>, Features),
	    {result, Affiliation} = node_call(Host, Type, get_affiliation, [Nidx, JID]),
	    if not RetrieveFeature ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"modify-affiliations">>)};
		Affiliation /= owner ->
		    {error, ?ERR_FORBIDDEN};
		true ->
		    node_call(Host, Type, get_node_affiliations, [Nidx])
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, []}} ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	{result, {_, Affs}} ->
	    Entities = lists:flatmap(fun
			({_, none}) ->
			    [];
			({AJID, Aff}) ->
			    [#xmlel{name = <<"affiliation">>,
				    attrs = [{<<"jid">>, jid:to_string(AJID)},
					{<<"affiliation">>, affiliation_to_string(Aff)}]}]
		    end,
		    Affs),
	    {result,
		[#xmlel{name = <<"pubsub">>,
			attrs = [{<<"xmlns">>, ?NS_PUBSUB_OWNER}],
			children = [#xmlel{name = <<"affiliations">>,
				attrs = nodeAttr(Node), children = Entities}]}]};
	Error ->
	    Error
    end.

-spec(set_affiliations/4 ::
    (
	Host        :: mod_pubsub:host(),
	Node        :: mod_pubsub:nodeId(),
	From        :: jid(),
	EntitiesEls :: [xmlel()])
    -> {result, []}
    %%%
    | {error, xmlel()}
    ).
set_affiliations(Host, Node, From, EntitiesEls) ->
    Owner = jid:tolower(jid:remove_resource(From)),
    Entities = lists:foldl(fun
		(_, error) ->
		    error;
		(El, Acc) ->
		    case El of
			#xmlel{name = <<"affiliation">>, attrs = Attrs} ->
			    JID = jid:from_string(xml:get_attr_s(<<"jid">>, Attrs)),
			    Affiliation = string_to_affiliation(xml:get_attr_s(<<"affiliation">>, Attrs)),
			    if (JID == error) or (Affiliation == false) -> error;
				true -> [{jid:tolower(JID), Affiliation} | Acc]
			    end
		    end
	    end,
	    [], EntitiesEls),
    case Entities of
	error ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    Action = fun (#pubsub_node{type = Type, id = Nidx, owners = O} = N) ->
		    Owners = node_owners_call(Host, Type, Nidx, O),
		    case lists:member(Owner, Owners) of
			true ->
			    OwnerJID = jid:make(Owner),
			    FilteredEntities = case Owners of
				[Owner] -> [E || E <- Entities, element(1, E) =/= OwnerJID];
				_ -> Entities
			    end,
			    lists:foreach(fun ({JID, Affiliation}) ->
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
				end,
				FilteredEntities),
			    {result, []};
			_ ->
			    {error, ?ERR_FORBIDDEN}
		    end
	    end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Result}} -> {result, Result};
		Other -> Other
	    end
    end.

get_options(Host, Node, JID, SubId, Lang) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    case lists:member(<<"subscription-options">>, plugin_features(Host, Type)) of
		true ->
		    get_options_helper(Host, JID, Lang, Node, Nidx, SubId, Type);
		false ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"subscription-options">>)}
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, XForm}} -> {result, [XForm]};
	Error -> Error
    end.

get_options_helper(Host, JID, Lang, Node, Nidx, SubId, Type) ->
    Subscriber = string_to_ljid(JID),
    {result, Subs} = node_call(Host, Type, get_subscriptions, [Nidx, Subscriber]),
    SubIds = [Id || {Sub, Id} <- Subs, Sub == subscribed],
    case {SubId, SubIds} of
	{_, []} ->
	    {error,
		extended_error(?ERR_NOT_ACCEPTABLE, <<"not-subscribed">>)};
	{<<>>, [SID]} ->
	    read_sub(Host, Node, Nidx, Subscriber, SID, Lang);
	{<<>>, _} ->
	    {error,
		extended_error(?ERR_NOT_ACCEPTABLE, <<"subid-required">>)};
	{_, _} ->
	    ValidSubId = lists:member(SubId, SubIds),
	    if ValidSubId ->
		    read_sub(Host, Node, Nidx, Subscriber, SubId, Lang);
		true ->
		    {error,
			extended_error(?ERR_NOT_ACCEPTABLE, <<"invalid-subid">>)}
	    end
    end.

read_sub(Host, Node, Nidx, Subscriber, SubId, Lang) ->
    SubModule = subscription_plugin(Host),
    Children = case SubModule:get_subscription(Subscriber, Nidx, SubId) of
	{error, notfound} ->
	    [];
	{result, #pubsub_subscription{options = Options}} ->
	    {result, XdataEl} = SubModule:get_options_xform(Lang, Options),
	    [XdataEl]
    end,
    OptionsEl = #xmlel{name = <<"options">>,
	    attrs = [{<<"jid">>, jid:to_string(Subscriber)},
		{<<"subid">>, SubId}
		| nodeAttr(Node)],
	    children = Children},
    PubsubEl = #xmlel{name = <<"pubsub">>,
	    attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
	    children = [OptionsEl]},
    {result, PubsubEl}.

set_options(Host, Node, JID, SubId, Configuration) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    case lists:member(<<"subscription-options">>, plugin_features(Host, Type)) of
		true ->
		    set_options_helper(Host, Configuration, JID, Nidx, SubId, Type);
		false ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"subscription-options">>)}
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, Result}} -> {result, Result};
	Error -> Error
    end.

set_options_helper(Host, Configuration, JID, Nidx, SubId, Type) ->
    SubModule = subscription_plugin(Host),
    SubOpts = case SubModule:parse_options_xform(Configuration) of
	{result, GoodSubOpts} -> GoodSubOpts;
	_ -> invalid
    end,
    Subscriber = string_to_ljid(JID),
    {result, Subs} = node_call(Host, Type, get_subscriptions, [Nidx, Subscriber]),
    SubIds = [Id || {Sub, Id} <- Subs, Sub == subscribed],
    case {SubId, SubIds} of
	{_, []} ->
	    {error,
		extended_error(?ERR_NOT_ACCEPTABLE, <<"not-subscribed">>)};
	{<<>>, [SID]} ->
	    write_sub(Host, Nidx, Subscriber, SID, SubOpts);
	{<<>>, _} ->
	    {error,
		extended_error(?ERR_NOT_ACCEPTABLE, <<"subid-required">>)};
	{_, _} ->
	    write_sub(Host, Nidx, Subscriber, SubId, SubOpts)
    end.

write_sub(_Host, _Nidx, _Subscriber, _SubId, invalid) ->
    {error,
	extended_error(?ERR_BAD_REQUEST, <<"invalid-options">>)};
write_sub(_Host, _Nidx, _Subscriber, _SubId, []) ->
    {result, []};
write_sub(Host, Nidx, Subscriber, SubId, Options) ->
    SubModule = subscription_plugin(Host),
    case SubModule:set_subscription(Subscriber, Nidx, SubId, Options) of
	{result, _} -> {result, []};
	{error, _} -> {error, extended_error(?ERR_NOT_ACCEPTABLE, <<"invalid-subid">>)}
    end.

%% @spec (Host, Node, JID, Plugins) -> {error, Reason} | {result, Response}
%%         Host = host()
%%         Node = pubsubNode()
%%         JID = jid()
%%         Plugins = [Plugin::string()]
%%         Reason = stanzaError()
%%         Response = [pubsubIQResponse()]
%% @doc <p>Return the list of subscriptions as an XMPP response.</p>
get_subscriptions(Host, Node, JID, Plugins) when is_list(Plugins) ->
    Result = lists:foldl(fun (Type, {Status, Acc}) ->
		    Features = plugin_features(Host, Type),
		    RetrieveFeature = lists:member(<<"retrieve-subscriptions">>, Features),
		    if not RetrieveFeature ->
			    {{error,
				    extended_error(?ERR_FEATURE_NOT_IMPLEMENTED,
					unsupported, <<"retrieve-subscriptions">>)},
				Acc};
			true ->
			    Subscriber = jid:remove_resource(JID),
			    {result, Subs} = node_action(Host, Type,
				    get_entity_subscriptions,
				    [Host, Subscriber]),
			    {Status, [Subs | Acc]}
		    end
	    end,
	    {ok, []}, Plugins),
    case Result of
	{ok, Subs} ->
	    Entities = lists:flatmap(fun
			({_, none}) ->
			    [];
			({#pubsub_node{nodeid = {_, SubsNode}}, Sub}) ->
			    case Node of
				<<>> ->
				    [#xmlel{name = <<"subscription">>,
					    attrs =
					    [{<<"subscription">>, subscription_to_string(Sub)}
						| nodeAttr(SubsNode)]}];
				SubsNode ->
				    [#xmlel{name = <<"subscription">>,
					    attrs =
					    [{<<"subscription">>, subscription_to_string(Sub)}]}];
				_ ->
				    []
			    end;
			({_, none, _}) ->
			    [];
			({#pubsub_node{nodeid = {_, SubsNode}}, Sub, SubId, SubJID}) ->
			    case Node of
				<<>> ->
				    [#xmlel{name = <<"subscription">>,
					    attrs =
					    [{<<"jid">>, jid:to_string(SubJID)},
						{<<"subid">>, SubId},
						{<<"subscription">>, subscription_to_string(Sub)}
						| nodeAttr(SubsNode)]}];
				SubsNode ->
				    [#xmlel{name = <<"subscription">>,
					    attrs =
					    [{<<"jid">>, jid:to_string(SubJID)},
						{<<"subid">>, SubId},
						{<<"subscription">>, subscription_to_string(Sub)}]}];
				_ ->
				    []
			    end;
			({#pubsub_node{nodeid = {_, SubsNode}}, Sub, SubJID}) ->
			    case Node of
				<<>> ->
				    [#xmlel{name = <<"subscription">>,
					    attrs =
					    [{<<"jid">>, jid:to_string(SubJID)},
						{<<"subscription">>, subscription_to_string(Sub)}
						| nodeAttr(SubsNode)]}];
				SubsNode ->
				    [#xmlel{name = <<"subscription">>,
					    attrs =
					    [{<<"jid">>, jid:to_string(SubJID)},
						{<<"subscription">>, subscription_to_string(Sub)}]}];
				_ ->
				    []
			    end
		    end,
		    lists:usort(lists:flatten(Subs))),
	    {result,
		[#xmlel{name = <<"pubsub">>,
			attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
			children = [#xmlel{name = <<"subscriptions">>, attrs = [],
				children = Entities}]}]};
	{Error, _} ->
	    Error
    end.

get_subscriptions(Host, Node, JID) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
	    Features = plugin_features(Host, Type),
	    RetrieveFeature = lists:member(<<"manage-subscriptions">>, Features),
	    {result, Affiliation} = node_call(Host, Type, get_affiliation, [Nidx, JID]),
	    if not RetrieveFeature ->
		    {error,
			extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, <<"manage-subscriptions">>)};
		Affiliation /= owner ->
		    {error, ?ERR_FORBIDDEN};
		true ->
		    node_call(Host, Type, get_node_subscriptions, [Nidx])
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Subs}} ->
	    Entities = lists:flatmap(fun
			({_, none}) ->
			    [];
			({_, pending, _}) ->
			    [];
			({AJID, Sub}) ->
			    [#xmlel{name = <<"subscription">>,
				    attrs =
				    [{<<"jid">>, jid:to_string(AJID)},
					{<<"subscription">>, subscription_to_string(Sub)}]}];
			({AJID, Sub, SubId}) ->
			    [#xmlel{name = <<"subscription">>,
				    attrs =
				    [{<<"jid">>, jid:to_string(AJID)},
					{<<"subscription">>, subscription_to_string(Sub)},
					{<<"subid">>, SubId}]}]
		    end,
		    Subs),
	    {result,
		[#xmlel{name = <<"pubsub">>,
			attrs = [{<<"xmlns">>, ?NS_PUBSUB_OWNER}],
			children = [#xmlel{name = <<"subscriptions">>,
				attrs = nodeAttr(Node),
				children = Entities}]}]};
	Error ->
	    Error
    end.

get_subscriptions_for_send_last(Host, PType, mnesia, JID, LJID, BJID) ->
    {result, Subs} = node_action(Host, PType,
	    get_entity_subscriptions,
	    [Host, JID]),
    [{Node, Sub, SubId, SubJID}
	|| {Node, Sub, SubId, SubJID} <- Subs,
	    Sub =:= subscribed, (SubJID == LJID) or (SubJID == BJID),
	    match_option(Node, send_last_published_item, on_sub_and_presence)];
get_subscriptions_for_send_last(Host, PType, odbc, JID, LJID, BJID) ->
    case catch node_action(Host, PType,
	    get_entity_subscriptions_for_send_last,
	    [Host, JID])
    of
	{result, Subs} ->
	    [{Node, Sub, SubId, SubJID}
		|| {Node, Sub, SubId, SubJID} <- Subs,
		    Sub =:= subscribed, (SubJID == LJID) or (SubJID == BJID)];
	_ ->
	    []
    end;
get_subscriptions_for_send_last(_Host, _PType, _, _JID, _LJID, _BJID) ->
    [].

set_subscriptions(Host, Node, From, EntitiesEls) ->
    Owner = jid:tolower(jid:remove_resource(From)),
    Entities = lists:foldl(fun
		(_, error) ->
		    error;
		(El, Acc) ->
		    case El of
			#xmlel{name = <<"subscription">>, attrs = Attrs} ->
			    JID = jid:from_string(xml:get_attr_s(<<"jid">>, Attrs)),
			    Sub = string_to_subscription(xml:get_attr_s(<<"subscription">>, Attrs)),
			    SubId = xml:get_attr_s(<<"subid">>, Attrs),
			    if (JID == error) or (Sub == false) -> error;
				true -> [{jid:tolower(JID), Sub, SubId} | Acc]
			    end
		    end
	    end,
	    [], EntitiesEls),
    case Entities of
	error ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    Notify = fun (JID, Sub, _SubId) ->
		    Stanza = #xmlel{name = <<"message">>, attrs = [],
			    children =
			    [#xmlel{name = <<"pubsub">>,
				    attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
				    children =
				    [#xmlel{name = <<"subscription">>,
					    attrs = [{<<"jid">>, jid:to_string(JID)},
						{<<"subscription">>, subscription_to_string(Sub)}
						| nodeAttr(Node)]}]}]},
		    ejabberd_router:route(service_jid(Host), jid:make(JID), Stanza)
	    end,
	    Action = fun (#pubsub_node{type = Type, id = Nidx, owners = O}) ->
		    Owners = node_owners_call(Host, Type, Nidx, O),
		    case lists:member(Owner, Owners) of
			true ->
			    Result = lists:foldl(fun ({JID, Sub, SubId}, Acc) ->
					    case
						node_call(Host, Type,
						    set_subscriptions,
						    [Nidx, JID, Sub, SubId])
					    of
						{error, Err} ->
						    [{error, Err} | Acc];
						_ ->
						    Notify(JID, Sub, SubId),
						    Acc
					    end
				    end,
				    [], Entities),
			    case Result of
				[] -> {result, []};
				_ -> {error, ?ERR_NOT_ACCEPTABLE}
			    end;
			_ ->
			    {error, ?ERR_FORBIDDEN}
		    end
	    end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Result}} -> {result, Result};
		Other -> Other
	    end
    end.

-spec(get_presence_and_roster_permissions/5 ::
    (
	Host          :: mod_pubsub:host(),
	From          :: ljid(),
	Owners        :: [ljid(),...],
	AccessModel   :: mod_pubsub:accessModel(),
	AllowedGroups :: [binary()])
    -> {PresenceSubscription::boolean(), RosterGroup::boolean()}
    ).
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

get_roster_info(_, _, {<<>>, <<>>, _}, _) ->
    {false, false};
get_roster_info(OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, _}, AllowedGroups) ->
    LJID = {SubscriberUser, SubscriberServer, <<>>},
    {Subscription, Groups} = ejabberd_hooks:run_fold(roster_get_jid_info,
	    OwnerServer, {none, []},
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

string_to_affiliation(<<"owner">>) -> owner;
string_to_affiliation(<<"publisher">>) -> publisher;
string_to_affiliation(<<"publish-only">>) -> publish_only;
string_to_affiliation(<<"member">>) -> member;
string_to_affiliation(<<"outcast">>) -> outcast;
string_to_affiliation(<<"none">>) -> none;
string_to_affiliation(_) -> false.

string_to_subscription(<<"subscribed">>) -> subscribed;
string_to_subscription(<<"pending">>) -> pending;
string_to_subscription(<<"unconfigured">>) -> unconfigured;
string_to_subscription(<<"none">>) -> none;
string_to_subscription(_) -> false.

affiliation_to_string(owner) -> <<"owner">>;
affiliation_to_string(publisher) -> <<"publisher">>;
affiliation_to_string(publish_only) -> <<"publish-only">>;
affiliation_to_string(member) -> <<"member">>;
affiliation_to_string(outcast) -> <<"outcast">>;
affiliation_to_string(_) -> <<"none">>.

subscription_to_string(subscribed) -> <<"subscribed">>;
subscription_to_string(pending) -> <<"pending">>;
subscription_to_string(unconfigured) -> <<"unconfigured">>;
subscription_to_string(_) -> <<"none">>.

-spec(service_jid/1 ::
    (
	Host :: mod_pubsub:host())
    -> jid()
    ).
service_jid(Host) ->
    case Host of
	{U, S, _} -> {jid, U, S, <<>>, U, S, <<>>};
	_ -> {jid, <<>>, Host, <<>>, <<>>, Host, <<>>}
    end.

%% @spec (LJID, NotifyType, Depth, NodeOptions, SubOptions) -> boolean()
%%        LJID = jid()
%%        NotifyType = items | nodes
%%        Depth = integer()
%%        NodeOptions = [{atom(), term()}]
%%        SubOptions = [{atom(), term()}]
%% @doc <p>Check if a notification must be delivered or not based on
%% node and subscription options.</p>
is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) ->
    sub_to_deliver(LJID, NotifyType, Depth, SubOptions)
    andalso node_to_deliver(LJID, NodeOptions).

sub_to_deliver(_LJID, NotifyType, Depth, SubOptions) ->
    lists:all(fun (Option) ->
		sub_option_can_deliver(NotifyType, Depth, Option)
	end,
	SubOptions).

node_to_deliver(LJID, NodeOptions) ->
    presence_can_deliver(LJID, get_option(NodeOptions, presence_based_delivery)).

sub_option_can_deliver(items, _, {subscription_type, nodes}) -> false;
sub_option_can_deliver(nodes, _, {subscription_type, items}) -> false;
sub_option_can_deliver(_, _, {subscription_depth, all}) -> true;
sub_option_can_deliver(_, Depth, {subscription_depth, D}) -> Depth =< D;
sub_option_can_deliver(_, _, {deliver, false}) -> false;
sub_option_can_deliver(_, _, {expire, When}) -> p1_time_compat:timestamp() < When;
sub_option_can_deliver(_, _, _) -> true.

-spec(presence_can_deliver/2 ::
    (
	Entity :: ljid(),
	_      :: boolean())
    -> boolean()
    ).
presence_can_deliver(_, false) ->
    true;
presence_can_deliver({User, Server, Resource}, true) ->
    case mnesia:dirty_match_object({session, '_', '_', {User, Server}, '_', '_'}) of
	[] ->
	    false;
	Ss ->
	    lists:foldl(fun
		    (_, true) ->
			true;
		    ({session, _, _, _, undefined, _}, _Acc) ->
			false;
		    ({session, {_, _, R}, _, _, _Priority, _}, _Acc) ->
			case Resource of
			    <<>> -> true;
			    R -> true;
			    _ -> false
			end
		end,
		false, Ss)
    end.

-spec(state_can_deliver/2 ::
    (
	Entity::ljid(),
	SubOptions :: mod_pubsub:subOptions() | [])
    -> [ljid()]
    ).
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

-spec(get_resource_state/3 ::
    (
	Entity     :: ljid(),
	ShowValues :: [binary()],
	JIDs       :: [ljid()])
    -> [ljid()]
    ).
get_resource_state({U, S, R}, ShowValues, JIDs) ->
    case ejabberd_sm:get_session_pid(U, S, R) of
	none ->
	    %% If no PID, item can be delivered
	    lists:append([{U, S, R}], JIDs);
	Pid ->
	    Show = case ejabberd_c2s:get_presence(Pid) of
		{_, _, <<"available">>, _} -> <<"online">>;
		{_, _, State, _} -> State
	    end,
	    case lists:member(Show, ShowValues) of
		%% If yes, item can be delivered
		true -> lists:append([{U, S, R}], JIDs);
		%% If no, item can't be delivered
		false -> JIDs
	    end
    end.

-spec(payload_xmlelements/1 ::
    (
	Payload :: mod_pubsub:payload())
    -> Count :: non_neg_integer()
    ).
payload_xmlelements(Payload) ->
    payload_xmlelements(Payload, 0).

payload_xmlelements([], Count) -> Count;
payload_xmlelements([#xmlel{} | Tail], Count) ->
    payload_xmlelements(Tail, Count + 1);
payload_xmlelements([_ | Tail], Count) ->
    payload_xmlelements(Tail, Count).

items_event_stanza(Node, Items) ->
    MoreEls = case Items of
	[LastItem] ->
	    {ModifNow, ModifUSR} = LastItem#pubsub_item.modification,
	    DateTime = calendar:now_to_datetime(ModifNow),
	    {T_string, Tz_string} = jlib:timestamp_to_iso(DateTime, utc),
	    [#xmlel{name = <<"delay">>, attrs = [{<<"xmlns">>, ?NS_DELAY},
			{<<"from">>, jid:to_string(ModifUSR)},
			{<<"stamp">>, <<T_string/binary, Tz_string/binary>>}],
		    children = [{xmlcdata, <<>>}]}];
	_ ->
	    []
    end,
    event_stanza_with_els([#xmlel{name = <<"items">>,
		attrs = [{<<"type">>, <<"headline">>} | nodeAttr(Node)],
		children = itemsEls(Items)}],
	MoreEls).

event_stanza(Els) ->
    event_stanza_with_els(Els, []).
event_stanza_with_els(Els, MoreEls) ->
    #xmlel{name = <<"message">>, attrs = [],
	children = [#xmlel{name = <<"event">>,
		attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}],
		children = Els}
	    | MoreEls]}.

event_stanza(Event, EvAttr) ->
    event_stanza_with_els([#xmlel{name = Event, attrs = EvAttr}], []).

%%%%%% broadcast functions

broadcast_publish_item(Host, Node, Nidx, Type, NodeOptions, ItemId, From, Payload, Removed) ->
    case get_collection_subscriptions(Host, Node) of
	SubsByDepth when is_list(SubsByDepth) ->
	    Content = case get_option(NodeOptions, deliver_payloads) of
		true -> Payload;
		false -> []
	    end,
	    Stanza = event_stanza(
		    [#xmlel{name = <<"items">>, attrs = nodeAttr(Node),
			    children = [#xmlel{name = <<"item">>, attrs = itemAttr(ItemId),
				    children = Content}]}]),
	    broadcast_stanza(Host, From, Node, Nidx, Type,
		NodeOptions, SubsByDepth, items, Stanza, true),
	    case Removed of
		[] ->
		    ok;
		_ ->
		    case get_option(NodeOptions, notify_retract) of
			true ->
			    RetractStanza = event_stanza(
				    [#xmlel{name = <<"items">>, attrs = nodeAttr(Node),
					    children = [#xmlel{name = <<"retract">>, attrs = itemAttr(RId)} || RId <- Removed]}]),
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

broadcast_retract_items(Host, Node, Nidx, Type, NodeOptions, ItemIds) ->
    broadcast_retract_items(Host, Node, Nidx, Type, NodeOptions, ItemIds, false).
broadcast_retract_items(_Host, _Node, _Nidx, _Type, _NodeOptions, [], _ForceNotify) ->
    {result, false};
broadcast_retract_items(Host, Node, Nidx, Type, NodeOptions, ItemIds, ForceNotify) ->
    case (get_option(NodeOptions, notify_retract) or ForceNotify) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		SubsByDepth when is_list(SubsByDepth) ->
		    Stanza = event_stanza(
			    [#xmlel{name = <<"items">>, attrs = nodeAttr(Node),
				    children = [#xmlel{name = <<"retract">>, attrs = itemAttr(ItemId)} || ItemId <- ItemIds]}]),
		    broadcast_stanza(Host, Node, Nidx, Type,
			NodeOptions, SubsByDepth, items, Stanza, true),
		    {result, true};
		_ ->
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_purge_node(Host, Node, Nidx, Type, NodeOptions) ->
    case get_option(NodeOptions, notify_retract) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		SubsByDepth when is_list(SubsByDepth) ->
		    Stanza = event_stanza(
			    [#xmlel{name = <<"purge">>, attrs = nodeAttr(Node)}]),
		    broadcast_stanza(Host, Node, Nidx, Type,
			NodeOptions, SubsByDepth, nodes, Stanza, false),
		    {result, true};
		_ ->
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_removed_node(Host, Node, Nidx, Type, NodeOptions, SubsByDepth) ->
    case get_option(NodeOptions, notify_delete) of
	true ->
	    case SubsByDepth of
		[] ->
		    {result, false};
		_ ->
		    Stanza = event_stanza(
			    [#xmlel{name = <<"delete">>, attrs = nodeAttr(Node)}]),
		    broadcast_stanza(Host, Node, Nidx, Type,
			NodeOptions, SubsByDepth, nodes, Stanza, false),
		    {result, true}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_created_node(_, _, _, _, _, []) ->
    {result, false};
broadcast_created_node(Host, Node, Nidx, Type, NodeOptions, SubsByDepth) ->
    Stanza = event_stanza([#xmlel{name = <<"create">>, attrs = nodeAttr(Node)}]),
    broadcast_stanza(Host, Node, Nidx, Type, NodeOptions, SubsByDepth, nodes, Stanza, true),
    {result, true}.

broadcast_config_notification(Host, Node, Nidx, Type, NodeOptions, Lang) ->
    case get_option(NodeOptions, notify_config) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		SubsByDepth when is_list(SubsByDepth) ->
		    Content = case get_option(NodeOptions, deliver_payloads) of
			true ->
			    [#xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
				    children = get_configure_xfields(Type, NodeOptions, Lang, [])}];
			false ->
			    []
		    end,
		    Stanza = event_stanza(
			    [#xmlel{name = <<"configuration">>, attrs = nodeAttr(Node), children = Content}]),
		    broadcast_stanza(Host, Node, Nidx, Type,
			NodeOptions, SubsByDepth, nodes, Stanza, false),
		    {result, true};
		_ ->
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

get_collection_subscriptions(Host, Node) ->
    Action = fun() ->
	    {result, get_node_subs_by_depth(Host, Node, service_jid(Host))}
    end,
    case transaction(Host, Action, sync_dirty) of
	{result, CollSubs} -> CollSubs;
	_ -> []
    end.

get_node_subs_by_depth(Host, Node, From) ->
    ParentTree = tree_call(Host, get_parentnodes_tree, [Host, Node, From]),
    [{Depth, [{N, get_node_subs(Host, N)} || N <- Nodes]} || {Depth, Nodes} <- ParentTree].

get_node_subs(Host, #pubsub_node{type = Type, id = Nidx}) ->
    case node_call(Host, Type, get_node_subscriptions, [Nidx]) of
	{result, Subs} -> get_options_for_subs(Host, Nidx, Subs);
	Other -> Other
    end.

get_options_for_subs(Host, Nidx, Subs) ->
    SubModule = subscription_plugin(Host),
    lists:foldl(fun({JID, subscribed, SubID}, Acc) ->
		case SubModule:get_subscription(JID, Nidx, SubID) of
		    #pubsub_subscription{options = Options} -> [{JID, SubID, Options} | Acc];
		    {error, notfound} -> [{JID, SubID, []} | Acc]
		end;
	    (_, Acc) ->
		Acc
	end, [], Subs).

broadcast_stanza(Host, _Node, _Nidx, _Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    NotificationType = get_option(NodeOptions, notification_type, headline),
    BroadcastAll = get_option(NodeOptions, broadcast_all_resources), %% XXX this is not standard, but usefull
    From = service_jid(Host),
    Stanza = add_message_type(BaseStanza, NotificationType),
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
			    ejabberd_router:route(From, jid:make(To), StanzaToSend)
		    end, LJIDs)
	end, SubIDsByJID).

broadcast_stanza({LUser, LServer, LResource}, Publisher, Node, Nidx, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    broadcast_stanza({LUser, LServer, LResource}, Node, Nidx, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM),
    %% Handles implicit presence subscriptions
    SenderResource = user_resource(LUser, LServer, LResource),
    case ejabberd_sm:get_session_pid(LUser, LServer, SenderResource) of
	C2SPid when is_pid(C2SPid) ->
	    NotificationType = get_option(NodeOptions, notification_type, headline),
	    Stanza = add_message_type(BaseStanza, NotificationType),
	    %% set the from address on the notification to the bare JID of the account owner
	    %% Also, add "replyto" if entity has presence subscription to the account owner
	    %% See XEP-0163 1.1 section 4.3.1
	    ejabberd_c2s:broadcast(C2SPid,
		{pep_message, <<((Node))/binary, "+notify">>},
		_Sender = jid:make(LUser, LServer, <<"">>),
		_StanzaToSend = add_extended_headers(Stanza,
		    _ReplyTo = extended_headers([jid:to_string(Publisher)])));
	_ ->
	    ?DEBUG("~p@~p has no session; can't deliver ~p to contacts", [LUser, LServer, BaseStanza])
    end;
broadcast_stanza(Host, _Publisher, Node, Nidx, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    broadcast_stanza(Host, Node, Nidx, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM).

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

user_resources(User, Server) ->
    ejabberd_sm:get_user_resources(User, Server).

user_resource(User, Server, <<>>) ->
    case user_resources(User, Server) of
	[R | _] -> R;
	_ -> <<>>
    end;
user_resource(_, _, Resource) ->
    Resource.

%%%%%%% Configuration handling

get_configure(Host, ServerHost, Node, From, Lang) ->
    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx}) ->
	    case node_call(Host, Type, get_affiliation, [Nidx, From]) of
		{result, owner} ->
		    Groups = ejabberd_hooks:run_fold(roster_groups, ServerHost, [], [ServerHost]),
		    {result,
			[#xmlel{name = <<"pubsub">>,
				attrs = [{<<"xmlns">>, ?NS_PUBSUB_OWNER}],
				children =
				[#xmlel{name = <<"configure">>,
					attrs = nodeAttr(Node),
					children =
					[#xmlel{name = <<"x">>,
						attrs = [{<<"xmlns">>, ?NS_XDATA},
						    {<<"type">>, <<"form">>}],
						children =
						get_configure_xfields(Type, Options, Lang, Groups)}]}]}]};
		_ ->
		    {error, ?ERR_FORBIDDEN}
	    end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
    end.

get_default(Host, Node, _From, Lang) ->
    Type = select_type(Host, Host, Node),
    Options = node_options(Host, Type),
    {result,
	[#xmlel{name = <<"pubsub">>,
		attrs = [{<<"xmlns">>, ?NS_PUBSUB_OWNER}],
		children =
		[#xmlel{name = <<"default">>, attrs = [],
			children =
			[#xmlel{name = <<"x">>,
				attrs = [{<<"xmlns">>, ?NS_XDATA},
				    {<<"type">>, <<"form">>}],
				children = get_configure_xfields(Type, Options, Lang, [])}]}]}]}.

match_option(Node, Var, Val) when is_record(Node, pubsub_node) ->
    match_option(Node#pubsub_node.options, Var, Val);
match_option(Options, Var, Val) when is_list(Options) ->
    get_option(Options, Var) == Val;
match_option(_, _, _) ->
    false.

get_option([], _) -> false;
get_option(Options, Var) -> get_option(Options, Var, false).

get_option(Options, Var, Def) ->
    case lists:keysearch(Var, 1, Options) of
	{value, {_Val, Ret}} -> Ret;
	_ -> Def
    end.

node_options(Host, Type) ->
    case config(serverhost(Host), default_node_config) of
	undefined -> node_plugin_options(Host, Type);
	[] -> node_plugin_options(Host, Type);
	Config -> Config
    end.
node_plugin_options(Host, Type) ->
    Module = plugin(Host, Type),
    case catch Module:options() of
	{'EXIT', {undef, _}} ->
	    DefaultModule = plugin(Host, ?STDNODE),
	    DefaultModule:options();
	Result ->
	    Result
    end.
filter_node_options(Options) ->
    lists:foldl(fun({Key, Val}, Acc) ->
		DefaultValue = proplists:get_value(Key, Options, Val),
		[{Key, DefaultValue}|Acc]
	end, [], node_flat:options()).

node_owners_action(Host, Type, Nidx, []) ->
    case gen_mod:db_type(serverhost(Host), ?MODULE) of
	odbc ->
	    case node_action(Host, Type, get_node_affiliations, [Nidx]) of
		{result, Affs} -> [LJID || {LJID, Aff} <- Affs, Aff =:= owner];
		_ -> []
	    end;
	_ ->
	    []
    end;
node_owners_action(_Host, _Type, _Nidx, Owners) ->
    Owners.

node_owners_call(Host, Type, Nidx, []) ->
    case gen_mod:db_type(serverhost(Host), ?MODULE) of
	odbc ->
	    case node_call(Host, Type, get_node_affiliations, [Nidx]) of
		{result, Affs} -> [LJID || {LJID, Aff} <- Affs, Aff =:= owner];
		_ -> []
	    end;
	_ ->
	    []
    end;
node_owners_call(_Host, _Type, _Nidx, Owners) ->
    Owners.

%% @spec (Host, Options) -> MaxItems
%%         Host = host()
%%         Options = [Option]
%%         Option = {Key::atom(), Value::term()}
%%         MaxItems = integer() | unlimited
%% @doc <p>Return the maximum number of items for a given node.</p>
%% <p>Unlimited means that there is no limit in the number of items that can
%% be stored.</p>
%% @todo In practice, the current data structure means that we cannot manage
%% millions of items on a given node. This should be addressed in a new
%% version.
max_items(Host, Options) ->
    case get_option(Options, persist_items) of
	true ->
	    case get_option(Options, max_items) of
		I when is_integer(I), I < 0 -> 0;
		I when is_integer(I) -> I;
		_ -> ?MAXITEMS
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

-define(BOOL_CONFIG_FIELD(Label, Var),
    ?BOOLXFIELD(Label,
	<<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
	(get_option(Options, Var)))).

-define(STRING_CONFIG_FIELD(Label, Var),
    ?STRINGXFIELD(Label,
	<<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
	(get_option(Options, Var, <<>>)))).

-define(INTEGER_CONFIG_FIELD(Label, Var),
    ?STRINGXFIELD(Label,
	<<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
	(jlib:integer_to_binary(get_option(Options, Var))))).

-define(JLIST_CONFIG_FIELD(Label, Var, Opts),
    ?LISTXFIELD(Label,
	<<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
	(jid:to_string(get_option(Options, Var))),
	[jid:to_string(O) || O <- Opts])).

-define(ALIST_CONFIG_FIELD(Label, Var, Opts),
    ?LISTXFIELD(Label,
	<<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
	(atom_to_binary(get_option(Options, Var), latin1)),
	[atom_to_binary(O, latin1) || O <- Opts])).

-define(LISTM_CONFIG_FIELD(Label, Var, Opts),
    ?LISTMXFIELD(Label,
	<<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
	(get_option(Options, Var)), Opts)).

-define(NLIST_CONFIG_FIELD(Label, Var),
    ?STRINGMXFIELD(Label,
	<<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
	get_option(Options, Var, []))).

get_configure_xfields(_Type, Options, Lang, Groups) ->
    [?XFIELD(<<"hidden">>, <<>>, <<"FORM_TYPE">>, (?NS_PUBSUB_NODE_CONFIG)),
	?BOOL_CONFIG_FIELD(<<"Deliver payloads with event notifications">>,
	    deliver_payloads),
	?BOOL_CONFIG_FIELD(<<"Deliver event notifications">>,
	    deliver_notifications),
	?BOOL_CONFIG_FIELD(<<"Notify subscribers when the node configuration changes">>,
	    notify_config),
	?BOOL_CONFIG_FIELD(<<"Notify subscribers when the node is deleted">>,
	    notify_delete),
	?BOOL_CONFIG_FIELD(<<"Notify subscribers when items are removed from the node">>,
	    notify_retract),
	?BOOL_CONFIG_FIELD(<<"Persist items to storage">>,
	    persist_items),
	?STRING_CONFIG_FIELD(<<"A friendly name for the node">>,
	    title),
	?INTEGER_CONFIG_FIELD(<<"Max # of items to persist">>,
	    max_items),
	?BOOL_CONFIG_FIELD(<<"Whether to allow subscriptions">>,
	    subscribe),
	?ALIST_CONFIG_FIELD(<<"Specify the access model">>,
	    access_model, [open, authorize, presence, roster, whitelist]),
	?LISTM_CONFIG_FIELD(<<"Roster groups allowed to subscribe">>,
	    roster_groups_allowed, Groups),
	?ALIST_CONFIG_FIELD(<<"Specify the publisher model">>,
	    publish_model, [publishers, subscribers, open]),
	?BOOL_CONFIG_FIELD(<<"Purge all items when the relevant publisher goes offline">>,
	    purge_offline),
	?ALIST_CONFIG_FIELD(<<"Specify the event message type">>,
	    notification_type, [headline, normal]),
	?INTEGER_CONFIG_FIELD(<<"Max payload size in bytes">>,
	    max_payload_size),
	?ALIST_CONFIG_FIELD(<<"When to send the last published item">>,
	    send_last_published_item, [never, on_sub, on_sub_and_presence]),
	?BOOL_CONFIG_FIELD(<<"Only deliver notifications to available users">>,
	    presence_based_delivery),
	?NLIST_CONFIG_FIELD(<<"The collections with which a node is affiliated">>,
	    collection)].

%%<p>There are several reasons why the node configuration request might fail:</p>
%%<ul>
%%<li>The service does not support node configuration.</li>
%%<li>The requesting entity does not have sufficient privileges to configure the node.</li>
%%<li>The request did not specify a node.</li>
%%<li>The node has no configuration options.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
set_configure(Host, Node, From, Els, Lang) ->
    case xml:remove_cdata(Els) of
	[#xmlel{name = <<"x">>} = XEl] ->
	    case {xml:get_tag_attr_s(<<"xmlns">>, XEl), xml:get_tag_attr_s(<<"type">>, XEl)} of
		{?NS_XDATA, <<"cancel">>} ->
		    {result, []};
		{?NS_XDATA, <<"submit">>} ->
		    Action = fun (#pubsub_node{options = Options, type = Type, id = Nidx} = N) ->
			    case node_call(Host, Type, get_affiliation, [Nidx, From]) of
				{result, owner} ->
				    case jlib:parse_xdata_submit(XEl) of
					invalid ->
					    {error, ?ERR_BAD_REQUEST};
					XData ->
					    OldOpts = case Options of
						[] -> node_options(Host, Type);
						_ -> Options
					    end,
					    case set_xoption(Host, XData, OldOpts) of
						NewOpts when is_list(NewOpts) ->
						    case tree_call(Host,
							    set_node,
							    [N#pubsub_node{options = NewOpts}])
						    of
							ok -> {result, ok};
							Err -> Err
						    end;
						Error ->
						    Error
					    end
				    end;
				_ ->
				    {error, ?ERR_FORBIDDEN}
			    end
		    end,
		    case transaction(Host, Node, Action, transaction) of
			{result, {TNode, ok}} ->
			    Nidx = TNode#pubsub_node.id,
			    Type = TNode#pubsub_node.type,
			    Options = TNode#pubsub_node.options,
			    broadcast_config_notification(Host, Node, Nidx, Type, Options, Lang),
			    {result, []};
			Other ->
			    Other
		    end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end;
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end.

add_opt(Key, Value, Opts) ->
    [{Key, Value} | lists:keydelete(Key, 1, Opts)].

-define(SET_BOOL_XOPT(Opt, Val),
    BoolVal = case Val of
	<<"0">> -> false;
	<<"1">> -> true;
	<<"false">> -> false;
	<<"true">> -> true;
	_ -> error
    end,
    case BoolVal of
	error -> {error, ?ERR_NOT_ACCEPTABLE};
	_ -> set_xoption(Host, Opts, add_opt(Opt, BoolVal, NewOpts))
    end).

-define(SET_STRING_XOPT(Opt, Val),
    set_xoption(Host, Opts, add_opt(Opt, Val, NewOpts))).

-define(SET_INTEGER_XOPT(Opt, Val, Min, Max),
    case catch jlib:binary_to_integer(Val) of
	IVal when is_integer(IVal), IVal >= Min ->
	    if (Max =:= undefined) orelse (IVal =< Max) ->
		set_xoption(Host, Opts, add_opt(Opt, IVal, NewOpts));
	       true ->
		{error, ?ERR_NOT_ACCEPTABLE}
	    end;
	_ ->
	    {error, ?ERR_NOT_ACCEPTABLE}
    end).

-define(SET_ALIST_XOPT(Opt, Val, Vals),
    case lists:member(Val, [atom_to_binary(V, latin1) || V <- Vals]) of
	true ->
	    set_xoption(Host, Opts, add_opt(Opt, jlib:binary_to_atom(Val), NewOpts));
	false ->
	    {error, ?ERR_NOT_ACCEPTABLE}
    end).

-define(SET_LIST_XOPT(Opt, Val),
    set_xoption(Host, Opts, add_opt(Opt, Val, NewOpts))).

set_xoption(_Host, [], NewOpts) -> NewOpts;
set_xoption(Host, [{<<"FORM_TYPE">>, _} | Opts], NewOpts) ->
    set_xoption(Host, Opts, NewOpts);
set_xoption(Host, [{<<"pubsub#roster_groups_allowed">>, Value} | Opts], NewOpts) ->
    ?SET_LIST_XOPT(roster_groups_allowed, Value);
set_xoption(Host, [{<<"pubsub#deliver_payloads">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_payloads, Val);
set_xoption(Host, [{<<"pubsub#deliver_notifications">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_notifications, Val);
set_xoption(Host, [{<<"pubsub#notify_config">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_config, Val);
set_xoption(Host, [{<<"pubsub#notify_delete">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_delete, Val);
set_xoption(Host, [{<<"pubsub#notify_retract">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_retract, Val);
set_xoption(Host, [{<<"pubsub#persist_items">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(persist_items, Val);
set_xoption(Host, [{<<"pubsub#max_items">>, [Val]} | Opts], NewOpts) ->
    MaxItems = get_max_items_node(Host),
    ?SET_INTEGER_XOPT(max_items, Val, 0, MaxItems);
set_xoption(Host, [{<<"pubsub#subscribe">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(subscribe, Val);
set_xoption(Host, [{<<"pubsub#access_model">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(access_model, Val, [open, authorize, presence, roster, whitelist]);
set_xoption(Host, [{<<"pubsub#publish_model">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(publish_model, Val, [publishers, subscribers, open]);
set_xoption(Host, [{<<"pubsub#notification_type">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(notification_type, Val, [headline, normal]);
set_xoption(Host, [{<<"pubsub#node_type">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(node_type, Val, [leaf, collection]);
set_xoption(Host, [{<<"pubsub#max_payload_size">>, [Val]} | Opts], NewOpts) ->
    ?SET_INTEGER_XOPT(max_payload_size, Val, 0, (?MAX_PAYLOAD_SIZE));
set_xoption(Host, [{<<"pubsub#send_last_published_item">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(send_last_published_item, Val, [never, on_sub, on_sub_and_presence]);
set_xoption(Host, [{<<"pubsub#presence_based_delivery">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(presence_based_delivery, Val);
set_xoption(Host, [{<<"pubsub#purge_offline">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(purge_offline, Val);
set_xoption(Host, [{<<"pubsub#title">>, Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(title, Value);
set_xoption(Host, [{<<"pubsub#type">>, Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(type, Value);
set_xoption(Host, [{<<"pubsub#body_xslt">>, Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(body_xslt, Value);
set_xoption(Host, [{<<"pubsub#collection">>, Value} | Opts], NewOpts) ->
    %    NewValue = [string_to_node(V) || V <- Value],
    ?SET_LIST_XOPT(collection, Value);
set_xoption(Host, [{<<"pubsub#node">>, [Value]} | Opts], NewOpts) ->
    %    NewValue = string_to_node(Value),
    ?SET_LIST_XOPT(node, Value);
set_xoption(Host, [_ | Opts], NewOpts) ->
    set_xoption(Host, Opts, NewOpts).

get_max_items_node({_, ServerHost, _}) ->
    get_max_items_node(ServerHost);
get_max_items_node(Host) ->
    config(serverhost(Host), max_items_node, undefined).

get_max_subscriptions_node({_, ServerHost, _}) ->
    get_max_subscriptions_node(ServerHost);
get_max_subscriptions_node(Host) ->
    config(serverhost(Host), max_subscriptions_node, undefined).

%%%% last item cache handling

is_last_item_cache_enabled({_, ServerHost, _}) ->
    is_last_item_cache_enabled(ServerHost);
is_last_item_cache_enabled(Host) ->
    config(serverhost(Host), last_item_cache, false).

set_cached_item({_, ServerHost, _}, Nidx, ItemId, Publisher, Payload) ->
    set_cached_item(ServerHost, Nidx, ItemId, Publisher, Payload);
set_cached_item(Host, Nidx, ItemId, Publisher, Payload) ->
    case is_last_item_cache_enabled(Host) of
	true -> mnesia:dirty_write({pubsub_last_item, Nidx, ItemId,
		    {p1_time_compat:timestamp(), jid:tolower(jid:remove_resource(Publisher))},
		    Payload});
	_ -> ok
    end.

unset_cached_item({_, ServerHost, _}, Nidx) ->
    unset_cached_item(ServerHost, Nidx);
unset_cached_item(Host, Nidx) ->
    case is_last_item_cache_enabled(Host) of
	true -> mnesia:dirty_delete({pubsub_last_item, Nidx});
	_ -> ok
    end.

-spec(get_cached_item/2 ::
    (
	Host    :: mod_pubsub:host(),
	Nidx :: mod_pubsub:nodeIdx())
    -> undefined | mod_pubsub:pubsubItem()
    ).
get_cached_item({_, ServerHost, _}, Nidx) ->
    get_cached_item(ServerHost, Nidx);
get_cached_item(Host, Nidx) ->
    case is_last_item_cache_enabled(Host) of
	true ->
	    case mnesia:dirty_read({pubsub_last_item, Nidx}) of
		[#pubsub_last_item{itemid = ItemId, creation = Creation, payload = Payload}] ->
		    %            [{pubsub_last_item, Nidx, ItemId, Creation,
		    %              Payload}] ->
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

host(ServerHost) ->
    config(ServerHost, host, <<"pubsub.", ServerHost/binary>>).

serverhost({_U, Server, _R})->
    Server;
serverhost(Host) ->
    case binary:match(Host, <<"pubsub.">>) of
	{0,7} ->
	    [_,ServerHost] = binary:split(Host, <<".">>),
	    ServerHost;
	_ ->
	    Host
    end.

tree(Host) ->
    case config(serverhost(Host), nodetree) of
	undefined -> tree(Host, ?STDTREE);
	Tree -> Tree
    end.

tree(_Host, <<"virtual">>) ->
    nodetree_virtual;   % special case, virtual does not use any backend
tree(Host, Name) ->
    case gen_mod:db_type(serverhost(Host), ?MODULE) of
	mnesia -> jlib:binary_to_atom(<<"nodetree_", Name/binary>>);
	odbc -> jlib:binary_to_atom(<<"nodetree_", Name/binary, "_odbc">>);
	_ -> Name
    end.

plugin(Host, Name) ->
    case gen_mod:db_type(serverhost(Host), ?MODULE) of
	mnesia -> jlib:binary_to_atom(<<"node_", Name/binary>>);
	odbc -> jlib:binary_to_atom(<<"node_", Name/binary, "_odbc">>);
	_ -> Name
    end.

plugins(Host) ->
    case config(serverhost(Host), plugins) of
	undefined -> [?STDNODE];
	[] -> [?STDNODE];
	Plugins -> Plugins
    end.

subscription_plugin(Host) ->
    case gen_mod:db_type(serverhost(Host), ?MODULE) of
	mnesia -> pubsub_subscription;
	odbc -> pubsub_subscription_odbc;
	_ -> none
    end.

config(ServerHost, Key) ->
    config(ServerHost, Key, undefined).
config(ServerHost, Key, Default) ->
    case catch ets:lookup(gen_mod:get_module_proc(ServerHost, config), Key) of
	[{Key, Value}] -> Value;
	_ -> Default
    end.

select_type(ServerHost, Host, Node, Type) ->
    SelectedType = case Host of
	{_User, _Server, _Resource} ->
	    case config(ServerHost, pep_mapping) of
		undefined -> ?PEPNODE;
		Mapping -> proplists:get_value(Node, Mapping, ?PEPNODE)
	    end;
	_ ->
	    Type
    end,
    ConfiguredTypes = plugins(ServerHost),
    case lists:member(SelectedType, ConfiguredTypes) of
	true -> SelectedType;
	false -> hd(ConfiguredTypes)
    end.

select_type(ServerHost, Host, Node) ->
    select_type(ServerHost, Host, Node, hd(plugins(ServerHost))).

feature(<<"rsm">>) -> ?NS_RSM;
feature(Feature) -> <<(?NS_PUBSUB)/binary, "#", Feature/binary>>.

features() ->
    [% see plugin "access-authorize",   % OPTIONAL
	<<"access-open">>,   % OPTIONAL this relates to access_model option in node_hometree
	<<"access-presence">>,   % OPTIONAL this relates to access_model option in node_pep
	<<"access-whitelist">>,   % OPTIONAL
	<<"collections">>,   % RECOMMENDED
	<<"config-node">>,   % RECOMMENDED
	<<"create-and-configure">>,   % RECOMMENDED
	<<"item-ids">>,   % RECOMMENDED
	<<"last-published">>,   % RECOMMENDED
	<<"member-affiliation">>,   % RECOMMENDED
	<<"presence-notifications">>,   % OPTIONAL
	<<"presence-subscribe">>,   % RECOMMENDED
	<<"publisher-affiliation">>,   % RECOMMENDED
	<<"publish-only-affiliation">>,   % OPTIONAL
	<<"retrieve-default">>,
	<<"shim">>].   % RECOMMENDED

% see plugin "retrieve-items",   % RECOMMENDED
% see plugin "retrieve-subscriptions",   % RECOMMENDED
% see plugin "subscribe",   % REQUIRED
% see plugin "subscription-options",   % OPTIONAL
% see plugin "subscription-notifications"   % OPTIONAL

plugin_features(Host, Type) ->
    Module = plugin(Host, Type),
    case catch Module:features() of
	{'EXIT', {undef, _}} -> [];
	Result -> Result
    end.

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
tree_call({_User, Server, _Resource}, Function, Args) ->
    tree_call(Server, Function, Args);
tree_call(Host, Function, Args) ->
    ?DEBUG("tree_call ~p ~p ~p", [Host, Function, Args]),
    catch apply(tree(Host), Function, Args).

tree_action(Host, Function, Args) ->
    ?DEBUG("tree_action ~p ~p ~p", [Host, Function, Args]),
    ServerHost = serverhost(Host),
    Fun = fun () -> tree_call(Host, Function, Args) end,
    case gen_mod:db_type(ServerHost, ?MODULE) of
	mnesia ->
	    catch mnesia:sync_dirty(Fun);
	odbc ->
	    case catch ejabberd_odbc:sql_bloc(ServerHost, Fun) of
		{atomic, Result} ->
		    Result;
		{aborted, Reason} ->
		    ?ERROR_MSG("transaction return internal error: ~p~n", [{aborted, Reason}]),
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end;
	Other ->
	    ?ERROR_MSG("unsupported backend: ~p~n", [Other]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

%% @doc <p>node plugin call.</p>
node_call(Host, Type, Function, Args) ->
    ?DEBUG("node_call ~p ~p ~p", [Type, Function, Args]),
    Module = plugin(Host, Type),
    case apply(Module, Function, Args) of
	{result, Result} ->
	    {result, Result};
	{error, Error} ->
	    {error, Error};
	{'EXIT', {undef, Undefined}} ->
	    case Type of
		?STDNODE -> {error, {undef, Undefined}};
		_ -> node_call(Host, ?STDNODE, Function, Args)
	    end;
	{'EXIT', Reason} ->
	    {error, Reason};
	Result ->
	    {result, Result} %% any other return value is forced as result
    end.

node_action(Host, Type, Function, Args) ->
    ?DEBUG("node_action ~p ~p ~p ~p", [Host, Type, Function, Args]),
    transaction(Host, fun () ->
		node_call(Host, Type, Function, Args)
	end,
	sync_dirty).

%% @doc <p>plugin transaction handling.</p>
transaction(Host, Node, Action, Trans) ->
    transaction(Host, fun () ->
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

transaction(Host, Fun, Trans) ->
    ServerHost = serverhost(Host),
    DBType = gen_mod:db_type(ServerHost, ?MODULE),
    Retry = case DBType of
	odbc -> 2;
	_ -> 1
    end,
    transaction_retry(Host, ServerHost, Fun, Trans, DBType, Retry).

transaction_retry(_Host, _ServerHost, _Fun, _Trans, _DBType, 0) ->
    {error, ?ERR_INTERNAL_SERVER_ERROR};
transaction_retry(Host, ServerHost, Fun, Trans, DBType, Count) ->
    Res = case DBType of
	mnesia ->
	    catch mnesia:Trans(Fun);
	odbc ->
	    SqlFun = case Trans of
		transaction -> sql_transaction;
		_ -> sql_bloc
	    end,
	    catch ejabberd_odbc:SqlFun(ServerHost, Fun);
	_ ->
	    {unsupported, DBType}
    end,
    case Res of
	{result, Result} ->
	    {result, Result};
	{error, Error} ->
	    {error, Error};
	{atomic, {result, Result}} ->
	    {result, Result};
	{atomic, {error, Error}} ->
	    {error, Error};
	{aborted, Reason} ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [{aborted, Reason}]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	{'EXIT', {timeout, _} = Reason} ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [Reason]),
	    transaction_retry(Host, ServerHost, Fun, Trans, DBType, Count - 1);
	{'EXIT', Reason} ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [{'EXIT', Reason}]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	Other ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [Other]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

%%%% helpers

%% Add pubsub-specific error element
extended_error(Error, Ext) ->
    extended_error(Error, Ext, [{<<"xmlns">>, ?NS_PUBSUB_ERRORS}]).

extended_error(Error, unsupported, Feature) ->
    %% Give a uniq identifier
    extended_error(Error, <<"unsupported">>,
	[{<<"xmlns">>, ?NS_PUBSUB_ERRORS},
	    {<<"feature">>, Feature}]);
extended_error(#xmlel{name = Error, attrs = Attrs, children = SubEls}, Ext, ExtAttrs) ->
    #xmlel{name = Error, attrs = Attrs,
	children = lists:reverse([#xmlel{name = Ext, attrs = ExtAttrs} | SubEls])}.

string_to_ljid(JID) ->
    case jid:from_string(JID) of
	error ->
	    {<<>>, <<>>, <<>>};
	J ->
	    case jid:tolower(J) of
		error -> {<<>>, <<>>, <<>>};
		J1 -> J1
	    end
    end.

-spec(uniqid/0 :: () -> mod_pubsub:itemId()).
uniqid() ->
    {T1, T2, T3} = p1_time_compat:timestamp(),
    iolist_to_binary(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3])).

nodeAttr(Node) -> [{<<"node">>, Node}].

itemAttr([]) -> [];
itemAttr(ItemId) -> [{<<"id">>, ItemId}].

itemsEls(Items) ->
    [#xmlel{name = <<"item">>, attrs = itemAttr(ItemId), children = Payload}
	|| #pubsub_item{itemid = {ItemId, _}, payload = Payload} <- Items].

-spec(add_message_type/2 ::
    (
	Message :: xmlel(),
	Type    :: atom())
    -> xmlel()
    ).

add_message_type(Message, normal) -> Message;
add_message_type(#xmlel{name = <<"message">>, attrs = Attrs, children = Els}, Type) ->
    #xmlel{name = <<"message">>,
	attrs = [{<<"type">>, jlib:atom_to_binary(Type)} | Attrs],
	children = Els};
add_message_type(XmlEl, _Type) ->
    XmlEl.

%% Place of <headers/> changed at the bottom of the stanza
%% cf. http://xmpp.org/extensions/xep-0060.html#publisher-publish-success-subid
%%
%% "[SHIM Headers] SHOULD be included after the event notification information
%% (i.e., as the last child of the <message/> stanza)".

add_shim_headers(Stanza, HeaderEls) ->
    add_headers(Stanza, <<"headers">>, ?NS_SHIM, HeaderEls).

add_extended_headers(Stanza, HeaderEls) ->
    add_headers(Stanza, <<"addresses">>, ?NS_ADDRESS, HeaderEls).

add_headers(#xmlel{name = Name, attrs = Attrs, children = Els}, HeaderName, HeaderNS, HeaderEls) ->
    HeaderEl = #xmlel{name = HeaderName,
	    attrs = [{<<"xmlns">>, HeaderNS}],
	    children = HeaderEls},
    #xmlel{name = Name, attrs = Attrs,
	children = lists:append(Els, [HeaderEl])}.

subid_shim(SubIds) ->
    [#xmlel{name = <<"header">>,
	    attrs = [{<<"name">>, <<"SubId">>}],
	    children = [{xmlcdata, SubId}]}
	|| SubId <- SubIds].

%% The argument is a list of Jids because this function could be used
%% with the 'pubsub#replyto' (type=jid-multi) node configuration.

extended_headers(Jids) ->
    [#xmlel{name = <<"address">>,
	    attrs = [{<<"type">>, <<"replyto">>}, {<<"jid">>, Jid}]}
	|| Jid <- Jids].

on_user_offline(_, JID, _) ->
    {User, Server, Resource} = jid:tolower(JID),
    case user_resources(User, Server) of
	[] -> purge_offline({User, Server, Resource});
	_ -> true
    end.

purge_offline(LJID) ->
    Host = host(element(2, LJID)),
    Plugins = plugins(Host),
    Result = lists:foldl(fun (Type, {Status, Acc}) ->
		    Features = plugin_features(Host, Type),
		    case lists:member(<<"retrieve-affiliations">>, plugin_features(Host, Type)) of
			false ->
			    {{error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED,
					unsupported, <<"retrieve-affiliations">>)},
				Acc};
			true ->
			    Items = lists:member(<<"retract-items">>, Features)
				andalso lists:member(<<"persistent-items">>, Features),
			    if Items ->
				    {result, Affs} = node_action(Host, Type,
					    get_entity_affiliations, [Host, LJID]),
				    {Status, [Affs | Acc]};
				true ->
				    {Status, Acc}
			    end
		    end
	    end,
	    {ok, []}, Plugins),
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
	{Error, _} ->
	    ?DEBUG("on_user_offline ~p", [Error])
    end.

purge_offline(Host, LJID, Node) ->
    Nidx = Node#pubsub_node.id,
    Type = Node#pubsub_node.type,
    Options = Node#pubsub_node.options,
    case node_action(Host, Type, get_items, [Nidx, service_jid(Host), none]) of
	{result, {[], _}} ->
	    ok;
	{result, {Items, _}} ->
	    {User, Server, _} = LJID,
	    PublishModel = get_option(Options, publish_model),
	    ForceNotify = get_option(Options, notify_retract),
	    {_, NodeId} = Node#pubsub_node.nodeid,
	    lists:foreach(fun
		    (#pubsub_item{itemid = {ItemId, _}, modification = {_, {U, S, _}}})
			    when (U == User) and (S == Server) ->
			case node_action(Host, Type, delete_item, [Nidx, {U, S, <<>>}, PublishModel, ItemId]) of
			    {result, {_, broadcast}} ->
				broadcast_retract_items(Host, NodeId, Nidx, Type, Options, [ItemId], ForceNotify),
				case get_cached_item(Host, Nidx) of
				    #pubsub_item{itemid = {ItemId, Nidx}} -> unset_cached_item(Host, Nidx);
				    _ -> ok
				end;
			    {result, _} ->
				ok;
			    Error ->
				Error
			end;
		    (_) ->
			true
		end, Items);
	Error ->
	    Error
    end.

mod_opt_type(access_createnode) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(ignore_pep_from_offline) ->
    fun (A) when is_boolean(A) -> A end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(last_item_cache) ->
    fun (A) when is_boolean(A) -> A end;
mod_opt_type(max_items_node) ->
    fun (A) when is_integer(A) andalso A >= 0 -> A end;
mod_opt_type(max_subscriptions_node) ->
    fun (A) when is_integer(A) andalso A >= 0 -> A end;
mod_opt_type(default_node_config) ->
    fun (A) when is_list(A) -> A end;
mod_opt_type(nodetree) ->
    fun (A) when is_binary(A) -> A end;
mod_opt_type(pep_mapping) ->
    fun (A) when is_list(A) -> A end;
mod_opt_type(plugins) ->
    fun (A) when is_list(A) -> A end;
mod_opt_type(_) ->
    [access_createnode, db_type, host,
     ignore_pep_from_offline, iqdisc, last_item_cache,
     max_items_node, nodetree, pep_mapping, plugins,
     max_subscriptions_node, default_node_config].
