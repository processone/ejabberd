%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%% @copyright 2006-2012 ProcessOne
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

-module(mod_pubsub_odbc).
-author('christophe.romain@process-one.net').
-version('1.13-0').

-behaviour(gen_server).
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("adhoc.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").

-define(STDTREE, "tree_odbc").
-define(STDNODE, "flat_odbc").
-define(PEPNODE, "pep_odbc").

%% exports for hooks
-export([presence_probe/3,
	 caps_update/3,
	 in_subscription/6,
	 out_subscription/4,
	 on_user_offline/3,
	 remove_user/2,
	 disco_local_identity/5,
	 disco_local_features/5,
	 disco_local_items/5,
	 disco_sm_identity/5,
	 disco_sm_features/5,
	 disco_sm_items/5
	]).
%% exported iq handlers
-export([iq_sm/3
	]).

%% exports for console debug manual use
-export([create_node/5,
	 delete_node/3,
	 subscribe_node/5,
	 unsubscribe_node/5,
	 publish_item/6,
	 delete_item/4,
	 send_items/6,
	 get_items/2,
	 get_item/3,
	 get_cached_item/2,
	 broadcast_stanza/9,
	 get_configure/5,
	 set_configure/5,
	 tree_action/3,
	 node_action/4
	]).

%% general helpers for plugins
-export([node_to_string/1,
	 string_to_node/1,
	 subscription_to_string/1,
	 affiliation_to_string/1,
	 string_to_subscription/1,
	 string_to_affiliation/1,
	 extended_error/2,
	 extended_error/3,
	 escape/1
	]).

%% API and gen_server callbacks
-export([start_link/2,
	 start/2,
	 stop/1,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%% calls for parallel sending of last items
-export([send_loop/1
	]).

-define(PROCNAME, ejabberd_mod_pubsub_odbc).
-define(LOOPNAME, ejabberd_mod_pubsub_loop).
-define(PLUGIN_PREFIX, "node_").
-define(TREE_PREFIX, "nodetree_").

-record(state, {server_host,
		host,
		access,
		pep_mapping = [],
		ignore_pep_from_offline = true,
		last_item_cache = false,
		max_items_node = ?MAXITEMS,
		nodetree = ?STDTREE,
		plugins = [?STDNODE]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc,
		 {?MODULE, start_link, [Host, Opts]},
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
%%			 {ok, State, Timeout} |
%%			 ignore	       |
%%			 {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ServerHost, Opts]) ->
    ?DEBUG("pubsub init ~p ~p",[ServerHost,Opts]),
    Host = gen_mod:get_opt_host(ServerHost, Opts, "pubsub.@HOST@"),
    Access = gen_mod:get_opt(access_createnode, Opts, all),
    PepOffline = gen_mod:get_opt(ignore_pep_from_offline, Opts, true),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    LastItemCache = gen_mod:get_opt(last_item_cache, Opts, false),
    MaxItemsNode = gen_mod:get_opt(max_items_node, Opts, ?MAXITEMS),
    pubsub_index:init(Host, ServerHost, Opts),
    ets:new(gen_mod:get_module_proc(Host, config), [set, named_table]),
    ets:new(gen_mod:get_module_proc(ServerHost, config), [set, named_table]),
    {Plugins, NodeTree, PepMapping} = init_plugins(Host, ServerHost, Opts),
    mnesia:create_table(pubsub_last_item, [{ram_copies, [node()]}, {attributes, record_info(fields, pubsub_last_item)}]),
    mod_disco:register_feature(ServerHost, ?NS_PUBSUB),
    ets:insert(gen_mod:get_module_proc(Host, config), {nodetree, NodeTree}),
    ets:insert(gen_mod:get_module_proc(Host, config), {plugins, Plugins}),
    ets:insert(gen_mod:get_module_proc(Host, config), {last_item_cache, LastItemCache}),
    ets:insert(gen_mod:get_module_proc(Host, config), {max_items_node, MaxItemsNode}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {nodetree, NodeTree}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {plugins, Plugins}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {last_item_cache, LastItemCache}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {max_items_node, MaxItemsNode}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {pep_mapping, PepMapping}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {ignore_pep_from_offline, PepOffline}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {host, Host}),
    ejabberd_hooks:add(sm_remove_connection_hook, ServerHost, ?MODULE, on_user_offline, 75),
    ejabberd_hooks:add(disco_local_identity, ServerHost, ?MODULE, disco_local_identity, 75),
    ejabberd_hooks:add(disco_local_features, ServerHost, ?MODULE, disco_local_features, 75),
    ejabberd_hooks:add(disco_local_items, ServerHost, ?MODULE, disco_local_items, 75),
    ejabberd_hooks:add(presence_probe_hook, ServerHost, ?MODULE, presence_probe, 80),
    ejabberd_hooks:add(roster_in_subscription, ServerHost, ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, ServerHost, ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(remove_user, ServerHost, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, ServerHost, ?MODULE, remove_user, 50),
    case lists:member(?PEPNODE, Plugins) of
	true ->
	    ejabberd_hooks:add(caps_update, ServerHost, ?MODULE, caps_update, 80),
	    ejabberd_hooks:add(disco_sm_identity, ServerHost, ?MODULE, disco_sm_identity, 75),
	    ejabberd_hooks:add(disco_sm_features, ServerHost, ?MODULE, disco_sm_features, 75),
	    ejabberd_hooks:add(disco_sm_items, ServerHost, ?MODULE, disco_sm_items, 75),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHost, ?NS_PUBSUB, ?MODULE, iq_sm, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHost, ?NS_PUBSUB_OWNER, ?MODULE, iq_sm, IQDisc);
	false ->
	    ok
    end,
    ejabberd_router:register_route(Host),
    put(server_host, ServerHost), % not clean, but needed to plug hooks at any location
    init_nodes(Host, ServerHost, NodeTree, Plugins),
    State = #state{host = Host,
		server_host = ServerHost,
		access = Access,
		pep_mapping = PepMapping,
		ignore_pep_from_offline = PepOffline,
		last_item_cache = LastItemCache,
		max_items_node = MaxItemsNode,
		nodetree = NodeTree,
		plugins = Plugins},
    init_send_loop(ServerHost, State),
    {ok, State}.

init_send_loop(ServerHost, State) ->
    Proc = gen_mod:get_module_proc(ServerHost, ?LOOPNAME),
    SendLoop = spawn(?MODULE, send_loop, [State]),
    register(Proc, SendLoop),
    SendLoop.

%% @spec (Host, ServerHost, Opts) -> Plugins
%%	 Host = mod_pubsub:host()   Opts = [{Key,Value}]
%%	 ServerHost = host()
%%	 Key = atom()
%%	 Value = term()
%%	 Plugins = [Plugin::string()]
%% @doc Call the init/1 function for each plugin declared in the config file.
%% The default plugin module is implicit.
%% <p>The Erlang code for the plugin is located in a module called
%% <em>node_plugin</em>. The 'node_' prefix is mandatory.</p>
%% <p>The modules are initialized in alphetical order and the list is checked
%% and sorted to ensure that each module is initialized only once.</p>
%% <p>See {@link node_hometree:init/1} for an example implementation.</p>
init_plugins(Host, ServerHost, Opts) ->
    TreePlugin = list_to_atom(?TREE_PREFIX ++
			      gen_mod:get_opt(nodetree, Opts, ?STDTREE)),
    ?DEBUG("** tree plugin is ~p",[TreePlugin]),
    TreePlugin:init(Host, ServerHost, Opts),
    Plugins = gen_mod:get_opt(plugins, Opts, [?STDNODE]),
    PepMapping = gen_mod:get_opt(pep_mapping, Opts, []),
    ?DEBUG("** PEP Mapping : ~p~n",[PepMapping]),
    PluginsOK = lists:foldl(fun(Name, Acc) ->
			  Plugin = list_to_atom(?PLUGIN_PREFIX ++ Name),
			  case catch apply(Plugin, init, [Host, ServerHost, Opts]) of
			{'EXIT', _Error} ->
			    Acc;
			_ ->
			    ?DEBUG("** init ~s plugin",[Name]),
			    [Name | Acc]
			  end
		end, [], Plugins),
    {lists:reverse(PluginsOK), TreePlugin, PepMapping}.

terminate_plugins(Host, ServerHost, Plugins, TreePlugin) ->
    lists:foreach(fun(Name) ->
			  ?DEBUG("** terminate ~s plugin",[Name]),
			  Plugin = list_to_atom(?PLUGIN_PREFIX++Name),
			  Plugin:terminate(Host, ServerHost)
		  end, Plugins),
    TreePlugin:terminate(Host, ServerHost),
    ok.

init_nodes(Host, ServerHost, _NodeTree, Plugins) ->
    %% TODO, this call should be done plugin side
    case lists:member("hometree_odbc", Plugins) of
    true ->
	create_node(Host, ServerHost, string_to_node("/home"), service_jid(Host), "hometree_odbc"),
	create_node(Host, ServerHost, string_to_node("/home/"++ServerHost), service_jid(Host), "hometree_odbc");
    false ->
	ok
    end.

send_loop(State) ->
    receive
    {presence, JID, Pid} ->
	Host = State#state.host,
	ServerHost = State#state.server_host,
	LJID = jlib:jid_tolower(JID),
	BJID = jlib:jid_remove_resource(LJID),
	%% for each node From is subscribed to
	%% and if the node is so configured, send the last published item to From
	lists:foreach(fun(PType) ->
	    Subscriptions = case catch node_action(Host, PType, get_entity_subscriptions_for_send_last, [Host, JID]) of
		{result, S} -> S;
		_ -> []
	    end,
	    lists:foreach(
		fun({Node, subscribed, _, SubJID}) -> 
		    if (SubJID == LJID) or (SubJID == BJID) ->
			#pubsub_node{nodeid = {H, N}, type = Type, id = NodeId} = Node,
			send_items(H, N, NodeId, Type, LJID, last);
		    true ->
			% resource not concerned about that subscription
			ok
		    end;
		   (_) ->
		    ok
		end, Subscriptions)
	end, State#state.plugins),
	%% and force send the last PEP events published by its offline and local contacts
	%% only if pubsub is explicitely configured for that.
	%% this is a hack in a sense that PEP should only be based on presence
	%% and is not able to "store" events of remote users (via s2s)
	%% this makes that hack only work for local domain by now
	if not State#state.ignore_pep_from_offline ->
	    {User, Server, Resource} = jlib:jid_tolower(JID),
		case catch ejabberd_c2s:get_subscribed(Pid) of
		Contacts when is_list(Contacts) ->
		    lists:foreach(
			fun({U, S, R}) ->
			    case S of
				ServerHost ->  %% local contacts
				    case user_resources(U, S) of
				    [] -> %% offline
					PeerJID = jlib:make_jid(U, S, R),
					self() ! {presence, User, Server, [Resource], PeerJID};
				    _ -> %% online
					% this is already handled by presence probe
					ok
				    end;
				_ ->  %% remote contacts
				    % we can not do anything in any cases
				    ok
			    end
			end, Contacts);
		_ ->
		    ok
		end;
	true ->
	    ok
	end,
	send_loop(State);
    {presence, User, Server, Resources, JID} ->
	%% get resources caps and check if processing is needed
	spawn(fun() ->
		Host = State#state.host,
		Owner = jlib:jid_remove_resource(jlib:jid_tolower(JID)),
		lists:foreach(fun(#pubsub_node{nodeid = {_, Node}, type = Type, id = NodeId, options = Options}) ->
			case get_option(Options, send_last_published_item) of
			    on_sub_and_presence ->
				    lists:foreach(
				      fun(Resource) ->
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
			          if Subscribed ->
				          send_items(Owner, Node, NodeId, Type, LJID, last);
				          true ->
						          ok
			          end    
				    end, Resources);
			    _ ->
				ok
			end
		    end, tree_action(Host, get_nodes, [Owner, JID]))
		end),
	send_loop(State);
    stop ->
	ok
    end.

%% -------
%% disco hooks handling functions
%%

disco_local_identity(Acc, _From, To, [], _Lang) ->
    case lists:member(?PEPNODE, plugins(To#jid.lserver)) of
  true  ->
      [{xmlelement, "identity", [{"category", "pubsub"}, {"type", "pep"}], []} | Acc];
  false -> Acc
    end;
disco_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_local_features(Acc, _From, To, [], _Lang) ->
    Host = To#jid.lserver,
    Feats = case Acc of
	{result, I} -> I;
	_ -> []
    end,
    {result, Feats ++ lists:map(fun(Feature) ->
	?NS_PUBSUB++"#"++Feature
    end, features(Host, <<>>))};
disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_local_items(Acc, _From, _To, [], _Lang) ->
    Acc;
disco_local_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_sm_identity(Acc, From, To, Node, Lang) when is_list(Node) ->
    disco_sm_identity(Acc, From, To, list_to_binary(Node), Lang);
disco_sm_identity(empty, From, To, Node, Lang) ->
    disco_sm_identity([], From, To, Node, Lang);
disco_sm_identity(Acc, From, To, Node, _Lang) ->
    disco_identity(jlib:jid_tolower(jlib:jid_remove_resource(To)), Node, From) ++ Acc.

disco_identity(_Host, <<>>, _From) ->
    [{xmlelement, "identity", [{"category", "pubsub"}, {"type", "pep"}], []}];
disco_identity(Host, Node, From) ->
    Action = fun(#pubsub_node{id = Idx, type = Type, options = Options}) ->
	    Owners = node_owners_call(Type, Idx),
	    case get_allowed_items_call(Host, Idx, From, Type, Options, Owners) of
		{result, _} ->
		    {result, [{xmlelement, "identity", [{"category", "pubsub"}, {"type", "pep"}], []},
			{xmlelement, "identity",
			    [{"category", "pubsub"},
				{"type", "leaf"}
				| case get_option(Options, title) of
				    false   -> [];
				    [Title] -> [{"name", Title}]
				end],
			    []}]};
		_ -> {result, []}
	    end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

disco_sm_features(Acc, From, To, Node, Lang) when is_list(Node) ->
    disco_sm_features(Acc, From, To, list_to_binary(Node), Lang);
disco_sm_features(empty, From, To, Node, Lang) ->
    disco_sm_features({result, []}, From, To, Node, Lang);
disco_sm_features({result, OtherFeatures} = _Acc, From, To, Node, _Lang) ->
    {result,
     OtherFeatures ++
       disco_features(jlib:jid_tolower(jlib:jid_remove_resource(To)), Node, From)};
disco_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_features(_Host, <<>>, _From) ->
    [?NS_PUBSUB
    | [?NS_PUBSUB++"#"++Feature || Feature <- features("pep")]];
disco_features(Host, Node, From) ->
    Action = fun(#pubsub_node{id = Idx, type = Type, options = Options}) ->
	    Owners = node_owners_call(Type, Idx),
	    case get_allowed_items_call(Host, Idx, From, Type, Options, Owners) of
		{result, _} ->
		    {result, [?NS_PUBSUB
			    | [?NS_PUBSUB ++ "#" ++ Feature || Feature <- features("pep")]]};
		_ -> {result, []}
	    end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

disco_sm_items(Acc, From, To, Node, Lang) when is_list(Node) ->
    disco_sm_items(Acc, From, To, list_to_binary(Node), Lang);
disco_sm_items(empty, From, To, Node, Lang) ->
    disco_sm_items({result, []}, From, To, Node, Lang);
disco_sm_items({result, OtherItems}, From, To, Node, _Lang) ->
    {result,
     lists:usort(OtherItems ++
       disco_items(jlib:jid_tolower(jlib:jid_remove_resource(To)), Node, From))};
disco_sm_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_items(Host, <<>>, From) ->
    Action = fun(#pubsub_node{nodeid ={_, NodeID}, options = Options, type = Type, id = Idx}, Acc) ->
		Owners = node_owners_call(Type, Idx),
		case get_allowed_items_call(Host, Idx, From, Type, Options, Owners) of
		    {result, _} ->
			[{xmlelement, "item",
				[{"node", binary_to_list(NodeID)},
				    {"jid",  case Host of
					    {_,_,_} -> jlib:jid_to_string(Host);
					    _Host   -> Host
					end}
				    | case get_option(Options, title) of
					false   -> [];
					[Title] -> [{"name", Title}]
				    end],
				[]}
			    | Acc];
		    _ -> Acc
		end
	    end,
    case transaction_on_nodes(Host, Action, sync_dirty) of
	{result, Items} -> Items;
	_ -> []
    end;

disco_items(Host, Node, From) ->
    Action = fun(#pubsub_node{id = Idx, type = Type, options = Options}) ->
	    Owners = node_owners_call(Type, Idx),
	    case get_allowed_items_call(Host, Idx, From, Type, Options, Owners) of
		{result, Items} ->
		    {result, [{xmlelement, "item",
			    [{"jid", case Host of
					{_,_,_} -> jlib:jid_to_string(Host);
					_Host   -> Host
				    end},
				{"name", ItemID}], []}
			|| #pubsub_item{itemid = {ItemID,_}} <- Items]};
		_ -> {result, []}
	    end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

%% -------
%% presence hooks handling functions
%%

caps_update(#jid{luser = U, lserver = S, lresource = R} = From, To, _Features) ->
    Pid = ejabberd_sm:get_session_pid(U, S, R),
    presence_probe(From, To, Pid).

presence_probe(#jid{luser = User, lserver = Server, lresource = Resource} = JID, JID, Pid) ->
    %%?DEBUG("presence probe self ~s@~s/~s  ~s@~s/~s",[User,Server,Resource,element(2,JID),element(3,JID),element(4,JID)]),
    presence(Server, {presence, JID, Pid}),
    presence(Server, {presence, User, Server, [Resource], JID});
presence_probe(#jid{luser = User, lserver = Server}, #jid{luser = User, lserver = Server}, _Pid) ->
    %% ignore presence_probe from other ressources for the current user
    %% this way, we do not send duplicated last items if user already connected with other clients
    ok;
presence_probe(#jid{luser = User, lserver = Server, lresource = Resource}, #jid{lserver = Host} = JID, _Pid) ->
    %%?DEBUG("presence probe peer ~s@~s/~s  ~s@~s/~s",[User,Server,Resource,element(2,JID),element(3,JID),element(4,JID)]),
    presence(Host, {presence, User, Server, [Resource], JID}).

presence(ServerHost, Presence) ->
    SendLoop = case whereis(gen_mod:get_module_proc(ServerHost, ?LOOPNAME)) of
	undefined ->
	    % in case send_loop process died, we rebuild a minimal State record and respawn it
	    Host = host(ServerHost),
	    Plugins = plugins(Host),
	    PepOffline = case catch ets:lookup(gen_mod:get_module_proc(ServerHost, config), ignore_pep_from_offline) of
		[{ignore_pep_from_offline, PO}] -> PO;
		_ -> true
		end,
	    State = #state{host = Host,
		server_host = ServerHost,
		ignore_pep_from_offline = PepOffline,
		plugins = Plugins},
	    init_send_loop(ServerHost, State);
	Pid ->
	    Pid
    end,
    SendLoop ! Presence.

%% -------
%% subscription hooks handling functions
%%

out_subscription(User, Server, JID, subscribed) ->
    Owner = jlib:make_jid(User, Server, ""),
    {PUser, PServer, PResource} = jlib:jid_tolower(JID),
    PResources = case PResource of
	[] -> user_resources(PUser, PServer);
	_ -> [PResource]
    end,
    presence(Server, {presence, PUser, PServer, PResources, Owner}),
    true;
out_subscription(_,_,_,_) ->
    true.
in_subscription(_, User, Server, Owner, unsubscribed, _) ->
    unsubscribe_user(jlib:make_jid(User, Server, ""), Owner),
    true;
in_subscription(_, _, _, _, _, _) ->
    true.

unsubscribe_user(Entity, Owner) ->
    BJID = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    Host = host(element(2, BJID)),
    spawn(fun() ->
	lists:foreach(fun(PType) ->
	    {result, Subscriptions} = node_action(Host, PType, get_entity_subscriptions, [Host, Entity]),
	    lists:foreach(fun
		({#pubsub_node{options = Options, id = NodeId}, subscribed, _, JID}) ->
		    case get_option(Options, access_model) of
			presence ->
			    case lists:member(BJID, node_owners(Host, PType, NodeId)) of
				true ->
				    node_action(Host, PType, unsubscribe_node, [NodeId, Entity, JID, all]);
				false ->
				    {result, ok}
			    end;
			_ ->
			    {result, ok}
		    end;
		(_) ->
		    ok
	    end, Subscriptions)
	end, plugins(Host))
    end).

%% -------
%% user remove hook handling function
%%

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Entity = jlib:make_jid(LUser, LServer, ""),
    Host = host(LServer),
    HomeTreeBase = string_to_node("/home/"++LServer++"/"++LUser),
    spawn(fun() ->
	lists:foreach(fun(PType) ->
	    {result, Subscriptions} = node_action(Host, PType, get_entity_subscriptions, [Host, Entity]),
	    lists:foreach(fun
		({#pubsub_node{id = NodeId}, _, _, JID}) -> node_action(Host, PType, unsubscribe_node, [NodeId, Entity, JID, all])
	    end, Subscriptions),
	    {result, Affiliations} = node_action(Host, PType, get_entity_affiliations, [Host, Entity]),
	    lists:foreach(fun
		({#pubsub_node{nodeid = {H, N}, parents = []}, owner}) -> delete_node(H, N, Entity);
		({#pubsub_node{nodeid = {H, N}, type = "hometree"}, owner}) when N == HomeTreeBase -> delete_node(H, N, Entity);
		({#pubsub_node{id = NodeId}, publisher}) -> node_action(Host, PType, set_affiliation, [NodeId, Entity, none]);
		(_) -> ok
	    end, Affiliations)
	end, plugins(Host))
    end).

%%--------------------------------------------------------------------
%% Function:
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%				      {reply, Reply, State, Timeout} |
%%				      {noreply, State} |
%%				      {noreply, State, Timeout} |
%%				      {stop, Reason, Reply, State} |
%%				      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
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
%%				      {noreply, State, Timeout} |
%%				      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%				       {noreply, State, Timeout} |
%%				       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({route, From, To, Packet},
	    #state{server_host = ServerHost,
		   access = Access,
		   plugins = Plugins} = State) ->
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
terminate(_Reason, #state{host = Host,
			  server_host = ServerHost,
			  nodetree = TreePlugin,
			  plugins = Plugins}) ->
    ejabberd_router:unregister_route(Host),
    case lists:member(?PEPNODE, Plugins) of
	true ->
	    ejabberd_hooks:delete(caps_update, ServerHost, ?MODULE, caps_update, 80),
	    ejabberd_hooks:delete(disco_sm_identity, ServerHost, ?MODULE, disco_sm_identity, 75),
	    ejabberd_hooks:delete(disco_sm_features, ServerHost, ?MODULE, disco_sm_features, 75),
	    ejabberd_hooks:delete(disco_sm_items, ServerHost, ?MODULE, disco_sm_items, 75),
	    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHost, ?NS_PUBSUB),
	    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHost, ?NS_PUBSUB_OWNER);
	false ->
	    ok
    end,
    ejabberd_hooks:delete(sm_remove_connection_hook, ServerHost, ?MODULE, on_user_offline, 75),
    ejabberd_hooks:delete(disco_local_identity, ServerHost, ?MODULE, disco_local_identity, 75),
    ejabberd_hooks:delete(disco_local_features, ServerHost, ?MODULE, disco_local_features, 75),
    ejabberd_hooks:delete(disco_local_items, ServerHost, ?MODULE, disco_local_items, 75),
    ejabberd_hooks:delete(presence_probe_hook, ServerHost, ?MODULE, presence_probe, 80),
    ejabberd_hooks:delete(roster_in_subscription, ServerHost, ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, ServerHost, ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(remove_user, ServerHost, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, ServerHost, ?MODULE, remove_user, 50),
    mod_disco:unregister_feature(ServerHost, ?NS_PUBSUB),
    gen_mod:get_module_proc(ServerHost, ?LOOPNAME) ! stop,
    terminate_plugins(Host, ServerHost, Plugins, TreePlugin).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_route(ServerHost, Access, Plugins, Host, From, To, Packet) ->
    {xmlelement, Name, Attrs, _Els} = Packet,
    case To of
	#jid{luser = "", lresource = ""} ->
	    case Name of
		"iq" ->
		    case jlib:iq_query_info(Packet) of
			#iq{type = get, xmlns = ?NS_DISCO_INFO,
			    sub_el = SubEl, lang = Lang} = IQ ->
			    {xmlelement, _, QAttrs, _} = SubEl,
			    Node = xml:get_attr_s("node", QAttrs),
			    Info = ejabberd_hooks:run_fold(
				     disco_info, ServerHost, [],
				     [ServerHost, ?MODULE, "", ""]),
			    Res = case iq_disco_info(Host, Node, From, Lang) of
				      {result, IQRes} ->
					  jlib:iq_to_xml(
					    IQ#iq{type = result,
						  sub_el = [{xmlelement, "query",
							     QAttrs, IQRes++Info}]});
				      {error, Error} ->
					  jlib:make_error_reply(Packet, Error)
				  end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS,
			    sub_el = SubEl} = IQ ->
			    {xmlelement, _, QAttrs, _} = SubEl,
			    Node = xml:get_attr_s("node", QAttrs),
			    Rsm = jlib:rsm_decode(IQ),
			    Res = case iq_disco_items(Host, Node, From, Rsm) of
				      {result, IQRes} ->
					  jlib:iq_to_xml(
					    IQ#iq{type = result,
						  sub_el = [{xmlelement, "query",
							     QAttrs, IQRes}]});
				      {error, Error} ->
					  jlib:make_error_reply(Packet, Error)
				  end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = IQType, xmlns = ?NS_PUBSUB,
			    lang = Lang, sub_el = SubEl} = IQ ->
			    Res =
				case iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, Access, Plugins) of
				    {result, IQRes} ->
					jlib:iq_to_xml(
					  IQ#iq{type = result,
						sub_el = IQRes});
				    {error, Error} ->
					jlib:make_error_reply(Packet, Error)
				end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = IQType, xmlns = ?NS_PUBSUB_OWNER,
			    lang = Lang, sub_el = SubEl} = IQ ->
			    Res =
				case iq_pubsub_owner(Host, ServerHost, From, IQType, SubEl, Lang) of
				    {result, IQRes} ->
					jlib:iq_to_xml(
					  IQ#iq{type = result,
						sub_el = IQRes});
				    {error, Error} ->
					jlib:make_error_reply(Packet, Error)
				end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = get, xmlns = ?NS_VCARD = XMLNS,
			    lang = Lang, sub_el = _SubEl} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el = [{xmlelement, "vCard", [{"xmlns", XMLNS}],
						   iq_get_vcard(Lang)}]},
			    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
			#iq{type = set, xmlns = ?NS_COMMANDS} = IQ ->
			    Res = case iq_command(Host, ServerHost, From, IQ, Access, Plugins) of
				      {error, Error} ->
					  jlib:make_error_reply(Packet, Error);
				      {result, IQRes} ->
					  jlib:iq_to_xml(IQ#iq{type = result,
							       sub_el = IQRes})
				  end,
			    ejabberd_router:route(To, From, Res);
			#iq{} ->
			    Err = jlib:make_error_reply(
				    Packet,
				    ?ERR_FEATURE_NOT_IMPLEMENTED),
			    ejabberd_router:route(To, From, Err);
			_ ->
			    ok
		    end;
		"message" ->
		    case xml:get_attr_s("type", Attrs) of
			"error" ->
			    ok;
			_ ->
			    case find_authorization_response(Packet) of
				none ->
				    ok;
				invalid ->
				    ejabberd_router:route(To, From,
							  jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST));
				XFields ->
				    handle_authorization_response(Host, From, To, Packet, XFields)
			    end
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    case xml:get_attr_s("type", Attrs) of
		"error" ->
		    ok;
		"result" ->
		    ok;
		_ ->
		    Err = jlib:make_error_reply(Packet, ?ERR_ITEM_NOT_FOUND),
		    ejabberd_router:route(To, From, Err)
	    end
    end.

command_disco_info(_Host, <<?NS_COMMANDS>>, _From) ->
    IdentityEl = {xmlelement, "identity", [{"category", "automation"}, {"type", "command-list"}], []},
    {result, [IdentityEl]};
command_disco_info(_Host, <<?NS_PUBSUB_GET_PENDING>>, _From) ->
    IdentityEl = {xmlelement, "identity", [{"category", "automation"}, {"type", "command-node"}], []},
    FeaturesEl = {xmlelement, "feature", [{"var", ?NS_COMMANDS}], []},
    {result, [IdentityEl, FeaturesEl]}.

node_disco_info(Host, Node, From) ->
    node_disco_info(Host, Node, From, true, true).
%node_disco_identity(Host, Node, From) ->
%    node_disco_info(Host, Node, From, true, false).
%node_disco_features(Host, Node, From) ->
%    node_disco_info(Host, Node, From, false, true).
node_disco_info(Host, Node, From, Identity, Features) ->
    Action =
	fun(#pubsub_node{type = Type, id = NodeId}) ->
		I = case Identity of
			false ->
			    [];
			true ->
			    Types =
				case tree_call(Host, get_subnodes, [Host, Node, From]) of
				    [] ->
					["leaf"]; %% No sub-nodes: it's a leaf node
				    _ ->
					case node_call(Type, get_items, [NodeId, From, none]) of
					    {result, []} -> ["collection"];
					    {result, _} -> ["leaf", "collection"];
					    _ -> []
					end
				end,
			    lists:map(fun(T) ->
					      {xmlelement, "identity", [{"category", "pubsub"},
									{"type", T}], []}
				      end, Types)
		    end,
		F = case Features of
			false ->
			    [];
			true ->
			    [{xmlelement, "feature", [{"var", ?NS_PUBSUB}], []} |
			     lists:map(fun
							("rsm")-> {xmlelement, "feature", [{"var", ?NS_RSM}], []};
							(T) -> {xmlelement, "feature", [{"var", ?NS_PUBSUB++"#"++T}], []}
				       end, features(Type))]
		    end,
		%% TODO: add meta-data info (spec section 5.4)
		{result, I ++ F}
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
    end.

iq_disco_info(Host, SNode, From, Lang) ->
    [RealSNode|_] = case SNode of
	[] -> [[]];
	_ -> string:tokens(SNode, "!")
    end,
    Node = string_to_node(RealSNode),
    case Node of
	<<>> ->
	    {result,
	     [{xmlelement, "identity",
	       [{"category", "pubsub"},
		{"type", "service"},
		{"name", translate:translate(Lang, "Publish-Subscribe")}], []},
		{xmlelement, "feature", [{"var", ?NS_DISCO_INFO}], []},
		{xmlelement, "feature", [{"var", ?NS_DISCO_ITEMS}], []},
		{xmlelement, "feature", [{"var", ?NS_PUBSUB}], []},
		{xmlelement, "feature", [{"var", ?NS_COMMANDS}], []},
		{xmlelement, "feature", [{"var", ?NS_VCARD}], []}] ++
	     lists:map(fun
			("rsm")-> {xmlelement, "feature", [{"var", ?NS_RSM}], []};
			(T) -> {xmlelement, "feature", [{"var", ?NS_PUBSUB++"#"++T}], []}
	     end, features(Host, Node))};
        <<?NS_COMMANDS>> ->
            command_disco_info(Host, Node, From);
        <<?NS_PUBSUB_GET_PENDING>> ->
            command_disco_info(Host, Node, From);
	_ ->
	    node_disco_info(Host, Node, From)
    end.

iq_disco_items(Host, [], From, _RSM) ->
    case tree_action(Host, get_subnodes, [Host, <<>>, From]) of
	Nodes when is_list(Nodes) ->
	    {result, lists:map(
		fun(#pubsub_node{nodeid = {_, SubNode}, options = Options}) ->
		    Attrs =
			case get_option(Options, title) of
			    false ->
				[{"jid", Host} |nodeAttr(SubNode)];
			    Title ->
				[{"jid", Host}, {"name", Title}|nodeAttr(SubNode)]
			end,
		    {xmlelement, "item", Attrs, []}
		end, Nodes)};
	Other ->
	    Other
    end;
iq_disco_items(Host, ?NS_COMMANDS, _From, _RSM) ->
    %% TODO: support localization of this string
    CommandItems = [{xmlelement, "item", [{"jid", Host}, {"node", ?NS_PUBSUB_GET_PENDING}, {"name", "Get Pending"}], []}],
    {result, CommandItems};
iq_disco_items(_Host, ?NS_PUBSUB_GET_PENDING, _From, _RSM) ->
    CommandItems = [],
    {result, CommandItems};
iq_disco_items(Host, Item, From, RSM) ->
    case string:tokens(Item, "!") of
	[_SNode, _ItemID] ->
	    {result, []};
	[SNode] ->
	    Node = string_to_node(SNode),
	    Action = fun(#pubsub_node{id = Idx, type = Type, options = Options}) ->
			Owners = node_owners_call(Type, Idx),
			{NodeItems, RsmOut} = case get_allowed_items_call(Host, Idx, From, Type, Options, Owners, RSM) of
			    {result, R} -> R;
			    _ -> {[], none}
			    end,
			Nodes = lists:map(
				fun(#pubsub_node{nodeid = {_, SubNode}, options = SubOptions}) ->
				    Attrs =
					case get_option(SubOptions, title) of
					    false ->
						[{"jid", Host} |nodeAttr(SubNode)];
					    Title ->
						[{"jid", Host}, {"name", Title}|nodeAttr(SubNode)]
					end,
					{xmlelement, "item", Attrs, []}
				end, tree_call(Host, get_subnodes, [Host, Node, From])),
			Items = lists:map(
				fun(#pubsub_item{itemid = {RN, _}}) ->
				    {result, Name} = node_call(Type, get_item_name, [Host, Node, RN]),
				    {xmlelement, "item", [{"jid", Host}, {"name", Name}], []}
				end, NodeItems),
			{result, Nodes ++ Items ++ jlib:rsm_encode(RsmOut)}
		end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Result}} -> {result, Result};
		Other -> Other
	    end
    end.

iq_sm(From, To, #iq{type = Type, sub_el = SubEl, xmlns = XMLNS, lang = Lang} = IQ) ->
    ServerHost = To#jid.lserver,
    LOwner = jlib:jid_tolower(jlib:jid_remove_resource(To)),
    Res = case XMLNS of
	      ?NS_PUBSUB -> iq_pubsub(LOwner, ServerHost, From, Type, SubEl, Lang);
	      ?NS_PUBSUB_OWNER -> iq_pubsub_owner(LOwner, ServerHost, From, Type, SubEl, Lang)
	  end,
    case Res of
	{result, IQRes} -> IQ#iq{type = result, sub_el = IQRes};
	{error, Error} -> IQ#iq{type = error, sub_el = [Error, SubEl]}
    end.

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [], [{xmlcdata, "ejabberd/mod_pubsub"}]},
     {xmlelement, "URL", [], [{xmlcdata, ?EJABBERD_URI}]},
     {xmlelement, "DESC", [],
      [{xmlcdata,
	translate:translate(Lang,
			    "ejabberd Publish-Subscribe module") ++
			    "\nCopyright (c) 2004-2012 ProcessOne"}]}].

iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang) ->
    iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, all, plugins(ServerHost)).

iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, Access, Plugins) ->
    {xmlelement, _, _, SubEls} = SubEl,
    case xml:remove_cdata(SubEls) of
	[{xmlelement, Name, Attrs, Els} | Rest] ->
	    Node = string_to_node(xml:get_attr_s("node", Attrs)),
	    case {IQType, Name} of
		{set, "create"} ->
		    Config = case Rest of
			[{xmlelement, "configure", _, C}] -> C;
			_ -> []
		    end,
		    %% Get the type of the node
		    Type = case xml:get_attr_s("type", Attrs) of
				[] -> hd(Plugins);
				T -> T
			    end,
		    %% we use Plugins list matching because we do not want to allocate
		    %% atoms for non existing type, this prevent atom allocation overflow
		    case lists:member(Type, Plugins) of
			false ->
			    {error, extended_error(
					?ERR_FEATURE_NOT_IMPLEMENTED,
					unsupported, "create-nodes")};
			true ->
			    create_node(Host, ServerHost, Node, From,
					Type, Access, Config)
		    end;
		{set, "publish"} ->
		    case xml:remove_cdata(Els) of
			[{xmlelement, "item", ItemAttrs, Payload}] ->
			    ItemId = xml:get_attr_s("id", ItemAttrs),
			    publish_item(Host, ServerHost, Node, From, ItemId, Payload);
			[] ->
			    %% Publisher attempts to publish to persistent node with no item
			    {error, extended_error(?ERR_BAD_REQUEST,
						   "item-required")};
			_ ->
			    %% Entity attempts to publish item with multiple payload elements or namespace does not match
			    {error, extended_error(?ERR_BAD_REQUEST,
						   "invalid-payload")}
		    end;
		{set, "retract"} ->
		    ForceNotify = case xml:get_attr_s("notify", Attrs) of
				      "1" -> true;
				      "true" -> true;
				      _ -> false
				  end,
		    case xml:remove_cdata(Els) of
			[{xmlelement, "item", ItemAttrs, _}] ->
			    ItemId = xml:get_attr_s("id", ItemAttrs),
			    delete_item(Host, Node, From, ItemId, ForceNotify);
			_ ->
			    %% Request does not specify an item
			    {error, extended_error(?ERR_BAD_REQUEST,
						   "item-required")}
		    end;
		{set, "subscribe"} ->
		    Config = case Rest of
			[{xmlelement, "options", _, C}] -> C;
			_ -> []
		    end,
		    JID = xml:get_attr_s("jid", Attrs),
		    subscribe_node(Host, Node, From, JID, Config);
		{set, "unsubscribe"} ->
		    JID = xml:get_attr_s("jid", Attrs),
		    SubId = xml:get_attr_s("subid", Attrs),
		    unsubscribe_node(Host, Node, From, JID, SubId);
		{get, "items"} ->
		    MaxItems = xml:get_attr_s("max_items", Attrs),
		    SubId = xml:get_attr_s("subid", Attrs),
		    ItemIDs = lists:foldl(fun
			({xmlelement, "item", ItemAttrs, _}, Acc) ->
			    case xml:get_attr_s("id", ItemAttrs) of
			    "" -> Acc;
			    ItemID -> [ItemID|Acc]
			    end;
			(_, Acc) ->
			    Acc
			end, [], xml:remove_cdata(Els)),
		    RSM = jlib:rsm_decode(SubEl),
		    get_items(Host, Node, From, SubId, MaxItems, ItemIDs, RSM);
		{get, "subscriptions"} ->
		    get_subscriptions(Host, Node, From, Plugins);
		{get, "affiliations"} ->
		    get_affiliations(Host, Node, From, Plugins);
		{get, "options"} ->
		    SubID = xml:get_attr_s("subid", Attrs),
		    JID = xml:get_attr_s("jid", Attrs),
		    get_options(Host, Node, JID, SubID, Lang);
		{set, "options"} ->
		    SubID = xml:get_attr_s("subid", Attrs),
		    JID = xml:get_attr_s("jid", Attrs),
		    set_options(Host, Node, JID, SubID, Els);
		_ ->
		    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	    end;
	Other ->
	    ?INFO_MSG("Too many actions: ~p", [Other]),
	    {error, ?ERR_BAD_REQUEST}
    end.

iq_pubsub_owner(Host, ServerHost, From, IQType, SubEl, Lang) ->
    {xmlelement, _, _, SubEls} = SubEl,
    Action = lists:filter(fun({xmlelement, "set", _, _}) -> false;
			    (_) -> true
			end, xml:remove_cdata(SubEls)),
    case Action of
	[{xmlelement, Name, Attrs, Els}] ->
	    Node = string_to_node(xml:get_attr_s("node", Attrs)),
	    case {IQType, Name} of
		{get, "configure"} ->
		    get_configure(Host, ServerHost, Node, From, Lang);
		{set, "configure"} ->
		    set_configure(Host, Node, From, Els, Lang);
		{get, "default"} ->
		    get_default(Host, Node, From, Lang);
		{set, "delete"} ->
		    delete_node(Host, Node, From);
		{set, "purge"} ->
		    purge_node(Host, Node, From);
		{get, "subscriptions"} ->
		    get_subscriptions(Host, Node, From);
		{set, "subscriptions"} ->
		    set_subscriptions(Host, Node, From, xml:remove_cdata(Els));
		{get, "affiliations"} ->
		    get_affiliations(Host, Node, From);
		{set, "affiliations"} ->
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
	Err ->
	    Err
    end.

%% @doc <p>Processes an Ad Hoc Command.</p>
adhoc_request(Host, _ServerHost, Owner,
	      #adhoc_request{node   = ?NS_PUBSUB_GET_PENDING,
			     lang   = Lang,
			     action = "execute",
			     xdata  = false},
	     _Access, Plugins) ->
    send_pending_node_form(Host, Owner, Lang, Plugins);
adhoc_request(Host, _ServerHost, Owner,
	      #adhoc_request{node   = ?NS_PUBSUB_GET_PENDING,
			     action = "execute",
			     xdata  = XData},
	     _Access, _Plugins) ->
    ParseOptions = case XData of
		       {xmlelement, "x", _Attrs, _SubEls} = XEl ->
			   case jlib:parse_xdata_submit(XEl) of
			       invalid ->
				   {error, ?ERR_BAD_REQUEST};
			       XData2 ->
				   case set_xoption(Host, XData2, []) of
				       NewOpts when is_list(NewOpts) ->
					   {result, NewOpts};
				       Err ->
					   Err
				   end
			   end;
		       _ ->
			   ?INFO_MSG("Bad XForm: ~p", [XData]),
			   {error, ?ERR_BAD_REQUEST}
		   end,
    case ParseOptions of
	{result, XForm} ->
	    case lists:keysearch(node, 1, XForm) of
		{value, {_, Node}} ->
		    send_pending_auth_events(Host, Node, Owner);
		false ->
		    {error, extended_error(?ERR_BAD_REQUEST, "bad-payload")}
	    end;
	Error ->
	    Error
    end;
adhoc_request(_Host, _ServerHost, _Owner, #adhoc_request{action = "cancel"},
              _Access, _Plugins) ->
    #adhoc_response{status = canceled};
adhoc_request(Host, ServerHost, Owner, #adhoc_request{action = []} = R,
              Access, Plugins) ->
    adhoc_request(Host, ServerHost, Owner, R#adhoc_request{action = "execute"},
                  Access, Plugins);
adhoc_request(_Host, _ServerHost, _Owner, Other, _Access, _Plugins) ->
    ?DEBUG("Couldn't process ad hoc command:~n~p", [Other]),
    {error, ?ERR_ITEM_NOT_FOUND}.

%% @spec (Host, Owner, Lang, Plugins) -> iqRes()
%% @doc <p>Sends the process pending subscriptions XForm for Host to
%% Owner.</p>
send_pending_node_form(Host, Owner, _Lang, Plugins) ->
    Filter =
	fun (Plugin) ->
		lists:member("get-pending", features(Plugin))
	end,
    case lists:filter(Filter, Plugins) of
	[] ->
	    {error, ?ERR_FEATURE_NOT_IMPLEMENTED};
	Ps ->
	    XOpts = lists:map(fun (Node) ->
				      {xmlelement, "option", [],
				       [{xmlelement, "value", [],
					 [{xmlcdata, node_to_string(Node)}]}]}
			      end, get_pending_nodes(Host, Owner, Ps)),
	    XForm = {xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
		     [{xmlelement, "field",
		       [{"type", "list-single"}, {"var", "pubsub#node"}],
		       lists:usort(XOpts)}]},
	    #adhoc_response{status = executing,
			    defaultaction = "execute",
			    elements = [XForm]}
    end.

get_pending_nodes(Host, Owner, Plugins) ->
    Tr =
	fun (Type) ->
		case node_call(Type, get_pending_nodes, [Host, Owner]) of
		    {result, Nodes} -> Nodes;
		    _	       -> []
		end
	end,
    case transaction(Host,
		     fun () -> {result, lists:flatmap(Tr, Plugins)} end,
		     sync_dirty) of
	{result, Res} -> Res;
	Err	   -> Err
    end.

%% @spec (Host, Node, Owner) -> iqRes()
%% @doc <p>Send a subscription approval form to Owner for all pending
%% subscriptions on Host and Node.</p>
send_pending_auth_events(Host, Node, Owner) ->
    ?DEBUG("Sending pending auth events for ~s on ~s:~s",
	   [jlib:jid_to_string(Owner), Host, node_to_string(Node)]),
    Action =
	fun (#pubsub_node{id = NodeID, type = Type}) ->
		case lists:member("get-pending", features(Type)) of
		    true ->
			case node_call(Type, get_affiliation, [NodeID, Owner]) of
			    {result, owner} ->
				node_call(Type, get_node_subscriptions, [NodeID]);
			    _ ->
				{error, ?ERR_FORBIDDEN}
			end;
		    false ->
			{error, ?ERR_FEATURE_NOT_IMPLEMENTED}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {N, Subscriptions}} ->
	    lists:foreach(fun({J, pending, _SubID}) -> send_authorization_request(N, jlib:make_jid(J));
			     ({J, pending}) -> send_authorization_request(N, jlib:make_jid(J));
			     (_) -> ok
			  end, Subscriptions),
	    #adhoc_response{};
	Err ->
	    Err
    end.

%%% authorization handling

send_authorization_request(#pubsub_node{nodeid = {Host, Node}, type = Type, id = NodeId}, Subscriber) ->
    Lang = "en", %% TODO fix
    Stanza = {xmlelement, "message",
	      [],
	      [{xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
		[{xmlelement, "title", [],
		  [{xmlcdata, translate:translate(Lang, "PubSub subscriber request")}]},
		 {xmlelement, "instructions", [],
		  [{xmlcdata, translate:translate(Lang, "Choose whether to approve this entity's subscription.")}]},
		 {xmlelement, "field",
		  [{"var", "FORM_TYPE"}, {"type", "hidden"}],
		  [{xmlelement, "value", [], [{xmlcdata, ?NS_PUBSUB_SUB_AUTH}]}]},
		 {xmlelement, "field",
		  [{"var", "pubsub#node"}, {"type", "text-single"},
		   {"label", translate:translate(Lang, "Node ID")}],
		  [{xmlelement, "value", [],
		    [{xmlcdata, node_to_string(Node)}]}]},
		 {xmlelement, "field", [{"var", "pubsub#subscriber_jid"},
					{"type", "jid-single"},
					{"label", translate:translate(Lang, "Subscriber Address")}],
		  [{xmlelement, "value", [],
		    [{xmlcdata, jlib:jid_to_string(Subscriber)}]}]},
		 {xmlelement, "field",
		  [{"var", "pubsub#allow"},
		   {"type", "boolean"},
		   {"label", translate:translate(Lang, "Allow this Jabber ID to subscribe to this pubsub node?")}],
		  [{xmlelement, "value", [], [{xmlcdata, "false"}]}]}]}]},
    lists:foreach(fun(Owner) ->
	ejabberd_router:route(service_jid(Host), jlib:make_jid(Owner), Stanza)
    end, node_owners(Host, Type, NodeId)).

find_authorization_response(Packet) ->
    {xmlelement, _Name, _Attrs, Els} = Packet,
    XData1 = lists:map(fun({xmlelement, "x", XAttrs, _} = XEl) ->
			       case xml:get_attr_s("xmlns", XAttrs) of
				   ?NS_XDATA ->
				       case xml:get_attr_s("type", XAttrs) of
					   "cancel" ->
					       none;
					   _ ->
					       jlib:parse_xdata_submit(XEl)
				       end;
				   _ ->
				       none
			       end;
			  (_) ->
			       none
		       end, xml:remove_cdata(Els)),
    XData = lists:filter(fun(E) -> E /= none end, XData1),
    case XData of
	[invalid] -> invalid;
	[] -> none;
	[XFields] when is_list(XFields) ->
	    ?DEBUG("XFields: ~p", [XFields]),
	    case lists:keysearch("FORM_TYPE", 1, XFields) of
		{value, {_, [?NS_PUBSUB_SUB_AUTH]}} ->
		    XFields;
		_ ->
		    invalid
	    end
    end.

%% @spec (Host, JID, Node, Subscription) -> void
%%	 Host = mod_pubsub:host()
%%	 JID = jlib:jid()
%%	 SNode = string()
%%	 Subscription = atom() | {atom(), mod_pubsub:subid()}
%% @doc Send a message to JID with the supplied Subscription
send_authorization_approval(Host, JID, SNode, Subscription) ->
    SubAttrs = case Subscription of
		   {S, SID} -> [{"subscription", subscription_to_string(S)},
				{"subid", SID}];
		   S	-> [{"subscription", subscription_to_string(S)}]
	       end,
    Stanza = event_stanza(
	[{xmlelement, "subscription",
	  [{"jid", jlib:jid_to_string(JID)}|nodeAttr(SNode)] ++ SubAttrs,
	  []}]),
    ejabberd_router:route(service_jid(Host), JID, Stanza).

handle_authorization_response(Host, From, To, Packet, XFields) ->
    case {lists:keysearch("pubsub#node", 1, XFields),
	  lists:keysearch("pubsub#subscriber_jid", 1, XFields),
	  lists:keysearch("pubsub#allow", 1, XFields)} of
	{{value, {_, [SNode]}}, {value, {_, [SSubscriber]}},
	 {value, {_, [SAllow]}}} ->
	    Node = string_to_node(SNode),
	    Subscriber = jlib:string_to_jid(SSubscriber),
	    Allow = case SAllow of
			"1" -> true;
			"true" -> true;
			_ -> false
		    end,
	    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
			     IsApprover = lists:member(jlib:jid_tolower(jlib:jid_remove_resource(From)), node_owners_call(Type, NodeId)),
			     {result, Subscriptions} = node_call(Type, get_subscriptions, [NodeId, Subscriber]),
			     if
				 not IsApprover ->
				     {error, ?ERR_FORBIDDEN};
				 true ->
				     update_auth(Host, SNode, Type, NodeId,
						 Subscriber, Allow,
						 Subscriptions)
			     end
		     end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{error, Error} ->
		    ejabberd_router:route(
		     To, From,
		     jlib:make_error_reply(Packet, Error));
		{result, {_, _NewSubscription}} ->
		    %% XXX: notify about subscription state change, section 12.11
		    ok;
		_ ->
		    ejabberd_router:route(
		      To, From,
		      jlib:make_error_reply(Packet, ?ERR_INTERNAL_SERVER_ERROR))
	    end;
	_ ->
	    ejabberd_router:route(
	      To, From,
	      jlib:make_error_reply(Packet, ?ERR_NOT_ACCEPTABLE))
    end.

update_auth(Host, Node, Type, NodeId, Subscriber,
	    Allow, Subscriptions) ->
    Subscription = lists:filter(fun({pending, _}) -> true;
				    (_)	    -> false
				end, Subscriptions),
    case Subscription of
	[{pending, SubID}] -> %% TODO does not work if several pending
	    NewSubscription = case Allow of
				  true  -> subscribed;
				  false -> none
			      end,
	    node_call(Type, set_subscriptions,
		      [NodeId, Subscriber, NewSubscription, SubID]),
	    send_authorization_approval(Host, Subscriber, Node,
					NewSubscription),
	    {result, ok};
	_ ->
	    {error, ?ERR_UNEXPECTED_REQUEST}
    end.

-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(BOOLXFIELD(Label, Var, Val),
	?XFIELD("boolean", Label, Var,
		case Val of
		    true -> "1";
		    _ -> "0"
		end)).

-define(STRINGXFIELD(Label, Var, Val),
	?XFIELD("text-single", Label, Var, Val)).

-define(STRINGMXFIELD(Label, Var, Vals),
	{xmlelement, "field", [{"type", "text-multi"},
				{"label", translate:translate(Lang, Label)},
				{"var", Var}],
			[{xmlelement, "value", [], [{xmlcdata, V}]} || V <- Vals]}).

-define(XFIELDOPT(Type, Label, Var, Val, Opts),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 lists:map(fun(Opt) ->
			   {xmlelement, "option", [],
			    [{xmlelement, "value", [],
			      [{xmlcdata, Opt}]}]}
		   end, Opts) ++
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(LISTXFIELD(Label, Var, Val, Opts),
	?XFIELDOPT("list-single", Label, Var, Val, Opts)).

-define(LISTMXFIELD(Label, Var, Vals, Opts),
	{xmlelement, "field", [{"type", "list-multi"},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 lists:map(fun(Opt) ->
			    {xmlelement, "option", [],
			     [{xmlelement, "value", [],
			       [{xmlcdata, Opt}]}]}
		    end, Opts) ++
	 lists:map(fun(Val) ->
			    {xmlelement, "value", [],
			     [{xmlcdata, Val}]}
		    end, Vals)}).

%% @spec (Host::host(), ServerHost::host(), Node::pubsubNode(), Owner::jid(), NodeType::nodeType()) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
%% @doc <p>Create new pubsub nodes</p>
%%<p>In addition to method-specific error conditions, there are several general reasons why the node creation request might fail:</p>
%%<ul>
%%<li>The service does not support node creation.</li>
%%<li>Only entities that are registered with the service are allowed to create nodes but the requesting entity is not registered.</li>
%%<li>The requesting entity does not have sufficient privileges to create nodes.</li>
%%<li>The requested NodeID already exists.</li>
%%<li>The request did not include a NodeID and "instant nodes" are not supported.</li>
%%</ul>
%%<p>ote: node creation is a particular case, error return code is evaluated at many places:</p>
%%<ul>
%%<li>iq_pubsub checks if service supports node creation (type exists)</li>
%%<li>create_node checks if instant nodes are supported</li>
%%<li>create_node asks node plugin if entity have sufficient privilege</li>
%%<li>nodetree create_node checks if nodeid already exists</li>
%%<li>node plugin create_node just sets default affiliation/subscription</li>
%%</ul>
create_node(Host, ServerHost, Node, Owner, Type) ->
    create_node(Host, ServerHost, Node, Owner, Type, all, []).
create_node(Host, ServerHost, <<>>, Owner, Type, Access, Configuration) ->
    case lists:member("instant-nodes", features(Type)) of
	true ->
	    NewNode = string_to_node(randoms:get_string()),
	    case create_node(Host, ServerHost,
			     NewNode, Owner, Type, Access, Configuration) of
		{result, []} ->
		    {result,
		     [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		       [{xmlelement, "create", nodeAttr(NewNode), []}]}]};
		Error ->
		    Error
	    end;
	false ->
	    %% Service does not support instant nodes
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, "nodeid-required")}
    end;
create_node(Host, ServerHost, Node, Owner, GivenType, Access, Configuration) ->
    Type = select_type(ServerHost, Host, Node, GivenType),
    %% TODO, check/set node_type = Type
    ParseOptions = case xml:remove_cdata(Configuration) of
		       [] ->
			   {result, node_options(Type)};
		       [{xmlelement, "x", _Attrs, _SubEls} = XEl] ->
			   case jlib:parse_xdata_submit(XEl) of
			       invalid ->
				   {error, ?ERR_BAD_REQUEST};
			       XData ->
				   case set_xoption(Host, XData, node_options(Type)) of
				       NewOpts when is_list(NewOpts) ->
					   {result, NewOpts};
				       Err ->
					   Err
				   end
			   end;
		       _ ->
			   ?INFO_MSG("Node ~p; bad configuration: ~p", [Node, Configuration]),
			   {error, ?ERR_BAD_REQUEST}
		   end,
    case ParseOptions of
	{result, NodeOptions} ->
	    CreateNode =
		fun() ->
			SNode = node_to_string(Node),
			Parent = case node_call(Type, node_to_path, [Node]) of
			    {result, [SNode]} -> <<>>;
			    {result, Path} -> element(2, node_call(Type, path_to_node, [lists:sublist(Path, length(Path)-1)]))
			end,
			Parents = case Parent of
			    <<>> -> [];
			    _ -> [Parent]
			end,
			case node_call(Type, create_node_permission, [Host, ServerHost, Node, Parent, Owner, Access]) of
			    {result, true} ->
				case tree_call(Host, create_node, [Host, Node, Type, Owner, NodeOptions, Parents]) of
				    {ok, NodeId} ->
					ParentTree = tree_call(Host, get_parentnodes_tree, [Host, Node, Owner]),
					SubsByDepth = [{Depth, [{N, get_node_subs(N)} || N <- Nodes]} || {Depth, Nodes} <- ParentTree],
					case node_call(Type, create_node, [NodeId, Owner]) of
					    {result, Result} -> {result, {NodeId, SubsByDepth, Result}};
					    Error -> Error
					end;
				    {error, {virtual, NodeId}} ->
					case node_call(Type, create_node, [NodeId, Owner]) of
					    {result, Result} -> {result, {NodeId, [], Result}};
					    Error -> Error
					end;
				    Error ->
					Error
				end;
			    _ ->
				{error, ?ERR_FORBIDDEN}
			end
		end,
	    Reply = [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		      [{xmlelement, "create", nodeAttr(Node),
			[]}]}],
	    case transaction(Host, CreateNode, transaction) of
		{result, {NodeId, SubsByDepth, {Result, broadcast}}} ->
		    broadcast_created_node(Host, Node, NodeId, Type, NodeOptions, SubsByDepth),
		    ejabberd_hooks:run(pubsub_create_node, ServerHost, [ServerHost, Host, Node, NodeId, NodeOptions]),
		    case Result of
			default -> {result, Reply};
			_ -> {result, Result}
		    end;
		{result, {NodeId, _SubsByDepth, default}} ->
		    ejabberd_hooks:run(pubsub_create_node, ServerHost, [ServerHost, Host, Node, NodeId, NodeOptions]),
		    {result, Reply};
		{result, {NodeId, _SubsByDepth, Result}} ->
		    ejabberd_hooks:run(pubsub_create_node, ServerHost, [ServerHost, Host, Node, NodeId, NodeOptions]),
		    {result, Result};
		Error ->
		    %% in case we change transaction to sync_dirty...
		    %%  node_call(Type, delete_node, [Host, Node]),
		    %%  tree_call(Host, delete_node, [Host, Node]),
		    Error
	    end;
	Error ->
	    Error
    end.

%% @spec (Host, Node, Owner) ->
%%			{error, Reason} | {result, []}
%%	 Host = host()
%%	 Node = pubsubNode()
%%	 Owner = jid()
%%	 Reason = stanzaError()
%% @doc <p>Delete specified node and all childs.</p>
%%<p>There are several reasons why the node deletion request might fail:</p>
%%<ul>
%%<li>The requesting entity does not have sufficient privileges to delete the node.</li>
%%<li>The node is the root collection node, which cannot be deleted.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
delete_node(_Host, <<>>, _Owner) ->
    %% Node is the root
    {error, ?ERR_NOT_ALLOWED};
delete_node(Host, Node, Owner) ->
    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
		    case node_call(Type, get_affiliation, [NodeId, Owner]) of
			{result, owner} ->
			    ParentTree = tree_call(Host, get_parentnodes_tree, [Host, Node, service_jid(Host)]),
			    SubsByDepth = [{Depth, [{N, get_node_subs(N)} || N <- Nodes]} || {Depth, Nodes} <- ParentTree],
			    Removed = tree_call(Host, delete_node, [Host, Node]),
			    case node_call(Type, delete_node, [Removed]) of
				{result, Res} -> {result, {SubsByDepth, Res}};
				Error -> Error
			    end;
			_ ->
			    %% Entity is not an owner
			    {error, ?ERR_FORBIDDEN}
		    end
	     end,
    Reply = [],
    ServerHost = get(server_host), % not clean, but prevent many API changes
    case transaction(Host, Node, Action, transaction) of
	{result, {_TNode, {SubsByDepth, {Result, broadcast, Removed}}}} ->
	    lists:foreach(fun({RNode, _RSubscriptions}) ->
		{RH, RN} = RNode#pubsub_node.nodeid,
		NodeId = RNode#pubsub_node.id,
		Type = RNode#pubsub_node.type,
		Options = RNode#pubsub_node.options,
		broadcast_removed_node(RH, RN, NodeId, Type, Options, SubsByDepth),
		ejabberd_hooks:run(pubsub_delete_node, ServerHost, [ServerHost, RH, RN, NodeId])
	    end, Removed),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {_TNode, {_, {Result, Removed}}}} ->
	    lists:foreach(fun({RNode, _RSubscriptions}) ->
		{RH, RN} = RNode#pubsub_node.nodeid,
		NodeId = RNode#pubsub_node.id,
		ejabberd_hooks:run(pubsub_delete_node, ServerHost, [ServerHost, RH, RN, NodeId])
	    end, Removed),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {TNode, {_, default}}} ->
	    NodeId = TNode#pubsub_node.id,
	    ejabberd_hooks:run(pubsub_delete_node, ServerHost, [ServerHost, Host, Node, NodeId]),
	    {result, Reply};
	{result, {TNode, {_, Result}}} ->
	    NodeId = TNode#pubsub_node.id,
	    ejabberd_hooks:run(pubsub_delete_node, ServerHost, [ServerHost, Host, Node, NodeId]),
	    {result, Result};
	Error ->
	    Error
    end.

%% @spec (Host, Node, From, JID, Configuration) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
%%	 Host = host()
%%	 Node = pubsubNode()
%%	 From = jid()
%%	 JID = jid()
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
subscribe_node(Host, Node, From, JID, Configuration) ->
    SubOpts = case pubsub_subscription_odbc:parse_options_xform(Configuration) of
	{result, GoodSubOpts} -> GoodSubOpts;
	_ -> invalid
    end,
    Subscriber = case jlib:string_to_jid(JID) of
		     error -> {"", "", ""};
		     J -> jlib:jid_tolower(J)
		 end,
    Action = fun(#pubsub_node{options = Options, type = Type, id = NodeId}) ->
		    Features = features(Type),
		    SubscribeFeature = lists:member("subscribe", Features),
		    OptionsFeature = lists:member("subscription-options", Features),
		    HasOptions = not (SubOpts == []),
		    SubscribeConfig = get_option(Options, subscribe),
		    AccessModel = get_option(Options, access_model),
		    SendLast = get_option(Options, send_last_published_item),
		    AllowedGroups = get_option(Options, roster_groups_allowed, []),
		    Owners = node_owners_call(Type, NodeId),
		    {PresenceSubscription, RosterGroup} = get_presence_and_roster_permissions(Host, Subscriber, Owners, AccessModel, AllowedGroups),
		    if
			not SubscribeFeature ->
			    %% Node does not support subscriptions
			    {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "subscribe")};
			not SubscribeConfig ->
			    %% Node does not support subscriptions
			    {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "subscribe")};
			HasOptions andalso not OptionsFeature ->
			    %% Node does not support subscription options
			    {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "subscription-options")};
			SubOpts == invalid ->
			    %% Passed invalit options submit form
			    {error, extended_error(?ERR_BAD_REQUEST, "invalid-options")};
			true ->
			    node_call(Type, subscribe_node,
					[NodeId, From, Subscriber,
					AccessModel, SendLast,
					PresenceSubscription, RosterGroup,
					SubOpts])
		    end
	    end,
    Reply = fun(Subscription) ->
		    %% TODO, this is subscription-notification, should depends on node features
		    SubAttrs = case Subscription of
				   {subscribed, SubId} ->
				       [{"subscription", subscription_to_string(subscribed)},
					{"subid", SubId},
					{"node",Node}];
				   Other ->
				       [{"subscription", subscription_to_string(Other)},
				        {"node", Node}]
			       end,
		    Fields =
			[{"jid", jlib:jid_to_string(Subscriber)} | SubAttrs],
		    [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}], 
			[{xmlelement, "subscription", Fields, []}]}]
	    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, subscribed, SubId, send_last}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    send_items(Host, Node, NodeId, Type, Subscriber, last),
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
	    %% this case should never occure anyway
	    {result, Result};
	Error ->
	    Error
    end.

%% @spec (Host, Noce, From, JID, SubId) -> {error, Reason} | {result, []}
%%	 Host = host()
%%	 Node = pubsubNode()
%%	 From = jid()
%%	 JID = string()
%%	 SubId = string()
%%	 Reason = stanzaError()
%% @doc <p>Unsubscribe <tt>JID</tt> from the <tt>Node</tt>.</p>
%%<p>There are several reasons why the unsubscribe request might fail:</p>
%%<ul>
%%<li>The requesting entity has multiple subscriptions to the node but does not specify a subscription ID.</li>
%%<li>The request does not specify an existing subscriber.</li>
%%<li>The requesting entity does not have sufficient privileges to unsubscribe the specified JID.</li>
%%<li>The node does not exist.</li>
%%<li>The request specifies a subscription ID that is not valid or current.</li>
%%</ul>
unsubscribe_node(Host, Node, From, JID, SubId) when is_list(JID) ->
    Subscriber = case jlib:string_to_jid(JID) of
		     error -> {"", "", ""};
		     J -> jlib:jid_tolower(J)
		 end,
    unsubscribe_node(Host, Node, From, Subscriber, SubId);
unsubscribe_node(Host, Node, From, Subscriber, SubId) ->
    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
		    node_call(Type, unsubscribe_node, [NodeId, From, Subscriber, SubId])
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, default}} ->
	    {result, []};
	{result, {_, Result}} ->
	    {result, Result};
	Error ->
	    Error
    end.

%% @spec (Host::host(), ServerHost::host(), JID::jid(), Node::pubsubNode(), ItemId::string(), Payload::term())  ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
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
publish_item(Host, ServerHost, Node, Publisher, "", Payload) ->
    %% if publisher does not specify an ItemId, the service MUST generate the ItemId
    publish_item(Host, ServerHost, Node, Publisher, uniqid(), Payload);
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload) ->
    Action = fun(#pubsub_node{options = Options, type = Type, id = NodeId}) ->
		    Features = features(Type),
		    PublishFeature = lists:member("publish", Features),
		    PublishModel = get_option(Options, publish_model),
		    MaxItems = max_items(Host, Options),
		    DeliverPayloads = get_option(Options, deliver_payloads),
		    PersistItems = get_option(Options, persist_items),
		    PayloadCount = payload_xmlelements(Payload),
		    PayloadSize = size(term_to_binary(Payload))-2, % size(term_to_binary([])) == 2
		    PayloadMaxSize = get_option(Options, max_payload_size),
		    % pubsub#deliver_payloads true 
		    % pubsub#persist_items true -> 1 item; false -> 0 item
		    if
			not PublishFeature ->
			    %% Node does not support item publication
			    {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "publish")};
			PayloadSize > PayloadMaxSize ->
			    %% Entity attempts to publish very large payload
			    {error, extended_error(?ERR_NOT_ACCEPTABLE, "payload-too-big")};
			(PayloadCount == 0) and (Payload == []) ->
			    %% Publisher attempts to publish to payload node with no payload
			    {error, extended_error(?ERR_BAD_REQUEST, "payload-required")};
			(PayloadCount > 1) or (PayloadCount == 0) ->
			    %% Entity attempts to publish item with multiple payload elements
			    {error, extended_error(?ERR_BAD_REQUEST, "invalid-payload")};
			(DeliverPayloads == 0) and (PersistItems == 0) and (PayloadSize > 0) ->
			    %% Publisher attempts to publish to transient notification node with item
			    {error, extended_error(?ERR_BAD_REQUEST, "item-forbidden")};
			((DeliverPayloads == 1) or (PersistItems == 1)) and (PayloadSize == 0) ->
			    %% Publisher attempts to publish to persistent node with no item
			    {error, extended_error(?ERR_BAD_REQUEST, "item-required")};
			true ->
			    node_call(Type, publish_item, [NodeId, Publisher, PublishModel, MaxItems, ItemId, Payload])
		    end
	    end,
    ejabberd_hooks:run(pubsub_publish_item, ServerHost, [ServerHost, Node, Publisher, service_jid(Host), ItemId, Payload]),
    Reply = [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}], 
		[{xmlelement, "publish", nodeAttr(Node),
		    [{xmlelement, "item", itemAttr(ItemId), []}]}]}],
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, Broadcast, Removed}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    case get_option(Options, deliver_notifications) of
			true ->
				BroadcastPayload = case Broadcast of
					default -> Payload;
					broadcast -> Payload;
					PluginPayload -> PluginPayload
				end,
				broadcast_publish_item(Host, Node, NodeId, Type, Options,
					Removed, ItemId, jlib:jid_tolower(Publisher),
					BroadcastPayload);
			false ->
				ok
		end,
	    set_cached_item(Host, NodeId, ItemId, Publisher, Payload),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {TNode, {default, Removed}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, NodeId, Type, Options, Removed),
	    set_cached_item(Host, NodeId, ItemId, Publisher, Payload),
	    {result, Reply};
	{result, {TNode, {Result, Removed}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, NodeId, Type, Options, Removed),
	    set_cached_item(Host, NodeId, ItemId, Publisher, Payload),
	    {result, Result};
	{result, {_, default}} ->
	    {result, Reply};
	{result, {_, Result}} ->
	    {result, Result};
	{error, ?ERR_ITEM_NOT_FOUND} ->
	    %% handles auto-create feature
	    %% for automatic node creation. we'll take the default node type:
	    %% first listed into the plugins configuration option, or pep
	    Type = select_type(ServerHost, Host, Node),
	    case lists:member("auto-create", features(Type)) of
		true ->
		    case create_node(Host, ServerHost, Node, Publisher, Type) of
			{result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
			  [{xmlelement, "create", [{"node", NewNode}], []}]}]} ->
			    publish_item(Host, ServerHost,  list_to_binary(NewNode),
				    Publisher, ItemId, Payload);
			_ ->
			    {error, ?ERR_ITEM_NOT_FOUND}
		    end;
		false ->
		    {error, ?ERR_ITEM_NOT_FOUND}
	    end;
	Error ->
	    Error
    end.

%% @spec (Host::host(), JID::jid(), Node::pubsubNode(), ItemId::string()) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
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
delete_item(Host, Node, Publisher, ItemId) ->
    delete_item(Host, Node, Publisher, ItemId, false).
delete_item(_, "", _, _, _) ->
    %% Request does not specify a node
    {error, extended_error(?ERR_BAD_REQUEST, "node-required")};
delete_item(Host, Node, Publisher, ItemId, ForceNotify) ->
    Action = fun(#pubsub_node{options = Options, type = Type, id = NodeId}) ->
		    Features = features(Type),
		    PersistentFeature = lists:member("persistent-items", Features),
		    DeleteFeature = lists:member("delete-items", Features),
		    PublishModel = get_option(Options, publish_model),
		    if
			%%->   iq_pubsub just does that matchs
			%%	%% Request does not specify an item
			%%	{error, extended_error(?ERR_BAD_REQUEST, "item-required")};
			not PersistentFeature ->
			    %% Node does not support persistent items
			    {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "persistent-items")};
			not DeleteFeature ->
			    %% Service does not support item deletion
			    {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "delete-items")};
			true ->
			    node_call(Type, delete_item, [NodeId, Publisher, PublishModel, ItemId])
		    end
	     end,
    Reply = [],
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, broadcast}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, NodeId, Type, Options, [ItemId], ForceNotify),
	    case get_cached_item(Host, NodeId) of
	    #pubsub_item{itemid = {ItemId, NodeId}, _ = '_'} -> unset_cached_item(Host, NodeId);
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

%% @spec (Host, JID, Node) ->
%%			{error, Reason} | {result, []}
%%	 Host = host()
%%	 Node = pubsubNode()
%%	 JID = jid()
%%	 Reason = stanzaError()
%% @doc <p>Delete all items of specified node owned by JID.</p>
%%<p>There are several reasons why the node purge request might fail:</p>
%%<ul>
%%<li>The node or service does not support node purging.</li>
%%<li>The requesting entity does not have sufficient privileges to purge the node.</li>
%%<li>The node is not configured to persist items.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
purge_node(Host, Node, Owner) ->
    Action = fun(#pubsub_node{options = Options, type = Type, id = NodeId}) ->
		     Features = features(Type),
		     PurgeFeature = lists:member("purge-nodes", Features),
		     PersistentFeature = lists:member("persistent-items", Features),
		     PersistentConfig = get_option(Options, persist_items),
		     if
			 not PurgeFeature ->
			     %% Service does not support node purging
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "purge-nodes")};
			 not PersistentFeature ->
			     %% Node does not support persistent items
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "persistent-items")};
			 not PersistentConfig ->
			     %% Node is not configured for persistent items
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "persistent-items")};
			 true ->
			     node_call(Type, purge_node, [NodeId, Owner])
		     end
	     end,
    Reply = [],
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, broadcast}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_purge_node(Host, Node, NodeId, Type, Options),
	    unset_cached_item(Host, NodeId),
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
get_items(Host, Node, From, SubId, SMaxItems, ItemIDs, RSM) ->
    MaxItems =
	if
	    SMaxItems == "" -> get_max_items_node(Host);
	    true ->
		case catch list_to_integer(SMaxItems) of
		    {'EXIT', _} -> {error, ?ERR_BAD_REQUEST};
		    Val -> Val
		end
	end,
    case MaxItems of
	{error, Error} ->
	    {error, Error};
	_ ->
	    Action = fun(#pubsub_node{options = Options, type = Type, id = NodeId}) ->
		     Features = features(Type),
		     RetreiveFeature = lists:member("retrieve-items", Features),
		     PersistentFeature = lists:member("persistent-items", Features),
		     AccessModel = get_option(Options, access_model),
		     AllowedGroups = get_option(Options, roster_groups_allowed, []),
		     Owners = node_owners_call(Type, NodeId),
		     {PresenceSubscription, RosterGroup} = get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups),
		     if
			 not RetreiveFeature ->
			     %% Item Retrieval Not Supported
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "retrieve-items")};
			 not PersistentFeature ->
			     %% Persistent Items Not Supported
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "persistent-items")};
			 true ->
			     node_call(Type, get_items,
				       [NodeId, From,
					AccessModel, PresenceSubscription, RosterGroup,
					SubId, RSM])
		     end
	     end,
	     case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, {Items, RSMOut}}} ->
		    SendItems = case ItemIDs of
			[] -> 
			    Items;
			_ ->
			    lists:filter(fun(#pubsub_item{itemid = {ItemId, _}}) ->
				lists:member(ItemId, ItemIDs)
			    end, Items) 
			end,
		    %% Generate the XML response (Item list), limiting the
		    %% number of items sent to MaxItems:
		    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
				[{xmlelement, "items", nodeAttr(Node),
				  itemsEls(lists:sublist(SendItems, MaxItems))}
				  | jlib:rsm_encode(RSMOut)]}]};
		Error ->
		    Error
	    end
    end.
get_items(Host, Node) ->
    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
	node_call(Type, get_items, [NodeId, service_jid(Host)])
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Items}} -> Items;
	Error -> Error
    end.
get_item(Host, Node, ItemId) ->
    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
	node_call(Type, get_item, [NodeId, ItemId])
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Items}} -> Items;
	Error -> Error
    end.
get_allowed_items_call(Host, NodeIdx, From, Type, Options, Owners) ->
    case get_allowed_items_call(Host, NodeIdx, From, Type, Options, Owners, none) of
	{result, {I, _}} -> {result, I};
	Error -> Error
    end.
get_allowed_items_call(Host, NodeIdx, From, Type, Options, Owners, RSM) ->
    AccessModel = get_option(Options, access_model),
    AllowedGroups = get_option(Options, roster_groups_allowed, []),
    {PresenceSubscription, RosterGroup} = get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups),
    node_call(Type, get_items, [NodeIdx, From, AccessModel, PresenceSubscription, RosterGroup, undefined, RSM]).


%% @spec (Host, Node, NodeId, Type, LJID, Number) -> any()
%%	 Host = pubsubHost()
%%	 Node = pubsubNode()
%%	 NodeId = pubsubNodeId()
%%	 Type = pubsubNodeType()
%%	 LJID = {U, S, []}
%%	 Number = last | integer()
%% @doc <p>Resend the items of a node to the user.</p>
%% @todo use cache-last-item feature
send_items(Host, Node, NodeId, Type, LJID, last) ->
    Stanza = case get_cached_item(Host, NodeId) of
	undefined ->
	    % special ODBC optimization, works only with node_hometree_odbc, node_flat_odbc and node_pep_odbc
	    case node_action(Host, Type, get_last_items, [NodeId, LJID, 1]) of
		{result, [LastItem]} ->
		    {ModifNow, ModifUSR} = LastItem#pubsub_item.modification,
		    event_stanza_with_delay(
			[{xmlelement, "items", nodeAttr(Node),
			  itemsEls([LastItem])}], ModifNow, ModifUSR);
		_ ->
		    event_stanza(
			[{xmlelement, "items", nodeAttr(Node),
			  itemsEls([])}])
	    end;
	LastItem ->
	    {ModifNow, ModifUSR} = LastItem#pubsub_item.modification,
	    event_stanza_with_delay(
		[{xmlelement, "items", nodeAttr(Node),
		  itemsEls([LastItem])}], ModifNow, ModifUSR)
    end,
    ejabberd_router:route(service_jid(Host), jlib:make_jid(LJID), Stanza);
send_items(Host, Node, NodeId, Type, LJID, Number) ->
    ToSend = case node_action(Host, Type, get_items, [NodeId, LJID]) of
	{result, []} -> 
	    [];
	{result, Items} ->
	    case Number of
		N when N > 0 -> lists:sublist(Items, N);
		_ -> Items
	    end;
	_ ->
	    []
    end,
    Stanza = case ToSend of
	[LastItem] ->
	    {ModifNow, ModifUSR} = LastItem#pubsub_item.modification,
	    event_stanza_with_delay(
		[{xmlelement, "items", nodeAttr(Node),
		  itemsEls(ToSend)}], ModifNow, ModifUSR);
	_ ->
	    event_stanza(
		[{xmlelement, "items", nodeAttr(Node),
		  itemsEls(ToSend)}])
    end,
    ejabberd_router:route(service_jid(Host), jlib:make_jid(LJID), Stanza).

%% @spec (Host, JID, Plugins) -> {error, Reason} | {result, Response}
%%	 Host = host()
%%	 JID = jid()
%%	 Plugins = [Plugin::string()]
%%	 Reason = stanzaError()
%%	 Response = [pubsubIQResponse()]
%% @doc <p>Return the list of affiliations as an XMPP response.</p>
get_affiliations(Host, <<>>, JID, Plugins) when is_list(Plugins) ->
    Result = lists:foldl(
	       fun(Type, {Status, Acc}) ->
		       Features = features(Type),
		       RetrieveFeature = lists:member("retrieve-affiliations", Features),
		       if
			   not RetrieveFeature ->
			       %% Service does not support retreive affiliatons
			       {{error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "retrieve-affiliations")}, Acc};
			   true ->
			       {result, Affiliations} = node_action(Host, Type, get_entity_affiliations, [Host, JID]),
			       {Status, [Affiliations|Acc]}
		       end
	       end, {ok, []}, Plugins),
    case Result of
	{ok, Affiliations} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({#pubsub_node{nodeid = {_, Node}}, Affiliation}) ->
				 [{xmlelement, "affiliation",
				   [{"affiliation", affiliation_to_string(Affiliation)}|nodeAttr(Node)],
				   []}]
			 end, lists:usort(lists:flatten(Affiliations))),
	    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		       [{xmlelement, "affiliations", [],
			 Entities}]}]};
	{Error, _} ->
	    Error
    end;
get_affiliations(Host, NodeId, JID, Plugins) when is_list(Plugins) ->
    Result = lists:foldl(
	       fun(Type, {Status, Acc}) ->
		       Features = features(Type),
		       RetrieveFeature = lists:member("retrieve-affiliations", Features),
		       if
			   not RetrieveFeature ->
			       %% Service does not support retreive affiliatons
			       {{error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "retrieve-affiliations")}, Acc};
			   true ->
			       {result, Affiliations} = node_action(Host, Type, get_entity_affiliations, [Host, JID]),
			       {Status, [Affiliations|Acc]}
		       end
	       end, {ok, []}, Plugins),
    case Result of
	{ok, Affiliations} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({#pubsub_node{nodeid = {_, Node}}, Affiliation})
			      when NodeId == Node ->
				 [{xmlelement, "affiliation",
				   [{"affiliation", affiliation_to_string(Affiliation)}|nodeAttr(Node)],
				   []}];
				(_) ->
				    []
			 end, lists:usort(lists:flatten(Affiliations))),
	    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		       [{xmlelement, "affiliations", [],
			 Entities}]}]};
	{Error, _} ->
	    Error
    end.


get_affiliations(Host, Node, JID) ->
    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
		     Features = features(Type),
		     RetrieveFeature = lists:member("modify-affiliations", Features),
		     {result, Affiliation} = node_call(Type, get_affiliation, [NodeId, JID]),
		     if
			 not RetrieveFeature ->
			     %% Service does not support modify affiliations
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "modify-affiliations")};
			 Affiliation /= owner ->
			     %% Entity is not an owner
			     {error, ?ERR_FORBIDDEN};
			 true ->
			     node_call(Type, get_node_affiliations, [NodeId])
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, []}} ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	{result, {_, Affiliations}} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({AJID, Affiliation}) ->
				 [{xmlelement, "affiliation",
				   [{"jid", jlib:jid_to_string(AJID)},
				    {"affiliation", affiliation_to_string(Affiliation)}],
				   []}]
			 end, Affiliations),
	    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB_OWNER}],
		       [{xmlelement, "affiliations", nodeAttr(Node),
			 Entities}]}]};
	Error ->
	    Error
    end.

set_affiliations(Host, Node, From, EntitiesEls) ->
    Owner = jlib:jid_tolower(jlib:jid_remove_resource(From)),
    Entities =
	lists:foldl(
	  fun(El, Acc) ->
		  case Acc of
		      error ->
			  error;
		      _ ->
			  case El of
			      {xmlelement, "affiliation", Attrs, _} ->
				  JID = jlib:string_to_jid(
					  xml:get_attr_s("jid", Attrs)),
				  Affiliation = string_to_affiliation(
						  xml:get_attr_s("affiliation", Attrs)),
				  if
				      (JID == error) or
				      (Affiliation == false) ->
					  error;
				      true ->
					  [{jlib:jid_tolower(JID), Affiliation} | Acc]
				  end
			  end
		  end
	  end, [], EntitiesEls),
    case Entities of
	error ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
			Owners = node_owners_call(Type, NodeId),
			case lists:member(Owner, Owners) of
			    true ->
				OwnerJID = jlib:make_jid(Owner),
				FilteredEntities = case Owners of
					[Owner] -> [E || E <- Entities, element(1, E) =/= OwnerJID];
					_ -> Entities
				    end,
				lists:foreach(
				    fun({JID, Affiliation}) ->
					node_call(Type, set_affiliation, [NodeId, JID, Affiliation])
				    end, FilteredEntities),
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

get_options(Host, Node, JID, SubID, Lang) ->
    Action = fun(#pubsub_node{type = Type, id = NodeID}) ->
		     case lists:member("subscription-options", features(Type)) of
			 true  ->
			     get_options_helper(JID, Lang, Node, NodeID, SubID, Type);
			 false ->
			    {error, extended_error(
					?ERR_FEATURE_NOT_IMPLEMENTED,
					unsupported, "subscription-options")}
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, XForm}} -> {result, [XForm]};
	Error		    -> Error
    end.

get_options_helper(JID, Lang, Node, NodeID, SubID, Type) ->
    Subscriber = case jlib:string_to_jid(JID) of
		     error -> {"", "", ""};
		     J -> jlib:jid_tolower(J)
		 end,
    {result, Subs} = node_call(Type, get_subscriptions,
			       [NodeID, Subscriber]),
    SubIDs = lists:foldl(fun({subscribed, SID}, Acc) ->
				 [SID | Acc];
			     (_, Acc) ->
				 Acc
			 end, [], Subs),
    case {SubID, SubIDs} of
	{_, []} ->
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, "not-subscribed")};
	{[], [SID]} ->
	    read_sub(Subscriber, Node, NodeID, SID, Lang);
	{[], _} ->
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, "subid-required")};
	{_, _} ->
	    read_sub(Subscriber, Node, NodeID, SubID, Lang)
    end.

read_sub(Subscriber, Node, NodeID, SubID, Lang) ->
    case pubsub_subscription_odbc:get_subscription(Subscriber, NodeID, SubID) of
	{error, notfound} ->
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	{result, #pubsub_subscription{options = Options}} ->
	    {result, XdataEl} = pubsub_subscription_odbc:get_options_xform(Lang, Options),
	    OptionsEl = {xmlelement, "options", [{"jid", jlib:jid_to_string(Subscriber)},
						 {"subid", SubID}|nodeAttr(Node)],
			 [XdataEl]},
            PubsubEl = {xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}], [OptionsEl]},
            {result, PubsubEl}
    end.

set_options(Host, Node, JID, SubID, Configuration) ->
    Action = fun(#pubsub_node{type = Type, id = NodeID}) ->
		     case lists:member("subscription-options", features(Type)) of
			 true ->
			     set_options_helper(Configuration, JID, NodeID,
						SubID, Type);
			 false ->
			    {error, extended_error(
					?ERR_FEATURE_NOT_IMPLEMENTED,
					unsupported, "subscription-options")}
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, Result}} -> {result, Result};
	Error		     -> Error
    end.

set_options_helper(Configuration, JID, NodeID, SubID, Type) ->
    SubOpts = case pubsub_subscription_odbc:parse_options_xform(Configuration) of
	{result, GoodSubOpts} -> GoodSubOpts;
	_ -> invalid
    end,
    Subscriber = case jlib:string_to_jid(JID) of
		     error -> {"", "", ""};
		     J -> jlib:jid_tolower(J)
		 end,
    {result, Subs} = node_call(Type, get_subscriptions,
			       [NodeID, Subscriber]),
    SubIDs = lists:foldl(fun({subscribed, SID}, Acc) ->
				 [SID | Acc];
			     (_, Acc) ->
				 Acc
			 end, [], Subs),
    case {SubID, SubIDs} of
	{_, []} ->
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, "not-subscribed")};
	{[], [SID]} ->
	    write_sub(Subscriber, NodeID, SID, SubOpts);
	{[], _} ->
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, "subid-required")};
	{_, _} ->
	    write_sub(Subscriber, NodeID, SubID, SubOpts)
    end.

write_sub(_Subscriber, _NodeID, _SubID, invalid) ->
    {error, extended_error(?ERR_BAD_REQUEST, "invalid-options")};
write_sub(Subscriber, NodeID, SubID, Options) ->
    case pubsub_subscription_odbc:set_subscription(Subscriber, NodeID, SubID, Options) of
	{error, notfound} ->
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	{result, _} ->
	    {result, []}
    end.

%% @spec (Host, Node, JID, Plugins) -> {error, Reason} | {result, Response}
%%	 Host = host()
%%	 Node = pubsubNode()
%%	 JID = jid()
%%	 Plugins = [Plugin::string()]
%%	 Reason = stanzaError()
%%	 Response = [pubsubIQResponse()]
%% @doc <p>Return the list of subscriptions as an XMPP response.</p>
get_subscriptions(Host, Node, JID, Plugins) when is_list(Plugins) ->
    Result = lists:foldl(
	       fun(Type, {Status, Acc}) ->
		       Features = features(Type),
		       RetrieveFeature = lists:member("retrieve-subscriptions", Features),
		       if
			   not RetrieveFeature ->
			       %% Service does not support retreive subscriptions
			       {{error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "retrieve-subscriptions")}, Acc};
			   true ->
			       Subscriber = jlib:jid_remove_resource(JID),
			       {result, Subscriptions} = node_action(Host, Type, get_entity_subscriptions, [Host, Subscriber]),
			       {Status, [Subscriptions|Acc]}
		       end
	       end, {ok, []}, Plugins),
    case Result of
	{ok, Subscriptions} ->
	    Entities = lists:flatmap(
			 fun({_, none}) ->
				[];
			    ({#pubsub_node{nodeid = {_, SubsNode}}, Subscription}) ->
				case Node of
				<<>> ->
				 [{xmlelement, "subscription",
				   [{"subscription", subscription_to_string(Subscription)}|nodeAttr(SubsNode)],
				   []}];
				SubsNode ->
				 [{xmlelement, "subscription",
				   [{"subscription", subscription_to_string(Subscription)}],
				   []}];
				_ ->
				 []
				end;
			    ({_, none, _}) ->
				[];
			    ({#pubsub_node{nodeid = {_, SubsNode}}, Subscription, SubID, SubJID}) ->
				case Node of
				<<>> ->
				 [{xmlelement, "subscription",
				   [{"jid", jlib:jid_to_string(SubJID)},
				    {"subid", SubID},
				    {"subscription", subscription_to_string(Subscription)}|nodeAttr(SubsNode)],
				   []}];
				SubsNode ->
				 [{xmlelement, "subscription",
				   [{"jid", jlib:jid_to_string(SubJID)},
				    {"subid", SubID},
				    {"subscription", subscription_to_string(Subscription)}],
				   []}];
				_ ->
				 []
				end;
			    ({#pubsub_node{nodeid = {_, SubsNode}}, Subscription, SubJID}) ->
				case Node of
				<<>> ->
				 [{xmlelement, "subscription",
				   [{"jid", jlib:jid_to_string(SubJID)},
				    {"subscription", subscription_to_string(Subscription)}|nodeAttr(SubsNode)],
				   []}];
				SubsNode ->
				 [{xmlelement, "subscription",
				   [{"jid", jlib:jid_to_string(SubJID)},
				    {"subscription", subscription_to_string(Subscription)}],
				   []}];
				_ ->
				 []
				end
			 end, lists:usort(lists:flatten(Subscriptions))),
	    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		       [{xmlelement, "subscriptions", [],
			 Entities}]}]};
	{Error, _} ->
	    Error
    end.
get_subscriptions(Host, Node, JID) ->
    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
		     Features = features(Type),
		     RetrieveFeature = lists:member("manage-subscriptions", Features),
		     {result, Affiliation} = node_call(Type, get_affiliation, [NodeId, JID]),
		     if
			 not RetrieveFeature ->
			     %% Service does not support manage subscriptions
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "manage-subscriptions")};
			 Affiliation /= owner ->
			     %% Entity is not an owner
			     {error, ?ERR_FORBIDDEN};
			 true ->
			     node_call(Type, get_node_subscriptions, [NodeId])
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Subscriptions}} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({_, pending, _}) -> [];
			    ({AJID, Subscription}) ->
				 [{xmlelement, "subscription",
				   [{"jid", jlib:jid_to_string(AJID)},
				    {"subscription", subscription_to_string(Subscription)}],
				   []}];
			    ({AJID, Subscription, SubId}) ->
				 [{xmlelement, "subscription",
				   [{"jid", jlib:jid_to_string(AJID)},
				    {"subscription", subscription_to_string(Subscription)},
				    {"subid", SubId}],
				   []}]
			 end, Subscriptions),
	    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB_OWNER}],
		       [{xmlelement, "subscriptions", nodeAttr(Node),
			 Entities}]}]};
	Error ->
	    Error
    end.

set_subscriptions(Host, Node, From, EntitiesEls) ->
    Owner = jlib:jid_tolower(jlib:jid_remove_resource(From)),
    Entities =
	lists:foldl(
	  fun(El, Acc) ->
		  case Acc of
		      error ->
			  error;
		      _ ->
			  case El of
			      {xmlelement, "subscription", Attrs, _} ->
				  JID = jlib:string_to_jid(
					  xml:get_attr_s("jid", Attrs)),
				  Subscription = string_to_subscription(
						   xml:get_attr_s("subscription", Attrs)),
				  SubId = xml:get_attr_s("subid", Attrs),
				  if
				      (JID == error) or
				      (Subscription == false) ->
					  error;
				      true ->
					  [{jlib:jid_tolower(JID), Subscription, SubId} | Acc]
				  end
			  end
		  end
	  end, [], EntitiesEls),
    case Entities of
	error ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    Notify = fun(JID, Sub, _SubId) ->
		Stanza = {xmlelement, "message", [],
			    [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
				[{xmlelement, "subscription",
				    [{"jid", jlib:jid_to_string(JID)}, 
				    %{"subid", SubId},
				     {"subscription", subscription_to_string(Sub)} | nodeAttr(Node)], []}]}]},
		ejabberd_router:route(service_jid(Host), jlib:make_jid(JID), Stanza)
	    end,
	    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
			    case lists:member(Owner, node_owners_call(Type, NodeId)) of
				true ->
				    Result = lists:foldl(fun({JID, Subscription, SubId}, Acc) ->

						    case node_call(Type, set_subscriptions, [NodeId, JID, Subscription, SubId]) of
							{error, Err} -> [{error, Err} | Acc];
							_ -> Notify(JID, Subscription, SubId), Acc
						    end
						end, [], Entities),
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


get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups) ->
    if (AccessModel == presence) or (AccessModel == roster) ->
	case Host of
	    {User, Server, _} ->
		get_roster_info(User, Server, From, AllowedGroups);
	    _ ->
		[{OUser, OServer, _}|_] = Owners,
		get_roster_info(OUser, OServer, From, AllowedGroups)
	end;
    true ->
	{true, true}
    end.

%% @spec (OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, SubscriberResource}, AllowedGroups)
%%    -> {PresenceSubscription, RosterGroup}
get_roster_info(_, _, {"", "", _}, _) ->
    {false, false};
get_roster_info(OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, _}, AllowedGroups) ->
    {Subscription, Groups} =
	ejabberd_hooks:run_fold(
	  roster_get_jid_info, OwnerServer,
	  {none, []},
	  [OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, ""}]),
    PresenceSubscription = (Subscription == both) orelse (Subscription == from)
			    orelse ({OwnerUser, OwnerServer} == {SubscriberUser, SubscriberServer}),
    RosterGroup = lists:any(fun(Group) ->
				    lists:member(Group, AllowedGroups)
			    end, Groups),
    {PresenceSubscription, RosterGroup};
get_roster_info(OwnerUser, OwnerServer, JID, AllowedGroups) ->
    get_roster_info(OwnerUser, OwnerServer, jlib:jid_tolower(JID), AllowedGroups).

%% @spec (AffiliationStr) -> Affiliation
%%	 AffiliationStr = string()
%%	 Affiliation = atom()
%% @doc <p>Convert an affiliation type from string to atom.</p>
string_to_affiliation("owner") -> owner;
string_to_affiliation("publisher") -> publisher;
string_to_affiliation("member") -> member;
string_to_affiliation("outcast") -> outcast;
string_to_affiliation("none") -> none;
string_to_affiliation(_) -> false.

%% @spec (SubscriptionStr) -> Subscription
%%	 SubscriptionStr = string()
%%	 Subscription = atom()
%% @doc <p>Convert a subscription type from string to atom.</p>
string_to_subscription("subscribed") -> subscribed;
string_to_subscription("pending") -> pending;
string_to_subscription("unconfigured") -> unconfigured;
string_to_subscription("none") -> none;
string_to_subscription(_) -> false.

%% @spec (Affiliation) -> AffiliationStr
%%	 Affiliation = atom()
%%	 AffiliationStr = string()
%% @doc <p>Convert an affiliation type from atom to string.</p>
affiliation_to_string(owner) -> "owner";
affiliation_to_string(publisher) -> "publisher";
affiliation_to_string(member) -> "member";
affiliation_to_string(outcast) -> "outcast";
affiliation_to_string(_) -> "none".

%% @spec (Subscription) -> SubscriptionStr
%%	 Subscription = atom()
%%	 SubscriptionStr = string()
%% @doc <p>Convert a subscription type from atom to string.</p>
subscription_to_string(subscribed) -> "subscribed";
subscription_to_string(pending) -> "pending";
subscription_to_string(unconfigured) -> "unconfigured";
subscription_to_string(_) -> "none".

%% @spec (Node) -> NodeStr
%%	 Node = pubsubNode()
%%	 NodeStr = string()
%% @doc <p>Convert a node type from pubsubNode to string.</p>
node_to_string(Node) -> binary_to_list(Node).
string_to_node(SNode) -> list_to_binary(SNode).

%% @spec (Host) -> jid()
%%	Host = host()
%% @doc <p>Generate pubsub service JID.</p>
service_jid(Host) ->
    case Host of 
    {U,S,_} -> {jid, U, S, "", U, S, ""}; 
    _ -> {jid, "", Host, "", "", Host, ""}
    end.

%% @spec (LJID, NotifyType, Depth, NodeOptions, SubOptions) -> boolean()
%%	LJID = jid()
%%	NotifyType = items | nodes
%%	Depth = integer()
%%	NodeOptions = [{atom(), term()}]
%%	SubOptions = [{atom(), term()}]
%% @doc <p>Check if a notification must be delivered or not based on
%% node and subscription options.</p>
is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) ->
    sub_to_deliver(LJID, NotifyType, Depth, SubOptions)
	andalso node_to_deliver(LJID, NodeOptions).

sub_to_deliver(_LJID, NotifyType, Depth, SubOptions) ->
    lists:all(fun (Option) ->
		      sub_option_can_deliver(NotifyType, Depth, Option)
	      end, SubOptions).

sub_option_can_deliver(items, _, {subscription_type, nodes}) -> false;
sub_option_can_deliver(nodes, _, {subscription_type, items}) -> false;
sub_option_can_deliver(_, _, {subscription_depth, all})      -> true;
sub_option_can_deliver(_, Depth, {subscription_depth, D})    -> Depth =< D;
sub_option_can_deliver(_, _, {deliver, false})	       -> false;
sub_option_can_deliver(_, _, {expire, When})		 -> now() < When;
sub_option_can_deliver(_, _, _)			      -> true.

node_to_deliver(LJID, NodeOptions) ->
    PresenceDelivery = get_option(NodeOptions, presence_based_delivery),
    presence_can_deliver(LJID, PresenceDelivery).

presence_can_deliver(_, false) -> true;
presence_can_deliver({User, Server, Resource}, true) ->
    case mnesia:dirty_match_object({session, '_', '_', {User, Server}, '_', '_'}) of
    [] -> false;
    Ss ->
	lists:foldl(fun(_, true) -> true;
		       ({session, _, _ , _, undefined, _}, _Acc) -> false;
		       ({session, _, {_, _, R}, _, _Priority, _}, _Acc) ->
			   case Resource of
			       [] -> true;
			       R -> true;
			       _ -> false
			   end
	end, false, Ss)
    end.

state_can_deliver({U, S, R}, []) -> [{U, S, R}];
state_can_deliver({U, S, R}, SubOptions) ->
    %% Check SubOptions for 'show_values'
    case lists:keysearch('show_values', 1, SubOptions) of
  %% If not in suboptions, item can be delivered, case doesn't apply
  false -> [{U, S, R}];
  %% If in a suboptions ...
  {_, {_, ShowValues}} ->
      %% Get subscriber resources
      Resources = case R of
    %% If the subscriber JID is a bare one, get all its resources
    [] -> user_resources(U, S);
    %% If the subscriber JID is a full one, use its resource
    R  -> [R]
      end,
      %% For each resource, test if the item is allowed to be delivered
      %% based on resource state
      lists:foldl(
        fun(Resource, Acc) ->
          get_resource_state({U, S, Resource}, ShowValues, Acc)
        end, [], Resources)
    end.

get_resource_state({U, S, R}, ShowValues, JIDs) ->
    %% Get user session PID
    case ejabberd_sm:get_session_pid(U, S, R) of
  %% If no PID, item can be delivered
  none -> lists:append([{U, S, R}], JIDs);
  %% If PID ...
  Pid ->
      %% Get user resource state
      %% TODO : add a catch clause
      Show = case ejabberd_c2s:get_presence(Pid) of
    {_, _, "available", _} -> "online";
    {_, _, State, _}       -> State
      end,
      %% Is current resource state listed in 'show-values' suboption ?
      case lists:member(Show, ShowValues) of %andalso Show =/= "online" of
    %% If yes, item can be delivered
    true  -> lists:append([{U, S, R}], JIDs);
    %% If no, item can't be delivered
    false -> JIDs
      end
    end.

%% @spec (Payload) -> int()
%%	Payload = term()
%% @doc <p>Count occurence of XML elements in payload.</p>
payload_xmlelements(Payload) -> payload_xmlelements(Payload, 0).
payload_xmlelements([], Count) -> Count;
payload_xmlelements([{xmlelement, _, _, _}|Tail], Count) -> payload_xmlelements(Tail, Count+1);
payload_xmlelements([_|Tail], Count) -> payload_xmlelements(Tail, Count).

%% @spec (Els) -> stanza()
%%	Els = [xmlelement()]
%% @doc <p>Build pubsub event stanza</p>
event_stanza(Els) ->
    event_stanza_withmoreels(Els, []).

event_stanza_with_delay(Els, ModifNow, ModifUSR) ->
    DateTime = calendar:now_to_datetime(ModifNow),
    MoreEls = [jlib:timestamp_to_xml(DateTime, utc, ModifUSR, "")],
    event_stanza_withmoreels(Els, MoreEls).

event_stanza_withmoreels(Els, MoreEls) ->
    {xmlelement, "message", [],
     [{xmlelement, "event", [{"xmlns", ?NS_PUBSUB_EVENT}], Els} | MoreEls]}.

%%%%%% broadcast functions

broadcast_publish_item(Host, Node, NodeId, Type, NodeOptions, Removed, ItemId, From, Payload) ->
    case get_collection_subscriptions(Host, Node) of
	SubsByDepth when is_list(SubsByDepth) ->
	    Content = case get_option(NodeOptions, deliver_payloads) of
		true -> Payload;
		false -> []
	    end,
	    Stanza = event_stanza(
		[{xmlelement, "items", nodeAttr(Node),
		    [{xmlelement, "item", itemAttr(ItemId), Content}]}]),
	    broadcast_stanza(Host, From, Node, NodeId, Type,
			     NodeOptions, SubsByDepth, items, Stanza, true),
	    case Removed of
		[] ->
		    ok;
		_ ->
		    case get_option(NodeOptions, notify_retract) of
			true ->
			    RetractStanza = event_stanza(
				[{xmlelement, "items", nodeAttr(Node),
				    [{xmlelement, "retract", itemAttr(RId), []} || RId <- Removed]}]),
			    broadcast_stanza(Host, Node, NodeId, Type,
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

broadcast_retract_items(Host, Node, NodeId, Type, NodeOptions, ItemIds) ->
    broadcast_retract_items(Host, Node, NodeId, Type, NodeOptions, ItemIds, false).
broadcast_retract_items(_Host, _Node, _NodeId, _Type, _NodeOptions, [], _ForceNotify) ->
    {result, false};
broadcast_retract_items(Host, Node, NodeId, Type, NodeOptions, ItemIds, ForceNotify) ->
    case (get_option(NodeOptions, notify_retract) or ForceNotify) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		SubsByDepth when is_list(SubsByDepth) ->
		    Stanza = event_stanza(
			[{xmlelement, "items", nodeAttr(Node),
			    [{xmlelement, "retract", itemAttr(ItemId), []} || ItemId <- ItemIds]}]),
		    broadcast_stanza(Host, Node, NodeId, Type,
				     NodeOptions, SubsByDepth, items, Stanza, true),
		    {result, true};
		_ ->
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_purge_node(Host, Node, NodeId, Type, NodeOptions) ->
    case get_option(NodeOptions, notify_retract) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		SubsByDepth when is_list(SubsByDepth) ->
		    Stanza = event_stanza(
			[{xmlelement, "purge", nodeAttr(Node),
			    []}]),
		    broadcast_stanza(Host, Node, NodeId, Type,
				     NodeOptions, SubsByDepth, nodes, Stanza, false),
		    {result, true};
		_ -> 
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_removed_node(Host, Node, NodeId, Type, NodeOptions, SubsByDepth) ->
    case get_option(NodeOptions, notify_delete) of
	true ->
	    case SubsByDepth of
		[] ->
		    {result, false};
		_ ->
		    Stanza = event_stanza(
			[{xmlelement, "delete", nodeAttr(Node),
			    []}]),
		    broadcast_stanza(Host, Node, NodeId, Type,
				     NodeOptions, SubsByDepth, nodes, Stanza, false),
		    {result, true}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_created_node(_, _, _, _, _, []) ->
    {result, false};
broadcast_created_node(Host, Node, NodeId, Type, NodeOptions, SubsByDepth) ->
    Stanza = event_stanza([{xmlelement, "create", nodeAttr(Node), []}]),
    broadcast_stanza(Host, Node, NodeId, Type, NodeOptions, SubsByDepth, nodes, Stanza, true),
    {result, true}.

broadcast_config_notification(Host, Node, NodeId, Type, NodeOptions, Lang) ->
    case get_option(NodeOptions, notify_config) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		SubsByDepth when is_list(SubsByDepth) ->
		    Content = case get_option(NodeOptions, deliver_payloads) of
			true ->
			    [{xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "result"}],
				get_configure_xfields(Type, NodeOptions, Lang, [])}];
			false ->
			    []
		    end,
		    Stanza = event_stanza(
			[{xmlelement, "configuration", nodeAttr(Node), Content}]),
		    broadcast_stanza(Host, Node, NodeId, Type,
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
	    {result, lists:map(fun({Depth, Nodes}) ->
			{Depth, [{N, get_node_subs(N)} || N <- Nodes]}
	    end, tree_call(Host, get_parentnodes_tree, [Host, Node, service_jid(Host)]))}
	end,
    case transaction(Host, Action, sync_dirty) of
	{result, CollSubs} -> CollSubs;
	_ -> []
    end.

get_node_subs(#pubsub_node{type   = Type,
			   id     = NodeID}) ->
    case node_call(Type, get_node_subscriptions, [NodeID]) of
	{result, Subs} -> get_options_for_subs(NodeID, Subs);
	Other -> Other
    end.

get_options_for_subs(NodeID, Subs) ->
    lists:foldl(fun({JID, subscribed, SubID}, Acc) ->
			case pubsub_subscription_odbc:get_subscription(JID, NodeID, SubID) of
			    {error, notfound} -> [{JID, SubID, []} | Acc];
			    {result, #pubsub_subscription{options = Options}} -> [{JID, SubID, Options} | Acc];
			    _ -> Acc
			end;
		    (_, Acc) ->
			Acc
		end, [], Subs).

broadcast_stanza(Host, _Node, _NodeId, _Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    NotificationType = get_option(NodeOptions, notification_type, headline),
    BroadcastAll = get_option(NodeOptions, broadcast_all_resources), %% XXX this is not standard, but usefull
    From = service_jid(Host),
    Stanza = case NotificationType of
	normal -> BaseStanza;
	MsgType -> add_message_type(BaseStanza, atom_to_list(MsgType))
	end,
    %% Handles explicit subscriptions
    SubIDsByJID = subscribed_nodes_by_jid(NotifyType, SubsByDepth),
    lists:foreach(fun ({LJID, NodeName, SubIDs}) ->
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
				                   add_shim_headers(Stanza, collection_shim(NodeName));
				                 {true, SubIDs} ->
				                   add_shim_headers(Stanza, lists:append(collection_shim(NodeName), subid_shim(SubIDs)))
		                   end,
			  lists:foreach(fun(To) ->
					ejabberd_router:route(From, jlib:make_jid(To), StanzaToSend)
				end, LJIDs)
		end, SubIDsByJID).

broadcast_stanza({LUser, LServer, LResource}, Publisher, Node, NodeId, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    broadcast_stanza({LUser, LServer, LResource}, Node, NodeId, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM),
    %% Handles implicit presence subscriptions
    SenderResource = case LResource of
	[] -> 
	    case user_resources(LUser, LServer) of
		[Resource|_] -> Resource;
		_ -> ""
	    end;
	_ ->
	    LResource
    end,
    case ejabberd_sm:get_session_pid(LUser, LServer, SenderResource) of
	C2SPid when is_pid(C2SPid) ->
	    Stanza = case get_option(NodeOptions, notification_type, headline) of
		normal -> BaseStanza;
		MsgType -> add_message_type(BaseStanza, atom_to_list(MsgType))
		end,
	    %% set the from address on the notification to the bare JID of the account owner
	    %% Also, add "replyto" if entity has presence subscription to the account owner
	    %% See XEP-0163 1.1 section 4.3.1
	    ejabberd_c2s:broadcast(C2SPid,
	        {pep_message, binary_to_list(Node)++"+notify"},
	        _Sender = jlib:make_jid(LUser, LServer, ""),
	        _StanzaToSend = add_extended_headers(Stanza,
	            _ReplyTo = extended_headers([jlib:jid_to_string(Publisher)])));
	_ ->
	    ?DEBUG("~p@~p has no session; can't deliver ~p to contacts", [LUser, LServer, BaseStanza])
    end;
broadcast_stanza(Host, _Publisher, Node, NodeId, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    broadcast_stanza(Host, Node, NodeId, Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza, SHIM).

subscribed_nodes_by_jid(NotifyType, SubsByDepth) ->
    NodesToDeliver = fun(Depth, Node, Subs, Acc) ->
	    NodeName = case Node#pubsub_node.nodeid of
		{_, N} -> N;
		Other -> Other
	    end,
	    NodeOptions = Node#pubsub_node.options,
	    lists:foldl(fun({LJID, SubID, SubOptions}, {JIDs, Recipients}) ->
		case is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) of
	true  ->
		  %% If is to deliver :
		  case state_can_deliver(LJID, SubOptions) of
		[]            -> {JIDs, Recipients};
		JIDsToDeliver ->
		    lists:foldl(
		      fun(JIDToDeliver, {JIDsAcc, RecipientsAcc}) ->
		    case lists:member(JIDToDeliver, JIDs) of
		    %% check if the JIDs co-accumulator contains the Subscription Jid,
		  false ->
			%%  - if not,
			%%  - add the Jid to JIDs list co-accumulator ;
			%%  - create a tuple of the Jid, NodeId, and SubID (as list),
			%%    and add the tuple to the Recipients list co-accumulator
			    {[JIDToDeliver | JIDsAcc], [{JIDToDeliver, NodeName, [SubID]} | RecipientsAcc]};
		  true ->
			%% - if the JIDs co-accumulator contains the Jid
			%%   get the tuple containing the Jid from the Recipient list co-accumulator
			    {_, {JIDToDeliver, NodeName1, SubIDs}} = lists:keysearch(JIDToDeliver, 1, RecipientsAcc),
			%%   delete the tuple from the Recipients list
			% v1 : Recipients1 = lists:keydelete(LJID, 1, Recipients),
			% v2 : Recipients1 = lists:keyreplace(LJID, 1, Recipients, {LJID, NodeId1, [SubID | SubIDs]}),
			%%   add the SubID to the SubIDs list in the tuple,
			%%   and add the tuple back to the Recipients list co-accumulator
			% v1.1 : {JIDs, lists:append(Recipients1, [{LJID, NodeId1, lists:append(SubIDs, [SubID])}])}
			% v1.2 : {JIDs, [{LJID, NodeId1, [SubID | SubIDs]} | Recipients1]}
			% v2: {JIDs, Recipients1}
			    {JIDsAcc, lists:keyreplace(JIDToDeliver, 1, RecipientsAcc, {JIDToDeliver, NodeName1, [SubID | SubIDs]})}
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

%%%%%%% Configuration handling

%%<p>There are several reasons why the default node configuration options request might fail:</p>
%%<ul>
%%<li>The service does not support node configuration.</li>
%%<li>The service does not support retrieval of default node configuration.</li>
%%</ul>
get_configure(Host, ServerHost, Node, From, Lang) ->
    Action =
	fun(#pubsub_node{options = Options, type = Type, id = NodeId}) ->
		case node_call(Type, get_affiliation, [NodeId, From]) of
		    {result, owner} ->
			Groups = ejabberd_hooks:run_fold(roster_groups, ServerHost, [], [ServerHost]),
			{result,
			 [{xmlelement, "pubsub",
			   [{"xmlns", ?NS_PUBSUB_OWNER}],
			   [{xmlelement, "configure", nodeAttr(Node),
			     [{xmlelement, "x",
			       [{"xmlns", ?NS_XDATA}, {"type", "form"}],
			       get_configure_xfields(Type, Options, Lang, Groups)
			      }]}]}]};
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
    Options = node_options(Type),
    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB_OWNER}],
		[{xmlelement, "default", [],
		    [{xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
			get_configure_xfields(Type, Options, Lang, [])
		}]}]}]}.

%% Get node option
%% The result depend of the node type plugin system.
get_option([], _) -> false;
get_option(Options, Var) ->
    get_option(Options, Var, false).
get_option(Options, Var, Def) ->
    case lists:keysearch(Var, 1, Options) of
	{value, {_Val, Ret}} -> Ret;
	_ -> Def
    end.

%% Get default options from the module plugin.
node_options(Type) ->
    Module = list_to_atom(?PLUGIN_PREFIX ++ Type),
    case catch Module:options() of
	{'EXIT',{undef,_}} ->
	    DefaultModule = list_to_atom(?PLUGIN_PREFIX++?STDNODE),
	    DefaultModule:options();
	Result ->
	    Result
    end.

%% @spec (Host, Type, NodeId) -> [ljid()]
%%    NodeId = pubsubNodeId()
%% @doc <p>Return list of node owners.</p>
node_owners(Host, Type, NodeId) ->
    case node_action(Host, Type, get_node_affiliations, [NodeId]) of
	{result, Affiliations} ->
	    lists:foldl(
		fun({LJID, owner}, Acc) -> [LJID|Acc];
		   (_, Acc) -> Acc
	    end, [], Affiliations);
	_ ->
	    []
    end.
node_owners_call(Type, NodeId) ->
    case node_call(Type, get_node_affiliations, [NodeId]) of
	{result, Affiliations} ->
	    lists:foldl(
		fun({LJID, owner}, Acc) -> [LJID|Acc];
		   (_, Acc) -> Acc
	    end, [], Affiliations);
	_ ->
	    []
    end.

%% @spec (Host, Options) -> MaxItems
%%	 Host = host()
%%	 Options = [Option]
%%	 Option = {Key::atom(), Value::term()}
%%	 MaxItems = integer() | unlimited
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
		false -> unlimited;
		Result when (Result < 0) -> 0;
		Result -> Result
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
	?BOOLXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		    get_option(Options, Var))).

-define(STRING_CONFIG_FIELD(Label, Var),
	?STRINGXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		      get_option(Options, Var, ""))).

-define(INTEGER_CONFIG_FIELD(Label, Var),
	?STRINGXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		      integer_to_list(get_option(Options, Var)))).

-define(JLIST_CONFIG_FIELD(Label, Var, Opts),
	?LISTXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		    jlib:jid_to_string(get_option(Options, Var)),
		    [jlib:jid_to_string(O) || O <- Opts])).

-define(ALIST_CONFIG_FIELD(Label, Var, Opts),
	?LISTXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		    atom_to_list(get_option(Options, Var)),
		    [atom_to_list(O) || O <- Opts])).

-define(LISTM_CONFIG_FIELD(Label, Var, Opts),
	?LISTMXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		     get_option(Options, Var), Opts)).

-define(NLIST_CONFIG_FIELD(Label, Var),
	?STRINGMXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		       [node_to_string(N) || N <- get_option(Options, Var, [])])).

get_configure_xfields(_Type, Options, Lang, Groups) ->
    [?XFIELD("hidden", "", "FORM_TYPE", ?NS_PUBSUB_NODE_CONFIG),
     ?BOOL_CONFIG_FIELD("Deliver payloads with event notifications", deliver_payloads),
     ?BOOL_CONFIG_FIELD("Deliver event notifications", deliver_notifications),
     ?BOOL_CONFIG_FIELD("Notify subscribers when the node configuration changes", notify_config),
     ?BOOL_CONFIG_FIELD("Notify subscribers when the node is deleted", notify_delete),
     ?BOOL_CONFIG_FIELD("Notify subscribers when items are removed from the node", notify_retract),
     ?BOOL_CONFIG_FIELD("Persist items to storage", persist_items),
     ?STRING_CONFIG_FIELD("A friendly name for the node", title),
     ?INTEGER_CONFIG_FIELD("Max # of items to persist", max_items),
     ?BOOL_CONFIG_FIELD("Whether to allow subscriptions", subscribe),
     ?ALIST_CONFIG_FIELD("Specify the access model", access_model,
			 [open, authorize, presence, roster, whitelist]),
     %% XXX: change to list-multi, include current roster groups as options
     ?LISTM_CONFIG_FIELD("Roster groups allowed to subscribe", roster_groups_allowed, Groups),
     ?ALIST_CONFIG_FIELD("Specify the publisher model", publish_model,
			 [publishers, subscribers, open]),
		 ?BOOL_CONFIG_FIELD("Purge all items when the relevant publisher goes offline", purge_offline),
     ?ALIST_CONFIG_FIELD("Specify the event message type", notification_type,
			 [headline, normal]),
     ?INTEGER_CONFIG_FIELD("Max payload size in bytes", max_payload_size),
     ?ALIST_CONFIG_FIELD("When to send the last published item", send_last_published_item,
			 [never, on_sub, on_sub_and_presence]),
     ?BOOL_CONFIG_FIELD("Only deliver notifications to available users", presence_based_delivery),
     ?NLIST_CONFIG_FIELD("The collections with which a node is affiliated", collection)
    ].

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
	[{xmlelement, "x", _Attrs1, _Els1} = XEl] ->
	    case {xml:get_tag_attr_s("xmlns", XEl), xml:get_tag_attr_s("type", XEl)} of
		{?NS_XDATA, "cancel"} ->
		    {result, []};
		{?NS_XDATA, "submit"} ->
		    Action =
			fun(#pubsub_node{options = Options, type = Type, id = NodeId} = N) ->
				case node_call(Type, get_affiliation, [NodeId, From]) of
				    {result, owner} ->
					case jlib:parse_xdata_submit(XEl) of
					    invalid ->
						{error, ?ERR_BAD_REQUEST};
					    XData ->
						OldOpts = case Options of
							      [] -> node_options(Type);
							      _ -> Options
							  end,
						case set_xoption(Host, XData, OldOpts) of
						    NewOpts when is_list(NewOpts) ->
							case tree_call(Host, set_node, [N#pubsub_node{options = NewOpts}]) of
							    ok -> {result, ok};
							    Err -> Err
							end;
						    Err ->
							Err
						end
					end;
				    _ ->
					{error, ?ERR_FORBIDDEN}
				end
			end,
		    case transaction(Host, Node, Action, transaction) of
			{result, {TNode, ok}} ->
			    NodeId = TNode#pubsub_node.id,
			    Type = TNode#pubsub_node.type,
			    Options = TNode#pubsub_node.options,
			    broadcast_config_notification(Host, Node, NodeId, Type, Options, Lang),
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
    Opts1 = lists:keydelete(Key, 1, Opts),
    [{Key, Value} | Opts1].

-define(SET_BOOL_XOPT(Opt, Val),
	BoolVal = case Val of
		      "0" -> false;
		      "1" -> true;
		      "false" -> false;
		      "true" -> true;
		      _ -> error
		  end,
	case BoolVal of
	    error -> {error, ?ERR_NOT_ACCEPTABLE};
	    _ -> set_xoption(Host, Opts, add_opt(Opt, BoolVal, NewOpts))
	end).

-define(SET_STRING_XOPT(Opt, Val),
	set_xoption(Host, Opts, add_opt(Opt, Val, NewOpts))).

-define(SET_INTEGER_XOPT(Opt, Val, Min, Max),
	case catch list_to_integer(Val) of
	    IVal when is_integer(IVal),
	    IVal >= Min,
	    IVal =< Max ->
		set_xoption(Host, Opts, add_opt(Opt, IVal, NewOpts));
	    _ ->
		{error, ?ERR_NOT_ACCEPTABLE}
	end).

-define(SET_ALIST_XOPT(Opt, Val, Vals),
	case lists:member(Val, [atom_to_list(V) || V <- Vals]) of
	    true -> set_xoption(Host, Opts, add_opt(Opt, list_to_atom(Val), NewOpts));
	    false -> {error, ?ERR_NOT_ACCEPTABLE}
	end).

-define(SET_LIST_XOPT(Opt, Val),
	set_xoption(Host, Opts, add_opt(Opt, Val, NewOpts))).

set_xoption(_Host, [], NewOpts) ->
    NewOpts;
set_xoption(Host, [{"FORM_TYPE", _} | Opts], NewOpts) ->
    set_xoption(Host, Opts, NewOpts);
set_xoption(Host, [{"pubsub#roster_groups_allowed", Value} | Opts], NewOpts) ->
    ?SET_LIST_XOPT(roster_groups_allowed, Value);
set_xoption(Host, [{"pubsub#deliver_payloads", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_payloads, Val);
set_xoption(Host, [{"pubsub#deliver_notifications", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_notifications, Val);
set_xoption(Host, [{"pubsub#notify_config", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_config, Val);
set_xoption(Host, [{"pubsub#notify_delete", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_delete, Val);
set_xoption(Host, [{"pubsub#notify_retract", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_retract, Val);
set_xoption(Host, [{"pubsub#persist_items", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(persist_items, Val);
set_xoption(Host, [{"pubsub#max_items", [Val]} | Opts], NewOpts) ->
    MaxItems = get_max_items_node(Host),
    ?SET_INTEGER_XOPT(max_items, Val, 0, MaxItems);
set_xoption(Host, [{"pubsub#subscribe", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(subscribe, Val);
set_xoption(Host, [{"pubsub#access_model", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(access_model, Val, [open, authorize, presence, roster, whitelist]);
set_xoption(Host, [{"pubsub#publish_model", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(publish_model, Val, [publishers, subscribers, open]);
set_xoption(Host, [{"pubsub#notification_type", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(notification_type, Val, [headline, normal]);
set_xoption(Host, [{"pubsub#node_type", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(node_type, Val, [leaf, collection]);
set_xoption(Host, [{"pubsub#max_payload_size", [Val]} | Opts], NewOpts) ->
    ?SET_INTEGER_XOPT(max_payload_size, Val, 0, ?MAX_PAYLOAD_SIZE);
set_xoption(Host, [{"pubsub#send_last_published_item", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(send_last_published_item, Val, [never, on_sub, on_sub_and_presence]);
set_xoption(Host, [{"pubsub#presence_based_delivery", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(presence_based_delivery, Val);
set_xoption(Host, [{"pubsub#purge_offline", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(purge_offline, Val);
set_xoption(Host, [{"pubsub#title", Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(title, Value);
set_xoption(Host, [{"pubsub#type", Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(type, Value);
set_xoption(Host, [{"pubsub#body_xslt", Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(body_xslt, Value);
set_xoption(Host, [{"pubsub#collection", Value} | Opts], NewOpts) ->
    NewValue = [string_to_node(V) || V <- Value],
    ?SET_LIST_XOPT(collection, NewValue);
set_xoption(Host, [{"pubsub#node", [Value]} | Opts], NewOpts) ->
    NewValue = string_to_node(Value),
    ?SET_LIST_XOPT(node, NewValue);
set_xoption(Host, [_ | Opts], NewOpts) ->
    % skip unknown field
    set_xoption(Host, Opts, NewOpts).

get_max_items_node({_, ServerHost, _}) ->
    get_max_items_node(ServerHost);
get_max_items_node(Host) ->
    case catch ets:lookup(gen_mod:get_module_proc(Host, config), max_items_node) of
    [{max_items_node, Integer}] -> Integer;
    _ -> ?MAXITEMS
    end.

%%%% last item cache handling

is_last_item_cache_enabled({_, ServerHost, _}) ->
    is_last_item_cache_enabled(ServerHost);
is_last_item_cache_enabled(Host) ->
    case catch ets:lookup(gen_mod:get_module_proc(Host, config), last_item_cache) of
    [{last_item_cache, true}] -> true;
    _ -> false
    end.

set_cached_item({_, ServerHost, _}, NodeId, ItemId, Publisher, Payload) ->
    set_cached_item(ServerHost, NodeId, ItemId, Publisher, Payload);
set_cached_item(Host, NodeId, ItemId, Publisher, Payload) ->
    case is_last_item_cache_enabled(Host) of
    true -> mnesia:dirty_write({pubsub_last_item, NodeId, ItemId, {now(), jlib:jid_tolower(jlib:jid_remove_resource(Publisher))}, Payload});
    _ -> ok
    end.
unset_cached_item({_, ServerHost, _}, NodeId) ->
    unset_cached_item(ServerHost, NodeId);
unset_cached_item(Host, NodeId) ->
    case is_last_item_cache_enabled(Host) of
    true -> mnesia:dirty_delete({pubsub_last_item, NodeId});
    _ -> ok
    end.
get_cached_item({_, ServerHost, _}, NodeId) ->
    get_cached_item(ServerHost, NodeId);
get_cached_item(Host, NodeId) ->
    case is_last_item_cache_enabled(Host) of
    true ->
	case mnesia:dirty_read({pubsub_last_item, NodeId}) of
	[{pubsub_last_item, NodeId, ItemId, Creation, Payload}] ->
	    #pubsub_item{itemid = {ItemId, NodeId}, payload = Payload,
			    creation = Creation, modification = Creation};
	_ ->
	    undefined
	end;
    _ ->
	undefined
    end.

%%%% plugin handling

host(ServerHost) ->
    case catch ets:lookup(gen_mod:get_module_proc(ServerHost, config), host) of
    [{host, Host}] -> Host;
    _ -> "pubsub."++ServerHost
    end.

plugins(Host) ->
    case catch ets:lookup(gen_mod:get_module_proc(Host, config), plugins) of
    [{plugins, []}] -> [?STDNODE];
    [{plugins, PL}] -> PL;
    _ -> [?STDNODE]
    end.
select_type(ServerHost, Host, Node, Type)->
    SelectedType = case Host of
    {_User, _Server, _Resource} -> 
	case catch ets:lookup(gen_mod:get_module_proc(ServerHost, config), pep_mapping) of
	[{pep_mapping, PM}] -> proplists:get_value(node_to_string(Node), PM, ?PEPNODE);
	_ -> ?PEPNODE
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

features() ->
	[
	 % see plugin "access-authorize",   % OPTIONAL
	 "access-open",   % OPTIONAL this relates to access_model option in node_hometree
	 "access-presence",   % OPTIONAL this relates to access_model option in node_pep
	 %TODO "access-roster",   % OPTIONAL
	 "access-whitelist",   % OPTIONAL
	 % see plugin "auto-create",   % OPTIONAL
	 % see plugin "auto-subscribe",   % RECOMMENDED
	 "collections",   % RECOMMENDED
	 "config-node",   % RECOMMENDED
	 "create-and-configure",   % RECOMMENDED
	 % see plugin "create-nodes",   % RECOMMENDED
	 % see plugin "delete-items",   % RECOMMENDED
	 % see plugin "delete-nodes",   % RECOMMENDED
	 % see plugin "filtered-notifications",   % RECOMMENDED
	 % see plugin "get-pending",   % OPTIONAL
	 % see plugin "instant-nodes",   % RECOMMENDED
	 "item-ids",   % RECOMMENDED
	 "last-published",   % RECOMMENDED
	 %TODO "cache-last-item",
	 %TODO "leased-subscription",   % OPTIONAL
	 % see plugin "manage-subscriptions",   % OPTIONAL
	 "member-affiliation",   % RECOMMENDED
	 %TODO "meta-data",   % RECOMMENDED
	 % see plugin "modify-affiliations",   % OPTIONAL
	 % see plugin "multi-collection",   % OPTIONAL
	 % see plugin "multi-subscribe",   % OPTIONAL
	 % see plugin "outcast-affiliation",   % RECOMMENDED
	 % see plugin "persistent-items",   % RECOMMENDED
	 "presence-notifications",   % OPTIONAL
	 "presence-subscribe",   % RECOMMENDED
	 % see plugin "publish",   % REQUIRED
	 %TODO "publish-options",   % OPTIONAL
	 "publisher-affiliation",   % RECOMMENDED
	 % see plugin "purge-nodes",   % OPTIONAL
	 % see plugin "retract-items",   % OPTIONAL
	 % see plugin "retrieve-affiliations",   % RECOMMENDED
	 "retrieve-default"   % RECOMMENDED
	 % see plugin "retrieve-items",   % RECOMMENDED
	 % see plugin "retrieve-subscriptions",   % RECOMMENDED
	 %TODO "shim", % OPTIONAL
	 % see plugin "subscribe",   % REQUIRED
	 % see plugin "subscription-options",   % OPTIONAL
	 % see plugin "subscription-notifications"   % OPTIONAL
	].
features(Type) ->
    Module = list_to_atom(?PLUGIN_PREFIX++Type),
    features() ++ case catch Module:features() of
		      {'EXIT', {undef, _}} -> [];
		      Result -> Result
		  end.
features(Host, <<>>) ->
    lists:usort(lists:foldl(fun(Plugin, Acc) ->
	Acc ++ features(Plugin)
    end, [], plugins(Host)));
features(Host, Node) ->
    Action = fun(#pubsub_node{type = Type}) -> {result, features(Type)} end,
    case transaction(Host, Node, Action, sync_dirty) of
    {result, Features} -> lists:usort(features() ++ Features);
    _ -> features()
    end.

%% @doc <p>node tree plugin call.</p>
tree_call({_User, Server, _Resource}, Function, Args) ->
    tree_call(Server, Function, Args);
tree_call(Host, Function, Args) ->
    ?DEBUG("tree_call ~p ~p ~p",[Host, Function, Args]),
    Module = case catch ets:lookup(gen_mod:get_module_proc(Host, config), nodetree) of
	[{nodetree, N}] -> N;
	_ -> list_to_atom(?TREE_PREFIX ++ ?STDTREE)
    end,
    catch apply(Module, Function, Args).
tree_action(Host, Function, Args) ->
    ?DEBUG("tree_action ~p ~p ~p",[Host,Function,Args]),
    Fun = fun() -> tree_call(Host, Function, Args) end,
    case catch ejabberd_odbc:sql_bloc(odbc_conn(Host), Fun) of
    {atomic, Result} -> 
	Result;
    {aborted, Reason} -> 
	?ERROR_MSG("transaction return internal error: ~p~n",[{aborted, Reason}]),
	{error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

%% @doc <p>node plugin call.</p>
node_call(Type, Function, Args) ->
    ?DEBUG("node_call ~p ~p ~p",[Type, Function, Args]),
    Module = list_to_atom(?PLUGIN_PREFIX++Type),
    case apply(Module, Function, Args) of
	{result, Result} -> {result, Result};
	{error, Error} -> {error, Error};
	{'EXIT', {undef, Undefined}} ->
	    case Type of
		?STDNODE -> {error, {undef, Undefined}};
		_ -> node_call(?STDNODE, Function, Args)
	    end;
	{'EXIT', Reason} -> {error, Reason};
	Result -> {result, Result} %% any other return value is forced as result
    end.

node_action(Host, Type, Function, Args) ->
    ?DEBUG("node_action ~p ~p ~p ~p",[Host,Type,Function,Args]),
    transaction(Host, fun() ->
			node_call(Type, Function, Args)
		end, sync_dirty).

%% @doc <p>plugin transaction handling.</p>
transaction(Host, Node, Action, Trans) ->
    transaction(Host, fun() ->
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
		end, Trans).
transaction_on_nodes(Host, Action, Trans) ->
    transaction(Host, fun() ->
			{result, lists:foldl(Action, [], tree_call(Host, get_nodes, [Host]))}
		end, Trans).

transaction(Host, Fun, Trans) ->
    transaction_retry(Host, Fun, Trans, 2).
transaction_retry(Host, Fun, Trans, Count) ->
    SqlFun = case Trans of
	    transaction -> sql_transaction;
	    _ -> sql_bloc
	end,
    case catch ejabberd_odbc:SqlFun(odbc_conn(Host), Fun) of
	{result, Result} -> {result, Result};
	{error, Error} -> {error, Error};
	{atomic, {result, Result}} -> {result, Result};
	{atomic, {error, Error}} -> {error, Error};
	{aborted, Reason} ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [{aborted, Reason}]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	{'EXIT', {timeout, _} = Reason} ->
	    case Count of
		0 ->
		    ?ERROR_MSG("transaction return internal error: ~p~n", [{'EXIT', Reason}]),
		    {error, ?ERR_INTERNAL_SERVER_ERROR};
		N ->
		    erlang:yield(),
		    transaction_retry(Host, Fun, Trans, N-1)
	    end;
	{'EXIT', Reason} ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [{'EXIT', Reason}]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	Other ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [Other]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

odbc_conn({_U, Host, _R})->
    Host;
odbc_conn(Host) ->
    lists:dropwhile(fun(A) -> A/=$. end, Host) -- ".".

%% escape value for database storage
escape({_U, _H, _R}=JID)->
    ejabberd_odbc:escape(jlib:jid_to_string(JID));
escape(Value)->
    ejabberd_odbc:escape(Value).

%%%% helpers

%% Add pubsub-specific error element
extended_error(Error, Ext) ->
    extended_error(Error, Ext,
		   [{"xmlns", ?NS_PUBSUB_ERRORS}]).
extended_error(Error, unsupported, Feature) ->
    extended_error(Error, "unsupported",
		   [{"xmlns", ?NS_PUBSUB_ERRORS},
		    {"feature", Feature}]);
extended_error({xmlelement, Error, Attrs, SubEls}, Ext, ExtAttrs) ->
    {xmlelement, Error, Attrs,
     lists:reverse([{xmlelement, Ext, ExtAttrs, []} | SubEls])}.

%% Give a uniq identifier
uniqid() ->
    {T1, T2, T3} = now(),
    lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3])).

% node attributes
nodeAttr(Node) when is_list(Node) ->
    [{"node", Node}];
nodeAttr(Node) ->
    [{"node", node_to_string(Node)}].

% item attributes
itemAttr([]) -> [];
itemAttr(ItemId) -> [{"id", ItemId}].

% build item elements from item list
itemsEls(Items) ->
    lists:map(fun(#pubsub_item{itemid = {ItemId, _}, payload = Payload}) ->
	{xmlelement, "item", itemAttr(ItemId), Payload}
    end, Items).

add_message_type({xmlelement, "message", Attrs, Els}, Type) ->
    {xmlelement, "message", [{"type", Type}|Attrs], Els};
add_message_type(XmlEl, _Type) ->
    XmlEl.

%% Place of <headers/> changed at the bottom of the stanza
%% cf. http://xmpp.org/extensions/xep-0060.html#publisher-publish-success-subid
%%
%% "[SHIM Headers] SHOULD be included after the event notification information
%% (i.e., as the last child of the <message/> stanza)".

add_shim_headers(Stanza, HeaderEls) ->
    add_headers(Stanza, "headers", ?NS_SHIM, HeaderEls).

add_extended_headers(Stanza, HeaderEls) ->
    add_headers(Stanza, "addresses", ?NS_ADDRESS, HeaderEls).

add_headers({xmlelement, Name, Attrs, Els}, HeaderName, HeaderNS, HeaderEls) ->
    HeaderEl = {xmlelement, HeaderName, [{"xmlns", HeaderNS}], HeaderEls},
    {xmlelement, Name, Attrs, lists:append(Els, [HeaderEl])}.

%% Removed multiple <header name=Collection>Foo</header/> elements
%% Didn't seem compliant, but not sure. Confirmation required.
%% cf. http://xmpp.org/extensions/xep-0248.html#notify
%%
%% "If an item is published to a node which is also included by a collection,
%%  and an entity is subscribed to that collection with a subscription type of
%%  "items" (Is there a way to check that currently ?), then the notifications
%%  generated by the service MUST contain additional information. The <items/>
%%  element contained in the notification message MUST specify the node
%%  identifier of the node that generated the notification (not the collection)
%%  and the <item/> element MUST contain a SHIM header that specifies the node
%%  identifier of the collection".

collection_shim(Node) ->
    [{xmlelement, "header", [{"name", "Collection"}],
      [{xmlcdata, node_to_string(Node)}]}].

subid_shim(SubIDs) ->
    [{xmlelement, "header", [{"name", "SubID"}],
      [{xmlcdata, SubID}]} || SubID <- SubIDs].

%% The argument is a list of Jids because this function could be used
%% with the 'pubsub#replyto' (type=jid-multi) node configuration.

extended_headers(Jids) ->
    [{xmlelement, "address", [{"type", "replyto"}, {"jid", Jid}], []} || Jid <- Jids].

on_user_offline(_, JID, _) ->
    {User, Server, Resource} = jlib:jid_tolower(JID),
    case ejabberd_sm:get_user_resources(User, Server) of
	[] -> purge_offline({User, Server, Resource});
	_  -> true
    end.

purge_offline({User, Server, _} = LJID) ->
    Host = host(element(2, LJID)),
    Plugins = plugins(Host),
    Result = lists:foldl(
	fun(Type, {Status, Acc}) ->
	    case lists:member("retrieve-affiliations", features(Type)) of
		false ->
		    {{error, extended_error('feature-not-implemented', unsupported, "retrieve-affiliations")}, Acc};
		true ->
		    {result, Affiliations} = node_action(Host, Type, get_entity_affiliations, [Host, LJID]),
		    {Status, [Affiliations|Acc]}
	    end
	end, {ok, []}, Plugins),
    case Result of
	{ok, Affiliations} ->
	    lists:foreach(
		fun({#pubsub_node{nodeid = {_, NodeId}, options = Options, type = Type}, Affiliation})
		    when Affiliation == 'owner' orelse Affiliation == 'publisher' ->
			Action = fun(#pubsub_node{type = NType, id = NodeIdx}) ->
			    node_call(NType, get_items, [NodeIdx, service_jid(Host)])
			end,
			case transaction(Host, NodeId, Action, sync_dirty) of
			    {result, {_, []}}    ->
				true;
			    {result, {_, Items}} ->
				Features = features(Type),
				case
				    {lists:member("retract-items", Features),
				    lists:member("persistent-items", Features),
				    get_option(Options, persist_items),
				    get_option(Options, purge_offline)}
				of
				    {true, true, true, true} ->
					ForceNotify = get_option(Options, notify_retract),
					lists:foreach(
					    fun(#pubsub_item{itemid = {ItemId, _}, modification = {_, Modification}}) ->
						case Modification of
						    {User, Server, _} ->
							delete_item(Host, NodeId, LJID, ItemId, ForceNotify);
						    _ ->
							true
						end;
					    (_) ->
						true
					end, Items);
				    _ ->
					true
				end;
			    Error ->
				Error
			end;
		   (_) ->
		       true
	    end, lists:usort(lists:flatten(Affiliations)));
	{Error, _} ->
	    ?DEBUG("on_user_offline ~p", [Error])
    end.
