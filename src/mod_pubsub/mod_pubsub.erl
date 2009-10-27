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
%%% Portions created by ProcessOne are Copyright 2006-2009, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2009, ProcessOne.
%%%
%%% @copyright 2006-2009 ProcessOne
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
-author('christophe.romain@process-one.net').
-version('1.13-0').

-behaviour(gen_server).
-behaviour(gen_mod).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("adhoc.hrl").
-include("pubsub.hrl").

-define(STDTREE, "tree").
-define(STDNODE, "flat").
-define(PEPNODE, "pep").

%% exports for hooks
-export([presence_probe/3,
	 in_subscription/6,
	 out_subscription/4,
	 remove_user/2,
	 disco_local_identity/5,
	 disco_local_features/5,
	 disco_local_items/5,
	 disco_sm_identity/5,
	 disco_sm_features/5,
	 disco_sm_items/5
	]).
%% exported iq handlers
-export([iq_local/3,
	 iq_sm/3
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
	 broadcast_stanza/8,
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
	 rename_default_nodeplugin/0
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

-define(PROCNAME, ejabberd_mod_pubsub).
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
		plugins = [?STDNODE],
		send_loop}).


%%------------------- Ad hoc commands nodes --------------------------
-define(NS_PUBSUB_GET_PENDING, "http://jabber.org/protocol/pubsub#get-pending").
%%--------------------------------------------------------------------


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
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
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
    ServerHostB = list_to_binary(ServerHost),
    pubsub_index:init(Host, ServerHost, Opts),
    ets:new(gen_mod:get_module_proc(Host, config), [set, named_table]),
    ets:new(gen_mod:get_module_proc(ServerHost, config), [set, named_table]),
    ets:new(gen_mod:get_module_proc(Host, last_items), [set, named_table]),
    ets:new(gen_mod:get_module_proc(ServerHost, last_items), [set, named_table]),
    {Plugins, NodeTree, PepMapping} = init_plugins(Host, ServerHost, Opts),
    mod_disco:register_feature(ServerHost, ?NS_PUBSUB_s),
    ets:insert(gen_mod:get_module_proc(Host, config), {nodetree, NodeTree}),
    ets:insert(gen_mod:get_module_proc(Host, config), {plugins, Plugins}),
    ets:insert(gen_mod:get_module_proc(Host, config), {last_item_cache, LastItemCache}),
    ets:insert(gen_mod:get_module_proc(Host, config), {max_items_node, MaxItemsNode}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {nodetree, NodeTree}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {plugins, Plugins}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {last_item_cache, LastItemCache}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {max_items_node, MaxItemsNode}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {pep_mapping, PepMapping}),
    ejabberd_hooks:add(disco_sm_identity, ServerHostB, ?MODULE, disco_sm_identity, 75),
    ejabberd_hooks:add(disco_sm_features, ServerHostB, ?MODULE, disco_sm_features, 75),
    ejabberd_hooks:add(disco_sm_items, ServerHostB, ?MODULE, disco_sm_items, 75),
    ejabberd_hooks:add(presence_probe_hook, ServerHostB, ?MODULE, presence_probe, 80),
    ejabberd_hooks:add(roster_in_subscription, ServerHostB, ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, ServerHostB, ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(remove_user, ServerHostB, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, ServerHostB, ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB, ?MODULE, iq_sm, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB_OWNER, ?MODULE, iq_sm, IQDisc),
    ejabberd_router:register_route(Host),
    case lists:member(?PEPNODE, Plugins) of
	true ->
	    ejabberd_hooks:add(disco_local_identity, ServerHostB, ?MODULE, disco_local_identity, 75),
	    ejabberd_hooks:add(disco_local_features, ServerHostB, ?MODULE, disco_local_features, 75),
	    ejabberd_hooks:add(disco_local_items, ServerHostB, ?MODULE, disco_local_items, 75),
	    gen_iq_handler:add_iq_handler(ejabberd_local, ServerHostB, ?NS_PUBSUB, ?MODULE, iq_local, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_local, ServerHostB, ?NS_PUBSUB_OWNER, ?MODULE, iq_local, IQDisc);
	false ->
	    ok
    end,
    ejabberd_router:register_route(Host),
    update_node_database(Host, ServerHost),
    update_state_database(Host, ServerHost),
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
    SendLoop = spawn(?MODULE, send_loop, [State]),
    {ok, State#state{send_loop = SendLoop}}.

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
    lists:foreach(fun(Name) ->
			  ?DEBUG("** init ~s plugin",[Name]),
			  Plugin = list_to_atom(?PLUGIN_PREFIX ++ Name),
			  Plugin:init(Host, ServerHost, Opts)
		  end, Plugins),
    {Plugins, TreePlugin, PepMapping}.

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
    case lists:member("hometree", Plugins) of
    true ->
	create_node(Host, ServerHost, string_to_node("/home"), service_jid(Host), "hometree"),
	create_node(Host, ServerHost, string_to_node("/home" ++ ServerHost), service_jid(Host), "hometree");
    false ->
	ok
    end.

update_node_database(Host, ServerHost) ->
    mnesia:del_table_index(pubsub_node, type),
    mnesia:del_table_index(pubsub_node, parentid),
    case catch mnesia:table_info(pubsub_node, attributes) of
	[host_node, host_parent, info] ->
	    ?INFO_MSG("upgrade node pubsub tables",[]),
	    F = fun() ->
			{Result, LastIdx} = lists:foldl(
			  fun({pubsub_node, NodeId, ParentId, {nodeinfo, Items, Options, Entities}}, {RecList, NodeIdx}) ->
				  ItemsList =
				      lists:foldl(
					fun({item, IID, Publisher, Payload}, Acc) ->
						C = {unknown, Publisher},
						M = {now(), Publisher},
						mnesia:write(
						  #pubsub_item{itemid = {IID, NodeIdx},
							       creation = C,
							       modification = M,
							       payload = Payload}),
						[{Publisher, IID} | Acc]
					end, [], Items),
				  Owners =
				      dict:fold(
					fun(JID, {entity, Aff, Sub}, Acc) ->
						UsrItems =
						    lists:foldl(
						      fun({P, I}, IAcc) ->
							      case P of
								  JID -> [I | IAcc];
								  _ -> IAcc
							      end
						      end, [], ItemsList),
						mnesia:write({pubsub_state,
							      {JID, NodeIdx},
							      UsrItems,
							      Aff,
							      Sub}),
						case Aff of
						    owner -> [JID | Acc];
						    _ -> Acc
						end
					end, [], Entities),
				  mnesia:delete({pubsub_node, NodeId}),
				  {[#pubsub_node{nodeid = NodeId,
						id = NodeIdx,
						parents = [element(2, ParentId)],
						owners = Owners,
						options = Options} |
				   RecList], NodeIdx + 1}
			  end, {[], 1},
			  mnesia:match_object(
			    {pubsub_node, {Host, '_'}, '_', '_'})),
			mnesia:write(#pubsub_index{index = node, last = LastIdx, free = []}),
			Result
		end,
	    {atomic, NewRecords} = mnesia:transaction(F),
	    {atomic, ok} = mnesia:delete_table(pubsub_node),
	    {atomic, ok} = mnesia:create_table(pubsub_node,
					       [{disc_copies, [node()]},
						{attributes, record_info(fields, pubsub_node)}]),
	    FNew = fun() -> lists:foreach(fun(Record) ->
						  mnesia:write(Record)
					  end, NewRecords)
		   end,
	    case mnesia:transaction(FNew) of
		{atomic, Result} ->
		    ?INFO_MSG("Pubsub node tables updated correctly: ~p", [Result]);
		{aborted, Reason} ->
		    ?ERROR_MSG("Problem updating Pubsub node tables:~n~p", [Reason])
	    end;
	[nodeid, parentid, type, owners, options] ->
	    F = fun({pubsub_node, NodeId, {_, Parent}, Type, Owners, Options}) ->
		    #pubsub_node{
			nodeid = NodeId,
			id = 0,
			parents = [Parent],
			type = Type,
			owners = Owners,
			options = Options}
		end,
	    mnesia:transform_table(pubsub_node, F, [nodeid, id, parents, type, owners, options]),
	    FNew = fun() ->
		LastIdx = lists:foldl(fun(#pubsub_node{nodeid = NodeId} = PubsubNode, NodeIdx) ->
		    mnesia:write(PubsubNode#pubsub_node{id = NodeIdx}),
		    lists:foreach(fun(#pubsub_state{stateid = StateId} = State) ->
			{JID, _} = StateId,
			mnesia:delete({pubsub_state, StateId}),
			mnesia:write(State#pubsub_state{stateid = {JID, NodeIdx}})
		    end, mnesia:match_object(#pubsub_state{stateid = {'_', NodeId}, _ = '_'})),
		    lists:foreach(fun(#pubsub_item{itemid = ItemId} = Item) ->
			{IID, _} = ItemId,
			{M1, M2} = Item#pubsub_item.modification,
			{C1, C2} = Item#pubsub_item.creation,
			mnesia:delete({pubsub_item, ItemId}),
			mnesia:write(Item#pubsub_item{itemid = {IID, NodeIdx},
						    modification = {M2, M1},
						    creation = {C2, C1}})
		    end, mnesia:match_object(#pubsub_item{itemid = {'_', NodeId}, _ = '_'})),
		    NodeIdx + 1
		end, 1, mnesia:match_object(
			{pubsub_node, {Host, '_'}, '_', '_', '_', '_', '_'})
		    ++  mnesia:match_object(
			{pubsub_node, {{'_', ServerHost, '_'}, '_'}, '_', '_', '_', '_', '_'})),
		mnesia:write(#pubsub_index{index = node, last = LastIdx, free = []})
		end,
	    case mnesia:transaction(FNew) of
		{atomic, Result} ->
		    rename_default_nodeplugin(),
		    ?INFO_MSG("Pubsub node tables updated correctly: ~p", [Result]);
		{aborted, Reason} ->
		    ?ERROR_MSG("Problem updating Pubsub node tables:~n~p", [Reason])
	    end;
	[nodeid, id, parent, type, owners, options] ->
	    F = fun({pubsub_node, NodeId, Id, Parent, Type, Owners, Options}) ->
		    #pubsub_node{
			nodeid = NodeId,
			id = Id,
			parents = [Parent],
			type = Type,
			owners = Owners,
			options = Options}
		end,
	    mnesia:transform_table(pubsub_node, F, [nodeid, id, parents, type, owners, options]),
	    rename_default_nodeplugin();
	_ ->
	    ok
    end,
    mnesia:transaction(fun() ->
	    case catch mnesia:first(pubsub_node) of
	    {_, L} when is_list(L) ->
            lists:foreach(
                    fun({H, N}) when is_list(N) ->
                        [Node] = mnesia:read({pubsub_node, {H, N}}),
                         Type = Node#pubsub_node.type,
                         BN = element(2, node_call(Type, path_to_node, [N])),
                         BP = case [element(2, node_call(Type, path_to_node, [P])) || P <- Node#pubsub_node.parents] of
                            [<<>>] -> [];
                            Parents -> Parents
                         end,
                         mnesia:write(Node#pubsub_node{nodeid={H, BN}, parents=BP}),
                         mnesia:delete({pubsub_node, {H, N}});
                      (_) ->
                        ok
	        end, mnesia:all_keys(pubsub_node));
	    _ ->
	        ok
    	end
    end). 

rename_default_nodeplugin() ->
    lists:foreach(fun(Node) ->
	mnesia:dirty_write(Node#pubsub_node{type = "hometree"})
    end, mnesia:dirty_match_object(#pubsub_node{type = "default", _ = '_'})).

update_state_database(_Host, _ServerHost) ->
    case catch mnesia:table_info(pubsub_state, attributes) of
	[stateid, items, affiliation, subscription] ->
	    ?INFO_MSG("upgrade state pubsub tables", []),
	    F = fun ({pubsub_state, {JID, NodeID}, Items, Aff, Sub}, Acc) ->
			Subs = case Sub of
				   none ->
				       [];
				   _ ->
				       {result, SubID} = pubsub_subscription:subscribe_node(JID, NodeID, []),
				       [{Sub, SubID}]
			       end,
			NewState = #pubsub_state{stateid       = {JID, NodeID},
						 items	 = Items,
						 affiliation   = Aff,
						 subscriptions = Subs},
			[NewState | Acc]
		end,
	    {atomic, NewRecs} = mnesia:transaction(fun mnesia:foldl/3,
						   [F, [], pubsub_state]),
	    {atomic, ok} = mnesia:delete_table(pubsub_state),
	    {atomic, ok} = mnesia:create_table(pubsub_state,
					       [{disc_copies, [node()]},
						{attributes, record_info(fields, pubsub_state)}]),
	    FNew = fun () ->
			   lists:foreach(fun mnesia:write/1, NewRecs)
		   end,
	    case mnesia:transaction(FNew) of
		{atomic, Result} ->
		    ?INFO_MSG("Pubsub state tables updated correctly: ~p",
			      [Result]);
		{aborted, Reason} ->
		    ?ERROR_MSG("Problem updating Pubsub state tables:~n~p",
			       [Reason])
	    end;
	_ ->
	    ok
    end.

send_queue(State, Msg) ->
    Pid = State#state.send_loop,
    case is_process_alive(Pid) of
    true ->
	Pid ! Msg,
	State;
    false ->
	SendLoop = spawn(?MODULE, send_loop, [State]),
	SendLoop ! Msg,
	State#state{send_loop = SendLoop}
    end.

send_loop(State) ->
    receive
    {presence, JID, Pid} ->
	Host = State#state.host,
	ServerHost = State#state.server_host,
	LJID = jlib:short_prepd_jid(JID),
	BJID = jlib:short_prepd_bare_jid(JID),
	%% for each node From is subscribed to
	%% and if the node is so configured, send the last published item to From
	lists:foreach(fun(PType) ->
	    {result, Subscriptions} = node_action(Host, PType, get_entity_subscriptions, [Host, JID]),
	    lists:foreach(
		fun({Node, subscribed, _, SubJID}) -> 
		    if (SubJID == LJID) or (SubJID == BJID) ->
			#pubsub_node{nodeid = {H, N}, type = Type, id = NodeId, options = Options} = Node,
			case get_option(Options, send_last_published_item) of
			    on_sub_and_presence ->
				send_items(H, N, NodeId, Type, LJID, last);
			    _ ->
				ok
			end;
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
	    {User, Server, Resource} = LJID,
	    case mod_caps:get_caps({User, Server, Resource}) of
	    nothing ->
		%% we don't have caps, no need to handle PEP items
		ok;
	    _ ->
		case catch ejabberd_c2s:get_subscribed(Pid) of
		Contacts when is_list(Contacts) ->
		    lists:foreach(
			fun({U, S, R}) ->
			    case S of
			    ServerHost ->  %% local contacts
				case ejabberd_sm:get_user_resources(U, S) of
				[] -> %% offline
				    PeerJID = exmpp_jid:make(U, S, R),
				    self() ! {presence, User, Server, [Resource], PeerJID};
				_ -> %% online
				    % this is already handled by presence probe
				    ok
				end;
			    _ -> %% remote contacts
				% we can not do anything in any cases
				ok
			    end
			end, Contacts);
		_ ->
		    ok
		end
	    end;
	true ->
	    ok
	end,
	send_loop(State);
    {presence, User, Server, Resources, JID} ->
	%% get resources caps and check if processing is needed
	spawn(fun() ->
	    {HasCaps, ResourcesCaps} = lists:foldl(fun(Resource, {R, L}) ->
			case mod_caps:get_caps({User, Server, Resource}) of
			nothing -> {R, L};
			Caps -> {true, [{Resource, Caps} | L]}
			end
		    end, {false, []}, Resources),
	    case HasCaps of
		true ->
		    Host = State#state.host,
		    ServerHost = State#state.server_host,
		    Owner = jlib:short_prepd_bare_jid(JID),
		    lists:foreach(fun(#pubsub_node{nodeid = {_, Node}, type = Type, id = NodeId, options = Options}) ->
			case get_option(Options, send_last_published_item) of
			    on_sub_and_presence ->
				lists:foreach(fun({Resource, Caps}) ->
				    CapsNotify = case catch mod_caps:get_features(ServerHost, Caps) of
					    Features when is_list(Features) -> lists:member(node_to_string(Node) ++ "+notify", Features);
					    _ -> false
					end,
				    case CapsNotify of
					true ->
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
					    end;
					false ->
					    ok
				    end
				end, ResourcesCaps);
			    _ ->
				ok
			end
		    end, tree_action(Host, get_nodes, [Owner, JID]));
		false ->
		    ok
	    end
	end),
	send_loop(State);
    stop ->
	ok
    end.

%% -------
%% disco hooks handling functions
%%

identity(Host) ->
    Identity = case lists:member(?PEPNODE, plugins(Host)) of
    true -> [?XMLATTR('category', <<"pubsub">>), ?XMLATTR('type', <<"pep">>)];
    false -> [?XMLATTR('category', <<"pubsub">>), ?XMLATTR('type', <<"service">>)]
    end,
    #xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs = Identity}.

disco_local_identity(Acc, _From, To, <<>>, _Lang) ->
    Acc ++ [identity(exmpp_jid:prep_domain_as_list(To))];
disco_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_local_features(Acc, _From, To, <<>>, _Lang) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    Feats = case Acc of
	{result, I} -> I;
	_ -> []
    end,
    {result, Feats ++ lists:map(fun(Feature) ->
	?NS_PUBSUB_s++"#"++Feature
    end, features(Host, []))};
disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_local_items(Acc, _From, _To, <<>>, _Lang) ->
    Acc;
disco_local_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_sm_identity(Acc, _From, To, <<>>, _Lang) ->
    Acc ++ [identity(exmpp_jid:prep_domain_as_list(To))];
disco_sm_identity(Acc, From, To, Node, _Lang) ->
    LOwner = jlib:short_prepd_bare_jid(To),
    Acc ++ case node_disco_identity(LOwner, From, Node) of
	       {result, I} -> I;
	       _ -> []
	   end.

disco_sm_features(Acc, _From, _To, [], _Lang) ->
    Acc;
disco_sm_features(Acc, From, To, Node, _Lang) ->
    LOwner = jlib:short_prepd_bare_jid(To),
    Features = node_disco_features(LOwner, From, Node),
    case {Acc, Features} of
	{{result, AccFeatures}, {result, AddFeatures}} ->
	    {result, AccFeatures++AddFeatures};
	{_, {result, AddFeatures}} ->
	    {result, AddFeatures};
	{_, _} ->
	    Acc
    end.

disco_sm_items(Acc, From, To, <<>>, _Lang) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    case tree_action(Host, get_subnodes, [Host, <<>>, From]) of
	[] ->
	    Acc;
	Nodes ->
	    SBJID = jlib:short_prepd_bare_jid(To),
	    Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
	    NodeItems = lists:map(
			  fun(#pubsub_node{nodeid = {_, Node}}) ->
				  #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
				   [?XMLATTR('jid', exmpp_jid:to_binary(SBJID)) | nodeAttr(Node)]}
			  end, Nodes),
	    {result, NodeItems ++ Items}
    end;

disco_sm_items(Acc, From, To, NodeB, _Lang) ->
    SNode = binary_to_list(NodeB),
    Node = string_to_node(SNode),
    %% TODO, use iq_disco_items(Host, Node, From)
    Host = exmpp_jid:prep_domain_as_list(To),
    LJID = jlib:short_prepd_bare_jid(To),
    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
	% TODO call get_items/6 instead for access control (EJAB-1033)
	case node_call(Type, get_items, [NodeId, From]) of
	    {result, []} ->
		none;
	    {result, AllItems} ->
		Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
		NodeItems = lists:map(
			  fun(#pubsub_item{itemid = {Id, _}}) ->
				  {result, Name} = node_call(Type, get_item_name, [Host, Node, Id]),
				  #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
				   [?XMLATTR('jid', exmpp_jid:to_binary(LJID)),
				    ?XMLATTR('name', Name)]}
			  end, AllItems),
		{result, NodeItems ++ Items};
	    _ ->
		none
	end
    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Items}} -> {result, Items};
	_ -> Acc
    end.

%% -------
%% presence hooks handling functions
%%

presence_probe(JID, JID, Pid) ->
    {User, Server, Resource} = jlib:short_prepd_jid(JID),
    Host = exmpp_jid:prep_domain_as_list(JID),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {presence, JID, Pid}),
    gen_server:cast(Proc, {presence, User, Server, [Resource], JID});
presence_probe(Peer, JID, _Pid) ->
    {User, Server, Resource} = jlib:short_prepd_jid(Peer),
    Host = exmpp_jid:prep_domain_as_list(JID),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {presence, User, Server, [Resource], JID}).

%% -------
%% subscription hooks handling functions
%%

out_subscription(User, Server, JID, subscribed) ->
    Owner = exmpp_jid:make(User, Server, ""),
    {U, S, R} = jlib:short_prepd_jid(JID),
    Rs = case R of
	[] -> user_resources(U, S);
	_ -> [R]
    end,
    Proc = gen_mod:get_module_proc(Server, ?PROCNAME),
    gen_server:cast(Proc, {presence, U, S, Rs, Owner});
out_subscription(_, _, _, _) ->
    ok.
in_subscription(_, User, Server, Owner, unsubscribed, _) ->
    Subscriber = exmpp_jid:make(User, Server, ""),
    Proc = gen_mod:get_module_proc(Server, ?PROCNAME),
    gen_server:cast(Proc, {unsubscribe, Subscriber, Owner});
in_subscription(_, _, _, _, _, _) ->
    ok.

%% -------
%% user remove hook handling function
%%

remove_user(User, Server) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    Proc = gen_mod:get_module_proc(Server, ?PROCNAME),
    gen_server:cast(Proc, {remove_user, LUser, LServer}).

%%--------------------------------------------------------------------
%% Function:
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
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
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({presence, JID, Pid}, State) ->
    %% A new resource is available. send last published items
    {noreply, send_queue(State, {presence, JID, Pid})};

handle_cast({presence, User, Server, Resources, JID}, State) ->
    %% A new resource is available. send last published PEP items
    {noreply, send_queue(State, {presence, User, Server, Resources, JID})};

handle_cast({remove_user, LUser, LServer}, State) ->
    spawn(fun() ->
	Host = State#state.host,
	Owner = exmpp_jid:make(LUser, LServer),
	%% remove user's subscriptions
	lists:foreach(fun(PType) ->
	    {result, Subscriptions} = node_action(Host, PType, get_entity_subscriptions, [Host, Owner]),
	    lists:foreach(fun
		({#pubsub_node{nodeid = {H, N}}, subscribed, _, JID}) ->
		    unsubscribe_node(H, N, Owner, JID, all);
		(_) ->
		    ok
	    end, Subscriptions),
	    {result, Affiliations} = node_action(Host, PType, get_entity_affiliations, [Host, Owner]),
	    lists:foreach(fun
		({#pubsub_node{nodeid = {H, N}}, owner}) ->
		    delete_node(H, N, Owner);
		(_) ->
		    ok
	    end, Affiliations)
	end, State#state.plugins)
    end),
    {noreply, State}; 

handle_cast({unsubscribe, Subscriber, Owner}, State) ->
    spawn(fun() ->
	Host = State#state.host,
	BJID = jlib:short_prepd_bare_jid(Owner),
	lists:foreach(fun(PType) ->
	    {result, Subscriptions} = node_action(Host, PType, get_entity_subscriptions, [Host, Subscriber]),
	    lists:foreach(fun
		({Node, subscribed, _, JID}) ->
		    #pubsub_node{options = Options, owners = Owners, type = Type, id = NodeId} = Node,
		    case get_option(Options, access_model) of
			presence ->
			    case lists:member(BJID, Owners) of
				true ->
				    node_action(Host, Type, unsubscribe_node, [NodeId, Subscriber, JID, all]);
				false ->
				    {result, ok}
			    end;
			_ ->
			    {result, ok}
		    end;
		(_) ->  
		    ok
	    end, Subscriptions)
	end, State#state.plugins)
    end),
    {noreply, State}; 

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({route, From, To, Packet},
	    #state{server_host = ServerHost,
		   access = Access,
		   plugins = Plugins} = State) ->
    case catch do_route(ServerHost, Access, Plugins, exmpp_jid:prep_domain_as_list(To), From, To, Packet) of
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
			  plugins = Plugins,
			  send_loop = SendLoop}) ->
    ejabberd_router:unregister_route(Host),
    ServerHostB = list_to_binary(ServerHost),
    case lists:member(?PEPNODE, Plugins) of
    true ->
	ejabberd_hooks:delete(disco_local_identity, ServerHostB, ?MODULE, disco_local_identity, 75),
	ejabberd_hooks:delete(disco_local_features, ServerHostB, ?MODULE, disco_local_features, 75),
	ejabberd_hooks:delete(disco_local_items, ServerHostB, ?MODULE, disco_local_items, 75),
	gen_iq_handler:remove_iq_handler(ejabberd_local, ServerHostB, ?NS_PUBSUB),
	gen_iq_handler:remove_iq_handler(ejabberd_local, ServerHostB, ?NS_PUBSUB_OWNER);
    false ->
	ok
    end,
    ejabberd_hooks:delete(disco_local_identity, ServerHostB, ?MODULE, disco_local_identity, 75),
    ejabberd_hooks:delete(disco_local_features, ServerHostB, ?MODULE, disco_local_features, 75),
    ejabberd_hooks:delete(disco_local_items, ServerHostB, ?MODULE, disco_local_items, 75),
    ejabberd_hooks:delete(disco_sm_identity, ServerHostB, ?MODULE, disco_sm_identity, 75),
    ejabberd_hooks:delete(disco_sm_features, ServerHostB, ?MODULE, disco_sm_features, 75),
    ejabberd_hooks:delete(disco_sm_items, ServerHostB, ?MODULE, disco_sm_items, 75),
    ejabberd_hooks:delete(presence_probe_hook, ServerHostB, ?MODULE, presence_probe, 80),
    ejabberd_hooks:delete(roster_in_subscription, ServerHostB, ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, ServerHostB, ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(remove_user, ServerHostB, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, ServerHostB, ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB_OWNER),
    mod_disco:unregister_feature(ServerHost, ?NS_PUBSUB_s),
    SendLoop ! stop,
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
    #xmlel{name = Name} = Packet,
    LNode = exmpp_jid:prep_node(To),
    LRes = exmpp_jid:prep_resource(To),
    case {LNode, LRes} of
	{undefined, undefined} ->
	    case Name of
		'iq' ->
		    case exmpp_iq:xmlel_to_iq(Packet) of
			#iq{type = get, ns = ?NS_DISCO_INFO,
			    payload = SubEl, lang = Lang} ->
			    QAttrs = SubEl#xmlel.attrs,
			    Node = exmpp_xml:get_attribute_from_list_as_list(QAttrs, 'node', ""),
			    ServerHostB = list_to_binary(ServerHost),
			    Info = ejabberd_hooks:run_fold(
				     disco_info, ServerHostB, [],
				     [ServerHost, ?MODULE, <<>>, ""]),
			    Res = case iq_disco_info(Host, Node, From, Lang) of
				      {result, IQRes} ->
					  Result = #xmlel{ns = ?NS_DISCO_INFO,
					    name = 'query', attrs = QAttrs,
					    children = IQRes++Info},
					  exmpp_iq:result(Packet, Result);
				      {error, Error} ->
					  exmpp_iq:error(Packet, Error)
				  end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = get, ns = ?NS_DISCO_ITEMS,
			    payload = SubEl} ->
			    QAttrs = SubEl#xmlel.attrs,
			    Node = exmpp_xml:get_attribute_from_list_as_list(QAttrs,
			      'node', ""),
			    Res = case iq_disco_items(Host, Node, From) of
				      {result, IQRes} ->
					  Result = #xmlel{ns = ?NS_DISCO_ITEMS,
					    name = 'query', attrs = QAttrs,
					    children = IQRes},
					  exmpp_iq:result(Packet, Result);
				      {error, Error} ->
					  exmpp_iq:error(Packet, Error)
				  end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = IQType, ns = ?NS_PUBSUB,
			    lang = Lang, payload = SubEl} ->
			    Res =
				case iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, Access, Plugins) of
				    {result, IQRes} ->
					exmpp_iq:result(Packet, IQRes);
				    {error, Error} ->
					exmpp_iq:error(Packet, Error)
				end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = IQType, ns = ?NS_PUBSUB_OWNER,
			    lang = Lang, payload = SubEl} ->
			    Res =
				case iq_pubsub_owner(Host, ServerHost, From, IQType, SubEl, Lang) of
				    {result, []} ->
				    	exmpp_iq:result(Packet);
				    {result, IQRes} ->
					exmpp_iq:result(Packet, IQRes);
				    {error, Error} ->
					exmpp_iq:error(Packet, Error)
				end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = get, ns = ?NS_VCARD = XMLNS,
			    lang = Lang} ->
			    VCard = #xmlel{ns = XMLNS, name = 'vCard',
			      children = iq_get_vcard(Lang)},
			    Res = exmpp_iq:result(Packet, VCard),
			    ejabberd_router:route(To, From, Res);
			#iq{type = set, ns = ?NS_ADHOC} = IQ ->
				Res = case iq_command(Host, ServerHost, From, IQ, Access, Plugins) of
					{error, Error} ->
						exmpp_iq:error(Packet, Error);
					{result, IQRes} ->
						exmpp_iq:result(Packet, IQRes)
				end,
				ejabberd_router:route(To, From, Res);

			#iq{} ->
			    Err = exmpp_iq:error(Packet,
			      'feature-not-implemented'),
			    ejabberd_router:route(To, From, Err);
			_ ->
			    ok
		    end;
		'message' ->
		    case exmpp_stanza:is_stanza_error(Packet) of
			true ->
			    ok;
			false ->
			    case find_authorization_response(Packet) of
				none ->
				    ok;
				invalid ->
				    ejabberd_router:route(To, From,
							  exmpp_message:error(Packet, 'bad-request'));
				XFields ->
				    handle_authorization_response(Host, From, To, Packet, XFields)
			    end
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    case exmpp_stanza:get_type(Packet) of
		<<"error">> ->
		    ok;
		<<"result">> ->
		    ok;
		_ ->
		    Err = exmpp_stanza:reply_with_error(Packet,
		      'item-not-found'),
		    ejabberd_router:route(To, From, Err)
	    end
    end.

node_disco_info(Host, Node, From) ->
    node_disco_info(Host, Node, From, true, true).
node_disco_identity(Host, Node, From) ->
    node_disco_info(Host, Node, From, true, false).
node_disco_features(Host, Node, From) ->
    node_disco_info(Host, Node, From, false, true).
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
					case node_call(Type, get_items, [NodeId, From]) of
					    {result, []} -> ["collection"];
					    {result, _} -> ["leaf", "collection"];
					    _ -> []
					end
				end,
			    lists:map(fun(T) ->
					      #xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs = [?XMLATTR('category', <<"pubsub">>),
									?XMLATTR('type', T)]}
				      end, Types)
		    end,
		F = case Features of
			false ->
			    [];
			true ->
			    [#xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR('var', ?NS_PUBSUB_s)]} |
			     lists:map(fun(T) ->
					       #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR('var', ?NS_PUBSUB_s++"#"++T)]}
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
	     [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
	       [?XMLATTR('category', "pubsub"),
		?XMLATTR('type', "service"),
		?XMLATTR('name', translate:translate(Lang, "Publish-Subscribe"))]},
		#xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR('var', ?NS_DISCO_INFO_s)]},
		#xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR('var', ?NS_DISCO_ITEMS_s)]},
		#xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR('var', ?NS_PUBSUB_s)]},
		#xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR('var', ?NS_VCARD_s)]}] ++
	     lists:map(fun(Feature) ->
		 #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR('var', ?NS_PUBSUB_s++"#"++Feature)]}
	     end, features(Host, Node))};
	_ ->
	    node_disco_info(Host, Node, From)
    end.

iq_disco_items(Host, [], From) ->
    case tree_action(Host, get_subnodes, [Host, <<>>, From]) of
	Nodes when is_list(Nodes) ->
	    {result, lists:map(
		fun(#pubsub_node{nodeid = {_, SubNode}, type = Type}) ->
		    {result, Path} = node_call(Type, node_to_path, [SubNode]),
		    [Name | _] = lists:reverse(Path),
		    #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs = [?XMLATTR('jid', Host),
			?XMLATTR('name', Name) | nodeAttr(SubNode)]}
		end, Nodes)};
	Other ->
	    Other
    end;
iq_disco_items(Host, Item, From) ->
    case string:tokens(Item, "!") of
	[_SNode, _ItemID] ->
	    {result, []};
	[SNode] ->
	    Node = string_to_node(SNode),
	    Action =
		fun(#pubsub_node{type = Type, id = NodeId}) ->
			% TODO call get_items/6 instead for access control (EJAB-1033)
			NodeItems = case node_call(Type, get_items, [NodeId, From]) of
					{result, I} -> I;
					_ -> []
				    end,
			Nodes = lists:map(
				  fun(#pubsub_node{nodeid = {_, SubNode}}) ->
					  {result, Path} = node_call(Type, node_to_path, [SubNode]),
					  [Name|_] = lists:reverse(Path),
					  #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs = [?XMLATTR('jid', Host), ?XMLATTR('name', Name) | nodeAttr(SubNode)]}
				  end, tree_call(Host, get_subnodes, [Host, Node, From])),
			Items = lists:map(
				  fun(#pubsub_item{itemid = {RN, _}}) ->
					  {result, Name} = node_call(Type, get_item_name, [Host, Node, RN]),
					  #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs = [?XMLATTR('jid', Host), ?XMLATTR('name', Name)]}
				  end, NodeItems),
			{result, Nodes ++ Items}
		end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Result}} -> {result, Result};
		Other -> Other
	    end
    end.

iq_local(From, To, #iq{type = Type, payload = SubEl, ns = XMLNS, lang = Lang} = IQ_Rec) ->
    ServerHost = exmpp_jid:prep_domain_as_list(To),
    FromHost = exmpp_jid:prep_domain_as_list(To),
    %% Accept IQs to server only from our own users.
    if
	FromHost /= ServerHost ->
	    exmpp_iq:error(IQ_Rec, 'forbidden');
	true ->
	    LOwner = jlib:short_prepd_bare_jid(From),
	    Res = case XMLNS of
		      ?NS_PUBSUB -> iq_pubsub(LOwner, ServerHost, From, Type, SubEl, Lang);
		      ?NS_PUBSUB_OWNER -> iq_pubsub_owner(LOwner, ServerHost, From, Type, SubEl, Lang)
		  end,
	    case Res of
		{result, []}      -> exmpp_iq:result(IQ_Rec);
		{result, [IQRes]} -> exmpp_iq:result(IQ_Rec, IQRes);
		{error, Error}    -> exmpp_iq:error(IQ_Rec, Error)
	    end
    end.

iq_sm(From, To, #iq{type = Type, payload = SubEl, ns = XMLNS, lang = Lang} = IQ_Rec) ->
    ServerHost = exmpp_jid:prep_domain_as_list(To),
    LOwner = jlib:short_prepd_bare_jid(To),
    Res = case XMLNS of
	      ?NS_PUBSUB -> iq_pubsub(LOwner, ServerHost, From, Type, SubEl, Lang);
	      ?NS_PUBSUB_OWNER -> iq_pubsub_owner(LOwner, ServerHost, From, Type, SubEl, Lang)
	  end,
    case Res of
	{result, []}      -> exmpp_iq:result(IQ_Rec);
	{result, [IQRes]} -> exmpp_iq:result(IQ_Rec, IQRes);
	{error, Error}    -> exmpp_iq:error(IQ_Rec, Error)
    end.

iq_get_vcard(Lang) ->
    [#xmlel{ns = ?NS_VCARD, name = 'FN', children = [#xmlcdata{cdata = <<"ejabberd/mod_pubsub">>}]},
     #xmlel{ns = ?NS_VCARD, name = 'URL', children = [#xmlcdata{cdata = list_to_binary(?EJABBERD_URI)}]},
     #xmlel{ns = ?NS_VCARD, name = 'DESC', children =
      [#xmlcdata{cdata = list_to_binary(
	translate:translate(Lang,
			    "ejabberd Publish-Subscribe module") ++
			    "\nCopyright (c) 2004-2009 Process-One")}]}].

iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang) ->
    iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, all, plugins(ServerHost)).

iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, Access, Plugins) ->
    case exmpp_xml:remove_cdata_from_list(SubEl#xmlel.children) of
	[#xmlel{name = Name, attrs = Attrs, children = Els} | Rest] ->
	    Node = string_to_node(exmpp_xml:get_attribute_from_list_as_list(attrs, 'node', false)),
	    case {IQType, Name} of
		{set, 'create'} ->
		    Config = case Rest of
			[#xmlel{name = 'configure', children = C}] -> C;
			_ -> []
		    end,
		    %% Get the type of the node
		    Type = case exmpp_xml:get_attribute_from_list_as_list(Attrs, 'type', "") of
				[] -> hd(Plugins);
				T -> T
			    end,
		    %% we use Plugins list matching because we do not want to allocate
		    %% atoms for non existing type, this prevent atom allocation overflow
		    case lists:member(Type, Plugins) of
			false ->
			    {error, extended_error(
					'feature-not-implemented',
					unsupported, "create-nodes")};
			true ->
			    create_node(Host, ServerHost, Node, From,
					Type, Access, Config)
		    end;
		{set, 'publish'} ->
		    case exmpp_xml:remove_cdata_from_list(Els) of
			[#xmlel{name = 'item', attrs = ItemAttrs, children = Payload}] ->
			    ItemId = exmpp_xml:get_attribute_from_list_as_list(ItemAttrs, 'id', ""),
			    publish_item(Host, ServerHost, Node, From, ItemId, Payload);
			[] ->
			    %% Publisher attempts to publish to persistent node with no item
			    {error, extended_error('bad-request',
						   "item-required")};
			_ ->
			    %% Entity attempts to publish item with multiple payload elements or namespace does not match
			    {error, extended_error('bad-request',
						   "invalid-payload")}
		    end;
		{set, 'retract'} ->
		    ForceNotify = case exmpp_xml:get_attribute_from_list_as_list(Attrs, 'notify', "") of
				      "1" -> true;
				      "true" -> true;
				      _ -> false
				  end,
		    case exmpp_xml:remove_cdata_from_list(Els) of
			[#xmlel{name = 'item', attrs = ItemAttrs}] ->
			    ItemId = exmpp_xml:get_attribute_from_list_as_list(ItemAttrs, 'id', ""),
			    delete_item(Host, Node, From, ItemId, ForceNotify);
			_ ->
			    %% Request does not specify an item
			    {error, extended_error('bad-request',
						   "item-required")}
		    end;
		{set, 'subscribe'} ->
		    Config = case Rest of
			[#xmlel{name = 'configure', children = C}] -> C;
			_ -> []
		    end,
		    JID = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'jid', ""),
		    subscribe_node(Host, Node, From, JID, Config);
		{set, 'unsubscribe'} ->
		    JID = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'jid', ""),
		    SubId = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'subid', ""),
		    unsubscribe_node(Host, Node, From, JID, SubId);
		{get, 'items'} ->
		    MaxItems = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'max_items', ""),
		    SubId = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'subid', ""),
		    ItemIDs = lists:foldl(fun
			(#xmlel{name = 'item', attrs = ItemAttrs}, Acc) ->
			    case exmpp_xml:get_attribute_from_list_as_list(ItemAttrs, 'id', "") of
			    "" -> Acc;
			    ItemID -> [ItemID|Acc]
			    end;
			(_, Acc) ->
			    Acc
			end, [], exmpp_xml:remove_cdata_from_list(Els)),
		    get_items(Host, Node, From, SubId, MaxItems, ItemIDs);
		{get, 'subscriptions'} ->
		    get_subscriptions(Host, Node, From, Plugins);
		{get, 'affiliations'} ->
		    get_affiliations(Host, From, Plugins);
		{get, "options"} ->
		    SubID = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'subid', ""),
		    JID = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'jid', ""),
		    get_options(Host, Node, JID, SubID, Lang);
		{set, "options"} ->
		    SubID = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'subid', ""),
		    JID = exmpp_xml:get_attribute_from_list_as_list(Attrs, 'jid', ""),
		    set_options(Host, Node, JID, SubID, Els);
		_ ->
		    {error, 'feature-not-implemented'}
	    end;
	Other ->
	    ?INFO_MSG("Too many actions: ~p", [Other]),
	    {error, 'bad-request'}
    end.

iq_pubsub_owner(Host, ServerHost, From, IQType, SubEl, Lang) ->
    SubEls = SubEl#xmlel.children,
    Action = exmpp_xml:remove_cdata_from_list(SubEls),
    case Action of
	[#xmlel{name = Name, attrs = Attrs, children = Els}] ->
	    Node = string_to_node(exmpp_xml:get_attribute_from_list_as_list(Attrs, 'node', "")),
	    case {IQType, Name} of
		{get, 'configure'} ->
		    get_configure(Host, ServerHost, Node, From, Lang);
		{set, 'configure'} ->
		    set_configure(Host, Node, From, Els, Lang);
		{get, 'default'} ->
		    get_default(Host, Node, From, Lang);
		{set, 'delete'} ->
		    delete_node(Host, Node, From);
		{set, 'purge'} ->
		    purge_node(Host, Node, From);
		{get, 'subscriptions'} ->
		    get_subscriptions(Host, Node, From);
		{set, 'subscriptions'} ->
		    set_subscriptions(Host, Node, From, exmpp_xml:remove_cdata_from_list(Els));
		{get, 'affiliations'} ->
		    get_affiliations(Host, Node, From);
		{set, 'affiliations'} ->
		    set_affiliations(Host, Node, From, exmpp_xml:remove_cdata_from_list(Els));
		_ ->
		    {error, 'feature-not-implemented'}
	    end;
	_ ->
	    ?INFO_MSG("Too many actions: ~p", [Action]),
	    {error, 'bad-request'}
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
    			#xmlel{name = 'x'} = XEl ->
			   case jlib:parse_xdata_submit(XEl) of
			       invalid ->
				   {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'bad-request')};
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
			   {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'bad-request')}
		   end,
    case ParseOptions of
	{result, XForm} ->
	    case lists:keysearch(node, 1, XForm) of
		{value, {_, Node}} ->
		    send_pending_auth_events(Host, Node, Owner);
		false ->
		    {error, extended_error('bad-request', "bad-payload")}
	    end;
	Error ->
	    Error
    end;
adhoc_request(_Host, _ServerHost, _Owner, Other, _Access, _Plugins) ->
    ?DEBUG("Couldn't process ad hoc command:~n~p", [Other]),
    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'item-not-found')}.

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
	    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'feature-not-implemented')};
	Ps ->
	    XOpts = lists:map(fun (Node) ->
	    			      #xmlel{ns = ?NS_DATA_FORMS, name='option',
				      	     children = [
				     	#xmlel{ns = ?NS_DATA_FORMS, name = 'value',
						children = [
							exmpp_xml:cdata(node_to_string(Node))]}]}
			      end, get_pending_nodes(Host, Owner, Ps)),
	    XForm = #xmlel{ns = ?NS_DATA_FORMS, name ='x', attrs = [?XMLATTR('type', <<"form">>)],
		    	  children = [
			  	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', 
				       attrs = [?XMLATTR('type', <<"list-single">>),
					         ?XMLATTR('var', <<"pubsub#node">>)],
				       children = lists:usort(XOpts)}]},
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
    case transaction(fun () -> {result, lists:flatmap(Tr, Plugins)} end,
		     sync_dirty) of
	{result, Res} -> Res;
	Err	   -> Err
    end.

%% @spec (Host, Node, Owner) -> iqRes()
%% @doc <p>Send a subscription approval form to Owner for all pending
%% subscriptions on Host and Node.</p>
send_pending_auth_events(Host, Node, Owner) ->
    ?DEBUG("Sending pending auth events for ~s on ~s:~s",
	   [exmpp_jid:jid_to_string(Owner), Host, node_to_string(Node)]),
    Action =
	fun(#pubsub_node{id = NodeID, type = Type}) ->
		case lists:member("get-pending", features(Type)) of
		    true ->
			case node_call(Type, get_affiliation, [NodeID, Owner]) of
			    {result, owner} ->
				{result, Subscriptions} = node_call(Type, get_node_subscriptions, [NodeID]),
				lists:foreach(fun({J, pending, _SubID}) ->
						    {U, S, R} = J,
						    send_authorization_request(Node, exmpp_jid:make(U,S,R));
						 ({J, pending}) ->
						    {U, S, R} = J,
						    send_authorization_request(Node, exmpp_jid:make(U,S,R));
						 (_) ->
						    ok
					    end, Subscriptions),
				{result, ok};
			    _ ->
				{error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'forbidden')}
			end;
		    false ->
			{error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'feature-not-implemented')}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, _} ->
	    #adhoc_response{};
	Err ->
	    Err
    end.

%%% authorization handling

send_authorization_request(#pubsub_node{owners = Owners, nodeid = {Host, Node}}, Subscriber) ->
    Lang = "en", %% TODO fix
    {U, S, R} = Subscriber,
    Stanza = #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message', children =
	      [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = [?XMLATTR('type', <<"form">>)], children =
		[#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		  [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "PubSub subscriber request"))}]},
		 #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
		  [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "Choose whether to approve this entity's subscription."))}]},
		 #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		  [?XMLATTR('var', <<"FORM_TYPE">>), ?XMLATTR('type', <<"hidden">>)], children =
		  [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(?NS_PUBSUB_SUBSCRIBE_AUTH_s)}]}]},
		 #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		  [?XMLATTR('var', <<"pubsub#node">>), ?XMLATTR('type', <<"text-single">>),
		   ?XMLATTR('label', translate:translate(Lang, "Node ID"))], children =
		  [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
		    [#xmlcdata{cdata = node_to_string(Node)}]}]},
		 #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR('var', <<"pubsub#subscriber_jid">>),
					?XMLATTR('type', <<"jid-single">>),
					?XMLATTR('label', translate:translate(Lang, "Subscriber Address"))], children =
		  [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
		    [#xmlcdata{cdata = exmpp_jid:to_binary(U, S, R)}]}]},
		 #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		  [?XMLATTR('var', <<"pubsub#allow">>),
		   ?XMLATTR('type', <<"boolean">>),
		   ?XMLATTR('label', translate:translate(Lang, "Allow this Jabber ID to subscribe to this pubsub node?"))], children =
		  [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"false">>}]}]}]}]},
    lists:foreach(fun(Owner) ->
    	{U, S, R} = Owner,
	ejabberd_router ! {route, service_jid(Host), exmpp_jid:make(U, S, R), Stanza}
    end, Owners).

find_authorization_response(Packet) ->
    Els = Packet#xmlel.children,
    XData1 = lists:map(fun(#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = XAttrs} = XEl) ->
			       case exmpp_xml:get_attribute_from_list_as_list(XAttrs, 'type', "") of
				   "cancel" ->
				       none;
				   _ ->
				       jlib:parse_xdata_submit(XEl)
			       end;
			  (_) ->
			       none
		       end, exmpp_xml:remove_cdata(Els)),
    XData = lists:filter(fun(E) -> E /= none end, XData1),
    case XData of
	[invalid] -> invalid;
	[] -> none;
	[XFields] when is_list(XFields) ->
	    case lists:keysearch("FORM_TYPE", 1, XFields) of
		{value, {_, [?NS_PUBSUB_SUBSCRIBE_AUTH_s]}} ->
		    XFields;
		_ ->
		    invalid
	    end
    end.

%% @spec (Host, JID, Node, Subscription) -> void
%%     Host = mod_pubsub:host()
%%     JID = jlib:jid()
%%     SNode = string()
%%     Subscription = atom() | {atom(), mod_pubsub:subid()}
%%     Plugins = [Plugin::string()]
%% @doc Send a message to JID with the supplied Subscription
send_authorization_approval(Host, JID, SNode, Subscription) ->
    SubAttrs = case Subscription of
		   {S, SID} -> [?XMLATTR('subscription', subscription_to_string(S)),
				?XMLATTR('subid', SID)];
		   S	-> [?XMLATTR('subscription', subscription_to_string(S))]
	       end,
    Stanza = event_stanza(
		[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'subscription', attrs =
		    [ ?XMLATTR('jid', exmpp_jid:to_binary(JID)) | nodeAttr(SNode)] ++ SubAttrs
		     }]),
    ejabberd_router ! {route, service_jid(Host), JID, Stanza}.
 
handle_authorization_response(Host, From, To, Packet, XFields) ->
    case {lists:keysearch("pubsub#node", 1, XFields),
	  lists:keysearch("pubsub#subscriber_jid", 1, XFields),
	  lists:keysearch("pubsub#allow", 1, XFields)} of
	{{value, {_, [SNode]}}, {value, {_, [SSubscriber]}},
	 {value, {_, [SAllow]}}} ->
	    Node = string_to_node(SNode),
	    Subscriber = exmpp_jid:parse(SSubscriber),
	    Allow = case SAllow of
			"1" -> true;
			"true" -> true;
			_ -> false
		    end,
	    Action = fun(#pubsub_node{type = Type, owners = Owners, id = NodeId}) ->
			     IsApprover = lists:member(jlib:short_prepd_bare_jid(From), Owners),
			     {result, Subscriptions} = node_call(Type, get_subscriptions, [NodeId, Subscriber]),
			     if
				 not IsApprover ->
				     {error, 'forbidden'};
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
		     exmpp_stanza:reply_with_error(Packet, Error));
		{result, _} ->
		    %% XXX: notify about subscription state change, section 12.11
		    ok;
		_ ->
		    ejabberd_router:route(
		      To, From,
		      exmpp_stanza:reply_with_error(Packet, 'internal-server-error'))
	    end;
	_ ->
	    ejabberd_router:route(
	      To, From,
	      exmpp_stanza:reply_with_error(Packet, 'not-acceptable'))
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
	    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'unexpected-request')}
    end.

-define(XFIELD(Type, Label, Var, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR('type', Type),
			       ?XMLATTR('label', translate:translate(Lang, Label)),
			       ?XMLATTR('var', Var)], children =
	 [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Val)}]}]}).

-define(BOOLXFIELD(Label, Var, Val),
	?XFIELD("boolean", Label, Var,
		case Val of
		    true -> "1";
		    _ -> "0"
		end)).

-define(STRINGXFIELD(Label, Var, Val),
	?XFIELD("text-single", Label, Var, Val)).

-define(STRINGMXFIELD(Label, Var, Vals),
	#xmlel{ns = ?NS_DATA_FORMS, 
	       name = 'field',
	       attrs = [?XMLATTR('type', <<"text-multi">>),
	       		?XMLATTR('label', translate:translate(Lang, Label)),
			?XMLATTR('var', Var)
			],
		children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value',
  				children = [?XMLCDATA(V)]}  || V <- Vals]}).  

-define(XFIELDOPT(Type, Label, Var, Val, Opts),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR('type', Type),
			       ?XMLATTR('label', translate:translate(Lang, Label)),
			       ?XMLATTR('var', Var)], children =
	 lists:map(fun(Opt) ->
			   #xmlel{ns = ?NS_DATA_FORMS, name = 'option', children =
			    [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
			      [#xmlcdata{cdata = list_to_binary(Opt)}]}]}
		   end, Opts) ++
	 [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Val)}]}]}).

-define(LISTXFIELD(Label, Var, Val, Opts),
	?XFIELDOPT("list-single", Label, Var, Val, Opts)).

-define(LISTMXFIELD(Label, Var, Vals, Opts),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR('type', <<"list-multi">>),
			       ?XMLATTR('label', translate:translate(Lang, Label)),
			       ?XMLATTR('var', Var)], children =
	 lists:map(fun(Opt) ->
			   #xmlel{ns = ?NS_DATA_FORMS, name = 'option', children =
			    [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
			      [#xmlcdata{cdata = list_to_binary(Opt)}]}]}
		   end, Opts) ++
	 lists:map(fun(Val) ->
			    #xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = 
			     [#xmlcdata{cdata = list_to_binary(Val)}]}
		   end, Vals)
	}).

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
		     [#xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
		       [#xmlel{ns = ?NS_PUBSUB, name = 'create', attrs = nodeAttr(NewNode)}]}]};
		Error -> 
            Error
	    end;
	false ->
	    %% Service does not support instant nodes
	    {error, extended_error('not-acceptable', "nodeid-required")}
    end;
create_node(Host, ServerHost, Node, Owner, GivenType, Access, Configuration) ->
    Type = select_type(ServerHost, Host, Node, GivenType),
    %% TODO, check/set node_type = Type
    ParseOptions = case exmpp_xml:remove_cdata_from_list(Configuration) of
		       [] ->
			   {result, node_options(Type)};
		       [#xmlel{name = 'x'} = XEl] ->
			   case jlib:parse_xdata_submit(XEl) of
			       invalid ->
				   {error, 'bad-request'};
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
			   {error, 'bad-request'}
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
					node_call(Type, create_node, [NodeId, Owner]);
				    {error, {virtual, NodeId}} ->
					node_call(Type, create_node, [NodeId, Owner]);
				    Error ->
					Error
				end;
			    _ ->
				{error, 'forbidden'}
			end
		end,
	    Reply = #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
		      [#xmlel{ns = ?NS_PUBSUB, name = 'create', attrs = nodeAttr(Node)}]},
	    case transaction(CreateNode, transaction) of
		{result, {Result, broadcast}} ->
		    %%Lang = "en", %% TODO: fix
		    %%OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
		    %%broadcast_publish_item(Host, Node, uniqid(), Owner,
		    %%	[{xmlelement, "x", [{"xmlns", ?NS_DATA_FORMS}, {"type", "result"}],
		    %%		[?XFIELD("hidden", "", "FORM_TYPE", ?NS_PUBSUB_NMI),
		    %%		?XFIELD("jid-single", "Node Creator", "creator", jlib:jid_to_string(OwnerKey))]}]),
		    case Result of
			default -> {result, Reply};
			_ -> {result, Result}
		    end;
		{result, default} ->
		    {result, Reply};
		{result, Result} ->
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
delete_node(_Host, [], _Owner) ->
    %% Node is the root
    {error, 'not-allowed'};
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
			    {error, 'forbidden'}
		    end
	     end,
    Reply = [],
    case transaction(Host, Node, Action, transaction) of
	{result, {_, {SubsByDepth, {Result, broadcast, Removed}}}} ->
	    lists:foreach(fun({RNode, _RSubscriptions}) ->
		{RH, RN} = RNode#pubsub_node.nodeid,
		NodeId = RNode#pubsub_node.id,
		Type = RNode#pubsub_node.type,
		Options = RNode#pubsub_node.options,
		broadcast_removed_node(RH, RN, NodeId, Type, Options, SubsByDepth),
		unset_cached_item(RH, NodeId)
	    end, Removed),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {_, {_, {Result, _Removed}}}} ->
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {_, {_, default}}} ->
	    {result, Reply};
	{result, {_, {_, Result}}} ->
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
    SubOpts = case pubsub_subscription:parse_options_xform(Configuration) of
	{result, GoodSubOpts} -> GoodSubOpts;
	_ -> invalid
    end,
    Subscriber = try
	jlib:short_prepd_jid(exmpp_jid:parse(JID))
    catch
	_:_ ->
	    {undefined, undefined, undefined}
    end,
    SubId = uniqid(),
    Action = fun(#pubsub_node{options = Options, owners = [Owner|_], type = Type, id = NodeId}) ->
		    Features = features(Type),
		    SubscribeFeature = lists:member("subscribe", Features),
		    OptionsFeature = lists:member("subscription-options", Features),
		    HasOptions = not (SubOpts == []),
		    SubscribeConfig = get_option(Options, subscribe),
		    AccessModel = get_option(Options, access_model),
		    SendLast = get_option(Options, send_last_published_item),
		    AllowedGroups = get_option(Options, roster_groups_allowed, []),
		    {PresenceSubscription, RosterGroup} =
			case Host of
			    {OUser, OServer, _} ->
				get_roster_info(OUser, OServer,
						Subscriber, AllowedGroups);
			    _ ->
				case Subscriber of
				    {"", "", ""} ->
					{false, false};
				    _ ->
					{OU, OS, _} = Owner,
					get_roster_info(OU, OS,
							Subscriber, AllowedGroups)
				end
			end,
		    if
			not SubscribeFeature ->
			    %% Node does not support subscriptions
			    {error, extended_error('feature-not-implemented', unsupported, "subscribe")};
			not SubscribeConfig ->
			    %% Node does not support subscriptions
			    {error, extended_error('feature-not-implemented', unsupported, "subscribe")};
			HasOptions andalso not OptionsFeature ->
			    %% Node does not support subscription options
			    {error, extended_error('feature-not-implemented', unsupported, "subscription-options")};
			SubOpts == invalid ->
			    %% Passed invalit options submit form
			    {error, extended_error('bad-request', "invalid-options")};
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
					{"subid", SubId}];
				   Other ->
				       [{"subscription", subscription_to_string(Other)}]
			       end,
		    Fields =
			[ ?XMLATTR('jid', exmpp_jid:to_binary(Subscriber)) | SubAttrs],
		    #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
			[#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs = Fields}]}
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
    Subscriber = try jlib:short_prepd_jid(exmpp_jid:parse(JID))
    catch
	_:_ ->
	    {undefined, undefined, undefined}
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
		    PayloadSize = size(term_to_binary(Payload)),
		    PayloadMaxSize = get_option(Options, max_payload_size),
		    % pubsub#deliver_payloads true 
		    % pubsub#persist_items true -> 1 item; false -> 0 item
		    if
			not PublishFeature ->
			    %% Node does not support item publication
			    {error, extended_error('feature-not-implemented', unsupported, "publish")};
			PayloadSize > PayloadMaxSize ->
			    %% Entity attempts to publish very large payload
			    {error, extended_error('not-acceptable', "payload-too-big")};
			(PayloadCount == 0) and (Payload == []) ->
			    %% Publisher attempts to publish to payload node with no payload
			    {error, extended_error('bad-request', "payload-required")};
			(PayloadCount > 1) or (PayloadCount == 0) ->
			    %% Entity attempts to publish item with multiple payload elements
			    {error, extended_error('bad-request', "invalid-payload")};
			(DeliverPayloads == 0) and (PersistItems == 0) and (PayloadSize > 0) ->
			    %% Publisher attempts to publish to transient notification node with item
			    {error, extended_error('bad-request', "item-forbidden")};
			((DeliverPayloads == 1) or (PersistItems == 1)) and (PayloadSize == 0) ->
			    %% Publisher attempts to publish to persistent node with no item
			    {error, extended_error('bad-request', "item-required")};
			true ->
			    node_call(Type, publish_item, [NodeId, Publisher, PublishModel, MaxItems, ItemId, Payload])
		    end
	    end,
    ServerHostB = list_to_binary(ServerHost),
    ejabberd_hooks:run(pubsub_publish_item, ServerHostB, [ServerHost, Node, Publisher, service_jid(Host), ItemId, Payload]),
    Reply = #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
		[#xmlel{ns = ?NS_PUBSUB, name = 'publish', attrs = nodeAttr(Node), children =
		    [#xmlel{ns = ?NS_PUBSUB, name = 'item', attrs = itemAttr(ItemId)}]}]},
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, Broadcast, Removed}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    BroadcastPayload = case Broadcast of
		default -> Payload;
		broadcast -> Payload;
		PluginPayload -> PluginPayload
	    end,
	    broadcast_publish_item(Host, Node, NodeId, Type, Options, Removed, ItemId, jlib:short_prepd_jid(Publisher), BroadcastPayload),
	    set_cached_item(Host, NodeId, ItemId, Payload),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {TNode, {default, Removed}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, NodeId, Type, Options, Removed),
	    set_cached_item(Host, NodeId, ItemId, Payload),
	    {result, Reply};
	{result, {TNode, {Result, Removed}}} ->
	    NodeId = TNode#pubsub_node.id,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, NodeId, Type, Options, Removed),
	    set_cached_item(Host, NodeId, ItemId, Payload),
	    {result, Result};
	{result, {_, default}} ->
	    {result, Reply};
	{result, {_, Result}} ->
	    {result, Result};
	{error, 'item-not-found'} ->
	    %% handles auto-create feature
	    %% for automatic node creation. we'll take the default node type:
	    %% first listed into the plugins configuration option, or pep
	    Type = select_type(ServerHost, Host, Node),
	    case lists:member("auto-create", features(Type)) of
		true ->
		    case create_node(Host, ServerHost, Node, Publisher, Type) of
			{result, _} ->
			    publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload);
			_ ->
			    {error, 'item-not-found'}
		    end;
		false ->
		    {error, 'item-not-found'}
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
    {error, extended_error('bad-request', "node-required")};
delete_item(Host, Node, Publisher, ItemId, ForceNotify) ->
    Action = fun(#pubsub_node{options = Options, type = Type, id = NodeId}) ->
		    Features = features(Type),
		    PersistentFeature = lists:member("persistent-items", Features),
		    DeleteFeature = lists:member("delete-items", Features),
		    PublishModel = get_option(Options, publish_model),
		    if
			%%->   iq_pubsub just does that matchs
			%%	%% Request does not specify an item
			%%	{error, extended_error('bad-request', "item-required")};
			not PersistentFeature ->
			    %% Node does not support persistent items
			    {error, extended_error('feature-not-implemented', unsupported, "persistent-items")};
			not DeleteFeature ->
			    %% Service does not support item deletion
			    {error, extended_error('feature-not-implemented', unsupported, "delete-items")};
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
			     {error, extended_error('feature-not-implemented', unsupported, "purge-nodes")};
			 not PersistentFeature ->
			     %% Node does not support persistent items
			     {error, extended_error('feature-not-implemented', unsupported, "persistent-items")};
			 not PersistentConfig ->
			     %% Node is not configured for persistent items
			     {error, extended_error('feature-not-implemented', unsupported, "persistent-items")};
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
get_items(Host, Node, From, SubId, SMaxItems, ItemIDs) ->
    MaxItems =
	if
	    SMaxItems == "" -> get_max_items_node(Host);
	    true ->
		case catch list_to_integer(SMaxItems) of
		    {'EXIT', _} -> {error, 'bad-request'};
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
		     {PresenceSubscription, RosterGroup} =
			 case Host of
			     {OUser, OServer, _} ->
				 get_roster_info(OUser, OServer,
						 jlib:short_prepd_jid(From), AllowedGroups);
			     _ ->
				 {true, true}
			 end,
		     if
			 not RetreiveFeature ->
			     %% Item Retrieval Not Supported
			     {error, extended_error('feature-not-implemented', unsupported, "retrieve-items")};
			 not PersistentFeature ->
			     %% Persistent Items Not Supported
			     {error, extended_error('feature-not-implemented', unsupported, "persistent-items")};
			 true ->
			     node_call(Type, get_items,
				       [NodeId, From,
					AccessModel, PresenceSubscription, RosterGroup,
					SubId])
		     end
	     end,
	     case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Items}} ->
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
		    {result, #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
				[#xmlel{ns = ?NS_PUBSUB, name = 'items', attrs = nodeAttr(Node), children =
				    itemsEls(lists:sublist(SendItems, MaxItems))}]}};
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
    case get_cached_item(Host, NodeId) of
	undefined ->
	    send_items(Host, Node, NodeId, Type, LJID, 1);
	LastItem ->
	    {ModifNow, ModifLjid} = LastItem#pubsub_item.modification,
	    Stanza = event_stanza_with_delay(
	    	[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node),
			children = itemsEls(LastItem)}], ModifNow, ModifLjid),
	    {U, S, R} = LJID,
	    ejabberd_router ! {route, service_jid(Host), exmpp_jid:make(U, S, R), Stanza}
    end;
send_items(Host, Node, NodeId, Type, {LU, LS, LR} = LJID, Number) ->
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
	    {ModifNow, ModifLjid} = LastItem#pubsub_item.modification,
	    event_stanza_with_delay(
		[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node), children =
		  itemsEls(ToSend)}], ModifNow, ModifLjid);
	_ ->
	    event_stanza(
		[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node), children =
		  itemsEls(ToSend)}])
    end,
    ejabberd_router ! {route, service_jid(Host), exmpp_jid:make(LU, LS, LR), Stanza}.

%% @spec (Host, JID, Plugins) -> {error, Reason} | {result, Response}
%%	 Host = host()
%%	 JID = jid()
%%	 Plugins = [Plugin::string()]
%%	 Reason = stanzaError()
%%	 Response = [pubsubIQResponse()]
%% @doc <p>Return the list of affiliations as an XMPP response.</p>
get_affiliations(Host, JID, Plugins) when is_list(Plugins) ->
    Result = lists:foldl(
	       fun(Type, {Status, Acc}) ->
		       Features = features(Type),
		       RetrieveFeature = lists:member("retrieve-affiliations", Features),
		       if
			   not RetrieveFeature ->
			       %% Service does not support retreive affiliatons
			       {{error, extended_error('feature-not-implemented', unsupported, "retrieve-affiliations")}, Acc};
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
				 [#xmlel{ns = ?NS_PUBSUB, name = 'affiliation', attrs =
				   [?XMLATTR('node', node_to_string(Node)),
				    ?XMLATTR('affiliation', affiliation_to_string(Affiliation))]}]
			 end, lists:usort(lists:flatten(Affiliations))),
	    {result, #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
		       [#xmlel{ns = ?NS_PUBSUB, name = 'affiliations', children =
			 Entities}]}};
	{Error, _} ->
	    Error
    end;
get_affiliations(Host, Node, JID) ->
    Action = fun(#pubsub_node{type = Type, id = NodeId}) ->
		    Features = features(Type),
		    RetrieveFeature = lists:member("modify-affiliations", Features),
		    {result, Affiliation} = node_call(Type, get_affiliation, [NodeId, JID]),
		    if
			not RetrieveFeature ->
			    %% Service does not support modify affiliations
			    {error, extended_error('feature-not-implemented', unsupported, "modify-affiliations")};
			Affiliation /= owner ->
			    %% Entity is not an owner
			    {error, 'forbidden'};
			true ->
			    node_call(Type, get_node_affiliations, [NodeId])
		    end
	    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, []}} ->
	    {error, 'item-not-found'};
	{result, {_, Affiliations}} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({{AU, AS, AR}, Affiliation}) ->
				 [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'affiliation', attrs =
				   [?XMLATTR('jid', exmpp_jid:to_binary(AU, AS, AR)),
				    ?XMLATTR('affiliation', affiliation_to_string(Affiliation))]}]
			 end, Affiliations),
	    {result, #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'pubsub', children =
		       [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'affiliations', attrs = nodeAttr(Node), children =
			 Entities}]}};
	Error ->
	    Error
    end.

set_affiliations(Host, Node, From, EntitiesEls) ->
    Owner = jlib:short_prepd_bare_jid(From),
    Entities =
	lists:foldl(
	  fun(El, Acc) ->
		  case Acc of
		      error ->
			  error;
		      _ ->
			  case El of
			      #xmlel{name = 'affiliation', attrs = Attrs} ->
				  JID = try
				      exmpp_jid:parse(
					exmpp_xml:get_attribute_from_list(Attrs, 'jid', ""))
				  catch
				      _:_ -> error
				  end,
				  Affiliation = string_to_affiliation(
						  exmpp_xml:get_attribute_from_list_as_list(Attrs, 'affiliation', false)),
				  if
				      (JID == error) or
				      (Affiliation == false) ->
					  error;
				      true ->
					  [{JID, Affiliation} | Acc]
				  end
			  end
		  end
	  end, [], EntitiesEls),
    case Entities of
	error ->
	    {error, 'bad-request'};
	_ ->
	    Action = fun(#pubsub_node{owners = Owners, type = Type, id = NodeId}=N) ->
			case lists:member(Owner, Owners) of
			    true ->
				lists:foreach(
				    fun({JID, Affiliation}) ->
					{result, _} = node_call(Type, set_affiliation, [NodeId, JID, Affiliation]),
					case Affiliation of
					    owner ->
						NewOwner = jlib:short_prepd_bare_jid(JID),
						NewOwners = [NewOwner|Owners],
						tree_call(Host, set_node, [N#pubsub_node{owners = NewOwners}]);
					    none ->
						OldOwner = jlib:short_prepd_bare_jid(JID),
						case lists:member(OldOwner, Owners) of
						    true ->
							NewOwners = Owners--[OldOwner],
							tree_call(Host, set_node, [N#pubsub_node{owners = NewOwners}]);
						    _ ->
							ok
						end;
					    _ ->
						ok
					end
				    end, Entities),
				    {result, []};
				_ ->
				    {error, 'forbidden'}
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
					'feature-not-implemented',
					unsupported, "subscription-options")}
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, XForm}} -> {result, [XForm]};
	Error		    -> Error
    end.

get_options_helper(JID, Lang, Node, NodeID, SubID, Type) ->
    Subscriber = try exmpp_jid:parse(JID) of
    		    J -> jlib:short_jid(J)
		  catch
		      _ ->
		         {"", "", ""} %%pablo TODO: "" or <<>> ?. short_jid uses exmpp_jid:node/1, etc. that returns binaries
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
	    {error, extended_error('not-acceptable', "not-subscribed")};
	{[], [SID]} ->
	    read_sub(Subscriber, Node, NodeID, SID, Lang);
	{[], _} ->
	    {error, extended_error('not-acceptable', "subid-required")};
	{_, _} ->
	    read_sub(Subscriber, Node, NodeID, SubID, Lang)
    end.

read_sub(Subscriber, Node, NodeID, SubID, Lang) ->
    case pubsub_subscription:get_subscription(Subscriber, NodeID, SubID) of
	{error, notfound} ->
	    {error, extended_error('not-acceptable', "invalid-subid")};
	{result, #pubsub_subscription{options = Options}} ->
            {result, XdataEl} = pubsub_subscription:get_options_xform(Lang, Options),
            OptionsEl = #xmlel{ns = ?NS_PUBSUB, name = 'options',
			       attrs = [ ?XMLATTR('jid', exmpp_jid:to_binary(Subscriber)),
					?XMLATTR('Subid', SubID) | nodeAttr(Node)],
			       children = [XdataEl]},
            PubsubEl = #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children = [OptionsEl]},
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
					'feature-not-implemented',
					unsupported, "subscription-options")}
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, Result}} -> {result, Result};
	Error		     -> Error
    end.

set_options_helper(Configuration, JID, NodeID, SubID, Type) ->
    SubOpts = case pubsub_subscription:parse_options_xform(Configuration) of
	{result, GoodSubOpts} -> GoodSubOpts;
	_ -> invalid
    end,
    Subscriber = try exmpp_jid:parse(JID) of
		     J -> jlib:short_jid(J)
		  catch
		     _ -> {"", "", ""} %%pablo TODO: "" or <<>> ?. short_jid uses exmpp_jid:node/1, etc. that returns binaries
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
	    {error, extended_error('not-acceptable', "not-subscribed")};
	{[], [SID]} ->
	    write_sub(Subscriber, NodeID, SID, SubOpts);
	{[], _} ->
	    {error, extended_error('not-acceptable', "subid-required")};
	{_, _} ->
	    write_sub(Subscriber, NodeID, SubID, SubOpts)
    end.

write_sub(_Subscriber, _NodeID, _SubID, invalid) ->
    {error, extended_error('bad-request', "invalid-options")};
write_sub(Subscriber, NodeID, SubID, Options) ->
    case pubsub_subscription:set_subscription(Subscriber, NodeID, SubID, Options) of
	{error, notfound} ->
	    {error, extended_error('not-acceptable', "invalid-subid")};
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
			       {{error, extended_error('feature-not-implemented', unsupported, "retrieve-subscriptions")}, Acc};
			   true ->
			       Subscriber = exmpp_jid:bare(JID),
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
				 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs =
				   [?XMLATTR('node', node_to_string(SubsNode)),
				    ?XMLATTR('subscription', subscription_to_string(Subscription))]}];
				SubsNode ->
				 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs =
				   [?XMLATTR('subscription', subscription_to_string(Subscription))]}];
				_ ->
				 []
				end;
			    ({_, none, _}) ->
				[];
			    ({#pubsub_node{nodeid = {_, SubsNode}}, Subscription, SubID, SubJID}) ->
				case Node of
				<<>> ->
				 [#xmlel{ns = ?NS_PUBSUB, name='subscription',
				 	 attrs = [?XMLATTR('jid', exmpp_jid:jid_to_binary(SubJID)),
					 	 ?XMLATTR('subid', SubID),
						 ?XMLATTR('subscription', subscription_to_string(Subscription)) | nodeAttr(SubsNode)]}];
				SubsNode ->
				 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', 
				 	 attrs = [?XMLATTR('jid', exmpp_jid:jid_to_binary(SubJID)),
					 	  ?XMLATTR('subid', SubID),
						  ?XMLATTR('subscription', subscription_to_string(Subscription))]}];
				_ ->
				 []
				end;
			    ({#pubsub_node{nodeid = {_, SubsNode}}, Subscription, SubJID}) ->
				case Node of
				<<>> ->
				 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs =
				   [?XMLATTR('node', node_to_string(SubsNode)),
				    ?XMLATTR('jid', exmpp_jid:to_binary(SubJID)),
				    ?XMLATTR('subscription', subscription_to_string(Subscription))]}];
				SubsNode ->
				 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs =
				   [?XMLATTR('jid', exmpp_jid:to_binary(SubJID)),
				    ?XMLATTR('subscription', subscription_to_string(Subscription))]}];
				_ ->
				 []
				end
			 end, lists:usort(lists:flatten(Subscriptions))),
	    {result, #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
		       [#xmlel{ns = ?NS_PUBSUB, name = 'subscriptions', children =
			 Entities}]}};
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
			    {error, extended_error('feature-not-implemented', unsupported, "manage-subscriptions")};
			Affiliation /= owner ->
			    %% Entity is not an owner
			    {error, 'forbidden'};
			true ->
			    node_call(Type, get_node_subscriptions, [NodeId])
		    end
	    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, []}} ->
	    {error, 'item-not-found'};
	{result, {_, Subscriptions}} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({_, pending, _}) -> [];
			    ({{AU, AS, AR}, Subscription}) ->
				 [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'subscription', attrs =
				   [?XMLATTR('jid', exmpp_jid:to_binary(AU, AS, AR)),
				    ?XMLATTR('subscription', subscription_to_string(Subscription))]}];
			    ({{AU, AS, AR}, Subscription, SubId}) ->
				 [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'subscription', attrs =
				   [?XMLATTR('jid', exmpp_jid:to_binary(AU, AS, AR)),
				    ?XMLATTR('subscription', subscription_to_string(Subscription)),
				    ?XMLATTR('subid', SubId)]}]
			 end, Subscriptions),
	    {result, #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'pubsub', children =
		       [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'subscriptions', attrs = nodeAttr(Node), children =
			 Entities}]}};
	Error ->
	    Error
    end.

set_subscriptions(Host, Node, From, EntitiesEls) ->
    Owner = jlib:short_prepd_bare_jid(From),
    Entities =
	lists:foldl(
	  fun(El, Acc) ->
		  case Acc of
		      error ->
			  error;
		      _ ->
			  case El of
			      #xmlel{name = 'subscription', attrs = Attrs} ->
				  JID = try
				      exmpp_jid:parse(
					exmpp_xml:get_attribute_from_list(Attrs, 'jid', ""))
				  catch
				      _:_ ->
					  error
				  end,
				  Subscription = string_to_subscription(
						   exmpp_xml:get_attribute_from_list_as_list(Attrs, 'subscription', false)),
				  SubId = exmpp_xml:get_attribute_from_list_as_list(Attrs, "subid", false),
				  if
				      (JID == error) or
				      (Subscription == false) ->
					  error;
				      true ->
					  [{JID, Subscription, SubId} | Acc] 
				  end
			  end
		  end
	  end, [], EntitiesEls),
    case Entities of
	error ->
	    {error, 'bad-request'};
	_ ->
	    Notify = fun(JID, Sub, _SubId) ->
		Stanza = #xmlel{ns = ?NS_JABBER_CLIENT, 
			name = 'message',
			children = 
			 [#xmlel{ns = ?NS_PUBSUB, 
				name = 'pubsub',
				children = 
				 [#xmlel{ns = ?NS_PUBSUB,
					name = 'subscription',
					attrs = [?XMLATTR('jid', exmpp_jid:to_binary(JID)),
						 ?XMLATTR('subsription', subscription_to_string(Sub)) | nodeAttr(Node)]}]}]},
		ejabberd_router ! {route, service_jid(Host), JID, Stanza}
	    end,
	    Action = fun(#pubsub_node{owners = Owners, type = Type, id = NodeId}) ->
			    case lists:member(Owner, Owners) of
				true ->
				    Result = lists:foldl(fun({JID, Subscription, SubId}, Acc) ->

						    case node_call(Type, set_subscriptions, [NodeId, JID, Subscription, SubId]) of
							{error, Err} -> [{error, Err} | Acc];
							_ -> Notify(JID, Subscription, SubId), Acc
						    end
						end, [], Entities),
				    case Result of
					[] -> {result, []};
					_ -> {error, 'not-acceptable'}
				    end;
				_ ->
				    {error, 'forbidden'}
			    end
		    end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Result}} -> {result, Result};
		Other -> Other
	    end
    end.


%% @spec (OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, SubscriberResource}, AllowedGroups)
%%    -> {PresenceSubscription, RosterGroup}
get_roster_info(OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, _}, AllowedGroups) ->
     OwnerServerB = list_to_binary(OwnerServer),
    {Subscription, Groups} =
	ejabberd_hooks:run_fold(
	  roster_get_jid_info, OwnerServerB,
	  {none, []},
	  [OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, undefined}]),
    PresenceSubscription = (Subscription == both) orelse (Subscription == from)
			    orelse ({OwnerUser, OwnerServer} == {SubscriberUser, SubscriberServer}),
    RosterGroup = lists:any(fun(Group) ->
				    lists:member(Group, AllowedGroups)
			    end, Groups),
    {PresenceSubscription, RosterGroup}.

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
    {U,S,_} -> exmpp_jid:make(U, S);
    _ -> exmpp_jid:make(Host)
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
presence_can_deliver({User, Server, _}, true) ->
    case mnesia:dirty_match_object({session, '_', '_', {User, Server}, '_', '_'}) of
    [] -> false;
    Ss ->
	lists:foldl(fun({session, _, _, _, undefined, _}, Acc) -> Acc;
		       ({session, _, _, _, _Priority, _}, _Acc) -> true
	end, false, Ss)
    end.

%% @spec (Payload) -> int()
%%	Payload = term()
%% @doc <p>Count occurence of XML elements in payload.</p>
payload_xmlelements(Payload) -> payload_xmlelements(Payload, 0).
payload_xmlelements([], Count) -> Count;
payload_xmlelements([#xmlel{}|Tail], Count) -> payload_xmlelements(Tail, Count+1);
payload_xmlelements([_|Tail], Count) -> payload_xmlelements(Tail, Count).

%% @spec (Els) -> stanza()
%%    Els = [xmlelement()]
%% @doc <p>Build pubsub event stanza</p>
event_stanza(Els) ->
    event_stanza_withmoreels(Els, []).


event_stanza_with_delay(Els, ModifNow, ModifLjid) ->
    DateTime = calendar:now_to_datetime(ModifNow),
    MoreEls = [jlib:timestamp_to_xml(DateTime, utc, ModifLjid, "")],
    event_stanza_withmoreels(Els, MoreEls).

event_stanza_withmoreels(Els, MoreEls) ->
    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message', children =
	[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'event', children = Els} | MoreEls]}.

%%%%%% broadcast functions

broadcast_publish_item(Host, Node, NodeId, Type, Options, Removed, ItemId, _From, Payload) ->
    %broadcast(Host, Node, NodeId, Options, none, true, 'items', ItemEls)
    case get_collection_subscriptions(Host, Node) of
        [] ->	
	    {result, false};

	SubsByDepth when is_list(SubsByDepth) -> 
	    Content = case get_option(Options, deliver_payloads) of
		true -> Payload;
		false -> []
	    end,
	    Stanza = event_stanza(
		[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node), children =
		[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'item', attrs = itemAttr(ItemId), children = Content}]}]),
	    broadcast_stanza(Host, Node, NodeId, Type, Options, SubsByDepth, items, Stanza),
	    case Removed of
		[] ->
		    ok;
		_ ->
		    case get_option(Options, notify_retract) of
			true ->
			    RetractStanza = event_stanza(
				[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node), children = 
				[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'retract', attrs = itemAttr(RId)} || RId <- Removed]}]),
			    broadcast_stanza(Host, Node, NodeId, Type, Options, SubsByDepth, items, RetractStanza);
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
    %broadcast(Host, Node, NodeId, NodeOptions, notify_retract, ForceNotify, 'retract', RetractEls)
    case (get_option(NodeOptions, notify_retract) or ForceNotify) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		 [] -> 
		    {result, false};

		SubsByDepth when is_list(SubsByDepth)->
		    Stanza = event_stanza(
			[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node), children = 
			[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'retract', attrs = itemAttr(ItemId)} || ItemId <- ItemIds]}]),
		    broadcast_stanza(Host, Node, NodeId, Type, NodeOptions, SubsByDepth, items, Stanza),
		    {result, true};
		_ ->
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_purge_node(Host, Node, NodeId, Type, NodeOptions) ->
    %broadcast(Host, Node, NodeId, NodeOptions, notify_retract, false, 'purge', [])
    case get_option(NodeOptions, notify_retract) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		[] -> 
		    {result, false};
		SubsByDepth when is_list(SubsByDepth) ->
		    Stanza = event_stanza(
			[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'purge', attrs = nodeAttr(Node)}]),
		    broadcast_stanza(Host, Node, NodeId, Type, NodeOptions, SubsByDepth, nodes, Stanza),
		    {result, true};
		_ -> 
		    {result, false}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_removed_node(Host, Node, NodeId, Type, NodeOptions, SubsByDepth) ->
    %broadcast(Host, Node, NodeId, NodeOptions, notify_delete, false, 'delete', [])
    case get_option(NodeOptions, notify_delete) of
	true ->
	    case SubsByDepth of
		[] -> 
		    {result, false};
		_ ->
		    Stanza = event_stanza(
			[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'delete', attrs = nodeAttr(Node)}]),
		    broadcast_stanza(Host, Node, NodeId, Type, NodeOptions, SubsByDepth, nodes, Stanza),
		    {result, true}
	    end;
	_ ->
	    {result, false}
    end.

broadcast_config_notification(Host, Node, NodeId, Type, NodeOptions, Lang) ->
    %broadcast(Host, Node, NodeId, NodeOptions, notify_config, false, 'items', ConfigEls)
    case get_option(NodeOptions, notify_config) of
	true ->
	    case get_collection_subscriptions(Host, Node) of
		[] -> 
		    {result, false};
		SubsByDepth when is_list(SubsByDepth) ->
		    Content = case get_option(NodeOptions, deliver_payloads) of
			true ->
			    [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = [?XMLATTR('type', <<"form">>)], children =
				get_configure_xfields(Type, NodeOptions, Lang, [])}];
			false ->
			    []
		    end,
		    Stanza = event_stanza(
			[#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node), children =
			    [#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'item', attrs = [?XMLATTR('id', <<"configuration">>)], children =
			    Content}]}]),
		    broadcast_stanza(Host, Node, NodeId, Type, NodeOptions, SubsByDepth, nodes, Stanza),
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
    case transaction(Action, sync_dirty) of
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
			case pubsub_subscription:read_subscription(JID, NodeID, SubID) of
			    {error, notfound} -> [{JID, SubID, []} | Acc];
			    #pubsub_subscription{options = Options} -> [{JID, SubID, Options} | Acc];
			    _ -> Acc
			end;
		    (_, Acc) ->
			Acc
		end, [], Subs).

% TODO: merge broadcast code that way
% TODO: pablo: Why is this commented? Seems to be present on trunk.
%broadcast(Host, Node, NodeId, Type, NodeOptions, Feature, Force, ElName, SubEls) ->
%    case (get_option(NodeOptions, Feature) or Force) of
%	true ->
%	    case node_action(Host, Type, get_node_subscriptions, [NodeId]) of
%		{result, []} -> 
%		    {result, false};
%		{result, Subs} ->
%		    Stanza = event_stanza([{xmlelement, ElName, nodeAttr(Node), SubEls}]),
%		    broadcast_stanza(Host, Node, Type, NodeOptions, SubOpts, Stanza),
%		    {result, true};
%		_ ->
%		    {result, false}
%	    end;
%	_ ->
%	    {result, false}
%    end

broadcast_stanza(Host, Node, _NodeId, _Type, NodeOptions, SubsByDepth, NotifyType, BaseStanza) ->
    NotificationType = get_option(NodeOptions, notification_type, headline),
    BroadcastAll = get_option(NodeOptions, broadcast_all_resources), %% XXX this is not standard, but usefull
    From = service_jid(Host),
    Stanza = case NotificationType of
	normal -> BaseStanza;
	MsgType -> add_message_type(BaseStanza, atom_to_list(MsgType))
	end,
    %% Handles explicit subscriptions
    NodesByJID = subscribed_nodes_by_jid(NotifyType, SubsByDepth),
    lists:foreach(fun ({LJID, Nodes}) ->
			  LJIDs = case BroadcastAll of
				      true ->
					  {U, S, _} = LJID,
					  [{U, S, R} || R <- user_resources(U, S)];
				      false ->
					  [LJID]
				  end,
			  SHIMStanza = add_headers(Stanza, collection_shim(Node, Nodes)),
			  lists:foreach(fun(To) ->
			  			{TU, TS, TR} = To, 
						ejabberd_router ! {route, From, exmpp_jid:make(TU, TS, TR), SHIMStanza}
					end, LJIDs)
		  end, NodesByJID),
    %% Handles implicit presence subscriptions
    case Host of
	{LUser, LServer, LResource} ->
	    SenderResource = case LResource of
		undefined ->
		    case user_resources(LUser, LServer) of
			[Resource|_] -> Resource;
			_ -> <<"">>
		    end;
		_ ->
		    LResource
	    end,
	    case ejabberd_sm:get_session_pid(LUser, LServer, SenderResource) of
		C2SPid when is_pid(C2SPid) ->
		    %% set the from address on the notification to the bare JID of the account owner
		    %% Also, add "replyto" if entity has presence subscription to the account owner
		    %% See XEP-0163 1.1 section 4.3.1
		    Sender = exmpp_jid:make(LUser, LServer),
		    %%ReplyTo = jlib:make_jid(LUser, LServer, SenderResource),  % This has to be used
		    case catch ejabberd_c2s:get_subscribed(C2SPid) of
			Contacts when is_list(Contacts) ->
			    lists:foreach(fun({U, S, _}) ->
				spawn(fun() ->
				    Rs = lists:foldl(fun(R, Acc) ->
					case is_caps_notify(LServer, Node, {U, S, R}) of
					    true -> [R | Acc];
					    false -> Acc
					end
				    end, [], user_resources(U, S)),
				    lists:foreach(fun(R) ->
					ejabberd_router ! {route, Sender, exmpp_jid:make(U, S, R), Stanza}
				    end, Rs)
				end)
			    end, Contacts);
			_ ->
			    ok
		    end,
		    ok;
		_ ->
		    ?DEBUG("~p@~p has no session; can't deliver ~p to contacts", [LUser, LServer, Stanza]),
		    ok
	    end;
	_ ->
	    ok
    end.

subscribed_nodes_by_jid(NotifyType, SubsByDepth) ->
    NodesToDeliver = fun(Depth, Node, Subs, Acc) ->
		NodeId = case Node#pubsub_node.nodeid of
		    {_, N} -> N;
		    Other -> Other
		end,
		NodeOptions = Node#pubsub_node.options,
		lists:foldl(fun({LJID, _SubID, SubOptions}, Acc2) ->
				     case is_to_deliver(LJID, NotifyType, Depth,
							NodeOptions, SubOptions) of
					 true  -> [{LJID, NodeId}|Acc2];
					 false -> Acc2
				     end
			     end, Acc, Subs)
	end,
    DepthsToDeliver = fun({Depth, SubsByNode}, Acc) ->
		lists:foldl(fun({Node, Subs}, Acc2) ->
				    NodesToDeliver(Depth, Node, Subs, Acc2)
			    end, Acc, SubsByNode)
	end,
    JIDSubs = lists:foldl(DepthsToDeliver, [], SubsByDepth),
    [{LJID, proplists:append_values(LJID, JIDSubs)} || LJID <- proplists:get_keys(JIDSubs)].

%% If we don't know the resource, just pick first if any
%% If no resource available, check if caps anyway (remote online)
user_resources(User, Server) ->
    case ejabberd_sm:get_user_resources(User, Server) of
	[] -> mod_caps:get_user_resources(User, Server);
	Rs -> Rs
    end.

is_caps_notify(Host, Node, LJID) ->
    case mod_caps:get_caps(LJID) of
	nothing -> 
	    false;
	Caps ->
	    case catch mod_caps:get_features(Host, Caps) of
		Features when is_list(Features) -> lists:member(node_to_string(Node) ++ "+notify", Features);
		_ -> false
	    end
    end.

%%%%%%% Configuration handling

%%<p>There are several reasons why the default node configuration options request might fail:</p>
%%<ul>
%%<li>The service does not support node configuration.</li>
%%<li>The service does not support retrieval of default node configuration.</li>
%%</ul>
get_configure(Host, ServerHost, Node, From, Lang) ->
    ServerHostB = list_to_binary(ServerHost),
    Action =
	fun(#pubsub_node{options = Options, type = Type, id = NodeId}) ->
		case node_call(Type, get_affiliation, [NodeId, From]) of
		    {result, owner} ->
			Groups = ejabberd_hooks:run_fold(roster_groups, ServerHostB, [], [ServerHostB]),
			{result,
			 #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'pubsub', children =
			   [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'configure', attrs =
			     nodeAttr(Node), children =
			     [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
			       [?XMLATTR('type', <<"form">>)], children =
			       get_configure_xfields(Type, Options, Lang, Groups)
			      }]}]}};
		    _ ->
			{error, 'forbidden'}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
    end.

get_default(Host, Node, _From, Lang) ->
    Type = select_type(Host, Host, Node),
    Options = node_options(Type),
    {result, #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'pubsub', children =
		[#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'default', children =
		    [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = [?XMLATTR('type', <<"form">>)], children =
			get_configure_xfields(Type, Options, Lang, [])
		}]}]}}.

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
		    exmpp_jid:to_list(get_option(Options, Var)),
		    [exmpp_jid:to_list(O) || O <- Opts])).

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
    [?XFIELD("hidden", "", "FORM_TYPE", ?NS_PUBSUB_NODE_CONFIG_s),
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
    case exmpp_xml:remove_cdata_from_list(Els) of
	[#xmlel{ns = ?NS_DATA_FORMS, name = 'x'} = XEl] ->
	    case exmpp_xml:get_attribute_as_list(XEl, 'type', undefined) of
		"cancel" ->
		    {result, []};
		"submit" ->
		    Action =
			fun(#pubsub_node{options = Options, type = Type, id = NodeId} = N) ->
				case node_call(Type, get_affiliation, [NodeId, From]) of
				    {result, owner} ->
					case jlib:parse_xdata_submit(XEl) of
					    invalid ->
						{error, 'bad-request'};
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
					{error, 'forbidden'}
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
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
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
	    error -> {error, 'not-acceptable'};
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
		{error, 'not-acceptable'}
	end).

-define(SET_ALIST_XOPT(Opt, Val, Vals),
	case lists:member(Val, [atom_to_list(V) || V <- Vals]) of
	    true -> set_xoption(Host, Opts, add_opt(Opt, list_to_atom(Val), NewOpts));
	    false -> {error, 'not-acceptable'}
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

set_cached_item({_, ServerHost, _}, NodeId, ItemId, Payload) ->
    set_cached_item(ServerHost, NodeId, ItemId, Payload);
set_cached_item(Host, NodeId, ItemId, Payload) ->
    case is_last_item_cache_enabled(Host) of
    true -> ets:insert(gen_mod:get_module_proc(Host, last_items), {NodeId, {ItemId, Payload}});
    _ -> ok
    end.
unset_cached_item({_, ServerHost, _}, NodeId) ->
    unset_cached_item(ServerHost, NodeId);
unset_cached_item(Host, NodeId) ->
    case is_last_item_cache_enabled(Host) of
    true -> ets:delete(gen_mod:get_module_proc(Host, last_items), NodeId);
    _ -> ok
    end.
get_cached_item({_, ServerHost, _}, NodeId) ->
    get_cached_item(ServerHost, NodeId);
get_cached_item(Host, NodeId) ->
    case is_last_item_cache_enabled(Host) of
    true ->
	case catch ets:lookup(gen_mod:get_module_proc(Host, last_items), NodeId) of
	[{NodeId, {ItemId, Payload}}] ->
	    #pubsub_item{itemid = {ItemId, NodeId}, payload = Payload};
	_ ->
	    undefined
	end;
    _ ->
	undefined
    end.

%%%% plugin handling
plugins(Host) when is_binary(Host) ->
	plugins(binary_to_list(Host));
plugins(Host) when is_list(Host) ->
    case catch ets:lookup(gen_mod:get_module_proc(Host, config), plugins) of
    [{plugins, []}] -> [?STDNODE];
    [{plugins, PL}] -> PL;
    _ -> [?STDNODE]
    end.
select_type(ServerHost, Host, Node, Type) when is_list(ServerHost) ->
    select_type(list_to_binary(ServerHost), Host, Node, Type);
select_type(ServerHost, Host, Node, Type) ->
    SelectedType = case Host of
    {_User, _Server, _Resource} -> 
	case catch ets:lookup(gen_mod:get_module_proc(ServerHost, config), pep_mapping) of
	[{pep_mapping, PM}] -> proplists:get_value(Node, PM, ?PEPNODE);
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
features(Host, []) ->
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
    catch mnesia:sync_dirty(Fun).

%% @doc <p>node plugin call.</p>
node_call(Type, Function, Args) ->
    ?DEBUG("node_call ~p ~p ~p",[Type, Function, Args]),
    Module = list_to_atom(?PLUGIN_PREFIX++Type),
    case catch apply(Module, Function, Args) of
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
    transaction(fun() ->
			node_call(Type, Function, Args)
		end, sync_dirty).

%% @doc <p>plugin transaction handling.</p>
transaction(Host, Node, Action, Trans) ->
    transaction(fun() ->
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

transaction(Fun, Trans) ->
    case catch mnesia:Trans(Fun) of
	{result, Result} -> {result, Result};
	{error, Error} -> {error, Error};
	{atomic, {result, Result}} -> {result, Result};
	{atomic, {error, Error}} -> {error, Error};
	{aborted, Reason} ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [{aborted, Reason}]),
	    {error, 'internal-server-error'};
	{'EXIT', Reason} ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [{'EXIT', Reason}]),
	    {error, 'internal-server-error'};
	Other ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [Other]),
	    {error, 'internal-server-error'}
    end.

%%%% helpers

%% Add pubsub-specific error element
extended_error(Error, Ext) ->
    extended_error(Error, Ext, []).
extended_error(Error, unsupported, Feature) ->
    extended_error(Error, unsupported,
		   [?XMLATTR('feature', Feature)]);
extended_error(Error, Ext, ExtAttrs) ->
    Pubsub_Err = #xmlel{ns = ?NS_PUBSUB_ERRORS, name = Ext, attrs = ExtAttrs},
    exmpp_xml:append_child(exmpp_stanza:error(?NS_JABBER_CLIENT, Error),
      Pubsub_Err).

%% Give a uniq identifier
uniqid() ->
    {T1, T2, T3} = now(),
    lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3])).

% node attributes
nodeAttr(Node) when is_list(Node) ->
    [?XMLATTR('node', Node)];
nodeAttr(Node) ->
    [?XMLATTR('node', node_to_string(Node))].

% item attributes
itemAttr([]) -> [];
itemAttr(ItemId) -> [?XMLATTR('id', ItemId)].

% build item elements from item list
itemsEls(Items) ->
    lists:map(fun(#pubsub_item{itemid = {ItemId, _}, payload = Payload}) ->
	#xmlel{ns = ?NS_PUBSUB, name = 'item', attrs = itemAttr(ItemId), children = Payload}
    end, Items).

add_message_type(#xmlel{name='message'} = El, Type) -> exmpp_stanza:set_type(El, Type);
add_message_type(El, _Type)  -> El.

add_headers(#xmlel{} = El, HeaderEls) ->
     HeaderEl = #xmlel{ns = ?NS_SHIM, name = 'headers', children = HeaderEls},
     exmpp_xml:prepend_child(El, HeaderEl).

collection_shim(Node, Nodes) ->
    [#xmlel{ns = ?NS_PUBSUB, name ='header', 
     	    attrs = [?XMLATTR('name', <<"Collection">>)],
	    children = [ ?XMLCDATA(node_to_string(N))]
     } || N <- Nodes -- [Node]].
