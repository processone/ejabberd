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
%%% Portions created by ProcessOne are Copyright 2006-2008, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2008, ProcessOne.
%%%
%%% @copyright 2006-2008 ProcessOne
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
%%% This module uses version 1.11 of the specification as a base.
%%% Most of the specification is implemented.
%%% Functions concerning configuration should be rewritten.
%%% Code is derivated from the original pubsub v1.7, by Alexey Shchepin <alexey@process-one.net>

%%% TODO
%%% plugin: generate Reply (do not use broadcast atom anymore)

-module(mod_pubsub).
-author('christophe.romain@process-one.net').
-version('1.11-01').

-behaviour(gen_server).
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").

-define(STDTREE, "default").
-define(STDNODE, "default").
-define(PEPNODE, "pep").

%% exports for hooks
-export([presence_probe/3,
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
	 subscribe_node/4,
	 unsubscribe_node/5,
	 publish_item/6,
	 delete_item/4,
	 get_configure/4,
	 set_configure/5,
	 get_items/3,
	 tree_action/3,
	 node_action/3,
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
	 make_stanza/3,
	 route_stanza/3
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

-define(PROCNAME, ejabberd_mod_pubsub).
-define(PLUGIN_PREFIX, "node_").
-define(TREE_PREFIX, "nodetree_").

-record(state, {server_host,
		host,
		access,
		pep_mapping = [],
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
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ServerHost, Opts]) ->
    ?DEBUG("pubsub init ~p ~p",[ServerHost,Opts]),
    Host = gen_mod:get_opt_host(ServerHost, Opts, "pubsub.@HOST@"),
    Access = gen_mod:get_opt(access_createnode, Opts, all),
    mod_disco:register_feature(ServerHost, ?NS_PUBSUB),
    ejabberd_hooks:add(disco_local_identity, ServerHost, ?MODULE, disco_local_identity, 75),
    ejabberd_hooks:add(disco_local_features, ServerHost, ?MODULE, disco_local_features, 75),
    ejabberd_hooks:add(disco_local_items, ServerHost, ?MODULE, disco_local_items, 75),
    ejabberd_hooks:add(disco_sm_identity, ServerHost, ?MODULE, disco_sm_identity, 75),
    ejabberd_hooks:add(disco_sm_features, ServerHost, ?MODULE, disco_sm_features, 75),
    ejabberd_hooks:add(disco_sm_items, ServerHost, ?MODULE, disco_sm_items, 75),
    ejabberd_hooks:add(presence_probe_hook, ServerHost, ?MODULE, presence_probe, 50),
    ejabberd_hooks:add(remove_user, ServerHost, ?MODULE, remove_user, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    lists:foreach(
      fun({NS,Mod,Fun}) ->
	      gen_iq_handler:add_iq_handler(
		Mod, ServerHost, NS, ?MODULE, Fun, IQDisc)
      end,
      [{?NS_PUBSUB, ejabberd_local, iq_local},
       {?NS_PUBSUB_OWNER, ejabberd_local, iq_local},
       {?NS_PUBSUB, ejabberd_sm, iq_sm},
       {?NS_PUBSUB_OWNER, ejabberd_sm, iq_sm}]),
    ejabberd_router:register_route(Host),
    {Plugins, NodeTree, PepMapping} = init_plugins(Host, ServerHost, Opts),
    update_database(Host),
    ets:new(gen_mod:get_module_proc(Host, pubsub_state), [set, named_table]),
    ets:insert(gen_mod:get_module_proc(Host, pubsub_state), {nodetree, NodeTree}),
    ets:insert(gen_mod:get_module_proc(Host, pubsub_state), {plugins, Plugins}),
    ets:new(gen_mod:get_module_proc(ServerHost, pubsub_state), [set, named_table]),
    ets:insert(gen_mod:get_module_proc(ServerHost, pubsub_state), {nodetree, NodeTree}),
    ets:insert(gen_mod:get_module_proc(ServerHost, pubsub_state), {plugins, Plugins}),
	ets:insert(gen_mod:get_module_proc(ServerHost, pubsub_state), {pep_mapping, PepMapping}),
    init_nodes(Host, ServerHost),
    {ok, #state{host = Host,
		server_host = ServerHost,
		access = Access,
		pep_mapping = PepMapping,
		nodetree = NodeTree,
		plugins = Plugins}}.

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
%% <p>See {@link node_default:init/1} for an example implementation.</p>
init_plugins(Host, ServerHost, Opts) ->
    TreePlugin = list_to_atom(?TREE_PREFIX ++
			      gen_mod:get_opt(nodetree, Opts, ?STDTREE)),
    ?DEBUG("** tree plugin is ~p",[TreePlugin]),
    TreePlugin:init(Host, ServerHost, Opts),
    Plugins = lists:usort(gen_mod:get_opt(plugins, Opts, []) ++ [?STDNODE]),
	PepMapping = lists:usort(gen_mod:get_opt(pep_mapping, Opts, [])), 
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

init_nodes(Host, ServerHost) ->
    create_node(Host, ServerHost, ["home"], service_jid(Host), ?STDNODE),
    create_node(Host, ServerHost, ["home", ServerHost], service_jid(Host), ?STDNODE),
    ok.

update_database(Host) ->
    case catch mnesia:table_info(pubsub_node, attributes) of
	[host_node, host_parent, info] ->
	    ?INFO_MSG("upgrade pubsub tables",[]),
	    F = fun() ->
			NewRecords =
			    lists:foldl(
			      fun({pubsub_node, NodeId, ParentId, {nodeinfo, Items, Options, Entities}}, RecList) ->
				      ItemsList =
					  lists:foldl(
					    fun({item, IID, Publisher, Payload}, Acc) ->
						    C = {Publisher, unknown},
						    M = {Publisher, now()},
						    mnesia:write(
						      #pubsub_item{itemid = {IID, NodeId},
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
						    mnesia:write(
						      #pubsub_state{stateid = {JID, NodeId},
								    items = UsrItems,
								    affiliation = Aff,
								    subscription = Sub}),
						    case Aff of
							owner -> [JID | Acc];
							_ -> Acc
						    end
					    end, [], Entities),
				      mnesia:delete({pubsub_node, NodeId}),
				      [#pubsub_node{nodeid = NodeId,
						    parentid = ParentId,
						    owners = Owners,
						    options = Options} |
				       RecList]
			      end, [],
			      mnesia:match_object(
				{pubsub_node, {Host, '_'}, '_', '_'})),
			mnesia:delete_table(pubsub_node),
			mnesia:create_table(pubsub_node,
					    [{disc_copies, [node()]},
					     {attributes, record_info(fields, pubsub_node)}]),
			lists:foreach(fun(Record) ->
					      mnesia:write(Record)
				      end, NewRecords)
		end,
	    mnesia:transaction(F);
	_ ->
	    ok
    end.

%% -------
%% disco hooks handling functions
%%

identity(Host) ->
    Identity = case lists:member(?PEPNODE, plugins(Host)) of
    true -> [{"category", "pubsub"}, {"type", "pep"}];
    false -> [{"category", "pubsub"}, {"type", "service"}]
    end,
    {xmlelement, "identity", Identity, []}.

disco_local_identity(Acc, _From, To, [], _Lang) ->
    Acc ++ [identity(To#jid.lserver)];
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
    end, features(Host, []))};
disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_local_items(Acc, _From, _To, [], _Lang) ->
    Acc;
disco_local_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_sm_identity(Acc, _From, To, [], _Lang) ->
    Acc ++ [identity(To#jid.lserver)];
disco_sm_identity(Acc, From, To, Node, _Lang) ->
    LOwner = jlib:jid_tolower(jlib:jid_remove_resource(To)),
    Acc ++ case node_disco_identity(LOwner, From, Node) of
	       {result, I} -> I;
	       _ -> []
	   end.

disco_sm_features(Acc, _From, _To, [], _Lang) ->
    Acc;
disco_sm_features(Acc, From, To, Node, _Lang) ->
    LOwner = jlib:jid_tolower(jlib:jid_remove_resource(To)),
    Features = node_disco_features(LOwner, From, Node),
    case {Acc, Features} of
	{{result, AccFeatures}, {result, AddFeatures}} ->
	    {result, AccFeatures++AddFeatures};
	{_, {result, AddFeatures}} ->
	    {result, AddFeatures};
	{_, _} ->
	    Acc
    end.

disco_sm_items(Acc, _From, To, [], _Lang) ->
    %% TODO, use iq_disco_items(Host, [], From)
    Host = To#jid.lserver,
    LJID = jlib:jid_tolower(jlib:jid_remove_resource(To)),
    case tree_action(Host, get_nodes, [Host]) of
	[] ->
	    Acc;
	Nodes ->
	    Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
	    NodeItems = lists:map(
			  fun(Node) ->
				  {xmlelement, "item",
				   [{"jid", jlib:jid_to_string(LJID)},
				    {"node", node_to_string(Node)}],
				   []}
			  end, Nodes),
	    {result, NodeItems ++ Items}
    end;

disco_sm_items(Acc, From, To, Node, _Lang) ->
    %% TODO, use iq_disco_items(Host, Node, From)
    Host = To#jid.lserver,
    LJID = jlib:jid_tolower(jlib:jid_remove_resource(To)),
    case get_items(Host, Node, From) of
	[] ->
	    Acc;
	AllItems ->
	    Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
	    NodeItems = lists:map(
			  fun(#pubsub_item{itemid = Id}) ->
				  %% "jid" is required by XEP-0030, and
				  %% "node" is forbidden by XEP-0060.
				  {xmlelement, "item",
				   [{"jid", jlib:jid_to_string(LJID)},
				    {"name", get_item_name(Host, Node, Id)}],
				   []}
			  end, AllItems),
	    {result, NodeItems ++ Items}
    end.

%% -------
%% presence hooks handling functions
%%

presence_probe(#jid{lserver = Host} = JID, JID, Pid) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {presence, JID, Pid});
presence_probe(_, _, _) ->
    ok.

%% -------
%% user remove hook handling function
%%

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
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
    LJID = jlib:jid_tolower(JID),
    Host = State#state.host,
    ServerHost = State#state.server_host,
    %% for each node From is subscribed to
    %% and if the node is so configured, send the last published item to From
    lists:foreach(fun(Type) ->
	{result, Subscriptions} = node_action(Type, get_entity_subscriptions, [Host, JID]),
	lists:foreach(
	    fun({Node, subscribed}) -> 
		case tree_action(Host, get_node, [Host, Node]) of
		    #pubsub_node{options = Options} ->
			case get_option(Options, send_last_published_item) of
			    on_sub_and_presence ->
				send_last_item(Host, Node, LJID);
			    _ ->
				ok
			end;
		    _ ->
			ok
		end;
		(_) ->
		ok
	    end, Subscriptions)
    end, State#state.plugins),
    %% and send to From last PEP events published by its contacts
    case catch ejabberd_c2s:get_subscribed(Pid) of
	ContactsWithCaps when is_list(ContactsWithCaps) ->
	    Caps = proplists:get_value(LJID, ContactsWithCaps),
	    ContactsUsers = lists:usort(lists:map(
				fun({{User, Server, _}, _}) -> {User, Server} end, ContactsWithCaps)),
	    lists:foreach(
		fun({User, Server}) ->
		    PepKey = {User, Server, ""},
		    lists:foreach(fun(#pubsub_node{nodeid = {_, Node}, options = Options}) ->
			case get_option(Options, send_last_published_item) of
			    on_sub_and_presence ->
				case is_caps_notify(ServerHost, Node, Caps) of
				    true ->
					Subscribed = case get_option(Options, access_model) of
					    open -> true;
					    presence -> true;
					    whitelist -> false; % subscribers are added manually
					    authorize -> false; % likewise
					    roster ->
						Grps = get_option(Options, roster_groups_allowed, []),
						element(2, get_roster_info(User, Server, LJID, Grps))
					end,
					if Subscribed ->
					    ?DEBUG("send ~s's ~s event to ~s",[jlib:jid_to_string(PepKey),Node,jlib:jid_to_string(LJID)]),
					    send_last_item(PepKey, Node, LJID);
					true ->
					    ok
					end;
				    false ->
					ok
				end;
			    _ ->
				ok
			end
		    end, tree_action(Host, get_nodes, [PepKey]))
		end, ContactsUsers);
	_ ->
	    ok
    end,
    {noreply, State};

handle_cast({remove_user, LUser, LServer}, State) ->
    Host = State#state.host,
    Owner = jlib:make_jid(LUser, LServer, ""),
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    %% remove user's subscriptions
    lists:foreach(fun(Type) ->
	{result, Subscriptions} = node_action(Type, get_entity_subscriptions, [Host, Owner]),
	lists:foreach(fun
	    ({Node, subscribed}) ->
		JID = jlib:jid_to_string(Owner),
		unsubscribe_node(Host, Node, Owner, JID, all);
	    (_) ->  
		ok
	end, Subscriptions)
    end, State#state.plugins),
    %% remove user's PEP nodes 
    lists:foreach(fun(#pubsub_node{nodeid={NodeKey, NodeName}}) ->
	delete_node(NodeKey, NodeName, Owner)
    end, tree_action(Host, get_nodes, [OwnerKey])),
    %% remove user's nodes
    delete_node(Host, ["home", LServer, LUser], Owner),
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
    terminate_plugins(Host, ServerHost, Plugins, TreePlugin),
    ejabberd_router:unregister_route(Host),
    ejabberd_hooks:delete(disco_local_identity, ServerHost, ?MODULE, disco_local_identity, 75),
    ejabberd_hooks:delete(disco_local_features, ServerHost, ?MODULE, disco_local_features, 75),
    ejabberd_hooks:delete(disco_local_items, ServerHost, ?MODULE, disco_local_items, 75),
    ejabberd_hooks:delete(disco_sm_identity, ServerHost, ?MODULE, disco_sm_identity, 75),
    ejabberd_hooks:delete(disco_sm_features, ServerHost, ?MODULE, disco_sm_features, 75),
    ejabberd_hooks:delete(disco_sm_items, ServerHost, ?MODULE, disco_sm_items, 75),
    ejabberd_hooks:delete(presence_probe_hook, ServerHost, ?MODULE, presence_probe, 50),
    ejabberd_hooks:delete(remove_user, ServerHost, ?MODULE, remove_user, 50),
    lists:foreach(fun({NS,Mod}) ->
			  gen_iq_handler:remove_iq_handler(Mod, ServerHost, NS)
		  end, [{?NS_PUBSUB, ejabberd_local},
			{?NS_PUBSUB_OWNER, ejabberd_local},
			{?NS_PUBSUB, ejabberd_sm},
			{?NS_PUBSUB_OWNER, ejabberd_sm}]),
    mod_disco:unregister_feature(ServerHost, ?NS_PUBSUB),
    ok.

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
			    Res = case iq_disco_info(Host, Node, From, Lang) of
				      {result, IQRes} ->
					  jlib:iq_to_xml(
					    IQ#iq{type = result,
						  sub_el = [{xmlelement, "query",
							     QAttrs, IQRes}]});
				      {error, Error} ->
					  jlib:make_error_reply(Packet, Error)
				  end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS,
			    sub_el = SubEl} = IQ ->
			    {xmlelement, _, QAttrs, _} = SubEl,
			    Node = xml:get_attr_s("node", QAttrs),
			    Res = case iq_disco_items(Host, Node, From) of
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
				case iq_pubsub_owner(Host, From, IQType, SubEl, Lang) of
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

node_disco_info(Host, Node, From) ->
    node_disco_info(Host, Node, From, true, true).
node_disco_identity(Host, Node, From) ->
    node_disco_info(Host, Node, From, true, false).
node_disco_features(Host, Node, From) ->
    node_disco_info(Host, Node, From, false, true).
node_disco_info(Host, Node, From, Identity, Features) ->
    Action =
	fun(#pubsub_node{type = Type}) ->
		I = case Identity of
			false ->
			    [];
			true ->
			    Types =
				case tree_call(Host, get_subnodes, [Host, Node, From]) of
				    [] ->
					["leaf"]; %% No sub-nodes: it's a leaf node
				    _ ->
					case node_call(Type, get_items, [Host, Node, From]) of
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
			     lists:map(fun(T) ->
					       {xmlelement, "feature", [{"var", ?NS_PUBSUB++"#"++T}], []}
				       end, features(Type))]
		    end,
		%% TODO: add meta-data info (spec section 5.4)
		{result, I ++ F}
	end,
    transaction(Host, Node, Action, sync_dirty).

iq_disco_info(Host, SNode, From, Lang) ->
    Node = string_to_node(SNode),
    case Node of
	[] ->
	    {result,
	     [{xmlelement, "identity",
	       [{"category", "pubsub"},
		{"type", "service"},
		{"name", translate:translate(Lang, "Publish-Subscribe")}], []},
	      {xmlelement, "feature", [{"var", ?NS_DISCO_INFO}], []},
	      {xmlelement, "feature", [{"var", ?NS_DISCO_ITEMS}], []},
	      {xmlelement, "feature", [{"var", ?NS_PUBSUB}], []},
	      {xmlelement, "feature", [{"var", ?NS_VCARD}], []}] ++
	     lists:map(fun(Feature) ->
		 {xmlelement, "feature", [{"var", ?NS_PUBSUB++"#"++Feature}], []}
	     end, features(Host, SNode))};
	_ ->
	    node_disco_info(Host, Node, From)
    end.

iq_disco_items(Host, [], From) ->
    {result, lists:map(
	       fun(#pubsub_node{nodeid = {_, SubNode}}) ->
		       SN = node_to_string(SubNode),
		       RN = lists:last(SubNode),
		       %% remove name attribute
		       {xmlelement, "item", [{"jid", Host},
					     {"node", SN},
					     {"name", RN}], []}
	       end, tree_action(Host, get_subnodes, [Host, [], From]))};
iq_disco_items(Host, Item, From) ->
    case string:tokens(Item, "!") of
	[_SNode, _ItemID] ->
	    {result, []};
	[SNode] ->
	    Node = string_to_node(SNode),
	    %% Note: Multiple Node Discovery not supported (mask on pubsub#type)
	    %% TODO this code is also back-compatible with pubsub v1.8 (for client issue)
	    %% TODO make it pubsub v1.10 compliant (this breaks client compatibility)
	    %% TODO That is, remove name attribute
	    Action =
		fun(#pubsub_node{type = Type}) ->
			NodeItems = case node_call(Type, get_items, [Host, Node, From]) of
					{result, I} -> I;
					_ -> []
				    end,
			Nodes = lists:map(
				  fun(#pubsub_node{nodeid = {_, SubNode}}) ->
					  SN = node_to_string(SubNode),
					  RN = lists:last(SubNode),
					  {xmlelement, "item", [{"jid", Host}, {"node", SN}, 
								{"name", RN}], []}
				  end, tree_call(Host, get_subnodes, [Host, Node, From])),
			Items = lists:map(
				  fun(#pubsub_item{itemid = {RN, _}}) ->
					  SN = node_to_string(Node) ++ "!" ++ RN,
					  {xmlelement, "item", [{"jid", Host}, {"node", SN},
								{"name", get_item_name(Host, Node, RN)}], []}
				  end, NodeItems),
			{result, Nodes ++ Items}
		end,
	    transaction(Host, Node, Action, sync_dirty)
    end.

iq_local(From, To, #iq{type = Type,
		       sub_el = SubEl,
		       xmlns = XMLNS,
		       lang = Lang} = IQ) ->
    ServerHost = To#jid.lserver,
    %% Accept IQs to server only from our own users.
    if
	From#jid.lserver /= ServerHost ->
	    IQ#iq{type = error, sub_el = [?ERR_FORBIDDEN, SubEl]};
	true ->
	    LOwner = jlib:jid_tolower(jlib:jid_remove_resource(From)),
	    Res = case XMLNS of
		      ?NS_PUBSUB -> iq_pubsub(LOwner, ServerHost, From, Type, SubEl, Lang);
		      ?NS_PUBSUB_OWNER -> iq_pubsub_owner(LOwner, From, Type, SubEl, Lang)
		  end,
	    case Res of
		{result, IQRes} -> IQ#iq{type = result, sub_el = IQRes};
		{error, Error} -> IQ#iq{type = error, sub_el = [Error, SubEl]}
	    end
    end.

iq_sm(From, To, #iq{type = Type, sub_el = SubEl, xmlns = XMLNS, lang = Lang} = IQ) ->
    ServerHost = To#jid.lserver,
    LOwner = jlib:jid_tolower(jlib:jid_remove_resource(To)),
    Res = case XMLNS of
	      ?NS_PUBSUB -> iq_pubsub(LOwner, ServerHost, From, Type, SubEl, Lang);
	      ?NS_PUBSUB_OWNER -> iq_pubsub_owner(LOwner, From, Type, SubEl, Lang)
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
			    "\nCopyright (c) 2004-2008 Process-One"}]}].

iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang) ->
    iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, all, plugins(ServerHost)).

iq_pubsub(Host, ServerHost, From, IQType, SubEl, _Lang, Access, Plugins) ->
    {xmlelement, _, _, SubEls} = SubEl,
    WithoutCdata = xml:remove_cdata(SubEls),
    Configuration = lists:filter(fun({xmlelement, Name, _, _}) ->
					 Name == "configure"
				 end, WithoutCdata),
    Action = WithoutCdata -- Configuration,
    case Action of
	[{xmlelement, Name, Attrs, Els}] ->
	    Node = case Host of
		       {_, _, _} -> xml:get_attr_s("node", Attrs);
		       _ -> string_to_node(xml:get_attr_s("node", Attrs))
		   end,
	    case {IQType, Name} of
		{set, "create"} ->
		    case Configuration of
			[{xmlelement, "configure", _, Config}] ->
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
			_ ->
			    %% this breaks backward compatibility!
			    %% can not create node without <configure/>
			    %% but this is the new spec anyway
			    ?INFO_MSG("Node ~p ; invalid configuration: ~p", [Node, Configuration]),
			    {error, ?ERR_BAD_REQUEST}
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
		    JID = xml:get_attr_s("jid", Attrs),
		    subscribe_node(Host, Node, From, JID);
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
		    get_items(Host, Node, From, SubId, MaxItems, ItemIDs);
		{get, "subscriptions"} ->
		    get_subscriptions(Host, From, Plugins);
		{get, "affiliations"} ->
		    get_affiliations(Host, From, Plugins);
		_ ->
		    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	    end;
	_ ->
	    ?INFO_MSG("Too many actions: ~p", [Action]),
	    {error, ?ERR_BAD_REQUEST}
    end.

iq_pubsub_owner(Host, From, IQType, SubEl, Lang) ->
    {xmlelement, _, _, SubEls} = SubEl,
    Action = xml:remove_cdata(SubEls),
    case Action of
	[{xmlelement, Name, Attrs, Els}] ->
	    Node = case Host of
		       {_, _, _} -> xml:get_attr_s("node", Attrs);
		       _ -> string_to_node(xml:get_attr_s("node", Attrs))
		   end,
	    case {IQType, Name} of
		{get, "configure"} ->
		    get_configure(Host, Node, From, Lang);
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

%%% authorization handling

send_authorization_request(Host, Node, Subscriber) ->
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
		   {"label", translate:translate(Lang, "Allow this JID to subscribe to this pubsub node?")}],
		  [{xmlelement, "value", [], [{xmlcdata, "false"}]}]}]}]},
    case tree_action(Host, get_node, [Host, Node]) of
	#pubsub_node{owners = Owners} ->
	    lists:foreach(
	      fun(Owner) ->
		      ejabberd_router ! {route, service_jid(Host), jlib:make_jid(Owner), Stanza}
	      end, Owners),
	    ok;
	_ ->
	    ok
    end.

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
	    case lists:keysearch("FORM_TYPE", 1, XFields) of
		{value, {_, ?NS_PUBSUB_SUB_AUTH}} ->
		    XFields;
		_ ->
		    invalid
	    end
    end.

handle_authorization_response(Host, From, To, Packet, XFields) ->
    case {lists:keysearch("pubsub#node", 1, XFields),
	  lists:keysearch("pubsub#subscriber_jid", 1, XFields),
	  lists:keysearch("pubsub#allow", 1, XFields)} of
	{{value, {_, SNode}}, {value, {_, SSubscriber}},
	 {value, {_, SAllow}}} ->
	    Node = case Host of
		       {_, _, _} -> [SNode];
		       _ -> string:tokens(SNode, "/")
		   end,
	    Subscriber = jlib:string_to_jid(SSubscriber),
	    Allow = case SAllow of
			"1" -> true;
			"true" -> true;
			_ -> false
		    end,
	    Action = fun(#pubsub_node{type = Type,
				      %%options = Options,
				      owners = Owners}) ->
			     IsApprover = lists:member(jlib:jid_tolower(jlib:jid_remove_resource(From)), Owners),
			     Subscription = node_call(Type, get_subscription, [Host, Node, Subscriber]),
			     if
				 not IsApprover ->
				     {error, ?ERR_FORBIDDEN};
				 Subscription /= pending ->
				     {error, ?ERR_UNEXPECTED_REQUEST};
				 true ->
				     NewSubscription = case Allow of
							   true -> subscribed;
							   false -> none
						       end,
				     node_call(Type, set_subscription, [Host, Node, Subscriber, NewSubscription])
			     end
		     end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{error, Error} ->
		    ejabberd_router:route(
		     To, From,
		     jlib:make_error_reply(Packet, Error));
		{result, _NewSubscription} ->
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

create_node(Host, ServerHost, [], Owner, Type, Access, Configuration) ->
    case lists:member("instant-nodes", features(Type)) of
	true ->
	    {LOU, LOS, _} = jlib:jid_tolower(Owner),
	    HomeNode = ["home", LOS, LOU],
	    create_node(Host, ServerHost,
			HomeNode, Owner, Type, Access, Configuration),
	    NewNode = HomeNode ++ [randoms:get_string()],
	    case create_node(Host, ServerHost,
			     NewNode, Owner, Type, Access, Configuration) of
		{result, []} ->
		    {result,
		     [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		       [{xmlelement, "create", [{"node", node_to_string(NewNode)}], []}]}]};
		Error -> Error
	    end;
	false ->
	    %% Service does not support instant nodes
	    {error, extended_error(?ERR_NOT_ACCEPTABLE, "nodeid-required")}
    end;
create_node(Host, ServerHost, Node, Owner, GivenType, Access, Configuration) ->
    Type = select_type(ServerHost, Host, Node, GivenType),
    Parent = lists:sublist(Node, length(Node) - 1),
    %% TODO, check/set node_type = Type
    ParseOptions = case xml:remove_cdata(Configuration) of
		       [] ->
			   {result, node_options(Type)};
		       [{xmlelement, "x", _Attrs, _SubEls} = XEl] ->
			   case jlib:parse_xdata_submit(XEl) of
			       invalid ->
				   {error, ?ERR_BAD_REQUEST};
			       XData ->
				   case set_xoption(XData, node_options(Type)) of
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
			case node_call(Type, create_node_permission, [Host, ServerHost, Node, Parent, Owner, Access]) of
			    {result, true} ->
				case tree_call(Host, create_node, [Host, Node, Type, Owner, NodeOptions]) of
				    ok ->
					node_call(Type, create_node, [Host, Node, Owner]);
				    {error, ?ERR_CONFLICT} ->
					case ets:lookup(gen_mod:get_module_proc(ServerHost, pubsub_state), nodetree) of
					    [{nodetree, nodetree_virtual}] -> node_call(Type, create_node, [Host, Node, Owner]);
					    _ -> {error, ?ERR_CONFLICT}
					end;
				    Error ->
					Error
				end;
			    _ ->
				{error, ?ERR_FORBIDDEN}
			end
		end,
	    Reply = [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		      [{xmlelement, "create", [{"node", node_to_string(Node)}],
			[]}]}],
	    case transaction(CreateNode, transaction) of
		{error, Error} ->
		    %% in case we change transaction to sync_dirty...
		    %%node_action:
		    %%  node_call(Type, delete_node, [Host, Node]),
		    %%  tree_call(Host, delete_node, [Host, Node]),
		    {error, Error};
		{result, {Result, broadcast}} ->
		    %%Lang = "en", %% TODO: fix
		    %%OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
		    %%broadcast_publish_item(Host, Node, uniqid(), Owner,
		    %%	[{xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "result"}],
		    %%		[?XFIELD("hidden", "", "FORM_TYPE", ?NS_PUBSUB_NMI),
		    %%		?XFIELD("jid-single", "Node Creator", "creator", jlib:jid_to_string(OwnerKey))]}]),
		    %% todo publish_item(Host, ServerHost, ["pubsub", "nodes"], node_to_string(Node)),
		    case Result of
			default -> {result, Reply};
			_ -> {result, Result}
		    end;
		{result, default} ->
		    {result, Reply};
		{result, Result} ->
		    {result, Result}
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
    {error, ?ERR_NOT_ALLOWED};
delete_node(Host, Node, Owner) ->
    Action = fun(#pubsub_node{type = Type}) ->
		     case node_call(Type, get_affiliation, [Host, Node, Owner]) of
			 {result, owner} ->
			     Removed = tree_call(Host, delete_node, [Host, Node]),
			     node_call(Type, delete_node, [Host, Removed]);
			 _ ->
			     %% Entity is not an owner
			     {error, ?ERR_FORBIDDEN}
		     end
	     end,
    Reply = [],
    case transaction(Host, Node, Action, transaction) of
	{error, Error} ->
	    {error, Error};
	{result, {Result, broadcast, Removed}} ->
	    broadcast_removed_node(Host, Removed),
	    %%broadcast_retract_item(Host, ["pubsub", "nodes"], node_to_string(Node)),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {Result, Removed}} ->
	    broadcast_removed_node(Host, Removed),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, default} ->
	    {result, Reply};
	{result, Result} ->
	    {result, Result}
    end.

%% @spec (Host, Node, From, JID) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
%%	 Host = host()
%%	 Node = pubsubNode()
%%	 From = jid()
%%	 JID = jid()
%% @see node_default:subscribe_node/5
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
subscribe_node(Host, Node, From, JID) ->
    Subscriber = case jlib:string_to_jid(JID) of
		     error -> {"", "", ""};
		     J -> jlib:jid_tolower(J)
		 end,
    SubId = uniqid(),
    Action = fun(#pubsub_node{options = Options, type = Type}) ->
		     Features = features(Type),
		     SubscribeFeature = lists:member("subscribe", Features),
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
				 {true, true}
			 end,
		     if
			 not SubscribeFeature ->
			     %% Node does not support subscriptions
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "subscribe")};
			 not SubscribeConfig ->
			     %% Node does not support subscriptions
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "subscribe")};
			 true ->
			     node_call(Type, subscribe_node,
				       [Host, Node, From, Subscriber,
					AccessModel, SendLast,
					PresenceSubscription, RosterGroup])
		     end
	     end,
    Reply = fun(Subscription) ->
		    %% TODO, this is subscription-notification, should depends on node features
		    Fields =
			[{"node", node_to_string(Node)},
			 {"jid", jlib:jid_to_string(Subscriber)},
			 {"subscription", subscription_to_string(Subscription)}],
		    [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}], 
			[{xmlelement, "subscription",
			    case Subscription of
			    subscribed -> [{"subid", SubId}|Fields];
			    _ -> Fields
			    end, []}]}]
	    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{error, Error} ->
	    {error, Error};
	{result, {Result, subscribed, send_last}} ->
	    send_last_item(Host, Node, Subscriber),
	    case Result of
		default -> {result, Reply(subscribed)};
		_ -> {result, Result}
	    end;
	{result, {Result, Subscription}} ->
	    case Subscription of
		pending -> send_authorization_request(Host, Node, Subscriber);
		_ -> ok
	    end,
	    case Result of
		default -> {result, Reply(Subscription)};
		_ -> {result, Result}
	    end;
	{result, Result} ->
	    %% this case should never occure anyway
	    {result, Result}
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
unsubscribe_node(Host, Node, From, JID, SubId) ->
    Subscriber = case jlib:string_to_jid(JID) of
		     error -> {"", "", ""};
		     J -> jlib:jid_tolower(J)
		 end,
    case node_action(Host, Node, unsubscribe_node,
		     [Host, Node, From, Subscriber, SubId]) of
	{error, Error} ->
	    {error, Error};
	{result, default} ->
	    {result, []};
	{result, Result} ->
	    {result, Result}
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
    Action = fun(#pubsub_node{options = Options, type = Type}) ->
		     Features = features(Type),
		     PublishFeature = lists:member("publish", Features),
		     PublishModel = get_option(Options, publish_model),
		     MaxItems = max_items(Options),
		     PayloadSize = size(term_to_binary(Payload)),
		     PayloadMaxSize = get_option(Options, max_payload_size),
		     if
			 not PublishFeature ->
			     %% Node does not support item publication
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "publish")};
			 PayloadSize > PayloadMaxSize ->
			     %% Entity attempts to publish very large payload
			     {error, extended_error(?ERR_NOT_ACCEPTABLE, "payload-too-big")};
			 %%?? ->   iq_pubsub just does that matchs
			 %%	% Entity attempts to publish item with multiple payload elements or namespace does not match
			 %%	{error, extended_error(?ERR_BAD_REQUEST, "invalid-payload")};
			 %%	% Publisher attempts to publish to persistent node with no item
			 %%	{error, extended_error(?ERR_BAD_REQUEST, "item-required")};
			 Payload == "" ->
			     %% Publisher attempts to publish to payload node with no payload
			     {error, extended_error(?ERR_BAD_REQUEST, "payload-required")};
			 %%?? ->
			 %%	% Publisher attempts to publish to transient notification node with item
			 %%	{error, extended_error(?ERR_BAD_REQUEST, "item-forbidden")};
			 true ->
			     node_call(Type, publish_item, [Host, Node, Publisher, PublishModel, MaxItems, ItemId, Payload])
		     end
	     end,
    ejabberd_hooks:run(pubsub_publish_item, ServerHost, [ServerHost, Node, Publisher, service_jid(Host), ItemId, Payload]),
    Reply = [],
    case transaction(Host, Node, Action, sync_dirty) of
	{error, ?ERR_ITEM_NOT_FOUND} ->
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
			    {error, ?ERR_ITEM_NOT_FOUND}
		    end;
		false ->
		    {error, ?ERR_ITEM_NOT_FOUND}
	    end;
	{error, Reason} ->
	    {error, Reason};
	{result, {Result, broadcast, Removed}} ->
	    lists:foreach(fun(OldItem) ->
				  broadcast_retract_item(Host, Node, OldItem)
			  end, Removed),
	    broadcast_publish_item(Host, Node, ItemId, jlib:jid_tolower(Publisher), Payload),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, default, Removed} ->
	    lists:foreach(fun(OldItem) ->
				  broadcast_retract_item(Host, Node, OldItem)
			  end, Removed),
	    {result, Reply};
	{result, Result, Removed} ->
	    lists:foreach(fun(OldItem) ->
				  broadcast_retract_item(Host, Node, OldItem)
			  end, Removed),
	    {result, Result};
	{result, default} ->
	    {result, Reply};
	{result, Result} ->
	    {result, Result}
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
    Action = fun(#pubsub_node{type = Type}) ->
		     Features = features(Type),
		     PersistentFeature = lists:member("persistent-items", Features),
		     DeleteFeature = lists:member("delete-nodes", Features),
		     if
			 %%?? ->   iq_pubsub just does that matchs
			 %%	%% Request does not specify an item
			 %%	{error, extended_error(?ERR_BAD_REQUEST, "item-required")};
			 not PersistentFeature ->
			     %% Node does not support persistent items
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "persistent-items")};
			 not DeleteFeature ->
			     %% Service does not support item deletion
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "delete-nodes")};
			 true ->
			     node_call(Type, delete_item, [Host, Node, Publisher, ItemId])
		     end
	     end,
    Reply = [],
    case transaction(Host, Node, Action, sync_dirty) of
	{error, Reason} ->
	    {error, Reason};
	{result, {Result, broadcast}} ->
	    broadcast_retract_item(Host, Node, ItemId, ForceNotify),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, default} ->
	    {result, Reply};
	{result, Result} ->
	    {result, Result}
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
    Action = fun(#pubsub_node{type = Type, options = Options}) ->
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
			     node_call(Type, purge_node, [Host, Node, Owner])
		     end
	     end,
    Reply = [],
    case transaction(Host, Node, Action, sync_dirty) of
	{error, Reason} ->
	    {error, Reason};
	{result, {Result, broadcast}} ->
	    broadcast_purge_node(Host, Node),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, default} ->
	    {result, Reply};
	{result, Result} ->
	    {result, Result}
    end.

%% @doc <p>Return the items of a given node.</p>
%% <p>The number of items to return is limited by MaxItems.</p>
%% <p>The permission are not checked in this function.</p>
%% @todo We probably need to check that the user doing the query has the right
%% to read the items.
get_items(Host, Node, From, SubId, SMaxItems, ItemIDs) ->
    MaxItems =
	if
	    SMaxItems == "" -> ?MAXITEMS;
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
	    Action = fun(#pubsub_node{options = Options, type = Type}) ->
		     Features = features(Type),
		     RetreiveFeature = lists:member("retrieve-items", Features),
		     PersistentFeature = lists:member("persistent-items", Features),
		     AccessModel = get_option(Options, access_model),
		     AllowedGroups = get_option(Options, roster_groups_allowed, []),
		     {PresenceSubscription, RosterGroup} =
			 case Host of
			     {OUser, OServer, _} ->
				 get_roster_info(OUser, OServer,
						 jlib:jid_tolower(From), AllowedGroups);
			     _ ->
				 {true, true}
			 end,
		     if
			 not RetreiveFeature ->
			     %% Item Retrieval Not Supported
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "retrieve-items")};
			 not PersistentFeature ->
			     %% Persistent Items Not Supported
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "persistent-items")};
			 true ->
			     node_call(Type, get_items,
				       [Host, Node, From,
					AccessModel, PresenceSubscription, RosterGroup,
					SubId])
		     end
	     end,
	     case transaction(Host, Node, Action, sync_dirty) of
		{error, Reason} ->
		    {error, Reason};
		{result, Items} ->
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
		    ItemsEls = lists:map(
				    fun(#pubsub_item{itemid = {ItemId, _},
						    payload = Payload}) ->
					    ItemAttrs = case ItemId of
							    "" -> [];
							    _ -> [{"id", ItemId}]
							end,
					    {xmlelement, "item", ItemAttrs, Payload}
				    end, lists:sublist(SendItems, MaxItems)),
		    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
				[{xmlelement, "items", [{"node", node_to_string(Node)}],
				    ItemsEls}]}]}
	    end
    end.

get_items(Host, Node, From) ->
    case node_action(Host, Node, get_items, [Host, Node, From]) of
	{result, Items} -> Items;
	_ -> []
    end.

%% @spec (Host, Node, LJID) -> any()
%%	 Host = host()
%%	 Node = pubsubNode()
%%	 LJID = {U, S, []}
%% @doc <p>Resend the items of a node to the user.</p>
%send_all_items(Host, Node, LJID) ->
%    send_items(Host, Node, LJID, all).

send_last_item(Host, Node, LJID) ->
    send_items(Host, Node, LJID, last).

%% TODO use cache-last-item feature
send_items(Host, Node, LJID, Number) ->
    ToSend = case get_items(Host, Node, LJID) of
	[] -> 
	    [];
	Items ->
	    case Number of
		last -> lists:sublist(lists:reverse(Items), 1);
		all -> Items;
		N when N > 0 -> lists:nthtail(length(Items)-N, Items);
		_ -> Items
	    end
    end,
    ItemsEls = lists:map(
		fun(#pubsub_item{itemid = {ItemId, _}, payload = Payload}) ->
		    ItemAttrs = case ItemId of
			"" -> [];
			_ -> [{"id", ItemId}]
		    end,
		    {xmlelement, "item", ItemAttrs, Payload}
		end, ToSend),
    Stanza = {xmlelement, "message", [],
	      [{xmlelement, "event", [{"xmlns", ?NS_PUBSUB_EVENT}],
		[{xmlelement, "items", [{"node", node_to_string(Node)}],
		  ItemsEls}]}]},
    ejabberd_router ! {route, service_jid(Host), jlib:make_jid(LJID), Stanza}.

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
			       {{error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "retrieve-affiliations")}, Acc};
			   true ->
			       {result, Affiliations} = node_action(Type, get_entity_affiliations, [Host, JID]),
			       {Status, [Affiliations|Acc]}
		       end
	       end, {ok, []}, Plugins),
    case Result of
	{ok, Affiliations} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({Node, Affiliation}) ->
				 [{xmlelement, "affiliation",
				   [{"node", node_to_string(Node)},
				    {"affiliation", affiliation_to_string(Affiliation)}],
				   []}]
			 end, lists:usort(lists:flatten(Affiliations))),
	    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		       [{xmlelement, "affiliations", [],
			 Entities}]}]};
	{Error, _} ->
	    Error
    end;
get_affiliations(Host, Node, JID) ->
    Action = fun(#pubsub_node{type = Type}) ->
		     Features = features(Type),
		     RetrieveFeature = lists:member("modify-affiliations", Features),
		     Affiliation = node_call(Type, get_affiliation, [Host, Node, JID]),
		     if
			 not RetrieveFeature ->
			     %% Service does not support modify affiliations
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "modify-affiliations")};
			 Affiliation /= {result, owner} ->
			     %% Entity is not an owner
			     {error, ?ERR_FORBIDDEN};
			 true ->
			     node_call(Type, get_node_affiliations, [Host, Node])
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{error, Reason} ->
	    {error, Reason};
	{result, []} ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	{result, Affiliations} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({AJID, Affiliation}) ->
				 [{xmlelement, "affiliation",
				   [{"jid", jlib:jid_to_string(AJID)},
				    {"affiliation", affiliation_to_string(Affiliation)}],
				   []}]
			 end, Affiliations),
	    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB_OWNER}],
		       [{xmlelement, "affiliations", [{"node", node_to_string(Node)}],
			 Entities}]}]}
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
	    Action = fun(#pubsub_node{type = Type, owners = Owners}) ->
			     case lists:member(Owner, Owners) of
				 true ->
				     lists:foreach(
				       fun({JID, Affiliation}) ->
					       node_call(
						 Type, set_affiliation,
						 [Host, Node, JID, Affiliation])
				       end, Entities),
				     {result, []};
				 _ ->
				     {error, ?ERR_FORBIDDEN}
			     end
		     end,
	    transaction(Host, Node, Action, sync_dirty)
    end.


%% @spec (Host, JID, Plugins) -> {error, Reason} | {result, Response}
%%	 Host = host()
%%	 JID = jid()
%%	 Plugins = [Plugin::string()]
%%	 Reason = stanzaError()
%%	 Response = [pubsubIQResponse()]
%% @doc <p>Return the list of subscriptions as an XMPP response.</p>
get_subscriptions(Host, JID, Plugins) when is_list(Plugins) ->
    Result = lists:foldl(
	       fun(Type, {Status, Acc}) ->
		       Features = features(Type),
		       RetrieveFeature = lists:member("retrieve-subscriptions", Features),
		       if
			   not RetrieveFeature ->
			       %% Service does not support retreive subscriptions
			       {{error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "retrieve-subscriptions")}, Acc};
			   true ->
			       {result, Subscriptions} = node_action(Type, get_entity_subscriptions, [Host, JID]),
			       {Status, [Subscriptions|Acc]}
		       end
	       end, {ok, []}, Plugins),
    case Result of
	{ok, Subscriptions} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({Node, Subscription}) ->
				 [{xmlelement, "subscription",
				   [{"node", node_to_string(Node)},
				    {"subscription", subscription_to_string(Subscription)}],
				   []}];
			    ({_, none, _}) -> [];
			    ({Node, Subscription, SubJID}) ->
				 [{xmlelement, "subscription",
				   [{"node", node_to_string(Node)},
				    {"jid", jlib:jid_to_string(SubJID)},
				    {"subscription", subscription_to_string(Subscription)}],
				   []}]
			 end, lists:usort(lists:flatten(Subscriptions))),
	    {result, [{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}],
		       [{xmlelement, "subscriptions", [],
			 Entities}]}]};
	{Error, _} ->
	    Error
    end;
get_subscriptions(Host, Node, JID) ->
    Action = fun(#pubsub_node{type = Type}) ->
		     Features = features(Type),
		     RetrieveFeature = lists:member("manage-subscriptions", Features),
		     Affiliation = node_call(Type, get_affiliation, [Host, Node, JID]),
		     if
			 not RetrieveFeature ->
			     %% Service does not support manage subscriptions
			     {error, extended_error(?ERR_FEATURE_NOT_IMPLEMENTED, unsupported, "manage-affiliations")};
			 Affiliation /= {result, owner} ->
			     %% Entity is not an owner
			     {error, ?ERR_FORBIDDEN};
			 true ->
			     node_call(Type, get_node_subscriptions, [Host, Node])
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{error, Reason} ->
	    {error, Reason};
	{result, []} ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	{result, Subscriptions} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
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
		       [{xmlelement, "subscriptions", [{"node", node_to_string(Node)}],
			 Entities}]}]}
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
				  if
				      (JID == error) or
				      (Subscription == false) ->
					  error;
				      true ->
					  [{jlib:jid_tolower(JID), Subscription} | Acc]
				  end
			  end
		  end
	  end, [], EntitiesEls),
    case Entities of
	error ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    Action = fun(#pubsub_node{type = Type, owners = Owners}) ->
			     case lists:member(Owner, Owners) of
				 true ->
				     lists:foreach(fun({JID, Subscription}) ->
							   node_call(Type, set_subscription, [Host, Node, JID, Subscription])
						   end, Entities),
				     {result, []};
				 _ ->
				     {error, ?ERR_FORBIDDEN}
			     end
		     end,
	    transaction(Host, Node, Action, sync_dirty)
    end.


%% @spec (OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, SubscriberResource}, AllowedGroups)
%%    -> {PresenceSubscription, RosterGroup}
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
    {PresenceSubscription, RosterGroup}.

%% @spec (AffiliationStr) -> Affiliation
%%	 AffiliationStr = string()
%%	 Affiliation = atom()
%% @doc <p>Convert an affiliation type from string to atom.</p>
string_to_affiliation("owner") -> owner;
string_to_affiliation("publisher") -> publisher;
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
node_to_string([]) -> "/";
node_to_string(Node) ->
    case Node of
	[[_ | _] | _] -> string:strip(lists:flatten(["/", lists:map(fun(S) -> [S, "/"] end, Node)]), right, $/);
	[Head | _] when is_integer(Head) -> Node
    end.
string_to_node(SNode) ->
    string:tokens(SNode, "/").

%% @spec (Host) -> jid()
%%	Host = host()
%% @doc <p>Generate pubsub service JID.</p>
service_jid(Host) ->
    case Host of 
    {U,S,_} -> {jid, U, S, "", U, S, ""}; 
    _ -> {jid, "", Host, "", "", Host, ""}
    end.

%% @spec (LJID, Subscription, PresenceDelivery) -> boolean()
%%	LJID = jid()
%%	Subscription = atom()
%%	PresenceDelivery = boolean()
%% @doc <p>Check if a notification must be delivered or not.</p>
is_to_delivered(_, none, _) -> false;
is_to_delivered(_, pending, _) -> false;
is_to_delivered(_, _, false) -> true;
is_to_delivered({User, Server, _}, _, true) ->
    case mnesia:dirty_match_object({session, '_', '_', {User, Server}, '_', '_'}) of
    [] -> false;
    Ss ->
	lists:foldl(fun({session, _, _, _, undefined, _}, Acc) -> Acc;
	               ({session, _, _, _, _Priority, _}, _Acc) -> true
	end, false, Ss)
    end.

%%%%%% broadcast functions

broadcast_publish_item(Host, Node, ItemId, _From, Payload) ->
    Action =
	fun(#pubsub_node{options = Options, type = Type}) ->
		case node_call(Type, get_states, [Host, Node]) of
		    {error, _} -> {result, false};
		    {result, []} -> {result, false};
		    {result, States} ->
			PresenceDelivery = get_option(Options, presence_based_delivery),
			BroadcastAll = get_option(Options, broadcast_all_resources),
			Content = case get_option(Options, deliver_payloads) of
			    true -> Payload;
			    false -> []
			end,
			ItemAttrs = case ItemId of
			    "" -> [];
			    _ -> [{"id", ItemId}]
			end,
			Stanza = make_stanza(Node, ItemAttrs, Content),
			lists:foreach(
			  fun(#pubsub_state{stateid = {LJID, _},
					    subscription = Subscription}) ->
				case is_to_delivered(LJID, Subscription, PresenceDelivery) of
				    true ->
					DestJIDs = case BroadcastAll of
					    true -> ejabberd_sm:get_user_resources(element(1, LJID), element(2, LJID));
					    false -> [LJID]
					end,
					route_stanza(Host, DestJIDs, Stanza);
				    false ->
					ok
				end
			  end, States),
			broadcast_by_caps(Host, Node, Type, Stanza),
			{result, true}
		end
	end,
    transaction(Host, Node, Action, sync_dirty).

%% ItemAttrs is a list of tuples:
%% For example: [{"id", ItemId}]
make_stanza(Node, ItemAttrs, Payload) ->
    {xmlelement, "message", [],
     [{xmlelement, "event",
       [{"xmlns", ?NS_PUBSUB_EVENT}],
       [{xmlelement, "items", [{"node", node_to_string(Node)}],
	 [{xmlelement, "item", ItemAttrs, Payload}]}]}]}.

%% DestJIDs = [{LUser, LServer, LResource}]
route_stanza(Host, DestJIDs, Stanza) ->
    lists:foreach(
      fun(DestJID) ->
	      ejabberd_router ! {route, service_jid(Host), jlib:make_jid(DestJID), Stanza}
      end, DestJIDs). 

broadcast_retract_item(Host, Node, ItemId) ->
    broadcast_retract_item(Host, Node, ItemId, false).
broadcast_retract_item(Host, Node, ItemId, ForceNotify) ->
    Action =
	fun(#pubsub_node{options = Options, type = Type}) ->
		case node_call(Type, get_states, [Host, Node]) of
		    {error, _} -> {result, false};
		    {result, []} -> {result, false};
		    {result, States} ->
			Notify = case ForceNotify of
				     true -> true;
				     _ -> get_option(Options, notify_retract)
				 end,
			ItemAttrs = case ItemId of
			    "" -> [];
			    _ -> [{"id", ItemId}]
			end,
			Stanza = {xmlelement, "message", [],
				   [{xmlelement, "event",
				     [{"xmlns", ?NS_PUBSUB_EVENT}],
				       [{xmlelement, "items", [{"node", node_to_string(Node)}],
				         [{xmlelement, "retract", ItemAttrs, []}]}]}]},
			case Notify of
			    true ->
				lists:foreach(
				  fun(#pubsub_state{stateid = {JID, _},
						    subscription = Subscription}) ->
					if (Subscription /= none) and
					   (Subscription /= pending) ->
					    ejabberd_router ! {route, service_jid(Host), jlib:make_jid(JID), Stanza};
					   true ->
					    ok
					end
				  end, States),
				broadcast_by_caps(Host, Node, Type, Stanza),
				{result, true};
			    false ->
				{result, false}
			end
		end
	end,
    transaction(Host, Node, Action, sync_dirty).

broadcast_purge_node(Host, Node) ->
    Action =
	fun(#pubsub_node{options = Options, type = Type}) ->
		case node_call(Type, get_states, [Host, Node]) of
		    {error, _} -> {result, false};
		    {result, []} -> {result, false};
		    {result, States} ->
			Stanza = {xmlelement, "message", [],
				   [{xmlelement, "event",
				     [{"xmlns", ?NS_PUBSUB_EVENT}],
				       [{xmlelement, "purge", [{"node", node_to_string(Node)}],
				         []}]}]},
			case get_option(Options, notify_retract) of
			    true ->
				lists:foreach(
				  fun(#pubsub_state{stateid = {JID,_},
						    subscription = Subscription}) ->
					if (Subscription /= none) and
					   (Subscription /= pending) ->
						ejabberd_router ! {route, service_jid(Host), jlib:make_jid(JID), Stanza};
					   true ->
					    ok
					end
				  end, States),
				broadcast_by_caps(Host, Node, Type, Stanza),
				{result, true};
			    false ->
				{result, false}
			end
		end
	end,
    transaction(Host, Node, Action, sync_dirty).

broadcast_removed_node(Host, Removed) ->
    lists:foreach(
      fun(Node) ->
	      Action =
		  fun(#pubsub_node{options = Options, type = Type}) ->
			Stanza = {xmlelement, "message", [],
				 [{xmlelement, "event", [{"xmlns", ?NS_PUBSUB_EVENT}],
				   [{xmlelement, "delete", [{"node", node_to_string(Node)}],
				     []}]}]},
			case get_option(Options, notify_delete) of
			    true ->
				case node_call(Type, get_states, [Host, Node]) of
				    {result, States} ->
					lists:foreach(
					    fun(#pubsub_state{stateid = {JID, _},
						subscription = Subscription}) ->
					    if (Subscription /= none) and
					       (Subscription /= pending) ->
						ejabberd_router ! {route, service_jid(Host), jlib:make_jid(JID), Stanza};
					       true ->
						ok
					    end
					end, States),
					broadcast_by_caps(Host, Node, Type, Stanza),
					{result, true};
				    _ ->
					{result, false}
				end;
			    _ ->
				{result, false}
			end
		  end,
	      transaction(Host, Node, Action, sync_dirty)
      end, Removed).

broadcast_config_notification(Host, Node, Lang) ->
    Action =
	fun(#pubsub_node{options = Options, owners = Owners, type = Type}) ->
		case node_call(Type, get_states, [Host, Node]) of
		    {error, _} -> {result, false};
		    {result, []} -> {result, false};
		    {result, States} ->
			case get_option(Options, notify_config) of
			    true ->
				PresenceDelivery = get_option(Options, presence_based_delivery),
				Content = case get_option(Options, deliver_payloads) of
				    true ->
					[{xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
					get_configure_xfields(Type, Options, Lang, Owners)}];
				    false ->
					[]
				end,
				Stanza = {xmlelement, "message", [],
					   [{xmlelement, "event", [{"xmlns", ?NS_PUBSUB_EVENT}],
					     [{xmlelement, "items", [{"node", node_to_string(Node)}],
					       [{xmlelement, "item", [{"id", "configuration"}],
					         Content}]}]}]},
				lists:foreach(
				  fun(#pubsub_state{stateid = {LJID, _},
						    subscription = Subscription}) ->
					case is_to_delivered(LJID, Subscription, PresenceDelivery) of
					    true ->
						ejabberd_router ! {route, service_jid(Host), jlib:make_jid(LJID), Stanza};
					    false ->
						ok
					end
				  end, States),
				broadcast_by_caps(Host, Node, Type, Stanza),
				{result, true};
			    _ ->
				{result, false}
			end
		end
	end,
    transaction(Host, Node, Action, sync_dirty).

%TODO: simplify broadcast_* using a generic function like that:
%broadcast(Host, Node, Fun) ->
%	transaction(fun() ->
%		case tree_call(Host, get_node, [Host, Node]) of
%		#pubsub_node{options = Options, owners = Owners, type = Type} ->
%			case node_call(Type, get_states, [Host, Node]) of
%			{error, _} -> {result, false};
%			{result, []} -> {result, false};
%			{result, States} ->
%				lists:foreach(fun(#pubsub_state{stateid = {JID,_}, subscription = Subscription}) ->
%					Fun(Host, Node, Options, Owners, JID, Subscription)
%				end, States),
%				{result, true}
%			end;
%		Other ->
%			Other
%		end
%	end, sync_dirty).


%% broadcast Stanza to all contacts of the user that are advertising
%% interest in this kind of Node.
broadcast_by_caps({LUser, LServer, LResource}, Node, _Type, Stanza) ->
    ?DEBUG("looking for pid of ~p@~p/~p", [LUser, LServer, LResource]),
    %% We need to know the resource, so we can ask for presence data.
    SenderResource = case LResource of
			 "" ->
			     %% If we don't know the resource, just pick one.
			     case ejabberd_sm:get_user_resources(LUser, LServer) of
				 [R|_] ->
				     R;
				 [] ->
				     ""
			     end;
			 _ ->
			     LResource
		     end,
    case SenderResource of
    "" ->
	?DEBUG("~p@~p is offline; can't deliver ~p to contacts", [LUser, LServer, Stanza]),
	ok;
    _ ->
	case ejabberd_sm:get_session_pid(LUser, LServer, SenderResource) of
	    C2SPid when is_pid(C2SPid) ->
		%% set the from address on the notification to the bare JID of the account owner
		%% Also, add "replyto" if entity has presence subscription to the account owner
		%% See XEP-0163 1.1 section 4.3.1
		Sender = jlib:make_jid(LUser, LServer, ""),
		%%ReplyTo = jlib:make_jid(LUser, LServer, SenderResource),  % This has to be used
		case catch ejabberd_c2s:get_subscribed_and_online(C2SPid) of
		    ContactsWithCaps when is_list(ContactsWithCaps) ->
			?DEBUG("found contacts with caps: ~p", [ContactsWithCaps]),
			lists:foreach(
			fun({JID, Caps}) ->
				case is_caps_notify(LServer, Node, Caps) of
				    true ->
					To = jlib:make_jid(JID),
					ejabberd_router ! {route, Sender, To, Stanza};
				    false ->
					ok
				end
			end, ContactsWithCaps);
		    _ ->
			ok
		end,
		ok;
	    _ ->
		?DEBUG("~p@~p has no session; can't deliver ~p to contacts", [LUser, LServer, Stanza]),
		ok
	end
    end;
broadcast_by_caps(_, _, _, _) ->
    ok.

is_caps_notify(Host, Node, Caps) ->
    case catch mod_caps:get_features(Host, Caps) of
	Features when is_list(Features) -> lists:member(Node ++ "+notify", Features);
	_ -> false
    end.

%%%%%%% Configuration handling

%%<p>There are several reasons why the default node configuration options request might fail:</p>
%%<ul>
%%<li>The service does not support node configuration.</li>
%%<li>The service does not support retrieval of default node configuration.</li>
%%</ul>
get_configure(Host, Node, From, Lang) ->
    Action =
	fun(#pubsub_node{options = Options, owners = Owners, type = Type}) ->
		case node_call(Type, get_affiliation, [Host, Node, From]) of
		    {result, owner} ->
			{result,
			 [{xmlelement, "pubsub",
			   [{"xmlns", ?NS_PUBSUB_OWNER}],
			   [{xmlelement, "configure",
			     [{"node", node_to_string(Node)}],
			     [{xmlelement, "x",
			       [{"xmlns", ?NS_XDATA}, {"type", "form"}],
			       get_configure_xfields(Type, Options, Lang, Owners)
			      }]}]}]};
		    _ ->
			{error, ?ERR_FORBIDDEN}
		end
	end,
    transaction(Host, Node, Action, sync_dirty).

get_default(Host, Node, _From, Lang) ->
    Type=select_type(Host, Host, Node),
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

%% @spec (Options) -> MaxItems
%%	 Options = [Option]
%%	 Option = {Key::atom(), Value::term()}
%%	 MaxItems = integer() | unlimited
%% @doc <p>Return the maximum number of items for a given node.</p>
%% <p>Unlimited means that there is no limit in the number of items that can
%% be stored.</p>
%% @todo In practice, the current data structure means that we cannot manage
%% millions of items on a given node. This should be addressed in a new
%% version.
max_items(Options) ->
    case get_option(Options, persist_items) of
	true ->
	    case get_option(Options, max_items) of
		false -> unlimited;
		Result when (Result < 0) -> 0;
		Result -> Result
	    end;
	false ->
	    case get_option(Options, send_last_published_item) of
		never -> 0;
		_ -> 1
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

get_configure_xfields(_Type, Options, Lang, _Owners) ->
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
     {xmlelement, "field", [{"type", "text-multi"},
			    {"label", translate:translate(Lang, "Roster groups allowed to subscribe")},
			    {"var", "pubsub#roster_groups_allowed"}],
      [{xmlelement, "value", [], [{xmlcdata, Value}]} ||
	  Value <- get_option(Options, roster_groups_allowed, [])]},
     ?ALIST_CONFIG_FIELD("Specify the publisher model", publish_model,
			 [publishers, subscribers, open]),
     ?INTEGER_CONFIG_FIELD("Max payload size in bytes", max_payload_size),
     ?ALIST_CONFIG_FIELD("When to send the last published item", send_last_published_item,
			 [never, on_sub, on_sub_and_presence]),
     ?BOOL_CONFIG_FIELD("Only deliver notifications to available users", presence_based_delivery)
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
			fun(#pubsub_node{options = Options, type = Type}=N) ->
				case node_call(Type, get_affiliation,
					       [Host, Node, From]) of
				    {result, owner} ->
					case jlib:parse_xdata_submit(XEl) of
					    invalid ->
						{error, ?ERR_BAD_REQUEST};
					    XData ->
						OldOpts = case Options of
							      [] -> node_options(Type);
							      _ -> Options
							  end,
						case set_xoption(XData, OldOpts) of
						    NewOpts when is_list(NewOpts) ->
							tree_call(Host, set_node,
								  [N#pubsub_node{options = NewOpts}]),
							{result, ok};
						    Err ->
							Err
						end
					end;
				    _ ->
					{error, ?ERR_FORBIDDEN}
				end
			end,
		    case transaction(Host, Node, Action, transaction) of
			{result, ok} ->
			    broadcast_config_notification(Host, Node, Lang),
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
	    _ -> set_xoption(Opts, add_opt(Opt, BoolVal, NewOpts))
	end).

-define(SET_STRING_XOPT(Opt, Val),
	set_xoption(Opts, add_opt(Opt, Val, NewOpts))).

-define(SET_INTEGER_XOPT(Opt, Val, Min, Max),
	case catch list_to_integer(Val) of
	    IVal when is_integer(IVal),
	    IVal >= Min,
	    IVal =< Max ->
		set_xoption(Opts, add_opt(Opt, IVal, NewOpts));
	    _ ->
		{error, ?ERR_NOT_ACCEPTABLE}
	end).

-define(SET_ALIST_XOPT(Opt, Val, Vals),
	case lists:member(Val, [atom_to_list(V) || V <- Vals]) of
	    true -> set_xoption(Opts, add_opt(Opt, list_to_atom(Val), NewOpts));
	    false -> {error, ?ERR_NOT_ACCEPTABLE}
	end).

-define(SET_LIST_XOPT(Opt, Val),
	set_xoption(Opts, add_opt(Opt, Val, NewOpts))).

set_xoption([], NewOpts) ->
    NewOpts;
set_xoption([{"FORM_TYPE", _} | Opts], NewOpts) ->
    set_xoption(Opts, NewOpts);
set_xoption([{"pubsub#roster_groups_allowed", _Value} | Opts], NewOpts) ->
    ?SET_LIST_XOPT(roster_groups_allowed, []);  % XXX: waiting for EJAB-659 to be solved
set_xoption([{"pubsub#deliver_payloads", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_payloads, Val);
set_xoption([{"pubsub#deliver_notifications", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_notifications, Val);
set_xoption([{"pubsub#notify_config", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_config, Val);
set_xoption([{"pubsub#notify_delete", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_delete, Val);
set_xoption([{"pubsub#notify_retract", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_retract, Val);
set_xoption([{"pubsub#persist_items", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(persist_items, Val);
set_xoption([{"pubsub#max_items", [Val]} | Opts], NewOpts) ->
    ?SET_INTEGER_XOPT(max_items, Val, 0, ?MAXITEMS);
set_xoption([{"pubsub#subscribe", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(subscribe, Val);
set_xoption([{"pubsub#access_model", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(access_model, Val, [open, authorize, presence, roster, whitelist]);
set_xoption([{"pubsub#publish_model", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(publish_model, Val, [publishers, subscribers, open]);
set_xoption([{"pubsub#node_type", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(node_type, Val, [leaf, collection]);
set_xoption([{"pubsub#max_payload_size", [Val]} | Opts], NewOpts) ->
    ?SET_INTEGER_XOPT(max_payload_size, Val, 0, ?MAX_PAYLOAD_SIZE);
set_xoption([{"pubsub#send_last_published_item", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(send_last_published_item, Val, [never, on_sub, on_sub_and_presence]);
set_xoption([{"pubsub#presence_based_delivery", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(presence_based_delivery, Val);
set_xoption([{"pubsub#title", Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(title, Value);
set_xoption([{"pubsub#type", Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(type, Value);
set_xoption([{"pubsub#body_xslt", Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(body_xslt, Value);
set_xoption([_ | _Opts], _NewOpts) ->
    {error, ?ERR_NOT_ACCEPTABLE}.

%%%% plugin handling

plugins(Host) ->
    case ets:lookup(gen_mod:get_module_proc(Host, pubsub_state), plugins) of
    [{plugins, PL}] -> PL;
    _ -> [?STDNODE]
    end.
select_type(ServerHost, Host, Node, Type)->
	?DEBUG("SELECT_TYPE : ~p~n", [Node]),
	case Host of
	{_User, _Server, _Resource} -> 
   		case ets:lookup(gen_mod:get_module_proc(ServerHost, pubsub_state), pep_mapping) of
   	 		[{pep_mapping, PM}] -> ?DEBUG("SELECT_TYPE : ~p~n", [PM]), proplists:get_value(Node, PM,?PEPNODE);
   	 		R -> ?DEBUG("SELECT_TYPE why ?: ~p~n", [R]), ?PEPNODE
   	 	end;
	_ -> 
	   Type
    end.
select_type(ServerHost, Host, Node) -> 
 	select_type(ServerHost, Host, Node,hd(plugins(ServerHost))).

features() ->
	[
	 %"access-authorize",   % OPTIONAL
	 "access-open",   % OPTIONAL this relates to access_model option in node_default
	 "access-presence",   % OPTIONAL this relates to access_model option in node_pep
	 %"access-roster",   % OPTIONAL
	 %"access-whitelist",   % OPTIONAL
	 % see plugin "auto-create",   % OPTIONAL
	 % see plugin "auto-subscribe",   % RECOMMENDED
	 "collections",   % RECOMMENDED
	 "config-node",   % RECOMMENDED
	 "create-and-configure",   % RECOMMENDED
	 % see plugin "create-nodes",   % RECOMMENDED
	 %TODO "delete-any",   % OPTIONAL
	 % see plugin "delete-nodes",   % RECOMMENDED
	 % see plugin "filtered-notifications",   % RECOMMENDED
	 %TODO "get-pending",   % OPTIONAL
	 % see plugin "instant-nodes",   % RECOMMENDED
	 "item-ids",   % RECOMMENDED
	 "last-published",   % RECOMMENDED
	 %TODO "cache-last-item",
	 %TODO "leased-subscription",   % OPTIONAL
	 % see plugin "manage-subscriptions",   % OPTIONAL
	 %TODO "member-affiliation",   % RECOMMENDED
	 %TODO "meta-data",   % RECOMMENDED
	 % see plugin "modify-affiliations",   % OPTIONAL
	 %TODO "multi-collection",   % OPTIONAL
	 %TODO "multi-subscribe",   % OPTIONAL
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
	 % see plugin "subscribe",   % REQUIRED
	 %TODO "subscription-options",   % OPTIONAL
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
    {result, Features} = node_action(Host, Node, features, []),
    lists:usort(features() ++ Features).

%% @doc <p>node tree plugin call.</p>
tree_call({_User, Server, _Resource}, Function, Args) ->
    tree_call(Server, Function, Args);
tree_call(Host, Function, Args) ->
    Module = case ets:lookup(gen_mod:get_module_proc(Host, pubsub_state), nodetree) of
	[{nodetree, N}] -> N;
	_ -> list_to_atom(?TREE_PREFIX ++ ?STDNODE)
    end,
    catch apply(Module, Function, Args).
tree_action(Host, Function, Args) ->
    Fun = fun() -> tree_call(Host, Function, Args) end,
    catch mnesia:sync_dirty(Fun).

%% @doc <p>node plugin call.</p>
node_call(Type, Function, Args) ->
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

node_action(Type, Function, Args) ->
    transaction(fun() ->
			node_call(Type, Function, Args)
		end, sync_dirty).
node_action(Host, Node, Function, Args) ->
    transaction(fun() ->
			case tree_call(Host, get_node, [Host, Node]) of
			    #pubsub_node{type=Type} -> node_call(Type, Function, Args);
			    Other -> Other
			end
		end, sync_dirty).

%% @doc <p>plugin transaction handling.</p>
transaction(Host, Node, Action, Trans) ->
    transaction(fun() ->
			case tree_call(Host, get_node, [Host, Node]) of
			    Record when is_record(Record, pubsub_node) -> Action(Record);
			    Other -> Other
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
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
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
    extended_error(Error, Ext, [{"xmlns", ?NS_PUBSUB_ERRORS}]).
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

%% @doc Return the name of a given node if available.
get_item_name(Host, Node, Id) ->
    {result, Name} = node_action(Host, Node, get_item_name, [Host, Node, Id]),
    Name.

