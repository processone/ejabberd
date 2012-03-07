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
-version('1.13-1').

-behaviour(gen_server).
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("adhoc.hrl").
-include("pubsub.hrl").
-include("mod_roster.hrl").

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

-record(state,
	{
	  server_host                               :: string(),
	  host                                      :: string(),
	  access                                    :: atom(),
	  pep_mapping             = []              :: [{Namespace::string(), NodeId::string()}],
	  ignore_pep_from_offline = true            :: boolean(),
	  last_item_cache         = false           :: boolean(),
	  max_items_node          = ?MAXITEMS       :: integer(),
	  nodetree                = 'nodetree_tree' :: atom(),
	  plugins                 = [?STDNODE]      :: [Plugin::nodeType()]
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec(start_link/2 ::
      (
		   Host :: string(),
		   Opts :: [{Option::atom(), Value::term()}])
      -> {'ok', pid()} | 'ignore' | {'error', _}
	    ).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).


-spec(start/2 ::
      (
	      Host :: string(),
	      Opts :: [{Option::atom(), Value::term()}])
      -> {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}
	    ).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc,
		 {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).


-spec(stop/1 ::
      (
	     Host :: string())
      -> 'ok' | {'error','not_found' | 'running' | 'simple_one_for_one'}
	    ).

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
	     Args :: [ServerHost::string() | [{Option::atom(), Value::term()}]])
      -> {'ok', State::#state{}}
	    ).

init([ServerHost, Opts]) ->
    ?DEBUG("pubsub init ~p ~p",[ServerHost,Opts]),
    Host = gen_mod:get_opt_host(ServerHost, Opts, "pubsub.@HOST@"),
    Access = gen_mod:get_opt('access_createnode', Opts, 'all'),
    PepOffline = gen_mod:get_opt('ignore_pep_from_offline', Opts, true),
    IQDisc = gen_mod:get_opt('iqdisc', Opts, 'one_queue'),
    LastItemCache = gen_mod:get_opt('last_item_cache', Opts, false),
    MaxItemsNode = gen_mod:get_opt('max_items_node', Opts, ?MAXITEMS),
    ServerHostB = list_to_binary(ServerHost),
    pubsub_index:init(Host, ServerHost, Opts),
    ets:new(gen_mod:get_module_proc(Host, 'config'), ['set', 'named_table']),
    ets:new(gen_mod:get_module_proc(ServerHost, 'config'), ['set', 'named_table']),
    {Plugins, NodeTree, PepMapping} = init_plugins(Host, ServerHost, Opts),
    mnesia:create_table(pubsub_last_item, [{ram_copies, [node()]}, {attributes, record_info(fields, pubsub_last_item)}]),
    mod_disco:register_feature(ServerHostB, ?NS_PUBSUB_s),
    ets:insert(gen_mod:get_module_proc(Host, 'config'), {'nodetree', NodeTree}),
    ets:insert(gen_mod:get_module_proc(Host, 'config'), {'plugins', Plugins}),
    ets:insert(gen_mod:get_module_proc(Host, 'config'), {'last_item_cache', LastItemCache}),
    ets:insert(gen_mod:get_module_proc(Host, 'config'), {'max_items_node', MaxItemsNode}),
    ets:insert(gen_mod:get_module_proc(ServerHost, 'config'), {'nodetree', NodeTree}),
    ets:insert(gen_mod:get_module_proc(ServerHost, 'config'), {'plugins', Plugins}),
    ets:insert(gen_mod:get_module_proc(ServerHost, 'config'), {'last_item_cache', LastItemCache}),
    ets:insert(gen_mod:get_module_proc(ServerHost, 'config'), {'max_items_node', MaxItemsNode}),
    ets:insert(gen_mod:get_module_proc(ServerHost, 'config'), {'pep_mapping', PepMapping}),
    ets:insert(gen_mod:get_module_proc(ServerHost, 'config'), {'ignore_pep_from_offline', PepOffline}),
    ets:insert(gen_mod:get_module_proc(ServerHost, 'config'), {'host', Host}),
    ejabberd_hooks:add(sm_remove_connection_hook, ServerHostB, ?MODULE, on_user_offline, 75),
    ejabberd_hooks:add(disco_local_identity, ServerHostB, ?MODULE, disco_local_identity, 75),
    ejabberd_hooks:add(disco_local_features, ServerHostB, ?MODULE, disco_local_features, 75),
    ejabberd_hooks:add(disco_local_items, ServerHostB, ?MODULE, disco_local_items, 75),
    ejabberd_hooks:add(presence_probe_hook, ServerHostB, ?MODULE, presence_probe, 80),
    ejabberd_hooks:add(roster_in_subscription, ServerHostB, ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, ServerHostB, ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(remove_user, ServerHostB, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, ServerHostB, ?MODULE, remove_user, 50),
    case lists:member(?PEPNODE, Plugins) of
	true ->
	    ejabberd_hooks:add(caps_update, ServerHostB, ?MODULE, caps_update, 80),
	    ejabberd_hooks:add(disco_sm_identity, ServerHostB, ?MODULE, disco_sm_identity, 75),
	    ejabberd_hooks:add(disco_sm_features, ServerHostB, ?MODULE, disco_sm_features, 75),
	    ejabberd_hooks:add(disco_sm_items, ServerHostB, ?MODULE, disco_sm_items, 75),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB, ?MODULE, iq_sm, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB_OWNER, ?MODULE, iq_sm, IQDisc);
	false ->
	    ok
    end,
    ejabberd_router:register_route(Host),
    put(server_host, ServerHost),
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


-spec(init_send_loop/2 ::
      (
		       ServerHost :: string(),
		       State      :: #state{})
      -> pid()
	    ).

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
%% <p>See {@link node_flat:init/1} for an example implementation.</p>
-spec(init_plugins/3 ::
      (
		     Host       :: string(),
		     ServerHost :: string(),
		     Opts       :: [{Option::atom(), Value::term()}])
      -> {Plugins    :: [Plugin::nodeType()],
	  TreePlugin :: atom(),
	  PepMapping :: [{Namespace::string(), NodeId::string()}]}
	    ).

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


-spec(terminate_plugins/4 ::
      (
			  Host       :: string(),
			  ServerHost :: string(),
			  Plugins    :: [Plugin::nodeType()],
			  TreePlugin :: atom())
      -> 'ok'
	    ).

terminate_plugins(Host, ServerHost, Plugins, TreePlugin) ->
    lists:foreach(fun(Name) ->
			  ?DEBUG("** terminate ~s plugin",[Name]),
			  Plugin = list_to_atom(?PLUGIN_PREFIX++Name),
			  Plugin:terminate(Host, ServerHost)
		  end, Plugins),
    TreePlugin:terminate(Host, ServerHost),
    ok.


-spec(init_nodes/4 ::
      (
		   Host       :: string(),
		   ServerHost :: string(),
		   NodeTree   :: atom(),
		   Plugins    :: [Plugin::nodeType()])
      -> 'ok'
	    ).

init_nodes(Host, ServerHost, _NodeTree, Plugins) ->
    %% TODO, this call should be done plugin side
    case lists:member("hometree_odbc", Plugins) of
	true ->
	    create_node(Host, ServerHost, string_to_node("/home"), service_jid(Host), "hometree_odbc"),
	    create_node(Host, ServerHost, string_to_node("/home/" ++ ServerHost), service_jid(Host), "hometree_odbc");
	false ->
	    ok
    end.


-spec(send_loop/1 ::
      (
		  State::#state{})
      -> 'ok'
	    ).

send_loop(State) ->
    receive
	{'presence', #jid{node = User, domain = Server, resource = Resource} = JID, Pid} ->
	    Host = State#state.host,
	    ServerHost = State#state.server_host,
	    LJID = {User,Server,Resource},
	    BJID = {User,Server,undefined},
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
						    #pubsub_node{id = {H, NodeId}, type = Type, idx = NodeIdx, options = Options} = Node,
						    case get_option(Options, 'send_last_published_item') of
							'on_sub_and_presence' ->
							    send_items(H, NodeId, NodeIdx, Type, LJID, 'last');
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
		    case catch ejabberd_c2s:get_subscribed(Pid) of
			Contacts when is_list(Contacts) ->
			    lists:foreach(
			      fun({U, S, R}) ->
				      case S of
					  ServerHost ->  %% local contacts
					      case user_resources(U, S) of
						  [] -> %% offline
						      PeerJID = exmpp_jid:make(U, S, R),
						      self() ! {'presence', User, Server, [Resource], PeerJID};
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
		    end;
	       true ->
		    ok
	    end,
	    send_loop(State);
	{'presence', User, Server, Resources, #jid{node = U, domain = S} = JID} ->
	    %% get resources caps and check if processing is needed
	    spawn(fun() ->
			  Host = State#state.host,
			  Owner = {U,S,undefined},
			  lists:foreach(fun(#pubsub_node{id = {_, NodeId}, type = Type, idx = NodeIdx, options = Options}) ->
						case get_option(Options, 'send_last_published_item') of
						    'on_sub_and_presence' ->
							lists:foreach(fun(Resource) ->
									      LJID = {User, Server, Resource},
									      Subscribed = case get_option(Options, 'access_model') of
											       'open' -> true;
											       'presence' -> true;
											       'whitelist' -> false; % subscribers are added manually
											       'authorize' -> false; % likewise
											       'roster' ->
												   RosterGroups = get_option(Options, 'roster_groups_allowed', []),
												   element(2, get_roster_info(U, S, LJID, RosterGroups))
											   end,
									      if Subscribed -> send_items(Owner, NodeId, NodeIdx, Type, LJID, 'last');
										 true -> ok
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
-spec(disco_local_identity/5 ::
      (
			     Acc    :: [] | [Identity::#xmlel{}],
			     From   :: jidEntity(),
			     To     :: jidComponent(),
			     NodeId :: nodeId(),
			     Lang   :: binary())
      -> Identities :: [] | [Identity::#xmlel{}]
	    ).

disco_local_identity(Acc, _From, #jid{domain = Host} = _To, <<>> = _NodeId, _Lang) ->
    case lists:member(?PEPNODE, plugins(Host)) of
	true ->
	    [#xmlel{name = 'identity', ns = ?NS_DISCO_INFO,
		    attrs = [?XMLATTR(<<"category">>, <<"pubsub">>), ?XMLATTR(<<"type">>, <<"pep">>)]}
	     | Acc];
	false -> Acc
    end;
disco_local_identity(Acc, _From, _To, _NodeId, _Lang) ->
    Acc.


-spec(disco_local_features/5 ::
      (
			     Acc    :: 'empty' | {'result', Features :: [] | [Feature :: atom() | string() | binary()]},
			     From   :: jidEntity(),
			     To     :: jidComponent(),
			     NodeId :: nodeId(),
			     Lang   :: binary())
      -> {'result', Features :: [] | [Feature :: atom() | string() | binary()]}
	    ).

disco_local_features(Acc, _From, #jid{domain = Host} = _To, <<>> = _NodeId, _Lang) ->
    OtherFeatures = case Acc of
			{result, Features} -> Features;
			_ -> []
		    end,
    {result, OtherFeatures ++
     [?NS_PUBSUB_s++"#"++Feature || Feature <- features(Host, <<>>)]};
disco_local_features(Acc, _From, _To, _NodeId, _Lang) ->
    Acc.


-spec(disco_local_items/5 ::
      (
			  Acc    :: {'result', Items :: [] | [Item::#xmlel{}]},
			  From   :: jidEntity(),
			  To     :: jidComponent(),
			  NodeId :: nodeId(),
			  Lang   :: binary())
      -> {'result', Items :: [] | [Item::#xmlel{}]}
	     | {'error', _} %% TODO
	    ).

disco_local_items(Acc, _From, _To, <<>>, _Lang) ->
    Acc;
disco_local_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.


-spec(disco_sm_identity/5 ::
      (
			  Acc    :: [] | [Identity::#xmlel{}],
			  From   :: jidEntity(),
			  To     :: jidContact(),
			  NodeId :: nodeId(),
			  Lang   :: binary())
      -> Identities :: [] | [Identity::#xmlel{}]
	    ).

disco_sm_identity(Acc, From, To, NodeId, _Lang) ->
    disco_identity(To, NodeId, From) ++ Acc.


-spec(disco_identity/3 ::
      (
		       Host   :: jidContact(),
		       NodeId :: nodeId(),
		       From   :: jidEntity())
      -> Identities :: [] | [Identity::#xmlel{}]
	    ).

disco_identity(_Host, <<>> = _NodeId, _From) ->
    [#xmlel{name = 'identity', ns = ?NS_DISCO_INFO,
	    attrs = [?XMLATTR(<<"category">>, <<"pubsub">>), ?XMLATTR(<<"type">>, <<"pep">>)]}];
disco_identity(#jid{node = U, domain = S, resource = R} = Host, NodeId, From) ->
    Action = fun(#pubsub_node{idx = NodeIdx, type = Type, options = Options}) ->
		     Owners = node_owners_call(Type, NodeIdx),
		     case get_allowed_items_call(Host, NodeIdx, From, Type, Options, Owners) of
			 {result, _} ->
			     {result,
			      [#xmlel{name = 'identity', ns = ?NS_DISCO_INFO,
				      attrs = [?XMLATTR(<<"category">>, <<"pubsub">>), ?XMLATTR(<<"type">>, <<"pep">>)]},
			       #xmlel{name = 'identity', ns = ?NS_DISCO_INFO,
				      attrs = [?XMLATTR(<<"category">>, <<"pubsub">>), ?XMLATTR(<<"type">>, <<"leaf">>)
					       | case get_option(Options, 'title') of
						     false -> [];
						     Title -> [?XMLATTR(<<"name">>, Title)]
						 end
					      ]}]};
			 {error, _} -> {result, []}
		     end
	     end,
    case transaction({U,S,R}, NodeId, Action, sync_dirty) of
	{result, {_, Identities}} -> Identities;
	_ -> _Identities = []
    end.


-spec(disco_sm_features/5 ::
      (
			  Acc    :: 'empty' | {'result', Features :: [] | [Feature :: atom() | string() | binary()]},
			  From   :: jidEntity(),
			  To     :: jidContact(),
			  NodeId :: nodeId(),
			  Lang   :: binary())
      -> {'result', Features :: [] | [Feature :: atom() | string() | binary()]}
	    ).

disco_sm_features('empty' = _Acc, From, To, NodeId, Lang) ->
    disco_sm_features({result, []}, From, To, NodeId, Lang);
disco_sm_features({result, OtherFeatures} = _Acc, From, To, NodeId, _Lang) ->
    {result, disco_features(To, NodeId, From) ++ OtherFeatures}.


-spec(disco_features/3 ::
      (
		       Host   :: jidContact(),
		       NodeId :: nodeId(),
		       From   :: jidEntity())
      -> Features :: [] | [Feature :: atom() | string() | binary()]
	    ).

disco_features(_Host, <<>> = _NodeId, _From) ->
    [?NS_PUBSUB_s
     | [?NS_PUBSUB_s++"#"++Feature || Feature <- features("pep")]];
disco_features(#jid{node = U, domain = S, resource = R} = Host, NodeId, From) ->
    Action = fun(#pubsub_node{idx = NodeIdx, type = Type, options = Options}) ->
		     Owners = node_owners_call(Type, NodeIdx),
		     case get_allowed_items_call(Host, NodeIdx, From, Type, Options, Owners) of
			 {result, _} ->
			     {result, [?NS_PUBSUB_s
				       | [?NS_PUBSUB_s ++ "#" ++ Feature || Feature <- features("pep")]]};
			 _ -> {result, []}
		     end
	     end,
    case transaction({U,S,R}, NodeId, Action, sync_dirty) of
	{result, {_, Features}} -> Features;
	_ -> _Features = []
    end.


-spec(disco_sm_items/5 ::
      (
		       Acc    :: 'empty'
		       | {'result', Items :: [] | [Item::#xmlel{}]},
		       From   :: jidEntity(),
		       To     :: jidContact(),
		       NodeId :: nodeId(),
		       Lang   :: binary())
      -> {'result', Items :: [] | [Item::#xmlel{}]}
	    ).

disco_sm_items('empty' = _Acc, From, To, NodeId, Lang) ->
    disco_sm_items({result, []}, From, To, NodeId, Lang);
disco_sm_items({result, OtherItems} = _Acc, From, To, NodeId, _Lang) ->
    {result, disco_items(To, NodeId, From) ++ OtherItems}.


-spec(disco_items/3 ::
      (
		    Host   :: jidContact(),
		    NodeId :: nodeId(),
		    From   :: jidEntity())
      -> Items :: [] | [Item::#xmlel{}]
	    ).

disco_items(#jid{raw = JID, node = U, domain = S, resource = R} = Host, <<>>, From) ->
    Action = fun(#pubsub_node{id ={_, NodeId}, options = Options, type = Type, idx = NodeIdx}, Acc) ->
		     Owners = node_owners_call(Type, NodeIdx),
		     case get_allowed_items_call(Host, NodeIdx, From, Type, Options, Owners) of
			 {result, _} ->
			     [#xmlel{name = 'item', ns = ?NS_DISCO_INFO,
				     attrs = [?XMLATTR(<<"jid">>, JID),
					      ?XMLATTR(<<"node">>, NodeId) |
					      case get_option(Options, 'title') of
						  false   -> [];
						  [Title] -> [?XMLATTR(<<"title">>, Title)]
					      end]}
			      | Acc];
			 _ -> Acc
		     end
	     end,
    case transaction_on_nodes({U,S,R}, Action, sync_dirty) of
	{result, Items} -> Items
						%_ -> _Items = []
    end;

disco_items(#jid{raw = JID, node = U, domain = S, resource = R} = Host, NodeId, From) ->
    Action = fun(#pubsub_node{idx = NodeIdx, type = Type, options = Options}) ->
		     Owners = node_owners_call(Type, NodeIdx),
		     case get_allowed_items_call(Host, NodeIdx, From, Type, Options, Owners) of
			 {result, Items} ->
			     {result,
			      [#xmlel{name = 'item', ns = ?NS_DISCO_INFO,
				      attrs = [?XMLATTR(<<"jid">>, JID),
					       ?XMLATTR(<<"name">>, ItemId)]}
			       || #pubsub_item{id = {ItemId,_}} <- Items]};
			 _ -> {result, []}
		     end
	     end,
    case transaction({U,S,R}, NodeId, Action, sync_dirty) of
	{result, {_, Result}} -> Result;
	_ -> []
    end.

%% -------
%% presence hooks handling functions
%%
caps_update(From, To, _Features) ->
    Pid = ejabberd_sm:get_session_pid(From),
    presence_probe(From, To, Pid).

-spec(presence_probe/3 ::
      (
		       Peer :: jidEntity(),
		       JID  :: jidEntity(),
		       Pid  :: pid())
      -> 'ok'
	     | {'presence', JID::jidEntity(), Pid::pid()}
	     | {'presence', User::binary(), Server::binary(), [Resource::binary()], JID::jidEntity()}
	    ).

presence_probe(#jid{node = User, domain = Server, resource = Resource} = Peer, #jid{domain = Host} = JID, Pid) ->
    case exmpp_jid:full_compare(Peer, JID) of
	true -> %% JID are equals
	    presence(Server, {'presence', JID, Pid}),
	    presence(Server, {'presence', User, Server, [Resource], JID});
	false ->
	    case exmpp_jid:bare_compare(Peer, JID) of
		true ->
		    %% ignore presence_probe from other ressources for the current user
		    %% this way, we do not send duplicated last items if user already connected with other clients
		    ok;
		false ->
		    presence(Host, {'presence', User, Server, [Resource], JID})
	    end
    end.


-spec(presence/2 ::
      (
		 ServerHost :: string() | binary(),
		 Presence   :: {'presence', JID::jidEntity(), Pid::pid()}
		 | {'presence', User::binary(), Server::binary(), [Resource::binary()], JID::jidEntity()})
      -> {'presence', JID::jidEntity(), Pid::pid()}
	     | {'presence', User::binary(), Server::binary(), [Resource::binary()], JID::jidEntity()}
	    ).

presence(ServerHost, Presence) when is_binary(ServerHost) ->
    presence(binary_to_list(ServerHost), Presence);
presence(ServerHost, Presence) ->
    SendLoop = case whereis(gen_mod:get_module_proc(ServerHost, ?LOOPNAME)) of
		   undefined ->
						% in case send_loop process died, we rebuild a minimal State record and respawn it
		       Host = host(ServerHost),
		       Plugins = plugins(Host),
		       PepOffline = case catch ets:lookup(gen_mod:get_module_proc(ServerHost, 'config'), 'ignore_pep_from_offline') of
					[{'ignore_pep_from_offline', PO}] -> PO;
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
    Owner = exmpp_jid:make(User, Server, ""),
    {U, S, R} = jlib:short_prepd_jid(JID),
    Rs = case R of
	     undefined -> user_resources(U, S);
	     _ -> [R]
	 end,
    presence(Server, {presence, U, S, Rs, Owner}),
    true;
out_subscription(_, _, _, _) ->
    true.
in_subscription(_, User, Server, Owner, unsubscribed, _) ->
    unsubscribe_user(exmpp_jid:make(User, Server, ""), Owner),
    true;
in_subscription(_, _, _, _, _, _) ->
    true.

unsubscribe_user(Entity, Owner) ->
    BJID = jlib:short_prepd_bare_jid(Owner),
    Host = host(element(2, BJID)),
    spawn(fun() ->
		  lists:foreach(fun(PType) ->
					{result, Subscriptions} = node_action(Host, PType, get_entity_subscriptions, [Host, Entity]),
					lists:foreach(fun
						      ({#pubsub_node{options = Options, idx = Nidx}, subscribed, _, JID}) ->
							     case get_option(Options, access_model) of
								 presence ->
								     case lists:member(BJID, node_owners(Host, PType, Nidx)) of
									 true ->
									     node_action(Host, PType, unsubscribe_node, [Nidx, Entity, JID, all]);
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

%% @spec(User::binary(), Server::binary()) -> any()
remove_user(UserB, ServerB) ->
    User = binary_to_list(UserB),
    Server = binary_to_list(ServerB),
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    Entity = exmpp_jid:make(LUser, LServer),
    Host = host(LServer),
    HomeTreeBase = string_to_node("/home/"++LServer++"/"++LUser),
    spawn(fun() ->
		  %% remove user's subscriptions
		  lists:foreach(fun(PType) ->
					{result, Subscriptions} = node_action(Host, PType, get_entity_subscriptions, [Host, Entity]),
					lists:foreach(fun
						      ({#pubsub_node{idx = Nidx}, _, _, JID}) -> node_action(Host, PType, unsubscribe_node, [Nidx, Entity, JID, all])
						     end, Subscriptions),
					{result, Affiliations} = node_action(Host, PType, get_entity_affiliations, [Host, Entity]),
					lists:foreach(fun
						      ({#pubsub_node{id = {H, N}, parents = []}, owner}) -> delete_node(H, N, Entity);
						      ({#pubsub_node{id = {H, N}, type = "hometree"}, owner}) when N == HomeTreeBase -> delete_node(H, N, Entity);
						      ({#pubsub_node{idx = Nidx}, publisher}) -> node_action(Host, PType, set_affiliation, [Nidx, Entity, none]);
						      (_) -> ok
						     end, Affiliations)
				end, plugins(Host))
	  end).

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
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
-spec(handle_info/2 ::
      (
		    Info  :: {'route',
			      From   :: jidEntity(),
			      To     :: jidComponent(),
			      Packet :: #xmlel{}},
		    State :: #state{})
      -> {'noreply', State::#state{}}
	    ).
%% TODO : what to do when JID recipient contains a resource ?
handle_info({route, From,
	     #jid{node = undefined, domain = Host, resource = undefined} = To, Packet},
	    #state{server_host = ServerHost,
		   access = Access,
		   plugins = Plugins} = State) ->
    case catch do_route(list_to_binary(ServerHost), Access, Plugins, Host, From, To, Packet) of
	{'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]);
	_ -> ok
    end,
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}. %% TODO : handle other case with recipient full JID

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%% @private
-spec(terminate/2 ::
      (
		  Reason :: _,
		  State  :: #state{})
      -> 'ok'
	    ).

terminate(_Reason, #state{host = Host,
			  server_host = ServerHost,
			  nodetree = TreePlugin,
			  plugins = Plugins}) ->
    ejabberd_router:unregister_route(Host),
    ServerHostB = list_to_binary(ServerHost),
    case lists:member(?PEPNODE, Plugins) of
	true ->
	    ejabberd_hooks:delete(caps_update, ServerHostB, ?MODULE, caps_update, 80),
	    ejabberd_hooks:delete(disco_sm_identity, ServerHostB, ?MODULE, disco_sm_identity, 75),
	    ejabberd_hooks:delete(disco_sm_features, ServerHostB, ?MODULE, disco_sm_features, 75),
	    ejabberd_hooks:delete(disco_sm_items, ServerHostB, ?MODULE, disco_sm_items, 75),
	    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB),
	    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB_OWNER);
	false ->
	    ok
    end,
    ejabberd_hooks:delete(sm_remove_connection_hook, ServerHostB, ?MODULE, on_user_offline, 75),
    ejabberd_hooks:delete(disco_local_identity, ServerHostB, ?MODULE, disco_local_identity, 75),
    ejabberd_hooks:delete(disco_local_features, ServerHostB, ?MODULE, disco_local_features, 75),
    ejabberd_hooks:delete(disco_local_items, ServerHostB, ?MODULE, disco_local_items, 75),
    ejabberd_hooks:delete(presence_probe_hook, ServerHostB, ?MODULE, presence_probe, 80),
    ejabberd_hooks:delete(roster_in_subscription, ServerHostB, ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, ServerHostB, ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(remove_user, ServerHostB, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, ServerHostB, ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHostB, ?NS_PUBSUB_OWNER),
    mod_disco:unregister_feature(ServerHostB, ?NS_PUBSUB_s),
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
-spec(do_route/7 ::
      (
		 ServerHost :: binary(),
		 Access     :: atom(),
		 Plugins    :: [Plugin::nodeType()],
		 Host       :: hostPubsub(),
		 From       :: jidEntity(),
		 To         :: jidComponent(),
		 Packet     :: #xmlel{})
      -> any()
	    ).

%%% <iq/>
do_route(ServerHost, Access, Plugins, Host, From, To, #xmlel{name = 'iq'} = Packet) ->
    case exmpp_iq:xmlel_to_iq(Packet) of
	%% Service discovery : disco#info
	#iq{type = 'get', ns = ?NS_DISCO_INFO, payload = #xmlel{attrs = Attrs}, lang = Lang} ->
	    NodeId = exmpp_xml:get_attribute_from_list(Attrs, <<"node">>, <<>>),
	    Info = ejabberd_hooks:run_fold(
		     disco_info, ServerHost, [],
		     [ServerHost, ?MODULE, <<>>, ""]),
	    Res = case iq_disco_info(Host, NodeId, From, Lang) of
		      {result, IQRes} ->
			  Result = #xmlel{ns = ?NS_DISCO_INFO,
					  name = 'query',
					  attrs = Attrs,
					  children = IQRes++Info},
			  exmpp_iq:result(Packet, Result);
		      {error, Error} ->
			  exmpp_iq:error(Packet, Error)
		  end,
	    ejabberd_router:route(To, From, Res);
	%% Service discovery : disco#items
	#iq{type = 'get', ns = ?NS_DISCO_ITEMS, payload = #xmlel{attrs = Attrs}} = IQ ->
	    NodeId = exmpp_xml:get_attribute_from_list(Attrs, <<"node">>, <<>>),
	    Rsm = jlib:rsm_decode(IQ),
	    Res = case iq_disco_items(Host, NodeId, From, Rsm) of
		      {result, IQRes} ->
			  Result = #xmlel{ns = ?NS_DISCO_ITEMS,
					  name = 'query',
					  attrs = Attrs,
					  children = IQRes},
			  exmpp_iq:result(Packet, Result);
		      {error, Error} ->
			  exmpp_iq:error(Packet, Error)
		  end,
	    ejabberd_router:route(To, From, Res);
	%% pubsub
	#iq{type = IQType, ns = ?NS_PUBSUB, lang = Lang, payload = #xmlel{} = SubEl}
	when IQType == 'get' orelse IQType == 'set' ->
	    Res =	case iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, Access, Plugins) of
			    {result,    []} -> exmpp_iq:result(Packet);
			    {result, IQRes} -> exmpp_iq:result(Packet, IQRes);
			    {error,  Error} -> exmpp_iq:error(Packet, Error)
			end,
	    ejabberd_router:route(To, From, Res);
	%% pubsub#owner
	#iq{type = IQType, ns = ?NS_PUBSUB_OWNER, lang = Lang, payload = #xmlel{} = SubEl}
	when IQType == 'get' orelse IQType == 'set' ->  
	    Res = case iq_pubsub_owner(Host, ServerHost, From, IQType, SubEl, Lang) of
		      {result,    []} -> exmpp_iq:result(Packet);
		      {result, IQRes} -> exmpp_iq:result(Packet, IQRes);
		      {error,  Error} -> exmpp_iq:error(Packet, Error)
		  end,
	    ejabberd_router:route(To, From, Res); %% TODO : add error handling
	%% vCard
	#iq{type = 'get', ns = ?NS_VCARD = XMLNS, lang = Lang} ->
	    VCard = #xmlel{ns = XMLNS,
			   name = 'vCard',
			   children = iq_get_vcard(Lang)},
	    Res = exmpp_iq:result(Packet, VCard),
	    ejabberd_router:route(To, From, Res);
	%% Ad hoc commands
	#iq{type = 'set', ns = ?NS_ADHOC} = IQ ->
	    Res = case iq_command(Host, ServerHost, From, IQ, Access, Plugins) of
		      {error,  Error} -> exmpp_iq:error(Packet, Error);
		      {result, IQRes} -> exmpp_iq:result(Packet, IQRes)
		  end,
	    ejabberd_router:route(To, From, Res); %%TODO : add error handling
	%% Other <iq/>
	_ ->
	    Err = exmpp_iq:error(Packet, 'feature-not-implemented'),
	    ejabberd_router:route(To, From, Err)
    end;

%%% <message/>
do_route(ServerHost, _Access, _Plugins, Host, From, To,
	 #xmlel{name = 'message', children = Els} = Packet) ->
    case exmpp_stanza:is_stanza_error(Packet) of
	true ->
	    ok;
	false ->
	    case exmpp_xml:remove_cdata_from_list(Els) of
		%% <message><x xmlns='jabber:x:data'/></message>
		[#xmlel{name = 'x', ns = ?NS_DATA_FORMS}] ->
		    case find_authorization_response(Packet) of
			none -> ok;
			invalid ->
			    ejabberd_router:route(To, From, exmpp_message:error(Packet, 'bad-request'));
			XFields ->
			    handle_authorization_response(Host, From, To, Packet, XFields)
		    end;
		%% Pubsub vanilla <message/>
		[#xmlel{name = 'pubsub', ns = ?NS_PUBSUB} = Pubsub] ->
		    case exmpp_xml:get_element(Pubsub, 'publish') of
			undefined ->
			    ok;
			Publish ->
			    case exmpp_xml:get_element(Publish, 'item') of
				undefined ->
				    ok;
				#xmlel{attrs = Attrs, children = Els} = _Item ->
				    NodeId = exmpp_xml:get_attribute(Publish, <<"node">>, <<>>),
				    ItemId = exmpp_xml:get_attribute_from_list(Attrs, <<"id">>, <<>>),
				    case publish_item(Host, ServerHost, NodeId, From, ItemId, Els) of
					{result, _} ->
					    ok;
					{error, Reason} ->
					    ejabberd_router:route(To, From, exmpp_message:error(Packet, Reason))
				    end
			    end
		    end;
		%% Other <message/>
		_ ->
		    ok
	    end
    end;

%%<presence/>
do_route(_ServerHost, _Access, _Plugins, _Host, _From, _To, _Packet) ->
    true.
%% Other cases ?
						%do_route(_, _, _, _, From, To, Packet) ->
						%    case exmpp_stanza:get_type(Packet) of
						%  <<"error">> -> ok;
						%  <<"result">> -> ok;
						%  _ ->
						%      Err = exmpp_stanza:reply_with_error(Packet, 'item-not-found'),
						%      ejabberd_router:route(To, From, Err)
						%    end.


-spec(command_disco_info/3 ::
      (
			   Host   :: hostPubsub(), %% Host::host() TODO : implement ad hoc commands for PEP
			   NodeId :: nodeId(),
			   From   :: jidEntity())
      -> {result, Info::[#xmlel{}]}
	     | {result, Info::[#xmlel{}
			       |#xmlel{}]}
	    ).

command_disco_info(_Host, ?NS_ADHOC_b = _NodeId, _From) ->
    {result,
     [#xmlel{ns = ?NS_DISCO_INFO,
             name = 'identity',
             attrs = [?XMLATTR(<<"category">>, <<"automation">>),
		      ?XMLATTR(<<"type">>, <<"command-list">>)]}]};
command_disco_info(_Host, ?NS_PUBSUB_GET_PENDING_b = _NodeId, _From) ->
    {result,
						% Identity
     [#xmlel{ns = ?NS_DISCO_INFO,
             name = 'identity',
             attrs = [?XMLATTR(<<"category">>, <<"automation">>),
                      ?XMLATTR(<<"type">>, <<"command-node">>)]},
						% Features
      #xmlel{ns = ?NS_DISCO_INFO,
             name = 'feature',
             attrs = [?XMLATTR(<<"var">>, ?NS_ADHOC)]}]}.


-spec(node_disco_info/3 ::
      (
			Host   :: hostPubsub(),
			NodeId :: nodeId(),
			From   :: jidEntity())
      -> {'result', [#xmlel{}
		     |#xmlel{}]}
	     | {'error', _}
	    ).

node_disco_info(Host, NodeId, From) ->
    Action = fun(#pubsub_node{type = Plugin, idx = NodeIdx}) ->
		     Types = case tree_call(Host, get_subnodes, [Host, NodeId, From]) of
				 [] -> ["leaf"];
				 _  ->
				     case node_call(Plugin, get_items, [NodeIdx, From, none]) of
					 {result, []} -> ["collection"];
					 {result,  _} -> ["leaf", "collection"];
					 _            -> []
				     end
			     end,
		     %% TODO: add meta-data info (spec section 5.4)
		     {result,
		      %% Identities
		      [#xmlel{ns    = ?NS_DISCO_INFO,
			      name  = 'identity',
			      attrs = [?XMLATTR(<<"category">>, <<"pubsub">>),
				       ?XMLATTR(<<"type">>, Type)]} || Type <- Types ]
		      ++
		      %% Features
		      [#xmlel{ns    = ?NS_DISCO_INFO,
			      name  = 'feature',
			      attrs = [?XMLATTR(<<"var">>, ?NS_PUBSUB_b)]} |
			     lists:map(fun
				     ("rsm") -> #xmlel{ns = ?NS_DISCO_INFO,
						       name = 'feature',
						       attrs = [?XMLATTR(<<"var">>, ?NS_RSM_b)]};
				     (T) ->  #xmlel{ns = ?NS_DISCO_INFO,
						       name = 'feature',
						       attrs = [?XMLATTR(<<"var">>, list_to_binary(?NS_PUBSUB_s++"#"++T))]}
			    end, features(Plugin))]}
                        %%|| Type <- features(Plugin)]]}
	     end,
    case transaction(Host, NodeId, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Error                 -> Error
    end.


-spec(iq_disco_info/4 ::
      (
		      Host   :: hostPubsub(),
		      NodeId :: nodeId(),
		      From   :: jidEntity(),
		      Lang   :: binary())
      -> {'result', [#xmlel{}
		     |#xmlel{}]}
	     | {'error', _}
	    ).

iq_disco_info(Host, <<>> = _NodeId, _From, Lang) ->
    {result,
     %% Identities
     [#xmlel{ns = ?NS_DISCO_INFO,
	     name = 'identity',
	     attrs = [?XMLATTR(<<"category">>, "pubsub"),
		      ?XMLATTR(<<"type">>, "service"),
		      ?XMLATTR(<<"name">>, translate:translate(Lang, "Publish-Subscribe"))]},
      %% Features
      #xmlel{ns = ?NS_DISCO_INFO,
	     name = 'feature',
	     attrs = [?XMLATTR(<<"var">>, ?NS_DISCO_INFO_b)]},
      #xmlel{ns = ?NS_DISCO_INFO,
	     name = 'feature',
	     attrs = [?XMLATTR(<<"var">>, ?NS_DISCO_ITEMS_b)]},
      #xmlel{ns = ?NS_DISCO_INFO,
	     name = 'feature',
	     attrs = [?XMLATTR(<<"var">>, ?NS_PUBSUB_b)]},
      #xmlel{ns = ?NS_DISCO_INFO,
	     name = 'feature',
	     attrs = [?XMLATTR(<<"var">>, ?NS_ADHOC_b)]},
      #xmlel{ns = ?NS_DISCO_INFO,
	     name = 'feature',
	     attrs = [?XMLATTR(<<"var">>, ?NS_VCARD_b)]}]
     ++
	lists:map(fun
		("rsm") -> #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR(<<"var">>, ?NS_RSM_b)]};
		(Feature) -> #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [?XMLATTR(<<"var">>, list_to_binary(?NS_PUBSUB_s++"#"++Feature))]}
	    end, features(Host, <<>>))};
iq_disco_info(Host, NodeId, From, _Lang)
  when NodeId == ?NS_ADHOC_b orelse NodeId == ?NS_PUBSUB_GET_PENDING_b ->
    command_disco_info(Host, NodeId, From);
iq_disco_info(Host, NodeId, From, _Lang) ->
    node_disco_info(Host, NodeId, From).


-spec(iq_disco_items/4 ::
      (
		       Host   :: hostPubsub(),
		       NodeId :: nodeId(),
		       From   :: jidEntity(),
		       Rsm :: _)
      -> {'result', [] | [#xmlel{}]}
	     | {'error', _}
	    ).

iq_disco_items(Host, <<>> = _NodeId, From, _Rsm) ->
    case tree_action(Host, get_subnodes, [Host, <<>>, From]) of
	Nodes when is_list(Nodes) ->
	    {result, lists:map(
		       fun(#pubsub_node{id = {_, SubNodeId}, options = Options}) ->
			       Attrs =
				   case get_option(Options, 'title') of
				       false ->
					   [?XMLATTR(<<"jid">>, Host) | nodeAttr(SubNodeId)];
				       Title ->
					   [?XMLATTR(<<"jid">>, Host),	?XMLATTR(<<"name">>, Title) | nodeAttr(SubNodeId)]
				   end,
			       #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs = Attrs}
		       end, Nodes)};
	Other ->
	    Other
    end;
iq_disco_items(Host, ?NS_ADHOC_b = _NodeId, _From, _Rsm) ->
    %% TODO: support localization of this string
    {result,
     [#xmlel{ns    = ?NS_DISCO_ITEMS,
	     name  = 'item',
	     attrs = [?XMLATTR(<<"jid">>, Host),
		      ?XMLATTR(<<"node">>, ?NS_PUBSUB_GET_PENDING_b),
		      ?XMLATTR(<<"name">>, "Get Pending")]}]};
iq_disco_items(_Host, ?NS_PUBSUB_GET_PENDING_b = _NodeId, _From, _Rsm) ->
    %% TODO
    {result, []};
iq_disco_items(Host, NodeId, From, Rsm) ->
    Action = fun(#pubsub_node{idx = NodeIdx, type = Type, options = Options}) ->
		     Owners = node_owners_call(Type, NodeIdx),
		     {NodeItems, RsmOut} = case get_allowed_items_call(Host, NodeIdx, From, Type, Options, Owners, Rsm) of
				     {result, R} -> R;
				     _ -> {[], none}
				 end,
		     Nodes = lists:map(
			       fun(#pubsub_node{id = {_, SubNodeId}, options = SubOptions}) ->
				       Attrs =
					   case get_option(SubOptions, 'title') of
					       false ->
						   [?XMLATTR(<<"jid">>, Host) | nodeAttr(SubNodeId)];
					       Title ->
						   [?XMLATTR(<<"jid">>, Host),	?XMLATTR(<<"name">>, Title) | nodeAttr(SubNodeId)]
					   end,
				       #xmlel{ns    = ?NS_DISCO_ITEMS,
					      name  = 'item',
					      attrs = Attrs}
			       end, tree_call(Host, get_subnodes, [Host, NodeId, From])),
		     Items = lists:map(
			       fun(#pubsub_item{id = {RN, _}}) ->
				       {result, Name} = node_call(Type, get_item_name, [Host, NodeId, RN]),
				       #xmlel{ns    = ?NS_DISCO_ITEMS,
					      name  = 'item',
					      attrs = [?XMLATTR(<<"jid">>, Host),
						       ?XMLATTR(<<"name">>, Name)]}
			       end, NodeItems),
		     {result, Nodes ++ Items ++ jlib:rsm_encode(RsmOut)}
	     end,
    case transaction(Host, NodeId, Action, sync_dirty) of
	{result, {_, Result}} -> {result, Result};
	Other -> Other
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


-spec(iq_sm/3 ::
      (
	      From :: jidEntity(),
	      To   :: jidContact(),
	      IQ   :: #iq{type::'get'|'set'})
      -> #iq{type::'result'|'error'}
	    ).

iq_sm(From, #jid{node = U, domain = S, resource = undefined = R} = _To,
      #iq{type = Type, payload = #xmlel{} = SubEl, ns = XMLNS, lang = Lang} = IQ)
  when (Type == 'get' orelse Type == 'set') ->
    Result = case XMLNS of
		 ?NS_PUBSUB       -> iq_pubsub({U,S,R}, S, From, Type, SubEl, Lang);
		 ?NS_PUBSUB_OWNER -> iq_pubsub_owner({U,S,R}, S, From, Type, SubEl, Lang)
	     end,
    case Result of
	{result,       []} -> exmpp_iq:result(IQ);
	{result, IQResult} -> exmpp_iq:result(IQ, IQResult);
	{error,     Error} -> exmpp_iq:error(IQ, Error)
    end.
%%TODO : other IQ type and other cases
						%iq_sm(_,_,IQ) -> exmpp_iq:result(IQ).


-spec(iq_get_vcard/1 ::
      (
		     Lang::binary())
      -> Vcard::[#xmlel{}]
	    ).

iq_get_vcard(Lang) ->
    [#xmlel{ns = ?NS_VCARD, name = 'FN', children = [#xmlcdata{cdata = <<"ejabberd/mod_pubsub">>}]},
     #xmlel{ns = ?NS_VCARD, name = 'URL', children = [#xmlcdata{cdata = list_to_binary(?EJABBERD_URI)}]},
     #xmlel{ns = ?NS_VCARD, name = 'DESC', children =
	    [#xmlcdata{cdata = list_to_binary(
				 translate:translate(Lang,
						     "ejabberd Publish-Subscribe module") ++
				 "\nCopyright (c) 2004-2012 ProcessOne")}]}].


-spec(iq_pubsub/6 ::
      (
		  Host       :: host(), % hostPubsub() | hostPEP()
		  ServerHost :: binary(),
		  From       :: jidEntity(),
		  IQType     :: 'get' | 'set',
		  SubEl      :: #xmlel{},
		  Lang       :: binary())
      -> {'result', Result::[] | #xmlel{}} | {'error', _}
	    ).

iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang) ->
    iq_pubsub(Host, ServerHost, From, IQType, SubEl, Lang, 'all', plugins(ServerHost)).


-spec(iq_pubsub/8 ::
      (
		  Host       :: host(), % hostPubsub() | hostPEP()
		  ServerHost :: binary(),
		  From       :: jidEntity(),
		  IQType     :: 'get' | 'set',
		  SubEl      :: #xmlel{},
		  Lang       :: binary(),
		  Access     :: atom(),
		  Plugins    :: [Plugin::nodeType()])
      -> {'result', Result::[] | #xmlel{}} | {'error', _}
	    ).

iq_pubsub(Host, ServerHost, From, IQType, #xmlel{children = Els}, Lang, Access, Plugins) ->
    case exmpp_xml:remove_cdata_from_list(Els) of
	[#xmlel{name = Name, attrs = Attrs, children = SubEls} | Rest] ->
	    %% Fix bug when owner retrieves his affiliations
	    NodeId = exmpp_xml:get_attribute_from_list(Attrs, <<"node">>, <<>>),
	    case {IQType, Name} of
		{'set', 'create'} ->
		    Config = case Rest of
				 [#xmlel{name = 'configure', children = C}] -> C;
				 _ -> []
			     end,
		    %% Get the type of the node
		    Plugin = case exmpp_xml:get_attribute_from_list_as_list(Attrs, <<"type">>, "") of
				 "" -> hd(Plugins);
				 T  -> T
			     end,
		    %% we use Plugins list matching because we do not want to allocate
		    %% atoms for non existing type, this prevent atom allocation overflow
		    case lists:member(Plugin, Plugins) of
			false ->
			    {error, extended_error('feature-not-implemented', unsupported, "create-nodes")};
			true ->
			    create_node(Host, ServerHost, NodeId, From, Plugin, Access, Config)
		    end;
		{'set', 'publish'} ->
		    case exmpp_xml:remove_cdata_from_list(SubEls) of
			[#xmlel{name = 'item', attrs = ItemAttrs, children = Payload}] ->
			    ItemId = exmpp_xml:get_attribute_from_list(ItemAttrs, <<"id">>, <<>>),
			    publish_item(Host, ServerHost, NodeId, From, ItemId,
					 exmpp_xml:remove_cdata_from_list(Payload));
			[] ->
			    %% Publisher attempts to publish to persistent node with no item
			    {error, extended_error('bad-request', "item-required")};
			_ ->
			    %% Entity attempts to publish item with multiple payload elements or namespace does not match
			    {error, extended_error('bad-request', "invalid-payload")}
		    end;
		{'set', 'retract'} ->
		    ForceNotify = case exmpp_xml:get_attribute_from_list(Attrs, <<"notify">>, <<>>) of
				      <<"1">>    -> true;
				      <<"true">> -> true;
				      _          -> false
				  end,
		    case exmpp_xml:remove_cdata_from_list(SubEls) of
			[#xmlel{name = 'item', attrs = ItemAttrs}] ->
			    ItemId = exmpp_xml:get_attribute_from_list(ItemAttrs, <<"id">>, <<>>),
			    delete_item(Host, NodeId, From, ItemId, ForceNotify);
			_ ->
			    %% Request does not specify an item
			    {error, extended_error('bad-request', "item-required")}
		    end;
		{'set', 'subscribe'} ->
		    Config = case Rest of
				 [#xmlel{name = 'options', children = C}] -> C;
				 _ -> []
			     end,
		    JID = exmpp_xml:get_attribute_from_list(Attrs, <<"jid">>, <<>>),
		    subscribe_node(Host, NodeId, From, JID, Config);
		{'set', 'unsubscribe'} ->
		    JID = exmpp_xml:get_attribute_from_list(Attrs, <<"jid">>, <<>>),
		    SubId = exmpp_xml:get_attribute_from_list(Attrs, <<"subid">>, <<>>),
		    unsubscribe_node(Host, NodeId, From, JID, SubId);
		{'get', 'items'} ->
		    MaxItems = exmpp_xml:get_attribute_from_list(Attrs, <<"max_items">>, <<>>),
		    SubId = exmpp_xml:get_attribute_from_list(Attrs, <<"subid">>, <<>>),
		    ItemIds = lists:foldl(fun
					  (#xmlel{name = 'item', attrs = ItemAttrs}, Acc) ->
						 case exmpp_xml:get_attribute_from_list(ItemAttrs, <<"id">>, <<>>) of
						     <<>>   -> Acc;
						     ItemId -> [ItemId|Acc]
						 end;
					  (_, Acc) -> Acc
					 end, [], exmpp_xml:remove_cdata_from_list(SubEls)),
		    get_items(Host, NodeId, From, SubId, MaxItems, ItemIds, jlib:rsm_decode(SubEls));
		{'get', 'subscriptions'} ->
		    get_subscriptions(Host, NodeId, From, Plugins);
		{'get', 'affiliations'} ->
		    get_affiliations(Host, From, Plugins);
		{'get', 'options'} ->
		    SubId = exmpp_xml:get_attribute_from_list(Attrs, <<"subid">>, <<>>),
		    JID = exmpp_xml:get_attribute_from_list(Attrs, <<"jid">>, <<>>),
		    get_options(Host, NodeId, JID, SubId, Lang);
		{'set', 'options'} ->
		    SubId = exmpp_xml:get_attribute_from_list(Attrs, <<"subid">>, <<>>),
		    JID = exmpp_xml:get_attribute_from_list(Attrs, <<"jid">>, <<>>),
		    set_options(Host, NodeId, JID, SubId, SubEls);
		_ ->
		    {error, 'feature-not-implemented'}
	    end;
	Other ->
	    ?INFO_MSG("Too many actions: ~p", [Other]),
	    {error, 'bad-request'}
    end.


-spec(iq_pubsub_owner/6 ::
      (
			Host       :: host(), % hostPubsub() | hostPEP()
			ServerHost :: binary(),
			From       :: jidEntity(),
			IQType     :: 'get' | 'set',
			SubEl      :: #xmlel{},
			Lang       :: binary())
      -> {'result', Result::[] | #xmlel{}} | {'error', _}
	    ).

iq_pubsub_owner(Host, ServerHost, From, IQType, #xmlel{children = Els}, Lang) ->
    case Action = exmpp_xml:remove_cdata_from_list(Els) of
	[#xmlel{name = Name, attrs = Attrs, children = SubEls}] ->
	    NodeId = exmpp_xml:get_attribute_from_list(Attrs, <<"node">>, <<>>),
	    case {IQType, Name} of
		{'get', 'configure'} ->
		    get_configure(Host, ServerHost, NodeId, From, Lang);
		{'set', 'configure'} ->
		    set_configure(Host, NodeId, From, SubEls, Lang);
		{'get', 'default'} ->
		    get_default(Host, NodeId, From, Lang);
		{'set', 'delete'} ->
		    delete_node(Host, NodeId, From);
		{'set', 'purge'} ->
		    purge_node(Host, NodeId, From);
		{'get', 'subscriptions'} ->
		    get_subscriptions(Host, NodeId, From);
		{'set', 'subscriptions'} ->
		    set_subscriptions(Host, NodeId, From, exmpp_xml:remove_cdata_from_list(SubEls));
		{'get', 'affiliations'} ->
		    get_affiliations(Host, NodeId, From);
		{'set', 'affiliations'} ->
		    set_affiliations(Host, NodeId, From, exmpp_xml:remove_cdata_from_list(SubEls));
		_ ->
		    {error, 'feature-not-implemented'}
	    end;
	_ ->
	    ?INFO_MSG("Too many actions: ~p", [Action]),
	    {error, 'bad-request'}
    end.


-spec(iq_command/6 ::
      (
		   Host       :: hostPubsub(), %% TODO : ad hoc commands for PEP
		   ServerHost :: binary(),
		   From       :: jidEntity(),
		   IQ         :: #iq{type :: 'set'},
		   Access     :: atom(),
		   Plugins    :: [Plugin::nodeType()])
      -> {'result', [Result::#xmlel{}]}
	     | {'error', Error::_}
	    ).

iq_command(Host, ServerHost, From, IQ, Access, Plugins) ->
    case adhoc:parse_request(IQ) of
	#adhoc_request{} = Request ->
	    case adhoc_request(Host, ServerHost, From, Request, Access, Plugins) of
		#adhoc_response{} = Response ->
		    {result, [adhoc:produce_response(Request, Response)]};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%% @doc <p>Processes an Ad Hoc Command.</p>
-spec(adhoc_request/6 ::
      (
		      Host       :: hostPubsub(), %% TODO : ad hoc commands for PEP
		      ServerHost :: binary(),
		      From       :: jidEntity(),
		      Request    :: #adhoc_request{},
		      Access     :: atom(),
		      Plugins    :: [Plugin::nodeType()])
      -> #adhoc_response{} | {'error', Error::_}
	    ).

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
adhoc_request(_Host, _ServerHost, _Owner, #adhoc_request{action = "cancel"},
              _Access, _Plugins) ->
    #adhoc_response{status = canceled};
adhoc_request(Host, ServerHost, Owner, #adhoc_request{action = []} = R,
              Access, Plugins) ->
    adhoc_request(Host, ServerHost, Owner, R#adhoc_request{action = "execute"},
                  Access, Plugins);
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
	    XForm = #xmlel{ns = ?NS_DATA_FORMS, name ='x', attrs = [?XMLATTR(<<"type">>, <<"form">>)],
			   children = [
				       #xmlel{ns = ?NS_DATA_FORMS, name = 'field', 
					      attrs = [?XMLATTR(<<"type">>, <<"list-single">>),
						       ?XMLATTR(<<"var">>, <<"pubsub#node">>)],
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
	   [exmpp_jid:to_list(Owner), Host, node_to_string(Node)]),
    Action =
	fun(#pubsub_node{idx = Nidx, type = Type}) ->
		case lists:member("get-pending", features(Type)) of
		    true ->
			case node_call(Type, get_affiliation, [Nidx, Owner]) of
			    {result, owner} ->
				node_call(Type, get_node_subscriptions, [Nidx]);
			    _ ->
				{error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'forbidden')}
			end;
		    false ->
			{error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'feature-not-implemented')}
		end
	end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {N, Subscriptions}} ->
	    lists:foreach(fun({J, pending, _SubId}) ->
				  {U, S, R} = J,
				  send_authorization_request(N, exmpp_jid:make(U,S,R));
			     ({J, pending}) ->
				  {U, S, R} = J,
				  send_authorization_request(N, exmpp_jid:make(U,S,R));
			     (_) ->
				  ok
			  end, Subscriptions),
	    #adhoc_response{};
	Err ->
	    Err
    end.

%%% authorization handling

send_authorization_request(#pubsub_node{id = {Host, Node}, type = Type, idx = Nidx}, Subscriber) ->
    Lang = <<"en">>, %% TODO fix
    {U, S, R} = Subscriber,
    Stanza = #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message', children =
		    [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = [?XMLATTR(<<"type">>, <<"form">>)], children =
			    [#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
				    [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "PubSub subscriber request"))}]},
			     #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
				    [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "Choose whether to approve this entity's subscription."))}]},
			     #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
				    [?XMLATTR(<<"var">>, <<"FORM_TYPE">>), ?XMLATTR(<<"type">>, <<"hidden">>)], children =
				    [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(?NS_PUBSUB_SUBSCRIBE_AUTH_s)}]}]},
			     #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
				    [?XMLATTR(<<"var">>, <<"pubsub#node">>), ?XMLATTR(<<"type">>, <<"text-single">>),
				     ?XMLATTR(<<"label">>, translate:translate(Lang, "Node ID"))], children =
				    [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
					    [#xmlcdata{cdata = Node}]}]},
			     #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR(<<"var">>, <<"pubsub#subscriber_jid">>),
										  ?XMLATTR(<<"type">>, <<"jid-single">>),
										  ?XMLATTR(<<"label">>, translate:translate(Lang, "Subscriber Address"))], children =
				    [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
					    [#xmlcdata{cdata = exmpp_jid:to_binary(U, S, R)}]}]},
			     #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
				    [?XMLATTR(<<"var">>, <<"pubsub#allow">>),
				     ?XMLATTR(<<"type">>, <<"boolean">>),
				     ?XMLATTR(<<"label">>, translate:translate(Lang, "Allow this Jabber ID to subscribe to this pubsub node?"))], children =
				    [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"false">>}]}]}]}]},
    lists:foreach(fun(Owner) ->
			  {U, S, R} = Owner,
			  ejabberd_router:route(service_jid(Host), exmpp_jid:make(U, S, R), Stanza)
		  end, node_owners(Host, Type, Nidx)).

find_authorization_response(Packet) ->
    Els = Packet#xmlel.children,
    XData1 = lists:map(fun(#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = XAttrs} = XEl) ->
			       case exmpp_xml:get_attribute_from_list_as_list(XAttrs, <<"type">>, "") of
				   "cancel" ->
				       none;
				   _ ->
				       jlib:parse_xdata_submit(XEl)
			       end;
			  (_) ->
			       none
		       end, exmpp_xml:remove_cdata_from_list(Els)),
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
		   {S, SID} -> [?XMLATTR(<<"subscription">>, subscription_to_string(S)),
				?XMLATTR(<<"subid">>, SID)];
		   S	-> [?XMLATTR(<<"subscription">>, subscription_to_string(S))]
	       end,
    Stanza = event_stanza(
	       [#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'subscription', attrs =
		       [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(JID)) | nodeAttr(SNode)] ++ SubAttrs
		      }]),
    ejabberd_router:route(service_jid(Host), JID, Stanza).

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
	    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
			     IsApprover = lists:member(jlib:short_prepd_bare_jid(From), node_owners_call(Type, Nidx)),
			     {result, Subscriptions} = node_call(Type, get_subscriptions, [Nidx, Subscriber]),
			     if
				 not IsApprover ->
				     {error, 'forbidden'};
				 true ->
				     update_auth(Host, SNode, Type, Nidx,
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
		    ok
	    end;
	_ ->
	    ejabberd_router:route(
	      To, From,
	      exmpp_stanza:reply_with_error(Packet, 'not-acceptable'))
    end.

update_auth(Host, Node, Type, Nidx, Subscriber,
	    Allow, Subscriptions) ->
    Subscription = lists:filter(fun({pending, _}) -> true;
				   (_)	    -> false
				end, Subscriptions),
    case Subscription of
	[{pending, SubId}] -> %% TODO does not work if several pending
	    NewSubscription = case Allow of
				  true  -> subscribed;
				  false -> none
			      end,
	    node_call(Type, set_subscriptions,
		      [Nidx, Subscriber, NewSubscription, SubId]),
	    send_authorization_approval(Host, Subscriber, Node,
					NewSubscription),
	    {result, ok};
	_ ->
	    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'unexpected-request')}
    end.

-define(XFIELD(Type, Label, Var, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR(<<"type">>, Type),
							     ?XMLATTR(<<"label">>, translate:translate(Lang, Label)),
							     ?XMLATTR(<<"var">>, Var)], children =
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
	       attrs = [?XMLATTR(<<"type">>, <<"text-multi">>),
	       		?XMLATTR(<<"label">>, translate:translate(Lang, Label)),
			?XMLATTR(<<"var">>, Var)
		       ],
	       children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value',
				  children = [?XMLCDATA(V)]}  || V <- Vals]}).  

-define(XFIELDOPT(Type, Label, Var, Val, Opts),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR(<<"type">>, Type),
							     ?XMLATTR(<<"label">>, translate:translate(Lang, Label)),
							     ?XMLATTR(<<"var">>, Var)], children =
	       lists:map(fun(Opt) ->
				 #xmlel{ns = ?NS_DATA_FORMS, name = 'option', children =
					[#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
						[#xmlcdata{cdata = list_to_binary(Opt)}]}]}
			 end, Opts) ++
	       [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Val)}]}]}).

-define(LISTXFIELD(Label, Var, Val, Opts),
	?XFIELDOPT("list-single", Label, Var, Val, Opts)).

-define(LISTMXFIELD(Label, Var, Vals, Opts),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR(<<"type">>, <<"list-multi">>),
							     ?XMLATTR(<<"label">>, translate:translate(Lang, Label)),
							     ?XMLATTR(<<"var">>, Var)], children =
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
%%<li>The requested NodeId already exists.</li>
%%<li>The request did not include a NodeId and "instant nodes" are not supported.</li>
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
					case node_call(Type, create_node, [NodeId, Owner]) of
					    {result, Result} -> {result, {NodeId, Result}};
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
				{error, 'forbidden'}
			end
		end,
	    Reply = #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
			   [#xmlel{ns = ?NS_PUBSUB, name = 'create', attrs = nodeAttr(Node)}]},
	    case transaction(Host, CreateNode, transaction) of
		{result, {NodeId, {Result, broadcast}}} ->
		    broadcast_created_node(Host, Node, NodeId, Type, NodeOptions),
		    ejabberd_hooks:run(pubsub_create_node, ServerHost, [ServerHost, Host, Node, NodeId, NodeOptions]),
		    case Result of
			default -> {result, Reply};
			_ -> {result, Result}
		    end;
		{result, {NodeId, default}} ->
		    ejabberd_hooks:run(pubsub_create_node, ServerHost, [ServerHost, Host, Node, NodeId, NodeOptions]),
		    {result, Reply};
		{result, {NodeId, Result}} ->
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
    {error, 'not-allowed'};
delete_node(Host, Node, Owner) ->
    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
		     case node_call(Type, get_affiliation, [Nidx, Owner]) of
			 {result, owner} ->
			     Removed = tree_call(Host, delete_node, [Host, Node]),
			     case node_call(Type, delete_node, [Removed]) of
				 {result, Res} -> {result, Res};
				 Error -> Error
			     end;
			 _ ->
			     %% Entity is not an owner
			     {error, 'forbidden'}
		     end
	     end,
    Reply = [],
    ServerHost = get(server_host),
    case transaction(Host, Node, Action, transaction) of
	{result, {_, {Result, broadcast, Removed}}} ->
	    lists:foreach(fun({RNode, _RSubscriptions}) ->
				  {RH, RN} = RNode#pubsub_node.id,
				  Nidx = RNode#pubsub_node.idx,
				  Type = RNode#pubsub_node.type,
				  Options = RNode#pubsub_node.options,
				  broadcast_removed_node(RH, RN, Nidx, Type, Options),
				  ejabberd_hooks:run(pubsub_delete_node, ServerHost, [ServerHost, RH, RN, Nidx]),
				  unset_cached_item(RH, Nidx)
			  end, Removed),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {_, {Result, Removed}}} ->
	    lists:foreach(fun({RNode, _RSubscriptions}) ->
				  {RH, RN} = RNode#pubsub_node.id,
				  Nidx = RNode#pubsub_node.idx,
				  ejabberd_hooks:run(pubsub_delete_node, ServerHost, [ServerHost, RH, RN, Nidx]),
				  unset_cached_item(RH, Nidx)
			  end, Removed),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {TNode, default}} ->
	    Nidx = TNode#pubsub_node.idx,
	    ejabberd_hooks:run(pubsub_delete_node, ServerHost, [ServerHost, Host, Node, Nidx]),
	    {result, Reply};
	{result, {TNode, Result}} ->
	    Nidx = TNode#pubsub_node.idx,
	    ejabberd_hooks:run(pubsub_delete_node, ServerHost, [ServerHost, Host, Node, Nidx]),
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
%% @see node_flat:subscribe_node/5
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
    Subscriber = try
		     jlib:short_prepd_jid(exmpp_jid:parse(JID))
		 catch
		     _:_ ->
			 {undefined, undefined, undefined}
		 end,
    Action = fun(#pubsub_node{options = Options, type = Type, idx = Nidx}) ->
		     Features = features(Type),
		     SubscribeFeature = lists:member("subscribe", Features),
		     OptionsFeature = lists:member("subscription-options", Features),
		     HasOptions = not (SubOpts == []),
		     SubscribeConfig = get_option(Options, subscribe),
		     AccessModel = get_option(Options, access_model),
		     SendLast = get_option(Options, send_last_published_item),
		     AllowedGroups = get_option(Options, roster_groups_allowed, []),
		     Owners = node_owners_call(Type, Nidx),
		     {PresenceSubscription, RosterGroup} = get_presence_and_roster_permissions(Host, Subscriber, Owners, AccessModel, AllowedGroups),
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
				       [Nidx, From, Subscriber,
					AccessModel, SendLast,
					PresenceSubscription, RosterGroup,
					SubOpts])
		     end
	     end,
    Reply = fun(Subscription) ->
		    %% TODO, this is subscription-notification, should depends on node features
		    SubAttrs = case Subscription of
				   {subscribed, SubId} ->
				       [?XMLATTR(<<"subscription">>, subscription_to_string(subscribed)),
					?XMLATTR(<<"subid">>, SubId),
					?XMLATTR(<<"node">>, Node)];
				   Other ->
				       [?XMLATTR(<<"subscription">>, subscription_to_string(Other)),
					?XMLATTR(<<"node">>, Node)]
			       end,
		    Fields =
			[ ?XMLATTR(<<"jid">>, JID) | SubAttrs],
		    #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
			   [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs = Fields}]}
	    end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, subscribed, SubId, send_last}}} ->
	    Nidx = TNode#pubsub_node.idx,
	    Type = TNode#pubsub_node.type,
	    send_items(Host, Node, Nidx, Type, Subscriber, last),
	    notify_owners(get_option(TNode#pubsub_node.options, notify_sub), Subscriber, Host, Node, TNode#pubsub_node.owners, "subscribed"),
	    case Result of
		default -> {result, Reply({subscribed, SubId})};
		_ -> {result, Result}
	    end;
	{result, {TNode, {default, subscribed, SubId}}} ->
	    notify_owners(get_option(TNode#pubsub_node.options, notify_sub), Subscriber, Host, Node, TNode#pubsub_node.owners, "subscribed"),
	    {result, Reply({subscribed, SubId})};
	{result, {TNode, {Result, subscribed, _SubId}}} ->
	    notify_owners(get_option(TNode#pubsub_node.options, notify_sub), Subscriber, Host, Node, TNode#pubsub_node.owners, "subscribed"),
	    {result, Result};
	{result, {TNode, {default, pending, _SubId}}} ->
	    send_authorization_request(TNode, Subscriber),
	    notify_owners(get_option(TNode#pubsub_node.options, notify_sub), Subscriber, Host, Node, TNode#pubsub_node.owners, "pending"),
	    {result, Reply(pending)};
	{result, {TNode, {Result, pending}}} ->
	    send_authorization_request(TNode, Subscriber),
	    notify_owners(get_option(TNode#pubsub_node.options, notify_sub), Subscriber, Host, Node, TNode#pubsub_node.owners, "pending"),
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
    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
		     node_call(Type, unsubscribe_node, [Nidx, From, Subscriber, SubId])
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, default}} ->
	    notify_owners(get_option(TNode#pubsub_node.options, notify_sub), Subscriber, Host, Node, TNode#pubsub_node.owners, "none"),
	    {result, []};
	{result, {TNode, Result}} ->
	    notify_owners(get_option(TNode#pubsub_node.options, notify_sub), Subscriber, Host, Node, TNode#pubsub_node.owners, "none"),
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
publish_item(Host, ServerHost, Node, Publisher, <<>>, Payload) ->
    %% if publisher does not specify an ItemId, the service MUST generate the ItemId
    publish_item(Host, ServerHost, Node, Publisher, uniqid(), Payload);
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload) ->
    Action = fun(#pubsub_node{options = Options, type = Type, idx = Nidx}) ->
		     Features = features(Type),
		     PublishFeature = lists:member("publish", Features),
		     PublishModel = get_option(Options, publish_model),
		     MaxItems = max_items(Host, Options),
		     DeliverPayloads = get_option(Options, deliver_payloads),
		     PersistItems = get_option(Options, persist_items),
		     {PayloadCount, PayloadNS} = payload_els_ns(Payload),
		     PayloadSize = size(term_to_binary(Payload))-2, % size(term_to_binary([])) == 2
		     PayloadMaxSize = get_option(Options, max_payload_size),
		     InvalidNS = case get_option(Options, type) of
				     false -> false;
				     [[]] -> false;
				     [ConfiguredNS] -> ConfiguredNS =/= PayloadNS
				 end,
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
			 (PayloadCount > 1) or (PayloadCount == 0) or InvalidNS ->
			     %% Entity attempts to publish item with multiple payload elements
			     %% or with wrong payload NS
			     {error, extended_error('bad-request', "invalid-payload")};
			 (DeliverPayloads == 0) and (PersistItems == 0) and (PayloadSize > 0) ->
			     %% Publisher attempts to publish to transient notification node with item
			     {error, extended_error('bad-request', "item-forbidden")};
			 ((DeliverPayloads == 1) or (PersistItems == 1)) and (PayloadSize == 0) ->
			     %% Publisher attempts to publish to persistent node with no item
			     {error, extended_error('bad-request', "item-required")};
			 true ->
			     node_call(Type, publish_item, [Nidx, Publisher, PublishModel, MaxItems, ItemId, Payload])
		     end
	     end,
    %%ServerHostS = binary_to_list(ServerHost),
    ejabberd_hooks:run(pubsub_publish_item, ServerHost, [ServerHost, Node, Publisher, service_jid(Host), ItemId, Payload]),
    Reply = #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
		   [#xmlel{ns = ?NS_PUBSUB, name = 'publish', attrs = nodeAttr(Node), children =
			   [#xmlel{ns = ?NS_PUBSUB, name = 'item', attrs = itemAttr(ItemId)}]}]},
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, Broadcast, Removed}}} ->
	    Nidx = TNode#pubsub_node.idx,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    case get_option(Options, deliver_notifications) of
			true ->
			    BrPayload = case Broadcast of
						   broadcast -> Payload;
						   PluginPayload -> PluginPayload
					       end,
			    broadcast_publish_item(Host, Node, Nidx, Type, Options, ItemId,
					jlib:short_prepd_jid(Publisher), BrPayload, Removed);
			false ->
				ok
		end,
	    set_cached_item(Host, Nidx, ItemId, Publisher, Payload),
	    case Result of
		default -> {result, Reply};
		_ -> {result, Result}
	    end;
	{result, {TNode, {default, Removed}}} ->
	    Nidx = TNode#pubsub_node.idx,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, Nidx, Type, Options, Removed),
	    set_cached_item(Host, Nidx, ItemId, Publisher, Payload),
	    {result, Reply};
	{result, {TNode, {Result, Removed}}} ->
	    Nidx = TNode#pubsub_node.idx,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, Nidx, Type, Options, Removed),
	    set_cached_item(Host, Nidx, ItemId, Publisher, Payload),
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
			{result, Reply2} ->
			    NewNode = exmpp_xml:get_path(Reply2, [{element, 'create'},
				    {attribute, <<"node">>}]),
			    publish_item(Host, ServerHost, NewNode, Publisher, ItemId,
				    Payload);
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
    Action = fun(#pubsub_node{options = Options, type = Type, idx = Nidx}) ->
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
			     node_call(Type, delete_item, [Nidx, Publisher, PublishModel, ItemId])
		     end
	     end,
    Reply = [],
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, broadcast}}} ->
	    Nidx = TNode#pubsub_node.idx,
	    Type = TNode#pubsub_node.type,
	    Options = TNode#pubsub_node.options,
	    broadcast_retract_items(Host, Node, Nidx, Type, Options, [ItemId], ForceNotify),
	    case get_cached_item(Host, Nidx) of
		#pubsub_item{id = {ItemId, Nidx}} -> unset_cached_item(Host, Nidx);
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
    Action = fun(#pubsub_node{options = Options, type = Type, idx = Nidx}) ->
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
			     node_call(Type, purge_node, [Nidx, Owner])
		     end
	     end,
    Reply = [],
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {TNode, {Result, broadcast}}} ->
	    Nidx = TNode#pubsub_node.idx,
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
get_items(Host, Node, From, SubId, SMaxItems, ItemIds, Rsm) ->
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
	    Action = fun(#pubsub_node{options = Options, type = Type, idx = Nidx}) ->
			     Features = features(Type),
			     RetreiveFeature = lists:member("retrieve-items", Features),
			     PersistentFeature = lists:member("persistent-items", Features),
			     AccessModel = get_option(Options, access_model),
			     AllowedGroups = get_option(Options, roster_groups_allowed, []),
			     Owners = node_owners_call(Type, Nidx),
			     {PresenceSubscription, RosterGroup} = get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups),
			     if
				 not RetreiveFeature ->
				     %% Item Retrieval Not Supported
				     {error, extended_error('feature-not-implemented', unsupported, "retrieve-items")};
				 not PersistentFeature ->
				     %% Persistent Items Not Supported
				     {error, extended_error('feature-not-implemented', unsupported, "persistent-items")};
				 true ->
				     node_call(Type, get_items,
					       [Nidx, From,
						AccessModel, PresenceSubscription, RosterGroup,
						SubId, Rsm])
			     end
		     end,
	    case transaction(Host, Node, Action, sync_dirty) of
		{result, {_, Items, RsmOut}} ->
		    SendItems = case ItemIds of
				    [] -> 
					Items;
				    _ ->
					lists:filter(fun(#pubsub_item{id = {ItemId, _}}) ->
							     lists:member(ItemId, ItemIds)
						     end, Items) 
				end,
		    %% Generate the XML response (Item list), limiting the
		    %% number of items sent to MaxItems:
		    {result, #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
				    [#xmlel{ns = ?NS_PUBSUB, name = 'items', attrs = nodeAttr(Node), children =
					    itemsEls(lists:sublist(SendItems, MaxItems))} | jlib:rsm_encode(RsmOut)]}};
		Error ->
		    Error
	    end
    end.


%% TODO : fix
-spec(get_items/2 ::
      (
		  Host   :: host(),
		  NodeId :: nodeId())
      -> Items :: [] | [Item::pubsubItem()]
	    ).

get_items(Host, NodeId) ->
    Action = fun(#pubsub_node{type = Type, idx = NodeIdx}) ->
		     node_call(Type, get_items, [NodeIdx, service_jid(Host)])
	     end,
    case transaction(Host, NodeId, Action, sync_dirty) of
	{result, {_, Items}} -> Items
						%Error -> Error
    end.

%% TODO : fix
-spec(get_item/3 ::
      (
		 Host   :: host(),
		 NodeId :: nodeId(),
		 ItemId :: itemId())
      -> {'result', Item::pubsubItem()} | {'error', 'item-not-found'}
	    ).

get_item(Host, NodeId, ItemId) ->
    Action = fun(#pubsub_node{type = Type, idx = NodeIdx}) ->
		     node_call(Type, get_item, [NodeIdx, ItemId])
	     end,
    case transaction(Host, NodeId, Action, sync_dirty) of
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
send_items(Host, Node, NodeId, Type, LJID, 'last') ->
    Stanza = case get_cached_item(Host, NodeId) of
	undefined ->
	    % special ODBC optimization, works only with node_hometree_odbc, node_flat_odbc and node_pep_odbc
	    case node_action(Host, Type, get_last_items, [NodeId, LJID, 1]) of
		{result, [LastItem]} ->
		    {ModifNow, ModifUSR} = LastItem#pubsub_item.modification,
		    event_stanza_with_delay([#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items',
					attrs = nodeAttr(Node),
					children = itemsEls([LastItem])}],
				ModifNow, ModifUSR);
		_ ->
		    event_stanza([#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items',
					attrs = nodeAttr(Node),
					children = itemsEls([])}])
	    end;
	LastItem ->
	    {ModifNow, ModifUSR} = LastItem#pubsub_item.modification,
	    event_stanza_with_delay(
		       [#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node),
			       children = itemsEls([LastItem])}], ModifNow, ModifUSR)
    end,
    {U, S, R} = LJID,
    ejabberd_router:route(service_jid(Host), exmpp_jid:make(U, S, R), Stanza);
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
		     {ModifNow, ModifUSR} = LastItem#pubsub_item.modification,
		     event_stanza_with_delay(
		       [#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node), children =
			       itemsEls(ToSend)}], ModifNow, ModifUSR);
		 _ ->
		     event_stanza(
		       [#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'items', attrs = nodeAttr(Node), children =
			       itemsEls(ToSend)}])
	     end,
    ejabberd_router:route(service_jid(Host), exmpp_jid:make(LU, LS, LR), Stanza).

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
			    ({#pubsub_node{id = {_, Node}}, Affiliation}) ->
				 [#xmlel{ns = ?NS_PUBSUB, name = 'affiliation', attrs =
					 [?XMLATTR(<<"node">>, node_to_string(Node)),
					  ?XMLATTR(<<"affiliation">>, affiliation_to_string(Affiliation))]}]
			 end, lists:usort(lists:flatten(Affiliations))),
	    {result, #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children =
			    [#xmlel{ns = ?NS_PUBSUB, name = 'affiliations', children =
				    Entities}]}};
	{Error, _} ->
	    Error
    end;
get_affiliations(Host, Node, JID) ->
    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
		     Features = features(Type),
		     RetrieveFeature = lists:member("modify-affiliations", Features),
		     {result, Affiliation} = node_call(Type, get_affiliation, [Nidx, JID]),
		     if
			 not RetrieveFeature ->
			     %% Service does not support modify affiliations
			     {error, extended_error('feature-not-implemented', unsupported, "modify-affiliations")};
			 Affiliation /= owner ->
			     %% Entity is not an owner
			     {error, 'forbidden'};
			 true ->
			     node_call(Type, get_node_affiliations, [Nidx])
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_, Affiliations}} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({{AU, AS, AR}, Affiliation}) ->
				 [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'affiliation', attrs =
					 [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(AU, AS, AR)),
					  ?XMLATTR(<<"affiliation">>, affiliation_to_string(Affiliation))]}]
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
					      exmpp_xml:get_attribute_from_list(Attrs, <<"jid">>, ""))
					catch
					    _:_ -> error
					end,
				  Affiliation = string_to_affiliation(
						  exmpp_xml:get_attribute_from_list_as_list(Attrs, <<"affiliation">>, "")),
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
	    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
			     Owners = node_owners_call(Type, Nidx),
			     case lists:member(Owner, Owners) of
				 true ->
				     OwnerJID = exmpp_jid:make(Owner),
				     FilteredEntities = case Owners of
							    [Owner] -> [E || E <- Entities, element(1, E) =/= OwnerJID];
							    _ -> Entities
							end,
				     lists:foreach(
				       fun({JID, Affiliation}) ->
					    % TODO, check if nothing missing here about new owners
					    node_call(Type, set_affiliation, [Nidx, JID, Affiliation])
				       end, FilteredEntities),
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

get_options(Host, Node, JID, SubId, Lang) ->
    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
		     case lists:member("subscription-options", features(Type)) of
			 true  ->
			     get_options_helper(JID, Lang, Node, Nidx, SubId, Type);
			 false ->
			     {error, extended_error(
				       'feature-not-implemented',
				       unsupported, "subscription-options")}
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	{result, {_Node, XForm}} -> {result, XForm};
	Error		    -> Error
    end.

get_options_helper(JID, Lang, Node, NodeId, SubId, Type) ->
    Subscriber = try exmpp_jid:parse(JID) of
		     J -> jlib:short_jid(J)
		 catch
		     _ ->
		         exmpp_jid:make("", "", "") %% TODO, check if use <<>> instead of ""
		 end,
    {result, Subs} = node_call(Type, get_subscriptions,
			       [NodeId, Subscriber]),
    SubIds = lists:foldl(fun({subscribed, SID}, Acc) ->
				 [SID | Acc];
			    (_, Acc) ->
				 Acc
			 end, [], Subs),
    case {SubId, SubIds} of
	{_, []} ->
	    {error, extended_error('not-acceptable', "not-subscribed")};
	{[], [SID]} ->
	    read_sub(Subscriber, Node, NodeId, SID, Lang);
	{[], _} ->
	    {error, extended_error('not-acceptable', "subid-required")};
	{_, _} ->
	    read_sub(Subscriber, Node, NodeId, SubId, Lang)
    end.

read_sub(Subscriber, Node, NodeId, SubId, Lang) ->
    case pubsub_subscription_odbc:get_subscription(Subscriber, NodeId, SubId) of
	{result, #pubsub_subscription{options = Options}} ->
            {result, XdataEl} = pubsub_subscription_odbc:get_options_xform(Lang, Options),
            OptionsEl = #xmlel{ns = ?NS_PUBSUB, name = 'options',
			       attrs = [ ?XMLATTR(<<"jid">>, exmpp_jid:to_binary(Subscriber)),
					 ?XMLATTR(<<"subid">>, SubId) | nodeAttr(Node)],
			       children = [XdataEl]},
            PubsubEl = #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children = [OptionsEl]},
            {result, PubsubEl};
	_ ->
	    OptionsEl = #xmlel{ns = ?NS_PUBSUB, name = 'options',
			       attrs = [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(Subscriber)),
					?XMLATTR(<<"subid">>, SubId) | nodeAttr(Node)]},
	    PubsubEl = #xmlel{ns = ?NS_PUBSUB, name = 'pubsub', children = [OptionsEl]},
            {result, PubsubEl}
    end.

set_options(Host, Node, JID, SubId, Configuration) ->
    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
		     case lists:member("subscription-options", features(Type)) of
			 true ->
			     set_options_helper(Configuration, JID, Nidx,
						SubId, Type);
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

set_options_helper(Configuration, JID, NodeId, SubId, Type) ->
    SubOpts = case pubsub_subscription_odbc:parse_options_xform(Configuration) of
		  {result, GoodSubOpts} -> GoodSubOpts;
		  _ -> invalid
	      end,
    Subscriber = try exmpp_jid:parse(JID) of
		     J -> J
		 catch
		     _ -> exmpp_jid:make("", "", "") %% TODO, check if use <<>> instead of ""
		 end,
    {result, Subs} = node_call(Type, get_subscriptions,
			       [NodeId, Subscriber]),
    SubIds = lists:foldl(fun({subscribed, SID}, Acc) ->
				 [SID | Acc];
			    (_, Acc) ->
				 Acc
			 end, [], Subs),
    case {SubId, SubIds} of
	{_, []} ->
	    {error, extended_error('not-acceptable', "not-subscribed")};
	{[], [SID]} ->
	    write_sub(Subscriber, NodeId, SID, SubOpts);
	{[], _} ->
	    {error, extended_error('not-acceptable', "subid-required")};
	{_, _} ->
	    write_sub(Subscriber, NodeId, SubId, SubOpts)
    end.

write_sub(_Subscriber, _NodeId, _SubId, invalid) ->
    {error, extended_error('bad-request', "invalid-options")};
write_sub(_Subscriber, _NodeID, _SubID, []) ->
    {result, []};
write_sub(Subscriber, NodeId, SubId, Options) ->
    case pubsub_subscription_odbc:set_subscription(Subscriber, NodeId, SubId, Options) of
	{result, _} ->
	    {result, []};
	{error, _} ->
	    {error, extended_error('not-acceptable', "invalid-subid")}
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
			    ({#pubsub_node{id = {_, SubsNode}}, Subscription}) ->
				 case Node of
				     <<>> ->
					 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs =
						 [?XMLATTR(<<"node">>, node_to_string(SubsNode)),
						  ?XMLATTR(<<"subscription">>, subscription_to_string(Subscription))]}];
				     SubsNode ->
					 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs =
						 [?XMLATTR(<<"subscription">>, subscription_to_string(Subscription))]}];
				     _ ->
					 []
				 end;
			    ({_, none, _}) ->
				 [];
			    ({#pubsub_node{id = {_, SubsNode}}, Subscription, SubId, SubJID}) ->
				 case Node of
				     <<>> ->
					 [#xmlel{ns = ?NS_PUBSUB, name='subscription',
						 attrs = [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(SubJID)),
							  ?XMLATTR(<<"subid">>, SubId),
							  ?XMLATTR(<<"subscription">>, subscription_to_string(Subscription)) | nodeAttr(SubsNode)]}];
				     SubsNode ->
					 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', 
						 attrs = [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(SubJID)),
							  ?XMLATTR(<<"subid">>, SubId),
							  ?XMLATTR(<<"subscription">>, subscription_to_string(Subscription))]}];
				     _ ->
					 []
				 end;
			    ({#pubsub_node{id = {_, SubsNode}}, Subscription, SubJID}) ->
				 case Node of
				     <<>> ->
					 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs =
						 [?XMLATTR(<<"node">>, node_to_string(SubsNode)),
						  ?XMLATTR(<<"jid">>, exmpp_jid:to_binary(SubJID)),
						  ?XMLATTR(<<"subscription">>, subscription_to_string(Subscription))]}];
				     SubsNode ->
					 [#xmlel{ns = ?NS_PUBSUB, name = 'subscription', attrs =
						 [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(SubJID)),
						  ?XMLATTR(<<"subscription">>, subscription_to_string(Subscription))]}];
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
    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
		     Features = features(Type),
		     RetrieveFeature = lists:member("manage-subscriptions", Features),
		     {result, Affiliation} = node_call(Type, get_affiliation, [Nidx, JID]),
		     if
			 not RetrieveFeature ->
			     %% Service does not support manage subscriptions
			     {error, extended_error('feature-not-implemented', unsupported, "manage-subscriptions")};
			 Affiliation /= owner ->
			     %% Entity is not an owner
			     {error, 'forbidden'};
			 true ->
			     node_call(Type, get_node_subscriptions, [Nidx])
		     end
	     end,
    case transaction(Host, Node, Action, sync_dirty) of
	%% Fix bug when node owner retrieve an empty subscriptions list 
						%	{result, {_, []}} ->
						%	    {error, 'item-not-found'};
	{result, {_, Subscriptions}} ->
	    Entities = lists:flatmap(
			 fun({_, none}) -> [];
			    ({_, pending, _}) -> [];
			    ({{AU, AS, AR}, Subscription}) ->
				 [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'subscription', attrs =
					 [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(AU, AS, AR)),
					  ?XMLATTR(<<"subscription">>, subscription_to_string(Subscription))]}];
			    ({{AU, AS, AR}, Subscription, SubId}) ->
				 [#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'subscription', attrs =
					 [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(AU, AS, AR)),
					  ?XMLATTR(<<"subscription">>, subscription_to_string(Subscription)),
					  ?XMLATTR(<<"subid">>, SubId)]}]
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
					      exmpp_xml:get_attribute_from_list(Attrs, <<"jid">>, ""))
					catch
					    _:_ ->
						error
					end,
				  Subscription = string_to_subscription(
						   exmpp_xml:get_attribute_from_list_as_list(Attrs, <<"subscription">>, false)),
				  SubId = exmpp_xml:get_attribute_from_list_as_list(Attrs, <<"subid">>, false),
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
							     attrs = [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(JID)),
								      ?XMLATTR(<<"subsription">>, subscription_to_string(Sub)) | nodeAttr(Node)]}]}]},
			     ejabberd_router:route(service_jid(Host), JID, Stanza)
		     end,
	    Action = fun(#pubsub_node{type = Type, idx = Nidx}) ->
			     case lists:member(Owner, node_owners_call(Type, Nidx)) of
				 true ->
				     Result = lists:foldl(fun({JID, Subscription, SubId}, Acc) ->

								  case node_call(Type, set_subscriptions, [Nidx, JID, Subscription, SubId]) of
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

get_roster_info(_, _, {undefined, undefined, _}, _) ->
    {false, false};
%% @spec (OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, SubscriberResource}, AllowedGroups)
%%    -> {PresenceSubscription, RosterGroup}
get_roster_info(OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, _}, AllowedGroups) ->
    {Subscription, Groups} =
	ejabberd_hooks:run_fold(
	  roster_get_jid_info, OwnerServer,
	  {none, []},
	  [OwnerUser, OwnerServer, exmpp_jid:make({SubscriberUser, SubscriberServer, undefined})]),
    PresenceSubscription = (Subscription == both) orelse (Subscription == from)
	orelse ({OwnerUser, OwnerServer} == {SubscriberUser, SubscriberServer}),
    RosterGroup = lists:any(fun(Group) ->
				    lists:member(Group, AllowedGroups)
			    end, Groups),
    {PresenceSubscription, RosterGroup};
get_roster_info(OwnerUser, OwnerServer, JID, AllowedGroups) ->
    get_roster_info(OwnerUser, OwnerServer, exmpp_jid:to_lower(JID), AllowedGroups).

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
-spec(service_jid/1 ::
      (
		    Host::host() | string())
      -> ServiceJID :: jidContact() | jidComponent()
	    ).

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

node_to_deliver(LJID, NodeOptions) ->
    presence_can_deliver(LJID, get_option(NodeOptions, presence_based_delivery)).

sub_option_can_deliver(items, _, {subscription_type, nodes}) -> false;
sub_option_can_deliver(nodes, _, {subscription_type, items}) -> false;
sub_option_can_deliver(_, _, {subscription_depth, all})      -> true;
sub_option_can_deliver(_, Depth, {subscription_depth, D})    -> Depth =< D;
sub_option_can_deliver(_, _, {deliver, false})               -> false;
sub_option_can_deliver(_, _, {expire, When})                 -> now() < When;
sub_option_can_deliver(_, _, _)                              -> true.

presence_can_deliver(_, false) -> true;
presence_can_deliver({User, Server, Resource}, true) ->
    case ejabberd_sm:get_user_sessions(User, Server) of
	[] -> false;
	Sessions ->
	    lists:foldl(fun(_, true) -> true;
			   ({session, _, _, _, undefined, _}, _Acc) -> false;
			   ({session, _, {_, _, R}, _, _Priority, _}, _Acc) ->
				case Resource of
				    undefined -> true;
				    R         -> true;
				    _         -> false
				end
			end, false, Sessions)
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
    case ejabberd_sm:get_session_pid({U, S, R}) of
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
payload_els_ns(Payload) -> payload_els_ns(Payload, 0, undefined).
payload_els_ns([], Count, NS) -> {Count, NS};
payload_els_ns([#xmlel{ns=NS}|Tail], Count, undefined) -> payload_els_ns(Tail, Count+1, NS);
payload_els_ns([#xmlel{}|Tail], Count, NS) -> payload_els_ns(Tail, Count+1, NS);
payload_els_ns([_|Tail], Count, NS) -> payload_els_ns(Tail, Count, NS).

%% @spec (Els) -> stanza()
%%    Els = [xmlel()]
%% @doc <p>Build pubsub event stanza</p>
event_stanza(Els) ->
    event_stanza_withmoreels(Els, []).

event_stanza_with_delay(Els, ModifNow, {U, S, R}) ->
    DateTime = calendar:now_to_datetime(ModifNow),
    LJID = exmpp_jid:make(U, S, R),
    MoreEls = [jlib:timestamp_to_xml(DateTime, utc, LJID, "")],
    event_stanza_withmoreels(Els, MoreEls).

event_stanza_withmoreels(Els, MoreEls) ->
    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message', children =
	   [#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'event', children = Els} | MoreEls]}.

event_stanza(Event, EvAttr) ->
    event_stanza([#xmlel{ns = ?NS_PUBSUB_EVENT, name = Event, attrs = EvAttr}]).
event_stanza(Event, EvAttr, Entries) ->
    event_stanza([#xmlel{ns = ?NS_PUBSUB_EVENT, name = Event, attrs = EvAttr, children =
		    [#xmlel{ns = ?NS_PUBSUB_EVENT, name = Entry, attrs = EnAttr} ||
			{Entry, EnAttr} <- Entries]}]).
event_stanza(Event, EvAttr, Entry, EnAttr, Payload) ->
    event_stanza([#xmlel{ns = ?NS_PUBSUB_EVENT, name = Event, attrs = EvAttr, children =
		    [#xmlel{ns = ?NS_PUBSUB_EVENT, name = Entry, attrs = EnAttr, children = Payload}]}]).
event_stanza(Event, EvAttr, Entry, EnAttr, Payload, Publisher) ->
    Stanza = event_stanza(Event, EvAttr, Entry, EnAttr, Payload),
    add_extended_headers(Stanza, extended_headers([jlib:jid_to_string(Publisher)])).

%%%%%% broadcast functions

broadcast_publish_item(Host, Node, NodeId, Type, NodeOptions, ItemId, Publisher, Payload, Removed) ->
    PStanza = case get_option(NodeOptions, deliver_payloads) of
	true -> event_stanza('items', nodeAttr(Node), 'item', itemAttr(ItemId), Payload, Publisher);
	false -> event_stanza('items', nodeAttr(Node), 'item', itemAttr(ItemId), [], Publisher)
	end,
    RStanza = event_stanza('items', nodeAttr(Node), [{'retract', itemAttr(Rid)} || Rid <- Removed]),
    Stanzas = [{true, PStanza, true}, {get_option(NodeOptions, notify_retract), RStanza, true}],
    {result, broadcast(Host, Node, NodeId, Type, NodeOptions, items, Stanzas)}.

broadcast_retract_items(Host, Node, NodeId, Type, NodeOptions, ItemIds) ->
    broadcast_retract_items(Host, Node, NodeId, Type, NodeOptions, ItemIds, notify_retract).
broadcast_retract_items(_Host, _Node, _NodeId, _Type, _NodeOptions, [], _) ->
     {result, false};
broadcast_retract_items(Host, Node, NodeId, Type, NodeOptions, ItemIds, false) ->
    broadcast_retract_items(Host, Node, NodeId, Type, NodeOptions, ItemIds, notify_retract);
broadcast_retract_items(Host, Node, NodeId, Type, NodeOptions, ItemIds, Notify) ->
    Stanza = event_stanza('items', nodeAttr(Node), [{'retract', itemAttr(Rid)} || Rid <- ItemIds]),
    {result, broadcast(Host, Node, NodeId, Type, NodeOptions, items, Notify, Stanza, true)}.

broadcast_purge_node(Host, Node, NodeId, Type, NodeOptions) ->
    Stanza = event_stanza('purge', nodeAttr(Node)),
    {result, broadcast(Host, Node, NodeId, Type, NodeOptions, nodes, notify_retract, Stanza, false)}.

broadcast_removed_node(Host, Node, NodeId, Type, NodeOptions) ->
    Stanza = event_stanza('delete', nodeAttr(Node)),
    {result, broadcast(Host, Node, NodeId, Type, NodeOptions, nodes, notify_delete, Stanza, false)}.

broadcast_created_node(Host, Node, NodeId, Type, NodeOptions) ->
    Stanza = event_stanza('create', nodeAttr(Node)),
    {result, broadcast(Host, Node, NodeId, Type, NodeOptions, nodes, true, Stanza, true)}.

broadcast_config_notification(Host, Node, NodeId, Type, NodeOptions, Lang) ->
    Stanza = case get_option(NodeOptions, deliver_payloads) of
	true ->
	    event_stanza([#xmlel{ns = ?NS_PUBSUB_EVENT, name = 'configuration', attrs = nodeAttr(Node), children =
			    [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = [?XMLATTR(<<"type">>, <<"form">>)], children =
				    get_configure_xfields(Type, NodeOptions, Lang, [])}]}]);
	false ->
	    event_stanza("configuration", nodeAttr(Node))
	end,
    {result, broadcast(Host, Node, NodeId, Type, NodeOptions, nodes, notify_config, Stanza, false)}.

broadcast(Host, Node, NodeId, Type, NodeOptions, Notify, Stanzas) ->
    Subs = node_subscriptions(Host, Node, NodeId, Type, NodeOptions, Notify),
    Result = [broadcast(Host, Node, NodeId, Type, NodeOptions, Subs, Stanza, SHIM) ||
		{Cond, Stanza, SHIM} <- Stanzas, Cond =:= true],
    lists:member(true, Result).
broadcast(Host, Node, NodeId, Type, NodeOptions, Notify, true, Stanza, SHIM) ->
    Subs = node_subscriptions(Host, Node, NodeId, Type, NodeOptions, Notify),
    broadcast(Host, Node, NodeId, Type, NodeOptions, Subs, Stanza, SHIM);
broadcast(_Host, _Node, _NodeId, _Type, _NodeOptions, _Notify, false, _Stanza, _SHIM) ->
    false;
broadcast(Host, Node, NodeId, Type, NodeOptions, Notify, Condition, Stanza, SHIM) ->
    broadcast(Host, Node, NodeId, Type, NodeOptions, Notify, get_option(NodeOptions, Condition), Stanza, SHIM).

broadcast({U, S, R}, Node, NodeId, Type, NodeOptions, Subscriptions, Stanza, SHIM) ->
    broadcast(S, Node, NodeId, Type, NodeOptions, Subscriptions, Stanza, SHIM)
	or case ejabberd_sm:get_session_pid({U, S, user_resource(U, S, R)}) of
	C2SPid when is_pid(C2SPid) ->
	    %% set the from address on the notification to the bare JID of the account owner
	    %% Also, add "replyto" if entity has presence subscription to the account owner
	    %% See XEP-0163 1.1 section 4.3.1
	    Event = {pep_message, << Node/binary, <<"+notify">>/binary >>},
	    Message = case get_option(NodeOptions, notification_type, headline) of
		normal -> Stanza;
		MsgType -> add_message_type(Stanza, atom_to_list(MsgType))
	    end,
	    ejabberd_c2s:broadcast(C2SPid, Event, jlib:make_jid(U, S, ""), Message),
	    true;
	_ ->
	    ?DEBUG("~p@~p has no session; can't deliver stanza: ~p", [U, S, Stanza]),
	    false
    end;
broadcast(_Host, _Node, _NodeId, _Type, _NodeOptions, [], _Stanza, _SHIM) ->
    false;
broadcast(Host, _Node, _NodeId, _Type, NodeOptions, Subscriptions, Stanza, SHIM) ->
    From = service_jid(Host),
    Message = case get_option(NodeOptions, notification_type, headline) of
	normal -> Stanza;
	MsgType -> add_message_type(Stanza, atom_to_list(MsgType))
	end,
    lists:foreach(fun({LJID, NodeName, SubIds}) ->
		Send = case {SHIM, SubIds} of
		    {false, _} -> Message;
		    {true, [_]} -> add_shim_headers(Message, collection_shim(NodeName));
		    {true, _} -> add_shim_headers(Message, lists:append(collection_shim(NodeName), subid_shim(SubIds)))
 		    end,
		ejabberd_router:route(From, jlib:make_jid(LJID), Send)
	end, Subscriptions),
    true.

node_subscriptions(Host, Node, NodeId, Type, _NodeOptions, Notify) ->
    % TODO temporary dirty condition, should be improved using plugin or node options
    case Type of
	?STDNODE -> node_subscriptions_bare(Host, Node, NodeId, Type);
	?PEPNODE -> node_subscriptions_bare(Host, Node, NodeId, Type);
	_ -> node_subscriptions_full(Host, Node, Notify)
    end.

node_subscriptions_bare(Host, Node, NodeId, Type) ->
    case node_action(Host, Type, get_node_subscriptions, [NodeId]) of
	{result, Subs} ->
	    SubsByJid = lists:foldl(
		fun({JID, subscribed, SubId}, Acc) ->
			case dict:is_key(JID, Acc) of
			    true -> dict:append(JID, SubId, Acc);
			    false -> dict:store(JID, [SubId], Acc)
			end;
		    (_, Acc) ->
			Acc
		end, dict:new(), Subs),
	    [{J, Node, S} || {J, S} <- dict:to_list(SubsByJid)];
 	_ ->
	    []
     end.

node_subscriptions_full(Host, Node, NotifyType) ->
     Action = fun() ->
	    Collection = tree_call(Host, get_parentnodes_tree, [Host, Node, service_jid(Host)]),
	    {result, [{Depth, [{N, sub_with_options(N)} || N <- Nodes]} || {Depth, Nodes} <- Collection]}
	end,
     case transaction(Host, Action, sync_dirty) of
	{result, CollSubs} -> subscribed_nodes_by_jid(NotifyType, CollSubs);
	_ -> []
     end.

 subscribed_nodes_by_jid(NotifyType, SubsByDepth) ->
     NodesToDeliver = fun(Depth, Node, Subs, Acc) ->
	     NodeName = case Node#pubsub_node.id of
		 {_, N} -> N;
		 Other -> Other
	     end,
	     NodeOptions = Node#pubsub_node.options,
	     lists:foldl(fun({LJID, SubId, SubOptions}, {JIDs, Recipients}) ->
			 case is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) of
			     true  ->
				 %% If is to deliver :
				 case state_can_deliver(LJID, SubOptions) of
				     []            -> {JIDs, Recipients};
				     JIDsToDeliver ->
					 lists:foldl(
					     fun(JIDToDeliver, {JIDsAcc, RecipientsAcc}) ->
						     case lists:member(JIDToDeliver, JIDs) of
							 %% check if the JIDs co-accumulator contains the Subscription JID,
							 false ->
							     %%  - if not,
							     %%  - add the JID to JIDs list co-accumulator ;
							     %%  - create a tuple of the JID, NodeId, and SubId (as list),
							     %%    and add the tuple to the Recipients list co-accumulator
							     {[JIDToDeliver | JIDsAcc], [{JIDToDeliver, NodeName, [SubId]} | RecipientsAcc]};
							 true ->
							     %% - if the JIDs co-accumulator contains the JID
							     %%   get the tuple containing the JID from the Recipient list co-accumulator
							     {_, {JIDToDeliver, NodeName1, SubIds}} = lists:keysearch(JIDToDeliver, 1, RecipientsAcc),
							     %%   delete the tuple from the Recipients list
							     % v1 : Recipients1 = lists:keydelete(LJID, 1, Recipients),
							     % v2 : Recipients1 = lists:keyreplace(LJID, 1, Recipients, {LJID, NodeId1, [SubId | SubIds]}),
							     %%   add the SubId to the SubIds list in the tuple,
							     %%   and add the tuple back to the Recipients list co-accumulator
							     % v1.1 : {JIDs, lists:append(Recipients1, [{LJID, NodeId1, lists:append(SubIds, [SubId])}])}
							     % v1.2 : {JIDs, [{LJID, NodeId1, [SubId | SubIds]} | Recipients1]}
							     % v2: {JIDs, Recipients1}
							     {JIDsAcc, lists:keyreplace(JIDToDeliver, 1, RecipientsAcc, {JIDToDeliver, NodeName1, [SubId | SubIds]})}
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

sub_with_options(#pubsub_node{type = Type, id = NodeId}) ->
    case node_call(Type, get_node_subscriptions, [NodeId]) of
	{result, Subs} ->
	    lists:foldl(
		fun({JID, subscribed, SubId}, Acc) -> [sub_with_options(JID, NodeId, SubId) | Acc];
		    (_, Acc) -> Acc
		end, [], Subs);
	_ ->
	    []
    end.
sub_with_options(JID, NodeId, SubId) ->
	case pubsub_subscription_odbc:read_subscription(JID, NodeId, SubId) of
	{result, #pubsub_subscription{options = Options}} -> {JID, SubId, Options};
	_ -> {JID, SubId, []}
    end.

user_resources(User, Server) ->
    ejabberd_sm:get_user_resources(User, Server).

user_resource(User, Server, undefined) ->
    case user_resources(User, Server) of
	[R|_] -> R;
	_ ->  []
    end;
user_resource(_, _, Resource) ->
    Resource.

%%%%%%% Configuration handling

%%<p>There are several reasons why the default node configuration options request might fail:</p>
%%<ul>
%%<li>The service does not support node configuration.</li>
%%<li>The service does not support retrieval of default node configuration.</li>
%%</ul>
get_configure(Host, ServerHost, Node, #jid{node = User, domain = Server} = From, Lang) ->
    Action =
	fun(#pubsub_node{options = Options, type = Type, idx = Nidx}) ->
		case node_call(Type, get_affiliation, [Nidx, From]) of
		    {result, owner} ->
		  Groups = case lists:member(binary_to_list(Server), ?MYHOSTS) of
		true ->
		    %Roster_Items = ejabberd_hooks:run_fold(roster_get, ServerHost, [], [{User,Server}]),
		    Roster_Groups = lists:foldl(fun
		      (#roster{groups = []}, Acc) ->          Acc;
		      (#roster{groups = Item_Groups}, Acc) -> [Item_Groups | Acc]
		    end, [], ejabberd_hooks:run_fold(roster_get, ServerHost, [], [{User,Server}])),
		    case Roster_Groups of
		  [[[]]] -> [];
		  _      -> lists:usort(Roster_Groups)
		    end;
		false ->
		    []
		  end,
			{result,
			 #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'pubsub', children =
				[#xmlel{ns = ?NS_PUBSUB_OWNER, name = 'configure', attrs =
					nodeAttr(Node), children =
					[#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
						[?XMLATTR(<<"type">>, <<"form">>)], children =
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
			    [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = [?XMLATTR(<<"type">>, <<"form">>)], children =
				    get_configure_xfields(Type, Options, Lang, [])
				   }]}]}}.

%% Get node option
%% The result depend of the node type plugin system.
-spec(get_option/2 ::
      (
		   Options :: [] | [Option::nodeOption()],
		   Key     :: atom())
      -> Value :: 'false' | term()
	    ).

get_option([], _) -> false;
get_option(Options, Key) ->
    get_option(Options, Key, false).


-spec(get_option/3 ::
      (
		   Options :: [] | [Option::nodeOption()],
		   Key     :: atom(),
		   Default :: term())
      -> Value :: term()
	    ).

get_option(Options, Key, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> Value;
	_ -> Default
    end.

%% Get default options from the module plugin.
-spec(node_options/1 ::
      (
		     Type::nodeType())
      -> Options::[Option::nodeOption()]
	    ).

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
-spec(max_items/2 ::
      (
		  Host    :: host(),
		  Options :: [Option::nodeOption()])
      -> MaxItems :: integer() | 'unlimited'
	    ).

max_items(Host, Options) ->
    case get_option(Options, 'persist_items') of
	true ->
	    case get_option(Options, 'max_items') of
		false -> 'unlimited';
		Result when (Result < 0) -> 0;
		Result -> Result
	    end;
	false ->
	    case get_option(Options, 'send_last_published_item') of
		'never' ->
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
     ?BOOL_CONFIG_FIELD("Notify owners about new subscribers and unsubscribers", notify_sub),
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
     ?NLIST_CONFIG_FIELD("The collections with which a node is affiliated", collection),
     ?STRING_CONFIG_FIELD("The type of node data, usually specified by the namespace of the payload (if any)", type)
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
	    case exmpp_xml:get_attribute_as_list(XEl, <<"type">>, undefined) of
		"cancel" ->
		    {result, []};
		"submit" ->
		    Action =
			fun(#pubsub_node{options = Options, type = Type, idx = Nidx} = N) ->
				case node_call(Type, get_affiliation, [Nidx, From]) of
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
			    Nidx = TNode#pubsub_node.idx,
			    Type = TNode#pubsub_node.type,
			    Options = TNode#pubsub_node.options,
			    broadcast_config_notification(Host, Node, Nidx, Type, Options, Lang),
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
set_xoption(Host, [{"pubsub#notify_sub", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_sub, Val);
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


-spec(get_max_items_node/1 ::
      (
			   Host :: host())
      -> MaxItems::integer()
	    ).

get_max_items_node({_, ServerHost, _}) ->
    get_max_items_node(ServerHost);
get_max_items_node(Host) ->
    case catch ets:lookup(gen_mod:get_module_proc(Host, 'config'), 'max_items_node') of
	[{'max_items_node', Integer}] -> Integer;
	_ -> ?MAXITEMS
    end.

%%%% last item cache handling
-spec(is_last_item_cache_enabled/1 ::
      (
				   Host :: string() | host())
      -> IsLastItemCacheEnabled::boolean()
	    ).

is_last_item_cache_enabled({_, ServerHost, _}) ->
    is_last_item_cache_enabled(binary_to_list(ServerHost));
is_last_item_cache_enabled(Host) ->
    case catch ets:lookup(gen_mod:get_module_proc(Host, 'config'), 'last_item_cache') of
	[{'last_item_cache', true}] -> true;
	_ -> false
    end.


-spec(set_cached_item/5 ::
      (
			Host      :: string() | host(),
			NodeId    :: nodeIdx(),
			ItemId    :: itemId(),
			Publisher :: jidEntity(),
			Payload   :: payload())
      -> 'ok' | {'aborted', Reason::_}
	    ).

set_cached_item({_, ServerHost, _}, NodeId, ItemId, Publisher, Payload) ->
    set_cached_item(binary_to_list(ServerHost), NodeId, ItemId, Publisher, Payload);
set_cached_item(Host, NodeId, ItemId, #jid{node = U, domain = S} = _Publisher, Payload) ->
    case is_last_item_cache_enabled(Host) of
	true ->
	    mnesia:dirty_write({pubsub_last_item, NodeId, ItemId, {now(), {U,S,undefined}}, Payload});
	_ -> ok
    end.


-spec(unset_cached_item/2 ::
      (
			  Host   :: string() | host(),
			  NodeId :: nodeIdx())
      -> 'ok' | {'aborted', Reason::_}
	    ).

unset_cached_item({_, ServerHost, _}, NodeId) ->
    unset_cached_item(binary_to_list(ServerHost), NodeId);
unset_cached_item(Host, NodeId) ->
    case is_last_item_cache_enabled(Host) of
	true -> mnesia:dirty_delete({pubsub_last_item, NodeId});
	_ -> ok
    end.


-spec(get_cached_item/2 ::
      (
			Host   :: string() | host(),
			NodeId :: nodeIdx())
      -> CachedItem :: 'undefined' | pubsubItem()
	    ).

get_cached_item({_, ServerHost, _}, NodeId) ->
    get_cached_item(binary_to_list(ServerHost), NodeId);
get_cached_item(Host, NodeId) ->
    case is_last_item_cache_enabled(Host) of
	true ->
	    case mnesia:dirty_read({pubsub_last_item, NodeId}) of %% TODO
		[{pubsub_last_item, NodeId, ItemId, Creation, Payload}] ->
		    #pubsub_item{id = {ItemId, NodeId}, payload = Payload,
				 creation = Creation, modification = Creation};
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.

%%%% plugin handling
-spec(host/1 ::
      (
	     ServerHost :: string() | binary())
      -> Host::string()
	    ).

host(ServerHost) ->
    case catch ets:lookup(gen_mod:get_module_proc(ServerHost, 'config'), 'host') of
	[{'host', Host}] -> Host;
	_ -> "pubsub."++ServerHost
    end.


-spec(plugins/1 ::
      (
		Host :: binary() | string())
      -> Plugins::[Plugin::nodeType()]
	    ).

plugins(Host) when is_binary(Host) ->
    plugins(binary_to_list(Host));
plugins(Host) when is_list(Host) ->
    case catch ets:lookup(gen_mod:get_module_proc(Host, 'config'), 'plugins') of
	[{'plugins', []}] -> [?STDNODE];
	[{'plugins', Plugins}] -> Plugins;
	_ -> [?STDNODE]
    end.


-spec(select_type/4 ::
      (
		    ServerHost :: binary() | string(),
		    Host       :: host(),
		    NodeId     :: nodeId(),
		    Type       :: nodeType())
      -> SelectedType::nodeType()
	    ).

select_type(ServerHost, Host, NodeId, Type) when is_binary(ServerHost) ->
    select_type(binary_to_list(ServerHost), Host, NodeId, Type);
select_type(ServerHost, Host, NodeId, Type) ->
    SelectedType = case Host of
		       {_User, _Server, _Resource} -> 
			   case catch ets:lookup(gen_mod:get_module_proc(ServerHost, 'config'), 'pep_mapping') of
			       [{'pep_mapping', PepMapping}] ->
				   proplists:get_value(node_to_string(NodeId), PepMapping, ?PEPNODE);
			       _ -> ?PEPNODE
			   end;
		       _ -> Type
		   end,
						%ConfiguredTypes = plugins(ServerHost),
    case lists:member(SelectedType, ConfiguredTypes = plugins(ServerHost)) of
	true -> SelectedType;
	false -> hd(ConfiguredTypes)
    end.


-spec(select_type/3 ::
      (
		    ServerHost :: binary() | string(),
		    Host       :: host(),
		    NodeId     :: nodeId())
      -> SelectedType::nodeType()
	    ).

select_type(ServerHost, Host, NodeId) -> 
    select_type(ServerHost, Host, NodeId, hd(plugins(ServerHost))).


-spec(features/0 :: () -> Features::[Feature::string()]).

features() ->
    [
						% see plugin "access-authorize",   % OPTIONAL
     "access-open",   % OPTIONAL this relates to access_model option in node_flat
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


-spec(features/1 ::
      (
		 Type::nodeType())
      -> Features::[Feature::string()]
	    ).

features(Type) ->
    Module = list_to_atom(?PLUGIN_PREFIX++Type),
    features() ++ case catch Module:features() of
		      {'EXIT', {undef, _}} -> [];
		      Result -> Result
		  end.


-spec(features/2 ::
      (
		 Host   :: string() | binary(),
		 NodeId :: nodeId())
      -> Features::[Feature::string()]
	    ).

features(Host, <<>> = _NodeId) ->
    lists:usort(lists:foldl(fun(Plugin, Acc) ->
				    Acc ++ features(Plugin)
			    end, [], plugins(Host)));
features(Host, NodeId) ->
    Action = fun(#pubsub_node{type = Type}) -> {result, features(Type)} end,
    case transaction(Host, NodeId, Action, sync_dirty) of
	{result, Features} -> lists:usort(features() ++ Features);
	_ -> features()
    end.

%% @doc <p>node tree plugin call.</p>
-spec(tree_call/3 ::
      (
		  Host     :: string() | host(),
		  Function :: atom(),
		  Args     :: [term()])
      -> any() %% TODO
	    ).

tree_call({_User, Server, _Resource}, Function, Args) ->
    tree_call(Server, Function, Args);
tree_call(Host, Function, Args) ->
    ?DEBUG("tree_call ~p ~p ~p",[Host, Function, Args]),
    Module = case catch ets:lookup(gen_mod:get_module_proc(Host, 'config'), 'nodetree') of
		 [{'nodetree', NodeTree}] -> NodeTree;
		 _ -> list_to_atom(?TREE_PREFIX ++ ?STDTREE)
	     end,
    catch apply(Module, Function, Args).


-spec(tree_action/3 ::
      (
		    Host     :: string() | host(),
		    Function :: atom(),
		    Args     :: [term()])
      -> any() %% TODO
	    ).

tree_action(Host, Function, Args) ->
    ?DEBUG("tree_action ~p ~p ~p",[Host,Function,Args]),
    Fun = fun() -> tree_call(Host, Function, Args) end,
    case catch ejabberd_odbc:sql_bloc(odbc_conn(Host), Fun) of
    {atomic, Result} -> 
	Result;
    {aborted, Reason} -> 
	?ERROR_MSG("transaction return internal error: ~p~n",[{aborted, Reason}]),
	{error, 'internal-server-error'}
    end.

%% @doc <p>node plugin call.</p>
-spec(node_call/3 ::
      (
		  Type     :: nodeType(),
		  Function :: atom(),
		  Args     :: [term()])
      -> {'result', Result::_} | {'error', Error::_} %% TODO
	    ).

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


-spec(node_action/4 ::
      (
		    Host     :: string() | host(),
		    Type     :: nodeType(),
		    Function :: atom(),
		    Args     :: [term()])
      -> any() %% TODO
	    ).

node_action(Host, Type, Function, Args) ->
    ?DEBUG("node_action ~p ~p ~p ~p",[Host,Type,Function,Args]),
    transaction(Host, fun() ->
			node_call(Type, Function, Args)
		end, sync_dirty).

%% @doc <p>plugin transaction handling.</p>
-spec(transaction/4 ::
      (
		    Host   :: string() | host(),
		    NodeId :: nodeId(),
		    Action :: fun(),
				 Trans  :: atom())
      -> {'result', {Node::pubsubNode(), Result::_}} | {'error', Error::_}
	    ).

transaction(Host, NodeId, Action, Trans) ->
    transaction(Host, fun() ->
			case tree_call(Host, get_node, [Host, NodeId]) of
			    #pubsub_node{} = Node ->
				case Action(Node) of
				    {result, Result} -> {result, {Node, Result}};
				    {atomic, {result, Result}} -> {result, {Node, Result}};
				    Other -> Other
				end;
			    Error ->
				Error
			end
		end, Trans).


-spec(transaction_on_nodes/3 ::
      (
		    Host   :: string() | host(),
		    Action :: fun(),
				 Trans  :: atom())
      -> {'result', Nodes :: [] | [Node::pubsubNode()]}
	    ).

transaction_on_nodes(Host, Action, Trans) ->
    transaction(Host, fun() ->
			{result, lists:foldl(Action, [], tree_call(Host, get_nodes, [Host]))}
		end, Trans).


-spec(transaction/3 ::
      (
		    Host   :: string() | host(),
		    Fun   :: fun(),
				Trans :: atom())
      -> {'result', Result::_} | {'error', Error::_}
	    ).

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
	    {error, 'internal-server-error'};
	{'EXIT', {timeout, _} = Reason} ->
	    case Count of
		0 ->
		    ?ERROR_MSG("transaction return internal error: ~p~n", [{'EXIT', Reason}]),
		    {error, 'internal-server-error'};
		N ->
		    erlang:yield(),
		    transaction_retry(Host, Fun, Trans, N-1)
	    end;
	{'EXIT', Reason} ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [{'EXIT', Reason}]),
	    {error, 'internal-server-error'};
	Other ->
	    ?ERROR_MSG("transaction return internal error: ~p~n", [Other]),
	    {error, 'internal-server-error'}
    end.

odbc_conn({_U, Host, _R})->
    Host;
odbc_conn(Host) ->
    lists:dropwhile(fun(A) -> A/=$. end, Host) -- ".".

%% escape value for database storage
escape({_U, _H, _R}=JID)->
    ejabberd_odbc:escape(exmpp_jid:to_list(JID));
escape(Value)->
    ejabberd_odbc:escape(Value).
%%%% helpers

%% Add pubsub-specific error element
extended_error(Error, Ext) ->
    extended_error(Error, Ext, []).
extended_error(Error, unsupported, Feature) ->
    extended_error(Error, unsupported,
		   [?XMLATTR(<<"feature">>, Feature)]);
extended_error(Error, Ext, ExtAttrs) ->
						%Pubsub_Err = #xmlel{ns = ?NS_PUBSUB_ERRORS, name = Ext, attrs = ExtAttrs},
    exmpp_xml:append_child(exmpp_stanza:error(?NS_JABBER_CLIENT, Error),
			   #xmlel{ns = ?NS_PUBSUB_ERRORS, name = Ext, attrs = ExtAttrs}).

%% Give a uniq identifier
-spec(uniqid/0 :: () -> Id::binary()).

uniqid() ->
    {T1, T2, T3} = now(),
    list_to_binary(lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3]))).

						% node attributes
nodeAttr(Node) when is_list(Node) ->
    [?XMLATTR(<<"node">>, Node)];
nodeAttr(Node) ->
    [?XMLATTR(<<"node">>, node_to_string(Node))].

						% item attributes
itemAttr([]) -> [];
itemAttr(ItemId) -> [?XMLATTR(<<"id">>, ItemId)].


						% build item elements from item list
-spec(itemsEls/1 ::
      (
		 PubsubItems::[PubsubItem::pubsubItem()])
      -> Items::[Item::#xmlel{}]
	    ).

itemsEls(PubsubItems) ->
    [#xmlel{ns = ?NS_PUBSUB, name = 'item', attrs = itemAttr(ItemId), children = Payload}
     || #pubsub_item{id = {ItemId, _}, payload = Payload} <- PubsubItems].


add_message_type(#xmlel{name='message'} = El, Type) -> exmpp_stanza:set_type(El, Type);
add_message_type(El, _Type)  -> El.

%% Place of <headers/> changed at the bottom of the stanza
%% cf. http://xmpp.org/extensions/xep-0060.html#publisher-publish-success-subid
%%
%% "[SHIM Headers] SHOULD be included after the event notification information
%% (i.e., as the last child of the <message/> stanza)".

add_shim_headers(Stanza, HeaderEls) ->
    add_headers(Stanza, "headers", ?NS_SHIM, HeaderEls).

add_extended_headers(Stanza, HeaderEls) ->
    add_headers(Stanza, "addresses", ?NS_ADDRESS, HeaderEls).

add_headers(#xmlel{children = Els} = Stanza, HeaderName, HeaderNS, HeaderEls) ->
    HeaderEl = #xmlel{name = HeaderName, ns = HeaderNS, children = HeaderEls},
    Stanza#xmlel{children = lists:append(Els, [HeaderEl])}.

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
    [#xmlel{ns = ?NS_PUBSUB, name ='header',
	    attrs = [?XMLATTR(<<"name">>, <<"Collection">>)],
	    children = [?XMLCDATA(node_to_string(Node))]}].

subid_shim(SubIds) ->
    [#xmlel{ns = ?NS_PUBSUB, name ='header',
	    attrs = [?XMLATTR(<<"name">>, <<"SubId">>)],
	    children = [?XMLCDATA(SubId)]}
     || SubId <- SubIds].


extended_headers(JIDs) ->
    [#xmlel{ns = ?NS_ADDRESS, name = 'address',
	    attrs = [?XMLATTR(<<"type">>, <<"replyto">>), ?XMLATTR(<<"jid">>, JID)]}
     || JID <- JIDs].

on_user_offline(_, JID, _) ->
    {User, Server, Resource} = jlib:short_prepd_jid(JID),
    case user_resources(User, Server) of
	[] -> purge_offline({User, Server, Resource});
	_  -> true
    end.

purge_offline({User, Server, _} = LJID) ->
    JID = exmpp_jid:make(User, Server),
    Host = host(Server),
    Plugins = plugins(Host),
    Result = lists:foldl(
	       fun(Type, {Status, Acc}) ->
		       case lists:member("retrieve-affiliations", features(Type)) of
			   false ->
			       {{error, extended_error('feature-not-implemented', unsupported, "retrieve-affiliations")}, Acc};
			   true ->
			       {result, Affiliations} = node_action(Host, Type, get_entity_affiliations, [Host, JID]),
			       {Status, [Affiliations|Acc]}
		       end
	       end, {ok, []}, Plugins),
    case Result of
	{ok, Affiliations} ->
	    lists:foreach(
	      fun({#pubsub_node{id = {_, NodeId}, options = Options, type = Type}, Affiliation})
		 when Affiliation == 'owner' orelse Affiliation == 'publisher' ->
		      Action = fun(#pubsub_node{type = NType, idx = Nidx}) ->
				       node_call(NType, get_items, [Nidx, service_jid(Host)])
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
					fun(#pubsub_item{id = {ItemId, _}, modification = {_, Modification}}) ->
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

notify_owners(false, _, _, _, _, _) -> true;
notify_owners(true, JID, Host, Node, Owners, State) ->
    Message = #xmlel{name = 'message', ns = ?NS_JABBER_CLIENT,
		     children = [#xmlel{name = 'pubsub', ns = ?NS_PUBSUB,
					children = [#xmlel{name = 'subscription', ns = ?NS_PUBSUB,
							   attrs = [?XMLATTR(<<"node">>, Node),
								    ?XMLATTR(<<"jid">>, exmpp_jid:prep_to_binary(exmpp_jid:make(JID))),
								    ?XMLATTR(<<"subscription">>, State)]}]}]},
    lists:foreach(
      fun(Owner) ->
	      ejabberd_router:route(exmpp_jid:make(Host), exmpp_jid:make(Owner), Message)
      end, Owners).
