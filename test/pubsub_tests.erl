%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 16 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(pubsub_tests).

%% API
-compile(export_all).
-import(suite, [pubsub_jid/1, send_recv/2, get_features/2, disconnect/1,
		put_event/2, get_event/1, wait_for_master/1, wait_for_slave/1,
		recv_message/1, my_jid/1, send/2, recv_presence/1, recv/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {pubsub_single, [sequence],
     [single_test(test_features),
      single_test(test_vcard),
      single_test(test_create),
      single_test(test_configure),
      single_test(test_delete),
      single_test(test_get_affiliations),
      single_test(test_get_subscriptions),
      single_test(test_create_instant),
      single_test(test_default),
      single_test(test_create_configure),
      single_test(test_publish),
      single_test(test_auto_create),
      single_test(test_get_items),
      single_test(test_delete_item),
      single_test(test_purge),
      single_test(test_subscribe),
      single_test(test_unsubscribe)]}.

test_features(Config) ->
    PJID = pubsub_jid(Config),
    AllFeatures = sets:from_list(get_features(Config, PJID)),
    NeededFeatures = sets:from_list(
		       [?NS_PUBSUB,
			?PUBSUB("access-open"),
			?PUBSUB("access-authorize"),
			?PUBSUB("create-nodes"),
			?PUBSUB("instant-nodes"),
			?PUBSUB("config-node"),
			?PUBSUB("retrieve-default"),
			?PUBSUB("create-and-configure"),
			?PUBSUB("publish"),
			?PUBSUB("auto-create"),
			?PUBSUB("retrieve-items"),
			?PUBSUB("delete-items"),
			?PUBSUB("subscribe"),
			?PUBSUB("retrieve-affiliations"),
			?PUBSUB("modify-affiliations"),
			?PUBSUB("retrieve-subscriptions"),
			?PUBSUB("manage-subscriptions"),
			?PUBSUB("purge-nodes"),
			?PUBSUB("delete-nodes")]),
    true = sets:is_subset(NeededFeatures, AllFeatures),
    disconnect(Config).

test_vcard(Config) ->
    JID = pubsub_jid(Config),
    ct:comment("Retreiving vCard from ~s", [jid:encode(JID)]),
    #iq{type = result, sub_els = [#vcard_temp{}]} =
	send_recv(Config, #iq{type = get, to = JID, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

test_create(Config) ->
    Node = ?config(pubsub_node, Config),
    Node = create_node(Config, Node),
    disconnect(Config).

test_create_instant(Config) ->
    Node = create_node(Config, <<>>),
    delete_node(Config, Node),
    disconnect(Config).

test_configure(Config) ->
    Node = ?config(pubsub_node, Config),
    NodeTitle = ?config(pubsub_node_title, Config),
    NodeConfig = get_node_config(Config, Node),
    MyNodeConfig = set_opts(NodeConfig,
			    [{title, NodeTitle}]),
    set_node_config(Config, Node, MyNodeConfig),
    NewNodeConfig = get_node_config(Config, Node),
    NodeTitle = proplists:get_value(title, NewNodeConfig),
    disconnect(Config).

test_default(Config) ->
    get_default_node_config(Config),
    disconnect(Config).

test_create_configure(Config) ->
    NodeTitle = ?config(pubsub_node_title, Config),
    DefaultNodeConfig = get_default_node_config(Config),
    CustomNodeConfig = set_opts(DefaultNodeConfig,
				[{title, NodeTitle}]),
    Node = create_node(Config, <<>>, CustomNodeConfig),
    NodeConfig = get_node_config(Config, Node),
    NodeTitle = proplists:get_value(title, NodeConfig),
    delete_node(Config, Node),
    disconnect(Config).

test_publish(Config) ->
    Node = create_node(Config, <<>>),
    publish_item(Config, Node),
    delete_node(Config, Node),
    disconnect(Config).

test_auto_create(Config) ->
    Node = randoms:get_string(),
    publish_item(Config, Node),
    delete_node(Config, Node),
    disconnect(Config).

test_get_items(Config) ->
    Node = create_node(Config, <<>>),
    ItemsIn = [publish_item(Config, Node) || _ <- lists:seq(1, 5)],
    ItemsOut = get_items(Config, Node),
    true = [I || #ps_item{id = I} <- lists:sort(ItemsIn)]
	== [I || #ps_item{id = I} <- lists:sort(ItemsOut)],
    delete_node(Config, Node),
    disconnect(Config).

test_delete_item(Config) ->
    Node = create_node(Config, <<>>),
    #ps_item{id = I} = publish_item(Config, Node),
    [#ps_item{id = I}] = get_items(Config, Node),
    delete_item(Config, Node, I),
    [] = get_items(Config, Node),
    delete_node(Config, Node),
    disconnect(Config).

test_subscribe(Config) ->
    Node = create_node(Config, <<>>),
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    [#ps_subscription{node = Node}] = get_subscriptions(Config),
    delete_node(Config, Node),
    disconnect(Config).

test_unsubscribe(Config) ->
    Node = create_node(Config, <<>>),
    subscribe_node(Config, Node),
    [#ps_subscription{node = Node}] = get_subscriptions(Config),
    unsubscribe_node(Config, Node),
    [] = get_subscriptions(Config),
    delete_node(Config, Node),
    disconnect(Config).

test_get_affiliations(Config) ->
    Nodes = lists:sort([create_node(Config, <<>>) || _ <- lists:seq(1, 5)]),
    Affs = get_affiliations(Config),
    Nodes = lists:sort([Node || #ps_affiliation{node = Node,
						type = owner} <- Affs]),
    [delete_node(Config, Node) || Node <- Nodes],
    disconnect(Config).

test_get_subscriptions(Config) ->
    Nodes = lists:sort([create_node(Config, <<>>) || _ <- lists:seq(1, 5)]),
    [subscribe_node(Config, Node) || Node <- Nodes],
    Subs = get_subscriptions(Config),
    Nodes = lists:sort([Node || #ps_subscription{node = Node} <- Subs]),
    [delete_node(Config, Node) || Node <- Nodes],
    disconnect(Config).

test_purge(Config) ->
    Node = create_node(Config, <<>>),
    ItemsIn = [publish_item(Config, Node) || _ <- lists:seq(1, 5)],
    ItemsOut = get_items(Config, Node),
    true = [I || #ps_item{id = I} <- lists:sort(ItemsIn)]
	== [I || #ps_item{id = I} <- lists:sort(ItemsOut)],
    purge_node(Config, Node),
    [] = get_items(Config, Node),
    delete_node(Config, Node),
    disconnect(Config).

test_delete(Config) ->
    Node = ?config(pubsub_node, Config),
    delete_node(Config, Node),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {pubsub_master_slave, [sequence],
     [master_slave_test(publish),
      master_slave_test(subscriptions),
      master_slave_test(affiliations),
      master_slave_test(authorize)]}.

publish_master(Config) ->
    Node = create_node(Config, <<>>),
    put_event(Config, Node),
    wait_for_slave(Config),
    #ps_item{id = ID} = publish_item(Config, Node),
    #ps_item{id = ID} = get_event(Config),
    delete_node(Config, Node),
    disconnect(Config).

publish_slave(Config) ->
    Node = get_event(Config),
    subscribe_node(Config, Node),
    wait_for_master(Config),
    #message{
       sub_els =
	   [#ps_event{
	       items = #ps_items{node = Node,
				 items = [Item]}}]} = recv_message(Config),
    put_event(Config, Item),
    disconnect(Config).

subscriptions_master(Config) ->
    Peer = ?config(slave, Config),
    Node = ?config(pubsub_node, Config),
    Node = create_node(Config, Node),
    [] = get_subscriptions(Config, Node),
    wait_for_slave(Config),
    lists:foreach(
      fun(Type) ->
	      ok = set_subscriptions(Config, Node, [{Peer, Type}]),
	      #ps_item{} = publish_item(Config, Node),
	      case get_subscriptions(Config, Node) of
		  [] when Type == none; Type == pending ->
		      ok;
		  [#ps_subscription{jid = Peer, type = Type}] ->
		      ok
	      end
      end, [subscribed, unconfigured, pending, none]),
    delete_node(Config, Node),
    disconnect(Config).

subscriptions_slave(Config) ->
    wait_for_master(Config),
    MyJID = my_jid(Config),
    Node = ?config(pubsub_node, Config),
    lists:foreach(
      fun(subscribed = Type) ->
	      ?recv2(#message{
			sub_els =
			    [#ps_event{
				subscription = #ps_subscription{
						  node = Node,
						  jid = MyJID,
						  type = Type}}]},
		     #message{sub_els = [#ps_event{}]});
	 (Type) ->
	      #message{
		 sub_els =
		     [#ps_event{
			 subscription = #ps_subscription{
					   node = Node,
					   jid = MyJID,
					   type = Type}}]} =
		  recv_message(Config)
      end, [subscribed, unconfigured, pending, none]),
    disconnect(Config).

affiliations_master(Config) ->
    Peer = ?config(slave, Config),
    BarePeer = jid:remove_resource(Peer),
    lists:foreach(
      fun(Aff) ->
	      Node = <<(atom_to_binary(Aff, utf8))/binary,
		       $-, (randoms:get_string())/binary>>,
	      create_node(Config, Node, default_node_config(Config)),
	      #ps_item{id = I} = publish_item(Config, Node),
	      ok = set_affiliations(Config, Node, [{Peer, Aff}]),
	      Affs = get_affiliations(Config, Node),
	      case lists:keyfind(BarePeer, #ps_affiliation.jid, Affs) of
		  false when Aff == none ->
		      ok;
		  #ps_affiliation{type = Aff} ->
		      ok
	      end,
	      put_event(Config, {Aff, Node, I}),
	      wait_for_slave(Config),
	      delete_node(Config, Node)
      end, [outcast, none, member, publish_only, publisher, owner]),
    put_event(Config, disconnect),
    disconnect(Config).

affiliations_slave(Config) ->
    affiliations_slave(Config, get_event(Config)).

affiliations_slave(Config, {outcast, Node, ItemID}) ->
    #stanza_error{reason = 'forbidden'} = subscribe_node(Config, Node),
    #stanza_error{} = unsubscribe_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_items(Config, Node),
    #stanza_error{reason = 'forbidden'} = publish_item(Config, Node),
    #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    affiliations_slave(Config, get_event(Config));
affiliations_slave(Config, {none, Node, ItemID}) ->
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    ok = unsubscribe_node(Config, Node),
    %% This violates the affiliation char from section 4.1
    [_|_] = get_items(Config, Node),
    #stanza_error{reason = 'forbidden'} = publish_item(Config, Node),
    #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    affiliations_slave(Config, get_event(Config));
affiliations_slave(Config, {member, Node, ItemID}) ->
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    ok = unsubscribe_node(Config, Node),
    [_|_] = get_items(Config, Node),
    #stanza_error{reason = 'forbidden'} = publish_item(Config, Node),
    #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    affiliations_slave(Config, get_event(Config));
affiliations_slave(Config, {publish_only, Node, ItemID}) ->
    #stanza_error{reason = 'forbidden'} = subscribe_node(Config, Node),
    #stanza_error{} = unsubscribe_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_items(Config, Node),
    #ps_item{id = _MyItemID} = publish_item(Config, Node),
    %% BUG: This should be fixed
    %% ?match(ok, delete_item(Config, Node, MyItemID)),
    #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    affiliations_slave(Config, get_event(Config));
affiliations_slave(Config, {publisher, Node, _ItemID}) ->
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    ok = unsubscribe_node(Config, Node),
    [_|_] = get_items(Config, Node),
    #ps_item{id = MyItemID} = publish_item(Config, Node),
    ok = delete_item(Config, Node, MyItemID),
    %% BUG: this should be fixed
    %% #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    affiliations_slave(Config, get_event(Config));
affiliations_slave(Config, {owner, Node, ItemID}) ->
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    ok = unsubscribe_node(Config, Node),
    [_|_] = get_items(Config, Node),
    #ps_item{id = MyItemID} = publish_item(Config, Node),
    ok = delete_item(Config, Node, MyItemID),
    ok = delete_item(Config, Node, ItemID),
    ok = purge_node(Config, Node),
    [_|_] = get_node_config(Config, Node),
    ok = set_node_config(Config, Node, default_node_config(Config)),
    ok = set_subscriptions(Config, Node, []),
    [] = get_subscriptions(Config, Node),
    ok = set_affiliations(Config, Node, [{Peer, outcast}, {MyJID, owner}]),
    [_, _] = get_affiliations(Config, Node),
    ok = delete_node(Config, Node),
    wait_for_master(Config),
    affiliations_slave(Config, get_event(Config));
affiliations_slave(Config, disconnect) ->
    disconnect(Config).

authorize_master(Config) ->
    send(Config, #presence{}),
    #presence{} = recv_presence(Config),
    Peer = ?config(slave, Config),
    PJID = pubsub_jid(Config),
    NodeConfig = set_opts(default_node_config(Config),
			  [{access_model, authorize}]),
    Node = ?config(pubsub_node, Config),
    Node = create_node(Config, Node, NodeConfig),
    wait_for_slave(Config),
    #message{sub_els = [#xdata{fields = F1}]} = recv_message(Config),
    C1 = pubsub_subscribe_authorization:decode(F1),
    Node = proplists:get_value(node, C1),
    Peer = proplists:get_value(subscriber_jid, C1),
    %% Deny it at first
    Deny = #xdata{type = submit,
		  fields = pubsub_subscribe_authorization:encode(
			     [{node, Node},
			      {subscriber_jid, Peer},
			      {allow, false}])},
    send(Config, #message{to = PJID, sub_els = [Deny]}),
    %% We should not have any subscriptions
    [] = get_subscriptions(Config, Node),
    wait_for_slave(Config),
    #message{sub_els = [#xdata{fields = F2}]} = recv_message(Config),
    C2 = pubsub_subscribe_authorization:decode(F2),
    Node = proplists:get_value(node, C2),
    Peer = proplists:get_value(subscriber_jid, C2),
    %% Now we accept is as the peer is very insisting ;)
    Approve = #xdata{type = submit,
		     fields = pubsub_subscribe_authorization:encode(
				[{node, Node},
				 {subscriber_jid, Peer},
				 {allow, true}])},
    send(Config, #message{to = PJID, sub_els = [Approve]}),
    wait_for_slave(Config),
    delete_node(Config, Node),
    disconnect(Config).

authorize_slave(Config) ->
    Node = ?config(pubsub_node, Config),
    MyJID = my_jid(Config),
    wait_for_master(Config),
    #ps_subscription{type = pending} = subscribe_node(Config, Node),
    %% We're denied at first
    #message{
       sub_els =
	   [#ps_event{
	       subscription = #ps_subscription{type = none,
					       jid = MyJID}}]} =
	recv_message(Config),
    wait_for_master(Config),
    #ps_subscription{type = pending} = subscribe_node(Config, Node),
    %% Now much better!
    #message{
       sub_els =
	   [#ps_event{
	       subscription = #ps_subscription{type = subscribed,
					       jid = MyJID}}]} =
	recv_message(Config),
    wait_for_master(Config),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("pubsub_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("pubsub_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("pubsub_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("pubsub_" ++ atom_to_list(T) ++ "_slave")]}.

set_opts(Config, Options) ->
    lists:foldl(
      fun({Opt, Val}, Acc) ->
	      lists:keystore(Opt, 1, Acc, {Opt, Val})
      end, Config, Options).

create_node(Config, Node) ->
    create_node(Config, Node, undefined).

create_node(Config, Node, Options) ->
    PJID = pubsub_jid(Config),
    NodeConfig = if is_list(Options) ->
			 #xdata{type = submit,
				fields = pubsub_node_config:encode(Options)};
		    true ->
			 undefined
		 end,
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{create = Node,
					  configure = {<<>>, NodeConfig}}]}) of
	#iq{type = result, sub_els = [#pubsub{create = NewNode}]} ->
	    NewNode;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

delete_node(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{delete = {Node, <<>>}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

purge_node(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{purge = Node}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_default_node_config(Config) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub_owner{default = {<<>>, undefined}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub_owner{default = {<<>>, NodeConfig}}]} ->
	    pubsub_node_config:decode(NodeConfig#xdata.fields);
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_node_config(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub_owner{configure = {Node, undefined}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub_owner{configure = {Node, NodeConfig}}]} ->
	    pubsub_node_config:decode(NodeConfig#xdata.fields);
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

set_node_config(Config, Node, Options) ->
    PJID = pubsub_jid(Config),
    NodeConfig = #xdata{type = submit,
			fields = pubsub_node_config:encode(Options)},
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{configure =
						    {Node, NodeConfig}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

publish_item(Config, Node) ->
    PJID = pubsub_jid(Config),
    ItemID = randoms:get_string(),
    Item = #ps_item{id = ItemID, xml_els = [xmpp:encode(#presence{id = ItemID})]},
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{publish = #ps_publish{
						       node = Node,
						       items = [Item]}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub{publish = #ps_publish{
					    node = Node,
					    items = [#ps_item{id = ItemID}]}}]} ->
	    Item;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_items(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub{items = #ps_items{node = Node}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub{items = #ps_items{node = Node, items = Items}}]} ->
	    Items;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

delete_item(Config, Node, I) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{retract =
					      #ps_retract{
						 node = Node,
						 items = [#ps_item{id = I}]}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

subscribe_node(Config, Node) ->
    PJID = pubsub_jid(Config),
    MyJID = my_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{subscribe = #ps_subscribe{
							 node = Node,
							 jid = MyJID}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub{
			  subscription = #ps_subscription{
					    node = Node,
					    jid = MyJID} = Sub}]} ->
	    Sub;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

unsubscribe_node(Config, Node) ->
    PJID = pubsub_jid(Config),
    MyJID = my_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{
				     unsubscribe = #ps_unsubscribe{
						      node = Node,
						      jid = MyJID}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_affiliations(Config) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub{affiliations = {<<>>, []}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub{affiliations = {<<>>, Affs}}]} ->
	    Affs;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_affiliations(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub_owner{affiliations = {Node, []}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub_owner{affiliations = {Node, Affs}}]} ->
	    Affs;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

set_affiliations(Config, Node, JTs) ->
    PJID = pubsub_jid(Config),
    Affs = [#ps_affiliation{jid = J, type = T} || {J, T} <- JTs],
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{affiliations =
						    {Node, Affs}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_subscriptions(Config) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub{subscriptions = {<<>>, []}}]}) of
	#iq{type = result, sub_els = [#pubsub{subscriptions = {<<>>, Subs}}]} ->
	    Subs;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_subscriptions(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub_owner{subscriptions = {Node, []}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub_owner{subscriptions = {Node, Subs}}]} ->
	    Subs;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

set_subscriptions(Config, Node, JTs) ->
    PJID = pubsub_jid(Config),
    Subs = [#ps_subscription{jid = J, type = T} || {J, T} <- JTs],
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{subscriptions =
						    {Node, Subs}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

default_node_config(Config) ->
    [{title, ?config(pubsub_node_title, Config)},
     {notify_delete, false},
     {send_last_published_item, never}].
