%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 18 Oct 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(privacy_tests).

%% API
-compile(export_all).
-import(suite, [disconnect/1, send_recv/2, get_event/1, put_event/2,
		recv_iq/1, recv_presence/1, recv_message/1, recv/1,
		send/2, my_jid/1, server_jid/1, get_features/1,
		set_roster/3, del_roster/1, get_roster/1]).
-include("suite.hrl").
-include("mod_roster.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single cases
%%%===================================================================
single_cases() ->
    {privacy_single, [sequence],
     [single_test(feature_enabled),
      single_test(set_get_list),
      single_test(get_list_non_existent),
      single_test(set_default),
      single_test(del_default),
      single_test(set_default_non_existent),
      single_test(set_active),
      single_test(del_active),
      single_test(set_active_non_existent),
      single_test(remove_list),
      single_test(remove_default_list),
      single_test(remove_active_list),
      %% TODO: this should be fixed
      %% single_test(remove_list_non_existent),
      single_test(allow_local_server),
      single_test(malformed_iq_query),
      single_test(malformed_get),
      single_test(malformed_set),
      single_test(malformed_type_value),
      single_test(set_get_block)]}.

feature_enabled(Config) ->
    Features = get_features(Config),
    true = lists:member(?NS_PRIVACY, Features),
    true = lists:member(?NS_BLOCKING, Features),
    disconnect(Config).

set_get_list(Config) ->
    ListName = <<"set-get-list">>,
    Items = [#privacy_item{order = 0, action = deny,
			   type = jid, value = <<"user@jabber.org">>,
			   iq = true},
	     #privacy_item{order = 1, action = allow,
			   type = group, value = <<"group">>,
			   message = true},
	     #privacy_item{order = 2, action = allow,
			   type = subscription, value = <<"both">>,
			   presence_in = true},
	     #privacy_item{order = 3, action = deny,
			   type = subscription, value = <<"from">>,
			   presence_out = true},
	     #privacy_item{order = 4, action = deny,
			   type = subscription, value = <<"to">>,
			   iq = true, message = true},
	     #privacy_item{order = 5, action = deny,
			   type = subscription, value = <<"none">>,
			   _ = true},
	     #privacy_item{order = 6, action = deny}],
    ok = set_items(Config, ListName, Items),
    #privacy_list{name = ListName, items = Items1} = get_list(Config, ListName),
    Items = lists:keysort(#privacy_item.order, Items1),
    del_privacy(disconnect(Config)).

get_list_non_existent(Config) ->
    ListName = <<"get-list-non-existent">>,
    #stanza_error{reason = 'item-not-found'} = get_list(Config, ListName),
    disconnect(Config).

set_default(Config) ->
    ListName = <<"set-default">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = set_default(Config, ListName),
    #privacy_query{default = ListName} = get_lists(Config),
    del_privacy(disconnect(Config)).

del_default(Config) ->
    ListName = <<"del-default">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = set_default(Config, ListName),
    #privacy_query{default = ListName} = get_lists(Config),
    ok = set_default(Config, none),
    #privacy_query{default = none} = get_lists(Config),
    del_privacy(disconnect(Config)).

set_default_non_existent(Config) ->
    ListName = <<"set-default-non-existent">>,
    #stanza_error{reason = 'item-not-found'} = set_default(Config, ListName),
    disconnect(Config).

set_active(Config) ->
    ListName = <<"set-active">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = set_active(Config, ListName),
    #privacy_query{active = ListName} = get_lists(Config),
    del_privacy(disconnect(Config)).

del_active(Config) ->
    ListName = <<"del-active">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = set_active(Config, ListName),
    #privacy_query{active = ListName} = get_lists(Config),
    ok = set_active(Config, none),
    #privacy_query{active = none} = get_lists(Config),
    del_privacy(disconnect(Config)).

set_active_non_existent(Config) ->
    ListName = <<"set-active-non-existent">>,
    #stanza_error{reason = 'item-not-found'} = set_active(Config, ListName),
    disconnect(Config).

remove_list(Config) ->
    ListName = <<"remove-list">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = del_list(Config, ListName),
    #privacy_query{lists = []} = get_lists(Config),
    del_privacy(disconnect(Config)).

remove_active_list(Config) ->
    ListName = <<"remove-active-list">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = set_active(Config, ListName),
    #stanza_error{reason = 'conflict'} = del_list(Config, ListName),
    del_privacy(disconnect(Config)).

remove_default_list(Config) ->
    ListName = <<"remove-default-list">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = set_default(Config, ListName),
    #stanza_error{reason = 'conflict'} = del_list(Config, ListName),
    del_privacy(disconnect(Config)).

remove_list_non_existent(Config) ->
    ListName = <<"remove-list-non-existent">>,
    #stanza_error{reason = 'item-not-found'} = del_list(Config, ListName),
    disconnect(Config).

allow_local_server(Config) ->
    ListName = <<"allow-local-server">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = set_active(Config, ListName),
    %% Whatever privacy rules are set, we should always communicate
    %% with our home server
    server_send_iqs(Config),
    server_recv_iqs(Config),
    send_stanzas_to_server_resource(Config),
    del_privacy(disconnect(Config)).

malformed_iq_query(Config) ->
    lists:foreach(
      fun(Type) ->
	      #iq{type = error} =
		  send_recv(Config,
			    #iq{type = Type,
				sub_els = [#privacy_list{name = <<"foo">>}]})
      end, [get, set]),
    disconnect(Config).

malformed_get(Config) ->
    JID = jid:make(randoms:get_string()),
    lists:foreach(
      fun(SubEl) ->
	      #iq{type = error} =
		  send_recv(Config, #iq{type = get, sub_els = [SubEl]})
      end, [#privacy_query{active = none},
	    #privacy_query{default = none},
	    #privacy_query{lists = [#privacy_list{name = <<"1">>},
				    #privacy_list{name = <<"2">>}]},
	    #block{items = [JID]}, #unblock{items = [JID]},
	    #block{}, #unblock{}]),
    disconnect(Config).

malformed_set(Config) ->
    lists:foreach(
      fun(SubEl) ->
	      #iq{type = error} =
		  send_recv(Config, #iq{type = set, sub_els = [SubEl]})
      end, [#privacy_query{active = none, default = none},
	    #privacy_query{lists = [#privacy_list{name = <<"1">>},
				    #privacy_list{name = <<"2">>}]},
	    #block{},
	    #block_list{},
	    #block_list{items = [jid:make(randoms:get_string())]}]).

malformed_type_value(Config) ->
    Item = #privacy_item{order = 0, action = deny},
    #stanza_error{reason = 'bad-request'} =
	set_items(Config, <<"malformed-jid">>,
		  [Item#privacy_item{type = jid, value = <<"@bad">>}]),
    #stanza_error{reason = 'bad-request'} =
	set_items(Config, <<"malformed-group">>,
		  [Item#privacy_item{type = group, value = <<"">>}]),
    #stanza_error{reason = 'bad-request'} =
	set_items(Config, <<"malformed-subscription">>,
		  [Item#privacy_item{type = subscription, value = <<"bad">>}]),
    disconnect(Config).

set_get_block(Config) ->
    J1 = jid:make(randoms:get_string(), randoms:get_string()),
    J2 = jid:make(randoms:get_string(), randoms:get_string()),
    {ok, ListName} = set_block(Config, [J1, J2]),
    JIDs = get_block(Config),
    JIDs = lists:sort([J1, J2]),
    {ok, ListName} = set_unblock(Config, [J2, J1]),
    [] = get_block(Config),
    del_privacy(disconnect(Config)).

%%%===================================================================
%%% Master-slave cases
%%%===================================================================
master_slave_cases() ->
    {privacy_master_slave, [sequence],
     [master_slave_test(deny_bare_jid),
      master_slave_test(deny_full_jid),
      master_slave_test(deny_server_jid),
      master_slave_test(deny_group),
      master_slave_test(deny_sub_both),
      master_slave_test(deny_sub_from),
      master_slave_test(deny_sub_to),
      master_slave_test(deny_sub_none),
      master_slave_test(deny_all),
      master_slave_test(deny_offline),
      master_slave_test(block),
      master_slave_test(unblock),
      master_slave_test(unblock_all)]}.

deny_bare_jid_master(Config) ->
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    deny_master(Config, {jid, jid:encode(PeerBareJID)}).

deny_bare_jid_slave(Config) ->
    deny_slave(Config).

deny_full_jid_master(Config) ->
    PeerJID = ?config(peer, Config),
    deny_master(Config, {jid, jid:encode(PeerJID)}).

deny_full_jid_slave(Config) ->
    deny_slave(Config).

deny_server_jid_master(Config) ->
    {_, Server, _} = jid:tolower(?config(peer, Config)),
    deny_master(Config, {jid, Server}).

deny_server_jid_slave(Config) ->
    deny_slave(Config).

deny_group_master(Config) ->
    Group = randoms:get_string(),
    deny_master(Config, {group, Group}).

deny_group_slave(Config) ->
    deny_slave(Config).

deny_sub_both_master(Config) ->
    deny_master(Config, {subscription, <<"both">>}).

deny_sub_both_slave(Config) ->
    deny_slave(Config).

deny_sub_from_master(Config) ->
    deny_master(Config, {subscription, <<"from">>}).

deny_sub_from_slave(Config) ->
    deny_slave(Config).

deny_sub_to_master(Config) ->
    deny_master(Config, {subscription, <<"to">>}).

deny_sub_to_slave(Config) ->
    deny_slave(Config).

deny_sub_none_master(Config) ->
    deny_master(Config, {subscription, <<"none">>}).

deny_sub_none_slave(Config) ->
    deny_slave(Config).

deny_all_master(Config) ->
    deny_master(Config, {undefined, <<"">>}).

deny_all_slave(Config) ->
    deny_slave(Config).

deny_master(Config, {Type, Value}) ->
    Sub = if Type == subscription ->
		  erlang:binary_to_atom(Value, utf8);
	     true ->
		  both
	  end,
    Groups = if Type == group -> [Value];
		true -> []
	     end,
    set_roster(Config, Sub, Groups),
    lists:foreach(
      fun(Opts) ->
	      ct:pal("Set list for ~s, ~s, ~w", [Type, Value, Opts]),
	      ListName = randoms:get_string(),
	      Item = #privacy_item{order = 0,
				   action = deny,
				   iq = proplists:get_bool(iq, Opts),
				   message = proplists:get_bool(message, Opts),
				   presence_in = proplists:get_bool(presence_in, Opts),
				   presence_out = proplists:get_bool(presence_out, Opts),
				   type = Type,
				   value = Value},
	      ok = set_items(Config, ListName, [Item]),
	      ok = set_active(Config, ListName),
	      put_event(Config, Opts),
	      case is_presence_in_blocked(Opts) of
		  true -> ok;
		  false -> recv_presences(Config)
	      end,
	      case is_iq_in_blocked(Opts) of
		  true -> ok;
		  false -> recv_iqs(Config)
	      end,
	      case is_message_in_blocked(Opts) of
		  true -> ok;
		  false -> recv_messages(Config)
	      end,
	      ct:comment("Waiting for 'send' command from the slave"),
	      send = get_event(Config),
	      case is_presence_out_blocked(Opts) of
		  true -> check_presence_blocked(Config, 'not-acceptable');
		  false -> ok
	      end,
	      case is_iq_out_blocked(Opts) of
		  true -> check_iq_blocked(Config, 'not-acceptable');
		  false -> send_iqs(Config)
	      end,
	      case is_message_out_blocked(Opts) of
		  true -> check_message_blocked(Config, 'not-acceptable');
		  false -> send_messages(Config)
	      end,
	      case is_other_blocked(Opts) of
		  true -> check_other_blocked(Config, 'not-acceptable');
		  false -> ok
	      end,
	      ct:comment("Waiting for slave to finish processing our stanzas"),
	      done = get_event(Config)
      end,
      [[iq], [message], [presence_in], [presence_out],
       [iq, message, presence_in, presence_out], []]),
    put_event(Config, disconnect),
    clean_up(disconnect(Config)).

deny_slave(Config) ->
    set_roster(Config, both, []),
    deny_slave(Config, get_event(Config)).

deny_slave(Config, disconnect) ->
    clean_up(disconnect(Config));
deny_slave(Config, Opts) ->
    send_presences(Config),
    case is_iq_in_blocked(Opts) of
	true -> check_iq_blocked(Config, 'service-unavailable');
	false -> send_iqs(Config)
    end,
    case is_message_in_blocked(Opts) of
	true -> check_message_blocked(Config, 'service-unavailable');
	false -> send_messages(Config)
    end,
    put_event(Config, send),
    case is_iq_out_blocked(Opts) of
	true -> ok;
	false -> recv_iqs(Config)
    end,
    case is_message_out_blocked(Opts) of
	true -> ok;
	false -> recv_messages(Config)
    end,
    put_event(Config, done),
    deny_slave(Config, get_event(Config)).

deny_offline_master(Config) ->
    set_roster(Config, both, []),
    ListName = <<"deny-offline">>,
    Item = #privacy_item{order = 0, action = deny},
    ok = set_items(Config, ListName, [Item]),
    ok = set_default(Config, ListName),
    NewConfig = disconnect(Config),
    put_event(NewConfig, send),
    ct:comment("Waiting for the slave to finish"),
    done = get_event(NewConfig),
    clean_up(NewConfig).

deny_offline_slave(Config) ->
    set_roster(Config, both, []),
    ct:comment("Waiting for 'send' command from the master"),
    send = get_event(Config),
    send_presences(Config),
    check_iq_blocked(Config, 'service-unavailable'),
    check_message_blocked(Config, 'service-unavailable'),
    put_event(Config, done),
    clean_up(disconnect(Config)).

block_master(Config) ->
    PeerJID = ?config(peer, Config),
    set_roster(Config, both, []),
    {ok, _} = set_block(Config, [PeerJID]),
    check_presence_blocked(Config, 'not-acceptable'),
    check_iq_blocked(Config, 'not-acceptable'),
    check_message_blocked(Config, 'not-acceptable'),
    check_other_blocked(Config, 'not-acceptable'),
    %% We should always be able to communicate with our home server
    server_send_iqs(Config),
    server_recv_iqs(Config),
    send_stanzas_to_server_resource(Config),
    put_event(Config, send),
    done = get_event(Config),
    clean_up(disconnect(Config)).

block_slave(Config) ->
    set_roster(Config, both, []),
    ct:comment("Waiting for 'send' command from master"),
    send = get_event(Config),
    send_presences(Config),
    check_iq_blocked(Config, 'service-unavailable'),
    check_message_blocked(Config, 'service-unavailable'),
    put_event(Config, done),
    clean_up(disconnect(Config)).

unblock_master(Config) ->
    PeerJID = ?config(peer, Config),
    set_roster(Config, both, []),
    {ok, ListName} = set_block(Config, [PeerJID]),
    {ok, ListName} = set_unblock(Config, [PeerJID]),
    put_event(Config, send),
    recv_presences(Config),
    recv_iqs(Config),
    recv_messages(Config),
    clean_up(disconnect(Config)).

unblock_slave(Config) ->
    set_roster(Config, both, []),
    ct:comment("Waiting for 'send' command from master"),
    send = get_event(Config),
    send_presences(Config),
    send_iqs(Config),
    send_messages(Config),
    clean_up(disconnect(Config)).

unblock_all_master(Config) ->
    PeerJID = ?config(peer, Config),
    set_roster(Config, both, []),
    {ok, ListName} = set_block(Config, [PeerJID]),
    {ok, ListName} = set_unblock(Config, []),
    put_event(Config, send),
    recv_presences(Config),
    recv_iqs(Config),
    recv_messages(Config),
    clean_up(disconnect(Config)).

unblock_all_slave(Config) ->
    set_roster(Config, both, []),
    ct:comment("Waiting for 'send' command from master"),
    send = get_event(Config),
    send_presences(Config),
    send_iqs(Config),
    send_messages(Config),
    clean_up(disconnect(Config)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("privacy_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("privacy_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("privacy_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("privacy_" ++ atom_to_list(T) ++ "_slave")]}.

set_items(Config, Name, Items) ->
    ct:comment("Setting privacy list ~s with items = ~p", [Name, Items]),
    case send_recv(
	   Config,
	   #iq{type = set, sub_els = [#privacy_query{
					 lists = [#privacy_list{
						     name = Name,
						     items = Items}]}]}) of
	#iq{type = result, sub_els = []} ->
	    ct:comment("Receiving privacy list push"),
	    #iq{type = set, id = ID,
		sub_els = [#privacy_query{lists = [#privacy_list{
						      name = Name}]}]} =
		recv_iq(Config),
	    send(Config, #iq{type = result, id = ID}),
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

get_list(Config, Name) ->
    ct:comment("Requesting privacy list ~s", [Name]),
    case send_recv(Config,
		   #iq{type = get,
		       sub_els = [#privacy_query{
				     lists = [#privacy_list{name = Name}]}]}) of
	#iq{type = result, sub_els = [#privacy_query{lists = [List]}]} ->
	    List;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

get_lists(Config) ->
    ct:comment("Requesting privacy lists"),
    case send_recv(Config, #iq{type = get, sub_els = [#privacy_query{}]}) of
	#iq{type = result, sub_els = [SubEl]} ->
	    SubEl;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

del_list(Config, Name) ->
    case send_recv(
	   Config,
	   #iq{type = set, sub_els = [#privacy_query{
					 lists = [#privacy_list{
						     name = Name}]}]}) of
	#iq{type = result, sub_els = []} ->
	    ct:comment("Receiving privacy list push"),
	    #iq{type = set, id = ID,
		sub_els = [#privacy_query{lists = [#privacy_list{
						      name = Name}]}]} =
		recv_iq(Config),
	    send(Config, #iq{type = result, id = ID}),
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

set_active(Config, Name) ->
    ct:comment("Setting active privacy list ~s", [Name]),
    case send_recv(
	   Config,
	   #iq{type = set, sub_els = [#privacy_query{active = Name}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

set_default(Config, Name) ->
    ct:comment("Setting default privacy list ~s", [Name]),
    case send_recv(
	   Config,
	   #iq{type = set, sub_els = [#privacy_query{default = Name}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

get_block(Config) ->
    case send_recv(Config, #iq{type = get, sub_els = [#block_list{}]}) of
	#iq{type = result, sub_els = [#block_list{items = JIDs}]} ->
	    lists:sort(JIDs);
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

set_block(Config, JIDs) ->
    case send_recv(Config, #iq{type = set,
			       sub_els = [#block{items = JIDs}]}) of
	#iq{type = result, sub_els = []} ->
	    {#iq{id = I1, sub_els = [#block{items = Items}]},
	     #iq{id = I2, sub_els = [#privacy_query{lists = Lists}]}} =
		?recv2(#iq{type = set, sub_els = [#block{}]},
		       #iq{type = set, sub_els = [#privacy_query{}]}),
	    send(Config, #iq{type = result, id = I1}),
	    send(Config, #iq{type = result, id = I2}),
	    ct:comment("Checking if all JIDs present in the push"),
	    true = lists:sort(JIDs) == lists:sort(Items),
	    ct:comment("Getting name of the corresponding privacy list"),
	    [#privacy_list{name = Name}] = Lists,
	    {ok, Name};
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

set_unblock(Config, JIDs) ->
    ct:comment("Unblocking ~p", [JIDs]),
    case send_recv(Config, #iq{type = set,
			       sub_els = [#unblock{items = JIDs}]}) of
	#iq{type = result, sub_els = []} ->
	    {#iq{id = I1, sub_els = [#unblock{items = Items}]},
	     #iq{id = I2, sub_els = [#privacy_query{lists = Lists}]}} =
		?recv2(#iq{type = set, sub_els = [#unblock{}]},
		       #iq{type = set, sub_els = [#privacy_query{}]}),
	    send(Config, #iq{type = result, id = I1}),
	    send(Config, #iq{type = result, id = I2}),
	    ct:comment("Checking if all JIDs present in the push"),
	    true = lists:sort(JIDs) == lists:sort(Items),
	    ct:comment("Getting name of the corresponding privacy list"),
	    [#privacy_list{name = Name}] = Lists,
	    {ok, Name};
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

del_privacy(Config) ->
    {U, S, _} = jid:tolower(my_jid(Config)),
    ct:comment("Removing all privacy data"),
    mod_privacy:remove_user(U, S),
    Config.

clean_up(Config) ->
    del_privacy(del_roster(Config)).

check_iq_blocked(Config, Reason) ->
    PeerJID = ?config(peer, Config),
    ct:comment("Checking if all IQs are blocked"),
    lists:foreach(
      fun(Type) ->
	      send(Config, #iq{type = Type, to = PeerJID})
      end, [error, result]),
    lists:foreach(
      fun(Type) ->
	      #iq{type = error} = Err =
		  send_recv(Config, #iq{type = Type, to = PeerJID,
					sub_els = [#ping{}]}),
	      #stanza_error{reason = Reason} = xmpp:get_error(Err)
      end, [set, get]).

check_message_blocked(Config, Reason) ->
    PeerJID = ?config(peer, Config),
    ct:comment("Checking if all messages are blocked"),
    %% TODO: do something with headline and groupchat.
    %% The hack from 64d96778b452aad72349b21d2ac94e744617b07a
    %% screws this up.
    lists:foreach(
      fun(Type) ->
	      send(Config, #message{type = Type, to = PeerJID})
      end, [error]),
    lists:foreach(
      fun(Type) ->
	      #message{type = error} = Err =
		  send_recv(Config, #message{type = Type, to = PeerJID}),
	      #stanza_error{reason = Reason} = xmpp:get_error(Err)
      end, [chat, normal]).

check_presence_blocked(Config, Reason) ->
    PeerJID = ?config(peer, Config),
    ct:comment("Checking if all presences are blocked"),
    lists:foreach(
      fun(Type) ->
	      #presence{type = error} = Err =
		  send_recv(Config, #presence{type = Type, to = PeerJID}),
	      #stanza_error{reason = Reason} = xmpp:get_error(Err)
      end, [available, unavailable]).

check_other_blocked(Config, Reason) ->
    PeerJID = ?config(peer, Config),
    ct:comment("Checking if subscriptions and presence-errors are blocked"),
    send(Config, #presence{type = error, to = PeerJID}),
    lists:foreach(
      fun(Type) ->
	      #presence{type = error} = Err =
		  send_recv(Config, #presence{type = Type, to = PeerJID}),
	      #stanza_error{reason = Reason} = xmpp:get_error(Err)
      end, [subscribe, subscribed, unsubscribe, unsubscribed]).

send_presences(Config) ->
    PeerJID = ?config(peer, Config),
    ct:comment("Sending all types of presences to the peer"),
    lists:foreach(
      fun(Type) ->
	      send(Config, #presence{type = Type, to = PeerJID})
      end, [available, unavailable]).

send_iqs(Config) ->
    PeerJID = ?config(peer, Config),
    ct:comment("Sending all types of IQs to the peer"),
    lists:foreach(
      fun(Type) ->
	      send(Config, #iq{type = Type, to = PeerJID})
      end, [set, get, error, result]).

send_messages(Config) ->
    PeerJID = ?config(peer, Config),
    ct:comment("Sending all types of messages to the peer"),
    lists:foreach(
      fun(Type) ->
	      send(Config, #message{type = Type, to = PeerJID})
      end, [chat, error, groupchat, headline, normal]).

recv_presences(Config) ->
    PeerJID = ?config(peer, Config),
    lists:foreach(
      fun(Type) ->
	      #presence{type = Type, from = PeerJID} =
		  recv_presence(Config)
      end, [available, unavailable]).

recv_iqs(Config) ->
    PeerJID = ?config(peer, Config),
    lists:foreach(
      fun(Type) ->
	      #iq{type = Type, from = PeerJID} = recv_iq(Config)
      end, [set, get, error, result]).

recv_messages(Config) ->
    PeerJID = ?config(peer, Config),
    lists:foreach(
      fun(Type) ->
	      #message{type = Type, from = PeerJID} = recv_message(Config)
      end, [chat, error, groupchat, headline, normal]).

match_all(Opts) ->
    IQ = proplists:get_bool(iq, Opts),
    Message = proplists:get_bool(message, Opts),
    PresenceIn = proplists:get_bool(presence_in, Opts),
    PresenceOut = proplists:get_bool(presence_out, Opts),
    not (IQ or Message or PresenceIn or PresenceOut).

is_message_in_blocked(Opts) ->
    proplists:get_bool(message, Opts) or match_all(Opts).

is_message_out_blocked(Opts) ->
    match_all(Opts).

is_iq_in_blocked(Opts) ->    
    proplists:get_bool(iq, Opts) or match_all(Opts).

is_iq_out_blocked(Opts) ->
    match_all(Opts).

is_presence_in_blocked(Opts) ->
    proplists:get_bool(presence_in, Opts) or match_all(Opts).

is_presence_out_blocked(Opts) ->
    proplists:get_bool(presence_out, Opts) or match_all(Opts).

is_other_blocked(Opts) ->
    %% 'other' means subscriptions and presence-errors
    match_all(Opts).

server_send_iqs(Config) ->
    ServerJID = server_jid(Config),
    MyJID = my_jid(Config),
    ct:comment("Sending IQs from ~s to ~s",
	       [jid:encode(ServerJID), jid:encode(MyJID)]),
    lists:foreach(
      fun(Type) ->
	      ejabberd_router:route(
		#iq{from = ServerJID, to = MyJID, type = Type})
      end, [error, result]),
    lists:foreach(
      fun(Type) ->
	      ejabberd_local:route_iq(
		#iq{from = ServerJID, to = MyJID, type = Type},
		fun(#iq{type = result, sub_els = []}) -> ok;
		   (IQ) -> ct:fail({unexpected_iq_result, IQ})
		end)
      end, [set, get]).

server_recv_iqs(Config) ->
    ServerJID = server_jid(Config),
    ct:comment("Receiving IQs from ~s", [jid:encode(ServerJID)]),
    lists:foreach(
      fun(Type) ->
	      #iq{type = Type, from = ServerJID} = recv_iq(Config)
      end, [error, result]),
    lists:foreach(
      fun(Type) ->
	      #iq{type = Type, from = ServerJID, id = I} = recv_iq(Config),
	      send(Config, #iq{to = ServerJID, type = result, id = I})
      end, [set, get]).

send_stanzas_to_server_resource(Config) ->
    ServerJID = server_jid(Config),
    ServerJIDResource = jid:replace_resource(ServerJID, <<"resource">>),
    %% All stanzas sent should be handled by local_send_to_resource_hook
    %% and should be bounced with item-not-found error
    ct:comment("Sending IQs to ~s", [jid:encode(ServerJIDResource)]),
    lists:foreach(
      fun(Type) ->
	      #iq{type = error} = Err =
		  send_recv(Config, #iq{type = Type, to = ServerJIDResource}),
		  #stanza_error{reason = 'item-not-found'} = xmpp:get_error(Err)
      end, [set, get]),
    ct:comment("Sending messages to ~s", [jid:encode(ServerJIDResource)]),
    lists:foreach(
      fun(Type) ->
	      #message{type = error} = Err =
		  send_recv(Config, #message{type = Type, to = ServerJIDResource}),
	      #stanza_error{reason = 'item-not-found'} = xmpp:get_error(Err)
      end, [normal, chat, groupchat, headline]),
    ct:comment("Sending presences to ~s", [jid:encode(ServerJIDResource)]),
    lists:foreach(
      fun(Type) ->
	      #presence{type = error} = Err =
		  send_recv(Config, #presence{type = Type, to = ServerJIDResource}),
	      #stanza_error{reason = 'item-not-found'} = xmpp:get_error(Err)
      end, [available, unavailable]).
