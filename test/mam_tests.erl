%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-module(mam_tests).

%% API
-compile(export_all).
-import(suite, [get_features/1, disconnect/1, my_jid/1, send_recv/2,
		wait_for_slave/1, server_jid/1, send/2, get_features/2,
		wait_for_master/1, recv_message/1, recv_iq/1, muc_room_jid/1,
		muc_jid/1, is_feature_advertised/3, get_event/1, put_event/2]).

-include("suite.hrl").
-define(VERSIONS, [?NS_MAM_TMP, ?NS_MAM_0, ?NS_MAM_1, ?NS_MAM_2]).

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {mam_single, [sequence],
     [single_test(feature_enabled),
      single_test(get_set_prefs),
      single_test(get_form),
      single_test(fake_by)]}.

feature_enabled(Config) ->
    BareMyJID = jid:remove_resource(my_jid(Config)),
    RequiredFeatures = sets:from_list(?VERSIONS),
    ServerFeatures = sets:from_list(get_features(Config)),
    UserFeatures = sets:from_list(get_features(Config, BareMyJID)),
    MUCFeatures = get_features(Config, muc_jid(Config)),
    ct:comment("Checking if all MAM server features are enabled"),
    true = sets:is_subset(RequiredFeatures, ServerFeatures),
    ct:comment("Checking if all MAM user features are enabled"),
    true = sets:is_subset(RequiredFeatures, UserFeatures),
    ct:comment("Checking if all MAM conference service features are enabled"),
    true = lists:member(?NS_MAM_1, MUCFeatures),
    true = lists:member(?NS_MAM_2, MUCFeatures),
    clean(disconnect(Config)).

fake_by(Config) ->
    BareServerJID = server_jid(Config),
    FullServerJID = jid:replace_resource(BareServerJID, p1_rand:get_string()),
    FullMyJID = my_jid(Config),
    BareMyJID = jid:remove_resource(FullMyJID),
    Fakes = lists:flatmap(
	      fun(JID) ->
		      [#mam_archived{id = p1_rand:get_string(), by = JID},
		       #stanza_id{id = p1_rand:get_string(), by = JID}]
	      end, [BareServerJID, FullServerJID, BareMyJID, FullMyJID]),
    Body = xmpp:mk_text(<<"body">>),
    ForeignJID = jid:make(p1_rand:get_string()),
    Archived = #mam_archived{id = p1_rand:get_string(), by = ForeignJID},
    StanzaID = #stanza_id{id = p1_rand:get_string(), by = ForeignJID},
    #message{body = Body, sub_els = SubEls} =
	send_recv(Config, #message{to = FullMyJID,
				   body = Body,
				   sub_els = [Archived, StanzaID|Fakes]}),
    ct:comment("Checking if only foreign tags present"),
    [ForeignJID, ForeignJID] = lists:flatmap(
				 fun(#mam_archived{by = By}) -> [By];
				    (#stanza_id{by = By}) -> [By];
				    (_) -> []
				 end, SubEls),
    clean(disconnect(Config)).

get_set_prefs(Config) ->
    Range = [{JID, #mam_prefs{xmlns = NS,
			      default = Default,
			      always = Always,
			      never = Never}} ||
		JID <- [undefined, server_jid(Config)],
		NS <- ?VERSIONS,
		Default <- [always, never, roster],
		Always <- [[], [jid:decode(<<"foo@bar.baz">>)]],
		Never <- [[], [jid:decode(<<"baz@bar.foo">>)]]],
    lists:foreach(
      fun({To, Prefs}) ->
	      NS = Prefs#mam_prefs.xmlns,
	      #iq{type = result, sub_els = [Prefs]} =
		  send_recv(Config, #iq{type = set, to = To,
					sub_els = [Prefs]}),
	      #iq{type = result, sub_els = [Prefs]} =
		  send_recv(Config, #iq{type = get, to = To,
					sub_els = [#mam_prefs{xmlns = NS}]})
      end, Range),
    clean(disconnect(Config)).

get_form(Config) ->
    ServerJID = server_jid(Config),
    Range = [{JID, NS} || JID <- [undefined, ServerJID],
			  NS <- ?VERSIONS -- [?NS_MAM_TMP]],
    lists:foreach(
      fun({To, NS}) ->
	      #iq{type = result,
		  sub_els = [#mam_query{xmlns = NS,
					xdata = #xdata{} = X}]} =
		  send_recv(Config, #iq{type = get, to = To,
					sub_els = [#mam_query{xmlns = NS}]}),
	      [NS] = xmpp_util:get_xdata_values(<<"FORM_TYPE">>, X),
	      true = xmpp_util:has_xdata_var(<<"with">>, X),
	      true = xmpp_util:has_xdata_var(<<"start">>, X),
	      true = xmpp_util:has_xdata_var(<<"end">>, X)
      end, Range),
    clean(disconnect(Config)).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {mam_master_slave, [sequence],
     [master_slave_test(archived_and_stanza_id),
      master_slave_test(query_all),
      master_slave_test(query_with),
      master_slave_test(query_rsm_max),
      master_slave_test(query_rsm_after),
      master_slave_test(query_rsm_before),
      master_slave_test(muc),
      master_slave_test(mucsub),
      master_slave_test(mucsub_from_muc),
      master_slave_test(mucsub_from_muc_non_persistent)]}.

archived_and_stanza_id_master(Config) ->
    #presence{} = send_recv(Config, #presence{}),
    wait_for_slave(Config),
    send_messages(Config, lists:seq(1, 5)),
    clean(disconnect(Config)).

archived_and_stanza_id_slave(Config) ->
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    recv_messages(Config, lists:seq(1, 5)),
    clean(disconnect(Config)).

query_all_master(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_slave(Config),
    send_messages(Config, lists:seq(1, 5)),
    query_all(Config, MyJID, Peer),
    clean(disconnect(Config)).

query_all_slave(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    recv_messages(Config, lists:seq(1, 5)),
    query_all(Config, Peer, MyJID),
    clean(disconnect(Config)).

query_with_master(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_slave(Config),
    send_messages(Config, lists:seq(1, 5)),
    query_with(Config, MyJID, Peer),
    clean(disconnect(Config)).

query_with_slave(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    recv_messages(Config, lists:seq(1, 5)),
    query_with(Config, Peer, MyJID),
    clean(disconnect(Config)).

query_rsm_max_master(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_slave(Config),
    send_messages(Config, lists:seq(1, 5)),
    query_rsm_max(Config, MyJID, Peer),
    clean(disconnect(Config)).

query_rsm_max_slave(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    recv_messages(Config, lists:seq(1, 5)),
    query_rsm_max(Config, Peer, MyJID),
    clean(disconnect(Config)).

query_rsm_after_master(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_slave(Config),
    send_messages(Config, lists:seq(1, 5)),
    query_rsm_after(Config, MyJID, Peer),
    clean(disconnect(Config)).

query_rsm_after_slave(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    recv_messages(Config, lists:seq(1, 5)),
    query_rsm_after(Config, Peer, MyJID),
    clean(disconnect(Config)).

query_rsm_before_master(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_slave(Config),
    send_messages(Config, lists:seq(1, 5)),
    query_rsm_before(Config, MyJID, Peer),
    clean(disconnect(Config)).

query_rsm_before_slave(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = set_default(Config, always),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    recv_messages(Config, lists:seq(1, 5)),
    query_rsm_before(Config, Peer, MyJID),
    clean(disconnect(Config)).

muc_master(Config) ->
    Room = muc_room_jid(Config),
    %% Joining
    ok = muc_tests:join_new(Config),
    %% MAM feature should not be advertised at this point,
    %% because MAM is not enabled so far
    false = is_feature_advertised(Config, ?NS_MAM_1, Room),
    false = is_feature_advertised(Config, ?NS_MAM_2, Room),
    %% Fill in some history
    send_messages_to_room(Config, lists:seq(1, 21)),
    %% We now should be able to retrieve those via MAM, even though
    %% MAM is disabled. However, only last 20 messages should be received.
    recv_messages_from_room(Config, lists:seq(2, 21)),
    %% Now enable MAM for the conference
    %% Retrieve config first
    CfgOpts = muc_tests:get_config(Config),
    %% Find the MAM field in the config
    true = proplists:is_defined(mam, CfgOpts),
    %% Enable MAM
    [104] = muc_tests:set_config(Config, [{mam, true}]),
    %% Check if MAM has been enabled
    true = is_feature_advertised(Config, ?NS_MAM_1, Room),
    true = is_feature_advertised(Config, ?NS_MAM_2, Room),
    %% We now sending some messages again
    send_messages_to_room(Config, lists:seq(1, 5)),
    %% And retrieve them via MAM again.
    recv_messages_from_room(Config, lists:seq(1, 5)),
    put_event(Config, disconnect),
    muc_tests:leave(Config),
    clean(disconnect(Config)).

muc_slave(Config) ->
    disconnect = get_event(Config),
    clean(disconnect(Config)).

mucsub_master(Config) ->
    Room = muc_room_jid(Config),
    Peer = ?config(peer, Config),
    wait_for_slave(Config),
    ct:comment("Joining muc room"),
    ok = muc_tests:join_new(Config),

    ct:comment("Enabling mam in room"),
    CfgOpts = muc_tests:get_config(Config),
    %% Find the MAM field in the config
    ?match(true, proplists:is_defined(mam, CfgOpts)),
    ?match(true, proplists:is_defined(allow_subscription, CfgOpts)),
    %% Enable MAM
    [104] = muc_tests:set_config(Config, [{mam, true}, {allow_subscription, true}]),

    ct:comment("Subscribing peer to room"),
    ?send_recv(#iq{to = Room, type = set, sub_els = [
	#muc_subscribe{jid = Peer, nick = <<"peer">>,
		       events = [?NS_MUCSUB_NODES_MESSAGES]}
    ]}, #iq{type = result}),

    ct:comment("Sending messages to room"),
    send_messages_to_room(Config, lists:seq(1, 5)),

    ct:comment("Retrieving messages from room mam storage"),
    recv_messages_from_room(Config, lists:seq(1, 5)),

    ct:comment("Cleaning up"),
    put_event(Config, ready),
    ready = get_event(Config),
    muc_tests:leave(Config),
    clean(disconnect(Config)).

mucsub_slave(Config) ->
    Room = muc_room_jid(Config),
    MyJID = my_jid(Config),
    MyJIDBare = jid:remove_resource(MyJID),
    ok = set_default(Config, always),
    send_recv(Config, #presence{}),
    wait_for_master(Config),

    ct:comment("Receiving mucsub events"),
    lists:foreach(
	fun(N) ->
	    Body = xmpp:mk_text(integer_to_binary(N)),
	    Msg = ?match(#message{from = Room, type = normal} = Msg, recv_message(Config), Msg),
	    PS = ?match(#ps_event{items = #ps_items{node = ?NS_MUCSUB_NODES_MESSAGES, items = [
		#ps_item{} = PS
	    ]}}, xmpp:get_subtag(Msg, #ps_event{}), PS),
	    ?match(#message{type = groupchat, body = Body}, xmpp:get_subtag(PS, #message{}))
	end, lists:seq(1, 5)),

    ct:comment("Retrieving personal mam archive"),
    QID = p1_rand:get_string(),
    I = send(Config, #iq{type = set,
			 sub_els = [#mam_query{xmlns = ?NS_MAM_2, id = QID}]}),
    lists:foreach(
	fun(N) ->
	    Body = xmpp:mk_text(integer_to_binary(N)),
	    Forw = ?match(#message{
		to = MyJID, from = MyJIDBare,
		sub_els = [#mam_result{
		    xmlns = ?NS_MAM_2,
		    queryid = QID,
		    sub_els = [#forwarded{
			delay = #delay{}} = Forw]}]},
			  recv_message(Config), Forw),
	    IMsg = ?match(#message{
		to = MyJIDBare, from = Room} = IMsg, xmpp:get_subtag(Forw, #message{}), IMsg),

	    PS = ?match(#ps_event{items = #ps_items{node = ?NS_MUCSUB_NODES_MESSAGES, items = [
		#ps_item{} = PS
	    ]}}, xmpp:get_subtag(IMsg, #ps_event{}), PS),
	    ?match(#message{type = groupchat, body = Body}, xmpp:get_subtag(PS, #message{}))
	end, lists:seq(1, 5)),
    RSM = ?match(#iq{from = MyJIDBare, id = I, type = result,
		     sub_els = [#mam_fin{xmlns = ?NS_MAM_2,
					 rsm = RSM,
					 complete = true}]}, recv_iq(Config), RSM),
    match_rsm_count(RSM, 5),

    % Wait for master exit
    ready = get_event(Config),
    % Unsubscribe yourself
    ?send_recv(#iq{to = Room, type = set, sub_els = [
	#muc_unsubscribe{}
    ]}, #iq{type = result}),
    put_event(Config, ready),
    clean(disconnect(Config)).

mucsub_from_muc_master(Config) ->
    mucsub_master(Config).

mucsub_from_muc_slave(Config) ->
    Server = ?config(server, Config),
    gen_mod:update_module(Server, mod_mam, #{user_mucsub_from_muc_archive => true}),
    Config2 = mucsub_slave(Config),
    gen_mod:update_module(Server, mod_mam, #{user_mucsub_from_muc_archive => false}),
    Config2.

mucsub_from_muc_non_persistent_master(Config) ->
    Config1 = lists:keystore(persistent_room, 1, Config, {persistent_room, false}),
    Config2 = mucsub_from_muc_master(Config1),
    lists:keydelete(persistent_room, 1, Config2).

mucsub_from_muc_non_persistent_slave(Config) ->
    Config1 = lists:keystore(persistent_room, 1, Config, {persistent_room, false}),
    Config2 = mucsub_from_muc_slave(Config1),
    lists:keydelete(persistent_room, 1, Config2).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("mam_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("mam_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("mam_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("mam_" ++ atom_to_list(T) ++ "_slave")]}.

clean(Config) ->
    {U, S, _} = jid:tolower(my_jid(Config)),
    mod_mam:remove_user(U, S),
    Config.

set_default(Config, Default) ->
    lists:foreach(
      fun(NS) ->
	      ct:comment("Setting default preferences of '~s' to '~s'",
			 [NS, Default]),
	      #iq{type = result,
		  sub_els = [#mam_prefs{xmlns = NS, default = Default}]} =
		  send_recv(Config, #iq{type = set,
					sub_els = [#mam_prefs{xmlns = NS,
							      default = Default}]})
      end, ?VERSIONS).

send_messages(Config, Range) ->
    Peer = ?config(peer, Config),
    lists:foreach(
      fun(N) ->
	      Body = xmpp:mk_text(integer_to_binary(N)),
              send(Config, #message{to = Peer, body = Body})
      end, Range).

recv_messages(Config, Range) ->
    Peer = ?config(peer, Config),
    lists:foreach(
      fun(N) ->
	      Body = xmpp:mk_text(integer_to_binary(N)),
	      #message{from = Peer, body = Body} = Msg =
		  recv_message(Config),
	      #mam_archived{by = BareMyJID} =
		  xmpp:get_subtag(Msg, #mam_archived{}),
	      #stanza_id{by = BareMyJID} =
		  xmpp:get_subtag(Msg, #stanza_id{})
      end, Range).

recv_archived_messages(Config, From, To, QID, Range) ->
    MyJID = my_jid(Config),
    lists:foreach(
      fun(N) ->
	      ct:comment("Retrieving ~pth message in range ~p",
			 [N, Range]),
              Body = xmpp:mk_text(integer_to_binary(N)),
              #message{to = MyJID,
                       sub_els =
                           [#mam_result{
                               queryid = QID,
                               sub_els =
                                   [#forwarded{
                                       delay = #delay{},
                                       sub_els = [El]}]}]} = recv_message(Config),
	      #message{from = From, to = To,
		       body = Body} = xmpp:decode(El)
      end, Range).

maybe_recv_iq_result(Config, ?NS_MAM_0, I) ->
    #iq{type = result, id = I} = recv_iq(Config);
maybe_recv_iq_result(_, _, _) ->
    ok.

query_iq_type(?NS_MAM_TMP) -> get;
query_iq_type(_) -> set.

send_query(Config, #mam_query{xmlns = NS} = Query) ->
    Type = query_iq_type(NS),
    I = send(Config, #iq{type = Type, sub_els = [Query]}),
    maybe_recv_iq_result(Config, NS, I),
    I.

recv_fin(Config, I, QueryID, NS, IsComplete) when NS == ?NS_MAM_1; NS == ?NS_MAM_2 ->
    ct:comment("Receiving fin iq for namespace '~s'", [NS]),
    #iq{type = result, id = I,
	sub_els = [#mam_fin{xmlns = NS,
			    complete = Complete,
			    rsm = RSM}]} = recv_iq(Config),
    ct:comment("Checking if complete is ~s", [IsComplete]),
    ?match(IsComplete, Complete),
    RSM;
recv_fin(Config, I, QueryID, ?NS_MAM_TMP = NS, _IsComplete) ->
    ct:comment("Receiving fin iq for namespace '~s'", [NS]),
    #iq{type = result, id = I,
	sub_els = [#mam_query{xmlns = NS,
			      rsm = RSM,
			      id = QueryID}]} = recv_iq(Config),
    RSM;
recv_fin(Config, _, QueryID, ?NS_MAM_0 = NS, IsComplete) ->
    ct:comment("Receiving fin message for namespace '~s'", [NS]),
    #message{} = FinMsg = recv_message(Config),
    #mam_fin{xmlns = NS,
	     id = QueryID,
	     complete = Complete,
	     rsm = RSM} = xmpp:get_subtag(FinMsg, #mam_fin{xmlns = NS}),
    ct:comment("Checking if complete is ~s", [IsComplete]),
    ?match(IsComplete, Complete),
    RSM.

send_messages_to_room(Config, Range) ->
    MyNick = ?config(master_nick, Config),
    Room = muc_room_jid(Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    lists:foreach(
      fun(N) ->
              Body = xmpp:mk_text(integer_to_binary(N)),
	      #message{from = MyNickJID,
		       type = groupchat,
		       body = Body} =
		  send_recv(Config, #message{to = Room, body = Body,
					     type = groupchat})
      end, Range).

recv_messages_from_room(Config, Range) ->
    MyNick = ?config(master_nick, Config),
    Room = muc_room_jid(Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    MyJID = my_jid(Config),
    QID = p1_rand:get_string(),
    I = send(Config, #iq{type = set, to = Room,
			 sub_els = [#mam_query{xmlns = ?NS_MAM_2, id = QID}]}),
    lists:foreach(
      fun(N) ->
	      Body = xmpp:mk_text(integer_to_binary(N)),
	      #message{
		 to = MyJID, from = Room,
		 sub_els =
		     [#mam_result{
			 xmlns = ?NS_MAM_2,
			 queryid = QID,
			 sub_els =
			     [#forwarded{
				 delay = #delay{},
				 sub_els = [El]}]}]} = recv_message(Config),
	      #message{from = MyNickJID,
		       type = groupchat,
		       body = Body} = xmpp:decode(El)
      end, Range),
    #iq{from = Room, id = I, type = result,
	sub_els = [#mam_fin{xmlns = ?NS_MAM_2,
			    rsm = RSM,
			    complete = true}]} = recv_iq(Config),
    match_rsm_count(RSM, length(Range)).

query_all(Config, From, To) ->
    lists:foreach(
      fun(NS) ->
	      query_all(Config, From, To, NS)
      end, ?VERSIONS).

query_all(Config, From, To, NS) ->
    QID = p1_rand:get_string(),
    Range = lists:seq(1, 5),
    ID = send_query(Config, #mam_query{xmlns = NS, id = QID}),
    recv_archived_messages(Config, From, To, QID, Range),
    RSM = recv_fin(Config, ID, QID, NS, _Complete = true),
    match_rsm_count(RSM, 5).

query_with(Config, From, To) ->
    lists:foreach(
      fun(NS) ->
	      query_with(Config, From, To, NS)
      end, ?VERSIONS).

query_with(Config, From, To, NS) ->
    Peer = ?config(peer, Config),
    BarePeer = jid:remove_resource(Peer),
    QID = p1_rand:get_string(),
    Range = lists:seq(1, 5),
    lists:foreach(
      fun(JID) ->
	      ct:comment("Sending query with jid ~s", [jid:encode(JID)]),
	      Query = if NS == ?NS_MAM_TMP ->
			      #mam_query{xmlns = NS, with = JID, id = QID};
			 true ->
			      Fs = mam_query:encode([{with, JID}]),
			      #mam_query{xmlns = NS, id = QID,
					 xdata = #xdata{type = submit,
							fields = Fs}}
		      end,
	      ID = send_query(Config, Query),
	      recv_archived_messages(Config, From, To, QID, Range),
	      RSM = recv_fin(Config, ID, QID, NS, true),
	      match_rsm_count(RSM, 5)
      end, [Peer, BarePeer]).

query_rsm_max(Config, From, To) ->
    lists:foreach(
      fun(NS) ->
	      query_rsm_max(Config, From, To, NS)
      end, ?VERSIONS).

query_rsm_max(Config, From, To, NS) ->
    lists:foreach(
      fun(Max) ->
	      QID = p1_rand:get_string(),
	      Range = lists:sublist(lists:seq(1, Max), 5),
	      Query = #mam_query{xmlns = NS, id = QID, rsm = #rsm_set{max = Max}},
	      ID = send_query(Config, Query),
	      recv_archived_messages(Config, From, To, QID, Range),
	      IsComplete = Max >= 5,
	      RSM = recv_fin(Config, ID, QID, NS, IsComplete),
	      match_rsm_count(RSM, 5)
      end, lists:seq(0, 6)).

query_rsm_after(Config, From, To) ->
    lists:foreach(
      fun(NS) ->
	      query_rsm_after(Config, From, To, NS)
      end, ?VERSIONS).

query_rsm_after(Config, From, To, NS) ->
    lists:foldl(
      fun(Range, #rsm_first{data = After}) ->
	      ct:comment("Retrieving ~p messages after '~s'",
			 [length(Range), After]),
	      QID = p1_rand:get_string(),
	      Query = #mam_query{xmlns = NS, id = QID,
				 rsm = #rsm_set{'after' = After}},
	      ID = send_query(Config, Query),
	      recv_archived_messages(Config, From, To, QID, Range),
	      RSM = #rsm_set{first = First} =
		  recv_fin(Config, ID, QID, NS, true),
	      match_rsm_count(RSM, 5),
	      First
      end, #rsm_first{data = undefined},
      [lists:seq(N, 5) || N <- lists:seq(1, 6)]).

query_rsm_before(Config, From, To) ->
    lists:foreach(
      fun(NS) ->
	      query_rsm_before(Config, From, To, NS)
      end, ?VERSIONS).

query_rsm_before(Config, From, To, NS) ->
    lists:foldl(
      fun(Range, Before) ->
	      ct:comment("Retrieving ~p messages before '~s'",
			 [length(Range), Before]),
	      QID = p1_rand:get_string(),
	      Query = #mam_query{xmlns = NS, id = QID,
				 rsm = #rsm_set{before = Before}},
	      ID = send_query(Config, Query),
	      recv_archived_messages(Config, From, To, QID, Range),
	      RSM = #rsm_set{last = Last} =
		  recv_fin(Config, ID, QID, NS, true),
	      match_rsm_count(RSM, 5),
	      Last
      end, <<"">>, lists:reverse([lists:seq(1, N) || N <- lists:seq(0, 5)])).

match_rsm_count(#rsm_set{count = undefined}, _) ->
    %% The backend doesn't support counting
    ok;
match_rsm_count(#rsm_set{count = Count1}, Count2) ->
    ct:comment("Checking if RSM 'count' is ~p", [Count2]),
    ?match(Count2, Count1).
