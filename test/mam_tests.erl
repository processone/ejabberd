%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mam_tests).

%% API
-compile(export_all).
-import(suite, [get_features/1, disconnect/1, my_jid/1, send_recv/2,
		wait_for_slave/1, server_jid/1, send/2, get_features/2,
		wait_for_master/1, recv_message/1, recv_iq/1, muc_room_jid/1,
		muc_jid/1, is_feature_advertised/3, get_event/1, put_event/2]).

-include("suite.hrl").
-define(VERSIONS, [?NS_MAM_TMP, ?NS_MAM_0, ?NS_MAM_1]).

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
    clean(disconnect(Config)).

fake_by(Config) ->
    BareServerJID = server_jid(Config),
    FullServerJID = jid:replace_resource(BareServerJID, randoms:get_string()),
    FullMyJID = my_jid(Config),
    BareMyJID = jid:remove_resource(FullMyJID),
    Fakes = lists:flatmap(
	      fun(JID) ->
		      [#mam_archived{id = randoms:get_string(), by = JID},
		       #stanza_id{id = randoms:get_string(), by = JID}]
	      end, [BareServerJID, FullServerJID, BareMyJID, FullMyJID]),
    Body = xmpp:mk_text(<<"body">>),
    ForeignJID = jid:make(randoms:get_string()),
    Archived = #mam_archived{id = randoms:get_string(), by = ForeignJID},
    StanzaID = #stanza_id{id = randoms:get_string(), by = ForeignJID},
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
      master_slave_test(muc)]}.

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
    %% Fill in some history
    send_messages_to_room(Config, lists:seq(1, 21)),
    %% We now should be able to retrieve those via MAM, even though
    %% MAM is disabled. However, only last 20 messages should be received.
    recv_messages_from_room(Config, lists:seq(2, 21)),
    %% Now enable MAM for the conference
    %% Retrieve config first
    #iq{type = result, sub_els = [#muc_owner{config = #xdata{} = RoomCfg}]} =
        send_recv(Config, #iq{type = get, sub_els = [#muc_owner{}],
                              to = Room}),
    %% Find the MAM field in the config and enable it
    NewFields = lists:flatmap(
		  fun(#xdata_field{var = <<"mam">> = Var}) ->
			  [#xdata_field{var = Var, values = [<<"1">>]}];
		     (_) ->
			  []
		  end, RoomCfg#xdata.fields),
    NewRoomCfg = #xdata{type = submit, fields = NewFields},
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, to = Room,
			      sub_els = [#muc_owner{config = NewRoomCfg}]}),
    #message{from = Room, type = groupchat,
	     sub_els = [#muc_user{status_codes = [104]}]} = recv_message(Config),
    %% Check if MAM has been enabled
    true = is_feature_advertised(Config, ?NS_MAM_1, Room),
    %% We now sending some messages again
    send_messages_to_room(Config, lists:seq(1, 5)),
    %% And retrieve them via MAM again.
    recv_messages_from_room(Config, lists:seq(1, 5)),
    put_event(Config, disconnect),
    clean(disconnect(Config)).

muc_slave(Config) ->
    disconnect = get_event(Config),
    clean(disconnect(Config)).

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
	      ct:comment("Retreiving ~pth message in range ~p",
			 [N, Range]),
              Body = xmpp:mk_text(integer_to_binary(N)),
              #message{to = MyJID,
                       sub_els =
                           [#mam_result{
                               queryid = QID,
                               sub_els =
                                   [#forwarded{
                                       delay = #delay{},
                                       xml_els = [El]}]}]} = recv_message(Config),
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

recv_fin(Config, I, QueryID, ?NS_MAM_1 = NS, IsComplete) ->
    ct:comment("Receiving fin iq for namespace '~s'", [NS]),
    #iq{type = result, id = I,
	sub_els = [#mam_fin{xmlns = NS,
			    id = QueryID,
			    complete = Complete,
			    rsm = RSM}]} = recv_iq(Config),
    ct:comment("Checking if complete is ~s", [IsComplete]),
    Complete = IsComplete,
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
    Complete = IsComplete,
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
    QID = randoms:get_string(),
    Count = length(Range),
    I = send(Config, #iq{type = set, to = Room,
			 sub_els = [#mam_query{xmlns = ?NS_MAM_1, id = QID}]}),
    lists:foreach(
      fun(N) ->
	      Body = xmpp:mk_text(integer_to_binary(N)),
	      #message{
		 to = MyJID, from = Room,
		 sub_els =
		     [#mam_result{
			 xmlns = ?NS_MAM_1,
			 queryid = QID,
			 sub_els =
			     [#forwarded{
				 delay = #delay{},
				 xml_els = [El]}]}]} = recv_message(Config),
	      #message{from = MyNickJID,
		       type = groupchat,
		       body = Body} = xmpp:decode(El)
      end, Range),
    #iq{from = Room, id = I, type = result,
	sub_els = [#mam_fin{xmlns = ?NS_MAM_1,
			    id = QID,
			    rsm = #rsm_set{count = Count},
			    complete = true}]} = recv_iq(Config).

query_all(Config, From, To) ->
    lists:foreach(
      fun(NS) ->
	      query_all(Config, From, To, NS)
      end, ?VERSIONS).

query_all(Config, From, To, NS) ->
    QID = randoms:get_string(),
    Range = lists:seq(1, 5),
    ID = send_query(Config, #mam_query{xmlns = NS, id = QID}),
    recv_archived_messages(Config, From, To, QID, Range),
    #rsm_set{count = 5} = recv_fin(Config, ID, QID, NS, _Complete = true).

query_with(Config, From, To) ->
    lists:foreach(
      fun(NS) ->
	      query_with(Config, From, To, NS)
      end, ?VERSIONS).

query_with(Config, From, To, NS) ->
    Peer = ?config(peer, Config),
    BarePeer = jid:remove_resource(Peer),
    QID = randoms:get_string(),
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
	      #rsm_set{count = 5} = recv_fin(Config, ID, QID, NS, true)
      end, [Peer, BarePeer]).

query_rsm_max(Config, From, To) ->
    lists:foreach(
      fun(NS) ->
	      query_rsm_max(Config, From, To, NS)
      end, ?VERSIONS).

query_rsm_max(Config, From, To, NS) ->
    lists:foreach(
      fun(Max) ->
	      QID = randoms:get_string(),
	      Range = lists:sublist(lists:seq(1, Max), 5),
	      Query = #mam_query{xmlns = NS, id = QID, rsm = #rsm_set{max = Max}},
	      ID = send_query(Config, Query),
	      recv_archived_messages(Config, From, To, QID, Range),
	      IsComplete = Max >= 5,
	      #rsm_set{count = 5} = recv_fin(Config, ID, QID, NS, IsComplete)
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
	      QID = randoms:get_string(),
	      Query = #mam_query{xmlns = NS, id = QID,
				 rsm = #rsm_set{'after' = After}},
	      ID = send_query(Config, Query),
	      recv_archived_messages(Config, From, To, QID, Range),
	      #rsm_set{count = 5, first = First} =
		  recv_fin(Config, ID, QID, NS, true),
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
	      QID = randoms:get_string(),
	      Query = #mam_query{xmlns = NS, id = QID,
				 rsm = #rsm_set{before = Before}},
	      ID = send_query(Config, Query),
	      recv_archived_messages(Config, From, To, QID, Range),
	      #rsm_set{count = 5, last = Last} =
		  recv_fin(Config, ID, QID, NS, true),
	      Last
      end, <<"">>, lists:reverse([lists:seq(1, N) || N <- lists:seq(0, 5)])).
