%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 Oct 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(muc_tests).

%% API
-compile(export_all).
-import(suite, [recv_presence/1, send_recv/2, my_jid/1, muc_room_jid/1,
		send/2, recv_message/1, recv_iq/1, muc_jid/1,
		alt_room_jid/1, wait_for_slave/1, wait_for_master/1,
		disconnect/1, put_event/2, get_event/1, peer_muc_jid/1,
		my_muc_jid/1, get_features/2, set_opt/3]).
-include("suite.hrl").
-include("jid.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single tests
%%%===================================================================
single_cases() ->
    {muc_single, [sequence],
     [single_test(service_presence_error),
      single_test(service_message_error),
      single_test(service_unknown_ns_iq_error),
      single_test(service_iq_set_error),
      single_test(service_improper_iq_error),
      single_test(service_features),
      single_test(service_disco_info_node_error),
      single_test(service_disco_items),
      single_test(service_unique),
      single_test(service_vcard),
      single_test(configure_non_existent),
      single_test(cancel_configure_non_existent),
      single_test(service_subscriptions)]}.

service_presence_error(Config) ->
    Service = muc_jid(Config),
    ServiceResource = jid:replace_resource(Service, randoms:get_string()),
    lists:foreach(
      fun(To) ->
	      send(Config, #presence{type = error, to = To}),
	      lists:foreach(
		fun(Type) ->
			#presence{type = error} = Err =
			    send_recv(Config, #presence{type = Type, to = To}),
			#stanza_error{reason = 'service-unavailable'} =
			    xmpp:get_error(Err)
		end, [available, unavailable])
      end, [Service, ServiceResource]),
    disconnect(Config).

service_message_error(Config) ->
    Service = muc_jid(Config),
    send(Config, #message{type = error, to = Service}),
    lists:foreach(
      fun(Type) ->
	      #message{type = error} = Err1 =
		  send_recv(Config, #message{type = Type, to = Service}),
	      #stanza_error{reason = 'forbidden'} = xmpp:get_error(Err1)
      end, [chat, normal, headline, groupchat]),
    ServiceResource = jid:replace_resource(Service, randoms:get_string()),
    send(Config, #message{type = error, to = ServiceResource}),
    lists:foreach(
      fun(Type) ->
	      #message{type = error} = Err2 =
		  send_recv(Config, #message{type = Type, to = ServiceResource}),
	      #stanza_error{reason = 'service-unavailable'} = xmpp:get_error(Err2)
      end, [chat, normal, headline, groupchat]),
    disconnect(Config).

service_unknown_ns_iq_error(Config) ->
    Service = muc_jid(Config),
    ServiceResource = jid:replace_resource(Service, randoms:get_string()),
    lists:foreach(
      fun(To) ->
	      send(Config, #iq{type = result, to = To}),
	      send(Config, #iq{type = error, to = To}),
	      lists:foreach(
		fun(Type) ->
			#iq{type = error} = Err1 =
			    send_recv(Config, #iq{type = Type, to = To,
						  sub_els = [#presence{}]}),
			#stanza_error{reason = 'service-unavailable'} =
			    xmpp:get_error(Err1)
		end, [set, get])
      end, [Service, ServiceResource]),
    disconnect(Config).

service_iq_set_error(Config) ->
    Service = muc_jid(Config),
    lists:foreach(
      fun(SubEl) ->
	      send(Config, #iq{type = result, to = Service,
			       sub_els = [SubEl]}),
	      #iq{type = error} = Err2 =
		  send_recv(Config, #iq{type = set, to = Service,
					sub_els = [SubEl]}),
	      #stanza_error{reason = 'not-allowed'} =
		  xmpp:get_error(Err2)
      end, [#disco_items{}, #disco_info{}, #vcard_temp{},
	    #muc_unique{}, #muc_subscriptions{}]),
    disconnect(Config).

service_improper_iq_error(Config) ->
    Service = muc_jid(Config),
    lists:foreach(
      fun(SubEl) ->
	      send(Config, #iq{type = result, to = Service,
			       sub_els = [SubEl]}),
	      lists:foreach(
		fun(Type) ->
			#iq{type = error} = Err3 =
			    send_recv(Config, #iq{type = Type, to = Service,
						  sub_els = [SubEl]}),
			#stanza_error{reason = Reason} = xmpp:get_error(Err3),
			true = Reason /= 'internal-server-error'
		end, [set, get])
      end, [#disco_item{jid = Service},
	    #identity{category = <<"category">>, type = <<"type">>},
	    #vcard_email{}, #muc_subscribe{nick = ?config(nick, Config)}]),
    disconnect(Config).

service_features(Config) ->
    ServerHost = ?config(server_host, Config),
    MUC = muc_jid(Config),
    Features = sets:from_list(get_features(Config, MUC)),
    MAMFeatures = case gen_mod:is_loaded(ServerHost, mod_mam) of
		      true -> [?NS_MAM_TMP, ?NS_MAM_0, ?NS_MAM_1];
		      false -> []
		  end,
    RequiredFeatures = sets:from_list(
			 [?NS_DISCO_INFO, ?NS_DISCO_ITEMS,
			  ?NS_REGISTER, ?NS_MUC,
			  ?NS_VCARD, ?NS_MUCSUB, ?NS_MUC_UNIQUE
			  | MAMFeatures]),
    ct:comment("Checking if all needed disco features are set"),
    true = sets:is_subset(RequiredFeatures, Features),
    disconnect(Config).

service_disco_info_node_error(Config) ->
    MUC = muc_jid(Config),
    Node = randoms:get_string(),
    #iq{type = error} = Err =
	send_recv(Config, #iq{type = get, to = MUC,
			      sub_els = [#disco_info{node = Node}]}),
    #stanza_error{reason = 'item-not-found'} = xmpp:get_error(Err),
    disconnect(Config).

service_disco_items(Config) ->
    #jid{server = Service} = muc_jid(Config),
    Rooms = lists:sort(
	      lists:map(
		fun(I) ->
			RoomName = integer_to_binary(I),
			jid:make(RoomName, Service)
		end, lists:seq(1, 5))),
    lists:foreach(
      fun(Room) ->
	      ok = join_new(Config, Room)
      end, Rooms),
    Items = disco_items(Config),
    Rooms = [J || #disco_item{jid = J} <- Items],
    lists:foreach(
      fun(Room) ->
	      ok = leave(Config, Room)
      end, Rooms),
    [] = disco_items(Config),
    disconnect(Config).

service_vcard(Config) ->
    MUC = muc_jid(Config),
    ct:comment("Retreiving vCard from ~s", [jid:encode(MUC)]),
    #iq{type = result, sub_els = [#vcard_temp{}]} =
	send_recv(Config, #iq{type = get, to = MUC, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

service_unique(Config) ->
    MUC = muc_jid(Config),
    ct:comment("Requesting muc unique from ~s", [jid:encode(MUC)]),
    #iq{type = result, sub_els = [#muc_unique{name = Name}]} =
	send_recv(Config, #iq{type = get, to = MUC, sub_els = [#muc_unique{}]}),
    ct:comment("Checking if unique name is set in the response"),
    <<_, _/binary>> = Name,
    disconnect(Config).

configure_non_existent(Config) ->
    [_|_] = get_config(Config),
    disconnect(Config).

cancel_configure_non_existent(Config) ->
    Room = muc_room_jid(Config),
    #iq{type = result, sub_els = []} =
	send_recv(Config,
		  #iq{to = Room, type = set,
		      sub_els = [#muc_owner{config = #xdata{type = cancel}}]}),
    disconnect(Config).

service_subscriptions(Config) ->
    MUC = #jid{server = Service} = muc_jid(Config),
    Rooms = lists:sort(
	      lists:map(
		fun(I) ->
			RoomName = integer_to_binary(I),
			jid:make(RoomName, Service)
		end, lists:seq(1, 5))),
    lists:foreach(
      fun(Room) ->
	      ok = join_new(Config, Room),
	      [104] = set_config(Config, [{allow_subscription, true}], Room),
	      [] = subscribe(Config, [], Room)
      end, Rooms),
    #iq{type = result, sub_els = [#muc_subscriptions{list = JIDs}]} =
	send_recv(Config, #iq{type = get, to = MUC,
			      sub_els = [#muc_subscriptions{}]}),
    Rooms = lists:sort(JIDs),
    lists:foreach(
      fun(Room) ->
	      ok = unsubscribe(Config, Room),
	      ok = leave(Config, Room)
      end, Rooms),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {muc_master_slave, [sequence],
     [master_slave_test(register),
      master_slave_test(groupchat_msg),
      master_slave_test(private_msg),
      master_slave_test(set_subject),
      master_slave_test(history),
      master_slave_test(invite),
      master_slave_test(invite_members_only),
      master_slave_test(invite_password_protected),
      master_slave_test(voice_request),
      master_slave_test(change_role),
      master_slave_test(kick),
      master_slave_test(change_affiliation),
      master_slave_test(destroy),
      master_slave_test(vcard),
      master_slave_test(nick_change),
      master_slave_test(config_title_desc),
      master_slave_test(config_public_list),
      master_slave_test(config_password),
      master_slave_test(config_whois),
      master_slave_test(config_members_only),
      master_slave_test(config_moderated),
      master_slave_test(config_private_messages),
      master_slave_test(config_query),
      master_slave_test(config_allow_invites),
      master_slave_test(config_visitor_status),
      master_slave_test(config_allow_voice_requests),
      master_slave_test(config_voice_request_interval),
      master_slave_test(config_visitor_nickchange),
      master_slave_test(join_conflict)]}.

join_conflict_master(Config) ->
    ok = join_new(Config),
    put_event(Config, join),
    ct:comment("Waiting for 'leave' command from the slave"),
    leave = get_event(Config),
    ok = leave(Config),
    disconnect(Config).

join_conflict_slave(Config) ->
    NewConfig = set_opt(nick, ?config(peer_nick, Config), Config),
    ct:comment("Waiting for 'join' command from the master"),
    join = get_event(Config),
    ct:comment("Fail trying to join the room with conflicting nick"),
    #stanza_error{reason = 'conflict'} = join(NewConfig),
    put_event(Config, leave),
    disconnect(NewConfig).

groupchat_msg_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(slave, Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    ok = master_join(Config),
    lists:foreach(
      fun(I) ->
	      Body = xmpp:mk_text(integer_to_binary(I)),
	      send(Config, #message{type = groupchat, to = Room,
				    body = Body}),
	      #message{type = groupchat, from = MyNickJID,
		       body = Body} = recv_message(Config)
      end, lists:seq(1, 5)),
    #muc_user{items = [#muc_item{jid = PeerJID,
				 role = none,
				 affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

groupchat_msg_slave(Config) ->
    Room = muc_room_jid(Config),
    PeerNick = ?config(master_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    {[], _, _} = slave_join(Config),
    lists:foreach(
      fun(I) ->
	      Body = xmpp:mk_text(integer_to_binary(I)),
	      #message{type = groupchat, from = PeerNickJID,
		       body = Body} = recv_message(Config)
      end, lists:seq(1, 5)),
    ok = leave(Config),
    disconnect(Config).

private_msg_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(slave, Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ok = master_join(Config),
    lists:foreach(
      fun(I) ->
	      Body = xmpp:mk_text(integer_to_binary(I)),
	      send(Config, #message{type = chat, to = PeerNickJID,
				    body = Body})
      end, lists:seq(1, 5)),
    #muc_user{items = [#muc_item{jid = PeerJID,
				 role = none,
				 affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, unavailable),
    ct:comment("Fail trying to send a private message to non-existing occupant"),
    send(Config, #message{type = chat, to = PeerNickJID}),
    #message{from = PeerNickJID, type = error} = ErrMsg = recv_message(Config),
    #stanza_error{reason = 'item-not-found'} = xmpp:get_error(ErrMsg),
    ok = leave(Config),
    disconnect(Config).

private_msg_slave(Config) ->
    Room = muc_room_jid(Config),
    PeerNick = ?config(master_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    {[], _, _} = slave_join(Config),
    lists:foreach(
      fun(I) ->
	      Body = xmpp:mk_text(integer_to_binary(I)),
	      #message{type = chat, from = PeerNickJID,
		       body = Body} = recv_message(Config)
      end, lists:seq(1, 5)),
    ok = leave(Config),
    disconnect(Config).

set_subject_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(slave, Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    Subject1 = xmpp:mk_text(?config(room_subject, Config)),
    Subject2 = xmpp:mk_text(<<"new-", (?config(room_subject, Config))/binary>>),
    ok = master_join(Config),
    ct:comment("Setting 1st subject"),
    send(Config, #message{type = groupchat, to = Room,
			  subject = Subject1}),
    #message{type = groupchat, from = MyNickJID,
	     subject = Subject1} = recv_message(Config),
    ct:comment("Waiting for the slave to leave"),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ct:comment("Setting 2nd subject"),
    send(Config, #message{type = groupchat, to = Room,
			  subject = Subject2}),
    #message{type = groupchat, from = MyNickJID,
	     subject = Subject2} = recv_message(Config),
    ct:comment("Asking the slave to join"),
    put_event(Config, join),
    recv_muc_presence(Config, PeerNickJID, available),
    ct:comment("Receiving 1st subject set by the slave"),
    #message{type = groupchat, from = PeerNickJID,
	     subject = Subject1} = recv_message(Config),
    ct:comment("Disallow subject change"),
    [104] = set_config(Config, [{changesubject, false}]),
    ct:comment("Waiting for the slave to leave"),
    #muc_user{items = [#muc_item{jid = PeerJID,
				 role = none,
				 affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

set_subject_slave(Config) ->
    Room = muc_room_jid(Config),
    MyNickJID = my_muc_jid(Config),
    PeerNick = ?config(master_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    Subject1 = xmpp:mk_text(?config(room_subject, Config)),
    Subject2 = xmpp:mk_text(<<"new-", (?config(room_subject, Config))/binary>>),
    {[], _, _} = slave_join(Config),
    ct:comment("Receiving 1st subject set by the master"),
    #message{type = groupchat, from = PeerNickJID,
	     subject = Subject1} = recv_message(Config),
    ok = leave(Config),
    ct:comment("Waiting for 'join' command from the master"),
    join = get_event(Config),
    {[], SubjMsg2, _} = join(Config),
    ct:comment("Checking if the master has set 2nd subject during our absence"),
    #message{type = groupchat, from = PeerNickJID,
	     subject = Subject2} = SubjMsg2,
    ct:comment("Setting 1st subject"),
    send(Config, #message{to = Room, type = groupchat, subject = Subject1}),
    #message{type = groupchat, from = MyNickJID,
	     subject = Subject1} = recv_message(Config),
    ct:comment("Waiting for the master to disallow subject change"),
    [104] = recv_config_change_message(Config),
    ct:comment("Fail trying to change the subject"),
    send(Config, #message{to = Room, type = groupchat, subject = Subject2}),
    #message{from = Room, type = error} = ErrMsg = recv_message(Config),
    #stanza_error{reason = 'forbidden'} = xmpp:get_error(ErrMsg),
    ok = leave(Config),
    disconnect(Config).

history_master(Config) ->
    Room = muc_room_jid(Config),
    ServerHost = ?config(server_host, Config),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    PeerNickJID = peer_muc_jid(Config),
    Size = gen_mod:get_module_opt(ServerHost, mod_muc, history_size,
				  fun(I) when is_integer(I), I>=0 -> I end,
				  20),
    ok = join_new(Config),
    ct:comment("Putting ~p+1 messages in the history", [Size]),
    %% Only Size messages will be stored
    lists:foreach(
      fun(I) ->
	      Body = xmpp:mk_text(integer_to_binary(I)),
	      send(Config, #message{to = Room, type = groupchat,
				    body = Body}),
	      #message{type = groupchat, from = MyNickJID,
		       body = Body} = recv_message(Config)
      end, lists:seq(0, Size)),
    put_event(Config, join),
    lists:foreach(
      fun(Type) ->
	      recv_muc_presence(Config, PeerNickJID, Type)
      end, [available, unavailable,
	    available, unavailable,
	    available, unavailable,
	    available, unavailable]),
    ok = leave(Config),
    disconnect(Config).

history_slave(Config) ->
    Room = muc_room_jid(Config),
    PeerNick = ?config(peer_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ServerHost = ?config(server_host, Config),
    Size = gen_mod:get_module_opt(ServerHost, mod_muc, history_size,
				  fun(I) when is_integer(I), I>=0 -> I end,
				  20),
    ct:comment("Waiting for 'join' command from the master"),
    join = get_event(Config),
    {History, _, _} = join(Config),
    ct:comment("Checking ordering of history events"),
    BodyList = [binary_to_integer(xmpp:get_text(Body))
		|| #message{type = groupchat, from = From,
			    body = Body} <- History,
		   From == PeerNickJID],
    BodyList = lists:seq(1, Size),
    ok = leave(Config),
    %% If the client wishes to receive no history, it MUST set the 'maxchars'
    %% attribute to a value of "0" (zero)
    %% (http://xmpp.org/extensions/xep-0045.html#enter-managehistory)
    ct:comment("Checking if maxchars=0 yields to no history"),
    {[], _, _} = join(Config, #muc{history = #muc_history{maxchars = 0}}),
    ok = leave(Config),
    ct:comment("Receiving only 10 last stanzas"),
    {History10, _, _} = join(Config,
				 #muc{history = #muc_history{maxstanzas = 10}}),
    BodyList10 = [binary_to_integer(xmpp:get_text(Body))
		  || #message{type = groupchat, from = From,
			      body = Body} <- History10,
		     From == PeerNickJID],
    BodyList10 = lists:nthtail(Size-10, lists:seq(1, Size)),
    ok = leave(Config),
    #delay{stamp = TS} = xmpp:get_subtag(hd(History), #delay{}),
    ct:comment("Receiving all history without the very first element"),
    {HistoryWithoutFirst, _, _} = join(Config,
					   #muc{history = #muc_history{since = TS}}),
    BodyListWithoutFirst = [binary_to_integer(xmpp:get_text(Body))
			    || #message{type = groupchat, from = From,
					body = Body} <- HistoryWithoutFirst,
			       From == PeerNickJID],
    BodyListWithoutFirst = lists:nthtail(1, lists:seq(1, Size)),
    ok = leave(Config),
    disconnect(Config).

invite_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(peer, Config),
    ok = join_new(Config),
    wait_for_slave(Config),
    %% Inviting the peer
    send(Config, #message{to = Room, type = normal,
			  sub_els =
			      [#muc_user{
				  invites =
				      [#muc_invite{to = PeerJID}]}]}),
    #message{from = Room} = DeclineMsg = recv_message(Config),
    #muc_user{decline = #muc_decline{from = PeerJID}} =
	xmpp:get_subtag(DeclineMsg, #muc_user{}),
    ok = leave(Config),
    disconnect(Config).

invite_slave(Config) ->
    Room = muc_room_jid(Config),
    wait_for_master(Config),
    PeerJID = ?config(master, Config),
    #message{from = Room, type = normal} = Msg = recv_message(Config),
    #muc_user{invites = [#muc_invite{from = PeerJID}]} =
	xmpp:get_subtag(Msg, #muc_user{}),
    %% Decline invitation
    send(Config,
	 #message{to = Room,
		  sub_els = [#muc_user{
				decline = #muc_decline{to = PeerJID}}]}),
    disconnect(Config).

invite_members_only_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(slave, Config),
    ok = join_new(Config),
    %% Setting the room to members-only
    [_|_] = set_config(Config, [{membersonly, true}]),
    wait_for_slave(Config),
    %% Inviting the peer
    send(Config, #message{to = Room, type = normal,
			  sub_els =
			      [#muc_user{
				  invites =
				      [#muc_invite{to = PeerJID}]}]}),
    #message{from = Room, type = normal} = AffMsg = recv_message(Config),
    #muc_user{items = [#muc_item{jid = PeerJID, affiliation = member}]} =
	xmpp:get_subtag(AffMsg, #muc_user{}),
    ok = leave(Config),
    disconnect(Config).

invite_members_only_slave(Config) ->
    Room = muc_room_jid(Config),
    wait_for_master(Config),
    %% Receiving invitation
    #message{from = Room, type = normal} = recv_message(Config),
    disconnect(Config).

invite_password_protected_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(slave, Config),
    Password = randoms:get_string(),
    ok = join_new(Config),
    [104] = set_config(Config, [{passwordprotectedroom, true},
                                    {roomsecret, Password}]),
    put_event(Config, Password),
    %% Inviting the peer
    send(Config, #message{to = Room, type = normal,
			  sub_els =
			      [#muc_user{
				  invites =
				      [#muc_invite{to = PeerJID}]}]}),
    ok = leave(Config),
    disconnect(Config).

invite_password_protected_slave(Config) ->
    Room = muc_room_jid(Config),
    Password = get_event(Config),
    %% Receiving invitation
    #message{from = Room, type = normal} = Msg = recv_message(Config),
    #muc_user{password = Password} = xmpp:get_subtag(Msg, #muc_user{}),
    disconnect(Config).

voice_request_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(slave, Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ok = join_new(Config),
    [104] = set_config(Config, [{members_by_default, false}]),
    wait_for_slave(Config),
    #muc_user{
       items = [#muc_item{role = visitor,
			  jid = PeerJID,
			  affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    ct:comment("Receiving voice request"),
    #message{from = Room, type = normal} = VoiceReq = recv_message(Config),
    #xdata{type = form, fields = Fs} = xmpp:get_subtag(VoiceReq, #xdata{}),
    [{jid, PeerJID},
     {request_allow, false},
     {role, participant},
     {roomnick, PeerNick}] = lists:sort(muc_request:decode(Fs)),
    ct:comment("Approving voice request"),
    ApprovalFs = muc_request:encode([{jid, PeerJID}, {role, participant},
				     {nick, PeerNick}, {request_allow, true}]),
    send(Config, #message{to = Room, sub_els = [#xdata{type = submit,
						       fields = ApprovalFs}]}),
    #muc_user{
       items = [#muc_item{role = participant,
			  jid = PeerJID,
			  affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    ct:comment("Waiting for the slave to leave"),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

voice_request_slave(Config) ->
    Room = muc_room_jid(Config),
    MyJID = my_jid(Config),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    wait_for_master(Config),
    {[], _, _} = join(Config, visitor),
    ct:comment("Requesting voice"),
    Fs = muc_request:encode([{role, participant}]),
    X = #xdata{type = submit, fields = Fs},
    send(Config, #message{to = Room, sub_els = [X]}),
    ct:comment("Waiting to become a participant"),
    #muc_user{
       items = [#muc_item{role = participant,
			  jid = MyJID,
			  affiliation = none}]} =
	recv_muc_presence(Config, MyNickJID, available),
    ok = leave(Config),
    disconnect(Config).

change_role_master(Config) ->
    Room = muc_room_jid(Config),
    MyJID = my_jid(Config),
    MyNick = ?config(nick, Config),
    PeerJID = ?config(slave, Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ok = join_new(Config),
    ct:comment("Waiting for the slave to join"),
    wait_for_slave(Config),
    #muc_user{items = [#muc_item{role = participant,
				 jid = PeerJID,
				 affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    lists:foreach(
      fun(Role) ->
	      ct:comment("Checking if the slave is not in the roles list"),
	      case get_role(Config, Role) of
		  [#muc_item{jid = MyJID, affiliation = owner,
			     role = moderator, nick = MyNick}] when Role == moderator ->
		      ok;
		  [] ->
		      ok
	      end,
	      Reason = randoms:get_string(),
	      put_event(Config, {Role, Reason}),
	      ok = set_role(Config, Role, Reason),
	      ct:comment("Receiving role change to ~s", [Role]),
	      #muc_user{
		 items = [#muc_item{role = Role,
				    affiliation = none,
				    reason = Reason}]} =
		  recv_muc_presence(Config, PeerNickJID, available),
	      [#muc_item{role = Role, affiliation = none,
			 nick = PeerNick}|_] = get_role(Config, Role)
      end, [visitor, participant, moderator]),
    put_event(Config, disconnect),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

change_role_slave(Config) ->
    wait_for_master(Config),
    {[], _, _} = join(Config),
    change_role_slave(Config, get_event(Config)).

change_role_slave(Config, {Role, Reason}) ->
    Room = muc_room_jid(Config),
    MyNick = ?config(slave_nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    ct:comment("Receiving role change to ~s", [Role]),
    #muc_user{status_codes = Codes,
	      items = [#muc_item{role = Role,
				 affiliation = none,
				 reason = Reason}]} =
	recv_muc_presence(Config, MyNickJID, available),
    true = lists:member(110, Codes),
    change_role_slave(Config, get_event(Config));
change_role_slave(Config, disconnect) ->
    ok = leave(Config),
    disconnect(Config).

change_affiliation_master(Config) ->
    Room = muc_room_jid(Config),
    MyJID = my_jid(Config),
    MyBareJID = jid:remove_resource(MyJID),
    MyNick = ?config(nick, Config),
    PeerJID = ?config(slave, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ok = join_new(Config),
    ct:comment("Waiting for the slave to join"),
    wait_for_slave(Config),
    #muc_user{items = [#muc_item{role = participant,
				 jid = PeerJID,
				 affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    lists:foreach(
      fun({Aff, Role, Status}) ->
	      ct:comment("Checking if slave is not in affiliation list"),
	      case get_affiliation(Config, Aff) of
		  [#muc_item{jid = MyBareJID,
			     affiliation = owner}] when Aff == owner ->
		      ok;
		  [] ->
		      ok
	      end,
	      Reason = randoms:get_string(),
	      put_event(Config, {Aff, Role, Status, Reason}),
	      ok = set_affiliation(Config, Aff, Reason),
	      ct:comment("Receiving affiliation change to ~s", [Aff]),
	      #muc_user{
		 items = [#muc_item{role = Role,
				    affiliation = Aff,
				    actor = Actor,
				    reason = Reason}]} =
		  recv_muc_presence(Config, PeerNickJID, Status),
	      if Aff == outcast ->
		      ct:comment("Checking if actor is set"),
		      #muc_actor{nick = MyNick} = Actor;
		 true ->
		      ok
	      end,
	      Affs = get_affiliation(Config, Aff),
	      ct:comment("Checking if the affiliation was correctly set"),
	      case lists:keyfind(PeerBareJID, #muc_item.jid, Affs) of
		  false when Aff == none ->
		      ok;
		  #muc_item{affiliation = Aff} ->
		      ok
	      end
      end, [{member, participant, available}, {none, participant, available},
	    {admin, moderator, available}, {owner, moderator, available},
	    {outcast, none, unavailable}]),
    ok = leave(Config),
    disconnect(Config).

change_affiliation_slave(Config) ->
    wait_for_master(Config),
    {[], _, _} = join(Config),
    change_affiliation_slave(Config, get_event(Config)).

change_affiliation_slave(Config, {Aff, Role, Status, Reason}) ->
    Room = muc_room_jid(Config),
    PeerNick = ?config(master_nick, Config),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    ct:comment("Receiving affiliation change to ~s", [Aff]),
    #muc_user{status_codes = Codes,
	      items = [#muc_item{role = Role,
				 actor = Actor,
				 affiliation = Aff,
				 reason = Reason}]} =
	recv_muc_presence(Config, MyNickJID, Status),
    true = lists:member(110, Codes),
    if Aff == outcast ->
	    ct:comment("Checking for status code '301' (banned)"),
	    true = lists:member(301, Codes),
	    ct:comment("Checking if actor is set"),
	    #muc_actor{nick = PeerNick} = Actor,
	    disconnect(Config);
       true ->
	    change_affiliation_slave(Config, get_event(Config))
    end.

kick_master(Config) ->
    Room = muc_room_jid(Config),
    MyNick = ?config(nick, Config),
    PeerJID = ?config(slave, Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    Reason = <<"Testing">>,
    ok = join_new(Config),
    ct:comment("Waiting for the slave to join"),
    wait_for_slave(Config),
    #muc_user{items = [#muc_item{role = participant,
				 jid = PeerJID,
				 affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    [#muc_item{role = participant, affiliation = none,
	       nick = PeerNick}|_] = get_role(Config, participant),
    ct:comment("Kicking slave"),
    ok = set_role(Config, none, Reason),
    ct:comment("Receiving role change to 'none'"),
    #muc_user{
       status_codes = Codes,
       items = [#muc_item{role = none,
			  affiliation = none,
			  actor = #muc_actor{nick = MyNick},
			  reason = Reason}]} =
	recv_muc_presence(Config, PeerNickJID, unavailable),
    [] = get_role(Config, participant),
    ct:comment("Checking if the code is '307' (kicked)"),
    true = lists:member(307, Codes),
    ok = leave(Config),
    disconnect(Config).

kick_slave(Config) ->
    Room = muc_room_jid(Config),
    PeerNick = ?config(master_nick, Config),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    Reason = <<"Testing">>,
    wait_for_master(Config),
    {[], _, _} = join(Config),
    ct:comment("Receiving role change to 'none'"),
    #muc_user{status_codes = Codes,
	      items = [#muc_item{role = none,
				 affiliation = none,
				 actor = #muc_actor{nick = PeerNick},
				 reason = Reason}]} =
	recv_muc_presence(Config, MyNickJID, unavailable),
    ct:comment("Checking if codes '110' (self-presence) "
	       "and '307' (kicked) are present"),
    true = lists:member(110, Codes),
    true = lists:member(307, Codes),
    disconnect(Config).

destroy_master(Config) ->
    Reason = <<"Testing">>,
    Room = muc_room_jid(Config),
    AltRoom = alt_room_jid(Config),
    PeerJID = ?config(peer, Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    ok = join_new(Config),
    ct:comment("Waiting for slave to join"),
    wait_for_slave(Config),
    #muc_user{items = [#muc_item{role = participant,
				 jid = PeerJID,
				 affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    wait_for_slave(Config),
    ok = destroy(Config, Reason),
    ct:comment("Receiving destruction presence"),
    #muc_user{items = [#muc_item{role = none,
				 affiliation = none}],
	      destroy = #muc_destroy{jid = AltRoom,
				     reason = Reason}} =
	recv_muc_presence(Config, MyNickJID, unavailable),
    disconnect(Config).

destroy_slave(Config) ->
    Reason = <<"Testing">>,
    Room = muc_room_jid(Config),
    AltRoom = alt_room_jid(Config),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    wait_for_master(Config),
    {[], _, _} = join(Config),
    #stanza_error{reason = 'forbidden'} = destroy(Config, Reason),
    wait_for_master(Config),
    ct:comment("Receiving destruction presence"),
    #muc_user{items = [#muc_item{role = none,
				 affiliation = none}],
	      destroy = #muc_destroy{jid = AltRoom,
				     reason = Reason}} =
	recv_muc_presence(Config, MyNickJID, unavailable),
    disconnect(Config).

vcard_master(Config) ->
    Room = muc_room_jid(Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    FN = randoms:get_string(),
    VCard = #vcard_temp{fn = FN},
    ok = join_new(Config),
    ct:comment("Waiting for slave to join"),
    wait_for_slave(Config),
    #muc_user{items = [#muc_item{role = participant,
				 affiliation = none}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    #stanza_error{reason = 'item-not-found'} = get_vcard(Config),
    ok = set_vcard(Config, VCard),
    VCard = get_vcard(Config),
    put_event(Config, VCard),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    leave = get_event(Config),
    ok = leave(Config),
    disconnect(Config).

vcard_slave(Config) ->
    wait_for_master(Config),
    {[], _, _} = join(Config),
    VCard = get_event(Config),
    VCard = get_vcard(Config),
    #stanza_error{reason = 'forbidden'} = set_vcard(Config, VCard),
    ok = leave(Config),
    VCard = get_vcard(Config),
    put_event(Config, leave),
    disconnect(Config).

nick_change_master(Config) ->
    NewNick = randoms:get_string(),
    PeerJID = ?config(peer, Config),
    PeerNickJID = peer_muc_jid(Config),
    ok = master_join(Config),
    put_event(Config, {new_nick, NewNick}),
    ct:comment("Waiting for nickchange presence from the slave"),
    #muc_user{status_codes = Codes,
	      items = [#muc_item{jid = PeerJID,
				 nick = NewNick}]} =
	recv_muc_presence(Config, PeerNickJID, unavailable),
    ct:comment("Checking if code '303' (nick change) is set"),
    true = lists:member(303, Codes),
    ct:comment("Waiting for updated presence from the slave"),
    PeerNewNickJID = jid:replace_resource(PeerNickJID, NewNick),
    recv_muc_presence(Config, PeerNewNickJID, available),
    ct:comment("Waiting for the slave to leave"),
    recv_muc_presence(Config, PeerNewNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

nick_change_slave(Config) ->
    MyJID = my_jid(Config),
    MyNickJID = my_muc_jid(Config),
    {[], _, _} = slave_join(Config),
    {new_nick, NewNick} = get_event(Config),
    MyNewNickJID = jid:replace_resource(MyNickJID, NewNick),
    ct:comment("Sending new presence"),
    send(Config, #presence{to = MyNewNickJID}),
    ct:comment("Receiving nickchange self-presence"),
    #muc_user{status_codes = Codes1,
	      items = [#muc_item{role = participant,
				 jid = MyJID,
				 nick = NewNick}]} =
	recv_muc_presence(Config, MyNickJID, unavailable),
    ct:comment("Checking if codes '110' (self-presence) and "
	       "'303' (nickchange) are present"),
    lists:member(110, Codes1),
    lists:member(303, Codes1),
    ct:comment("Receiving self-presence update"),
    #muc_user{status_codes = Codes2,
	      items = [#muc_item{jid = MyJID,
				 role = participant}]} =
	recv_muc_presence(Config, MyNewNickJID, available),
    ct:comment("Checking if code '110' (self-presence) is set"),
    lists:member(110, Codes2),
    NewConfig = set_opt(nick, NewNick, Config),
    ok = leave(NewConfig),
    disconnect(NewConfig).

config_title_desc_master(Config) ->
    Title = randoms:get_string(),
    Desc = randoms:get_string(),
    Room = muc_room_jid(Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ok = master_join(Config),
    [104] = set_config(Config, [{roomname, Title}, {roomdesc, Desc}]),
    RoomCfg = get_config(Config),
    Title = proplists:get_value(roomname, RoomCfg),
    Desc = proplists:get_value(roomdesc, RoomCfg),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_title_desc_slave(Config) ->
    {[], _, _} = slave_join(Config),
    [104] = recv_config_change_message(Config),
    ok = leave(Config),
    disconnect(Config).

config_public_list_master(Config) ->
    Room = muc_room_jid(Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ok = join_new(Config),
    wait_for_slave(Config),
    recv_muc_presence(Config, PeerNickJID, available),
    lists:member(<<"muc_public">>, get_features(Config, Room)),
    [104] = set_config(Config, [{public_list, false},
				    {publicroom, false}]),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    lists:member(<<"muc_hidden">>, get_features(Config, Room)),
    wait_for_slave(Config),
    ok = leave(Config),
    disconnect(Config).

config_public_list_slave(Config) ->
    Room = muc_room_jid(Config),
    wait_for_master(Config),
    PeerNick = ?config(peer_nick, Config),
    PeerNickJID = peer_muc_jid(Config),
    [#disco_item{jid = Room}] = disco_items(Config),
    [#disco_item{jid = PeerNickJID,
		 name = PeerNick}] = disco_room_items(Config),
    {[], _, _} = join(Config),
    [104] = recv_config_change_message(Config),
    ok = leave(Config),
    [] = disco_items(Config),
    [] = disco_room_items(Config),
    wait_for_master(Config),
    disconnect(Config).

config_password_master(Config) ->
    Password = randoms:get_string(),
    Room = muc_room_jid(Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ok = join_new(Config),
    lists:member(<<"muc_unsecured">>, get_features(Config, Room)),
    [104] = set_config(Config, [{passwordprotectedroom, true},
				    {roomsecret, Password}]),
    lists:member(<<"muc_passwordprotected">>, get_features(Config, Room)),
    put_event(Config, Password),
    recv_muc_presence(Config, PeerNickJID, available),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_password_slave(Config) ->
    Password = get_event(Config),
    #stanza_error{reason = 'not-authorized'} = join(Config),
    #stanza_error{reason = 'not-authorized'} =
	join(Config, #muc{password = randoms:get_string()}),
    {[], _, _} = join(Config, #muc{password = Password}),
    ok = leave(Config),
    disconnect(Config).

config_whois_master(Config) ->
    Room = muc_room_jid(Config),
    PeerNickJID = peer_muc_jid(Config),
    MyNickJID = my_muc_jid(Config),
    ok = master_join(Config),
    lists:member(<<"muc_semianonymous">>, get_features(Config, Room)),
    [172] = set_config(Config, [{whois, anyone}]),
    lists:member(<<"muc_nonanonymous">>, get_features(Config, Room)),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    recv_muc_presence(Config, PeerNickJID, available),
    send(Config, #presence{to = Room}),
    recv_muc_presence(Config, MyNickJID, available),
    [173] = set_config(Config, [{whois, moderators}]),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_whois_slave(Config) ->
    PeerJID = ?config(peer, Config),
    PeerNickJID = peer_muc_jid(Config),
    {[], _, _} = slave_join(Config),
    ct:comment("Checking if the room becomes non-anonymous (code '172')"),
    [172] = recv_config_change_message(Config),
    ct:comment("Re-joining in order to check status codes"),
    ok = leave(Config),
    {[], _, Codes} = join(Config),
    ct:comment("Checking if code '100' (non-anonymous) present"),
    true = lists:member(100, Codes),
    ct:comment("Receiving presence from peer with JID exposed"),
    #muc_user{items = [#muc_item{jid = PeerJID}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    ct:comment("Waiting for the room to become anonymous again (code '173')"),
    [173] = recv_config_change_message(Config),
    ok = leave(Config),
    disconnect(Config).

config_members_only_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    PeerNickJID = peer_muc_jid(Config),
    ok = master_join(Config),
    lists:member(<<"muc_open">>, get_features(Config, Room)),
    [104] = set_config(Config, [{membersonly, true}]),
    #muc_user{status_codes = Codes,
	      items = [#muc_item{jid = PeerJID,
				 affiliation = none,
				 role = none}]} =
	recv_muc_presence(Config, PeerNickJID, unavailable),
    ct:comment("Checking if code '322' (non-member) is set"),
    true = lists:member(322, Codes),
    lists:member(<<"muc_membersonly">>, get_features(Config, Room)),
    ct:comment("Waiting for slave to fail joining the room"),
    set_member = get_event(Config),
    ok = set_affiliation(Config, member, randoms:get_string()),
    #message{from = Room, type = normal} = Msg = recv_message(Config),
    #muc_user{items = [#muc_item{jid = PeerBareJID,
				 affiliation = member}]} =
	xmpp:get_subtag(Msg, #muc_user{}),
    ct:comment("Asking peer to join"),
    put_event(Config, join),
    ct:comment("Waiting for peer to join"),
    recv_muc_presence(Config, PeerNickJID, available),
    ok = set_affiliation(Config, none, randoms:get_string()),
    ct:comment("Waiting for peer to be kicked"),
    #muc_user{status_codes = NewCodes,
	      items = [#muc_item{affiliation = none,
				 role = none}]} =
	recv_muc_presence(Config, PeerNickJID, unavailable),
    ct:comment("Checking if code '321' (became non-member in "
	       "members-only room) is set"),
    true = lists:member(321, NewCodes),
    ok = leave(Config),
    disconnect(Config).

config_members_only_slave(Config) ->
    MyJID = my_jid(Config),
    MyNickJID = my_muc_jid(Config),
    {[], _, _} = slave_join(Config),
    [104] = recv_config_change_message(Config),
    ct:comment("Getting kicked because the room has become members-only"),
    #muc_user{status_codes = Codes,
	      items = [#muc_item{jid = MyJID,
				 role = none,
				 affiliation = none}]} =
	recv_muc_presence(Config, MyNickJID, unavailable),
    ct:comment("Checking if the code '110' (self-presence) "
	       "and '322' (non-member) is set"),
    true = lists:member(110, Codes),
    true = lists:member(322, Codes),
    ct:comment("Fail trying to join members-only room"),
    #stanza_error{reason = 'registration-required'} = join(Config),
    ct:comment("Asking the peer to set us member"),
    put_event(Config, set_member),
    ct:comment("Waiting for the peer to ask for join"),
    join = get_event(Config),
    {[], _, _} = join(Config, participant, member),
    #muc_user{status_codes = NewCodes,
	      items = [#muc_item{jid = MyJID,
				 role = none,
				 affiliation = none}]} =
	recv_muc_presence(Config, MyNickJID, unavailable),
    ct:comment("Checking if the code '110' (self-presence) "
	       "and '321' (became non-member in members-only room) is set"),
    true = lists:member(110, NewCodes),
    true = lists:member(321, NewCodes),
    disconnect(Config).

config_moderated_master(Config) ->
    Room = muc_room_jid(Config),
    PeerNickJID = peer_muc_jid(Config),
    ok = master_join(Config),
    lists:member(<<"muc_moderated">>, get_features(Config, Room)),
    ok = set_role(Config, visitor, randoms:get_string()),
    #muc_user{items = [#muc_item{role = visitor}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    set_unmoderated = get_event(Config),
    [104] = set_config(Config, [{moderatedroom, false}]),
    #message{from = PeerNickJID, type = groupchat} = recv_message(Config),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    lists:member(<<"muc_unmoderated">>, get_features(Config, Room)),
    ok = leave(Config),
    disconnect(Config).

config_moderated_slave(Config) ->
    Room = muc_room_jid(Config),
    MyNickJID = my_muc_jid(Config),
    {[], _, _} = slave_join(Config),
    #muc_user{items = [#muc_item{role = visitor}]} =
	recv_muc_presence(Config, MyNickJID, available),
    send(Config, #message{to = Room, type = groupchat}),
    ErrMsg = #message{from = Room, type = error} = recv_message(Config),
    #stanza_error{reason = 'forbidden'} = xmpp:get_error(ErrMsg),
    put_event(Config, set_unmoderated),
    [104] = recv_config_change_message(Config),
    send(Config, #message{to = Room, type = groupchat}),
    #message{from = MyNickJID, type = groupchat} = recv_message(Config),
    ok = leave(Config),
    disconnect(Config).

config_private_messages_master(Config) ->
    PeerNickJID = peer_muc_jid(Config),
    ok = master_join(Config),
    ct:comment("Waiting for a private message from the slave"),
    #message{from = PeerNickJID, type = chat} = recv_message(Config),
    ok = set_role(Config, visitor, <<>>),
    ct:comment("Waiting for the peer to become a visitor"),
    recv_muc_presence(Config, PeerNickJID, available),
    ct:comment("Waiting for a private message from the slave"),
    #message{from = PeerNickJID, type = chat} = recv_message(Config),
    [104] = set_config(Config, [{allow_private_messages_from_visitors, moderators}]),
    ct:comment("Waiting for a private message from the slave"),
    #message{from = PeerNickJID, type = chat} = recv_message(Config),
    [104] = set_config(Config, [{allow_private_messages_from_visitors, nobody}]),
    wait_for_slave(Config),
    [104] = set_config(Config, [{allow_private_messages_from_visitors, anyone},
				    {allow_private_messages, false}]),
    ct:comment("Fail trying to send a private message"),
    send(Config, #message{to = PeerNickJID, type = chat}),
    #message{from = PeerNickJID, type = error} = ErrMsg = recv_message(Config),
    #stanza_error{reason = 'forbidden'} = xmpp:get_error(ErrMsg),
    ok = set_role(Config, participant, <<>>),
    ct:comment("Waiting for the peer to become a participant"),
    recv_muc_presence(Config, PeerNickJID, available),
    ct:comment("Waiting for the peer to leave"),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_private_messages_slave(Config) ->
    MyNickJID = my_muc_jid(Config),
    PeerNickJID = peer_muc_jid(Config),
    {[], _, _} = slave_join(Config),
    ct:comment("Sending a private message"),
    send(Config, #message{to = PeerNickJID, type = chat}),
    ct:comment("Waiting to become a visitor"),
    #muc_user{items = [#muc_item{role = visitor}]} =
	recv_muc_presence(Config, MyNickJID, available),
    ct:comment("Sending a private message"),
    send(Config, #message{to = PeerNickJID, type = chat}),
    [104] = recv_config_change_message(Config),
    ct:comment("Sending a private message"),
    send(Config, #message{to = PeerNickJID, type = chat}),
    [104] = recv_config_change_message(Config),
    ct:comment("Fail trying to send a private message"),
    send(Config, #message{to = PeerNickJID, type = chat}),
    #message{from = PeerNickJID, type = error} = ErrMsg1 = recv_message(Config),
    #stanza_error{reason = 'forbidden'} = xmpp:get_error(ErrMsg1),
    wait_for_master(Config),
    [104] = recv_config_change_message(Config),
    ct:comment("Waiting to become a participant again"),
    #muc_user{items = [#muc_item{role = participant}]} =
	recv_muc_presence(Config, MyNickJID, available),
    ct:comment("Fail trying to send a private message"),
    send(Config, #message{to = PeerNickJID, type = chat}),
    #message{from = PeerNickJID, type = error} = ErrMsg2 = recv_message(Config),
    #stanza_error{reason = 'forbidden'} = xmpp:get_error(ErrMsg2),
    ok = leave(Config),
    disconnect(Config).

config_query_master(Config) ->
    PeerNickJID = peer_muc_jid(Config),
    ok = join_new(Config),
    wait_for_slave(Config),
    recv_muc_presence(Config, PeerNickJID, available),
    ct:comment("Receiving IQ query from the slave"),
    #iq{type = get, from = PeerNickJID, id = I,
	sub_els = [#ping{}]} = recv_iq(Config),
    send(Config, #iq{type = result, to = PeerNickJID, id = I}),
    [104] = set_config(Config, [{allow_query_users, false}]),
    ct:comment("Fail trying to send IQ"),
    #iq{type = error, from = PeerNickJID} = Err =
	send_recv(Config, #iq{type = get, to = PeerNickJID,
			      sub_els = [#ping{}]}),
    #stanza_error{reason = 'not-allowed'} = xmpp:get_error(Err),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_query_slave(Config) ->
    PeerNickJID = peer_muc_jid(Config),
    wait_for_master(Config),
    ct:comment("Checking if IQ queries are denied from non-occupants"),
    #iq{type = error, from = PeerNickJID} = Err1 =
	send_recv(Config, #iq{type = get, to = PeerNickJID,
			      sub_els = [#ping{}]}),
    #stanza_error{reason = 'not-acceptable'} = xmpp:get_error(Err1),
    {[], _, _} = join(Config),
    ct:comment("Sending IQ to the master"),
    #iq{type = result, from = PeerNickJID, sub_els = []} =
	send_recv(Config, #iq{to = PeerNickJID, type = get, sub_els = [#ping{}]}),
    [104] = recv_config_change_message(Config),
    ct:comment("Fail trying to send IQ"),
    #iq{type = error, from = PeerNickJID} = Err2 =
	send_recv(Config, #iq{type = get, to = PeerNickJID,
			      sub_els = [#ping{}]}),
    #stanza_error{reason = 'not-allowed'} = xmpp:get_error(Err2),
    ok = leave(Config),
    disconnect(Config).

config_allow_invites_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(peer, Config),
    PeerNickJID = peer_muc_jid(Config),
    ok = master_join(Config),
    [104] = set_config(Config, [{allowinvites, true}]),
    ct:comment("Receiving an invitation from the slave"),
    #message{from = Room, type = normal} = recv_message(Config),
    [104] = set_config(Config, [{allowinvites, false}]),
    send_invitation = get_event(Config),
    ct:comment("Sending an invitation"),
    send(Config, #message{to = Room, type = normal,
			  sub_els =
			      [#muc_user{
				  invites =
				      [#muc_invite{to = PeerJID}]}]}),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_allow_invites_slave(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(peer, Config),
    InviteMsg = #message{to = Room, type = normal,
			 sub_els =
			     [#muc_user{
				 invites =
				     [#muc_invite{to = PeerJID}]}]},
    {[], _, _} = slave_join(Config),
    [104] = recv_config_change_message(Config),
    ct:comment("Sending an invitation"),
    send(Config, InviteMsg),
    [104] = recv_config_change_message(Config),
    ct:comment("Fail sending an invitation"),
    send(Config, InviteMsg),
    #message{from = Room, type = error} = Err = recv_message(Config),
    #stanza_error{reason = 'not-allowed'} = xmpp:get_error(Err),
    ct:comment("Checking if the master is still able to send invitations"),
    put_event(Config, send_invitation),
    #message{from = Room, type = normal} = recv_message(Config),
    ok = leave(Config),
    disconnect(Config).

config_visitor_status_master(Config) ->
    PeerNickJID = peer_muc_jid(Config),
    Status = xmpp:mk_text(randoms:get_string()),
    ok = join_new(Config),
    [104] = set_config(Config, [{members_by_default, false}]),
    ct:comment("Asking the slave to join as a visitor"),
    put_event(Config, {join, Status}),
    #muc_user{items = [#muc_item{role = visitor}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    ct:comment("Receiving status change from the visitor"),
    #presence{from = PeerNickJID, status = Status} = recv_presence(Config),
    [104] = set_config(Config, [{allow_visitor_status, false}]),
    ct:comment("Receiving status change with <status/> stripped"),
    #presence{from = PeerNickJID, status = []} = recv_presence(Config),
    ct:comment("Waiting for the slave to leave"),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_visitor_status_slave(Config) ->
    Room = muc_room_jid(Config),
    MyNickJID = my_muc_jid(Config),
    ct:comment("Waiting for 'join' command from the master"),
    {join, Status} = get_event(Config),
    {[], _, _} = join(Config, visitor, none),
    ct:comment("Sending status change"),
    send(Config, #presence{to = Room, status = Status}),
    #presence{from = MyNickJID, status = Status} = recv_presence(Config),
    [104] = recv_config_change_message(Config),
    ct:comment("Sending status change again"),
    send(Config, #presence{to = Room, status = Status}),
    #presence{from = MyNickJID, status = []} = recv_presence(Config),
    ok = leave(Config),
    disconnect(Config).

config_allow_voice_requests_master(Config) ->
    PeerNickJID = peer_muc_jid(Config),
    ok = join_new(Config),
    [104] = set_config(Config, [{members_by_default, false}]),
    ct:comment("Asking the slave to join as a visitor"),
    put_event(Config, join),
    #muc_user{items = [#muc_item{role = visitor}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    [104] = set_config(Config, [{allow_voice_requests, false}]),
    ct:comment("Waiting for the slave to leave"),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_allow_voice_requests_slave(Config) ->
    Room = muc_room_jid(Config),
    ct:comment("Waiting for 'join' command from the master"),
    join = get_event(Config),
    {[], _, _} = join(Config, visitor),
    [104] = recv_config_change_message(Config),
    ct:comment("Fail sending voice request"),
    Fs = muc_request:encode([{role, participant}]),
    X = #xdata{type = submit, fields = Fs},
    send(Config, #message{to = Room, sub_els = [X]}),
    #message{from = Room, type = error} = Err = recv_message(Config),
    #stanza_error{reason = 'forbidden'} = xmpp:get_error(Err),
    ok = leave(Config),
    disconnect(Config).

config_voice_request_interval_master(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(peer, Config),
    PeerNick = ?config(peer_nick, Config),
    PeerNickJID = peer_muc_jid(Config),
    ok = join_new(Config),
    [104] = set_config(Config, [{members_by_default, false}]),
    ct:comment("Asking the slave to join as a visitor"),
    put_event(Config, join),
    #muc_user{items = [#muc_item{role = visitor}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    [104] = set_config(Config, [{voice_request_min_interval, 5}]),
    ct:comment("Receiving a voice request from slave"),
    #message{from = Room, type = normal} = recv_message(Config),
    ct:comment("Deny voice request at first"),
    Fs = muc_request:encode([{jid, PeerJID}, {role, participant},
			     {nick, PeerNick}, {request_allow, false}]),
    send(Config, #message{to = Room, sub_els = [#xdata{type = submit,
                                                       fields = Fs}]}),
    put_event(Config, denied),
    ct:comment("Waiting for repeated voice request from the slave"),
    #message{from = Room, type = normal} = recv_message(Config),
    ct:comment("Waiting for the slave to leave"),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_voice_request_interval_slave(Config) ->
    Room = muc_room_jid(Config),
    Fs = muc_request:encode([{role, participant}]),
    X = #xdata{type = submit, fields = Fs},
    ct:comment("Waiting for 'join' command from the master"),
    join = get_event(Config),
    {[], _, _} = join(Config, visitor),
    [104] = recv_config_change_message(Config),
    ct:comment("Sending voice request"),
    send(Config, #message{to = Room, sub_els = [X]}),
    ct:comment("Waiting for the master to deny our voice request"),
    denied = get_event(Config),
    ct:comment("Requesting voice again"),
    send(Config, #message{to = Room, sub_els = [X]}),
    ct:comment("Receving voice request error because we're sending to fast"),
    #message{from = Room, type = error} = Err = recv_message(Config),
    #stanza_error{reason = 'resource-constraint'} = xmpp:get_error(Err),
    ct:comment("Waiting for 5 seconds"),
    timer:sleep(timer:seconds(5)),
    ct:comment("Repeating again"),
    send(Config, #message{to = Room, sub_els = [X]}),
    ok = leave(Config),
    disconnect(Config).

config_visitor_nickchange_master(Config) ->
    PeerNickJID = peer_muc_jid(Config),
    ok = join_new(Config),
    [104] = set_config(Config, [{members_by_default, false}]),
    ct:comment("Asking the slave to join as a visitor"),
    put_event(Config, join),
    ct:comment("Waiting for the slave to join"),
    #muc_user{items = [#muc_item{role = visitor}]} =
	recv_muc_presence(Config, PeerNickJID, available),
    [104] = set_config(Config, [{allow_visitor_nickchange, false}]),
    ct:comment("Waiting for the slave to leave"),
    recv_muc_presence(Config, PeerNickJID, unavailable),
    ok = leave(Config),
    disconnect(Config).

config_visitor_nickchange_slave(Config) ->
    NewNick = randoms:get_string(),
    MyNickJID = my_muc_jid(Config),
    MyNewNickJID = jid:replace_resource(MyNickJID, NewNick),
    ct:comment("Waiting for 'join' command from the master"),
    join = get_event(Config),
    {[], _, _} = join(Config, visitor),
    [104] = recv_config_change_message(Config),
    ct:comment("Fail trying to change nickname"),
    send(Config, #presence{to = MyNewNickJID}),
    #presence{from = MyNewNickJID, type = error} = Err = recv_presence(Config),
    #stanza_error{reason = 'not-allowed'} = xmpp:get_error(Err),
    ok = leave(Config),
    disconnect(Config).

register_master(Config) ->
    MUC = muc_jid(Config),
    %% Register nick "master1"
    register_nick(Config, MUC, <<"">>, <<"master1">>),
    %% Unregister nick "master1" via jabber:register
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, to = MUC,
			      sub_els = [#register{remove = true}]}),
    %% Register nick "master2"
    register_nick(Config, MUC, <<"">>, <<"master2">>),
    %% Now register nick "master"
    register_nick(Config, MUC, <<"master2">>, <<"master">>),
    %% Wait for slave to fail trying to register nick "master"
    wait_for_slave(Config),
    wait_for_slave(Config),
    %% Now register empty ("") nick, which means we're unregistering
    register_nick(Config, MUC, <<"master">>, <<"">>),
    disconnect(Config).

register_slave(Config) ->
    MUC = muc_jid(Config),
    wait_for_master(Config),
    %% Trying to register occupied nick "master"
    Fs = muc_register:encode([{roomnick, <<"master">>}]),
    X = #xdata{type = submit, fields = Fs},
    #iq{type = error} =
	send_recv(Config, #iq{type = set, to = MUC,
			      sub_els = [#register{xdata = X}]}),
    wait_for_master(Config),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("muc_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("muc_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("muc_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("muc_" ++ atom_to_list(T) ++ "_slave")]}.

recv_muc_presence(Config, From, Type) ->
    Pres = #presence{from = From, type = Type} = recv_presence(Config),
    xmpp:get_subtag(Pres, #muc_user{}).

join_new(Config) ->
    join_new(Config, muc_room_jid(Config)).

join_new(Config, Room) ->
    MyJID = my_jid(Config),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    ct:comment("Joining new room"),
    send(Config, #presence{to = MyNickJID, sub_els = [#muc{}]}),
    %% As per XEP-0045 we MUST receive stanzas in the following order:
    %% 1. In-room presence from other occupants
    %% 2. In-room presence from the joining entity itself (so-called "self-presence")
    %% 3. Room history (if any)
    %% 4. The room subject
    %% 5. Live messages, presence updates, new user joins, etc.
    %% As this is the newly created room, we receive only the 2nd and 4th stanza.
    #muc_user{
       status_codes = Codes,
       items = [#muc_item{role = moderator,
			  jid = MyJID,
			  affiliation = owner}]} =
	recv_muc_presence(Config, MyNickJID, available),
    ct:comment("Checking if codes '110' (self-presence) and "
	       "'201' (new room) is set"),
    true = lists:member(110, Codes),
    true = lists:member(201, Codes),
    ct:comment("Receiving empty room subject"),
    #message{from = Room, type = groupchat, body = [],
	     subject = [#text{data = <<>>}]} = recv_message(Config),
    case ?config(persistent_room, Config) of
	true ->
	    [104] = set_config(Config, [{persistentroom, true}], Room),
	    ok;
	false ->
	    ok
    end.

recv_history_and_subject(Config) ->
    ct:comment("Receiving room history and/or subject"),
    recv_history_and_subject(Config, []).

recv_history_and_subject(Config, History) ->
    Room = muc_room_jid(Config),
    #message{type = groupchat, subject = Subj,
	     body = Body, thread = Thread} = Msg = recv_message(Config),
    case xmpp:get_subtag(Msg, #delay{}) of
	#delay{from = Room} ->
	    recv_history_and_subject(Config, [Msg|History]);
	false when Subj /= [], Body == [], Thread == undefined ->
	    {lists:reverse(History), Msg}
    end.

join(Config) ->
    join(Config, participant, none, #muc{}).

join(Config, Role) when is_atom(Role) ->
    join(Config, Role, none, #muc{});
join(Config, #muc{} = SubEl) ->
    join(Config, participant, none, SubEl).

join(Config, Role, Aff) when is_atom(Role), is_atom(Aff) ->
    join(Config, Role, Aff, #muc{});
join(Config, Role, #muc{} = SubEl) when is_atom(Role) ->
    join(Config, Role, none, SubEl).

join(Config, Role, Aff, SubEl) ->
    ct:comment("Joining existing room as ~s/~s", [Aff, Role]),
    MyJID = my_jid(Config),
    Room = muc_room_jid(Config),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    PeerNick = ?config(peer_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    send(Config, #presence{to = MyNickJID, sub_els = [SubEl]}),
    case recv_presence(Config) of
	#presence{type = error, from = MyNickJID} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{});
	#presence{type = available, from = PeerNickJID} = Pres ->
	    #muc_user{items = [#muc_item{role = moderator,
					 affiliation = owner}]} =
		xmpp:get_subtag(Pres, #muc_user{}),
	    ct:comment("Receiving initial self-presence"),
	    #muc_user{status_codes = Codes,
		      items = [#muc_item{role = Role,
					 jid = MyJID,
					 affiliation = Aff}]} =
		recv_muc_presence(Config, MyNickJID, available),
	    ct:comment("Checking if code '110' (self-presence) is set"),
	    true = lists:member(110, Codes),
	    {History, Subj} = recv_history_and_subject(Config),
	    {History, Subj, Codes};
	#presence{type = available, from = MyNickJID} = Pres ->
	    #muc_user{status_codes = Codes,
		      items = [#muc_item{role = Role,
					 jid = MyJID,
					 affiliation = Aff}]} =
		xmpp:get_subtag(Pres, #muc_user{}),
	    ct:comment("Checking if code '110' (self-presence) is set"),
	    true = lists:member(110, Codes),
	    {History, Subj} = recv_history_and_subject(Config),
	    {empty, History, Subj, Codes}
    end.

leave(Config) ->
    leave(Config, muc_room_jid(Config)).

leave(Config, Room) ->
    MyJID = my_jid(Config),
    MyNick = ?config(nick, Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    Mode = ?config(mode, Config),
    IsPersistent = ?config(persistent_room, Config),
    if Mode /= slave, IsPersistent ->
	    [104] = set_config(Config, [{persistentroom, false}], Room);
       true ->
	    ok
    end,
    ct:comment("Leaving the room"),
    send(Config, #presence{to = MyNickJID, type = unavailable}),
    #muc_user{
       status_codes = Codes,
       items = [#muc_item{role = none, jid = MyJID}]} =
	recv_muc_presence(Config, MyNickJID, unavailable),
    ct:comment("Checking if code '110' (self-presence) is set"),
    true = lists:member(110, Codes),
    ok.

get_config(Config) ->
    ct:comment("Get room config"),
    Room = muc_room_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = Room,
		       sub_els = [#muc_owner{}]}) of
	#iq{type = result,
	    sub_els = [#muc_owner{config = #xdata{type = form} = X}]} ->
	    muc_roomconfig:decode(X#xdata.fields);
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

set_config(Config, RoomConfig) ->
    set_config(Config, RoomConfig, muc_room_jid(Config)).

set_config(Config, RoomConfig, Room) ->
    ct:comment("Set room config: ~p", [RoomConfig]),
    Fs = case RoomConfig of
	     [] -> [];
	     _ -> muc_roomconfig:encode(RoomConfig)
	 end,
    case send_recv(Config,
		   #iq{type = set, to = Room,
		       sub_els = [#muc_owner{config = #xdata{type = submit,
							     fields = Fs}}]}) of
	#iq{type = result, sub_els = []} ->
	    #message{from = Room, type = groupchat} = Msg = recv_message(Config),
	    #muc_user{status_codes = Codes} = xmpp:get_subtag(Msg, #muc_user{}),
	    lists:sort(Codes);
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

create_persistent(Config) ->
    [_|_] = get_config(Config),
    [] = set_config(Config, [{persistentroom, true}], false),
    ok.

destroy(Config) ->
    destroy(Config, <<>>).

destroy(Config, Reason) ->
    Room = muc_room_jid(Config),
    AltRoom = alt_room_jid(Config),
    ct:comment("Destroying a room"),
    case send_recv(Config,
		   #iq{type = set, to = Room,
		       sub_els = [#muc_owner{destroy = #muc_destroy{
							  reason = Reason,
							  jid = AltRoom}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

disco_items(Config) ->
    MUC = muc_jid(Config),
    ct:comment("Performing disco#items request to ~s", [jid:encode(MUC)]),
    #iq{type = result, from = MUC, sub_els = [DiscoItems]} =
	send_recv(Config, #iq{type = get, to = MUC,
			      sub_els = [#disco_items{}]}),
    lists:keysort(#disco_item.jid, DiscoItems#disco_items.items).

disco_room_items(Config) ->
    Room = muc_room_jid(Config),
    #iq{type = result, from = Room, sub_els = [DiscoItems]} =
	send_recv(Config, #iq{type = get, to = Room,
			      sub_els = [#disco_items{}]}),
    DiscoItems#disco_items.items.

get_affiliations(Config, Aff) ->
    Room = muc_room_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = Room,
		       sub_els = [#muc_admin{items = [#muc_item{affiliation = Aff}]}]}) of
	#iq{type = result, sub_els = [#muc_admin{items = Items}]} ->
	    Items;
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

master_join(Config) ->
    Room = muc_room_jid(Config),
    PeerJID = ?config(slave, Config),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jid:replace_resource(Room, PeerNick),
    ok = join_new(Config),
    wait_for_slave(Config),
    #muc_user{items = [#muc_item{jid = PeerJID,
				 role = participant,
				 affiliation = none}]} = 
	recv_muc_presence(Config, PeerNickJID, available),
    ok.

slave_join(Config) ->
    wait_for_master(Config),
    join(Config).

set_role(Config, Role, Reason) ->
    ct:comment("Changing role to ~s", [Role]),
    Room = muc_room_jid(Config),
    PeerNick = ?config(slave_nick, Config),
    case send_recv(
	   Config,
	   #iq{type = set, to = Room,
	       sub_els =
		   [#muc_admin{
		       items = [#muc_item{role = Role,
					  reason = Reason,
					  nick = PeerNick}]}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

get_role(Config, Role) ->
    ct:comment("Requesting list for role '~s'", [Role]),
    Room = muc_room_jid(Config),
    case send_recv(
	   Config,
	   #iq{type = get, to = Room,
	       sub_els = [#muc_admin{
			     items = [#muc_item{role = Role}]}]}) of
	#iq{type = result, sub_els = [#muc_admin{items = Items}]} ->
	    lists:keysort(#muc_item.affiliation, Items);
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

set_affiliation(Config, Aff, Reason) ->
    ct:comment("Changing affiliation to ~s", [Aff]),
    Room = muc_room_jid(Config),
    PeerJID = ?config(slave, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    case send_recv(
	   Config,
	   #iq{type = set, to = Room,
	       sub_els =
		   [#muc_admin{
		       items = [#muc_item{affiliation = Aff,
					  reason = Reason,
					  jid = PeerBareJID}]}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

get_affiliation(Config, Aff) ->
    ct:comment("Requesting list for affiliation '~s'", [Aff]),
    Room = muc_room_jid(Config),
    case send_recv(
	   Config,
	   #iq{type = get, to = Room,
	       sub_els = [#muc_admin{
			     items = [#muc_item{affiliation = Aff}]}]}) of
	#iq{type = result, sub_els = [#muc_admin{items = Items}]} ->
	    Items;
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

set_vcard(Config, VCard) ->
    Room = muc_room_jid(Config),
    ct:comment("Setting vCard for ~s", [jid:encode(Room)]),
    case send_recv(Config, #iq{type = set, to = Room,
			       sub_els = [VCard]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

get_vcard(Config) ->
    Room = muc_room_jid(Config),
    ct:comment("Retreiving vCard from ~s", [jid:encode(Room)]),
    case send_recv(Config, #iq{type = get, to = Room,
			       sub_els = [#vcard_temp{}]}) of
	#iq{type = result, sub_els = [VCard]} ->
	    VCard;
	#iq{type = error} = Err ->
	    xmpp:get_subtag(Err, #stanza_error{})
    end.

recv_config_change_message(Config) ->
    ct:comment("Receiving configuration change notification message"),
    Room = muc_room_jid(Config),
    #message{type = groupchat, from = Room} = Msg = recv_message(Config),
    #muc_user{status_codes = Codes} = xmpp:get_subtag(Msg, #muc_user{}),
    lists:sort(Codes).

register_nick(Config, MUC, PrevNick, Nick) ->
    PrevRegistered = if PrevNick /= <<"">> -> true;
			true -> false
		     end,
    NewRegistered = if Nick /= <<"">> -> true;
		       true -> false
		    end,
    ct:comment("Requesting registration form"),
    #iq{type = result,
	sub_els = [#register{registered = PrevRegistered,
			     xdata = #xdata{type = form,
					    fields = FsWithoutNick}}]} =
	send_recv(Config, #iq{type = get, to = MUC,
			      sub_els = [#register{}]}),
    ct:comment("Checking if previous nick is registered"),
    PrevNick = proplists:get_value(
		 roomnick, muc_register:decode(FsWithoutNick)),
    X = #xdata{type = submit, fields = muc_register:encode([{roomnick, Nick}])},
    ct:comment("Submitting registration form"),
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, to = MUC,
			      sub_els = [#register{xdata = X}]}),
    ct:comment("Checking if new nick was registered"),
    #iq{type = result,
	sub_els = [#register{registered = NewRegistered,
			     xdata = #xdata{type = form,
					    fields = FsWithNick}}]} =
	send_recv(Config, #iq{type = get, to = MUC,
			      sub_els = [#register{}]}),
    Nick = proplists:get_value(
	     roomnick, muc_register:decode(FsWithNick)).

subscribe(Config, Events, Room) ->
    MyNick = ?config(nick, Config),
    case send_recv(Config,
		   #iq{type = set, to = Room,
		       sub_els = [#muc_subscribe{nick = MyNick,
						 events = Events}]}) of
	#iq{type = result, sub_els = [#muc_subscribe{events = ResEvents}]} ->
	    lists:sort(ResEvents);
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

unsubscribe(Config, Room) ->
    case send_recv(Config, #iq{type = set, to = Room,
			       sub_els = [#muc_unsubscribe{}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.
