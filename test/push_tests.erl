%%%-------------------------------------------------------------------
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Created : 15 Jul 2017 by Holger Weiss <holger@zedat.fu-berlin.de>
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

-module(push_tests).

%% API
-compile(export_all).
-import(suite, [close_socket/1, connect/1, disconnect/1, get_event/1,
		get_features/2, make_iq_result/1, my_jid/1, put_event/2, recv/1,
		recv_iq/1, recv_message/1, self_presence/2, send/2, send_recv/2,
		server_jid/1]).

-include("suite.hrl").

-define(PUSH_NODE, <<"d3v1c3">>).
-define(PUSH_XDATA_FIELDS,
	[#xdata_field{var = <<"FORM_TYPE">>,
		      values = [?NS_PUBSUB_PUBLISH_OPTIONS]},
	 #xdata_field{var = <<"secret">>,
		      values = [<<"c0nf1d3nt14l">>]}]).

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {push_single, [sequence],
     [single_test(feature_enabled),
      single_test(unsupported_iq)]}.

feature_enabled(Config) ->
    BareMyJID = jid:remove_resource(my_jid(Config)),
    Features = get_features(Config, BareMyJID),
    true = lists:member(?NS_PUSH_0, Features),
    disconnect(Config).

unsupported_iq(Config) ->
    PushJID = my_jid(Config),
    lists:foreach(
      fun(SubEl) ->
	      #iq{type = error} =
		  send_recv(Config, #iq{type = get, sub_els = [SubEl]})
      end, [#push_enable{jid = PushJID}, #push_disable{jid = PushJID}]),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {push_master_slave, [sequence],
     [master_slave_test(sm),
      master_slave_test(offline),
      master_slave_test(mam)]}.

sm_master(Config) ->
    ct:comment("Waiting for the slave to close the socket"),
    peer_down = get_event(Config),
    ct:comment("Waiting a bit in order to test the keepalive feature"),
    ct:sleep(5000), % Without mod_push_keepalive, the session would time out.
    ct:comment("Sending message to the slave"),
    send_test_message(Config),
    ct:comment("Handling push notification"),
    handle_notification(Config),
    ct:comment("Receiving bounced message from the slave"),
    #message{type = error} = recv_message(Config),
    ct:comment("Closing the connection"),
    disconnect(Config).

sm_slave(Config) ->
    ct:comment("Enabling push notifications"),
    ok = enable_push(Config),
    ct:comment("Enabling stream management"),
    ok = enable_sm(Config),
    ct:comment("Closing the socket"),
    close_socket(Config).

offline_master(Config) ->
    ct:comment("Waiting for the slave to be ready"),
    ready = get_event(Config),
    ct:comment("Sending message to the slave"),
    send_test_message(Config), % No push notification, slave is online.
    ct:comment("Waiting for the slave to disconnect"),
    peer_down = get_event(Config),
    ct:comment("Sending message to offline storage"),
    send_test_message(Config),
    ct:comment("Handling push notification for offline message"),
    handle_notification(Config),
    ct:comment("Closing the connection"),
    disconnect(Config).

offline_slave(Config) ->
    ct:comment("Re-enabling push notifications"),
    ok = enable_push(Config),
    ct:comment("Letting the master know that we're ready"),
    put_event(Config, ready),
    ct:comment("Receiving message from the master"),
    recv_test_message(Config),
    ct:comment("Closing the connection"),
    disconnect(Config).

mam_master(Config) ->
    ct:comment("Waiting for the slave to be ready"),
    ready = get_event(Config),
    ct:comment("Sending message to the slave"),
    send_test_message(Config),
    ct:comment("Handling push notification for MAM message"),
    handle_notification(Config),
    ct:comment("Closing the connection"),
    disconnect(Config).

mam_slave(Config) ->
    self_presence(Config, available),
    ct:comment("Receiving message from offline storage"),
    recv_test_message(Config),
    %% Don't re-enable push notifications, otherwise the notification would be
    %% suppressed while the slave is online.
    ct:comment("Enabling MAM"),
    ok = enable_mam(Config),
    ct:comment("Letting the master know that we're ready"),
    put_event(Config, ready),
    ct:comment("Receiving message from the master"),
    recv_test_message(Config),
    ct:comment("Waiting for the master to disconnect"),
    peer_down = get_event(Config),
    ct:comment("Disabling push notifications"),
    ok = disable_push(Config),
    ct:comment("Closing the connection and cleaning up"),
    clean(disconnect(Config)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("push_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("push_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("push_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("push_" ++ atom_to_list(T) ++ "_slave")]}.

enable_sm(Config) ->
    send(Config, #sm_enable{xmlns = ?NS_STREAM_MGMT_3, resume = true}),
    case recv(Config) of
	#sm_enabled{resume = true} ->
	    ok;
	#sm_failed{reason = Reason} ->
	    Reason
    end.

enable_mam(Config) ->
    case send_recv(
	   Config, #iq{type = set, sub_els = [#mam_prefs{xmlns = ?NS_MAM_1,
							 default = always}]}) of
	#iq{type = result} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

enable_push(Config) ->
    %% Usually, the push JID would be a server JID (such as push.example.com).
    %% We specify the peer's full user JID instead, so the push notifications
    %% will be sent to the peer.
    PushJID = ?config(peer, Config),
    XData = #xdata{type = submit, fields = ?PUSH_XDATA_FIELDS},
    case send_recv(
	   Config, #iq{type = set,
		       sub_els = [#push_enable{jid = PushJID,
					       node = ?PUSH_NODE,
					       xdata = XData}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

disable_push(Config) ->
    PushJID = ?config(peer, Config),
    case send_recv(
	   Config, #iq{type = set,
		       sub_els = [#push_disable{jid = PushJID,
						node = ?PUSH_NODE}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

send_test_message(Config) ->
    Peer = ?config(peer, Config),
    Msg = #message{to = Peer, body = [#text{data = <<"test">>}]},
    send(Config, Msg).

recv_test_message(Config) ->
    Peer = ?config(peer, Config),
    #message{from = Peer,
	     body = [#text{data = <<"test">>}]} = recv_message(Config).

handle_notification(Config) ->
    From = server_jid(Config),
    Item = #ps_item{sub_els = [xmpp:encode(#push_notification{})]},
    Publish = #ps_publish{node = ?PUSH_NODE, items = [Item]},
    XData = #xdata{type = submit, fields = ?PUSH_XDATA_FIELDS},
    PubSub = #pubsub{publish = Publish, publish_options = XData},
    IQ = #iq{type = set, from = From, sub_els = [PubSub]} = recv_iq(Config),
    send(Config, make_iq_result(IQ)).

clean(Config) ->
    {U, S, _} = jid:tolower(my_jid(Config)),
    mod_push:remove_user(U, S),
    mod_mam:remove_user(U, S),
    Config.
