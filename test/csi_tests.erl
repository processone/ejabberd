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

-module(csi_tests).

%% API
-compile(export_all).
-import(suite, [disconnect/1, wait_for_slave/1, wait_for_master/1,
		send/2, send_recv/2, recv_presence/1, recv_message/1,
		server_jid/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {csi_single, [sequence],
     [single_test(feature_enabled)]}.

feature_enabled(Config) ->
    true = ?config(csi, Config),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {csi_master_slave, [sequence],
     [master_slave_test(all)]}.

all_master(Config) ->
    Peer = ?config(peer, Config),
    Presence = #presence{to = Peer},
    ChatState = #message{to = Peer, thread = <<"1">>,
			 sub_els = [#chatstate{type = active}]},
    Message = ChatState#message{body = [#text{data = <<"body">>}]},
    PepPayload = xmpp:encode(#presence{}),
    PepOne = #message{
		to = Peer,
		sub_els =
		    [#ps_event{
			items =
			    #ps_items{
			       node = <<"foo-1">>,
			       items =
				   [#ps_item{
				       id = <<"pep-1">>,
				       xml_els = [PepPayload]}]}}]},
    PepTwo = #message{
		to = Peer,
		sub_els =
		    [#ps_event{
			items =
			    #ps_items{
			       node = <<"foo-2">>,
			       items =
				   [#ps_item{
				       id = <<"pep-2">>,
				       xml_els = [PepPayload]}]}}]},
    %% Wait for the slave to become inactive.
    wait_for_slave(Config),
    %% Should be queued (but see below):
    send(Config, Presence),
    %% Should replace the previous presence in the queue:
    send(Config, Presence#presence{type = unavailable}),
    %% The following two PEP stanzas should be queued (but see below):
    send(Config, PepOne),
    send(Config, PepTwo),
    %% The following two PEP stanzas should replace the previous two:
    send(Config, PepOne),
    send(Config, PepTwo),
    %% Should be queued (but see below):
    send(Config, ChatState),
    %% Should replace the previous chat state in the queue:
    send(Config, ChatState#message{sub_els = [#chatstate{type = composing}]}),
    %% Should be sent immediately, together with the queued stanzas:
    send(Config, Message),
    %% Wait for the slave to become active.
    wait_for_slave(Config),
    %% Should be delivered, as the client is active again:
    send(Config, ChatState),
    disconnect(Config).

all_slave(Config) ->
    Peer = ?config(peer, Config),
    change_client_state(Config, inactive),
    wait_for_master(Config),
    #presence{from = Peer, type = unavailable, sub_els = [#delay{}]} =
	recv_presence(Config),
    #message{
       from = Peer,
       sub_els =
	   [#ps_event{
	       items =
		   #ps_items{
		      node = <<"foo-1">>,
		      items =
			  [#ps_item{
			      id = <<"pep-1">>}]}},
	    #delay{}]} = recv_message(Config),
    #message{
       from = Peer,
       sub_els =
	   [#ps_event{
	       items =
		   #ps_items{
		      node = <<"foo-2">>,
		      items =
			  [#ps_item{
			      id = <<"pep-2">>}]}},
	    #delay{}]} = recv_message(Config),
    #message{from = Peer, thread = <<"1">>,
	     sub_els = [#chatstate{type = composing},
			#delay{}]} = recv_message(Config),
    #message{from = Peer, thread = <<"1">>,
	     body = [#text{data = <<"body">>}],
	     sub_els = [#chatstate{type = active}]} = recv_message(Config),
    change_client_state(Config, active),
    wait_for_master(Config),
    #message{from = Peer, thread = <<"1">>,
	     sub_els = [#chatstate{type = active}]} = recv_message(Config),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("csi_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("csi_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("csi_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("csi_" ++ atom_to_list(T) ++ "_slave")]}.

change_client_state(Config, NewState) ->
    send(Config, #csi{type = NewState}),
    send_recv(Config, #iq{type = get, to = server_jid(Config),
			  sub_els = [#ping{}]}).
