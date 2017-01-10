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

-module(mix_tests).

%% API
-compile(export_all).
-import(suite, [mix_jid/1, mix_room_jid/1, my_jid/1, is_feature_advertised/3,
		disconnect/1, send_recv/2, recv_message/1, send/2,
		put_event/2, get_event/1]).
-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {mix_single, [sequence],
     [single_test(feature_enabled)]}.

feature_enabled(Config) ->
    MIX = mix_jid(Config),
    ct:comment("Checking if ~s is set", [?NS_MIX_0]),
    true = is_feature_advertised(Config, ?NS_MIX_0, MIX),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {mix_master_slave, [sequence],
     [master_slave_test(all)]}.

all_master(Config) ->
    MIX = mix_jid(Config),
    Room = mix_room_jid(Config),
    MyJID = my_jid(Config),
    MyBareJID = jid:remove_resource(MyJID),
    #iq{type = result,
	sub_els =
	    [#disco_info{
		identities = [#identity{category = <<"conference">>,
					type = <<"text">>}],
		xdata = [#xdata{type = result, fields = XFields}]}]} =
	send_recv(Config, #iq{type = get, to = MIX, sub_els = [#disco_info{}]}),
    true = lists:any(
	     fun(#xdata_field{var = <<"FORM_TYPE">>,
			      values = [?NS_MIX_SERVICEINFO_0]}) -> true;
		(_) -> false
	     end, XFields),
    %% Joining
    Nodes = [?NS_MIX_NODES_MESSAGES, ?NS_MIX_NODES_PRESENCE,
	     ?NS_MIX_NODES_PARTICIPANTS, ?NS_MIX_NODES_SUBJECT,
	     ?NS_MIX_NODES_CONFIG],
    #iq{type = result,
	sub_els = [#mix_join{subscribe = Nodes, jid = MyBareJID}]} =
	send_recv(Config, #iq{type = set, to = Room,
			      sub_els = [#mix_join{subscribe = Nodes}]}),
    #message{from = Room,
	     sub_els =
		 [#ps_event{
		     items = #ps_items{
				node = ?NS_MIX_NODES_PARTICIPANTS,
				items = [#ps_item{
					    id = ParticipantID,
					    xml_els = [PXML]}]}}]} =
	recv_message(Config),
    #mix_participant{jid = MyBareJID} = xmpp:decode(PXML),
    %% Coming online
    PresenceID = randoms:get_string(),
    Presence = xmpp:encode(#presence{}),
    #iq{type = result,
	sub_els =
	    [#pubsub{
		publish = #ps_publish{
			     node = ?NS_MIX_NODES_PRESENCE,
			     items = [#ps_item{id = PresenceID}]}}]} =
	send_recv(
	  Config,
	  #iq{type = set, to = Room,
	      sub_els =
		  [#pubsub{
		      publish = #ps_publish{
				   node = ?NS_MIX_NODES_PRESENCE,
				   items = [#ps_item{
					       id = PresenceID,
					       xml_els = [Presence]}]}}]}),
    #message{from = Room,
	     sub_els =
		 [#ps_event{
		     items = #ps_items{
				node = ?NS_MIX_NODES_PRESENCE,
				items = [#ps_item{
					    id = PresenceID,
					    xml_els = [Presence]}]}}]} =
	recv_message(Config),
    %% Coming offline
    send(Config, #presence{type = unavailable, to = Room}),
    %% Receiving presence retract event
    #message{from = Room,
	     sub_els = [#ps_event{
			   items = #ps_items{
				      node = ?NS_MIX_NODES_PRESENCE,
				      retract = PresenceID}}]} =
	recv_message(Config),
    %% Leaving
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, to = Room, sub_els = [#mix_leave{}]}),
    #message{from = Room,
	     sub_els =
		 [#ps_event{
		     items = #ps_items{
				node = ?NS_MIX_NODES_PARTICIPANTS,
				retract = ParticipantID}}]} =
	recv_message(Config),
    put_event(Config, disconnect),
    disconnect(Config).

all_slave(Config) ->
    disconnect = get_event(Config),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("mix_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("mix_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("mix_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("mix_" ++ atom_to_list(T) ++ "_slave")]}.
