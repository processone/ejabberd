%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_call.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : Matrix 1:1 calls (the VoIP module's m.call.* events)
%%% Created : 25 Oct 2026 by Paavel A. <pd@narayana.im>
%%%
%%% The gateway's Matrix 1:1 calls: the Matrix VoIP module's m.call.* room
%%% events cross the gateway both ways as the https://ns.another.im/matrix-e2e/0
%%% <event/> of a direct chat, opaque JSON the device's call driver reads
%%% and writes.
%%%
%%% Inbound, mod_matrix_gw_room's notify_event_xmpp/2 hands every m.call.*
%%% event of a direct room to forward/3, which routes it to the local user:
%%%
%%%   <message type='chat' from='bob%example.org@matrix.example.com'
%%%            to='ann@example.com'>
%%%     <event xmlns='https://ns.another.im/matrix-e2e/0' type='m.call.invite' id='$ev'
%%%            room='!r:example.org' sender='@bob:example.org'
%%%            ts='1761390000000'>{...the event's content...}</event>
%%%     <x xmlns='p1:matrix' room_id='!r:example.org'/>
%%%   </message>
%%%
%%% The user's own events are not echoed (the m.text rule); a room's are not
%%% forwarded at all (1:1 calls only). Outbound, an m.call.* <event/> is a
%%% room event like any other the device sends (mod_matrix_gw_event: the
%%% event's type and content into the direct room, made first when there is
%%% none, and <sent/> back). The gateway and its user JIDs advertise
%%% https://ns.another.im/matrix-e2e/0#calls (disco_features/1).
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_matrix_gw_call).

-export([forward/3, forward_message/3, disco_features/1]).

-include_lib("xmpp/include/xmpp.hrl").

-define(NS_MATRIX_CALLS, <<"https://ns.another.im/matrix-e2e/0#calls">>).

-type fields() :: #{type := binary(), id := binary(), room := binary(),
                    sender := binary(), ts := integer() | undefined,
                    content := term()}.

%%--------------------------------------------------------------------
%% Matrix -> XMPP
%%--------------------------------------------------------------------
%% An m.call.* event of a direct room: SenderJID is the event's sender as a
%% JID (error when its user id is not one), UserJID the room's local user.
-spec forward(jid() | error, jid(), fields()) -> ok.
forward(SenderJID, UserJID, Fields) ->
    case forward_message(SenderJID, UserJID, Fields) of
        #message{} = Msg -> ejabberd_router:route(Msg);
        ignore -> ok
    end.

-spec forward_message(jid() | error, jid(), fields()) -> message() | ignore.
forward_message(#jid{} = SenderJID, #jid{} = UserJID,
                #{type := Type, id := EventID, room := RoomID, sender := Sender,
                  ts := TS, content := Content}) ->
    case bare(SenderJID) == bare(UserJID) of
        true ->
            ignore;
        false ->
            Event = mod_matrix_gw_event:event_el(
                      #{type => Type, id => EventID, room => RoomID, sender => Sender, ts => TS},
                      Content),
            X = #xmlel{name = <<"x">>,
                       attrs = [{<<"xmlns">>, <<"p1:matrix">>}, {<<"room_id">>, RoomID}]},
            #message{from = SenderJID, to = UserJID, type = chat, sub_els = [Event, X]}
    end;
forward_message(_, _, _) ->
    ignore.

bare(JID) ->
    {U, S, _} = jid:tolower(JID),
    {U, S}.

%%--------------------------------------------------------------------
%% Discovery
%%--------------------------------------------------------------------
%% The features this module adds to a disco#info answer of the gateway
%% (no user part) or of a Matrix user's JID there; none for a room's.
-spec disco_features(jid()) -> [binary()].
disco_features(#jid{luser = <<C, _/binary>>}) when C == $!; C == $# ->
    [];
disco_features(#jid{}) ->
    [?NS_MATRIX_CALLS].
