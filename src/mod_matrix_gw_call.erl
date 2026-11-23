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
%%% forwarded at all (1:1 calls only). Outbound, mod_matrix_gw_room's
%%% route/1 hands every message to route_event/3 first: one from a local
%%% user to u%srv@G carrying an m.call.* <event/> becomes the PDU {type,
%%% content, sender} of the direct room with that user, the room created
%%% first as the first text message creates it; any other <event/> type is
%%% refused (not-acceptable), content that is not a JSON object too
%%% (bad-request). The gateway and its user JIDs advertise
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

-export([forward/3, forward_message/3, route_event/3, pdu/3,
         disco_features/1]).

-include_lib("xmpp/include/xmpp.hrl").

-define(NS_MATRIX_E2E, <<"https://ns.another.im/matrix-e2e/0">>).
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
            Attrs = [{<<"xmlns">>, ?NS_MATRIX_E2E}, {<<"type">>, Type},
                     {<<"id">>, EventID}, {<<"room">>, RoomID},
                     {<<"sender">>, Sender}] ++
                [{<<"ts">>, integer_to_binary(TS)} || is_integer(TS)],
            Event = #xmlel{name = <<"event">>, attrs = Attrs,
                           children = [{xmlcdata, iolist_to_binary(misc:json_encode(Content))}]},
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
%% XMPP -> Matrix
%%--------------------------------------------------------------------
%% `pass` when the message is not this module's (no <event/> of the
%% namespace, an error, a room's), so the gateway's own clauses take it;
%% `ok` when it was handled, refused or ignored. ToMatrixID is the
%% recipient's Matrix user id, or error.
-spec route_event(message(), binary(), {ok, binary()} | error) -> pass | ok.
route_event(#message{type = error}, _Host, _ToMatrixID) ->
    pass;
route_event(#message{from = From, to = To, lang = Lang} = Pkt, Host, ToMatrixID) ->
    case {event_child(Pkt), To#jid.luser} of
        {false, _} ->
            pass;
        {_, <<C, _/binary>>} when C == $!; C == $# ->
            pass;
        {_, _} when From#jid.lserver /= Host ->
            ok;
        {_, _} when ToMatrixID == error ->
            ok;
        {#xmlel{} = El, _} ->
            {ok, ToID} = ToMatrixID,
            Type = fxml:get_attr_s(<<"type">>, El#xmlel.attrs),
            case Type of
                <<"m.call.", _/binary>> ->
                    case content(El) of
                        {ok, Content} ->
                            MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
                            Sender = <<$@, (From#jid.luser)/binary, $:, MatrixServer/binary>>,
                            mod_matrix_gw_room:send_direct(Host, From, ToID,
                                                           pdu(Type, Content, Sender));
                        error ->
                            Txt = <<"the event's content is not a JSON object">>,
                            ejabberd_router:route_error(Pkt, xmpp:err_bad_request(Txt, Lang))
                    end;
                _ ->
                    Txt = <<"only m.call.* events cross this gateway">>,
                    ejabberd_router:route_error(Pkt, xmpp:err_not_acceptable(Txt, Lang))
            end,
            ok
    end.

%% The room event the gateway adds for an <event/>: its type, the JSON as
%% the content, the local user as the sender.
-spec pdu(binary(), map(), binary()) -> map().
pdu(Type, Content, Sender) ->
    #{<<"type">> => Type, <<"content">> => Content, <<"sender">> => Sender}.

event_child(#message{sub_els = Els}) ->
    case [El || #xmlel{name = <<"event">>} = El <- Els,
                fxml:get_attr_s(<<"xmlns">>, El#xmlel.attrs) == ?NS_MATRIX_E2E] of
        [El | _] -> El;
        [] -> false
    end.

content(El) ->
    try misc:json_decode(fxml:get_tag_cdata(El)) of
        #{} = Content -> {ok, Content};
        _ -> error
    catch _:_ ->
            error
    end.

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
