%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_event.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : Room events on the end-to-end wire, both ways
%%% Created : 2 Nov 2026 by Paavel A. <pd@narayana.im>
%%%
%%% The https://ns.another.im/matrix-e2e/0 room events and room iqs, as
%%% functions of their input, which the room module
%%% (mod_matrix_gw_room) calls from its few hooks.
%%%
%%% XMPP -> Matrix: a <message> from a local user carrying
%%%
%%%   <event xmlns='https://ns.another.im/matrix-e2e/0' type='T'>JSON content</event>
%%%
%%% to u%srv@G (chat) or a room JID (groupchat) is the room event of type T
%%% with that content, whatever T is (m.room.encrypted, m.call.*,
%%% m.reaction, m.key.verification.*, an m.room.message with an
%%% EncryptedFile); a state type (m.room.* but message and encrypted) is
%%% refused, those have their own iqs. The sender's full JID gets
%%%
%%%   <message from='G'><sent xmlns='https://ns.another.im/matrix-e2e/0' id='XMPP-ID'
%%%     event='$id' room='!r:srv'/><no-store xmlns='urn:xmpp:hints'/></message>
%%%
%%% once the event is stored, or an error. A plain body (or file, edit,
%%% reaction) into a room whose state has m.room.encryption is refused
%%% not-acceptable + <not-encrypted/>, and so is an <event/> of another type
%%% than m.room.encrypted there.
%%%
%%% Matrix -> XMPP, the forwarded set (forwarded/3): m.room.encrypted
%%% (with EME and the body "Encrypted message (Matrix)"), m.room.redaction
%%% (its `redacts` an attribute, beside an XEP-0424 retraction for the
%%% clients that read no <event/>), m.room.encryption, m.room.member in a
%%% direct chat (its state_key put into the JSON, where the device reads
%%% it), m.room.message of msgtype m.key.verification.request and
%%% m.key.verification.*: each an <event type id room sender ts>JSON</event>.
%%% A plain m.text keeps the gateway's <body> and gains an empty <event/> so
%%% a redaction or an edit can name it (plain_event_el/4).
%%%
%%% Each event a local user sends with an XMPP id is kept in matrix_sent
%%% {{user, xmpp_id}, event_id, room} (at most ?MAX_SENT per user, the
%%% oldest dropped first): <event-id id/> on G answers from it.
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
-module(mod_matrix_gw_event).

-export([create_db/0,
         parse_message/2, acceptable_type/1, content_action/1,
         forwarded/3, event_el/2, plain_event_el/4, forward_message/4,
         retraction_els/3, encryption_content/0, encryption_event/1,
         sent_message/5, sent_el/1, room_info_el/1, room_iq/1,
         not_encrypted/1, not_encrypted_message/4, too_large/1,
         record_sent/4, lookup_sent/2, sent_event/1, route_iq/1,
         disco_features/1, membership/4]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

-define(NS_E2E, <<"https://ns.another.im/matrix-e2e/0">>).
-define(NS_EME, <<"urn:xmpp:eme:0">>).
-define(NS_RETRACT, <<"urn:xmpp:message-retract:1">>).
-define(NS_MODERATE, <<"urn:xmpp:message-moderate:1">>).
-define(NS_FALLBACK, <<"urn:xmpp:fallback:0">>).
-define(MEGOLM, <<"m.megolm.v1.aes-sha2">>).
-define(ROTATION_MS, 604800000).
-define(ROTATION_MSGS, 100).
-define(ENCRYPTED_BODY, <<"Encrypted message (Matrix)">>).
-define(RETRACTED_BODY, <<"Message retracted">>).
-define(MAX_SENT, 10000).

%% {{LUser, LServer}, XmppID} -> the event a local user sent with that id
-record(matrix_sent, {key :: {{binary(), binary()}, binary()},
                      event_id :: binary(),
                      room :: binary(),
                      seq :: non_neg_integer()}).
%% the order of a user's rows, for the bound
-record(matrix_sent_log, {key :: {{binary(), binary()}, non_neg_integer()},
                          xmpp_id :: binary()}).
%% the first and the next seq of a user's rows
-record(matrix_sent_seq, {user :: {binary(), binary()},
                          first = 0 :: non_neg_integer(),
                          next = 0 :: non_neg_integer()}).

-type fields() :: #{type := binary(), id := binary(), room := binary(),
                    sender := binary(), ts := integer() | undefined,
                    content => term(), redacts => binary(),
                    state_key => binary()}.

create_db() ->
    %% indexed by event id too, for the XMPP id
    %% and the sender of an event the room no longer holds
    ejabberd_mnesia:create(?MODULE, matrix_sent,
                           [{disc_copies, [node()]}, {type, set},
                            {index, [event_id]},
                            {attributes, record_info(fields, matrix_sent)}]),
    ejabberd_mnesia:create(?MODULE, matrix_sent_log,
                           [{disc_copies, [node()]}, {type, ordered_set},
                            {attributes, record_info(fields, matrix_sent_log)}]),
    ejabberd_mnesia:create(?MODULE, matrix_sent_seq,
                           [{disc_copies, [node()]}, {type, set},
                            {attributes, record_info(fields, matrix_sent_seq)}]),
    ok.

%%--------------------------------------------------------------------
%% XMPP -> Matrix
%%--------------------------------------------------------------------

%% What a message routed to the gateway is to the end-to-end path:
%%   pass                    no <event/> of the namespace (or an error):
%%                           the gateway's own clauses take it
%%   ignore                  one, from another server: dropped, as
%%                           26.09 drops those
%%   {error, stanza_error()} one that cannot be sent
%%   {event, Type, Content}  the room event to add
-spec parse_message(message(), binary()) ->
          pass | ignore | {error, stanza_error()} | {event, binary(), map()}.
parse_message(#message{type = error}, _Host) ->
    pass;
parse_message(#message{from = From, lang = Lang} = Pkt, Host) ->
    case event_child(Pkt) of
        false ->
            pass;
        _ when From#jid.lserver /= Host ->
            ignore;
        El ->
            Type = fxml:get_attr_s(<<"type">>, El#xmlel.attrs),
            case acceptable_type(Type) of
                false ->
                    Txt = <<"this event type does not cross the gateway as an <event/>">>,
                    {error, xmpp:err_not_acceptable(Txt, Lang)};
                true ->
                    case content(El) of
                        {ok, Content} ->
                            {event, Type, Content};
                        error ->
                            Txt = <<"the event's content is not a JSON object">>,
                            {error, xmpp:err_bad_request(Txt, Lang)}
                    end
            end
    end.

%% Every message event type is the device's to send; the room's state
%% (membership, power, encryption, ...) and redactions have their own paths
%% (MUC, <encryption/>, <redact/>).
-spec acceptable_type(binary()) -> boolean().
acceptable_type(<<"m.room.message">>) -> true;
acceptable_type(<<"m.room.encrypted">>) -> true;
acceptable_type(<<"m.room.", _/binary>>) -> false;
acceptable_type(<<>>) -> false;
acceptable_type(Type) when is_binary(Type) -> byte_size(Type) =< 255;
acceptable_type(_) -> false.

%% Whether an action of mod_matrix_gw_xmpp:classify/1 puts content into the
%% room (refused in an encrypted room) rather than an EDU (typing, a
%% receipt), which an encrypted room has too.
-spec content_action(tuple()) -> boolean().
content_action(Action) ->
    lists:member(element(1, Action), [text, media, edit, reactions]).

event_child(#message{sub_els = Els}) ->
    case [El || #xmlel{name = <<"event">>} = El <- Els,
                fxml:get_attr_s(<<"xmlns">>, El#xmlel.attrs) == ?NS_E2E] of
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

%% The content of the m.room.encryption state event the gateway adds: the
%% spec's defaults for the rotation.
-spec encryption_content() -> map().
encryption_content() ->
    #{<<"algorithm">> => ?MEGOLM,
      <<"rotation_period_ms">> => ?ROTATION_MS,
      <<"rotation_period_msgs">> => ?ROTATION_MSGS}.

-spec encryption_event(binary()) -> map().
encryption_event(Sender) ->
    #{<<"type">> => <<"m.room.encryption">>, <<"state_key">> => <<>>,
      <<"sender">> => Sender, <<"content">> => encryption_content()}.

%% The answer to the sender once her event is in the room: from the
%% gateway's own JID (the device takes <sent/> from there only).
-spec sent_message(jid(), jid(), binary(), binary(), binary()) -> message().
sent_message(Service, To, XmppID, EventID, RoomID) ->
    #message{id = p1_rand:get_string(), from = Service, to = To,
             sub_els = [#xmlel{name = <<"sent">>,
                               attrs = [{<<"xmlns">>, ?NS_E2E}, {<<"id">>, XmppID},
                                        {<<"event">>, EventID}, {<<"room">>, RoomID}]},
                        mod_matrix_gw_xmpp:no_store_el()]}.

-spec sent_el(binary()) -> xmlel().
sent_el(EventID) ->
    #xmlel{name = <<"sent">>, attrs = [{<<"xmlns">>, ?NS_E2E}, {<<"event">>, EventID}]}.

-spec not_encrypted(binary()) -> stanza_error().
not_encrypted(Lang) ->
    Err = xmpp:err_not_acceptable(<<"the room is encrypted">>, Lang),
    Err#stanza_error{sub_els = [#xmlel{name = <<"not-encrypted">>,
                                       attrs = [{<<"xmlns">>, ?NS_E2E}]}]}.

%% The error an event (or a message) too large for a PDU comes back as
%% : not-acceptable with the namespace's <too-large/>, as a
%% <to-device/> over 64 KiB.
-spec too_large(binary()) -> stanza_error().
too_large(Lang) ->
    Err = xmpp:err_not_acceptable(<<"Over the 64 KiB a Matrix event may have">>, Lang),
    Err#stanza_error{sub_els = [#xmlel{name = <<"too-large">>,
                                       attrs = [{<<"xmlns">>, ?NS_E2E}]}]}.

%% The error a plain message into an encrypted room comes back as, from the
%% chat it was sent to.
-spec not_encrypted_message(jid(), jid(), binary(), binary()) -> message().
not_encrypted_message(Chat, To, MsgID, Lang) ->
    #message{id = MsgID, type = error, from = Chat, to = To, lang = Lang,
             sub_els = [not_encrypted(Lang)]}.

%%--------------------------------------------------------------------
%% Matrix -> XMPP
%%--------------------------------------------------------------------

%% What the room does with an event of Type and Content in a room of Kind
%% (direct or multi), before the gateway's own mapping:
%%   forward    the <event/> of the forwarded set
%%   redaction  the same, with the retraction beside it
%%   pass       the gateway's own mapping (text, reactions, files, calls,
%%              MUC presence) or nothing
-spec forwarded(binary(), map(), direct | multi) -> forward | redaction | pass.
forwarded(<<"m.room.encrypted">>, _, _) -> forward;
forwarded(<<"m.room.redaction">>, _, _) -> redaction;
forwarded(<<"m.room.encryption">>, _, _) -> forward;
forwarded(<<"m.room.member">>, _, direct) -> forward;
forwarded(<<"m.room.message">>, #{<<"msgtype">> := <<"m.key.verification.request">>}, _) ->
    forward;
forwarded(<<"m.key.verification.", _/binary>>, _, _) -> forward;
forwarded(_, _, _) -> pass.

%% The <event/> element: attributes from Fields, the content as its JSON
%% (none for the empty one of a plain message).
-spec event_el(fields(), map() | none) -> xmlel().
event_el(#{type := Type, id := EventID, room := RoomID, sender := Sender} = F, Content) ->
    Attrs = [{<<"xmlns">>, ?NS_E2E}, {<<"type">>, Type}, {<<"id">>, EventID},
             {<<"room">>, RoomID}, {<<"sender">>, Sender}] ++
        [{<<"ts">>, integer_to_binary(TS)} || TS <- [maps:get(ts, F, undefined)],
                                              is_integer(TS)] ++
        [{<<"redacts">>, R} || R <- [maps:get(redacts, F, <<>>)], R /= <<>>] ++
        [{<<"state_key">>, K} || K <- [maps:get(state_key, F, undefined)], is_binary(K)],
    Children = case Content of
                   none -> [];
                   _ -> [{xmlcdata, iolist_to_binary(misc:json_encode(Content))}]
               end,
    #xmlel{name = <<"event">>, attrs = Attrs, children = Children}.

%% The empty <event/> a plain message's stanza carries.
-spec plain_event_el(binary(), binary(), binary(), integer()) -> xmlel().
plain_event_el(EventID, RoomID, Sender, TS) ->
    event_el(#{type => <<"m.room.message">>, id => EventID, room => RoomID,
               sender => Sender, ts => TS}, none).

%% The sub-elements of the stanza a forwarded event of Fields is, between
%% the stanza's own (the caller's: from, to, type, id, the MUC stanza-id)
%% and <x xmlns='p1:matrix'/>. Opts: retract => the XMPP id of the redacted
%% message (a redaction of a message, not of a reaction), occupant_els =>
%% the redactor's XEP-0421 <occupant-id/> in a room (put in XEP-0425's
%% <moderated/>); By the moderator's JID when the redactor is not the
%% target's sender.
-spec forward_message(message(), fields(), map(), jid() | undefined) -> message().
forward_message(#message{sub_els = Els0} = Msg, #{type := Type, room := RoomID} = F, Opts, By) ->
    Content0 = maps:get(content, F),
    Content = case {Type, maps:get(state_key, F, undefined)} of
                  {<<"m.room.member">>, K} when is_binary(K), is_map(Content0) ->
                      Content0#{<<"state_key">> => K};
                  _ ->
                      Content0
              end,
    Ev = event_el(F, Content),
    X = #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, <<"p1:matrix">>}, {<<"room_id">>, RoomID}]},
    {Body, Els} =
        case Type of
            <<"m.room.encrypted">> ->
                {[#text{data = ?ENCRYPTED_BODY}],
                 [#xmlel{name = <<"encryption">>,
                         attrs = [{<<"xmlns">>, ?NS_EME}, {<<"namespace">>, ?NS_E2E},
                                  {<<"name">>, <<"Matrix">>}]},
                  Ev, mod_matrix_gw_xmpp:store_el()]};
            <<"m.room.redaction">> ->
                case maps:get(retract, Opts, undefined) of
                    Target when is_binary(Target), Target /= <<>> ->
                        Reason = case Content of
                                     #{<<"reason">> := R} when is_binary(R) -> R;
                                     _ -> <<>>
                                 end,
                        {RBody, REls} = retraction(Target, By, maps:get(occupant_els, Opts, []),
                                                   Reason),
                        {RBody, [Ev | REls]};
                    _ ->
                        {[], [Ev, mod_matrix_gw_xmpp:store_el()]}
                end;
            <<"m.key.verification.", _/binary>> ->
                {[], [Ev]};
            <<"m.room.message">> ->
                {[], [Ev]};
            _ ->
                {[], [Ev, mod_matrix_gw_xmpp:store_el()]}
        end,
    Msg#message{body = Body, sub_els = Els0 ++ Els ++ [X]}.

%% The body and elements of a retraction stanza: the rooms' renderer
%% (mod_matrix_gw_muc:retraction/4, one for the sender's own XEP-0424 and a
%% moderator's XEP-0425, OccupantEls the moderator's <occupant-id/>), so
%% a direct chat and a room say it alike.
-spec retraction(binary(), jid() | undefined, [xmlel()], binary()) -> {[text()], [xmlel()]}.
retraction(Target, By, OccupantEls, Reason) ->
    {Body, Els} = mod_matrix_gw_muc:retraction(Target, By, OccupantEls, Reason),
    {[#text{data = Body}], Els}.

%% The retraction of the message the XMPP side knows as Target: XEP-0424's
%% <retract/> with the XEP-0428 fallback marker; with By (a moderator's
%% JID, the redactor not being the sender) XEP-0425's <moderated by/> in
%% it, and the reason when there is one. The stanza's body is the
%% caller's ("Message retracted").
-spec retraction_els(binary(), jid() | undefined, binary()) -> [xmlel()].
retraction_els(Target, By, Reason) ->
    Moderated = case By of
                    #jid{} ->
                        [#xmlel{name = <<"moderated">>,
                                attrs = [{<<"xmlns">>, ?NS_MODERATE},
                                         {<<"by">>, jid:encode(By)}]}];
                    _ ->
                        []
                end,
    ReasonEl = [#xmlel{name = <<"reason">>, children = [{xmlcdata, Reason}]}
                || Reason /= <<>>, By /= undefined],
    [#xmlel{name = <<"retract">>, attrs = [{<<"xmlns">>, ?NS_RETRACT}, {<<"id">>, Target}],
            children = Moderated ++ ReasonEl},
     #xmlel{name = <<"fallback">>, attrs = [{<<"xmlns">>, ?NS_FALLBACK},
                                            {<<"for">>, ?NS_RETRACT}]}].

%%--------------------------------------------------------------------
%% The room iqs (to u%srv@G or a room JID)
%%--------------------------------------------------------------------

%% Which room iq this is, if one: {room_info} | {encryption, Algorithm} |
%% {redact, EventID, Reason} | {error, stanza_error()} | false.
-spec room_iq(iq()) -> {room_info} | {encryption, binary()} |
                       {redact, binary(), binary()} | {error, stanza_error()} | false.
room_iq(#iq{type = Type, lang = Lang, sub_els = [#xmlel{name = Name, attrs = Attrs}]}) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_E2E ->
            case {Name, Type} of
                {<<"room-info">>, get} ->
                    {room_info};
                {<<"encryption">>, set} ->
                    {encryption, fxml:get_attr_s(<<"algorithm">>, Attrs)};
                {<<"redact">>, set} ->
                    case fxml:get_attr_s(<<"event">>, Attrs) of
                        <<>> -> {error, xmpp:err_bad_request(<<"no event">>, Lang)};
                        E -> {redact, E, fxml:get_attr_s(<<"reason">>, Attrs)}
                    end;
                {N, _} when N == <<"room-info">>; N == <<"encryption">>; N == <<"redact">> ->
                    {error, xmpp:err_bad_request(<<"wrong iq type">>, Lang)};
                _ ->
                    false
            end;
        _ ->
            false
    end;
room_iq(_) ->
    false.

%% <room-info/> from what the room module read of its state: id, version,
%% encryption (undefined or the m.room.encryption content), direct, power
%% (the asking user's level), members [{MXID, Membership} | {MXID,
%% Membership, Level}], and redact (optional: the level a redaction of
%% another's event needs; with each member's level, what the device judges
%% a forwarded m.room.redaction by).
-spec room_info_el(map()) -> xmlel().
room_info_el(#{id := ID, version := Version, encryption := Enc, direct := Direct,
               power := Power, members := Members} = Info) ->
    EncAttrs =
        case Enc of
            #{<<"algorithm">> := Alg} when is_binary(Alg) ->
                [{<<"encryption">>, Alg},
                 {<<"rotation_period_ms">>, int(maps:get(<<"rotation_period_ms">>, Enc, ?ROTATION_MS), ?ROTATION_MS)},
                 {<<"rotation_period_msgs">>, int(maps:get(<<"rotation_period_msgs">>, Enc, ?ROTATION_MSGS), ?ROTATION_MSGS)}];
            _ ->
                []
        end,
    #xmlel{name = <<"room-info">>,
           attrs = [{<<"xmlns">>, ?NS_E2E}, {<<"id">>, ID}, {<<"version">>, Version}] ++
               EncAttrs ++
               [{<<"direct">>, case Direct of true -> <<"1">>; false -> <<"0">> end},
                {<<"power">>, integer_to_binary(Power)}] ++
               [{<<"redact">>, integer_to_binary(R)} || R <- [maps:get(redact, Info, none)], is_integer(R)],
           children = [#xmlel{name = <<"member">>,
                              attrs = [{<<"id">>, element(1, M)}, {<<"membership">>, element(2, M)}] ++
                                  [{<<"power">>, integer_to_binary(P)}
                                   || tuple_size(M) == 3, P <- [element(3, M)], is_integer(P)]}
                       || M <- lists:sort(Members)]}.

int(I, _) when is_integer(I), I >= 0 -> integer_to_binary(I);
int(B, Default) when is_binary(B) ->
    try binary_to_integer(B) of
        I when I >= 0 -> B;
        _ -> integer_to_binary(Default)
    catch _:_ -> integer_to_binary(Default)
    end;
int(_, Default) -> integer_to_binary(Default).

%%--------------------------------------------------------------------
%% matrix_sent and <event-id/>
%%--------------------------------------------------------------------

%% The event a local user (LUser, LServer) sent with XmppID, kept; the
%% user's oldest rows past MAX_SENT dropped.
-spec record_sent({binary(), binary()}, binary(), binary(), binary()) -> ok.
record_sent(_US, <<>>, _EventID, _RoomID) ->
    ok;
record_sent(US, XmppID, EventID, RoomID) ->
    F = fun() ->
                #matrix_sent_seq{first = First, next = Next} =
                    case mnesia:read(matrix_sent_seq, US, write) of
                        [S] -> S;
                        [] -> #matrix_sent_seq{user = US}
                    end,
                mnesia:write(#matrix_sent{key = {US, XmppID}, event_id = EventID,
                                          room = RoomID, seq = Next}),
                mnesia:write(#matrix_sent_log{key = {US, Next}, xmpp_id = XmppID}),
                First2 = trim(US, First, Next + 1),
                mnesia:write(#matrix_sent_seq{user = US, first = First2, next = Next + 1})
        end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted, Reason} ->
            ?WARNING_MSG("matrix_sent not written: ~p", [Reason]),
            ok
    end.

trim(US, First, Next) when Next - First > ?MAX_SENT ->
    case mnesia:read(matrix_sent_log, {US, First}, write) of
        [#matrix_sent_log{xmpp_id = X}] ->
            mnesia:delete({matrix_sent_log, {US, First}}),
            case mnesia:read(matrix_sent, {US, X}, write) of
                [#matrix_sent{seq = First}] -> mnesia:delete({matrix_sent, {US, X}});
                _ -> ok
            end;
        [] ->
            ok
    end,
    trim(US, First + 1, Next);
trim(_US, First, _Next) ->
    First.

-spec lookup_sent({binary(), binary()}, binary()) -> {ok, binary(), binary()} | error.
lookup_sent(US, XmppID) ->
    case mnesia:dirty_read(matrix_sent, {US, XmppID}) of
        [#matrix_sent{event_id = E, room = R}] -> {ok, E, R};
        [] -> error
    end.

%% The local user who sent EventID from XMPP, the XMPP id and the room
%% (what a room that no longer holds the event knows of it).
-spec sent_event(binary()) -> {ok, {binary(), binary()}, binary(), binary()} | error.
sent_event(EventID) ->
    case catch mnesia:dirty_index_read(matrix_sent, EventID, #matrix_sent.event_id) of
        [#matrix_sent{key = {US, XmppID}, room = R} | _] -> {ok, US, XmppID, R};
        _ -> error
    end.

%% <event-id id='XMPP-ID'/> get on the gateway's JID, from a local user.
-spec route_iq(iq()) -> ok.
route_iq(#iq{type = get, from = From, lang = Lang,
             sub_els = [#xmlel{name = <<"event-id">>, attrs = Attrs}]} = IQ) ->
    Res = case {fxml:get_attr_s(<<"xmlns">>, Attrs), fxml:get_attr_s(<<"id">>, Attrs)} of
              {?NS_E2E, ID} when ID /= <<>> ->
                  case is_local(From) of
                      false ->
                          xmpp:make_error(IQ, xmpp:err_not_allowed(<<"not a local user">>, Lang));
                      true ->
                          case lookup_sent({From#jid.luser, From#jid.lserver}, ID) of
                              {ok, E, R} ->
                                  xmpp:make_iq_result(
                                    IQ, #xmlel{name = <<"event-id">>,
                                               attrs = [{<<"xmlns">>, ?NS_E2E},
                                                        {<<"event">>, E}, {<<"room">>, R}]});
                              error ->
                                  xmpp:make_error(IQ, xmpp:err_item_not_found())
                          end
                  end;
              _ ->
                  xmpp:make_error(IQ, xmpp:err_bad_request(<<"no id">>, Lang))
          end,
    ejabberd_router:route(Res);
route_iq(#iq{lang = Lang} = IQ) ->
    ejabberd_router:route(
      xmpp:make_error(IQ, xmpp:err_bad_request(<<"wrong iq type">>, Lang))).

is_local(#jid{lserver = S}) ->
    S == ejabberd_config:get_myname().

%%--------------------------------------------------------------------
%% Discovery, and the e2e-keys seam
%%--------------------------------------------------------------------

%% The feature a room JID adds to its disco#info (the gateway's own JID and
%% the users' are mod_matrix_gw_e2e's).
-spec disco_features(jid()) -> [binary()].
disco_features(#jid{luser = <<C, _/binary>>}) when C == $!; C == $# ->
    [?NS_E2E];
disco_features(_) ->
    [].

%% Every m.room.member change a room applies, for mod_matrix_gw_e2e's
%% tables (who shares a room with whom, the peer servers).
-spec membership(binary(), binary(), binary(), binary()) -> ok.
membership(Host, RoomID, MXID, Membership) ->
    try mod_matrix_gw_e2e:membership(Host, RoomID, MXID, Membership)
    catch
        Class:Reason ->
            ?WARNING_MSG("membership of ~s in ~s not noted: ~p",
                         [MXID, RoomID, {Class, Reason}]),
            ok
    end,
    %% MatrixRTC's delayed events of a local user leaving the room go
    %% (mod_matrix_gw_rtc)
    try mod_matrix_gw_rtc:membership(Host, RoomID, MXID, Membership)
    catch
        Class2:Reason2 ->
            ?WARNING_MSG("delayed events of ~s in ~s not dropped: ~p",
                         [MXID, RoomID, {Class2, Reason2}]),
            ok
    end,
    ok.
