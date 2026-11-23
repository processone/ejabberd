%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_xmpp.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : What a message means on the other side, both ways
%%% Created : 25 Oct 2026 by Paavel A. <pd@narayana.im>
%%%
%%% the translations between XMPP stanzas and Matrix events and EDUs that
%%% the gateway of 26.09 did not make (it carried m.text bodies alone and
%%% dropped everything else), as functions of their input only, so the room
%%% module calls them and the tests check them on their own:
%%%
%%%   XMPP                                  Matrix
%%%   <body/>                               m.room.message m.text
%%%   XEP-0308 <replace id=X/>              m.room.message, m.relates_to m.replace X
%%%   XEP-0444 <reactions id=X/>            m.reaction m.annotation X, one per key
%%%   XEP-0085 composing / paused, active   m.typing EDU, typing true / false
%%%   XEP-0333 <displayed id=X/>            m.receipt EDU, m.read of X
%%%                                         m.notice: the body; m.emote: "/me " body
%%%
%%% Anything else is left out, with a debug line where it is dropped.
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
-module(mod_matrix_gw_xmpp).

-export([classify/1, text_content/2, edit_content/3, reaction_content/2,
         redaction_content/1, inbound_message/1, typing_edu/3, receipt_edu/4,
         parse_edu/1, replace_el/1, reactions_el/2, displayed_el/1,
         chatstate_el/1, xmpp_id/1, add_reaction/4, remove_reaction/2,
         reaction_keys/3, trim_ids/2, markable_el/0, store_el/0, no_store_el/0]).

-include_lib("xmpp/include/xmpp.hrl").

-define(NS_CORRECT, <<"urn:xmpp:message-correct:0">>).
-define(NS_REACTIONS, <<"urn:xmpp:reactions:0">>).
-define(NS_MARKERS, <<"urn:xmpp:chat-markers:0">>).
-define(NS_STATES, <<"http://jabber.org/protocol/chatstates">>).
-define(XMPP_ID, <<"net.process-one.xmpp-id">>).

%%--------------------------------------------------------------------
%% XMPP to Matrix
%%--------------------------------------------------------------------

%% What an XMPP message asks of the room, in the order to do it:
%%   {edit, TargetID, Text}   a correction
%%   {text, Text}             a new message
%%   {reactions, TargetID, Keys}
%%   {typing, boolean()}      from a chat state
%%   {displayed, ID}
-spec classify(message()) -> [tuple()].
classify(#message{body = Body, sub_els = Els}) ->
    Text = xmpp:get_text(Body),
    Raw = [El || El <- Els],
    Replace = find(Raw, <<"replace">>, ?NS_CORRECT),
    Reactions = find(Raw, <<"reactions">>, ?NS_REACTIONS),
    Displayed = find(Raw, <<"displayed">>, ?NS_MARKERS),
    Message =
        case {Text, Replace} of
            {<<>>, _} -> [];
            {_, #xmlel{} = R} -> [{edit, fxml:get_attr_s(<<"id">>, R#xmlel.attrs), Text}];
            {_, none} -> [{text, Text}]
        end,
    React =
        case Reactions of
            #xmlel{attrs = Attrs, children = Children} ->
                Keys = [fxml:get_tag_cdata(C) || #xmlel{name = <<"reaction">>} = C <- Children],
                [{reactions, fxml:get_attr_s(<<"id">>, Attrs), [K || K <- Keys, K /= <<>>]}];
            none -> []
        end,
    State =
        case chatstate(Raw) of
            composing -> [{typing, true}];
            none -> [];
            _ -> [{typing, false}]
        end,
    Marker =
        case Displayed of
            #xmlel{attrs = Attrs2} -> [{displayed, fxml:get_attr_s(<<"id">>, Attrs2)}];
            none -> []
        end,
    Message ++ React ++ State ++ Marker.

find([], _Name, _NS) -> none;
find([#xmlel{name = Name, attrs = Attrs} = El | Els], Name, NS) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        NS -> El;
        _ -> find(Els, Name, NS)
    end;
find([_ | Els], Name, NS) -> find(Els, Name, NS).

chatstate([]) -> none;
chatstate([#chatstate{type = T} | _]) -> T;
chatstate([#xmlel{name = Name, attrs = Attrs} | Els]) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_STATES when Name == <<"composing">> -> composing;
        ?NS_STATES -> other;
        _ -> chatstate(Els)
    end;
chatstate([_ | Els]) -> chatstate(Els).

%% The content of an m.text, with the XMPP message's id when it has one
%% (the key 26.09 used), so the echo and a correction find it again.
-spec text_content(binary(), binary()) -> map().
text_content(Text, XmppID) ->
    with_id(#{<<"msgtype">> => <<"m.text">>, <<"body">> => Text}, XmppID).

-spec edit_content(binary(), binary(), binary()) -> map().
edit_content(TargetEventID, Text, XmppID) ->
    with_id(#{<<"msgtype">> => <<"m.text">>,
              <<"body">> => <<"* ", Text/binary>>,
              <<"m.new_content">> => #{<<"msgtype">> => <<"m.text">>, <<"body">> => Text},
              <<"m.relates_to">> => #{<<"rel_type">> => <<"m.replace">>,
                                      <<"event_id">> => TargetEventID}},
            XmppID).

-spec reaction_content(binary(), binary()) -> map().
reaction_content(TargetEventID, Key) ->
    #{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.annotation">>,
                              <<"event_id">> => TargetEventID,
                              <<"key">> => Key}}.

-spec redaction_content(binary()) -> map().
redaction_content(EventID) ->
    #{<<"redacts">> => EventID}.

with_id(Content, <<>>) -> Content;
with_id(Content, XmppID) -> Content#{?XMPP_ID => XmppID}.

%% The XMPP message id an event came with, or <<>>.
-spec xmpp_id(map()) -> binary().
xmpp_id(#{<<"content">> := #{?XMPP_ID := ID}}) when is_binary(ID) -> ID;
xmpp_id(_) -> <<>>.

%%--------------------------------------------------------------------
%% Matrix to XMPP
%%--------------------------------------------------------------------

%% What an m.room.message or m.reaction event is to an XMPP client:
%%   {text, Body}                     a message
%%   {edit, TargetEventID, Body}      a correction of TargetEventID
%%   {reaction, TargetEventID, Key}   one reaction added
%%   ignore                           nothing it can show (logged by the caller)
-spec inbound_message(map()) -> {text, binary()} | {edit, binary(), binary()} |
                                {reaction, binary(), binary()} | ignore.
inbound_message(#{<<"type">> := <<"m.reaction">>,
                  <<"content">> := #{<<"m.relates_to">> :=
                                         #{<<"rel_type">> := <<"m.annotation">>,
                                           <<"event_id">> := Target,
                                           <<"key">> := Key}}})
  when is_binary(Target), is_binary(Key) ->
    {reaction, Target, Key};
inbound_message(#{<<"type">> := <<"m.room.message">>,
                  <<"content">> := #{<<"m.relates_to">> :=
                                         #{<<"rel_type">> := <<"m.replace">>,
                                           <<"event_id">> := Target},
                                     <<"m.new_content">> := New}})
  when is_binary(Target), is_map(New) ->
    case body(New) of
        none -> ignore;
        Body -> {edit, Target, Body}
    end;
inbound_message(#{<<"type">> := <<"m.room.message">>, <<"content">> := Content}) ->
    case body(Content) of
        none -> ignore;
        Body -> {text, Body}
    end;
inbound_message(_) ->
    ignore.

body(#{<<"msgtype">> := <<"m.emote">>, <<"body">> := B}) when is_binary(B) ->
    <<"/me ", B/binary>>;
body(#{<<"msgtype">> := T, <<"body">> := B}) when is_binary(B), (T == <<"m.text">> orelse T == <<"m.notice">>) ->
    B;
body(#{<<"msgtype">> := T, <<"body">> := B, <<"url">> := U})
  when is_binary(B), is_binary(U),
       (T == <<"m.image">> orelse T == <<"m.file">> orelse T == <<"m.video">> orelse T == <<"m.audio">>) ->
    %% media is phase 2: the name and the mxc:// address, so the reader
    %% knows something was sent
    <<B/binary, " (", U/binary, ")">>;
body(_) ->
    none.

%% A sender's reactions to one event, kept by the room as
%% #{{Sender, Target} => #{Key => ReactionEventID}}.
-spec add_reaction(map(), {binary(), binary()}, binary(), binary()) -> map().
add_reaction(Reactions, SenderTarget, Key, ReactionEventID) ->
    Keys = maps:get(SenderTarget, Reactions, #{}),
    Reactions#{SenderTarget => Keys#{Key => ReactionEventID}}.

%% The reaction event ReactionEventID taken back: {SenderTarget, Reactions2}
%% or none when it is not one of the reactions kept.
-spec remove_reaction(map(), binary()) -> {{binary(), binary()}, map()} | none.
remove_reaction(Reactions, ReactionEventID) ->
    maps:fold(
      fun(_ST, _Keys, {_, _} = Found) -> Found;
         (ST, Keys, none) ->
              case [K || {K, E} <- maps:to_list(Keys), E == ReactionEventID] of
                  [K | _] -> {ST, Reactions#{ST => maps:remove(K, Keys)}};
                  [] -> none
              end
      end, none, Reactions).

-spec reaction_keys(map(), binary(), binary()) -> [binary()].
reaction_keys(Reactions, Sender, Target) ->
    lists:sort(maps:keys(maps:get({Sender, Target}, Reactions, #{}))).

%% A map of ids bounded to Max entries, the oldest (smallest insertion
%% counter) dropped first: #{Key => {Counter, Value}}.
-spec trim_ids(map(), pos_integer()) -> map().
trim_ids(Map, Max) when map_size(Map) =< Max -> Map;
trim_ids(Map, Max) ->
    Sorted = lists:keysort(2, [{K, C} || {K, {C, _}} <- maps:to_list(Map)]),
    Drop = [K || {K, _} <- lists:sublist(Sorted, map_size(Map) - Max)],
    maps:without(Drop, Map).

%%--------------------------------------------------------------------
%% EDUs
%%--------------------------------------------------------------------

-spec typing_edu(binary(), binary(), boolean()) -> map().
typing_edu(RoomID, UserID, Typing) ->
    #{<<"edu_type">> => <<"m.typing">>,
      <<"content">> => #{<<"room_id">> => RoomID, <<"user_id">> => UserID,
                         <<"typing">> => Typing}}.

-spec receipt_edu(binary(), binary(), binary(), integer()) -> map().
receipt_edu(RoomID, UserID, EventID, TS) ->
    #{<<"edu_type">> => <<"m.receipt">>,
      <<"content">> => #{RoomID => #{<<"m.read">> =>
                                         #{UserID => #{<<"event_ids">> => [EventID],
                                                       <<"data">> => #{<<"ts">> => TS}}}}}}.

%% An incoming EDU as the room module acts on it:
%%   [{typing, RoomID, UserID, boolean()}]
%%   [{receipt, RoomID, UserID, EventID}]   one per read event
%%   []                                     anything else (m.presence, ...)
-spec parse_edu(term()) -> [tuple()].
parse_edu(#{<<"edu_type">> := <<"m.typing">>,
            <<"content">> := #{<<"room_id">> := RoomID, <<"user_id">> := UserID,
                               <<"typing">> := Typing}})
  when is_binary(RoomID), is_binary(UserID), is_boolean(Typing) ->
    [{typing, RoomID, UserID, Typing}];
parse_edu(#{<<"edu_type">> := <<"m.receipt">>, <<"content">> := Content}) when is_map(Content) ->
    lists:append(
      [[{receipt, RoomID, UserID, EventID}
        || {UserID, #{<<"event_ids">> := EventIDs}} <- maps:to_list(Users),
           is_binary(UserID), is_list(EventIDs), EventID <- EventIDs, is_binary(EventID)]
       || {RoomID, #{<<"m.read">> := Users}} <- maps:to_list(Content),
          is_binary(RoomID), is_map(Users)]);
parse_edu(_) ->
    [].

%%--------------------------------------------------------------------
%% XML
%%--------------------------------------------------------------------

-spec replace_el(binary()) -> xmlel().
replace_el(ID) ->
    #xmlel{name = <<"replace">>, attrs = [{<<"xmlns">>, ?NS_CORRECT}, {<<"id">>, ID}]}.

-spec reactions_el(binary(), [binary()]) -> xmlel().
reactions_el(ID, Keys) ->
    #xmlel{name = <<"reactions">>, attrs = [{<<"xmlns">>, ?NS_REACTIONS}, {<<"id">>, ID}],
           children = [#xmlel{name = <<"reaction">>, children = [{xmlcdata, K}]} || K <- Keys]}.

-spec displayed_el(binary()) -> xmlel().
displayed_el(ID) ->
    #xmlel{name = <<"displayed">>, attrs = [{<<"xmlns">>, ?NS_MARKERS}, {<<"id">>, ID}]}.

-spec chatstate_el(boolean()) -> xmlel().
chatstate_el(Typing) ->
    #xmlel{name = case Typing of true -> <<"composing">>; false -> <<"paused">> end,
           attrs = [{<<"xmlns">>, ?NS_STATES}]}.

%% XEP-0333's <markable/>: the reader may say it was displayed.
-spec markable_el() -> xmlel().
markable_el() ->
    #xmlel{name = <<"markable">>, attrs = [{<<"xmlns">>, ?NS_MARKERS}]}.

%% XEP-0334 hints: a message with no body kept (a reaction, a marker), or
%% not (a chat state).
-spec store_el() -> xmlel().
store_el() ->
    #xmlel{name = <<"store">>, attrs = [{<<"xmlns">>, ?NS_HINTS}]}.

-spec no_store_el() -> xmlel().
no_store_el() ->
    #xmlel{name = <<"no-store">>, attrs = [{<<"xmlns">>, ?NS_HINTS}]}.
