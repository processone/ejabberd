%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_muc.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : A Matrix room as a complete MUC room, both ways
%%% Created : 2 Nov 2026 by Paavel A. <pd@narayana.im>
%%%
%%% The gateway of 26.09 joins a Matrix room by MUC presence and carries its
%%% text; what a daily-driver XMPP client expects of a room beyond that is
%%% here, as functions of their input, which the room module
%%% (mod_matrix_gw_room) calls with the room's state:
%%%
%%%   Matrix                                XMPP (XEP-0045 unless said)
%%%   m.room.member join/leave/kick/ban     occupant presence, codes 110, 307,
%%%                                         301, actor and reason
%%%   m.room.member displayname             XEP-0172 <nick/> on the presence
%%%   power levels, creator                 affiliation and role
%%%   m.room.power_levels change            admin iq: role moderator/participant,
%%%                                         affiliation owner/admin/member
%%%   m.room.member leave/ban/invite        admin iq: role none (kick),
%%%                                         affiliation outcast/none/member
%%%   m.room.name, m.room.topic             disco#info, muc#roominfo, subject,
%%%                                         muc#owner config form, code 104
%%%   m.room.join_rules                     muc_membersonly / muc_open
%%%   (the sender of each event)            XEP-0421 occupant-id
%%%   m.room.redaction of another's event   XEP-0425 moderation (and its
%%%                                         retraction as XEP-0424 says it)
%%%   m.room.member invite                  mediated invite, XEP-0249 invite
%%%   m.room.canonical_alias, a new room    a join of #name%<our domain>
%%%                                         that does not exist creates it
%%%
%%% and the room aliases this server publishes (matrix_room_alias, mnesia),
%%% which /_matrix/federation/v1/query/directory answers from.
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
-module(mod_matrix_gw_muc).

-export([create_db/0, alias_set/2, alias_lookup/1, alias_forget/1, local_alias/2,
         nick/1, nick_to_user_id/1, occupant_id/2, occupant_id_el/2, nick_el/1,
         levels/1, item/3, can_redact/2, set_user_level/3, admin_level/2,
         membership_change/4, change_item/5, change_codes/2,
         admin_action/4, target_user_id/2,
         disco_info/1, config_form/2, config_changes/1,
         invites/1, direct_invite/1, moderate_request/1, retraction/4,
         create_events/4, history_request/2, stripped_state_keys/0,
         avatar_update_el/1, room_vcard/3, status_message/3]).

-include_lib("xmpp/include/xmpp.hrl").

-define(NS_OCCUPANT, <<"urn:xmpp:occupant-id:0">>).
-define(NS_RETRACT, <<"urn:xmpp:message-retract:1">>).
-define(NS_RETRACT_0, <<"urn:xmpp:message-retract:0">>).
-define(NS_MODERATE, <<"urn:xmpp:message-moderate:1">>).
-define(NS_MODERATE_0, <<"urn:xmpp:message-moderate:0">>).
-define(NS_FASTEN, <<"urn:xmpp:fasten:0">>).
-define(NS_FALLBACK, <<"urn:xmpp:fallback:0">>).
-define(NS_HINTS_, <<"urn:xmpp:hints">>).
-define(NS_NICK_, <<"http://jabber.org/protocol/nick">>).
-define(NS_VCARD_UPDATE_, <<"vcard-temp:x:update">>).
-define(ROOMINFO, <<"http://jabber.org/protocol/muc#roominfo">>).
-define(ROOMCONFIG, <<"http://jabber.org/protocol/muc#roomconfig">>).
-define(MAX_HISTORY, 100).
-define(DEFAULT_HISTORY, 20).

-record(matrix_room_alias,
        {alias :: binary() | '$1',
         room_id :: binary()}).

%%--------------------------------------------------------------------
%% Aliases this server publishes
%%--------------------------------------------------------------------

create_db() ->
    ejabberd_mnesia:create(
      ?MODULE, matrix_room_alias,
      [{disc_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_room_alias)}]),
    ok.

-spec alias_set(binary(), binary()) -> ok.
alias_set(Alias, RoomID) ->
    mnesia:dirty_write(#matrix_room_alias{alias = Alias, room_id = RoomID}).

-spec alias_lookup(binary()) -> {ok, binary()} | error.
alias_lookup(Alias) ->
    case mnesia:dirty_read(matrix_room_alias, Alias) of
        [#matrix_room_alias{room_id = RoomID}] -> {ok, RoomID};
        [] -> error
    end.

%% A room this server left: its aliases go with it.
-spec alias_forget(binary()) -> ok.
alias_forget(RoomID) ->
    lists:foreach(fun(A) -> mnesia:dirty_delete(matrix_room_alias, A) end,
                  mnesia:dirty_select(matrix_room_alias,
                                      [{#matrix_room_alias{alias = '$1', room_id = RoomID},
                                        [], ['$1']}])).

%% The alias #Name:Domain a room JID's local part #Name%Domain names when
%% Domain is this server's Matrix name, else other.
-spec local_alias(binary(), binary()) -> {ok, binary()} | other.
local_alias(<<$#, Parts/binary>>, Domain) ->
    case binary:split(Parts, <<"%">>) of
        [Name, Domain] when Name /= <<>> -> {ok, <<$#, Name/binary, $:, Domain/binary>>};
        _ -> other
    end;
local_alias(_, _) ->
    other.

%%--------------------------------------------------------------------
%% Occupants
%%--------------------------------------------------------------------

%% A Matrix user's nick in a room: the user id without its @, stable and
%% unique (26.09's choice, kept).
-spec nick(binary()) -> binary().
nick(<<$@, Nick/binary>>) -> Nick;
nick(UserID) -> UserID.

-spec nick_to_user_id(binary()) -> {ok, binary()} | error.
nick_to_user_id(Nick) ->
    case binary:split(Nick, <<":">>) of
        [U, S] when U /= <<>>, S /= <<>> -> {ok, <<$@, Nick/binary>>};
        _ -> error
    end.

%% XEP-0421: the same id for a user in a room on every presence and message,
%% another in another room; not the user id itself (the nick is that).
-spec occupant_id(binary(), binary()) -> binary().
occupant_id(RoomID, UserID) ->
    base64:encode(crypto:hash(sha256, <<"mod_matrix_gw occupant-id", 0, RoomID/binary, 0,
                                        UserID/binary>>),
                  #{mode => urlsafe, padding => false}).

-spec occupant_id_el(binary(), binary()) -> xmlel().
occupant_id_el(RoomID, UserID) ->
    #xmlel{name = <<"occupant-id">>,
           attrs = [{<<"xmlns">>, ?NS_OCCUPANT}, {<<"id">>, occupant_id(RoomID, UserID)}]}.

%% XEP-0172: the display name, when there is one.
-spec nick_el(term()) -> [xmlel()].
nick_el(Name) when is_binary(Name), Name /= <<>> ->
    [#xmlel{name = <<"nick">>, attrs = [{<<"xmlns">>, ?NS_NICK_}], children = [{xmlcdata, Name}]}];
nick_el(_) ->
    [].

%%--------------------------------------------------------------------
%% Power levels
%%--------------------------------------------------------------------

%% The levels of a m.room.power_levels content (#{} when there is none),
%% with the spec's defaults; `message` is the level to send m.room.message.
-spec levels(map()) -> map().
levels(PL) when is_map(PL) ->
    Int = fun(K, D) -> int(maps:get(K, PL, D), D) end,
    Events = case maps:get(<<"events">>, PL, #{}) of E when is_map(E) -> E; _ -> #{} end,
    Users = case maps:get(<<"users">>, PL, #{}) of U when is_map(U) -> U; _ -> #{} end,
    EventsDefault = Int(<<"events_default">>, 0),
    #{ban => Int(<<"ban">>, 50), kick => Int(<<"kick">>, 50), redact => Int(<<"redact">>, 50),
      invite => Int(<<"invite">>, 0), state_default => Int(<<"state_default">>, 50),
      events_default => EventsDefault, users_default => Int(<<"users_default">>, 0),
      message => int(maps:get(<<"m.room.message">>, Events, EventsDefault), EventsDefault),
      events => Events, users => Users};
levels(_) ->
    levels(#{}).

int(I, _) when is_integer(I) -> I;
int(B, D) when is_binary(B) ->
    try binary_to_integer(B) catch _:_ -> D end;
int(_, D) -> D.

%% The MUC item of a user at Level with Membership: owner from level 100
%% (a creator's level, in room versions that have one, is above), admin
%% from the state default (who may change the room), member for the rest
%% of the room's members; moderator from the kick level, visitor below the
%% level to send a message.
-spec item(map(), integer(), binary() | undefined) -> muc_item().
item(_Levels, _Level, <<"ban">>) ->
    #muc_item{affiliation = outcast, role = none};
item(Levels, Level, Membership) ->
    #{kick := Kick, state_default := StateDefault, users_default := Default,
      message := Message} = Levels,
    Elevated = Level > Default,
    Affiliation =
        if
            Level >= 100 -> owner;
            Level >= StateDefault, Elevated -> admin;
            Membership == <<"join">>; Membership == <<"invite">> -> member;
            true -> none
        end,
    Role =
        if
            Membership /= <<"join">> -> none;
            Level >= Kick, Elevated -> moderator;
            Level < Message -> visitor;
            true -> participant
        end,
    #muc_item{affiliation = Affiliation, role = Role}.

%% Whether a user at Level may redact others' events (XEP-0425's feature).
-spec can_redact(map(), integer()) -> boolean().
can_redact(#{redact := Redact} = Levels, Level) ->
    Level >= Redact andalso Level > maps:get(users_default, Levels).

%% The power levels content with UserID at Level (default: the room's
%% default, the user taken out of the users map).
-spec set_user_level(map(), binary(), integer() | default) -> map().
set_user_level(PL, UserID, Level) ->
    Users = case maps:get(<<"users">>, PL, #{}) of U when is_map(U) -> U; _ -> #{} end,
    Users2 = case Level of
                 default -> maps:remove(UserID, Users);
                 _ -> Users#{UserID => Level}
             end,
    PL#{<<"users">> => Users2}.

%% The level an admin iq gives: a moderator kicks, an admin changes the
%% room (and bans), an owner is 100.
-spec admin_level(moderator | admin | owner, map()) -> integer().
admin_level(moderator, #{kick := Kick, users_default := D}) -> max(Kick, D + 1);
admin_level(admin, #{kick := Kick, ban := Ban, state_default := S, users_default := D}) ->
    lists:max([Kick, Ban, S, D + 1]);
admin_level(owner, _) -> 100.

%%--------------------------------------------------------------------
%% Membership changes
%%--------------------------------------------------------------------

%% What an m.room.member event is to the room's occupants, from the
%% member's content before it (undefined: none) and after:
%%   join     she came in            profile  her name or avatar changed
%%   leave    she went               kick     she was made to go (307)
%%   ban      she was banned in the room (301)
%%   outcast  she was banned from outside it
%%   unban    a ban taken back       invite   she was invited
%%   ignore   anything else (an invite declined or withdrawn, a knock)
%% A Prev of undefined is a state before the event the room cannot tell
%% (its previous events' states missing): taken as no member then, so a
%% leave or ban is still one.
-spec membership_change(map() | undefined, map(), binary(), binary()) -> atom().
membership_change(undefined, Content, Sender, StateKey) ->
    case membership(Content) of
        <<"join">> -> join;
        _ -> membership_change(#{<<"membership">> => <<"join">>}, Content, Sender, StateKey)
    end;
membership_change(Prev, Content, Sender, StateKey) ->
    Was = membership(Prev),
    case {Was, membership(Content)} of
        {<<"join">>, <<"join">>} -> profile;
        {_, <<"join">>} -> join;
        {<<"join">>, <<"leave">>} when Sender == StateKey -> leave;
        {<<"join">>, <<"leave">>} -> kick;
        {<<"ban">>, <<"leave">>} -> unban;
        {<<"join">>, <<"ban">>} -> ban;
        {_, <<"ban">>} -> outcast;
        {Was, <<"invite">>} when Was /= <<"invite">> -> invite;
        _ -> ignore
    end.

membership(#{<<"membership">> := M}) when is_binary(M) -> M;
membership(_) -> <<"leave">>.

%% The item of an occupant's unavailable presence for Change, with the
%% actor and the reason of a kick or a ban.
-spec change_item(atom(), muc_item(), binary(), binary(), map()) -> muc_item().
change_item(Change, Item, Sender, StateKey, Content) when Change == kick; Change == ban ->
    Reason = case maps:get(<<"reason">>, Content, <<>>) of R when is_binary(R) -> R; _ -> <<>> end,
    Actor = case Sender of
                StateKey -> undefined;
                _ -> #muc_actor{nick = nick(Sender)}
            end,
    Item#muc_item{actor = Actor, reason = Reason, role = none,
                  affiliation = case Change of ban -> outcast; kick -> Item#muc_item.affiliation end};
change_item(_, Item, _, _, _) ->
    Item#muc_item{role = none}.

-spec change_codes(atom(), boolean()) -> [pos_integer()].
change_codes(Change, Self) ->
    [110 || Self] ++
        case Change of
            kick -> [307];
            ban -> [301];
            _ -> []
        end.

%%--------------------------------------------------------------------
%% XEP-0045 admin requests
%%--------------------------------------------------------------------

%% The Matrix user an admin item names: by nick (role changes) or JID
%% (affiliation changes); ToUserID turns a JID into a user id.
-spec target_user_id(muc_item(), fun((jid()) -> {ok, binary()} | error)) ->
          {ok, binary()} | error.
target_user_id(#muc_item{jid = #jid{} = JID}, ToUserID) ->
    ToUserID(JID);
target_user_id(#muc_item{nick = Nick}, _) when Nick /= <<>> ->
    nick_to_user_id(Nick);
target_user_id(_, _) ->
    error.

%% What an admin item asks of the room for a target at Level with
%% Membership (a binary, <<"leave">> for none):
%%   {member, Membership, Reason}   an m.room.member event about the target
%%   {level, Level | default}       a power levels change for the target
%%   ok                             nothing to do
%%   {error, Reason}                not-acceptable / feature-not-implemented
-spec admin_action(muc_item(), map(), integer(), binary()) ->
          {member, binary(), binary()} | {level, integer() | default} | ok | {error, atom()}.
admin_action(#muc_item{role = none, affiliation = undefined, reason = Reason}, _, _, Membership) ->
    case Membership of
        <<"join">> -> {member, <<"leave">>, Reason};
        _ -> ok
    end;
admin_action(#muc_item{role = moderator, affiliation = undefined}, Levels, Level, _) ->
    New = admin_level(moderator, Levels),
    if Level >= New -> ok;
       true -> {level, New}
    end;
admin_action(#muc_item{role = participant, affiliation = undefined}, Levels, Level, _) ->
    case Level >= maps:get(kick, Levels) andalso Level > maps:get(users_default, Levels) of
        true -> {level, default};
        false -> ok
    end;
admin_action(#muc_item{role = visitor, affiliation = undefined}, _, _, _) ->
    {error, 'feature-not-implemented'};
admin_action(#muc_item{affiliation = outcast, reason = Reason}, _, _, _) ->
    {member, <<"ban">>, Reason};
admin_action(#muc_item{affiliation = none, reason = Reason}, Levels, Level, Membership) ->
    %% an invite is withdrawn whatever the
    %% invitee's level
    case Membership of
        <<"ban">> -> {member, <<"leave">>, Reason};
        <<"invite">> -> {member, <<"leave">>, Reason};
        _ when Level > map_get(users_default, Levels) -> {level, default};
        _ -> ok
    end;
admin_action(#muc_item{affiliation = member, reason = Reason}, Levels, Level, Membership) ->
    case Membership of
        _ when Level > map_get(users_default, Levels) -> {level, default};
        <<"join">> -> ok;
        <<"invite">> -> ok;
        <<"ban">> -> {error, 'not-acceptable'};
        _ -> {member, <<"invite">>, Reason}
    end;
admin_action(#muc_item{affiliation = A}, Levels, Level, _) when A == admin; A == owner ->
    New = admin_level(A, Levels),
    if Level == New -> ok;
       true -> {level, New}
    end;
admin_action(_, _, _, _) ->
    {error, 'bad-request'}.

%%--------------------------------------------------------------------
%% The room's identity
%%--------------------------------------------------------------------

%% disco#info of a room, from what the room module knows of it:
%% name, topic, join_rule, occupants (a count), moderate (whether the
%% asker may redact others' events), avatar_hash, features (more of them).
-spec disco_info(map()) -> disco_info().
disco_info(Info) ->
    Name = maps:get(name, Info, <<>>),
    Topic = maps:get(topic, Info, <<>>),
    Access = case maps:get(join_rule, Info, <<"invite">>) of
                 <<"public">> -> [<<"muc_open">>, <<"muc_public">>];
                 _ -> [<<"muc_membersonly">>, <<"muc_hidden">>]
             end,
    Moderation = case maps:get(moderate, Info, false) of
                     true -> [?NS_MODERATE, ?NS_MODERATE_0, ?NS_RETRACT];
                     false -> [?NS_RETRACT]
                 end,
    Features = lists:usort([?NS_DISCO_INFO, ?NS_DISCO_ITEMS, ?NS_MUC, <<"muc_nonanonymous">>,
                            <<"muc_persistent">>, <<"muc_unsecured">>, <<"muc_unmoderated">>,
                            ?NS_OCCUPANT, <<"vcard-temp">>]
                           ++ Access ++ Moderation ++ maps:get(features, Info, [])),
    Fields =
        [#xdata_field{var = <<"FORM_TYPE">>, type = hidden, values = [?ROOMINFO]},
         #xdata_field{var = <<"muc#roominfo_description">>, label = <<"Description">>,
                      values = [Topic]},
         #xdata_field{var = <<"muc#roominfo_subject">>, label = <<"Subject">>, values = [Topic]},
         #xdata_field{var = <<"muc#roominfo_occupants">>, label = <<"Number of occupants">>,
                      values = [integer_to_binary(maps:get(occupants, Info, 0))]},
         #xdata_field{var = <<"muc#roomconfig_roomname">>, label = <<"Name">>, values = [Name]}]
        ++ [#xdata_field{var = <<"muc#roominfo_avatarhash">>, values = [H]}
            || H <- [maps:get(avatar_hash, Info, undefined)], is_binary(H)],
    #disco_info{identities = [#identity{category = <<"conference">>, type = <<"text">>,
                                        name = Name}],
                features = Features,
                xdata = [#xdata{type = result, fields = Fields}]}.

%% The room configuration form (muc#owner get): the name and the
%% description (the topic), the two a Matrix room's state has.
-spec config_form(binary(), binary()) -> xdata().
config_form(Name, Topic) ->
    #xdata{type = form, title = <<"Room configuration">>,
           fields = [#xdata_field{var = <<"FORM_TYPE">>, type = hidden, values = [?ROOMCONFIG]},
                     #xdata_field{var = <<"muc#roomconfig_roomname">>, type = 'text-single',
                                  label = <<"Name">>, values = [Name]},
                     #xdata_field{var = <<"muc#roomconfig_roomdesc">>, type = 'text-single',
                                  label = <<"Description">>, values = [Topic]},
                     #xdata_field{var = <<"muc#roomconfig_persistentroom">>, type = boolean,
                                  label = <<"Persistent">>, values = [<<"1">>]}]}.

%% What a submitted form changes: [{name, Name}, {topic, Topic}], each
%% only when the form has the field.
-spec config_changes(xdata() | undefined) -> [{name | topic, binary()}].
config_changes(#xdata{type = submit, fields = Fields}) ->
    lists:flatmap(
      fun(#xdata_field{var = <<"muc#roomconfig_roomname">>, values = Vs}) ->
              [{name, iolist_to_binary(Vs)}];
         (#xdata_field{var = <<"muc#roomconfig_roomdesc">>, values = Vs}) ->
              [{topic, iolist_to_binary(Vs)}];
         (_) ->
              []
      end, Fields);
config_changes(_) ->
    [].

%% The room's vCard (XEP-0054 on the room JID, XEP-0486): its avatar.
-spec room_vcard(binary(), binary() | undefined, binary() | undefined) -> vcard_temp().
room_vcard(Name, undefined, _) ->
    #vcard_temp{fn = Name};
room_vcard(Name, Mime, Bytes) ->
    #vcard_temp{fn = Name, photo = #vcard_photo{type = Mime, binval = Bytes}}.

%% XEP-0153 on the room's own presence (XEP-0486): the avatar's hash, or
%% an empty photo (no avatar).
-spec avatar_update_el(binary() | undefined) -> xmlel().
avatar_update_el(Hash) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_VCARD_UPDATE_}],
           children = [#xmlel{name = <<"photo">>,
                              children = [{xmlcdata, Hash} || is_binary(Hash)]}]}.

%% A status message of the room (XEP-0045 §10.2.1: 104, the configuration
%% changed).
-spec status_message(jid(), jid(), [pos_integer()]) -> message().
status_message(RoomJID, To, Codes) ->
    #message{id = p1_rand:get_string(), from = RoomJID, to = To, type = groupchat,
             sub_els = [#muc_user{status_codes = Codes}]}.

%%--------------------------------------------------------------------
%% Invites
%%--------------------------------------------------------------------

%% The mediated invites (XEP-0045 §7.8.2) a message to a room carries:
%% [{InviteeJID, Reason}].
-spec invites(message()) -> [{jid(), binary()}].
invites(Msg) ->
    case catch xmpp:get_subtag(Msg, #muc_user{}) of
        #muc_user{invites = Invites} ->
            [{To, R} || #muc_invite{to = #jid{} = To, reason = R} <- Invites];
        _ ->
            []
    end.

%% A direct invite (XEP-0249): {RoomJID, Reason} or none.
-spec direct_invite(message()) -> {jid(), binary()} | none.
direct_invite(Msg) ->
    case catch xmpp:get_subtag(Msg, #x_conference{jid = jid:make(<<>>)}) of
        #x_conference{jid = #jid{} = Room, reason = R} -> {Room, R};
        _ -> none
    end.

%%--------------------------------------------------------------------
%% Moderation (XEP-0425) and retractions (XEP-0424)
%%--------------------------------------------------------------------

%% The message a moderation iq's element asks to retract, with the reason:
%% XEP-0425 1.0's <moderate id/> or 0.2's <apply-to id><moderate/></apply-to>.
-spec moderate_request(term()) -> {ok, binary(), binary()} | error.
moderate_request(#xmlel{name = <<"moderate">>} = El) ->
    case {fxml:get_tag_attr_s(<<"xmlns">>, El), fxml:get_tag_attr_s(<<"id">>, El)} of
        {?NS_MODERATE, ID} when ID /= <<>> ->
            case fxml:get_subtag_with_xmlns(El, <<"retract">>, ?NS_RETRACT) of
                false -> error;
                _ -> {ok, ID, reason(El)}
            end;
        _ ->
            error
    end;
moderate_request(#xmlel{name = <<"apply-to">>} = El) ->
    case {fxml:get_tag_attr_s(<<"xmlns">>, El), fxml:get_tag_attr_s(<<"id">>, El),
          fxml:get_subtag_with_xmlns(El, <<"moderate">>, ?NS_MODERATE_0)} of
        {?NS_FASTEN, ID, #xmlel{} = M} when ID /= <<>> ->
            case fxml:get_subtag(M, <<"retract">>) of
                false -> error;
                _ -> {ok, ID, reason(M)}
            end;
        _ ->
            error
    end;
moderate_request(_) ->
    error.

reason(El) ->
    case fxml:get_subtag(El, <<"reason">>) of
        #xmlel{} = R -> fxml:get_tag_cdata(R);
        false -> <<>>
    end.

%% What a groupchat message retracting TargetID says, {Body, SubEls}: the
%% sender's own retraction (XEP-0424) when By is undefined, else a
%% moderator's (XEP-0425 1.0, and 0.2's for clients of that version) with
%% By the moderator's occupant JID and OccupantEls her <occupant-id/>.
%% Nothing is deleted: the retraction is a message of its own, the
%% message it names stays in every archive that has it, and the fallback
%% body says what happened to a client that knows neither.
-spec retraction(binary(), jid() | undefined, [xmlel()], binary()) -> {binary(), [xmlel()]}.
retraction(TargetID, undefined, _OccupantEls, _Reason) ->
    {<<"Message retracted">>,
     [#xmlel{name = <<"retract">>, attrs = [{<<"xmlns">>, ?NS_RETRACT}, {<<"id">>, TargetID}]},
      fallback(), store()]};
retraction(TargetID, By, OccupantEls, Reason) ->
    ByS = jid:encode(By),
    ReasonEls = [#xmlel{name = <<"reason">>, children = [{xmlcdata, Reason}]} || Reason /= <<>>],
    Body = case Reason of
               <<>> -> <<"This message has been moderated">>;
               _ -> <<"This message has been moderated: ", Reason/binary>>
           end,
    {Body,
     [#xmlel{name = <<"retract">>, attrs = [{<<"xmlns">>, ?NS_RETRACT}, {<<"id">>, TargetID}],
             children = [#xmlel{name = <<"moderated">>,
                                attrs = [{<<"xmlns">>, ?NS_MODERATE}, {<<"by">>, ByS}],
                                children = OccupantEls}] ++ ReasonEls},
      #xmlel{name = <<"apply-to">>, attrs = [{<<"xmlns">>, ?NS_FASTEN}, {<<"id">>, TargetID}],
             children = [#xmlel{name = <<"moderated">>,
                                attrs = [{<<"xmlns">>, ?NS_MODERATE_0}, {<<"by">>, ByS}],
                                children = [#xmlel{name = <<"retract">>,
                                                   attrs = [{<<"xmlns">>, ?NS_RETRACT_0}]}]
                                ++ ReasonEls}]},
      fallback(), store()]}.

fallback() ->
    #xmlel{name = <<"fallback">>, attrs = [{<<"xmlns">>, ?NS_FALLBACK}, {<<"for">>, ?NS_RETRACT}]}.

store() ->
    #xmlel{name = <<"store">>, attrs = [{<<"xmlns">>, ?NS_HINTS_}]}.

%%--------------------------------------------------------------------
%% A room made from XMPP
%%--------------------------------------------------------------------

%% The events that make a room for its creator Creator (a local user) who
%% joined #name%<our domain>: invite-only, history shared with members,
%% the creator at 100, the alias as its canonical alias.
-spec create_events(binary(), binary(), binary(), binary()) -> [map()].
create_events(Creator, Alias, Version, Name) ->
    State = fun(Type, Content) ->
                    #{<<"type">> => Type, <<"sender">> => Creator, <<"state_key">> => <<"">>,
                      <<"content">> => Content}
            end,
    [State(<<"m.room.create">>, #{<<"creator">> => Creator, <<"room_version">> => Version}),
     #{<<"type">> => <<"m.room.member">>, <<"sender">> => Creator, <<"state_key">> => Creator,
       <<"content">> => #{<<"membership">> => <<"join">>}},
     State(<<"m.room.power_levels">>,
           #{<<"ban">> => 50, <<"kick">> => 50, <<"redact">> => 50, <<"invite">> => 0,
             <<"state_default">> => 50, <<"events_default">> => 0, <<"users_default">> => 0,
             %% MatrixRTC's types too (mod_matrix_gw_rtc)
             <<"events">> => maps:merge(
                               mod_matrix_gw_rtc:power_levels(),
                               #{<<"m.room.name">> => 50, <<"m.room.avatar">> => 50,
                                 <<"m.room.topic">> => 50,
                                 <<"m.room.canonical_alias">> => 50,
                                 <<"m.room.power_levels">> => 100,
                                 <<"m.room.history_visibility">> => 100,
                                 <<"m.room.encryption">> => 100,
                                 <<"m.room.server_acl">> => 100,
                                 <<"m.room.tombstone">> => 100}),
             <<"users">> => #{Creator => 100}}),
     State(<<"m.room.join_rules">>, #{<<"join_rule">> => <<"invite">>}),
     State(<<"m.room.history_visibility">>, #{<<"history_visibility">> => <<"shared">>}),
     State(<<"m.room.guest_access">>, #{<<"guest_access">> => <<"forbidden">>}),
     State(<<"m.room.canonical_alias">>, #{<<"alias">> => Alias})]
        ++ [State(<<"m.room.name">>, #{<<"name">> => Name}) || Name /= <<>>].

%% The state an invite carries for the invitee's client (/v2/invite's
%% invite_room_state): what it shows of the room before joining.
-spec stripped_state_keys() -> [{binary(), binary()}].
stripped_state_keys() ->
    [{T, <<"">>} || T <- [<<"m.room.create">>, <<"m.room.join_rules">>, <<"m.room.name">>,
                          <<"m.room.canonical_alias">>, <<"m.room.avatar">>,
                          <<"m.room.encryption">>, <<"m.room.topic">>]].

%%--------------------------------------------------------------------
%% History
%%--------------------------------------------------------------------

%% What a join asks of the room's history (<history/>, XEP-0045 §7.2.14):
%% {MaxStanzas, SinceMs}, at most MAX_HISTORY messages; DEFAULT_HISTORY
%% when it asks nothing; maxchars='0' is none.
-spec history_request(muc_history() | undefined, integer()) -> {non_neg_integer(), integer()}.
history_request(#muc_history{maxchars = 0}, _Now) ->
    {0, 0};
history_request(#muc_history{maxstanzas = N, seconds = S, since = Since}, Now) ->
    Max = case N of
              undefined -> ?MAX_HISTORY;
              _ -> min(N, ?MAX_HISTORY)
          end,
    From = lists:max([0 | [Now - S * 1000 || is_integer(S)]
                      ++ [misc:now_to_usec(Since) div 1000 || Since /= undefined]]),
    case {N, S, Since} of
        {undefined, undefined, undefined} -> {?DEFAULT_HISTORY, 0};
        _ -> {Max, From}
    end;
history_request(_, _) ->
    {?DEFAULT_HISTORY, 0}.
