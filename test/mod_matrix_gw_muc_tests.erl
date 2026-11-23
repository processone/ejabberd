%%% mod_matrix_gw_muc's tests: the mapping on its own (items from power
%%% levels, membership changes, admin requests, forms, invites, moderation,
%%% the room made from XMPP, history), then a whole room on an ejabberd
%%% started in this node with the gateway on: a room made by a local user's
%%% join of #lounge%example.com, its alias in /query/directory, a second
%%% local user refused, invited and let in, the subject, the configuration
%%% form, disco#info, a grant and a revoke, moderation and a retraction, a
%%% kick, a ban and its list, the vCard, the history of a second resource,
%%% a direct invite of a remote user. No federation: the room's members are
%%% all local but the invited one, of remote.invalid, which resolves nowhere.
-module(mod_matrix_gw_muc_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).
-define(ROOM, <<"#lounge%example.com@matrix.example.com">>).
-define(ANN, <<"ann@example.com/phone">>).
-define(ANN2, <<"ann@example.com/tablet">>).
-define(BEN, <<"ben@example.com/pc">>).
-define(ANN_ID, <<"@ann:example.com">>).
-define(BEN_ID, <<"@ben:example.com">>).

j(S) -> jid:decode(S).

pl() ->
    #{<<"ban">> => 50, <<"kick">> => 50, <<"redact">> => 50, <<"state_default">> => 50,
      <<"events_default">> => 0, <<"users_default">> => 0,
      <<"events">> => #{<<"m.room.message">> => 10},
      <<"users">> => #{?ANN_ID => 100}}.

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------
pure_test_() ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(xmpp) end,
     [fun names/0, fun levels/0, fun items/0, fun changes/0, fun admin/0, fun disco/0,
      fun forms/0, fun invites/0, fun moderation/0, fun retractions/0, fun creation/0,
      fun history/0, fun vcard/0]}.

names() ->
    ?assertEqual(<<"bob:remote.invalid">>, mod_matrix_gw_muc:nick(<<"@bob:remote.invalid">>)),
    ?assertEqual(<<"plain">>, mod_matrix_gw_muc:nick(<<"plain">>)),
    ?assertEqual({ok, <<"@bob:srv">>}, mod_matrix_gw_muc:nick_to_user_id(<<"bob:srv">>)),
    ?assertEqual(error, mod_matrix_gw_muc:nick_to_user_id(<<"bob">>)),
    ?assertEqual(error, mod_matrix_gw_muc:nick_to_user_id(<<":srv">>)),
    %% the same in a room, another in another room, not the user id
    A = mod_matrix_gw_muc:occupant_id(<<"!r:s">>, <<"@a:s">>),
    ?assertEqual(A, mod_matrix_gw_muc:occupant_id(<<"!r:s">>, <<"@a:s">>)),
    ?assertNotEqual(A, mod_matrix_gw_muc:occupant_id(<<"!r2:s">>, <<"@a:s">>)),
    ?assertNotEqual(A, mod_matrix_gw_muc:occupant_id(<<"!r:s">>, <<"@b:s">>)),
    ?assertEqual(nomatch, binary:match(A, [<<"@">>, <<"=">>, <<"+">>, <<"/">>])),
    El = mod_matrix_gw_muc:occupant_id_el(<<"!r:s">>, <<"@a:s">>),
    ?assertEqual(#occupant_id{id = A}, xmpp:decode(El)),
    ?assertEqual([#nick{name = <<"Ann">>}], [xmpp:decode(E) || E <- mod_matrix_gw_muc:nick_el(<<"Ann">>)]),
    ?assertEqual([], mod_matrix_gw_muc:nick_el(<<>>)),
    ?assertEqual([], mod_matrix_gw_muc:nick_el(undefined)),
    ?assertEqual({ok, <<"#lounge:example.com">>},
                 mod_matrix_gw_muc:local_alias(<<"#lounge%example.com">>, ?HOST)),
    ?assertEqual(other, mod_matrix_gw_muc:local_alias(<<"#lounge%remote.invalid">>, ?HOST)),
    ?assertEqual(other, mod_matrix_gw_muc:local_alias(<<"#%example.com">>, ?HOST)),
    ?assertEqual(other, mod_matrix_gw_muc:local_alias(<<"!abc%example.com">>, ?HOST)).

levels() ->
    L = mod_matrix_gw_muc:levels(pl()),
    ?assertMatch(#{ban := 50, kick := 50, redact := 50, invite := 0, state_default := 50,
                   events_default := 0, users_default := 0, message := 10}, L),
    %% the spec's defaults, levels written as strings, junk
    ?assertMatch(#{ban := 50, invite := 0, message := 0, users := #{}, events := #{}},
                 mod_matrix_gw_muc:levels(#{})),
    ?assertMatch(#{kick := 75, ban := 50, message := 0},
                 mod_matrix_gw_muc:levels(#{<<"kick">> => <<"75">>, <<"ban">> => <<"x">>,
                                            <<"events">> => 3, <<"users">> => 4})),
    ?assertMatch(#{ban := 50}, mod_matrix_gw_muc:levels(undefined)),
    ?assertMatch(#{ban := 50}, mod_matrix_gw_muc:levels(#{<<"ban">> => 1.5})),
    ?assert(mod_matrix_gw_muc:can_redact(L, 50)),
    ?assertNot(mod_matrix_gw_muc:can_redact(L, 49)),
    ?assertEqual(#{<<"users">> => #{?BEN_ID => 50}},
                 mod_matrix_gw_muc:set_user_level(#{}, ?BEN_ID, 50)),
    ?assertEqual(#{<<"users">> => #{}},
                 mod_matrix_gw_muc:set_user_level(#{<<"users">> => #{?BEN_ID => 50}}, ?BEN_ID, default)),
    ?assertEqual(#{<<"users">> => #{?BEN_ID => 1}},
                 mod_matrix_gw_muc:set_user_level(#{<<"users">> => junk}, ?BEN_ID, 1)),
    ?assertEqual(50, mod_matrix_gw_muc:admin_level(moderator, L)),
    ?assertEqual(50, mod_matrix_gw_muc:admin_level(admin, L)),
    ?assertEqual(100, mod_matrix_gw_muc:admin_level(owner, L)),
    ?assertEqual(76, mod_matrix_gw_muc:admin_level(admin, L#{state_default := 76})).

items() ->
    L = mod_matrix_gw_muc:levels(pl()),
    I = fun(Level, M) -> I0 = mod_matrix_gw_muc:item(L, Level, M),
                         {I0#muc_item.affiliation, I0#muc_item.role} end,
    ?assertEqual({owner, moderator}, I(100, <<"join">>)),
    ?assertEqual({owner, moderator}, I(1 bsl 53, <<"join">>)),
    ?assertEqual({admin, moderator}, I(50, <<"join">>)),
    ?assertEqual({member, participant}, I(10, <<"join">>)),
    ?assertEqual({member, visitor}, I(0, <<"join">>)),
    ?assertEqual({member, none}, I(0, <<"invite">>)),
    ?assertEqual({none, none}, I(0, <<"leave">>)),
    ?assertEqual({admin, none}, I(50, <<"leave">>)),
    ?assertEqual({outcast, none}, I(50, <<"ban">>)),
    ?assertEqual({none, none}, I(0, undefined)),
    %% a room whose default level is the kick level: nobody is a moderator
    %% for being a member
    L2 = L#{users_default := 50},
    ?assertMatch(#muc_item{affiliation = member, role = participant},
                 mod_matrix_gw_muc:item(L2, 50, <<"join">>)).

changes() ->
    C = fun(Prev, New, Sender, Key) ->
                mod_matrix_gw_muc:membership_change(
                  case Prev of undefined -> undefined; none -> #{}; _ -> #{<<"membership">> => Prev} end,
                  #{<<"membership">> => New}, Sender, Key)
        end,
    ?assertEqual(join, C(none, <<"join">>, <<"@a:s">>, <<"@a:s">>)),
    ?assertEqual(join, C(<<"invite">>, <<"join">>, <<"@a:s">>, <<"@a:s">>)),
    ?assertEqual(profile, C(<<"join">>, <<"join">>, <<"@a:s">>, <<"@a:s">>)),
    ?assertEqual(leave, C(<<"join">>, <<"leave">>, <<"@a:s">>, <<"@a:s">>)),
    ?assertEqual(kick, C(<<"join">>, <<"leave">>, <<"@m:s">>, <<"@a:s">>)),
    ?assertEqual(ban, C(<<"join">>, <<"ban">>, <<"@m:s">>, <<"@a:s">>)),
    ?assertEqual(outcast, C(none, <<"ban">>, <<"@m:s">>, <<"@a:s">>)),
    ?assertEqual(unban, C(<<"ban">>, <<"leave">>, <<"@m:s">>, <<"@a:s">>)),
    ?assertEqual(invite, C(none, <<"invite">>, <<"@m:s">>, <<"@a:s">>)),
    ?assertEqual(ignore, C(<<"invite">>, <<"invite">>, <<"@m:s">>, <<"@a:s">>)),
    ?assertEqual(ignore, C(<<"invite">>, <<"leave">>, <<"@a:s">>, <<"@a:s">>)),
    ?assertEqual(ignore, C(none, <<"knock">>, <<"@a:s">>, <<"@a:s">>)),
    %% a state before the event the room cannot tell
    ?assertEqual(join, C(undefined, <<"join">>, <<"@a:s">>, <<"@a:s">>)),
    ?assertEqual(leave, C(undefined, <<"leave">>, <<"@a:s">>, <<"@a:s">>)),
    ?assertEqual(kick, C(undefined, <<"leave">>, <<"@m:s">>, <<"@a:s">>)),
    ?assertEqual(ban, C(undefined, <<"ban">>, <<"@m:s">>, <<"@a:s">>)),
    %% the item of the unavailable presence: actor and reason
    Item = #muc_item{affiliation = member, role = participant},
    ?assertEqual(#muc_item{affiliation = member, role = none, reason = <<"spam">>,
                           actor = #muc_actor{nick = <<"m:s">>}},
                 mod_matrix_gw_muc:change_item(kick, Item, <<"@m:s">>, <<"@a:s">>,
                                               #{<<"reason">> => <<"spam">>})),
    ?assertEqual(#muc_item{affiliation = outcast, role = none},
                 mod_matrix_gw_muc:change_item(ban, Item, <<"@a:s">>, <<"@a:s">>, #{<<"reason">> => 5})),
    ?assertEqual(#muc_item{affiliation = member, role = none},
                 mod_matrix_gw_muc:change_item(leave, Item, <<"@a:s">>, <<"@a:s">>, #{})),
    ?assertEqual([110, 307], mod_matrix_gw_muc:change_codes(kick, true)),
    ?assertEqual([301], mod_matrix_gw_muc:change_codes(ban, false)),
    ?assertEqual([], mod_matrix_gw_muc:change_codes(leave, false)).

admin() ->
    L = mod_matrix_gw_muc:levels(pl()),
    A = fun(Item, Level, M) -> mod_matrix_gw_muc:admin_action(Item, L, Level, M) end,
    Kick = #muc_item{role = none, nick = <<"b:s">>, reason = <<"go">>},
    ?assertEqual({member, <<"leave">>, <<"go">>}, A(Kick, 0, <<"join">>)),
    ?assertEqual(ok, A(Kick, 0, <<"leave">>)),
    ?assertEqual({level, 50}, A(#muc_item{role = moderator}, 0, <<"join">>)),
    ?assertEqual(ok, A(#muc_item{role = moderator}, 100, <<"join">>)),
    ?assertEqual({level, default}, A(#muc_item{role = participant}, 50, <<"join">>)),
    ?assertEqual(ok, A(#muc_item{role = participant}, 0, <<"join">>)),
    ?assertEqual({error, 'feature-not-implemented'}, A(#muc_item{role = visitor}, 0, <<"join">>)),
    ?assertEqual({member, <<"ban">>, <<"r">>}, A(#muc_item{affiliation = outcast, reason = <<"r">>}, 0, <<"join">>)),
    ?assertEqual({member, <<"leave">>, <<>>}, A(#muc_item{affiliation = none}, 0, <<"ban">>)),
    ?assertEqual({level, default}, A(#muc_item{affiliation = none}, 50, <<"join">>)),
    ?assertEqual({member, <<"leave">>, <<>>}, A(#muc_item{affiliation = none}, 0, <<"invite">>)),
    %% an invited user with a level: the invite withdrawn
    ?assertEqual({member, <<"leave">>, <<>>}, A(#muc_item{affiliation = none}, 50, <<"invite">>)),
    ?assertEqual(ok, A(#muc_item{affiliation = none}, 0, <<"leave">>)),
    ?assertEqual({level, default}, A(#muc_item{affiliation = member}, 50, <<"join">>)),
    ?assertEqual(ok, A(#muc_item{affiliation = member}, 0, <<"join">>)),
    ?assertEqual(ok, A(#muc_item{affiliation = member}, 0, <<"invite">>)),
    ?assertEqual({error, 'not-acceptable'}, A(#muc_item{affiliation = member}, 0, <<"ban">>)),
    ?assertEqual({member, <<"invite">>, <<>>}, A(#muc_item{affiliation = member}, 0, <<"leave">>)),
    ?assertEqual({level, 50}, A(#muc_item{affiliation = admin}, 0, <<"join">>)),
    ?assertEqual(ok, A(#muc_item{affiliation = admin}, 50, <<"join">>)),
    ?assertEqual({level, 100}, A(#muc_item{affiliation = owner}, 50, <<"join">>)),
    ?assertEqual({error, 'bad-request'}, A(#muc_item{}, 0, <<"join">>)),
    %% the target: by JID, else by nick
    ToID = fun(#jid{luser = U}) -> {ok, <<"@", U/binary, ":x">>} end,
    ?assertEqual({ok, <<"@b:x">>}, mod_matrix_gw_muc:target_user_id(#muc_item{jid = j(<<"b@x">>)}, ToID)),
    ?assertEqual({ok, <<"@b:s">>}, mod_matrix_gw_muc:target_user_id(Kick, ToID)),
    ?assertEqual(error, mod_matrix_gw_muc:target_user_id(#muc_item{}, ToID)).

disco() ->
    I = mod_matrix_gw_muc:disco_info(#{name => <<"Lounge">>, topic => <<"T">>, join_rule => <<"invite">>,
                                       occupants => 3, moderate => true, avatar_hash => <<"abc">>,
                                       features => [<<"x:extra">>]}),
    ?assertEqual([#identity{category = <<"conference">>, type = <<"text">>, name = <<"Lounge">>}],
                 I#disco_info.identities),
    F = I#disco_info.features,
    [?assert(lists:member(X, F)) || X <- [?NS_MUC, <<"muc_membersonly">>, <<"muc_nonanonymous">>,
                                          <<"muc_persistent">>, <<"urn:xmpp:occupant-id:0">>,
                                          <<"urn:xmpp:message-moderate:1">>, <<"x:extra">>]],
    [#xdata{fields = Fields}] = I#disco_info.xdata,
    V = fun(Var) -> hd([Vs || #xdata_field{var = Var2, values = Vs} <- Fields, Var2 == Var]) end,
    ?assertEqual([<<"T">>], V(<<"muc#roominfo_description">>)),
    ?assertEqual([<<"T">>], V(<<"muc#roominfo_subject">>)),
    ?assertEqual([<<"3">>], V(<<"muc#roominfo_occupants">>)),
    ?assertEqual([<<"abc">>], V(<<"muc#roominfo_avatarhash">>)),
    %% it encodes
    ?assertMatch(#xmlel{}, xmpp:encode(I)),
    Open = mod_matrix_gw_muc:disco_info(#{join_rule => <<"public">>}),
    ?assert(lists:member(<<"muc_open">>, Open#disco_info.features)),
    ?assertNot(lists:member(<<"urn:xmpp:message-moderate:1">>, Open#disco_info.features)),
    [#xdata{fields = OFields}] = Open#disco_info.xdata,
    ?assertEqual([], [x || #xdata_field{var = <<"muc#roominfo_avatarhash">>} <- OFields]).

forms() ->
    #xdata{type = form, fields = Fields} = Form = mod_matrix_gw_muc:config_form(<<"N">>, <<"T">>),
    ?assertMatch(#xmlel{}, xmpp:encode(#muc_owner{config = Form})),
    ?assertEqual([<<"N">>], hd([Vs || #xdata_field{var = <<"muc#roomconfig_roomname">>, values = Vs} <- Fields])),
    Submit = #xdata{type = submit,
                    fields = [#xdata_field{var = <<"FORM_TYPE">>, values = [<<"x">>]},
                              #xdata_field{var = <<"muc#roomconfig_roomname">>, values = [<<"New">>]},
                              #xdata_field{var = <<"muc#roomconfig_roomdesc">>, values = []}]},
    ?assertEqual([{name, <<"New">>}, {topic, <<>>}], mod_matrix_gw_muc:config_changes(Submit)),
    ?assertEqual([], mod_matrix_gw_muc:config_changes(Submit#xdata{type = cancel})),
    ?assertEqual([], mod_matrix_gw_muc:config_changes(undefined)),
    #message{type = groupchat, sub_els = [#muc_user{status_codes = [104]}]} =
        mod_matrix_gw_muc:status_message(j(?ROOM), j(?ANN), [104]).

invites() ->
    Bob = j(<<"bob%remote.invalid@", ?GW/binary>>),
    Mediated = #message{to = j(?ROOM),
                        sub_els = [xmpp:encode(#muc_user{invites = [#muc_invite{to = Bob, reason = <<"come">>},
                                                                    #muc_invite{to = undefined}]})]},
    ?assertEqual([{Bob, <<"come">>}], mod_matrix_gw_muc:invites(Mediated)),
    ?assertEqual([], mod_matrix_gw_muc:invites(#message{})),
    Direct = #message{to = Bob, sub_els = [xmpp:encode(#x_conference{jid = j(?ROOM), reason = <<"r">>})]},
    ?assertEqual({j(?ROOM), <<"r">>}, mod_matrix_gw_muc:direct_invite(Direct)),
    ?assertEqual(none, mod_matrix_gw_muc:direct_invite(#message{})).

moderation() ->
    V1 = fxml_stream:parse_element(
           <<"<moderate xmlns='urn:xmpp:message-moderate:1' id='$ev'>"
             "<retract xmlns='urn:xmpp:message-retract:1'/><reason>spam</reason></moderate>">>),
    ?assertEqual({ok, <<"$ev">>, <<"spam">>}, mod_matrix_gw_muc:moderate_request(V1)),
    V0 = fxml_stream:parse_element(
           <<"<apply-to xmlns='urn:xmpp:fasten:0' id='$ev'><moderate xmlns='urn:xmpp:message-moderate:0'>"
             "<retract xmlns='urn:xmpp:message-retract:0'/></moderate></apply-to>">>),
    ?assertEqual({ok, <<"$ev">>, <<>>}, mod_matrix_gw_muc:moderate_request(V0)),
    Bad = [<<"<moderate xmlns='urn:xmpp:message-moderate:1' id='$ev'/>">>,
           <<"<moderate xmlns='urn:xmpp:message-moderate:0' id='$ev'/>">>,
           <<"<apply-to xmlns='urn:xmpp:fasten:0' id='$ev'><moderate xmlns='urn:xmpp:message-moderate:0'/></apply-to>">>,
           <<"<apply-to xmlns='urn:xmpp:fasten:0' id='$ev'/>">>,
           <<"<other/>">>],
    [?assertEqual(error, mod_matrix_gw_muc:moderate_request(fxml_stream:parse_element(B))) || B <- Bad],
    ?assertEqual(error, mod_matrix_gw_muc:moderate_request(nothing)).

retractions() ->
    {Body, Els} = mod_matrix_gw_muc:retraction(<<"$t">>, undefined, [], <<>>),
    ?assertEqual(<<"Message retracted">>, Body),
    Msg = #message{type = groupchat, body = [#text{data = Body}], sub_els = Els},
    #xmlel{} = Enc = xmpp:encode(Msg),
    R = fxml:get_subtag_with_xmlns(Enc, <<"retract">>, <<"urn:xmpp:message-retract:1">>),
    ?assertEqual(<<"$t">>, fxml:get_tag_attr_s(<<"id">>, R)),
    ?assertEqual(false, fxml:get_subtag(R, <<"moderated">>)),
    ?assertNotEqual(false, fxml:get_subtag_with_xmlns(Enc, <<"fallback">>, <<"urn:xmpp:fallback:0">>)),
    By = j(<<?ROOM/binary, "/ann:example.com">>),
    Occ = mod_matrix_gw_muc:occupant_id_el(<<"!r:s">>, ?ANN_ID),
    {Body2, Els2} = mod_matrix_gw_muc:retraction(<<"$t">>, By, [Occ], <<"spam">>),
    ?assertEqual(<<"This message has been moderated: spam">>, Body2),
    Enc2 = xmpp:encode(#message{type = groupchat, body = [#text{data = Body2}], sub_els = Els2}),
    R2 = fxml:get_subtag_with_xmlns(Enc2, <<"retract">>, <<"urn:xmpp:message-retract:1">>),
    M2 = fxml:get_subtag_with_xmlns(R2, <<"moderated">>, <<"urn:xmpp:message-moderate:1">>),
    ?assertEqual(jid:encode(By), fxml:get_tag_attr_s(<<"by">>, M2)),
    ?assertNotEqual(false, fxml:get_subtag_with_xmlns(M2, <<"occupant-id">>, <<"urn:xmpp:occupant-id:0">>)),
    ?assertEqual(<<"spam">>, fxml:get_subtag_cdata(R2, <<"reason">>)),
    A = fxml:get_subtag_with_xmlns(Enc2, <<"apply-to">>, <<"urn:xmpp:fasten:0">>),
    ?assertNotEqual(false, fxml:get_subtag_with_xmlns(A, <<"moderated">>, <<"urn:xmpp:message-moderate:0">>)),
    {Body3, _} = mod_matrix_gw_muc:retraction(<<"$t">>, By, [], <<>>),
    ?assertEqual(<<"This message has been moderated">>, Body3).

creation() ->
    Events = mod_matrix_gw_muc:create_events(?ANN_ID, <<"#lounge:example.com">>, <<"10">>, <<"Lounge">>),
    Types = [maps:get(<<"type">>, E) || E <- Events],
    ?assertEqual([<<"m.room.create">>, <<"m.room.member">>, <<"m.room.power_levels">>,
                  <<"m.room.join_rules">>, <<"m.room.history_visibility">>, <<"m.room.guest_access">>,
                  <<"m.room.canonical_alias">>, <<"m.room.name">>], Types),
    [Create, Join, PL, JR | _] = Events,
    ?assertMatch(#{<<"content">> := #{<<"creator">> := ?ANN_ID, <<"room_version">> := <<"10">>}}, Create),
    ?assertMatch(#{<<"state_key">> := ?ANN_ID, <<"content">> := #{<<"membership">> := <<"join">>}}, Join),
    ?assertMatch(#{<<"content">> := #{<<"users">> := #{?ANN_ID := 100}}}, PL),
    ?assertMatch(#{<<"content">> := #{<<"join_rule">> := <<"invite">>}}, JR),
    ?assertEqual(7, length(mod_matrix_gw_muc:create_events(?ANN_ID, <<"#a:b">>, <<"10">>, <<>>))),
    ?assert(lists:member({<<"m.room.create">>, <<"">>}, mod_matrix_gw_muc:stripped_state_keys())).

history() ->
    Now = 1000000000,
    H = fun(X) -> mod_matrix_gw_muc:history_request(X, Now) end,
    ?assertEqual({20, 0}, H(undefined)),
    ?assertEqual({20, 0}, H(#muc_history{})),
    ?assertEqual({0, 0}, H(#muc_history{maxchars = 0})),
    ?assertEqual({5, 0}, H(#muc_history{maxstanzas = 5})),
    ?assertEqual({100, 0}, H(#muc_history{maxstanzas = 500})),
    ?assertEqual({100, Now - 60000}, H(#muc_history{seconds = 60})),
    ?assertEqual({3, 123000}, H(#muc_history{maxstanzas = 3, since = misc:usec_to_now(123000000)})).

vcard() ->
    ?assertEqual(#vcard_temp{fn = <<"N">>}, mod_matrix_gw_muc:room_vcard(<<"N">>, undefined, undefined)),
    ?assertMatch(#vcard_temp{photo = #vcard_photo{type = <<"image/png">>, binval = <<"png">>}},
                 mod_matrix_gw_muc:room_vcard(<<"N">>, <<"image/png">>, <<"png">>)),
    ?assertEqual(#vcard_xupdate{hash = <<"abc">>}, xmpp:decode(mod_matrix_gw_muc:avatar_update_el(<<"abc">>))),
    ?assertMatch(#vcard_xupdate{}, xmpp:decode(mod_matrix_gw_muc:avatar_update_el(undefined))).

%%--------------------------------------------------------------------
%% A room on a running gateway
%%--------------------------------------------------------------------
ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup, fun() -> mod_matrix_gw_test_lib:start_ejabberd(?MODULE) end, fun mod_matrix_gw_test_lib:stop_ejabberd/1,
     fun(_) ->
             [{timeout, 60, {"a whole room, made from XMPP", fun room/0}},
              {timeout, 10, {"the alias table", fun aliases/0}}]
     end}.

session(Full) ->
    #jid{luser = U, lserver = S, lresource = R} = j(Full),
    ejabberd_sm:open_session({erlang:timestamp(), self()}, U, S, R, 0, []).

%% What reached this process within a moment (every session is this
%% process; each stanza says whom it is for).
received() ->
    receive
        {route, Pkt} -> [Pkt | received()]
    after 500 -> []
    end.

for(To, Pkts) ->
    [P || P <- Pkts, jid:encode(xmpp:get_to(P)) == To].

route(Pkt) ->
    ok = mod_matrix_gw_room:route(Pkt),
    received().

occ(Nick) -> j(<<?ROOM/binary, "/", Nick/binary>>).

join(Full, Nick, Els) ->
    route(#presence{from = j(Full), to = occ(Nick), type = available,
                    sub_els = [xmpp:encode(#muc{history = Els})]}).

iq(Full, Type, El) ->
    Got = route(#iq{id = p1_rand:get_string(), type = Type, from = j(Full), to = j(?ROOM),
                    sub_els = [El]}),
    [IQ] = [I || #iq{} = I <- for(Full, Got)],
    IQ.

dec(#xmlel{} = El) -> xmpp:decode(El);
dec(El) -> El.

item_of(#presence{} = P) ->
    #muc_user{items = [Item]} = xmpp:get_subtag(P, #muc_user{}),
    Item.

codes(P) ->
    (xmpp:get_subtag(P, #muc_user{}))#muc_user.status_codes.

presences_from(Nick, Pkts) ->
    From = occ(Nick),
    [P || #presence{from = F} = P <- Pkts, jid:tolower(F) == jid:tolower(From)].

room() ->
    session(?ANN), session(?BEN), session(?ANN2),
    %% ann makes the room by joining it
    Got1 = join(?ANN, <<"ann">>, undefined),
    [Self] = presences_from(<<"ann:example.com">>, for(?ANN, Got1)),
    %% the room's nick is not the one asked for: 210
    ?assertEqual([110, 201, 210], lists:sort(codes(Self))),
    ?assertMatch(#muc_item{affiliation = owner, role = moderator}, item_of(Self)),
    ?assertEqual(jid:remove_resource(j(?ANN)), (item_of(Self))#muc_item.jid),
    ?assertMatch(#occupant_id{}, xmpp:get_subtag(Self, #occupant_id{})),
    ?assertMatch([#message{subject = [_]}], [M || #message{subject = [_]} = M <- for(?ANN, Got1)]),
    {ok, RoomID} = mod_matrix_gw_muc:alias_lookup(<<"#lounge:example.com">>),
    ?assertEqual({ok, RoomID, [?HOST]}, mod_matrix_gw_room:directory(?HOST, <<"#lounge:example.com">>)),
    ?assertEqual(error, mod_matrix_gw_room:directory(?HOST, <<"#nothing:example.com">>)),
    %% ben is not invited: his join is refused
    Got2 = join(?BEN, <<"ben">>, undefined),
    ?assertMatch([#presence{type = error}], for(?BEN, Got2)),
    ?assertMatch(#stanza_error{reason = 'registration-required'}, xmpp:get_error(hd(for(?BEN, Got2)))),
    %% ann invites him (mediated); he gets the invite and comes in
    Got3 = route(#message{from = j(?ANN), to = j(?ROOM),
                          sub_els = [#muc_user{invites = [#muc_invite{to = j(<<"ben@example.com">>)}]}]}),
    [Inv] = [M || #message{} = M <- Got3, jid:remove_resource(xmpp:get_to(M)) == j(<<"ben@example.com">>)],
    #muc_user{invites = [#muc_invite{from = InvFrom}]} = xmpp:get_subtag(Inv, #muc_user{}),
    ?assertEqual(jid:remove_resource(j(?ANN)), InvFrom),
    Got4 = join(?BEN, <<"ben">>, undefined),
    [BenSelf] = presences_from(<<"ben:example.com">>, for(?BEN, Got4)),
    ?assertMatch(#muc_item{affiliation = member, role = participant}, item_of(BenSelf)),
    ?assertEqual([110, 210], codes(BenSelf)),
    ?assertMatch([_], presences_from(<<"ann:example.com">>, for(?BEN, Got4))),
    ?assertMatch([_], presences_from(<<"ben:example.com">>, for(?ANN, Got4))),
    %% the member list is the members', the other lists the
    %% moderators'; the configuration form the members'; nothing of it, nor
    %% the vCard, for another server (disco#info is anyone's)
    Cy = <<"cy@example.com/pc">>,
    session(Cy),
    Forbidden = fun(#iq{type = error} = E) -> (xmpp:get_error(E))#stanza_error.reason end,
    ?assertEqual(forbidden, Forbidden(iq(?BEN, get, #muc_admin{items = [#muc_item{affiliation = outcast}]}))),
    ?assertEqual(forbidden, Forbidden(iq(?BEN, get, #muc_admin{items = [#muc_item{role = moderator}]}))),
    ?assertMatch(#iq{type = result}, iq(?BEN, get, #muc_admin{items = [#muc_item{affiliation = member}]})),
    ?assertMatch(#iq{type = result}, iq(?BEN, get, #muc_owner{})),
    ?assertEqual(forbidden, Forbidden(iq(Cy, get, #muc_admin{items = [#muc_item{affiliation = member}]}))),
    ?assertEqual(forbidden, Forbidden(iq(Cy, get, #muc_owner{}))),
    ok = ejabberd_router:register_route(<<"other.example">>, ?HOST),
    X = <<"x@other.example/r">>,
    [?assertEqual(forbidden, Forbidden(iq(X, get, El)))
     || El <- [#muc_admin{items = [#muc_item{affiliation = member}]}, #muc_owner{}, #vcard_temp{}]],
    ?assertMatch(#iq{type = result}, iq(X, get, #disco_info{})),
    ejabberd_router:unregister_route(<<"other.example">>),
    %% the subject: ann may set it, ben may not
    Got5 = route(#message{from = j(?ANN), to = j(?ROOM), type = groupchat,
                          subject = [#text{data = <<"New topic">>}]}),
    ?assertMatch([#message{subject = [#text{data = <<"New topic">>}]}],
                 [M || #message{subject = [_]} = M <- for(?BEN, Got5)]),
    Got6 = route(#message{from = j(?BEN), to = j(?ROOM), type = groupchat,
                          subject = [#text{data = <<"Mine">>}]}),
    ?assertMatch([#message{type = error}], for(?BEN, Got6)),
    %% the configuration form: name and description
    #iq{type = result, sub_els = [OwnerEl]} = iq(?ANN, get, #muc_owner{}),
    #muc_owner{config = #xdata{}} = dec(OwnerEl),
    Submit = #xdata{type = submit,
                    fields = [#xdata_field{var = <<"muc#roomconfig_roomname">>, values = [<<"Lounge">>]},
                              #xdata_field{var = <<"muc#roomconfig_roomdesc">>, values = [<<"New topic">>]}]},
    Got7 = route(#iq{id = <<"c1">>, type = set, from = j(?ANN), to = j(?ROOM),
                     sub_els = [#muc_owner{config = Submit}]}),
    ?assertMatch([#iq{type = result}], [I || #iq{} = I <- for(?ANN, Got7)]),
    ?assertMatch([_], [M || #message{} = M <- for(?BEN, Got7),
                            (xmpp:get_subtag(M, #muc_user{}))#muc_user.status_codes == [104]]),
    ?assertMatch(#iq{type = error}, iq(?BEN, set, #muc_owner{config = Submit#xdata{
                                                                   fields = [#xdata_field{var = <<"muc#roomconfig_roomname">>,
                                                                                          values = [<<"Ben's">>]}]}})),
    ?assertMatch(#iq{type = result}, iq(?BEN, set, #muc_owner{config = #xdata{type = cancel}})),
    %% disco#info: the name, and moderation for ann only
    Info = fun(Full) -> #iq{type = result, sub_els = [E]} = iq(Full, get, #disco_info{}),
                        dec(E) end,
    #disco_info{identities = [#identity{name = <<"Lounge">>}], features = AnnF} = Info(?ANN),
    ?assert(lists:member(<<"urn:xmpp:message-moderate:1">>, AnnF)),
    ?assert(lists:member(<<"muc_membersonly">>, AnnF)),
    #disco_info{features = BenF} = Info(?BEN),
    ?assertNot(lists:member(<<"urn:xmpp:message-moderate:1">>, BenF)),
    ?assertMatch(#iq{type = result}, iq(?ANN, get, #disco_items{})),
    %% ann makes ben a moderator, and takes it back
    Got8 = route(#iq{id = <<"a1">>, type = set, from = j(?ANN), to = j(?ROOM),
                     sub_els = [#muc_admin{items = [#muc_item{nick = <<"ben:example.com">>, role = moderator}]}]}),
    ?assertMatch([#iq{type = result}], [I || #iq{} = I <- for(?ANN, Got8)]),
    [BenMod | _] = presences_from(<<"ben:example.com">>, for(?ANN, Got8)),
    ?assertMatch(#muc_item{role = moderator}, item_of(BenMod)),
    #iq{type = result, sub_els = [ModsEl]} = iq(?BEN, get, #muc_admin{items = [#muc_item{role = moderator}]}),
    #muc_admin{items = Mods} = dec(ModsEl),
    ?assertEqual([<<"ann:example.com">>, <<"ben:example.com">>], lists:sort([N || #muc_item{nick = N} <- Mods])),
    Got9 = route(#iq{id = <<"a2">>, type = set, from = j(?ANN), to = j(?ROOM),
                     sub_els = [#muc_admin{items = [#muc_item{nick = <<"ben:example.com">>, role = participant}]}]}),
    [BenPart | _] = presences_from(<<"ben:example.com">>, for(?ANN, Got9)),
    ?assertMatch(#muc_item{role = participant}, item_of(BenPart)),
    %% ben may not kick ann
    ?assertMatch(#iq{type = error}, iq(?BEN, set, #muc_admin{items = [#muc_item{nick = <<"ann:example.com">>, role = none}]})),
    %% ben writes, ann moderates his message; the room says so
    Got10 = route(#message{id = <<"b1">>, from = j(?BEN), to = j(?ROOM), type = groupchat,
                           body = [#text{data = <<"spam">>}]}),
    [BenMsg] = [M || #message{body = [_]} = M <- for(?ANN, Got10)],
    #stanza_id{id = BenEvent} = xmpp:get_subtag(BenMsg, #stanza_id{}),
    ?assertMatch(#occupant_id{}, xmpp:get_subtag(BenMsg, #occupant_id{})),
    Moderate = fxml_stream:parse_element(
                 <<"<moderate xmlns='urn:xmpp:message-moderate:1' id='", BenEvent/binary, "'>"
                   "<retract xmlns='urn:xmpp:message-retract:1'/><reason>no</reason></moderate>">>),
    Got11 = route(#iq{id = <<"m1">>, type = set, from = j(?ANN), to = j(?ROOM), sub_els = [Moderate]}),
    ?assertMatch([#iq{type = result}], [I || #iq{} = I <- for(?ANN, Got11)]),
    [Retracted] = [M || #message{type = groupchat} = M <- for(?BEN, Got11)],
    ?assertEqual(j(?ROOM), Retracted#message.from),
    ?assertEqual(<<"This message has been moderated: no">>, xmpp:get_text(Retracted#message.body)),
    %% ben retracts his own; he may not moderate ann's
    Got12 = route(#message{id = <<"b2">>, from = j(?BEN), to = j(?ROOM), type = groupchat,
                           body = [#text{data = <<"oops">>}]}),
    [BenMsg2] = [M || #message{body = [_]} = M <- for(?ANN, Got12)],
    #stanza_id{id = BenEvent2} = xmpp:get_subtag(BenMsg2, #stanza_id{}),
    Own = fxml_stream:parse_element(
            <<"<moderate xmlns='urn:xmpp:message-moderate:1' id='", BenEvent2/binary, "'>"
              "<retract xmlns='urn:xmpp:message-retract:1'/></moderate>">>),
    Got13 = route(#iq{id = <<"m2">>, type = set, from = j(?BEN), to = j(?ROOM), sub_els = [Own]}),
    [OwnRetract] = [M || #message{type = groupchat} = M <- for(?ANN, Got13)],
    ?assertEqual(occ(<<"ben:example.com">>), OwnRetract#message.from),
    ?assertEqual(<<"Message retracted">>, xmpp:get_text(OwnRetract#message.body)),
    Got14 = route(#message{id = <<"a3">>, from = j(?ANN), to = j(?ROOM), type = groupchat,
                           body = [#text{data = <<"mine">>}]}),
    [AnnMsg] = [M || #message{body = [_]} = M <- for(?BEN, Got14)],
    #stanza_id{id = AnnEvent} = xmpp:get_subtag(AnnMsg, #stanza_id{}),
    NotHis = fxml_stream:parse_element(
               <<"<moderate xmlns='urn:xmpp:message-moderate:1' id='", AnnEvent/binary, "'>"
                 "<retract xmlns='urn:xmpp:message-retract:1'/></moderate>">>),
    ?assertMatch(#iq{type = error}, iq(?BEN, set, NotHis)),
    Unknown = fxml_stream:parse_element(
                <<"<moderate xmlns='urn:xmpp:message-moderate:1' id='nothing'>"
                  "<retract xmlns='urn:xmpp:message-retract:1'/></moderate>">>),
    ?assertMatch(#iq{type = error}, iq(?ANN, set, Unknown)),
    %% the vCard (no avatar)
    #iq{type = result, sub_els = [VC]} = iq(?ANN, get, #vcard_temp{}),
    ?assertMatch(#vcard_temp{fn = <<"Lounge">>, photo = undefined}, dec(VC)),
    %% a second resource of ann's: the room's last messages, 2 asked; the
    %% redacted ones (spam, oops) left the gateway's history with the
    %% redaction (mod_matrix_gw_event's pruning, as the spec has a server
    %% prune its copy; the clients that had them keep them)
    Got15 = join(?ANN2, <<"ann">>, #muc_history{maxstanzas = 2}),
    History = [M || #message{body = [_]} = M <- for(?ANN2, Got15)],
    ?assertEqual([<<"mine">>], [xmpp:get_text(B) || #message{body = B} <- History]),
    ?assertMatch(#occupant_id{}, xmpp:get_subtag(hd(History), #occupant_id{})),
    %% ann kicks ben: he is out (307, with the actor)
    Got16 = route(#iq{id = <<"k1">>, type = set, from = j(?ANN), to = j(?ROOM),
                      sub_els = [#muc_admin{items = [#muc_item{nick = <<"ben:example.com">>, role = none,
                                                               reason = <<"bye">>}]}]}),
    [Kicked] = presences_from(<<"ben:example.com">>, for(?BEN, Got16)),
    ?assertEqual(unavailable, Kicked#presence.type),
    ?assertEqual([110, 307], codes(Kicked)),
    ?assertMatch(#muc_item{actor = #muc_actor{nick = <<"ann:example.com">>}, reason = <<"bye">>},
                 item_of(Kicked)),
    ?assertMatch([#presence{type = unavailable}], presences_from(<<"ben:example.com">>, for(?ANN, Got16))),
    %% his client still shows the room; what he writes
    %% bounces, not-acceptable (no occupant), and reaches nobody
    Got16b = route(#message{id = <<"b3">>, from = j(?BEN), to = j(?ROOM), type = groupchat,
                            body = [#text{data = <<"are you there?">>}]}),
    [Bounce] = for(?BEN, Got16b),
    ?assertMatch(#message{type = error, id = <<"b3">>}, Bounce),
    ?assertMatch(#stanza_error{reason = 'not-acceptable'}, xmpp:get_error(Bounce)),
    ?assertEqual([], for(?ANN, Got16b)),
    %% and cannot come back uninvited; then banned, listed, unbanned
    ?assertMatch([#presence{type = error}], for(?BEN, join(?BEN, <<"ben">>, undefined))),
    ?assertMatch(#iq{type = result},
                 iq(?ANN, set, #muc_admin{items = [#muc_item{jid = j(<<"ben@example.com">>), affiliation = outcast}]})),
    #iq{type = result, sub_els = [OutEl]} = iq(?ANN, get, #muc_admin{items = [#muc_item{affiliation = outcast}]}),
    ?assertMatch(#muc_admin{items = [#muc_item{jid = #jid{luser = <<"ben">>}}]}, dec(OutEl)),
    Banned = join(?BEN, <<"ben">>, undefined),
    ?assertMatch(#stanza_error{reason = forbidden}, xmpp:get_error(hd(for(?BEN, Banned)))),
    ?assertMatch(#iq{type = result},
                 iq(?ANN, set, #muc_admin{items = [#muc_item{jid = j(<<"ben@example.com">>), affiliation = none}]})),
    #iq{type = result, sub_els = [OutEl2]} = iq(?ANN, get, #muc_admin{items = [#muc_item{affiliation = outcast}]}),
    ?assertMatch(#muc_admin{items = []}, dec(OutEl2)),
    %% a direct invite (XEP-0249) of a Matrix user: an invite in the room
    %% (his server resolves nowhere; the room goes on)
    Bob = <<"bob%remote.invalid@", ?GW/binary>>,
    route(#message{from = j(?ANN), to = j(Bob), sub_els = [#x_conference{jid = j(?ROOM)}]}),
    #iq{type = result, sub_els = [MemEl]} = iq(?ANN, get, #muc_admin{items = [#muc_item{affiliation = member}]}),
    #muc_admin{items = Members} = dec(MemEl),
    ?assert(lists:member(j(Bob), [J || #muc_item{jid = J} <- Members])),
    %% what the room does not do
    ?assertMatch(#iq{type = error}, iq(?ANN, get, #muc_owner{destroy = #muc_destroy{}})),
    ?assertMatch(#iq{type = error}, iq(?ANN, set, #muc_admin{items = [#muc_item{nick = <<"x">>, role = none}]})),
    %% a private message to an occupant is feature-not-implemented
    %% an invite to a room the gateway is not in is
    %% item-not-found
    [PM] = route(#message{from = j(?ANN), to = occ(<<"ben:example.com">>), type = chat,
                          body = [#text{data = <<"psst">>}]}),
    ?assertMatch(#stanza_error{reason = 'feature-not-implemented'}, xmpp:get_error(PM)),
    ?assertEqual([], route(#message{from = j(?ANN), to = occ(<<"ben:example.com">>), type = chat,
                                    sub_els = [#chatstate{type = composing}]})),
    %% L14: a reaction or marker naming nothing here: item-not-found
    [NoTarget] = for(?ANN, route(#message{id = <<"r9">>, from = j(?ANN), to = j(?ROOM), type = groupchat,
                                          sub_els = [mod_matrix_gw_xmpp:reactions_el(<<"nothing">>, [<<"+1">>])]})),
    ?assertMatch(#stanza_error{reason = 'item-not-found'}, xmpp:get_error(NoTarget)),
    ?assertMatch([#message{type = error}],
                 for(?ANN, route(#message{id = <<"r10">>, from = j(?ANN), to = j(?ROOM), type = groupchat,
                                          sub_els = [mod_matrix_gw_xmpp:displayed_el(<<"nothing">>)]}))),
    %% L14: an iq with no child, or two, to a Matrix user: bad-request
    [Bad] = route(#iq{id = <<"q0">>, type = get, from = j(?ANN), to = j(Bob), sub_els = []}),
    ?assertMatch(#stanza_error{reason = 'bad-request'}, xmpp:get_error(Bad)),
    [Bad2] = route(#iq{id = <<"q2">>, type = get, from = j(?ANN), to = j(Bob),
                       sub_els = [#disco_info{}, #disco_items{}]}),
    ?assertMatch(#stanza_error{reason = 'bad-request'}, xmpp:get_error(Bad2)),
    ?assertMatch([#message{type = error}],
                 route(#message{from = j(?ANN), to = j(<<"#nowhere%example.com@", ?GW/binary>>),
                                sub_els = [#muc_user{invites = [#muc_invite{to = j(Bob)}]}]})).

aliases() ->
    ok = mod_matrix_gw_muc:alias_set(<<"#a:example.com">>, <<"!x:example.com">>),
    ok = mod_matrix_gw_muc:alias_set(<<"#b:example.com">>, <<"!x:example.com">>),
    ?assertEqual({ok, <<"!x:example.com">>}, mod_matrix_gw_muc:alias_lookup(<<"#a:example.com">>)),
    ok = mod_matrix_gw_muc:alias_forget(<<"!x:example.com">>),
    ?assertEqual(error, mod_matrix_gw_muc:alias_lookup(<<"#a:example.com">>)),
    ?assertEqual(error, mod_matrix_gw_muc:alias_lookup(<<"#b:example.com">>)).
