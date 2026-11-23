%%% mod_matrix_gw_event's tests: the wire's pieces on their own (the
%%% <event/> parsed and built, the forwarded set, the retraction, the room
%%% iqs, <room-info/>), then the whole path on an ejabberd started in this
%%% node with the gateway on: an encrypted event into a new direct chat and
%%% its <sent/>, <event-id/>, <room-info/>, <encryption/>, <redact/> with
%%% the target pruned, the not-encrypted refusals, the peer's events
%%% forwarded (encrypted, redaction with its retraction, membership, a plain
%%% text's empty <event/>), a room's (a stored room brought back as a room
%%% ann joins), and matrix_sent's bound. The
%%% remote server here, remote.invalid, resolves nowhere.
-module(mod_matrix_gw_event_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(NS, <<"https://ns.another.im/matrix-e2e/0">>).
-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).
-define(ANN, <<"ann@example.com">>).
-define(ANN_RES, <<"ann@example.com/phone">>).
-define(ANN_ID, <<"@ann:example.com">>).
-define(MEGOLM, <<"m.megolm.v1.aes-sha2">>).

j(S) -> jid:decode(S).

el(Name, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, ?NS} | Attrs], children = Children}.

event_el(Type, Json) ->
    el(<<"event">>, [{<<"type">>, Type}], [{xmlcdata, Json}]).

msg(From, To, Type, Json) ->
    #message{id = <<"x1">>, type = chat, from = j(From), to = j(To),
             sub_els = [event_el(Type, Json)]}.

json(Map) -> iolist_to_binary(json:encode(Map)).

megolm() ->
    #{<<"algorithm">> => ?MEGOLM, <<"sender_key">> => <<"sk">>, <<"device_id">> => <<"DEV">>,
      <<"session_id">> => <<"sid">>, <<"ciphertext">> => <<"ct">>}.

fields(Type, Content) ->
    #{type => Type, id => <<"$ev">>, room => <<"!r:remote.invalid">>,
      sender => <<"@bob:remote.invalid">>, ts => 1761390000000, content => Content}.

attr(Name, El) -> fxml:get_tag_attr_s(Name, El).

names(Els) -> [N || #xmlel{name = N} <- Els].

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------
pure_test_() ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(xmpp) end,
     [fun parse/0, fun types/0, fun forwarded_set/0, fun elements/0,
      fun forward_encrypted/0, fun forward_redaction/0, fun forward_others/0,
      fun retraction/0, fun room_iqs/0, fun room_info/0, fun answers/0, fun disco/0,
      fun membership_without_e2e/0]}.

parse() ->
    To = <<"bob%remote.invalid@", ?GW/binary>>,
    P = fun(M) -> mod_matrix_gw_event:parse_message(M, ?HOST) end,
    M = msg(?ANN_RES, To, <<"m.room.encrypted">>, json(megolm())),
    ?assertEqual({event, <<"m.room.encrypted">>, megolm()}, P(M)),
    ?assertEqual(pass, P(M#message{type = error})),
    ?assertEqual(pass, P(M#message{sub_els = []})),
    ?assertEqual(ignore, P(M#message{from = j(<<"eve@elsewhere.example/x">>)})),
    ?assertMatch({error, #stanza_error{reason = 'bad-request'}},
                 P(msg(?ANN_RES, To, <<"m.reaction">>, <<"[1]">>))),
    ?assertMatch({error, #stanza_error{reason = 'bad-request'}},
                 P(msg(?ANN_RES, To, <<"m.reaction">>, <<"not json">>))),
    ?assertMatch({error, #stanza_error{reason = 'not-acceptable'}},
                 P(msg(?ANN_RES, To, <<"m.room.member">>, <<"{}">>))),
    ?assertMatch({error, #stanza_error{reason = 'not-acceptable'}},
                 P(msg(?ANN_RES, To, <<>>, <<"{}">>))).

types() ->
    A = fun mod_matrix_gw_event:acceptable_type/1,
    ?assert(A(<<"m.room.encrypted">>)),
    ?assert(A(<<"m.room.message">>)),
    ?assert(A(<<"m.call.invite">>)),
    ?assert(A(<<"m.reaction">>)),
    ?assert(A(<<"m.key.verification.start">>)),
    ?assertNot(A(<<"m.room.encryption">>)),
    ?assertNot(A(<<"m.room.redaction">>)),
    ?assertNot(A(<<>>)),
    ?assertNot(A(binary:copy(<<"x">>, 256))),
    ?assertNot(A(undefined)),
    C = fun mod_matrix_gw_event:content_action/1,
    ?assert(C({text, <<"hi">>})),
    ?assert(C({media, <<"u">>, #{}, <<>>})),
    ?assert(C({edit, <<"x">>, <<"hi">>})),
    ?assert(C({reactions, <<"x">>, []})),
    ?assertNot(C({typing, true})),
    ?assertNot(C({displayed, <<"x">>})).

forwarded_set() ->
    F = fun mod_matrix_gw_event:forwarded/3,
    ?assertEqual(forward, F(<<"m.room.encrypted">>, #{}, direct)),
    ?assertEqual(forward, F(<<"m.room.encrypted">>, #{}, multi)),
    ?assertEqual(redaction, F(<<"m.room.redaction">>, #{}, multi)),
    ?assertEqual(forward, F(<<"m.room.encryption">>, #{}, multi)),
    ?assertEqual(forward, F(<<"m.room.member">>, #{}, direct)),
    ?assertEqual(pass, F(<<"m.room.member">>, #{}, multi)),
    ?assertEqual(forward, F(<<"m.room.message">>,
                            #{<<"msgtype">> => <<"m.key.verification.request">>}, direct)),
    ?assertEqual(pass, F(<<"m.room.message">>, #{<<"msgtype">> => <<"m.text">>}, direct)),
    ?assertEqual(forward, F(<<"m.key.verification.mac">>, #{}, direct)),
    ?assertEqual(pass, F(<<"m.call.invite">>, #{}, direct)),
    ?assertEqual(pass, F(<<"m.reaction">>, #{}, multi)),
    ?assertEqual(pass, F(<<"org.example.unknown">>, #{}, multi)).

elements() ->
    Ev = mod_matrix_gw_event:event_el((fields(<<"m.room.encrypted">>, #{}))#{redacts => <<"$t">>,
                                                                            state_key => <<"@s:x">>},
                                      #{<<"a">> => 1}),
    ?assertEqual([{<<"xmlns">>, ?NS}, {<<"type">>, <<"m.room.encrypted">>}, {<<"id">>, <<"$ev">>},
                  {<<"room">>, <<"!r:remote.invalid">>}, {<<"sender">>, <<"@bob:remote.invalid">>},
                  {<<"ts">>, <<"1761390000000">>}, {<<"redacts">>, <<"$t">>},
                  {<<"state_key">>, <<"@s:x">>}], Ev#xmlel.attrs),
    ?assertEqual(#{<<"a">> => 1}, json:decode(fxml:get_tag_cdata(Ev))),
    Plain = mod_matrix_gw_event:plain_event_el(<<"$p">>, <<"!r:s">>, <<"@b:s">>, 5),
    ?assertEqual([], Plain#xmlel.children),
    ?assertEqual(<<"m.room.message">>, attr(<<"type">>, Plain)),
    ?assertEqual(<<"5">>, attr(<<"ts">>, Plain)),
    NoTS = mod_matrix_gw_event:event_el((fields(<<"m.room.encrypted">>, #{}))#{ts => undefined}, none),
    ?assertEqual(<<>>, attr(<<"ts">>, NoTS)),
    ?assertEqual(#{<<"algorithm">> => ?MEGOLM, <<"rotation_period_ms">> => 604800000,
                   <<"rotation_period_msgs">> => 100},
                 mod_matrix_gw_event:encryption_content()),
    ?assertMatch(#{<<"type">> := <<"m.room.encryption">>, <<"state_key">> := <<>>,
                   <<"sender">> := ?ANN_ID}, mod_matrix_gw_event:encryption_event(?ANN_ID)).

base() ->
    #message{id = <<"$ev">>, from = j(<<"bob%remote.invalid@", ?GW/binary>>), to = j(?ANN),
             type = chat}.

%% §2.5's shape: the fallback body, EME, the event, the store hint, then
%% upstream's <x/>.
forward_encrypted() ->
    M = mod_matrix_gw_event:forward_message(base(), fields(<<"m.room.encrypted">>, megolm()),
                                            #{}, undefined),
    ?assertEqual([#text{data = <<"Encrypted message (Matrix)">>}], M#message.body),
    [EME, Ev, Store, X] = M#message.sub_els,
    ?assertEqual(<<"encryption">>, EME#xmlel.name),
    ?assertEqual([{<<"xmlns">>, <<"urn:xmpp:eme:0">>}, {<<"namespace">>, ?NS},
                  {<<"name">>, <<"Matrix">>}], EME#xmlel.attrs),
    ?assertEqual(megolm(), json:decode(fxml:get_tag_cdata(Ev))),
    ?assertEqual(<<"store">>, Store#xmlel.name),
    ?assertEqual(<<"!r:remote.invalid">>, attr(<<"room_id">>, X)),
    ?assertMatch(#xmlel{name = <<"message">>}, xmpp:encode(M)).

forward_redaction() ->
    F = (fields(<<"m.room.redaction">>, #{<<"reason">> => <<"oops">>}))#{redacts => <<"$t">>},
    %% the retraction of the target as the XMPP side knows it
    M = mod_matrix_gw_event:forward_message(base(), F, #{retract => <<"xmpp-t">>}, undefined),
    ?assertEqual([#text{data = <<"Message retracted">>}], M#message.body),
    ?assertEqual([<<"event">>, <<"retract">>, <<"fallback">>, <<"store">>, <<"x">>],
                 names(M#message.sub_els)),
    [Ev, R, Fb | _] = M#message.sub_els,
    ?assertEqual(<<"$t">>, attr(<<"redacts">>, Ev)),
    ?assertEqual(<<"xmpp-t">>, attr(<<"id">>, R)),
    ?assertEqual(<<"urn:xmpp:message-retract:1">>, attr(<<"xmlns">>, R)),
    ?assertEqual([], R#xmlel.children),
    ?assertEqual(<<"urn:xmpp:message-retract:1">>, attr(<<"for">>, Fb)),
    %% by a moderator: XEP-0425's <moderated/> and the reason
    By = j(<<"!r%remote.invalid@", ?GW/binary, "/mod:remote.invalid">>),
    M2 = mod_matrix_gw_event:forward_message(base(), F, #{retract => <<"$t">>}, By),
    [_, R2 | _] = M2#message.sub_els,
    ?assertEqual([<<"moderated">>, <<"reason">>], names(R2#xmlel.children)),
    %% a reaction taken back: the event alone, no retraction
    M3 = mod_matrix_gw_event:forward_message(base(), F, #{}, undefined),
    ?assertEqual([], M3#message.body),
    ?assertEqual([<<"event">>, <<"store">>, <<"x">>], names(M3#message.sub_els)).

forward_others() ->
    %% a DM's membership: its state_key into the JSON, where the device reads it
    M = mod_matrix_gw_event:forward_message(
          base(), (fields(<<"m.room.member">>, #{<<"membership">> => <<"join">>}))#{state_key => <<"@bob:remote.invalid">>},
          #{}, undefined),
    [Ev, _Store, _X] = M#message.sub_els,
    ?assertEqual(#{<<"membership">> => <<"join">>, <<"state_key">> => <<"@bob:remote.invalid">>},
                 json:decode(fxml:get_tag_cdata(Ev))),
    ?assertEqual([], M#message.body),
    %% verification: no store hint
    V = mod_matrix_gw_event:forward_message(base(), fields(<<"m.key.verification.start">>, #{}),
                                            #{}, undefined),
    ?assertEqual([<<"event">>, <<"x">>], names(V#message.sub_els)),
    R = mod_matrix_gw_event:forward_message(
          base(), fields(<<"m.room.message">>, #{<<"msgtype">> => <<"m.key.verification.request">>}),
          #{}, undefined),
    ?assertEqual([<<"event">>, <<"x">>], names(R#message.sub_els)),
    E = mod_matrix_gw_event:forward_message(base(), fields(<<"m.room.encryption">>, #{}), #{}, undefined),
    ?assertEqual([<<"event">>, <<"store">>, <<"x">>], names(E#message.sub_els)).

retraction() ->
    [R, F] = mod_matrix_gw_event:retraction_els(<<"id1">>, undefined, <<"why">>),
    ?assertEqual([], R#xmlel.children),
    ?assertEqual(<<"fallback">>, F#xmlel.name),
    By = j(<<"room@", ?GW/binary, "/mod">>),
    [R2, _] = mod_matrix_gw_event:retraction_els(<<"id1">>, By, <<>>),
    [Mod] = R2#xmlel.children,
    ?assertEqual(<<"urn:xmpp:message-moderate:1">>, attr(<<"xmlns">>, Mod)),
    ?assertEqual(jid:encode(By), attr(<<"by">>, Mod)).

iq(Type, El) ->
    #iq{type = Type, id = <<"i1">>, from = j(?ANN_RES), to = j(<<"ivy%remote.invalid@", ?GW/binary>>),
        sub_els = [El]}.

room_iqs() ->
    R = fun mod_matrix_gw_event:room_iq/1,
    ?assertEqual({room_info}, R(iq(get, el(<<"room-info">>, [], [])))),
    ?assertEqual({encryption, ?MEGOLM}, R(iq(set, el(<<"encryption">>, [{<<"algorithm">>, ?MEGOLM}], [])))),
    ?assertEqual({redact, <<"$e">>, <<>>}, R(iq(set, el(<<"redact">>, [{<<"event">>, <<"$e">>}], [])))),
    ?assertEqual({redact, <<"$e">>, <<"r">>},
                 R(iq(set, el(<<"redact">>, [{<<"event">>, <<"$e">>}, {<<"reason">>, <<"r">>}], [])))),
    ?assertMatch({error, #stanza_error{reason = 'bad-request'}}, R(iq(set, el(<<"redact">>, [], [])))),
    ?assertMatch({error, #stanza_error{reason = 'bad-request'}}, R(iq(set, el(<<"room-info">>, [], [])))),
    ?assertEqual(false, R(iq(get, el(<<"other">>, [], [])))),
    ?assertEqual(false, R(iq(get, #xmlel{name = <<"room-info">>, attrs = [{<<"xmlns">>, <<"x">>}]}))),
    ?assertEqual(false, R(iq(get, #disco_info{}))).

room_info() ->
    Base = #{id => <<"!r:s">>, version => <<"9">>, direct => true, power => 100,
             members => [{<<"@z:s">>, <<"invite">>}, {<<"@a:s">>, <<"join">>}]},
    Plain = mod_matrix_gw_event:room_info_el(Base#{encryption => undefined}),
    ?assertEqual([{<<"xmlns">>, ?NS}, {<<"id">>, <<"!r:s">>}, {<<"version">>, <<"9">>},
                  {<<"direct">>, <<"1">>}, {<<"power">>, <<"100">>}], Plain#xmlel.attrs),
    ?assertEqual([[{<<"id">>, <<"@a:s">>}, {<<"membership">>, <<"join">>}],
                  [{<<"id">>, <<"@z:s">>}, {<<"membership">>, <<"invite">>}]],
                 [A || #xmlel{attrs = A} <- Plain#xmlel.children]),
    Enc = mod_matrix_gw_event:room_info_el(Base#{encryption => #{<<"algorithm">> => ?MEGOLM},
                                                direct => false}),
    ?assertEqual(?MEGOLM, attr(<<"encryption">>, Enc)),
    ?assertEqual(<<"604800000">>, attr(<<"rotation_period_ms">>, Enc)),
    ?assertEqual(<<"100">>, attr(<<"rotation_period_msgs">>, Enc)),
    ?assertEqual(<<"0">>, attr(<<"direct">>, Enc)),
    Odd = mod_matrix_gw_event:room_info_el(
            Base#{encryption => #{<<"algorithm">> => ?MEGOLM, <<"rotation_period_ms">> => <<"5">>,
                                  <<"rotation_period_msgs">> => <<"x">>}}),
    ?assertEqual(<<"5">>, attr(<<"rotation_period_ms">>, Odd)),
    ?assertEqual(<<"100">>, attr(<<"rotation_period_msgs">>, Odd)),
    Odd2 = mod_matrix_gw_event:room_info_el(
             Base#{encryption => #{<<"algorithm">> => ?MEGOLM, <<"rotation_period_ms">> => -1,
                                   <<"rotation_period_msgs">> => <<"-3">>}}),
    ?assertEqual(<<"604800000">>, attr(<<"rotation_period_ms">>, Odd2)),
    ?assertEqual(<<"100">>, attr(<<"rotation_period_msgs">>, Odd2)),
    %% the redact level and the members' levels when given
    Lv = mod_matrix_gw_event:room_info_el(Base#{encryption => undefined, redact => 50,
                                                members => [{<<"@a:s">>, <<"join">>, 100}, {<<"@b:s">>, <<"join">>, 0}]}),
    ?assertEqual(<<"50">>, attr(<<"redact">>, Lv)),
    ?assertEqual([[{<<"id">>, <<"@a:s">>}, {<<"membership">>, <<"join">>}, {<<"power">>, <<"100">>}],
                  [{<<"id">>, <<"@b:s">>}, {<<"membership">>, <<"join">>}, {<<"power">>, <<"0">>}]],
                 [A || #xmlel{attrs = A} <- Lv#xmlel.children]).

answers() ->
    S = mod_matrix_gw_event:sent_message(j(?GW), j(?ANN_RES), <<"x1">>, <<"$e">>, <<"!r:s">>),
    ?assertEqual(j(?GW), S#message.from),
    [Sent, NoStore] = S#message.sub_els,
    ?assertEqual([{<<"xmlns">>, ?NS}, {<<"id">>, <<"x1">>}, {<<"event">>, <<"$e">>},
                  {<<"room">>, <<"!r:s">>}], Sent#xmlel.attrs),
    ?assertEqual(<<"no-store">>, NoStore#xmlel.name),
    ?assertEqual([{<<"xmlns">>, ?NS}, {<<"event">>, <<"$e">>}],
                 (mod_matrix_gw_event:sent_el(<<"$e">>))#xmlel.attrs),
    Err = mod_matrix_gw_event:not_encrypted(<<"en">>),
    ?assertMatch(#stanza_error{reason = 'not-acceptable',
                               sub_els = [#xmlel{name = <<"not-encrypted">>}]}, Err),
    E = mod_matrix_gw_event:not_encrypted_message(j(?GW), j(?ANN_RES), <<"m9">>, <<>>),
    ?assertMatch(#message{type = error, id = <<"m9">>}, E),
    ?assertMatch(#xmlel{name = <<"message">>}, xmpp:encode(E)).

disco() ->
    ?assertEqual([?NS], mod_matrix_gw_event:disco_features(j(<<"!abc%remote.invalid@", ?GW/binary>>))),
    ?assertEqual([?NS], mod_matrix_gw_event:disco_features(j(<<"#room%remote.invalid@", ?GW/binary>>))),
    ?assertEqual([], mod_matrix_gw_event:disco_features(j(<<"bob%remote.invalid@", ?GW/binary>>))),
    ?assertEqual([], mod_matrix_gw_event:disco_features(j(?GW))).

%% Without mod_matrix_gw_e2e's tables the seam is a no-op.
membership_without_e2e() ->
    ?assertEqual(ok, mod_matrix_gw_event:membership(?HOST, <<"!r:s">>, <<"@b:s">>, <<"join">>)).

%%--------------------------------------------------------------------
%% On a running gateway
%%--------------------------------------------------------------------
ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup, fun() -> mod_matrix_gw_test_lib:start_ejabberd(?MODULE) end, fun mod_matrix_gw_test_lib:stop_ejabberd/1,
     fun(_) ->
             [{timeout, 60, {Name, F}}
              || {Name, F} <- [{"an encrypted event makes an encrypted direct chat; <sent/>, <event-id/>",
                                fun encrypted_dm/0},
                               {"<room-info/>, and nothing for a chat not made", fun room_info_iq/0},
                               {"plain content into the encrypted chat: <not-encrypted/>",
                                fun not_encrypted_refused/0},
                               {"an event or message over 64 KiB refused; another server's sender",
                                fun too_large_and_foreign/0},
                               {"<redact/>: the redaction, the target pruned, the refusals",
                                fun redact_iq/0},
                               {"<encryption/> makes or turns a chat encrypted", fun encryption_iq/0},
                               {"the peer's events forwarded", fun peer_forwarded/0},
                               {"a room's events forwarded to its occupants", fun room_forwarded/0},
                               {"<event-id/> answers and refusals", fun event_id_iq/0},
                               {"matrix_sent keeps at most its bound per user", fun sent_bound/0},
                               {"after a restart, a correction, a reaction, <redact/> of an earlier message",
                                fun l2_restart/0},
                               {"a redaction of an unknown event only from one with the power",
                                fun l4_unknown_target/0},
                               {"a direct chat a third user joins is a room", fun l5_third_member/0},
                               {"a session's end is no Matrix leave for ten minutes", fun l6_leave_grace/0},
                               {"two first messages at once make one direct chat", fun l8_one_room/0}]]
     end}.

session(Full) ->
    #jid{luser = U, lserver = S, lresource = R} = j(Full),
    ejabberd_sm:open_session({erlang:timestamp(), self()}, U, S, R, 0, []).

close(Full) ->
    #jid{luser = U, lserver = S, lresource = R} = j(Full),
    case ejabberd_sm:get_session_sid(U, S, R) of
        none -> ok;
        SID -> ejabberd_sm:close_session(SID, U, S, R)
    end.

received() ->
    receive
        {route, Pkt} -> [Pkt | received()]
    after 300 -> []
    end.

direct_pid(Peer) ->
    direct_pid(Peer, 100).
direct_pid(Peer, Tries) ->
    case mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, Peer}) of
        [{matrix_direct, _, RoomID}] ->
            {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
            {RoomID, Pid};
        [] when Tries > 0 ->
            timer:sleep(20),
            direct_pid(Peer, Tries - 1);
        [] ->
            error({no_direct_room, Peer})
    end.

chain(Pid) ->
    Latest = gen_statem:call(Pid, get_latest_events),
    [Last] = maps:keys(Latest),
    lists:reverse(walk(Pid, Last)).

walk(Pid, ID) ->
    [JSON] = [M || M <- tuple_to_list(find(Pid, ID)), is_map(M), is_map_key(<<"prev_events">>, M)],
    case maps:get(<<"prev_events">>, JSON) of
        [] -> [JSON];
        [Prev | _] -> [JSON | walk(Pid, Prev)]
    end.

find(Pid, ID) ->
    {ok, Event} = gen_statem:call(Pid, {find_event, ID}),
    Event.

types(Chain) -> [maps:get(<<"type">>, E) || E <- Chain].

last_id(Pid) ->
    [Last] = maps:keys(gen_statem:call(Pid, get_latest_events)),
    Last.

peer(Name) -> <<Name/binary, "%remote.invalid@", ?GW/binary>>.
peer_id(Name) -> <<"@", Name/binary, ":remote.invalid">>.

%% Sends Pkt as ann's resource and gives what came back.
exchange(Pkt) ->
    session(?ANN_RES),
    _ = received(),
    ok = mod_matrix_gw_room:route(Pkt),
    Got = received(),
    close(?ANN_RES),
    Got.

encrypted_dm() ->
    Got = exchange(msg(?ANN_RES, peer(<<"ivy">>), <<"m.room.encrypted">>, json(megolm()))),
    {RoomID, Pid} = direct_pid(peer_id(<<"ivy">>)),
    Chain = chain(Pid),
    ?assertEqual([<<"m.room.create">>, <<"m.room.member">>, <<"m.room.power_levels">>,
                  <<"m.room.join_rules">>, <<"m.room.history_visibility">>,
                  <<"m.room.guest_access">>, <<"m.room.member">>, <<"m.room.encryption">>,
                  <<"m.room.encrypted">>], types(Chain)),
    ?assertEqual(megolm(), maps:get(<<"content">>, lists:last(Chain))),
    EventID = last_id(Pid),
    [Sent] = [M || #message{} = M <- Got],
    ?assertEqual(j(?GW), Sent#message.from),
    ?assertEqual(j(?ANN_RES), Sent#message.to),
    [S | _] = Sent#message.sub_els,
    ?assertEqual([{<<"xmlns">>, ?NS}, {<<"id">>, <<"x1">>}, {<<"event">>, EventID},
                  {<<"room">>, RoomID}], S#xmlel.attrs),
    ?assertEqual({ok, EventID, RoomID}, mod_matrix_gw_event:lookup_sent({<<"ann">>, ?HOST}, <<"x1">>)),
    %% the next one into the same room, still <sent/>
    Got2 = exchange((msg(?ANN_RES, peer(<<"ivy">>), <<"m.room.encrypted">>, json(megolm())))#message{id = <<"x2">>}),
    ?assertEqual({RoomID, Pid}, direct_pid(peer_id(<<"ivy">>))),
    [Sent2] = [M || #message{} = M <- Got2],
    [S2 | _] = Sent2#message.sub_els,
    ?assertEqual(last_id(Pid), attr(<<"event">>, S2)).

room_info_iq() ->
    [R] = exchange(iq(get, el(<<"room-info">>, [], []))),
    #iq{type = result, sub_els = [Info]} = R,
    {RoomID, _} = direct_pid(peer_id(<<"ivy">>)),
    ?assertEqual(RoomID, attr(<<"id">>, Info)),
    ?assertEqual(<<"9">>, attr(<<"version">>, Info)),
    ?assertEqual(?MEGOLM, attr(<<"encryption">>, Info)),
    ?assertEqual(<<"1">>, attr(<<"direct">>, Info)),
    ?assertEqual(<<"100">>, attr(<<"power">>, Info)),
    %% each member's level and the redact level too
    ?assertEqual([[{<<"id">>, ?ANN_ID}, {<<"membership">>, <<"join">>}, {<<"power">>, <<"100">>}],
                  [{<<"id">>, peer_id(<<"ivy">>)}, {<<"membership">>, <<"invite">>}, {<<"power">>, <<"100">>}]],
                 [A || #xmlel{attrs = A} <- Info#xmlel.children]),
    ?assertEqual(<<"50">>, attr(<<"redact">>, Info)),
    %% no chat with frank yet
    [E] = exchange((iq(get, el(<<"room-info">>, [], [])))#iq{to = j(peer(<<"mona">>))}),
    ?assertMatch(#stanza_error{reason = 'item-not-found'}, xmpp:get_error(E)),
    %% nor a room the gateway has
    [E2] = exchange((iq(get, el(<<"room-info">>, [], [])))#iq{to = j(<<"!abcd%remote.invalid@", ?GW/binary>>)}),
    ?assertMatch(#stanza_error{reason = 'item-not-found'}, xmpp:get_error(E2)),
    %% nor anything for another server's user
    ok = mod_matrix_gw_room:route((iq(get, el(<<"room-info">>, [], [])))#iq{from = j(<<"eve@elsewhere.example/x">>)}),
    %% a wrong type is a bad request
    [E3] = exchange(iq(set, el(<<"room-info">>, [], []))),
    ?assertMatch(#stanza_error{reason = 'bad-request'}, xmpp:get_error(E3)).

not_encrypted_refused() ->
    {_, Pid} = direct_pid(peer_id(<<"ivy">>)),
    Before = last_id(Pid),
    Plain = #message{id = <<"p1">>, type = chat, from = j(?ANN_RES), to = j(peer(<<"ivy">>)),
                     body = [#text{data = <<"hello">>}]},
    [Err] = [M || #message{type = error} = M <- exchange(Plain)],
    ?assertEqual(<<"p1">>, Err#message.id),
    ?assertEqual(j(peer(<<"ivy">>)), Err#message.from),
    ?assertMatch(#stanza_error{reason = 'not-acceptable', sub_els = [#xmlel{name = <<"not-encrypted">>}]},
                 xmpp:get_error(Err)),
    %% an <event/> of another type too
    [Err2] = [M || #message{type = error} = M
                       <- exchange(msg(?ANN_RES, peer(<<"ivy">>), <<"m.reaction">>, <<"{}">>))],
    ?assertMatch(#stanza_error{reason = 'not-acceptable'}, xmpp:get_error(Err2)),
    %% a chat state alone is no content: no error
    State = #message{id = <<"p2">>, type = chat, from = j(?ANN_RES), to = j(peer(<<"ivy">>)),
                     sub_els = [#chatstate{type = composing}]},
    ?assertEqual([], [M || #message{type = error} = M <- exchange(State)]),
    ?assertEqual(Before, last_id(Pid)).

%% an <event/> whose PDU would be over 65536 bytes is
%% refused not-acceptable <too-large/> and adds nothing; a plain body as
%% large bounces the same way. A message from another server's
%% user makes no room and no matrix_direct row for anyone.
too_large_and_foreign() ->
    {_, Pid} = direct_pid(peer_id(<<"ivy">>)),
    Before = last_id(Pid),
    Big = json((megolm())#{<<"ciphertext">> => binary:copy(<<"A">>, 70000)}),
    [Err] = [M || #message{type = error} = M
                      <- exchange((msg(?ANN_RES, peer(<<"ivy">>), <<"m.room.encrypted">>, Big))#message{id = <<"big1">>})],
    ?assertEqual(<<"big1">>, Err#message.id),
    ?assertMatch(#stanza_error{reason = 'not-acceptable', sub_els = [#xmlel{name = <<"too-large">>}]},
                 xmpp:get_error(Err)),
    ?assertEqual(Before, last_id(Pid)),
    %% just under: taken
    Fits = json((megolm())#{<<"ciphertext">> => binary:copy(<<"A">>, 60000)}),
    [Sent] = [M || #message{type = normal} = M
                       <- exchange((msg(?ANN_RES, peer(<<"ivy">>), <<"m.room.encrypted">>, Fits))#message{id = <<"big2">>})],
    ?assertEqual(<<"big2">>, attr(<<"id">>, hd(Sent#message.sub_els))),
    %% a plain chat: the large body bounces, the small one goes
    Plain = fun(ID, Body) -> #message{id = ID, type = chat, from = j(?ANN_RES), to = j(peer(<<"pia">>)),
                                      body = [#text{data = Body}]} end,
    _ = exchange(Plain(<<"small">>, <<"hi">>)),
    {_, PPid} = direct_pid(peer_id(<<"pia">>)),
    PBefore = last_id(PPid),
    [PErr] = [M || #message{type = error} = M <- exchange(Plain(<<"big3">>, binary:copy(<<"b">>, 70000)))],
    ?assertEqual(<<"big3">>, PErr#message.id),
    ?assertMatch(#stanza_error{reason = 'not-acceptable', sub_els = [#xmlel{name = <<"too-large">>}]},
                 xmpp:get_error(PErr)),
    ?assertEqual(PBefore, last_id(PPid)),
    %% A2: ann of another server writes to quinn: refused, no room, no row
    Foreign = #message{id = <<"f1">>, type = chat, from = j(<<"ann@elsewhere.example/x">>),
                       to = j(peer(<<"quinn">>)), body = [#text{data = <<"hi">>}]},
    ok = mod_matrix_gw_room:route(Foreign),
    timer:sleep(200),
    ?assertEqual([], mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, peer_id(<<"quinn">>)})),
    ?assertEqual([], mnesia:dirty_read(matrix_direct, {{<<"ann">>, <<"elsewhere.example">>}, peer_id(<<"quinn">>)})),
    %% nor does it take over ann's own chat with ivy
    {IvyRoom, _} = direct_pid(peer_id(<<"ivy">>)),
    ok = mod_matrix_gw_room:route(Foreign#message{to = j(peer(<<"ivy">>))}),
    timer:sleep(200),
    ?assertMatch({IvyRoom, _}, direct_pid(peer_id(<<"ivy">>))).

redact_iq() ->
    {_, Pid} = direct_pid(peer_id(<<"ivy">>)),
    Target = last_id(Pid),
    [R] = exchange(iq(set, el(<<"redact">>, [{<<"event">>, Target}, {<<"reason">>, <<"typo">>}], []))),
    #iq{type = result, sub_els = [Sent]} = R,
    Redaction = last_id(Pid),
    ?assertEqual(Redaction, attr(<<"event">>, Sent)),
    #{<<"type">> := <<"m.room.redaction">>, <<"redacts">> := Target,
      <<"content">> := #{<<"reason">> := <<"typo">>}} = lists:last(chain(Pid)),
    %% the gateway's copy of the target is pruned
    [Pruned] = [M || M <- tuple_to_list(find(Pid, Target)), is_map(M), is_map_key(<<"prev_events">>, M)],
    ?assertEqual(#{}, maps:get(<<"content">>, Pruned)),
    %% an event the room has not, by one with the power to redact: a
    %% redaction (the auth rules need no target)
    [RN] = exchange(iq(set, el(<<"redact">>, [{<<"event">>, <<"$none">>}], []))),
    ?assertMatch(#iq{type = result}, RN),
    %% bob's own event, and ann's power taken below the redact level: forbidden
    BobID = peer_id(<<"ivy">>),
    {ok, _} = gen_statem:call(Pid, {add_event, #{<<"type">> => <<"m.room.member">>, <<"sender">> => BobID,
                                                 <<"state_key">> => BobID,
                                                 <<"content">> => #{<<"membership">> => <<"join">>}}}),
    {ok, BobEvent} = gen_statem:call(Pid, {add_event, #{<<"type">> => <<"m.room.encrypted">>,
                                                        <<"sender">> => BobID, <<"content">> => megolm()}}),
    PL = #{<<"users">> => #{?ANN_ID => 10, BobID => 100}, <<"redact">> => 50,
           <<"events">> => #{<<"m.room.encryption">> => 100}},
    {ok, _} = gen_statem:call(Pid, {add_event, #{<<"type">> => <<"m.room.power_levels">>,
                                                 <<"sender">> => ?ANN_ID, <<"state_key">> => <<>>,
                                                 <<"content">> => PL}}),
    [E2] = exchange(iq(set, el(<<"redact">>, [{<<"event">>, BobEvent}], []))),
    ?assertMatch(#stanza_error{reason = 'forbidden'}, xmpp:get_error(E2)),
    %% and an event the room has not, without that power: item-not-found
    [E] = exchange(iq(set, el(<<"redact">>, [{<<"event">>, <<"$none2">>}], []))),
    ?assertMatch(#stanza_error{reason = 'item-not-found'}, xmpp:get_error(E)),
    %% <encryption/> again: already so, the answer is the room-info
    [R3] = exchange(iq(set, el(<<"encryption">>, [{<<"algorithm">>, ?MEGOLM}], []))),
    ?assertMatch(#iq{type = result}, R3),
    [R4] = exchange(iq(set, el(<<"encryption">>, [{<<"algorithm">>, <<"m.other">>}], []))),
    ?assertMatch(#stanza_error{reason = 'not-acceptable'}, xmpp:get_error(R4)).

encryption_iq() ->
    %% a chat not made yet: made encrypted
    [R] = exchange((iq(set, el(<<"encryption">>, [{<<"algorithm">>, ?MEGOLM}], [])))#iq{to = j(peer(<<"jack">>))}),
    #iq{type = result, sub_els = [Info]} = R,
    ?assertEqual(?MEGOLM, attr(<<"encryption">>, Info)),
    {_, Pid} = direct_pid(peer_id(<<"jack">>)),
    ?assertEqual(<<"m.room.encryption">>, lists:last(types(chain(Pid)))),
    %% a plain chat (made by a text) turned encrypted
    [] = [M || #message{type = error} = M
                   <- exchange(#message{id = <<"t1">>, type = chat, from = j(?ANN_RES), to = j(peer(<<"kate">>)),
                                        body = [#text{data = <<"hi">>}]})],
    {_, DPid} = direct_pid(peer_id(<<"kate">>)),
    timer:sleep(100),
    ?assertNot(lists:member(<<"m.room.encryption">>, types(chain(DPid)))),
    [R2] = exchange((iq(set, el(<<"encryption">>, [{<<"algorithm">>, ?MEGOLM}], [])))#iq{to = j(peer(<<"kate">>))}),
    ?assertMatch(#iq{type = result}, R2),
    ?assertEqual(<<"m.room.encryption">>, lists:last(types(chain(DPid)))),
    %% an unknown algorithm on a plain chat
    [] = [M || #message{type = error} = M
                   <- exchange(#message{id = <<"t2">>, type = chat, from = j(?ANN_RES), to = j(peer(<<"liam">>)),
                                        body = [#text{data = <<"hi">>}]})],
    _ = direct_pid(peer_id(<<"liam">>)),
    [R3] = exchange((iq(set, el(<<"encryption">>, [{<<"algorithm">>, <<"m.other">>}], [])))#iq{to = j(peer(<<"liam">>))}),
    ?assertMatch(#stanza_error{reason = 'not-acceptable'}, xmpp:get_error(R3)),
    %% a plain event creates a plain chat (a call, a verification)
    [_Sent] = exchange(msg(?ANN_RES, peer(<<"nora">>), <<"m.key.verification.request">>, <<"{}">>)),
    {_, GPid} = direct_pid(peer_id(<<"nora">>)),
    ?assertEqual(<<"m.key.verification.request">>, lists:last(types(chain(GPid)))),
    ?assertNot(lists:member(<<"m.room.encryption">>, types(chain(GPid)))).

%% bob's events (added as the room would take them from his server) reach
%% ann as the forwarded set.
peer_forwarded() ->
    session(?ANN_RES),
    _ = received(),
    Bob = peer_id(<<"ivy">>),
    BobJID = j(peer(<<"ivy">>)),
    [Sent] = [M || #message{} = M <- exchange_open(msg(?ANN_RES, peer(<<"otto">>), <<"m.room.encrypted">>,
                                                       json(megolm())))],
    {RoomID, Pid} = direct_pid(peer_id(<<"otto">>)),
    _ = Sent,
    Hank = peer_id(<<"otto">>),
    Add = fun(J) -> {ok, ID} = gen_statem:call(Pid, {add_event, J}), ID end,
    Add(#{<<"type">> => <<"m.room.member">>, <<"sender">> => Hank, <<"state_key">> => Hank,
          <<"content">> => #{<<"membership">> => <<"join">>}}),
    [Join] = [M || #message{} = M <- received()],
    [JEv | _] = Join#message.sub_els,
    ?assertEqual(<<"m.room.member">>, attr(<<"type">>, JEv)),
    ?assertEqual(Hank, attr(<<"state_key">>, JEv)),
    ?assertMatch(#{<<"membership">> := <<"join">>, <<"state_key">> := Hank},
                 json:decode(fxml:get_tag_cdata(JEv))),
    Enc = Add(#{<<"type">> => <<"m.room.encrypted">>, <<"sender">> => Hank, <<"content">> => megolm()}),
    [EncMsg] = [M || #message{} = M <- received()],
    ?assertEqual(Enc, EncMsg#message.id),
    ?assertEqual(j(peer(<<"otto">>)), EncMsg#message.from),
    ?assertEqual([#text{data = <<"Encrypted message (Matrix)">>}], EncMsg#message.body),
    [_, EncEv | _] = EncMsg#message.sub_els,
    ?assertEqual([{<<"xmlns">>, ?NS}, {<<"type">>, <<"m.room.encrypted">>}, {<<"id">>, Enc},
                  {<<"room">>, RoomID}, {<<"sender">>, Hank}],
                 lists:sublist(EncEv#xmlel.attrs, 5)),
    %% a plain text: the body, and an empty <event/> naming it
    Text = Add(#{<<"type">> => <<"m.room.message">>, <<"sender">> => Hank,
                 <<"content">> => #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"plain">>}}),
    [TextMsg] = [M || #message{} = M <- received()],
    ?assertEqual([#text{data = <<"plain">>}], TextMsg#message.body),
    [PlainEv] = [E || #xmlel{name = <<"event">>} = E <- TextMsg#message.sub_els],
    ?assertEqual(Text, attr(<<"id">>, PlainEv)),
    ?assertEqual([], PlainEv#xmlel.children),
    %% hank redacts his text: the event, and the retraction of the text
    Red = Add(#{<<"type">> => <<"m.room.redaction">>, <<"sender">> => Hank, <<"redacts">> => Text,
                <<"content">> => #{<<"redacts">> => Text}}),
    [RedMsg] = [M || #message{} = M <- received()],
    ?assertEqual(Red, RedMsg#message.id),
    ?assertEqual([#text{data = <<"Message retracted">>}], RedMsg#message.body),
    [REv, Retract | _] = RedMsg#message.sub_els,
    ?assertEqual(Text, attr(<<"redacts">>, REv)),
    ?assertEqual(Text, attr(<<"id">>, Retract)),
    %% a reaction of hank's, taken back: its new (empty) set, the redaction's
    %% event, no retraction
    Reaction = Add(#{<<"type">> => <<"m.reaction">>, <<"sender">> => Hank,
                     <<"content">> => #{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.annotation">>,
                                                                 <<"event_id">> => Enc,
                                                                 <<"key">> => <<"+1">>}}}),
    _ = received(),
    Add(#{<<"type">> => <<"m.room.redaction">>, <<"sender">> => Hank, <<"redacts">> => Reaction,
          <<"content">> => #{<<"redacts">> => Reaction}}),
    Got = [M || #message{} = M <- received()],
    ?assertEqual(2, length(Got)),
    [RR] = [M || M <- Got, lists:member(<<"event">>, names(M#message.sub_els))],
    ?assertNot(lists:member(<<"retract">>, names(RR#message.sub_els))),
    %% a redaction of an event the room has not: its <event/> forwarded for
    %% the device, but no XEP-0424 retraction (the target
    %% could be any id the redactor names, ann's XMPP ids among them)
    Add(#{<<"type">> => <<"m.room.redaction">>, <<"sender">> => Hank, <<"redacts">> => <<"$gone">>,
          <<"content">> => #{<<"redacts">> => <<"$gone">>}}),
    [Gone] = [M || #message{} = M <- received()],
    ?assertNot(lists:member(<<"retract">>, names(Gone#message.sub_els))),
    ?assert(lists:member(<<"event">>, names(Gone#message.sub_els))),
    ?assertEqual([], Gone#message.body),
    Add(#{<<"type">> => <<"m.room.redaction">>, <<"sender">> => Hank, <<"redacts">> => <<"x1">>,
          <<"content">> => #{<<"redacts">> => <<"x1">>}}),
    [ByXmppID] = [M || #message{} = M <- received()],
    ?assertNot(lists:member(<<"retract">>, names(ByXmppID#message.sub_els))),
    %% the room turned encrypted by hank (a state change) and his
    %% verification request: forwarded
    Add(#{<<"type">> => <<"m.key.verification.request">>, <<"sender">> => Hank,
          <<"content">> => #{<<"methods">> => [<<"m.sas.v1">>]}}),
    [VMsg] = [M || #message{} = M <- received()],
    ?assertEqual([<<"event">>, <<"x">>], names(VMsg#message.sub_els)),
    %% an unknown type: nothing
    Add(#{<<"type">> => <<"org.example.thing">>, <<"sender">> => Hank, <<"content">> => #{}}),
    ?assertEqual([], [M || #message{} = M <- received()]),
    %% <event-id/> of what ann sent
    _ = Bob, _ = BobJID,
    close(?ANN_RES).

%% As exchange/1 with the session left open.
exchange_open(Pkt) ->
    ok = mod_matrix_gw_room:route(Pkt),
    received().

%% A room ann made with hank, stored as a multi-user room and brought back:
%% ann joins it by MUC presence and gets hank's events as groupchat.
room_forwarded() ->
    RoomID = <<"!multi:example.com">>,
    Version = mod_matrix_gw_room:binary_to_room_version(<<"9">>),
    Hank = peer_id(<<"otto">>),
    {_, Events} =
        lists:foldl(
          fun({Sender, Type, StateKey, Content}, {Prev, Acc}) ->
                  Auth = [E || {E, _, _} <- Acc],
                  JSON0 = #{<<"type">> => Type, <<"state_key">> => StateKey,
                            <<"content">> => Content, <<"sender">> => Sender,
                            <<"room_id">> => RoomID, <<"origin">> => ?HOST,
                            <<"origin_server_ts">> => 1700000000000 + length(Acc),
                            <<"depth">> => length(Acc) + 1,
                            <<"prev_events">> => Prev, <<"auth_events">> => Auth},
                  Hash = mod_matrix_gw:content_hash(JSON0),
                  JSON1 = JSON0#{<<"hashes">> => #{<<"sha256">> => mod_matrix_gw:base64_encode(Hash)}},
                  JSON = mod_matrix_gw:sign_event(?HOST, JSON1, Version),
                  ID = mod_matrix_gw:get_event_id(JSON, Version),
                  StateMap = maps:from_list([{{T, K}, E} || {E, T, K} <- [{ID, Type, StateKey} | [{E2, T2, K2} || {E2, {T2, K2}, _} <- Acc]]]),
                  {[ID], [{ID, {Type, StateKey}, {JSON, StateMap}} | Acc]}
          end, {[], []},
          [{?ANN_ID, <<"m.room.create">>, <<>>, #{<<"creator">> => ?ANN_ID, <<"room_version">> => <<"9">>}},
           {?ANN_ID, <<"m.room.member">>, ?ANN_ID, #{<<"membership">> => <<"join">>}},
           {?ANN_ID, <<"m.room.power_levels">>, <<>>, #{<<"users">> => #{?ANN_ID => 100, Hank => 50},
                                                        <<"redact">> => 50}},
           {?ANN_ID, <<"m.room.join_rules">>, <<>>, #{<<"join_rule">> => <<"public">>}},
           {Hank, <<"m.room.member">>, Hank, #{<<"membership">> => <<"join">>}}]),
    [{Last, _, _} | _] = Events,
    ok = mod_matrix_gw_store:save_meta(RoomID, <<"9">>, multi, [Last]),
    ok = mod_matrix_gw_store:save_events(RoomID, [{ID, J, SM} || {ID, _, {J, SM}} <- Events], [Last]),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
    RoomJID = <<"!", (integer_to_binary(binary:decode_unsigned(<<"multi">>), 16))/binary,
                "%example.com@", ?GW/binary>>,
    session(?ANN_RES),
    _ = received(),
    ok = mod_matrix_gw_room:route(#presence{from = j(?ANN_RES), to = j(<<RoomJID/binary, "/ann">>),
                                            type = available}),
    timer:sleep(200),
    _ = received(),
    %% the room's disco names the feature
    ok = mod_matrix_gw_room:route(#iq{type = get, id = <<"d">>, from = j(?ANN_RES), to = j(RoomJID),
                                      sub_els = [xmpp:encode(#disco_info{node = <<>>})]}),
    [#iq{type = result, sub_els = [DI]}] = [I || #iq{} = I <- received()],
    #disco_info{features = Fs} = case DI of #xmlel{} -> xmpp:decode(DI); _ -> DI end,
    ?assert(lists:member(?NS, Fs)),
    Add = fun(J) -> {ok, ID} = gen_statem:call(Pid, {add_event, J}), ID end,
    %% <room-info/> of the room
    ok = mod_matrix_gw_room:route((iq(get, el(<<"room-info">>, [], [])))#iq{to = j(RoomJID)}),
    [#iq{type = result, sub_els = [Info]}] = [I || #iq{} = I <- received()],
    ?assertEqual(<<"0">>, attr(<<"direct">>, Info)),
    ?assertEqual(<<>>, attr(<<"encryption">>, Info)),
    %% ann's <event/> into the room: <sent/>, and the room's echo to her
    ok = mod_matrix_gw_room:route((msg(?ANN_RES, RoomJID, <<"m.room.encrypted">>, json(megolm())))#message{
                                    type = groupchat, id = <<"g1">>}),
    Got = [M || #message{} = M <- received()],
    [Echo] = [M || #message{type = groupchat} = M <- Got],
    ?assertEqual(<<"g1">>, Echo#message.id),
    ?assertEqual(j(<<RoomJID/binary, "/ann:example.com">>), Echo#message.from),
    [SentMsg] = [M || #message{type = normal} = M <- Got],
    [S | _] = SentMsg#message.sub_els,
    ?assertEqual(<<"g1">>, attr(<<"id">>, S)),
    Mine = attr(<<"event">>, S),
    %% hank's encrypted event, as groupchat from his nick with the stanza-id
    Enc = Add(#{<<"type">> => <<"m.room.encrypted">>, <<"sender">> => Hank, <<"content">> => megolm()}),
    [EncMsg] = [M || #message{} = M <- received()],
    ?assertEqual(groupchat, EncMsg#message.type),
    ?assertEqual(j(<<RoomJID/binary, "/otto:remote.invalid">>), EncMsg#message.from),
    ?assertMatch([#stanza_id{id = Enc} | _], EncMsg#message.sub_els),
    %% hank (power 50, the redact level) redacts ann's event: moderated
    Add(#{<<"type">> => <<"m.room.redaction">>, <<"sender">> => Hank, <<"redacts">> => Mine,
          <<"content">> => #{<<"redacts">> => Mine}}),
    [RedMsg] = [M || #message{} = M <- received()],
    [Retract] = [E || #xmlel{name = <<"retract">>} = E <- RedMsg#message.sub_els],
    ?assertEqual(Mine, attr(<<"id">>, Retract)),
    ?assertEqual([<<"moderated">>], names(Retract#xmlel.children)),
    %% the room turned encrypted: forwarded; then ann's plain body refused
    Add(#{<<"type">> => <<"m.room.encryption">>, <<"sender">> => ?ANN_ID, <<"state_key">> => <<>>,
          <<"content">> => mod_matrix_gw_event:encryption_content()}),
    [EncState] = [M || #message{} = M <- received()],
    [ESEv] = [E || #xmlel{name = <<"event">>} = E <- EncState#message.sub_els],
    ?assertEqual(<<"m.room.encryption">>, attr(<<"type">>, ESEv)),
    ok = mod_matrix_gw_room:route(#message{id = <<"g2">>, type = groupchat, from = j(?ANN_RES),
                                           to = j(RoomJID), body = [#text{data = <<"plain">>}]}),
    [Err] = [M || #message{type = error} = M <- received()],
    ?assertEqual(j(RoomJID), Err#message.from),
    ?assertMatch(#stanza_error{reason = 'not-acceptable'}, xmpp:get_error(Err)),
    %% the members, for mod_matrix_gw_e2e
    ?assertEqual([{?ANN_ID, <<"join">>}, {Hank, <<"join">>}], mod_matrix_gw_room:members(RoomID)),
    ?assertEqual([], mod_matrix_gw_room:members(<<"!none:example.com">>)),
    %% an <event/> to a room the gateway has not
    ok = mod_matrix_gw_room:route((msg(?ANN_RES, <<"!abcd%remote.invalid@", ?GW/binary>>,
                                       <<"m.room.encrypted">>, <<"{}">>))#message{type = groupchat}),
    [NoRoom] = [M || #message{type = error} = M <- received()],
    ?assertMatch(#stanza_error{reason = 'item-not-found'}, xmpp:get_error(NoRoom)),
    close(?ANN_RES).

event_id_iq() ->
    IQ = fun(Attrs) -> #iq{type = get, id = <<"e1">>, from = j(?ANN_RES), to = j(?GW),
                           sub_els = [el(<<"event-id">>, Attrs, [])]} end,
    {ok, EventID, RoomID} = mod_matrix_gw_event:lookup_sent({<<"ann">>, ?HOST}, <<"x1">>),
    [R] = exchange_iq(IQ([{<<"id">>, <<"x1">>}])),
    #iq{type = result, sub_els = [E]} = R,
    ?assertEqual(RoomID, attr(<<"room">>, E)),
    ?assertEqual(EventID, attr(<<"event">>, E)),
    [R2] = exchange_iq(IQ([{<<"id">>, <<"never">>}])),
    ?assertMatch(#stanza_error{reason = 'item-not-found'}, xmpp:get_error(R2)),
    [R3] = exchange_iq(IQ([])),
    ?assertMatch(#stanza_error{reason = 'bad-request'}, xmpp:get_error(R3)),
    [R4] = exchange_iq((IQ([{<<"id">>, <<"x1">>}]))#iq{type = set}),
    ?assertMatch(#stanza_error{reason = 'bad-request'}, xmpp:get_error(R4)),
    %% another server's user: refused (its answer goes to that server)
    ok = mod_matrix_gw:route((IQ([{<<"id">>, <<"x1">>}]))#iq{from = j(<<"eve@elsewhere.example/x">>)}).

exchange_iq(IQ) ->
    session(?ANN_RES),
    _ = received(),
    ok = mod_matrix_gw:route(IQ),
    Got = received(),
    close(?ANN_RES),
    Got.

sent_bound() ->
    US = {<<"zed">>, ?HOST},
    [mod_matrix_gw_event:record_sent(US, integer_to_binary(I), <<"$e", (integer_to_binary(I))/binary>>,
                                     <<"!r:s">>) || I <- lists:seq(1, 10005)],
    ?assertEqual(error, mod_matrix_gw_event:lookup_sent(US, <<"5">>)),
    ?assertEqual({ok, <<"$e6">>, <<"!r:s">>}, mod_matrix_gw_event:lookup_sent(US, <<"6">>)),
    ?assertEqual({ok, <<"$e10005">>, <<"!r:s">>}, mod_matrix_gw_event:lookup_sent(US, <<"10005">>)),
    %% an id sent again moves to the newest; its old place drops nothing of it
    ok = mod_matrix_gw_event:record_sent(US, <<"6">>, <<"$again">>, <<"!r:s">>),
    ok = mod_matrix_gw_event:record_sent(US, <<"new">>, <<"$n">>, <<"!r:s">>),
    ?assertEqual({ok, <<"$again">>, <<"!r:s">>}, mod_matrix_gw_event:lookup_sent(US, <<"6">>)),
    ?assertEqual(error, mod_matrix_gw_event:lookup_sent(US, <<"7">>)),
    ?assertEqual(ok, mod_matrix_gw_event:record_sent(US, <<>>, <<"$x">>, <<"!r:s">>)),
    ?assertEqual(10000, length([K || {matrix_sent, {U, _} = K, _, _, _} <- ets:tab2list(matrix_sent),
                                     U == US])).

%% The JSON of the room's last event.
last_json(Pid) ->
    [JSON] = [M || M <- tuple_to_list(find(Pid, last_id(Pid))), is_map(M),
                   is_map_key(<<"prev_events">>, M)],
    JSON.

rex_msg(ID, Body, Els) ->
    #message{id = ID, type = chat, from = j(?ANN_RES), to = j(peer(<<"rex">>)),
             body = [#text{data = Body} || Body /= <<>>], sub_els = Els}.

%% ann's "meet at 5" from a plain client, then the room's
%% process restarted (it comes back from the store, which keeps no
%% messages, and with no id map): her correction is an m.replace of the
%% old event, her reaction names it, her <redact/> of it is a redaction,
%% and rex's redaction of it retracts it under her XMPP id.
l2_restart() ->
    [] = [M || #message{type = error} = M <- exchange(rex_msg(<<"l2a">>, <<"meet at 5">>, []))],
    {RoomID, Pid} = direct_pid(peer_id(<<"rex">>)),
    timer:sleep(100),
    First = last_id(Pid),
    _ = exchange(rex_msg(<<"l2b">>, <<"and bring tea">>, [])),
    timer:sleep(100),
    ?assertNotEqual(First, last_id(Pid)),
    Rex = peer_id(<<"rex">>),
    {ok, _} = gen_statem:call(Pid, {add_event, #{<<"type">> => <<"m.room.member">>, <<"sender">> => Rex,
                                                 <<"state_key">> => Rex,
                                                 <<"content">> => #{<<"membership">> => <<"join">>}}}),
    Ref = monitor(process, Pid),
    exit(Pid, kill),
    receive {'DOWN', Ref, _, _, _} -> ok end,
    mnesia:dirty_delete(matrix_room, RoomID),
    {RoomID, Pid2} = direct_pid(peer_id(<<"rex">>)),
    ?assertNotEqual(Pid, Pid2),
    ?assertEqual(error, gen_statem:call(Pid2, {find_event, First})),
    %% the correction
    _ = exchange(rex_msg(<<"l2c">>, <<"meet at 6">>, [mod_matrix_gw_xmpp:replace_el(<<"l2a">>)])),
    timer:sleep(100),
    ?assertMatch(#{<<"type">> := <<"m.room.message">>,
                   <<"content">> := #{<<"m.relates_to">> := #{<<"rel_type">> := <<"m.replace">>,
                                                              <<"event_id">> := First}}},
                 last_json(Pid2)),
    %% the reaction
    _ = exchange(rex_msg(<<"l2d">>, <<>>, [mod_matrix_gw_xmpp:reactions_el(<<"l2a">>, [<<"+1">>])])),
    timer:sleep(100),
    ?assertMatch(#{<<"type">> := <<"m.reaction">>,
                   <<"content">> := #{<<"m.relates_to">> := #{<<"event_id">> := First}}},
                 last_json(Pid2)),
    %% rex redacts it: the retraction names her XMPP id
    session(?ANN_RES),
    _ = received(),
    {ok, _} = gen_statem:call(Pid2, {add_event, #{<<"type">> => <<"m.room.redaction">>, <<"sender">> => Rex,
                                                  <<"redacts">> => First,
                                                  <<"content">> => #{<<"redacts">> => First}}}),
    [RedMsg] = [M || #message{} = M <- received()],
    close(?ANN_RES),
    [Retract] = [E || #xmlel{name = <<"retract">>} = E <- RedMsg#message.sub_els],
    ?assertEqual(<<"l2a">>, attr(<<"id">>, Retract)),
    %% the peer's redaction of ann's message: XEP-0425, by him
    [Moderated] = [E || #xmlel{name = <<"moderated">>} = E <- Retract#xmlel.children],
    ?assertEqual(peer(<<"rex">>), attr(<<"by">>, Moderated)),
    ?assertEqual(<<"This message has been moderated">>, xmpp:get_text(RedMsg#message.body)),
    %% her own <redact/> of it (the room no longer holds it)
    [R] = exchange((iq(set, el(<<"redact">>, [{<<"event">>, First}], [])))#iq{to = j(peer(<<"rex">>))}),
    #iq{type = result, sub_els = [Sent]} = R,
    ?assertEqual(last_id(Pid2), attr(<<"event">>, Sent)),
    ?assertMatch(#{<<"type">> := <<"m.room.redaction">>, <<"redacts">> := First}, last_json(Pid2)),
    %% an id nobody sent: still nothing
    [E] = exchange((iq(set, el(<<"redact">>, [{<<"event">>, <<"nothing">>}], [])))#iq{to = j(peer(<<"rex">>))}),
    ?assertMatch(#stanza_error{reason = 'item-not-found'}, xmpp:get_error(E)),
    %% a MUC join of the direct chat's room JID: not-acceptable
    [<<$!, Local/binary>>, Server] = binary:split(RoomID, <<":">>),
    RoomJID = <<"!", (integer_to_binary(binary:decode_unsigned(Local), 16))/binary, "%",
                Server/binary, "@", ?GW/binary>>,
    Join = [P || #presence{} = P <- exchange(#presence{from = j(?ANN_RES), to = j(<<RoomJID/binary, "/ann">>),
                                                       type = available})],
    ?assertMatch([#presence{type = error}], Join),
    ?assertMatch(#stanza_error{reason = 'not-acceptable'}, xmpp:get_error(hd(Join))).

%% in the room of room_forwarded/0, hank taken to power 0:
%% his redaction of an event the gateway does not know reaches nobody;
%% back at the redact level it goes on as the <event/> (no retraction).
l4_unknown_target() ->
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, <<"!multi:example.com">>),
    Hank = peer_id(<<"otto">>),
    PL = fun(Level) ->
                 {ok, _} = gen_statem:call(Pid, {add_event, #{<<"type">> => <<"m.room.power_levels">>,
                                                              <<"sender">> => ?ANN_ID, <<"state_key">> => <<>>,
                                                              <<"content">> => #{<<"users">> => #{?ANN_ID => 100,
                                                                                                  Hank => Level},
                                                                                 <<"redact">> => 50}}},
                                           5000)
         end,
    Redact = fun(Target) ->
                     {ok, _} = gen_statem:call(Pid, {add_event, #{<<"type">> => <<"m.room.redaction">>,
                                                                  <<"sender">> => Hank, <<"redacts">> => Target,
                                                                  <<"content">> => #{<<"redacts">> => Target}}},
                                               5000)
             end,
    session(?ANN_RES),
    PL(0),
    _ = received(),
    Redact(<<"$gone-l4">>),
    ?assertEqual([], [M || #message{} = M <- received()]),
    PL(50),
    _ = received(),
    Redact(<<"$gone-l4b">>),
    [Gone] = [M || #message{} = M <- received()],
    ?assert(lists:member(<<"event">>, names(Gone#message.sub_els))),
    ?assertNot(lists:member(<<"retract">>, names(Gone#message.sub_els))),
    close(?ANN_RES).

%% una joins ann's direct chat, invites tess, who joins: the
%% chat is a room now; ann gets a mediated invite to the room's JID, the
%% matrix_direct row is gone, and tess's message is no 1:1 chat from her.
l5_third_member() ->
    [] = [M || #message{type = error} = M
                   <- exchange(#message{id = <<"u1">>, type = chat, from = j(?ANN_RES),
                                        to = j(peer(<<"una">>)), body = [#text{data = <<"hi una">>}]})],
    {RoomID, Pid} = direct_pid(peer_id(<<"una">>)),
    Una = peer_id(<<"una">>),
    Tess = peer_id(<<"tess">>),
    Add = fun(J) -> {ok, ID} = gen_statem:call(Pid, {add_event, J}, 5000), ID end,
    Member = fun(Sender, Key, M) -> #{<<"type">> => <<"m.room.member">>, <<"sender">> => Sender,
                                      <<"state_key">> => Key, <<"content">> => #{<<"membership">> => M}} end,
    session(?ANN_RES),
    Add(Member(Una, Una, <<"join">>)),
    Add(Member(Una, Tess, <<"invite">>)),
    _ = received(),
    ?assertMatch([_], mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, Una})),
    Add(Member(Tess, Tess, <<"join">>)),
    Got = received(),
    [Inv] = [M || #message{} = M <- Got, xmpp:get_subtag(M, #muc_user{}) /= false],
    #muc_user{invites = [#muc_invite{from = InvFrom}]} = xmpp:get_subtag(Inv, #muc_user{}),
    ?assertEqual(j(peer(<<"una">>)), InvFrom),
    ?assertEqual(<<"!">>, binary:part((Inv#message.from)#jid.luser, 0, 1)),
    ?assertEqual([], mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, Una})),
    ?assertEqual({ok, Pid}, mod_matrix_gw_room:get_room_pid(?HOST, RoomID)),
    Add(#{<<"type">> => <<"m.room.message">>, <<"sender">> => Tess,
          <<"content">> => #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"hi ann">>}}),
    ?assertEqual([], [M || #message{type = chat} = M <- received()]),
    %% ann joins the room's JID: tess's message is in its history
    ok = mod_matrix_gw_room:route(#presence{from = j(?ANN_RES),
                                            to = jid:replace_resource(Inv#message.from, <<"ann">>),
                                            type = available}),
    Joined = received(),
    ?assert(lists:member(<<"hi ann">>, [xmpp:get_text(B) || #message{type = groupchat, body = B} <- Joined])),
    close(?ANN_RES).

%% leave_timeout is ten minutes by default: ann's session
%% gone (closed before the unavailable presence, as c2s does) leaves her a
%% Matrix member of the room of room_forwarded/0, and she comes back with no
%% new join; her leave of the room from a session that goes on is the
%% Matrix leave at once.
l6_leave_grace() ->
    ?assertEqual(600, mod_matrix_gw_opt:leave_timeout(?HOST)),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, <<"!multi:example.com">>),
    RoomJID = <<"!", (integer_to_binary(binary:decode_unsigned(<<"multi">>), 16))/binary,
                "%example.com@", ?GW/binary>>,
    Pres = fun(Type) -> #presence{from = j(?ANN_RES), to = j(<<RoomJID/binary, "/ann">>), type = Type} end,
    Before = last_id(Pid),
    session(?ANN_RES),
    _ = received(),
    close(?ANN_RES),
    ok = mod_matrix_gw_room:route(Pres(unavailable)),
    timer:sleep(300),
    ?assertEqual(Before, last_id(Pid)),
    session(?ANN_RES),
    ok = mod_matrix_gw_room:route(Pres(available)),
    Back = received(),
    ?assertMatch([_ | _], [P || #presence{type = available} = P <- Back]),
    ?assertEqual(Before, last_id(Pid)),
    %% she leaves, her session going on: the leave at once, and the room,
    %% with no local member left, is forgotten
    Shares = fun() -> mnesia:dirty_index_read(matrix_room_share, <<"!multi:example.com">>, 3) end,
    %% (a restored room's members are noted on their next change; noted here)
    ok = mod_matrix_gw_event:membership(?HOST, <<"!multi:example.com">>, peer_id(<<"otto">>), <<"join">>),
    ?assertMatch([_ | _], Shares()),
    Ref = monitor(process, Pid),
    ok = mod_matrix_gw_room:route(Pres(unavailable)),
    receive {'DOWN', Ref, _, _, _} -> ok after 3000 -> error(no_leave) end,
    close(?ANN_RES),
    %% the room forgotten, nobody shares it any more
    ?assertEqual([], Shares()).

%% two messages to a new peer routed at once (a file and its
%% caption, two resources) make one room with both in it.
l8_one_room() ->
    Rooms = length(mnesia:dirty_all_keys(matrix_room)),
    M = fun(ID, Body) -> #message{id = ID, type = chat, from = j(?ANN_RES), to = j(peer(<<"vic">>)),
                                  body = [#text{data = Body}]} end,
    session(?ANN_RES),
    Self = self(),
    Two = spawn(fun() -> ok = mod_matrix_gw_room:route(M(<<"v2">>, <<"second">>)), Self ! two end),
    ok = mod_matrix_gw_room:route(M(<<"v1">>, <<"first">>)),
    receive two -> ok end,
    _ = Two,
    timer:sleep(300),
    _ = received(),
    close(?ANN_RES),
    ?assertEqual(Rooms + 1, length(mnesia:dirty_all_keys(matrix_room))),
    {_, Pid} = direct_pid(peer_id(<<"vic">>)),
    Bodies = [maps:get(<<"body">>, maps:get(<<"content">>, E), none)
              || E <- chain(Pid), maps:get(<<"type">>, E) == <<"m.room.message">>],
    ?assertEqual([<<"first">>, <<"second">>], lists:sort(Bodies)).
