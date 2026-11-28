%%% mod_matrix_gw_rtc's tests (https://ns.another.im/matrix-rtc/0):
%%% the types, the owner check, stickiness,
%%% <state/>, <send/>'s parsing and checks as functions of their input;
%%% then, on an ejabberd started with the gateway on (two foci, small
%%% delayed bounds), the caps gate, <transports/>, <openid/> and the
%%% federation's userinfo, <send/> into a new direct chat and its echo to
%%% the resources that speak MatrixRTC, the txn, the refusals,
%%% <room-state/>, the delayed events (scheduled, listed, restarted, sent,
%%% cancelled, fired, bounded, dropped with a leave and the account, kept
%%% across a restart of the module's process, the offline switch), a room
%%% made here, forward/4's owner check and origin budget, the counters.
%%% The resources' caps and devices are stood in for by persistent terms
%%% (the module's TEST seams).
-module(mod_matrix_gw_rtc_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").

-define(NS, <<"https://ns.another.im/matrix-rtc/0">>).
-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).
-define(ANN, <<"ann@example.com">>).
-define(PHONE, <<"ann@example.com/phone">>).
-define(OLD, <<"ann@example.com/old">>).
-define(ANN_ID, <<"@ann:example.com">>).
-define(M, mod_matrix_gw_rtc).
-define(MEMBER, <<"org.matrix.msc3401.call.member">>).
-define(NOTIFY, <<"org.matrix.msc4075.rtc.notification">>).
-define(KEY, <<"_@ann:example.com_ANNDEV">>).

j(S) -> jid:decode(S).

json(Map) -> iolist_to_binary(misc:json_encode(Map)).

el(Name, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, ?NS} | Attrs], children = Children}.

send_el(Attrs, Content) ->
    el(<<"send">>, Attrs, [{xmlcdata, json(Content)}]).

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------

kinds_test() ->
    ?assertEqual(member_a, ?M:kind(?MEMBER)),
    ?assertEqual(member_b, ?M:kind(<<"m.rtc.member">>)),
    ?assertEqual(member_b, ?M:kind(<<"org.matrix.msc4143.rtc.member">>)),
    ?assertEqual(slot, ?M:kind(<<"m.rtc.slot">>)),
    ?assertEqual(slot, ?M:kind(<<"org.matrix.msc4143.rtc.slot">>)),
    [?assertEqual(notification, ?M:kind(T))
     || T <- [?NOTIFY, <<"m.rtc.notification">>, <<"org.matrix.msc4075.call.notify">>,
              <<"m.call.notify">>]],
    ?assertEqual(decline, ?M:kind(<<"org.matrix.msc4310.rtc.decline">>)),
    ?assertEqual(none, ?M:kind(<<"m.call.invite">>)),
    ?assert(?M:is_rtc_type(?MEMBER)),
    ?assertNot(?M:is_rtc_type(<<"m.room.message">>)),
    ?assertNot(?M:is_rtc_type(undefined)),
    PL = ?M:power_levels(),
    ?assertEqual(0, maps:get(?MEMBER, PL)),
    ?assertEqual(0, maps:get(<<"m.call.notify">>, PL)),
    ?assertEqual(50, maps:get(<<"m.rtc.slot">>, PL)),
    ?assertEqual(9, map_size(PL)),
    ?assertEqual([?NS], ?M:disco_features(undefined)).

owner_test() ->
    O = fun mod_matrix_gw_rtc:owner_ok/4,
    B = <<"@rtcbob:remote.invalid">>,
    ?assert(O(?MEMBER, <<"_@rtcbob:remote.invalid_DEV">>, B, #{})),
    ?assert(O(?MEMBER, <<"_@rtcbob:remote.invalid_DEV_m.callROOM">>, B, #{})),
    ?assert(O(?MEMBER, <<"@rtcbob:remote.invalid_DEV">>, B, #{})),
    ?assert(O(?MEMBER, B, B, #{})),
    %% another user's key, a prefix of the sender's, none
    ?assertNot(O(?MEMBER, <<"_@eve:remote.invalid_DEV">>, B, #{})),
    ?assertNot(O(?MEMBER, <<"_@rtcbob:remote.invalid_">>, B, #{})),
    ?assertNot(O(?MEMBER, <<"_@rtcbob:remote.invalidx_D">>, B, #{})),
    ?assertNot(O(?MEMBER, undefined, B, #{})),
    Member = fun(U, ID, SK) -> #{<<"member">> => #{<<"user_id">> => U, <<"id">> => ID},
                                 <<"sticky_key">> => SK} end,
    ?assert(O(<<"m.rtc.member">>, undefined, B, Member(B, <<"k1">>, <<"k1">>))),
    ?assertNot(O(<<"m.rtc.member">>, undefined, B, Member(<<"@eve:x">>, <<"k1">>, <<"k1">>))),
    ?assertNot(O(<<"m.rtc.member">>, undefined, B, Member(B, <<"k1">>, <<"k2">>))),
    ?assertNot(O(<<"m.rtc.member">>, undefined, B, Member(B, <<>>, <<>>))),
    ?assertNot(O(<<"m.rtc.member">>, undefined, B, #{})),
    ?assert(O(?NOTIFY, undefined, B, #{})).

sticky_test() ->
    S = fun mod_matrix_gw_rtc:sticky_left/2,
    ?assertEqual(none, S(#{}, 1000)),
    ?assertEqual(none, S(#{<<"sticky">> => #{}}, 1000)),
    ?assertEqual(none, S(#{<<"sticky">> => #{<<"duration_ms">> => -1}}, 1000)),
    ?assertEqual(5000, S(#{<<"sticky">> => #{<<"duration_ms">> => 6000},
                           <<"origin_server_ts">> => 1000}, 2000)),
    ?assertEqual(6000, S(#{<<"msc4354_sticky">> => #{<<"duration_ms">> => 6000}}, 2000)),
    %% a stamp in the future counts from now; the duration clamped to an hour
    ?assertEqual(6000, S(#{<<"sticky">> => #{<<"duration_ms">> => 6000},
                           <<"origin_server_ts">> => 9000}, 2000)),
    ?assertEqual(3600000, S(#{<<"sticky">> => #{<<"duration_ms">> => 99999999},
                              <<"origin_server_ts">> => x}, 2000)),
    ?assertEqual(0, S(#{<<"sticky">> => #{<<"duration_ms">> => 10},
                        <<"origin_server_ts">> => 0}, 2000)).

state_el_test() ->
    El = ?M:state_el(#{type => ?MEMBER, id => <<"$e">>, room => <<"!r:x">>, sender => <<"@b:x">>,
                       ts => 17, state_key => <<"_@b:x_D">>, sticky_ms => 9,
                       content => #{<<"a">> => 1}}),
    ?assertEqual([{<<"xmlns">>, ?NS}, {<<"type">>, ?MEMBER}, {<<"id">>, <<"$e">>},
                  {<<"room">>, <<"!r:x">>}, {<<"sender">>, <<"@b:x">>}, {<<"ts">>, <<"17">>},
                  {<<"state_key">>, <<"_@b:x_D">>}, {<<"sticky_ms">>, <<"9">>}], El#xmlel.attrs),
    ?assertEqual(#{<<"a">> => 1}, misc:json_decode(fxml:get_tag_cdata(El))),
    El2 = ?M:state_el(#{type => ?NOTIFY, id => <<"$e">>, room => <<"!r:x">>, sender => <<"@b:x">>,
                        ts => undefined, content => #{}}),
    ?assertEqual(5, length(El2#xmlel.attrs)),
    ?assertEqual(<<"{}">>, fxml:get_tag_cdata(El2)).

redact_log_test() ->
    R = fun(Args) ->
                #{msg := {_, A}} = ?M:redact_log(#{level => debug, msg => {"~p", Args}, meta => #{}}, []),
                A
        end,
    Tok = <<"w7qmNBH-vjmJ3amxItd_6dToOiLtq52Wevrl3pZ7Y6o">>,
    %% the <openid/> result as the router prints it (records, attrs)
    El = #xmlel{name = <<"openid">>, attrs = [{<<"xmlns">>, ?NS}, {<<"access_token">>, Tok},
                                              {<<"token_type">>, <<"Bearer">>}]},
    [#iq{sub_els = [#xmlel{attrs = Attrs}]}] = R([#iq{id = <<"1">>, type = result, sub_els = [El]}]),
    ?assertEqual(<<"[redacted]">>, proplists:get_value(<<"access_token">>, Attrs)),
    ?assertEqual(<<"Bearer">>, proplists:get_value(<<"token_type">>, Attrs)),
    %% the stream's Send XML, the HTTP query, a JSON body, an iolist
    [X1, X2, X3, X4] = R([<<"<openid xmlns='https://ns.another.im/matrix-rtc/0' access_token='", Tok/binary, "' token_type='Bearer'/>">>,
                          <<"/_matrix/federation/v1/openid/userinfo?access_token=", Tok/binary, "&x=1">>,
                          <<"{\"access_token\": \"", Tok/binary, "\"}">>,
                          [<<"a">> | <<"access_token=", Tok/binary>>]]),
    ?assertEqual(<<"<openid xmlns='https://ns.another.im/matrix-rtc/0' access_token='[redacted]' token_type='Bearer'/>">>, X1),
    ?assertEqual(<<"/_matrix/federation/v1/openid/userinfo?access_token=[redacted]&x=1">>, X2),
    ?assertEqual(<<"{\"access_token\": \"[redacted]\"}">>, X3),
    ?assertEqual([<<"a">> | <<"access_token=[redacted]">>], X4),
    %% a Bearer header, as a header pair and in text
    [H1, H2] = R([[{'Authorization', <<"Bearer ", Tok/binary>>}], <<"Authorization: Bearer ", Tok/binary>>]),
    ?assertEqual([{'Authorization', <<"Bearer [redacted]">>}], H1),
    ?assertEqual(<<"Authorization: Bearer [redacted]">>, H2),
    %% nothing else changes; a report and a string are walked too
    Plain = [<<"hello">>, {a, 1}, #{k => <<"v">>}, "text", 3],
    ?assertEqual(Plain, R(Plain)),
    #{msg := {report, #{q := Q}}} = ?M:redact_log(#{level => info, meta => #{},
                                                    msg => {report, #{q => <<"access_token=", Tok/binary>>}}}, []),
    ?assertEqual(<<"access_token=[redacted]">>, Q),
    #{msg := {string, S}} = ?M:redact_log(#{level => info, meta => #{},
                                            msg => {string, <<"access_token=", Tok/binary>>}}, []),
    ?assertEqual(<<"access_token=[redacted]">>, S),
    ?assertEqual(#{level => info}, ?M:redact_log(#{level => info}, [])),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~p", [R([El])])), Tok)).

parse_test() ->
    P = fun mod_matrix_gw_rtc:parse_send/1,
    {ok, S} = P(send_el([{<<"type">>, ?MEMBER}, {<<"state_key">>, ?KEY}, {<<"delay_ms">>, <<"8000">>},
                         {<<"txn">>, <<"t1">>}], #{})),
    ?assertMatch(#{type := ?MEMBER, state_key := ?KEY, delay_ms := 8000, sticky_ms := undefined,
                   txn := <<"t1">>, content := #{}}, S),
    %% an empty state key is one
    {ok, S2} = P(send_el([{<<"type">>, ?MEMBER}, {<<"state_key">>, <<>>}], #{})),
    ?assertEqual(<<>>, maps:get(state_key, S2)),
    ?assertEqual({error, bad_request}, P(el(<<"send">>, [{<<"type">>, ?NOTIFY}], [{xmlcdata, <<"[1]">>}]))),
    ?assertEqual({error, bad_request}, P(el(<<"send">>, [{<<"type">>, ?NOTIFY}], [{xmlcdata, <<"{">>}]))),
    ?assertEqual({error, bad_request}, P(send_el([{<<"type">>, ?NOTIFY}, {<<"delay_ms">>, <<"-1">>}], #{}))),
    ?assertEqual({error, bad_request}, P(send_el([{<<"type">>, ?NOTIFY}, {<<"sticky_ms">>, <<"x">>}], #{}))),
    Big = binary:copy(<<"x">>, 16384),
    ?assertEqual({error, too_large}, P(send_el([{<<"type">>, ?NOTIFY}], #{<<"k">> => Big}))).

check_test() ->
    C = fun(Attrs, Content, Dev) ->
                {ok, S} = mod_matrix_gw_rtc:parse_send(send_el(Attrs, Content)),
                mod_matrix_gw_rtc:check_send(S, ?ANN_ID, Dev)
        end,
    T = fun(Type) -> {<<"type">>, Type} end,
    K = fun(Key) -> {<<"state_key">>, Key} end,
    ?assertEqual({error, bad_type}, C([T(<<"m.room.message">>)], #{}, <<"ANNDEV">>)),
    %% generation A
    ?assertEqual(ok, C([T(?MEMBER), K(?KEY)], #{}, <<"ANNDEV">>)),
    ?assertEqual(ok, C([T(?MEMBER), K(<<?KEY/binary, "_m.callslot">>)], #{}, <<"ANNDEV">>)),
    ?assertEqual({error, state_key}, C([T(?MEMBER), K(<<?KEY/binary, "X">>)], #{}, <<"ANNDEV">>)),
    ?assertEqual({error, state_key}, C([T(?MEMBER), K(<<"_@ann:example.com_OTHER">>)], #{}, <<"ANNDEV">>)),
    ?assertEqual({error, state_key}, C([T(?MEMBER), K(<<"_@rtcbob:remote.invalid_ANNDEV">>)], #{}, <<"ANNDEV">>)),
    ?assertEqual({error, state_key}, C([T(?MEMBER), K(?KEY)], #{}, none)),
    ?assertEqual({error, bad_type}, C([T(?MEMBER)], #{}, <<"ANNDEV">>)),
    ?assertEqual({error, bad_type}, C([T(?MEMBER), K(?KEY), {<<"sticky_ms">>, <<"5">>}], #{}, <<"ANNDEV">>)),
    %% slots
    ?assertEqual(ok, C([T(<<"m.rtc.slot">>), K(<<"m.call#ROOM">>)], #{}, none)),
    ?assertEqual({error, state_key}, C([T(<<"m.rtc.slot">>), K(<<"#ROOM">>)], #{}, none)),
    ?assertEqual({error, state_key}, C([T(<<"m.rtc.slot">>), K(<<"m.call">>)], #{}, none)),
    ?assertEqual({error, bad_type}, C([T(<<"m.rtc.slot">>)], #{}, none)),
    %% generation B: checked, then not sent yet (1.3.x)
    Member = #{<<"member">> => #{<<"user_id">> => ?ANN_ID, <<"device_id">> => <<"ANNDEV">>,
                                 <<"id">> => <<"m1">>}, <<"sticky_key">> => <<"m1">>},
    ?assertEqual({error, sticky}, C([T(<<"m.rtc.member">>), {<<"sticky_ms">>, <<"3600000">>}], Member, <<"ANNDEV">>)),
    ?assertEqual({error, state_key}, C([T(<<"m.rtc.member">>), {<<"sticky_ms">>, <<"1">>}], Member, <<"OTHER">>)),
    ?assertEqual({error, state_key}, C([T(<<"m.rtc.member">>), {<<"sticky_ms">>, <<"1">>}], Member, none)),
    ?assertEqual({error, state_key}, C([T(<<"m.rtc.member">>), {<<"sticky_ms">>, <<"1">>}],
                                       Member#{<<"sticky_key">> => <<"m2">>}, <<"ANNDEV">>)),
    ?assertEqual({error, bad_type}, C([T(<<"m.rtc.member">>)], Member, <<"ANNDEV">>)),
    ?assertEqual({error, bad_type}, C([T(<<"m.rtc.member">>), K(<<"x">>), {<<"sticky_ms">>, <<"1">>}],
                                      Member, <<"ANNDEV">>)),
    %% the ring and the decline: timeline events
    ?assertEqual(ok, C([T(?NOTIFY)], #{}, none)),
    ?assertEqual(ok, C([T(<<"org.matrix.msc4310.rtc.decline">>), {<<"delay_ms">>, <<"5">>}], #{}, none)),
    ?assertEqual({error, bad_type}, C([T(?NOTIFY), K(<<>>)], #{}, none)),
    ?assertEqual({error, bad_type}, C([T(?NOTIFY), {<<"sticky_ms">>, <<"1">>}], #{}, none)).

%%--------------------------------------------------------------------
%% On a running gateway
%%--------------------------------------------------------------------

ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup,
     fun() ->
             Dir = mod_matrix_gw_test_lib:start_ejabberd(?MODULE),
             %% phone speaks MatrixRTC and holds the device ANNDEV, old does not
             persistent_term:put({?M, features},
                                 fun({_, _, R}) -> lists:member(R, [<<"phone">>, <<"tab">>]) end),
             persistent_term:put({?M, devices},
                                 fun(_, <<"ann">>) -> [{<<"ANNDEV">>, <<"phone">>}, {<<"OLDDEV">>, <<"old">>}];
                                    (_, _) -> []
                                 end),
             Dir
     end,
     fun(Dir) ->
             [persistent_term:erase({?M, K}) || K <- [features, devices, resources]],
             mod_matrix_gw_test_lib:stop_ejabberd(Dir)
     end,
     fun(_) ->
             [{timeout, 30, {Name, fun() -> fresh(), F() end}}
              || {Name, F} <- [{"the options and <transports/>", fun transports/0},
                               {"the gate: no caps, not ours, wrong iq types", fun gate/0},
                               {"disco#info names the namespace", fun disco/0},
                               {"<openid/> and userinfo, its refusals and limits", fun openid/0},
                               {"<send/> into a new direct chat, echoed to MatrixRTC resources", fun send_dm/0},
                               {"<send/>'s txn and refusals", fun send_refused/0},
                               {"<room-state/>", fun room_state/0},
                               {"delayed: schedule, list, restart, cancel, send", fun delayed/0},
                               {"delayed: fired when due, bounded", fun delayed_due/0},
                               {"delayed: a leave, the account, the offline switch", fun delayed_drop/0},
                               {"delayed: a restarted process arms the rows", fun delayed_restart/0},
                               {"a room made here: power levels, groupchat echo", fun room_multi/0},
                               {"forward/4: owner check, origin budget, recipients", fun forward/0},
                               {"the caps kept from presence", fun caps_hooks/0},
                               {"the counters and their command", fun counters/0}]]
     end}.

fresh() ->
    flush(),
    [close(F) || F <- [?PHONE, ?OLD, <<"ann@example.com/tab">>]],
    mnesia:clear_table(matrix_delayed),
    [persistent_term:erase({mod_matrix_gw_s2s, limit, L})
     || L <- [rtc_state_user, rtc_openid_user, rtc_userinfo_ip, rtc_delayed_user, rtc_state_origin]],
    catch ets:delete(matrix_gw_rate, {rtc_state_user, {<<"ann">>, ?HOST}}),
    catch ets:delete(matrix_gw_rate, {rtc_delayed_user, {<<"ann">>, ?HOST}}),
    catch ets:delete(matrix_gw_rate, {rtc_openid_user, {<<"ann">>, ?HOST}}),
    ok.

flush() ->
    receive _ -> flush() after 0 -> ok end.

session(Full) ->
    close(Full),
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

%% An iq from From to To with El, routed as the gateway would get it; the
%% answer (the first iq back with its id).
iq(From, To, Type, El) ->
    ID = p1_rand:get_string(),
    IQ = #iq{type = Type, id = ID, from = j(From), to = j(To), sub_els = [El]},
    case j(To) of
        #jid{luser = <<>>} -> mod_matrix_gw:route(IQ);
        _ -> mod_matrix_gw_room:route(IQ)
    end,
    answer(ID, 3000).

answer(ID, Wait) ->
    receive
        {route, #iq{id = ID} = R} -> R
    after Wait -> error({no_answer, ID})
    end.

err(#iq{type = error} = IQ) ->
    #stanza_error{reason = R, sub_els = Els} = xmpp:get_error(IQ),
    {R, [N || #xmlel{name = N} <- Els]};
err(IQ) ->
    {not_an_error, IQ}.

child(#iq{type = result, sub_els = [El]}) -> El.

direct_pid(Peer) -> direct_pid(Peer, 100).
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
    {ok, Event} = gen_statem:call(Pid, {find_event, ID}),
    [JSON] = [M || M <- tuple_to_list(Event), is_map(M), is_map_key(<<"prev_events">>, M)],
    case maps:get(<<"prev_events">>, JSON) of
        [] -> [JSON];
        [Prev | _] -> [JSON | walk(Pid, Prev)]
    end.

states(Pkts) ->
    [{M#message.to, M#message.from, M#message.type, El}
     || #message{} = M <- Pkts, #xmlel{name = <<"state">>} = El <- M#message.sub_els].

transports() ->
    session(?PHONE),
    ?assertEqual(legacy, mod_matrix_gw_opt:rtc_member_format(?HOST)),
    ?assertEqual(100000, mod_matrix_gw_opt:rtc_max_delay_ms(?HOST)),
    ?assertEqual(3, mod_matrix_gw_opt:rtc_max_delayed_per_user(?HOST)),
    ?assertEqual(auto, proplists:get_value(rtc_member_format, mod_matrix_gw:mod_options(?HOST))),
    ?assertEqual([], proplists:get_value(rtc_transports, mod_matrix_gw:mod_options(?HOST))),
    ?assertEqual(86400000, proplists:get_value(rtc_max_delay_ms, mod_matrix_gw:mod_options(?HOST))),
    ?assertEqual(10, proplists:get_value(rtc_max_delayed_per_user, mod_matrix_gw:mod_options(?HOST))),
    ?assertEqual(sticky, (mod_matrix_gw:mod_opt_type(rtc_member_format))(sticky)),
    ?assertEqual(5, (mod_matrix_gw:mod_opt_type(rtc_max_delay_ms))(5)),
    ?assertEqual(5, (mod_matrix_gw:mod_opt_type(rtc_max_delayed_per_user))(5)),
    ?assertEqual([], mod_matrix_gw_opt:rtc_transports(#{rtc_transports => []})),
    ?assertEqual(auto, mod_matrix_gw_opt:rtc_member_format(#{rtc_member_format => auto})),
    ?assertEqual(1, mod_matrix_gw_opt:rtc_max_delay_ms(#{rtc_max_delay_ms => 1})),
    ?assertEqual(1, mod_matrix_gw_opt:rtc_max_delayed_per_user(#{rtc_max_delayed_per_user => 1})),
    El = child(iq(?PHONE, ?GW, get, el(<<"transports">>, [], []))),
    ?assertEqual(<<"legacy">>, fxml:get_tag_attr_s(<<"format">>, El)),
    ?assertEqual([[{<<"type">>, <<"livekit">>},
                   {<<"livekit_service_url">>, <<"https://rtc.example.com/livekit/jwt">>}],
                  [{<<"type">>, <<"m.livekit">>}, {<<"server_name">>, <<"example.com">>},
                   {<<"url">>, <<"wss://sfu.example.com">>}]],
                 [A || #xmlel{name = <<"transport">>, attrs = A} <- El#xmlel.children]).

gate() ->
    session(?OLD),
    ?assertEqual({'feature-not-implemented', []},
                 err(iq(?OLD, ?GW, get, el(<<"transports">>, [], [])))),
    ?assertEqual({'feature-not-implemented', []},
                 err(iq(?OLD, <<"rtcbob%remote.invalid@", ?GW/binary>>, get,
                        el(<<"room-state">>, [{<<"type">>, ?MEMBER}], [])))),
    %% another server's user (a domain this process stands for)
    ejabberd_router:register_route(<<"probe.example">>, ?HOST),
    ?assertEqual({'not-allowed', []},
                 err(iq(<<"eve@probe.example/x">>, ?GW, get, el(<<"openid">>, [], [])))),
    ejabberd_router:unregister_route(<<"probe.example">>),
    session(?PHONE),
    ?assertEqual({'bad-request', []}, err(iq(?PHONE, ?GW, set, el(<<"transports">>, [], [])))),
    ?assertEqual({'bad-request', []}, err(iq(?PHONE, ?GW, set, el(<<"openid">>, [], [])))),
    ?assertEqual({'feature-not-implemented', []},
                 err(iq(?PHONE, ?GW, get, el(<<"livekit-token">>, [{<<"room">>, <<"!r:x">>}], [])))),
    Bob = <<"rtcbob%remote.invalid@", ?GW/binary>>,
    ?assertEqual({'bad-request', []}, err(iq(?PHONE, Bob, get, send_el([{<<"type">>, ?NOTIFY}], #{})))),
    ?assertEqual({'bad-request', []}, err(iq(?PHONE, Bob, set, el(<<"room-state">>, [{<<"type">>, ?MEMBER}], [])))),
    ?assertEqual({'bad-request', []}, err(iq(?PHONE, Bob, get, el(<<"room-state">>, [], [])))),
    %% another namespace's <send/> and <delayed/> are not ours
    ?assertMatch({_, _}, err(iq(?PHONE, Bob, set, #xmlel{name = <<"send">>, attrs = [{<<"xmlns">>, <<"urn:x">>}]}))),
    ?assertEqual(pass, ?M:route_chat_iq(#iq{sub_els = []})),
    ?assertMatch({_, _}, err(iq(?PHONE, ?GW, get, #xmlel{name = <<"delayed">>, attrs = [{<<"xmlns">>, <<"urn:x">>}]}))).

disco() ->
    #iq{type = result, sub_els = [Info]} =
        mod_matrix_gw:process_disco_info(#iq{type = get, id = <<"d">>, from = j(?PHONE), to = j(?GW),
                                             sub_els = [#disco_info{node = <<>>}]}),
    ?assert(lists:member(?NS, Info#disco_info.features)),
    session(?PHONE),
    #iq{type = result, sub_els = [U]} =
        iq(?PHONE, <<"rtcbob%remote.invalid@", ?GW/binary>>, get, xmpp:encode(#disco_info{node = <<>>})),
    #disco_info{features = Fs} = case U of
                                     #xmlel{} -> xmpp:decode(U);
                                     _ -> U
                                 end,
    ?assert(lists:member(?NS, Fs)).

openid() ->
    session(?PHONE),
    El = child(iq(?PHONE, ?GW, get, el(<<"openid">>, [], []))),
    Token = fxml:get_tag_attr_s(<<"access_token">>, El),
    ?assertEqual(43, byte_size(Token)),
    ?assertEqual(<<"Bearer">>, fxml:get_tag_attr_s(<<"token_type">>, El)),
    ?assertEqual(?HOST, fxml:get_tag_attr_s(<<"matrix_server_name">>, El)),
    ?assertEqual(<<"3600">>, fxml:get_tag_attr_s(<<"expires_in">>, El)),
    Req = fun(Q, H) -> #request{method = 'GET', q = Q, headers = H, ip = {{10, 0, 0, 1}, 4000},
                                path = [], host = ?HOST, data = <<>>, raw_path = <<>>} end,
    Sub = fun({200, _, Body}) -> maps:get(<<"sub">>, misc:json_decode(Body));
             (Other) -> Other
          end,
    ?assertEqual(?ANN_ID, Sub(mod_matrix_gw:process([<<"federation">>, <<"v1">>, <<"openid">>, <<"userinfo">>],
                                                    Req([{<<"access_token">>, Token}], [])))),
    ?assertEqual(?ANN_ID, Sub(?M:userinfo(Req([], [{'Authorization', <<"Bearer ", Token/binary>>}])))),
    {401, _, Body} = ?M:userinfo(Req([{<<"access_token">>, <<"nope">>}], [])),
    ?assertEqual(<<"M_UNKNOWN_TOKEN">>, maps:get(<<"errcode">>, misc:json_decode(Body))),
    {401, _, _} = ?M:userinfo(Req([], [])),
    %% expired
    mnesia:dirty_write({matrix_openid, <<"old">>, ?ANN_ID, 1}),
    {401, _, _} = ?M:userinfo(Req([{<<"access_token">>, <<"old">>}], [])),
    {405, _, _} = ?M:userinfo((Req([], []))#request{method = 'POST'}),
    persistent_term:put({mod_matrix_gw_s2s, limit, rtc_userinfo_ip}, 1),
    R2 = fun() -> ?M:userinfo((Req([{<<"access_token">>, Token}], []))#request{ip = {{10, 0, 0, 2}, 1}}) end,
    {200, _, _} = R2(),
    ?assertMatch({429, _, _, _}, R2()),
    %% the user's tokens a minute
    persistent_term:put({mod_matrix_gw_s2s, limit, rtc_openid_user}, 1),
    ets:delete(matrix_gw_rate, {rtc_openid_user, {<<"ann">>, ?HOST}}),
    _ = child(iq(?PHONE, ?GW, get, el(<<"openid">>, [], []))),
    ?assertEqual({'resource-constraint', [<<"limit">>]}, err(iq(?PHONE, ?GW, get, el(<<"openid">>, [], [])))),
    %% the hourly purge takes the expired one
    mod_matrix_gw_rtc:procname(?HOST) ! purge_openid,
    timer:sleep(100),
    ?assertEqual([], mnesia:dirty_read(matrix_openid, <<"old">>)),
    ?assertMatch([_], mnesia:dirty_read(matrix_openid, Token)).

%% ann's phone joins a call with bob: the direct chat is made, the member
%% event is in it with its state key, <sent event/> answers, and the echo
%% reaches phone (and tab), not old.
send_dm() ->
    session(?PHONE), session(?OLD), session(<<"ann@example.com/tab">>),
    Bob = <<"rtcbob%remote.invalid@", ?GW/binary>>,
    Content = #{<<"application">> => <<"m.call">>, <<"call_id">> => <<>>,
                <<"device_id">> => <<"ANNDEV">>, <<"expires">> => 14400000},
    R = iq(?PHONE, Bob, set, send_el([{<<"type">>, ?MEMBER}, {<<"state_key">>, ?KEY},
                                      {<<"txn">>, <<"t1">>}], Content)),
    #xmlel{name = <<"sent">>} = Sent = child(R),
    EventID = fxml:get_tag_attr_s(<<"event">>, Sent),
    {RoomID, Pid} = direct_pid(<<"@rtcbob:remote.invalid">>),
    Chain = chain(Pid),
    Last = lists:last(Chain),
    ?assertMatch(#{<<"type">> := ?MEMBER, <<"state_key">> := ?KEY, <<"sender">> := ?ANN_ID,
                   <<"content">> := Content}, Last),
    %% the room's power levels let anyone in a call
    [PL] = [C || #{<<"type">> := <<"m.room.power_levels">>, <<"content">> := C} <- Chain],
    ?assertEqual(0, maps:get(?MEMBER, maps:get(<<"events">>, PL))),
    ?assertEqual(50, maps:get(<<"m.rtc.slot">>, maps:get(<<"events">>, PL))),
    Got = states(received()),
    ?assertEqual(lists:sort([j(?PHONE), j(<<"ann@example.com/tab">>)]),
                 lists:sort([To || {To, _, _, _} <- Got])),
    [{_, From, chat, St} | _] = Got,
    ?assertEqual(j(Bob), From),
    ?assertEqual(EventID, fxml:get_tag_attr_s(<<"id">>, St)),
    ?assertEqual(RoomID, fxml:get_tag_attr_s(<<"room">>, St)),
    ?assertEqual(?ANN_ID, fxml:get_tag_attr_s(<<"sender">>, St)),
    ?assertEqual(?KEY, fxml:get_tag_attr_s(<<"state_key">>, St)),
    ?assertEqual(Content, misc:json_decode(fxml:get_tag_cdata(St))),
    %% the same txn: the same answer, nothing sent
    R2 = iq(?PHONE, Bob, set, send_el([{<<"type">>, ?MEMBER}, {<<"state_key">>, ?KEY},
                                       {<<"txn">>, <<"t1">>}], Content)),
    ?assertEqual(Sent, child(R2)),
    ?assertEqual(Chain, chain(Pid)),
    %% the ring, a timeline event, then the leave
    #xmlel{name = <<"sent">>} = child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?NOTIFY}],
                                                                  #{<<"notification_type">> => <<"ring">>}))),
    #xmlel{name = <<"sent">>} = child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?MEMBER},
                                                                   {<<"state_key">>, ?KEY}], #{}))),
    ?assertMatch([#{<<"type">> := ?NOTIFY}, #{<<"type">> := ?MEMBER, <<"content">> := #{}}],
                 lists:nthtail(length(Chain), chain(Pid))),
    close(?OLD), close(<<"ann@example.com/tab">>).

send_refused() ->
    session(?PHONE), session(?OLD),
    Bob = <<"rtcbob%remote.invalid@", ?GW/binary>>,
    E = fun(Attrs, Content) -> err(iq(?PHONE, Bob, set, send_el(Attrs, Content))) end,
    ?assertEqual({'not-acceptable', [<<"bad-type">>]}, E([{<<"type">>, <<"m.room.message">>}], #{})),
    ?assertEqual({'not-acceptable', [<<"bad-type">>]}, E([{<<"type">>, ?MEMBER}], #{})),
    ?assertEqual({'not-acceptable', [<<"state-key">>]},
                 E([{<<"type">>, ?MEMBER}, {<<"state_key">>, <<"_@ann:example.com_OLDDEV">>}], #{})),
    ?assertEqual({'not-acceptable', [<<"too-large">>]},
                 E([{<<"type">>, ?NOTIFY}], #{<<"x">> => binary:copy(<<"y">>, 17000)})),
    ?assertEqual({'bad-request', []}, err(iq(?PHONE, Bob, set, el(<<"send">>, [{<<"type">>, ?NOTIFY}],
                                                                 [{xmlcdata, <<"nope">>}])))),
    ?assertEqual({'feature-not-implemented', []},
                 E([{<<"type">>, <<"m.rtc.member">>}, {<<"sticky_ms">>, <<"1000">>}],
                   #{<<"member">> => #{<<"user_id">> => ?ANN_ID, <<"device_id">> => <<"ANNDEV">>,
                                       <<"id">> => <<"m">>}, <<"sticky_key">> => <<"m">>})),
    ?assertEqual({'not-acceptable', []},
                 E([{<<"type">>, ?NOTIFY}, {<<"delay_ms">>, <<"100001">>}], #{})),
    %% old's device is its own: a member event of OLDDEV from old has no caps
    ?assertEqual({'feature-not-implemented', []},
                 err(iq(?OLD, Bob, set, send_el([{<<"type">>, ?MEMBER},
                                                {<<"state_key">>, <<"_@ann:example.com_OLDDEV">>}], #{})))),
    %% a room the gateway does not have
    ?assertEqual({'item-not-found', []},
                 err(iq(?PHONE, <<"!nowhere%remote.invalid@", ?GW/binary>>, set,
                        send_el([{<<"type">>, ?NOTIFY}], #{})))),
    %% a slot the room's levels refuse? ann is at 100 in her direct chat;
    %% a remote user's room is not hers: item-not-found
    %% the rate: 60 a minute, here 1
    persistent_term:put({mod_matrix_gw_s2s, limit, rtc_state_user}, 1),
    ets:delete(matrix_gw_rate, {rtc_state_user, {<<"ann">>, ?HOST}}),
    #xmlel{name = <<"sent">>} = child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?NOTIFY}], #{}))),
    ?assertEqual({'resource-constraint', [<<"limit">>]}, E([{<<"type">>, ?NOTIFY}], #{})).

room_state() ->
    session(?PHONE),
    %% a direct chat that does not exist yet: nothing
    Carol = <<"rtccarol%remote.invalid@", ?GW/binary>>,
    #xmlel{name = <<"room-state">>, children = []} =
        child(iq(?PHONE, Carol, get, el(<<"room-state">>, [{<<"type">>, ?MEMBER}], []))),
    Bob = <<"rtcbob%remote.invalid@", ?GW/binary>>,
    #xmlel{name = <<"sent">>} = child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?MEMBER},
                                                                   {<<"state_key">>, ?KEY}],
                                                                  #{<<"call_id">> => <<>>}))),
    El = child(iq(?PHONE, Bob, get, el(<<"room-state">>, [],
                                       [#xmlel{name = <<"type">>, children = [{xmlcdata, ?MEMBER}]},
                                        #xmlel{name = <<"type">>, children = [{xmlcdata, <<"m.rtc.member">>}]}]))),
    [St] = El#xmlel.children,
    ?assertEqual(?KEY, fxml:get_tag_attr_s(<<"state_key">>, St)),
    ?assertEqual(#{<<"call_id">> => <<>>}, misc:json_decode(fxml:get_tag_cdata(St))),
    %% other types: the room's members are not RTC state, but asked they come
    El2 = child(iq(?PHONE, Bob, get, el(<<"room-state">>, [{<<"type">>, <<"m.room.member">>}], []))),
    ?assertEqual(2, length(El2#xmlel.children)).

delayed() ->
    session(?PHONE),
    Bob = <<"rtcbob%remote.invalid@", ?GW/binary>>,
    S = child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?MEMBER}, {<<"state_key">>, ?KEY},
                                            {<<"delay_ms">>, <<"50000">>}, {<<"txn">>, <<"d1">>}], #{}))),
    ID = fxml:get_tag_attr_s(<<"delay_id">>, S),
    ?assertMatch(<<"d-", _/binary>>, ID),
    ?assertEqual(<<>>, fxml:get_tag_attr_s(<<"event">>, S)),
    {RoomID, Pid} = direct_pid(<<"@rtcbob:remote.invalid">>),
    N0 = length(chain(Pid)),
    ?assert(maps:is_key(ID, ?M:timers(?HOST))),
    L = child(iq(?PHONE, ?GW, get, el(<<"delayed">>, [], []))),
    [Item] = L#xmlel.children,
    ?assertEqual(ID, fxml:get_tag_attr_s(<<"id">>, Item)),
    ?assertEqual(RoomID, fxml:get_tag_attr_s(<<"room">>, Item)),
    ?assertEqual(?KEY, fxml:get_tag_attr_s(<<"state_key">>, Item)),
    Due = binary_to_integer(fxml:get_tag_attr_s(<<"due_ms">>, Item)),
    ?assert(Due > 40000 andalso Due =< 50000),
    A = fun(Id, Action) -> iq(?PHONE, ?GW, set, el(<<"delayed">>, [{<<"id">>, Id}, {<<"action">>, Action}], [])) end,
    #iq{type = result} = A(ID, <<"restart">>),
    ?assertEqual({'bad-request', []}, err(A(ID, <<"nap">>))),
    ?assertEqual({'item-not-found', []}, err(A(<<"d-nope">>, <<"send">>))),
    %% send: the leave now, the row gone
    #iq{type = result} = A(ID, <<"send">>),
    timer:sleep(200),
    ?assertMatch(#{<<"type">> := ?MEMBER, <<"state_key">> := ?KEY, <<"content">> := #{}},
                 lists:last(chain(Pid))),
    ?assertEqual(N0 + 1, length(chain(Pid))),
    ?assertEqual({'item-not-found', []}, err(A(ID, <<"cancel">>))),
    %% a second, cancelled
    S2 = child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?NOTIFY}, {<<"delay_ms">>, <<"50000">>}], #{}))),
    ID2 = fxml:get_tag_attr_s(<<"delay_id">>, S2),
    #iq{type = result} = A(ID2, <<"cancel">>),
    ?assertEqual([], (child(iq(?PHONE, ?GW, get, el(<<"delayed">>, [], []))))#xmlel.children),
    ?assertNot(maps:is_key(ID2, ?M:timers(?HOST))),
    %% another user's row is not found by ann
    mnesia:dirty_write({matrix_delayed, <<"d-ben">>, {<<"ben">>, ?HOST}, <<"r">>, RoomID, ?NOTIFY,
                        undefined, #{}, 1000, erlang:system_time(millisecond) + 99999, 0}),
    ?assertEqual({'item-not-found', []}, err(A(<<"d-ben">>, <<"cancel">>))),
    %% the rate of actions
    persistent_term:put({mod_matrix_gw_s2s, limit, rtc_delayed_user}, 1),
    ets:delete(matrix_gw_rate, {rtc_delayed_user, {<<"ann">>, ?HOST}}),
    _ = iq(?PHONE, ?GW, get, el(<<"delayed">>, [], [])),
    ?assertEqual({'resource-constraint', [<<"limit">>]}, err(iq(?PHONE, ?GW, get, el(<<"delayed">>, [], [])))),
    ?assertEqual({'resource-constraint', [<<"limit">>]}, err(A(<<"d-ben">>, <<"cancel">>))).

delayed_due() ->
    session(?PHONE),
    Bob = <<"rtcbob%remote.invalid@", ?GW/binary>>,
    {_, Pid} = direct_pid(<<"@rtcbob:remote.invalid">>),
    N0 = length(chain(Pid)),
    S = child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?MEMBER}, {<<"state_key">>, ?KEY},
                                            {<<"delay_ms">>, <<"300">>}], #{}))),
    ID = fxml:get_tag_attr_s(<<"delay_id">>, S),
    timer:sleep(900),
    ?assertEqual([], mnesia:dirty_read(matrix_delayed, ID)),
    ?assertEqual(N0 + 1, length(chain(Pid))),
    %% three rows at most (the suite's bound)
    [child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?NOTIFY}, {<<"delay_ms">>, <<"60000">>}], #{})))
     || _ <- [1, 2, 3]],
    ?assertEqual({'resource-constraint', [<<"limit">>]},
                 err(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?NOTIFY}, {<<"delay_ms">>, <<"60000">>}], #{})))),
    %% a row whose room is gone: dropped when it fires
    mnesia:dirty_write({matrix_delayed, <<"d-gone">>, {<<"ann">>, ?HOST}, <<"phone">>, <<"!gone:x">>,
                        ?NOTIFY, undefined, #{}, 10, erlang:system_time(millisecond), 0}),
    gen_server:cast(?M:procname(?HOST), {arm, <<"d-gone">>}),
    timer:sleep(200),
    ?assertEqual([], mnesia:dirty_read(matrix_delayed, <<"d-gone">>)),
    %% a timer whose row was restarted later arms again
    Later = erlang:system_time(millisecond) + 60000,
    mnesia:dirty_write({matrix_delayed, <<"d-later">>, {<<"ann">>, ?HOST}, <<"phone">>, <<"!gone:x">>,
                        ?NOTIFY, undefined, #{}, 10, Later, 0}),
    ?M:procname(?HOST) ! {fire, <<"d-later">>},
    ?M:procname(?HOST) ! {fire, <<"d-none">>},
    timer:sleep(100),
    ?assert(maps:is_key(<<"d-later">>, ?M:timers(?HOST))).

delayed_drop() ->
    session(?PHONE),
    Bob = <<"rtcbob%remote.invalid@", ?GW/binary>>,
    {RoomID, _} = direct_pid(<<"@rtcbob:remote.invalid">>),
    New = fun() -> fxml:get_tag_attr_s(<<"delay_id">>,
                                       child(iq(?PHONE, Bob, set, send_el([{<<"type">>, ?NOTIFY},
                                                                           {<<"delay_ms">>, <<"60000">>}], #{}))))
          end,
    ID1 = New(),
    %% another user's leave, a join: nothing
    ok = ?M:membership(?HOST, RoomID, <<"@rtcbob:remote.invalid">>, <<"leave">>),
    ok = ?M:membership(?HOST, RoomID, ?ANN_ID, <<"join">>),
    ok = ?M:membership(?HOST, RoomID, <<"@:example.com">>, <<"leave">>),
    ?assertMatch([_], mnesia:dirty_read(matrix_delayed, ID1)),
    ok = ?M:membership(?HOST, <<"!other:x">>, ?ANN_ID, <<"leave">>),
    ?assertMatch([_], mnesia:dirty_read(matrix_delayed, ID1)),
    %% ann's leave of the room
    ok = ?M:membership(?HOST, RoomID, ?ANN_ID, <<"leave">>),
    ?assertEqual([], mnesia:dirty_read(matrix_delayed, ID1)),
    %% the offline switch: a row due later than its delay from now is
    %% brought back to it when its resource goes
    Far = erlang:system_time(millisecond) + 99999999,
    mnesia:dirty_write({matrix_delayed, <<"d-far">>, {<<"ann">>, ?HOST}, <<"phone">>, RoomID,
                        ?NOTIFY, undefined, #{}, 60000, Far, 0}),
    ID2 = New(),
    ?M:unset_presence_hook(<<"ann">>, ?HOST, <<"phone">>, <<>>),
    timer:sleep(100),
    [{matrix_delayed, _, _, _, _, _, _, _, _, Due, _}] = mnesia:dirty_read(matrix_delayed, <<"d-far">>),
    ?assert(Due < Far),
    ?assertMatch([_], mnesia:dirty_read(matrix_delayed, ID2)),
    %% the account deleted: its rows and tokens go
    mnesia:dirty_write({matrix_openid, <<"tok">>, ?ANN_ID, erlang:system_time(millisecond) + 99999}),
    ok = ?M:remove_user(<<"ann">>, ?HOST),
    ?assertEqual([], mnesia:dirty_read(matrix_delayed, ID2)),
    ?assertEqual([], mnesia:dirty_read(matrix_openid, <<"tok">>)).

delayed_restart() ->
    Due = erlang:system_time(millisecond) + 60000,
    mnesia:dirty_write({matrix_delayed, <<"d-keep">>, {<<"ann">>, ?HOST}, <<"phone">>, <<"!r:x">>,
                        ?NOTIFY, undefined, #{}, 60000, Due, 0}),
    Old = whereis(?M:procname(?HOST)),
    exit(Old, kill),
    wait_new(Old, 100),
    ?assert(maps:is_key(<<"d-keep">>, ?M:timers(?HOST))),
    ?assertEqual({error, badarg}, gen_server:call(?M:procname(?HOST), what)),
    gen_server:cast(?M:procname(?HOST), what),
    ?M:procname(?HOST) ! what,
    ?assertEqual({ok, st}, ?M:code_change(old, st, x)),
    gen_server:cast(?M:procname(?HOST), {disarm, <<"d-keep">>}),
    ?assertNot(maps:is_key(<<"d-keep">>, ?M:timers(?HOST))).

wait_new(Old, 0) -> error({not_restarted, Old});
wait_new(Old, N) ->
    case whereis(?M:procname(?HOST)) of
        P when is_pid(P), P /= Old -> ok;
        _ -> timer:sleep(20), wait_new(Old, N - 1)
    end.

%% ann makes #rtc%example.com by joining it: its power levels have the RTC
%% types, her phone's ring comes back as groupchat from the room's JID.
room_multi() ->
    Room = <<"#rtc%example.com@", ?GW/binary>>,
    session(?PHONE),
    ok = mod_matrix_gw_room:route(#presence{from = j(?PHONE), to = j(<<Room/binary, "/ann">>),
                                            type = available, sub_els = [xmpp:encode(#muc{})]}),
    _ = received(),
    {ok, RoomID} = mod_matrix_gw_muc:alias_lookup(<<"#rtc:example.com">>),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
    [PL] = [C || #{<<"type">> := <<"m.room.power_levels">>, <<"content">> := C} <- chain(Pid)],
    ?assertEqual(50, maps:get(<<"org.matrix.msc4143.rtc.slot">>, maps:get(<<"events">>, PL))),
    R = iq(?PHONE, Room, set, send_el([{<<"type">>, ?NOTIFY}], #{<<"notification_type">> => <<"ring">>})),
    #xmlel{name = <<"sent">>} = child(R),
    [{To, From, groupchat, St}] = states(received()),
    ?assertEqual(j(?PHONE), To),
    ?assertEqual(jid:remove_resource(j(<<Room/binary, "/ann">>)), jid:remove_resource(From)),
    ?assertEqual(RoomID, fxml:get_tag_attr_s(<<"room">>, St)),
    %% a slot (ann is the room's 100)
    #xmlel{name = <<"sent">>} =
        child(iq(?PHONE, Room, set, send_el([{<<"type">>, <<"m.rtc.slot">>}, {<<"state_key">>, <<"m.call#ROOM">>}],
                                           #{<<"application">> => #{<<"type">> => <<"m.call">>}}))),
    El = child(iq(?PHONE, Room, get, el(<<"room-state">>, [{<<"type">>, <<"m.rtc.slot">>}], []))),
    ?assertEqual([<<"m.call#ROOM">>], [fxml:get_tag_attr_s(<<"state_key">>, C) || C <- El#xmlel.children]),
    %% she leaves, the last of us: the gateway is out of the room
    ok = mod_matrix_gw_room:route(#presence{from = j(?PHONE), to = j(<<Room/binary, "/ann">>),
                                            type = unavailable}),
    _ = received(),
    ?assertEqual({'item-not-found', []},
                 err(iq(?PHONE, Room, set, send_el([{<<"type">>, ?NOTIFY}], #{})))),
    ?assertEqual({'item-not-found', []},
                 err(iq(?PHONE, Room, get, el(<<"room-state">>, [{<<"type">>, <<"m.rtc.slot">>}], [])))).

forward() ->
    session(?PHONE), session(?OLD),
    Bob = j(<<"rtcbob%remote.invalid@", ?GW/binary>>),
    F = #{type => ?MEMBER, id => <<"$f">>, room => <<"!r:remote.invalid">>,
          sender => <<"@rtcbob:remote.invalid">>, ts => 5, state_key => <<"_@rtcbob:remote.invalid_B">>,
          content => #{}},
    Rcpt = [{j(?ANN), Bob, chat}],
    ok = ?M:forward(?HOST, F, <<"remote.invalid">>, Rcpt),
    [{To, Bob, chat, _}] = states(received()),
    ?assertEqual(j(?PHONE), To),
    %% a forged state key: stored by the room, not forwarded, counted
    C0 = proplists:get_value(forged, ?M:status(?HOST), 0),
    ok = ?M:forward(?HOST, F#{state_key => <<"_@ann:example.com_ANNDEV">>}, <<"remote.invalid">>, Rcpt),
    ?assertEqual([], states(received())),
    ?assertEqual(C0 + 1, proplists:get_value(forged, ?M:status(?HOST))),
    %% the origin's budget
    persistent_term:put({mod_matrix_gw_s2s, limit, rtc_state_origin}, 1),
    ets:delete(matrix_gw_rate, {rtc_state_origin, <<"over.invalid">>}),
    ok = ?M:forward(?HOST, F#{sender => <<"@x:over.invalid">>, state_key => <<"_@x:over.invalid_D">>},
                    <<"over.invalid">>, Rcpt),
    ok = ?M:forward(?HOST, F#{sender => <<"@x:over.invalid">>, state_key => <<"_@x:over.invalid_D">>},
                    <<"over.invalid">>, Rcpt),
    ?assertEqual(1, length(states(received()))),
    ?assert(proplists:get_value(origin_over, ?M:status(?HOST)) >= 1),
    %% a local sender is not counted against any origin
    ok = ?M:forward(?HOST, F#{sender => ?ANN_ID, state_key => ?KEY}, ?HOST, Rcpt),
    ?assertEqual(1, length(states(received()))),
    %% the resources seam: none
    persistent_term:erase({mod_matrix_gw_s2s, limit, rtc_state_origin}),
    persistent_term:put({?M, resources}, fun(_, _) -> [] end),
    ok = ?M:forward(?HOST, F, <<"remote.invalid">>, Rcpt),
    persistent_term:erase({?M, resources}),
    ?assertEqual([], states(received())).

caps_hooks() ->
    %% the real gate: no mod_caps here, so no feature, whatever the caps
    Seam = persistent_term:get({?M, features}),
    persistent_term:erase({?M, features}),
    try
        P = #presence{sub_els = [#caps{node = <<"n">>, version = <<"v">>, hash = <<"sha-1">>}]},
        ?M:set_presence_hook(<<"ann">>, ?HOST, <<"phone">>, P),
        ?assertMatch([{_, #caps{}}], ets:lookup(?M:mem(?HOST), {caps, <<"ann">>, ?HOST, <<"phone">>})),
        ?assertNot(?M:has_feature(?HOST, {<<"ann">>, ?HOST, <<"phone">>})),
        ?assertNot(?M:has_feature(?HOST, {<<"ann">>, ?HOST, <<"none">>})),
        ?M:unset_presence_hook(<<"ann">>, ?HOST, <<"phone">>, <<>>),
        ?assertEqual([], ets:lookup(?M:mem(?HOST), {caps, <<"ann">>, ?HOST, <<"phone">>}))
    after
        persistent_term:put({?M, features}, Seam)
    end.

counters() ->
    ?M:audit(?HOST, test_kind, <<"@x:y">>, <<"!r">>, #{}),
    ?M:audit(?HOST, test_kind, <<"@x:y">>, <<"!r">>, #{}),
    ?M:audit(?HOST, forged, <<"@x:y">>, <<"!r">>, #{}),
    ?assertEqual(2, proplists:get_value(test_kind, ?M:status(?HOST))),
    ?assertEqual(2, proplists:get_value(<<"test_kind">>, ?M:status_command(?HOST))),
    [Cmd] = ?M:get_commands_spec(),
    ?assertEqual(matrix_gw_rtc_status, element(2, Cmd)),
    %% the txn memory purged once a minute (nothing young goes)
    ets:insert(?M:mem(?HOST), {{txn, x, y, z}, el, 0}),
    ?M:procname(?HOST) ! purge,
    timer:sleep(100),
    ?assertEqual([], ets:lookup(?M:mem(?HOST), {txn, x, y, z})).
