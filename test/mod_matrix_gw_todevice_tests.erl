%%% The Matrix gateway's per-device queue (mod_matrix_gw_todevice):
%%% the items on the wire and <to-device/>'s parsing as functions of their
%%% input; then, on an ejabberd started with the gateway, the queue against
%%% a device that is this process (a session of ann's): order, one item in
%%% flight, result, error, timeout, presence, <resume/>, <to-device/> to
%%% Matrix (grouped per server) and to our own devices, m.direct_to_device
%%% from Matrix, the bound and its warning, the txn dedup and a courier
%%% restarted with its rows. The other modules are stood in for by
%%% this one (devices_of/2 and otk_count/3 of mod_matrix_gw_e2e,
%%% queue_edu/3 of mod_matrix_gw_s2s).
-module(mod_matrix_gw_todevice_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-export([devices_of/2, resource_of/3, otk_count/3, queue_edu/3, room_share/2]).

-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).
-define(NS, <<"https://ns.another.im/matrix-e2e/0">>).
-define(M, mod_matrix_gw_todevice).
-define(ANN, {<<"ann">>, ?HOST}).

%%--------------------------------------------------------------------
%% The stand-ins
%%--------------------------------------------------------------------

devices_of(Host, User) ->
    maps:get({User, Host}, persistent_term:get({?MODULE, devices}, #{}), []).

%% a replaced device is still where it was (the {?MODULE, replaced} map)
resource_of(Host, User, Dev) ->
    case maps:get({User, Host, Dev}, persistent_term:get({?MODULE, replaced}, #{}), none) of
        none ->
            case lists:keyfind(Dev, 1, devices_of(Host, User)) of
                {_, R} -> {ok, R};
                false -> error
            end;
        R ->
            {ok, R}
    end.

otk_count(_Host, _User, _Dev) ->
    7.

%% who shares a room with MXID: ann, unless the {?MODULE, shares} map says
room_share(_Host, MXID) ->
    maps:get(MXID, persistent_term:get({?MODULE, shares}, #{}), [<<"ann">>]).

queue_edu(_Host, Server, EDU) ->
    persistent_term:get({?MODULE, pid}) ! {edu, Server, EDU},
    ok.

set_devices(Map) ->
    persistent_term:put({?MODULE, devices}, Map).

el(Name, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, ?NS} | Attrs], children = Children}.

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------

item_el_test_() ->
    E = fun mod_matrix_gw_todevice:item_el/3,
    [?_assertEqual(el(<<"to-device">>, [{<<"seq">>, <<"3">>}, {<<"type">>, <<"m.dummy">>},
                                        {<<"sender">>, <<"@bob:m.org">>}, {<<"message_id">>, <<"t1">>},
                                        {<<"device">>, <<"DEV">>}], [{xmlcdata, <<"{}">>}]),
                   E(<<"DEV">>, 3, {to_device, <<"m.dummy">>, <<"@bob:m.org">>, <<"t1">>, <<"{}">>})),
     ?_assertEqual(el(<<"devices-changed">>, [{<<"seq">>, <<"1">>}],
                      [#xmlel{name = <<"user">>, attrs = [{<<"id">>, <<"@a:b">>}, {<<"stream_id">>, <<"5">>}]},
                       #xmlel{name = <<"user">>, attrs = [{<<"id">>, <<"@c:d">>}, {<<"stream_id">>, <<"x">>}]}]),
                   E(<<"DEV">>, 1, {devices_changed, [{<<"@a:b">>, 5}, {<<"@c:d">>, <<"x">>}]})),
     ?_assertEqual(el(<<"keys-count">>, [{<<"seq">>, <<"2">>}, {<<"algorithm">>, <<"signed_curve25519">>},
                                         {<<"count">>, <<"4">>}], []),
                   E(<<"DEV">>, 2, {keys_count, <<"signed_curve25519">>, 4, false})),
     ?_assertEqual(el(<<"keys-count">>, [{<<"seq">>, <<"2">>}, {<<"algorithm">>, <<"signed_curve25519">>},
                                         {<<"count">>, <<"0">>}, {<<"fallback_used">>, <<"1">>}], []),
                   E(<<"DEV">>, 2, {keys_count, <<"signed_curve25519">>, 0, true})),
     ?_assertEqual(el(<<"device-deleted">>, [{<<"seq">>, <<"9">>}, {<<"id">>, <<"OLD">>}], []),
                   E(<<"DEV">>, 9, {device_deleted, <<"OLD">>}))].

to_device_el(Type, Txn, Users) ->
    el(<<"to-device">>, [{<<"type">>, Type}, {<<"txn">>, Txn}],
       [#xmlel{name = <<"user">>, attrs = [{<<"id">>, U}],
               children = [#xmlel{name = <<"device">>, attrs = [{<<"id">>, D}],
                                  children = [{xmlcdata, J}]} || {D, J} <- Ds]}
        || {U, Ds} <- Users]).

parse_test_() ->
    P = fun mod_matrix_gw_todevice:to_device_parse/1,
    Big = binary:copy(<<"x">>, 65536),
    [?_assertEqual({ok, [{{<<"bob">>, <<"m.org">>, <<"@bob:m.org">>},
                          [{<<"D1">>, #{<<"a">> => 1}}, {<<"*">>, #{}}]}]},
                   P(to_device_el(<<"m.dummy">>, <<"t">>, [{<<"@bob:m.org">>, [{<<"D1">>, <<"{\"a\":1}">>},
                                                                               {<<"*">>, <<"{}">>},
                                                                               {<<>>, <<"{}">>}]}]))),
     ?_assertEqual({ok, []}, P(to_device_el(<<"m.dummy">>, <<"t">>, []))),
     ?_assertEqual(error, P(to_device_el(<<"m.dummy">>, <<"t">>, [{<<"bob">>, [{<<"D">>, <<"{}">>}]}]))),
     ?_assertEqual(error, P(to_device_el(<<"m.dummy">>, <<"t">>, [{<<"@bob:m.org">>, [{<<"D">>, <<"[1]">>}]}]))),
     ?_assertEqual(error, P(to_device_el(<<"m.dummy">>, <<"t">>, [{<<"@bob:m.org">>, [{<<"D">>, <<"{">>}]}]))),
     ?_assertEqual(too_large, P(to_device_el(<<"m.dummy">>, <<"t">>,
                                             [{<<"@bob:m.org">>, [{<<"D">>, <<"{\"k\":\"", Big/binary, "\"}">>}]}])))].

%%--------------------------------------------------------------------
%% On a running gateway
%%--------------------------------------------------------------------

ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup,
     fun() ->
             Dir = mod_matrix_gw_test_lib:start_ejabberd(?MODULE),
             persistent_term:put({?M, e2e}, ?MODULE),
             persistent_term:put({?M, s2s}, ?MODULE),
             persistent_term:put({?M, iq_timeout}, 1000),
             Dir
     end,
     fun(Dir) ->
             [persistent_term:erase({?M, K}) || K <- [e2e, s2s, iq_timeout]],
             mod_matrix_gw_test_lib:stop_ejabberd(Dir)
     end,
     fun(_) ->
             [{timeout, 30, {Name, fun() -> fresh(), F() end}}
              || {Name, F} <- [{"the option", fun options/0},
                               {"order kept, one in flight, result deletes", fun order/0},
                               {"an error stops the queue until <resume/>", fun error_stops/0},
                               {"not-mine: logged, the item kept", fun not_mine/0},
                               {"an item refused three times is dropped", fun refused_dropped/0},
                               {"no answer: stopped, resumed", fun timeout/0},
                               {"presence starts and stops the device", fun presence/0},
                               {"a replaced device's item on its resource's presence", fun replaced/0},
                               {"<resume/>: the answer, the refusals", fun resume_iq/0},
                               {"<to-device/>: one EDU per server, our devices' rows", fun to_device_iq/0},
                               {"<to-device/>: too large, malformed, the txn deduplicated", fun to_device_refused/0},
                               {"m.direct_to_device from Matrix, * expanded", fun inbound/0},
                               {"inbound rows only for who shares a room, or wrote first", fun s6_share/0},
                               {"inbound rows and bytes within the origin's and the gateway's", fun s6_budget/0},
                               {"queue/4 to all devices", fun queue_all/0},
                               {"todevice_max and its warning", fun bound/0},
                               {"a restarted courier keeps the rows and serves", fun restart/0},
                               {"a device back while an item is in flight gets it again", fun a10_again/0},
                               {"a replaced session's late unset_presence leaves the device", fun a10_late_stop/0},
                               {"device-list changes and key counts are coalesced", fun a12_coalesce/0},
                               {"drop/3 empties a device's queue, the seq goes on", fun a15_drop/0},
                               {"the courier's leftovers", fun courier/0}]]
     end}.

fresh() ->
    persistent_term:put({?MODULE, pid}, self()),
    mnesia:clear_table(matrix_todevice),
    set_devices(#{?ANN => [{<<"DEV1">>, <<"r1">>}, {<<"DEV2">>, <<"r2">>}]}),
    [close(R) || R <- [<<"r1">>, <<"r2">>, <<"r9">>]],
    %% the courier forgets every device of ann's
    [?M:unset_presence_hook(<<"ann">>, ?HOST, R, <<>>) || R <- [<<"r1">>, <<"r2">>, <<"r3">>]],
    timer:sleep(50),
    flush().

flush() ->
    receive _ -> flush() after 0 -> ok end.

j(B) -> jid:decode(B).

session(Res) ->
    close(Res),
    ejabberd_sm:open_session({erlang:timestamp(), self()}, <<"ann">>, ?HOST, Res, 0, []).

close(Res) ->
    case ejabberd_sm:get_session_sid(<<"ann">>, ?HOST, Res) of
        none -> ok;
        SID -> ejabberd_sm:close_session(SID, <<"ann">>, ?HOST, Res)
    end.

%% The next iq the gateway sends to this process, or none in Wait ms.
next_iq(Wait) ->
    receive
        {route, #iq{type = set, from = #jid{lserver = ?GW}} = IQ} -> IQ;
        {route, _} -> next_iq(Wait)
    after Wait -> none
    end.

seq(#iq{sub_els = [#xmlel{} = El]}) ->
    binary_to_integer(fxml:get_tag_attr_s(<<"seq">>, El)).

ack(IQ) ->
    ejabberd_router:route(xmpp:make_iq_result(IQ)).

dummy(N) ->
    {to_device, <<"m.dummy">>, <<"@bob:remote.invalid">>, integer_to_binary(N), #{<<"n">> => N}}.

seqs(Dev) -> mod_matrix_gw_todevice:items(?ANN, Dev).

%% <resume/> from ann's resource Res, answered.
resume(Res) ->
    IQ = #iq{type = set, id = <<"res">>, from = j(<<"ann@example.com/", Res/binary>>), to = j(?GW),
             sub_els = [el(<<"resume">>, [], [])]},
    mod_matrix_gw:route(IQ),
    receive
        {route, #iq{id = <<"res">>} = R} -> R
    after 2000 -> error(no_answer)
    end.

options() ->
    ?assertEqual(10000, mod_matrix_gw_opt:todevice_max(?HOST)),
    ?assertEqual(5, mod_matrix_gw_opt:todevice_max(#{todevice_max => 5})),
    ?assertEqual(10000, proplists:get_value(todevice_max, mod_matrix_gw:mod_options(?HOST))),
    ?assertEqual(3, (mod_matrix_gw:mod_opt_type(todevice_max))(3)),
    ?assertEqual(unlimited, (mod_matrix_gw:mod_opt_type(todevice_max))(unlimited)).

order() ->
    session(<<"r1">>),
    [?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(N)) || N <- [1, 2, 3]],
    [S1, S2, S3] = seqs(<<"DEV1">>),
    ?assert(S1 < S2 andalso S2 < S3),
    %% not resumed: nothing yet
    ?assertEqual(none, next_iq(200)),
    #iq{type = result, sub_els = [R]} = resume(<<"r1">>),
    ?assertEqual(<<"3">>, fxml:get_tag_attr_s(<<"queued">>, R)),
    ?assertEqual(<<"7">>, fxml:get_tag_attr_s(<<"count">>, R)),
    ?assertEqual(<<"DEV1">>, fxml:get_tag_attr_s(<<"device">>, R)),
    I1 = next_iq(2000),
    ?assertEqual(S1, seq(I1)),
    ?assertEqual(j(<<"ann@example.com/r1">>), I1#iq.to),
    #xmlel{name = <<"to-device">>, children = [{xmlcdata, JSON}]} = El = hd(I1#iq.sub_els),
    ?assertEqual(#{<<"n">> => 1}, misc:json_decode(JSON)),
    ?assertEqual(<<"DEV1">>, fxml:get_tag_attr_s(<<"device">>, El)),
    %% one in flight
    ?assertEqual(none, next_iq(300)),
    ack(I1),
    I2 = next_iq(2000),
    ?assertEqual(S2, seq(I2)),
    %% a result for a seq already gone changes nothing
    ack(I1),
    ?assertEqual(none, next_iq(200)),
    ack(I2),
    I3 = next_iq(2000),
    ?assertEqual(S3, seq(I3)),
    ack(I3),
    ?assertEqual(none, next_iq(300)),
    ?assertEqual([], seqs(<<"DEV1">>)),
    %% idle: a new item goes at once
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, {keys_count, <<"signed_curve25519">>, 3, true}),
    I4 = next_iq(2000),
    ?assertMatch(#xmlel{name = <<"keys-count">>}, hd(I4#iq.sub_els)),
    ?assert(seq(I4) > S3),
    ack(I4),
    timer:sleep(100),
    ?assertEqual([], seqs(<<"DEV1">>)),
    ?assertMatch(#{{?ANN, <<"DEV1">>} := {idle, <<"r1">>}}, ?M:state(?HOST)).

error_stops() ->
    session(<<"r1">>),
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(1)),
    resume(<<"r1">>),
    I1 = next_iq(2000),
    ejabberd_router:route(xmpp:make_error(I1, xmpp:err_service_unavailable())),
    timer:sleep(100),
    ?assertNot(maps:is_key({?ANN, <<"DEV1">>}, ?M:state(?HOST))),
    ?assertEqual(1, length(seqs(<<"DEV1">>))),
    %% stopped: a new item waits
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(2)),
    ?assertEqual(none, next_iq(300)),
    resume(<<"r1">>),
    I2 = next_iq(2000),
    ?assertEqual(seq(I1), seq(I2)),
    %% the late result of the stopped one still deletes it
    ack(I2),
    I3 = next_iq(2000),
    ?assertEqual(seq(I1) + 1, seq(I3)),
    ack(I3),
    timer:sleep(100),
    ?assertEqual([], seqs(<<"DEV1">>)).

not_mine() ->
    session(<<"r1">>),
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, {device_deleted, <<"DEV1">>}),
    resume(<<"r1">>),
    I1 = next_iq(2000),
    Err = (xmpp:err_not_acceptable())#stanza_error{sub_els = [el(<<"not-mine">>, [], [])]},
    ejabberd_router:route(xmpp:make_error(I1, Err)),
    timer:sleep(100),
    ?assertEqual([seq(I1)], seqs(<<"DEV1">>)),
    ?assertNot(maps:is_key({?ANN, <<"DEV1">>}, ?M:state(?HOST))),
    %% a stale error for another seq changes nothing
    resume(<<"r1">>),
    I2 = next_iq(2000),
    ejabberd_router:route(xmpp:make_error(I2#iq{id = I1#iq.id}, xmpp:err_bad_request())),
    timer:sleep(100),
    ?assertMatch(#{{?ANN, <<"DEV1">>} := {busy, _, _}}, ?M:state(?HOST)),
    ack(I2).

%% the device refuses the head item at every <resume/>; the
%% third refusal drops it and the next item goes on the next <resume/>.
refused_dropped() ->
    session(<<"r1">>),
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, {device_deleted, <<"OLD">>}),
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(2)),
    [Head, Next] = seqs(<<"DEV1">>),
    NotMine = (xmpp:err_not_acceptable())#stanza_error{sub_els = [el(<<"not-mine">>, [], [])]},
    lists:foreach(
      fun(Err) ->
              resume(<<"r1">>),
              I = next_iq(2000),
              ?assertEqual(Head, seq(I)),
              ejabberd_router:route(xmpp:make_error(I, Err)),
              timer:sleep(100)
      end, [NotMine, xmpp:err_service_unavailable(), NotMine]),
    ?assertEqual([Next], seqs(<<"DEV1">>)),
    resume(<<"r1">>),
    I2 = next_iq(2000),
    ?assertEqual(Next, seq(I2)),
    ack(I2),
    timer:sleep(100),
    ?assertEqual([], seqs(<<"DEV1">>)).

timeout() ->
    session(<<"r1">>),
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(1)),
    resume(<<"r1">>),
    I1 = next_iq(2000),
    %% no answer: after the (test's) timeout the device is stopped
    timer:sleep(1600),
    ?assertNot(maps:is_key({?ANN, <<"DEV1">>}, ?M:state(?HOST))),
    ?assertEqual([seq(I1)], seqs(<<"DEV1">>)),
    resume(<<"r1">>),
    I2 = next_iq(2000),
    ?assertEqual(seq(I1), seq(I2)),
    ack(I2).

presence() ->
    session(<<"r2">>),
    ?M:queue(?HOST, <<"ann">>, <<"DEV2">>, dummy(1)),
    ?assertEqual(none, next_iq(200)),
    %% a resource with no device: nothing
    ?M:set_presence_hook(<<"ann">>, ?HOST, <<"other">>, #presence{}),
    ?assertEqual(none, next_iq(200)),
    ?M:set_presence_hook(<<"ann">>, ?HOST, <<"r2">>, #presence{}),
    I1 = next_iq(2000),
    ?assertEqual(j(<<"ann@example.com/r2">>), I1#iq.to),
    %% presence again while it is in flight: still one
    ?M:set_presence_hook(<<"ann">>, ?HOST, <<"r2">>, #presence{}),
    ?assertEqual(none, next_iq(300)),
    ack(I1),
    timer:sleep(100),
    ?assertMatch(#{{?ANN, <<"DEV2">>} := {idle, <<"r2">>}}, ?M:state(?HOST)),
    ?M:unset_presence_hook(<<"ann">>, ?HOST, <<"r2">>, <<>>),
    timer:sleep(100),
    ?assertNot(maps:is_key({?ANN, <<"DEV2">>}, ?M:state(?HOST))),
    ?M:queue(?HOST, <<"ann">>, <<"DEV2">>, dummy(2)),
    ?assertEqual(none, next_iq(300)),
    %% the hook registered by the module: a real presence starts it
    Ann = j(<<"ann@example.com/r2">>),
    ejabberd_sm:set_presence(ejabberd_sm:get_session_sid(<<"ann">>, ?HOST, <<"r2">>),
                             <<"ann">>, ?HOST, <<"r2">>, 0, #presence{from = Ann, to = Ann}),
    I2 = next_iq(2000),
    ack(I2),
    %% and closing the session stops it
    ejabberd_sm:close_session_unset_presence(ejabberd_sm:get_session_sid(<<"ann">>, ?HOST, <<"r2">>),
                                             <<"ann">>, ?HOST, <<"r2">>, <<>>),
    timer:sleep(100),
    ?assertNot(maps:is_key({?ANN, <<"DEV2">>}, ?M:state(?HOST))).

replaced() ->
    set_devices(#{?ANN => [{<<"DEV1">>, <<"r1">>}]}),
    persistent_term:put({?MODULE, replaced}, #{{<<"ann">>, ?HOST, <<"OLD">>} => <<"r2">>}),
    try
        session(<<"r2">>),
        ?M:queue(?HOST, <<"ann">>, <<"OLD">>, {device_deleted, <<"OLD">>}),
        %% DEV1 has items too, on another resource: not started by r2
        ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(1)),
        ?M:set_presence_hook(<<"ann">>, ?HOST, <<"r2">>, #presence{}),
        I1 = next_iq(2000),
        ?assertEqual(j(<<"ann@example.com/r2">>), I1#iq.to),
        ?assertMatch(#xmlel{name = <<"device-deleted">>}, hd(I1#iq.sub_els)),
        ?assertEqual(<<"OLD">>, fxml:get_tag_attr_s(<<"id">>, hd(I1#iq.sub_els))),
        ack(I1),
        ?assertEqual(none, next_iq(300)),
        ?assertNot(maps:is_key({?ANN, <<"DEV1">>}, ?M:state(?HOST)))
    after
        persistent_term:erase({?MODULE, replaced}),
        mnesia:clear_table(matrix_todevice)
    end.

resume_iq() ->
    session(<<"r1">>),
    #iq{type = result, sub_els = [R]} = resume(<<"r1">>),
    ?assertEqual(<<"0">>, fxml:get_tag_attr_s(<<"queued">>, R)),
    %% a resource without a device
    session(<<"r9">>),
    ?assertMatch(#iq{type = error}, resume(<<"r9">>)),
    ?assertEqual('not-authorized', err(resume(<<"r9">>))),
    %% a user of another server
    Other = #iq{type = set, id = <<"res">>, from = j(<<"eve@elsewhere.example/x">>), to = j(?GW),
                sub_els = [el(<<"resume">>, [], [])]},
    ?M:route_iq(Other),
    ?assertEqual(none, next_iq(100)),
    %% another namespace goes on as any iq (nobody handles it here)
    Foreign = #iq{type = set, id = <<"res">>, from = j(<<"ann@example.com/r1">>), to = j(?GW),
                  sub_els = [#xmlel{name = <<"resume">>, attrs = [{<<"xmlns">>, <<"urn:other">>}]}]},
    mod_matrix_gw:route(Foreign),
    receive {route, #iq{id = <<"res">>, type = error}} -> ok after 2000 -> error(no_error) end,
    %% a get is not ours either
    ?M:route_iq(Foreign#iq{type = get}),
    receive {route, #iq{id = <<"res">>, type = error}} -> ok after 2000 -> error(no_error) end.

err(#iq{} = IQ) ->
    #stanza_error{reason = R} = xmpp:get_error(IQ),
    R.

%% <to-device/> from ann's DEV1 (resource r1), its answer.
send_to_device(El) ->
    IQ = #iq{type = set, id = <<"td">>, from = j(<<"ann@example.com/r1">>), to = j(?GW), sub_els = [El]},
    mod_matrix_gw:route(IQ),
    receive
        {route, #iq{id = <<"td">>} = R} -> R
    after 2000 -> error(no_answer)
    end.

edus() ->
    receive {edu, S, E} -> [{S, E} | edus()] after 200 -> [] end.

to_device_iq() ->
    session(<<"r1">>),
    set_devices(#{?ANN => [{<<"DEV1">>, <<"r1">>}, {<<"DEV2">>, <<"r2">>}, {<<"DEV3">>, <<"r3">>}],
                  {<<"cat">>, ?HOST} => [{<<"C1">>, <<"c">>}]}),
    El = to_device_el(<<"m.room.encrypted">>, <<"txn-1">>,
                      [{<<"@bob:one.example">>, [{<<"B1">>, <<"{\"x\":1}">>}, {<<"B2">>, <<"{\"x\":2}">>}]},
                       {<<"@bea:one.example">>, [{<<"*">>, <<"{\"x\":3}">>}]},
                       {<<"@carl:two.example">>, [{<<"C9">>, <<"{\"x\":4}">>}]},
                       {<<"@ann:example.com">>, [{<<"*">>, <<"{\"own\":1}">>}]},
                       {<<"@cat:example.com">>, [{<<"C1">>, <<"{\"cat\":1}">>}, {<<"NOPE">>, <<"{}">>}]},
                       {<<"@nobody:example.com">>, [{<<"*">>, <<"{}">>}]}]),
    ?assertMatch(#iq{type = result}, send_to_device(El)),
    Sent = lists:sort(edus()),
    Content = fun(Msgs) -> #{<<"edu_type">> => <<"m.direct_to_device">>,
                             <<"content">> => #{<<"sender">> => <<"@ann:example.com">>,
                                                <<"type">> => <<"m.room.encrypted">>,
                                                <<"message_id">> => <<"txn-1">>,
                                                <<"messages">> => Msgs}} end,
    ?assertEqual([{<<"one.example">>, Content(#{<<"@bob:one.example">> => #{<<"B1">> => #{<<"x">> => 1},
                                                                           <<"B2">> => #{<<"x">> => 2}},
                                                <<"@bea:one.example">> => #{<<"*">> => #{<<"x">> => 3}}})},
                  {<<"two.example">>, Content(#{<<"@carl:two.example">> => #{<<"C9">> => #{<<"x">> => 4}}})}],
                 Sent),
    %% our own: every other device of ann's, not the sender's; cat's named one
    ?assertEqual([], seqs(<<"DEV1">>)),
    ?assertEqual(1, length(seqs(<<"DEV2">>))),
    ?assertEqual(1, length(seqs(<<"DEV3">>))),
    ?assertMatch({_, {to_device, <<"m.room.encrypted">>, <<"@ann:example.com">>, <<"txn-1">>, _}},
                 mod_matrix_gw_todevice:next_item(?ANN, <<"DEV2">>)),
    ?assertEqual(1, length(mod_matrix_gw_todevice:items({<<"cat">>, ?HOST}, <<"C1">>))),
    ?assertEqual([], mod_matrix_gw_todevice:items({<<"cat">>, ?HOST}, <<"NOPE">>)),
    %% the same txn again: answered, nothing sent twice
    ?assertMatch(#iq{type = result}, send_to_device(El)),
    ?assertEqual([], edus()),
    ?assertEqual(1, length(seqs(<<"DEV2">>))),
    %% a user with a device named, and a duplicate user entry merged
    El2 = to_device_el(<<"m.dummy">>, <<"txn-2">>,
                       [{<<"@bob:one.example">>, [{<<"B1">>, <<"{}">>}]},
                        {<<"@bob:one.example">>, [{<<"B2">>, <<"{}">>}]},
                        {<<"@ann:example.com">>, [{<<"DEV2">>, <<"{}">>}]}]),
    ?assertMatch(#iq{type = result}, send_to_device(El2)),
    ?assertMatch([{<<"one.example">>, #{<<"content">> := #{<<"messages">> := #{<<"@bob:one.example">> :=
                                                                                   #{<<"B1">> := _, <<"B2">> := _}}}}}],
                 edus()),
    ?assertEqual(2, length(seqs(<<"DEV2">>))),
    mnesia:clear_table(matrix_todevice).

to_device_refused() ->
    session(<<"r1">>),
    Big = binary:copy(<<"x">>, 70000),
    R1 = send_to_device(to_device_el(<<"m.dummy">>, <<"t">>, [{<<"@bob:one.example">>,
                                                               [{<<"B">>, <<"{\"k\":\"", Big/binary, "\"}">>}]}])),
    ?assertEqual('not-acceptable', err(R1)),
    #stanza_error{sub_els = Els} = xmpp:get_error(R1),
    ?assertMatch([#xmlel{name = <<"too-large">>}], [E || #xmlel{} = E <- Els]),
    ?assertEqual('bad-request', err(send_to_device(to_device_el(<<"m.dummy">>, <<>>, [])))),
    ?assertEqual('bad-request', err(send_to_device(to_device_el(<<>>, <<"t">>, [])))),
    ?assertEqual('bad-request', err(send_to_device(to_device_el(<<"m.dummy">>, <<"t">>,
                                                              [{<<"@bob:one.example">>, [{<<"B">>, <<"nope">>}]}])))),
    %% from a resource with no device
    session(<<"r9">>),
    IQ = #iq{type = set, id = <<"td">>, from = j(<<"ann@example.com/r9">>), to = j(?GW),
             sub_els = [to_device_el(<<"m.dummy">>, <<"t">>, [])]},
    mod_matrix_gw:route(IQ),
    receive {route, #iq{id = <<"td">>} = R} -> ?assertEqual('not-authorized', err(R)) after 2000 -> error(no) end,
    ?assertEqual([], edus()).

inbound() ->
    set_devices(#{?ANN => [{<<"DEV1">>, <<"r1">>}, {<<"DEV2">>, <<"r2">>}]}),
    EDU = fun(Sender, ID, Msgs) ->
                  #{<<"edu_type">> => <<"m.direct_to_device">>,
                    <<"content">> => #{<<"sender">> => Sender, <<"type">> => <<"m.room_key_request">>,
                                       <<"message_id">> => ID, <<"messages">> => Msgs}}
          end,
    ok = ?M:inbound_edu(?HOST, <<"remote.example">>,
                        EDU(<<"@bob:remote.example">>, <<"m1">>,
                            #{<<"@ann:example.com">> => #{<<"*">> => #{<<"a">> => 1}},
                              <<"@nobody:example.com">> => #{<<"*">> => #{}},
                              <<"@x:elsewhere.example">> => #{<<"X">> => #{}},
                              <<"@ann:example.com/bad">> => 5})),
    ?assertEqual(1, length(seqs(<<"DEV1">>))),
    ?assertEqual(1, length(seqs(<<"DEV2">>))),
    ?assertMatch({_, {to_device, <<"m.room_key_request">>, <<"@bob:remote.example">>, <<"m1">>, <<"{\"a\":1}">>}},
                 mod_matrix_gw_todevice:next_item(?ANN, <<"DEV1">>)),
    %% the same message id again from that server: once
    ok = ?M:inbound_edu(?HOST, <<"remote.example">>,
                        EDU(<<"@bob:remote.example">>, <<"m1">>, #{<<"@ann:example.com">> => #{<<"*">> => #{}}})),
    ?assertEqual(1, length(seqs(<<"DEV1">>))),
    %% one device named; an unknown one; a content that is no object
    ok = ?M:inbound_edu(?HOST, <<"remote.example">>,
                        EDU(<<"@bob:remote.example">>, <<"m2">>,
                            #{<<"@ann:example.com">> => #{<<"DEV2">> => #{}, <<"DEVX">> => #{},
                                                          <<"DEV1">> => <<"no">>}})),
    ?assertEqual(1, length(seqs(<<"DEV1">>))),
    ?assertEqual(2, length(seqs(<<"DEV2">>))),
    %% a sender of another server than the transaction's: dropped
    ok = ?M:inbound_edu(?HOST, <<"remote.example">>,
                        EDU(<<"@eve:evil.example">>, <<"m3">>, #{<<"@ann:example.com">> => #{<<"*">> => #{}}})),
    ?assertEqual(2, length(seqs(<<"DEV2">>))),
    %% the seam's arity two: not checked; no message id: no dedup
    ok = ?M:inbound_edu(?HOST, EDU(<<"@eve:evil.example">>, 7, #{<<"@ann:example.com">> => #{<<"DEV2">> => #{}}})),
    ok = ?M:inbound_edu(?HOST, EDU(<<"@eve:evil.example">>, 7, #{<<"@ann:example.com">> => #{<<"DEV2">> => #{}}})),
    ?assertEqual(4, length(seqs(<<"DEV2">>))),
    %% malformed
    ok = ?M:inbound_edu(?HOST, <<"remote.example">>, #{<<"content">> => #{}}),
    mnesia:clear_table(matrix_todevice).

queue_all() ->
    ?M:queue(?HOST, <<"ann">>, all, {devices_changed, [{<<"@bob:remote.example">>, 4}]}),
    ?assertEqual(1, length(seqs(<<"DEV1">>))),
    ?assertEqual(1, length(seqs(<<"DEV2">>))),
    ?M:queue(?HOST, <<"nobody">>, all, {devices_changed, []}),
    mnesia:clear_table(matrix_todevice).

bound() ->
    Mem = ?M:mem(?HOST),
    ets:delete(Mem, {warned, <<"ann">>, ?HOST, <<"DEV1">>}),
    with_opts(#{todevice_max => 3},
              fun() ->
                      [?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(N)) || N <- lists:seq(1, 5)],
                      [S1, _, _] = seqs(<<"DEV1">>),
                      ?assertMatch({_, {to_device, _, _, <<"3">>, _}},
                                   mod_matrix_gw_todevice:next_item(?ANN, <<"DEV1">>)),
                      [{_, T1, warn}] = ets:lookup(Mem, {warned, <<"ann">>, ?HOST, <<"DEV1">>}),
                      timer:sleep(5),
                      ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(6)),
                      ?assertEqual(3, length(seqs(<<"DEV1">>))),
                      ?assert(hd(seqs(<<"DEV1">>)) > S1),
                      %% warned once within the hour
                      ?assertEqual([{{warned, <<"ann">>, ?HOST, <<"DEV1">>}, T1, warn}],
                                   ets:lookup(Mem, {warned, <<"ann">>, ?HOST, <<"DEV1">>})),
                      %% within the bound: nothing dropped
                      ?M:queue(?HOST, <<"ann">>, <<"DEV2">>, dummy(1)),
                      ?assertEqual(1, length(seqs(<<"DEV2">>)))
              end),
    mnesia:clear_table(matrix_todevice).

with_opts(Extra, F) ->
    [Entry] = ets:lookup(ejabberd_modules, {mod_matrix_gw, ?HOST}),
    ets:insert(ejabberd_modules, setelement(3, Entry, maps:merge(element(3, Entry), Extra))),
    try F()
    after ets:insert(ejabberd_modules, Entry)
    end.

restart() ->
    session(<<"r1">>),
    [?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(N)) || N <- [1, 2]],
    %% DEV2's resource is not online: not served after the restart
    ?M:queue(?HOST, <<"ann">>, <<"DEV2">>, dummy(1)),
    %% a device the stand-in no longer knows: neither
    ?M:queue(?HOST, <<"ann">>, <<"GONE">>, dummy(1)),
    Before = seqs(<<"DEV1">>),
    Old = whereis(?M:procname(?HOST)),
    exit(Old, kill),
    New = wait_restarted(Old, 50),
    ?assertNotEqual(Old, New),
    ?assertEqual(Before, seqs(<<"DEV1">>)),
    I1 = next_iq(2000),
    ?assertEqual(hd(Before), seq(I1)),
    ?assertEqual(j(<<"ann@example.com/r1">>), I1#iq.to),
    ack(I1),
    I2 = next_iq(2000),
    ack(I2),
    ?assertEqual(none, next_iq(300)),
    ?assertEqual([], seqs(<<"DEV1">>)),
    ?assertEqual(1, length(seqs(<<"DEV2">>))),
    %% the seq counter did not go back
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(3)),
    I3 = next_iq(2000),
    ?assert(seq(I3) > seq(I2)),
    ack(I3),
    mnesia:clear_table(matrix_todevice).

wait_restarted(Old, 0) -> error({not_restarted, Old});
wait_restarted(Old, N) ->
    case whereis(?M:procname(?HOST)) of
        Pid when is_pid(Pid), Pid /= Old -> Pid;
        _ -> timer:sleep(20), wait_restarted(Old, N - 1)
    end.

courier() ->
    Proc = ?M:procname(?HOST),
    Mem = ?M:mem(?HOST),
    %% txns and warnings forgotten when old
    ets:insert(Mem, [{{txn, <<"ann">>, <<"old">>}, 0, txn}, {{txn, <<"ann">>, <<"new">>}, now_ms(), txn},
                     {{warned, <<"ann">>, ?HOST, <<"X">>}, 0, warn}]),
    ?assert(?M:seen(?HOST, {txn, <<"ann">>, <<"new">>}, false)),
    ?assertNot(?M:seen(?HOST, {txn, <<"ann">>, <<"old">>}, false)),
    Proc ! purge,
    ?assertEqual({error, badarg}, gen_server:call(Proc, nonsense)),
    ?assertEqual([], ets:lookup(Mem, {txn, <<"ann">>, <<"old">>})),
    ?assertEqual([], ets:lookup(Mem, {warned, <<"ann">>, ?HOST, <<"X">>})),
    ?assertMatch([_], ets:lookup(Mem, {txn, <<"ann">>, <<"new">>})),
    gen_server:cast(Proc, nonsense),
    %% answers for a seq not in flight
    K = {?ANN, <<"DEV1">>},
    Proc ! {iq_reply, #iq{type = result}, {K, 999}},
    Proc ! {iq_reply, timeout, {K, 999}},
    gen_server:cast(Proc, {start, ?ANN, <<"DEV1">>, <<"r1">>}),
    Proc ! {iq_reply, #iq{type = result}, {K, 999}},
    Proc ! {iq_reply, #iq{type = error}, {K, 999}},
    ?assertMatch(#{K := {idle, <<"r1">>}}, ?M:state(?HOST)),
    Proc ! nonsense,
    ?assertMatch(#{}, ?M:state(?HOST)),
    ?assertEqual({ok, x}, ?M:code_change(old, x, extra)),
    ?assertEqual(ok, ?M:terminate(normal, x)).

now_ms() -> erlang:system_time(millisecond).

a10_again() ->
    session(<<"r1">>),
    [?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(N)) || N <- [1, 2]],
    resume(<<"r1">>),
    I1 = next_iq(2000),
    %% the device comes back (its presence) while I1 is in flight, to a
    %% socket that is gone: no answer ever comes for I1
    ?M:set_presence_hook(<<"ann">>, ?HOST, <<"r1">>, #presence{}),
    ?assertEqual(none, next_iq(300)),
    %% after the timeout I1 is sent again, no <resume/> needed
    I1b = next_iq(3000),
    ?assertEqual(seq(I1), seq(I1b)),
    ack(I1b),
    I2 = next_iq(2000),
    ?assertEqual(seq(I1) + 1, seq(I2)),
    %% an error after a <resume/> while in flight: sent again too
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(3)),
    #iq{type = result} = resume(<<"r1">>),
    ejabberd_router:route(xmpp:make_error(I2, xmpp:err_service_unavailable())),
    I2b = next_iq(2000),
    ?assertEqual(seq(I2), seq(I2b)),
    ack(I2b),
    I3 = next_iq(2000),
    ack(I3),
    %% with no comeback, a timeout still stops the device
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(4)),
    _I4 = next_iq(2000),
    timer:sleep(1600),
    ?assertNot(maps:is_key({?ANN, <<"DEV1">>}, ?M:state(?HOST))),
    mnesia:clear_table(matrix_todevice).

a10_late_stop() ->
    session(<<"r1">>),
    resume(<<"r1">>),
    timer:sleep(100),
    ?assertMatch(#{{?ANN, <<"DEV1">>} := {idle, <<"r1">>}}, ?M:state(?HOST)),
    %% a new session takes the resource; the old one's hook comes late
    Self = self(),
    New = spawn(fun() ->
                        timer:sleep(5),
                        ejabberd_sm:open_session({erlang:timestamp(), self()}, <<"ann">>, ?HOST, <<"r1">>, 0, []),
                        Self ! opened,
                        receive stop -> ok end
                end),
    receive opened -> ok after 2000 -> error(not_opened) end,
    ?M:unset_presence_hook(<<"ann">>, ?HOST, <<"r1">>, <<>>),
    timer:sleep(100),
    ?assertMatch(#{{?ANN, <<"DEV1">>} := {idle, <<"r1">>}}, ?M:state(?HOST)),
    %% the new session's own going stops it
    [ejabberd_sm:close_session(SID, <<"ann">>, ?HOST, <<"r1">>)
     || SID <- ejabberd_sm:get_session_sids(<<"ann">>, ?HOST, <<"r1">>)],
    gen_server:cast(?M:procname(?HOST), {stop, ?ANN, <<"r1">>, New}),
    timer:sleep(100),
    ?assertNot(maps:is_key({?ANN, <<"DEV1">>}, ?M:state(?HOST))),
    New ! stop,
    flush().

a12_coalesce() ->
    [?M:queue(?HOST, <<"ann">>, <<"DEV2">>, {devices_changed, [{<<"@u", (integer_to_binary(N rem 3))/binary,
                                                                   ":remote.example">>, N}]})
     || N <- lists:seq(1, 1000)],
    ?M:queue(?HOST, <<"ann">>, <<"DEV2">>, dummy(1)),
    ?M:queue(?HOST, <<"ann">>, <<"DEV2">>, {devices_changed, [{<<"@u0:remote.example">>, 7}]}),
    %% one devices-changed row, each user once at its highest stream; the
    %% message stays a message of its own
    [S1, S2] = seqs(<<"DEV2">>),
    ?assertMatch({S1, {to_device, _, _, _, _}}, mod_matrix_gw_todevice:next_item(?ANN, <<"DEV2">>)),
    [{matrix_todevice, _, devices_changed, {devices_changed, Users}, _}] =
        mnesia:dirty_read(matrix_todevice, {?ANN, <<"DEV2">>, S2}),
    ?assertEqual([{<<"@u0:remote.example">>, 999}, {<<"@u1:remote.example">>, 1000},
                  {<<"@u2:remote.example">>, 998}], Users),
    %% key counts: the latest count, fallback_used kept once seen
    ?M:queue(?HOST, <<"ann">>, <<"DEV2">>, {keys_count, <<"signed_curve25519">>, 0, true}),
    ?M:queue(?HOST, <<"ann">>, <<"DEV2">>, {keys_count, <<"signed_curve25519">>, 50, false}),
    ?assertEqual(3, length(seqs(<<"DEV2">>))),
    ?assertEqual([{keys_count, <<"signed_curve25519">>, 50, true}],
                 [P || S <- seqs(<<"DEV2">>),
                       {matrix_todevice, _, keys_count, P, _} <- mnesia:dirty_read(matrix_todevice, {?ANN, <<"DEV2">>, S})]),
    mnesia:clear_table(matrix_todevice).

a15_drop() ->
    [?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(N)) || N <- [1, 2, 3]],
    Last = lists:last(seqs(<<"DEV1">>)),
    ok = ?M:drop(?HOST, <<"ann">>, <<"DEV1">>),
    ?assertEqual([], seqs(<<"DEV1">>)),
    ?M:queue(?HOST, <<"ann">>, <<"DEV1">>, dummy(4)),
    ?assertEqual([Last + 1], seqs(<<"DEV1">>)),
    mnesia:clear_table(matrix_todevice).

%% a to-device message from a Matrix user is a row only for a
%% local user who shares a room or a direct chat with the sender, or one of
%% whose devices wrote to the sender first.
s6_share() ->
    session(<<"r1">>),
    set_devices(#{?ANN => [{<<"DEV1">>, <<"r1">>}, {<<"DEV2">>, <<"r2">>}]}),
    persistent_term:put({?MODULE, shares}, #{<<"@zed:far.example">> => []}),
    EDU = fun(ID) ->
                  #{<<"edu_type">> => <<"m.direct_to_device">>,
                    <<"content">> => #{<<"sender">> => <<"@zed:far.example">>, <<"type">> => <<"m.key.verification.request">>,
                                       <<"message_id">> => ID,
                                       <<"messages">> => #{<<"@ann:example.com">> => #{<<"*">> => #{<<"a">> => 1}}}}}
          end,
    try
        ok = ?M:inbound_edu(?HOST, <<"far.example">>, EDU(<<"s6-1">>)),
        ?assertEqual([], seqs(<<"DEV1">>)),
        ?assertEqual([], seqs(<<"DEV2">>)),
        %% ann's DEV1 writes to zed first: zed may answer
        El = to_device_el(<<"m.key.verification.request">>, <<"s6-txn">>,
                          [{<<"@zed:far.example">>, [{<<"Z1">>, <<"{\"x\":1}">>}]}]),
        ?assertMatch(#iq{type = result}, send_to_device(El)),
        _ = edus(),
        ok = ?M:inbound_edu(?HOST, <<"far.example">>, EDU(<<"s6-2">>)),
        ?assertEqual(1, length(seqs(<<"DEV1">>))),
        ?assertEqual(1, length(seqs(<<"DEV2">>)))
    after
        persistent_term:erase({?MODULE, shares}),
        ets:delete(?M:mem(?HOST), {peer, <<"ann">>, <<"@zed:far.example">>}),
        mnesia:clear_table(matrix_todevice)
    end.

%% rows (a `*` a row per device) and bytes a minute per
%% origin and in all; a row over 64 KiB dropped.
s6_budget() ->
    set_devices(#{?ANN => [{<<"DEV1">>, <<"r1">>}, {<<"DEV2">>, <<"r2">>}]}),
    EDU = fun(ID, Content) ->
                  #{<<"edu_type">> => <<"m.direct_to_device">>,
                    <<"content">> => #{<<"sender">> => <<"@bob:rows.example">>, <<"type">> => <<"m.dummy">>,
                                       <<"message_id">> => ID,
                                       <<"messages">> => #{<<"@ann:example.com">> => #{<<"*">> => Content}}}}
          end,
    Limit = fun(Name, V) -> persistent_term:put({mod_matrix_gw_s2s, limit, Name}, V) end,
    Rate = fun(K) -> ets:delete(matrix_gw_rate, K) end,
    [Rate(K) || K <- [{todevice_rows, <<"rows.example">>}, {todevice_bytes, <<"rows.example">>},
                      todevice_rows_all, todevice_bytes_all]],
    try
        %% three rows a minute: the first EDU (two rows) in, the second
        %% (two more) dropped whole
        Limit(todevice_rows, 3),
        ok = ?M:inbound_edu(?HOST, <<"rows.example">>, EDU(<<"b1">>, #{})),
        ok = ?M:inbound_edu(?HOST, <<"rows.example">>, EDU(<<"b2">>, #{})),
        ?assertEqual(1, length(seqs(<<"DEV1">>))),
        ?assertEqual(1, length(seqs(<<"DEV2">>))),
        persistent_term:erase({mod_matrix_gw_s2s, limit, todevice_rows}),
        %% bytes: 2 x 30 bytes within 100, then over
        Limit(todevice_bytes, 100),
        Pad = #{<<"p">> => binary:copy(<<"x">>, 22)},
        ok = ?M:inbound_edu(?HOST, <<"rows.example">>, EDU(<<"b3">>, Pad)),
        ok = ?M:inbound_edu(?HOST, <<"rows.example">>, EDU(<<"b4">>, Pad)),
        ?assertEqual(2, length(seqs(<<"DEV1">>))),
        persistent_term:erase({mod_matrix_gw_s2s, limit, todevice_bytes}),
        %% the gateway's rows, whatever the origin
        Limit(todevice_rows_all, 1),
        Rate(todevice_rows_all),
        ok = ?M:inbound_edu(?HOST, <<"rows.example">>, EDU(<<"b5">>, #{})),
        ?assertEqual(2, length(seqs(<<"DEV1">>))),
        persistent_term:erase({mod_matrix_gw_s2s, limit, todevice_rows_all}),
        Rate(todevice_rows_all),
        %% a row over 64 KiB: dropped, the others kept
        Big = #{<<"p">> => binary:copy(<<"x">>, 70000)},
        ok = ?M:inbound_edu(?HOST, <<"rows.example">>,
                            #{<<"edu_type">> => <<"m.direct_to_device">>,
                              <<"content">> => #{<<"sender">> => <<"@bob:rows.example">>, <<"type">> => <<"m.dummy">>,
                                                 <<"message_id">> => <<"b6">>,
                                                 <<"messages">> => #{<<"@ann:example.com">> =>
                                                                         #{<<"DEV1">> => Big, <<"DEV2">> => #{}}}}}),
        ?assertEqual(2, length(seqs(<<"DEV1">>))),
        ?assertEqual(3, length(seqs(<<"DEV2">>)))
    after
        [persistent_term:erase({mod_matrix_gw_s2s, limit, N})
         || N <- [todevice_rows, todevice_bytes, todevice_rows_all]],
        mnesia:clear_table(matrix_todevice)
    end.
