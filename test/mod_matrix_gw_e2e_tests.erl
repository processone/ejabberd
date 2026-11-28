%%% The Matrix gateway's end-to-end keys (mod_matrix_gw_e2e): the
%%% signature checks as functions of their input; then, on an ejabberd
%%% started with the gateway, the device's iqs as a device sends them
%%% (<device/>, <keys-upload/>, <signing-keys/>, <keys-query/>,
%%% <keys-claim/>), the federation's three endpoints, the remote keys
%%% (fetched, cached, stale, failed, bounded) with the federation stood in
%%% for by a function, the EDUs both ways and the queue items, caught by a
%%% function of the suite (set_sink/1).
-module(mod_matrix_gw_e2e_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).
-define(NS, <<"https://ns.another.im/matrix-e2e/0">>).
-define(KIM, <<"@kim:example.com">>).
-define(BOB, <<"@bob:remote.test">>).
-define(SINK, mod_matrix_gw_e2e_tests_sink).
-define(FED, mod_matrix_gw_e2e_tests_fed).

-define(E2E, mod_matrix_gw_e2e).

%%--------------------------------------------------------------------
%% Keys as a device makes them
%%--------------------------------------------------------------------

keypair() ->
    crypto:generate_key(eddsa, ed25519).

b64(B) -> mod_matrix_gw:base64_encode(B).

sign(Obj, User, KeyID, Priv) ->
    Msg = mod_matrix_gw:encode_canonical_json(maps:without([<<"signatures">>, <<"unsigned">>], Obj)),
    Sig = b64(crypto:sign(eddsa, none, Msg, [Priv, ed25519])),
    Sigs = maps:get(<<"signatures">>, Obj, #{}),
    Obj#{<<"signatures">> => Sigs#{User => (maps:get(User, Sigs, #{}))#{KeyID => Sig}}}.

device_keys(User, Dev, {Pub, Priv}) ->
    sign(#{<<"user_id">> => User, <<"device_id">> => Dev,
           <<"algorithms">> => [<<"m.olm.v1.curve25519-aes-sha2">>, <<"m.megolm.v1.aes-sha2">>],
           <<"keys">> => #{<<"curve25519:", Dev/binary>> => b64(crypto:strong_rand_bytes(32)),
                           <<"ed25519:", Dev/binary>> => b64(Pub)}},
         User, <<"ed25519:", Dev/binary>>, Priv).

key_object(User, Dev, {_, Priv}, Fallback) ->
    O = #{<<"key">> => b64(crypto:strong_rand_bytes(32))},
    sign(case Fallback of true -> O#{<<"fallback">> => true}; false -> O end,
         User, <<"ed25519:", Dev/binary>>, Priv).

otks(User, Dev, Ed, Prefix, N) ->
    maps:from_list([{iolist_to_binary(["signed_curve25519:", Prefix, io_lib:format("~3..0B", [I])]),
                     key_object(User, Dev, Ed, false)} || I <- lists:seq(1, N)]).

fallback(User, Dev, Ed, ID) ->
    #{<<"signed_curve25519:", ID/binary>> => key_object(User, Dev, Ed, true)}.

csk(User, Usage, Pub) ->
    #{<<"user_id">> => User, <<"usage">> => [Usage], <<"keys">> => #{<<"ed25519:", (b64(Pub))/binary>> => b64(Pub)}}.

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------

signatures_test_() ->
    Ed = {Pub, _} = keypair(),
    DK = device_keys(?KIM, <<"DEVA">>, Ed),
    Keys = maps:get(<<"keys">>, DK),
    Tampered = DK#{<<"keys">> => Keys#{<<"curve25519:DEVA">> => b64(crypto:strong_rand_bytes(32))}},
    C = fun ?E2E:check_device_keys/3,
    [?_assertEqual({ok, b64(Pub)}, C(DK, ?KIM, <<"DEVA">>)),
     %% unsigned is outside the signature
     ?_assertEqual({ok, b64(Pub)}, C(DK#{<<"unsigned">> => #{<<"x">> => 1}}, ?KIM, <<"DEVA">>)),
     ?_assertEqual(error, C(Tampered, ?KIM, <<"DEVA">>)),
     ?_assertEqual(error, C(DK, ?BOB, <<"DEVA">>)),
     ?_assertEqual(error, C(DK, ?KIM, <<"DEVB">>)),
     ?_assertEqual(error, C(maps:remove(<<"signatures">>, DK), ?KIM, <<"DEVA">>)),
     ?_assertEqual(error, C(DK#{<<"keys">> => Keys#{<<"ed25519:DEVA">> => <<"short">>}}, ?KIM, <<"DEVA">>)),
     ?_assertEqual(error, C(<<"no object">>, ?KIM, <<"DEVA">>)),
     %% a signature that is no base64 at all
     ?_assertNot(?E2E:verify_json(DK#{<<"signatures">> => #{?KIM => #{<<"ed25519:DEVA">> => <<"a">>}}},
                                  ?KIM, <<"ed25519:DEVA">>, b64(Pub))),
     ?_assertNot(?E2E:verify_json(#{}, ?KIM, <<"ed25519:DEVA">>, b64(Pub)))].

csk_test_() ->
    {Pub, _} = keypair(),
    C = fun ?E2E:check_csk/3,
    M = csk(?KIM, <<"master">>, Pub),
    [?_assertEqual({ok, b64(Pub)}, C(M, ?KIM, <<"master">>)),
     ?_assertEqual(error, C(M, ?KIM, <<"self_signing">>)),
     ?_assertEqual(error, C(M, ?BOB, <<"master">>)),
     ?_assertEqual(error, C(M#{<<"keys">> => #{<<"ed25519:x">> => b64(Pub)}}, ?KIM, <<"master">>)),
     ?_assertEqual(error, C(M#{<<"keys">> => #{<<"ed25519:abc">> => <<"abc">>}}, ?KIM, <<"master">>)),
     ?_assertEqual(error, C(M#{<<"keys">> => (maps:get(<<"keys">>, M))#{<<"ed25519:y">> => <<"y">>}},
                            ?KIM, <<"master">>)),
     ?_assertEqual(error, C(M#{<<"usage">> => <<"master">>}, ?KIM, <<"master">>)),
     ?_assertEqual(error, C(#{}, ?KIM, <<"master">>))].

disco_test_() ->
    J = fun jid:decode/1,
    [?_assertEqual([?NS], ?E2E:disco_features(J(?GW))),
     ?_assertEqual([?NS], ?E2E:disco_features(J(<<"bob%remote.test@", ?GW/binary>>))),
     ?_assertEqual([], ?E2E:disco_features(J(<<"!r%remote.test@", ?GW/binary>>))),
     ?_assertEqual([], ?E2E:disco_features(J(<<"#a%remote.test@", ?GW/binary>>)))].

%%--------------------------------------------------------------------
%% On a running gateway
%%--------------------------------------------------------------------

ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup,
     fun() ->
             Holder = holder(),
             Dir = mod_matrix_gw_test_lib:start_ejabberd(?MODULE),
             [ok = reg(U) || U <- [<<"kim">>, <<"lee">>, <<"mia">>, <<"ned">>, <<"oli">>]],
             ?E2E:set_sink(fun(X) -> ets:insert(?SINK, {erlang:unique_integer([monotonic]), X}) end),
             {Dir, Holder}
     end,
     fun({Dir, Holder}) ->
             persistent_term:erase({?E2E, sink}),
             persistent_term:erase({?E2E, federation}),
             persistent_term:erase({?E2E, remote_max}),
             exit(Holder, kill),
             mod_matrix_gw_test_lib:stop_ejabberd(Dir)
     end,
     fun(_) ->
             [{timeout, 60, {Name, F}}
              || {Name, F} <- [{"<device/>: bind, conflict, rebind, refusals", fun device/0},
                               {"<keys-upload/>: count, bound, signatures, the stream", fun upload/0},
                               {"<signing-keys/>", fun signing/0},
                               {"<keys-query/> of our own users", fun query_local/0},
                               {"<keys-claim/> of our own devices; fallback; missing", fun claim_local/0},
                               {"a claim is atomic: two claims, one key", fun claim_atomic/0},
                               {"replaces and <not-mine/>: the old device told and deleted", fun replaces/0},
                               {"a device replaced on its own resource: queue and row gone", fun a15_same_resource/0},
                               {"claims from one server within its minute's budget", fun a6_claim_rate/0},
                               {"the federation's endpoints", fun federation/0},
                               {"remote keys: fetched, cached, stale, failed, bounded", fun remote/0},
                               {"remote claims, a request per server", fun remote_claim/0},
                               {"EDUs in: the origin, the stale copy, the fan-out", fun edus_in/0},
                               {"membership: peer servers and who shares a room", fun membership/0},
                               {"a remote user newly shared: the copy stale, the room's devices told", fun newly_shared/0},
                               {"the last shared room gone: the copy stale, nobody told", fun unshared/0},
                               {"the gateway's disco#info and the iqs through mod_matrix_gw", fun through/0},
                               {"past the cap a new device retires the oldest unbound one", fun s15_device_cap/0}]]
     end}.

reg(U) ->
    case ejabberd_auth:try_register(U, ?HOST, <<"secret">>) of
        ok -> ok;
        {error, exists} -> ok
    end.

%% Owns the suite's tables for the whole run.
holder() ->
    Parent = self(),
    Pid = spawn(fun() ->
                        ets:new(?SINK, [named_table, public, ordered_set]),
                        ets:new(?FED, [named_table, public, bag]),
                        Parent ! ready,
                        receive stop -> ok end
                end),
    receive ready -> Pid after 5000 -> error(no_holder) end.

%% What went out since the last call, in order.
sunk() ->
    L = [X || {_, X} <- ets:tab2list(?SINK)],
    ets:delete_all_objects(?SINK),
    L.

j(S) -> jid:decode(S).

el(Name, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, ?NS} | Attrs], children = Children}.

json_el(Name, JSON) ->
    #xmlel{name = Name, children = [{xmlcdata, misc:json_encode(JSON)}]}.

session(Full) ->
    case get({session, Full}) of
        undefined -> put({session, Full}, true), open(Full);
        _ -> ok
    end.

open(Full) ->
    #jid{luser = U, lserver = S, lresource = R} = j(Full),
    SID = {erlang:timestamp(), self()},
    persistent_term:put({?MODULE, sid, Full}, SID),
    ejabberd_sm:open_session(SID, U, S, R, 0, []).

iq(Full, Type, El) ->
    session(Full),
    Id = p1_rand:get_string(),
    ejabberd_router:route(#iq{type = Type, id = Id, from = j(Full), to = j(?GW), sub_els = [El]}),
    receive
        {route, #iq{id = Id} = IQ} -> IQ
    after 15000 -> timeout
    end.

%% A result's element; an error's {reason, app child name}.
answer(#iq{type = result, sub_els = [El]}) -> El;
answer(#iq{type = result, sub_els = []}) -> empty;
answer(#iq{type = error} = IQ) ->
    #stanza_error{reason = R, sub_els = Els} = xmpp:get_error(IQ),
    {R, [N || #xmlel{name = N} <- Els]}.

attrs(#xmlel{attrs = A}) -> maps:from_list([{K, V} || {K, V} <- A, K /= <<"xmlns">>]).

bind(Full, ID) -> bind(Full, ID, []).
bind(Full, ID, More) ->
    answer(iq(Full, set, el(<<"device">>, [{<<"id">>, ID}, {<<"name">>, jid_res(Full)} | More], []))).

jid_res(Full) -> (j(Full))#jid.lresource.

upload(Full, Parts) ->
    answer(iq(Full, set, el(<<"keys-upload">>, [], [json_el(N, J) || {N, J} <- Parts]))).

count(#xmlel{name = <<"keys-upload">>, children = [#xmlel{name = <<"count">>} = C]}) ->
    binary_to_integer(fxml:get_tag_cdata(C));
count(Other) -> Other.

device() ->
    Phone = <<"kim@example.com/phone">>,
    ?assertEqual(#{<<"id">> => <<"KIMPHONE">>, <<"user">> => ?KIM, <<"stream_id">> => <<"0">>},
                 attrs(bind(Phone, <<"KIMPHONE">>))),
    ?assertEqual([{<<"KIMPHONE">>, <<"phone">>}], ?E2E:devices_of(?HOST, <<"kim">>)),
    ?assertEqual({ok, <<"phone">>}, ?E2E:resource_of(?HOST, <<"kim">>, <<"KIMPHONE">>)),
    ?assertEqual(error, ?E2E:resource_of(?HOST, <<"kim">>, <<"NOSUCH">>)),
    %% again from the same resource: the same binding
    ?assertMatch(#xmlel{name = <<"device">>}, bind(Phone, <<"KIMPHONE">>)),
    %% another live resource asking for it: conflict, and where it is
    ?assertEqual({'conflict', [<<"bound-elsewhere">>]}, bind(<<"kim@example.com/laptop">>, <<"KIMPHONE">>)),
    ?assertEqual(#{<<"resource">> => <<"phone">>},
                 begin
                     #iq{} = I = iq(<<"kim@example.com/laptop">>, set,
                                    el(<<"device">>, [{<<"id">>, <<"KIMPHONE">>}], [])),
                     #stanza_error{sub_els = [B]} = xmpp:get_error(I),
                     attrs(B)
                 end),
    %% the laptop gets its own
    ?assertMatch(#xmlel{}, bind(<<"kim@example.com/laptop">>, <<"KIMLAPTOP">>)),
    %% a resource that is gone gives its id up to another
    ?assertMatch(#xmlel{}, bind(<<"kim@example.com/tablet">>, <<"KIMTAB">>)),
    close(<<"kim@example.com/tablet">>),
    ?assertMatch(#xmlel{}, bind(<<"kim@example.com/tablet2">>, <<"KIMTAB">>)),
    ?assertEqual({ok, <<"tablet2">>}, ?E2E:resource_of(?HOST, <<"kim">>, <<"KIMTAB">>)),
    %% a resource binding another id leaves its old device unbound
    ?assertMatch(#xmlel{}, bind(<<"kim@example.com/tablet2">>, <<"KIMTAB2">>)),
    ?assertEqual(error, ?E2E:resource_of(?HOST, <<"kim">>, <<"KIMTAB">>)),
    ?assert(lists:member({<<"KIMTAB">>, <<>>}, ?E2E:devices_of(?HOST, <<"kim">>))),
    %% refusals
    ?assertEqual({'bad-request', []}, bind(Phone, <<"bad id">>)),
    ?assertEqual({'bad-request', []}, bind(Phone, <<>>)),
    ?assertEqual({'bad-request', []}, bind(Phone, <<"KIMPHONE">>, [{<<"replaces">>, <<"a b">>}])),
    ?assertEqual({'not-allowed', []}, bind(<<"nobody@example.com/x">>, <<"NOBODY">>)),
    %% a get is no request of ours
    ?assertEqual({'service-unavailable', []},
                 answer(iq(Phone, get, el(<<"device">>, [{<<"id">>, <<"KIMPHONE">>}], [])))),
    %% the server itself is no user
    ?assertEqual(ok, ?E2E:route_iq(#iq{type = set, id = <<"s">>, from = j(?HOST), to = j(?GW),
                                       sub_els = [el(<<"device">>, [{<<"id">>, <<"X">>}], [])]})),
    ?assertEqual(ok, ?E2E:route_iq(#iq{type = get, id = <<"s">>, from = j(?HOST), to = j(?GW),
                                       sub_els = [el(<<"keys-query">>, [], [])]})),
    %% another namespace is not ours
    ?assertMatch(ok, ?E2E:route_iq(#iq{type = set, id = <<"s">>, from = j(Phone), to = j(?GW),
                                       sub_els = [#xmlel{name = <<"device">>, attrs = [{<<"xmlns">>, <<"x">>}]}]})),
    ?assertMatch(ok, ?E2E:route_iq(#iq{type = set, id = <<"s">>, from = j(Phone), to = j(?GW), sub_els = []})),
    sunk().

%% The session session/1 opened for Full closed: the resource is gone.
close(Full) ->
    #jid{luser = U, lserver = S, lresource = R} = j(Full),
    erase({session, Full}),
    ejabberd_sm:close_session(persistent_term:get({?MODULE, sid, Full}), U, S, R).

upload() ->
    Phone = <<"kim@example.com/phone">>,
    Ed = keypair(),
    persistent_term:put({?MODULE, kim_ed}, Ed),
    DK = device_keys(?KIM, <<"KIMPHONE">>, Ed),
    %% a peer server, so that the changes go somewhere
    ?E2E:membership(?HOST, <<"!r1:remote.test">>, ?KIM, <<"join">>),
    ?E2E:membership(?HOST, <<"!r1:remote.test">>, ?BOB, join),
    sunk(),
    ?assertEqual(3, count(upload(Phone, [{<<"device-keys">>, DK},
                                          {<<"one-time-keys">>, otks(?KIM, <<"KIMPHONE">>, Ed, "a", 3)},
                                          {<<"fallback-keys">>, fallback(?KIM, <<"KIMPHONE">>, Ed, <<"fb1">>)}]))),
    Out1 = sunk(),
    [#{<<"stream_id">> := S1, <<"prev_id">> := P1, <<"keys">> := K1}] =
        [C || {edu, <<"remote.test">>, #{<<"edu_type">> := <<"m.device_list_update">>, <<"content">> := C}} <- Out1],
    ?assertEqual({1, [], DK}, {S1, P1, K1}),
    %% kim's other devices hear of it, the phone does not
    Told = lists:sort([D || {queue, <<"kim">>, D, {devices_changed, [{?KIM, 1}]}} <- Out1]),
    ?assertEqual([<<"KIMLAPTOP">>, <<"KIMTAB">>, <<"KIMTAB2">>], Told),
    %% the same keys again: no change, no stream
    ?assertEqual(3, count(upload(Phone, [{<<"device-keys">>, DK#{<<"unsigned">> => #{<<"a">> => 1}}}]))),
    ?assertEqual([], sunk()),
    %% more keys; ids already held are not taken twice
    ?assertEqual(5, count(upload(Phone, [{<<"one-time-keys">>, otks(?KIM, <<"KIMPHONE">>, Ed, "a", 5)}]))),
    %% at most 100
    ?assertEqual(100, count(upload(Phone, [{<<"one-time-keys">>, otks(?KIM, <<"KIMPHONE">>, Ed, "b", 120)}]))),
    ?assertEqual(100, ?E2E:otk_count(?HOST, <<"kim">>, <<"KIMPHONE">>)),
    %% a tampered DeviceKeys, another device's or another user's: refused
    Keys = maps:get(<<"keys">>, DK),
    Bad = {'not-acceptable', [<<"bad-signature">>]},
    ?assertEqual(Bad, upload(Phone, [{<<"device-keys">>, DK#{<<"keys">> => Keys#{<<"curve25519:KIMPHONE">> => b64(<<0:256>>)}}}])),
    ?assertEqual(Bad, upload(Phone, [{<<"device-keys">>, device_keys(?KIM, <<"KIMLAPTOP">>, Ed)}])),
    ?assertEqual(Bad, upload(Phone, [{<<"device-keys">>, device_keys(<<"@lee:example.com">>, <<"KIMPHONE">>, Ed)}])),
    %% a one-time key not signed by the device
    ?assertEqual(Bad, upload(Phone, [{<<"one-time-keys">>, otks(?KIM, <<"KIMPHONE">>, keypair(), "c", 1)}])),
    ?assertEqual(Bad, upload(Phone, [{<<"one-time-keys">>, #{<<"curve25519:x">> => key_object(?KIM, <<"KIMPHONE">>, Ed, false)}}])),
    ?assertEqual(Bad, upload(Phone, [{<<"one-time-keys">>, #{<<"signed_curve25519:x">> => #{<<"key">> => <<"short">>}}}])),
    %% keys of a device that has none yet
    ?assertEqual(Bad, upload(<<"kim@example.com/laptop">>,
                             [{<<"one-time-keys">>, otks(?KIM, <<"KIMLAPTOP">>, Ed, "a", 1)}])),
    ?assertEqual(100, ?E2E:otk_count(?HOST, <<"kim">>, <<"KIMPHONE">>)),
    %% not JSON objects
    ?assertEqual({'bad-request', []},
                 answer(iq(Phone, set, el(<<"keys-upload">>, [], [#xmlel{name = <<"device-keys">>,
                                                                       children = [{xmlcdata, <<"[1]">>}]}])))),
    ?assertEqual({'bad-request', []},
                 answer(iq(Phone, set, el(<<"keys-upload">>, [], [#xmlel{name = <<"device-keys">>,
                                                                       children = [{xmlcdata, <<"{">>}]}])))),
    %% no device bound to the resource
    ?assertEqual({'not-authorized', []}, upload(<<"kim@example.com/other">>, [])),
    %% new device keys: the stream goes on, prev_id the last one
    DK2 = device_keys(?KIM, <<"KIMPHONE">>, Ed),
    ?assertEqual(100, count(upload(Phone, [{<<"device-keys">>, DK2}]))),
    [#{<<"stream_id">> := 2, <<"prev_id">> := [1]}] =
        [C || {edu, _, #{<<"edu_type">> := <<"m.device_list_update">>, <<"content">> := C}} <- sunk()],
    %% the fallback key replaced
    ?assertEqual(100, count(upload(Phone, [{<<"fallback-keys">>, fallback(?KIM, <<"KIMPHONE">>, Ed, <<"fb2">>)}]))),
    ?assertEqual(100, count(upload(Phone, [{<<"fallback-keys">>, fallback(?KIM, <<"KIMPHONE">>, Ed, <<"fb2">>)}]))),
    {ok, Up} = ?E2E:upload(?HOST, <<"kim">>, <<"KIMPHONE">>, #{}, <<"phone">>),
    ?assertEqual(100, Up),
    sunk().

signing() ->
    Phone = <<"kim@example.com/phone">>,
    {MPub, MPriv} = keypair(),
    {SPub, _} = keypair(),
    Master = csk(?KIM, <<"master">>, MPub),
    Self = sign(csk(?KIM, <<"self_signing">>, SPub), ?KIM, <<"ed25519:", (b64(MPub))/binary>>, MPriv),
    persistent_term:put({?MODULE, kim_xsign}, {Master, Self}),
    SK = fun(M, S) -> answer(iq(Phone, set, el(<<"signing-keys">>, [], [json_el(<<"master">>, M),
                                                                       json_el(<<"self-signing">>, S)]))) end,
    sunk(),
    ?assertEqual(empty, SK(Master, Self)),
    Out = sunk(),
    ?assertEqual([#{<<"user_id">> => ?KIM, <<"master_key">> => Master, <<"self_signing_key">> => Self}],
                 [C || {edu, <<"remote.test">>, #{<<"edu_type">> := <<"m.signing_key_update">>,
                                                  <<"content">> := C}} <- Out]),
    ?assert(lists:member({queue, <<"kim">>, <<"KIMPHONE">>, {devices_changed, [{?KIM, 3}]}}, Out)),
    %% the same again: nothing goes out
    ?assertEqual(empty, SK(Master, Self)),
    ?assertEqual([], sunk()),
    Bad = {'not-acceptable', [<<"bad-signature">>]},
    %% a self-signing key the master did not sign, or of the wrong use
    ?assertEqual(Bad, SK(Master, csk(?KIM, <<"self_signing">>, SPub))),
    ?assertEqual(Bad, SK(Master, sign(csk(?KIM, <<"user_signing">>, SPub), ?KIM,
                                      <<"ed25519:", (b64(MPub))/binary>>, MPriv))),
    ?assertEqual(Bad, SK(csk(?KIM, <<"self_signing">>, MPub), Self)),
    ?assertEqual(Bad, SK(csk(?BOB, <<"master">>, MPub), Self)),
    ?assertEqual(Bad, answer(iq(Phone, set, el(<<"signing-keys">>, [], [json_el(<<"master">>, Master)])))),
    ?assertEqual({'not-authorized', []},
                 answer(iq(<<"kim@example.com/other">>, set, el(<<"signing-keys">>, [], [])))),
    ?assertEqual([], sunk()).

query(Full, Users) ->
    answer(iq(Full, get, el(<<"keys-query">>, [], [el(<<"user">>, [{<<"id">>, U}], []) || U <- Users]))).

users(#xmlel{name = <<"keys-query">>, children = Cs}) ->
    maps:from_list([{maps:get(<<"id">>, attrs(C)),
                     {attrs(C), misc:json_decode(fxml:get_tag_cdata(C))}} || C <- Cs]);
users(Other) -> Other.

query_local() ->
    Laptop = <<"kim@example.com/laptop">>,
    #{?KIM := {#{<<"stream_id">> := <<"3">>, <<"fresh">> := <<"1">>}, Obj},
      <<"@lee:example.com">> := {#{<<"fresh">> := <<"1">>}, #{<<"device_keys">> := #{}}},
      <<"@nobody:example.com">> := {_, #{<<"device_keys">> := Empty}}} =
        users(query(Laptop, [?KIM, <<"@lee:example.com">>, <<"@nobody:example.com">>, ?KIM])),
    ?assertEqual(#{}, Empty),
    {Master, Self} = persistent_term:get({?MODULE, kim_xsign}),
    ?assertMatch(#{<<"master_key">> := Master, <<"self_signing_key">> := Self}, Obj),
    #{<<"device_keys">> := #{<<"KIMPHONE">> := DK} = DKs} = Obj,
    %% only devices with keys; the name for our own devices
    ?assertEqual([<<"KIMPHONE">>], maps:keys(DKs)),
    ?assertEqual(#{<<"device_display_name">> => <<"phone">>}, maps:get(<<"unsigned">>, DK)),
    ?assertMatch({ok, _}, ?E2E:check_device_keys(DK, ?KIM, <<"KIMPHONE">>)),
    %% A9: another user asking gets no device names
    #{?KIM := {_, #{<<"device_keys">> := #{<<"KIMPHONE">> := DKOther}}}} =
        users(query(<<"mia@example.com/q">>, [?KIM])),
    ?assertNot(maps:is_key(<<"unsigned">>, DKOther)),
    ?assertEqual(maps:without([<<"unsigned">>], DK), DKOther),
    %% more than 50 users
    ?assertEqual({'resource-constraint', []},
                 query(Laptop, [<<"@u", (integer_to_binary(I))/binary, ":example.com">> || I <- lists:seq(1, 51)])),
    ?assertEqual({'not-allowed', []}, query(<<"nobody@example.com/x">>, [?KIM])).

claim(Full, Keys) ->
    answer(iq(Full, get, el(<<"keys-claim">>, [],
                            [el(<<"key">>, [{<<"user">>, U}, {<<"device">>, D}, {<<"algorithm">>, A}], [])
                             || {U, D, A} <- Keys]))).

claimed(#xmlel{name = <<"keys-claim">>, children = Cs}) ->
    [{N, attrs(C), case N of
                       <<"key">> -> misc:json_decode(fxml:get_tag_cdata(C));
                       _ -> none
                   end} || #xmlel{name = N} = C <- Cs].

claim_local() ->
    Laptop = <<"kim@example.com/laptop">>,
    sunk(),
    S = <<"signed_curve25519">>,
    [{<<"missing">>, #{<<"device">> := <<"KIMLAPTOP">>}, none},
     {<<"key">>, #{<<"id">> := <<"signed_curve25519:a001">>, <<"fallback">> := <<"0">>,
                   <<"user">> := ?KIM, <<"device">> := <<"KIMPHONE">>}, KO},
     {<<"missing">>, #{<<"device">> := <<"KIMTAB">>}, none},
     {<<"missing">>, #{<<"device">> := <<"KIMPHONE">>, <<"user">> := <<"@nobody:example.com">>}, none}] =
        claimed(claim(Laptop, [{?KIM, <<"KIMPHONE">>, S}, {?KIM, <<"KIMLAPTOP">>, S},
                               {<<"@nobody:example.com">>, <<"KIMPHONE">>, S},
                               {?KIM, <<"KIMTAB">>, <<"other_algorithm">>}])),
    %% the KeyObject as the device signed it
    {EdPub, _} = persistent_term:get({?MODULE, kim_ed}),
    ?assert(?E2E:verify_json(KO, ?KIM, <<"ed25519:KIMPHONE">>, b64(EdPub))),
    ?assertEqual([{queue, <<"kim">>, <<"KIMPHONE">>, {keys_count, S, 99, false}}], sunk()),
    ?assertEqual(99, ?E2E:otk_count(?HOST, <<"kim">>, <<"KIMPHONE">>)),
    %% drained: then the fallback key, kept for the next
    [?E2E:claim_local(?HOST, <<"kim">>, <<"KIMPHONE">>) || _ <- lists:seq(1, 99)],
    sunk(),
    [{<<"key">>, #{<<"id">> := <<"signed_curve25519:fb2">>, <<"fallback">> := <<"1">>}, #{<<"fallback">> := true}}] =
        claimed(claim(Laptop, [{?KIM, <<"KIMPHONE">>, S}])),
    ?assertEqual([{queue, <<"kim">>, <<"KIMPHONE">>, {keys_count, S, 0, true}}], sunk()),
    ?assertMatch({ok, <<"signed_curve25519:fb2">>, _}, ?E2E:claim_local(?HOST, <<"kim">>, <<"KIMPHONE">>)),
    ?assertEqual(none, ?E2E:claim_local(?HOST, <<"kim">>, <<"KIMLAPTOP">>)),
    ?assertEqual({'not-allowed', []}, claim(<<"nobody@example.com/x">>, [])),
    %% A16: at most 100 keys an iq, and our devices' keys within the
    %% user's budget a minute (remote ones are their servers' to count)
    ?assertEqual({'resource-constraint', []},
                 claim(Laptop, [{?KIM, <<"D", (integer_to_binary(I))/binary>>, S} || I <- lists:seq(1, 101)])),
    ets:delete(matrix_gw_rate, {claims_local, <<"kim">>, ?HOST}),
    persistent_term:put({mod_matrix_gw_s2s, limit, local_claims}, 2),
    try
        ?assertEqual({'resource-constraint', []},
                     claim(Laptop, [{?KIM, D, S} || D <- [<<"X1">>, <<"X2">>, <<"X3">>]]))
    after
        persistent_term:erase({mod_matrix_gw_s2s, limit, local_claims})
    end,
    sunk().

claim_atomic() ->
    Full = <<"ned@example.com/one">>,
    Ed = keypair(),
    NED = <<"@ned:example.com">>,
    ?assertMatch(#xmlel{}, bind(Full, <<"NEDONE">>)),
    Race = fun() ->
                   Self = self(),
                   [spawn(fun() -> Self ! {claimed, ?E2E:claim_local(?HOST, <<"ned">>, <<"NEDONE">>)} end)
                    || _ <- lists:seq(1, 10)],
                   [receive {claimed, R} -> R after 5000 -> timeout end || _ <- lists:seq(1, 10)]
           end,
    %% one key and no fallback: one claim has it, nine have nothing
    ?assertEqual(1, count(upload(Full, [{<<"device-keys">>, device_keys(NED, <<"NEDONE">>, Ed)},
                                        {<<"one-time-keys">>, otks(NED, <<"NEDONE">>, Ed, "x", 1)}]))),
    R1 = Race(),
    ?assertEqual(1, length([K || {ok, K, _} <- R1])),
    ?assertEqual(9, length([none || none <- R1])),
    %% five keys and a fallback: five distinct keys, the others the fallback
    ?assertEqual(5, count(upload(Full, [{<<"one-time-keys">>, otks(NED, <<"NEDONE">>, Ed, "y", 5)},
                                        {<<"fallback-keys">>, fallback(NED, <<"NEDONE">>, Ed, <<"f">>)}]))),
    R2 = [K || {ok, K, _} <- Race()],
    ?assertEqual(10, length(R2)),
    ?assertEqual(5, length(lists:usort([K || K <- R2, K /= <<"signed_curve25519:f">>]))),
    ?assertEqual(5, length([K || K <- R2, K == <<"signed_curve25519:f">>])),
    %% a one-time key and a fallback key under the same id (vodozemac's
    %% first of each): two keys, the one-time one first
    Two = <<"ned@example.com/two">>,
    ?assertMatch(#xmlel{}, bind(Two, <<"NEDTWO">>)),
    ?assertEqual(1, count(upload(Two, [{<<"device-keys">>, device_keys(NED, <<"NEDTWO">>, Ed)},
                                       {<<"one-time-keys">>, #{<<"signed_curve25519:AAAA">> =>
                                                                   key_object(NED, <<"NEDTWO">>, Ed, false)}},
                                       {<<"fallback-keys">>, fallback(NED, <<"NEDTWO">>, Ed, <<"AAAA">>)}]))),
    ?assertMatch({ok, <<"signed_curve25519:AAAA">>, #{<<"key">> := _} = O} when not is_map_key(<<"fallback">>, O),
                 ?E2E:claim_local(?HOST, <<"ned">>, <<"NEDTWO">>)),
    ?assertMatch({ok, <<"signed_curve25519:AAAA">>, #{<<"fallback">> := true}},
                 ?E2E:claim_local(?HOST, <<"ned">>, <<"NEDTWO">>)),
    sunk().

replaces() ->
    Old = <<"lee@example.com/old">>,
    New = <<"lee@example.com/new">>,
    LEE = <<"@lee:example.com">>,
    Ed = keypair(),
    ?E2E:membership(?HOST, <<"!r2:remote.test">>, LEE, invite),
    ?E2E:membership(?HOST, <<"!r2:remote.test">>, <<"@carl:far.test">>, join),
    ?assertMatch(#xmlel{}, bind(Old, <<"LEEOLD">>)),
    ?assertEqual(2, count(upload(Old, [{<<"device-keys">>, device_keys(LEE, <<"LEEOLD">>, Ed)},
                                       {<<"one-time-keys">>, otks(LEE, <<"LEEOLD">>, Ed, "a", 2)}]))),
    sunk(),
    ?assertEqual(#{<<"id">> => <<"LEENEW">>, <<"user">> => LEE, <<"stream_id">> => <<"2">>},
                 attrs(bind(New, <<"LEENEW">>, [{<<"replaces">>, <<"LEEOLD">>}]))),
    Out = sunk(),
    ?assert(lists:member({queue, <<"lee">>, <<"LEEOLD">>, {device_deleted, <<"LEEOLD">>}}, Out)),
    ?assertEqual([#{<<"user_id">> => LEE, <<"device_id">> => <<"LEEOLD">>, <<"stream_id">> => 2,
                    <<"prev_id">> => [1], <<"deleted">> => true}],
                 [C || {edu, <<"far.test">>, #{<<"content">> := C}} <- Out]),
    ?assertEqual([{<<"LEENEW">>, <<"new">>}], ?E2E:devices_of(?HOST, <<"lee">>)),
    ?assertEqual(0, ?E2E:otk_count(?HOST, <<"lee">>, <<"LEEOLD">>)),
    %% the old binding is still known, to be told
    ?assertEqual({ok, <<"old">>}, ?E2E:resource_of(?HOST, <<"lee">>, <<"LEEOLD">>)),
    ?E2E:forget_device(?HOST, <<"lee">>, <<"LEEOLD">>),
    ?assertEqual(error, ?E2E:resource_of(?HOST, <<"lee">>, <<"LEEOLD">>)),
    %% an active device is not forgotten so
    ?E2E:forget_device(?HOST, <<"lee">>, <<"LEENEW">>),
    ?assertEqual({ok, <<"new">>}, ?E2E:resource_of(?HOST, <<"lee">>, <<"LEENEW">>)),
    %% replacing what is not there: nothing told
    ?assertMatch(#xmlel{}, bind(New, <<"LEENEW">>, [{<<"replaces">>, <<"LEEGONE">>}])),
    ?assertEqual([], sunk()),
    %% <not-mine/>: the device deleted
    ?E2E:delete_device(?HOST, <<"lee">>, <<"LEENEW">>),
    [#{<<"deleted">> := true, <<"device_id">> := <<"LEENEW">>, <<"stream_id">> := 3}] =
        [C || {edu, <<"far.test">>, #{<<"content">> := C}} <- sunk()],
    ?assertEqual([], ?E2E:devices_of(?HOST, <<"lee">>)),
    ?E2E:delete_device(?HOST, <<"lee">>, <<"LEENEW">>),
    ?assertEqual([], sunk()),
    %% and a deleted id bound again starts over
    ?assertMatch(#xmlel{}, bind(New, <<"LEENEW">>)),
    ?assertEqual([{<<"LEENEW">>, <<"new">>}], ?E2E:devices_of(?HOST, <<"lee">>)),
    sunk().

federation() ->
    {200, Devs} = ?E2E:user_devices(?HOST, ?KIM),
    {Master, Self} = persistent_term:get({?MODULE, kim_xsign}),
    ?assertMatch(#{<<"user_id">> := ?KIM, <<"stream_id">> := 3, <<"master_key">> := Master,
                   <<"self_signing_key">> := Self,
                   <<"devices">> := [#{<<"device_id">> := <<"KIMPHONE">>, <<"keys">> := _}]}, Devs),
    [#{<<"keys">> := DK}] = maps:get(<<"devices">>, Devs),
    %% no device name over federation
    ?assertNot(maps:is_key(<<"unsigned">>, DK)),
    %% A24: one of our domain who is not here looks like one with no devices
    ?assertMatch({200, #{<<"devices">> := [], <<"stream_id">> := 0}},
                 ?E2E:user_devices(?HOST, <<"@nobody:example.com">>)),
    ?assertMatch({404, _}, ?E2E:user_devices(?HOST, ?BOB)),
    ?assertMatch({200, #{<<"devices">> := [], <<"stream_id">> := 0}},
                 ?E2E:user_devices(?HOST, <<"@mia:example.com">>)),
    %% keys/query: all devices, some, none; the unknown {}
    {200, Q} = ?E2E:keys_query(?HOST, #{<<"device_keys">> => #{?KIM => [], <<"@lee:example.com">> => [<<"X">>],
                                                                 ?BOB => [], <<"@nobody:example.com">> => []}}),
    ?assertMatch(#{<<"device_keys">> := #{?KIM := #{<<"KIMPHONE">> := DK}, <<"@lee:example.com">> := #{},
                                          ?BOB := #{}, <<"@nobody:example.com">> := #{}},
                   <<"master_keys">> := #{?KIM := Master}, <<"self_signing_keys">> := #{?KIM := Self}}, Q),
    ?assertEqual(1, maps:size(maps:get(<<"master_keys">>, Q))),
    {200, Q2} = ?E2E:keys_query(?HOST, #{<<"device_keys">> => #{?KIM => [<<"OTHER">>]}}),
    ?assertEqual(#{?KIM => #{}}, maps:get(<<"device_keys">>, Q2)),
    {200, Q3} = ?E2E:keys_query(?HOST, #{<<"device_keys">> => #{?KIM => <<"not a list">>}}),
    ?assertMatch(#{?KIM := #{<<"KIMPHONE">> := _}}, maps:get(<<"device_keys">>, Q3)),
    ?assertMatch({400, _}, ?E2E:keys_query(?HOST, #{})),
    %% keys/claim: ned's device has none left but its fallback
    sunk(),
    {200, C} = ?E2E:keys_claim(?HOST, #{<<"one_time_keys">> =>
                                            #{<<"@ned:example.com">> => #{<<"NEDONE">> => <<"signed_curve25519">>,
                                                                          <<"NOSUCH">> => <<"signed_curve25519">>,
                                                                          <<"X">> => <<"other">>},
                                              ?BOB => #{<<"B">> => <<"signed_curve25519">>},
                                              <<"@kim:example.com">> => <<"bad">>}}),
    ?assertMatch(#{<<"one_time_keys">> := #{<<"@ned:example.com">> :=
                                                #{<<"NEDONE">> := #{<<"signed_curve25519:f">> := #{<<"fallback">> := true}}}}},
                 C),
    ?assertEqual([<<"@ned:example.com">>], maps:keys(maps:get(<<"one_time_keys">>, C))),
    ?assertEqual(1, maps:size(maps:get(<<"@ned:example.com">>, maps:get(<<"one_time_keys">>, C)))),
    ?assertMatch([{queue, <<"ned">>, <<"NEDONE">>, {keys_count, _, 0, true}}], sunk()),
    ?assertMatch({400, _}, ?E2E:keys_claim(?HOST, #{})),
    %% A24: a user of ours who is not here is answered as one without keys
    {200, #{<<"one_time_keys">> := C2}} =
        ?E2E:keys_claim(?HOST, #{<<"one_time_keys">> => #{<<"@nobody:example.com">> => #{<<"X">> => <<"signed_curve25519">>},
                                                           ?BOB => #{<<"B">> => <<"signed_curve25519">>}}}),
    ?assertEqual(#{<<"@nobody:example.com">> => #{}}, C2),
    ?assertMatch({400, _}, ?E2E:keys_claim(?HOST, <<"o.test">>, #{})).

a15_same_resource() ->
    Full = <<"lee@example.com/same">>,
    Ed = keypair(),
    LEE = <<"@lee:example.com">>,
    ?assertMatch(#xmlel{}, bind(Full, <<"LEEA">>)),
    ?assertEqual(1, count(upload(Full, [{<<"device-keys">>, device_keys(LEE, <<"LEEA">>, Ed)},
                                        {<<"one-time-keys">>, otks(LEE, <<"LEEA">>, Ed, "a", 1)}]))),
    sunk(),
    %% the installation on that resource takes another id, replacing its old
    ?assertMatch(#xmlel{}, bind(Full, <<"LEEB">>, [{<<"replaces">>, <<"LEEA">>}])),
    Out = sunk(),
    %% nobody to tell: its queue dropped, no <device-deleted/> for it
    ?assert(lists:member({drop, <<"lee">>, <<"LEEA">>}, Out)),
    ?assertEqual([], [I || {queue, _, <<"LEEA">>, I} <- Out]),
    %% the federation still hears of it
    ?assertMatch([_], [C || {edu, <<"far.test">>, #{<<"content">> := #{<<"device_id">> := <<"LEEA">>,
                                                                        <<"deleted">> := true} = C}} <- Out]),
    %% and its row is gone, tombstone and all
    ?assertEqual([], mnesia:dirty_read(matrix_device, {{<<"lee">>, ?HOST}, <<"LEEA">>})),
    ?assertEqual(error, ?E2E:resource_of(?HOST, <<"lee">>, <<"LEEA">>)),
    ?assertEqual(0, ?E2E:otk_count(?HOST, <<"lee">>, <<"LEEA">>)),
    sunk().

a6_claim_rate() ->
    Full = <<"oli@example.com/o">>,
    OLI = <<"@oli:example.com">>,
    Ed = keypair(),
    ?assertMatch(#xmlel{}, bind(Full, <<"OLIDEV">>)),
    ?assertEqual(10, count(upload(Full, [{<<"device-keys">>, device_keys(OLI, <<"OLIDEV">>, Ed)},
                                         {<<"one-time-keys">>, otks(OLI, <<"OLIDEV">>, Ed, "a", 10)}]))),
    sunk(),
    Ask = #{<<"one_time_keys">> => #{OLI => #{<<"OLIDEV">> => <<"signed_curve25519">>}}},
    ets:delete(matrix_gw_rate, {claims, <<"o.test">>}),
    persistent_term:put({mod_matrix_gw_s2s, limit, claims}, 3),
    Res = try [?E2E:keys_claim(?HOST, <<"o.test">>, Ask) || _ <- lists:seq(1, 8)]
          after persistent_term:erase({mod_matrix_gw_s2s, limit, claims})
          end,
    Ok = [R || {200, _} = R <- Res],
    Refused = [R || {429, #{<<"errcode">> := <<"M_LIMIT_EXCEEDED">>}} = R <- Res],
    %% (a minute's window may turn once meanwhile: at most twice the budget)
    ?assert(length(Ok) =< 6),
    ?assertEqual(8, length(Ok) + length(Refused)),
    %% a refused claim takes no key and tells the device nothing
    ?assertEqual(10 - length(Ok), ?E2E:otk_count(?HOST, <<"oli">>, <<"OLIDEV">>)),
    ?assertEqual(length(Ok), length([K || {queue, <<"oli">>, _, {keys_count, _, _, _} = K} <- sunk()])),
    %% another server has a budget of its own; the endpoint's arity two none
    ?assertMatch({200, _}, ?E2E:keys_claim(?HOST, <<"p.test">>, Ask)),
    ?assertMatch({200, _}, ?E2E:keys_claim(?HOST, Ask)),
    sunk().

%% Bob's server, as the suite stands it in: Fed(Server, Path, JSON) noted.
bob_keys() ->
    BobEd = keypair(),
    DK = device_keys(?BOB, <<"BOBDEV">>, BobEd),
    {MPub, _} = keypair(),
    #{<<"device_keys">> => #{?BOB => #{<<"BOBDEV">> => DK,
                                       %% an entry that names another device: dropped
                                       <<"LIAR">> => DK}},
      <<"master_keys">> => #{?BOB => csk(?BOB, <<"master">>, MPub)}}.

fed_calls() ->
    L = ets:lookup(?FED, call),
    ets:delete(?FED, call),
    [X || {call, X} <- L].

remote() ->
    Laptop = <<"kim@example.com/laptop">>,
    Answer = bob_keys(),
    Up = fun(Res) ->
                 ?E2E:set_federation(fun(Server, Path, JSON) ->
                                             ets:insert(?FED, {call, {Server, Path, JSON}}),
                                             Res
                                     end)
         end,
    Up({ok, Answer}),
    CARL = <<"@carl:far.test">>,
    #{?BOB := {#{<<"fresh">> := <<"1">>, <<"stream_id">> := <<"0">>}, Obj},
      CARL := {#{<<"fresh">> := <<"1">>}, #{<<"device_keys">> := #{}}},
      <<"bad">> := {#{<<"fresh">> := <<"0">>}, _}} = users(query(Laptop, [?BOB, CARL, <<"bad">>])),
    ?assertEqual([<<"BOBDEV">>], maps:keys(maps:get(<<"device_keys">>, Obj))),
    ?assert(maps:is_key(<<"master_key">>, Obj)),
    ?assertNot(maps:is_key(<<"self_signing_key">>, Obj)),
    ?assertEqual(lists:sort([{<<"remote.test">>, [<<"user">>, <<"keys">>, <<"query">>],
                              #{<<"device_keys">> => #{?BOB => []}}},
                             {<<"far.test">>, [<<"user">>, <<"keys">>, <<"query">>],
                              #{<<"device_keys">> => #{CARL => []}}}]),
                 lists:sort(fed_calls())),
    %% within the hour: the copy, nobody asked
    #{?BOB := {#{<<"fresh">> := <<"1">>}, Obj}} = users(query(Laptop, [?BOB])),
    ?assertEqual([], fed_calls()),
    %% stale (an EDU said so), the server down: the copy, fresh='0'
    ?assertEqual(ok, ?E2E:inbound_edu(?HOST, <<"remote.test">>,
                                      #{<<"edu_type">> => <<"m.device_list_update">>,
                                        <<"content">> => #{<<"user_id">> => ?BOB, <<"stream_id">> => 7}})),
    Up(error),
    #{?BOB := {#{<<"fresh">> := <<"0">>, <<"stream_id">> := <<"7">>}, Obj}} = users(query(Laptop, [?BOB])),
    ?assertEqual(1, length(fed_calls())),
    %% a server that answers nonsense is down too
    Up({ok, #{<<"x">> => 1}}),
    #{?BOB := {#{<<"fresh">> := <<"0">>}, _}} = users(query(Laptop, [?BOB])),
    Up(crash),
    #{?BOB := {#{<<"fresh">> := <<"0">>}, _}} = users(query(Laptop, [?BOB])),
    %% back: fresh again, the stream kept
    Up({ok, Answer}),
    #{?BOB := {#{<<"fresh">> := <<"1">>, <<"stream_id">> := <<"7">>}, _}} = users(query(Laptop, [?BOB])),
    %% never fetched and the server down: nothing, fresh='0'
    Up(error),
    #{<<"@dan:down.test">> := {#{<<"fresh">> := <<"0">>}, #{<<"device_keys">> := #{}}}} =
        users(query(Laptop, [<<"@dan:down.test">>])),
    fed_calls(),
    %% the bound: the least recently served go first
    ?E2E:set_remote_max(3),
    try
        Now = erlang:system_time(millisecond),
        [begin
             timer:sleep(2),
             ?E2E:store_remote(?HOST, <<"@u", (integer_to_binary(I))/binary, ":lru.test">>, #{devices => #{}})
         end || I <- lists:seq(1, 3)],
        timer:sleep(2),
        ?assertEqual(3, mnesia:table_info(matrix_remote_user, size)),
        ?assertMatch({ok, _, _}, ?E2E:remote_copy(<<"@u1:lru.test">>, Now + 100000)),
        ?E2E:store_remote(?HOST, <<"@u4:lru.test">>, #{devices => none, master => <<"no map">>}),
        ?assertEqual(3, mnesia:table_info(matrix_remote_user, size)),
        ?assertMatch({ok, _, _}, ?E2E:remote_copy(<<"@u1:lru.test">>, Now)),
        ?assertEqual(error, ?E2E:remote_copy(<<"@u2:lru.test">>, Now)),
        ?assertMatch({ok, 0, #{<<"device_keys">> := #{}}}, ?E2E:remote_copy(<<"@u4:lru.test">>, Now))
    after
        ?E2E:set_remote_max(10000)
    end.

remote_claim() ->
    Laptop = <<"kim@example.com/laptop">>,
    BobEd = keypair(),
    KO = key_object(?BOB, <<"BOBDEV">>, BobEd, false),
    FB = key_object(?BOB, <<"BOB2">>, BobEd, true),
    ?E2E:set_federation(
       fun(<<"remote.test">> = S, Path, JSON) ->
               ets:insert(?FED, {call, {S, Path, JSON}}),
               {ok, #{<<"one_time_keys">> => #{?BOB => #{<<"BOBDEV">> => #{<<"signed_curve25519:k1">> => KO},
                                                         <<"BOB2">> => #{<<"signed_curve25519:fb">> => FB},
                                                         <<"BOB3">> => #{<<"bad id">> => KO}}}}};
          (_, _, _) ->
               error
       end),
    S = <<"signed_curve25519">>,
    Got = claimed(claim(Laptop, [{?BOB, <<"BOBDEV">>, S}, {?BOB, <<"BOB2">>, S}, {?BOB, <<"BOB3">>, S},
                                 {<<"@carl:far.test">>, <<"C">>, S}, {<<"nonsense">>, <<"D">>, S}])),
    ?assertMatch([{<<"key">>, #{<<"device">> := <<"BOB2">>, <<"id">> := <<"signed_curve25519:fb">>,
                                <<"fallback">> := <<"1">>}, FB},
                  {<<"missing">>, #{<<"device">> := <<"BOB3">>}, none},
                  {<<"key">>, #{<<"device">> := <<"BOBDEV">>, <<"id">> := <<"signed_curve25519:k1">>,
                                <<"fallback">> := <<"0">>}, KO},
                  {<<"missing">>, #{<<"user">> := <<"@carl:far.test">>}, none},
                  {<<"missing">>, #{<<"user">> := <<"nonsense">>}, none}], Got),
    ?assertEqual([{<<"remote.test">>, [<<"user">>, <<"keys">>, <<"claim">>],
                   #{<<"one_time_keys">> => #{?BOB => #{<<"BOBDEV">> => S, <<"BOB2">> => S, <<"BOB3">> => S}}}}],
                 fed_calls()).

edus_in() ->
    ?E2E:store_remote(?HOST, ?BOB, #{devices => #{}}),
    sunk(),
    %% kim shares !r1 with bob (upload/0); bob's server says his devices changed
    Edu = fun(Origin, Type, C) -> ?E2E:inbound_edu(?HOST, Origin, #{<<"edu_type">> => Type, <<"content">> => C}) end,
    ?assertEqual(ok, Edu(<<"remote.test">>, <<"m.device_list_update">>,
                         #{<<"user_id">> => ?BOB, <<"device_id">> => <<"B">>, <<"stream_id">> => 9})),
    Out = sunk(),
    ?assertEqual(lists:sort([{queue, <<"kim">>, D, {devices_changed, [{?BOB, 9}]}}
                             || {D, _} <- ?E2E:devices_of(?HOST, <<"kim">>)]),
                 lists:sort(Out)),
    %% the copy is stale now
    [R] = mnesia:dirty_read(matrix_remote_user, ?BOB),
    ?assertEqual({true, 9}, {element(8, R), element(3, R)}),
    %% a signing key update: the stream the copy has
    ?assertEqual(ok, Edu(<<"remote.test">>, <<"m.signing_key_update">>, #{<<"user_id">> => ?BOB})),
    ?assertMatch([{queue, <<"kim">>, _, {devices_changed, [{?BOB, 9}]}} | _], sunk()),
    %% an unknown user: stream 0 if none given, nobody shares a room with him
    ?assertEqual(ok, Edu(<<"remote.test">>, <<"m.signing_key_update">>, #{<<"user_id">> => <<"@zed:remote.test">>})),
    ?assertEqual(ok, Edu(<<"remote.test">>, <<"m.device_list_update">>,
                         #{<<"user_id">> => <<"@zed:remote.test">>, <<"stream_id">> => 4})),
    ?assertEqual([], mnesia:dirty_read(matrix_remote_user, <<"@zed:remote.test">>)),
    %% from another server than the user's: ignored
    ?assertEqual(ok, Edu(<<"evil.test">>, <<"m.device_list_update">>, #{<<"user_id">> => ?BOB, <<"stream_id">> => 1})),
    ?assertEqual([], sunk()),
    %% not ours
    ?assertEqual(pass, ?E2E:inbound_edu(?HOST, <<"remote.test">>, #{<<"edu_type">> => <<"m.typing">>,
                                                                    <<"content">> => #{}})),
    ?assertEqual(pass, ?E2E:inbound_edu(?HOST, <<"remote.test">>, #{<<"edu_type">> => <<"m.device_list_update">>,
                                                                    <<"content">> => #{}})).

membership() ->
    M = fun(R, U, Ms) -> ?E2E:membership(?HOST, R, U, Ms) end,
    MIA = <<"@mia:example.com">>,
    ?assertEqual([], ?E2E:peer_servers(?HOST, <<"mia">>)),
    M(<<"!a:one.test">>, MIA, join),
    M(<<"!a:one.test">>, <<"@x:one.test">>, join),
    M(<<"!a:one.test">>, <<"@y:two.test">>, invite),
    M(<<"!b:one.test">>, MIA, join),
    M(<<"!b:one.test">>, <<"@y:two.test">>, join),
    M(<<"!b:one.test">>, ?KIM, join),
    ?assertEqual([<<"one.test">>, <<"two.test">>], ?E2E:peer_servers(?HOST, <<"mia">>)),
    ?assertEqual([<<"kim">>, <<"mia">>], ?E2E:room_share(?HOST, <<"@y:two.test">>)),
    ?assertEqual([<<"kim">>], ?E2E:room_share(?HOST, MIA)),
    %% y leaves one room: still in the other (a set per room, no counter)
    M(<<"!a:one.test">>, <<"@y:two.test">>, <<"leave">>),
    ?assertEqual([<<"one.test">>, <<"two.test">>], ?E2E:peer_servers(?HOST, <<"mia">>)),
    M(<<"!b:one.test">>, <<"@y:two.test">>, ban),
    ?assertEqual([<<"one.test">>], ?E2E:peer_servers(?HOST, <<"mia">>)),
    ?assertEqual([], ?E2E:room_share(?HOST, <<"@y:two.test">>)),
    %% a direct chat shares too (the rooms' own table)
    mnesia:dirty_write({matrix_direct, {{<<"mia">>, ?HOST}, <<"@d:three.test">>}, <<"!dm:example.com">>}),
    ?assertEqual([<<"one.test">>, <<"three.test">>], ?E2E:peer_servers(?HOST, <<"mia">>)),
    ?assertEqual([<<"mia">>], ?E2E:room_share(?HOST, <<"@d:three.test">>)),
    mnesia:dirty_delete(matrix_direct, {{<<"mia">>, ?HOST}, <<"@d:three.test">>}),
    %% nonsense is no member
    ?assertEqual(ok, M(undefined, MIA, join)),
    M(<<"!a:one.test">>, MIA, leave),
    M(<<"!b:one.test">>, MIA, leave),
    ?assertEqual([], ?E2E:peer_servers(?HOST, <<"mia">>)).

%% Matrix servers send m.device_list_update only to the servers already
%% sharing a room with the user: a copy taken before is stale once a room
%% is shared (a device made while no room was shared never reached the
%% gateway).
newly_shared() ->
    M = fun(R, U, Ms) -> ?E2E:membership(?HOST, R, U, Ms) end,
    OLI = <<"@oli:example.com">>,
    ZOE = <<"@zoe:new.test">>,
    Stale = fun(U) -> [R] = mnesia:dirty_read(matrix_remote_user, U), element(8, R) end,
    ?assertMatch(#xmlel{}, bind(<<"oli@example.com/a">>, <<"OLIA">>)),
    ?assertMatch(#xmlel{}, bind(<<"oli@example.com/b">>, <<"OLIB">>)),
    %% zoe's keys fetched while no room is shared with her (a <keys-query/>)
    ?E2E:store_remote(?HOST, ZOE, #{devices => #{}}),
    [R0] = mnesia:dirty_read(matrix_remote_user, ZOE),
    mnesia:dirty_write(setelement(3, R0, 5)),
    ?assertEqual(false, Stale(ZOE)),
    sunk(),
    %% the room as a join's state brings it: zoe's row first, nobody local yet
    M(<<"!n1:new.test">>, ZOE, join),
    ?assertEqual(false, Stale(ZOE)),
    ?assertEqual([], sunk()),
    %% then oli's: zoe newly shared, the copy stale, oli's devices told
    M(<<"!n1:new.test">>, OLI, join),
    ?assertEqual(true, Stale(ZOE)),
    Olis = lists:sort([D || {D, _} <- ?E2E:devices_of(?HOST, <<"oli">>)]),
    ?assert(lists:member(<<"OLIA">>, Olis) andalso lists:member(<<"OLIB">>, Olis)),
    ?assertEqual([{queue, <<"oli">>, D, {devices_changed, [{ZOE, 5}]}} || D <- Olis],
                 lists:sort(sunk())),
    %% fetched again: fresh; the same state again (rows there) changes nothing
    ?E2E:store_remote(?HOST, ZOE, #{devices => #{}}),
    ?assertEqual(false, Stale(ZOE)),
    M(<<"!n1:new.test">>, OLI, join),
    M(<<"!n1:new.test">>, ZOE, invite),
    ?assertEqual(false, Stale(ZOE)),
    ?assertEqual([], sunk()),
    %% another room with her while one is shared: nothing
    M(<<"!n2:new.test">>, OLI, join),
    M(<<"!n2:new.test">>, ZOE, invite),
    ?assertEqual(false, Stale(ZOE)),
    ?assertEqual([], sunk()),
    %% her invite taken up (a join on her row): stale and told, as an EDU would
    M(<<"!n2:new.test">>, ZOE, join),
    ?assertEqual(true, Stale(ZOE)),
    ?assertEqual(length(Olis), length([x || {queue, <<"oli">>, _, {devices_changed, [{Z, 5}]}} <- sunk(), Z == ZOE])),
    %% a remote user joining a room a local user is in: her own new row
    ?E2E:store_remote(?HOST, ZOE, #{devices => #{}}),
    M(<<"!n1:new.test">>, ZOE, leave),
    M(<<"!n2:new.test">>, ZOE, leave),
    ?assertEqual(true, Stale(ZOE)),
    ?E2E:store_remote(?HOST, ZOE, #{devices => #{}}),
    sunk(),
    M(<<"!n2:new.test">>, ZOE, invite),
    ?assertEqual(true, Stale(ZOE)),
    ?assertEqual(length(Olis), length([x || {queue, <<"oli">>, _, {devices_changed, [{Z, _}]}} <- sunk(), Z == ZOE])),
    %% a user never fetched: no copy made, the devices told (stream 0)
    QUI = <<"@qui:new.test">>,
    M(<<"!n2:new.test">>, QUI, join),
    ?assertEqual([], mnesia:dirty_read(matrix_remote_user, QUI)),
    ?assertEqual(length(Olis), length([x || {queue, <<"oli">>, _, {devices_changed, [{Q, 0}]}} <- sunk(), Q == QUI])),
    %% local users' rows among themselves and nonsense: nothing
    M(<<"!n2:new.test">>, ?KIM, join),
    M(<<"!n2:new.test">>, <<"not an id">>, join),
    ?assertEqual([], sunk()),
    [M(<<"!n2:new.test">>, U, leave) || U <- [ZOE, QUI, ?KIM, <<"not an id">>, OLI]],
    M(<<"!n1:new.test">>, OLI, leave),
    sunk().

%% The spec's other half: nobody here shares a room with the user any
%% more, so no EDU keeps the copy fresh.
unshared() ->
    M = fun(R, U, Ms) -> ?E2E:membership(?HOST, R, U, Ms) end,
    OLI = <<"@oli:example.com">>,
    MIA = <<"@mia:example.com">>,
    ZOE = <<"@zoe:new.test">>,
    Stale = fun(U) -> [R] = mnesia:dirty_read(matrix_remote_user, U), element(8, R) end,
    M(<<"!u1:new.test">>, OLI, join),
    M(<<"!u1:new.test">>, MIA, join),
    M(<<"!u1:new.test">>, ZOE, join),
    M(<<"!u2:new.test">>, MIA, join),
    M(<<"!u2:new.test">>, ZOE, join),
    ?E2E:store_remote(?HOST, ZOE, #{devices => #{}}),
    sunk(),
    %% oli leaves; mia still shares both rooms
    M(<<"!u1:new.test">>, OLI, leave),
    ?assertEqual(false, Stale(ZOE)),
    %% mia leaves one; the other room is still shared
    M(<<"!u1:new.test">>, MIA, leave),
    ?assertEqual(false, Stale(ZOE)),
    %% a leave of a row never there: nothing
    M(<<"!u3:new.test">>, ZOE, leave),
    ?assertEqual(false, Stale(ZOE)),
    %% zoe leaves the last shared room (she stays in u1 with nobody local)
    M(<<"!u2:new.test">>, ZOE, leave),
    ?assertEqual(true, Stale(ZOE)),
    ?assertEqual([], sunk()),
    %% the local side going does the same: back in u2, fetched, then mia goes
    M(<<"!u2:new.test">>, ZOE, join),
    ?E2E:store_remote(?HOST, ZOE, #{devices => #{}}),
    sunk(),
    M(<<"!u2:new.test">>, MIA, ban),
    ?assertEqual(true, Stale(ZOE)),
    ?assertEqual([], sunk()),
    %% a direct chat still shares: not stale
    M(<<"!u2:new.test">>, MIA, join),
    ?E2E:store_remote(?HOST, ZOE, #{devices => #{}}),
    mnesia:dirty_write({matrix_direct, {{<<"mia">>, ?HOST}, ZOE}, <<"!dm2:example.com">>}),
    M(<<"!u2:new.test">>, MIA, leave),
    ?assertEqual(false, Stale(ZOE)),
    mnesia:dirty_delete(matrix_direct, {{<<"mia">>, ?HOST}, ZOE}),
    [M(R, ZOE, leave) || R <- [<<"!u1:new.test">>, <<"!u2:new.test">>]],
    sunk().

through() ->
    %% the gateway's own disco#info
    #iq{type = result, sub_els = [#disco_info{features = Fs}]} =
        mod_matrix_gw:process_disco_info(#iq{type = get, id = <<"d">>, from = j(<<"kim@example.com/phone">>),
                                             to = j(?GW), sub_els = [#disco_info{node = <<>>}]}),
    ?assert(lists:member(?NS, Fs)),
    %% a Matrix user's JID at the gateway, as a client asks
    session(<<"kim@example.com/phone">>),
    ejabberd_router:route(#iq{type = get, id = <<"d2">>, from = j(<<"kim@example.com/phone">>),
                              to = j(<<"bob%remote.test@", ?GW/binary>>),
                              sub_els = [xmpp:encode(#disco_info{node = <<>>})]}),
    receive
        {route, #iq{id = <<"d2">>, type = result, sub_els = [El]}} ->
            #disco_info{features = UFs} = xmpp:decode(El),
            ?assert(lists:member(?NS, UFs))
    after 5000 -> error(timeout)
    end,
    %% mod_matrix_gw's route/1 hands the iqs here
    session(<<"mia@example.com/m">>),
    ejabberd_router:route(#iq{type = get, id = <<"q">>, from = j(<<"mia@example.com/m">>), to = j(?GW),
                              sub_els = [el(<<"keys-query">>, [], [el(<<"user">>, [{<<"id">>, ?KIM}], [])])]}),
    receive
        {route, #iq{id = <<"q">>, type = result}} -> ok
    after 5000 -> error(timeout)
    end.

%% one resource binding fresh ids over and over leaves each
%% earlier device unbound; past the cap (3 here) the oldest goes, told to
%% the federation and the user's devices as a deleted device.
s15_device_cap() ->
    ok = reg(<<"pat">>),
    persistent_term:put({?E2E, max_devices}, 3),
    try
        Full = <<"pat@example.com/one">>,
        Other = <<"pat@example.com/two">>,
        Ids = fun() -> lists:sort([D || {D, _} <- ?E2E:devices_of(?HOST, <<"pat">>)]) end,
        [begin ?assertMatch(#xmlel{}, bind(Full, D)), timer:sleep(5) end
         || D <- [<<"PATA">>, <<"PATB">>, <<"PATC">>]],
        ?assertEqual([<<"PATA">>, <<"PATB">>, <<"PATC">>], Ids()),
        _ = sunk(),
        %% the fourth: PATA, the oldest unbound, is retired
        ?assertMatch(#xmlel{}, bind(Full, <<"PATD">>)),
        ?assertEqual([<<"PATB">>, <<"PATC">>, <<"PATD">>], Ids()),
        Sunk = sunk(),
        %% its queue dropped, the others told the list changed
        ?assert(lists:member({drop, <<"pat">>, <<"PATA">>}, Sunk)),
        ?assert(lists:any(fun({queue, <<"pat">>, <<"PATB">>, {devices_changed, _}}) -> true;
                             (_) -> false
                          end, Sunk)),
        ?assertEqual(error, ?E2E:resource_of(?HOST, <<"pat">>, <<"PATA">>)),
        %% the same id again is no new device: nothing retired
        ?assertMatch(#xmlel{}, bind(Full, <<"PATD">>)),
        ?assertEqual([<<"PATB">>, <<"PATC">>, <<"PATD">>], Ids()),
        %% another resource: the oldest unbound again (PATB), the bound
        %% and online PATD stays
        ?assertMatch(#xmlel{}, bind(Other, <<"PATE">>)),
        ?assertEqual([<<"PATC">>, <<"PATD">>, <<"PATE">>], Ids())
    after
        persistent_term:erase({?E2E, max_devices})
    end.
