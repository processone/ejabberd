%%% The Matrix gateway's federation endpoints against a hostile server,
%%% on an ejabberd started
%%% with the gateway: requests signed as the suite's own servers, whose keys
%%% the suite puts into their connections (mod_matrix_gw_s2s), handed to
%%% mod_matrix_gw:process/2 as ejabberd_http would; a direct room of
%%% ann@example.com with @bob:far.test brought back from the store.
-module(mod_matrix_gw_fed_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").
-include("mod_matrix_gw.hrl").

-define(HOST, <<"example.com">>).
-define(S2S, mod_matrix_gw_s2s).
-define(ROOM, <<"!fedroom:example.com">>).
-define(ANN, <<"@ann:example.com">>).
-define(BOB, <<"@bob:far.test">>).
-define(KEYID, <<"ed25519:k">>).

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------

server_name_test_() ->
    V = fun ?S2S:valid_server_name/1,
    [?_assert(V(<<"matrix.org">>)),
     ?_assert(V(<<"matrix.org:8448">>)),
     ?_assert(V(<<"127.0.0.1:1">>)),
     ?_assert(V(<<"[::1]:8448">>)),
     ?_assertNot(V(<<"bad name">>)),
     ?_assertNot(V(<<"evil.example/x">>)),
     ?_assertNot(V(<<"a:b:c">>)),
     ?_assertNot(V(<<>>)),
     ?_assertNot(V(binary:copy(<<"a">>, 256))),
     ?_assertNot(V(17))].

%%--------------------------------------------------------------------
%% On a running gateway
%%--------------------------------------------------------------------

ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup,
     fun() ->
             Dir = mod_matrix_gw_test_lib:start_ejabberd(?MODULE),
             [ok = reg(U) || U <- [<<"ann">>, <<"cid">>]],
             Far = fake_server(<<"far.test">>),
             Other = fake_server(<<"other.test">>),
             persistent_term:put({?MODULE, servers}, {Far, Other}),
             room(Far),
             {Dir, Far}
     end,
     fun({Dir, _}) ->
             [persistent_term:erase({?S2S, limit, L}) || L <- [new_origins, invites, claims, idle_ms,
                                                              new_connections, max_connections,
                                                              claims_device, claims_all, invites_user,
                                                              invites_all, e2e_edus_all,
                                                              edu_retry_ms, edu_backoff_min]],
             persistent_term:erase({?S2S, edu_sender}),
             mod_matrix_gw_test_lib:stop_ejabberd(Dir)
     end,
     fun(_) ->
             [{timeout, 60, {Name, F}}
              || {Name, F} <- [{"rate/4: a window, its maximum, a cost over it", fun rate/0},
                               {"a /send PDU whose content was changed is kept redacted", fun a3_content_hash/0},
                               {"unauthenticated origins leave no connection", fun a4_unauthenticated/0},
                               {"/v2/invite checks the event, the user, the room, the rate", fun a5_invite/0},
                               {"a direct chat whose join fails ends its process", fun a5_failed_join/0},
                               {"/user/keys/claim within the origin's claims", fun a6_claims/0},
                               {"/event and /backfill only to a server in the room", fun a13_event/0},
                               {"/user/devices of a user who is not here", fun a24_devices/0},
                               {"an unused connection stops", fun a4_idle/0},
                               {"connections only within the gate", fun s1_gate/0},
                               {"PDU senders of servers not in the room fetch no key", fun s1_sender_domains/0},
                               {"a remote alias from another server is not resolved", fun s1_alias/0},
                               {"a PDU of the wrong shape never ends the room", fun s2_prev_events/0},
                               {"missing events authorised against the state before them", fun s3_state_before/0},
                               {"the history only from a sender not banned", fun s3_history/0},
                               {"answers read up to a bound", fun s4_bounded/0},
                               {"claims per device and in all, invites per user, EDUs in all", fun s7_global/0},
                               {"a missing event of another room is dropped", fun s8_room_id/0},
                               {"the invite names the signed event's room", fun s9_invite_jid/0},
                               {"V12: the auth rules of room version 12", fun v12_rules/0},
                               {"V12: a missing event's /state is the state before it", fun v12_state_before/0},
                               {"V11: a missing event's /state is the state before it", fun v11_state_before/0},
                               {"a to-device EDU outlives its tries and a restart", fun l1_edus_held/0}]]
     end}.

reg(U) ->
    case ejabberd_auth:try_register(U, ?HOST, <<"secret">>) of
        ok -> ok;
        {error, exists} -> ok
    end.

servers() -> persistent_term:get({?MODULE, servers}).

b64(B) -> mod_matrix_gw:base64_encode(B).

%% A server the suite stands in for: its connection made, its key put in.
fake_server(Name) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, Pid} = ?S2S:get_connection(?HOST, Name),
    Until = erlang:system_time(millisecond) + 3600000,
    sys:replace_state(Pid, fun({St, Data}) ->
                                   #{} = element(8, Data),
                                   {St, setelement(8, Data, #{?KEYID => {Pub, Until}})}
                           end, 30000),
    {Name, Priv}.

name({Name, _}) -> Name.

keyid({_, own}) -> <<"ed25519:test">>;
keyid(_) -> ?KEYID.

sign({_, own}, JSON) ->
    mod_matrix_gw:sign_json(?HOST, JSON);
sign({Name, Priv}, JSON) ->
    Sigs = maps:get(<<"signatures">>, JSON, #{}),
    Msg = mod_matrix_gw:encode_canonical_json(maps:without([<<"signatures">>, <<"unsigned">>], JSON)),
    Sig = b64(crypto:sign(eddsa, none, Msg, [Priv, ed25519])),
    JSON#{<<"signatures">> => Sigs#{Name => #{?KEYID => Sig}}}.

%% A request of Server's, signed, as ejabberd_http hands it over.
req(Server, Method, Path, Q, Body) ->
    URI = <<"/_matrix/", (str:join(Path, <<"/">>))/binary>>,
    Name = name(Server),
    Base = #{<<"method">> => atom_to_binary(Method), <<"uri">> => URI,
             <<"origin">> => Name, <<"destination">> => ?HOST},
    {Data, Signed} = case Body of
                         none -> {<<>>, Base};
                         _ -> {misc:json_encode(Body), Base#{<<"content">> => Body}}
                     end,
    KeyID = keyid(Server),
    #{<<"signatures">> := #{Name := #{KeyID := Sig}}} = sign(Server, Signed),
    Auth = <<"X-Matrix origin=", Name/binary, ",key=\"", KeyID/binary, "\",sig=\"", Sig/binary,
             "\",destination=\"", ?HOST/binary, "\"">>,
    #request{method = Method, path = Path, q = Q, headers = [{'Authorization', Auth}],
             raw_path = URI, length = byte_size(Data), data = Data, host = ?HOST,
             ip = {{127, 0, 0, 1}, 1}}.

call(Server, Method, Path, Q, Body) ->
    case mod_matrix_gw:process(Path, req(Server, Method, Path, Q, Body)) of
        %% a status ejabberd_http has no phrase of its own for brings one
        {Code, <<_, _/binary>>, _, <<"{", _/binary>> = B} -> {Code, misc:json_decode(B)};
        {Code, _, <<"{", _/binary>> = B} -> {Code, misc:json_decode(B)};
        {Code, _, B} -> {Code, B}
    end.

version() -> mod_matrix_gw_room:binary_to_room_version(<<"9">>).

%% An event of Server's, hashed and signed: {EventID, JSON}.
event(Server, Fields) ->
    V = version(),
    JSON0 = Fields#{<<"origin">> => name(Server)},
    JSON1 = JSON0#{<<"hashes">> => #{<<"sha256">> => b64(mod_matrix_gw:content_hash(JSON0))}},
    Pruned = maps:without([<<"age_ts">>, <<"unsigned">>], mod_matrix_gw:prune_event(JSON1, V)),
    JSON = JSON1#{<<"signatures">> => maps:get(<<"signatures">>, sign(Server, Pruned))},
    {mod_matrix_gw:get_event_id(JSON, V), JSON}.

%% The room: ann's, bob invited and joined, kept in the store and brought
%% back; its events' ids by name in the persistent term.
room(Far) ->
    Own = own(),
    Steps = [{create, Own, <<"m.room.create">>, <<>>, ?ANN, #{<<"creator">> => ?ANN, <<"room_version">> => <<"9">>}},
             {ann, Own, <<"m.room.member">>, ?ANN, ?ANN, #{<<"membership">> => <<"join">>}},
             {pl, Own, <<"m.room.power_levels">>, <<>>, ?ANN, #{<<"users">> => #{?ANN => 100}}},
             {jr, Own, <<"m.room.join_rules">>, <<>>, ?ANN, #{<<"join_rule">> => <<"invite">>}},
             {inv, Own, <<"m.room.member">>, ?BOB, ?ANN, #{<<"membership">> => <<"invite">>, <<"is_direct">> => true}},
             {bob, Far, <<"m.room.member">>, ?BOB, ?BOB, #{<<"membership">> => <<"join">>}}],
    {_, _, _, Events} =
        lists:foldl(
          fun({Tag, Server, Type, Key, Sender, Content}, {Prev, State, N, Acc}) ->
                  {ID, JSON} = event(Server, #{<<"type">> => Type, <<"state_key">> => Key,
                                               <<"content">> => Content, <<"sender">> => Sender,
                                               <<"room_id">> => ?ROOM,
                                               <<"origin_server_ts">> => 1700000000000 + N,
                                               <<"depth">> => N + 1, <<"prev_events">> => Prev,
                                               <<"auth_events">> => maps:values(State)}),
                  State2 = State#{{Type, Key} => ID},
                  persistent_term:put({?MODULE, id, Tag}, ID),
                  {[ID], State2, N + 1, [{ID, JSON, State2} | Acc]}
          end, {[], #{}, 0, []}, Steps),
    [{Last, _, _} | _] = Events,
    ok = mod_matrix_gw_store:save_meta(?ROOM, <<"9">>, {direct, {<<"ann">>, ?HOST}, ?BOB, established}, [Last]),
    ok = mod_matrix_gw_store:save_events(?ROOM, Events, [Last]),
    mod_matrix_gw_room:restore_rooms(?HOST),
    {ok, _} = mod_matrix_gw_room:get_room_pid(?HOST, ?ROOM),
    ok.

id(Tag) -> persistent_term:get({?MODULE, id, Tag}).

%% This server, signing with the configured key.
own() -> {?HOST, own}.

%% A message of bob's after his join.
message(Far, N, Body) ->
    event(Far, #{<<"type">> => <<"m.room.message">>, <<"sender">> => ?BOB, <<"room_id">> => ?ROOM,
                 <<"content">> => #{<<"msgtype">> => <<"m.text">>, <<"body">> => Body},
                 <<"origin_server_ts">> => 1700000001000 + N, <<"depth">> => 7,
                 <<"prev_events">> => [id(bob)],
                 <<"auth_events">> => [id(create), id(pl), id(bob)]}).

%%--------------------------------------------------------------------

rate() ->
    K = {?MODULE, erlang:unique_integer()},
    ?assert(?S2S:rate(K, 3, 60000, 2)),
    ?assert(?S2S:rate(K, 3, 60000, 1)),
    ?assertNot(?S2S:rate(K, 3, 60000, 1)),
    ?assertNot(?S2S:rate({?MODULE, x}, 3, 60000, 4)),
    ?assert(?S2S:rate({?MODULE, y}, 3, 60000, 0)),
    ?assertEqual(5, ?S2S:limit(nothing_set, 5)).

a3_content_hash() ->
    {Far, _} = servers(),
    %% an event of bob's, its content changed after it was signed: the id
    %% and the signature still verify, the hash does not
    {ID, JSON} = message(Far, 1, <<"the real words">>),
    Forged = JSON#{<<"content">> => #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"forged words">>}},
    ?assertEqual({ok, ID}, mod_matrix_gw_room:process_pdu(?HOST, <<"far.test">>, Forged)),
    {ok, Stored} = mod_matrix_gw_room:get_event(?HOST, ?ROOM, ID),
    ?assertEqual(#{}, maps:get(<<"content">>, Stored)),
    %% the genuine copy coming later changes nothing
    ?assertEqual({ok, ID}, mod_matrix_gw_room:process_pdu(?HOST, <<"far.test">>, JSON)),
    ?assertEqual({ok, Stored}, mod_matrix_gw_room:get_event(?HOST, ?ROOM, ID)),
    %% an event as signed keeps its content
    {ID2, JSON2} = message(Far, 2, <<"untouched">>),
    ?assertEqual({ok, ID2}, mod_matrix_gw_room:process_pdu(?HOST, <<"far.test">>, JSON2)),
    {ok, Stored2} = mod_matrix_gw_room:get_event(?HOST, ?ROOM, ID2),
    ?assertMatch(#{<<"body">> := <<"untouched">>}, maps:get(<<"content">>, Stored2)),
    %% and a signature that does not verify is still refused
    {_, JSON3} = message(Far, 3, <<"x">>),
    ?assertMatch({error, _}, mod_matrix_gw_room:process_pdu(
                               ?HOST, <<"far.test">>,
                               JSON3#{<<"depth">> => 8})).

s2s_rows() ->
    lists:sort(mnesia:dirty_all_keys(matrix_s2s)).

children() ->
    proplists:get_value(active, supervisor:count_children(?S2S:supervisor(?HOST))).

auth(Origin) ->
    #{<<"origin">> => Origin, <<"key">> => <<"ed25519:a">>, <<"sig">> => <<"x">>}.

check(Origin) ->
    Req = #request{method = 'GET', raw_path = <<"/_matrix/federation/v1/version">>, length = 0},
    ?S2S:check_auth(?HOST, Origin, auth(Origin), none, Req).

a4_unauthenticated() ->
    Rows = s2s_rows(),
    Children = children(),
    %% not a server name: refused before anything
    [?assertNot(check(O)) || O <- [<<"bad name">>, <<"evil.example/x">>, <<"a:b:c">>]],
    ?assertEqual(Rows, s2s_rows()),
    %% a new budget of two servers this minute
    ets:delete(matrix_gw_rate, new_origins),
    persistent_term:put({?S2S, limit, new_origins}, 2),
    Origins = [<<"127.0.0.1:", (integer_to_binary(P))/binary>> || P <- [1, 2, 3, 4]],
    [?assertNot(check(O)) || O <- Origins],
    %% a connection made for a request that did not verify is stopped,
    %% and the server is not tried again for a while
    timer:sleep(500),
    ?assertEqual(Rows, s2s_rows()),
    ?assertEqual(Children, children()),
    Negative = [O || O <- Origins, ets:lookup(matrix_gw_rate, {negative, O}) /= []],
    ?assert(length(Negative) >= 2),
    ?assert(length(Negative) < 4),
    ?assertNot(check(hd(Negative))),
    timer:sleep(200),
    ?assertEqual(Rows, s2s_rows()),
    persistent_term:erase({?S2S, limit, new_origins}),
    %% a known server with a bad signature: false, its connection kept
    {Far, _} = servers(),
    ?assertNot(check(name(Far))),
    ?assert(lists:member(name(Far), s2s_rows())),
    %% and a good one verifies (our own name with our own key too)
    ?assertMatch({200, _}, call(Far, 'GET', [<<"federation">>, <<"v1">>, <<"user">>, <<"devices">>, ?ANN], [], none)),
    ?assertMatch({200, _}, call(own(), 'GET', [<<"federation">>, <<"v1">>, <<"user">>, <<"devices">>, ?ANN], [], none)),
    ?assertMatch({401, _}, call({<<"far.test">>, element(2, crypto:generate_key(eddsa, ed25519))},
                                'GET', [<<"federation">>, <<"v1">>, <<"user">>, <<"devices">>, ?ANN], [], none)).

invite(Server, RoomID, Sender, UserID, Content) ->
    invite(Server, RoomID, Sender, UserID, Content, <<"m.room.member">>).

invite(Server, RoomID, Sender, UserID, Content, Type) ->
    {ID, JSON} = event(Server, #{<<"type">> => Type, <<"state_key">> => UserID, <<"sender">> => Sender,
                                 <<"room_id">> => RoomID, <<"content">> => Content,
                                 <<"origin_server_ts">> => erlang:system_time(millisecond),
                                 <<"depth">> => 5, <<"prev_events">> => [], <<"auth_events">> => []}),
    call(Server, 'PUT', [<<"federation">>, <<"v2">>, <<"invite">>, RoomID, ID], [],
         #{<<"event">> => JSON, <<"room_version">> => <<"9">>}).

a5_invite() ->
    {Far, _} = servers(),
    Inv = #{<<"membership">> => <<"invite">>},
    Direct = Inv#{<<"is_direct">> => true},
    Rooms = mnesia:dirty_all_keys(matrix_room),
    %% a user who is not here
    ?assertMatch({404, #{<<"errcode">> := <<"M_NOT_FOUND">>}},
                 invite(Far, <<"!a:far.test">>, <<"@mallory:far.test">>, <<"@nobody:example.com">>, Direct)),
    %% not a membership invite
    ?assertMatch({400, _}, invite(Far, <<"!a:far.test">>, <<"@mallory:far.test">>, ?ANN, Direct,
                                  <<"m.room.message">>)),
    ?assertMatch({400, _}, invite(Far, <<"!a:far.test">>, <<"@mallory:far.test">>, ?ANN,
                                  #{<<"membership">> => <<"join">>, <<"is_direct">> => true})),
    %% a sender of another server
    ?assertMatch({403, _}, invite(Far, <<"!a:far.test">>, <<"@mallory:other.test">>, ?ANN, Direct)),
    %% a direct invite naming a room the gateway has (ann's with bob)
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, ?ROOM),
    ?assertMatch({403, _}, invite(Far, ?ROOM, <<"@mallory:far.test">>, ?ANN, Direct)),
    ?assertEqual([{matrix_direct, {{<<"ann">>, ?HOST}, ?BOB}, ?ROOM}],
                 mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, ?BOB})),
    ?assertEqual([], mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, <<"@mallory:far.test">>})),
    ?assert(is_process_alive(Pid)),
    ?assertMatch(#room_version{}, gen_statem:call(Pid, get_room_version)),
    ?assertEqual(Rooms, mnesia:dirty_all_keys(matrix_room)),
    %% a room invite (no direct chat) is still taken: a mediated MUC invite
    ?assertMatch({200, #{<<"event">> := _}},
                 invite(Far, <<"!b:far.test">>, <<"@mallory:far.test">>, ?ANN, Inv)),
    %% at most so many invites a minute from one server
    ets:delete(matrix_gw_rate, {invite, <<"far.test">>}),
    persistent_term:put({?S2S, limit, invites}, 2),
    Codes = [element(1, invite(Far, <<"!c:far.test">>, <<"@mallory:far.test">>, ?ANN, Inv))
             || _ <- lists:seq(1, 5)],
    persistent_term:erase({?S2S, limit, invites}),
    ?assert(lists:member(429, Codes)),
    ?assertEqual([], [C || C <- Codes, C /= 200, C /= 429]).

a5_failed_join() ->
    {Far, _} = servers(),
    RoomID = <<"!lost:far.test">>,
    Sender = <<"@mallory:far.test">>,
    ?assertMatch({200, _}, invite(Far, RoomID, Sender, <<"@cid:example.com">>,
                                  #{<<"membership">> => <<"invite">>, <<"is_direct">> => true})),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
    Ref = monitor(process, Pid),
    %% far.test answers no make_join: the room's process ends
    receive {'DOWN', Ref, _, _, _} -> ok after 40000 -> error(still_there) end,
    ?assertEqual([], mnesia:dirty_read(matrix_room, RoomID)),
    ?assertEqual([], mnesia:dirty_read(matrix_direct, {{<<"cid">>, ?HOST}, Sender})).

a6_claims() ->
    {Far, _} = servers(),
    Claim = fun(Devs) ->
                    call(Far, 'POST', [<<"federation">>, <<"v1">>, <<"user">>, <<"keys">>, <<"claim">>], [],
                         #{<<"one_time_keys">> => #{?ANN => maps:from_list([{D, <<"signed_curve25519">>}
                                                                             || D <- Devs])}})
            end,
    ?assertMatch({200, #{<<"one_time_keys">> := #{?ANN := #{}}}}, Claim([<<"A">>])),
    persistent_term:put({?S2S, limit, claims}, 1),
    ?assertMatch({429, #{<<"errcode">> := <<"M_LIMIT_EXCEEDED">>}}, Claim([<<"A">>, <<"B">>])),
    persistent_term:erase({?S2S, limit, claims}).

a13_event() ->
    {Far, Other} = servers(),
    ID = id(bob),
    Event = fun(S) -> call(S, 'GET', [<<"federation">>, <<"v1">>, <<"event">>, ID], [], none) end,
    Backfill = fun(S) ->
                       Path = [<<"federation">>, <<"v1">>, <<"backfill">>, ?ROOM],
                       call(S, 'GET', Path, [{<<"limit">>, <<"5">>}, {<<"v">>, ID}], none)
               end,
    %% far.test is in the room (bob joined), other.test is not
    ?assertMatch({200, #{<<"pdus">> := [#{<<"sender">> := ?BOB}]}}, Event(Far)),
    ?assertMatch({400, _}, Event(Other)),
    {200, #{<<"pdus">> := PDUs}} = Backfill(Far),
    ?assert(length(PDUs) >= 1),
    ?assertMatch({200, #{<<"pdus">> := []}}, Backfill(Other)),
    ?assertMatch({ok, _}, mod_matrix_gw_room:get_event(?HOST, <<"far.test">>, ?ROOM, ID)),
    ?assertEqual({error, not_allowed}, mod_matrix_gw_room:get_event(?HOST, <<"other.test">>, ?ROOM, ID)),
    ?assertEqual({error, event_not_found}, mod_matrix_gw_room:get_event(?HOST, <<"far.test">>, ?ROOM, <<"$none">>)),
    ?assertEqual({error, room_not_found}, mod_matrix_gw_room:get_event(?HOST, <<"far.test">>, <<"!none:x">>, ID)).

a24_devices() ->
    {Far, _} = servers(),
    Devices = fun(U) -> call(Far, 'GET', [<<"federation">>, <<"v1">>, <<"user">>, <<"devices">>, U], [], none) end,
    %% one who is not here looks like one with no devices
    ?assertMatch({200, #{<<"devices">> := [], <<"stream_id">> := 0, <<"user_id">> := <<"@nobody:example.com">>}},
                 Devices(<<"@nobody:example.com">>)),
    ?assertMatch({200, #{<<"devices">> := []}}, Devices(?ANN)),
    ?assertMatch({404, _}, Devices(<<"@x:elsewhere.test">>)).

a4_idle() ->
    persistent_term:put({?S2S, limit, idle_ms}, 300),
    try
        {ok, Pid} = ?S2S:get_connection(?HOST, <<"127.0.0.1:7">>),
        Ref = monitor(process, Pid),
        receive {'DOWN', Ref, _, _, normal} -> ok after 5000 -> error(not_stopped) end,
        ?assertEqual([], mnesia:dirty_read(matrix_s2s, <<"127.0.0.1:7">>)),
        %% one with an EDU queued is used: it stays while the EDU waits
        {ok, Pid2} = ?S2S:get_connection(?HOST, <<"127.0.0.1:8">>),
        ok = ?S2S:queue_edu(?HOST, <<"127.0.0.1:8">>, #{<<"edu_type">> => <<"m.typing">>, <<"content">> => #{}}),
        timer:sleep(1000),
        ?assert(is_process_alive(Pid2)),
        exit(Pid2, kill)
    after
        persistent_term:erase({?S2S, limit, idle_ms})
    end.

%%--------------------------------------------------------------------
%% Against a hostile server, more
%%--------------------------------------------------------------------

limit(Name, Value, F) ->
    persistent_term:put({?S2S, limit, Name}, Value),
    try F()
    after persistent_term:erase({?S2S, limit, Name})
    end.

%% get_connection/2 is the gate: a server name, within the new
%% connections a minute and the live ones.
s1_gate() ->
    ?assertEqual({error, refused}, ?S2S:get_connection(?HOST, <<"bad name">>)),
    ?assertEqual({error, refused}, ?S2S:get_connection(?HOST, <<"x/y">>)),
    Rows = s2s_rows(),
    ets:delete(matrix_gw_rate, new_connections),
    limit(new_connections, 1,
          fun() ->
                  {ok, P1} = ?S2S:get_connection(?HOST, <<"127.0.0.1:21">>),
                  ?assertEqual({error, refused}, ?S2S:get_connection(?HOST, <<"127.0.0.1:22">>)),
                  %% one there already is no new one
                  ?assertEqual({ok, P1}, ?S2S:get_connection(?HOST, <<"127.0.0.1:21">>)),
                  gen_statem:stop(P1)
          end),
    ets:delete(matrix_gw_rate, new_connections),
    timer:sleep(100),
    ?assertEqual(Rows, s2s_rows()),
    limit(max_connections, length(Rows),
          fun() -> ?assertEqual({error, refused}, ?S2S:get_connection(?HOST, <<"127.0.0.1:23">>)) end),
    ?assertEqual(Rows, s2s_rows()).

send(Server, PDUs) ->
    send(Server, PDUs, []).

send(Server, PDUs, EDUs) ->
    Txn = integer_to_binary(erlang:unique_integer([positive])),
    call(Server, 'PUT', [<<"federation">>, <<"v1">>, <<"send">>, Txn], [],
         #{<<"origin">> => name(Server), <<"origin_server_ts">> => erlang:system_time(millisecond),
           <<"pdus">> => PDUs, <<"edus">> => EDUs}).

%% An authenticated /send whose PDUs name senders of 20 servers that
%% are neither the origin nor in the room makes no connection to them.
s1_sender_domains() ->
    {Far, _} = servers(),
    Rows = s2s_rows(),
    Children = children(),
    PDUs = [element(2, event(Far, #{<<"type">> => <<"m.room.message">>,
                                    <<"sender">> => <<"@x:n", (integer_to_binary(N))/binary, ".invalid">>,
                                    <<"room_id">> => ?ROOM,
                                    <<"content">> => #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"x">>},
                                    <<"origin_server_ts">> => 1700000002000 + N, <<"depth">> => 7,
                                    <<"prev_events">> => [id(bob)],
                                    <<"auth_events">> => [id(create), id(pl), id(bob)]}))
            || N <- lists:seq(1, 20)],
    {200, #{<<"pdus">> := Res}} = send(Far, PDUs),
    ?assertEqual(20, maps:size(Res)),
    ?assert(lists:all(fun(#{<<"error">> := _}) -> true; (_) -> false end, maps:values(Res))),
    timer:sleep(200),
    ?assertEqual(Rows, s2s_rows()),
    ?assertEqual(Children, children()),
    %% bob's own server is the origin: still taken
    {ID, JSON} = message(Far, 20, <<"still here">>),
    ?assertMatch({200, #{<<"pdus">> := #{ID := #{}}}}, send(Far, [JSON])).

%% A presence and a groupchat message from another XMPP server to
%% #r%n1.invalid@G: no lookup of the alias, no connection.
s1_alias() ->
    Rows = s2s_rows(),
    From = jid:make(<<"x">>, <<"other.example">>, <<"r">>),
    To = jid:make(<<"#r%n1.invalid">>, <<"matrix.example.com">>, <<"nick">>),
    mod_matrix_gw:route(#presence{from = From, to = To}),
    mod_matrix_gw:route(#message{from = From, to = jid:remove_resource(To), type = groupchat,
                                 body = [#text{data = <<"hi">>}]}),
    mod_matrix_gw:route(#iq{type = get, id = <<"i">>, from = From, to = jid:remove_resource(To),
                            sub_els = [#disco_info{}]}),
    timer:sleep(300),
    ?assertEqual(Rows, s2s_rows()),
    ?assertNot(lists:member(<<"n1.invalid">>, s2s_rows())).

%% prev_events not a list, or of 10,000 ids: a per-PDU error, the
%% room still there.
s2_prev_events() ->
    {Far, _} = servers(),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, ?ROOM),
    Bad = fun(Prev, N) ->
                  element(2, event(Far, #{<<"type">> => <<"m.room.message">>, <<"sender">> => ?BOB,
                                          <<"room_id">> => ?ROOM,
                                          <<"content">> => #{<<"body">> => <<"x">>},
                                          <<"origin_server_ts">> => 1700000003000 + N, <<"depth">> => 7,
                                          <<"prev_events">> => Prev,
                                          <<"auth_events">> => [id(create), id(pl), id(bob)]}))
          end,
    Many = [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 10000)],
    [?assertMatch({error, _}, mod_matrix_gw_room:process_pdu(?HOST, <<"far.test">>, PDU))
     || PDU <- [Bad(<<"x">>, 1), Bad(Many, 2), Bad([1, 2], 3),
                (Bad([id(bob)], 4))#{<<"content">> => <<"no">>},
                (Bad([id(bob)], 5))#{<<"state_key">> => 5},
                (Bad([id(bob)], 6))#{<<"sender">> => 7},
                (Bad([id(bob)], 7))#{<<"origin_server_ts">> => <<"now">>},
                maps:remove(<<"room_id">>, Bad([id(bob)], 8)),
                (Bad([id(bob)], 9))#{<<"room_id">> => 9},
                #{}]],
    {200, #{<<"pdus">> := _}} = send(Far, [Bad(<<"x">>, 10)]),
    ?assert(is_process_alive(Pid)),
    ?assertMatch(#room_version{}, gen_statem:call(Pid, get_room_version)),
    %% the calls a caller hands rubbish to answer and live
    ?assertEqual({[], []}, gen_statem:call(Pid, {partition_missed_events, <<"x">>})),
    ?assertEqual({[], []}, gen_statem:call(Pid, {partition_events_with_statemap, 7})),
    ?assert(is_process_alive(Pid)).

%% A room of its own: ann's, @x of far.test invited, joined, then banned.
-define(S3ROOM, <<"!s3room:example.com">>).
-define(X, <<"@x:far.test">>).

s3_room() ->
    case mnesia:dirty_read(matrix_room, ?S3ROOM) of
        [_] ->
            ok;
        [] ->
            {Far, _} = servers(),
            Own = own(),
            Steps = [{s3create, Own, <<"m.room.create">>, <<>>, ?ANN, #{<<"creator">> => ?ANN, <<"room_version">> => <<"9">>}},
                     {s3ann, Own, <<"m.room.member">>, ?ANN, ?ANN, #{<<"membership">> => <<"join">>}},
                     {s3pl, Own, <<"m.room.power_levels">>, <<>>, ?ANN, #{<<"users">> => #{?ANN => 100}}},
                     {s3jr, Own, <<"m.room.join_rules">>, <<>>, ?ANN, #{<<"join_rule">> => <<"invite">>}},
                     {s3inv, Own, <<"m.room.member">>, ?X, ?ANN, #{<<"membership">> => <<"invite">>}},
                     {s3xjoin, Far, <<"m.room.member">>, ?X, ?X, #{<<"membership">> => <<"join">>}},
                     {s3ban, Own, <<"m.room.member">>, ?X, ?ANN, #{<<"membership">> => <<"ban">>}}],
            {_, _, _, Events} =
                lists:foldl(
                  fun({Tag, Server, Type, Key, Sender, Content}, {Prev, State, N, Acc}) ->
                          {ID, JSON} = event(Server, #{<<"type">> => Type, <<"state_key">> => Key,
                                                       <<"content">> => Content, <<"sender">> => Sender,
                                                       <<"room_id">> => ?S3ROOM,
                                                       <<"origin_server_ts">> => 1700000000000 + N,
                                                       <<"depth">> => N + 1, <<"prev_events">> => Prev,
                                                       <<"auth_events">> => auth_of(Type, Key, Sender, State)}),
                          State2 = State#{{Type, Key} => ID},
                          persistent_term:put({?MODULE, id, Tag}, ID),
                          {[ID], State2, N + 1, [{ID, JSON, State2} | Acc]}
                  end, {[], #{}, 0, []}, Steps),
            [{Last, _, _} | _] = Events,
            ok = mod_matrix_gw_store:save_meta(?S3ROOM, <<"9">>, multi, [Last]),
            ok = mod_matrix_gw_store:save_events(?S3ROOM, Events, [Last]),
            {ok, _} = mod_matrix_gw_room:get_room_pid(?HOST, ?S3ROOM),
            ok
    end.

%% The auth events the spec selects for an event, from the state so far.
auth_of(Type, Key, Sender, State) ->
    Keys = [{<<"m.room.create">>, <<>>}, {<<"m.room.power_levels">>, <<>>},
            {<<"m.room.member">>, Sender}] ++
        case Type of
            <<"m.room.member">> -> [{<<"m.room.member">>, Key}, {<<"m.room.join_rules">>, <<>>}];
            _ -> []
        end,
    lists:usort([ID || K <- Keys, {ok, ID} <- [maps:find(K, State)]]).

x_event(Fields, N) ->
    {Far, _} = servers(),
    event(Far, maps:merge(#{<<"sender">> => ?X, <<"room_id">> => ?S3ROOM,
                            <<"origin_server_ts">> => 1700000005000 + N, <<"depth">> => 8},
                          Fields)).

latest(Pid) -> gen_statem:call(Pid, get_latest_events).

%% Events a missing-events answer brings: allowed by the state before
%% them, soft-failed against the current one.
s3_state_before() ->
    s3_room(),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, ?S3ROOM),
    V = version(),
    Old = [id(s3create), id(s3pl), id(s3xjoin)],
    %% E1: x's message from before the ban (prev = x's join): allowed then,
    %% soft-failed now: kept, not shown, no extremity
    {E1, J1} = x_event(#{<<"type">> => <<"m.room.message">>, <<"content">> => #{<<"body">> => <<"old">>},
                         <<"prev_events">> => [id(s3xjoin)], <<"auth_events">> => Old, <<"depth">> => 7}, 1),
    %% E2: x's message after the ban naming x's old join as its auth: the
    %% state before it has the ban: refused
    {E2, J2} = x_event(#{<<"type">> => <<"m.room.message">>, <<"content">> => #{<<"body">> => <<"new">>},
                         <<"prev_events">> => [id(s3ban)], <<"auth_events">> => Old}, 2),
    %% E3: x joining again the same way
    {E3, J3} = x_event(#{<<"type">> => <<"m.room.member">>, <<"state_key">> => ?X,
                         <<"content">> => #{<<"membership">> => <<"join">>},
                         <<"prev_events">> => [id(s3ban)], <<"auth_events">> => Old ++ [id(s3jr)]}, 3),
    ok = mod_matrix_gw_room:process_missing_events(?HOST, <<"far.test">>, Pid, ?S3ROOM, V, [J1, J2, J3]),
    ?assertMatch({ok, _}, gen_statem:call(Pid, {find_event, E1})),
    ?assertEqual(error, gen_statem:call(Pid, {find_event, E2})),
    ?assertEqual(error, gen_statem:call(Pid, {find_event, E3})),
    L = latest(Pid),
    [?assertNot(maps:is_key(E, L)) || E <- [E1, E2, E3]],
    ?assert(maps:is_key(id(s3ban), L)),
    ?assertEqual({?X, <<"ban">>}, lists:keyfind(?X, 1, mod_matrix_gw_room:members(?S3ROOM))),
    %% the same through /send: refused or soft-failed, the ban holds
    {E4, J4} = x_event(#{<<"type">> => <<"m.room.member">>, <<"state_key">> => ?X,
                         <<"content">> => #{<<"membership">> => <<"join">>},
                         <<"prev_events">> => [id(s3ban)], <<"auth_events">> => Old ++ [id(s3jr)]}, 4),
    {Far, _} = servers(),
    {200, _} = send(Far, [J4]),
    ?assertNot(maps:is_key(E4, latest(Pid))),
    ?assertEqual({?X, <<"ban">>}, lists:keyfind(?X, 1, mod_matrix_gw_room:members(?S3ROOM))).

%% A backfilled event is history only from a sender the room knows
%% and has not banned.
s3_history() ->
    s3_room(),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, ?S3ROOM),
    {_, Data} = sys:get_state(Pid),
    V = version(),
    E = fun(Sender) ->
                mod_matrix_gw_room:json_to_event(
                  #{<<"type">> => <<"m.room.message">>, <<"sender">> => Sender, <<"room_id">> => ?S3ROOM,
                    <<"content">> => #{}, <<"origin_server_ts">> => 1, <<"depth">> => 1,
                    <<"prev_events">> => [], <<"auth_events">> => []}, V)
        end,
    ?assert(mod_matrix_gw_room:history_sender(E(?ANN), Data)),
    ?assertNot(mod_matrix_gw_room:history_sender(E(?X), Data)),
    ?assertNot(mod_matrix_gw_room:history_sender(E(<<"@stranger:far.test">>), Data)).

%% A server streaming without end, one announcing a body too large,
%% one sending a large error body: the answer stops at the bound.
s4_bounded() ->
    {ok, _} = application:ensure_all_started(inets),
    ?assertEqual(64 * 1024 * 1024, mod_matrix_gw_http:max_body([<<"_matrix">>, <<"federation">>, <<"v2">>, <<"send_join">>, <<"r">>, <<"e">>])),
    ?assertEqual(16 * 1024 * 1024, mod_matrix_gw_http:max_body([<<"_matrix">>, <<"federation">>, <<"v1">>, <<"get_missing_events">>, <<"r">>])),
    ?assertEqual(1024 * 1024, mod_matrix_gw_http:max_body([<<"_matrix">>, <<"federation">>, <<"v1">>, <<"event">>, <<"e">>])),
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    Server = spawn_link(fun() -> serve(LSock) end),
    URL = fun(P) -> "http://127.0.0.1:" ++ integer_to_list(Port) ++ P end,
    Get = fun(P, Max) ->
                  mod_matrix_gw_http:request(get, {URL(P), []}, [{timeout, 5000}],
                                             [{sync, true}, {body_format, binary}], Max)
          end,
    T0 = erlang:monotonic_time(millisecond),
    ?assertEqual({error, too_large}, Get("/endless", 100000)),
    ?assertEqual({error, too_large}, Get("/chunked", 100000)),
    ?assertEqual({error, too_large}, Get("/announced", 100000)),
    ?assertEqual({error, too_large}, Get("/error", 100000)),
    ?assert(erlang:monotonic_time(millisecond) - T0 < 4000),
    ?assertMatch({ok, {{_, 200, _}, _, <<"small">>}}, Get("/small", 100000)),
    ?assertMatch({ok, {{_, 404, _}, _, <<"nope">>}}, Get("/missing", 100000)),
    %% a trickle: the timeout is the whole answer's
    ?assertEqual({error, timeout},
                 mod_matrix_gw_http:request(get, {URL("/trickle"), []}, [{timeout, 1000}],
                                            [{sync, true}, {body_format, binary}], 100000)),
    %% the receiver form
    Self = self(),
    {ok, Ref} = mod_matrix_gw_http:request(get, {URL("/endless"), []}, [{timeout, 5000}],
                                           [{sync, false}, {receiver, fun(R) -> Self ! {got, R} end}], 100000),
    receive {got, {Ref, R}} -> ?assertEqual({error, too_large}, R) after 5000 -> error(no_answer) end,
    {ok, Ref2} = mod_matrix_gw_http:request(get, {URL("/small"), []}, [{timeout, 5000}],
                                            [{sync, false}, {receiver, fun(R2) -> Self ! {got, R2} end}], 100000),
    receive {got, {Ref2, R2}} -> ?assertMatch({{_, 200, _}, _, <<"small">>}, R2) after 5000 -> error(no_answer) end,
    unlink(Server),
    exit(Server, kill),
    gen_tcp:close(LSock).

serve(LSock) ->
    {ok, S} = gen_tcp:accept(LSock),
    spawn(fun() -> answer(S) end),
    serve(LSock).

answer(S) ->
    {ok, Req} = gen_tcp:recv(S, 0, 5000),
    [_, Path | _] = binary:split(Req, [<<" ">>], [global]),
    Chunk = binary:copy(<<"x">>, 16384),
    Loop = fun L(F) -> case gen_tcp:send(S, F()) of ok -> L(F); _ -> ok end end,
    case Path of
        <<"/endless">> ->
            gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n">>),
            Loop(fun() -> Chunk end);
        <<"/chunked">> ->
            gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n">>),
            Loop(fun() -> <<"4000\r\n", Chunk/binary, "\r\n">> end);
        <<"/announced">> ->
            gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nContent-Length: 999999999\r\n\r\n">>),
            Loop(fun() -> Chunk end);
        <<"/error">> ->
            gen_tcp:send(S, <<"HTTP/1.1 500 Oops\r\nContent-Length: 999999999\r\n\r\n">>),
            Loop(fun() -> Chunk end);
        <<"/small">> ->
            gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nsmall">>);
        <<"/trickle">> ->
            gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nContent-Length: 1000\r\n\r\n">>),
            Loop(fun() -> timer:sleep(200), <<"x">> end);
        _ ->
            gen_tcp:send(S, <<"HTTP/1.1 404 Not Found\r\nContent-Length: 4\r\n\r\nnope">>)
    end,
    gen_tcp:close(S).

%% The claims of one of our devices a minute whatever the origin, the
%% gateway's; invites a minute per local user; e2e EDUs in all.
s7_global() ->
    {Far, Other} = servers(),
    Claim = fun(S, Devs) ->
                    call(S, 'POST', [<<"federation">>, <<"v1">>, <<"user">>, <<"keys">>, <<"claim">>], [],
                         #{<<"one_time_keys">> => #{?ANN => maps:from_list([{D, <<"signed_curve25519">>}
                                                                             || D <- Devs])}})
            end,
    [ets:delete(matrix_gw_rate, K) || K <- [{claims, name(Far)}, {claims, name(Other)}, claims_all,
                                            {claims_device, {?ANN, <<"S7">>}}]],
    limit(claims_device, 2,
          fun() ->
                  ?assertMatch({200, _}, Claim(Far, [<<"S7">>])),
                  ?assertMatch({200, _}, Claim(Other, [<<"S7">>])),
                  ?assertMatch({429, #{<<"errcode">> := <<"M_LIMIT_EXCEEDED">>}}, Claim(Far, [<<"S7">>])),
                  ?assertMatch({429, _}, Claim(Other, [<<"S7">>])),
                  %% another device is another budget
                  ?assertMatch({200, _}, Claim(Other, [<<"S7b">>]))
          end),
    limit(claims_all, 1,
          fun() ->
                  ets:delete(matrix_gw_rate, claims_all),
                  ?assertMatch({200, _}, Claim(Other, [<<"S7c">>])),
                  ?assertMatch({429, _}, Claim(Far, [<<"S7d">>]))
          end),
    ?assert(mod_matrix_gw:claims_allowed(?HOST, #{<<"one_time_keys">> => #{<<"@a:elsewhere">> => #{<<"D">> => <<"x">>}}})),
    %% invites: ann's a minute, whatever the origin
    Inv = #{<<"membership">> => <<"invite">>},
    [ets:delete(matrix_gw_rate, K) || K <- [{invite, name(Far)}, {invite, name(Other)}, {invite_user, <<"ann">>}]],
    limit(invites_user, 2,
          fun() ->
                  ?assertMatch({200, _}, invite(Far, <<"!s7a:far.test">>, <<"@m:far.test">>, ?ANN, Inv)),
                  ?assertMatch({200, _}, invite(Other, <<"!s7b:other.test">>, <<"@m:other.test">>, ?ANN, Inv)),
                  ?assertMatch({429, _}, invite(Other, <<"!s7c:other.test">>, <<"@m:other.test">>, ?ANN, Inv)),
                  %% cid's are cid's
                  ?assertMatch({200, _}, invite(Other, <<"!s7d:other.test">>, <<"@m:other.test">>,
                                                <<"@cid:example.com">>, Inv))
          end),
    %% end-to-end EDUs in all
    ets:delete(matrix_gw_rate, e2e_edus_all),
    limit(e2e_edus_all, 2,
          fun() ->
                  ?assert(mod_matrix_gw:e2e_edu_allowed(<<"a.test">>, <<"m.device_list_update">>)),
                  ?assert(mod_matrix_gw:e2e_edu_allowed(<<"b.test">>, <<"m.device_list_update">>)),
                  ?assertNot(mod_matrix_gw:e2e_edu_allowed(<<"c.test">>, <<"m.device_list_update">>))
          end).

%% A missing event naming another room: not stored, no extremity.
s8_room_id() ->
    {Far, _} = servers(),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, ?ROOM),
    {ID, JSON} = event(Far, #{<<"type">> => <<"m.room.message">>, <<"sender">> => ?BOB,
                              <<"room_id">> => <<"!elsewhere:far.test">>,
                              <<"content">> => #{<<"body">> => <<"x">>},
                              <<"origin_server_ts">> => 1700000006000, <<"depth">> => 7,
                              <<"prev_events">> => [id(bob)],
                              <<"auth_events">> => [id(create), id(pl), id(bob)]}),
    ok = mod_matrix_gw_room:process_missing_events(?HOST, <<"far.test">>, Pid, ?ROOM, version(), [JSON, <<"junk">>]),
    ?assertEqual(error, gen_statem:call(Pid, {find_event, ID})),
    ?assertNot(maps:is_key(ID, latest(Pid))).

%% An invite whose invite_room_state names an alias of another server
%% is for the room of the signed event: !<hex>%%<origin>@G, the alias only
%% its reason.
s9_invite_jid() ->
    {Far, _} = servers(),
    Res = <<"s9">>,
    ejabberd_sm:open_session({erlang:timestamp(), self()}, <<"ann">>, ?HOST, Res, 0, []),
    try
        RoomID = <<"!s9room">>,
        {ID, JSON} = event(Far, #{<<"type">> => <<"m.room.member">>, <<"state_key">> => ?ANN,
                                  <<"sender">> => <<"@m:far.test">>, <<"room_id">> => RoomID,
                                  <<"content">> => #{<<"membership">> => <<"invite">>},
                                  <<"origin_server_ts">> => erlang:system_time(millisecond),
                                  <<"depth">> => 5, <<"prev_events">> => [], <<"auth_events">> => []}),
        ets:delete(matrix_gw_rate, {invite, name(Far)}),
        ets:delete(matrix_gw_rate, {invite_user, <<"ann">>}),
        IRS = [#{<<"type">> => <<"m.room.canonical_alias">>, <<"state_key">> => <<>>,
                 <<"sender">> => <<"@m:far.test">>, <<"content">> => #{<<"alias">> => <<"#lure:other.test">>}},
               #{<<"type">> => <<"m.room.name">>, <<"state_key">> => <<>>,
                 <<"sender">> => <<"@m:far.test">>, <<"content">> => #{<<"name">> => <<"Lure">>}}],
        ?assertMatch({200, _}, call(Far, 'PUT', [<<"federation">>, <<"v2">>, <<"invite">>, RoomID, ID], [],
                                    #{<<"event">> => JSON, <<"room_version">> => <<"9">>,
                                      <<"invite_room_state">> => IRS})),
        {ok, Esc} = {ok, <<"!", (integer_to_binary(binary:decode_unsigned(<<"s9room">>), 16))/binary,
                           "%%far.test">>},
        receive
            {route, #message{from = From} = Msg} ->
                ?assertEqual(jid:make(Esc, <<"matrix.example.com">>), From),
                #muc_user{invites = [#muc_invite{reason = Reason}]} = xmpp:get_subtag(Msg, #muc_user{}),
                ?assertEqual(<<"Lure #lure:other.test">>, Reason)
        after 3000 ->
                error(no_invite)
        end
    after
        ejabberd_sm:close_session(ejabberd_sm:get_session_sid(<<"ann">>, ?HOST, Res), <<"ann">>, ?HOST, Res)
    end.
%%--------------------------------------------------------------------
%% Room version 12 (MSC4289 creators, MSC4291 room id = the create
%% event's hash, no create event in auth_events), and the state a missing
%% event's /state brings
%%--------------------------------------------------------------------

-define(M, <<"@m:far.test">>).
%% mod_matrix_gw_room's own record (kept in step with it)
-record(event, {id, room_version, room_id, type, state_key, depth, auth_events, sender,
                prev_events, origin_server_ts, json, state_map}).
-define(N, <<"@n:far.test">>).

%% A room of far.test's @m in version Ver: create, @m's join, power levels
%% (Users), public join rules, encryption, ann's join; Extra into the create
%% event's content. [{Tag, Event}] in order and the room id.
vroom(Ver, Users, Extra) ->
    V = mod_matrix_gw_room:binary_to_room_version(Ver),
    Hydra = V#room_version.hydra,
    Create0 = #{<<"type">> => <<"m.room.create">>, <<"state_key">> => <<>>, <<"sender">> => ?M,
                <<"content">> => maps:merge(#{<<"room_version">> => Ver}, Extra),
                <<"origin_server_ts">> => 1700000000000, <<"depth">> => 1,
                <<"prev_events">> => [], <<"auth_events">> => []},
    {RoomID, Create} =
        case Hydra of
            true ->
                <<$$, H/binary>> = mod_matrix_gw:get_event_id(Create0, V),
                {<<$!, H/binary>>, Create0};
            false ->
                R = <<"!v", Ver/binary, "room:far.test">>,
                {R, Create0#{<<"room_id">> => R,
                             <<"content">> => (maps:get(<<"content">>, Create0))#{<<"creator">> => ?M}}}
        end,
    CreateID = mod_matrix_gw:get_event_id(Create, V),
    Steps = [{join, <<"m.room.member">>, ?M, ?M, #{<<"membership">> => <<"join">>}},
             {pl, <<"m.room.power_levels">>, <<>>, ?M, #{<<"users">> => Users, <<"ban">> => 50}},
             {jr, <<"m.room.join_rules">>, <<>>, ?M, #{<<"join_rule">> => <<"public">>}},
             {enc, <<"m.room.encryption">>, <<>>, ?M, #{<<"algorithm">> => <<"m.megolm.v1.aes-sha2">>}},
             {ann, <<"m.room.member">>, ?ANN, ?ANN, #{<<"membership">> => <<"join">>}}],
    State0 = #{{<<"m.room.create">>, <<>>} => CreateID},
    {_, _, Evs} =
        lists:foldl(
          fun({Tag, Type, Key, Sender, Content}, {Prev, State, Acc}) ->
                  Keys = [{<<"m.room.power_levels">>, <<>>}, {<<"m.room.member">>, Sender}] ++
                      [{<<"m.room.create">>, <<>>} || not Hydra] ++
                      case Type of
                          <<"m.room.member">> -> [{<<"m.room.join_rules">>, <<>>}];
                          _ -> []
                      end,
                  Auth = lists:usort([ID || K <- Keys, {ok, ID} <- [maps:find(K, State)]]),
                  JSON = #{<<"type">> => Type, <<"state_key">> => Key, <<"sender">> => Sender,
                           <<"content">> => Content, <<"room_id">> => RoomID,
                           <<"origin_server_ts">> => 1700000000000 + length(Acc) + 1,
                           <<"depth">> => length(Acc) + 2, <<"prev_events">> => [Prev],
                           <<"auth_events">> => Auth},
                  ID = mod_matrix_gw:get_event_id(JSON, V),
                  {ID, State#{{Type, Key} => ID}, Acc ++ [{Tag, {ID, JSON, State#{{Type, Key} => ID}}}]}
          end, {CreateID, State0, []}, Steps),
    {RoomID, V, [{create, {CreateID, Create, State0}} | Evs]}.

ev(Tag, Evs, V) ->
    {_, JSON, _} = proplists:get_value(Tag, Evs),
    mod_matrix_gw_room:json_to_event(JSON, V).

%% The auth rules on their own, against a state map of events.
v12_rules() ->
    {_RoomID, V, Evs} = vroom(<<"12">>, #{}, #{<<"additional_creators">> => [?N]}),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, ?ROOM),
    {_, Data0} = sys:get_state(Pid),
    Data = setelement(6, Data0, V),   % #data.room_version
    ?assertEqual(V, element(6, Data)),
    Create = ev(create, Evs, V),
    Join = ev(join, Evs, V),
    PL = ev(pl, Evs, V),
    CreateID = Create#event.id,
    Auth = fun(E, State) -> mod_matrix_gw_room:check_event_auth(E, maps:from_list([{{S#event.type, S#event.state_key}, S} || S <- State]), Data) end,
    %% the create event: no room_id, additional_creators user ids
    ?assert(Auth(Create, [])),
    {_, CJSON, _} = proplists:get_value(create, Evs),
    ?assertThrow(room_id_in_create, mod_matrix_gw_room:json_to_event(CJSON#{<<"room_id">> => <<"!x">>}, V)),
    Bad = mod_matrix_gw_room:json_to_event(CJSON#{<<"content">> => #{<<"room_version">> => <<"12">>, <<"additional_creators">> => <<"@n:far.test">>}}, V),
    ?assertNot(Auth(Bad, [])),
    Bad2 = mod_matrix_gw_room:json_to_event(CJSON#{<<"content">> => #{<<"room_version">> => <<"12">>, <<"additional_creators">> => [<<"not a user">>]}}, V),
    ?assertNot(Auth(Bad2, [])),
    %% the create event named in auth_events: refused as it is read
    {_, JJSON, _} = proplists:get_value(join, Evs),
    ?assertThrow(create_in_auth_events,
                 mod_matrix_gw_room:json_to_event(JJSON#{<<"auth_events">> => [CreateID]}, V)),
    %% the creator's join: auth_events empty (the create event put back),
    %% the only previous event the create event
    ?assertEqual([CreateID], Join#event.auth_events),
    ?assert(Auth(Join, [Create])),
    %% not after another event, not by another user
    ?assertNot(Auth(Join#event{prev_events = [PL#event.id]}, [Create])),
    ?assertNot(Auth(Join#event{sender = ?N, state_key = ?N}, [Create])),
    %% power levels: the creators at an infinite level, never in users
    ?assert(Auth(PL, [Create, Join])),
    {_, PJSON, _} = proplists:get_value(pl, Evs),
    WithCreator = mod_matrix_gw_room:json_to_event(PJSON#{<<"content">> => #{<<"users">> => #{?M => 100}}}, V),
    ?assertNot(Auth(WithCreator, [Create, Join])),
    WithAdditional = mod_matrix_gw_room:json_to_event(PJSON#{<<"content">> => #{<<"users">> => #{?N => 100}}}, V),
    ?assertNot(Auth(WithAdditional, [Create, Join])),
    %% an additional creator outranks any level: raises ban past 100
    NJoin = Join#event{id = <<"$njoin">>, sender = ?N, state_key = ?N,
                       json = (Join#event.json)#{<<"sender">> => ?N, <<"state_key">> => ?N}},
    Raise = fun(Sender) ->
                    E = mod_matrix_gw_room:json_to_event(
                          PJSON#{<<"sender">> => Sender,
                                 <<"content">> => #{<<"users">> => #{}, <<"ban">> => 1000}}, V),
                    E
            end,
    ?assert(Auth(Raise(?N), [Create, Join, PL, NJoin])),
    %% a member at level 0 may not
    XJoin = NJoin#event{id = <<"$xjoin">>, sender = ?X, state_key = ?X,
                        json = (Join#event.json)#{<<"sender">> => ?X, <<"state_key">> => ?X}},
    ?assertNot(Auth(Raise(?X), [Create, Join, PL, XJoin])),
    %% an event of a member without power levels needs the create event
    ?assertNot(Auth(PL, [Join])),
    %% version 11 keeps its rules: the create event in auth_events, the
    %% creator's join by its count
    {_, V11, Evs11} = vroom(<<"11">>, #{?M => 100}, #{}),
    D11 = setelement(6, Data0, V11),
    C11 = ev(create, Evs11, V11),
    J11 = ev(join, Evs11, V11),
    ?assertEqual([C11#event.id], J11#event.auth_events),
    ?assert(mod_matrix_gw_room:check_event_auth(J11, #{{<<"m.room.create">>, <<>>} => C11}, D11)).

%% A room the gateway was brought into by its join: every earlier event
%% known from send_join without a state map, ann's join with the state.
vroom_stored(Ver) ->
    {RoomID, V, Evs} = vroom(Ver, case Ver of <<"12">> -> #{}; _ -> #{?M => 100} end, #{}),
    Stored = [case Tag of
                  ann -> {ID, JSON, State};
                  _ -> {ID, JSON, undefined}
              end || {Tag, {ID, JSON, State}} <- Evs],
    {AnnID, _, _} = proplists:get_value(ann, Evs),
    ok = mod_matrix_gw_store:save_meta(RoomID, Ver, {direct, {<<"ann">>, ?HOST}, ?M, established}, [AnnID]),
    ok = mod_matrix_gw_store:save_events(RoomID, Stored, [AnnID]),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
    {RoomID, V, Evs, Pid}.

%% What fetch_prev_statemaps/6 does for a missing event's prev event: the
%% prev event with the state /state answered (the state before it, the
%% event itself left out as Synapse serves it), then the event through
%% the room as a missing event is.
state_before(Ver) ->
    {RoomID, V, Evs, Pid} = vroom_stored(Ver),
    Create = ev(create, Evs, V),
    Join = ev(join, Evs, V),
    PL = ev(pl, Evs, V),
    ok = gen_statem:call(Pid, {auth_and_store_external_events, [Create#event{state_map = #{}}]}),
    {ok, #event{state_map = CSM}} = gen_statem:call(Pid, {find_event, Create#event.id}),
    ?assertEqual(#{{<<"m.room.create">>, <<>>} => Create#event.id}, CSM),
    %% the creator's join, the first missing event: allowed by the state
    %% before it (the create event's)
    ?assertEqual(ok, gen_statem:call(Pid, {resolve_auth_store_event, Join})),
    %% the power levels after it, its prev event come by /state too
    ok = gen_statem:call(Pid, {auth_and_store_external_events,
                               [Join#event{state_map = #{{<<"m.room.create">>, <<>>} => Create#event.id}}]}),
    {ok, #event{state_map = JSM}} = gen_statem:call(Pid, {find_event, Join#event.id}),
    ?assertEqual(Join#event.id, maps:get({<<"m.room.member">>, ?M}, JSM)),
    ?assertEqual(ok, gen_statem:call(Pid, {resolve_auth_store_event, PL})),
    ?assertMatch({ok, #event{state_map = #{{<<"m.room.power_levels">>, <<>>} := _}}},
                 gen_statem:call(Pid, {find_event, PL#event.id})),
    %% the room as ann's device reads it: encrypted, both joined
    ?assertEqual([{?ANN, <<"join">>}, {?M, <<"join">>}], mod_matrix_gw_room:members(RoomID)),
    {AnnID, _, _} = proplists:get_value(ann, Evs),
    ?assertEqual([AnnID], maps:keys(latest(Pid))).

v12_state_before() -> state_before(<<"12">>).
v11_state_before() -> state_before(<<"11">>).

%% the stand-in server fails Fails transactions, then takes
%% them; every body it is offered goes to the test process.
edu_sender(Fails) ->
    Self = self(),
    N = atomics:new(1, []),
    persistent_term:put({?S2S, edu_sender},
                        fun(Server, Body) ->
                                I = atomics:add_get(N, 1, 1),
                                Self ! {edu_body, Server, I, Body},
                                case I =< Fails of
                                    true -> {error, timeout};
                                    false -> {{"HTTP/1.1", 200, "OK"}, [], <<"{}">>}
                                end
                        end).

edu_bodies(Acc) ->
    receive {edu_body, _, I, Body} -> edu_bodies([{I, Body} | Acc])
    after 0 -> lists:reverse(Acc)
    end.

wait_edu(I) ->
    receive {edu_body, _, I, Body} -> Body after 10000 -> error({no_try, I}) end.

types(#{<<"edus">> := EDUs}) -> [T || #{<<"edu_type">> := T} <- EDUs].

l1_edus_held() ->
    Server = <<"l1.test">>,
    persistent_term:put({?S2S, limit, edu_retry_ms}, 20),
    persistent_term:put({?S2S, limit, edu_backoff_min}, 100),
    ToDevice = #{<<"edu_type">> => <<"m.direct_to_device">>,
                 <<"content">> => #{<<"sender">> => ?ANN, <<"type">> => <<"m.room.encrypted">>,
                                    <<"message_id">> => <<"k1">>, <<"messages">> => #{}}},
    Typing = #{<<"edu_type">> => <<"m.typing">>, <<"content">> => #{}},
    try
        %% five tries fail, the held one fails once more, then it goes
        edu_sender(6),
        {ok, Conn} = ?S2S:get_connection(?HOST, Server),
        sys:suspend(Conn),
        ok = ?S2S:queue_edu(?HOST, Server, ToDevice),
        ok = ?S2S:queue_edu(?HOST, Server, Typing),
        sys:resume(Conn),
        ?assertEqual([<<"m.direct_to_device">>], types(wait_edu(1))),
        [wait_edu(I) || I <- [2, 3, 4, 5]],
        %% out of tries: held, the connection kept; the typing behind it,
        %% sent while the server is known down, fails once and is dropped
        ?assertEqual([<<"m.typing">>], types(wait_edu(6))),
        timer:sleep(20),
        ?assertMatch([_], ?S2S:held_edus(Server)),
        ?assert(is_process_alive(Conn)),
        Sent = wait_edu(7),
        ?assertEqual([<<"m.direct_to_device">>], types(Sent)),
        timer:sleep(100),
        ?assertEqual([], ?S2S:held_edus(Server)),
        ?assertEqual([], edu_bodies([])),
        %% held over a restart of the connection: the next one sends it
        edu_sender(1000),
        ok = ?S2S:queue_edu(?HOST, Server, ToDevice),
        [wait_edu(I) || I <- [1, 2, 3, 4, 5]],
        timer:sleep(50),
        ?assertMatch([_], ?S2S:held_edus(Server)),
        [{_, _, Pid}] = mnesia:dirty_read(matrix_s2s, Server),
        Ref = monitor(process, Pid),
        exit(Pid, kill),
        receive {'DOWN', Ref, _, _, _} -> ok end,
        mnesia:dirty_delete(matrix_s2s, Server),
        ?assertMatch([_], ?S2S:held_edus(Server)),
        edu_bodies([]),
        edu_sender(0),
        {ok, _} = ?S2S:get_connection(?HOST, Server),
        ?assertEqual([<<"m.direct_to_device">>], types(wait_edu(1))),
        timer:sleep(100),
        ?assertEqual([], ?S2S:held_edus(Server))
    after
        persistent_term:erase({?S2S, edu_sender}),
        persistent_term:erase({?S2S, limit, edu_retry_ms}),
        persistent_term:erase({?S2S, limit, edu_backoff_min})
    end.

%% /send hands on its to-device EDUs before its PDUs, the
%% rest after them.
edu_order_test() ->
    T = fun(Type) -> #{<<"edu_type">> => Type, <<"content">> => #{}} end,
    ?assertEqual({[T(<<"m.direct_to_device">>), T(<<"m.direct_to_device">>)],
                  [T(<<"m.typing">>), T(<<"m.receipt">>), #{}]},
                 mod_matrix_gw_xmpp:edu_order([T(<<"m.typing">>), T(<<"m.direct_to_device">>),
                                               T(<<"m.receipt">>), #{}, T(<<"m.direct_to_device">>)])),
    ?assertEqual({[], []}, mod_matrix_gw_xmpp:edu_order(<<"x">>)).
