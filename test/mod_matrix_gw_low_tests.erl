%%% The Matrix gateway against a hostile server or user, the smaller
%%% points: the rate table's purge, a /key/v2 answer's signatures, the
%%% dead discovery gone, typing and receipts of members only, the direct
%%% chat's row after its join, the failure log lines, /send_join and
%%% /send_leave of a malformed event. On an ejabberd started with the
%%% gateway, as mod_matrix_gw_fed_tests: requests signed as the suite's own
%%% servers (near.test), two direct rooms of dan@example.com with
%%% @bob:near.test brought back from the store (bob joined in one, only
%%% invited in the other).
-module(mod_matrix_gw_low_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").
-include("mod_matrix_gw.hrl").

%% the log sink (S16)
-export([log/2]).

-define(HOST, <<"example.com">>).
-define(S2S, mod_matrix_gw_s2s).
-define(JOINED, <<"!lowjoined:example.com">>).
-define(INVITED, <<"!lowinvited:example.com">>).
-define(DAN, <<"@dan:example.com">>).
-define(BOB, <<"@bob:near.test">>).
-define(KEYID, <<"ed25519:k">>).

b64(B) -> mod_matrix_gw:base64_encode(B).

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------

%% A /key/v2/server answer of Name signed by the keys in Signers, listing
%% the keys in Listed.
key_answer(Name, Listed, Signers) ->
    JSON = #{<<"server_name">> => Name, <<"valid_until_ts">> => 1 bsl 50,
             <<"verify_keys">> => maps:from_list([{K, #{<<"key">> => b64(Pub)}} || {K, {Pub, _}} <- Listed])},
    lists:foldl(fun({K, {_, Priv}}, Acc) -> sign_as(Name, K, Priv, Acc) end, JSON, Signers).

sign_as(Name, KeyID, Priv, JSON) ->
    Msg = mod_matrix_gw:encode_canonical_json(maps:without([<<"signatures">>, <<"unsigned">>], JSON)),
    Sigs = maps:get(<<"signatures">>, JSON, #{}),
    JSON#{<<"signatures">> => Sigs#{Name => (maps:get(Name, Sigs, #{}))#{KeyID => b64(crypto:sign(eddsa, none, Msg, [Priv, ed25519]))}}}.

s11_key_response_test_() ->
    A = {<<"ed25519:a">>, crypto:generate_key(eddsa, ed25519)},
    B = {<<"ed25519:b">>, crypto:generate_key(eddsa, ed25519)},
    N = {<<"ed25519:n">>, {NPub, NPriv}} = {<<"ed25519:n">>, crypto:generate_key(eddsa, ed25519)},
    Ok = fun(JSON, Via, GetKey) -> ?S2S:key_response_ok(JSON, <<"s.test">>, Via, GetKey) end,
    NoNotary = fun(_) -> error end,
    Notary = fun(<<"ed25519:n">>) -> {ok, NPub, 0}; (_) -> error end,
    Both = key_answer(<<"s.test">>, [A, B], [A, B]),
    OnlyA = key_answer(<<"s.test">>, [A, B], [A]),
    [%% every key listed signed it: taken
     ?_assert(Ok(Both, <<"s.test">>, NoNotary)),
     ?_assert(Ok(key_answer(<<"s.test">>, [A], [A]), undefined, NoNotary)),
     %% a second key listed that did not sign: refused (was taken, S11)
     ?_assertNot(Ok(OnlyA, <<"s.test">>, NoNotary)),
     ?_assertNot(Ok(key_answer(<<"s.test">>, [A, B], [B]), <<"s.test">>, NoNotary)),
     %% signed by another server's name, no keys, a key of another kind
     ?_assertNot(Ok(key_answer(<<"other.test">>, [A], [A]), <<"s.test">>, NoNotary)),
     ?_assertNot(Ok(key_answer(<<"s.test">>, [], []), <<"s.test">>, NoNotary)),
     ?_assertNot(Ok((key_answer(<<"s.test">>, [A], [A]))#{<<"verify_keys">> => #{<<"curve25519:x">> => #{<<"key">> => <<"AAAA">>}}},
                    <<"s.test">>, NoNotary)),
     ?_assertNot(Ok(#{}, <<"s.test">>, NoNotary)),
     %% through a notary: the notary's own signature is needed too
     ?_assertNot(Ok(Both, <<"notary.test">>, Notary)),
     ?_assert(Ok(sign_as(<<"notary.test">>, element(1, N), NPriv, Both), <<"notary.test">>, Notary)),
     ?_assertNot(Ok(sign_as(<<"notary.test">>, element(1, N), NPriv, Both), <<"notary.test">>, NoNotary)),
     ?_assertNot(Ok(sign_as(<<"notary.test">>, element(1, N), NPriv, OnlyA), <<"notary.test">>, Notary))].

s13_dead_discovery_test_() ->
    [?_assertNot(erlang:function_exported(?S2S, get_matrix_host_port, 2)),
     ?_assertNot(lists:keymember(do_get_matrix_host_port, 1, ?S2S:module_info(functions)))].

s16_failure_test_() ->
    F = fun mod_matrix_gw_room:failure/3,
    Secret = #{<<"content">> => #{<<"body">> => <<"secret words">>, <<"ciphertext">> => <<"AwgA">>}},
    ST = [{m, f, [Secret], []}, {m, g, 2, []}],
    Has = fun(T) -> binary:match(iolist_to_binary(io_lib:format("~p", [T])), [<<"secret">>, <<"AwgA">>]) /= nomatch end,
    [?_assertNot(Has(F(error, {badmatch, Secret}, ST))),
     ?_assertNot(Has(F(error, {case_clause, [Secret]}, ST))),
     ?_assertNot(Has(F(throw, Secret, ST))),
     ?_assertNot(Has(F(error, {badmatch, <<"secret words">>}, ST))),
     ?_assertEqual({error, function_clause}, F(error, function_clause, ST)),
     %% the ids beside a reason's tag stay: they say which event
     ?_assertEqual({error, {event_auth_error, <<"$ev">>}}, F(error, {event_auth_error, <<"$ev">>}, ST)),
     ?_assertEqual({error, {badmatch, '_'}}, F(error, {badmatch, Secret}, ST))].

%%--------------------------------------------------------------------
%% On a running gateway
%%--------------------------------------------------------------------

ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup,
     fun() ->
             Dir = mod_matrix_gw_test_lib:start_ejabberd(?MODULE),
             [ok = reg(U) || U <- [<<"dan">>, <<"eli">>]],
             Near = fake_server(<<"near.test">>),
             persistent_term:put({?MODULE, near}, Near),
             room(Near, ?JOINED, true),
             room(Near, ?INVITED, false),
             {Dir, Near}
     end,
     fun({Dir, _}) ->
             persistent_term:erase({?S2S, limit, rate_max_keys}),
             mod_matrix_gw_test_lib:stop_ejabberd(Dir)
     end,
     fun(_) ->
             [{timeout, 60, {Name, F}}
              || {Name, F} <- [{"the rate table drops what is over, never every window", fun s10_rate_purge/0},
                               {"typing and receipts of a joined member only", fun s12_edus_of_members/0},
                               {"no event term in the log of a failed PDU", fun s16_log/0},
                               {"/send_join and /send_leave of a malformed event: M_BAD_JSON", fun s17_bad_json/0},
                               {"a direct join that fails keeps the user's chat", fun s14_direct_row/0}]]
     end}.

reg(U) ->
    case ejabberd_auth:try_register(U, ?HOST, <<"secret">>) of
        ok -> ok;
        {error, exists} -> ok
    end.

near() -> persistent_term:get({?MODULE, near}).

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
    sign_as(Name, ?KEYID, Priv, JSON).

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
        {Code, <<_, _/binary>>, _, <<"{", _/binary>> = B} -> {Code, misc:json_decode(B)};
        {Code, _, <<"{", _/binary>> = B} -> {Code, misc:json_decode(B)};
        {Code, _, B} -> {Code, B}
    end.

version() -> mod_matrix_gw_room:binary_to_room_version(<<"9">>).

event(Server, Fields) ->
    V = version(),
    JSON0 = Fields#{<<"origin">> => name(Server)},
    JSON1 = JSON0#{<<"hashes">> => #{<<"sha256">> => b64(mod_matrix_gw:content_hash(JSON0))}},
    Pruned = maps:without([<<"age_ts">>, <<"unsigned">>], mod_matrix_gw:prune_event(JSON1, V)),
    JSON = JSON1#{<<"signatures">> => maps:get(<<"signatures">>, sign(Server, Pruned))},
    {mod_matrix_gw:get_event_id(JSON, V), JSON}.

%% dan's direct room with bob, bob joined (Joined) or only invited.
room(Near, RoomID, Joined) ->
    Own = {?HOST, own},
    Steps = [{create, Own, <<"m.room.create">>, <<>>, ?DAN, #{<<"creator">> => ?DAN, <<"room_version">> => <<"9">>}},
             {dan, Own, <<"m.room.member">>, ?DAN, ?DAN, #{<<"membership">> => <<"join">>}},
             {pl, Own, <<"m.room.power_levels">>, <<>>, ?DAN, #{<<"users">> => #{?DAN => 100}}},
             {jr, Own, <<"m.room.join_rules">>, <<>>, ?DAN, #{<<"join_rule">> => <<"invite">>}},
             {inv, Own, <<"m.room.member">>, ?BOB, ?DAN, #{<<"membership">> => <<"invite">>, <<"is_direct">> => true}}]
        ++ [{bob, Near, <<"m.room.member">>, ?BOB, ?BOB, #{<<"membership">> => <<"join">>}} || Joined],
    {_, _, _, Events} =
        lists:foldl(
          fun({Tag, Server, Type, Key, Sender, Content}, {Prev, State, N, Acc}) ->
                  {ID, JSON} = event(Server, #{<<"type">> => Type, <<"state_key">> => Key,
                                               <<"content">> => Content, <<"sender">> => Sender,
                                               <<"room_id">> => RoomID,
                                               <<"origin_server_ts">> => 1700000000000 + N,
                                               <<"depth">> => N + 1, <<"prev_events">> => Prev,
                                               <<"auth_events">> => maps:values(State)}),
                  State2 = State#{{Type, Key} => ID},
                  persistent_term:put({?MODULE, id, RoomID, Tag}, ID),
                  {[ID], State2, N + 1, [{ID, JSON, State2} | Acc]}
          end, {[], #{}, 0, []}, Steps),
    [{Last, _, _} | _] = Events,
    ok = mod_matrix_gw_store:save_meta(RoomID, <<"9">>, {direct, {<<"dan">>, ?HOST}, ?BOB, established}, [Last]),
    ok = mod_matrix_gw_store:save_events(RoomID, Events, [Last]),
    mod_matrix_gw_room:restore_rooms(?HOST),
    {ok, _} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
    ok.

id(RoomID, Tag) -> persistent_term:get({?MODULE, id, RoomID, Tag}).

%% dan online on a resource of this process: what is routed to him comes
%% here as {route, Packet}.
online(LUser, Res) ->
    ejabberd_sm:open_session({erlang:timestamp(), self()}, LUser, ?HOST, Res, 0, []).

received() ->
    receive {route, #message{} = M} -> [M | received()]
    after 1000 -> []
    end.

%%--------------------------------------------------------------------

s10_rate_purge() ->
    Tab = matrix_gw_rate,
    Mine = fun() -> [K || {K, _, _, _} <- ets:tab2list(Tab), is_tuple(K), element(1, K) == ?MODULE] end,
    Now = erlang:monotonic_time(millisecond),
    %% the table as full as its bound: what was there, ten long windows
    %% of this suite and a negative entry whose minute is over
    persistent_term:put({?S2S, limit, rate_max_keys}, ets:info(Tab, size) + 12),
    Long = [{?MODULE, long, I} || I <- lists:seq(1, 10)],
    [?assert(?S2S:rate(K, 5, 3600000, 1)) || K <- Long],
    ets:insert(Tab, {{negative, <<"gone.test">>}, Now - 1, negative}),
    ets:insert(Tab, {{?MODULE, over}, 0, 1, Now - 1}),
    %% a new key: what is over goes, every window still open stays
    ?assert(?S2S:rate({?MODULE, new}, 5, 3600000, 1)),
    ?assertEqual([], ets:lookup(Tab, {negative, <<"gone.test">>})),
    ?assertEqual([], ets:lookup(Tab, {?MODULE, over})),
    ?assertEqual(lists:sort([{?MODULE, new} | Long]), lists:sort(Mine())),
    %% their counts go on: one window's fifth is its last
    [?assert(?S2S:rate(hd(Long), 5, 3600000, 1)) || _ <- lists:seq(1, 4)],
    ?assertNot(?S2S:rate(hd(Long), 5, 3600000, 1)),
    %% nothing over and still full: the tenth that ends first goes, not all
    persistent_term:put({?S2S, limit, rate_max_keys}, ets:info(Tab, size)),
    Short = {?MODULE, short},
    ets:insert(Tab, {Short, 0, 1, Now + 1000}),
    ?assert(?S2S:rate({?MODULE, newer}, 5, 3600000, 1)),
    ?assertEqual([], ets:lookup(Tab, Short)),
    ?assertNot(?S2S:rate(hd(Long), 5, 3600000, 1)),
    ?assert(length(Mine()) >= 10),
    persistent_term:erase({?S2S, limit, rate_max_keys}),
    [ets:delete(Tab, K) || K <- Mine()].

s12_edus_of_members() ->
    online(<<"dan">>, <<"s12">>),
    _ = received(),
    Typing = fun(Room, User) ->
                     #{<<"edu_type">> => <<"m.typing">>,
                       <<"content">> => #{<<"room_id">> => Room, <<"user_id">> => User, <<"typing">> => true}}
             end,
    Receipt = fun(Room, User) ->
                      #{<<"edu_type">> => <<"m.receipt">>,
                        <<"content">> => #{Room => #{<<"m.read">> => #{User => #{<<"event_ids">> => [id(Room, inv)],
                                                                                  <<"data">> => #{<<"ts">> => 1}}}}}}
              end,
    %% bob joined: his typing and his receipt reach dan
    mod_matrix_gw_room:process_edus(?HOST, <<"near.test">>, [Typing(?JOINED, ?BOB), Receipt(?JOINED, ?BOB)]),
    ?assertEqual(2, length(received())),
    %% bob only invited, or one who is in no room: nothing
    mod_matrix_gw_room:process_edus(?HOST, <<"near.test">>,
                                    [Typing(?INVITED, ?BOB), Receipt(?INVITED, ?BOB),
                                     Typing(?JOINED, <<"@eve:near.test">>)]),
    ?assertEqual([], received()).

%% A logger handler of the suite: every formatted INFO line or above, kept.
log(#{level := Level, msg := Msg}, _Config) ->
    case logger:compare_levels(Level, info) of
        lt -> ok;
        _ ->
            Text = case Msg of
                       {report, R} -> io_lib:format("~p", [R]);
                       {string, S} -> S;
                       {F, A} -> io_lib:format(F, A)
                   end,
            catch ets:insert(?MODULE, {erlang:unique_integer(), iolist_to_binary(Text)})
    end.

with_log(Fun) ->
    Holder = spawn(fun() -> ets:new(?MODULE, [named_table, public, bag]), receive stop -> ok end end),
    timer:sleep(50),
    Level = maps:get(level, logger:get_primary_config()),
    logger:set_primary_config(level, info),
    ok = logger:add_handler(?MODULE, ?MODULE, #{level => info}),
    try
        Fun(),
        timer:sleep(300),
        [T || {_, T} <- ets:tab2list(?MODULE)]
    after
        logger:remove_handler(?MODULE),
        logger:set_primary_config(level, Level),
        Holder ! stop
    end.

s16_log() ->
    Near = near(),
    Secret = #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"secret words">>, <<"ciphertext">> => <<"AwgAsecret">>},
    Lines = with_log(
              fun() ->
                      %% PDUs that cannot be processed, each with a secret
                      PDUs = [#{<<"type">> => <<"m.room.message">>, <<"room_id">> => Secret,
                                <<"sender">> => ?BOB, <<"content">> => Secret},
                              #{<<"type">> => <<"m.room.message">>, <<"room_id">> => ?JOINED,
                                <<"sender">> => ?BOB, <<"content">> => Secret, <<"depth">> => Secret,
                                <<"prev_events">> => Secret, <<"auth_events">> => [Secret],
                                <<"origin_server_ts">> => Secret}],
                      {200, _} = call(Near, 'PUT', [<<"federation">>, <<"v1">>, <<"send">>, <<"s16">>], [],
                                      #{<<"origin">> => <<"near.test">>, <<"origin_server_ts">> => 1,
                                        <<"pdus">> => PDUs}),
                      %% and a join whose event is not what the room reads
                      call(Near, 'PUT', [<<"federation">>, <<"v2">>, <<"send_join">>, ?JOINED, <<"$x">>], [],
                           #{<<"type">> => <<"m.room.member">>, <<"room_id">> => ?JOINED,
                             <<"sender">> => <<"@carl:near.test">>, <<"state_key">> => <<"@carl:near.test">>,
                             <<"origin_server_ts">> => 1, <<"depth">> => 3, <<"prev_events">> => [],
                             <<"auth_events">> => [], <<"content">> => (Secret)#{<<"membership">> => <<"join">>}})
              end),
    ?assertEqual([], [L || L <- Lines, binary:match(L, [<<"secret">>, <<"AwgA">>]) /= nomatch]).

s17_bad_json() ->
    Near = near(),
    Member = fun(Membership, Over) ->
                     maps:merge(#{<<"type">> => <<"m.room.member">>, <<"room_id">> => ?JOINED,
                                  <<"sender">> => ?BOB, <<"state_key">> => ?BOB,
                                  <<"origin_server_ts">> => 1700000009000, <<"depth">> => 9,
                                  <<"prev_events">> => [id(?JOINED, bob)], <<"auth_events">> => [],
                                  <<"content">> => #{<<"membership">> => Membership}}, Over)
             end,
    Join = fun(JSON) -> call(Near, 'PUT', [<<"federation">>, <<"v2">>, <<"send_join">>, ?JOINED, <<"$x">>], [], JSON) end,
    Leave = fun(JSON) -> call(Near, 'PUT', [<<"federation">>, <<"v2">>, <<"send_leave">>, ?JOINED, <<"$x">>], [], JSON) end,
    Bad = [#{<<"sender">> => 17, <<"state_key">> => 17},
           #{<<"sender">> => [<<"@bob:near.test">>], <<"state_key">> => [<<"@bob:near.test">>]},
           #{<<"sender">> => <<"bob">>, <<"state_key">> => <<"bob">>},
           #{<<"origin_server_ts">> => <<"1700000009000">>},
           #{<<"origin_server_ts">> => 1.5},
           #{<<"prev_events">> => <<"$a">>},
           #{<<"depth">> => <<"9">>}],
    [begin
         ?assertMatch({400, #{<<"errcode">> := <<"M_BAD_JSON">>}}, Join(Member(<<"join">>, Over))),
         ?assertMatch({400, #{<<"errcode">> := <<"M_BAD_JSON">>}}, Leave(Member(<<"leave">>, Over)))
     end || Over <- Bad],
    %% the room is still there and answers
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, ?JOINED),
    ?assertMatch(#room_version{}, gen_statem:call(Pid, get_room_version)).

s14_direct_row() ->
    Key = {{<<"eli">>, ?HOST}, ?BOB},
    Old = <<"!old:near.test">>,
    mnesia:dirty_write({matrix_direct, Key, Old}),
    RoomID = <<"!s14:near.test">>,
    ok = mod_matrix_gw_room:join_direct(?HOST, <<"near.test">>, RoomID, ?BOB, <<"@eli:example.com">>),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
    Ref = monitor(process, Pid),
    %% while the join is tried, eli's chat with bob is the one he had
    timer:sleep(200),
    ?assertEqual([{matrix_direct, Key, Old}], mnesia:dirty_read(matrix_direct, Key)),
    %% near.test answers no make_join: the room's process ends, the row stays
    receive {'DOWN', Ref, _, _, _} -> ok after 40000 -> error(still_there) end,
    ?assertEqual([{matrix_direct, Key, Old}], mnesia:dirty_read(matrix_direct, Key)).
