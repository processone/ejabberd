-module(eredis_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eredis.hrl").

-import(eredis, [create_multibulk/1]).

get_set_test() ->
    C = c(),
    ?assertMatch({ok, _}, eredis:q(C, ["DEL", foo])),

    ?assertEqual({ok, undefined}, eredis:q(C, ["GET", foo])),
    ?assertEqual({ok, <<"OK">>}, eredis:q(C, ["SET", foo, bar])),
    ?assertEqual({ok, <<"bar">>}, eredis:q(C, ["GET", foo])).


delete_test() ->
    C = c(),
    ?assertMatch({ok, _}, eredis:q(C, ["DEL", foo])),

    ?assertEqual({ok, <<"OK">>}, eredis:q(C, ["SET", foo, bar])),
    ?assertEqual({ok, <<"1">>}, eredis:q(C, ["DEL", foo])),
    ?assertEqual({ok, undefined}, eredis:q(C, ["GET", foo])).

mset_mget_test() ->
    C = c(),
    Keys = lists:seq(1, 1000),

    ?assertMatch({ok, _}, eredis:q(C, ["DEL" | Keys])),

    KeyValuePairs = [[K, K*2] || K <- Keys],
    ExpectedResult = [list_to_binary(integer_to_list(K * 2)) || K <- Keys],

    ?assertEqual({ok, <<"OK">>}, eredis:q(C, ["MSET" | lists:flatten(KeyValuePairs)])),
    ?assertEqual({ok, ExpectedResult}, eredis:q(C, ["MGET" | Keys])),
    ?assertMatch({ok, _}, eredis:q(C, ["DEL" | Keys])).

exec_test() ->
    C = c(),

    ?assertMatch({ok, _}, eredis:q(C, ["LPUSH", "k1", "b"])),
    ?assertMatch({ok, _}, eredis:q(C, ["LPUSH", "k1", "a"])),
    ?assertMatch({ok, _}, eredis:q(C, ["LPUSH", "k2", "c"])),

    ?assertEqual({ok, <<"OK">>}, eredis:q(C, ["MULTI"])),
    ?assertEqual({ok, <<"QUEUED">>}, eredis:q(C, ["LRANGE", "k1", "0", "-1"])),
    ?assertEqual({ok, <<"QUEUED">>}, eredis:q(C, ["LRANGE", "k2", "0", "-1"])),

    ExpectedResult = [[<<"a">>, <<"b">>], [<<"c">>]],

    ?assertEqual({ok, ExpectedResult}, eredis:q(C, ["EXEC"])),

    ?assertMatch({ok, _}, eredis:q(C, ["DEL", "k1", "k2"])).

exec_nil_test() ->
    C1 = c(),
    C2 = c(),

    ?assertEqual({ok, <<"OK">>}, eredis:q(C1, ["WATCH", "x"])),
    ?assertMatch({ok, _}, eredis:q(C2, ["INCR", "x"])),
    ?assertEqual({ok, <<"OK">>}, eredis:q(C1, ["MULTI"])),
    ?assertEqual({ok, <<"QUEUED">>}, eredis:q(C1, ["GET", "x"])),
    ?assertEqual({ok, undefined}, eredis:q(C1, ["EXEC"])),
    ?assertMatch({ok, _}, eredis:q(C1, ["DEL", "x"])).

pipeline_test() ->
    C = c(),

    P1 = [["SET", a, "1"],
          ["LPUSH", b, "3"],
          ["LPUSH", b, "2"]],

    ?assertEqual([{ok, <<"OK">>}, {ok, <<"1">>}, {ok, <<"2">>}],
                 eredis:qp(C, P1)),

    P2 = [["MULTI"],
          ["GET", a],
          ["LRANGE", b, "0", "-1"],
          ["EXEC"]],

    ?assertEqual([{ok, <<"OK">>},
                  {ok, <<"QUEUED">>},
                  {ok, <<"QUEUED">>},
                  {ok, [<<"1">>, [<<"2">>, <<"3">>]]}],
                 eredis:qp(C, P2)),

    ?assertMatch({ok, _}, eredis:q(C, ["DEL", a, b])).

pipeline_mixed_test() ->
    C = c(),
    P1 = [["LPUSH", c, "1"] || _ <- lists:seq(1, 100)],
    P2 = [["LPUSH", d, "1"] || _ <- lists:seq(1, 100)],
    Expect = [{ok, list_to_binary(integer_to_list(I))} || I <- lists:seq(1, 100)],
    spawn(fun () ->
                  erlang:yield(),
                  ?assertEqual(Expect, eredis:qp(C, P1))
          end),
    spawn(fun () ->
                  ?assertEqual(Expect, eredis:qp(C, P2))
          end),
    timer:sleep(10),
    ?assertMatch({ok, _}, eredis:q(C, ["DEL", c, d])).


c() ->
    Res = eredis:start_link(),
    ?assertMatch({ok, _}, Res),
    {ok, C} = Res,
    C.

multibulk_test_() ->
    [?_assertEqual(<<"*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$3\r\nbar\r\n">>,
                   list_to_binary(create_multibulk(["SET", "foo", "bar"]))),
     ?_assertEqual(<<"*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$3\r\nbar\r\n">>,
                   list_to_binary(create_multibulk(['SET', foo, bar]))),

     ?_assertEqual(<<"*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$3\r\n123\r\n">>,
                   list_to_binary(create_multibulk(['SET', foo, 123]))),

     ?_assertThrow({cannot_store_floats, 123.5},
                   list_to_binary(create_multibulk(['SET', foo, 123.5])))
    ].
