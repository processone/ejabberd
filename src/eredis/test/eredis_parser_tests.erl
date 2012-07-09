%%
%% Parser tests. In particular tests for partial responses. This would
%% probably be a very good candidate for testing with quickcheck or
%% properl.
%%

-module(eredis_parser_tests).

-include_lib("eredis.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(eredis_parser, [parse/2, init/0, parse_bulk/1, parse_bulk/2,
                        parse_multibulk/1, parse_multibulk/2]).


% parse a binary one byte at a time
one_byte_parse(B) ->
    one_byte_parse(init(), B).

one_byte_parse(S, <<>>) ->
    parse(S, <<>>);
one_byte_parse(S, <<Byte>>) ->
    parse(S, <<Byte>>);
one_byte_parse(S, <<Byte, B/binary>>) ->
    case parse(S, <<Byte>>) of
        {continue, NewState} ->
            one_byte_parse(NewState, B);
        {ok, Value, Rest, NewState} ->
            {ok, Value, <<Rest/binary, B/binary>>, NewState};
        {error, Err, Rest, NewState} ->
            {error, Err, <<Rest/binary, B/binary>>, NewState};
        Other ->
            Other
    end.


parse_bulk_test() ->
    B = <<"$3\r\nbar\r\n">>,
    ?assertEqual({ok, <<"bar">>, #pstate{}}, parse(#pstate{}, B)).

parse_split_bulk_test() ->
    State1 = init(),
    B1 = <<"$3\r\n">>,
    B2 = <<"bar\r\n">>,

    {continue, State2} = parse(State1, B1),
    ?assertEqual(#pstate{state = bulk_continue, continuation_data = {3, <<"\r\n">>}},
                 State2),

    ?assertMatch({ok, <<"bar">>, _}, parse(State2, B2)).


parse_very_split_bulk_test() ->
    State1 = init(),
    B1 = <<"$1">>,
    B2 = <<"3\r\n">>,
    B3 = <<"foobarbazquux\r\n">>, %% 13 bytes

    ?assertEqual({continue,
                  #pstate{state = bulk_continue,
                          continuation_data = {incomplete_size, <<"$1">>}}},
                 parse(State1, B1)),
    {continue, State2} = parse(State1, B1),

    ?assertEqual({continue,
                  #pstate{state = bulk_continue,
                          continuation_data = {13, <<"\r\n">>}}},
                 parse(State2, B2)),
    {continue, State3} = parse(State2, B2),

    ?assertMatch({ok, <<"foobarbazquux">>, _}, parse(State3, B3)).


too_much_data_test() ->
    B = <<"$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n">>,
    ?assertEqual({ok, <<"1">>, <<"$1\r\n2\r\n$1\r\n3\r\n">>}, parse_bulk(B)).

too_much_data_in_continuation_test() ->
    B1 = <<"$1\r\n">>,
    B2 = <<"1\r\n$1\r\n2\r\n$1\r\n3\r\n">>,

    ?assertEqual({continue, {1, <<"\r\n">>}}, parse_bulk(B1)),
    {continue, ContinuationData1} = parse_bulk(B1),

    ?assertEqual({ok, <<"1">>, <<"$1\r\n2\r\n$1\r\n3\r\n">>},
                 parse_bulk(ContinuationData1, B2)).

bulk_test_() ->
    B = <<"$3\r\nbar\r\n">>,
    ?_assertEqual({ok, <<"bar">>, <<>>}, parse_bulk(B)).

bulk_split_test() ->
    B1 = <<"$3\r\n">>,
    B2 = <<"bar\r\n">>,

    ?assertEqual({continue, {3, <<"\r\n">>}}, parse_bulk(B1)),
    {continue, Res} = parse_bulk(B1),
    ?assertEqual({ok, <<"bar">>, <<>>}, parse_bulk(Res, B2)).

bulk_very_split_test() ->
    B1 = <<"$1">>,
    B2 = <<"3\r\n">>,
    B3 = <<"foobarbazquux\r\n">>, %% 13 bytes

    ?assertEqual({continue, {incomplete_size, <<"$1">>}}, parse_bulk(B1)),
    {continue, ContinuationData1} = parse_bulk(B1),

    ?assertEqual({continue, {13, <<"\r\n">>}}, parse_bulk(ContinuationData1, B2)),
    {continue, ContinuationData2} = parse_bulk(ContinuationData1, B2),

    ?assertEqual({ok, <<"foobarbazquux">>, <<>>}, parse_bulk(ContinuationData2, B3)).

bulk_split_on_newline_test() ->
    B1 = <<"$13\r\nfoobarbazquux">>,
    B2 = <<"\r\n">>, %% 13 bytes

    ?assertEqual({continue, {13, <<"\r\nfoobarbazquux">>}}, parse_bulk(B1)),
    {continue, ContinuationData1} = parse_bulk(B1),
    ?assertEqual({ok, <<"foobarbazquux">>, <<>>}, parse_bulk(ContinuationData1, B2)).


bulk_nil_test() ->
    B = <<"$-1\r\n">>,
    ?assertEqual({ok, undefined, init()}, parse(init(), B)).

bulk_nil_chunked_test() ->
    State1 = init(),
    B1 = <<"$-1">>,
    B2 = <<"\r\n">>,
    ?assertEqual({continue, #pstate{state = bulk_continue,
                                    continuation_data = {incomplete_size,<<"$-1">>}}},
                 parse(State1, B1)),

    {continue, State2} = parse(State1, B1),

    ?assertEqual({ok, undefined, init()}, parse(State2, B2)).

bulk_nil_with_extra_test() ->
    B = <<"$-1\r\n$3\r\nfoo\r\n">>,
    ?assertEqual({ok, undefined, <<"$3\r\nfoo\r\n">>, init()}, parse(init(), B)).

bulk_crap_test() ->
    B = <<"\r\n">>,
    ?assertEqual({error, unknown_response}, parse(init(), B)).




multibulk_test() ->
    %% [{1, 1}, {2, 2}, {3, 3}]
    B = <<"*3\r\n$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n">>,
    ?assertEqual({ok, [<<"1">>, <<"2">>, <<"3">>], <<>>}, parse_multibulk(B)).

multibulk_parse_test() ->
    %% [{1, 1}, {2, 2}, {3, 3}]
    B = <<"*3\r\n$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n">>,
    ?assertEqual({ok, [<<"1">>, <<"2">>, <<"3">>], #pstate{}}, parse(init(), B)).

multibulk_one_byte_parse_test() ->
    %% [{1, 1}, {2, 2}, {3, 3}]
    B = <<"*3\r\n$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n">>,
    ?assertEqual({ok, [<<"1">>, <<"2">>, <<"3">>], #pstate{}},
                 one_byte_parse(B)).

nested_multibulk_test() ->
    %% [[1, 2], [3, 4]]
    B = <<"*2\r\n*2\r\n$1\r\n1\r\n$1\r\n2\r\n*2\r\n$1\r\n3\r\n$1\r\n4\r\n">>,
    ?assertEqual({ok, [[<<"1">>, <<"2">>], [<<"3">>, <<"4">>]], <<>>},
                 parse_multibulk(B)).

nested_multibulk_parse_test() ->
    %% [[1, 2], [3, 4]]
    B = <<"*2\r\n*2\r\n$1\r\n1\r\n$1\r\n2\r\n*2\r\n$1\r\n3\r\n$1\r\n4\r\n">>,
    ?assertEqual({ok, [[<<"1">>, <<"2">>], [<<"3">>, <<"4">>]], #pstate{}},
                 parse(init(), B)).

nested_multibulk_one_byte_parse_test() ->
    %% [[1, 2], [3, 4]]
    B = <<"*2\r\n*2\r\n$1\r\n1\r\n$1\r\n2\r\n*2\r\n$1\r\n3\r\n$1\r\n4\r\n">>,
    ?assertEqual({ok, [[<<"1">>, <<"2">>], [<<"3">>, <<"4">>]], #pstate{}},
                 one_byte_parse(B)).

multibulk_split_parse_test() ->
    %% [{1, 1}, {2, 2}, {3, 3}]
    B1 = <<"*3\r\n$1\r\n1\r\n$1">>,
    B2 = <<"\r\n2\r\n$1\r\n3\r\n">>,

    State1 = init(),

    ?assertEqual({continue,
                  #pstate{state = multibulk_continue,
                          continuation_data =
                              {in_parsing_bulks,2,<<"$1">>,[<<"1">>]}}},
                 parse(State1, B1)),

    {continue, State2} = parse(State1, B1),

    ?assertMatch({ok, [<<"1">>, <<"2">>, <<"3">>], _}, parse(State2, B2)).

multibulk_split_test() ->
    %% Split into 2 parts: <<"*3\r\n$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n">>
    B1 = <<"*3\r\n$1\r\n1\r\n$1">>,
    B2 = <<"\r\n2\r\n$1\r\n3\r\n">>,

    {continue, ContinuationData1} = parse_multibulk(B1),
    Result = parse_multibulk(ContinuationData1, B2),
    ?assertEqual({ok, [<<"1">>, <<"2">>, <<"3">>], <<>>}, Result).

multibulk_very_split_test() ->
    %% Split into 4 parts: <<"*3\r\n$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n">>
    B1 = <<"*">>,
    B2 = <<"3\r\n$1\r">>,
    B3 = <<"\n1\r\n$1\r\n2\r\n$1">>,
    B4 = <<"\r\n3\r\n">>,

    ?assertEqual({continue, {incomplete_size, <<"*">>}}, parse_multibulk(B1)),
    {continue, ContinuationData1} = parse_multibulk(B1),
    {continue, ContinuationData2} = parse_multibulk(ContinuationData1, B2),
    {continue, ContinuationData3} = parse_multibulk(ContinuationData2, B3),

    Result = parse_multibulk(ContinuationData3, B4),
    ?assertEqual({ok, [<<"1">>, <<"2">>, <<"3">>], <<>>}, Result).

multibulk_newline_split_test() ->
    %% Split into 4 parts: <<"*3\r\n$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n">>
    B1 = <<"*2\r\n$1\r\n1">>,
    B2 = <<"\r\n$1\r\n2\r\n">>,
    ?assertEqual({continue, {in_parsing_bulks, 2, <<"$1\r\n1">>, []}},
                 parse_multibulk(B1)),

    {continue, ContinuationData1} = parse_multibulk(B1),

    ?assertEqual({ok, [<<"1">>, <<"2">>], <<>>}, parse_multibulk(ContinuationData1, B2)).

multibulk_nil_test() ->
    B = <<"*-1\r\n">>,
    ?assertEqual({ok, undefined, <<>>}, parse_multibulk(B)).

multibulk_nil_parse_test() ->
    B = <<"*-1\r\n">>,
    ?assertEqual({ok, undefined, #pstate{}}, parse(init(), B)).

big_chunks_test() ->
    %% Real-world example, MGET 1..200
    B1 = <<"*200\r\n$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n$1\r\n4\r\n$1\r\n5\r\n$1\r\n6\r\n$1\r\n7\r\n$1\r\n8\r\n$1\r\n9\r\n$2\r\n10\r\n$2\r\n11\r\n$2\r\n12\r\n$2\r\n13\r\n$2\r\n14\r\n$2\r\n15\r\n$2\r\n16\r\n$2\r\n17\r\n$2\r\n18\r\n$2\r\n19\r\n$2\r\n20\r\n$2\r\n21\r\n$2\r\n22\r\n$2\r\n23\r\n$2\r\n24\r\n$2\r\n25\r\n$2\r\n26\r\n$2\r\n27\r\n$2\r\n28\r\n$2\r\n29\r\n$2\r\n30\r\n$2\r\n31\r\n$2\r\n32\r\n$2\r\n33\r\n$2\r\n34\r\n$2\r\n35\r\n$2\r\n36\r\n$2\r\n37\r\n$2\r\n38\r\n$2\r\n39\r\n$2\r\n40\r\n$2\r\n41\r\n$2\r\n42\r\n$2\r\n43\r\n$2\r\n44\r\n$2\r\n45\r\n$2\r\n46\r\n$2\r\n47\r\n$2\r\n48\r\n$2\r\n49\r\n$2\r\n50\r\n$2\r\n51\r\n$2\r\n52\r\n$2\r\n53\r\n$2\r\n54\r\n$2\r\n55\r\n$2\r\n56\r\n$2\r\n57\r\n$2\r\n58\r\n$2\r\n59\r\n$2\r\n60\r\n$2\r\n61\r\n$2\r\n62\r\n$2\r\n63\r\n$2\r\n64\r\n$2\r\n65\r\n$2\r\n66\r\n$2\r\n67\r\n$2\r\n68\r\n$2\r\n69\r\n$2\r\n70\r\n$2\r\n71\r\n$2\r\n72\r\n$2\r\n73\r\n$2\r\n74\r\n$2\r\n75\r\n$2\r\n76\r\n$2\r\n77\r\n$2\r\n78\r\n$2\r\n79\r\n$2\r\n80\r\n$2\r\n81\r\n$2\r\n82\r\n$2\r\n83\r\n$2\r\n84\r\n$2\r\n85\r\n$2\r\n86\r\n$2\r\n87\r\n$2\r\n88\r\n$2\r\n89\r\n$2\r\n90\r\n$2\r\n91\r\n$2\r\n92\r\n$2\r\n93\r\n$2\r\n94\r\n$2\r\n95\r\n$2\r\n96\r\n$2\r\n97\r\n$2\r\n98\r\n$2\r\n99\r\n$3\r\n100\r\n$3\r\n101\r\n$3\r\n102\r\n$3\r\n103\r\n$3\r\n104\r\n$3\r\n105\r\n$3\r\n106\r\n$3\r\n107\r\n$3\r\n108\r\n$3\r\n109\r\n$3\r\n110\r\n$3\r\n111\r\n$3\r\n112\r\n$3\r\n113\r\n$3\r\n114\r\n$3\r\n115\r\n$3\r\n116\r\n$3\r\n117\r\n$3\r\n118\r\n$3\r\n119\r\n$3\r\n120\r\n$3\r\n121\r\n$3\r\n122\r\n$3\r\n123\r\n$3\r\n124\r\n$3\r\n125\r\n$3\r\n126\r\n$3\r\n127\r\n$3\r\n128\r\n$3\r\n129\r\n$3\r\n130\r\n$3\r\n131\r\n$3\r\n132\r\n$3\r\n133\r\n$3\r\n134\r\n$3\r\n135\r\n$3\r\n136\r\n$3\r\n137\r\n$3\r\n138\r\n$3\r\n139\r\n$3\r\n140\r\n$3\r\n141\r\n$3\r\n142\r\n$3\r\n143\r\n$3\r\n144\r\n$3\r\n145\r\n$3\r\n146\r\n$3\r\n147\r\n$3\r\n148\r\n$3\r\n149\r\n$3\r\n150\r\n$3\r\n151\r\n$3\r\n152\r\n$3\r\n153\r\n$3\r\n154\r\n$3\r\n155\r\n$3\r\n156\r\n$3\r\n157\r\n$3\r\n158\r\n$3\r\n159\r\n$3\r\n160\r\n$3\r\n161\r\n$3\r\n162\r\n$3\r\n163\r\n$3\r\n164\r\n$3\r\n165\r\n$3\r\n166\r\n$3\r\n167\r\n$3\r\n168\r\n$3\r\n169\r\n$3\r\n170\r\n$3\r\n171\r\n$3\r\n172\r\n$3\r\n173\r\n$3\r\n1">>,
    B2 = <<"74\r\n$3\r\n175\r\n$3\r\n176\r\n$3\r\n177\r\n$3\r\n178\r\n$3\r\n179\r\n$3\r\n180\r\n$3\r\n181\r\n$3\r\n182\r\n$3\r\n183\r\n$3\r\n184\r\n$3\r\n185\r\n$3\r\n186\r\n$3\r\n187\r\n$3\r\n188\r\n$3\r\n189\r\n$3\r\n190\r\n$3\r\n191\r\n$3\r\n192\r\n$3\r\n193\r\n$3\r\n194\r\n$3\r\n195\r\n$3\r\n196\r\n$3\r\n197\r\n$3\r\n198\r\n$3\r\n199\r\n$3\r\n200\r\n">>,
    ExpectedValues = [list_to_binary(integer_to_list(N)) || N <- lists:seq(1, 200)],
    State1 = init(),

    ?assertMatch({continue,
                  #pstate{state = multibulk_continue,
                          continuation_data = {in_parsing_bulks, 27, _, _}}},
                 parse(State1, B1)),
    {continue, State2} = parse(State1, B1),

    ?assertMatch({ok, ExpectedValues, #pstate{state = undefined,
                                              continuation_data = undefined}},
                 parse(State2, B2)).


chunk_test() ->
    B1 = <<"*500\r\n$1\r\n1\r\n$1\r\n2\r\n$1\r\n3\r\n$1\r\n4\r\n$1\r\n5\r\n$1\r\n6\r\n$1\r\n7\r\n$1\r\n8\r\n$1\r\n9\r\n$2\r\n10\r\n$2\r\n11\r\n$2\r\n12\r\n$2\r\n13\r\n$2\r\n14\r\n$2\r\n15\r\n$2\r\n16\r\n$2\r\n17\r\n$2\r\n18\r\n$2\r\n19\r\n$2\r\n20\r\n$2\r\n21\r\n$2\r\n22\r\n$2\r\n23\r\n$2\r\n24\r\n$2\r\n25\r\n$2\r\n26\r\n$2\r\n27\r\n$2\r\n28\r\n$2\r\n29\r\n$2\r\n30\r\n$2\r\n31\r\n$2\r\n32\r\n$2\r\n33\r\n$2\r\n34\r\n$2\r\n35\r\n$2\r\n36\r\n$2\r\n37\r\n$2\r\n38\r\n$2\r\n39\r\n$2\r\n40\r\n$2\r\n41\r\n$2\r\n42\r\n$2\r\n43\r\n$2\r\n44\r\n$2\r\n45\r\n$2\r\n46\r\n$2\r\n47\r\n$2\r\n48\r\n$2\r\n49\r\n$2\r\n50\r\n$2\r\n51\r\n$2\r\n52\r\n$2\r\n53\r\n$2\r\n54\r\n$2\r\n55\r\n$2\r\n56\r\n$2\r\n57\r\n$2\r\n58\r\n$2\r\n59\r\n$2\r\n60\r\n$2\r\n61\r\n$2\r\n62\r\n$2\r\n63\r\n$2\r\n64\r\n$2\r\n65\r\n$2\r\n66\r\n$2\r\n67\r\n$2\r\n68\r\n$2\r\n69\r\n$2\r\n70\r\n$2\r\n71\r\n$2\r\n72\r\n$2\r\n73\r\n$2\r\n74\r\n$2\r\n75\r\n$2\r\n76\r\n$2\r\n77\r\n$2\r\n78\r\n$2\r\n79\r\n$2\r\n80\r\n$2\r\n81\r\n$2\r\n82\r\n$2\r\n83\r\n$2\r\n84\r\n$2\r\n85\r\n$2\r\n86\r\n$2\r\n87\r\n$2\r\n88\r\n$2\r\n89\r\n$2\r\n90\r\n$2\r\n91\r\n$2\r\n92\r\n$2\r\n93\r\n$2\r\n94\r\n$2\r\n95\r\n$2\r\n96\r\n$2\r\n97\r\n$2\r\n98\r\n$2\r\n99\r\n$3\r\n100\r\n$3\r\n101\r\n$3\r\n102\r\n$3\r\n103\r\n$3\r\n104\r\n$3\r\n105\r\n$3\r\n106\r\n$3\r\n107\r\n$3\r\n108\r\n$3\r\n109\r\n$3\r\n110\r\n$3\r\n111\r\n$3\r\n112\r\n$3\r\n113\r\n$3\r\n114\r\n$3\r\n115\r\n$3\r\n116\r\n$3\r\n117\r\n$3\r\n118\r\n$3\r\n119\r\n$3\r\n120\r\n$3\r\n121\r\n$3\r\n122\r\n$3\r\n123\r\n$3\r\n124\r\n$3\r\n125\r\n$3\r\n126\r\n$3\r\n127\r\n$3\r\n128\r\n$3\r\n129\r\n$3\r\n130\r\n$3\r\n131\r\n$3\r\n132\r\n$3\r\n133\r\n$3\r\n134\r\n$3\r\n135\r\n$3\r\n136\r\n$3\r\n137\r\n$3\r\n138\r\n$3\r\n139\r\n$3\r\n140\r\n$3\r\n141\r\n$3\r\n142\r\n$3\r\n143\r\n$3\r\n144\r\n$3\r\n145\r\n$3\r\n146\r\n$3\r\n147\r\n$3\r\n148\r\n$3\r\n149\r\n$3\r\n150\r\n$3\r\n151\r\n$3\r\n152\r\n$3\r\n153\r\n$3\r\n154\r\n$3\r\n155\r\n$3\r\n156\r\n$3\r\n157\r\n$3\r\n158\r\n$3\r\n159\r\n$3\r\n160\r\n$3\r\n161\r\n$3\r\n162\r\n$3\r\n163\r\n$3\r\n164\r\n$3\r\n165\r\n$3\r\n166\r\n$3\r\n167\r\n$3\r\n168\r\n$3\r\n169\r\n$3\r\n170\r\n$3\r\n171\r\n$3\r\n172\r\n$3\r\n173\r\n$3\r\n1">>,
    B2 = <<"74\r\n$3\r\n175\r\n$3\r\n176\r\n$3\r\n177\r\n$3\r\n178\r\n$3\r\n179\r\n$3\r\n180\r\n$3\r\n181\r\n$3\r\n182\r\n$3\r\n183\r\n$3\r\n184\r\n$3\r\n185\r\n$3\r\n186\r\n$3\r\n187\r\n$3\r\n188\r\n$3\r\n189\r\n$3\r\n190\r\n$3\r\n191\r\n$3\r\n192\r\n$3\r\n193\r\n$3\r\n194\r\n$3\r\n195\r\n$3\r\n196\r\n$3\r\n197\r\n$3\r\n198\r\n$3\r\n199\r\n$3\r\n200\r\n$3\r\n201\r\n$3\r\n202\r\n$3\r\n203\r\n$3\r\n204\r\n$3\r\n205\r\n$3\r\n206\r\n$3\r\n207\r\n$3\r\n208\r\n$3\r\n209\r\n$3\r\n210\r\n$3\r\n211\r\n$3\r\n212\r\n$3\r\n213\r\n$3\r\n214\r\n$3\r\n215\r\n$3\r\n216\r\n$3\r\n217\r\n$3\r\n218\r\n$3\r\n219\r\n$3\r\n220\r\n$3\r\n221\r\n$3\r\n222\r\n$3\r\n223\r\n$3\r\n224\r\n$3\r\n225\r\n$3\r\n226\r\n$3\r\n227\r\n$3\r\n228\r\n$3\r\n229\r\n$3\r\n230\r\n$3\r\n231\r\n$3\r\n232\r\n$3\r\n233\r\n$3\r\n234\r\n$3\r\n235\r\n$3\r\n236\r\n$3\r\n237\r\n$3\r\n238\r\n$3\r\n239\r\n$3\r\n240\r\n$3\r\n241\r\n$3\r\n242\r\n$3\r\n243\r\n$3\r\n244\r\n$3\r\n245\r\n$3\r\n246\r\n$3\r\n247\r\n$3\r\n248\r\n$3\r\n249\r\n$3\r\n250\r\n$3\r\n251\r\n$3\r\n252\r\n$3\r\n253\r\n$3\r\n254\r\n$3\r\n255\r\n$3\r\n256\r\n$3\r\n257\r\n$3\r\n258\r\n$3\r\n259\r\n$3\r\n260\r\n$3\r\n261\r\n$3\r\n262\r\n$3\r\n263\r\n$3\r\n264\r\n$3\r\n265\r\n$3\r\n266\r\n$3\r\n267\r\n$3\r\n268\r\n$3\r\n269\r\n$3\r\n270\r\n$3\r\n271\r\n$3\r\n272\r\n$3\r\n273\r\n$3\r\n274\r\n$3\r\n275\r\n$3\r\n276\r\n$3\r\n277\r\n$3\r\n278\r\n$3\r\n279\r\n$3\r\n280\r\n$3\r\n281\r\n$3\r\n282\r\n$3\r\n283\r\n$3\r\n284\r\n$3\r\n285\r\n$3\r\n286\r\n$3\r\n287\r\n$3\r\n288\r\n$3\r\n289\r\n$3\r\n290\r\n$3\r\n291\r\n$3\r\n292\r\n$3\r\n293\r\n$3\r\n294\r\n$3\r\n295\r\n$3\r\n296\r\n$3\r\n297\r\n$3\r\n298\r\n$3\r\n299\r\n$3\r\n300\r\n$3\r\n301\r\n$3\r\n302\r\n$3\r\n303\r\n$3\r\n304\r\n$3\r\n305\r\n$3\r\n306\r\n$3\r\n307\r\n$3\r\n308\r\n$3\r\n309\r\n$3\r\n310\r\n$3\r\n311\r\n$3\r\n312\r\n$3\r\n313\r\n$3\r\n314\r\n$3\r\n315\r\n$3\r\n316\r\n$3\r\n317\r\n$3\r\n318\r\n$3\r\n319\r\n$3\r\n320\r\n$3\r\n321\r\n$3\r\n322\r\n$3\r\n323\r\n$3\r\n324\r\n$3\r\n325\r\n$3\r\n326\r\n$3\r\n327\r\n$3\r\n328\r\n$3\r\n329\r\n$3\r\n330\r\n$3\r\n331\r\n$3\r\n332\r\n$3\r\n333\r\n$3\r\n334\r\n$3\r\n335\r\n$3\r\n336">>,
    B3 = <<"\r\n$3\r\n337\r\n$3\r\n338\r\n$3\r\n339\r\n$3\r\n340\r\n$3\r\n341\r\n$3\r\n342\r\n$3\r\n343\r\n$3\r\n344\r\n$3\r\n345\r\n$3\r\n346\r\n$3\r\n347\r\n$3\r\n348\r\n$3\r\n349\r\n$3\r\n350\r\n$3\r\n351\r\n$3\r\n352\r\n$3\r\n353\r\n$3\r\n354\r\n$3\r\n355\r\n$3\r\n356\r\n$3\r\n357\r\n$3\r\n358\r\n$3\r\n359\r\n$3\r\n360\r\n$3\r\n361\r\n$3\r\n362\r\n$3\r\n363\r\n$3\r\n364\r\n$3\r\n365\r\n$3\r\n366\r\n$3\r\n367\r\n$3\r\n368\r\n$3\r\n369\r\n$3\r\n370\r\n$3\r\n371\r\n$3\r\n372\r\n$3\r\n373\r\n$3\r\n374\r\n$3\r\n375\r\n$3\r\n376\r\n$3\r\n377\r\n$3\r\n378\r\n$3\r\n379\r\n$3\r\n380\r\n$3\r\n381\r\n$3\r\n382\r\n$3\r\n383\r\n$3\r\n384\r\n$3\r\n385\r\n$3\r\n386\r\n$3\r\n387\r\n$3\r\n388\r\n$3\r\n389\r\n$3\r\n390\r\n$3\r\n391\r\n$3\r\n392\r\n$3\r\n393\r\n$3\r\n394\r\n$3\r\n395\r\n$3\r\n396\r\n$3\r\n397\r\n$3\r\n398\r\n$3\r\n399\r\n$3\r\n400\r\n$3\r\n401\r\n$3\r\n402\r\n$3\r\n403\r\n$3\r\n404\r\n$3\r\n405\r\n$3\r\n406\r\n$3\r\n407\r\n$3\r\n408\r\n$3\r\n409\r\n$3\r\n410\r\n$3\r\n411\r\n$3\r\n412\r\n$3\r\n413\r\n$3\r\n414\r\n$3\r\n415\r\n$3\r\n416\r\n$3\r\n417\r\n$3\r\n418\r\n$3\r\n419\r\n$3\r\n420\r\n$3\r\n421\r\n$3\r\n422\r\n$3\r\n423\r\n$3\r\n424\r\n$3\r\n425\r\n$3\r\n426\r\n$3\r\n427\r\n$3\r\n428\r\n$3\r\n429\r\n$3\r\n430\r\n$3\r\n431\r\n$3\r\n432\r\n$3\r\n433\r\n$3\r\n434\r\n$3\r\n435\r\n$3\r\n436\r\n$3\r\n437\r\n$3\r\n438\r\n$3\r\n439\r\n$3\r\n440\r\n$3\r\n441\r\n$3\r\n442\r\n$3\r\n443\r\n$3\r\n444\r\n$3\r\n445\r\n$3\r\n446\r\n$3\r\n447\r\n$3\r\n448\r\n$3\r\n449\r\n$3\r\n450\r\n$3\r\n451\r\n$3\r\n452\r\n$3\r\n453\r\n$3\r\n454\r\n$3\r\n455\r\n$3\r\n456\r\n$3\r\n457\r\n$3\r\n458\r\n$3\r\n459\r\n$3\r\n460\r\n$3\r\n461\r\n$3\r\n462\r\n$3\r\n463\r\n$3\r\n464\r\n$3\r\n465\r\n$3\r\n466\r\n$3\r\n467\r\n$3\r\n468\r\n$3\r\n469\r\n$3\r\n470\r\n$3\r\n471\r\n$3\r\n472\r\n$3\r\n473\r\n$3\r\n474\r\n$3\r\n475\r\n$3\r\n476\r\n$3\r\n477\r\n$3\r\n478\r\n$3\r\n479\r\n$3\r\n480\r\n$3\r\n481\r\n$3\r\n482\r\n$3\r\n483\r\n$3\r\n484\r\n$3\r\n485\r\n$3\r\n486\r\n$3\r\n487\r\n$3\r\n488\r\n$3\r\n489\r\n$3\r\n490\r\n$3\r\n491\r\n$3\r\n492\r\n$3\r\n493\r\n$3\r\n494\r\n$3\r\n495\r\n$3\r\n496\r\n$3\r\n497\r\n$3\r\n498\r\n">>,

    {continue, ContinuationData1} = parse_multibulk(B1),
    {continue, ContinuationData2} = parse_multibulk(ContinuationData1, B2),

    ?assertMatch({continue, {in_parsing_bulks, 2, <<>>, _}},
                 parse_multibulk(ContinuationData2, B3)).

%% @doc: Test a binary string which contains \r\n inside it's data
binary_safe_test() ->
    B = <<"$14\r\nfoobar\r\nbarbaz\r\n">>,
    ?assertEqual({ok, <<"foobar\r\nbarbaz">>, init()}, parse(init(), B)).


status_test() ->
    B = <<"+OK\r\n">>,
    ?assertEqual({ok, <<"OK">>, init()}, parse(init(), B)).

status_chunked_test() ->
    B1 = <<"+O">>,
    B2 = <<"K\r\n">>,
    State1 = init(),

    ?assertEqual({continue, #pstate{state = status_continue,
                                    continuation_data = {incomplete_simple, <<"O">>}}},
                 parse(State1, B1)),
    {continue, State2} = parse(State1, B1),
    ?assertEqual({ok, <<"OK">>, init()}, parse(State2, B2)).

error_test() ->
    B = <<"-ERR wrong number of arguments for 'get' command\r\n">>,
    ?assertEqual({error, <<"ERR wrong number of arguments for 'get' command">>, init()},
                 parse(init(), B)).

integer_test() ->
    B = <<":2\r\n">>,
    ?assertEqual({ok, <<"2">>, init()}, parse(init(), B)).

integer_reply_inside_multibulk_test() ->
    B = <<"*2\r\n:1\r\n:1\r\n">>,
    ?assertEqual({ok, [<<"1">>, <<"1">>], init()}, parse(init(), B)).

status_inside_multibulk_test() ->
    B = <<"*2\r\n+OK\r\n:1\r\n">>,
    ?assertEqual({ok, [<<"OK">>, <<"1">>], init()}, parse(init(), B)).

error_inside_multibulk_test() ->
    B = <<"*2\r\n-ERR foobar\r\n:1\r\n">>,
    ?assertEqual({ok, [<<"ERR foobar">>, <<"1">>], init()}, parse(init(), B)).
