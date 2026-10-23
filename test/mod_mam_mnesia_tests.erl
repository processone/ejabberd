-module(mod_mam_mnesia_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_mam.hrl").

-export([initialize/1, pagination_case/0, filters_case/0,
         selected_key_filters_case/0, timestamp_ties_case/0,
         count_only_case/0, bounded_memory_case/0]).

-define(HOST, <<"mam.example.test">>).
-define(OWNER, {<<"owner">>, ?HOST}).
-define(PEER, {<<"peer">>, <<"remote.example.test">>, <<"one">>}).

mnesia_select_test_() ->
    {timeout, 300,
     {setup, fun start_peer/0, fun stop_peer/1,
      fun({Peer, _Dir}) ->
          [{atom_to_list(Case),
            {timeout, 180,
             fun() ->
                 ?assertEqual(ok, peer:call(Peer, ?MODULE, Case, [], 170000))
             end}} || Case <- [pagination_case, filters_case,
                               selected_key_filters_case, timestamp_ties_case,
                               count_only_case, bounded_memory_case]]
      end}}.

start_peer() ->
    {ok, CWD} = file:get_cwd(),
    Dir = filename:join(CWD, "mam_mnesia_test_" ++
                        integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    Paths = lists:append([["-pa", filename:absname(Path)] ||
                            Path <- code:get_path()]),
    try start_peer(Dir, Paths)
    catch Class:Reason:Stack ->
        ok = file:del_dir_r(Dir),
        erlang:raise(Class, Reason, Stack)
    end.

start_peer(Dir, Paths) ->
    {ok, Peer, _Node} = peer:start_link(
                        #{connection => standard_io,
                          args => ["+S", "2" | Paths]}),
    try
        ok = peer:call(Peer, ?MODULE, initialize, [Dir], 30000),
        {Peer, Dir}
    catch Class:Reason:Stack ->
        peer:stop(Peer),
        erlang:raise(Class, Reason, Stack)
    end.

stop_peer({Peer, Dir}) ->
    try peer:stop(Peer)
    after
        ok = file:del_dir_r(Dir)
    end.

initialize(Dir) ->
    ok = application:set_env(mnesia, dir, Dir),
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia),
    {atomic, ok} = mnesia:create_table(
                     archive_msg,
                     [{disc_only_copies, [node()]},
                      {type, bag},
                      {attributes, record_info(fields, archive_msg)}]),
    ok = mnesia:wait_for_tables([archive_msg], 10000),
    ok = meck:new(mod_mam, [no_link]),
    mock_conversion().

mock_conversion() ->
    meck:expect(mod_mam, msg_to_el,
                fun(Msg, _Type, _Requestor, _Archive) -> {ok, Msg} end).

pagination_case() ->
    reset([message(ID) || ID <- [110, 10, 101, 9, 100, 99]]),
    check([], false, 6, [], #rsm_set{max = 0}),
    check([9], false, 6, [], #rsm_set{max = 1}),
    check([9, 10, 99, 100, 101], false, 6, [], #rsm_set{max = 5}),
    check([9, 10, 99, 100, 101, 110], true, 6, [], #rsm_set{max = 6}),
    check([9, 10, 99, 100, 101, 110], true, 6, [], #rsm_set{max = 50}),
    check([10], false, 6, [], #rsm_set{'after' = <<"9">>, max = 1}),
    check([99, 100], false, 6, [], #rsm_set{'after' = <<"10">>, max = 2}),
    check([100, 101, 110], true, 6, [],
          #rsm_set{'after' = <<"99">>, max = 50}),
    check([], true, 6, [], #rsm_set{'after' = <<"110">>, max = 0}),
    check([99, 10], false, 6, [], #rsm_set{before = <<"100">>, max = 2}),
    check([9], true, 6, [], #rsm_set{before = <<"10">>, max = 1}),
    check([110, 101], false, 6, [], #rsm_set{before = <<>>, max = 2}),
    check([100], false, 6, [],
          #rsm_set{'after' = <<"99">>, before = <<"100">>, max = 1}),
    check([9, 10, 99, 100, 101, 110], true, 6, [], undefined),
    check([99, 10, 9], true, 6, [], #rsm_set{before = <<"100">>}),
    check([], true, 6, [], #rsm_set{max = -1}),
    ?assertEqual({[9], false, 6},
                 compact(select([], #rsm_set{max = 1}, only_messages))),
    reset([]),
    check([], true, 0, [], #rsm_set{max = 1}),
    ok.

filters_case() ->
    SecondResource = {<<"peer">>, <<"remote.example.test">>, <<"two">>},
    OtherPeer = {<<"other">>, <<"remote.example.test">>, <<"one">>},
    reset([message(4), message(2, ?OWNER, SecondResource),
           message(3, ?OWNER, OtherPeer), message(1),
           message(5, {<<"different">>, ?HOST}, ?PEER)]),
    check([2, 3, 4], true, 3,
          [{start, timestamp(2)}, {'end', timestamp(4)}], #rsm_set{max = 50}),
    check([1, 2, 4], true, 3,
          [{with, make_jid(bare(?PEER))}], #rsm_set{max = 50}),
    check([1, 4], true, 2,
          [{with, make_jid(?PEER)}], #rsm_set{max = 50}),
    check([2], false, 2,
          [{start, timestamp(2)}, {'end', timestamp(4)},
           {with, make_jid(bare(?PEER))}], #rsm_set{max = 1}),
    check([], true, 0,
          [{start, timestamp(4)}, {'end', timestamp(2)}], #rsm_set{max = 50}),
    ok.

selected_key_filters_case() ->
    SecondResource = {<<"peer">>, <<"remote.example.test">>, <<"two">>},
    OtherPeer = {<<"other">>, <<"remote.example.test">>, <<"one">>},
    Allowed = message(10),
    SameBare = message(10, ?OWNER, SecondResource),
    reset([Allowed, SameBare, message(10, ?OWNER, OtherPeer),
           message(10, {<<"different">>, ?HOST}, ?PEER)]),
    {[{<<"10">>, 10, Allowed}], true, 1} =
        select([{with, make_jid(?PEER)}], #rsm_set{max = 50}),
    {Rows, true, 2} = select([{with, make_jid(bare(?PEER))}],
                             #rsm_set{max = 50}),
    ?assertEqual(lists:sort([Allowed, SameBare]),
                 lists:sort([Msg || {_ID, _TS, Msg} <- Rows])),
    ok.

timestamp_ties_case() ->
    First = message(10),
    Second = First#archive_msg{packet = packet(<<"second">>)},
    reset([message(11), First, message(9), Second]),
    {AllRows, true, 4} = select([], undefined),
    [FirstTie, LastTie] = [Row || {<<"10">>, _TS, _Msg} = Row <- AllRows],
    check([9, 10], false, 4, [], #rsm_set{max = 2}),
    {Rows, false, 4} = select([], #rsm_set{'after' = <<"9">>, max = 2}),
    ?assertEqual(lists:sort([First, Second]),
                 lists:sort([Msg || {_ID, _TS, Msg} <- Rows])),
    {[ForwardTie], false, 4} =
        select([], #rsm_set{'after' = <<"9">>, max = 1}),
    ?assertEqual(FirstTie, ForwardTie),
    {[BackwardTie], false, 4} =
        select([], #rsm_set{before = <<"11">>, max = 1}),
    ?assertEqual(LastTie, BackwardTie),
    check([10, 10], false, 4, [], #rsm_set{before = <<"11">>, max = 2}),
    check([11], true, 4, [], #rsm_set{'after' = <<"10">>, max = 50}),
    check([10, 10], true, 2,
          [{start, timestamp(10)}, {'end', timestamp(10)}],
          #rsm_set{max = 50}),
    ok.

count_only_case() ->
    reset([message(ID) || ID <- [1, 2, 3, 4]]),
    ok = meck:expect(mod_mam, msg_to_el,
                     fun(_, _, _, _) -> erlang:error(unexpected_conversion) end),
    try
        ?assertEqual({[], true, 4},
                     select([], #rsm_set{max = 1}, only_count)),
        ?assertEqual({[], true, 2},
                     select([{start, timestamp(2)}, {'end', timestamp(3)}],
                            #rsm_set{'after' = <<"3">>, max = 0}, only_count))
    after
        ok = mock_conversion()
    end,
    ok.

bounded_memory_case() ->
    reset([]),
    Packet = #xmlel{
                name = <<"message">>,
                children = [#xmlel{
                               name = <<"field">>,
                               attrs = [{<<"index">>, integer_to_binary(N)}],
                               children = [#xmlel{
                                              name = <<"value">>,
                                              children = [{xmlcdata, <<"payload">>}]}]}
                            || N <- lists:seq(1, 128)]},
    Messages = [(message(ID))#archive_msg{packet = Packet} ||
                   ID <- lists:seq(1, 5000)],
    %% Seed the backing bag in one batch to avoid repeated large-bucket rewrites.
    ok = dets:insert(archive_msg, Messages),
    ok = dets:sync(archive_msg),
    ?assertEqual({[], false, 5000}, bounded_select(#rsm_set{max = 0}, all)),
    ?assertEqual({[1], false, 5000}, bounded_select(#rsm_set{max = 1}, all)),
    ?assertEqual({[], true, 5000}, bounded_select(#rsm_set{}, only_count)),
    ok.

bounded_select(RSM, Flags) ->
    Parent = self(),
    Tag = make_ref(),
    {Pid, Monitor} = spawn_opt(
                       fun() -> Parent ! {Tag, compact(select([], RSM, Flags))} end,
                       [monitor,
                        {max_heap_size, #{size => 2000000, kill => true,
                                          error_logger => false}}]),
    receive
        {Tag, Result} ->
            receive
                {'DOWN', Monitor, process, Pid, normal} -> Result;
                {'DOWN', Monitor, process, Pid, Reason} ->
                    erlang:error({query_worker_failed, Reason})
            after 5000 ->
                exit(Pid, kill),
                erlang:error(query_worker_did_not_exit)
            end;
        {'DOWN', Monitor, process, Pid, Reason} ->
            erlang:error({query_worker_failed, Reason})
    after 60000 ->
        exit(Pid, kill),
        erlang:error(query_worker_timeout)
    end.

reset(Messages) ->
    {atomic, ok} = mnesia:clear_table(archive_msg),
    lists:foreach(fun mnesia:dirty_write/1, Messages),
    ok = meck:reset(mod_mam).

message(ID) ->
    message(ID, ?OWNER, ?PEER).

message(ID, Owner, Peer) ->
    #archive_msg{us = Owner,
                 id = integer_to_binary(ID),
                 timestamp = timestamp(ID),
                 peer = Peer,
                 bare_peer = bare(Peer),
                 packet = packet(integer_to_binary(ID))}.

packet(Body) ->
    #xmlel{name = <<"message">>, children = [{xmlcdata, Body}]}.

timestamp(ID) ->
    {0, 0, ID}.

bare({User, Server, _Resource}) ->
    {User, Server, <<>>}.

make_jid({User, Server, Resource}) ->
    #jid{user = User, server = Server, resource = Resource,
         luser = User, lserver = Server, lresource = Resource}.

select(Query, RSM) ->
    Archive = make_jid({element(1, ?OWNER), ?HOST, <<>>}),
    mod_mam_mnesia:select(?HOST, Archive, Archive, Query, RSM, chat).

select(Query, RSM, Flags) ->
    Archive = make_jid({element(1, ?OWNER), ?HOST, <<>>}),
    mod_mam_mnesia:select(?HOST, Archive, Archive, Query, RSM, chat, Flags).

compact({Rows, Complete, Count}) ->
    {[binary_to_integer(ID) || {ID, _TS, _Msg} <- Rows], Complete, Count}.

check(IDs, Complete, Count, Query, RSM) ->
    ?assertEqual({IDs, Complete, Count}, compact(select(Query, RSM))).
