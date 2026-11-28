%%% The Matrix gateway's profiles (mod_matrix_gw_profile):
%%% on an ejabberd with the gateway, mod_vcard and mod_http_upload whose
%%% put_url is this suite's own HTTP server, our users' displayname and
%%% avatar_url (the vCard photo put into the store once, an mxc of ours),
%%% and a Matrix user's profile (the cache filled as a /query/profile
%%% answer fills it) as a vCard, a XEP-0153 presence and a name. What the
%%% gateway routes is caught on ejabberd's filter_packet hook.
-module(mod_matrix_gw_profile_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").

-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).
-define(PORT, 18364).
-define(STORE_URL, <<"http://127.0.0.1:18364/upload/">>).
-define(STORE, mod_matrix_gw_profile_tests_store).
-define(CACHE, matrix_gw_profile_cache).
-define(P, mod_matrix_gw_profile).

-export([capture/1]).

parse_profile_test_() ->
    [?_assertEqual(#{<<"displayname">> => <<"Bob">>, <<"avatar_url">> => <<"mxc://s/a">>},
                   ?P:parse_profile(<<"{\"displayname\":\"Bob\",\"avatar_url\":\"mxc://s/a\",\"x\":1}">>)),
     ?_assertEqual(#{}, ?P:parse_profile(<<"{\"displayname\":null,\"avatar_url\":\"\"}">>)),
     ?_assertEqual(#{}, ?P:parse_profile(<<"[1]">>)),
     ?_assertEqual(#{}, ?P:parse_profile(<<"not json">>))].

vcard_el_test_() ->
    V = ?P:vcard_el(<<"@bob:s">>, <<"Bob">>, {<<"image/png">>, <<"PNG">>}),
    V2 = ?P:vcard_el(<<"@bob:s">>, <<>>, none),
    [?_assertEqual([<<"Bob">>, <<"Bob">>, <<"image/png">>, base64:encode(<<"PNG">>)],
                   [fxml:get_path_s(V, P) || P <- [[{elem, <<"FN">>}, cdata], [{elem, <<"NICKNAME">>}, cdata],
                                                   [{elem, <<"PHOTO">>}, {elem, <<"TYPE">>}, cdata],
                                                   [{elem, <<"PHOTO">>}, {elem, <<"BINVAL">>}, cdata]]]),
     ?_assertEqual(<<"@bob:s">>, fxml:get_path_s(V2, [{elem, <<"FN">>}, cdata])),
     ?_assertEqual(<<>>, fxml:get_path_s(V2, [{elem, <<"PHOTO">>}, {elem, <<"TYPE">>}, cdata])),
     ?_assertEqual(<<"h">>, fxml:get_path_s(?P:xupdate_el(<<"h">>), [{elem, <<"photo">>}, cdata])),
     ?_assertMatch(#xmlel{children = []}, ?P:xupdate_el(undefined))].

%%--------------------------------------------------------------------
%% On an ejabberd with the gateway, mod_vcard and the store
%%--------------------------------------------------------------------

ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup,
     fun() -> Store = store_start(), {mod_matrix_gw_test_lib:start_ejabberd(?MODULE), Store} end,
     fun({Dir, Store}) -> exit(Store, kill), mod_matrix_gw_test_lib:stop_ejabberd(Dir) end,
     fun(_) ->
             [{timeout, 60, {Name, F}}
              || {Name, F} <- [{"our user's displayname and avatar_url", fun local/0},
                               {"query/profile through the module", fun query_profile/0},
                               {"a join's member content", fun member/0},
                               {"a Matrix user's vCard", fun remote_vcard/0},
                               {"a Matrix user's presence: subscribe, probe", fun presence/0},
                               {"a Matrix user's name from the cache", fun names/0},
                               {"a profile asked of the user's server", fun fetch/0},
                               {"what is not ours to answer", fun refused/0}]]
     end}.

store_start() ->
    Parent = self(),
    Pid = spawn(fun() ->
                        ets:new(?STORE, [named_table, public]),
                        {ok, L} = gen_tcp:listen(?PORT, [binary, {packet, http_bin}, {active, false},
                                                         {reuseaddr, true}, {ip, {127, 0, 0, 1}}]),
                        Parent ! ready,
                        store_loop(L)
                end),
    receive ready -> Pid after 5000 -> error(no_store) end.

store_loop(L) ->
    {ok, S} = gen_tcp:accept(L),
    catch store_serve(S),
    gen_tcp:close(S),
    store_loop(L).

store_serve(S) ->
    {ok, {http_request, Method, {abs_path, Path}, _}} = gen_tcp:recv(S, 0, 5000),
    Headers = store_headers(S, []),
    Len = binary_to_integer(proplists:get_value('Content-Length', Headers, <<"0">>)),
    ok = inet:setopts(S, [{packet, raw}]),
    Body = case Len of
               0 -> <<>>;
               _ -> {ok, B} = gen_tcp:recv(S, Len, 5000), B
           end,
    Key = hd(binary:split(Path, <<"?">>)),
    Reply = fun(Code, Data) ->
                    gen_tcp:send(S, [<<"HTTP/1.1 ">>, integer_to_binary(Code), <<" X\r\nContent-Length: ">>,
                                     integer_to_binary(byte_size(Data)),
                                     <<"\r\nContent-Type: image/png\r\nConnection: close\r\n\r\n">>, Data])
            end,
    case {Method, Key} of
        {'PUT', _} ->
            ets:insert(?STORE, {Key, Body}),
            Reply(200, <<>>);
        {'GET', _} ->
            case ets:lookup(?STORE, Key) of
                [{_, Data}] -> Reply(200, Data);
                [] -> Reply(404, <<"no">>)
            end
    end.

store_headers(S, Acc) ->
    case gen_tcp:recv(S, 0, 5000) of
        {ok, {http_header, _, Name, _, Value}} -> store_headers(S, [{Name, Value} | Acc]);
        {ok, http_eoh} -> Acc
    end.

reg_user(User) ->
    case ejabberd_auth:try_register(User, ?HOST, <<"secret">>) of
        ok -> ok;
        {error, exists} -> ok
    end.

set_vcard(User, Nick, Photo) ->
    ok = mod_vcard:set_vcard(User, ?HOST, #vcard_temp{nickname = Nick, photo = Photo}).

local() ->
    reg_user(<<"ann">>),
    %% nothing in the vCard: nothing to say
    ?assertEqual(#{}, ?P:local_profile(<<"ann">>, ?HOST, block)),
    set_vcard(<<"ann">>, <<"Ann">>, #vcard_photo{type = <<"image/jpeg">>, binval = <<"JPEG1">>}),
    #{<<"displayname">> := <<"Ann">>, <<"avatar_url">> := <<"mxc://example.com/", _/binary>> = MXC} =
        ?P:local_profile(<<"ann">>, ?HOST, block),
    %% the picture is in the store, named by our mxc
    {ok, #{url := URL, mime := <<"image/jpeg">>, size := 5}} = mod_matrix_gw_media:resolve(?HOST, MXC, #{}),
    #{path := Path} = uri_string:parse(URL),
    ?assertEqual([{Path, <<"JPEG1">>}], ets:lookup(?STORE, Path)),
    ?assertMatch(<<_:40/binary, "/", _:40/binary, "/avatar.jpg">>, lists:last(binary:split(Path, <<"/upload/">>))),
    %% the same picture: the same mxc, nothing put again
    N = ets:info(?STORE, size),
    ?assertEqual(MXC, ?P:local_avatar(<<"ann">>, ?HOST, cached)),
    ?assertEqual(N, ets:info(?STORE, size)),
    %% a new one: cached says none yet and makes it meanwhile
    set_vcard(<<"ann">>, <<"Ann">>, #vcard_photo{binval = <<"PNG2">>}),
    ?assertEqual(<<>>, ?P:local_avatar(<<"ann">>, ?HOST, cached)),
    New = wait(fun() -> ?P:local_avatar(<<"ann">>, ?HOST, cached) end),
    ?assertNotEqual(MXC, New),
    ?assertMatch({ok, #{mime := <<"image/png">>}}, mod_matrix_gw_media:resolve(?HOST, New, #{})),
    %% FN when there is no NICKNAME; a photo by URL is none of ours
    reg_user(<<"cid">>),
    ok = mod_vcard:set_vcard(<<"cid">>, ?HOST,
                             #vcard_temp{fn = <<"Cid C">>, photo = #vcard_photo{extval = <<"https://x/a.png">>}}),
    ?assertEqual(#{<<"displayname">> => <<"Cid C">>}, ?P:local_profile(<<"cid">>, ?HOST, block)),
    %% gif and webp keep their names; the store refusing: no avatar
    reg_user(<<"dee">>),
    set_vcard(<<"dee">>, <<>>, #vcard_photo{type = <<"image/gif">>, binval = <<"GIF">>}),
    ?assertMatch(<<"mxc://", _/binary>>, ?P:local_avatar(<<"dee">>, ?HOST, block)),
    set_vcard(<<"dee">>, <<>>, #vcard_photo{type = <<"image/webp">>, binval = <<"WEBP">>}),
    ?assertMatch(<<"mxc://", _/binary>>, ?P:local_avatar(<<"dee">>, ?HOST, block)),
    set_vcard(<<"dee">>, <<>>, #vcard_photo{type = <<"image/png">>, binval = binary:copy(<<"x">>, 1001)}),
    ?assertEqual(<<>>, ?P:local_avatar(<<"dee">>, ?HOST, block)).

wait(F) -> wait(F, 50).
wait(F, 0) -> F();
wait(F, N) ->
    case F() of
        <<>> -> timer:sleep(100), wait(F, N - 1);
        V -> V
    end.

query_profile() ->
    Req = fun(Q) -> mod_matrix_gw:process([<<"federation">>, <<"v1">>, <<"query">>, <<"profile">>],
                                          #request{method = 'GET', path = [], q = Q, headers = [],
                                                   raw_path = <<>>, length = 0, data = <<>>,
                                                   host = ?HOST, ip = {{127, 0, 0, 1}, 1}}) end,
    %% unsigned: refused before anything is read
    ?assertMatch({401, _, _}, Req([{<<"user_id">>, <<"@ann:example.com">>}, {<<"field">>, <<"avatar_url">>}])).

member() ->
    C = #{<<"membership">> => <<"join">>},
    ?assertMatch(#{<<"membership">> := <<"join">>, <<"displayname">> := <<"Ann">>,
                   <<"avatar_url">> := <<"mxc://", _/binary>>},
                 ?P:member_content(?HOST, <<"@ann:example.com">>, C)),
    ?assertEqual(C, ?P:member_content(?HOST, <<"@bob:far.example">>, C)),
    ?assertEqual(C, ?P:member_content(?HOST, <<"not an id">>, C)).

%% What ejabberd routes, to the test process registered as capture.
capture(Pkt) ->
    catch (mod_matrix_gw_profile_tests_capture ! {captured, Pkt}),
    Pkt.

caught(Match) ->
    receive
        {captured, Pkt} ->
            case Match(Pkt) of
                true -> Pkt;
                false -> caught(Match)
            end
    after 5000 -> timeout
    end.

with_capture(F) ->
    register(mod_matrix_gw_profile_tests_capture, self()),
    ejabberd_hooks:add(filter_packet, ?MODULE, capture, 1),
    try F()
    after
        ejabberd_hooks:delete(filter_packet, ?MODULE, capture, 1),
        unregister(mod_matrix_gw_profile_tests_capture)
    end.

bob_cached() ->
    %% a remote avatar fetched before (the media module's table) and the
    %% profile as /query/profile gave it
    ets:insert(?STORE, {<<"/upload/far/bob.png">>, <<"BOBPNG">>}),
    mnesia:dirty_write({matrix_media_remote, {<<"far.example">>, <<"bobav">>},
                        <<?STORE_URL/binary, "far/bob.png">>, <<"bob.png">>, <<"image/png">>, 6, 0}),
    ets:insert(?CACHE, {<<"@bob:far.example">>, erlang:system_time(millisecond),
                        #{<<"displayname">> => <<"Bob B">>, <<"avatar_url">> => <<"mxc://far.example/bobav">>},
                        undefined}).

ann() -> jid:decode(<<"ann@example.com/phone">>).
bob() -> jid:make(<<"bob%far.example">>, ?GW).

remote_vcard() ->
    bob_cached(),
    with_capture(
      fun() ->
              ejabberd_router:route(#iq{type = get, id = <<"v1">>, from = ann(), to = bob(),
                                        sub_els = [#xmlel{name = <<"vCard">>,
                                                          attrs = [{<<"xmlns">>, <<"vcard-temp">>}]}]}),
              #iq{type = result, sub_els = [V]} =
                  caught(fun(#iq{id = <<"v1">>, type = T}) -> T == result; (_) -> false end),
              ?assertEqual([<<"Bob B">>, <<"image/png">>, base64:encode(<<"BOBPNG">>)],
                           [fxml:get_path_s(V, P) || P <- [[{elem, <<"FN">>}, cdata],
                                                          [{elem, <<"PHOTO">>}, {elem, <<"TYPE">>}, cdata],
                                                          [{elem, <<"PHOTO">>}, {elem, <<"BINVAL">>}, cdata]]]),
              %% the decoded form too
              ejabberd_router:route(#iq{type = get, id = <<"v2">>, from = ann(), to = bob(),
                                        sub_els = [#vcard_temp{}]}),
              ?assertMatch(#iq{type = result},
                           caught(fun(#iq{id = <<"v2">>, type = T}) -> T == result; (_) -> false end)),
              %% a user whose server cannot be reached: an empty vCard
              ejabberd_router:route(#iq{type = get, id = <<"v3">>, from = ann(),
                                        to = jid:make(<<"x%127.0.0.1\\3a1">>, ?GW),
                                        sub_els = [#vcard_temp{}]}),
              #iq{type = result, sub_els = [Empty]} =
                  caught(fun(#iq{id = <<"v3">>, type = T}) -> T == result; (_) -> false end),
              ?assertMatch(#xmlel{name = <<"vCard">>, children = []}, Empty),
              %% an avatar that cannot be had: the vCard without it
              ets:insert(?CACHE, {<<"@eve:far.example">>, erlang:system_time(millisecond),
                                  #{<<"displayname">> => <<"Eve">>, <<"avatar_url">> => <<"mxc://far.example/none">>},
                                  undefined}),
              mnesia:dirty_write({matrix_media_remote, {<<"far.example">>, <<"none">>},
                                  <<?STORE_URL/binary, "far/missing.png">>, <<"m.png">>, <<"image/png">>, 6, 0}),
              ejabberd_router:route(#iq{type = get, id = <<"v4">>, from = ann(),
                                        to = jid:make(<<"eve%far.example">>, ?GW), sub_els = [#vcard_temp{}]}),
              #iq{type = result, sub_els = [EveV]} =
                  caught(fun(#iq{id = <<"v4">>, type = T}) -> T == result; (_) -> false end),
              ?assertEqual([<<"Eve">>, <<>>], [fxml:get_path_s(EveV, [{elem, <<"FN">>}, cdata]),
                                               fxml:get_path_s(EveV, [{elem, <<"PHOTO">>}, {elem, <<"TYPE">>}, cdata])])
      end).

presence() ->
    bob_cached(),
    with_capture(
      fun() ->
              Ann = ann(),
              ejabberd_router:route(#presence{type = subscribe, from = Ann, to = bob()}),
              ?assertMatch(#presence{type = subscribed},
                           caught(fun(#presence{type = T, from = F}) -> T == subscribed andalso F == bob();
                                     (_) -> false end)),
              ejabberd_router:route(#presence{type = probe, from = Ann, to = bob()}),
              #presence{type = unavailable, to = Ann, sub_els = [X, Nick]} =
                  caught(fun(#presence{type = T, to = To}) -> T == unavailable andalso To == Ann;
                            (_) -> false end),
              ?assertEqual(str:to_hexlist(crypto:hash(sha, <<"BOBPNG">>)),
                           fxml:get_path_s(X, [{elem, <<"photo">>}, cdata])),
              ?assertEqual(<<"Bob B">>, fxml:get_tag_cdata(Nick)),
              %% the hash is kept: the next probe has it at once
              ejabberd_router:route(#presence{type = probe, from = Ann, to = bob()}),
              ?assertMatch(#presence{sub_els = [_, _]},
                           caught(fun(#presence{type = T, to = To}) -> T == unavailable andalso To == Ann;
                                     (_) -> false end)),
              ejabberd_router:route(#presence{type = unsubscribe, from = Ann, to = bob()}),
              ?assertMatch(#presence{type = unsubscribed},
                           caught(fun(#presence{type = T}) -> T == unsubscribed; (_) -> false end)),
              %% a user with no avatar: an empty photo; an unreachable one: no <x/>
              ets:insert(?CACHE, {<<"@nopic:far.example">>, erlang:system_time(millisecond),
                                  #{<<"displayname">> => <<"N">>}, undefined}),
              ejabberd_router:route(#presence{type = probe, from = Ann,
                                              to = jid:make(<<"nopic%far.example">>, ?GW)}),
              #presence{sub_els = [X2, _]} =
                  caught(fun(#presence{type = T, to = To}) -> T == unavailable andalso To == Ann;
                            (_) -> false end),
              ?assertMatch(#xmlel{children = [#xmlel{name = <<"photo">>, children = []}]}, X2),
              ejabberd_router:route(#presence{type = probe, from = Ann, to = jid:make(<<"y%127.0.0.1\\3a1">>, ?GW)}),
              ?assertMatch(#presence{sub_els = []},
                           caught(fun(#presence{type = T, to = To}) -> T == unavailable andalso To == Ann;
                                     (_) -> false end)),
              %% available presence (a directed one) says nothing to Matrix
              ?assertEqual(ok, ?P:route_presence(#presence{type = available, from = Ann, to = bob()}))
      end).

names() ->
    bob_cached(),
    ?assertEqual(<<"Bob B">>, ?P:cached_name(?HOST, <<"@bob:far.example">>)),
    ?assertEqual({ok, #{<<"displayname">> => <<"Bob B">>, <<"avatar_url">> => <<"mxc://far.example/bobav">>}},
                 ?P:remote_profile(?HOST, <<"@bob:far.example">>)),
    %% stale: still the name, asked again behind
    ets:insert(?CACHE, {<<"@old:127.0.0.1:1">>, 0, #{<<"displayname">> => <<"Old">>}, undefined}),
    ?assertEqual(<<"Old">>, ?P:cached_name(?HOST, <<"@old:127.0.0.1:1">>)),
    ?assertEqual(error, ?P:remote_profile(?HOST, <<"@old:127.0.0.1:1">>)),
    %% unknown: nothing now, asked behind
    ?assertEqual(<<>>, ?P:cached_name(?HOST, <<"@new:127.0.0.1:1">>)),
    ?assertEqual(error, ?P:remote_profile(?HOST, <<"not an id">>)).

refused() ->
    Ann = ann(),
    with_capture(
      fun() ->
              %% a user of another server, a JID that is no Matrix user, a set
              ?P:route_iq(#iq{type = get, id = <<"r1">>, from = jid:decode(<<"x@elsewhere.org/r">>), to = bob(),
                              sub_els = [#vcard_temp{}]}),
              ?assertMatch(#iq{type = error}, caught(fun(#iq{id = I}) -> I == <<"r1">>; (_) -> false end)),
              ?P:route_iq(#iq{type = get, id = <<"r2">>, from = Ann, to = jid:make(<<"nopercent">>, ?GW),
                              sub_els = [#vcard_temp{}]}),
              ?assertMatch(#iq{type = error}, caught(fun(#iq{id = I}) -> I == <<"r2">>; (_) -> false end)),
              ?P:route_iq(#iq{type = set, id = <<"r3">>, from = Ann, to = bob(), sub_els = [#vcard_temp{}]}),
              ?assertMatch(#iq{type = error}, caught(fun(#iq{id = I}) -> I == <<"r3">>; (_) -> false end)),
              ?assertEqual(ok, ?P:route_presence(#presence{type = subscribe, from = Ann,
                                                           to = jid:make(<<"x">>, <<"elsewhere.org">>)})),
              ?assertEqual(ok, ?P:route_presence(#presence{type = subscribe, from = Ann,
                                                           to = jid:make(?GW)}))
      end).

fetch() ->
    Answer = fun(Body) -> fun(<<"far.example">>) -> {ok, {{"HTTP/1.1", 200, "OK"}, [], Body}} end end,
    MXID = <<"@fay:far.example">>,
    ?assertEqual({ok, #{<<"displayname">> => <<"Fay">>, <<"avatar_url">> => <<"mxc://far.example/f1">>}},
                 ?P:fetch_profile_with(MXID, Answer(<<"{\"displayname\":\"Fay\",\"avatar_url\":\"mxc://far.example/f1\"}">>))),
    ?assertEqual(<<"Fay">>, ?P:cached_name(?HOST, MXID)),
    %% the avatar's hash kept while the avatar stays, dropped when it changes
    [{_, _, P1, _}] = ets:lookup(?CACHE, MXID),
    ets:insert(?CACHE, {MXID, 0, P1, <<"hash1">>}),
    {ok, _} = ?P:fetch_profile_with(MXID, Answer(<<"{\"displayname\":\"Fay F\",\"avatar_url\":\"mxc://far.example/f1\"}">>)),
    ?assertMatch([{_, _, #{<<"displayname">> := <<"Fay F">>}, <<"hash1">>}], ets:lookup(?CACHE, MXID)),
    {ok, _} = ?P:fetch_profile_with(MXID, Answer(<<"{\"avatar_url\":\"mxc://far.example/f2\"}">>)),
    ?assertMatch([{_, _, _, undefined}], ets:lookup(?CACHE, MXID)),
    %% an error answer, or no answer
    ?assertEqual(error, ?P:fetch_profile_with(MXID, fun(_) -> {ok, {{"HTTP/1.1", 404, "x"}, [], <<"{}">>}} end)),
    ?assertEqual(error, ?P:fetch_profile_with(MXID, fun(_) -> {error, timeout} end)),
    ?assertEqual(error, ?P:fetch_profile_with(<<"nocolon">>, fun(_) -> error(called) end)),
    %% a user never fetched whose server fails: an empty profile, kept a while
    ?assertEqual(error, ?P:fetch_profile_with(<<"@gus:far.example">>, fun(_) -> {error, timeout} end)),
    ?assertMatch([{_, _, #{}, undefined}], ets:lookup(?CACHE, <<"@gus:far.example">>)),
    ?assertEqual({ok, #{}}, ?P:remote_profile(?HOST, <<"@gus:far.example">>)).
