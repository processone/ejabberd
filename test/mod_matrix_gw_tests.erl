%%% The Matrix gateway's tests: its own files on their own
%%% (mod_matrix_gw_http's discovery and TLS options, mod_matrix_gw_xmpp's
%%% translations, mod_matrix_gw_store), then the gateway's pieces of
%%% the older files on an ejabberd started in this node with the gateway on:
%%% the options, the federation answers that need no other server, a room
%%% brought back from the store.
-module(mod_matrix_gw_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").
-include("mod_matrix_gw.hrl").

-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).

el(Name, NS, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, NS} | Attrs], children = Children}.

msg(Body, Els) ->
    #message{id = <<"m1">>, type = chat, body = [#text{data = Body} || Body /= <<>>],
             sub_els = Els}.

%%--------------------------------------------------------------------
%% mod_matrix_gw_http
%%--------------------------------------------------------------------

server_name_test_() ->
    P = fun mod_matrix_gw_http:parse_server_name/1,
    [?_assertEqual({<<"example.org">>, default}, P(<<"example.org">>)),
     ?_assertEqual({<<"example.org">>, 8449}, P(<<"example.org:8449">>)),
     ?_assertEqual({<<"1.2.3.4">>, 8448}, P(<<"1.2.3.4:8448">>)),
     ?_assertEqual({<<"[::1]">>, default}, P(<<"[::1]">>)),
     ?_assertEqual({<<"[::1]">>, 443}, P(<<"[::1]:443">>)),
     ?_assertEqual(error, P(<<"[::1">>)),
     ?_assertEqual(error, P(<<"[::1]x">>)),
     ?_assertEqual(error, P(<<"example.org:0">>)),
     ?_assertEqual(error, P(<<"example.org:70000">>)),
     ?_assertEqual(error, P(<<"example.org:port">>)),
     ?_assertEqual(error, P(<<":8448">>)),
     ?_assertEqual(error, P(<<>>)),
     ?_assertEqual(error, P(<<"a:b:c">>))].

well_known_test_() ->
    W = fun mod_matrix_gw_http:parse_well_known/1,
    [?_assertEqual(<<"fed.example.org:443">>, W(<<"{\"m.server\": \"fed.example.org:443\"}">>)),
     ?_assertEqual(error, W(<<"{\"m.server\": \"\"}">>)),
     ?_assertEqual(error, W(<<"{\"m.server\": \"bad:port\"}">>)),
     ?_assertEqual(error, W(<<"{\"other\": 1}">>)),
     ?_assertEqual(error, W(<<"[1]">>)),
     ?_assertEqual(error, W(<<"not json">>))].

resolve_test_() ->
    NoWK = fun(_) -> error end,
    NoSrv = fun(_) -> [] end,
    WK = fun(Body) -> fun(<<"example.org">>) -> Body; (_) -> error end end,
    Srv = fun(Records) -> fun(Name) -> proplists:get_value(Name, Records, []) end end,
    R = fun mod_matrix_gw_http:resolve/3,
    [%% an explicit port or an address: as it is, no lookup
     ?_assertEqual(#{host => <<"example.org">>, port => 8449, name => <<"example.org">>},
                   R(<<"example.org:8449">>, NoWK, NoSrv)),
     ?_assertEqual(#{host => <<"10.0.0.1">>, port => 8448, name => <<"10.0.0.1">>},
                   R(<<"10.0.0.1">>, NoWK, NoSrv)),
     ?_assertEqual(#{host => <<"[::1]">>, port => 8448, name => <<"[::1]">>},
                   R(<<"[::1]">>, NoWK, NoSrv)),
     ?_assertEqual(error, R(<<"bad:name:x">>, NoWK, NoSrv)),
     %% nothing anywhere: port 8448
     ?_assertEqual(#{host => <<"example.org">>, port => 8448, name => <<"example.org">>},
                   R(<<"example.org">>, NoWK, NoSrv)),
     %% .well-known with a port, with an address, with a name and SRV
     ?_assertEqual(#{host => <<"fed.example.org">>, port => 443, name => <<"fed.example.org">>},
                   R(<<"example.org">>, WK(<<"{\"m.server\":\"fed.example.org:443\"}">>), NoSrv)),
     ?_assertEqual(#{host => <<"10.0.0.2">>, port => 8448, name => <<"10.0.0.2">>},
                   R(<<"example.org">>, WK(<<"{\"m.server\":\"10.0.0.2\"}">>), NoSrv)),
     ?_assertEqual(#{host => <<"srv.example.org">>, port => 8000, name => <<"fed.example.org">>},
                   R(<<"example.org">>, WK(<<"{\"m.server\":\"fed.example.org\"}">>),
                     Srv([{"_matrix-fed._tcp.fed.example.org", [{10, 5, 8000, "srv.example.org"}]}]))),
     %% a broken .well-known is no delegation
     ?_assertEqual(#{host => <<"example.org">>, port => 8448, name => <<"example.org">>},
                   R(<<"example.org">>, WK(<<"{}">>), NoSrv)),
     %% SRV: _matrix-fed first, the lowest priority then the highest
     %% weight; "." is no service; _matrix as the fallback
     ?_assertEqual(#{host => <<"b.example.org">>, port => 8002, name => <<"example.org">>},
                   R(<<"example.org">>, NoWK,
                     Srv([{"_matrix-fed._tcp.example.org",
                           [{20, 0, 8001, "a.example.org"}, {10, 1, 8003, "c.example.org"},
                            {10, 9, 8002, "b.example.org"}, {0, 0, 1, "."}]},
                          {"_matrix._tcp.example.org", [{0, 0, 9000, "old.example.org"}]}]))),
     ?_assertEqual(#{host => <<"old.example.org">>, port => 9000, name => <<"example.org">>},
                   R(<<"example.org">>, NoWK,
                     Srv([{"_matrix._tcp.example.org", [{0, 0, 9000, "old.example.org"}]}])))].

%% what a URL a remote server names may be.
safe_url_test_() ->
    R = fun("cdn.example") -> [{93, 184, 216, 34}];
           ("both.example") -> [{16#2606, 16#2800, 16#220, 1, 16#248, 16#1893, 16#25c8, 16#1946}, {93, 184, 216, 34}, {16#2606, 16#2800, 16#220, 1, 16#248, 16#1893, 16#25c8, 16#1946}];
           ("v6.example") -> [{16#2606, 16#2800, 16#220, 1, 16#248, 16#1893, 16#25c8, 16#1946}];
           ("lan.example") -> [{192, 168, 0, 10}];
           ("crash.example") -> error(nxdomain);
           (_) -> []
        end,
    S = fun(U) -> mod_matrix_gw_http:safe_url(U, R) end,
    P = fun mod_matrix_gw_http:public_address/1,
    [?_assertEqual({ok, #{url => "https://93.184.216.34/a/b?c=d", name => <<"cdn.example">>,
                          host_header => "cdn.example"}}, S(<<"https://cdn.example/a/b?c=d#frag">>)),
     ?_assertEqual({ok, #{url => "https://93.184.216.34:8443/x", name => <<"cdn.example">>,
                          host_header => "cdn.example:8443"}}, S(<<"HTTPS://CDN.example:8443/x">>)),
     ?_assertMatch({ok, #{url := "https://93.184.216.34/x"}}, S(<<"https://both.example/x">>)),
     ?_assertMatch({ok, #{url := "https://[2606:2800:220:1:248:1893:25c8:1946]/x"}}, S(<<"https://v6.example/x">>)),
     ?_assertMatch({ok, #{url := "https://[2606:2800:220:1:248:1893:25c8:1946]/", name := <<"2606:2800:220:1:248:1893:25c8:1946">>,
                          host_header := "[2606:2800:220:1:248:1893:25c8:1946]"}},
                   S(<<"https://[2606:2800:220:1:248:1893:25c8:1946]/">>)),
     ?_assertEqual({ok, #{url => "https://8.8.8.8/", name => <<"8.8.8.8">>, host_header => "8.8.8.8"}},
                   S(<<"https://8.8.8.8/">>)),
     ?_assertEqual({error, not_https}, S(<<"http://cdn.example/">>)),
     ?_assertEqual({error, not_https}, S(<<"ftp://cdn.example/">>)),
     ?_assertEqual({error, bad_url}, S(<<"https://u:p@cdn.example/">>)),
     ?_assertEqual({error, bad_url}, S(<<"no url">>)),
     ?_assertEqual({error, bad_url}, S(<<"https://a b/">>)),
     ?_assertEqual({error, bad_url}, S(not_a_binary)),
     ?_assertEqual({error, unresolved}, S(<<"https://nowhere.example/">>)),
     ?_assertEqual({error, unresolved}, S(<<"https://crash.example/">>)),
     ?_assertEqual({error, {private, "192.168.0.10"}}, S(<<"https://lan.example/">>)),
     ?_assertEqual({error, {private, "127.0.0.1"}}, S(<<"https://127.0.0.1:5280/api/x">>)),
     ?_assertEqual({error, {private, "::1"}}, S(<<"https://[::1]/">>)),
     ?_assertMatch({error, {private, _}}, S(<<"https://169.254.169.254/latest">>)),
     ?_assertEqual({error, unresolved}, mod_matrix_gw_http:safe_url(<<"https://nothing.invalid/">>)),
     ?_assertMatch({error, {private, _}}, mod_matrix_gw_http:safe_url(<<"https://localhost/">>)),
     ?_assert(lists:all(P, [{93, 184, 216, 34}, {8, 8, 8, 8}, {100, 63, 0, 1}, {100, 128, 0, 1},
                            {172, 15, 0, 1}, {172, 32, 0, 1}, {192, 0, 1, 1}, {198, 20, 0, 1},
                            {2606, 0, 0, 0, 0, 0, 0, 1}, {16#2001, 16#4860, 0, 0, 0, 0, 0, 16#8888},
                            {0, 0, 0, 0, 0, 16#ffff, 16#5db8, 16#d822}])),
     ?_assertNot(lists:any(P, [{0, 0, 0, 0}, {10, 1, 2, 3}, {127, 0, 0, 1}, {100, 64, 0, 1},
                               {169, 254, 169, 254}, {172, 16, 0, 1}, {172, 31, 255, 255},
                               {192, 168, 1, 1}, {192, 0, 0, 8}, {192, 0, 2, 1}, {198, 18, 0, 1},
                               {198, 51, 100, 1}, {203, 0, 113, 1}, {224, 0, 0, 1}, {240, 0, 0, 1},
                               {255, 255, 255, 255},
                               {0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 1},
                               {0, 0, 0, 0, 0, 0, 16#7f00, 1},
                               {0, 0, 0, 0, 0, 16#ffff, 16#7f00, 1}, {0, 0, 0, 0, 0, 16#ffff, 16#a9fe, 16#a9fe},
                               {16#64, 16#ff9b, 0, 0, 0, 0, 16#a00, 1}, {16#2002, 16#c0a8, 1, 0, 0, 0, 0, 1},
                               {16#2001, 0, 0, 0, 0, 0, 0, 1}, {16#2001, 16#db8, 0, 0, 0, 0, 0, 1},
                               {16#100, 0, 0, 0, 0, 0, 0, 1},
                               {16#fc00, 0, 0, 0, 0, 0, 0, 1}, {16#fd12, 0, 0, 0, 0, 0, 0, 1},
                               {16#fe80, 0, 0, 0, 0, 0, 0, 1}, {16#fec0, 0, 0, 0, 0, 0, 0, 1},
                               {16#ff02, 0, 0, 0, 0, 0, 0, 1}, not_an_address]))].

tls_options_test_() ->
    Opts = mod_matrix_gw_http:tls_options(<<"example.org">>, [<<"der">>]),
    IPOpts = mod_matrix_gw_http:tls_options(<<"[::1]">>, []),
    HTTP = mod_matrix_gw_http:http_options([{timeout, 5000}, {ssl, [old]}],
                                           #{host => <<"h">>, port => 1, name => <<"n.example">>}, []),
    [?_assertEqual(verify_peer, proplists:get_value(verify, Opts)),
     ?_assertEqual([<<"der">>], proplists:get_value(cacerts, Opts)),
     ?_assertEqual("example.org", proplists:get_value(server_name_indication, Opts)),
     ?_assertMatch([{match_fun, F}] when is_function(F), proplists:get_value(customize_hostname_check, Opts)),
     ?_assertEqual(disable, proplists:get_value(server_name_indication, IPOpts)),
     ?_assertEqual(5000, proplists:get_value(timeout, HTTP)),
     ?_assertEqual("n.example", proplists:get_value(server_name_indication,
                                                   proplists:get_value(ssl, HTTP))),
     ?_assertEqual(1, length([S || {ssl, _} = S <- HTTP]))].

cacerts_test() ->
    #{cert := DER} = public_key:pkix_test_root_cert("mod_matrix_gw_tests CA", []),
    File = "/tmp/mod_matrix_gw_tests-ca.pem",
    ok = file:write_file(File, public_key:pem_encode([{'Certificate', DER, not_encrypted}])),
    ?assertEqual([DER], mod_matrix_gw_http:cacerts(list_to_binary(File))),
    ?assert(is_list(mod_matrix_gw_http:cacerts(undefined))),
    ?assert(is_list(mod_matrix_gw_http:cacerts(<<>>))).

answers_test() ->
    {404, [{<<"Content-Type">>, _}], Body} =
        mod_matrix_gw_http:error(404, <<"M_NOT_FOUND">>, <<"no">>),
    ?assertEqual(#{<<"errcode">> => <<"M_NOT_FOUND">>, <<"error">> => <<"no">>},
                 misc:json_decode(Body)),
    {200, _, B2} = mod_matrix_gw_http:json(200, #{<<"a">> => 1}),
    ?assertEqual(#{<<"a">> => 1}, misc:json_decode(B2)).

%%--------------------------------------------------------------------
%% mod_matrix_gw_xmpp
%%--------------------------------------------------------------------

classify_test_() ->
    C = fun mod_matrix_gw_xmpp:classify/1,
    Replace = el(<<"replace">>, <<"urn:xmpp:message-correct:0">>, [{<<"id">>, <<"m0">>}], []),
    Reactions = el(<<"reactions">>, <<"urn:xmpp:reactions:0">>, [{<<"id">>, <<"m0">>}],
                   [#xmlel{name = <<"reaction">>, children = [{xmlcdata, <<"👍"/utf8>>}]},
                    #xmlel{name = <<"reaction">>, children = []}]),
    State = fun(S) -> el(S, <<"http://jabber.org/protocol/chatstates">>, [], []) end,
    Displayed = el(<<"displayed">>, <<"urn:xmpp:chat-markers:0">>, [{<<"id">>, <<"$e">>}], []),
    Other = el(<<"origin-id">>, <<"urn:xmpp:sid:0">>, [], []),
    [?_assertEqual([{text, <<"hi">>}], C(msg(<<"hi">>, [Other, <<"junk">>]))),
     ?_assertEqual([{edit, <<"m0">>, <<"hi!">>}], C(msg(<<"hi!">>, [Replace]))),
     ?_assertEqual([{reactions, <<"m0">>, [<<"👍"/utf8>>]}], C(msg(<<>>, [Reactions]))),
     ?_assertEqual([{text, <<"x">>}, {typing, false}], C(msg(<<"x">>, [State(<<"active">>)]))),
     ?_assertEqual([{typing, true}], C(msg(<<>>, [State(<<"composing">>)]))),
     ?_assertEqual([{typing, false}], C(msg(<<>>, [State(<<"paused">>)]))),
     ?_assertEqual([{typing, true}], C(msg(<<>>, [#chatstate{type = composing}]))),
     ?_assertEqual([{typing, false}], C(msg(<<>>, [#chatstate{type = gone}]))),
     ?_assertEqual([{displayed, <<"$e">>}], C(msg(<<>>, [Displayed]))),
     ?_assertEqual([], C(msg(<<>>, [el(<<"replace">>, <<"other">>, [], [])])))].

content_test_() ->
    X = <<"net.process-one.xmpp-id">>,
    [?_assertEqual(#{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"b">>, X => <<"i">>},
                   mod_matrix_gw_xmpp:text_content(<<"b">>, <<"i">>)),
     ?_assertEqual(#{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"b">>},
                   mod_matrix_gw_xmpp:text_content(<<"b">>, <<>>)),
     ?_assertMatch(#{<<"body">> := <<"* b">>,
                     <<"m.new_content">> := #{<<"body">> := <<"b">>},
                     <<"m.relates_to">> := #{<<"rel_type">> := <<"m.replace">>, <<"event_id">> := <<"$t">>}},
                   mod_matrix_gw_xmpp:edit_content(<<"$t">>, <<"b">>, <<"i">>)),
     ?_assertEqual(#{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.annotation">>,
                                             <<"event_id">> => <<"$t">>, <<"key">> => <<"k">>}},
                   mod_matrix_gw_xmpp:reaction_content(<<"$t">>, <<"k">>)),
     ?_assertEqual(#{<<"redacts">> => <<"$t">>}, mod_matrix_gw_xmpp:redaction_content(<<"$t">>)),
     ?_assertEqual(<<"i">>, mod_matrix_gw_xmpp:xmpp_id(#{<<"content">> => #{X => <<"i">>}})),
     ?_assertEqual(<<>>, mod_matrix_gw_xmpp:xmpp_id(#{<<"content">> => #{X => 5}})),
     ?_assertEqual(<<>>, mod_matrix_gw_xmpp:xmpp_id(#{}))].

inbound_test_() ->
    I = fun mod_matrix_gw_xmpp:inbound_message/1,
    M = fun(Content) -> #{<<"type">> => <<"m.room.message">>, <<"content">> => Content} end,
    [?_assertEqual({text, <<"hi">>}, I(M(#{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"hi">>}))),
     ?_assertEqual({text, <<"note">>}, I(M(#{<<"msgtype">> => <<"m.notice">>, <<"body">> => <<"note">>}))),
     ?_assertEqual({text, <<"/me waves">>}, I(M(#{<<"msgtype">> => <<"m.emote">>, <<"body">> => <<"waves">>}))),
     ?_assertEqual({media, #{<<"msgtype">> => <<"m.image">>, <<"body">> => <<"a.png">>, <<"url">> => <<"mxc://s/x">>}},
                   I(M(#{<<"msgtype">> => <<"m.image">>, <<"body">> => <<"a.png">>, <<"url">> => <<"mxc://s/x">>}))),
     ?_assertEqual(ignore, I(M(#{<<"msgtype">> => <<"m.image">>, <<"body">> => <<"a.png">>}))),
     %% every msgtype with a body shows it, m.location
     %% too (mod_matrix_gw_content adds its <geoloc/>)
     ?_assertEqual({text, <<"here">>}, I(M(#{<<"msgtype">> => <<"m.location">>, <<"body">> => <<"here">>}))),
     ?_assertEqual(ignore, I(M(#{<<"body">> => <<"no type">>}))),
     ?_assertEqual({edit, <<"$t">>, <<"new">>},
                   I(M(#{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"* new">>,
                         <<"m.new_content">> => #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"new">>},
                         <<"m.relates_to">> => #{<<"rel_type">> => <<"m.replace">>, <<"event_id">> => <<"$t">>}}))),
     ?_assertEqual(ignore,
                   I(M(#{<<"m.new_content">> => #{<<"msgtype">> => <<"m.location">>},
                         <<"m.relates_to">> => #{<<"rel_type">> => <<"m.replace">>, <<"event_id">> => <<"$t">>}}))),
     ?_assertEqual({reaction, <<"$t">>, <<"k">>},
                   I(#{<<"type">> => <<"m.reaction">>,
                       <<"content">> => #{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.annotation">>,
                                                                  <<"event_id">> => <<"$t">>,
                                                                  <<"key">> => <<"k">>}}})),
     ?_assertEqual(ignore, I(#{<<"type">> => <<"m.reaction">>, <<"content">> => #{}})),
     ?_assertEqual(ignore, I(#{<<"type">> => <<"m.room.topic">>}))].

reactions_test() ->
    R1 = mod_matrix_gw_xmpp:add_reaction(#{}, {<<"@a:s">>, <<"$t">>}, <<"k1">>, <<"$r1">>),
    R2 = mod_matrix_gw_xmpp:add_reaction(R1, {<<"@a:s">>, <<"$t">>}, <<"k2">>, <<"$r2">>),
    R3 = mod_matrix_gw_xmpp:add_reaction(R2, {<<"@b:s">>, <<"$t">>}, <<"k1">>, <<"$r3">>),
    ?assertEqual([<<"k1">>, <<"k2">>], mod_matrix_gw_xmpp:reaction_keys(R3, <<"@a:s">>, <<"$t">>)),
    ?assertEqual([], mod_matrix_gw_xmpp:reaction_keys(R3, <<"@c:s">>, <<"$t">>)),
    {{<<"@a:s">>, <<"$t">>}, R4} = mod_matrix_gw_xmpp:remove_reaction(R3, <<"$r1">>),
    ?assertEqual([<<"k2">>], mod_matrix_gw_xmpp:reaction_keys(R4, <<"@a:s">>, <<"$t">>)),
    ?assertEqual(none, mod_matrix_gw_xmpp:remove_reaction(R4, <<"$r1">>)),
    ?assertEqual(none, mod_matrix_gw_xmpp:remove_reaction(#{}, <<"$x">>)).

trim_ids_test() ->
    M = #{a => {1, x}, b => {3, y}, c => {2, z}},
    ?assertEqual(M, mod_matrix_gw_xmpp:trim_ids(M, 3)),
    ?assertEqual(#{b => {3, y}, c => {2, z}}, mod_matrix_gw_xmpp:trim_ids(M, 2)),
    ?assertEqual(#{b => {3, y}}, mod_matrix_gw_xmpp:trim_ids(M, 1)).

edu_test_() ->
    P = fun mod_matrix_gw_xmpp:parse_edu/1,
    Typing = mod_matrix_gw_xmpp:typing_edu(<<"!r:s">>, <<"@a:s">>, true),
    Receipt = mod_matrix_gw_xmpp:receipt_edu(<<"!r:s">>, <<"@a:s">>, <<"$e">>, 7),
    Two = #{<<"edu_type">> => <<"m.receipt">>,
            <<"content">> => #{<<"!r:s">> => #{<<"m.read">> =>
                                                   #{<<"@a:s">> => #{<<"event_ids">> => [<<"$1">>, <<"$2">>, 3]},
                                                     <<"@b:s">> => #{<<"no">> => 1}}},
                               <<"!q:s">> => #{<<"m.read.private">> => #{}}}},
    [?_assertEqual([{typing, <<"!r:s">>, <<"@a:s">>, true}], P(Typing)),
     ?_assertEqual([{receipt, <<"!r:s">>, <<"@a:s">>, <<"$e">>}], P(Receipt)),
     ?_assertMatch(#{<<"content">> := #{<<"!r:s">> := #{<<"m.read">> := #{<<"@a:s">> := #{<<"data">> := #{<<"ts">> := 7}}}}}},
                   Receipt),
     ?_assertEqual([{receipt, <<"!r:s">>, <<"@a:s">>, <<"$1">>}, {receipt, <<"!r:s">>, <<"@a:s">>, <<"$2">>}],
                   lists:sort(P(Two))),
     ?_assertEqual([], P(#{<<"edu_type">> => <<"m.typing">>, <<"content">> => #{<<"typing">> => 1}})),
     ?_assertEqual([], P(#{<<"edu_type">> => <<"m.presence">>, <<"content">> => #{}})),
     ?_assertEqual([], P(5))].

xml_test_() ->
    [?_assertMatch(#xmlel{name = <<"replace">>, attrs = [{<<"xmlns">>, <<"urn:xmpp:message-correct:0">>}, {<<"id">>, <<"i">>}]},
                   mod_matrix_gw_xmpp:replace_el(<<"i">>)),
     ?_assertMatch(#xmlel{name = <<"reactions">>, children = [#xmlel{name = <<"reaction">>, children = [{xmlcdata, <<"k">>}]}]},
                   mod_matrix_gw_xmpp:reactions_el(<<"i">>, [<<"k">>])),
     ?_assertMatch(#xmlel{name = <<"displayed">>, attrs = [_, {<<"id">>, <<"i">>}]},
                   mod_matrix_gw_xmpp:displayed_el(<<"i">>)),
     ?_assertMatch(#xmlel{name = <<"composing">>}, mod_matrix_gw_xmpp:chatstate_el(true)),
     ?_assertMatch(#xmlel{name = <<"paused">>}, mod_matrix_gw_xmpp:chatstate_el(false)),
     ?_assertMatch(#xmlel{name = <<"markable">>}, mod_matrix_gw_xmpp:markable_el()),
     ?_assertMatch(#xmlel{name = <<"store">>}, mod_matrix_gw_xmpp:store_el()),
     ?_assertMatch(#xmlel{name = <<"no-store">>}, mod_matrix_gw_xmpp:no_store_el())].

%%--------------------------------------------------------------------
%% On an ejabberd with the gateway on
%%--------------------------------------------------------------------

ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup, fun() -> mod_matrix_gw_test_lib:start_ejabberd(?MODULE) end, fun mod_matrix_gw_test_lib:stop_ejabberd/1,
     fun(_) ->
             [{timeout, 30, {Name, F}}
              || {Name, F} <- [{"the store keeps, loads and forgets a room", fun store/0},
                               {"the options: matrix_domain may be a configured vhost", fun options/0},
                               {"the federation answers", fun answers/0},
                               {"a stored direct room comes back as it was", fun restore/0},
                               {"a session closed for system-shutdown is the server stopping",
                                fun shutdown/0}]]
     end}.

store() ->
    R = <<"!store:example.com">>,
    Kind = {direct, {<<"ann">>, ?HOST}, <<"@bob:example.org">>, established},
    ok = mod_matrix_gw_store:create_db(),
    ok = mod_matrix_gw_store:save_meta(R, <<"10">>, Kind, [<<"$b">>, <<"$a">>]),
    ok = mod_matrix_gw_store:save_events(R, [{<<"$a">>, #{<<"x">> => 1}, #{k => <<"$a">>}},
                                             {<<"$b">>, #{<<"y">> => 2}, undefined}], [<<"$b">>]),
    ok = mod_matrix_gw_store:save_events(<<"!nometa:x">>, [{<<"$z">>, #{}, undefined}], []),
    {ok, #{version := <<"10">>, kind := Kind, latest := [<<"$b">>], events := Events}} =
        mod_matrix_gw_store:load(R),
    ?assertEqual([{<<"$a">>, #{<<"x">> => 1}, #{k => <<"$a">>}}, {<<"$b">>, #{<<"y">> => 2}, undefined}],
                 lists:sort(Events)),
    ?assert(lists:member(R, mod_matrix_gw_store:rooms())),
    ok = mod_matrix_gw_store:delete_events(R, [<<"$a">>]),
    {ok, #{events := [{<<"$b">>, _, _}]}} = mod_matrix_gw_store:load(R),
    ok = mod_matrix_gw_store:delete(R),
    ok = mod_matrix_gw_store:delete(<<"!nometa:x">>),
    ?assertEqual(error, mod_matrix_gw_store:load(R)),
    ?assertNot(lists:member(R, mod_matrix_gw_store:rooms())).

options() ->
    %% upstream validated matrix_domain with econf:host(), which refuses a
    %% vhost of this server (route_conflict): the domain itself is the point
    ?assertEqual(?HOST, (mod_matrix_gw:mod_opt_type(matrix_domain))(?HOST)),
    ?assertEqual(?HOST, mod_matrix_gw_opt:matrix_domain(?HOST)),
    ?assertEqual(?GW, mod_matrix_gw_opt:host(?HOST)),
    ?assertEqual(undefined, mod_matrix_gw_opt:tls_cacertfile(?HOST)),
    ?assertEqual(undefined, mod_matrix_gw_opt:tls_cacertfile(#{tls_cacertfile => undefined})),
    ?assertMatch({_, _}, mod_matrix_gw_opt:key(?HOST)),
    ?assertMatch(#{desc := _}, mod_matrix_gw:mod_doc()),
    ?assertEqual(undefined, proplists:get_value(tls_cacertfile, mod_matrix_gw:mod_options(?HOST))).

%% A request as ejabberd_http hands it to the module, unsigned.
req(Method, Path, Q) ->
    #request{method = Method, path = Path, q = Q, headers = [], raw_path = <<>>,
             length = 0, data = <<>>, host = ?HOST, ip = {{127, 0, 0, 1}, 1}}.

json({Code, _, Body}) -> {Code, misc:json_decode(Body)}.

answers() ->
    P = fun(Path, Q) -> json(mod_matrix_gw:process(Path, req('GET', Path, Q))) end,
    ?assertMatch({200, #{<<"server">> := #{<<"name">> := <<"ejabberd/mod_matrix_gw">>}}},
                 P([<<"federation">>, <<"v1">>, <<"version">>], [])),
    ?assertMatch({200, #{<<"server_name">> := ?HOST}}, P([<<"key">>, <<"v2">>, <<"server">>], [])),
    %% everything else wants a signed request, and says so in the spec's way
    ?assertMatch({401, #{<<"errcode">> := <<"M_UNAUTHORIZED">>}},
                 P([<<"federation">>, <<"v1">>, <<"user">>, <<"devices">>, <<"@ann:example.com">>], [])),
    ?assertMatch({401, #{<<"errcode">> := <<"M_UNAUTHORIZED">>}},
                 P([<<"federation">>, <<"v1">>, <<"query">>, <<"directory">>], [])),
    ?assertMatch({401, _}, P([<<"federation">>, <<"v1">>, <<"query">>, <<"profile">>],
                             [{<<"user_id">>, <<"@ann:example.com">>}])),
    ?assertMatch({400, #{<<"errcode">> := <<"M_INVALID_PARAM">>}},
                 P([<<"federation">>, <<"v1">>, <<"query">>, <<"profile">>],
                   [{<<"user_id">>, <<"@ann:example.com">>}, {<<"field">>, <<"x">>}])),
    ?assertMatch({400, #{<<"errcode">> := <<"M_MISSING_PARAM">>}},
                 P([<<"federation">>, <<"v1">>, <<"state">>, <<"!r:x">>], [])),
    ?assertMatch({401, _}, P([<<"federation">>, <<"v1">>, <<"state">>, <<"!r:x">>],
                             [{<<"event_id">>, <<"$e">>}])),
    ?assertMatch({401, _}, P([<<"federation">>, <<"v1">>, <<"make_leave">>, <<"!r:x">>, <<"@b:x">>], [])),
    ?assertMatch({404, #{<<"errcode">> := <<"M_UNRECOGNIZED">>}},
                 P([<<"federation">>, <<"v1">>, <<"nothing">>], [])),
    BadAuth = (req('GET', [<<"federation">>, <<"v1">>, <<"query">>, <<"directory">>], []))
                  #request{headers = [{'Authorization', <<"Bearer x">>}]},
    ?assertMatch({401, _}, json(mod_matrix_gw:process([<<"federation">>, <<"v1">>, <<"query">>,
                                                       <<"directory">>], BadAuth))),
    Big = (req('PUT', [<<"federation">>, <<"v1">>, <<"send">>, <<"t">>], []))
              #request{headers = [{'Authorization', <<"X-Matrix origin=\"o\",key=\"ed25519:k\",sig=\"s\"">>}],
                       length = 2000000},
    ?assertMatch({413, #{<<"errcode">> := <<"M_TOO_LARGE">>}},
                 json(mod_matrix_gw:process([<<"federation">>, <<"v1">>, <<"send">>, <<"t">>], Big))).

restore() ->
    %% the room ann@example.com made with @bob:example.org, as the store keeps
    %% it: its create, ann's join, the power levels and join rules, bob's
    %% invite; then the room process started for its id finds it there
    RoomID = <<"!restored:example.com">>,
    Version = mod_matrix_gw_room:binary_to_room_version(<<"9">>),
    Ann = <<"@ann:example.com">>,
    Bob = <<"@bob:example.org">>,
    {_, Events} =
        lists:foldl(
          fun({Type, StateKey, Content}, {Prev, Acc}) ->
                  Auth = [E || {E, _, _} <- Acc],
                  JSON0 = #{<<"type">> => Type, <<"state_key">> => StateKey,
                            <<"content">> => Content, <<"sender">> => Ann,
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
          [{<<"m.room.create">>, <<>>, #{<<"creator">> => Ann, <<"room_version">> => <<"9">>}},
           {<<"m.room.member">>, Ann, #{<<"membership">> => <<"join">>}},
           {<<"m.room.power_levels">>, <<>>, #{<<"users">> => #{Ann => 100, Bob => 100}}},
           {<<"m.room.join_rules">>, <<>>, #{<<"join_rule">> => <<"invite">>}},
           {<<"m.room.member">>, Bob, #{<<"membership">> => <<"invite">>, <<"is_direct">> => true}}]),
    [{Last, _, _} | _] = Events,
    ok = mod_matrix_gw_store:save_meta(RoomID, <<"9">>, {direct, {<<"ann">>, ?HOST}, Bob, undefined}, [Last]),
    ok = mod_matrix_gw_store:save_events(RoomID, [{ID, J, SM} || {ID, _, {J, SM}} <- Events], [Last]),
    mod_matrix_gw_room:restore_rooms(?HOST),
    {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
    ?assertMatch(#{<<"room_version">> := <<"9">>},
                 #{<<"room_version">> => element(2, gen_statem:call(Pid, get_room_version))}),
    ?assertEqual([Last], sets:to_list(gen_statem:call(Pid, get_latest_events))),
    %% the direct chat's lookup is back: ann's next message goes to this room
    ?assertMatch([_], mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, Bob})),
    %% and the room answers for its state as before the restart
    {ok, _, StatePDUs} = mod_matrix_gw_room:get_state(?HOST, ?HOST, RoomID, Last),
    ?assertEqual({error, not_allowed}, mod_matrix_gw_room:get_state(?HOST, <<"example.org">>, RoomID, Last)),
    ?assertEqual(4, length(StatePDUs)),
    %% a version the gateway cannot read is no room
    ?assertEqual(undefined, mod_matrix_gw_room:get_known_room_version(#{<<"room_id">> => <<"!none:x">>})),
    ?assertMatch(#room_version{}, mod_matrix_gw_room:get_known_room_version(#{<<"room_id">> => RoomID})),
    ?assertEqual(undefined, mod_matrix_gw_room:get_known_room_version(#{})).

shutdown() ->
    %% ejabberdctl stop closes every session (system-shutdown) a second
    %% before ejabberd_stopping runs: such a close is no leave either
    persistent_term:erase({mod_matrix_gw, stopping}),
    Timeout = #{stop_reason => {stream, {out, #stream_error{reason = 'connection-timeout'}}}},
    ?assertEqual(Timeout, mod_matrix_gw:c2s_terminated(Timeout, normal)),
    ?assertEqual(#{}, mod_matrix_gw:c2s_terminated(#{}, normal)),
    ?assertNot(mod_matrix_gw:is_stopping()),
    Shutdown = #{stop_reason => {stream, {out, #stream_error{reason = 'system-shutdown'}}}},
    ?assertEqual(Shutdown, mod_matrix_gw:c2s_terminated(Shutdown, normal)),
    ?assert(mod_matrix_gw:is_stopping()),
    persistent_term:erase({mod_matrix_gw, stopping}),
    ?assertNot(mod_matrix_gw:is_stopping()).
