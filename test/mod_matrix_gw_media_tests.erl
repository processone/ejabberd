%%% The Matrix gateway's media (mod_matrix_gw_media): the multipart
%%% writer and parser, mxc ids, shares and events as functions of their
%%% input; then, on an ejabberd started with the gateway and
%%% mod_http_upload whose put_url and get_url are this suite's own HTTP
%%% server (the gateway's upload_url), the table and its bound, the media
%%% endpoints, a remote file fetched (the federation stood in for by
%%% functions) and put into the store through a real slot, and the two
%%% iqs. Federation with a real homeserver is not tested here.
-module(mod_matrix_gw_media_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").

-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).
-define(NS, <<"https://ns.another.im/matrix-e2e/0">>).
-define(PORT, 18363).
-define(STORE_URL, <<"http://127.0.0.1:18363/upload/">>).
-define(STORE, mod_matrix_gw_media_tests_store).

el(Name, NS, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, NS} | Attrs], children = Children}.

cdata(Name, Text) ->
    #xmlel{name = Name, children = [{xmlcdata, Text}]}.

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------

multipart_test_() ->
    Body = mod_matrix_gw_media:multipart(<<"b0">>, [{[{<<"Content-Type">>, <<"application/json">>}], <<"{}">>},
                                                   {[{<<"Location">>, <<"https://s/x">>}], <<>>}]),
    P = fun mod_matrix_gw_media:parse_multipart/2,
    Synapse = <<"\r\n--6067d4698f8d40a0a794ea7d7379d53a\r\nContent-Type: application/json\r\n\r\n{}\r\n"
                "--6067d4698f8d40a0a794ea7d7379d53a\r\nContent-Type: image/png\r\n"
                "Content-Disposition: inline; filename=a.png\r\n\r\nPNG\r\nDATA\r\n"
                "--6067d4698f8d40a0a794ea7d7379d53a--\r\n">>,
    [?_assertEqual(<<"--b0\r\nContent-Type: application/json\r\n\r\n{}\r\n"
                     "--b0\r\nLocation: https://s/x\r\n\r\n\r\n--b0--\r\n">>, Body),
     ?_assertEqual({ok, [{[{<<"content-type">>, <<"application/json">>}], <<"{}">>},
                         {[{<<"location">>, <<"https://s/x">>}], <<>>}]},
                   P(<<"multipart/mixed; boundary=b0">>, Body)),
     ?_assertEqual({ok, [{[{<<"content-type">>, <<"application/json">>}], <<"{}">>},
                         {[{<<"content-type">>, <<"image/png">>},
                           {<<"content-disposition">>, <<"inline; filename=a.png">>}],
                          <<"PNG\r\nDATA">>}]},
                   P(<<"Multipart/Mixed; boundary=\"6067d4698f8d40a0a794ea7d7379d53a\"">>, Synapse)),
     %% a part with no headers at all
     ?_assertEqual({ok, [{[], <<"{}">>}]}, P(<<"multipart/mixed; boundary=q">>, <<"--q\r\n\r\n{}\r\n--q--">>)),
     %% a part that does not end in CRLF before the boundary
     ?_assertEqual({ok, [{[], <<"{}">>}]}, P(<<"multipart/mixed; boundary=q">>, <<"--q\r\n\r\n{}--q--">>)),
     ?_assertEqual(error, P(undefined, Body)),
     ?_assertEqual(error, P(<<"application/json">>, Body)),
     ?_assertEqual(error, P(<<"multipart/mixed; boundary=zz">>, Body)),
     ?_assertEqual(error, P(<<"multipart/mixed; boundary=q">>, <<"--q\r\nbroken header\r\n\r\nx\r\n--q--">>))].

mxc_test_() ->
    P = fun mod_matrix_gw_media:parse_mxc/1,
    [?_assertEqual({<<"matrix.org">>, <<"AbC-_9">>}, P(<<"mxc://matrix.org/AbC-_9">>)),
     ?_assertEqual({<<"h:8448">>, <<"x">>}, P(<<"mxc://h:8448/x">>)),
     ?_assertEqual(error, P(<<"mxc://matrix.org/a/b">>)),
     ?_assertEqual(error, P(<<"mxc://matrix.org/">>)),
     ?_assertEqual(error, P(<<"mxc:///x">>)),
     ?_assertEqual(error, P(<<"mxc://nohost">>)),
     ?_assertEqual(error, P(<<"https://matrix.org/x">>))].

under_test_() ->
    U = fun mod_matrix_gw_media:under/2,
    B = <<"https://s.example/upload">>,
    [?_assert(U(<<"https://s.example/upload/abc/a.png">>, B)),
     ?_assert(U(<<"https://s.example/upload/abc/a.png">>, <<B/binary, "/">>)),
     ?_assertNot(U(<<"https://s.example/upload/">>, B)),
     ?_assertNot(U(<<"https://s.example/uploadx/a">>, B)),
     ?_assertNot(U(<<"https://s.example/upload/../secret">>, B)),
     ?_assertNot(U(<<"https://s.example/upload/a/../../b">>, B)),
     ?_assertNot(U(<<"https://s.example/upload/a/./b">>, B)),
     ?_assertNot(U(<<"https://s.example/upload/..">>, B)),
     ?_assertNot(U(<<"https://s.example/upload/a#frag">>, B)),
     ?_assertNot(U(<<"https://other/upload/a">>, B)),
     ?_assertNot(U(<<"https://s.example/upload/a">>, undefined))].

share_test_() ->
    S = fun mod_matrix_gw_media:share/1,
    URL = <<"https://s/upload/k/a%20b.jpg">>,
    OOB = el(<<"x">>, <<"jabber:x:oob">>, [], [cdata(<<"url">>, URL)]),
    File = el(<<"file">>, <<"urn:xmpp:file:metadata:0">>, [],
              [cdata(<<"media-type">>, <<"image/jpeg">>), cdata(<<"name">>, <<"cat.jpg">>),
               cdata(<<"size">>, <<"1234">>), cdata(<<"width">>, <<"640">>),
               cdata(<<"height">>, <<"480">>), cdata(<<"length">>, <<"x">>),
               cdata(<<"desc">>, <<"a cat">>)]),
    Sources = #xmlel{name = <<"sources">>,
                     children = [el(<<"url-data">>, <<"http://jabber.org/protocol/url-data">>,
                                    [{<<"target">>, <<"https://s/upload/k/cat.jpg">>}], [])]},
    SFS = el(<<"file-sharing">>, <<"urn:xmpp:sfs:0">>, [], [File, Sources]),
    [?_assertEqual({URL, #{}}, S([el(<<"origin-id">>, <<"urn:xmpp:sid:0">>, [], []), OOB])),
     ?_assertEqual({URL, #{}}, S([#oob_x{url = URL}])),
     ?_assertEqual({<<"https://s/upload/k/cat.jpg">>,
                    #{mime => <<"image/jpeg">>, name => <<"cat.jpg">>, size => 1234,
                      width => 640, height => 480, desc => <<"a cat">>}},
                   S([OOB, SFS])),
     %% a client's <dimensions/> (a sticker's size)
     ?_assertMatch({_, #{width := 512, height := 256}},
                   S([el(<<"file-sharing">>, <<"urn:xmpp:sfs:0">>, [],
                         [el(<<"file">>, <<"urn:xmpp:file:metadata:0">>, [],
                             [cdata(<<"dimensions">>, <<"512x256">>)]), Sources])])),
     %% a share with no source is no share; the OOB beside it is
     ?_assertEqual({URL, #{}}, S([el(<<"file-sharing">>, <<"urn:xmpp:sfs:0">>, [], [File]), OOB])),
     ?_assertEqual({<<"https://s/upload/k/cat.jpg">>, #{}},
                   S([el(<<"file-sharing">>, <<"urn:xmpp:sfs:0">>, [], [Sources])])),
     ?_assertEqual(none, S([el(<<"x">>, <<"jabber:x:data">>, [], []), el(<<"file-sharing">>, <<"other">>, [], [Sources])])),
     ?_assertEqual(none, S([el(<<"x">>, <<"jabber:x:oob">>, [], [])])),
     ?_assertEqual(none, S([]))].

classify_test_() ->
    C = fun mod_matrix_gw_xmpp:classify/1,
    OOB = el(<<"x">>, <<"jabber:x:oob">>, [], [cdata(<<"url">>, <<"https://s/a.png">>)]),
    Msg = fun(Body, Els) -> #message{id = <<"m1">>, type = chat, sub_els = Els,
                                     body = [#text{data = Body} || Body /= <<>>]} end,
    [?_assertEqual([{media, <<"https://s/a.png">>, #{}, <<"https://s/a.png">>}],
                   C(Msg(<<"https://s/a.png">>, [OOB]))),
     ?_assertEqual([{media, <<"https://s/a.png">>, #{}, <<>>}], C(Msg(<<>>, [OOB]))),
     %% a correction stays a correction
     ?_assertMatch([{edit, <<"m0">>, _}],
                   C(Msg(<<"x">>, [OOB, el(<<"replace">>, <<"urn:xmpp:message-correct:0">>,
                                         [{<<"id">>, <<"m0">>}], [])])))].

inbound_test_() ->
    I = fun mod_matrix_gw_xmpp:inbound_message/1,
    E = fun(Content) -> #{<<"type">> => <<"m.room.message">>, <<"content">> => Content} end,
    Image = #{<<"msgtype">> => <<"m.image">>, <<"body">> => <<"a.png">>, <<"url">> => <<"mxc://s/x">>},
    Enc = #{<<"msgtype">> => <<"m.file">>, <<"body">> => <<"f">>, <<"file">> => #{<<"url">> => <<"mxc://s/y">>}},
    [?_assertEqual({media, Image}, I(E(Image))),
     ?_assertEqual({encrypted_media, Enc}, I(E(Enc))),
     ?_assertEqual(ignore, I(E(#{<<"msgtype">> => <<"m.video">>, <<"body">> => <<"v">>}))),
     ?_assertEqual(ignore, I(E(#{<<"msgtype">> => <<"m.audio">>, <<"url">> => 5})))].

content_test_() ->
    MC = fun mod_matrix_gw_media:media_content/3,
    [?_assertEqual(#{<<"msgtype">> => <<"m.image">>, <<"body">> => <<"a.png">>,
                     <<"filename">> => <<"a.png">>, <<"url">> => <<"mxc://d/i">>,
                     <<"info">> => #{<<"mimetype">> => <<"image/png">>, <<"size">> => 5,
                                     <<"w">> => 4, <<"h">> => 3},
                     <<"net.process-one.xmpp-id">> => <<"m1">>},
                   MC(<<"mxc://d/i">>, #{name => <<"a.png">>, size => 5, width => 4, height => 3}, <<"m1">>)),
     ?_assertMatch(#{<<"msgtype">> := <<"m.video">>, <<"info">> := #{<<"duration">> := 9000}},
                   MC(<<"mxc://d/i">>, #{name => <<"v">>, mime => <<"video/webm">>, duration => 9000}, <<>>)),
     ?_assertMatch(#{<<"msgtype">> := <<"m.audio">>, <<"info">> := #{<<"mimetype">> := <<"audio/ogg">>}},
                   MC(<<"mxc://d/i">>, #{name => <<"voice.OPUS">>}, <<>>)),
     ?_assertMatch(#{<<"msgtype">> := <<"m.file">>, <<"body">> := <<"file">>,
                     <<"info">> := #{<<"mimetype">> := <<"application/octet-stream">>}},
                   MC(<<"mxc://d/i">>, #{}, <<>>)),
     ?_assertNot(maps:is_key(<<"net.process-one.xmpp-id">>, MC(<<"mxc://d/i">>, #{}, <<>>)))].

xmpp_side_test_() ->
    Els = mod_matrix_gw_media:share_els(#{url => <<"https://s/u/a.png">>, name => <<"a.png">>,
                                          mime => <<"image/png">>, size => 7, width => 2,
                                          duration => 3}),
    [OOB, SFS, Store] = Els,
    Event = mod_matrix_gw_media:event_el(<<"m.room.message">>, <<"$e">>, <<"!r:s">>, <<"@b:s">>, 17,
                                         #{<<"msgtype">> => <<"m.file">>}),
    [?_assertEqual(<<"https://s/u/a.png">>, fxml:get_path_s(OOB, [{elem, <<"url">>}, cdata])),
     ?_assertEqual(<<"jabber:x:oob">>, fxml:get_tag_attr_s(<<"xmlns">>, OOB)),
     ?_assertEqual(<<"urn:xmpp:sfs:0">>, fxml:get_tag_attr_s(<<"xmlns">>, SFS)),
     ?_assertEqual(<<"image/png">>, fxml:get_path_s(SFS, [{elem, <<"file">>}, {elem, <<"media-type">>}, cdata])),
     ?_assertEqual(<<"7">>, fxml:get_path_s(SFS, [{elem, <<"file">>}, {elem, <<"size">>}, cdata])),
     ?_assertEqual(<<"2">>, fxml:get_path_s(SFS, [{elem, <<"file">>}, {elem, <<"width">>}, cdata])),
     ?_assertEqual(<<"3">>, fxml:get_path_s(SFS, [{elem, <<"file">>}, {elem, <<"length">>}, cdata])),
     ?_assertEqual(<<>>, fxml:get_path_s(SFS, [{elem, <<"file">>}, {elem, <<"height">>}, cdata])),
     ?_assertEqual(<<"https://s/u/a.png">>,
                   fxml:get_path_s(SFS, [{elem, <<"sources">>}, {elem, <<"url-data">>}, {attr, <<"target">>}])),
     %% the share read back is the share written
     ?_assertEqual({<<"https://s/u/a.png">>, #{name => <<"a.png">>, mime => <<"image/png">>, size => 7,
                                               width => 2, duration => 3}},
                   mod_matrix_gw_media:share(Els)),
     ?_assertMatch(#xmlel{name = <<"store">>}, Store),
     ?_assertEqual(<<"{\"msgtype\":\"m.file\"}">>, fxml:get_tag_cdata(Event)),
     ?_assertEqual([<<"m.room.message">>, <<"$e">>, <<"!r:s">>, <<"@b:s">>, <<"17">>, ?NS],
                   [fxml:get_tag_attr_s(A, Event) || A <- [<<"type">>, <<"id">>, <<"room">>, <<"sender">>,
                                                          <<"ts">>, <<"xmlns">>]]),
     ?_assertEqual(<<"n">>, mod_matrix_gw_media:file_name(#{<<"filename">> => <<"n">>, <<"body">> => <<"b">>})),
     ?_assertEqual(<<"b">>, mod_matrix_gw_media:file_name(#{<<"filename">> => <<>>, <<"body">> => <<"b">>})),
     ?_assertEqual(<<"file">>, mod_matrix_gw_media:file_name(#{})),
     ?_assertEqual([<<"a (a file from Matrix that could not be fetched: not found on its server)">>,
                    <<"a (a file from Matrix that could not be fetched: too large for the gateway)">>,
                    <<"a (a file from Matrix that could not be fetched: its server did not answer)">>,
                    <<"a (a file from Matrix that could not be fetched: it could not be stored)">>,
                    <<"a (a file from Matrix that could not be fetched: its server cannot be reached)">>,
                    <<"a (a file from Matrix that could not be fetched: its server named a place the gateway does not fetch from)">>,
                    <<"a (a file from Matrix that could not be fetched: the gateway is fetching too many files)">>],
                   [mod_matrix_gw_media:failure_body(<<"a">>, R)
                    || R <- [not_found, too_large, timeout, upload, unreachable, refused, busy]])].

%%--------------------------------------------------------------------
%% On an ejabberd with the gateway and the store
%%--------------------------------------------------------------------

ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup,
     fun() -> Store = store_start(), {mod_matrix_gw_test_lib:start_ejabberd(?MODULE), Store} end,
     fun({Dir, Store}) -> exit(Store, kill), mod_matrix_gw_test_lib:stop_ejabberd(Dir) end,
     fun(_) ->
             [{timeout, 60, {Name, F}}
              || {Name, F} <- [{"the options; the store is upload_url", fun options/0},
                               {"mint: one mxc per URL, the store's only, the table bounded", fun mint/0},
                               {"the federation's download and thumbnail", fun federation/0},
                               {"the legacy endpoints: redirect, proxy, other servers", fun legacy/0},
                               {"the legacy proxy within the slots and its bound", fun s5_proxy/0},
                               {"a remote file: the answers a fetch takes", fun fetch/0},
                               {"a remote file into the store through a real slot", fun upload/0},
                               {"resolve: ours, cached, unreachable", fun resolve/0},
                               {"fetch slots, the whole answer's deadline, the remote table's bound",
                                fun bounds/0},
                               {"<media-id/>", fun media_id_iq/0},
                               {"<media-fetch/>", fun media_fetch_iq/0},
                               {"plain chats: a share to Matrix, an event to XMPP", fun plain/0}]]
     end}.

%% The store: PUT keeps the bytes under the path, GET gives them back;
%% /nolength answers 2000 bytes with no Content-Length; /upload/slow trickles a
%% byte every 200 ms (for 5 s at most) of the 900 it announces.
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
        {'GET', <<"/upload/slow">>} ->
            gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nContent-Length: 900\r\nConnection: close\r\n\r\n">>),
            lists:foldl(fun(_, ok) -> timer:sleep(200), gen_tcp:send(S, <<"x">>);
                           (_, Err) -> Err
                        end, ok, lists:seq(1, 25));
        {'GET', <<"/nolength">>} ->
            gen_tcp:send(S, [<<"HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n">>, binary:copy(<<"x">>, 2000)]);
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

stored(URL) ->
    #{path := Path} = uri_string:parse(URL),
    ets:insert(?STORE, {Path, <<"bytes of ", Path/binary>>}),
    URL.

options() ->
    ?assertEqual(?STORE_URL, <<(mod_matrix_gw_media:upload_url(?HOST))/binary, "/">>),
    ?assertEqual(1000, mod_matrix_gw_opt:media_max_bytes(?HOST)),
    ?assertEqual(20, mod_matrix_gw_opt:media_max_entries(?HOST)),
    ?assertEqual(<<"http://127.0.0.1:18363/upload">>, mod_matrix_gw_opt:upload_url(?HOST)),
    ?assertEqual(<<"https://u/">>, mod_matrix_gw_opt:upload_url(#{upload_url => <<"https://u/">>})),
    ?assertEqual(5, mod_matrix_gw_opt:media_max_bytes(#{media_max_bytes => 5})),
    ?assertEqual(5, mod_matrix_gw_opt:media_max_entries(#{media_max_entries => 5})),
    Opts = mod_matrix_gw:mod_options(?HOST),
    ?assertEqual({undefined, 104857600, 100000},
                 {proplists:get_value(upload_url, Opts), proplists:get_value(media_max_bytes, Opts),
                  proplists:get_value(media_max_entries, Opts)}),
    ?assertEqual(<<"https://u/x">>, (mod_matrix_gw:mod_opt_type(upload_url))(<<"https://u/x">>)),
    ?assertEqual(7, (mod_matrix_gw:mod_opt_type(media_max_bytes))(7)),
    ?assertEqual(7, (mod_matrix_gw:mod_opt_type(media_max_entries))(7)),
    %% the option, when set, is the store
    Old = gen_mod:get_module_opts(?HOST, mod_matrix_gw),
    with_opts(Old#{upload_url => <<"https://files.example/u">>},
              fun() -> ?assertEqual(<<"https://files.example/u">>, mod_matrix_gw_media:upload_url(?HOST)) end),
    %% no upload_url and no mod_s3_upload: no store, no file crosses, and
    %% slots asked of upload.<host>
    with_opts(Old#{upload_url => undefined},
              fun() ->
                      ?assertEqual(undefined, mod_matrix_gw_media:upload_url(?HOST)),
                      ?assertEqual({error, not_allowed},
                                   mod_matrix_gw_media:mint(?HOST, <<?STORE_URL/binary, "a">>, #{})),
                      ?assertMatch({ok, _}, mod_matrix_gw_media:upload(?HOST, <<"x">>, <<"a/b">>, <<"x">>))
              end).

with_opts(Opts, F) ->
    with_module_opts(mod_matrix_gw, Opts, F).

%% F run with Module's options (as gen_mod keeps them) at Opts on the
%% vhost, the old ones after.
with_module_opts(Module, Opts, F) ->
    [Entry] = ets:lookup(ejabberd_modules, {Module, ?HOST}),
    ets:insert(ejabberd_modules, setelement(3, Entry, Opts)),
    try F()
    after ets:insert(ejabberd_modules, Entry)
    end.

mint() ->
    URL = <<?STORE_URL/binary, "k1/a.png">>,
    {ok, <<"mxc://example.com/", ID/binary>> = MXC} = mod_matrix_gw_media:mint(?HOST, URL, #{size => 3}),
    ?assertEqual(22, byte_size(ID)),
    ?assertMatch({ok, MXC}, mod_matrix_gw_media:mint(?HOST, URL, #{})),
    ?assertMatch({ok, MXC}, mod_matrix_gw_media:mint(?HOST, URL, #{name => <<"other">>})),
    ?assertEqual({ok, #{url => URL, name => <<"a.png">>, mime => <<"image/png">>, size => 3}},
                 mod_matrix_gw_media:resolve(?HOST, MXC, #{})),
    ?assertEqual({error, not_allowed}, mod_matrix_gw_media:mint(?HOST, <<"https://elsewhere/a.png">>, #{})),
    {ok, Other} = mod_matrix_gw_media:mint(?HOST, <<?STORE_URL/binary, "k2/a%20b.bin">>, #{}),
    ?assertNotEqual(MXC, Other),
    ?assertMatch({ok, #{name := <<"a b.bin">>, mime := <<"application/octet-stream">>}},
                 mod_matrix_gw_media:resolve(?HOST, Other, #{})),
    ?assertNot(maps:is_key(size, element(2, mod_matrix_gw_media:resolve(?HOST, Other, #{})))),
    %% names from URLs that have none to give
    Name = fun(U) -> {ok, M} = mod_matrix_gw_media:mint(?HOST, <<?STORE_URL/binary, U/binary>>, #{}),
                     {ok, #{name := N}} = mod_matrix_gw_media:resolve(?HOST, M, #{}),
                     N
           end,
    ?assertEqual(<<"file">>, Name(<<"k3/">>)),
    ?assertEqual(<<"%ZZ">>, Name(<<"k3/%ZZ">>)),
    ?assertEqual(<<"file">>, Name(<<"k3/a b">>)),
    %% past media_max_entries (20) the oldest go: the first is forgotten
    %% (bound/3 sorts by the millisecond made; the entries above are kept
    %% older than every one below, which a fast run made in the same one)
    timer:sleep(5),
    [{ok, _} = mod_matrix_gw_media:mint(?HOST, <<?STORE_URL/binary, "b/", (integer_to_binary(N))/binary>>, #{})
     || N <- lists:seq(1, 25)],
    ?assert(mnesia:table_info(matrix_media, size) =< 20),
    ?assertEqual({error, not_found}, mod_matrix_gw_media:resolve(?HOST, MXC, #{})),
    {ok, Last} = mod_matrix_gw_media:mint(?HOST, <<?STORE_URL/binary, "b/25">>, #{}),
    ?assertMatch({ok, _}, mod_matrix_gw_media:resolve(?HOST, Last, #{})).

%% A request as ejabberd_http hands it to the module.
req(Path, Q) ->
    #request{method = 'GET', path = Path, q = Q, headers = [], raw_path = <<>>,
             length = 0, data = <<>>, host = ?HOST, ip = {{127, 0, 0, 1}, 1}}.

errcode({Code, _, Body}) ->
    {Code, maps:get(<<"errcode">>, misc:json_decode(Body))}.

own(Name, Mime) ->
    URL = stored(<<?STORE_URL/binary, "o/", Name/binary>>),
    {ok, <<"mxc://example.com/", ID/binary>>} = mod_matrix_gw_media:mint(?HOST, URL, #{mime => Mime}),
    {ID, URL}.

federation() ->
    {Img, ImgURL} = own(<<"p.png">>, <<"image/png">>),
    {Doc, _} = own(<<"d.pdf">>, <<"application/pdf">>),
    P = fun(Kind, ID) -> mod_matrix_gw_media:process([<<"federation">>, <<"v1">>, <<"media">>, Kind, ID],
                                                     req([], [])) end,
    {200, [{<<"Content-Type">>, CT}], Body} = P(<<"download">>, Img),
    ?assertMatch({ok, [{_, <<"{}">>}, {[{<<"location">>, ImgURL}], <<>>}]},
                 mod_matrix_gw_media:parse_multipart(CT, Body)),
    {200, [{_, CT2}], Body2} = P(<<"thumbnail">>, Img),
    ?assertMatch({ok, [_, {[{<<"location">>, ImgURL}], _}]}, mod_matrix_gw_media:parse_multipart(CT2, Body2)),
    ?assertMatch({200, _, _}, P(<<"download">>, Doc)),
    %% no thumbnail of what is not an image; nothing of an unknown id
    ?assertEqual({404, <<"M_NOT_FOUND">>}, errcode(P(<<"thumbnail">>, Doc))),
    ?assertEqual({404, <<"M_NOT_FOUND">>}, errcode(P(<<"download">>, <<"nosuchid">>))),
    ?assertEqual({404, <<"M_UNRECOGNIZED">>},
                 errcode(mod_matrix_gw_media:process([<<"federation">>, <<"v1">>, <<"media">>, <<"x">>], req([], [])))),
    %% through the module: the federation's endpoint wants a signed request
    ?assertEqual({401, <<"M_UNAUTHORIZED">>},
                 errcode(mod_matrix_gw:process([<<"federation">>, <<"v1">>, <<"media">>, <<"download">>, Img],
                                               req([], [])))).

legacy() ->
    {Img, ImgURL} = own(<<"l.png">>, <<"image/png">>),
    {Doc, _} = own(<<"l.pdf">>, <<"application/pdf">>),
    L = fun(Kind, Server, ID, Q) ->
                Path = [<<"media">>, <<"v3">>, Kind, Server, ID],
                mod_matrix_gw:process(Path, req(Path, Q))
        end,
    ?assertEqual({307, [{<<"Location">>, ImgURL}], <<>>},
                 L(<<"download">>, ?HOST, Img, [{<<"allow_redirect">>, <<"true">>}])),
    ?assertEqual({307, [{<<"Location">>, ImgURL}], <<>>},
                 L(<<"thumbnail">>, ?HOST, Img, [{<<"allow_redirect">>, <<"true">>}])),
    %% no redirect allowed: the bytes, read from the store
    {200, Headers, Bytes} = L(<<"download">>, ?HOST, Img, []),
    ?assertEqual(<<"bytes of /upload/o/l.png">>, Bytes),
    ?assertEqual(<<"image/png">>, proplists:get_value(<<"Content-Type">>, Headers)),
    ?assertEqual(<<"inline; filename*=utf-8''l.png">>, proplists:get_value(<<"Content-Disposition">>, Headers)),
    ?assertEqual({404, <<"M_NOT_FOUND">>}, errcode(L(<<"thumbnail">>, ?HOST, Doc, []))),
    ?assertEqual({404, <<"M_NOT_FOUND">>}, errcode(L(<<"download">>, <<"matrix.org">>, Img, []))),
    ?assertEqual({404, <<"M_NOT_FOUND">>}, errcode(L(<<"download">>, ?HOST, <<"none">>, []))),
    ?assertEqual({404, <<"M_UNRECOGNIZED">>}, errcode(L(<<"config">>, ?HOST, <<"x">>, []))),
    %% the store has lost it, or it is over media_max_bytes: 502
    {ok, <<"mxc://example.com/", Gone/binary>>} =
        mod_matrix_gw_media:mint(?HOST, <<?STORE_URL/binary, "gone/x.png">>, #{}),
    ?assertEqual({502, <<"M_UNKNOWN">>}, errcode(L(<<"download">>, ?HOST, Gone, []))),
    ets:insert(?STORE, {<<"/upload/big/x.png">>, binary:copy(<<"x">>, 1001)}),
    {ok, <<"mxc://example.com/", Big/binary>>} =
        mod_matrix_gw_media:mint(?HOST, <<?STORE_URL/binary, "big/x.png">>, #{}),
    ?assertEqual({502, <<"M_TOO_LARGE">>}, errcode(L(<<"download">>, ?HOST, Big, []))),
    with_opts((gen_mod:get_module_opts(?HOST, mod_matrix_gw))#{upload_url => <<"http://127.0.0.1:18363/">>},
              fun() ->
                      {ok, <<"mxc://example.com/", NoLen/binary>>} =
                          mod_matrix_gw_media:mint(?HOST, <<"http://127.0.0.1:18363/nolength">>, #{}),
                      ?assertEqual({502, <<"M_TOO_LARGE">>}, errcode(L(<<"download">>, ?HOST, NoLen, [])))
              end),
    %% an address with no server behind it
    with_opts((gen_mod:get_module_opts(?HOST, mod_matrix_gw))#{upload_url => <<"http://127.0.0.1:1/">>},
              fun() ->
                      {ok, <<"mxc://example.com/", Closed/binary>>} =
                          mod_matrix_gw_media:mint(?HOST, <<"http://127.0.0.1:1/x">>, #{}),
                      ?assertEqual({502, <<"M_UNKNOWN">>}, errcode(L(<<"download">>, ?HOST, Closed, [])))
              end).

%% The federation as functions: Fed(Path, Query) and Get(URL) answer what
%% the test says; Upload records what it is given.
http(Fed, Get) ->
    Self = self(),
    #{federation => Fed, get => Get, resolve => fun resolve_name/1,
      upload => fun(Name, Mime, Bytes) -> Self ! {uploaded, Name, Mime, Bytes}, {ok, <<"https://store/", Name/binary>>} end}.

%% The names the fetch tests use, as DNS would answer them.
resolve_name("cdn") -> [{93, 184, 216, 34}];
resolve_name("cdn2") -> [{93, 184, 216, 35}, {16#2606, 16#2800, 16#220, 1, 16#248, 16#1893, 16#25c8, 16#1946}];
resolve_name("internal.example") -> [{10, 0, 0, 1}];
resolve_name("mixed.example") -> [{93, 184, 216, 34}, {127, 0, 0, 1}];
resolve_name("v6local.example") -> [{16#fd00, 0, 0, 0, 0, 0, 0, 1}];
resolve_name(_) -> [].

multipart_answer(Part) ->
    Body = mod_matrix_gw_media:multipart(<<"bb">>, [{[], <<"{}">>}, Part]),
    {ok, 200, [{<<"content-type">>, <<"multipart/mixed; boundary=bb">>}], Body}.

fetch() ->
    F = fun(Origin, ID, Hints, Http) -> mod_matrix_gw_media:fetch(Origin, ID, Hints, 1000, Http) end,
    NoGet = fun(_) -> error(no_get) end,
    %% the bytes in the multipart, their type and name from the part
    Bytes = fun(_Path, []) ->
                    multipart_answer({[{<<"Content-Type">>, <<"image/png; x=y">>},
                                       {<<"Content-Disposition">>, <<"inline; filename=\"pic.png\"">>}],
                                      <<"PNG">>})
            end,
    ?assertMatch({ok, #{url := <<"https://store/pic.png">>, name := <<"pic.png">>, mime := <<"image/png">>,
                        size := 3}},
                 F(<<"r1.example">>, <<"a1">>, #{}, http(Bytes, NoGet))),
    receive {uploaded, <<"pic.png">>, <<"image/png">>, <<"PNG">>} -> ok after 0 -> error(not_uploaded) end,
    %% fetched once: the copy is remembered
    ?assertMatch({ok, #{url := <<"https://store/pic.png">>}},
                 mod_matrix_gw_media:resolve(?HOST, <<"mxc://r1.example/a1">>, #{})),
    %% the asker's name and type before the server's; RFC 5987 names
    Star = fun(_, _) -> multipart_answer({[{<<"Content-Disposition">>,
                                            <<"attachment; filename*=UTF-8''caf%C3%A9.txt">>}], <<"x">>}) end,
    ?assertMatch({ok, #{name := <<"café.txt"/utf8>>, mime := <<"text/plain">>}},
                 F(<<"r1.example">>, <<"a2">>, #{}, http(Star, NoGet))),
    ?assertMatch({ok, #{name := <<"n">>, mime := <<"m/t">>}},
                 F(<<"r1.example">>, <<"a3">>, #{name => <<"n">>, mime => <<"m/t">>}, http(Star, NoGet))),
    NoName = fun(_, _) -> multipart_answer({[], <<"x">>}) end,
    ?assertMatch({ok, #{name := <<"a4">>, mime := <<"application/octet-stream">>}},
                 F(<<"r1.example">>, <<"a4">>, #{}, http(NoName, NoGet))),
    BadStar = fun(_, _) -> multipart_answer({[{<<"Content-Disposition">>, <<"inline; filename*=utf-8''%ZZ">>}], <<"x">>}) end,
    ?assertMatch({ok, #{name := <<"a5">>}}, F(<<"r1.example">>, <<"a5">>, #{}, http(BadStar, NoGet))),
    Plain = fun(_, _) -> multipart_answer({[{<<"Content-Disposition">>, <<"inline">>}], <<"x">>}) end,
    ?assertMatch({ok, #{name := <<"a6">>}}, F(<<"r1.example">>, <<"a6">>, #{}, http(Plain, NoGet))),
    %% a Location, followed once
    Loc = fun(_, _) -> multipart_answer({[{<<"Location">>, <<"https://cdn/x">>}], <<>>}) end,
    Cdn = fun(<<"https://cdn/x">>) -> {ok, 200, [{<<"content-type">>, <<"video/mp4">>}], <<"MP4">>} end,
    ?assertMatch({ok, #{mime := <<"video/mp4">>, size := 3}},
                 F(<<"r1.example">>, <<"l1">>, #{}, http(Loc, Cdn))),
    ?assertEqual({error, not_found},
                 F(<<"r1.example">>, <<"l2">>, #{}, http(Loc, fun(_) -> {ok, 404, [], <<>>} end))),
    ?assertEqual({error, timeout},
                 F(<<"r1.example">>, <<"l3">>, #{}, http(Loc, fun(_) -> {error, timeout} end))),
    %% a Location anywhere but a public https address is not
    %% fetched (the get fun is never called: NoGet would crash the fetch)
    LocTo = fun(URL) -> fun(_, _) -> multipart_answer({[{<<"Location">>, URL}], <<>>}) end end,
    [?assertEqual({error, refused}, F(<<"evil.example">>, <<"s", (integer_to_binary(N))/binary>>, #{},
                                      http(LocTo(URL), NoGet)))
     || {N, URL} <- lists:enumerate(
                      [<<"http://127.0.0.1:5280/api/change_password?user=admin&host=example.com&newpass=x">>,
                       <<"https://127.0.0.1:5280/api/x">>,
                       <<"https://[::1]/x">>,
                       <<"https://[::ffff:127.0.0.1]/x">>,
                       <<"https://169.254.169.254/latest/meta-data/">>,
                       <<"https://10.1.2.3/x">>,
                       <<"https://192.168.1.1/x">>,
                       <<"https://172.20.0.5/x">>,
                       <<"https://100.64.0.1/x">>,
                       <<"https://0.0.0.0/x">>,
                       <<"https://224.0.0.1/x">>,
                       <<"https://[fe80::1]/x">>,
                       <<"https://internal.example/x">>,
                       <<"https://mixed.example/x">>,
                       <<"https://v6local.example/x">>,
                       <<"https://nowhere.example/x">>,
                       <<"https://user@cdn/x">>,
                       <<"http://cdn/x">>,
                       <<"file:///etc/passwd">>,
                       <<"gopher://cdn/x">>,
                       <<"not a url">>])],
    %% a relative Location is the origin's own: no host, not followed
    ?assertEqual({error, refused}, F(<<"evil.example">>, <<"rel">>, #{}, http(LocTo(<<"/x">>), NoGet))),
    %% the store's own objects may be named, http or not (the operator's)
    StoreGet = fun(<<"http://127.0.0.1:18363/upload/k/x.png">>) -> {ok, 200, [], <<"PNG">>} end,
    ?assertMatch({ok, #{size := 3}},
                 F(<<"r1.example">>, <<"st">>, #{},
                   (http(LocTo(<<"http://127.0.0.1:18363/upload/k/x.png">>), StoreGet))#{store => ?STORE_URL})),
    ?assertEqual({error, refused},
                 F(<<"r1.example">>, <<"st2">>, #{},
                   (http(LocTo(<<"http://127.0.0.1:18363/other/x.png">>), NoGet))#{store => ?STORE_URL})),
    %% one redirect after the Location, checked again; not a second one
    Hop = fun(<<"https://cdn/x">>) -> {ok, 302, [{<<"location">>, <<"https://cdn2/y">>}], <<>>};
             (<<"https://cdn2/y">>) -> {ok, 200, [{<<"content-type">>, <<"video/mp4">>}], <<"MP4">>}
          end,
    ?assertMatch({ok, #{mime := <<"video/mp4">>}}, F(<<"r1.example">>, <<"h1">>, #{}, http(Loc, Hop))),
    Relative = fun(<<"https://cdn/x">>) -> {ok, 301, [{<<"location">>, <<"/z">>}], <<>>};
                  (<<"https://cdn/z">>) -> {ok, 200, [], <<"Z">>}
               end,
    ?assertMatch({ok, #{size := 1}}, F(<<"r1.example">>, <<"h2">>, #{}, http(Loc, Relative))),
    HopIn = fun(<<"https://cdn/x">>) -> {ok, 307, [{<<"location">>, <<"http://127.0.0.1:5280/api/x">>}], <<>>} end,
    ?assertEqual({error, refused}, F(<<"r1.example">>, <<"h3">>, #{}, http(Loc, HopIn))),
    TwoHops = fun(<<"https://cdn/x">>) -> {ok, 302, [{<<"location">>, <<"https://cdn2/y">>}], <<>>};
                 (<<"https://cdn2/y">>) -> {ok, 302, [{<<"location">>, <<"https://cdn/x">>}], <<>>}
              end,
    ?assertEqual({error, unreachable}, F(<<"r1.example">>, <<"h4">>, #{}, http(Loc, TwoHops))),
    %% an older server: the legacy endpoint, its redirect followed
    Old = fun([_, <<"federation">> | _], []) ->
                  {ok, 404, [], <<"{\"errcode\":\"M_UNRECOGNIZED\"}">>};
             ([<<"_matrix">>, <<"media">>, <<"v3">>, <<"download">>, <<"old.example">>, <<"o1">>],
              [{<<"allow_remote">>, <<"false">>}]) ->
                  {ok, 200, [{<<"content-type">>, <<"audio/ogg">>}], <<"OGG">>};
             ([<<"_matrix">>, <<"media">>, <<"v3">>, <<"download">>, _, <<"o2">>], _) ->
                  {ok, 307, [{<<"location">>, <<"https://cdn/x">>}], <<>>};
             ([<<"_matrix">>, <<"media">>, <<"v3">>, <<"download">>, _, <<"o3">>], _) ->
                  {ok, 404, [], <<"{\"errcode\":\"M_NOT_FOUND\"}">>};
             ([<<"_matrix">>, <<"media">>, <<"v3">>, <<"download">>, _, <<"o4">>], _) ->
                  {ok, 302, [], <<>>};
             ([<<"_matrix">>, <<"media">>, <<"v3">>, <<"download">>, _, <<"o5">>], _) ->
                  {error, {failed_connect, []}}
          end,
    ?assertMatch({ok, #{mime := <<"audio/ogg">>}}, F(<<"old.example">>, <<"o1">>, #{}, http(Old, NoGet))),
    ?assertMatch({ok, #{mime := <<"video/mp4">>}}, F(<<"old.example">>, <<"o2">>, #{}, http(Old, Cdn))),
    ?assertEqual({error, not_found}, F(<<"old.example">>, <<"o3">>, #{}, http(Old, NoGet))),
    ?assertEqual({error, unreachable}, F(<<"old.example">>, <<"o4">>, #{}, http(Old, NoGet))),
    ?assertEqual({error, unreachable}, F(<<"old.example">>, <<"o5">>, #{}, http(Old, NoGet))),
    %% the legacy endpoint's redirect to the admin API
    OldEvil = fun([_, <<"federation">> | _], []) ->
                      {ok, 404, [], <<"{\"errcode\":\"M_UNRECOGNIZED\"}">>};
                 ([<<"_matrix">>, <<"media">>, <<"v3">>, <<"download">>, _, _], _) ->
                      {ok, 307, [{<<"location">>, <<"http://127.0.0.1:5280/api/register?user=x&host=example.com&password=y">>}], <<>>}
              end,
    ?assertEqual({error, refused}, F(<<"evil.example">>, <<"o6">>, #{}, http(OldEvil, NoGet))),
    %% the errors a server gives
    Answer = fun(Res) -> F(<<"r2.example">>, <<"e">>, #{}, http(fun(_, _) -> Res end, NoGet)) end,
    ?assertEqual({error, not_found}, Answer({ok, 404, [], <<"{\"errcode\":\"M_NOT_FOUND\"}">>})),
    ?assertEqual({error, not_found}, Answer({ok, 400, [], <<"{\"errcode\":\"M_NOT_FOUND\"}">>})),
    ?assertEqual({error, too_large}, Answer({ok, 413, [], <<>>})),
    ?assertEqual({error, too_large}, Answer({ok, 502, [], <<"{\"errcode\":\"M_TOO_LARGE\"}">>})),
    ?assertEqual({error, timeout}, Answer({ok, 504, [], <<"not json">>})),
    ?assertEqual({error, unreachable}, Answer({ok, 500, [], <<"[]">>})),
    ?assertEqual({error, unreachable}, Answer({ok, 200, [], <<"no multipart">>})),
    ?assertEqual({error, unreachable},
                 Answer({ok, 200, [{<<"content-type">>, <<"multipart/mixed; boundary=bb">>}],
                         mod_matrix_gw_media:multipart(<<"bb">>, [{[], <<"{}">>}])})),
    ?assertEqual({error, timeout}, Answer({error, timeout})),
    ?assertEqual({error, too_large}, Answer({error, too_large})),
    ?assertEqual({error, unreachable}, Answer({error, {failed_connect, []}})),
    ?assertEqual({error, unreachable}, Answer({error, {no_destination, <<"r2.example">>}})),
    ?assertEqual({error, too_large}, Answer(multipart_answer({[], binary:copy(<<"x">>, 1001)}))),
    %% the store refusing it
    Refused = (http(Bytes, NoGet))#{upload => fun(_, _, _) -> {error, upload} end},
    ?assertEqual({error, upload}, F(<<"r3.example">>, <<"u">>, #{}, Refused)).

upload() ->
    {ok, URL} = mod_matrix_gw_media:upload(?HOST, <<"cat 1.png">>, <<"image/png">>, <<"MEOW">>),
    ?assertMatch(<<"http://127.0.0.1:18363/upload/", _:40/binary, "/", _:40/binary, "/cat_1.png">>, URL),
    #{path := Path} = uri_string:parse(URL),
    ?assertEqual([{Path, <<"MEOW">>}], ets:lookup(?STORE, Path)),
    %% too large for the slot: the service says no
    ?assertEqual({error, upload}, mod_matrix_gw_media:upload(?HOST, <<"big">>, <<"a/b">>, binary:copy(<<"x">>, 1001))),
    %% a store that refuses the PUT
    Up = gen_mod:get_module_opts(?HOST, mod_http_upload),
    Refusing = Up#{put_url => <<"http://127.0.0.1:1/upload">>},
    with_module_opts(mod_http_upload, Refusing,
                     fun() ->
                             mod_http_upload:reload(?HOST, Refusing, Up),
                             _ = sys:get_state(mod_http_upload:get_proc_name(?HOST, mod_http_upload)),
                             ?assertEqual({error, upload},
                                          mod_matrix_gw_media:upload(?HOST, <<"x">>, <<"a/b">>, <<"x">>))
                     end),
    mod_http_upload:reload(?HOST, Up, Refusing),
    _ = sys:get_state(mod_http_upload:get_proc_name(?HOST, mod_http_upload)),
    %% no upload service at all
    gen_mod:stop_module(?HOST, mod_http_upload),
    try
        ?assertEqual({error, upload}, mod_matrix_gw_media:upload(?HOST, <<"x">>, <<"a/b">>, <<"x">>))
    after
        gen_mod:start_module(?HOST, mod_http_upload)
    end.

resolve() ->
    {ID, URL} = own(<<"r.png">>, <<"image/png">>),
    ?assertMatch({ok, #{url := URL}}, mod_matrix_gw_media:resolve(?HOST, <<"mxc://example.com/", ID/binary>>, #{})),
    ?assertEqual({error, not_found}, mod_matrix_gw_media:resolve(?HOST, <<"mxc://example.com/none">>, #{})),
    ?assertEqual({error, not_found}, mod_matrix_gw_media:resolve(?HOST, <<"not an mxc">>, #{})),
    %% a remote server that is not there: the federation's own request fails
    ?assertEqual({error, unreachable}, mod_matrix_gw_media:resolve(?HOST, <<"mxc://127.0.0.1:1/abc">>, #{})),
    %% a remote file fetched before: the store's copy, no request
    mnesia:dirty_write({matrix_media_remote, {<<"far.example">>, <<"c1">>}, <<"https://s/c1">>,
                        <<"c.png">>, <<"image/png">>, 9, 0}),
    ?assertEqual({ok, #{url => <<"https://s/c1">>, name => <<"c.png">>, mime => <<"image/png">>, size => 9}},
                 mod_matrix_gw_media:resolve(?HOST, <<"mxc://far.example/c1">>, #{})).

%% at most Slots fetches at once (the rest wait, then are
%% busy); a fetch's answer within its deadline even when bytes keep coming;
%% matrix_media_remote bounded by media_max_entries.
bounds() ->
    Self = self(),
    Run = fun(N) ->
                  spawn(fun() ->
                                R = mod_matrix_gw_media:with_slot(
                                      2, 5000,
                                      fun() ->
                                              Self ! {in, N, self()},
                                              receive go -> ok end,
                                              {done, N}
                                      end),
                                Self ! {result, N, R}
                        end)
          end,
    [Run(N) || N <- [1, 2, 3]],
    In = fun() -> receive {in, N, P} -> {N, P} after 1000 -> none end end,
    {_, P1} = In(),
    {_, P2} = In(),
    ?assertEqual(none, In()),                   % the third waits
    P1 ! go,
    {N3, P3} = In(),                            % and gets the slot freed
    P2 ! go, P3 ! go,
    Results = lists:sort([receive {result, N, R} -> {N, R} after 2000 -> error(no_result) end
                          || _ <- [1, 2, 3]]),
    ?assertEqual([{1, {done, 1}}, {2, {done, 2}}, {3, {done, 3}}], Results),
    ?assert(lists:member(N3, [1, 2, 3])),
    %% no slot within the wait: busy, and a crash gives its slot back
    Hold = spawn(fun() -> mod_matrix_gw_media:with_slot(1, 0, fun() -> Self ! held, receive go -> ok end end) end),
    receive held -> ok after 1000 -> error(not_held) end,
    ?assertEqual({error, busy}, mod_matrix_gw_media:with_slot(1, 300, fun() -> error(ran) end)),
    Hold ! go,
    timer:sleep(100),
    ?assertError(boom, mod_matrix_gw_media:with_slot(1, 0, fun() -> error(boom) end)),
    ?assertEqual(ok, mod_matrix_gw_media:with_slot(1, 0, fun() -> ok end)),
    %% a trickle of bytes does not keep a fetch alive past its deadline
    T0 = erlang:monotonic_time(millisecond),
    ?assertEqual({error, timeout},
                 mod_matrix_gw_media:plain_get(?HOST, <<?STORE_URL/binary, "slow">>, 1000, T0 + 1000)),
    ?assert(erlang:monotonic_time(millisecond) - T0 < 3000),
    timer:sleep(300),
    %% the remote table holds media_max_entries (20) at most
    [mnesia:dirty_write({matrix_media_remote, {<<"far.example">>, integer_to_binary(I)}, <<"https://s/r">>,
                         <<"r">>, <<"m/t">>, 1, I}) || I <- lists:seq(1, 30)],
    _ = mod_matrix_gw_media:fetch(?HOST, <<"127.0.0.1:1">>, <<"none">>, #{}),
    ?assert(mnesia:table_info(matrix_media_remote, size) =< 20),
    ?assertEqual([], mnesia:dirty_read(matrix_media_remote, {<<"far.example">>, <<"1">>})).

%% Ann's session is this process: what the gateway answers her comes here.
ask(From, Name, Attrs) ->
    ask(From, ?NS, Name, Attrs).

ask(From, NS, Name, Attrs) ->
    #jid{luser = U, lserver = S, lresource = R} = J = jid:decode(From),
    ejabberd_sm:open_session({erlang:timestamp(), self()}, U, S, R, 0, []),
    Id = p1_rand:get_string(),
    ejabberd_router:route(#iq{type = get, id = Id, from = J, to = jid:make(?GW),
                              sub_els = [el(Name, NS, Attrs, [])]}),
    receive
        {route, #iq{id = Id} = IQ} -> IQ
    after 5000 -> timeout
    end.

result(#iq{type = result, sub_els = [#xmlel{attrs = Attrs}]}) ->
    maps:from_list([{K, V} || {K, V} <- Attrs, K /= <<"xmlns">>]);
result(#iq{type = error} = IQ) ->
    (xmpp:get_error(IQ))#stanza_error.reason.

media_id_iq() ->
    %% ann may be there already: the suites share one ejabberd (the calls
    %% suite registers her too)
    case ejabberd_auth:try_register(<<"ann">>, ?HOST, <<"secret">>) of
        ok -> ok;
        {error, exists} -> ok
    end,
    Ann = <<"ann@example.com/phone">>,
    URL = <<?STORE_URL/binary, "iq/a.jpg">>,
    #{<<"mxc">> := MXC} = result(ask(Ann, <<"media-id">>, [{<<"url">>, URL}, {<<"name">>, <<"photo.jpg">>},
                                                           {<<"mime">>, <<"image/jpeg">>}, {<<"size">>, <<"12">>}])),
    ?assertMatch(<<"mxc://example.com/", _:22/binary>>, MXC),
    ?assertEqual(#{<<"mxc">> => MXC}, result(ask(Ann, <<"media-id">>, [{<<"url">>, URL}]))),
    ?assertEqual({ok, #{url => URL, name => <<"photo.jpg">>, mime => <<"image/jpeg">>, size => 12}},
                 mod_matrix_gw_media:resolve(?HOST, MXC, #{})),
    ?assertEqual('not-allowed', result(ask(Ann, <<"media-id">>, [{<<"url">>, <<"https://elsewhere/a">>}]))),
    ?assertEqual('not-allowed', result(ask(Ann, <<"media-id">>, [{<<"size">>, <<"-1">>}]))),
    %% only for the users of this server
    ?assertEqual('not-allowed', result(ask(<<"nobody@example.com/x">>, <<"media-id">>, [{<<"url">>, URL}]))),
    %% a set is no request of ours
    #jid{luser = U, lserver = S, lresource = R} = J = jid:decode(Ann),
    ejabberd_sm:open_session({erlang:timestamp(), self()}, U, S, R, 0, []),
    ejabberd_router:route(#iq{type = set, id = <<"s1">>, from = J, to = jid:make(?GW),
                              sub_els = [el(<<"media-id">>, ?NS, [{<<"url">>, URL}], [])]}),
    receive {route, #iq{id = <<"s1">>} = Set} -> ?assertEqual('service-unavailable', result(Set))
    after 5000 -> error(timeout)
    end,
    %% the server itself is no user
    ?assertEqual(ok, mod_matrix_gw_media:route_iq(#iq{type = get, id = <<"s2">>, from = jid:make(?HOST),
                                                      to = jid:make(?GW),
                                                      sub_els = [el(<<"media-id">>, ?NS, [], [])]})),
    %% another namespace's media-* is not ours to answer
    ?assertEqual('service-unavailable', result(ask(Ann, <<"urn:other">>, <<"media-id">>, [{<<"url">>, URL}]))).

media_fetch_iq() ->
    Ann = <<"ann@example.com/phone">>,
    {ID, URL} = own(<<"f.png">>, <<"image/png">>),
    ?assertEqual(#{<<"url">> => URL, <<"name">> => <<"f.png">>, <<"mime">> => <<"image/png">>},
                 result(ask(Ann, <<"media-fetch">>, [{<<"mxc">>, <<"mxc://example.com/", ID/binary>>}]))),
    mnesia:dirty_write({matrix_media_remote, {<<"far.example">>, <<"c2">>}, <<"https://s/c2">>,
                        <<"c.png">>, <<"image/png">>, 9, 0}),
    ?assertEqual(#{<<"url">> => <<"https://s/c2">>, <<"name">> => <<"c.png">>, <<"mime">> => <<"image/png">>,
                   <<"size">> => <<"9">>},
                 result(ask(Ann, <<"media-fetch">>, [{<<"mxc">>, <<"mxc://far.example/c2">>}]))),
    ?assertEqual('item-not-found', result(ask(Ann, <<"media-fetch">>, [{<<"mxc">>, <<"mxc://example.com/none">>}]))),
    ?assertEqual('item-not-found', result(ask(Ann, <<"media-fetch">>, []))),
    ?assertEqual('remote-server-not-found',
                 result(ask(Ann, <<"media-fetch">>, [{<<"mxc">>, <<"mxc://127.0.0.1:1/zz">>}]))),
    ?assertEqual('not-allowed', result(ask(<<"eve@example.com/x">>, <<"media-fetch">>, [{<<"mxc">>, <<"mxc://far.example/c2">>}]))).

plain() ->
    Ann = jid:decode(<<"ann@example.com/phone">>),
    URL = stored(<<?STORE_URL/binary, "p/cat.jpg">>),
    %% XMPP to Matrix: the file, then the caption
    {ok, [Image, Caption]} = mod_matrix_gw_media:outbound_contents(
                               ?HOST, URL, #{width => 3}, <<"look ", URL/binary>>, <<"x1">>),
    ?assertMatch(#{<<"msgtype">> := <<"m.image">>, <<"body">> := <<"cat.jpg">>,
                   <<"url">> := <<"mxc://example.com/", _/binary>>,
                   <<"info">> := #{<<"mimetype">> := <<"image/jpeg">>, <<"w">> := 3},
                   <<"net.process-one.xmpp-id">> := <<"x1">>}, Image),
    ?assertEqual(#{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"look">>}, Caption),
    ?assertMatch({ok, [#{<<"msgtype">> := <<"m.image">>}]},
                 mod_matrix_gw_media:outbound_contents(?HOST, URL, #{}, URL, <<>>)),
    ?assertEqual(not_ours, mod_matrix_gw_media:outbound_contents(?HOST, <<"https://e/a.png">>, #{}, <<"t">>, <<>>)),
    %% Matrix to XMPP: the event's file in the store, as a share
    #{<<"url">> := MXC} = Image,
    Msg = #message{id = <<"$ev">>, from = jid:make(<<"bob%far.example">>, ?GW), to = Ann, type = chat,
                   sub_els = [#xmlel{name = <<"markable">>}]},
    Content = #{<<"msgtype">> => <<"m.image">>, <<"body">> => <<"cat.jpg">>, <<"url">> => MXC,
                <<"info">> => #{<<"mimetype">> => <<"image/jpeg">>, <<"w">> => 640, <<"h">> => 480,
                                <<"duration">> => <<"bad">>}},
    #message{body = [#text{data = URL}], sub_els = [#xmlel{name = <<"markable">>} | Share]} =
        mod_matrix_gw_media:share_message(?HOST, Msg, Content),
    ?assertEqual({URL, #{name => <<"cat.jpg">>, mime => <<"image/jpeg">>, width => 640, height => 480}},
                 mod_matrix_gw_media:share(Share)),
    %% a file that cannot be had: its name and why
    #message{body = [#text{data = Failed}], sub_els = [_]} =
        mod_matrix_gw_media:share_message(?HOST, Msg, #{<<"msgtype">> => <<"m.file">>, <<"body">> => <<"x.zip">>,
                                                        <<"url">> => <<"mxc://example.com/none">>,
                                                        <<"info">> => 5}),
    ?assertEqual(<<"x.zip (a file from Matrix that could not be fetched: not found on its server)">>, Failed),
    %% delivered by a process of its own to ann's session
    #jid{luser = U, lserver = S, lresource = R} = Ann,
    ejabberd_sm:open_session({erlang:timestamp(), self()}, U, S, R, 0, []),
    Pid = mod_matrix_gw_media:route_share(?HOST, Msg#message{id = <<"$routed">>}, Content),
    ?assert(is_pid(Pid)),
    receive
        {route, #message{id = <<"$routed">>, body = [#text{data = Got}]}} -> ?assertEqual(URL, Got)
    after 5000 -> error(not_routed)
    end.

%% the unauthenticated endpoint reads a file only in a fetch
%% slot and only up to its bound; a file known to be larger is not read.
s5_proxy() ->
    Limit = fun(Name, V) -> persistent_term:put({mod_matrix_gw_s2s, limit, Name}, V) end,
    {Img, ImgURL} = own(<<"s5.png">>, <<"image/png">>),
    L = fun(ID) ->
                Path = [<<"media">>, <<"v3">>, <<"download">>, ?HOST, ID],
                mod_matrix_gw:process(Path, req(Path, []))
        end,
    ?assertMatch({200, _, <<"bytes of /upload/o/s5.png">>}, L(Img)),
    try
        %% every slot (4 with media_max_bytes 1000) held: busy, nothing read
        Self = self(),
        Holders = [spawn(fun() ->
                                 mod_matrix_gw_media:with_slot(4, 0, fun() -> Self ! held, receive go -> ok end end)
                         end) || _ <- [1, 2, 3, 4]],
        [receive held -> ok after 1000 -> error(not_held) end || _ <- Holders],
        Limit(proxy_wait, 200),
        ?assertEqual({503, <<"M_LIMIT_EXCEEDED">>}, errcode(L(Img))),
        [H ! go || H <- Holders],
        timer:sleep(100),
        ?assertMatch({200, _, _}, L(Img)),
        %% the proxy's bound below media_max_bytes: too large
        Limit(proxy_max, 10),
        ?assertEqual({502, <<"M_TOO_LARGE">>}, errcode(L(Img))),
        %% a size known to be over it: refused without a request (the
        %% store has no such object: a read would be M_UNKNOWN)
        {ok, <<"mxc://example.com/", Big/binary>>} =
            mod_matrix_gw_media:mint(?HOST, <<?STORE_URL/binary, "absent/big.png">>, #{size => 11}),
        ?assertEqual({502, <<"M_TOO_LARGE">>}, errcode(L(Big))),
        %% a client that follows a redirect still gets the store's URL
        ?assertEqual({307, [{<<"Location">>, ImgURL}], <<>>},
                     begin
                         Path = [<<"media">>, <<"v3">>, <<"download">>, ?HOST, Img],
                         mod_matrix_gw:process(Path, req(Path, [{<<"allow_redirect">>, <<"true">>}]))
                     end)
    after
        [persistent_term:erase({mod_matrix_gw_s2s, limit, N}) || N <- [proxy_wait, proxy_max]]
    end.
