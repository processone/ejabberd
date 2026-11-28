%%% The Matrix gateway's content mapping (mod_matrix_gw_content):
%%% replies, mentions, places, polls, stickers and receipts as
%%% functions of their input, both ways, and classify/1 and
%%% inbound_message/1 of mod_matrix_gw_xmpp where they hand over to it.
-module(mod_matrix_gw_content_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(C, mod_matrix_gw_content).

el(Name, NS, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, NS} | Attrs], children = Children}.

cdata(Name, Text) ->
    #xmlel{name = Name, children = [{xmlcdata, Text}]}.

attr(El, Name) -> fxml:get_tag_attr_s(Name, El).

reply(To, ID) -> el(<<"reply">>, <<"urn:xmpp:reply:0">>, [{<<"to">>, To}, {<<"id">>, ID}], []).

fallback(For, Bodies) ->
    el(<<"fallback">>, <<"urn:xmpp:fallback:0">>, [{<<"for">>, For}],
       [#xmlel{name = <<"body">>, attrs = A} || A <- Bodies]).

mention(URI) ->
    el(<<"reference">>, <<"urn:xmpp:reference:0">>, [{<<"type">>, <<"mention">>}, {<<"uri">>, URI},
                                                    {<<"begin">>, <<"0">>}, {<<"end">>, <<"3">>}], []).

geoloc(Kids) -> el(<<"geoloc">>, <<"http://jabber.org/protocol/geoloc">>, [], Kids).

poll_el(Attrs, Title, Options) ->
    el(<<"poll">>, <<"urn:fluux:poll:0">>, Attrs,
       [cdata(<<"title">>, Title), cdata(<<"description">>, <<"Pick one">>)] ++
           [#xmlel{name = <<"option">>, attrs = [{<<"emoji">>, E}], children = [{xmlcdata, L}]}
            || {E, L} <- Options]).

%%--------------------------------------------------------------------
%% XMPP to Matrix
%%--------------------------------------------------------------------

rel_reply_test_() ->
    R = fun ?C:rel/2,
    Quote = <<"> hello\n> there\n">>,
    Body = <<Quote/binary, "fine, thanks">>,
    [%% the fallback cut by its code point offsets
     ?_assertEqual({<<"fine, thanks">>, #{reply => {<<"bob@x">>, <<"m1">>}}},
                   R([reply(<<"bob@x">>, <<"m1">>),
                      fallback(<<"urn:xmpp:reply:0">>, [[{<<"start">>, <<"0">>},
                                                         {<<"end">>, integer_to_binary(byte_size(Quote))}]])],
                     Body)),
     %% code points, not bytes
     ?_assertEqual({<<"ok">>, #{reply => {<<>>, <<"m1">>}}},
                   R([el(<<"reply">>, <<"urn:xmpp:reply:0">>, [{<<"id">>, <<"m1">>}], []),
                      fallback(<<"urn:xmpp:reply:0">>, [[{<<"start">>, <<"0">>}, {<<"end">>, <<"5">>}]])],
                     <<"> é\n\nok"/utf8>>)),
     %% no fallback: the text as it is; another feature's fallback is not cut
     ?_assertEqual({Body, #{reply => {<<"b">>, <<"m1">>}}}, R([reply(<<"b">>, <<"m1">>)], Body)),
     ?_assertEqual({Body, #{reply => {<<"b">>, <<"m1">>}}},
                   R([reply(<<"b">>, <<"m1">>), fallback(<<"urn:other">>, [])], Body)),
     %% a fallback naming no span is the whole body; bad offsets read as the edges
     ?_assertEqual({<<>>, #{reply => {<<"b">>, <<"m1">>}}},
                   R([reply(<<"b">>, <<"m1">>), fallback(<<"urn:xmpp:reply:0">>, [])], Body)),
     ?_assertEqual({<<>>, #{reply => {<<"b">>, <<"m1">>}}},
                   R([reply(<<"b">>, <<"m1">>),
                      fallback(<<"urn:xmpp:reply:0">>, [[{<<"start">>, <<"x">>}, {<<"end">>, <<"-1">>}]])],
                     Body)),
     %% a reply with no id is none
     ?_assertEqual({Body, #{}}, R([el(<<"reply">>, <<"urn:xmpp:reply:0">>, [], [])], Body)),
     ?_assertEqual({<<"x">>, #{}}, R([el(<<"origin-id">>, <<"urn:xmpp:sid:0">>, [], [])], <<"x">>)),
     %% a record among the elements is read as its XML
     ?_assertEqual({<<"x">>, #{}}, R([#chatstate{type = active}, #origin_id{id = <<"o">>}], <<"x">>))].

rel_mentions_test_() ->
    R = fun ?C:rel/2,
    [?_assertEqual({<<"bob hi">>, #{mentions => [<<"xmpp:a@x">>, <<"xmpp:b@x">>]}},
                   R([mention(<<"xmpp:b@x">>), mention(<<"xmpp:a@x">>), mention(<<"xmpp:b@x">>),
                      el(<<"reference">>, <<"urn:xmpp:reference:0">>, [{<<"type">>, <<"data">>},
                                                                      {<<"uri">>, <<"https://e">>}], []),
                      el(<<"reference">>, <<"urn:xmpp:reference:0">>, [{<<"type">>, <<"mention">>}], [])],
                     <<"bob hi">>)),
     %% XEP-0513: a person by jid, the room by #channel
     ?_assertEqual({<<"all">>, #{mentions => [<<"xmpp:c@x">>], room_mention => true}},
                   R([el(<<"mention">>, <<"urn:xmpp:mentions:0">>, [{<<"jid">>, <<"c@x">>}], []),
                      el(<<"mention">>, <<"urn:xmpp:mentions:0">>,
                         [{<<"mentions">>, <<"urn:xmpp:mentions:0#channel">>}], [])],
                     <<"all">>))].

rel_geo_test_() ->
    R = fun ?C:rel/2,
    [?_assertEqual({<<"geo:1.5,2">>, #{geo => #{lat => <<"1.5">>, lon => <<"2">>, accuracy => <<"10">>,
                                                description => <<"Home">>, text => <<"Main St">>}}},
                   R([geoloc([cdata(<<"accuracy">>, <<"10">>), cdata(<<"description">>, <<"Home">>),
                              cdata(<<"lat">>, <<"1.5">>), cdata(<<"lon">>, <<"2">>),
                              cdata(<<"text">>, <<"Main St">>)])], <<"geo:1.5,2">>)),
     %% out of range, not numbers: no place
     ?_assertEqual({<<"t">>, #{}}, R([geoloc([cdata(<<"lat">>, <<"91">>), cdata(<<"lon">>, <<"0">>)])], <<"t">>)),
     ?_assertEqual({<<"t">>, #{}}, R([geoloc([cdata(<<"lat">>, <<"a">>), cdata(<<"lon">>, <<"0">>)])], <<"t">>)),
     %% a body that is a geo: URI alone, as Conversations sends it
     ?_assertEqual({<<"geo:-33.86,151.2;u=35">>, #{geo => #{uri => <<"geo:-33.86,151.2;u=35">>}}},
                   R([], <<"geo:-33.86,151.2;u=35">>)),
     ?_assertEqual({<<"see geo:1,2">>, #{}}, R([], <<"see geo:1,2">>)),
     ?_assertEqual({<<"geo:1,200">>, #{}}, R([], <<"geo:1,200">>))].

rel_poll_test_() ->
    R = fun ?C:rel/2,
    Opts = [{<<"1️⃣"/utf8>>, <<"Pizza">>}, {<<"2️⃣"/utf8>>, <<"Sushi">>}],
    [?_assertEqual({<<"b">>, #{poll => #{title => <<"Lunch?">>, description => <<"Pick one">>,
                                         options => Opts, multiple => true, hide => false}}},
                   R([poll_el([{<<"allow-multiple">>, <<"1">>}], <<"Lunch?">>, Opts)], <<"b">>)),
     ?_assertMatch({_, #{poll := #{multiple := false, hide := true}}},
                   R([poll_el([{<<"hide-results">>, <<"true">>}], <<"Lunch?">>, Opts)], <<"b">>)),
     %% fewer than two options with emoji and label: no poll
     ?_assertEqual({<<"b">>, #{}},
                   R([poll_el([], <<"Lunch?">>, [{<<"1️⃣"/utf8>>, <<"Pizza">>}, {<<>>, <<"x">>}])], <<"b">>)),
     ?_assertEqual({<<"b">>, #{}}, R([poll_el([], <<>>, Opts)], <<"b">>)),
     ?_assertEqual({<<"c">>, #{poll_closed => #{message_id => <<"P">>, title => <<"Lunch?">>}}},
                   R([el(<<"poll-closed">>, <<"urn:fluux:poll:0">>, [{<<"message-id">>, <<"P">>}],
                         [cdata(<<"title">>, <<"Lunch?">>)])], <<"c">>)),
     ?_assertEqual({<<"c">>, #{}}, R([el(<<"poll-closed">>, <<"urn:fluux:poll:0">>, [], [])], <<"c">>))].

mention_mxid_test_() ->
    M = fun(U) -> ?C:mention_mxid(U, <<"example.com">>, <<"matrix.example.com">>, <<"example.com">>) end,
    [?_assertEqual({ok, <<"@bob:far.example">>}, M(<<"xmpp:bob%far.example@matrix.example.com">>)),
     ?_assertEqual({ok, <<"@a b:far.example">>}, M(<<"xmpp:a\\20b%far.example@matrix.example.com">>)),
     ?_assertEqual({ok, <<"@bob:far.example">>},
                   M(<<"xmpp:!room%far.example@matrix.example.com/bob:far.example">>)),
     ?_assertEqual({ok, <<"@ann:example.com">>}, M(<<"xmpp:ann@example.com?message">>)),
     ?_assertEqual(error, M(<<"xmpp:!room@matrix.example.com/nocolon">>)),
     ?_assertEqual(error, M(<<"xmpp:nopercent@matrix.example.com">>)),
     ?_assertEqual(error, M(<<"xmpp:eve@elsewhere.org">>)),
     ?_assertEqual(error, M(<<"xmpp:example.com">>)),
     ?_assertEqual(error, M(<<"https://example.com">>))].

resolve_rel_test_() ->
    Resolve = #{event => fun(<<"m1">>) -> <<"$e1">>; (<<"P">>) -> <<"$p">>; (_) -> undefined end,
                sender => fun(<<"$e1">>) -> <<"@bob:s">>; (_) -> undefined end,
                mxid => fun(<<"xmpp:c@s">>) -> {ok, <<"@c:s">>}; (_) -> error end},
    RR = fun(Rel) -> ?C:resolve_rel(Rel, Resolve) end,
    [?_assertEqual(#{reply => <<"$e1">>, mentions => [<<"@bob:s">>, <<"@c:s">>]},
                   RR(#{reply => {<<"b">>, <<"m1">>}, mentions => [<<"xmpp:c@s">>, <<"xmpp:nobody">>]})),
     %% an unknown reply target is dropped, the text still goes
     ?_assertEqual(#{}, RR(#{reply => {<<"b">>, <<"m9">>}, mentions => [<<"xmpp:nobody">>]})),
     ?_assertEqual(#{poll_end => <<"$p">>}, RR(#{poll_closed => #{message_id => <<"P">>}})),
     ?_assertEqual(#{}, RR(#{poll_closed => #{message_id => <<"Q">>}})),
     ?_assertEqual(#{geo => #{uri => <<"geo:1,2">>}}, ?C:resolve_rel(#{geo => #{uri => <<"geo:1,2">>}}, #{}))].

outbound_test_() ->
    O = fun ?C:outbound/3,
    [?_assertEqual({<<"m.room.message">>,
                    #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"fine">>,
                      <<"m.relates_to">> => #{<<"m.in_reply_to">> => #{<<"event_id">> => <<"$e1">>}},
                      <<"m.mentions">> => #{<<"user_ids">> => [<<"@bob:s">>], <<"room">> => true},
                      <<"net.process-one.xmpp-id">> => <<"x1">>}},
                   O(<<"fine">>, #{reply => <<"$e1">>, mentions => [<<"@bob:s">>], room_mention => true},
                     <<"x1">>)),
     ?_assertEqual({<<"m.room.message">>, #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<"t">>}},
                   O(<<"t">>, #{}, <<>>)),
     %% a place: geo_uri and MSC3488's location
     ?_assertEqual({<<"m.room.message">>,
                    #{<<"msgtype">> => <<"m.location">>, <<"body">> => <<"Home, Main St\ngeo:1.5,2;u=10">>,
                      <<"geo_uri">> => <<"geo:1.5,2;u=10">>,
                      <<"org.matrix.msc3488.location">> => #{<<"uri">> => <<"geo:1.5,2;u=10">>,
                                                             <<"description">> => <<"Home, Main St">>}}},
                   O(<<>>, #{geo => #{lat => <<"1.5">>, lon => <<"2">>, accuracy => <<"10">>,
                                      description => <<"Home">>, text => <<"Main St">>}}, <<>>)),
     ?_assertMatch({_, #{<<"body">> := <<"Home\ngeo:1,2">>,
                         <<"org.matrix.msc3488.location">> := #{<<"description">> := <<"Home">>}}},
                   O(<<>>, #{geo => #{lat => <<"1">>, lon => <<"2">>, description => <<"Home">>}}, <<>>)),
     ?_assertMatch({_, #{<<"body">> := <<"Main St\ngeo:1,2">>}},
                   O(<<>>, #{geo => #{lat => <<"1">>, lon => <<"2">>, text => <<"Main St">>}}, <<>>)),
     ?_assertMatch({_, #{<<"body">> := <<"Location geo:1,2">>, <<"geo_uri">> := <<"geo:1,2">>}},
                   O(<<" ">>, #{geo => #{uri => <<"geo:1,2">>}}, <<>>)),
     ?_assertMatch({_, #{<<"body">> := <<"geo:1,2">>, <<"geo_uri">> := <<"geo:1,2">>}},
                   O(<<"geo:1,2">>, #{geo => #{lat => <<"1">>, lon => <<"2">>}}, <<>>)),
     %% a poll: MSC3381's unstable names, the emojis as answer ids
     ?_assertEqual({<<"org.matrix.msc3381.poll.start">>,
                    #{<<"org.matrix.msc3381.poll.start">> =>
                          #{<<"question">> => #{<<"org.matrix.msc1767.text">> => <<"Lunch?">>,
                                                <<"body">> => <<"Lunch?">>},
                            <<"kind">> => <<"org.matrix.msc3381.poll.disclosed">>,
                            <<"max_selections">> => 1,
                            <<"answers">> => [#{<<"id">> => <<"A">>, <<"org.matrix.msc1767.text">> => <<"Pizza">>},
                                              #{<<"id">> => <<"B">>, <<"org.matrix.msc1767.text">> => <<"Sushi">>}]},
                      <<"org.matrix.msc1767.text">> => <<"fallback">>,
                      <<"net.process-one.xmpp-id">> => <<"P">>}},
                   O(<<"fallback">>, #{poll => #{title => <<"Lunch?">>, options => [{<<"A">>, <<"Pizza">>},
                                                                                     {<<"B">>, <<"Sushi">>}]}},
                     <<"P">>)),
     ?_assertMatch({_, #{<<"org.matrix.msc3381.poll.start">> :=
                             #{<<"kind">> := <<"org.matrix.msc3381.poll.undisclosed">>,
                               <<"max_selections">> := 2}}},
                   O(<<"f">>, #{poll => #{title => <<"T">>, options => [{<<"A">>, <<"a">>}, {<<"B">>, <<"b">>}],
                                          multiple => true, hide => true}}, <<>>)),
     ?_assertEqual({<<"org.matrix.msc3381.poll.end">>,
                    #{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.reference">>, <<"event_id">> => <<"$p">>},
                      <<"org.matrix.msc3381.poll.end">> => #{},
                      <<"org.matrix.msc1767.text">> => <<"closed">>, <<"body">> => <<"closed">>}},
                   O(<<"closed">>, #{poll_end => <<"$p">>}, <<>>))].

votes_test_() ->
    Options = [{<<"a">>, <<"1️⃣"/utf8>>, <<"Pizza">>}, {<<"b">>, <<"2️⃣"/utf8>>, <<"Sushi">>}],
    [?_assertEqual({[<<"b">>], [<<"👍"/utf8>>]},
                   ?C:vote_split([<<"👍"/utf8>>, <<"2️⃣"/utf8>>], Options)),
     ?_assertEqual({[], []}, ?C:vote_split([], Options)),
     ?_assertEqual(#{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.reference">>, <<"event_id">> => <<"$p">>},
                     <<"org.matrix.msc3381.poll.response">> => #{<<"answers">> => [<<"b">>]}},
                   ?C:response_content(<<"$p">>, [<<"b">>]))].

sticker_test_() ->
    Media = #{<<"msgtype">> => <<"m.image">>, <<"body">> => <<"s.webp">>, <<"filename">> => <<"s.webp">>,
              <<"url">> => <<"mxc://d/s">>, <<"info">> => #{}},
    [?_assertEqual(#{<<"body">> => <<"Cat">>, <<"url">> => <<"mxc://d/s">>, <<"info">> => #{}},
                   ?C:sticker_content(Media, <<"Cat">>)),
     ?_assertEqual(#{<<"body">> => <<"s.webp">>, <<"url">> => <<"mxc://d/s">>, <<"info">> => #{}},
                   ?C:sticker_content(Media, <<>>)),
     ?_assertMatch([#xmlel{name = <<"sticker">>}, #xmlel{name = <<"fallback">>},
                    #xmlel{name = <<"fallback">>}], ?C:sticker_els())].

receipts_test_() ->
    [?_assertEqual([<<"$b">>],
                   ?C:failed_pdus(<<"{\"pdus\":{\"$a\":{},\"$b\":{\"error\":\"no\"}}}">>)),
     ?_assertEqual([], ?C:failed_pdus(<<"{\"pdus\":[]}">>)),
     ?_assertEqual([], ?C:failed_pdus(<<"not json">>)),
     ?_assertEqual([<<"urn:xmpp:receipts">>, <<"urn:xmpp:chat-markers:0">>],
                   [attr(E, <<"xmlns">>) || E <- ?C:receipt_els(<<"x1">>, [receipt, markable])]),
     ?_assertEqual([<<"x1">>], [attr(E, <<"id">>) || E <- ?C:receipt_els(<<"x1">>, [markable])])].

classify_test_() ->
    C = fun mod_matrix_gw_xmpp:classify/1,
    Ann = jid:decode(<<"ann@example.com/phone">>),
    Msg = fun(Body, Els) -> #message{id = <<"m1">>, from = Ann, type = chat, sub_els = Els,
                                     body = [#text{data = Body} || Body /= <<>>]} end,
    OOB = el(<<"x">>, <<"jabber:x:oob">>, [], [cdata(<<"url">>, <<"https://s/a.png">>)]),
    [?_assertEqual([{text, <<"hi">>}], C(Msg(<<"hi">>, []))),
     ?_assertEqual([{text, <<"hi">>, #{mentions => [<<"xmpp:b@x">>]}}], C(Msg(<<"hi">>, [mention(<<"xmpp:b@x">>)]))),
     %% the receipts asked: XEP-0184 and XEP-0333
     ?_assertEqual([{text, <<"hi">>}, {receipt_request, Ann, [receipt, markable]}],
                   C(Msg(<<"hi">>, [el(<<"request">>, <<"urn:xmpp:receipts">>, [], []),
                                    el(<<"markable">>, <<"urn:xmpp:chat-markers:0">>, [], [])]))),
     ?_assertEqual([{text, <<"hi">>}, {receipt_request, Ann, [receipt]}],
                   C(Msg(<<"hi">>, [el(<<"request">>, <<"urn:xmpp:receipts">>, [], [])]))),
     %% none for a message that is no message
     ?_assertEqual([], C(Msg(<<>>, [el(<<"request">>, <<"urn:xmpp:receipts">>, [], [])]))),
     %% a sticker is a share marked so
     ?_assertEqual([{media, <<"https://s/a.png">>, #{sticker => true}, <<"https://s/a.png">>}],
                   C(Msg(<<"https://s/a.png">>, [OOB, el(<<"sticker">>, <<"urn:xmpp:stickers:0">>, [], [])])))].

%%--------------------------------------------------------------------
%% Matrix to XMPP
%%--------------------------------------------------------------------

inbound_message_test_() ->
    I = fun mod_matrix_gw_xmpp:inbound_message/1,
    E = fun(Type, Content) -> #{<<"type">> => Type, <<"content">> => Content} end,
    Start = #{<<"org.matrix.msc3381.poll.start">> =>
                  #{<<"question">> => #{<<"org.matrix.msc1767.text">> => <<"Q?">>},
                    <<"kind">> => <<"org.matrix.msc3381.poll.undisclosed">>, <<"max_selections">> => 2,
                    <<"answers">> => [#{<<"id">> => <<"x">>, <<"org.matrix.msc1767.text">> => <<"X">>},
                                      #{<<"id">> => <<"y">>, <<"org.matrix.msc1767.text">> => <<"Y">>}]}},
    Stable = #{<<"m.poll">> =>
                   #{<<"question">> => #{<<"m.text">> => [#{<<"body">> => <<"Q?">>}]},
                     <<"kind">> => <<"m.disclosed">>, <<"max_selections">> => 1,
                     <<"answers">> => [#{<<"m.id">> => <<"x">>, <<"m.text">> => [#{<<"body">> => <<"X">>}]},
                                       #{<<"m.id">> => <<"y">>, <<"m.text">> => <<"Y">>}]}},
    Rel = #{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.reference">>, <<"event_id">> => <<"$p">>}},
    [?_assertEqual({poll, #{title => <<"Q?">>, answers => [{<<"x">>, <<"X">>}, {<<"y">>, <<"Y">>}],
                            multiple => true, hide => true}},
                   I(E(<<"org.matrix.msc3381.poll.start">>, Start))),
     ?_assertEqual({poll, #{title => <<"Q?">>, answers => [{<<"x">>, <<"X">>}, {<<"y">>, <<"Y">>}],
                            multiple => false, hide => false}},
                   I(E(<<"m.poll.start">>, Stable))),
     ?_assertEqual(ignore, I(E(<<"m.poll.start">>, #{}))),
     %% the question's body when there is no MSC1767 text; one answer is no poll
     ?_assertEqual(ignore, I(E(<<"org.matrix.msc3381.poll.start">>,
                               #{<<"org.matrix.msc3381.poll.start">> =>
                                     #{<<"question">> => #{<<"body">> => <<"Q">>},
                                       <<"answers">> => [#{<<"id">> => <<"x">>, <<"body">> => <<"X">>}]}}))),
     ?_assertEqual({vote, <<"$p">>, [<<"x">>]},
                   I(E(<<"org.matrix.msc3381.poll.response">>,
                       Rel#{<<"org.matrix.msc3381.poll.response">> => #{<<"answers">> => [<<"x">>, 5]}}))),
     ?_assertEqual({vote, <<"$p">>, [<<"y">>]}, I(E(<<"m.poll.response">>, Rel#{<<"m.selections">> => [<<"y">>]}))),
     ?_assertEqual(ignore, I(E(<<"m.poll.response">>, Rel))),
     ?_assertEqual(ignore, I(E(<<"m.poll.response">>, #{}))),
     ?_assertEqual({poll_end, <<"$p">>}, I(E(<<"org.matrix.msc3381.poll.end">>, Rel))),
     ?_assertEqual({poll_end, <<"$p">>}, I(E(<<"m.poll.end">>, Rel))),
     ?_assertEqual(ignore, I(E(<<"m.poll.end">>, #{}))),
     ?_assertEqual({sticker, #{<<"url">> => <<"mxc://s/x">>}}, I(E(<<"m.sticker">>, #{<<"url">> => <<"mxc://s/x">>}))),
     ?_assertEqual(ignore, I(E(<<"m.sticker">>, #{<<"body">> => <<"no url">>}))),
     %% every msgtype with a body shows it
     ?_assertEqual({text, <<"maintenance">>},
                   I(E(<<"m.room.message">>, #{<<"msgtype">> => <<"m.server_notice">>,
                                               <<"body">> => <<"maintenance">>}))),
     ?_assertEqual({text, <<"Location">>},
                   I(E(<<"m.room.message">>, #{<<"msgtype">> => <<"m.location">>, <<"body">> => <<"Location">>,
                                               <<"geo_uri">> => <<"geo:1,2">>}))),
     ?_assertEqual(ignore, I(E(<<"m.room.message">>, #{<<"msgtype">> => <<"m.text">>, <<"body">> => <<>>}))),
     ?_assert(lists:all(fun ?C:is_content_type/1,
                        [<<"m.sticker">>, <<"org.matrix.msc3381.poll.start">>, <<"org.matrix.msc3381.poll.response">>,
                         <<"org.matrix.msc3381.poll.end">>, <<"m.poll.start">>, <<"m.poll.response">>,
                         <<"m.poll.end">>])),
     ?_assertNot(?C:is_content_type(<<"m.room.message">>))].

reply_fallback_test_() ->
    S = fun ?C:strip_matrix_reply/1,
    [?_assertEqual(<<"my answer">>, S(<<"> <@bob:s> hello\n> more\n\nmy answer">>)),
     ?_assertEqual(<<"a\nb">>, S(<<"> q\n\na\nb">>)),
     %% a quote the sender wrote is not a fallback
     ?_assertEqual(<<"> q\nno blank">>, S(<<"> q\nno blank">>)),
     ?_assertEqual(<<"plain">>, S(<<"plain">>)),
     ?_assertEqual(<<"> a\n> b\n">>, ?C:quote(<<"a\nb">>)),
     ?_assertEqual(3, ?C:cp_len(<<"aé😀"/utf8>>)),
     ?_assertEqual(1, ?C:cp_len(<<255>>)),
     ?_assertEqual(<<"$t">>, ?C:in_reply_to(#{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.thread">>,
                                                                     <<"event_id">> => <<"$t">>}})),
     %% a thread's reply names its parent
     ?_assertEqual(<<"$p">>, ?C:in_reply_to(#{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.thread">>,
                                                                     <<"event_id">> => <<"$t">>,
                                                                     <<"m.in_reply_to">> => #{<<"event_id">> => <<"$p">>}}})),
     ?_assertEqual(undefined, ?C:in_reply_to(#{}))].

ctx() ->
    #{reply_id => fun(<<"$e1">>) -> <<"x1">>; (E) -> E end,
      reply_to => fun(<<"$e1">>) -> <<"ann@example.com">>; (_) -> <<>> end,
      target_body => fun(<<"$e1">>) -> <<"hello\nthere">>; (_) -> undefined end,
      jid_of => fun(<<"@ann:example.com">>) -> <<"ann@example.com">>;
                   (<<"@bob:far">>) -> <<"bob%far@matrix.example.com">>;
                   (_) -> error end}.

inbound_reply_test_() ->
    IE = fun(Content, Body) -> ?C:inbound_els(Content, Body, ctx()) end,
    Reply = fun(E) -> #{<<"m.relates_to">> => #{<<"m.in_reply_to">> => #{<<"event_id">> => E}}} end,
    {Body, [ReplyEl, Fallback]} = IE(Reply(<<"$e1">>), <<"> <@ann:example.com> hello\n\nfine">>),
    {Body2, [ReplyEl2]} = IE(Reply(<<"$e9">>), <<"fine">>),
    [?_assertEqual(<<"> hello\n> there\nfine">>, Body),
     ?_assertEqual([<<"urn:xmpp:reply:0">>, <<"x1">>, <<"ann@example.com">>],
                   [attr(ReplyEl, A) || A <- [<<"xmlns">>, <<"id">>, <<"to">>]]),
     ?_assertEqual([<<"urn:xmpp:reply:0">>, <<"0">>, <<"16">>],
                   [attr(Fallback, <<"for">>) | [fxml:get_path_s(Fallback, [{elem, <<"body">>}, {attr, A}])
                                                 || A <- [<<"start">>, <<"end">>]]]),
     %% the quote is the stripped body's, so read back it is cut away
     ?_assertEqual({<<"fine">>, #{reply => {<<"ann@example.com">>, <<"x1">>}}}, ?C:rel([ReplyEl, Fallback], Body)),
     %% a target not known: the reply alone, no quote
     ?_assertEqual(<<"fine">>, Body2),
     ?_assertEqual(<<>>, attr(ReplyEl2, <<"to">>)),
     ?_assertEqual({<<"x">>, []}, ?C:inbound_els(#{}, <<"x">>, #{})),
     ?_assertMatch({<<"x">>, [#xmlel{name = <<"reply">>}]}, ?C:inbound_els(Reply(<<"$e1">>), <<"x">>, #{}))].

inbound_mentions_test_() ->
    IE = fun(Content, Body) -> ?C:inbound_els(Content, Body, ctx()) end,
    Pill = <<"<a href=\"https://matrix.to/#/@ann:example.com\">Ann E.</a>: hi">>,
    {_, [Ref]} = IE(#{<<"m.mentions">> => #{<<"user_ids">> => [<<"@ann:example.com">>]},
                      <<"formatted_body">> => Pill}, <<"Ann E.: hi">>),
    {_, [Ref2, Room]} = IE(#{<<"m.mentions">> => #{<<"user_ids">> => [<<"@bob:far">>, 7], <<"room">> => true}},
                           <<"hé bob">>),
    {_, [Ref3]} = IE(#{<<"m.mentions">> => #{<<"user_ids">> => [<<"@ann:example.com">>]}}, <<"nobody named">>),
    %% after a reply's quote the span moves by the quote's length
    {_, [_, _, Ref4]} = IE(#{<<"m.mentions">> => #{<<"user_ids">> => [<<"@ann:example.com">>]},
                             <<"m.relates_to">> => #{<<"m.in_reply_to">> => #{<<"event_id">> => <<"$e1">>}}},
                           <<"ann: yes">>),
    [?_assertEqual([<<"mention">>, <<"xmpp:ann@example.com">>, <<"0">>, <<"6">>],
                   [attr(Ref, A) || A <- [<<"type">>, <<"uri">>, <<"begin">>, <<"end">>]]),
     ?_assertEqual([<<"xmpp:bob%far@matrix.example.com">>, <<"3">>, <<"6">>],
                   [attr(Ref2, A) || A <- [<<"uri">>, <<"begin">>, <<"end">>]]),
     ?_assertEqual(<<"urn:xmpp:mentions:0#channel">>, attr(Room, <<"mentions">>)),
     ?_assertEqual(<<>>, attr(Ref3, <<"begin">>)),
     ?_assertEqual([<<"16">>, <<"19">>], [attr(Ref4, A) || A <- [<<"begin">>, <<"end">>]]),
     %% users with no JID, or m.mentions that are not a map
     ?_assertEqual({<<"x">>, []}, IE(#{<<"m.mentions">> => #{<<"user_ids">> => [<<"@x:y">>]}}, <<"x">>)),
     ?_assertEqual({<<"x">>, []}, IE(#{<<"m.mentions">> => #{<<"user_ids">> => 5}}, <<"x">>)),
     ?_assertEqual({<<"x">>, []}, IE(#{<<"m.mentions">> => []}, <<"x">>))].

inbound_geo_test_() ->
    IE = fun(Content, Body) -> ?C:inbound_els(Content, Body, ctx()) end,
    Loc = fun(URI) -> #{<<"msgtype">> => <<"m.location">>, <<"geo_uri">> => URI} end,
    {Body, [Geo, Fallback]} = IE(Loc(<<"geo:51.5,-0.12;u=20">>), <<"Big Ben">>),
    [?_assertEqual(<<"geo:51.5,-0.12;u=20">>, Body),
     ?_assertEqual([<<"20">>, <<"Big Ben">>, <<"51.5">>, <<"-0.12">>],
                   [fxml:get_path_s(Geo, [{elem, N}, cdata])
                    || N <- [<<"accuracy">>, <<"description">>, <<"lat">>, <<"lon">>]]),
     ?_assertEqual(<<"http://jabber.org/protocol/geoloc">>, attr(Fallback, <<"for">>)),
     %% Element's own words are no description
     ?_assertMatch({<<"geo:1,2">>, [#xmlel{children = [_, _]}, _]}, IE(Loc(<<"geo:1,2">>), <<"Location">>)),
     ?_assertMatch({<<"geo:1,2">>, [#xmlel{children = [_, _]}, _]}, IE(Loc(<<"geo:1,2">>), <<"Location geo:1,2">>)),
     ?_assertMatch({<<"geo:1,2">>, [#xmlel{children = [_, _]}, _]}, IE(Loc(<<"geo:1,2">>), <<"geo:1,2">>)),
     %% the altitude and other parameters are read past
     ?_assertMatch({_, [#xmlel{children = [_, _]}, _]}, IE(Loc(<<"geo:1,2,30;crs=wgs84">>), <<"geo:1,2,30;crs=wgs84">>)),
     ?_assertEqual({<<"Somewhere">>, []}, IE(Loc(<<"geo:x">>), <<"Somewhere">>)),
     ?_assertEqual({<<"S">>, []}, IE(Loc(<<"geo:95,0">>), <<"S">>)),
     ?_assertEqual(error, ?C:geo_els(<<"nope">>, <<>>))].

poll_els_test_() ->
    Poll = #{title => <<"Lunch?">>, answers => [{<<"a">>, <<"Pizza">>}, {<<"b">>, <<"Sushi">>}],
             multiple => true, hide => true},
    Options = ?C:poll_options(Poll, false),
    {Body, [PollEl, Fallback, Store]} = ?C:poll_els(Poll, Options, <<"Pick">>),
    [?_assertEqual([{<<"a">>, <<"1️⃣"/utf8>>, <<"Pizza">>}, {<<"b">>, <<"2️⃣"/utf8>>, <<"Sushi">>}], Options),
     ?_assertEqual([{<<"🍕"/utf8>>, <<"🍕"/utf8>>, <<"Pizza">>}],
                   ?C:poll_options(#{answers => [{<<"🍕"/utf8>>, <<"Pizza">>}]}, true)),
     %% past ten answers, the regional indicators
     ?_assertEqual(<<"🇦"/utf8>>,
                   element(2, lists:nth(11, ?C:poll_options(
                                              #{answers => [{integer_to_binary(N), <<"x">>}
                                                            || N <- lists:seq(1, 12)]}, false)))),
     ?_assertEqual(<<"📊 Poll: Lunch?\nPick\n1️⃣ Pizza\n2️⃣ Sushi"/utf8>>, Body),
     %% read back, the poll it was
     ?_assertEqual({Body, #{poll => #{title => <<"Lunch?">>, description => <<"Pick">>,
                                      options => [{<<"1️⃣"/utf8>>, <<"Pizza">>}, {<<"2️⃣"/utf8>>, <<"Sushi">>}],
                                      multiple => true, hide => true}}},
                   ?C:rel([PollEl, Fallback, Store], Body)),
     ?_assertEqual(<<"urn:fluux:poll:0">>, attr(Fallback, <<"for">>)),
     ?_assertMatch({_, [#xmlel{attrs = [{<<"xmlns">>, _}]}, _, _]},
                   ?C:poll_els(Poll#{multiple => false, hide => false}, Options, <<>>))].

poll_closed_test_() ->
    Poll = #{title => <<"Lunch?">>, answers => [{<<"a">>, <<"Pizza">>}, {<<"b">>, <<"Sushi">>}]},
    Options = ?C:poll_options(Poll, false),
    One = <<"1️⃣"/utf8>>, Two = <<"2️⃣"/utf8>>,
    Votes = [{<<"bob:s">>, [One, Two, <<"👍"/utf8>>]}, {<<"carol:s">>, [Two]}, {<<"dave:s">>, [<<"👍"/utf8>>]}],
    {Body, [El, _, _]} = ?C:poll_closed_els(Poll, Options, Votes, <<"P">>),
    {_, [ElM, _, _]} = ?C:poll_closed_els(Poll#{multiple => true}, Options, Votes, <<"P">>),
    Tally = fun(E) -> [{attr(T, <<"emoji">>), attr(T, <<"count">>), attr(T, <<"voters">>)}
                       || #xmlel{name = <<"tally">>} = T <- E#xmlel.children] end,
    [%% single answer: bob's last option in option order counts
     ?_assertEqual(<<"📊 Poll closed: Lunch?\n1️⃣ Pizza: 0\n2️⃣ Sushi: 2"/utf8>>, Body),
     ?_assertEqual([{One, <<"0">>, <<>>}, {Two, <<"2">>, <<"bob:s,carol:s">>}], Tally(El)),
     ?_assertEqual([<<"P">>, <<"Lunch?">>], [attr(El, <<"message-id">>), fxml:get_path_s(El, [{elem, <<"title">>}, cdata])]),
     ?_assertEqual([{One, <<"1">>, <<"bob:s">>}, {Two, <<"2">>, <<"bob:s,carol:s">>}], Tally(ElM))].

misc_test_() ->
    [?_assertEqual(<<"Ann">>, fxml:get_tag_cdata(?C:nick_el(<<"Ann">>))),
     ?_assertEqual(<<"http://jabber.org/protocol/nick">>, attr(?C:nick_el(<<"Ann">>), <<"xmlns">>)),
     ?_assertEqual(error, ?C:poll_response(#{})),
     ?_assertEqual(error, ?C:poll_end_target(#{})),
     ?_assertEqual(error, ?C:poll_start(#{}))].
