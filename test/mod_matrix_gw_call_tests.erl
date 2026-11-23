%%% mod_matrix_gw_call's tests: the element and the PDU on their own, then
%%% the whole path on an ejabberd started in this node with the gateway on:
%%% a call event from a local user into a new direct room and into the same
%%% room again (read back from the room process), the refusals, the
%%% sender's own event not echoed, and the disco feature. The remote server
%%% here, remote.invalid, resolves nowhere.
-module(mod_matrix_gw_call_tests).

-export([ejabberd_cases/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(NS, <<"https://ns.another.im/matrix-e2e/0">>).
-define(CALLS, <<"https://ns.another.im/matrix-e2e/0#calls">>).
-define(HOST, <<"example.com">>).
-define(GW, <<"matrix.example.com">>).
-define(ANN, <<"ann@example.com">>).
-define(ANN_RES, <<"ann@example.com/phone">>).
-define(ANN_ID, <<"@ann:example.com">>).
-define(TYPES, [<<"m.call.invite">>, <<"m.call.answer">>, <<"m.call.candidates">>,
                <<"m.call.select_answer">>, <<"m.call.negotiate">>, <<"m.call.reject">>,
                <<"m.call.hangup">>, <<"m.call.sdp_stream_metadata_changed">>]).

j(S) -> jid:decode(S).

event_el(Type, Json) ->
    #xmlel{name = <<"event">>, attrs = [{<<"xmlns">>, ?NS}, {<<"type">>, Type}],
           children = [{xmlcdata, Json}]}.

%% A message from From to To carrying an <event/> of Type with Json.
call_msg(From, To, Type, Json) ->
    #message{id = <<"c1">>, type = chat, from = j(From), to = j(To),
             sub_els = [event_el(Type, Json)]}.

content(CallID) ->
    #{<<"call_id">> => CallID, <<"party_id">> => <<"abcdefgh">>, <<"version">> => <<"1">>}.

json(Map) -> iolist_to_binary(json:encode(Map)).

fields(Type, Sender) ->
    #{type => Type, id => <<"$ev1">>, room => <<"!r:remote.invalid">>, sender => Sender,
      ts => 1761390000000, content => content(<<"c-1">>)}.

%%--------------------------------------------------------------------
%% On their own
%%--------------------------------------------------------------------
pure_test_() ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(xmpp) end,
     [fun forward_element/0, fun forward_ignored/0, fun disco/0,
      fun passed_and_ignored/0]}.

%% Each m.call.* type becomes the <event/> of §2.5 with the content as JSON,
%% beside upstream's <x xmlns='p1:matrix'/>, from the peer to the user.
forward_element() ->
    Bob = j(<<"bob%remote.invalid@", ?GW/binary>>),
    lists:foreach(
      fun(Type) ->
              Msg = mod_matrix_gw_call:forward_message(Bob, j(?ANN), fields(Type, <<"@bob:remote.invalid">>)),
              ?assertMatch(#message{type = chat, body = []}, Msg),
              ?assertEqual(Bob, Msg#message.from),
              ?assertEqual(j(?ANN), Msg#message.to),
              [Ev, X] = Msg#message.sub_els,
              ?assertEqual(<<"event">>, Ev#xmlel.name),
              ?assertEqual([{<<"xmlns">>, ?NS}, {<<"type">>, Type}, {<<"id">>, <<"$ev1">>},
                            {<<"room">>, <<"!r:remote.invalid">>},
                            {<<"sender">>, <<"@bob:remote.invalid">>},
                            {<<"ts">>, <<"1761390000000">>}], Ev#xmlel.attrs),
              ?assertEqual(content(<<"c-1">>), json:decode(fxml:get_tag_cdata(Ev))),
              ?assertEqual(#xmlel{name = <<"x">>,
                                  attrs = [{<<"xmlns">>, <<"p1:matrix">>},
                                           {<<"room_id">>, <<"!r:remote.invalid">>}]}, X),
              %% it encodes as a stanza
              ?assertMatch(#xmlel{name = <<"message">>}, xmpp:encode(Msg))
      end, ?TYPES),
    %% no stamp, no ts
    NoTS = mod_matrix_gw_call:forward_message(Bob, j(?ANN), (fields(<<"m.call.hangup">>, <<"@b:r">>))#{ts => undefined}),
    [Ev | _] = NoTS#message.sub_els,
    ?assertEqual(<<>>, fxml:get_tag_attr_s(<<"ts">>, Ev)).

%% The user's own event (another device's, or the gateway's echo) is not
%% sent back to her; a sender that is no user id is nobody.
forward_ignored() ->
    F = fields(<<"m.call.invite">>, ?ANN_ID),
    ?assertEqual(ignore, mod_matrix_gw_call:forward_message(j(?ANN), j(?ANN), F)),
    ?assertEqual(ignore, mod_matrix_gw_call:forward_message(j(?ANN_RES), j(?ANN), F)),
    ?assertEqual(ignore, mod_matrix_gw_call:forward_message(error, j(?ANN), F)),
    ?assertEqual(ok, mod_matrix_gw_call:forward(error, j(?ANN), F)).

disco() ->
    ?assertEqual([?CALLS], mod_matrix_gw_call:disco_features(j(?GW))),
    ?assertEqual([?CALLS], mod_matrix_gw_call:disco_features(j(<<"bob%remote.invalid@", ?GW/binary>>))),
    ?assertEqual([], mod_matrix_gw_call:disco_features(j(<<"!abc%remote.invalid@", ?GW/binary>>))),
    ?assertEqual([], mod_matrix_gw_call:disco_features(j(<<"#room%remote.invalid@", ?GW/binary>>))).

%% A call event is a room event like any other (mod_matrix_gw_event): what
%% is not one goes on to the gateway's own clauses (`pass`); one from
%% another server is dropped, as upstream drops it.
passed_and_ignored() ->
    To = <<"bob%remote.invalid@", ?GW/binary>>,
    R = fun(Msg) -> mod_matrix_gw_event:parse_message(Msg, ?HOST) end,
    Call = call_msg(?ANN_RES, To, <<"m.call.invite">>, <<"{}">>),
    Plain = #message{type = chat, from = j(?ANN_RES), to = j(To), body = [#text{data = <<"hi">>}]},
    ?assertEqual(pass, R(Plain)),
    ?assertEqual(pass, R(Plain#message{sub_els = [#xmlel{name = <<"event">>, attrs = [{<<"xmlns">>, <<"other">>}]}]})),
    ?assertEqual(pass, R(Call#message{type = error})),
    ?assertEqual({event, <<"m.call.invite">>, #{}}, R(Call)),
    ?assertEqual(ignore, R(Call#message{from = j(<<"eve@elsewhere.example/x">>)})).

%%--------------------------------------------------------------------
%% On a running gateway
%%--------------------------------------------------------------------
ejabberd_test_() ->
    mod_matrix_gw_test_lib:in_peer(?MODULE, fun ?MODULE:ejabberd_cases/0).

ejabberd_cases() ->
    {setup, fun() -> mod_matrix_gw_test_lib:start_ejabberd(?MODULE) end, fun mod_matrix_gw_test_lib:stop_ejabberd/1,
     fun(_) ->
             [{timeout, 30, {Name, F}}
              || {Name, F} <- [{"a call into a new direct room, then the same room", fun new_and_existing/0},
                               {"the first text message still creates the room", fun text_creates/0},
                               {"a state event type: not-acceptable", fun refused/0},
                               {"content that is no JSON object: bad-request", fun bad_json/0},
                               {"from another server: nothing", fun remote_sender/0},
                               {"a peer's call event is forwarded", fun forwarded/0},
                               {"disco#info of the gateway and its JIDs", fun disco_answers/0}]]
     end}.

%% A session of ann's at Full, delivered to this process.
session(Full) ->
    #jid{luser = U, lserver = S, lresource = R} = j(Full),
    ejabberd_sm:open_session({erlang:timestamp(), self()}, U, S, R, 0, []).

close(Full) ->
    #jid{luser = U, lserver = S, lresource = R} = j(Full),
    case ejabberd_sm:get_session_sid(U, S, R) of
        none -> ok;
        SID -> ejabberd_sm:close_session(SID, U, S, R)
    end.

%% What reached this process within a moment.
received() ->
    receive
        {route, Pkt} -> [Pkt | received()]
    after 300 -> []
    end.

%% The direct room of ann's with Peer: its process, waited for.
direct_pid(Peer) ->
    direct_pid(Peer, 100).
direct_pid(Peer, Tries) ->
    case mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, Peer}) of
        [{matrix_direct, _, RoomID}] ->
            {ok, Pid} = mod_matrix_gw_room:get_room_pid(?HOST, RoomID),
            {RoomID, Pid};
        [] when Tries > 0 ->
            timer:sleep(20),
            direct_pid(Peer, Tries - 1);
        [] ->
            error({no_direct_room, Peer})
    end.

%% The room's events from its latest back to m.room.create, oldest first,
%% each as its JSON (the record stays the room module's).
chain(Pid) ->
    Latest = gen_statem:call(Pid, get_latest_events),
    [Last] = maps:keys(Latest),
    lists:reverse(walk(Pid, Last)).

walk(Pid, ID) ->
    {ok, Event} = gen_statem:call(Pid, {find_event, ID}),
    [JSON] = [M || M <- tuple_to_list(Event), is_map(M), is_map_key(<<"prev_events">>, M)],
    case maps:get(<<"prev_events">>, JSON) of
        [] -> [JSON];
        [Prev | _] -> [JSON | walk(Pid, Prev)]
    end.

types(Chain) -> [maps:get(<<"type">>, E) || E <- Chain].

%% ann calls bob: the room is made with upstream's creation events, then the
%% invite as she sent it; her hangup goes into the same room. Neither is
%% echoed to her.
new_and_existing() ->
    session(?ANN_RES),
    To = <<"bob%remote.invalid@", ?GW/binary>>,
    Invite = (content(<<"c-new">>))#{<<"lifetime">> => 60000,
                                     <<"offer">> => #{<<"type">> => <<"offer">>, <<"sdp">> => <<"v=0\r\n">>}},
    ok = mod_matrix_gw_room:route(call_msg(?ANN_RES, To, <<"m.call.invite">>, json(Invite))),
    {RoomID, Pid} = direct_pid(<<"@bob:remote.invalid">>),
    Chain = chain(Pid),
    ?assertEqual([<<"m.room.create">>, <<"m.room.member">>, <<"m.room.power_levels">>,
                  <<"m.room.join_rules">>, <<"m.room.history_visibility">>,
                  <<"m.room.guest_access">>, <<"m.room.member">>, <<"m.call.invite">>],
                 types(Chain)),
    Last = lists:last(Chain),
    ?assertEqual(Invite, maps:get(<<"content">>, Last)),
    ?assertEqual(?ANN_ID, maps:get(<<"sender">>, Last)),
    ?assertEqual(RoomID, maps:get(<<"room_id">>, Last)),
    %% the same room for the next one
    Hangup = (content(<<"c-new">>))#{<<"reason">> => <<"user_hangup">>},
    ok = mod_matrix_gw_room:route(call_msg(?ANN_RES, To, <<"m.call.hangup">>, json(Hangup))),
    ?assertEqual({RoomID, Pid}, direct_pid(<<"@bob:remote.invalid">>)),
    Chain2 = chain(Pid),
    ?assertEqual(Chain, lists:droplast(Chain2)),
    ?assertMatch(#{<<"type">> := <<"m.call.hangup">>, <<"content">> := Hangup,
                   <<"sender">> := ?ANN_ID}, lists:last(Chain2)),
    %% her own events are not sent back to her: what she gets is the
    %% gateway's <sent/> for each, from the gateway
    Got = [M || #message{} = M <- received()],
    ?assertEqual([j(?GW), j(?GW)], [M#message.from || M <- Got]),
    ?assertEqual([[<<"sent">>, <<"no-store">>], [<<"sent">>, <<"no-store">>]],
                 [[N || #xmlel{name = N} <- M#message.sub_els] || M <- Got]),
    close(?ANN_RES).

%% The body path, which shares the room creation now: unchanged.
text_creates() ->
    To = <<"carol%remote.invalid@", ?GW/binary>>,
    ok = mod_matrix_gw_room:route(#message{type = chat, from = j(?ANN_RES), to = j(To),
                                           body = [#text{data = <<"hello">>}]}),
    {_, Pid} = direct_pid(<<"@carol:remote.invalid">>),
    Chain = chain(Pid),
    ?assertEqual(8, length(Chain)),
    ?assertMatch(#{<<"type">> := <<"m.room.message">>,
                   <<"content">> := #{<<"body">> := <<"hello">>, <<"msgtype">> := <<"m.text">>}},
                 lists:last(Chain)).

%% Errors come back to the sender's full JID.
error_of(Msg) ->
    session(?ANN_RES),
    ok = mod_matrix_gw_room:route(Msg),
    Got = [M || #message{type = error} = M <- received()],
    close(?ANN_RES),
    [Err] = Got,
    xmpp:get_error(Err).

refused() ->
    To = <<"dave%remote.invalid@", ?GW/binary>>,
    ?assertMatch(#stanza_error{reason = 'not-acceptable'},
                 error_of(call_msg(?ANN_RES, To, <<"m.room.member">>, <<"{}">>))),
    ?assertMatch(#stanza_error{reason = 'not-acceptable'},
                 error_of(call_msg(?ANN_RES, To, <<"m.room.power_levels">>, <<"{}">>))),
    ?assertEqual([], mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, <<"@dave:remote.invalid">>})).

bad_json() ->
    To = <<"dave%remote.invalid@", ?GW/binary>>,
    ?assertMatch(#stanza_error{reason = 'bad-request'},
                 error_of(call_msg(?ANN_RES, To, <<"m.call.invite">>, <<"not json">>))),
    ?assertMatch(#stanza_error{reason = 'bad-request'},
                 error_of(call_msg(?ANN_RES, To, <<"m.call.invite">>, <<"[1, 2]">>))),
    ?assertEqual([], mnesia:dirty_read(matrix_direct, {{<<"ann">>, ?HOST}, <<"@dave:remote.invalid">>})).

remote_sender() ->
    To = <<"erin%remote.invalid@", ?GW/binary>>,
    ok = mod_matrix_gw_room:route(call_msg(<<"eve@elsewhere.example/x">>, To, <<"m.call.invite">>, <<"{}">>)),
    timer:sleep(100),
    ?assertEqual([], [K || {_, <<"@erin:remote.invalid">>} = K <- mnesia:dirty_all_keys(matrix_direct)]).

%% forward/3 routes the element to the user's sessions.
forwarded() ->
    session(?ANN_RES),
    Bob = j(<<"bob%remote.invalid@", ?GW/binary>>),
    ok = mod_matrix_gw_call:forward(Bob, j(?ANN), fields(<<"m.call.invite">>, <<"@bob:remote.invalid">>)),
    Got = [M || #message{} = M <- received()],
    close(?ANN_RES),
    [Msg] = Got,
    ?assertEqual(Bob, Msg#message.from),
    [#xmlel{name = <<"event">>} = Ev | _] = Msg#message.sub_els,
    ?assertEqual(<<"m.call.invite">>, fxml:get_tag_attr_s(<<"type">>, Ev)).

disco_answers() ->
    Info = #disco_info{node = <<>>},
    #iq{type = result, sub_els = [Gw]} =
        mod_matrix_gw:process_disco_info(#iq{type = get, id = <<"d1">>, from = j(?ANN_RES), to = j(?GW),
                                             sub_els = [Info]}),
    ?assert(lists:member(?CALLS, Gw#disco_info.features)),
    ?assert(lists:member(?CALLS, features(<<"bob%remote.invalid@", ?GW/binary>>))),
    ?assertNot(lists:member(?CALLS, features(<<"!abc%remote.invalid@", ?GW/binary>>))),
    %% a Matrix user's JID is gateway/matrix, no MUC; a room's
    %% conference/text
    ?assertEqual([#identity{category = <<"gateway">>, type = <<"matrix">>}],
                 identities(<<"bob%remote.invalid@", ?GW/binary>>)),
    ?assertNot(lists:member(<<"http://jabber.org/protocol/muc">>,
                            features(<<"bob%remote.invalid@", ?GW/binary>>))),
    ?assertEqual([#identity{category = <<"conference">>, type = <<"text">>}],
                 identities(<<"!abc%remote.invalid@", ?GW/binary>>)).

identities(To) ->
    session(?ANN_RES),
    ok = mod_matrix_gw_room:route(#iq{type = get, id = <<"d3">>, from = j(?ANN_RES), to = j(To),
                                      sub_els = [xmpp:encode(#disco_info{node = <<>>})]}),
    [#iq{sub_els = [El]}] = [IQ || #iq{type = result} = IQ <- received()],
    close(?ANN_RES),
    #disco_info{identities = Ids} = case El of
                                        #xmlel{} -> xmpp:decode(El);
                                        _ -> El
                                    end,
    Ids.

%% The disco#info answer for To at the gateway, as a client gets it.
features(To) ->
    session(?ANN_RES),
    ok = mod_matrix_gw_room:route(#iq{type = get, id = <<"d2">>, from = j(?ANN_RES), to = j(To),
                                      sub_els = [xmpp:encode(#disco_info{node = <<>>})]}),
    Got = [IQ || #iq{type = result} = IQ <- received()],
    close(?ANN_RES),
    [#iq{sub_els = [El]}] = Got,
    #disco_info{features = Features} = case El of
                                           #xmlel{} -> xmpp:decode(El);
                                           _ -> El
                                       end,
    Features.
