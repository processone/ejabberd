%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_rtc.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : MatrixRTC calls through the gateway (https://ns.another.im/matrix-rtc/0)
%%% Created : 11 Nov 2026 by Paavel A. <pd@narayana.im>
%%%
%%% MatrixRTC (MSC4143) for the gateway's users (an XMPP client's
%%% wire, https://ns.another.im/matrix-rtc/0). The gateway moves a MatrixRTC call's
%%% membership events, delayed events and OpenID tokens; it never holds a
%%% media key and never sees a frame.
%%%
%%%   the RTC types  org.matrix.msc3401.call.member (state, generation A),
%%%                  org.matrix.msc4143.rtc.member / m.rtc.member (sticky,
%%%                  generation B, read only here), the slot types (state),
%%%                  the MSC4075 notification types and the MSC4310 decline
%%%                  (timeline)
%%%   device -> G    <send/> and <room-state/> to the chat JID (the room
%%%                  process does the adding, mod_matrix_gw_room:rtc_iq/3),
%%%                  <delayed/>, <transports/>, <openid/>, <livekit-token/>
%%%                  to G (route_iq/1); every iq from a resource whose
%%%                  XEP-0115 caps lack https://ns.another.im/matrix-rtc/0 is
%%%                  feature-not-implemented
%%%   G -> device    each RTC event a room takes (the device's own echoed)
%%%                  as <message><state/></message> from the chat JID to
%%%                  each resource of the local members whose caps carry
%%%                  the feature (forward/4), after the owner check at the
%%%                  edge and the per-origin budget
%%%   delayed        MSC4140 for our users: matrix_delayed rows (disc), a
%%%                  timer each in this module's process, fired through
%%%                  the room with the auth of that moment; the rows of a
%%%                  resource gone offline still fire (the dead man's
%%%                  switch); dropped on a leave of their room and with
%%%                  the account
%%%   OpenID         matrix_openid tokens (disc, an hour) and the
%%%                  federation's GET /_matrix/federation/v1/openid/userinfo
%%%   audit          every send, forward, forgery, fire, token and userinfo
%%%                  hit counted (status/1, the matrix_gw_rtc_status
%%%                  command) and logged at info
%%%   the log        no OpenID token in ejabberd's log at any level: a
%%%                  primary logger filter (redact_log/2, installed by
%%%                  init/1) masks it where the router's, the stream's and
%%%                  ejabberd_http's debug lines would print it
%%%
%%% Not here (1.3.x): sending sticky events (MSC4354: the carry through
%%% signing, the push to joining servers, the in-room map) and the MSC4195
%%% get_token proxy; <send sticky_ms/> and <livekit-token/> answer
%%% feature-not-implemented. The seams: check_send/3's member_b clause and
%%% route_iq/1's livekit-token clause.
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------
-module(mod_matrix_gw_rtc).

-behaviour(gen_server).

-export([create_db/0, start_link/1, procname/1,
         route_iq/1, route_chat_iq/1, disco_features/1,
         kind/1, is_rtc_type/1, power_levels/0,
         forward/4, state_el/1, owner_ok/4, sticky_left/2,
         send_result/3, send_error/3, schedule/2, room_state_result/2,
         set_presence_hook/4, unset_presence_hook/4, remove_user/2,
         membership/4, userinfo/1, audit/5, status/1, status_command/1,
         get_commands_spec/0, redact_log/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-ifdef(TEST).
-export([parse_send/1, check_send/3, has_feature/2, mem/1, timers/1]).
-endif.

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_commands.hrl").

-define(NS, <<"https://ns.another.im/matrix-rtc/0">>).
-define(MAX_CONTENT, 16384).
-define(MAX_STICKY, 3600000).
-define(TXN_TTL, 300000).
-define(PURGE_EVERY, 60000).
-define(OPENID_TTL, 3600).
-define(OPENID_PURGE_EVERY, 3600000).
-define(STATE_USER, 60).
-define(DELAYED_USER, 120).
-define(OPENID_USER, 10).
-define(USERINFO_IP, 60).
-define(STATE_ORIGIN, 600).

-record(matrix_delayed,
        {id :: binary(),
         us :: {binary(), binary()},
         resource :: binary(),
         room :: binary(),
         type :: binary(),
         state_key :: binary() | undefined,
         content :: map(),
         delay_ms :: non_neg_integer(),
         due_ts :: integer(),
         created_ts :: integer()}).

-record(matrix_openid,
        {token :: binary() | '$1',
         mxid :: binary() | '_',
         expires_ts :: integer() | '_' | '$2'}).

create_db() ->
    ejabberd_mnesia:create(?MODULE, matrix_delayed,
                           [{disc_copies, [node()]}, {type, set},
                            {index, [us]},
                            {attributes, record_info(fields, matrix_delayed)}]),
    ejabberd_mnesia:create(?MODULE, matrix_openid,
                           [{disc_copies, [node()]}, {type, set},
                            {attributes, record_info(fields, matrix_openid)}]),
    ok.

%%--------------------------------------------------------------------
%% The RTC types
%%--------------------------------------------------------------------

-spec kind(binary()) -> member_a | member_b | slot | notification | decline | none.
kind(<<"org.matrix.msc3401.call.member">>) -> member_a;
kind(<<"org.matrix.msc4143.rtc.member">>) -> member_b;
kind(<<"m.rtc.member">>) -> member_b;
kind(<<"org.matrix.msc4143.rtc.slot">>) -> slot;
kind(<<"m.rtc.slot">>) -> slot;
kind(<<"org.matrix.msc4075.rtc.notification">>) -> notification;
kind(<<"m.rtc.notification">>) -> notification;
kind(<<"org.matrix.msc4075.call.notify">>) -> notification;
kind(<<"m.call.notify">>) -> notification;
kind(<<"org.matrix.msc4310.rtc.decline">>) -> decline;
kind(_) -> none.

-spec is_rtc_type(term()) -> boolean().
is_rtc_type(Type) when is_binary(Type) -> kind(Type) /= none;
is_rtc_type(_) -> false.

%% The `events` entries of the power levels of a room the gateway makes:
%% anyone may be in a call and ring, a slot is a moderator's.
-spec power_levels() -> map().
power_levels() ->
    maps:from_list(
      [{T, 0} || T <- [<<"org.matrix.msc3401.call.member">>, <<"org.matrix.msc4143.rtc.member">>,
                       <<"m.rtc.member">>, <<"org.matrix.msc4075.rtc.notification">>,
                       <<"m.rtc.notification">>, <<"org.matrix.msc4075.call.notify">>,
                       <<"m.call.notify">>]] ++
          [{T, 50} || T <- [<<"org.matrix.msc4143.rtc.slot">>, <<"m.rtc.slot">>]]).

-spec disco_features(jid()) -> [binary()].
disco_features(_) ->
    [?NS].

%%--------------------------------------------------------------------
%% The owner check at the edge (and on <send/>)
%%--------------------------------------------------------------------

%% Whether an event of Type, StateKey and Content may be Sender's: a
%% generation A member's state key is the sender's (`_<mxid>_<device>…`,
%% or the older `<mxid>_<device>…` and `<mxid>` forms), a generation B
%% member names the sender and its sticky_key is its member id.
-spec owner_ok(binary(), binary() | undefined, binary(), term()) -> boolean().
owner_ok(Type, StateKey, Sender, Content) ->
    case kind(Type) of
        member_a when is_binary(StateKey) ->
            StateKey == Sender
                orelse prefix(<<"_", Sender/binary, "_">>, StateKey)
                orelse prefix(<<Sender/binary, "_">>, StateKey);
        member_a ->
            false;
        member_b ->
            case Content of
                #{<<"member">> := #{<<"user_id">> := Sender, <<"id">> := ID},
                  <<"sticky_key">> := ID} when is_binary(ID), ID /= <<>> ->
                    true;
                _ ->
                    false
            end;
        _ ->
            true
    end.

prefix(P, B) ->
    S = byte_size(P),
    byte_size(B) > S andalso binary:part(B, 0, S) == P.

%% MSC4354's stickiness left of a PDU (JSON) at Now, in ms (0 when over),
%% or none for a PDU with no stickiness: from min(received, origin ts),
%% the duration clamped to an hour.
-spec sticky_left(map(), integer()) -> non_neg_integer() | none.
sticky_left(JSON, Now) ->
    Sticky = case JSON of
                 #{<<"sticky">> := S} -> S;
                 #{<<"msc4354_sticky">> := S} -> S;
                 _ -> none
             end,
    case Sticky of
        #{<<"duration_ms">> := D} when is_integer(D), D >= 0 ->
            TS = case maps:get(<<"origin_server_ts">>, JSON, Now) of
                     T when is_integer(T) -> min(T, Now);
                     _ -> Now
                 end,
            max(0, TS + min(D, ?MAX_STICKY) - Now);
        _ ->
            none
    end.

%%--------------------------------------------------------------------
%% Matrix -> XMPP: <state/>
%%--------------------------------------------------------------------

-type fields() :: #{type := binary(), id := binary(), room := binary(),
                    sender := binary(), ts := integer() | undefined,
                    content := term(), state_key => binary(),
                    sticky_ms => non_neg_integer()}.

-spec state_el(fields()) -> xmlel().
state_el(#{type := Type, id := ID, room := Room, sender := Sender} = F) ->
    Attrs = [{<<"xmlns">>, ?NS}, {<<"type">>, Type}, {<<"id">>, ID},
             {<<"room">>, Room}, {<<"sender">>, Sender}] ++
        [{<<"ts">>, integer_to_binary(TS)} || TS <- [maps:get(ts, F, undefined)], is_integer(TS)] ++
        [{<<"state_key">>, K} || K <- [maps:get(state_key, F, undefined)], is_binary(K)] ++
        [{<<"sticky_ms">>, integer_to_binary(S)} || S <- [maps:get(sticky_ms, F, undefined)],
                                                   is_integer(S)],
    #xmlel{name = <<"state">>, attrs = Attrs,
           children = [{xmlcdata, iolist_to_binary(misc:json_encode(maps:get(content, F, #{})))}]}.

%% An RTC event of the room the gateway has, to Recipients
%% [{UserBareJID, ChatJID, chat | groupchat}]: each resource of theirs
%% whose caps carry the feature gets the <state/> (with <no-copy/>: each
%% is sent its own, so carbons would only reach the ones that lack it).
%% An event that fails the owner check, and one past its origin's budget,
%% is stored by the room and not forwarded.
-spec forward(binary(), fields(), binary(), [{jid(), jid(), chat | groupchat}]) -> ok.
forward(Host, #{type := Type, sender := Sender, room := Room} = F, Origin, Recipients) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    case owner_ok(Type, maps:get(state_key, F, undefined), Sender, maps:get(content, F, #{})) of
        false ->
            audit(Host, forged, Sender, Room, #{type => Type, id => maps:get(id, F)}),
            ok;
        true when Origin /= Domain ->
            case mod_matrix_gw_s2s:rate({rtc_state_origin, Origin},
                                        mod_matrix_gw_s2s:limit(rtc_state_origin, ?STATE_ORIGIN),
                                        60000, 1) of
                true ->
                    deliver(Host, F, Recipients);
                false ->
                    audit(Host, origin_over, Origin, Room, #{type => Type}),
                    ok
            end;
        true ->
            deliver(Host, F, Recipients)
    end.

deliver(Host, #{room := Room, type := Type} = F, Recipients) ->
    El = state_el(F),
    X = #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, <<"p1:matrix">>}, {<<"room_id">>, Room}]},
    Sent = length(
             [ejabberd_router:route(
                #message{id = p1_rand:get_string(), type = MsgType, from = Chat,
                         to = jid:make(U, S, R),
                         sub_els = [El, X, mod_matrix_gw_xmpp:no_store_el(),
                                    #xmlel{name = <<"no-copy">>,
                                           attrs = [{<<"xmlns">>, <<"urn:xmpp:hints">>}]}]})
              || {#jid{luser = U, lserver = S}, Chat, MsgType} <- Recipients,
                 R <- resources(U, S),
                 has_feature(Host, {U, S, R})]),
    audit(Host, forwarded, maps:get(sender, F), Room, #{type => Type, to => Sent}),
    ok.

-ifdef(TEST).
resources(U, S) ->
    case persistent_term:get({?MODULE, resources}, undefined) of
        undefined -> ejabberd_sm:get_user_resources(U, S);
        Fun -> Fun(U, S)
    end.
-else.
resources(U, S) ->
    ejabberd_sm:get_user_resources(U, S).
-endif.

%%--------------------------------------------------------------------
%% The capability gate
%%--------------------------------------------------------------------

%% Whether the resource {LUser, LServer, LResource} advertises the
%% feature in its XEP-0115 caps (kept from its last presence, resolved by
%% mod_caps).
-ifdef(TEST).
has_feature(Host, LJID) ->
    case persistent_term:get({?MODULE, features}, undefined) of
        undefined -> caps_feature(Host, LJID);
        Fun -> Fun(LJID)
    end.
-else.
has_feature(Host, LJID) ->
    caps_feature(Host, LJID).
-endif.

caps_feature(Host, {U, S, R}) ->
    case catch ets:lookup(mem(Host), {caps, U, S, R}) of
        [{_, Caps}] ->
            try lists:member(?NS, mod_caps:get_features(S, Caps))
            catch _:_ -> false
            end;
        _ ->
            false
    end.

-spec set_presence_hook(binary(), binary(), binary(), presence()) -> any().
set_presence_hook(User, Server, Resource, Presence) ->
    LServer = jid:nameprep(Server),
    Key = {caps, jid:nodeprep(User), LServer, jid:resourceprep(Resource)},
    Caps = try mod_caps:read_caps(Presence) catch _:_ -> nothing end,
    catch ets:insert(mem(LServer), {Key, Caps}),
    ok.

%% A resource gone: its caps forgotten; its delayed rows keep their due
%% time (at most their delay from now: the dead man's switch).
-spec unset_presence_hook(binary(), binary(), binary(), binary()) -> any().
unset_presence_hook(User, Server, Resource, _Status) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LRes = jid:resourceprep(Resource),
    catch ets:delete(mem(LServer), {caps, LUser, LServer, LRes}),
    catch gen_server:cast(procname(LServer), {offline, {LUser, LServer}, LRes}),
    ok.

%%--------------------------------------------------------------------
%% Device -> gateway: the chat's iqs
%%--------------------------------------------------------------------

%% An iq to a chat JID: pass when it is not of this namespace, else
%% answered (here, or by the room's process).
-spec route_chat_iq(iq()) -> pass | ok.
route_chat_iq(#iq{sub_els = [#xmlel{name = Name, attrs = Attrs} = El]} = IQ)
  when Name == <<"send">>; Name == <<"room-state">> ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS -> chat_iq(IQ, Name, El);
        _ -> pass
    end;
route_chat_iq(_) ->
    pass.

chat_iq(#iq{type = Type, lang = Lang} = IQ, Name, El) ->
    Host = ejabberd_config:get_myname(),
    case gate(Host, IQ) of
        {error, Err} ->
            reply(IQ, Err);
        {ok, US, LRes} ->
            case {Name, Type} of
                {<<"send">>, set} ->
                    chat_send(Host, IQ, US, LRes, El);
                {<<"room-state">>, get} ->
                    Types = [T || T <- [fxml:get_attr_s(<<"type">>, El#xmlel.attrs)
                                        | [fxml:get_tag_cdata(C) || #xmlel{name = <<"type">>} = C
                                                                       <- El#xmlel.children]],
                                  T /= <<>>],
                    case Types of
                        [] -> reply(IQ, xmpp:err_bad_request(<<"no type">>, Lang));
                        _ -> mod_matrix_gw_room:rtc_route(IQ, {room_state, lists:usort(Types)})
                    end;
                _ ->
                    reply(IQ, xmpp:err_bad_request(<<"wrong iq type">>, Lang))
            end
    end,
    ok.

chat_send(Host, #iq{lang = Lang} = IQ, {U, _} = US, LRes, El) ->
    MXID = mxid(Host, U),
    case parse_send(El) of
        {error, Why} ->
            reply(IQ, err(Why, Lang));
        {ok, Send} ->
            Dev = device_of(Host, U, LRes),
            case check_send(Send, MXID, Dev) of
                {error, Why} ->
                    reply(IQ, err(Why, Lang));
                ok ->
                    TxnKey = txn_key(US, LRes, maps:get(txn, Send)),
                    case txn_lookup(Host, TxnKey) of
                        {ok, Result} ->
                            reply(IQ, Result);
                        none ->
                            case mod_matrix_gw_s2s:rate({rtc_state_user, US},
                                                        mod_matrix_gw_s2s:limit(rtc_state_user, ?STATE_USER),
                                                        60000, 1) of
                                false ->
                                    reply(IQ, err({limit, <<"state">>}, Lang));
                                true ->
                                    Max = mod_matrix_gw_opt:rtc_max_delay_ms(Host),
                                    case maps:get(delay_ms, Send) of
                                        D when is_integer(D), D > Max ->
                                            reply(IQ, xmpp:err_not_acceptable(
                                                        <<"delay_ms over the gateway's maximum">>, Lang));
                                        _ ->
                                            mod_matrix_gw_room:rtc_route(
                                              IQ, {send, Send#{us => US, resource => LRes,
                                                               mxid => MXID, txn_key => TxnKey}})
                                    end
                            end
                    end
            end
    end.

%% The <send/>'s attributes and content: {ok, Send} or {error, Why}.
-spec parse_send(xmlel()) -> {ok, map()} | {error, atom()}.
parse_send(El) ->
    Type = fxml:get_tag_attr_s(<<"type">>, El),
    StateKey = case fxml:get_tag_attr(<<"state_key">>, El) of
                   {value, K} -> K;
                   false -> undefined
               end,
    CData = fxml:get_tag_cdata(El),
    case {int_attr(<<"sticky_ms">>, El), int_attr(<<"delay_ms">>, El)} of
        {bad, _} ->
            {error, bad_request};
        {_, bad} ->
            {error, bad_request};
        {_, _} when byte_size(CData) > ?MAX_CONTENT ->
            {error, too_large};
        {Sticky, Delay} ->
            try misc:json_decode(CData) of
                #{} = Content ->
                    {ok, #{type => Type, state_key => StateKey, sticky_ms => Sticky,
                           delay_ms => Delay, txn => fxml:get_tag_attr_s(<<"txn">>, El),
                           content => Content}};
                _ ->
                    {error, bad_request}
            catch _:_ ->
                    {error, bad_request}
            end
    end.

int_attr(Name, El) ->
    case fxml:get_tag_attr_s(Name, El) of
        <<>> ->
            undefined;
        B ->
            try binary_to_integer(B) of
                I when I >= 0 -> I;
                _ -> bad
            catch _:_ -> bad
            end
    end.

%% What a device may send as MXID from its device Dev (none: no device
%% bound to its resource): ok or {error, Why}.
-spec check_send(map(), binary(), binary() | none) -> ok | {error, term()}.
check_send(#{type := Type, state_key := Key, sticky_ms := Sticky} = Send, MXID, Dev) ->
    case kind(Type) of
        none ->
            {error, bad_type};
        member_a when not is_binary(Key); Sticky /= undefined ->
            {error, bad_type};
        member_a when Dev == none ->
            {error, state_key};
        member_a ->
            Own = <<"_", MXID/binary, "_", Dev/binary>>,
            case Key == Own orelse prefix(<<Own/binary, "_">>, Key) of
                true -> ok;
                false -> {error, state_key}
            end;
        slot when not is_binary(Key); Sticky /= undefined ->
            {error, bad_type};
        slot ->
            case binary:split(Key, <<"#">>) of
                [App, _] when App /= <<>> -> ok;
                _ -> {error, state_key}
            end;
        member_b when Key /= undefined; Sticky == undefined ->
            {error, bad_type};
        member_b ->
            Content = maps:get(content, Send),
            case owner_ok(Type, undefined, MXID, Content) andalso
                Dev /= none andalso
                maps:get(<<"device_id">>, maps:get(<<"member">>, Content), undefined) == Dev of
                true ->
                    %% 1.3.x: the sticky PDU (MSC4354's carry and push)
                    {error, sticky};
                false ->
                    {error, state_key}
            end;
        _ when Key /= undefined; Sticky /= undefined ->
            {error, bad_type};
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% Device -> gateway: the iqs to G
%%--------------------------------------------------------------------

-spec route_iq(iq()) -> ok.
route_iq(#iq{type = Type, lang = Lang, sub_els = [#xmlel{name = Name} = El]} = IQ) ->
    Host = ejabberd_config:get_myname(),
    case gate(Host, IQ) of
        {error, Err} ->
            reply(IQ, Err);
        {ok, US, LRes} ->
            case {Name, Type} of
                {<<"delayed">>, set} -> delayed_action(Host, IQ, US, El);
                {<<"delayed">>, get} -> delayed_list(IQ, US);
                {<<"transports">>, get} -> reply(IQ, transports_el(Host));
                {<<"openid">>, get} -> openid(Host, IQ, US, LRes);
                {<<"livekit-token">>, get} ->
                    %% 1.3.x: MSC4195's get_token over federation
                    reply(IQ, xmpp:err_feature_not_implemented(
                                <<"MSC4195 tokens are not served by this gateway yet">>, Lang));
                _ ->
                    reply(IQ, xmpp:err_bad_request(<<"wrong iq type">>, Lang))
            end
    end,
    ok.

%% A local user's resource whose caps carry the feature: {ok, US, LRes}.
gate(Host, #iq{from = From, lang = Lang}) ->
    case From of
        #jid{luser = U, lserver = Host, lresource = R} when U /= <<>>, R /= <<>> ->
            case has_feature(Host, {U, Host, R}) of
                true ->
                    {ok, {U, Host}, R};
                false ->
                    {error, xmpp:err_feature_not_implemented(
                              <<"This resource does not advertise https://ns.another.im/matrix-rtc/0">>, Lang)}
            end;
        _ ->
            {error, xmpp:err_not_allowed(<<"Only for the devices of this server's users">>, Lang)}
    end.

transports_el(Host) ->
    Format = mod_matrix_gw_opt:rtc_member_format(Host),
    #xmlel{name = <<"transports">>,
           attrs = [{<<"xmlns">>, ?NS}, {<<"format">>, atom_to_binary(Format)}],
           children = [transport_el(T) || T <- mod_matrix_gw_opt:rtc_transports(Host)]}.

transport_el(T) ->
    Pairs = [{bin(K), bin(V)} || {K, V} <- T],
    {Type, Rest} = case lists:keytake(<<"type">>, 1, Pairs) of
                       {value, P, R} -> {[P], R};
                       false -> {[], Pairs}
                   end,
    #xmlel{name = <<"transport">>, attrs = Type ++ lists:sort(Rest)}.

bin(A) when is_atom(A) -> atom_to_binary(A);
bin(B) when is_binary(B) -> B;
bin(I) when is_integer(I) -> integer_to_binary(I);
bin(L) when is_list(L) -> unicode:characters_to_binary(L).

openid(Host, #iq{lang = Lang} = IQ, {U, _} = US, LRes) ->
    case mod_matrix_gw_s2s:rate({rtc_openid_user, US},
                                mod_matrix_gw_s2s:limit(rtc_openid_user, ?OPENID_USER), 60000, 1) of
        false ->
            reply(IQ, err({limit, <<"openid">>}, Lang));
        true ->
            Token = b64url(crypto:strong_rand_bytes(32)),
            MXID = mxid(Host, U),
            Domain = mod_matrix_gw_opt:matrix_domain(Host),
            mnesia:dirty_write(#matrix_openid{token = Token, mxid = MXID,
                                              expires_ts = now_ms() + ?OPENID_TTL * 1000}),
            audit(Host, openid, MXID, <<>>, #{resource => LRes}),
            reply(IQ, #xmlel{name = <<"openid">>,
                             attrs = [{<<"xmlns">>, ?NS}, {<<"access_token">>, Token},
                                      {<<"token_type">>, <<"Bearer">>},
                                      {<<"matrix_server_name">>, Domain},
                                      {<<"expires_in">>, integer_to_binary(?OPENID_TTL)}]})
    end.

%% GET /_matrix/federation/v1/openid/userinfo?access_token=T (or the
%% token as a Bearer): unauthenticated, as the spec has it; within
%% rtc_userinfo_ip a minute per address.
-spec userinfo(#request{}) -> {integer(), list(), binary()} | tuple().
userinfo(#request{method = 'GET', q = Q, headers = Headers, ip = IPPort}) ->
    Host = ejabberd_config:get_myname(),
    IP = element(1, IPPort),
    Token = case proplists:get_value(<<"access_token">>, Q) of
                T when is_binary(T), T /= <<>> ->
                    T;
                _ ->
                    case proplists:get_value('Authorization', Headers) of
                        <<"Bearer ", B/binary>> -> B;
                        _ -> <<>>
                    end
            end,
    case mod_matrix_gw_s2s:rate({rtc_userinfo_ip, IP},
                                mod_matrix_gw_s2s:limit(rtc_userinfo_ip, ?USERINFO_IP), 60000, 1) of
        false ->
            {429, <<"Too Many Requests">>, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
             misc:json_encode(#{<<"errcode">> => <<"M_LIMIT_EXCEEDED">>,
                                <<"error">> => <<"Too many requests">>})};
        true ->
            Now = now_ms(),
            case catch mnesia:dirty_read(matrix_openid, Token) of
                [#matrix_openid{mxid = MXID, expires_ts = E}] when E > Now ->
                    audit(Host, userinfo, MXID, <<>>, #{}),
                    mod_matrix_gw_http:json(200, #{<<"sub">> => MXID});
                _ ->
                    audit(Host, userinfo_refused, <<>>, <<>>, #{}),
                    mod_matrix_gw_http:error(401, <<"M_UNKNOWN_TOKEN">>, <<"Unknown or expired token">>)
            end
    end;
userinfo(_) ->
    mod_matrix_gw_http:error(405, <<"M_UNRECOGNIZED">>, <<"Unrecognized request">>).

%%--------------------------------------------------------------------
%% The room's answers (mod_matrix_gw_room:rtc_iq/3 calls these)
%%--------------------------------------------------------------------

%% <sent/> for an event added, the txn remembered.
-spec send_result(iq(), map(), binary()) -> iq().
send_result(IQ, Send, EventID) ->
    El = #xmlel{name = <<"sent">>, attrs = [{<<"xmlns">>, ?NS}, {<<"event">>, EventID}]},
    Host = ejabberd_config:get_myname(),
    txn_store(Host, maps:get(txn_key, Send, none), El),
    audit(Host, sent, maps:get(mxid, Send, <<>>), maps:get(room, Send, <<>>),
          #{type => maps:get(type, Send), id => EventID}),
    xmpp:make_iq_result(IQ, El).

-spec send_error(iq(), atom() | tuple(), binary()) -> iq().
send_error(IQ, Why, Lang) ->
    xmpp:make_error(IQ, err(Why, Lang)).

%% A <send delay_ms/> into the room Send's room: the row written and
%% armed, <sent delay_id/>; or the user's rows are full.
-spec schedule(iq(), map()) -> iq().
schedule(#iq{lang = Lang} = IQ, #{us := US, resource := LRes, room := Room, type := Type,
                                  state_key := Key, content := Content, delay_ms := Delay} = Send) ->
    Host = ejabberd_config:get_myname(),
    Max = mod_matrix_gw_opt:rtc_max_delayed_per_user(Host),
    case length(rows_of(US)) >= Max of
        true ->
            xmpp:make_error(IQ, err({limit, <<"delayed">>}, Lang));
        false ->
            Now = now_ms(),
            ID = <<"d-", (b64url(crypto:strong_rand_bytes(12)))/binary>>,
            Row = #matrix_delayed{id = ID, us = US, resource = LRes, room = Room, type = Type,
                                  state_key = Key, content = Content, delay_ms = Delay,
                                  due_ts = Now + Delay, created_ts = Now},
            mnesia:dirty_write(Row),
            gen_server:cast(procname(Host), {arm, ID}),
            El = #xmlel{name = <<"sent">>, attrs = [{<<"xmlns">>, ?NS}, {<<"delay_id">>, ID}]},
            txn_store(Host, maps:get(txn_key, Send, none), El),
            audit(Host, delayed, maps:get(mxid, Send, <<>>), Room, #{type => Type, id => ID}),
            xmpp:make_iq_result(IQ, El)
    end.

%% <room-state/>'s answer: the <state/> of each event's Fields.
-spec room_state_result(iq(), [fields()]) -> iq().
room_state_result(IQ, Fields) ->
    xmpp:make_iq_result(IQ, #xmlel{name = <<"room-state">>, attrs = [{<<"xmlns">>, ?NS}],
                                   children = [state_el(F) || F <- Fields]}).

%%--------------------------------------------------------------------
%% Delayed events
%%--------------------------------------------------------------------

rows_of(US) ->
    mnesia:dirty_index_read(matrix_delayed, US, #matrix_delayed.us).

delayed_list(#iq{lang = Lang} = IQ, US) ->
    case delayed_rate(US) of
        false ->
            reply(IQ, err({limit, <<"delayed">>}, Lang));
        true ->
            Now = now_ms(),
            Items = [#xmlel{name = <<"item">>,
                            attrs = [{<<"id">>, ID}, {<<"room">>, Room}, {<<"type">>, Type}] ++
                                [{<<"state_key">>, K} || is_binary(K)] ++
                                [{<<"due_ms">>, integer_to_binary(max(0, Due - Now))}]}
                     || #matrix_delayed{id = ID, room = Room, type = Type, state_key = K, due_ts = Due}
                            <- lists:keysort(#matrix_delayed.due_ts, rows_of(US))],
            reply(IQ, #xmlel{name = <<"delayed">>, attrs = [{<<"xmlns">>, ?NS}], children = Items})
    end.

delayed_action(Host, #iq{lang = Lang} = IQ, US, El) ->
    ID = fxml:get_tag_attr_s(<<"id">>, El),
    Action = fxml:get_tag_attr_s(<<"action">>, El),
    case delayed_rate(US) of
        false ->
            reply(IQ, err({limit, <<"delayed">>}, Lang));
        true ->
            case {catch mnesia:dirty_read(matrix_delayed, ID), Action} of
                {_, A} when A /= <<"restart">>, A /= <<"send">>, A /= <<"cancel">> ->
                    reply(IQ, xmpp:err_bad_request(<<"action is restart, send or cancel">>, Lang));
                {[#matrix_delayed{us = US} = Row], <<"restart">>} ->
                    mnesia:dirty_write(Row#matrix_delayed{due_ts = now_ms() + Row#matrix_delayed.delay_ms}),
                    gen_server:cast(procname(Host), {arm, ID}),
                    reply(IQ, xmpp:make_iq_result(IQ));
                {[#matrix_delayed{us = US} = Row], <<"send">>} ->
                    mnesia:dirty_delete(matrix_delayed, ID),
                    gen_server:cast(procname(Host), {disarm, ID}),
                    fire(Host, Row, sent),
                    reply(IQ, xmpp:make_iq_result(IQ));
                {[#matrix_delayed{us = US}], <<"cancel">>} ->
                    mnesia:dirty_delete(matrix_delayed, ID),
                    gen_server:cast(procname(Host), {disarm, ID}),
                    audit(Host, delayed_cancelled, element(1, US), <<>>, #{id => ID}),
                    reply(IQ, xmpp:make_iq_result(IQ));
                _ ->
                    reply(IQ, xmpp:err_item_not_found(<<"no such delayed event">>, Lang))
            end
    end.

delayed_rate(US) ->
    mod_matrix_gw_s2s:rate({rtc_delayed_user, US},
                           mod_matrix_gw_s2s:limit(rtc_delayed_user, ?DELAYED_USER), 60000, 1).

%% A row's event into its room, as its user, with the room's auth of now.
fire(Host, #matrix_delayed{id = ID, us = {U, _} = US, room = Room, type = Type,
                           state_key = Key, content = Content}, Why) ->
    JSON0 = #{<<"type">> => Type, <<"sender">> => mxid(Host, U), <<"content">> => Content},
    JSON = case Key of
               undefined -> JSON0;
               _ -> JSON0#{<<"state_key">> => Key}
           end,
    audit(Host, fired, mxid(Host, U), Room, #{id => ID, type => Type, why => Why}),
    case mod_matrix_gw_room:rtc_fire(Host, Room, US, JSON) of
        ok ->
            ok;
        error ->
            ?INFO_MSG("Matrix gateway: delayed event ~ts of ~ts: the room ~ts is not here, dropped",
                      [ID, mxid(Host, U), Room]),
            audit(Host, fire_failed, mxid(Host, U), Room, #{id => ID})
    end.

%% A local user's leave (or ban) of a room: the user's rows for it go.
-spec membership(binary(), binary(), binary(), binary()) -> ok.
membership(Host, Room, MXID, Membership) when Membership == <<"leave">>; Membership == <<"ban">> ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    case mod_matrix_gw_room:parse_user_id(MXID) of
        {ok, U, Domain} ->
            case jid:nodeprep(U) of
                LU when is_binary(LU), LU /= <<>> ->
                    [drop_row(Host, ID) || #matrix_delayed{id = ID, room = R} <- rows_of({LU, Host}),
                                           R == Room],
                    ok;
                _ ->
                    ok
            end;
        _ ->
            ok
    end;
membership(_Host, _Room, _MXID, _Membership) ->
    ok.

drop_row(Host, ID) ->
    mnesia:dirty_delete(matrix_delayed, ID),
    catch gen_server:cast(procname(Host), {disarm, ID}).

%% An account deleted: its rows and tokens go.
-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    [drop_row(LServer, ID) || #matrix_delayed{id = ID} <- catch rows_of({LUser, LServer})],
    MXID = catch mxid(LServer, LUser),
    [mnesia:dirty_delete(matrix_openid, T)
     || T <- case catch mnesia:dirty_select(matrix_openid,
                                              [{#matrix_openid{mxid = MXID, token = '$1', _ = '_'},
                                                [], ['$1']}]) of
                 L when is_list(L) -> L;
                 _ -> []
             end],
    ok.

%%--------------------------------------------------------------------
%% txn, errors, helpers
%%--------------------------------------------------------------------

txn_key(_US, _LRes, <<>>) -> none;
txn_key(US, LRes, Txn) -> {txn, US, LRes, Txn}.

txn_lookup(_Host, none) ->
    none;
txn_lookup(Host, Key) ->
    Now = now_ms(),
    case catch ets:lookup(mem(Host), Key) of
        [{_, El, T}] when Now - T < ?TXN_TTL -> {ok, El};
        _ -> none
    end.

txn_store(_Host, none, _El) ->
    ok;
txn_store(Host, Key, El) ->
    catch ets:insert(mem(Host), {Key, El, now_ms()}),
    ok.

reply(IQ, #stanza_error{} = Err) ->
    ejabberd_router:route(xmpp:make_error(IQ, Err));
reply(IQ, #iq{} = Res) ->
    _ = IQ,
    ejabberd_router:route(Res);
reply(IQ, #xmlel{} = El) ->
    ejabberd_router:route(xmpp:make_iq_result(IQ, El)).

err(bad_type, Lang) ->
    app(xmpp:err_not_acceptable(<<"Not an RTC event type, or not in this form">>, Lang), <<"bad-type">>);
err(state_key, Lang) ->
    app(xmpp:err_not_acceptable(<<"The state key (or member) is not this device's">>, Lang), <<"state-key">>);
err(too_large, Lang) ->
    app(xmpp:err_not_acceptable(<<"Over the 16 KiB an RTC event's content may have">>, Lang), <<"too-large">>);
err(power_level, Lang) ->
    app(xmpp:err_forbidden(<<"The room's power levels refuse it">>, Lang), <<"power-level">>);
err(sticky, Lang) ->
    xmpp:err_feature_not_implemented(<<"Sticky events are not sent by this gateway yet">>, Lang);
err({limit, Name}, Lang) ->
    Err = xmpp:err_resource_constraint(<<"Over the gateway's limit">>, Lang),
    Err#stanza_error{sub_els = [#xmlel{name = <<"limit">>,
                                       attrs = [{<<"xmlns">>, ?NS}, {<<"name">>, Name}]}]};
err(_, Lang) ->
    xmpp:err_bad_request(<<"Malformed request">>, Lang).

app(Err, Name) ->
    Err#stanza_error{sub_els = [#xmlel{name = Name, attrs = [{<<"xmlns">>, ?NS}]}]}.

mxid(Host, LUser) ->
    <<"@", LUser/binary, ":", (mod_matrix_gw_opt:matrix_domain(Host))/binary>>.

%% The Matrix device bound to the resource, or none.
device_of(Host, LUser, LRes) ->
    case [D || {D, R} <- devices_of(Host, LUser), jid:resourceprep(R) == LRes] of
        [Dev | _] -> Dev;
        [] -> none
    end.

-ifdef(TEST).
devices_of(Host, LUser) ->
    case persistent_term:get({?MODULE, devices}, undefined) of
        undefined -> mod_matrix_gw_e2e:devices_of(Host, LUser);
        Fun -> Fun(Host, LUser)
    end.
-else.
devices_of(Host, LUser) ->
    mod_matrix_gw_e2e:devices_of(Host, LUser).
-endif.

now_ms() ->
    erlang:system_time(millisecond).

b64url(Bin) ->
    base64:encode(Bin, #{mode => urlsafe, padding => false}).

%%--------------------------------------------------------------------
%% Audit
%%--------------------------------------------------------------------

%% Kind happened, Who (a user id, a server) in Room: counted, logged at
%% info (forgeries and refusals too, once a minute per kind and who).
-spec audit(binary(), atom(), binary(), binary(), map()) -> ok.
audit(Host, Kind, Who, Room, Meta) ->
    catch ets:update_counter(mem(Host), {count, Kind}, 1, {{count, Kind}, 0}),
    Log = case Kind of
              K when K == forged; K == origin_over; K == userinfo_refused ->
                  mod_matrix_gw_s2s:rate({rtc_audit_logged, Kind, Who}, 1, 60000, 1);
              forwarded ->
                  false;
              _ ->
                  true
          end,
    case Log of
        true ->
            ?INFO_MSG("Matrix gateway RTC ~ts: ~ts in ~ts ~0p", [Kind, Who, Room, Meta]);
        false ->
            ?DEBUG("Matrix gateway RTC ~ts: ~ts in ~ts ~0p", [Kind, Who, Room, Meta])
    end,
    ok.

%%--------------------------------------------------------------------
%% Tokens out of the log
%%--------------------------------------------------------------------

%% ejabberd at debug level prints every stanza (ejabberd_router's Route,
%% the stream's Send XML) and every HTTP query (ejabberd_http), so the
%% <openid/> result and the userinfo URL would put a live OpenID token in
%% the log file. A primary logger filter masks an access_token attribute,
%% an access_token= query or JSON field and a Bearer header in every log
%% event's arguments; an event it cannot walk passes unchanged.
-define(LOG_FILTER, mod_matrix_gw_rtc_redact).
-define(REDACTED, <<"[redacted]">>).
-define(REDACT_DEPTH, 40).

install_log_filter() ->
    _ = logger:add_primary_filter(?LOG_FILTER, {fun ?MODULE:redact_log/2, []}),
    ok.

-spec redact_log(logger:log_event(), term()) -> logger:filter_return().
redact_log(#{msg := Msg} = Event, _) ->
    try Event#{msg := redact_msg(Msg)}
    catch _:_ -> Event
    end;
redact_log(Event, _) ->
    Event.

redact_msg({report, R}) -> {report, redact(R, 0)};
redact_msg({string, S}) -> {string, redact(S, 0)};
redact_msg({Format, Args}) when is_list(Args) -> {Format, [redact(A, 0) || A <- Args]};
redact_msg(Msg) -> Msg.

redact(X, D) when D > ?REDACT_DEPTH -> X;
redact({<<"access_token">>, V}, _) when is_binary(V) -> {<<"access_token">>, ?REDACTED};
redact({'Authorization', <<"Bearer ", _/binary>>}, _) -> {'Authorization', <<"Bearer ", ?REDACTED/binary>>};
redact(B, _) when is_binary(B) -> redact_bin(B);
redact(L, D) when is_list(L) -> redact_list(L, D + 1);
redact(T, D) when is_tuple(T) -> list_to_tuple([redact(E, D + 1) || E <- tuple_to_list(T)]);
redact(M, D) when is_map(M) -> maps:map(fun(_, V) -> redact(V, D + 1) end, M);
redact(X, _) -> X.

%% Improper lists (iolists) too.
redact_list([H | T], D) -> [redact(H, D) | redact_list(T, D)];
redact_list([], _) -> [];
redact_list(Tail, D) -> redact(Tail, D).

redact_bin(B) ->
    case binary:match(B, [<<"access_token">>, <<"Bearer ">>]) of
        nomatch ->
            B;
        _ ->
            B1 = re:replace(B, <<"(access_token(?:=|='|=\"|\":\\s*\"))[^'\"&\\s>]+">>,
                            <<"\\1[redacted]">>, [global, {return, binary}]),
            re:replace(B1, <<"(Bearer )[A-Za-z0-9._~+/=-]+">>, <<"\\1[redacted]">>,
                       [global, {return, binary}])
    end.

%% The counters, [{Kind, N}].
-spec status(binary()) -> [{atom(), non_neg_integer()}].
status(Host) ->
    lists:sort([{K, N} || {{count, K}, N} <- (catch ets:tab2list(mem(Host))),
                          is_atom(K), is_integer(N)]).

-spec status_command(binary()) -> [{binary(), integer()}].
status_command(Host) ->
    [{atom_to_binary(K), N} || {K, N} <- status(jid:nameprep(Host))].

-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = matrix_gw_rtc_status, tags = [matrix],
                        desc = "The Matrix gateway's MatrixRTC counters",
                        module = ?MODULE, function = status_command,
                        args = [{host, binary}],
                        args_desc = ["The XMPP host"],
                        args_example = [<<"example.com">>],
                        result = {counters, {list, {counter, {tuple, [{kind, string},
                                                                      {count, integer}]}}}}}].

%%--------------------------------------------------------------------
%% The process: the delayed events' timers, the memory
%%--------------------------------------------------------------------

procname(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

mem(Host) ->
    gen_mod:get_module_proc(Host, matrix_rtc_mem).

start_link(Host) ->
    gen_server:start_link({local, procname(Host)}, ?MODULE, [Host], []).

-ifdef(TEST).
timers(Host) ->
    gen_server:call(procname(Host), timers).
-endif.

init([Host]) ->
    install_log_filter(),
    case ets:info(mem(Host)) of
        undefined -> ets:new(mem(Host), [named_table, public, set]);
        _ -> ok
    end,
    erlang:send_after(?PURGE_EVERY, self(), purge),
    erlang:send_after(?OPENID_PURGE_EVERY, self(), purge_openid),
    %% the rows on disc armed again (a restart keeps them)
    IDs = [ID || #matrix_delayed{id = ID, us = {_, S}} <- ets:tab2list(matrix_delayed), S == Host],
    State = lists:foldl(fun(ID, St) -> arm(ID, St) end, #{host => Host, timers => #{}}, IDs),
    {ok, State}.

handle_call(timers, _From, #{timers := T} = State) ->
    {reply, T, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({arm, ID}, State) ->
    {noreply, arm(ID, State)};
handle_cast({disarm, ID}, State) ->
    {noreply, disarm(ID, State)};
handle_cast({offline, US, LRes}, #{host := Host} = State) ->
    Now = now_ms(),
    Rows = [R || #matrix_delayed{resource = Res} = R <- rows_of(US), Res == LRes],
    State2 = lists:foldl(
               fun(#matrix_delayed{id = ID, due_ts = Due, delay_ms = D} = R, St) when Due > Now + D ->
                       mnesia:dirty_write(R#matrix_delayed{due_ts = Now + D}),
                       arm(ID, St);
                  (_, St) ->
                       St
               end, State, Rows),
    [audit(Host, offline_switch, element(1, US), <<>>, #{rows => length(Rows)}) || Rows /= []],
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({fire, ID}, #{host := Host, timers := T} = State) ->
    State2 = State#{timers := maps:remove(ID, T)},
    Now = now_ms(),
    case catch mnesia:dirty_read(matrix_delayed, ID) of
        [#matrix_delayed{due_ts = Due} = Row] when Due =< Now + 500 ->
            mnesia:dirty_delete(matrix_delayed, ID),
            fire(Host, Row, due),
            {noreply, State2};
        [#matrix_delayed{}] ->
            {noreply, arm(ID, State2)};
        _ ->
            {noreply, State2}
    end;
handle_info(purge, #{host := Host} = State) ->
    Now = now_ms(),
    catch ets:select_delete(mem(Host), [{{{txn, '_', '_', '_'}, '_', '$1'},
                                         [{'<', '$1', Now - ?TXN_TTL}], [true]}]),
    erlang:send_after(?PURGE_EVERY, self(), purge),
    {noreply, State};
handle_info(purge_openid, State) ->
    Now = now_ms(),
    [mnesia:dirty_delete(matrix_openid, T)
     || T <- mnesia:dirty_select(matrix_openid, [{#matrix_openid{token = '$1', expires_ts = '$2', _ = '_'},
                                                  [{'=<', '$2', Now}], ['$1']}])],
    erlang:send_after(?OPENID_PURGE_EVERY, self(), purge_openid),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

arm(ID, State) ->
    State1 = disarm(ID, State),
    case catch mnesia:dirty_read(matrix_delayed, ID) of
        [#matrix_delayed{due_ts = Due}] ->
            Ref = erlang:send_after(max(0, Due - now_ms()), self(), {fire, ID}),
            State1#{timers := (maps:get(timers, State1))#{ID => Ref}};
        _ ->
            State1
    end.

disarm(ID, #{timers := T} = State) ->
    case maps:take(ID, T) of
        {Ref, T2} ->
            erlang:cancel_timer(Ref),
            State#{timers := T2};
        error ->
            State
    end.
