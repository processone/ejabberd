%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_room.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Matrix rooms
%%% Created :  1 May 2022 by Alexey Shchepin <alexey@process-one.net>
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
-module(mod_matrix_gw_room).

-behaviour(gen_statem).

%% API
-export([start_link/2, supervisor/1, create_db/0,
         get_room_pid/2, join_direct/5, process_pdu/3,
         get_missing_events/7, get_state_ids/4,
         get_rooms_list/0, get_event/3, get_event/4,
         make_join/4, send_join/5,
         create_new_room/3, room_add_event/3,
         binary_to_room_version/1,
         parse_user_id/1,
         send_muc_invite/7,
         escape/1, unescape/1,
         route/1]).
-ifdef(TEST).
-export([process_missing_events/6, json_to_event/2, history_sender/2]).
-endif.
-export([process_edus/3, get_state/4, make_leave/3,
         get_known_room_version/1, restore_rooms/1]).
%% the end-to-end room events (mod_matrix_gw_event)
-export([members/1]).
%% what a failure log line may carry
-export([failure/3]).
%% /query/directory
-export([directory/2]).
%% MatrixRTC (mod_matrix_gw_rtc)
-export([rtc_route/2, rtc_fire/4]).

%% gen_statem callbacks
-export([init/1, terminate/3, code_change/4, callback_mode/0]).
-export([handle_event/4]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("mod_matrix_gw.hrl").

-record(matrix_room,
        {room_id  :: binary(),
         pid :: pid()}).

-record(matrix_direct,
        {local_remote,
         room_id :: binary()}).

-record(event,
        {id :: binary(),
         room_version :: #room_version{},
         room_id :: binary(),
         type :: binary(),
         state_key :: binary() | undefined,
         depth :: integer(),
         auth_events :: [binary()],
         sender :: binary(),
         prev_events :: [binary()],
         origin_server_ts :: integer(),
         json :: #{atom() | binary() => misc:json_value()},
         state_map}).

-record(direct,
        {local_user :: jid() | undefined,
         remote_user :: binary() | undefined,
         client_state}).

-record(multi_user,
        {join_ts :: integer() | undefined,
         room_jid :: jid() | undefined,
         %% the MUC history the resource asked for when joining
         %% (<history maxstanzas/>), until it is sent with the subject
         history = 20 :: non_neg_integer() | sent,
         %% <history since/seconds/> as a time in ms, and
         %% whether the join created the room (status 201)
         since = 0 :: integer(),
         created = false :: boolean(),
         %% the nick the join asked for (the
         %% room's is the user id's, status 210 when they differ)
         nick = <<>> :: binary()}).

-record(multi,
        {users :: #{{binary(), binary()} =>
                        ({online, #{binary() => #multi_user{}}} |
                         {offline, reference()})}}).

-record(data,
        {host :: binary(),
         kind :: #direct{} | #multi{} | undefined,
         room_id :: binary(),
         room_jid :: jid(),
         room_version :: #room_version{},
         via :: binary | undefined,
         events = #{},
         latest_events = sets:new([{version, 2}]),
         nonlatest_events = sets:new([{version, 2}]),
         event_queue = treap:empty(),
         outgoing_txns = #{},
         %% XMPP message ids of the events sent from XMPP
         %% #{XmppID => {Counter, EventID}}, the reactions kept
         %% (mod_matrix_gw_xmpp:add_reaction/4), the room's last messages
         %% as MUC history [{TS, EventID, Sender, Body}], newest first, and
         %% what mod_matrix_gw_store last saved of the room
         xmpp_ids = #{},
         counter = 0,
         reactions = #{},
         backlog = [],
         typing = #{},
         stored_meta,
         %% {Sender, XmppID} of the <event/> being added, whose
         %% XMPP id is not in its (encrypted) content
         e2e_sending,
         %% the room's avatar as its vCard has it,
         %% {MXC, SHA1, Mime, Bytes}, or {fetching, MXC}
         avatar,
         %% the receipts asked
         %% of messages sent from XMPP, #{receipts => #{XmppID => {Counter,
         %% {From, Kinds}}}}
         content = #{}}).

-define(ROOM_CREATE, <<"m.room.create">>).
%% the spec's size limits of a PDU (canonical JSON) and of its ids
-define(MAX_PDU, 65536).
-define(MAX_PDU_ID, 255).
%% the spec's bounds of a PDU's prev_events and
%% auth_events (a local event names at most so many prev events too)
-define(MAX_PREV_EVENTS, 20).
-define(MAX_AUTH_EVENTS, 10).
-define(ROOM_MEMBER, <<"m.room.member">>).
-define(ROOM_JOIN_RULES, <<"m.room.join_rules">>).
-define(ROOM_POWER_LEVELS, <<"m.room.power_levels">>).
-define(ROOM_3PI, <<"m.room.third_party_invite">>).
-define(ROOM_MESSAGE, <<"m.room.message">>).
-define(ROOM_HISTORY_VISIBILITY, <<"m.room.history_visibility">>).
-define(ROOM_TOPIC, <<"m.room.topic">>).
-define(ROOM_ALIASES, <<"m.room.aliases">>).
-define(ROOM_NAME, <<"m.room.name">>).
-define(ROOM_AVATAR, <<"m.room.avatar">>).
-define(ROOM_CANONICAL_ALIAS, <<"m.room.canonical_alias">>).

-define(CREATOR_PL, (1 bsl 53)).

-define(MAX_DEPTH, 16#7FFFFFFFFFFFFFFF).
-define(MAX_TXN_RETRIES, 5).

-define(MATRIX_ROOM_ALIAS_CACHE, matrix_room_alias_cache).
-define(MATRIX_ROOM_ALIAS_CACHE_ERROR_TIMEOUT, 60000).

%% how many XMPP ids and messages a room remembers, the history a
%% joining resource gets when it asks for none, and how long a local member
%% of a room brought back after a restart has to come back before the
%% gateway leaves the room for her (at least the leave_timeout option)
-define(MAX_XMPP_IDS, 1000).
-define(MAX_BACKLOG, 100).
-define(BACKFILL_PAGES, 4).
-define(DEFAULT_HISTORY, 20).
-define(RESTORE_GRACE, 600).
-define(ROOM_REDACTION, <<"m.room.redaction">>).
-define(REACTION, <<"m.reaction">>).
-define(ROOM_ENCRYPTION, <<"m.room.encryption">>).
%% event types besides m.room.message and m.reaction
%% that reach the XMPP side (mod_matrix_gw_content:is_content_type/1)
-define(IS_CONTENT(T), (T == <<"m.sticker">> orelse
                        T == <<"org.matrix.msc3381.poll.start">> orelse
                        T == <<"org.matrix.msc3381.poll.response">> orelse
                        T == <<"org.matrix.msc3381.poll.end">> orelse
                        T == <<"m.poll.start">> orelse T == <<"m.poll.response">> orelse
                        T == <<"m.poll.end">>)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary(), binary()) ->
                        {ok, Pid :: pid()} |
                        ignore |
                        {error, Error :: term()}.
start_link(Host, RoomID) ->
    gen_statem:start_link(?MODULE, [Host, RoomID],
                          ejabberd_config:fsm_limit_opts([])).

-spec supervisor(binary()) -> atom().
supervisor(Host) ->
    gen_mod:get_module_proc(Host, mod_matrix_gw_room_sup).

create_db() ->
    ejabberd_mnesia:create(
      ?MODULE, matrix_room,
      [{ram_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_room)}]),
    ejabberd_mnesia:create(
      ?MODULE, matrix_direct,
      [{ram_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_direct)}]),
    ets_cache:new(?MATRIX_ROOM_ALIAS_CACHE),
    ok.

get_room_pid(Host, RoomID) ->
    case get_existing_room_pid(Host, RoomID) of
        {error, not_found} ->
            try
                Proc = gen_mod:get_module_proc_check(Host, mod_matrix_gw_room_sup),
                supervisor:start_child(Proc, [Host, RoomID])
            of
                {ok, undefined} -> {error, ignored};
                Res -> Res
            catch
                error:badarg -> {error, wrong_host}
            end;
        {ok, Pid} ->
            {ok, Pid}
    end.

get_existing_room_pid(_Host, RoomID) ->
    case mnesia:dirty_read(matrix_room, RoomID) of
        [] ->
            {error, not_found};
        [#matrix_room{pid = Pid}] ->
            {ok, Pid}
    end.

join_direct(Host, MatrixServer, RoomID, Sender, UserID) ->
    case get_room_pid(Host, RoomID) of
        {ok, Pid} ->
            gen_statem:cast(Pid, {join_direct, MatrixServer, RoomID, Sender, UserID});
        {error, _} = Error ->
            Error
    end.

route(#presence{from = From, to = #jid{luser = <<C, _/binary>>} = To,
                type = Type} = Packet)
  when C == $!;
       C == $# ->
    Host = ejabberd_config:get_myname(),
    case room_id_from_xmpp(Host, To#jid.luser, From) of
        {ok, RoomID, Via} ->
            case From#jid.lserver of
                Host ->
                    case Type of
                        available ->
                            case get_room_pid(Host, RoomID) of
                                {ok, Pid} ->
                                    gen_statem:cast(Pid, {join, From, Packet, Via});
                                {error, _} = Error ->
                                    ?DEBUG("join failed ~p", [{From, To, Error}]),
                                    ok
                            end;
                        unavailable ->
                            case get_existing_room_pid(Host, RoomID) of
                                {ok, Pid} ->
                                    gen_statem:cast(Pid, {leave, From});
                                _ ->
                                    ok
                            end;
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        error ->
            %% a local user joining #name%<our domain>
            %% that does not exist creates it
            MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
            case {Type, From#jid.lserver, mod_matrix_gw_muc:local_alias(To#jid.luser, MatrixServer)} of
                {available, Host, {ok, Alias}} ->
                    create_room(Host, From, Packet, Alias);
                _ ->
                    Lang = xmpp:get_lang(Packet),
                    Txt = <<"bad or non-existing room id">>,
                    Err = xmpp:err_not_acceptable(Txt, Lang),
                    ejabberd_router:route_error(Packet, Err),
                    ok
            end
    end;
%% a message carrying a https://ns.another.im/matrix-e2e/0 <event/> is the
%% room event it names (mod_matrix_gw_event); any other comes back once,
%% marked, for the clauses below
route(#message{meta = Meta} = Pkt)
  when not is_map_key(matrix_e2e_event, Meta) ->
    Host = ejabberd_config:get_myname(),
    case mod_matrix_gw_event:parse_message(Pkt, Host) of
        pass -> route(Pkt#message{meta = Meta#{matrix_e2e_event => pass}});
        ignore -> ok;
        {error, Err} -> ejabberd_router:route_error(Pkt, Err), ok;
        {event, Type, Content} -> route_e2e_event(Pkt, Host, Type, Content)
    end;
%% a subject change from an occupant (a groupchat
%% message with a subject and no body) is the room's m.room.topic
route(#message{from = From, to = #jid{luser = <<C, _/binary>>} = To,
               type = groupchat, subject = [_ | _] = Subject, body = Body} = Pkt)
  when C == $!;
       C == $# ->
    case xmpp:get_text(Body) of
        <<>> ->
            with_room(To, Pkt, fun(Pid) ->
                                       gen_statem:cast(Pid, {subject, From, xmpp:get_text(Subject), Pkt})
                               end);
        _ ->
            route(Pkt#message{subject = []})
    end;
%% a groupchat message is whatever mod_matrix_gw_xmpp:classify/1
%% finds in it (a body, a correction, reactions, a chat state), not only a
%% body
route(#message{from = From, to = #jid{luser = <<C, _/binary>>} = To,
               type = groupchat,
               id = MsgID} = Pkt)
  when C == $!;
       C == $# ->
    Host = ejabberd_config:get_myname(),
    case mod_matrix_gw_xmpp:classify(Pkt) of
        [] ->
            ok;
        Actions ->
            case room_id_from_xmpp(Host, To#jid.luser, From) of
                {ok, RoomID, _Via} when From#jid.lserver == Host ->
                    case get_existing_room_pid(Host, RoomID) of
                        {ok, Pid} ->
                            gen_statem:cast(Pid, {xmpp_actions, From, MsgID, Actions});
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end
    end;
route(#message{type = error}) ->
    ok;
route(#message{from = From, to = #jid{luser = <<C, _/binary>>} = To} = Pkt)
  when C == $!;
       C == $# ->
    %% mediated invites (XEP-0045 §7.8.2); a private
    %% message to an occupant, which Matrix has not, is answered
    %% feature-not-implemented when it has a body
    case mod_matrix_gw_muc:invites(Pkt) of
        [] when To#jid.lresource /= <<>> ->
            case xmpp:get_text(Pkt#message.body) of
                <<>> -> ok;
                _ -> ejabberd_router:route_error(
                       Pkt, xmpp:err_feature_not_implemented(
                              <<"Private messages are not supported in Matrix rooms">>,
                              xmpp:get_lang(Pkt)))
            end;
        [] -> ok;
        Invites -> with_room(To, Pkt, fun(Pid) -> gen_statem:cast(Pid, {invite, From, Invites, Pkt}) end)
    end;
%% a direct invite (XEP-0249) of a Matrix user to a
%% Matrix room is the room's invite; any other message comes back once,
%% marked, for the clause below
route(#message{from = From, to = To, meta = Meta} = Pkt)
  when not is_map_key(matrix_direct_invite, Meta) ->
    case mod_matrix_gw_muc:direct_invite(Pkt) of
        {#jid{luser = <<C, _/binary>>} = RoomJID, Reason} when C == $!; C == $# ->
            with_room(RoomJID, Pkt,
                      fun(Pid) -> gen_statem:cast(Pid, {invite, From, [{To, Reason}], Pkt}) end);
        _ ->
            route(Pkt#message{meta = Meta#{matrix_direct_invite => pass}})
    end;
route(#message{from = From, to = To, id = MsgID, lang = Lang} = Pkt) ->
    Host = ejabberd_config:get_myname(),
    case user_id_from_jid(To, Host) of
        _ when From#jid.lserver /= Host; From#jid.luser == <<>> ->
            %% a direct chat is a local user's; anyone
            %% else writing to a Matrix user through the gateway would speak
            %% (and make rooms, and take over the direct chat's row) as the
            %% local user of the same name
            ejabberd_router:route_error(
              Pkt, xmpp:err_not_allowed(<<"Only for the users of this server">>, Lang)),
            ok;
        {ok, ToMatrixID} ->
            Actions = mod_matrix_gw_xmpp:classify(Pkt),
            Key = {{From#jid.luser, From#jid.lserver}, ToMatrixID},
            case mnesia:dirty_read(matrix_direct, Key) of
                [#matrix_direct{room_id = RoomID}] when Actions /= [] ->
                    ?DEBUG("msg ~p~n", [{RoomID, From, ToMatrixID, Actions}]),
                    case get_existing_room_pid(Host, RoomID) of
                        {ok, Pid} ->
                            gen_statem:cast(Pid, {xmpp_actions, From, MsgID, Actions});
                        {error, _} ->
                            %% answered
                            ?INFO_MSG("no process for the direct room ~s of ~s", [RoomID, ToMatrixID]),
                            ejabberd_router:route_error(
                              Pkt, xmpp:err_recipient_unavailable(
                                     <<"The chat is not available now">>, Lang)),
                            ok
                    end;
                [_] ->
                    ok;
                [] ->
                    %% a new direct room, made with the first message that
                    %% has a body or a file; a chat state or marker alone
                    %% makes none (the gateway: the message itself is sent as
                    %% the room's actions once it is made)
                    case [A || A <- Actions, element(1, A) == text orelse element(1, A) == media] of
                        [] ->
                            ok;
                        [_ | _] ->
                            MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
                            FromMatrixID =
                                <<$@, (From#jid.luser)/binary, $:, MatrixServer/binary>>,
                            case new_direct_room(Host, FromMatrixID, ToMatrixID, []) of
                                {ok, Pid} ->
                                    gen_statem:cast(Pid, {xmpp_actions, From, MsgID, Actions});
                                _ ->
                                    ok
                            end
                    end
            end;
        error ->
            ok
    end;
route(#iq{type = Type}) when Type == error; Type == result ->
    ok;
%% MatrixRTC's <send/> and <room-state/> to the chat
%% (mod_matrix_gw_rtc); any other comes back once, marked
route(#iq{sub_els = [#xmlel{name = N}]} = IQ)
  when (N == <<"send">> orelse N == <<"room-state">>),
       not is_map_key(matrix_rtc_iq, IQ#iq.meta) ->
    case mod_matrix_gw_rtc:route_chat_iq(IQ) of
        pass -> route(IQ#iq{meta = (IQ#iq.meta)#{matrix_rtc_iq => pass}});
        ok -> ok
    end;
%% <room-info/>, <encryption/>, <redact/> to the chat
%% (mod_matrix_gw_event)
route(#iq{sub_els = [#xmlel{attrs = [_ | _]}]} = IQ)
  when not is_map_key(matrix_e2e_iq, IQ#iq.meta) ->
    case mod_matrix_gw_event:room_iq(IQ) of
        false -> route(IQ#iq{meta = (IQ#iq.meta)#{matrix_e2e_iq => pass}});
        {error, Err} -> ejabberd_router:route(xmpp:make_error(IQ, Err));
        Req -> route_e2e_iq(IQ, Req)
    end;
%% an iq to a room the gateway is in is the room
%% process's (disco#info, admin and owner requests, vCard, moderation);
%% any other comes back once, marked, for the clause below
route(#iq{to = #jid{luser = <<C, _/binary>>, lresource = <<>>} = To, meta = Meta} = IQ)
  when (C == $! orelse C == $#), not is_map_key(matrix_room_iq, Meta) ->
    Host = ejabberd_config:get_myname(),
    case catch room_id_from_xmpp(Host, To#jid.luser, IQ#iq.from) of
        {ok, RoomID, _} ->
            case get_existing_room_pid(Host, RoomID) of
                {ok, Pid} -> gen_statem:cast(Pid, {room_iq, IQ});
                _ -> route(IQ#iq{meta = Meta#{matrix_room_iq => pass}})
            end;
        _ ->
            route(IQ#iq{meta = Meta#{matrix_room_iq => pass}})
    end;
route(#iq{type = Type, lang = Lang, sub_els = [_]} = IQ0) ->
    try xmpp:decode_els(IQ0) of
        #iq{sub_els = [SubEl]} = IQ ->
            Result =
                case {Type, SubEl} of
                    {set, _} ->
                        {error, xmpp:err_not_allowed()};
                    {get, #disco_info{node = <<>>}} ->
                        %% a Matrix user's JID is
                        %% a gateway's contact (gateway/matrix), not a room
                        {Identity, Muc} =
                            case IQ#iq.to of
                                #jid{luser = <<C, _/binary>>} when C == $!; C == $# ->
                                    {#identity{category = <<"conference">>, type = <<"text">>},
                                     [?NS_MUC]};
                                _ ->
                                    {#identity{category = <<"gateway">>, type = <<"matrix">>}, []}
                            end,
                        {result,
                         #disco_info{identities = [Identity],
                                     features = Muc ++ [?NS_DISCO_INFO, ?NS_DISCO_ITEMS] ++
                                         mod_matrix_gw_e2e:disco_features(IQ#iq.to) ++
                                         mod_matrix_gw_call:disco_features(IQ#iq.to) ++
                                         mod_matrix_gw_event:disco_features(IQ#iq.to) ++
                                         mod_matrix_gw_rtc:disco_features(IQ#iq.to)}};
                    {get, #disco_info{node = _}} ->
                        {error, xmpp:err_item_not_found()};
                    {get, #disco_items{node = <<>>}} ->
                        {result, #disco_items{}};
                    {get, #disco_items{node = _}} ->
                        {error, xmpp:err_item_not_found()};
                    _ ->
                        {error, xmpp:err_service_unavailable()}
                end,
            case Result of
                {result, Res} ->
                    ejabberd_router:route(xmpp:make_iq_result(IQ, Res));
                {error, Error} ->
                    ejabberd_router:route(xmpp:make_error(IQ, Error))
            end
    catch _:{xmpp_codec, Why} ->
            ErrTxt = xmpp:io_format_error(Why),
	    Err = xmpp:err_bad_request(ErrTxt, Lang),
	    ejabberd_router:route_error(IQ0, Err),
	    ok
    end;
%% an iq with no child or several (RFC 6120
%% §8.2.3) is a bad request
route(#iq{type = Type, lang = Lang} = IQ) when Type == get; Type == set ->
    ejabberd_router:route_error(IQ, xmpp:err_bad_request(<<"One child element expected">>, Lang)),
    ok;
route(_) ->
    ok.

%% 26.09's room creation for a first message, lifted out of
%% route/1; Events follow the creation events; the
%% room's process, for the first message's actions (media's merge)
%% One at a time per local user and peer, and the
%% matrix_direct row written before it returns, so that a second stanza
%% routed before the first room's process took its {create} finds that
%% room instead of making another; a live room found then is the answer.
new_direct_room(Host, FromMatrixID, ToMatrixID, Events) ->
    case user_id_to_jid(FromMatrixID, Host) of
        #jid{luser = LUser, lserver = Host} ->
            Key = {{LUser, Host}, ToMatrixID},
            global:trans({{?MODULE, new_direct_room, Key}, self()},
                         fun() ->
                                 case mnesia:dirty_read(matrix_direct, Key) of
                                     [#matrix_direct{room_id = Old}] ->
                                         case get_existing_room_pid(Host, Old) of
                                             {ok, Pid} -> {ok, Pid};
                                             _ -> new_direct_room(Host, FromMatrixID, ToMatrixID,
                                                                  Events, Key)
                                         end;
                                     [] ->
                                         new_direct_room(Host, FromMatrixID, ToMatrixID, Events, Key)
                                 end
                         end, [node()]);
        _ ->
            error
    end.

new_direct_room(Host, FromMatrixID, ToMatrixID, Events, Key) ->
    RoomID = new_room_id(),
    ?DEBUG("new room id ~p~n", [RoomID]),
    case get_room_pid(Host, RoomID) of
        {ok, Pid} ->
            mnesia:dirty_write(#matrix_direct{local_remote = Key, room_id = RoomID}),
            MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
            gen_statem:cast(Pid, {create, MatrixServer, RoomID,
                                  FromMatrixID, ToMatrixID}),
            JSONs =
                [#{<<"content">> =>
                       #{<<"creator">> => FromMatrixID,
                         <<"room_version">> => <<"9">>},
                   <<"sender">> => FromMatrixID,
                   <<"state_key">> => <<"">>,
                   <<"type">> => ?ROOM_CREATE},
                 #{<<"content">> =>
                       %% her name and avatar
                       mod_matrix_gw_profile:member_content(
                         Host, FromMatrixID, #{<<"membership">> => <<"join">>}),
                   <<"sender">> => FromMatrixID,
                   <<"state_key">> => FromMatrixID,
                   <<"type">> => ?ROOM_MEMBER},
                 #{<<"content">> =>
                       #{<<"ban">> => 50,
                         <<"events">> =>
                             %% MatrixRTC's types too (mod_matrix_gw_rtc)
                             maps:merge(
                               mod_matrix_gw_rtc:power_levels(),
                               #{<<"m.room.avatar">> => 50,
                                 <<"m.room.canonical_alias">> => 50,
                                 <<"m.room.encryption">> => 100,
                                 <<"m.room.history_visibility">> => 100,
                                 <<"m.room.name">> => 50,
                                 <<"m.room.power_levels">> => 100,
                                 <<"m.room.server_acl">> => 100,
                                 <<"m.room.tombstone">> => 100}),
                         <<"events_default">> => 0,
                         <<"historical">> => 100,
                         <<"invite">> => 0,
                         <<"kick">> => 50,
                         <<"redact">> => 50,
                         <<"state_default">> => 50,
                         <<"users">> =>
                             #{FromMatrixID => 100,
                               ToMatrixID => 100},
                         <<"users_default">> => 0},
                   <<"sender">> => FromMatrixID,
                   <<"state_key">> => <<"">>,
                   <<"type">> => ?ROOM_POWER_LEVELS},
                 #{<<"content">> => #{<<"join_rule">> => <<"invite">>},
                   <<"sender">> => FromMatrixID,
                   <<"state_key">> => <<"">>,
                   <<"type">> => ?ROOM_JOIN_RULES},
                 #{<<"content">> => #{<<"history_visibility">> => <<"shared">>},
                   <<"sender">> => FromMatrixID,
                   <<"state_key">> => <<"">>,
                   <<"type">> => ?ROOM_HISTORY_VISIBILITY},
                 #{<<"content">> => #{<<"guest_access">> => <<"can_join">>},
                   <<"sender">> => FromMatrixID,
                   <<"state_key">> => <<"">>,
                   <<"type">> => <<"m.room.guest_access">>},
                 #{<<"content">> =>
                       #{<<"is_direct">> => true,
                         <<"membership">> => <<"invite">>},
                   <<"sender">> => FromMatrixID,
                   <<"state_key">> => ToMatrixID,
                   <<"type">> => ?ROOM_MEMBER}
                ] ++ Events,
            lists:foreach(fun(JSON) ->
                                  gen_statem:cast(Pid, {add_event, JSON})
                          end, JSONs),
            {ok, Pid};
        {error, Reason} ->
            ?INFO_MSG("no direct room for ~s: ~p", [ToMatrixID, Reason]),
            ok
    end.

get_missing_events(Host, Origin, RoomID, EarliestEvents, LatestEvents, Limit, MinDepth) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            Events = gen_statem:call(
                       Pid, {get_missing_events, Origin, EarliestEvents, LatestEvents,
                             Limit, MinDepth}),
            [E#event.json || E <- Events];
        {error, _} ->
            %%TODO
            []
    end.

get_state_ids(Host, Origin, RoomID, EventID) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            gen_statem:call(
              Pid, {get_state_ids, Origin, EventID});
        {error, _} ->
            %%TODO
            {error, room_not_found}
    end.

get_rooms_list() ->
    mnesia:dirty_all_keys(matrix_room).

get_event(Host, RoomID, EventID) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            gen_statem:call(Pid, {get_event, EventID});
        {error, _} ->
            %%TODO
            {error, room_not_found}
    end.

%% An event for the federation, only to a server
%% joined to its room (as /get_missing_events and /state_ids have it).
get_event(Host, Origin, RoomID, EventID) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            gen_statem:call(Pid, {get_event, Origin, EventID});
        {error, _} ->
            {error, room_not_found}
    end.

make_join(Host, RoomID, UserID, Params) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            gen_statem:call(Pid, {make_join, UserID, Params});
        {error, _} ->
            {error, room_not_found}
    end.

send_join(Host, Origin, RoomID, EventID, JSON) ->
    case process_pdu(Host, Origin, JSON) of
        {ok, EventID} ->
            {ok, EventJSON} = get_event(Host, RoomID, EventID),
            {ok, AuthChain, StateMap} = get_state_ids(Host, Origin, RoomID, EventID),
            AuthChainJSON =
                lists:map(fun(EID) -> {ok, E} = get_event(Host, RoomID, EID), E end, AuthChain),
            StateMapJSON =
                lists:map(fun(EID) -> {ok, E} = get_event(Host, RoomID, EID), E end, StateMap),
            MyOrigin = mod_matrix_gw_opt:matrix_domain(Host),
            Res = #{<<"event">> => EventJSON,
                    <<"state">> => StateMapJSON,
                    <<"auth_chain">> => AuthChainJSON,
                    <<"origin">> => MyOrigin},
            {ok, Res};
        {ok, _} ->
            {error, <<"Bad event id">>};
        {error, _} = Error ->
            Error
    end.

create_new_room(Host, XMPPID, MatrixID) ->
    RoomID = new_room_id(),
    case get_room_pid(Host, RoomID) of
        {ok, Pid} ->
            MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
            gen_statem:cast(Pid, {create, MatrixServer, RoomID,
                                  XMPPID, MatrixID}),
            {ok, RoomID};
        {error, _} = Error ->
            Error
    end.

room_add_event(Host, RoomID, Event) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            gen_statem:call(Pid, {add_event, Event});
        {error, _} ->
            {error, room_not_found}
    end.

%% The version of the room of a PDU when this gateway has the
%% room, else undefined.
get_known_room_version(#{<<"room_id">> := RoomID}) when is_binary(RoomID) ->
    Host = ejabberd_config:get_myname(),
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} -> catch get_room_version(Pid);
        _ -> undefined
    end;
get_known_room_version(_) ->
    undefined.

%% The room's state at EventID (before it) as events, with their
%% auth chain, for /state (26.09 served the ids alone).
get_state(Host, Origin, RoomID, EventID) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} -> gen_statem:call(Pid, {get_state, Origin, EventID});
        {error, _} -> {error, room_not_found}
    end.

%% The leave event a remote user signs to leave the room or
%% decline its invite (/make_leave).
make_leave(Host, RoomID, UserID) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} -> gen_statem:call(Pid, {make_leave, UserID});
        {error, _} -> {error, room_not_found}
    end.

%% The EDUs of a /send transaction (26.09 read none): typing
%% and read receipts of Origin's own users in the rooms this gateway is in
%% go to their rooms; anything else is ignored.
process_edus(Host, Origin, EDUs) ->
    lists:foreach(
      fun(EDU) ->
              lists:foreach(
                fun(Item) ->
                        RoomID = element(2, Item),
                        UserID = element(3, Item),
                        case catch mod_matrix_gw:get_id_domain_exn(UserID) of
                            Origin ->
                                case get_existing_room_pid(Host, RoomID) of
                                    {ok, Pid} -> gen_statem:cast(Pid, {edu, Item});
                                    _ -> ok
                                end;
                            _ ->
                                ?DEBUG("EDU from ~s about another server's user: ~p",
                                       [Origin, Item])
                        end
                end,
                case mod_matrix_gw_xmpp:parse_edu(EDU) of
                    [] ->
                        %% m.presence (not mapped), an end-to-end encryption
                        %% type, or one unknown
                        ?DEBUG("EDU from ~s ignored: ~p",
                               [Origin, catch maps:get(<<"edu_type">>, EDU)]),
                        [];
                    Items ->
                        Items
                end)
      end, EDUs).

%% Brings back the rooms mod_matrix_gw_store keeps, at start.
restore_rooms(Host) ->
    lists:foreach(
      fun(RoomID) ->
              case get_room_pid(Host, RoomID) of
                  {ok, _} -> ok;
                  Err -> ?WARNING_MSG("Matrix room ~s not restored: ~p", [RoomID, Err])
              end
      end, mod_matrix_gw_store:rooms()).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> gen_statem:init_result(term()).
init([Host, RoomID]) ->
    ServiceHost = mod_matrix_gw_opt:host(Host),
    {ok, RID} = room_id_to_xmpp(RoomID),
    RoomJID = jid:make(RID, ServiceHost),
    mnesia:dirty_write(
      #matrix_room{room_id = RoomID,
                   pid = self()}),
    %% a room kept across a restart comes back as it was
    {ok, state_name,
     restore(#data{host = Host,
                   room_id = RoomID,
                   room_jid = RoomJID,
                   room_version = binary_to_room_version(<<"9">>)})}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the gen_statem runs with CallbackMode =:= handle_event_function
%% this function is called for every event a gen_statem receives.
%% @end
%%--------------------------------------------------------------------
-spec handle_event(
        gen_statem:event_type(), Msg :: term(),
        State :: term(), Data :: term()) ->
                          gen_statem:handle_event_result().
handle_event({call, From}, get_room_version, _State, Data) ->
    {keep_state, Data, [{reply, From, Data#data.room_version}]};
handle_event({call, From}, get_latest_events, _State, Data) ->
    {keep_state, Data, [{reply, From, Data#data.latest_events}]};
%% set_latest_events is for debugging only
handle_event({call, From}, {set_latest_events, LE}, _State, Data) ->
    {keep_state, Data#data{latest_events = LE}, [{reply, From, ok}]};
handle_event({call, From}, {find_event, EventID}, _State, Data) ->
    Res = maps:find(EventID, Data#data.events),
    {keep_state, Data, [{reply, From, Res}]};
%% what a caller hands over never ends the room
%% (a crash of a direct chat's process left the chat dead until a restart)
handle_event({call, From}, {partition_missed_events, EventIDs}, _State, Data) ->
    Res = try lists:partition(
                fun(EventID) ->
                        maps:is_key(EventID, Data#data.events)
                end, EventIDs)
          catch _:_ -> {[], []}
          end,
    {keep_state, Data, [{reply, From, Res}]};
handle_event({call, From}, {partition_events_with_statemap, EventIDs}, _State, Data) ->
    Res = try lists:partition(
                fun(EventID) ->
                        case maps:find(EventID, Data#data.events) of
                            {ok, #event{state_map = undefined}} -> false;
                            {ok, _} -> true;
                            error -> false
                        end
                end, EventIDs)
          catch _:_ -> {[], []}
          end,
    {keep_state, Data, [{reply, From, Res}]};
%% whether a server is in the room (process_pdu/3)
handle_event({call, From}, {is_server_joined, Server}, _State, Data) ->
    {keep_state_and_data, [{reply, From, catch is_server_joined(Server, Data)}]};
handle_event({call, From}, {auth_and_store_external_events, EventList}, _State, Data) ->
    try
        Data2 = do_auth_and_store_external_events(EventList, Data),
        {keep_state, Data2, [{reply, From, ok}]}
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed auth_and_store_external_events: ~p", [failure(Class, Reason, ST)]),
            {keep_state, Data, [{reply, From, {error, Reason}},
                                {next_event, internal, update_client}]}
    end;
handle_event({call, From}, {resolve_auth_store_event, Event}, State, Data) ->
    handle_event({call, From}, {resolve_auth_store_event, Event, false}, State, Data);
handle_event({call, From}, {resolve_auth_store_event, Event, SoftFailCheck}, _State, Data) ->
    try
        Data2 = do_resolve_auth_store_event(Event, SoftFailCheck, Data),
        {keep_state, Data2, [{reply, From, ok},
                             {next_event, internal, update_client}]}
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed resolve_auth_store_event: ~p", [failure(Class, Reason, ST)]),
            {keep_state, Data, [{reply, From, {error, Reason}},
                                {next_event, internal, update_client}]}
    end;
handle_event({call, From},
             {get_missing_events, Origin, EarliestEvents, LatestEvents, Limit, MinDepth},
             _State, Data) ->
    try
        PDUs = do_get_missing_events(Origin, EarliestEvents, LatestEvents, Limit, MinDepth, Data),
        {keep_state_and_data, [{reply, From, PDUs}]}
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed get_missing_events: ~p", [failure(Class, Reason, ST)]),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;
handle_event({call, From},
             {get_state_ids, Origin, EventID},
             _State, Data) ->
    try
        Reply = do_get_state_ids(Origin, EventID, Data),
        {keep_state_and_data, [{reply, From, Reply}]}
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed get_state_ids: ~p", [failure(Class, Reason, ST)]),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;
handle_event({call, From},
             {get_event, EventID},
             _State, Data) ->
    try
        Reply =
            case maps:find(EventID, Data#data.events) of
                {ok, Event} ->
                    {ok, Event#event.json};
                _ ->
                    {error, event_not_found}
            end,
        {keep_state_and_data, [{reply, From, Reply}]}
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed get_event: ~p", [failure(Class, Reason, ST)]),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;
handle_event({call, From}, {get_event, Origin, EventID}, _State, Data) ->
    Reply =
        case is_server_joined(Origin, Data) of
            true ->
                case maps:find(EventID, Data#data.events) of
                    {ok, Event} -> {ok, Event#event.json};
                    _ -> {error, event_not_found}
                end;
            false ->
                {error, not_allowed}
        end,
    {keep_state_and_data, [{reply, From, Reply}]};
handle_event({call, From},
             {make_join, UserID, Params},
             _State, Data) ->
    try
        Ver = (Data#data.room_version)#room_version.id,
        Reply =
            case lists:member({<<"ver">>, Ver}, Params) of
                true ->
                    JSON = #{<<"content">> =>
                                 #{<<"membership">> => <<"join">>},
                             <<"sender">> => UserID,
                             <<"state_key">> => UserID,
                             <<"type">> => ?ROOM_MEMBER},
                    {JSON2, _} = fill_event(JSON, Data),
                    Event = json_to_event(JSON2, Data#data.room_version),
                    case check_event_auth(Event, Data) of
                        true ->
                            Res = #{<<"event">> => JSON2,
                                    <<"room_version">> => Ver},
                            {ok, Res};
                        false ->
                            {error, not_invited}
                    end;
                false ->
                    {error, {incompatible_version, Ver}}
            end,
        {keep_state_and_data, [{reply, From, Reply}]}
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed make_join: ~p", [failure(Class, Reason, ST)]),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;
%% /state, the state before EventID as events
handle_event({call, From}, {get_state, Origin, EventID}, _State, Data) ->
    Reply =
        try do_get_state_ids(Origin, EventID, Data) of
            {ok, AuthChain, PDUs} ->
                JSON = fun(IDs) -> [(get_event_exn(E, Data))#event.json || E <- IDs] end,
                {ok, JSON(AuthChain), JSON(PDUs)};
            Error ->
                Error
        catch
            Class:Reason:ST ->
                ?INFO_MSG("failed get_state: ~p", [failure(Class, Reason, ST)]),
                {error, Reason}
        end,
    {keep_state_and_data, [{reply, From, Reply}]};
%% /make_leave, for a user invited or joined
handle_event({call, From}, {make_leave, UserID}, _State, Data) ->
    Reply =
        try
            JSON = #{<<"content">> => #{<<"membership">> => <<"leave">>},
                     <<"sender">> => UserID,
                     <<"state_key">> => UserID,
                     <<"type">> => ?ROOM_MEMBER},
            {JSON2, _} = fill_event(JSON, Data),
            Event = json_to_event(JSON2, Data#data.room_version),
            case check_event_auth(Event, Data) of
                true ->
                    {ok, #{<<"event">> => JSON2,
                           <<"room_version">> => (Data#data.room_version)#room_version.id}};
                false ->
                    {error, not_allowed}
            end
        catch
            Class:Reason:ST ->
                ?INFO_MSG("failed make_leave: ~p", [failure(Class, Reason, ST)]),
                {error, Reason}
        end,
    {keep_state_and_data, [{reply, From, Reply}]};
handle_event(cast, {join_direct, MatrixServer, RoomID, Sender, UserID}, _State, Data) ->
    Host = Data#data.host,
    %% TODO: check if there is another solution to "You are not invited to this room" and not receiving the first messages in the room
    timer:sleep(1000),
    case user_id_to_jid(UserID, Data) of
        #jid{lserver = Host} = UserJID ->
            %%  the matrix_direct row is written once the join
            %% is stored (direct_joined/1): a join that fails leaves the
            %% chat the user had with Sender as it was
            MakeJoinRes =
                mod_matrix_gw:send_request(
                  Host, get, MatrixServer,
                  [<<"_matrix">>, <<"federation">>, <<"v1">>, <<"make_join">>,
                   RoomID, UserID],
                  [{<<"ver">>, V} || V <- supported_versions()],
                  none,
                  [{timeout, 5000}],
                  [{sync, true},
                   {body_format, binary}]),
            ?DEBUG("make_join ~p~n", [MakeJoinRes]),
            case MakeJoinRes of
                {ok, {{_, 200, _}, _Headers, Body}} ->
                    try misc:json_decode(Body) of
                        #{<<"event">> := Event,
                          <<"room_version">> := SRoomVersion} ->
                            case binary_to_room_version(SRoomVersion) of
                                false ->
                                    ?DEBUG("unsupported room version on make_join: ~p", [MakeJoinRes]),
                                    join_direct_failed(UserJID, Sender, RoomID);
                                #room_version{} = RoomVersion ->
                                    Origin = mod_matrix_gw_opt:matrix_domain(Host),
                                    %% her name and avatar
                                    Event1 =
                                        Event#{<<"content">> =>
                                                   mod_matrix_gw_profile:member_content(
                                                     Host, UserID,
                                                     maps:get(<<"content">>, Event, #{}))},
                                    Event2 =
                                        Event1#{<<"origin">> => Origin,
                                               <<"origin_server_ts">> =>
                                                   erlang:system_time(millisecond)},
                                    CHash = mod_matrix_gw:content_hash(Event2),
                                    Event3 =
                                        Event2#{<<"hashes">> =>
                                                    #{<<"sha256">> =>
                                                          mod_matrix_gw:base64_encode(CHash)}},
                                    Event4 = mod_matrix_gw:sign_event(Host, Event3, RoomVersion),
                                    EventID = mod_matrix_gw:get_event_id(Event4, RoomVersion),
                                    SendJoinRes =
                                        mod_matrix_gw:send_request(
                                          Data#data.host, put, MatrixServer,
                                          [<<"_matrix">>, <<"federation">>,
                                           <<"v2">>, <<"send_join">>,
                                           RoomID, EventID],
                                          [],
                                          Event4,
                                          [{connect_timeout, 5000},
                                           {timeout, 60000}],
                                          [{sync, true},
                                           {body_format, binary}]),
                                    ?DEBUG("send_join ~p~n", [SendJoinRes]),
                                    process_send_join_res(
                                      MatrixServer, SendJoinRes, RoomVersion,
                                      Data#data{
                                        kind = #direct{local_user = UserJID,
                                                       remote_user = Sender},
                                        room_version = RoomVersion})
                            end;
                        _JSON ->
                            ?DEBUG("received bad JSON on make_join: ~p", [MakeJoinRes]),
                            join_direct_failed(UserJID, Sender, RoomID)
                    catch
                        _:_ ->
                            ?DEBUG("received bad JSON on make_join: ~p", [MakeJoinRes]),
                            join_direct_failed(UserJID, Sender, RoomID)
                    end;
                _ ->
                    ?DEBUG("failed make_join: ~p", [MakeJoinRes]),
                    join_direct_failed(UserJID, Sender, RoomID)
            end;
        UserJID ->
            ?INFO_MSG("bad join user id: ~p", [{UserID, UserJID}]),
            {stop, normal}
    end;
handle_event(cast, {join, UserJID, Packet, Via}, _State, Data) ->
    Host = Data#data.host,
    {LUser, LServer, LResource} = jid:tolower(UserJID),
    case Data#data.kind of
        #multi{users = #{{LUser, LServer} := {online, #{LResource := _}}}} ->
            {keep_state_and_data, []};
        #multi{} = Kind ->
            case user_id_from_jid(UserJID, Host) of
                {ok, UserID} ->
                    JoinTS = erlang:system_time(millisecond),
                    JSON = #{<<"content">> =>
                                 member_profile(Host, UserID, #{<<"membership">> => <<"join">>}),
                             <<"sender">> => UserID,
                             <<"state_key">> => UserID,
                             <<"type">> => ?ROOM_MEMBER},
                    {History, Since} = requested_history(Packet),
                    Users = Kind#multi.users,
                    Resources =
                        case Users of
                            #{{LUser, LServer} := {online, Rs}} -> Rs;
                            #{{LUser, LServer} := {offline, TimerRef}} ->
                                erlang:cancel_timer(TimerRef),
                                #{};
                            _ -> #{}
                        end,
                    RoomJID = jid:remove_resource(xmpp:get_to(Packet)),
                    Data2 =
                        Data#data{
                          kind =
                              Kind#multi{
                                users =
                                    Users#{{LUser, LServer} =>
                                               {online,
                                                Resources#{LResource =>
                                                               (multi_user(Packet))#multi_user{
                                                                 join_ts = JoinTS,
                                                                 room_jid = RoomJID,
                                                                 history = History,
                                                                 since = Since}}}}}},
                    %% a user the room already has as a member (her
                    %% other resource is in, or the room came back after a
                    %% restart) joins with no new Matrix event
                    case is_joined_member(UserID, Data) of
                        true ->
                            {keep_state, send_join_info(UserJID, UserID, RoomJID,
                                                        restored_backfill(Data2)), []};
                        false ->
                            %% a join the room refuses
                            %% (not invited, banned) is the presence's error
                            join_member(UserJID, JSON, Packet, Data, Data2)
                    end;
                error ->
                    ?INFO_MSG("bad join user id: ~p", [UserJID]),
                    {keep_state_and_data, []}
            end;
        #direct{} ->
            %% a direct chat is no room to join
            ejabberd_router:route_error(
              Packet, xmpp:err_not_acceptable(<<"This is a direct chat, not a room">>,
                                              xmpp:get_lang(Packet))),
            {keep_state_and_data, []};
        _ ->
            Lang = xmpp:get_lang(Packet),
            case user_id_from_jid(UserJID, Host) of
                {ok, UserID} ->
                    %% TODO: async
                    RoomID = Data#data.room_id,
                    case Via of
                        MatrixServer when is_binary(MatrixServer) ->
                            MakeJoinRes =
                                mod_matrix_gw:send_request(
                                  Host, get, MatrixServer,
                                  [<<"_matrix">>, <<"federation">>, <<"v1">>, <<"make_join">>,
                                   RoomID, UserID],
                                  [{<<"ver">>, V} || V <- supported_versions()],
                                  none,
                                  [{timeout, 5000}],
                                  [{sync, true},
                                   {body_format, binary}]),
                            ?DEBUG("make_join ~p~n", [MakeJoinRes]),
                            case MakeJoinRes of
                                {ok, {{_, 200, _}, _Headers, Body}} ->
                                    try misc:json_decode(Body) of
                                        #{<<"event">> := Event,
                                          <<"room_version">> := SRoomVersion} ->
                                            case binary_to_room_version(SRoomVersion) of
                                                false ->
                                                    ?DEBUG("unsupported room version on make_join: ~p", [MakeJoinRes]),
                                                    {stop, normal};
                                                #room_version{} = RoomVersion ->
                                                    JoinTS = erlang:system_time(millisecond),
                                                    Origin = mod_matrix_gw_opt:matrix_domain(Host),
                                                    Event2 =
                                                        Event#{<<"origin">> => Origin,
                                                               <<"origin_server_ts">> => JoinTS,
                                                               %% her name and avatar
                                                               <<"content">> =>
                                                                   member_profile(Host, UserID,
                                                                                  maps:get(<<"content">>, Event, #{}))},
                                                    CHash = mod_matrix_gw:content_hash(Event2),
                                                    Event3 =
                                                        Event2#{<<"hashes">> =>
                                                                    #{<<"sha256">> =>
                                                                          mod_matrix_gw:base64_encode(CHash)}},
                                                    Event4 = mod_matrix_gw:sign_event(Host, Event3, RoomVersion),
                                                    EventID = mod_matrix_gw:get_event_id(Event4, RoomVersion),
                                                    SendJoinRes =
                                                        mod_matrix_gw:send_request(
                                                          Data#data.host, put, MatrixServer,
                                                          [<<"_matrix">>, <<"federation">>,
                                                           <<"v2">>, <<"send_join">>,
                                                           RoomID, EventID],
                                                          [],
                                                          Event4,
                                                          [{connect_timeout, 5000},
                                                           {timeout, 60000}],
                                                          [{sync, true},
                                                           {body_format, binary}]),
                                                    RoomJID = jid:remove_resource(xmpp:get_to(Packet)),
                                                    ?DEBUG("send_join ~p~n", [SendJoinRes]),
                                                    process_send_join_res(
                                                      MatrixServer, SendJoinRes, RoomVersion,
                                                      Data#data{
                                                        kind =
                                                            #multi{users =
                                                                       #{{LUser, LServer} =>
                                                                             {online,
                                                                              #{LResource => (multi_user(Packet))#multi_user{join_ts = JoinTS,
                                                                                                                             room_jid = RoomJID}}}}},
                                                        room_version = RoomVersion})
                                            end;
                                        _JSON ->
                                            ?DEBUG("received bad JSON on make_join: ~p", [MakeJoinRes]),
                                            Txt = <<"received bad JSON on make_join">>,
                                            Err = xmpp:err_bad_request(Txt, Lang),
                                            ejabberd_router:route_error(Packet, Err),
                                            {stop, normal}
                                    catch
                                        _:_ ->
                                            ?DEBUG("received bad JSON on make_join: ~p", [MakeJoinRes]),
                                            Txt = <<"received bad JSON on make_join">>,
                                            Err = xmpp:err_bad_request(Txt, Lang),
                                            ejabberd_router:route_error(Packet, Err),
                                            {stop, normal}
                                    end;
                                {ok, {{_, 400, _}, _Headers, Body}} ->
                                    ?DEBUG("failed make_join: ~p", [MakeJoinRes]),
                                    Txt = <<"make_join failed: ", Body/binary>>,
                                    Err = xmpp:err_bad_request(Txt, Lang),
                                    ejabberd_router:route_error(Packet, Err),
                                    {stop, normal};
                                _ ->
                                    ?DEBUG("failed make_join: ~p", [MakeJoinRes]),
                                    Txt = <<"make_join failed">>,
                                    Err = xmpp:err_bad_request(Txt, Lang),
                                    ejabberd_router:route_error(Packet, Err),
                                    {stop, normal}
                            end;
                        undefined ->
                            ?DEBUG("don't know which server to connect to", []),
                            Txt = <<"unknown remote server">>,
                            Err = xmpp:err_bad_request(Txt, Lang),
                            ejabberd_router:route_error(Packet, Err),
                            {stop, normal}
                    end;
                error ->
                    ?INFO_MSG("bad join user id: ~p", [UserJID]),
                    Txt = <<"bad user id">>,
                    Err = xmpp:err_bad_request(Txt, Lang),
                    ejabberd_router:route_error(Packet, Err),
                    {stop, normal}
            end
    end;
handle_event(cast, {leave, UserJID}, _State, Data) ->
    Host = Data#data.host,
    {LUser, LServer, LResource} = jid:tolower(UserJID),
    %% the server shutting down closes the sessions; the rooms
    %% keep their members, who have RESTORE_GRACE to come back after it
    Stopping = mod_matrix_gw:is_stopping(),
    case Data#data.kind of
        _ when Stopping ->
            {keep_state_and_data, []};
        #multi{users = #{{LUser, LServer} := {online, #{LResource := #multi_user{room_jid = LeftRoomJID}} = Resources}} = Users} ->
            %% the leaving occupant's own unavailable presence
            %% (XEP-0045 §7.14), which 26.09 never sent
            send_self_unavailable(UserJID, LeftRoomJID, Data),
            Resources2 = maps:remove(LResource, Resources),
            if
                Resources2 == #{} ->
                    %% a resource whose session is
                    %% gone (c2s closes it before it sends the unavailable
                    %% presence: stream management gave up, the phone was
                    %% away) did not leave: leave_timeout (ten minutes by
                    %% default, as after a restart) to come back; one whose
                    %% session is still there left the room: at once
                    LeaveTimeout =
                        case ejabberd_sm:get_session_pid(LUser, LServer, LResource) of
                            none -> mod_matrix_gw_opt:leave_timeout(Host) * 1000;
                            _ -> 0
                        end,
                    TimerRef = erlang:start_timer(LeaveTimeout, self(),
                                                  {leave, LUser, LServer}),
                    Users2 = Users#{{LUser, LServer} => {offline, TimerRef}},
                    Kind = (Data#data.kind)#multi{users = Users2},
                    Data2 = Data#data{kind = Kind},
                    {keep_state, Data2, []};
                true ->
                    Users2 = Users#{{LUser, LServer} => {online, Resources2}},
                    Kind = (Data#data.kind)#multi{users = Users2},
                    Data2 = Data#data{kind = Kind},
                    {keep_state, Data2, []}
            end;
        _ ->
            {keep_state_and_data, []}
    end;
handle_event(cast, {create, _MatrixServer, RoomID, LocalUserID, RemoteUserID}, _State, Data) ->
    Host = Data#data.host,
    case user_id_to_jid(LocalUserID, Data) of
        #jid{lserver = Host} = UserJID ->
            Key = {{UserJID#jid.luser, UserJID#jid.lserver}, RemoteUserID},
            %% a direct chat whose room lives is not
            %% taken over by a second one made meanwhile
            case mnesia:dirty_read(matrix_direct, Key) of
                [#matrix_direct{room_id = Old}] when Old /= RoomID ->
                    case get_existing_room_pid(Host, Old) of
                        {ok, OldPid} when is_pid(OldPid), node(OldPid) == node() ->
                            case is_process_alive(OldPid) of
                                true ->
                                    ?INFO_MSG("direct room ~s of ~ts kept, ~s not made its own",
                                              [Old, jid:encode(UserJID), RoomID]);
                                false ->
                                    mnesia:dirty_write(#matrix_direct{local_remote = Key, room_id = RoomID})
                            end;
                        _ ->
                            mnesia:dirty_write(#matrix_direct{local_remote = Key, room_id = RoomID})
                    end;
                _ ->
                    mnesia:dirty_write(#matrix_direct{local_remote = Key, room_id = RoomID})
            end,
            {keep_state,
             Data#data{kind = #direct{local_user = UserJID,
                                      remote_user = RemoteUserID}}, []};
        UserJID ->
            ?INFO_MSG("bad create user id: ~p", [{LocalUserID, UserJID}]),
            {stop, normal}
    end;
handle_event(cast, {add_event, JSON}, _State, Data) ->
    try
        {Data2, _Event} = add_event(JSON, Data),
        {keep_state, Data2, [{next_event, internal, update_client}]}
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed add_event: ~p", [failure(Class, Reason, ST)]),
            {keep_state, Data, []}
    end;
handle_event({call, From}, {add_event, JSON}, _State, Data) ->
    try
        {Data2, Event} = add_event(JSON, Data),
        {keep_state, Data2, [{reply, From, {ok, Event#event.id}},
                             {next_event, internal, update_client}]}
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed add_event: ~p", [failure(Class, Reason, ST)]),
            {keep_state, Data, []}
    end;
%% a local user's <event/>, and the room iqs (mod_matrix_gw_event)
handle_event(cast, {e2e_event, From, To, MsgID, Lang, Type, Content}, _State, Data) ->
    Data2 = e2e_event(From, To, MsgID, Lang, Type, Content, Data),
    {keep_state, Data2, [{next_event, internal, update_client}]};
handle_event(cast, {e2e_iq, IQ, Req}, _State, Data) ->
    {Reply, Data2} =
        try e2e_iq(IQ, Req, Data)
        catch Class:Reason:ST ->
                ?INFO_MSG("failed room iq ~p in ~s: ~p",
                          [Req, Data#data.room_id, failure(Class, Reason, ST)]),
                {xmpp:make_error(IQ, xmpp:err_internal_server_error()), Data}
        end,
    ejabberd_router:route(Reply),
    {keep_state, Data2, [{next_event, internal, update_client}]};
%% MatrixRTC's <send/> and <room-state/>, and a delayed event
%% fired (mod_matrix_gw_rtc)
handle_event(cast, {rtc_iq, IQ, Req}, _State, Data) ->
    {Reply, Data2} =
        try rtc_iq(IQ, Req, Data)
        catch Class:Reason:ST ->
                ?INFO_MSG("failed RTC iq in ~s: ~p",
                          [Data#data.room_id, failure(Class, Reason, ST)]),
                {xmpp:make_error(IQ, xmpp:err_internal_server_error()), Data}
        end,
    ejabberd_router:route(Reply),
    {keep_state, Data2, [{next_event, internal, update_client}]};
handle_event(cast, {rtc_fire, {LUser, LServer}, JSON}, _State, Data) ->
    Data2 = case owns_room(jid:make(LUser, LServer), Data) of
                true ->
                    case local_add(JSON, Data) of
                        {ok, D2} ->
                            D2;
                        {error, Why} ->
                            ?INFO_MSG("delayed ~s of ~s in ~s refused: ~p",
                                      [maps:get(<<"type">>, JSON), maps:get(<<"sender">>, JSON),
                                       Data#data.room_id, Why]),
                            Data
                    end;
                false ->
                    Data
            end,
    {keep_state, Data2, [{next_event, internal, update_client}]};
handle_event({call, From}, e2e_members, _State, Data) ->
    Members = try state_members(current_state_map(Data), Data)
              catch _:_ -> []
              end,
    {keep_state_and_data, [{reply, From, Members}]};
%% what an XMPP message asks of the room (mod_matrix_gw_xmpp:classify/1);
%% in an encrypted room only what is no content (typing, receipts), the
%% rest refused <not-encrypted/>
handle_event(cast, {xmpp_actions, From, MsgID, Actions0}, _State, Data) ->
    Actions =
        case lists:any(fun mod_matrix_gw_event:content_action/1, Actions0)
            andalso is_encrypted(Data) of
            true ->
                ejabberd_router:route(
                  mod_matrix_gw_event:not_encrypted_message(chat_jid(From, Data), From, MsgID, <<>>)),
                [A || A <- Actions0, not mod_matrix_gw_event:content_action(A)];
            false ->
                Actions0
        end,
    case user_id_from_jid(From, Data#data.host) of
        {ok, UserID} ->
            {Data2, Refused} =
                lists:foldl(
                  fun(Action, {D, R}) ->
                          try {xmpp_action(Action, UserID, MsgID, D), R}
                          catch
                              error:{too_large, _} ->
                                  %% the message bounces
                                  ejabberd_router:route(
                                    #message{id = MsgID, type = error, from = chat_jid(From, D),
                                             to = From, sub_els = [mod_matrix_gw_event:too_large(<<>>)]}),
                                  {D, R};
                              error:{event_auth_error, _} = Why ->
                                  ?INFO_MSG("~p of ~s refused in ~s: ~p",
                                            [element(1, Action), UserID, D#data.room_id, Why]),
                                  {D, first_error(R, refused)};
                              %% a reaction or marker naming a
                              %% message nobody here knows, a share of a
                              %% file not in the store with no text
                              error:{unknown_target, _} ->
                                  {D, first_error(R, xmpp:err_item_not_found(
                                                       <<"No such message in this chat">>, <<"en">>))};
                              error:{not_shareable, _} ->
                                  {D, first_error(R, xmpp:err_not_acceptable(
                                                       <<"The file is not in this server's store">>,
                                                       <<"en">>))};
                              Class:Reason:ST ->
                                  ?INFO_MSG("failed ~p in ~s: ~p",
                                            [element(1, Action), D#data.room_id,
                                             failure(Class, Reason, ST)]),
                                  {D, R}
                          end
                  end, {Data, undefined}, Actions),
            %% what the room's rules refuse is
            %% answered, once a stanza: not-acceptable to one who is not an
            %% occupant of the room (XEP-0045 §7.4: kicked, never let in),
            %% forbidden otherwise; so is what names nothing (L14)
            case Refused of
                undefined -> ok;
                refused -> ejabberd_router:route(refused_message(From, MsgID, Data2));
                Err -> ejabberd_router:route(#message{id = MsgID, type = error,
                                                      from = chat_jid(From, Data2), to = From,
                                                      sub_els = [Err]})
            end,
            {keep_state, Data2, [{next_event, internal, update_client}]};
        error ->
            keep_state_and_data
    end;
%% a typing notice or read receipt from the federation
handle_event(cast, {edu, Item}, _State, Data) ->
    %%  of a user joined to this room only
    try is_joined_member(element(3, Item), Data) andalso notify_edu(Item, Data)
    catch Class:Reason:ST ->
            ?INFO_MSG("failed EDU ~p: ~p", [Item, failure(Class, Reason, ST)])
    end,
    keep_state_and_data;
%% an iq to the room (room_iq/2)
handle_event(cast, {room_iq, IQ}, _State, Data) ->
    {Reply, Data2} =
        try room_iq(IQ, Data)
        catch Class:Reason:ST ->
                ?INFO_MSG("failed iq in ~s: ~p", [Data#data.room_id, failure(Class, Reason, ST)]),
                {xmpp:make_error(IQ, xmpp:err_internal_server_error()), Data}
        end,
    case Reply of
        noreply -> ok;
        _ -> ejabberd_router:route(Reply)
    end,
    {keep_state, Data2, [{next_event, internal, update_client}]};
%% a subject from an occupant is the room's topic
handle_event(cast, {subject, From, Topic, Pkt}, _State, Data) ->
    Res = case room_user(From, Data) of
              {ok, UserID} -> local_add(state_json(?ROOM_TOPIC, UserID, #{<<"topic">> => Topic}), Data);
              Err -> Err
          end,
    case Res of
        {ok, Data2} ->
            {keep_state, Data2, [{next_event, internal, update_client}]};
        {error, Why} ->
            ejabberd_router:route_error(Pkt, xmpp_error(Why)),
            keep_state_and_data
    end;
%% invites of an occupant (mediated, XEP-0045 §7.8.2, or
%% direct, XEP-0249) as m.room.member invites
handle_event(cast, {invite, From, Invites, Pkt}, _State, Data) ->
    Host = Data#data.host,
    Res = case room_user(From, Data) of
              {ok, UserID} ->
                  local_adds([member_json(UserID, Invitee, <<"invite">>, Reason)
                              || {JID, Reason} <- Invites,
                                 {ok, Invitee} <- [user_id_from_jid(JID, Host)]], Data);
              Err ->
                  Err
          end,
    case Res of
        {ok, Data2} ->
            {keep_state, Data2, [{next_event, internal, update_client}]};
        {error, Why} ->
            ejabberd_router:route_error(Pkt, xmpp_error(Why)),
            keep_state_and_data
    end;
%% a room made for a local user's join of an alias of
%% ours (create_room/4): version 10, invite only, she its owner
handle_event(cast, {create_multi, UserJID, Packet, Alias}, _State, #data{kind = undefined} = Data) ->
    Host = Data#data.host,
    {ok, UserID} = user_id_from_jid(UserJID, Host),
    Version = <<"10">>,
    {LUser, LServer, LResource} = jid:tolower(UserJID),
    RoomJID = jid:remove_resource(xmpp:get_to(Packet)),
    MU = (multi_user(Packet))#multi_user{join_ts = erlang:system_time(millisecond),
                                         room_jid = RoomJID, created = true},
    Data2 = Data#data{room_version = binary_to_room_version(Version),
                      kind = #multi{users = #{{LUser, LServer} => {online, #{LResource => MU}}}}},
    Events = [case E of
                  #{<<"type">> := ?ROOM_MEMBER, <<"content">> := C} ->
                      E#{<<"content">> => member_profile(Host, UserID, C)};
                  _ ->
                      E
              end || E <- mod_matrix_gw_muc:create_events(UserID, Alias, Version, <<>>)],
    case local_adds(Events, Data2) of
        {ok, Data3} ->
            ?INFO_MSG("Matrix room ~s made for ~s as ~s", [Data#data.room_id, UserID, Alias]),
            {keep_state, Data3, [{next_event, internal, update_client}]};
        {error, Why} ->
            ?INFO_MSG("Matrix room ~s not made: ~p", [Alias, Why]),
            mod_matrix_gw_muc:alias_forget(Data#data.room_id),
            ejabberd_router:route_error(Packet, xmpp:err_internal_server_error()),
            {stop, normal}
    end;
handle_event(cast, {create_multi, _UserJID, Packet, _Alias}, _State, _Data) ->
    ejabberd_router:route_error(Packet, xmpp:err_conflict()),
    keep_state_and_data;
handle_event(cast, Msg, State, Data) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {next_state, State, Data, []};
handle_event(internal, update_client, _State,
             #data{kind = #direct{local_user = JID}} = Data) ->
    try
        case update_client(Data) of
            {ok, Data2} ->
                {keep_state, Data2, []};
            {leave, LeaveReason, Data2} ->
                ?INFO_MSG("leaving ~p: ~p", [Data#data.room_id, LeaveReason]),
                Host = Data#data.host,
                MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
                LocalUserID = <<$@, (JID#jid.luser)/binary, $:, MatrixServer/binary>>,
                JSON = #{<<"content">> =>
                             #{<<"membership">> => <<"leave">>},
                         <<"sender">> => LocalUserID,
                         <<"state_key">> => LocalUserID,
                         <<"type">> => ?ROOM_MEMBER},
                {keep_state, Data2, [{next_event, cast, {add_event, JSON}}]};
            stop ->
                forget(Data),
                {stop, normal}
        end
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed update_client: ~p", [failure(Class, Reason, ST)]),
            {keep_state_and_data, []}
    end;
handle_event(internal, update_client, _State,
             #data{kind = #multi{}} = Data) ->
    try
        case update_client(Data) of
            {ok, Data2} ->
                {keep_state, Data2, []};
            stop ->
                forget(Data),
                {stop, normal}
        end
    catch
        Class:Reason:ST ->
            ?INFO_MSG("failed update_client: ~p", [failure(Class, Reason, ST)]),
            {keep_state_and_data, []}
    end;
handle_event(internal, update_client, _State, #data{kind = undefined}) ->
    {keep_state_and_data, []};
handle_event(info, {send_txn_res, RequestID, TxnID, Server, Res}, _State, Data) ->
    ?DEBUG("send_txn_res ~p", [{RequestID, TxnID, Server, Res}]),
    case Data#data.outgoing_txns of
        #{Server := {{RequestID, TxnID, Events, Count}, Queue}} ->
            Done =
                case Res of
                    {{_, 200, _}, _Headers, _Body} -> true;
                    _ when Count < ?MAX_TXN_RETRIES -> false;
                    _ -> true
                end,
            case Done of
                true ->
                    %% XEP-0184/XEP-0333 receipts of
                    %% the events the other server took
                    Data1 =
                        case Res of
                            {{_, 200, _}, _, RBody} -> send_receipts(Events, RBody, Data);
                            _ -> Data
                        end,
                    Data2 =
                        case Queue of
                            [] ->
                                Data1#data{outgoing_txns =
                                              maps:remove(Server, Data1#data.outgoing_txns)};
                            _ ->
                                send_new_txn(lists:reverse(Queue), Server, Data1)
                        end,
                    {keep_state, Data2, []};
                false ->
                    erlang:send_after(30000, self(), {resend_txn, Server}),
                    {keep_state, Data, []}
            end;
        _ ->
            {keep_state, Data, []}
    end;
handle_event(info, {resend_txn, Server}, _State, Data) ->
    case Data#data.outgoing_txns of
        #{Server := {{_RequestID, TxnID, Events, Count}, Queue}} ->
            Data2 = send_txn(TxnID, Events, Server, Count + 1, Queue, Data),
            {keep_state, Data2, []};
        _ ->
            {keep_state, Data, []}
    end;
handle_event(info, {timeout, TimerRef, {leave, LUser, LServer}}, State, Data) ->
    Host = Data#data.host,
    %% no leave of a server shutting down (the leave cast's rule)
    Stopping = mod_matrix_gw:is_stopping(),
    case Data#data.kind of
        _ when Stopping ->
            {next_state, State, Data, []};
        #multi{users = #{{LUser, LServer} := {offline, TimerRef}} = Users} ->
            Users2 = maps:remove({LUser, LServer}, Users),
            Kind = (Data#data.kind)#multi{users = Users2},
            Data2 = Data#data{kind = Kind},
            {ok, UserID} = user_id_from_jid(jid:make(LUser, LServer), Host),
            JSON = #{<<"content">> =>
                         #{<<"membership">> => <<"leave">>},
                     <<"sender">> => UserID,
                     <<"state_key">> => UserID,
                     <<"type">> => ?ROOM_MEMBER},
            {keep_state, Data2, [{next_event, cast, {add_event, JSON}}]};
        _ ->
            {next_state, State, Data, []}
    end;
%% the room's avatar read (fetch_avatar/2)
handle_event(info, {avatar_fetched, MXC, Result}, _State, Data) ->
    {keep_state, avatar_fetched(MXC, Result, Data)};
handle_event(info, Info, State, Data) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {next_state, State, Data, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(Reason, _State, Data) ->
    mnesia:dirty_delete_object(
      #matrix_room{room_id = Data#data.room_id,
                   pid = self()}),
    %% TODO: wait for messages
    case Data#data.kind of
        #direct{local_user = #jid{} = LocalUserJID,
                remote_user = RemoteUser} ->
            mnesia:dirty_delete_object(
              #matrix_direct{local_remote = {{LocalUserJID#jid.luser, LocalUserJID#jid.lserver},
                                             RemoteUser},
                             room_id = Data#data.room_id});
        _ ->
            ok
    end,
    ?INFO_MSG("terminated ~p: ~p", [Data#data.room_id, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
                         {ok, NewState :: term(), NewData :: term()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

callback_mode() ->
    handle_event_function.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_event_exn(EventID, Data) ->
    maps:get(EventID, Data#data.events).

%% A direct chat whose join failed is no room: its
%% process ends (no idle process per invite) and its lookup goes.
join_direct_failed(UserJID, Sender, RoomID) ->
    mnesia:dirty_delete_object(
      #matrix_direct{local_remote = {{UserJID#jid.luser, UserJID#jid.lserver}, Sender},
                     room_id = RoomID}),
    {stop, normal}.

%% A direct chat joined from the peer's invite is
%% the chat of the local user with the peer from now on.
direct_joined(#data{kind = #direct{local_user = #jid{luser = LUser, lserver = LServer},
                                   remote_user = Remote},
                    room_id = RoomID}) ->
    mnesia:dirty_write(#matrix_direct{local_remote = {{LUser, LServer}, Remote},
                                      room_id = RoomID});
direct_joined(_) ->
    ok.

process_send_join_res(MatrixServer, SendJoinRes, RoomVersion, Data) ->
    case SendJoinRes of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            try
                case misc:json_decode(Body) of
                    #{<<"auth_chain">> := JSONAuthChain,
                      <<"event">> := JSONEvent,
                      <<"state">> := JSONState} = JSON when is_list(JSONAuthChain),
                                                            is_list(JSONState) ->
                        AuthChain =
                            lists:map(fun(J) -> json_to_event(J, RoomVersion) end,
                                      JSONAuthChain),
                        State =
                            lists:map(fun(J) -> json_to_event(J, RoomVersion) end,
                                      JSONState),
                        Event = json_to_event(JSONEvent, RoomVersion),
                        ?DEBUG("send_join res: ~p~n", [JSON]),
                        case Data#data.kind of
                            #multi{} ->
                                %% TODO: do check_event_sig_and_hash, but faster
                                ok;
                            _ ->
                                lists:foreach(
                                  fun(E) ->
                                          ?DEBUG("send_join res check ~p~n", [E]),
                                          case check_event_sig_and_hash(Data#data.host, E) of
                                              {ok, _} -> ok;
                                              {error, Error} -> error(Error)
                                          end
                                  end, [Event] ++ AuthChain ++ State)
                        end,
                        CreateEvents =
                            lists:filter(
                              fun(#event{type = ?ROOM_CREATE,
                                         state_key = <<"">>}) -> true;
                                 (_) -> false
                              end, State),
                        RoomVersionID = RoomVersion#room_version.id,
                        case CreateEvents of
                            [#event{
                                id = CreateEventID,
                                json =
                                    #{<<"content">> :=
                                          #{<<"room_version">> :=
                                                RoomVersionID}}} =
                                 CreateEvent] ->
                                ?DEBUG("create event: ~p~n", [CreateEvent]),
                                AuthCreateEvents =
                                    lists:filtermap(
                                      fun(#event{id = ID,
                                                 type = ?ROOM_CREATE,
                                                 state_key = <<"">>}) ->
                                              {true, ID};
                                         (_) -> false
                                      end, AuthChain),
                                case AuthCreateEvents of
                                    [CreateEventID] ->
                                        Data2 = process_send_join_res2(
                                                  MatrixServer, AuthChain, Event, State,
                                                  Data),
                                        direct_joined(Data2),
                                        {keep_state, Data2, []};
                                    _ ->
                                        ?DEBUG("bad auth create events: ~p, expected: ~p", [AuthCreateEvents, [CreateEventID]]),
                                        {keep_state, Data, []}
                                end;
                            _ ->
                                ?DEBUG("bad create event: ~p", [CreateEvents]),
                                {stop, normal, Data}
                        end
                end
            catch
                error:{invalid_signature, EventID} ->
                    ?INFO_MSG("failed signature check on event ~p", [EventID]),
                    {stop, normal, Data};
                Class:Reason:ST ->
                    ?INFO_MSG("failed send_join: ~p", [failure(Class, Reason, ST)]),
                    {stop, normal, Data}
            end;
        _ ->
            ?DEBUG("failed send_join: ~p", [SendJoinRes]),
            {stop, normal, Data}
    end.

process_send_join_res2(MatrixServer, AuthChain, Event, State, Data) ->
    Data2 = do_auth_and_store_external_events(AuthChain ++ State, Data),
    StateMap =
        lists:foldl(
          fun(E, Acc) ->
                  Acc#{{E#event.type, E#event.state_key} => E#event.id}
          end, #{}, State),
    StateMap2 =
        case Event#event.state_key of
            undefined ->
                StateMap;
            _ ->
                StateMap#{{Event#event.type, Event#event.state_key} => Event#event.id}
        end,
    Event2 = Event#event{state_map = StateMap2},
    %% a room's last messages, for the joining resource's MUC
    %% history, before the join is stored and its presence sent
    Data2b =
        case Data2#data.kind of
            #multi{} -> backfill_history(MatrixServer, Event2, Data2);
            _ -> Data2
        end,
    Data3 =
        case check_event_auth(Event2, Data2b) of
            true ->
                store_event(Event2, Data2b);
            false ->
                error({event_auth_error, Event2#event.id})
        end,
    MissingEventsQuery =
        #{<<"earliest_events">> => [],
          <<"latest_events">> => [Event#event.id],
          <<"limit">> => 10},
    Host = Data3#data.host,
    Pid = self(),
    RoomID = Data3#data.room_id,
    RoomVersion = Data3#data.room_version,
    mod_matrix_gw:send_request(
      Host, post, MatrixServer,
      [<<"_matrix">>, <<"federation">>, <<"v1">>,
       <<"get_missing_events">>, RoomID],
      [],
      MissingEventsQuery,
      [{timeout, 60000}],
      [{sync, false},
       {body_format, binary},
       {receiver,
        fun({_, Res}) ->
                spawn(fun() ->
                              process_missing_events_res(
                                Host, MatrixServer, Pid, RoomID, RoomVersion,
                                {ok, Res})
                      end)
        end}]),
    Data3.

do_auth_and_store_external_events(EventList, Data) ->
    Events = maps:from_list(lists:map(fun(E) -> {E#event.id, E} end,
                                      EventList)),
    SortedEvents = simple_toposort(Events),
    ?DEBUG("topo ~p~n", [SortedEvents]),
    %% TODO: add more checks
    Data2 =
        lists:foldl(
          fun(E, Acc) ->
                  Ev = maps:get(E, Events),
                  case check_event_auth(Ev, Acc) of
                      true when is_map(Ev#event.state_map) ->
                          %% an event with the state
                          %% at it (a missing prev event and its /state)
                          %% becomes part of the room's graph as one /send
                          %% brings: allowed by that state, soft-failed
                          %% against the current one
                          case auth_before(Ev, Ev#event.state_map, Acc) of
                              true ->
                                  case soft_failed(Ev, Acc) of
                                      true -> Acc#data{events = (Acc#data.events)#{E => Ev}};
                                      false -> store_event(Ev, Acc)
                                  end;
                              false ->
                                  error({event_auth_error, E})
                          end;
                      true ->
                          store_event(Ev, Acc);
                      false ->
                          error({event_auth_error, E})
                  end
          end, Data, SortedEvents),
    Data2.

auth_and_store_external_events(Pid, EventList) ->
    gen_statem:call(Pid, {auth_and_store_external_events, EventList}).

statemap_find(Key, StateMap, Data) ->
    case maps:find(Key, StateMap) of
        {ok, #event{}} = Res ->
            Res;
        {ok, EventID} when is_binary(EventID) ->
            maps:find(EventID, Data#data.events);
        error ->
            error
    end.

check_event_auth(Event, Data) ->
    StateMap =
        maps:from_list(
          lists:map(
            fun(EID) ->
                    E = get_event_exn(EID, Data),
                    {{E#event.type, E#event.state_key}, E}
            end, Event#event.auth_events)),
    check_event_auth(Event, StateMap, Data).

check_event_auth(Event, StateMap, Data) ->
    RoomVersion = Data#data.room_version,
    case Event#event.type of
        ?ROOM_CREATE ->
            case {maps:size(StateMap), Event#event.prev_events} of
                {0, []} ->
                    Check12 =
                        case RoomVersion#room_version.hydra of
                            true ->
                                case Event#event.json of
                                    #{<<"room_id">> := _} ->
                                        false;
                                    _ ->
                                        true
                                end;
                            false ->
                                RDomain = mod_matrix_gw:get_id_domain_exn(Data#data.room_id),
                                SDomain = mod_matrix_gw:get_id_domain_exn(Event#event.sender),
                                RDomain == SDomain
                        end,
                    if
                        Check12 ->
                            %% TODO: check content.room_version
                            case RoomVersion#room_version.implicit_room_creator of
                                false ->
                                    case Event#event.json of
                                        #{<<"content">> :=
                                              #{<<"creator">> := _}} ->
                                            true;
                                        _ ->
                                            false
                                    end;
                                true ->
                                    case RoomVersion#room_version.hydra of
                                        true ->
                                            case Event#event.json of
                                                #{<<"content">> :=
                                                      #{<<"additional_creators">> := Creators}} when is_list(Creators) ->
                                                    lists:foreach(
                                                      fun(C) ->
                                                              case check_user_id(C) of
                                                                  true -> ok;
                                                                  false -> error(not_allowed)
                                                              end
                                                      end, Creators),
                                                    true;
                                                #{<<"content">> :=
                                                      #{<<"additional_creators">> := _}} ->
                                                    false;
                                                _ ->
                                                    true
                                            end;
                                        false ->
                                            true
                                    end
                            end;
                        true ->
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            case StateMap of
                #{{?ROOM_CREATE, <<"">>} := _} ->
                    case Event#event.type of
                        ?ROOM_MEMBER ->
                            case Event#event.json of
                                #{<<"content">> :=
                                      #{<<"membership">> := Membership}} ->
                                    %% TODO: join_authorised_via_users_server
                                    case Membership of
                                        <<"join">> ->
                                            check_event_auth_join(
                                              Event, StateMap, Data);
                                        <<"invite">> ->
                                            check_event_auth_invite(
                                              Event, StateMap, Data);
                                        <<"leave">> ->
                                            check_event_auth_leave(
                                              Event, StateMap, Data);
                                        <<"ban">> ->
                                            check_event_auth_ban(
                                              Event, StateMap, Data);
                                        <<"knock">> when (Data#data.room_version)#room_version.knock_join_rule ->
                                            check_event_auth_knock(
                                              Event, StateMap, Data);
                                        _ ->
                                            false
                                    end;
                                _ ->
                                    false
                            end;
                        ?ROOM_ALIASES when (Data#data.room_version)#room_version.special_case_aliases_auth ->
                            case Event#event.state_key of
                                undefined ->
                                    false;
                                StateKey ->
                                    case mod_matrix_gw:get_id_domain_exn(Event#event.sender) of
                                        StateKey ->
                                            true;
                                        _ ->
                                            false
                                    end
                            end;
                        _ ->
                            Sender = Event#event.sender,
                            case maps:find({?ROOM_MEMBER, Sender}, StateMap) of
                                {ok, #event{
                                        json = #{<<"content">> :=
                                                     #{<<"membership">> :=
                                                           <<"join">>}}}} ->
                                    case Event#event.type of
                                        ?ROOM_3PI ->
                                            SenderLevel = get_user_power_level(Event#event.sender, StateMap, Data),
                                            InviteLevel =
                                                case maps:find({?ROOM_POWER_LEVELS, <<"">>}, StateMap) of
                                                    {ok, #event{json = #{<<"content">> := #{<<"invite">> := S}}}} ->
                                                        get_int(S);
                                                    _ -> 0
                                                end,
                                            SenderLevel >= InviteLevel;
                                        _ ->
                                            case check_event_power_level(
                                                   Event, StateMap, Data) of
                                                true ->
                                                    case Event#event.type of
                                                        ?ROOM_POWER_LEVELS ->
                                                            check_event_auth_power_levels(
                                                              Event, StateMap, Data);
                                                        _ ->
                                                            true
                                                    end;
                                                false ->
                                                    false
                                            end
                                    end;
                                _ ->
                                    false
                            end
                    end;
                _ ->
                    false
            end
    end.

check_event_auth_join(Event, StateMap, Data) ->
    RoomVersion = Data#data.room_version,
    StateKey = Event#event.state_key,
    case {length(Event#event.auth_events),
          RoomVersion#room_version.implicit_room_creator,
          maps:get({?ROOM_CREATE, <<"">>}, StateMap, undefined)} of
        {1, false, #event{json = #{<<"content">> := #{<<"creator">> := StateKey}}}} ->
            ?DEBUG("creator join ~p~n", [Event]),
            true;
        {1, true, #event{sender = StateKey}} ->
            ?DEBUG("creator join ~p~n", [Event]),
            true;
        _ ->
            case Event#event.sender of
                StateKey ->
                    JoinRule =
                        case maps:find({?ROOM_JOIN_RULES, <<"">>}, StateMap) of
                            {ok, #event{
                                    json = #{<<"content">> :=
                                                 #{<<"join_rule">> := JR}}}} ->
                                JR;
                            _ ->
                                <<"invite">>
                        end,
                    case maps:find({?ROOM_MEMBER, StateKey}, StateMap) of
                        {ok, #event{
                                json = #{<<"content">> :=
                                             #{<<"membership">> :=
                                                   <<"ban">>}}}} ->
                            false;
                        {ok, #event{
                                json = #{<<"content">> :=
                                             #{<<"membership">> :=
                                                   <<"join">>}}}} ->
                            true;
                        {ok, #event{
                                json = #{<<"content">> :=
                                             #{<<"membership">> :=
                                                   SenderMembership}}}} ->
                            case {JoinRule, SenderMembership} of
                                {<<"public">>, _} -> true;
                                {<<"invite">>, <<"invite">>} -> true;
                                {<<"knock">>, <<"invite">>}
                                  when (Data#data.room_version)#room_version.knock_join_rule ->
                                    true;
                                {<<"restricted">>, <<"invite">>}
                                  when (Data#data.room_version)#room_version.restricted_join_rule ->
                                    %% TODO
                                    true;
                                {<<"knock_restricted">>, <<"invite">>}
                                  when (Data#data.room_version)#room_version.knock_restricted_join_rule ->
                                    %% TODO
                                    true;
                                _ -> false
                            end;
                        error ->
                            case JoinRule of
                                <<"public">> -> true;
                                _ -> false
                            end
                    end;
                _ ->
                    false
            end
    end.

check_event_auth_invite(Event, StateMap, Data) ->
    StateKey = Event#event.state_key,
    case Event#event.json of
        #{<<"content">> := #{<<"third_party_invite">> := _}} ->
            %% TODO
            {todo, Event};
        _ ->
            case maps:find({?ROOM_MEMBER, Event#event.sender}, StateMap) of
                {ok, #event{
                        json = #{<<"content">> :=
                                     #{<<"membership">> :=
                                           <<"join">>}}}} ->
                    case maps:find({?ROOM_MEMBER, StateKey}, StateMap) of
                        {ok, #event{
                                json = #{<<"content">> :=
                                             #{<<"membership">> :=
                                                   <<"ban">>}}}} ->
                            false;
                        {ok, #event{
                                json = #{<<"content">> :=
                                             #{<<"membership">> :=
                                                   <<"join">>}}}} ->
                            false;
                        _ ->
                            UserLevel = get_user_power_level(Event#event.sender, StateMap, Data),
                            InviteLevel =
                                case maps:find({?ROOM_POWER_LEVELS, <<"">>}, StateMap) of
                                    {ok, #event{json = #{<<"content">> := #{<<"invite">> := S}}}} ->
                                        get_int(S);
                                    _ -> 0
                                end,
                            UserLevel >= InviteLevel
                    end;
                _ ->
                    false
            end
    end.

check_event_auth_leave(Event, StateMap, Data) ->
    StateKey = Event#event.state_key,
    case maps:find({?ROOM_MEMBER, Event#event.sender}, StateMap) of
        {ok, #event{
                json = #{<<"content">> :=
                             #{<<"membership">> := SenderMembership}}}} ->
            case Event#event.sender of
                StateKey ->
                    case SenderMembership of
                        <<"invite">> -> true;
                        <<"join">> -> true;
                        <<"knock">> when (Data#data.room_version)#room_version.knock_join_rule -> true;
                        _ -> false
                    end;
                _ ->
                    case SenderMembership of
                        <<"join">> ->
                            SenderLevel = get_user_power_level(Event#event.sender, StateMap, Data),
                            CheckBan =
                                case maps:find({?ROOM_MEMBER, StateKey}, StateMap) of
                                    {ok, #event{
                                            json = #{<<"content">> :=
                                                         #{<<"membership">> :=
                                                               <<"ban">>}}}} ->
                                        BanLevel =
                                            case maps:find({?ROOM_POWER_LEVELS, <<"">>}, StateMap) of
                                                {ok, #event{json = #{<<"content">> := #{<<"ban">> := S}}}} ->
                                                    get_int(S);
                                                _ -> 50
                                            end,
                                        SenderLevel >= BanLevel;
                                    _ ->
                                        true
                                end,
                            if
                                CheckBan ->
                                    KickLevel =
                                        case maps:find({?ROOM_POWER_LEVELS, <<"">>}, StateMap) of
                                            {ok, #event{json = #{<<"content">> := #{<<"kick">> := S1}}}} ->
                                                get_int(S1);
                                            _ -> 50
                                        end,
                                    TargetLevel = get_user_power_level(StateKey, StateMap, Data),
                                    SenderLevel >= KickLevel andalso SenderLevel > TargetLevel;
                                true ->
                                    false
                            end;
                        _ ->
                            false
                    end
            end;
        _ ->
            false
    end.

check_event_auth_ban(Event, StateMap, Data) ->
    StateKey = Event#event.state_key,
    case maps:find({?ROOM_MEMBER, Event#event.sender}, StateMap) of
        {ok, #event{
                json = #{<<"content">> :=
                             #{<<"membership">> := SenderMembership}}}} ->
            case SenderMembership of
                <<"join">> ->
                    SenderLevel = get_user_power_level(Event#event.sender, StateMap, Data),
                    BanLevel =
                        case maps:find({?ROOM_POWER_LEVELS, <<"">>}, StateMap) of
                            {ok, #event{json = #{<<"content">> := #{<<"ban">> := S}}}} ->
                                get_int(S);
                            _ -> 50
                        end,
                    TargetLevel = get_user_power_level(StateKey, StateMap, Data),
                    SenderLevel >= BanLevel andalso SenderLevel > TargetLevel;
                _ ->
                    false
            end;
        _ ->
            false
    end.

check_event_auth_knock(Event, StateMap, Data) ->
    StateKey = Event#event.state_key,
    case Event#event.sender of
        StateKey ->
            JoinRule =
                case maps:find({?ROOM_JOIN_RULES, <<"">>}, StateMap) of
                    {ok, #event{
                            json = #{<<"content">> :=
                                         #{<<"join_rule">> := JR}}}} ->
                        JR;
                    _ ->
                        <<"invite">>
                end,
            IsKnock =
                case JoinRule of
                    <<"knock">> ->
                        true;
                    <<"knock_restricted">> when (Data#data.room_version)#room_version.knock_restricted_join_rule ->
                        true;
                    _ ->
                        false
                end,
            case IsKnock of
                true ->
                    case maps:find({?ROOM_MEMBER, StateKey}, StateMap) of
                        {ok, #event{
                                json = #{<<"content">> :=
                                             #{<<"membership">> :=
                                                   <<"ban">>}}}} ->
                            false;
                        {ok, #event{
                                json = #{<<"content">> :=
                                             #{<<"membership">> :=
                                                   <<"join">>}}}} ->
                            false;
                        _ ->
                            true
                    end;
                false ->
                    false
            end;
        _ ->
            false
    end.

check_event_power_level(Event, StateMap, Data) ->
    PLContent =
        case maps:find({?ROOM_POWER_LEVELS, <<"">>}, StateMap) of
            {ok, #event{json = #{<<"content">> := C}}} -> C;
            _ -> #{}
        end,
    RequiredLevel = get_event_power_level(
                      Event#event.type, Event#event.state_key, PLContent),
    UserLevel = get_user_power_level(Event#event.sender, StateMap, Data),
    if
        UserLevel >= RequiredLevel ->
            Sender = Event#event.sender,
            case Event#event.state_key of
                Sender -> true;
                <<$@, _/binary>> -> false;
                _ -> true
            end;
        true ->
            false
    end.

get_event_power_level(Type, StateKey, PL) ->
    case {StateKey, PL} of
        {_, #{<<"events">> := #{Type := Level}}} ->
            get_int(Level);
        {undefined, #{<<"events_default">> := Level}} ->
            get_int(Level);
        {undefined, _} ->
            0;
        {StateKey, #{<<"state_default">> := Level}} when is_binary(StateKey) ->
            get_int(Level);
        {StateKey, _} when is_binary(StateKey) ->
            50
    end.

get_user_power_level(User, StateMap, Data) ->
    RoomVersion = Data#data.room_version,
    PL =
        case statemap_find({?ROOM_POWER_LEVELS, <<"">>}, StateMap, Data) of
            {ok, #event{json = #{<<"content">> := C}}} -> C;
            _ -> #{}
        end,
    IsCreator =
        case {RoomVersion#room_version.hydra,
              RoomVersion#room_version.implicit_room_creator,
              statemap_find({?ROOM_CREATE, <<"">>}, StateMap, Data)} of
            {false, false,
             {ok, #event{json = #{<<"content">> := #{<<"creator">> := User}}}}} ->
                true;
            {false, true, {ok, #event{sender = User}}} ->
                true;
            {true, _, {ok, #event{sender = User}}} ->
                true;
            {true, _,
             {ok, #event{
                     json = #{<<"content">> :=
                                  #{<<"additional_creators">> := Creators}}}}}
              when is_list(Creators) ->
                lists:member(User, Creators);
            _ ->
                false
        end,
    case {RoomVersion#room_version.hydra,
          IsCreator,
          PL} of
        {true, true, _} ->
            ?CREATOR_PL;
        {_, _, #{<<"users">> := #{User := Level}}} -> get_int(Level);
        {_, _, #{<<"users_default">> := Level}} -> get_int(Level);
        {_, true, _} ->
            100;
        _ ->
            0
    end.

check_event_auth_power_levels(Event, StateMap, Data) ->
    try
        case Event#event.json of
            #{<<"content">> := NewPL = #{<<"users">> := Users}} when is_map(Users) ->
                case (Data#data.room_version)#room_version.hydra of
                    false ->
                        ok;
                    true ->
                        case statemap_find({?ROOM_CREATE, <<"">>}, StateMap, Data) of
                            {ok, #event{sender = C} = E} ->
                                Creators =
                                    case E#event.json of
                                        #{<<"content">> :=
                                              #{<<"additional_creators">> := ACs}} ->
                                            [C | ACs];
                                        _ ->
                                            [C]
                                    end,
                                case maps:size(maps:with(Creators, Users)) > 0 of
                                    true ->
                                        error(creators_in_pl);
                                    false ->
                                        ok
                                end;
                            _ ->
                                error(missed_create_event)
                        end
                end,
                CheckKeys =
                    case (Data#data.room_version)#room_version.limit_notifications_power_levels of
                        false ->
                            [<<"events">>, <<"users">>];
                        true ->
                            [<<"events">>, <<"users">>, <<"notifications">>]
                    end,
                case (Data#data.room_version)#room_version.enforce_int_power_levels of
                    true ->
                        lists:foreach(
                          fun(Field) ->
                                  case NewPL of
                                      #{Field := V} when is_integer(V) -> ok;
                                      #{Field := _V} -> error(not_allowed);
                                      _ -> ok
                                  end
                          end,
                          [<<"users_default">>, <<"events_default">>, <<"state_default">>,
                           <<"ban">>, <<"redact">>, <<"kick">>, <<"invite">>]),
                        lists:foreach(
                          fun(Key) ->
                                  NewMap = maps:get(Key, NewPL, #{}),
                                  maps:fold(
                                    fun(_Field, V, _) ->
                                            if
                                                is_integer(V) -> ok;
                                                true -> error(not_allowed)
                                            end
                                    end, [], NewMap)
                          end,
                          CheckKeys);
                    false ->
                        ok
                end,
                maps:fold(
                  fun(K, _V, _) ->
                          case check_user_id(K) of
                              true -> ok;
                              false -> error(not_allowed)
                          end
                  end, ok, Users),
                StateKey = Event#event.state_key,
                case StateMap of
                    #{{?ROOM_POWER_LEVELS, StateKey} :=
                          #event{json = #{<<"content">> := OldPL}}} ->
                        UserLevel = get_user_power_level(Event#event.sender, StateMap, Data),
                        lists:foreach(
                          fun(Field) ->
                                  case check_event_auth_power_levels_aux(
                                         Field, OldPL, NewPL, UserLevel, none) of
                                      true -> ok;
                                      false -> error(not_allowed)
                                  end
                          end,
                          [<<"users_default">>, <<"events_default">>, <<"state_default">>,
                           <<"ban">>, <<"redact">>, <<"kick">>, <<"invite">>]),
                        lists:foreach(
                          fun(Key) ->
                                  OldMap = maps:get(Key, OldPL, #{}),
                                  NewMap = maps:get(Key, NewPL, #{}),
                                  UserID =
                                      case Key of
                                          <<"users">> ->
                                              {some, Event#event.sender};
                                          _ -> none
                                      end,
                                  maps:fold(
                                    fun(Field, _, _) ->
                                            case check_event_auth_power_levels_aux(
                                                   Field, OldMap, NewMap, UserLevel, UserID) of
                                                true -> ok;
                                                false -> error(not_allowed)
                                            end
                                    end, [], maps:merge(OldMap, NewMap))
                          end,
                          CheckKeys),
                        true;
                    _ ->
                        true
                end;
            _ ->
                false
        end
    catch
        error:not_allowed ->
            false
    end.

check_event_auth_power_levels_aux(Field, OldDict, NewDict, UserLevel, UserID) ->
    UserLevel2 =
        case UserID of
            none -> UserLevel;
            {some, Field} -> UserLevel;
            {some, _} -> UserLevel - 1
        end,
    case {maps:find(Field, OldDict), maps:find(Field, NewDict)} of
        {error, error} -> true;
        {error, {ok, S}} ->
            get_int(S) =< UserLevel;
        {{ok, S}, error} ->
            get_int(S) =< UserLevel2;
        {{ok, S1}, {ok, S2}} ->
            OldLevel = get_int(S1),
            NewLevel = get_int(S2),
            if
                OldLevel == NewLevel -> true;
                true ->
                    OldLevel =< UserLevel2 andalso NewLevel =< UserLevel
            end
    end.

check_user_id(S) ->
    case S of
        <<$@, Parts/binary>> ->
            case binary:split(Parts, <<":">>) of
                [_, _] -> true;
                _ -> false
            end;
        _ ->
            false
    end.

parse_user_id(Str) ->
    case Str of
        <<$@, Parts/binary>> ->
            case binary:split(Parts, <<":">>) of
                [U, S] -> {ok, U, S};
                _ -> error
            end;
        _ ->
            error
    end.

get_int(I) when is_integer(I) -> I;
get_int(S) when is_binary(S) -> binary_to_integer(S).

fill_event(JSON, Data) ->
    Host = Data#data.host,
    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
    PrevEvents = lists:sublist(sets:to_list(Data#data.latest_events), ?MAX_PREV_EVENTS),
    Depth =
        lists:max(
          [0 | lists:map(
                 fun(EID) ->
                         (maps:get(EID, Data#data.events))#event.depth
                 end, PrevEvents)]),
    Depth2 = min(Depth + 1, ?MAX_DEPTH),
    ?DEBUG("fill ~p", [{PrevEvents, Data#data.events}]),
    StateMaps =
        lists:map(
          fun(EID) ->
                  case Data#data.events of
                      #{EID := #event{state_map = undefined}} ->
                          error({missed_state_map, EID});
                      #{EID := #event{state_map = SM}} ->
                          SM;
                      _ ->
                          error({missed_prev_event, EID})
                  end
          end, PrevEvents),
    StateMap = resolve_state_maps(StateMaps, Data),
    AuthEvents =
        lists:usort(
          lists:flatmap(
            fun(Key) ->
                    case StateMap of
                        #{Key := E} -> [E];
                        _ -> []
                    end
            end,
            compute_event_auth_keys(JSON, Data#data.room_version))),
    ?DEBUG("auth_events ~p", [{AuthEvents, compute_event_auth_keys(JSON, Data#data.room_version)}]),
    {JSON#{<<"auth_events">> => AuthEvents,
           <<"depth">> => Depth2,
           <<"origin">> => MatrixServer,
           <<"origin_server_ts">> => erlang:system_time(millisecond),
           <<"prev_events">> => PrevEvents,
           <<"room_id">> => Data#data.room_id},
     StateMap}.

add_event(JSON, Data) ->
    Host = Data#data.host,
    {Msg, StateMap} = fill_event(JSON, Data),
    CHash = mod_matrix_gw:content_hash(Msg),
    Msg2 =
        Msg#{<<"hashes">> =>
                 #{<<"sha256">> =>
                       mod_matrix_gw:base64_encode(CHash)}},
    Msg3 = mod_matrix_gw:sign_event(Host, Msg2, Data#data.room_version),
    %% an event every other server would refuse is
    %% not made (the spec's 65536 bytes of PDU, 255 of its ids and type)
    check_size(Msg3),
    Event = json_to_event(Msg3, Data#data.room_version),
    StateMap2 =
        case Event#event.state_key of
            undefined ->
                StateMap;
            _ ->
                StateMap#{{Event#event.type, Event#event.state_key} => Event#event.id}
        end,
    Event2 = Event#event{state_map = StateMap2},
    ?DEBUG("add_event ~p~n", [Event2]),
    case check_event_auth(Event2, Data) of
        true ->
            %%TODO: soft fail
            {store_event(Event2, Data), Event2};
        false ->
            error({event_auth_error, Event2#event.id})
    end.


%% Raises error({too_large, What}) for a PDU over the spec's size limits.
check_size(PDU) ->
    Long = [K || K <- [<<"type">>, <<"state_key">>, <<"sender">>, <<"room_id">>],
                 V <- [maps:get(K, PDU, <<>>)], is_binary(V), byte_size(V) > ?MAX_PDU_ID],
    Size = iolist_size(mod_matrix_gw:encode_canonical_json(PDU)),
    if Long /= [] -> error({too_large, hd(Long)});
       Size > ?MAX_PDU -> error({too_large, Size});
       true -> ok
    end.

store_event(Event, Data) ->
    %% TODO
    Events = Data#data.events,
    case maps:find(Event#event.id, Events) of
        {ok, #event{state_map = undefined}} when Event#event.state_map /= undefined ->
            %% kept across a restart (mod_matrix_gw_store)
            persist(Event, Data#data.latest_events,
                    Data#data{events = Events#{Event#event.id => Event}});
        {ok, _} ->
            Data;
        error ->
            ?DEBUG("store ~p~n", [Event#event.id]),
            %% the room's bookkeeping of XMPP ids, reactions and
            %% history first, then the notifications, then the store
            Data2 = notify_event(Event, track_event(Event, Data)),
            {LatestEvents, NonLatestEvents} =
                case Event of
                    #event{state_map = undefined} ->
                        {Data2#data.latest_events, Data2#data.nonlatest_events};
                    _ ->
                        SeenEvents = Event#event.prev_events ++ Event#event.auth_events,
                        LatestEs =
                            lists:foldl(fun(E, Acc) -> sets:del_element(E, Acc) end, Data2#data.latest_events,
                                        SeenEvents),
                        NonLatestEs =
                            lists:foldl(fun(E, Acc) -> sets:add_element(E, Acc) end, Data2#data.nonlatest_events,
                                        SeenEvents),
                        LatestEs2 =
                            case maps:is_key(Event#event.id, NonLatestEs) of
                                true ->
                                    LatestEs;
                                false ->
                                    LatestEs#{Event#event.id => []}
                            end,
                        %%?DEBUG("latest ~p~n", [{LatestEvents2, NonLatestEvents}]),
                        {LatestEs2, NonLatestEs}
                end,
            EventQueue =
                treap:insert(
                  Event#event.id,
                  {erlang:monotonic_time(micro_seconds),
                   erlang:unique_integer([monotonic])},
                  [], Data2#data.event_queue),
            Data3 = persist(Event, Data2#data.latest_events,
                            Data2#data{events = Events#{Event#event.id => Event},
                                       latest_events = LatestEvents,
                                       nonlatest_events = NonLatestEvents,
                                       event_queue = EventQueue}),
            %% a redaction prunes its target (mod_matrix_gw_event)
            prune_redacted(Event, Data3)
    end.

simple_toposort(Events) ->
    {Res, _Used} =
        lists:foldl(
          fun(E, {_Res, Used} = Acc) ->
                  EventID = E#event.id,
                  case maps:is_key(EventID, Used) of
                      false ->
                          simple_toposort_dfs(EventID, Acc, Events);
                      true ->
                          Acc
                  end
          end, {[], #{}}, maps:values(Events)),
    lists:reverse(Res).

simple_toposort_dfs(EventID, {Res, Used}, Events) ->
    case maps:find(EventID, Events) of
        error ->
            %error({unknown_event, EventID});
            {Res, Used};
        {ok, Event} ->
            Used2 = Used#{EventID => gray},
            {Res8, Used8} =
                lists:foldl(
                  fun(ID, {_Res3, Used3} = Acc) ->
                          case maps:get(ID, Used3, white) of
                              white ->
                                  simple_toposort_dfs(ID, Acc, Events);
                              gray ->
                                  error(loop_in_auth_chain);
                              black ->
                                  Acc
                          end
                  end, {Res, Used2}, Event#event.auth_events),
            Used9 = Used8#{EventID => black},
            Res9 = [EventID | Res8],
            {Res9, Used9}
    end.

check_event_sig_and_hash(Host, Event) ->
    case check_event_signature(Host, Event) of
        true ->
            case check_event_content_hash(Event) of
                true ->
                    {ok, Event};
                false ->
                    ?DEBUG("mismatched content hash: ~p", [Event#event.id]),
                    PrunedJSON = mod_matrix_gw:prune_event(
                                   Event#event.json, Event#event.room_version),
                    {ok, Event#event{json = PrunedJSON}}
            end;
        false ->
            {error, {invalid_signature, Event#event.id}}
    end.

get_room_version(Pid) ->
    gen_statem:call(Pid, get_room_version).

partition_missed_events(Pid, EventIDs) ->
    gen_statem:call(Pid, {partition_missed_events, EventIDs}).

partition_events_with_statemap(Pid, EventIDs) ->
    gen_statem:call(Pid, {partition_events_with_statemap, EventIDs}).

get_latest_events(Pid) ->
    gen_statem:call(Pid, get_latest_events).

check_event_signature(Host, Event) ->
    PrunedEvent = mod_matrix_gw:prune_event(Event#event.json,
                                            Event#event.room_version),
    mod_matrix_gw_s2s:check_signature(Host, PrunedEvent,
                                      Event#event.room_version).

find_event(Pid, EventID) ->
    gen_statem:call(Pid, {find_event, EventID}).

resolve_auth_store_event(Pid, Event) ->
    gen_statem:call(Pid, {resolve_auth_store_event, Event}).

%% the event a /send carries, checked for soft failure too
resolve_auth_store_new_event(Pid, Event) ->
    gen_statem:call(Pid, {resolve_auth_store_event, Event, true}).

process_pdu(Host, Origin, #{<<"room_id">> := RoomID} = PDU) when is_binary(RoomID) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            RoomVersion = get_room_version(Pid),
            %% a PDU of the wrong shape is refused
            %% here, never handed to the room's process
            try json_to_event(PDU, RoomVersion) of
                Event0 ->
                    %% the sender's server is the
                    %% origin's or one in the room before any key of it is
                    %% fetched
                    case sender_allowed(Origin, Pid, Event0) of
                        true ->
                            process_pdu(Host, Origin, Pid, RoomID, RoomVersion, Event0);
                        false ->
                            ?INFO_MSG("a PDU from ~s of a sender of a server not in ~s, ~s: refused",
                                      [Origin, RoomID, Event0#event.sender]),
                            {error, <<"sender not of the origin nor of a server in the room">>}
                    end
            catch
                throw:Why ->
                    ?DEBUG("malformed PDU from ~s: ~p", [Origin, Why]),
                    {error, <<"malformed PDU">>}
            end;
        {error, not_found} ->
            {error, <<"Room doesn't exist">>}
    end;
process_pdu(_Host, _Origin, _PDU) ->
    {error, <<"malformed PDU">>}.

process_pdu(Host, Origin, Pid, RoomID, RoomVersion, Event0) ->
    %% the content hash checked too (the
    %% spec's checks on receipt of a PDU, step 3): content that does
    %% not match it is replaced by the redacted event's
    case check_event_sig_and_hash(Host, Event0) of
        {ok, Event} ->
            ?DEBUG("process pdu: ~p~n", [Event#event.id]),
            {SeenEvents, MissedEvents} =
                 partition_missed_events(Pid, Event#event.prev_events),
            ?DEBUG("seen/missed: ~p~n", [{SeenEvents, MissedEvents}]),
            case MissedEvents of
                [] ->
                    ok;
                _ ->
                    LatestEvents = get_latest_events(Pid),
                    EarliestEvents =
                        lists:foldl(
                          fun(E, Acc) ->
                                  Acc#{E => []}
                          end, LatestEvents, SeenEvents),
                    ?DEBUG("earliest ~p~n", [EarliestEvents]),
                    MissingEventsQuery =
                        #{<<"earliest_events">> => maps:keys(EarliestEvents),
                          <<"latest_events">> => [Event#event.id],
                          <<"limit">> => 10},
                    MissingEventsRes =
                        mod_matrix_gw:send_request(
                          Host, post, Origin,
                          [<<"_matrix">>, <<"federation">>, <<"v1">>,
                           <<"get_missing_events">>, RoomID],
                          [],
                          MissingEventsQuery,
                          [{timeout, 60000}],
                          [{sync, true},
                           {body_format, binary}]),
                    ?DEBUG("missing res ~p~n", [MissingEventsRes]),
                    process_missing_events_res(Host, Origin, Pid, RoomID, RoomVersion,
                                               MissingEventsRes),
                    ok
            end,
            resolve_auth_store_new_event(Pid, Event),
            {ok, Event#event.id};
        {error, _} ->
            {error, <<"Signature check failed">>}
    end.

%% Whether a PDU Origin sent may be checked at all:
%% its sender is of the origin, or of a server joined to the room, or it is
%% the sender's own join or knock (a server new to the room, its join relayed
%% by the server it joined through). Anything else would make the gateway
%% fetch the keys of whatever server the PDU names.
sender_allowed(Origin, Pid, #event{sender = Sender, type = Type, state_key = StateKey, json = JSON}) ->
    case catch mod_matrix_gw:get_id_domain_exn(Sender) of
        Origin ->
            true;
        Server when is_binary(Server) ->
            case {Type, StateKey, maps:get(<<"content">>, JSON, #{})} of
                {?ROOM_MEMBER, Sender, #{<<"membership">> := M}} when M == <<"join">>; M == <<"knock">> ->
                    true;
                _ ->
                    (catch gen_statem:call(Pid, {is_server_joined, Server})) == true
            end;
        _ ->
            false
    end.

process_missing_events_res(Host, Origin, Pid, RoomID, RoomVersion,
                           {ok, {{_, 200, _}, _Headers, Body}}) ->
    try
        case misc:json_decode(Body) of
            #{<<"events">> := JSONEvents} when is_list(JSONEvents) ->
                process_missing_events(Host, Origin, Pid, RoomID, RoomVersion, JSONEvents)
        end
    catch
        Class:Reason:ST ->
            ?DEBUG("failed process_missing_events_res: ~p", [failure(Class, Reason, ST)]),
            ok
    end;
process_missing_events_res(_Host, _Origin, _Pid, _RoomID, _RoomVersion, _) ->
    ok.

%% Each returned event on its own: one of the
%% wrong shape, of another room (S8: it would become an extremity no other
%% server can fetch) or failing its signature is skipped, not the rest; the
%% ones kept are authorised in the room's process as /send's are (S3,
%% do_resolve_auth_store_event/3).
process_missing_events(Host, Origin, Pid, RoomID, RoomVersion, JSONEvents) ->
    Events = lists:flatmap(
               fun(J) ->
                       try json_to_event(J, RoomVersion) of
                           #event{room_id = RoomID} = E ->
                               [E];
                           #event{id = EID, room_id = Other} ->
                               ?INFO_MSG("a missing event ~s from ~s names the room ~p, not ~s: dropped",
                                         [EID, Origin, Other, RoomID]),
                               []
                       catch
                           throw:Why ->
                               ?DEBUG("a malformed missing event from ~s: ~p", [Origin, Why]),
                               []
                       end
               end, lists:sublist(JSONEvents, 20)),
    SortedEvents = lists:keysort(#event.depth, Events),
    ?DEBUG("sevents ~p~n", [[E#event.id || E <- SortedEvents]]),
    lists:foreach(
      fun(Event) ->
              case check_event_sig_and_hash(Host, Event) of
                  {ok, _} ->
                      ShouldProcess =
                          case find_event(Pid, Event#event.id) of
                              {ok, #event{state_map = undefined}} ->
                                  true;
                              {ok, _} ->
                                  false;
                              error ->
                                  true
                          end,
                      case ShouldProcess of
                          true ->
                              try
                                  fetch_prev_statemaps(Host, Origin, Pid,
                                                       RoomID, RoomVersion, Event),
                                  resolve_auth_store_event(Pid, Event)
                              catch
                                  Class:Reason ->
                                      ?INFO_MSG("a missing event ~s from ~s not taken: ~p",
                                                [Event#event.id, Origin, {Class, Reason}])
                              end,
                              ok;
                          false ->
                              ok
                      end;
                  {error, Reason} ->
                      ?INFO_MSG("a missing event ~s from ~s not taken: ~p",
                                [Event#event.id, Origin, Reason])
              end
      end, SortedEvents),
    ok.

fetch_prev_statemaps(Host, Origin, Pid, RoomID, RoomVersion, Event) ->
    ?DEBUG("fetch_prev_statemaps ~p~n", [Event#event.id]),
    {SeenEvents, MissedEvents} =
        partition_events_with_statemap(Pid, Event#event.prev_events),
    ?DEBUG("s/m ~p~n", [{SeenEvents, MissedEvents}]),
    lists:foreach(
      fun(MissedEventID) ->
              case request_event(Host, Origin, Pid, RoomID, RoomVersion, MissedEventID) of
                  {ok, MissedEvent} ->
                      case request_room_state(Host, Origin, Pid, RoomID, RoomVersion, MissedEvent) of
                          {ok, AuthChain, State} ->
                              auth_and_store_external_events(Pid, AuthChain ++ State),
                              StateMap =
                                  lists:foldl(
                                    fun(E, Acc) ->
                                            Acc#{{E#event.type, E#event.state_key} => E#event.id}
                                    end, #{}, State),
                              auth_and_store_external_events(
                                Pid, [MissedEvent#event{state_map = StateMap}]),
                              ok;
                          {error, Reason} ->
                              ?INFO_MSG("failed request_room_state: ~p", [{RoomID, Event#event.id, Reason}]),
                              ok
                      end;
                  {error, Error} ->
                      error(Error)
              end
      end, MissedEvents).

request_room_state(Host, Origin, _Pid, RoomID, RoomVersion, Event) ->
    Res =
        mod_matrix_gw:send_request(
          Host, get, Origin,
          [<<"_matrix">>, <<"federation">>,
           <<"v1">>, <<"state">>,
           RoomID],
          [{<<"event_id">>, Event#event.id}],
          none,
          [{connect_timeout, 5000},
           {timeout, 60000}],
          [{sync, true},
           {body_format, binary}]),
    case Res of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            try
                case misc:json_decode(Body) of
                    #{<<"auth_chain">> := JSONAuthChain,
                      <<"pdus">> := JSONState} = _JSON when is_list(JSONAuthChain),
                                                            is_list(JSONState) ->
                        AuthChain =
                            lists:map(fun(J) -> json_to_event(J, RoomVersion) end,
                                      JSONAuthChain),
                        State =
                            lists:map(fun(J) -> json_to_event(J, RoomVersion) end,
                                      JSONState),
                        lists:foreach(
                          fun(E) ->
                                  case check_event_sig_and_hash(Host, E) of
                                      {ok, _} ->
                                          case E#event.room_id of
                                              RoomID ->
                                                  case E#event.state_key of
                                                      undefined ->
                                                          error({missed_state_key, E#event.id});
                                                      _ ->
                                                          ok
                                                  end;
                                              RoomID2 ->
                                                  error({mismatched_room_id, E#event.id, RoomID, RoomID2})
                                          end;
                                      {error, Error} -> error(Error)
                                  end
                          end, AuthChain ++ State),
                        ?DEBUG("req state ~p~n",
                               [{[E#event.id || E <- AuthChain],
                                 [E#event.id || E <- State]}]),
                        {ok, AuthChain, State}
                end
            catch
                Class:Reason:ST ->
                    ?INFO_MSG("failed request_room_state: ~p", [failure(Class, Reason, ST)]),
                    {error, Reason}
            end;
        {ok, {{_, _Status, Reason}, _Headers, _Body}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

request_event(Host, Origin, _Pid, RoomID, RoomVersion, EventID) ->
    Res =
        mod_matrix_gw:send_request(
          Host, get, Origin,
          [<<"_matrix">>, <<"federation">>,
           <<"v1">>, <<"event">>,
           EventID],
          [],
          none,
          [{timeout, 5000}],
          [{sync, true},
           {body_format, binary}]),
    case Res of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            try
                case misc:json_decode(Body) of
                    #{<<"pdus">> := [PDU]} ->
                        Event = json_to_event(PDU, RoomVersion),
                        case check_event_sig_and_hash(Host, Event) of
                            {ok, _} ->
                                case Event#event.room_id of
                                    RoomID ->
                                        ok;
                                    RoomID2 ->
                                        error({mismatched_room_id, Event#event.id, RoomID, RoomID2})
                                end;
                            {error, Error} -> error(Error)
                        end,
                        {ok, Event}
                end
            catch
                Class:Reason:ST ->
                    ?INFO_MSG("failed request_event: ~p", [failure(Class, Reason, ST)]),
                    {error, Reason}
            end;
        {ok, {{_, _Status, Reason}, _Headers, _Body}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

get_event_prev_state_map(Event, Data) ->
    StateMaps =
        lists:map(
          fun(EID) ->
                  case Data#data.events of
                      #{EID := #event{state_map = undefined}} ->
                          error({missed_state_map, EID});
                      #{EID := #event{state_map = SM}} ->
                          SM;
                      _ ->
                          error({missed_prev_event, EID})
                  end
          end, Event#event.prev_events),
    resolve_state_maps(StateMaps, Data).

do_resolve_auth_store_event(Event, SoftFailCheck, Data) ->
    StateMap = get_event_prev_state_map(Event, Data),
    StateMap2 =
        case Event#event.state_key of
            undefined ->
                StateMap;
            _ ->
                StateMap#{{Event#event.type, Event#event.state_key} => Event#event.id}
        end,
    Event2 = Event#event{state_map = StateMap2},
    case check_event_auth(Event2, Data) andalso auth_before(Event2, StateMap, Data) of
        true ->
            %% soft failure (server-server API, "Soft failure"):
            %% an event that passes against its own auth events and state but
            %% not against the room's current state is kept in the graph but
            %% neither shown nor made a forward extremity;
            %% whatever path brought a remote event (soft_failed/2 passes
            %% the gateway's own)
            _ = SoftFailCheck,
            case soft_failed(Event2, Data) of
                true ->
                    ?INFO_MSG("soft-failed event ~s in ~s from ~s",
                              [Event2#event.id, Data#data.room_id, Event2#event.sender]),
                    Data#data{events = (Data#data.events)#{Event2#event.id => Event2}};
                false ->
                    store_event(Event2, Data)
            end;
        false ->
            error({event_auth_error, Event2#event.id})
    end.

%% The spec's checks on receipt, step 5: a remote
%% event passes the auth rules against the state before it (StateMap, the
%% resolved state of its prev events, type and state key to event id), not
%% only against the auth events its sender chose. The gateway's own events
%% are its own room's doing.
auth_before(#event{type = ?ROOM_CREATE}, _StateMap, _Data) ->
    true;
auth_before(#event{sender = Sender} = Event, StateMap, Data) ->
    Local = mod_matrix_gw_opt:matrix_domain(Data#data.host),
    case catch mod_matrix_gw:get_id_domain_exn(Sender) of
        Local ->
            true;
        _ ->
            case catch check_event_auth(Event, auth_state(Event, StateMap, Data), Data) of
                true ->
                    true;
                _ ->
                    ?INFO_MSG("event ~s in ~s from ~s refused: not allowed by the state before it",
                              [Event#event.id, Data#data.room_id, Sender]),
                    false
            end
    end.

%% The events of State (type and state key to event id) the auth rules
%% read for Event: its auth keys and the create event.
auth_state(#event{json = JSON}, State, Data) ->
    lists:foldl(
      fun(Key, Acc) ->
              case maps:find(Key, State) of
                  {ok, #event{} = E} ->
                      Acc#{Key => E};
                  {ok, EID} ->
                      case maps:find(EID, Data#data.events) of
                          {ok, E} -> Acc#{Key => E};
                          error -> Acc
                      end;
                  error ->
                      Acc
              end
      end, #{},
      [{?ROOM_CREATE, <<"">>} | compute_event_auth_keys(JSON, Data#data.room_version)]).

resolve_state_maps([], _Data) ->
    #{};
resolve_state_maps([StateMap], _Data) ->
    StateMap;
resolve_state_maps(StateMaps, Data) ->
    {Unconflicted, Conflicted0} = calculate_conflict(StateMaps),
    Conflicted1 = lists:append(maps:values(Conflicted0)),
    Conflicted =
        case (Data#data.room_version)#room_version.hydra of
            false ->
                Conflicted1;
            true ->
                calculate_conflicted_subgraph(Conflicted1, Data)
        end,
    ?DEBUG("confl ~p~n", [{Unconflicted, Conflicted}]),
    case Conflicted of
        [] ->
            Unconflicted;
        _ ->
            AuthDiff = calculate_auth_diff(StateMaps, Data),
            ?DEBUG("auth diff ~p~n", [AuthDiff]),
            FullConflictedSet =
                maps:from_list([{E, []} || E <- AuthDiff ++ Conflicted]),
            ?DEBUG("fcs ~p~n", [FullConflictedSet]),
            %% TODO: test
            PowerEvents =
                lists:filter(
                  fun(EventID) ->
                          Event = maps:get(EventID, Data#data.events),
                          is_power_event(Event)
                  end, maps:keys(FullConflictedSet)),
            SortedPowerEvents = lexicographic_toposort(PowerEvents, FullConflictedSet, Data),
            ?DEBUG("spe ~p~n", [SortedPowerEvents]),
            StateMap =
                case (Data#data.room_version)#room_version.hydra of
                    false ->
                        iterative_auth_checks(SortedPowerEvents, Unconflicted, Data);
                    true ->
                        maps:merge(
                          Unconflicted,
                          iterative_auth_checks(SortedPowerEvents, #{}, Data))
                end,
            PowerEventsSet = maps:from_list([{E, []} || E <- SortedPowerEvents]),
            OtherEvents = lists:filter(fun(E) -> not maps:is_key(E, PowerEventsSet) end,
                                       maps:keys(FullConflictedSet)),
            PLID = maps:get({?ROOM_POWER_LEVELS, <<"">>}, StateMap, undefined),
            SortedOtherEvents = mainline_sort(OtherEvents, PLID, Data),
            ?DEBUG("mainline ~p~n", [SortedOtherEvents]),
            StateMap2 = iterative_auth_checks(SortedOtherEvents, StateMap, Data),
            Resolved = maps:merge(StateMap2, Unconflicted),
            ?DEBUG("resolved ~p~n", [Resolved]),
            Resolved
    end.

calculate_conflict(StateMaps) ->
    Keys = lists:usort(lists:flatmap(fun maps:keys/1, StateMaps)),
    lists:foldl(
      fun(Key, {Unconflicted, Conflicted}) ->
              EventIDs =
                  lists:usort(
                    lists:map(fun(StateMap) ->
                                      maps:find(Key, StateMap)
                              end, StateMaps)),
              case EventIDs of
                  [{ok, EventID}] ->
                      {Unconflicted#{Key => EventID}, Conflicted};
                  _ ->
                      EventIDs2 =
                          lists:flatmap(
                            fun(error) -> [];
                               ({ok, EventID}) -> [EventID]
                            end, EventIDs),
                      {Unconflicted, Conflicted#{Key => EventIDs2}}
              end
      end, {#{}, #{}}, Keys).

calculate_conflicted_subgraph([], _Data) ->
    [];
calculate_conflicted_subgraph(Events, Data) ->
    MinDepth =
        lists:min(
          [(maps:get(EID, Data#data.events))#event.depth || EID <- Events]),
    AuthEvents =
        lists:append(
          [(maps:get(EID, Data#data.events))#event.auth_events || EID <- Events]),
    Used0 =
        maps:from_list([{E, true} || E <- Events]),
    {Res, _Used} =
        lists:foldl(
          fun(EID, {_Res, Used} = Acc) ->
                  case maps:is_key(EID, Used) of
                      false ->
                          calculate_conflicted_subgraph_dfs(EID, Acc, MinDepth, Data);
                      true ->
                          Acc
                  end
          end, {Events, Used0}, AuthEvents),
    Res.

calculate_conflicted_subgraph_dfs(EventID, {Res, Used}, MinDepth, Data) ->
    case maps:find(EventID, Data#data.events) of
        error ->
            {Res, Used};
        {ok, Event} when Event#event.depth < MinDepth ->
            {Res, Used};
        {ok, Event} ->
            Used2 = Used#{EventID => gray},
            {Res8, Used8, Reachable} =
                lists:foldl(
                  fun(_ID, {_Res3, _Used3, true} = Acc) ->
                          Acc;
                     (ID, {Res3, Used3, false}) ->
                          {Res4, Used4} =
                              case maps:get(ID, Used3, white) of
                                  white ->
                                      calculate_conflicted_subgraph_dfs(ID, {Res3, Used3}, MinDepth, Data);
                                  _ ->
                                      {Res3, Used3}
                              end,
                          case maps:get(ID, Used4, white) of
                              gray ->
                                  error(loop_in_auth_chain);
                              true ->
                                  {Res4, Used4, true};
                              _ ->
                                  {Res4, Used4, false}
                          end
                  end, {Res, Used2, false}, Event#event.auth_events),
            Used9 = Used8#{EventID => Reachable},
            Res9 =
                case Reachable of
                    true ->
                        [EventID | Res8];
                    false ->
                        Res8
                end,
            {Res9, Used9}
    end.


%% TODO: not optimal
calculate_auth_diff(StateMaps, Data) ->
    N = length(StateMaps),
    Queue =
        lists:foldl(
          fun({K, StateMap}, Q) ->
                  maps:fold(
                    fun(_, EID, Q2) ->
                            Depth = (maps:get(EID, Data#data.events))#event.depth,
                            Set =
                                case gb_trees:lookup({Depth, EID}, Q2) of
                                    none ->
                                        1 bsl N - 1;
                                    {value, S} ->
                                        S
                                end,
                            Set2 = Set band bnot (1 bsl K),
                            gb_trees:enter({Depth, EID}, Set2, Q2)
                    end, Q, StateMap)
          end, gb_trees:empty(),
          lists:zip(lists:seq(0, N - 1), StateMaps)),
    Count = lists:sum(gb_trees:values(Queue)),
    calculate_auth_diff_bfs(Queue, Count, [], Data).

calculate_auth_diff_bfs(_Queue, 0, Res, _Data) ->
    Res;
calculate_auth_diff_bfs(Queue, Count, Res, Data) ->
    %?DEBUG("authdiff bfs ~p~n", [{gb_trees:to_list(Queue), Count, Res}]),
    case gb_trees:is_empty(Queue) of
        true ->
            error(internal_error);
        false ->
            {{_, EventID}, Set, Queue2} = gb_trees:take_largest(Queue),
            Res2 = case Set of
                       0 -> Res;
                       _ -> [EventID | Res]
                   end,
            Event = maps:get(EventID, Data#data.events),
            calculate_auth_diff_bfs2(Event#event.auth_events, Set, Queue2, Count - Set, Res2, Data)
    end.

calculate_auth_diff_bfs2([], _Set, Queue, Count, Res, Data) ->
    calculate_auth_diff_bfs(Queue, Count, Res, Data);
calculate_auth_diff_bfs2([EID | Events], Set, Queue, Count, Res, Data) ->
    Event = maps:get(EID, Data#data.events),
    case gb_trees:lookup({Event#event.depth, EID}, Queue) of
        none ->
            Queue2 = gb_trees:insert({Event#event.depth, EID}, Set, Queue),
            calculate_auth_diff_bfs2(Events, Set, Queue2, Count + Set, Res, Data);
        {value, Set2} ->
            Set3 = Set band Set2,
            Queue2 = gb_trees:enter({Event#event.depth, EID}, Set3, Queue),
            calculate_auth_diff_bfs2(Events, Set, Queue2, Count - Set2 + Set3, Res, Data)
    end.

is_power_event(#event{type = ?ROOM_POWER_LEVELS, state_key = <<"">>}) ->
    true;
is_power_event(#event{type = ?ROOM_JOIN_RULES, state_key = <<"">>}) ->
    true;
is_power_event(#event{type = ?ROOM_MEMBER, state_key = StateKey, sender = Sender,
                      json = #{<<"content">> := #{<<"membership">> := <<"leave">>}}}) ->
    StateKey /= Sender;
is_power_event(#event{type = ?ROOM_MEMBER, state_key = StateKey, sender = Sender,
                      json = #{<<"content">> := #{<<"membership">> := <<"ban">>}}}) ->
    StateKey /= Sender;
is_power_event(_) ->
    false.

lexicographic_toposort(EventIDs, EventSet, Data) ->
    {Used, Rev} =
        lists:foldl(
          fun(EventID, {Used, Rev} = Acc) ->
                  case maps:is_key(EventID, EventSet) of
                      true ->
                          case maps:is_key(EventID, Used) of
                              false ->
                                  lexicographic_toposort_prepare(EventID, Used, Rev, EventSet, Data);
                              true ->
                                  Acc
                          end;
                      false ->
                          Acc
                  end
          end, {#{}, #{}}, EventIDs),
    ?DEBUG("rev ~p~n", [Rev]),
    OutgoingCnt =
        maps:fold(
          fun(EventID, _, Acc) ->
                  lists:foldl(
                    fun(EID, Acc2) ->
                            case maps:is_key(EID, Acc2) of
                                true ->
                                    C = maps:get(EID, Acc2),
                                    maps:put(EID, C + 1, Acc2);
                                false ->
                                    Acc2
                            end
                    end, Acc, maps:get(EventID, Rev, []))
          end, maps:map(fun(_, _) -> 0 end, Used), Used),
    Current =
        maps:fold(
          fun(EventID, 0, Acc) ->
                  Event = maps:get(EventID, Data#data.events),
                  PowerLevel = get_sender_power_level(EventID, Data),
                  gb_trees:enter({-PowerLevel, Event#event.origin_server_ts, EventID}, [], Acc);
             (_, _, Acc) ->
                  Acc
          end, gb_trees:empty(), OutgoingCnt),
    OutgoingCnt2 = maps:filter(fun(_, 0) -> false; (_, _) -> true end, OutgoingCnt),
    lexicographic_toposort_loop(Current, OutgoingCnt2, Rev, [], Data).

lexicographic_toposort_prepare(EventID, Used, Rev, EventSet, Data) ->
    Event = maps:get(EventID, Data#data.events),
    Used2 = Used#{EventID => []},
    lists:foldl(
      fun(EID, {Used3, Rev3} = Acc) ->
              case maps:is_key(EID, EventSet) of
                  true ->
                      Rev4 = maps:update_with(
                               EID,
                               fun(Es) -> [EventID | Es] end, [EventID], Rev3),
                      case maps:is_key(EID, Used3) of
                          false ->
                              lexicographic_toposort_prepare(EID, Used3, Rev4, EventSet, Data);
                          true ->
                              {Used3, Rev4}
                      end;
                  false ->
                      Acc
              end
      end, {Used2, Rev}, Event#event.auth_events).

lexicographic_toposort_loop(Current, OutgoingCnt, Rev, Res, Data) ->
    %?DEBUG("toposort ~p", [{gb_trees:to_list(Current), OutgoingCnt, Res}]),
    case gb_trees:is_empty(Current) of
        true ->
            case maps:size(OutgoingCnt) of
                0 ->
                    lists:reverse(Res);
                _ ->
                    error(loop_in_auth_chain)
            end;
        false ->
            {{_, _, EventID}, _, Current2} = gb_trees:take_smallest(Current),
            {OutgoingCnt2, Current3} =
                lists:foldl(
                  fun(EID, {OutCnt, Cur} = Acc) ->
                          case maps:is_key(EID, OutCnt) of
                              true ->
                                  C = maps:get(EID, OutCnt) - 1,
                                  case C of
                                      0 ->
                                          E = maps:get(EID, Data#data.events),
                                          PowerLevel = get_sender_power_level(EID, Data),
                                          Cur2 = gb_trees:enter({-PowerLevel, E#event.origin_server_ts, EID}, [], Cur),
                                          {maps:remove(EID, OutCnt), Cur2};
                                      _ ->
                                          {maps:put(EID, C, OutCnt), Cur}
                                  end;
                              false ->
                                  Acc
                          end
                  end, {OutgoingCnt, Current2}, maps:get(EventID, Rev, [])),
            lexicographic_toposort_loop(Current3, OutgoingCnt2, Rev, [EventID | Res], Data)
    end.

get_sender_power_level(EventID, Data) ->
    RoomVersion = Data#data.room_version,
    Event = maps:get(EventID, Data#data.events),
    PowerEventID = find_power_level_event(EventID, Data),
    PowerEvent =
        case PowerEventID of
            undefined -> undefined;
            _ -> maps:get(PowerEventID, Data#data.events)
        end,
    Sender = Event#event.sender,
    IsCreator = is_creator(EventID, Sender, Data),
    case {RoomVersion#room_version.hydra,
          IsCreator,
          PowerEvent} of
        {true, true, _} ->
            ?CREATOR_PL;
        {_, true, undefined} ->
            100;
        {_, false, undefined} ->
            0;
        {_, _,
         #event{json = #{<<"content">> := #{<<"users">> := #{Sender := Level}}}}} ->
            get_int(Level);
        {_, _,
         #event{json = #{<<"content">> := #{<<"users_default">> := Level}}}} ->
            get_int(Level);
        _ ->
            0
    end.

iterative_auth_checks(Events, StateMap, Data) ->
    lists:foldl(
      fun(EventID, StateMap2) ->
              Event = maps:get(EventID, Data#data.events),
              StateMap3 =
                  lists:foldl(
                    fun(EID, SM) ->
                            E = maps:get(EID, Data#data.events),
                            case maps:is_key({E#event.type, E#event.state_key}, SM) of
                                true ->
                                    SM;
                                false ->
                                    SM#{{E#event.type, E#event.state_key} => E#event.id}
                            end
                    end, StateMap2, Event#event.auth_events),
              %% TODO: not optimal
              StateMap4 =
                  maps:map(fun(_, EID) -> maps:get(EID, Data#data.events) end, StateMap3),
              case check_event_auth(Event, StateMap4, Data) of
                  true ->
                      StateMap2#{{Event#event.type, Event#event.state_key} => EventID};
                  false ->
                      StateMap2
              end
      end, StateMap, Events).

mainline_sort(OtherEvents, PLID, Data) ->
    IdxMap = mainline_sort_init(PLID, -1, #{}, Data),
    {OtherEvents2, _} =
        lists:foldl(
          fun(EventID, {Events, IMap}) ->
                  Event = maps:get(EventID, Data#data.events),
                  {Idx, IMap2} = mainline_sort_find(EventID, IMap, Data),
                  {[{Idx, Event#event.origin_server_ts, EventID} | Events], IMap2}
          end, {[], IdxMap}, OtherEvents),
    lists:map(fun({_, _, EID}) -> EID end, lists:sort(OtherEvents2)).

mainline_sort_init(undefined, _Idx, IdxMap, _Data) ->
    IdxMap;
mainline_sort_init(PLID, Idx, IdxMap, Data) when is_binary(PLID) ->
    IdxMap2 = maps:put(PLID, Idx, IdxMap),
    PLID2 = find_power_level_event(PLID, Data),
    mainline_sort_init(PLID2, Idx - 1, IdxMap2, Data).

mainline_sort_find(undefined, IdxMap, _Data) ->
    {0, IdxMap};
mainline_sort_find(EventID, IdxMap, Data) ->
    case maps:find(EventID, IdxMap) of
        {ok, Idx} -> {Idx, IdxMap};
        error ->
            PLID = find_power_level_event(EventID, Data),
            {Idx, IdxMap2} = mainline_sort_find(PLID, IdxMap, Data),
            IdxMap3 = maps:put(EventID, Idx, IdxMap2),
            {Idx, IdxMap3}
    end.

find_power_level_event(EventID, Data) ->
    Event = maps:get(EventID, Data#data.events),
    lists:foldl(
      fun(EID, undefined) ->
              E = maps:get(EID, Data#data.events),
              case E of
                  #event{type = ?ROOM_POWER_LEVELS, state_key = <<"">>} -> EID;
                  _ -> undefined
              end;
         (_, PLID) ->
              PLID
      end, undefined, Event#event.auth_events).

find_create_event(EventID, Data) ->
    Event = maps:get(EventID, Data#data.events),
    lists:foldl(
      fun(EID, undefined) ->
              E = maps:get(EID, Data#data.events),
              case E of
                  #event{type = ?ROOM_CREATE, state_key = <<"">>} -> E;
                  _ -> undefined
              end;
         (_, Create) ->
              Create
      end, undefined, Event#event.auth_events).

is_creator(EventID, User, Data) ->
    case find_create_event(EventID, Data) of
        undefined ->
            false;
        CreateEvent ->
            RoomVersion = Data#data.room_version,
            case {RoomVersion#room_version.hydra,
                  RoomVersion#room_version.implicit_room_creator,
                  CreateEvent} of
                {false, false,
                 #event{type = ?ROOM_CREATE, state_key = <<"">>,
                        json = #{<<"content">> :=
                                     #{<<"creator">> := User}}}} ->
                    true;
                {false, true,
                 #event{type = ?ROOM_CREATE, state_key = <<"">>,
                        sender = User}} ->
                    true;
                {true, _,
                 #event{type = ?ROOM_CREATE, state_key = <<"">>,
                        sender = User}} ->
                    true;
                {true, _,
                 #event{
                    json = #{<<"content">> :=
                                 #{<<"additional_creators">> := Creators}}}}
                  when is_list(Creators) ->
                    lists:member(User, Creators);
                _ ->
                    false
            end
    end.


binary_to_room_version(<<"4">>) ->
    #room_version{id = <<"4">>,
                  enforce_key_validity = false,
                  special_case_aliases_auth = true,
                  strict_canonicaljson = false,
                  limit_notifications_power_levels = false,
                  knock_join_rule = false,
                  restricted_join_rule = false,
                  restricted_join_rule_fix = false,
                  knock_restricted_join_rule = false,
                  enforce_int_power_levels = false,
                  implicit_room_creator = false,
                  updated_redaction_rules = false,
                  hydra = false
                 };
binary_to_room_version(<<"5">>) ->
    #room_version{id = <<"5">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = true,
                  strict_canonicaljson = false,
                  limit_notifications_power_levels = false,
                  knock_join_rule = false,
                  restricted_join_rule = false,
                  restricted_join_rule_fix = false,
                  knock_restricted_join_rule = false,
                  enforce_int_power_levels = false,
                  implicit_room_creator = false,
                  updated_redaction_rules = false,
                  hydra = false
                 };
binary_to_room_version(<<"6">>) ->
    #room_version{id = <<"6">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = false,
                  strict_canonicaljson = true,
                  limit_notifications_power_levels = true,
                  knock_join_rule = false,
                  restricted_join_rule = false,
                  restricted_join_rule_fix = false,
                  knock_restricted_join_rule = false,
                  enforce_int_power_levels = false,
                  implicit_room_creator = false,
                  updated_redaction_rules = false,
                  hydra = false
                 };
binary_to_room_version(<<"7">>) ->
    #room_version{id = <<"7">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = false,
                  strict_canonicaljson = true,
                  limit_notifications_power_levels = true,
                  knock_join_rule = true,
                  restricted_join_rule = false,
                  restricted_join_rule_fix = false,
                  knock_restricted_join_rule = false,
                  enforce_int_power_levels = false,
                  implicit_room_creator = false,
                  updated_redaction_rules = false,
                  hydra = false
                 };
binary_to_room_version(<<"8">>) ->
    #room_version{id = <<"8">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = false,
                  strict_canonicaljson = true,
                  limit_notifications_power_levels = true,
                  knock_join_rule = true,
                  restricted_join_rule = true,
                  restricted_join_rule_fix = false,
                  knock_restricted_join_rule = false,
                  enforce_int_power_levels = false,
                  implicit_room_creator = false,
                  updated_redaction_rules = false,
                  hydra = false
                 };
binary_to_room_version(<<"9">>) ->
    #room_version{id = <<"9">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = false,
                  strict_canonicaljson = true,
                  limit_notifications_power_levels = true,
                  knock_join_rule = true,
                  restricted_join_rule = true,
                  restricted_join_rule_fix = true,
                  knock_restricted_join_rule = false,
                  enforce_int_power_levels = false,
                  implicit_room_creator = false,
                  updated_redaction_rules = false,
                  hydra = false
                 };
binary_to_room_version(<<"10">>) ->
    #room_version{id = <<"10">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = false,
                  strict_canonicaljson = true,
                  limit_notifications_power_levels = true,
                  knock_join_rule = true,
                  restricted_join_rule = true,
                  restricted_join_rule_fix = true,
                  knock_restricted_join_rule = true,
                  enforce_int_power_levels = true,
                  implicit_room_creator = false,
                  updated_redaction_rules = false,
                  hydra = false
                 };
binary_to_room_version(<<"11">>) ->
    #room_version{id = <<"11">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = false,
                  strict_canonicaljson = true,
                  limit_notifications_power_levels = true,
                  knock_join_rule = true,
                  restricted_join_rule = true,
                  restricted_join_rule_fix = true,
                  knock_restricted_join_rule = true,
                  enforce_int_power_levels = true,
                  implicit_room_creator = true,
                  updated_redaction_rules = true,
                  hydra = false
                 };
binary_to_room_version(<<"org.matrix.hydra.11">>) ->
    #room_version{id = <<"org.matrix.hydra.11">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = false,
                  strict_canonicaljson = true,
                  limit_notifications_power_levels = true,
                  knock_join_rule = true,
                  restricted_join_rule = true,
                  restricted_join_rule_fix = true,
                  knock_restricted_join_rule = true,
                  enforce_int_power_levels = true,
                  implicit_room_creator = true,
                  updated_redaction_rules = true,
                  hydra = true
                 };
binary_to_room_version(<<"12">>) ->
    #room_version{id = <<"12">>,
                  enforce_key_validity = true,
                  special_case_aliases_auth = false,
                  strict_canonicaljson = true,
                  limit_notifications_power_levels = true,
                  knock_join_rule = true,
                  restricted_join_rule = true,
                  restricted_join_rule_fix = true,
                  knock_restricted_join_rule = true,
                  enforce_int_power_levels = true,
                  implicit_room_creator = true,
                  updated_redaction_rules = true,
                  hydra = true
                 };
binary_to_room_version(_) ->
    false.

supported_versions() ->
    [<<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>,
     <<"10">>, <<"11">>, <<"org.matrix.hydra.11">>, <<"12">>].

%% A PDU's fields of the types the spec gives them
%% (the ids lists of binaries, at most ?MAX_PREV_EVENTS and
%% ?MAX_AUTH_EVENTS, content an object, state_key a string when there is
%% one), thrown out otherwise before any room process reads them.
json_to_event(#{<<"type">> := Type,
                <<"depth">> := Depth,
                <<"auth_events">> := AuthEvents0,
                <<"sender">> := Sender,
                <<"prev_events">> := PrevEvents,
                <<"origin_server_ts">> := OriginServerTS} = JSON, RoomVersion)
  when is_binary(Type),
       is_integer(Depth), Depth >= 0,
       is_list(AuthEvents0), length(AuthEvents0) =< ?MAX_AUTH_EVENTS,
       is_list(PrevEvents), length(PrevEvents) =< ?MAX_PREV_EVENTS,
       is_binary(Sender),
       is_integer(OriginServerTS) ->
    lists:all(fun is_binary/1, AuthEvents0 ++ PrevEvents) orelse throw(bad_event_ids),
    is_map(maps:get(<<"content">>, JSON, none)) orelse throw(bad_content),
    is_binary(maps:get(<<"state_key">>, JSON, <<>>)) orelse throw(bad_state_key),
    EventID = mod_matrix_gw:get_event_id(JSON, RoomVersion),
    {RoomID, AuthEvents} =
        case RoomVersion#room_version.hydra of
            true ->
                case {maps:get(<<"room_id">>, JSON, undefined), Type} of
                    {undefined, ?ROOM_CREATE} ->
                        <<$$, S/binary>> = EventID,
                        {<<$!, S/binary>>, AuthEvents0};
                    {undefined, _} ->
                        throw(missed_room_id);
                    {RID, ?ROOM_CREATE} when is_binary(RID) ->
                        throw(room_id_in_create);
                    {<<$!, S/binary>> = RID, _} ->
                        CreateEvent = <<$$, S/binary>>,
                        case lists:member(CreateEvent, AuthEvents0) of
                            true ->
                                throw(create_in_auth_events);
                            false ->
                                ok
                        end,
                        {RID, [CreateEvent | AuthEvents0]}
                end;
            false ->
                case maps:get(<<"room_id">>, JSON, undefined) of
                    RID when is_binary(RID) -> {RID, AuthEvents0};
                    _ -> throw(missed_room_id)
                end
        end,
    StateKey = maps:get(<<"state_key">>, JSON, undefined),
    case RoomVersion#room_version.strict_canonicaljson of
        true ->
            case mod_matrix_gw:is_canonical_json(JSON) of
                true ->
                    ok;
                false ->
                    throw(non_canonical_json)
            end;
        false ->
            ok
    end,
    #event{id = EventID,
           room_version = RoomVersion,
           room_id = RoomID,
           type = Type,
           state_key = StateKey,
           depth = Depth,
           auth_events = AuthEvents,
           sender = Sender,
           prev_events = PrevEvents,
           origin_server_ts = OriginServerTS,
           json = JSON};
json_to_event(_JSON, _RoomVersion) ->
    throw(malformed_event).

check_event_content_hash(Event) ->
    JSON = Event#event.json,
    case JSON of
        #{<<"hashes">> := #{<<"sha256">> := S}} ->
            Hash = mod_matrix_gw:content_hash(JSON),
            mod_matrix_gw:base64_decode(S) == Hash;
        _ ->
            false
    end.

notify_event(Event, Data) ->
    %% a local user's invite of a remote user to a room
    %% (an invite of a join's state, with no state map, was sent long ago)
    try maybe_send_invite(Event, Data)
    catch Class:Reason:ST ->
            ?INFO_MSG("invite not sent in ~s: ~p", [Data#data.room_id, failure(Class, Reason, ST)])
    end,
    Data2 = notify_event_matrix(Event, Data),
    %% the event among the room's events already (its
    %% state map names it: a member's item reads her own join)
    Data3 = Data2#data{events = (Data2#data.events)#{Event#event.id => Event}},
    %% the end-to-end forwarded set first (mod_matrix_gw_event),
    %% then MatrixRTC's types (mod_matrix_gw_rtc)
    case e2e_notify(Event, Data3) of
        pass ->
            case mod_matrix_gw_rtc:is_rtc_type(Event#event.type) of
                true -> rtc_notify(Event, Data3);
                false -> notify_event_xmpp(Event, Data3)
            end;
        Data4 ->
            Data4
    end.

notify_event_matrix(
  #event{type = ?ROOM_MEMBER,
         state_key = StateKey,
         sender = Sender,
         json = #{<<"content">> := #{<<"membership">> := <<"invite">>}}} = Event,
  #data{kind = #direct{}} = Data) ->
    Host = Data#data.host,
    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
    case mod_matrix_gw:get_id_domain_exn(StateKey) of
        MatrixServer ->
            Data;
        RemoteServer ->
            StrippedState =
                maps:with([{?ROOM_CREATE, <<"">>}, {?ROOM_JOIN_RULES, <<"">>},
                           {?ROOM_MEMBER, Sender}],
                          Event#event.state_map),
            StrippedState2 =
                maps:map(
                  fun(_, EID) ->
                          E = maps:get(EID, Data#data.events),
                          maps:with([<<"sender">>, <<"type">>, <<"state_key">>, <<"content">>],
                                    E#event.json)
                  end, StrippedState),
            JSON = #{<<"event">> => Event#event.json,
                     <<"room_version">> => (Event#event.room_version)#room_version.id,
                     <<"invite_room_state">> => maps:values(StrippedState2)},
            InviteRes =
                mod_matrix_gw:send_request(
                  Data#data.host, put, RemoteServer,
                  [<<"_matrix">>, <<"federation">>,
                   <<"v2">>, <<"invite">>,
                   Data#data.room_id, Event#event.id],
                  [],
                  JSON,
                  [{timeout, 5000}],
                  [{sync, true},
                   {body_format, binary}]),
            ?DEBUG("send invite ~p~n", [InviteRes]),
            Data
    end;
notify_event_matrix(#event{sender = Sender} = Event,
                    Data) ->
    case user_id_to_jid(Sender, Data) of
        #jid{} = SenderJID ->
            %RemoteServers = maps:keys(Data#data.remote_servers),
            RemoteServers = get_remote_servers(Data),
            Host = Data#data.host,
            MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
            lists:foldl(
              fun(Server, DataAcc) ->
                      case Server of
                          MatrixServer ->
                              %% TODO
                              %case parse_user_id(Data#data.remote_user) of
                              %    {ok, U, MatrixServer} ->
                              %        mod_matrix_gw_c2s:notify(
                              %          Host, U, Event);
                              %    _ ->
                              %        ok
                              %end,
                              DataAcc;
                          _ ->
                              case SenderJID#jid.lserver of
                                  Host ->
                                      case DataAcc#data.outgoing_txns of
                                          #{Server := {T, Queue}} ->
                                              Queue2 = [Event | Queue],
                                              DataAcc#data{
                                                outgoing_txns =
                                                    maps:put(Server, {T, Queue2},
                                                             DataAcc#data.outgoing_txns)};
                                          _ ->
                                              send_new_txn([Event], Server, DataAcc)
                                      end;
                                  _ ->
                                      %% the accumulator (26.09
                                      %% returned the room's data as it was)
                                      DataAcc
                              end
                      end
              end, Data, RemoteServers);
        error ->
            Data
    end.

%% m.room.message of every kind with a body, corrections
%% (m.replace) and reactions (m.reaction), where 26.09 carried m.text
%% alone; the message's id is its event id, so a correction, a reaction or
%% a displayed marker from XMPP finds the event again
notify_event_xmpp(
  #event{type = Type, sender = Sender} = Event,
  #data{kind = #direct{local_user = UserJID}} = Data)
  when Type == ?ROOM_MESSAGE; Type == ?REACTION; ?IS_CONTENT(Type) ->
    case user_id_to_jid(Sender, Data) of
        #jid{} = SenderJID ->
            LSenderJID = jid:tolower(SenderJID),
            LUserJID = jid:tolower(UserJID),
            case LSenderJID of
                LUserJID ->
                    Data;
                _ ->
                    case xmpp_payload(Event, chat, Data) of
                        {ID, Body, Els} ->
                            RoomID = Data#data.room_id,
                            Msg = with_body(
                                    #message{id = ID,
                                             from = SenderJID,
                                             to = UserJID,
                                             type = chat,
                                             sub_els = Els ++
                                                 [#xmlel{name = <<"x">>,
                                                         attrs = [{<<"xmlns">>, <<"p1:matrix">>},
                                                                  {<<"room_id">>, RoomID}]}]},
                                    Body),
                            route_payload(maybe_delay(Msg, Event, SenderJID), Data);
                        ignore ->
                            ok
                    end,
                    Data
            end;
        error ->
            Data
    end;
notify_event_xmpp(
  #event{type = Type, sender = Sender,
         origin_server_ts = OriginTS} = Event,
  #data{kind = #multi{users = Users}} = Data)
  when Type == ?ROOM_MESSAGE; Type == ?REACTION; ?IS_CONTENT(Type) ->
    case {Sender, xmpp_payload(Event, groupchat, Data)} of
        {<<$@, SenderUser/binary>>, {ID, Body, Els}} ->
            ?DEBUG("notify xmpp ~p", [Users]),
            maps:fold(
              fun({LUser, LServer}, {online, Resources}, ok) ->
                      maps:fold(
                        fun(LResource, #multi_user{join_ts = JoinTS,
                                                   room_jid = RoomJID}, ok)
                              when JoinTS =< OriginTS ->
                                From = jid:replace_resource(RoomJID, SenderUser),
                                UserJID = jid:make(LUser, LServer, LResource),
                                Msg = with_body(
                                        #message{id = ID,
                                                 from = From,
                                                 to = UserJID,
                                                 type = groupchat,
                                                 sub_els = [#stanza_id{by = RoomJID, id = Event#event.id},
                                                            %% XEP-0421
                                                            mod_matrix_gw_muc:occupant_id_el(
                                                              Data#data.room_id, Sender) | Els]},
                                        Body),
                                route_payload(maybe_delay(Msg, Event, RoomJID), Data);
                           (_, _, _) -> ok
                        end, ok, Resources);
                 (_, _, ok) ->
                      ok
              end, ok, Users),
            Data;
        _ ->
            Data
    end;
%% a membership change is the occupants' presence (an
%% event with no state map, of a join's auth chain or state, is older than
%% every occupant: nothing)
%% (mod_matrix_gw_muc:membership_change/4): a join (to the joining
%% resource its occupant list, its own presence with 110, its history and
%% the subject, XEP-0045 §7.2; 26.09 sent the list to every resource of
%% the user), a name change, a leave, a kick (307) or a ban (301) with its
%% actor and reason, an invite by a local user of a local user (a mediated
%% invite); the room's power levels changing a member's affiliation or
%% role, its topic (the subject), name and avatar (status 104)
notify_event_xmpp(
  #event{type = ?ROOM_MEMBER, sender = Sender, state_key = StateKey,
         json = #{<<"content">> := Content}, state_map = SM} = Event,
  #data{kind = #multi{}} = Data) when is_binary(StateKey), is_map(Content), is_map(SM) ->
    case mod_matrix_gw_muc:membership_change(prev_member_content(Event, Data), Content,
                                             Sender, StateKey) of
        join ->
            notify_join(Event, Data);
        profile ->
            notify_occupant(StateKey, Event, Data),
            Data;
        Gone when Gone == leave; Gone == kick; Gone == ban ->
            notify_gone(Gone, Event, Data);
        invite ->
            notify_invite(Event, Data),
            Data;
        _ ->
            Data
    end;
notify_event_xmpp(#event{type = ?ROOM_POWER_LEVELS, state_key = <<"">>, state_map = SM} = Event,
                  #data{kind = #multi{}} = Data) when is_map(SM) ->
    notify_power_levels(Event, Data),
    Data;
notify_event_xmpp(#event{type = ?ROOM_TOPIC, state_key = <<"">>, sender = Sender,
                         json = #{<<"content">> := Content}, state_map = SM} = Event,
                  #data{kind = #multi{}} = Data) when is_map(SM) ->
    Topic = case Content of
                #{<<"topic">> := T} when is_binary(T) -> T;
                _ -> <<"">>
            end,
    for_occupants(
      Event, Data,
      fun(UserJID, RoomJID, _Self) ->
              ejabberd_router:route(
                #message{id = Event#event.id, from = jid:replace_resource(RoomJID, mod_matrix_gw_muc:nick(Sender)),
                         to = UserJID, type = groupchat, subject = [#text{data = Topic}],
                         sub_els = [mod_matrix_gw_muc:occupant_id_el(Data#data.room_id, Sender)]})
      end),
    Data;
notify_event_xmpp(#event{type = ?ROOM_NAME, state_key = <<"">>, state_map = SM} = Event,
                  #data{kind = #multi{}} = Data) when is_map(SM) ->
    for_occupants(Event, Data,
                  fun(UserJID, RoomJID, _) ->
                          ejabberd_router:route(mod_matrix_gw_muc:status_message(RoomJID, UserJID, [104]))
                  end),
    Data;
notify_event_xmpp(#event{type = ?ROOM_AVATAR, state_key = <<"">>, state_map = SM} = Event,
                  #data{kind = #multi{}} = Data) when is_map(SM) ->
    for_occupants(Event, Data,
                  fun(UserJID, RoomJID, _) ->
                          ejabberd_router:route(mod_matrix_gw_muc:status_message(RoomJID, UserJID, [104]))
                  end),
    fetch_avatar(Event#event.state_map, Data);
%% m.call.* in a direct room, forwarded to the local user as the
%% https://ns.another.im/matrix-e2e/0 <event/> (mod_matrix_gw_call); none in a room
notify_event_xmpp(
  #event{type = <<"m.call.", _/binary>> = Type, id = EventID, sender = Sender,
         origin_server_ts = TS, json = JSON},
  #data{kind = #direct{local_user = UserJID}, room_id = RoomID} = Data) ->
    mod_matrix_gw_call:forward(
      user_id_to_jid(Sender, Data), UserJID,
      #{type => Type, id => EventID, room => RoomID, sender => Sender, ts => TS,
        content => maps:get(<<"content">>, JSON, #{})}),
    Data;
notify_event_xmpp(_Event, Data) ->
    Data.

send_initial_presences(JID, RoomJID, Event, Data) ->
    send_initial_presences(JID, RoomJID, Event#event.sender, Event#event.state_map, Data).

send_initial_presences(JID, RoomJID, Self, StateMap, Data) ->
    ?DEBUG("send_initial_presences ~p", [{JID, Self}]),
    maps:fold(
      fun({?ROOM_MEMBER, UserID}, EID, ok) when UserID /= Self ->
              case statemap_find({?ROOM_MEMBER, UserID}, #{{?ROOM_MEMBER, UserID} => EID}, Data) of
                  {ok, #event{json = #{<<"content">> := #{<<"membership">> := <<"join">>}}}} ->
                      ejabberd_router:route(occupant_presence(UserID, StateMap, RoomJID, JID, [], Data)),
                      ok;
                  _ ->
                      ok
              end;
         (_, _, ok) ->
              ok
      end, ok, StateMap).

%% the item of a user in a room from its state, as
%% mod_matrix_gw_muc:item/3 has it (26.09: moderator and admin from the
%% ban level, else participant and member)
get_user_muc_item(User, StateMap, Data) ->
    Levels = mod_matrix_gw_muc:levels(state_content(?ROOM_POWER_LEVELS, <<"">>, StateMap, Data)),
    Membership = maps:get(<<"membership">>, state_content(?ROOM_MEMBER, User, StateMap, Data),
                          <<"leave">>),
    mod_matrix_gw_muc:item(Levels, get_user_power_level(User, StateMap, Data), Membership).


send_new_txn(Events, Server, Data) ->
    TxnID = p1_rand:get_string(),
    send_txn(TxnID, Events, Server, 1, [], Data).

send_txn(TxnID, Events, Server, Count, Queue, Data) ->
    ?DEBUG("send txn ~p~n", [{TxnID, Server}]),
    Host = Data#data.host,
    Origin = mod_matrix_gw_opt:matrix_domain(Host),
    PDUs =
        lists:map(fun(E) -> E#event.json end, Events),
    Body =
        #{<<"origin">> => Origin,
          <<"origin_server_ts">> =>
              erlang:system_time(millisecond),
          <<"pdus">> => PDUs},
    Self = self(),
    Receiver =
        fun({RequestID, Res}) ->
                ?DEBUG("send_txn_res ~p", [{RequestID, Res}]),
                Self ! {send_txn_res, RequestID, TxnID, Server, Res}
        end,
    %% a server that cannot be reached now fails the transaction
    %% (retried as any failed one), not the event that made it
    RequestID =
        case catch mod_matrix_gw:send_request(
                     Host, put, Server,
                     [<<"_matrix">>, <<"federation">>,
                      <<"v1">>, <<"send">>,
                      TxnID],
                     [],
                     Body,
                     [{timeout, 5000}, {connect_timeout, 5000}],
                     [{sync, false},
                      {receiver, Receiver}]) of
            {ok, RID} ->
                RID;
            Err ->
                Self ! {send_txn_res, undefined, TxnID, Server, Err},
                undefined
        end,
    Data#data{outgoing_txns =
                  maps:put(Server, {{RequestID, TxnID, Events, Count}, Queue},
                           Data#data.outgoing_txns)}.

do_get_missing_events(Origin, EarliestEvents, LatestEvents, Limit, MinDepth, Data) ->
    case is_server_joined(Origin, Data) of
        true ->
            Visited = maps:from_list([{E, []} || E <- EarliestEvents]),
            Queue = queue:from_list(LatestEvents),
            Limit2 = min(max(Limit, 0), 20),
            do_get_missing_events_bfs(Queue, Visited, Limit2, MinDepth, [], Data);
        false ->
            []
    end.

do_get_missing_events_bfs(_Queue, _Visited, 0, _MinDepth, Res, _Data) ->
    Res;
do_get_missing_events_bfs(Queue, Visited, Limit, MinDepth, Res, Data) ->
    case queue:out(Queue) of
        {{value, EventID}, Queue2} ->
            case maps:find(EventID, Data#data.events) of
                {ok, #event{prev_events = PrevEvents}} ->
                    do_get_missing_events_bfs2(
                      PrevEvents, Queue2, Visited, Limit, MinDepth, Res, Data);
                _ ->
                    do_get_missing_events_bfs(Queue2, Visited, Limit, MinDepth, Res, Data)
            end;
        {empty, _} ->
            Res
    end.

do_get_missing_events_bfs2(_PrevEvents, _Queue, _Visited, 0, _MinDepth, Res, _Data) ->
    Res;
do_get_missing_events_bfs2([], Queue, Visited, Limit, MinDepth, Res, Data) ->
    do_get_missing_events_bfs(Queue, Visited, Limit, MinDepth, Res, Data);
do_get_missing_events_bfs2([EventID | PrevEvents], Queue, Visited, Limit, MinDepth, Res, Data) ->
    case maps:is_key(EventID, Visited) of
        true ->
            do_get_missing_events_bfs2(PrevEvents, Queue, Visited, Limit, MinDepth, Res, Data);
        false ->
            case maps:find(EventID, Data#data.events) of
                {ok, #event{depth = Depth} = Event} when Depth >= MinDepth ->
                    Queue2 = queue:in(EventID, Queue),
                    Visited2 = Visited#{EventID => []},
                    Res2 = [Event | Res],
                    do_get_missing_events_bfs2(
                      PrevEvents, Queue2, Visited2, Limit - 1, MinDepth, Res2, Data);
                _ ->
                    do_get_missing_events_bfs2(PrevEvents, Queue, Visited, Limit, MinDepth, Res, Data)
            end
    end.

do_get_state_ids(Origin, EventID, Data) ->
    case is_server_joined(Origin, Data) of
        true ->
            case maps:find(EventID, Data#data.events) of
                {ok, #event{state_map = StateMap} = Event} when is_map(StateMap) ->
                    PrevStateMap = get_event_prev_state_map(Event, Data),
                    PDUs = maps:values(PrevStateMap),
                    AuthChain = do_get_state_ids_dfs(PDUs, #{}, [], Data),
                    {ok, AuthChain, PDUs};
                error ->
                    {error, event_not_found}
            end;
        false ->
            {error, not_allowed}
    end.

do_get_state_ids_dfs([], _Visited, Res, _Data) ->
    Res;
do_get_state_ids_dfs([EventID | Queue], Visited, Res, Data) ->
    case maps:is_key(EventID, Visited) of
        true ->
            do_get_state_ids_dfs(Queue, Visited, Res, Data);
        false ->
            case maps:find(EventID, Data#data.events) of
                {ok, Event} ->
                    Visited2 = Visited#{EventID => []},
                    do_get_state_ids_dfs(
                      Event#event.auth_events ++ Queue, Visited2, [EventID | Res], Data);
                error ->
                    do_get_state_ids_dfs(Queue, Visited, Res, Data)
            end
    end.


is_server_joined(Server, Data) ->
    try
        sets:fold(
          fun(EventID, ok) ->
                  case maps:find(EventID, Data#data.events) of
                      {ok, Event} ->
                          maps:fold(
                            fun({?ROOM_MEMBER, UserID}, EID, ok) ->
                                    case mod_matrix_gw:get_id_domain_exn(UserID) of
                                        Server ->
                                            case maps:find(EID, Data#data.events) of
                                                {ok, #event{
                                                        json = #{<<"content">> :=
                                                                     #{<<"membership">> :=
                                                                           <<"join">>}}}} ->
                                                    throw(found);
                                                _ ->
                                                    ok
                                            end;
                                        _ ->
                                            ok
                                    end;
                               (_, _, ok) ->
                                    ok
                            end, ok, Event#event.state_map),
                          ok;
                      _ ->
                          ok
                  end
          end, ok, Data#data.latest_events),
        false
    catch
        throw:found ->
            true
    end.

get_remote_servers(Data) ->
    Servers =
        sets:fold(
          fun(EventID, Acc) ->
                  case maps:find(EventID, Data#data.events) of
                      {ok, Event} when is_map(Event#event.state_map) ->
                          maps:fold(
                            fun({?ROOM_MEMBER, UserID}, EID, Acc2) ->
                                    Server = mod_matrix_gw:get_id_domain_exn(UserID),
                                    case maps:find(EID, Data#data.events) of
                                        {ok, #event{
                                                json = #{<<"content">> :=
                                                             #{<<"membership">> :=
                                                                   <<"join">>}}}} ->
                                            maps:put(Server, [], Acc2);
                                        _ ->
                                            Acc2
                                    end;
                               (_, _, Acc2) ->
                                    Acc2
                            end, Acc, Event#event.state_map);
                      _ ->
                          Acc
                  end
          end, #{}, Data#data.latest_events),
    maps:keys(Servers).

get_joined_users(Data) ->
    Users =
        sets:fold(
          fun(EventID, Acc) ->
                  case maps:find(EventID, Data#data.events) of
                      {ok, Event} when is_map(Event#event.state_map) ->
                          maps:fold(
                            fun({?ROOM_MEMBER, UserID}, EID, Acc2) ->
                                    case maps:find(EID, Data#data.events) of
                                        {ok, #event{
                                                json = #{<<"content">> :=
                                                             #{<<"membership">> :=
                                                                   <<"join">>}}}} ->
                                            maps:put(UserID, [], Acc2);
                                        _ ->
                                            Acc2
                                    end;
                               (_, _, Acc2) ->
                                    Acc2
                            end, Acc, Event#event.state_map);
                      _ ->
                          Acc
                  end
          end, #{}, Data#data.latest_events),
    maps:keys(Users).

user_id_to_jid(Str, #data{} = Data) ->
    user_id_to_jid(Str, Data#data.host);
user_id_to_jid(Str, Host) when is_binary(Host) ->
    ServerName = mod_matrix_gw_opt:matrix_domain(Host),
    case parse_user_id(Str) of
        {ok, U, ServerName} ->
            jid:make(U, Host);
        {ok, U, S} ->
            ServiceHost = mod_matrix_gw_opt:host(Host),
            EscU = escape(U),
            EscS = escape(S),
            jid:make(<<EscU/binary, $%, EscS/binary>>, ServiceHost);
        error ->
            error
    end.

user_id_from_jid(#jid{luser = U, lserver = Host}, Host) ->
    ServerName = mod_matrix_gw_opt:matrix_domain(Host),
    {ok, <<$@, U/binary, $:, ServerName/binary>>};
user_id_from_jid(JID, _Host) ->
    case binary:split(JID#jid.luser, <<"%">>) of
        [EscU, EscS] ->
            U = unescape(EscU),
            S = unescape(EscS),
            {ok, <<$@, U/binary, $:, S/binary>>};
        _ ->
            error
    end.

new_room_id() ->
    Host = ejabberd_config:get_myname(),
    Letters = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ">>,
    N = size(Letters),
    S = << <<(binary:at(Letters, X rem N))>> ||
            <<X>> <= crypto:strong_rand_bytes(18)>>,
    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
    <<$!, S/binary, $:, MatrixServer/binary>>.

compute_event_auth_keys(#{<<"type">> := ?ROOM_CREATE}, _RoomVersion) ->
    [];
compute_event_auth_keys(#{<<"type">> := ?ROOM_MEMBER,
                          <<"sender">> := Sender,
                          <<"content">> := #{<<"membership">> := Membership} = Content,
                          <<"state_key">> := StateKey},
                        RoomVersion) ->
    Common1 = [{?ROOM_POWER_LEVELS, <<"">>},
               {?ROOM_MEMBER, Sender},
               {?ROOM_MEMBER, StateKey}],
    Common =
        case RoomVersion#room_version.hydra of
            false ->
                [{?ROOM_CREATE, <<"">>} | Common1];
            true ->
                Common1
        end,
    case Membership of
        <<"join">> ->
            case Content of
                #{<<"join_authorised_via_users_server">> := AuthUser}
                  when RoomVersion#room_version.restricted_join_rule ->
                    [{?ROOM_MEMBER, AuthUser}, {?ROOM_JOIN_RULES, <<"">>} | Common];
                _ ->
                    [{?ROOM_JOIN_RULES, <<"">>} | Common]
            end;
        <<"invite">> ->
            case Content of
                #{<<"third_party_invite">> := #{<<"signed">> := #{<<"token">> := Token}}} ->
                    [{?ROOM_3PI, Token}, {?ROOM_JOIN_RULES, <<"">>} | Common];
                _ ->
                    [{?ROOM_JOIN_RULES, <<"">>} | Common]
            end;
        <<"knock">> ->
            [{?ROOM_JOIN_RULES, <<"">>} | Common];
        _ ->
            Common
    end;
compute_event_auth_keys(#{<<"type">> := _, <<"sender">> := Sender}, RoomVersion) ->
    Common1 =
        [{?ROOM_POWER_LEVELS, <<"">>},
         {?ROOM_MEMBER, Sender}],
    case RoomVersion#room_version.hydra of
        false ->
            [{?ROOM_CREATE, <<"">>} | Common1];
        true ->
            Common1
    end.


update_client(#data{kind = #direct{client_state = undefined,
                                   local_user = JID,
                                   remote_user = RemoteUserID}} = Data) ->
    Host = Data#data.host,
    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
    LocalUserID = <<$@, (JID#jid.luser)/binary, $:, MatrixServer/binary>>,
    Users = get_joined_users(Data),
    case lists:member(LocalUserID, Users) of
        true ->
            case lists:delete(LocalUserID, Users) of
                [RemoteUserID] ->
                    {ok, Data#data{kind = (Data#data.kind)#direct{client_state = established}}};
                [_] ->
                    {leave, unknown_remote_user,
                     Data#data{kind = (Data#data.kind)#direct{client_state = leave}}};
                [] ->
                    {ok, Data};
                _ ->
                    {leave, too_many_users,
                     Data#data{kind = (Data#data.kind)#direct{client_state = leave}}}
            end;
        false ->
            {ok, Data}
    end;
update_client(#data{kind = #direct{client_state = established,
                                   local_user = JID,
                                   remote_user = RemoteUserID}} = Data) ->
    Host = Data#data.host,
    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
    LocalUserID = <<$@, (JID#jid.luser)/binary, $:, MatrixServer/binary>>,
    Users = get_joined_users(Data),
    case lists:member(LocalUserID, Users) of
        true ->
            case lists:member(RemoteUserID, Users) of
                true ->
                    %% a third member makes the
                    %% direct chat a room
                    case Users -- [LocalUserID, RemoteUserID] of
                        [] -> {ok, Data};
                        Others -> {ok, direct_to_multi(Others, Data)}
                    end;
                false ->
                    {leave, remote_user_left, Data#data{kind = (Data#data.kind)#direct{client_state = leave}}}
            end;
        false ->
            stop
    end;
update_client(#data{kind = #direct{client_state = leave}}) ->
    stop;
update_client(#data{kind = #multi{users = Users}} = Data) ->
    ?DEBUG("update_client ~p", [Data#data.kind]),
    if
        Users == #{} ->
            stop;
        true ->
            {ok, Data}
    end.


%% A direct chat someone invited a third user into,
%% who joined, is a room from then on: its matrix_direct row goes (the
%% local user's next message to the peer makes a new direct chat), the
%% room is a multi-user one with her as a member who has not joined it on
%% the XMPP side (no leave timer: her Matrix membership stays until she
%% leaves the room), and she gets a mediated invite to the room's JID from
%% the peer. Her messages go to the room from then on.
direct_to_multi(Others, #data{kind = #direct{local_user = JID, remote_user = Remote}} = Data) ->
    {LUser, LServer} = {JID#jid.luser, JID#jid.lserver},
    RoomID = Data#data.room_id,
    mnesia:dirty_delete_object(#matrix_direct{local_remote = {{LUser, LServer}, Remote},
                                              room_id = RoomID}),
    ?INFO_MSG("Matrix direct chat ~s of ~s@~s with ~s gained ~p: a room now",
              [RoomID, LUser, LServer, Remote, Others]),
    Data2 = Data#data{kind = #multi{users = #{{LUser, LServer} => {offline, make_ref()}}}},
    Inviter = case user_id_to_jid(Remote, Data) of
                  #jid{} = J -> J;
                  error -> undefined
              end,
    Names = str:join([mod_matrix_gw_muc:nick(O) || O <- Others], <<", ">>),
    ejabberd_router:route(
      #message{id = p1_rand:get_string(), from = Data#data.room_jid,
               to = jid:make(LUser, LServer),
               sub_els = [#muc_user{invites = [#muc_invite{
                                                  from = Inviter,
                                                  reason = <<Names/binary, " joined your chat with ",
                                                             (mod_matrix_gw_muc:nick(Remote))/binary,
                                                             "; it goes on as this room">>}]}]}),
    persist(none, Data2#data.latest_events, Data2).

send_muc_invite(Host, Origin, RoomID, Sender, UserID, Event, IRS) ->
    case {user_id_to_jid(Sender, Host), user_id_to_jid(UserID, Host)} of
        {#jid{} = SenderJID, #jid{lserver = Host} = UserJID} ->
            process_pdu(Host, Origin, Event),
            ServiceHost = mod_matrix_gw_opt:host(Host),
            %% the room invited to is the signed
            %% event's (its id, reached through the origin); the alias and
            %% name of invite_room_state, which the inviting server alone
            %% asserts, are only shown, as the invite's reason
            Shown =
                lists:foldl(
                  fun(#{<<"type">> := T, <<"content">> := C}, Acc) when is_map(C) ->
                          case {T, C} of
                              {<<"m.room.canonical_alias">>, #{<<"alias">> := A}} when is_binary(A) ->
                                  Acc#{alias => A};
                              {<<"m.room.name">>, #{<<"name">> := N}} when is_binary(N) ->
                                  Acc#{name => N};
                              _ ->
                                  Acc
                          end;
                     (_, Acc) -> Acc
                  end, #{}, if is_list(IRS) -> IRS; true -> [] end),
            Reason = case [V || V <- [maps:get(name, Shown, none), maps:get(alias, Shown, none)],
                                is_binary(V), byte_size(V) =< 255] of
                         [] -> <<>>;
                         Vs -> str:join(Vs, <<" ">>)
                     end,
            {ok, EscRoomID} = room_id_to_xmpp(RoomID, Origin),
            RoomJID = jid:make(EscRoomID, ServiceHost),
            Invite = #muc_invite{to = undefined, from = SenderJID, reason = Reason},
            XUser = #muc_user{invites = [Invite]},
            Msg = #message{
                     from = RoomJID,
                     to = UserJID,
                     sub_els = [XUser]
                    },
            ejabberd_router:route(Msg);
        _ ->
            ok
    end.

room_id_to_xmpp(RoomID) ->
    room_id_to_xmpp(RoomID, undefined).

room_id_to_xmpp(RoomID, Origin) ->
    case RoomID of
        <<$!, Parts/binary>> ->
            case binary:split(Parts, <<":">>) of
                [R, S] ->
                    Len = 8 * size(R),
                    <<IR:Len>> = R,
                    HR = integer_to_binary(IR, 16),
                    {ok, <<$!, HR/binary, $%, S/binary>>};
                [R] ->
                    Len = 8 * size(R),
                    <<IR:Len>> = R,
                    HR = integer_to_binary(IR, 16),
                    case Origin of
                        undefined ->
                            {ok, <<$!, HR/binary>>};
                        S when is_binary(S) ->
                            {ok, <<$!, HR/binary, $%, $%, S/binary>>}
                    end;
                _ -> error
            end;
        _ ->
            error
    end.

%% the room id of a !-JID's opaque part R, Make(Opaque) the
%% room id (Others: more raw readings, a version 12 id without its
%% server). 26.09 writes the opaque part in hex (a JID's local part is
%% case-folded, a room id is not); a client may write it as it is. The
%% room the gateway has under either reading is the one, a raw id matched
%% without case when only that tells it; a room it does not have is the
%% hex reading's when R is hex, else the first raw one.
bang_room_id(R, Make, Others, Via) ->
    Hex = try
              Len = size(R) * 4,
              IR = binary_to_integer(R, 16),
              {ok, Make(<<IR:Len>>)}
          catch _:_ -> error
          end,
    Raws = [Make(R) | Others],
    Known = fun(ID) -> mnesia:dirty_read(matrix_room, ID) /= [] end,
    RoomID = case Hex of
                 {ok, H} ->
                     case Known(H) of
                         true -> H;
                         false -> raw_room_id(Raws, H, Known)
                     end;
                 error ->
                     raw_room_id(Raws, hd(Raws), Known)
             end,
    {ok, RoomID, Via}.

raw_room_id(Raws, Default, Known) ->
    case [Raw || Raw <- Raws, Known(Raw)] of
        [Raw | _] ->
            Raw;
        [] ->
            Lowers = [string:lowercase(Raw) || Raw <- Raws],
            case [ID || ID <- mnesia:dirty_all_keys(matrix_room),
                        is_binary(ID), lists:member(string:lowercase(ID), Lowers)] of
                [ID | _] -> ID;
                [] -> Default
            end
    end.

%% For a JID of another server, a remote alias is
%% looked up in the cache only: a stanza from anyone (s2s) to
%% #x%<name>@G made the gateway connect to whatever <name> was.
room_id_from_xmpp(Host, RID, #jid{lserver = Host}) ->
    room_id_from_xmpp(Host, RID);
room_id_from_xmpp(Host, <<$#, Parts/binary>> = RID, _From) ->
    case binary:split(Parts, <<"%">>) of
        [R, S] ->
            case mod_matrix_gw_opt:matrix_domain(Host) of
                S ->
                    room_id_from_xmpp(Host, RID);
                _ ->
                    case ets_cache:lookup(?MATRIX_ROOM_ALIAS_CACHE, <<$#, R/binary, $:, S/binary>>) of
                        {ok, <<$!, _/binary>> = RoomID} -> {ok, RoomID, S};
                        _ -> error
                    end
            end;
        _ ->
            error
    end;
room_id_from_xmpp(Host, RID, _From) ->
    room_id_from_xmpp(Host, RID).

room_id_from_xmpp(Host, RID) ->
    case RID of
        <<$!, Parts/binary>> ->
            %% a room id written as it is (!opaque%srv@G, as
            %% the end-to-end wire has it) names the room too, not
            %% only 26.09's hex form; neither crashes the routing
            case binary:split(Parts, <<"%">>) of
                [R, <<$%, S/binary>>] ->
                    bang_room_id(R, fun(I) -> <<$!, I/binary>> end, [], S);
                [R, <<>>] ->
                    bang_room_id(R, fun(I) -> <<$!, I/binary>> end, [], undefined);
                [R, S] ->
                    %% a room id of version 12 has no server: then %srv is
                    %% only where to reach the room
                    bang_room_id(R, fun(I) -> <<$!, I/binary, $:, S/binary>> end,
                                 [<<$!, R/binary>>], S);
                [R] ->
                    bang_room_id(R, fun(I) -> <<$!, I/binary>> end, [], undefined);
                _ -> error
            end;
        <<$#, Parts/binary>> ->
            case binary:split(Parts, <<"%">>) of
                [R, S] ->
                    Alias = <<$#, R/binary, $:, S/binary>>,
                    %% an alias of ours is in our table
                    Resolved = case mod_matrix_gw_opt:matrix_domain(Host) of
                                   S -> mod_matrix_gw_muc:alias_lookup(Alias);
                                   _ -> resolve_alias(Host, S, Alias)
                               end,
                    case Resolved of
                        {ok, <<$!, _/binary>> = RoomID} ->
                            {ok, RoomID, S};
                        error ->
                            error
                    end;
                _ -> error
            end;
        _ ->
            error
    end.

resolve_alias(Host, Origin, Alias) ->
    ets_cache:lookup(
      ?MATRIX_ROOM_ALIAS_CACHE, Alias,
      fun() ->
              Res =
                  mod_matrix_gw:send_request(
                    Host, get, Origin,
                    [<<"_matrix">>, <<"federation">>,
                     <<"v1">>, <<"query">>, <<"directory">>],
                    [{<<"room_alias">>, Alias}],
                    none,
                    [{timeout, 5000}],
                    [{sync, true},
                     {body_format, binary}]),
              case Res of
                  {ok, {{_, 200, _}, _Headers, Body}} ->
                      try
                          case misc:json_decode(Body) of
                              #{<<"room_id">> := RoomID} ->
                                  {ok, RoomID}
                          end
                      catch
                          Class:Reason:ST ->
                              ?DEBUG("failed resolve_alias: ~p", [failure(Class, Reason, ST)]),
                              {cache_with_timeout, error, ?MATRIX_ROOM_ALIAS_CACHE_ERROR_TIMEOUT}
                      end;
                  {ok, {{_, _Status, _Reason}, _Headers, _Body}} ->
                      {cache_with_timeout, error, ?MATRIX_ROOM_ALIAS_CACHE_ERROR_TIMEOUT};
                  {error, _Reason} ->
                      {cache_with_timeout, error, ?MATRIX_ROOM_ALIAS_CACHE_ERROR_TIMEOUT}
              end
      end).


escape(S) ->
    escape(S, <<>>).

escape(<<>>, Res) ->
    Res;
escape(<<C, S/binary>>, Res) ->
    Res2 =
        case C of
            $\s -> <<Res/binary, "\\20">>;
            $"  -> <<Res/binary, "\\22">>;
            $%  -> <<Res/binary, "\\25">>;
            $&  -> <<Res/binary, "\\26">>;
            $'  -> <<Res/binary, "\\27">>;
            $/  -> <<Res/binary, "\\2f">>;
            $:  -> <<Res/binary, "\\3a">>;
            $<  -> <<Res/binary, "\\3c">>;
            $>  -> <<Res/binary, "\\3e">>;
            $@  -> <<Res/binary, "\\40">>;
            $\\ -> <<Res/binary, "\\5c">>;
            _ -> <<Res/binary, C>>
        end,
    escape(S, Res2).

unescape(S) ->
    unescape(S, <<>>).

unescape(<<>>, Res) -> Res;
unescape(<<"\\20", S/binary>>, Res) -> unescape(S, <<Res/binary, $\s>>);
unescape(<<"\\22", S/binary>>, Res) -> unescape(S, <<Res/binary, $\">>);
unescape(<<"\\25", S/binary>>, Res) -> unescape(S, <<Res/binary, $%>>);
unescape(<<"\\26", S/binary>>, Res) -> unescape(S, <<Res/binary, $&>>);
unescape(<<"\\27", S/binary>>, Res) -> unescape(S, <<Res/binary, $\'>>);
unescape(<<"\\2f", S/binary>>, Res) -> unescape(S, <<Res/binary, $/>>);
unescape(<<"\\3a", S/binary>>, Res) -> unescape(S, <<Res/binary, $:>>);
unescape(<<"\\3c", S/binary>>, Res) -> unescape(S, <<Res/binary, $<>>);
unescape(<<"\\3e", S/binary>>, Res) -> unescape(S, <<Res/binary, $>>>);
unescape(<<"\\40", S/binary>>, Res) -> unescape(S, <<Res/binary, $@>>);
unescape(<<"\\5c", S/binary>>, Res) -> unescape(S, <<Res/binary, $\\>>);
unescape(<<C, S/binary>>, Res) -> unescape(S, <<Res/binary, C>>).

%%%===================================================================
%%% Federation, persistence and the XMPP side
%%%===================================================================

%%--------------------------------------------------------------------
%% Kept across a restart (mod_matrix_gw_store)
%%--------------------------------------------------------------------

%% A room as mod_matrix_gw_store kept it, if it did: its version, kind,
%% state events and forward extremities. A local member of a multi-user
%% room gets RESTORE_GRACE seconds to join again before the gateway leaves
%% the room for her, as if she had left it then.
restore(Data) ->
    case catch mod_matrix_gw_store:load(Data#data.room_id) of
        {ok, #{version := Version, kind := Kind, latest := Latest, events := Stored}} ->
            case binary_to_room_version(Version) of
                #room_version{} = RoomVersion ->
                    Events =
                        lists:foldl(
                          fun({EventID, JSON, StateMap}, Acc) ->
                                  try json_to_event(JSON, RoomVersion) of
                                      #event{id = EventID} = E ->
                                          Acc#{EventID => E#event{state_map = StateMap}};
                                      _ ->
                                          Acc
                                  catch _:_ ->
                                          Acc
                                  end
                          end, #{}, Stored),
                    LatestSet = sets:from_list([E || E <- Latest, maps:is_key(E, Events)],
                                               [{version, 2}]),
                    NonLatest =
                        maps:fold(
                          fun(_, E, Acc) ->
                                  lists:foldl(fun sets:add_element/2, Acc,
                                              E#event.prev_events ++ E#event.auth_events)
                          end, sets:new([{version, 2}]), Events),
                    Data2 = Data#data{room_version = RoomVersion,
                                      events = Events,
                                      latest_events = LatestSet,
                                      nonlatest_events = NonLatest,
                                      stored_meta = {Version, Kind}},
                    ?INFO_MSG("Matrix room ~s restored with ~B events",
                              [Data#data.room_id, maps:size(Events)]),
                    %% a room's history is
                    %% backfilled again at the first join after the restart
                    Data3 = case Kind of
                                multi -> Data2#data{content = (Data2#data.content)#{backfill => restored}};
                                _ -> Data2
                            end,
                    Data3#data{kind = restore_kind(Kind, Data3)};
                false ->
                    Data
            end;
        _ ->
            Data
    end.

restore_kind({direct, {LUser, LServer}, RemoteUserID, ClientState}, Data) ->
    mnesia:dirty_write(
      #matrix_direct{local_remote = {{LUser, LServer}, RemoteUserID},
                     room_id = Data#data.room_id}),
    #direct{local_user = jid:make(LUser, LServer),
            remote_user = RemoteUserID,
            client_state = ClientState};
restore_kind(multi, Data) ->
    Host = Data#data.host,
    Grace = max(mod_matrix_gw_opt:leave_timeout(Host), ?RESTORE_GRACE) * 1000,
    Users =
        lists:foldl(
          fun(UserID, Acc) ->
                  case user_id_to_jid(UserID, Data) of
                      #jid{lserver = Host, luser = LUser} ->
                          Timer = erlang:start_timer(Grace, self(), {leave, LUser, Host}),
                          Acc#{{LUser, Host} => {offline, Timer}};
                      _ ->
                          Acc
                  end
          end, #{}, get_joined_users(Data)),
    #multi{users = Users}.

stored_kind(#direct{local_user = #jid{} = JID, remote_user = Remote,
                    client_state = ClientState}) when is_binary(Remote) ->
    {direct, {JID#jid.luser, JID#jid.lserver}, Remote, ClientState};
stored_kind(#multi{}) ->
    multi;
stored_kind(_) ->
    undefined.

%% Saves what a restart needs after Event was stored (none: only the
%% room's kind changed): the room's version and kind, every state event,
%% the forward extremities; a message event that stopped being an
%% extremity is forgotten.
persist(Event, OldLatest, Data) ->
    case stored_kind(Data#data.kind) of
        undefined ->
            Data;
        Kind ->
            try
                do_persist(Event, OldLatest, Kind, Data)
            catch
                Class:Reason:ST ->
                    ?WARNING_MSG("Matrix room ~s not saved: ~p",
                                 [Data#data.room_id, failure(Class, Reason, ST)]),
                    Data
            end
    end.

do_persist(Event, OldLatest, Kind, Data) ->
    RoomID = Data#data.room_id,
    Version = (Data#data.room_version)#room_version.id,
    Latest = Data#data.latest_events,
    LatestList = sets:to_list(Latest),
    Keep = fun(#event{id = ID, state_key = SK}) ->
                   SK /= undefined orelse sets:is_element(ID, Latest)
           end,
    Stored = fun(#event{id = ID, json = JSON, state_map = SM}) -> {ID, JSON, SM} end,
    {New, NewID} = case Event of
                       #event{id = EID} -> {[Event], EID};
                       none -> {[], undefined}
                   end,
    case Data#data.stored_meta of
        {Version, Kind} ->
            Gone = [E || E <- sets:to_list(sets:subtract(OldLatest, Latest)),
                         E /= NewID,
                         case maps:find(E, Data#data.events) of
                             {ok, #event{state_key = undefined}} -> true;
                             _ -> false
                         end],
            mod_matrix_gw_store:save_events(
              RoomID, [Stored(E) || E <- New, Keep(E)], LatestList),
            mod_matrix_gw_store:delete_events(RoomID, Gone),
            Data;
        _ ->
            %% the room's first save, or its kind changed: all of it
            mod_matrix_gw_store:save_meta(RoomID, Version, Kind, LatestList),
            mod_matrix_gw_store:save_events(
              RoomID, [Stored(E) || E <- maps:values(Data#data.events), Keep(E)], LatestList),
            Data#data{stored_meta = {Version, Kind}}
    end.

%% The room is left: nothing of it is kept.
forget(Data) ->
    catch mod_matrix_gw_store:delete(Data#data.room_id),
    %% nobody here shares the room with its
    %% members any more (mod_matrix_gw_e2e's matrix_room_share rows go:
    %% device-list changes stop going to their servers)
    catch [mod_matrix_gw_event:membership(Data#data.host, Data#data.room_id, MXID, <<"leave">>)
           || {MXID, _} <- state_members(current_state_map(Data), Data)],
    %% the aliases of a room made here go with it
    catch mod_matrix_gw_muc:alias_forget(Data#data.room_id),
    ok.

%%--------------------------------------------------------------------
%% XMPP ids, reactions and history
%%--------------------------------------------------------------------

%% The room's bookkeeping for a new event: the XMPP id it was sent with,
%% a reaction added or taken back (whose new set the XMPP side gets here),
%% a message for the MUC history.
track_event(#event{id = EventID, type = Type, sender = Sender, json = JSON} = Event, Data) ->
    %% an <event/>'s XMPP id is the stanza's, not in its content
    XmppID0 = case {mod_matrix_gw_xmpp:xmpp_id(JSON), Data#data.e2e_sending} of
                  {<<>>, {Sender, X}} when is_binary(X) -> X;
                  {X, _} -> X
              end,
    Data2 =
        case XmppID0 of
            <<>> ->
                Data;
            XmppID ->
                case user_id_to_jid(Sender, Data) of
                    #jid{lserver = LServer, luser = LUser} when LServer == Data#data.host ->
                        mod_matrix_gw_event:record_sent({LUser, LServer}, XmppID, EventID,
                                                        Data#data.room_id);
                    _ ->
                        ok
                end,
                C = Data#data.counter + 1,
                Data#data{counter = C,
                          xmpp_ids = mod_matrix_gw_xmpp:trim_ids(
                                       (Data#data.xmpp_ids)#{XmppID => {C, EventID}},
                                       ?MAX_XMPP_IDS)}
        end,
    case Type of
        ?ROOM_MESSAGE ->
            add_backlog(Event, Data2);
        ?REACTION ->
            case mod_matrix_gw_xmpp:inbound_message(JSON) of
                {reaction, Target, Key} ->
                    Data2#data{reactions = mod_matrix_gw_xmpp:add_reaction(
                                             Data2#data.reactions, {Sender, Target},
                                             Key, EventID)};
                _ ->
                    Data2
            end;
        %% a poll response is the voter's reactions
        %% with the answers' emojis, in place of the ones it had
        T when T == <<"org.matrix.msc3381.poll.response">>; T == <<"m.poll.response">> ->
            case mod_matrix_gw_xmpp:inbound_message(JSON) of
                {vote, Target, Answers} ->
                    case poll_options_of(Target, Data2) of
                        [] ->
                            Data2;
                        Options ->
                            Key = {Sender, Target},
                            Keys = maps:without([E || {_, E, _} <- Options],
                                                maps:get(Key, Data2#data.reactions, #{})),
                            Chosen = maps:from_list([{E, EventID} || {Id, E, _} <- Options,
                                                                     lists:member(Id, Answers)]),
                            Data2#data{reactions = (Data2#data.reactions)#{Key => maps:merge(Keys, Chosen)}}
                    end;
                _ ->
                    Data2
            end;
        ?ROOM_REDACTION ->
            case mod_matrix_gw_xmpp:remove_reaction(Data2#data.reactions, redacts(JSON)) of
                {{Reactor, Target}, Reactions2} ->
                    Data3 = Data2#data{reactions = Reactions2},
                    %% the reactor's new set, as XEP-0444 says it
                    Synthetic =
                        Event#event{type = ?REACTION, sender = Reactor,
                                    json = JSON#{<<"type">> => ?REACTION,
                                                 <<"content">> =>
                                                     #{<<"m.relates_to">> =>
                                                           #{<<"rel_type">> => <<"m.annotation">>,
                                                             <<"event_id">> => Target,
                                                             <<"key">> => <<>>}}}},
                    notify_event_xmpp(Synthetic, Data3);
                none ->
                    %% a message redacted: its retraction, in a direct
                    %% chat and in a room alike, is the end-to-end
                    %% forwarding's (e2e_notify/2 -> e2e_forward/4: ONE
                    %% stanza, the <event/> and the retraction rendered by
                    %% mod_matrix_gw_muc:retraction/4, never a deletion)
                    ?DEBUG("redaction ~s in ~s", [EventID, Data#data.room_id]),
                    Data2
            end;
        _ ->
            Data2
    end.

redacts(#{<<"redacts">> := R}) when is_binary(R) -> R;
redacts(#{<<"content">> := #{<<"redacts">> := R}}) when is_binary(R) -> R;
redacts(_) -> <<>>.

%% A message for the MUC history, newest first, at most MAX_BACKLOG.
add_backlog(#event{type = ?ROOM_MESSAGE, id = EventID, sender = Sender,
                   origin_server_ts = TS, json = JSON}, Data) ->
    case mod_matrix_gw_xmpp:inbound_message(JSON) of
        {text, Body} ->
            case lists:keymember(EventID, 2, Data#data.backlog) of
                true ->
                    Data;
                false ->
                    Backlog = lists:reverse(lists:keysort(1, [{TS, EventID, Sender, Body}
                                                              | Data#data.backlog])),
                    Data#data{backlog = lists:sublist(Backlog, ?MAX_BACKLOG)}
            end;
        _ ->
            Data
    end;
add_backlog(_, Data) ->
    Data.

%% The room's last messages before the join, from the server we join
%% through (/backfill), for the history of a room just joined: shown, not
%% put into the room's graph. The gateway (rooms): pages of up to MAX_BACKLOG
%% events (state events among them) back from the oldest of the last page,
%% until MAX_BACKLOG messages or BACKFILL_PAGES pages.
backfill_history(MatrixServer, #event{prev_events = Prev, state_map = SM}, Data) ->
    %% the join's state (not stored yet), which the
    %% history's senders are checked against (history_sender/2)
    put(matrix_history_state, SM),
    try backfill_history(MatrixServer, Prev, ?BACKFILL_PAGES, #{}, Data)
    after erase(matrix_history_state)
    end.

backfill_history(_MatrixServer, [], _Pages, _Seen, Data) ->
    Data;
backfill_history(_MatrixServer, _From, 0, _Seen, Data) ->
    Data;
backfill_history(_MatrixServer, _From, _Pages, _Seen, Data)
  when length(Data#data.backlog) >= ?MAX_BACKLOG ->
    Data;
backfill_history(MatrixServer, From, Pages, Seen, Data) ->
    Res = mod_matrix_gw:send_request(
            Data#data.host, get, MatrixServer,
            [<<"_matrix">>, <<"federation">>, <<"v1">>, <<"backfill">>, Data#data.room_id],
            [{<<"limit">>, integer_to_binary(?MAX_BACKLOG)} | [{<<"v">>, E} || E <- From]],
            none,
            [{connect_timeout, 5000}, {timeout, 10000}],
            [{sync, true}, {body_format, binary}]),
    case Res of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            try misc:json_decode(Body) of
                #{<<"pdus">> := PDUs} when is_list(PDUs) ->
                    {Data2, Events} =
                        lists:foldl(
                          fun(PDU, {D, Es}) ->
                                  try
                                      E = json_to_event(PDU, D#data.room_version),
                                      RoomID = D#data.room_id,
                                      RoomID = E#event.room_id,
                                      {ok, E2} = check_event_sig_and_hash(D#data.host, E),
                                      %% shown only
                                      %% from a sender the room knows and
                                      %% has not banned
                                      true = history_sender(E2, D),
                                      {add_backlog(E2, D), [E2 | Es]}
                                  catch _:_ ->
                                          {D, Es}
                                  end
                          end, {Data, []}, PDUs),
                    New = [E || E <- Events, not maps:is_key(E#event.id, Seen)],
                    Seen2 = lists:foldl(fun(E, S) -> S#{E#event.id => true} end, Seen, New),
                    %% the next page: back from the oldest events of this
                    %% one, those whose previous events it does not have
                    Next = lists:usort([P || E <- New, P <- E#event.prev_events,
                                             not maps:is_key(P, Seen2)]),
                    case New of
                        [] -> Data2;
                        _ -> backfill_history(MatrixServer, lists:sublist(Next, 20), Pages - 1, Seen2, Data2)
                    end;
                _ ->
                    Data
            catch _:_ ->
                    Data
            end;
        _ ->
            ?DEBUG("no backfill of ~s from ~s: ~p", [Data#data.room_id, MatrixServer, Res]),
            Data
    end.

%% Whether a backfilled event may be shown as the
%% history: its sender has a membership in the state the join brought
%% (while the join backfills; else the room's current state) and it is not
%% a ban. The state before an
%% event of the history is not known here (its prev events are not in the
%% graph); the current state at least keeps out a sender who was never in
%% the room or was banned from it, while a member who left keeps the words
%% said before.
history_sender(#event{sender = Sender}, Data) ->
    try case get(matrix_history_state) of
            SM when is_map(SM) -> SM;
            _ -> current_state_map(Data)
        end of
        #{{?ROOM_MEMBER, Sender} := EID} ->
            case maps:find(EID, Data#data.events) of
                {ok, #event{json = #{<<"content">> := #{<<"membership">> := <<"ban">>}}}} -> false;
                {ok, _} -> true;
                error -> false
            end;
        _ ->
            false
    catch _:_ ->
            false
    end.
%% A room brought back after a restart has no
%% messages of before it (the store keeps none): at the first join of a
%% resource, its last messages again from a server in the room, back from
%% the forward extremities, as a fresh join's.
restored_backfill(#data{content = #{backfill := restored} = C} = Data) ->
    Data1 = Data#data{content = maps:remove(backfill, C)},
    Own = mod_matrix_gw_opt:matrix_domain(Data#data.host),
    case get_remote_servers(Data1) -- [Own] of
        [Server | _] ->
            try backfill_history(Server, sets:to_list(Data1#data.latest_events),
                                 ?BACKFILL_PAGES, #{}, Data1)
            catch Class:Reason ->
                    ?DEBUG("no backfill of ~s after a restart: ~p",
                           [Data#data.room_id, {Class, Reason}]),
                    Data1
            end;
        [] ->
            Data1
    end;
restored_backfill(Data) ->
    Data.

%% <history/> of a MUC join (maxstanzas up to
%% MAX_BACKLOG, since, seconds; mod_matrix_gw_muc:history_request/2),
%% {DEFAULT_HISTORY, 0} without one.
requested_history(Packet) ->
    History = case catch xmpp:get_subtag(Packet, #muc{}) of
                  #muc{history = #muc_history{} = H} -> H;
                  _ -> undefined
              end,
    {N, Since} = mod_matrix_gw_muc:history_request(History, erlang:system_time(millisecond)),
    {min(N, ?MAX_BACKLOG), Since}.

multi_user(Packet) ->
    {History, Since} = requested_history(Packet),
    Nick = case xmpp:get_to(Packet) of
               #jid{resource = R} -> R;
               _ -> <<>>
           end,
    #multi_user{history = History, since = Since, nick = Nick}.

%% XEP-0045 §7.2.9: a nick the room changed is status 210 in the joining
%% resource's own presence.
nick_codes(#multi_user{nick = Asked}, UserID) when Asked /= <<>> ->
    [210 || Asked /= mod_matrix_gw_muc:nick(UserID)];
nick_codes(_, _) ->
    [].

send_history(UserJID, RoomJID, MU, JoinTS, Data) ->
    #multi_user{history = N, since = Since} = MU,
    Entries = lists:sublist([E || {TS, _, _, _} = E <- Data#data.backlog, TS < JoinTS, TS >= Since], N),
    lists:foreach(
      fun({TS, EventID, <<$@, Nick/binary>> = Sender, Body}) ->
              Msg = #message{id = xmpp_id_of(EventID, Data),
                             from = jid:replace_resource(RoomJID, Nick),
                             to = UserJID,
                             type = groupchat,
                             body = [#text{data = Body}],
                             sub_els = [#stanza_id{by = RoomJID, id = EventID},
                                        mod_matrix_gw_muc:occupant_id_el(Data#data.room_id, Sender)]},
              ejabberd_router:route(
                misc:add_delay_info(Msg, RoomJID, misc:usec_to_now(TS * 1000)));
         (_) ->
              ok
      end, lists:reverse(Entries)).

send_subject(UserJID, RoomJID, StateMap, Data) ->
    Topic =
        case statemap_find({?ROOM_TOPIC, <<"">>}, StateMap, Data) of
            {ok, #event{json = #{<<"content">> := #{<<"topic">> := T}}}} when is_binary(T) -> T;
            _ -> <<"">>
        end,
    ejabberd_router:route(#message{from = RoomJID, to = UserJID, type = groupchat,
                                   subject = [#text{data = Topic}]}).

%% The room's current state: its forward extremities' states, resolved.
current_state_map(Data) ->
    StateMaps = [SM || EID <- sets:to_list(Data#data.latest_events),
                       #event{state_map = SM} <- [maps:get(EID, Data#data.events, undefined)],
                       is_map(SM)],
    resolve_state_maps(StateMaps, Data).

%% A failure as an INFO line may show it: the class
%% and the reason's name (an atom, or a tuple's tag with the ids, numbers
%% and atoms beside it), never a term of the event (a badmatch or a
%% case_clause carries the JSON: room content, ciphertext). The stack goes
%% to DEBUG with each frame's arity in place of its arguments.
-spec failure(atom(), term(), list()) -> {atom(), term()}.
failure(Class, Reason, ST) ->
    ?DEBUG("failure ~p:~p at ~p",
           [Class, reason_name(Reason),
            [case Frame of
                 {M, F, A, _} when is_list(A) -> {M, F, length(A)};
                 {M, F, A, _} -> {M, F, A};
                 _ -> frame
             end || Frame <- ST]]),
    {Class, reason_name(Reason)}.

reason_name(R) when is_atom(R); is_integer(R) ->
    R;
reason_name(R) when is_tuple(R), tuple_size(R) > 0, is_atom(element(1, R)) ->
    list_to_tuple([element(1, R) | [case X of
                                         _ when is_atom(X); is_integer(X) -> X;
                                         <<C, _/binary>> when (C == $$ orelse C == $! orelse C == $@
                                                               orelse C == $#), byte_size(X) =< 255 -> X;
                                         _ -> '_'
                                     end || X <- tl(tuple_to_list(R))]]);
reason_name(_) ->
    '_'.

is_joined_member(UserID, Data) ->
    try current_state_map(Data) of
        #{{?ROOM_MEMBER, UserID} := EID} ->
            case maps:find(EID, Data#data.events) of
                {ok, #event{json = #{<<"content">> := #{<<"membership">> := <<"join">>}}}} -> true;
                _ -> false
            end;
        _ ->
            false
    catch _:_ ->
            false
    end.

%% What a resource joining a room its user is already in gets: the
%% occupants, its own presence, the history, the subject.
send_join_info(UserJID, UserID, RoomJID, Data) ->
    StateMap = current_state_map(Data),
    send_initial_presences(UserJID, RoomJID, UserID, StateMap, Data),
    {LUser, LServer, LResource} = jid:tolower(UserJID),
    #multi{users = Users} = Kind = Data#data.kind,
    Codes = case Users of
                #{{LUser, LServer} := {online, #{LResource := MU0}}} -> [110 | nick_codes(MU0, UserID)];
                _ -> [110]
            end,
    ejabberd_router:route(occupant_presence(UserID, StateMap, RoomJID, UserJID, Codes, Data)),
    send_avatar_presence(UserJID, RoomJID, Data),
    case Users of
        #{{LUser, LServer} := {online, #{LResource := #multi_user{join_ts = JoinTS,
                                                                  history = N} = MU} = Rs}} ->
            case N of
                sent -> ok;
                _ -> send_history(UserJID, RoomJID, MU, JoinTS, Data)
            end,
            send_subject(UserJID, RoomJID, StateMap, Data),
            Rs2 = Rs#{LResource => MU#multi_user{history = sent}},
            Data#data{kind = Kind#multi{users = Users#{{LUser, LServer} => {online, Rs2}}}};
        _ ->
            Data
    end.

send_self_unavailable(UserJID, RoomJID, Data) ->
    case user_id_from_jid(UserJID, Data#data.host) of
        {ok, <<$@, Nick/binary>>} ->
            ejabberd_router:route(
              #presence{from = jid:replace_resource(RoomJID, Nick),
                        to = UserJID,
                        type = unavailable,
                        sub_els = [#muc_user{items = [#muc_item{affiliation = none,
                                                                role = none}],
                                             status_codes = [110]}]});
        _ ->
            ok
    end.

%% The XMPP message an event is: {ID, Body, SubEls} or ignore. Its id is
%% the event's, or the XMPP id it was sent with (a groupchat message's echo
%% to its sender's resources); a correction or reaction names its target
%% by the id the XMPP side knows it by (in groupchat, a reaction names the
%% stanza-id, XEP-0444 §4.3).
xmpp_payload(#event{id = EventID, type = Type, sender = Sender, json = JSON} = Event, Mode, Data) ->
    ID = case {Mode, mod_matrix_gw_xmpp:xmpp_id(JSON)} of
             {groupchat, X} when X /= <<>> -> X;
             _ -> EventID
         end,
    %% a plain message names its event for the device (an empty
    %% <event/>, mod_matrix_gw_event), on text and corrections only (an
    %% <event/> of another kind would make the device drop the stanza)
    Plain = mod_matrix_gw_event:plain_event_el(EventID, Data#data.room_id, Sender,
                                               maps:get(<<"origin_server_ts">>, JSON, 0)),
    %% in a direct chat a message names its sender
    %% (XEP-0172) and carries its event id as XEP-0359's stanza-id, as a
    %% room's does
    ChatEls = case Mode of
                  chat -> [#stanza_id{by = jid:make(mod_matrix_gw_opt:host(Data#data.host)),
                                      id = EventID} |
                           [mod_matrix_gw_content:nick_el(N) || N <- [sender_name(Event, Data)],
                                                                N /= <<>>]];
                  groupchat -> []
              end,
    case mod_matrix_gw_xmpp:inbound_message(JSON) of
        {text, Body} ->
            %% a reply, mentions, a place
            %% (mod_matrix_gw_content:inbound_els/3)
            {Body2, Extra} = mod_matrix_gw_content:inbound_els(
                               maps:get(<<"content">>, JSON, #{}), Body, content_ctx(Mode, Data)),
            {ID, [#text{data = Body2}],
             Extra ++ [mod_matrix_gw_xmpp:markable_el() || Mode == chat] ++ ChatEls ++ [Plain]};
        {edit, Target, Body} ->
            {ID, [#text{data = Body}], [mod_matrix_gw_xmpp:replace_el(xmpp_id_of(Target, Data)), Plain]};
        {R, Target, _} when R == reaction; R == vote ->
            RefID = case Mode of
                        chat -> xmpp_id_of(Target, Data);
                        groupchat -> Target
                    end,
            Keys = mod_matrix_gw_xmpp:reaction_keys(Data#data.reactions, Sender, Target),
            {ID, [], [mod_matrix_gw_xmpp:reactions_el(RefID, Keys),
                      mod_matrix_gw_xmpp:store_el()]};
        %% a file: its body is filled when it is in the store
        %% (route_payload/2); an EncryptedFile goes to the device as it is
        {media, Content} ->
            {ID, {media, Content}, [mod_matrix_gw_xmpp:markable_el() || Mode == chat] ++ ChatEls};
        {sticker, Content} ->
            {ID, {media, Content}, [mod_matrix_gw_xmpp:markable_el() || Mode == chat] ++
                 mod_matrix_gw_content:sticker_els() ++ ChatEls};
        {poll, Poll} ->
            Options = mod_matrix_gw_content:poll_options(Poll, mod_matrix_gw_xmpp:xmpp_id(JSON) /= <<>>),
            {Body, Els} = mod_matrix_gw_content:poll_els(Poll, Options, <<>>),
            {ID, [#text{data = Body}], Els ++ [mod_matrix_gw_xmpp:markable_el() || Mode == chat] ++ ChatEls};
        {poll_end, Target} ->
            case poll_of(Target, Data) of
                {Poll, Options} ->
                    Votes = [{nick_of(S), maps:keys(Ks)}
                             || {{S, T}, Ks} <- maps:to_list(Data#data.reactions), T == Target],
                    {Body, Els} = mod_matrix_gw_content:poll_closed_els(
                                    Poll, Options, Votes, xmpp_id_of(Target, Data)),
                    {ID, [#text{data = Body}], Els ++ ChatEls};
                undefined ->
                    ?DEBUG("end of an unknown poll ~s in ~s", [Target, Data#data.room_id]),
                    ignore
            end;
        {encrypted_media, Content} ->
            {ID, [#text{data = mod_matrix_gw_media:file_name(Content)}],
             [mod_matrix_gw_media:event_el(Type, EventID, Data#data.room_id, Sender,
                                           maps:get(<<"origin_server_ts">>, JSON, 0), Content),
              mod_matrix_gw_xmpp:store_el()]};
        ignore ->
            ?DEBUG("~s ~s in ~s not carried to XMPP", [Type, EventID, Data#data.room_id]),
            ignore
    end.

%% what mod_matrix_gw_content:inbound_els/3 asks of the
%% room: the id and sender of a message replied to, its text, the JID of a
%% user mentioned
content_ctx(Mode, Data) ->
    Find = fun(E) -> maps:find(E, Data#data.events) end,
    #{reply_id => fun(E) ->
                          case Mode of
                              chat -> xmpp_id_of(E, Data);
                              groupchat -> E
                          end
                  end,
      reply_to => fun(E) ->
                          case Find(E) of
                              {ok, #event{sender = <<$@, Nick/binary>>}} when Mode == groupchat ->
                                  jid:encode(jid:replace_resource(Data#data.room_jid, Nick));
                              {ok, #event{sender = S}} ->
                                  case user_id_to_jid(S, Data) of
                                      #jid{} = J -> jid:encode(J);
                                      error -> <<>>
                                  end;
                              error ->
                                  <<>>
                          end
                  end,
      target_body => fun(E) ->
                             case Find(E) of
                                 {ok, #event{json = J}} ->
                                     case mod_matrix_gw_xmpp:inbound_message(J) of
                                         {text, B} -> mod_matrix_gw_content:strip_matrix_reply(B);
                                         _ -> undefined
                                     end;
                                 error ->
                                     undefined
                             end
                     end,
      jid_of => fun(M) ->
                        case user_id_to_jid(M, Data) of
                            #jid{} = J -> jid:encode(J);
                            error -> error
                        end
                end}.

%% the display name of an event's sender, from its
%% m.room.member in the state the event was sent in, else the profile
%% cache (mod_matrix_gw_profile)
sender_name(#event{sender = Sender, state_map = StateMap}, Data) ->
    FromState =
        case is_map(StateMap) andalso maps:find({?ROOM_MEMBER, Sender}, StateMap) of
            {ok, EID} ->
                case maps:find(EID, Data#data.events) of
                    {ok, #event{json = #{<<"content">> := #{<<"displayname">> := N}}}}
                      when is_binary(N) -> N;
                    _ -> <<>>
                end;
            _ ->
                <<>>
        end,
    case FromState of
        <<>> -> mod_matrix_gw_profile:cached_name(Data#data.host, Sender);
        _ -> FromState
    end.

nick_of(<<$@, Nick/binary>>) -> Nick;
nick_of(MXID) -> MXID.

%% a poll start of the room, {Poll, Options}, or
%% undefined
poll_of(EventID, Data) ->
    case maps:find(EventID, Data#data.events) of
        {ok, #event{json = JSON}} ->
            case mod_matrix_gw_xmpp:inbound_message(JSON) of
                {poll, Poll} ->
                    {Poll, mod_matrix_gw_content:poll_options(
                             Poll, mod_matrix_gw_xmpp:xmpp_id(JSON) /= <<>>)};
                _ ->
                    undefined
            end;
        error ->
            undefined
    end.

poll_options_of(EventID, Data) ->
    case poll_of(EventID, Data) of
        {_, Options} -> Options;
        undefined -> []
    end.

%% the receipts asked of the events the other server
%% took (its /send answer names no error for them), sent from the Matrix
%% user to the resource that sent the message
send_receipts(Events, RBody, #data{kind = #direct{remote_user = Remote}, content = C} = Data)
  when is_binary(Remote) ->
    case maps:get(receipts, C, #{}) of
        Receipts when map_size(Receipts) == 0 ->
            Data;
        Receipts ->
            Failed = mod_matrix_gw_content:failed_pdus(RBody),
            {Sent, Receipts2} =
                lists:foldl(
                  fun(#event{id = EID, json = JSON}, {Acc, Rs}) ->
                          X = mod_matrix_gw_xmpp:xmpp_id(JSON),
                          case maps:take(X, Rs) of
                              {{_, {From, Kinds}}, Rs2} ->
                                  case lists:member(EID, Failed) of
                                      true -> {Acc, Rs2};
                                      false -> {[{X, From, Kinds} | Acc], Rs2}
                                  end;
                              error ->
                                  {Acc, Rs}
                          end
                  end, {[], Receipts}, Events),
            case user_id_to_jid(Remote, Data) of
                #jid{} = SenderJID ->
                    lists:foreach(
                      fun({X, From, Kinds}) ->
                              ejabberd_router:route(
                                #message{id = p1_rand:get_string(), from = SenderJID, to = From,
                                         type = chat,
                                         sub_els = mod_matrix_gw_content:receipt_els(X, Kinds)})
                      end, lists:reverse(Sent));
                error ->
                    ok
            end,
            Data#data{content = C#{receipts => Receipts2}}
    end;
send_receipts(_Events, _RBody, Data) ->
    Data.

%% a message of xmpp_payload/3, a file's once it is in the store
%% (its body {media, Content} is in the message's meta, with_body/2)
route_payload(#message{meta = #{matrix_media := Content} = Meta} = Msg, Data) ->
    mod_matrix_gw_media:route_share(Data#data.host, Msg#message{meta = maps:remove(matrix_media, Meta)},
                                    Content),
    ok;
route_payload(Msg, _Data) ->
    ejabberd_router:route(Msg).

with_body(Msg, {media, Content}) ->
    Msg#message{meta = #{matrix_media => Content}};
with_body(Msg, Body) ->
    Msg#message{body = Body}.

%% The id an event has on the XMPP side: the XMPP message's when it was
%% sent from there, else the event id.
xmpp_id_of1(EventID, Data) ->
    ByIds = fun() ->
                    maps:fold(fun(XmppID, {_, E}, _) when E == EventID -> XmppID;
                                 (_, _, Acc) -> Acc
                              end, EventID, Data#data.xmpp_ids)
            end,
    case maps:find(EventID, Data#data.events) of
        {ok, #event{json = JSON}} ->
            case mod_matrix_gw_xmpp:xmpp_id(JSON) of
                %% an <event/>'s (encrypted content) or a pruned one's
                <<>> -> ByIds();
                XmppID -> XmppID
            end;
        error ->
            ByIds()
    end.

%% an event the room's map has forgotten (a
%% restart, past MAX_XMPP_IDS) by matrix_sent
xmpp_id_of(EventID, Data) ->
    case xmpp_id_of1(EventID, Data) of
        EventID ->
            RoomID = Data#data.room_id,
            case mod_matrix_gw_event:sent_event(EventID) of
                {ok, _, XmppID, RoomID} -> XmppID;
                _ -> EventID
            end;
        XmppID ->
            XmppID
    end.

%% The event an XMPP id names: one sent from XMPP, or an event id itself.
%% The room's map is a cache of the last ids; an
%% older one, or one from before a restart, is in matrix_sent (disc), as
%% UserID's own (a correction, her own retraction) or as another local
%% member's of this room (a reaction, a marker, a moderation).
event_id_of(ID, UserID, Data) ->
    case maps:find(ID, Data#data.xmpp_ids) of
        {ok, {_, EventID}} -> EventID;
        error ->
            case ID of
                <<$$, _/binary>> -> ID;
                _ -> sent_event_id(ID, UserID, Data)
            end
    end.

sent_event_id(ID, UserID, Data) ->
    Host = Data#data.host,
    RoomID = Data#data.room_id,
    Asker = case user_id_to_jid(UserID, Data) of
                #jid{luser = U, lserver = Host} -> [{U, Host}];
                _ -> []
            end,
    Users = Asker ++ [US || US <- local_users(Data), not lists:member(US, Asker)],
    lists:foldl(
      fun(_, EventID) when is_binary(EventID) -> EventID;
         (US, undefined) ->
              case mod_matrix_gw_event:lookup_sent(US, ID) of
                  {ok, EventID, RoomID} -> EventID;
                  _ -> undefined
              end
      end, undefined, Users).

local_users(#data{kind = #direct{local_user = #jid{luser = U, lserver = S}}}) -> [{U, S}];
local_users(#data{kind = #multi{users = Users}}) -> maps:keys(Users);
local_users(_) -> [].

%% The sender of an event of the room: the room's copy's, or
%% the local user's who sent it from XMPP (matrix_sent) when the room no
%% longer holds it; error when neither knows it.
target_sender(EventID, Data) ->
    case maps:find(EventID, Data#data.events) of
        {ok, #event{sender = Sender}} ->
            {ok, Sender};
        error ->
            RoomID = Data#data.room_id,
            case mod_matrix_gw_event:sent_event(EventID) of
                {ok, {LUser, LServer}, _, RoomID} ->
                    user_id_from_jid(jid:make(LUser, LServer), Data#data.host);
                _ ->
                    error
            end
    end.

%% A delay (XEP-0203) on a message of an event older than half a minute.
maybe_delay(Msg, #event{origin_server_ts = TS}, By) ->
    case erlang:system_time(millisecond) - TS > 30000 of
        true -> misc:add_delay_info(Msg, By, misc:usec_to_now(TS * 1000));
        false -> Msg
    end.

%%--------------------------------------------------------------------
%% From XMPP
%%--------------------------------------------------------------------

xmpp_action({text, Text}, UserID, MsgID, Data) ->
    add_json(?ROOM_MESSAGE, mod_matrix_gw_xmpp:text_content(Text, MsgID), UserID, Data);
%% a file in the store as m.image/m.video/m.audio/m.file and its caption;
%% a link elsewhere is the text it was
xmpp_action({media, URL, Meta, Text}, UserID, MsgID, Data) ->
    case mod_matrix_gw_media:outbound_contents(Data#data.host, URL, Meta, Text, MsgID) of
        %% a XEP-0449 sticker
        {ok, [Media | _]} when is_map_key(sticker, Meta) ->
            add_json(<<"m.sticker">>,
                     mod_matrix_gw_content:sticker_content(Media, maps:get(desc, Meta, <<>>)),
                     UserID, Data);
        {ok, Contents} ->
            lists:foldl(fun(C, D) -> add_json(?ROOM_MESSAGE, C, UserID, D) end, Data, Contents);
        not_ours when Text /= <<>> ->
            xmpp_action({text, Text}, UserID, MsgID, Data);
        not_ours ->
            error({not_shareable, URL})
    end;
%% a text with a reply, mentions, a place, a poll or a
%% poll's close (mod_matrix_gw_content)
xmpp_action({text, Text, Rel}, UserID, MsgID, Data) ->
    Host = Data#data.host,
    Rel2 = mod_matrix_gw_content:resolve_rel(
             Rel, #{event => fun(ID) -> event_id_of(ID, UserID, Data) end,
                    sender => fun(E) ->
                                      case maps:find(E, Data#data.events) of
                                          {ok, #event{sender = S}} -> S;
                                          error -> undefined
                                      end
                              end,
                    mxid => fun(U) -> mod_matrix_gw_content:mention_mxid(U, Host) end}),
    {Type, Content} = mod_matrix_gw_content:outbound(Text, Rel2, MsgID),
    add_json(Type, Content, UserID, Data);
%% the receipts the sender asked for, sent once the
%% other server takes the event (send_receipts/3); a room reflects its
%% messages itself (XEP-0045)
xmpp_action({receipt_request, From, Kinds}, _UserID, MsgID, #data{kind = #direct{}} = Data)
  when MsgID /= <<>> ->
    C = Data#data.content,
    Receipts = mod_matrix_gw_xmpp:trim_ids(
                 (maps:get(receipts, C, #{}))#{MsgID => {erlang:unique_integer([monotonic, positive]),
                                                         {From, Kinds}}},
                 ?MAX_XMPP_IDS),
    Data#data{content = C#{receipts => Receipts}};
xmpp_action({receipt_request, _From, _Kinds}, _UserID, _MsgID, Data) ->
    Data;
xmpp_action({edit, ID, Text}, UserID, MsgID, Data) ->
    case event_id_of(ID, UserID, Data) of
        undefined ->
            ?DEBUG("correction of an unknown message ~s, sent as a new one", [ID]),
            xmpp_action({text, Text}, UserID, MsgID, Data);
        Target ->
            add_json(?ROOM_MESSAGE, mod_matrix_gw_xmpp:edit_content(Target, Text, MsgID),
                     UserID, Data)
    end;
xmpp_action({reactions, ID, Keys}, UserID, _MsgID, Data) ->
    case event_id_of(ID, UserID, Data) of
        undefined ->
            ?DEBUG("reactions to an unknown message ~s refused", [ID]),
            error({unknown_target, ID});
        Target ->
            %% on a poll, the answers' emojis are a
            %% vote (an m.poll.response when they changed), the rest
            %% reactions
            Options = poll_options_of(Target, Data),
            {Keys1, Data1} = poll_vote(Target, Keys, Options, UserID, Data),
            Had = maps:without([E || {_, E, _} <- Options],
                               maps:get({UserID, Target}, Data1#data.reactions, #{})),
            Added = [K || K <- lists:usort(Keys1), not maps:is_key(K, Had)],
            Gone = [E || {K, E} <- maps:to_list(Had), not lists:member(K, Keys1)],
            Data2 = lists:foldl(
                      fun(K, D) ->
                              add_json(?REACTION, mod_matrix_gw_xmpp:reaction_content(Target, K),
                                       UserID, D)
                      end, Data1, Added),
            lists:foldl(
              fun(E, D) ->
                      {D2, _} = add_event(#{<<"type">> => ?ROOM_REDACTION,
                                            <<"sender">> => UserID,
                                            <<"redacts">> => E,
                                            <<"content">> => mod_matrix_gw_xmpp:redaction_content(E)},
                                          D),
                      D2
              end, Data2, Gone)
    end;
%% A user's typing starts with composing and ends at the first other chat
%% state (after a message, <active/>): the EDU says each once, so a client's
%% <active/> on every message is no notice at all.
xmpp_action({typing, Typing}, UserID, _MsgID, Data) ->
    case {Typing, maps:is_key(UserID, Data#data.typing)} of
        {true, _} ->
            queue_edu(mod_matrix_gw_xmpp:typing_edu(Data#data.room_id, UserID, true), Data),
            Data#data{typing = (Data#data.typing)#{UserID => true}};
        {false, true} ->
            queue_edu(mod_matrix_gw_xmpp:typing_edu(Data#data.room_id, UserID, false), Data),
            Data#data{typing = maps:remove(UserID, Data#data.typing)};
        {false, false} ->
            Data
    end;
xmpp_action({displayed, ID}, UserID, _MsgID, Data) ->
    case event_id_of(ID, UserID, Data) of
        undefined ->
            error({unknown_target, ID});
        EventID ->
            queue_edu(mod_matrix_gw_xmpp:receipt_edu(Data#data.room_id, UserID, EventID,
                                                     erlang:system_time(millisecond)), Data),
            Data
    end.

poll_vote(_Target, Keys, [], _UserID, Data) ->
    {Keys, Data};
poll_vote(Target, Keys, Options, UserID, Data) ->
    {Votes, Others} = mod_matrix_gw_content:vote_split(Keys, Options),
    Had = maps:get({UserID, Target}, Data#data.reactions, #{}),
    HadVotes = [Id || {Id, E, _} <- Options, maps:is_key(E, Had)],
    case lists:sort(Votes) == lists:sort(HadVotes) of
        true ->
            {Others, Data};
        false ->
            {Others, add_json(<<"org.matrix.msc3381.poll.response">>,
                              mod_matrix_gw_content:response_content(Target, Votes), UserID, Data)}
    end.

add_json(Type, Content, UserID, Data) ->
    {Data2, _} = add_event(#{<<"type">> => Type, <<"sender">> => UserID,
                             <<"content">> => Content}, Data),
    Data2.

%% An EDU to every other server in the room.
queue_edu(EDU, Data) ->
    Host = Data#data.host,
    lists:foreach(fun(Server) -> mod_matrix_gw_s2s:queue_edu(Host, Server, EDU) end,
                  get_remote_servers(Data)).

%% A typing notice or read receipt from the federation, for the XMPP side.
notify_edu({typing, _, UserID, Typing},
           #data{kind = #direct{local_user = UserJID, remote_user = UserID}} = Data) ->
    ejabberd_router:route(
      #message{id = p1_rand:get_string(),
               from = user_id_to_jid(UserID, Data), to = UserJID, type = chat,
               sub_els = [mod_matrix_gw_xmpp:chatstate_el(Typing),
                          mod_matrix_gw_xmpp:no_store_el()]});
notify_edu({typing, _, <<$@, Nick/binary>> = UserID, Typing},
           #data{kind = #multi{users = Users}} = Data) ->
    Sender = case user_id_to_jid(UserID, Data) of
                 #jid{} = J -> {J#jid.luser, J#jid.lserver};
                 error -> undefined
             end,
    maps:foreach(
      fun({LUser, LServer}, {online, Resources}) when {LUser, LServer} /= Sender ->
              maps:foreach(
                fun(LResource, #multi_user{room_jid = RoomJID}) ->
                        ejabberd_router:route(
                          #message{id = p1_rand:get_string(),
                                   from = jid:replace_resource(RoomJID, Nick),
                                   to = jid:make(LUser, LServer, LResource),
                                   type = groupchat,
                                   sub_els = [mod_matrix_gw_xmpp:chatstate_el(Typing),
                                              mod_matrix_gw_xmpp:no_store_el()]})
                end, Resources);
         (_, _) ->
              ok
      end, Users);
notify_edu({receipt, _, UserID, EventID},
           #data{kind = #direct{local_user = UserJID, remote_user = UserID}} = Data) ->
    ejabberd_router:route(
      #message{id = p1_rand:get_string(),
               from = user_id_to_jid(UserID, Data), to = UserJID, type = chat,
               sub_els = [mod_matrix_gw_xmpp:displayed_el(xmpp_id_of(EventID, Data)),
                          mod_matrix_gw_xmpp:store_el()]});
notify_edu(Item, Data) ->
    ?DEBUG("EDU ~p not carried to XMPP in ~s", [Item, Data#data.room_id]),
    ok.

%% Whether a remote event fails the auth rules against the room's current
%% state (its auth events taken from that state instead of its own).
soft_failed(#event{type = ?ROOM_CREATE}, _Data) ->
    false;
soft_failed(#event{sender = Sender, json = JSON} = Event, Data) ->
    Local = mod_matrix_gw_opt:matrix_domain(Data#data.host),
    case catch mod_matrix_gw:get_id_domain_exn(Sender) of
        Local ->
            false;
        _ ->
            try
                Current = current_state_map(Data),
                StateMap =
                    lists:foldl(
                      fun(Key, Acc) ->
                              case maps:find(Key, Current) of
                                  {ok, EID} ->
                                      case maps:find(EID, Data#data.events) of
                                          {ok, E} -> Acc#{Key => E};
                                          error -> Acc
                                      end;
                                  error ->
                                      Acc
                              end
                      end, #{},
                      %% the create event too: a room of version 12
                      %% (hydra) names it in no auth_events, its rules
                      %% still read it
                      [{?ROOM_CREATE, <<"">>} |
                       compute_event_auth_keys(JSON, Data#data.room_version)]),
                not check_event_auth(Event, StateMap, Data)
            catch _:_ ->
                    false
            end
    end.

%%--------------------------------------------------------------------
%% The end-to-end room events (mod_matrix_gw_event)
%%--------------------------------------------------------------------

%% A room event from a local user's <event/>: into the room of the room
%% JID, or the direct room with u%srv@G, made first when there is none
%% (encrypted when matrix_encrypt_dms says so and the event is
%% m.room.encrypted).
route_e2e_event(#message{from = From, to = To, id = MsgID, lang = Lang} = Pkt,
                Host, Type, Content) ->
    Cast = {e2e_event, From, To, MsgID, Lang, Type, Content},
    case To#jid.luser of
        <<C, _/binary>> when C == $!; C == $# ->
            case room_pid_of(Host, To) of
                {ok, Pid} -> gen_statem:cast(Pid, Cast);
                error -> no_room(Pkt)
            end;
        _ ->
            case user_id_from_jid(To, Host) of
                {ok, ToMatrixID} ->
                    case direct_pid_of(Host, From, ToMatrixID) of
                        {ok, Pid} ->
                            gen_statem:cast(Pid, Cast);
                        none ->
                            {ok, FromMatrixID} = user_id_from_jid(From, Host),
                            %% encrypted from the start when the device
                            %% already encrypts: a plain first event (a
                            %% call, a verification) would be refused in
                            %% a room it made encrypted
                            Enc = [mod_matrix_gw_event:encryption_event(FromMatrixID)
                                   || Type == <<"m.room.encrypted">>,
                                      mod_matrix_gw_opt:matrix_encrypt_dms(Host)],
                            case new_direct_room(Host, FromMatrixID, ToMatrixID, Enc) of
                                {ok, Pid} -> gen_statem:cast(Pid, Cast);
                                _ -> no_room(Pkt)
                            end;
                        error ->
                            no_room(Pkt)
                    end;
                error ->
                    ok
            end
    end,
    ok.

no_room(Pkt) ->
    Lang = xmpp:get_lang(Pkt),
    ejabberd_router:route_error(Pkt, xmpp:err_item_not_found(<<"no such room">>, Lang)).

%% The process of the room a room JID names, when the gateway has it.
room_pid_of(Host, #jid{luser = RID}) ->
    case catch room_id_from_xmpp(Host, RID) of
        {ok, RoomID, _Via} ->
            case get_existing_room_pid(Host, RoomID) of
                {ok, Pid} -> {ok, Pid};
                _ -> error
            end;
        _ ->
            error
    end.

%% The process of the direct room of From's user with ToMatrixID: none
%% when there is no such room yet, error when its process is gone.
direct_pid_of(Host, From, ToMatrixID) ->
    case mnesia:dirty_read(matrix_direct, {{From#jid.luser, From#jid.lserver}, ToMatrixID}) of
        [#matrix_direct{room_id = RoomID}] ->
            case get_existing_room_pid(Host, RoomID) of
                {ok, Pid} -> {ok, Pid};
                _ -> error
            end;
        [] ->
            none
    end.

%% <room-info/>, <encryption/>, <redact/> to a room JID or u%srv@G, from a
%% local user, answered by the room's process; <encryption/> on a direct
%% chat that does not exist yet makes it, encrypted from its start.
route_e2e_iq(#iq{from = From, to = To, lang = Lang} = IQ, Req) ->
    Host = ejabberd_config:get_myname(),
    Reply = fun(Err) -> ejabberd_router:route(xmpp:make_error(IQ, Err)) end,
    NotFound = fun() -> Reply(xmpp:err_item_not_found(<<"no such room">>, Lang)) end,
    case From#jid.lserver of
        Host ->
            case To#jid.luser of
                <<C, _/binary>> when C == $!; C == $# ->
                    case room_pid_of(Host, To) of
                        {ok, Pid} -> gen_statem:cast(Pid, {e2e_iq, IQ, Req});
                        error -> NotFound()
                    end;
                _ ->
                    case user_id_from_jid(To, Host) of
                        {ok, ToMatrixID} ->
                            case {direct_pid_of(Host, From, ToMatrixID), Req} of
                                {{ok, Pid}, _} ->
                                    gen_statem:cast(Pid, {e2e_iq, IQ, Req});
                                {none, {encryption, _}} ->
                                    {ok, FromMatrixID} = user_id_from_jid(From, Host),
                                    Enc = [mod_matrix_gw_event:encryption_event(FromMatrixID)],
                                    case new_direct_room(Host, FromMatrixID, ToMatrixID, Enc) of
                                        {ok, Pid} -> gen_statem:cast(Pid, {e2e_iq, IQ, Req});
                                        _ -> NotFound()
                                    end;
                                _ ->
                                    NotFound()
                            end;
                        error ->
                            NotFound()
                    end
            end;
        _ ->
            Reply(xmpp:err_not_allowed(<<"not a local user">>, Lang))
    end,
    ok.

%%--------------------------------------------------------------------
%% MatrixRTC (mod_matrix_gw_rtc)
%%--------------------------------------------------------------------

%% A checked <send/> or <room-state/> from a local user to the room's JID
%% or u%srv@G, answered by the room's process; a <send/> to a direct chat
%% that does not exist yet makes it (as a first <event/> does), a
%% <room-state/> of one is empty.
rtc_route(#iq{from = From, to = To, lang = Lang} = IQ, Req) ->
    Host = ejabberd_config:get_myname(),
    NotFound = fun() -> ejabberd_router:route(
                          xmpp:make_error(IQ, xmpp:err_item_not_found(<<"no such room">>, Lang)))
               end,
    Cast = {rtc_iq, IQ, Req},
    case To#jid.luser of
        <<C, _/binary>> when C == $!; C == $# ->
            case room_pid_of(Host, To) of
                {ok, Pid} -> gen_statem:cast(Pid, Cast);
                error -> NotFound()
            end;
        _ ->
            case user_id_from_jid(To, Host) of
                {ok, ToMatrixID} ->
                    case {direct_pid_of(Host, From, ToMatrixID), Req} of
                        {{ok, Pid}, _} ->
                            gen_statem:cast(Pid, Cast);
                        {none, {room_state, _}} ->
                            ejabberd_router:route(mod_matrix_gw_rtc:room_state_result(IQ, []));
                        {none, {send, _}} ->
                            {ok, FromMatrixID} = user_id_from_jid(From, Host),
                            case new_direct_room(Host, FromMatrixID, ToMatrixID, []) of
                                {ok, Pid} -> gen_statem:cast(Pid, Cast);
                                _ -> NotFound()
                            end;
                        _ ->
                            NotFound()
                    end;
                error ->
                    NotFound()
            end
    end,
    ok.

%% A delayed event's JSON into the room RoomID as the local user US: ok,
%% or error when the gateway has no such room now.
rtc_fire(Host, RoomID, US, JSON) ->
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            gen_statem:cast(Pid, {rtc_fire, US, JSON}),
            ok;
        _ ->
            error
    end.

rtc_iq(#iq{from = From, lang = Lang} = IQ, Req, Data) ->
    case owns_room(From, Data) of
        false ->
            {xmpp:make_error(IQ, xmpp:err_item_not_found(<<"no such room">>, Lang)), Data};
        true ->
            {ok, UserID} = user_id_from_jid(From, Data#data.host),
            rtc_iq(IQ, Req, UserID, Data)
    end.

rtc_iq(IQ, {send, #{delay_ms := Delay} = Send}, _UserID, Data) when is_integer(Delay) ->
    {mod_matrix_gw_rtc:schedule(IQ, Send#{room => Data#data.room_id}), Data};
rtc_iq(#iq{lang = Lang} = IQ, {send, #{type := Type, state_key := Key, content := Content} = Send},
       UserID, Data) ->
    JSON0 = #{<<"type">> => Type, <<"sender">> => UserID, <<"content">> => Content},
    JSON = case Key of
               undefined -> JSON0;
               _ -> JSON0#{<<"state_key">> => Key}
           end,
    try add_event(JSON, Data) of
        {Data2, Event} ->
            {mod_matrix_gw_rtc:send_result(IQ, Send#{room => Data#data.room_id}, Event#event.id),
             Data2}
    catch
        error:{event_auth_error, _} ->
            {mod_matrix_gw_rtc:send_error(IQ, power_level, Lang), Data};
        error:{too_large, _} ->
            {mod_matrix_gw_rtc:send_error(IQ, too_large, Lang), Data}
    end;
rtc_iq(#iq{lang = Lang} = IQ, {room_state, Types}, UserID, Data) ->
    StateMap = current_state_map(Data),
    Member = maps:get(<<"membership">>, state_content(?ROOM_MEMBER, UserID, StateMap, Data), <<"leave">>),
    case Member == <<"join">> orelse Member == <<"invite">> of
        false ->
            {xmpp:make_error(IQ, xmpp:err_item_not_found(<<"not a member of the room">>, Lang)), Data};
        true ->
            State = [rtc_fields(E, #{})
                     || {T, _} = K <- lists:sort(maps:keys(StateMap)), lists:member(T, Types),
                        {ok, #event{} = E} <- [statemap_find(K, StateMap, Data)]],
            Fields = [F || #{type := T, sender := S} = F <- State ++ rtc_sticky(Types, Data),
                           mod_matrix_gw_rtc:owner_ok(T, maps:get(state_key, F, undefined), S,
                                                      maps:get(content, F))],
            {mod_matrix_gw_rtc:room_state_result(IQ, Fields), Data}
    end.

%% The unexpired sticky events of the generation B member types among
%% Types, the newest per sender and sticky key (MSC4354's map).
rtc_sticky(Types, Data) ->
    Now = erlang:system_time(millisecond),
    Sticky = [T || T <- Types, mod_matrix_gw_rtc:kind(T) == member_b],
    Best = case Sticky of
               [] ->
                   #{};
               _ ->
                   maps:fold(
                     fun(_, #event{type = T, sender = S, json = J} = E, Acc) ->
                             case lists:member(T, Sticky) andalso mod_matrix_gw_rtc:sticky_left(J, Now) of
                                 Left when is_integer(Left), Left > 0 ->
                                     SK = maps:get(<<"sticky_key">>, maps:get(<<"content">>, J, #{}), <<>>),
                                     Key = {S, T, SK},
                                     case Acc of
                                         #{Key := {L0, _}} when L0 >= Left -> Acc;
                                         _ -> Acc#{Key => {Left, E}}
                                     end;
                                 _ ->
                                     Acc
                             end
                     end, #{}, Data#data.events)
           end,
    [rtc_fields(E, #{sticky_ms => Left}) || {_, {Left, E}} <- lists:sort(maps:to_list(Best))].

rtc_fields(#event{type = Type, id = ID, sender = Sender, origin_server_ts = TS,
                  state_key = Key, json = JSON}, Extra) ->
    F = #{type => Type, id => ID, room => maps:get(<<"room_id">>, JSON, <<>>), sender => Sender,
          ts => TS, content => case JSON of
                                   #{<<"content">> := C} when is_map(C) -> C;
                                   _ -> #{}
                               end},
    maps:merge(case Key of
                   K when is_binary(K) -> F#{state_key => K};
                   _ -> F
               end, Extra).

%% An RTC event the room took (a remote one or a local user's): to the
%% local members' devices that speak MatrixRTC.
rtc_notify(#event{type = Type, sender = Sender, state_map = SM, json = JSON} = Event, Data)
  when is_map(SM) ->
    case rtc_recipients(SM, Data) of
        [] ->
            Data;
        Recipients ->
            Extra = case mod_matrix_gw_rtc:kind(Type) of
                        member_b ->
                            case mod_matrix_gw_rtc:sticky_left(JSON, erlang:system_time(millisecond)) of
                                none -> #{};
                                Left -> #{sticky_ms => Left}
                            end;
                        _ ->
                            #{}
                    end,
            Origin = case catch mod_matrix_gw:get_id_domain_exn(Sender) of
                         O when is_binary(O) -> O;
                         _ -> <<>>
                     end,
            try mod_matrix_gw_rtc:forward(Data#data.host,
                                          (rtc_fields(Event, Extra))#{room => Data#data.room_id},
                                          Origin, Recipients)
            catch Class:Reason:ST ->
                    ?INFO_MSG("failed to forward ~s ~s in ~s: ~p",
                              [Type, Event#event.id, Data#data.room_id, failure(Class, Reason, ST)])
            end,
            Data
    end;
rtc_notify(_Event, Data) ->
    Data.

%% [{UserBareJID, ChatJID, MessageType}]: a direct chat's local user (from
%% the peer's JID), a room's local joined members (from the room's JID).
rtc_recipients(_SM, #data{kind = #direct{local_user = #jid{} = U, remote_user = R}} = Data)
  when is_binary(R) ->
    case user_id_to_jid(R, Data) of
        #jid{} = Chat -> [{jid:remove_resource(U), Chat, chat}];
        _ -> []
    end;
rtc_recipients(SM, #data{kind = #multi{users = Users}} = Data) ->
    [{jid:make(LU, LS), rtc_room_jid({LU, LS}, Users, Data), groupchat}
     || UserID <- joined_members(SM, Data), {LU, LS} <- [local_us(UserID, Data)]];
rtc_recipients(_SM, _Data) ->
    [].

%% The room JID a member's resources joined by, else the room's own.
rtc_room_jid(US, Users, Data) ->
    case Users of
        #{US := {online, Resources}} ->
            case [J || #multi_user{room_jid = J} <- maps:values(Resources)] of
                [J | _] -> J;
                [] -> Data#data.room_jid
            end;
        _ ->
            Data#data.room_jid
    end.

%% The members of a room the gateway has, with their membership.
-spec members(binary()) -> [{binary(), binary()}].
members(RoomID) ->
    Host = ejabberd_config:get_myname(),
    case get_existing_room_pid(Host, RoomID) of
        {ok, Pid} ->
            try gen_statem:call(Pid, e2e_members, 10000)
            catch _:_ -> []
            end;
        _ ->
            []
    end.

%% [{MXID, Membership}] of a state map.
state_members(StateMap, Data) ->
    lists:sort(
      maps:fold(
        fun({?ROOM_MEMBER, MXID}, _, Acc) ->
                case statemap_find({?ROOM_MEMBER, MXID}, StateMap, Data) of
                    {ok, #event{json = #{<<"content">> := #{<<"membership">> := M}}}}
                      when is_binary(M) ->
                        [{MXID, M} | Acc];
                    _ ->
                        Acc
                end;
           (_, _, Acc) ->
                Acc
        end, [], StateMap)).

%% The room's m.room.encryption content, or undefined.
room_encryption(StateMap, Data) ->
    case statemap_find({?ROOM_ENCRYPTION, <<"">>}, StateMap, Data) of
        {ok, #event{json = #{<<"content">> := #{<<"algorithm">> := A} = C}}} when is_binary(A) ->
            C;
        _ ->
            undefined
    end.

is_encrypted(Data) ->
    try room_encryption(current_state_map(Data), Data) /= undefined
    catch _:_ -> false
    end.

%% The JID a local user writes to for this room: the peer's in a direct
%% room, the room JID she joined by in a room.
chat_jid(_From, #data{kind = #direct{remote_user = Remote}} = Data) when is_binary(Remote) ->
    case user_id_to_jid(Remote, Data) of
        #jid{} = J -> J;
        error -> Data#data.room_jid
    end;
chat_jid(From, #data{kind = #multi{users = Users}} = Data) ->
    {LUser, LServer, LResource} = jid:tolower(From),
    case Users of
        #{{LUser, LServer} := {online, #{LResource := #multi_user{room_jid = J}}}} -> J;
        _ -> Data#data.room_jid
    end;
chat_jid(_From, Data) ->
    Data#data.room_jid.

first_error(undefined, E) -> E;
first_error(E, _) -> E.

%% The error a refused message gets.
refused_message(From, MsgID, #data{kind = Kind} = Data) ->
    {LUser, LServer, LResource} = jid:tolower(From),
    Err = case Kind of
              #multi{users = #{{LUser, LServer} := {online, #{LResource := _}}}} ->
                  xmpp:err_forbidden(<<"The room's rules refuse your message">>, <<"en">>);
              #multi{} ->
                  xmpp:err_not_acceptable(<<"You are not an occupant of this room">>, <<"en">>);
              _ ->
                  xmpp:err_forbidden(<<"The room's rules refuse your message">>, <<"en">>)
          end,
    #message{id = MsgID, type = error, from = chat_jid(From, Data), to = From,
             sub_els = [Err]}.

%% Whether From's user is the local user of this direct room, or (a room)
%% any local user: who may act on the room through it.
owns_room(From, #data{kind = #direct{local_user = #jid{} = U}}) ->
    {From#jid.luser, From#jid.lserver} == {U#jid.luser, U#jid.lserver};
owns_room(_From, #data{kind = #multi{}}) ->
    true;
owns_room(_, _) ->
    false.

%% An <event/> from a local user into this room: added, then <sent/>, or
%% the error.
e2e_event(From, To, MsgID, Lang, Type, Content, Data) ->
    Host = Data#data.host,
    Error = fun(Err) ->
                    ejabberd_router:route(
                      #message{id = MsgID, type = error, from = To, to = From, lang = Lang,
                               sub_els = [Err]}),
                    Data
            end,
    case {owns_room(From, Data), user_id_from_jid(From, Host)} of
        {true, {ok, UserID}} ->
            case Type /= <<"m.room.encrypted">> andalso is_encrypted(Data) of
                true ->
                    Error(mod_matrix_gw_event:not_encrypted(Lang));
                false ->
                    try add_event(#{<<"type">> => Type, <<"sender">> => UserID,
                                    <<"content">> => Content},
                                  Data#data{e2e_sending = {UserID, MsgID}}) of
                        {Data2, Event} ->
                            ejabberd_router:route(
                              mod_matrix_gw_event:sent_message(
                                jid:make(mod_matrix_gw_opt:host(Host)), From, MsgID,
                                Event#event.id, Data#data.room_id)),
                            Data2#data{e2e_sending = undefined}
                    catch
                        error:{event_auth_error, _} ->
                            Error(xmpp:err_forbidden(<<"the room's rules refuse it">>, Lang));
                        error:{too_large, _} ->
                            Error(mod_matrix_gw_event:too_large(Lang));
                        Class:Reason:ST ->
                            ?INFO_MSG("failed <event/> ~s in ~s: ~p",
                                      [Type, Data#data.room_id, failure(Class, Reason, ST)]),
                            Error(xmpp:err_internal_server_error())
                    end
            end;
        _ ->
            Error(xmpp:err_not_allowed(<<"not this user's room">>, Lang))
    end.

%% The room iqs, answered from this room's state.
e2e_iq(#iq{from = From, lang = Lang} = IQ, Req, Data) ->
    Host = Data#data.host,
    {ok, UserID} = user_id_from_jid(From, Host),
    StateMap = current_state_map(Data),
    Member = case statemap_find({?ROOM_MEMBER, UserID}, StateMap, Data) of
                 {ok, #event{json = #{<<"content">> := #{<<"membership">> := M}}}} -> M;
                 _ -> <<"leave">>
             end,
    Info = fun(SM, D) ->
                   mod_matrix_gw_event:room_info_el(
                     #{id => D#data.room_id,
                       version => (D#data.room_version)#room_version.id,
                       encryption => room_encryption(SM, D),
                       direct => is_record(D#data.kind, direct),
                       power => get_user_power_level(UserID, SM, D),
                       %%  what the device judges a
                       %% redaction by: the level redact needs, each
                       %% member's level
                       redact => redact_level(SM, D),
                       members => [{M, S, get_user_power_level(M, SM, D)}
                                   || {M, S} <- state_members(SM, D)]})
           end,
    case Req of
        _ when not (Member == <<"join">> orelse Member == <<"invite">>) ->
            {xmpp:make_error(IQ, xmpp:err_item_not_found(<<"not a member of the room">>, Lang)), Data};
        _ ->
            case owns_room(From, Data) of
                false ->
                    {xmpp:make_error(IQ, xmpp:err_item_not_found(<<"no such room">>, Lang)), Data};
                true ->
                    e2e_iq(IQ, Req, UserID, StateMap, Info, Data)
            end
    end.

e2e_iq(IQ, {room_info}, _UserID, StateMap, Info, Data) ->
    {xmpp:make_iq_result(IQ, Info(StateMap, Data)), Data};
e2e_iq(#iq{lang = Lang} = IQ, {encryption, Alg}, UserID, StateMap, Info, Data) ->
    case {room_encryption(StateMap, Data), Alg} of
        {#{<<"algorithm">> := Alg}, _} ->
            {xmpp:make_iq_result(IQ, Info(StateMap, Data)), Data};
        {#{}, _} ->
            {xmpp:make_error(IQ, xmpp:err_not_acceptable(
                                   <<"the room is encrypted under another algorithm">>, Lang)),
             Data};
        {undefined, <<"m.megolm.v1.aes-sha2">>} ->
            try add_event(mod_matrix_gw_event:encryption_event(UserID), Data) of
                {Data2, _} ->
                    {xmpp:make_iq_result(IQ, Info(current_state_map(Data2), Data2)), Data2}
            catch
                error:{event_auth_error, _} ->
                    {xmpp:make_error(IQ, xmpp:err_forbidden(
                                           <<"your power level does not allow it">>, Lang)),
                     Data}
            end;
        {undefined, _} ->
            {xmpp:make_error(IQ, xmpp:err_not_acceptable(<<"unknown algorithm">>, Lang)), Data}
    end;
e2e_iq(#iq{lang = Lang} = IQ, {redact, Target, Reason}, UserID, StateMap, _Info, Data) ->
    RedactLevel =
        case statemap_find({?ROOM_POWER_LEVELS, <<"">>}, StateMap, Data) of
            {ok, #event{json = #{<<"content">> := #{<<"redact">> := L}}}} -> get_int(L);
            _ -> 50
        end,
    Power = get_user_power_level(UserID, StateMap, Data) >= RedactLevel,
    %% a target the room no longer holds is still
    %% the user's own by matrix_sent, or anyone's for one who has the power
    %% (the redaction's auth rules need no target)
    Allowed = case {target_sender(Target, Data), Target} of
                  {{ok, TargetSender}, _} -> TargetSender == UserID orelse Power;
                  {error, <<$$, _/binary>>} when Power -> true;
                  {error, _} -> unknown
              end,
    case Allowed of
        unknown ->
            {xmpp:make_error(IQ, xmpp:err_item_not_found(<<"no such event in the room">>, Lang)),
             Data};
        false ->
            {xmpp:make_error(IQ, xmpp:err_forbidden(
                                   <<"not your message, and no power to redact">>, Lang)),
             Data};
        true ->
            Content = case Reason of
                          <<>> -> mod_matrix_gw_xmpp:redaction_content(Target);
                          _ -> (mod_matrix_gw_xmpp:redaction_content(Target))#{<<"reason">> => Reason}
                      end,
            try add_event(#{<<"type">> => ?ROOM_REDACTION, <<"sender">> => UserID,
                            <<"redacts">> => Target, <<"content">> => Content}, Data) of
                {Data2, Event} ->
                    {xmpp:make_iq_result(IQ, mod_matrix_gw_event:sent_el(Event#event.id)),
                     Data2}
            catch
                error:{event_auth_error, _} ->
                    {xmpp:make_error(IQ, xmpp:err_forbidden(
                                           <<"the room's rules refuse it">>, Lang)),
                     Data}
            end
    end.

%% The forwarded set (mod_matrix_gw_event:forwarded/3), before the
%% gateway's own mapping: Data when the event was this path's, pass when
%% it is the mapping's. Every membership change goes to mod_matrix_gw_e2e
%% too.
e2e_notify(#event{type = Type, state_key = StateKey, json = JSON} = Event, Data) ->
    Content = case JSON of
                  #{<<"content">> := C} when is_map(C) -> C;
                  _ -> #{}
              end,
    case {Type, StateKey, Content} of
        {?ROOM_MEMBER, <<$@, _/binary>>, #{<<"membership">> := M}} when is_binary(M) ->
            mod_matrix_gw_event:membership(Data#data.host, Data#data.room_id, StateKey, M);
        _ ->
            ok
    end,
    Kind = case Data#data.kind of
               #direct{local_user = #jid{}} -> direct;
               #multi{} -> multi;
               _ -> none
           end,
    case Kind of
        none ->
            pass;
        _ ->
            case mod_matrix_gw_event:forwarded(Type, Content, Kind) of
                pass ->
                    pass;
                What ->
                    try e2e_forward(What, Event, Content, Data)
                    catch Class:Reason:ST ->
                            ?INFO_MSG("failed to forward ~s ~s in ~s: ~p",
                                      [Type, Event#event.id, Data#data.room_id,
                                       failure(Class, Reason, ST)]),
                            Data
                    end
            end
    end.

e2e_forward(What, #event{type = Type, id = EventID, sender = Sender,
                         origin_server_ts = TS, state_key = StateKey, json = JSON} = Event,
            Content, Data) ->
    RoomID = Data#data.room_id,
    Redacts = redacts(JSON),
    Fields0 = #{type => Type, id => EventID, room => RoomID, sender => Sender, ts => TS,
                content => Content},
    Fields1 = case StateKey of
                  undefined -> Fields0;
                  _ -> Fields0#{state_key => StateKey}
              end,
    Fields = case Type of
                 ?ROOM_REDACTION -> Fields1#{redacts => Redacts};
                 _ -> Fields1
             end,
    %% a redaction: the retraction of the message it takes back, unless the
    %% target is a reaction (whose new set the reactions' mapping sends) or
    %% the redactor may not take it back (the server applies no such one).
    %% A target the room no longer holds but a local user sent from XMPP is
    %% hers (matrix_sent). One nobody here knows (older than
    %% the gateway's memory, or an id the redactor made up, an XMPP id among
    %% them) goes on as its <event/> for the device, with no XEP-0424
    %% retraction of anything, and only when
    %% the redactor has the room's redact power: the half of the rule that
    %% needs no target (Synapse does the same without it)
    {Applies, TargetSender, IsReaction, Known} =
        case What of
            redaction ->
                case maps:find(Redacts, Data#data.events) of
                    {ok, #event{type = TT, sender = TS0} = Target} ->
                        {redaction_applies(Event, Target, Data), TS0, TT == ?REACTION, true};
                    error ->
                        case target_sender(Redacts, Data) of
                            {ok, TS1} ->
                                {redaction_applies_to(Event, TS1, Data), TS1,
                                 false, true};
                            error ->
                                {redactor_power(Event, Data), undefined, false, false}
                        end
                end;
            forward ->
                {true, undefined, false, true}
        end,
    case Applies of
        false when not Known ->
            ?INFO_MSG("redaction ~s of ~s in ~s by ~s, who has no power to redact "
                      "an event the gateway does not know: dropped",
                      [EventID, Redacts, RoomID, Sender]),
            Data;
        false ->
            ?DEBUG("redaction ~s of ~s in ~s does not apply", [EventID, Redacts, RoomID]),
            Data;
        true ->
            Retract = fun(Mode) ->
                              case What == redaction andalso not IsReaction andalso Known of
                                  false -> #{};
                                  true when Mode == chat -> #{retract => xmpp_id_of(Redacts, Data)};
                                  true -> #{retract => Redacts}
                              end
                      end,
            Moderated = TargetSender /= undefined andalso TargetSender /= Sender,
            case Data#data.kind of
                #direct{local_user = UserJID} ->
                    case user_id_to_jid(Sender, Data) of
                        #jid{} = SenderJID ->
                            case jid:remove_resource(jid:tolower(SenderJID)) ==
                                jid:remove_resource(jid:tolower(UserJID)) of
                                true ->
                                    Data;
                                false ->
                                    %% the peer's redaction of
                                    %% her message is XEP-0425's, by him (a
                                    %% plain client applies XEP-0424 only
                                    %% from the message's sender)
                                    By = case Moderated of
                                             true -> SenderJID;
                                             false -> undefined
                                         end,
                                    Msg = mod_matrix_gw_event:forward_message(
                                            #message{id = EventID, from = SenderJID, to = UserJID,
                                                     type = chat},
                                            Fields, Retract(chat), By),
                                    ejabberd_router:route(maybe_delay(Msg, Event, SenderJID)),
                                    Data
                            end;
                        error ->
                            Data
                    end;
                #multi{users = Users} ->
                    <<$@, Nick/binary>> = Sender,
                    ID = xmpp_id_of(EventID, Data),
                    %% the sender's XEP-0421 occupant id, as on every
                    %% groupchat message of the room (the rooms' merge)
                    Occupant = mod_matrix_gw_muc:occupant_id_el(RoomID, Sender),
                    maps:foreach(
                      fun({LUser, LServer}, {online, Resources}) ->
                              maps:foreach(
                                fun(LResource, #multi_user{join_ts = JoinTS, room_jid = RoomJID})
                                      when JoinTS =< TS ->
                                        %% a moderator's redaction comes from the
                                        %% room itself (XEP-0425), By her
                                        %% occupant JID; anything else from the
                                        %% sender's occupant JID with her id
                                        Occupant0 = jid:replace_resource(RoomJID, Nick),
                                        {From, By, OccupantEl} =
                                            case Moderated of
                                                true -> {RoomJID, Occupant0, []};
                                                false -> {Occupant0, undefined, [Occupant]}
                                            end,
                                        Msg = mod_matrix_gw_event:forward_message(
                                                #message{id = ID, from = From, type = groupchat,
                                                         to = jid:make(LUser, LServer, LResource),
                                                         sub_els = [#stanza_id{by = RoomJID,
                                                                               id = EventID}
                                                                    | OccupantEl]},
                                                Fields,
                                                (Retract(groupchat))#{occupant_els => [Occupant]},
                                                By),
                                        ejabberd_router:route(maybe_delay(Msg, Event, RoomJID));
                                   (_, _) ->
                                        ok
                                end, Resources);
                         (_, _) ->
                              ok
                      end, Users),
                    Data
            end
    end.

%% Whether a redaction takes its target back: the redactor's power reaches
%% the room's `redact` level, or it comes from the target sender's server
%% (room versions 3 and later).
%% The power level m.room.redaction of another's
%% event needs in the state StateMap (the spec's default 50).
redact_level(StateMap, Data) ->
    case statemap_find({?ROOM_POWER_LEVELS, <<"">>}, StateMap, Data) of
        {ok, #event{json = #{<<"content">> := #{<<"redact">> := L}}}} -> get_int(L);
        _ -> 50
    end.

redaction_applies(Redaction, #event{sender = TargetSender}, Data) ->
    redaction_applies_to(Redaction, TargetSender, Data).

redaction_applies_to(#event{sender = Sender} = Redaction, TargetSender, Data) ->
    SameServer = (catch mod_matrix_gw:get_id_domain_exn(Sender)) ==
        (catch mod_matrix_gw:get_id_domain_exn(TargetSender)),
    SameServer orelse redactor_power(Redaction, Data).

%% Whether the redactor's power reaches the room's `redact` level (in the
%% state before the redaction, else the current one).
redactor_power(#event{sender = Sender} = Redaction, Data) ->
        try
            StateMap = case Redaction#event.state_map of
                           SM when is_map(SM) -> SM;
                           _ -> current_state_map(Data)
                       end,
            RedactLevel =
                case statemap_find({?ROOM_POWER_LEVELS, <<"">>}, StateMap, Data) of
                    {ok, #event{json = #{<<"content">> := #{<<"redact">> := L}}}} -> get_int(L);
                    _ -> 50
                end,
            get_user_power_level(Sender, StateMap, Data) >= RedactLevel
        catch _:_ ->
                false
        end.

%% A redaction stored: the gateway's copy of its target pruned, as the
%% spec has a server do (the device and the account's archive keep theirs:
%% never a deletion on the XMPP side), and gone from the MUC history.
prune_redacted(#event{type = ?ROOM_REDACTION, json = JSON} = Redaction, Data) ->
    Target = redacts(JSON),
    case maps:find(Target, Data#data.events) of
        {ok, #event{json = TJSON, state_key = SK, state_map = SM} = TEvent} ->
            case redaction_applies(Redaction, TEvent, Data) of
                true ->
                    Pruned = mod_matrix_gw:prune_event(TJSON, Data#data.room_version),
                    Events = (Data#data.events)#{Target => TEvent#event{json = Pruned}},
                    Latest = Data#data.latest_events,
                    case SK /= undefined orelse sets:is_element(Target, Latest) of
                        true ->
                            catch mod_matrix_gw_store:save_events(
                                    Data#data.room_id, [{Target, Pruned, SM}],
                                    sets:to_list(Latest));
                        false ->
                            ok
                    end,
                    Data#data{events = Events,
                              backlog = lists:keydelete(Target, 2, Data#data.backlog)};
                false ->
                    Data
            end;
        error ->
            Data
    end;
prune_redacted(_Event, Data) ->
    Data.

%%--------------------------------------------------------------------
%% A Matrix room as a complete MUC room
%% (mod_matrix_gw_muc has what does not need the room's state)
%%--------------------------------------------------------------------

%% The content of the state event {Type, Key} in StateMap, #{} without one.
state_content(Type, Key, StateMap, Data) ->
    case statemap_find({Type, Key}, StateMap, Data) of
        {ok, #event{json = #{<<"content">> := C}}} when is_map(C) -> C;
        _ -> #{}
    end.

%% The member's content before Event, #{} when she was none, undefined
%% when the room cannot tell.
prev_member_content(#event{state_key = StateKey} = Event, Data) ->
    try get_event_prev_state_map(Event, Data) of
        Prev -> state_content(?ROOM_MEMBER, StateKey, Prev, Data)
    catch _:_ ->
            undefined
    end.

%% The members whose membership in StateMap is join.
joined_members(StateMap, Data) ->
    [U || {?ROOM_MEMBER, U} <- maps:keys(StateMap),
          maps:get(<<"membership">>, state_content(?ROOM_MEMBER, U, StateMap, Data), <<>>)
              == <<"join">>].

%% Fun(UserJID, RoomJID, {LUser, LServer}) for every resource in the room
%% that joined before Event (the rule of 26.09's message clause).
for_occupants(#event{origin_server_ts = TS}, Data, Fun) ->
    for_occupants_at(TS, Data, Fun).

for_occupants_at(TS, #data{kind = #multi{users = Users}}, Fun) ->
    maps:foreach(
      fun(US, {online, Resources}) ->
              maps:foreach(
                fun(LResource, #multi_user{join_ts = JoinTS, room_jid = RoomJID}) when JoinTS =< TS ->
                        {LUser, LServer} = US,
                        Fun(jid:make(LUser, LServer, LResource), RoomJID, US);
                   (_, _) ->
                        ok
                end, Resources);
         (_, _) ->
              ok
      end, Users);
for_occupants_at(_, _, _) ->
    ok.

%% The bare JID of a local user id, {LUser, LServer}; none for another's.
local_us(UserID, Data) ->
    Host = Data#data.host,
    case user_id_to_jid(UserID, Data) of
        #jid{lserver = Host, luser = U} -> {U, Host};
        _ -> none
    end.

%% An occupant's available presence: her item (with her JID: the room is
%% not anonymous), XEP-0421 occupant-id and XEP-0172 display name.
occupant_presence(UserID, StateMap, RoomJID, To, Codes, Data) ->
    Item = get_user_muc_item(UserID, StateMap, Data),
    JID = case user_id_to_jid(UserID, Data) of
              #jid{} = J -> J;
              error -> undefined
          end,
    Content = state_content(?ROOM_MEMBER, UserID, StateMap, Data),
    #presence{from = jid:replace_resource(RoomJID, mod_matrix_gw_muc:nick(UserID)),
              to = To, type = available,
              sub_els = [#muc_user{items = [Item#muc_item{jid = JID}], status_codes = Codes},
                         mod_matrix_gw_muc:occupant_id_el(Data#data.room_id, UserID)
                         | mod_matrix_gw_muc:nick_el(maps:get(<<"displayname">>, Content, undefined))]}.

%% A join: the occupants have the new occupant's presence; the joining
%% user's resources their occupant list, own presence (110, and 201 when
%% the join made the room), the room's avatar, history and subject, once.
notify_join(#event{state_key = UserID, origin_server_ts = OriginTS,
                   state_map = StateMap} = Event,
            #data{kind = #multi{users = Users} = Kind} = Data) ->
    Self = local_us(UserID, Data),
    Users2 =
        maps:map(
          fun(US, {online, Resources}) ->
                  {online,
                   maps:map(
                     fun(LResource, #multi_user{join_ts = JoinTS, room_jid = RoomJID} = MU)
                           when JoinTS =< OriginTS ->
                             {LUser, LServer} = US,
                             UserJID = jid:make(LUser, LServer, LResource),
                             case US of
                                 Self ->
                                     send_initial_presences(UserJID, RoomJID,
                                                            Event#event{sender = UserID}, Data),
                                     Codes = [110] ++ [201 || MU#multi_user.created]
                                         ++ nick_codes(MU, UserID),
                                     ejabberd_router:route(
                                       occupant_presence(UserID, StateMap, RoomJID, UserJID, Codes, Data)),
                                     send_avatar_presence(UserJID, RoomJID, Data),
                                     case MU#multi_user.history of
                                         N when is_integer(N) ->
                                             send_history(UserJID, RoomJID, MU, JoinTS, Data),
                                             send_subject(UserJID, RoomJID, StateMap, Data),
                                             MU#multi_user{history = sent};
                                         sent ->
                                             MU
                                     end;
                                 _ ->
                                     ejabberd_router:route(
                                       occupant_presence(UserID, StateMap, RoomJID, UserJID, [], Data)),
                                     MU
                             end;
                        (_, MU) ->
                             MU
                     end, Resources)};
             (_, V) ->
                  V
          end, Users),
    fetch_avatar(StateMap, Data#data{kind = Kind#multi{users = Users2}}).

%% An occupant's presence again (her name, avatar, affiliation or role
%% changed).
notify_occupant(UserID, #event{state_map = StateMap} = Event, Data) ->
    Self = local_us(UserID, Data),
    for_occupants(Event, Data,
                  fun(UserJID, RoomJID, US) ->
                          ejabberd_router:route(
                            occupant_presence(UserID, StateMap, RoomJID, UserJID,
                                              [110 || US == Self], Data))
                  end).

%% A member gone (leave, kick, ban): her unavailable presence to every
%% occupant, with the codes of a kick or a ban, the actor and the reason;
%% a local user made to go is out of the room, all her resources.
notify_gone(Change, #event{sender = Sender, state_key = UserID, origin_server_ts = OriginTS,
                           state_map = StateMap, json = #{<<"content">> := Content}} = Event,
            #data{kind = #multi{users = Users}} = Data) ->
    Self = local_us(UserID, Data),
    Item0 = get_user_muc_item(UserID, StateMap, Data),
    Item = (mod_matrix_gw_muc:change_item(Change, Item0, Sender, UserID, Content))#muc_item{
             jid = case user_id_to_jid(UserID, Data) of #jid{} = J -> J; _ -> undefined end},
    for_occupants(Event, Data,
                  fun(UserJID, RoomJID, US) ->
                          ejabberd_router:route(
                            #presence{from = jid:replace_resource(RoomJID, mod_matrix_gw_muc:nick(UserID)),
                                      to = UserJID, type = unavailable,
                                      sub_els = [#muc_user{items = [Item],
                                                           status_codes = mod_matrix_gw_muc:change_codes(
                                                                            Change, US == Self)},
                                                 mod_matrix_gw_muc:occupant_id_el(Data#data.room_id, UserID)]})
                  end),
    case Users of
        #{Self := {online, Resources}} ->
            JoinTS = maps:fold(fun(_, #multi_user{join_ts = TS}, Acc) -> max(Acc, TS) end, 0, Resources),
            if
                JoinTS =< OriginTS ->
                    Data#data{kind = (Data#data.kind)#multi{users = maps:remove(Self, Users)}};
                true ->
                    Data
            end;
        _ ->
            Data
    end.

%% A local user invited by a local user: a mediated invite (XEP-0045
%% §7.8.2) from the room as the inviter joined it. A remote inviter's
%% invite came through /invite already (send_muc_invite/7).
notify_invite(#event{sender = Sender, state_key = UserID, json = #{<<"content">> := Content}},
              #data{kind = #multi{users = Users}} = Data) ->
    Host = Data#data.host,
    case {local_us(Sender, Data), user_id_to_jid(UserID, Data)} of
        {{_, _} = SenderUS, #jid{lserver = Host} = UserJID} ->
            RoomJID = case Users of
                          #{SenderUS := {online, Rs}} when map_size(Rs) > 0 ->
                              [#multi_user{room_jid = R} | _] = maps:values(Rs),
                              R;
                          _ ->
                              Data#data.room_jid
                      end,
            Reason = case maps:get(<<"reason">>, Content, <<>>) of R2 when is_binary(R2) -> R2; _ -> <<>> end,
            ejabberd_router:route(
              #message{id = p1_rand:get_string(), from = RoomJID, to = UserJID,
                       sub_els = [#muc_user{invites = [#muc_invite{from = user_id_to_jid(Sender, Data),
                                                                   reason = Reason}]}]});
        _ ->
            ok
    end.

%% New power levels: the presence of every member whose affiliation or
%% role they change.
notify_power_levels(#event{state_map = New} = Event, Data) ->
    try get_event_prev_state_map(Event, Data) of
        Prev ->
            lists:foreach(
              fun(UserID) ->
                      case get_user_muc_item(UserID, Prev, Data) ==
                          get_user_muc_item(UserID, New, Data) of
                          true -> ok;
                          false -> notify_occupant(UserID, Event, Data)
                      end
              end, joined_members(New, Data))
    catch _:_ ->
            ok
    end.

%% A local user's invite of a remote user: the invite goes to the
%% invitee's server (/v2/invite) with the room's stripped state, beside
%% the transaction to the servers in the room. The direct room's own
%% invite is 26.09's (notify_event_matrix/2).
maybe_send_invite(#event{type = ?ROOM_MEMBER, sender = Sender, state_key = UserID, state_map = SM,
                         json = #{<<"content">> := #{<<"membership">> := <<"invite">>}}} = Event,
                  #data{kind = #multi{}} = Data) when is_map(SM) ->
    Host = Data#data.host,
    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
    case {local_us(Sender, Data), catch mod_matrix_gw:get_id_domain_exn(UserID)} of
        {{_, _}, Server} when is_binary(Server), Server /= MatrixServer ->
            Keys = [{?ROOM_MEMBER, Sender} | mod_matrix_gw_muc:stripped_state_keys()],
            Stripped =
                [maps:with([<<"sender">>, <<"type">>, <<"state_key">>, <<"content">>], E#event.json)
                 || K <- Keys, {ok, E} <- [statemap_find(K, Event#event.state_map, Data)]],
            JSON = #{<<"event">> => Event#event.json,
                     <<"room_version">> => (Data#data.room_version)#room_version.id,
                     <<"invite_room_state">> => Stripped},
            RoomID = Data#data.room_id,
            %% not in the room's process: the invitee's server may be slow
            %% or away, the room goes on
            spawn(fun() ->
                          Res = (catch mod_matrix_gw:send_request(
                                         Host, put, Server,
                                         [<<"_matrix">>, <<"federation">>, <<"v2">>, <<"invite">>,
                                          RoomID, Event#event.id],
                                         [], JSON, [{timeout, 10000}],
                                         [{sync, true}, {body_format, binary}])),
                          case Res of
                              {ok, {{_, 200, _}, _, _}} -> ok;
                              _ -> ?INFO_MSG("invite of ~s to ~s refused: ~p", [UserID, RoomID, Res])
                          end
                  end),
            ok;
        _ ->
            ok
    end;
maybe_send_invite(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% From XMPP to a room
%%--------------------------------------------------------------------

%% Fun(Pid) with the process of the room RoomJID names, if the gateway is
%% in it; else the stanza's error.
with_room(#jid{luser = LUser}, Pkt, Fun) ->
    Host = ejabberd_config:get_myname(),
    case catch room_id_from_xmpp(Host, LUser, xmpp:get_from(Pkt)) of
        {ok, RoomID, _} ->
            case get_existing_room_pid(Host, RoomID) of
                {ok, Pid} -> Fun(Pid);
                _ -> ejabberd_router:route_error(Pkt, xmpp:err_item_not_found())
            end;
        _ ->
            ejabberd_router:route_error(Pkt, xmpp:err_item_not_found())
    end.

%% A room made for a local user's join of #name%<our domain>.
create_room(Host, From, Packet, Alias) ->
    RoomID = new_room_id(),
    case get_room_pid(Host, RoomID) of
        {ok, Pid} ->
            mod_matrix_gw_muc:alias_set(Alias, RoomID),
            gen_statem:cast(Pid, {create_multi, From, Packet, Alias});
        {error, Reason} ->
            ?INFO_MSG("no room for ~s: ~p", [Alias, Reason]),
            ejabberd_router:route_error(Packet, xmpp:err_internal_server_error())
    end.

%% /query/directory: a room alias of ours, while the gateway is in the room.
directory(Host, Alias) ->
    case mod_matrix_gw_muc:alias_lookup(Alias) of
        {ok, RoomID} ->
            case get_existing_room_pid(Host, RoomID) of
                {ok, _} -> {ok, RoomID, [mod_matrix_gw_opt:matrix_domain(Host)]};
                _ -> error
            end;
        error ->
            error
    end.

%% An event of a local user, checked by the room's auth rules before it is
%% signed into the room (add_event/2), so the federation never sees one the
%% rules refuse: {ok, Data2} or {error, forbidden}.
local_add(JSON, Data) ->
    try add_event(JSON, Data) of
        {Data2, _Event} -> {ok, Data2}
    catch
        error:{event_auth_error, _} ->
            {error, forbidden};
        error:{too_large, _} ->
            {error, 'not-acceptable'};
        Class:Reason:ST ->
            ?INFO_MSG("failed ~s in ~s: ~p", [maps:get(<<"type">>, JSON, <<>>), Data#data.room_id,
                                              failure(Class, Reason, ST)]),
            {error, internal}
    end.

local_adds([], Data) ->
    {ok, Data};
local_adds([JSON | JSONs], Data) ->
    case local_add(JSON, Data) of
        {ok, Data2} -> local_adds(JSONs, Data2);
        Error -> Error
    end.

xmpp_error(forbidden) -> xmpp:err_forbidden();
xmpp_error('not-acceptable') -> xmpp:err_not_acceptable();
xmpp_error('feature-not-implemented') -> xmpp:err_feature_not_implemented();
xmpp_error('item-not-found') -> xmpp:err_item_not_found();
xmpp_error('jid-malformed') -> xmpp:err_jid_malformed();
xmpp_error('bad-request') -> xmpp:err_bad_request();
xmpp_error(_) -> xmpp:err_internal_server_error().

%% The user id of a local member of the room From is a resource of.
room_user(#jid{} = From, Data) ->
    case user_id_from_jid(From, Data#data.host) of
        {ok, UserID} when From#jid.lserver == Data#data.host ->
            case is_joined_member(UserID, Data) of
                true -> {ok, UserID};
                false -> {error, forbidden}
            end;
        _ ->
            {error, forbidden}
    end.

member_json(Sender, UserID, Membership, Reason) ->
    #{<<"type">> => ?ROOM_MEMBER, <<"sender">> => Sender, <<"state_key">> => UserID,
      <<"content">> => maps:merge(#{<<"membership">> => Membership},
                                  case Reason of
                                      <<>> -> #{};
                                      _ -> #{<<"reason">> => Reason}
                                  end)}.

state_json(Type, Sender, Content) ->
    #{<<"type">> => Type, <<"sender">> => Sender, <<"state_key">> => <<"">>,
      <<"content">> => Content}.

%% A join the room's rules refuse (not invited, banned): the presence's
%% error, and the resource is not in the room.
join_member(UserJID, JSON, Packet, Data0, Data) ->
    case local_add(JSON, Data) of
        {ok, Data2} ->
            {keep_state, Data2, [{next_event, internal, update_client}]};
        {error, Why} ->
            #{<<"state_key">> := UserID} = JSON,
            Membership = maps:get(<<"membership">>,
                                  state_content(?ROOM_MEMBER, UserID, current_state_map(Data0), Data0),
                                  <<"leave">>),
            Err = case {Why, Membership} of
                      {forbidden, <<"ban">>} -> xmpp:err_forbidden(<<"You are banned from this room">>, <<"en">>);
                      {forbidden, _} -> xmpp:err_registration_required(<<"You are not invited to this room">>, <<"en">>);
                      _ -> xmpp:err_internal_server_error()
                  end,
            ?DEBUG("join of ~s refused: ~p", [jid:encode(UserJID), Why]),
            ejabberd_router:route_error(Packet, Err),
            {keep_state, Data0, [{next_event, internal, update_client}]}
    end.

%% Iqs to the room JID: disco#info, the admin and owner requests of
%% XEP-0045, the room's vCard, XEP-0425 moderation. An answer, or noreply
%% when the answer comes later (the avatar being fetched).
room_iq(#iq{type = set, from = From, sub_els = [#xmlel{} = El]} = IQ, Data)
  when El#xmlel.name == <<"moderate">>; El#xmlel.name == <<"apply-to">> ->
    case {mod_matrix_gw_muc:moderate_request(El), room_user(From, Data)} of
        {{ok, ID, Reason}, {ok, UserID}} -> moderate(IQ, ID, Reason, UserID, Data);
        {error, _} -> {xmpp:make_error(IQ, xmpp:err_bad_request()), Data};
        {_, {error, Why}} -> {xmpp:make_error(IQ, xmpp_error(Why)), Data}
    end;
room_iq(#iq{type = Type, from = From, sub_els = [_]} = IQ0, Data) ->
    try xmpp:decode_els(IQ0) of
        #iq{sub_els = [SubEl]} = IQ ->
            case {Type, SubEl} of
                {get, #disco_info{node = <<>>}} ->
                    {xmpp:make_iq_result(IQ, room_disco_info(IQ, Data)), Data};
                {get, #disco_items{node = <<>>}} ->
                    {xmpp:make_iq_result(IQ, #disco_items{}), Data};
                {get, #muc_admin{items = [#muc_item{} = Item]}} ->
                    %% the lists are the room's
                    %% members' (the member list), its moderators' (the
                    %% others), as XEP-0045 has them: real JIDs inside
                    with_user(IQ, From, Data,
                              fun(UserID) ->
                                      case may_list(Item, UserID, Data) of
                                          true ->
                                              {xmpp:make_iq_result(IQ, #muc_admin{items = admin_list(Item, Data)}),
                                               Data};
                                          false ->
                                              {xmpp:make_error(IQ, xmpp:err_forbidden(
                                                                     <<"Only a moderator may ask for this list">>,
                                                                     IQ#iq.lang)), Data}
                                      end
                              end);
                {set, #muc_admin{items = Items}} when Items /= [] ->
                    with_user(IQ, From, Data, fun(UserID) -> admin_set(IQ, Items, UserID, Data) end);
                {get, #muc_owner{config = undefined, destroy = undefined}} ->
                    with_user(IQ, From, Data,
                              fun(_UserID) ->
                                      StateMap = current_state_map(Data),
                                      {xmpp:make_iq_result(
                                         IQ, #muc_owner{config = mod_matrix_gw_muc:config_form(
                                                                   room_name(StateMap, Data),
                                                                   room_topic(StateMap, Data))}),
                                       Data}
                              end);
                {set, #muc_owner{config = #xdata{} = X, destroy = undefined}} ->
                    with_user(IQ, From, Data, fun(UserID) -> config_set(IQ, X, UserID, Data) end);
                {get, #vcard_temp{}} when From#jid.lserver == Data#data.host ->
                    %% the local users' (it may
                    %% fetch the avatar)
                    room_vcard(IQ, Data);
                {get, #vcard_temp{}} ->
                    {xmpp:make_error(IQ, xmpp:err_forbidden(<<"Only for the users of this server">>,
                                                            IQ#iq.lang)), Data};
                _ ->
                    {xmpp:make_error(IQ, xmpp:err_service_unavailable()), Data}
            end
    catch _:{xmpp_codec, Why} ->
            {xmpp:make_error(IQ0, xmpp:err_bad_request(xmpp:io_format_error(Why), IQ0#iq.lang)), Data}
    end;
room_iq(IQ, Data) ->
    {xmpp:make_error(IQ, xmpp:err_bad_request()), Data}.

%% Whether UserID, a joined local member, may have the list Item asks
%% for: the member list any member, the others a moderator.
may_list(#muc_item{affiliation = member, role = undefined}, _UserID, _Data) ->
    true;
may_list(_Item, UserID, Data) ->
    case get_user_muc_item(UserID, current_state_map(Data), Data) of
        #muc_item{role = moderator} -> true;
        _ -> false
    end.

with_user(IQ, From, Data, Fun) ->
    case room_user(From, Data) of
        {ok, UserID} -> Fun(UserID);
        {error, Why} -> {xmpp:make_error(IQ, xmpp_error(Why)), Data}
    end.

room_name(StateMap, Data) ->
    case state_content(?ROOM_NAME, <<"">>, StateMap, Data) of
        #{<<"name">> := N} when is_binary(N), N /= <<>> -> N;
        _ ->
            case state_content(?ROOM_CANONICAL_ALIAS, <<"">>, StateMap, Data) of
                #{<<"alias">> := A} when is_binary(A) -> A;
                _ -> <<>>
            end
    end.

room_topic(StateMap, Data) ->
    case state_content(?ROOM_TOPIC, <<"">>, StateMap, Data) of
        #{<<"topic">> := T} when is_binary(T) -> T;
        _ -> <<>>
    end.

room_disco_info(#iq{from = From, to = To}, Data) ->
    StateMap = current_state_map(Data),
    Levels = mod_matrix_gw_muc:levels(state_content(?ROOM_POWER_LEVELS, <<"">>, StateMap, Data)),
    Moderate = case user_id_from_jid(From, Data#data.host) of
                   {ok, UserID} ->
                       mod_matrix_gw_muc:can_redact(
                         Levels, get_user_power_level(UserID, StateMap, Data));
                   _ ->
                       false
               end,
    Avatar = case Data#data.avatar of
                 {_, Hash, _, _} when is_binary(Hash) -> Hash;
                 _ -> undefined
             end,
    mod_matrix_gw_muc:disco_info(
      #{name => room_name(StateMap, Data), topic => room_topic(StateMap, Data),
        join_rule => maps:get(<<"join_rule">>, state_content(?ROOM_JOIN_RULES, <<"">>, StateMap, Data),
                              <<"invite">>),
        occupants => length(joined_members(StateMap, Data)), moderate => Moderate,
        avatar_hash => Avatar,
        features => mod_matrix_gw_call:disco_features(To) ++ mod_matrix_gw_event:disco_features(To) ++
            mod_matrix_gw_rtc:disco_features(To)}).

%% An affiliation or role list (XEP-0045 §9.5, §8.5, ...) from the state:
%% its members and the users the power levels name.
admin_list(#muc_item{affiliation = Aff, role = Role}, Data) ->
    StateMap = current_state_map(Data),
    PLUsers = maps:keys(maps:get(<<"users">>, state_content(?ROOM_POWER_LEVELS, <<"">>, StateMap, Data), #{})),
    Users = lists:usort([U || {?ROOM_MEMBER, U} <- maps:keys(StateMap)] ++ PLUsers),
    lists:filtermap(
      fun(UserID) ->
              Item = get_user_muc_item(UserID, StateMap, Data),
              Match = case {Aff, Role} of
                          {undefined, R} -> Item#muc_item.role == R andalso R /= none;
                          {A, _} -> Item#muc_item.affiliation == A andalso A /= none
                      end,
              case {Match, user_id_to_jid(UserID, Data)} of
                  {true, #jid{} = JID} ->
                      {true, Item#muc_item{jid = JID, nick = mod_matrix_gw_muc:nick(UserID)}};
                  _ ->
                      false
              end
      end, Users).

%% XEP-0045 admin requests as Matrix events of the requester: a kick, a ban
%% or unban, an invite or a revoked one, a power level; the room's rules
%% (the requester's power) decide, before anything reaches the federation.
admin_set(IQ, Items, UserID, Data) ->
    Host = Data#data.host,
    Levels = mod_matrix_gw_muc:levels(
               state_content(?ROOM_POWER_LEVELS, <<"">>, current_state_map(Data), Data)),
    Res = lists:foldl(
            fun(_, {error, _} = Err) ->
                    Err;
               (Item, {ok, D}) ->
                    case mod_matrix_gw_muc:target_user_id(Item, fun(J) -> user_id_from_jid(J, Host) end) of
                        {ok, Target} ->
                            SM = current_state_map(D),
                            Membership = maps:get(<<"membership">>,
                                                  state_content(?ROOM_MEMBER, Target, SM, D), <<"leave">>),
                            Level = get_user_power_level(Target, SM, D),
                            case mod_matrix_gw_muc:admin_action(Item, Levels, Level, Membership) of
                                {member, M, Reason} ->
                                    local_add(member_json(UserID, Target, M, Reason), D);
                                {level, L} ->
                                    PL2 = mod_matrix_gw_muc:set_user_level(
                                            state_content(?ROOM_POWER_LEVELS, <<"">>, SM, D), Target, L),
                                    local_add(state_json(?ROOM_POWER_LEVELS, UserID, PL2), D);
                                ok ->
                                    {ok, D};
                                {error, _} = Err ->
                                    Err
                            end;
                        error ->
                            {error, 'jid-malformed'}
                    end
            end, {ok, Data}, Items),
    case Res of
        {ok, Data2} -> {xmpp:make_iq_result(IQ), Data2};
        {error, Why} -> {xmpp:make_error(IQ, xmpp_error(Why)), Data}
    end.

%% The configuration form submitted: the room's name and topic.
config_set(IQ, X, UserID, Data) ->
    StateMap = current_state_map(Data),
    JSONs = lists:flatmap(
              fun({name, Name}) ->
                      case state_content(?ROOM_NAME, <<"">>, StateMap, Data) of
                          #{<<"name">> := Name} -> [];
                          _ -> [state_json(?ROOM_NAME, UserID, #{<<"name">> => Name})]
                      end;
                 ({topic, Topic}) ->
                      case room_topic(StateMap, Data) of
                          Topic -> [];
                          _ -> [state_json(?ROOM_TOPIC, UserID, #{<<"topic">> => Topic})]
                      end
              end, mod_matrix_gw_muc:config_changes(X)),
    case X of
        #xdata{type = cancel} ->
            {xmpp:make_iq_result(IQ), Data};
        _ ->
            case local_adds(JSONs, Data) of
                {ok, Data2} -> {xmpp:make_iq_result(IQ), Data2};
                {error, Why} -> {xmpp:make_error(IQ, xmpp_error(Why)), Data}
            end
    end.

%% XEP-0425: a moderator's retraction of another's message (its stanza-id,
%% the event id), or her own, as an m.room.redaction; redacting another's
%% event takes the redact level (the event's own auth rules do not check
%% it from room version 3 on, the servers applying it do).
moderate(IQ, ID, Reason, UserID, Data) ->
    case event_id_of(ID, UserID, Data) of
        undefined ->
            {xmpp:make_error(IQ, xmpp:err_item_not_found()), Data};
        Target ->
            StateMap = current_state_map(Data),
            Levels = mod_matrix_gw_muc:levels(state_content(?ROOM_POWER_LEVELS, <<"">>, StateMap, Data)),
            Allowed = case target_sender(Target, Data) of
                          {ok, UserID} -> true;
                          _ -> mod_matrix_gw_muc:can_redact(
                                 Levels, get_user_power_level(UserID, StateMap, Data))
                      end,
            Content = maps:merge(#{<<"redacts">> => Target},
                                 case Reason of
                                     <<>> -> #{};
                                     _ -> #{<<"reason">> => Reason}
                                 end),
            case Allowed andalso local_add(#{<<"type">> => ?ROOM_REDACTION, <<"sender">> => UserID,
                                             <<"redacts">> => Target, <<"content">> => Content}, Data) of
                false -> {xmpp:make_error(IQ, xmpp:err_forbidden()), Data};
                {ok, Data2} -> {xmpp:make_iq_result(IQ), Data2};
                {error, Why} -> {xmpp:make_error(IQ, xmpp_error(Why)), Data}
            end
    end.

%%--------------------------------------------------------------------
%% The room's avatar (m.room.avatar), as its vCard and presence say it
%% (XEP-0054 on the room JID, XEP-0153 hash, XEP-0486)
%%--------------------------------------------------------------------

room_avatar_mxc(Data) ->
    room_avatar_mxc(current_state_map(Data), Data).

room_avatar_mxc(StateMap, Data) ->
    case state_content(?ROOM_AVATAR, <<"">>, StateMap, Data) of
        #{<<"url">> := <<"mxc://", _/binary>> = MXC} -> MXC;
        _ -> undefined
    end.

%% The vCard now, or once the avatar is fetched (noreply until then).
room_vcard(IQ, Data) ->
    Name = room_name(current_state_map(Data), Data),
    case {room_avatar_mxc(Data), Data#data.avatar} of
        {undefined, _} ->
            {xmpp:make_iq_result(IQ, mod_matrix_gw_muc:room_vcard(Name, undefined, undefined)), Data};
        {MXC, {MXC, _Hash, Mime, Bytes}} ->
            {xmpp:make_iq_result(IQ, mod_matrix_gw_muc:room_vcard(Name, Mime, Bytes)), Data};
        {MXC, {fetching, MXC, IQs}} ->
            {noreply, Data#data{avatar = {fetching, MXC, [IQ | IQs]}}};
        {_, _} ->
            Data2 = fetch_avatar(Data),
            case Data2#data.avatar of
                {fetching, MXC2, IQs} -> {noreply, Data2#data{avatar = {fetching, MXC2, [IQ | IQs]}}};
                _ -> {xmpp:make_iq_result(IQ, mod_matrix_gw_muc:room_vcard(Name, undefined, undefined)), Data2}
            end
    end.

%% The avatar fetched into the store (mod_matrix_gw_media:resolve/3) and
%% read from there by a process of its own; the room gets avatar_fetched.
fetch_avatar(Data) ->
    fetch_avatar(current_state_map(Data), Data).

fetch_avatar(StateMap, Data) ->
    case {room_avatar_mxc(StateMap, Data), Data#data.avatar} of
        {undefined, _} ->
            Data#data{avatar = undefined};
        {MXC, {fetching, MXC, _}} ->
            Data;
        {MXC, {MXC, _, _, _}} ->
            Data;
        {MXC, _} ->
            Self = self(),
            Host = Data#data.host,
            spawn(fun() -> Self ! {avatar_fetched, MXC, catch avatar_bytes(Host, MXC)} end),
            Data#data{avatar = {fetching, MXC, []}}
    end.

avatar_bytes(Host, MXC) ->
    {ok, #{url := URL} = Info} = mod_matrix_gw_media:resolve(Host, MXC, #{}),
    %% read back as every store object is, the
    %% certificate verified and the body bounded while it comes
    {ok, 200, _, Body} = mod_matrix_gw_media:plain_get(Host, URL, 1048576),
    {ok, maps:get(mime, Info, <<"image/png">>), Body}.

avatar_fetched(MXC, Result, Data) ->
    case Data#data.avatar of
        {fetching, MXC, IQs} ->
            Name = room_name(current_state_map(Data), Data),
            {Avatar, VCard} =
                case Result of
                    {ok, Mime, Bytes} ->
                        Hash = str:sha(Bytes),
                        {{MXC, Hash, Mime, Bytes}, mod_matrix_gw_muc:room_vcard(Name, Mime, Bytes)};
                    Err ->
                        ?INFO_MSG("avatar ~s of ~s not fetched: ~p", [MXC, Data#data.room_id, Err]),
                        {{MXC, undefined, undefined, undefined},
                         mod_matrix_gw_muc:room_vcard(Name, undefined, undefined)}
                end,
            lists:foreach(fun(IQ) -> ejabberd_router:route(xmpp:make_iq_result(IQ, VCard)) end, IQs),
            Data2 = Data#data{avatar = Avatar},
            case Data2#data.kind of
                #multi{} ->
                    for_occupants_at(erlang:system_time(millisecond), Data2,
                                  fun(UserJID, RoomJID, _) ->
                                          send_avatar_presence(UserJID, RoomJID, Data2)
                                  end);
                _ ->
                    ok
            end,
            Data2;
        _ ->
            Data
    end.

%% The room's own presence with its avatar's hash, when it has one.
send_avatar_presence(UserJID, RoomJID, Data) ->
    case Data#data.avatar of
        {_, Hash, _, _} when is_binary(Hash) ->
            ejabberd_router:route(#presence{from = RoomJID, to = UserJID, type = available,
                                            sub_els = [mod_matrix_gw_muc:avatar_update_el(Hash)]});
        _ ->
            ok
    end.

%% A local user's m.room.member join content with her display name and
%% avatar (mod_matrix_gw_profile).
member_profile(Host, UserID, Content) ->
    mod_matrix_gw_profile:member_content(Host, UserID, Content).
