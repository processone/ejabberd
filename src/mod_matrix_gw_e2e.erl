%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_e2e.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : The end-to-end keys of the local users' Matrix devices
%%% Created : 2 Nov 2026 by Paavel A. <pd@narayana.im>
%%%
%%% The gateway's end-to-end keys (https://ns.another.im/matrix-e2e/0): the public keys
%%% of the local users' Matrix devices, served to the devices over XMPP and
%%% to the federation over HTTP, and the remote users' keys fetched and
%%% kept.
%%%
%%% A local XMPP account is one Matrix user, @<localpart>:<matrix_domain>;
%%% each device of the account (an installation, bound to its resource) is
%%% one Matrix device. Every private key stays on the device: the gateway
%%% keeps device keys, one-time and fallback keys, and the cross-signing
%%% master and self-signing keys, checks their signatures on upload, hands a
%%% one-time key out once, and tells the federation what changed.
%%%
%%% The device's iqs to the gateway's JID G (route_iq/1, from mod_matrix_gw's
%%% route/1, by element name; a local user's full JID only):
%%%   set <device id name replaces?/>       -> <device id user stream_id/>
%%%   set <keys-upload>...</keys-upload>    -> <keys-upload><count/></keys-upload>
%%%   set <signing-keys>...</signing-keys>  -> empty
%%%   get <keys-query><user id/>...         -> <keys-query><user id stream_id fresh>JSON</user>...
%%%   get <keys-claim><key user device algorithm/>... -> <key ... id fallback>JSON</key> | <missing/>
%%% The federation (mod_matrix_gw's process/2, after the signature check):
%%%   GET /user/devices/{userId}, POST /user/keys/query, POST /user/keys/claim.
%%% EDUs: m.device_list_update and m.signing_key_update both ways; the ones
%%% received mark the remote copy stale and tell the local devices that share
%%% a room with that user (<devices-changed/>, mod_matrix_gw_todevice's queue).
%%%
%%% Tables (mnesia disc_copies): matrix_device, matrix_otk, matrix_xsign,
%%% matrix_user_stream, matrix_remote_user (at most remote_max() rows, the
%%% least recently served dropped), matrix_room_share (who is in which room,
%%% fed by membership/4; the peer servers of a user and the local users
%%% sharing a room with a user are read from it, and from the direct chats).
%%%
%%% Nothing here logs key material: ids and sizes only.
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
-module(mod_matrix_gw_e2e).

-export([create_db/0, route_iq/1, disco_features/1,
         user_devices/2, keys_query/2, keys_claim/2,
         inbound_edu/3, membership/4,
         devices_of/2, resource_of/3, otk_count/3,
         delete_device/3, forget_device/3,
         peer_servers/2, room_share/2]).

-ifdef(TEST).
-export([verify_json/4, check_device_keys/3, check_csk/3, claim_local/3,
         bump_stream/1, set_remote_max/1, set_sink/1, set_federation/1, store_remote/3,
         remote_copy/2, upload/5, bind/6]).
-endif.

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

-define(NS, <<"https://ns.another.im/matrix-e2e/0">>).
-define(OTK_ALG, <<"signed_curve25519">>).
-define(MAX_OTKS, 100).
-define(MAX_QUERY, 50).
-define(FRESH_MS, 3600000).
-define(REMOTE_MAX, 10000).
-define(FED_TIMEOUT, 10000).

-type us() :: {binary(), binary()}.

%% A device of a local user: {US, DeviceId}; keys the DeviceKeys object as
%% the device uploaded it (none before the first upload); state deleted
%% once replaced (kept so the old binding can be told, forget_device/3).
-record(matrix_device,
        {us_dev :: {us(), binary()},
         us :: us(),
         resource = <<>> :: binary(),
         name = <<>> :: binary(),
         keys = none :: none | map(),
         created = 0 :: integer(),
         state = active :: active | deleted}).

%% A one-time or fallback key: {US, DeviceId, "signed_curve25519:ID",
%% Fallback}.
-record(matrix_otk,
        {key :: {us(), binary(), binary(), boolean()},
         us_dev :: {us(), binary()},
         object :: map(),
         fallback = false :: boolean()}).

-record(matrix_xsign,
        {us :: us(),
         master :: map(),
         self_signing :: map()}).

%% The user's device-list stream: bumped by every device or signing change.
-record(matrix_user_stream,
        {us :: us(),
         stream_id = 0 :: non_neg_integer()}).

-record(matrix_remote_user,
        {mxid :: binary() | '$1',
         stream_id = 0 :: non_neg_integer() | '_',
         devices = #{} :: map() | '_',
         master = none :: none | map() | '_',
         self_signing = none :: none | map() | '_',
         fetched_at = 0 :: integer() | '_',
         stale = false :: boolean() | '_',
         used_at = 0 :: integer() | '$2'}).

%% A member of a room (joined or invited), as membership/4 hears it.
-record(matrix_room_share,
        {room_mxid :: {binary(), binary()},
         room :: binary(),
         mxid :: binary()}).

create_db() ->
    Tables = [{matrix_device, record_info(fields, matrix_device), [us]},
              {matrix_otk, record_info(fields, matrix_otk), [us_dev]},
              {matrix_xsign, record_info(fields, matrix_xsign), []},
              {matrix_user_stream, record_info(fields, matrix_user_stream), []},
              {matrix_remote_user, record_info(fields, matrix_remote_user), []},
              {matrix_room_share, record_info(fields, matrix_room_share), [room, mxid]}],
    lists:foreach(
      fun({Name, Fields, Index}) ->
              ejabberd_mnesia:create(?MODULE, Name,
                                     [{disc_copies, [node()]}, {type, set},
                                      {index, Index}, {attributes, Fields}])
      end, Tables).

%%--------------------------------------------------------------------
%% Discovery
%%--------------------------------------------------------------------
%% The gateway's JID and its Matrix users' JIDs; a room's is the rooms'.
-spec disco_features(jid()) -> [binary()].
disco_features(#jid{luser = <<C, _/binary>>}) when C == $!; C == $# -> [];
disco_features(#jid{}) -> [?NS].

%%--------------------------------------------------------------------
%% The seam: what the other parts of the gateway read
%%--------------------------------------------------------------------

%% The active devices of a local user, with the resource each is bound to
%% (<<>> when none is).
-spec devices_of(binary(), binary()) -> [{binary(), binary()}].
devices_of(Host, LUser) ->
    [{D, R} || #matrix_device{us_dev = {_, D}, resource = R, state = active}
                   <- mnesia:dirty_index_read(matrix_device, {LUser, Host}, #matrix_device.us)].

%% The resource a device id is bound to; a replaced device's too, until
%% forget_device/3.
-spec resource_of(binary(), binary(), binary()) -> {ok, binary()} | error.
resource_of(Host, LUser, DeviceID) ->
    case mnesia:dirty_read(matrix_device, {{LUser, Host}, DeviceID}) of
        [#matrix_device{resource = R}] when R /= <<>> -> {ok, R};
        _ -> error
    end.

-spec otk_count(binary(), binary(), binary()) -> non_neg_integer().
otk_count(Host, LUser, DeviceID) ->
    length([K || #matrix_otk{fallback = false} = K
                     <- mnesia:dirty_index_read(matrix_otk, {{LUser, Host}, DeviceID},
                                                #matrix_otk.us_dev)]).

%% A device the device itself says it is not (<not-mine/>): deleted for
%% the federation (deleted: true), its keys gone.
-spec delete_device(binary(), binary(), binary()) -> ok.
delete_device(Host, LUser, DeviceID) ->
    US = {LUser, Host},
    case mnesia:transaction(fun() -> retire(US, DeviceID) end) of
        {atomic, true} ->
            ?INFO_MSG("matrix e2e: device ~s of ~s deleted", [DeviceID, LUser]),
            device_changed(Host, LUser, DeviceID, deleted, none);
        _ ->
            ok
    end.

%% A replaced device's row, once its binding was told.
-spec forget_device(binary(), binary(), binary()) -> ok.
forget_device(Host, LUser, DeviceID) ->
    Key = {{LUser, Host}, DeviceID},
    mnesia:transaction(
      fun() ->
              case mnesia:read(matrix_device, Key) of
                  [#matrix_device{state = deleted}] -> mnesia:delete({matrix_device, Key});
                  _ -> ok
              end
      end),
    ok.

%% Within a transaction: the device tombstoned and its keys deleted; true
%% when it was active.
retire(US, DeviceID) ->
    case mnesia:read(matrix_device, {US, DeviceID}) of
        [#matrix_device{state = active} = D] ->
            mnesia:write(D#matrix_device{state = deleted, keys = none}),
            [mnesia:delete_object(K) || K <- mnesia:index_read(matrix_otk, {US, DeviceID},
                                                               #matrix_otk.us_dev)],
            true;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% Rooms: who shares a room with whom
%%--------------------------------------------------------------------

%% Every m.room.member state change a room sees (mod_matrix_gw_room's
%% rooms call it): join and invite add MXID to the room, anything else
%% takes it out.
-spec membership(binary(), binary(), binary(), binary() | atom()) -> ok.
membership(_Host, RoomID, MXID, Membership) when is_binary(RoomID), is_binary(MXID) ->
    Key = {RoomID, MXID},
    case to_bin(Membership) of
        M when M == <<"join">>; M == <<"invite">> ->
            mnesia:dirty_write(#matrix_room_share{room_mxid = Key, room = RoomID, mxid = MXID});
        _ ->
            mnesia:dirty_delete(matrix_room_share, Key)
    end,
    ok;
membership(_, _, _, _) ->
    ok.

to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(B) -> B.

rooms_of(MXID) ->
    [R || #matrix_room_share{room = R} <- mnesia:dirty_index_read(matrix_room_share, MXID,
                                                                  #matrix_room_share.mxid)].

members_of(RoomID) ->
    [M || #matrix_room_share{mxid = M} <- mnesia:dirty_index_read(matrix_room_share, RoomID,
                                                                  #matrix_room_share.room)].

%% The remote servers a local user shares a room or a direct chat with:
%% where its device-list changes go.
-spec peer_servers(binary(), binary()) -> [binary()].
peer_servers(Host, LUser) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    Own = mxid(Host, LUser),
    InRooms = [M || R <- rooms_of(Own), M <- members_of(R)],
    Direct = [Remote || {matrix_direct, {{_, _}, Remote}, _}
                            <- direct_rows({{LUser, Host}, '_'})],
    lists:usort([S || M <- InRooms ++ Direct, {ok, _, S} <- [parse(M)], S /= Domain]).

%% The local users (local parts) who share a room or a direct chat with
%% MXID, MXID itself left out.
-spec room_share(binary(), binary()) -> [binary()].
room_share(Host, MXID) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    InRooms = [M || R <- rooms_of(MXID), M <- members_of(R), M /= MXID],
    Local = [L || M <- InRooms, {ok, U, S} <- [parse(M)], S == Domain,
                  L <- [jid:nodeprep(U)], is_binary(L), L /= <<>>],
    Direct = [L || {matrix_direct, {{L, H}, _}, _} <- direct_rows({{'_', '_'}, MXID}), H == Host],
    lists:usort(Local ++ Direct).

direct_rows(Key) ->
    try mnesia:dirty_match_object({matrix_direct, Key, '_'})
    catch _:_ -> []
    end.

%%--------------------------------------------------------------------
%% The device's iqs
%%--------------------------------------------------------------------

-spec route_iq(iq()) -> any().
route_iq(#iq{type = Type, from = From, lang = Lang,
             sub_els = [#xmlel{name = Name, attrs = Attrs} = El]} = IQ) ->
    Host = ejabberd_config:get_myname(),
    case {fxml:get_attr_s(<<"xmlns">>, Attrs), Type, Name} of
        {?NS, set, N} when N == <<"device">>; N == <<"keys-upload">>; N == <<"signing-keys">> ->
            case local_user(From, Host) of
                {ok, LUser, Res} ->
                    ejabberd_router:route(set_iq(Host, LUser, Res, IQ, N, El));
                error ->
                    route_error(IQ, xmpp:err_not_allowed(<<"Only for the users of this server">>, Lang))
            end;
        {?NS, get, N} when N == <<"keys-query">>; N == <<"keys-claim">> ->
            case local_user(From, Host) of
                {ok, LUser, _Res} ->
                    %% the federation may be asked: a process of its own
                    spawn(fun() -> ejabberd_router:route(get_iq(Host, LUser, IQ, N, El)) end),
                    ok;
                error ->
                    route_error(IQ, xmpp:err_not_allowed(<<"Only for the users of this server">>, Lang))
            end;
        _ ->
            ejabberd_router:process_iq(IQ)
    end;
route_iq(IQ) ->
    ejabberd_router:process_iq(IQ).

route_error(IQ, Err) ->
    ejabberd_router:route_error(IQ, Err).

%% A local user's full JID (the server-stamped bare JID, a resource).
local_user(#jid{luser = U, lserver = Host, lresource = R}, Host) when U /= <<>>, R /= <<>> ->
    case ejabberd_auth:user_exists(U, Host) of
        true -> {ok, U, R};
        false -> error
    end;
local_user(_, _) ->
    error.

set_iq(Host, LUser, Res, #iq{lang = Lang} = IQ, <<"device">>, El) ->
    ID = fxml:get_tag_attr_s(<<"id">>, El),
    Name = fxml:get_tag_attr_s(<<"name">>, El),
    Replaces = fxml:get_tag_attr_s(<<"replaces">>, El),
    case valid_device_id(ID) andalso (Replaces == <<>> orelse valid_device_id(Replaces)) of
        false ->
            xmpp:make_error(IQ, xmpp:err_bad_request(<<"Bad device id">>, Lang));
        true ->
            case bind(Host, LUser, Res, ID, Name, Replaces) of
                {ok, Stream} ->
                    xmpp:make_iq_result(
                      IQ, el(<<"device">>, [{<<"xmlns">>, ?NS}, {<<"id">>, ID},
                                            {<<"user">>, mxid(Host, LUser)},
                                            {<<"stream_id">>, integer_to_binary(Stream)}], []));
                {conflict, Other} ->
                    Err = xmpp:err_conflict(<<"The device id is bound to another resource">>, Lang),
                    xmpp:make_error(
                      IQ, Err#stanza_error{sub_els = [el(<<"bound-elsewhere">>,
                                                         [{<<"xmlns">>, ?NS}, {<<"resource">>, Other}], [])]})
            end
    end;
set_iq(Host, LUser, Res, #iq{lang = Lang} = IQ, <<"keys-upload">>, El) ->
    case bound_device(Host, LUser, Res) of
        error ->
            xmpp:make_error(IQ, xmpp:err_not_authorized(<<"No device registered">>, Lang));
        {ok, DeviceID} ->
            Parts = [{N, fxml:get_tag_cdata(C)} || #xmlel{name = N} = C <- El#xmlel.children],
            case decode_parts(Parts, [<<"device-keys">>, <<"one-time-keys">>, <<"fallback-keys">>]) of
                error ->
                    xmpp:make_error(IQ, xmpp:err_bad_request(<<"Bad JSON">>, Lang));
                {ok, Got} ->
                    case upload(Host, LUser, DeviceID, Got, Res) of
                        {ok, Count} ->
                            xmpp:make_iq_result(
                              IQ, el(<<"keys-upload">>, [{<<"xmlns">>, ?NS}],
                                     [el(<<"count">>, [{<<"algorithm">>, ?OTK_ALG}],
                                         [{xmlcdata, integer_to_binary(Count)}])]));
                        {error, bad_signature} ->
                            bad_signature(IQ, Lang)
                    end
            end
    end;
set_iq(Host, LUser, Res, #iq{lang = Lang} = IQ, <<"signing-keys">>, El) ->
    case bound_device(Host, LUser, Res) of
        error ->
            xmpp:make_error(IQ, xmpp:err_not_authorized(<<"No device registered">>, Lang));
        {ok, _DeviceID} ->
            Parts = [{N, fxml:get_tag_cdata(C)} || #xmlel{name = N} = C <- El#xmlel.children],
            case decode_parts(Parts, [<<"master">>, <<"self-signing">>]) of
                {ok, #{<<"master">> := Master, <<"self-signing">> := Self}} ->
                    case signing_keys(Host, LUser, Master, Self) of
                        ok -> xmpp:make_iq_result(IQ);
                        {error, bad_signature} -> bad_signature(IQ, Lang)
                    end;
                _ ->
                    bad_signature(IQ, Lang)
            end
    end.

bad_signature(IQ, Lang) ->
    Err = xmpp:err_not_acceptable(<<"The keys do not verify">>, Lang),
    xmpp:make_error(IQ, Err#stanza_error{sub_els = [el(<<"bad-signature">>, [{<<"xmlns">>, ?NS}], [])]}).

get_iq(Host, _LUser, #iq{lang = Lang} = IQ, <<"keys-query">>, El) ->
    Users = lists:usort([fxml:get_tag_attr_s(<<"id">>, C)
                         || #xmlel{name = <<"user">>} = C <- El#xmlel.children]),
    case length(Users) > ?MAX_QUERY of
        true ->
            xmpp:make_error(IQ, xmpp:err_resource_constraint(<<"At most 50 users">>, Lang));
        false ->
            Answers = query_users(Host, Users),
            xmpp:make_iq_result(
              IQ, el(<<"keys-query">>, [{<<"xmlns">>, ?NS}],
                     [el(<<"user">>, [{<<"id">>, U}, {<<"stream_id">>, integer_to_binary(S)},
                                      {<<"fresh">>, bool(F)}],
                         [{xmlcdata, misc:json_encode(Obj)}])
                      || {U, S, F, Obj} <- Answers]))
    end;
get_iq(Host, _LUser, IQ, <<"keys-claim">>, El) ->
    Wanted = [{fxml:get_tag_attr_s(<<"user">>, C), fxml:get_tag_attr_s(<<"device">>, C),
               fxml:get_tag_attr_s(<<"algorithm">>, C)}
              || #xmlel{name = <<"key">>} = C <- El#xmlel.children],
    Claimed = claim(Host, [{U, D} || {U, D, A} <- Wanted, A == ?OTK_ALG, U /= <<>>, D /= <<>>]),
    Els = [case maps:find({U, D}, Claimed) of
               {ok, {KeyID, Obj}} ->
                   el(<<"key">>, [{<<"user">>, U}, {<<"device">>, D}, {<<"algorithm">>, ?OTK_ALG},
                                  {<<"id">>, KeyID},
                                  {<<"fallback">>, bool(maps:get(<<"fallback">>, Obj, false) == true)}],
                      [{xmlcdata, misc:json_encode(Obj)}]);
               error ->
                   el(<<"missing">>, [{<<"user">>, U}, {<<"device">>, D}], [])
           end || {U, D, _} <- lists:usort(Wanted)],
    xmpp:make_iq_result(IQ, el(<<"keys-claim">>, [{<<"xmlns">>, ?NS}], Els)).

bool(true) -> <<"1">>;
bool(false) -> <<"0">>.

el(Name, Attrs, Children) ->
    #xmlel{name = Name, attrs = Attrs, children = Children}.

decode_parts(Parts, Known) ->
    try
        {ok, maps:from_list(
               [{N, case misc:json_decode(T) of
                        #{} = M -> M;
                        _ -> throw(bad)
                    end} || {N, T} <- Parts, lists:member(N, Known)])}
    catch _:_ -> error
    end.

%% Ten upper-case letters from a client; any short printable token taken.
valid_device_id(ID) ->
    byte_size(ID) >= 1 andalso byte_size(ID) =< 64 andalso
        lists:all(fun(C) -> (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z)
                                orelse (C >= $0 andalso C =< $9) orelse C == $_ orelse C == $-
                  end, binary_to_list(ID)).

%%--------------------------------------------------------------------
%% <device/>: the binding
%%--------------------------------------------------------------------

%% (LUser, ID) bound to Res: a conflict when another live resource holds
%% it; another device bound to Res before is unbound (it stays a device,
%% with no resource); Replaces, a device of the same user, is retired.
-spec bind(binary(), binary(), binary(), binary(), binary(), binary()) ->
          {ok, non_neg_integer()} | {conflict, binary()}.
bind(Host, LUser, Res, ID, Name, Replaces) ->
    US = {LUser, Host},
    Live = ejabberd_sm:get_user_resources(LUser, Host),
    F = fun() ->
                case mnesia:read(matrix_device, {US, ID}, write) of
                    [#matrix_device{state = active, resource = R}]
                      when R /= Res, R /= <<>> ->
                        case lists:member(R, Live) of
                            true -> mnesia:abort({conflict, R});
                            false -> ok
                        end;
                    _ ->
                        ok
                end,
                [mnesia:write(D#matrix_device{resource = <<>>})
                 || #matrix_device{us_dev = {_, Other}, resource = R, state = active} = D
                        <- mnesia:index_read(matrix_device, US, #matrix_device.us),
                    R == Res, Other /= ID],
                New = case mnesia:read(matrix_device, {US, ID}, write) of
                          [#matrix_device{state = active} = D] ->
                              D#matrix_device{resource = Res, name = Name};
                          _ ->
                              #matrix_device{us_dev = {US, ID}, us = US, resource = Res, name = Name,
                                             created = erlang:system_time(millisecond)}
                      end,
                mnesia:write(New),
                case Replaces /= <<>> andalso Replaces /= ID andalso retire(US, Replaces) of
                    true -> {replaced, Replaces};
                    false -> none
                end
        end,
    case mnesia:transaction(F) of
        {aborted, {conflict, R}} ->
            ?INFO_MSG("matrix e2e: device ~s of ~s is bound to another resource", [ID, LUser]),
            {conflict, R};
        {atomic, Replaced} ->
            ?INFO_MSG("matrix e2e: device ~s of ~s bound to its resource~s",
                      [ID, LUser, case Replaced of
                                      {replaced, O} -> [", replacing ", O];
                                      none -> ""
                                  end]),
            case Replaced of
                {replaced, Old} ->
                    queue(Host, LUser, Old, {device_deleted, Old}),
                    device_changed(Host, LUser, Old, deleted, none);
                none ->
                    ok
            end,
            {ok, stream(US)}
    end.

%% The device id a resource registered.
bound_device(Host, LUser, Res) ->
    case [D || #matrix_device{us_dev = {_, D}, resource = R, state = active}
                   <- mnesia:dirty_index_read(matrix_device, {LUser, Host}, #matrix_device.us),
               R == Res] of
        [D | _] -> {ok, D};
        [] -> error
    end.

%%--------------------------------------------------------------------
%% <keys-upload/>
%%--------------------------------------------------------------------

%% Got: the parts the device sent, decoded. The device keys checked first
%% (their own signature, this user and this device); one-time keys and the
%% fallback key signed by that device key. The count of unclaimed one-time
%% keys after it.
-spec upload(binary(), binary(), binary(), map(), binary()) ->
          {ok, non_neg_integer()} | {error, bad_signature}.
upload(Host, LUser, DeviceID, Got, _Res) ->
    US = {LUser, Host},
    MXID = mxid(Host, LUser),
    [Dev] = mnesia:dirty_read(matrix_device, {US, DeviceID}),
    DK = case Got of
             #{<<"device-keys">> := K} ->
                 case check_device_keys(K, MXID, DeviceID) of
                     {ok, _} -> {new, K};
                     error -> error
                 end;
             _ -> {old, Dev#matrix_device.keys}
         end,
    Ed = case DK of
             {_, #{<<"keys">> := #{} = KM}} -> maps:get(<<"ed25519:", DeviceID/binary>>, KM, none);
             _ -> none
         end,
    OTKs = maps:to_list(maps:get(<<"one-time-keys">>, Got, #{})),
    FBs = maps:to_list(maps:get(<<"fallback-keys">>, Got, #{})),
    KeyOk = fun({KeyID, Obj}) ->
                    is_map(Obj) andalso key_id_ok(KeyID) andalso key_ok(maps:get(<<"key">>, Obj, none))
                        andalso Ed /= none andalso verify_json(Obj, MXID, <<"ed25519:", DeviceID/binary>>, Ed)
            end,
    case DK /= error andalso lists:all(KeyOk, OTKs ++ FBs) of
        false ->
            ?INFO_MSG("matrix e2e: keys of device ~s of ~s refused: a signature does not verify",
                      [DeviceID, LUser]),
            {error, bad_signature};
        true ->
            F = fun() ->
                        Changed =
                            case DK of
                                {new, Keys} ->
                                    [D] = mnesia:read(matrix_device, {US, DeviceID}, write),
                                    mnesia:write(D#matrix_device{keys = Keys}),
                                    strip(Keys) /= strip(D#matrix_device.keys);
                                _ ->
                                    false
                            end,
                        Have = mnesia:index_read(matrix_otk, {US, DeviceID}, #matrix_otk.us_dev),
                        %% one-time and fallback key ids are counted apart
                        %% (vodozemac gives the first of each the same id)
                        HaveIDs = [K || #matrix_otk{key = {_, _, K, false}} <- Have],
                        HaveFB = [K || #matrix_otk{key = {_, _, K, true}} <- Have],
                        Room = max(0, ?MAX_OTKS - length(HaveIDs)),
                        New = [KO || {K, _} = KO <- lists:sort(OTKs), not lists:member(K, HaveIDs)],
                        {Taken, Dropped} = lists:split(min(Room, length(New)), New),
                        [mnesia:write(#matrix_otk{key = {US, DeviceID, K, false}, us_dev = {US, DeviceID},
                                                  object = O}) || {K, O} <- Taken],
                        case FBs of
                            [] ->
                                ok;
                            _ ->
                                FBIDs = [K || {K, _} <- FBs],
                                [mnesia:delete_object(O) || #matrix_otk{key = {_, _, K, true}} = O <- Have,
                                                            not lists:member(K, FBIDs)],
                                [mnesia:write(#matrix_otk{key = {US, DeviceID, K, true}, us_dev = {US, DeviceID},
                                                          object = O, fallback = true})
                                 || {K, O} <- FBs, not lists:member(K, HaveFB)]
                        end,
                        {Changed, length(Taken), length(Dropped)}
                end,
            {atomic, {Changed, NTaken, NDropped}} = mnesia:transaction(F),
            ?INFO_MSG("matrix e2e: device ~s of ~s uploaded~s ~B one-time key(s)~s~s",
                      [DeviceID, LUser, case DK of {new, _} -> " its device keys,"; _ -> "" end,
                       NTaken, case NDropped of 0 -> ""; _ -> io_lib:format(" (~B over the bound dropped)", [NDropped]) end,
                       case FBs of [] -> ""; _ -> " and a fallback key" end]),
            case Changed of
                true ->
                    {new, Keys} = DK,
                    device_changed(Host, LUser, DeviceID, Keys, DeviceID);
                false ->
                    ok
            end,
            {ok, otk_count(Host, LUser, DeviceID)}
    end.

strip(none) -> none;
strip(Keys) -> maps:without([<<"unsigned">>], Keys).

key_id_ok(<<"signed_curve25519:", ID/binary>>) -> ID /= <<>>;
key_id_ok(_) -> false.

%% Unpadded base64 of 32 bytes.
key_ok(K) when is_binary(K), byte_size(K) == 43 ->
    lists:all(fun(C) -> (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z)
                            orelse (C >= $0 andalso C =< $9) orelse C == $+ orelse C == $/
              end, binary_to_list(K));
key_ok(_) ->
    false.

%% A DeviceKeys object of User's device: its own signature verifies under
%% its Ed25519 key; {ok, Ed25519} or error.
-spec check_device_keys(term(), binary(), binary()) -> {ok, binary()} | error.
check_device_keys(#{<<"user_id">> := User, <<"device_id">> := Dev, <<"keys">> := #{} = Keys} = DK,
                  User, Dev) ->
    Ed = maps:get(<<"ed25519:", Dev/binary>>, Keys, none),
    Curve = maps:get(<<"curve25519:", Dev/binary>>, Keys, none),
    case key_ok(Ed) andalso key_ok(Curve) andalso verify_json(DK, User, <<"ed25519:", Dev/binary>>, Ed) of
        true -> {ok, Ed};
        false -> error
    end;
check_device_keys(_, _, _) ->
    error.

%% Obj's signature by User under KeyID verifies with the Ed25519 key Pub
%% (base64), over its canonical JSON without signatures and unsigned.
-spec verify_json(term(), binary(), binary(), binary()) -> boolean().
verify_json(#{<<"signatures">> := #{} = Sigs} = Obj, User, KeyID, Pub) ->
    try
        #{KeyID := Sig} = maps:get(User, Sigs),
        Msg = mod_matrix_gw:encode_canonical_json(maps:without([<<"signatures">>, <<"unsigned">>], Obj)),
        crypto:verify(eddsa, none, Msg, mod_matrix_gw:base64_decode(Sig),
                      [mod_matrix_gw:base64_decode(Pub), ed25519])
    catch _:_ ->
            false
    end;
verify_json(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------
%% <signing-keys/>
%%--------------------------------------------------------------------

%% A CrossSigningKey of User for Usage holding one Ed25519 key: {ok, Pub}.
-spec check_csk(term(), binary(), binary()) -> {ok, binary()} | error.
check_csk(#{<<"user_id">> := User, <<"usage">> := Usage, <<"keys">> := #{} = Keys}, User, Want)
  when is_list(Usage) ->
    case {lists:member(Want, Usage), maps:to_list(Keys)} of
        {true, [{<<"ed25519:", Pub/binary>>, Pub}]} ->
            case key_ok(Pub) of
                true -> {ok, Pub};
                false -> error
            end;
        _ ->
            error
    end;
check_csk(_, _, _) ->
    error.

%% The master key (its shape: the device does not sign it, and Matrix asks
%% no signature of it) and the self-signing key, signed by the master.
signing_keys(Host, LUser, Master, Self) ->
    MXID = mxid(Host, LUser),
    case check_csk(Master, MXID, <<"master">>) of
        {ok, MPub} ->
            case check_csk(Self, MXID, <<"self_signing">>) =/= error andalso
                verify_json(Self, MXID, <<"ed25519:", MPub/binary>>, MPub) of
                true ->
                    US = {LUser, Host},
                    Old = mnesia:dirty_read(matrix_xsign, US),
                    mnesia:dirty_write(#matrix_xsign{us = US, master = Master, self_signing = Self}),
                    case Old of
                        [#matrix_xsign{master = Master, self_signing = Self}] ->
                            ok;
                        _ ->
                            ?INFO_MSG("matrix e2e: cross-signing keys of ~s ~s",
                                      [LUser, case Old of [] -> "uploaded"; _ -> "replaced" end]),
                            signing_changed(Host, LUser, Master, Self)
                    end,
                    ok;
                false ->
                    {error, bad_signature}
            end;
        error ->
            {error, bad_signature}
    end.

%%--------------------------------------------------------------------
%% Changes: the stream, the federation, our devices
%%--------------------------------------------------------------------

stream(US) ->
    case mnesia:dirty_read(matrix_user_stream, US) of
        [#matrix_user_stream{stream_id = S}] -> S;
        [] -> 0
    end.

%% {Previous, New}
-spec bump_stream(us()) -> {non_neg_integer(), pos_integer()}.
bump_stream(US) ->
    New = mnesia:dirty_update_counter(matrix_user_stream, US, 1),
    {New - 1, New}.

%% A device of LUser changed (Keys its new keys, or deleted): the stream
%% bumped, m.device_list_update to every peer server, <devices-changed/> to
%% the local devices that care, Except (the device that did it) left out.
device_changed(Host, LUser, DeviceID, Keys, Except) ->
    {Prev, New} = bump_stream({LUser, Host}),
    MXID = mxid(Host, LUser),
    Content0 = #{<<"user_id">> => MXID, <<"device_id">> => DeviceID,
                 <<"stream_id">> => New, <<"prev_id">> => [Prev || Prev > 0]},
    Content = case Keys of
                  deleted -> Content0#{<<"deleted">> => true};
                  _ -> Content0#{<<"keys">> => strip(Keys)}
              end,
    send_edu(Host, LUser, #{<<"edu_type">> => <<"m.device_list_update">>, <<"content">> => Content}),
    tell_local(Host, LUser, New, Except).

signing_changed(Host, LUser, Master, Self) ->
    {_, New} = bump_stream({LUser, Host}),
    Content = #{<<"user_id">> => mxid(Host, LUser), <<"master_key">> => Master,
                <<"self_signing_key">> => Self},
    send_edu(Host, LUser, #{<<"edu_type">> => <<"m.signing_key_update">>, <<"content">> => Content}),
    tell_local(Host, LUser, New, none).

send_edu(Host, LUser, EDU) ->
    Servers = peer_servers(Host, LUser),
    ?DEBUG("matrix e2e: ~s of ~s to ~p", [maps:get(<<"edu_type">>, EDU), LUser, Servers]),
    [case sink() of
         undefined -> catch mod_matrix_gw_s2s:queue_edu(Host, S, EDU);
         F -> F({edu, S, EDU})
     end || S <- Servers],
    ok.

%% <devices-changed/> for LUser's MXID to LUser's other devices and to the
%% devices of the local users sharing a room with LUser.
tell_local(Host, LUser, Stream, Except) ->
    MXID = mxid(Host, LUser),
    Item = {devices_changed, [{MXID, Stream}]},
    lists:foreach(
      fun(U) ->
              [queue(Host, U, D, Item) || {D, _} <- devices_of(Host, U), {U, D} /= {LUser, Except}]
      end, lists:usort([LUser | room_share(Host, MXID)])),
    ok.

%% mod_matrix_gw_todevice's queue.
queue(Host, LUser, DeviceID, Item) ->
    case sink() of
        undefined ->
            mod_matrix_gw_todevice:queue(Host, LUser, DeviceID, Item);
        F ->
            F({queue, LUser, DeviceID, Item})
    end.

%% What goes out (EDUs, queue items) handed to a function of the suite's
%% instead (set_sink/1); undefined outside the suite.
sink() ->
    persistent_term:get({?MODULE, sink}, undefined).

%%--------------------------------------------------------------------
%% Received EDUs
%%--------------------------------------------------------------------

%% m.device_list_update and m.signing_key_update from Origin about one of
%% its users: our copy of that user's keys is stale, and the local devices
%% sharing a room with the user are told. ok when taken, pass otherwise.
-spec inbound_edu(binary(), binary(), map()) -> ok | pass.
inbound_edu(Host, Origin, #{<<"edu_type">> := Type, <<"content">> := #{<<"user_id">> := MXID} = C})
  when Type == <<"m.device_list_update">>; Type == <<"m.signing_key_update">> ->
    case parse(MXID) of
        {ok, _, Origin} ->
            Stream = case C of
                         #{<<"stream_id">> := S} when is_integer(S), S >= 0 -> S;
                         _ -> none
                     end,
            Known = case mnesia:dirty_read(matrix_remote_user, MXID) of
                        [#matrix_remote_user{} = R] ->
                            Stream2 = case Stream of none -> R#matrix_remote_user.stream_id; _ -> Stream end,
                            mnesia:dirty_write(R#matrix_remote_user{stale = true, stream_id = Stream2}),
                            Stream2;
                        [] ->
                            case Stream of none -> 0; _ -> Stream end
                    end,
            ?DEBUG("matrix e2e: ~s for ~s (stream ~p)", [Type, MXID, Known]),
            Item = {devices_changed, [{MXID, Known}]},
            [queue(Host, U, D, Item) || U <- room_share(Host, MXID), {D, _} <- devices_of(Host, U)],
            ok;
        _ ->
            ?INFO_MSG("matrix e2e: ~s for ~s from ~s ignored: not its user", [Type, MXID, Origin]),
            ok
    end;
inbound_edu(_Host, _Origin, _EDU) ->
    pass.

%%--------------------------------------------------------------------
%% Keys of users: ours from the tables, others' from their servers
%%--------------------------------------------------------------------

%% The keys of a local user: {StreamID, #{device_keys, master_key?,
%% self_signing_key?}}; Devices [] for all of them. Names (unsigned
%% device_display_name) for our own devices only, not over federation
%% (Synapse's default).
local_keys(Host, LUser, Devices, WithNames) ->
    US = {LUser, Host},
    DKs = maps:from_list(
            [{D, case WithNames andalso N /= <<>> of
                     true -> K#{<<"unsigned">> => #{<<"device_display_name">> => N}};
                     false -> maps:without([<<"unsigned">>], K)
                 end}
             || #matrix_device{us_dev = {_, D}, keys = #{} = K, name = N, state = active}
                    <- mnesia:dirty_index_read(matrix_device, US, #matrix_device.us),
                Devices == [] orelse lists:member(D, Devices)]),
    X = case mnesia:dirty_read(matrix_xsign, US) of
            [#matrix_xsign{master = M, self_signing = S}] ->
                #{<<"master_key">> => M, <<"self_signing_key">> => S};
            [] ->
                #{}
        end,
    {stream(US), X#{<<"device_keys">> => DKs}}.

%% Users' keys for <keys-query/>: [{MXID, Stream, Fresh, Obj}].
query_users(Host, Users) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    {Local, Remote} = lists:partition(fun(U) -> server_of(U) == Domain end, Users),
    LocalAns = [case local_part(Host, U) of
                    {ok, L} -> {S, Obj} = local_keys(Host, L, [], true), {U, S, true, Obj};
                    error -> {U, 0, true, #{<<"device_keys">> => #{}}}
                end || U <- Local],
    Now = erlang:system_time(millisecond),
    Need = [U || U <- Remote, parse(U) /= error,
                 case mnesia:dirty_read(matrix_remote_user, U) of
                     [#matrix_remote_user{stale = false, fetched_at = T}] -> Now - T > ?FRESH_MS;
                     _ -> true
                 end],
    Fetched = fetch_remote(Host, Need),
    RemoteAns = [remote_answer(U, lists:member(U, Need), lists:member(U, Fetched)) || U <- Remote],
    LocalAns ++ RemoteAns.

remote_answer(MXID, Asked, Got) ->
    case remote_copy(MXID, erlang:system_time(millisecond)) of
        {ok, Stream, Obj} -> {MXID, Stream, Got orelse not Asked, Obj};
        error -> {MXID, 0, false, #{<<"device_keys">> => #{}}}
    end.

%% The cached copy, its use noted (the LRU).
-spec remote_copy(binary(), integer()) -> {ok, non_neg_integer(), map()} | error.
remote_copy(MXID, Now) ->
    case mnesia:dirty_read(matrix_remote_user, MXID) of
        [#matrix_remote_user{stream_id = S, devices = Ds, master = M, self_signing = SS} = R] ->
            mnesia:dirty_write(R#matrix_remote_user{used_at = Now}),
            Obj = maps:from_list([{<<"device_keys">>, Ds}] ++
                                     [{<<"master_key">>, M} || is_map(M)] ++
                                     [{<<"self_signing_key">>, SS} || is_map(SS)]),
            {ok, S, Obj};
        [] ->
            error
    end.

%% Users fetched from their servers, a /user/keys/query per server, the
%% servers asked side by side; the users whose copy was refreshed.
fetch_remote(_Host, []) ->
    [];
fetch_remote(Host, Users) ->
    ByServer = group(fun server_of/1, Users),
    Self = self(),
    Refs = [spawn_ref(fun() -> fetch_server(Host, Server, Us) end, Self)
            || {Server, Us} <- maps:to_list(ByServer)],
    lists:append([receive {Ref, L} when is_list(L) -> L; {Ref, _} -> []
                   after ?FED_TIMEOUT + 2000 -> []
                   end || Ref <- Refs]).

%% F run in a process of its own, its result (or its exception) sent to
%% Self under the reference returned.
spawn_ref(F, Self) ->
    Ref = make_ref(),
    spawn(fun() -> Self ! {Ref, catch F()} end),
    Ref.

fetch_server(Host, Server, Users) ->
    Req = #{<<"device_keys">> => maps:from_list([{U, []} || U <- Users])},
    case federation(Host, Server, [<<"user">>, <<"keys">>, <<"query">>], Req) of
        {ok, #{<<"device_keys">> := #{} = DKs} = Res} ->
            Ms = maps:get(<<"master_keys">>, Res, #{}),
            SSs = maps:get(<<"self_signing_keys">>, Res, #{}),
            [U || U <- Users,
                  ok == store_remote(Host, U, #{devices => maps:get(U, DKs, #{}),
                                                master => maps:get(U, Ms, none),
                                                self_signing => maps:get(U, SSs, none)})];
        _ ->
            ?INFO_MSG("matrix e2e: the keys of ~B user(s) of ~s could not be fetched; the copy served",
                      [length(Users), Server]),
            []
    end.

%% A fetched copy kept: the devices whose object names that user and
%% device (the device checks the signatures again); past remote_max() rows
%% the least recently served go.
-spec store_remote(binary(), binary(), map()) -> ok.
store_remote(_Host, MXID, #{devices := Ds} = Got) ->
    Now = erlang:system_time(millisecond),
    Devices = case Ds of
                  #{} -> maps:filter(fun(D, #{<<"user_id">> := U, <<"device_id">> := D2}) ->
                                             U == MXID andalso D2 == D;
                                        (_, _) -> false
                                     end, Ds);
                  _ -> #{}
              end,
    CSK = fun(K) -> case maps:get(K, Got, none) of #{} = M -> M; _ -> none end end,
    Stream = case mnesia:dirty_read(matrix_remote_user, MXID) of
                 [#matrix_remote_user{stream_id = S}] -> S;
                 [] -> 0
             end,
    mnesia:dirty_write(#matrix_remote_user{mxid = MXID, stream_id = Stream, devices = Devices,
                                           master = CSK(master), self_signing = CSK(self_signing),
                                           fetched_at = Now, used_at = Now}),
    trim(),
    ok.

trim() ->
    Max = remote_max(),
    case mnesia:table_info(matrix_remote_user, size) of
        N when N > Max ->
            Old = lists:sublist(
                    lists:sort(mnesia:dirty_select(
                                       matrix_remote_user,
                                       [{#matrix_remote_user{mxid = '$1', used_at = '$2', _ = '_'},
                                         [], [{{'$2', '$1'}}]}])),
                    N - Max),
            [mnesia:dirty_delete(matrix_remote_user, M) || {_, M} <- Old],
            ok;
        _ ->
            ok
    end.

remote_max() ->
    persistent_term:get({?MODULE, remote_max}, ?REMOTE_MAX).

%%--------------------------------------------------------------------
%% Claims
%%--------------------------------------------------------------------

%% Wanted [{MXID, Device}]: #{{MXID, Device} => {KeyID, KeyObject}}.
claim(Host, Wanted) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    {Local, Remote} = lists:partition(fun({U, _}) -> server_of(U) == Domain end, Wanted),
    LocalGot = lists:flatmap(
                 fun({U, D}) ->
                         case local_part(Host, U) of
                             {ok, L} ->
                                 case claim_local(Host, L, D) of
                                     {ok, KeyID, Obj} -> [{{U, D}, {KeyID, Obj}}];
                                     none -> []
                                 end;
                             error -> []
                         end
                 end, lists:usort(Local)),
    maps:merge(maps:from_list(LocalGot), claim_remote(Host, [W || {U, _} = W <- Remote, parse(U) /= error])).

%% One key of a local device, atomically: an unclaimed one-time key
%% (deleted), else the fallback key (kept); the device told the count left.
-spec claim_local(binary(), binary(), binary()) -> {ok, binary(), map()} | none.
claim_local(Host, LUser, DeviceID) ->
    US = {LUser, Host},
    F = fun() ->
                Keys = mnesia:index_read(matrix_otk, {US, DeviceID}, #matrix_otk.us_dev),
                case lists:keysort(#matrix_otk.key, [K || #matrix_otk{fallback = false} = K <- Keys]) of
                    [#matrix_otk{key = {_, _, KeyID, _} = Key, object = Obj} | Rest] ->
                        mnesia:delete({matrix_otk, Key}),
                        {ok, KeyID, Obj, length(Rest), false};
                    [] ->
                        case [K || #matrix_otk{fallback = true} = K <- Keys] of
                            [#matrix_otk{key = {_, _, KeyID, _}, object = Obj} | _] ->
                                {ok, KeyID, Obj, 0, true};
                            [] ->
                                none
                        end
                end
        end,
    case mnesia:transaction(F) of
        {atomic, {ok, KeyID, Obj, Left, Fallback}} ->
            queue(Host, LUser, DeviceID, {keys_count, ?OTK_ALG, Left, Fallback}),
            {ok, KeyID, Obj};
        _ ->
            none
    end.

%% Remote devices' keys, a /user/keys/claim per server.
claim_remote(_Host, []) ->
    #{};
claim_remote(Host, Wanted) ->
    ByServer = group(fun({U, _}) -> server_of(U) end, Wanted),
    Self = self(),
    Refs = [spawn_ref(fun() -> claim_server(Host, Server, Ws) end, Self)
            || {Server, Ws} <- maps:to_list(ByServer)],
    lists:foldl(fun(Ref, Acc) ->
                        receive {Ref, #{} = M} -> maps:merge(Acc, M); {Ref, _} -> Acc
                        after ?FED_TIMEOUT + 2000 -> Acc
                        end
                end, #{}, Refs).

claim_server(Host, Server, Wanted) ->
    Req = #{<<"one_time_keys">> =>
                maps:map(fun(_, Ds) -> maps:from_list([{D, ?OTK_ALG} || D <- Ds]) end,
                         group(fun({U, _}) -> U end, fun({_, D}) -> D end, Wanted))},
    case federation(Host, Server, [<<"user">>, <<"keys">>, <<"claim">>], Req) of
        {ok, #{<<"one_time_keys">> := #{} = OTKs}} ->
            maps:from_list(
              [{{U, D}, {KeyID, Obj}}
               || {U, D} <- Wanted,
                  {KeyID, Obj} <- case OTKs of
                                      #{U := #{D := #{} = Ks}} -> lists:sublist(maps:to_list(Ks), 1);
                                      _ -> []
                                  end,
                  key_id_ok(KeyID), is_map(Obj)]);
        _ ->
            ?INFO_MSG("matrix e2e: no keys claimed from ~s", [Server]),
            #{}
    end.

%%--------------------------------------------------------------------
%% The federation's endpoints (mod_matrix_gw's process/2 after the check)
%%--------------------------------------------------------------------

%% GET /user/devices/{userId}
-spec user_devices(binary(), binary()) -> {integer(), map()}.
user_devices(Host, MXID) ->
    case local_part(Host, MXID) of
        {ok, L} ->
            {Stream, Obj} = local_keys(Host, L, [], false),
            Devices = [#{<<"device_id">> => D, <<"keys">> => K}
                       || {D, K} <- lists:sort(maps:to_list(maps:get(<<"device_keys">>, Obj)))],
            {200, maps:merge(maps:with([<<"master_key">>, <<"self_signing_key">>], Obj),
                             #{<<"user_id">> => MXID, <<"stream_id">> => Stream,
                               <<"devices">> => Devices})};
        error ->
            {404, #{<<"errcode">> => <<"M_NOT_FOUND">>, <<"error">> => <<"no such user">>}}
    end.

%% POST /user/keys/query
-spec keys_query(binary(), term()) -> {integer(), map()}.
keys_query(Host, #{<<"device_keys">> := #{} = Asked}) ->
    Res = maps:fold(
            fun(MXID, Devs, {DK, M, S}) ->
                    case local_part(Host, MXID) of
                        {ok, L} ->
                            Ds = case Devs of L1 when is_list(L1) -> [D || D <- L1, is_binary(D)]; _ -> [] end,
                            {_, Obj} = local_keys(Host, L, Ds, false),
                            {DK#{MXID => maps:get(<<"device_keys">>, Obj)},
                             with_key(MXID, <<"master_key">>, Obj, M),
                             with_key(MXID, <<"self_signing_key">>, Obj, S)};
                        error ->
                            {DK#{MXID => #{}}, M, S}
                    end
            end, {#{}, #{}, #{}}, Asked),
    {DK, M, S} = Res,
    {200, #{<<"device_keys">> => DK, <<"master_keys">> => M, <<"self_signing_keys">> => S}};
keys_query(_Host, _) ->
    {400, #{<<"errcode">> => <<"M_BAD_JSON">>, <<"error">> => <<"device_keys missing">>}}.

with_key(MXID, K, Obj, Acc) ->
    case Obj of
        #{K := V} -> Acc#{MXID => V};
        _ -> Acc
    end.

%% POST /user/keys/claim
-spec keys_claim(binary(), term()) -> {integer(), map()}.
keys_claim(Host, #{<<"one_time_keys">> := #{} = Asked}) ->
    OTKs = maps:fold(
             fun(MXID, #{} = Devs, Acc) ->
                     case local_part(Host, MXID) of
                         {ok, L} ->
                             Got = maps:fold(
                                     fun(D, ?OTK_ALG, A) ->
                                             case claim_local(Host, L, D) of
                                                 {ok, KeyID, Obj} -> A#{D => #{KeyID => Obj}};
                                                 none -> A
                                             end;
                                        (_, _, A) -> A
                                     end, #{}, Devs),
                             Acc#{MXID => Got};
                         error ->
                             Acc
                     end;
                (_, _, Acc) ->
                     Acc
             end, #{}, Asked),
    {200, #{<<"one_time_keys">> => OTKs}};
keys_claim(_Host, _) ->
    {400, #{<<"errcode">> => <<"M_BAD_JSON">>, <<"error">> => <<"one_time_keys missing">>}}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% A request to a Matrix server: {ok, JSON} or error. The suite stands the
%% federation in with set_federation/1.
federation(Host, Server, Path, JSON) ->
    case persistent_term:get({?MODULE, federation}, undefined) of
        F when is_function(F, 3) ->
            F(Server, Path, JSON);
        undefined ->
            case mod_matrix_gw:send_request(
                   Host, post, Server, [<<"_matrix">>, <<"federation">>, <<"v1">> | Path], [], JSON,
                   [{timeout, ?FED_TIMEOUT}], [{sync, true}, {body_format, binary}]) of
                {ok, {{_, 200, _}, _, Body}} ->
                    try misc:json_decode(Body) of
                        #{} = M -> {ok, M};
                        _ -> error
                    catch _:_ -> error
                    end;
                _ ->
                    error
            end
    end.

-ifdef(TEST).
set_federation(F) ->
    persistent_term:put({?MODULE, federation}, F).

set_sink(F) ->
    persistent_term:put({?MODULE, sink}, F).

set_remote_max(N) ->
    persistent_term:put({?MODULE, remote_max}, N).
-endif.

mxid(Host, LUser) ->
    <<$@, LUser/binary, $:, (mod_matrix_gw_opt:matrix_domain(Host))/binary>>.

parse(MXID) when is_binary(MXID) ->
    mod_matrix_gw_room:parse_user_id(MXID);
parse(_) ->
    error.

server_of(MXID) ->
    case parse(MXID) of
        {ok, _, S} -> S;
        error -> <<>>
    end.

%% The local part of a user of this gateway's domain who exists here.
local_part(Host, MXID) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    case parse(MXID) of
        {ok, U, Domain} ->
            case jid:nodeprep(U) of
                L when is_binary(L), L /= <<>> ->
                    case ejabberd_auth:user_exists(L, Host) of
                        true -> {ok, L};
                        false -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.

group(KeyF, L) ->
    group(KeyF, fun(X) -> X end, L).

group(KeyF, ValF, L) ->
    lists:foldr(fun(X, Acc) -> maps:update_with(KeyF(X), fun(Vs) -> [ValF(X) | Vs] end, [ValF(X)], Acc) end,
                #{}, L).
