%%%----------------------------------------------------------------------
%%% File    : mod_matrix_gw.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Matrix gateway
%%% Created : 23 Apr 2022 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(mod_matrix_gw).

-author('alexey@process-one.net').

-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).
-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process/2,
	 start_link/1,
	 procname/1,
         init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
         depends/2, mod_opt_type/1, mod_options/1, mod_doc/0]).
-export([parse_auth/1, encode_canonical_json/1,
         is_canonical_json/1,
         get_id_domain_exn/1,
         base64_decode/1, base64_encode/1,
         prune_event/2, get_event_id/2, content_hash/1,
         sign_event/3, sign_pruned_event/2, sign_json/2,
         send_request/8, send_request/9, s2s_out_bounce_packet/2, user_receive_packet/1,
         stopping/0, is_stopping/0, c2s_terminated/2,
	 process_disco_info/1,
	 process_disco_items/1,
         route/1]).

-ifdef(TEST).
-export([e2e_edu_allowed/2, claims_allowed/2]).
-endif.

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("translate.hrl").
-include("ejabberd_web_admin.hrl").
-include("mod_matrix_gw.hrl").

-define(MAX_REQUEST_SIZE, 1000000).
%% invites one origin may send a minute, EDUs
%% of the end-to-end kinds (device lists, to-device) a second
-define(INVITES_PER_MINUTE, 30).
-define(E2E_EDUS_PER_SECOND, 100).
%% beside the per-origin windows, whatever the
%% origins (one operator is a new origin a minute under each name it
%% delegates): invites a minute per local user and in all, end-to-end EDUs
%% a second in all, claims of one local device's keys a minute and in all
-define(INVITES_PER_USER, 30).
-define(INVITES_ALL, 300).
-define(E2E_EDUS_ALL, 1000).
-define(CLAIMS_PER_DEVICE, 100).
-define(CLAIMS_ALL, 1000).

-define(CORS_HEADERS,
        [{<<"Access-Control-Allow-Origin">>, <<"*">>},
         {<<"Access-Control-Allow-Methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>},
         {<<"Access-Control-Allow-Headers">>, <<"X-Requested-With, Content-Type, Authorization">>}]).

process([<<"key">>, <<"v2">>, <<"server">> | _],
        #request{method = 'GET', host = _Host} = _Request) ->
    Host = ejabberd_config:get_myname(),
    KeyName = mod_matrix_gw_opt:key_name(Host),
    KeyID = <<"ed25519:", KeyName/binary>>,
    ServerName = mod_matrix_gw_opt:matrix_domain(Host),
    TS = erlang:system_time(millisecond) + timer:hours(24 * 7),
    {PubKey, _PrivKey} = mod_matrix_gw_opt:key(Host),
    JSON = #{<<"old_verify_keys">> => #{},
             <<"server_name">> => ServerName,
             <<"valid_until_ts">> => TS,
             <<"verify_keys">> => #{
               KeyID => #{
                 <<"key">> => base64_encode(PubKey)
                }
              }},
    SJSON = sign_json(Host, JSON),
    {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
     misc:json_encode(SJSON)};
process([<<"federation">>, <<"v1">>, <<"version">>],
        #request{method = 'GET', host = _Host} = _Request) ->
    JSON = #{<<"server">> => #{<<"name">> => <<"ejabberd/mod_matrix_gw">>,
                               <<"version">> => <<"0.1">>}},
    {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
     misc:json_encode(JSON)};
process([<<"federation">>, <<"v1">>, <<"query">>, <<"profile">>],
        #request{method = 'GET', host = _Host} = Request) ->
    case proplists:get_value(<<"user_id">>, Request#request.q) of
        UserID when is_binary(UserID) ->
            Field =
                case proplists:get_value(<<"field">>, Request#request.q) of
                    <<"displayname">> -> displayname;
                    <<"avatar_url">> -> avatar_url;
                    undefined -> all;
                    _ -> error
                end,
            case Field of
                error ->
                    mod_matrix_gw_http:error(400, <<"M_INVALID_PARAM">>, <<"bad 'field' parameter">>);
                _ ->
                    case preprocess_federation_request(Request) of
                        {ok, _JSON, _Origin} ->
                            %% the user's vCard nickname (or full
                            %% name) as displayname, its photo as
                            %% avatar_url (an mxc of ours,
                            %% mod_matrix_gw_profile)
                            Host = ejabberd_config:get_myname(),
                            case local_user(Host, UserID) of
                                {ok, LUser} ->
                                    Profile = mod_matrix_gw_profile:local_profile(LUser, Host, block),
                                    mod_matrix_gw_http:json(
                                      200, case Field of
                                               all -> Profile;
                                               displayname -> maps:with([<<"displayname">>], Profile);
                                               avatar_url -> maps:with([<<"avatar_url">>], Profile)
                                           end);
                                error ->
                                    mod_matrix_gw_http:error(404, <<"M_NOT_FOUND">>, <<"no such user">>)
                            end;
                        {result, HTTPResult} ->
                            HTTPResult
                    end
            end;
        undefined ->
            {400, [], <<"400 Bad Request: missing 'user_id' parameter">>}
    end;
process([<<"federation">>, <<"v1">>, <<"user">>, <<"devices">>, UserID],
        #request{method = 'GET', host = _Host} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, _JSON, _Origin} ->
            %% the local user's devices and cross-signing keys
            %% (mod_matrix_gw_e2e); M_NOT_FOUND for one who is not here
            {Code, Res} = mod_matrix_gw_e2e:user_devices(ejabberd_config:get_myname(), UserID),
            mod_matrix_gw_http:json(Code, Res);
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"user">>, <<"keys">>, <<"query">>],
        #request{method = 'POST', host = _Host} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, #{<<"device_keys">> := _} = JSON, _Origin} ->
            %% the local users' keys (mod_matrix_gw_e2e)
            {Code, Res} = mod_matrix_gw_e2e:keys_query(ejabberd_config:get_myname(), JSON),
            mod_matrix_gw_http:json(Code, Res);
        {ok, _JSON, _Origin} ->
            {400, [], <<"400 Bad Request: invalid format">>};
        {result, HTTPResult} ->
            HTTPResult
    end;
%% a one-time key of each local device asked (mod_matrix_gw_e2e)
process([<<"federation">>, <<"v1">>, <<"user">>, <<"keys">>, <<"claim">>],
        #request{method = 'POST'} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, #{<<"one_time_keys">> := _} = JSON, Origin} ->
            %% within the origin's claims a minute;
            %%  and within each local device's and the
            %% gateway's, whatever the origins
            Host = ejabberd_config:get_myname(),
            case claims_allowed(Host, JSON) of
                true ->
                    {Code, Res} = mod_matrix_gw_e2e:keys_claim(Host, Origin, JSON),
                    http_json(Code, Res);
                false ->
                    ?INFO_MSG("Matrix gateway: claims from ~s over the gateway's or a device's "
                              "claims a minute: refused", [Origin]),
                    http_json(429, #{<<"errcode">> => <<"M_LIMIT_EXCEEDED">>,
                                     <<"error">> => <<"too many one-time key claims">>,
                                     <<"retry_after_ms">> => 60000})
            end;
        {ok, _JSON, _Origin} ->
            mod_matrix_gw_http:error(400, <<"M_BAD_JSON">>, <<"one_time_keys missing">>);
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v2">>, <<"invite">>, RoomID, EventID],
        #request{method = 'PUT', host = _Host} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, #{<<"event">> := #{%<<"origin">> := Origin,
                                <<"content">> := Content,
                                <<"room_id">> := RoomID,
                                <<"sender">> := Sender,
                                <<"state_key">> := UserID} = Event,
               <<"room_version">> := RoomVer} = JSON,
         Origin} ->
            Host = ejabberd_config:get_myname(),
            case {mod_matrix_gw_room:binary_to_room_version(RoomVer),
                  invite_check(Host, Origin, RoomID, Event)} of
                {false, _} ->
                    mod_matrix_gw_http:json(
                      400, #{<<"errcode">> => <<"M_INCOMPATIBLE_ROOM_VERSION">>,
                             <<"error">> => <<"unsupported room version">>,
                             <<"room_version">> => RoomVer});
                {_, {error, Code, ErrCode, Text}} ->
                    ?INFO_MSG("Matrix gateway: an invite from ~s for ~p refused: ~s",
                              [Origin, UserID, Text]),
                    http_json(Code, #{<<"errcode">> => ErrCode, <<"error">> => Text});
                {#room_version{} = RoomVersion, ok} ->
                    PrunedEvent = prune_event(Event, RoomVersion),
                    %?DEBUG("invite ~p~n", [{RoomID, EventID, Event, RoomVer, catch mod_matrix_gw_s2s:check_signature(Host, PrunedEvent, RoomVersion), get_pruned_event_id(PrunedEvent)}]),
                    case mod_matrix_gw_s2s:check_signature(Host, PrunedEvent, RoomVersion) of
                        true ->
                            case get_pruned_event_id(PrunedEvent) of
                                EventID ->
                                    SEvent = sign_pruned_event(Host, PrunedEvent),
                                    ?DEBUG("sign event ~p~n", [SEvent]),
                                    ResJSON = #{<<"event">> => SEvent},
                                    case Content of
                                        #{<<"is_direct">> := true} ->
                                            mod_matrix_gw_room:join_direct(Host, Origin, RoomID, Sender, UserID);
                                        _ ->
                                            IRS = case JSON of
                                                      #{<<"invite_room_state">> := IRS1} -> IRS1;
                                                      _ -> []
                                                  end,
                                            mod_matrix_gw_room:send_muc_invite(Host, Origin, RoomID, Sender, UserID, Event, IRS)
                                    end,
                                    ?DEBUG("res ~s~n", [misc:json_encode(ResJSON)]),
                                    {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}], misc:json_encode(ResJSON)};
                                _ ->
                                    {400, [], <<"400 Bad Request: bad event id">>}
                            end;
                        false ->
                            {400, [], <<"400 Bad Request: signature check failed">>}
                    end
            end;
        {ok, _JSON, _Origin} ->
            {400, [], <<"400 Bad Request: invalid format">>};
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"send">>, _TxnID],
        #request{method = 'PUT', host = _Host} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, #{<<"origin">> := Origin,
               <<"pdus">> := PDUs} = JSON,
         Origin} when is_list(PDUs) ->
            ?DEBUG("send request ~p~n", [JSON]),
            Host = ejabberd_config:get_myname(),
            %% the to-device EDUs (room keys) before
            %% the PDUs, so that a Megolm event finds its key; the rest
            %% (typing, receipts of these events) after them
            {KeyEDUs, LaterEDUs} = mod_matrix_gw_xmpp:edu_order(maps:get(<<"edus">>, JSON, [])),
            catch dispatch_edus(Host, Origin, KeyEDUs),
            %% a PDU that cannot be processed is an error of its
            %% own, never the whole transaction's (nor a crash)
            Res = lists:flatmap(
                    fun(PDU) ->
                            try mod_matrix_gw_room:process_pdu(Host, Origin, PDU) of
                                {ok, EventID} -> [{EventID, #{}}];
                                {error, Error} -> pdu_error(PDU, Error)
                            catch
                                Class:Reason:ST ->
                                    %% no term of the
                                    %% PDU in the log, the stack at DEBUG
                                    ?INFO_MSG("failed to process a PDU from ~s in ~p: ~p",
                                              [Origin, case PDU of
                                                           #{<<"room_id">> := R} when is_binary(R),
                                                                                      byte_size(R) =< 255 -> R;
                                                           _ -> none
                                                       end,
                                               mod_matrix_gw_room:failure(Class, Reason, ST)]),
                                    pdu_error(PDU, <<"failed to process the PDU">>)
                            end
                    end, PDUs),
            %% EDUs (26.09 read none), by type (dispatch_edus/3)
            catch dispatch_edus(Host, Origin, LaterEDUs),
            ?DEBUG("send res ~p~n", [Res]),
            %% the spec's answer is {"pdus": {EventID: Result}}
            mod_matrix_gw_http:json(200, #{<<"pdus">> => maps:from_list(Res)});
        {ok, _JSON, _Origin} ->
            {400, [], <<"400 Bad Request: invalid format">>};
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"get_missing_events">>, RoomID],
        #request{method = 'POST', host = _Host} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, #{<<"earliest_events">> := EarliestEvents,
               <<"latest_events">> := LatestEvents} = JSON,
         Origin} ->
            ?DEBUG("get_missing_events request ~p~n", [JSON]),
            Limit = maps:get(<<"limit">>, JSON, 10),
            MinDepth = maps:get(<<"min_depth">>, JSON, 0),
            Host = ejabberd_config:get_myname(),
            PDUs = mod_matrix_gw_room:get_missing_events(
                     Host, Origin, RoomID, EarliestEvents, LatestEvents, Limit, MinDepth),
            ?DEBUG("get_missing_events res ~p~n", [PDUs]),
            Res = #{<<"events">> => PDUs},
            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
             misc:json_encode(Res)};
        {ok, _JSON, _Origin} ->
            {400, [], <<"400 Bad Request: invalid format">>};
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"backfill">>, RoomID],
        #request{method = 'GET', host = _Host} = Request) ->
    case catch binary_to_integer(proplists:get_value(<<"limit">>, Request#request.q)) of
        Limit when is_integer(Limit) ->
            case preprocess_federation_request(Request) of
                {ok, _JSON, Origin} ->
                    LatestEvents = proplists:get_all_values(<<"v">>, Request#request.q),
                    ?DEBUG("backfill request ~p~n", [{Limit, LatestEvents}]),
                    Host = ejabberd_config:get_myname(),
                    PDUs1 = mod_matrix_gw_room:get_missing_events(
                              Host, Origin, RoomID, [], LatestEvents, Limit, 0),
                    %% to a server joined to the room
                    PDUs2 = lists:flatmap(
                              fun(EventID) ->
                                      case catch mod_matrix_gw_room:get_event(Host, Origin, RoomID, EventID) of
                                          {ok, PDU} ->
                                              [PDU];
                                          _ ->
                                              []
                                      end
                              end, LatestEvents),
                    PDUs = PDUs2 ++ PDUs1,
                    ?DEBUG("backfill res ~p~n", [PDUs]),
                    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
                    Res = #{<<"origin">> => MatrixServer,
                            <<"origin_server_ts">> => erlang:system_time(millisecond),
                            <<"pdus">> => PDUs},
                    {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                     misc:json_encode(Res)};
                {result, HTTPResult} ->
                    HTTPResult
            end;
        _ ->
            {400, [], <<"400 Bad Request: bad 'limit' parameter">>}
    end;
process([<<"federation">>, <<"v1">>, <<"state_ids">>, RoomID],
        #request{method = 'GET', host = _Host} = Request) ->
    case proplists:get_value(<<"event_id">>, Request#request.q) of
        EventID when is_binary(EventID) ->
            case preprocess_federation_request(Request) of
                {ok, _JSON, Origin} ->
                    Host = ejabberd_config:get_myname(),
                    case mod_matrix_gw_room:get_state_ids(Host, Origin, RoomID, EventID) of
                        {ok, AuthChain, PDUs} ->
                            Res = #{<<"auth_chain_ids">> => AuthChain,
                                    <<"pdu_ids">> => PDUs},
                            ?DEBUG("get_state_ids res ~p~n", [Res]),
                            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             misc:json_encode(Res)};
                        {error, room_not_found} ->
                            {400, [], <<"400 Bad Request: room not found">>};
                        {error, not_allowed} ->
                            {403, [], <<"403 Forbidden: origin not in room">>};
                        {error, event_not_found} ->
                            {400, [], <<"400 Bad Request: 'event_id' not found">>}
                    end;
                {result, HTTPResult} ->
                    HTTPResult
            end;
        undefined ->
            {400, [], <<"400 Bad Request: missing 'event_id' parameter">>}
    end;
process([<<"federation">>, <<"v1">>, <<"event">>, EventID],
        #request{method = 'GET', host = _Host} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, _JSON, Origin} ->
            Host = ejabberd_config:get_myname(),
            %% TODO: very inefficient, replace with an SQL call
            %% from the rooms the origin is joined
            %% to only, a room that does not answer skipped
            PDU =
                lists:foldl(
                  fun(RoomID, undefined) ->
                          case catch mod_matrix_gw_room:get_event(Host, Origin, RoomID, EventID) of
                              {ok, PDU} ->
                                  PDU;
                              _ ->
                                  undefined
                          end;
                     (_, Acc) ->
                          Acc
                  end, undefined, mod_matrix_gw_room:get_rooms_list()),
            ?DEBUG("get_event res ~p~n", [PDU]),
            case PDU of
                undefined ->
                    {400, [], <<"400 Bad Request: event not found">>};
                _ ->
                    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
                    Res = #{<<"origin">> => MatrixServer,
                            <<"origin_server_ts">> => erlang:system_time(millisecond),
                            <<"pdus">> => [PDU]},
                    {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                     misc:json_encode(Res)}
            end;
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"make_join">>, RoomID, UserID],
        #request{method = 'GET', host = _Host, q = Params} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, _JSON, Origin} ->
            Host = ejabberd_config:get_myname(),
            case get_id_domain_exn(UserID) of
                Origin ->
                    case mod_matrix_gw_room:make_join(Host, RoomID, UserID, Params) of
                        {error, room_not_found} ->
                            Res = #{<<"errcode">> => <<"M_NOT_FOUND">>,
                                    <<"error">> => <<"Unknown room">>},
                            {404, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             misc:json_encode(Res)};
                        {error, not_invited} ->
                            Res = #{<<"errcode">> => <<"M_FORBIDDEN">>,
                                    <<"error">> => <<"You are not invited to this room">>},
                            {403, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             misc:json_encode(Res)};
                        {error, {incompatible_version, Ver}} ->
                            Res = #{<<"errcode">> => <<"M_INCOMPATIBLE_ROOM_VERSION">>,
                                    <<"error">> => <<"Your homeserver does not support the features required to join this room">>,
                                    <<"room_version">> => Ver},
                            {400, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             misc:json_encode(Res)};
                        {ok, Res} ->
                            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             misc:json_encode(Res)}
                    end;
                _ ->
                    Res = #{<<"errcode">> => <<"M_FORBIDDEN">>,
                            <<"error">> => <<"User not from origin">>},
                    {403, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                     misc:json_encode(Res)}
            end;
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v2">>, <<"send_join">>, RoomID, EventID],
        #request{method = 'PUT', host = _Host} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, #{<<"content">> := #{<<"membership">> := <<"join">>},
               %<<"origin">> := Origin,
               <<"room_id">> := RoomID,
               <<"sender">> := Sender,
               <<"state_key">> := Sender,
               <<"type">> := <<"m.room.member">>} = JSON, Origin} ->
            Host = ejabberd_config:get_myname(),
            %% a member event of the wrong shape is
            %% M_BAD_JSON, and nothing in its handling crashes the request
            case member_pdu_ok(JSON) andalso (catch get_id_domain_exn(Sender)) of
                Bad when Bad == false; element(1, Bad) == 'EXIT' ->
                    mod_matrix_gw_http:error(400, <<"M_BAD_JSON">>, <<"Invalid event format">>);
                Origin ->
                    try mod_matrix_gw_room:send_join(Host, Origin, RoomID, EventID, JSON) of
                        {error, Error} when is_binary(Error) ->
                            Res = #{<<"errcode">> => <<"M_BAD_REQUEST">>,
                                    <<"error">> => Error},
                            {403, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             misc:json_encode(Res)};
                        {ok, Res} ->
                            ?DEBUG("send_join res: ~p~n", [Res]),
                            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             misc:json_encode(Res)}
                    catch
                        Class:Reason:ST ->
                            ?INFO_MSG("failed send_join of ~s to ~s: ~p",
                                      [Origin, RoomID, mod_matrix_gw_room:failure(Class, Reason, ST)]),
                            mod_matrix_gw_http:error(400, <<"M_BAD_JSON">>, <<"The event could not be processed">>)
                    end;
                _ ->
                    Res = #{<<"errcode">> => <<"M_FORBIDDEN">>,
                            <<"error">> => <<"User not from origin">>},
                    {403, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                     misc:json_encode(Res)}
            end;
        {ok, _JSON, _Origin} ->
            Res = #{<<"errcode">> => <<"M_BAD_REQUEST">>,
                    <<"error">> => <<"Invalid event format">>},
            {400, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
             misc:json_encode(Res)};
        {result, HTTPResult} ->
            HTTPResult
    end;
%process([<<"client">> | ClientPath], Request) ->
%    {HTTPCode, Headers, JSON} = mod_matrix_gw_c2s:process(ClientPath, Request),
%    ?DEBUG("resp ~p~n", [JSON]),
%    {HTTPCode,
%     [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>} |
%      ?CORS_HEADERS] ++ Headers,
%     jiffy:encode(JSON)};
%% The room's state at an event, in full (26.09 served the
%% ids alone).
process([<<"federation">>, <<"v1">>, <<"state">>, RoomID],
        #request{method = 'GET'} = Request) ->
    case proplists:get_value(<<"event_id">>, Request#request.q) of
        EventID when is_binary(EventID) ->
            case preprocess_federation_request(Request) of
                {ok, _JSON, Origin} ->
                    Host = ejabberd_config:get_myname(),
                    case mod_matrix_gw_room:get_state(Host, Origin, RoomID, EventID) of
                        {ok, AuthChain, PDUs} ->
                            mod_matrix_gw_http:json(200, #{<<"auth_chain">> => AuthChain,
                                                           <<"pdus">> => PDUs});
                        {error, not_allowed} ->
                            mod_matrix_gw_http:error(403, <<"M_FORBIDDEN">>, <<"origin not in room">>);
                        {error, _} ->
                            mod_matrix_gw_http:error(404, <<"M_NOT_FOUND">>, <<"no such room or event">>)
                    end;
                {result, HTTPResult} ->
                    HTTPResult
            end;
        undefined ->
            mod_matrix_gw_http:error(400, <<"M_MISSING_PARAM">>, <<"missing 'event_id' parameter">>)
    end;
%% The aliases of the rooms made here (mod_matrix_gw_muc),
%% while this server is in them.
process([<<"federation">>, <<"v1">>, <<"query">>, <<"directory">>],
        #request{method = 'GET'} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, _JSON, _Origin} ->
            Host = ejabberd_config:get_myname(),
            Alias = proplists:get_value(<<"room_alias">>, Request#request.q, <<>>),
            case mod_matrix_gw_room:directory(Host, Alias) of
                {ok, RoomID, Servers} ->
                    mod_matrix_gw_http:json(200, #{<<"room_id">> => RoomID,
                                                   <<"servers">> => Servers});
                error ->
                    mod_matrix_gw_http:error(404, <<"M_NOT_FOUND">>, <<"no such room alias">>)
            end;
        {result, HTTPResult} ->
            HTTPResult
    end;
%% A remote user leaving a room this gateway is in, in two steps
%% as for joining.
process([<<"federation">>, <<"v1">>, <<"make_leave">>, RoomID, UserID],
        #request{method = 'GET'} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, _JSON, Origin} ->
            Host = ejabberd_config:get_myname(),
            case catch get_id_domain_exn(UserID) of
                Origin ->
                    case mod_matrix_gw_room:make_leave(Host, RoomID, UserID) of
                        {ok, Res} ->
                            mod_matrix_gw_http:json(200, Res);
                        {error, room_not_found} ->
                            mod_matrix_gw_http:error(404, <<"M_NOT_FOUND">>, <<"Unknown room">>);
                        {error, _} ->
                            mod_matrix_gw_http:error(403, <<"M_FORBIDDEN">>, <<"You cannot leave this room">>)
                    end;
                _ ->
                    mod_matrix_gw_http:error(403, <<"M_FORBIDDEN">>, <<"User not from origin">>)
            end;
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v2">>, <<"send_leave">>, RoomID, EventID],
        #request{method = 'PUT'} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, #{<<"content">> := #{<<"membership">> := <<"leave">>},
               <<"room_id">> := RoomID,
               <<"sender">> := Sender,
               <<"state_key">> := Sender,
               <<"type">> := <<"m.room.member">>} = JSON, Origin} ->
            Host = ejabberd_config:get_myname(),
            %% as /send_join
            case member_pdu_ok(JSON) andalso (catch get_id_domain_exn(Sender)) of
                Bad when Bad == false; element(1, Bad) == 'EXIT' ->
                    mod_matrix_gw_http:error(400, <<"M_BAD_JSON">>, <<"Invalid event format">>);
                Origin ->
                    try mod_matrix_gw_room:process_pdu(Host, Origin, JSON) of
                        {ok, EventID} ->
                            ?INFO_MSG("send_leave: ~s left ~s", [Sender, RoomID]),
                            mod_matrix_gw_http:json(200, #{});
                        {ok, _} ->
                            mod_matrix_gw_http:error(400, <<"M_BAD_JSON">>, <<"Bad event id">>);
                        _ ->
                            mod_matrix_gw_http:error(403, <<"M_FORBIDDEN">>, <<"The leave was refused">>)
                    catch
                        Class:Reason:ST ->
                            ?INFO_MSG("failed send_leave of ~s to ~s: ~p",
                                      [Origin, RoomID, mod_matrix_gw_room:failure(Class, Reason, ST)]),
                            mod_matrix_gw_http:error(400, <<"M_BAD_JSON">>, <<"The event could not be processed">>)
                    end;
                _ ->
                    mod_matrix_gw_http:error(403, <<"M_FORBIDDEN">>, <<"User not from origin">>)
            end;
        {ok, _JSON, _Origin} ->
            mod_matrix_gw_http:error(400, <<"M_BAD_JSON">>, <<"Invalid event format">>);
        {result, HTTPResult} ->
            HTTPResult
    end;
%% OpenID's userinfo, the federation's one unauthenticated
%% endpoint (MatrixRTC's JWT services ask it, mod_matrix_gw_rtc).
process([<<"federation">>, <<"v1">>, <<"openid">>, <<"userinfo">>], Request) ->
    mod_matrix_gw_rtc:userinfo(Request);
%% Media (mod_matrix_gw_media), the federation's signed and the
%% legacy unauthenticated endpoints.
process([<<"federation">>, <<"v1">>, <<"media">> | _] = Path, Request) ->
    case preprocess_federation_request(Request) of
        {ok, _JSON, _Origin} -> mod_matrix_gw_media:process(Path, Request);
        {result, HTTPResult} -> HTTPResult
    end;
process([<<"media">> | _] = Path, Request) ->
    mod_matrix_gw_media:process(Path, Request);
process(Path, Request) ->
    ?DEBUG("matrix 404: ~p~n~p~n", [Path, Request]),
    %% the spec's answer to an endpoint not served
    mod_matrix_gw_http:error(404, <<"M_UNRECOGNIZED">>, <<"Unrecognized request">>).

%% The fields of a /send_join or /send_leave member
%% event the room reads before its checks, of the spec's types: sender and
%% state_key strings (one user id), origin_server_ts an integer, content an
%% object, and the event lists lists.
member_pdu_ok(#{<<"sender">> := Sender, <<"state_key">> := Sender,
                <<"origin_server_ts">> := TS, <<"content">> := Content} = JSON)
  when is_binary(Sender), is_integer(TS), is_map(Content) ->
    lists:all(fun(K) -> is_list(maps:get(K, JSON, [])) end,
              [<<"prev_events">>, <<"auth_events">>]) andalso
        is_integer(maps:get(<<"depth">>, JSON, 0));
member_pdu_ok(_) ->
    false.

%% The entry of a PDU /send could not take, under its event id
%% (computed with its room's version when the room is known; none when the
%% PDU is too broken for one).
pdu_error(PDU, Error) ->
    try
        RoomVersion =
            case mod_matrix_gw_room:get_known_room_version(PDU) of
                #room_version{} = V -> V;
                _ -> mod_matrix_gw_room:binary_to_room_version(<<"9">>)
            end,
        [{get_event_id(PDU, RoomVersion), #{<<"error">> => Error}}]
    catch _:_ ->
            []
    end.

%% What /v2/invite takes: a membership event
%% inviting a user of ours who exists, sent by a user of the origin, at most
%% ?INVITES_PER_MINUTE a minute from one origin; a direct chat's invite
%% names a room the gateway does not have (or the chat it has with that
%% sender already), so that no invite takes over a room's process.
invite_check(Host, Origin, RoomID, #{<<"sender">> := Sender, <<"state_key">> := UserID} = Event) ->
    Content = maps:get(<<"content">>, Event, #{}),
    case {maps:get(<<"type">>, Event, none), Content} of
        {<<"m.room.member">>, #{<<"membership">> := <<"invite">>}} ->
            case catch get_id_domain_exn(Sender) of
                Origin ->
                    case local_user(Host, UserID) of
                        {ok, LUser} ->
                            %%  the origin's, the invited
                            %% user's and the gateway's invites a minute
                            case mod_matrix_gw_s2s:rate({invite, Origin},
                                                        mod_matrix_gw_s2s:limit(invites, ?INVITES_PER_MINUTE),
                                                        60000, 1)
                                andalso mod_matrix_gw_s2s:rate({invite_user, LUser},
                                                               mod_matrix_gw_s2s:limit(invites_user, ?INVITES_PER_USER),
                                                               60000, 1)
                                andalso mod_matrix_gw_s2s:rate(invites_all,
                                                               mod_matrix_gw_s2s:limit(invites_all, ?INVITES_ALL),
                                                               60000, 1) of
                                true ->
                                    case Content of
                                        #{<<"is_direct">> := true} ->
                                            direct_invite_check(Host, LUser, Sender, RoomID);
                                        _ ->
                                            ok
                                    end;
                                false ->
                                    {error, 429, <<"M_LIMIT_EXCEEDED">>, <<"too many invites">>}
                            end;
                        error ->
                            {error, 404, <<"M_NOT_FOUND">>, <<"no such user">>}
                    end;
                _ ->
                    {error, 403, <<"M_FORBIDDEN">>, <<"sender not from origin">>}
            end;
        _ ->
            {error, 400, <<"M_BAD_JSON">>, <<"not an invite">>}
    end.

%% The claims of a /user/keys/claim within each of
%% our devices' claims a minute and the gateway's.
claims_allowed(Host, #{<<"one_time_keys">> := #{} = Asked}) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    Devices = [{MXID, D} || {MXID, #{} = Devs} <- maps:to_list(Asked),
                            (catch get_id_domain_exn(MXID)) == Domain,
                            D <- maps:keys(Devs)],
    PerDevice = mod_matrix_gw_s2s:limit(claims_device, ?CLAIMS_PER_DEVICE),
    Devices == []
        orelse (mod_matrix_gw_s2s:rate(claims_all, mod_matrix_gw_s2s:limit(claims_all, ?CLAIMS_ALL),
                                       60000, length(Devices))
                andalso lists:all(fun(K) -> mod_matrix_gw_s2s:rate({claims_device, K}, PerDevice, 60000, 1) end,
                                  Devices));
claims_allowed(_Host, _JSON) ->
    true.

direct_invite_check(Host, LUser, Sender, RoomID) ->
    case mnesia:dirty_read(matrix_room, RoomID) of
        [] ->
            ok;
        _ ->
            case mnesia:dirty_read(matrix_direct, {{LUser, Host}, Sender}) of
                [{matrix_direct, _, RoomID}] -> ok;
                _ -> {error, 403, <<"M_FORBIDDEN">>, <<"the room is known here">>}
            end
    end.

%% An answer of JSON; ejabberd_http has no reason phrase of its
%% own for 429 (M_LIMIT_EXCEEDED) and would close the connection unanswered.
http_json(429, JSON) ->
    {429, <<"Too Many Requests">>, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
     misc:json_encode(JSON)};
http_json(Code, JSON) ->
    mod_matrix_gw_http:json(Code, JSON).

%% The local part of a user of this server's Matrix domain who
%% exists here, or error.
local_user(Host, UserID) ->
    MatrixServer = mod_matrix_gw_opt:matrix_domain(Host),
    case mod_matrix_gw_room:parse_user_id(UserID) of
        {ok, U, MatrixServer} ->
            case jid:nodeprep(U) of
                LUser when is_binary(LUser), LUser /= <<>> ->
                    case ejabberd_auth:user_exists(LUser, Host) of
                        true -> {ok, LUser};
                        false -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.

preprocess_federation_request(Request) ->
    preprocess_federation_request(Request, true).

preprocess_federation_request(Request, _DoSignCheck) ->
    ?DEBUG("matrix federation: ~p~n", [Request]),
    case proplists:get_value('Authorization', Request#request.headers) of
        Auth when is_binary(Auth) ->
            case parse_auth(Auth) of
                #{<<"origin">> := MatrixServer,
                  <<"key">> := _,
                  <<"sig">> := _} = AuthParams ->
                    ?DEBUG("auth ~p~n", [AuthParams]),
                    if
                        Request#request.length =< ?MAX_REQUEST_SIZE ->
                            Request2 = recv_data(Request),
                            JSON =
                                if
                                    Request#request.length > 0 ->
                                        try
                                            misc:json_decode(Request2#request.data)
                                        catch
                                            _:_ ->
                                                error
                                        end;
                                    true ->
                                        none
                                end,
                            ?DEBUG("json ~p~n", [JSON]),
                            case JSON of
                                error ->
                                    {result, mod_matrix_gw_http:error(400, <<"M_NOT_JSON">>, <<"invalid JSON">>)};
                                %JSON when not DoSignCheck ->
                                %    {ok, JSON, MatrixServer};
                                JSON ->
                                    Host = ejabberd_config:get_myname(),
                                    case mod_matrix_gw_s2s:check_auth(
                                           Host, MatrixServer,
                                           AuthParams, JSON,
                                           Request2) of
                                        true ->
                                            ?DEBUG("auth ok~n", []),
                                            {ok, JSON, MatrixServer};
                                        false ->
                                            ?DEBUG("auth failed~n", []),
                                            {result, mod_matrix_gw_http:error(401, <<"M_UNAUTHORIZED">>, <<"bad signature">>)}
                                    end
                            end;
                        true ->
                            {result, mod_matrix_gw_http:error(413, <<"M_TOO_LARGE">>, <<"request too large">>)}
                    end;
                _ ->
                    {result, mod_matrix_gw_http:error(401, <<"M_UNAUTHORIZED">>, <<"bad 'Authorization' header">>)}
            end;
        undefined ->
            {result, mod_matrix_gw_http:error(401, <<"M_UNAUTHORIZED">>, <<"no 'Authorization' header">>)}
    end.

recv_data(#request{length = Len, data = Trail,
		   sockmod = SockMod, socket = Socket} = Request) ->
    NewLen = Len - byte_size(Trail),
    if NewLen > 0 ->
	    case SockMod:recv(Socket, NewLen, 60000) of
		{ok, Data} ->
                    Request#request{data = <<Trail/binary, Data/binary>>};
		{error, _} -> Request
	    end;
       true ->
	    Request
    end.


-record(state,
        {host :: binary(),
         server_host :: binary()}).

-type state() :: #state{}.

start(Host, _Opts) ->
    case mod_matrix_gw_sup:start(Host) of
	{ok, _} ->
            {ok, [{hook, s2s_out_bounce_packet, s2s_out_bounce_packet, 50},
                  {hook, user_receive_packet, user_receive_packet, 50},
                  %% a device online gets its queue (mod_matrix_gw_todevice)
                  {hook, set_presence_hook, mod_matrix_gw_todevice, set_presence_hook, 50},
                  {hook, unset_presence_hook, mod_matrix_gw_todevice, unset_presence_hook, 50},
                  %% MatrixRTC's caps gate and delayed events (mod_matrix_gw_rtc)
                  {hook, set_presence_hook, mod_matrix_gw_rtc, set_presence_hook, 50},
                  {hook, unset_presence_hook, mod_matrix_gw_rtc, unset_presence_hook, 50},
                  {hook, remove_user, mod_matrix_gw_rtc, remove_user, 50},
                  {commands, mod_matrix_gw_rtc:get_commands_spec()}]};
	Err ->
	    Err
    end.

stop(Host) ->
    Proc = mod_matrix_gw_sup:procname(Host),
    supervisor:terminate_child(ejabberd_gen_mod_sup, Proc),
    supervisor:delete_child(ejabberd_gen_mod_sup, Proc).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

start_link(Host) ->
    Proc = procname(Host),
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE, [Host],
			   ejabberd_config:fsm_limit_opts([])).

-spec init(list()) -> {ok, state()}.
init([Host]) ->
    process_flag(trap_exit, true),
    mod_matrix_gw_s2s:create_db(),
    mod_matrix_gw_room:create_db(),
    %% rooms and direct chats kept across a restart, brought
    %% back once the module is up
    mod_matrix_gw_store:create_db(),
    %% our files' mxc ids and the remote files fetched
    mod_matrix_gw_media:create_db(),
    %% the end-to-end keys (mod_matrix_gw_e2e)
    mod_matrix_gw_e2e:create_db(),
    %% each device's queue (to-device messages, key counts)
    mod_matrix_gw_todevice:create_db(),
    %% the events local users sent, by XMPP id (<event-id/>)
    mod_matrix_gw_event:create_db(),
    %% MatrixRTC's delayed events and OpenID tokens
    mod_matrix_gw_rtc:create_db(),
    %% the aliases of the rooms made here
    mod_matrix_gw_muc:create_db(),
    %% our users' avatars as mxc ids, Matrix users' profiles
    mod_matrix_gw_profile:create_db(),
    self() ! restore_rooms,
    %% a server shutting down closes every session, which would
    %% make every local user leave every Matrix room; it is no leave.
    %% ejabberdctl stop closes the sessions a second before ejabberd_stopping
    %% runs, so a session closed for system-shutdown says so too (before
    %% ejabberd_c2s's own hook, at 100, sends its unavailable presence)
    persistent_term:erase({?MODULE, stopping}),
    ejabberd_hooks:add(ejabberd_stopping, ?MODULE, stopping, 50),
    ejabberd_hooks:add(c2s_terminated, Host, ?MODULE, c2s_terminated, 50),
    %mod_matrix_gw_c2s:create_db(),
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    MyHost = gen_mod:get_opt(host, Opts),
    register_routes(Host, [MyHost]),
    gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_DISCO_INFO,
                                  ?MODULE, process_disco_info),
    gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_DISCO_ITEMS,
                                  ?MODULE, process_disco_items),
    {ok, #state{server_host = Host, host = MyHost}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
			 {reply, ok | {ok, pid()} | {error, any()}, state()} |
			 {stop, normal, ok, state()}.
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(restore_rooms, State) ->
    mod_matrix_gw_room:restore_rooms(State#state.server_host),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(term(), state()) -> any().
terminate(_Reason, #state{host = Host, server_host = ServerHost}) ->
    ejabberd_hooks:delete(ejabberd_stopping, ?MODULE, stopping, 50),
    ejabberd_hooks:delete(c2s_terminated, ServerHost, ?MODULE, c2s_terminated, 50),
    unregister_routes([Host]),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


-spec register_routes(binary(), [binary()]) -> ok.
register_routes(ServerHost, Hosts) ->
    lists:foreach(
      fun(Host) ->
	      ejabberd_router:register_route(
		Host, ServerHost, {apply, ?MODULE, route})
      end, Hosts).

unregister_routes(Hosts) ->
    lists:foreach(
      fun(Host) ->
	      ejabberd_router:unregister_route(Host)
      end, Hosts).

procname(Host) ->
    binary_to_atom(
      <<(atom_to_binary(?MODULE, latin1))/binary, "_", Host/binary>>, utf8).

parse_auth(<<"X-Matrix ", S/binary>>) ->
    parse_auth1(S, <<>>, []);
parse_auth(_) ->
    error.

parse_auth1(<<$=, Cs/binary>>, S, Ts) ->
    parse_auth2(Cs, S, <<>>, Ts);
parse_auth1(<<$,, Cs/binary>>, <<>>, Ts) -> parse_auth1(Cs, [], Ts);
parse_auth1(<<$\s, Cs/binary>>, <<>>, Ts) -> parse_auth1(Cs, [], Ts);
parse_auth1(<<C, Cs/binary>>, S, Ts) -> parse_auth1(Cs, <<S/binary, C>>, Ts);
parse_auth1(<<>>, <<>>, T) -> maps:from_list(T);
parse_auth1(<<>>, _S, _T) -> error.

parse_auth2(<<$", Cs/binary>>, Key, Val, Ts) ->
    parse_auth3(Cs, Key, Val, Ts);
parse_auth2(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse_auth4(Cs, Key, <<Val/binary, C>>, Ts);
parse_auth2(<<>>, _, _, _) -> error.

parse_auth3(<<$", Cs/binary>>, Key, Val, Ts) ->
    parse_auth4(Cs, Key, Val, Ts);
parse_auth3(<<$\\, C, Cs/binary>>, Key, Val, Ts) ->
    parse_auth3(Cs, Key, <<Val/binary, C>>, Ts);
parse_auth3(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse_auth3(Cs, Key, <<Val/binary, C>>, Ts);
parse_auth3(<<>>, _, _, _) -> error.

parse_auth4(<<$,, Cs/binary>>, Key, Val, Ts) ->
    parse_auth1(Cs, <<>>, [{Key, Val} | Ts]);
parse_auth4(<<$\s, Cs/binary>>, Key, Val, Ts) ->
    parse_auth4(Cs, Key, Val, Ts);
parse_auth4(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse_auth4(Cs, Key, <<Val/binary, C>>, Ts);
parse_auth4(<<>>, Key, Val, Ts) ->
    parse_auth1(<<>>, <<>>, [{Key, Val} | Ts]).

prune_event(#{<<"type">> := Type, <<"content">> := Content} = Event,
            RoomVersion) ->
    Keys =
        case RoomVersion#room_version.updated_redaction_rules of
            false ->
                [<<"event_id">>, <<"type">>, <<"room_id">>, <<"sender">>,
                 <<"state_key">>, <<"content">>, <<"hashes">>,
                 <<"signatures">>, <<"depth">>, <<"prev_events">>,
                 <<"prev_state">>, <<"auth_events">>, <<"origin">>,
                 <<"origin_server_ts">>, <<"membership">>];
            true ->
                [<<"event_id">>, <<"type">>, <<"room_id">>, <<"sender">>,
                 <<"state_key">>, <<"content">>, <<"hashes">>,
                 <<"signatures">>, <<"depth">>, <<"prev_events">>,
                 <<"auth_events">>, <<"origin_server_ts">>]
        end,
    Keys2 =
        case {RoomVersion#room_version.hydra, Type} of
            {true, <<"m.room.create">>} ->
                lists:delete(<<"room_id">>, Keys);
            _ ->
                Keys
        end,
    Event2 = maps:with(Keys2, Event),
    Content2 =
        case Type of
            <<"m.room.member">> ->
                C3 =
                    case RoomVersion#room_version.restricted_join_rule_fix of
                        true ->
                            maps:with([<<"membership">>,
                                       <<"join_authorised_via_users_server">>],
                                      Content);
                        false ->
                            maps:with([<<"membership">>], Content)
                    end,
                case RoomVersion#room_version.updated_redaction_rules of
                    false ->
                        C3;
                    true ->
                        case Content of
                            #{<<"third_party_invite">> :=
                                  #{<<"signed">> := InvSign}} ->
                                C3#{<<"third_party_invite">> =>
                                        #{<<"signed">> => InvSign}};
                            _ ->
                                C3
                        end
                end;
            <<"m.room.create">> ->
                case RoomVersion#room_version.updated_redaction_rules of
                    false ->
                        maps:with([<<"creator">>], Content);
                    true ->
                        Content
                end;
            <<"m.room.join_rules">> ->
                case RoomVersion#room_version.restricted_join_rule of
                    false ->
                        maps:with([<<"join_rule">>], Content);
                    true ->
                        maps:with([<<"join_rule">>, <<"allow">>], Content)
                end;
            <<"m.room.power_levels">> ->
                case RoomVersion#room_version.updated_redaction_rules of
                    false ->
                        maps:with(
                          [<<"ban">>, <<"events">>, <<"events_default">>,
                           <<"kick">>, <<"redact">>, <<"state_default">>,
                           <<"users">>, <<"users_default">>], Content);
                    true ->
                        maps:with(
                          [<<"ban">>, <<"events">>, <<"events_default">>,
                           <<"invite">>,
                           <<"kick">>, <<"redact">>, <<"state_default">>,
                           <<"users">>, <<"users_default">>], Content)
                end;
            <<"m.room.history_visibility">> ->
                maps:with([<<"history_visibility">>], Content);
            <<"m.room.redaction">> ->
                case RoomVersion#room_version.updated_redaction_rules of
                    false ->
                        #{};
                    true ->
                        maps:with([<<"redacts">>], Content)
                end;
            <<"m.room.aliases">> when RoomVersion#room_version.special_case_aliases_auth ->
                maps:with([<<"aliases">>], Content);
            _ -> #{}
        end,
    Event2#{<<"content">> := Content2}.

reference_hash(PrunedEvent) ->
    Event2 = maps:without([<<"signatures">>, <<"age_ts">>, <<"unsigned">>],
                          PrunedEvent),
    S = encode_canonical_json(Event2),
    crypto:hash(sha256, S).

content_hash(Event) ->
    Event2 = maps:without([<<"signatures">>, <<"age_ts">>, <<"unsigned">>,
                           <<"hashes">>, <<"outlier">>, <<"destinations">>],
                          Event),
    S = encode_canonical_json(Event2),
    crypto:hash(sha256, S).

get_event_id(Event, RoomVersion) ->
    PrunedEvent = prune_event(Event, RoomVersion),
    get_pruned_event_id(PrunedEvent).

get_pruned_event_id(PrunedEvent) ->
    B = base64url_encode(reference_hash(PrunedEvent)),
    <<$$, B/binary>>.

encode_canonical_json(JSON) ->
    JSON2 = sort_json(JSON),
    misc:json_encode(JSON2).

sort_json(#{} = Map) ->
    Map2 = maps:map(fun(_K, V) ->
                            sort_json(V)
                    end, Map),
    {lists:sort(maps:to_list(Map2))};
sort_json(List) when is_list(List) ->
    lists:map(fun sort_json/1, List);
sort_json(JSON) ->
    JSON.

is_canonical_json(N) when is_integer(N),
                          -16#1FFFFFFFFFFFFF =< N,
                          N =< 16#1FFFFFFFFFFFFF ->
    true;
is_canonical_json(B) when is_binary(B) ->
    true;
is_canonical_json(B) when is_boolean(B) ->
    true;
is_canonical_json(null) ->
    true;
is_canonical_json(Map) when is_map(Map) ->
    maps:fold(
      fun(_K, V, true) ->
              is_canonical_json(V);
         (_K, _V, false) ->
              false
      end, true, Map);
is_canonical_json(List) when is_list(List) ->
    lists:all(fun is_canonical_json/1, List);
is_canonical_json(_) ->
    false.


base64_decode(B) ->
    Fixed =
        case size(B) rem 4 of
            0 -> B;
            %1 -> <<B/binary, "===">>;
            2 -> <<B/binary, "==">>;
            3 -> <<B/binary, "=">>
        end,
    base64:decode(Fixed).

base64_encode(B) ->
    D = base64:encode(B),
    K = binary:longest_common_suffix([D, <<"==">>]),
    binary:part(D, 0, size(D) - K).

base64url_encode(B) ->
    D = base64_encode(B),
    D1 = binary:replace(D, <<"+">>, <<"-">>, [global]),
    binary:replace(D1, <<"/">>, <<"_">>, [global]).

sign_event(Host, Event, RoomVersion) ->
    PrunedEvent = prune_event(Event, RoomVersion),
    case sign_pruned_event(Host, PrunedEvent) of
        #{<<"signatures">> := Signatures} ->
            Event#{<<"signatures">> => Signatures}
    end.

sign_pruned_event(Host, PrunedEvent) ->
    Event2 = maps:without([<<"age_ts">>, <<"unsigned">>], PrunedEvent),
    sign_json(Host, Event2).

sign_json(Host, JSON) ->
    Signatures = maps:get(<<"signatures">>, JSON, #{}),
    JSON2 = maps:without([<<"signatures">>, <<"unsigned">>], JSON),
    Msg = encode_canonical_json(JSON2),
    SignatureName = mod_matrix_gw_opt:matrix_domain(Host),
    KeyName = mod_matrix_gw_opt:key_name(Host),
    {_PubKey, PrivKey} = mod_matrix_gw_opt:key(Host),
    KeyID = <<"ed25519:", KeyName/binary>>,
    Sig = crypto:sign(eddsa, none, Msg, [PrivKey, ed25519]),
    Sig64 = base64_encode(Sig),
    Signatures2 = Signatures#{SignatureName => #{KeyID => Sig64}},
    JSON#{<<"signatures">> => Signatures2}.

-spec send_request(
        binary(),
        get | post | put,
        binary(),
        [binary()],
        [{binary(), binary()}],
        none | #{atom() | binary() => misc:json_value()},
        [any()],
        [any()]) -> {ok, any()} | {error, any()}.

send_request(Host, Method, MatrixServer, Path, Query, JSON,
             HTTPOptions, Options) ->
    %% found by the spec's discovery (mod_matrix_gw_http:resolve/3)
    case mod_matrix_gw_s2s:get_destination(Host, MatrixServer) of
        error ->
            ?INFO_MSG("Matrix server ~s not found, ~s not sent", [MatrixServer, Path]),
            {error, {no_destination, MatrixServer}};
        Dest ->
            send_request(Host, Method, MatrixServer, Dest, Path, Query, JSON,
                         HTTPOptions, Options)
    end.

%% A request to MatrixServer at Destination (its discovery's
%% result), its certificate verified as that server's.
send_request(Host, Method, MatrixServer, Dest, Path, Query, JSON,
             HTTPOptions, Options) ->
    URI1 = iolist_to_binary(
             lists:map(fun(P) -> [$/, uri_string:quote(P)] end, Path)),
    URI =
        case Query of
            [] -> URI1;
            _ ->
                URI2 = str:join(
                         lists:map(
                           fun({K, V}) ->
                                   iolist_to_binary(
                                     [uri_string:quote(K), $=,
                                      uri_string:quote(V)])
                           end, Query), $&),
                <<URI1/binary, $?, URI2/binary>>
        end,
    #{host := MHost, port := MPort} = Dest,
    URL = <<"https://", MHost/binary,
            ":", (integer_to_binary(MPort))/binary,
            URI/binary>>,
    SMethod =
        case Method of
            get -> <<"GET">>;
            put -> <<"PUT">>;
            post -> <<"POST">>
        end,
    Auth = make_auth_header(Host, MatrixServer, SMethod, URI, JSON),
    Headers = [{"Authorization", binary_to_list(Auth)} | mod_matrix_gw_s2s:host_header(Dest)],
    Content =
        case JSON of
            none -> <<>>;
            _ -> misc:json_encode(JSON)
        end,
    Request =
        case Method of
            get ->
                {URL, Headers};
            _ ->
                {URL, Headers, "application/json;charset=UTF-8", Content}
        end,
    ?DEBUG("httpc request ~p", [{Method, Request, HTTPOptions, Options}]),
    %% the answer read up to a bound per endpoint
    %% (mod_matrix_gw_http:max_body/1, or the caller's {max_body, N})
    Max = proplists:get_value(max_body, Options, mod_matrix_gw_http:max_body(Path)),
    HTTPRes =
        mod_matrix_gw_http:request(Method,
                      Request,
                      mod_matrix_gw_http:http_options(
                        HTTPOptions, Dest, mod_matrix_gw_s2s:cacerts(Host)),
                      proplists:delete(max_body, Options), Max),
    ?DEBUG("httpc request res ~p", [HTTPRes]),
    case HTTPRes of
        {error, too_large} ->
            case mod_matrix_gw_s2s:rate({too_large_logged, MatrixServer}, 1, 60000, 1) of
                true ->
                    ?WARNING_MSG("Matrix request ~s ~s to ~s: the answer is over ~B bytes, dropped",
                                 [SMethod, URI1, MatrixServer, Max]);
                false ->
                    ok
            end;
        {error, Reason} ->
            %% an operator sees why a server cannot be reached
            ?INFO_MSG("Matrix request ~s ~s to ~s failed: ~p",
                      [SMethod, URI1, MatrixServer, Reason]);
        _ ->
            ok
    end,
    HTTPRes.

make_auth_header(Host, MatrixServer, Method, URI, Content) ->
    Origin = mod_matrix_gw_opt:matrix_domain(Host),
    JSON = #{<<"method">> => Method,
             <<"uri">> => URI,
             <<"origin">> => Origin,
             <<"destination">> => MatrixServer
            },
    JSON2 =
        case Content of
            none -> JSON;
            _ ->
                JSON#{<<"content">> => Content}
        end,
    JSON3 = sign_json(Host, JSON2),
    #{<<"signatures">> := #{Origin := #{} = KeySig}} = JSON3,
    {KeyID, Sig, _} = maps:next(maps:iterator(KeySig)),
    <<"X-Matrix origin=", Origin/binary, ",key=\"", KeyID/binary,
      "\",sig=\"", Sig/binary, "\",",
      "destination=\"", MatrixServer/binary, "\"">>.

get_id_domain_exn(B) ->
    case binary:split(B, <<":">>) of
        [_, Tail] -> Tail;
        _ -> error({invalid_id, B})
    end.

s2s_out_bounce_packet(S2SState, Pkt) ->
    #{server_host := Host} = S2SState,
    case mod_matrix_gw_opt:matrix_id_as_jid(Host) of
        false ->
            S2SState;
        true ->
            To = xmpp:get_to(Pkt),
            ServiceHost = mod_matrix_gw_opt:host(Host),
            EscU = mod_matrix_gw_room:escape(To#jid.user),
            EscS = mod_matrix_gw_room:escape(To#jid.lserver),
            NewTo = jid:make(<<EscU/binary, $%, EscS/binary>>, ServiceHost),
            ejabberd_router:route(xmpp:set_to(Pkt, NewTo)),
            {stop, ignore}
    end.

user_receive_packet({Pkt, C2SState} = Acc) ->
    #{lserver := Host} = C2SState,
    case mod_matrix_gw_opt:matrix_id_as_jid(Host) of
        false ->
            Acc;
        true ->
            ServiceHost = mod_matrix_gw_opt:host(Host),
            From = xmpp:get_from(Pkt),
            case From#jid.lserver of
                ServiceHost ->
                    case binary:split(From#jid.user, <<"%">>) of
                        [EscU, EscS] ->
                            U = mod_matrix_gw_room:unescape(EscU),
                            S = mod_matrix_gw_room:unescape(EscS),
                            NewFrom = jid:make(U, S),
                            {xmpp:set_from(Pkt, NewFrom), C2SState};
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
    end.

%% ejabberd is shutting down (the ejabberd_stopping hook, run
%% before the sessions are closed).
-spec stopping() -> ok.
stopping() ->
    persistent_term:put({?MODULE, stopping}, true),
    ok.

%% A c2s session closed with the system-shutdown stream error is
%% the server shutting down (ejabberd_sm closes every session so, for
%% `ejabberdctl stop` before ejabberd_stopping and for the server's own stop
%% after it); the flag holds until the module starts again.
-spec c2s_terminated(ejabberd_c2s:state(), term()) -> ejabberd_c2s:state().
c2s_terminated(#{stop_reason := {stream, {out, #stream_error{reason = 'system-shutdown'}}}} = State,
               _Reason) ->
    stopping(),
    State;
c2s_terminated(State, _Reason) ->
    State.

-spec is_stopping() -> boolean().
is_stopping() ->
    persistent_term:get({?MODULE, stopping}, false).

-spec route(stanza()) -> ok.
%% <media-id/> and <media-fetch/> (mod_matrix_gw_media)
route(#iq{to = #jid{luser = <<"">>, lresource = <<"">>},
          sub_els = [#xmlel{name = <<"media-", _/binary>>}]} = IQ) ->
    mod_matrix_gw_media:route_iq(IQ);
%% the device's keys (mod_matrix_gw_e2e)
route(#iq{to = #jid{luser = <<"">>, lresource = <<"">>},
          sub_els = [#xmlel{name = Name}]} = IQ)
  when Name == <<"device">>; Name == <<"keys-upload">>; Name == <<"signing-keys">>;
       Name == <<"keys-query">>; Name == <<"keys-claim">> ->
    mod_matrix_gw_e2e:route_iq(IQ);
%% <resume/> and <to-device/> (mod_matrix_gw_todevice)
route(#iq{to = #jid{luser = <<"">>, lresource = <<"">>},
          sub_els = [#xmlel{name = Name}]} = IQ)
  when Name == <<"resume">>; Name == <<"to-device">> ->
    mod_matrix_gw_todevice:route_iq(IQ);
%% MatrixRTC's iqs to the gateway (mod_matrix_gw_rtc)
route(#iq{to = #jid{luser = <<"">>, lresource = <<"">>},
          sub_els = [#xmlel{name = Name, attrs = Attrs}]} = IQ)
  when Name == <<"delayed">>; Name == <<"transports">>; Name == <<"openid">>;
       Name == <<"livekit-token">> ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        <<"https://ns.another.im/matrix-rtc/0">> -> mod_matrix_gw_rtc:route_iq(IQ);
        _ -> ejabberd_router:process_iq(IQ)
    end;
%% <event-id/> (mod_matrix_gw_event)
route(#iq{to = #jid{luser = <<"">>, lresource = <<"">>},
          sub_els = [#xmlel{name = <<"event-id">>}]} = IQ) ->
    mod_matrix_gw_event:route_iq(IQ);
route(#iq{to = #jid{luser = <<"">>, lresource = <<"">>}} = IQ) ->
    ejabberd_router:process_iq(IQ);
%% a Matrix user's vCard and presence (mod_matrix_gw_profile)
route(#iq{type = get, to = #jid{luser = <<C, _/binary>>},
          sub_els = [#xmlel{name = <<"vCard">>}]} = IQ)
  when C /= $!, C /= $# ->
    mod_matrix_gw_profile:route_iq(IQ);
route(#iq{type = get, to = #jid{luser = <<C, _/binary>>},
          sub_els = [#vcard_temp{}]} = IQ)
  when C /= $!, C /= $# ->
    mod_matrix_gw_profile:route_iq(IQ);
route(#presence{to = #jid{luser = <<C, _/binary>>}} = Pkt)
  when C /= $!, C /= $# ->
    mod_matrix_gw_profile:route_presence(Pkt);
route(Pkt) ->
    mod_matrix_gw_room:route(Pkt).

-spec process_disco_info(iq()) -> iq().
process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{type = get,
		       sub_els = [#disco_info{node = <<"">>}]} = IQ) ->
    Features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS, ?NS_MUC] ++
        mod_matrix_gw_e2e:disco_features(IQ#iq.to) ++
        mod_matrix_gw_call:disco_features(IQ#iq.to) ++
        mod_matrix_gw_rtc:disco_features(IQ#iq.to),
    Identity = #identity{category = <<"gateway">>,
			 type = <<"matrix">>},
    xmpp:make_iq_result(
      IQ, #disco_info{features = Features,
		      identities = [Identity]});
process_disco_info(#iq{type = get, lang = Lang,
		       sub_els = [#disco_info{}]} = IQ) ->
    xmpp:make_error(IQ, xmpp:err_item_not_found(?T("Node not found"), Lang));
process_disco_info(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_disco_items(iq()) -> iq().
process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get,
			sub_els = [#disco_items{node = <<>>}]} = IQ) ->
    xmpp:make_iq_result(IQ, #disco_items{});
process_disco_items(#iq{type = get, lang = Lang,
                        sub_els = [#disco_items{}]} = IQ) ->
    xmpp:make_error(IQ, xmpp:err_item_not_found(?T("Node not found"), Lang));
process_disco_items(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).


depends(_Host, _Opts) ->
    [].

mod_opt_type(host) ->
    econf:host();
mod_opt_type(matrix_domain) ->
    %% econf:domain(), not econf:host(): host() refuses a name ejabberd
    %% already routes (route_conflict), so the XMPP domain itself, the
    %% server name this gateway is meant to carry, could only be had as the
    %% default @HOST@ and never written out.
    econf:domain();
mod_opt_type(key_name) ->
    econf:binary();
mod_opt_type(key) ->
    fun(Key) ->
	    Key1 = (yconf:binary())(Key),
            Key2 = base64_decode(Key1),
            crypto:generate_key(eddsa, ed25519, Key2)
    end;
mod_opt_type(matrix_id_as_jid) ->
    econf:bool();
mod_opt_type(notary_servers) ->
    econf:list(econf:host());
mod_opt_type(leave_timeout) ->
    econf:non_neg_int();
mod_opt_type(tls_cacertfile) ->
    econf:pem();
mod_opt_type(upload_url) ->
    econf:url([http, https]);
mod_opt_type(media_max_bytes) ->
    econf:pos_int();
mod_opt_type(media_max_entries) ->
    econf:pos_int();
mod_opt_type(todevice_max) ->
    econf:pos_int(unlimited);
mod_opt_type(matrix_encrypt_dms) ->
    econf:bool();
%% MatrixRTC (mod_matrix_gw_rtc)
mod_opt_type(rtc_transports) ->
    econf:list(econf:map(econf:binary(), econf:binary()));
mod_opt_type(rtc_member_format) ->
    econf:enum([legacy, sticky, auto]);
mod_opt_type(rtc_max_delay_ms) ->
    econf:pos_int();
mod_opt_type(rtc_max_delayed_per_user) ->
    econf:pos_int().

-spec mod_options(binary()) -> [{key, {binary(), binary()}} |
                                {atom(), any()}].

mod_options(Host) ->
    [{matrix_domain, Host},
     {host, <<"matrix.", Host/binary>>},
     {key_name, <<"">>},
     {key, {<<"">>, <<"">>}},
     {matrix_id_as_jid, false},
     {notary_servers, []},
     %% a session lost (a phone's stream
     %% management expired) is ten minutes away, as a restart is
     {leave_timeout, 600},
     {tls_cacertfile, undefined},
     {upload_url, undefined},
     {media_max_bytes, 104857600},
     {media_max_entries, 100000},
     %% finite, so that no server fills a device's
     %% queue for good
     {todevice_max, 10000},
     {matrix_encrypt_dms, true},
     %% MatrixRTC (mod_matrix_gw_rtc): no focus of our own, the
     %% format the room's live members use, MSC4140's usual bounds
     {rtc_transports, []},
     {rtc_member_format, auto},
     {rtc_max_delay_ms, 86400000},
     {rtc_max_delayed_per_user, 10}].

mod_doc() ->
    #{desc =>
          [?T("https://matrix.org/[Matrix] gateway. "),
           ?T("Supports room versions 9, 10 and 11 since ejabberd 25.03; "
              "room versions 4 and higher since ejabberd 25.07; "
              "room version 12 (hydra rooms) since ejabberd 25.08. "),
           ?T("Erlang/OTP 25 or higher is required to use this module."),
           ?T("This module is available since ejabberd 24.02.")],
      note => "improved in 25.08",
      example =>
	  ["listen:",
	   "  -",
	   "    port: 8448",
	   "    module: ejabberd_http",
	   "    tls: true",
	   "    request_handlers:",
	   "      \"/_matrix\": mod_matrix_gw",
	   "",
	   "modules:",
	   "  mod_matrix_gw:",
	   "    key_name: \"key1\"",
	   "    key: \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"",
	   "    matrix_id_as_jid: true"],
      opts =>
          [{matrix_domain,
            #{value => ?T("Domain"),
              desc =>
                  ?T("Specify a domain in the Matrix federation. "
                     "The keyword '@HOST@' is replaced with the hostname. "
                     "The default value is '@HOST@'.")}},
	   {host,
            #{value => ?T("Host"),
              desc =>
                  ?T("This option defines the Jabber IDs of the service. "
                     "If the 'host' option is not specified, the Jabber ID will be "
		     "the hostname of the virtual host with the prefix '\"matrix.\"'. "
                     "The keyword '@HOST@' is replaced with the real virtual host name.")}},
	   {key_name,
            #{value => "string()",
              desc =>
                  ?T("Name of the matrix signing key.")}},
	   {key,
            #{value => "string()",
              desc =>
                  ?T("Value of the matrix signing key, in base64.")}},
	   {matrix_id_as_jid,
            #{value => "true | false",
              desc =>
                  ?T("If set to 'true', all packets failing to be delivered via an XMPP "
		     "server-to-server connection will then be routed to the Matrix gateway "
		     "by translating a Jabber ID 'user@matrixdomain.tld' to a Matrix user "
		     "identifier '@user:matrixdomain.tld'. When set to 'false', messages "
		     "must be explicitly sent to the matrix gateway service Jabber ID to be "
		     "routed to a remote Matrix server. In this case, to send a message to "
		     "Matrix user '@user:matrixdomain.tld', the client must send a message "
		     "to the JID 'user%matrixdomain.tld@matrix.myxmppdomain.tld', where "
		     "'matrix.myxmppdomain.tld' is the JID of the gateway service as set by the "
		     "'host' option. The default is 'false'.")}},
	   {notary_servers,
            #{value => "[Server, ...]",
              desc =>
                  ?T("A list of notary servers.")}},
	   {leave_timeout,
            #{value => "integer()",
              desc =>
                  ?T("Delay in seconds between a user's last session in a room ending "
                     "(her client gone, not her leave) and the 'leave' Matrix event; she "
                     "keeps her membership if she comes back in time. A leave of the room "
                     "from a session that goes on is the 'leave' at once. The default is 600 "
                     "(it was 0).")}},
	   {tls_cacertfile,
            #{value => ?T("Path"),
              desc =>
                  ?T("A PEM file of the CA certificates the other servers' "
                     "certificates are verified against, instead of the "
                     "system's store. Federation requests always verify the "
                     "other server's certificate.")}},
	   {upload_url,
            #{value => ?T("URL"),
              desc =>
                  ?T("The base URL of the XEP-0363 store's objects: a file "
                     "there shared from XMPP crosses to Matrix, and files "
                     "from Matrix are put there. The default is where "
                     "'mod_s3_upload' says its objects are read.")}},
	   {media_max_bytes,
            #{value => "integer()",
              desc =>
                  ?T("The largest file fetched from a Matrix server or "
                     "served from the store, in bytes. The default is "
                     "104857600.")}},
	   {media_max_entries,
            #{value => "integer()",
              desc =>
                  ?T("How many of the local users' files have an mxc:// "
                     "id; past it the oldest are forgotten. The default is "
                     "100000.")}},
	   {todevice_max,
            #{value => "integer() | unlimited",
              desc =>
                  ?T("How many items (to-device messages, device list "
                     "changes, key counts) the gateway keeps for one device "
                     "of a local user until the device takes them; past it "
                     "the oldest are dropped and a warning is logged once an "
                     "hour. The default is 10000.")}},
	   {matrix_encrypt_dms,
            #{value => "true | false",
              desc =>
                  ?T("Whether a direct chat that a device's encrypted "
                     "event (https://ns.another.im/matrix-e2e/0, m.room.encrypted) "
                     "creates is encrypted (m.room.encryption among its "
                     "first events). A chat a plain message or another "
                     "event creates never is; one made by the device's "
                     "<encryption/> always is. The default is 'true'.")}},
	   {rtc_transports,
            #{value => "[{type: livekit, livekit_service_url: URL} | {type: m.livekit, url: URL, server_name: Name}]",
              desc =>
                  ?T("The MatrixRTC foci the gateway names to its users' "
                     "devices (https://ns.another.im/matrix-rtc/0 <transports/>): an "
                     "lk-jwt-service for generation A calls, a LiveKit SFU "
                     "for generation B. With none, the users join calls but "
                     "start none. The default is '[]'.")}},
	   {rtc_member_format,
            #{value => "legacy | sticky | auto",
              desc =>
                  ?T("The call membership the devices write: legacy "
                     "(org.matrix.msc3401.call.member state), sticky "
                     "(m.rtc.member), auto (the room's live members', "
                     "legacy when there are none). The default is 'auto'.")}},
	   {rtc_max_delay_ms,
            #{value => "integer()",
              desc =>
                  ?T("The longest delay of a delayed event (MSC4140) a "
                     "device may schedule, in ms. The default is 86400000.")}},
	   {rtc_max_delayed_per_user,
            #{value => "integer()",
              desc =>
                  ?T("How many delayed events a user may have scheduled at "
                     "once. The default is 10.")}}
          ]
     }.

%% the EDUs of a /send transaction from Origin. Each goes to the
%% module of its type (dispatch_edu/3: the end-to-end ones, m.device_list_update
%% and m.signing_key_update to mod_matrix_gw_e2e:inbound_edu/2,
%% m.direct_to_device to mod_matrix_gw_todevice:inbound_edu/2, a clause
%% each); what no clause takes (typing, receipts, m.presence, the unknown)
%% goes to the rooms, mod_matrix_gw_room:process_edus/3, as before.
dispatch_edus(Host, Origin, EDUs) ->
    Rooms = [EDU || EDU <- EDUs, dispatch_edu(Host, Origin, EDU) == pass],
    mod_matrix_gw_room:process_edus(Host, Origin, Rooms).

%% ok: taken; pass: the rooms'. The end-to-end kinds within
%% ?E2E_EDUS_PER_SECOND a second from one origin, the rest
%% dropped with a line in the log once a minute.
dispatch_edu(Host, Origin, #{<<"edu_type">> := Type} = EDU)
  when Type == <<"m.device_list_update">>; Type == <<"m.signing_key_update">> ->
    case e2e_edu_allowed(Origin, Type) of
        true -> mod_matrix_gw_e2e:inbound_edu(Host, Origin, EDU);
        false -> ok
    end,
    ok;
dispatch_edu(Host, Origin, #{<<"edu_type">> := <<"m.direct_to_device">> = Type} = EDU) ->
    case e2e_edu_allowed(Origin, Type) of
        true -> mod_matrix_gw_todevice:inbound_edu(Host, Origin, EDU);
        false -> ok
    end,
    ok;
dispatch_edu(_Host, _Origin, _EDU) ->
    pass.

e2e_edu_allowed(Origin, Type) ->
    Max = mod_matrix_gw_s2s:limit(e2e_edus, ?E2E_EDUS_PER_SECOND),
    %%  and the gateway's, whatever the origins
    case mod_matrix_gw_s2s:rate({e2e_edus, Origin}, Max, 1000, 1)
        andalso mod_matrix_gw_s2s:rate(e2e_edus_all, mod_matrix_gw_s2s:limit(e2e_edus_all, ?E2E_EDUS_ALL),
                                       1000, 1) of
        true ->
            true;
        false ->
            case mod_matrix_gw_s2s:rate({e2e_edus_logged, Origin}, 1, 60000, 1) of
                true ->
                    ?WARNING_MSG("Matrix gateway: over ~B end-to-end EDUs a second from ~s: "
                                 "dropped (~s the latest)", [Max, Origin, Type]);
                false ->
                    ok
            end,
            false
    end.
