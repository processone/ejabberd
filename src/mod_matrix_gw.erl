%%%----------------------------------------------------------------------
%%% File    : mod_matrix_gw.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Matrix gateway
%%% Created : 23 Apr 2022 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
-ifndef(OTP_BELOW_24).

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
         get_id_domain_exn/1,
         base64_decode/1, base64_encode/1,
         prune_event/2, get_event_id/2, content_hash/1,
         sign_event/3, sign_pruned_event/2, sign_json/2,
         send_request/8, s2s_out_bounce_packet/2, user_receive_packet/1,
         route/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("translate.hrl").
-include("ejabberd_web_admin.hrl").
-include("mod_matrix_gw.hrl").

-define(MAX_REQUEST_SIZE, 1000000).

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
     jiffy:encode(SJSON)};
process([<<"federation">>, <<"v1">>, <<"version">>],
        #request{method = 'GET', host = _Host} = _Request) ->
    JSON = #{<<"server">> => #{<<"name">> => <<"ejabberd/mod_matrix_gw">>,
                               <<"version">> => <<"0.1">>}},
    {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
     jiffy:encode(JSON)};
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
                    {400, [], <<"400 Bad Request: bad 'field' parameter">>};
                _ ->
                    case preprocess_federation_request(Request) of
                        {ok, _JSON, _Origin} ->
                            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}], <<"{}">>};
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
            Res = #{<<"devices">> =>
                        [#{<<"device_id">> => <<"ejabberd/mod_matrix_gw">>,
                           <<"keys">> => []}],
                    <<"stream_id">> => 1,
                    <<"user_id">> => UserID},
            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}], jiffy:encode(Res)};
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"user">>, <<"keys">>, <<"query">>],
        #request{method = 'POST', host = _Host} = Request) ->
    case preprocess_federation_request(Request, false) of
        {ok, #{<<"device_keys">> := DeviceKeys}, _Origin} ->
            DeviceKeys2 = maps:map(fun(_Key, _) -> #{} end, DeviceKeys),
            Res = #{<<"device_keys">> => DeviceKeys2},
            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
             jiffy:encode(Res)};
        {ok, _JSON, _Origin} ->
            {400, [], <<"400 Bad Request: invalid format">>};
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v2">>, <<"invite">>, RoomID, EventID],
        #request{method = 'PUT', host = _Host} = Request) ->
    case preprocess_federation_request(Request) of
        {ok, #{<<"event">> := #{%<<"origin">> := Origin,
                                <<"room_id">> := RoomID,
                                <<"sender">> := Sender,
                                <<"state_key">> := UserID} = Event,
               <<"room_version">> := RoomVer},
         Origin} ->
            case mod_matrix_gw_room:binary_to_room_version(RoomVer) of
                #room_version{} = RoomVersion ->
                    %% TODO: check type and userid
                    Host = ejabberd_config:get_myname(),
                    PrunedEvent = prune_event(Event, RoomVersion),
                    ?DEBUG("invite ~p~n", [{RoomID, EventID, Event, RoomVer, catch mod_matrix_gw_s2s:check_signature(Host, PrunedEvent), get_pruned_event_id(PrunedEvent)}]),
                    case mod_matrix_gw_s2s:check_signature(Host, PrunedEvent) of
                        true ->
                            case get_pruned_event_id(PrunedEvent) of
                                EventID ->
                                    SEvent = sign_pruned_event(Host, PrunedEvent),
                                    ?DEBUG("sign event ~p~n", [SEvent]),
                                    ResJSON = #{<<"event">> => SEvent},
                                    mod_matrix_gw_room:join(Host, Origin, RoomID, Sender, UserID),
                                    ?DEBUG("res ~s~n", [jiffy:encode(ResJSON)]),
                                    {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}], jiffy:encode(ResJSON)};
                                _ ->
                                    {400, [], <<"400 Bad Request: bad event id">>}
                            end;
                        false ->
                            {400, [], <<"400 Bad Request: signature check failed">>}
                    end;
                false ->
                    {400, [], <<"400 Bad Request: unsupported room version">>}
            end;
        {ok, _JSON, _Origin} ->
            {400, [], <<"400 Bad Request: invalid format">>};
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"send">>, _TxnID],
        #request{method = 'PUT', host = _Host} = Request) ->
    case preprocess_federation_request(Request, false) of
        {ok, #{<<"origin">> := Origin,
               <<"pdus">> := PDUs} = JSON,
         Origin} ->
            ?DEBUG("send request ~p~n", [JSON]),
            Host = ejabberd_config:get_myname(),
            Res = lists:map(
                    fun(PDU) ->
                            case mod_matrix_gw_room:process_pdu(Host, Origin, PDU) of
                                {ok, EventID} -> {EventID, #{}};
                                {error, Error} ->
                                    {get_event_id(PDU, mod_matrix_gw_room:binary_to_room_version(<<"9">>)),
                                     #{<<"error">> => Error}}
                            end
                    end, PDUs),
            ?DEBUG("send res ~p~n", [Res]),
            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
             jiffy:encode(maps:from_list(Res))};
        {ok, _JSON, _Origin} ->
            {400, [], <<"400 Bad Request: invalid format">>};
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"get_missing_events">>, RoomID],
        #request{method = 'POST', host = _Host} = Request) ->
    case preprocess_federation_request(Request, false) of
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
             jiffy:encode(Res)};
        {ok, _JSON, _Origin} ->
            {400, [], <<"400 Bad Request: invalid format">>};
        {result, HTTPResult} ->
            HTTPResult
    end;
process([<<"federation">>, <<"v1">>, <<"backfill">>, RoomID],
        #request{method = 'GET', host = _Host} = Request) ->
    case catch binary_to_integer(proplists:get_value(<<"limit">>, Request#request.q)) of
        Limit when is_integer(Limit) ->
            case preprocess_federation_request(Request, false) of
                {ok, _JSON, Origin} ->
                    LatestEvents = proplists:get_all_values(<<"v">>, Request#request.q),
                    ?DEBUG("backfill request ~p~n", [{Limit, LatestEvents}]),
                    Host = ejabberd_config:get_myname(),
                    PDUs1 = mod_matrix_gw_room:get_missing_events(
                              Host, Origin, RoomID, [], LatestEvents, Limit, 0),
                    PDUs2 = lists:flatmap(
                              fun(EventID) ->
                                      case mod_matrix_gw_room:get_event(Host, RoomID, EventID) of
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
                     jiffy:encode(Res)};
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
                             jiffy:encode(Res)};
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
        {ok, _JSON, _Origin} ->
            Host = ejabberd_config:get_myname(),
            %% TODO: very inefficient, replace with an SQL call
            PDU =
                lists:foldl(
                  fun(RoomID, undefined) ->
                          case mod_matrix_gw_room:get_event(Host, RoomID, EventID) of
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
                     jiffy:encode(Res)}
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
                             jiffy:encode(Res)};
                        {error, not_invited} ->
                            Res = #{<<"errcode">> => <<"M_FORBIDDEN">>,
                                    <<"error">> => <<"You are not invited to this room">>},
                            {403, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             jiffy:encode(Res)};
                        {error, {incompatible_version, Ver}} ->
                            Res = #{<<"errcode">> => <<"M_INCOMPATIBLE_ROOM_VERSION">>,
                                    <<"error">> => <<"Your homeserver does not support the features required to join this room">>,
                                    <<"room_version">> => Ver},
                            {400, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             jiffy:encode(Res)};
                        {ok, Res} ->
                            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             jiffy:encode(Res)}
                    end;
                _ ->
                    Res = #{<<"errcode">> => <<"M_FORBIDDEN">>,
                            <<"error">> => <<"User not from origin">>},
                    {403, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                     jiffy:encode(Res)}
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
            case get_id_domain_exn(Sender) of
                Origin ->
                    case mod_matrix_gw_room:send_join(Host, Origin, RoomID, EventID, JSON) of
                        {error, Error} when is_binary(Error) ->
                            Res = #{<<"errcode">> => <<"M_BAD_REQUEST">>,
                                    <<"error">> => Error},
                            {403, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             jiffy:encode(Res)};
                        {ok, Res} ->
                            ?DEBUG("send_join res: ~p~n", [Res]),
                            {200, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                             jiffy:encode(Res)}
                    end;
                _ ->
                    Res = #{<<"errcode">> => <<"M_FORBIDDEN">>,
                            <<"error">> => <<"User not from origin">>},
                    {403, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
                     jiffy:encode(Res)}
            end;
        {ok, _JSON, _Origin} ->
            Res = #{<<"errcode">> => <<"M_BAD_REQUEST">>,
                    <<"error">> => <<"Invalid event format">>},
            {400, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}],
             jiffy:encode(Res)};
        {result, HTTPResult} ->
            HTTPResult
    end;
process(_Path, _Request) ->
    ?DEBUG("matrix 404: ~p~n~p~n", [_Path, _Request]),
    ejabberd_web:error(not_found).

preprocess_federation_request(Request) ->
    preprocess_federation_request(Request, true).

preprocess_federation_request(Request, DoSignCheck) ->
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
                                            jiffy:decode(Request2#request.data,
                                                         [return_maps])
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
                                    {result, {400, [], <<"400 Bad Request: invalid JSON">>}};
                                JSON when not DoSignCheck ->
                                    {ok, JSON, MatrixServer};
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
                                            {result, {401, [], <<"401 Unauthorized">>}}
                                    end
                            end;
                        true ->
                            {result, {400, [], <<"400 Bad Request: size limit">>}}
                    end;
                _ ->
                    {result, {400, [], <<"400 Bad Request: bad 'Authorization' header">>}}
            end;
        undefined ->
            {result, {400, [], <<"400 Bad Request: no 'Authorization' header">>}}
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
                  {hook, user_receive_packet, user_receive_packet, 50}]};
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
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    MyHost = gen_mod:get_opt(host, Opts),
    register_routes(Host, [MyHost]),
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
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(term(), state()) -> any().
terminate(_Reason, #state{host = Host}) ->
    unregister_routes([Host]).

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
    Event2 =
        case RoomVersion#room_version.updated_redaction_rules of
            false ->
                maps:with(
                  [<<"event_id">>, <<"type">>, <<"room_id">>, <<"sender">>,
                   <<"state_key">>, <<"content">>, <<"hashes">>,
                   <<"signatures">>, <<"depth">>, <<"prev_events">>,
                   <<"prev_state">>, <<"auth_events">>, <<"origin">>,
                   <<"origin_server_ts">>, <<"membership">>], Event);
            true ->
                maps:with(
                  [<<"event_id">>, <<"type">>, <<"room_id">>, <<"sender">>,
                   <<"state_key">>, <<"content">>, <<"hashes">>,
                   <<"signatures">>, <<"depth">>, <<"prev_events">>,
                   <<"auth_events">>, <<"origin_server_ts">>], Event)
        end,
    Content2 =
        case Type of
            <<"m.room.member">> ->
                C3 = maps:with([<<"membership">>,
                                <<"join_authorised_via_users_server">>],
                               Content),
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
                maps:with([<<"join_rule">>, <<"allow">>], Content);
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
    jiffy:encode(JSON2).

sort_json(#{} = Map) ->
    Map2 = maps:map(fun(_K, V) ->
                            sort_json(V)
                    end, Map),
    {lists:sort(maps:to_list(Map2))};
sort_json(List) when is_list(List) ->
    lists:map(fun sort_json/1, List);
sort_json(JSON) ->
    JSON.

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
        none | #{atom() | binary() => jiffy:json_value()},
        [any()],
        [any()]) -> {ok, any()} | {error, any()}.

send_request(Host, Method, MatrixServer, Path, Query, JSON,
             HTTPOptions, Options) ->
    URI1 = iolist_to_binary(
             lists:map(fun(P) -> [$/, misc:uri_quote(P)] end, Path)),
    URI =
        case Query of
            [] -> URI1;
            _ ->
                URI2 = str:join(
                         lists:map(
                           fun({K, V}) ->
                                   iolist_to_binary(
                                     [misc:uri_quote(K), $=,
                                      misc:uri_quote(V)])
                           end, Query), $&),
                <<URI1/binary, $?, URI2/binary>>
        end,
    {MHost, MPort} = mod_matrix_gw_s2s:get_matrix_host_port(Host, MatrixServer),
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
    Headers = [{"Authorization", binary_to_list(Auth)}],
    Content =
        case JSON of
            none -> <<>>;
            _ -> jiffy:encode(JSON)
        end,
    Request =
        case Method of
            get ->
                {URL, Headers};
            _ ->
                {URL, Headers, "application/json;charset=UTF-8", Content}
        end,
    httpc:request(Method,
                  Request,
                  HTTPOptions,
                  Options).

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

route(Pkt) ->
    mod_matrix_gw_room:route(Pkt).

depends(_Host, _Opts) ->
    [].

mod_opt_type(host) ->
    econf:host();
mod_opt_type(matrix_domain) ->
    econf:host();
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
mod_opt_type(persist) ->
    econf:bool().

mod_options(Host) ->
    [{matrix_domain, Host},
     {host, <<"matrix.", Host/binary>>},
     {key_name, <<"">>},
     {key, {<<"">>, <<"">>}},
     {matrix_id_as_jid, false},
     {persist, false}].

mod_doc() ->
    #{desc =>
          [?T("https://matrix.org/[Matrix] gateway."), "",
           ?T("This module is available since ejabberd 24.02.")],
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
                  ?T("If set to 'false', all packets failing to be delivered via an XMPP "
		     "server-to-server connection will then be routed to the Matrix gateway "
		     "by translating a Jabber ID 'user@matrixdomain.tld' to a Matrix user "
		     "identifier '@user:matrixdomain.tld'. When set to 'true', messages "
		     "must be explicitly sent to the matrix gateway service Jabber ID to be "
		     "routed to a remote Matrix server. In this case, to send a message to "
		     "Matrix user '@user:matrixdomain.tld', the client must send a message "
		     "to the JID 'user%matrixdomain.tld@matrix.myxmppdomain.tld', where "
		     "'matrix.myxmppdomain.tld' is the JID of the gateway service as set by the "
		     "'host' option. The default is 'false'.")}}
          ]
     }.
-endif.
