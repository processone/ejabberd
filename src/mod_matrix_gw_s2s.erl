%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Matrix S2S
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
-module(mod_matrix_gw_s2s).
-behaviour(gen_statem).

%% API
-export([start_link/2, supervisor/1, create_db/0,
         get_connection/2, check_auth/5, check_signature/3,
         get_matrix_host_port/2, get_destination/2, queue_edu/3,
         cacerts/1]).

%% gen_statem callbacks
-export([init/1, terminate/3, code_change/4, callback_mode/0]).
-export([host_header/1]).
-export([handle_event/4]).

-include("logger.hrl").
-include("ejabberd_http.hrl").
-include_lib("kernel/include/inet.hrl").
-include("mod_matrix_gw.hrl").

-record(matrix_s2s,
        {to  :: binary(),
         pid :: pid()}).

-record(pending,
        {request_id :: any(),
         servers :: [binary()],
         key_queue = []}).

-record(wait,
        {timer_ref :: reference(),
         last :: integer()}).

-record(data,
        {host :: binary(),
         matrix_server :: binary(),
         matrix_host_port :: {binary(), integer()} | undefined,
         %% where the server is (mod_matrix_gw_http:resolve/3)
         %% and until when that holds; the EDUs waiting for a transaction
         %% and the one in flight {RequestID, TxnID, EDUs, Tries}
         destination :: {mod_matrix_gw_http:destination() | error, integer()} | undefined,
         edu_queue = [] :: [map()],
         edu_txn :: {any(), binary(), [map()], integer()} | undefined,
         keys = #{},
         state :: #pending{} | #wait{}}).

-define(KEYS_REQUEST_TIMEOUT, 600000).
%% a server's discovery is kept a day (the spec's default for a
%% .well-known without cache headers), an hour when it found nothing
-define(DESTINATION_TTL, timer:hours(24)).
-define(DESTINATION_ERROR_TTL, timer:hours(1)).
%% EDUs: at most 100 in a transaction (the spec's limit), five tries each
%% transaction, 30 seconds apart, as the rooms retry their PDUs
-define(MAX_EDUS, 100).
-define(MAX_EDU_TRIES, 5).
-define(EDU_RETRY, 30000).

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
start_link(Host, MatrixServer) ->
    gen_statem:start_link(?MODULE, [Host, MatrixServer],
                          ejabberd_config:fsm_limit_opts([])).

-spec supervisor(binary()) -> atom().
supervisor(Host) ->
    gen_mod:get_module_proc(Host, mod_matrix_gw_s2s_sup).

create_db() ->
    ejabberd_mnesia:create(
      ?MODULE, matrix_s2s,
      [{ram_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_s2s)}]),
    ok.

get_connection(Host, MatrixServer) ->
    case mnesia:dirty_read(matrix_s2s, MatrixServer) of
	[] ->
	    try
                Proc = gen_mod:get_module_proc_check(Host, mod_matrix_gw_s2s_sup),
                supervisor:start_child(Proc, [Host, MatrixServer])
            of
		{ok, undefined} -> {error, ignored};
		Res -> Res
            catch
                error:badarg ->
                    {error, wrong_host}
	    end;
        [#matrix_s2s{pid = Pid}] ->
            {ok, Pid}
    end.

get_key(Host, MatrixServer, KeyID) ->
    case mod_matrix_gw_opt:matrix_domain(Host) of
        MatrixServer ->
            {PubKey, _PrivKey} = mod_matrix_gw_opt:key(Host),
            TS = erlang:system_time(millisecond) + timer:hours(24 * 7),
            {ok, PubKey, TS};
        _ ->
            case get_connection(Host, MatrixServer) of
                {ok, S2SPid} ->
                    gen_statem:call(S2SPid, {get_key, KeyID});
                Error -> Error
            end
    end.

get_matrix_host_port(Host, MatrixServer) ->
    case get_destination(Host, MatrixServer) of
        #{host := H, port := P} -> {H, P};
        Error -> Error
    end.

%% Where MatrixServer's federation API is, with the name its
%% certificate must carry (mod_matrix_gw_http:resolve/3), or error.
-spec get_destination(binary(), binary()) -> mod_matrix_gw_http:destination() | error.
get_destination(Host, MatrixServer) ->
    case mod_matrix_gw_opt:matrix_domain(Host) of
        MatrixServer ->
            error;
        _ ->
            case get_connection(Host, MatrixServer) of
                {ok, S2SPid} ->
                    try gen_statem:call(S2SPid, get_destination, 30000)
                    catch exit:_ -> error
                    end;
                _ -> error
            end
    end.

%% An EDU for MatrixServer, sent in a transaction of its own
%% with whatever else is queued for that server (at most 100 EDUs),
%% retried as the rooms' PDUs are.
-spec queue_edu(binary(), binary(), map()) -> ok.
queue_edu(Host, MatrixServer, EDU) ->
    case mod_matrix_gw_opt:matrix_domain(Host) of
        MatrixServer ->
            ok;
        _ ->
            case get_connection(Host, MatrixServer) of
                {ok, S2SPid} -> gen_statem:cast(S2SPid, {queue_edu, EDU});
                _ -> ok
            end
    end.

%% The CA certificates federation requests are verified against.
-spec cacerts(binary()) -> [public_key:der_encoded()].
cacerts(Host) ->
    mod_matrix_gw_http:cacerts(mod_matrix_gw_opt:tls_cacertfile(Host)).


%process_query(Host, MatrixServer, AuthParams, Query, JSON, Request) ->
%    case get_connection(Host, MatrixServer) of
%        {ok, S2SPid} ->
%            #request{sockmod = SockMod, socket = Socket} = Request,
%            SockMod:controlling_process(Socket, S2SPid),
%            gen_statem:cast(S2SPid, {query, AuthParams, Query, JSON, Request}),
%            ok;
%        {error, _} = Error ->
%            Error
%    end.

check_auth(Host, MatrixServer, AuthParams, Content, Request) ->
    Destination = mod_matrix_gw_opt:matrix_domain(Host),
    case get_connection(Host, MatrixServer) of
        {ok, _} when map_get(<<"destination">>, AuthParams) /= Destination,
                     is_map_key(<<"destination">>, AuthParams) ->
            %% a request signed for another server is not ours
            ?WARNING_MSG("Request from ~s for another destination: ~p",
                         [MatrixServer, maps:get(<<"destination">>, AuthParams)]),
            false;
        {ok, S2SPid} ->
            #{<<"key">> := KeyID} = AuthParams,
            Now = erlang:system_time(millisecond),
            case catch gen_statem:call(S2SPid, {get_key, KeyID}) of
                {ok, VerifyKey, ValidUntil} when ValidUntil >= Now ->
                    #{<<"sig">> := Sig} = AuthParams,
                    JSON = #{<<"method">> => atom_to_binary(Request#request.method, latin1),
                             <<"uri">> => Request#request.raw_path,
                             <<"origin">> => MatrixServer,
                             <<"destination">> => Destination,
                             <<"signatures">> => #{
                                 MatrixServer => #{KeyID => Sig}
                                }
                            },
                    JSON2 =
                        case Content of
                            none -> JSON;
                            _ ->
                                JSON#{<<"content">> => Content}
                        end,
                    case check_signature(JSON2, MatrixServer, KeyID, VerifyKey) of
                        true ->
                            true;
                        false ->
                            ?WARNING_MSG("Failed authentication: ~p", [JSON2]),
                            false
                    end;
                _ ->
                    false
            end;
        {error, _} = _Error ->
            false
    end.

check_signature(Host, JSON, RoomVersion) ->
    case JSON of
        #{<<"sender">> := Sender,
          <<"signatures">> := Sigs,
          <<"origin_server_ts">> := OriginServerTS} ->
            MatrixServer = mod_matrix_gw:get_id_domain_exn(Sender),
            case Sigs of
                #{MatrixServer := #{} = KeySig} ->
                    case maps:next(maps:iterator(KeySig)) of
                        {KeyID, _Sig, _} ->
                            case catch get_key(Host, MatrixServer, KeyID) of
                                {ok, VerifyKey, ValidUntil} ->
                                    if
                                        not RoomVersion#room_version.enforce_key_validity or
                                        (OriginServerTS =< ValidUntil) ->
                                            case check_signature(JSON, MatrixServer, KeyID, VerifyKey) of
                                                true ->
                                                    true;
                                                false ->
                                                    ?WARNING_MSG("Failed authentication: ~p", [JSON]),
                                                    false
                                            end;
                                        true ->
                                            false
                                    end;
                                _ ->
                                    false
                            end;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.


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
init([Host, MatrixServer]) ->
    mnesia:dirty_write(
      #matrix_s2s{to = MatrixServer,
                  pid = self()}),
    {ok, state_name,
     request_keys(
       MatrixServer,
       #data{host = Host,
             matrix_server = MatrixServer,
             state = #wait{timer_ref = make_ref(), last = 0}})}.

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
%handle_event({call, From}, _Msg, State, Data) ->
%    {next_state, State, Data, [{reply, From, ok}]}.
handle_event({call, From}, get_destination, _State, Data) ->
    {Dest, Data2} = local_destination(Data),
    {keep_state, Data2, [{reply, From, Dest}]};
handle_event(cast, {queue_edu, EDU}, _State, Data) ->
    {keep_state, maybe_send_edus(Data#data{edu_queue = Data#data.edu_queue ++ [EDU]})};
handle_event(info, {edu_txn_res, RequestID, Res}, _State,
             #data{edu_txn = {RequestID, TxnID, EDUs, Tries}} = Data) ->
    case Res of
        {{_, 200, _}, _, _} ->
            {keep_state, maybe_send_edus(Data#data{edu_txn = undefined})};
        _ when Tries < ?MAX_EDU_TRIES ->
            ?DEBUG("EDU transaction ~s to ~s failed (~p), retrying",
                   [TxnID, Data#data.matrix_server, Res]),
            erlang:send_after(?EDU_RETRY, self(), resend_edus),
            {keep_state, Data#data{edu_txn = {undefined, TxnID, EDUs, Tries}}};
        _ ->
            ?INFO_MSG("EDU transaction ~s to ~s dropped after ~B tries: ~p",
                      [TxnID, Data#data.matrix_server, Tries, Res]),
            {keep_state, maybe_send_edus(Data#data{edu_txn = undefined})}
    end;
handle_event(info, resend_edus, _State, #data{edu_txn = {undefined, TxnID, EDUs, Tries}} = Data) ->
    {keep_state, send_edu_txn(TxnID, EDUs, Tries + 1, Data)};
handle_event({call, From}, get_matrix_host_port, _State, Data) ->
    case Data#data.matrix_host_port of
        undefined ->
            Result = do_get_matrix_host_port(Data#data.matrix_server),
            Data2 = Data#data{matrix_host_port = Result},
            {keep_state, Data2, [{reply, From, Result}]};
        Result ->
            {keep_state_and_data, [{reply, From, Result}]}
    end;
handle_event({call, From}, {get_key, KeyID}, State, Data) ->
    Now = erlang:system_time(millisecond),
    case maps:find(KeyID, Data#data.keys) of
        {ok, {Key, ValidUntil}} when ValidUntil >= Now ->
            {keep_state, Data, [{reply, From, {ok, Key, ValidUntil}}]};
        {ok, {Key, ValidUntil}} ->
            %% the key's valid_until_ts has passed (26.09's
            %% TODO). An event signed before then still verifies with it,
            %% a request does not: fetch the server's keys again, unless a
            %% fetch is under way or the last one, under a minute ago,
            %% brought nothing newer
            case Data#data.state of
                #pending{key_queue = KeyQueue} = St ->
                    {next_state, State,
                     Data#data{state = St#pending{key_queue = [{From, KeyID} | KeyQueue]}}, []};
                #wait{last = Last} when Last + 60000 > Now ->
                    {keep_state, Data, [{reply, From, {ok, Key, ValidUntil}}]};
                #wait{} ->
                    Data2 = request_keys(Data#data.matrix_server, Data),
                    #pending{key_queue = KeyQueue} = St = Data2#data.state,
                    {next_state, State,
                     Data2#data{state = St#pending{key_queue = [{From, KeyID} | KeyQueue]}}, []}
            end;
        error ->
            case Data#data.state of
                #pending{key_queue = KeyQueue} = St ->
                    KeyQueue2 = [{From, KeyID} | KeyQueue],
                    {next_state, State,
                     Data#data{state = St#pending{key_queue = KeyQueue2}}, []};
                #wait{timer_ref = TimerRef, last = Last} ->
                    TS = erlang:system_time(millisecond),
                    if
                        Last + ?KEYS_REQUEST_TIMEOUT =< TS ->
                            Data2 = request_keys(Data#data.matrix_server, Data),
                            #pending{key_queue = KeyQueue} = St = Data2#data.state,
                            KeyQueue2 = [{From, KeyID} | KeyQueue],
                            {next_state, State,
                             Data2#data{state = St#pending{key_queue = KeyQueue2}}, []};
                        true ->
                            Timeout =
                                case erlang:read_timer(TimerRef) of
                                    false ->
                                        Last + ?KEYS_REQUEST_TIMEOUT - TS;
                                    Left ->
                                        erlang:cancel_timer(TimerRef),
                                        min(Left,
                                            Last + ?KEYS_REQUEST_TIMEOUT - TS)
                                end,
                            TRef = erlang:start_timer(Timeout, self(), []),
                            {next_state, State,
                             Data#data{state = #wait{timer_ref = TRef,
                                                     last = Last}},
                             [{reply, From, error}]}
                    end
            end
    end;
handle_event(cast, {key_reply, RequestID, HTTPResult}, State,
             #data{state = #pending{request_id = RequestID,
                                    servers = Servers,
                                    key_queue = KeyQueue} = St} = Data) ->
    TS = erlang:system_time(millisecond),
    Res =
        case HTTPResult of
            {{_, 200, _}, _, SJSON} ->
                try
                    JSON1 = misc:json_decode(SJSON),
                    JSON =
                        case JSON1 of
                            #{<<"server_keys">> := [J]} -> J;
                            _ -> JSON1
                        end,
                    ?DEBUG("keys ~p~n", [JSON]),
                    #{<<"verify_keys">> := VerifyKeys} = JSON,
                    %% the keys are the server's we asked about
                    #{<<"server_name">> := ServerName} = JSON,
                    ServerName = Data#data.matrix_server,
                    {KeyID, KeyData, _} = maps:next(maps:iterator(VerifyKeys)),
                    #{<<"key">> := SKey} = KeyData,
                    VerifyKey = mod_matrix_gw:base64_decode(SKey),
                    ?DEBUG("key ~p~n", [VerifyKey]),
                    ?DEBUG("check ~p~n",
                           [catch check_signature(
                                    JSON, Data#data.matrix_server,
                                    KeyID, VerifyKey)]),
                    true = check_signature(
                             JSON, Data#data.matrix_server,
                             KeyID, VerifyKey),
                    #{<<"valid_until_ts">> := ValidUntil} = JSON,
                    ValidUntil2 =
                        min(ValidUntil,
                            erlang:system_time(millisecond) + timer:hours(24 * 7)),
                    OldKeysJSON =
                        case JSON of
                            #{<<"old_verify_keys">> := OldKeysJ}
                              when is_map(OldKeysJ) ->
                                OldKeysJ;
                            _ ->
                                #{}
                        end,
                    OldKeys =
                        maps:filtermap(
                          fun(_KID,
                              #{<<"key">> := SK,
                                <<"expired_ts">> := Exp})
                                when is_integer(Exp),
                                     is_binary(SK) ->
                                  {true, {mod_matrix_gw:base64_decode(SK),
                                          Exp}};
                             (_, _) -> false
                          end, OldKeysJSON),
                    NewKeys =
                        maps:filtermap(
                          fun(_KID,
                              #{<<"key">> := SK})
                                when is_binary(SK) ->
                                  {true, {mod_matrix_gw:base64_decode(SK),
                                          ValidUntil2}};
                             (_, _) -> false
                          end, VerifyKeys),
                    {ok, maps:merge(OldKeys, NewKeys), ValidUntil2}
                catch
                    _:_ ->
                        {ok, Data#data.keys, TS + ?KEYS_REQUEST_TIMEOUT}
                end;
            _ ->
                case Servers of
                    [] ->
                        {ok, Data#data.keys, TS + ?KEYS_REQUEST_TIMEOUT};
                    [S | Servers1] ->
                        {error,
                         request_keys(
                           S,
                           Data#data{state = St#pending{servers = Servers1}})}
                end
        end,
    case Res of
        {ok, Keys, ValidTS} ->
            Replies =
                lists:map(
                  fun({From, KeyID}) ->
                          case maps:find(KeyID, Keys) of
                              {ok, {Key, KeyValidUntil}} ->
                                  {reply, From, {ok, Key, KeyValidUntil}};
                              error ->
                                  {reply, From, error}
                          end
                  end,
                  KeyQueue),
            TimerRef = erlang:start_timer(max(ValidTS - TS, ?KEYS_REQUEST_TIMEOUT),
                                          self(), []),
            Data2 = Data#data{keys = Keys,
                              state = #wait{timer_ref = TimerRef,
                                            last = TS}},
            ?DEBUG("KEYS ~p~n", [{Keys, Data2}]),
            {next_state, State, Data2, Replies};
        {error, Data2} ->
            {next_state, State, Data2, []}
    end;
handle_event(info, {timeout, TimerRef, []}, State,
             #data{state = #wait{timer_ref = TimerRef}} = Data) ->
    Data2 = request_keys(Data#data.matrix_server, Data),
    {next_state, State, Data2, []};
handle_event(cast, Msg, State, Data) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {next_state, State, Data, []};
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
terminate(_Reason, _State, Data) ->
    mnesia:dirty_delete_object(
      #matrix_s2s{to = Data#data.matrix_server,
                  pid = self()}),
    %% TODO: wait for messages
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

%% The discovery of mod_matrix_gw_http:resolve/3, its
%% .well-known fetched over verified TLS and SRV looked up for
%% _matrix-fed._tcp then _matrix._tcp.
resolve_destination(Host, MatrixServer) ->
    CACerts = cacerts(Host),
    WellKnown =
        fun(Name) ->
                URL = <<"https://", Name/binary, "/.well-known/matrix/server">>,
                case httpc:request(get, {URL, []},
                                   [{timeout, 5000}, {connect_timeout, 5000},
                                    {ssl, mod_matrix_gw_http:tls_options(Name, CACerts)}],
                                   [{sync, true}, {body_format, binary}]) of
                    {ok, {{_, 200, _}, _Headers, Body}} -> Body;
                    _ -> error
                end
        end,
    Srv =
        fun(Name) ->
                case inet_res:getbyname(Name, srv, 5000) of
                    {ok, {hostent, _, _, _, _, Records}} -> Records;
                    _ -> []
                end
        end,
    Res = mod_matrix_gw_http:resolve(MatrixServer, WellKnown, Srv),
    ?DEBUG("~s is at ~p", [MatrixServer, Res]),
    Res.

maybe_send_edus(#data{edu_txn = undefined, edu_queue = [_ | _] = Queue} = Data) ->
    {EDUs, Rest} = lists:split(min(?MAX_EDUS, length(Queue)), Queue),
    send_edu_txn(p1_rand:get_string(), EDUs, 1, Data#data{edu_queue = Rest});
maybe_send_edus(Data) ->
    Data.

send_edu_txn(TxnID, EDUs, Tries, Data) ->
    Host = Data#data.host,
    Body = #{<<"origin">> => mod_matrix_gw_opt:matrix_domain(Host),
             <<"origin_server_ts">> => erlang:system_time(millisecond),
             <<"pdus">> => [],
             <<"edus">> => EDUs},
    Self = self(),
    Receiver = fun({RequestID, Res}) -> Self ! {edu_txn_res, RequestID, Res} end,
    %% the destination from this process's own cache: send_request/8 would
    %% ask this very process for it
    {Dest, Data1} = local_destination(Data),
    case catch send_with(Dest,
                 Host, put, Data#data.matrix_server,
                 [<<"_matrix">>, <<"federation">>, <<"v1">>, <<"send">>, TxnID],
                 [], Body, [{timeout, 5000}, {connect_timeout, 5000}],
                 [{sync, false}, {receiver, Receiver}]) of
        {ok, RequestID} ->
            Data1#data{edu_txn = {RequestID, TxnID, EDUs, Tries}};
        Err ->
            ?DEBUG("EDU transaction to ~s not sent: ~p", [Data#data.matrix_server, Err]),
            Self ! {edu_txn_res, undefined, Err},
            Data1#data{edu_txn = {undefined, TxnID, EDUs, Tries}}
    end.

send_with(error, _Host, _Method, MatrixServer, _Path, _Query, _JSON, _HTTPOptions, _Options) ->
    {error, {no_destination, MatrixServer}};
send_with(Dest, Host, Method, MatrixServer, Path, Query, JSON, HTTPOptions, Options) ->
    mod_matrix_gw:send_request(Host, Method, MatrixServer, Dest, Path, Query, JSON,
                               HTTPOptions, Options).

local_destination(Data) ->
    Now = erlang:system_time(millisecond),
    case Data#data.destination of
        {Dest, Until} when Until > Now ->
            {Dest, Data};
        _ ->
            Dest = resolve_destination(Data#data.host, Data#data.matrix_server),
            TTL = case Dest of error -> ?DESTINATION_ERROR_TTL; _ -> ?DESTINATION_TTL end,
            {Dest, Data#data{destination = {Dest, Now + TTL}}}
    end.

do_get_matrix_host_port(MatrixServer) ->
    case binary:split(MatrixServer, <<":">>) of
        [Addr] ->
            case inet:parse_address(binary_to_list(Addr)) of
                {ok, _} ->
                    {Addr, 8448};
                _ ->
                    URL = <<"https://", Addr/binary, "/.well-known/matrix/server">>,
                    HTTPRes =
                        httpc:request(get, {URL, []},
                                      [{timeout, 5000}],
                                      [{sync, true},
                                       {body_format, binary}]),
                    ?DEBUG("HTTPRes ~p~n", [HTTPRes]),
                    Res =
                        case HTTPRes of
                            {ok, {{_, 200, _}, _Headers, Body}} ->
                                try
                                    case misc:json_decode(Body) of
                                        #{<<"m.server">> := Server} ->
                                            case binary:split(Server, <<":">>) of
                                                [ServerAddr] ->
                                                    {ServerAddr, 8448};
                                                [ServerAddr, ServerPort] ->
                                                    {ServerAddr, binary_to_integer(ServerPort)}
                                            end
                                    end
                                catch
                                    _:_ ->
                                        error
                                end;
                            _ ->
                                error
                        end,
                    case Res of
                        error ->
                            SRVName =
                                "_matrix._tcp." ++ binary_to_list(MatrixServer),
                            case inet_res:getbyname(SRVName, srv, 5000) of
                                {ok, HostEntry} ->
                                    {hostent, _Name, _Aliases, _AddrType, _Len,
                                     HAddrList} = HostEntry,
                                    case h_addr_list_to_host_ports(HAddrList) of
                                        {ok, [{Host, Port} | _]} ->
                                            {list_to_binary(Host), Port};
                                        _ ->
                                            {MatrixServer, 8448}
                                    end;
                                {error, _} ->
                                    {MatrixServer, 8448}
                            end;
                        _ ->
                            Res
                    end
            end;
        [Addr, SPort] ->
            case catch binary_to_integer(SPort) of
                Port when is_integer(Port) ->
                    {Addr, Port};
                _ ->
                    error
            end
    end.

%% Copied from xmpp_stream_out.erl
-type host_port() :: {inet:hostname(), inet:port_number()}.
-type h_addr_list() :: [{integer(), integer(), inet:port_number(), string()}].
-spec h_addr_list_to_host_ports(h_addr_list()) -> {ok, [host_port(),...]} |
						  {error, nxdomain}.
h_addr_list_to_host_ports(AddrList) ->
    PrioHostPorts = lists:flatmap(
		      fun({Priority, Weight, Port, Host}) ->
			      N = case Weight of
				      0 -> 0;
				      _ -> (Weight + 1) * p1_rand:uniform()
				  end,
			      [{Priority * 65536 - N, Host, Port}];
			 (_) ->
			      []
		      end, AddrList),
    HostPorts = [{Host, Port}
		 || {_Priority, Host, Port} <- lists:usort(PrioHostPorts)],
    case HostPorts of
	[] -> {error, nxdomain};
	_ -> {ok, HostPorts}
    end.


%% The Host header a request carries when the server is reached
%% at another name than its own (an SRV target).
host_header(#{host := Name, name := Name}) -> [];
host_header(#{name := Name}) -> [{"Host", binary_to_list(Name)}].

check_signature(JSON, SignatureName, KeyID, VerifyKey) ->
    try
        #{<<"signatures">> := Signatures} = JSON,
        #{SignatureName := SignatureData} = Signatures,
        #{KeyID := SSignature} = SignatureData,
        Signature = mod_matrix_gw:base64_decode(SSignature),
        JSON2 = maps:without([<<"signatures">>, <<"unsigned">>], JSON),
        Msg = mod_matrix_gw:encode_canonical_json(JSON2),
        crypto:verify(eddsa, none, Msg, Signature, [VerifyKey, ed25519])
    catch
        _:_ ->
            false
    end.

request_keys(Via, Data) ->
    %% found by the spec's discovery, over verified TLS
    Dest =
        case resolve_destination(Data#data.host, Via) of
            error -> #{host => Via, port => 8448, name => Via};
            D -> D
        end,
    #{host := MHost, port := MPort} = Dest,
    URL =
        case Data#data.matrix_server of
            Via ->
                <<"https://", MHost/binary,
                  ":", (integer_to_binary(MPort))/binary,
                  "/_matrix/key/v2/server">>;
            MatrixServer ->
                <<"https://", MHost/binary,
                  ":", (integer_to_binary(MPort))/binary,
                  "/_matrix/key/v2/query/", MatrixServer/binary>>
        end,
    Self = self(),
    {ok, RequestID} =
        httpc:request(get, {URL, host_header(Dest)},
                      mod_matrix_gw_http:http_options(
                        [{timeout, 5000}, {connect_timeout, 5000}], Dest,
                        cacerts(Data#data.host)),
                      [{sync, false},
                       {receiver,
                        fun({RequestId, Result}) ->
                                gen_statem:cast(
                                  Self, {key_reply, RequestId, Result})
                        end}]),
    case Data#data.state of
        #pending{request_id = OldReqID} = St ->
            case OldReqID of
                undefined ->
                    ok;
                _ ->
                    httpc:cancel_request(OldReqID)
            end,
            Data#data{state = St#pending{request_id = RequestID}};
        #wait{timer_ref = TimerRef} ->
            erlang:cancel_timer(TimerRef),
            NotaryServers = mod_matrix_gw_opt:notary_servers(Data#data.host),
            Data#data{state = #pending{request_id = RequestID,
                                       servers = NotaryServers}}
    end.
