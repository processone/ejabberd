%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Matrix S2S
%%% Created :  1 May 2022 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-ifndef(OTP_BELOW_25).
-behaviour(gen_statem).

%% API
-export([start_link/2, supervisor/1, create_db/0,
         get_connection/2, check_auth/5, check_signature/2,
         get_matrix_host_port/2]).

%% gen_statem callbacks
-export([init/1, terminate/3, code_change/4, callback_mode/0]).
-export([handle_event/4]).

-include("logger.hrl").
-include("ejabberd_http.hrl").
-include_lib("kernel/include/inet.hrl").

-record(matrix_s2s,
        {to  :: binary(),
         pid :: pid()}).

-record(data,
        {host :: binary(),
         matrix_server :: binary(),
         matrix_host_port :: {binary(), integer()} | undefined,
         keys = #{},
         key_queue = #{}}).

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
	    case supervisor:start_child(supervisor(Host),
					[Host, MatrixServer]) of
		{ok, undefined} -> {error, ignored};
		Res -> Res
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
    case mod_matrix_gw_opt:matrix_domain(Host) of
        MatrixServer ->
            error;
        _ ->
            case get_connection(Host, MatrixServer) of
                {ok, S2SPid} ->
                    gen_statem:call(S2SPid, get_matrix_host_port);
                Error -> Error
            end
    end.


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
    case get_connection(Host, MatrixServer) of
        {ok, S2SPid} ->
            #{<<"key">> := KeyID} = AuthParams,
            case catch gen_statem:call(S2SPid, {get_key, KeyID}) of
                {ok, VerifyKey, _ValidUntil} ->
                    %% TODO: check ValidUntil
                    Destination = mod_matrix_gw_opt:matrix_domain(Host),
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

check_signature(Host, JSON) ->
    case JSON of
        #{<<"sender">> := Sender,
          <<"signatures">> := Sigs} ->
            MatrixServer = mod_matrix_gw:get_id_domain_exn(Sender),
            case Sigs of
                #{MatrixServer := #{} = KeySig} ->
                    case maps:next(maps:iterator(KeySig)) of
                        {KeyID, _Sig, _} ->
                            case catch get_key(Host, MatrixServer, KeyID) of
                                {ok, VerifyKey, _ValidUntil} ->
                                    %% TODO: check ValidUntil
                                    case check_signature(JSON, MatrixServer, KeyID, VerifyKey) of
                                        true ->
                                            true;
                                        false ->
                                            ?WARNING_MSG("Failed authentication: ~p", [JSON]),
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
     #data{host = Host,
           matrix_server = MatrixServer}}.

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
handle_event({call, From}, get_matrix_host_port, _State, Data) ->
    case Data#data.matrix_host_port of
        undefined ->
            Result = do_get_matrix_host_port(Data),
            Data2 = Data#data{matrix_host_port = Result},
            {keep_state, Data2, [{reply, From, Result}]};
        Result ->
            {keep_state_and_data, [{reply, From, Result}]}
    end;
handle_event({call, From}, {get_key, KeyID}, State, Data) ->
    case maps:find(KeyID, Data#data.keys) of
        {ok, {ok, _, _} = Result} ->
            {keep_state, Data, [{reply, From, Result}]};
        {ok, error = Result} ->
            {keep_state, Data, [{reply, From, Result}]};
        {ok, pending} ->
            KeyQueue = maps:update_with(
                         KeyID,
                         fun(Xs) ->
                                 [From | Xs]
                         end,
                         [From],
                         Data#data.key_queue),
            {next_state, State,
             Data#data{key_queue = KeyQueue}, []};
        error ->
            {MHost, MPort} = do_get_matrix_host_port(Data),
            URL = <<"https://", MHost/binary,
                    ":", (integer_to_binary(MPort))/binary,
                    "/_matrix/key/v2/server/", KeyID/binary>>,
            Self = self(),
            httpc:request(get, {URL, []},
                          [{timeout, 5000}],
                          [{sync, false},
                           {receiver,
                            fun({_RequestId, Result}) ->
                                    gen_statem:cast(
                                      Self, {key_reply, KeyID, Result})
                            end}]),
            Keys = (Data#data.keys)#{KeyID => pending},
            KeyQueue = maps:update_with(
                         KeyID,
                         fun(Xs) ->
                                 [From | Xs]
                         end,
                         [From],
                         Data#data.key_queue),
            {next_state, State,
             Data#data{keys = Keys,
                       key_queue = KeyQueue},
             []}
    end;
handle_event(cast, {query, AuthParams, _Query, _JSON, _Request} = Msg,
             State, Data) ->
    #{<<"key">> := KeyID} = AuthParams,
    case maps:find(KeyID, Data#data.keys) of
        {ok, {ok, VerifyKey, _ValidUntil}} ->
            Data2 = process_unverified_query(
                      KeyID, VerifyKey, Msg, Data),
            {next_state, State, Data2, []};
        {ok, error} ->
            %TODO
            {next_state, State, Data, []};
        {ok, pending} ->
            KeyQueue = maps:update_with(
                         KeyID,
                         fun(Xs) ->
                                 [Msg | Xs]
                         end,
                         [Msg],
                         Data#data.key_queue),
            {next_state, State,
             Data#data{key_queue = KeyQueue}, []};
        error ->
            {MHost, MPort} = do_get_matrix_host_port(Data),
            URL = <<"https://", MHost/binary,
                    ":", (integer_to_binary(MPort))/binary,
                    "/_matrix/key/v2/server/", KeyID/binary>>,
            Self = self(),
            httpc:request(get, {URL, []},
                          [{timeout, 5000}],
                          [{sync, false},
                           {receiver,
                            fun({_RequestId, Result}) ->
                                    gen_statem:cast(
                                      Self, {key_reply, KeyID, Result})
                            end}]),
            Keys = (Data#data.keys)#{KeyID => pending},
            KeyQueue = maps:update_with(
                         KeyID,
                         fun(Xs) ->
                                 [Msg | Xs]
                         end,
                         [Msg],
                         Data#data.key_queue),
            {next_state, State,
             Data#data{keys = Keys,
                       key_queue = KeyQueue},
             []}
    end;
handle_event(cast, {key_reply, KeyID, HTTPResult}, State, Data) ->
    case HTTPResult of
        {{_, 200, _}, _, SJSON} ->
            try
                JSON = misc:json_decode(SJSON),
                ?DEBUG("key ~p~n", [JSON]),
                #{<<"verify_keys">> := VerifyKeys} = JSON,
                #{KeyID := KeyData} = VerifyKeys,
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
                Keys = (Data#data.keys)#{KeyID => {ok, VerifyKey, ValidUntil2}},
                Froms = maps:get(KeyID, Data#data.key_queue, []),
                KeyQueue = maps:remove(KeyID, Data#data.key_queue),
                Data2 = Data#data{keys = Keys,
                                  key_queue = KeyQueue},
                Replies =
                    lists:map(
                      fun(From) ->
                              {reply, From, {ok, VerifyKey, ValidUntil2}}
                      end, Froms),
                ?DEBUG("KEYS ~p~n", [{Keys, Data2}]),
                {next_state, State, Data2, Replies}
            catch
                _:_ ->
                    %% TODO
                    Keys2 = (Data#data.keys)#{KeyID => error},
                    {next_state, State, Data#data{keys = Keys2}, []}
            end;
        _ ->
            %% TODO
            Keys = (Data#data.keys)#{KeyID => error},
            {next_state, State, Data#data{keys = Keys}, []}
    end;
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

do_get_matrix_host_port(Data) ->
    MatrixServer = Data#data.matrix_server,
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

%process_unverified_queries(KeyID, Data) ->
%    case maps:find(KeyID, Data#data.keys) of
%        {ok, {ok, VerifyKey, _ValidUntil}} ->
%            Queue = maps:get(KeyID, Data#data.key_queue, []),
%            KeyQueue = maps:remove(KeyID, Data#data.key_queue),
%            Data2 = Data#data{key_queue = KeyQueue},
%            lists:foldl(
%              fun(Query, DataAcc) ->
%                      process_unverified_query(KeyID, VerifyKey, Query, DataAcc)
%              end, Data2, Queue);
%        _ ->
%            %% TODO
%            Data
%    end.

process_unverified_query(
  KeyID, VerifyKey, {query, AuthParams, _Query, Content, Request} = _Msg, Data) ->
    Destination = mod_matrix_gw_opt:matrix_domain(Data#data.host),
    #{<<"sig">> := Sig} = AuthParams,
    JSON = #{<<"method">> => atom_to_binary(Request#request.method, latin1),
             <<"uri">> => Request#request.raw_path,
             <<"origin">> => Data#data.matrix_server,
             <<"destination">> => Destination,
             <<"signatures">> => #{
               Data#data.matrix_server => #{KeyID => Sig}
              }
            },
    JSON2 =
        case Content of
            none -> JSON;
            _ ->
                JSON#{<<"content">> => Content}
        end,
    case check_signature(JSON2, Data#data.matrix_server, KeyID, VerifyKey) of
        true ->
            todo_remove_me;
            %process_query(Msg, Data);
        false ->
            ?WARNING_MSG("Failed authentication: ~p", [JSON]),
            %% TODO
            Data
    end.

-endif.
