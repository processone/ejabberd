%%%-------------------------------------------------------------------
%%% @author Pawel Chmielowski <pawel@process-one.net>
%%% @copyright (C) 2002-2023 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(mod_mqtt_bridge_session).
-behaviour(p1_server).
-define(VSN, 1).
-vsn(?VSN).

%% API
-export([start/9, start_link/9]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").
-include("mqtt.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include_lib("public_key/include/public_key.hrl").

-type error_reason() ::
	{auth, reason_code()} |
	{code, reason_code()} |
	{peer_disconnected, reason_code(), binary()} |
	{socket, socket_error_reason()} |
	{codec, mqtt_codec:error_reason()} |
	{unexpected_packet, atom()} |
	{tls, inet:posix() | atom() | binary()} |
	{replaced, pid()} |
	{resumed, pid()} |
	subscribe_forbidden | publish_forbidden |
	will_topic_forbidden | internal_server_error |
	session_expired | idle_connection |
	queue_full | shutdown | db_failure |
	{payload_format_invalid, will | publish} |
	session_expiry_non_zero | unknown_topic_alias.

-type socket() ::
	{gen_tcp, inet:socket()} |
	{fast_tls, fast_tls:tls_socket()} |
	{mod_mqtt_ws, mod_mqtt_ws:socket()}.
-type seconds() :: non_neg_integer().
-type socket_error_reason() :: closed | timeout | inet:posix().

-define(PING_TIMEOUT, timer:seconds(50)).
-define(MAX_UINT32, 4294967295).

-record(state, {vsn = ?VSN :: integer(),
		version :: undefined | mqtt_version(),
		socket :: undefined | socket(),
		usr :: undefined | {binary(), binary(), binary()},
		ping_timer = undefined :: undefined | reference(),
		stop_reason :: undefined | error_reason(),
		subscriptions = #{},
		publish = #{},
		ws_codec = none,
		id = 0 :: non_neg_integer(),
		codec :: mqtt_codec:state(),
		authentication :: #{username => binary(), password => binary(), certfile => binary()}}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
start(Proc, Transport, Host, Port, Path, Publish, Subscribe, Authentication, ReplicationUser) ->
    p1_server:start({local, Proc}, ?MODULE, [Proc, Transport, Host, Port, Path, Publish, Subscribe, Authentication,
					     ReplicationUser], []).

start_link(Proc, Transport, Host, Port, Path, Publish, Subscribe, Authentication, ReplicationUser) ->
    p1_server:start_link({local, Proc}, ?MODULE, [Proc, Transport, Host, Port, Path, Publish, Subscribe,
						  Authentication, ReplicationUser], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([_Proc, Proto, Host, Port, Path, Publish, Subscribe, Authentication, ReplicationUser]) ->
    {Version, Transport} = case Proto of
			       mqtt -> {4, gen_tcp};
			       mqtts -> {4, ssl};
			       mqtt5 -> {5, gen_tcp};
			       mqtt5s -> {5, ssl};
			       ws -> {4, gen_tcp};
			       wss -> {4, ssl}
			   end,
    State = #state{version = Version,
		   id = p1_rand:uniform(65535),
		   codec = mqtt_codec:new(4096),
		   subscriptions = Subscribe,
		   authentication = Authentication,
		   usr = jid:tolower(ReplicationUser),
		   publish = Publish},
    case Authentication of
	#{certfile := Cert} when Proto == mqtts; Proto == mqtt5s; Proto == wss ->
	    Sock = ssl:connect(Host, Port, [binary, {active, true}, {certfile, Cert}]),
	    if Proto == ws orelse Proto == wss ->
		connect_ws(Host, Port, Path, Sock, State, ssl, none);
		true -> connect(Sock, State, ssl, none)
	    end;
	#{username := User, password := Pass} ->
	    Sock = Transport:connect(Host, Port, [binary, {active, true}]),
	    if Proto == ws orelse Proto == wss ->
		connect_ws(Host, Port, Path, Sock, State, Transport, {User, Pass});
		true -> connect(Sock, State, Transport, {User, Pass})
	    end;
	_ ->
	    {stop, {error, <<"Certificate can be only used for encrypted connections">>	}}
    end.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) ->
    {noreply, state()} | {noreply, state(), timeout()} | {stop, term(), state()}.
handle_info({Tag, TCPSock, TCPData},
	    #state{ws_codec = {init, Hash, Auth, Last}} = State)
    when (Tag == tcp orelse Tag == ssl) ->
    Data = <<Last/binary, TCPData/binary>>,
    case erlang:decode_packet(http_bin, Data, []) of
	{ok, {http_response, _, 101, _}, Rest} ->
	    handle_info({tcp, TCPSock, Rest}, State#state{ws_codec = {inith, Hash, none, Auth, <<>>}});
	{ok, {http_response, _, _, _}, _Rest} ->
	    stop(State, {socket, closed});
	{ok, {http_error, _}, _} ->
	    stop(State, {socket, closed});
	{error, _} ->
	    stop(State, {socket, closed});
	{more, _} ->
	    {noreply, State#state{ws_codec = {init, Hash, Auth, Data}}}
    end;
handle_info({Tag, TCPSock, TCPData},
	    #state{ws_codec = {inith, Hash, Upgrade, Auth, Last},
		   socket = {Transport, _}} = State)
    when (Tag == tcp orelse Tag == ssl) ->
    Data = <<Last/binary, TCPData/binary>>,
    case erlang:decode_packet(httph_bin, Data, []) of
	{ok, {http_header, _, <<"Sec-Websocket-Accept">>, _, Val}, Rest} ->
	    case str:to_lower(Val) of
		Hash ->
		    handle_info({tcp, TCPSock, Rest},
				State#state{ws_codec = {inith, ok, Upgrade, Auth, <<>>}});
		_ ->
		    stop(State, {socket, closed})
	    end;
	{ok, {http_header, _, 'Connection', _, Val}, Rest} ->
	    case str:to_lower(Val) of
		<<"upgrade">> ->
		    handle_info({tcp, TCPSock, Rest},
				State#state{ws_codec = {inith, Hash, ok, Auth, <<>>}});
		_ ->
		    stop(State, {socket, closed})
	    end;
	{ok, {http_header, _, _, _, _}, Rest} ->
	    handle_info({tcp, TCPSock, Rest}, State);
	{ok, {http_error, _}, _} ->
	    stop(State, {socket, closed});
	{ok, http_eoh, Rest} ->
	    case {Hash, Upgrade} of
		{ok, ok} ->
		    {ok, State2} = connect({ok, TCPSock},
					   State#state{ws_codec = ejabberd_websocket_codec:new_client()},
					   Transport, Auth),
		    handle_info({tcp, TCPSock, Rest}, State2);
		_ ->
		    stop(State, {socket, closed})
	    end;
	{error, _} ->
	    stop(State, {socket, closed});
	{more, _} ->
	    {noreply, State#state{ws_codec = {inith, Hash, Upgrade, Data}}}
    end;
handle_info({Tag, TCPSock, TCPData},
	    #state{ws_codec = WSCodec} = State)
    when (Tag == tcp orelse Tag == ssl) andalso WSCodec /= none ->
    {Packets, Acc0} =
    case ejabberd_websocket_codec:decode(WSCodec, TCPData) of
	{ok, NewWSCodec, Packets0} ->
	    {Packets0, {noreply, ok, State#state{ws_codec = NewWSCodec}}};
	{error, _Error, Packets0} ->
	    {Packets0, {stop_after, {socket, closed}, State}}
    end,
    Res2 =
    lists:foldl(
	fun(_, {stop, _, _} = Res) -> Res;
	   ({_Op, Data}, {Tag, Res, S}) ->
	       case handle_info({tcp_decoded, TCPSock, Data}, S) of
		   {stop, _, _} = Stop ->
		       Stop;
		   {_, NewState} ->
		       {Tag, Res, NewState}
	       end
	end, Acc0, Packets),
    case Res2 of
	{noreply, _, State} ->
	    {noreply, State};
	_ ->
	    Res2
    end;
handle_info({Tag, TCPSock, TCPData},
	    #state{codec = Codec} = State)
    when Tag == tcp; Tag == ssl; Tag == tcp_decoded ->
    case mqtt_codec:decode(Codec, TCPData) of
	{ok, Pkt, Codec1} ->
	    ?DEBUG("Got MQTT packet:~n~ts", [pp(Pkt)]),
	    State1 = State#state{codec = Codec1},
	    case handle_packet(Pkt, State1) of
		{ok, State2} ->
		    handle_info({tcp_decoded, TCPSock, <<>>}, State2);
		{error, State2, Reason} ->
		    stop(State2, Reason)
	    end;
	{more, Codec1} ->
	    State1 = State#state{codec = Codec1},
	    {noreply, State1};
	{error, Why} ->
	    stop(State, {codec, Why})
    end;
handle_info({tcp_closed, _Sock}, State) ->
    ?DEBUG("MQTT connection reset by peer", []),
    stop(State, {socket, closed});
handle_info({ssl_closed, _Sock}, State) ->
    ?DEBUG("MQTT connection reset by peer", []),
    stop(State, {socket, closed});
handle_info({tcp_error, _Sock, Reason}, State) ->
    ?DEBUG("MQTT connection error: ~ts", [format_inet_error(Reason)]),
    stop(State, {socket, Reason});
handle_info({ssl_error, _Sock, Reason}, State) ->
    ?DEBUG("MQTT connection error: ~ts", [format_inet_error(Reason)]),
    stop(State, {socket, Reason});
handle_info({publish, #publish{topic = Topic} = Pkt}, #state{publish = Publish} = State) ->
    case maps:find(Topic, Publish) of
	{ok, RemoteTopic} ->
	    case send(State, Pkt#publish{qos = 0, topic = RemoteTopic}) of
		{ok, State2} ->
		    {noreply, State2}
	    end;
	_ ->
	    {noreply, State}
    end;
handle_info({timeout, _TRef, ping_timeout}, State) ->
    case send(State, #pingreq{}) of
	{ok, State2} ->
	    {noreply, State2}
    end;
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec handle_packet(mqtt_packet(), state()) ->
    {ok, state()} |
    {error, state(), error_reason()}.
handle_packet(#connack{} = Pkt, State) ->
    handle_connack(Pkt, State);
handle_packet(#suback{}, State) ->
    {ok, State};
handle_packet(#publish{} = Pkt, State) ->
    handle_publish(Pkt, State);
handle_packet(#pingresp{}, State) ->
    {ok, State};
handle_packet(#disconnect{properties = #{session_expiry_interval := SE}},
	      State) when SE > 0 ->
    %% Protocol violation
    {error, State, session_expiry_non_zero};
handle_packet(#disconnect{code = Code, properties = Props},
	      State) ->
    Reason = maps:get(reason_string, Props, <<>>),
    {error, State, {peer_disconnected, Code, Reason}};
handle_packet(Pkt, State) ->
    ?WARNING_MSG("Unexpected packet:~n~ts~n** when state:~n~ts",
		 [pp(Pkt), pp(State)]),
    {error, State, {unexpected_packet, element(1, Pkt)}}.

terminate(Reason, State) ->
    Reason1 = case Reason of
		  shutdown -> shutdown;
		  {shutdown, _} -> shutdown;
		  normal -> State#state.stop_reason;
		  {process_limit, _} -> queue_full;
		  _ -> internal_server_error
	      end,
    disconnect(State, Reason1).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% State transitions
%%%===================================================================
connect({error, Reason}, _State, _Transport, _Auth) ->
    {stop, {error, Reason}};
connect({ok, Sock}, State0, Transport, Auth) ->
    State = State0#state{socket = {Transport, Sock}},
    Connect = case Auth of
		  {User, Pass} ->
		      #connect{client_id = integer_to_binary(State#state.id),
			       clean_start = true,
			       username = User,
			       password = Pass,
			       keep_alive = 60,
			       proto_level = State#state.version};
		  _ ->
		      #connect{client_id = integer_to_binary(State#state.id),
			       clean_start = true,
			       keep_alive = 60,
			       proto_level = State#state.version}
	      end,
    Pkt = mqtt_codec:encode(State#state.version, Connect),
    send(State, Connect),
    {ok, _, Codec2} = mqtt_codec:decode(State#state.codec, Pkt),
    {ok, State#state{codec = Codec2}}.

connect_ws(_Host, _Port, _Path, {error, Reason}, _State, _Transport, _Auth) ->
    {stop, {error, Reason}};
connect_ws(Host, Port, Path, {ok, Sock}, State0, Transport, Auth) ->
    Key = base64:encode(p1_rand:get_string()),
    Hash = str:to_lower(base64:encode(crypto:hash(sha, <<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>))),
    Data = <<"GET ", (list_to_binary(Path))/binary, " HTTP/1.1\r\n",
	     "Host: ", (list_to_binary(Host))/binary, ":", (integer_to_binary(Port))/binary,"\r\n",
	     "Upgrade: websocket\r\n",
	     "Connection: Upgrade\r\n",
	     "Sec-WebSocket-Protocol: mqtt\r\n",
	     "Sec-WebSocket-Key: ", Key/binary, "\r\n",
	     "Sec-WebSocket-Version: 13\r\n\r\n">>,
    Res = Transport:send(Sock, Data),
    check_sock_result({Transport, Sock}, Res),
    {ok, State0#state{ws_codec = {init, Hash, Auth, <<>>}, socket = {Transport, Sock}}}.

-spec stop(state(), error_reason()) ->
    {noreply, state(), infinity} |
    {stop, normal, state()}.
stop(State, Reason) ->
    {stop, normal, State#state{stop_reason = Reason}}.


%%%===================================================================
%%% CONNECT/PUBLISH/SUBSCRIBE/UNSUBSCRIBE handlers
%%%===================================================================
-spec handle_connack(connack(), state()) ->
    {ok, state()} |
    {error, state(), error_reason()}.
handle_connack(#connack{code = success}, #state{subscriptions = Subs} = State) ->
    Filters = maps:fold(
	fun(RemoteTopic, _LocalTopic, Acc) ->
	    [{RemoteTopic, #sub_opts{}} | Acc]
	end, [], Subs),
    Pkt = #subscribe{id = 1, filters = Filters},
    send(State, Pkt);
handle_connack(#connack{}, State) ->
    {error, State, {auth, 'not-authorized'}}.

-spec handle_publish(publish(), state()) ->
    {ok, state()} |
    {error, state(), error_reason()}.
handle_publish(#publish{topic = Topic, payload = Payload, properties = Props},
	       #state{usr = USR, subscriptions = Subs} = State) ->
    case maps:get(Topic, Subs, none) of
	none ->
	    {ok, State};
	LocalTopic ->
	    MessageExpiry = maps:get(message_expiry_interval, Props, ?MAX_UINT32),
	    ExpiryTime = min(unix_time() + MessageExpiry, ?MAX_UINT32),
	    mod_mqtt:publish(USR, #publish{retain = true, topic = LocalTopic, payload = Payload, properties = Props},
			     ExpiryTime),
	    {ok, State}
    end.

%%%===================================================================
%%% Socket management
%%%===================================================================
-spec send(state(), mqtt_packet()) ->
    {ok, state()} |
    {error, state(), error_reason()}.
send(State, #publish{} = Pkt) ->
    case is_expired(Pkt) of
	{false, Pkt1} ->
	    {ok, do_send(State, Pkt1)};
	true ->
	    {ok, State}
    end;
send(State, Pkt) ->
    {ok, do_send(State, Pkt)}.

-spec do_send(state(), mqtt_packet()) -> state().
do_send(#state{ws_codec = WSCodec, socket = {SockMod, Sock} = Socket} = State, Pkt)
    when WSCodec /= none ->
    ?DEBUG("Send MQTT packet:~n~ts", [pp(Pkt)]),
    Data = mqtt_codec:encode(State#state.version, Pkt),
    WSData = ejabberd_websocket_codec:encode(WSCodec, 2, Data),
    Res = SockMod:send(Sock, WSData),
    check_sock_result(Socket, Res),
    reset_ping_timer(State);
do_send(#state{socket = {SockMod, Sock} = Socket} = State, Pkt) ->
    ?DEBUG("Send MQTT packet:~n~ts", [pp(Pkt)]),
    Data = mqtt_codec:encode(State#state.version, Pkt),
    Res = SockMod:send(Sock, Data),
    check_sock_result(Socket, Res),
    reset_ping_timer(State);
do_send(State, _Pkt) ->
    State.

-spec disconnect(state(), error_reason()) -> state().
disconnect(#state{socket = {SockMod, Sock}} = State, Err) ->
    State1 = case Err of
		 {auth, Code} ->
		     do_send(State, #connack{code = Code});
		 {codec, {Tag, _, _} = CErr} when Tag == unsupported_protocol_version;
					   Tag == unsupported_protocol_name ->
		     do_send(State#state{version = ?MQTT_VERSION_4},
			     #connack{code = mqtt_codec:error_reason_code(CErr)});
		 _ when State#state.version == undefined ->
		     State;
		 {Tag, _} when Tag == socket; Tag == tls ->
		     State;
		 {peer_disconnected, _, _} ->
		     State;
		 _ ->
		     case State of
			 _ when State#state.version == ?MQTT_VERSION_5 ->
			     Code = disconnect_reason_code(Err),
			     Pkt = #disconnect{code = Code},
			     do_send(State, Pkt);
			 _ ->
			     State
		     end
	     end,
    SockMod:close(Sock),
    State1#state{socket = undefined,
		 version = undefined,
		 codec = mqtt_codec:renew(State#state.codec)};
disconnect(State, _) ->
    State.

-spec reset_ping_timer(state()) -> state().
reset_ping_timer(State) ->
    misc:cancel_timer(State#state.ping_timer),
    State#state{ping_timer = erlang:start_timer(?PING_TIMEOUT, self(), ping_timeout)}.

-spec check_sock_result(socket(), ok | {error, inet:posix()}) -> ok.
check_sock_result(_, ok) ->
    ok;
check_sock_result({_, Sock}, {error, Why}) ->
    self() ! {tcp_closed, Sock},
    ?DEBUG("MQTT socket error: ~p", [format_inet_error(Why)]).

%%%===================================================================
%%% Formatters
%%%===================================================================
-spec pp(any()) -> iolist().
pp(Term) ->
    io_lib_pretty:print(Term, fun pp/2).

-spec format_inet_error(socket_error_reason()) -> string().
format_inet_error(closed) ->
    "connection closed";
format_inet_error(timeout) ->
    format_inet_error(etimedout);
format_inet_error(Reason) ->
    case inet:format_error(Reason) of
	"unknown POSIX error" -> atom_to_list(Reason);
	Txt -> Txt
    end.

-spec pp(atom(), non_neg_integer()) -> [atom()] | no.
pp(state, 17) -> record_info(fields, state);
pp(Rec, Size) -> mqtt_codec:pp(Rec, Size).

-spec disconnect_reason_code(error_reason()) -> reason_code().
disconnect_reason_code({code, Code}) -> Code;
disconnect_reason_code({codec, Err}) -> mqtt_codec:error_reason_code(Err);
disconnect_reason_code({unexpected_packet, _}) -> 'protocol-error';
disconnect_reason_code({replaced, _}) -> 'session-taken-over';
disconnect_reason_code({resumed, _}) -> 'session-taken-over';
disconnect_reason_code(internal_server_error) -> 'implementation-specific-error';
disconnect_reason_code(db_failure) -> 'implementation-specific-error';
disconnect_reason_code(idle_connection) -> 'keep-alive-timeout';
disconnect_reason_code(queue_full) -> 'quota-exceeded';
disconnect_reason_code(shutdown) -> 'server-shutting-down';
disconnect_reason_code(subscribe_forbidden) -> 'topic-filter-invalid';
disconnect_reason_code(publish_forbidden) -> 'topic-name-invalid';
disconnect_reason_code(will_topic_forbidden) -> 'topic-name-invalid';
disconnect_reason_code({payload_format_invalid, _}) -> 'payload-format-invalid';
disconnect_reason_code(session_expiry_non_zero) -> 'protocol-error';
disconnect_reason_code(unknown_topic_alias) -> 'protocol-error';
disconnect_reason_code(_) -> 'unspecified-error'.

%%%===================================================================
%%% Timings
%%%===================================================================
-spec unix_time() -> seconds().
unix_time() ->
    erlang:system_time(second).

-spec is_expired(publish()) -> true | {false, publish()}.
is_expired(#publish{meta = Meta, properties = Props} = Pkt) ->
    case maps:get(expiry_time, Meta, ?MAX_UINT32) of
	?MAX_UINT32 ->
	    {false, Pkt};
	ExpiryTime ->
	    Left = ExpiryTime - unix_time(),
	    if Left > 0 ->
		Props1 = Props#{message_expiry_interval => Left},
		{false, Pkt#publish{properties = Props1}};
		true ->
		    ?DEBUG("Dropping expired packet:~n~ts", [pp(Pkt)]),
		    true
	    end
    end.
