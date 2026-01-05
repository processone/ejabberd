%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2026 ProcessOne, SARL. All Rights Reserved.
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
-module(mqtt_codec).

%% API
-export([new/1, new/2, renew/1, decode/2, encode/2]).
-export([pp/1, pp/2, format_error/1, format_reason_code/1]).
-export([error_reason_code/1, is_error_code/1]).
%% Validators
-export([topic/1, topic_filter/1, qos/1, utf8/1]).
-export([decode_varint/1]).

-include("mqtt.hrl").

-define(MAX_UINT16, 65535).
-define(MAX_UINT32, 4294967295).
-define(MAX_VARINT, 268435456).

-record(codec_state, {version    :: undefined | mqtt_version(),
                      type       :: undefined | non_neg_integer(),
		      flags      :: undefined | non_neg_integer(),
		      size       :: undefined | non_neg_integer(),
		      max_size   :: pos_integer() | infinity,
		      buf = <<>> :: binary()}).

-type error_reason() :: bad_varint |
			{payload_too_big, integer()} |
			{bad_packet_type, char()} |
			{bad_packet, atom()} |
                        {unexpected_packet, atom()} |
			{bad_reason_code, atom(), char()} |
                        {bad_properties, atom()} |
                        {bad_property, atom(), atom()} |
                        {duplicated_property, atom(), atom()} |
			bad_will_topic_or_message |
			bad_connect_username_or_password |
			bad_publish_id_or_payload |
			{bad_topic_filters, atom()} |
			{bad_qos, char()} |
			bad_topic | bad_topic_filter | bad_utf8_string |
			{unsupported_protocol_name, binary(), binary()} |
                        {unsupported_protocol_version, char(), iodata()} |
			{{bad_flag, atom()}, char(), term()} |
			{{bad_flags, atom()}, char(), char()}.

-opaque state() :: #codec_state{}.
-export_type([state/0, error_reason/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec new(pos_integer() | infinity) -> state().
new(MaxSize) ->
    new(MaxSize, undefined).

-spec new(pos_integer() | infinity, undefined | mqtt_version()) -> state().
new(MaxSize, Version) ->
    #codec_state{max_size = MaxSize, version = Version}.

-spec renew(state()) -> state().
renew(#codec_state{version = Version, max_size = MaxSize}) ->
    #codec_state{version = Version, max_size = MaxSize}.

-spec decode(state(), binary()) -> {ok, mqtt_packet(), state()} |
				   {more, state()} |
				   {error, error_reason()}.
decode(#codec_state{size = undefined, buf = Buf} = State, Data) ->
    Buf1 = <<Buf/binary, Data/binary>>,
    case Buf1 of
	<<Type:4, Flags:4, Data1/binary>> ->
	    try
		case decode_varint(Data1) of
		    {Len, _} when Len >= State#codec_state.max_size ->
			err({payload_too_big, State#codec_state.max_size});
		    {Len, Data2} when size(Data2) >= Len ->
			<<Payload:Len/binary, Data3/binary>> = Data2,
                        Version = State#codec_state.version,
                        Pkt = decode_pkt(Version, Type, Flags, Payload),
			State1 = case Pkt of
                                     #connect{proto_level = V} ->
                                         State#codec_state{version = V};
                                     _ ->
                                         State
                                 end,
                        {ok, Pkt, State1#codec_state{buf = Data3}};
		    {Len, Data2} ->
			{more, State#codec_state{type = Type,
						 flags = Flags,
						 size = Len,
						 buf = Data2}};
		    more ->
			{more, State#codec_state{buf = Buf1}}
		end
	    catch _:{?MODULE, Why} ->
		    {error, Why}
	    end;
	<<>> ->
	    {more, State}
    end;
decode(#codec_state{size = Len, buf = Buf,
                    version = Version,
		    type = Type, flags = Flags} = State, Data) ->
    Buf1 = <<Buf/binary, Data/binary>>,
    if size(Buf1) >= Len ->
	    <<Payload:Len/binary, Data1/binary>> = Buf1,
	    try
		Pkt = decode_pkt(Version, Type, Flags, Payload),
                State1 = case Pkt of
                             #connect{proto_level = V} ->
                                 State#codec_state{version = V};
                             _ ->
                                 State
                         end,
		{ok, Pkt, State1#codec_state{type = undefined,
                                             flags = undefined,
                                             size = undefined,
                                             buf = Data1}}
	    catch _:{?MODULE, Why} ->
		    {error, Why}
	    end;
       true ->
	    {more, State#codec_state{buf = Buf1}}
    end.

-spec encode(mqtt_version(), mqtt_packet()) -> binary().
encode(Version, Pkt) ->
    case Pkt of
        #connect{proto_level = Version} -> encode_connect(Pkt);
        #connack{} -> encode_connack(Version, Pkt);
        #publish{} -> encode_publish(Version, Pkt);
        #puback{} -> encode_puback(Version, Pkt);
        #pubrec{} -> encode_pubrec(Version, Pkt);
        #pubrel{} -> encode_pubrel(Version, Pkt);
        #pubcomp{} -> encode_pubcomp(Version, Pkt);
        #subscribe{} -> encode_subscribe(Version, Pkt);
        #suback{} -> encode_suback(Version, Pkt);
        #unsubscribe{} -> encode_unsubscribe(Version, Pkt);
        #unsuback{} -> encode_unsuback(Version, Pkt);
        #pingreq{} -> encode_pingreq();
        #pingresp{} -> encode_pingresp();
        #disconnect{} -> encode_disconnect(Version, Pkt);
        #auth{} -> encode_auth(Pkt)
    end.

-spec pp(any()) -> iolist().
pp(Term) ->
    io_lib_pretty:print(Term, fun pp/2).

-spec format_error(error_reason()) -> string().
format_error({payload_too_big, Max}) ->
    format("Payload exceeds ~B bytes", [Max]);
format_error(bad_varint) ->
    "Variable Integer is out of boundaries";
format_error({bad_packet_type, Type}) ->
    format("Unexpected packet type: ~B", [Type]);
format_error({bad_packet, Name}) ->
    format("Malformed ~ts packet", [string:to_upper(atom_to_list(Name))]);
format_error({unexpected_packet, Name}) ->
    format("Unexpected ~ts packet", [string:to_upper(atom_to_list(Name))]);
format_error({bad_reason_code, Name, Code}) ->
    format("Unexpected reason code in ~ts code: ~B",
           [string:to_upper(atom_to_list(Name)), Code]);
format_error({bad_properties, Name}) ->
    format("Malformed properties of ~ts packet",
           [string:to_upper(atom_to_list(Name))]);
format_error({bad_property, Pkt, Prop}) ->
    format("Malformed property ~ts of ~ts packet",
           [Prop, string:to_upper(atom_to_list(Pkt))]);
format_error({duplicated_property, Pkt, Prop}) ->
    format("Property ~ts is included more than once into ~ts packet",
           [Prop, string:to_upper(atom_to_list(Pkt))]);
format_error(bad_will_topic_or_message) ->
    "Malformed Will Topic or Will Message";
format_error(bad_connect_username_or_password) ->
    "Malformed username or password of CONNECT packet";
format_error(bad_publish_id_or_payload) ->
    "Malformed id or payload of PUBLISH packet";
format_error({bad_topic_filters, Name}) ->
    format("Malformed topic filters of ~ts packet",
	   [string:to_upper(atom_to_list(Name))]);
format_error({bad_qos, Q}) ->
    format_got_expected("Malformed QoS value", Q, "0, 1 or 2");
format_error(bad_topic) ->
    "Malformed topic";
format_error(bad_topic_filter) ->
    "Malformed topic filter";
format_error(bad_utf8_string) ->
    "Malformed UTF-8 string";
format_error({unsupported_protocol_name, Got, Expected}) ->
    format_got_expected("Unsupported protocol name", Got, Expected);
format_error({unsupported_protocol_version, Got, Expected}) ->
    format_got_expected("Unsupported protocol version", Got, Expected);
format_error({{bad_flag, Name}, Got, Expected}) ->
    Txt = "Unexpected " ++ atom_to_list(Name) ++ " flag",
    format_got_expected(Txt, Got, Expected);
format_error({{bad_flags, Name}, Got, Expected}) ->
    Txt = "Unexpected " ++ string:to_upper(atom_to_list(Name)) ++ " flags",
    format_got_expected(Txt, Got, Expected);
format_error(Reason) ->
    format("Unexpected error: ~w", [Reason]).

-spec error_reason_code(error_reason()) -> reason_code().
error_reason_code({unsupported_protocol_name, _, _}) ->
    'unsupported-protocol-version';
error_reason_code({unsupported_protocol_version, _, _}) ->
    'unsupported-protocol-version';
error_reason_code({payload_too_big, _}) -> 'packet-too-large';
error_reason_code({unexpected_packet, _}) -> 'protocol-error';
error_reason_code(_) -> 'malformed-packet'.

-spec format_reason_code(reason_code()) -> string().
format_reason_code('success') -> "Success";
format_reason_code('normal-disconnection') -> "Normal disconnection";
format_reason_code('granted-qos-0') -> "Granted QoS 0";
format_reason_code('granted-qos-1') -> "Granted QoS 1";
format_reason_code('granted-qos-2') -> "Granted QoS 2";
format_reason_code('no-matching-subscribers') -> "No matching subscribers";
format_reason_code('no-subscription-existed') -> "No subscription existed";
format_reason_code('continue-authentication') -> "Continue authentication";
format_reason_code('re-authenticate') -> "Re-authenticate";
format_reason_code('unspecified-error') -> "Unspecified error";
format_reason_code('malformed-packet') -> "Malformed Packet";
format_reason_code('protocol-error') -> "Protocol Error";
format_reason_code('bad-user-name-or-password') -> "Bad User Name or Password";
format_reason_code('not-authorized') -> "Not authorized";
format_reason_code('server-unavailable') -> "Server unavailable";
format_reason_code('server-busy') -> "Server busy";
format_reason_code('banned') -> "Banned";
format_reason_code('server-shutting-down') -> "Server shutting down";
format_reason_code('bad-authentication-method') -> "Bad authentication method";
format_reason_code('keep-alive-timeout') -> "Keep Alive timeout";
format_reason_code('session-taken-over') -> "Session taken over";
format_reason_code('topic-filter-invalid') -> "Topic Filter invalid";
format_reason_code('topic-name-invalid') -> "Topic Name invalid";
format_reason_code('packet-identifier-in-use') -> "Packet Identifier in use";
format_reason_code('receive-maximum-exceeded') -> "Receive Maximum exceeded";
format_reason_code('topic-alias-invalid') -> "Topic Alias invalid";
format_reason_code('packet-too-large') -> "Packet too large";
format_reason_code('message-rate-too-high') -> "Message rate too high";
format_reason_code('quota-exceeded') -> "Quota exceeded";
format_reason_code('administrative-action') -> "Administrative action";
format_reason_code('payload-format-invalid') -> "Payload format invalid";
format_reason_code('retain-not-supported') -> "Retain not supported";
format_reason_code('qos-not-supported') -> "QoS not supported";
format_reason_code('use-another-server') -> "Use another server";
format_reason_code('server-moved') -> "Server moved";
format_reason_code('connection-rate-exceeded') -> "Connection rate exceeded";
format_reason_code('maximum-connect-time') -> "Maximum connect time";
format_reason_code('unsupported-protocol-version') ->
    "Unsupported Protocol Version";
format_reason_code('client-identifier-not-valid') ->
    "Client Identifier not valid";
format_reason_code('packet-identifier-not-found') ->
    "Packet Identifier not found";
format_reason_code('disconnect-with-will-message') ->
    "Disconnect with Will Message";
format_reason_code('implementation-specific-error') ->
    "Implementation specific error";
format_reason_code('shared-subscriptions-not-supported') ->
    "Shared Subscriptions not supported";
format_reason_code('subscription-identifiers-not-supported') ->
    "Subscription Identifiers not supported";
format_reason_code('wildcard-subscriptions-not-supported') ->
    "Wildcard Subscriptions not supported";
format_reason_code(Code) ->
    format("Unexpected error: ~w", [Code]).

-spec is_error_code(char() | reason_code()) -> boolean().
is_error_code('success') -> false;
is_error_code('normal-disconnection') -> false;
is_error_code('granted-qos-0') -> false;
is_error_code('granted-qos-1') -> false;
is_error_code('granted-qos-2') -> false;
is_error_code('disconnect-with-will-message') -> false;
is_error_code('no-matching-subscribers') -> false;
is_error_code('no-subscription-existed') -> false;
is_error_code('continue-authentication') -> false;
is_error_code('re-authenticate') -> false;
is_error_code(Code) when is_integer(Code) -> Code >= 128;
is_error_code(_) -> true.

%%%===================================================================
%%% Decoder
%%%===================================================================
-spec decode_varint(binary()) -> {non_neg_integer(), binary()} | more.
decode_varint(Data) ->
    decode_varint(Data, 0, 1).

-spec decode_varint(binary(), non_neg_integer(), pos_integer()) ->
			{non_neg_integer(), binary()} | more.
decode_varint(<<C, Data/binary>>, Val, Mult) ->
    NewVal = Val + (C band 127) * Mult,
    NewMult = Mult*128,
    if NewMult > ?MAX_VARINT ->
	    err(bad_varint);
       (C band 128) == 0 ->
	    {NewVal, Data};
       true ->
	    decode_varint(Data, NewVal, NewMult)
    end;
decode_varint(_, _, _) ->
    more.

-spec decode_pkt(mqtt_version() | undefined,
                 non_neg_integer(), non_neg_integer(), binary()) -> mqtt_packet().
decode_pkt(undefined, 1, Flags, Data) ->
    decode_connect(Flags, Data);
decode_pkt(Version, Type, Flags, Data) when Version /= undefined, Type>1 ->
    case Type of
	2 -> decode_connack(Version, Flags, Data);
	3 -> decode_publish(Version, Flags, Data);
	4 -> decode_puback(Version, Flags, Data);
	5 -> decode_pubrec(Version, Flags, Data);
	6 -> decode_pubrel(Version, Flags, Data);
	7 -> decode_pubcomp(Version, Flags, Data);
	8 -> decode_subscribe(Version, Flags, Data);
	9 -> decode_suback(Version, Flags, Data);
	10 -> decode_unsubscribe(Version, Flags, Data);
	11 -> decode_unsuback(Version, Flags, Data);
	12 -> decode_pingreq(Flags, Data);
	13 -> decode_pingresp(Flags, Data);
	14 -> decode_disconnect(Version, Flags, Data);
        15 when Version == ?MQTT_VERSION_5 -> decode_auth(Flags, Data);
        _ -> err({bad_packet_type, Type})
    end;
decode_pkt(_, Type, _, _) ->
    err({unexpected_packet, decode_packet_type(Type)}).

-spec decode_connect(non_neg_integer(), binary()) -> connect().
decode_connect(Flags, <<ProtoLen:16, Proto:ProtoLen/binary,
                        ProtoLevel, Data/binary>>) ->
    assert(Proto, <<"MQTT">>, unsupported_protocol_name),
    if ProtoLevel == ?MQTT_VERSION_4; ProtoLevel == ?MQTT_VERSION_5 ->
            decode_connect(ProtoLevel, Flags, Data);
       true ->
            err({unsupported_protocol_version, ProtoLevel, "4 or 5"})
    end;
decode_connect(_, _) ->
    err({bad_packet, connect}).

-spec decode_connect(mqtt_version(), non_neg_integer(), binary()) -> connect().
decode_connect(Version, Flags,
               <<UserFlag:1, PassFlag:1, WillRetain:1,
                 WillQoS:2, WillFlag:1, CleanStart:1,
                 Reserved:1, KeepAlive:16, Data/binary>>) ->
    assert(Flags, 0, {bad_flags, connect}),
    assert(Reserved, 0, {bad_flag, reserved}),
    {Props, Data1} = case Version of
                         ?MQTT_VERSION_5 -> decode_props(connect, Data);
                         ?MQTT_VERSION_4 -> {#{}, Data}
                     end,
    case Data1 of
        <<ClientIDLen:16, ClientID:ClientIDLen/binary, Data2/binary>> ->
            {Will, WillProps, Data3} =
                decode_will(Version, WillFlag, WillRetain, WillQoS, Data2),
            {Username, Password} = decode_user_pass(UserFlag, PassFlag, Data3),
            #connect{proto_level = Version,
                     will = Will,
                     will_properties = WillProps,
                     properties = Props,
                     clean_start = dec_bool(CleanStart),
                     keep_alive = KeepAlive,
                     client_id = utf8(ClientID),
                     username = utf8(Username),
                     password = Password};
        _ ->
            err({bad_packet, connect})
    end;
decode_connect(_, _, _) ->
    err({bad_packet, connect}).

-spec decode_connack(mqtt_version(), non_neg_integer(), binary()) -> connack().
decode_connack(Version, Flags, <<0:7, SessionPresent:1, Data/binary>>) ->
    assert(Flags, 0, {bad_flags, connack}),
    {Code, PropMap} = decode_code_with_props(Version, connack, Data),
    #connack{session_present = dec_bool(SessionPresent),
	     code = Code, properties = PropMap};
decode_connack(_, _, _) ->
    err({bad_packet, connack}).

-spec decode_publish(mqtt_version(), non_neg_integer(), binary()) -> publish().
decode_publish(Version, Flags, <<TLen:16, Topic:TLen/binary, Data/binary>>) ->
    Retain = Flags band 1,
    QoS = qos((Flags bsr 1) band 3),
    DUP = Flags band 8,
    {ID, Props, Payload} = decode_id_props_payload(Version, QoS, Data),
    #publish{dup = dec_bool(DUP),
	     qos = QoS,
	     retain = dec_bool(Retain),
	     topic = topic(Topic, Props),
	     id = ID,
             properties = Props,
	     payload = Payload};
decode_publish(_, _, _) ->
    err({bad_packet, publish}).

-spec decode_puback(mqtt_version(), non_neg_integer(), binary()) -> puback().
decode_puback(Version, Flags, <<ID:16, Data/binary>>) when ID>0 ->
    assert(Flags, 0, {bad_flags, puback}),
    {Code, PropMap} = decode_code_with_props(Version, puback, Data),
    #puback{id = ID, code = Code, properties = PropMap};
decode_puback(_, _, _) ->
    err({bad_packet, puback}).

-spec decode_pubrec(mqtt_version(), non_neg_integer(), binary()) -> pubrec().
decode_pubrec(Version, Flags, <<ID:16, Data/binary>>) when ID>0 ->
    assert(Flags, 0, {bad_flags, pubrec}),
    {Code, PropMap} = decode_code_with_props(Version, pubrec, Data),
    #pubrec{id = ID, code = Code, properties = PropMap};
decode_pubrec(_, _, _) ->
    err({bad_packet, pubrec}).

-spec decode_pubrel(mqtt_version(), non_neg_integer(), binary()) -> pubrel().
decode_pubrel(Version, Flags, <<ID:16, Data/binary>>) when ID>0 ->
    assert(Flags, 2, {bad_flags, pubrel}),
    {Code, PropMap} = decode_code_with_props(Version, pubrel, Data),
    #pubrel{id = ID, code = Code, properties = PropMap};
decode_pubrel(_, _, _) ->
    err({bad_packet, pubrel}).

-spec decode_pubcomp(mqtt_version(), non_neg_integer(), binary()) -> pubcomp().
decode_pubcomp(Version, Flags, <<ID:16, Data/binary>>) when ID>0 ->
    assert(Flags, 0, {bad_flags, pubcomp}),
    {Code, PropMap} = decode_code_with_props(Version, pubcomp, Data),
    #pubcomp{id = ID, code = Code, properties = PropMap};
decode_pubcomp(_, _, _) ->
    err({bad_packet, pubcomp}).

-spec decode_subscribe(mqtt_version(), non_neg_integer(), binary()) -> subscribe().
decode_subscribe(Version, Flags, <<ID:16, Data/binary>>) when ID>0 ->
    assert(Flags, 2, {bad_flags, subscribe}),
    case Version of
        ?MQTT_VERSION_4 ->
            Filters = decode_subscribe_filters(Data),
            #subscribe{id = ID, filters = Filters};
        ?MQTT_VERSION_5 ->
            {Props, Payload} = decode_props(subscribe, Data),
            Filters = decode_subscribe_filters(Payload),
            #subscribe{id = ID, filters = Filters, properties = Props}
    end;
decode_subscribe(_, _, _) ->
    err({bad_packet, subscribe}).

-spec decode_suback(mqtt_version(), non_neg_integer(), binary()) -> suback().
decode_suback(Version, Flags, <<ID:16, Data/binary>>) when ID>0 ->
    assert(Flags, 0, {bad_flags, suback}),
    case Version of
        ?MQTT_VERSION_4 ->
            #suback{id = ID,
                    codes = decode_suback_codes(Data)};
        ?MQTT_VERSION_5 ->
            {PropMap, Tail} = decode_props(suback, Data),
            #suback{id = ID,
                    codes = decode_suback_codes(Tail),
                    properties = PropMap}
    end;
decode_suback(_, _, _) ->
    err({bad_packet, suback}).

-spec decode_unsubscribe(mqtt_version(), non_neg_integer(), binary()) -> unsubscribe().
decode_unsubscribe(Version, Flags, <<ID:16, Data/binary>>) when ID>0 ->
    assert(Flags, 2, {bad_flags, unsubscribe}),
    case Version of
        ?MQTT_VERSION_4 ->
            Filters = decode_unsubscribe_filters(Data),
            #unsubscribe{id = ID, filters = Filters};
        ?MQTT_VERSION_5 ->
            {Props, Payload} = decode_props(unsubscribe, Data),
            Filters = decode_unsubscribe_filters(Payload),
            #unsubscribe{id = ID, filters = Filters, properties = Props}
    end;
decode_unsubscribe(_, _, _) ->
    err({bad_packet, unsubscribe}).

-spec decode_unsuback(mqtt_version(), non_neg_integer(), binary()) -> unsuback().
decode_unsuback(Version, Flags, <<ID:16, Data/binary>>) when ID>0 ->
    assert(Flags, 0, {bad_flags, unsuback}),
    case Version of
        ?MQTT_VERSION_4 ->
            #unsuback{id = ID};
        ?MQTT_VERSION_5 ->
            {PropMap, Tail} = decode_props(unsuback, Data),
            #unsuback{id = ID,
                      codes = decode_unsuback_codes(Tail),
                      properties = PropMap}
    end;
decode_unsuback(_, _, _) ->
    err({bad_packet, unsuback}).

-spec decode_pingreq(non_neg_integer(), binary()) -> pingreq().
decode_pingreq(Flags, <<>>) ->
    assert(Flags, 0, {bad_flags, pingreq}),
    #pingreq{};
decode_pingreq(_, _) ->
    err({bad_packet, pingreq}).

-spec decode_pingresp(non_neg_integer(), binary()) -> pingresp().
decode_pingresp(Flags, <<>>) ->
    assert(Flags, 0, {bad_flags, pingresp}),
    #pingresp{};
decode_pingresp(_, _) ->
    err({bad_packet, pingresp}).

-spec decode_disconnect(mqtt_version(), non_neg_integer(), binary()) -> disconnect().
decode_disconnect(Version, Flags, Payload) ->
    assert(Flags, 0, {bad_flags, disconnect}),
    {Code, PropMap} = decode_code_with_props(Version, disconnect, Payload),
    #disconnect{code = Code, properties = PropMap}.

-spec decode_auth(non_neg_integer(), binary()) -> auth().
decode_auth(Flags, Payload) ->
    assert(Flags, 0, {bad_flags, auth}),
    {Code, PropMap} = decode_code_with_props(?MQTT_VERSION_5, auth, Payload),
    #auth{code = Code, properties = PropMap}.

-spec decode_packet_type(char()) -> atom().
decode_packet_type(1) -> connect;
decode_packet_type(2) -> connack;
decode_packet_type(3) -> publish;
decode_packet_type(4) -> puback;
decode_packet_type(5) -> pubrec;
decode_packet_type(6) -> pubrel;
decode_packet_type(7) -> pubcomp;
decode_packet_type(8) -> subscribe;
decode_packet_type(9) -> suback;
decode_packet_type(10) -> unsubscribe;
decode_packet_type(11) -> unsuback;
decode_packet_type(12) -> pingreq;
decode_packet_type(13) -> pingresp;
decode_packet_type(14) -> disconnect;
decode_packet_type(15) -> auth;
decode_packet_type(T) -> err({bad_packet_type, T}).

-spec decode_will(mqtt_version(), 0|1, 0|1, qos(), binary()) ->
			 {undefined | publish(), properties(), binary()}.
decode_will(_, 0, WillRetain, WillQoS, Data) ->
    assert(WillRetain, 0, {bad_flag, will_retain}),
    assert(WillQoS, 0, {bad_flag, will_qos}),
    {undefined, #{}, Data};
decode_will(Version, 1, WillRetain, WillQoS, Data) ->
    {Props, Data1} = case Version of
                         ?MQTT_VERSION_5 -> decode_props(connect, Data);
                         ?MQTT_VERSION_4 -> {#{}, Data}
                     end,
    case Data1 of
        <<TLen:16, Topic:TLen/binary,
          MLen:16, Message:MLen/binary, Data2/binary>> ->
            {#publish{retain = dec_bool(WillRetain),
                      qos = qos(WillQoS),
                      topic = topic(Topic),
                      payload = Message},
             Props, Data2};
        _ ->
            err(bad_will_topic_or_message)
    end.

-spec decode_user_pass(non_neg_integer(), non_neg_integer(),
		       binary()) -> {binary(), binary()}.
decode_user_pass(1, 0, <<Len:16, User:Len/binary>>) ->
    {utf8(User), <<>>};
decode_user_pass(1, 1, <<ULen:16, User:ULen/binary,
			 PLen:16, Pass:PLen/binary>>) ->
    {utf8(User), Pass};
decode_user_pass(0, Flag, <<>>) ->
    assert(Flag, 0, {bad_flag, password}),
    {<<>>, <<>>};
decode_user_pass(_, _, _) ->
    err(bad_connect_username_or_password).

-spec decode_id_props_payload(mqtt_version(), non_neg_integer(), binary()) ->
                        {undefined | non_neg_integer(), properties(), binary()}.
decode_id_props_payload(Version, 0, Data) ->
    case Version of
        ?MQTT_VERSION_4 ->
            {undefined, #{}, Data};
        ?MQTT_VERSION_5 ->
            {Props, Payload} = decode_props(publish, Data),
            {undefined, Props, Payload}
    end;
decode_id_props_payload(Version, _, <<ID:16, Data/binary>>) when ID>0 ->
    case Version of
        ?MQTT_VERSION_4 ->
            {ID, #{}, Data};
        ?MQTT_VERSION_5 ->
            {Props, Payload} = decode_props(publish, Data),
            {ID, Props, Payload}
    end;
decode_id_props_payload(_, _, _) ->
    err(bad_publish_id_or_payload).

-spec decode_subscribe_filters(binary()) -> [{binary(), sub_opts()}].
decode_subscribe_filters(<<Len:16, Filter:Len/binary,
                           Reserved:2, RH:2, RAP:1, NL:1, QoS:2,
                           Tail/binary>>) ->
    assert(Reserved, 0, {bad_flag, reserved}),
    case RH of
        3 -> err({{bad_flag, retain_handling}, RH, "0, 1 or 2"});
        _ -> ok
    end,
    Opts = #sub_opts{qos = qos(QoS),
                     no_local = dec_bool(NL),
                     retain_as_published = dec_bool(RAP),
                     retain_handling = RH},
    [{topic_filter(Filter), Opts}|decode_subscribe_filters(Tail)];
decode_subscribe_filters(<<>>) ->
    [];
decode_subscribe_filters(_) ->
    err({bad_topic_filters, subscribe}).

-spec decode_unsubscribe_filters(binary()) -> [binary()].
decode_unsubscribe_filters(<<Len:16, Filter:Len/binary, Tail/binary>>) ->
    [topic_filter(Filter)|decode_unsubscribe_filters(Tail)];
decode_unsubscribe_filters(<<>>) ->
    [];
decode_unsubscribe_filters(_) ->
    err({bad_topic_filters, unsubscribe}).

-spec decode_suback_codes(binary()) -> [reason_code()].
decode_suback_codes(<<Code, Data/binary>>) ->
    [decode_suback_code(Code)|decode_suback_codes(Data)];
decode_suback_codes(<<>>) ->
    [].

-spec decode_unsuback_codes(binary()) -> [reason_code()].
decode_unsuback_codes(<<Code, Data/binary>>) ->
    [decode_unsuback_code(Code)|decode_unsuback_codes(Data)];
decode_unsuback_codes(<<>>) ->
    [].

-spec decode_utf8_pair(binary()) -> {utf8_pair(), binary()}.
decode_utf8_pair(<<NSize:16, Name:NSize/binary,
                   VSize:16, Val:VSize/binary, Tail/binary>>) ->
    {{utf8(Name), utf8(Val)}, Tail};
decode_utf8_pair(_) ->
    err(bad_utf8_pair).

-spec decode_props(atom(), binary()) -> {properties(), binary()}.
decode_props(Pkt, Data) ->
    try
        {Len, Data1} = decode_varint(Data),
        <<PData:Len/binary, Tail/binary>> = Data1,
        {decode_props(Pkt, PData, #{}), Tail}
    catch _:{badmatch, _} ->
            err({bad_properties, Pkt})
    end.

-spec decode_props(atom(), binary(), properties()) -> properties().
decode_props(_, <<>>, Props) ->
    Props;
decode_props(Pkt, Data, Props) ->
    {Type, Payload} = decode_varint(Data),
    {Name, Val, Tail} = decode_prop(Pkt, Type, Payload),
    Props1 = maps:update_with(
               Name,
               fun(Vals) when is_list(Val) ->
                       Vals ++ Val;
                  (_) ->
                       err({duplicated_property, Pkt, Name})
               end, Val, Props),
    decode_props(Pkt, Tail, Props1).

-spec decode_prop(atom(), char(), binary()) -> {property(), term(), binary()}.
decode_prop(_, 18, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {assigned_client_identifier, utf8(Data), Bin};
decode_prop(_, 22, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {authentication_data, Data, Bin};
decode_prop(_, 21, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {authentication_method, utf8(Data), Bin};
decode_prop(_, 3, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {content_type, utf8(Data), Bin};
decode_prop(_, 9, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {correlation_data, Data, Bin};
decode_prop(_, 39, <<Size:32, Bin/binary>>) when Size>0 ->
    {maximum_packet_size, Size, Bin};
decode_prop(Pkt, 36, <<QoS, Bin/binary>>) ->
    {maximum_qos,
     case QoS of
         0 -> 0;
         1 -> 1;
         _ -> err({bad_property, Pkt, maximum_qos})
     end, Bin};
decode_prop(_, 2, <<I:32, Bin/binary>>) ->
    {message_expiry_interval, I, Bin};
decode_prop(Pkt, 1, <<I, Bin/binary>>) ->
    {payload_format_indicator,
     case I of
         0 -> binary;
         1 -> utf8;
         _ -> err({bad_property, Pkt, payload_format_indicator})
     end, Bin};
decode_prop(_, 31, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {reason_string, utf8(Data), Bin};
decode_prop(_, 33, <<Max:16, Bin/binary>>) when Max>0 ->
    {receive_maximum, Max, Bin};
decode_prop(Pkt, 23, Data) ->
    decode_bool_prop(Pkt, request_problem_information, Data);
decode_prop(Pkt, 25, Data) ->
    decode_bool_prop(Pkt, request_response_information, Data);
decode_prop(_, 26, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {response_information, utf8(Data), Bin};
decode_prop(_, 8, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {response_topic, topic(Data), Bin};
decode_prop(Pkt, 37, Data) ->
    decode_bool_prop(Pkt, retain_available, Data);
decode_prop(_, 19, <<Secs:16, Bin/binary>>) ->
    {server_keep_alive, Secs, Bin};
decode_prop(_, 28, <<Size:16, Data:Size/binary, Bin/binary>>) ->
    {server_reference, utf8(Data), Bin};
decode_prop(_, 17, <<I:32, Bin/binary>>) ->
    {session_expiry_interval, I, Bin};
decode_prop(Pkt, 42, Data) ->
    decode_bool_prop(Pkt, shared_subscription_available, Data);
decode_prop(Pkt, 11, Data) when Pkt == publish; Pkt == subscribe ->
    case decode_varint(Data) of
        {ID, Bin} when Pkt == publish ->
            {subscription_identifier, [ID], Bin};
        {ID, Bin} when Pkt == subscribe ->
            {subscription_identifier, ID, Bin};
        _ ->
            err({bad_property, publish, subscription_identifier})
     end;
decode_prop(Pkt, 41, Data) ->
    decode_bool_prop(Pkt, subscription_identifiers_available, Data);
decode_prop(_, 35, <<Alias:16, Bin/binary>>) when Alias>0 ->
    {topic_alias, Alias, Bin};
decode_prop(_, 34, <<Max:16, Bin/binary>>) ->
    {topic_alias_maximum, Max, Bin};
decode_prop(_, 38, Data) ->
    {Pair, Bin} = decode_utf8_pair(Data),
    {user_property, [Pair], Bin};
decode_prop(Pkt, 40, Data) ->
    decode_bool_prop(Pkt, wildcard_subscription_available, Data);
decode_prop(_, 24, <<I:32, Bin/binary>>) ->
    {will_delay_interval, I, Bin};
decode_prop(Pkt, _, _) ->
    err({bad_properties, Pkt}).

decode_bool_prop(Pkt, Name, <<Val, Bin/binary>>) ->
    case Val of
        0 -> {Name, false, Bin};
        1 -> {Name, true, Bin};
        _ -> err({bad_property, Pkt, Name})
    end;
decode_bool_prop(Pkt, Name, _) ->
    err({bad_property, Pkt, Name}).

-spec decode_code_with_props(mqtt_version(), atom(), binary()) ->
                                    {reason_code(), properties()}.
decode_code_with_props(_, connack, <<Code, Props/binary>>) ->
    {decode_connack_code(Code),
     case Props of
         <<>> ->
             #{};
         _ ->
             {PropMap, <<>>} = decode_props(connack, Props),
             PropMap
     end};
decode_code_with_props(_, Pkt, <<>>) ->
    {decode_reason_code(Pkt, 0), #{}};
decode_code_with_props(?MQTT_VERSION_5, Pkt, <<Code>>) ->
    {decode_reason_code(Pkt, Code), #{}};
decode_code_with_props(?MQTT_VERSION_5, Pkt, <<Code, Props/binary>>) ->
    {PropMap, <<>>} = decode_props(Pkt, Props),
    {decode_reason_code(Pkt, Code), PropMap};
decode_code_with_props(_, Pkt, _) ->
    err({bad_packet, Pkt}).

-spec decode_pubcomp_code(char()) -> reason_code().
decode_pubcomp_code(0) -> 'success';
decode_pubcomp_code(146) -> 'packet-identifier-not-found';
decode_pubcomp_code(Code) -> err({bad_reason_code, pubcomp, Code}).

-spec decode_pubrec_code(char()) -> reason_code().
decode_pubrec_code(0) -> 'success';
decode_pubrec_code(16) -> 'no-matching-subscribers';
decode_pubrec_code(128) -> 'unspecified-error';
decode_pubrec_code(131) -> 'implementation-specific-error';
decode_pubrec_code(135) -> 'not-authorized';
decode_pubrec_code(144) -> 'topic-name-invalid';
decode_pubrec_code(145) -> 'packet-identifier-in-use';
decode_pubrec_code(151) -> 'quota-exceeded';
decode_pubrec_code(153) -> 'payload-format-invalid';
decode_pubrec_code(Code) -> err({bad_reason_code, pubrec, Code}).

-spec decode_disconnect_code(char()) -> reason_code().
decode_disconnect_code(0) -> 'normal-disconnection';
decode_disconnect_code(4) -> 'disconnect-with-will-message';
decode_disconnect_code(128) -> 'unspecified-error';
decode_disconnect_code(129) -> 'malformed-packet';
decode_disconnect_code(130) -> 'protocol-error';
decode_disconnect_code(131) -> 'implementation-specific-error';
decode_disconnect_code(135) -> 'not-authorized';
decode_disconnect_code(137) -> 'server-busy';
decode_disconnect_code(139) -> 'server-shutting-down';
decode_disconnect_code(140) -> 'bad-authentication-method';
decode_disconnect_code(141) -> 'keep-alive-timeout';
decode_disconnect_code(142) -> 'session-taken-over';
decode_disconnect_code(143) -> 'topic-filter-invalid';
decode_disconnect_code(144) -> 'topic-name-invalid';
decode_disconnect_code(147) -> 'receive-maximum-exceeded';
decode_disconnect_code(148) -> 'topic-alias-invalid';
decode_disconnect_code(149) -> 'packet-too-large';
decode_disconnect_code(150) -> 'message-rate-too-high';
decode_disconnect_code(151) -> 'quota-exceeded';
decode_disconnect_code(152) -> 'administrative-action';
decode_disconnect_code(153) -> 'payload-format-invalid';
decode_disconnect_code(154) -> 'retain-not-supported';
decode_disconnect_code(155) -> 'qos-not-supported';
decode_disconnect_code(156) -> 'use-another-server';
decode_disconnect_code(157) -> 'server-moved';
decode_disconnect_code(158) -> 'shared-subscriptions-not-supported';
decode_disconnect_code(159) -> 'connection-rate-exceeded';
decode_disconnect_code(160) -> 'maximum-connect-time';
decode_disconnect_code(161) -> 'subscription-identifiers-not-supported';
decode_disconnect_code(162) -> 'wildcard-subscriptions-not-supported';
decode_disconnect_code(Code) -> err({bad_reason_code, disconnect, Code}).

-spec decode_auth_code(char()) -> reason_code().
decode_auth_code(0) -> 'success';
decode_auth_code(24) -> 'continue-authentication';
decode_auth_code(25) -> 're-authenticate';
decode_auth_code(Code) -> err({bad_reason_code, auth, Code}).

-spec decode_suback_code(char()) -> 0..2 | reason_code().
decode_suback_code(0) -> 0;
decode_suback_code(1) -> 1;
decode_suback_code(2) -> 2;
decode_suback_code(128) -> 'unspecified-error';
decode_suback_code(131) -> 'implementation-specific-error';
decode_suback_code(135) -> 'not-authorized';
decode_suback_code(143) -> 'topic-filter-invalid';
decode_suback_code(145) -> 'packet-identifier-in-use';
decode_suback_code(151) -> 'quota-exceeded';
decode_suback_code(158) -> 'shared-subscriptions-not-supported';
decode_suback_code(161) -> 'subscription-identifiers-not-supported';
decode_suback_code(162) -> 'wildcard-subscriptions-not-supported';
decode_suback_code(Code) -> err({bad_reason_code, suback, Code}).

-spec decode_unsuback_code(char()) -> reason_code().
decode_unsuback_code(0) -> 'success';
decode_unsuback_code(17) -> 'no-subscription-existed';
decode_unsuback_code(128) -> 'unspecified-error';
decode_unsuback_code(131) -> 'implementation-specific-error';
decode_unsuback_code(135) -> 'not-authorized';
decode_unsuback_code(143) -> 'topic-filter-invalid';
decode_unsuback_code(145) -> 'packet-identifier-in-use';
decode_unsuback_code(Code) -> err({bad_reason_code, unsuback, Code}).

-spec decode_puback_code(char()) -> reason_code().
decode_puback_code(0) -> 'success';
decode_puback_code(16) -> 'no-matching-subscribers';
decode_puback_code(128) -> 'unspecified-error';
decode_puback_code(131) -> 'implementation-specific-error';
decode_puback_code(135) -> 'not-authorized';
decode_puback_code(144) -> 'topic-name-invalid';
decode_puback_code(145) -> 'packet-identifier-in-use';
decode_puback_code(151) -> 'quota-exceeded';
decode_puback_code(153) -> 'payload-format-invalid';
decode_puback_code(Code) -> err({bad_reason_code, puback, Code}).

-spec decode_pubrel_code(char()) -> reason_code().
decode_pubrel_code(0) -> 'success';
decode_pubrel_code(146) -> 'packet-identifier-not-found';
decode_pubrel_code(Code) -> err({bad_reason_code, pubrel, Code}).

-spec decode_connack_code(char()) -> reason_code().
decode_connack_code(0) -> 'success';
decode_connack_code(1) -> 'unsupported-protocol-version';
decode_connack_code(2) -> 'client-identifier-not-valid';
decode_connack_code(3) -> 'server-unavailable';
decode_connack_code(4) -> 'bad-user-name-or-password';
decode_connack_code(5) -> 'not-authorized';
decode_connack_code(128) -> 'unspecified-error';
decode_connack_code(129) -> 'malformed-packet';
decode_connack_code(130) -> 'protocol-error';
decode_connack_code(131) -> 'implementation-specific-error';
decode_connack_code(132) -> 'unsupported-protocol-version';
decode_connack_code(133) -> 'client-identifier-not-valid';
decode_connack_code(134) -> 'bad-user-name-or-password';
decode_connack_code(135) -> 'not-authorized';
decode_connack_code(136) -> 'server-unavailable';
decode_connack_code(137) -> 'server-busy';
decode_connack_code(138) -> 'banned';
decode_connack_code(140) -> 'bad-authentication-method';
decode_connack_code(144) -> 'topic-name-invalid';
decode_connack_code(149) -> 'packet-too-large';
decode_connack_code(151) -> 'quota-exceeded';
decode_connack_code(153) -> 'payload-format-invalid';
decode_connack_code(154) -> 'retain-not-supported';
decode_connack_code(155) -> 'qos-not-supported';
decode_connack_code(156) -> 'use-another-server';
decode_connack_code(157) -> 'server-moved';
decode_connack_code(159) -> 'connection-rate-exceeded';
decode_connack_code(Code) -> err({bad_reason_code, connack, Code}).

-spec decode_reason_code(atom(), char()) -> reason_code().
decode_reason_code(pubcomp, Code) -> decode_pubcomp_code(Code);
decode_reason_code(pubrec, Code) -> decode_pubrec_code(Code);
decode_reason_code(disconnect, Code) -> decode_disconnect_code(Code);
decode_reason_code(auth, Code) -> decode_auth_code(Code);
decode_reason_code(puback, Code) -> decode_puback_code(Code);
decode_reason_code(pubrel, Code) -> decode_pubrel_code(Code);
decode_reason_code(connack, Code) -> decode_connack_code(Code).

%%%===================================================================
%%% Encoder
%%%===================================================================
encode_connect(#connect{proto_level = Version, properties = Props,
                        will = Will, will_properties = WillProps,
                        clean_start = CleanStart,
                        keep_alive = KeepAlive, client_id = ClientID,
                        username = Username, password = Password}) ->
    UserFlag = Username /= <<>>,
    PassFlag = UserFlag andalso Password /= <<>>,
    WillFlag = is_record(Will, publish),
    WillRetain = WillFlag andalso Will#publish.retain,
    WillQoS = if WillFlag -> Will#publish.qos;
		 true -> 0
	      end,
    Header = <<4:16, "MQTT", Version, (enc_bool(UserFlag)):1,
	       (enc_bool(PassFlag)):1, (enc_bool(WillRetain)):1,
	       WillQoS:2, (enc_bool(WillFlag)):1,
	       (enc_bool(CleanStart)):1, 0:1,
	       KeepAlive:16>>,
    EncClientID = <<(size(ClientID)):16, ClientID/binary>>,
    EncWill = encode_will(Will),
    EncUserPass = encode_user_pass(Username, Password),
    Payload = case Version of
                  ?MQTT_VERSION_5 ->
                      [Header, encode_props(Props), EncClientID,
                       if WillFlag -> encode_props(WillProps);
                          true -> <<>>
                       end,
                       EncWill, EncUserPass];
                  _ ->
                      [Header, EncClientID, EncWill, EncUserPass]
              end,
    <<1:4, 0:4, (encode_with_len(Payload))/binary>>.

encode_connack(Version, #connack{session_present = SP,
                                 code = Code, properties = Props}) ->
    Payload = [enc_bool(SP),
               encode_connack_code(Version, Code),
               encode_props(Version, Props)],
    <<2:4, 0:4, (encode_with_len(Payload))/binary>>.

encode_publish(Version, #publish{qos = QoS, retain = Retain, dup = Dup,
                                 topic = Topic, id = ID, payload = Payload,
                                 properties = Props}) ->
    Data1 = <<(size(Topic)):16, Topic/binary>>,
    Data2 = case QoS of
		0 -> <<>>;
		_ when ID>0 -> <<ID:16>>
	    end,
    Data3 = encode_props(Version, Props),
    Data4 = encode_with_len([Data1, Data2, Data3, Payload]),
    <<3:4, (enc_bool(Dup)):1, QoS:2, (enc_bool(Retain)):1, Data4/binary>>.

encode_puback(Version, #puback{id = ID, code = Code,
                               properties = Props}) when ID>0 ->
    Data = encode_code_with_props(Version, Code, Props),
    <<4:4, 0:4, (encode_with_len([<<ID:16>>|Data]))/binary>>.

encode_pubrec(Version, #pubrec{id = ID, code = Code,
                               properties = Props}) when ID>0 ->
    Data = encode_code_with_props(Version, Code, Props),
    <<5:4, 0:4, (encode_with_len([<<ID:16>>|Data]))/binary>>.

encode_pubrel(Version, #pubrel{id = ID, code = Code,
                               properties = Props}) when ID>0 ->
    Data = encode_code_with_props(Version, Code, Props),
    <<6:4, 2:4, (encode_with_len([<<ID:16>>|Data]))/binary>>.

encode_pubcomp(Version, #pubcomp{id = ID, code = Code,
                                 properties = Props}) when ID>0 ->
    Data = encode_code_with_props(Version, Code, Props),
    <<7:4, 0:4, (encode_with_len([<<ID:16>>|Data]))/binary>>.

encode_subscribe(Version, #subscribe{id = ID,
                                     filters = [_|_] = Filters,
                                     properties = Props}) when ID>0 ->
    EncFilters = [<<(size(Filter)):16, Filter/binary,
                    (encode_subscription_options(SubOpts))>> ||
		     {Filter, SubOpts} <- Filters],
    Payload = [<<ID:16>>, encode_props(Version, Props), EncFilters],
    <<8:4, 2:4, (encode_with_len(Payload))/binary>>.

encode_suback(Version, #suback{id = ID, codes = Codes,
                               properties = Props}) when ID>0 ->
    Payload = [<<ID:16>>, encode_props(Version, Props)
               |[encode_reason_code(Code) || Code <- Codes]],
    <<9:4, 0:4, (encode_with_len(Payload))/binary>>.

encode_unsubscribe(Version, #unsubscribe{id = ID,
                                         filters = [_|_] = Filters,
                                         properties = Props}) when ID>0 ->
    EncFilters = [<<(size(Filter)):16, Filter/binary>> || Filter <- Filters],
    Payload = [<<ID:16>>, encode_props(Version, Props), EncFilters],
    <<10:4, 2:4, (encode_with_len(Payload))/binary>>.

encode_unsuback(Version, #unsuback{id = ID, codes = Codes,
                                   properties = Props}) when ID>0 ->
    EncCodes = case Version of
                   ?MQTT_VERSION_5 ->
                       [encode_reason_code(Code) || Code <- Codes];
                   ?MQTT_VERSION_4 ->
                       []
               end,
    Payload = [<<ID:16>>, encode_props(Version, Props)|EncCodes],
    <<11:4, 0:4, (encode_with_len(Payload))/binary>>.

encode_pingreq() ->
    <<12:4, 0:4, 0>>.

encode_pingresp() ->
    <<13:4, 0:4, 0>>.

encode_disconnect(Version, #disconnect{code = Code, properties = Props}) ->
    Data = encode_code_with_props(Version, Code, Props),
    <<14:4, 0:4, (encode_with_len(Data))/binary>>.

encode_auth(#auth{code = Code, properties = Props}) ->
    Data = encode_code_with_props(?MQTT_VERSION_5, Code, Props),
    <<15:4, 0:4, (encode_with_len(Data))/binary>>.

-spec encode_with_len(iodata()) -> binary().
encode_with_len(IOData) ->
    Data = iolist_to_binary(IOData),
    Len = encode_varint(size(Data)),
    <<Len/binary, Data/binary>>.

-spec encode_varint(non_neg_integer()) -> binary().
encode_varint(X) when X < 128 ->
    <<0:1, X:7>>;
encode_varint(X) when X < ?MAX_VARINT ->
    <<1:1, (X rem 128):7, (encode_varint(X div 128))/binary>>.

-spec encode_props(mqtt_version(), properties()) -> binary().
encode_props(?MQTT_VERSION_5, Props) ->
    encode_props(Props);
encode_props(?MQTT_VERSION_4, _) ->
    <<>>.

-spec encode_props(properties()) -> binary().
encode_props(Props) ->
    encode_with_len(
      maps:fold(
        fun(Name, Val, Acc) ->
                [encode_prop(Name, Val)|Acc]
        end, [], Props)).

-spec encode_prop(property(), term()) -> iodata().
encode_prop(assigned_client_identifier, <<>>) ->
    <<>>;
encode_prop(assigned_client_identifier, ID) ->
    <<18, (size(ID)):16, ID/binary>>;
encode_prop(authentication_data, <<>>) ->
    <<>>;
encode_prop(authentication_data, Data) ->
    <<22, (size(Data)):16, Data/binary>>;
encode_prop(authentication_method, <<>>) ->
    <<>>;
encode_prop(authentication_method, M) ->
    <<21, (size(M)):16, M/binary>>;
encode_prop(content_type, <<>>) ->
    <<>>;
encode_prop(content_type, T) ->
    <<3, (size(T)):16, T/binary>>;
encode_prop(correlation_data, <<>>) ->
    <<>>;
encode_prop(correlation_data, Data) ->
    <<9, (size(Data)):16, Data/binary>>;
encode_prop(maximum_packet_size, Size) when Size>0, Size=<?MAX_UINT32 ->
    <<39, Size:32>>;
encode_prop(maximum_qos, QoS) when QoS>=0, QoS<2 ->
    <<36, QoS>>;
encode_prop(message_expiry_interval, I) when I>=0, I=<?MAX_UINT32 ->
    <<2, I:32>>;
encode_prop(payload_format_indicator, binary) ->
    <<>>;
encode_prop(payload_format_indicator, utf8) ->
    <<1, 1>>;
encode_prop(reason_string, <<>>) ->
    <<>>;
encode_prop(reason_string, S) ->
    <<31, (size(S)):16, S/binary>>;
encode_prop(receive_maximum, Max) when Max>0, Max=<?MAX_UINT16 ->
    <<33, Max:16>>;
encode_prop(request_problem_information, true) ->
    <<>>;
encode_prop(request_problem_information, false) ->
    <<23, 0>>;
encode_prop(request_response_information, false) ->
    <<>>;
encode_prop(request_response_information, true) ->
    <<25, 1>>;
encode_prop(response_information, <<>>) ->
    <<>>;
encode_prop(response_information, S) ->
    <<26, (size(S)):16, S/binary>>;
encode_prop(response_topic, <<>>) ->
    <<>>;
encode_prop(response_topic, T) ->
    <<8, (size(T)):16, T/binary>>;
encode_prop(retain_available, true) ->
    <<>>;
encode_prop(retain_available, false) ->
    <<37, 0>>;
encode_prop(server_keep_alive, Secs) when Secs>=0, Secs=<?MAX_UINT16 ->
    <<19, Secs:16>>;
encode_prop(server_reference, <<>>) ->
    <<>>;
encode_prop(server_reference, S) ->
    <<28, (size(S)):16, S/binary>>;
encode_prop(session_expiry_interval, I) when I>=0, I=<?MAX_UINT32 ->
    <<17, I:32>>;
encode_prop(shared_subscription_available, true) ->
    <<>>;
encode_prop(shared_subscription_available, false) ->
    <<42, 0>>;
encode_prop(subscription_identifier, [_|_] = IDs) ->
    [encode_prop(subscription_identifier, ID) || ID <- IDs];
encode_prop(subscription_identifier, ID) when ID>0, ID<?MAX_VARINT ->
    <<11, (encode_varint(ID))/binary>>;
encode_prop(subscription_identifiers_available, true) ->
    <<>>;
encode_prop(subscription_identifiers_available, false) ->
    <<41, 0>>;
encode_prop(topic_alias, Alias) when Alias>0, Alias=<?MAX_UINT16 ->
    <<35, Alias:16>>;
encode_prop(topic_alias_maximum, 0) ->
    <<>>;
encode_prop(topic_alias_maximum, Max) when Max>0, Max=<?MAX_UINT16 ->
    <<34, Max:16>>;
encode_prop(user_property, Pairs) ->
    [<<38, (encode_utf8_pair(Pair))/binary>> || Pair <- Pairs];
encode_prop(wildcard_subscription_available, true) ->
    <<>>;
encode_prop(wildcard_subscription_available, false) ->
    <<40, 0>>;
encode_prop(will_delay_interval, 0) ->
    <<>>;
encode_prop(will_delay_interval, I) when I>0, I=<?MAX_UINT32 ->
    <<24, I:32>>.

-spec encode_user_pass(binary(), binary()) -> binary().
encode_user_pass(User, Pass) when User /= <<>> andalso Pass /= <<>> ->
    <<(size(User)):16, User/binary, (size(Pass)):16, Pass/binary>>;
encode_user_pass(User, _) when User /= <<>> ->
    <<(size(User)):16, User/binary>>;
encode_user_pass(_, _) ->
    <<>>.

-spec encode_will(undefined | publish()) -> binary().
encode_will(#publish{topic = Topic, payload = Payload}) ->
    <<(size(Topic)):16, Topic/binary,
      (size(Payload)):16, Payload/binary>>;
encode_will(undefined) ->
    <<>>.

encode_subscription_options(#sub_opts{qos = QoS,
                                      no_local = NL,
                                      retain_as_published = RAP,
                                      retain_handling = RH})
  when QoS>=0, RH>=0, QoS<3, RH<3 ->
    (RH bsl 4) bor (enc_bool(RAP) bsl 3) bor (enc_bool(NL) bsl 2) bor QoS.

-spec encode_code_with_props(mqtt_version(), reason_code(), properties()) -> [binary()].
encode_code_with_props(Version, Code, Props) ->
    if Version == ?MQTT_VERSION_4 orelse
       (Code == success andalso Props == #{}) ->
            [];
       Props == #{} ->
            [encode_reason_code(Code)];
       true ->
            [encode_reason_code(Code), encode_props(Props)]
    end.

-spec encode_utf8_pair({binary(), binary()}) -> binary().
encode_utf8_pair({Key, Val}) ->
    <<(size(Key)):16, Key/binary, (size(Val)):16, Val/binary>>.

-spec encode_connack_code(mqtt_version(), atom()) -> char().
encode_connack_code(?MQTT_VERSION_5, Reason) -> encode_reason_code(Reason);
encode_connack_code(_, success) -> 0;
encode_connack_code(_, 'unsupported-protocol-version') -> 1;
encode_connack_code(_, 'client-identifier-not-valid') -> 2;
encode_connack_code(_, 'server-unavailable') -> 3;
encode_connack_code(_, 'bad-user-name-or-password') -> 4;
encode_connack_code(_, 'not-authorized') -> 5;
encode_connack_code(_, _) -> 128.

-spec encode_reason_code(char() | reason_code()) -> char().
encode_reason_code('success') -> 0;
encode_reason_code('normal-disconnection') -> 0;
encode_reason_code('granted-qos-0') -> 0;
encode_reason_code('granted-qos-1') -> 1;
encode_reason_code('granted-qos-2') -> 2;
encode_reason_code('disconnect-with-will-message') -> 4;
encode_reason_code('no-matching-subscribers') -> 16;
encode_reason_code('no-subscription-existed') -> 17;
encode_reason_code('continue-authentication') -> 24;
encode_reason_code('re-authenticate') -> 25;
encode_reason_code('unspecified-error') -> 128;
encode_reason_code('malformed-packet') -> 129;
encode_reason_code('protocol-error') -> 130;
encode_reason_code('implementation-specific-error') -> 131;
encode_reason_code('unsupported-protocol-version') -> 132;
encode_reason_code('client-identifier-not-valid') -> 133;
encode_reason_code('bad-user-name-or-password') -> 134;
encode_reason_code('not-authorized') -> 135;
encode_reason_code('server-unavailable') -> 136;
encode_reason_code('server-busy') -> 137;
encode_reason_code('banned') -> 138;
encode_reason_code('server-shutting-down') -> 139;
encode_reason_code('bad-authentication-method') -> 140;
encode_reason_code('keep-alive-timeout') -> 141;
encode_reason_code('session-taken-over') -> 142;
encode_reason_code('topic-filter-invalid') -> 143;
encode_reason_code('topic-name-invalid') -> 144;
encode_reason_code('packet-identifier-in-use') -> 145;
encode_reason_code('packet-identifier-not-found') -> 146;
encode_reason_code('receive-maximum-exceeded') -> 147;
encode_reason_code('topic-alias-invalid') -> 148;
encode_reason_code('packet-too-large') -> 149;
encode_reason_code('message-rate-too-high') -> 150;
encode_reason_code('quota-exceeded') -> 151;
encode_reason_code('administrative-action') -> 152;
encode_reason_code('payload-format-invalid') -> 153;
encode_reason_code('retain-not-supported') -> 154;
encode_reason_code('qos-not-supported') -> 155;
encode_reason_code('use-another-server') -> 156;
encode_reason_code('server-moved') -> 157;
encode_reason_code('shared-subscriptions-not-supported') -> 158;
encode_reason_code('connection-rate-exceeded') -> 159;
encode_reason_code('maximum-connect-time') -> 160;
encode_reason_code('subscription-identifiers-not-supported') -> 161;
encode_reason_code('wildcard-subscriptions-not-supported') -> 162;
encode_reason_code(Code) when is_integer(Code) -> Code.

%%%===================================================================
%%% Formatters
%%%===================================================================
-spec pp(atom(), non_neg_integer()) -> [atom()] | no.
pp(codec_state, 6) -> record_info(fields, codec_state);
pp(connect, 9) -> record_info(fields, connect);
pp(connack, 3) -> record_info(fields, connack);
pp(publish, 8) -> record_info(fields, publish);
pp(puback, 3) -> record_info(fields, puback);
pp(pubrec, 3) -> record_info(fields, pubrec);
pp(pubrel, 4) -> record_info(fields, pubrel);
pp(pubcomp, 3) -> record_info(fields, pubcomp);
pp(subscribe, 4) -> record_info(fields, subscribe);
pp(suback, 3) -> record_info(fields, suback);
pp(unsubscribe, 3) -> record_info(fields, unsubscribe);
pp(unsuback, 1) -> record_info(fields, unsuback);
pp(pingreq, 1) -> record_info(fields, pingreq);
pp(pingresp, 0) -> record_info(fields, pingresp);
pp(disconnect, 2) -> record_info(fields, disconnect);
pp(sub_opts, 4) -> record_info(fields, sub_opts);
pp(_, _) -> no.

-spec format(io:format(), list()) -> string().
format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

format_got_expected(Txt, Got, Expected) ->
    FmtGot = term_format(Got),
    FmtExp = term_format(Expected),
    format("~ts: " ++ FmtGot ++ " (expected: " ++ FmtExp ++ ")",
           [Txt, Got, Expected]).

term_format(I) when is_integer(I) ->
    "~B";
term_format(B) when is_binary(B) ->
    term_format(binary_to_list(B));
term_format(A) when is_atom(A) ->
    term_format(atom_to_list(A));
term_format(T) ->
    case io_lib:printable_latin1_list(T) of
        true -> "~ts";
        false -> "~w"
    end.

%%%===================================================================
%%% Validators
%%%===================================================================
-spec assert(T, any(), any()) -> T.
assert(Got, Got, _) ->
    Got;
assert(Got, Expected, Reason) ->
    err({Reason, Got, Expected}).

-spec qos(qos()) -> qos().
qos(QoS) when is_integer(QoS), QoS>=0, QoS<3 ->
    QoS;
qos(QoS) ->
    err({bad_qos, QoS}).

-spec topic(binary()) -> binary().
topic(Topic) ->
    topic(Topic, #{}).

-spec topic(binary(), properties()) -> binary().
topic(<<>>, Props) ->
    case maps:is_key(topic_alias, Props) of
        true -> <<>>;
        false -> err(bad_topic)
    end;
topic(Bin, _) when is_binary(Bin) ->
    ok = check_topic(Bin),
    ok = check_utf8(Bin),
    Bin;
topic(_, _) ->
    err(bad_topic).

-spec topic_filter(binary()) -> binary().
topic_filter(<<>>) ->
    err(bad_topic_filter);
topic_filter(Bin) when is_binary(Bin) ->
    ok = check_topic_filter(Bin, $/),
    ok = check_utf8(Bin),
    Bin;
topic_filter(_) ->
    err(bad_topic_filter).

-spec utf8(binary()) -> binary().
utf8(Bin) ->
    ok = check_utf8(Bin),
    ok = check_zero(Bin),
    Bin.

-spec check_topic(binary()) -> ok.
check_topic(<<H, _/binary>>) when H == $#; H == $+; H == 0 ->
    err(bad_topic);
check_topic(<<_, T/binary>>) ->
    check_topic(T);
check_topic(<<>>) ->
    ok.

-spec check_topic_filter(binary(), char()) -> ok.
check_topic_filter(<<>>, _) ->
    ok;
check_topic_filter(_, $#) ->
    err(bad_topic_filter);
check_topic_filter(<<$#, _/binary>>, C) when C /= $/ ->
    err(bad_topic_filter);
check_topic_filter(<<$+, _/binary>>, C) when C /= $/ ->
    err(bad_topic_filter);
check_topic_filter(<<C, _/binary>>, $+) when C /= $/ ->
    err(bad_topic_filter);
check_topic_filter(<<0, _/binary>>, _) ->
    err(bad_topic_filter);
check_topic_filter(<<H, T/binary>>, _) ->
    check_topic_filter(T, H).

-spec check_utf8(binary()) -> ok.
check_utf8(Bin) ->
    case unicode:characters_to_binary(Bin, utf8) of
	UTF8Str when is_binary(UTF8Str) ->
	    ok;
	_ ->
	    err(bad_utf8_string)
    end.

-spec check_zero(binary()) -> ok.
check_zero(<<0, _/binary>>) ->
    err(bad_utf8_string);
check_zero(<<_, T/binary>>) ->
    check_zero(T);
check_zero(<<>>) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec dec_bool(non_neg_integer()) -> boolean().
dec_bool(0) -> false;
dec_bool(_) -> true.

-spec enc_bool(boolean()) -> 0..1.
enc_bool(true) -> 1;
enc_bool(false) -> 0.

-spec err(any()) -> no_return().
err(Reason) ->
    erlang:error({?MODULE, Reason}).
