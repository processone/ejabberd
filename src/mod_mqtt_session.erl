%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2022 ProcessOne, SARL. All Rights Reserved.
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
-module(mod_mqtt_session).
-behaviour(p1_server).
-define(VSN, 2).
-vsn(?VSN).

%% API
-export([start/3, start_link/3, accept/1, route/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").
-include("mqtt.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include_lib("public_key/include/public_key.hrl").

-record(state, {vsn = ?VSN            :: integer(),
                version               :: undefined | mqtt_version(),
                socket                :: undefined | socket(),
		peername              :: undefined | peername(),
		timeout = infinity    :: timer(),
		jid                   :: undefined | jid:jid(),
		session_expiry = 0    :: milli_seconds(),
		will                  :: undefined | publish(),
                will_delay = 0        :: milli_seconds(),
		stop_reason           :: undefined | error_reason(),
		acks = #{}            :: acks(),
		subscriptions = #{}   :: subscriptions(),
                topic_aliases = #{}   :: topic_aliases(),
		id = 0                :: non_neg_integer(),
		in_flight             :: undefined | publish() | pubrel(),
		codec                 :: mqtt_codec:state(),
		queue                 :: undefined | p1_queue:queue(publish()),
		tls                   :: boolean(),
		tls_verify            :: boolean()}).

-type acks() :: #{non_neg_integer() => pubrec()}.
-type subscriptions() :: #{binary() => {sub_opts(), non_neg_integer()}}.
-type topic_aliases() :: #{non_neg_integer() => binary()}.

-type error_reason() :: {auth, reason_code()} |
                        {code, reason_code()} |
                        {peer_disconnected, reason_code(), binary()} |
			{socket, socket_error_reason()} |
			{codec, mqtt_codec:error_reason()} |
			{unexpected_packet, atom()} |
			{tls, inet:posix() | atom() | binary()} |
			{replaced, pid()} | {resumed, pid()} |
                        subscribe_forbidden | publish_forbidden |
                        will_topic_forbidden | internal_server_error |
                        session_expired | idle_connection |
			queue_full | shutdown | db_failure |
                        {payload_format_invalid, will | publish} |
                        session_expiry_non_zero | unknown_topic_alias.

-type state() :: #state{}.
-type socket() :: {gen_tcp, inet:socket()} |
		  {fast_tls, fast_tls:tls_socket()} |
		  {mod_mqtt_ws, mod_mqtt_ws:socket()}.
-type peername() :: {inet:ip_address(), inet:port_number()}.
-type seconds() :: non_neg_integer().
-type milli_seconds() :: non_neg_integer().
-type timer() :: infinity | {milli_seconds(), integer()}.
-type socket_error_reason() :: closed | timeout | inet:posix().

-define(CALL_TIMEOUT, timer:minutes(1)).
-define(RELAY_TIMEOUT, timer:minutes(1)).
-define(MAX_UINT32, 4294967295).

%%%===================================================================
%%% API
%%%===================================================================
start(SockMod, Socket, ListenOpts) ->
    p1_server:start(?MODULE, [SockMod, Socket, ListenOpts],
		    ejabberd_config:fsm_limit_opts(ListenOpts)).

start_link(SockMod, Socket, ListenOpts) ->
    p1_server:start_link(?MODULE, [SockMod, Socket, ListenOpts],
			 ejabberd_config:fsm_limit_opts(ListenOpts)).

-spec accept(pid()) -> ok.
accept(Pid) ->
    p1_server:cast(Pid, accept).

-spec route(pid(), term()) -> boolean().
route(Pid, Term) ->
    ejabberd_cluster:send(Pid, Term).

-spec format_error(error_reason()) -> string().
format_error(session_expired) ->
    "Disconnected session is expired";
format_error(idle_connection) ->
    "Idle connection";
format_error(queue_full) ->
    "Message queue is overloaded";
format_error(internal_server_error) ->
    "Internal server error";
format_error(db_failure) ->
    "Database failure";
format_error(shutdown) ->
    "System shutting down";
format_error(subscribe_forbidden) ->
    "Subscribing to this topic is forbidden by service policy";
format_error(publish_forbidden) ->
    "Publishing to this topic is forbidden by service policy";
format_error(will_topic_forbidden) ->
    "Publishing to this will topic is forbidden by service policy";
format_error(session_expiry_non_zero) ->
    "Session Expiry Interval in DISCONNECT packet should have been zero";
format_error(unknown_topic_alias) ->
    "No mapping found for this Topic Alias";
format_error({payload_format_invalid, will}) ->
    "Will payload format doesn't match its indicator";
format_error({payload_format_invalid, publish}) ->
    "PUBLISH payload format doesn't match its indicator";
format_error({peer_disconnected, Code, <<>>}) ->
    format("Peer disconnected with reason: ~ts",
           [mqtt_codec:format_reason_code(Code)]);
format_error({peer_disconnected, Code, Reason}) ->
    format("Peer disconnected with reason: ~ts (~ts)", [Reason, Code]);
format_error({replaced, Pid}) ->
    format("Replaced by ~p at ~ts", [Pid, node(Pid)]);
format_error({resumed, Pid}) ->
    format("Resumed by ~p at ~ts", [Pid, node(Pid)]);
format_error({unexpected_packet, Name}) ->
    format("Unexpected ~ts packet", [string:to_upper(atom_to_list(Name))]);
format_error({tls, Reason}) ->
    format("TLS failed: ~ts", [format_tls_error(Reason)]);
format_error({socket, A}) ->
    format("Connection failed: ~ts", [format_inet_error(A)]);
format_error({code, Code}) ->
    format("Protocol error: ~ts", [mqtt_codec:format_reason_code(Code)]);
format_error({auth, Code}) ->
    format("Authentication failed: ~ts", [mqtt_codec:format_reason_code(Code)]);
format_error({codec, CodecError}) ->
    format("Protocol error: ~ts", [mqtt_codec:format_error(CodecError)]);
format_error(A) when is_atom(A) ->
    atom_to_list(A);
format_error(Reason) ->
    format("Unrecognized error: ~w", [Reason]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([SockMod, Socket, ListenOpts]) ->
    MaxSize = proplists:get_value(max_payload_size, ListenOpts, infinity),
    State1 = #state{socket = {SockMod, Socket},
		    id = p1_rand:uniform(65535),
		    tls = proplists:get_bool(tls, ListenOpts),
		    tls_verify = proplists:get_bool(tls_verify, ListenOpts),
		    codec = mqtt_codec:new(MaxSize)},
    Timeout = timer:seconds(30),
    State2 = set_timeout(State1, Timeout),
    {ok, State2, Timeout}.

handle_call({get_state, _}, From, #state{stop_reason = {resumed, Pid}} = State) ->
    p1_server:reply(From, {error, {resumed, Pid}}),
    noreply(State);
handle_call({get_state, Pid}, From, State) ->
    case stop(State, {resumed, Pid}) of
        {stop, Status, State1} ->
            {stop, Status, State1#state{stop_reason = {replaced, Pid}}};
        {noreply, State1, _} ->
            ?DEBUG("Transferring MQTT session state to ~p at ~ts", [Pid, node(Pid)]),
            Q1 = p1_queue:file_to_ram(State1#state.queue),
            p1_server:reply(From, {ok, State1#state{queue = Q1}}),
            SessionExpiry = State1#state.session_expiry,
            State2 = set_timeout(State1, min(SessionExpiry, ?RELAY_TIMEOUT)),
            State3 = State2#state{queue = undefined,
                                  stop_reason = {resumed, Pid},
                                  acks = #{},
                                  will = undefined,
                                  session_expiry = 0,
                                  topic_aliases = #{},
                                  subscriptions = #{}},
            noreply(State3)
    end;
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    noreply(State).

handle_cast(accept, #state{socket = {_, Sock}} = State) ->
    case peername(State) of
	{ok, IPPort} ->
	    State1 = State#state{peername = IPPort},
	    case starttls(State) of
		{ok, Socket1} ->
		    State2 = State1#state{socket = Socket1},
		    handle_info({tcp, Sock, <<>>}, State2);
		{error, Why} ->
		    stop(State1, Why)
	    end;
	{error, Why} ->
	    stop(State, {socket, Why})
    end;
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    noreply(State).

handle_info(Msg, #state{stop_reason = {resumed, Pid} = Reason} = State) ->
    case Msg of
	{#publish{}, _} ->
	    ?DEBUG("Relaying delayed publish to ~p at ~ts", [Pid, node(Pid)]),
	    ejabberd_cluster:send(Pid, Msg),
	    noreply(State);
	timeout ->
	    stop(State, Reason);
	_ ->
	    noreply(State)
    end;
handle_info({#publish{meta = Meta} = Pkt, ExpiryTime}, State) ->
    ID = next_id(State#state.id),
    Meta1 = Meta#{expiry_time => ExpiryTime},
    Pkt1 = Pkt#publish{id = ID, meta = Meta1},
    State1 = State#state{id = ID},
    case send(State1, Pkt1) of
	{ok, State2} -> noreply(State2);
	{error, State2, Reason} -> stop(State2, Reason)
    end;
handle_info({tcp, TCPSock, TCPData},
	    #state{codec = Codec, socket = Socket} = State) ->
    case recv_data(Socket, TCPData) of
	{ok, Data} ->
	    case mqtt_codec:decode(Codec, Data) of
		{ok, Pkt, Codec1} ->
		    ?DEBUG("Got MQTT packet:~n~ts", [pp(Pkt)]),
		    State1 = State#state{codec = Codec1},
		    case handle_packet(Pkt, State1) of
			{ok, State2} ->
			    handle_info({tcp, TCPSock, <<>>}, State2);
			{error, State2, Reason} ->
			    stop(State2, Reason)
		    end;
		{more, Codec1} ->
		    State1 = State#state{codec = Codec1},
		    State2 = reset_keep_alive(State1),
		    activate(Socket),
		    noreply(State2);
		{error, Why} ->
		    stop(State, {codec, Why})
	    end;
	{error, Why} ->
	    stop(State, Why)
    end;
handle_info({tcp_closed, _Sock}, State) ->
    ?DEBUG("MQTT connection reset by peer", []),
    stop(State, {socket, closed});
handle_info({tcp_error, _Sock, Reason}, State) ->
    ?DEBUG("MQTT connection error: ~ts", [format_inet_error(Reason)]),
    stop(State, {socket, Reason});
handle_info(timeout, #state{socket = Socket} = State) ->
    case Socket of
	undefined ->
	    ?DEBUG("MQTT session expired", []),
	    stop(State#state{session_expiry = 0}, session_expired);
	_ ->
	    ?DEBUG("MQTT connection timed out", []),
	    stop(State, idle_connection)
    end;
handle_info({replaced, Pid}, State) ->
    stop(State#state{session_expiry = 0}, {replaced, Pid});
handle_info({timeout, _TRef, publish_will}, State) ->
    noreply(publish_will(State));
handle_info({Ref, badarg}, State) when is_reference(Ref) ->
    %% TODO: figure out from where this messages comes from
    noreply(State);
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    noreply(State).

-spec handle_packet(mqtt_packet(), state()) -> {ok, state()} |
					       {error, state(), error_reason()}.
handle_packet(#connect{proto_level = Version} = Pkt, State) ->
    handle_connect(Pkt, State#state{version = Version});
handle_packet(#publish{} = Pkt, State) ->
    handle_publish(Pkt, State);
handle_packet(#puback{id = ID}, #state{in_flight = #publish{qos = 1, id = ID}} = State) ->
    resend(State#state{in_flight = undefined});
handle_packet(#puback{id = ID, code = Code}, State) ->
    ?DEBUG("Ignoring unexpected PUBACK with id=~B and code '~ts'", [ID, Code]),
    {ok, State};
handle_packet(#pubrec{id = ID, code = Code},
              #state{in_flight = #publish{qos = 2, id = ID}} = State) ->
    case mqtt_codec:is_error_code(Code) of
        true ->
            ?DEBUG("Got PUBREC with error code '~ts', "
                   "aborting acknowledgement", [Code]),
            resend(State#state{in_flight = undefined});
        false ->
            Pubrel = #pubrel{id = ID},
            send(State#state{in_flight = Pubrel}, Pubrel)
    end;
handle_packet(#pubrec{id = ID, code = Code}, State) ->
    case mqtt_codec:is_error_code(Code) of
        true ->
            ?DEBUG("Ignoring unexpected PUBREC with id=~B and code '~ts'",
                   [ID, Code]),
            {ok, State};
        false ->
            Code1 = 'packet-identifier-not-found',
            ?DEBUG("Unexpected PUBREC with id=~B, "
                   "sending PUBREL with error code '~ts'", [ID, Code1]),
            send(State, #pubrel{id = ID, code = Code1})
    end;
handle_packet(#pubcomp{id = ID}, #state{in_flight = #pubrel{id = ID}} = State) ->
    resend(State#state{in_flight = undefined});
handle_packet(#pubcomp{id = ID}, State) ->
    ?DEBUG("Ignoring unexpected PUBCOMP with id=~B: most likely "
           "it's a repeated response to duplicated PUBREL", [ID]),
    {ok, State};
handle_packet(#pubrel{id = ID}, State) ->
    case maps:take(ID, State#state.acks) of
        {_, Acks} ->
            send(State#state{acks = Acks}, #pubcomp{id = ID});
        error ->
            Code = 'packet-identifier-not-found',
            ?DEBUG("Unexpected PUBREL with id=~B, "
                   "sending PUBCOMP with error code '~ts'", [ID, Code]),
            Pubcomp = #pubcomp{id = ID, code = Code},
            send(State, Pubcomp)
    end;
handle_packet(#subscribe{} = Pkt, State) ->
    handle_subscribe(Pkt, State);
handle_packet(#unsubscribe{} = Pkt, State) ->
    handle_unsubscribe(Pkt, State);
handle_packet(#pingreq{}, State) ->
    send(State, #pingresp{});
handle_packet(#disconnect{properties = #{session_expiry_interval := SE}},
              #state{session_expiry = 0} = State) when SE>0 ->
    %% Protocol violation
    {error, State, session_expiry_non_zero};
handle_packet(#disconnect{code = Code, properties = Props},
              #state{jid = #jid{lserver = Server}} = State) ->
    Reason = maps:get(reason_string, Props, <<>>),
    Expiry = case maps:get(session_expiry_interval, Props, undefined) of
                 undefined -> State#state.session_expiry;
                 SE -> min(timer:seconds(SE), session_expiry(Server))
             end,
    State1 = State#state{session_expiry = Expiry},
    State2 = case Code of
                 'normal-disconnection' -> State1#state{will = undefined};
                 _ -> State1
             end,
    {error, State2, {peer_disconnected, Code, Reason}};
handle_packet(Pkt, State) ->
    ?WARNING_MSG("Unexpected packet:~n~ts~n** when state:~n~ts",
		 [pp(Pkt), pp(State)]),
    {error, State, {unexpected_packet, element(1, Pkt)}}.

terminate(_, #state{peername = undefined}) ->
    ok;
terminate(Reason, State) ->
    Reason1 = case Reason of
		  shutdown -> shutdown;
                  {shutdown, _} -> shutdown;
		  normal -> State#state.stop_reason;
                  {process_limit, _} -> queue_full;
		  _ -> internal_server_error
	      end,
    case State#state.jid of
	#jid{} -> unregister_session(State, Reason1);
	undefined -> log_disconnection(State, Reason1)
    end,
    State1 = disconnect(State, Reason1),
    publish_will(State1).

code_change(_OldVsn, State, _Extra) ->
    {ok, upgrade_state(State)}.

%%%===================================================================
%%% State transitions
%%%===================================================================
-spec noreply(state()) -> {noreply, state(), non_neg_integer() | infinity}.
noreply(#state{timeout = infinity} = State) ->
    {noreply, State, infinity};
noreply(#state{timeout = {MSecs, StartTime}} = State) ->
    CurrentTime = current_time(),
    Timeout = max(0, MSecs - CurrentTime + StartTime),
    {noreply, State, Timeout}.

-spec stop(state(), error_reason()) -> {noreply, state(), infinity} |
				       {stop, normal, state()}.
stop(#state{session_expiry = 0} = State, Reason) ->
    {stop, normal, State#state{stop_reason = Reason}};
stop(#state{session_expiry = SessExp} = State, Reason) ->
    case State#state.socket of
        undefined ->
            noreply(State);
        _ ->
            WillDelay = State#state.will_delay,
            log_disconnection(State, Reason),
	    State1 = disconnect(State, Reason),
            State2 = if WillDelay == 0 ->
                             publish_will(State1);
                        WillDelay < SessExp ->
                             erlang:start_timer(WillDelay, self(), publish_will),
                             State1;
                        true ->
                             State1
                     end,
	    State3 = set_timeout(State2, SessExp),
	    State4 = State3#state{stop_reason = Reason},
	    noreply(State4)
    end.

%% Here is the code upgrading state between different
%% code versions. This is needed when doing session resumption from
%% remote node running the version of the code with incompatible #state{}
%% record fields. Also used by code_change/3 callback.
-spec upgrade_state(tuple()) -> state().
upgrade_state(State) ->
    case element(2, State) of
	?VSN ->
	    State;
	VSN when VSN > ?VSN ->
	    erlang:error({downgrade_not_supported, State});
	VSN ->
	    State1 = upgrade_state(State, VSN),
	    upgrade_state(setelement(2, State1, VSN+1))
    end.

-spec upgrade_state(tuple(), 1..?VSN) -> tuple().
upgrade_state(OldState, 1) ->
    %% Appending 'tls' field
    erlang:append_element(OldState, false);
upgrade_state(State, _VSN) ->
    State.

%%%===================================================================
%%% Session management
%%%===================================================================
-spec open_session(state(), jid(), boolean()) -> {ok, boolean(), state()} |
						 {error, state(), error_reason()}.
open_session(State, JID, _CleanStart = false) ->
    USR = {_, S, _} = jid:tolower(JID),
    case mod_mqtt:lookup_session(USR) of
	{ok, Pid} ->
	    try p1_server:call(Pid, {get_state, self()}, ?CALL_TIMEOUT) of
		{ok, State1} ->
                    State2 = upgrade_state(State1),
		    Q1 = case queue_type(S) of
			     ram -> State2#state.queue;
			     _ -> p1_queue:ram_to_file(State2#state.queue)
			 end,
		    Q2 = p1_queue:set_limit(Q1, queue_limit(S)),
		    State3 = State#state{queue = Q2,
					 acks = State2#state.acks,
					 subscriptions = State2#state.subscriptions,
					 id = State2#state.id,
					 in_flight = State2#state.in_flight},
		    ?DEBUG("Resumed state from ~p at ~ts:~n~ts",
			   [Pid, node(Pid), pp(State3)]),
		    register_session(State3, JID, Pid);
		{error, Why} ->
		    {error, State, Why}
	    catch exit:{Why, {p1_server, _, _}} ->
		    ?WARNING_MSG("Failed to copy session state from ~p at ~ts: ~ts",
				 [Pid, node(Pid), format_exit_reason(Why)]),
		    register_session(State, JID, undefined)
	    end;
	{error, notfound} ->
	    register_session(State, JID, undefined);
	{error, Why} ->
	    {error, State, Why}
    end;
open_session(State, JID, _CleanStart = true) ->
    register_session(State, JID, undefined).

-spec register_session(state(), jid(), undefined | pid()) ->
                   {ok, boolean(), state()} | {error, state(), error_reason()}.
register_session(#state{peername = IP} = State, JID, Parent) ->
    USR = {_, S, _} = jid:tolower(JID),
    case mod_mqtt:open_session(USR) of
	ok ->
	    case resubscribe(USR, State#state.subscriptions) of
		ok ->
		    ?INFO_MSG("~ts for ~ts from ~ts",
			      [if is_pid(Parent) ->
				       io_lib:format(
					 "Reopened MQTT session via ~p",
					 [Parent]);
				  true ->
				       "Opened MQTT session"
			       end,
			       jid:encode(JID),
			       ejabberd_config:may_hide_data(
				 misc:ip_to_list(IP))]),
		    Q = case State#state.queue of
			    undefined ->
				p1_queue:new(queue_type(S), queue_limit(S));
			    Q1 ->
				Q1
			end,
                    {ok, is_pid(Parent), State#state{jid = JID, queue = Q}};
		{error, Why} ->
                    mod_mqtt:close_session(USR),
		    {error, State#state{session_expiry = 0}, Why}
	    end;
	{error, Reason} ->
	    ?ERROR_MSG("Failed to register MQTT session for ~ts from ~ts: ~ts",
		       err_args(JID, IP, Reason)),
	    {error, State, Reason}
    end.

-spec unregister_session(state(), error_reason()) -> ok.
unregister_session(#state{jid = #jid{} = JID, peername = IP} = State, Reason) ->
    Msg = "Closing MQTT session for ~ts from ~ts: ~ts",
    case Reason of
        {Tag, _} when Tag == replaced; Tag == resumed ->
            ?DEBUG(Msg, err_args(JID, IP, Reason));
        {socket, _} ->
            ?INFO_MSG(Msg, err_args(JID, IP, Reason));
        Tag when Tag == idle_connection; Tag == session_expired; Tag == shutdown ->
            ?INFO_MSG(Msg, err_args(JID, IP, Reason));
        {peer_disconnected, Code, _} ->
            case mqtt_codec:is_error_code(Code) of
                true -> ?WARNING_MSG(Msg, err_args(JID, IP, Reason));
                false -> ?INFO_MSG(Msg, err_args(JID, IP, Reason))
            end;
        _ ->
            ?WARNING_MSG(Msg, err_args(JID, IP, Reason))
    end,
    USR = jid:tolower(JID),
    unsubscribe(maps:keys(State#state.subscriptions), USR, #{}),
    case mod_mqtt:close_session(USR) of
	ok -> ok;
	{error, Why} ->
            ?ERROR_MSG(
               "Failed to close MQTT session for ~ts from ~ts: ~ts",
               err_args(JID, IP, Why))
    end;
unregister_session(_, _) ->
    ok.

%%%===================================================================
%%% CONNECT/PUBLISH/SUBSCRIBE/UNSUBSCRIBE handlers
%%%===================================================================
-spec handle_connect(connect(), state()) -> {ok, state()} |
					    {error, state(), error_reason()}.
handle_connect(#connect{clean_start = CleanStart} = Pkt,
	       #state{jid = undefined, peername = IP} = State) ->
    case authenticate(Pkt, IP, State) of
        {ok, JID} ->
            case validate_will(Pkt, JID) of
                ok ->
                    case open_session(State, JID, CleanStart) of
                        {ok, SessionPresent, State1} ->
                            State2 = set_session_properties(State1, Pkt),
                            ConnackProps = get_connack_properties(State2, Pkt),
                            Connack = #connack{session_present = SessionPresent,
                                               properties = ConnackProps},
                            case send(State2, Connack) of
                                {ok, State3} -> resend(State3);
                                {error, _, _} = Err -> Err
                            end;
                        {error, _, _} = Err ->
                            Err
                    end;
                {error, Reason} ->
                    {error, State, Reason}
            end;
        {error, Code} ->
            {error, State, {auth, Code}}
    end.

-spec handle_publish(publish(), state()) -> {ok, state()} |
					    {error, state(), error_reason()}.
handle_publish(#publish{qos = QoS, id = ID} = Publish, State) ->
    case QoS == 2 andalso maps:is_key(ID, State#state.acks) of
	true ->
	    send(State, maps:get(ID, State#state.acks));
	false ->
            case validate_publish(Publish, State) of
                ok ->
                    State1 = store_topic_alias(State, Publish),
                    Ret = publish(State1, Publish),
                    {Code, Props} = get_publish_code_props(Ret),
                    case Ret of
                        {ok, _} when QoS == 2 ->
                            Pkt = #pubrec{id = ID, code = Code,
                                          properties = Props},
                            Acks = maps:put(ID, Pkt, State1#state.acks),
                            State2 = State1#state{acks = Acks},
                            send(State2, Pkt);
                        {error, _} when QoS == 2 ->
                            Pkt = #pubrec{id = ID, code = Code,
                                          properties = Props},
                            send(State1, Pkt);
                        _ when QoS == 1 ->
                            Pkt = #puback{id = ID, code = Code,
                                          properties = Props},
                            send(State1, Pkt);
                        _ ->
                            {ok, State1}
                    end;
                {error, Why} ->
                    {error, State, Why}
            end
    end.

-spec handle_subscribe(subscribe(), state()) ->
		{ok, state()} | {error, state(), error_reason()}.
handle_subscribe(#subscribe{id = ID, filters = TopicFilters} = Pkt, State) ->
    case validate_subscribe(Pkt) of
        ok ->
            USR = jid:tolower(State#state.jid),
            SubID = maps:get(subscription_identifier, Pkt#subscribe.properties, 0),
            OldSubs = State#state.subscriptions,
            {Codes, NewSubs, Props} = subscribe(TopicFilters, USR, SubID),
            Subs = maps:merge(OldSubs, NewSubs),
            State1 = State#state{subscriptions = Subs},
            Suback = #suback{id = ID, codes = Codes, properties = Props},
            case send(State1, Suback) of
                {ok, State2} ->
                    Pubs = select_retained(USR, NewSubs, OldSubs),
                    send_retained(State2, Pubs);
                {error, _, _} = Err ->
                    Err
            end;
        {error, Why} ->
            {error, State, Why}
    end.

-spec handle_unsubscribe(unsubscribe(), state()) ->
			 {ok, state()} | {error, state(), error_reason()}.
handle_unsubscribe(#unsubscribe{id = ID, filters = TopicFilters}, State) ->
    USR = jid:tolower(State#state.jid),
    {Codes, Subs, Props} = unsubscribe(TopicFilters, USR, State#state.subscriptions),
    State1 = State#state{subscriptions = Subs},
    Unsuback = #unsuback{id = ID, codes = Codes, properties = Props},
    send(State1, Unsuback).

%%%===================================================================
%%% Aux functions for CONNECT/PUBLISH/SUBSCRIBE/UNSUBSCRIBE handlers
%%%===================================================================
-spec set_session_properties(state(), connect()) -> state().
set_session_properties(#state{version = Version,
                              jid = #jid{lserver = Server}} = State,
                       #connect{clean_start = CleanStart,
                                keep_alive = KeepAlive,
                                properties = Props} = Pkt) ->
    SEMin = case CleanStart of
                false when Version == ?MQTT_VERSION_4 -> infinity;
                _ -> timer:seconds(maps:get(session_expiry_interval, Props, 0))
            end,
    SEConfig = session_expiry(Server),
    State1 = State#state{session_expiry = min(SEMin, SEConfig)},
    State2 = set_will_properties(State1, Pkt),
    set_keep_alive(State2, KeepAlive).

-spec set_will_properties(state(), connect()) -> state().
set_will_properties(State, #connect{will = #publish{} = Will,
                                    will_properties = Props}) ->
    {WillDelay, Props1} = case maps:take(will_delay_interval, Props) of
                              error -> {0, Props};
                              Ret -> Ret
                          end,
    State#state{will = Will#publish{properties = Props1},
                will_delay = timer:seconds(WillDelay)};
set_will_properties(State, _) ->
    State.

-spec get_connack_properties(state(), connect()) -> properties().
get_connack_properties(#state{session_expiry = SessExp, jid = JID},
                       #connect{client_id = ClientID,
                                keep_alive = KeepAlive,
				properties = Props}) ->
    Props1 = case ClientID of
                 <<>> -> #{assigned_client_identifier => JID#jid.lresource};
                 _ -> #{}
             end,
    Props2 = case maps:find(authentication_method, Props) of
		 {ok, Method} -> Props1#{authentication_method => Method};
		 error -> Props1
	     end,
    Props2#{session_expiry_interval => SessExp div 1000,
            shared_subscription_available => false,
            topic_alias_maximum => topic_alias_maximum(JID#jid.lserver),
            server_keep_alive => KeepAlive}.

-spec subscribe([{binary(), sub_opts()}], jid:ljid(), non_neg_integer()) ->
                       {[reason_code()], subscriptions(), properties()}.
subscribe(TopicFilters, USR, SubID) ->
    subscribe(TopicFilters, USR, SubID, [], #{}, ok).

-spec subscribe([{binary(), sub_opts()}], jid:ljid(), non_neg_integer(),
                [reason_code()], subscriptions(), ok | {error, error_reason()}) ->
                       {[reason_code()], subscriptions(), properties()}.
subscribe([{TopicFilter, SubOpts}|TopicFilters], USR, SubID, Codes, Subs, Err) ->
    case mod_mqtt:subscribe(USR, TopicFilter, SubOpts, SubID) of
        ok ->
            Code = subscribe_reason_code(SubOpts#sub_opts.qos),
            subscribe(TopicFilters, USR, SubID, [Code|Codes],
                      maps:put(TopicFilter, {SubOpts, SubID}, Subs), Err);
        {error, Why} = Err1 ->
            Code = subscribe_reason_code(Why),
            subscribe(TopicFilters, USR, SubID, [Code|Codes], Subs, Err1)
    end;
subscribe([], _USR, _SubID, Codes, Subs, Err) ->
    Props = case Err of
                ok -> #{};
                {error, Why} ->
                    #{reason_string => format_reason_string(Why)}
            end,
    {lists:reverse(Codes), Subs, Props}.

-spec unsubscribe([binary()], jid:ljid(), subscriptions()) ->
                         {[reason_code()], subscriptions(), properties()}.
unsubscribe(TopicFilters, USR, Subs) ->
    unsubscribe(TopicFilters, USR, [], Subs, ok).

-spec unsubscribe([binary()], jid:ljid(),
                  [reason_code()], subscriptions(),
                  ok | {error, error_reason()}) ->
                         {[reason_code()], subscriptions(), properties()}.
unsubscribe([TopicFilter|TopicFilters], USR, Codes, Subs, Err) ->
    case mod_mqtt:unsubscribe(USR, TopicFilter) of
        ok ->
            unsubscribe(TopicFilters, USR, [success|Codes],
                        maps:remove(TopicFilter, Subs), Err);
        {error, notfound} ->
            unsubscribe(TopicFilters, USR,
                        ['no-subscription-existed'|Codes],
                        maps:remove(TopicFilter, Subs), Err);
        {error, Why} = Err1 ->
            Code = unsubscribe_reason_code(Why),
            unsubscribe(TopicFilters, USR, [Code|Codes], Subs, Err1)
    end;
unsubscribe([], _USR, Codes, Subs, Err) ->
    Props = case Err of
                ok -> #{};
                {error, Why} ->
                    #{reason_string => format_reason_string(Why)}
            end,
    {lists:reverse(Codes), Subs, Props}.

-spec select_retained(jid:ljid(), subscriptions(), subscriptions()) -> [{publish(), seconds()}].
select_retained(USR, NewSubs, OldSubs) ->
    lists:flatten(
      maps:fold(
        fun(_Filter, {#sub_opts{retain_handling = 2}, _SubID}, Acc) ->
                Acc;
           (Filter, {#sub_opts{retain_handling = 1, qos = QoS}, SubID}, Acc) ->
                case maps:is_key(Filter, OldSubs) of
                    true -> Acc;
                    false -> [mod_mqtt:select_retained(USR, Filter, QoS, SubID)|Acc]
                end;
           (Filter, {#sub_opts{qos = QoS}, SubID}, Acc) ->
                [mod_mqtt:select_retained(USR, Filter, QoS, SubID)|Acc]
        end, [], NewSubs)).

-spec send_retained(state(), [{publish(), seconds()}]) ->
                    {ok, state()} | {error, state(), error_reason()}.
send_retained(State, [{#publish{meta = Meta} = Pub, Expiry}|Pubs]) ->
    I = next_id(State#state.id),
    Meta1 = Meta#{expiry_time => Expiry},
    Pub1 = Pub#publish{id = I, retain = true, meta = Meta1},
    case send(State#state{id = I}, Pub1) of
        {ok, State1} ->
            send_retained(State1, Pubs);
        Err ->
            Err
    end;
send_retained(State, []) ->
    {ok, State}.

-spec publish(state(), publish()) -> {ok, non_neg_integer()} |
                                     {error, error_reason()}.
publish(State, #publish{topic = Topic, properties = Props} = Pkt) ->
    MessageExpiry = maps:get(message_expiry_interval, Props, ?MAX_UINT32),
    ExpiryTime = min(unix_time() + MessageExpiry, ?MAX_UINT32),
    USR = jid:tolower(State#state.jid),
    Props1 = maps:filter(
               fun(payload_format_indicator, _) -> true;
                  (content_type, _) -> true;
                  (response_topic, _) -> true;
                  (correlation_data, _) -> true;
                  (user_property, _) -> true;
                  (_, _) -> false
               end, Props),
    Topic1 = case Topic of
                 <<>> ->
                     Alias = maps:get(topic_alias, Props),
                     maps:get(Alias, State#state.topic_aliases);
                 _ ->
                     Topic
             end,
    Pkt1 = Pkt#publish{topic = Topic1, properties = Props1},
    mod_mqtt:publish(USR, Pkt1, ExpiryTime).

-spec store_topic_alias(state(), publish()) -> state().
store_topic_alias(State, #publish{topic = <<_, _/binary>> = Topic,
                                  properties = #{topic_alias := Alias}}) ->
    Aliases = maps:put(Alias, Topic, State#state.topic_aliases),
    State#state{topic_aliases = Aliases};
store_topic_alias(State, _) ->
    State.

%%%===================================================================
%%% Socket management
%%%===================================================================
-spec send(state(), mqtt_packet()) -> {ok, state()} |
				      {error, state(), error_reason()}.
send(State, #publish{} = Pkt) ->
    case is_expired(Pkt) of
        {false, Pkt1} ->
            case State#state.in_flight == undefined andalso
                 p1_queue:is_empty(State#state.queue) of
                true ->
                    Dup = case Pkt1#publish.qos of
                              0 -> undefined;
                              _ -> Pkt1
                          end,
                    State1 = State#state{in_flight = Dup},
                    {ok, do_send(State1, Pkt1)};
                false ->
                    ?DEBUG("Queueing packet:~n~ts~n** when state:~n~ts",
                           [pp(Pkt), pp(State)]),
                    try p1_queue:in(Pkt, State#state.queue) of
                        Q ->
                            State1 = State#state{queue = Q},
                            {ok, State1}
                    catch error:full ->
                            Q = p1_queue:clear(State#state.queue),
                            State1 = State#state{queue = Q, session_expiry = 0},
                            {error, State1, queue_full}
                    end
            end;
        true ->
            {ok, State}
    end;
send(State, Pkt) ->
    {ok, do_send(State, Pkt)}.

-spec resend(state()) -> {ok, state()} | {error, state(), error_reason()}.
resend(#state{in_flight = undefined} = State) ->
    case p1_queue:out(State#state.queue) of
        {{value, #publish{qos = QoS} = Pkt}, Q} ->
            case is_expired(Pkt) of
                true ->
                    resend(State#state{queue = Q});
                {false, Pkt1} when QoS > 0 ->
                    State1 = State#state{in_flight = Pkt1, queue = Q},
                    {ok, do_send(State1, Pkt1)};
                {false, Pkt1} ->
                    State1 = do_send(State#state{queue = Q}, Pkt1),
                    resend(State1)
            end;
	{empty, _} ->
	    {ok, State}
    end;
resend(#state{in_flight = Pkt} = State) ->
    {ok, do_send(State, set_dup_flag(Pkt))}.

-spec do_send(state(), mqtt_packet()) -> state().
do_send(#state{socket = {SockMod, Sock} = Socket} = State, Pkt) ->
    ?DEBUG("Send MQTT packet:~n~ts", [pp(Pkt)]),
    Data = mqtt_codec:encode(State#state.version, Pkt),
    Res = SockMod:send(Sock, Data),
    check_sock_result(Socket, Res),
    State;
do_send(State, _Pkt) ->
    State.

-spec activate(socket()) -> ok.
activate({SockMod, Sock} = Socket) ->
    Res = case SockMod of
	      gen_tcp -> inet:setopts(Sock, [{active, once}]);
	      _ -> SockMod:setopts(Sock, [{active, once}])
	  end,
    check_sock_result(Socket, Res).

-spec peername(state()) -> {ok, peername()} | {error, socket_error_reason()}.
peername(#state{socket = {SockMod, Sock}}) ->
    case SockMod of
	gen_tcp -> inet:peername(Sock);
	_ -> SockMod:peername(Sock)
    end.

-spec disconnect(state(), error_reason()) -> state().
disconnect(#state{socket = {SockMod, Sock}} = State, Err) ->
    State1 = case Err of
                 {auth, Code} ->
                     do_send(State, #connack{code = Code});
                 {codec, {Tag, _, _}} when Tag == unsupported_protocol_version;
                                           Tag == unsupported_protocol_name ->
                     do_send(State#state{version = ?MQTT_VERSION_4},
                             #connack{code = connack_reason_code(Err)});
                 _ when State#state.version == undefined ->
                     State;
                 {Tag, _} when Tag == socket; Tag == tls ->
                     State;
                 {peer_disconnected, _, _} ->
                     State;
                 _ ->
                     Props = #{reason_string => format_reason_string(Err)},
                     case State#state.jid of
                         undefined ->
                             Code = connack_reason_code(Err),
                             Pkt = #connack{code = Code, properties = Props},
                             do_send(State, Pkt);
                         _ when State#state.version == ?MQTT_VERSION_5 ->
                             Code = disconnect_reason_code(Err),
                             Pkt = #disconnect{code = Code, properties = Props},
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

-spec check_sock_result(socket(), ok | {error, inet:posix()}) -> ok.
check_sock_result(_, ok) ->
    ok;
check_sock_result({_, Sock}, {error, Why}) ->
    self() ! {tcp_closed, Sock},
    ?DEBUG("MQTT socket error: ~p", [format_inet_error(Why)]).

-spec starttls(state()) -> {ok, socket()} | {error, error_reason()}.
starttls(#state{socket = {gen_tcp, Socket}, tls = true}) ->
    case ejabberd_pkix:get_certfile() of
	{ok, Cert} ->
            CAFileOpt =
                case ejabberd_option:c2s_cafile(ejabberd_config:get_myname()) of
                    undefined -> [];
                    CAFile -> [{cafile, CAFile}]
                end,
	    case fast_tls:tcp_to_tls(Socket, [{certfile, Cert}] ++ CAFileOpt) of
		{ok, TLSSock} ->
		    {ok, {fast_tls, TLSSock}};
		{error, Why} ->
		    {error, {tls, Why}}
	    end;
	error ->
	    {error, {tls, no_certfile}}
    end;
starttls(#state{socket = Socket}) ->
    {ok, Socket}.

-spec recv_data(socket(), binary()) -> {ok, binary()} | {error, error_reason()}.
recv_data({fast_tls, Sock}, Data) ->
    case fast_tls:recv_data(Sock, Data) of
	{ok, _} = OK -> OK;
	{error, E} when is_atom(E) -> {error, {socket, E}};
	{error, E} when is_binary(E) -> {error, {tls, E}}
    end;
recv_data(_, Data) ->
    {ok, Data}.

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

-spec format_tls_error(atom() | binary()) -> string() | binary().
format_tls_error(no_certfile) ->
    "certificate not configured";
format_tls_error(Reason) when is_atom(Reason) ->
    format_inet_error(Reason);
format_tls_error(Reason) ->
    Reason.

-spec format_exit_reason(term()) -> string().
format_exit_reason(noproc) ->
    "process is dead";
format_exit_reason(normal) ->
    "process has exited";
format_exit_reason(killed) ->
    "process has been killed";
format_exit_reason(timeout) ->
    "remote call to process timed out";
format_exit_reason(Why) ->
    format("unexpected error: ~p", [Why]).

%% Same as format_error/1, but hides sensitive data
%% and returns result as binary
-spec format_reason_string(error_reason()) -> binary().
format_reason_string({resumed, _}) ->
    <<"Resumed by another connection">>;
format_reason_string({replaced, _}) ->
    <<"Replaced by another connection">>;
format_reason_string(Err) ->
    list_to_binary(format_error(Err)).

-spec format(io:format(), list()) -> string().
format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

-spec pp(atom(), non_neg_integer()) -> [atom()] | no.
pp(state, 17) -> record_info(fields, state);
pp(Rec, Size) -> mqtt_codec:pp(Rec, Size).

-spec publish_reason_code(error_reason()) -> reason_code().
publish_reason_code(publish_forbidden) -> 'topic-name-invalid';
publish_reason_code(_) -> 'implementation-specific-error'.

-spec subscribe_reason_code(qos() | error_reason()) -> reason_code().
subscribe_reason_code(0) -> 'granted-qos-0';
subscribe_reason_code(1) -> 'granted-qos-1';
subscribe_reason_code(2) -> 'granted-qos-2';
subscribe_reason_code(subscribe_forbidden) -> 'topic-filter-invalid';
subscribe_reason_code(_) -> 'implementation-specific-error'.

-spec unsubscribe_reason_code(error_reason()) -> reason_code().
unsubscribe_reason_code(_) -> 'implementation-specific-error'.

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

-spec connack_reason_code(error_reason()) -> reason_code().
connack_reason_code({Tag, Code}) when Tag == auth; Tag == code -> Code;
connack_reason_code({codec, Err}) -> mqtt_codec:error_reason_code(Err);
connack_reason_code({unexpected_packet, _}) -> 'protocol-error';
connack_reason_code(internal_server_error) -> 'implementation-specific-error';
connack_reason_code(db_failure) -> 'implementation-specific-error';
connack_reason_code(idle_connection) -> 'keep-alive-timeout';
connack_reason_code(queue_full) -> 'quota-exceeded';
connack_reason_code(shutdown) -> 'server-shutting-down';
connack_reason_code(will_topic_forbidden) -> 'topic-name-invalid';
connack_reason_code({payload_format_invalid, _}) -> 'payload-format-invalid';
connack_reason_code(session_expiry_non_zero) -> 'protocol-error';
connack_reason_code(_) -> 'unspecified-error'.

%%%===================================================================
%%% Configuration processing
%%%===================================================================
-spec queue_type(binary()) -> ram | file.
queue_type(Host) ->
    mod_mqtt_opt:queue_type(Host).

-spec queue_limit(binary()) -> non_neg_integer() | unlimited.
queue_limit(Host) ->
    mod_mqtt_opt:max_queue(Host).

-spec session_expiry(binary()) -> milli_seconds().
session_expiry(Host) ->
    mod_mqtt_opt:session_expiry(Host).

-spec topic_alias_maximum(binary()) -> non_neg_integer().
topic_alias_maximum(Host) ->
    mod_mqtt_opt:max_topic_aliases(Host).

%%%===================================================================
%%% Timings
%%%===================================================================
-spec current_time() -> milli_seconds().
current_time() ->
    erlang:monotonic_time(millisecond).

-spec unix_time() -> seconds().
unix_time() ->
    erlang:system_time(second).

-spec set_keep_alive(state(), seconds()) -> state().
set_keep_alive(State, 0) ->
    ?DEBUG("Disabling MQTT keep-alive", []),
    State#state{timeout = infinity};
set_keep_alive(State, Secs) ->
    Secs1 = round(Secs * 1.5),
    ?DEBUG("Setting MQTT keep-alive to ~B seconds", [Secs1]),
    set_timeout(State, timer:seconds(Secs1)).

-spec reset_keep_alive(state()) -> state().
reset_keep_alive(#state{timeout = {MSecs, _}, jid = #jid{}} = State) ->
    set_timeout(State, MSecs);
reset_keep_alive(State) ->
    State.

-spec set_timeout(state(), milli_seconds()) -> state().
set_timeout(State, MSecs) ->
    Time = current_time(),
    State#state{timeout = {MSecs, Time}}.

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

%%%===================================================================
%%% Authentication
%%%===================================================================
-spec parse_credentials(connect()) -> {ok, jid:jid()} | {error, reason_code()}.
parse_credentials(#connect{client_id = <<>>} = C) ->
    parse_credentials(C#connect{client_id = p1_rand:get_string()});
parse_credentials(#connect{username = <<>>, client_id = ClientID}) ->
    Host = ejabberd_config:get_myname(),
    JID = case jid:make(ClientID, Host) of
              error -> jid:make(str:sha(ClientID), Host);
              J -> J
          end,
    parse_credentials(JID, ClientID);
parse_credentials(#connect{username = User} = Pkt) ->
    try jid:decode(User) of
	#jid{luser = <<>>} ->
            case jid:make(User, ejabberd_config:get_myname()) of
                error ->
                    {error, 'bad-user-name-or-password'};
                JID ->
                    parse_credentials(JID, Pkt#connect.client_id)
            end;
	JID ->
            parse_credentials(JID, Pkt#connect.client_id)
    catch _:{bad_jid, _} ->
	    {error, 'bad-user-name-or-password'}
    end.

-spec parse_credentials(jid:jid(), binary()) -> {ok, jid:jid()} | {error, reason_code()}.
parse_credentials(JID, ClientID) ->
    case gen_mod:is_loaded(JID#jid.lserver, mod_mqtt) of
        false ->
            {error, 'server-unavailable'};
        true ->
            case jid:replace_resource(JID, ClientID) of
                error ->
                    {error, 'client-identifier-not-valid'};
                JID1 ->
                    {ok, JID1}
            end
    end.

-spec authenticate(connect(), peername(), state()) -> {ok, jid:jid()} | {error, reason_code()}.
authenticate(Pkt, IP, State) ->
    case authenticate(Pkt, State) of
	{ok, JID, AuthModule} ->
	    ?INFO_MSG("Accepted MQTT authentication for ~ts by ~s backend from ~s",
		      [jid:encode(JID),
		       ejabberd_auth:backend_type(AuthModule),
		       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
	    {ok, JID};
	{error, _} = Err ->
	    Err
    end.

-spec authenticate(connect(), state()) -> {ok, jid:jid(), module()} | {error, reason_code()}.
authenticate(#connect{password = Pass, properties = Props} = Pkt, State) ->
    case parse_credentials(Pkt) of
	{ok, #jid{luser = LUser, lserver = LServer} = JID} ->
	    case maps:find(authentication_method, Props) of
		{ok, <<"X-OAUTH2">>} ->
		    Token = maps:get(authentication_data, Props, <<>>),
		    case ejabberd_oauth:check_token(
			   LUser, LServer, [<<"sasl_auth">>], Token) of
			true -> {ok, JID, ejabberd_oauth};
			_ -> {error, 'not-authorized'}
		    end;
		{ok, _} ->
		    {error, 'bad-authentication-method'};
		error ->
                    case Pass of
                        <<>> ->
                            case tls_auth(JID, State) of
                                true ->
                                    {ok, JID, pkix};
                                false ->
                                    {error, 'not-authorized'}
                            end;
                        _ ->
                            case ejabberd_auth:check_password_with_authmodule(
                                   LUser, <<>>, LServer, Pass) of
                                {true, AuthModule} -> {ok, JID, AuthModule};
                                false -> {error, 'not-authorized'}
                            end
                    end
	    end;
	{error, _} = Err ->
	    Err
    end.

-spec tls_auth(jid:jid(), state()) -> boolean().
tls_auth(_JID, #state{tls_verify = false}) ->
    false;
tls_auth(JID, State) ->
    case State#state.socket of
        {fast_tls, Sock} ->
            case fast_tls:get_peer_certificate(Sock, otp) of
                {ok, Cert} ->
                    case fast_tls:get_verify_result(Sock) of
                        0 ->
                            case get_cert_jid(Cert) of
                                {ok, JID2} ->
                                    jid:remove_resource(jid:tolower(JID)) ==
                                        jid:remove_resource(jid:tolower(JID2));
                                error ->
                                    false
                            end;
                        VerifyRes ->
                            Reason = fast_tls:get_cert_verify_string(VerifyRes, Cert),
                            ?WARNING_MSG("TLS verify failed: ~s", [Reason]),
                            false
                    end;
                error ->
                    false
            end;
        _ ->
            false
    end.

get_cert_jid(Cert) ->
    case Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject of
        {rdnSequence, Attrs1} ->
            Attrs = lists:flatten(Attrs1),
            case lists:keyfind(?'id-at-commonName',
                               #'AttributeTypeAndValue'.type, Attrs) of
                #'AttributeTypeAndValue'{value = {utf8String, Val}} ->
                    try jid:decode(Val) of
                        #jid{luser = <<>>} ->
                            case jid:make(Val, ejabberd_config:get_myname()) of
                                error ->
                                    error;
                                JID ->
                                    {ok, JID}
                            end;
                        JID ->
                            {ok, JID}
                    catch _:{bad_jid, _} ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.

%%%===================================================================
%%% Validators
%%%===================================================================
-spec validate_will(connect(), jid:jid()) -> ok | {error, error_reason()}.
validate_will(#connect{will = undefined}, _) ->
    ok;
validate_will(#connect{will = #publish{topic = Topic, payload = Payload},
                       will_properties = Props}, JID) ->
    case mod_mqtt:check_publish_access(Topic, jid:tolower(JID)) of
        deny -> {error, will_topic_forbidden};
        allow -> validate_payload(Props, Payload, will)
    end.

-spec validate_publish(publish(), state()) -> ok | {error, error_reason()}.
validate_publish(#publish{topic = Topic, payload = Payload,
                          properties = Props}, State) ->
    case validate_topic(Topic, Props, State) of
        ok -> validate_payload(Props, Payload, publish);
        Err -> Err
    end.

-spec validate_subscribe(subscribe()) -> ok | {error, error_reason()}.
validate_subscribe(#subscribe{filters = Filters}) ->
    case lists:any(
           fun({<<"$share/", _/binary>>, _}) -> true;
              (_) -> false
           end, Filters) of
        true ->
            {error, {code, 'shared-subscriptions-not-supported'}};
        false ->
            ok
    end.

-spec validate_topic(binary(), properties(), state()) -> ok | {error, error_reason()}.
validate_topic(<<>>, Props, State) ->
    case maps:get(topic_alias, Props, 0) of
        0 ->
            {error, {code, 'topic-alias-invalid'}};
        Alias ->
            case maps:is_key(Alias, State#state.topic_aliases) of
                true -> ok;
                false -> {error, unknown_topic_alias}
            end
    end;
validate_topic(_, #{topic_alias := Alias}, State) ->
    JID = State#state.jid,
    Max = topic_alias_maximum(JID#jid.lserver),
    if Alias > Max ->
            {error, {code, 'topic-alias-invalid'}};
       true ->
            ok
    end;
validate_topic(_, _, _) ->
    ok.

-spec validate_payload(properties(), binary(), will | publish) -> ok | {error, error_reason()}.
validate_payload(#{payload_format_indicator := utf8}, Payload, Type) ->
    try mqtt_codec:utf8(Payload) of
        _ -> ok
    catch _:_ ->
            {error, {payload_format_invalid, Type}}
    end;
validate_payload(_, _, _) ->
    ok.

%%%===================================================================
%%% Misc
%%%===================================================================
-spec resubscribe(jid:ljid(), subscriptions()) -> ok | {error, error_reason()}.
resubscribe(USR, Subs) ->
    case maps:fold(
           fun(TopicFilter, {SubOpts, ID}, ok) ->
                   mod_mqtt:subscribe(USR, TopicFilter, SubOpts, ID);
              (_, _, {error, _} = Err) ->
                   Err
           end, ok, Subs) of
        ok ->
            ok;
        {error, _} = Err1 ->
            unsubscribe(maps:keys(Subs), USR, #{}),
            Err1
    end.

-spec publish_will(state()) -> state().
publish_will(#state{will = #publish{} = Will,
		    jid = #jid{} = JID} = State) ->
    case publish(State, Will) of
        {ok, _} ->
            ?DEBUG("Will of ~ts has been published to ~ts",
                   [jid:encode(JID), Will#publish.topic]);
        {error, Why} ->
            ?WARNING_MSG("Failed to publish will of ~ts to ~ts: ~ts",
                         [jid:encode(JID), Will#publish.topic,
                          format_error(Why)])
    end,
    State#state{will = undefined};
publish_will(State) ->
    State.

-spec next_id(non_neg_integer()) -> pos_integer().
next_id(ID) ->
    (ID rem 65535) + 1.

-spec set_dup_flag(mqtt_packet()) -> mqtt_packet().
set_dup_flag(#publish{qos = QoS} = Pkt) when QoS>0 ->
    Pkt#publish{dup = true};
set_dup_flag(Pkt) ->
    Pkt.

-spec get_publish_code_props({ok, non_neg_integer()} |
                             {error, error_reason()}) -> {reason_code(), properties()}.
get_publish_code_props({ok, 0}) ->
    {'no-matching-subscribers', #{}};
get_publish_code_props({ok, _}) ->
    {success, #{}};
get_publish_code_props({error, Err}) ->
    Code = publish_reason_code(Err),
    Reason = format_reason_string(Err),
    {Code, #{reason_string => Reason}}.

-spec err_args(undefined | jid:jid(), peername(), error_reason()) -> iolist().
err_args(undefined, IP, Reason) ->
    [ejabberd_config:may_hide_data(misc:ip_to_list(IP)),
     format_error(Reason)];
err_args(JID, IP, Reason) ->
    [jid:encode(JID),
     ejabberd_config:may_hide_data(misc:ip_to_list(IP)),
     format_error(Reason)].

-spec log_disconnection(state(), error_reason()) -> ok.
log_disconnection(#state{jid = JID, peername = IP}, Reason) ->
    Msg = case JID of
              undefined -> "Rejected MQTT connection from ~ts: ~ts";
              _ -> "Closing MQTT connection for ~ts from ~ts: ~ts"
          end,
    case Reason of
        {Tag, _} when Tag == replaced; Tag == resumed; Tag == socket ->
            ?DEBUG(Msg, err_args(JID, IP, Reason));
        idle_connection ->
            ?DEBUG(Msg, err_args(JID, IP, Reason));
        Tag when Tag == session_expired; Tag == shutdown ->
            ?INFO_MSG(Msg, err_args(JID, IP, Reason));
        {peer_disconnected, Code, _} ->
            case mqtt_codec:is_error_code(Code) of
                true -> ?WARNING_MSG(Msg, err_args(JID, IP, Reason));
                false -> ?DEBUG(Msg, err_args(JID, IP, Reason))
            end;
        _ ->
            ?WARNING_MSG(Msg, err_args(JID, IP, Reason))
    end.
