%%
%% eredis_pubsub_client
%%
%% This client implements a subscriber to a Redis pubsub channel. It
%% is implemented in the same way as eredis_client, except channel
%% messages are streamed to the controlling process. Messages are
%% queued and delivered when the client acknowledges receipt.
%%
%% There is one consuming process per eredis_sub_client.
-module(eredis_sub_client).
-author('knut.nesheim@wooga.com').

-behaviour(gen_server).
-include("eredis.hrl").
-include("eredis_sub.hrl").


%% API
-export([start_link/6, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%
%% API
%%

-spec start_link(Host::list(),
                 Port::integer(),
                 Password::string(),
                 ReconnectSleep::integer(),
                 MaxQueueSize::integer(),
                 QueueBehaviour::drop | exit) ->
                        {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Host, Port, Password, ReconnectSleep, MaxQueueSize, QueueBehaviour) ->
    Args = [Host, Port, Password, ReconnectSleep, MaxQueueSize, QueueBehaviour],
    gen_server:start_link(?MODULE, Args, []).


stop(Pid) ->
    gen_server:call(Pid, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Port, Password, ReconnectSleep, MaxQueueSize, QueueBehaviour]) ->
    State = #state{host            = Host,
                   port            = Port,
                   password        = list_to_binary(Password),
                   reconnect_sleep = ReconnectSleep,
                   channels        = [],
                   parser_state    = eredis_parser:init(),
                   msg_queue       = queue:new(),
                   max_queue_size  = MaxQueueSize,
                   queue_behaviour = QueueBehaviour},

    case connect(State) of
        {ok, NewState} ->
            inet:setopts(NewState#state.socket, [{active, once}]),
            {ok, NewState};
        {error, Reason} ->
            {stop, {connection_error, Reason}}
    end.

%% Set the controlling process. All messages on all channels are directed here.
handle_call({controlling_process, Pid}, _From, State) ->
    case State#state.controlling_process of
        undefined ->
            ok;
        {OldRef, _OldPid} ->
            erlang:demonitor(OldRef)
    end,
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{controlling_process={Ref, Pid}, msg_state = ready}};

handle_call(get_channels, _From, State) ->
    {reply, {ok, State#state.channels}, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.


%% Controlling process acks, but we have no connection. When the
%% connection comes back up, we should be ready to forward a message
%% again.
handle_cast({ack_message, Pid},
            #state{controlling_process={_, Pid}, socket = undefined} = State) ->
    {noreply, State#state{msg_state = ready}};

%% Controlling process acknowledges receipt of previous message. Send
%% the next if there is any messages queued or ask for more on the
%% socket.
handle_cast({ack_message, Pid},
            #state{controlling_process={_, Pid}} = State) ->
    NewState = case queue:out(State#state.msg_queue) of
                   {empty, _Queue} ->
                       State#state{msg_state = ready};
                   {{value, Msg}, Queue} ->
                       send_to_controller(Msg, State),
                       State#state{msg_queue = Queue, msg_state = need_ack}
               end,
    {noreply, NewState};

handle_cast({subscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["SUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    {noreply, State#state{channels = Channels ++ State#state.channels}};


handle_cast({psubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["PSUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    {noreply, State#state{channels = Channels ++ State#state.channels}};



handle_cast({unsubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["UNSUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = lists:foldl(fun (C, Cs) -> lists:delete(C, Cs) end,
                              State#state.channels, Channels),
    {noreply, State#state{channels = NewChannels}};



handle_cast({punsubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["PUNSUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = lists:foldl(fun (C, Cs) -> lists:delete(C, Cs) end,
                              State#state.channels, Channels),
    {noreply, State#state{channels = NewChannels}};



handle_cast({ack_message, _}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% Receive data from socket, see handle_response/2
handle_info({tcp, _Socket, Bs}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    NewState = handle_response(Bs, State),
    case queue:len(NewState#state.msg_queue) > NewState#state.max_queue_size of
        true ->
            case State#state.queue_behaviour of
                drop ->
                    Msg = {dropped, queue:len(NewState#state.msg_queue)},
                    send_to_controller(Msg, NewState),
                    {noreply, NewState#state{msg_queue = queue:new()}};
                exit ->
                    {stop, max_queue_size, State}
            end;
        false ->
            {noreply, NewState}
    end;

%% Socket got closed, for example by Redis terminating idle
%% clients. Spawn of a new process which will try to reconnect and
%% notify us when Redis is ready. In the meantime, we can respond with
%% an error message to all our clients.
handle_info({tcp_closed, _Socket}, State) ->
    Self = self(),
    send_to_controller({eredis_disconnected, Self}, State),
    spawn(fun() -> reconnect_loop(Self, State) end),

    %% Throw away the socket. The absence of a socket is used to
    %% signal we are "down"
    {noreply, State#state{socket = undefined}};

%% Redis is ready to accept requests, the given Socket is a socket
%% already connected and authenticated.
handle_info({connection_ready, Socket}, #state{socket = undefined} = State) ->
    send_to_controller({eredis_connected, self()}, State),
    {noreply, State#state{socket = Socket}};

%% Our controlling process is down.
handle_info({'DOWN', Ref, process, Pid, _Reason},
            #state{controlling_process={Ref, Pid}} = State) ->
    {stop, shutdown, State#state{controlling_process=undefined,
                                 msg_state=ready,
                                 msg_queue=queue:new()}};

%% eredis can be used in Poolboy, but it requires to support a simple API
%% that Poolboy uses to manage the connections.
handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, State) ->
    case State#state.socket of
        undefined -> ok;
        Socket    -> gen_tcp:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec handle_response(Data::binary(), State::#state{}) -> NewState::#state{}.
%% @doc: Handle the response coming from Redis. This should only be
%% channel messages that we should forward to the controlling process
%% or queue if the previous message has not been acked. If there are
%% more than a single response in the data we got, queue the responses
%% and serve them up when the controlling process is ready
handle_response(Data, #state{parser_state = ParserState} = State) ->
    case eredis_parser:parse(ParserState, Data) of
        {ReturnCode, Value, NewParserState} ->
            reply({ReturnCode, Value},
                  State#state{parser_state=NewParserState});

        {ReturnCode, Value, Rest, NewParserState} ->
            NewState = reply({ReturnCode, Value},
                             State#state{parser_state=NewParserState}),
            handle_response(Rest, NewState);

        {continue, NewParserState} ->
            State#state{parser_state = NewParserState}
    end.

%% @doc: Sends a reply to the controlling process if the process has
%% acknowledged the previous process, otherwise the message is queued
%% for later delivery.
reply({ok, [<<"message">>, Channel, Message]}, State) ->
    queue_or_send({message, Channel, Message, self()}, State);

reply({ok, [<<"pmessage">>, Pattern, Channel, Message]}, State) ->
    queue_or_send({pmessage, Pattern, Channel, Message, self()}, State);



reply({ok, [<<"subscribe">>, Channel, _]}, State) ->
    queue_or_send({subscribed, Channel, self()}, State);

reply({ok, [<<"psubscribe">>, Channel, _]}, State) ->
    queue_or_send({subscribed, Channel, self()}, State);


reply({ok, [<<"unsubscribe">>, Channel, _]}, State) ->
    queue_or_send({unsubscribed, Channel, self()}, State);


reply({ok, [<<"punsubscribe">>, Channel, _]}, State) ->
    queue_or_send({unsubscribed, Channel, self()}, State);
reply({ReturnCode, Value}, State) ->
    throw({unexpected_response_from_redis, ReturnCode, Value, State}).


queue_or_send(Msg, State) ->
    case State#state.msg_state of
        need_ack ->
            MsgQueue = queue:in(Msg, State#state.msg_queue),
            State#state{msg_queue = MsgQueue};
        ready ->
            send_to_controller(Msg, State),
            State#state{msg_state = need_ack}
    end.


%% @doc: Helper for connecting to Redis. These commands are
%% synchronous and if Redis returns something we don't expect, we
%% crash. Returns {ok, State} or {SomeError, Reason}.
connect(State) ->
    case gen_tcp:connect(State#state.host, State#state.port, ?SOCKET_OPTS) of
        {ok, Socket} ->
            inet:setopts(Socket, [{active, false}]),
            case authenticate(Socket, State#state.password) of
                ok ->
                    {ok, State#state{socket = Socket}};
                {error, Reason} ->
                    {error, {authentication_error, Reason}}
            end;
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.


authenticate(_Socket, <<>>) ->
    ok;
authenticate(Socket, Password) ->
    do_sync_command(Socket, ["AUTH", " ", Password, "\r\n"]).

%% @doc: Executes the given command synchronously, expects Redis to
%% return "+OK\r\n", otherwise it will fail.
do_sync_command(Socket, Command) ->
    case gen_tcp:send(Socket, Command) of
        ok ->
            %% Hope there's nothing else coming down on the socket..
            case gen_tcp:recv(Socket, 0) of
                {ok, <<"+OK\r\n">>} ->
                    ok;
                Other ->
                    {error, {unexpected_data, Other}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc: Loop until a connection can be established, this includes
%% successfully issuing the auth and select calls. When we have a
%% connection, give the socket to the redis client.
reconnect_loop(Client, #state{reconnect_sleep=ReconnectSleep}=State) ->
    case catch(connect(State)) of
        {ok, #state{socket = Socket}} ->
            gen_tcp:controlling_process(Socket, Client),
            Client ! {connection_ready, Socket};
        {error, _Reason} ->
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State);
        %% Something bad happened when connecting, like Redis might be
        %% loading the dataset and we got something other than 'OK' in
        %% auth or select
        _ ->
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State)
    end.


send_to_controller(_Msg, #state{controlling_process=undefined}) ->
    ok;
send_to_controller(Msg, #state{controlling_process={_Ref, Pid}}) ->
    %%error_logger:info_msg("~p ! ~p~n", [Pid, Msg]),
    Pid ! Msg.
