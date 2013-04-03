-module(mod_bosh_socket).

-behaviour(gen_fsm).

%% API
-export([start/2,
         start_link/2,
         start_supervisor/0,
         handle_request/2,
         pause/2]).

%% Private API
-export([get_handlers/1,
         get_pending/1]).

%% ejabberd_socket compatibility
-export([starttls/2, starttls/3,
         compress/1, compress/2,
         reset_stream/1,
         send/2,
         send_xml/2,
         change_shaper/2,
         monitor/1,
         get_sockmod/1,
         close/1,
         peername/1]).

%% gen_fsm callbacks
-export([init/1,
         accumulate/2, accumulate/3,
         normal/2, normal/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("mod_bosh.hrl").

-define(ACCUMULATE_PERIOD, 10).
-define(DEFAULT_HOLD, 1).
-define(CONCURRENT_REQUESTS, 2).
-define(DEFAULT_WAIT, 60).

-define(DEFAULT_INACTIVITY, 30).
-define(DEFAULT_MAXPAUSE, 120).
-define(DEFAULT_ACKS, false).

-type rid() :: pos_integer().

-record(state, {c2s_pid :: pid(),
                handlers = [] :: [{rid(), timer:tref(), pid()}],
                %% Elements buffered for sending to the client.
                pending = [] :: [xmlstreamelement()],
                sid :: bosh_sid(),
                wait = ?DEFAULT_WAIT,
                hold = ?DEFAULT_HOLD,
                rid :: rid(),
                %% Requests deferred for later processing because
                %% of having Rid greater than expected.
                deferred = [] :: [{rid(), {event_type(), #xmlelement{}}}],

                %%% Options. These have accompanying DEFAULT_* macros and
                %%% are set up in init/1 based on ejabberd.cfg.

                %% Allowed inactivity period in seconds.
                inactivity :: pos_integer() | infinity,
                inactivity_tref,
                %% Max pause period in seconds.
                maxpause :: pos_integer() | undefined,
                %% Are acknowledgements used?
                acks :: boolean()}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Sid, Peer) ->
    supervisor:start_child(?BOSH_SOCKET_SUP, [Sid, Peer]).

start_link(Sid, Peer) ->
    gen_fsm:start_link(?MODULE, [Sid, Peer], []).

start_supervisor() ->
    ChildId = ?BOSH_SOCKET_SUP,
    ChildSpec =
        {ChildId,
         {ejabberd_tmp_sup, start_link,
          [ChildId, ?MODULE]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    case supervisor:start_child(ejabberd_sup, ChildSpec) of
        {ok, undefined} ->
            {error, undefined};
        {ok, Child} ->
            {ok, Child};
        {ok, Child, _Info} ->
            {ok, Child};
        {error, {already_started, Child}} ->
            {ok, Child};
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_request(Pid, {EventTag, Handler, Body}) -> ok
    when Pid :: pid(),
         EventTag :: event_type(),
         Handler :: pid(),
         Body :: #xmlelement{}.
handle_request(Pid, Request) ->
    gen_fsm:send_all_state_event(Pid, Request).

-spec pause(Pid, Seconds) -> ok
    when Pid :: pid(),
         Seconds :: pos_integer().
pause(Pid, Seconds) ->
    gen_fsm:send_all_state_event(Pid, {pause, Seconds}).

%%--------------------------------------------------------------------
%% Private API
%%--------------------------------------------------------------------

get_handlers(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_handlers).

get_pending(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_pending).

%%--------------------------------------------------------------------
%% gen_fsm callbacks
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Sid, Peer]) ->
    BoshSocket = #bosh_socket{sid = Sid, pid = self(), peer = Peer},
    C2SOpts = [{xml_socket, true}],
    {ok, C2SPid} = ejabberd_c2s:start({mod_bosh_socket, BoshSocket}, C2SOpts),
    ?DEBUG("mod_bosh_socket started~n", []),
    {ok, accumulate, #state{sid = Sid,
                            c2s_pid = C2SPid,
                            inactivity = get_inactivity(),
                            maxpause = get_maxpause(),
                            acks = get_acks()}}.

get_inactivity() ->
    case mod_bosh:get_inactivity() of
        undefined -> ?DEFAULT_INACTIVITY;
        I -> I
    end.

%% TODO: maybe make maxpause runtime configurable like inactivity?
get_maxpause() ->
    case gen_mod:get_module_opt(?MYNAME, mod_bosh, maxpause, undefined) of
        undefined -> ?DEFAULT_MAXPAUSE;
        MP -> MP
    end.

get_acks() ->
    case gen_mod:get_module_opt(?MYNAME, mod_bosh, acks, undefined) of
        undefined -> ?DEFAULT_ACKS;
        Acks -> Acks
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

accumulate(acc_off, #state{pending = Pending} = S) ->
    NS = S#state{pending = []},
    {next_state, normal, send_or_store(Pending, NS)};
accumulate(Event, State) ->
    ?DEBUG("Unhandled event in 'accumulate' state: ~w~n", [Event]),
    {next_state, accumulate, State}.

normal(acc_off, #state{} = S) ->
    {next_state, normal, S};
normal(Event, State) ->
    ?DEBUG("Unhandled event in 'normal' state: ~w~n", [Event]),
    {next_state, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
accumulate(Event, _From, State) ->
    ?DEBUG("Unhandled sync event in 'accumulate' state: ~w~n", [Event]),
    {reply, ok, state_name, State}.

normal(Event, _From, State) ->
    ?DEBUG("Unhandled sync event in 'normal' state: ~w~n", [Event]),
    {reply, ok, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

handle_event({EventTag, Handler, #xmlelement{} = Body}, StateName, State) ->
    NS = cancel_inactivity_timer(State),
    try
        Rid = binary_to_integer(exml_query:attr(Body, <<"rid">>)),
        EventHandledState = handle_stream_event({Rid, EventTag, Body}, NS),
        HandlerAddedState = new_request_handler(StateName, {Rid, Handler},
                                               EventHandledState),
        case EventTag of
            _ when EventTag == streamstart; EventTag == restart ->
                timer:apply_after(?ACCUMULATE_PERIOD,
                                  gen_fsm, send_event, [self(), acc_off]),
                {next_state, accumulate, HandlerAddedState};
            _ when EventTag == normal; EventTag == streamend ->
                {next_state, StateName, HandlerAddedState}
        end
    catch
        throw:{invalid_rid, TState} ->
            {stop, {shutdown, invalid_rid}, TState}
    end;
handle_event({pause, Seconds}, _StateName, #state{maxpause = MaxPause} = S)
       when MaxPause == undefined;
            Seconds > MaxPause ->
    [Pid ! policy_violation || {_, _, Pid} <- S#state.handlers],
    {stop, {shutdown, policy_violation}, S#state{handlers = []}};
handle_event({pause, Seconds}, StateName, State) ->
    NewState = handle_pause(Seconds, State),
    {next_state, StateName, NewState};

handle_event(Event, StateName, State) ->
    ?DEBUG("Unhandled all state event: ~w~n", [Event]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(get_handlers, _From, StateName,
                  #state{handlers = Handlers} = S) ->
    {reply, Handlers, StateName, S};
handle_sync_event(get_pending, _From, StateName,
                  #state{pending = Pending} = S) ->
    {reply, Pending, StateName, S};
handle_sync_event(Event, _From, StateName, State) ->
    ?DEBUG("Unhandled sync all state event: ~w~n", [Event]),
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

handle_info({send, #xmlstreamend{} = StreamEnd}, _SName,
            #state{pending = Pending} = S) ->
    NS = send_or_store([StreamEnd | Pending], S#state{pending = []}),
    {next_state, normal, NS};
handle_info({send, Data}, accumulate = SName, #state{} = S) ->
    {next_state, SName, store(Data, S)};
handle_info({send, Data}, normal = SName, #state{} = S) ->
    NS = send_or_store(Data, S),
    {next_state, SName, NS};
handle_info(reset_stream, SName, #state{} = S) ->
    %% TODO: actually reset the stream once it's stored per bosh session
    ?DEBUG("Stream reset by c2s~n", []),
    {next_state, SName, S};
handle_info(close, _SName, State) ->
    {stop, normal, State};
handle_info(inactivity_timeout, _SName, State) ->
    ?DEBUG("terminating due to client inactivity~n", []),
    {stop, {shutdown, inactivity_timeout}, State};
handle_info({wait_timeout, {Rid, Pid}}, SName,
            #state{handlers = Handlers} = S) ->
    ?DEBUG("'wait' limit reached for ~p~n", [Pid]),
    %% In case some message was being handled when the timer fired
    %% it may turn out that Pid is no longer available in Handlers.
    case lists:keytake(Rid, 1, Handlers) of
        false ->
            {next_state, SName, S};
        {value, {Rid, _, Pid}, NewHandlers} ->
            NS = send_to_handler({Rid, Pid}, [], S, []),
            {next_state, SName, NS#state{handlers = NewHandlers}}
    end;
handle_info(Info, SName, State) ->
    ?DEBUG("Unhandled info in '~s' state: ~w~n", [SName, Info]),
    {next_state, SName, State}.

terminate(_Reason, StateName, #state{sid = Sid, handlers = Handlers} = S) ->
    [Pid ! {close, Sid} || {_, _, Pid} <- lists:sort(Handlers)],
    ?BOSH_BACKEND:delete_session(Sid),
    catch ejabberd_c2s:stop(S#state.c2s_pid),
    ?DEBUG("Closing session ~p in '~s' state. Handlers: ~p Pending: ~p~n",
           [Sid, StateName, Handlers, S#state.pending]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% callback implementations
%%--------------------------------------------------------------------

handle_stream_event({Rid, EventTag, Body} = Event, #state{rid = OldRid} = S) ->
    case {EventTag,
          is_valid_rid(Rid, OldRid),
          is_acceptable_rid(Rid, OldRid)} of
        {streamstart, _, _} ->
            process_stream_event(EventTag, Body, S#state{rid = Rid});
        {_, true, _} ->
            process_stream_event(EventTag, Body, S#state{rid = Rid});
        {_, false, true} ->
            ?DEBUG("storing stream event for deferred processing: ~p~n",
                   [{EventTag, Body}]),
            S#state{deferred = [Event | S#state.deferred]};
        {_, false, false} ->
            ?ERROR_MSG("invalid rid: ~p~n", [{EventTag, Body}]),
            [Pid ! item_not_found || {_, _, Pid} <- lists:sort(S#state.handlers)],
            throw({invalid_rid, S#state{handlers = []}})
    end.

process_stream_event(EventTag, Body, #state{c2s_pid = C2SPid} = State) ->
    {Els, NewState} = bosh_unwrap(EventTag, Body, State),
    [forward_to_c2s(C2SPid, El) || El <- Els],
    process_deferred_events(NewState).

process_deferred_events(#state{deferred = Deferred} = S) ->
    lists:foldl(fun(Event, State) ->
                    ?DEBUG("processing deferred event: ~p~n", [Event]),
                    handle_stream_event(Event, State)
                end,
                S#state{deferred = []},
                lists:sort(Deferred)).

is_valid_rid(Rid, OldRid) when Rid == OldRid + 1 ->
    true;
is_valid_rid(_, _) ->
    false.

is_acceptable_rid(Rid, OldRid)
        when Rid > OldRid + 1,
             Rid =< OldRid + ?CONCURRENT_REQUESTS ->
    true;
is_acceptable_rid(_, _) ->
    false.

%% Send data to the client if any request handler is available.
%% Otherwise, store for sending later.
send_or_store(Data, #state{handlers = []} = S) ->
    store(Data, S);
send_or_store(Data, #state{} = S) when not is_list(Data) ->
    send_or_store([Data], S);
send_or_store([], #state{} = S) ->
    S;
send_or_store(Data, #state{handlers = Hs} = State) ->
    ?DEBUG("Forwarding to handler. Handlers: ~p~n", [Hs]),
    send_to_handler(Data, State).

send_to_handler(Data, State) ->
    send_to_handler(Data, State, []).

send_to_handler(Data, #state{handlers = Handlers} = S, Opts) ->
    [{Rid, TRef, Pid} | HRest] = lists:sort(Handlers),
    %% The cancellation might fail if the timer already fired.
    %% Don't worry, it's handled on receiving the timeout message.
    timer:cancel(TRef),
    send_to_handler({Rid, Pid}, Data, S#state{handlers = HRest}, Opts).

send_to_handler({Rid, Pid}, Data, State, Opts) ->
    {Wrapped, NS} = bosh_wrap(Data, State),
    Acked = maybe_ack(Wrapped, Rid, NS),
    ?DEBUG("send to ~p: ~p~n", [Pid, Acked]),
    Pid ! {bosh_reply, Acked},
    case proplists:get_value(pause, Opts, false) of
        false ->
            setup_inactivity_timer(NS);
        _ ->
            NS
    end.

maybe_ack(#xmlelement{attrs = Attrs} = Body, HandlerRid, #state{rid = Rid} = S)
       when Rid > HandlerRid ->
    Body#xmlelement{attrs = lists:keydelete(<<"ack">>, 1, Attrs)
                            ++ ack(S#state.acks, Rid)};
maybe_ack(Body, _, _) ->
    Body.

setup_inactivity_timer(#state{inactivity = infinity} = S) ->
    S;
setup_inactivity_timer(S) ->
    {ok, TRef} = timer:send_after(timer:seconds(S#state.inactivity),
                                  inactivity_timeout),
    S#state{inactivity_tref = TRef}.

cancel_inactivity_timer(S) ->
    timer:cancel(S#state.inactivity_tref),
    S#state{inactivity_tref = undefined}.

%% Store data for sending later.
store(Data, #state{pending = Pending} = S) ->
    S#state{pending = [Data | Pending]}.

forward_to_c2s(C2SPid, StreamElement) ->
    gen_fsm:send_event(C2SPid, StreamElement).

%% Keep in mind the hardcoding for hold == 1.
new_request_handler(accumulate, {Rid, Pid}, #state{handlers = [_]} = S) ->
    NS = send_to_handler([], S),
    add_handler({Rid, Pid}, NS);
new_request_handler(accumulate, Handler, #state{handlers = []} = S) ->
    add_handler(Handler, S);
new_request_handler(normal, Handler, #state{pending = [],
                                            handlers = [_]} = S) ->
    NS = send_to_handler([], S),
    add_handler(Handler, NS);
new_request_handler(normal, Handler, #state{pending = [],
                                            handlers = []} = S) ->
    add_handler(Handler, S);
new_request_handler(normal, Handler, #state{pending = Pending} = S) ->
    NS = add_handler(Handler, S#state{pending = []}),
    send_or_store(Pending, NS).

add_handler({Rid, Pid}, #state{handlers = Handlers} = S) ->
    {ok, TRef} = timer:send_after(timer:seconds(S#state.wait),
                                  {wait_timeout, {Rid, Pid}}),
    S#state{handlers = [{Rid, TRef, Pid} | Handlers]}.

-spec bosh_unwrap(EventTag, #xmlelement{}, #state{})
    -> {[StreamEvent], #state{}}
    when EventTag :: event_type(),
         StreamEvent :: #xmlstreamstart{}
                     | {xmlstreamelement, #xmlelement{}}
                     | #xmlstreamend{}.
bosh_unwrap(StreamEvent, Body, #state{} = S)
       when StreamEvent =:= streamstart;
            StreamEvent =:= restart ->
    Wait = get_attr(<<"wait">>, Body, S#state.wait),
    Hold = get_attr(<<"hold">>, Body, S#state.hold),
    E = stream_start(exml_query:attr(Body, <<"from">>),
                     exml_query:attr(Body, <<"to">>)),
    {[E], record_set(S, [{#state.wait, Wait},
                         {#state.hold, Hold}])};
bosh_unwrap(streamend, Body, State) ->
    {Els, NewState} = bosh_unwrap(normal, Body, State),
    {Els ++ [#xmlstreamend{name = <<>>}], NewState};
bosh_unwrap(normal, Body, #state{sid = Sid} = State) ->
    Sid = exml_query:attr(Body, <<"sid">>),
    ?NS_HTTPBIND = exml_query:attr(Body, <<"xmlns">>),
    {[{xmlstreamelement, El}
      || El <- Body#xmlelement.children,
         %% Ignore whitespace keepalives.
         El /= {xmlcdata, <<" ">>}],
     State}.

get_attr(Attr, Element, Default) ->
    case exml_query:attr(Element, Attr) of
        undefined ->
            Default;
        Value ->
            binary_to_integer(Value)
    end.

stream_start(From, To) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = [{<<"from">>, From},
                             {<<"to">>, To},
                             {<<"version">>, <<"1.0">>},
                             {<<"xml:lang">>, <<"en">>},
                             {<<"xmlns">>, <<"jabber:client">>},
                             {<<"xmlns:stream">>, ?NS_STREAM}]}.

bosh_wrap(Elements, #state{} = S) ->
    EventsStanzas = lists:partition(fun is_stream_event/1, Elements),
    {{Body, Children}, NS} = case EventsStanzas of
        {[], Stanzas} ->
            {{bosh_body(S), Stanzas}, S};
        {[#xmlstreamstart{} = StreamStart], Stanzas} ->
            {{bosh_stream_start_body(StreamStart, S), Stanzas}, S};
        {[#xmlstreamend{}], []} ->
            %% No stanzas except stream end - OK.
            {{bosh_stream_end_body(), []}, S};
        {[#xmlstreamend{} = StreamEnd], Stanzas} ->
            %% Can't wrap remaining stanzas in a stream end body.
            %% Send Stanzas and forfeit sending stream end.
            ?DEBUG("pending stanzas, can't send stream end", []),
            Pending = S#state.pending,
            {{bosh_body(S), Stanzas},
             S#state{pending = [StreamEnd, Pending]}}
    end,
    {Body#xmlelement{children = Children}, NS}.

is_stream_event(#xmlstreamstart{}) ->
    true;
is_stream_event(#xmlstreamend{}) ->
    true;
is_stream_event(_) ->
    false.

%% Bosh body for a session creation response.
bosh_stream_start_body(#xmlstreamstart{attrs = Attrs}, #state{} = S) ->
    %% TODO: acks?
    #xmlelement{name = <<"body">>,
                attrs = [{<<"wait">>, integer_to_binary(S#state.wait)},
                         {<<"requests">>, integer_to_binary(?CONCURRENT_REQUESTS)},
                         {<<"hold">>, integer_to_binary(S#state.hold)},
                         {<<"from">>, proplists:get_value(<<"from">>, Attrs)},
                         %% TODO: how to support these with cowboy?
                         {<<"accept">>, <<"deflate,gzip">>},
                         {<<"sid">>, S#state.sid},
                         {<<"xmpp:restartlogic">>, <<"true">>},
                         {<<"xmpp:version">>, <<"1.0">>},
                         {<<"xmlns">>, ?NS_HTTPBIND},
                         {<<"xmlns:xmpp">>, <<"urn:xmpp:xbosh">>},
                         {<<"xmlns:stream">>, ?NS_STREAM}] ++
                        inactivity(S#state.inactivity) ++
                        maxpause(S#state.maxpause) ++
                        %% TODO: shouldn't an ack be sent on restart?
                        ack(S#state.acks, S#state.rid),
                children = []}.

inactivity(I) ->
    [{<<"inactivity">>, integer_to_binary(I)} || is_integer(I)].

maxpause(MP) ->
    [{<<"maxpause">>, integer_to_binary(MP)} || is_integer(MP)].

ack(Acks, Rid) ->
    [{<<"ack">>, integer_to_binary(Rid)} || Acks =:= true].

%% Bosh body for an ordinary stream element(s).
bosh_body(#state{} = S) ->
    %% TODO: acks?
    #xmlelement{name = <<"body">>,
                attrs = [{<<"sid">>, S#state.sid},
                         {<<"xmlns">>, ?NS_HTTPBIND}],
                children = []}.

bosh_stream_end_body() ->
    #xmlelement{name = <<"body">>,
                attrs = [{<<"type">>, <<"terminate">>},
                         {<<"xmlns">>, ?NS_HTTPBIND}],
                children = []}.

handle_pause(Seconds, State) ->
    F = fun(_, S) ->
            send_to_handler([], S, [pause])
    end,
    NS = lists:foldl(F, State,
                     lists:seq(1, length(State#state.handlers))),
    NS#state{inactivity = Seconds}.

%%--------------------------------------------------------------------
%% ejabberd_socket compatibility
%%--------------------------------------------------------------------

%% Should be negotiated on HTTP level.
starttls(SocketData, TLSOpts) ->
    starttls(SocketData, TLSOpts, <<>>).

starttls(_SocketData, _TLSOpts, _Data) ->
    throw({error, negotiate_tls_on_http_level}).

%% Should be negotiated on HTTP level.
compress(SocketData) ->
    compress(SocketData, <<>>).

compress(_SocketData, _Data) ->
    throw({error, negotiate_compression_on_http_level}).

%% TODO: adjust for BOSH
reset_stream(#bosh_socket{pid = Pid} = SocketData) ->
    Pid ! reset_stream,
    SocketData.

send_xml(Socket, {xmlstreamelement, XML}) ->
    send(Socket, XML);
send_xml(Socket, #xmlstreamstart{} = XML) ->
    send(Socket, XML);
send_xml(Socket, #xmlstreamend{} = XML) ->
    send(Socket, XML).

send(#bosh_socket{pid = Pid}, Data) ->
    Pid ! {send, Data},
    ok.

change_shaper(SocketData, _Shaper) ->
    %% TODO: we ignore shapers for now
    SocketData.

monitor(#bosh_socket{pid = Pid}) ->
    erlang:monitor(process, Pid).

get_sockmod(_SocketData) ->
    ?MODULE.

close(#bosh_socket{pid = Pid}) ->
    Pid ! close.

-spec peername(#bosh_socket{}) -> {ok, {Addr, Port}}
    when Addr :: inet:ip_address(),
         Port :: inet:port_number().
peername(#bosh_socket{peer = Peer}) ->
    {ok, Peer}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Set Fields of the Record to Values,
%% when {Field, Value} <- FieldValues (in list comprehension syntax).
record_set(Record, FieldValues) ->
    F = fun({Field, Value}, Rec) ->
            setelement(Field, Rec, Value)
        end,
    lists:foldl(F, Record, FieldValues).
