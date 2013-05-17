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
         get_pending/1,
         get_client_acks/1,
         set_client_acks/2,
         get_cached_responses/1]).

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
-define(DEFAULT_MAXPAUSE, 120).
-define(DEFAULT_CLIENT_ACKS, false).

-type cached_response() :: {rid(), erlang:timestamp(), #xmlel{}}.
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
                deferred = [] :: [{rid(), {event_type(), #xmlel{}}}],
                client_acks = ?DEFAULT_CLIENT_ACKS :: boolean(),
                sent = [] :: [cached_response()],
                %% Allowed inactivity period in seconds.
                inactivity :: pos_integer() | infinity,
                inactivity_tref,
                %% Max pause period in seconds.
                maxpause :: pos_integer() | undefined,
                %% Are acknowledgements used?
                server_acks :: boolean(),
                last_processed :: rid() | undefined,
                %% Report scheduled for sending at the earliest
                %% possible occasion.
                report = false :: {rid(), timer:time()} | false}).

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
         Body :: #xmlel{}.
handle_request(Pid, Request) ->
    gen_fsm:send_all_state_event(Pid, Request).

%% TODO: no handler for this call is present!
%% No check for violating maxpause is made when calling this!
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

-spec get_client_acks(pid()) -> boolean().
get_client_acks(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_client_acks).

-spec set_client_acks(pid(), boolean()) -> any().
set_client_acks(Pid, Enabled) ->
    gen_fsm:sync_send_all_state_event(Pid, {set_client_acks, Enabled}).

-spec get_cached_responses(pid()) -> [cached_response()].
get_cached_responses(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_cached_responses).

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
                            inactivity = mod_bosh:get_inactivity(),
                            maxpause = get_maxpause(),
                            server_acks = mod_bosh:get_server_acks()}}.

%% TODO: maybe make maxpause runtime configurable like inactivity?
get_maxpause() ->
    case gen_mod:get_module_opt(?MYNAME, mod_bosh, maxpause, undefined) of
        undefined -> ?DEFAULT_MAXPAUSE;
        MP -> MP
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

handle_event({EventTag, Handler, #xmlel{} = Body}, SName, S) ->
    NS = cancel_inactivity_timer(S),
    Rid = binary_to_integer(exml_query:attr(Body, <<"rid">>)),
    try
        NNS = handle_stream_event({EventTag, Body, Rid}, Handler, SName, NS),
        %% TODO: it's the event which determines the next state,
        %%       this ought to be returned from handle_stream_event
        case EventTag of
            _ when EventTag == streamstart; EventTag == restart ->
                timer:apply_after(?ACCUMULATE_PERIOD,
                                  gen_fsm, send_event, [self(), acc_off]),
                {next_state, accumulate, NNS};
            _ ->
                {next_state, SName, NNS}
        end
    catch
        throw:{invalid_rid, TState} ->
            {stop, {shutdown, invalid_rid}, TState};
        throw:{invalid_pause, TState} ->
            {stop, {shutdown, policy_violation}, TState}
    end;

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
handle_sync_event(get_client_acks, _From, StateName,
                  #state{client_acks = ClientAcks} = S) ->
    {reply, ClientAcks, StateName, S};
handle_sync_event({set_client_acks, ClientAcks}, _From, StateName,
                  #state{} = S) ->
    NS = S#state{client_acks = ClientAcks},
    {reply, ok, StateName, NS};
handle_sync_event(get_cached_responses, _From, StateName,
                  #state{sent = CachedResponses} = S) ->
    {reply, CachedResponses, StateName, S};
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
    NS = send_or_store(Pending ++ [StreamEnd], S#state{pending = []}),
    {next_state, normal, NS};
handle_info({send, Data}, accumulate = SName, #state{} = S) ->
    {next_state, SName, store([Data], S)};
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
            NS = send_to_handler({Rid, Pid}, [], S),
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

handle_stream_event({EventTag, Body, Rid} = Event, Handler,
                    SName, #state{rid = OldRid} = S) ->
    NS = maybe_add_handler(Handler, Rid, S),
    NNS = case {EventTag,
                maybe_is_retransmission(Rid, OldRid, S#state.sent),
                is_valid_rid(Rid, OldRid),
                is_acceptable_rid(Rid, OldRid)}
    of
        {_, {true, CachedResponse}, _, _} ->
            case CachedResponse of
                none ->
                    NS;
                _ ->
                    resend_cached(CachedResponse, NS)
            end;
        {streamstart, _, _, _} ->
            process_acked_stream_event(Event, SName, NS);
        {_, _, true, _} ->
            process_acked_stream_event(Event, SName, NS);
        {_, _, false, true} ->
            ?DEBUG("storing stream event for deferred processing: ~p~n",
                   [{EventTag, Body}]),
            NS#state{deferred = [Event | NS#state.deferred]};
        {_, _, false, false} ->
            ?ERROR_MSG("invalid rid ~p:~n~p~n", [Rid, {EventTag, Body}]),
            [Pid ! item_not_found
             || {_, _, Pid} <- lists:sort(NS#state.handlers)],
            throw({invalid_rid, NS#state{handlers = []}})
    end,
    return_surplus_handlers(SName, NNS).

-spec maybe_is_retransmission(rid(), rid(), [cached_response()])
    -> false | {true, none} | {true, cached_response()}.
maybe_is_retransmission(Rid, OldRid, Sent) ->
    case {lists:keyfind(Rid, 1, Sent), Rid =:= OldRid} of
        {false, false} ->
            false;
        {false, true} ->
            ?INFO_MSG("request ~p repeated but no response found in cache ~p~n",
                      [Rid, Sent]),
            {true, none};
        {CachedResponse, _} ->
            {true, CachedResponse}
    end.

resend_cached({_Rid, _, CachedBody}, S) ->
    send_to_handler(CachedBody, S).

process_acked_stream_event({EventTag, Body, Rid}, SName,
                           #state{} = S) ->
    MaybeBAck = exml_query:attr(Body, <<"ack">>),
    {Action, Ack} = determine_report_action(MaybeBAck, S#state.client_acks,
                                            Rid, S#state.last_processed),
    NS = maybe_trim_cache(Ack, S),
    case Action of
        noreport ->
            process_stream_event(EventTag, Body, SName, NS#state{rid = Rid});
        report ->
            NS2 = schedule_report(Ack, NS),
            NS3 = process_stream_event(EventTag, Body, SName,
                                       NS2#state{rid = Rid}),
            maybe_send_report(NS3)
    end.

determine_report_action(undefined, false, _, _) ->
    {noreport, undefined};
determine_report_action(undefined, true, Rid, LastProcessed) ->
    if
        Rid+1 == LastProcessed ->
            {noreport, undefined};
        Rid+1 /= LastProcessed ->
            ?WARNING_MSG("expected 'ack' attribute on ~p~n", [Rid]),
            {noreport, undefined}
    end;
determine_report_action(BAck, _, _, LastProcessed) ->
    Ack = binary_to_integer(BAck),
    case {LastProcessed, is_valid_ack(Ack, LastProcessed)} of
        {undefined, _} ->
            {noreport, Ack};
        {_, true} ->
            {noreport, Ack};
        {_, false} ->
            {report, Ack}
    end.

is_valid_ack(Ack, LastProcessed)
        when Ack < LastProcessed ->
    false;
is_valid_ack(_, _) ->
    true.

maybe_trim_cache(undefined, S) ->
    S;
maybe_trim_cache(Ack, S) ->
    UpToAck = fun({R,_,_}) when R =< Ack ->
                    true;
                 (_) ->
                    false
              end,
    NewSent = lists:dropwhile(UpToAck, S#state.sent),
    S#state{sent = NewSent}.

schedule_report(Ack, #state{sent = Sent} = S) ->
    ReportRid = Ack + 1,
    try
        {resp,
         {ReportRid, TimeSent, _}} = {resp, lists:keyfind(ReportRid, 1, Sent)},
        ElapsedTimeMillis = erlang:round(timer:now_diff(now(), TimeSent)
                                         / 1000),
        Report = {ReportRid, ElapsedTimeMillis},
        case S#state.report of
            false ->
                S#state{report = Report};
            OldReport when OldReport < Report ->
                S#state{report = OldReport};
            _ ->
                S#state{report = Report}
        end
    catch
        error:{badmatch, {resp, false}} ->
            ?ERROR_MSG("no cached response for RID ~p, responses ~p~n",
                       [ReportRid, Sent]),
            S
    end.

maybe_send_report(#state{report = false} = S) ->
    S;
maybe_send_report(#state{} = S) ->
    send_or_store([], S).

process_stream_event(pause, Body, SName, State) ->
    Seconds = binary_to_integer(exml_query:attr(Body, <<"pause">>)),
    NewState = process_pause_event(Seconds, State),
    process_deferred_events(SName, NewState);
process_stream_event(EventTag, Body, SName, #state{c2s_pid = C2SPid} = State) ->
    {Els, NewState} = bosh_unwrap(EventTag, Body, State),
    [forward_to_c2s(C2SPid, El) || El <- Els],
    process_deferred_events(SName, NewState).

process_pause_event(Seconds, #state{maxpause = MaxPause} = S)
        when MaxPause == undefined;
             Seconds > MaxPause ->
    [Pid ! policy_violation || {_, _, Pid} <- S#state.handlers],
    throw({invalid_pause, S#state{handlers = []}});
process_pause_event(Seconds, State) ->
    NS = State#state{inactivity = Seconds},
    F = fun(_, S) ->
            send_to_handler([], S)
    end,
    lists:foldl(F, NS, lists:seq(1, length(State#state.handlers))).

process_deferred_events(SName, #state{deferred = Deferred} = S) ->
    lists:foldl(fun(Event, State) ->
                    ?DEBUG("processing deferred event: ~p~n", [Event]),
                    handle_stream_event(Event, none, SName, State)
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
send_or_store(Data, State) when not is_list(Data) ->
    send_or_store([Data], State);
send_or_store(Data, #state{handlers = []} = S) ->
    store(Data, S);
send_or_store(Data, State) ->
    send_to_handler(Data, State).

%% send_to_handler() assumes that Handlers is not empty!
%% Be sure that's the case if calling it.
send_to_handler(Data, State) ->
    {Handler, NS} = pick_handler(State),
    send_to_handler(Handler, Data, NS).

%% Return handler and new state if a handler is available
%% or `false` otherwise.
-spec pick_handler(#state{}) -> {{rid(), pid()}, #state{}} | false.
pick_handler(#state{handlers = []}) ->
    false;
pick_handler(#state{handlers = Handlers} = S) ->
    [{Rid, TRef, Pid} | HRest] = lists:sort(Handlers),
    %% The cancellation might fail if the timer already fired.
    %% Don't worry, it's handled on receiving the timeout message.
    timer:cancel(TRef),
    {{Rid, Pid}, S#state{handlers = HRest}}.

send_to_handler({_, Pid}, #xmlel{name = <<"body">>} = Wrapped, State) ->
    send_wrapped_to_handler(Pid, Wrapped, State);
send_to_handler({Rid, Pid}, Data, State) ->
    {Wrapped, NS} = bosh_wrap(Data, Rid, State),
    NS2 = cache_response({Rid, now(), Wrapped}, NS),
    send_wrapped_to_handler(Pid, Wrapped, NS2).

%% This is the most specific variant of send_to_handler()
%% and the *only one* actually performing a send
%% to the cowboy_loop_handler serving a HTTP request.
send_wrapped_to_handler(Pid, Wrapped, State) ->
    Pid ! {bosh_reply, Wrapped},
    setup_inactivity_timer(State).

maybe_ack(HandlerRid, #state{rid = Rid} = S) ->
    if
        Rid > HandlerRid ->
            server_ack(S#state.server_acks, Rid);
        Rid =< HandlerRid ->
            []
    end.

maybe_report(#state{report = false} = S) ->
    {[], S};
maybe_report(#state{report = Report} = S) ->
    {ReportRid, ElapsedTime} = Report,
    NewAttrs = [{<<"report">>, integer_to_binary(ReportRid)},
                {<<"time">>, integer_to_binary(ElapsedTime)}],
    {NewAttrs, S#state{report = false}}.

cache_response({Rid,_,_} = Response, #state{sent = Sent} = S) ->
    NewSent = elists:insert(Response, Sent),
    CacheUpTo = case S#state.client_acks of
        true ->
            %% Acknowledgements are on - there's no limit on the number
            %% of cached responses.
            infinity;
        false ->
            %% Leave up to ?CONCURRENT_REQUESTS responses in cache.
            ?CONCURRENT_REQUESTS
    end,
    S#state{sent = cache_up_to(CacheUpTo, NewSent),
            last_processed = last_processed(Rid, S#state.last_processed)}.

cache_up_to(infinity, Responses) ->
    Responses;
cache_up_to(N, Responses) ->
    lists:nthtail(max(0, length(Responses) - N), Responses).

last_processed(Rid, undefined) ->
    Rid;
last_processed(Rid1, Rid2) ->
    max(Rid1, Rid2).

setup_inactivity_timer(#state{inactivity = infinity} = S) ->
    S;
setup_inactivity_timer(S) ->
    cancel_inactivity_timer(S),
    {ok, TRef} = timer:send_after(timer:seconds(S#state.inactivity),
                                  inactivity_timeout),
    S#state{inactivity_tref = TRef}.

cancel_inactivity_timer(#state{inactivity_tref = undefined} = S) ->
    S;
cancel_inactivity_timer(S) ->
    timer:cancel(S#state.inactivity_tref),
    S#state{inactivity_tref = undefined}.

%% Store data for sending later.
store(Data, #state{pending = Pending} = S) ->
    S#state{pending = Pending ++ Data}.

forward_to_c2s(C2SPid, StreamElement) ->
    gen_fsm:send_event(C2SPid, StreamElement).

maybe_add_handler(Handler, Rid, S) when is_pid(Handler) ->
    add_handler({Rid, Handler}, S);
maybe_add_handler(_, _, S) ->
    S.

add_handler({Rid, Pid}, #state{handlers = Handlers} = S) ->
    {ok, TRef} = timer:send_after(timer:seconds(S#state.wait),
                                  {wait_timeout, {Rid, Pid}}),
    S#state{handlers = [{Rid, TRef, Pid} | Handlers]}.

%% Keep in mind the hardcoding for hold == 1.
return_surplus_handlers(accumulate, #state{handlers = []} = State) ->
    State;
return_surplus_handlers(accumulate, #state{handlers = [_]} = State) ->
    State;
return_surplus_handlers(accumulate, #state{handlers = _} = S) ->
    NS = send_to_handler([], S),
    return_surplus_handlers(accumulate, NS);
return_surplus_handlers(normal, #state{handlers = []} = State) ->
    State;
return_surplus_handlers(normal, #state{handlers = [_], pending = []} = State) ->
    State;
return_surplus_handlers(normal, #state{pending = Pending} = S) ->
    NS = send_or_store(Pending, S#state{pending = []}),
    return_surplus_handlers(normal, NS).

-spec bosh_unwrap(EventTag, #xmlel{}, #state{})
    -> {[StreamEvent], #state{}}
    when EventTag :: event_type(),
         StreamEvent :: #xmlstreamstart{}
                     | {xmlstreamelement, #xmlel{}}
                     | #xmlstreamend{}.
bosh_unwrap(StreamEvent, Body, #state{} = S)
       when StreamEvent =:= streamstart;
            StreamEvent =:= restart ->
    Wait = get_attr(<<"wait">>, Body, S#state.wait),
    Hold = get_attr(<<"hold">>, Body, S#state.hold),
    ClientAcks = get_client_acks(StreamEvent, Body, S#state.client_acks),
    E = stream_start(exml_query:attr(Body, <<"from">>),
                     exml_query:attr(Body, <<"to">>)),
    {[E], record_set(S, [{#state.wait, Wait},
                         {#state.hold, Hold},
                         {#state.client_acks, ClientAcks}])};
bosh_unwrap(streamend, Body, State) ->
    {Els, NewState} = bosh_unwrap(normal, Body, State),
    {Els ++ [#xmlstreamend{name = <<>>}], NewState};
bosh_unwrap(normal, Body, #state{sid = Sid} = State) ->
    Sid = exml_query:attr(Body, <<"sid">>),
    ?NS_HTTPBIND = exml_query:attr(Body, <<"xmlns">>),
    {[{xmlstreamelement, El}
      || El <- Body#xmlel.children,
         %% Ignore whitespace keepalives.
         El /= #xmlcdata{content = <<" ">>}],
     State}.

get_client_acks(restart, _, Default) ->
    Default;
get_client_acks(streamstart, Element, Default) ->
    case exml_query:attr(Element, <<"ack">>) of
        undefined ->
            Default;
        <<"1">> ->
            true;
        _ ->
            ?INFO_MSG("ignoring invalid client ack on stream start~n", []),
            false
    end.

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

bosh_wrap(Elements, Rid, #state{} = S) ->
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
             S#state{pending = Pending ++ [StreamEnd]}}
    end,
    MaybeAck = maybe_ack(Rid, NS),
    {MaybeReport, NNS} = maybe_report(NS),
    ExtraAttrs = MaybeAck ++ MaybeReport,
    {Body#xmlel{attrs = Body#xmlel.attrs ++ ExtraAttrs,
                children = Children}, NNS}.

is_stream_event(#xmlstreamstart{}) ->
    true;
is_stream_event(#xmlstreamend{}) ->
    true;
is_stream_event(_) ->
    false.

%% Bosh body for a session creation response.
bosh_stream_start_body(#xmlstreamstart{attrs = Attrs}, #state{} = S) ->
    #xmlel{name = <<"body">>,
           attrs = [{<<"wait">>, integer_to_binary(S#state.wait)},
                    {<<"requests">>,
                     integer_to_binary(?CONCURRENT_REQUESTS)},
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
           server_ack(S#state.server_acks, S#state.rid),
           children = []}.

inactivity(I) ->
    [{<<"inactivity">>, integer_to_binary(I)} || is_integer(I)].

maxpause(MP) ->
    [{<<"maxpause">>, integer_to_binary(MP)} || is_integer(MP)].

server_ack(ServerAcks, Rid) ->
    [{<<"ack">>, integer_to_binary(Rid)} || ServerAcks =:= true].

%% Bosh body for an ordinary stream element(s).
bosh_body(#state{} = S) ->
    #xmlel{name = <<"body">>,
           attrs = [{<<"sid">>, S#state.sid},
                    {<<"xmlns">>, ?NS_HTTPBIND}],
           children = []}.

bosh_stream_end_body() ->
    #xmlel{name = <<"body">>,
           attrs = [{<<"type">>, <<"terminate">>},
                    {<<"xmlns">>, ?NS_HTTPBIND}],
           children = []}.

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

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

cache_up_to_test_() ->
    [?_test(?assertEqual( [4,5], cache_up_to(2, [1,2,3,4,5]) ))].

-endif.
