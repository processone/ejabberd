-module(mod_bosh_socket).

-behaviour(gen_fsm).

%% API
-export([start/2,
         start_link/2,
         start_supervisor/0,
         add_request_handler/2,
         send_to_c2s/2
        ]).

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

-define(DEFAULT_WAIT, 60).
-define(DEFAULT_HOLD, 1).
-define(DEFAULT_INACTIVITY, 30).
-define(ACCUMULATE_PERIOD, 10).

-record(state, {c2s_pid :: pid(),
                handlers = [] :: [pid()],
                pending = [],

                sid :: bosh_sid(),
                wait,
                hold,
                rid,
                inactivity = ?DEFAULT_INACTIVITY}).

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

add_request_handler(Pid, HandlerPid) ->
    gen_fsm:send_event(Pid, {new_handler, HandlerPid}).

send_to_c2s(Pid, StreamElement) ->
    gen_fsm:send_all_state_event(Pid, StreamElement).

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
    %% TODO: C2SOpts probably shouldn't be empty
    C2SOpts = [{xml_socket, true}],
    {ok, C2SPid} = ejabberd_c2s:start({mod_bosh_socket, BoshSocket}, C2SOpts),
    ?DEBUG("mod_bosh_socket started~n", []),
    {ok, accumulate, #state{sid = Sid, c2s_pid = C2SPid}}.


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

accumulate({new_handler, HandlerPid}, #state{} = S) ->
    NS = new_request_handler(accumulate, HandlerPid, S),
    {next_state, accumulate, NS};
accumulate(acc_off, #state{pending = Pending} = S) ->
    NS = S#state{pending = []},
    {next_state, normal, send_or_store(Pending, NS)};
accumulate(Event, State) ->
    ?DEBUG("Unhandled event in 'accumulate' state: ~w~n", [Event]),
    {next_state, accumulate, State}.

normal({new_handler, HandlerPid}, #state{} = S) ->
    NS = new_request_handler(normal, HandlerPid, S),
    {next_state, normal, NS};
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

handle_event({StreamEvent, #xmlelement{} = Body},
             _StateName, #state{c2s_pid = C2SPid} = S)
       when StreamEvent == streamstart;
            StreamEvent == restart ->
    {StreamStart, NS} = bosh_body_to_stream_start(Body, S),
    forward_to_c2s(C2SPid, StreamStart),
    timer:apply_after(?ACCUMULATE_PERIOD,
                      gen_fsm, send_event, [self(), acc_off]),
    {next_state, accumulate, NS};
handle_event(#xmlelement{} = Body, StateName, #state{c2s_pid = C2SPid} = S) ->
    %% TODO: handle out-of-order requests
    {Els, NS} = bosh_unwrap(Body, S),
    [ forward_to_c2s(C2SPid, {xmlstreamelement, El}) || El <- Els ],
    {next_state, StateName, NS};
handle_event({streamend, #xmlelement{} = Body}, StateName,
             #state{c2s_pid = C2SPid} = S) ->
    {Els, NS} = bosh_unwrap(Body, S),
    [ forward_to_c2s(C2SPid, {xmlstreamelement, El}) || El <- Els ],
    forward_to_c2s(C2SPid, {xmlstreamend, []}),
    {next_state, StateName, NS};
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
handle_sync_event(get_handlers, _From, StateName, #state{handlers = Handlers} = S) ->
    {reply, Handlers, StateName, S};
handle_sync_event(get_pending, _From, StateName, #state{pending = Pending} = S) ->
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
handle_info(Info, SName, State) ->
    ?DEBUG("Unhandled info in '~s' state: ~w~n", [SName, Info]),
    {next_state, SName, State}.


terminate(_Reason, StateName, #state{sid = Sid, handlers = Handlers} = S) ->
    [ H ! {close, Sid} || H <- Handlers ],
    ?BOSH_BACKEND:delete_session(Sid),
    ?DEBUG("Closing session ~p in '~s' state. Handlers: ~p Pending: ~p~n",
           [Sid, StateName, Handlers, S#state.pending]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% callback implementations
%%--------------------------------------------------------------------

%% Send data to the client if any request handler is available.
%% Otherwise, store for sending later.
send_or_store(Data, #state{handlers = []} = S) ->
    store(Data, S);
send_or_store(Data, #state{} = S) when not is_list(Data) ->
    send_or_store([Data], S);
send_or_store([], #state{} = S) ->
    S;
send_or_store(Data, #state{handlers = [H | Hs]} = S) ->
    ?DEBUG("Forwarding element to handler. Handlers: ~p~n", [[H | Hs]]),
    {Wrapped, NS} = bosh_wrap(Data, S),
    H ! {send, Wrapped},
    NS#state{handlers = Hs}.

%% Store data for sending later.
store(Data, #state{pending = Pending} = S) ->
    S#state{pending = [Data | Pending]}.

forward_to_c2s(C2SPid, StreamElement) ->
    gen_fsm:send_event(C2SPid, StreamElement).

new_request_handler(accumulate, Pid, #state{handlers = Handlers} = S) ->
    S#state{handlers = [Pid | Handlers]};
new_request_handler(normal, Pid, #state{pending = [],
                                        handlers = Handlers} = S) ->
    S#state{handlers = [Pid | Handlers]};
new_request_handler(normal, Pid, #state{pending = Pending,
                                        handlers = Handlers} = S) ->
    NS = S#state{pending = [], handlers = [Pid | Handlers]},
    send_or_store(Pending, NS).

-spec bosh_body_to_stream_start(#xmlelement{}, #state{})
    -> {#xmlstreamstart{}, #state{}}.
bosh_body_to_stream_start(Body, #state{} = S) ->
    Wait = get_wait(exml_query:attr(Body, <<"wait">>)),
    Hold = get_hold(exml_query:attr(Body, <<"hold">>)),
    Rid = binary_to_integer(exml_query:attr(Body, <<"rid">>)),
    E = #xmlstreamstart{name = <<"stream:stream">>,
                        attrs = [{<<"from">>, exml_query:attr(Body, <<"from">>)},
                                 {<<"to">>, exml_query:attr(Body, <<"to">>)},
                                 {<<"version">>, <<"1.0">>},
                                 {<<"xml:lang">>, <<"en">>},
                                 {<<"xmlns">>, <<"jabber:client">>},
                                 {<<"xmlns:stream">>, ?NS_STREAM}]},
    {E, record_set(S, [{#state.wait, Wait},
                       {#state.hold, Hold},
                       {#state.rid, Rid}])}.

get_wait(BWait) ->
    get_attr(BWait, ?DEFAULT_WAIT).

get_hold(BHold) ->
    get_attr(BHold, ?DEFAULT_HOLD).

get_attr(undefined, Default) ->
    Default;
get_attr(BAttr, _Default) ->
    binary_to_integer(BAttr).

bosh_unwrap(Body, #state{sid = Sid} = S) ->
    %% TODO: verify these
    Rid = exml_query:attr(Body, <<"rid">>),
    Sid = exml_query:attr(Body, <<"sid">>),
    ?NS_HTTPBIND = exml_query:attr(Body, <<"xmlns">>),
    {Body#xmlelement.children, S#state{rid = Rid}}.

bosh_wrap(Elements, #state{} = S) ->
    {{Body, Children}, NS} = case lists:partition(fun is_stream_event/1, Elements) of
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
            Pending = S#state.pending,
            {{bosh_body(S), Stanzas}, S#state{pending = [StreamEnd, Pending]}}
    end,
    {Body#xmlelement{children = Children}, NS}.

%% Bosh body for a session creation response.
bosh_stream_start_body(#xmlstreamstart{attrs = Attrs}, #state{} = S) ->
    %% TODO: acks?
    #xmlelement{name = <<"body">>,
                attrs = [{<<"wait">>, integer_to_binary(S#state.wait)},
                         {<<"inactivity">>,
                          integer_to_binary(S#state.inactivity)},
                         {<<"requests">>, <<"2">>},
                         {<<"hold">>, integer_to_binary(S#state.hold)},
                         {<<"from">>, proplists:get_value(<<"from">>, Attrs)},
                         %% TODO: how to support these with cowboy?
                         {<<"accept">>, <<"deflate,gzip">>},
                         {<<"sid">>, S#state.sid},
                         {<<"xmpp:restartlogic">>, <<"true">>},
                         {<<"xmpp:version">>, <<"1.0">>},
                         %% TODO: what's it for?
                         %{<<"authid">>, <<"ServerStreamID">>},
                         {<<"xmlns">>, ?NS_HTTPBIND},
                         {<<"xmlns:xmpp">>, <<"urn:xmpp:xbosh">>},
                         {<<"xmlns:stream">>, ?NS_STREAM}],
                children = []}.

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

is_stream_event(#xmlstreamstart{}) ->
    true;
is_stream_event(#xmlstreamend{}) ->
    true;
is_stream_event(_) ->
    false.

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

%send_xml(Socket, {xmlstreamraw, Text}) ->
%    send(Socket, Text);
send_xml(Socket, {xmlstreamelement, XML}) ->
    send(Socket, XML);
send_xml(Socket, #xmlstreamstart{} = XML) ->
    send(Socket, XML);
send_xml(Socket, #xmlstreamend{} = XML) ->
    send(Socket, XML).
%send_xml(Socket, XML) ->
%    Text = exml:to_iolist(XML),
%    send(Socket, Text).

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
