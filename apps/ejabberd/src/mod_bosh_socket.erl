-module(mod_bosh_socket).

-behaviour(gen_server).

%% API
-export([start/2,
         start_supervisor/0,
         add_request_handler/2,
         send_to_c2s/2
        ]).

%% ejabberd_socket compatibility
-export([starttls/2, starttls/3,
         compress/1, compress/2,
         %reset_stream/1,
         send/2,
         send_xml/2,
         change_shaper/2,
         monitor/1,
         get_sockmod/1,
         close/1,
         peername/1
        ]).

%% gen_server callbacks
-export([start_link/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("mod_bosh.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_WAIT, 60).
-define(DEFAULT_HOLD, 1).
-define(DEFAULT_INACTIVITY, 30).

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
    gen_server:start_link(?MODULE, [Sid, Peer], []).

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
    gen_server:cast(Pid, {new_handler, HandlerPid}).

send_to_c2s(Pid, StreamElement) ->
    gen_server:cast(Pid, StreamElement).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Sid, Peer]) ->
    BoshSocket = #bosh_socket{sid = Sid, pid = self(), peer = Peer},
    %% TODO: C2SOpts probably shouldn't be empty
    C2SOpts = [{xml_socket, true}],
    {ok, C2SPid} = ejabberd_c2s:start({mod_bosh_socket, BoshSocket}, C2SOpts),
    ?DEBUG("mod_bosh_socket started~n", []),
    {ok, #state{sid = Sid, c2s_pid = C2SPid}}.

handle_call(Request, _From, State) ->
    ?DEBUG("Unhandled call: ~w~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(#xmlelement{name = <<"body">>} = Body,
            #state{c2s_pid = C2SPid} = S) ->
    {StreamStart, NS} = bosh_body_to_stream_start(Body, S),
    forward_to_c2s(C2SPid, StreamStart),
    {noreply, NS};
handle_cast(#xmlelement{} = El, #state{c2s_pid = C2SPid} = S) ->
    forward_to_c2s(C2SPid, El),
    {noreply, S};
handle_cast({new_handler, HandlerPid}, #state{} = S) ->
    NS = new_request_handler(HandlerPid, S),
    {noreply, NS};
handle_cast(Msg, State) ->
    ?DEBUG("Unhandled cast: ~w~n", [Msg]),
    {noreply, State}.

handle_info({send, Data}, #state{handlers = [], pending = Pending} = S) ->
    {noreply, S#state{pending = [Data | Pending]}};
handle_info({send, Data}, #state{handlers = [H | Hs]} = S) ->
    H ! {send, bosh_wrap([Data], S)},
    {noreply, S#state{handlers = Hs}};
handle_info({close, Sid}, State) ->
    %% TODO: kill waiting request handlers
    ?BOSH_BACKEND:delete_session(Sid),
    ?DEBUG("mod_bosh_socket closing~n", []),
    {stop, normal, State};
handle_info(Info, State) ->
    ?DEBUG("Unhandled info: ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% callback implementations
%%--------------------------------------------------------------------

forward_to_c2s(C2SPid, StreamElement) ->
    gen_fsm:send_event(C2SPid, StreamElement).

new_request_handler(Pid, #state{pending = [], handlers = Handlers} = S) ->
    S#state{handlers = [Pid | Handlers]};
new_request_handler(Pid, #state{pending = Pending} = S) ->
    NS = S#state{pending = []},
    Pid ! {send, bosh_wrap(Pending, NS)},
    NS.

bosh_body_to_stream_start(Body, #state{} = S) ->
    Wait = get_wait(exml_query:attr(Body, <<"wait">>)),
    Hold = get_hold(exml_query:attr(Body, <<"hold">>)),
    Rid = exml_query:attr(Body, <<"rid">>),
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

bosh_wrap(Elements, #state{} = S) ->
    {Body, Children} = case lists:partition(fun is_stream_start/1, Elements) of
        {[], Stanzas} ->
            {bosh_body(S), Stanzas};
        {[StreamStart], Stanzas} ->
            {bosh_stream_start_body(StreamStart, S), Stanzas}
    end,
    Body#xmlelement{children = Children}.

bosh_stream_start_body(#xmlstreamstart{attrs = Attrs}, #state{} = S) ->
    #xmlelement{name = <<"body">>,
                attrs = [{<<"wait">>, integer_to_binary(S#state.wait)},
                         {<<"inactivity">>,
                          integer_to_binary(S#state.inactivity)},
                         %% TODO: don't use polling for now, decide later
                         %{<<"polling">>, <<"5">>},
                         {<<"requests">>, <<"2">>},
                         {<<"hold">>, integer_to_binary(S#state.hold)},
                         {<<"from">>, proplists:get_value(<<"from">>, Attrs)},
                         {<<"accept">>, <<"deflate,gzip">>},
                         {<<"sid">>, integer_to_binary(S#state.hold)},
                         {<<"secure">>, <<"true">>},
                         {<<"charsets">>, <<"ISO_8859-1 ISO-2022-JP">>},
                         {<<"xmpp:restartlogic">>, <<"true">>},
                         {<<"xmpp:version">>, <<"1.0">>},
                         %% TODO: what's it for?
                         %{<<"authid">>, <<"ServerStreamID">>},
                         {<<"xmlns">>, ?NS_HTTPBIND},
                         {<<"xmlns:xmpp">>, <<"urn:xmpp:xbosh">>},
                         {<<"xmlns:stream">>, ?NS_STREAM}],
                children = []}.

bosh_body(#state{} = S) ->
    #xmlelement{name = <<"body">>,
                attrs = [{<<"rid">>, integer_to_binary(S#state.rid)},
                         {<<"sid">>, integer_to_binary(S#state.sid)},
                         {<<"xmlns">>, ?NS_HTTPBIND}],
                children = []}.

is_stream_start(#xmlstreamstart{}) ->
    true;
is_stream_start(_) ->
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

%reset_stream(#websocket{pid = Pid} = SocketData) ->
%    Pid ! reset_stream,
%    SocketData.

%send_xml(Socket, {xmlstreamraw, Text}) ->
%    send(Socket, Text);
send_xml(Socket, {xmlstreamelement, XML}) ->
    send(Socket, XML);
send_xml(Socket, #xmlstreamstart{} = XML) ->
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

close(#bosh_socket{sid = Sid, pid = Pid}) ->
    Pid ! {close, Sid}.

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
