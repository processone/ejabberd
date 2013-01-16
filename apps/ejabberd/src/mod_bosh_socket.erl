-module(mod_bosh_socket).

-behaviour(gen_server).

%% API
-export([start/2,
         start_supervisor/0]).

%% ejabberd_socket compatibility
-export([starttls/2, starttls/3,
         compress/1, compress/2,
         %reset_stream/1,
         send/2,
         send_xml/2,
         %change_shaper/2,
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
-include_lib("exml/include/exml_stream.hrl").
-include("mod_bosh.hrl").

-define(SERVER, ?MODULE).

-record(state, {c2s_pid :: pid(),
                requests = [] :: [pid()],
                pending = []}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Sid, Peer) ->
    supervisor:start_child(?BOSH_SOCKET_SUP, [Sid, Peer]).

start_link(Sid, Peer) ->
    gen_server:start_link(?MODULE, [Sid, Peer], [{debug, [trace]}]).

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

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Sid, Peer]) ->
    BoshSocket = #bosh_socket{sid = Sid, pid = self(), peer = Peer},
    %% TODO: C2SOpts probably shouldn't be empty
    C2SOpts = [{xml_socket, true}],
    {ok, C2SPid} = ejabberd_c2s:start({mod_bosh_socket, BoshSocket}, C2SOpts),
    ?DEBUG("mod_bosh_socket started~n", []),
    {ok, #state{c2s_pid = C2SPid}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(#xmlstreamstart{} = El, #state{c2s_pid = C2SPid} = S) ->
    forward_to_c2s(C2SPid, El),
    {noreply, S};
handle_cast(#xmlelement{} = El, #state{c2s_pid = C2SPid} = S) ->
    forward_to_c2s(C2SPid, El),
    {noreply, S};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({newrequest, R}, #state{requests = Rs, pending = []} = S) ->
    {noreply, S#state{requests = [R | Rs]}};
handle_info({newrequest, R}, #state{pending = Pending} = S) ->
    R ! {send, iolist_to_binary(lists:reverse(Pending))},
    {noreply, S#state{pending = []}};
handle_info({send, Data}, #state{requests = [], pending = Pending} = S) ->
    {noreply, S#state{pending = [Data | Pending]}};
handle_info({send, Data}, #state{requests = [R | Rs]} = S) ->
    R ! {send, Data},
    {noreply, S#state{requests = Rs}};
handle_info({close, Sid}, State) ->
    %% TODO: kill waiting request handlers
    ?BOSH_BACKEND:delete_session(Sid),
    ?DEBUG("mod_bosh_socket closing~n", []),
    {stop, normal, State};
handle_info(_Info, State) ->
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
%send_xml(Socket, {xmlstreamelement, XML}) ->
    %send_xml(Socket, XML);
send_xml(Socket, #xmlstreamstart{} = XML) ->
    send(Socket, XML).
%send_xml(Socket, XML) ->
%    Text = exml:to_iolist(XML),
%    send(Socket, Text).

send(#bosh_socket{pid = Pid}, Data) ->
    Pid ! {send, Data},
    ok.

%change_shaper(SocketData, _Shaper) ->
    %SocketData. %% TODO: we ignore shapers for now

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
