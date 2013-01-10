-module(mod_bosh_socket).

-behaviour(gen_server).

%% API
-export([start/0,
         start_supervisor/0]).

%% ejabberd_socket compatibility
-export([starttls/2, starttls/3,
         compress/1, compress/2,
         %reset_stream/1,
         %send/2,
         %send_xml/2,
         %change_shaper/2,
         monitor/1,
         get_sockmod/1,
         close/1,
         peername/1
        ]).

%% gen_server callbacks
-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ejabberd.hrl").
-include("mod_bosh.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start() ->
    supervisor:start_child(?BOSH_SOCKET_SUP, []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

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

init([]) ->
    ?DEBUG("mod_bosh_socket started~n", []),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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

%send_xml(SocketData, {xmlstreamraw, Text}) ->
%    send(SocketData, Text);
%send_xml(SocketData, {xmlstreamelement, XML}) ->
%    send_xml(SocketData, XML);
%send_xml(SocketData, XML) ->
%    Text = exml:to_iolist(XML),
%    send(SocketData, Text).

%send(#websocket{pid = Pid}, Data) ->
%    Pid ! {send, Data},
%    ok.

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
