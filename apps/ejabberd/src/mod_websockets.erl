%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Module providing support for websockets in ejabberd
%%% @end
%%%===================================================================
-module(mod_websockets).
-behaviour(gen_mod).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

%% cowboy_http_websocket_handler callbacks
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

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

-include("ejabberd.hrl").
-include_lib("exml/include/exml_stream.hrl").

-define(LISTENER, ?MODULE).
-define(DEFAULT_PORT, 5288).

-record(websocket, {pid :: pid(),
                    peername :: string()}).
-record(ws_state, {c2s_pid :: pid(),
                   parser :: exml_stream:parser()}).


%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

start(_Host, Opts) ->
    WSHost = gen_mod:get_opt(host, Opts, '_'), %% default to any
    WSPrefix = gen_mod:get_opt(prefix, Opts, "/ws-xmpp"),
    NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 100),
    Port = gen_mod:get_opt(port, Opts, ?DEFAULT_PORT),
    Dispatch = cowboy_router:compile([{WSHost, [{WSPrefix, ?MODULE, Opts}] }]),
    case cowboy:start_http(?LISTENER, NumAcceptors,
                                [{port, Port}],
                                [{env, [{dispatch, Dispatch}]}]) of
        {error, {already_started, _Pid}} ->
            ok;
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

stop(_Host) ->
    cowboy:stop_listener(?LISTENER).

%%--------------------------------------------------------------------
%% cowboy_http_handler callbacks
%%--------------------------------------------------------------------

init(Transport, Req, Opts) ->
    ?DEBUG("cowboy init: ~p~n", [{Transport, Req, Opts}]),
    {upgrade, protocol, cowboy_websocket}.

handle(Req, State) ->
        {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
        ok.

%%--------------------------------------------------------------------
%% cowboy_http_websocket_handler callbacks
%%--------------------------------------------------------------------

% Called for every new websocket connection.
websocket_init(Transport, Req, Opts) ->
    ?DEBUG("websocket_init: ~p~n", [{Transport, Req, Opts}]),
    {Peer, NewReq} = cowboy_req:peer(Req),
    SocketData = #websocket{pid=self(),
                            peername = Peer},
    case ejabberd_c2s:start({?MODULE, SocketData}, Opts) of
        {ok, Pid} ->
            ?DEBUG("started c2s via websockets: ~p", [Pid]),
            {ok, Parser} = exml_stream:new_parser(),
            State = #ws_state{c2s_pid = Pid,
                              parser = Parser},
            {ok, NewReq, State};
        {error, Reason} ->
            ?WARNING_MSG("c2s start failed: ~p", [Reason]),
            {shutdown, NewReq}
    end.


% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    ?INFO_MSG("Received: ~p", [Msg]),
    {ok, NewState} = handle_text(Msg, State),
    {ok, Req, NewState};

websocket_handle({binary, Msg}, Req, State) ->
    ?INFO_MSG("Received binary: ~p", [Msg]),
    {ok, NewState} = handle_text(Msg, State),
    {ok, Req, NewState};

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Any, Req, State) ->
    ?INFO_MSG("Received non-text: ~p", [Any]),
    {ok, Req, State}.

% Other messages from the system are handled here.
websocket_info({send, Text}, Req, State) ->
    {reply, {text, Text}, Req, State};
websocket_info(reset_stream, Req, #ws_state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {ok, Req, State#ws_state{parser = NewParser}};
websocket_info(stop, Req, #ws_state{parser = Parser} = State) ->
    exml_stream:free_parser(Parser),
    {shutdown, Req, State};
websocket_info(Info, Req, State) ->
    ?INFO_MSG("unknown info: ~p", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

handle_text(Text, #ws_state{c2s_pid = C2S, parser = Parser} = State) ->
    {ok, NewParser, Elements} = exml_stream:parse(Parser, Text),
    [send_to_c2s(C2S, Elem) || Elem <- Elements],
    {ok, State#ws_state{parser = NewParser}}.

send_to_c2s(C2S, #xmlelement{} = Element) ->
    send_to_c2s(C2S, {xmlstreamelement, Element});
send_to_c2s(C2S, StreamElement) ->
    gen_fsm:send_event(C2S, StreamElement).

%%--------------------------------------------------------------------
%% ejabberd_socket compatibility
%%--------------------------------------------------------------------

starttls(SocketData, TLSOpts) ->
    starttls(SocketData, TLSOpts, <<>>).

starttls(_SocketData, _TLSOpts, _Data) ->
    throw({error, tls_not_allowed_on_websockets}).

compress(SocketData) ->
    compress(SocketData, <<>>).

compress(_SocketData, _Data) ->
    throw({error, compression_not_allowed_on_websockets}).

reset_stream(#websocket{pid = Pid} = SocketData) ->
    Pid ! reset_stream,
    SocketData.

send_xml(SocketData, {xmlstreamraw, Text}) ->
    send(SocketData, Text);
send_xml(SocketData, {xmlstreamelement, XML}) ->
    send_xml(SocketData, XML);
send_xml(SocketData, XML) ->
    Text = exml:to_iolist(XML),
    send(SocketData, Text).

send(#websocket{pid = Pid}, Data) ->
    Pid ! {send, Data},
    ok.

change_shaper(SocketData, _Shaper) ->
    SocketData. %% TODO: we ignore shapers for now

monitor(#websocket{pid = Pid}) ->
    erlang:monitor(process, Pid).

get_sockmod(_SocketData) ->
    ?MODULE.

close(#websocket{pid = Pid}) ->
    Pid ! close.

peername(#websocket{peername = PeerName}) ->
    PeerName.
