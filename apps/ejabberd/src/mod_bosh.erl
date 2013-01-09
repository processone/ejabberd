%%%===================================================================
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Cowboy based BOSH support for MongooseIM
%%% @end
%%%===================================================================
-module(mod_bosh).
-behaviour(gen_mod).
-behaviour(cowboy_loop_handler).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% ejabberd independent listener callbacks
-export([socket_type/0,
         start_listener/2]).

%% cowboy_loop_handler callbacks
-export([init/3,
         info/3,
         terminate/2]).

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
-define(BOSH_BACKEND, (mod_bosh_backend:backend())).
-define(DEFAULT_PORT, 5280).
-define(DEFAULT_BACKEND, redis).
-define(INACTIVITY_TIMEOUT, 120000).  %% 2 minutes

-record(websocket, {pid :: pid(),
                    peername :: string()}).
%-record(ws_state, {c2s_pid :: pid(),
		   %parser :: exml_stream:parser()}).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

start(_Host, Opts) ->
    Port = gen_mod:get_opt(port, Opts, ?DEFAULT_PORT),
    case start_cowboy(Port, Opts) of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {ok, Pid} ->
            load_backend(Opts),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_Host) ->
    cowboy:stop_listener(?LISTENER).

%%--------------------------------------------------------------------
%% ejabberd independent listener callbacks
%%--------------------------------------------------------------------

socket_type() ->
    independent.

start_listener({Port, _InetAddr, tcp}, Opts) ->
    OptsWPort = lists:keystore(port, 1, Opts, {port, Port}),
    gen_mod:start_module(?MYNAME, ?MODULE, OptsWPort).

%%--------------------------------------------------------------------
%% cowboy_loop_handler callbacks
%%--------------------------------------------------------------------

init(Transport, Req, Opts) ->
    ?DEBUG("cowboy init: ~p~n", [{Transport, Req, Opts}]),
    {Method, Req2} = cowboy_req:method(Req),
    {Echo, Req3} = cowboy_req:qs_val(<<"echo">>, Req2),
    {ok, _TRef} = timer:send_after(5000, {reply, echo(Method, Echo)}),
    {loop, Req3, undefined_state, ?INACTIVITY_TIMEOUT, hibernate}.

info({reply, {405 = Code, _Headers, _Body}}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(Code, Req),
    {ok, Req2, State};
info({reply, {Code, Headers, Body}}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
    {ok, Req2, State};
info(Message, Req, State) ->
    ?DEBUG("Received (loop): ~w~n", [Message]),
    {loop, Req, State, hibernate}.

terminate(_Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

start_cowboy(Port, Opts) ->
    Host = '_',
    %Prefix = <<"http-bind">>,
    %DispatchRules = [{[Prefix], ?MODULE, Opts}],
    DispatchRules = [{[], ?MODULE, Opts}],
    FullDispatch = [{Host, DispatchRules}],
    %NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 100),
    NumAcceptors = proplists:get_value(num_acceptors, Opts, 100),
    TransportOpts = [{port, Port}],
    ProtocolOpts = [{dispatch, FullDispatch}],
    %% TODO: since cowboy commit 1b3f510b7e this is required
    %ProtocolOpts = [{env, [{dispatch, FullDispatch}]}],
    cowboy:start_http(?LISTENER, NumAcceptors,
		      TransportOpts, ProtocolOpts).

load_backend(Opts) ->
    Backend = proplists:get_value(backend, Opts, ?DEFAULT_BACKEND),
    {Mod, Code} = dynamic_compile:from_string(mod_bosh_backend_src(Backend)),
    code:load_binary(Mod, "mod_bosh_backend.erl", Code).

echo(<<"GET">>, undefined) ->
    {400, [], <<"Missing echo parameter.">>};
echo(<<"GET">>, Echo) ->
    {200, [{<<"content-encoding">>, <<"utf-8">>}], Echo};
echo(_, _) ->
    %% Method not allowed.
    {405, undefined, undefined}.

%%--------------------------------------------------------------------
%% ejabberd_socket compatibility
%%--------------------------------------------------------------------

%% TODO: adjust for BOSH

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

-spec mod_bosh_backend_src(atom()) -> string().
mod_bosh_backend_src(Backend) ->
    lists:flatten(
      ["-module(mod_bosh_backend).
        -export([backend/0]).

        -spec backend() -> atom().
        backend() ->
            mod_bosh_", atom_to_list(Backend), ".\n"]).
