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

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("mod_bosh.hrl").

-define(LISTENER, ?MODULE).
-define(DEFAULT_PORT, 5280).
-define(DEFAULT_BACKEND, mnesia).
-define(INACTIVITY_TIMEOUT, 120000).  %% 2 minutes

%% Request State
-record(rstate, {body,
                 sid :: bosh_sid()}).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

start(_Host, Opts) ->
    Port = gen_mod:get_opt(port, Opts, ?DEFAULT_PORT),
    try
        ok = start_cowboy(Port, Opts),
        ok = start_backend(Opts),
        {ok, _Pid} = mod_bosh_socket:start_supervisor()
    catch
        error:{badmatch, ErrorReason} ->
            ErrorReason
    end.

stop(_Host) ->
    %% TODO: stop backend and supervisor
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
    ?DEBUG("New request: ~w~n", [{Transport, Req, Opts}]),
    {Msg, NewReq} = try
        {<<"POST">>, Req2} = cowboy_req:method(Req),
        {true, Req3} = cowboy_req:has_body(Req2),
        {process_body, Req3}
    catch
        %% In order to issue a reply, init() must accept the request for processing.
        %% Hence, handling of these errors is forwarded to info().
        error:{badmatch, {false, NReq}} ->
            {no_body, NReq};
        error:{badmatch, {Method, NReq}} when is_binary(Method) ->
            {wrong_method, NReq}
    end,
    self() ! Msg,
    {loop, NewReq, #rstate{}}.

info(no_body, Req, State) ->
    ?DEBUG("Missing request body: ~w~n", [Req]),
    {ok, no_body_error(Req), State};
info(wrong_method, Req, State) ->
    ?DEBUG("Wrong request method: ~w~n", [Req]),
    {ok, method_not_allowed_error(Req), State};
info(process_body, Req, S) ->
    %?DEBUG("Loop on request: ~w~n", [{Req, S}]),
    {ok, Body, Req1} = cowboy_req:body(Req),
    %% TODO: the parser should be stored per session,
    %%       but the session is identified inside the to-be-parsed element
    {ok, BodyElem} = exml:parse(Body),
    process_body(Req1, S#rstate{body=BodyElem}).

terminate(_Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

start_cowboy(Port, Opts) ->
    Host = '_',
    Prefix = <<"http-bind">>,
    DispatchRules = [{[Prefix], ?MODULE, Opts}],
    FullDispatch = [{Host, DispatchRules}],
    %NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 100),
    NumAcceptors = proplists:get_value(num_acceptors, Opts, 100),
    TransportOpts = [{port, Port}],
    ProtocolOpts = [{dispatch, FullDispatch}],
    %% TODO: since cowboy commit 1b3f510b7e this is required
    %ProtocolOpts = [{env, [{dispatch, FullDispatch}]}],
    case cowboy:start_http(?LISTENER, NumAcceptors,
                           TransportOpts, ProtocolOpts) of
        {error, {already_started, _Pid}} ->
            ok;
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

start_backend(Opts) ->
    %% TODO: there's completely no error checking here
    Backend = proplists:get_value(backend, Opts, ?DEFAULT_BACKEND),
    {Mod, Code} = dynamic_compile:from_string(mod_bosh_dynamic_src(Backend)),
    code:load_binary(Mod, "mod_bosh_dynamic.erl", Code),
    ?BOSH_BACKEND:start(Opts),
    ok.

process_body(Req, #rstate{body=#xmlelement{attrs=Attrs} = Body} = S) ->
    case exml_query:attr(Body, <<"sid">>) of
        undefined ->
            start_session(Req, S);
        Sid ->
            %% TODO: get session from BACKEND, for el in body.children(): self() ! el
            {ok, Req, S}
    end.

start_session(Req, #rstate{body=Body} = S) ->
    Sid = make_sid(),
    {ok, Socket} = mod_bosh_socket:start(),
    {Peer, Req1} = cowboy_req:peer(Req),
    BoshSocket = #bosh_socket{sid = Sid, pid = Socket, peer = Peer},
    %% TODO: C2SOpts probably shouldn't be empty
    C2SOpts = [],
    {ok, C2SPid} = ejabberd_c2s:start({mod_bosh_socket, BoshSocket}, C2SOpts),
    BoshSession = #bosh_session{sid = Sid, c2s_pid = C2SPid},
    ?BOSH_BACKEND:create_session(BoshSession),
    %% TODO: send proper reply
    {ok, not_implemented_error(Req1), S}.

make_sid() ->
    list_to_binary(sha:sha(term_to_binary({now(), make_ref()}))).

send_to_c2s(C2S, #xmlelement{} = Element) ->
    send_to_c2s(C2S, {xmlstreamelement, Element});
send_to_c2s(C2S, StreamElement) ->
    gen_fsm:send_event(C2S, StreamElement).

body_to_stream_start(Body) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = [{<<"from">>, exml_query:attr(Body, <<"from">>)},
                             {<<"to">>, exml_query:attr(Body, <<"to">>)},
                             {<<"version">>, <<"1.0">>},
                             {<<"xml:lang">>, <<"en">>},
                             {<<"xmlns">>, <<"jabber:client">>},
                             {<<"xmlns:stream">>, ?NS_STREAM}]}.

%%--------------------------------------------------------------------
%% HTTP errors
%%--------------------------------------------------------------------

no_body_error(Req) ->
    strip_ok(cowboy_req:reply(400, [], <<"Missing request body">>, Req)).

method_not_allowed_error(Req) ->
    strip_ok(cowboy_req:reply(405, [], <<"Use POST request method">>, Req)).

not_implemented_error(Req) ->
    strip_ok(cowboy_req:reply(400, [], <<"Not implemented yet">>, Req)).

strip_ok({ok, Req}) ->
    Req.

%%--------------------------------------------------------------------
%% Backend configuration
%%--------------------------------------------------------------------

-spec mod_bosh_dynamic_src(atom()) -> string().
mod_bosh_dynamic_src(Backend) ->
    lists:flatten(
      ["-module(mod_bosh_dynamic).
        -export([backend/0]).

        -spec backend() -> atom().
        backend() ->
            mod_bosh_", atom_to_list(Backend), ".\n"]).
