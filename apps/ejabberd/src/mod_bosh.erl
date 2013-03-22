%%%===================================================================
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Cowboy based BOSH support for MongooseIM
%%% @end
%%%===================================================================
-module(mod_bosh).
-behaviour(gen_mod).
-behaviour(cowboy_loop_handler).

%% API
-export([get_inactivity/0,
         set_inactivity/1]).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% ejabberd independent listener callbacks
-export([socket_type/0,
         start_listener/2]).

%% cowboy_loop_handler callbacks
-export([init/3,
         info/3,
         terminate/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("mod_bosh.hrl").

-define(LISTENER, ?MODULE).
-define(DEFAULT_PORT, 5280).
-define(DEFAULT_BACKEND, mnesia).
-define(DEFAULT_MAX_AGE, 1728000).  %% 20 days in seconds

%% Request State
-record(rstate, {}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec get_inactivity() -> pos_integer() | infinity | undefined.
get_inactivity() ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, inactivity, undefined).

%% Return true if succeeded, false otherwise.
-spec set_inactivity(SecondsOrInfinity) -> boolean()
    when SecondsOrInfinity :: pos_integer() | infinity.
set_inactivity(infinity) ->
    gen_mod:set_module_opt(?MYNAME, ?MODULE, inactivity, infinity);
set_inactivity(Seconds) when is_integer(Seconds), Seconds > 0 ->
    gen_mod:set_module_opt(?MYNAME, ?MODULE, inactivity, Seconds).

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

init(_Transport, Req, _Opts) ->
    ?DEBUG("New request~n", []),
    {Msg, NewReq} = try
        {Method, Req2} = cowboy_req:method(Req),
        case Method of
            <<"OPTIONS">> ->
                {accept_options, Req2};
            <<"POST">> ->
                {has_body, true} = {has_body, cowboy_req:has_body(Req2)},
                {forward_body, Req2};
            _ ->
                error({badmatch, {Method, Req2}})
        end
    catch
        %% In order to issue a reply, init() must accept the request for processing.
        %% Hence, handling of these errors is forwarded to info().
        error:{badmatch, {has_body, false}} ->
            {no_body, Req};
        error:{badmatch, {WrongMethod, NReq}} when is_binary(WrongMethod) ->
            {{wrong_method, WrongMethod}, NReq}
    end,
    self() ! Msg,
    {loop, NewReq, #rstate{}}.

info(accept_options, Req, State) ->
    {Origin, Req2} = cowboy_req:header(<<"origin">>, Req),
    Headers = [ac_allow_origin(Origin),
               ac_allow_methods(),
               ac_allow_headers(),
               ac_max_age()],
    ?DEBUG("OPTIONS response: ~p~n", [Headers]),
    {ok, strip_ok(cowboy_req:reply(200, Headers, <<>>, Req2)), State};
info(no_body, Req, State) ->
    ?DEBUG("Missing request body: ~p~n", [Req]),
    {ok, no_body_error(Req), State};
info({wrong_method, Method}, Req, State) ->
    ?DEBUG("Wrong request method: ~p~n", [Method]),
    {ok, method_not_allowed_error(Req), State};
info(forward_body, Req, S) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    %% TODO: the parser should be stored per session,
    %%       but the session is identified inside the to-be-parsed element
    {ok, BodyElem} = exml:parse(Body),
    ?DEBUG("Parsed body: ~p~n", [BodyElem]),
    forward_body(Req1, BodyElem, S);
info({bosh_reply, El}, Req, S) ->
    BEl = exml:to_binary(El),
    ?DEBUG("Sending (binary) to ~p: ~p~n", [exml_query:attr(El, <<"sid">>), BEl]),
    {ok, Req1} = cowboy_req:reply(200, [content_type(),
                                        ac_allow_origin(<<"*">>),
                                        ac_allow_methods(),
                                        ac_allow_headers(),
                                        ac_max_age()], BEl, Req),
    {ok, Req1, S};
info({close, Sid}, Req, S) ->
    ?DEBUG("Closing handler for ~p~n", [Sid]),
    {ok, Req1} = cowboy_req:reply(200, [], [], Req),
    {ok, Req1, S};
info(item_not_found, Req, S) ->
    {ok, terminal_condition(<<"item-not-found">>, Req), S}.

terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

start_cowboy(Port, Opts) ->
    Host = proplists:get_value(host, Opts, '_'),
    Prefix = proplists:get_value(prefix, Opts, "/http-bind"),
    NumAcceptors = proplists:get_value(num_acceptors, Opts, 100),
    Dispatch = cowboy_router:compile([{Host, [{Prefix, ?MODULE, Opts}] }]),
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

start_backend(Opts) ->
    Backend = proplists:get_value(backend, Opts, ?DEFAULT_BACKEND),
    {Mod, Code} = dynamic_compile:from_string(mod_bosh_dynamic_src(Backend)),
    code:load_binary(Mod, "mod_bosh_dynamic.erl", Code),
    ?BOSH_BACKEND:start(Opts),
    ok.

-spec event_type(Body)
    -> start | restart | normal | terminate when Body::#xmlelement{}.
event_type(Body) ->
    %% Order of checks is important:
    %% stream restart has got sid attribute,
    %% so check for it at the end.
    catch begin
        case exml_query:attr(Body, <<"type">>) of
            <<"terminate">> ->
                throw(terminate);
            _ ->
                check_next
        end,
        case exml_query:attr(Body, <<"xmpp:restart">>) of
            <<"true">> ->
                throw(restart);
            _ ->
                check_next
        end,
        case exml_query:attr(Body, <<"sid">>) of
            undefined ->
                throw(start);
            _ ->
                normal
        end
    end.

forward_body(Req, #xmlelement{} = Body, S) ->
    try
        case event_type(Body) of
            start ->
                case maybe_start_session(Req, Body) of
                    {true, Req1} ->
                        {loop, Req1, S};
                    {false, Req1} ->
                        {ok, Req1, S}
                end;
            restart ->
                Socket = get_session_socket(exml_query:attr(Body, <<"sid">>)),
                handle_request(Socket, {restart, Body}),
                {loop, Req, S};
            normal ->
                Socket = get_session_socket(exml_query:attr(Body, <<"sid">>)),
                handle_request(Socket, {normal, Body}),
                {loop, Req, S};
            terminate ->
                Socket = get_session_socket(exml_query:attr(Body, <<"sid">>)),
                handle_request(Socket, {streamend, Body}),
                {loop, Req, S}
        end
    catch
        error:item_not_found ->
            {ok, terminal_condition(<<"item-not-found">>, Req), S}
    end.

handle_request(Socket, {EventType, Body}) ->
    mod_bosh_socket:handle_request(Socket, {EventType, self(), Body}).

get_session_socket(Sid) ->
    case ?BOSH_BACKEND:get_session(Sid) of
        [BS] ->
            BS#bosh_session.socket;
        [] ->
            ?ERROR_MSG("BOSH session ~p not found!~n", [Sid]),
            error(item_not_found)
    end.

maybe_start_session(Req, Body) ->
    try
        {<<"hold">>, <<"1">>} = {<<"hold">>,
                                 exml_query:attr(Body, <<"hold">>)},
        Hosts = ejabberd_config:get_global_option(hosts),
        {<<"to">>, true} = {<<"to">>,
                            lists:member(exml_query:attr(Body, <<"to">>),
                                         Hosts)},
        %% Version isn't checked as it would be meaningless when supporting
        %% only a subset of the specification.
        {Peer, Req1} = cowboy_req:peer(Req),
        start_session(Peer, Body),
        {true, Req1}
    catch
        error:{badmatch, {<<"to">>, _}} ->
            {false, terminal_condition(<<"host-unknown">>, Req)};
        error:{badmatch, {_Attr, _Value}} ->
            %% TODO: return some sensible condition details
            {false, terminal_condition(<<"undefined-condition">>, [], Req)}
    end.

start_session(Peer, Body) ->
    Sid = make_sid(),
    {ok, Socket} = mod_bosh_socket:start(Sid, Peer),
    BoshSession = #bosh_session{sid = Sid, socket = Socket},
    ?BOSH_BACKEND:create_session(BoshSession),
    handle_request(Socket, {streamstart, Body}),
    ?DEBUG("Created new session ~p~n", [Sid]).

make_sid() ->
    list_to_binary(sha:sha(term_to_binary({now(), make_ref()}))).

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
%% BOSH Terminal Binding Error Conditions
%%--------------------------------------------------------------------

terminal_condition(Condition, Req) ->
    terminal_condition(Condition, [], Req).

terminal_condition(Condition, Details, Req) ->
    Body = terminal_condition_body(Condition, Details),
    strip_ok(cowboy_req:reply(200, [content_type()], Body, Req)).

terminal_condition_body(Condition, Children) ->
    exml:to_binary(#xmlelement{name = <<"body">>,
                               attrs = [{<<"type">>, <<"terminate">>},
                                        {<<"condition">>, Condition},
                                        {<<"xmlns">>, ?NS_HTTPBIND}],
                               children = Children}).

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

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

content_type() ->
    {<<"content-type">>, <<"text/xml; charset=utf8">>}.

ac_allow_origin(Origin) ->
    {<<"Access-Control-Allow-Origin">>, Origin}.

ac_allow_methods() ->
    {<<"Access-Control-Allow-Methods">>, <<"POST, OPTIONS">>}.

ac_allow_headers() ->
    {<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}.

ac_max_age() ->
    {<<"Access-Control-Max-Age">>, integer_to_binary(?DEFAULT_MAX_AGE)}.
