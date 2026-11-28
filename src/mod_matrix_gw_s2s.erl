%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Matrix S2S
%%% Created :  1 May 2022 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------
-module(mod_matrix_gw_s2s).
-behaviour(gen_statem).

%% API
-export([start_link/2, supervisor/1, create_db/0,
         get_connection/2, check_auth/5, check_signature/3,
         get_destination/2, queue_edu/3,
         cacerts/1]).
%% the limits against a hostile server
-export([rate/4, limit/2, valid_server_name/1]).
%% EDUs held for a server that was down
-export([held_edus/1]).
-ifdef(TEST).
-export([key_response_ok/4]).
-endif.

%% gen_statem callbacks
-export([init/1, terminate/3, code_change/4, callback_mode/0]).
-export([host_header/1]).
-export([handle_event/4]).

-include("logger.hrl").
-include("ejabberd_http.hrl").
-include_lib("kernel/include/inet.hrl").
-include("mod_matrix_gw.hrl").

-record(matrix_s2s,
        {to  :: binary(),
         pid :: pid()}).

-record(pending,
        {request_id :: any(),
         servers :: [binary()],
         %% the server asked, the matrix server
         %% itself or a notary
         via :: binary() | undefined,
         key_queue = []}).

-record(wait,
        {timer_ref :: reference(),
         last :: integer()}).

%% a transaction of EDUs a server did not take in
%% its tries, kept (disc) to be sent again until it does; key = {Server,
%% when it was held, a unique integer}, so a server's rows sort oldest first
-record(matrix_edu_retry,
        {key :: {binary(), integer() | '_', integer() | '_'},
         txn_id :: binary() | '_',
         edus :: [map()] | '_'}).

-record(data,
        {host :: binary(),
         matrix_server :: binary(),
         %% unused since the gateway, kept for the record's shape
         matrix_host_port :: {binary(), integer()} | undefined,
         %% where the server is (mod_matrix_gw_http:resolve/3)
         %% and until when that holds; the EDUs waiting for a transaction
         %% and the one in flight {RequestID, TxnID, EDUs, Tries}
         destination :: {mod_matrix_gw_http:destination() | error, integer()} | undefined,
         edu_queue = [] :: [map()],
         edu_txn :: {any(), binary(), [map()], integer()} | undefined,
         keys = #{},
         %% the last call or EDU (a connection unused for
         %% idle_ms() stops)
         used = 0 :: integer(),
         state :: #pending{} | #wait{},
         %% the held transaction in flight (its
         %% row's key) or none, the next back-off and whether the held rows
         %% are due (a timer fired or a transaction got through)
         edu_held = none :: none | {binary(), integer(), integer()},
         edu_backoff = 0 :: non_neg_integer(),
         edu_due = false :: boolean(),
         edu_timer :: reference() | undefined}).

-define(KEYS_REQUEST_TIMEOUT, 600000).
%% a server's discovery is kept a day (the spec's default for a
%% .well-known without cache headers), an hour when it found nothing
-define(DESTINATION_TTL, timer:hours(24)).
-define(DESTINATION_ERROR_TTL, timer:hours(1)).
%% EDUs: at most 100 in a transaction (the spec's limit), five tries each
%% transaction, 30 seconds apart, as the rooms retry their PDUs
-define(MAX_EDUS, 100).
-define(MAX_EDU_TRIES, 5).
-define(EDU_RETRY, 30000).
%% what is not typing or a receipt is never dropped
%% for failed tries: it is held (matrix_edu_retry) and sent again after a
%% back-off doubling from a minute to a day, and at once after any
%% transaction to that server gets through; at most ?MAX_HELD transactions
%% a server (the oldest goes, with a warning). A request may take a minute,
%% as the rooms' PDU transactions may.
-define(EDU_TIMEOUT, 60000).
-define(EDU_BACKOFF_MIN, 60000).
-define(EDU_BACKOFF_MAX, 86400000).
-define(MAX_HELD, 1000).
%% a connection unused for an hour stops (its keys
%% and discovery are fetched again when it is needed); at most 300 servers
%% a minute get a connection from an unauthenticated request, and one whose
%% request did not verify is not tried again for a minute
-define(IDLE_MS, 3600000).
-define(NEW_ORIGINS, 300).
-define(NEGATIVE_MS, 60000).
-define(RATE_TAB, matrix_gw_rate).
%% new connections a minute, whoever asks for them,
%% and live connections at most
-define(NEW_CONNECTIONS, 1200).
-define(MAX_CONNECTIONS, 10000).
-define(RATE_MAX_KEYS, 100000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary(), binary()) ->
                        {ok, Pid :: pid()} |
                        ignore |
                        {error, Error :: term()}.
start_link(Host, MatrixServer) ->
    gen_statem:start_link(?MODULE, [Host, MatrixServer],
                          ejabberd_config:fsm_limit_opts([])).

-spec supervisor(binary()) -> atom().
supervisor(Host) ->
    gen_mod:get_module_proc(Host, mod_matrix_gw_s2s_sup).

create_db() ->
    ejabberd_mnesia:create(
      ?MODULE, matrix_s2s,
      [{ram_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_s2s)}]),
    %% EDU transactions held for a server
    ejabberd_mnesia:create(
      ?MODULE, matrix_edu_retry,
      [{disc_copies, [node()]},
       {type, ordered_set},
       {attributes, record_info(fields, matrix_edu_retry)}]),
    %% the counters of rate/4, owned by the module's process
    %% (mod_matrix_gw's init/1 calls this)
    case ets:info(?RATE_TAB) of
        undefined -> ets:new(?RATE_TAB, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    ok.

%% Cost more of Key in the current window of Period ms, if that
%% stays within Max; false (nothing counted) otherwise. A fixed window per
%% key; with no table (the module not started) everything is allowed. A
%% counter row is {Key, Window, N, Expires}; past rate_max_keys() rows the
%% expired ones go (counters and the negative cache alike), and
%% when that is not enough the tenth that expires first: the other origins'
%% windows go on.
-spec rate(term(), pos_integer(), pos_integer(), non_neg_integer()) -> boolean().
rate(Key, Max, Period, Cost) ->
    Now = erlang:monotonic_time(millisecond),
    Window = Now div Period,
    try ets:lookup(?RATE_TAB, Key) of
        [{_, Window, N, _}] when N + Cost > Max ->
            false;
        [{_, Window, _, _}] ->
            ets:update_counter(?RATE_TAB, Key, {3, Cost}),
            true;
        _ when Cost > Max ->
            false;
        _ ->
            case ets:info(?RATE_TAB, size) >= limit(rate_max_keys, ?RATE_MAX_KEYS) of
                true -> purge_rates(Now);
                false -> ok
            end,
            ets:insert(?RATE_TAB, {Key, Window, Cost, (Window + 1) * Period}),
            true
    catch error:badarg ->
            true
    end.

%% The rows of the rate table whose time is over; then, while the table is
%% still full, the tenth of it that expires first.
purge_rates(Now) ->
    ets:select_delete(?RATE_TAB, [{{'_', '_', '_', '$1'}, [{'=<', '$1', Now}], [true]},
                                  {{'_', '$1', negative}, [{'=<', '$1', Now}], [true]}]),
    Max = limit(rate_max_keys, ?RATE_MAX_KEYS),
    case ets:info(?RATE_TAB, size) of
        Size when Size >= Max ->
            Expiries = lists:sort(ets:select(?RATE_TAB, [{{'_', '_', '_', '$1'}, [], ['$1']},
                                                         {{'_', '$1', negative}, [], ['$1']}])),
            Cut = lists:nth(max(1, min(length(Expiries), Size div 10)), Expiries),
            ets:select_delete(?RATE_TAB, [{{'_', '_', '_', '$1'}, [{'=<', '$1', Cut}], [true]},
                                          {{'_', '$1', negative}, [{'=<', '$1', Cut}], [true]}]),
            ok;
        _ ->
            ok
    end.

%% A limit, Default unless the suite set another
%% (persistent_term {mod_matrix_gw_s2s, limit, Name}).
-spec limit(atom(), term()) -> term().
limit(Name, Default) ->
    persistent_term:get({?MODULE, limit, Name}, Default).

%% A server name as the spec has it (a host name of letters,
%% digits, dots and dashes, an IPv4 address or a bracketed IPv6 one, and an
%% optional port), at most 255 bytes.
-spec valid_server_name(term()) -> boolean().
valid_server_name(Name) when is_binary(Name), byte_size(Name) =< 255 ->
    case mod_matrix_gw_http:parse_server_name(Name) of
        {<<$[, _/binary>>, _} ->
            true;
        {HostName, _} ->
            lists:all(fun(C) -> (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z)
                                    orelse (C >= $0 andalso C =< $9) orelse C == $. orelse C == $-
                      end, binary_to_list(HostName));
        error ->
            false
    end;
valid_server_name(_) ->
    false.

%% The connection to MatrixServer, made when there is none. The gateway
%% : the one gate for every new connection, whoever asks (a PDU's
%% sender, an alias's server, a local query): a server name only, not one
%% refused a minute ago (negative/1), within ?NEW_CONNECTIONS new ones a
%% minute and ?MAX_CONNECTIONS live ones; {error, refused} beyond, logged
%% once a minute. The caller retries as it does for a server that does not
%% answer (a room's transaction, an EDU's), or reports the server unreachable.
get_connection(Host, MatrixServer) ->
    case mnesia:dirty_read(matrix_s2s, MatrixServer) of
	[] ->
            case admit(MatrixServer) of
                ok ->
                    try
                        Proc = gen_mod:get_module_proc_check(Host, mod_matrix_gw_s2s_sup),
                        supervisor:start_child(Proc, [Host, MatrixServer])
                    of
                        {ok, undefined} -> {error, ignored};
                        Res -> Res
                    catch
                        error:badarg ->
                            {error, wrong_host}
                    end;
                {error, Why} ->
                    case rate(connections_logged, 1, 60000, 1) of
                        true ->
                            ?WARNING_MSG("Matrix gateway: no connection made to ~p: ~s",
                                         [MatrixServer, Why]);
                        false ->
                            ok
                    end,
                    {error, refused}
            end;
        [#matrix_s2s{pid = Pid}] ->
            {ok, Pid}
    end.

admit(MatrixServer) ->
    Now = erlang:monotonic_time(millisecond),
    case valid_server_name(MatrixServer) of
        false ->
            {error, "not a server name"};
        true ->
            case catch ets:lookup(?RATE_TAB, {negative, MatrixServer}) of
                [{_, Until, negative}] when Until > Now ->
                    {error, "refused a moment ago"};
                _ ->
                    Live = try mnesia:table_info(matrix_s2s, size) catch _:_ -> 0 end,
                    case Live >= limit(max_connections, ?MAX_CONNECTIONS) of
                        true ->
                            {error, "too many servers connected"};
                        false ->
                            case rate(new_connections, limit(new_connections, ?NEW_CONNECTIONS), 60000, 1) of
                                true -> ok;
                                false -> {error, "too many new servers a minute"}
                            end
                    end
            end
    end.

get_key(Host, MatrixServer, KeyID) ->
    case mod_matrix_gw_opt:matrix_domain(Host) of
        MatrixServer ->
            {PubKey, _PrivKey} = mod_matrix_gw_opt:key(Host),
            TS = erlang:system_time(millisecond) + timer:hours(24 * 7),
            {ok, PubKey, TS};
        _ ->
            case get_connection(Host, MatrixServer) of
                {ok, S2SPid} ->
                    gen_statem:call(S2SPid, {get_key, KeyID});
                Error -> Error
            end
    end.

%% Where MatrixServer's federation API is, with the name its
%% certificate must carry (mod_matrix_gw_http:resolve/3), or error.
-spec get_destination(binary(), binary()) -> mod_matrix_gw_http:destination() | error.
get_destination(Host, MatrixServer) ->
    case mod_matrix_gw_opt:matrix_domain(Host) of
        MatrixServer ->
            error;
        _ ->
            case get_connection(Host, MatrixServer) of
                {ok, S2SPid} ->
                    try gen_statem:call(S2SPid, get_destination, 30000)
                    catch exit:_ -> error
                    end;
                _ -> error
            end
    end.

%% An EDU for MatrixServer, sent in a transaction of its own
%% with whatever else is queued for that server (at most 100 EDUs),
%% retried as the rooms' PDUs are.
-spec queue_edu(binary(), binary(), map()) -> ok.
queue_edu(Host, MatrixServer, EDU) ->
    case mod_matrix_gw_opt:matrix_domain(Host) of
        MatrixServer ->
            ok;
        _ ->
            case get_connection(Host, MatrixServer) of
                {ok, S2SPid} -> gen_statem:cast(S2SPid, {queue_edu, EDU});
                _ -> ok
            end
    end.

%% The CA certificates federation requests are verified against.
-spec cacerts(binary()) -> [public_key:der_encoded()].
cacerts(Host) ->
    mod_matrix_gw_http:cacerts(mod_matrix_gw_opt:tls_cacertfile(Host)).


%process_query(Host, MatrixServer, AuthParams, Query, JSON, Request) ->
%    case get_connection(Host, MatrixServer) of
%        {ok, S2SPid} ->
%            #request{sockmod = SockMod, socket = Socket} = Request,
%            SockMod:controlling_process(Socket, S2SPid),
%            gen_statem:cast(S2SPid, {query, AuthParams, Query, JSON, Request}),
%            ok;
%        {error, _} = Error ->
%            Error
%    end.

check_auth(Host, MatrixServer, AuthParams, Content, Request) ->
    Destination = mod_matrix_gw_opt:matrix_domain(Host),
    case AuthParams of
        #{<<"destination">> := D} when D /= Destination ->
            %% a request signed for another server is not ours
            ?WARNING_MSG("Request from ~s for another destination: ~p",
                         [MatrixServer, D]),
            false;
        _ ->
            %% no connection, no key fetch, for a name
            %% that is no server name
            case valid_server_name(MatrixServer) of
                true ->
                    check_auth(Host, MatrixServer, Destination, AuthParams, Content, Request);
                false ->
                    ?DEBUG("Request from an invalid server name ~p", [MatrixServer]),
                    false
            end
    end.

%% The key from the connection there is, or one made
%% for this request when the new-server budget allows it; a connection made
%% for a request that does not verify is stopped at once and the server is
%% not tried again for a minute. Our own server name verifies with our key.
check_auth(Host, MatrixServer, MatrixServer, AuthParams, Content, Request) ->
    verify_request(Host, MatrixServer, MatrixServer, AuthParams, Content, Request,
                   fun(KeyID) -> catch get_key(Host, MatrixServer, KeyID) end);
check_auth(Host, MatrixServer, Destination, AuthParams, Content, Request) ->
    case auth_connection(Host, MatrixServer) of
        {ok, S2SPid, New} ->
            Res = verify_request(Host, MatrixServer, Destination, AuthParams, Content, Request,
                                 fun(KeyID) -> catch gen_statem:call(S2SPid, {get_key, KeyID}) end),
            case {Res, New} of
                {false, true} ->
                    negative(MatrixServer),
                    gen_statem:cast(S2SPid, unused);
                _ ->
                    ok
            end,
            Res;
        refused ->
            false
    end.

auth_connection(Host, MatrixServer) ->
    case mnesia:dirty_read(matrix_s2s, MatrixServer) of
        [#matrix_s2s{pid = Pid}] ->
            {ok, Pid, false};
        [] ->
            Now = erlang:monotonic_time(millisecond),
            case catch ets:lookup(?RATE_TAB, {negative, MatrixServer}) of
                [{_, Until, negative}] when Until > Now ->
                    refused;
                _ ->
                    case rate(new_origins, limit(new_origins, ?NEW_ORIGINS), 60000, 1) of
                        true ->
                            case get_connection(Host, MatrixServer) of
                                {ok, Pid} -> {ok, Pid, true};
                                _ -> refused
                            end;
                        false ->
                            case rate(new_origins_logged, 1, 60000, 1) of
                                true ->
                                    ?WARNING_MSG("Matrix gateway: over ~B new servers a minute "
                                                 "asked for (~s the latest): refused unchecked",
                                                 [limit(new_origins, ?NEW_ORIGINS), MatrixServer]);
                                false ->
                                    ok
                            end,
                            refused
                    end
            end
    end.

negative(MatrixServer) ->
    Until = erlang:monotonic_time(millisecond) + limit(negative_ms, ?NEGATIVE_MS),
    catch ets:insert(?RATE_TAB, {{negative, MatrixServer}, Until, negative}),
    ok.

verify_request(_Host, MatrixServer, Destination, AuthParams, Content, Request, GetKey) ->
    #{<<"key">> := KeyID} = AuthParams,
    Now = erlang:system_time(millisecond),
    case GetKey(KeyID) of
        {ok, VerifyKey, ValidUntil} when ValidUntil >= Now ->
            #{<<"sig">> := Sig} = AuthParams,
            JSON = #{<<"method">> => atom_to_binary(Request#request.method, latin1),
                     <<"uri">> => Request#request.raw_path,
                     <<"origin">> => MatrixServer,
                     <<"destination">> => Destination,
                     <<"signatures">> => #{
                         MatrixServer => #{KeyID => Sig}
                        }
                    },
            JSON2 =
                case Content of
                    none -> JSON;
                    _ ->
                        JSON#{<<"content">> => Content}
                end,
            case check_signature(JSON2, MatrixServer, KeyID, VerifyKey) of
                true ->
                    true;
                false ->
                    %% what was asked, not the
                    %% content, and once a minute per server
                    case rate({auth_logged, MatrixServer}, 1, 60000, 1) of
                        true ->
                            ?WARNING_MSG("Failed authentication: ~s ~s from ~s (~B bytes)",
                                         [Request#request.method, Request#request.raw_path,
                                          MatrixServer, Request#request.length]);
                        false ->
                            ok
                    end,
                    false
            end;
        _ ->
            false
    end.

check_signature(Host, JSON, RoomVersion) ->
    case JSON of
        #{<<"sender">> := Sender,
          <<"signatures">> := Sigs,
          <<"origin_server_ts">> := OriginServerTS} ->
            MatrixServer = mod_matrix_gw:get_id_domain_exn(Sender),
            case Sigs of
                #{MatrixServer := #{} = KeySig} ->
                    case maps:next(maps:iterator(KeySig)) of
                        {KeyID, _Sig, _} ->
                            case catch get_key(Host, MatrixServer, KeyID) of
                                {ok, VerifyKey, ValidUntil} ->
                                    if
                                        not RoomVersion#room_version.enforce_key_validity or
                                        (OriginServerTS =< ValidUntil) ->
                                            case check_signature(JSON, MatrixServer, KeyID, VerifyKey) of
                                                true ->
                                                    true;
                                                false ->
                                                    %% the event's
                                                    %% id fields, not the event
                                                    ?WARNING_MSG("Failed authentication of an event of ~s in ~p",
                                                                 [Sender, maps:get(<<"room_id">>, JSON, none)]),
                                                    false
                                            end;
                                        true ->
                                            false
                                    end;
                                _ ->
                                    false
                            end;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> gen_statem:init_result(term()).
init([Host, MatrixServer]) ->
    mnesia:dirty_write(
      #matrix_s2s{to = MatrixServer,
                  pid = self()}),
    %% the keys are fetched after init, so that the
    %% supervisor, which starts every connection, is not held by one
    %% server's discovery; and an unused connection stops
    idle_timer(),
    %% what was held for this server before (an
    %% earlier connection, a restart) is sent soon
    case held_edus(MatrixServer) of
        [] -> ok;
        _ -> erlang:send_after(1000, self(), retry_held_edus)
    end,
    {ok, state_name,
     #data{host = Host,
           matrix_server = MatrixServer,
           used = erlang:monotonic_time(millisecond),
           state = #wait{timer_ref = make_ref(), last = 0}},
     [{next_event, internal, request_keys}]}.

idle_ms() ->
    limit(idle_ms, ?IDLE_MS).

idle_timer() ->
    erlang:send_after(max(100, min(idle_ms() div 2, 600000)), self(), idle_check).

%% Nothing queued, nothing in flight, nobody waiting for a key, nothing
%% held (the connection lives on to send it).
unused(#data{edu_queue = [], edu_txn = undefined, state = #wait{}} = Data) ->
    held_edus(Data#data.matrix_server) == [];
unused(#data{edu_queue = [], edu_txn = undefined, state = #pending{key_queue = []}} = Data) ->
    held_edus(Data#data.matrix_server) == [];
unused(_) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the gen_statem runs with CallbackMode =:= handle_event_function
%% this function is called for every event a gen_statem receives.
%% @end
%%--------------------------------------------------------------------
-spec handle_event(
        gen_statem:event_type(), Msg :: term(),
        State :: term(), Data :: term()) ->
                          gen_statem:handle_event_result().
%handle_event({call, From}, _Msg, State, Data) ->
%    {next_state, State, Data, [{reply, From, ok}]}.
%% calls and queued EDUs are uses
handle_event({call, _} = Type, Msg, State, Data) ->
    handle_event1(Type, Msg, State, Data#data{used = erlang:monotonic_time(millisecond)});
handle_event(cast, {queue_edu, _} = Msg, State, Data) ->
    handle_event1(cast, Msg, State, Data#data{used = erlang:monotonic_time(millisecond)});
handle_event(internal, request_keys, _State, Data) ->
    {keep_state, request_keys(Data#data.matrix_server, Data)};
handle_event(info, idle_check, _State, Data) ->
    case unused(Data) andalso
        erlang:monotonic_time(millisecond) - Data#data.used >= idle_ms() of
        true ->
            ?DEBUG("Matrix gateway: the connection to ~s is unused, stopped",
                   [Data#data.matrix_server]),
            {stop, normal};
        false ->
            idle_timer(),
            keep_state_and_data
    end;
%% a connection made for a request that did not verify
handle_event(cast, unused, _State, Data) ->
    case unused(Data) of
        true -> {stop, normal};
        false -> keep_state_and_data
    end;
handle_event(Type, Msg, State, Data) ->
    handle_event1(Type, Msg, State, Data).

handle_event1({call, From}, get_destination, _State, Data) ->
    {Dest, Data2} = local_destination(Data),
    {keep_state, Data2, [{reply, From, Dest}]};
handle_event1(cast, {queue_edu, EDU}, _State, Data) ->
    {keep_state, maybe_send_edus(Data#data{edu_queue = Data#data.edu_queue ++ [EDU]})};
handle_event1(info, {edu_txn_res, RequestID, Res}, _State,
             #data{edu_txn = {RequestID, TxnID, EDUs, Tries}} = Data) ->
    case Res of
        {{_, 200, _}, _, _} ->
            {keep_state, maybe_send_edus(edu_txn_done(Data#data{edu_txn = undefined}))};
        _ when Data#data.edu_held /= none ->
            %% a held transaction failed again: kept, a longer wait
            {keep_state, maybe_send_edus(edu_held_failed(TxnID, Res, Data#data{edu_txn = undefined}))};
        _ when Tries < ?MAX_EDU_TRIES, Data#data.edu_backoff == 0 ->
            ?DEBUG("EDU transaction ~s to ~s failed (~p), retrying",
                   [TxnID, Data#data.matrix_server, Res]),
            %% a failed discovery is tried again
            %% before the next try, not served from its hour's cache
            erlang:send_after(edu_retry_ms(), self(), resend_edus),
            {keep_state, forget_failed_destination(
                           Data#data{edu_txn = {undefined, TxnID, EDUs, Tries}})};
        _ ->
            %% out of tries (or the server is known
            %% to be down): held, not dropped
            {keep_state, maybe_send_edus(hold_edus(TxnID, EDUs, Tries, Res,
                                                   Data#data{edu_txn = undefined}))}
    end;
handle_event1(info, resend_edus, _State, #data{edu_txn = {undefined, TxnID, EDUs, Tries}} = Data) ->
    {keep_state, send_edu_txn(TxnID, EDUs, Tries + 1, Data)};
handle_event1(info, retry_held_edus, _State, Data) ->
    {keep_state, maybe_send_edus(Data#data{edu_due = true, edu_timer = undefined})};
handle_event1({call, From}, {get_key, KeyID}, State, Data) ->
    Now = erlang:system_time(millisecond),
    case maps:find(KeyID, Data#data.keys) of
        {ok, {Key, ValidUntil}} when ValidUntil >= Now ->
            {keep_state, Data, [{reply, From, {ok, Key, ValidUntil}}]};
        {ok, {Key, ValidUntil}} ->
            %% the key's valid_until_ts has passed (26.09's
            %% TODO). An event signed before then still verifies with it,
            %% a request does not: fetch the server's keys again, unless a
            %% fetch is under way or the last one, under a minute ago,
            %% brought nothing newer
            case Data#data.state of
                #pending{key_queue = KeyQueue} = St ->
                    {next_state, State,
                     Data#data{state = St#pending{key_queue = [{From, KeyID} | KeyQueue]}}, []};
                #wait{last = Last} when Last + 60000 > Now ->
                    {keep_state, Data, [{reply, From, {ok, Key, ValidUntil}}]};
                #wait{} ->
                    Data2 = request_keys(Data#data.matrix_server, Data),
                    #pending{key_queue = KeyQueue} = St = Data2#data.state,
                    {next_state, State,
                     Data2#data{state = St#pending{key_queue = [{From, KeyID} | KeyQueue]}}, []}
            end;
        error ->
            case Data#data.state of
                #pending{key_queue = KeyQueue} = St ->
                    KeyQueue2 = [{From, KeyID} | KeyQueue],
                    {next_state, State,
                     Data#data{state = St#pending{key_queue = KeyQueue2}}, []};
                #wait{timer_ref = TimerRef, last = Last} ->
                    TS = erlang:system_time(millisecond),
                    if
                        Last + ?KEYS_REQUEST_TIMEOUT =< TS ->
                            Data2 = request_keys(Data#data.matrix_server, Data),
                            #pending{key_queue = KeyQueue} = St = Data2#data.state,
                            KeyQueue2 = [{From, KeyID} | KeyQueue],
                            {next_state, State,
                             Data2#data{state = St#pending{key_queue = KeyQueue2}}, []};
                        true ->
                            Timeout =
                                case erlang:read_timer(TimerRef) of
                                    false ->
                                        Last + ?KEYS_REQUEST_TIMEOUT - TS;
                                    Left ->
                                        erlang:cancel_timer(TimerRef),
                                        min(Left,
                                            Last + ?KEYS_REQUEST_TIMEOUT - TS)
                                end,
                            TRef = erlang:start_timer(Timeout, self(), []),
                            {next_state, State,
                             Data#data{state = #wait{timer_ref = TRef,
                                                     last = Last}},
                             [{reply, From, error}]}
                    end
            end
    end;
handle_event1(cast, {key_reply, RequestID, HTTPResult}, State,
             #data{state = #pending{request_id = RequestID,
                                    servers = Servers,
                                    via = Via,
                                    key_queue = KeyQueue} = St} = Data) ->
    TS = erlang:system_time(millisecond),
    Res =
        case HTTPResult of
            {{_, 200, _}, _, SJSON} ->
                try
                    JSON1 = misc:json_decode(SJSON),
                    JSON =
                        case JSON1 of
                            #{<<"server_keys">> := [J]} -> J;
                            _ -> JSON1
                        end,
                    ?DEBUG("keys ~p~n", [JSON]),
                    #{<<"verify_keys">> := VerifyKeys} = JSON,
                    %% the keys are the server's we asked about
                    #{<<"server_name">> := ServerName} = JSON,
                    ServerName = Data#data.matrix_server,
                    %% signed by the server under
                    %% every key it lists, and by the notary that answered
                    true = key_response_ok(
                             JSON, Data#data.matrix_server, Via,
                             fun(NotaryKeyID) -> notary_key(Data#data.host, Via, NotaryKeyID) end),
                    #{<<"valid_until_ts">> := ValidUntil} = JSON,
                    ValidUntil2 =
                        min(ValidUntil,
                            erlang:system_time(millisecond) + timer:hours(24 * 7)),
                    OldKeysJSON =
                        case JSON of
                            #{<<"old_verify_keys">> := OldKeysJ}
                              when is_map(OldKeysJ) ->
                                OldKeysJ;
                            _ ->
                                #{}
                        end,
                    OldKeys =
                        maps:filtermap(
                          fun(_KID,
                              #{<<"key">> := SK,
                                <<"expired_ts">> := Exp})
                                when is_integer(Exp),
                                     is_binary(SK) ->
                                  {true, {mod_matrix_gw:base64_decode(SK),
                                          Exp}};
                             (_, _) -> false
                          end, OldKeysJSON),
                    NewKeys =
                        maps:filtermap(
                          fun(_KID,
                              #{<<"key">> := SK})
                                when is_binary(SK) ->
                                  {true, {mod_matrix_gw:base64_decode(SK),
                                          ValidUntil2}};
                             (_, _) -> false
                          end, VerifyKeys),
                    {ok, maps:merge(OldKeys, NewKeys), ValidUntil2}
                catch
                    _:_ ->
                        {ok, Data#data.keys, TS + ?KEYS_REQUEST_TIMEOUT}
                end;
            _ ->
                case Servers of
                    [] ->
                        {ok, Data#data.keys, TS + ?KEYS_REQUEST_TIMEOUT};
                    [S | Servers1] ->
                        {error,
                         request_keys(
                           S,
                           Data#data{state = St#pending{servers = Servers1}})}
                end
        end,
    case Res of
        {ok, Keys, ValidTS} ->
            Replies =
                lists:map(
                  fun({From, KeyID}) ->
                          case maps:find(KeyID, Keys) of
                              {ok, {Key, KeyValidUntil}} ->
                                  {reply, From, {ok, Key, KeyValidUntil}};
                              error ->
                                  {reply, From, error}
                          end
                  end,
                  KeyQueue),
            TimerRef = erlang:start_timer(max(ValidTS - TS, ?KEYS_REQUEST_TIMEOUT),
                                          self(), []),
            Data2 = Data#data{keys = Keys,
                              state = #wait{timer_ref = TimerRef,
                                            last = TS}},
            ?DEBUG("KEYS ~p~n", [{Keys, Data2}]),
            {next_state, State, Data2, Replies};
        {error, Data2} ->
            {next_state, State, Data2, []}
    end;
handle_event1(info, {timeout, TimerRef, []}, State,
             #data{state = #wait{timer_ref = TimerRef}} = Data) ->
    Data2 = request_keys(Data#data.matrix_server, Data),
    {next_state, State, Data2, []};
handle_event1(cast, Msg, State, Data) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {next_state, State, Data, []};
handle_event1(info, Info, State, Data) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {next_state, State, Data, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, Data) ->
    mnesia:dirty_delete_object(
      #matrix_s2s{to = Data#data.matrix_server,
                  pid = self()}),
    %% TODO: wait for messages
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
                         {ok, NewState :: term(), NewData :: term()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

callback_mode() ->
    handle_event_function.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% The discovery of mod_matrix_gw_http:resolve/3, its
%% .well-known fetched over verified TLS and SRV looked up for
%% _matrix-fed._tcp then _matrix._tcp.
resolve_destination(Host, MatrixServer) ->
    CACerts = cacerts(Host),
    WellKnown =
        fun(Name) ->
                URL = <<"https://", Name/binary, "/.well-known/matrix/server">>,
                %% 64 KiB at most
                case mod_matrix_gw_http:request(get, {URL, []},
                                   [{timeout, 5000}, {connect_timeout, 5000},
                                    {ssl, mod_matrix_gw_http:tls_options(Name, CACerts)}],
                                   [{sync, true}, {body_format, binary}], 65536) of
                    {ok, {{_, 200, _}, _Headers, Body}} -> Body;
                    _ -> error
                end
        end,
    Srv =
        fun(Name) ->
                case inet_res:getbyname(Name, srv, 5000) of
                    {ok, {hostent, _, _, _, _, Records}} -> Records;
                    _ -> []
                end
        end,
    Res = mod_matrix_gw_http:resolve(MatrixServer, WellKnown, Srv),
    ?DEBUG("~s is at ~p", [MatrixServer, Res]),
    Res.

maybe_send_edus(#data{edu_txn = undefined, edu_due = true} = Data) ->
    %% the oldest held transaction first, under its
    %% own id (a server that had taken it answers from its cache)
    case held_edus(Data#data.matrix_server) of
        [#matrix_edu_retry{key = Key, txn_id = TxnID, edus = EDUs} | _] ->
            send_edu_txn(TxnID, EDUs, 1, Data#data{edu_held = Key});
        [] ->
            maybe_send_edus(Data#data{edu_due = false, edu_backoff = 0})
    end;
maybe_send_edus(#data{edu_txn = undefined, edu_queue = [_ | _] = Queue} = Data) ->
    {EDUs, Rest} = lists:split(min(?MAX_EDUS, length(Queue)), Queue),
    send_edu_txn(p1_rand:get_string(), EDUs, 1, Data#data{edu_queue = Rest});
maybe_send_edus(Data) ->
    Data.

%% EDU transactions held for MatrixServer, oldest
%% first.
-spec held_edus(binary()) -> [#matrix_edu_retry{}].
held_edus(MatrixServer) ->
    try mnesia:dirty_select(matrix_edu_retry,
                            [{#matrix_edu_retry{key = {MatrixServer, '_', '_'}, _ = '_'}, [], ['$_']}])
    catch _:_ -> []
    end.

edu_retry_ms() ->
    limit(edu_retry_ms, ?EDU_RETRY).

%% A transaction got through: a held one's row goes; whatever else is held
%% for the server is due now (it answers again).
edu_txn_done(#data{edu_held = none, edu_backoff = 0} = Data) ->
    Data;
edu_txn_done(#data{edu_held = Key} = Data) ->
    case Key of
        none -> ok;
        _ -> mnesia:dirty_delete(matrix_edu_retry, Key)
    end,
    case held_edus(Data#data.matrix_server) of
        [] ->
            ?INFO_MSG("Matrix gateway: ~s takes EDUs again; every held one sent",
                      [Data#data.matrix_server]),
            Data#data{edu_held = none, edu_backoff = 0, edu_due = false};
        _ ->
            Data#data{edu_held = none, edu_due = true}
    end.

edu_held_failed(TxnID, Res, Data) ->
    ?DEBUG("Held EDU transaction ~s to ~s failed again: ~p",
           [TxnID, Data#data.matrix_server, Res]),
    backoff(forget_failed_destination(Data#data{edu_held = none, edu_due = false})).

%% Out of tries: typing and receipts are dropped (they are of the moment),
%% the rest held. Logged once per outage (the device is told nothing).
hold_edus(TxnID, EDUs, Tries, Res, Data) ->
    Server = Data#data.matrix_server,
    Keep = [E || E <- EDUs, not transient_edu(E)],
    case Keep of
        [] ->
            ok;
        _ ->
            Now = erlang:system_time(microsecond),
            mnesia:dirty_write(#matrix_edu_retry{key = {Server, Now, erlang:unique_integer([positive])},
                                                 txn_id = TxnID, edus = Keep}),
            Held = held_edus(Server),
            case length(Held) - limit(max_held_edus, ?MAX_HELD) of
                Over when Over > 0 ->
                    ?WARNING_MSG("Matrix gateway: over ~B EDU transactions held for ~s; "
                                 "the oldest ~B dropped",
                                 [limit(max_held_edus, ?MAX_HELD), Server, Over]),
                    [mnesia:dirty_delete(matrix_edu_retry, K)
                     || #matrix_edu_retry{key = K} <- lists:sublist(Held, Over)];
                _ ->
                    ok
            end
    end,
    case Data#data.edu_backoff of
        0 ->
            ?INFO_MSG("Matrix gateway: EDU transaction ~s to ~s failed ~B times (~p); "
                      "~B EDUs held, sent again when ~s answers",
                      [TxnID, Server, Tries, Res, length(Keep), Server]),
            backoff(forget_failed_destination(Data));
        _ ->
            Data
    end.

transient_edu(#{<<"edu_type">> := T}) ->
    T == <<"m.typing">> orelse T == <<"m.receipt">>;
transient_edu(_) ->
    false.

%% The next wait for the held transactions: from a minute, doubling, at
%% most a day.
backoff(Data) ->
    Min = limit(edu_backoff_min, ?EDU_BACKOFF_MIN),
    Wait = case Data#data.edu_backoff of
               0 -> Min;
               B -> min(B * 2, ?EDU_BACKOFF_MAX)
           end,
    case Data#data.edu_timer of
        undefined -> ok;
        Old -> erlang:cancel_timer(Old)
    end,
    Data#data{edu_backoff = Wait,
              edu_timer = erlang:send_after(Wait, self(), retry_held_edus)}.

%% A discovery that found nothing is not kept for its hour when a
%% transaction failed: the next try looks again.
forget_failed_destination(#data{destination = {error, _}} = Data) ->
    Data#data{destination = undefined};
forget_failed_destination(Data) ->
    Data.

send_edu_txn(TxnID, EDUs, Tries, Data) ->
    Host = Data#data.host,
    Body = #{<<"origin">> => mod_matrix_gw_opt:matrix_domain(Host),
             <<"origin_server_ts">> => erlang:system_time(millisecond),
             <<"pdus">> => [],
             <<"edus">> => EDUs},
    Self = self(),
    Receiver = fun({RequestID, Res}) -> Self ! {edu_txn_res, RequestID, Res} end,
    %% the destination from this process's own cache: send_request/8 would
    %% ask this very process for it. The gateway: a suite stands in for the
    %% remote server (persistent_term {mod_matrix_gw_s2s, edu_sender}, a
    %% fun of the server and the body giving the answer)
    {Dest, Data1} = case persistent_term:get({?MODULE, edu_sender}, undefined) of
                        undefined -> local_destination(Data);
                        _ -> {stand_in, Data}
                    end,
    case catch send_with(Dest,
                 Host, put, Data#data.matrix_server,
                 [<<"_matrix">>, <<"federation">>, <<"v1">>, <<"send">>, TxnID],
                 [], Body, [{timeout, ?EDU_TIMEOUT}, {connect_timeout, 5000}],
                 [{sync, false}, {receiver, Receiver}]) of
        {ok, RequestID} ->
            Data1#data{edu_txn = {RequestID, TxnID, EDUs, Tries}};
        Err ->
            ?DEBUG("EDU transaction to ~s not sent: ~p", [Data#data.matrix_server, Err]),
            Self ! {edu_txn_res, undefined, Err},
            Data1#data{edu_txn = {undefined, TxnID, EDUs, Tries}}
    end.

send_with(error, _Host, _Method, MatrixServer, _Path, _Query, _JSON, _HTTPOptions, _Options) ->
    {error, {no_destination, MatrixServer}};
send_with(stand_in, _Host, _Method, MatrixServer, _Path, _Query, JSON, _HTTPOptions, Options) ->
    Ref = make_ref(),
    Res = (persistent_term:get({?MODULE, edu_sender}))(MatrixServer, JSON),
    (proplists:get_value(receiver, Options))({Ref, Res}),
    {ok, Ref};
send_with(Dest, Host, Method, MatrixServer, Path, Query, JSON, HTTPOptions, Options) ->
    mod_matrix_gw:send_request(Host, Method, MatrixServer, Dest, Path, Query, JSON,
                               HTTPOptions, Options).

local_destination(Data) ->
    Now = erlang:system_time(millisecond),
    case Data#data.destination of
        {Dest, Until} when Until > Now ->
            {Dest, Data};
        _ ->
            Dest = resolve_destination(Data#data.host, Data#data.matrix_server),
            TTL = case Dest of error -> ?DESTINATION_ERROR_TTL; _ -> ?DESTINATION_TTL end,
            {Dest, Data#data{destination = {Dest, Now + TTL}}}
    end.

%% The Host header a request carries when the server is reached
%% at another name than its own (an SRV target).
host_header(#{host := Name, name := Name}) -> [];
host_header(#{name := Name}) -> [{"Host", binary_to_list(Name)}].

check_signature(JSON, SignatureName, KeyID, VerifyKey) ->
    try
        #{<<"signatures">> := Signatures} = JSON,
        #{SignatureName := SignatureData} = Signatures,
        #{KeyID := SSignature} = SignatureData,
        Signature = mod_matrix_gw:base64_decode(SSignature),
        JSON2 = maps:without([<<"signatures">>, <<"unsigned">>], JSON),
        Msg = mod_matrix_gw:encode_canonical_json(JSON2),
        crypto:verify(eddsa, none, Msg, Signature, [VerifyKey, ed25519])
    catch
        _:_ ->
            false
    end.

%% A /key/v2 answer about ServerName is taken only
%% when every key in its verify_keys is a key of ed25519 that signed the
%% answer (the spec: "signed by each of the keys"), and, when a notary Via
%% answered for ServerName, when it carries a signature of Via's that
%% verifies with Via's own key (GetNotaryKey(KeyID) -> {ok, Key, _}).
-spec key_response_ok(map(), binary(), binary() | undefined, fun((binary()) -> term())) -> boolean().
key_response_ok(#{<<"verify_keys">> := VerifyKeys} = JSON, ServerName, Via, GetNotaryKey)
  when is_map(VerifyKeys), map_size(VerifyKeys) > 0 ->
    SelfSigned =
        lists:all(
          fun({<<"ed25519:", _/binary>> = KeyID, #{<<"key">> := SKey}}) when is_binary(SKey) ->
                  case catch mod_matrix_gw:base64_decode(SKey) of
                      Key when is_binary(Key), byte_size(Key) == 32 ->
                          check_signature(JSON, ServerName, KeyID, Key);
                      _ ->
                          false
                  end;
             (_) ->
                  false
          end, maps:to_list(VerifyKeys)),
    SelfSigned andalso
        case Via of
            ServerName -> true;
            undefined -> true;
            _ -> notary_signed(JSON, Via, GetNotaryKey)
        end;
key_response_ok(_, _, _, _) ->
    false.

%% A notary's key, through the notary's own connection, waited for at most
%% ten seconds (two notaries fetching each other's keys through each other
%% must not wait for ever).
notary_key(Host, Via, KeyID) ->
    Conn = case mod_matrix_gw_opt:matrix_domain(Host) of
               Via -> own;
               _ -> catch get_connection(Host, Via)
           end,
    case Conn of
        own ->
            get_key(Host, Via, KeyID);
        {ok, Pid} when Pid /= self() ->
            catch gen_statem:call(Pid, {get_key, KeyID}, 10000);
        _ ->
            error
    end.

notary_signed(#{<<"signatures">> := #{} = Sigs} = JSON, Via, GetNotaryKey) ->
    case Sigs of
        #{Via := #{} = ViaSigs} ->
            lists:any(
              fun(KeyID) ->
                      case GetNotaryKey(KeyID) of
                          {ok, Key, _} -> check_signature(JSON, Via, KeyID, Key);
                          _ -> false
                      end
              end, maps:keys(ViaSigs));
        _ ->
            false
    end;
notary_signed(_, _, _) ->
    false.

request_keys(Via, Data) ->
    %% found by the spec's discovery, over verified TLS
    Dest =
        case resolve_destination(Data#data.host, Via) of
            error -> #{host => Via, port => 8448, name => Via};
            D -> D
        end,
    #{host := MHost, port := MPort} = Dest,
    URL =
        case Data#data.matrix_server of
            Via ->
                <<"https://", MHost/binary,
                  ":", (integer_to_binary(MPort))/binary,
                  "/_matrix/key/v2/server">>;
            MatrixServer ->
                <<"https://", MHost/binary,
                  ":", (integer_to_binary(MPort))/binary,
                  "/_matrix/key/v2/query/", MatrixServer/binary>>
        end,
    Self = self(),
    %% the keys' answer read up to 1 MiB
    {ok, RequestID} =
        mod_matrix_gw_http:request(get, {URL, host_header(Dest)},
                      mod_matrix_gw_http:http_options(
                        [{timeout, 5000}, {connect_timeout, 5000}], Dest,
                        cacerts(Data#data.host)),
                      [{sync, false},
                       {receiver,
                        fun({RequestId, Result}) ->
                                gen_statem:cast(
                                  Self, {key_reply, RequestId, Result})
                        end}], 1048576),
    case Data#data.state of
        #pending{request_id = OldReqID} = St ->
            case OldReqID of
                undefined ->
                    ok;
                _ ->
                    httpc:cancel_request(OldReqID)
            end,
            Data#data{state = St#pending{request_id = RequestID, via = Via}};
        #wait{timer_ref = TimerRef} ->
            erlang:cancel_timer(TimerRef),
            NotaryServers = mod_matrix_gw_opt:notary_servers(Data#data.host),
            Data#data{state = #pending{request_id = RequestID,
                                       servers = NotaryServers,
                                       via = Via}}
    end.
