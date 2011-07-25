%% @author Andrew Kreiling <akreiling@pobox.com>
%% @copyright 2008 Andrew Kreiling <akreiling@pobox.com>. All Rights Reserved.
%%
%% @copyright 2010 Nicolas Niclausse: Add random_str fun
%%
%% @license : MIT
%%
%% @doc
%% UUID module for Erlang
%%
%% This Erlang module was designed to be a simple library for generating UUIDs. It
%% conforms to RFC 4122 whenever possible.
%%
-module(uuid).
-author('Andrew Kreiling <akreiling@pobox.com>').
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/0, start/1, start_link/0, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([v4/0, random/0, srandom/0, sha/2, md5/2, timestamp/0, timestamp/2, to_string/1, random_str/0]).

-define(SERVER, ?MODULE).
-define(UUID_DNS_NAMESPACE, <<107,167,184,16,157,173,17,209,128,180,0,192,79,212,48,200>>).
-define(UUID_URL_NAMESPACE, <<107,167,184,17,157,173,17,209,128,180,0,192,79,212,48,200>>).
-define(UUID_OID_NAMESPACE, <<107,167,184,18,157,173,17,209,128,180,0,192,79,212,48,200>>).
-define(UUID_X500_NAMESPACE, <<107,167,184,20,157,173,17,209,128,180,0,192,79,212,48,200>>).

-record(state, {node, clock_seq}).

%% @type uuid() = binary(). A binary representation of a UUID

%% @spec v4() -> uuid()
%% @equiv random()
%% @deprecated Please use the function random() instead.
%%
v4() ->
    random().

%% @spec random_str() -> string()
%% @doc
%% Generates a string representation of a random UUID
%%
random_str() ->
    to_string(random()).

%% @spec random() -> uuid()
%% @doc
%% Generates a random UUID
%%
random() ->
    U = <<
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32
    >>,
    format_uuid(U, 4).

%% @spec srandom() -> uuid()
%% @doc
%% Seeds random number generation with erlang:now() and generates a random UUID
%%
srandom() ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    random().

%% @spec sha(Namespace, Name) -> uuid()
%% where
%%      Namespace = dns | url | oid | x500 | uuid()
%%      Name = list() | binary()
%% @doc
%% Generates a UUID based on a crypto:sha() hash
%%
sha(Namespace, Name) when is_list(Name) ->
    sha(Namespace, list_to_binary(Name));

sha(Namespace, Name) ->
    Context = crypto:sha_update(crypto:sha_update(crypto:sha_init(), namespace(Namespace)), Name),
    U = crypto:sha_final(Context),
    format_uuid(U, 5).

%% @spec md5(Namespace, Name) -> uuid()
%% where
%%      Namespace = dns | url | oid | x500 | uuid()
%%      Name = list() | binary()
%% @doc
%% Generates a UUID based on a crypto:md5() hash
%%
md5(Namespace, Name) when is_list(Name) ->
    md5(Namespace, list_to_binary(Name));

md5(Namespace, Name) ->
    Context = crypto:md5_update(crypto:md5_update(crypto:md5_init(), namespace(Namespace)), Name),
    U = crypto:md5_final(Context),
    format_uuid(U, 3).

%% @spec timestamp() -> uuid()
%% @doc
%% Generates a UUID based on timestamp
%%
%% Requires that the uuid gen_server is started
%%
timestamp() ->
    gen_server:call(?SERVER, timestamp).

%% @spec timestamp(Node, CS) -> uuid()
%% where
%%      Node = binary()
%%      CS = int()
%% @doc
%% Generates a UUID based on timestamp
%%
timestamp(Node, CS) ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    T = (((((MegaSecs * 1000000) + Secs) * 1000000) + MicroSecs) * 10) + 16#01b21dd213814000,
    format_uuid(T band 16#ffffffff, (T bsr 32) band 16#ffff, (T bsr 48) band 16#ffff, (CS bsr 8) band 16#ff, CS band 16#ff, Node, 1).

%% @spec to_string(UUID) -> string()
%% where
%%      UUID = uuid()
%% @doc
%% Generates a string representation of a UUID
%%
to_string(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = _UUID) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", [TL, TM, THV, CSR, CSL, N])).

%%
%% uuid gen_server for generating timestamps with saved state
%%

start() ->
    start([]).

start(Args) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, stop).

init(Options) ->
    {A1,A2,A3} = proplists:get_value(seed, Options, erlang:now()),
    random:seed(A1, A2, A3),
    State = #state{
        node = proplists:get_value(node, Options, <<0:48>>),
        clock_seq = random:uniform(65536)
    },
    error_logger:info_report("uuid server started"),
    {ok, State}.

handle_call(timestamp, _From, State) ->
    Reply = timestamp(State#state.node, State#state.clock_seq),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_report("uuid server stopped"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal API
%%

namespace(dns) -> ?UUID_DNS_NAMESPACE;
namespace(url) -> ?UUID_URL_NAMESPACE;
namespace(oid) -> ?UUID_OID_NAMESPACE;
namespace(x500) -> ?UUID_X500_NAMESPACE;
namespace(UUID) when is_binary(UUID) -> UUID;
namespace(_) -> error.

format_uuid(TL, TM, THV, CSR, CSL, <<N:48>>, V) ->
    format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>, V);

format_uuid(TL, TM, THV, CSR, CSL, N, V) ->
    format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>, V).

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48, _Rest/binary>>, V) ->
    <<TL:32, TM:16, ((THV band 16#0fff) bor (V bsl 12)):16, ((CSR band 16#3f) bor 16#80):8, CSL:8, N:48>>.

%% vim:sw=4:sts=4:ts=8:et
