%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_http.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : The federation's HTTPS: server discovery, verified TLS, errors
%%% Created : 25 Oct 2026 by Paavel A. <pd@narayana.im>
%%%
%%% The gateway of 26.09 called httpc with no ssl options, so OTP checked no
%%% certificate of any server it federated with, and found a server by
%%% .well-known and the deprecated _matrix._tcp SRV record alone. Here:
%%%
%%%   tls_options/2   verify_peer against the system's CA store (or the
%%%                   module's tls_cacertfile), the server name checked as
%%%                   HTTPS does, SNI
%%%   resolve/3       the server-server API's discovery (spec 1.19, "Resolving
%%%                   server names"): an IP literal or an explicit port as it
%%%                   is; else /.well-known/matrix/server, then SRV
%%%                   _matrix-fed._tcp and _matrix._tcp, then port 8448; with
%%%                   the name the certificate must carry and the Host header
%%%   json/2, error/3 the answers the API defines, JSON with errcode
%%%   safe_url/1,2    whether a URL a remote party named (a media
%%%                   Location, a legacy redirect) may be fetched: https
%%%                   only, its name resolved and every address public
%%%                   (no loopback, private, link-local, CGNAT, ULA,
%%%                   multicast, reserved), with the address to connect to
%%%                   so that a second resolution cannot point elsewhere
%%%                   (a Location to http://127.0.0.1:5280/api
%%%                   ran ejabberd's admin API as the loopback's admin)
%%%   request/5       httpc:request/4 with the answer's body bounded
%%%                  : streamed to the caller and read up to a
%%%                   byte bound within the request's timeout as a whole;
%%%                   {error, too_large} past it; max_body/1 the bound of
%%%                   each federation endpoint
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
-module(mod_matrix_gw_http).

-export([tls_options/2, http_options/3, resolve/3, parse_server_name/1,
         parse_well_known/1, json/2, error/3, cacerts/1, safe_url/1, safe_url/2,
         public_address/1]).
%% every federation answer read with a bound
-export([request/5, max_body/1]).

-define(DEFAULT_PORT, 8448).

-type destination() :: #{host := binary(), port := inet:port_number(), name := binary()}.
-export_type([destination/0]).

%% The ssl options for a request to a server whose certificate must name
%% Name. CACerts: a list of DER certificates.
-spec tls_options(binary(), [public_key:der_encoded()]) -> [ssl:tls_client_option()].
tls_options(Name, CACerts) ->
    SNI = case inet:parse_address(binary_to_list(strip_brackets(Name))) of
              {ok, _} -> [{server_name_indication, disable}];
              _ -> [{server_name_indication, binary_to_list(Name)}]
          end,
    [{verify, verify_peer},
     {cacerts, CACerts},
     {depth, 10},
     {customize_hostname_check,
      [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]} | SNI].

%% httpc's HTTPOptions with the ssl options added for Destination.
-spec http_options([tuple()], destination(), [public_key:der_encoded()]) -> [tuple()].
http_options(HTTPOptions, #{name := Name}, CACerts) ->
    [{ssl, tls_options(Name, CACerts)} | lists:keydelete(ssl, 1, HTTPOptions)].

%% The CA certificates the gateway trusts: the module's tls_cacertfile when
%% set, else the system's store.
-spec cacerts(binary() | undefined) -> [public_key:der_encoded()].
cacerts(File) when is_binary(File), File /= <<>> ->
    {ok, PEM} = file:read_file(File),
    [DER || {'Certificate', DER, not_encrypted} <- public_key:pem_decode(PEM)];
cacerts(_) ->
    try public_key:cacerts_get()
    catch _:_ -> []
    end.

%% "host", "host:port", "1.2.3.4:port", "[::1]:port" as {Host, Port | default}.
-spec parse_server_name(binary()) -> {binary(), inet:port_number() | default} | error.
parse_server_name(<<$[, _/binary>> = Name) ->
    case binary:split(Name, <<"]">>) of
        [<<$[, Addr/binary>>, <<>>] -> {<<$[, Addr/binary, $]>>, default};
        [<<$[, Addr/binary>>, <<$:, Port/binary>>] -> with_port(<<$[, Addr/binary, $]>>, Port);
        _ -> error
    end;
parse_server_name(Name) ->
    case binary:split(Name, <<":">>) of
        [Host] when Host /= <<>> -> {Host, default};
        [Host, Port] when Host /= <<>> -> with_port(Host, Port);
        _ -> error
    end.

with_port(Host, Port) ->
    case catch binary_to_integer(Port) of
        P when is_integer(P), P > 0, P < 65536 -> {Host, P};
        _ -> error
    end.

is_ip(Host) ->
    case inet:parse_address(binary_to_list(strip_brackets(Host))) of
        {ok, _} -> true;
        _ -> false
    end.

strip_brackets(<<$[, Rest/binary>>) -> binary:part(Rest, 0, max(0, byte_size(Rest) - 1));
strip_brackets(H) -> H.

%% The m.server of a /.well-known/matrix/server body, or error.
-spec parse_well_known(binary()) -> binary() | error.
parse_well_known(Body) ->
    try misc:json_decode(Body) of
        #{<<"m.server">> := S} when is_binary(S), S /= <<>> ->
            case parse_server_name(S) of
                error -> error;
                _ -> S
            end;
        _ -> error
    catch _:_ -> error
    end.

%% Where ServerName's federation API is: WellKnown(Host) is the body of
%% https://Host/.well-known/matrix/server or error; Srv(Name) the SRV
%% records of Name as [{Prio, Weight, Port, Target}] or [].
-spec resolve(binary(), fun((binary()) -> binary() | error),
              fun((string()) -> [{integer(), integer(), inet:port_number(), string()}])) ->
                     destination() | error.
resolve(ServerName, WellKnown, Srv) ->
    case parse_server_name(ServerName) of
        error ->
            error;
        {Host, Port} when is_integer(Port) ->
            #{host => Host, port => Port, name => Host};
        {Host, default} ->
            case is_ip(Host) of
                true ->
                    #{host => Host, port => ?DEFAULT_PORT, name => Host};
                false ->
                    case parse_well_known_body(WellKnown(Host)) of
                        error ->
                            srv_or_default(Host, Srv);
                        Delegated ->
                            case parse_server_name(Delegated) of
                                {DHost, DPort} when is_integer(DPort) ->
                                    #{host => DHost, port => DPort, name => DHost};
                                {DHost, default} ->
                                    case is_ip(DHost) of
                                        true -> #{host => DHost, port => ?DEFAULT_PORT, name => DHost};
                                        false -> srv_or_default(DHost, Srv)
                                    end
                            end
                    end
            end
    end.

parse_well_known_body(error) -> error;
parse_well_known_body(Body) -> parse_well_known(Body).

srv_or_default(Host, Srv) ->
    Name = binary_to_list(Host),
    case pick(Srv("_matrix-fed._tcp." ++ Name)) of
        none ->
            case pick(Srv("_matrix._tcp." ++ Name)) of
                none -> #{host => Host, port => ?DEFAULT_PORT, name => Host};
                {Target, Port} -> #{host => Target, port => Port, name => Host}
            end;
        {Target, Port} ->
            #{host => Target, port => Port, name => Host}
    end.

pick(Records) ->
    case lists:sort([{Prio, -Weight, Port, Target} || {Prio, Weight, Port, Target} <- Records,
                                                      Target /= "."]) of
        [{_, _, Port, Target} | _] -> {list_to_binary(Target), Port};
        [] -> none
    end.

%% An answer of the API: Code with JSON.
-spec json(integer(), map()) -> {integer(), [{binary(), binary()}], binary()}.
json(Code, JSON) ->
    {Code, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>}], misc:json_encode(JSON)}.

%% An error of the API: {"errcode": ErrCode, "error": Text}.
-spec error(integer(), binary(), binary()) -> {integer(), [{binary(), binary()}], binary()}.
error(Code, ErrCode, Text) ->
    json(Code, #{<<"errcode">> => ErrCode, <<"error">> => Text}).

%%--------------------------------------------------------------------
%% URLs a remote party names
%%--------------------------------------------------------------------

%% Where a GET of URL, which a remote party named, may go: #{url} the same
%% URL with the host replaced by the address it resolved to (so httpc
%% connects there and nowhere else), #{name} the name the certificate must
%% carry (SNI and the hostname check), #{host_header} the Host header. Or
%% {error, Why}: not_https, bad_url, unresolved, {private, Address}.
-spec safe_url(binary()) -> {ok, #{url := string(), name := binary(),
                                   host_header := string()}} | {error, term()}.
safe_url(URL) ->
    safe_url(URL, fun default_resolve/1).

%% Resolve(Name) -> [inet:ip_address()] (the tests stand it in).
-spec safe_url(binary(), fun((string()) -> [inet:ip_address()])) ->
          {ok, #{url := string(), name := binary(), host_header := string()}} | {error, term()}.
safe_url(URL, Resolve) when is_binary(URL) ->
    try uri_string:parse(URL) of
        #{scheme := Scheme, host := Host} = U when is_binary(Scheme), is_binary(Host), Host /= <<>> ->
            case {string:lowercase(Scheme), maps:is_key(userinfo, U)} of
                {<<"https">>, false} ->
                    safe_host(U, Resolve);
                {<<"https">>, true} ->
                    {error, bad_url};
                _ ->
                    {error, not_https}
            end;
        _ ->
            {error, bad_url}
    catch _:_ ->
            {error, bad_url}
    end;
safe_url(_, _) ->
    {error, bad_url}.

safe_host(#{host := Host0} = U, Resolve) ->
    Host = string:lowercase(strip_brackets(Host0)),
    Addrs = case inet:parse_address(binary_to_list(Host)) of
                {ok, Literal} -> [Literal];
                _ ->
                    try Resolve(binary_to_list(Host))
                    catch _:_ -> []
                    end
            end,
    case Addrs of
        [] ->
            {error, unresolved};
        _ ->
            case [A || A <- Addrs, not public_address(A)] of
                [Bad | _] ->
                    {error, {private, inet:ntoa(Bad)}};
                [] ->
                    %% IPv4 first: httpc's default profile connects over IPv4
                    IP = hd([A || A <- Addrs, tuple_size(A) == 4] ++ Addrs),
                    Port = maps:get(port, U, undefined),
                    HostName = case binary:match(Host, <<":">>) of
                                   nomatch -> Host;
                                   _ -> <<"[", Host/binary, "]">>
                               end,
                    HostHeader = case Port of
                                     undefined -> HostName;
                                     _ -> <<HostName/binary, ":", (integer_to_binary(Port))/binary>>
                                 end,
                    U2 = maps:without([fragment], U#{scheme => <<"https">>, host => list_to_binary(inet:ntoa(IP))}),
                    {ok, #{url => binary_to_list(uri_string:recompose(U2)),
                           name => Host,
                           host_header => binary_to_list(HostHeader)}}
            end
    end.

default_resolve(Name) ->
    V4 = case inet:getaddrs(Name, inet) of
             {ok, A} -> A;
             _ -> []
         end,
    V6 = case inet:getaddrs(Name, inet6) of
             {ok, B} -> B;
             _ -> []
         end,
    V4 ++ V6.

%% Whether an address is a public unicast one (RFC 6890's special-purpose
%% registries: none of this-network, loopback, private, CGNAT, link-local,
%% IETF protocol assignments, documentation, benchmarking, multicast,
%% reserved, broadcast; for IPv6 also ULA, link-local, the v4-mapped,
%% NAT64 and 6to4 forms of such an IPv4 address, Teredo, ORCHID).
-spec public_address(inet:ip_address()) -> boolean().
public_address({A, B, C, _D}) ->
    not (A == 0 orelse A == 10 orelse A == 127 orelse A >= 224
         orelse (A == 100 andalso B >= 64 andalso B =< 127)
         orelse (A == 169 andalso B == 254)
         orelse (A == 172 andalso B >= 16 andalso B =< 31)
         orelse (A == 192 andalso B == 168)
         orelse (A == 192 andalso B == 0 andalso C == 0)
         orelse (A == 192 andalso B == 0 andalso C == 2)
         orelse (A == 198 andalso (B == 18 orelse B == 19))
         orelse (A == 198 andalso B == 51 andalso C == 100)
         orelse (A == 203 andalso B == 0 andalso C == 113));
public_address({0, 0, 0, 0, 0, 16#ffff, H, L}) ->
    public_address(v4(H, L));                   % ::ffff:a.b.c.d
public_address({0, _, _, _, _, _, _, _}) ->
    false;                                      % ::/16: ::, ::1, the old v4-compatible
public_address({16#64, 16#ff9b, _, _, _, _, H, L}) ->
    public_address(v4(H, L));                   % NAT64
public_address({16#2002, H, L, _, _, _, _, _}) ->
    public_address(v4(H, L));                   % 6to4
public_address({16#2001, B, _, _, _, _, _, _}) when B < 16#200 ->
    false;                                      % 2001::/23 IETF (Teredo, ORCHID, benchmarking)
public_address({16#2001, 16#db8, _, _, _, _, _, _}) ->
    false;                                      % documentation
public_address({16#100, 0, 0, 0, _, _, _, _}) ->
    false;                                      % discard-only
public_address({A, _, _, _, _, _, _, _}) ->
    not (A band 16#fe00 == 16#fc00              % ULA fc00::/7
         orelse A band 16#ffc0 == 16#fe80       % link-local fe80::/10
         orelse A band 16#ffc0 == 16#fec0       % site-local fec0::/10
         orelse A band 16#ff00 == 16#ff00);     % multicast
public_address(_) ->
    false.

v4(H, L) ->
    {H bsr 8, H band 255, L bsr 8, L band 255}.

%%--------------------------------------------------------------------
%% Bounded answers
%%--------------------------------------------------------------------

%% The bound of the answer to a federation request on Path (the request's
%% path, from "_matrix"): a room's state and a join's answer can be large,
%% the events a gap asks for less, anything else (keys, profiles, an event,
%% directory, invites, transactions) small.
-spec max_body([binary()]) -> pos_integer().
max_body([<<"_matrix">>, <<"federation">>, _, E | _])
  when E == <<"send_join">>; E == <<"state">> ->
    64 * 1024 * 1024;
max_body([<<"_matrix">>, <<"federation">>, _, E | _])
  when E == <<"get_missing_events">>; E == <<"backfill">>; E == <<"state_ids">> ->
    16 * 1024 * 1024;
max_body([<<"_matrix">>, <<"federation">>, _, <<"user">> | _]) ->
    4 * 1024 * 1024;
max_body(_) ->
    1024 * 1024.

%% httpc:request/4 with the body of the answer read up to Max bytes:
%% {ok, {StatusLine, Headers, Body}} or {error, too_large | timeout |
%% Reason} as httpc gives them (sync), or, with {sync, false} and a
%% {receiver, Fun}, {ok, Ref} and Fun({Ref, Result}) once read. The
%% request's {timeout, T} is the whole answer's (not a chunk's), 60 s
%% when none is given. A 2xx body is streamed to the reader, which stops at
%% Max; another status's body is bounded by httpc's max_body_size (an
%% answer of no length and no chunks, ended by the connection's close,
%% only by the timeout: httpc reads such a body whole).
-spec request(atom(), tuple(), [tuple()], [tuple()], pos_integer()) -> {ok, term()} | {error, term()}.
request(Method, Request, HTTPOptions, Options, Max) ->
    case {proplists:get_value(sync, Options, true), proplists:get_value(receiver, Options)} of
        {false, Receiver} when is_function(Receiver, 1) ->
            Ref = make_ref(),
            spawn(fun() -> Receiver({Ref, bounded(Method, Request, HTTPOptions, Options, Max)}) end),
            {ok, Ref};
        {false, _} ->
            %% a caller that reads the stream itself (the media's collect)
            httpc:request(Method, Request, HTTPOptions, Options);
        _ ->
            %% read in a process of its own: what httpc still sends after
            %% a cancel goes nowhere, and the caller's heap holds only what
            %% was kept
            Self = self(),
            {Pid, MRef} = spawn_monitor(fun() -> Self ! {self(), bounded(Method, Request, HTTPOptions, Options, Max)} end),
            receive
                {Pid, Res} ->
                    erlang:demonitor(MRef, [flush]),
                    case Res of
                        {error, _} = Err -> Err;
                        _ -> {ok, Res}
                    end;
                {'DOWN', MRef, process, Pid, Why} ->
                    {error, Why}
            end
    end.

bounded(Method, Request, HTTPOptions, Options, Max) ->
    Timeout = case proplists:get_value(timeout, HTTPOptions, infinity) of
                  T when is_integer(T) -> T;
                  _ -> 60000
              end,
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    Opts = [{sync, false}, {stream, self}, {body_format, binary}, {max_body_size, Max} |
            [O || {K, _} = O <- Options,
                  not lists:member(K, [sync, stream, body_format, receiver, max_body_size])]],
    case catch httpc:request(Method, Request, HTTPOptions, Opts) of
        {ok, ReqID} -> collect(ReqID, Max, Deadline, undefined, [], 0);
        {error, _} = Err -> Err;
        Other -> {error, Other}
    end.

collect(ReqID, Max, Deadline, Headers, Acc, Size) ->
    receive
        {http, {ReqID, stream_start, Hs}} ->
            case catch list_to_integer(proplists:get_value("content-length", Hs, "")) of
                N when is_integer(N), N > Max ->
                    httpc:cancel_request(ReqID),
                    {error, too_large};
                _ ->
                    collect(ReqID, Max, Deadline, Hs, Acc, Size)
            end;
        {http, {ReqID, stream, Bin}} ->
            case Size + byte_size(Bin) of
                S when S > Max ->
                    httpc:cancel_request(ReqID),
                    {error, too_large};
                S ->
                    collect(ReqID, Max, Deadline, Headers, [Bin | Acc], S)
            end;
        {http, {ReqID, stream_end, Hs}} ->
            {{"HTTP/1.1", 200, "OK"}, case Headers of undefined -> Hs; _ -> Headers end,
             iolist_to_binary(lists:reverse(Acc))};
        {http, {ReqID, {error, body_too_big}}} ->
            {error, too_large};
        {http, {ReqID, {error, {body_too_long, _}}}} ->
            {error, too_large};
        {http, {ReqID, {error, Reason}}} ->
            {error, Reason};
        {http, {ReqID, {StatusLine, Hs, Body}}} ->
            {StatusLine, Hs, iolist_to_binary(Body)}
    after max(0, Deadline - erlang:monotonic_time(millisecond)) ->
            httpc:cancel_request(ReqID),
            {error, timeout}
    end.
