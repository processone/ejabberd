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
         parse_well_known/1, json/2, error/3, cacerts/1]).

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
