%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 30 Apr 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2017   ProcessOne
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
-module(ejabberd_sip).

-ifndef(SIP).
-include("logger.hrl").
-export([socket_type/0, start/2, listen_opt_type/1]).
log_error() ->
    ?CRITICAL_MSG("ejabberd is not compiled with SIP support", []).
socket_type() ->
    log_error(),
    raw.
listen_opt_type(_) ->
    log_error(),
    [].
start(_, _) ->
    log_error(),
    {error, sip_not_compiled}.
-else.
%% API
-export([tcp_init/2, udp_init/2, udp_recv/5, start/2,
	 socket_type/0, listen_opt_type/1]).

%%%===================================================================
%%% API
%%%===================================================================
tcp_init(Socket, Opts) ->
    ejabberd:start_app(esip),
    esip_socket:tcp_init(Socket, Opts).

udp_init(Socket, Opts) ->
    ejabberd:start_app(esip),
    esip_socket:udp_init(Socket, Opts).

udp_recv(Sock, Addr, Port, Data, Opts) ->
    esip_socket:udp_recv(Sock, Addr, Port, Data, Opts).

start(Opaque, Opts) ->
    esip_socket:start(Opaque, Opts).

socket_type() ->
    raw.

listen_opt_type(certfile) ->
    fun(S) ->
	    ejabberd_pkix:add_certfile(S),
	    iolist_to_binary(S)
    end;
listen_opt_type(tls) ->
    fun(B) when is_boolean(B) -> B end;
listen_opt_type(_) ->
    [tls, certfile].

%%%===================================================================
%%% Internal functions
%%%===================================================================
-endif.
