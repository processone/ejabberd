%%%-------------------------------------------------------------------
%%% File    : ejabberd_sip.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose :
%%% Created : 30 Apr 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2021   ProcessOne
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
-behaviour(ejabberd_listener).

-ifndef(SIP).
-include("logger.hrl").
-export([accept/1, start/3, start_link/3, listen_options/0]).
fail() ->
    ?CRITICAL_MSG("Listening module ~ts is not available: "
		  "ejabberd is not compiled with SIP support",
		  [?MODULE]),
    erlang:error(sip_not_compiled).
accept(_) ->
    fail().
listen_options() ->
    fail().
start(_, _, _) ->
    fail().
start_link(_, _, _) ->
    fail().
-else.
%% API
-export([tcp_init/2, udp_init/2, udp_recv/5, start/3,
	 start_link/3, accept/1]).
-export([listen_opt_type/1, listen_options/0]).

%%%===================================================================
%%% API
%%%===================================================================
tcp_init(Socket, Opts) ->
    ejabberd:start_app(esip),
    esip_socket:tcp_init(Socket, set_certfile(Opts)).

udp_init(Socket, Opts) ->
    ejabberd:start_app(esip),
    esip_socket:udp_init(Socket, Opts).

udp_recv(Sock, Addr, Port, Data, Opts) ->
    esip_socket:udp_recv(Sock, Addr, Port, Data, Opts).

start(SockMod, Socket, Opts) ->
    esip_socket:start({SockMod, Socket}, Opts).

start_link(gen_tcp, Sock, Opts) ->
    esip_socket:start_link(Sock, Opts).

accept(_) ->
    ok.

set_certfile(Opts) ->
    case lists:keymember(certfile, 1, Opts) of
	true ->
	    Opts;
	false ->
	    case ejabberd_pkix:get_certfile(ejabberd_config:get_myname()) of
		{ok, CertFile} ->
		    [{certfile, CertFile}|Opts];
		error ->
		    Opts
	    end
    end.

listen_opt_type(certfile) ->
    econf:pem().

listen_options() ->
    [{tls, false},
     {certfile, undefined}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
-endif.
