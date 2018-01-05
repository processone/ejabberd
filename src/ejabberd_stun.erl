%%%-------------------------------------------------------------------
%%% File    : ejabberd_stun.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : STUN RFC-5766
%%% Created :  8 May 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2018   ProcessOne
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

-module(ejabberd_stun).

-protocol({rfc, 5766}).
-protocol({xep, 176, '1.0'}).

-ifndef(STUN).
-include("logger.hrl").
-export([socket_type/0, start/2, listen_opt_type/1]).
log_error() ->
    ?CRITICAL_MSG("ejabberd is not compiled with STUN/TURN support", []).
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
-export([tcp_init/2, udp_init/2, udp_recv/5, start/2,
	 socket_type/0, listen_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
tcp_init(Socket, Opts) ->
    ejabberd:start_app(stun),
    stun:tcp_init(Socket, prepare_turn_opts(Opts)).

udp_init(Socket, Opts) ->
    ejabberd:start_app(stun),
    stun:udp_init(Socket, prepare_turn_opts(Opts)).

udp_recv(Socket, Addr, Port, Packet, Opts) ->
    stun:udp_recv(Socket, Addr, Port, Packet, Opts).

start(Opaque, Opts) ->
    stun:start(Opaque, Opts).

socket_type() ->
    raw.

%%%===================================================================
%%% Internal functions
%%%===================================================================
prepare_turn_opts(Opts) ->
    UseTurn = proplists:get_bool(use_turn, Opts),
    prepare_turn_opts(Opts, UseTurn).

prepare_turn_opts(Opts, _UseTurn = false) ->
    set_certfile(Opts);
prepare_turn_opts(Opts, _UseTurn = true) ->
    NumberOfMyHosts = length(?MYHOSTS),
    case proplists:get_value(turn_ip, Opts) of
	undefined ->
	    ?WARNING_MSG("option 'turn_ip' is undefined, "
			 "more likely the TURN relay won't be working "
			 "properly", []);
	_ ->
	    ok
    end,
    AuthFun = fun ejabberd_auth:get_password_s/2,
    Shaper = proplists:get_value(shaper, Opts, none),
    AuthType = proplists:get_value(auth_type, Opts, user),
    Realm = case proplists:get_value(auth_realm, Opts) of
		undefined when AuthType == user ->
		    if NumberOfMyHosts > 1 ->
			    ?WARNING_MSG("you have several virtual "
					 "hosts configured, but option "
					 "'auth_realm' is undefined and "
					 "'auth_type' is set to 'user', "
					 "more likely the TURN relay won't "
					 "be working properly. Using ~s as "
					 "a fallback", [?MYNAME]);
		       true ->
			    ok
		    end,
		    [{auth_realm, ?MYNAME}];
		_ ->
		    []
	    end,
    MaxRate = shaper:get_max_rate(Shaper),
    Opts1 = Realm ++ [{auth_fun, AuthFun},{shaper, MaxRate} |
		      lists:keydelete(shaper, 1, Opts)],
    set_certfile(Opts1).

set_certfile(Opts) ->
    case lists:keymember(certfile, 1, Opts) of
	true ->
	    Opts;
	false ->
	    Realm = proplists:get_value(auth_realm, Opts, ?MYNAME),
	    case ejabberd_pkix:get_certfile(Realm) of
		{ok, CertFile} ->
		    [{certfile, CertFile}|Opts];
		error ->
		    case ejabberd_config:get_option({domain_certfile, Realm}) of
			undefined ->
			    Opts;
			CertFile ->
			    [{certfile, CertFile}|Opts]
		    end
	    end
    end.

listen_opt_type(use_turn) ->
    fun(B) when is_boolean(B) -> B end;
listen_opt_type(turn_ip) ->
    fun(S) ->
	    {ok, Addr} = inet_parse:ipv4_address(binary_to_list(S)),
	    Addr
    end;
listen_opt_type(shaper) ->
    fun acl:shaper_rules_validator/1;
listen_opt_type(auth_type) ->
    fun(anonymous) -> anonymous;
       (user) -> user
    end;
listen_opt_type(auth_realm) ->
    fun iolist_to_binary/1;
listen_opt_type(tls) ->
    fun(B) when is_boolean(B) -> B end;
listen_opt_type(certfile) ->
    fun(S) ->
	    %% We cannot deprecate the option for now:
	    %% I think STUN/TURN clients are too stupid to set SNI
	    ejabberd_pkix:add_certfile(S),
	    iolist_to_binary(S)
    end;
listen_opt_type(turn_min_port) ->
    fun(P) when is_integer(P), P > 0, P =< 65535 -> P end;
listen_opt_type(turn_max_port) ->
    fun(P) when is_integer(P), P > 0, P =< 65535 -> P end;
listen_opt_type(turn_max_allocations) ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
listen_opt_type(turn_max_permissions) ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
listen_opt_type(server_name) ->
    fun iolist_to_binary/1;
listen_opt_type(_) ->
    [shaper, auth_type, auth_realm, tls, certfile, turn_min_port,
     turn_max_port, turn_max_allocations, turn_max_permissions,
     server_name].
-endif.
