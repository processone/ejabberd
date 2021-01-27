%%%-------------------------------------------------------------------
%%% File    : ejabberd_stun.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : STUN RFC-5766
%%% Created :  8 May 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(ejabberd_stun).
-behaviour(ejabberd_listener).
-protocol({rfc, 5766}).
-protocol({xep, 176, '1.0'}).

-ifndef(STUN).
-include("logger.hrl").
-export([accept/1, start/3, start_link/3, listen_options/0]).
fail() ->
    ?CRITICAL_MSG("Listening module ~ts is not available: "
		  "ejabberd is not compiled with STUN/TURN support",
		  [?MODULE]),
    erlang:error(stun_not_compiled).
accept(_) ->
    fail().
listen_options() ->
    fail().
start(_, _, _) ->
    fail().
start_link(_, _, _) ->
    fail().
-else.
-export([tcp_init/2, udp_init/2, udp_recv/5, start/3,
	 start_link/3, accept/1, listen_opt_type/1, listen_options/0,
	 get_password/2]).

-include("logger.hrl").
-ifndef(LAGER).
-export([stun_filter/2]).
-define(STUN_MAX_LOG_LEVEL, notice). % Drop STUN/TURN info/debug messages.
-endif.

%%%===================================================================
%%% API
%%%===================================================================
tcp_init(Socket, Opts) ->
    init_logger(),
    ejabberd:start_app(stun),
    stun:tcp_init(Socket, prepare_turn_opts(Opts)).

-dialyzer({nowarn_function, udp_init/2}).
udp_init(Socket, Opts) ->
    init_logger(),
    ejabberd:start_app(stun),
    stun:udp_init(Socket, prepare_turn_opts(Opts)).

udp_recv(Socket, Addr, Port, Packet, Opts) ->
    stun:udp_recv(Socket, Addr, Port, Packet, Opts).

start(SockMod, Socket, Opts) ->
    stun:start({SockMod, Socket}, Opts).

start_link(_SockMod, Socket, Opts) ->
    stun:start_link(Socket, Opts).

accept(_Pid) ->
    ok.

get_password(User, Realm) ->
    case ejabberd_hooks:run_fold(stun_get_password, <<>>, [User, Realm]) of
	Password when byte_size(Password) > 0 ->
	    Password;
	<<>> ->
	    case ejabberd_auth:get_password_s(User, Realm) of
		Password when is_binary(Password) ->
		    Password;
		_ ->
		    ?INFO_MSG("Cannot use hashed password of ~s@~s for "
			      "STUN/TURN authentication", [User, Realm]),
		    <<>>
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
prepare_turn_opts(Opts) ->
    UseTurn = proplists:get_bool(use_turn, Opts),
    prepare_turn_opts(Opts, UseTurn).

prepare_turn_opts(Opts, _UseTurn = false) ->
    set_certfile(Opts);
prepare_turn_opts(Opts, _UseTurn = true) ->
    NumberOfMyHosts = length(ejabberd_option:hosts()),
    TurnIP = case proplists:get_value(turn_ipv4_address, Opts) of
		 undefined ->
		     MyIP = misc:get_my_ipv4_address(),
		     case MyIP of
			 {127, _, _, _} ->
			     ?WARNING_MSG("Option 'turn_ipv4_address' is "
					  "undefined and the server's hostname "
					  "doesn't resolve to a public IPv4 "
					  "address, most likely the TURN relay "
					  "won't be working properly", []);
			 _ ->
			     ok
		     end,
		     [{turn_ipv4_address, MyIP}];
		 _ ->
		     []
	     end,
    AuthFun = fun ejabberd_stun:get_password/2,
    Shaper = proplists:get_value(shaper, Opts, none),
    AuthType = proplists:get_value(auth_type, Opts, user),
    Realm = case proplists:get_value(auth_realm, Opts) of
		undefined when AuthType == user ->
		    if NumberOfMyHosts > 1 ->
			    ?INFO_MSG("You have several virtual hosts "
				      "configured, but option 'auth_realm' is "
				      "undefined and 'auth_type' is set to "
				      "'user', so the TURN relay might not be "
				      "working properly. Using ~ts as a "
				      "fallback",
				      [ejabberd_config:get_myname()]);
		       true ->
			    ok
		    end,
		    [{auth_realm, ejabberd_config:get_myname()}];
		_ ->
		    []
	    end,
    MaxRate = ejabberd_shaper:get_max_rate(Shaper),
    Opts1 = TurnIP ++ Realm ++ [{auth_fun, AuthFun},{shaper, MaxRate} |
				lists:keydelete(shaper, 1, Opts)],
    set_certfile(Opts1).

set_certfile(Opts) ->
    case lists:keymember(certfile, 1, Opts) of
	true ->
	    Opts;
	false ->
	    Realm = proplists:get_value(auth_realm, Opts, ejabberd_config:get_myname()),
	    case ejabberd_pkix:get_certfile(Realm) of
		{ok, CertFile} ->
		    [{certfile, CertFile}|Opts];
		error ->
		    Opts
	    end
    end.

listen_opt_type(use_turn) ->
    econf:bool();
listen_opt_type(ip) ->
    econf:ip();
listen_opt_type(turn_ipv4_address) ->
    econf:ipv4();
listen_opt_type(turn_ipv6_address) ->
    econf:ipv6();
listen_opt_type(auth_type) ->
    econf:enum([anonymous, user]);
listen_opt_type(auth_realm) ->
    econf:binary();
listen_opt_type(turn_min_port) ->
    econf:int(1025, 65535);
listen_opt_type(turn_max_port) ->
    econf:int(1025, 65535);
listen_opt_type(turn_max_allocations) ->
    econf:pos_int(infinity);
listen_opt_type(turn_max_permissions) ->
    econf:pos_int(infinity);
listen_opt_type(turn_blacklist) ->
    econf:list_or_single(econf:ip_mask());
listen_opt_type(server_name) ->
    econf:binary();
listen_opt_type(certfile) ->
    econf:pem().

listen_options() ->
    [{shaper, none},
     {use_turn, false},
     {turn_ipv4_address, undefined},
     {turn_ipv6_address, undefined},
     {auth_type, user},
     {auth_realm, undefined},
     {tls, false},
     {certfile, undefined},
     {turn_min_port, 49152},
     {turn_max_port, 65535},
     {turn_max_allocations, 10},
     {turn_max_permissions, 10},
     {turn_blacklist, [<<"127.0.0.0/8">>, <<"::1/128">>]},
     {server_name, <<"ejabberd">>}].

-spec init_logger() -> ok.
-ifdef(LAGER).
init_logger() ->
    ok.
-else.
init_logger() ->
    case logger:add_primary_filter(ejabberd_stun, {fun ?MODULE:stun_filter/2,
						   ?STUN_MAX_LOG_LEVEL}) of
	ok ->
	    ok;
	{error, {already_exist, _}} ->
	    ok
    end.

-spec stun_filter(logger:log_event(), logger:level() | term())
      -> logger:filter_return().
stun_filter(#{meta := #{domain := [stun | _]}, level := Level}, MaxLevel) ->
    case logger:compare_levels(Level, MaxLevel) of
	lt ->
	    stop;
	_ ->
	    ignore
    end;
stun_filter(Event, _Extra) ->
    Event.
-endif.

-endif.
