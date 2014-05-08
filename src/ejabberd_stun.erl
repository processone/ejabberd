%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_stun).

%% API
-export([tcp_init/2, udp_init/2, udp_recv/5, start/2, socket_type/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
tcp_init(Socket, Opts) ->
    ejabberd:start_app(p1_stun),
    stun:tcp_init(Socket, prepare_turn_opts(Opts)).

udp_init(Socket, Opts) ->
    ejabberd:start_app(p1_stun),
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
    Opts;
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
    Shaper = gen_mod:get_opt(shaper, Opts,
			     fun(S) when is_atom(S) -> S end,
			     none),
    AuthType = gen_mod:get_opt(auth_type, Opts,
			       fun(anonymous) -> anonymous;
				  (user) -> user
			       end, user),
    Realm = case gen_mod:get_opt(auth_realm, Opts, fun iolist_to_binary/1) of
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
    Realm ++ [{auth_fun, AuthFun},{shaper, MaxRate} |
	      lists:keydelete(shaper, 1, Opts)].
