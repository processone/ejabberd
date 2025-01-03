%%%-------------------------------------------------------------------
%%% File    : mod_metrics.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Simple metrics handler for runtime statistics
%%% Created : 22 Oct 2015 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-module(mod_metrics).

-author('christophe.romain@process-one.net').
-behaviour(gen_mod).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-export([start/2, stop/1, mod_opt_type/1, mod_options/1, depends/2, reload/3]).
-export([push/2, mod_doc/0]).
-export([offline_message_hook/1,
         sm_register_connection_hook/3, sm_remove_connection_hook/3,
         user_send_packet/1, user_receive_packet/1,
         s2s_send_packet/1, s2s_receive_packet/1,
         remove_user/2, register_user/2]).

-define(SOCKET_NAME, mod_metrics_udp_socket).
-define(SOCKET_REGISTER_RETRIES, 10).

-type probe() :: atom() | {atom(), integer()}.

%%====================================================================
%% API
%%====================================================================

start(_Host, _Opts) ->
    {ok, [{hook, offline_message_hook, offline_message_hook, 20},
          {hook, sm_register_connection_hook, sm_register_connection_hook, 20},
          {hook, sm_remove_connection_hook, sm_remove_connection_hook, 20},
          {hook, user_send_packet, user_send_packet, 20},
          {hook, user_receive_packet, user_receive_packet, 20},
          {hook, s2s_send_packet, s2s_send_packet, 20},
          {hook, s2s_receive_packet, s2s_receive_packet, 20},
          {hook, remove_user, remove_user, 20},
          {hook, register_user, register_user, 20}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

%%====================================================================
%% Hooks handlers
%%====================================================================
-spec offline_message_hook({any(), message()}) -> {any(), message()}.
offline_message_hook({_Action, #message{to = #jid{lserver = LServer}}} = Acc) ->
    push(LServer, offline_message),
    Acc.

-spec sm_register_connection_hook(ejabberd_sm:sid(), jid(), ejabberd_sm:info()) -> any().
sm_register_connection_hook(_SID, #jid{lserver=LServer}, _Info) ->
    push(LServer, sm_register_connection).

-spec sm_remove_connection_hook(ejabberd_sm:sid(), jid(), ejabberd_sm:info()) -> any().
sm_remove_connection_hook(_SID, #jid{lserver=LServer}, _Info) ->
    push(LServer, sm_remove_connection).

-spec user_send_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_send_packet({Packet, #{jid := #jid{lserver = LServer}} = C2SState}) ->
    push(LServer, user_send_packet),
    {Packet, C2SState}.

-spec user_receive_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_receive_packet({Packet, #{jid := #jid{lserver = LServer}} = C2SState}) ->
    push(LServer, user_receive_packet),
    {Packet, C2SState}.

-spec s2s_send_packet(stanza()) -> stanza().
s2s_send_packet(Packet) ->
    #jid{lserver = LServer} = xmpp:get_from(Packet),
    push(LServer, s2s_send_packet),
    Packet.

-spec s2s_receive_packet({stanza(), ejabberd_s2s_in:state()}) ->
				{stanza(), ejabberd_s2s_in:state()}.
s2s_receive_packet({Packet, S2SState}) ->
    To = xmpp:get_to(Packet),
    LServer = ejabberd_router:host_of_route(To#jid.lserver),
    push(LServer, s2s_receive_packet),
    {Packet, S2SState}.

-spec remove_user(binary(), binary()) -> any().
remove_user(_User, Server) ->
    push(jid:nameprep(Server), remove_user).

-spec register_user(binary(), binary()) -> any().
register_user(_User, Server) ->
    push(jid:nameprep(Server), register_user).

%%====================================================================
%% metrics push handler
%%====================================================================
-spec push(binary(), probe()) -> ok | {error, not_owner | inet:posix()}.
push(Host, Probe) ->
    IP = mod_metrics_opt:ip(Host),
    Port = mod_metrics_opt:port(Host),
    send_metrics(Host, Probe, IP, Port).

-spec send_metrics(binary(), probe(), inet:ip4_address(), inet:port_number()) ->
			  ok | {error, not_owner | inet:posix()}.
send_metrics(Host, Probe, Peer, Port) ->
    % our default metrics handler is https://github.com/processone/grapherl
    % grapherl metrics are named first with service domain, then nodename
    % and name of the data itself, followed by type timestamp and value
    % example => process-one.net/xmpp-1.user_receive_packet:c/1441784958:1
    [_, FQDN] = binary:split(misc:atom_to_binary(node()), <<"@">>),
    [Node|_] = binary:split(FQDN, <<".">>),
    BaseId = <<Host/binary, "/", Node/binary, ".">>,
    TS = integer_to_binary(erlang:system_time(second)),
    case get_socket(?SOCKET_REGISTER_RETRIES) of
	{ok, Socket} ->
	    case Probe of
		{Key, Val} ->
		    BVal = integer_to_binary(Val),
		    Data = <<BaseId/binary, (misc:atom_to_binary(Key))/binary,
			    ":g/", TS/binary, ":", BVal/binary>>,
		    gen_udp:send(Socket, Peer, Port, Data);
		Key ->
		    Data = <<BaseId/binary, (misc:atom_to_binary(Key))/binary,
			    ":c/", TS/binary, ":1">>,
		    gen_udp:send(Socket, Peer, Port, Data)
	    end;
	Err ->
	    Err
    end.

-spec get_socket(integer()) -> {ok, gen_udp:socket()} | {error, inet:posix()}.
get_socket(N) ->
    case whereis(?SOCKET_NAME) of
	undefined ->
	    case gen_udp:open(0) of
		{ok, Socket} ->
		    try register(?SOCKET_NAME, Socket) of
			true -> {ok, Socket}
		    catch _:badarg when N > 1 ->
			    gen_udp:close(Socket),
			    get_socket(N-1)
		    end;
		{error, Reason} = Err ->
		    ?ERROR_MSG("Can not open udp socket to grapherl: ~ts",
			       [inet:format_error(Reason)]),
		    Err
	    end;
	Socket ->
	    {ok, Socket}
    end.

mod_opt_type(ip) ->
    econf:ipv4();
mod_opt_type(port) ->
    econf:port().

mod_options(_) ->
    [{ip, {127,0,0,1}}, {port, 11111}].

mod_doc() ->
    #{desc =>
          [?T("This module sends events to external backend "
              "(by now only https://github.com/processone/grapherl"
              "[grapherl] is supported). Supported events are:"), "",
           "- sm_register_connection", "",
           "- sm_remove_connection", "",
           "- user_send_packet", "",
           "- user_receive_packet", "",
           "- s2s_send_packet", "",
           "- s2s_receive_packet", "",
           "- register_user", "",
           "- remove_user", "",
           "- offline_message", "",
           ?T("When enabled, every call to these hooks triggers "
              "a counter event to be sent to the external backend.")],
      opts =>
          [{ip,
            #{value => ?T("IPv4Address"),
              desc =>
                  ?T("IPv4 address where the backend is located. "
                     "The default value is '127.0.0.1'.")}},
           {port,
            #{value => ?T("Port"),
              desc =>
                  ?T("An internet port number at which the backend "
                     "is listening for incoming connections/packets. "
                     "The default value is '11111'.")}}]}.
