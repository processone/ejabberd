%%%-------------------------------------------------------------------
%%% File    : mod_metrics.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Simple metrics handler for runtime statistics
%%% Created : 22 Oct 2015 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-behaviour(ejabberd_config).
-author('christophe.romain@process-one.net').
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(HOOKS, [offline_message_hook,
                sm_register_connection_hook, sm_remove_connection_hook,
                user_send_packet, user_receive_packet,
                s2s_send_packet, s2s_receive_packet,
                remove_user, register_user]).

-export([start/2, stop/1, send_metrics/4]).

-export([offline_message_hook/3,
         sm_register_connection_hook/3, sm_remove_connection_hook/3,
         user_send_packet/4, user_receive_packet/5,
         s2s_send_packet/3, s2s_receive_packet/3,
         remove_user/2, register_user/2]).

%%====================================================================
%% API
%%====================================================================

start(Host, _Opts) ->
    [ejabberd_hooks:add(Hook, Host, ?MODULE, Hook, 20)
     || Hook <- ?HOOKS].

stop(Host) ->
    [ejabberd_hooks:delete(Hook, Host, ?MODULE, Hook, 20)
     || Hook <- ?HOOKS].

%%====================================================================
%% Hooks handlers
%%====================================================================

offline_message_hook(_From, #jid{lserver=LServer}, _Packet) ->
    push(LServer, offline_message).

sm_register_connection_hook(_SID, #jid{lserver=LServer}, _Info) ->
    push(LServer, sm_register_connection).
sm_remove_connection_hook(_SID, #jid{lserver=LServer}, _Info) ->
    push(LServer, sm_remove_connection).

user_send_packet(Packet, _C2SState, #jid{lserver=LServer}, _To) ->
    push(LServer, user_send_packet),
    Packet.
user_receive_packet(Packet, _C2SState, _JID, _From, #jid{lserver=LServer}) ->
    push(LServer, user_receive_packet),
    Packet.

s2s_send_packet(#jid{lserver=LServer}, _To, _Packet) ->
    push(LServer, s2s_send_packet).
s2s_receive_packet(_From, #jid{lserver=LServer}, _Packet) ->
    push(LServer, s2s_receive_packet).

remove_user(_User, Server) ->
    push(jid:nameprep(Server), remove_user).
register_user(_User, Server) ->
    push(jid:nameprep(Server), register_user).

%%====================================================================
%% metrics push handler
%%====================================================================

push(Host, Probe) ->
    spawn(?MODULE, send_metrics, [Host, Probe, {127,0,0,1}, 11111]).

send_metrics(Host, Probe, Peer, Port) ->
    % our default metrics handler is https://github.com/processone/grapherl
    % grapherl metrics are named first with service domain, then nodename
    % and name of the data itself, followed by type timestamp and value
    % example => process-one.net/xmpp-1.user_receive_packet:c/1441784958:1
    [_, NodeId] = str:tokens(jlib:atom_to_binary(node()), <<"@">>),
    [Node | _] = str:tokens(NodeId, <<".">>),
    BaseId = <<Host/binary, "/", Node/binary, ".">>,
    DateTime = erlang:universaltime(),
    UnixTime = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
    TS = integer_to_binary(UnixTime),
    case gen_udp:open(0) of
	{ok, Socket} ->
	    case Probe of
		{Key, Val} ->
		    BVal = integer_to_binary(Val),
		    Data = <<BaseId/binary, (jlib:atom_to_binary(Key))/binary,
			    ":g/", TS/binary, ":", BVal/binary>>,
		    gen_udp:send(Socket, Peer, Port, Data);
		Key ->
		    Data = <<BaseId/binary, (jlib:atom_to_binary(Key))/binary,
			    ":c/", TS/binary, ":1">>,
		    gen_udp:send(Socket, Peer, Port, Data)
	    end,
	    gen_udp:close(Socket);
	Error ->
	    ?WARNING_MSG("can not open udp socket to grapherl: ~p", [Error])
    end.
