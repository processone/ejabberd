%%%-------------------------------------------------------------------
%%% File    : mod_metrics.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Simple metrics handler for runtime statistics
%%% Created : 22 Oct 2015 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
-include("xmpp.hrl").

-export([start/2, stop/1, send_metrics/4, opt_type/1, mod_opt_type/1,
	 depends/2, reload/3]).

-export([offline_message_hook/1,
         sm_register_connection_hook/3, sm_remove_connection_hook/3,
         user_send_packet/1, user_receive_packet/1,
         s2s_send_packet/1, s2s_receive_packet/1,
         remove_user/2, register_user/2]).

%%====================================================================
%% API
%%====================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message_hook, 20),
    ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, sm_register_connection_hook, 20),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, sm_remove_connection_hook, 20),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 20),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, user_receive_packet, 20),
    ejabberd_hooks:add(s2s_send_packet, Host, ?MODULE, s2s_send_packet, 20),
    ejabberd_hooks:add(s2s_receive_packet, Host, ?MODULE, s2s_receive_packet, 20),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 20),
    ejabberd_hooks:add(register_user, Host, ?MODULE, register_user, 20).

stop(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_message_hook, 20),
    ejabberd_hooks:delete(sm_register_connection_hook, Host, ?MODULE, sm_register_connection_hook, 20),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, sm_remove_connection_hook, 20),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 20),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, user_receive_packet, 20),
    ejabberd_hooks:delete(s2s_send_packet, Host, ?MODULE, s2s_send_packet, 20),
    ejabberd_hooks:delete(s2s_receive_packet, Host, ?MODULE, s2s_receive_packet, 20),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 20),
    ejabberd_hooks:delete(register_user, Host, ?MODULE, register_user, 20).

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

-spec s2s_send_packet(stanza()) -> any().
s2s_send_packet(Packet) ->
    #jid{lserver = LServer} = xmpp:get_from(Packet),
    push(LServer, s2s_send_packet).

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

push(Host, Probe) ->
    spawn(?MODULE, send_metrics, [Host, Probe, {127,0,0,1}, 11111]).

send_metrics(Host, Probe, Peer, Port) ->
    % our default metrics handler is https://github.com/processone/grapherl
    % grapherl metrics are named first with service domain, then nodename
    % and name of the data itself, followed by type timestamp and value
    % example => process-one.net/xmpp-1.user_receive_packet:c/1441784958:1
    [_, NodeId] = str:tokens(misc:atom_to_binary(node()), <<"@">>),
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
		    Data = <<BaseId/binary, (misc:atom_to_binary(Key))/binary,
			    ":g/", TS/binary, ":", BVal/binary>>,
		    gen_udp:send(Socket, Peer, Port, Data);
		Key ->
		    Data = <<BaseId/binary, (misc:atom_to_binary(Key))/binary,
			    ":c/", TS/binary, ":1">>,
		    gen_udp:send(Socket, Peer, Port, Data)
	    end,
	    gen_udp:close(Socket);
	Error ->
	    ?WARNING_MSG("can not open udp socket to grapherl: ~p", [Error])
    end.

opt_type(_) ->
     [].

mod_opt_type(_) ->
    [].
