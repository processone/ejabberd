%%%----------------------------------------------------------------------
%%% File    : mod_service_log.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Copy user messages to logger service
%%% Created : 24 Aug 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_service_log).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 log_user_send/3,
	 log_user_receive/4]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

start(Host, _Opts) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:add(user_send_packet, HostB,
		       ?MODULE, log_user_send, 50),
    ejabberd_hooks:add(user_receive_packet, HostB,
		       ?MODULE, log_user_receive, 50),
    ok.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(user_send_packet, HostB,
			  ?MODULE, log_user_send, 50),
    ejabberd_hooks:delete(user_receive_packet, HostB,
			  ?MODULE, log_user_receive, 50),
    ok.

log_user_send(From, To, Packet) ->
    log_packet(From, To, Packet, exmpp_jid:prep_domain_as_list(From)).

log_user_receive(_JID, From, To, Packet) ->
    log_packet(From, To, Packet, exmpp_jid:prep_domain_as_list(To)).


log_packet(From, To, Packet, Host) ->
    Loggers = gen_mod:get_module_opt(Host, ?MODULE, loggers, []),
    ServerJID = exmpp_jid:make(Host),
    FixedPacket = exmpp_stanza:set_jids(Packet, From, To),
    lists:foreach(
      fun(Logger) ->
	      ejabberd_router:route(
		ServerJID,
		exmpp_jid:make(Logger),
		#xmlel{name = 'route', children = [FixedPacket]})
      end, Loggers).
    
