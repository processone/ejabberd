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

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, _Opts) ->
    ejabberd_hooks:add(user_send_packet, Host,
		       ?MODULE, log_user_send, 50),
    ejabberd_hooks:add(user_receive_packet, Host,
		       ?MODULE, log_user_receive, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, log_user_send, 50),
    ejabberd_hooks:delete(user_receive_packet, Host,
			  ?MODULE, log_user_receive, 50),
    ok.

log_user_send(From, To, Packet) ->
    log_packet(From, To, Packet, From#jid.lserver).

log_user_receive(_JID, From, To, Packet) ->
    log_packet(From, To, Packet, To#jid.lserver).


log_packet(From, To, {xmlelement, Name, Attrs, Els}, Host) ->
    Loggers = gen_mod:get_module_opt(Host, ?MODULE, loggers, []),
    ServerJID = #jid{user = "", server = Host, resource = "",
		     luser = "", lserver = Host, lresource = ""},
    NewAttrs = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
					  jlib:jid_to_string(To),
					  Attrs),
    FixedPacket = {xmlelement, Name, NewAttrs, Els},
    lists:foreach(
      fun(Logger) ->
	      ejabberd_router:route(
		ServerJID,
		#jid{user = "", server = Logger, resource = "",
		     luser = "", lserver = Logger, lresource = ""},
		{xmlelement, "route", [], [FixedPacket]})
      end, Loggers).
    
