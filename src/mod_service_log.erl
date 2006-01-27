%%%----------------------------------------------------------------------
%%% File    : mod_service_log.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Copy user messages to logger service
%%% Created : 24 Aug 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_service_log).
-author('alexey@sevcom.net').

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
    log_packet(From, To, Packet).

log_user_receive(_JID, From, To, Packet) ->
    log_packet(From, To, Packet).


log_packet(From, To, {xmlelement, Name, Attrs, Els}) ->
    Host = From#jid.lserver,
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
    
