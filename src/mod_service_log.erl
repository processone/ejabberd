%%%----------------------------------------------------------------------
%%% File    : mod_service_log.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Manage announce messages
%%% Created : 24 Aug 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_service_log).
-author('alexey@sevcom.net').

-behaviour(gen_mod).

-export([start/1,
	 stop/0,
	 log_user_send/3,
	 log_user_receive/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(_) ->
    ejabberd_hooks:add(user_send_packet,
		       ?MODULE, log_user_send, 50),
    ejabberd_hooks:add(user_receive_packet,
		       ?MODULE, log_user_receive, 50),
    ok.

stop() ->
    ejabberd_hooks:delete(user_send_packet,
			  ?MODULE, log_user_send, 50),
    ejabberd_hooks:delete(user_receive_packet,
			  ?MODULE, log_user_receive, 50),
    ok.

log_user_send(From, To, Packet) ->
    log_packet(From, To, Packet).

log_user_receive(_JID, From, To, Packet) ->
    log_packet(From, To, Packet).


log_packet(From, To, {xmlelement, Name, Attrs, Els}) ->
    Loggers = gen_mod:get_module_opt(?MODULE, loggers, []),
    ServerJID = #jid{user = "", server = ?MYNAME, resource = "",
		     luser = "", lserver = ?MYNAME, lresource = ""},
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
    
