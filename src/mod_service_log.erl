%%%----------------------------------------------------------------------
%%% File    : mod_service_log.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Copy user messages to logger service
%%% Created : 24 Aug 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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
%%%----------------------------------------------------------------------

-module(mod_service_log).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, log_user_send/1, mod_options/1,
	 log_user_receive/1, mod_opt_type/1, depends/2, mod_doc/0]).

-include("logger.hrl").
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").

start(_Host, _Opts) ->
    {ok, [{hook, user_send_packet, log_user_send, 50},
          {hook, user_receive_packet, log_user_receive, 50}]}.

stop(_Host) ->
    ok.

depends(_Host, _Opts) ->
    [].

-spec log_user_send({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
log_user_send({Packet, C2SState}) ->
    From = xmpp:get_from(Packet),
    log_packet(Packet, From#jid.lserver),
    {Packet, C2SState}.

-spec log_user_receive({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
log_user_receive({Packet, C2SState}) ->
    To = xmpp:get_to(Packet),
    log_packet(Packet, To#jid.lserver),
    {Packet, C2SState}.

-spec log_packet(stanza(), binary()) -> ok.
log_packet(Packet, Host) ->
    Loggers = mod_service_log_opt:loggers(Host),
    ForwardedMsg = #message{from = jid:make(Host),
			    id = p1_rand:get_string(),
			    sub_els = [#forwarded{
					  sub_els = [Packet]}]},
    lists:foreach(
      fun(Logger) ->
	      ejabberd_router:route(xmpp:set_to(ForwardedMsg, jid:make(Logger)))
      end, Loggers).

mod_opt_type(loggers) ->
    econf:list(econf:domain()).

mod_options(_) ->
    [{loggers, []}].

mod_doc() ->
    #{desc =>
          ?T("This module forwards copies of all stanzas "
             "to remote XMPP servers or components. "
             "Every stanza is encapsulated into <forwarded/> "
             "element as described in "
             "https://xmpp.org/extensions/xep-0297.html"
             "[XEP-0297: Stanza Forwarding]."),
      opts =>
          [{loggers,
            #{value => "[Domain, ...]",
              desc =>
                  ?T("A list of servers or connected components "
                     "to which stanzas will be forwarded.")}}],
      example =>
          ["modules:",
           "  ...",
           "  mod_service_log:",
           "    loggers:",
           "      - xmpp-server.tld",
           "      - component.domain.tld",
           "  ..."]}.
