%%%----------------------------------------------------------------------
%%% File    : mod_service_log.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Copy user messages to logger service
%%% Created : 24 Aug 2003 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(mod_service_log).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, log_user_send/1,
	 log_user_receive/1, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

start(Host, _Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       log_user_send, 50),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       log_user_receive, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  log_user_send, 50),
    ejabberd_hooks:delete(user_receive_packet, Host,
			  ?MODULE, log_user_receive, 50),
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
    Loggers = gen_mod:get_module_opt(Host, ?MODULE, loggers,
				     mod_opt_type(loggers), []),
    ForwardedMsg = #message{from = jid:make(Host),
			    id = randoms:get_string(),
			    sub_els = [#forwarded{
					  xml_els = [xmpp:encode(Packet)]}]},
    lists:foreach(
      fun(Logger) ->
	      ejabberd_router:route(xmpp:set_to(ForwardedMsg, jid:make(Logger)))
      end, Loggers).

mod_opt_type(loggers) ->
    fun (L) ->
	    lists:map(fun (S) ->
			      B = iolist_to_binary(S),
			      N = jid:nameprep(B),
			      if N /= error -> N end
		      end,
		      L)
    end;
mod_opt_type(_) -> [loggers].
