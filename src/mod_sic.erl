%%%----------------------------------------------------------------------
%%% File    : mod_sic.erl
%%% Author  : Karim Gemayel <karim.gemayel@process-one.net>
%%% Purpose : XEP-0279 Server IP Check
%%% Created : 6 Mar 2010 by Karim Gemayel <karim.gemayel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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

-module(mod_sic).

-protocol({xep, 279, '0.2', '2.1.3', "complete", ""}).

-author('karim.gemayel@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq/1,
	 process_sm_iq/1, mod_options/1, depends/2, mod_doc/0]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

start(_Host, _Opts) ->
    {ok, [{iq_handler, ejabberd_local, ?NS_SIC_0, process_local_iq},
          {iq_handler, ejabberd_sm, ?NS_SIC_0, process_sm_iq},
          {iq_handler, ejabberd_local, ?NS_SIC_1, process_local_iq},
          {iq_handler, ejabberd_sm, ?NS_SIC_1, process_sm_iq}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

process_local_iq(#iq{from = #jid{user = User, server = Server,
				 resource = Resource},
		     type = get} = IQ) ->
    get_ip({User, Server, Resource}, IQ);
process_local_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)).

process_sm_iq(#iq{from = #jid{user = User, server = Server,
			      resource = Resource},
		  to = #jid{user = User, server = Server},
		  type = get} = IQ) ->
    get_ip({User, Server, Resource}, IQ);
process_sm_iq(#iq{type = get, lang = Lang} = IQ) ->
    Txt = ?T("Query to another users is forbidden"),
    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang));
process_sm_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)).

get_ip({User, Server, Resource},
       #iq{lang = Lang, sub_els = [#sic{xmlns = NS}]} = IQ) ->
    case ejabberd_sm:get_user_ip(User, Server, Resource) of
	{IP, Port} when is_tuple(IP) ->
	    Result = case NS of
			 ?NS_SIC_0 -> #sic{ip = IP, xmlns = NS};
			 ?NS_SIC_1 -> #sic{ip = IP, port = Port, xmlns = NS}
		     end,
	    xmpp:make_iq_result(IQ, Result);
	_ ->
	    Txt = ?T("User session not found"),
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang))
    end.

mod_options(_Host) ->
    [].

mod_doc() ->
    #{desc =>
          [?T("This module adds support for "
              "https://xmpp.org/extensions/xep-0279.html"
              "[XEP-0279: Server IP Check]. This protocol enables "
              "a client to discover its external IP address."), "",
           ?T("WARNING: The protocol extension is deferred and seems "
              "like there are no clients supporting it, so using this "
              "module is not recommended and, furthermore, the module "
              "might be removed in the future.")]}.
