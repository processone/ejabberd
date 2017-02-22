%%%----------------------------------------------------------------------
%%% File    : mod_sic.erl
%%% Author  : Karim Gemayel <karim.gemayel@process-one.net>
%%% Purpose : XEP-0279 Server IP Check
%%% Created : 6 Mar 2010 by Karim Gemayel <karim.gemayel@process-one.net>
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

-module(mod_sic).

-protocol({xep, 279, '0.2'}).

-author('karim.gemayel@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq/1,
	 process_sm_iq/1, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_SIC_0,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_SIC_0,
				  ?MODULE, process_sm_iq, IQDisc),    
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_SIC_1,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_SIC_1,
				  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_SIC_0),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_SIC_0),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_SIC_1),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_SIC_1).

reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_SIC_0,
					  ?MODULE, process_local_iq, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_SIC_0,
					  ?MODULE, process_sm_iq, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_SIC_1,
					  ?MODULE, process_local_iq, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_SIC_1,
					  ?MODULE, process_sm_iq, IQDisc);
	true ->
	    ok
    end.

depends(_Host, _Opts) ->
    [].

process_local_iq(#iq{from = #jid{user = User, server = Server,
				 resource = Resource},
		     type = get} = IQ) ->
    get_ip({User, Server, Resource}, IQ);
process_local_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)).

process_sm_iq(#iq{from = #jid{user = User, server = Server,
			      resource = Resource},
		  to = #jid{user = User, server = Server},
		  type = get} = IQ) ->
    get_ip({User, Server, Resource}, IQ);
process_sm_iq(#iq{type = get, lang = Lang} = IQ) ->
    Txt = <<"Query to another users is forbidden">>,
    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang));
process_sm_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
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
	    Txt = <<"User session not found">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang))
    end.

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [iqdisc].
