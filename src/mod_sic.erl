%%%----------------------------------------------------------------------
%%% File    : mod_sic.erl
%%% Author  : Karim Gemayel <karim.gemayel@process-one.net>
%%% Purpose : XEP-0279 Server IP Check
%%% Created : 6 Mar 2010 by Karim Gemayel <karim.gemayel@process-one.net>
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
%%%----------------------------------------------------------------------

-module(mod_sic).

-author('karim.gemayel@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3,
	 process_sm_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(NS_SIC, <<"urn:xmpp:sic:0">>).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_SIC, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_SIC, ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_SIC),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_SIC).

process_local_iq(#jid{user = User, server = Server,
		      resource = Resource},
		 _To, #iq{type = get, sub_el = _SubEl} = IQ) ->
    get_ip({User, Server, Resource}, IQ);
process_local_iq(_From, _To,
		 #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

process_sm_iq(#jid{user = User, server = Server,
		   resource = Resource},
	      #jid{user = User, server = Server},
	      #iq{type = get, sub_el = _SubEl} = IQ) ->
    get_ip({User, Server, Resource}, IQ);
process_sm_iq(_From, _To,
	      #iq{type = get, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]};
process_sm_iq(_From, _To,
	      #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

get_ip({User, Server, Resource},
       #iq{sub_el =
	       #xmlel{name = Name, attrs = Attrs} = SubEl} =
	   IQ) ->
    case ejabberd_sm:get_user_ip(User, Server, Resource) of
      {IP, _} when is_tuple(IP) ->
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = Name, attrs = Attrs,
			    children =
				[{xmlcdata,
				  iolist_to_binary(jlib:ip_to_list(IP))}]}]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
    end.
