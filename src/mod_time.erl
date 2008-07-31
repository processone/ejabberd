%%%----------------------------------------------------------------------
%%% File    : mod_time.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(mod_time).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_local_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").


start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_TIME,
				  ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_TIME).

process_local_iq(_From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    UTC = jlib:timestamp_to_iso(calendar:universal_time()),
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_TIME}],
			     [{xmlelement, "utc", [],
			       [{xmlcdata, UTC}]}]}]}
    end.


