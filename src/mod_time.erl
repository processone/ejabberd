%%%----------------------------------------------------------------------
%%% File    : mod_time.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Purpose :
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_time).

-author('alexey@process-one.net').

-protocol({xep, 202, '2.0'}).

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3,
	 mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_TIME, ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_TIME).

process_local_iq(_From, _To,
		 #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  Now_universal = calendar:universal_time(),
	  Now_local = calendar:universal_time_to_local_time(Now_universal),
	  {UTC, UTC_diff} = jlib:timestamp_to_iso(Now_universal,
						  utc),
	  Seconds_diff =
	      calendar:datetime_to_gregorian_seconds(Now_local) -
		calendar:datetime_to_gregorian_seconds(Now_universal),
	  {Hd, Md, _} =
	      calendar:seconds_to_time(abs(Seconds_diff)),
	  {_, TZO_diff} = jlib:timestamp_to_iso({{0, 1, 1},
						 {0, 0, 0}},
						{sign(Seconds_diff), {Hd, Md}}),
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"time">>,
			    attrs = [{<<"xmlns">>, ?NS_TIME}],
			    children =
				[#xmlel{name = <<"tzo">>, attrs = [],
					children = [{xmlcdata, TZO_diff}]},
				 #xmlel{name = <<"utc">>, attrs = [],
					children =
					    [{xmlcdata,
                                              <<UTC/binary,
                                                UTC_diff/binary>>}]}]}]}
    end.

sign(N) when N < 0 -> <<"-">>;
sign(_) -> <<"+">>.

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [iqdisc].
