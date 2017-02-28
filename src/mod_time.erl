%%%----------------------------------------------------------------------
%%% File    : mod_time.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Purpose :
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_time).

-author('alexey@process-one.net').

-protocol({xep, 202, '2.0'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq/1,
	 mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_TIME, ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_TIME).

reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_TIME,
					  ?MODULE, process_local_iq, IQDisc);
	true ->
	    ok
    end.

process_local_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_local_iq(#iq{type = get} = IQ) ->
    Now = p1_time_compat:timestamp(),
    Now_universal = calendar:now_to_universal_time(Now),
    Now_local = calendar:universal_time_to_local_time(Now_universal),
    Seconds_diff =
	calendar:datetime_to_gregorian_seconds(Now_local) -
	calendar:datetime_to_gregorian_seconds(Now_universal),
    {Hd, Md, _} = calendar:seconds_to_time(abs(Seconds_diff)),
    xmpp:make_iq_result(IQ, #time{tzo = {Hd, Md}, utc = Now}).

depends(_Host, _Opts) ->
    [].

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [iqdisc].
