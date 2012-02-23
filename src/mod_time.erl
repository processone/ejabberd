%%%----------------------------------------------------------------------
%%% File    : mod_time.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
	 process_local_iq90/3, % TODO: Remove once XEP-0090 is Obsolete
	 process_local_iq/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").


start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    %% TODO: Remove the next two lines once XEP-0090 is Obsolete
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_TIME_OLD,
				  ?MODULE, process_local_iq90, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_TIME,
				  ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    %% TODO: Remove the next two lines once XEP-0090 is Obsolete
    gen_iq_handler:remove_iq_handler(ejabberd_local, list_to_binary(Host), ?NS_TIME_OLD),
    gen_iq_handler:remove_iq_handler(ejabberd_local, list_to_binary(Host), ?NS_TIME).

process_local_iq(_From, _To, #iq{type = get} = IQ_Rec) ->
	    Now = now(),
	    Now_universal = calendar:now_to_universal_time(Now),
	    Now_local = calendar:now_to_local_time(Now),
	    {UTC, UTC_diff} = jlib:timestamp_to_iso(Now_universal, utc),
	    Seconds_diff = calendar:datetime_to_gregorian_seconds(Now_local)
	     - calendar:datetime_to_gregorian_seconds(Now_universal),
    {Hd, Md, _} = calendar:seconds_to_time(abs(Seconds_diff)),
    {_, TZO_diff} = jlib:timestamp_to_iso({{0, 0, 0}, {0, 0, 0}}, {sign(Seconds_diff), {Hd, Md}}),
    Result = #xmlel{ns = ?NS_TIME, name = 'time', children = [
	    	#xmlel{ns = ?NS_TIME, name = 'tzo', children = [
		    #xmlcdata{cdata = list_to_binary(TZO_diff)}]},
	    	#xmlel{ns = ?NS_TIME, name = 'utc', children = [
		    #xmlcdata{cdata = list_to_binary(UTC ++ UTC_diff)}]}
      ]},
    exmpp_iq:result(IQ_Rec, Result);
process_local_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

%% TODO: Remove this function once XEP-0090 is Obsolete
process_local_iq90(_From, _To, #iq{type = get} = IQ_Rec) ->
    UTC = jlib:timestamp_to_iso(calendar:universal_time()),
    Result = #xmlel{ns = ?NS_TIME_OLD, name = 'query',
		    children = [#xmlel{ns = ?NS_TIME_OLD, name = 'utc',
				       children=[#xmlcdata{cdata =
							   list_to_binary(UTC)}
						]}
			       ]},
    exmpp_iq:result(IQ_Rec, Result);
process_local_iq90(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

sign(N) when N < 0 -> "-";
sign(_)            -> "+".
