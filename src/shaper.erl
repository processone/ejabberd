%%%----------------------------------------------------------------------
%%% File    : shaper.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Functions to control connections traffic
%%% Created :  9 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(shaper).
-author('alexey@process-one.net').

-export([new/1, new1/1, update/2]).

-include("ejabberd.hrl").

-record(maxrate, {maxrate, lastrate, lasttime}).


new(Name) ->
    Data = case ejabberd_config:get_global_option({shaper, Name, global}) of
	       undefined ->
		   none;
	       D ->
		   D
	   end,
    new1(Data).


new1(none) ->
    none;
new1({maxrate, MaxRate}) ->
    #maxrate{maxrate = MaxRate,
	     lastrate = 0,
	     lasttime = now_to_usec(now())}.


update(none, _Size) ->
    {none, 0};
update(#maxrate{} = State, Size) ->
    MinInterv = 1000 * Size /
	(2 * State#maxrate.maxrate - State#maxrate.lastrate),
    Interv = (now_to_usec(now()) - State#maxrate.lasttime) / 1000,
    ?DEBUG("State: ~p, Size=~p~nM=~p, I=~p~n",
              [State, Size, MinInterv, Interv]),
    Pause = if
		MinInterv > Interv ->
		    1 + trunc(MinInterv - Interv);
		true ->
		    0
	    end,
    NextNow = now_to_usec(now()) + Pause * 1000,
    {State#maxrate{
       lastrate = (State#maxrate.lastrate +
		   1000000 * Size / (NextNow - State#maxrate.lasttime))/2,
       lasttime = NextNow},
     Pause}.


now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.
