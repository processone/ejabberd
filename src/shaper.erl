%%%----------------------------------------------------------------------
%%% File    : shaper.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Functions to control connections traffic
%%% Created :  9 Feb 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(shaper).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([new/1, new1/1, update/2]).

-record(maxrate, {maxrate, lastrate, lasttime}).


new(Name) ->
    Data = case ejabberd_config:get_global_option({shaper, Name}) of
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


update(none, Size) ->
    none;
update(#maxrate{} = State, Size) ->
    MinInterv = 1000 * Size /
	(2 * State#maxrate.maxrate - State#maxrate.lastrate),
    Interv = (now_to_usec(now()) - State#maxrate.lasttime) / 1000,
    %io:format("State: ~p, Size=~p~nM=~p, I=~p~n",
    %          [State, Size, MinInterv, Interv]),
    if
	MinInterv > Interv ->
	    timer:sleep(1 + trunc(MinInterv - Interv));
	true ->
	    ok
    end,
    Now = now_to_usec(now()),
    State#maxrate{
      lastrate = (State#maxrate.lastrate +
		  1000000 * Size / (Now - State#maxrate.lasttime))/2,
      lasttime = Now}.


now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.
