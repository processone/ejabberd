%%%----------------------------------------------------------------------
%%% File    : shaper.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Functions to control connections traffic
%%% Created :  9 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(shaper).

-author('alexey@process-one.net').

-export([start/0, new/1, new1/1, update/2, get_max_rate/1,
         transform_options/1, load_from_config/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(maxrate, {maxrate  = 0   :: integer(),
                  lastrate = 0.0 :: float(),
                  lasttime = 0   :: integer()}).

-record(shaper, {name    :: {atom(), global},
                 maxrate :: integer()}).

-type shaper() :: none | #maxrate{}.

-export_type([shaper/0]).

-spec start() -> ok.

start() ->
    mnesia:create_table(shaper,
                        [{ram_copies, [node()]},
                         {local_content, true},
			 {attributes, record_info(fields, shaper)}]),
    mnesia:add_table_copy(shaper, node(), ram_copies),
    load_from_config(),
    ok.

-spec load_from_config() -> ok | {error, any()}.

load_from_config() ->
    Shapers = ejabberd_config:get_option(
                shaper, fun(V) -> V end, []),
    case mnesia:transaction(
           fun() ->
                   lists:foreach(
                     fun({Name, MaxRate}) ->
                             mnesia:write(#shaper{name = {Name, global},
                                                  maxrate = MaxRate})
                     end, Shapers)
           end) of
        {atomic, ok} ->
            ok;
        Err ->
            {error, Err}
    end.

-spec get_max_rate(atom()) -> none | non_neg_integer().

get_max_rate(none) ->
    none;
get_max_rate(Name) ->
    case ets:lookup(shaper, {Name, global}) of
	[#shaper{maxrate = R}] ->
	    R;
	[] ->
	    none
    end.

-spec new(atom()) -> shaper().

new(none) ->
    none;
new(Name) ->
    MaxRate = case ets:lookup(shaper, {Name, global}) of
                  [#shaper{maxrate = R}] ->
                      R;
                  [] ->
                      none
              end,
    new1(MaxRate).

-spec new1(none | integer()) -> shaper().

new1(none) -> none;
new1(MaxRate) ->
    #maxrate{maxrate = MaxRate, lastrate = 0.0,
	     lasttime = now_to_usec(now())}.

-spec update(shaper(), integer()) -> {shaper(), integer()}.

update(none, _Size) -> {none, 0};
update(#maxrate{} = State, Size) ->
    MinInterv = 1000 * Size /
		  (2 * State#maxrate.maxrate - State#maxrate.lastrate),
    Interv = (now_to_usec(now()) - State#maxrate.lasttime) /
	       1000,
    ?DEBUG("State: ~p, Size=~p~nM=~p, I=~p~n",
	   [State, Size, MinInterv, Interv]),
    Pause = if MinInterv > Interv ->
		   1 + trunc(MinInterv - Interv);
	       true -> 0
	    end,
    NextNow = now_to_usec(now()) + Pause * 1000,
    {State#maxrate{lastrate =
		       (State#maxrate.lastrate +
			  1000000 * Size / (NextNow - State#maxrate.lasttime))
			 / 2,
		   lasttime = NextNow},
     Pause}.

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({OptName, Name, {maxrate, N}}, Opts) when OptName == shaper ->
    [{shaper, [{Name, N}]}|Opts];
transform_options({OptName, Name, none}, Opts) when OptName == shaper ->
    [{shaper, [{Name, none}]}|Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

now_to_usec({MSec, Sec, USec}) ->
    (MSec * 1000000 + Sec) * 1000000 + USec.
