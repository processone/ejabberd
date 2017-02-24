%%%----------------------------------------------------------------------
%%% File    : shaper.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Functions to control connections traffic
%%% Created :  9 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(shaper).

-behaviour(gen_server).
-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start_link/0, new/1, new1/1, update/2,
	 get_max_rate/1, transform_options/1, load_from_config/0,
	 opt_type/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(maxrate, {maxrate  = 0   :: integer(),
                  lastrate = 0.0 :: float(),
                  lasttime = 0   :: integer()}).

-record(shaper, {name    :: {atom(), global},
                 maxrate :: integer()}).

-record(state, {}).

-type shaper() :: none | #maxrate{}.

-export_type([shaper/0]).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ejabberd_mnesia:create(?MODULE, shaper,
                        [{ram_copies, [node()]},
                         {local_content, true},
			 {attributes, record_info(fields, shaper)}]),
    mnesia:add_table_copy(shaper, node(), ram_copies),
    ejabberd_hooks:add(config_reloaded, ?MODULE, load_from_config, 20),
    load_from_config(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
	     lasttime = p1_time_compat:system_time(micro_seconds)}.

-spec update(shaper(), integer()) -> {shaper(), integer()}.

update(none, _Size) -> {none, 0};
update(#maxrate{} = State, Size) ->
    MinInterv = 1000 * Size /
		  (2 * State#maxrate.maxrate - State#maxrate.lastrate),
    Interv = (p1_time_compat:system_time(micro_seconds) - State#maxrate.lasttime) /
	       1000,
    ?DEBUG("State: ~p, Size=~p~nM=~p, I=~p~n",
	   [State, Size, MinInterv, Interv]),
    Pause = if MinInterv > Interv ->
		   1 + trunc(MinInterv - Interv);
	       true -> 0
	    end,
    NextNow = p1_time_compat:system_time(micro_seconds) + Pause * 1000,
    Div = case NextNow - State#maxrate.lasttime of
        0 -> 1;
        V -> V
    end,
    {State#maxrate{lastrate =
		       (State#maxrate.lastrate +
			  1000000 * Size / Div)
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

opt_type(shaper) -> fun (V) -> V end;
opt_type(_) -> [shaper].
