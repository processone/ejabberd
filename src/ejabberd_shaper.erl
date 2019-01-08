%%%----------------------------------------------------------------------
%%% File    : ejabberd_shaper.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Functions to control connections traffic
%%% Created :  9 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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

-module(ejabberd_shaper).

-behaviour(gen_server).
-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start_link/0, new/1, update/2,
	 get_max_rate/1, transform_options/1, load_from_config/0,
	 opt_type/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").

-record(shaper, {name :: {atom(), global},
		 maxrate :: integer(),
		 burst_size :: integer()}).

-record(state, {}).

-type shaper() :: none | p1_shaper:state().
-export_type([shaper/0]).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ejabberd_mnesia:create(?MODULE, shaper,
			   [{ram_copies, [node()]},
			    {local_content, true},
			    {attributes, record_info(fields, shaper)}]),
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
    Shapers = ejabberd_config:get_option(shaper, []),
    case mnesia:transaction(
	fun() ->
	    lists:foreach(
		fun({Name, MaxRate, BurstSize}) ->
		    mnesia:write(
			#shaper{name = {Name, global},
				maxrate = MaxRate,
				burst_size = BurstSize})
		end,
		Shapers)
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
    case ets:lookup(shaper, {Name, global}) of
	[#shaper{maxrate = R, burst_size = B}] ->
	    p1_shaper:new(R, B);
	[] ->
	    none
    end.

-spec update(shaper(), integer()) -> {shaper(), integer()}.
update(none, _Size) -> {none, 0};
update(Shaper, Size) ->
    Result = p1_shaper:update(Shaper, Size),
    ?DEBUG("Shaper update:~n~s =>~n~s",
	   [p1_shaper:pp(Shaper), p1_shaper:pp(Result)]),
    Result.

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({shaper, Name, {maxrate, N}}, Opts) ->
    [{shaper, [{Name, N}]} | Opts];
transform_options({shaper, Name, none}, Opts) ->
    [{shaper, [{Name, none}]} | Opts];
transform_options({shaper, List}, Opts) when is_list(List) ->
    R = lists:map(
	fun({Name, Args}) when is_list(Args) ->
	    MaxRate = proplists:get_value(rate, Args, 1000),
	    BurstSize = proplists:get_value(burst_size, Args, MaxRate),
	    {Name, MaxRate, BurstSize};
	   ({Name, Val}) ->
	       {Name, Val, Val}
	end, List),
    [{shaper, R} | Opts];
transform_options(Opt, Opts) ->
    [Opt | Opts].

-spec opt_type(atom()) -> fun((any()) -> any()) | [atom()].
opt_type(shaper) -> fun(V) -> V end;
opt_type(_) -> [shaper].
