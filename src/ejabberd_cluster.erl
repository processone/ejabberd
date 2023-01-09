%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  5 Jul 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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
%%%-------------------------------------------------------------------
-module(ejabberd_cluster).
-behaviour(gen_server).

%% API
-export([start_link/0, call/4, call/5, multicall/3, multicall/4, multicall/5,
	 eval_everywhere/3, eval_everywhere/4]).
%% Backend dependent API
-export([get_nodes/0, get_known_nodes/0, join/1, leave/1, subscribe/0,
	 subscribe/1, node_id/0, get_node_by_id/1, send/2, wait_for_sync/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% hooks
-export([set_ticktime/0]).

-include("logger.hrl").

-type dst() :: pid() | atom() | {atom(), node()}.

-callback init() -> ok | {error, any()}.
-callback get_nodes() -> [node()].
-callback get_known_nodes() -> [node()].
-callback join(node()) -> ok | {error, any()}.
-callback leave(node()) -> ok | {error, any()}.
-callback node_id() -> binary().
-callback get_node_by_id(binary()) -> node().
-callback send({atom(), node()}, term()) -> boolean().
-callback wait_for_sync(timeout()) -> ok | {error, any()}.
-callback subscribe(dst()) -> ok.

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec call(node(), module(), atom(), [any()]) -> any().
call(Node, Module, Function, Args) ->
    call(Node, Module, Function, Args, rpc_timeout()).

-spec call(node(), module(), atom(), [any()], timeout()) -> any().
call(Node, Module, Function, Args, Timeout) ->
    rpc:call(Node, Module, Function, Args, Timeout).

-spec multicall(module(), atom(), [any()]) -> {list(), [node()]}.
multicall(Module, Function, Args) ->
    multicall(get_nodes(), Module, Function, Args).

-spec multicall([node()], module(), atom(), list()) -> {list(), [node()]}.
multicall(Nodes, Module, Function, Args) ->
    multicall(Nodes, Module, Function, Args, rpc_timeout()).

-spec multicall([node()], module(), atom(), list(), timeout()) -> {list(), [node()]}.
multicall(Nodes, Module, Function, Args, Timeout) ->
    rpc:multicall(Nodes, Module, Function, Args, Timeout).

-spec eval_everywhere(module(), atom(), [any()]) -> ok.
eval_everywhere(Module, Function, Args) ->
    eval_everywhere(get_nodes(), Module, Function, Args),
    ok.

-spec eval_everywhere([node()], module(), atom(), [any()]) -> ok.
eval_everywhere(Nodes, Module, Function, Args) ->
    rpc:eval_everywhere(Nodes, Module, Function, Args),
    ok.

%%%===================================================================
%%% Backend dependent API
%%%===================================================================
-spec get_nodes() -> [node()].
get_nodes() ->
    Mod = get_mod(),
    Mod:get_nodes().

-spec get_known_nodes() -> [node()].
get_known_nodes() ->
    Mod = get_mod(),
    Mod:get_known_nodes().

-spec join(node()) -> ok | {error, any()}.
join(Node) ->
    Mod = get_mod(),
    Mod:join(Node).

-spec leave(node()) -> ok | {error, any()}.
leave(Node) ->
    Mod = get_mod(),
    Mod:leave(Node).

-spec node_id() -> binary().
node_id() ->
    Mod = get_mod(),
    Mod:node_id().

-spec get_node_by_id(binary()) -> node().
get_node_by_id(ID) ->
    Mod = get_mod(),
    Mod:get_node_by_id(ID).

%% Note that false positive returns are possible, while false negatives are not.
%% In other words: positive return value (i.e. 'true') doesn't guarantee
%% successful delivery, while negative return value ('false') means
%% the delivery has definitely failed.
-spec send(dst(), term()) -> boolean().
send({Name, Node}, Msg) when Node == node() ->
    send(Name, Msg);
send(undefined, _Msg) ->
    false;
send(Name, Msg) when is_atom(Name) ->
    send(whereis(Name), Msg);
send(Pid, Msg) when is_pid(Pid) andalso node(Pid) == node() ->
    case erlang:is_process_alive(Pid) of
	true ->
	    erlang:send(Pid, Msg),
	    true;
	false ->
	    false
    end;
send(Dst, Msg) ->
    Mod = get_mod(),
    Mod:send(Dst, Msg).

-spec wait_for_sync(timeout()) -> ok | {error, any()}.
wait_for_sync(Timeout) ->
    Mod = get_mod(),
    Mod:wait_for_sync(Timeout).

-spec subscribe() -> ok.
subscribe() ->
    subscribe(self()).

-spec subscribe(dst()) -> ok.
subscribe(Proc) ->
    Mod = get_mod(),
    Mod:subscribe(Proc).

%%%===================================================================
%%% Hooks
%%%===================================================================
set_ticktime() ->
    Ticktime = ejabberd_option:net_ticktime() div 1000,
    case net_kernel:set_net_ticktime(Ticktime) of
	{ongoing_change_to, Time} when Time /= Ticktime ->
	    ?ERROR_MSG("Failed to set new net_ticktime because "
		       "the net kernel is busy changing it to the "
		       "previously configured value. Please wait for "
		       "~B seconds and retry", [Time]);
	_ ->
	    ok
    end.

%%%===================================================================
%%% gen_server API
%%%===================================================================
init([]) ->
    set_ticktime(),
    Nodes = ejabberd_option:cluster_nodes(),
    lists:foreach(fun(Node) ->
                          net_kernel:connect_node(Node)
                  end, Nodes),
    Mod = get_mod(),
    case Mod:init() of
	ok ->
	    ejabberd_hooks:add(config_reloaded, ?MODULE, set_ticktime, 50),
	    Mod:subscribe(?MODULE),
	    {ok, #state{}};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({node_up, Node}, State) ->
    ?INFO_MSG("Node ~ts has joined", [Node]),
    {noreply, State};
handle_info({node_down, Node}, State) ->
    ?INFO_MSG("Node ~ts has left", [Node]),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, set_ticktime, 50).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_mod() ->
    Backend = ejabberd_option:cluster_backend(),
    list_to_existing_atom("ejabberd_cluster_" ++ atom_to_list(Backend)).

rpc_timeout() ->
    ejabberd_option:rpc_timeout().
