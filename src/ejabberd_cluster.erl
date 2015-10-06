%%%----------------------------------------------------------------------
%%% File    : ejabberd_cluster.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Ejabberd clustering management
%%% Created : 7 Oct 2015 by Christophe Romain <christophe.romain@process-one.net>
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

-module(ejabberd_cluster).

%% API
-export([get_nodes/0, call/4, multicall/3, multicall/4]).

-include("ejabberd.hrl").
-include("logger.hrl").

-spec get_nodes() -> [node()].

get_nodes() ->
    mnesia:system_info(running_db_nodes).

-spec call(node(), module(), atom(), [any()]) -> any().

call(Node, Module, Function, Args) ->
    rpc:call(Node, Module, Function, Args, 5000).

-spec multicall(module(), atom(), [any()]) -> {list(), [node()]}.

multicall(Module, Function, Args) ->
    multicall(get_nodes(), Module, Function, Args).

-spec multicall([node()], module(), atom(), list()) -> {list(), [node()]}.

multicall(Nodes, Module, Function, Args) ->
    rpc:multicall(Nodes, Module, Function, Args, 5000).

