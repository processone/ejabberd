%%%----------------------------------------------------------------------
%%% File    : nodetree_virtual.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Standard node tree plugin using no storage backend
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
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

%%% @doc The module <strong>{@module}</strong> is the PubSub node tree plugin that
%%% allow virtual nodes handling. This prevent storage of nodes.
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p>This plugin development is still a work in progress. Due to optimizations in
%%% mod_pubsub, this plugin can not work anymore without altering functioning.
%%% Please, send us comments, feedback and improvements.</p>

-module(nodetree_virtual).
-behaviour(gen_pubsub_nodetree).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-export([init/3, terminate/2, options/0, set_node/1,
    get_node/3, get_node/2, get_node/1, get_nodes/2,
    get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
    get_subnodes/3, get_subnodes_tree/3, create_node/6,
    delete_node/2]).

init(_Host, _ServerHost, _Opts) ->
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{virtual_tree, true}].

set_node(_Node) ->
    ok.

get_node(Host, Node, _From) ->
    get_node(Host, Node).

get_node(Host, Node) ->
    Nidx = nodeidx(Host, Node),
    node_record(Host, Node, Nidx).

get_node(Nidx) ->
    {Host, Node} = nodeid(Nidx),
    node_record(Host, Node, Nidx).

get_nodes(Host, _From) ->
    get_nodes(Host).

get_nodes(_Host) ->
    [].

get_parentnodes(_Host, _Node, _From) ->
    [].

get_parentnodes_tree(Host, Node, From) ->
    [{0, [get_node(Host, Node, From)]}].

get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).

get_subnodes(_Host, _Node) ->
    [].

get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).

get_subnodes_tree(_Host, _Node) ->
    [].

create_node(Host, Node, _Type, _Owner, _Options, _Parents) ->
    {error, {virtual, nodeidx(Host, Node)}}.

delete_node(Host, Node) ->
    [get_node(Host, Node)].

%% internal helper

node_record({U,S,R}, Node, Nidx) ->
    Host = mod_pubsub:host(S),
    Type = <<"pep">>,
    Module = mod_pubsub:plugin(Host, Type),
    #pubsub_node{nodeid = {{U,S,R},Node}, id = Nidx, type = Type,
                 owners = [{U,S,R}],
                 options = Module:options()};
node_record(Host, Node, Nidx) ->
    [Type|_] = mod_pubsub:plugins(Host),
    Module = mod_pubsub:plugin(Host, Type),
    #pubsub_node{nodeid = {Host, Node}, id = Nidx, type = Type,
                 owners = [{<<"">>, Host, <<"">>}],
                 options = Module:options()}.

nodeidx({U,S,R}, Node) ->
    JID = jid:encode(jid:make(U,S,R)),
    <<JID/binary, ":", Node/binary>>;
nodeidx(Host, Node) ->
    <<Host/binary, ":", Node/binary>>.
nodeid(Nidx) ->
    [Head, Node] = binary:split(Nidx, <<":">>),
    case jid:decode(Head) of
        {jid,<<>>,Host,<<>>,_,_,_} -> {Host, Node};
        {jid,U,S,R,_,_,_} -> {{U,S,R}, Node}
    end.
