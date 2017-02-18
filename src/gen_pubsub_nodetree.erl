%%%----------------------------------------------------------------------
%%% File    : gen_pubsub_nodetree.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Define the pubsub node tree plugin behaviour
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

-module(gen_pubsub_nodetree).


-type(host() :: mod_pubsub:host()).
-type(nodeId() :: mod_pubsub:nodeId()).
-type(nodeIdx() :: mod_pubsub:nodeIdx()).
-type(pubsubNode() :: mod_pubsub:pubsubNode()).
-type(nodeOptions() :: mod_pubsub:nodeOptions()).

-callback init(Host :: host(),
	ServerHost :: binary(),
	Opts :: [any()]) -> atom().

-include("xmpp.hrl").

-callback terminate(Host :: host(), ServerHost :: binary()) -> atom().

-callback options() -> nodeOptions().

-callback set_node(PubsubNode :: pubsubNode()) ->
    ok | {result, NodeIdx::nodeIdx()} | {error, stanza_error()}.

-callback get_node(Host   :: host(),
	NodeId :: nodeId(),
	From   :: jid:jid()) ->
    pubsubNode() |
    {error, stanza_error()}.

-callback get_node(Host :: host(),
	NodeId :: nodeId()) ->
    pubsubNode() |
    {error, stanza_error()}.

-callback get_node(NodeIdx :: nodeIdx()) ->
    pubsubNode() |
    {error, stanza_error()}.

-callback get_nodes(Host :: host(),
	From :: jid:jid())->
    [pubsubNode()].

-callback get_nodes(Host :: host())->
    [pubsubNode()].

-callback get_parentnodes(Host :: host(),
	NodeId :: nodeId(),
	From :: jid:jid()) ->
    [pubsubNode()] |
    {error, stanza_error()}.

-callback get_parentnodes_tree(Host :: host(),
	NodeId :: nodeId(),
	From :: jid:jid()) ->
    [{0, [pubsubNode(),...]}].

-callback get_subnodes(Host :: host(),
	NodeId :: nodeId(),
	From :: jid:jid()) ->
    [pubsubNode()].

-callback get_subnodes_tree(Host :: host(),
	NodeId :: nodeId(),
	From :: jid:jid()) ->
    [pubsubNode()].

-callback create_node(Host :: host(),
	NodeId :: nodeId(),
	Type :: binary(),
	Owner :: jid:jid(),
	Options :: nodeOptions(),
	Parents :: [nodeId()]) ->
    {ok, NodeIdx::nodeIdx()} |
    {error, stanza_error()} |
    {error, {virtual, {host(), nodeId()}}}.

-callback delete_node(Host :: host(),
	NodeId :: nodeId()) ->
    [pubsubNode()].
