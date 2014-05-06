%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2014, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2014, ProcessOne.
%%%
%%%
%%% @copyright 2006-2014 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @private
%%% @doc <p>The module <strong>{@module}</strong> defines the PubSub node
%%% tree plugin behaviour. This behaviour is used to check that a PubSub
%%% node tree plugin respects the current ejabberd PubSub plugin API.</p>

-module(gen_pubsub_nodetree).

-include("jlib.hrl").

-type(host() :: mod_pubsub:host()
              | mod_pubsub_odbc:host()
).

-type(nodeId() :: mod_pubsub:nodeId()
                | mod_pubsub_odbc:nodeId()
).

-type(nodeIdx() :: mod_pubsub:nodeIdx()
                | mod_pubsub_odbc:nodeIdx()
).

-type(itemId() :: mod_pubsub:itemId()
                | mod_pubsub_odbc:itemId()
).

-type(pubsubNode() :: mod_pubsub:pubsubNode()
                    | mod_pubsub_odbc:pubsubNode()
).

-type(nodeOptions() :: mod_pubsub:nodeOptions()
                     | mod_pubsub_odbc:nodeOptions()
).

-callback init(Host :: host(),
               ServerHost :: binary(),
               Opts :: [any()]) -> atom().

-callback terminate(Host :: host(), ServerHost :: binary()) -> atom().

-callback options() -> nodeOptions().

-callback set_node(PubsubNode :: pubsubNode()) ->
    ok | {result, NodeIdx::mod_pubsub_odbc:nodeIdx()} | {error, xmlel()}.

-callback get_node(Host   :: host(),
                   NodeId :: nodeId(),
                   From   :: jid()) ->
    pubsubNode() |
    {error, xmlel()}.

-callback get_node(Host :: host(),
                   NodeId :: nodeId()) ->
    pubsubNode() |
    {error, xmlel()}.

-callback get_node(NodeIdx :: nodeIdx()) ->
    pubsubNode() |
    {error, xmlel()}.

-callback get_nodes(Host :: host(),
                   From :: jid())->
    [pubsubNode()].

-callback get_nodes(Host :: host())->
    [pubsubNode()].

-callback get_parentnodes(Host :: host(),
                          NodeId :: nodeId(),
                          From :: jid()) ->
    [pubsubNode()] |
    {error, xmlel()}.

-callback get_parentnodes_tree(Host :: host(),
                               NodeId :: nodeId(),
                               From :: jid()) ->
    [{0, [pubsubNode(),...]}].

-callback get_subnodes(Host :: host(),
                       NodeId :: nodeId(),
                       From :: ljid()) ->
    [pubsubNode()].

-callback get_subnodes_tree(Host :: host(),
                            NodeId :: nodeId(),
                            From :: ljid()) ->
    [pubsubNode()].

-callback create_node(Host :: host(),
                      NodeId :: nodeId(),
                      Type :: binary(),
                      Owner :: jid(),
                      Options :: nodeOptions(),
                      Parents :: [nodeId()]) ->
    {ok, NodeIdx::nodeIdx()} |
    {error, xmlel()}.

-callback delete_node(Host :: host(),
                      NodeId :: nodeId()) ->
    [pubsubNode()].
