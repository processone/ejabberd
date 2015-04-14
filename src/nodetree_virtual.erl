%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the PubSub node tree plugin that
%%% allow virtual nodes handling.
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p>This plugin development is still a work in progress. Due to optimizations in
%%% mod_pubsub, this plugin can not work anymore without altering functioning.
%%% Please, send us comments, feedback and improvements.</p>

-module(nodetree_virtual).

-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-include("jlib.hrl").

-behaviour(gen_pubsub_nodetree).

-export([init/3, terminate/2, options/0, set_node/1,
	 get_node/3, get_node/2, get_node/1, get_nodes/2,
	 get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
	 get_subnodes/3, get_subnodes_tree/3, create_node/6,
	 delete_node/2]).

%% ================
%% API definition
%% ================

%% @spec (Host, ServerHost, Opts) -> any()
%%     Host = mod_pubsub:host()
%%     ServerHost = host()
%%     Opts = list()
%% @doc <p>Called during pubsub modules initialisation. Any pubsub plugin must
%% implement this function. It can return anything.</p>
%% <p>This function is mainly used to trigger the setup task necessary for the
%% plugin. It can be used for example by the developer to create the specific
%% module database schema if it does not exists yet.</p>
%% @spec () -> [Option]
%%     Option = mod_pubsub:nodetreeOption()
%% @doc <p>Returns the default pubsub node tree options.</p>
%% @spec (NodeRecord) -> ok | {error, Reason}
%%     NodeRecord = mod_pubsub:pubsub_node()
%% @doc <p>No node record is stored on database. Just do nothing.</p>
%% @spec (Host, Node, From) -> pubsubNode()
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
%% @doc <p>Virtual node tree does not handle a node database. Any node is considered
%% as existing. Node record contains default values.</p>
init(_Host, _ServerHost, _Opts) -> ok.

terminate(_Host, _ServerHost) -> ok.

options() -> [{virtual_tree, true}].

set_node(_NodeRecord) -> ok.

get_node(Host, Node, _From) -> get_node(Host, Node).

get_node(Host, Node) -> get_node({Host, Node}).

get_node({Host, _} = NodeId) ->
    Record = #pubsub_node{nodeid = NodeId, id = NodeId},
    Module = jlib:binary_to_atom(<<"node_",
				     (Record#pubsub_node.type)/binary>>),
    Options = Module:options(),
    Owners = [{<<"">>, Host, <<"">>}],
    Record#pubsub_node{owners = Owners, options = Options}.

%% @spec (Host, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
%%     From = mod_pubsub:jid()
%% @doc <p>Virtual node tree does not handle a node database. Any node is considered
%% as existing. Nodes list can not be determined.</p>
%% @spec (Host, Node, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
%% @doc <p>Virtual node tree does not handle parent/child. Child list is empty.</p>
%% @spec (Host, Node, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
%% @doc <p>Virtual node tree does not handle parent/child. Child list is empty.</p>
%% @spec (Host, Node, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
%% @doc <p>Virtual node tree does not handle parent/child. Child list is empty.</p>
get_nodes(Host, _From) -> get_nodes(Host).

get_nodes(_Host) -> [].

get_parentnodes(_Host, _Node, _From) -> [].

-spec(get_parentnodes_tree/3 ::
(
  Host   :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId(),
  From   :: jid())
    -> [{0, [mod_pubsub:pubsubNode(),...]}]
).
get_parentnodes_tree(Host, NodeId, From) ->
    case get_node(Host, NodeId, From) of
      Node when is_record(Node, pubsub_node) -> [{0, [Node]}];
      _Error -> []
    end.

get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).
%% @spec (Host, Node, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
%% @doc <p>Virtual node tree does not handle parent/child. Child list is empty.</p>

get_subnodes(_Host, _Node) -> [].

get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).
%% @spec (Host, Node, Type, Owner, Options, Parents) -> ok
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     Type = mod_pubsub:nodeType()
%%     Owner = mod_pubsub:jid()
%%     Options = list()
%% @doc <p>No node record is stored on database. Any valid node
%% is considered as already created.</p>
%% <p>default allowed nodes: /home/host/user/any/node/name</p>
%% @spec (Host, Node) -> [mod_pubsub:node()]
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%% @doc <p>Virtual node tree does not handle parent/child.
%% node deletion just affects the corresponding node.</p>

get_subnodes_tree(_Host, _Node) -> [].

create_node(Host, Node, _Type, _Owner, _Options,
	    _Parents) ->
    {error, {virtual, {Host, Node}}}.

delete_node(Host, Node) -> [get_node(Host, Node)].
