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
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%%
%%% @copyright 2006-2012 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the PubSub node tree plugin that
%%% allow virtual nodes handling.
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p>The development of this plugin is still a work in progress. Due to optimizations in
%%% mod_pubsub, this plugin can not work anymore without altering functioning.
%%% Please, send us comments, feedback and improvements.</p>

-module(nodetree_virtual).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-behaviour(gen_pubsub_nodetree).

-export([init/3,
	 terminate/2,
	 options/0,
	 set_node/1,
	 get_node/3,
	 get_node/2,
	 get_node/1,
	 get_nodes/2,
	 get_nodes/1,
	 get_subnodes/3,
	 get_subnodes_tree/3,
	 create_node/6,
	 delete_node/2
	]).

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
init(_Host, _ServerHost, _Opts) ->
    ok.
terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%     Option = mod_pubsub:nodetreeOption()
%% @doc <p>Returns the default pubsub node tree options.</p>
options() ->
    [{virtual_tree, true}].

%% @spec (NodeRecord) -> ok | {error, Reason}
%%     NodeRecord = mod_pubsub:pubsub_node()
%% @doc <p>No node record is stored on database. Just do nothing.</p>
set_node(_NodeRecord) ->
    ok.

%% @spec (Host, Node, From) -> pubsubNode()
%%     Host = mod_pubsub:host()
%%     Node = node()
%% @doc <p>Virtual node tree does not handle a node database. Any node is considered
%% as existing. Node record contains default values.</p>
get_node(Host, Node, _From) ->
    get_node(Host, Node).
get_node(Host, Node) ->
    %% TODO : to fix idx
    #pubsub_node{id = {Host, Node}, idx = {Host, Node}, owners = [{undefined, list_to_binary(Host), undefined}]}.
get_node({Host, _} = NodeId) ->
    #pubsub_node{id = NodeId, idx = NodeId, owners = [{undefined, list_to_binary(Host), undefined}]}.

%% @spec (Host, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host() | ljid()
%% @doc <p>Virtual node tree does not handle a node database. Any node is considered
%% as existing. Nodes list can not be determined.</p>
get_nodes(Host, _From) ->
    get_nodes(Host).
get_nodes(_Host) ->
    [].

%% @spec (Host, Node, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host()
%%     Node = node()
%%     From = ljid()
%% @doc <p>Virtual node tree does not handle parent/child. Child list is empty.</p>
get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).
get_subnodes(_Host, _Node) ->
    [].

%% @spec (Host, Index, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host()
%%     Node = node()
%% @doc <p>Virtual node tree does not handle parent/child. Child list is empty.</p>
get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).
get_subnodes_tree(_Host, _Node) ->
    [].

%% @spec (Host, Node, Type, Owner, Options, Parents) -> ok
%%     Host = mod_pubsub:host()
%%     Node = node()
%%     Type = nodeType()
%%     Owner = ljid()
%%     Options = list()
%% @doc <p>No node record is stored on database. Any valid node
%% is considered as already created.</p>
%% <p>default allowed nodes: /home/host/user/any/node/name</p>
create_node(Host, Node, _Type, Owner, _Options, _Parents) ->
    UserName = exmpp_jid:prep_node_as_list(Owner),
    UserHost = exmpp_jid:prep_domain_as_list(Owner),
    case Node of
	["home", UserHost, UserName | _] -> {error, {virtual, {Host, Node}}};
	_ -> {error, 'not-allowed'}
    end.

%% @spec (Host, Node) -> [mod_pubsub:node()]
%%     Host = mod_pubsub:host()
%%     Node = node()
%% @doc <p>Virtual node tree does not handle parent/child.
%% node deletion just affects the corresponding node.</p>
delete_node(Host, Node) ->
    [get_node(Host, Node)].
