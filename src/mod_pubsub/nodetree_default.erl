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
%%% Portions created by ProcessOne are Copyright 2006-2009, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2009, ProcessOne.
%%%
%%%
%%% @copyright 2006-2009 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the default PubSub node tree plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node tree
%%% types.</p>
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% useable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(nodetree_default).
-author('christophe.romain@process-one.net').

-include_lib("exmpp/include/exmpp.hrl").

-include("pubsub.hrl").

-behaviour(gen_pubsub_nodetree).

-export([init/3,
	 terminate/2,
	 options/0,
	 set_node/1,
	 get_node/3,
	 get_node/2,
	 get_nodes/2,
	 get_nodes/1,
	 get_subnodes/3,
	 get_subnodes_tree/3,
	 create_node/5,
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
    mnesia:create_table(pubsub_node,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, pubsub_node)}]),
    NodesFields = record_info(fields, pubsub_node),
    case mnesia:table_info(pubsub_node, attributes) of
	NodesFields -> ok;
	_ ->
	    ok
	    %% mnesia:transform_table(pubsub_state, ignore, StatesFields)
    end,
    ok.
terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%     Option = mod_pubsub:nodetreeOption()
%% @doc Returns the default pubsub node tree options.
options() ->
    [{virtual_tree, false}].

%% @spec (NodeRecord) -> ok | {error, Reason}
%%     Record = mod_pubsub:pubsub_node()
set_node(Record) when is_record(Record, pubsub_node) ->
    mnesia:write(Record);
set_node(_) ->
    {error, 'internal-server-error'}.

%% @spec (Host, Node) -> pubsubNode() | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
get_node(Host, Node, _From) ->
    get_node(Host, Node).
get_node(Host, Node) ->
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
	[Record] when is_record(Record, pubsub_node) -> Record;
	[] -> {error, 'item-not-found'};
	Error -> Error
    end.

%% @spec (Host) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
get_nodes(Host, _From) ->
    get_nodes(Host).
get_nodes(Host) ->
    mnesia:match_object(#pubsub_node{nodeid = {Host, '_'}, _ = '_'}).

%% @spec (Host, Node, From) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).
get_subnodes(Host, Node) ->
    mnesia:match_object(#pubsub_node{nodeid = {Host, '_'}, parent = Node, _ = '_'}).

%% @spec (Host, Index) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).
get_subnodes_tree(Host, Node) ->
    mnesia:foldl(fun(#pubsub_node{nodeid = {H, N} = R}, Acc) ->
			 case lists:prefix(Node, N) and (H == Host) of
			     true -> [R | Acc];
			     _ -> Acc
			 end
		 end, [], pubsub_node).

%% @spec (Host, Node, Type, Owner, Options) -> ok | {error, Reason}
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
%%     Node = mod_pubsub:pubsubNode()
%%     NodeType = mod_pubsub:nodeType()
%%     Owner = mod_pubsub:jid()
%%     Options = list()
create_node(Host, Node, Type, Owner, Options) ->
    BJID = jlib:short_prepd_bare_jid(Owner),
    case mnesia:read({pubsub_node, {Host, Node}}) of
	[] ->
	    {ParentNode, ParentExists} =
		case Host of
		    {_U, _S, _R} ->
			%% This is special case for PEP handling
			%% PEP does not uses hierarchy
			{[], true};
		    _ ->
			Parent = lists:sublist(Node, length(Node) - 1),
			case Parent of
			[] -> 
			    {[], true};
			_ ->
			    case mnesia:read({pubsub_node, {Host, Parent}}) of
				[] -> {Parent, false};
				_ -> {Parent, true}
			    end
			end
		end,
	    case ParentExists of
		true ->
		    NodeId = pubsub_index:new(node),
		    mnesia:write(#pubsub_node{nodeid = {Host, Node},
					      id = NodeId,
					      parent = ParentNode,
					      type = Type,
					      owners = [BJID],
					      options = Options}),
		    {ok, NodeId};
		false ->
		    %% Requesting entity is prohibited from creating nodes
		    {error, 'forbidden'}
	    end;
	_ ->
	    %% NodeID already exists
	    {error, 'conflict'}
    end.

%% @spec (Host, Node) -> [mod_pubsub:node()]
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
%%     Node = mod_pubsub:pubsubNode()
delete_node(Host, Node) ->
    Removed = get_subnodes_tree(Host, Node),
    lists:foreach(fun(#pubsub_node{nodeid = {_, N}, id = I}) ->
	    pubsub_index:free(node, I),
	    mnesia:delete({pubsub_node, {Host, N}})
	end, Removed),
    Removed.
