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

%%% @doc The module <strong>{@module}</strong> is the default PubSub node tree plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node tree
%%% types.</p>
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% useable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(nodetree_tree_odbc).

-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-include("jlib.hrl").

-define(PUBSUB, mod_pubsub_odbc).

-define(PLUGIN_PREFIX, <<"node_">>).
-define(ODBC_SUFFIX, <<"_odbc">>).

-behaviour(gen_pubsub_nodetree).

-export([init/3, terminate/2, options/0, set_node/1,
	 get_node/3, get_node/2, get_node/1, get_nodes/2,
	 get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
	 get_subnodes/3, get_subnodes_tree/3, create_node/6,
	 delete_node/2]).

-export([raw_to_node/2]).

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
%% @doc Returns the default pubsub node tree options.
%% @spec (Host, Node, From) -> pubsubNode() | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
init(_Host, _ServerHost, _Opts) -> ok.

terminate(_Host, _ServerHost) -> ok.

options() -> [{virtual_tree, false}, {odbc, true}].

get_node(Host, Node, _From) -> get_node(Host, Node).

-spec(get_node/2 ::
(
  Host :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> mod_pubsub:pubsubNode()
     | {error, _}
).
get_node(Host, Node) ->
    H = (?PUBSUB):escape(Host),
    N = (?PUBSUB):escape(Node),
    case catch
	   ejabberd_odbc:sql_query_t([<<"select node, parent, type, nodeid from "
					"pubsub_node where host='">>,
				      H, <<"' and node='">>, N, <<"';">>])
	of
      {selected,
       [<<"node">>, <<"parent">>, <<"type">>, <<"nodeid">>],
       [RItem]} ->
	  raw_to_node(Host, RItem);
      {'EXIT', _Reason} ->
	  {error, ?ERR_INTERNAL_SERVER_ERROR};
      _ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

-spec(get_node/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> mod_pubsub:pubsubNode()
     | {error, _}
).
get_node(NodeIdx) ->
    case catch
	   ejabberd_odbc:sql_query_t([<<"select host, node, parent, type from "
					"pubsub_node where nodeid='">>,
				      NodeIdx, <<"';">>])
	of
      {selected,
       [<<"host">>, <<"node">>, <<"parent">>, <<"type">>],
       [[Host, Node, Parent, Type]]} ->
	  raw_to_node(Host, [Node, Parent, Type, NodeIdx]);
      {'EXIT', _Reason} ->
	  {error, ?ERR_INTERNAL_SERVER_ERROR};
      _ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

%% @spec (Host, From) -> [pubsubNode()] | {error, Reason}
%%	 Host = mod_pubsub:host() | mod_pubsub:jid()
get_nodes(Host, _From) -> get_nodes(Host).

-spec(get_nodes/1 ::
(
  Host::mod_pubsub:host())
    -> [mod_pubsub:pubsubNode()]
).
get_nodes(Host) ->
    H = (?PUBSUB):escape(Host),
    case catch
	   ejabberd_odbc:sql_query_t([<<"select node, parent, type, nodeid from "
					"pubsub_node where host='">>,
				      H, <<"';">>])
	of
      {selected,
       [<<"node">>, <<"parent">>, <<"type">>, <<"nodeid">>],
       RItems} ->
	  lists:map(fun (Item) -> raw_to_node(Host, Item) end,
		    RItems);
      _ -> []
    end.

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | mod_pubsub:jid()
%%     Node   = mod_pubsub:pubsubNode()
%%     From   = mod_pubsub:jid()
%%     Depth  = integer()
%%     Record = pubsubNode()
%% @doc <p>Default node tree does not handle parents, return empty list.</p>
%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | mod_pubsub:jid()
%%     Node   = mod_pubsub:pubsubNode()
%%     From   = mod_pubsub:jid()
%%     Depth  = integer()
%%     Record = pubsubNode()
%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
get_parentnodes(_Host, _Node, _From) -> [].

-spec(get_parentnodes_tree/3 ::
(
  Host   :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId(),
  From   :: jid())
    -> [{0, [mod_pubsub:pubsubNode(),...]}]
).

get_parentnodes_tree(Host, Node, From) ->
    case get_node(Host, Node, From) of
      N when is_record(N, pubsub_node) -> [{0, [N]}];
      _Error -> []
    end.

get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).

%% @spec (Host, Index) -> [pubsubNode()] | {error, Reason}
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
-spec(get_subnodes/2 ::
(
  Host   :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode()]
).
get_subnodes(Host, Node) ->
    H = (?PUBSUB):escape(Host),
    N = (?PUBSUB):escape(Node),
    case catch
	   ejabberd_odbc:sql_query_t([<<"select node, parent, type, nodeid from "
					"pubsub_node where host='">>,
				      H, <<"' and parent='">>, N, <<"';">>])
	of
      {selected,
       [<<"node">>, <<"parent">>, <<"type">>, <<"nodeid">>],
       RItems} ->
	  lists:map(fun (Item) -> raw_to_node(Host, Item) end,
		    RItems);
      _ -> []
    end.

get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).

%% @spec (Host, Index) -> [pubsubNode()] | {error, Reason}
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
-spec(get_subnodes_tree/2 ::
(
  Host :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode()]
).

get_subnodes_tree(Host, Node) ->
    H = (?PUBSUB):escape(Host),
    N = (?PUBSUB):escape(Node),
    case catch
	   ejabberd_odbc:sql_query_t([<<"select node, parent, type, nodeid from "
					"pubsub_node where host='">>,
				      H, <<"' and node like '">>, N, <<"%';">>])
	of
      {selected,
       [<<"node">>, <<"parent">>, <<"type">>, <<"nodeid">>],
       RItems} ->
	  lists:map(fun (Item) -> raw_to_node(Host, Item) end,
		    RItems);
      _ -> []
    end.

%% @spec (Host, Node, Type, Owner, Options, Parents) -> ok | {error, Reason}
%%	 Host = mod_pubsub:host() | mod_pubsub:jid()
%%	 Node = mod_pubsub:pubsubNode()
%%	 NodeType = mod_pubsub:nodeType()
%%	 Owner = mod_pubsub:jid()
%%	 Options = list()
%%	 Parents = list()

-spec(create_node/6 ::
(
  Host    :: mod_pubsub:host(),
  NodeId  :: mod_pubsub:nodeId(),
  Type    :: binary(), 
  Owner   :: jid(),
  Options :: mod_pubsub:nodeOptions(),
  Parents :: [mod_pubsub:nodeId()])
    -> {ok, NodeIdx::mod_pubsub:nodeIdx()}
    %%%
     | {error, _}
).

create_node(Host, Node, Type, Owner, Options, Parents) ->
    BJID = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    case nodeid(Host, Node) of
      {error, ?ERR_ITEM_NOT_FOUND} ->
	  ParentExists = case Host of
			   {_U, _S, _R} ->
			       %% This is special case for PEP handling
			       %% PEP does not uses hierarchy
			       true;
			   _ ->
			       case Parents of
				 [] -> true;
				 [Parent | _] ->
				     case nodeid(Host, Parent) of
				       {result, PNodeId} ->
					   case nodeowners(PNodeId) of
					     [{<<>>, Host, <<>>}] -> true;
					     Owners ->
						 lists:member(BJID, Owners)
					   end;
				       _ -> false
				     end;
				 _ -> false
			       end
			 end,
	  case ParentExists of
	    true ->
		case set_node(#pubsub_node{nodeid = {Host, Node},
					   parents = Parents, type = Type,
					   options = Options})
		    of
		  {result, NodeId} -> {ok, NodeId};
		  Other -> Other
		end;
	    false -> {error, ?ERR_FORBIDDEN}
	  end;
      {result, _} -> {error, ?ERR_CONFLICT};
      Error -> Error
    end.

%% @spec (Host, Node) -> [mod_pubsub:node()]
%%	 Host = mod_pubsub:host() | mod_pubsub:jid()
%%	 Node = mod_pubsub:pubsubNode()
-spec(delete_node/2 ::
(
  Host :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode()]
).

delete_node(Host, Node) ->
    H = (?PUBSUB):escape(Host),
    N = (?PUBSUB):escape(Node),
    Removed = get_subnodes_tree(Host, Node),
    catch
      ejabberd_odbc:sql_query_t([<<"delete from pubsub_node where host='">>,
				 H, <<"' and node like '">>, N, <<"%';">>]),
    Removed.

%% helpers
-spec(raw_to_node/2 ::
(
  Host :: mod_pubsub:host(),
  _    :: {NodeId::mod_pubsub:nodeId(),
           Parent::mod_pubsub:nodeId(),
           Type::binary(),
           NodeIdx::mod_pubsub:nodeIdx()})
    -> mod_pubsub:pubsubNode()
).
raw_to_node(Host, [Node, Parent, Type, NodeIdx]) ->
    Options = case catch
		     ejabberd_odbc:sql_query_t([<<"select name,val from pubsub_node_option "
						  "where nodeid='">>,
						NodeIdx, <<"';">>])
		  of
		{selected, [<<"name">>, <<"val">>], ROptions} ->
		    DbOpts = lists:map(fun ([Key, Value]) ->
					       RKey =
						   jlib:binary_to_atom(Key),
					       Tokens = element(2,
								erl_scan:string(
								    binary_to_list(<<Value/binary, ".">>))),
					       RValue = element(2,
								erl_parse:parse_term(Tokens)),
					       {RKey, RValue}
				       end,
				       ROptions),
		    Module =
			jlib:binary_to_atom(<<(?PLUGIN_PREFIX)/binary,
						Type/binary,
						(?ODBC_SUFFIX)/binary>>),
		    StdOpts = Module:options(),
		    lists:foldl(fun ({Key, Value}, Acc) ->
					lists:keyreplace(Key, 1, Acc,
							 {Key, Value})
				end,
				StdOpts, DbOpts);
		_ -> []
	      end,
    Parents = case Parent of
		  <<>> -> [];
		  _ -> [Parent]
	      end,
    #pubsub_node{nodeid =
		     {Host, Node},
		 parents = Parents,
		 id = NodeIdx, type = Type, options = Options}.

% @spec (NodeRecord) -> ok | {error, Reason}
%%	 Record = mod_pubsub:pubsub_node()
-spec(set_node/1 ::
(
  Record::mod_pubsub:pubsubNode())
    -> {result, NodeIdx::mod_pubsub:nodeIdx()}
    %%%
     | {error, _}
).
set_node(Record) ->
    {Host, Node} = Record#pubsub_node.nodeid,
    Parent = case Record#pubsub_node.parents of
	       [] -> <<>>;
	       [First | _] -> First
	     end,
    Type = Record#pubsub_node.type,
    H = (?PUBSUB):escape(Host),
    N = (?PUBSUB):escape(Node),
    P = (?PUBSUB):escape(Parent),
    NodeIdx = case nodeid(Host, Node) of
	       {result, OldNodeIdx} ->
		   catch
		     ejabberd_odbc:sql_query_t([<<"delete from pubsub_node_option where "
						  "nodeid='">>,
						OldNodeIdx, <<"';">>]),
		   catch
		     ejabberd_odbc:sql_query_t([<<"update pubsub_node set host='">>,
						H, <<"' node='">>, N,
						<<"' parent='">>, P,
						<<"' type='">>, Type,
						<<"' where nodeid='">>,
						OldNodeIdx, <<"';">>]),
		   OldNodeIdx;
	       _ ->
		   catch
		     ejabberd_odbc:sql_query_t([<<"insert into pubsub_node(host, node, "
						  "parent, type) values('">>,
						H, <<"', '">>, N, <<"', '">>, P,
						<<"', '">>, Type, <<"');">>]),
		   case nodeid(Host, Node) of
		     {result, NewNodeIdx} -> NewNodeIdx;
		     _ -> none  % this should not happen
		   end
	     end,
    case NodeIdx of
      none -> {error, ?ERR_INTERNAL_SERVER_ERROR};
      _ ->
	  lists:foreach(fun ({Key, Value}) ->
				SKey = iolist_to_binary(atom_to_list(Key)),
				SValue =
				    (?PUBSUB):escape(list_to_binary(lists:flatten(io_lib:fwrite("~p",
						    [Value])))),
				catch
				  ejabberd_odbc:sql_query_t([<<"insert into pubsub_node_option(nodeid, "
							       "name, val) values('">>,
							     NodeIdx, <<"', '">>,
							     SKey, <<"', '">>,
							     SValue, <<"');">>])
			end,
			Record#pubsub_node.options),
	  {result, NodeIdx}
    end.

-spec(nodeid/2 ::
(
  Host   :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> {result, NodeIdx::mod_pubsub:nodeIdx()}
    %%%
     | {error, _}
).

nodeid(Host, NodeId) ->
    H = (?PUBSUB):escape(Host),
    N = (?PUBSUB):escape(NodeId),
    case catch
	   ejabberd_odbc:sql_query_t([<<"select nodeid from pubsub_node where "
					"host='">>,
				      H, <<"' and node='">>, N, <<"';">>])
	of
      {selected, [<<"nodeid">>], [[NodeIdx]]} ->
	  {result, NodeIdx};
      {'EXIT', _Reason} ->
	  {error, ?ERR_INTERNAL_SERVER_ERROR};
      _ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

-spec(nodeowners/1 ::
(
    NodeIdx::mod_pubsub:nodeIdx())
    -> Node_Owners::[ljid()]
).

nodeowners(NodeIdx) ->
    {result, Res} = node_hometree_odbc:get_node_affiliations(NodeIdx),
    lists:foldl(fun ({LJID, owner}, Acc) -> [LJID | Acc];
		    (_, Acc) -> Acc
		end,
		[], Res).
