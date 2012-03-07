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

-include_lib("stdlib/include/qlc.hrl").

-include("pubsub.hrl").

-define(PUBSUB, mod_pubsub_odbc).
-define(PLUGIN_PREFIX, "node_").

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
	 get_parentnodes/3,
	 get_parentnodes_tree/3,
	 get_subnodes/3,
	 get_subnodes_tree/3,
	 create_node/6,
	 delete_node/2
	]).

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
init(_Host, _ServerHost, _Opts) ->
    ok.
terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%     Option = nodetreeOption()
%% @doc Returns the default pubsub node tree options.
options() ->
    [{virtual_tree, false},
     {odbc, true}].


%% @spec (Host, Node, From) -> node() | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = node()
get_node(Host, Node, _From) ->
    get_node(Host, Node).
get_node(Host, Node) ->
    H = ?PUBSUB:escape(Host),
    N = ?PUBSUB:escape(?PUBSUB:node_to_string(Node)),
    case catch ejabberd_odbc:sql_query_t(
		 ["select node, parent, type, nodeid "
		  "from pubsub_node "
		  "where host='", H, "' and node='", N, "';"])
	of
	{selected, ["node", "parent", "type", "nodeid"], [RItem]} ->
	    raw_to_node(Host, RItem);
	{'EXIT', _Reason} ->
	    {error, 'internal_server_error'};
	_ ->
	    {error, 'item_not_found'}
    end.
get_node(Nidx) ->
    case catch ejabberd_odbc:sql_query_t(
		 ["select host, node, parent, type "
		  "from pubsub_node "
		  "where nodeid='", Nidx, "';"])
	of
	{selected, ["host", "node", "parent", "type"], [{Host, Node, Parent, Type}]} ->
	    raw_to_node(Host, {Node, Parent, Type, Nidx});
	{'EXIT', _Reason} ->
	    {error, 'internal_server_error'};
	_ ->
	    {error, 'item_not_found'}
    end.

%% @spec (Host, From) -> [node()] | {error, Reason}
%%     Host = mod_pubsub:host() | ljid()
get_nodes(Host, _From) ->
    get_nodes(Host).
get_nodes(Host) ->
    H = ?PUBSUB:escape(Host),
    case catch ejabberd_odbc:sql_query_t(
		 ["select node, parent, type, nodeid "
		  "from pubsub_node "
		  "where host='", H, "';"])
	of
	{selected, ["node", "parent", "type", "nodeid"], RItems} ->
	    lists:map(fun(Item) -> raw_to_node(Host, Item) end, RItems);
	_ ->
	    []
    end.

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | ljid()
%%     Node   = node()
%%     From   = ljid()
%%     Depth  = int()
%%     Record = mod_pubsub:pubsub_node()
%% @doc <p>Default node tree does not handle parents, return empty list.</p>
get_parentnodes(_Host, _Node, _From) ->
    [].

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | ljid()
%%     Node   = node()
%%     From   = ljid()
%%     Depth  = int()
%%     Record = mod_pubsub:pubsub_node()
%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
get_parentnodes_tree(Host, Node, From) ->
    case get_node(Host, Node, From) of
	N when is_record(N, pubsub_node) -> [{0, [N]}];
	_Error -> []
    end.

get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).
get_subnodes(Host, Node) ->
    H = ?PUBSUB:escape(Host),
    N = ?PUBSUB:escape(?PUBSUB:node_to_string(Node)),
    case catch ejabberd_odbc:sql_query_t(
		 ["select node, parent, type, nodeid "
		  "from pubsub_node "
		  "where host='", H, "' and parent='", N, "';"])
	of
	{selected, ["node", "parent", "type", "nodeid"], RItems} ->
	    lists:map(fun(Item) -> raw_to_node(Host, Item) end, RItems);
	_ ->
	    []
    end.

%% @spec (Host, Index, From) -> [nodeidx()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = node()
%%     From = ljid()
get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).
get_subnodes_tree(Host, Node) ->
    H = ?PUBSUB:escape(Host),
    N = ?PUBSUB:escape(?PUBSUB:node_to_string(Node)),
    case catch ejabberd_odbc:sql_query_t(
		["select node, parent, type, nodeid "
		 "from pubsub_node "
		 "where host='", H, "' and node like '", N, "%';"])
	of
	{selected, ["node", "parent", "type", "nodeid"], RItems} ->
	    lists:map(fun(Item) -> raw_to_node(Host, Item) end, RItems);
	_ ->
	    []
    end.

%% @spec (Host, Node, Type, Owner, Options, Parents) -> ok | {error, Reason}
%%     Host = mod_pubsub:host() | ljid()
%%     Node = node()
%%     NodeType = nodeType()
%%     Owner = ljid()
%%     Options = list()
create_node(Host, Node, Type, Owner, Options, Parents) ->
    BJID = jlib:short_prepd_bare_jid(Owner),
    case nodeid(Host, Node) of
	{error, 'item_not_found'} ->
	    ParentExists = case Host of
		{_, _, _} ->
		    %% This is special case for PEP handling
		    %% PEP does not uses hierarchy
		    true;
		_ ->
		    case Parents of
			[] ->
			    true;
			[Parent | _] ->
			    case nodeid(Host, Parent) of
				{result, Pidx} -> 
				    BHost = list_to_binary(Host),
				    case nodeowners(Pidx) of
					[{undefined, BHost, undefined}] -> true;
					Owners -> lists:member(BJID, Owners)
				    end;
				_ ->
				    false
			    end;
			_ ->
			    false
		    end
	    end,
	    case ParentExists of
		true -> 
		    case set_node(#pubsub_node{id={Host, Node},
                                               parents=Parents,
                                               type=Type,
                                               options=Options}) of
			{result, Nidx} -> {ok, Nidx};
			Other -> Other
		    end;
		false -> 
		    %% Requesting entity is prohibited from creating nodes
		    {error, 'forbidden'}
	    end;
	{result, _} -> 
	    %% Node already exists
	    {error, 'conflict'};
	Error -> 
	    Error
    end.


%% @spec (Host, Node) -> [node()]
%%     Host = mod_pubsub:host() | ljid()
%%     Node = node()
delete_node(Host, Node) ->
    H = ?PUBSUB:escape(Host),
    N = ?PUBSUB:escape(?PUBSUB:node_to_string(Node)),
    Removed = get_subnodes_tree(Host, Node),
    catch ejabberd_odbc:sql_query_t(
	    ["delete from pubsub_node "
	     "where host='", H, "' and node like '", N, "%';"]),
    Removed.


%% helpers

raw_to_node(Host, {Node, Parent, Type, Nidx}) ->
    Options = case catch ejabberd_odbc:sql_query_t(
			   ["select name,val "
			    "from pubsub_node_option "
			    "where nodeid='", Nidx, "';"])
		  of
		  {selected, ["name", "val"], ROptions} ->
		    	DbOpts = lists:map(fun({Key, Value}) -> 
					RKey = list_to_atom(Key),
					Tokens = element(2, erl_scan:string(Value++".")),
					RValue = element(2, erl_parse:parse_term(Tokens)),
					{RKey, RValue}
				end, ROptions),
				Module = list_to_atom(?PLUGIN_PREFIX++Type),
				StdOpts = Module:options(),
				lists:foldl(fun({Key, Value}, Acc)->
					lists:keyreplace(Key, 1, Acc, {Key, Value})
				end, StdOpts, DbOpts);
		  _ ->
		      []
	      end,
    #pubsub_node{
		id = {Host, ?PUBSUB:string_to_node(Node)},
		parents = [?PUBSUB:string_to_node(Parent)],
		idx = Nidx,
		type = Type, 
		options = Options}.

%% @spec (NodeRecord) -> ok | {error, Reason}
%%	 Record = mod_pubsub:pubsub_node()
set_node(Record) ->
    {Host, Node} = Record#pubsub_node.id,
    Parent = case Record#pubsub_node.parents of
                [] -> <<>>;
                [First | _] -> First
    end,
    Type = Record#pubsub_node.type,
    H = ?PUBSUB:escape(Host),
    N = ?PUBSUB:escape(?PUBSUB:node_to_string(Node)),
    P = ?PUBSUB:escape(?PUBSUB:node_to_string(Parent)),
    Nidx = case nodeid(Host, Node) of
		 {result, OldNidx} ->
		     catch ejabberd_odbc:sql_query_t(
			     ["delete from pubsub_node_option "
			      "where nodeid='", OldNidx, "';"]),
		     catch ejabberd_odbc:sql_query_t(
			     ["update pubsub_node "
			      "set host='", H, "' "
			      "node='", N, "' "
			      "parent='", P, "' "
			      "type='", Type, "' "
			      "where nodeid='", OldNidx, "';"]),
		     OldNidx;
		 _ ->
		     catch ejabberd_odbc:sql_query_t(
			     ["insert into pubsub_node(host, node, parent, type) "
			      "values('", H, "', '", N, "', '", P, "', '", Type, "');"]),
		     case nodeid(Host, Node) of
			 {result, NewNidx} -> NewNidx;
			 _ -> none  % this should not happe
		     end
	     end,
    case Nidx of
	none ->
	    {error, 'internal_server_error'};
	_ ->
	    lists:foreach(fun({Key, Value}) ->
				  SKey = atom_to_list(Key),
				  SValue = ?PUBSUB:escape(lists:flatten(io_lib:fwrite("~p",[Value]))),
				  catch ejabberd_odbc:sql_query_t(
					  ["insert into pubsub_node_option(nodeid, name, val) "
					   "values('", Nidx, "', '", SKey, "', '", SValue, "');"]) 
			  end, Record#pubsub_node.options),
	    {result, Nidx}
    end.

nodeid(Host, Node) ->
    H = ?PUBSUB:escape(Host),
    N = ?PUBSUB:escape(?PUBSUB:node_to_string(Node)),
    case catch ejabberd_odbc:sql_query_t(
		 ["select nodeid "
		  "from pubsub_node "
		  "where host='", H, "' and node='", N, "';"])
	of
	{selected, ["nodeid"], [{Nidx}]} ->
	    {result, Nidx};
	{'EXIT', _Reason} ->
	    {error, 'internal_server_error'};
	_ ->
	    {error, 'item_not_found'}
    end.

nodeowners(Nidx) ->
    {result, Res} = node_flat_odbc:get_node_affiliations(Nidx),
    lists:foldl(fun({LJID, owner}, Acc) -> [LJID|Acc];
		   (_, Acc) -> Acc
		end, [], Res).
