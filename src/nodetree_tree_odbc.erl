%%%----------------------------------------------------------------------
%%% File    : nodetree_tree_odbc.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Standard node tree plugin with ODBC backend
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
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
-behaviour(gen_pubsub_nodetree).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([init/3, terminate/2, options/0, set_node/1,
    get_node/3, get_node/2, get_node/1, get_nodes/2,
    get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
    get_subnodes/3, get_subnodes_tree/3, create_node/6,
    delete_node/2]).

-export([raw_to_node/2]).

init(_Host, _ServerHost, _Opts) ->
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{odbc, true} | nodetree_tree:options()].

set_node(Record) when is_record(Record, pubsub_node) ->
    {Host, Node} = Record#pubsub_node.nodeid,
    Parent = case Record#pubsub_node.parents of
	[] -> <<>>;
	[First | _] -> First
    end,
    Type = Record#pubsub_node.type,
    H = node_flat_odbc:encode_host(Host),
    N = ejabberd_odbc:escape(Node),
    P = ejabberd_odbc:escape(Parent),
    Nidx = case nodeidx(Host, Node) of
	{result, OldNidx} ->
	    catch
	    ejabberd_odbc:sql_query_t([<<"delete from pubsub_node_option where "
			"nodeid='">>, OldNidx, <<"';">>]),
	    catch
	    ejabberd_odbc:sql_query_t([<<"update pubsub_node set host='">>,
		    H, <<"' node='">>, N,
		    <<"' parent='">>, P,
		    <<"' type='">>, Type,
		    <<"' where nodeid='">>,
		    OldNidx, <<"';">>]),
	    OldNidx;
	_ ->
	    catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_node(host, node, "
			"parent, type) values('">>,
		    H, <<"', '">>, N, <<"', '">>, P,
		    <<"', '">>, Type, <<"');">>]),
	    case nodeidx(Host, Node) of
		{result, NewNidx} -> NewNidx;
		_ -> none  % this should not happen
	    end
    end,
    case Nidx of
	none ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	_ ->
	    lists:foreach(fun ({Key, Value}) ->
			SKey = iolist_to_binary(atom_to_list(Key)),
			SValue = ejabberd_odbc:escape(
				list_to_binary(
				    lists:flatten(io_lib:fwrite("~p", [Value])))),
			catch
			ejabberd_odbc:sql_query_t([<<"insert into pubsub_node_option(nodeid, "
				    "name, val) values('">>,
				Nidx, <<"', '">>,
				SKey, <<"', '">>, SValue, <<"');">>])
		end,
		Record#pubsub_node.options),
	    {result, Nidx}
    end.

get_node(Host, Node, _From) ->
    get_node(Host, Node).

get_node(Host, Node) ->
    H = node_flat_odbc:encode_host(Host),
    N = ejabberd_odbc:escape(Node),
    case catch
	ejabberd_odbc:sql_query_t([<<"select node, parent, type, nodeid from "
		    "pubsub_node where host='">>,
		H, <<"' and node='">>, N, <<"';">>])
    of
	{selected,
		    [<<"node">>, <<"parent">>, <<"type">>, <<"nodeid">>], [RItem]} ->
	    raw_to_node(Host, RItem);
	{'EXIT', _Reason} ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_node(Nidx) ->
    case catch
	ejabberd_odbc:sql_query_t([<<"select host, node, parent, type from "
		    "pubsub_node where nodeid='">>,
		Nidx, <<"';">>])
    of
	{selected,
		    [<<"host">>, <<"node">>, <<"parent">>, <<"type">>], [[Host, Node, Parent, Type]]} ->
	    raw_to_node(Host, [Node, Parent, Type, Nidx]);
	{'EXIT', _Reason} ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_nodes(Host, _From) ->
    get_nodes(Host).

get_nodes(Host) ->
    H = node_flat_odbc:encode_host(Host),
    case catch
	ejabberd_odbc:sql_query_t([<<"select node, parent, type, nodeid from "
		    "pubsub_node where host='">>, H, <<"';">>])
    of
	{selected,
		    [<<"node">>, <<"parent">>, <<"type">>, <<"nodeid">>], RItems} ->
	    [raw_to_node(Host, Item) || Item <- RItems];
	_ ->
	    []
    end.

get_parentnodes(_Host, _Node, _From) ->
    [].

%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
get_parentnodes_tree(Host, Node, From) ->
    case get_node(Host, Node, From) of
	{error, _} -> [];
	Record -> [{0, [Record]}]
    end.

get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).

get_subnodes(Host, Node) ->
    H = node_flat_odbc:encode_host(Host),
    N = ejabberd_odbc:escape(Node),
    case catch
	ejabberd_odbc:sql_query_t([<<"select node, parent, type, nodeid from "
		    "pubsub_node where host='">>,
		H, <<"' and parent='">>, N, <<"';">>])
    of
	{selected,
		    [<<"node">>, <<"parent">>, <<"type">>, <<"nodeid">>], RItems} ->
	    [raw_to_node(Host, Item) || Item <- RItems];
	_ ->
	    []
    end.

get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).

get_subnodes_tree(Host, Node) ->
    H = node_flat_odbc:encode_host(Host),
    N = ejabberd_odbc:escape(Node),
    case catch
	ejabberd_odbc:sql_query_t([<<"select node, parent, type, nodeid from "
		    "pubsub_node where host='">>,
		H, <<"' and node like '">>, N, <<"%';">>])
    of
	{selected,
		    [<<"node">>, <<"parent">>, <<"type">>, <<"nodeid">>], RItems} ->
	    [raw_to_node(Host, Item) || Item <- RItems];
	_ ->
	    []
    end.

create_node(Host, Node, Type, Owner, Options, Parents) ->
    BJID = jid:tolower(jid:remove_resource(Owner)),
    case nodeidx(Host, Node) of
	{error, ?ERR_ITEM_NOT_FOUND} ->
	    ParentExists = case Host of
		{_U, _S, _R} ->
		    %% This is special case for PEP handling
		    %% PEP does not uses hierarchy
		    true;
		_ ->
		    case Parents of
			[] ->
			    true;
			[Parent | _] ->
			    case nodeidx(Host, Parent) of
				{result, PNode} ->
				    case nodeowners(PNode) of
					[{<<>>, Host, <<>>}] -> true;
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
		    case set_node(#pubsub_node{nodeid = {Host, Node},
				parents = Parents, type = Type,
				options = Options})
		    of
			{result, Nidx} -> {ok, Nidx};
			Other -> Other
		    end;
		false ->
		    {error, ?ERR_FORBIDDEN}
	    end;
	{result, _} ->
	    {error, ?ERR_CONFLICT};
	Error ->
	    Error
    end.

delete_node(Host, Node) ->
    H = node_flat_odbc:encode_host(Host),
    N = ejabberd_odbc:escape(Node),
    Removed = get_subnodes_tree(Host, Node),
    catch ejabberd_odbc:sql_query_t([<<"delete from pubsub_node where host='">>,
	    H, <<"' and node like '">>, N, <<"%';">>]),
    Removed.

%% helpers
raw_to_node(Host, [Node, Parent, Type, Nidx]) ->
    Options = case catch
	ejabberd_odbc:sql_query_t([<<"select name,val from pubsub_node_option "
		    "where nodeid='">>, Nidx, <<"';">>])
    of
	{selected, [<<"name">>, <<"val">>], ROptions} ->
	    DbOpts = lists:map(fun ([Key, Value]) ->
			    RKey = jlib:binary_to_atom(Key),
			    Tokens = element(2, erl_scan:string(binary_to_list(<<Value/binary, ".">>))),
			    RValue = element(2, erl_parse:parse_term(Tokens)),
			    {RKey, RValue}
		    end,
		    ROptions),
	    Module = jlib:binary_to_atom(<<"node_", Type/binary, "_odbc">>),
	    StdOpts = Module:options(),
	    lists:foldl(fun ({Key, Value}, Acc) ->
			lists:keyreplace(Key, 1, Acc, {Key, Value})
		end,
		StdOpts, DbOpts);
	_ ->
	    []
    end,
    Parents = case Parent of
	<<>> -> [];
	_ -> [Parent]
    end,
    #pubsub_node{nodeid = {Host, Node},
	parents = Parents,
	id = Nidx, type = Type, options = Options}.

nodeidx(Host, Node) ->
    H = node_flat_odbc:encode_host(Host),
    N = ejabberd_odbc:escape(Node),
    case catch
	ejabberd_odbc:sql_query_t([<<"select nodeid from pubsub_node where "
		    "host='">>,
		H, <<"' and node='">>, N, <<"';">>])
    of
	{selected, [<<"nodeid">>], [[Nidx]]} ->
	    {result, Nidx};
	{'EXIT', _Reason} ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.

nodeowners(Nidx) ->
    {result, Res} = node_flat_odbc:get_node_affiliations(Nidx),
    [LJID || {LJID, Aff} <- Res, Aff =:= owner].
