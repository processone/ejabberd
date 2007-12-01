%%% ====================================================================
%%% This software is copyright 2006, Process-one.
%%%
%%% $Id: gen_pubsub_nodetree.erl 100 2007-11-15 13:04:44Z mremond $
%%%
%%% @copyright 2006 Process-one
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

-export([behaviour_info/1]).

%% @spec (Query::atom()) -> Callbacks | atom()
%%	Callbacks = [{Function,Arity}]
%%	Function = atom()
%%	Arity = integer()
%% @doc Behaviour definition
behaviour_info(callbacks) ->
	[{init, 3},
	 {terminate, 2},
	 {options, 0},
	 {set_node, 1},
	 {get_node, 2},
	 {get_nodes, 1},
	 {get_subnodes, 2},
	 {get_subnodes_tree, 2},
	 {create_node, 5},
	 {delete_node, 2}
	];
behaviour_info(_Other) ->
	undefined.
