%%% ====================================================================
%%% This software is copyright 2007, Process-one.
%%%
%%% @copyright 2006 Process-one
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @private
%%% @doc <p>The module <strong>{@module}</strong> defines the PubSub node
%%% plugin behaviour. This behaviour is used to check that a PubSub plugin
%%% respects the current ejabberd PubSub plugin API.</p>

-module(gen_pubsub_node).

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
     {features, 0},
     {create_node_permission, 6},
     {create_node, 3},
     {delete_node, 2},
     {purge_node, 3},
     {subscribe_node, 8},
     {unsubscribe_node, 5},
     {publish_item, 7},
     {delete_item, 4},
     {remove_extra_items, 4},
     {get_node_affiliations, 2},
     {get_entity_affiliations, 2},
     {get_affiliation, 3},
     {set_affiliation, 4},
     {get_node_subscriptions, 2},
     {get_entity_subscriptions, 2},
     {get_subscription, 3},
     {set_subscription, 4},
     {get_states, 2},
     {get_state, 3},
     {set_state, 1},
     {get_items, 2},
     {get_item, 3},
     {set_item, 1}
    ];
behaviour_info(_Other) ->
    undefined.
