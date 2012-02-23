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
     {create_node, 2},
     {delete_node, 1},
     {purge_node, 2},
     {subscribe_node, 8},
     {unsubscribe_node, 4},
     {publish_item, 6},
     {delete_item, 4},
     {remove_extra_items, 3},
     {get_node_affiliations, 1},
     {get_entity_affiliations, 2},
     {get_affiliation, 2},
     {set_affiliation, 3},
     {get_node_subscriptions, 1},
     {get_entity_subscriptions, 2},
     {get_subscriptions, 2},
     {set_subscriptions, 4},
     {get_pending_nodes, 2},
     {get_states, 1},
     {get_state, 2},
     {set_state, 1},
     {get_items, 6},
     {get_items, 2},
     {get_item, 7},
     {get_item, 2},
     {set_item, 1},
     {get_item_name, 3},
     {node_to_path, 1},
     {path_to_node, 1}
    ];
behaviour_info(_Other) ->
    undefined.
