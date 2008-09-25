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
%%% Portions created by ProcessOne are Copyright 2006-2008, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2008, ProcessOne.
%%%
%%%
%%% @copyright 2006-2008 ProcessOne
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
     {get_items, 7},
     {get_items, 3},
     {get_item, 8},
     {get_item, 3},
     {set_item, 1},
     {get_item_name, 3}
    ];
behaviour_info(_Other) ->
    undefined.
