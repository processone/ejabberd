%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 22 Oct 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(roster_tests).

%% API
-compile(export_all).
-import(suite, [send_recv/2, recv_iq/1, send/2, disconnect/1, del_roster/1,
		del_roster/2, make_iq_result/1, wait_for_slave/1,
		wait_for_master/1, recv_presence/1, self_presence/2,
		put_event/2, get_event/1, match_failure/2, get_roster/1]).
-include("suite.hrl").
-include("mod_roster.hrl").

-record(state, {subscription = none :: none | from | to | both,
		peer_available = false,
		pending_in = false :: boolean(),
		pending_out = false :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================
init(_TestCase, Config) ->
    Config.

stop(_TestCase, Config) ->
    Config.

%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {roster_single, [sequence],
     [single_test(feature_enabled),
      single_test(iq_set_many_items),
      single_test(iq_set_duplicated_groups),
      single_test(iq_get_item),
      single_test(iq_unexpected_element),
      single_test(iq_set_ask),
      single_test(set_item),
      single_test(version)]}.

feature_enabled(Config) ->
    ct:comment("Checking if roster versioning stream feature is set"),
    true = ?config(rosterver, Config),
    disconnect(Config).

set_item(Config) ->
    JID = jid:decode(<<"nurse@example.com">>),
    Item = #roster_item{jid = JID},
    {V1, Item} = set_items(Config, [Item]),
    {V1, [Item]} = get_items(Config),
    ItemWithGroups = Item#roster_item{groups = [<<"G1">>, <<"G2">>]},
    {V2, ItemWithGroups} = set_items(Config, [ItemWithGroups]),
    {V2, [ItemWithGroups]} = get_items(Config),
    {V3, Item} = set_items(Config, [Item]),
    {V3, [Item]} = get_items(Config),
    ItemWithName = Item#roster_item{name = <<"some name">>},
    {V4, ItemWithName} = set_items(Config, [ItemWithName]),
    {V4, [ItemWithName]} = get_items(Config),
    ItemRemoved = Item#roster_item{subscription = remove},
    {V5, ItemRemoved} = set_items(Config, [ItemRemoved]),
    {V5, []} = get_items(Config),
    del_roster(disconnect(Config), JID).

iq_set_many_items(Config) ->
    J1 = jid:decode(<<"nurse1@example.com">>),
    J2 = jid:decode(<<"nurse2@example.com">>),
    ct:comment("Trying to send roster-set with many <item/> elements"),
    Items = [#roster_item{jid = J1}, #roster_item{jid = J2}],
    #stanza_error{reason = 'bad-request'} = set_items(Config, Items),
    disconnect(Config).

iq_set_duplicated_groups(Config) ->
    JID = jid:decode(<<"nurse@example.com">>),
    G = randoms:get_string(),
    ct:comment("Trying to send roster-set with duplicated groups"),
    Item = #roster_item{jid = JID, groups = [G, G]},
    #stanza_error{reason = 'bad-request'} = set_items(Config, [Item]),
    disconnect(Config).

iq_set_ask(Config) ->
    JID = jid:decode(<<"nurse@example.com">>),
    ct:comment("Trying to send roster-set with 'ask' included"),
    Item = #roster_item{jid = JID, ask = subscribe},
    #stanza_error{reason = 'bad-request'} = set_items(Config, [Item]),
    disconnect(Config).

iq_get_item(Config) ->
    JID = jid:decode(<<"nurse@example.com">>),
    ct:comment("Trying to send roster-get with <item/> element"),
    #iq{type = error} = Err3 =
	send_recv(Config, #iq{type = get,
			      sub_els = [#roster_query{
					    items = [#roster_item{jid = JID}]}]}),
    #stanza_error{reason = 'bad-request'} = xmpp:get_error(Err3),
    disconnect(Config).

iq_unexpected_element(Config) ->
    JID = jid:decode(<<"nurse@example.com">>),
    ct:comment("Trying to send IQs with unexpected element"),
    lists:foreach(
      fun(Type) ->
	      #iq{type = error} = Err4 =
		  send_recv(Config, #iq{type = Type,
					sub_els = [#roster_item{jid = JID}]}),
	      #stanza_error{reason = 'service-unavailable'} = xmpp:get_error(Err4)
      end, [get, set]),
    disconnect(Config).

version(Config) ->
    JID = jid:decode(<<"nurse@example.com">>),
    ct:comment("Requesting roster"),
    {InitialVersion, _} = get_items(Config, <<"">>),
    ct:comment("Requesting roster with initial version"),
    {empty, []} = get_items(Config, InitialVersion),
    ct:comment("Adding JID to the roster"),
    {NewVersion, _} = set_items(Config, [#roster_item{jid = JID}]),
    ct:comment("Requesting roster with initial version"),
    {NewVersion, _} = get_items(Config, InitialVersion),
    ct:comment("Requesting roster with new version"),
    {empty, []} = get_items(Config, NewVersion),
    del_roster(disconnect(Config), JID).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {roster_master_slave, [sequence],
     [master_slave_test(subscribe)]}.

subscribe_master(Config) ->
    Actions = actions(),
    process_subscriptions_master(Config, Actions),
    del_roster(disconnect(Config)).

subscribe_slave(Config) ->
    process_subscriptions_slave(Config),
    del_roster(disconnect(Config)).

process_subscriptions_master(Config, Actions) ->
    EnumeratedActions = lists:zip(lists:seq(1, length(Actions)), Actions),
    self_presence(Config, available),
    lists:foldl(
      fun({N, {Dir, Type}}, State) ->
	      timer:sleep(100),
	      if Dir == out -> put_event(Config, {N, in, Type});
		 Dir == in -> put_event(Config, {N, out, Type})
	      end,
	      wait_for_slave(Config),
	      ct:pal("Performing ~s-~s (#~p) "
		     "in state:~n~s~nwith roster:~n~s",
		     [Dir, Type, N, pp(State),
		      pp(get_roster(Config))]),
	      transition(Config, Dir, Type, State)
      end, #state{}, EnumeratedActions),
    put_event(Config, done),
    wait_for_slave(Config),
    Config.

process_subscriptions_slave(Config) ->
    self_presence(Config, available),
    process_subscriptions_slave(Config, get_event(Config), #state{}).

process_subscriptions_slave(Config, done, _State) ->
    wait_for_master(Config),
    Config;
process_subscriptions_slave(Config, {N, Dir, Type}, State) ->
    wait_for_master(Config),
    ct:pal("Performing ~s-~s (#~p) "
	   "in state:~n~s~nwith roster:~n~s",
	   [Dir, Type, N, pp(State), pp(get_roster(Config))]),
    NewState = transition(Config, Dir, Type, State),
    process_subscriptions_slave(Config, get_event(Config), NewState).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("roster_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("roster_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("roster_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("roster_" ++ atom_to_list(T) ++ "_slave")]}.

get_items(Config) ->
    get_items(Config, <<"">>).

get_items(Config, Version) ->
    case send_recv(Config, #iq{type = get,
			       sub_els = [#roster_query{ver = Version}]}) of
	#iq{type = result,
	    sub_els = [#roster_query{ver = NewVersion, items = Items}]} ->
	    {NewVersion, Items};
	#iq{type = result, sub_els = []} ->
	    {empty, []};
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

get_item(Config, JID) ->
    case get_items(Config) of
	{_Ver, Items} when is_list(Items) ->
	    lists:keyfind(JID, #roster_item.jid, Items);
	_ ->
	    false
    end.

set_items(Config, Items) ->
    case send_recv(Config, #iq{type = set,
			       sub_els = [#roster_query{items = Items}]}) of
	#iq{type = result, sub_els = []} ->
	    recv_push(Config);
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

recv_push(Config) ->
    ct:comment("Receiving roster push"),
    Push = #iq{type = set,
	       sub_els = [#roster_query{ver = Ver, items = [PushItem]}]}
	= recv_iq(Config),
    send(Config, make_iq_result(Push)),
    {Ver, PushItem}.

recv_push(Config, Subscription, Ask) ->
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    Match = #roster_item{jid = PeerBareJID,
			 subscription = Subscription,
			 ask = Ask,
			 groups = [],
			 name = <<"">>},
    ct:comment("Receiving roster push"),
    Push = #iq{type = set, sub_els = [#roster_query{items = [Item]}]} =
	recv_iq(Config),
    case Item of
	Match -> send(Config, make_iq_result(Push));
	_ -> match_failure(Item, Match)
    end.

recv_presence(Config, Type) ->
    PeerJID = ?config(peer, Config),
    case recv_presence(Config) of
	#presence{from = PeerJID, type = Type} -> ok;
	Pres -> match_failure(Pres, #presence{from = PeerJID, type = Type})
    end.

recv_subscription(Config, Type) ->
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    case recv_presence(Config) of
	#presence{from = PeerBareJID, type = Type} -> ok;
	Pres -> match_failure(Pres, #presence{from = PeerBareJID, type = Type})
    end.

pp(Term) ->
    io_lib_pretty:print(Term, fun pp/2).

pp(state, N) ->
    Fs = record_info(fields, state),
    try N = length(Fs), Fs
    catch _:_ -> no end;
pp(roster, N) ->
    Fs = record_info(fields, roster),
    try N = length(Fs), Fs
    catch _:_ -> no end;
pp(_, _) -> no.

%% RFC6121, A.2.1
transition(Config, out, subscribe,
	   #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    send(Config, #presence{to = PeerBareJID, type = subscribe}),
    case {Sub, Out, In} of
	{none, false, _} ->
	    recv_push(Config, none, subscribe),
	    State#state{pending_out = true};
	{none, true, false} ->
	    %% BUG: we should not receive roster push here
	    recv_push(Config, none, subscribe),
	    State;
	{from, false, false} ->
	    recv_push(Config, from, subscribe),
	    State#state{pending_out = true};
	_ ->
	    State
    end;
%% RFC6121, A.2.2
transition(Config, out, unsubscribe,
	   #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    send(Config, #presence{to = PeerBareJID, type = unsubscribe}),
    case {Sub, Out, In} of
	{none, true, _} ->
	    recv_push(Config, none, undefined),
	    State#state{pending_out = false};
	{to, false, _} ->
	    recv_push(Config, none, undefined),
	    recv_presence(Config, unavailable),
	    State#state{subscription = none, peer_available = false};
	{from, true, false} ->
	    recv_push(Config, from, undefined),
	    State#state{pending_out = false};
	{both, false, false} ->
	    recv_push(Config, from, undefined),
	    recv_presence(Config, unavailable),
	    State#state{subscription = from, peer_available = false};
	_ ->
	    State
    end;
%% RFC6121, A.2.3
transition(Config, out, subscribed,
	    #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    send(Config, #presence{to = PeerBareJID, type = subscribed}),
    case {Sub, Out, In} of
	{none, false, true} ->
	    recv_push(Config, from, undefined),
	    State#state{subscription = from, pending_in = false};
	{none, true, true} ->
	    recv_push(Config, from, subscribe),
	    State#state{subscription = from, pending_in = false};
	{to, false, true} ->
	    recv_push(Config, both, undefined),
	    State#state{subscription = both, pending_in = false};
	{to, false, _} ->
	    %% BUG: we should not transition to 'both' state
	    recv_push(Config, both, undefined),
	    State#state{subscription = both};
	_ ->
	    State
    end;
%% RFC6121, A.2.4
transition(Config, out, unsubscribed,
	   #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    send(Config, #presence{to = PeerBareJID, type = unsubscribed}),
    case {Sub, Out, In} of
	{none, false, true} ->
	    State#state{subscription = none, pending_in = false};
	{none, true, true} ->
	    recv_push(Config, none, subscribe),
	    State#state{subscription = none, pending_in = false};
	{to, _, true} ->
	    State#state{pending_in = false};
	{from, false, _} ->
	    recv_push(Config, none, undefined),
	    State#state{subscription = none};
	{from, true, _} ->
	    recv_push(Config, none, subscribe),
	    State#state{subscription = none};
	{both, _, _} ->
	    recv_push(Config, to, undefined),
	    State#state{subscription = to};
	_ ->
	    State
    end;
%% RFC6121, A.3.1
transition(Config, in, subscribe = Type,
	   #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    case {Sub, Out, In} of
	{none, false, false} ->
	    recv_subscription(Config, Type),
	    State#state{pending_in = true};
	{none, true, false} ->
	    recv_push(Config, none, subscribe),
	    recv_subscription(Config, Type),
	    State#state{pending_in = true};
	{to, false, false} ->
	    %% BUG: we should not receive roster push in this state!
	    recv_push(Config, to, undefined),
	    recv_subscription(Config, Type),
	    State#state{pending_in = true};
	_ ->
	    State
    end;
%% RFC6121, A.3.2
transition(Config, in, unsubscribe = Type,
	   #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    case {Sub, Out, In} of
	{none, _, true} ->
	    State#state{pending_in = false};
	{to, _, true} ->
	    recv_push(Config, to, undefined),
	    recv_subscription(Config, Type),
	    State#state{pending_in = false};
	{from, false, _} ->
	    recv_push(Config, none, undefined),
	    recv_subscription(Config, Type),
	    State#state{subscription = none};
	{from, true, _} ->
	    recv_push(Config, none, subscribe),
	    recv_subscription(Config, Type),
	    State#state{subscription = none};
	{both, _, _} ->
	    recv_push(Config, to, undefined),
	    recv_subscription(Config, Type),
	    State#state{subscription = to};
	_ ->
	    State
    end;
%% RFC6121, A.3.3
transition(Config, in, subscribed = Type,
	   #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    case {Sub, Out, In} of
	{none, true, _} ->
	    recv_push(Config, to, undefined),
	    recv_subscription(Config, Type),
	    recv_presence(Config, available),
	    State#state{subscription = to, pending_out = false, peer_available = true};
	{from, true, _} ->
	    recv_push(Config, both, undefined),
	    recv_subscription(Config, Type),
	    recv_presence(Config, available),
	    State#state{subscription = both, pending_out = false, peer_available = true};
	{from, false, _} ->
	    %% BUG: we should not transition to 'both' in this state
	    recv_push(Config, both, undefined),
	    recv_subscription(Config, Type),
	    recv_presence(Config, available),
	    State#state{subscription = both, pending_out = false, peer_available = true};
	_ ->
	    State
    end;
%% RFC6121, A.3.4
transition(Config, in, unsubscribed = Type,
	   #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    case {Sub, Out, In} of
	{none, true, true} ->
	    %% BUG: we should receive roster push in this state!
	    recv_subscription(Config, Type),
	    State#state{subscription = none, pending_out = false};
	{none, true, false} ->
	    recv_push(Config, none, undefined),
	    recv_subscription(Config, Type),
	    State#state{subscription = none, pending_out = false};
	{none, false, false} ->
	    State;
	{to, false, _} ->
	    recv_push(Config, none, undefined),
	    recv_subscription(Config, Type),
	    recv_presence(Config, unavailable),
	    State#state{subscription = none, peer_available = false};
	{from, true, false} ->
	    recv_push(Config, from, undefined),
	    recv_subscription(Config, Type),
	    State#state{subscription = from, pending_out = false};
	{both, _, _} ->
	    recv_push(Config, from, undefined),
	    recv_subscription(Config, Type),
	    recv_presence(Config, unavailable),
	    State#state{subscription = from, peer_available = false};
	_ ->
	    State
    end;
%% Outgoing roster remove
transition(Config, out, remove,
	   #state{subscription = Sub, pending_in = In, pending_out = Out}) ->
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    Item = #roster_item{jid = PeerBareJID, subscription = remove},
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set,
			      sub_els = [#roster_query{items = [Item]}]}),
    recv_push(Config, remove, undefined),
    case {Sub, Out, In} of
	{to, _, _} ->
	    recv_presence(Config, unavailable);
	{both, _, _} ->
	    recv_presence(Config, unavailable);
	_ ->
	    ok
    end,
    #state{};
%% Incoming roster remove
transition(Config, in, remove,
	   #state{subscription = Sub, pending_in = In, pending_out = Out} = State) ->
    case {Sub, Out, In} of
	{none, true, _} ->
	    ok;
	{from, false, _} ->
	    recv_push(Config, none, undefined),
	    recv_subscription(Config, unsubscribe);
	{from, true, _} ->
	    recv_push(Config, none, subscribe),
	    recv_subscription(Config, unsubscribe);
	{to, false, _} ->
	    %% BUG: we should receive push here
	    %% recv_push(Config, none, undefined),
	    recv_presence(Config, unavailable),
	    recv_subscription(Config, unsubscribed);
	{both, _, _} ->
	    recv_presence(Config, unavailable),
	    recv_push(Config, to, undefined),
	    recv_subscription(Config, unsubscribe),
	    recv_push(Config, none, undefined),
	    recv_subscription(Config, unsubscribed);
	_ ->
	    ok
    end,
    State#state{subscription = none}.

actions() ->
    States = [{Dir, Type} || Dir <- [out, in],
			     Type <- [subscribe, subscribed,
				      unsubscribe, unsubscribed,
				      remove]],
    Actions = lists:flatten([[X, Y] || X <- States, Y <- States]),
    remove_dups(Actions, []).

remove_dups([X|T], [X,X|_] = Acc) ->
    remove_dups(T, Acc);
remove_dups([X|T], Acc) ->
    remove_dups(T, [X|Acc]);
remove_dups([], Acc) ->
    lists:reverse(Acc).
