%%%----------------------------------------------------------------------
%%% File    : node_flat_sql.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Standard PubSub node plugin with ODBC backend
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
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

%%% @doc The module <strong>{@module}</strong> is the default PubSub plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node
%%% types.</p>
%%% <p>PubSub plugin nodes are using the {@link gen_node} behaviour.</p>

-module(node_flat_sql).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-compile([{parse_transform, ejabberd_sql_pt}]).

-include("pubsub.hrl").
-include("xmpp.hrl").
-include("ejabberd_sql_pt.hrl").

-export([init/3, terminate/2, options/0, features/0,
    create_node_permission/6, create_node/2, delete_node/1,
    purge_node/2, subscribe_node/8, unsubscribe_node/4,
    publish_item/7, delete_item/4, remove_extra_items/3,
    get_entity_affiliations/2, get_node_affiliations/1,
    get_affiliation/2, set_affiliation/3,
    get_entity_subscriptions/2, get_node_subscriptions/1,
    get_subscriptions/2, set_subscriptions/4,
    get_pending_nodes/2, get_states/1, get_state/2,
    set_state/1, get_items/7, get_items/3, get_item/7,
    get_item/2, set_item/1, get_item_name/3, node_to_path/1,
    path_to_node/1,
    get_entity_subscriptions_for_send_last/2, get_last_items/3]).

-export([decode_jid/1, encode_jid/1,
         encode_jid_like/1,
    decode_affiliation/1, decode_subscriptions/1,
    encode_affiliation/1, encode_subscriptions/1,
         encode_host/1,
         encode_host_like/1]).

init(_Host, _ServerHost, _Opts) ->
    %%pubsub_subscription_sql:init(Host, ServerHost, Opts),
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{sql, true}, {rsm, true} | node_flat:options()].

features() ->
    [<<"rsm">> | node_flat:features()].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_flat:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    {_U, _S, _R} = OwnerKey = jid:tolower(jid:remove_resource(Owner)),
    J = encode_jid(OwnerKey),
    A = encode_affiliation(owner),
    S = encode_subscriptions([]),
    ejabberd_sql:sql_query_t(
      ?SQL("insert into pubsub_state("
	   "nodeid, jid, affiliation, subscriptions) "
	   "values (%(Nidx)d, %(J)s, %(A)s, %(S)s)")),
    {result, {default, broadcast}}.

delete_node(Nodes) ->
    Reply = lists:map(
	      fun(#pubsub_node{id = Nidx} = PubsubNode) ->
		      Subscriptions =
			  case ejabberd_sql:sql_query_t(
				 ?SQL("select @(jid)s, @(subscriptions)s "
				      "from pubsub_state where nodeid=%(Nidx)d")) of
			      {selected, RItems} ->
				  [{decode_jid(SJID), decode_subscriptions(Subs)}
				   || {SJID, Subs} <- RItems];
			      _ ->
				  []
			  end,
		      {PubsubNode, Subscriptions}
	      end, Nodes),
    {result, {default, broadcast, Reply}}.

subscribe_node(Nidx, Sender, Subscriber, AccessModel,
	    SendLast, PresenceSubscription, RosterGroup, _Options) ->
    SubKey = jid:tolower(Subscriber),
    GenKey = jid:remove_resource(SubKey),
    Authorized = jid:tolower(jid:remove_resource(Sender)) == GenKey,
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Whitelisted = lists:member(Affiliation, [member, publisher, owner]),
    PendingSubscription = lists:any(fun
		({pending, _}) -> true;
		(_) -> false
	    end,
	    Subscriptions),
    Owner = Affiliation == owner,
    if not Authorized ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_bad_request(), mod_pubsub:err_invalid_jid())};
	(Affiliation == outcast) or (Affiliation == publish_only) ->
	    {error, xmpp:err_forbidden()};
	PendingSubscription ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_authorized(),
		      mod_pubsub:err_pending_subscription())};
	(AccessModel == presence) and (not PresenceSubscription) and (not Owner) ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_authorized(),
		      mod_pubsub:err_presence_subscription_required())};
	(AccessModel == roster) and (not RosterGroup) and (not Owner) ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_authorized(),
		      mod_pubsub:err_not_in_roster_group())};
	(AccessModel == whitelist) and (not Whitelisted) and (not Owner) ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_allowed(), mod_pubsub:err_closed_node())};
	%%MustPay ->
	%%        % Payment is required for a subscription
	%%        {error, ?ERR_PAYMENT_REQUIRED};
	%%ForbiddenAnonymous ->
	%%        % Requesting entity is anonymous
	%%        {error, ?ERR_FORBIDDEN};
	true ->
	    %%{result, SubId} = pubsub_subscription_sql:subscribe_node(Subscriber, Nidx, Options),
	    {NewSub, SubId} = case Subscriptions of
		[{subscribed, Id}|_] ->
		    {subscribed, Id};
		[] ->
		    Id = pubsub_subscription_sql:make_subid(),
		    Sub = case AccessModel of
			authorize -> pending;
			_ -> subscribed
		    end,
		    update_subscription(Nidx, SubKey, [{Sub, Id} | Subscriptions]),
		    {Sub, Id}
	    end,
	    case {NewSub, SendLast} of
		{subscribed, never} ->
		    {result, {default, subscribed, SubId}};
		{subscribed, _} ->
		    {result, {default, subscribed, SubId, send_last}};
		{_, _} ->
		    {result, {default, pending, SubId}}
	    end
    end.

unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    SubKey = jid:tolower(Subscriber),
    GenKey = jid:remove_resource(SubKey),
    Authorized = jid:tolower(jid:remove_resource(Sender)) == GenKey,
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, SubKey),
    SubIdExists = case SubId of
	<<>> -> false;
	Binary when is_binary(Binary) -> true;
	_ -> false
    end,
    if
	%% Requesting entity is prohibited from unsubscribing entity
	not Authorized ->
	    {error, xmpp:err_forbidden()};
	%% Entity did not specify SubId
	%%SubId == "", ?? ->
	%%        {error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%% Invalid subscription identifier
	%%InvalidSubId ->
	%%        {error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	%% Requesting entity is not a subscriber
	Subscriptions == [] ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_unexpected_request(),
		      mod_pubsub:err_not_subscribed())};
	%% Subid supplied, so use that.
	SubIdExists ->
	    Sub = first_in_list(fun
			({_, S}) when S == SubId -> true;
			(_) -> false
		    end,
		    Subscriptions),
	    case Sub of
		{value, S} ->
		    delete_subscription(SubKey, Nidx, S, Affiliation, Subscriptions),
		    {result, default};
		false ->
		    {error, mod_pubsub:extended_error(
			      xmpp:err_unexpected_request(),
			      mod_pubsub:err_not_subscribed())}
	    end;
	%% Asking to remove all subscriptions to the given node
	SubId == all ->
	    [delete_subscription(SubKey, Nidx, S, Affiliation, Subscriptions)
		|| S <- Subscriptions],
	    {result, default};
	%% No subid supplied, but there's only one matching subscription
	length(Subscriptions) == 1 ->
	    delete_subscription(SubKey, Nidx, hd(Subscriptions), Affiliation, Subscriptions),
	    {result, default};
	%% No subid and more than one possible subscription match.
	true ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_bad_request(), mod_pubsub:err_subid_required())}
    end.

delete_subscription(SubKey, Nidx, {Subscription, SubId}, Affiliation, Subscriptions) ->
    NewSubs = Subscriptions -- [{Subscription, SubId}],
    %%pubsub_subscription_sql:unsubscribe_node(SubKey, Nidx, SubId),
    case {Affiliation, NewSubs} of
	{none, []} -> del_state(Nidx, SubKey);
	_ -> update_subscription(Nidx, SubKey, NewSubs)
    end.

publish_item(Nidx, Publisher, PublishModel, MaxItems, ItemId, Payload,
	     _PubOpts) ->
    SubKey = jid:tolower(Publisher),
    GenKey = jid:remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Subscribed = case PublishModel of
	subscribers -> node_flat:is_subscribed(Subscriptions);
	_ -> undefined
    end,
    if not ((PublishModel == open) or
		    (PublishModel == publishers) and
		    ((Affiliation == owner)
			 or (Affiliation == publisher)
			 or (Affiliation == publish_only))
		    or (Subscribed == true)) ->
	    {error, xmpp:err_forbidden()};
	true ->
	    if MaxItems > 0 ->
		    PubId = {p1_time_compat:timestamp(), SubKey},
		    set_item(#pubsub_item{itemid = {ItemId, Nidx},
			    creation = {p1_time_compat:timestamp(), GenKey},
			    modification = PubId,
			    payload = Payload}),
		    Items = [ItemId | itemids(Nidx, GenKey) -- [ItemId]],
		    {result, {_, OI}} = remove_extra_items(Nidx, MaxItems, Items),
		    {result, {default, broadcast, OI}};
		true ->
		    {result, {default, broadcast, []}}
	    end
    end.

remove_extra_items(_Nidx, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(Nidx, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    del_items(Nidx, OldItems),
    {result, {NewItems, OldItems}}.

delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    SubKey = jid:tolower(Publisher),
    GenKey = jid:remove_resource(SubKey),
    {result, Affiliation} = get_affiliation(Nidx, GenKey),
    Allowed = Affiliation == publisher orelse
	Affiliation == owner orelse
	(PublishModel == open andalso
	  case get_item(Nidx, ItemId) of
	    {result, #pubsub_item{creation = {_, GenKey}}} -> true;
	    _ -> false
          end),
    if not Allowed ->
	    {error, xmpp:err_forbidden()};
	true ->
	    case del_item(Nidx, ItemId) of
		{updated, 1} -> {result, {default, broadcast}};
		_ -> {error, xmpp:err_item_not_found()}
	    end
    end.

purge_node(Nidx, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    GenState = get_state(Nidx, GenKey),
    case GenState of
	#pubsub_state{affiliation = owner} ->
	    {result, States} = get_states(Nidx),
	    lists:foreach(fun
		    (#pubsub_state{items = []}) -> ok;
		    (#pubsub_state{items = Items}) -> del_items(Nidx, Items)
		end,
		States),
	    {result, {default, broadcast}};
	_ ->
	    {error, xmpp:err_forbidden()}
    end.

get_entity_affiliations(Host, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    H = encode_host(Host),
    J = encode_jid(GenKey),
    {result,
     case ejabberd_sql:sql_query_t(
	    ?SQL("select @(node)s, @(type)s, @(i.nodeid)d, @(affiliation)s "
		 "from pubsub_state i, pubsub_node n where "
		 "i.nodeid = n.nodeid and jid=%(J)s and host=%(H)s")) of
	 {selected, RItems} ->
	     [{nodetree_tree_sql:raw_to_node(Host, {N, <<"">>, T, I}),
	       decode_affiliation(A)} || {N, T, I, A} <- RItems];
	 _ ->
	     []
     end}.

get_node_affiliations(Nidx) ->
    {result,
     case ejabberd_sql:sql_query_t(
	    ?SQL("select @(jid)s, @(affiliation)s from pubsub_state "
		 "where nodeid=%(Nidx)d")) of
	 {selected, RItems} ->
	     [{decode_jid(J), decode_affiliation(A)} || {J, A} <- RItems];
	 _ ->
	     []
     end}.

get_affiliation(Nidx, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    J = encode_jid(GenKey),
    {result,
     case ejabberd_sql:sql_query_t(
	    ?SQL("select @(affiliation)s from pubsub_state "
		 "where nodeid=%(Nidx)d and jid=%(J)s")) of
	 {selected, [{A}]} ->
	     decode_affiliation(A);
	 _ ->
	     none
     end}.

set_affiliation(Nidx, Owner, Affiliation) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    {_, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey),
    case {Affiliation, Subscriptions} of
	{none, []} -> del_state(Nidx, GenKey);
	_ -> update_affiliation(Nidx, GenKey, Affiliation)
    end.

get_entity_subscriptions(Host, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    H = encode_host(Host),
    SJ = encode_jid(SubKey),
    GJ = encode_jid(GenKey),
    GJLike = <<(encode_jid_like(GenKey))/binary, "/%">>,
    Query =
        case SubKey of
            GenKey ->
                ?SQL("select @(node)s, @(type)s, @(i.nodeid)d,"
                     " @(jid)s, @(subscriptions)s "
                     "from pubsub_state i, pubsub_node n "
                     "where i.nodeid = n.nodeid and "
                     "(jid=%(GJ)s or jid like %(GJLike)s escape '^')"
                     " and host=%(H)s");
            _ ->
                ?SQL("select @(node)s, @(type)s, @(i.nodeid)d,"
                     " @(jid)s, @(subscriptions)s "
                     "from pubsub_state i, pubsub_node n "
                     "where i.nodeid = n.nodeid and"
                     " jid in (%(SJ)s, %(GJ)s) and host=%(H)s")
        end,
    {result,
     case ejabberd_sql:sql_query_t(Query) of
	 {selected, RItems} ->
	     lists:foldl(
	       fun({N, T, I, J, S}, Acc) ->
		       Node = nodetree_tree_sql:raw_to_node(Host, {N, <<"">>, T, I}),
		       Jid = decode_jid(J),
		       case decode_subscriptions(S) of
			   [] ->
			       [{Node, none, Jid} | Acc];
			   Subs ->
			       lists:foldl(
				 fun({Sub, SubId}, Acc2) ->
					 [{Node, Sub, SubId, Jid} | Acc2]
				 end, Acc, Subs)
		       end
	       end, [], RItems);
	 _ ->
	     []
     end}.

-spec get_entity_subscriptions_for_send_last(Host :: mod_pubsub:hostPubsub(),
					     Owner :: jid()) ->
						    {result, [{mod_pubsub:pubsubNode(),
							       mod_pubsub:subscription(),
							       mod_pubsub:subId(),
							       ljid()}]}.

get_entity_subscriptions_for_send_last(Host, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    H = encode_host(Host),
    SJ = encode_jid(SubKey),
    GJ = encode_jid(GenKey),
    GJLike = <<(encode_jid_like(GenKey))/binary, "/%">>,
    Query =
        case SubKey of
            GenKey ->
                ?SQL("select @(node)s, @(type)s, @(i.nodeid)d,"
                     " @(jid)s, @(subscriptions)s "
                     "from pubsub_state i, pubsub_node n, pubsub_node_option o "
                     "where i.nodeid = n.nodeid and n.nodeid = o.nodeid and name='send_last_published_item' "
                     "and val='on_sub_and_presence' and "
                     "(jid=%(GJ)s or jid like %(GJLike)s escape '^')"
                     " and host=%(H)s");
            _ ->
                ?SQL("select @(node)s, @(type)s, @(i.nodeid)d,"
                     " @(jid)s, @(subscriptions)s "
                     "from pubsub_state i, pubsub_node n, pubsub_node_option o "
                     "where i.nodeid = n.nodeid and n.nodeid = o.nodeid and name='send_last_published_item' "
                     "and val='on_sub_and_presence' and"
                     " jid in (%(SJ)s, %(GJ)s) and host=%(H)s")
    end,
    {result,
     case ejabberd_sql:sql_query_t(Query) of
	 {selected, RItems} ->
	     lists:foldl(
	       fun ({N, T, I, J, S}, Acc) ->
		       Node = nodetree_tree_sql:raw_to_node(Host, {N, <<"">>, T, I}),
		       Jid = decode_jid(J),
		       case decode_subscriptions(S) of
			   [] ->
			       [{Node, none, Jid} | Acc];
			   Subs ->
			       lists:foldl(
				 fun ({Sub, SubId}, Acc2) ->
					 [{Node, Sub, SubId, Jid}| Acc2]
				 end, Acc, Subs)
		       end
	       end, [], RItems);
	 _ ->
	     []
     end}.

get_node_subscriptions(Nidx) ->
    {result,
     case ejabberd_sql:sql_query_t(
	    ?SQL("select @(jid)s, @(subscriptions)s from pubsub_state "
		 "where nodeid=%(Nidx)d")) of
	 {selected, RItems} ->
	     lists:foldl(
	       fun ({J, S}, Acc) ->
		       Jid = decode_jid(J),
		       case decode_subscriptions(S) of
			   [] ->
			       [{Jid, none} | Acc];
			   Subs ->
			       lists:foldl(
				 fun ({Sub, SubId}, Acc2) ->
					 [{Jid, Sub, SubId} | Acc2]
				 end, Acc, Subs)
		       end
	       end, [], RItems);
	 _ ->
	     []
     end}.

get_subscriptions(Nidx, Owner) ->
    SubKey = jid:tolower(Owner),
    J = encode_jid(SubKey),
    {result,
     case ejabberd_sql:sql_query_t(
	    ?SQL("select @(subscriptions)s from pubsub_state"
		 " where nodeid=%(Nidx)d and jid=%(J)s")) of
	 {selected, [{S}]} ->
	     decode_subscriptions(S);
	 _ ->
	     []
     end}.

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    SubKey = jid:tolower(Owner),
    SubState = get_state_without_itemids(Nidx, SubKey),
    case {SubId, SubState#pubsub_state.subscriptions} of
	{_, []} ->
	    case Subscription of
		none ->
		    {error, mod_pubsub:extended_error(
			      xmpp:err_bad_request(),
			      mod_pubsub:err_not_subscribed())};
		_ ->
		    new_subscription(Nidx, Owner, Subscription, SubState)
	    end;
	{<<>>, [{_, SID}]} ->
	    case Subscription of
		none -> unsub_with_subid(Nidx, SID, SubState);
		_ -> replace_subscription({Subscription, SID}, SubState)
	    end;
	{<<>>, [_ | _]} ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_bad_request(),
		      mod_pubsub:err_subid_required())};
	_ ->
	    case Subscription of
		none -> unsub_with_subid(Nidx, SubId, SubState);
		_ -> replace_subscription({Subscription, SubId}, SubState)
	    end
    end.

replace_subscription(NewSub, SubState) ->
    NewSubs = replace_subscription(NewSub, SubState#pubsub_state.subscriptions, []),
    set_state(SubState#pubsub_state{subscriptions = NewSubs}).

replace_subscription(_, [], Acc) -> Acc;
replace_subscription({Sub, SubId}, [{_, SubId} | T], Acc) ->
    replace_subscription({Sub, SubId}, T, [{Sub, SubId} | Acc]).

new_subscription(_Nidx, _Owner, Subscription, SubState) ->
    %%{result, SubId} = pubsub_subscription_sql:subscribe_node(Owner, Nidx, []),
    SubId = pubsub_subscription_sql:make_subid(),
    Subscriptions = [{Subscription, SubId} | SubState#pubsub_state.subscriptions],
    set_state(SubState#pubsub_state{subscriptions = Subscriptions}),
    {Subscription, SubId}.

unsub_with_subid(Nidx, SubId, SubState) ->
    %%pubsub_subscription_sql:unsubscribe_node(SubState#pubsub_state.stateid, Nidx, SubId),
    NewSubs = [{S, Sid}
	    || {S, Sid} <- SubState#pubsub_state.subscriptions,
		SubId =/= Sid],
    case {NewSubs, SubState#pubsub_state.affiliation} of
	{[], none} -> del_state(Nidx, element(1, SubState#pubsub_state.stateid));
	_ -> set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

get_pending_nodes(Host, Owner) ->
    GenKey = jid:remove_resource(jid:tolower(Owner)),
    States = mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'},
		affiliation = owner, _ = '_'}),
    Nidxxs = [Nidx || #pubsub_state{stateid = {_, Nidx}} <- States],
    NodeTree = mod_pubsub:tree(Host),
    Reply = mnesia:foldl(fun (#pubsub_state{stateid = {_, Nidx}} = S, Acc) ->
		    case lists:member(Nidx, Nidxxs) of
			true ->
			    case get_nodes_helper(NodeTree, S) of
				{value, Node} -> [Node | Acc];
				false -> Acc
			    end;
			false ->
			    Acc
		    end
	    end,
	    [], pubsub_state),
    {result, Reply}.

get_nodes_helper(NodeTree, #pubsub_state{stateid = {_, N}, subscriptions = Subs}) ->
    HasPending = fun
	({pending, _}) -> true;
	(pending) -> true;
	(_) -> false
    end,
    case lists:any(HasPending, Subs) of
	true ->
	    case NodeTree:get_node(N) of
		#pubsub_node{nodeid = {_, Node}} -> {value, Node};
		_ -> false
	    end;
	false ->
	    false
    end.

get_states(Nidx) ->
    case ejabberd_sql:sql_query_t(
	   ?SQL("select @(jid)s, @(affiliation)s, @(subscriptions)s "
		"from pubsub_state where nodeid=%(Nidx)d")) of
	{selected, RItems} ->
	    {result,
	     lists:map(
	       fun({SJID, Aff, Subs}) ->
		       JID = decode_jid(SJID),
		       #pubsub_state{stateid = {JID, Nidx},
				     items = itemids(Nidx, JID),
				     affiliation = decode_affiliation(Aff),
				     subscriptions = decode_subscriptions(Subs)}
	       end, RItems)};
	_ ->
	    {result, []}
    end.

get_state(Nidx, JID) ->
    State = get_state_without_itemids(Nidx, JID),
    {SJID, _} = State#pubsub_state.stateid,
    State#pubsub_state{items = itemids(Nidx, SJID)}.

-spec get_state_without_itemids(Nidx :: mod_pubsub:nodeIdx(), Key :: ljid()) ->
				       mod_pubsub:pubsubState().

get_state_without_itemids(Nidx, JID) ->
    J = encode_jid(JID),
    case ejabberd_sql:sql_query_t(
	   ?SQL("select @(jid)s, @(affiliation)s, @(subscriptions)s "
		"from pubsub_state "
		"where nodeid=%(Nidx)d and jid=%(J)s")) of
	{selected, [{SJID, Aff, Subs}]} ->
	    #pubsub_state{stateid = {decode_jid(SJID), Nidx},
			  nodeidx = Nidx,
			  affiliation = decode_affiliation(Aff),
			  subscriptions = decode_subscriptions(Subs)};
	_ ->
	    #pubsub_state{stateid = {JID, Nidx}, nodeidx = Nidx}
    end.

set_state(State) ->
    {_, Nidx} = State#pubsub_state.stateid,
    set_state(Nidx, State).

set_state(Nidx, State) ->
    {JID, _} = State#pubsub_state.stateid,
    J = encode_jid(JID),
    S = encode_subscriptions(State#pubsub_state.subscriptions),
    A = encode_affiliation(State#pubsub_state.affiliation),
    ?SQL_UPSERT_T(
       "pubsub_state",
       ["!nodeid=%(Nidx)d",
        "!jid=%(J)s",
        "affiliation=%(A)s",
        "subscriptions=%(S)s"
       ]),
    ok.

del_state(Nidx, JID) ->
    J = encode_jid(JID),
    catch ejabberd_sql:sql_query_t(
            ?SQL("delete from pubsub_state"
                 " where jid=%(J)s and nodeid=%(Nidx)d")),
    ok.

get_items(Nidx, From, undefined) ->
    MaxItems = case ejabberd_sql:sql_query_t(
		      ?SQL("select @(val)s from pubsub_node_option "
			   "where nodeid=%(Nidx)d and name='max_items'")) of
		   {selected, [{Value}]} ->
		       misc:expr_to_term(Value);
		   _ ->
		       ?MAXITEMS
	       end,
    get_items(Nidx, From, #rsm_set{max = MaxItems});
get_items(Nidx, _From, #rsm_set{max = Max, index = IncIndex,
				'after' = After, before = Before}) ->
    {Way, Order} = if After == <<>> -> {<<"is not">>, <<"desc">>};
		      After /= undefined -> {<<"<">>, <<"desc">>};
		      Before == <<>> -> {<<"is not">>, <<"asc">>};
		      Before /= undefined -> {<<">">>, <<"asc">>};
		      true -> {<<"is not">>, <<"desc">>}
		   end,
    SNidx = misc:i2l(Nidx),
    I = if After /= undefined -> After;
	   Before /= undefined -> Before;
	   true -> undefined
	end,
    [AttrName, Id] =
	case I of
	    undefined when IncIndex =/= undefined ->
		case ejabberd_sql:sql_query_t(
		       [<<"select creation from pubsub_item pi "
			  "where exists ( select count(*) as count1 "
			  "from pubsub_item where nodeid='">>, SNidx,
			<<"' and creation > pi.creation having count1 = ">>,
			integer_to_binary(IncIndex), <<" );">>]) of
		    {selected, [_], [[O]]} ->
			[<<"creation">>, <<"'", O/binary, "'">>];
		    _ ->
			[<<"creation">>, <<"null">>]
		end;
	    undefined ->
		[<<"creation">>, <<"null">>];
	    <<>> ->
		[<<"creation">>, <<"null">>];
	    I ->
		[A, B] = str:tokens(ejabberd_sql:escape(I), <<"@">>),
		[A, <<"'", B/binary, "'">>]
	end,
    Count = case ejabberd_sql:sql_query_t(
		   [<<"select count(*) from pubsub_item where nodeid='">>,
		    SNidx, <<"';">>]) of
		{selected, [_], [[C]]} -> binary_to_integer(C);
		_ -> 0
	    end,
    Query = fun(mssql, _) ->
		    ejabberd_sql:sql_query_t(
		      [<<"select top ">>, integer_to_binary(Max),
		       <<" itemid, publisher, creation, modification, payload "
			 "from pubsub_item where nodeid='">>, SNidx,
		       <<"' and ">>, AttrName, <<" ">>, Way, <<" ">>, Id, <<" order by ">>,
		       AttrName, <<" ">>, Order, <<";">>]);
	       (_, _) ->
		    ejabberd_sql:sql_query_t(
		      [<<"select itemid, publisher, creation, modification, payload "
			 "from pubsub_item where nodeid='">>, SNidx,
		       <<"' and ">>, AttrName, <<" ">>, Way, <<" ">>, Id, <<" order by ">>,
		       AttrName, <<" ">>, Order, <<" limit ">>,
		       integer_to_binary(Max), <<" ;">>])
	    end,
    case ejabberd_sql:sql_query_t(Query) of
	{selected, [<<"itemid">>, <<"publisher">>, <<"creation">>,
		    <<"modification">>, <<"payload">>], RItems} ->
	    case RItems of
		[[_, _, _, F, _]|_] ->
		    Index = case catch ejabberd_sql:sql_query_t(
					 [<<"select count(*) from pubsub_item "
					    "where nodeid='">>, SNidx, <<"' and ">>,
					  AttrName, <<" > '">>, F, <<"';">>]) of
				{selected, [_], [[In]]} -> binary_to_integer(In);
				_ -> 0
			    end,
		    [_, _, _, L, _] = lists:last(RItems),
		    RsmOut = #rsm_set{count = Count,
				      index = Index,
				      first = #rsm_first{
						 index = Index,
						 data = <<"creation@", F/binary>>},
				      last = <<"creation@", L/binary>>},
		    {result, {[raw_to_item(Nidx, RItem) || RItem <- RItems], RsmOut}};
		[] ->
		    {result, {[], #rsm_set{count = Count}}}
	    end;
	_ ->
	    {result, {[], undefined}}
    end.

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId, RSM) ->
    SubKey = jid:tolower(JID),
    GenKey = jid:remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Whitelisted = node_flat:can_fetch_item(Affiliation, Subscriptions),
    if %%SubId == "", ?? ->
	%% Entity has multiple subscriptions to the node but does not specify a subscription ID
	%{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%%InvalidSubId ->
	%% Entity is subscribed but specifies an invalid subscription ID
	%{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	(Affiliation == outcast) or (Affiliation == publish_only) ->
	    {error, xmpp:err_forbidden()};
	(AccessModel == presence) and not PresenceSubscription ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_authorized(),
		      mod_pubsub:err_presence_subscription_required())};
	(AccessModel == roster) and not RosterGroup ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_authorized(),
		      mod_pubsub:err_not_in_roster_group())};
	(AccessModel == whitelist) and not Whitelisted ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_allowed(), mod_pubsub:err_closed_node())};
	(AccessModel == authorize) and not Whitelisted ->
	    {error, xmpp:err_forbidden()};
	%%MustPay ->
	%%        % Payment is required for a subscription
	%%        {error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_items(Nidx, JID, RSM)
    end.

get_last_items(Nidx, _From, Count) ->
    Limit = misc:i2l(Count),
    SNidx = misc:i2l(Nidx),
    Query = fun(mssql, _) ->
		    ejabberd_sql:sql_query_t(
		      [<<"select top ">>, Limit,
		       <<" itemid, publisher, creation, modification, payload "
			 "from pubsub_item where nodeid='">>, SNidx,
		       <<"' order by modification desc ;">>]);
	       (_, _) ->
		    ejabberd_sql:sql_query_t(
		      [<<"select itemid, publisher, creation, modification, payload "
			 "from pubsub_item where nodeid='">>, SNidx,
		       <<"' order by modification desc limit ">>, Limit, <<";">>])
	    end,
    case catch ejabberd_sql:sql_query_t(Query) of
	{selected,
		    [<<"itemid">>, <<"publisher">>, <<"creation">>, <<"modification">>, <<"payload">>], RItems} ->
	    {result, [raw_to_item(Nidx, RItem) || RItem <- RItems]};
	_ ->
	    {result, []}
    end.

get_item(Nidx, ItemId) ->
    case catch ejabberd_sql:sql_query_t(
                 ?SQL("select @(itemid)s, @(publisher)s, @(creation)s,"
                      " @(modification)s, @(payload)s from pubsub_item"
                      " where nodeid=%(Nidx)d and itemid=%(ItemId)s"))
    of
	{selected, [RItem]} ->
	    {result, raw_to_item(Nidx, RItem)};
	{selected, []} ->
	    {error, xmpp:err_item_not_found()};
	{'EXIT', _} ->
	    {error, xmpp:err_internal_server_error(<<"Database failure">>, ?MYLANG)}
    end.

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = jid:tolower(JID),
    GenKey = jid:remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Whitelisted = node_flat:can_fetch_item(Affiliation, Subscriptions),
    if %%SubId == "", ?? ->
	%% Entity has multiple subscriptions to the node but does not specify a subscription ID
	%{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%%InvalidSubId ->
	%% Entity is subscribed but specifies an invalid subscription ID
	%{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	(Affiliation == outcast) or (Affiliation == publish_only) ->
	    {error, xmpp:err_forbidden()};
	(AccessModel == presence) and not PresenceSubscription ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_authorized(),
		      mod_pubsub:err_presence_subscription_required())};
	(AccessModel == roster) and not RosterGroup ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_authorized(),
		      mod_pubsub:err_not_in_roster_group())};
	(AccessModel == whitelist) and not Whitelisted ->
	    {error, mod_pubsub:extended_error(
		      xmpp:err_not_allowed(), mod_pubsub:err_closed_node())};
	(AccessModel == authorize) and not Whitelisted ->
	    {error, xmpp:err_forbidden()};
	%%MustPay ->
	%%        % Payment is required for a subscription
	%%        {error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_item(Nidx, ItemId)
    end.

set_item(Item) ->
    {ItemId, Nidx} = Item#pubsub_item.itemid,
    {C, _} = Item#pubsub_item.creation,
    {M, JID} = Item#pubsub_item.modification,
    P = encode_jid(JID),
    Payload = Item#pubsub_item.payload,
    XML = str:join([fxml:element_to_binary(X) || X<-Payload], <<>>),
    S = fun ({T1, T2, T3}) ->
	    str:join([misc:i2l(T1, 6), misc:i2l(T2, 6), misc:i2l(T3, 6)], <<":">>)
    end,
    SM = S(M),
    SC = S(C),
    ?SQL_UPSERT_T(
       "pubsub_item",
       ["!nodeid=%(Nidx)d",
        "!itemid=%(ItemId)s",
        "publisher=%(P)s",
        "modification=%(SM)s",
        "payload=%(XML)s",
        "-creation=%(SC)s"
       ]),
    ok.

del_item(Nidx, ItemId) ->
    catch ejabberd_sql:sql_query_t(
            ?SQL("delete from pubsub_item where itemid=%(ItemId)s"
                 " and nodeid=%(Nidx)d")).

del_items(_, []) ->
    ok;
del_items(Nidx, [ItemId]) ->
    del_item(Nidx, ItemId);
del_items(Nidx, ItemIds) ->
    I = str:join([[<<"'">>, ejabberd_sql:escape(X), <<"'">>] || X <- ItemIds], <<",">>),
    SNidx = misc:i2l(Nidx),
    catch
    ejabberd_sql:sql_query_t([<<"delete from pubsub_item where itemid in (">>,
	    I, <<") and nodeid='">>, SNidx, <<"';">>]).

get_item_name(_Host, _Node, Id) ->
    Id.

node_to_path(Node) ->
    node_flat:node_to_path(Node).

path_to_node(Path) ->
    node_flat:path_to_node(Path).


first_in_list(_Pred, []) ->
    false;
first_in_list(Pred, [H | T]) ->
    case Pred(H) of
	true -> {value, H};
	_ -> first_in_list(Pred, T)
    end.

itemids(Nidx, {_U, _S, _R} = JID) ->
    SJID = encode_jid(JID),
    SJIDLike = <<(encode_jid_like(JID))/binary, "/%">>,
    case catch
	ejabberd_sql:sql_query_t(
          ?SQL("select @(itemid)s from pubsub_item where "
               "nodeid=%(Nidx)d and (publisher=%(SJID)s"
               " or publisher like %(SJIDLike)s escape '^') "
               "order by modification desc"))
    of
	{selected, RItems} ->
	    [ItemId || {ItemId} <- RItems];
	_ ->
	    []
    end.

select_affiliation_subscriptions(Nidx, JID) ->
    J = encode_jid(JID),
    case catch
	ejabberd_sql:sql_query_t(
          ?SQL("select @(affiliation)s, @(subscriptions)s from "
               " pubsub_state where nodeid=%(Nidx)d and jid=%(J)s"))
    of
	{selected, [{A, S}]} ->
	    {decode_affiliation(A), decode_subscriptions(S)};
	_ ->
	    {none, []}
    end.

select_affiliation_subscriptions(Nidx, JID, JID) ->
    select_affiliation_subscriptions(Nidx, JID);
select_affiliation_subscriptions(Nidx, GenKey, SubKey) ->
    {result, Affiliation} = get_affiliation(Nidx, GenKey),
    {result, BareJidSubs} = get_subscriptions(Nidx, GenKey),
    {result, FullJidSubs} = get_subscriptions(Nidx, SubKey),
    {Affiliation, BareJidSubs++FullJidSubs}.

update_affiliation(Nidx, JID, Affiliation) ->
    J = encode_jid(JID),
    A = encode_affiliation(Affiliation),
    ?SQL_UPSERT_T(
       "pubsub_state",
       ["!nodeid=%(Nidx)d",
        "!jid=%(J)s",
        "affiliation=%(A)s",
        "-subscriptions=''"
       ]).

update_subscription(Nidx, JID, Subscription) ->
    J = encode_jid(JID),
    S = encode_subscriptions(Subscription),
    ?SQL_UPSERT_T(
       "pubsub_state",
       ["!nodeid=%(Nidx)d",
        "!jid=%(J)s",
        "subscriptions=%(S)s",
        "-affiliation='n'"
       ]).

-spec decode_jid(SJID :: binary()) -> ljid().
decode_jid(SJID) ->
    jid:tolower(jid:decode(SJID)).

-spec decode_affiliation(Arg :: binary()) -> atom().
decode_affiliation(<<"o">>) -> owner;
decode_affiliation(<<"p">>) -> publisher;
decode_affiliation(<<"u">>) -> publish_only;
decode_affiliation(<<"m">>) -> member;
decode_affiliation(<<"c">>) -> outcast;
decode_affiliation(_) -> none.

-spec decode_subscription(Arg :: binary()) -> atom().
decode_subscription(<<"s">>) -> subscribed;
decode_subscription(<<"p">>) -> pending;
decode_subscription(<<"u">>) -> unconfigured;
decode_subscription(_) -> none.

-spec decode_subscriptions(Subscriptions :: binary()) -> [] | [{atom(), binary()},...].
decode_subscriptions(Subscriptions) ->
    lists:foldl(fun (Subscription, Acc) ->
		case str:tokens(Subscription, <<":">>) of
		    [S, SubId] -> [{decode_subscription(S), SubId} | Acc];
		    _ -> Acc
		end
	end,
	[], str:tokens(Subscriptions, <<",">>)).

-spec encode_jid(JID :: ljid()) -> binary().
encode_jid(JID) ->
    jid:encode(JID).

-spec encode_jid_like(JID :: ljid()) -> binary().
encode_jid_like(JID) ->
    ejabberd_sql:escape_like_arg_circumflex(jid:encode(JID)).

-spec encode_host(Host :: host()) -> binary().
encode_host({_U, _S, _R} = LJID) -> encode_jid(LJID);
encode_host(Host) -> Host.

-spec encode_host_like(Host :: host()) -> binary().
encode_host_like({_U, _S, _R} = LJID) -> ejabberd_sql:escape(encode_jid_like(LJID));
encode_host_like(Host) ->
    ejabberd_sql:escape(ejabberd_sql:escape_like_arg_circumflex(Host)).

-spec encode_affiliation(Arg :: atom()) -> binary().
encode_affiliation(owner) -> <<"o">>;
encode_affiliation(publisher) -> <<"p">>;
encode_affiliation(publish_only) -> <<"u">>;
encode_affiliation(member) -> <<"m">>;
encode_affiliation(outcast) -> <<"c">>;
encode_affiliation(_) -> <<"n">>.

-spec encode_subscription(Arg :: atom()) -> binary().
encode_subscription(subscribed) -> <<"s">>;
encode_subscription(pending) -> <<"p">>;
encode_subscription(unconfigured) -> <<"u">>;
encode_subscription(_) -> <<"n">>.

-spec encode_subscriptions(Subscriptions :: [] | [{atom(), binary()},...]) -> binary().
encode_subscriptions(Subscriptions) ->
    str:join([<<(encode_subscription(S))/binary, ":", SubId/binary>>
	    || {S, SubId} <- Subscriptions], <<",">>).

%%% record getter/setter

raw_to_item(Nidx, [ItemId, SJID, Creation, Modification, XML]) ->
    raw_to_item(Nidx, {ItemId, SJID, Creation, Modification, XML});
raw_to_item(Nidx, {ItemId, SJID, Creation, Modification, XML}) ->
    JID = decode_jid(SJID),
    ToTime = fun (Str) ->
	    [T1, T2, T3] = str:tokens(Str, <<":">>),
	    {misc:l2i(T1), misc:l2i(T2), misc:l2i(T3)}
    end,
    Payload = case fxml_stream:parse_element(XML) of
	{error, _Reason} -> [];
	El -> [El]
    end,
    #pubsub_item{itemid = {ItemId, Nidx},
	creation = {ToTime(Creation), jid:remove_resource(JID)},
	modification = {ToTime(Modification), JID},
	payload = Payload}.
