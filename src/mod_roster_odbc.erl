%%%----------------------------------------------------------------------
%%% File    : mod_roster_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 15 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_roster_odbc).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 process_iq/3,
	 process_local_iq/3,
	 get_user_roster/2,
	 get_subscription_lists/3,
	 get_in_pending_subscriptions/3,
	 in_subscription/6,
	 out_subscription/4,
	 set_items/3,
	 remove_user/2,
	 get_jid_info/4,
	 webadmin_page/3,
	 webadmin_user/4]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").


start(Host, Opts) ->
    HostB = list_to_binary(Host),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(roster_get, HostB,
		       ?MODULE, get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription, HostB,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, HostB,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_subscription_lists, HostB,
		       ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:add(roster_get_jid_info, HostB,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(resend_subscription_requests_hook, HostB,
		       ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:add(webadmin_page_host, HostB,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, HostB,
		       ?MODULE, webadmin_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_ROSTER,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(roster_get, HostB,
			  ?MODULE, get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription, HostB,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, HostB,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_subscription_lists, HostB,
			  ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:delete(roster_get_jid_info, HostB,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(resend_subscription_requests_hook, HostB,
		       ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:delete(webadmin_page_host, HostB,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, HostB,
			  ?MODULE, webadmin_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_ROSTER).


process_iq(From, To, IQ_Rec) ->
    LServer = exmpp_jid:prep_domain_as_list(From),
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    process_local_iq(From, To, IQ_Rec);
	_ ->
	    exmpp_iq:error(IQ_Rec, 'item-not-found')
    end.

process_local_iq(From, To, #iq{type = get} = IQ_Rec) ->
    process_iq_get(From, To, IQ_Rec);
process_local_iq(From, To, #iq{type = set} = IQ_Rec) ->
    process_iq_set(From, To, IQ_Rec).



process_iq_get(From, To, IQ_Rec) ->
    US = {exmpp_jid:lnode(From), exmpp_jid:prep_domain(From)},
    case catch ejabberd_hooks:run_fold(roster_get, exmpp_jid:prep_domain(To), [], [US]) of
	Items when is_list(Items) ->
	    XItems = lists:map(fun item_to_xml/1, Items),
	    Result = #xmlel{ns = ?NS_ROSTER, name = 'query',
	      children = XItems},
	    exmpp_iq:result(IQ_Rec, Result);
	_ ->
	    exmpp_iq:error(IQ_Rec, 'internal-server-error')
    end.

get_user_roster(Acc, {LUser, LServer}) ->
    Items = get_roster(LUser, LServer),
    lists:filter(fun(#roster{subscription = none, ask = in}) ->
			 false;
		    (_) ->
			 true
		 end, Items) ++ Acc.

get_roster(LUser, LServer) when is_binary(LUser), is_binary(LServer)->
    Username = ejabberd_odbc:escape(LUser),
    DomainString = binary_to_list(LServer),
    case catch odbc_queries:get_roster(DomainString, Username) of
	{selected, ["username", "jid", "nick", "subscription", "ask",
		    "askmessage", "server", "subscribe", "type"],
	 Items} when is_list(Items) ->
	    JIDGroups = case catch odbc_queries:get_roster_jid_groups(DomainString, Username) of
			    {selected, ["jid","grp"], JGrps}
			    when is_list(JGrps) ->
				[{list_to_binary(S), list_to_binary(G)} || {S, G} <- JGrps];
			    _ ->
				[]
			end,
	    RItems = lists:flatmap(
		       fun(I) ->
			       case raw_to_record(LServer, I) of
				   %% Bad JID in database:
				   error ->
				       [];
				   R ->
				       {U2, S2, R2} = R#roster.jid,
				       SJID = exmpp_jid:jid_to_binary(U2, S2, R2),
				       Groups = lists:flatmap(
						  fun({S, G}) when S == SJID ->
							  [G];
						     (_) ->
							  []
						  end, JIDGroups),
				       [R#roster{groups = Groups}]
			       end
		       end, Items),
	    RItems;
	_ ->
	    []
    end.


item_to_xml(Item) ->
    {U, S, R} = Item#roster.jid,
    Attrs1 = exmpp_xml:set_attribute_in_list([],
      'jid', exmpp_jid:jid_to_binary(U, S, R)),
    Attrs2 = case Item#roster.name of
		 <<>> ->
		     Attrs1;
		 Name ->
		     exmpp_xml:set_attribute_in_list(Attrs1, 'name', Name)
	     end,
    Attrs3 = exmpp_xml:set_attribute_in_list(Attrs2,
      'subscription', Item#roster.subscription),
    Attrs = case ask_to_pending(Item#roster.ask) of
		out ->
		    exmpp_xml:set_attribute_in_list(Attrs3,
		      'ask', <<"subscribe">>);
		both ->
		    exmpp_xml:set_attribute_in_list(Attrs3,
		      'ask', <<"subscribe">>);
		_ ->
		    Attrs3
	    end,
    SubEls = lists:map(fun(G) ->
				exmpp_xml:set_cdata(
				  #xmlel{ns = ?NS_ROSTER, name = 'group'}, G)
        	       end, Item#roster.groups),
    #xmlel{ns = ?NS_ROSTER, name = 'item', attrs = Attrs, children = SubEls}.


process_iq_set(From, To, #iq{payload = Request} = IQ_Rec) ->
    case Request of
	#xmlel{children = Els} ->
	    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els);
	_ ->
	    ok
    end,
    exmpp_iq:result(IQ_Rec).

process_item_set(From, To, #xmlel{} = El) ->
    try
	JID1 = exmpp_jid:parse(exmpp_xml:get_attribute_as_binary(El, 'jid', <<>>)),
    User = exmpp_jid:lnode(From),
    Server = exmpp_jid:prep_domain(From),
    LServer = binary_to_list(Server),
	{U0, S0, R0} = LJID = jlib:short_prepd_jid(JID1),
	Username = ejabberd_odbc:escape(User),
	SJID = ejabberd_odbc:escape(exmpp_jid:jid_to_binary(U0, S0, R0)),
	F = fun() ->
		    {selected,
		     ["username", "jid", "nick", "subscription",
		      "ask", "askmessage", "server", "subscribe", "type"],
		     Res} = odbc_queries:get_roster_by_jid(LServer, Username, SJID),
		    Item = case Res of
			       [] ->
				   #roster{usj = {User, Server, LJID},
					   us = {User, Server},
					   jid = LJID};
			       [I] ->
				   R = raw_to_record(exmpp_jid:prep_domain(From), I),
				   case R of
				       %% Bad JID in database:
				       error ->
					   #roster{usj = {User, Server, LJID},
						   us = {User, Server},
						   jid = LJID};
				       _ ->
					   R#roster{
					     usj = {User, Server, LJID},
					     us = {User, Server},
					     jid = LJID,
					     name = <<>>}
				   end
			   end,
		    Item1 = process_item_attrs(Item, El#xmlel.attrs),
		    Item2 = process_item_els(Item1, El#xmlel.children),
		    case Item2#roster.subscription of
			remove ->
			    send_unsubscribing_presence(From, Item),
			    ok;
			_ ->
			    ItemVals = record_to_string(Item2),
			    ItemGroups = groups_to_string(Item2),
			    odbc_queries:update_roster(LServer, Username, SJID, ItemVals, ItemGroups)
		    end,
		    %% If the item exist in shared roster, take the
		    %% subscription information from there:
		    Item3 = ejabberd_hooks:run_fold(roster_process_item,
						    exmpp_jid:prep_domain(From), Item2, [exmpp_jid:prep_domain(From)]),
		    {Item, Item3}
	    end,
	case odbc_queries:sql_transaction(LServer, F) of
	    {atomic, {OldItem, Item}} ->
		push_item(exmpp_jid:node(From), exmpp_jid:prep_domain(From), To, Item),
		case Item#roster.subscription of
		    remove ->
			IsTo = case OldItem#roster.subscription of
				   both -> true;
				   to -> true;
				   _ -> false
			       end,
			IsFrom = case OldItem#roster.subscription of
				     both -> true;
				     from -> true;
				     _ -> false
				 end,
			{U, S, R} = OldItem#roster.jid,
			if IsTo ->
				ejabberd_router:route(
				  exmpp_jid:bare(From),
				  exmpp_jid:make(U, S, R),
				  exmpp_presence:unsubscribe());
			   true -> ok
			end,
			if IsFrom ->
				ejabberd_router:route(
				  exmpp_jid:bare(From),
				  exmpp_jid:make(U, S, R),
				  exmpp_presence:unsubscribed());
			   true -> ok
			end,
			ok;
		    _ ->
			ok
		end;
	    E ->
		?DEBUG("ROSTER: roster item set error: ~p~n", [E]),
		ok
	end
    catch
	_ ->
	    ok
    end;
process_item_set(_From, _To, _) ->
    ok.

process_item_attrs(Item, [#xmlattr{name = Attr, value = Val} | Attrs]) ->
    case Attr of
	'name' ->
	    process_item_attrs(Item#roster{name = Val}, Attrs);
	'subscription' ->
	    case Val of
		<<"remove">> ->
		    process_item_attrs(Item#roster{subscription = remove},
				       Attrs);
		_ ->
		    process_item_attrs(Item, Attrs)
	    end;
	'ask' ->
	    process_item_attrs(Item, Attrs);
	_ ->
	    process_item_attrs(Item, Attrs)
    end;
process_item_attrs(Item, []) ->
    Item.


process_item_els(Item, [#xmlel{name = Name} = El | Els]) ->
    case Name of
	'group' ->
	    Groups = [exmpp_xml:get_cdata(El) | Item#roster.groups],
	    process_item_els(Item#roster{groups = Groups}, Els);
	_ ->
	    process_item_els(Item, Els)
    end;
process_item_els(Item, [_ | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) ->
    Item.


push_item(User, Server, From, Item) when is_binary(User), is_binary(Server) ->
    ejabberd_sm:route(exmpp_jid:make(),
		      exmpp_jid:make(User, Server),
		      #xmlel{name = 'broadcast', children =
		       [{item,
			 Item#roster.jid,
			 Item#roster.subscription}]}),
    lists:foreach(fun(Resource) ->
			  push_item(User, Server, Resource, From, Item)
		  end, ejabberd_sm:get_user_resources(User, Server)).

% TODO: don't push to those who not load roster
push_item(User, Server, Resource, From, Item) ->
    Request = #xmlel{ns = ?NS_ROSTER, name = 'query',
      children = [item_to_xml(Item)]},
    ResIQ = exmpp_iq:set(?NS_JABBER_CLIENT, Request,
      "push" ++ randoms:get_string()),
    ejabberd_router:route(
      From,
      exmpp_jid:make(User, Server, Resource),
      ResIQ).

get_subscription_lists(_, User, Server) 
        when is_binary(User), is_binary(Server) ->
    try
	LServer = binary_to_list(Server),
	Username = ejabberd_odbc:escape(User),
	case catch odbc_queries:get_roster(LServer, Username) of
	    {selected, ["username", "jid", "nick", "subscription", "ask",
			"askmessage", "server", "subscribe", "type"],
	     Items} when is_list(Items) ->
		fill_subscription_lists(Server, Items, [], []);
	    _ ->
		{[], []}
	end
    catch
	_ ->
	    {[], []}
    end.

fill_subscription_lists(LServer, [RawI | Is], F, T) ->
    I = raw_to_record(LServer, RawI),
    case I of
	%% Bad JID in database:
	error ->
	    fill_subscription_lists(LServer, Is, F, T);
	_ ->
	    J = I#roster.jid,
	    case I#roster.subscription of
		both ->
		    fill_subscription_lists(LServer, Is, [J | F], [J | T]);
		from ->
		    fill_subscription_lists(LServer, Is, [J | F], T);
		to ->
		    fill_subscription_lists(LServer, Is, F, [J | T]);
		_ ->
		    fill_subscription_lists(LServer, Is, F, T)
	    end
    end;
fill_subscription_lists(_LServer, [], F, T) ->
    {F, T}.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.



in_subscription(_, User, Server, JID, Type, Reason) ->
    process_subscription(in, User, Server, JID, Type, Reason).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, <<>>).

process_subscription(Direction, User, Server, JID1, Type, Reason)
        when is_binary(User), is_binary(Server) ->
    try
	LServer = binary_to_list(Server),
	{N0,D0,R0} = LJID = jlib:short_prepd_jid(JID1),
	Username = ejabberd_odbc:escape(User),
	SJID = ejabberd_odbc:escape(exmpp_jid:jid_to_binary(N0,D0,R0)),
	F = fun() ->
		    Item =
			case odbc_queries:get_roster_by_jid(LServer, Username, SJID) of
			    {selected,
			     ["username", "jid", "nick", "subscription", "ask",
			      "askmessage", "server", "subscribe", "type"],
			     [I]} ->
				%% raw_to_record can return error, but
				%% jlib_to_string would fail before this point
				R = raw_to_record(list_to_binary(LServer), I),
				Groups =
				    case odbc_queries:get_roster_groups(LServer, Username, SJID) of
					{selected, ["grp"], JGrps} when is_list(JGrps) ->
					    [list_to_binary(JGrp) || {JGrp} <- JGrps];
					_ ->
					    []
				    end,
				R#roster{groups = Groups};
			    {selected,
			     ["username", "jid", "nick", "subscription", "ask",
			      "askmessage", "server", "subscribe", "type"],
			     []} ->
				#roster{usj = {User, Server, LJID},
					us = {User, Server},
					jid = LJID}
			end,
		    NewState = case Direction of
				   out ->
				       out_state_change(Item#roster.subscription,
							Item#roster.ask,
							Type);
				   in ->
				       in_state_change(Item#roster.subscription,
						       Item#roster.ask,
						       Type)
			       end,
		    AutoReply = case Direction of
				    out ->
					none;
				    in ->
					in_auto_reply(Item#roster.subscription,
						      Item#roster.ask,
						      Type)
				end,
		    AskMessage = case NewState of
				     {_, both} -> Reason;
				     {_, in}   -> Reason;
				     _         -> <<>>
				 end,
		    case NewState of
			none ->
			    {none, AutoReply};
			{none, none} when Item#roster.subscription == none,
					  Item#roster.ask == in ->
			    odbc_queries:del_roster(LServer, Username, SJID),
			    {none, AutoReply};
			{Subscription, Pending} ->
			    AskBinary = case AskMessage of
					    undefined -> <<>>;
					    B  -> B
					end,
			    NewItem = Item#roster{subscription = Subscription,
						  ask = Pending,
						  askmessage = AskBinary},
			    ItemVals = record_to_string(NewItem),
			    odbc_queries:roster_subscribe(LServer, Username, SJID, ItemVals),
			    {{push, NewItem}, AutoReply}
		    end
	    end,
	case odbc_queries:sql_transaction(LServer, F) of
	    {atomic, {Push, AutoReply}} ->
		case AutoReply of
		    none ->
			ok;
		    _ ->
			ejabberd_router:route(
			  exmpp_jid:make(User, Server), JID1,
			  exmpp_presence:AutoReply())
		end,
		case Push of
		    {push, Item} ->
			if
			    Item#roster.subscription == none,
			    Item#roster.ask == in ->
				ok;
			    true ->
				push_item(User, Server,
					  exmpp_jid:make(User, Server), Item)
			end,
			true;
		    none ->
			false
		end;
	    _ ->
		false
	end
    catch
	_ ->
	    false
    end.


%% in_state_change(Subscription, Pending, Type) -> NewState
%% NewState = none | {NewSubscription, NewPending}
-ifdef(ROSTER_GATEWAY_WORKAROUND).
-define(NNSD, {to, none}).
-define(NISD, {to, in}).
-else.
-define(NNSD, none).
-define(NISD, none).
-endif.

in_state_change(none, none, subscribe)    -> {none, in};
in_state_change(none, none, subscribed)   -> ?NNSD;
in_state_change(none, none, unsubscribe)  -> none;
in_state_change(none, none, unsubscribed) -> none;
in_state_change(none, out,  subscribe)    -> {none, both};
in_state_change(none, out,  subscribed)   -> {to, none};
in_state_change(none, out,  unsubscribe)  -> none;
in_state_change(none, out,  unsubscribed) -> {none, none};
in_state_change(none, in,   subscribe)    -> none;
in_state_change(none, in,   subscribed)   -> ?NISD;
in_state_change(none, in,   unsubscribe)  -> {none, none};
in_state_change(none, in,   unsubscribed) -> none;
in_state_change(none, both, subscribe)    -> none;
in_state_change(none, both, subscribed)   -> {to, in};
in_state_change(none, both, unsubscribe)  -> {none, out};
in_state_change(none, both, unsubscribed) -> {none, in};
in_state_change(to,   none, subscribe)    -> {to, in};
in_state_change(to,   none, subscribed)   -> none;
in_state_change(to,   none, unsubscribe)  -> none;
in_state_change(to,   none, unsubscribed) -> {none, none};
in_state_change(to,   in,   subscribe)    -> none;
in_state_change(to,   in,   subscribed)   -> none;
in_state_change(to,   in,   unsubscribe)  -> {to, none};
in_state_change(to,   in,   unsubscribed) -> {none, in};
in_state_change(from, none, subscribe)    -> none;
in_state_change(from, none, subscribed)   -> none;
in_state_change(from, none, unsubscribe)  -> {none, none};
in_state_change(from, none, unsubscribed) -> none;
in_state_change(from, out,  subscribe)    -> none;
in_state_change(from, out,  subscribed)   -> {both, none};
in_state_change(from, out,  unsubscribe)  -> {none, out};
in_state_change(from, out,  unsubscribed) -> {from, none};
in_state_change(both, none, subscribe)    -> none;
in_state_change(both, none, subscribed)   -> none;
in_state_change(both, none, unsubscribe)  -> {to, none};
in_state_change(both, none, unsubscribed) -> {from, none}.

out_state_change(none, none, subscribe)    -> {none, out};
out_state_change(none, none, subscribed)   -> none;
out_state_change(none, none, unsubscribe)  -> none;
out_state_change(none, none, unsubscribed) -> none;
out_state_change(none, out,  subscribe)    -> {none, out}; %% We need to resend query (RFC3921, section 9.2)
out_state_change(none, out,  subscribed)   -> none;
out_state_change(none, out,  unsubscribe)  -> {none, none};
out_state_change(none, out,  unsubscribed) -> none;
out_state_change(none, in,   subscribe)    -> {none, both};
out_state_change(none, in,   subscribed)   -> {from, none};
out_state_change(none, in,   unsubscribe)  -> none;
out_state_change(none, in,   unsubscribed) -> {none, none};
out_state_change(none, both, subscribe)    -> none;
out_state_change(none, both, subscribed)   -> {from, out};
out_state_change(none, both, unsubscribe)  -> {none, in};
out_state_change(none, both, unsubscribed) -> {none, out};
out_state_change(to,   none, subscribe)    -> none;
out_state_change(to,   none, subscribed)   -> none;
out_state_change(to,   none, unsubscribe)  -> {none, none};
out_state_change(to,   none, unsubscribed) -> none;
out_state_change(to,   in,   subscribe)    -> none;
out_state_change(to,   in,   subscribed)   -> {both, none};
out_state_change(to,   in,   unsubscribe)  -> {none, in};
out_state_change(to,   in,   unsubscribed) -> {to, none};
out_state_change(from, none, subscribe)    -> {from, out};
out_state_change(from, none, subscribed)   -> none;
out_state_change(from, none, unsubscribe)  -> none;
out_state_change(from, none, unsubscribed) -> {none, none};
out_state_change(from, out,  subscribe)    -> none;
out_state_change(from, out,  subscribed)   -> none;
out_state_change(from, out,  unsubscribe)  -> {from, none};
out_state_change(from, out,  unsubscribed) -> {none, out};
out_state_change(both, none, subscribe)    -> none;
out_state_change(both, none, subscribed)   -> none;
out_state_change(both, none, unsubscribe)  -> {from, none};
out_state_change(both, none, unsubscribed) -> {to, none}.

in_auto_reply(from, none, subscribe)    -> subscribed;
in_auto_reply(from, out,  subscribe)    -> subscribed;
in_auto_reply(both, none, subscribe)    -> subscribed;
in_auto_reply(none, in,   unsubscribe)  -> unsubscribed;
in_auto_reply(none, both, unsubscribe)  -> unsubscribed;
in_auto_reply(to,   in,   unsubscribe)  -> unsubscribed;
in_auto_reply(from, none, unsubscribe)  -> unsubscribed;
in_auto_reply(from, out,  unsubscribe)  -> unsubscribed;
in_auto_reply(both, none, unsubscribe)  -> unsubscribed;
in_auto_reply(_,    _,    _)  ->           none.


remove_user(User, Server) when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = binary_to_list(exmpp_stringprep:nameprep(Server)),
	Username = ejabberd_odbc:escape(LUser),
        send_unsubscription_to_rosteritems(LUser, LServer),
	odbc_queries:del_user_roster_t(LServer, Username),
	ok
    catch
	_ ->
	    ok
    end.

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
send_unsubscription_to_rosteritems(LUser, LServer) ->
    RosterItems = get_user_roster([], {LUser, LServer}),
    From = jlib:make_jid({LUser, LServer, ""}),
    lists:foreach(fun(RosterItem) ->
			  send_unsubscribing_presence(From, RosterItem)
		  end,
		  RosterItems).

%% @spec (From::jid(), Item::roster()) -> ok
send_unsubscribing_presence(From, Item) ->
    IsTo = case Item#roster.subscription of
	       both -> true;
	       to -> true;
	       _ -> false
	   end,
    IsFrom = case Item#roster.subscription of
		 both -> true;
		 from -> true;
		 _ -> false
	     end,
    if IsTo ->
	    send_presence_type(
	      jlib:jid_remove_resource(From),
	      jlib:make_jid(Item#roster.jid), "unsubscribe");
       true -> ok
    end,
    if IsFrom ->
	    send_presence_type(
	      jlib:jid_remove_resource(From),
	      jlib:make_jid(Item#roster.jid), "unsubscribed");
       true -> ok
    end,
    ok.

send_presence_type(From, To, Type) ->
    ejabberd_router:route(
      From, To,
      {xmlelement, "presence",
       [{"type", Type}],
       []}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_items(User, Server, #xmlel{children = Els}) when is_binary(User), is_binary(Server) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    catch odbc_queries:sql_transaction(
	    LServer,
	    lists:flatmap(fun(El) ->
				  process_item_set_t(LUser, LServer, El)
			  end, Els)).

process_item_set_t(LUser, LServer, #xmlel{} = El) ->
    try
	JID1 = exmpp_jid:parse(exmpp_xml:get_attribute_as_binary(El, 'jid', <<>>)),
	{U0, S0, R0} = LJID = jlib:short_prepd_jid(JID1),
	Username = ejabberd_odbc:escape(LUser),
	SJID = ejabberd_odbc:escape(exmpp_jid:jid_to_binary(U0, S0, R0)),
	Item = #roster{usj = {LUser, LServer, LJID},
		       us = {LUser, LServer},
		       jid = LJID},
	Item1 = process_item_attrs_ws(Item, El#xmlel.attrs),
	Item2 = process_item_els(Item1, El#xmlel.children),
	case Item2#roster.subscription of
	    remove ->
		odbc_queries:del_roster_sql(Username, SJID);
	    _ ->
		ItemVals = record_to_string(Item1),
		ItemGroups = groups_to_string(Item2),
		odbc_queries:update_roster_sql(Username, SJID, ItemVals, ItemGroups)
	end
    catch
	_ ->
	    []
    end;
process_item_set_t(_LUser, _LServer, _) ->
    [].

process_item_attrs_ws(Item, [#xmlattr{name = Attr, value = Val} | Attrs]) ->
    case Attr of
	'name' ->
	    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
	'subscription' ->
	    case Val of
		<<"remove">> ->
		    process_item_attrs_ws(Item#roster{subscription = remove},
					  Attrs);
		<<"none">> ->
		    process_item_attrs_ws(Item#roster{subscription = none},
					  Attrs);
		<<"both">> ->
		    process_item_attrs_ws(Item#roster{subscription = both},
					  Attrs);
		<<"from">> ->
		    process_item_attrs_ws(Item#roster{subscription = from},
					  Attrs);
		<<"to">> ->
		    process_item_attrs_ws(Item#roster{subscription = to},
					  Attrs);
		_ ->
		    process_item_attrs_ws(Item, Attrs)
	    end;
	'ask' ->
	    process_item_attrs_ws(Item, Attrs);
	_ ->
	    process_item_attrs_ws(Item, Attrs)
    end;
process_item_attrs_ws(Item, []) ->
    Item.

get_in_pending_subscriptions(Ls, User, Server) 
        when is_binary(User), is_binary(Server) ->
    JID = exmpp_jid:make(User, Server),
    LUser = exmpp_jid:lnode(JID),
    LServer = exmpp_jid:prep_domain_as_list(JID),
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_roster(LServer, Username) of
	{selected, ["username", "jid", "nick", "subscription", "ask",
		    "askmessage", "server", "subscribe", "type"],
	 Items} when is_list(Items) ->
    	    Ls ++ lists:map(
		    fun(R) ->
			    Message = R#roster.askmessage,
			    {U0, S0, R0} = R#roster.jid,
			    Pres1 = exmpp_presence:subscribe(),
			    Pres2 = exmpp_stanza:set_jids(Pres1,
			      exmpp_jid:jid_to_binary(U0, S0, R0),
			      exmpp_jid:jid_to_binary(JID)),
			    exmpp_presence:set_status(Pres2, Message)
		    end,
		    lists:flatmap(
		      fun(I) ->
			      case raw_to_record(exmpp_jid:prep_domain(JID), I) of
				  %% Bad JID in database:
				  error ->
				      [];
				  R ->
				      case R#roster.ask of
					  in   -> [R];
					  both -> [R];
					  _ -> []
				      end
			      end
		      end,
		      Items));
	_ ->
	    Ls
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% JID is  jid() record, because it's used latter on for both short_prepd_jid 
%% and short_prepd_bare_jid
get_jid_info(_, User, Server, JID) when is_binary(User), is_binary(Server) ->
    try
	LServer = binary_to_list(Server),
	LJID = {N, D, R} = jlib:short_prepd_jid(JID),
	Username = ejabberd_odbc:escape(User),
	SJID = ejabberd_odbc:escape(exmpp_jid:jid_to_binary(N, D, R)),
	case catch odbc_queries:get_subscription(LServer, Username, SJID) of
	    {selected, ["subscription"], [{SSubscription}]} ->
		Subscription = case SSubscription of
				   "B" -> both;
				   "T" -> to;
				   "F" -> from;
				   _ -> none
			       end,
		Groups = case catch odbc_queries:get_rostergroup_by_jid(LServer, Username, SJID) of
			     {selected, ["grp"], JGrps} when is_list(JGrps) ->
				 [list_to_binary(JGrp) || {JGrp} <- JGrps];
			     _ ->
				 []
			 end,
		{Subscription, Groups};
	    _ ->
		LRJID = jlib:short_prepd_bare_jid(JID),
		if
		    LRJID == LJID ->
			{none, []};
		    true ->
			{LR_N, LR_D, LR_R} = LRJID,
			SRJID = ejabberd_odbc:escape(exmpp_jid:jid_to_binary(LR_N, LR_D, LR_R)),
			case catch odbc_queries:get_subscription(LServer, Username, SRJID) of
			    {selected, ["subscription"], [{SSubscription}]} ->
				Subscription = case SSubscription of
						   "B" -> both;
						   "T" -> to;
						   "F" -> from;
						   _ -> none
					       end,
				Groups = case catch odbc_queries:get_rostergroup_by_jid(LServer, Username, SRJID) of
					     {selected, ["grp"], JGrps} when is_list(JGrps) ->
						 [list_to_binary(JGrp) || {JGrp} <- JGrps];
					     _ ->
						 []
					 end,
				{Subscription, Groups};
			    _ ->
				{none, []}
			end
		end
	end
    catch
	_ ->
	    {none, []}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

raw_to_record(LServer, {User, SJID, Nick, SSubscription, SAsk, SAskMessage,
			_SServer, _SSubscribe, _SType}) when is_binary(LServer) ->
    try
	JID = exmpp_jid:parse(SJID),
	LJID = jlib:short_prepd_jid(JID),
	Subscription = case SSubscription of
			   "B" -> both;
			   "T" -> to;
			   "F" -> from;
			   _ -> none
		       end,
	Ask = case SAsk of
		  "S" -> subscribe;
		  "U" -> unsubscribe;
		  "B" -> both;
		  "O" -> out;
		  "I" -> in;
		  _ -> none
	      end,
    UserB = list_to_binary(User),
	#roster{usj = {UserB, LServer, LJID},
		us = {UserB, LServer},
		jid = LJID,
		name =  list_to_binary(Nick),
		subscription = Subscription,
		ask = Ask,
		askmessage = list_to_binary(SAskMessage)}
    catch
	_ ->
	    error
    end.

record_to_string(#roster{us = {User, _Server},
			 jid = JID,
			 name = Name,
			 subscription = Subscription,
			 ask = Ask,
			 askmessage = AskMessage}) ->
    Username = ejabberd_odbc:escape(User),
    {U, S, R} = JID,
    SJID = ejabberd_odbc:escape(exmpp_jid:jid_to_binary(U, S, R)),
    Nick = ejabberd_odbc:escape(Name),
    SSubscription = case Subscription of
			both -> "B";
			to   -> "T";
			from -> "F";
			none -> "N"
		    end,
    SAsk = case Ask of
	       subscribe   -> "S";
	       unsubscribe -> "U";
	       both	   -> "B";
	       out	   -> "O";
	       in	   -> "I";
	       none	   -> "N"
	   end,
    SAskMessage = ejabberd_odbc:escape(AskMessage),
    [Username, SJID, Nick, SSubscription, SAsk, SAskMessage, "N", "", "item"].

groups_to_string(#roster{us = {User, _Server},
			 jid = JID,
			 groups = Groups}) ->
    Username = ejabberd_odbc:escape(User),
    {U, S, R} = JID,
    SJID = ejabberd_odbc:escape(exmpp_jid:jid_to_binary(U, S, R)),

    %% Empty groups do not need to be converted to string to be inserted in
    %% the database
    lists:foldl(
      fun([], Acc) -> Acc;
	 (Group, Acc) ->
 	      G = ejabberd_odbc:escape(Group),
	      [[Username, SJID, G]|Acc] end, [], Groups).

webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "roster"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = user_roster(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_roster(User, Server, Query, Lang) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	Items1 = get_roster(LUser, LServer),
	Res = user_roster_parse_query(User, Server, Items1, Query),
	Items = get_roster(LUser, LServer),
	SItems = lists:sort(Items),
	FItems =
	    case SItems of
		[] ->
		    [?CT("None")];
		_ ->
		    [?XE("table",
			 [?XE("thead",
			      [?XE("tr",
				   [?XCT("td", "Jabber ID"),
				    ?XCT("td", "Nickname"),
				    ?XCT("td", "Subscription"),
				    ?XCT("td", "Pending"),
				    ?XCT("td", "Groups")
				   ])]),
			  ?XE("tbody",
			      lists:map(
				fun(R) ->
					Groups =
					    lists:flatmap(
					      fun(Group) ->
						      [?C(Group), ?BR]
					      end, R#roster.groups),
					Pending = ask_to_pending(R#roster.ask),
					TDJID = build_contact_jid_td(R#roster.jid),
					?XE("tr",
					    [TDJID,
					     ?XAC("td", [?XMLATTR('class', <<"valign">>)],
						  binary_to_list(R#roster.name)),
					     ?XAC("td", [?XMLATTR('class', <<"valign">>)],
						  atom_to_list(R#roster.subscription)),
					     ?XAC("td", [?XMLATTR('class', <<"valign">>)],
						  atom_to_list(Pending)),
					     ?XAE("td", [?XMLATTR('class', <<"valign">>)], Groups),
					     if
						 Pending == in ->
						     ?XAE("td", [?XMLATTR('class', <<"valign">>)],
							  [?INPUTT("submit",
								   "validate" ++
								   ejabberd_web_admin:term_to_id(R#roster.jid),
								   "Validate")]);
						 true ->
						     ?X("td")
					     end,
					     ?XAE("td", [?XMLATTR('class', <<"valign">>)],
						  [?INPUTT("submit",
							   "remove" ++
							   ejabberd_web_admin:term_to_id(R#roster.jid),
							   "Remove")])])
				end, SItems))])]
	    end,
	[?XC("h1", ?T("Roster of ") ++ us_to_list(US))] ++
	    case Res of
		ok -> [?XREST("Submitted")];
		error -> [?XREST("Bad format")];
		nothing -> []
	    end ++
	    [?XAE("form", [?XMLATTR('action', <<"">>), ?XMLATTR('method', <<"post">>)],
		  FItems ++
		  [?P,
		   ?INPUT("text", "newjid", ""), ?C(" "),
		   ?INPUTT("submit", "addjid", "Add Jabber ID")
		  ])]
      catch
	  _ ->
	      [?XC("h1", ?T("Roster of ") ++ us_to_list({User, Server}))] ++
	      [?CT("Bad format"), ?P] ++
	      [?XAE("form", [?XMLATTR('action', <<"">>), ?XMLATTR('method', <<"post">>)],
		    [?P,
		     ?INPUT("text", "newjid", ""), ?C(" "),
		     ?INPUTT("submit", "addjid", "Add Jabber ID")
		    ])]
      end.

build_contact_jid_td({U, S, R}) ->
    %% Convert {U, S, R} into {jid, U, S, R, U, S, R}:
    ContactJID = exmpp_jid:make(U, S, R),
    JIDURI = case {exmpp_jid:lnode(ContactJID), exmpp_jid:prep_domain(ContactJID)} of
		 {undefined, _} -> "";
		 {CUser, CServer} ->
		     CUser_S = binary_to_list(CUser),
		     CServer_S = binary_to_list(CServer),
		     case lists:member(CServer_S, ?MYHOSTS) of
			 false -> "";
			 true -> "/admin/server/" ++ CServer_S ++ "/user/" ++ CUser_S ++ "/"
		     end
	     end,
    case JIDURI of
	[] ->
	    ?XAC('td', [?XMLATTR('class', <<"valign">>)], exmpp_jid:jid_to_list(ContactJID));
	URI when is_list(URI) ->
	    ?XAE('td', [?XMLATTR('class', <<"valign">>)], [?AC(JIDURI, exmpp_jid:jid_to_list(ContactJID))])
    end.

user_roster_parse_query(User, Server, Items, Query) ->
    case lists:keysearch("addjid", 1, Query) of
	{value, _} ->
	    case lists:keysearch("newjid", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, SJID}} ->
		    try
			JID = exmpp_jid:parse(SJID),
			user_roster_subscribe_jid(User, Server, JID),
			ok
		    catch
			_ ->
			    error
		    end;
		false ->
		    error
	    end;
	false ->
	    case catch user_roster_item_parse_query(
			 User, Server, Items, Query) of
		submitted ->
		    ok;
		{'EXIT', _Reason} ->
		    error;
		_ ->
		    nothing
	    end
    end.


user_roster_subscribe_jid(User, Server, JID) ->
    out_subscription(User, Server, JID, subscribe),
    UJID = exmpp_jid:make(User, Server),
    ejabberd_router:route(
      UJID, JID, exmpp_presence:subscribe()).

user_roster_item_parse_query(User, Server, Items, Query) ->
    lists:foreach(
      fun(R) ->
	      JID = R#roster.jid,
	      case lists:keysearch(
		     "validate" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
		  {value, _} ->
		      {U, S, R} = JID,
		      JID1 = exmpp_jid:make(U, S, R),
		      out_subscription(
			User, Server, JID1, subscribed),
		      UJID = exmpp_jid:make(User, Server),
		      ejabberd_router:route(
			UJID, JID1, exmpp_presence:subscribed()),
		      throw(submitted);
		  false ->
		      case lists:keysearch(
			     "remove" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
			  {value, _} ->
			      UJID = exmpp_jid:make(User, Server),
			      Attrs1 = exmpp_xml:set_attribute_in_list([],
				'jid', exmpp_jid:jid_to_list(JID)),
			      Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1,
				'subscription', "remove"),
			      Item = #xmlel{ns = ?NS_ROSTER, name = 'item',
				attrs = Attrs2},
			      Request = #xmlel{
				ns = ?NS_ROSTER,
				name = 'query',
				children = [Item]},
			      process_iq(
				UJID, UJID,
				exmpp_iq:set(?NS_JABBER_CLIENT, Request)),
			      throw(submitted);
			  false ->
			      ok
		      end

	      end
      end, Items),
    nothing.

us_to_list({User, Server}) ->
    exmpp_jid:bare_jid_to_list(User, Server).

webadmin_user(Acc, _User, _Server, Lang) ->
    Acc ++ [?XE("h3", [?ACT("roster/", "Roster")])].
