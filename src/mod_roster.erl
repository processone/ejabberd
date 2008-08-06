%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
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

-module(mod_roster).
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
	 item_to_xml/1,
	 webadmin_page/3,
	 webadmin_user/4]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").


start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{attributes, record_info(fields, roster)}]),
    update_table(),
    mnesia:add_table_index(roster, us),
    ejabberd_hooks:add(roster_get, Host,
		       ?MODULE, get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, Host,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
		       ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:add(roster_get_jid_info, Host,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(resend_subscription_requests_hook, Host,
		       ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    % XXX OLD FORMAT: NS as string.
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, atom_to_list(?NS_ROSTER),
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(roster_get, Host,
			  ?MODULE, get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, Host,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_subscription_lists, Host,
			  ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(resend_subscription_requests_hook, Host,
			  ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host,
			  ?MODULE, webadmin_user, 50),
    % XXX OLD FORMAT: NS as string.
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     atom_to_list(?NS_ROSTER)).


process_iq(From, To, IQ) ->
    #jid{ldomain = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    process_local_iq(From, To, IQ);
	_ ->
	    exmpp_iq:error(IQ, 'item-not-found')
    end.

process_local_iq(From, To, IQ) ->
    case exmpp_iq:get_type(IQ) of
	set ->
	    process_iq_set(From, To, IQ);
	get ->
	    process_iq_get(From, To, IQ)
    end.



process_iq_get(From, To, IQ) ->
    LUser = From#jid.lnode,
    LServer = From#jid.ldomain,
    US = {LUser, LServer},
    case catch ejabberd_hooks:run_fold(roster_get, To#jid.ldomain, [], [US]) of
	Items when is_list(Items) ->
	    XItems = lists:map(fun item_to_xml/1, Items),
	    exmpp_iq:result(IQ, #xmlel{ns = ?NS_ROSTER, name = 'query',
		children = XItems});
	_ ->
	    exmpp_iq:error(IQ, 'internal-server-error')
    end.

get_user_roster(Acc, US) ->
    case catch mnesia:dirty_index_read(roster, US, #roster.us) of
	Items when is_list(Items) ->
	    lists:filter(fun(#roster{subscription = none, ask = in}) ->
				 false;
			    (_) ->
				 true
			 end, Items) ++ Acc;
	_ ->
	    Acc
    end.


item_to_xml(Item) ->
    {U, S, R} = Item#roster.jid,
    Attrs1 = exmpp_xml:set_attribute_in_list([],
      'jid', exmpp_jid:jid_to_list(U, S, R)),
    Attrs2 = case Item#roster.name of
		 "" ->
		     Attrs1;
		 Name ->
		     exmpp_xml:set_attribute_in_list(Attrs1, 'name', Name)
	     end,
    Attrs3 = exmpp_xml:set_attribute_in_list(Attrs2,
      'subscription', Item#roster.subscription),
    Attrs4 = case ask_to_pending(Item#roster.ask) of
		 out ->
		     exmpp_xml:set_attribute_in_list(Attrs3,
		       'ask', "subscribe");
		 both ->
		     exmpp_xml:set_attribute_in_list(Attrs3,
		       'ask', "subscribe");
		 _ ->
		     Attrs3
	     end,
    SubEls1 = lists:map(fun(G) ->
				exmpp_xml:set_cdata(
				  #xmlel{ns = ?NS_ROSTER, name = 'group'}, G)
			end, Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    #xmlel{ns = ?NS_ROSTER, name = 'item', attrs = Attrs4, children = SubEls}.


process_iq_set(From, To, IQ) ->
    case exmpp_iq:get_request(IQ) of
	#xmlel{children = Els} ->
	    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els);
	_ ->
	    ok
    end,
    exmpp_iq:result(IQ).

process_item_set(From, To, #xmlel{} = Item) ->
    try
	JID1 = exmpp_jid:list_to_jid(exmpp_xml:get_attribute(Item, 'jid', "")),
	% XXX OLD FORMAT: old JID (with empty strings).
	#jid{node = User, lnode = LUser, ldomain = LServer} =
	  jlib:to_old_jid(From),
	JID = {JID1#jid.node, JID1#jid.domain, JID1#jid.resource},
	LJID = jlib:short_jid(JID1),
	F = fun() ->
		    Res = mnesia:read({roster, {LUser, LServer, LJID}}),
		    Item = case Res of
			       [] ->
				   #roster{usj = {LUser, LServer, LJID},
					   us = {LUser, LServer},
					   jid = JID};
			       [I] ->
				   I#roster{jid = JID,
					    name = "",
					    groups = [],
					    xs = []}
			   end,
		    Item1 = process_item_attrs(Item, Item#xmlel.attrs),
		    Item2 = process_item_els(Item1, Item#xmlel.children),
		    case Item2#roster.subscription of
			remove ->
			    mnesia:delete({roster, {LUser, LServer, LJID}});
			_ ->
			    mnesia:write(Item2)
		    end,
		    %% If the item exist in shared roster, take the
		    %% subscription information from there:
		    Item3 = ejabberd_hooks:run_fold(roster_process_item,
						    LServer, Item2, [LServer]),
		    {Item, Item3}
	    end,
	case mnesia:transaction(F) of
	    {atomic, {OldItem, Item}} ->
		push_item(User, LServer, To, Item),
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
				  exmpp_jid:jid_to_bare_jid(From),
				  exmpp_jid:make_jid(U, S, R),
				  exmpp_presence:unsubscribe());
			   true -> ok
			end,
			if IsFrom ->
				ejabberd_router:route(
				  exmpp_jid:jid_to_bare_jid(From),
				  exmpp_jid:make_jid(U, S, R),
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
	'jid' ->
	    try
		JID1 = exmpp_jid:list_to_jid(Val),
		JID = {JID1#jid.node, JID1#jid.domain, JID1#jid.resource},
		process_item_attrs(Item#roster{jid = JID}, Attrs)
	    catch
		_ ->
		    process_item_attrs(Item, Attrs)
	    end;
	'name' ->
	    process_item_attrs(Item#roster{name = Val}, Attrs);
	'subscription' ->
	    case Val of
		"remove" ->
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


process_item_els(Item, [#xmlel{ns = NS, name = Name} = El | Els]) ->
    case Name of
	'group' ->
	    Groups = [exmpp_xml:get_cdata(El) | Item#roster.groups],
	    process_item_els(Item#roster{groups = Groups}, Els);
	_ ->
	    if
		NS == ?NS_JABBER_CLIENT; NS == ?NS_JABBER_SERVER ->
		    process_item_els(Item, Els);
		true ->
		    XEls = [El | Item#roster.xs],
		    process_item_els(Item#roster{xs = XEls}, Els)
	    end
    end;
process_item_els(Item, [_ | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) ->
    Item.


push_item(User, Server, From, Item) ->
    ejabberd_sm:route(exmpp_jid:make_bare_jid(""),
		      exmpp_jid:make_bare_jid(User, Server),
		      #xmlel{name = 'broadcast', children =
		       [{item,
			 Item#roster.jid,
			 Item#roster.subscription}]}),
    lists:foreach(fun(Resource) ->
			  push_item(User, Server, Resource, From, Item)
		  end, ejabberd_sm:get_user_resources(User, Server)).

% TODO: don't push to those who didn't load roster
push_item(User, Server, Resource, From, Item) ->
    Request = #xmlel{ns = ?NS_ROSTER, name = 'query',
      children = item_to_xml(Item)},
    ResIQ = exmpp_iq:set(?NS_JABBER_CLIENT, Request, "push"),
    ejabberd_router:route(
      From,
      exmpp_jid:make_jid(User, Server, Resource),
      ResIQ).

get_subscription_lists(_, User, Server) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    case mnesia:dirty_index_read(roster, US, #roster.us) of
	Items when is_list(Items) ->
	    fill_subscription_lists(Items, [], []);
	_ ->
	    {[], []}
    end.

fill_subscription_lists([I | Is], F, T) ->
    J = element(3, I#roster.usj),
    case I#roster.subscription of
	both ->
	    fill_subscription_lists(Is, [J | F], [J | T]);
	from ->
	    fill_subscription_lists(Is, [J | F], T);
	to ->
	    fill_subscription_lists(Is, F, [J | T]);
	_ ->
	    fill_subscription_lists(Is, F, T)
    end;
fill_subscription_lists([], F, T) ->
    {F, T}.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.



in_subscription(_, User, Server, JID, Type, Reason) ->
    process_subscription(in, User, Server, JID, Type, Reason).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, []).

process_subscription(Direction, User, Server, JID1, Type, Reason) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    LJID = jlib:short_jid(JID1),
    F = fun() ->
		Item = case mnesia:read({roster, {LUser, LServer, LJID}}) of
			   [] ->
			       JID = {JID1#jid.node,
				      JID1#jid.domain,
				      JID1#jid.resource},
			       #roster{usj = {LUser, LServer, LJID},
				       us = US,
				       jid = JID};
			   [I] ->
			       I
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
				 _         -> ""
			     end,
		case NewState of
		    none ->
			{none, AutoReply};
		    {none, none} when Item#roster.subscription == none,
		                      Item#roster.ask == in ->
			mnesia:delete({roster, {LUser, LServer, LJID}}),
			{none, AutoReply};
		    {Subscription, Pending} ->
			NewItem = Item#roster{subscription = Subscription,
					      ask = Pending,
					      askmessage = list_to_binary(AskMessage)},
			mnesia:write(NewItem),
			{{push, NewItem}, AutoReply}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {Push, AutoReply}} ->
	    case AutoReply of
		none ->
		    ok;
		_ ->
		    ejabberd_router:route(
		      exmpp_jid:make_bare_jid(User, Server), JID1,
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
				      exmpp_jid:make_bare_jid(User, Server), Item)
		    end,
		    true;
		none ->
		    false
	    end;
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


remove_user(User, Server) ->
    LUser = exmpp_stringpre:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		lists:foreach(fun(R) ->
				      mnesia:delete_object(R)
			      end,
			      mnesia:index_read(roster, US, #roster.us))
        end,
    mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_items(User, Server, SubEl) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    F = fun() ->
		lists:foreach(fun(El) ->
				      process_item_set_t(LUser, LServer, El)
			      end, Els)
	end,
    mnesia:transaction(F).

process_item_set_t(LUser, LServer, #xmlel{} = El) ->
    try
	JID1 = exmpp_jid:list_to_jid(exmpp_xml:get_attribute(El, 'jid', "")),
	JID = {JID1#jid.node, JID1#jid.domain, JID1#jid.resource},
	LJID = {JID1#jid.lnode, JID1#jid.ldomain, JID1#jid.lresource},
	Item = #roster{usj = {LUser, LServer, LJID},
		       us = {LUser, LServer},
		       jid = JID},
	Item1 = process_item_attrs_ws(Item, El#xmlel.attrs),
	Item2 = process_item_els(Item1, El#xmlel.children),
	case Item2#roster.subscription of
	    remove ->
		mnesia:delete({roster, {LUser, LServer, LJID}});
	    _ ->
		mnesia:write(Item2)
	end
    catch
	_ ->
	    ok
    end;
process_item_set_t(_LUser, _LServer, _) ->
    ok.

process_item_attrs_ws(Item, [#xmlattr{name = Attr, value = Val} | Attrs]) ->
    case Attr of
	'jid' ->
	    try
		JID1 = exmpp_jid:list_to_jid(Val),
		JID = {JID1#jid.node, JID1#jid.domain, JID1#jid.resource},
		process_item_attrs_ws(Item#roster{jid = JID}, Attrs)
	    catch
		_ ->
		    process_item_attrs_ws(Item, Attrs)
	    end;
	'name' ->
	    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
	'subscription' ->
	    case Val of
		"remove" ->
		    process_item_attrs_ws(Item#roster{subscription = remove},
					  Attrs);
		"none" ->
		    process_item_attrs_ws(Item#roster{subscription = none},
					  Attrs);
		"both" ->
		    process_item_attrs_ws(Item#roster{subscription = both},
					  Attrs);
		"from" ->
		    process_item_attrs_ws(Item#roster{subscription = from},
					  Attrs);
		"to" ->
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

get_in_pending_subscriptions(Ls, User, Server) ->
    JID = exmpp_jid:make_bare_jid(User, Server),
    US = {JID#jid.lnode, JID#jid.ldomain},
    case mnesia:dirty_index_read(roster, US, #roster.us) of
	Result when list(Result) ->
    	    Ls ++ lists:map(
		    fun(R) ->
			    Message = R#roster.askmessage,
			    Status  = if is_binary(Message) ->
					      binary_to_list(Message);
					 true ->
					      ""
				      end,
			    {U, S, R} = R#roster.jid,
			    Attrs1 = exmpp_stanza:set_sender_in_list([],
			      exmpp_jid:jid_to_list(U, S, R)),
			    Attrs2 = exmpp_stanza:set_recipient_in_list(Attrs1,
			      exmpp_jid:jid_to_list(JID)),
			    Pres1 = exmpp_presence:subscribe(),
			    Pres2 = Pres1#xmlel{attrs = Attrs2},
			    exmpp_presence:set_status(Pres2, Status)
		    end,
		    lists:filter(
		      fun(R) ->
			      case R#roster.ask of
				  in   -> true;
				  both -> true;
				  _ -> false
			      end
		      end,
		      Result));
	_ ->
	    Ls
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_jid_info(_, User, Server, JID) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LJID = jlib:short_jid(JID),
    LServer = exmpp_stringprep:nameprep(Server),
    case catch mnesia:dirty_read(roster, {LUser, LServer, LJID}) of
	[#roster{subscription = Subscription, groups = Groups}] ->
	    {Subscription, Groups};
	_ ->
	    LRJID = jlib:short_jid(exmpp_jid:jid_to_bare_jid(JID)),
	    if
		LRJID == LJID ->
		    {none, []};
		true ->
		    case catch mnesia:dirty_read(
				 roster, {LUser, LServer, LRJID}) of
			[#roster{subscription = Subscription,
				 groups = Groups}] ->
			    {Subscription, Groups};
			_ ->
			    {none, []}
		    end
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


update_table() ->
    Fields = record_info(fields, roster),
    case mnesia:table_info(roster, attributes) of
	Fields ->
	    ok;
	[uj, user, jid, name, subscription, ask, groups, xattrs, xs] ->
	    convert_table1(Fields);
	[usj, us, jid, name, subscription, ask, groups, xattrs, xs] ->
	    convert_table2(Fields);
	_ ->
	    ?INFO_MSG("Recreating roster table", []),
	    mnesia:transform_table(roster, ignore, Fields)
    end.


%% Convert roster table to support virtual host
convert_table1(Fields) ->
    ?INFO_MSG("Virtual host support: converting roster table from "
	      "{uj, user, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    Host = ?MYNAME,
    {atomic, ok} = mnesia:create_table(
		     mod_roster_tmp_table,
		     [{disc_only_copies, [node()]},
		      {type, bag},
		      {local_content, true},
		      {record_name, roster},
		      {attributes, record_info(fields, roster)}]),
    mnesia:del_table_index(roster, user),
    mnesia:transform_table(roster, ignore, Fields),
    F1 = fun() ->
		 mnesia:write_lock_table(mod_roster_tmp_table),
		 mnesia:foldl(
		   fun(#roster{usj = {U, JID}, us = U} = R, _) ->
			   mnesia:dirty_write(
			     mod_roster_tmp_table,
			     R#roster{usj = {U, Host, JID},
				      us = {U, Host}})
		   end, ok, roster)
	 end,
    mnesia:transaction(F1),
    mnesia:clear_table(roster),
    F2 = fun() ->
		 mnesia:write_lock_table(roster),
		 mnesia:foldl(
		   fun(R, _) ->
			   mnesia:dirty_write(R)
		   end, ok, mod_roster_tmp_table)
	 end,
    mnesia:transaction(F2),
    mnesia:delete_table(mod_roster_tmp_table).


%% Convert roster table: xattrs fields become 
convert_table2(Fields) ->
    ?INFO_MSG("Converting roster table from "
	      "{usj, us, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    mnesia:transform_table(roster, ignore, Fields).


webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "roster"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = user_roster(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_roster(User, Server, Query, Lang) ->
    US = {exmpp_stringprep:nodeprep(User), exmpp_stringprep:nameprep(Server)},
    Items1 = mnesia:dirty_index_read(roster, US, #roster.us),
    Res = user_roster_parse_query(User, Server, Items1, Query),
    Items = mnesia:dirty_index_read(roster, US, #roster.us),
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
				    {U, S, R} = R#roster.jid,
				    ?XE("tr",
					[?XAC("td", [{"class", "valign"}],
					      catch exmpp_jid:jid_to_list(U, S, R)),
					 ?XAC("td", [{"class", "valign"}],
					      R#roster.name),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(R#roster.subscription)),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(Pending)),
					 ?XAE("td", [{"class", "valign"}], Groups),
					 if
					     Pending == in ->
						 ?XAE("td", [{"class", "valign"}],
						      [?INPUTT("submit",
							       "validate" ++
							       ejabberd_web_admin:term_to_id(R#roster.jid),
							       "Validate")]);
					     true ->
						 ?X("td")
					 end,
					 ?XAE("td", [{"class", "valign"}],
					      [?INPUTT("submit",
						       "remove" ++
						       ejabberd_web_admin:term_to_id(R#roster.jid),
						       "Remove")])])
			    end, SItems))])]
	end,
    [?XC("h1", ?T("Roster of ") ++ us_to_list(US))] ++
	case Res of
	    ok -> [?CT("Submitted"), ?P];
	    error -> [?CT("Bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      FItems ++
	      [?P,
	       ?INPUT("text", "newjid", ""), ?C(" "),
	       ?INPUTT("submit", "addjid", "Add Jabber ID")
	      ])].

user_roster_parse_query(User, Server, Items, Query) ->
    case lists:keysearch("addjid", 1, Query) of
	{value, _} ->
	    case lists:keysearch("newjid", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, SJID}} ->
		    try
			JID = exmpp_jid:list_to_jid(SJID),
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
    UJID = exmpp_jid:make_bare_jid(User, Server),
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
		      JID1 = exmpp_jid:make_jid(U, S, R),
		      out_subscription(
			User, Server, JID1, subscribed),
		      UJID = exmpp_jid:make_bare_jid(User, Server),
		      ejabberd_router:route(
			UJID, JID1, exmpp_presence:subscribed()),
		      throw(submitted);
		  false ->
		      case lists:keysearch(
			     "remove" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
			  {value, _} ->
			      UJID = exmpp_jid:make_bare_jid(User, Server),
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

