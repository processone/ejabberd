%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_roster).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1, stop/0,
	 process_iq/3,
	 process_local_iq/3,
	 get_user_roster/2,
	 get_subscription_lists/3,
	 in_subscription/5,
	 out_subscription/4,
	 set_items/3,
	 remove_user/2,
	 get_jid_info/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").


start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{attributes, record_info(fields, roster)}]),
    update_table(),
    mnesia:add_table_index(roster, us),
    ejabberd_hooks:add(roster_get,
		       ?MODULE, get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_subscription_lists,
		       ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:add(roster_get_jid_info,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_ROSTER,
				  ?MODULE, process_iq, IQDisc).

stop() ->
    ejabberd_hooks:delete(roster_get,
			  ?MODULE, get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_subscription_lists,
			  ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:delete(roster_get_jid_info,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_ROSTER).


-define(PSI_ROSTER_WORKAROUND, true).

-ifdef(PSI_ROSTER_WORKAROUND).

process_iq(From, To, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    #jid{lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    ResIQ = process_local_iq(From, To, IQ),
	    ejabberd_router:route(From, From,
				  jlib:iq_to_xml(ResIQ)),
	    ignore;
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end.

-else.

process_iq(From, To, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    #jid{lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    process_local_iq(From, To, IQ);
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end.

-endif.

process_local_iq(From, To, #iq{type = Type} = IQ) ->
    case Type of
	set ->
	    process_iq_set(From, To, IQ);
	get ->
	    process_iq_get(From, To, IQ)
    end.



process_iq_get(From, _To, #iq{sub_el = SubEl} = IQ) ->
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    US = {LUser, LServer},
    case catch ejabberd_hooks:run_fold(roster_get, [], [US]) of
	Items when is_list(Items) ->
	    XItems = lists:map(fun item_to_xml/1, Items),
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_ROSTER}],
			     XItems}]};
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
    end.

get_user_roster(Acc, US) ->
    case catch mnesia:dirty_index_read(roster, US, #roster.us) of
	Items when is_list(Items) ->
	    Items ++ Acc;
	_ ->
	    Acc
    end.




item_to_xml(Item) ->
    Attrs1 = [{"jid", jlib:jid_to_string(Item#roster.jid)}],
    Attrs2 = case Item#roster.name of
		 "" ->
		     Attrs1;
		 Name ->
		     [{"name", Name} | Attrs1]
	     end,
    Attrs3 = case Item#roster.subscription of
		 none ->
		     [{"subscription", "none"} | Attrs2];
		 from ->
		     [{"subscription", "from"} | Attrs2];
		 to ->
		     [{"subscription", "to"} | Attrs2];
		 both ->
		     [{"subscription", "both"} | Attrs2];
		 remove ->
		     [{"subscription", "remove"} | Attrs2]
	     end,
    Attrs4 = case ask_to_pending(Item#roster.ask) of
		 out ->
		     [{"ask", "subscribe"} | Attrs3];
		 both ->
		     [{"ask", "subscribe"} | Attrs3];
		 _ ->
		     Attrs3
	     end,
    Attrs = Attrs4 ++ Item#roster.xattrs,
    SubEls1 = lists:map(fun(G) ->
				{xmlelement, "group", [], [{xmlcdata, G}]}
			end, Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    {xmlelement, "item", Attrs, SubEls}.


process_iq_set(From, To, #iq{sub_el = SubEl} = IQ) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els),
    IQ#iq{type = result, sub_el = []}.

process_item_set(From, To, {xmlelement, _Name, Attrs, Els}) ->
    JID1 = jlib:string_to_jid(xml:get_attr_s("jid", Attrs)),
    #jid{user = User, luser = LUser, lserver = LServer} = From,
    case JID1 of
	error ->
	    ok;
	_ ->
	    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
	    LJID = jlib:jid_tolower(JID1),
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
						xattrs = [],
						xs = []}
			       end,
			Item1 = process_item_attrs(Item, Attrs),
			Item2 = process_item_els(Item1, Els),
			case Item2#roster.subscription of
			    remove ->
				mnesia:delete({roster, {LUser, LServer, LJID}});
			    _ ->
				mnesia:write(Item2)
			end,
			{Item, Item2}
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
			    if IsTo ->
				    ejabberd_router:route(
				      jlib:jid_remove_resource(From),
				      jlib:make_jid(OldItem#roster.jid),
				      {xmlelement, "presence",
				       [{"type", "unsubscribe"}],
				       []});
			       true -> ok
			    end,
			    if IsFrom ->
				    ejabberd_router:route(
				      jlib:jid_remove_resource(From),
				      jlib:make_jid(OldItem#roster.jid),
				      {xmlelement, "presence",
				       [{"type", "unsubscribed"}],
				       []});
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
    end;
process_item_set(_From, _To, _) ->
    ok.

process_item_attrs(Item, [{Attr, Val} | Attrs]) ->
    case Attr of
	"jid" ->
	    case jlib:string_to_jid(Val) of
		error ->
		    process_item_attrs(Item, Attrs);
		JID1 ->
		    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
		    process_item_attrs(Item#roster{jid = JID}, Attrs)
	    end;
	"name" ->
	    process_item_attrs(Item#roster{name = Val}, Attrs);
	"subscription" ->
	    case Val of
		"remove" ->
		    process_item_attrs(Item#roster{subscription = remove},
				       Attrs);
		_ ->
		    process_item_attrs(Item, Attrs)
	    end;
	"ask" ->
	    process_item_attrs(Item, Attrs);
	_ ->
	    XAttrs = Item#roster.xattrs,
	    process_item_attrs(Item#roster{xattrs = [{Attr, Val} | XAttrs]},
			       Attrs)
    end;
process_item_attrs(Item, []) ->
    Item.


process_item_els(Item, [{xmlelement, Name, Attrs, SEls} | Els]) ->
    case Name of
	"group" ->
	    Groups = [xml:get_cdata(SEls) | Item#roster.groups],
	    process_item_els(Item#roster{groups = Groups}, Els);
	_ ->
	    case xml:get_attr_s("xmlns", Attrs) of
		"" ->
		    process_item_els(Item, Els);
		_ ->
		    XEls = [{xmlelement, Name, Attrs, SEls} | Item#roster.xs],
		    process_item_els(Item#roster{xs = XEls}, Els)
	    end
    end;
process_item_els(Item, [{xmlcdata, _} | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) ->
    Item.


push_item(User, Server, From, Item) ->
    ejabberd_sm:route(jlib:make_jid("", "", ""),
		      jlib:make_jid(User, Server, ""),
		      {xmlelement, "broadcast", [],
		       [{item,
			 Item#roster.jid,
			 Item#roster.subscription}]}),
    lists:foreach(fun(Resource) ->
			  push_item(User, Server, Resource, From, Item)
		  end, ejabberd_sm:get_user_resources(User, Server)).

% TODO: don't push to those who not load roster
-ifdef(PSI_ROSTER_WORKAROUND).

push_item(User, Server, Resource, _From, Item) ->
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
		sub_el = [{xmlelement, "query",
			   [{"xmlns", ?NS_ROSTER}],
			   [item_to_xml(Item)]}]},
    ejabberd_router:route(
      jlib:make_jid(User, Server, Resource),
      jlib:make_jid(User, Server, Resource),
      jlib:iq_to_xml(ResIQ)).

-else.

push_item(User, Server, Resource, From, Item) ->
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
		sub_el = [{xmlelement, "query",
			   [{"xmlns", ?NS_ROSTER}],
			   [item_to_xml(Item)]}]},
    ejabberd_router:route(
      From,
      jlib:make_jid(User, Server, Resource),
      jlib:iq_to_xml(ResIQ)).

-endif.

get_subscription_lists(_, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
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



in_subscription(_, User, Server, JID, Type) ->
    process_subscription(in, User, Server, JID, Type).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type).

process_subscription(Direction, User, Server, JID1, Type) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    LJID = jlib:jid_tolower(JID1),
    F = fun() ->
		Item = case mnesia:read({roster, {LUser, LServer, LJID}}) of
			   [] ->
			       JID = {JID1#jid.user,
				      JID1#jid.server,
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
		case NewState of
		    none ->
			{none, AutoReply};
		    {Subscription, Pending} ->
			NewItem = Item#roster{subscription = Subscription,
					      ask = Pending},
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
		    T = case AutoReply of
			    subscribed -> "subscribed";
			    unsubscribed -> "unsubscribed"
			end,
		    ejabberd_router:route(
		      jlib:make_jid(User, Server, ""), JID1,
		      {xmlelement, "presence", [{"type", T}], []})
	    end,
	    case Push of
		{push, Item} ->
		    push_item(User, Server,
			      jlib:make_jid("", Server, ""), Item),
		    true;
		none ->
		    false
	    end;
	_ ->
	    false
    end.

%% in_state_change(Subscription, Pending, Type) -> NewState
%% NewState = none | {NewSubscription, NewPending}

in_state_change(none, none, subscribe)    -> {none, in};
in_state_change(none, none, subscribed)   -> {to, none}; % Workaround for gateways
%in_state_change(none, none, subscribed)   -> none;
in_state_change(none, none, unsubscribe)  -> none;
in_state_change(none, none, unsubscribed) -> none;
in_state_change(none, out,  subscribe)    -> {none, both};
in_state_change(none, out,  subscribed)   -> {to, none};
in_state_change(none, out,  unsubscribe)  -> none;
in_state_change(none, out,  unsubscribed) -> {none, none};
in_state_change(none, in,   subscribe)    -> none;
in_state_change(none, in,   subscribed)   -> {to, in}; % Workaround for gateways
%in_state_change(none, in,   subscribed)   -> none;
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
out_state_change(none, out,  subscribe)    -> none;
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
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
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
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    F = fun() ->
		lists:foreach(fun(El) ->
				      process_item_set_t(LUser, LServer, El)
			      end, Els)
	end,
    mnesia:transaction(F).

process_item_set_t(LUser, LServer, {xmlelement, _Name, Attrs, Els}) ->
    JID1 = jlib:string_to_jid(xml:get_attr_s("jid", Attrs)),
    case JID1 of
	error ->
	    ok;
	_ ->
	    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
	    LJID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
	    Item = #roster{usj = {LUser, LServer, LJID},
			   us = {LUser, LServer},
			   jid = JID},
	    Item1 = process_item_attrs_ws(Item, Attrs),
	    Item2 = process_item_els(Item1, Els),
	    case Item2#roster.subscription of
		remove ->
		    mnesia:delete({roster, {LUser, LServer, LJID}});
		_ ->
		    mnesia:write(Item2)
	    end
    end;
process_item_set_t(_LUser, _LServer, _) ->
    ok.

process_item_attrs_ws(Item, [{Attr, Val} | Attrs]) ->
    case Attr of
	"jid" ->
	    case jlib:string_to_jid(Val) of
		error ->
		    process_item_attrs_ws(Item, Attrs);
		JID1 ->
		    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
		    process_item_attrs_ws(Item#roster{jid = JID}, Attrs)
	    end;
	"name" ->
	    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
	"subscription" ->
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
	"ask" ->
	    process_item_attrs_ws(Item, Attrs);
	_ ->
	    XAttrs = Item#roster.xattrs,
	    process_item_attrs_ws(Item#roster{xattrs = [{Attr, Val} | XAttrs]},
				  Attrs)
    end;
process_item_attrs_ws(Item, []) ->
    Item.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_jid_info(_, User, Server, JID) ->
    LUser = jlib:nodeprep(User),
    LJID = jlib:jid_tolower(JID),
    LServer = jlib:nameprep(Server),
    case catch mnesia:dirty_read(roster, {LUser, LServer, LJID}) of
	[#roster{subscription = Subscription, groups = Groups}] ->
	    {Subscription, Groups};
	_ ->
	    LRJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
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
	    ?INFO_MSG("Converting roster table from "
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
	    mnesia:delete_table(mod_roster_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating roster table", []),
	    mnesia:transform_table(roster, ignore, Fields)
    end.


