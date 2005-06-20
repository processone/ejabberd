%%%----------------------------------------------------------------------
%%% File    : mod_roster_odbc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Roster management
%%% Created : 15 Dec 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_roster_odbc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 process_iq/3,
	 process_local_iq/3,
	 get_subscription_lists/2,
	 in_subscription/4,
	 out_subscription/3,
	 set_items/2,
	 remove_user/1,
	 get_jid_info/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(roster, {user,
		 jid,
		 name = "",
		 subscription = none,
		 ask = none}).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(roster_out_subscription, Host,
		       ?MODULE, out_subscription, 50),
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
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ROSTER,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
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
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ROSTER).


-define(PSI_ROSTER_WORKAROUND, true).

-ifdef(PSI_ROSTER_WORKAROUND).

process_iq(From, To, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    #jid{lserver = LServer} = From,
    case ?MYNAME of
	LServer ->
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
    case ?MYNAME of
	LServer ->
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
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(
		 ["select username, jid, nick, subscription, ask, "
		  "server, subscribe, type from rosterusers "
		  "where username='", Username, "'"]) of
	{selected, ["username", "jid", "nick", "subscription", "ask",
		    "server", "subscribe", "type"],
	 Items} when is_list(Items) ->
	    XItems = lists:flatmap(
		       fun(I) ->
			       case raw_to_record(I) of
				   error ->
				       [];
				   R ->
				       [item_to_xml(R)]
			       end
		       end, Items),
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_ROSTER}],
			     XItems}]};
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
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
    Attrs = case ask_to_pending(Item#roster.ask) of
		out ->
		    [{"ask", "subscribe"} | Attrs3];
		both ->
		    [{"ask", "subscribe"} | Attrs3];
		_ ->
		    Attrs3
	    end,
    % TODO
    %SubEls = lists:map(fun(G) ->
    %    		       {xmlelement, "group", [], [{xmlcdata, G}]}
    %    	       end, Item#roster.groups),
    SubEls = [],
    {xmlelement, "item", Attrs, SubEls}.


process_iq_set(From, To, #iq{sub_el = SubEl} = IQ) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els),
    IQ#iq{type = result, sub_el = []}.

process_item_set(From, To, {xmlelement, _Name, Attrs, Els}) ->
    JID1 = jlib:string_to_jid(xml:get_attr_s("jid", Attrs)),
    #jid{user = User, luser = LUser} = From,
    case JID1 of
	error ->
	    ok;
	_ ->
	    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
	    LJID = jlib:jid_tolower(JID1),
	    Username = ejabberd_odbc:escape(LUser),
	    SJID = ejabberd_odbc:escape(jlib:jid_to_string(LJID)),
	    case catch ejabberd_odbc:sql_query(
			 ["select username, jid, nick, subscription, ask, "
			  "server, subscribe, type from rosterusers "
			  "where username='", Username, "' "
			  "and jid='", SJID, "'"]) of
		{selected, ["username", "jid", "nick", "subscription", "ask",
			    "server", "subscribe", "type"],
		 Res} ->
		    Item = case Res of
			       [] ->
				   #roster{user = LUser,
					   jid = LJID};
			       [I] ->
				   (raw_to_record(I))#roster{user = LUser,
							     jid = JID,
							     name = ""}
			   end,
		    Item1 = process_item_attrs(Item, Attrs),
		    case Item1#roster.subscription of
			remove ->
			    catch ejabberd_odbc:sql_query(
				    ["begin;"
				     "delete from rosterusers "
				     "      where username='", Username, "' "
				     "        and jid='", SJID, "';"
				     "delete from rostergroups "
				     "      where username='", Username, "' "
				     "        and jid='", SJID, "';"
				     "commit"]);
			_ ->
			    ItemVals = record_to_string(Item1),
			    catch ejabberd_odbc:sql_query(
				    ["begin;"
				     "delete from rosterusers "
				     "      where username='", Username, "' "
				     "        and jid='", SJID, "';"
				     "insert into rosterusers("
				     "              username, jid, nick, "
				     "              subscription, ask, "
				     "              server, subscribe, type) "
				     " values ", ItemVals, ";"
				     "delete from rostergroups "
				     "      where username='", Username, "' "
				     "        and jid='", SJID, "';"
				     % TODO
				     "commit"])

			    %mnesia:write(Item1)
		    end,
		    push_item(User, To, Item1),
		    case Item1#roster.subscription of
			remove ->
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
				    ejabberd_router:route(
				      jlib:jid_remove_resource(From),
				      jlib:make_jid(Item#roster.jid),
				      {xmlelement, "presence",
				       [{"type", "unsubscribe"}],
				       []});
			       true -> ok
			    end,
			    if IsFrom ->
				    ejabberd_router:route(
				      jlib:jid_remove_resource(From),
				      jlib:make_jid(Item#roster.jid),
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
	    process_item_attrs(Item, Attrs)
    end;
process_item_attrs(Item, []) ->
    Item.


% TODO
%process_item_els(Item, [{xmlelement, Name, Attrs, SEls} | Els]) ->
%    case Name of
%	"group" ->
%	    Groups = [xml:get_cdata(SEls) | Item#roster.groups],
%	    process_item_els(Item#roster{groups = Groups}, Els);
%	_ ->
%	    case xml:get_attr_s("xmlns", Attrs) of
%		"" ->
%		    process_item_els(Item, Els);
%		_ ->
%		    XEls = [{xmlelement, Name, Attrs, SEls} | Item#roster.xs],
%		    process_item_els(Item#roster{xs = XEls}, Els)
%	    end
%    end;
process_item_els(Item, [{xmlcdata, _} | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) ->
    Item.


push_item(User, From, Item) ->
    ejabberd_sm:route(jlib:make_jid("", "", ""),
		      jlib:make_jid(User, "", ""),
		      {xmlelement, "broadcast", [],
		       [{item,
			 Item#roster.jid,
			 Item#roster.subscription}]}),
    lists:foreach(fun(Resource) ->
			  push_item(User, Resource, From, Item)
		  end, ejabberd_sm:get_user_resources(User, 'TODO')).

% TODO: don't push to those who not load roster
-ifdef(PSI_ROSTER_WORKAROUND).

push_item(User, Resource, _From, Item) ->
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
		sub_el = [{xmlelement, "query",
			   [{"xmlns", ?NS_ROSTER}],
			   [item_to_xml(Item)]}]},
    ejabberd_router:route(
      jlib:make_jid(User, ?MYNAME, Resource),
      jlib:make_jid(User, ?MYNAME, Resource),
      jlib:iq_to_xml(ResIQ)).

-else.

push_item(User, Resource, From, Item) ->
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
		sub_el = [{xmlelement, "query",
			   [{"xmlns", ?NS_ROSTER}],
			   [item_to_xml(Item)]}]},
    ejabberd_router:route(
      From,
      jlib:make_jid(User, ?MYNAME, Resource),
      jlib:iq_to_xml(ResIQ)).

-endif.

get_subscription_lists(_, User) ->
    LUser = jlib:nodeprep(User),
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(
		 ["select username, jid, nick, subscription, ask, "
		  "server, subscribe, type from rosterusers "
		  "where username='", Username, "'"]) of
	{selected, ["username", "jid", "nick", "subscription", "ask",
		    "server", "subscribe", "type"],
	 Items} when is_list(Items) ->
	    fill_subscription_lists(Items, [], []);
	_ ->
	    {[], []}
    end.

fill_subscription_lists([I | Is], F, T) ->
    J = I#roster.jid,
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



in_subscription(_, User, JID, Type) ->
    process_subscription(in, User, JID, Type).

out_subscription(User, JID, Type) ->
    process_subscription(out, User, JID, Type).

process_subscription(Direction, User, JID1, Type) ->
    LUser = jlib:nodeprep(User),
    LJID = jlib:jid_tolower(JID1),
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_string(LJID)),
    Item = case catch ejabberd_odbc:sql_query(
			["select username, jid, nick, subscription, ask, "
			 "server, subscribe, type from rosterusers "
			 "where username='", Username, "' "
			 "and jid='", SJID, "'"]) of
	       {selected, ["username", "jid", "nick", "subscription", "ask",
			   "server", "subscribe", "type"],
		[I]} ->
		   raw_to_record(I);
	       _ ->
		   #roster{user = LUser,
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
    Push = case NewState of
	       none ->
		   none;
	       {Subscription, Pending} ->
		   NewItem = Item#roster{subscription = Subscription,
					 ask = Pending},
		   ItemVals = record_to_string(NewItem),
		   catch ejabberd_odbc:sql_query(
			   ["begin;"
			    "delete from rosterusers "
			    "      where username='", Username, "' "
			    "        and jid='", SJID, "';"
			    "insert into rosterusers("
			    "              username, jid, nick, "
			    "              subscription, ask, "
			    "              server, subscribe, type) "
			    " values ", ItemVals, ";"
			    "commit"]),
		   {push, NewItem}
	   end,
    case AutoReply of
	none ->
	    ok;
	_ ->
	    T = case AutoReply of
		    subscribed -> "subscribed";
		    unsubscribed -> "unsubscribed"
		end,
	    ejabberd_router:route(
	      jlib:make_jid(User, ?MYNAME, ""), JID1,
	      {xmlelement, "presence", [{"type", T}], []})
    end,
    case Push of
	{push, PushItem} ->
	    push_item(User, jlib:make_jid("", ?MYNAME, ""), PushItem),
	    true;
	none ->
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


remove_user(User) ->
    LUser = jlib:nodeprep(User),
    Username = ejabberd_odbc:escape(LUser),
    catch ejabberd_odbc:sql_query(
	    ["begin;"
	     "delete from rosterusers "
	     "      where username='", Username, "';"
	     "delete from rostergroups "
	     "      where username='", Username, "';"
	     "commit"]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_items(User, SubEl) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    LUser = jlib:nodeprep(User),
    F = fun() ->
		lists:foreach(fun(El) -> process_item_set_t(LUser, El) end, Els)
	end,
    mnesia:transaction(F).

process_item_set_t(LUser, {xmlelement, _Name, Attrs, Els}) ->
    JID1 = jlib:string_to_jid(xml:get_attr_s("jid", Attrs)),
    case JID1 of
	error ->
	    ok;
	_ ->
	    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
	    LJID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
	    Item = #roster{user = LUser,
			   jid = LJID},
	    Item1 = process_item_attrs_ws(Item, Attrs),
	    Item2 = process_item_els(Item1, Els),
	    case Item2#roster.subscription of
		remove ->
		    mnesia:delete({roster, {LUser, LJID}});
		_ ->
		    mnesia:write(Item2)
	    end
    end;
process_item_set_t(_LUser, _) ->
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
	    process_item_attrs_ws(Item, Attrs)
    end;
process_item_attrs_ws(Item, []) ->
    Item.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_jid_info(_, User, JID) ->
% TODO
%    LUser = jlib:nodeprep(User),
%    LJID = jlib:jid_tolower(JID),
%    case catch mnesia:dirty_read(roster, {LUser, LJID}) of
%	[#roster{subscription = Subscription, groups = Groups}] ->
%	    {Subscription, Groups};
%	_ ->
%	    LRJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
%	    if
%		LRJID == LJID ->
%		    {none, []};
%		true ->
%		    case catch mnesia:dirty_read(roster, {LUser, LRJID}) of
%			[#roster{subscription = Subscription,
%				 groups = Groups}] ->
%			    {Subscription, Groups};
%			_ ->
%			    {none, []}
%		    end
%	    end
%    end.
{none, []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

raw_to_record({User, SJID, Nick, SSubscription, SAsk,
	       _SServer, _SSubscribe, _SType}) ->
    case jlib:string_to_jid(SJID) of
	error ->
	    error;
	JID ->
	    LJID = jlib:jid_tolower(JID),
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
	    #roster{user = User,
		    jid = LJID,
		    name = Nick,
		    subscription = Subscription,
		    ask = Ask}
    end.

record_to_string(#roster{user = User,
			 jid = JID,
			 name = Name,
			 subscription = Subscription,
			 ask = Ask}) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(jlib:jid_to_string(JID)),
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
    ["("
     "'", Username, "',"
     "'", SJID, "',"
     "'", Nick, "',"
     "'", SSubscription, "',"
     "'", SAsk, "',"
     "'N', '', 'item')"].

