%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_roster).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 process_iq/3,
	 process_local_iq/3,
	 get_subscription_lists/1,
	 in_subscription/3,
	 out_subscription/3]).

-include_lib("mnemosyne/include/mnemosyne.hrl").
-include("ejabberd.hrl").
-include("namespaces.hrl").

-record(roster, {uj,
		 user,
		 jid,
		 name = "",
		 subscription = none,
		 ask = none,
		 groups = [],
		 xattrs = [],
		 xs = []}).

start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{attributes, record_info(fields, roster)}]),
    mnesia:add_table_index(roster, user),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_ROSTER,
				  ?MODULE, process_local_iq, IQDisc).

process_local_iq(From, To, {iq, _, Type, _, _} = IQ) ->
    case Type of
	set ->
	    process_iq_set(From, To, IQ);
	get ->
	    process_iq_get(From, To, IQ)
    end.



process_iq(From, To, IQ) ->
    {iq, ID, Type, XMLNS, SubEl} = IQ,
    {_, Server, _} = From,
    case ?MYNAME of
	Server ->
	    process_local_iq(From, To, IQ),
	    ignore;
	_ ->
	    {iq, ID, error, XMLNS,
	     [SubEl, jlib:make_error_element("404", "Not Found")]}
    end.

process_iq_get(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    {User, _, _} = From,
    LUser = jlib:tolower(User),
    F = fun() ->
		mnesia:index_read(roster, LUser, #roster.user)
        end,
    case mnesia:transaction(F) of
	{atomic, Items} ->
	    XItems = lists:map(fun item_to_xml/1, Items),
	    {iq, ID, result, XMLNS, [{xmlelement, "query",
				      [{"xmlns", ?NS_ROSTER}],
				      XItems}]};
	_ ->
	    {iq, ID, error, XMLNS,
	     [SubEl, jlib:make_error_element("500",
					     "Internal Server Error")]}
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
    Attrs4 = case Item#roster.ask of
		 none ->
		     Attrs3;
		 subscribe ->
		     [{"ask", "subscribe"} | Attrs3];
		 unsubscribe ->
		     [{"ask", "unsubscribe"} | Attrs3]
	     end,
    Attrs = Attrs4 ++ Item#roster.xattrs,
    SubEls1 = lists:map(fun(G) ->
				{xmlelement, "group", [], [{xmlcdata, G}]}
			end, Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    {xmlelement, "item", Attrs, SubEls}.


process_iq_set(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    {User, _, _} = From,
    {xmlelement, Name, Attrs, Els} = SubEl,
    lists:foreach(fun(El) -> process_item_set(User, From, To, El) end, Els),
    {iq, ID, result, XMLNS, []}.

process_item_set(User, From, To, XItem) ->
    {xmlelement, Name, Attrs, Els} = XItem,
    % TODO: load existing item
    JID = jlib:string_to_jid(xml:get_attr_s("jid", Attrs)),
    LUser = jlib:tolower(User),
    case JID of
	error ->
	    ok;
	_ ->
	    LJID = jlib:jid_tolower(JID),
	    F = fun() ->
			Res = mnesia:read({roster, {LUser, LJID}}),
			Item = case Res of
				   [] ->
				       #roster{uj = {LUser, LJID},
					       user = LUser,
					       jid = JID};
				   [I] ->
				       I#roster{user = LUser,
						jid = JID,
						name = "",
						groups = [],
						xattrs = [],
						xs = []}
			       end,
			Item1 = process_item_attrs(Item, Attrs),
			Item2 = process_item_els(Item1, Els),
			case Item2#roster.subscription of
			    remove ->
				mnesia:delete({roster, {LUser, LJID}});
			    _ ->
				mnesia:write(Item2)
			end,
			{Item, Item2}
		end,
	    case mnesia:transaction(F) of
		{atomic, {OldItem, Item}} ->
		    push_item(User, To, Item),
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
				      From, OldItem#roster.jid,
				      {xmlelement, "presence",
				       [{"type", "unsubscribe"}],
				       []});
			       true -> ok
			    end,
			    if IsFrom ->
				    ejabberd_router:route(
				      From, OldItem#roster.jid,
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
    end.

process_item_attrs(Item, [{Attr, Val} | Attrs]) ->
    case Attr of
	"jid" ->
	    case jlib:string_to_jid(Val) of
		error ->
		    process_item_attrs(Item, Attrs);
		JID ->
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


% {user, jid, name, subscription, groups, xattrs, xs}
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


push_item(User, From, Item) ->
    ejabberd_sm ! {route, {"", "", ""}, {User, "", ""},
		   {xmlelement, "broadcast", [],
		    [{item,
		      Item#roster.jid,
		      Item#roster.subscription}]}},
    lists:foreach(fun(Resource) ->
			  push_item(User, Resource, From, Item)
		  end, ejabberd_sm:get_user_resources(User)).

% TODO: don't push to those who not load roster
push_item(User, Resource, From, Item) ->
    ResIQ = {iq, "", set, ?NS_ROSTER,
	     [{xmlelement, "query",
	       [{"xmlns", ?NS_ROSTER}],
	       [item_to_xml(Item)]}]},
    ejabberd_router ! {route,
		       From,
		       {User, ?MYNAME, Resource},
		       jlib:iq_to_xml(ResIQ)}.


get_subscription_lists(User) ->
    LUser = jlib:tolower(User),
    F = fun() ->
		mnesia:index_read(roster, LUser, #roster.user)
        end,
    case mnesia:transaction(F) of
	{atomic, Items} ->
	    fill_subscription_lists(Items, [], []);
	_ ->
	    {[], []}
    end.

fill_subscription_lists([I | Is], F, T) ->
    %J = I#roster.jid,
    J = element(2, I#roster.uj),
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


in_subscription(User, From, Type) ->
    LUser = jlib:tolower(User),
    LFrom = jlib:jid_tolower(From),
    {FU, FS, FR} = From,
    F = fun() ->
		case mnesia:read({roster, {LUser, LFrom}}) of
		    [] ->
			case Type of
			    subscribe ->
				true;
			    unsubscribe ->
				true;
			    unsubscribed ->
				false;
			    subscribed ->
				NewItem = #roster{uj = {LUser, LFrom},
						  user = LUser,
						  jid = From},
				mnesia:write(NewItem),
				true
			end;
		    [I] ->
			case Type of
			    subscribe ->
				S = I#roster.subscription,
				if
				    (S == both) or (S == from) ->
					{update,
					 {xmlelement, "presence",
					  [{"type", "subscribed"}], []},
					 I};
				    true ->
					true
				end;
			    unsubscribe ->
				S = I#roster.subscription,
				if
				    (S == none) or (S == to) ->
					{update,
					 {xmlelement, "presence",
					  [{"type", "unsubscribed"}], []},
					 I};
				    true ->
					true
				end;
			    _ ->
				S = I#roster.subscription,
				NS = case Type of
					 subscribed ->
					     case S of
						 from -> both;
						 none -> to;
						 _    -> S
					     end;
					 unsubscribed ->
					     case S of
						 both -> from;
						 to   -> none;
						 _    -> S
					     end
				     end,
				NewItem = I#roster{subscription = NS,
						   ask = none},
				mnesia:write(NewItem),
				{push, NewItem}
			end
		end
        end,
    case mnesia:transaction(F) of
	{atomic, true} ->
	    true;
	{atomic, false} ->
	    false;
	{atomic, {update, Presence, Item}} ->
	    ejabberd_router:route({User, ?MYNAME, ""}, {FU, FS, ""}, Presence),
	    ejabberd_sm ! {route, {"", "", ""}, {User, "", ""},
			   {xmlelement, "broadcast", [],
			    [{item,
			      Item#roster.jid,
			      Item#roster.subscription}]}},
	    false;
	{atomic, {push, Item}} ->
	    push_item(User, {"", ?MYNAME, ""}, Item),
	    true;
	_ ->
	    false
    end.

out_subscription(User, JID, Type) ->
    LUser = jlib:tolower(User),
    LJID = jlib:jid_tolower(JID),
    F = fun() ->
		Item = case mnesia:read({roster, {LUser, LJID}}) of
			   [] ->
			       if (Type == unsubscribe) or
				  (Type == unsubscribed) ->
				       false;
				  true ->
				       #roster{uj = {LUser, LJID},
					       user = LUser,
					       jid = JID}
			       end;
			   [I] ->
			       I
		       end,
		if Item == false ->
			ok;
		   true ->
			{NewItem, Update} =
			    case Type of
				subscribe ->
				    {Item#roster{ask = subscribe}, false};
				unsubscribe ->
				    {Item#roster{ask = unsubscribe}, false};
				subscribed ->
				    S = Item#roster.subscription,
				    NS = case S of
					     to   -> both;
					     none -> from;
					     _    -> S
					 end,
				    {Item#roster{subscription = NS,
						 ask = none},
				     true};
				unsubscribed ->
				    S = Item#roster.subscription,
				    NS = case S of
					     both -> to;
					     from -> none;
					     _    -> S
					 end,
				    {Item#roster{subscription = NS,
						 ask = none},
				     true}
			    end,
			mnesia:write(NewItem),
			{push, NewItem, Update}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{atomic, {push, Item, Update}} ->
	    push_item(User, {"", ?MYNAME, ""}, Item),
	    if
		Update ->
		    ejabberd_sm ! {route, {"", "", ""}, {User, "", ""},
				   {xmlelement, "broadcast", [],
				    [{item,
				      Item#roster.jid,
				      Item#roster.subscription}]}};
		true ->
		    ok
	    end;
	_ ->
	    false
    end.

