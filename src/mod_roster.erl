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

-export([]).

-export([start/0, init/0, process_iq/3]).

-include_lib("mnemosyne/include/mnemosyne.hrl").
-include("ejabberd.hrl").

-record(roster, {user,
		 jid,
		 name = "",
		 subscription = none,
		 groups = [],
		 xattrs = [],
		 xs = []}).

-define(ME, ejabberd_mod_roster).

start() ->
    register(?ME, spawn(mod_roster, init, [])).

init() ->
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{type, bag},
				{attributes, record_info(fields, roster)}]),
    mnesia:add_table_index(roster, jid),
    ejabberd_local:register_iq_handler("jabber:iq:roster",
				       ?MODULE, process_iq),
    loop().

loop() ->
    receive
	{process_iq, From, To, {iq, ID, Type, XMLNS, SubEl}} ->
	    case Type of
		set ->
		    ResIQ = process_iq_set(From, To,
					   {iq, ID, Type, XMLNS, SubEl}),
		    ejabberd_router ! {route,
				       To,
				       From,
				       jlib:iq_to_xml(ResIQ)},
		    loop();
		get ->
		    ResIQ = process_iq_get(From, To,
					   {iq, ID, Type, XMLNS, SubEl}),
		    ejabberd_router ! {route,
				       To,
				       From,
				       jlib:iq_to_xml(ResIQ)},
		    loop()
	    end
    end.



process_iq(From, To, IQ) ->
    {iq, ID, Type, XMLNS, SubEl} = IQ,
    {_, Server, _} = From,
    case ?MYNAME of
	Server ->
	    ?ME ! {process_iq, From, To, IQ},
	    ignore;
	_ ->
	    {iq, ID, error, XMLNS,
	     [SubEl, jlib:make_error_element("404", "Not Found")]}
    end.

process_iq_get(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    {User, _, _} = From,
    LUser = jlib:tolower(User),
    F = fun() ->
		mnesia:read({roster, LUser})
        end,
    case mnesia:transaction(F) of
	{atomic, Items} ->
	    XItems = lists:map(fun item_to_xml/1, Items),
	    {iq, ID, result, XMLNS, [{xmlelement, "query",
				      [{"xmlns", "jabber:iq:roster"}],
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
		 remove ->
		     [{"subscription", "remove"} | Attrs2];
		 _ ->
		     % TODO
		     Attrs2
	     end,
    Attrs = Attrs3 ++ Item#roster.xattrs,
    SubEls1 = lists:map(fun(G) ->
				{xmlelement, "group", [], [{xmlcdata, G}]}
			end, Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    {xmlelement, "item", Attrs, SubEls}.


process_iq_set(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    {User, _, _} = From,
    {xmlelement, Name, Attrs, Els} = SubEl,
    lists:foreach(fun(El) -> process_item_set(User, To, El) end, Els),
    {iq, ID, result, XMLNS, []}.

process_item_set(User, To, XItem) ->
    {xmlelement, Name, Attrs, Els} = XItem,
    % TODO: load existing item
    JID = jlib:string_to_jid(xml:get_attr_s("jid", Attrs)),
    LUser = jlib:tolower(User),
    case JID of
	error ->
	    ok;
	_ ->
	    F = fun() ->
			Res = mnemosyne:eval(query [X || X <- table(roster),
							 X.user = LUser,
							 X.jid = JID]
					     end),
			Item = case Res of
				   [] ->
				       #roster{user = LUser,
					       jid = JID,
					       groups = [],
					       xattrs = [],
					       xs = []};
				   [I] ->
				       mnesia:delete_object(I),
				       I#roster{groups = [],
						xattrs = [],
						xs = []}
			       end,
			Item1 = process_item_attrs(Item, Attrs),
			Item2 = process_item_els(Item1, Els),
			case Item2#roster.subscription of
			    remove ->
				ok;
			    _ ->
				mnesia:write(Item2)
			end,
			Item2
		end,
	    case mnesia:transaction(F) of
		{atomic, Item} ->
		    push_item(User, To, Item),
		    ok;
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
    lists:foreach(fun(Resource) ->
			  push_item(User, Resource, From, Item)
		  end, ejabberd_sm:get_user_resources(User)).

% TODO: don't push to those who not load roster
push_item(User, Resource, From, Item) ->
    ResIQ = {iq, "", set, "jabber:iq:roster",
	     [{xmlelement, "query",
	       [{"xmlns", "jabber:iq:roster"}],
	       [item_to_xml(Item)]}]},
    ejabberd_router ! {route,
		       From,
		       {User, ?MYNAME, Resource},
		       jlib:iq_to_xml(ResIQ)}.


