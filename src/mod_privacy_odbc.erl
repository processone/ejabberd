%%%----------------------------------------------------------------------
%%% File    : mod_privacy_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created :  5 Oct 2006 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(mod_privacy_odbc).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 process_iq/3,
	 process_iq_set/4,
	 process_iq_get/5,
	 get_user_list/3,
	 check_packet/6,
	 updated_list/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(privacy_iq_get, Host,
		       ?MODULE, process_iq_get, 50),
    ejabberd_hooks:add(privacy_iq_set, Host,
		       ?MODULE, process_iq_set, 50),
    ejabberd_hooks:add(privacy_get_user_list, Host,
		       ?MODULE, get_user_list, 50),
    ejabberd_hooks:add(privacy_check_packet, Host,
		       ?MODULE, check_packet, 50),
    ejabberd_hooks:add(privacy_updated_list, Host,
		       ?MODULE, updated_list, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVACY,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(privacy_iq_get, Host,
			  ?MODULE, process_iq_get, 50),
    ejabberd_hooks:delete(privacy_iq_set, Host,
			  ?MODULE, process_iq_set, 50),
    ejabberd_hooks:delete(privacy_get_user_list, Host,
			  ?MODULE, get_user_list, 50),
    ejabberd_hooks:delete(privacy_check_packet, Host,
			  ?MODULE, check_packet, 50),
    ejabberd_hooks:delete(privacy_updated_list, Host,
			  ?MODULE, updated_list, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVACY).

process_iq(_From, _To, IQ) ->
    SubEl = IQ#iq.sub_el,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.


process_iq_get(_, From, _To, #iq{sub_el = SubEl},
	       #userlist{name = Active}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    {xmlelement, _, _, Els} = SubEl,
    case xml:remove_cdata(Els) of
	[] ->
	    process_lists_get(LUser, LServer, Active);
	[{xmlelement, Name, Attrs, _SubEls}] ->
	    case Name of
		"list" ->
		    ListName = xml:get_attr("name", Attrs),
		    process_list_get(LUser, LServer, ListName);
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end;
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end.


process_lists_get(LUser, LServer, Active) ->
    Default = case catch sql_get_default_privacy_list(LUser, LServer) of
		  {selected, ["name"], []} ->
		      none;
		  {selected, ["name"], [{DefName}]} ->
		      DefName;
		  _ ->
		      none
	      end,
    case catch sql_get_privacy_list_names(LUser, LServer) of
	{selected, ["name"], []} ->
	    {result, [{xmlelement, "query", [{"xmlns", ?NS_PRIVACY}], []}]};
	{selected, ["name"], Names} ->
	    LItems = lists:map(
		       fun({N}) ->
			       {xmlelement, "list",
				[{"name", N}], []}
		       end, Names),
	    DItems =
		case Default of
		    none ->
			LItems;
		    _ ->
			[{xmlelement, "default",
			  [{"name", Default}], []} | LItems]
		end,
	    ADItems =
		case Active of
		    none ->
			DItems;
		    _ ->
			[{xmlelement, "active",
			  [{"name", Active}], []} | DItems]
		end,
	    {result,
	     [{xmlelement, "query", [{"xmlns", ?NS_PRIVACY}],
	       ADItems}]};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_list_get(LUser, LServer, {value, Name}) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
	{selected, ["id"], []} ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	{selected, ["id"], [{ID}]} ->
	    case catch sql_get_privacy_list_data_by_id(ID, LServer) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    Items = lists:map(fun raw_to_item/1, RItems),
		    LItems = lists:map(fun item_to_xml/1, Items),
		    {result,
		     [{xmlelement, "query", [{"xmlns", ?NS_PRIVACY}],
		       [{xmlelement, "list",
			 [{"name", Name}], LItems}]}]};
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end;
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;

process_list_get(_LUser, _LServer, false) ->
    {error, ?ERR_BAD_REQUEST}.

item_to_xml(Item) ->
    Attrs1 = [{"action", action_to_list(Item#listitem.action)},
	      {"order", order_to_list(Item#listitem.order)}],
    Attrs2 = case Item#listitem.type of
		 none ->
		     Attrs1;
		 Type ->
		     [{"type", type_to_list(Item#listitem.type)},
		      {"value", value_to_list(Type, Item#listitem.value)} |
		      Attrs1]
	     end,
    SubEls = case Item#listitem.match_all of
		 true ->
		     [];
		 false ->
		     SE1 = case Item#listitem.match_iq of
			       true ->
				   [{xmlelement, "iq", [], []}];
			       false ->
				   []
			   end,
		     SE2 = case Item#listitem.match_message of
			       true ->
				   [{xmlelement, "message", [], []} | SE1];
			       false ->
				   SE1
			   end,
		     SE3 = case Item#listitem.match_presence_in of
			       true ->
				   [{xmlelement, "presence-in", [], []} | SE2];
			       false ->
				   SE2
			   end,
		     SE4 = case Item#listitem.match_presence_out of
			       true ->
				   [{xmlelement, "presence-out", [], []} | SE3];
			       false ->
				   SE3
			   end,
		     SE4
	     end,
    {xmlelement, "item", Attrs2, SubEls}.


action_to_list(Action) ->
    case Action of
	allow -> "allow";
	deny -> "deny"
    end.

order_to_list(Order) ->
    integer_to_list(Order).

type_to_list(Type) ->
    case Type of
	jid -> "jid";
	group -> "group";
	subscription -> "subscription"
    end.

value_to_list(Type, Val) ->
    case Type of
	jid -> jlib:jid_to_string(Val);
	group -> Val;
	subscription ->
	    case Val of
		both -> "both";
		to -> "to";
		from -> "from";
		none -> "none"
	    end
    end.



list_to_action(S) ->
    case S of
	"allow" -> allow;
	"deny" -> deny
    end.



process_iq_set(_, From, _To, #iq{sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    {xmlelement, _, _, Els} = SubEl,
    case xml:remove_cdata(Els) of
	[{xmlelement, Name, Attrs, SubEls}] ->
	    ListName = xml:get_attr("name", Attrs),
	    case Name of
		"list" ->
		    process_list_set(LUser, LServer, ListName,
				     xml:remove_cdata(SubEls));
		"active" ->
		    process_active_set(LUser, LServer, ListName);
		"default" ->
		    process_default_set(LUser, LServer, ListName);
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end;
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end.


process_default_set(LUser, LServer, {value, Name}) ->
    F = fun() ->
		case sql_get_privacy_list_names_t(LUser) of
		    {selected, ["name"], []} ->
			{error, ?ERR_ITEM_NOT_FOUND};
		    {selected, ["name"], Names} ->
			case lists:member({Name}, Names) of
			    true ->
				sql_set_default_privacy_list(LUser, Name),
				{result, []};
			    false ->
				{error, ?ERR_ITEM_NOT_FOUND}
			end
		end
	end,
    case odbc_queries:sql_transaction(LServer, F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, _} = Res} ->
	    Res;
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;

process_default_set(LUser, LServer, false) ->
    case catch sql_unset_default_privacy_list(LUser, LServer) of
	{'EXIT', _Reason} ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	{error, _Reason} ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	_ ->
	    {result, []}
    end.


process_active_set(LUser, LServer, {value, Name}) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
	{selected, ["id"], []} ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	{selected, ["id"], [{ID}]} ->
	    case catch sql_get_privacy_list_data_by_id(ID, LServer) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    Items = lists:map(fun raw_to_item/1, RItems),
		    {result, [], #userlist{name = Name, list = Items}};
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end;
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;

process_active_set(_LUser, _LServer, false) ->
    {result, [], #userlist{}}.






process_list_set(LUser, LServer, {value, Name}, Els) ->
    case parse_items(Els) of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	remove ->
	    F =
		fun() ->
			case sql_get_default_privacy_list_t(LUser) of
			    {selected, ["name"], []} ->
				sql_remove_privacy_list(LUser, Name),
				{result, []};
			    {selected, ["name"], [{Default}]} ->
				%% TODO: check active
				if
				    Name == Default ->
					{error, ?ERR_CONFLICT};
				    true ->
					sql_remove_privacy_list(LUser, Name),
					{result, []}
				end
			end
		end,
	    case odbc_queries:sql_transaction(LServer, F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _} = Res} ->
		    ejabberd_router:route(
		      jlib:make_jid(LUser, LServer, ""),
		      jlib:make_jid(LUser, LServer, ""),
		      {xmlelement, "broadcast", [],
		       [{privacy_list,
			 #userlist{name = Name, list = []},
			 Name}]}),
		    Res;
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end;
	List ->
	    RItems = lists:map(fun item_to_raw/1, List),
	    F =
		fun() ->
			ID =
			    case sql_get_privacy_list_id_t(LUser, Name) of
				{selected, ["id"], []} ->
				    sql_add_privacy_list(LUser, Name),
				    {selected, ["id"], [{I}]} =
					sql_get_privacy_list_id_t(LUser, Name),
				    I;
				{selected, ["id"], [{I}]} ->
				    I
			    end,
			sql_set_privacy_list(ID, RItems),
			{result, []}
		end,
	    case odbc_queries:sql_transaction(LServer, F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _} = Res} ->
		    ejabberd_router:route(
		      jlib:make_jid(LUser, LServer, ""),
		      jlib:make_jid(LUser, LServer, ""),
		      {xmlelement, "broadcast", [],
		       [{privacy_list,
			 #userlist{name = Name, list = List},
			 Name}]}),
		    Res;
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end
    end;

process_list_set(_LUser, _LServer, false, _Els) ->
    {error, ?ERR_BAD_REQUEST}.


parse_items([]) ->
    remove;
parse_items(Els) ->
    parse_items(Els, []).

parse_items([], Res) ->
    lists:reverse(Res);
parse_items([{xmlelement, "item", Attrs, SubEls} | Els], Res) ->
    Type   = xml:get_attr("type",   Attrs),
    Value  = xml:get_attr("value",  Attrs),
    SAction = xml:get_attr("action", Attrs),
    SOrder = xml:get_attr("order",  Attrs),
    Action = case catch list_to_action(element(2, SAction)) of
		 {'EXIT', _} -> false;
		 Val -> Val
	     end,
    Order = case catch list_to_integer(element(2, SOrder)) of
		{'EXIT', _} ->
		    false;
		IntVal ->
		    if
			IntVal >= 0 ->
			    IntVal;
			true ->
			    false
		    end
	    end,
    if
	(Action /= false) and (Order /= false) ->
	    I1 = #listitem{action = Action, order = Order},
	    I2 = case {Type, Value} of
		     {{value, T}, {value, V}} ->
			 case T of
			     "jid" ->
				 case jlib:string_to_jid(V) of
				     error ->
					 false;
				     JID ->
					 I1#listitem{
					   type = jid,
					   value = jlib:jid_tolower(JID)}
				 end;
			     "group" ->
				 I1#listitem{type = group,
					     value = V};
			     "subscription" ->
				 case V of
				     "none" ->
					 I1#listitem{type = subscription,
						     value = none};
				     "both" ->
					 I1#listitem{type = subscription,
						     value = both};
				     "from" ->
					 I1#listitem{type = subscription,
						     value = from};
				     "to" ->
					 I1#listitem{type = subscription,
						     value = to};
				     _ ->
					 false
				 end
			 end;
		     {{value, _}, false} ->
			 false;
		     _ ->
			 I1
		 end,
	    case I2 of
		false ->
		    false;
		_ ->
		    case parse_matches(I2, xml:remove_cdata(SubEls)) of
			false ->
			    false;
			I3 ->
			    parse_items(Els, [I3 | Res])
		    end
	    end;
	true ->
	    false
    end;

parse_items(_, _Res) ->
    false.


parse_matches(Item, []) ->
    Item#listitem{match_all = true};
parse_matches(Item, Els) ->
    parse_matches1(Item, Els).

parse_matches1(Item, []) ->
    Item;
parse_matches1(Item, [{xmlelement, "message", _, _} | Els]) ->
    parse_matches1(Item#listitem{match_message = true}, Els);
parse_matches1(Item, [{xmlelement, "iq", _, _} | Els]) ->
    parse_matches1(Item#listitem{match_iq = true}, Els);
parse_matches1(Item, [{xmlelement, "presence-in", _, _} | Els]) ->
    parse_matches1(Item#listitem{match_presence_in = true}, Els);
parse_matches1(Item, [{xmlelement, "presence-out", _, _} | Els]) ->
    parse_matches1(Item#listitem{match_presence_out = true}, Els);
parse_matches1(_Item, [{xmlelement, _, _, _} | _Els]) ->
    false.







get_user_list(_, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),

    case catch sql_get_default_privacy_list(LUser, LServer) of
	{selected, ["name"], []} ->
	    #userlist{};
	{selected, ["name"], [{Default}]} ->
	    case catch sql_get_privacy_list_data(LUser, LServer, Default) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    Items = lists:map(fun raw_to_item/1, RItems),
		    #userlist{name = Default, list = Items};
		_ ->
		    #userlist{}
	    end;
	_ ->
	    #userlist{}
    end.


check_packet(_, User, Server,
	     #userlist{list = List},
	     {From, To, {xmlelement, PName, _, _}},
	     Dir) ->
    case List of
	[] ->
	    allow;
	_ ->
	    PType = case PName of
			"message" -> message;
			"iq" -> iq;
			"presence" -> presence
		    end,
	    case {PType, Dir} of
		{message, in} ->
		    LJID = jlib:jid_tolower(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, jlib:nameprep(Server),
			  {none, []}, [User, Server, LJID]),
		    check_packet_aux(List, message,
				     LJID, Subscription, Groups);
		{iq, in} ->
		    LJID = jlib:jid_tolower(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, jlib:nameprep(Server),
			  {none, []}, [User, Server, LJID]),
		    check_packet_aux(List, iq,
				     LJID, Subscription, Groups);
		{presence, in} ->
		    LJID = jlib:jid_tolower(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, jlib:nameprep(Server),
			  {none, []}, [User, Server, LJID]),
		    check_packet_aux(List, presence_in,
				     LJID, Subscription, Groups);
		{presence, out} ->
		    LJID = jlib:jid_tolower(To),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, jlib:nameprep(Server),
			  {none, []}, [User, Server, LJID]),
		    check_packet_aux(List, presence_out,
				     LJID, Subscription, Groups);
		_ ->
		    allow
	    end
    end.

check_packet_aux([], _PType, _JID, _Subscription, _Groups) ->
    allow;
check_packet_aux([Item | List], PType, JID, Subscription, Groups) ->
    #listitem{type = Type, value = Value, action = Action} = Item,
    case is_ptype_match(Item, PType) of
	true ->
	    case Type of
		none ->
		    Action;
		_ ->
		    case is_type_match(Type, Value,
				       JID, Subscription, Groups) of
			true ->
			    Action;
			false ->
			    check_packet_aux(List, PType,
					     JID, Subscription, Groups)
		    end
	    end;
	false ->
	    check_packet_aux(List, PType, JID, Subscription, Groups)
    end.


is_ptype_match(Item, PType) ->
    case Item#listitem.match_all of
	true ->
	    true;
	false ->
	    case PType of
		message ->
		    Item#listitem.match_message;
		iq ->
		    Item#listitem.match_iq;
		presence_in ->
		    Item#listitem.match_presence_in;
		presence_out ->
		    Item#listitem.match_presence_out
	    end
    end.


is_type_match(Type, Value, JID, Subscription, Groups) ->
    case Type of
	jid ->
	    case Value of
		{"", Server, ""} ->
		    case JID of
			{_, Server, _} ->
			    true;
			_ ->
			    false
		    end;
		{User, Server, ""} ->
		    case JID of
			{User, Server, _} ->
			    true;
			_ ->
			    false
		    end;
		_ ->
		    Value == JID
	    end;
	subscription ->
	    Value == Subscription;
	group ->
	    lists:member(Value, Groups)
    end.


updated_list(_,
	     #userlist{name = OldName} = Old,
	     #userlist{name = NewName} = New) ->
    if
	OldName == NewName ->
	    New;
	true ->
	    Old
    end.


raw_to_item({SType, SValue, SAction, SOrder, SMatchAll, SMatchIQ,
	     SMatchMessage, SMatchPresenceIn, SMatchPresenceOut}) ->
    {Type, Value} =
	case SType of
	    "n" ->
		{none, none};
	    "j" ->
		case jlib:string_to_jid(SValue) of
		    #jid{} = JID ->
			{jid, jlib:jid_tolower(JID)}
		end;
	    "g" ->
		{group, SValue};
	    "s" ->
		case SValue of
		    "none" ->
			{subscription, none};
		    "both" ->
			{subscription, both};
		    "from" ->
			{subscription, from};
		    "to" ->
			{subscription, to}
		end
	end,
    Action =
	case SAction of
	    "a" -> allow;
	    "d" -> deny
	end,
    Order = list_to_integer(SOrder),
    MatchAll = SMatchAll == "1" orelse SMatchAll == "t",
    MatchIQ = SMatchIQ == "1" orelse SMatchIQ == "t" ,
    MatchMessage =  SMatchMessage == "1" orelse SMatchMessage == "t",
    MatchPresenceIn =  SMatchPresenceIn == "1" orelse SMatchPresenceIn == "t",
    MatchPresenceOut =  SMatchPresenceOut == "1" orelse SMatchPresenceOut == "t",
    #listitem{type = Type,
	      value = Value,
	      action = Action,
	      order = Order,
	      match_all = MatchAll,
	      match_iq = MatchIQ,
	      match_message = MatchMessage,
	      match_presence_in = MatchPresenceIn,
	      match_presence_out = MatchPresenceOut
	     }.

item_to_raw(#listitem{type = Type,
		      value = Value,
		      action = Action,
		      order = Order,
		      match_all = MatchAll,
		      match_iq = MatchIQ,
		      match_message = MatchMessage,
		      match_presence_in = MatchPresenceIn,
		      match_presence_out = MatchPresenceOut
		     }) ->
    {SType, SValue} =
	case Type of
	    none ->
		{"n", ""};
	    jid ->
		{"j", jlib:jid_to_string(Value)};
	    group ->
		{"g", Value};
	    subscription ->
		case Value of
		    none ->
			{"s", "none"};
		    both ->
			{"s", "both"};
		    from ->
			{"s", "from"};
		    to ->
			{"s", "to"}
		end
	end,
    SAction =
	case Action of
	    allow -> "a";
	    deny -> "d"
	end,
    SOrder = integer_to_list(Order),
    SMatchAll = if MatchAll -> "1"; true -> "0" end,
    SMatchIQ = if MatchIQ -> "1"; true -> "0" end,
    SMatchMessage = if MatchMessage -> "1"; true -> "0" end,
    SMatchPresenceIn = if MatchPresenceIn -> "1"; true -> "0" end,
    SMatchPresenceOut = if MatchPresenceOut -> "1"; true -> "0" end,
    ["'", SType, "', "
     "'", SValue, "', "
     "'", SAction, "', "
     "'", SOrder, "', "
     "'", SMatchAll, "', "
     "'", SMatchIQ, "', "
     "'", SMatchMessage, "', "
     "'", SMatchPresenceIn, "', "
     "'", SMatchPresenceOut, "'"].


sql_get_default_privacy_list(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      LServer,
      ["select name from privacy_default_list "
       "where username='", Username, "';"]).

sql_get_default_privacy_list_t(LUser) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query_t(
      ["select name from privacy_default_list "
       "where username='", Username, "';"]).

sql_get_privacy_list_names(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      LServer,
      ["select name from privacy_list "
       "where username='", Username, "';"]).

sql_get_privacy_list_names_t(LUser) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query_t(
      ["select name from privacy_list "
       "where username='", Username, "';"]).

sql_get_privacy_list_id(LUser, LServer, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query(
      LServer,
      ["select id from privacy_list "
       "where username='", Username, "' and name='", SName, "';"]).

sql_get_privacy_list_id_t(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query_t(
      ["select id from privacy_list "
       "where username='", Username, "' and name='", SName, "';"]).

sql_get_privacy_list_data(LUser, LServer, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query(
      LServer,
      ["select t, value, action, ord, match_all, match_iq, "
       "match_message, match_presence_in, match_presence_out "
       "from privacy_list_data "
       "where id = (select id from privacy_list where "
       "            username='", Username, "' and name='", SName, "') "
       "order by ord;"]).

sql_get_privacy_list_data_by_id(ID, LServer) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select t, value, action, ord, match_all, match_iq, "
       "match_message, match_presence_in, match_presence_out "
       "from privacy_list_data "
       "where id='", ID, "' order by ord;"]).

sql_set_default_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query_t(
      ["delete from privacy_default_list "
       "      where username='", Username, "';"]),
    ejabberd_odbc:sql_query_t(
      ["insert into privacy_default_list(username, name) "
       "values ('", Username, "', '", SName, "');"]).

sql_unset_default_privacy_list(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      LServer,
      ["delete from privacy_default_list "
       "      where username='", Username, "';"]).

sql_remove_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query_t(
      ["delete from privacy_list "
       "where username='", Username, "' and name='", SName, "';"]).

sql_add_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query_t(
      ["insert into privacy_list(username, name) "
       "values ('", Username, "', '", SName, "');"]).

sql_set_privacy_list(ID, RItems) ->
    ejabberd_odbc:sql_query_t(
      ["delete from privacy_list_data "
       "where id='", ID, "';"]),
    lists:foreach(fun(Items) ->
			  ejabberd_odbc:sql_query_t(
			    ["insert into privacy_list_data("
			     "id, t, value, action, ord, match_all, match_iq, "
			     "match_message, match_presence_in, "
			     "match_presence_out "
			     ") "
			     "values ('", ID, "', ", Items, ");"])
		  end, RItems).
