%%%----------------------------------------------------------------------
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

-module(mod_privacy).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 process_iq/3,
	 process_iq_set/4,
	 process_iq_get/5,
	 get_user_list/3,
	 check_packet/6,
	 remove_user/2,
         item_to_raw/1,
         raw_to_item/1,
         is_list_needdb/1,
	 updated_list/3]).

%% For mod_blocking
-export([sql_add_privacy_list/2,
         sql_get_default_privacy_list/2,
         sql_get_default_privacy_list_t/1,
         sql_get_privacy_list_data/3,
         sql_get_privacy_list_data_by_id_t/1,
         sql_get_privacy_list_id_t/2,
         sql_set_default_privacy_list/2,
         sql_set_privacy_list/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").


start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    case gen_mod:db_type(Opts) of
        mnesia ->
            mnesia:create_table(privacy,
                                [{disc_copies, [node()]},
                                 {attributes, record_info(fields, privacy)}]),
            update_table();
        _ ->
            ok
    end,
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
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
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
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
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
    case process_lists_get(LUser, LServer, Active,
                           gen_mod:db_type(LServer, ?MODULE)) of
        error ->
            {error, ?ERR_INTERNAL_SERVER_ERROR};
        {_Default, []} ->
            {result, [{xmlelement, "query", [{"xmlns", ?NS_PRIVACY}], []}]};
        {Default, LItems} ->
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
               ADItems}]}
    end.

process_lists_get(LUser, LServer, _Active, mnesia) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	{'EXIT', _Reason} ->
            error;
	[] ->
            {none, []};
	[#privacy{default = Default, lists = Lists}] ->
            LItems = lists:map(
                       fun({N, _}) ->
                               {xmlelement, "list", [{"name", N}], []}
                       end, Lists),
            {Default, LItems}
    end;
process_lists_get(LUser, LServer, _Active, odbc) ->
    Default = case catch sql_get_default_privacy_list(LUser, LServer) of
		  {selected, ["name"], []} ->
		      none;
		  {selected, ["name"], [{DefName}]} ->
		      DefName;
		  _ ->
		      none
	      end,
    case catch sql_get_privacy_list_names(LUser, LServer) of
	{selected, ["name"], Names} ->
            LItems = lists:map(
                       fun({N}) ->
                               {xmlelement, "list", [{"name", N}], []}
                       end, Names),
            {Default, LItems};
        _ ->
            error
    end.

process_list_get(LUser, LServer, {value, Name}) ->
    case process_list_get(LUser, LServer, Name,
                          gen_mod:db_type(LServer, ?MODULE)) of
        error ->
            {error, ?ERR_INTERNAL_SERVER_ERROR};
        not_found ->
            {error, ?ERR_ITEM_NOT_FOUND};
        Items ->
            LItems = lists:map(fun item_to_xml/1, Items),
            {result,
             [{xmlelement, "query", [{"xmlns", ?NS_PRIVACY}],
               [{xmlelement, "list",
                 [{"name", Name}], LItems}]}]}
    end;
process_list_get(_LUser, _LServer, false) ->
    {error, ?ERR_BAD_REQUEST}.

process_list_get(LUser, LServer, Name, mnesia) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	{'EXIT', _Reason} ->
            error;
	[] ->
            not_found;
	[#privacy{lists = Lists}] ->
	    case lists:keysearch(Name, 1, Lists) of
		{value, {_, List}} ->
                    List;
		_ ->
                    not_found
	    end
    end;
process_list_get(LUser, LServer, Name, odbc) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
	{selected, ["id"], []} ->
            not_found;
	{selected, ["id"], [{ID}]} ->
	    case catch sql_get_privacy_list_data_by_id(ID, LServer) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    lists:map(fun raw_to_item/1, RItems);
		_ ->
                    error
	    end;
	_ ->
            error
    end.

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

process_default_set(LUser, LServer, Value) ->
    case process_default_set(LUser, LServer, Value,
                             gen_mod:db_type(LServer, ?MODULE)) of
        {atomic, error} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR};
        {atomic, not_found} ->
            {error, ?ERR_ITEM_NOT_FOUND};
	{atomic, ok} ->
            {result, []};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_default_set(LUser, LServer, {value, Name}, mnesia) ->
    F = fun() ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
                        not_found;
		    [#privacy{lists = Lists} = P] ->
			case lists:keymember(Name, 1, Lists) of
			    true ->
				mnesia:write(P#privacy{default = Name,
						       lists = Lists}),
                                ok;
			    false ->
                                not_found
			end
		end
	end,
    mnesia:transaction(F);
process_default_set(LUser, LServer, {value, Name}, odbc) ->
    F = fun() ->
		case sql_get_privacy_list_names_t(LUser) of
		    {selected, ["name"], []} ->
                        not_found;
		    {selected, ["name"], Names} ->
			case lists:member({Name}, Names) of
			    true ->
				sql_set_default_privacy_list(LUser, Name),
                                ok;
			    false ->
                                not_found
			end
		end
	end,
    odbc_queries:sql_transaction(LServer, F);
process_default_set(LUser, LServer, false, mnesia) ->
    F = fun() ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
                        ok;
		    [R] ->
			mnesia:write(R#privacy{default = none})
		end
	end,
    mnesia:transaction(F);
process_default_set(LUser, LServer, false, odbc) ->
    case catch sql_unset_default_privacy_list(LUser, LServer) of
	{'EXIT', _Reason} ->
            {atomic, error};
	{error, _Reason} ->
            {atomic, error};
	_ ->
	    {atomic, ok}
    end.

process_active_set(LUser, LServer, {value, Name}) ->
    case process_active_set(LUser, LServer, Name,
                            gen_mod:db_type(LServer, ?MODULE)) of
        error ->
            {error, ?ERR_ITEM_NOT_FOUND};
        Items ->
            NeedDb = is_list_needdb(Items),
            {result, [], #userlist{name = Name, list = Items, needdb = NeedDb}}
    end;
process_active_set(_LUser, _LServer, false) ->
    {result, [], #userlist{}}.

process_active_set(LUser, LServer, Name, mnesia) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	[] ->
            error;
	[#privacy{lists = Lists}] ->
	    case lists:keysearch(Name, 1, Lists) of
		{value, {_, List}} ->
                    List;
		false ->
                    error
	    end
    end;
process_active_set(LUser, LServer, Name, odbc) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
	{selected, ["id"], []} ->
            error;
	{selected, ["id"], [{ID}]} ->
	    case catch sql_get_privacy_list_data_by_id(ID, LServer) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    lists:map(fun raw_to_item/1, RItems);
		_ ->
                    error
	    end;
	_ ->
            error
    end.

remove_privacy_list(LUser, LServer, Name, mnesia) ->
    F =	fun() ->
                case mnesia:read({privacy, {LUser, LServer}}) of
                    [] ->
                        ok;
                    [#privacy{default = Default, lists = Lists} = P] ->
                        %% TODO: check active
                        if
                            Name == Default ->
                                conflict;
                            true ->
                                NewLists =
                                    lists:keydelete(Name, 1, Lists),
                                mnesia:write(
                                  P#privacy{lists = NewLists})
                        end
                end
        end,
    mnesia:transaction(F);
remove_privacy_list(LUser, LServer, Name, odbc) ->
    F =	fun() ->
                case sql_get_default_privacy_list_t(LUser) of
                    {selected, ["name"], []} ->
                        sql_remove_privacy_list(LUser, Name),
                        ok;
                    {selected, ["name"], [{Default}]} ->
                        %% TODO: check active
                        if
                            Name == Default ->
                                conflict;
                            true ->
                                sql_remove_privacy_list(LUser, Name),
                                ok
                        end
                end
        end,
    odbc_queries:sql_transaction(LServer, F).

set_privacy_list(LUser, LServer, Name, List, mnesia) ->
    F =	fun() ->
                case mnesia:wread({privacy, {LUser, LServer}}) of
                    [] ->
                        NewLists = [{Name, List}],
                        mnesia:write(#privacy{us = {LUser, LServer},
                                              lists = NewLists});
                    [#privacy{lists = Lists} = P] ->
                        NewLists1 = lists:keydelete(Name, 1, Lists),
                        NewLists = [{Name, List} | NewLists1],
                        mnesia:write(P#privacy{lists = NewLists})
		end
        end,
    mnesia:transaction(F);
set_privacy_list(LUser, LServer, Name, List, odbc) ->
    RItems = lists:map(fun item_to_raw/1, List),
    F =	fun() ->
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
                ok
        end,
    odbc_queries:sql_transaction(LServer, F).

process_list_set(LUser, LServer, {value, Name}, Els) ->
    case parse_items(Els) of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	remove ->
	    case remove_privacy_list(LUser, LServer, Name,
                                     gen_mod:db_type(LServer, ?MODULE)) of
		{atomic, conflict} ->
                    {error, ?ERR_CONFLICT};
		{atomic, ok} ->
		    ejabberd_router:route(
		      jlib:make_jid(LUser, LServer, ""),
		      jlib:make_jid(LUser, LServer, ""),
		      {xmlelement, "broadcast", [],
		       [{privacy_list,
			 #userlist{name = Name, list = []},
			 Name}]}),
                    {result, []};
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end;
	List ->
	    case set_privacy_list(LUser, LServer, Name, List,
                                  gen_mod:db_type(LServer, ?MODULE)) of
		{atomic, ok} ->
		    NeedDb = is_list_needdb(List),
		    ejabberd_router:route(
		      jlib:make_jid(LUser, LServer, ""),
		      jlib:make_jid(LUser, LServer, ""),
		      {xmlelement, "broadcast", [],
		       [{privacy_list,
			 #userlist{name = Name, list = List, needdb = NeedDb},
			 Name}]}),
		    {result, []};
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
    %% Sort the items by their 'order' attribute
    lists:keysort(#listitem.order, Res);
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







is_list_needdb(Items) ->
    lists:any(
      fun(X) ->
	      case X#listitem.type of
		  subscription -> true;
		  group -> true;
		  _ -> false
	      end
      end, Items).

get_user_list(Acc, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    {Default, Items} = get_user_list(Acc, LUser, LServer,
                                     gen_mod:db_type(LServer, ?MODULE)),
    NeedDb = is_list_needdb(Items),
    #userlist{name = Default, list = Items, needdb = NeedDb}.

get_user_list(_, LUser, LServer, mnesia) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	[] ->
            {none, []};
	[#privacy{default = Default, lists = Lists}] ->
	    case Default of
		none ->
                    {none, []};
		_ ->
		    case lists:keysearch(Default, 1, Lists) of
			{value, {_, List}} ->
                            {Default, List};
			_ ->
                            {none, []}
		    end
	    end;
	_ ->
            {none, []}
    end;
get_user_list(_, LUser, LServer, odbc) ->
    case catch sql_get_default_privacy_list(LUser, LServer) of
	{selected, ["name"], []} ->
            {none, []};
	{selected, ["name"], [{Default}]} ->
	    case catch sql_get_privacy_list_data(LUser, LServer, Default) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    {Default, lists:map(fun raw_to_item/1, RItems)};
		_ ->
                    {none, []}
	    end;
	_ ->
            {none, []}
    end.

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
check_packet(_, _User, _Server,
	     _UserList,
	     {#jid{luser = "", lserver = Server} = _From,
              #jid{lserver = Server} = _To,
              _},
	     in) ->
    allow;
check_packet(_, _User, _Server,
	     _UserList,
	     {#jid{lserver = Server} = _From,
              #jid{luser = "", lserver = Server} = _To,
              _},
	     out) ->
    allow;
check_packet(_, _User, _Server,
	     _UserList,
	     {#jid{luser = User, lserver = Server} = _From,
              #jid{luser = User, lserver = Server} = _To,
              _},
	     _Dir) ->
    allow;
check_packet(_, User, Server,
	     #userlist{list = List, needdb = NeedDb},
	     {From, To, {xmlelement, PName, Attrs, _}},
	     Dir) ->
    case List of
	[] ->
	    allow;
	_ ->
	    PType = case PName of
			"message" -> message;
			"iq" -> iq;
			"presence" ->
			    case xml:get_attr_s("type", Attrs) of
				%% notification
				"" -> presence;
				"unavailable" -> presence;
				%% subscribe, subscribed, unsubscribe,
				%% unsubscribed, error, probe, or other
				_ -> other
			    end
		    end,
	    PType2 = case {PType, Dir} of
			 {message, in} -> message;
			 {iq, in} -> iq;
			 {presence, in} -> presence_in;
			 {presence, out} -> presence_out;
			 {_, _} -> other
		     end,
	    LJID = case Dir of
		       in -> jlib:jid_tolower(From);
		       out -> jlib:jid_tolower(To)
		   end,
	    {Subscription, Groups} =
		case NeedDb of
		    true -> ejabberd_hooks:run_fold(roster_get_jid_info,
						    jlib:nameprep(Server),
						    {none, []},
						    [User, Server, LJID]);
		    false -> {[], []}
		end,
	    check_packet_aux(List, PType2, LJID, Subscription, Groups)
    end.

%% Ptype = mesage | iq | presence_in | presence_out | other
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
		    Item#listitem.match_presence_out;
		other ->
		    false
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


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    remove_user(LUser, LServer, gen_mod:db_type(LServer, ?MODULE)).

remove_user(LUser, LServer, mnesia) ->
    F = fun() ->
		mnesia:delete({privacy,
			       {LUser, LServer}})
        end,
    mnesia:transaction(F);
remove_user(LUser, LServer, odbc) ->
    sql_del_privacy_lists(LUser, LServer).

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
    MatchAll = ejabberd_odbc:to_bool(SMatchAll),
    MatchIQ = ejabberd_odbc:to_bool(SMatchIQ),
    MatchMessage = ejabberd_odbc:to_bool(SMatchMessage),
    MatchPresenceIn = ejabberd_odbc:to_bool(SMatchPresenceIn),
    MatchPresenceOut =  ejabberd_odbc:to_bool(SMatchPresenceOut),
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
		{"j", ejabberd_odbc:escape(jlib:jid_to_string(Value))};
	    group ->
		{"g", ejabberd_odbc:escape(Value)};
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
    [SType, SValue, SAction, SOrder, SMatchAll, SMatchIQ,
     SMatchMessage, SMatchPresenceIn, SMatchPresenceOut].

sql_get_default_privacy_list(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:get_default_privacy_list(LServer, Username).

sql_get_default_privacy_list_t(LUser) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:get_default_privacy_list_t(Username).

sql_get_privacy_list_names(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:get_privacy_list_names(LServer, Username).

sql_get_privacy_list_names_t(LUser) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:get_privacy_list_names_t(Username).

sql_get_privacy_list_id(LUser, LServer, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:get_privacy_list_id(LServer, Username, SName).

sql_get_privacy_list_id_t(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:get_privacy_list_id_t(Username, SName).

sql_get_privacy_list_data(LUser, LServer, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:get_privacy_list_data(LServer, Username, SName).

sql_get_privacy_list_data_by_id(ID, LServer) ->
    odbc_queries:get_privacy_list_data_by_id(LServer, ID).

sql_get_privacy_list_data_by_id_t(ID) ->
    odbc_queries:get_privacy_list_data_by_id_t(ID).

sql_set_default_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:set_default_privacy_list(Username, SName).

sql_unset_default_privacy_list(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:unset_default_privacy_list(LServer, Username).

sql_remove_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:remove_privacy_list(Username, SName).

sql_add_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:add_privacy_list(Username, SName).

sql_set_privacy_list(ID, RItems) ->
    odbc_queries:set_privacy_list(ID, RItems).

sql_del_privacy_lists(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    Server = ejabberd_odbc:escape(LServer),
    odbc_queries:del_privacy_lists(LServer, Server, Username).

update_table() ->
    Fields = record_info(fields, privacy),
    case mnesia:table_info(privacy, attributes) of
	Fields ->
	    ok;
	[user, default, lists] ->
	    ?INFO_MSG("Converting privacy table from "
		      "{user, default, lists} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_privacy_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, privacy},
			      {attributes, record_info(fields, privacy)}]),
	    mnesia:transform_table(privacy, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_privacy_tmp_table),
			 mnesia:foldl(
			   fun(#privacy{us = U} = R, _) ->
				   mnesia:dirty_write(
				     mod_privacy_tmp_table,
				     R#privacy{us = {U, Host}})
			   end, ok, privacy)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(privacy),
	    F2 = fun() ->
			 mnesia:write_lock_table(privacy),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_privacy_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_privacy_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating privacy table", []),
	    mnesia:transform_table(privacy, ignore, Fields)
    end.


