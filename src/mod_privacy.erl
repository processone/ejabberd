%%%----------------------------------------------------------------------
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
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
	 updated_list/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_privacy.hrl").


start(Host, Opts) ->
    HostB = list_to_binary(Host),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(privacy, [{disc_copies, [node()]},
				  {attributes, record_info(fields, privacy)}]),
    update_table(),
    ejabberd_hooks:add(privacy_iq_get, HostB,
		       ?MODULE, process_iq_get, 50),
    ejabberd_hooks:add(privacy_iq_set, HostB,
		       ?MODULE, process_iq_set, 50),
    ejabberd_hooks:add(privacy_get_user_list, HostB,
		       ?MODULE, get_user_list, 50),
    ejabberd_hooks:add(privacy_check_packet, HostB,
		       ?MODULE, check_packet, 50),
    ejabberd_hooks:add(privacy_updated_list, HostB,
		       ?MODULE, updated_list, 50),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_PRIVACY,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(privacy_iq_get, HostB,
			  ?MODULE, process_iq_get, 50),
    ejabberd_hooks:delete(privacy_iq_set, HostB,
			  ?MODULE, process_iq_set, 50),
    ejabberd_hooks:delete(privacy_get_user_list, HostB,
			  ?MODULE, get_user_list, 50),
    ejabberd_hooks:delete(privacy_check_packet, HostB,
			  ?MODULE, check_packet, 50),
    ejabberd_hooks:delete(privacy_updated_list, HostB,
			  ?MODULE, updated_list, 50),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_PRIVACY).

process_iq(_From, _To, IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').


process_iq_get(_, From, _To, #iq{payload = SubEl},
	       #userlist{name = Active}) ->
    LUser = exmpp_jid:lnode_as_list(From),
    LServer = exmpp_jid:ldomain_as_list(From),
    case exmpp_xml:get_child_elements(SubEl) of
	[] ->
	    process_lists_get(LUser, LServer, Active);
	[#xmlel{name = Name} = Child] ->
	    case Name of
		list ->
		    ListName = exmpp_xml:get_attribute_as_list(Child, name, false),
		    process_list_get(LUser, LServer, ListName);
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
    end.


process_lists_get(LUser, LServer, Active) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	{'EXIT', _Reason} ->
	    {error, 'internal-server-error'};
	[] ->
	    {result, #xmlel{ns = ?NS_PRIVACY, name = 'query'}};
	[#privacy{default = Default, lists = Lists}] ->
	    case Lists of
		[] ->
		    {result, #xmlel{ns = ?NS_PRIVACY, name = 'query'}};
		_ ->
		    LItems = lists:map(
			       fun({N, _}) ->
			       exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = list}, name, N)
			       end, Lists),
		    DItems =
			case Default of
			    none ->
				LItems;
			    _ ->
				[exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = default}, name, Default) | LItems]
			end,
		    ADItems =
			case Active of
			    none ->
				DItems;
			    _ ->
				[exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = active}, name, Active) | DItems]
			end,
			{result, #xmlel{ns = ?NS_PRIVACY, name = 'query', children = ADItems}}
	    end
    end.

process_list_get(_LUser, _LServer, false) ->
    {error, 'bad-request'};

process_list_get(LUser, LServer, Name) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	{'EXIT', _Reason} ->
	    {error, 'internal-server-error'};
	[] ->
	    {error, 'item-not-found'};
	    %{result, [{xmlelement, "query", [{"xmlns", ?NS_PRIVACY}], []}]};
	[#privacy{lists = Lists}] ->
	    case lists:keysearch(Name, 1, Lists) of
		{value, {_, List}} ->
		    LItems = lists:map(fun item_to_xml/1, List),
		    ListEl = exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = list, children = LItems}, name, Name),
		    {result,#xmlel{ns = ?NS_PRIVACY, name = 'query', children = [ListEl]}};
		_ ->
		    {error, 'item-not-found'}
	    end
    end.


item_to_xml(Item) ->
    Attrs1 = [?XMLATTR('action', action_to_binary(Item#listitem.action)),
	      ?XMLATTR('order', order_to_binary(Item#listitem.order))],
    Attrs2 = case Item#listitem.type of
		 none ->
		     Attrs1;
		 Type ->
		     [?XMLATTR('type', type_to_binary(Item#listitem.type)),
		      ?XMLATTR('value', value_to_binary(Type, Item#listitem.value)) |
		      Attrs1]
	     end,
    SubEls = case Item#listitem.match_all of
		 true ->
		     [];
		 false ->
		     SE1 = case Item#listitem.match_iq of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = iq}];
			       false ->
				   []
			   end,
		     SE2 = case Item#listitem.match_message of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = message} | SE1];
			       false ->
				   SE1
			   end,
		     SE3 = case Item#listitem.match_presence_in of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = 'presence-in'} | SE2];
			       false ->
				   SE2
			   end,
		     SE4 = case Item#listitem.match_presence_out of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = 'presence-out'} | SE3];
			       false ->
				   SE3
			   end,
		     SE4
	     end,
    exmpp_xml:set_attributes(#xmlel{ns = ?NS_PRIVACY, name = item, children = SubEls}, Attrs2).


action_to_binary(Action) ->
    case Action of
	allow -> <<"allow">>;
	deny -> <<"deny">>
    end.

order_to_binary(Order) ->
    list_to_binary(integer_to_list(Order)).

type_to_binary(Type) ->
    case Type of
	jid -> <<"jid">>;
	group -> <<"group">>;
	subscription -> <<"subscription">>
    end.

value_to_binary(Type, Val) ->
    case Type of
	jid ->
	    {N, D, R} = Val,
	    exmpp_jid:jid_to_binary(N, D, R);
	group -> Val;
	subscription ->
	    case Val of
		both -> <<"both">>;
		to -> <<"to">>;
		from -> <<"from">>;
		none -> <<"none">>
	    end
    end.



list_to_action(S) ->
    case S of
	"allow" -> allow;
	"deny" -> deny
    end.



process_iq_set(_, From, _To, #iq{payload = SubEl}) ->
    LUser = exmpp_jid:lnode_as_list(From),
    LServer = exmpp_jid:ldomain_as_list(From),
    case exmpp_xml:get_child_elements(SubEl) of
	[#xmlel{name = Name} = Child] ->
	    ListName = exmpp_xml:get_attribute_as_list(Child, 'name', false),
	    case Name of
		list ->
		    process_list_set(LUser, LServer, ListName,
				     exmpp_xml:get_child_elements(Child));
		active ->
		    process_active_set(LUser, LServer, ListName);
		default ->
		    process_default_set(LUser, LServer, ListName);
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
    end.


process_default_set(LUser, LServer, false) ->
    F = fun() ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
			{result, []};
		    [R] ->
			mnesia:write(R#privacy{default = none}),
			{result, []}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, _} = Res} ->
	    Res;
	_ ->
	    {error, 'internal-server-error'}
    end;

process_default_set(LUser, LServer, Name) ->
    F = fun() ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
			{error, 'item-not-found'};
		    [#privacy{lists = Lists} = P] ->
			case lists:keymember(Name, 1, Lists) of
			    true ->
				mnesia:write(P#privacy{default = Name,
						       lists = Lists}),
				{result, []};
			    false ->
				{error, 'item-not-found'}
			end
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, _} = Res} ->
	    Res;
	_ ->
	    {error, 'internal-server-error'}
    end.


process_active_set(_LUser, _LServer, false) ->
    {result, [], #userlist{}};

process_active_set(LUser, LServer, Name) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	[] ->
	    {error, 'item-not-found'};
	[#privacy{lists = Lists}] ->
	    case lists:keysearch(Name, 1, Lists) of
		{value, {_, List}} ->
		    {result, [], #userlist{name = Name, list = List}};
		false ->
		    {error, 'item-not-found'}
	    end
    end.



process_list_set(_LUser, _LServer, false, _Els) ->
    {error, 'bad-request'};

process_list_set(LUser, LServer, Name, Els) ->
    case parse_items(Els) of
	false ->
	    {error, 'bad-request'};
	remove ->
	    F =
		fun() ->
			case mnesia:read({privacy, {LUser, LServer}}) of
			    [] ->
				{result, []};
			    [#privacy{default = Default, lists = Lists} = P] ->
				% TODO: check active
				if
				    Name == Default ->
					{error, 'conflict'};
				    true ->
					NewLists =
					    lists:keydelete(Name, 1, Lists),
					mnesia:write(
					  P#privacy{lists = NewLists}),
					{result, []}
				end
			end
		end,
	    case mnesia:transaction(F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _} = Res} ->
		    ejabberd_router:route(
		      exmpp_jid:make_jid(LUser, LServer),
		      exmpp_jid:make_jid(LUser, LServer),
		      #xmlel{name = 'broadcast', 
			children=[{privacy_list,
				   #userlist{name = Name, list = []},
					     Name}]}),
		    Res;
		_ ->
		    {error, 'internal-server-error'}
	    end;
	List ->
	    F =
		fun() ->
			case mnesia:wread({privacy, {LUser, LServer}}) of
			    [] ->
				NewLists = [{Name, List}],
				mnesia:write(#privacy{us = {LUser, LServer},
						      lists = NewLists}),
				{result, []};
			    [#privacy{lists = Lists} = P] ->
				NewLists1 = lists:keydelete(Name, 1, Lists),
				NewLists = [{Name, List} | NewLists1],
				mnesia:write(P#privacy{lists = NewLists}),
				{result, []}
			end
		end,
	    case mnesia:transaction(F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _} = Res} ->
		    ejabberd_router:route(
		      exmpp_jid:make_jid(LUser, LServer),
		      exmpp_jid:make_jid(LUser, LServer),
		      #xmlel{name = 'broadcast', 
			children=[{privacy_list,
				   #userlist{name = Name, list = List},
					     Name}]}),
		    Res;
		_ ->
		    {error, 'internal_server_error'}
	    end
    end.



parse_items([]) ->
    remove;
parse_items(Els) ->
    parse_items(Els, []).

parse_items([], Res) ->
    %% Sort the items by their 'order' attribute
    %% 5 is the position of 'order' attribute in a #listitem tuple
    %% This integer can be calculated at runtime with:
    %% 2 + length(lists:takewhile(fun(E) -> E =/= order end,
    %% 			       record_info(fields, listitem))),
    lists:keysort(5, Res);
parse_items([El = #xmlel{name = item} | Els], Res) ->
    Type   = exmpp_xml:get_attribute_as_list(El, type, false),
    Value  = exmpp_xml:get_attribute_as_list(El, value, false),
    SAction =exmpp_xml:get_attribute_as_list(El, action, false),
    SOrder = exmpp_xml:get_attribute_as_list(El, order, false),
    Action = case catch list_to_action(SAction) of
		 {'EXIT', _} -> false;
		 Val -> Val
	     end,
    Order = case catch list_to_integer(SOrder) of
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
		     {T,  V} when is_list(T), is_list(V) ->
			 case T of
			     "jid" ->
				 try
				     JID = exmpp_jid:parse_jid(V),
				     I1#listitem{
				       type = jid,
				       value = jlib:short_prepd_jid(JID)}
				 catch
				     _ ->
					 false
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
		     {T, false} when is_list(T) ->
			 false;
		     _ ->
			 I1
		 end,
	    case I2 of
		false ->
		    false;
		_ ->
		    case parse_matches(I2, exmpp_xml:get_child_elements(El)) of
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
parse_matches1(Item, [#xmlel{name = message} | Els]) ->
    parse_matches1(Item#listitem{match_message = true}, Els);
parse_matches1(Item, [#xmlel{name = iq} | Els]) ->
    parse_matches1(Item#listitem{match_iq = true}, Els);
parse_matches1(Item, [#xmlel{name = 'presence-in'} | Els]) ->
    parse_matches1(Item#listitem{match_presence_in = true}, Els);
parse_matches1(Item, [#xmlel{name = 'presence-out'} | Els]) ->
    parse_matches1(Item#listitem{match_presence_out = true}, Els);
parse_matches1(_Item, [#xmlel{} | _Els]) ->
    false.







get_user_list(_, User, Server) 
        when is_binary(User), is_binary(Server) ->
    try
	LUser = binary_to_list(User),
	LServer = binary_to_list(Server),
	case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	    [] ->
		#userlist{};
	    [#privacy{default = Default, lists = Lists}] ->
		case Default of
		    none ->
			#userlist{};
		    _ ->
			case lists:keysearch(Default, 1, Lists) of
			    {value, {_, List}} ->
				SortedList = lists:keysort(#listitem.order, List),
				#userlist{name = Default, list = SortedList};
			    _ ->
				#userlist{}
			end
		end;
	    _ ->
		#userlist{}
	end
    catch
	_ ->
	    #userlist{}
    end.


check_packet(_, User, Server,
	     #userlist{list = List},
	     {From, To, #xmlel{name = PName}},
	     Dir) when 
               PName =:= message ; 
		       PName =:= iq ;
		       PName =:= presence ->
    case List of
	[] ->
	    allow;
	_ ->
	    case {PName, Dir} of
		{message, in} ->
		    LJID = jlib:short_prepd_jid(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, 
              Server, 
			  {none, []}, [User, Server, From]),
		    check_packet_aux(List, message,
				     LJID, Subscription, Groups);
		{iq, in} ->
		    LJID = jlib:short_prepd_jid(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, 
               Server,
			  {none, []}, [User, Server, From]),
		    check_packet_aux(List, iq,
				     LJID, Subscription, Groups);
		{presence, in} ->
		    LJID = jlib:short_prepd_jid(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, 
                Server,
			  {none, []}, [User, Server, From]),
		    check_packet_aux(List, presence_in,
				     LJID, Subscription, Groups);
		{presence, out} ->
		    LJID = jlib:short_prepd_jid(To),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, 
              Server,
			  {none, []}, [User, Server, To]),
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
		{undefined, Server, undefined} ->
		    case JID of
			{_, Server, _} ->
			    true;
			_ ->
			    false
		    end;
		{User, Server, undefined} ->
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
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    F = fun() ->
		mnesia:delete({privacy,
			       {LUser, LServer}})
        end,
    mnesia:transaction(F).


updated_list(_,
	     #userlist{name = OldName} = Old,
	     #userlist{name = NewName} = New) ->
    if
	OldName == NewName ->
	    New;
	true ->
	    Old
    end.


update_table() ->
    Fields = record_info(fields, privacy),
    case mnesia:table_info(privacy, attributes) of
	Fields ->
	    convert_to_exmpp();
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
			   fun(#privacy{us = U, lists = L} = R, _) ->
				   U1 = convert_jid_to_exmpp(U),
				   L1 = convert_lists_to_exmpp(L),
				   mnesia:dirty_write(
				     mod_privacy_tmp_table,
				     R#privacy{us = {U1, Host}, lists = L1})
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


convert_to_exmpp() ->
    Fun = fun() ->
	mnesia:foldl(fun convert_to_exmpp2/2, done, privacy, write)
    end,
    mnesia:transaction(Fun).

convert_to_exmpp2(#privacy{us = {U, S} = Key, lists = L} = P, Acc) ->
    U1 = convert_jid_to_exmpp(U),
    L1 = convert_lists_to_exmpp(L),
    New_P = P#privacy{
      us = {U1, S},
      lists = L1
    },
    if
	New_P /= P -> mnesia:delete({privacy, Key}), mnesia:write(New_P);
	true       -> ok
    end,
    Acc.

convert_jid_to_exmpp("") -> undefined;
convert_jid_to_exmpp(V)  -> V.

convert_lists_to_exmpp(L) ->
    convert_lists_to_exmpp2(L, []).

convert_lists_to_exmpp2([{Name, List} | Rest], Result) ->
    convert_lists_to_exmpp2(Rest,
      [{Name, convert_list_to_exmpp(List, [])} | Result]);
convert_lists_to_exmpp2([], Result) ->
    lists:reverse(Result).

convert_list_to_exmpp([#listitem{type = jid, value = {U, S, R}} = I | Rest],
  Result) ->
    U1 = convert_jid_to_exmpp(U),
    R1 = convert_jid_to_exmpp(R),
    New_I = I#listitem{value = {U1, S, R1}},
    convert_list_to_exmpp(Rest, [New_I | Result]);
convert_list_to_exmpp([], Result) ->
    lists:reverse(Result).
