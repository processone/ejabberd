%%%----------------------------------------------------------------------
%%% File    : mod_pubsub.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Pub/sub support (JEP-0060)
%%% Created :  4 Jul 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_pubsub).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 init/3,
	 loop/2,
	 stop/0,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DICT, dict).
-define(MAXITEMS, 10).

-record(pubsub_node, {node, parent, info}).
-record(nodeinfo, {items = [],
		   options = [],
		   entities = ?DICT:new()
		  }).
-record(entity, {affiliation = none,
		 subscription = none}).
-record(item, {id, publisher, payload}).


start(Opts) ->
    mnesia:create_table(pubsub_node,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, pubsub_node)}]),
    mnesia:add_table_index(pubsub_node, parent),
    Host = gen_mod:get_opt(host, Opts, "pubsub." ++ ?MYNAME),
    ServedHosts = gen_mod:get_opt(served_hosts, Opts, [?MYNAME]),
    register(ejabberd_mod_pubsub,
	     proc_lib:spawn_link(?MODULE, init, [Host, ServedHosts, self()])).



init(Host, ServedHosts, Parent) ->
    ejabberd_router:register_route(Host),
    create_new_node(Host, ["pubsub"], {"", Host, ""}),
    create_new_node(Host, ["pubsub", "nodes"], {"", Host, ""}),
    create_new_node(Host, ["home"], {"", Host, ""}),
    lists:foreach(fun(H) ->
			  create_new_node(Host, ["home", H], {"", Host, ""})
		  end, ServedHosts),
    loop(Host, Parent).

loop(Host, Parent) ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(Host, From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Host, Parent);
	{room_destroyed, Room} ->
	    ets:delete(muc_online_room, Room),
	    loop(Host, Parent);
	stop ->
	    ejabberd_router:unregister_global_route(Host),
	    ok;
	reload ->
	    ?MODULE:loop(Host, Parent);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], Host);
	_ ->
	    loop(Host, Parent)
    end.


do_route(Host, From, To, Packet) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    case To of
	{"", _, ""} ->
	    case Name of
		"iq" ->
		    case jlib:iq_query_info(Packet) of
			{iq, ID, get, ?NS_DISCO_INFO = XMLNS, SubEl} ->
			    {xmlelement, _, QAttrs, _} = SubEl,
			    Node = xml:get_attr_s("node", QAttrs),
			    Res = {iq, ID, result, XMLNS,
				   [{xmlelement, "query",
				     QAttrs,
				     iq_disco_info(Node)}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			{iq, ID, get, ?NS_DISCO_ITEMS = XMLNS, SubEl} ->
			    {xmlelement, _, QAttrs, _} = SubEl,
			    Node = xml:get_attr_s("node", QAttrs),
			    Res =
				case iq_disco_items(Host, From, Node) of
				    {result, IQRes} ->
					jlib:iq_to_xml(
					  {iq, ID, result, XMLNS,
					   [{xmlelement, "query",
					     QAttrs,
					     IQRes}]});
				    {error, Error} ->
					jlib:make_error_reply(
					  Packet, Error)
				end,
			    ejabberd_router:route(To,
						  From,
						  Res);
			%{iq, ID, get, ?NS_REGISTER = XMLNS, SubEl} ->
			%    Lang = xml:get_tag_attr_s(
			%	     "xml:lang", SubEl),
			%    Res = {iq, ID, result, XMLNS,
			%	   [{xmlelement, "query",
			%	     [{"xmlns", XMLNS}],
			%	     iq_get_register_info(
			%	       From, Lang)}]},
			%    ejabberd_router:route(To,
			%			  From,
			%			  jlib:iq_to_xml(Res));
			%{iq, ID, set, ?NS_REGISTER = XMLNS, SubEl} ->
			%    case process_iq_register_set(From, SubEl) of
			%	{result, IQRes} ->
			%	    Res = {iq, ID, result, XMLNS,
			%		   [{xmlelement, "query",
			%		     [{"xmlns", XMLNS}],
			%		     IQRes}]},
			%	    ejabberd_router:route(
			%	      To, From, jlib:iq_to_xml(Res));
			%	{error, Error} ->
			%	    Err = jlib:make_error_reply(
			%		    Packet, Error),
			%	    ejabberd_router:route(
			%	      To, From, Err)
			%    end;
			{iq, ID, Type, ?NS_PUBSUB = XMLNS, SubEl} ->
			    Res =
				case iq_pubsub(Host, From, Type, SubEl) of
				    {result, IQRes} ->
					jlib:iq_to_xml(
					  {iq, ID, result, XMLNS,
					   IQRes});
				    {error, Error} ->
					jlib:make_error_reply(
					  Packet, Error)
				end,
			    ejabberd_router:route(To,
						  From,
						  Res);
			{iq, ID, get, ?NS_VCARD = XMLNS, SubEl} ->
			    Lang = xml:get_tag_attr_s(
				     "xml:lang", SubEl),
			    Res = {iq, ID, result, XMLNS,
				   [{xmlelement, "query",
				     [{"xmlns", XMLNS}],
				     iq_get_vcard(Lang)}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			reply ->
			    ok;
			_ ->
			    Err = jlib:make_error_reply(
				    Packet,
				    ?ERR_FEATURE_NOT_IMPLEMENTED),
			    ejabberd_router:route(To, From, Err)
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    case xml:get_attr_s("type", Attrs) of
		"error" ->
		    ok;
		"result" ->
		    ok;
		_ ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_ITEM_NOT_FOUND),
		    ejabberd_router:route(To, From, Err)
	    end
    end.




stop() ->
    ejabberd_mod_pubsub ! stop,
    ok.



node_to_string(Node) ->
    string:strip(lists:flatten(lists:map(fun(S) -> [S, "/"] end, Node)),
		 right, $/).


iq_disco_info(SNode) ->
    Node = string:tokens(SNode, "/"),
    case Node of
	[] ->
	    [{xmlelement, "identity",
	      [{"category", "service"},
	       {"type", "pubsub"},
	       {"name", "ejabberd/mod_pubsub"}], []},
	     %{xmlelement, "feature", [{"var", ?NS_REGISTER}], []},
	     {xmlelement, "feature", [{"var", ?NS_PUBSUB}], []},
	     {xmlelement, "feature", [{"var", ?NS_PUBSUB_EVENT}], []},
	     {xmlelement, "feature", [{"var", ?NS_PUBSUB_OWNER}], []},
	     {xmlelement, "feature", [{"var", ?NS_VCARD}], []}];
	_ ->
	    % TODO
	    []
    end.


iq_disco_items(Host, From, SNode) ->
    Node = string:tokens(SNode, "/"),
    F = fun() ->
		case mnesia:read({pubsub_node, Node}) of
		    [#pubsub_node{info = Info}] ->
			SubNodes = mnesia:index_read(pubsub_node,
						     Node,
						     #pubsub_node.parent),
			SubItems =
			    lists:map(fun(#pubsub_node{node = N}) ->
					      SN = node_to_string(N),
					      {xmlelement, "item",
					       [{"jid", Host},
						{"node", SN},
						{"name", lists:last(N)}], []}
				      end, SubNodes),
			SN = node_to_string(Node),
			Items =
			    lists:map(fun(#item{id = Name}) ->
					      {xmlelement, "item",
					       [{"jid", Host},
						{"node", SN ++ "!" ++ Name},
						{"name", Name}], []}
				      end, Info#nodeinfo.items),
			SubItems ++ Items;
		    [] ->
			case Node of
			    [] ->
				SubNodes = mnesia:index_read(
					     pubsub_node,
					     Node,
					     #pubsub_node.parent),
				lists:map(
				  fun(#pubsub_node{node = N}) ->
					  SN = node_to_string(N),
					  {xmlelement, "item",
					   [{"jid", Host},
					    {"node", SN},
					    {"name", lists:last(N)}],
					   []}
				  end, SubNodes) ;
			    _ ->
				{error, ?ERR_ITEM_NOT_FOUND}
			end
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, Res} ->
	    {result, Res};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.


% TODO
%-define(XFIELD(Type, Label, Var, Val),
%	{xmlelement, "field", [{"type", Type},
%			       {"label", translate:translate(Lang, Label)},
%			       {"var", Var}],
%	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).
%
%iq_get_register_info(From, Lang) ->
%    {LUser, LServer, _} = jlib:jid_tolower(From),
%    LUS = {LUser, LServer},
%    Nick = case catch mnesia:dirty_read(muc_registered, LUS) of
%	       {'EXIT', Reason} ->
%		   "";
%	       [] ->
%		   "";
%	       [#muc_registered{nick = N}] ->
%		   N
%	   end,
%    [{xmlelement, "instructions", [],
%      [{xmlcdata, translate:translate(
%		    Lang, "You need a x:data capable client to register.")}]},
%     {xmlelement, "x",
%      [{"xmlns", ?NS_XDATA}],
%      [{xmlelement, "title", [],
%	[{xmlcdata,
%	  translate:translate(
%	    Lang, "Nick Registration")}]},
%       {xmlelement, "instructions", [],
%	[{xmlcdata,
%	  translate:translate(
%	    Lang, "Enter nick you want to register.")}]},
%       ?XFIELD("text-single", "Nick", "nick", Nick)]}].
%
%iq_set_register_info(From, XData) ->
%    {LUser, LServer, _} = jlib:jid_tolower(From),
%    LUS = {LUser, LServer},
%    case lists:keysearch("nick", 1, XData) of
%	false ->
%	    {error, ?ERR_BAD_REQUEST};
%	{value, {_, [Nick]}} ->
%	    F = fun() ->
%			case Nick of
%			    "" ->
%				mnesia:delete({muc_registered, LUS}),
%				ok;
%			    _ ->
%				Allow = case mnesia:index_read(
%					       muc_registered,
%					       Nick,
%					       #muc_registered.nick) of
%					    [] ->
%						true;
%					    [#muc_registered{user = U}] ->
%						U == LUS
%					end,
%				if
%				    Allow ->
%					mnesia:write(
%					  #muc_registered{user = LUS,
%							  nick = Nick}),
%					ok;
%				    true ->
%					false
%				end
%			end
%		end,
%	    case mnesia:transaction(F) of
%		{atomic, ok} ->
%		    {result, []};
%		{atomic, false} ->
%		    {error, ?ERR_NOT_ALLOWED};
%		_ ->
%		    {error, ?ERR_INTERNAL_SERVER_ERROR}
%	    end
%    end.
%
%process_iq_register_set(From, SubEl) ->
%    {xmlelement, Name, Attrs, Els} = SubEl,
%    case xml:remove_cdata(Els) of
%	[{xmlelement, "x", Attrs1, Els1} = XEl] ->
%	    case {xml:get_tag_attr_s("xmlns", XEl),
%		  xml:get_tag_attr_s("type", XEl)} of
%		{?NS_XDATA, "cancel"} ->
%		    {result, []};
%		{?NS_XDATA, "submit"} ->
%		    XData = jlib:parse_xdata_submit(XEl),
%		    case XData of
%			invalid ->
%			    {error, ?ERR_BAD_REQUEST};
%			_ ->
%			    iq_set_register_info(From, XData)
%		    end;
%		_ ->
%		    {error, ?ERR_BAD_REQUEST}
%	    end;
%	_ ->
%	    {error, ?ERR_BAD_REQUEST}
%    end.

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_pubsub"}]},
     {xmlelement, "URL", [],
      [{xmlcdata,
	"http://ejabberd.jabberstudio.org/"}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, "ejabberd pub/sub module\n"
	"Copyright (c) 2003 Alexey Shchepin"}]}].


iq_pubsub(Host, From, Type, SubEl) ->
    {xmlelement, _, _, SubEls} = SubEl,
    case xml:remove_cdata(SubEls) of
	[{xmlelement, Name, Attrs, Els}] ->
	    SNode = xml:get_attr_s("node", Attrs),
	    Node = string:tokens(SNode, "/"),
	    case {Type, Name} of
		{set, "create"} ->
		    create_new_node(Host, Node, From);
		{set, "publish"} ->
		    case xml:remove_cdata(Els) of
			[{xmlelement, "item", ItemAttrs, Payload}] ->
			    ItemID = xml:get_attr_s("id", ItemAttrs),
			    publish_item(Host, From, Node, ItemID, Payload);
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		{set, "retract"} ->
		    case xml:remove_cdata(Els) of
			[{xmlelement, "item", ItemAttrs, _}] ->
			    ItemID = xml:get_attr_s("id", ItemAttrs),
			    delete_item(Host, From, Node, ItemID);
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		{set, "subscribe"} ->
		    JID = xml:get_attr_s("jid", Attrs),
		    subscribe_node(Host, From, JID, Node);
		{set, "unsubscribe"} ->
		    JID = xml:get_attr_s("jid", Attrs),
		    unsubscribe_node(Host, From, JID, Node);
		{get, "items"} ->
		    MaxItems = xml:get_attr_s("max_items", Attrs),
		    get_items(Host, From, Node, MaxItems);
		{set, "delete"} ->
		    delete_node(Host, From, Node);
		{set, "purge"} ->
		    purge_node(Host, From, Node);
		{get, "entities"} ->
		    get_entities(From, Node);
		{set, "entities"} ->
		    set_entities(From, Node, xml:remove_cdata(Els));
		%{get, "configure"} ->
		%    get_node_config(From, Node);
		_ ->
		    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	    end;
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end.


-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).


create_new_node(Host, Node, Owner) ->
    case Node of
	[] ->
	    {LOU, LOS, _} = jlib:jid_tolower(Owner),
	    HomeNode = ["home", LOS, LOU],
	    create_new_node(Host, HomeNode, Owner),
	    NewNode = ["home", LOS, LOU, randoms:get_string()],
	    create_new_node(Host, NewNode, Owner);
	_ ->
	    LOwner = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
	    Parent = lists:sublist(Node, length(Node) - 1),
	    F = fun() ->
			ParentExists = (Parent == []) orelse
			    case mnesia:read({pubsub_node, Parent}) of
				[_] ->
				    true;
				[] ->
				    false
			    end,
			case ParentExists of
			    false ->
				{error, ?ERR_CONFLICT};
			    _ ->
				case mnesia:read({pubsub_node, Node}) of
				    [_] ->
					{error, ?ERR_CONFLICT};
				    [] ->
					Entities =
					    ?DICT:store(
					       LOwner,
					       #entity{affiliation = owner,
						       subscription = none},
					       ?DICT:new()),
					mnesia:write(
					  #pubsub_node{node = Node,
						       parent = Parent,
						       info = #nodeinfo{
							 entities = Entities}}),
					ok
				end
			end
		end,
	    case check_create_permission(Host, Node, Owner) of
		true ->
		    case mnesia:transaction(F) of
			{atomic, ok} ->
			    Lang = "",
			    broadcast_publish_item(
			      Host, ["pubsub", "nodes"], node_to_string(Node),
			      [{xmlelement, "x",
				[{"xmlns", ?NS_XDATA},
				 {"type", "result"}],
				[?XFIELD("hidden", "", "FORM_TYPE",
					 ?NS_PUBSUB_NMI),
				 ?XFIELD("jid-single", "Node Creator",
					 "creator",
					 jlib:jid_to_string(LOwner))]}]),
			    {result,
			     [{xmlelement, "pubsub",
				[{"xmlns", ?NS_PUBSUB}],
				[{xmlelement, "create",
				  [{"node", node_to_string(Node)}], []}]}]};
			{atomic, {error, _} = Error} ->
			    Error;
			_ ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR}
		    end;
		_ ->
		    {error, ?ERR_NOT_ALLOWED}
	    end
    end.


publish_item(Host, JID, Node, ItemID, Payload) ->
    Publisher = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    F = fun() ->
		case mnesia:read({pubsub_node, Node}) of
		    [#pubsub_node{info = Info} = N] ->
			Affiliation = get_affiliation(Info, Publisher),
			if
			    (Affiliation == owner) or
			    (Affiliation == publisher) ->
				NewInfo =
				    insert_item(Info, ItemID,
						Publisher, Payload),
				mnesia:write(
				  N#pubsub_node{info = NewInfo}),
				{result, []};
			    true ->
				{error, ?ERR_NOT_ALLOWED}
			end;
		    [] ->
			{error, ?ERR_ITEM_NOT_FOUND}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, Res}} ->
	    broadcast_publish_item(Host, Node, ItemID, Payload),
	    {result, Res};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.


delete_item(Host, JID, Node, ItemID) ->
    Publisher = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    F = fun() ->
		case mnesia:read({pubsub_node, Node}) of
		    [#pubsub_node{info = Info} = N] ->
			case check_item_publisher(Info, ItemID, Publisher)
			    orelse
			    (get_affiliation(Info, Publisher) == owner) of
			    true ->
				NewInfo =
				    remove_item(Info, ItemID),
				mnesia:write(
				  N#pubsub_node{info = NewInfo}),
				{result, []};
			    _ ->
				{error, ?ERR_NOT_ALLOWED}
			end;
		    [] ->
			{error, ?ERR_ITEM_NOT_FOUND}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, Res}} ->
	    broadcast_retract_item(Host, Node, ItemID),
	    {result, Res};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.


subscribe_node(Host, From, JID, Node) ->
    Sender = jlib:jid_tolower(jlib:jid_remove_resource(From)),
    SubscriberJID =
	case jlib:string_to_jid(JID) of
	    error ->
		{"", "", ""};
	    J ->
		J
	end,
    Subscriber = jlib:jid_tolower(SubscriberJID),
    F = fun() ->
		case mnesia:read({pubsub_node, Node}) of
		    [#pubsub_node{info = Info} = N] ->
			Affiliation = get_affiliation(Info, Subscriber),
			if
			    Affiliation /= outcast ->
				NewInfo =
				    add_subscriber(Info, Subscriber),
				mnesia:write(
				  N#pubsub_node{info = NewInfo}),
				{result, []};
			    true ->
				{error, ?ERR_NOT_ALLOWED}
			end;
		    [] ->
			{error, ?ERR_ITEM_NOT_FOUND}
		end
	end,
    if
	Sender == Subscriber ->
	    case mnesia:transaction(F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, Res}} ->
		    {result, Res};
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end;
	true ->
	    {error, ?ERR_NOT_ALLOWED}
    end.


unsubscribe_node(Host, From, JID, Node) ->
    Sender = jlib:jid_tolower(jlib:jid_remove_resource(From)),
    SubscriberJID =
	case jlib:string_to_jid(JID) of
	    error ->
		{"", "", ""};
	    J ->
		J
	end,
    Subscriber = jlib:jid_tolower(SubscriberJID),
    F = fun() ->
		case mnesia:read({pubsub_node, Node}) of
		    [#pubsub_node{info = Info} = N] ->
			Subscription = get_subscription(Info, Subscriber),
			if
			    Subscription /= none ->
				NewInfo =
				    remove_subscriber(Info, Subscriber),
				mnesia:write(
				  N#pubsub_node{info = NewInfo}),
				{result, []};
			    true ->
				{error, ?ERR_NOT_ALLOWED}
			end;
		    [] ->
			{error, ?ERR_ITEM_NOT_FOUND}
		end
	end,
    if
	Sender == Subscriber ->
	    case mnesia:transaction(F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, Res}} ->
		    {result, Res};
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end;
	true ->
	    {error, ?ERR_NOT_ALLOWED}
    end.


get_items(Host, JID, Node, SMaxItems) ->
    MaxItems =
	if
	    SMaxItems == "" ->
		?MAXITEMS;
	    true ->
		case catch list_to_integer(SMaxItems) of
		    {'EXIT', _} ->
			{error, ?ERR_BAD_REQUEST};
		    Val ->
			Val
		end
	end,
    case MaxItems of
	{error, _} = Error ->
	    Error;
	_ ->
	    case catch mnesia:dirty_read(pubsub_node, Node) of
		[#pubsub_node{info = Info}] ->
		    Items = lists:sublist(Info#nodeinfo.items, MaxItems),
		    ItemsEls =
			lists:map(
			  fun(#item{id = ItemID,
				    payload = Payload}) ->
				  ItemAttrs = case ItemID of
						  "" -> [];
						  _ -> [{"id", ItemID}]
					      end,
				  {xmlelement, "item", ItemAttrs, Payload}
			  end, Items),
		    {result, [{xmlelement, "pubsub",
			       [{"xmlns", ?NS_PUBSUB_EVENT}],
			       [{xmlelement, "items",
				 [{"node", node_to_string(Node)}],
				 ItemsEls}]}]};
		_ ->
		    {error, ?ERR_ITEM_NOT_FOUND}
	    end
    end.


delete_node(Host, JID, Node) ->
    Owner = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    F = fun() ->
		case mnesia:read({pubsub_node, Node}) of
		    [#pubsub_node{info = Info}] ->
			case get_affiliation(Info, Owner) of
			    owner ->
				% TODO: don't iterate over all table
				Removed =
				    mnesia:foldl(
				      fun(#pubsub_node{node = N,
						       info = #nodeinfo{
							 entities = Entities
							}}, Acc) ->
					      case lists:prefix(Node, N) of
						  true ->
						      [{N, Entities} | Acc];
						  _ ->
						      Acc
					      end
				      end, [], pubsub_node),
				lists:foreach(
				  fun({N, _}) ->
					  mnesia:delete({pubsub_node, N})
				  end, Removed),
				{removed, Removed};
			    _ ->
				{error, ?ERR_NOT_ALLOWED}
			end;
		    [] ->
			{error, ?ERR_ITEM_NOT_FOUND}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {removed, Removed}} ->
	    broadcast_removed_node(Host, Removed),
	    Lang = "",
	    broadcast_retract_item(
	      Host, ["pubsub", "nodes"], node_to_string(Node)),
	    {result, []};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.


purge_node(Host, JID, Node) ->
    Owner = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    F = fun() ->
		case mnesia:read({pubsub_node, Node}) of
		    [#pubsub_node{info = Info} = N] ->
			case get_affiliation(Info, Owner) of
			    owner ->
				NewInfo = Info#nodeinfo{items = []},
				mnesia:write(
				  N#pubsub_node{info = NewInfo}),
				{result, Info#nodeinfo.items, []};
			    _ ->
				{error, ?ERR_NOT_ALLOWED}
			end;
		    [] ->
			{error, ?ERR_ITEM_NOT_FOUND}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, Items, Res}} ->
	    lists:foreach(
	      fun(#item{id = ItemID}) ->
		      broadcast_retract_item(Host, Node, ItemID)
	      end, Items),
	    {result, Res};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.


get_entities(OJID, Node) ->
    Owner = jlib:jid_tolower(jlib:jid_remove_resource(OJID)),
    case catch mnesia:dirty_read(pubsub_node, Node) of
	[#pubsub_node{info = Info}] ->
	    case get_affiliation(Info, Owner) of
		owner ->
		    Entities = Info#nodeinfo.entities,
		    EntitiesEls =
			?DICT:fold(
			  fun(JID,
			      #entity{affiliation = Affiliation,
				      subscription = Subscription},
			      Acc) ->
				  [{xmlelement, "entity",
				    [{"jid", jlib:jid_to_string(JID)},
				     {"affiliation",
				      affiliation_to_string(Affiliation)},
				     {"subscription",
				      subscription_to_string(Subscription)}],
				    []} | Acc]
			  end, [], Entities),
		    {result, [{xmlelement, "pubsub",
			       [{"xmlns", ?NS_PUBSUB_EVENT}],
			       [{xmlelement, "entities",
				 [{"node", node_to_string(Node)}],
				 EntitiesEls}]}]};
		_ ->
		    {error, ?ERR_NOT_ALLOWED}
	    end;
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.


set_entities(OJID, Node, EntitiesEls) ->
    Owner = jlib:jid_tolower(jlib:jid_remove_resource(OJID)),
    Entities =
	lists:foldl(
	  fun(El, Acc) ->
		  case Acc of
		      error ->
			  error;
		      _ ->
			  case El of
			      {xmlelement, "entity", Attrs, _} ->
				  JID = jlib:string_to_jid(
					  xml:get_attr_s("jid", Attrs)),
				  Affiliation =
				      case xml:get_attr_s("affiliation",
							  Attrs) of
					  "owner" -> owner;
					  "publisher" -> publisher;
					  "outcast" -> outcast;
					  "none" -> none;
					  _ -> false
				      end,
				  Subscription =
				      case xml:get_attr_s("subscription",
							  Attrs) of
					  "subscribed" -> subscribed;
					  "pending" -> pending;
					  "unconfigured" -> unconfigured;
					  "none" -> none;
					  _ -> false
				      end,
				  if
				      (JID == error) or
				      (Affiliation == false) or
				      (Subscription == false) ->
					  error;
				      true ->
					  [{JID,
					    #entity{
					      affiliation = Affiliation,
					      subscription = Subscription}} |
					   Acc]
				  end
			  end
		  end
	  end, [], EntitiesEls),
    case Entities of
	error ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    F = fun() ->
			case mnesia:read({pubsub_node, Node}) of
			    [#pubsub_node{info = Info} = N] ->
				case get_affiliation(Info, Owner) of
				    owner ->
					NewInfo =
					    set_info_entities(Info, Entities),
					mnesia:write(
					  N#pubsub_node{info = NewInfo}),
					{result, []};
				    _ ->
					{error, ?ERR_NOT_ALLOWED}
				end;
			    [] ->
				{error, ?ERR_ITEM_NOT_FOUND}
			end
		end,
	    case mnesia:transaction(F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _}} ->
		    {result, []};
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end
    end.


%get_node_config(OJID, Node) ->
%    Owner = jlib:jid_tolower(jlib:jid_remove_resource(OJID)),
%    case catch mnesia:dirty_read(pubsub_node, Node) of
%	[#pubsub_node{info = Info}] ->
%	    case get_affiliation(Info, Owner) of
%		owner ->
%		    Entities = Info#nodeinfo.entities,
%		    EntitiesEls =
%			?DICT:fold(
%			  fun(JID,
%			      #entity{affiliation = Affiliation,
%				      subscription = Subscription},
%			      Acc) ->
%				  [{xmlelement, "entity",
%				    [{"jid", jlib:jid_to_string(JID)},
%				     {"affiliation",
%				      affiliation_to_string(Affiliation)},
%				     {"subscription",
%				      subscription_to_string(Subscription)}],
%				    []} | Acc]
%			  end, [], Entities),
%		    {result, [{xmlelement, "pubsub",
%			       [{"xmlns", ?NS_PUBSUB_EVENT}],
%			       [{xmlelement, "entities",
%				 [{"node", node_to_string(Node)}],
%				 EntitiesEls}]}]};
%		_ ->
%		    {error, ?ERR_NOT_ALLOWED}
%	    end;
%	_ ->
%	    {error, ?ERR_ITEM_NOT_FOUND}
%    end.







get_affiliation(#nodeinfo{entities = Entities}, JID) ->
    LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    case ?DICT:find(LJID, Entities) of
	{ok, #entity{affiliation = Affiliation}} ->
	    Affiliation;
	_ ->
	    none
    end.

get_subscription(#nodeinfo{entities = Entities}, JID) ->
    LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    case ?DICT:find(LJID, Entities) of
	{ok, #entity{subscription = Subscription}} ->
	    Subscription;
	_ ->
	    none
    end.

affiliation_to_string(Affiliation) ->
    case Affiliation of
	owner -> "owner";
	publisher -> "publisher";
	outcast -> "outcast";
	_ -> "none"
    end.

subscription_to_string(Subscription) ->
    case Subscription of
	subscribed -> "subscribed";
	pending -> "pending";
	unconfigured -> "unconfigured";
	_ -> "none"
    end.


check_create_permission(Host, Node, Owner) ->
    if
	{"", Host, ""} == Owner ->
	    true;
	true ->
	    {User, Server, _} = Owner,
	    case Node of
		["home", Server, User | _] ->
		    true;
		_ ->
		    false
	    end
    end.

insert_item(Info, ItemID, Publisher, Payload) ->
    Items = Info#nodeinfo.items,
    Items1 = lists:filter(fun(I) ->
				  I#item.id /= ItemID
			  end, Items),
    Items2 = [#item{id = ItemID, publisher = Publisher, payload = Payload} |
	      Items1],
    Items3 = lists:sublist(Items2, ?MAXITEMS),
    Info#nodeinfo{items = Items3}.

remove_item(Info, ItemID) ->
    Items = Info#nodeinfo.items,
    Items1 = lists:filter(fun(I) ->
				  I#item.id /= ItemID
			  end, Items),
    Info#nodeinfo{items = Items1}.

check_item_publisher(Info, ItemID, Publisher) ->
    Items = Info#nodeinfo.items,
    case lists:keysearch(ItemID, #item.id, Items) of
	{value, #item{publisher = Publisher}} ->
	    true;
	_ ->
	    false
    end.

add_subscriber(Info, Subscriber) ->
    Entities = Info#nodeinfo.entities,
    case ?DICT:find(Subscriber, Entities) of
	{ok, Entity} ->
	    Info#nodeinfo{
	      entities = ?DICT:store(Subscriber,
				     Entity#entity{subscription = subscribed},
				     Entities)};
	_ ->
	    Info#nodeinfo{
	      entities = ?DICT:store(Subscriber,
				     #entity{subscription = subscribed},
				     Entities)}
    end.

remove_subscriber(Info, Subscriber) ->
    Entities = Info#nodeinfo.entities,
    case ?DICT:find(Subscriber, Entities) of
	{ok, #entity{affiliation = none}} ->
	    Info#nodeinfo{
	      entities = ?DICT:erase(Subscriber, Entities)};
	{ok, Entity} ->
	    Info#nodeinfo{
	      entities = ?DICT:store(Subscriber,
				     Entity#entity{subscription = none},
				     Entities)};
	_ ->
	    Info
    end.


set_info_entities(Info, Entities) ->
    NewEntities =
	lists:foldl(
	  fun({JID, Ent}, Es) ->
		  case Ent of
		      #entity{affiliation = none, subscription = none} ->
			  ?DICT:erase(JID, Es);
		      _ ->
			  ?DICT:store(JID, Ent, Es)
		  end
	  end, Info#nodeinfo.entities, Entities),
    Info#nodeinfo{entities = NewEntities}.
				


broadcast_publish_item(Host, Node, ItemID, Payload) ->
    case catch mnesia:dirty_read(pubsub_node, Node) of
	[#pubsub_node{info = Info}] ->
	    ?DICT:fold(
	       fun(JID, #entity{subscription = Subscription}, _) ->
		       if 
			   (Subscription /= none) and
			   (Subscription /= pending) ->
			       ItemAttrs = case ItemID of
					       "" -> [];
					       _ -> [{"id", ItemID}]
					   end,
			       Stanza =
				   {xmlelement, "message", [],
				    [{xmlelement, "x",
				      [{"xmlns", ?NS_PUBSUB_EVENT}],
				      [{xmlelement, "items",
					[{"node", node_to_string(Node)}],
					[{xmlelement, "item",
					  ItemAttrs,
					  Payload}]}]}]},
			       ejabberd_router:route({"", Host, ""},
						     JID, Stanza);
			   true ->
			       ok
		       end
	       end, ok, Info#nodeinfo.entities);
	_ ->
	    false
    end.


broadcast_retract_item(Host, Node, ItemID) ->
    case catch mnesia:dirty_read(pubsub_node, Node) of
	[#pubsub_node{info = Info}] ->
	    ?DICT:fold(
	       fun(JID, #entity{subscription = Subscription}, _) ->
		       if 
			   (Subscription /= none) and
			   (Subscription /= pending) ->
			       ItemAttrs = case ItemID of
					       "" -> [];
					       _ -> [{"id", ItemID}]
					   end,
			       Stanza =
				   {xmlelement, "message", [],
				    [{xmlelement, "x",
				      [{"xmlns", ?NS_PUBSUB_EVENT}],
				      [{xmlelement, "items",
					[{"node", node_to_string(Node)}],
					[{xmlelement, "retract",
					 ItemAttrs, []}]}]}]},
			       ejabberd_router:route({"", Host, ""},
						     JID, Stanza);
			   true ->
			       ok
		       end
	       end, ok, Info#nodeinfo.entities);
	_ ->
	    false
    end.


broadcast_removed_node(Host, Removed) ->
    lists:foreach(
      fun({Node, Entities}) ->
	      ?DICT:fold(
		 fun(JID, #entity{subscription = Subscription}, _) ->
			 if 
			     (Subscription /= none) and
			     (Subscription /= pending) ->
				 Stanza =
				     {xmlelement, "message", [],
				      [{xmlelement, "x",
					[{"xmlns", ?NS_PUBSUB_EVENT}],
					[{xmlelement, "delete",
					  [{"node", node_to_string(Node)}],
					    []}]}]},
				 ejabberd_router:route({"", Host, ""},
						       JID, Stanza);
			     true ->
				 ok
			 end
		 end, ok, Entities)
      end, Removed).



system_continue(Parent, _, State) ->
    loop(State, Parent).

system_terminate(Reason, Parent, _, State) ->
    exit(Reason).

system_code_change(State, _Mod, Ver, _Extra) ->
    {ok, State}.
