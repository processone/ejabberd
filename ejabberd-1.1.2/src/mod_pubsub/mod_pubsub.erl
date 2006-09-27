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

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
	 start/2,
	 stop/1]).

-export([delete_item/3,
	 set_entities/4,
	 delete_node/2,
	 create_new_node/2,
	 subscribe_node/3,
	 get_node_config/4,
	 set_node_config/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {host, server_host, access}).

-define(DICT, dict).
-define(MAXITEMS, 20).
-define(MAX_PAYLOAD_SIZE, 100000).

-record(pubsub_node, {host_node, host_parent, info}).
-record(nodeinfo, {items = [],
		   options = [],
		   entities = ?DICT:new()
		  }).
-record(entity, {affiliation = none,
		 subscription = none}).
-record(item, {id, publisher, payload}).

-define(PROCNAME, ejabberd_mod_pubsub).
-define(MYJID, #jid{user = "", server = Host, resource = "",
		    luser = "", lserver = Host, lresource = ""}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 temporary,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:stop_child(ejabberd_sup, Proc).

delete_item(From, Node, ItemID) ->
    delete_item(get_host(), From, Node, ItemID).

delete_node(From, Node) ->
    delete_node(get_host(), From, Node).

create_new_node(Node, From) ->
    create_new_node(get_host(), Node, From).

subscribe_node(From, JID, Node) ->
    subscribe_node(get_host(), From, JID, Node).

set_node_config(From, Node, Els, Lang) ->
    set_node_config(get_host(), From, Node, Els, Lang).

get_host() ->
    ejabberd_mod_pubsub ! {get_host, self()},
    receive
	{pubsub_host, Host} ->
	    Host
    after 5000 ->
	    timeout
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ServerHost, Opts]) ->
    mnesia:create_table(pubsub_node,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, pubsub_node)}]),
    Host = gen_mod:get_opt(host, Opts, "pubsub." ++ ServerHost),
    update_table(Host),
    mnesia:add_table_index(pubsub_node, host_parent),
    ServedHosts = gen_mod:get_opt(served_hosts, Opts, []),
    Access = gen_mod:get_opt(access_createnode, Opts, all),

    ejabberd_router:register_route(Host),
    create_new_node(Host, ["pubsub"], ?MYJID),
    create_new_node(Host, ["pubsub", "nodes"], ?MYJID),
    create_new_node(Host, ["home"], ?MYJID),
    create_new_node(Host, ["home", ServerHost], ?MYJID),
    lists:foreach(fun(H) ->
			  create_new_node(Host, ["home", H], ?MYJID)
		  end, ServedHosts),
    ets:new(gen_mod:get_module_proc(Host, pubsub_presence),
	    [set, named_table]),
    {ok, #state{host = Host, server_host = ServerHost, access = Access}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, 
#state{server_host = ServerHost, access = Access} = State) ->
    case catch do_route(To#jid.lserver, ServerHost, Access, From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_route(Host, ServerHost, Access, From, To, Packet) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    case To of
	#jid{luser = "", lresource = ""} ->
	    case Name of
		"iq" ->
		    case jlib:iq_query_info(Packet) of
			#iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS,
			    sub_el = SubEl} = IQ ->
			    {xmlelement, _, QAttrs, _} = SubEl,
			    Node = xml:get_attr_s("node", QAttrs),
			    Res = IQ#iq{type = result,
					sub_el = [{xmlelement, "query",
						   QAttrs,
						   iq_disco_info(Node)}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS = XMLNS,
			    sub_el = SubEl} = IQ ->
			    {xmlelement, _, QAttrs, _} = SubEl,
			    Node = xml:get_attr_s("node", QAttrs),
			    Res =
				case iq_disco_items(Host, From, Node) of
				    {result, IQRes} ->
					jlib:iq_to_xml(
					  IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   QAttrs,
							   IQRes}]});
				    {error, Error} ->
					jlib:make_error_reply(
					  Packet, Error)
				end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = Type, xmlns = ?NS_PUBSUB = XMLNS,
			    sub_el = SubEl} = IQ ->
			    Res =
				case iq_pubsub(Host, ServerHost, From, Type, SubEl, Access) of
				    {result, IQRes} ->
					jlib:iq_to_xml(
					  IQ#iq{type = result,
						sub_el = IQRes});
				    {error, Error} ->
					jlib:make_error_reply(
					  Packet, Error)
				end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = Type, xmlns = ?NS_PUBSUB_OWNER = XMLNS,
			    lang = Lang, sub_el = SubEl} = IQ ->
			    Res =
				case iq_pubsub_owner(
				       Host, From, Type, Lang, SubEl) of
				    {result, IQRes} ->
					jlib:iq_to_xml(
					  IQ#iq{type = result,
						sub_el = IQRes});
				    {error, Error} ->
					jlib:make_error_reply(
					  Packet, Error)
				end,
			    ejabberd_router:route(To, From, Res);
			#iq{type = get, xmlns = ?NS_VCARD = XMLNS,
			    lang = Lang, sub_el = SubEl} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el = [{xmlelement, "vCard",
						   [{"xmlns", XMLNS}],
						   iq_get_vcard(Lang)}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			#iq{} ->
			    Err = jlib:make_error_reply(
				    Packet,
				    ?ERR_FEATURE_NOT_IMPLEMENTED),
			    ejabberd_router:route(To, From, Err);
			_ ->
			    ok
		    end;
		"presence" ->
		    Type = xml:get_attr_s("type", Attrs),
		    if
			(Type == "unavailable") or (Type == "error") ->
			    ets:delete(
			      gen_mod:get_module_proc(Host, pubsub_presence),
			      {From#jid.luser, From#jid.lserver});
			true ->
			    ets:insert(
			      gen_mod:get_module_proc(Host, pubsub_presence),
			      {{From#jid.luser, From#jid.lserver}, []})
		    end,
		    ok;
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



node_to_string(Node) ->
    string:strip(lists:flatten(lists:map(fun(S) -> [S, "/"] end, Node)),
		 right, $/).


iq_disco_info(SNode) ->
    Node = string:tokens(SNode, "/"),
    case Node of
	[] ->
	    [{xmlelement, "identity",
	      [{"category", "pubsub"},
	       {"type", "generic"},
	       {"name", "Publish-Subscribe"}], []},
	     {xmlelement, "feature", [{"var", ?NS_PUBSUB}], []},
	     {xmlelement, "feature", [{"var", ?NS_PUBSUB_EVENT}], []},
	     {xmlelement, "feature", [{"var", ?NS_PUBSUB_OWNER}], []},
	     {xmlelement, "feature", [{"var", ?NS_VCARD}], []}];
	_ ->
	    % TODO
	    []
    end.

iq_disco_items(Host, From, SNode) ->
	{Node,ItemID} = case SNode of
	[] ->
		{[],none};
	_ ->
		Tokens = string:tokens(SNode, "!"),
		NodeList = string:tokens(lists:nth(1, Tokens), "/"),
		ItemName = case length(Tokens) of
		2 -> lists:nth(2, Tokens);
		_ -> none
		end,
		{NodeList, ItemName}
	end,
	NodeFull = string:tokens(SNode,"/"),
    F = fun() ->
		case mnesia:read({pubsub_node, {Host, Node}}) of
		    [#pubsub_node{info = Info}] ->
			case ItemID of
			none ->
				SubNodes = mnesia:index_read(pubsub_node,
						     {Host, Node},
						     #pubsub_node.host_parent),
				SubItems = lists:map(fun(#pubsub_node{host_node = {_, N}}) ->
					      SN = node_to_string(N),
					      {xmlelement, "item",
					       [{"jid", Host},
						{"node", SN},
						{"name", lists:last(N)}], []}
				      end, SubNodes),
				SN = node_to_string(Node),
				Items = lists:map(fun(#item{id = Name}) ->
						RealName = case Name of
						[] -> "item";
						_ -> Name
						end,
					      {xmlelement, "item",
					       [{"jid", Host},
						{"node", SN ++ "!" ++ Name},
						{"name", RealName}], []}
				      end, Info#nodeinfo.items),
				SubItems ++ Items;
			_ ->
				[]
			end;
		    [] ->
			case Node of
			    [] ->
				SubNodes = mnesia:index_read(
					     pubsub_node,
					     {Host, Node},
					     #pubsub_node.host_parent),
				lists:map(
				  fun(#pubsub_node{host_node = {_, N}}) ->
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

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_pubsub"}]},
     {xmlelement, "URL", [],
      [{xmlcdata,
	"http://ejabberd.jabberstudio.org/"}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(
		    Lang,
		    "ejabberd pub/sub module\n"
		    "Copyright (c) 2003-2006 Alexey Shchepin")}]}].


iq_pubsub(Host, ServerHost, From, Type, SubEl, Access) ->
    {xmlelement, _, _, SubEls} = SubEl,
    case xml:remove_cdata(SubEls) of
	[{xmlelement, Name, Attrs, Els}] ->
	    SNode = xml:get_attr_s("node", Attrs),
	    Node = string:tokens(SNode, "/"),
	    case {Type, Name} of
		{set, "create"} ->
		    create_new_node(Host, Node, From, ServerHost, Access);
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
		    get_entities(Host, From, Node);
		{set, "entities"} ->
		    set_entities(Host, From, Node, xml:remove_cdata(Els));
		{get, "affiliations"} ->
		    get_affiliations(Host, From);
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

-define(BOOLXFIELD(Label, Var, Val),
	?XFIELD("boolean", Label, Var,
		case Val of
		    true -> "1";
		    _ -> "0"
		end)).

-define(STRINGXFIELD(Label, Var, Val),
	?XFIELD("text-single", Label, Var, Val)).

-define(XFIELDOPT(Type, Label, Var, Val, Opts),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 lists:map(fun(Opt) ->
			   {xmlelement, "option", [],
			    [{xmlelement, "value", [],
			      [{xmlcdata, Opt}]}]}
		   end, Opts) ++
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(LISTXFIELD(Label, Var, Val, Opts),
	?XFIELDOPT("list-single", Label, Var, Val, Opts)).



%% Create new pubsub nodes
%% This function is used during init to create the first bootstrap nodes
create_new_node(Host, Node, Owner) ->
    %% This is the case use during "bootstrapping to create the initial
    %% hierarchy. Should always be ... undefined,all
    create_new_node(Host, Node, Owner, undefined, all).
create_new_node(Host, Node, Owner, ServerHost, Access) ->
    case Node of
	[] ->
	    {LOU, LOS, _} = jlib:jid_tolower(Owner),
	    HomeNode = ["home", LOS, LOU],
	    create_new_node(Host, HomeNode, Owner, ServerHost, Access),
	    NewNode = ["home", LOS, LOU, randoms:get_string()],
	    create_new_node(Host, NewNode, Owner, ServerHost, Access);
	_ ->
	    LOwner = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
	    Parent = lists:sublist(Node, length(Node) - 1),
	    F = fun() ->
			ParentExists = (Parent == []) orelse
			    case mnesia:read({pubsub_node, {Host, Parent}}) of
				[_] ->
				    true;
				[] ->
				    false
			    end,
			case ParentExists of
			    false ->
				{error, ?ERR_CONFLICT};
			    _ ->
				case mnesia:read({pubsub_node, {Host, Node}}) of
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
					  #pubsub_node{host_node = {Host, Node},
						       host_parent = {Host, Parent},
						       info = #nodeinfo{
							 entities = Entities}}),
					ok
				end
			end
		end,
	    case check_create_permission(Host, Node, Owner, ServerHost, Access) of
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
    ejabberd_hooks:run(pubsub_publish_item, Host,
		       [JID, ?MYJID, Node, ItemID, Payload]),
    Publisher = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    F = fun() ->
		case mnesia:read({pubsub_node, {Host, Node}}) of
		    [#pubsub_node{info = Info} = N] ->
			Affiliation = get_affiliation(Info, Publisher),
			Subscription = get_subscription(Info, Publisher),
			MaxSize = get_node_option(Info, max_payload_size),
			Model = get_node_option(Info, publish_model),
			Size = size(term_to_binary(Payload)),
			if
			    ((Model == open) or
			    ((Model == publishers) and
			     ((Affiliation == owner) or
			      (Affiliation == publisher))) or
			    ((Model == subscribers) and
			     (Subscription == subscribed))) and
			    (Size =< MaxSize) ->
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
		case mnesia:read({pubsub_node, {Host, Node}}) of
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
    SubscriberWithoutResource = jlib:jid_remove_resource(Subscriber),
    F = fun() ->
  		case mnesia:read({pubsub_node, {Host, Node}}) of
  		    [#pubsub_node{info = Info} = N] ->
  			Affiliation = get_affiliation(Info, Subscriber),
 			AllowSubscriptions = get_node_option(Info, subscribe),
 			if
 			    AllowSubscriptions and
 			    (Affiliation /= outcast) ->
 				NewInfo = add_subscriber(Info, Subscriber),
 				mnesia:write(N#pubsub_node{info = NewInfo}),
 				{result, [], Info};
  			    true ->
  				{error, ?ERR_NOT_ALLOWED}
  			end;
		    [] ->
			{error, ?ERR_ITEM_NOT_FOUND}
		end
	end,
    if
	Sender == SubscriberWithoutResource ->
	    case mnesia:transaction(F) of
		{atomic, {error, _} = Error} ->
		    Error;
 		{atomic, {result, Res, Info}} ->
 		    case get_node_option(Info, send_item_subscribe) of
 			true ->
 			    ItemsEls =
 				lists:map(
 				  fun(#item{id = ItemID,
					    payload = Payload}) ->
					  ItemAttrs = case ItemID of
							  "" -> [];
							  _ -> [{"id", ItemID}]
						      end,
 					  {xmlelement, "item",
 					   ItemAttrs, Payload}
				  end, Info#nodeinfo.items),
 			    Stanza =
 				{xmlelement, "message",
 				 [],
 				 [{xmlelement, "x",
 				   [{"xmlns", ?NS_PUBSUB_EVENT}],
 				   [{xmlelement, "items",
 				     [{"node", node_to_string(Node)}],
 				     ItemsEls}]}]},
 			    ejabberd_router:route(
 			      ?MYJID, jlib:make_jid(Subscriber), Stanza);
 			false ->
 			    ok
 		    end,
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
		case mnesia:read({pubsub_node, {Host, Node}}) of
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
	    case catch mnesia:dirty_read(pubsub_node, {Host, Node}) of
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
		case mnesia:read({pubsub_node, {Host, Node}}) of
		    [#pubsub_node{info = Info}] ->
			case get_affiliation(Info, Owner) of
			    owner ->
				% TODO: don't iterate over entire table
				Removed =
				    mnesia:foldl(
				      fun(#pubsub_node{host_node = {_, N},
						       info = NInfo}, Acc) ->
					      case lists:prefix(Node, N) of
						  true ->
						      [{N, NInfo} | Acc];
						  _ ->
						      Acc
					      end
				      end, [], pubsub_node),
				lists:foreach(
				  fun({N, _}) ->
					  mnesia:delete({pubsub_node, {Host, N}})
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
		case mnesia:read({pubsub_node, {Host, Node}}) of
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


get_entities(Host, OJID, Node) ->
    Owner = jlib:jid_tolower(jlib:jid_remove_resource(OJID)),
    case catch mnesia:dirty_read(pubsub_node, {Host, Node}) of
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


set_entities(Host, OJID, Node, EntitiesEls) ->
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
					  [{jlib:jid_tolower(JID),
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
			case mnesia:read({pubsub_node, {Host, Node}}) of
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


get_affiliations(Host, JID) ->
    LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    case catch mnesia:dirty_select(
		 pubsub_node,
		 [{#pubsub_node{_ = '_'},
		   [],
		   ['$_']}]) of
	{'EXIT', _} ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	Nodes ->
	    Entities =
		lists:flatmap(
		  fun(#pubsub_node{host_node = {H, Node}, info = Info})
		     when H == Host ->
			  Affiliation = get_affiliation(Info, LJID),
			  Subscription = get_subscription(Info, LJID),
			  if
			      (Affiliation /= none) or
			      (Subscription /= none) ->
				  [{xmlelement, "entity",
				    [{"node", node_to_string(Node)},
				     {"jid", jlib:jid_to_string(JID)},
				     {"affiliation",
				      affiliation_to_string(Affiliation)},
				     {"subscription",
				      subscription_to_string(Subscription)}],
				    []}];
			      true ->
				  []
			  end;
		     (_) ->
			  []
		  end, Nodes),
	    {result, [{xmlelement, "pubsub",
		       [{"xmlns", ?NS_PUBSUB_EVENT}],
		       [{xmlelement, "affiliations", [],
			 Entities}]}]}
    end.




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


check_create_permission(Host, Node, Owner, ServerHost, Access) ->
	#jid{luser = User, lserver = Server, lresource = Resource} = Owner,
    case acl:match_rule(ServerHost, Access, {User, Server, Resource}) of
    allow ->
    	if Server == Host ->
	    	true;
		true ->
	    	case Node of
			["home", Server, User | _] ->
		    	true;
			_ ->
			    false
		    end
	    end;
	_ ->
	    case Owner of
		?MYJID ->
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
    Items3 = lists:sublist(Items2, get_max_items(Info)),
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
    case catch mnesia:dirty_read(pubsub_node, {Host, Node}) of
	[#pubsub_node{info = Info}] ->
	    ?DICT:fold(
	       fun(JID, #entity{subscription = Subscription}, _) ->
		       Present = case get_node_option(
					Info, presence_based_delivery) of
				     true ->
					 case ets:lookup(
						gen_mod:get_module_proc(Host, pubsub_presence),
						{element(1, JID),
						 element(2, JID)}) of
					     [_] ->
						 true;
					     [] ->
						 false
					 end;
				     false ->
					 true
				 end,
		       if
			   (Subscription /= none) and
			   (Subscription /= pending) and
			   Present ->
			       ItemAttrs = case ItemID of
					       "" -> [];
					       _ -> [{"id", ItemID}]
					   end,
			       Content = case get_node_option(
						Info, deliver_payloads) of
					     true ->
						 Payload;
					     false ->
						 []
					 end,
			       Stanza =
				   {xmlelement, "message", [],
				    [{xmlelement, "event",
				      [{"xmlns", ?NS_PUBSUB_EVENT}],
				      [{xmlelement, "items",
					[{"node", node_to_string(Node)}],
					[{xmlelement, "item",
					  ItemAttrs,
					  Content}]}]}]},
			       ejabberd_router:route(
				 ?MYJID, jlib:make_jid(JID), Stanza);
			   true ->
			       ok
		       end
	       end, ok, Info#nodeinfo.entities);
	_ ->
	    false
    end.


broadcast_retract_item(Host, Node, ItemID) ->
    case catch mnesia:dirty_read(pubsub_node, {Host, Node}) of
	[#pubsub_node{info = Info}] ->
	    case get_node_option(Info, notify_retract) of
		true ->
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
				       ejabberd_router:route(
					 ?MYJID, jlib:make_jid(JID), Stanza);
				   true ->
				       ok
			       end
		       end, ok, Info#nodeinfo.entities);
		false ->
		    ok
	    end;
	_ ->
	    false
    end.


broadcast_removed_node(Host, Removed) ->
    lists:foreach(
      fun({Node, Info}) ->
	      case get_node_option(Info, notify_delete) of
		  true ->
		      Entities = Info#nodeinfo.entities,
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
					 ejabberd_router:route(
					   ?MYJID, jlib:make_jid(JID), Stanza);
				     true ->
					 ok
				 end
			 end, ok, Entities);
		  false ->
		      ok
	      end
      end, Removed).


broadcast_config_notification(Host, Node, Lang) ->
    case catch mnesia:dirty_read(pubsub_node, {Host, Node}) of
	[#pubsub_node{info = Info}] ->
	    case get_node_option(Info, notify_config) of
		true ->
		    ?DICT:fold(
		       fun(JID, #entity{subscription = Subscription}, _) ->
			       Present = case get_node_option(
						Info, presence_based_delivery) of
					     true ->
						 case ets:lookup(
							gen_mod:get_module_proc(Host, pubsub_presence),
							{element(1, JID),
							 element(2, JID)}) of
						     [_] ->
							 true;
						     [] ->
							 false
						 end;
					     false ->
						 true
					 end,
			       if
				   (Subscription /= none) and
				   (Subscription /= pending) and
				   Present ->
				       Fields = get_node_config_xfields(
						  Node, Info, Lang),
				       Content = case get_node_option(
							Info, deliver_payloads) of
						     true ->
							 [{xmlelement, "x",
							   [{"xmlns", ?NS_XDATA},
							    {"type", "form"}],
							   Fields}];
						     false ->
							 []
						 end,
				       Stanza =
					   {xmlelement, "message", [],
					    [{xmlelement, "x",
					      [{"xmlns", ?NS_PUBSUB_EVENT}],
					      [{xmlelement, "items",
						[{"node", node_to_string(Node)}],
						[{xmlelement, "item",
						  [{"id", "configuration"}],
						  Content}]}]}]},
				       ejabberd_router:route(
					 ?MYJID, jlib:make_jid(JID), Stanza);
				   true ->
				       ok
			       end
		       end, ok, Info#nodeinfo.entities);
		false ->
		    ok
	    end;
	_ ->
	    false
    end.



iq_pubsub_owner(Host, From, Type, Lang, SubEl) ->
    {xmlelement, _, _, SubEls} = SubEl,
    case xml:remove_cdata(SubEls) of
	[{xmlelement, Name, Attrs, Els}] ->
	    SNode = xml:get_attr_s("node", Attrs),
	    Node = string:tokens(SNode, "/"),
	    case {Type, Name} of
		{get, "configure"} ->
		    get_node_config(Host, From, Node, Lang);
		{set, "configure"} ->
		    set_node_config(Host, From, Node, Els, Lang);
		_ ->
		    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	    end;
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end.

get_node_config(Host, From, Node, Lang) ->
    case catch mnesia:dirty_read(pubsub_node, {Host, Node}) of
	[#pubsub_node{info = Info}] ->
	    case get_affiliation(Info, From) of
		owner ->
		    Fields = get_node_config_xfields(Node, Info, Lang),
		    {result, [{xmlelement, "pubsub",
			       [{"xmlns", ?NS_PUBSUB_OWNER}],
			       [{xmlelement, "configure",
				 [{"node", node_to_string(Node)}],
				 [{xmlelement, "x", [{"xmlns", ?NS_XDATA},
						     {"type", "form"}],
				   Fields}]}]}]};
		_ ->
		    {error, ?ERR_NOT_AUTHORIZED}
	    end;
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.

% TODO: move to jlib.hrl
-define(NS_PUBSUB_NODE_CONFIG, "http://jabber.org/protocol/pubsub#node_config").

-define(BOOL_CONFIG_FIELD(Label, Var),
	?BOOLXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		    get_node_option(Info, Var))).

-define(STRING_CONFIG_FIELD(Label, Var),
	?STRINGXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		      get_node_option(Info, Var))).

-define(INTEGER_CONFIG_FIELD(Label, Var),
	?STRINGXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		      integer_to_list(get_node_option(Info, Var)))).

-define(JLIST_CONFIG_FIELD(Label, Var, Opts),
	?LISTXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		    jlib:jid_to_string(get_node_option(Info, Var)),
		    [jlib:jid_to_string(O) || O <- Opts])).

-define(ALIST_CONFIG_FIELD(Label, Var, Opts),
	?LISTXFIELD(Label, "pubsub#" ++ atom_to_list(Var),
		    atom_to_list(get_node_option(Info, Var)),
		    [atom_to_list(O) || O <- Opts])).


-define(DEFAULT_OPTIONS,
	[{deliver_payloads, true},
	 {notify_config, false},
	 {notify_delete, false},
	 {notify_retract, true},
	 {persist_items, true},
	 {max_items, ?MAXITEMS div 2},
	 {subscribe, true},
	 {subscription_model, open},
	 {publish_model, publishers},
	 {max_payload_size, ?MAX_PAYLOAD_SIZE},
	 {send_item_subscribe, false},
	 {presence_based_delivery, false}]).

get_node_option(Info, current_approver) ->
    Default = hd(get_owners_jids(Info)),
    Options = Info#nodeinfo.options,
    element(
      2, element(2, lists:keysearch(
		      current_approver, 1,
		      Options ++ [{current_approver, Default}])));
get_node_option(#nodeinfo{options = Options}, Var) ->
    element(
      2, element(2, lists:keysearch(Var, 1, Options ++ ?DEFAULT_OPTIONS))).

get_max_items(Info) ->
    case get_node_option(Info, persist_items) of
	true ->
	    get_node_option(Info, max_items);
	false ->
	    0
    end.

get_owners_jids(Info) ->
    Entities = Info#nodeinfo.entities,
    Owners =
	?DICT:fold(
	   fun(JID,
	       #entity{affiliation = Affiliation,
		       subscription = Subscription},
	       Acc) ->
		   case Affiliation of
		       owner ->
			   [JID | Acc];
		       _ ->
			   Acc
		   end
	   end, [], Entities),
    lists:sort(Owners).


get_node_config_xfields(Node, Info, Lang) ->
    [?XFIELD("hidden", "", "FORM_TYPE", ?NS_PUBSUB_NODE_CONFIG),
     ?BOOL_CONFIG_FIELD("Deliver payloads with event notifications", deliver_payloads),
     ?BOOL_CONFIG_FIELD("Notify subscribers when the node configuration changes", notify_config),
     ?BOOL_CONFIG_FIELD("Notify subscribers when the node is deleted", notify_delete),
     ?BOOL_CONFIG_FIELD("Notify subscribers when items are removed from the node", notify_retract),
     ?BOOL_CONFIG_FIELD("Persist items to storage", persist_items),
     ?INTEGER_CONFIG_FIELD("Max # of items to persist", max_items),
     ?BOOL_CONFIG_FIELD("Whether to allow subscriptions", subscribe),
     ?ALIST_CONFIG_FIELD("Specify the subscriber model", subscription_model,
			 [open]),
     ?ALIST_CONFIG_FIELD("Specify the publisher model", publish_model,
			 [publishers, subscribers, open]),
     ?INTEGER_CONFIG_FIELD("Max payload size in bytes", max_payload_size),
     ?BOOL_CONFIG_FIELD("Send items to new subscribers", send_item_subscribe),
     ?BOOL_CONFIG_FIELD("Only deliver notifications to available users", presence_based_delivery),
     ?JLIST_CONFIG_FIELD("Specify the current subscription approver", current_approver,
			 get_owners_jids(Info))
    ].


set_node_config(Host, From, Node, Els, Lang) ->
    case catch mnesia:dirty_read(pubsub_node, {Host, Node}) of
	[#pubsub_node{info = Info} = N] ->
	    case get_affiliation(Info, From) of
		owner ->
		    case xml:remove_cdata(Els) of
			[{xmlelement, "x", _Attrs1, _Els1} = XEl] ->
			    case {xml:get_tag_attr_s("xmlns", XEl),
				  xml:get_tag_attr_s("type", XEl)} of
				{?NS_XDATA, "cancel"} ->
				    {result, []};
				{?NS_XDATA, "submit"} ->
				    CurOpts = Info#nodeinfo.options,
				    set_node_config1(
				      Host, From, Node, XEl, CurOpts, Lang);
				_ ->
				    {error, ?ERR_BAD_REQUEST}
			    end;
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		_ ->
		    {error, ?ERR_NOT_AUTHORIZED}
	    end;
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.


set_node_config1(Host, From, Node, XEl, CurOpts, Lang) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
	invalid ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    case set_xoption(XData, CurOpts) of
		NewOpts when is_list(NewOpts) ->
		    change_node_opts(Host, NewOpts, Node, Lang);
		Err ->
		    Err
	    end
    end.

add_opt(Key, Value, Opts) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    [{Key, Value} | Opts1].


-define(SET_BOOL_XOPT(Opt, Val),
	case Val of
	    "0" -> set_xoption(Opts, add_opt(Opt, false, NewOpts));
	    "1" -> set_xoption(Opts, add_opt(Opt, true, NewOpts));
	    _ -> {error, ?ERR_BAD_REQUEST}
	end).

-define(SET_STRING_XOPT(Opt, Val),
	set_xoption(Opts, add_opt(Opt, Val, NewOpts))).

-define(SET_INTEGER_XOPT(Opt, Val, Min, Max),
	case catch list_to_integer(Val) of
	    IVal when is_integer(IVal),
		      IVal >= Min,
		      IVal =< Max ->
		set_xoption(Opts, add_opt(Opt, IVal, NewOpts));
	    _ ->
		{error, ?ERR_BAD_REQUEST}
	end).

-define(SET_ALIST_XOPT(Opt, Val, Vals),
	case lists:member(Val, [atom_to_list(V) || V <- Vals]) of
	    true ->
		set_xoption(Opts, add_opt(Opt, list_to_atom(Val), NewOpts));
	    false ->
		{error, ?ERR_BAD_REQUEST}
	end).


set_xoption([], NewOpts) ->
    NewOpts;
set_xoption([{"FORM_TYPE", _} | Opts], NewOpts) ->
    set_xoption(Opts, NewOpts);
set_xoption([{"pubsub#deliver_payloads", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_payloads, Val);
set_xoption([{"pubsub#notify_config", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_config, Val);
set_xoption([{"pubsub#notify_delete", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_delete, Val);
set_xoption([{"pubsub#notify_retract", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_retract, Val);
set_xoption([{"pubsub#persist_items", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(persist_items, Val);
set_xoption([{"pubsub#max_items", [Val]} | Opts], NewOpts) ->
    ?SET_INTEGER_XOPT(max_items, Val, 0, ?MAXITEMS);
set_xoption([{"pubsub#subscribe", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(subscribe, Val);
set_xoption([{"pubsub#subscription_model", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(subscription_model, Val, [open]);
set_xoption([{"pubsub#publish_model", [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(publish_model, Val, [publishers, subscribers, open]);
set_xoption([{"pubsub#max_payload_size", [Val]} | Opts], NewOpts) ->
    ?SET_INTEGER_XOPT(max_payload_size, Val, 0, ?MAX_PAYLOAD_SIZE);
set_xoption([{"pubsub#send_item_subscribe", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(send_item_subscribe, Val);
set_xoption([{"pubsub#presence_based_delivery", [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(presence_based_delivery, Val);
set_xoption([{"pubsub#current_approver", _} | Opts], NewOpts) ->
    % TODO
    set_xoption(Opts, NewOpts);
%set_xoption([{"title", [Val]} | Opts], NewOpts) ->
%    ?SET_STRING_XOPT(title, Val);
set_xoption([_ | _Opts], _NewOpts) ->
    {error, ?ERR_BAD_REQUEST}.


change_node_opts(Host, NewOpts, Node, Lang) ->
    F = fun() ->
		case mnesia:read({pubsub_node, {Host, Node}}) of
		    [#pubsub_node{info = Info} = N] ->
			NewInfo = Info#nodeinfo{options = NewOpts},
			mnesia:write(
			  N#pubsub_node{info = NewInfo}),
			{result, []};
		    [] ->
			{error, ?ERR_ITEM_NOT_FOUND}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, Res}} ->
	    broadcast_config_notification(Host, Node, Lang),
	    {result, Res};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.





find_my_host(LServer) ->
    Parts = string:tokens(LServer, "."),
    find_my_host(Parts, ?MYHOSTS).

find_my_host([], _Hosts) ->
    ?MYNAME;
find_my_host([_ | Tail] = Parts, Hosts) ->
    Domain = parts_to_string(Parts),
    case lists:member(Domain, Hosts) of
	true ->
	    Domain;
	false ->
	    find_my_host(Tail, Hosts)
    end.

parts_to_string(Parts) ->
    string:strip(lists:flatten(lists:map(fun(S) -> [S, $.] end, Parts)),
		 right, $.).



update_table(Host) ->
    Fields = record_info(fields, pubsub_node),
    case mnesia:table_info(pubsub_node, attributes) of
	Fields ->
	    ok;
	[node, parent, info] ->
	    ?INFO_MSG("Converting pubsub_node table from "
		      "{node, parent, info} format", []),
	    {atomic, ok} = mnesia:create_table(
			     mod_pubsub_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, pubsub_node},
			      {attributes, record_info(fields, pubsub_node)}]),
	    mnesia:del_table_index(pubsub_node, parent),
	    mnesia:transform_table(pubsub_node, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_pubsub_tmp_table),
			 mnesia:foldl(
			   fun(#pubsub_node{host_node = N,
					    host_parent = P} = R, _) ->
				   mnesia:dirty_write(
				     mod_pubsub_tmp_table,
				     R#pubsub_node{host_node = {Host, N},
						   host_parent = {Host, P}})
			   end, ok, pubsub_node)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(pubsub_node),
	    F2 = fun() ->
			 mnesia:write_lock_table(pubsub_node),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_pubsub_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_pubsub_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating pubsub_node table", []),
	    mnesia:transform_table(pubsub_node, ignore, Fields)
    end.




