%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

%%% @doc Roster management (Mnesia storage).
%%%
%%% Includes support for XEP-0237: Roster Versioning.
%%% The roster versioning follows an all-or-nothing strategy:
%%%  - If the version supplied by the client is the latest, return an empty response.
%%%  - If not, return the entire new roster (with updated version string).
%%% Roster version is a hash digest of the entire roster.
%%% No additional data is stored in DB.

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
	 webadmin_user/4,
	 get_versioning_feature/2,
	 roster_versioning_enabled/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-define(NS_ROSTER_VER, "urn:xmpp:features:rosterver").

%% @type rosteritem() = {roster, USJ, US, Contact_JID, Name, Subscription, Ask, Groups, Askmessage, Xs}
%%     USJ = {LUser, LServer, Prepd_Contact_JID}
%%         LUser = binary()
%%         LServer = binary()
%%         Prepd_Contact_JID = jlib:shortjid()
%%     US = {LUser, LServer}
%%     Contact_JID = jlib:shortjid()
%%     Name = binary()
%%     Subscription = none | to | from | both
%%     Ask = none | out | in | both
%%     Groups = [binary()]
%%     Askmessage = binary()
%%     Xs = [exmpp_xml:xmlel()]

%% @spec (Host, Opts) -> term()
%%     Host = string()
%%     Opts = list()

start(Host, Opts) when is_list(Host) ->
    HostB = list_to_binary(Host),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version, [{disc_copies, [node()]},
				{attributes, record_info(fields, roster_version)}]),
    update_table(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ejabberd_hooks:add(roster_get, HostB,
		       ?MODULE, get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription, HostB,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, HostB,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_subscription_lists, HostB,
		       ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:add(roster_get_jid_info, HostB,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(resend_subscription_requests_hook, HostB,
		       ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:add(roster_get_versioning_feature, HostB,
		       ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:add(webadmin_page_host, HostB,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, HostB,
		       ?MODULE, webadmin_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_ROSTER,
				  ?MODULE, process_iq, IQDisc).

%% @spec (Host) -> term()
%%     Host = string()

stop(Host) when is_list(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(roster_get, HostB,
			  ?MODULE, get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription, HostB,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, HostB,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_subscription_lists, HostB,
			  ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:delete(roster_get_jid_info, HostB,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(resend_subscription_requests_hook, HostB,
			  ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:delete(roster_get_versioning_feature, HostB,
		          ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:delete(webadmin_page_host, HostB,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, HostB,
			  ?MODULE, webadmin_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB,
				     ?NS_ROSTER).

%% @spec (From, To, IQ_Rec) -> IQ_Result
%%     From = exmpp_jid:jid()
%%     To = exmpp_jid:jid()
%%     IQ_Rec = exmpp_iq:iq()
%%     IQ_Result = exmpp_iq:iq()

process_iq(From, To, IQ_Rec)
  when ?IS_JID(From), ?IS_JID(To), ?IS_IQ_RECORD(IQ_Rec) ->
    LServer = exmpp_jid:prep_domain_as_list(From),
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    process_local_iq(From, To, IQ_Rec);
	_ ->
	    exmpp_iq:error(IQ_Rec, 'item-not-found')
    end.

%% @spec (From, To, IQ_Rec) -> IQ_Result
%%     From = exmpp_jid:jid()
%%     To = exmpp_jid:jid()
%%     IQ_Rec = exmpp_iq:iq()
%%     IQ_Result = exmpp_iq:iq()

process_local_iq(From, To, #iq{type = get} = IQ_Rec)
  when ?IS_JID(From), ?IS_JID(To), ?IS_IQ_RECORD(IQ_Rec) ->
    process_iq_get(From, To, IQ_Rec);
process_local_iq(From, To, #iq{type = set} = IQ_Rec)
  when ?IS_JID(From), ?IS_JID(To), ?IS_IQ_RECORD(IQ_Rec) ->
    process_iq_set(From, To, IQ_Rec).

roster_hash(Items) ->
	sha:sha(term_to_binary(
		lists:sort(
			[R#roster{groups = lists:sort(Grs)} || 
				R = #roster{groups = Grs} <- Items]))).
		
%% @spec (Host) -> true | false
%% @type Host = binary()
roster_versioning_enabled(Host)  ->
	gen_mod:get_module_opt(binary_to_list(Host), ?MODULE, versioning, false).

%% @spec (Host) -> true | false
%% @type Host = binary()
roster_version_on_db(Host) ->
	gen_mod:get_module_opt(binary_to_list(Host), ?MODULE, store_current_id, false).

%% Returns a list that may contain an xmlelement with the XEP-237 feature if it's enabled.
get_versioning_feature(Acc, Host) ->
    case roster_versioning_enabled(Host) of
	true ->
	    Feature = exmpp_xml:element(?NS_ROSTER_VER, 'ver', [],
		       [exmpp_xml:element(?NS_ROSTER_VER, 'optional')]),
	    [Feature | Acc];
	false -> []
    end.

roster_version(LServer ,LUser) ->
	US = {LUser, LServer},
	case roster_version_on_db(LServer) of
		true ->
			case mnesia:dirty_read(roster_version, US) of
				[#roster_version{version =V}] -> V;
				[] -> not_found
			end;
		false ->
			roster_hash(ejabberd_hooks:run_fold(roster_get, LServer, [], [US]))
	end.

%% @spec (From, To, IQ_Rec) -> IQ_Result
%%     From = exmpp_jid:jid()
%%     To = exmpp_jid:jid()
%%     IQ_Rec = exmpp_iq:iq()
%%     IQ_Result = exmpp_iq:iq()
%% Load roster from DB only if neccesary
%% It is neccesary if
%%  	- roster versioning is disabled in server OR
%%	- roster versioning is not used by the client OR
%%	- roster versioning is used by server and client BUT the server isn't storing version IDs on db OR
%%	- the roster version from client don't match current version
process_iq_get(From, To, IQ_Rec) ->
    US = {_, LServer} = {exmpp_jid:prep_node(From), exmpp_jid:prep_domain(From)},
    try
	    {ItemsToSend, VersionToSend} = 
		case {exmpp_xml:get_attribute_as_list(exmpp_iq:get_request(IQ_Rec), ver,  not_found), 
		      roster_versioning_enabled(LServer),
		      roster_version_on_db(LServer)} of
		{not_found, _ , _} ->
			{lists:map(fun item_to_xml/1, 
					ejabberd_hooks:run_fold(roster_get, exmpp_jid:prep_domain(To), [], [US])), false};
		{_, false, _} ->
			{lists:map(fun item_to_xml/1, 
					ejabberd_hooks:run_fold(roster_get, exmpp_jid:prep_domain(To), [], [US])), false};
		
		{RequestedVersion, true, true} ->
			%% Retrieve version from DB. Only load entire roster
			%% when neccesary.
			case mnesia:dirty_read(roster_version, US) of
				[#roster_version{version = RequestedVersion}] ->
					{false, false};
				[#roster_version{version = NewVersion}] ->
					{lists:map(fun item_to_xml/1, 
						ejabberd_hooks:run_fold(roster_get, exmpp_jid:prep_domain(To), [], [US])), NewVersion};
				[] ->
					RosterVersion = sha:sha(term_to_binary(now())),
					mnesia:dirty_write(#roster_version{us = US, version = RosterVersion}),
					{lists:map(fun item_to_xml/1,
						ejabberd_hooks:run_fold(roster_get, exmpp_jid:prep_domain(To), [], [US])), RosterVersion}
			end;
		{RequestedVersion, true, false} ->
			RosterItems = ejabberd_hooks:run_fold(roster_get, exmpp_jid:prep_domain(To), [] , [US]),
			case roster_hash(RosterItems) of
				RequestedVersion ->
					{false, false};
				New ->
					{lists:map(fun item_to_xml/1, RosterItems), New}
			end
			
		end,
		case {ItemsToSend, VersionToSend} of
			{false, false} -> 
				exmpp_iq:result(IQ_Rec);
			{Items, false} -> 
				exmpp_iq:result(IQ_Rec, exmpp_xml:element(?NS_ROSTER, 'query', [] , Items));
			{Items, Version} -> 
				exmpp_iq:result(IQ_Rec, exmpp_xml:element(?NS_ROSTER, 'query', [?XMLATTR('ver', Version)], Items))
		end
    catch 
    	_:_ ->  
 	    exmpp_iq:error(IQ_Rec, 'internal-server-error')
     end.





%% @spec (Acc, US) -> New_Acc
%%     Acc = [rosteritem()]
%%     US = {User, Server}
%%         User = binary()
%%         Server = binary()
%%     New_Acc = [rosteritem()]

get_user_roster(Acc, {U, S} = US) when is_binary(U), is_binary(S) ->
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

%% @spec (Item) -> XML
%%     Item = rosteritem()
%%     XML = exmpp_xml:xmlel()

item_to_xml(Item) ->
    {U, S, R} = Item#roster.jid,
    Attrs1 = exmpp_xml:set_attribute_in_list([],
      'jid', exmpp_jid:to_binary(U, S, R)),
    Attrs2 = case Item#roster.name of
		 <<>> ->
		     Attrs1;
		 Name ->
		     exmpp_xml:set_attribute_in_list(Attrs1, 'name', Name)
	     end,
    Attrs3 = exmpp_xml:set_attribute_in_list(Attrs2,
      'subscription', Item#roster.subscription),
    Attrs4 = case ask_to_pending(Item#roster.ask) of
		 out ->
		     exmpp_xml:set_attribute_in_list(Attrs3,
		       'ask', <<"subscribe">>);
		 both ->
		     exmpp_xml:set_attribute_in_list(Attrs3,
		       'ask', <<"subscribe">>);
		 _ ->
		     Attrs3
	     end,
    SubEls1 = lists:map(fun(G) ->
				exmpp_xml:set_cdata(
				  #xmlel{ns = ?NS_ROSTER, name = 'group'}, G)
			end, Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    #xmlel{ns = ?NS_ROSTER, name = 'item', attrs = Attrs4, children = SubEls}.

%% @spec (From, To, IQ_Rec) -> IQ_Result
%%     From = exmpp_jid:jid()
%%     To = exmpp_jid:jid()
%%     IQ_Rec = exmpp_iq:iq()
%%     IQ_Result = exmpp_iq:iq()

process_iq_set(From, To, #iq{payload = Request} = IQ_Rec) ->
    case Request of
	#xmlel{children = Els} ->
	    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els);
	_ ->
	    ok
    end,
    exmpp_iq:result(IQ_Rec).

%% @spec (From, To, El) -> ok
%%    From = exmpp_jid:jid()
%%    To = exmpp_jid:jid()
%%    El = exmpp_xml:xmlel()

process_item_set(From, To, #xmlel{} = El) ->
    try
	JID1 = exmpp_jid:parse(exmpp_xml:get_attribute_as_binary(El, 'jid', <<>>)),
	User = exmpp_jid:node(From),
	LUser = exmpp_jid:prep_node(From),
	LServer = exmpp_jid:prep_domain(From),
	JID = jlib:short_jid(JID1),
	LJID = jlib:short_prepd_jid(JID1),
	F = fun() ->
		    Res = mnesia:read({roster, {LUser, LServer, LJID}}),
		    Item = case Res of
			       [] ->
				   #roster{usj = {LUser, LServer, LJID},
					   us = {LUser, LServer},
					   jid = JID};
			       [I] ->
				   I#roster{jid = JID,
					    name = <<>>,
					    groups = [],
					    xs = []}
			   end,
		    Item1 = process_item_attrs(Item, El#xmlel.attrs),
		    Item2 = process_item_els(Item1, El#xmlel.children),
		    case Item2#roster.subscription of
			remove ->
			    mnesia:delete({roster, {LUser, LServer, LJID}});
			_ ->
			    mnesia:write(Item2)
		    end,
		    %% If the item exist in shared roster, take the
		    %% subscription information from there:
		    Item3 = ejabberd_hooks:run_fold(roster_process_item,
						    exmpp_jid:prep_domain(From), Item2, [exmpp_jid:prep_domain(From)]),
		    case roster_version_on_db(LServer) of
		    	true -> mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
			false -> ok
		    end,
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
				  exmpp_jid:bare(From),
				  exmpp_jid:make(U, S, R),
				  exmpp_presence:unsubscribe());
			   true -> ok
			end,
			if IsFrom ->
				ejabberd_router:route(
				  exmpp_jid:bare(From),
				  exmpp_jid:make(U, S, R),
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

%% @spec (Item, Attrs) -> New_Item
%%     Item = rosteritem()
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     New_Item = rosteritem()

process_item_attrs(Item, [#xmlattr{name = Attr, value = Val} | Attrs]) ->
    case Attr of
	'name' ->
	    process_item_attrs(Item#roster{name = Val}, Attrs);
	'subscription' ->
	    case Val of
		<<"remove">> ->
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

%% @spec (Item, Els) -> New_Item
%%     Item = rosteritem()
%%     Els = [exmpp_xml:xmlel()]
%%     New_Item = rosteritem()

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

%% @spec (User, Server, From, Item) -> term()
%%     User = binary()
%%     Server = binary()
%%     From = exmpp_jid:jid()
%%     Item = rosteritem()

push_item(User, Server, From, Item)
  when is_binary(User), is_binary(Server), ?IS_JID(From) ->
    ejabberd_sm:route(exmpp_jid:make(),
		      exmpp_jid:make(User, Server),
		      #xmlel{name = 'broadcast', children =
		       [{item,
			 Item#roster.jid,
			 Item#roster.subscription}]}),

    case roster_versioning_enabled(Server) of
    	true ->
		push_item_version(Server, User, From, Item, roster_version(Server, User));
	false ->
	    lists:foreach(fun(Resource) ->
 			  push_item(User, Server, Resource, From, Item)
		  end, ejabberd_sm:get_user_resources(User, Server))
    end.

%% @spec (User, Server, Resource, From, Item) -> term()
%%     User = binary()
%%     Server = binary()
%%     Resource = binary()
%%     From = exmpp_jid:jid()
%%     Item = rosteritem()

% TODO: don't push to those who didn't load roster
push_item(User, Server, Resource, From, Item)
  when is_binary(User), is_binary(Server), is_binary(Resource),
  ?IS_JID(From) ->
    Request = #xmlel{ns = ?NS_ROSTER, name = 'query',
      children = [item_to_xml(Item)]},
    ResIQ = exmpp_iq:set(?NS_JABBER_CLIENT, Request,
      "push" ++ randoms:get_string()),
    ejabberd_router:route(
      From,
      exmpp_jid:make(User, Server, Resource),
      ResIQ).

%% @doc Roster push, calculate and include the version attribute.
%% TODO: don't push to those who didn't load roster
push_item_version(Server, User, From, Item, RosterVersion)  ->
    lists:foreach(fun(Resource) ->
			  push_item_version(User, Server, Resource, From, Item, RosterVersion)
		end, ejabberd_sm:get_user_resources(User, Server)).

push_item_version(User, Server, Resource, From, Item, RosterVersion) ->
    Request = #xmlel{ns = ?NS_ROSTER, name = 'query', attrs = [?XMLATTR('ver', RosterVersion)],
      children = [mod_roster:item_to_xml(Item)]},
    ResIQ = exmpp_iq:set(?NS_JABBER_CLIENT, Request,
      "push" ++ randoms:get_string()),
    ejabberd_router:route(
      From,
      exmpp_jid:make(User, Server, Resource),
      ResIQ).

%% @spec (Ignored, User, Server) -> Subscription_Lists
%%     Ignored = term()
%%     User = binary()
%%     Server = binary()
%%     Subscription_Lists = {F, T}
%%         F = [jlib:shortjid()]
%%         T = [jlib:shortjid()]

get_subscription_lists(_, User, Server) 
  when is_binary(User), is_binary(Server) ->
    try
	US = {User,Server},
	case mnesia:dirty_index_read(roster, US, #roster.us) of
	    Items when is_list(Items) ->
		fill_subscription_lists(Items, [], []);
	    _ ->
		{[], []}
	end
    catch
	_ ->
	    {[], []}
    end.

%% @spec (Items, F, T) -> {New_F, New_T}
%%     Items = [rosteritem()]
%%     F = [jlib:shortjid()]
%%     T = [jlib:shortjid()]
%%     New_F = [jlib:shortjid()]
%%     New_T = [jlib:shortjid()]

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

%% @hidden

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.

%% @spec (Ignored, User, Server, JID, Type, Reason) -> bool()
%%     Ignored = term()
%%     User = binary()
%%     Server = binary()
%%     JID = exmpp_jid:jid()
%%     Type = subscribe | subscribed | unsubscribe | unsubscribed
%%     Reason = binary() | undefined

in_subscription(_, User, Server, JID, Type, Reason)
  when is_binary(User), is_binary(Server), ?IS_JID(JID) ->
    process_subscription(in, User, Server, JID, Type, Reason).

%% @spec (User, Server, JID, Type) -> bool()
%%     User = binary()
%%     Server = binary()
%%     JID = exmpp_jid:jid()
%%     Type = subscribe | subscribed | unsubscribe | unsubscribed

out_subscription(User, Server, JID, Type)
  when is_binary(User), is_binary(Server), ?IS_JID(JID) ->
    process_subscription(out, User, Server, JID, Type, <<>>).

%% @spec (Direction, User, Server, JID1, Type, Reason) -> bool()
%%     Direction = in | out
%%     User = binary()
%%     Server = binary()
%%     JID1 = exmpp_jid:jid()
%%     Type = subscribe | subscribed | unsubscribe | unsubscribed
%%     Reason = binary() | undefined

process_subscription(Direction, User, Server, JID1, Type, Reason) 
  when is_binary(User), is_binary(Server) ->
    try
	US = {User, Server},
	LJID = jlib:short_prepd_jid(JID1),
	F = fun() ->
		    Item = case mnesia:read({roster, {User, Server, LJID}}) of
			       [] ->
				   JID = jlib:short_jid(JID1),
				   #roster{usj = {User, Server, LJID},
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
				     _         -> <<>>
				 end,
		    case NewState of
			none ->
			    {none, AutoReply};
			{none, none} when Item#roster.subscription == none,
					  Item#roster.ask == in ->
			    mnesia:delete({roster, {User, Server, LJID}}),
			    {none, AutoReply};
			{Subscription, Pending} ->
			    AskBinary = case AskMessage of
					    undefined -> <<>>;
					    B  -> B
					end,
			    NewItem = Item#roster{subscription = Subscription,
						  ask = Pending,
						  askmessage = AskBinary},
			    mnesia:write(NewItem),
			    case roster_version_on_db(Server) of
				true -> mnesia:write(#roster_version{us = {User, Server}, version = sha:sha(term_to_binary(now()))});
				false -> ok
			    end,
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
			  exmpp_jid:make(User, Server), JID1,
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
					  exmpp_jid:make(User, Server), Item)
			end,
			true;
		    none ->
			false
		end;
	    _ ->
		false
	end
    catch
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

%% @spec (User, Server) -> term()
%%     User = binary()
%%     Server = binary()

remove_user(User, Server) 
  when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
        send_unsubscription_to_rosteritems(LUser, LServer),
	F = fun() ->
		    lists:foreach(fun(R) ->
					  mnesia:delete_object(R)
				  end,
				  mnesia:index_read(roster, US, #roster.us))
	    end,
	mnesia:transaction(F)
    catch
	_ ->
	    ok
    end.

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
send_unsubscription_to_rosteritems(LUser, LServer) ->
    RosterItems = get_user_roster([], {LUser, LServer}),
    From = exmpp_jid:make(LUser, LServer, ""),
    lists:foreach(fun(RosterItem) ->
			  send_unsubscribing_presence(From, RosterItem)
		  end,
		  RosterItems).

%% @spec (From::jid(), Item::roster()) -> ok
send_unsubscribing_presence(From, Item) ->
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
    {INode, IDom, IRes} = Item#roster.jid,
    SendToJID = exmpp_jid:make(INode, IDom, IRes),
    if IsTo ->
    	 ejabberd_router:route(
	    	exmpp_jid:bare(From),
		SendToJID,
		exmpp_presence:unsubscribe());
       true -> ok
    end,
    if IsFrom ->
    	 ejabberd_router:route(
	    	exmpp_jid:bare(From),
		SendToJID,
		exmpp_presence:unsubscribed());
       true -> ok
    end,
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec (User, Server, El) -> term()
%%     User = binary()
%%     Server = binary()
%%     El = exmpp_xml:xmlel()

set_items(User, Server, #xmlel{children = Els})
  when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	F = fun() ->
		    lists:foreach(fun(El) ->
					  process_item_set_t(LUser, LServer, El)
				  end, Els)
	    end,
	mnesia:transaction(F)
    catch
	_ ->
	    ok
    end.

%% @spec (LUser, LServer, El) -> term()
%%     LUser = binary()
%%     LServer = binary()
%%     El = exmpp_xml:xmlel()

process_item_set_t(LUser, LServer, #xmlel{} = El) ->
    try
	JID1 = exmpp_jid:parse(exmpp_xml:get_attribute_as_list(El, 'jid', <<>>)),
	JID = jlib:short_jid(JID1),
	LJID = jlib:short_prepd_jid(JID1),
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

%% @spec (Item, Attrs) -> New_Item
%%     Item = rosteritem()
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     New_Item = rosteritem()

process_item_attrs_ws(Item, [#xmlattr{name = Attr, value = Val} | Attrs]) ->
    case Attr of
	'name' ->
	    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
	'subscription' ->
	    case Val of
		<<"remove">> ->
		    process_item_attrs_ws(Item#roster{subscription = remove},
					  Attrs);
		<<"none">> ->
		    process_item_attrs_ws(Item#roster{subscription = none},
					  Attrs);
		<<"both">> ->
		    process_item_attrs_ws(Item#roster{subscription = both},
					  Attrs);
		<<"from">> ->
		    process_item_attrs_ws(Item#roster{subscription = from},
					  Attrs);
		<<"to">> ->
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

%% @spec (Ls, User, Server) -> New_Ls
%%     Ls = [exmpp_xml:xmlel()]
%%     User = binary()
%%     Server = binary()
%%     New_Ls = [exmpp_xml:xmlel()]

get_in_pending_subscriptions(Ls, User, Server)
  when is_binary(User), is_binary(Server) ->
    JID = exmpp_jid:make(User, Server),
    US = {exmpp_jid:prep_node(JID), exmpp_jid:prep_domain(JID)},
    case mnesia:dirty_index_read(roster, US, #roster.us) of
	Result when is_list(Result) ->
	    Ls ++ lists:map(
		    fun(R) ->
			    Message = R#roster.askmessage,
			    {U0, S0, R0} = R#roster.jid,
			    Pres1 = exmpp_presence:subscribe(),
			    Pres2 = exmpp_stanza:set_jids(Pres1,
			      exmpp_jid:to_binary(U0, S0, R0),
			      exmpp_jid:to_binary(JID)),
			    exmpp_presence:set_status(Pres2, Message)
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

%% @spec (Ignored, User, Server, JID) -> {Subscription, Groups}
%%     Ignored = term()
%%     User = binary()
%%     Server = binary()
%%     JID = exmpp_jid:jid()
%%     Subscription = none | to | from | both
%%     Groups = [binary()]

get_jid_info(_, User, Server, JID)
  when is_binary(User), is_binary(Server), ?IS_JID(JID) ->
    try
	LJID = jlib:short_prepd_jid(JID),
	case catch mnesia:dirty_read(roster, {User, Server, LJID}) of
	    [#roster{subscription = Subscription, groups = Groups}] ->
		{Subscription, Groups};
	    _ ->
		LRJID = jlib:short_prepd_bare_jid(JID),
		if
		    LRJID == LJID ->
			{none, []};
		    true ->
			case catch mnesia:dirty_read(
				     roster, {User, Server, LRJID}) of
			    [#roster{subscription = Subscription,
				     groups = Groups}] ->
				{Subscription, Groups};
			    _ ->
				{none, []}
			end
		end
	end
    catch
	_ ->
	    {none, []}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden

update_table() ->
    Fields = record_info(fields, roster),
    case mnesia:table_info(roster, attributes) of
	Fields ->
	    convert_to_exmpp();
	[uj, user, jid, name, subscription, ask, groups, xattrs, xs] ->
	    convert_table1(Fields);
	[usj, us, jid, name, subscription, ask, groups, xattrs, xs] ->
	    convert_table2(Fields);
	_ ->
	    ?INFO_MSG("Recreating roster table", []),
	    mnesia:transform_table(roster, ignore, Fields)
    end.

%% @hidden

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
		   fun(#roster{usj = {U, {JID_U, JID_S, JID_R}}, us = U, xs = XS, askmessage = AM} = R, _) ->
			   U1 = convert_jid_to_exmpp(U),
			   JID_U1 = convert_jid_to_exmpp(JID_U),
			   JID_R1 = convert_jid_to_exmpp(JID_R),
			   JID1 = {JID_U1, JID_S, JID_R1},
			   XS1 = convert_xs_to_exmpp(XS),
                           AM1 = convert_askmessage_to_exmpp(AM),
			   mnesia:dirty_write(
			     mod_roster_tmp_table,
			     R#roster{usj = {U1, Host, JID1},
				      us = {U1, Host}, xs = XS1,
				      askmessage = AM1})
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

%% @hidden

%% Convert roster table: xattrs fields become 
convert_table2(Fields) ->
    ?INFO_MSG("Converting roster table from "
	      "{usj, us, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    mnesia:transform_table(roster, ignore, Fields),
    convert_to_exmpp().

%% @hidden

convert_to_exmpp() ->
    Fun = fun() ->
	case mnesia:first(roster) of
	    {_User, Server, _JID} when is_binary(Server) ->
		none;
	    {_User, Server, _JID} when is_list(Server) ->
		mnesia:foldl(fun convert_to_exmpp2/2,
		  done, roster, write);
	    '$end_of_table' ->
		none
	end
    end,
    mnesia:transaction(Fun).

%% @hidden

convert_to_exmpp2(#roster{
  usj = {USJ_U, USJ_S, {USJ_JU, USJ_JS, USJ_JR}} = Key,
  us = {US_U, US_S},
  jid = {JID_U, JID_S, JID_R},
  name = N, xs = XS, groups = G, askmessage = AM} = R, Acc) ->
    % Remove old entry.
    mnesia:delete({roster, Key}),
    % Convert "" to undefined in JIDs and string() to binary().
    USJ_U1  = convert_jid_to_exmpp(USJ_U),
    USJ_S1  = convert_jid_to_exmpp(USJ_S),
    USJ_JU1 = convert_jid_to_exmpp(USJ_JU),
    USJ_JS1 = convert_jid_to_exmpp(USJ_JS),
    USJ_JR1 = convert_jid_to_exmpp(USJ_JR),
    US_U1   = convert_jid_to_exmpp(US_U),
    US_S1   = convert_jid_to_exmpp(US_S),
    JID_U1  = convert_jid_to_exmpp(JID_U),
    JID_S1  = convert_jid_to_exmpp(JID_S),
    JID_R1  = convert_jid_to_exmpp(JID_R),
    % Convert name.
    N1 = convert_name_to_exmpp(N),
    % Convert groups.
    G1 = convert_groups_to_exmpp(G, []),
    % Convert xs.
    XS1 = convert_xs_to_exmpp(XS),
    % Convert askmessage.
    AM1 = convert_askmessage_to_exmpp(AM),
    % Prepare the new record.
    New_R = R#roster{
      usj = {USJ_U1, USJ_S1, {USJ_JU1, USJ_JS1, USJ_JR1}},
      us  = {US_U1, US_S1},
      jid = {JID_U1, JID_S1, JID_R1},
      name = N1, groups = G1, xs = XS1, askmessage = AM1},
    % Write the new record.
    mnesia:write(New_R),
    Acc.

%% @hidden

convert_jid_to_exmpp("")                  -> undefined;
convert_jid_to_exmpp(V) when is_list(V)   -> list_to_binary(V).

%% @hidden

convert_name_to_exmpp(N) when is_list(N)  -> list_to_binary(N).

%% @hidden

convert_groups_to_exmpp([G | Rest], New_G) ->
    convert_groups_to_exmpp(Rest, [list_to_binary(G) | New_G]);
convert_groups_to_exmpp([], New_G) ->
    lists:reverse(New_G).

%% @hidden

convert_xs_to_exmpp(Els) ->
    convert_xs_to_exmpp(Els, []).

%% @hidden

convert_xs_to_exmpp([El | Rest], Result) ->
    New_El = exmpp_xml:xmlelement_to_xmlel(El,
      [?NS_JABBER_CLIENT], [{?NS_XMPP, ?NS_XMPP_pfx}]),
    convert_xs_to_exmpp(Rest, [New_El | Result]);
convert_xs_to_exmpp([], Result) ->
    lists:reverse(Result).

%% @hidden

convert_askmessage_to_exmpp(AM) when is_binary(AM) ->
    AM;
convert_askmessage_to_exmpp(AM) ->
    list_to_binary(AM).

%% @spec (Acc, Host, Request) -> {stop, Result} | Acc
%%     Acc = term()
%%     Host = string()
%%     Request = ejabberd_http:request()
%%     Result = [ejabberd_web:html()]

webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "roster"],
		       q = Query,
		       lang = Lang} = _Request)
  when is_list(Host), is_list(U) ->
    Res = user_roster(list_to_binary(U), list_to_binary(Host), Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

%% @spec (User, Server, Query, Lang) -> Result
%%     User = binary()
%%     Server = binary()
%%     Query = ejabberd_http:query()
%%     Lang = string()
%%     Result = [ejabberd_web:html()]

user_roster(User, Server, Query, Lang) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
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
					TDJID = build_contact_jid_td(R#roster.jid),
					?XE("tr",
					    [TDJID,
					     ?XAC("td", [?XMLATTR('class', <<"valign">>)],
						  binary_to_list(R#roster.name)),
					     ?XAC("td", [?XMLATTR('class', <<"valign">>)],
						  atom_to_list(R#roster.subscription)),
					     ?XAC("td", [?XMLATTR('class', <<"valign">>)],
						  atom_to_list(Pending)),
					     ?XAE("td", [?XMLATTR('class', <<"valign">>)], Groups),
					     if
						 Pending == in ->
						     ?XAE("td", [?XMLATTR('class', <<"valign">>)],
							  [?INPUTT("submit",
								   "validate" ++
								   ejabberd_web_admin:term_to_id(R#roster.jid),
								   "Validate")]);
						 true ->
						     ?X("td")
					     end,
					     ?XAE("td", [?XMLATTR('class', <<"valign">>)],
						  [?INPUTT("submit",
							   "remove" ++
							   ejabberd_web_admin:term_to_id(R#roster.jid),
							   "Remove")])])
				end, SItems))])]
	    end,
	[?XC("h1", ?T("Roster of ") ++ us_to_list(US))] ++
	    case Res of
		ok -> [?XREST("Submitted")];
		error -> [?XREST("Bad format")];
		nothing -> []
	    end ++
	    [?XAE("form", [?XMLATTR('action', <<"">>), ?XMLATTR('method', <<"post">>)],
		  FItems ++
		  [?P,
		   ?INPUT("text", "newjid", ""), ?C(" "),
		   ?INPUTT("submit", "addjid", "Add Jabber ID")
		  ])]
      catch
	  _ ->
	      [?XC("h1", ?T("Roster of ") ++ us_to_list({User, Server}))] ++
	      [?CT("Bad format"), ?P] ++
	      [?XAE("form", [?XMLATTR('action', <<"">>), ?XMLATTR('method', <<"post">>)],
		    [?P,
		     ?INPUT("text", "newjid", ""), ?C(" "),
		     ?INPUTT("submit", "addjid", "Add Jabber ID")
		    ])]
      end.

%% @spec (JID) -> Result
%%     JID = jlib:shortjid()
%%     Result = ejabberd_web:html()

build_contact_jid_td({U, S, R}) ->
    %% Convert {U, S, R} into {jid, U, S, R, U, S, R}:
    ContactJID = exmpp_jid:make(U, S, R),
    JIDURI = case {exmpp_jid:prep_node(ContactJID), exmpp_jid:prep_domain(ContactJID)} of
		 {undefined, _} -> "";
		 {CUser, CServer} ->
		     CUser_S = binary_to_list(CUser),
		     CServer_S = binary_to_list(CServer),
		     case lists:member(CServer_S, ?MYHOSTS) of
			 false -> "";
			 true -> "/admin/server/" ++ CServer_S ++ "/user/" ++ CUser_S ++ "/"
		     end
	     end,
    case JIDURI of
	[] ->
	    ?XAC('td', [?XMLATTR('class', <<"valign">>)], exmpp_jid:to_list(ContactJID));
	URI when is_list(URI) ->
	    ?XAE('td', [?XMLATTR('class', <<"valign">>)], [?AC(JIDURI, exmpp_jid:to_list(ContactJID))])
    end.

%% @spec (User, Server, Items, Query) -> ok | nothing | error
%%     User = binary()
%%     Server = binary()
%%     Items = [rosteritem()]
%%     Query = ejabberd_http:query()

user_roster_parse_query(User, Server, Items, Query) ->
    case lists:keysearch("addjid", 1, Query) of
	{value, _} ->
	    case lists:keysearch("newjid", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, SJID}} ->
		    try
			JID = exmpp_jid:parse(SJID),
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

%% @spec (User, Server, JID) -> term()
%%     User = binary()
%%     Server = binary()
%%     JID = exmpp_jid:jid()

user_roster_subscribe_jid(User, Server, JID) ->
    out_subscription(User, Server, JID, subscribe),
    UJID = exmpp_jid:make(User, Server),
    ejabberd_router:route(
      UJID, JID, exmpp_presence:subscribe()).

%% @spec (User, Server, Items, Query) -> term()
%%     User = binary()
%%     Server = binary()
%%     Items = [rosteritem()]
%%     Query = ejabberd_http:query()

user_roster_item_parse_query(User, Server, Items, Query) ->
    lists:foreach(
      fun(Roster) ->
	      JID = Roster#roster.jid,
	      case lists:keysearch(
		     "validate" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
		  {value, _} ->
		      {U, S, R} = JID,
		      JID1 = exmpp_jid:make(U, S, R),
		      out_subscription(
			User, Server, JID1, subscribed),
		      UJID = exmpp_jid:make(User, Server),
		      ejabberd_router:route(
			UJID, JID1, exmpp_presence:subscribed()),
		      throw(submitted);
		  false ->
		      case lists:keysearch(
			     "remove" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
			  {value, _} ->
			      {U, S, R} = JID,
			      UJID = exmpp_jid:make(User, Server),
			      Attrs1 = exmpp_xml:set_attribute_in_list([],
				'jid', exmpp_jid:to_list(U, S, R)),
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

%% @spec ({User, Server}) -> string()
%%     User = binary()
%%     Server = binary()

us_to_list({User, Server}) ->
    exmpp_jid:bare_to_list(User, Server).

%% @spec (Acc, User, Server, Lang) -> New_Acc
%%     Acc = [ejabberd_web:html()]
%%     User = string()
%%     Server = string()
%%     Lang = string()
%%     New_Acc = [ejabberd_web:html()]

webadmin_user(Acc, _User, _Server, Lang) ->
    % `Lang' is used by the `T' macro, called from the `ACT' macro.
    Acc ++ [?XE("h3", [?ACT("roster/", "Roster")])].

