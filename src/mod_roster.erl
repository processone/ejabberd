%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

%%% @doc Roster management (Mnesia storage).
%%%
%%% Includes support for XEP-0237: Roster Versioning.
%%% The roster versioning follows an all-or-nothing strategy:
%%%  - If the version supplied by the client is the latest, return an empty response.
%%%  - If not, return the entire new roster (with updated version string).
%%% Roster version is a hash digest of the entire roster.
%%% No additional data is stored in DB.

%%% Database schema (version / storage / table)
%%%
%%% 2.1.x / mnesia / roster
%%%  usj = {Username::string(), Host::string(), {ContactUsername::string(), ContactServer::string(), ""}}
%%%  us = {Username::string(), Host::string()}
%%%  jid = {ContactUsername::string(), ContactServer::string(), ""}
%%%  name = ContactRosterName::string()
%%%  subscription = none | from | to | both
%%%  ask = none | out | in
%%%  groups = [GroupName::string()]
%%%  askmessage = binary()
%%%  xs = [xmlelement()]
%%%
%%% 2.1.x / odbc / rosterusers
%%%  username = varchar250
%%%  jid = varchar250
%%%  nick = text
%%%  subscription = character1
%%%  ask = character1
%%%  askmessage = text
%%%  server = character1
%%%  subscribe = text
%%%  type = text
%%% 2.1.x / odbc / rostergroups
%%%  username = varchar250
%%%  jid = varchar250
%%%  grp = text
%%%
%%% 3.0.0-prealpha / mnesia / roster
%%%  usj = {Username::binary(), Host::binary(), {ContactUsername::binary(), ContactServer::binary(), undefined}}
%%%  us = {Userma,e::binary(), Host::binary()}
%%%  jid = {ContactUsername::binary(), ContactServer::binary(), undefined}
%%%  name = ContactRosterName::binary()
%%%  subscription = none | from | to | both
%%%  ask = none | out | in
%%%  groups = [GroupName::binary()]
%%%  askmessage = binary()
%%%  xs = [xmlel()]
%%%
%%% 3.0.0-prealpha / odbc / rosterusers
%%% 3.0.0-prealpha / odbc / rostergroups
%%%  Same as 2.1.x
%%%
%%% 3.0.0-alpha / mnesia / rosteritem
%%%  user_host_jid = {Username::binary(), Host::binary(), {ContactUsername::binary(), ContactServer::binary(), undefined}}
%%%  name = ContactRosterName::binary()
%%%  subscription = none | from | to | both
%%%  ask = none | out | in
%%%  askmessage = binary()
%%% 3.0.0-alpha / mnesia / rostergroup
%%%  user_host_jid = {Username::binary(), Host::binary(), {ContactUsername::binary(), ContactServer::binary(), undefined}}
%%%  grp = GroupName::binary()
%%%
%%% 3.0.0-alpha / odbc / rosteritem
%%%  user = varchar250
%%%  host = varchar250
%%%  jid = varchar250
%%%  name = text
%%%  subscription = text = none | from | to | both
%%%  ask = text = none | out | in
%%%  askmessage = text
%%% 3.0.0-alpha / odbc / rostergroup
%%%  user = varchar250
%%%  host = varchar250
%%%  jid = varchar250
%%%  grp = varchar250

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
  
%% @spec (Host, Opts) -> term()
%%     Host = string()
%%     Opts = list()

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),

    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    gen_storage:create_table(Backend, HostB,
			     rosteritem, [{disc_copies, [node()]},
					  {odbc_host, HostB},
					  {attributes, record_info(fields, rosteritem)},
					  {types, [{user_host_jid, {text, text, ljid}},
						   {subscription, atom},
						   {ask, atom}]}]),
    gen_storage:create_table(Backend, HostB,
			     rostergroup, [{disc_copies, [node()]},
					   {odbc_host, HostB},
					   {type, bag},
					   {attributes, record_info(fields, rostergroup)},
					   {types, [{user_host_jid, {text, text, ljid}}]}]),
    mnesia:create_table(roster_version, [{disc_copies, [node()]},
				{attributes, record_info(fields, roster_version)}]),
    update_table(HostB, Backend),
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
    case ?IS_MY_HOST(LServer) of
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
    try_process_iq_set(From, To, IQ_Rec).

roster_hash(Items) ->
	sha:sha(term_to_binary(
		lists:sort(
			[R#roster{groups = lists:sort(Grs)} || 
				R = #roster{groups = Grs} <- Items]))).
		
%% @spec (Host::binary()) -> true | false
roster_versioning_enabled(Host)  ->
	gen_mod:get_module_opt(binary_to_list(Host), ?MODULE, versioning, false).

%% @spec (Host::binary()) -> true | false
roster_version_on_db(Host) ->
	gen_mod:get_module_opt(binary_to_list(Host), ?MODULE, store_current_id, false).

%% Returns a list that may contain an xmlelement with the XEP-237 feature if it's enabled.
get_versioning_feature(Acc, Host) ->
    case roster_versioning_enabled(Host) of
	true ->
	    Feature = exmpp_xml:element(?NS_ROSTER_VER_s, 'ver'),
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
%% @doc Load roster from DB only if neccesary.
%% It is neccesary if
%%  	- roster versioning is disabled in server OR
%%	- roster versioning is not used by the client OR
%%	- roster versioning is used by server and client BUT the server isn't storing version IDs on db OR
%%	- the roster version from client don't match current version
process_iq_get(From, To, IQ_Rec) ->
    US = {_, LServer} = {exmpp_jid:prep_node(From), exmpp_jid:prep_domain(From)},
    try
	    {ItemsToSend, VersionToSend} = 
		case {exmpp_xml:get_attribute_as_list(exmpp_iq:get_request(IQ_Rec), <<"ver">>,  not_found), 
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
				exmpp_iq:result(IQ_Rec, exmpp_xml:element(?NS_ROSTER, 'query', [?XMLATTR(<<"ver">>, Version)], Items))
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

get_user_roster(Acc, {U, S}) when is_binary(U), is_binary(S) ->
	LUser = exmpp_stringprep:nodeprep(U),
	LServer = exmpp_stringprep:nameprep(S),
    Items = get_roster(LUser, LServer),
    lists:filter(fun(#roster{subscription = none, ask = in}) ->
			 false;
		    (_) ->
			 true
		 end, Items) ++ Acc.

%% Reads the roster information from the database, and returns a list of #roster records.
get_roster(LUser, LServer) ->
    F = fun() ->
		U = gen_storage:select(LServer, rosteritem,
				       [{'=', user_host_jid, {LUser, LServer, '_'}}]),
		G = gen_storage:select(LServer, rostergroup,
				       [{'=', user_host_jid, {LUser, LServer, '_'}}]),
		[storageroster_to_roster(Rosteritem, G) ||
		    Rosteritem <- U]
	end,
    {atomic, Rs} = gen_storage:transaction(LServer, rosteritem, F),
    Rs.

storageroster_to_roster(#rosteritem{user_host_jid = {U, S, JID} = USJ,
				    name = Name,
				    subscription = Subscription,
				    ask = Ask,
				    askmessage = AskMessage},
			Rostergroups) ->
    US = {U, S},
    Groups =
	lists:foldl(
	  fun(#rostergroup{user_host_jid = USJ1, grp = G}, R)
	     when USJ =:= USJ1 ->
		  %% G is a string when using odbc beckend, and a binary when using mnesia
		  GString = convert_to_binary(G),
		  [GString | R];
	     (_, R) ->
		  R
	  end, [], Rostergroups),
    #roster{usj = {U, S, JID},
	    us = US,
	    jid = JID,
	    name = convert_to_binary(Name),
	    subscription = Subscription,
	    ask = Ask,
	    askmessage = AskMessage,
	    groups = Groups}.

convert_to_binary(A) when is_list(A) -> list_to_binary(A);
convert_to_binary(A) when is_binary(A) -> A.

%% @spec (Item) -> XML
%%     Item = rosteritem()
%%     XML = exmpp_xml:xmlel()

item_to_xml(Item) ->
    {U, S, R} = Item#roster.jid,
    Attrs1 = exmpp_xml:set_attribute_in_list([],
      <<"jid">>, exmpp_jid:to_binary(U, S, R)),
    Attrs2 = case Item#roster.name of
		 <<>> ->
		     Attrs1;
		 Name ->
		     exmpp_xml:set_attribute_in_list(Attrs1, <<"name">>, Name)
	     end,
    Attrs3 = exmpp_xml:set_attribute_in_list(Attrs2,
      <<"subscription">>, Item#roster.subscription),
    Attrs4 = case ask_to_pending(Item#roster.ask) of
		 out ->
		     exmpp_xml:set_attribute_in_list(Attrs3,
		       <<"ask">>, <<"subscribe">>);
		 both ->
		     exmpp_xml:set_attribute_in_list(Attrs3,
		       <<"ask">>, <<"subscribe">>);
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

try_process_iq_set(From, To, IQ) ->
    LServer = exmpp_jid:prep_domain_as_list(From),
    Access = gen_mod:get_module_opt(LServer, ?MODULE, access, all),
    case acl:match_rule(LServer, Access, From) of
	deny ->
	    exmpp_iq:error(IQ, 'not-allowed');
	allow ->
	    process_iq_set(From, To, IQ)
    end.

%% @spec (From, To, El) -> ok
%%    From = exmpp_jid:jid()
%%    To = exmpp_jid:jid()
%%    El = exmpp_xml:xmlel()

process_item_set(From, To, #xmlel{} = El) ->
    try
	JID1 = exmpp_jid:parse(exmpp_xml:get_attribute_as_binary(El, <<"jid">>, <<>>)),
	User = exmpp_jid:node(From),
	LUser = exmpp_jid:prep_node(From),
	LServer = exmpp_jid:prep_domain(From),
	JID = jlib:short_jid(JID1),
	LJID = jlib:short_prepd_jid(JID1),
	F = fun() ->
			Res = gen_storage:read(LServer,
					       {rosteritem, {LUser, LServer, LJID}}),
		    Item = case Res of
			       [] ->
				   #roster{usj = {LUser, LServer, LJID},
					   us = {LUser, LServer},
					   jid = JID};
				   [#rosteritem{subscription = Subscription,
						ask = Ask,
						askmessage = AskMessage}] ->
				       #roster{usj = {LUser, LServer, LJID},
					       us = {LUser, LServer},
					       jid = JID,
					       subscription = Subscription,
					       ask = Ask,
					       askmessage = AskMessage}
			   end,
		    Item1 = process_item_attrs(Item, El#xmlel.attrs),
		    Item2 = process_item_els(Item1, El#xmlel.children),
			case Item2 of
			    #roster{subscription = remove} ->
				gen_storage:delete(LServer, {rosteritem, {LUser, LServer, LJID}}),
				gen_storage:delete(LServer, {rostergroup, {LUser, LServer, LJID}});
			    #roster{name = Name,
				    subscription = Subscription2,
				    ask = Ask2,
				    askmessage = AskMessage2,
				    groups = Groups} ->
				I2 = #rosteritem{user_host_jid = {LUser, LServer, LJID},
						 name = Name,
						 subscription = Subscription2,
						 ask = Ask2,
						 askmessage = AskMessage2},
				gen_storage:write(LServer, I2),
				gen_storage:delete(LServer, {rostergroup, {LUser, LServer, LJID}}),
				lists:foreach(
				  fun(Group) ->
					  gen_storage:write(LServer,
							    #rostergroup{user_host_jid = {LUser, LServer, LJID},
									 grp = Group})
				  end, Groups)
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
        case gen_storage:transaction(LServer, rosteritem, F) of
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
	<<"name">> ->
	    process_item_attrs(Item#roster{name = Val}, Attrs);
	<<"subscription">> ->
	    case Val of
		<<"remove">> ->
		    process_item_attrs(Item#roster{subscription = remove},
				       Attrs);
		_ ->
		    process_item_attrs(Item, Attrs)
	    end;
	<<"ask">> ->
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
    {U, S, R2} = Item#roster.jid,
    %% the ?XMLATTR macro will convert 'undefined' to <<"undefined">> .. so here we use <<>> for bare jids.
    R = case R2 of 
        undefined -> <<>>;
        _ -> R2
    end,
    ejabberd_sm:route(exmpp_jid:make(),
		      exmpp_jid:make(User, Server),
		      #xmlel{name = 'broadcast', ns = roster_item, attrs =
		       [?XMLATTR(<<"u">>, U),
		        ?XMLATTR(<<"s">>, S),
		        ?XMLATTR(<<"r">>, R),
		        ?XMLATTR(<<"subs">>, Item#roster.subscription)]}),

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
push_item(User, Server, Resource, From, Item) ->
    push_item(User, Server, Resource, From, Item, not_found).

push_item(User, Server, Resource, From, Item, RosterVersion)
  when is_binary(User), is_binary(Server), is_binary(Resource),
  ?IS_JID(From) ->
    ExtraAttrs = case RosterVersion of
	not_found -> [];
	_ -> [?XMLATTR(<<"ver">>, RosterVersion)]
    end,
    Request = #xmlel{ns = ?NS_ROSTER, name = 'query',
      attrs = ExtraAttrs,
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
			  push_item(User, Server, Resource, From, Item, RosterVersion)
		end, ejabberd_sm:get_user_resources(User, Server)).

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
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	Items = gen_storage:dirty_select(LServer, rosteritem, [{'=', user_host_jid, {LUser, LServer, '_'}}]),
		fill_subscription_lists(Items, [], [])
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

fill_subscription_lists([#rosteritem{user_host_jid = {_, _, LJ},
				     subscription = Subscription} | Is],
			 F, T) ->
    case Subscription of
	both ->
	    fill_subscription_lists(Is, [LJ | F], [LJ | T]);
	from ->
	    fill_subscription_lists(Is, [LJ | F], T);
	to ->
	    fill_subscription_lists(Is, F, [LJ | T]);
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
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	LJID = jlib:short_prepd_jid(JID1),
	F = fun() ->
		    Item = case gen_storage:read(LServer, {rosteritem, {LUser, LServer, LJID}}) of
			       [] ->
				   #rosteritem{user_host_jid = {LUser, LServer, LJID}
					   };
			       [I] ->
				   I
			   end,
		    NewState = case Direction of
				   out ->
				       out_state_change(Item#rosteritem.subscription,
							Item#rosteritem.ask,
							Type);
				   in ->
				       in_state_change(Item#rosteritem.subscription,
						       Item#rosteritem.ask,
						       Type)
			       end,
		    AutoReply = case Direction of
				    out ->
					none;
				    in ->
					in_auto_reply(Item#rosteritem.subscription,
						      Item#rosteritem.ask,
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
			{none, none} when Item#rosteritem.subscription == none,
					  Item#rosteritem.ask == in ->
			    gen_storage:delete(LServer, {rosteritem, {LUser, LServer, LJID}}),
			    gen_storage:delete(LServer, {rostergroup, {LUser, LServer, LJID}}),
			    {none, AutoReply};
			{Subscription, Pending} ->
			    Groups = gen_storage:read(LServer,
						      {rostergroup, {LUser, LServer, LJID}}),
                            AskBinary = case AskMessage of
                                            undefined -> <<>>;
                                            B  -> B
                                        end,
			    NewItem = Item#rosteritem{subscription = Subscription,
						      ask = Pending,
						      askmessage = AskBinary},
			    gen_storage:write(LServer, NewItem),
			    NewItem1 = storageroster_to_roster(NewItem, Groups),
			    case roster_version_on_db(LServer) of
				    true -> mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
				    false -> ok
			    end,
			    {{push, NewItem1}, AutoReply}
		    end
	    end,
	case gen_storage:transaction(LServer, rosteritem, F) of
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
			    Item#rosteritem.subscription == none,
			    Item#rosteritem.ask == in ->
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
in_state_change(from, none, subscribed)   -> {both, none};
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
out_state_change(to,   none, subscribed)   -> {both, none};
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
        send_unsubscription_to_rosteritems(LUser, LServer),
	F = fun() ->
		    lists:foreach(
		      fun(R) ->
			      gen_storage:delete_object(LServer, R)
		      end,
		      gen_storage:select(LServer, rosteritem, [{'=', user_host_jid, {LUser, LServer, '_'}}])),
		    lists:foreach(
		      fun(R) ->
			      gen_storage:delete_object(LServer, R)
		      end,
		      gen_storage:select(LServer, rostergroup, [{'=', user_host_jid, {LUser, LServer, '_'}}]))
	    end,
	gen_storage:transaction(LServer, rosteritem, F)
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
	gen_storage:transaction(LServer, rosteritem, F)
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
	JID1 = exmpp_jid:parse(exmpp_xml:get_attribute_as_list(El, <<"jid">>, <<>>)),
	JID = jlib:short_jid(JID1),
	LJID = jlib:short_prepd_jid(JID1),
	Item = #roster{usj = {LUser, LServer, LJID},
		       us = {LUser, LServer},
		       jid = JID},
	Item1 = process_item_attrs_ws(Item, El#xmlel.attrs),
	Item2 = process_item_els(Item1, El#xmlel.children),
	    case Item2 of
		#roster{subscription = remove} ->
		    gen_storage:delete(LServer, {rosteritem, {LUser, LServer, LJID}}),
		    gen_storage:delete(LServer, {rostergroup, {LUser, LServer, LJID}});
		#roster{name = Name,
			subscription = Subscription,
			ask = Ask,
			askmessage = AskMessage,
			groups = Groups} ->
		    gen_storage:write(LServer, #rosteritem{user_host_jid = {LUser, LServer, LJID},
							   name = Name,
							   subscription = Subscription,
							   ask = Ask,
							   askmessage = AskMessage}),
		    gen_storage:delete(LServer, {rostergroup, {LUser, LServer, LJID}}),
		    lists:foreach(
		      fun(Group) ->
			      gen_storage:write(LServer,
						#rostergroup{user_host_jid = {LUser, LServer, LJID},
							     grp = Group})
		      end, Groups)
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
	<<"name">> ->
	    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
	<<"subscription">> ->
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
	<<"ask">> ->
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
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
    Result = gen_storage:dirty_select(LServer, rosteritem, [{'=', user_host_jid, {LUser, LServer, '_'}}]),
	    Ls ++ lists:map(
		    fun(#rosteritem{user_host_jid = {_, _, RJID},
				    askmessage = Message}) ->
			    Status  = if is_binary(Message) ->
					      binary_to_list(Message);
					 is_list(Message) ->
					      Message;
					 true ->
					      ""
				      end,
			    {U0, S0, R0} = RJID,
			    Pres1 = exmpp_presence:subscribe(),
			    Pres2 = exmpp_stanza:set_jids(Pres1,
			      exmpp_jid:to_binary(U0, S0, R0),
			      exmpp_jid:to_binary(JID)),
			    exmpp_presence:set_status(Pres2, Status)
		    end,
		    lists:filter(
		      fun(R) ->
			      case R#rosteritem.ask of
				  in   -> true;
				  both -> true;
				  _ -> false
			      end
		      end,
		      Result)).


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
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
    try
	F = fun() -> 
	    LJID = jlib:short_prepd_jid(JID),
	    LRJID = jlib:short_prepd_bare_jid(JID),
	    case catch gen_storage:read(LServer, {rosteritem, {LUser, LServer, LRJID}}) of
		[#rosteritem{subscription = Subscription}] ->
		    Groups =
			[Group || #rostergroup{grp = Group} <-
				      gen_storage:read(LServer, {rostergroup, {LUser, LServer, LRJID}})],
		    {Subscription, Groups};
		_ ->
		    if
			LRJID == LJID ->
			    {none, []};
			true ->
				    case catch gen_storage:dirty_read(
						 LServer, rosteritem,
						 {LUser, LServer, LRJID}) of
					[#rosteritem{subscription = Subscription}] ->
					    Groups =
						[Group || #rostergroup{grp = Group} <-
							      gen_storage:read(LServer, {rostergroup, {LUser, LServer, LRJID}})],
					    {Subscription, Groups};
					_ ->
					    {none, []}
				    end
		    end
	    end
    end,
	{atomic, Result} = gen_storage:transaction(LServer, rosteritem, F),
	Result
    catch
	_ ->
	    {none, []}
    end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Only supports migration from ejabberd 1.1.2 or higher.

update_table(global, Storage) ->
    [update_table(HostB, Storage) || HostB <- ejabberd_hosts:get_hosts(ejabberd)];

update_table(HostB, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      HostB, rosteritem,
      [{roster, [usj, us, jid, name, subscription, ask, groups, askmessage, xs],
	fun({roster, USJ, _, _, Name, Subscription, Ask, Groups, AskMessage, _Xs}) ->
		%% Convert "" to undefined in JIDs and string() to binary().
		{USJ_U, USJ_S, {USJ_JU, USJ_JS, USJ_JR}} = USJ,
		USJ_U1  = convert_jid_to_exmpp(USJ_U),
		USJ_S1  = convert_jid_to_exmpp(USJ_S),
		USJ_JU1 = convert_jid_to_exmpp(USJ_JU),
		USJ_JS1 = convert_jid_to_exmpp(USJ_JS),
		USJ_JR1 = convert_jid_to_exmpp(USJ_JR),
		USJ1 = {USJ_U1, USJ_S1, {USJ_JU1, USJ_JS1, USJ_JR1}},
		lists:foreach(
		  fun(Group) ->
			  Group2 = list_to_binary(Group),
			  gen_storage:write(HostB,
					    #rostergroup{user_host_jid = USJ1,
							 grp = Group2})
		  end, Groups),
		Name2 = convert_name_to_exmpp(Name),
	        AskMessage2 = convert_askmessage_to_exmpp(AskMessage),
		#rosteritem{user_host_jid = USJ1,
			    name = Name2,
			    subscription = Subscription,
			    ask = Ask,
			    askmessage = AskMessage2}
	end}
      ]);

update_table(Host, odbc) ->
    gen_storage_migration:migrate_odbc(
      Host, [rosteritem, rostergroup],
      [{"rosterusers", ["username", "jid", "nick",
			"subscription", "ask", "askmessage",
			"server", "subscribe", "type"],
	fun(SELECT,
	    Username, JID, Nick,
	    Subscription, Ask, AskMessage,
	    Server, Subscribe, Type) ->
		USJ = {Username, Host, JID},
		[#rosteritem{user_host_jid = USJ,
			     name = Nick,
			     subscription = case Subscription of
						"B" -> both;
						"T" -> to;
						"F" -> from;
						_ -> none
					    end,
			     ask = case Ask of
				       "S" -> subscribe;
				       "U" -> unsubscribe;
				       "B" -> both;
				       "O" -> out;
				       "I" -> in;
				       _ -> none
				   end,
			     askmessage = AskMessage}
		 | [#rostergroup{user_host_jid = USJ,
				 grp = Group}
		    || [Group] <- SELECT(["grp"], "rostergroups", [{"username", Username},
								   {"jid", JID}])]]
	end}]),
	ejabberd_odbc:sql_query(Host, "DROP TABLE rostergroups").

convert_jid_to_exmpp("")                  -> undefined;
convert_jid_to_exmpp(V) when is_list(V)   -> list_to_binary(V).

convert_name_to_exmpp(N) when is_list(N)  -> list_to_binary(N).

convert_askmessage_to_exmpp(AM) when is_binary(AM) ->
    AM;
convert_askmessage_to_exmpp(AM) ->
    list_to_binary(AM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	Items1 = get_roster(User, Server),
	Res = user_roster_parse_query(User, Server, Items1, Query),
	Items = get_roster(User, Server),
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
					     ?XAC("td", [?XMLATTR(<<"class">>, <<"valign">>)],
						  R#roster.name),
					     ?XAC("td", [?XMLATTR(<<"class">>, <<"valign">>)],
						  atom_to_list(R#roster.subscription)),
					     ?XAC("td", [?XMLATTR(<<"class">>, <<"valign">>)],
						  atom_to_list(Pending)),
					     ?XAE("td", [?XMLATTR(<<"class">>, <<"valign">>)], Groups),
					     if
						 Pending == in ->
						     ?XAE("td", [?XMLATTR(<<"class">>, <<"valign">>)],
							  [?INPUTT("submit",
								   "validate" ++
								   ejabberd_web_admin:term_to_id(R#roster.jid),
								   "Validate")]);
						 true ->
						     ?X("td")
					     end,
					     ?XAE("td", [?XMLATTR(<<"class">>, <<"valign">>)],
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
	    [?XAE("form", [?XMLATTR(<<"action">>, <<"">>), ?XMLATTR(<<"method">>, <<"post">>)],
		  FItems ++
		  [?P,
		   ?INPUT("text", "newjid", ""), ?C(" "),
		   ?INPUTT("submit", "addjid", "Add Jabber ID")
		  ])]
      catch
	  _ ->
	      [?XC("h1", ?T("Roster of ") ++ us_to_list({User, Server}))] ++
	      [?CT("Bad format"), ?P] ++
	      [?XAE("form", [?XMLATTR(<<"action">>, <<"">>), ?XMLATTR(<<"method">>, <<"post">>)],
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
		     case ?IS_MY_HOST(CServer_S) of
			 false -> "";
			 true -> "/admin/server/" ++ CServer_S ++ "/user/" ++ CUser_S ++ "/"
		     end
	     end,
    case JIDURI of
	[] ->
	    ?XAC('td', [?XMLATTR(<<"class">>, <<"valign">>)], exmpp_jid:to_list(ContactJID));
	URI when is_list(URI) ->
	    ?XAE('td', [?XMLATTR(<<"class">>, <<"valign">>)], [?AC(JIDURI, exmpp_jid:to_list(ContactJID))])
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
				<<"jid">>, exmpp_jid:to_list(U, S, R)),
			      Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1,
				<<"subscription">>, "remove"),
			      Item = #xmlel{ns = ?NS_ROSTER, name = 'item',
				attrs = Attrs2},
			      Request = #xmlel{
				ns = ?NS_ROSTER,
				name = 'query',
				children = [Item]},
			      process_iq_set(
				UJID, UJID,
				#iq{type = set, ns = ?NS_JABBER_CLIENT, payload = Request}),
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
