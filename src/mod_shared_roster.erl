%%%----------------------------------------------------------------------
%%% File    : mod_shared_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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

-module(mod_shared_roster).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 webadmin_menu/3, webadmin_page/3,
	 get_user_roster/2,
	 get_subscription_lists/3,
	 get_jid_info/4,
	 process_item/2,
	 in_subscription/6,
	 out_subscription/4,
	 user_available/1,
	 unset_presence/4,
	 register_user/2,
	 remove_user/2,
	 list_groups/1,
	 create_group/2,
	 create_group/3,
	 delete_group/2,
	 get_group_opts/2,
	 set_group_opts/3,
	 get_group_users/2,
	 get_group_explicit_users/2,
	 is_user_in_group/3,
	 add_user_to_group/3,
	 remove_user_from_group/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-record(sr_group, {group_host, opts}).
-record(sr_user, {us, group_host}).

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, _Opts) ->
    mnesia:create_table(sr_group,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, sr_group)}]),
    mnesia:create_table(sr_user,
			[{disc_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, sr_user)}]),
    mnesia:add_table_index(sr_user, group_host),
    ejabberd_hooks:add(webadmin_menu_host, HostB,
		       ?MODULE, webadmin_menu, 70),
    ejabberd_hooks:add(webadmin_page_host, HostB,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(roster_get, HostB,
		       ?MODULE, get_user_roster, 70),
    ejabberd_hooks:add(roster_in_subscription, HostB,
        	       ?MODULE, in_subscription, 30),
    ejabberd_hooks:add(roster_out_subscription, HostB,
        	       ?MODULE, out_subscription, 30),
    ejabberd_hooks:add(roster_get_subscription_lists, HostB,
		       ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:add(roster_get_jid_info, HostB,
        	       ?MODULE, get_jid_info, 70),
    ejabberd_hooks:add(roster_process_item, HostB,
		       ?MODULE, process_item, 50),
    ejabberd_hooks:add(user_available_hook, HostB,
		       ?MODULE, user_available, 50),
    ejabberd_hooks:add(unset_presence_hook, HostB,
		       ?MODULE, unset_presence, 50),
    ejabberd_hooks:add(register_user, HostB,
		       ?MODULE, register_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50).
%%ejabberd_hooks:add(remove_user, HostB,
%%    	       ?MODULE, remove_user, 50),

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(webadmin_menu_host, HostB,
			  ?MODULE, webadmin_menu, 70),
    ejabberd_hooks:delete(webadmin_page_host, HostB,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(roster_get, HostB,
			  ?MODULE, get_user_roster, 70),
    ejabberd_hooks:delete(roster_in_subscription, HostB,
        		  ?MODULE, in_subscription, 30),
    ejabberd_hooks:delete(roster_out_subscription, HostB,
        		  ?MODULE, out_subscription, 30),
    ejabberd_hooks:delete(roster_get_subscription_lists, HostB,
        		  ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:delete(roster_get_jid_info, HostB,
        		  ?MODULE, get_jid_info, 70),
    ejabberd_hooks:delete(roster_process_item, HostB,
			  ?MODULE, process_item, 50),
    ejabberd_hooks:delete(user_available_hook, HostB,
			  ?MODULE, user_available, 50),
    ejabberd_hooks:delete(unset_presence_hook, HostB,
			  ?MODULE, unset_presence, 50),
    ejabberd_hooks:delete(register_user, HostB,
			  ?MODULE, register_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50).
%%ejabberd_hooks:delete(remove_user, HostB,
%%    		  ?MODULE, remove_user, 50),


get_user_roster(Items, {U, S}) when is_binary(U) ->
    get_user_roster(Items, {binary_to_list(U), binary_to_list(S)});
get_user_roster(Items, US) ->
    {U, S} = US,
    DisplayedGroups = get_user_displayed_groups(US),
    %% Get shared roster users in all groups and remove self: 
    SRUsers = 
	lists:foldl(
	  fun(Group, Acc1) ->
		  GroupName = get_group_name(S, Group),
		  lists:foldl(
		    fun({User, Server} = UserServer, Acc2) ->
			    if UserServer == US -> Acc2;
			       true ->
				    UserServerB = {list_to_binary(User), list_to_binary(Server)},
				    dict:append(UserServerB,
						   GroupName,
						   Acc2)
			    end
		    end, Acc1, get_group_users(S, Group))
	  end, dict:new(), DisplayedGroups),

    %% If partially subscribed users are also in shared roster, show them as
    %% totally subscribed:
    {NewItems1, SRUsersRest} =
	lists:mapfoldl(
	  fun(Item, SRUsers1) ->
		  {_, _, {U1, S1, _}} = Item#roster.usj,
		  US1 = {U1, S1},
		  case dict:find(US1, SRUsers1) of
		      {ok, _GroupNames} ->
			  {Item#roster{subscription = both, ask = none},
			   dict:erase(US1, SRUsers1)};
		      error ->
			  {Item, SRUsers1}
		  end
	  end, SRUsers, Items),

    %% Export items in roster format:
    ModVcard = get_vcard_module(S),
    SRItems = [ #roster{usj = {U, S, {U1, S1, undefined}},
		       us = {U, S},
		       jid = {U1, S1, undefined},
		       name = get_rosteritem_name(ModVcard, U1, S1),
		       subscription = both,
		       ask = none,
		       groups = GroupNames} ||
		  {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

get_vcard_module(Server) ->
    Modules = gen_mod:loaded_modules(Server),
    [M || M <- Modules,
	  (M == mod_vcard) or (M == mod_vcard_ldap)].

get_rosteritem_name([], _, _) ->
    <<>>;
get_rosteritem_name([ModVcard], U, S) ->
    From = exmpp_jid:make("", S, "mod_shared_roster"),
    To = exmpp_jid:make(U, S, ""),
    Payload = #xmlel{ns = ?NS_VCARD, name = 'vCard'},
    IQ = #iq{kind = request,
        type = get,
        ns = ?NS_VCARD,
        payload = Payload},
    IQ_Vcard = ModVcard:process_sm_iq(From, To, IQ),
    try get_rosteritem_name_vcard(exmpp_iq:get_payload(IQ_Vcard))
    catch E1:E2 ->
	    ?ERROR_MSG("Error ~p found when trying to get the vCard of ~s@~s "
		       "in ~p:~n ~p", [E1, U, S, ModVcard, E2]),
	    <<>>
    end.

get_rosteritem_name_vcard(undefined) ->
    <<>>;
get_rosteritem_name_vcard(Vcard) ->
    case exmpp_xml:get_path(Vcard, [{element, "NICKNAME"}, cdata]) of
	<<>> -> exmpp_xml:get_path(Vcard, [{element, "FN"}, cdata]);
	Nickname -> Nickname
    end.

%% This function rewrites the roster entries when moving or renaming
%% them in the user contact list.
process_item(RosterItem, Host) ->
    USFrom = {UserFrom, ServerFrom} = RosterItem#roster.us,
    {UserTo, ServerTo, ResourceTo} = RosterItem#roster.jid,
    NameTo = RosterItem#roster.name,
    USTo = {UserTo, ServerTo},
    DisplayedGroups = get_user_displayed_groups(USFrom),
    CommonGroups = lists:filter(fun(Group) ->
					is_user_in_group(USTo, Group, Host)
				end, DisplayedGroups),
    case CommonGroups of
	[] -> RosterItem;
	%% Roster item cannot be removed: We simply reset the original groups:
	_ when RosterItem#roster.subscription == remove ->
	    GroupNames = lists:map(fun(Group) ->
					   get_group_name(Host, Group)
				   end, CommonGroups),
	    RosterItem#roster{subscription = both, ask = none,
			      groups=[GroupNames]};
	%% Both users have at least a common shared group,
	%% So each user can see the other
	_ ->
	    %% Check if the list of groups of the new roster item
	    %% include at least a new one
	    case lists:subtract(RosterItem#roster.groups, CommonGroups) of
                %% If it doesn't, then remove this user from any
                %% existing roster groups.
		[] ->
                    %% Remove pending subscription by setting it
                    %% unsubscribed.

                    %% Remove pending out subscription
                    mod_roster:out_subscription(UserTo, ServerTo,
                                         exmpp_jid:make(UserFrom, ServerFrom),
                                         unsubscribe),

                    %% Remove pending in subscription
                    mod_roster:in_subscription(aaaa, UserFrom, ServerFrom,
                                        exmpp_jid:make(UserTo, ServerTo),
                                        unsubscribe, ""),

                    %% But we're still subscribed, so respond as such.
		    RosterItem#roster{subscription = both, ask = none};
		%% If so, it means the user wants to add that contact
		%% to his personal roster
		PersonalGroups ->
		    %% Store roster items in From and To rosters
		    set_new_rosteritems(UserFrom, ServerFrom,
					UserTo, ServerTo, ResourceTo, NameTo,
					PersonalGroups)
	    end
    end.

build_roster_record(User1, Server1, User2, Server2, Name2, Groups) ->
    USR2 = {User2, Server2, undefined},
    #roster{usj = {User1, Server1, USR2},
	    us = {User1, Server1},
	    jid = USR2,
	    name = Name2,
	    subscription = both,
	    ask = none,
	    groups = Groups
	   }.

set_new_rosteritems(UserFrom, ServerFrom,
		    UserTo, ServerTo, ResourceTo, NameTo, GroupsFrom) ->
    RIFrom = build_roster_record(UserFrom, ServerFrom,
				 UserTo, ServerTo, NameTo, GroupsFrom),
    set_item(UserFrom, ServerFrom, ResourceTo, RIFrom),
    JIDTo = exmpp_jid:make(UserTo, ServerTo),

    JIDFrom = exmpp_jid:make(UserFrom, ServerFrom),
    RITo = build_roster_record(UserTo, ServerTo,
			       UserFrom, ServerFrom, UserFrom,[]),
    set_item(UserTo, ServerTo, undefined, RITo),

    %% From requests
    mod_roster:out_subscription(UserFrom, ServerFrom, JIDTo, subscribe),
    mod_roster:in_subscription(aaa, UserTo, ServerTo, JIDFrom, subscribe, ""),

    %% To accepts
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom, subscribed),
    mod_roster:in_subscription(aaa, UserFrom, ServerFrom, JIDTo, subscribed, ""),

    %% To requests
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom, subscribe),
    mod_roster:in_subscription(aaa, UserFrom, ServerFrom, JIDTo, subscribe, ""),

    %% From accepts
    mod_roster:out_subscription(UserFrom, ServerFrom, JIDTo, subscribed),
    mod_roster:in_subscription(aaa, UserTo, ServerTo, JIDFrom, subscribed, ""),

    RIFrom.

set_item(User, Server, Resource, Item) ->
    Request = #xmlel{ns = ?NS_ROSTER, name = 'query',
      children = [mod_roster:item_to_xml(Item)]},
    ResIQ = exmpp_iq:set(?NS_JABBER_CLIENT, Request,
      "push" ++ randoms:get_string()),
    ejabberd_router:route(
      exmpp_jid:make(User, Server, Resource),
      exmpp_jid:make(Server),
      ResIQ).


get_subscription_lists({F, T}, User, Server)
        when is_binary(User), is_binary(Server) ->
    try
	LUser = binary_to_list(User),
	LServer = binary_to_list(Server),
	US = {LUser, LServer},
	DisplayedGroups = get_user_displayed_groups(US),
	SRUsers =
	    lists:usort(
	      lists:flatmap(
		fun(Group) ->
			get_group_users(LServer, Group)
		end, DisplayedGroups)),
	SRJIDs = [{list_to_binary(U1), list_to_binary(S1), undefined} || {U1, S1} <- SRUsers],
	{lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T)}
    catch
	_ ->
	    {[], []}
    end.

get_jid_info({Subscription, Groups}, User, Server, JID)
        when is_binary(User), is_binary(Server) ->
    try
	LUser = binary_to_list(User),
	LServer = binary_to_list(Server),
	US = {LUser, LServer},
	{U1, S1, _} = jlib:short_prepd_jid(JID),
	US1 = {U1, S1},
	DisplayedGroups = get_user_displayed_groups(US),
	SRUsers = 
	    lists:foldl(
	      fun(Group, Acc1) ->
		      lists:foldl(
			fun(User1, Acc2) ->
				dict:append(
				  User1, get_group_name(LServer, Group), Acc2)
			end, Acc1, get_group_users(LServer, Group))
	      end, dict:new(), DisplayedGroups),
	case dict:find(US1, SRUsers) of
	    {ok, GroupNames} ->
		NewGroups = if
				Groups == [] -> GroupNames;
				true -> Groups
			    end,
		{both, NewGroups};
	    error ->
		{Subscription, Groups}
	end
    catch
	_ ->
	    {[], []}
    end.

in_subscription(Acc, User, Server, JID, Type, _Reason) ->
    process_subscription(in, User, Server, JID, Type, Acc).

out_subscription(UserFrom, ServerFrom, JIDTo, unsubscribed) ->
    {UserTo, ServerTo, _} = jlib:short_prepd_bare_jid(JIDTo),
    case ejabberd_hosts:registered(binary_to_list(ServerTo)) of
	true ->
	    %% Remove pending out subscription
	    JIDFrom = exmpp_jid:make(UserFrom, ServerFrom),
	    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom, unsubscribe);
	false ->
	    ok
    end,
    case ejabberd_hosts:registered(binary_to_list(ServerFrom)) of
	true ->
	    %% Remove pending in subscription
	    mod_roster:in_subscription(aaaa, UserFrom, ServerFrom, JIDTo, unsubscribe, "");
	false ->
	    ok
    end,
    process_subscription(out, UserFrom, ServerFrom, JIDTo, unsubscribed, false);
out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, false).

process_subscription(Direction, User, Server, JID, _Type, Acc) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	{U1, S1, _} = jlib:short_prepd_bare_jid(JID),
	US1 = {U1, S1},
	DisplayedGroups = get_user_displayed_groups(US),
	SRUsers =
	    lists:usort(
	      lists:flatmap(
		fun(Group) ->
			get_group_users(LServer, Group)
		end, DisplayedGroups)),
	case lists:member(US1, SRUsers) of
	    true ->
		case Direction of
		    in ->
			{stop, false};
		    out ->
			stop
		end;
	    false ->
		Acc
	end
    catch
	_ ->
	    Acc
    end.

%% @spec(Host::string) -> [Group::string()]
list_groups(Host) ->
    mnesia:dirty_select(
      sr_group,
      [{#sr_group{group_host = {'$1', '$2'},
		  _ = '_'},
	[{'==', '$2', Host}],
	['$1']}]).

%% @spec(Host::string) -> [Group::string()]
groups_with_opts(HostB) when is_binary(HostB) ->
    groups_with_opts(binary_to_list(HostB));
groups_with_opts(Host) ->
    Gs = mnesia:dirty_select(
	   sr_group,
	   [{#sr_group{group_host={'$1', Host}, opts='$2', _='_'},
	     [],
	     [['$1','$2']] }]),
    lists:map(fun([G,O]) -> {G, O} end, Gs).

create_group(Host, Group) ->
    create_group(Host, Group, []).

create_group(Host, Group, Opts) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun() ->
		mnesia:write(R)
	end,
    mnesia:transaction(F).

delete_group(Host, Group) ->
    GroupHost = {Group, Host},
    F = fun() ->
		%% Delete the group ...
		mnesia:delete({sr_group, GroupHost}),
		%% ... and its users
		Users = mnesia:index_read(sr_user, GroupHost, #sr_user.group_host),
		lists:foreach(fun(UserEntry) ->
				      mnesia:delete_object(UserEntry)
			      end, Users)
	end,
    mnesia:transaction(F).

get_group_opts(Host, Group) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    Opts;
	_ ->
	    error
    end.

set_group_opts(Host, Group, Opts) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun() ->
		mnesia:write(R)
	end,
    mnesia:transaction(F).

get_user_groups(US) ->
    Host = element(2, US),
    case catch mnesia:dirty_read(sr_user, US) of
	Rs when is_list(Rs) ->
	    [Group || #sr_user{group_host = {Group, H}} <- Rs, H == Host];
	_ ->
	    []
    end ++ get_special_users_groups(Host).

is_group_enabled(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    not lists:member(disabled, Opts);
	_ ->
	    false
    end.

%% @spec (Host::string(), Group::string(), Opt::atom(), Default) -> OptValue | Default
get_group_opt(Host, Group, Opt, Default) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    case lists:keysearch(Opt, 1, Opts) of
		{value, {_, Val}} ->
		    Val;
		false ->
		    Default
	    end;
	_ ->
	    Default
    end.

get_online_users(Host) ->
    lists:usort([{binary_to_list(U), binary_to_list(S)}
		 || {U, S, _} <-
			ejabberd_sm:get_vh_session_list(list_to_binary(Host))]).

%% @spec(Host::string(), Group::string()) -> [{Username::string(), Server::string()}]
get_group_users(HostB, Group) when is_binary(HostB) ->
    get_group_users(binary_to_list(HostB), Group);

get_group_users(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    case get_group_opt(Host, Group, all_users, false) of
	true ->
	    ejabberd_auth:get_vh_registered_users(Host);
	false ->
	    []
    end ++
    case get_group_opt(Host, Group, online_users, false) of
	true ->
	    get_online_users(Host);
	false ->
	    []
    end ++
    get_group_explicit_users(Host, Group).

%% @spec(Host::string(), Group::string(), GroupOpts) -> [{Username::binary(), Server::binary()}]
get_group_users(HostB, Group, GroupOpts) when is_binary(HostB) ->
    get_group_users(binary_to_list(HostB), Group, GroupOpts);
get_group_users(Host, Group, GroupOpts) ->
    case proplists:get_value(all_users, GroupOpts, false) of
	true ->
	    ejabberd_auth:get_vh_registered_users(Host);
	false ->
	    []
    end ++
    case proplists:get_value(online_users, GroupOpts, false) of
	true ->
	    get_online_users(Host);
	false ->
	    []
    end ++
    get_group_explicit_users(Host, Group).

%% @spec (Host::string(), Group::string()) -> [{User::string(), Server::string()}]
get_group_explicit_users(Host, Group) ->
    Read = (catch mnesia:dirty_index_read(
		    sr_user,
		    {Group, Host},
		    #sr_user.group_host)),
    case Read of
	Rs when is_list(Rs) ->
	    [R#sr_user.us || R <- Rs];
	_ ->
	    []
    end.

get_group_name(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    get_group_opt(Host, Group, name, Group).

%% @spec(Host::string)
%% @doc Get list of names of groups that have @all@ or @online@ in the memberlist
get_special_users_groups(HostB) when is_binary(HostB)->
    get_special_users_groups(binary_to_list(HostB));
get_special_users_groups(Host) ->
    lists:filter(
      fun(Group) ->
	      get_group_opt(Host, Group, all_users, false)
		  orelse get_group_opt(Host, Group, online_users, false)
      end,
      list_groups(Host)).

%% Get list of names of groups that have @online@ in the memberlist
get_special_users_groups_online(Host) ->
    lists:filter(
      fun(Group) ->
	      get_group_opt(Host, Group, online_users, false)
      end,
      list_groups(Host)).

%% Given two lists of groupnames and their options,
%% return the list of displayed groups to the second list
displayed_groups(GroupsOpts, SelectedGroupsOpts) ->
    DisplayedGroups =
	lists:usort(
	  lists:flatmap(
	    fun({_Group, Opts}) ->
		    [G || G <- proplists:get_value(displayed_groups, Opts, []),
			  not lists:member(disabled, Opts)]
	    end, SelectedGroupsOpts)),
    [G || G <- DisplayedGroups,
	  not lists:member(disabled, proplists:get_value(G, GroupsOpts, []))].

%% Given a list of group names with options,
%% for those that have @all@ in memberlist,
%% get the list of groups displayed
get_special_displayed_groups(GroupsOpts) ->
    Groups = lists:filter(
	       fun({_Group, Opts}) ->
		       proplists:get_value(all_users, Opts, false)
	       end, GroupsOpts),
    displayed_groups(GroupsOpts, Groups).

%% Given a username and server, and a list of group names with options,
%% for the list of groups of that server that user is member
%% get the list of groups displayed
%% @spec (LUser::string(), LServer::string(), GroupOpts::[{GroupName::string(), [Opt]}]) -> [{GroupName::string(), [Opt]}]
get_user_displayed_groups(LUser, LServer, GroupsOpts) ->
    Groups = case catch mnesia:dirty_read(sr_user, {LUser, LServer}) of
		 Rs when is_list(Rs) ->
		     [{Group, proplists:get_value(Group, GroupsOpts, [])} ||
			 #sr_user{group_host = {Group, H}} <- Rs, H == LServer];
		 _ ->
		     []
	     end,
    displayed_groups(GroupsOpts, Groups).

%% @doc Get the list of groups that are displayed to this user
get_user_displayed_groups(US) ->
    Host = element(2, US),
    DisplayedGroups1 =
	lists:usort(
	  lists:flatmap(
	    fun(Group) ->
		    case is_group_enabled(Host, Group) of
			true ->
			    get_group_opt(Host, Group, displayed_groups, []);
			false ->
			    []
		    end
	    end, get_user_groups(US))),
    [Group || Group <- DisplayedGroups1, is_group_enabled(Host, Group)].

is_user_in_group(US, Group, Host) ->
    case catch mnesia:dirty_match_object(
		 #sr_user{us=US, group_host={Group, Host}}) of
        [] -> lists:member(US, get_group_users(Host, Group));
	_  -> true
    end.


%% @spec (Host::string(), {User::string(), Server::string()}, Group::string()) -> {atomic, ok}
add_user_to_group(Host, {undefined, LServer}, Group) ->
    add_user_to_group(Host, {"", LServer}, Group);
add_user_to_group(Host, {LUser, LServer} = US, Group) ->
    case re:run(LUser, "^@.+@$", [{capture, none}]) of
	match ->
	    GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
	    MoreGroupOpts =
		case LUser of
		    "@all@" -> [{all_users, true}];
		    "@online@" -> [{online_users, true}];
		    _ -> []
		end,
            mod_shared_roster:set_group_opts(
	      Host, Group,
	      GroupOpts ++ MoreGroupOpts);
	nomatch ->
	    %% Push this new user to members of groups where this group is displayed
	    push_user_to_displayed(LUser, LServer, Group, Host, both),
	    %% Push members of groups that are displayed to this group
	    push_displayed_to_user(LUser, LServer, Group, Host, both),
	    R = #sr_user{us = US, group_host = {Group, Host}},
	    F = fun() ->
			mnesia:write(R)
	        end,
	    mnesia:transaction(F)
    end.

push_displayed_to_user(LUser, LServer, Group, Host, Subscription) ->
    GroupsOpts = groups_with_opts(LServer),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    DisplayedGroups = proplists:get_value(displayed_groups, GroupOpts, []),
    [push_members_to_user(LUser, LServer, DGroup, Host, Subscription) || DGroup <- DisplayedGroups].

remove_user_from_group(Host, {undefined, LServer}, Group) ->
    remove_user_from_group(Host, {"", LServer}, Group);
remove_user_from_group(Host, {LUser, LServer} = US, Group) ->
    GroupHost = {Group, Host},
    case re:run(LUser, "^@.+@$", [{capture, none}]) of
	match ->
	    GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
	    NewGroupOpts =
		case LUser of
		    "@all@" ->
			lists:filter(fun(X) -> X/={all_users,true} end, GroupOpts);
		    "@online@" ->
			lists:filter(fun(X) -> X/={online_users,true} end, GroupOpts)
		end,
	    mod_shared_roster:set_group_opts(Host, Group, NewGroupOpts);
	nomatch ->
	    R = #sr_user{us = US, group_host = GroupHost},
	    F = fun() ->
			mnesia:delete_object(R)
		end,
	    Result = mnesia:transaction(F),
	    %% Push removal of the old user to members of groups where the group that this user was members was displayed
	    push_user_to_displayed(LUser, LServer, Group, Host, remove),
	    %% Push removal of members of groups that where displayed to the group which this user has left
	    push_displayed_to_user(LUser, LServer, Group, Host, remove),
	    Result
    end.


push_members_to_user(LUser, LServer, Group, Host, Subscription) ->
    GroupsOpts = groups_with_opts(LServer),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    GroupName = proplists:get_value(name, GroupOpts, Group),
    Members = get_group_users(Host, Group),
    lists:foreach(
      fun({U, S}) ->
	      push_roster_item(LUser, LServer, U, S, GroupName, Subscription)
      end, Members).

register_user(User, Server) ->
    %% Get list of groups where this user is member
    Groups = get_user_groups({User, Server}),
    %% Push this user to members of groups where is displayed a group which this user is member
    [push_user_to_displayed(User, Server, Group, Server, both) || Group <- Groups].

remove_user(User, Server) ->
    push_user_to_members(User, Server, remove).

push_user_to_members(User, Server, Subscription) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	GroupsOpts = groups_with_opts(LServer),
	SpecialGroups = get_special_displayed_groups(GroupsOpts),
	LUserS = binary_to_list(LUser),
	LServerS = binary_to_list(LServer),
	UserGroups = get_user_displayed_groups(LUserS, LServerS, GroupsOpts),
	lists:foreach(
	  fun(Group) ->
		  remove_user_from_group(LServerS, {LUserS, LServerS}, Group),
		  GroupOpts = proplists:get_value(Group, GroupsOpts, []),
		  GroupName = proplists:get_value(name, GroupOpts, Group),
		  lists:foreach(
		    fun({U, S}) ->
                            push_roster_item(U, S, LUser, LServer, GroupName, Subscription)
		    end, get_group_users(LServer, Group, GroupOpts))
	  end, lists:usort(SpecialGroups++UserGroups))
    catch
	_ ->
	    ok
    end.

push_user_to_displayed(User, Server, Group, Host, Subscription) when is_binary(User) ->
    LUser = binary_to_list(User),
    LServer = binary_to_list(Server),
    push_user_to_displayed(LUser, LServer, Group, Host, Subscription);
push_user_to_displayed(LUser, LServer, Group, Host, Subscription) ->
    GroupsOpts = groups_with_opts(Host),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    GroupName = proplists:get_value(name, GroupOpts, Group),
    DisplayedToGroupsOpts = displayed_to_groups(Group, Host),
    [push_user_to_group(LUser, LServer, GroupD, Host, GroupName, Subscription) || {GroupD, _Opts} <- DisplayedToGroupsOpts].

push_user_to_group(LUser, LServer, Group, Host, GroupName, Subscription) ->
    lists:foreach(
      fun({U, S})  when (U == LUser) and (S == LServer) -> ok;
         ({U, S}) ->
	      push_roster_item(U, S, LUser, LServer, GroupName, Subscription)
      end, get_group_users(Host, Group)).

%% Get list of groups to which this group is displayed
displayed_to_groups(GroupName, LServer) ->
    GroupsOpts = groups_with_opts(LServer),
    lists:filter(
      fun({_Group, Opts}) ->
	      lists:member(GroupName, proplists:get_value(displayed_groups, Opts, []))
      end, GroupsOpts).

push_item(User, Server, From, Item) ->
    %% It was
    %%  ejabberd_sm:route(jlib:make_jid("", "", ""),
    %%                    jlib:make_jid(User, Server, "")
    %% why?
    {U, S, R} = Item#roster.jid,
    ejabberd_sm:route(From, exmpp_jid:make(User, Server),
		      #xmlel{name = 'broadcast', ns = roster_item, attrs =
		       [?XMLATTR(<<"u">>, U),
		        ?XMLATTR(<<"s">>, S),
		        ?XMLATTR(<<"r">>, R),
		        ?XMLATTR(<<"subs">>, Item#roster.subscription)]}),
    Request = #xmlel{ns = ?NS_ROSTER, name = 'query',
      children = [mod_roster:item_to_xml(Item)]},
    Stanza = exmpp_iq:set(?NS_JABBER_CLIENT, Request,
      "push" ++ randoms:get_string()),
    lists:foreach(
      fun(Resource) ->
	      JID = exmpp_jid:make(User, Server, Resource),
	      ejabberd_router:route(JID, JID, Stanza)
      end, ejabberd_sm:get_user_resources(list_to_binary(User), list_to_binary(Server))).

push_roster_item(User, Server, ContactU, ContactS, GroupName, Subscription) ->
    Item = #roster{usj = {User, Server, {ContactU, ContactS, ""}},
		   us = {User, Server},
		   jid = {ContactU, ContactS, ""},
		   name = "",
		   subscription = Subscription,
		   ask = none,
		   groups = [GroupName]},
    push_item(User, Server, exmpp_jid:make(Server), Item).

user_available(New) ->
    LUser = exmpp_jid:node(New),
    LServer = exmpp_jid:domain(New),
    Resources = ejabberd_sm:get_user_resources(LUser, LServer),
    ?DEBUG("user_available for ~p @ ~p (~p resources)",
	   [LUser, LServer, length(Resources)]),
    case length(Resources) of
	%% first session for this user
	1 ->
	    %% This is a simplification - we ignore he 'display'
	    %% property - @online@ is always reflective.
	    OnlineGroups = get_special_users_groups_online(binary_to_list(LServer)),
	    lists:foreach(
	      fun(OG) ->
		      ?DEBUG("user_available: pushing  ~p @ ~p grp ~p",
			     [LUser, LServer, OG ]),
		      push_user_to_displayed(LUser, LServer, OG, LServer, both)
	      end, OnlineGroups);
	_ ->
	    ok
    end.

unset_presence(User, Server, Resource, Status) ->
    LUser = binary_to_list(User),
    LServer = binary_to_list(Server),
    Resources = ejabberd_sm:get_user_resources(User, Server),
    ?DEBUG("unset_presence for ~p @ ~p / ~p -> ~p (~p resources)",
	   [LUser, LServer, Resource, Status, length(Resources)]),
    %% if user has no resources left...
    case length(Resources) of
	0 ->
	    %% This is a simplification - we ignore he 'display'
	    %% property - @online@ is always reflective.
	    OnlineGroups = get_special_users_groups_online(LServer),
	    %% for each of these groups...
	    lists:foreach(
	      fun(OG) ->
		      %% Push removal of the old user to members of groups
		      %% where the group that this uwas members was displayed
		      push_user_to_displayed(LUser, LServer, OG, LServer, remove),
		      %% Push removal of members of groups that where
		      %% displayed to the group which thiuser has left
		      push_displayed_to_user(LUser, LServer, OG, LServer,remove)
	      end, OnlineGroups);
	_ ->
	    ok
    end.

%%---------------------
%% Web Admin
%%---------------------

webadmin_menu(Acc, _Host, Lang) ->
    [{"shared-roster", ?T("Shared Roster Groups")} | Acc].

webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["shared-roster"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = list_shared_roster_groups(Host, Query, Lang),
    {stop, Res};

webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["shared-roster", Group],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = shared_roster_group(Host, Group, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

list_shared_roster_groups(Host, Query, Lang) ->
    Res = list_sr_groups_parse_query(Host, Query),
    SRGroups = mod_shared_roster:list_groups(Host),
    FGroups =
	?XAE("table", [],
	     [?XE("tbody",
		  lists:map(
		    fun(Group) ->
			    ?XE("tr",
				[?XE("td", [?INPUT("checkbox", "selected",
						   Group)]),
				 ?XE("td", [?AC(Group ++ "/", Group)])
				]
			       )
		    end, lists:sort(SRGroups)) ++
		  [?XE("tr",
		       [?X("td"),
			?XE("td", [?INPUT("text", "namenew", "")]),
			?XE("td", [?INPUTT("submit", "addnew", "Add New")])
		       ]
		      )]
		 )]),
    ?H1GL(?T("Shared Roster Groups"), "modsharedroster", "mod_shared_roster") ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
	    nothing -> []
	end ++
	[?XAE("form", [?XMLATTR(<<"action">>, <<"">>), ?XMLATTR(<<"method">>, <<"post">>)],
	      [FGroups,
	       ?BR,
	       ?INPUTT("submit", "delete", "Delete Selected")
	      ])
	].

list_sr_groups_parse_query(Host, Query) ->
    case lists:keysearch("addnew", 1, Query) of
	{value, _} ->
	    list_sr_groups_parse_addnew(Host, Query);
	_ ->
	    case lists:keysearch("delete", 1, Query) of
		{value, _} ->
		    list_sr_groups_parse_delete(Host, Query);
		_ ->
		    nothing
	    end
    end.

list_sr_groups_parse_addnew(Host, Query) ->
    case lists:keysearch("namenew", 1, Query) of
	{value, {_, Group}} when Group /= "" ->
	    mod_shared_roster:create_group(Host, Group),
	    ok;
	_ ->
	    error
    end.

list_sr_groups_parse_delete(Host, Query) ->
    SRGroups = mod_shared_roster:list_groups(Host),
    lists:foreach(
      fun(Group) ->
	      case lists:member({"selected", Group}, Query) of
		  true ->
		      mod_shared_roster:delete_group(Host, Group);
		  _ ->
		      ok
	      end
      end, SRGroups),
    ok.


shared_roster_group(Host, Group, Query, Lang) ->
    Res = shared_roster_group_parse_query(Host, Group, Query),
    GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
    Name = get_opt(GroupOpts, name, ""),
    Description = get_opt(GroupOpts, description, ""),
    AllUsers = get_opt(GroupOpts, all_users, false),
    OnlineUsers = get_opt(GroupOpts, online_users, false),
    %%Disabled = false,
    DisplayedGroups = get_opt(GroupOpts, displayed_groups, []),
    Members = mod_shared_roster:get_group_explicit_users(Host, Group),
    FMembers =
	if
	    AllUsers ->
		"@all@\n";
	    true ->
		[]
	end ++
	if
	    OnlineUsers ->
		"@online@\n";
	    true ->
		[]
	end ++
	[[us_to_list(Member), $\n] || Member <- Members],
    FDisplayedGroups = [[DG, $\n] || DG <- DisplayedGroups],
    DescNL = length(re:split(Description, "\n", [{return, list}])),
    FGroup =
	?XAE("table", [?XMLATTR(<<"class">>, <<"withtextareas">>)],
	     [?XE("tbody",
		  [?XE("tr",
		       [?XCT("td", "Group ID:"),
			?XE("td", [?C(Group)])
		       ]
		      ),
		   ?XE("tr",
		       [?XCT("td", "Name:"),
			?XE("td", [?INPUT("text", "name", Name)])
		       ]
		      ),
		   ?XE("tr",
		       [?XCT("td", "Description:"),
			?XE("td", [
				   ?TEXTAREA("description", integer_to_list(lists:max([3, DescNL])), "20", Description)
				  ]
			   )
		       ]
		      ),
		   ?XE("tr",
		       [?XCT("td", "Members:"),
			?XE("td", [
				   ?TEXTAREA("members", integer_to_list(lists:max([3, length(FMembers)])), "20", FMembers)
				  ]
			   )
		       ]
		      ),
		   ?XE("tr",
		       [?XCT("td", "IDs of Displayed Groups:"),
			?XE("td", [
				   ?TEXTAREA("dispgroups", integer_to_list(lists:max([3, length(FDisplayedGroups)])), "20", FDisplayedGroups)
				  ]
			   )
		       ]
		      )]
		 )]),
    ?H1GL(?T("Shared Roster Groups"), "modsharedroster", "mod_shared_roster") ++
	[?XC("h2", ?T("Group ") ++ Group)] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
	    nothing -> []
	end ++
	[?XAE("form", [?XMLATTR(<<"action">>, <<"">>), ?XMLATTR(<<"method">>, <<"post">>)],
	      [FGroup,
	       ?BR,
	       ?INPUTT("submit", "submit", "Submit")
	      ])
	].

shared_roster_group_parse_query(Host, Group, Query) ->
    case lists:keysearch("submit", 1, Query) of
	{value, _} ->
	    {value, {_, Name}} = lists:keysearch("name", 1, Query),
	    {value, {_, Description}} = lists:keysearch("description", 1, Query),
	    {value, {_, SMembers}} = lists:keysearch("members", 1, Query),
	    {value, {_, SDispGroups}} = lists:keysearch("dispgroups", 1, Query),
	    NameOpt =
		if
		    Name == "" -> [];
		    true -> [{name, Name}]
		end,
	    DescriptionOpt =
		if
		    Description == "" -> [];
		    true -> [{description, Description}]
		end,
	    DispGroups = string:tokens(SDispGroups, "\r\n"),
	    DispGroupsOpt =
		if
		    DispGroups == [] -> [];
		    true -> [{displayed_groups, DispGroups}]
		end,

	    OldMembers = mod_shared_roster:get_group_explicit_users(
			   Host, Group),
	    SJIDs = string:tokens(SMembers, ", \r\n"),
	    NewMembers =
		lists:foldl(
		  fun(_SJID, error) -> error;
		     (SJID, USs) ->
			  case SJID of
			      "@all@" ->
				  USs;
			      "@online@" ->
				  USs;
			      _ ->
				  try
				      JID = exmpp_jid:parse(SJID),
				      [{exmpp_jid:prep_node_as_list(JID), exmpp_jid:prep_domain_as_list(JID)} | USs]
				  catch
				      _ ->
					  error
				  end
			  end
		  end, [], SJIDs),
	    AllUsersOpt =
		case lists:member("@all@", SJIDs) of
		    true -> [{all_users, true}];
		    false -> []
		end,
	    OnlineUsersOpt =
		case lists:member("@online@", SJIDs) of
		    true -> [{online_users, true}];
		    false -> []
		end,

	    mod_shared_roster:set_group_opts(
	      Host, Group,
	      NameOpt ++ DispGroupsOpt ++ DescriptionOpt ++ AllUsersOpt ++ OnlineUsersOpt),

	    if
		NewMembers == error -> error;
		true ->
		    AddedMembers = NewMembers -- OldMembers,
		    RemovedMembers = OldMembers -- NewMembers,
		    lists:foreach(
		      fun(US) ->
			      mod_shared_roster:remove_user_from_group(
				Host, US, Group)
		      end, RemovedMembers),
		    lists:foreach(
		      fun(US) ->
			      mod_shared_roster:add_user_to_group(
				Host, US, Group)
		      end, AddedMembers),
		    ok
	    end;
	_ ->
	    nothing
    end.

get_opt(Opts, Opt, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	{value, {_, Val}} ->
	    Val;
	false ->
	    Default
    end.

us_to_list({User, Server}) ->
    exmpp_jid:bare_to_list(User, Server).

split_grouphost(Host, Group) ->
    case string:tokens(Group, "@") of
	[GroupName, HostName] ->
	    {HostName, GroupName};
	[_] ->
	    {Host, Group}
    end.
