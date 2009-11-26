%%%----------------------------------------------------------------------
%%% File    : mod_shared_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@process-one.net>
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

start(Host, _Opts) ->
    HostB = list_to_binary(Host),
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
    ejabberd_hooks:add(register_user, HostB,
		       ?MODULE, register_user, 50),
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
    ejabberd_hooks:delete(register_user, HostB,
			  ?MODULE, register_user, 50),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50).
%%ejabberd_hooks:delete(remove_user, HostB,
%%    		  ?MODULE, remove_user, 50),


get_user_roster(Items, US) ->
    {U, S} = US,
    DisplayedGroups = get_user_displayed_groups(US),
    %% Get shared roster users in all groups and remove self: 
    SRUsers = 
	lists:foldl(
	  fun(Group, Acc1) ->
		  GroupName = get_group_name(S, Group),
		  lists:foldl(
		    fun(User, Acc2) ->
			    if User == US -> Acc2;
			       true -> dict:append(User, 
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
    SRItems = [#roster{usj = {U, S, {U1, S1, undefined}},
		       us = US,
		       jid = {U1, S1, undefined},
		       name = "",
		       subscription = both,
		       ask = none,
		       groups = GroupNames} ||
		  {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

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
                    Mod = get_roster_mod(ServerFrom),

                    %% Remove pending out subscription
                    Mod:out_subscription(UserTo, ServerTo,
                                         exmpp_jid:make(UserFrom, ServerFrom),
                                         unsubscribe),

                    %% Remove pending in subscription
                    Mod:in_subscription(aaaa, UserFrom, ServerFrom,
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
    Mod = get_roster_mod(ServerFrom),

    RIFrom = build_roster_record(UserFrom, ServerFrom,
				 UserTo, ServerTo, NameTo, GroupsFrom),
    set_item(UserFrom, ServerFrom, ResourceTo, RIFrom),
    JIDTo = exmpp_jid:make(UserTo, ServerTo),

    JIDFrom = exmpp_jid:make(UserFrom, ServerFrom),
    RITo = build_roster_record(UserTo, ServerTo,
			       UserFrom, ServerFrom, UserFrom,[]),
    set_item(UserTo, ServerTo, undefined, RITo),

    %% From requests
    Mod:out_subscription(UserFrom, ServerFrom, JIDTo, subscribe),
    Mod:in_subscription(aaa, UserTo, ServerTo, JIDFrom, subscribe, ""),

    %% To accepts
    Mod:out_subscription(UserTo, ServerTo, JIDFrom, subscribed),
    Mod:in_subscription(aaa, UserFrom, ServerFrom, JIDTo, subscribed, ""),

    %% To requests
    Mod:out_subscription(UserTo, ServerTo, JIDFrom, subscribe),
    Mod:in_subscription(aaa, UserFrom, ServerFrom, JIDTo, subscribe, ""),

    %% From accepts
    Mod:out_subscription(UserFrom, ServerFrom, JIDTo, subscribed),
    Mod:in_subscription(aaa, UserTo, ServerTo, JIDFrom, subscribed, ""),

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
	SRJIDs = [{U1, S1, undefined} || {U1, S1} <- SRUsers],
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
    Mod = get_roster_mod(ServerFrom),

    %% Remove pending out subscription
    {UserTo, ServerTo, _} = jlib:short_prepd_bare_jid(JIDTo),
    JIDFrom = exmpp_jid:make(UserFrom, UserTo),
    Mod:out_subscription(UserTo, ServerTo, JIDFrom, unsubscribe),

    %% Remove pending in subscription
    Mod:in_subscription(aaaa, UserFrom, ServerFrom, JIDTo, unsubscribe, ""),

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

list_groups(Host) ->
    mnesia:dirty_select(
      sr_group,
      [{#sr_group{group_host = {'$1', '$2'},
		  _ = '_'},
	[{'==', '$2', Host}],
	['$1']}]).

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

is_group_enabled(Host, Group) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    not lists:member(disabled, Opts);
	_ ->
	    false
    end.

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
	    false
    end.

get_group_users(Host, Group) ->
    case get_group_opt(Host, Group, all_users, false) of
	true ->
	    ejabberd_auth:get_vh_registered_users(Host);
	false ->
	    []
    end ++ get_group_explicit_users(Host, Group).

get_group_users(_User, Host, Group, GroupOpts) ->
    case proplists:get_value(all_users, GroupOpts, false) of
	true ->
	    ejabberd_auth:get_vh_registered_users(Host);
	false ->
	    []
    end ++ get_group_explicit_users(Host, Group).

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

get_group_name(Host, Group) ->
    get_group_opt(Host, Group, name, Group).

%% Get list of names of groups that have @all@ in the memberlist
get_special_users_groups(Host) ->
    lists:filter(
      fun(Group) ->
	      get_group_opt(Host, Group, all_users, false)
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

is_user_in_group({_U, S} = US, Group, Host) ->
    case catch mnesia:dirty_match_object(
		 #sr_user{us=US, group_host={Group, Host}}) of
        [] -> lists:member(US, get_group_users(S, Group));
	_  -> true
    end.


%% @spec (Host::string(), {User::string(), Server::string()}, Group::string()) -> {atomic, ok}
add_user_to_group(Host, US, Group) ->
    {LUser, LServer} = US,
    case re:run(LUser, "^@.+@$", [{capture, none}]) of
	match ->
	    GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
	    AllUsersOpt =
		case LUser == "@all@" of
		    true -> [{all_users, true}];
		    false -> []
		end,
            mod_shared_roster:set_group_opts(
	      Host, Group,
	      GroupOpts ++ AllUsersOpt);
	nomatch ->
	    %% Push this new user to members of groups where this group is displayed
	    push_user_to_displayed(LUser, LServer, Group, both),
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

remove_user_from_group(Host, US, Group) ->
    GroupHost = {Group, Host},
    {LUser, LServer} = US,
    case re:run(LUser, "^@.+@$", [{capture, none}]) of
	match ->
	    GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
	    NewGroupOpts =
		case LUser of
		    "@all@" ->
			lists:filter(fun(X) -> X/={all_users,true} end, GroupOpts)
		end,
	    mod_shared_roster:set_group_opts(Host, Group, NewGroupOpts);
	nomatch ->
	    R = #sr_user{us = US, group_host = GroupHost},
	    F = fun() ->
			mnesia:delete_object(R)
		end,
	    Result = mnesia:transaction(F),
	    %% Push removal of the old user to members of groups where the group that this user was members was displayed
	    push_user_to_displayed(LUser, LServer, Group, remove),
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
    [push_user_to_displayed(User, Server, Group, both) || Group <- Groups].

remove_user(User, Server) ->
    push_user_to_members(User, Server, remove).

push_user_to_members(User, Server, Subscription) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	GroupsOpts = groups_with_opts(LServer),
	SpecialGroups = get_special_displayed_groups(GroupsOpts),
	UserGroups = get_user_displayed_groups(LUser, LServer, GroupsOpts),
	lists:foreach(
	  fun(Group) ->
		  GroupOpts = proplists:get_value(Group, GroupsOpts, []),
		  GroupName = proplists:get_value(name, GroupOpts, Group),
		  lists:foreach(
		    fun({U, S}) ->
                            push_roster_item(U, S, LUser, LServer, GroupName, Subscription)
		    end, get_group_users(LUser, LServer, Group, GroupOpts))
	  end, lists:usort(SpecialGroups++UserGroups))
    catch
	_ ->
	    ok
    end.

push_user_to_displayed(LUser, LServer, Group, Subscription) ->
    GroupsOpts = groups_with_opts(LServer),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    GroupName = proplists:get_value(name, GroupOpts, Group),
    DisplayedToGroupsOpts = displayed_to_groups(Group, LServer),
    [push_user_to_group(LUser, LServer, GroupD, GroupName, Subscription) || {GroupD, _Opts} <- DisplayedToGroupsOpts].

push_user_to_group(LUser, LServer, Group, GroupName, Subscription) ->
    lists:foreach(
      fun({U, S}) ->
	      push_roster_item(U, S, LUser, LServer, GroupName, Subscription)
      end, get_group_users(LServer, Group)).

%% Get list of groups to which this group is displayed
displayed_to_groups(GroupName, LServer) ->
    GroupsOpts = groups_with_opts(LServer),
    lists:filter(
      fun({_Group, Opts}) ->
	      lists:member(GroupName, proplists:get_value(displayed_groups, Opts, []))
      end, GroupsOpts).

push_item(_User, _Server, _From, none) ->
    ok;
push_item(User, Server, From, Item) ->
    %% It was
    %%  ejabberd_sm:route(jlib:make_jid("", "", ""),
    %%                    jlib:make_jid(User, Server, "")
    %% why?
    ejabberd_sm:route(From, exmpp_jid:make(User, Server),
		      #xmlel{name = 'broadcast', children =
                       [{item,
                         Item#roster.jid,
			 Item#roster.subscription}]}),
    Request = #xmlel{ns = ?NS_ROSTER, name = 'query',
      children = [item_to_xml(Item)]},
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

item_to_xml(Item) ->
    {U, S, R} = Item#roster.jid,
    Attrs1 = exmpp_xml:set_attribute_in_list([],
      'jid', exmpp_jid:to_list(U, S, R)),
    Attrs2 = case Item#roster.name of
		 "" ->
		     Attrs1;
		 Name ->
		     exmpp_xml:set_attribute_in_list(Attrs1, 'name', Name)
	     end,
    Attrs3 = exmpp_xml:set_attribute_in_list(Attrs2,
      'subscription', Item#roster.subscription),
    Attrs4 = case ask_to_pending(Item#roster.ask) of
		 out ->
		     exmpp_xml:set_attribute_in_list(Attrs3,
		       'ask', "subscribe");
		 both ->
		     exmpp_xml:set_attribute_in_list(Attrs3,
		       'ask', "subscribe");
		 _ ->
		     Attrs3
	     end,
    SubEls1 = lists:map(fun(G) ->
				exmpp_xml:set_cdata(
				  #xmlel{ns = ?NS_ROSTER, name = 'group'}, G)
			end, Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    #xmlel{ns = ?NS_ROSTER, name = 'item', attrs = Attrs4, children = SubEls}.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.


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
	[?XAE("form", [?XMLATTR('action', <<"">>), ?XMLATTR('method', <<"post">>)],
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
    %%Disabled = false,
    DisplayedGroups = get_opt(GroupOpts, displayed_groups, []),
    Members = mod_shared_roster:get_group_explicit_users(Host, Group),
    FMembers =
	if
	    AllUsers ->
		"@all@\n";
	    true ->
		[]
	end ++ [[us_to_list(Member), $\n] || Member <- Members],
    FDisplayedGroups = [[DG, $\n] || DG <- DisplayedGroups],
    DescNL = length(re:split(Description, "\n", [{return, list}])),
    FGroup =
	?XAE("table", [?XMLATTR('class', <<"withtextareas">>)],
	     [?XE("tbody",
		  [?XE("tr",
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
		       [?XCT("td", "Displayed Groups:"),
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
	[?XAE("form", [?XMLATTR('action', <<"">>), ?XMLATTR('method', <<"post">>)],
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

	    mod_shared_roster:set_group_opts(
	      Host, Group,
	      NameOpt ++ DispGroupsOpt ++ DescriptionOpt ++ AllUsersOpt),

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

%% Get the roster module for Server.
get_roster_mod(Server) ->
    case lists:member(mod_roster_odbc,
                      gen_mod:loaded_modules(Server)) of
        true -> mod_roster_odbc;
        false -> mod_roster
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
