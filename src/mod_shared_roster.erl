%%%----------------------------------------------------------------------
%%% File    : mod_shared_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_shared_roster).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, export/1,
	 import_info/0, webadmin_menu/3, webadmin_page/3,
	 get_user_roster/2, c2s_session_opened/1,
	 get_jid_info/4, import/5, process_item/2, import_start/2,
	 in_subscription/6, out_subscription/4, c2s_self_presence/1,
	 unset_presence/4, register_user/2, remove_user/2,
	 list_groups/1, create_group/2, create_group/3,
	 delete_group/2, get_group_opts/2, set_group_opts/3,
	 get_group_users/2, get_group_explicit_users/2,
	 is_user_in_group/3, add_user_to_group/3, opts_to_binary/1,
	 remove_user_from_group/3, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("mod_roster.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-include("mod_shared_roster.hrl").

-define(SETS, gb_sets).

-type group_options() :: [{atom(), any()}].
-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), [binary()]) -> ok.
-callback list_groups(binary()) -> [binary()].
-callback groups_with_opts(binary()) -> [{binary(), group_options()}].
-callback create_group(binary(), binary(), group_options()) -> {atomic, any()}.
-callback delete_group(binary(), binary()) -> {atomic, any()}.
-callback get_group_opts(binary(), binary()) -> group_options() | error.
-callback set_group_opts(binary(), binary(), group_options()) -> {atomic, any()}.
-callback get_user_groups({binary(), binary()}, binary()) -> [binary()].
-callback get_group_explicit_users(binary(), binary()) -> [{binary(), binary()}].
-callback get_user_displayed_groups(binary(), binary(), group_options()) ->
    [{binary(), group_options()}].
-callback is_user_in_group({binary(), binary()}, binary(), binary()) -> boolean().
-callback add_user_to_group(binary(), {binary(), binary()}, binary()) -> any().
-callback remove_user_from_group(binary(), {binary(), binary()}, binary()) -> {atomic, any()}.

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(webadmin_menu_host, Host, ?MODULE,
		       webadmin_menu, 70),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE,
		       webadmin_page, 50),
    ejabberd_hooks:add(roster_get, Host, ?MODULE,
		       get_user_roster, 70),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ?MODULE, in_subscription, 30),
    ejabberd_hooks:add(roster_out_subscription, Host,
		       ?MODULE, out_subscription, 30),
    ejabberd_hooks:add(c2s_session_opened, Host,
		       ?MODULE, c2s_session_opened, 70),
    ejabberd_hooks:add(roster_get_jid_info, Host, ?MODULE,
		       get_jid_info, 70),
    ejabberd_hooks:add(roster_process_item, Host, ?MODULE,
		       process_item, 50),
    ejabberd_hooks:add(c2s_self_presence, Host, ?MODULE,
		       c2s_self_presence, 50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE,
		       unset_presence, 50),
    ejabberd_hooks:add(register_user, Host, ?MODULE,
		       register_user, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50).

stop(Host) ->
    ejabberd_hooks:delete(webadmin_menu_host, Host, ?MODULE,
			  webadmin_menu, 70),
    ejabberd_hooks:delete(webadmin_page_host, Host, ?MODULE,
			  webadmin_page, 50),
    ejabberd_hooks:delete(roster_get, Host, ?MODULE,
			  get_user_roster, 70),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, in_subscription, 30),
    ejabberd_hooks:delete(roster_out_subscription, Host,
			  ?MODULE, out_subscription, 30),
    ejabberd_hooks:delete(c2s_session_opened,
			  Host, ?MODULE, c2s_session_opened, 70),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 70),
    ejabberd_hooks:delete(roster_process_item, Host,
			  ?MODULE, process_item, 50),
    ejabberd_hooks:delete(c2s_self_presence, Host,
			  ?MODULE, c2s_self_presence, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
			  ?MODULE, unset_presence, 50),
    ejabberd_hooks:delete(register_user, Host, ?MODULE,
			  register_user, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user,
			  50).

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    ok.

depends(_Host, _Opts) ->
    [].

-spec get_user_roster([#roster{}], {binary(), binary()}) -> [#roster{}].
get_user_roster(Items, US) ->
    {U, S} = US,
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = lists:foldl(fun (Group, Acc1) ->
				  GroupName = get_group_name(S, Group),
				  lists:foldl(fun (User, Acc2) ->
						      if User == US -> Acc2;
							 true ->
							     dict:append(User,
									 GroupName,
									 Acc2)
						      end
					      end,
					      Acc1, get_group_users(S, Group))
			  end,
			  dict:new(), DisplayedGroups),
    {NewItems1, SRUsersRest} = lists:mapfoldl(fun (Item,
						   SRUsers1) ->
						      {_, _, {U1, S1, _}} =
							  Item#roster.usj,
						      US1 = {U1, S1},
						      case dict:find(US1,
								     SRUsers1)
							  of
							{ok, GroupNames} ->
							    {Item#roster{subscription
									     =
									     both,
									 groups =
									     Item#roster.groups ++ GroupNames,
									 ask =
									     none},
							     dict:erase(US1,
									SRUsers1)};
							error ->
							    {Item, SRUsers1}
						      end
					      end,
					      SRUsers, Items),
    SRItems = [#roster{usj = {U, S, {U1, S1, <<"">>}},
		       us = US, jid = {U1, S1, <<"">>},
		       name = get_rosteritem_name(U1, S1),
		       subscription = both, ask = none, groups = GroupNames}
	       || {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

get_rosteritem_name(U, S) ->
    case gen_mod:is_loaded(S, mod_vcard) of
        true ->
	    SubEls = mod_vcard:get_vcard(U, S),
	    get_rosteritem_name_vcard(SubEls);
        false ->
            <<"">>
    end.

-spec get_rosteritem_name_vcard([xmlel()]) -> binary().
get_rosteritem_name_vcard([Vcard|_]) ->
    case fxml:get_path_s(Vcard,
			[{elem, <<"NICKNAME">>}, cdata])
	of
      <<"">> ->
	  fxml:get_path_s(Vcard, [{elem, <<"FN">>}, cdata]);
      Nickname -> Nickname
    end;
get_rosteritem_name_vcard(_) ->
    <<"">>.

%% This function rewrites the roster entries when moving or renaming
%% them in the user contact list.
-spec process_item(#roster{}, binary()) -> #roster{}.
process_item(RosterItem, Host) ->
    USFrom = {UserFrom, ServerFrom} = RosterItem#roster.us,
    {UserTo, ServerTo, ResourceTo} = RosterItem#roster.jid,
    NameTo = RosterItem#roster.name,
    USTo = {UserTo, ServerTo},
    DisplayedGroups = get_user_displayed_groups(USFrom),
    CommonGroups = lists:filter(fun (Group) ->
					is_user_in_group(USTo, Group, Host)
				end,
				DisplayedGroups),
    case CommonGroups of
      [] -> RosterItem;
      %% Roster item cannot be removed: We simply reset the original groups:
      _ when RosterItem#roster.subscription == remove ->
	  GroupNames = lists:map(fun (Group) ->
					 get_group_name(Host, Group)
				 end,
				 CommonGroups),
	  RosterItem#roster{subscription = both, ask = none,
			    groups = GroupNames};
      %% Both users have at least a common shared group,
      %% So each user can see the other
      _ ->
	  case lists:subtract(RosterItem#roster.groups,
			      CommonGroups)
	      of
	    %% If it doesn't, then remove this user from any
	    %% existing roster groups.
	    [] ->
		mod_roster:out_subscription(UserTo, ServerTo,
					    jid:make(UserFrom, ServerFrom),
					    unsubscribe),
		mod_roster:in_subscription(false, UserFrom, ServerFrom,
					   jid:make(UserTo, ServerTo),
					   unsubscribe, <<"">>),
		RosterItem#roster{subscription = both, ask = none};
	    %% If so, it means the user wants to add that contact
	    %% to his personal roster
	    PersonalGroups ->
		set_new_rosteritems(UserFrom, ServerFrom, UserTo,
				    ServerTo, ResourceTo, NameTo,
				    PersonalGroups)
	  end
    end.

build_roster_record(User1, Server1, User2, Server2,
		    Name2, Groups) ->
    USR2 = {User2, Server2, <<"">>},
    #roster{usj = {User1, Server1, USR2},
	    us = {User1, Server1}, jid = USR2, name = Name2,
	    subscription = both, ask = none, groups = Groups}.

set_new_rosteritems(UserFrom, ServerFrom, UserTo,
		    ServerTo, ResourceTo, NameTo, GroupsFrom) ->
    RIFrom = build_roster_record(UserFrom, ServerFrom,
				 UserTo, ServerTo, NameTo, GroupsFrom),
    set_item(UserFrom, ServerFrom, ResourceTo, RIFrom),
    JIDTo = jid:make(UserTo, ServerTo),
    JIDFrom = jid:make(UserFrom, ServerFrom),
    RITo = build_roster_record(UserTo, ServerTo, UserFrom,
			       ServerFrom, UserFrom, []),
    set_item(UserTo, ServerTo, <<"">>, RITo),
    mod_roster:out_subscription(UserFrom, ServerFrom, JIDTo,
				subscribe),
    mod_roster:in_subscription(false, UserTo, ServerTo,
			       JIDFrom, subscribe, <<"">>),
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom,
				subscribed),
    mod_roster:in_subscription(false, UserFrom, ServerFrom,
			       JIDTo, subscribed, <<"">>),
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom,
				subscribe),
    mod_roster:in_subscription(false, UserFrom, ServerFrom,
			       JIDTo, subscribe, <<"">>),
    mod_roster:out_subscription(UserFrom, ServerFrom, JIDTo,
				subscribed),
    mod_roster:in_subscription(false, UserTo, ServerTo,
			       JIDFrom, subscribed, <<"">>),
    RIFrom.

set_item(User, Server, Resource, Item) ->
    ResIQ = #iq{from = jid:make(User, Server, Resource),
		to = jid:make(Server),
		type = set, id = <<"push", (randoms:get_string())/binary>>,
		sub_els = [#roster_query{
			      items = [mod_roster:encode_item(Item)]}]},
    ejabberd_router:route(ResIQ).

c2s_session_opened(#{jid := #jid{luser = LUser, lserver = LServer},
		     pres_f := PresF, pres_t := PresT} = State) ->
    US = {LUser, LServer},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = lists:flatmap(fun(Group) ->
						get_group_users(LServer, Group)
					end,
			    DisplayedGroups),
    PresBoth = lists:foldl(
		 fun({U, S, _}, Acc) ->
			 ?SETS:add_element({U, S, <<"">>}, Acc);
		    ({U, S}, Acc) ->
			 ?SETS:add_element({U, S, <<"">>}, Acc)
		 end, ?SETS:new(), SRUsers),
    State#{pres_f => ?SETS:union(PresBoth, PresF),
	   pres_t => ?SETS:union(PresBoth, PresT)}.

-spec get_jid_info({subscription(), [binary()]}, binary(), binary(), jid())
      -> {subscription(), [binary()]}.
get_jid_info({Subscription, Groups}, User, Server,
	     JID) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jid:tolower(JID),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = lists:foldl(fun (Group, Acc1) ->
				  lists:foldl(fun (User1, Acc2) ->
						      dict:append(User1,
								  get_group_name(LServer,
										 Group),
								  Acc2)
					      end,
					      Acc1,
					      get_group_users(LServer, Group))
			  end,
			  dict:new(), DisplayedGroups),
    case dict:find(US1, SRUsers) of
      {ok, GroupNames} ->
	  NewGroups = if Groups == [] -> GroupNames;
			 true -> Groups
		      end,
	  {both, NewGroups};
      error -> {Subscription, Groups}
    end.

-spec in_subscription(boolean(), binary(), binary(), jid(),
		      subscribe | subscribed | unsubscribe | unsubscribed,
		      binary()) -> boolean().
in_subscription(Acc, User, Server, JID, Type,
		_Reason) ->
    process_subscription(in, User, Server, JID, Type, Acc).

-spec out_subscription(
	binary(), binary(), jid(),
	subscribed | unsubscribed | subscribe | unsubscribe) -> boolean().
out_subscription(UserFrom, ServerFrom, JIDTo,
		 unsubscribed) ->
    #jid{luser = UserTo, lserver = ServerTo} = JIDTo,
    JIDFrom = jid:make(UserFrom, ServerFrom),
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom,
				unsubscribe),
    mod_roster:in_subscription(false, UserFrom, ServerFrom,
			       JIDTo, unsubscribe, <<"">>),
    process_subscription(out, UserFrom, ServerFrom, JIDTo,
			 unsubscribed, false);
out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type,
			 false).

process_subscription(Direction, User, Server, JID,
		     _Type, Acc) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} =
	jid:tolower(jid:remove_resource(JID)),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = lists:usort(lists:flatmap(fun (Group) ->
						get_group_users(LServer, Group)
					end,
					DisplayedGroups)),
    case lists:member(US1, SRUsers) of
      true ->
	  case Direction of
	    in -> {stop, false};
	    out -> stop
	  end;
      false -> Acc
    end.

list_groups(Host) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:list_groups(Host).

groups_with_opts(Host) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:groups_with_opts(Host).

create_group(Host, Group) ->
    create_group(Host, Group, []).

create_group(Host, Group, Opts) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:create_group(Host, Group, Opts).

delete_group(Host, Group) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:delete_group(Host, Group).

get_group_opts(Host, Group) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:get_group_opts(Host, Group).

set_group_opts(Host, Group, Opts) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:set_group_opts(Host, Group, Opts).

get_user_groups(US) ->
    Host = element(2, US),
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:get_user_groups(US, Host) ++ get_special_users_groups(Host).

is_group_enabled(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    case get_group_opts(Host, Group) of
      error -> false;
      Opts -> not lists:member(disabled, Opts)
    end.

%% @spec (Host::string(), Group::string(), Opt::atom(), Default) -> OptValue | Default
get_group_opt(Host, Group, Opt, Default) ->
    case get_group_opts(Host, Group) of
      error -> Default;
      Opts ->
	  case lists:keysearch(Opt, 1, Opts) of
	    {value, {_, Val}} -> Val;
	    false -> Default
	  end
    end.

get_online_users(Host) ->
    lists:usort([{U, S}
		 || {U, S, _} <- ejabberd_sm:get_vh_session_list(Host)]).

get_group_users(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    case get_group_opt(Host, Group, all_users, false) of
      true -> ejabberd_auth:get_vh_registered_users(Host);
      false -> []
    end
      ++
      case get_group_opt(Host, Group, online_users, false) of
	true -> get_online_users(Host);
	false -> []
      end
	++ get_group_explicit_users(Host, Group).

get_group_users(Host, Group, GroupOpts) ->
    case proplists:get_value(all_users, GroupOpts, false) of
      true -> ejabberd_auth:get_vh_registered_users(Host);
      false -> []
    end
      ++
      case proplists:get_value(online_users, GroupOpts, false)
	  of
	true -> get_online_users(Host);
	false -> []
      end
	++ get_group_explicit_users(Host, Group).

get_group_explicit_users(Host, Group) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:get_group_explicit_users(Host, Group).

get_group_name(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    get_group_opt(Host, Group, name, Group).

%% Get list of names of groups that have @all@/@online@/etc in the memberlist
get_special_users_groups(Host) ->
    lists:filter(fun (Group) ->
			 get_group_opt(Host, Group, all_users, false) orelse
			   get_group_opt(Host, Group, online_users, false)
		 end,
		 list_groups(Host)).

%% Get list of names of groups that have @online@ in the memberlist
get_special_users_groups_online(Host) ->
    lists:filter(fun (Group) ->
			 get_group_opt(Host, Group, online_users, false)
		 end,
		 list_groups(Host)).

%% Given two lists of groupnames and their options,
%% return the list of displayed groups to the second list
displayed_groups(GroupsOpts, SelectedGroupsOpts) ->
    DisplayedGroups = lists:usort(lists:flatmap(fun
						  ({_Group, Opts}) ->
						      [G
						       || G
							      <- proplists:get_value(displayed_groups,
										     Opts,
										     []),
							  not
							    lists:member(disabled,
									 Opts)]
						end,
						SelectedGroupsOpts)),
    [G
     || G <- DisplayedGroups,
	not
	  lists:member(disabled,
		       proplists:get_value(G, GroupsOpts, []))].

%% Given a list of group names with options,
%% for those that have @all@ in memberlist,
%% get the list of groups displayed
get_special_displayed_groups(GroupsOpts) ->
    Groups = lists:filter(fun ({_Group, Opts}) ->
				  proplists:get_value(all_users, Opts, false)
			  end,
			  GroupsOpts),
    displayed_groups(GroupsOpts, Groups).

%% Given a username and server, and a list of group names with options,
%% for the list of groups of that server that user is member
%% get the list of groups displayed
get_user_displayed_groups(LUser, LServer, GroupsOpts) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Groups = Mod:get_user_displayed_groups(LUser, LServer, GroupsOpts),
    displayed_groups(GroupsOpts, Groups).

%% @doc Get the list of groups that are displayed to this user
get_user_displayed_groups(US) ->
    Host = element(2, US),
    DisplayedGroups1 = lists:usort(lists:flatmap(fun
						   (Group) ->
						       case
							 is_group_enabled(Host,
									  Group)
							   of
							 true ->
							     get_group_opt(Host,
									   Group,
									   displayed_groups,
									   []);
							 false -> []
						       end
						 end,
						 get_user_groups(US))),
    [Group
     || Group <- DisplayedGroups1,
	is_group_enabled(Host, Group)].

is_user_in_group(US, Group, Host) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    case Mod:is_user_in_group(US, Group, Host) of
	false ->
	    lists:member(US, get_group_users(Host, Group));
	true ->
	    true
    end.

%% @spec (Host::string(), {User::string(), Server::string()}, Group::string()) -> {atomic, ok}
add_user_to_group(Host, US, Group) ->
    {LUser, LServer} = US,
    case ejabberd_regexp:run(LUser, <<"^@.+@\$">>) of
      match ->
	  GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
	  MoreGroupOpts = case LUser of
			    <<"@all@">> -> [{all_users, true}];
			    <<"@online@">> -> [{online_users, true}];
			    _ -> []
			  end,
	  mod_shared_roster:set_group_opts(Host, Group,
				   GroupOpts ++ MoreGroupOpts);
      nomatch ->
	  DisplayedToGroups = displayed_to_groups(Group, Host),
	  DisplayedGroups = get_displayed_groups(Group, LServer),
	  push_user_to_displayed(LUser, LServer, Group, Host, both, DisplayedToGroups),
	  push_displayed_to_user(LUser, LServer, Host, both, DisplayedGroups),
	  broadcast_user_to_displayed(LUser, LServer, Host, both, DisplayedToGroups),
	  broadcast_displayed_to_user(LUser, LServer, Host, both, DisplayedGroups),
	  Mod = gen_mod:db_mod(Host, ?MODULE),
	  Mod:add_user_to_group(Host, US, Group)
    end.

get_displayed_groups(Group, LServer) ->
    GroupsOpts = groups_with_opts(LServer),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    proplists:get_value(displayed_groups, GroupOpts, []).

broadcast_displayed_to_user(LUser, LServer, Host, Subscription, DisplayedGroups) ->
    [broadcast_members_to_user(LUser, LServer, DGroup, Host,
			  Subscription)
	 || DGroup <- DisplayedGroups].

push_displayed_to_user(LUser, LServer, Host, Subscription, DisplayedGroups) ->
    [push_members_to_user(LUser, LServer, DGroup, Host,
			  Subscription)
     || DGroup <- DisplayedGroups].

remove_user_from_group(Host, US, Group) ->
    {LUser, LServer} = US,
    case ejabberd_regexp:run(LUser, <<"^@.+@\$">>) of
      match ->
	  GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
	  NewGroupOpts = case LUser of
			   <<"@all@">> ->
			       lists:filter(fun (X) -> X /= {all_users, true}
					    end,
					    GroupOpts);
			   <<"@online@">> ->
			       lists:filter(fun (X) -> X /= {online_users, true}
					    end,
					    GroupOpts)
			 end,
	  mod_shared_roster:set_group_opts(Host, Group, NewGroupOpts);
      nomatch ->
	  Mod = gen_mod:db_mod(Host, ?MODULE),
	  Result = Mod:remove_user_from_group(Host, US, Group),
	  DisplayedToGroups = displayed_to_groups(Group, Host),
	  DisplayedGroups = get_displayed_groups(Group, LServer),
	  push_user_to_displayed(LUser, LServer, Group, Host, remove, DisplayedToGroups),
	  push_displayed_to_user(LUser, LServer, Host, remove, DisplayedGroups),
	  Result
    end.

push_members_to_user(LUser, LServer, Group, Host,
		     Subscription) ->
    GroupsOpts = groups_with_opts(LServer),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    GroupName = proplists:get_value(name, GroupOpts, Group),
    Members = get_group_users(Host, Group),
    lists:foreach(fun ({U, S}) ->
			  push_roster_item(LUser, LServer, U, S, GroupName,
					   Subscription)
		  end,
		  Members).

broadcast_members_to_user(LUser, LServer, Group, Host, Subscription) ->
    Members = get_group_users(Host, Group),
    lists:foreach(
      fun({U, S}) ->
	      broadcast_subscription(U, S, {LUser, LServer, <<"">>}, Subscription)
      end, Members).

-spec register_user(binary(), binary()) -> ok.
register_user(User, Server) ->
    Groups = get_user_groups({User, Server}),
    [push_user_to_displayed(User, Server, Group, Server,
			    both, displayed_to_groups(Group, Server))
     || Group <- Groups],
    ok.

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    push_user_to_members(User, Server, remove).

push_user_to_members(User, Server, Subscription) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    GroupsOpts = groups_with_opts(LServer),
    SpecialGroups =
	get_special_displayed_groups(GroupsOpts),
    UserGroups = get_user_displayed_groups(LUser, LServer,
					   GroupsOpts),
    lists:foreach(fun (Group) ->
			  remove_user_from_group(LServer, {LUser, LServer},
						 Group),
			  GroupOpts = proplists:get_value(Group, GroupsOpts,
							  []),
			  GroupName = proplists:get_value(name, GroupOpts,
							  Group),
			  lists:foreach(fun ({U, S}) ->
						push_roster_item(U, S, LUser,
								 LServer,
								 GroupName,
								 Subscription)
					end,
					get_group_users(LServer, Group,
							GroupOpts))
		  end,
		  lists:usort(SpecialGroups ++ UserGroups)).

push_user_to_displayed(LUser, LServer, Group, Host, Subscription, DisplayedToGroupsOpts) ->
    GroupsOpts = groups_with_opts(Host),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    GroupName = proplists:get_value(name, GroupOpts, Group),
    [push_user_to_group(LUser, LServer, GroupD, Host,
			GroupName, Subscription)
     || GroupD <- DisplayedToGroupsOpts].

broadcast_user_to_displayed(LUser, LServer, Host, Subscription, DisplayedToGroupsOpts) ->
    [broadcast_user_to_group(LUser, LServer, GroupD, Host, Subscription)
	|| GroupD <- DisplayedToGroupsOpts].

push_user_to_group(LUser, LServer, Group, Host,
		   GroupName, Subscription) ->
    lists:foreach(fun ({U, S})
			  when (U == LUser) and (S == LServer) ->
			  ok;
		      ({U, S}) ->
			  push_roster_item(U, S, LUser, LServer, GroupName,
					   Subscription)
		  end,
		  get_group_users(Host, Group)).

broadcast_user_to_group(LUser, LServer, Group, Host, Subscription) ->
    lists:foreach(
      fun({U, S})  when (U == LUser) and (S == LServer) -> ok;
         ({U, S}) ->
	      broadcast_subscription(LUser, LServer, {U, S, <<"">>}, Subscription)
      end, get_group_users(Host, Group)).

%% Get list of groups to which this group is displayed
displayed_to_groups(GroupName, LServer) ->
    GroupsOpts = groups_with_opts(LServer),
    Gs = lists:filter(fun ({_Group, Opts}) ->
			 lists:member(GroupName,
				      proplists:get_value(displayed_groups,
							  Opts, []))
		 end,
		 GroupsOpts),
    [Name || {Name, _} <- Gs].

push_item(User, Server, Item) ->
    Stanza = #iq{type = set, id = <<"push", (randoms:get_string())/binary>>,
		 sub_els = [#roster_query{
			       items = [mod_roster:encode_item(Item)]}]},
    lists:foreach(fun (Resource) ->
			  JID = jid:make(User, Server, Resource),
			  ejabberd_router:route(
			    xmpp:set_from_to(Stanza, jid:remove_resource(JID), JID))
		  end,
		  ejabberd_sm:get_user_resources(User, Server)).

push_roster_item(User, Server, ContactU, ContactS,
		 GroupName, Subscription) ->
    Item = #roster{usj =
		       {User, Server, {ContactU, ContactS, <<"">>}},
		   us = {User, Server}, jid = {ContactU, ContactS, <<"">>},
		   name = <<"">>, subscription = Subscription, ask = none,
		   groups = [GroupName]},
    push_item(User, Server, Item).

-spec c2s_self_presence({presence(), ejabberd_c2s:state()})
      -> {presence(), ejabberd_c2s:state()}.
c2s_self_presence({_, #{pres_last := _}} = Acc) ->
    %% This is just a presence update, nothing to do
    Acc;
c2s_self_presence({#presence{type = available}, #{jid := New}} = Acc) ->
    LUser = New#jid.luser,
    LServer = New#jid.lserver,
    Resources = ejabberd_sm:get_user_resources(LUser, LServer),
    ?DEBUG("user_available for ~p @ ~p (~p resources)",
	   [LUser, LServer, length(Resources)]),
    case length(Resources) of
      %% first session for this user
      1 ->
	  UserGroups = get_user_groups({LUser, LServer}),
	  lists:foreach(fun (OG) ->
				?DEBUG("user_available: pushing  ~p @ ~p grp ~p",
				       [LUser, LServer, OG]),
			  DisplayedToGroups = displayed_to_groups(OG, LServer),
			  DisplayedGroups = get_displayed_groups(OG, LServer),
			  broadcast_displayed_to_user(LUser, LServer, LServer, both, DisplayedGroups),
			  broadcast_user_to_displayed(LUser, LServer, LServer, both, DisplayedToGroups)
			end,
			UserGroups);
      _ -> ok
    end,
    Acc;
c2s_self_presence(Acc) ->
    Acc.

-spec unset_presence(binary(), binary(), binary(), binary()) -> ok.
unset_presence(LUser, LServer, Resource, Status) ->
    Resources = ejabberd_sm:get_user_resources(LUser,
					       LServer),
    ?DEBUG("unset_presence for ~p @ ~p / ~p -> ~p "
	   "(~p resources)",
	   [LUser, LServer, Resource, Status, length(Resources)]),
    case length(Resources) of
      0 ->
	  OnlineGroups = get_special_users_groups_online(LServer),
	  lists:foreach(fun (OG) ->
				push_user_to_displayed(LUser, LServer, OG,
						       LServer, remove, displayed_to_groups(OG, LServer)),
				push_displayed_to_user(LUser, LServer,
						       LServer, remove, displayed_to_groups(OG, LServer))
			end,
			OnlineGroups);
      _ -> ok
    end.

%%---------------------
%% Web Admin
%%---------------------

webadmin_menu(Acc, _Host, Lang) ->
    [{<<"shared-roster">>, ?T(<<"Shared Roster Groups">>)}
     | Acc].

webadmin_page(_, Host,
	      #request{us = _US, path = [<<"shared-roster">>],
		       q = Query, lang = Lang} =
		  _Request) ->
    Res = list_shared_roster_groups(Host, Query, Lang),
    {stop, Res};
webadmin_page(_, Host,
	      #request{us = _US, path = [<<"shared-roster">>, Group],
		       q = Query, lang = Lang} =
		  _Request) ->
    Res = shared_roster_group(Host, Group, Query, Lang),
    {stop, Res};
webadmin_page(Acc, _, _) -> Acc.

list_shared_roster_groups(Host, Query, Lang) ->
    Res = list_sr_groups_parse_query(Host, Query),
    SRGroups = mod_shared_roster:list_groups(Host),
    FGroups = (?XAE(<<"table">>, [],
		    [?XE(<<"tbody">>,
			 (lists:map(fun (Group) ->
					    ?XE(<<"tr">>,
						[?XE(<<"td">>,
						     [?INPUT(<<"checkbox">>,
							     <<"selected">>,
							     Group)]),
						 ?XE(<<"td">>,
						     [?AC(<<Group/binary, "/">>,
							  Group)])])
				    end,
				    lists:sort(SRGroups))
			    ++
			    [?XE(<<"tr">>,
				 [?X(<<"td">>),
				  ?XE(<<"td">>,
				      [?INPUT(<<"text">>, <<"namenew">>,
					      <<"">>)]),
				  ?XE(<<"td">>,
				      [?INPUTT(<<"submit">>, <<"addnew">>,
					       <<"Add New">>)])])]))])),
    (?H1GL((?T(<<"Shared Roster Groups">>)),
	   <<"mod_shared_roster">>, <<"mod_shared_roster">>))
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	error -> [?XREST(<<"Bad format">>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [FGroups, ?BR,
	       ?INPUTT(<<"submit">>, <<"delete">>,
		       <<"Delete Selected">>)])].

list_sr_groups_parse_query(Host, Query) ->
    case lists:keysearch(<<"addnew">>, 1, Query) of
      {value, _} -> list_sr_groups_parse_addnew(Host, Query);
      _ ->
	  case lists:keysearch(<<"delete">>, 1, Query) of
	    {value, _} -> list_sr_groups_parse_delete(Host, Query);
	    _ -> nothing
	  end
    end.

list_sr_groups_parse_addnew(Host, Query) ->
    case lists:keysearch(<<"namenew">>, 1, Query) of
      {value, {_, Group}} when Group /= <<"">> ->
	  mod_shared_roster:create_group(Host, Group), ok;
      _ -> error
    end.

list_sr_groups_parse_delete(Host, Query) ->
    SRGroups = mod_shared_roster:list_groups(Host),
    lists:foreach(fun (Group) ->
			  case lists:member({<<"selected">>, Group}, Query) of
			    true -> mod_shared_roster:delete_group(Host, Group);
			    _ -> ok
			  end
		  end,
		  SRGroups),
    ok.

shared_roster_group(Host, Group, Query, Lang) ->
    Res = shared_roster_group_parse_query(Host, Group,
					  Query),
    GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
    Name = get_opt(GroupOpts, name, <<"">>),
    Description = get_opt(GroupOpts, description, <<"">>),
    AllUsers = get_opt(GroupOpts, all_users, false),
    OnlineUsers = get_opt(GroupOpts, online_users, false),
    DisplayedGroups = get_opt(GroupOpts, displayed_groups,
			      []),
    Members = mod_shared_roster:get_group_explicit_users(Host,
						 Group),
    FMembers = iolist_to_binary(
                 [if AllUsers -> <<"@all@\n">>;
                     true -> <<"">>
                  end,
                  if OnlineUsers -> <<"@online@\n">>;
                     true -> <<"">>
                  end,
                  [[us_to_list(Member), $\n] || Member <- Members]]),
    FDisplayedGroups = [<<DG/binary, $\n>> || DG <- DisplayedGroups],
    DescNL = length(ejabberd_regexp:split(Description,
					   <<"\n">>)),
    FGroup = (?XAE(<<"table">>,
		   [{<<"class">>, <<"withtextareas">>}],
		   [?XE(<<"tbody">>,
			[?XE(<<"tr">>,
			     [?XCT(<<"td">>, <<"Name:">>),
			      ?XE(<<"td">>,
				  [?INPUT(<<"text">>, <<"name">>, Name)])]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, <<"Description:">>),
			      ?XE(<<"td">>,
				  [?TEXTAREA(<<"description">>,
					     integer_to_binary(lists:max([3,
                                                                               DescNL])),
					     <<"20">>, Description)])]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, <<"Members:">>),
			      ?XE(<<"td">>,
				  [?TEXTAREA(<<"members">>,
					     integer_to_binary(lists:max([3,
                                                                               length(Members)+3])),
					     <<"20">>, FMembers)])]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, <<"Displayed Groups:">>),
			      ?XE(<<"td">>,
				  [?TEXTAREA(<<"dispgroups">>,
					     integer_to_binary(lists:max([3,											        length(FDisplayedGroups)])),
					     <<"20">>,
					     list_to_binary(FDisplayedGroups))])])])])),
    (?H1GL((?T(<<"Shared Roster Groups">>)),
	   <<"mod_shared_roster">>, <<"mod_shared_roster">>))
      ++
      [?XC(<<"h2">>, <<(?T(<<"Group ">>))/binary, Group/binary>>)] ++
	case Res of
	  ok -> [?XREST(<<"Submitted">>)];
	  error -> [?XREST(<<"Bad format">>)];
	  nothing -> []
	end
	  ++
	  [?XAE(<<"form">>,
		[{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
		[FGroup, ?BR,
		 ?INPUTT(<<"submit">>, <<"submit">>, <<"Submit">>)])].

shared_roster_group_parse_query(Host, Group, Query) ->
    case lists:keysearch(<<"submit">>, 1, Query) of
      {value, _} ->
	  {value, {_, Name}} = lists:keysearch(<<"name">>, 1,
					       Query),
	  {value, {_, Description}} =
	      lists:keysearch(<<"description">>, 1, Query),
	  {value, {_, SMembers}} = lists:keysearch(<<"members">>,
						   1, Query),
	  {value, {_, SDispGroups}} =
	      lists:keysearch(<<"dispgroups">>, 1, Query),
	  NameOpt = if Name == <<"">> -> [];
		       true -> [{name, Name}]
		    end,
	  DescriptionOpt = if Description == <<"">> -> [];
			      true -> [{description, Description}]
			   end,
	  DispGroups = str:tokens(SDispGroups, <<"\r\n">>),
	  DispGroupsOpt = if DispGroups == [] -> [];
			     true -> [{displayed_groups, DispGroups}]
			  end,
	  OldMembers = mod_shared_roster:get_group_explicit_users(Host,
							  Group),
	  SJIDs = str:tokens(SMembers, <<", \r\n">>),
	  NewMembers = lists:foldl(fun (_SJID, error) -> error;
				       (SJID, USs) ->
					   case SJID of
					     <<"@all@">> -> USs;
					     <<"@online@">> -> USs;
					     _ ->
						 try jid:decode(SJID) of
						     JID ->
						       [{JID#jid.luser,
							 JID#jid.lserver}
							| USs]
						 catch _:{bad_jid, _} ->
							 error
						 end
					   end
				   end,
				   [], SJIDs),
	  AllUsersOpt = case lists:member(<<"@all@">>, SJIDs) of
			  true -> [{all_users, true}];
			  false -> []
			end,
	  OnlineUsersOpt = case lists:member(<<"@online@">>,
					     SJIDs)
			       of
			     true -> [{online_users, true}];
			     false -> []
			   end,
	  CurrentDisplayedGroups = get_displayed_groups(Group, Host),
	  AddedDisplayedGroups =  DispGroups -- CurrentDisplayedGroups,
	  RemovedDisplayedGroups = CurrentDisplayedGroups -- DispGroups,
	  displayed_groups_update(OldMembers, RemovedDisplayedGroups, remove),
	  displayed_groups_update(OldMembers, AddedDisplayedGroups, both),
	  mod_shared_roster:set_group_opts(Host, Group,
				   NameOpt ++
				     DispGroupsOpt ++
				       DescriptionOpt ++
					 AllUsersOpt ++ OnlineUsersOpt),
	  if NewMembers == error -> error;
	     true ->
		 AddedMembers = NewMembers -- OldMembers,
		 RemovedMembers = OldMembers -- NewMembers,
		 lists:foreach(fun (US) ->
				       mod_shared_roster:remove_user_from_group(Host,
									US,
									Group)
			       end,
			       RemovedMembers),
		 lists:foreach(fun (US) ->
				       mod_shared_roster:add_user_to_group(Host, US,
								   Group)
			       end,
			       AddedMembers),
		 ok
	  end;
      _ -> nothing
    end.

get_opt(Opts, Opt, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
      {value, {_, Val}} -> Val;
      false -> Default
    end.

us_to_list({User, Server}) ->
    jid:encode({User, Server, <<"">>}).

split_grouphost(Host, Group) ->
    case str:tokens(Group, <<"@">>) of
      [GroupName, HostName] -> {HostName, GroupName};
      [_] -> {Host, Group}
    end.

broadcast_subscription(User, Server, ContactJid, Subscription) ->
    ejabberd_sm:route(jid:make(User, Server),
                      {item, ContactJid, Subscription}).

displayed_groups_update(Members, DisplayedGroups, Subscription) ->
    lists:foreach(fun({U, S}) ->
	push_displayed_to_user(U, S, S, Subscription, DisplayedGroups),
	    case Subscription of
		both ->
		    broadcast_displayed_to_user(U, S, S, to, DisplayedGroups),
		    broadcast_displayed_to_user(U, S, S, from, DisplayedGroups);
		Subscr ->
		    broadcast_displayed_to_user(U, S, S, Subscr, DisplayedGroups)
	    end
	end, Members).

opts_to_binary(Opts) ->
    lists:map(
      fun({name, Name}) ->
              {name, iolist_to_binary(Name)};
         ({description, Desc}) ->
              {description, iolist_to_binary(Desc)};
         ({displayed_groups, Gs}) ->
              {displayed_groups, [iolist_to_binary(G) || G <- Gs]};
         (Opt) ->
              Opt
      end, Opts).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import_info() ->
    [{<<"sr_group">>, 3}, {<<"sr_user">>, 3}].

import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []).

import(LServer, {sql, _}, DBType, Tab, L) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Tab, L).

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(_) -> [db_type].
