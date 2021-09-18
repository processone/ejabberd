%%%----------------------------------------------------------------------
%%% File    : mod_shared_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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
	 get_user_roster/2,
	 get_jid_info/4, import/5, process_item/2, import_start/2,
	 in_subscription/2, out_subscription/1, c2s_self_presence/1,
	 unset_presence/4, register_user/2, remove_user/2,
	 list_groups/1, create_group/2, create_group/3,
	 delete_group/2, get_group_opts/2, set_group_opts/3,
	 get_group_users/2, get_group_explicit_users/2,
	 is_user_in_group/3, add_user_to_group/3, opts_to_binary/1,
	 remove_user_from_group/3, mod_opt_type/1, mod_options/1, mod_doc/0, depends/2]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("mod_roster.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-include("mod_shared_roster.hrl").

-include("translate.hrl").

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
-callback use_cache(binary()) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/1, cache_nodes/1]).

-define(GROUP_OPTS_CACHE, shared_roster_group_opts_cache).
-define(USER_GROUPS_CACHE, shared_roster_user_groups_cache).
-define(GROUP_EXPLICIT_USERS_CACHE, shared_roster_group_explicit_cache).
-define(SPECIAL_GROUPS_CACHE, shared_roster_special_groups_cache).

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    Mod:init(Host, Opts),
    init_cache(Mod, Host, Opts),
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
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    if
	NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    init_cache(NewMod, Host, NewOpts),
    ok.

depends(_Host, _Opts) ->
    [].

-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    ets_cache:new(?SPECIAL_GROUPS_CACHE, [{max_size, 4}]),
    case use_cache(Mod, Host) of
        true ->
	    CacheOpts = cache_opts(Opts),
	    ets_cache:new(?GROUP_OPTS_CACHE, CacheOpts),
	    ets_cache:new(?USER_GROUPS_CACHE, CacheOpts),
	    ets_cache:new(?GROUP_EXPLICIT_USERS_CACHE, CacheOpts);
        false ->
	    ets_cache:delete(?GROUP_OPTS_CACHE),
	    ets_cache:delete(?USER_GROUPS_CACHE),
	    ets_cache:delete(?GROUP_EXPLICIT_USERS_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = mod_shared_roster_opt:cache_size(Opts),
    CacheMissed = mod_shared_roster_opt:cache_missed(Opts),
    LifeTime = mod_shared_roster_opt:cache_life_time(Opts),
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
        true -> Mod:use_cache(Host);
        false -> mod_shared_roster_opt:use_cache(Host)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
        true -> Mod:cache_nodes(Host);
        false -> ejabberd_cluster:get_nodes()
    end.

-spec get_user_roster([#roster{}], {binary(), binary()}) -> [#roster{}].
get_user_roster(Items, {U, S} = US) ->
    {DisplayedGroups, Cache} = get_user_displayed_groups(US),
    SRUsers = lists:foldl(
	fun(Group, Acc1) ->
	    GroupLabel = get_group_label_cached(S, Group, Cache),
	    lists:foldl(
		fun(User, Acc2) ->
		    if User == US -> Acc2;
			true ->
			    dict:append(User, GroupLabel, Acc2)
		    end
		end,
		Acc1, get_group_users_cached(S, Group, Cache))
	end,
	dict:new(), DisplayedGroups),
    {NewItems1, SRUsersRest} = lists:mapfoldl(
	fun(Item, SRUsers1) ->
	    {_, _, {U1, S1, _}} = Item#roster.usj,
	    US1 = {U1, S1},
	    case dict:find(US1, SRUsers1) of
		{ok, GroupLabels} ->
		    {Item#roster{subscription = both,
				 groups = Item#roster.groups ++ GroupLabels,
				 ask = none},
		     dict:erase(US1, SRUsers1)};
		error ->
		    {Item, SRUsers1}
	    end
	end,
	SRUsers, Items),
    SRItems = [#roster{usj = {U, S, {U1, S1, <<"">>}},
		       us = US, jid = {U1, S1, <<"">>},
		       name = get_rosteritem_name(U1, S1),
		       subscription = both, ask = none, groups = GroupLabels}
	       || {{U1, S1}, GroupLabels} <- dict:to_list(SRUsersRest)],
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
    {DisplayedGroups, Cache} = get_user_displayed_groups(USFrom),
    CommonGroups = lists:filter(fun (Group) ->
					is_user_in_group(USTo, Group, Host)
				end,
				DisplayedGroups),
    case CommonGroups of
      [] -> RosterItem;
      %% Roster item cannot be removed: We simply reset the original groups:
      _ when RosterItem#roster.subscription == remove ->
	  GroupLabels = lists:map(fun (Group) ->
					 get_group_label_cached(Host, Group, Cache)
				 end,
				 CommonGroups),
	  RosterItem#roster{subscription = both, ask = none,
			    groups = GroupLabels};
      %% Both users have at least a common shared group,
      %% So each user can see the other
      _ ->
	  case lists:subtract(RosterItem#roster.groups,
			      CommonGroups)
	      of
	    %% If it doesn't, then remove this user from any
	    %% existing roster groups.
	    [] ->
		Pres = #presence{from = jid:make(UserTo, ServerTo),
				 to = jid:make(UserFrom, ServerFrom),
				 type = unsubscribe},
		mod_roster:out_subscription(Pres),
		mod_roster:in_subscription(false, Pres),
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
    mod_roster:out_subscription(
      #presence{from = JIDFrom, to = JIDTo, type = subscribe}),
    mod_roster:in_subscription(
      false, #presence{to = JIDTo, from = JIDFrom, type = subscribe}),
    mod_roster:out_subscription(
      #presence{from = JIDTo, to = JIDFrom, type = subscribed}),
    mod_roster:in_subscription(
      false, #presence{to = JIDFrom, from = JIDTo, type = subscribed}),
    mod_roster:out_subscription(
      #presence{from = JIDTo, to = JIDFrom, type = subscribe}),
    mod_roster:in_subscription(
      false, #presence{to = JIDFrom, from = JIDTo, type = subscribe}),
    mod_roster:out_subscription(
      #presence{from = JIDFrom, to = JIDTo, type = subscribed}),
    mod_roster:in_subscription(
      false, #presence{to = JIDTo, from = JIDFrom, type = subscribed}),
    RIFrom.

set_item(User, Server, Resource, Item) ->
    ResIQ = #iq{from = jid:make(User, Server, Resource),
		to = jid:make(Server),
		type = set, id = <<"push", (p1_rand:get_string())/binary>>,
		sub_els = [#roster_query{
			      items = [mod_roster:encode_item(Item)]}]},
    ejabberd_router:route(ResIQ).

-spec get_jid_info({subscription(), ask(), [binary()]}, binary(), binary(), jid())
      -> {subscription(), ask(), [binary()]}.
get_jid_info({Subscription, Ask, Groups}, User, Server,
	     JID) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jid:tolower(JID),
    US1 = {U1, S1},
    {DisplayedGroups, Cache} = get_user_displayed_groups(US),
    SRUsers = lists:foldl(
	fun(Group, Acc1) ->
	    GroupLabel = get_group_label_cached(LServer, Group, Cache), %++
	    lists:foldl(
		fun(User1, Acc2) ->
		    dict:append(User1, GroupLabel, Acc2)
		end, Acc1, get_group_users_cached(LServer, Group, Cache))
	end,
	dict:new(), DisplayedGroups),
    case dict:find(US1, SRUsers) of
      {ok, GroupLabels} ->
	  NewGroups = if Groups == [] -> GroupLabels;
			 true -> Groups
		      end,
	  {both, none, NewGroups};
      error -> {Subscription, Ask, Groups}
    end.

-spec in_subscription(boolean(), presence()) -> boolean().
in_subscription(Acc, #presence{to = To, from = JID, type = Type}) ->
    #jid{user = User, server = Server} = To,
    process_subscription(in, User, Server, JID, Type, Acc).

-spec out_subscription(presence()) -> boolean().
out_subscription(#presence{from = From, to = To, type = unsubscribed} = Pres) ->
    #jid{user = User, server = Server} = From,
    mod_roster:out_subscription(Pres#presence{type = unsubscribe}),
    mod_roster:in_subscription(false, xmpp:set_from_to(
					Pres#presence{type = unsubscribe},
					To, From)),
    process_subscription(out, User, Server, To, unsubscribed, false);
out_subscription(#presence{from = From, to = To, type = Type}) ->
    #jid{user = User, server = Server} = From,
    process_subscription(out, User, Server, To, Type, false).

process_subscription(Direction, User, Server, JID,
		     _Type, Acc) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} =
	jid:tolower(jid:remove_resource(JID)),
    US1 = {U1, S1},
    {DisplayedGroups, _} = get_user_displayed_groups(US),
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
    case proplists:get_value(all_users, Opts, false) orelse
	 proplists:get_value(online_users, Opts, false) of
	true ->
	    update_wildcard_cache(Host, Group, Opts);
	_ ->
	    ok
    end,
    case use_cache(Mod, Host) of
	true ->
	    ets_cache:insert(?GROUP_OPTS_CACHE, {Host, Group}, Opts, cache_nodes(Mod, Host));
	_ ->
	    ok
    end,
    Mod:create_group(Host, Group, Opts).

delete_group(Host, Group) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    update_wildcard_cache(Host, Group, []),
    case use_cache(Mod, Host) of
	true ->
	    ets_cache:delete(?GROUP_OPTS_CACHE, {Host, Group}, cache_nodes(Mod, Host)),
	    ets_cache:clear(?USER_GROUPS_CACHE, cache_nodes(Mod, Host)),
	    ets_cache:delete(?GROUP_EXPLICIT_USERS_CACHE, {Host, Group}, cache_nodes(Mod, Host));
	_ ->
	    ok
    end,
    Mod:delete_group(Host, Group).

get_groups_opts_cached(Host1, Group1, Cache) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    Key = {Group, Host},
    case Cache of
	#{Key := Opts} ->
	    {Opts, Cache};
	_ ->
	    Opts = get_group_opts_int(Host, Group),
	    {Opts, Cache#{Key => Opts}}
    end.

get_group_opts(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    get_group_opts_int(Host, Group).

get_group_opts_int(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Res = case use_cache(Mod, Host) of
	true ->
	    ets_cache:lookup(
		?GROUP_OPTS_CACHE, {Host, Group},
		fun() ->
		    case Mod:get_group_opts(Host, Group) of
			error -> error;
			V -> {cache, V}
		    end
		end);
	false ->
	    Mod:get_group_opts(Host, Group)
    end,
    case Res of
        {ok, Opts} -> Opts;
        error -> error
    end.

set_group_opts(Host, Group, Opts) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    update_wildcard_cache(Host, Group, Opts),
    case use_cache(Mod, Host) of
	true ->
	    ets_cache:delete(?GROUP_OPTS_CACHE, {Host, Group}, cache_nodes(Mod, Host)),
	    ets_cache:insert(?GROUP_OPTS_CACHE, {Host, Group}, Opts, cache_nodes(Mod, Host));
	_ ->
	    ok
    end,
    Mod:set_group_opts(Host, Group, Opts).

get_user_groups(US) ->
    Host = element(2, US),
    Mod = gen_mod:db_mod(Host, ?MODULE),
    UG = case use_cache(Mod, Host) of
	     true ->
		 ets_cache:lookup(
		     ?USER_GROUPS_CACHE, {Host, US},
		     fun() ->
			 {cache, Mod:get_user_groups(US, Host)}
		     end);
	     false ->
		 Mod:get_user_groups(US, Host)
	 end,
    UG ++ get_groups_with_wildcards(Host, both).

get_group_opt_cached(Host, Group, Opt, Default, Cache) ->
    case get_groups_opts_cached(Host, Group, Cache) of
	{error, _} -> Default;
	{Opts, _} ->
	    proplists:get_value(Opt, Opts, Default)
    end.

%% @spec (Host::string(), Group::string(), Opt::atom(), Default) -> OptValue | Default
get_group_opt(Host, Group, Opt, Default) ->
    case get_group_opts(Host, Group) of
      error -> Default;
      Opts ->
	  proplists:get_value(Opt, Opts, Default)
    end.

get_online_users(Host) ->
    lists:usort([{U, S}
		 || {U, S, _} <- ejabberd_sm:get_vh_session_list(Host)]).

get_group_users_cached(Host, Group, Cache) ->
    {Opts, _} = get_groups_opts_cached(Host, Group, Cache),
    get_group_users(Host, Group, Opts).

get_group_users(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    get_group_users(Host, Group, get_group_opts(Host, Group)).

get_group_users(Host, Group, GroupOpts) ->
    case proplists:get_value(all_users, GroupOpts, false) of
      true -> ejabberd_auth:get_users(Host);
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
    case use_cache(Mod, Host) of
	true ->
	    ets_cache:lookup(
		?GROUP_EXPLICIT_USERS_CACHE, {Host, Group},
		fun() ->
		    {cache, Mod:get_group_explicit_users(Host, Group)}
		end);
	false ->
	    Mod:get_group_explicit_users(Host, Group)
    end.

get_group_label_cached(Host, Group, Cache) ->
    get_group_opt_cached(Host, Group, label, Group, Cache).

-spec update_wildcard_cache(binary(), binary(), list()) -> ok.
update_wildcard_cache(Host, Group, NewOpts) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Online = get_groups_with_wildcards(Host, online),
    Both = get_groups_with_wildcards(Host, both),
    IsOnline = proplists:get_value(online_users, NewOpts, false),
    IsAll = proplists:get_value(all_users, NewOpts, false),

    OnlineUpdated = lists:member(Group, Online) /= IsOnline,
    BothUpdated = lists:member(Group, Both) /= (IsOnline orelse IsAll),

    if
	OnlineUpdated ->
	    NewOnline = case IsOnline of
			    true -> [Group | Online];
			    _ -> Online -- [Group]
			end,
	    ets_cache:update(?SPECIAL_GROUPS_CACHE, {Host, online},
			     {ok, NewOnline}, fun() -> ok end, cache_nodes(Mod, Host));
	true -> ok
    end,
    if
	BothUpdated ->
	    NewBoth = case IsOnline orelse IsAll of
			    true -> [Group | Both];
			    _ -> Both -- [Group]
			end,
	    ets_cache:update(?SPECIAL_GROUPS_CACHE, {Host, both},
			     {ok, NewBoth}, fun() -> ok end, cache_nodes(Mod, Host));
	true -> ok
    end,
    ok.

-spec get_groups_with_wildcards(binary(), online | both) -> list(binary()).
get_groups_with_wildcards(Host, Type) ->
    Res =
    ets_cache:lookup(
	?SPECIAL_GROUPS_CACHE, {Host, Type},
	fun() ->
	    Res = lists:filtermap(
		fun({Group, Opts}) ->
		    case proplists:get_value(online_users, Opts, false) orelse
			 (Type == both andalso proplists:get_value(all_users, Opts, false)) of
			true -> {true, Group};
			false -> false
		    end
		end,
		groups_with_opts(Host)),
	    {cache, {ok, Res}}
	end),
    case Res of
	{ok, List} -> List;
	_ -> []
    end.

%% Given two lists of groupnames and their options,
%% return the list of displayed groups to the second list
displayed_groups(GroupsOpts, SelectedGroupsOpts) ->
    DisplayedGroups = lists:usort(lists:flatmap(
	fun
	    ({_Group, Opts}) ->
		[G || G <- proplists:get_value(displayed_groups, Opts, []),
		 not lists:member(disabled, Opts)]
	end, SelectedGroupsOpts)),
    [G || G <- DisplayedGroups, not lists:member(disabled, proplists:get_value(G, GroupsOpts, []))].

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
    {Groups, Cache} =
    lists:foldl(
	fun(Group, {Groups, Cache}) ->
	    case get_groups_opts_cached(Host, Group, Cache) of
		{error, Cache2} ->
		    {Groups, Cache2};
		{Opts, Cache3} ->
		    case lists:member(disabled, Opts) of
			false ->
			    {proplists:get_value(displayed_groups, Opts, []) ++ Groups, Cache3};
			_ ->
			    {Groups, Cache3}
		    end
	    end
	end, {[], #{}}, get_user_groups(US)),
    lists:foldl(
	fun(Group, {Groups0, Cache0}) ->
	    case get_groups_opts_cached(Host, Group, Cache0) of
		{error, Cache1} ->
		    {Groups0, Cache1};
		{Opts, Cache2} ->
		    case lists:member(disabled, Opts) of
			false ->
			    {[Group|Groups0], Cache2};
			_ ->
			    {Groups0, Cache2}
		    end
	    end
	end, {[], Cache}, lists:usort(Groups)).

is_user_in_group(US, Group, Host) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    case Mod:is_user_in_group(US, Group, Host) of
	false ->
	    lists:member(US, get_group_users(Host, Group));
	true ->
	    true
    end.

%% @spec (Host::string(), {User::string(), Server::string()}, Group::string()) -> {atomic, ok} | error
add_user_to_group(Host, US, Group) ->
    {_LUser, LServer} = US,
    case lists:member(LServer, ejabberd_config:get_option(hosts)) of
	true -> add_user_to_group2(Host, US, Group);
	false ->
	    ?INFO_MSG("Attempted adding to shared roster user of inexistent vhost ~ts", [LServer]),
	    error
    end.
add_user_to_group2(Host, US, Group) ->
    {LUser, LServer} = US,
    case ejabberd_regexp:run(LUser, <<"^@.+@\$">>) of
      match ->
	  GroupOpts = get_group_opts(Host, Group),
	  MoreGroupOpts = case LUser of
			    <<"@all@">> -> [{all_users, true}];
			    <<"@online@">> -> [{online_users, true}];
			    _ -> []
			  end,
	  set_group_opts(Host, Group,
			 GroupOpts ++ MoreGroupOpts);
      nomatch ->
	  DisplayedToGroups = displayed_to_groups(Group, Host),
	  DisplayedGroups = get_displayed_groups(Group, LServer),
	  push_user_to_displayed(LUser, LServer, Group, Host, both, DisplayedToGroups),
	  push_displayed_to_user(LUser, LServer, Host, both, DisplayedGroups),
	  Mod = gen_mod:db_mod(Host, ?MODULE),
	  Mod:add_user_to_group(Host, US, Group),
	  case use_cache(Mod, Host) of
	      true ->
		  ets_cache:delete(?USER_GROUPS_CACHE, {Host, US}, cache_nodes(Mod, Host)),
		  ets_cache:delete(?GROUP_EXPLICIT_USERS_CACHE, {Host, Group}, cache_nodes(Mod, Host));
	      false ->
		  ok
	  end
    end.

get_displayed_groups(Group, LServer) ->
    get_group_opt(LServer, Group, displayed_groups, []).

push_displayed_to_user(LUser, LServer, Host, Subscription, DisplayedGroups) ->
    [push_members_to_user(LUser, LServer, DGroup, Host,
			  Subscription)
     || DGroup <- DisplayedGroups].

remove_user_from_group(Host, US, Group) ->
    {LUser, LServer} = US,
    case ejabberd_regexp:run(LUser, <<"^@.+@\$">>) of
      match ->
	  GroupOpts = get_group_opts(Host, Group),
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
	  set_group_opts(Host, Group, NewGroupOpts);
      nomatch ->
	  Mod = gen_mod:db_mod(Host, ?MODULE),
	  Result = Mod:remove_user_from_group(Host, US, Group),
	  case use_cache(Mod, Host) of
	      true ->
		  ets_cache:delete(?USER_GROUPS_CACHE, {Host, US}, cache_nodes(Mod, Host)),
		  ets_cache:delete(?GROUP_EXPLICIT_USERS_CACHE, {Host, Group}, cache_nodes(Mod, Host));
	      false ->
		  ok
	  end,
	  DisplayedToGroups = displayed_to_groups(Group, Host),
	  DisplayedGroups = get_displayed_groups(Group, LServer),
	  push_user_to_displayed(LUser, LServer, Group, Host, remove, DisplayedToGroups),
	  push_displayed_to_user(LUser, LServer, Host, remove, DisplayedGroups),
	  Result
    end.

push_members_to_user(LUser, LServer, Group, Host,
		     Subscription) ->
    GroupOpts = get_group_opts(LServer, Group),
    GroupLabel = proplists:get_value(label, GroupOpts, Group), %++
    Members = get_group_users(Host, Group),
    lists:foreach(fun ({U, S}) ->
			  N = get_rosteritem_name(U, S),
			  push_roster_item(LUser, LServer, U, S, N, GroupLabel,
					   Subscription)
		  end,
		  Members).

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
    RosterName = get_rosteritem_name(LUser, LServer),
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
			  GroupLabel = proplists:get_value(label, GroupOpts,
							  Group),
			  lists:foreach(fun ({U, S}) ->
						push_roster_item(U, S, LUser,
								 LServer,
								 RosterName,
								 GroupLabel,
								 Subscription)
					end,
					get_group_users(LServer, Group,
							GroupOpts))
		  end,
		  lists:usort(SpecialGroups ++ UserGroups)).

push_user_to_displayed(LUser, LServer, Group, Host, Subscription, DisplayedToGroupsOpts) ->
    GroupLabel = get_group_opt(Host, Group, label, Group), %++
    [push_user_to_group(LUser, LServer, GroupD, Host,
			GroupLabel, Subscription)
     || GroupD <- DisplayedToGroupsOpts].

push_user_to_group(LUser, LServer, Group, Host,
		   GroupLabel, Subscription) ->
    RosterName = get_rosteritem_name(LUser, LServer),
    lists:foreach(fun ({U, S})
			  when (U == LUser) and (S == LServer) ->
			  ok;
		      ({U, S}) ->
			  case lists:member(S, ejabberd_option:hosts()) of
			      true ->
				  push_roster_item(U, S, LUser, LServer, RosterName, GroupLabel,
						   Subscription);
			      _ ->
				  ok
			  end
		  end,
		  get_group_users(Host, Group)).

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
    mod_roster:push_item(jid:make(User, Server),
			 Item#roster{subscription = none},
			 Item).

push_roster_item(User, Server, ContactU, ContactS, ContactN,
		 GroupLabel, Subscription) ->
    Item = #roster{usj =
		       {User, Server, {ContactU, ContactS, <<"">>}},
		   us = {User, Server}, jid = {ContactU, ContactS, <<"">>},
		   name = ContactN, subscription = Subscription, ask = none,
		   groups = [GroupLabel]},
    push_item(User, Server, Item).

-spec c2s_self_presence({presence(), ejabberd_c2s:state()})
      -> {presence(), ejabberd_c2s:state()}.
c2s_self_presence(Acc) ->
    Acc.

-spec unset_presence(binary(), binary(), binary(), binary()) -> ok.
unset_presence(LUser, LServer, Resource, Status) ->
    Resources = ejabberd_sm:get_user_resources(LUser,
					       LServer),
    ?DEBUG("Unset_presence for ~p @ ~p / ~p -> ~p "
	   "(~p resources)",
	   [LUser, LServer, Resource, Status, length(Resources)]),
    case length(Resources) of
      0 ->
	  lists:foreach(
	      fun(OG) ->
		  DisplayedToGroups = displayed_to_groups(OG, LServer),
		  push_user_to_displayed(LUser, LServer, OG,
					 LServer, remove, DisplayedToGroups),
		  push_displayed_to_user(LUser, LServer,
					 LServer, remove, DisplayedToGroups)
	      end, get_groups_with_wildcards(LServer, online));
      _ -> ok
    end.

%%---------------------
%% Web Admin
%%---------------------

webadmin_menu(Acc, _Host, Lang) ->
    [{<<"shared-roster">>, translate:translate(Lang, ?T("Shared Roster Groups"))}
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
    SRGroups = list_groups(Host),
    FGroups = (?XAE(<<"table">>, [],
		    [?XE(<<"tbody">>,
			 [?XE(<<"tr">>,
				 [?X(<<"td">>),
				  ?XE(<<"td">>, [?CT(?T("Name:"))])
				  ])]++
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
					      <<"">>),
				       ?C(<<" ">>),
				       ?INPUTT(<<"submit">>, <<"addnew">>,
					       ?T("Add New"))])])]))])),
    (?H1GL((translate:translate(Lang, ?T("Shared Roster Groups"))),
	   <<"modules/#mod-shared-roster">>, <<"mod_shared_roster">>))
      ++
      case Res of
	ok -> [?XREST(?T("Submitted"))];
	error -> [?XREST(?T("Bad format"))];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [FGroups, ?BR,
	       ?INPUTTD(<<"submit">>, <<"delete">>,
		       ?T("Delete Selected"))])].

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
	  create_group(Host, Group),
	  ok;
      _ ->
	  error
    end.

list_sr_groups_parse_delete(Host, Query) ->
    SRGroups = list_groups(Host),
    lists:foreach(fun (Group) ->
			  case lists:member({<<"selected">>, Group}, Query) of
			    true -> delete_group(Host, Group);
			    _ -> ok
			  end
		  end,
		  SRGroups),
    ok.

shared_roster_group(Host, Group, Query, Lang) ->
    Res = shared_roster_group_parse_query(Host, Group,
					  Query),
    GroupOpts = get_group_opts(Host, Group),
    Label = get_opt(GroupOpts, label, <<"">>), %%++
    Description = get_opt(GroupOpts, description, <<"">>),
    AllUsers = get_opt(GroupOpts, all_users, false),
    OnlineUsers = get_opt(GroupOpts, online_users, false),
    DisplayedGroups = get_opt(GroupOpts, displayed_groups,
			      []),
    Members = get_group_explicit_users(Host,
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
			     [?XCT(<<"td">>, ?T("Name:")),
			      ?XE(<<"td">>, [?C(Group)]),
			      ?XE(<<"td">>, [?C(<<"">>)])]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, ?T("Label:")),
			      ?XE(<<"td">>,
				  [?INPUT(<<"text">>, <<"label">>, Label)]),
			      ?XE(<<"td">>, [?CT(?T("Name in the rosters where this group will be displayed"))])]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, ?T("Description:")),
			      ?XE(<<"td">>,
				  [?TEXTAREA(<<"description">>,
					     integer_to_binary(lists:max([3,
                                                                               DescNL])),
					     <<"20">>, Description)]),
			      ?XE(<<"td">>, [?CT(?T("Only admins can see this"))])
]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, ?T("Members:")),
			      ?XE(<<"td">>,
				  [?TEXTAREA(<<"members">>,
					     integer_to_binary(lists:max([3,
                                                                               length(Members)+3])),
					     <<"20">>, FMembers)]),
			      ?XE(<<"td">>, [?C(<<"JIDs, @all@, @online@">>)])
]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, ?T("Displayed:")),
			      ?XE(<<"td">>,
				  [?TEXTAREA(<<"dispgroups">>,
					     integer_to_binary(lists:max([3,											        length(FDisplayedGroups)])),
					     <<"20">>,
					     list_to_binary(FDisplayedGroups))]),
			      ?XE(<<"td">>, [?CT(?T("Groups that will be displayed to the members"))])
])])])),
    (?H1GL((translate:translate(Lang, ?T("Shared Roster Groups"))),
	   <<"modules/#mod-shared-roster">>, <<"mod_shared_roster">>))
      ++
      [?XC(<<"h2">>, translate:translate(Lang, ?T("Group")))] ++
	case Res of
	  ok -> [?XREST(?T("Submitted"))];
	  {error_elements, NonAddedList1, NG1} ->
		make_error_el(Lang,
		    ?T("Members not added (inexistent vhost!): "),
		    [jid:encode({U,S,<<>>}) || {U,S} <- NonAddedList1])
		++ make_error_el(Lang, ?T("'Displayed groups' not added (they do not exist!): "), NG1);
	  error -> [?XREST(?T("Bad format"))];
	  nothing -> []
	end
	  ++
	  [?XAE(<<"form">>,
		[{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
		[FGroup, ?BR,
		 ?INPUTT(<<"submit">>, <<"submit">>, ?T("Submit"))])].

make_error_el(_, _, []) ->
    [];
make_error_el(Lang, Message, BinList) ->
    NG2 = str:join(BinList, <<", ">>),
    NG3 = translate:translate(Lang, Message),
    NG4 = str:concat(NG3, NG2),
    [?XRES(NG4)].

shared_roster_group_parse_query(Host, Group, Query) ->
    case lists:keysearch(<<"submit">>, 1, Query) of
      {value, _} ->
	  {value, {_, Label}} = lists:keysearch(<<"label">>, 1,
					       Query), %++
	  {value, {_, Description}} =
	      lists:keysearch(<<"description">>, 1, Query),
	  {value, {_, SMembers}} = lists:keysearch(<<"members">>,
						   1, Query),
	  {value, {_, SDispGroups}} =
	      lists:keysearch(<<"dispgroups">>, 1, Query),
	  LabelOpt = if Label == <<"">> -> [];
		       true -> [{label, Label}] %++
		    end,
	  DescriptionOpt = if Description == <<"">> -> [];
			      true -> [{description, Description}]
			   end,
	  DispGroups1 = str:tokens(SDispGroups, <<"\r\n">>),
	  {DispGroups, WrongDispGroups} = filter_groups_existence(Host, DispGroups1),
	  DispGroupsOpt = if DispGroups == [] -> [];
			     true -> [{displayed_groups, DispGroups}]
			  end,
	  OldMembers = get_group_explicit_users(Host,
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
	  set_group_opts(Host, Group,
			 LabelOpt ++
			 DispGroupsOpt ++
			 DescriptionOpt ++
			 AllUsersOpt ++ OnlineUsersOpt),
	  if NewMembers == error -> error;
	     true ->
		 AddedMembers = NewMembers -- OldMembers,
		 RemovedMembers = OldMembers -- NewMembers,
		 lists:foreach(
		     fun(US) ->
			 remove_user_from_group(Host,
						US,
						Group)
		     end,
		     RemovedMembers),
		 NonAddedMembers = lists:filter(
		     fun(US) ->
			 error == add_user_to_group(Host, US,
						    Group)
		     end,
		     AddedMembers),
		 case (NonAddedMembers /= []) or (WrongDispGroups /= []) of
		    true -> {error_elements, NonAddedMembers, WrongDispGroups};
		    false -> ok
		 end
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

filter_groups_existence(Host, Groups) ->
    lists:partition(
	fun(Group) -> error /= get_group_opts(Host, Group) end,
	Groups).

displayed_groups_update(Members, DisplayedGroups, Subscription) ->
    lists:foreach(
      fun({U, S}) ->
	      push_displayed_to_user(U, S, S, Subscription, DisplayedGroups)
      end, Members).

opts_to_binary(Opts) ->
    lists:map(
      fun({label, Label}) ->
              {label, iolist_to_binary(Label)};
         ({name, Label}) -> % For SQL backwards compat with ejabberd 20.03 and older
              {label, iolist_to_binary(Label)};
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

mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(use_cache) ->
    econf:bool();
mod_opt_type(cache_size) ->
    econf:pos_int(infinity);
mod_opt_type(cache_missed) ->
    econf:bool();
mod_opt_type(cache_life_time) ->
    econf:timeout(second, infinity).

mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {use_cache, ejabberd_option:use_cache(Host)},
     {cache_size, ejabberd_option:cache_size(Host)},
     {cache_missed, ejabberd_option:cache_missed(Host)},
     {cache_life_time, ejabberd_option:cache_life_time(Host)}].

mod_doc() ->
    #{desc =>
	  [?T("This module enables you to create shared roster groups: "
	      "groups of accounts that can see members from (other) groups "
	      "in their rosters."), "",
	   ?T("The big advantages of this feature are that end users do not "
	      "need to manually add all users to their rosters, and that they "
	      "cannot permanently delete users from the shared roster groups. "
	      "A shared roster group can have members from any XMPP server, "
	      "but the presence will only be available from and to members of "
	      "the same virtual host where the group is created. It still "
	      "allows the users to have / add their own contacts, as it does "
	      "not replace the standard roster. Instead, the shared roster "
	      "contacts are merged to the relevant users at retrieval time. "
	      "The standard user rosters thus stay unmodified."), "",
	   ?T("Shared roster groups can be edited via the Web Admin, "
	      "and some API commands called 'srg_*'. "
	      "Each group has a unique name and those parameters:"), "",
	   ?T("- Label: Used in the rosters where this group is displayed."),"",
	   ?T("- Description: of the group, which has no effect."), "",
	   ?T("- Members: A list of JIDs of group members, entered one per "
	      "line in the Web Admin. The special member directive '@all@' "
	      "represents all the registered users in the virtual host; "
	      "which is only recommended for a small server with just a few "
	      "hundred users. The special member directive '@online@' "
	      "represents the online users in the virtual host. With those "
	      "two directives, the actual list of members in those shared "
	      "rosters is generated dynamically at retrieval time."), "",
	   ?T("- Displayed: A list of groups that will be in the "
	      "rosters of this group's members. A group of other vhost can "
	      "be identified with 'groupid@vhost'."), "",
	   ?T("This module depends on _`mod_roster`_. "
	      "If not enabled, roster queries will return 503 errors.")],
      opts =>
          [{db_type,
            #{value => "mnesia | sql",
              desc =>
                  ?T("Define the type of storage where the module will create "
		     "the tables and store user information. The default is "
		     "the storage defined by the top-level _`default_db`_ option, "
		     "or 'mnesia' if omitted. If 'sql' value is defined, "
		     "make sure you have defined the database.")}},
           {use_cache,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`use_cache`_ option, but applied to this module only.")}},
           {cache_size,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Same as top-level _`cache_size`_ option, but applied to this module only.")}},
           {cache_missed,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`cache_missed`_ option, but applied to this module only.")}},
           {cache_life_time,
            #{value => "timeout()",
              desc =>
                  ?T("Same as top-level _`cache_life_time`_ option, but applied to this module only.")}}],
      example =>
	  [{?T("Take the case of a computer club that wants all its members "
	       "seeing each other in their rosters. To achieve this, they "
	       "need to create a shared roster group similar to this one:"),
	    ["Name: club_members",
	     "Label: Club Members",
	     "Description: Members from the computer club",
	     "Members: member1@example.org, member2@example.org, member3@example.org",
	     "Displayed Groups: club_members"]},
	   {?T("In another case we have a company which has three divisions: "
	       "Management, Marketing and Sales. All group members should see "
	       "all other members in their rosters. Additionally, all managers "
	       "should have all marketing and sales people in their roster. "
	       "Simultaneously, all marketeers and the whole sales team "
	       "should see all managers. This scenario can be achieved by "
	       "creating shared roster groups as shown in the following lists:"),
	    ["First list:",
	     "Name: management",
	     "Label: Management",
	     "Description: Management",
	     "Members: manager1@example.org, manager2@example.org",
	     "Displayed: management, marketing, sales",
	     "",
	     "Second list:",
	     "Name: marketing",
	     "Label: Marketing",
	     "Description: Marketing",
	     "Members: marketeer1@example.org, marketeer2@example.org, marketeer3@example.org",
	     "Displayed: management, marketing",
	     "",
	     "Third list:",
	     "Name: sales",
	     "Label: Sales",
	     "Description: Sales",
	     "Members: salesman1@example.org, salesman2@example.org, salesman3@example.org",
	     "Displayed: management, sales"
	    ]}
	  ]}.
