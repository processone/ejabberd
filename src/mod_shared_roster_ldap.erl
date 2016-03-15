%%%-------------------------------------------------------------------
%%% File    : mod_shared_roster_ldap.erl
%%% Author  : Realloc <realloc@realloc.spb.ru>
%%%           Marcin Owsiany <marcin@owsiany.pl>
%%%           Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Contributor : Mike Kaganski <mikekaganski@hotmail.com>
%%% Description : LDAP shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_shared_roster_ldap).

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-export([get_user_roster/2, get_subscription_lists/3,
	 get_jid_info/4, process_item/2, in_subscription/6,
	 out_subscription/4, mod_opt_type/1, opt_type/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("eldap.hrl").
-define(ERROR_MSG(Fmt, Args), error_logger:error_msg(Fmt, Args)).

-define(CACHE_SIZE, 1).
-define(CACHE_VALIDITY, 300). %% in seconds
-define(LDAP_SEARCH_TIMEOUT, 5). %% Timeout for LDAP search queries in seconds
-define(INVALID_SETTING_MSG, "~s is not properly set! ~s will not function.").

-record(state,
	{host = <<"">>                                :: binary(),
         eldap_id = <<"">>                            :: binary(),
         servers = []                                 :: [binary()],
         backups = []                                 :: [binary()],
         port = ?LDAP_PORT                            :: inet:port_number(),
         tls_options = []                             :: list(),
	 dn = <<"">>                                  :: binary(),
         base = <<"">>                                :: binary(),
         password = <<"">>                            :: binary(),
         uid = <<"">>                                 :: binary(),
         deref_aliases = never                        :: never | searching |
                                                         finding | always,
         group_attr = <<"">>                          :: binary(),
	 group_desc = <<"">>                          :: binary(),
         user_desc = <<"">>                           :: binary(),
         uid_format = <<"">>                          :: binary(),
	 uid_format_re = <<"">>                       :: binary(),
         filter = <<"">>                              :: binary(),
         ufilter = <<"">>                             :: binary(),
         rfilter = <<"">>                             :: binary(),
         gfilter = <<"">>                             :: binary(),
	 auth_check = true                            :: boolean(),
         %% Group data parameters
	 group_base = <<"">>                          :: binary(),
         %% - Subgroup of roster filter
         %% This filter defines which groups are displayed in the shared roster
         %% Valid values are 'all' or "LDAP filter string" or "LDAP filter string containing %g"
         shgfilter = <<"">>                           :: binary(),
         shg_attr = <<"">>                            :: binary(),
         %% - Subgroup of group filter
         group_is_dn = true                           :: boolean(),
         member_attr = <<"">>                          :: binary(),
         %% User data parameters
         member_selection_mode = memberattr_dn        :: memberattr_normal | memberattr_dn | 
	 		       	 		      	 group_children,
         %% Algorithm control parameters
         subscribe_all = false                        :: binary(),
         roster_cache_size = ?CACHE_SIZE              :: non_neg_integer(),
         roster_cache_validity = ?CACHE_VALIDITY      :: non_neg_integer()}).

%% If #state.member_selection_mode is memberattr_normal or memberattr_dn,
%% then members is list of member_attr values;
%% if #state.member_selection_mode is group_children,
%% then members is dn of the group (to make it possible to search for its subtree)
-record(group_info, {desc, members}).

-record(user_info, {us, name}).

-record(shared_roster_item, {us, name, groups}).

% Groups visible to this group
% grp may be atom 'all' or a group name string.
% shgrps is a list containing one or more grp
-record(shg_data, {grp, shgrps}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE,
                          [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
                 permanent, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
get_user_roster(Items, {U, S} = US) ->
    {ok, State} = eldap_utils:get_state(S, ?MODULE),
    SRUsers = get_shared_roster(State, US),
    %%?ERROR_MSG("XXXXXX get_user_roster: SRUsers=~p", [SRUsers]),
    %% If partially subscribed users are also in shared roster,
    %% show them as totally subscribed:
    {NewItems1, SRUsersRest} = lists:mapfoldl(
        fun(Item, SRUsers1) ->
            {_, _, {U1, S1, _}} = Item#roster.usj,
            US1 = {U1, S1},
            case lists:keytake(US1, #shared_roster_item.us, SRUsers1) of
            %%case dict:find(US1, SRUsers1) of
                {value, _, SRUsers2} -> {Item#roster{subscription = both, ask = none}, SRUsers2};
                %%{ok, _GroupNames} -> {Item#roster{subscription = both, ask = none}, dict:erase(US1, SRUsers1)};
                false -> {Item, SRUsers1}
            end
        end,
        SRUsers, Items),
    %% Export items in roster format:
    SRItems = [#roster{usj = {U, S, {U1, S1, <<"">>}},
                       us = US,
                       jid = {U1, S1, <<"">>},
                       name = Name,
                       subscription = both,
                       ask = none,
                       groups = Groups} ||
              #shared_roster_item{us = {U1, S1}, name = Name, groups = Groups} <- SRUsersRest],
    %% SRItems = [#roster{usj = {U, S, {U1, S1, <<"">>}},
    %%            us = US, jid = {U1, S1, <<"">>},
    %%            name = get_user_name(U1, S1), subscription = both,
    %%            ask = none, groups = GroupNames}
    %%            || {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

%% This function in use to rewrite the roster entries when moving or renaming
%% them in the user contact list.
process_item(RosterItem, _Host) ->
    {ok, State} = eldap_utils:get_state(_Host, ?MODULE),
    {User,Server,_Resource} = RosterItem#roster.jid,
    USTo = {User,Server},
    SR = get_shared_roster(State, RosterItem#roster.us),
    case lists:keysearch(USTo, #shared_roster_item.us, SR) of
        false ->
            RosterItem;
        {value, #shared_roster_item{groups = Groups}} when RosterItem#roster.subscription == remove ->
            %% Roster item cannot be removed:
            %% We simply reset the original groups:
            RosterItem#roster{subscription = both, ask = none,
                              groups=Groups};
        _ ->
            RosterItem#roster{subscription = both, ask = none}
    end.

get_subscription_lists({F, T}, User, Server) ->
    U = jid:nodeprep(User),
    S = jid:nameprep(Server),
    {ok, State} = eldap_utils:get_state(S, ?MODULE),
    SRJIDs = get_presense_subscribers(State, {U, S}),
%?INFO_MSG("SRJIDs: ~p", [SRJIDs]),
    {lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T)}.

get_jid_info({Subscription, Groups}, User, Server, JID) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    {U1, S1, _} = jid:tolower(JID),
    US1 = {U1, S1},
    SR = get_shared_roster(State, {User, Server}),
    case lists:keysearch(US1, #shared_roster_item.us, SR) of
        false -> {Subscription, Groups};
        {value, #shared_roster_item{groups = GroupNames}} when Groups == [] -> {both, GroupNames};
        _ -> {both, Groups}
    end.

in_subscription(Acc, User, Server, JID, Type, _Reason) ->
    process_subscription(in, User, Server, JID, Type, Acc).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, false).

process_subscription(Direction, User, Server, JID, _Type, Acc) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    {U1, S1, _} = jid:tolower(JID),
    US1 = {U1, S1},
    SR = get_shared_roster(State, {User, Server}),
    case lists:keysearch(US1, #shared_roster_item.us, SR) of
        false -> Acc;
        _ when Direction == in -> {stop, false};
        _ -> stop
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    State = parse_options(Host, Opts),
    if
        State#state.roster_cache_size > 0 ->
            cache_tab:new(shared_roster_ldap_sr,
                          [{max_size, State#state.roster_cache_size},
                           {lru, false}, % We don't need LRU algorithm
                           {life_time, State#state.roster_cache_validity}]);
        true ->
            false
    end,
    ejabberd_hooks:add(roster_get, Host,
                       ?MODULE, get_user_roster, 70),
    ejabberd_hooks:add(roster_in_subscription, Host,
                       ?MODULE, in_subscription, 30),
    ejabberd_hooks:add(roster_out_subscription, Host,
                       ?MODULE, out_subscription, 30),
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
                       ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:add(roster_get_jid_info, Host,
                       ?MODULE, get_jid_info, 70),
    ejabberd_hooks:add(roster_process_item, Host,
                       ?MODULE, process_item, 50),
    eldap_pool:start_link(State#state.eldap_id,
                          State#state.servers,
                          State#state.backups,
                          State#state.port,
                          State#state.dn,
                          State#state.password,
                          State#state.tls_options),
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(roster_get, Host,
                          ?MODULE, get_user_roster, 70),
    ejabberd_hooks:delete(roster_in_subscription, Host,
                          ?MODULE, in_subscription, 30),
    ejabberd_hooks:delete(roster_out_subscription, Host,
                          ?MODULE, out_subscription, 30),
    ejabberd_hooks:delete(roster_get_subscription_lists, Host,
                          ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
                          ?MODULE, get_jid_info, 70),
    ejabberd_hooks:delete(roster_process_item, Host,
                          ?MODULE, process_item, 50).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_eldap_search(PoolName, Opts) ->
    case eldap_pool:search(PoolName, Opts) of
        #eldap_search_result{entries = Es} ->
            %% A result with entries. Return their list.
            Es;
        Err ->
            %% Something else. Pretend we got no results.
            ?ERROR_MSG("Error searching: ~p ~p", [Err, Opts]),
            []
    end.

%% Pass given Filter or FilterTemplate and SubstList to eldap_filter:parse,
%% and if successful, run LDAP search on the whole subtree of Base, using
%% resulting filter, retrieving given AttributesList. Return the result entries.
%% On any error, print an error message and return an empty list of results.
eldap_search(State, Base, EldapFilter, AttributesList) when is_tuple(EldapFilter) ->
    do_eldap_search(State#state.eldap_id,
                    [{base, Base},
%%                     {scope, wholeSubtree} %% This is the default
                     {filter, EldapFilter},
                     {timeout, ?LDAP_SEARCH_TIMEOUT},
                     {deref_aliases, State#state.deref_aliases},
                     {attributes, AttributesList}]);
%% Filter is string
eldap_search(State, Base, Filter, AttributesList) ->
    eldap_search(State, Base, Filter, [], AttributesList).

eldap_search(State, Base, FilterTemplate, SubstList, AttributesList) ->
    case apply(eldap_filter, parse, [eldap_filter:do_sub(FilterTemplate, SubstList)]) of
        {ok, EldapFilter} ->
            %% Filter parsing succeeded
            eldap_search(State, Base, EldapFilter, AttributesList);
        Err ->
            %% Filter parsing failed. Pretend we got no results.
            ?ERROR_MSG("Error parsing filter: ~p", [Err]),
            []
    end.

%% The same as above, but gets the Attributes for the specified DN.
%% Note that this function doesn't honor the State's base DN;
%% TODO: fix this (create a custom check?)
eldap_search_dn(State, DN, EldapFilter, AttributesList) when is_tuple(EldapFilter) ->
    do_eldap_search(State#state.eldap_id,
                    [{scope, baseObject},
                     {base, DN},
                     {filter, EldapFilter},
                     {timeout, ?LDAP_SEARCH_TIMEOUT},
                     {deref_aliases, State#state.deref_aliases},
                     {attributes, AttributesList}]);
%% Filter is string.
eldap_search_dn(State, DN, Filter, AttributesList) ->
    case eldap_filter:parse(Filter) of
        {ok, EldapFilter} ->
            %% Filter parsing succeeded
            eldap_search_dn(State, DN, EldapFilter, AttributesList);
        Err ->
            %% Filter parsing failed. Pretend we got no results.
            ?ERROR_MSG("Error parsing filter: ~p", [Err]),
            []
    end.

intersection(L1,L2) -> lists:filter(fun(X) -> lists:member(X,L1) end, L2).

filter_roster(Roster, all) -> Roster;
filter_roster(_, []) -> [];
filter_roster(Roster, IncludeGroups) when is_list(IncludeGroups) ->
    lists:foldl(
        fun(RosterItem, Acc) ->
            case intersection(IncludeGroups, RosterItem#shared_roster_item.groups) of
                [] -> Acc;
                CommonGroups -> [RosterItem#shared_roster_item{groups=CommonGroups} | Acc]
            end
        end,
        [], Roster).

get_user_visible_groups(UserGroups, VisibilityMap) ->
    lists:foldl(
        fun(Group, Acc) ->
            case (lists:keysearch(Group, #shg_data.grp, VisibilityMap)) of
                {value, #shg_data{shgrps=Gs}} when is_list(Gs) -> Gs ++ Acc;
                _ -> Acc
            end
        end,
        UserGroups, UserGroups).

%% Returns [#shared_roster_item];
%% Removes the US from returned data
%% If State#state.user_groups_only is 'true', then it removes all users that are not in US's groups,
%% and also removes the groups from the users that the US is not member of.
get_shared_roster(State, {_, Server} = US) ->
    case (catch get_full_roster(State, Server)) of
        {ok, {VisibilityMap, FullRoster}} ->
	    %%?ERROR_MSG("XXXXXX get_shared_roster: VMap=~p FullRoster=~p", [VisibilityMap, FullRoster]),
            CommonRosterGroups = lists:foldl(
                fun(_, all) -> all;
                   (#shg_data{grp=all, shgrps=all}, _) -> all;
                   (#shg_data{grp=all, shgrps=Gs}, Acc) when is_list(Gs) -> Gs ++ Acc;
                   (_, Acc) -> Acc
                end,
                [], VisibilityMap),
            case lists:keytake(US, #shared_roster_item.us, FullRoster) of
                false -> filter_roster(FullRoster, CommonRosterGroups);
                {value, #shared_roster_item{groups=UserGroups}, Roster2} ->
                    VisibleGroups = case (CommonRosterGroups) of
                        all -> all;
                        CRG -> get_user_visible_groups(UserGroups, VisibilityMap) ++ CRG
                    end,
                    filter_roster(Roster2, VisibleGroups)
            end;
        {'EXIT', CatchData} -> ?ERROR_MSG("Error getting shared roster for user ~p: ~p", [US, CatchData]), [];
        _Unexpected -> []
    end.

%% 1. If user is not a member of shared roster -> no additional subscriptions
%% 2. Else if ldap_subscribe_all is set AND this user is member of a group published to all ->
%%                                      add all registered users of this vhost
%% 3. Else add only those groups this user' groups are published to
get_presense_subscribers(State, {_, Server} = US) ->
    case (catch get_full_roster(State, Server)) of
        {ok, {VisibilityMap, FullRoster}} ->
            case lists:keytake(US, #shared_roster_item.us, FullRoster) of
                false -> []; % Case #1
                {value, #shared_roster_item{groups=UserGroups}, Roster2} ->
                    AllGroups = lists:usort(lists:foldl(
                        fun(#shared_roster_item{groups=Gs}, Acc) -> Gs ++ Acc end,
                        [], FullRoster)),
                    Fun = case (State#state.subscribe_all) of
                        true -> % Possible case 2
                            fun(_, all) -> all;
                               (#shg_data{grp=all, shgrps=all}, _) -> all;
                               (#shg_data{grp=all, shgrps=Gs}, Acc) when is_list(Gs) ->
                                    case intersection(Gs, UserGroups) of
                                        [] -> Acc;
                                        _SomeCommon -> all
                                    end;
                               (#shg_data{grp=G, shgrps=Gs}, Acc) when is_list(Gs) ->
                                    case intersection(Gs, UserGroups) of
                                        [] -> Acc;
                                        _SomeCommon -> [G | Acc]
                                    end;
                               (_, Acc) -> Acc
                            end;
                        _False -> % Case 3
                            fun(#shg_data{grp=all}, Acc) -> AllGroups ++ Acc;
                               (#shg_data{grp=G, shgrps=Gs}, Acc) when is_list(Gs) ->
                                    case intersection(Gs, UserGroups) of
                                        [] -> Acc;
                                        _SomeCommon -> [G | Acc]
                                    end;
                               (_, Acc) -> Acc
                            end
                    end,
                    PublishTo = lists:foldl(Fun, [], VisibilityMap),
                    case (PublishTo) of
                        all ->
                            [{U1, S1, <<"">>} || {U1, S1} <- ejabberd_auth:get_vh_registered_users(Server)];
                        Groups ->
                            [{U1, S1, <<"">>} || #shared_roster_item{us = {U1, S1}} <- filter_roster(Roster2, UserGroups ++ Groups)]
                    end
            end;
        {'EXIT', CatchData} -> ?ERROR_MSG("Error getting shared roster for user ~p: ~p", [US, CatchData]), [];
        _Unexpected -> []
    end.

get_full_roster(State, Server) when State#state.roster_cache_size > 0 ->
    cache_tab:dirty_lookup(shared_roster_ldap_sr,
                           {Server},
                           fun() -> search_roster_info(State, Server) end);
get_full_roster(State, Server) ->
    search_roster_info(State, Server).

search_visible_groups(State, _) when State#state.shgfilter == all ->
    [{all, all}];
search_visible_groups(State, _) when State#state.shgfilter == none ->
    [{all, none}];
search_visible_groups(State, Groups) ->
    case (string:str(State#state.shgfilter, "%g")) of
        0 -> [{all, search_group_visible_groups(State, "")}];
        _ -> lists:map(
            fun(Group) -> {Group, search_group_visible_groups(State, Group)} end,
            Groups)
    end.

search_group_visible_groups(State, Group) ->
    Entries = eldap_search(State, State#state.group_base, State#state.shgfilter, [{<<"%g">>, Group}], [State#state.shg_attr]),
    lists:usort(lists:flatmap(
        fun(#eldap_entry{attributes = Attrs}) ->
            case Attrs of
                [{_GroupAttr, ValuesList}] ->
                    ValuesList;
                _ ->
                    []
            end
        end, Entries)).

group2name(all, _) -> all;
group2name(none, _) -> none;
group2name(Group, GroupNames) ->
    case (lists:keysearch(Group, 1, GroupNames)) of
        {value, {_, Name}} -> Name;
        _ -> false
    end.

groups2names(all, _) -> all;
groups2names(none, _) -> none;
groups2names(GroupList, GroupNames) ->
    lists:foldl(
        fun(G, Acc) ->
            case (group2name(G, GroupNames)) of
                false -> Acc;
                Name -> [Name | Acc]
            end
        end,
        [], GroupList).

prep_vis_map(VisGroups, GroupNames) ->
    lists:foldl(
        fun({G, Gs}, Acc) ->
            case (group2name(G, GroupNames)) of
                false -> Acc;
                Name -> [#shg_data{grp=Name, shgrps=groups2names(Gs, GroupNames)} | Acc]
            end
        end,
        [], VisGroups).

search_roster_info(State, _Host) ->
    Entries = eldap_search(State, State#state.group_base, State#state.rfilter, [State#state.group_attr]),
    AllGroupIds = lists:usort(lists:flatmap(
        fun(#eldap_entry{attributes = Attrs}) ->
            case Attrs of
                [{_GroupAttr, ValuesList}] ->
                    ValuesList;
                _ ->
                    []
            end
        end, Entries)),
    VisGroups = search_visible_groups(State, AllGroupIds),
    %%?ERROR_MSG("XXXXXX search_roster_info: VisGroups=~p", [VisGroups]),

    {GroupNames, RosterItems} = case State#state.member_selection_mode of
        group_children ->
            {GroupNames0, UsersDict0} = lists:foldl(
                fun(Group, {GrNAcc, Dict1} = Acc) ->
                    case search_group_info(State, Group) of
                        {ok, #group_info{desc = GroupName, members = GroupDN}} ->
                            {[{Group, GroupName} | GrNAcc], search_users_info(State, GroupDN, GroupName, Dict1)};
                        _ -> Acc             %% Error getting group data -> No users!
                    end
                end,
                {[], dict:new()}, AllGroupIds),

            {GroupNames0, dict:fold(
                fun(#user_info{us=US, name=UserName}, Groups, AccIn) ->
                    [#shared_roster_item{us = US, name = UserName, groups = Groups} | AccIn]
                end,
                [], UsersDict0)};
        _ ->
            {GroupNames1, UsersDict1} = lists:foldl(
                fun(Group, {GrNAcc, Dict1} = Acc) ->
                    case search_group_info(State, Group) of
                        {ok, #group_info{desc = GroupName, members = Members}} ->
                            {[{Group, GroupName} | GrNAcc], lists:foldl(
                                fun(Member, Dict) -> dict:append(Member, GroupName, Dict) end,
                                Dict1, Members)};
                        _ -> Acc             %% Error getting group data -> No users!
                    end
                end,
                {[], dict:new()}, AllGroupIds),

            %%?ERROR_MSG("UsersDict1: ~p", [UsersDict1]),
            %%?ERROR_MSG("GroupNames1: ~p", [GroupNames1]),

            {GroupNames1, dict:fold(
                fun(Member, Groups, AccIn) ->
                    case search_user_info(State, Member) of
                        {ok, #user_info{us=US, name=UserName}} ->
		             %%?ERROR_MSG("XXXX found user: ~p ~p ~p", [UserName, Groups, US]),
                            [#shared_roster_item{us = US, name = UserName, groups = Groups} | AccIn];
                        _ -> AccIn
                    end
                end,
                [], UsersDict1)}
    end,

    VisibilityMap = prep_vis_map(VisGroups, GroupNames),
    {ok, {VisibilityMap, RosterItems}}.

search_group_info(State, Group) ->
    AttList = case State#state.member_selection_mode of
        group_children -> [State#state.group_desc];
        _              -> [State#state.group_desc, State#state.member_attr]
    end,
    SearchResult = case State#state.group_is_dn of
        true -> eldap_search_dn(State,
                                Group,
                                State#state.gfilter,
                                AttList);
        _    -> eldap_search(State,
                             State#state.group_base,
                             State#state.gfilter,
                             [{<<"%g">>, Group}],
                             AttList)
    end,
    case SearchResult of
        [] ->
            error;
        LDAPEntries ->
            case State#state.member_selection_mode of
                group_children ->
                    [#eldap_entry{object_name=Name, attributes=Attrs} | _] = LDAPEntries,
                    {ok, #group_info{desc = eldap_utils:get_ldap_attr(State#state.group_desc, Attrs),
                                     members = Name}};
                _ ->
                    {GroupDesc, MembersLists} = lists:foldl(
                        fun(#eldap_entry{attributes=Attrs}, {DescAcc, MembersAcc}) ->
                            case {eldap_utils:get_ldap_attr(State#state.group_desc, Attrs),
                                  lists:keysearch(State#state.member_attr, 1, Attrs)} of
                                {Desc, {value, {GroupMemberAttr, Members}}}
                                when GroupMemberAttr == State#state.member_attr ->
                                    {Desc, lists:usort(Members ++ MembersAcc)};
                                _ ->
                                    {DescAcc, MembersAcc}
                            end
                        end,
                        {Group, []}, LDAPEntries),
                    {ok, #group_info{desc = GroupDesc,
                                     members = lists:usort(MembersLists)}}
            end
    end.

%% Takes the attributes from LDAP user search;
%% returns error or {ok, #user_info}
construct_user(State, Attrs) ->
    Extractor = case State#state.uid_format_re of
        <<"">> -> fun(UID) ->
                catch eldap_utils:get_user_part(UID, State#state.uid_format)
              end;
        _  -> fun(UID) ->
                catch get_user_part_re(UID, State#state.uid_format_re)
              end
    end,
    AuthChecker = case State#state.auth_check of
                  true -> fun ejabberd_auth:is_user_exists/2;
                  _ -> fun(_U, _S) -> true end
    end,
    Host = State#state.host,

    case {eldap_utils:get_ldap_attr(State#state.uid, Attrs),
          eldap_utils:get_ldap_attr(State#state.user_desc, Attrs)} of
        {UID, Desc} when UID /= "" ->
            %% By returning "" get_ldap_attr means "not found"
            case Extractor(UID) of
                {ok, UID1} ->
                    UID2 = jid:nodeprep(UID1),
                    case UID2 of
                        error -> error;
                        _ ->
                            case AuthChecker(UID2, Host) of
                                true -> {ok, #user_info{us={UID2, Host}, name=Desc}};
                                _ -> error
                            end
                    end;
                _ -> error
            end;
        _ ->
            error
    end.

%% This function is used when State#state.member_selection_mode is group_children
%% Returns UsersDict to which the users (#user_info) of this group are added
%%search_users_info(State, GroupInfo) ->
search_users_info(State, GroupDN, GroupName, UsersDict) ->
    SearchResult = eldap_search(State,
                                GroupDN,
                                State#state.ufilter,
                                [State#state.user_desc, State#state.uid]),
    lists:foldl(
        fun(#eldap_entry{attributes=Attrs}, Dict1) ->
            case construct_user(State, Attrs) of
                {ok, UserInfo} ->
                    dict:append(UserInfo, GroupName, Dict1);
                _ -> Dict1
            end
        end, UsersDict, SearchResult).

%% This function is used when State#state.member_selection_mode is either memberattr_normal or memberattr_dn
search_user_info(State, User) ->
            %%?ERROR_MSG("XXX search_user_info: searching for ~p", [User]),
    SearchResult = case State#state.member_selection_mode of
        memberattr_dn -> eldap_search_dn(State,
                                         User,
                                         State#state.ufilter,
                                         [State#state.user_desc, State#state.uid]);
        memberattr_normal -> eldap_search(State,
                                          State#state.base,
                                          State#state.ufilter,
                                          [{<<"%u">>, User}],
                                          [State#state.user_desc, State#state.uid])
    end,
    case SearchResult of
        [#eldap_entry{attributes=Attrs}|_] ->
            construct_user(State, Attrs);
        [] ->
            %%?ERROR_MSG("XX not found", []),
            error
    end.

%% Getting User ID part by regex pattern
get_user_part_re(String, Pattern) ->
    case catch re:run(String, Pattern) of
        {match, Captured} ->
            {First, Len} = lists:nth(2,Captured),
            Result = string:sub_string(String, First+1, First+Len),
            {ok,Result};
        _ -> {error, badmatch}
    end.

% select(SelectFirst, First, Second) ->
%     case SelectFirst of
%         true -> First;
%         _    -> Second
%     end.

% prepare_filter(Opts, Name, Default, ReturnParsed) ->
%     F = gen_mod:get_opt(Name, Opts, Default),
%     prepare_filter(F, Name, ReturnParsed).

% prepare_filter(F, Name, ReturnParsed) ->
%     case eldap_filter:parse(F) of
%         {ok, EldapFilter} ->
%             case ReturnParsed of
%                 true -> EldapFilter;
%                 _    -> F
%             end;
%         _ ->
%             ?ERROR_MSG(?INVALID_SETTING_MSG, [atom_to_list(Name), ?MODULE]),
%             []
%     end.

parse_options(Host, Opts) ->
    Eldap_ID = jlib:atom_to_binary(gen_mod:get_module_proc(Host, ?MODULE)),
    Cfg = eldap_utils:get_config(Host, Opts),
    GroupAttr = gen_mod:get_opt(ldap_groupattr, Opts,
                                fun iolist_to_binary/1,
                                <<"cn">>),
    GroupDesc = gen_mod:get_opt(ldap_groupdesc, Opts,
                                fun iolist_to_binary/1,
                                GroupAttr),
    UserDesc = gen_mod:get_opt(ldap_userdesc, Opts,
                               fun iolist_to_binary/1,
                               <<"cn">>),
    UserUID = gen_mod:get_opt(ldap_useruid, Opts,
                              fun iolist_to_binary/1,
                              <<"cn">>),
    UIDAttr = gen_mod:get_opt(ldap_memberattr, Opts,
                              fun iolist_to_binary/1,
                              <<"memberUid">>),
    UIDAttrFormat = gen_mod:get_opt(ldap_memberattr_format, Opts,
                                    fun iolist_to_binary/1,
                                    <<"%u">>),
    UIDAttrFormatRe = gen_mod:get_opt(ldap_memberattr_format_re, Opts,
                                      fun(S) ->
                                              Re = iolist_to_binary(S),
                                              {ok, MP} = re:compile(Re),
                                              MP
                                      end, <<"">>),
    AuthCheck = gen_mod:get_opt(ldap_auth_check, Opts,
                                fun(on) -> true;
                                   (off) -> false;
                                   (false) -> false;
                                   (true) -> true
                                end, true),
    RosterCacheValidity = gen_mod:get_opt(
                           {ldap_group_cache_validity, Host}, Opts,
                           fun(I) when is_integer(I), I>0 -> I end,
                           ?CACHE_VALIDITY),
    RosterCacheSize = gen_mod:get_opt(
                       {ldap_roster_cache_size, Host}, Opts,
                       fun(I) when is_integer(I), I>0 -> I end,
                       ?CACHE_SIZE),
    ConfigFilter = gen_mod:get_opt({ldap_filter, Host}, Opts,
				   fun check_filter/1, <<"">>),
    ConfigUserFilter = gen_mod:get_opt({ldap_ufilter, Host}, Opts,
				       fun check_filter/1, <<"">>),
    ConfigGroupFilter = gen_mod:get_opt({ldap_gfilter, Host}, Opts,
					fun check_filter/1, <<"">>),
    RosterFilter = gen_mod:get_opt({ldap_rfilter, Host}, Opts,
				   fun check_filter/1, <<"">>),
    SubFilter = <<"(&(", UIDAttr/binary, "=",
    	        UIDAttrFormat/binary, ")(", GroupAttr/binary, "=%g))">>,
    UserSubFilter = case ConfigUserFilter of
    		        <<"">> ->
				  eldap_filter:do_sub(SubFilter, [{<<"%g">>, <<"*">>}]);
				  UString -> UString
       	           end,
    GroupSubFilter = case ConfigGroupFilter of
    		          <<"">> ->
				    eldap_filter:do_sub(SubFilter,
							[{<<"%u">>, <<"*">>}]);
				    GString -> GString
		     end,
    Filter = case ConfigFilter of
           <<"">> -> SubFilter;
	          _ ->
		       <<"(&", SubFilter/binary, ConfigFilter/binary, ")">>
		  end,
    UserFilter = case ConfigFilter of
    	          <<"">> -> UserSubFilter;
		  	    _ ->
			             <<"(&", UserSubFilter/binary, ConfigFilter/binary, ")">>
			    end,
    GroupFilter = case ConfigFilter of
    		    <<"">> -> GroupSubFilter;
		    	       _ ->
					<<"(&", GroupSubFilter/binary, ConfigFilter/binary,
								         ")">>
			       end,
%%%%%%%%%%%%%
    GroupBase = gen_mod:get_opt(ldap_group_base, Opts, fun iolist_to_binary/1,
    	      			Cfg#eldap_config.base),
    GroupIsDN = gen_mod:get_opt(ldap_group_is_dn, Opts,
                                fun(on) -> true;
                                   (off) -> false;
                                   (false) -> false;
                                   (true) -> true
                                end, true),
    MemberSelMode = gen_mod:get_opt(ldap_member_selection_mode, Opts,
    		    		fun(memberattr_normal) -> memberattr_normal;
				   (memberattr_dn)     -> memberattr_dn;
				   (group_children)    -> group_children;
				   (Invalid) ->
            ?ERROR_MSG("Invalid ldap_member_selection_mode '~p'. "
                       "Value 'memberattr_normal' will be used instead.",
                       [Invalid])
       end, memberattr_normal),
    SubscribeAll = gen_mod:get_opt(ldap_subscribe_all, Opts,
                                fun(on) -> true;
                                   (off) -> false;
                                   (false) -> false;
                                   (true) -> true
                                end, false),
    % MemberIsDN = (MemberSelMode == member_attr_dn) or (MemberSelMode == group_children),
    ShGFilter = gen_mod:get_opt(ldap_shgfilter, Opts, 
    	      			fun(all) -> all;
				   (none) -> none;
				   (S) -> check_filter(S)
                             end, all),
    ShGAttr = gen_mod:get_opt(ldap_shgattr, Opts,
                              fun iolist_to_binary/1,
			      << GroupAttr/binary >>),
%%%%%%
    #state{host = Host, eldap_id = Eldap_ID,
       servers = Cfg#eldap_config.servers,
          backups = Cfg#eldap_config.backups,
           port = Cfg#eldap_config.port,
	      tls_options = Cfg#eldap_config.tls_options,
	         dn = Cfg#eldap_config.dn,
           password = Cfg#eldap_config.password,
           base = Cfg#eldap_config.base,
           deref_aliases = Cfg#eldap_config.deref_aliases,
	      group_attr = GroupAttr, group_desc = GroupDesc,
	      user_desc = UserDesc, uid = UserUID,
	      uid_format = UIDAttrFormat,
	      uid_format_re = UIDAttrFormatRe, filter = Filter,
	      ufilter = UserFilter, rfilter = RosterFilter,
	      gfilter = GroupFilter, auth_check = AuthCheck,
        group_base = GroupBase,
        member_attr = UIDAttr,
        member_selection_mode = MemberSelMode,
        group_is_dn = GroupIsDN,
        shgfilter = ShGFilter,
        shg_attr = ShGAttr,
        subscribe_all = SubscribeAll,
	      roster_cache_size = RosterCacheSize,
	      roster_cache_validity = RosterCacheValidity}.

check_filter(F) ->
  NewF = iolist_to_binary(F),
  {ok, _} = eldap_filter:parse(NewF),
  NewF.

mod_opt_type(deref_aliases) ->
    fun (never) -> never;
	(searching) -> searching;
	(finding) -> finding;
	(always) -> always
    end;
mod_opt_type(ldap_backups) ->
    fun (L) -> [iolist_to_binary(H) || H <- L] end;
mod_opt_type(ldap_base) -> fun iolist_to_binary/1;
mod_opt_type(ldap_deref_aliases) ->
    fun (never) -> never;
	(searching) -> searching;
	(finding) -> finding;
	(always) -> always
    end;
mod_opt_type(ldap_encrypt) ->
    fun (tls) -> tls;
	(starttls) -> starttls;
	(none) -> none
    end;
mod_opt_type(ldap_password) -> fun iolist_to_binary/1;
mod_opt_type(ldap_port) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(ldap_rootdn) -> fun iolist_to_binary/1;
mod_opt_type(ldap_servers) ->
    fun (L) -> [iolist_to_binary(H) || H <- L] end;
mod_opt_type(ldap_tls_cacertfile) ->
    fun iolist_to_binary/1;
mod_opt_type(ldap_tls_certfile) ->
    fun iolist_to_binary/1;
mod_opt_type(ldap_tls_depth) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(ldap_tls_verify) ->
    fun (hard) -> hard;
	(soft) -> soft;
	(false) -> false
    end;
mod_opt_type(ldap_auth_check) ->
    fun (on) -> true;
	(off) -> false;
	(false) -> false;
	(true) -> true
    end;
mod_opt_type(ldap_filter) -> fun check_filter/1;
mod_opt_type(ldap_gfilter) -> fun check_filter/1;
mod_opt_type(ldap_group_cache_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(ldap_group_cache_validity) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(ldap_groupattr) -> fun iolist_to_binary/1;
mod_opt_type(ldap_groupdesc) -> fun iolist_to_binary/1;
mod_opt_type(ldap_memberattr) -> fun iolist_to_binary/1;
mod_opt_type(ldap_memberattr_format) ->
    fun iolist_to_binary/1;
mod_opt_type(ldap_memberattr_format_re) ->
    fun (S) ->
	    Re = iolist_to_binary(S), {ok, MP} = re:compile(Re), MP
    end;
mod_opt_type(ldap_rfilter) -> fun check_filter/1;
mod_opt_type(ldap_ufilter) -> fun check_filter/1;
mod_opt_type(ldap_user_cache_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(ldap_user_cache_validity) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(ldap_userdesc) -> fun iolist_to_binary/1;
mod_opt_type(ldap_useruid) -> fun iolist_to_binary/1;
mod_opt_type(ldap_group_base) -> fun iolist_to_binary/1;
mod_opt_type(ldap_group_is_dn) -> fun(B) when is_boolean(B) -> B end;
mod_opt_type(ldap_member_selection_mode) ->
    fun(memberattr_normal) -> memberattr_normal;
       (memberattr_dn)     -> memberattr_dn;
       (group_children)    -> group_children
    end;
mod_opt_type(ldap_subscribe_all) -> fun(B) when is_boolean(B) -> B end;
mod_opt_type(ldap_shgfilter) ->
    fun(all) -> all;
       (none) -> none;
       (S) -> check_filter(S)
    end;
mod_opt_type(ldap_shgattr) -> fun iolist_to_binary/1;
mod_opt_type(_) ->
    [ldap_auth_check, ldap_filter, ldap_gfilter,
     ldap_group_cache_size, ldap_group_cache_validity,
     ldap_groupattr, ldap_groupdesc, ldap_memberattr,
     ldap_memberattr_format, ldap_memberattr_format_re,
     ldap_rfilter, ldap_ufilter, ldap_user_cache_size,
     ldap_user_cache_validity, ldap_userdesc, ldap_useruid,
     deref_aliases, ldap_backups, ldap_base,
     ldap_deref_aliases, ldap_encrypt, ldap_password,
     ldap_port, ldap_rootdn, ldap_servers,
     ldap_tls_cacertfile, ldap_tls_certfile, ldap_tls_depth,
     ldap_tls_verify, ldap_group_base, ldap_group_is_dn,
     ldap_member_selection_mode, ldap_subscribe_all,
     ldap_shgfilter, ldap_shgattr].

opt_type(ldap_filter) -> fun check_filter/1;
opt_type(ldap_gfilter) -> fun check_filter/1;
opt_type(ldap_group_cache_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(ldap_group_cache_validity) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(ldap_rfilter) -> fun check_filter/1;
opt_type(ldap_ufilter) -> fun check_filter/1;
opt_type(ldap_user_cache_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(ldap_user_cache_validity) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(_) ->
    [ldap_filter, ldap_gfilter, ldap_group_cache_size,
     ldap_group_cache_validity, ldap_rfilter, ldap_ufilter,
     ldap_user_cache_size, ldap_user_cache_validity].
