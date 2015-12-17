%%%----------------------------------------------------------------------
%%% File    : mod_shared_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-export([start/2, stop/1, item_to_xml/1, export/1,
	 import/1, webadmin_menu/3, webadmin_page/3,
	 get_user_roster/2, get_subscription_lists/3,
	 get_jid_info/4, import/3, process_item/2,
	 in_subscription/6, out_subscription/4, user_available/1,
	 unset_presence/4, register_user/2, remove_user/2,
	 list_groups/1, create_group/2, create_group/3,
	 delete_group/2, get_group_opts/2, set_group_opts/3,
	 get_group_users/2, get_group_explicit_users/2,
	 is_user_in_group/3, add_user_to_group/3,
	 remove_user_from_group/3, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_roster.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-record(sr_group, {group_host = {<<"">>, <<"">>} :: {'$1' | binary(), '$2' | binary()},
                   opts = [] :: list() | '_' | '$2'}).

-record(sr_user, {us = {<<"">>, <<"">>} :: {binary(), binary()},
                  group_host = {<<"">>, <<"">>} :: {binary(), binary()}}).

start(Host, Opts) ->
    case gen_mod:db_type(Host, Opts) of
      mnesia ->
	  mnesia:create_table(sr_group,
			      [{disc_copies, [node()]},
			       {attributes, record_info(fields, sr_group)}]),
	  mnesia:create_table(sr_user,
			      [{disc_copies, [node()]}, {type, bag},
			       {attributes, record_info(fields, sr_user)}]),
          update_tables(),
	  mnesia:add_table_index(sr_user, group_host);
      _ -> ok
    end,
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
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
		       ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:add(roster_get_jid_info, Host, ?MODULE,
		       get_jid_info, 70),
    ejabberd_hooks:add(roster_process_item, Host, ?MODULE,
		       process_item, 50),
    ejabberd_hooks:add(user_available_hook, Host, ?MODULE,
		       user_available, 50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE,
		       unset_presence, 50),
    ejabberd_hooks:add(register_user, Host, ?MODULE,
		       register_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE,
		       remove_user, 50),
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
    ejabberd_hooks:delete(roster_get_subscription_lists,
			  Host, ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 70),
    ejabberd_hooks:delete(roster_process_item, Host,
			  ?MODULE, process_item, 50),
    ejabberd_hooks:delete(user_available_hook, Host,
			  ?MODULE, user_available, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
			  ?MODULE, unset_presence, 50),
    ejabberd_hooks:delete(register_user, Host, ?MODULE,
			  register_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user,
			  50).
    %%ejabberd_hooks:delete(remove_user, Host,
    %%    		  ?MODULE, remove_user, 50),

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
							{ok, _GroupNames} ->
							    {Item#roster{subscription
									     =
									     both,
									 ask =
									     none},
							     dict:erase(US1,
									SRUsers1)};
							error ->
							    {Item, SRUsers1}
						      end
					      end,
					      SRUsers, Items),
    ModVcard = get_vcard_module(S),
    SRItems = [#roster{usj = {U, S, {U1, S1, <<"">>}},
		       us = US, jid = {U1, S1, <<"">>},
		       name = get_rosteritem_name(ModVcard, U1, S1),
		       subscription = both, ask = none, groups = GroupNames}
	       || {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

get_vcard_module(Server) ->
    Modules = gen_mod:loaded_modules(Server),
    [M
     || M <- Modules,
	(M == mod_vcard) or (M == mod_vcard_ldap)].

get_rosteritem_name([], _, _) -> <<"">>;
get_rosteritem_name([ModVcard], U, S) ->
    From = jid:make(<<"">>, S, jlib:atom_to_binary(?MODULE)),
    To = jid:make(U, S, <<"">>),
    case lists:member(To#jid.lserver, ?MYHOSTS) of
        true ->
            IQ = {iq, <<"">>, get, <<"vcard-temp">>, <<"">>,
                  #xmlel{name = <<"vCard">>,
                         attrs = [{<<"xmlns">>, <<"vcard-temp">>}],
                         children = []}},
            IQ_Vcard = ModVcard:process_sm_iq(From, To, IQ),
            case catch get_rosteritem_name_vcard(IQ_Vcard#iq.sub_el) of
                {'EXIT', Err} ->
                    ?ERROR_MSG("Error found when trying to get the "
                               "vCard of ~s@~s in ~p:~n ~p",
                               [U, S, ModVcard, Err]),
                    <<"">>;
                NickName ->
                    NickName
            end;
        false ->
            <<"">>
    end.

get_rosteritem_name_vcard([]) -> <<"">>;
get_rosteritem_name_vcard([Vcard]) ->
    case xml:get_path_s(Vcard,
			[{elem, <<"NICKNAME">>}, cdata])
	of
      <<"">> ->
	  xml:get_path_s(Vcard, [{elem, <<"FN">>}, cdata]);
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
					    jid:make(UserFrom, ServerFrom,
							  <<"">>),
					    unsubscribe),
		mod_roster:in_subscription(aaaa, UserFrom, ServerFrom,
					   jid:make(UserTo, ServerTo,
							 <<"">>),
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
    JIDTo = jid:make(UserTo, ServerTo, <<"">>),
    JIDFrom = jid:make(UserFrom, ServerFrom, <<"">>),
    RITo = build_roster_record(UserTo, ServerTo, UserFrom,
			       ServerFrom, UserFrom, []),
    set_item(UserTo, ServerTo, <<"">>, RITo),
    mod_roster:out_subscription(UserFrom, ServerFrom, JIDTo,
				subscribe),
    mod_roster:in_subscription(aaa, UserTo, ServerTo,
			       JIDFrom, subscribe, <<"">>),
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom,
				subscribed),
    mod_roster:in_subscription(aaa, UserFrom, ServerFrom,
			       JIDTo, subscribed, <<"">>),
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom,
				subscribe),
    mod_roster:in_subscription(aaa, UserFrom, ServerFrom,
			       JIDTo, subscribe, <<"">>),
    mod_roster:out_subscription(UserFrom, ServerFrom, JIDTo,
				subscribed),
    mod_roster:in_subscription(aaa, UserTo, ServerTo,
			       JIDFrom, subscribed, <<"">>),
    RIFrom.

set_item(User, Server, Resource, Item) ->
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
		id = <<"push", (randoms:get_string())/binary>>,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs = [{<<"xmlns">>, ?NS_ROSTER}],
			    children = [mod_roster:item_to_xml(Item)]}]},
    ejabberd_router:route(jid:make(User, Server,
					Resource),
			  jid:make(<<"">>, Server, <<"">>),
			  jlib:iq_to_xml(ResIQ)).

get_subscription_lists({F, T}, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = lists:usort(lists:flatmap(fun (Group) ->
						get_group_users(LServer, Group)
					end,
					DisplayedGroups)),
    SRJIDs = [{U1, S1, <<"">>} || {U1, S1} <- SRUsers],
    {lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T)}.

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

in_subscription(Acc, User, Server, JID, Type,
		_Reason) ->
    process_subscription(in, User, Server, JID, Type, Acc).

out_subscription(UserFrom, ServerFrom, JIDTo,
		 unsubscribed) ->
    #jid{luser = UserTo, lserver = ServerTo} = JIDTo,
    JIDFrom = jid:make(UserFrom, ServerFrom, <<"">>),
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom,
				unsubscribe),
    mod_roster:in_subscription(aaaa, UserFrom, ServerFrom,
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
    list_groups(Host, gen_mod:db_type(Host, ?MODULE)).

list_groups(Host, mnesia) ->
    mnesia:dirty_select(sr_group,
			[{#sr_group{group_host = {'$1', '$2'}, _ = '_'},
			  [{'==', '$2', Host}], ['$1']}]);
list_groups(Host, riak) ->
    case ejabberd_riak:get_keys_by_index(sr_group, <<"host">>, Host) of
        {ok, Gs} ->
            [G || {G, _} <- Gs];
        _ ->
            []
    end;
list_groups(Host, odbc) ->
    case ejabberd_odbc:sql_query(Host,
				 [<<"select name from sr_group;">>])
	of
      {selected, [<<"name">>], Rs} -> [G || [G] <- Rs];
      _ -> []
    end.

groups_with_opts(Host) ->
    groups_with_opts(Host, gen_mod:db_type(Host, ?MODULE)).

groups_with_opts(Host, mnesia) ->
    Gs = mnesia:dirty_select(sr_group,
			     [{#sr_group{group_host = {'$1', Host}, opts = '$2',
					 _ = '_'},
			       [], [['$1', '$2']]}]),
    lists:map(fun ([G, O]) -> {G, O} end, Gs);
groups_with_opts(Host, riak) ->
    case ejabberd_riak:get_by_index(sr_group, sr_group_schema(),
				    <<"host">>, Host) of
        {ok, Rs} ->
            [{G, O} || #sr_group{group_host = {G, _}, opts = O} <- Rs];
        _ ->
            []
    end;
groups_with_opts(Host, odbc) ->
    case ejabberd_odbc:sql_query(Host,
				 [<<"select name, opts from sr_group;">>])
	of
      {selected, [<<"name">>, <<"opts">>], Rs} ->
	  [{G, opts_to_binary(ejabberd_odbc:decode_term(Opts))}
	   || [G, Opts] <- Rs];
      _ -> []
    end.

create_group(Host, Group) ->
    create_group(Host, Group, []).

create_group(Host, Group, Opts) ->
    create_group(Host, Group, Opts,
		 gen_mod:db_type(Host, ?MODULE)).

create_group(Host, Group, Opts, mnesia) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun () -> mnesia:write(R) end,
    mnesia:transaction(F);
create_group(Host, Group, Opts, riak) ->
    {atomic, ejabberd_riak:put(#sr_group{group_host = {Group, Host},
                                         opts = Opts},
			       sr_group_schema(),
                               [{'2i', [{<<"host">>, Host}]}])};
create_group(Host, Group, Opts, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    SOpts = ejabberd_odbc:encode_term(Opts),
    F = fun () ->
		odbc_queries:update_t(<<"sr_group">>,
				      [<<"name">>, <<"opts">>], [SGroup, SOpts],
				      [<<"name='">>, SGroup, <<"'">>])
	end,
    ejabberd_odbc:sql_transaction(Host, F).

delete_group(Host, Group) ->
    delete_group(Host, Group,
		 gen_mod:db_type(Host, ?MODULE)).

delete_group(Host, Group, mnesia) ->
    GroupHost = {Group, Host},
    F = fun () ->
		mnesia:delete({sr_group, GroupHost}),
		Users = mnesia:index_read(sr_user, GroupHost,
					  #sr_user.group_host),
		lists:foreach(fun (UserEntry) ->
				      mnesia:delete_object(UserEntry)
			      end,
			      Users)
	end,
    mnesia:transaction(F);
delete_group(Host, Group, riak) ->
    try
        ok = ejabberd_riak:delete(sr_group, {Group, Host}),
        ok = ejabberd_riak:delete_by_index(sr_user, <<"group_host">>,
                                           {Group, Host}),
        {atomic, ok}
    catch _:{badmatch, Err} ->
            {atomic, Err}
    end;
delete_group(Host, Group, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    F = fun () ->
		ejabberd_odbc:sql_query_t([<<"delete from sr_group where name='">>,
					   SGroup, <<"';">>]),
		ejabberd_odbc:sql_query_t([<<"delete from sr_user where grp='">>,
					   SGroup, <<"';">>])
	end,
    case ejabberd_odbc:sql_transaction(Host, F) of
        {atomic,{updated,_}} -> {atomic, ok};
        Res -> Res
    end.

get_group_opts(Host, Group) ->
    get_group_opts(Host, Group,
		   gen_mod:db_type(Host, ?MODULE)).

get_group_opts(Host, Group, mnesia) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
      [#sr_group{opts = Opts}] -> Opts;
      _ -> error
    end;
get_group_opts(Host, Group, riak) ->
    case ejabberd_riak:get(sr_group, sr_group_schema(), {Group, Host}) of
        {ok, #sr_group{opts = Opts}} -> Opts;
        _ -> error
    end;
get_group_opts(Host, Group, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    case catch ejabberd_odbc:sql_query(Host,
				       [<<"select opts from sr_group where name='">>,
					SGroup, <<"';">>])
	of
      {selected, [<<"opts">>], [[SOpts]]} ->
	  opts_to_binary(ejabberd_odbc:decode_term(SOpts));
      _ -> error
    end.

set_group_opts(Host, Group, Opts) ->
    set_group_opts(Host, Group, Opts,
		   gen_mod:db_type(Host, ?MODULE)).

set_group_opts(Host, Group, Opts, mnesia) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun () -> mnesia:write(R) end,
    mnesia:transaction(F);
set_group_opts(Host, Group, Opts, riak) ->
    {atomic, ejabberd_riak:put(#sr_group{group_host = {Group, Host},
                                         opts = Opts},
			       sr_group_schema(),
                               [{'2i', [{<<"host">>, Host}]}])};
set_group_opts(Host, Group, Opts, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    SOpts = ejabberd_odbc:encode_term(Opts),
    F = fun () ->
		odbc_queries:update_t(<<"sr_group">>,
				      [<<"name">>, <<"opts">>], [SGroup, SOpts],
				      [<<"name='">>, SGroup, <<"'">>])
	end,
    ejabberd_odbc:sql_transaction(Host, F).

get_user_groups(US) ->
    Host = element(2, US),
    DBType = gen_mod:db_type(Host, ?MODULE),
    get_user_groups(US, Host, DBType) ++
      get_special_users_groups(Host).

get_user_groups(US, Host, mnesia) ->
    case catch mnesia:dirty_read(sr_user, US) of
      Rs when is_list(Rs) ->
	  [Group
	   || #sr_user{group_host = {Group, H}} <- Rs, H == Host];
      _ -> []
    end;
get_user_groups(US, Host, riak) ->
    case ejabberd_riak:get_by_index(sr_user, sr_user_schema(), <<"us">>, US) of
        {ok, Rs} ->
            [Group || #sr_user{group_host = {Group, H}} <- Rs, H == Host];
        _ ->
            []
    end;
get_user_groups(US, Host, odbc) ->
    SJID = make_jid_s(US),
    case catch ejabberd_odbc:sql_query(Host,
				       [<<"select grp from sr_user where jid='">>,
					SJID, <<"';">>])
	of
      {selected, [<<"grp">>], Rs} -> [G || [G] <- Rs];
      _ -> []
    end.

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
    get_group_explicit_users(Host, Group,
			     gen_mod:db_type(Host, ?MODULE)).

get_group_explicit_users(Host, Group, mnesia) ->
    Read = (catch mnesia:dirty_index_read(sr_user,
					  {Group, Host}, #sr_user.group_host)),
    case Read of
      Rs when is_list(Rs) -> [R#sr_user.us || R <- Rs];
      _ -> []
    end;
get_group_explicit_users(Host, Group, riak) ->
    case ejabberd_riak:get_by_index(sr_user, sr_user_schema(),
				    <<"group_host">>, {Group, Host}) of
        {ok, Rs} ->
            [R#sr_user.us || R <- Rs];
        _ ->
            []
    end;
get_group_explicit_users(Host, Group, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    case catch ejabberd_odbc:sql_query(Host,
				       [<<"select jid from sr_user where grp='">>,
					SGroup, <<"';">>])
	of
      {selected, [<<"jid">>], Rs} ->
	  lists:map(fun ([JID]) ->
			    {U, S, _} =
				jid:tolower(jid:from_string(JID)),
			    {U, S}
		    end,
		    Rs);
      _ -> []
    end.

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
    Groups = get_user_displayed_groups(LUser, LServer,
				       GroupsOpts,
				       gen_mod:db_type(LServer, ?MODULE)),
    displayed_groups(GroupsOpts, Groups).

get_user_displayed_groups(LUser, LServer, GroupsOpts,
			  mnesia) ->
    case catch mnesia:dirty_read(sr_user, {LUser, LServer})
	of
      Rs when is_list(Rs) ->
	  [{Group, proplists:get_value(Group, GroupsOpts, [])}
	   || #sr_user{group_host = {Group, H}} <- Rs,
	      H == LServer];
      _ -> []
    end;
get_user_displayed_groups(LUser, LServer, GroupsOpts,
                          riak) ->
    case ejabberd_riak:get_by_index(sr_user, sr_user_schema(),
                                    <<"us">>, {LUser, LServer}) of
        {ok, Rs} ->
            [{Group, proplists:get_value(Group, GroupsOpts, [])}
             || #sr_user{group_host = {Group, _}} <- Rs];
        _ ->
            []
    end;
get_user_displayed_groups(LUser, LServer, GroupsOpts,
			  odbc) ->
    SJID = make_jid_s(LUser, LServer),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select grp from sr_user where jid='">>,
					SJID, <<"';">>])
	of
      {selected, [<<"grp">>], Rs} ->
	  [{Group, proplists:get_value(Group, GroupsOpts, [])}
	   || [Group] <- Rs];
      _ -> []
    end.

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
    is_user_in_group(US, Group, Host,
		     gen_mod:db_type(Host, ?MODULE)).

is_user_in_group(US, Group, Host, mnesia) ->
    case catch mnesia:dirty_match_object(#sr_user{us = US,
						  group_host = {Group, Host}})
	of
      [] -> lists:member(US, get_group_users(Host, Group));
      _ -> true
    end;
is_user_in_group(US, Group, Host, riak) ->
    case ejabberd_riak:get_by_index(sr_user, sr_user_schema(), <<"us">>, US) of
        {ok, Rs} ->
            case lists:any(
                   fun(#sr_user{group_host = {G, H}}) ->
                           (Group == G) and (Host == H)
                   end, Rs) of
                false ->
                    lists:member(US, get_group_users(Host, Group));
                true ->
                    true
            end;
        _Err ->
            false
    end;
is_user_in_group(US, Group, Host, odbc) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_odbc:escape(Group),
    case catch ejabberd_odbc:sql_query(Host,
				       [<<"select * from sr_user where jid='">>,
					SJID, <<"' and grp='">>, SGroup,
					<<"';">>])
	of
      {selected, _, []} ->
	  lists:member(US, get_group_users(Host, Group));
      _ -> true
    end.

%% @spec (Host::string(), {User::string(), Server::string()}, Group::string()) -> {atomic, ok}
add_user_to_group(Host, US, Group) ->
    {LUser, LServer} = US,
    case ejabberd_regexp:run(LUser, <<"^@.+@\$">>) of
      match ->
	  GroupOpts = (?MODULE):get_group_opts(Host, Group),
	  MoreGroupOpts = case LUser of
			    <<"@all@">> -> [{all_users, true}];
			    <<"@online@">> -> [{online_users, true}];
			    _ -> []
			  end,
	  (?MODULE):set_group_opts(Host, Group,
				   GroupOpts ++ MoreGroupOpts);
      nomatch ->
	  DisplayedToGroups = displayed_to_groups(Group, Host),
	  DisplayedGroups = get_displayed_groups(Group, LServer),
	  push_user_to_displayed(LUser, LServer, Group, Host, both, DisplayedToGroups),
	  push_displayed_to_user(LUser, LServer, Host, both, DisplayedGroups),
	  broadcast_user_to_displayed(LUser, LServer, Host, both, DisplayedToGroups),
	  broadcast_displayed_to_user(LUser, LServer, Host, both, DisplayedGroups),
	  add_user_to_group(Host, US, Group, gen_mod:db_type(Host, ?MODULE))
    end.

add_user_to_group(Host, US, Group, mnesia) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun () -> mnesia:write(R) end,
    mnesia:transaction(F);
add_user_to_group(Host, US, Group, riak) ->
    {atomic, ejabberd_riak:put(
               #sr_user{us = US, group_host = {Group, Host}},
	       sr_user_schema(),
               [{i, {US, {Group, Host}}},
                {'2i', [{<<"us">>, US},
                        {<<"group_host">>, {Group, Host}}]}])};
add_user_to_group(Host, US, Group, odbc) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_odbc:escape(Group),
    F = fun () ->
		odbc_queries:update_t(<<"sr_user">>,
				      [<<"jid">>, <<"grp">>], [SJID, SGroup],
				      [<<"jid='">>, SJID, <<"' and grp='">>,
				       SGroup, <<"'">>])
	end,
    ejabberd_odbc:sql_transaction(Host, F).

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
	  GroupOpts = (?MODULE):get_group_opts(Host, Group),
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
	  (?MODULE):set_group_opts(Host, Group, NewGroupOpts);
      nomatch ->
	  Result = remove_user_from_group(Host, US, Group,
					  gen_mod:db_type(Host, ?MODULE)),
	  DisplayedToGroups = displayed_to_groups(Group, Host),
	  DisplayedGroups = get_displayed_groups(Group, LServer),
	  push_user_to_displayed(LUser, LServer, Group, Host, remove, DisplayedToGroups),
	  push_displayed_to_user(LUser, LServer, Host, remove, DisplayedGroups),
	  Result
    end.

remove_user_from_group(Host, US, Group, mnesia) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun () -> mnesia:delete_object(R) end,
    mnesia:transaction(F);
remove_user_from_group(Host, US, Group, riak) ->
    {atomic, ejabberd_riak:delete(sr_group, {US, {Group, Host}})};
remove_user_from_group(Host, US, Group, odbc) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_odbc:escape(Group),
    F = fun () ->
		ejabberd_odbc:sql_query_t([<<"delete from sr_user where jid='">>,
					   SJID, <<"' and grp='">>, SGroup,
					   <<"';">>]),
		ok
	end,
    ejabberd_odbc:sql_transaction(Host, F).

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

register_user(User, Server) ->
    Groups = get_user_groups({User, Server}),
    [push_user_to_displayed(User, Server, Group, Server,
			    both, displayed_to_groups(Group, Server))
     || Group <- Groups].

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
    Stanza = jlib:iq_to_xml(#iq{type = set,
				xmlns = ?NS_ROSTER,
				id = <<"push", (randoms:get_string())/binary>>,
				sub_el =
				    [#xmlel{name = <<"query">>,
					    attrs = [{<<"xmlns">>, ?NS_ROSTER}],
					    children = [item_to_xml(Item)]}]}),
    lists:foreach(fun (Resource) ->
			  JID = jid:make(User, Server, Resource),
			  ejabberd_router:route(jid:remove_resource(JID), JID, Stanza)
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

item_to_xml(Item) ->
    Attrs1 = [{<<"jid">>,
	       jid:to_string(Item#roster.jid)}],
    Attrs2 = case Item#roster.name of
	       <<"">> -> Attrs1;
	       Name -> [{<<"name">>, Name} | Attrs1]
	     end,
    Attrs3 = case Item#roster.subscription of
	       none -> [{<<"subscription">>, <<"none">>} | Attrs2];
	       from -> [{<<"subscription">>, <<"from">>} | Attrs2];
	       to -> [{<<"subscription">>, <<"to">>} | Attrs2];
	       both -> [{<<"subscription">>, <<"both">>} | Attrs2];
	       remove -> [{<<"subscription">>, <<"remove">>} | Attrs2]
	     end,
    Attrs4 = case ask_to_pending(Item#roster.ask) of
	       out -> [{<<"ask">>, <<"subscribe">>} | Attrs3];
	       both -> [{<<"ask">>, <<"subscribe">>} | Attrs3];
	       _ -> Attrs3
	     end,
    SubEls1 = lists:map(fun (G) ->
				#xmlel{name = <<"group">>, attrs = [],
				       children = [{xmlcdata, G}]}
			end,
			Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    #xmlel{name = <<"item">>, attrs = Attrs4,
	   children = SubEls}.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.

user_available(New) ->
    LUser = New#jid.luser,
    LServer = New#jid.lserver,
    Resources = ejabberd_sm:get_user_resources(LUser,
					       LServer),
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
    end.

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
    SRGroups = (?MODULE):list_groups(Host),
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
	   <<"modsharedroster">>, <<"mod_shared_roster">>))
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
	  (?MODULE):create_group(Host, Group), ok;
      _ -> error
    end.

list_sr_groups_parse_delete(Host, Query) ->
    SRGroups = (?MODULE):list_groups(Host),
    lists:foreach(fun (Group) ->
			  case lists:member({<<"selected">>, Group}, Query) of
			    true -> (?MODULE):delete_group(Host, Group);
			    _ -> ok
			  end
		  end,
		  SRGroups),
    ok.

shared_roster_group(Host, Group, Query, Lang) ->
    Res = shared_roster_group_parse_query(Host, Group,
					  Query),
    GroupOpts = (?MODULE):get_group_opts(Host, Group),
    Name = get_opt(GroupOpts, name, <<"">>),
    Description = get_opt(GroupOpts, description, <<"">>),
    AllUsers = get_opt(GroupOpts, all_users, false),
    OnlineUsers = get_opt(GroupOpts, online_users, false),
    DisplayedGroups = get_opt(GroupOpts, displayed_groups,
			      []),
    Members = (?MODULE):get_group_explicit_users(Host,
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
					     jlib:integer_to_binary(lists:max([3,
                                                                               DescNL])),
					     <<"20">>, Description)])]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, <<"Members:">>),
			      ?XE(<<"td">>,
				  [?TEXTAREA(<<"members">>,
					     jlib:integer_to_binary(lists:max([3,
                                                                               byte_size(FMembers)])),
					     <<"20">>, FMembers)])]),
			 ?XE(<<"tr">>,
			     [?XCT(<<"td">>, <<"Displayed Groups:">>),
			      ?XE(<<"td">>,
				  [?TEXTAREA(<<"dispgroups">>,
					     jlib:integer_to_binary(lists:max([3,											        length(FDisplayedGroups)])),
					     <<"20">>,
					     list_to_binary(FDisplayedGroups))])])])])),
    (?H1GL((?T(<<"Shared Roster Groups">>)),
	   <<"modsharedroster">>, <<"mod_shared_roster">>))
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
	  OldMembers = (?MODULE):get_group_explicit_users(Host,
							  Group),
	  SJIDs = str:tokens(SMembers, <<", \r\n">>),
	  NewMembers = lists:foldl(fun (_SJID, error) -> error;
				       (SJID, USs) ->
					   case SJID of
					     <<"@all@">> -> USs;
					     <<"@online@">> -> USs;
					     _ ->
						 case jid:from_string(SJID)
						     of
						   JID
						       when is_record(JID,
								      jid) ->
						       [{JID#jid.luser,
							 JID#jid.lserver}
							| USs];
						   error -> error
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
	  (?MODULE):set_group_opts(Host, Group,
				   NameOpt ++
				     DispGroupsOpt ++
				       DescriptionOpt ++
					 AllUsersOpt ++ OnlineUsersOpt),
	  if NewMembers == error -> error;
	     true ->
		 AddedMembers = NewMembers -- OldMembers,
		 RemovedMembers = OldMembers -- NewMembers,
		 lists:foreach(fun (US) ->
				       (?MODULE):remove_user_from_group(Host,
									US,
									Group)
			       end,
			       RemovedMembers),
		 lists:foreach(fun (US) ->
				       (?MODULE):add_user_to_group(Host, US,
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
    jid:to_string({User, Server, <<"">>}).

split_grouphost(Host, Group) ->
    case str:tokens(Group, <<"@">>) of
      [GroupName, HostName] -> {HostName, GroupName};
      [_] -> {Host, Group}
    end.

broadcast_subscription(User, Server, ContactJid, Subscription) ->
    ejabberd_sm:route(
		      jid:make(<<"">>, Server, <<"">>),
		      jid:make(User, Server, <<"">>),
                      {broadcast, {item, ContactJid,
				   Subscription}}).

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

make_jid_s(U, S) ->
    ejabberd_odbc:escape(jid:to_string(jid:tolower(jid:make(U,
									   S,
									   <<"">>)))).

make_jid_s({U, S}) -> make_jid_s(U, S).

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

sr_group_schema() ->
    {record_info(fields, sr_group), #sr_group{}}.

sr_user_schema() ->
    {record_info(fields, sr_user), #sr_user{}}.

update_tables() ->
    update_sr_group_table(),
    update_sr_user_table().

update_sr_group_table() ->
    Fields = record_info(fields, sr_group),
    case mnesia:table_info(sr_group, attributes) of
        Fields ->
            ejabberd_config:convert_table_to_binary(
              sr_group, Fields, set,
              fun(#sr_group{group_host = {G, _}}) -> G end,
              fun(#sr_group{group_host = {G, H},
                            opts = Opts} = R) ->
                      R#sr_group{group_host = {iolist_to_binary(G),
                                               iolist_to_binary(H)},
                                 opts = opts_to_binary(Opts)}
              end);
        _ ->
            ?INFO_MSG("Recreating sr_group table", []),
            mnesia:transform_table(sr_group, ignore, Fields)
    end.

update_sr_user_table() ->
    Fields = record_info(fields, sr_user),
    case mnesia:table_info(sr_user, attributes) of
        Fields ->
            ejabberd_config:convert_table_to_binary(
              sr_user, Fields, bag,
              fun(#sr_user{us = {U, _}}) -> U end,
              fun(#sr_user{us = {U, S}, group_host = {G, H}} = R) ->
                      R#sr_user{us = {iolist_to_binary(U), iolist_to_binary(S)},
                                group_host = {iolist_to_binary(G),
                                              iolist_to_binary(H)}}
              end);
        _ ->
            ?INFO_MSG("Recreating sr_user table", []),
            mnesia:transform_table(sr_user, ignore, Fields)
    end.

export(_Server) ->
    [{sr_group,
      fun(Host, #sr_group{group_host = {Group, LServer}, opts = Opts})
            when LServer == Host ->
              SGroup = ejabberd_odbc:escape(Group),
              SOpts = ejabberd_odbc:encode_term(Opts),
              [[<<"delete from sr_group where name='">>, Group, <<"';">>],
               [<<"insert into sr_group(name, opts) values ('">>,
                SGroup, <<"', '">>, SOpts, <<"');">>]];
         (_Host, _R) ->
              []
      end},
     {sr_user,
      fun(Host, #sr_user{us = {U, S}, group_host = {Group, LServer}})
            when LServer == Host ->
              SGroup = ejabberd_odbc:escape(Group),
              SJID = ejabberd_odbc:escape(
                       jid:to_string(
                         jid:tolower(
                           jid:make(U, S, <<"">>)))),
              [[<<"delete from sr_user where jid='">>, SJID,
                <<"'and grp='">>, Group, <<"';">>],
               [<<"insert into sr_user(jid, grp) values ('">>,
                SJID, <<"', '">>, SGroup, <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select name, opts from sr_group;">>,
      fun([Group, SOpts]) ->
              #sr_group{group_host = {Group, LServer},
                        opts = ejabberd_odbc:decode_term(SOpts)}
      end},
     {<<"select jid, grp from sr_user;">>,
      fun([SJID, Group]) ->
              #jid{luser = U, lserver = S} = jid:from_string(SJID),
              #sr_user{us = {U, S}, group_host = {Group, LServer}}
      end}].

import(_LServer, mnesia, #sr_group{} = G) ->
    mnesia:dirty_write(G);

import(_LServer, mnesia, #sr_user{} = U) ->
    mnesia:dirty_write(U);
import(_LServer, riak, #sr_group{group_host = {_, Host}} = G) ->
    ejabberd_riak:put(G, sr_group_schema(), [{'2i', [{<<"host">>, Host}]}]);
import(_LServer, riak, #sr_user{us = US, group_host = {Group, Host}} = User) ->
    ejabberd_riak:put(User, sr_user_schema(),
                      [{i, {US, {Group, Host}}},
                       {'2i', [{<<"us">>, US},
                               {<<"group_host">>, {Group, Host}}]}]);
import(_, _, _) ->
    pass.

mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(_) -> [db_type].
