%%%-------------------------------------------------------------------
%%% File    : mod_shared_roster_ldap.erl
%%% Author  : Realloc <realloc@realloc.spb.ru>
%%%           Marcin Owsiany <marcin@owsiany.pl>
%%%           Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : LDAP shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@process-one.net>
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
%%%-------------------------------------------------------------------
-module(mod_shared_roster_ldap).

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([get_user_roster/2,
	 get_subscription_lists/3,
	 get_jid_info/4,
	 process_item/2,
	 in_subscription/6,
	 out_subscription/4]).

-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include("eldap/eldap.hrl").

-define(CACHE_SIZE, 1000).
-define(USER_CACHE_VALIDITY, 300). %% in seconds
-define(GROUP_CACHE_VALIDITY, 300). %% in seconds
-define(LDAP_SEARCH_TIMEOUT, 5). %% Timeout for LDAP search queries in seconds

-record(state, {host,
		eldap_id,
		servers,
		backups,
		port,
		tls_options,
		dn,
		base,
		password,
		uid,
                deref_aliases,
		group_attr,
		group_desc,
		user_desc,
		user_uid,
		uid_format,
		uid_format_re,
		filter,
		ufilter,
		rfilter,
		gfilter,
		auth_check,
		user_cache_size,
		group_cache_size,
		user_cache_validity,
		group_cache_validity}).

-record(group_info, {desc, members}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {
      Proc, {?MODULE, start_link, [Host, Opts]},
      permanent, 1000, worker, [?MODULE]
     },
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
get_user_roster(Items, {U, S} = US) ->
    SRUsers = get_user_to_groups_map(US, true),
    %% If partially subscribed users are also in shared roster,
    %% show them as totally subscribed:
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
    SRItems = [#roster{usj = {U, S, {U1, S1, ""}},
		       us = US,
		       jid = {U1, S1, ""},
		       name = get_user_name(U1,S1),
		       subscription = both,
		       ask = none,
		       groups = GroupNames} ||
		  {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

%% This function in use to rewrite the roster entries when moving or renaming
%% them in the user contact list.
process_item(RosterItem, _Host) ->
    USFrom = RosterItem#roster.us,
    {User,Server,_Resource} = RosterItem#roster.jid,
    USTo = {User,Server},
    Map = get_user_to_groups_map(USFrom, false),
    case dict:find(USTo, Map) of
        error ->
            RosterItem;
        {ok, []} ->
            RosterItem;
        {ok, GroupNames} when RosterItem#roster.subscription == remove ->
            %% Roster item cannot be removed:
	    %% We simply reset the original groups:
            RosterItem#roster{subscription = both, ask = none,
			      groups=GroupNames};
        _ ->
            RosterItem#roster{subscription = both, ask = none}
    end.

get_subscription_lists({F, T}, User, Server) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers =
	lists:usort(
	  lists:flatmap(
	    fun(Group) ->
		    get_group_users(LServer, Group)
	    end, DisplayedGroups)),
    SRJIDs = [{U1, S1, ""} || {U1, S1} <- SRUsers],
    {lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T)}.

get_jid_info({Subscription, Groups}, User, Server, JID) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},

    %% TODO BADLOP: I don't know what version is correct, A or B:
    %%
    %% A) as binaries
    {U1, S1, _} = jlib:short_prepd_jid(JID),
    US1 = {U1, S1},
    %%
    %% B) as strings
    %% US1 = {exmpp_jid:prep_node_as_list(JID), exmpp_jid:prep_domain_as_list(JID)},

    SRUsers = get_user_to_groups_map(US, false),
    case dict:find(US1, SRUsers) of
	{ok, GroupNames} ->
	    NewGroups = if
			    Groups == [] -> GroupNames;
			    true -> Groups
			end,
	    {both, NewGroups};
	error ->
	    {Subscription, Groups}
    end.

in_subscription(Acc, User, Server, JID, Type, _Reason) ->
    process_subscription(in, User, Server, JID, Type, Acc).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, false).

process_subscription(Direction, User, Server, JID, _Type, Acc) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},

    %% TODO BADLOP: I don't know what version is correct, A or B:
    %%
    %% A) as binaries
    {U1, S1, _} = jlib:short_prepd_jid(JID),
    US1 = {U1, S1},
    %%
    %% B) as strings
    %% US1 = {exmpp_jid:prep_node_as_list(JID), exmpp_jid:prep_domain_as_list(JID)},

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
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    State = parse_options(Host, Opts),
    cache_tab:new(shared_roster_ldap_user,
		  [{max_size, State#state.user_cache_size},
		   {lru, false}, % We don't need LRU algorithm
		   {life_time, State#state.user_cache_validity}]),
    cache_tab:new(shared_roster_ldap_group,
		  [{max_size, State#state.group_cache_size},
		   {lru, false}, % We don't need LRU algorithm
		   {life_time, State#state.group_cache_validity}]),
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
%% For a given user, map all his shared roster contacts to groups they are
%% members of. Skip the user himself if SkipUS is true.
get_user_to_groups_map({_, Server} = US, SkipUS) ->
    DisplayedGroups = get_user_displayed_groups(US),
    lists:foldl(
      fun(Group, Dict1) ->
	      GroupName = get_group_name(Server, Group),
	      lists:foldl(
		fun(Contact, Dict) ->
			if SkipUS, Contact == US ->
				Dict;
			   true ->
				dict:append(Contact, GroupName, Dict)
			end
		end, Dict1, get_group_users(Server, Group))
      end, dict:new(), DisplayedGroups).

%% Pass given FilterParseArgs to eldap_filter:parse, and if successful, run and
%% return the resulting filter, retrieving given AttributesList. Return the
%% result entries. On any error silently return an empty list of results.
%%
%% Eldap server ID and base DN for the query are both retrieved from the State
%% record.
eldap_search(State, FilterParseArgs, AttributesList) ->
    case apply(eldap_filter, parse, FilterParseArgs) of
        {ok, EldapFilter} ->
	    %% Filter parsing succeeded
            case eldap_pool:search(State#state.eldap_id,
				   [{base, State#state.base},
				    {filter, EldapFilter},
				    {timeout, ?LDAP_SEARCH_TIMEOUT},
                                    {deref_aliases, State#state.deref_aliases},
				    {attributes, AttributesList}]) of
                #eldap_search_result{entries = Es} ->
		    %% A result with entries. Return their list.
		    Es;
                _ ->
		    %% Something else. Pretend we got no results.
		    []
            end;
        _ ->
	    %% Filter parsing failed. Pretend we got no results.
            []
    end.

get_user_displayed_groups({_User, Host}) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    GroupAttr = State#state.group_attr,
    Entries = eldap_search(State, [State#state.rfilter], [GroupAttr]),
    Reply = lists:flatmap(
	      fun(#eldap_entry{attributes = Attrs}) ->
		      case Attrs of
			  [{GroupAttr, ValuesList}] ->
			      ValuesList;
			  _ ->
			      []
		      end
	      end, Entries),
    lists:usort(Reply).

get_group_users(Host, Group) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case cache_tab:dirty_lookup(
	   shared_roster_ldap_group,
	   {Group, Host},
	   fun() -> search_group_info(State, Group) end) of
	{ok, #group_info{members = Members}} when Members /= undefined ->
	    Members;
	_ ->
	    []
    end.

get_group_name(Host, Group) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case cache_tab:dirty_lookup(
	   shared_roster_ldap_group,
	   {Group, Host},
	   fun() -> search_group_info(State, Group) end) of
	{ok, #group_info{desc = GroupName}} when GroupName /= undefined ->
            GroupName;
        _ ->
            Group
    end.

get_user_name(User, Host) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case cache_tab:dirty_lookup(
	   shared_roster_ldap_user,
	   {User, Host},
	   fun() -> search_user_name(State, User) end) of
	{ok, UserName} ->
	    UserName;
	error ->
	    User
    end.

search_group_info(State, Group) ->
    Extractor =
	case State#state.uid_format_re of
	    "" ->
		fun(UID) ->
			catch eldap_utils:get_user_part(UID, State#state.uid_format)
		end;
	    _  ->
		fun(UID) ->
			catch get_user_part_re(UID, State#state.uid_format_re)
		end
	end,
    AuthChecker = case State#state.auth_check of
		      true -> fun ejabberd_auth:is_user_exists/2;
		      _ -> fun(_U, _S) -> true end
		  end,
    Host = State#state.host,
    case eldap_search(
	   State,
	   [eldap_filter:do_sub(State#state.gfilter, [{"%g", Group}])],
	   [State#state.group_attr, State#state.group_desc, State#state.uid]) of
	[] ->
	    error;
	LDAPEntries ->
	    {GroupDesc, MembersLists} =
		lists:foldl(
		  fun(#eldap_entry{attributes=Attrs}, {DescAcc, JIDsAcc}) ->
			  case {eldap_utils:get_ldap_attr(State#state.group_attr, Attrs),
				eldap_utils:get_ldap_attr(State#state.group_desc, Attrs),
				lists:keysearch(State#state.uid, 1, Attrs)} of
			      {ID, Desc, {value, {GroupMemberAttr, Members}}}
			      when ID /= "", GroupMemberAttr == State#state.uid ->
				  JIDs = lists:foldl(
					   fun({ok, UID}, L) ->
						   PUID = exmpp_stringprep:nodeprep(UID),
						   case AuthChecker(PUID, Host) of
						       true -> [{PUID, Host} | L];
						       _ -> L
						   end;
					      (_, L) ->
						   L
					   end, [], lists:map(Extractor, Members)),
				  {Desc, [JIDs|JIDsAcc]};
			      _ ->
				  {DescAcc, JIDsAcc}
			  end
		  end, {Group, []}, LDAPEntries),
	    {ok, #group_info{desc = GroupDesc,
			     members = lists:usort(lists:flatten(MembersLists))}}
    end.

search_user_name(State, User) ->
    case eldap_search(
	   State,
	   [eldap_filter:do_sub(State#state.ufilter, [{"%u", User}])],
	   [State#state.user_desc, State#state.user_uid]) of
	[#eldap_entry{attributes=Attrs}|_] ->
	    case {eldap_utils:get_ldap_attr(State#state.user_uid, Attrs),
		  eldap_utils:get_ldap_attr(State#state.user_desc, Attrs)} of
		{UID, Desc} when UID /= "" ->
		    %% By returning "" get_ldap_attr means "not found"
		    {ok, Desc};
		_ ->
		    error
	    end;
	[] ->
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

parse_options(Host, Opts) ->
    Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, ?MODULE)),
    LDAPServers = case gen_mod:get_opt(ldap_servers, Opts, undefined) of
		      undefined ->
			  ejabberd_config:get_local_option({ldap_servers, Host});
		      S -> S
		  end,
    LDAPBackups = case gen_mod:get_opt(ldap_backups, Opts, undefined) of
                      undefined ->
                          ejabberd_config:get_local_option({ldap_servers, Host});
                      Backups -> Backups
                  end,
    LDAPEncrypt = case gen_mod:get_opt(ldap_encrypt, Opts, undefined) of
		      undefined ->
			  ejabberd_config:get_local_option({ldap_encrypt, Host});
		      E -> E
		  end,
    LDAPTLSVerify = case gen_mod:get_opt(ldap_tls_verify, Opts, undefined) of
			undefined ->
			    ejabberd_config:get_local_option({ldap_tls_verify, Host});
			Verify -> Verify
		    end,
    LDAPTLSCAFile = case gen_mod:get_opt(ldap_tls_cacertfile, Opts, undefined) of
                        undefined ->
                            ejabberd_config:get_local_option({ldap_tls_cacertfile, Host});
                        CAFile -> CAFile
                    end,
    LDAPTLSDepth = case gen_mod:get_opt(ldap_tls_depth, Opts, undefined) of
                       undefined ->
                           ejabberd_config:get_local_option({ldap_tls_depth, Host});
                       Depth ->
                           Depth
                   end,
    LDAPPort = case gen_mod:get_opt(ldap_port, Opts, undefined) of
		   undefined ->
		       case ejabberd_config:get_local_option({ldap_port, Host}) of
			   undefined -> case LDAPEncrypt of
					    tls -> ?LDAPS_PORT;
					    starttls -> ?LDAP_PORT;
					    _ -> ?LDAP_PORT
					end;
			   P -> P
		       end;
		   P -> P
	       end,
    LDAPBase = case gen_mod:get_opt(ldap_base, Opts, undefined) of
		   undefined ->
		       ejabberd_config:get_local_option({ldap_base, Host});
		   B -> B
	       end,
    GroupAttr = case gen_mod:get_opt(ldap_groupattr, Opts, undefined) of
		    undefined -> "cn";
		    GA -> GA
		end,
    GroupDesc = case gen_mod:get_opt(ldap_groupdesc, Opts, undefined) of
		    undefined -> GroupAttr;
		    GD -> GD
		end,
    UserDesc = case gen_mod:get_opt(ldap_userdesc, Opts, undefined) of
		   undefined -> "cn";
		   UD -> UD
	       end,
    UserUID = case gen_mod:get_opt(ldap_useruid, Opts, undefined) of
		  undefined -> "cn";
		  UU -> UU
	      end,
    UIDAttr = case gen_mod:get_opt(ldap_memberattr, Opts, undefined) of
		  undefined -> "memberUid";
		  UA -> UA
	      end,
    UIDAttrFormat = case gen_mod:get_opt(ldap_memberattr_format, Opts, undefined) of
			undefined -> "%u";
			UAF -> UAF
		    end,
    UIDAttrFormatRe =
	case gen_mod:get_opt(ldap_memberattr_format_re, Opts, undefined) of
	    undefined -> "";
	    UAFre -> case catch re:compile(UAFre) of
			 {ok, MP} ->
			     MP;
			 _ ->
			     ?ERROR_MSG("Invalid ldap_memberattr_format_re '~s' "
					"or no RE support in this erlang version. "
					"ldap_memberattr_format '~s' will be used "
					"instead.", [UAFre, UIDAttrFormat]),
			     ""
		     end
	end,
    AuthCheck = case gen_mod:get_opt(ldap_auth_check, Opts, undefined) of
		    undefined -> true;
		    on -> true;
		    AC -> AC
		end,
    RootDN = case gen_mod:get_opt(ldap_rootdn, Opts, undefined) of
		 undefined ->
		     case ejabberd_config:get_local_option({ldap_rootdn, Host}) of
			 undefined -> "";
			 RDN -> RDN
		     end;
		 RDN -> RDN
	     end,
    Password = case gen_mod:get_opt(ldap_password, Opts, undefined) of
		   undefined ->
		       case ejabberd_config:get_local_option({ldap_password, Host}) of
			   undefined -> "";
			   Pass -> Pass
		       end;
		   Pass -> Pass
	       end,
    UserCacheValidity =
	case gen_mod:get_opt(ldap_user_cache_validity, Opts, undefined) of
	    undefined ->
		case ejabberd_config:get_local_option({ldap_user_cache_validity, Host}) of
		    undefined -> ?USER_CACHE_VALIDITY;
		    UVSeconds -> UVSeconds
		end;
	    UVSeconds -> UVSeconds
	end,
    GroupCacheValidity =
	case gen_mod:get_opt(ldap_group_cache_validity, Opts, undefined) of
	    undefined ->
		case ejabberd_config:get_local_option({ldap_group_cache_validity, Host}) of
		    undefined -> ?GROUP_CACHE_VALIDITY;
		    GVSeconds -> GVSeconds
		end;
	    GVSeconds -> GVSeconds
	end,
    UserCacheSize =
	case gen_mod:get_opt(ldap_user_cache_size, Opts, undefined) of
	    undefined ->
		case ejabberd_config:get_local_option({ldap_user_cache_size, Host}) of
		    undefined -> ?CACHE_SIZE;
		    USSeconds -> USSeconds
		end;
	    USSeconds -> USSeconds
	end,
    GroupCacheSize =
	case gen_mod:get_opt(ldap_group_cache_size, Opts, undefined) of
	    undefined ->
		case ejabberd_config:get_local_option({ldap_group_cache_size, Host}) of
		    undefined -> ?CACHE_SIZE;
		    GSSeconds -> GSSeconds
		end;
	    GSSeconds -> GSSeconds
	end,
    ConfigFilter = case gen_mod:get_opt(ldap_filter, Opts, undefined) of
		       undefined ->
			   ejabberd_config:get_local_option({ldap_filter, Host});
		       F ->
			   F
		   end,
    ConfigUserFilter = case gen_mod:get_opt(ldap_ufilter, Opts, undefined) of
			   undefined ->
                               ejabberd_config:get_local_option({ldap_ufilter, Host});
			   UF -> UF
		       end,

    ConfigGroupFilter = case gen_mod:get_opt(ldap_gfilter, Opts, undefined) of
			    undefined ->
			        ejabberd_config:get_local_option({ldap_gfilter, Host});
                            GF -> GF
                        end,

    RosterFilter = case gen_mod:get_opt(ldap_rfilter, Opts, undefined) of
		       undefined ->
			   ejabberd_config:get_local_option({ldap_rfilter, Host});
		       RF ->
			   RF
		   end,
    lists:foreach(fun eldap_utils:check_filter/1, 
                  [ConfigFilter, ConfigUserFilter,
                   ConfigGroupFilter, RosterFilter]),
    SubFilter = "(&("++UIDAttr++"="++UIDAttrFormat++")("++GroupAttr++"=%g))",
    UserSubFilter = case ConfigUserFilter of
                        undefined -> eldap_filter:do_sub(SubFilter, [{"%g", "*"}]);
                        "" -> eldap_filter:do_sub(SubFilter, [{"%g", "*"}]);
                        UString -> UString
                    end,
    GroupSubFilter = case ConfigGroupFilter of
			 undefined -> eldap_filter:do_sub(SubFilter, [{"%u", "*"}]);
		         "" -> eldap_filter:do_sub(SubFilter, [{"%u", "*"}]);
		         GString -> GString
                     end,
    Filter = case ConfigFilter of
		 undefined -> SubFilter;
		 "" -> SubFilter;
		 _ -> "(&" ++ SubFilter ++ ConfigFilter ++ ")"
	     end,
    UserFilter = case ConfigFilter of
		     undefined -> UserSubFilter;
		     "" -> UserSubFilter;
		     _ -> "(&" ++ UserSubFilter ++ ConfigFilter ++ ")"
		 end,
    GroupFilter = case ConfigFilter of
		      undefined -> GroupSubFilter;
		      "" -> GroupSubFilter;
		      _ -> "(&" ++ GroupSubFilter ++ ConfigFilter ++ ")"
		  end,
    DerefAliases = case gen_mod:get_opt(deref_aliases, Opts, undefined) of
                       undefined ->
                           case ejabberd_config:get_local_option(
                                  {deref_aliases, Host}) of
                               undefined -> never;
                               D -> D
                           end;
                       D -> D
                   end,
    #state{host = Host,
	   eldap_id = Eldap_ID,
	   servers = LDAPServers,
	   backups = LDAPBackups,
	   port = LDAPPort,
	   tls_options = [{encrypt, LDAPEncrypt},
			  {tls_verify, LDAPTLSVerify},
                          {tls_cacertfile, LDAPTLSCAFile},
                          {tls_depth, LDAPTLSDepth}],
	   dn = RootDN,
	   base = LDAPBase,
	   password = Password,
	   uid = UIDAttr,
           deref_aliases = DerefAliases,
	   group_attr = GroupAttr,
	   group_desc = GroupDesc,
	   user_desc = UserDesc,
	   user_uid = UserUID,
	   uid_format = UIDAttrFormat,
	   uid_format_re = UIDAttrFormatRe,
	   filter = Filter,
	   ufilter = UserFilter,
	   rfilter = RosterFilter,
	   gfilter = GroupFilter,
	   auth_check = AuthCheck,
	   user_cache_size = UserCacheSize,
	   user_cache_validity = UserCacheValidity,
	   group_cache_size = GroupCacheSize,
	   group_cache_validity = GroupCacheValidity}.
