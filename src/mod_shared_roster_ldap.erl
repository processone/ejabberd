%%%-------------------------------------------------------------------
%%% File    : mod_shared_roster_ldap.erl
%%% Author  : Realloc <realloc@realloc.spb.ru>
%%%           Marcin Owsiany <marcin@owsiany.pl>
%%%           Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : LDAP shared roster management
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
%%%-------------------------------------------------------------------
-module(mod_shared_roster_ldap).

-behaviour(ejabberd_config).

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-export([get_user_roster/2,
	 get_jid_info/4, process_item/2, in_subscription/6,
	 out_subscription/4, mod_opt_type/1, opt_type/1, depends/2,
	 transform_module_options/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_roster.hrl").
-include("eldap.hrl").

-define(USER_CACHE, shared_roster_ldap_user_cache).
-define(GROUP_CACHE, shared_roster_ldap_group_cache).
-define(LDAP_SEARCH_TIMEOUT, 5).    %% Timeout for LDAP search queries in seconds
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
         user_uid = <<"">>                            :: binary(),
         uid_format = <<"">>                          :: binary(),
	 uid_format_re = <<"">>                       :: binary(),
         filter = <<"">>                              :: binary(),
         ufilter = <<"">>                             :: binary(),
         rfilter = <<"">>                             :: binary(),
         gfilter = <<"">>                             :: binary(),
	 auth_check = true                            :: boolean()}).

-record(group_info, {desc, members}).

%%====================================================================
%% API
%%====================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, _OldOpts) ->
    case init_cache(Host, NewOpts) of
	true ->
	    ets_cache:setopts(?USER_CACHE, cache_opts(Host, NewOpts)),
	    ets_cache:setopts(?GROUP_CACHE, cache_opts(Host, NewOpts));
	false ->
	    ok
    end,
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {set_state, parse_options(Host, NewOpts)}).

depends(_Host, _Opts) ->
    [{mod_roster, hard}].

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
-spec get_user_roster([#roster{}], {binary(), binary()}) -> [#roster{}].
get_user_roster(Items, {U, S} = US) ->
    SRUsers = get_user_to_groups_map(US, true),
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
		       name = get_user_name(U1, S1), subscription = both,
		       ask = none, groups = GroupNames}
	       || {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

%% This function in use to rewrite the roster entries when moving or renaming
%% them in the user contact list.
-spec process_item(#roster{}, binary()) -> #roster{}.
process_item(RosterItem, _Host) ->
    USFrom = RosterItem#roster.us,
    {User, Server, _Resource} = RosterItem#roster.jid,
    USTo = {User, Server},
    Map = get_user_to_groups_map(USFrom, false),
    case dict:find(USTo, Map) of
      error -> RosterItem;
      {ok, []} -> RosterItem;
      {ok, GroupNames}
	  when RosterItem#roster.subscription == remove ->
	  RosterItem#roster{subscription = both, ask = none,
			    groups = GroupNames};
      _ -> RosterItem#roster{subscription = both, ask = none}
    end.

-spec get_jid_info({subscription(), [binary()]}, binary(), binary(), jid())
      -> {subscription(), [binary()]}.
get_jid_info({Subscription, Groups}, User, Server,
	     JID) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jid:tolower(JID),
    US1 = {U1, S1},
    SRUsers = get_user_to_groups_map(US, false),
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

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    State = parse_options(Host, Opts),
    init_cache(Host, Opts),
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
    eldap_pool:start_link(State#state.eldap_id,
			  State#state.servers, State#state.backups,
			  State#state.port, State#state.dn,
			  State#state.password, State#state.tls_options),
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({set_state, NewState}, _State) ->
    {noreply, NewState};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(roster_get, Host, ?MODULE,
			  get_user_roster, 70),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, in_subscription, 30),
    ejabberd_hooks:delete(roster_out_subscription, Host,
			  ?MODULE, out_subscription, 30),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 70),
    ejabberd_hooks:delete(roster_process_item, Host,
			  ?MODULE, process_item, 50).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_user_to_groups_map({_, Server} = US, SkipUS) ->
    DisplayedGroups = get_user_displayed_groups(US),
    lists:foldl(fun (Group, Dict1) ->
			GroupName = get_group_name(Server, Group),
			lists:foldl(fun (Contact, Dict) ->
					    if SkipUS, Contact == US -> Dict;
					       true ->
						   dict:append(Contact,
							       GroupName, Dict)
					    end
				    end,
				    Dict1, get_group_users(Server, Group))
		end,
		dict:new(), DisplayedGroups).

eldap_search(State, FilterParseArgs, AttributesList) ->
    case apply(eldap_filter, parse, FilterParseArgs) of
      {ok, EldapFilter} ->
	  case eldap_pool:search(State#state.eldap_id,
				 [{base, State#state.base},
				  {filter, EldapFilter},
				  {timeout, ?LDAP_SEARCH_TIMEOUT},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, AttributesList}])
	      of
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

get_user_displayed_groups({User, Host}) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    GroupAttr = State#state.group_attr,
    Entries = eldap_search(State,
			   [eldap_filter:do_sub(State#state.rfilter,
						[{<<"%u">>, User}])],
			   [GroupAttr]),
    Reply = lists:flatmap(fun (#eldap_entry{attributes =
						Attrs}) ->
				  case Attrs of
				    [{GroupAttr, ValuesList}] -> ValuesList;
				    _ -> []
				  end
			  end,
			  Entries),
    lists:usort(Reply).

get_group_users(Host, Group) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case ets_cache:lookup(?GROUP_CACHE,
			  {Group, Host},
			  fun () -> search_group_info(State, Group) end) of
        {ok, #group_info{members = Members}}
	  when Members /= undefined ->
            Members;
        _ -> []
    end.

get_group_name(Host, Group) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case ets_cache:lookup(?GROUP_CACHE,
			  {Group, Host},
			  fun () -> search_group_info(State, Group) end)
	of
      {ok, #group_info{desc = GroupName}}
	  when GroupName /= undefined ->
	  GroupName;
      _ -> Group
    end.

get_user_name(User, Host) ->
    {ok, State} = eldap_utils:get_state(Host, ?MODULE),
    case ets_cache:lookup(?USER_CACHE,
			  {User, Host},
			  fun () -> search_user_name(State, User) end)
	of
      {ok, UserName} -> UserName;
      error -> User
    end.

search_group_info(State, Group) ->
    Extractor = case State#state.uid_format_re of
		  <<"">> ->
		      fun (UID) ->
			      catch eldap_utils:get_user_part(UID,
							      State#state.uid_format)
		      end;
		  _ ->
		      fun (UID) ->
			      catch get_user_part_re(UID,
						     State#state.uid_format_re)
		      end
		end,
    AuthChecker = case State#state.auth_check of
		    true -> fun ejabberd_auth:user_exists/2;
		    _ -> fun (_U, _S) -> true end
		  end,
    case eldap_search(State,
		      [eldap_filter:do_sub(State#state.gfilter,
					   [{<<"%g">>, Group}])],
		      [State#state.group_attr, State#state.group_desc,
		       State#state.uid])
	of
        [] ->
            error;
      LDAPEntries ->
          {GroupDesc, MembersLists} = lists:foldl(fun(Entry, Acc) ->
                                                           extract_members(State, Extractor, AuthChecker, Entry, Acc)
                                                   end,
                                                   {Group, []}, LDAPEntries),
	  {ok, #group_info{desc = GroupDesc, members = lists:usort(lists:flatten(MembersLists))}}
    end.

extract_members(State, Extractor, AuthChecker, #eldap_entry{attributes = Attrs}, {DescAcc, JIDsAcc}) ->
    Host = State#state.host,
    case {eldap_utils:get_ldap_attr(State#state.group_attr, Attrs),
          eldap_utils:get_ldap_attr(State#state.group_desc, Attrs),
          lists:keysearch(State#state.uid, 1, Attrs)} of
        {ID, Desc, {value, {GroupMemberAttr, Members}}} when ID /= <<"">>,
                                                             GroupMemberAttr == State#state.uid ->
            JIDs = lists:foldl(fun({ok, UID}, L) ->
                                       PUID = jid:nodeprep(UID),
                                       case PUID of
                                           error ->
                                               L;
                                           _ ->
                                               case AuthChecker(PUID, Host) of
                                                   true ->
                                                       [{PUID, Host} | L];
                                                   _ ->
                                                       L
                                               end
                                       end;
                                  (_, L) -> L
                               end,
                               [],
                               lists:map(Extractor, Members)),
            {Desc, [JIDs | JIDsAcc]};
        _ ->
            {DescAcc, JIDsAcc}
    end.

search_user_name(State, User) ->
    case eldap_search(State,
		      [eldap_filter:do_sub(State#state.ufilter,
					   [{<<"%u">>, User}])],
		      [State#state.user_desc, State#state.user_uid])
	of
      [#eldap_entry{attributes = Attrs} | _] ->
	  case {eldap_utils:get_ldap_attr(State#state.user_uid,
					  Attrs),
		eldap_utils:get_ldap_attr(State#state.user_desc, Attrs)}
	      of
	    {UID, Desc} when UID /= <<"">> -> {ok, Desc};
	    _ -> error
	  end;
      [] -> error
    end.

%% Getting User ID part by regex pattern
get_user_part_re(String, Pattern) ->
    case catch re:run(String, Pattern) of
      {match, Captured} ->
	  {First, Len} = lists:nth(2, Captured),
	  Result = str:sub_string(String, First + 1, First + Len),
	  {ok, Result};
      _ -> {error, badmatch}
    end.

parse_options(Host, Opts) ->
    Eldap_ID = misc:atom_to_binary(gen_mod:get_module_proc(Host, ?MODULE)),
    Cfg = eldap_utils:get_config(Host, Opts),
    GroupAttr = gen_mod:get_opt(ldap_groupattr, Opts, <<"cn">>),
    GroupDesc = gen_mod:get_opt(ldap_groupdesc, Opts, GroupAttr),
    UserDesc = gen_mod:get_opt(ldap_userdesc, Opts, <<"cn">>),
    UserUID = gen_mod:get_opt(ldap_useruid, Opts, <<"cn">>),
    UIDAttr = gen_mod:get_opt(ldap_memberattr, Opts, <<"memberUid">>),
    UIDAttrFormat = gen_mod:get_opt(ldap_memberattr_format, Opts, <<"%u">>),
    UIDAttrFormatRe = gen_mod:get_opt(ldap_memberattr_format_re, Opts, <<"">>),
    AuthCheck = gen_mod:get_opt(ldap_auth_check, Opts, true),
    ConfigFilter = gen_mod:get_opt({ldap_filter, Host}, Opts, <<"">>),
    ConfigUserFilter = gen_mod:get_opt({ldap_ufilter, Host}, Opts, <<"">>),
    ConfigGroupFilter = gen_mod:get_opt({ldap_gfilter, Host}, Opts, <<"">>),
    RosterFilter = gen_mod:get_opt({ldap_rfilter, Host}, Opts, <<"">>),
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
    #state{host = Host, eldap_id = Eldap_ID,
	   servers = Cfg#eldap_config.servers,
	   backups = Cfg#eldap_config.backups,
           port = Cfg#eldap_config.port,
	   tls_options = Cfg#eldap_config.tls_options,
	   dn = Cfg#eldap_config.dn,
           password = Cfg#eldap_config.password,
           base = Cfg#eldap_config.base,
           deref_aliases = Cfg#eldap_config.deref_aliases,
	   uid = UIDAttr,
	   group_attr = GroupAttr, group_desc = GroupDesc,
	   user_desc = UserDesc, user_uid = UserUID,
	   uid_format = UIDAttrFormat,
	   uid_format_re = UIDAttrFormatRe, filter = Filter,
	   ufilter = UserFilter, rfilter = RosterFilter,
	   gfilter = GroupFilter, auth_check = AuthCheck}.

init_cache(Host, Opts) ->
    UseCache = use_cache(Host, Opts),
    case UseCache of
	true ->
	    CacheOpts = cache_opts(Host, Opts),
	    ets_cache:new(?USER_CACHE, CacheOpts),
	    ets_cache:new(?GROUP_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?USER_CACHE),
	    ets_cache:delete(?GROUP_CACHE)
    end,
    UseCache.

use_cache(Host, Opts) ->
    gen_mod:get_opt(use_cache, Opts, ejabberd_config:use_cache(Host)).

cache_opts(Host, Opts) ->
    MaxSize = gen_mod:get_opt(cache_size, Opts,
			      ejabberd_config:cache_size(Host)),
    CacheMissed = gen_mod:get_opt(cache_missed, Opts,
				  ejabberd_config:cache_missed(Host)),
    LifeTime = case gen_mod:get_opt(cache_life_time, Opts,
				    ejabberd_config:cache_life_time(Host)) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

transform_module_options(Opts) ->
    lists:map(
      fun({ldap_group_cache_size, I}) ->
	      ?WARNING_MSG("Option 'ldap_group_cache_size' is deprecated, "
			   "use 'cache_size' instead", []),
	      {cache_size, I};
	 ({ldap_user_cache_size, I}) ->
	      ?WARNING_MSG("Option 'ldap_user_cache_size' is deprecated, "
			   "use 'cache_size' instead", []),
	      {cache_size, I};
	 ({ldap_group_cache_validity, Secs}) ->
	      ?WARNING_MSG("Option 'ldap_group_cache_validity' is deprecated, "
			   "use 'cache_life_time' instead", []),
	      {cache_life_time, Secs};
	 ({ldap_user_cache_validity, Secs}) ->
	      ?WARNING_MSG("Option 'ldap_user_cache_validity' is deprecated, "
			   "use 'cache_life_time' instead", []),
	      {cache_life_time, Secs};
	 (Opt) ->
	      Opt
      end, Opts).

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
    fun misc:try_read_file/1;
mod_opt_type(ldap_tls_certfile) ->
    fun ejabberd_pkix:try_certfile/1;
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
mod_opt_type(ldap_filter) -> fun eldap_utils:check_filter/1;
mod_opt_type(ldap_gfilter) -> fun eldap_utils:check_filter/1;
mod_opt_type(O) when O == cache_size;
		     O == cache_life_time ->
    fun (I) when is_integer(I), I > 0 -> I;
	(infinity) -> infinity
    end;
mod_opt_type(O) when O == use_cache; O == cache_missed ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(ldap_groupattr) -> fun iolist_to_binary/1;
mod_opt_type(ldap_groupdesc) -> fun iolist_to_binary/1;
mod_opt_type(ldap_memberattr) -> fun iolist_to_binary/1;
mod_opt_type(ldap_memberattr_format) ->
    fun iolist_to_binary/1;
mod_opt_type(ldap_memberattr_format_re) ->
    fun (S) ->
	    Re = iolist_to_binary(S), {ok, MP} = re:compile(Re), MP
    end;
mod_opt_type(ldap_rfilter) -> fun eldap_utils:check_filter/1;
mod_opt_type(ldap_ufilter) -> fun eldap_utils:check_filter/1;
mod_opt_type(ldap_userdesc) -> fun iolist_to_binary/1;
mod_opt_type(ldap_useruid) -> fun iolist_to_binary/1;
mod_opt_type(_) ->
    [ldap_auth_check, ldap_filter, ldap_gfilter,
     ldap_groupattr, ldap_groupdesc, ldap_memberattr,
     ldap_memberattr_format, ldap_memberattr_format_re,
     ldap_rfilter, ldap_ufilter, ldap_userdesc, ldap_useruid,
     deref_aliases, ldap_backups, ldap_base,
     ldap_deref_aliases, ldap_encrypt, ldap_password,
     ldap_port, ldap_rootdn, ldap_servers,
     ldap_tls_cacertfile, ldap_tls_certfile, ldap_tls_depth,
     ldap_tls_verify, use_cache, cache_missed, cache_size, cache_life_time].

opt_type(ldap_gfilter) -> fun eldap_utils:check_filter/1;
opt_type(ldap_rfilter) -> fun eldap_utils:check_filter/1;
opt_type(ldap_ufilter) -> fun eldap_utils:check_filter/1;
opt_type(_) ->
    [ldap_gfilter, ldap_rfilter, ldap_ufilter].
