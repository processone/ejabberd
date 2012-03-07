%%%----------------------------------------------------------------------
%%% File    : acl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ACL support
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(acl).
-author('alexey@process-one.net').

-export([start/0,
	 to_record/3,
	 add/3,
	 add_list/3,
	 match_rule/3,
	 for_host/1,
	 % for debugging only
	 match_acl/3]).

-include("ejabberd.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% @type aclspec() = all | JID_Exact | JID_Regexp | JID_Glob | Shared_Group
%%     JID_Exact = {user, U} | {user, U, S} | {server, S} | {resource, R}
%%         U = string()
%%         S = string()
%%         R = string()
%%     JID_Regexp = {user_regexp, UR} | {user_regexp, UR, S} | {server_regexp, SR} | {resource_regexp, RR} | {node_regexp, UR, SR}
%%         UR = string()
%%         SR = string()
%%         RR = string()
%%     JID_Glob = {user_glob, UG} | {user_glob, UG, S} | {server_glob, SG} | {resource_glob, RG} | {node_glob, UG, SG}
%%         UG = string()
%%         SG = string()
%%         RG = string()
%%     Shared_Group = {shared_group, G} | {shared_group, G, H}
%%         G = string()
%%         H = string().

%% @type acl() = {acl, ACLName, ACLSpec}
%%     ACLName = atom()
%%     ACLSpec = aclspec().
%% Record in its Ejabberd-configuration-file variant.

%% @type storedacl() = {acl, {ACLName, Host}, ACLSpec}
%%     ACLName = atom()
%%     Host = global | string()
%%     ACLSpec = aclspec().
%% Record in its Mnesia-table-record variant.

-record(acl, {aclname, aclspec}).

%% @spec () -> ok

start() ->
    mnesia:create_table(acl,
			[{disc_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, acl)}]),
    mnesia:add_table_copy(acl, node(), ram_copies),
    ok.

%% @spec (Host, ACLName, ACLSpec) -> storedacl()
%%     Host = global | string()
%%     ACLName = atom()
%%     ACLSpec = aclspec()

to_record(Host, ACLName, ACLSpec) ->
    #acl{aclname = {ACLName, Host}, aclspec = normalize_spec(ACLSpec)}.

%% @spec (Host, ACLName, ACLSpec) -> {atomic, ok} | {aborted, Reason}
%%     Host = global | string()
%%     ACLName = atom()
%%     ACLSpec = all | none | aclspec()
%%     Reason = term()

add(Host, ACLName, ACLSpec) ->
    F = fun() ->
		mnesia:write(#acl{aclname = {ACLName, Host},
				  aclspec = normalize_spec(ACLSpec)})
	end,
    mnesia:transaction(F).

%% @spec (Host, ACLs, Clear) -> ok | false
%%     Host = global | string()
%%     ACLs = [acl()]
%%     Clear = bool()

add_list(Host, ACLs, Clear) ->
    F = fun() ->
		if
		    Clear ->
			Ks = mnesia:select(
			       acl, [{{acl, {'$1', Host}, '$2'}, [], ['$1']}]),
			lists:foreach(fun(K) ->
					      mnesia:delete({acl, {K, Host}})
				      end, Ks);
		    true ->
			ok
		end,
		lists:foreach(fun(ACL) ->
				      case ACL of
					  #acl{aclname = ACLName,
					       aclspec = ACLSpec} ->
					      mnesia:write(
						#acl{aclname = {ACLName, Host},
						     aclspec = normalize_spec(ACLSpec)})
				      end
			      end, ACLs)
	end,
    case mnesia:transaction(F) of
	{atomic, _} ->
	    ok;
	_ ->
	    false
    end.

%% @spec (String) -> Prepd_String
%%     String = string()
%%     Prepd_String = string()

normalize(String) ->
    exmpp_stringprep:nodeprep(String).

%% @spec (ACLSpec) -> Normalized_ACLSpec
%%     ACLSpec = all | none | aclspec()
%%     Normalized_ACLSpec = aclspec()

normalize_spec({A, B}) ->
    {A, normalize(B)};
normalize_spec({A, B, C}) ->
    {A, normalize(B), normalize(C)};
normalize_spec(all) ->
    all;
normalize_spec(none) ->
    none.



%% @spec (Host, Rule, JID) -> Access
%%     Host = global | string()
%%     Rule = all | none | atom()
%%     JID = exmpp_jid:jid()
%%     Access = allow | deny | atom()

match_rule(global, Rule, JID) ->
    case Rule of
	all -> allow;
	none -> deny;
	_ ->
	    case ejabberd_config:get_global_option({access, Rule, global}) of
		undefined ->
		    deny;
		GACLs ->
		    match_acls(GACLs, JID, global)
	    end
    end;

match_rule(Host, Rule, JID) ->
    case Rule of
	all -> allow;
	none -> deny;
	_ ->
	    case ejabberd_config:get_global_option({access, Rule, global}) of
		undefined ->
		    case ejabberd_config:get_global_option({access, Rule, Host}) of
			undefined ->
			    deny;
			ACLs ->
			    match_acls(ACLs, JID, Host)
		    end;
		GACLs ->
		    case ejabberd_config:get_global_option({access, Rule, Host}) of
			undefined ->
			    match_acls(GACLs, JID, Host);
			ACLs ->
			    case lists:reverse(GACLs) of
				[{allow, all} | Rest] ->
				    match_acls(
				      lists:reverse(Rest) ++ ACLs ++
				      [{allow, all}],
				      JID, Host);
				_ ->
				    match_acls(GACLs ++ ACLs, JID, Host)
			    end
		    end
	    end
    end.

%% @spec (ACLs, JID, Host) -> Access
%%     ACLs = [{Access, ACLName}]
%%         Access = deny | atom()
%%         ACLName = atom()
%%     JID = exmpp_jid:jid()
%%     Host = string()

match_acls([], _, _Host) ->
    deny;
match_acls([{Access, ACLName} | ACLs], JID, Host) ->
    case match_acl(ACLName, JID, Host) of
	true ->
	    Access;
	_ ->
	    match_acls(ACLs, JID, Host)
    end.

%% @spec (ACLName, JID, Host) -> bool()
%%     ACLName = all | none | atom()
%%     JID = exmpp_jid:jid()
%%     Host = string()

match_acl(ACLName, JID, Host) ->
    case ACLName of
	all -> true;
	none -> false;
	_ ->
	    User = exmpp_jid:prep_node_as_list(JID),
	    Server = exmpp_jid:prep_domain_as_list(JID),
	    Resource = exmpp_jid:prep_resource_as_list(JID),
	    lists:any(fun(#acl{aclname=Name, aclspec = Spec}) ->
			      case Spec of
				  all ->
				      true;
				  {user, U} ->
				      (U == User)
					  andalso
					    ((Host == Server) orelse
					     ((Host == global) andalso
					      ?IS_MY_HOST(Server)));
				  {user, U, S} ->
				      (U == User) andalso (S == Server);
				  {server, S} ->
				      S == Server;
				  {resource, R} ->
				      R == Resource;
                                  {user_regexp, UR} when is_tuple(Name),
                                                         element(2, Name) =:= global ->
                                      ?IS_MY_HOST(Server)
					  andalso is_regexp_match(User, UR);
				  {user_regexp, UR} ->
				      ((Host == Server) orelse
				       ((Host == global) andalso
					?IS_MY_HOST(Server)))
					  andalso is_regexp_match(User, UR);
				  {shared_group, G} ->
				      mod_shared_roster:is_user_in_group({User, Server}, G, Host);
				  {shared_group, G, H} ->
				      mod_shared_roster:is_user_in_group({User, Server}, G, H);
				  {user_regexp, UR, S} ->
				      (S == Server) andalso
					  is_regexp_match(User, UR);
				  {server_regexp, SR} ->
				      is_regexp_match(Server, SR);
				  {resource_regexp, RR} ->
				      is_regexp_match(Resource, RR);
				  {node_regexp, UR, SR} ->
				      is_regexp_match(Server, SR) andalso
					  is_regexp_match(User, UR);
				  {user_glob, UR} ->
				      ((Host == Server) orelse
				       ((Host == global) andalso
					?IS_MY_HOST(Server)))
					  andalso
					  is_glob_match(User, UR);
				  {user_glob, UR, S} ->
				      (S == Server) andalso
					  is_glob_match(User, UR);
				  {server_glob, SR} ->
				      is_glob_match(Server, SR);
				  {resource_glob, RR} ->
				      is_glob_match(Resource, RR);
				  {node_glob, UR, SR} ->
				      is_glob_match(Server, SR) andalso
					  is_glob_match(User, UR);
				  WrongSpec ->
				      ?ERROR_MSG(
					 "Wrong ACL expression: ~p~n"
					 "Check your config file and reload it with the override_acls option enabled",
					 [WrongSpec]),
				      false
			      end
		      end,
		      ets:lookup(acl, {ACLName, global}) ++
		      ets:lookup(acl, {ACLName, Host}))
    end.

%% @spec (String, RegExp) -> bool()
%%     String = string() | undefined
%%     RegExp = string()

is_regexp_match(undefined, _RegExp) ->
    false;
is_regexp_match(String, RegExp) ->
    try re:run(String, RegExp, [{capture, none}]) of
	nomatch ->
	    false;
	match ->
	    true
    catch
	_:ErrDesc ->
	    ?ERROR_MSG(
	       "Wrong regexp ~p in ACL:~n~p",
	       [RegExp, ErrDesc]),
	    false
    end.

%% @spec (String, Glob) -> bool()
%%     String = string() | undefined
%%     Glob = string()

is_glob_match(String, Glob) ->
    is_regexp_match(String, xmerl_regexp:sh_to_awk(Glob)).


for_host(Host) ->
    mnesia:select(acl,
		ets:fun2ms(fun (#acl{aclname = {_ACLName, H}}) 
		                when H =:= Host ->
				    object()
		end)).
