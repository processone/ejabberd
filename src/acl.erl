%%%----------------------------------------------------------------------
%%% File    : acl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ACL support
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(acl).

-author('alexey@process-one.net').

-export([start/0, to_record/3, add/3, add_list/3,
         add_local/3, add_list_local/3, load_from_config/0,
	 match_rule/3, match_acl/3, transform_options/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(acl, {aclname, aclspec}).
-record(access, {name       :: aclname(),
                 rules = [] :: [access_rule()]}).

-type regexp() :: binary().
-type glob() :: binary().
-type access_name() :: atom().
-type access_rule() :: {atom(), any()}.
-type host() :: binary().
-type aclname() :: {atom(), binary() | global}.
-type aclspec() :: all | none |
                   {user, {binary(), host()} | binary()} |
                   {server, binary()} |
                   {resource, binary()} |
                   {user_regexp, {regexp(), host()} | regexp()} |
                   {shared_group, {binary(), host()} | binary()} |
                   {user_regexp, {regexp(), host()} | regexp()} |
                   {server_regexp, regexp()} |
                   {resource_regexp, regexp()} |
                   {node_regexp, {regexp(), regexp()}} |
                   {user_glob, {glob(), host()} | glob()} |
                   {server_glob, glob()} |
                   {resource_glob, glob()} |
                   {ip, {inet:ip_address(), integer()}} |
                   {node_glob, {glob(), glob()}}.

-type acl() :: #acl{aclname :: aclname(),
                    aclspec :: aclspec()}.

-export_type([acl/0]).

start() ->
    case catch mnesia:table_info(acl, storage_type) of
        disc_copies ->
            mnesia:delete_table(acl);
        _ ->
            ok
    end,
    mnesia:create_table(acl,
			[{ram_copies, [node()]}, {type, bag},
                         {local_content, true},
			 {attributes, record_info(fields, acl)}]),
    mnesia:create_table(access,
                        [{ram_copies, [node()]},
                         {local_content, true},
			 {attributes, record_info(fields, access)}]),
    mnesia:add_table_copy(acl, node(), ram_copies),
    mnesia:add_table_copy(access, node(), ram_copies),
    load_from_config(),
    ok.

-spec to_record(binary(), atom(), aclspec()) -> acl().

to_record(Host, ACLName, ACLSpec) ->
    #acl{aclname = {ACLName, Host},
	 aclspec = normalize_spec(ACLSpec)}.

-spec add(binary(), aclname(), aclspec()) -> ok | {error, any()}.

add(Host, ACLName, ACLSpec) ->
    {ResL, BadNodes} = rpc:multicall(mnesia:system_info(running_db_nodes),
                                     ?MODULE, add_local,
                                     [Host, ACLName, ACLSpec]),
    case lists:keyfind(aborted, 1, ResL) of
        false when BadNodes == [] ->
            ok;
        false ->
            {error, {failed_nodes, BadNodes}};
        Err ->
            {error, Err}
    end.

add_local(Host, ACLName, ACLSpec) ->
    F = fun () ->
		mnesia:write(#acl{aclname = {ACLName, Host},
				  aclspec = normalize_spec(ACLSpec)})
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        Err ->
            Err
    end.

-spec add_list(binary(), [acl()], boolean()) -> ok | {error, any()}.

add_list(Host, ACLs, Clear) ->
    {ResL, BadNodes} = rpc:multicall(mnesia:system_info(running_db_nodes),
                                     ?MODULE, add_list_local,
                                     [Host, ACLs, Clear]),
    case lists:keyfind(aborted, 1, ResL) of
        false when BadNodes == [] ->
            ok;
        false ->
            {error, {failed_nodes, BadNodes}};
        Err ->
            {error, Err}
    end.

add_list_local(Host, ACLs, Clear) ->
    F = fun () ->
		if Clear ->
		       Ks = mnesia:select(acl,
					  [{{acl, {'$1', Host}, '$2'}, [],
					    ['$1']}]),
		       lists:foreach(fun (K) -> mnesia:delete({acl, {K, Host}})
				     end,
				     Ks);
		   true -> ok
		end,
		lists:foreach(fun (ACL) ->
				      case ACL of
					#acl{aclname = ACLName,
					     aclspec = ACLSpec} ->
					    mnesia:write(#acl{aclname =
								  {ACLName,
								   Host},
							      aclspec =
								  normalize_spec(ACLSpec)})
				      end
			      end,
			      ACLs)
	end,
    mnesia:transaction(F).

-spec add_access(binary() | global,
                 access_name(), [access_rule()]) ->  ok | {error, any()}.

add_access(Host, Access, Rules) ->
    case mnesia:transaction(
           fun() ->
                   mnesia:write(
                     #access{name = {Access, Host},
                             rules = Rules})
           end) of
        {atomic, ok} ->
            ok;
        Err ->
            {error, Err}
    end.

-spec load_from_config() -> ok.

load_from_config() ->
    Hosts = [global|?MYHOSTS],
    lists:foreach(
      fun(Host) ->
              ACLs = ejabberd_config:get_option(
                       {acl, Host}, fun(V) -> V end, []),
              AccessRules = ejabberd_config:get_option(
                              {access, Host}, fun(V) -> V end, []),
              lists:foreach(
                fun({ACLName, SpecList}) ->
                        lists:foreach(
                          fun({ACLType, ACLSpecs}) when is_list(ACLSpecs) ->
                                  lists:foreach(
                                    fun(ACLSpec) ->
                                            add(Host, ACLName,
                                                {ACLType, ACLSpec})
                                    end, lists:flatten(ACLSpecs));
                             ({ACLType, ACLSpecs}) ->
                                  add(Host, ACLName, {ACLType, ACLSpecs})
                          end, lists:flatten(SpecList))
                end, ACLs),
              lists:foreach(
                fun({Access, Rules}) ->
                        add_access(Host, Access, Rules)
                end, AccessRules)
      end, Hosts).

b(S) ->
    iolist_to_binary(S).

nodeprep(S) ->
    jlib:nodeprep(b(S)).

nameprep(S) ->
    jlib:nameprep(b(S)).

resourceprep(S) ->
    jlib:resourceprep(b(S)).

normalize_spec(Spec) ->
    case Spec of
        all -> all;
        none -> none;
        {user, {U, S}} -> {user, {nodeprep(U), nameprep(S)}};
        {user, U} -> {user, nodeprep(U)};
        {shared_group, {G, H}} -> {shared_group, {b(G), nameprep(H)}};
        {shared_group, G} -> {shared_group, b(G)};
        {user_regexp, {UR, S}} -> {user_regexp, {b(UR), nameprep(S)}};
        {user_regexp, UR} -> {user_regexp, b(UR)};
        {node_regexp, {UR, SR}} -> {node_regexp, {b(UR), b(SR)}};
        {user_glob, {UR, S}} -> {user_glob, {b(UR), nameprep(S)}};
        {user_glob, UR} -> {user_glob, b(UR)};
        {node_glob, {UR, SR}} -> {node_glob, {b(UR), b(SR)}};
        {server, S} -> {server, nameprep(S)};
        {resource, R} -> {resource, resourceprep(R)};
        {server_regexp, SR} -> {server_regexp, b(SR)};
        {server_glob, S} -> {server_glob, b(S)};
        {resource_glob, R} -> {resource_glob, b(R)};
        {ip, {Net, Mask}} ->
            {ip, {Net, Mask}};
        {ip, S} ->
            case parse_ip_netmask(b(S)) of
                {ok, Net, Mask} ->
                    {ip, {Net, Mask}};
                error ->
                    ?INFO_MSG("Invalid network address: ~p", [S]),
                    none
            end
    end.

-spec match_rule(global | binary(), access_name(),
                 jid() | ljid() | inet:ip_address()) -> any().

match_rule(_Host, all, _JID) ->
    allow;
match_rule(_Host, none, _JID) ->
    deny;
match_rule(Host, Access, JID) ->
    GAccess = ets:lookup(access, {Access, global}),
    LAccess = if Host /= global ->
                      ets:lookup(access, {Access, Host});
                 true ->
                      []
              end,
    case GAccess ++ LAccess of
        [] ->
            deny;
        AccessList ->
            Rules = lists:flatmap(
                      fun(#access{rules = Rs}) ->
                              Rs
                      end, AccessList),
            match_acls(Rules, JID, Host)
    end.

match_acls([], _, _Host) -> deny;
match_acls([{ACL, Access} | ACLs], JID, Host) ->
    case match_acl(ACL, JID, Host) of
      true -> Access;
      _ -> match_acls(ACLs, JID, Host)
    end.

-spec match_acl(atom(),
                jid() | ljid() | inet:ip_address(),
                binary()) -> boolean().

match_acl(all, _JID, _Host) ->
    true;
match_acl(none, _JID, _Host) ->
    false;
match_acl(ACL, IP, Host) when tuple_size(IP) == 4;
                              tuple_size(IP) == 8 ->
    lists:any(
      fun(#acl{aclspec = {ip, {Net, Mask}}}) ->
              is_ip_match(IP, Net, Mask);
         (_) ->
              false
      end,
      ets:lookup(acl, {ACL, Host}) ++
          ets:lookup(acl, {ACL, global}));
match_acl(ACL, JID, Host) ->
    {User, Server, Resource} = jlib:jid_tolower(JID),
    lists:any(
      fun(#acl{aclspec = Spec}) ->
              case Spec of
                  all -> true;
                  {user, {U, S}} -> U == User andalso S == Server;
                  {user, U} ->
                      U == User andalso
                          lists:member(Server, ?MYHOSTS);
                  {server, S} -> S == Server;
                  {resource, R} -> R == Resource;
                  {shared_group, {G, H}} ->
                      Mod = loaded_shared_roster_module(H),
                      Mod:is_user_in_group({User, Server}, G, H);
                  {shared_group, G} ->
                      Mod = loaded_shared_roster_module(Host),
                      Mod:is_user_in_group({User, Server}, G, Host);
                  {user_regexp, {UR, S}} ->
                      S == Server andalso is_regexp_match(User, UR);
                  {user_regexp, UR} ->
                      lists:member(Server, ?MYHOSTS)
                          andalso is_regexp_match(User, UR);
                  {server_regexp, SR} ->
                      is_regexp_match(Server, SR);
                  {resource_regexp, RR} ->
                      is_regexp_match(Resource, RR);
                  {node_regexp, {UR, SR}} ->
                      is_regexp_match(Server, SR) andalso
                          is_regexp_match(User, UR);
                  {user_glob, {UR, S}} ->
                      S == Server andalso is_glob_match(User, UR);
                  {user_glob, UR} ->
                      lists:member(Server, ?MYHOSTS)
                          andalso is_glob_match(User, UR);
                  {server_glob, SR} -> is_glob_match(Server, SR);
                  {resource_glob, RR} ->
                      is_glob_match(Resource, RR);
                  {node_glob, {UR, SR}} ->
                      is_glob_match(Server, SR) andalso
                          is_glob_match(User, UR);
                  WrongSpec ->
                      ?ERROR_MSG("Wrong ACL expression: ~p~nCheck your "
                                 "config file and reload it with the override_a"
                                 "cls option enabled",
                                 [WrongSpec]),
                      false
              end
      end,
      ets:lookup(acl, {ACL, Host}) ++
          ets:lookup(acl, {ACL, global})).

is_regexp_match(String, RegExp) ->
    case ejabberd_regexp:run(String, RegExp) of
      nomatch -> false;
      match -> true;
      {error, ErrDesc} ->
	  ?ERROR_MSG("Wrong regexp ~p in ACL: ~p",
		     [RegExp, ErrDesc]),
	  false
    end.

is_glob_match(String, Glob) ->
    is_regexp_match(String,
		    ejabberd_regexp:sh_to_awk(Glob)).

is_ip_match({_, _, _, _} = IP, {_, _, _, _} = Net, Mask) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot (1 bsl (32 - Mask) - 1),
    IPInt band M =:= NetInt band M;
is_ip_match({_, _, _, _, _, _, _, _} = IP,
            {_, _, _, _, _, _, _, _} = Net, Mask) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot (1 bsl (128 - Mask) - 1),
    IPInt band M =:= NetInt band M;
is_ip_match(_, _, _) ->
    false.

ip_to_integer({IP1, IP2, IP3, IP4}) ->
    IP1 bsl 8 bor IP2 bsl 8 bor IP3 bsl 8 bor IP4;
ip_to_integer({IP1, IP2, IP3, IP4, IP5, IP6, IP7,
	       IP8}) ->
    IP1 bsl 16 bor IP2 bsl 16 bor IP3 bsl 16 bor IP4 bsl 16
      bor IP5
      bsl 16
      bor IP6
      bsl 16
      bor IP7
      bsl 16
      bor IP8.

loaded_shared_roster_module(Host) ->
    case gen_mod:is_loaded(Host, mod_shared_roster_ldap) of
      true -> mod_shared_roster_ldap;
      false -> mod_shared_roster
    end.

parse_ip_netmask(S) ->
    case str:tokens(S, <<"/">>) of
      [IPStr] ->
	  case inet_parse:address(binary_to_list(IPStr)) of
	    {ok, {_, _, _, _} = IP} -> {ok, IP, 32};
	    {ok, {_, _, _, _, _, _, _, _} = IP} -> {ok, IP, 128};
	    _ -> error
	  end;
      [IPStr, MaskStr] ->
	  case catch jlib:binary_to_integer(MaskStr) of
	    Mask when is_integer(Mask), Mask >= 0 ->
		case inet_parse:address(binary_to_list(IPStr)) of
		  {ok, {_, _, _, _} = IP} when Mask =< 32 ->
		      {ok, IP, Mask};
		  {ok, {_, _, _, _, _, _, _, _} = IP} when Mask =< 128 ->
		      {ok, IP, Mask};
		  _ -> error
		end;
	    _ -> error
	  end;
      _ -> error
    end.

transform_options(Opts) ->
    Opts1 = lists:foldl(fun transform_options/2, [], Opts),
    {ACLOpts, Opts2} = lists:mapfoldl(
                         fun({acl, Os}, Acc) ->
                                 {Os, Acc};
                            (O, Acc) ->
                                 {[], [O|Acc]}
                         end, [], Opts1),
    {AccessOpts, Opts3} = lists:mapfoldl(
                            fun({access, Os}, Acc) ->
                                    {Os, Acc};
                               (O, Acc) ->
                                    {[], [O|Acc]}
                            end, [], Opts2),
    ACLOpts1 = ejabberd_config:collect_options(lists:flatten(ACLOpts)),
    AccessOpts1 = case ejabberd_config:collect_options(
                         lists:flatten(AccessOpts)) of
                      [] -> [];
                      L1 -> [{access, L1}]
                  end,
    ACLOpts2 = case lists:map(
                      fun({ACLName, Os}) ->
                              {ACLName, ejabberd_config:collect_options(Os)}
                      end, ACLOpts1) of
                   [] -> [];
                   L2 -> [{acl, L2}]
               end,
    ACLOpts2 ++ AccessOpts1 ++ Opts3.

transform_options({acl, Name, Type}, Opts) ->
    T = case Type of
            all -> all;
            none -> none;
            {user, U} -> {user, [b(U)]};
            {user, U, S} -> {user, [[{b(U), b(S)}]]};
            {shared_group, G} -> {shared_group, [b(G)]};
            {shared_group, G, H} -> {shared_group, [[{b(G), b(H)}]]};
            {user_regexp, UR} -> {user_regexp, [b(UR)]};
            {user_regexp, UR, S} -> {user_regexp, [[{b(UR), b(S)}]]};
            {node_regexp, UR, SR} -> {node_regexp, [[{b(UR), b(SR)}]]};
            {user_glob, UR} -> {user_glob, [b(UR)]};
            {user_glob, UR, S} -> {user_glob, [[{b(UR), b(S)}]]};
            {node_glob, UR, SR} -> {node_glob, [[{b(UR), b(SR)}]]};
            {server, S} -> {server, [b(S)]};
            {resource, R} -> {resource, [b(R)]};
            {server_regexp, SR} -> {server_regexp, [b(SR)]};
            {server_glob, S} -> {server_glob, [b(S)]};
            {ip, S} -> {ip, [b(S)]};
            {resource_glob, R} -> {resource_glob, [b(R)]}
        end,
    [{acl, [{Name, [T]}]}|Opts];
transform_options({access, Name, Rules}, Opts) ->
    NewRules = [{ACL, Action} || {Action, ACL} <- Rules],
    [{access, [{Name, NewRules}]}|Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].
