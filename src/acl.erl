%%%----------------------------------------------------------------------
%%% File    : acl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ACL support
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(acl).

-behaviour(gen_server).
-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([add_access/3, clear/0]).
-export([start_link/0, add/3, add_list/3, add_local/3, add_list_local/3,
	 load_from_config/0, match_rule/3, any_rules_allowed/3,
	 transform_options/1, opt_type/1, acl_rule_matches/3,
	 acl_rule_verify/1, access_matches/3,
	 transform_access_rules_config/1,
	 parse_ip_netmask/1,
	 access_rules_validator/1, shaper_rules_validator/1,
	 normalize_spec/1, resolve_access/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jid.hrl").

-record(acl, {aclname, aclspec}).
-record(access, {name       :: aclname(),
                 rules = [] :: [access_rule()]}).
-record(state, {}).

-type regexp() :: binary().
-type iprange() :: {inet:ip_address(), integer()} | binary().
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
                   {ip, iprange()} |
                   {node_glob, {glob(), glob()}}.

-type acl() :: #acl{aclname :: aclname(),
                    aclspec :: aclspec()}.

-export_type([acl/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ejabberd_mnesia:create(?MODULE, acl,
			[{ram_copies, [node()]}, {type, bag},
                         {local_content, true},
			 {attributes, record_info(fields, acl)}]),
    ejabberd_mnesia:create(?MODULE, access,
                        [{ram_copies, [node()]},
                         {local_content, true},
			 {attributes, record_info(fields, access)}]),
    mnesia:add_table_copy(acl, node(), ram_copies),
    mnesia:add_table_copy(access, node(), ram_copies),
    ejabberd_hooks:add(config_reloaded, ?MODULE, load_from_config, 20),
    load_from_config(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec add(binary(), aclname(), aclspec()) -> ok | {error, any()}.

add(Host, ACLName, ACLSpec) ->
    {ResL, BadNodes} = ejabberd_cluster:multicall(
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
    {ResL, BadNodes} = ejabberd_cluster:multicall(
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
    Obj = #access{name = {Access, Host}, rules = Rules},
    case mnesia:transaction(fun() -> mnesia:write(Obj) end) of
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
              AccessRulesNew = ejabberd_config:get_option(
				 {access_rules, Host}, fun(V) -> V end, []),
              ShaperRules = ejabberd_config:get_option(
				 {shaper_rules, Host}, fun(V) -> V end, []),
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
			NRules = lists:map(fun({ACL, Type}) ->
					     {Type, [{acl, ACL}]}
				     end, Rules),
                        add_access(Host, Access, NRules ++ [{deny, [all]}])
                end, AccessRules),
              lists:foreach(
                fun({Access, Rules}) ->
                        add_access(Host, Access, Rules)
                end, AccessRulesNew),
              lists:foreach(
                fun({Access, Rules}) ->
                        add_access(Host, Access, Rules)
                end, ShaperRules)
      end, Hosts).

%% Delete all previous set ACLs and Access rules
clear() ->
    mnesia:clear_table(acl),
    mnesia:clear_table(access),
    ok.

b(S) ->
    iolist_to_binary(S).

nodeprep(S) ->
    jid:nodeprep(b(S)).

nameprep(S) ->
    jid:nameprep(b(S)).

resourceprep(S) ->
    jid:resourceprep(b(S)).

split_user_server(Str, NormFunUsr, NormFunSrv) ->
    case binary:split(Str, <<"@">>) of
	[U, S] ->
	    {NormFunUsr(U), NormFunSrv(S)};
	_ ->
	    NormFunUsr(Str)
    end.

normalize_spec(Spec) ->
    case Spec of
        all -> all;
        none -> none;
        {acl, N} -> {acl, N};
        {user, {U, S}} -> {user, {nodeprep(U), nameprep(S)}};
        {user, U} -> {user, split_user_server(U, fun nodeprep/1, fun nameprep/1)};
        {shared_group, {G, H}} -> {shared_group, {b(G), nameprep(H)}};
        {shared_group, G} -> {shared_group, split_user_server(G, fun b/1, fun nameprep/1)};
        {user_regexp, {UR, S}} -> {user_regexp, {b(UR), nameprep(S)}};
        {user_regexp, UR} -> {user_regexp, split_user_server(UR, fun b/1, fun nameprep/1)};
        {node_regexp, {UR, SR}} -> {node_regexp, {b(UR), b(SR)}};
        {user_glob, {UR, S}} -> {user_glob, {b(UR), nameprep(S)}};
        {user_glob, UR} -> {user_glob, split_user_server(UR, fun b/1, fun nameprep/1)};
        {node_glob, {UR, SR}} -> {node_glob, {b(UR), b(SR)}};
        {server, S} -> {server, nameprep(S)};
        {resource, R} -> {resource, resourceprep(R)};
        {server_regexp, SR} -> {server_regexp, b(SR)};
        {resource_regexp, R} -> {resource_regexp, b(R)};
        {server_glob, S} -> {server_glob, b(S)};
        {resource_glob, R} -> {resource_glob, b(R)};
        {ip, {Net, Mask}} -> {ip, {Net, Mask}};
        {ip, S} ->
            case parse_ip_netmask(b(S)) of
                {ok, Net, Mask} ->
                    {ip, {Net, Mask}};
                error ->
                    ?INFO_MSG("Invalid network address: ~p", [S]),
                    none
            end
    end.

-spec any_rules_allowed(global | binary(), [access_name()],
                           jid() | ljid() | inet:ip_address()) -> boolean().

any_rules_allowed(Host, Access, Entity) ->
    lists:any(fun (Rule) ->
                      allow == acl:match_rule(Host, Rule, Entity)
              end,
              Access).

-spec match_rule(global | binary(), access_name(),
                 jid() | ljid() | inet:ip_address()) -> any().

match_rule(Host, Access, IP) when tuple_size(IP) == 4;
    tuple_size(IP) == 8 ->
    access_matches(Access, #{ip => IP}, Host);
match_rule(Host, Access, JID) ->
    access_matches(Access, #{usr => jid:tolower(JID)}, Host).

-spec acl_rule_verify(aclspec()) -> boolean().

acl_rule_verify(all) ->
    true;
acl_rule_verify(none) ->
    true;
acl_rule_verify({ip, {{A,B,C,D}, Mask}})
    when is_integer(A), is_integer(B), is_integer(C), is_integer(D),
    A >= 0, A =< 255, B >= 0, B =< 255, C >= 0, C =< 255, D >= 0, D =< 255,
    is_integer(Mask), Mask >= 0, Mask =< 32 ->
    true;
acl_rule_verify({ip, {{A,B,C,D,E,F,G,H}, Mask}}) when
    is_integer(A), is_integer(B), is_integer(C), is_integer(D),
    is_integer(E), is_integer(F), is_integer(G), is_integer(H),
    A >= 0, A =< 65535, B >= 0, B =< 65535, C >= 0, C =< 65535, D >= 0, D =< 65535,
    E >= 0, E =< 65535, F >= 0, F =< 65535, G >= 0, G =< 65535, H >= 0, H =< 65535,
    is_integer(Mask), Mask >= 0, Mask =< 64 ->
    true;
acl_rule_verify({user, {U, S}}) when is_binary(U), is_binary(S) ->
    true;
acl_rule_verify({user, U}) when is_binary(U) ->
    true;
acl_rule_verify({server, S}) when is_binary(S) ->
    true;
acl_rule_verify({resource, R}) when is_binary(R) ->
    true;
acl_rule_verify({shared_group, {G, H}}) when is_binary(G), is_binary(H) ->
    true;
acl_rule_verify({shared_group, G}) when is_binary(G) ->
    true;
acl_rule_verify({user_regexp, {UR, S}}) when is_binary(UR), is_binary(S) ->
    true;
acl_rule_verify({user_regexp, UR}) when is_binary(UR) ->
    true;
acl_rule_verify({server_regexp, SR}) when is_binary(SR) ->
    true;
acl_rule_verify({resource_regexp, RR}) when is_binary(RR) ->
    true;
acl_rule_verify({node_regexp, {UR, SR}}) when is_binary(UR), is_binary(SR) ->
    true;
acl_rule_verify({user_glob, {UR, S}}) when is_binary(UR), is_binary(S) ->
    true;
acl_rule_verify({user_glob, UR}) when is_binary(UR) ->
    true;
acl_rule_verify({server_glob, SR}) when is_binary(SR) ->
    true;
acl_rule_verify({resource_glob, RR}) when is_binary(RR) ->
    true;
acl_rule_verify({node_glob, {UR, SR}}) when is_binary(UR), is_binary(SR) ->
    true;
acl_rule_verify(_Spec) ->
    false.
invalid_syntax(Msg, Data) ->
    throw({invalid_syntax, (str:format(Msg, Data))}).

acl_rules_verify([{acl, Name} | Rest], true) when is_atom(Name) ->
    acl_rules_verify(Rest, true);
acl_rules_verify([{acl, Name} = Rule | _Rest], false) when is_atom(Name) ->
    invalid_syntax(<<"Using acl: rules not allowed: ~p">>, [Rule]);
acl_rules_verify([Rule | Rest], AllowAcl) ->
    case acl_rule_verify(Rule) of
	false ->
	    invalid_syntax(<<"Invalid rule: ~p">>, [Rule]);
	true ->
	    acl_rules_verify(Rest, AllowAcl)
    end;
acl_rules_verify([], _AllowAcl) ->
    true;
acl_rules_verify(Rules, _AllowAcl) ->
    invalid_syntax(<<"Not a acl rules list: ~p">>, [Rules]).



all_acl_rules_matches([], _Data, _Host) ->
    false;
all_acl_rules_matches(Rules, Data, Host) ->
    all_acl_rules_matches2(Rules, Data, Host).

all_acl_rules_matches2([Rule | Tail], Data, Host) ->
    case acl_rule_matches(Rule, Data, Host) of
	true ->
	    all_acl_rules_matches2(Tail, Data, Host);
	false ->
	    false
    end;
all_acl_rules_matches2([], _Data, _Host) ->
    true.

any_acl_rules_matches([], _Data, _Host) ->
    false;
any_acl_rules_matches([Rule|Tail], Data, Host) ->
    case acl_rule_matches(Rule, Data, Host) of
	true ->
	    true;
	false ->
	    any_acl_rules_matches(Tail, Data, Host)
    end.

-spec acl_rule_matches(aclspec(), any(), global|binary()) -> boolean().

acl_rule_matches(all, _Data, _Host) ->
    true;
acl_rule_matches({acl, all}, _Data, _Host) ->
    true;
acl_rule_matches({acl, Name}, Data, Host) ->
    ACLs = get_aclspecs(Name, Host),
    RawACLs = lists:map(fun(#acl{aclspec = R}) -> R end, ACLs),
    any_acl_rules_matches(RawACLs, Data, Host);
acl_rule_matches({ip, {Net, Mask}}, #{ip := {IP, _Port}}, _Host) ->
    is_ip_match(IP, Net, Mask);
acl_rule_matches({ip, {Net, Mask}}, #{ip := IP}, _Host) ->
    is_ip_match(IP, Net, Mask);
acl_rule_matches({user, {U, S}}, #{usr := {U, S, _}}, _Host) ->
    true;
acl_rule_matches({user, U}, #{usr := {U, S, _}}, _Host) ->
    lists:member(S, ?MYHOSTS);
acl_rule_matches({server, S}, #{usr := {_, S, _}}, _Host) ->
    true;
acl_rule_matches({resource, R}, #{usr := {_, _, R}}, _Host) ->
    true;
acl_rule_matches({shared_group, {G, H}}, #{usr := {U, S, _}}, _Host) ->
    Mod = loaded_shared_roster_module(H),
    Mod:is_user_in_group({U, S}, G, H);
acl_rule_matches({shared_group, G}, #{usr := {U, S, _}}, Host) ->
    Mod = loaded_shared_roster_module(Host),
    Mod:is_user_in_group({U, S}, G, Host);
acl_rule_matches({user_regexp, {UR, S}}, #{usr := {U, S, _}}, _Host) ->
    is_regexp_match(U, UR);
acl_rule_matches({user_regexp, UR}, #{usr := {U, S, _}}, _Host) ->
    lists:member(S, ?MYHOSTS) andalso is_regexp_match(U, UR);
acl_rule_matches({server_regexp, SR}, #{usr := {_, S, _}}, _Host) ->
    is_regexp_match(S, SR);
acl_rule_matches({resource_regexp, RR}, #{usr := {_, _, R}}, _Host) ->
    is_regexp_match(R, RR);
acl_rule_matches({node_regexp, {UR, SR}}, #{usr := {U, S, _}}, _Host) ->
    is_regexp_match(U, UR) andalso is_regexp_match(S, SR);
acl_rule_matches({user_glob, {UR, S}}, #{usr := {U, S, _}}, _Host) ->
    is_glob_match(U, UR);
acl_rule_matches({user_glob, UR}, #{usr := {U, S, _}}, _Host) ->
    lists:member(S, ?MYHOSTS) andalso is_glob_match(U, UR);
acl_rule_matches({server_glob, SR}, #{usr := {_, S, _}}, _Host) ->
    is_glob_match(S, SR);
acl_rule_matches({resource_glob, RR}, #{usr := {_, _, R}}, _Host) ->
    is_glob_match(R, RR);
acl_rule_matches({node_glob, {UR, SR}}, #{usr := {U, S, _}}, _Host) ->
    is_glob_match(U, UR) andalso is_glob_match(S, SR);
acl_rule_matches(_ACL, _Data, _Host) ->
    false.

resolve_access(all, _Host) ->
    all;
resolve_access(none, _Host) ->
    none;
resolve_access(Name, Host) when is_atom(Name) ->
    GAccess = mnesia:dirty_read(access, {Name, global}),
    LAccess =
    if Host /= global -> mnesia:dirty_read(access, {Name, Host});
	    true -> []
	end,
    case GAccess ++ LAccess of
	[] ->
	    [];
	AccessList ->
	    lists:flatmap(
		fun(#access{rules = Rs}) ->
		    Rs
		end, AccessList)
    end;
resolve_access(Rules, _Host) when is_list(Rules) ->
    Rules.

-spec access_matches(atom()|list(), any(), global|binary()) -> allow|deny|atom()|integer().
access_matches(Rules, Data, Host) ->
    case resolve_access(Rules, Host) of
	all -> allow;
	none -> deny;
	RRules -> access_rules_matches(RRules, Data, Host)
    end.

-spec access_rules_matches(list(), any(), global|binary()) -> any().

access_rules_matches(AR, Data, Host) ->
    access_rules_matches(AR, Data, Host, deny).

access_rules_matches([{Type, Acls} | Rest], Data, Host, Default) ->
    case all_acl_rules_matches(Acls, Data, Host) of
	false ->
	    access_rules_matches(Rest, Data, Host, Default);
	true ->
	    Type
    end;
access_rules_matches([], _Data, _Host, Default) ->
    Default.

get_aclspecs(ACL, Host) ->
    mnesia:dirty_read(acl, {ACL, Host}) ++ mnesia:dirty_read(acl, {ACL, global}).

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
	  case catch binary_to_integer(MaskStr) of
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

transform_access_rules_config(Config) when is_list(Config) ->
    lists:map(fun transform_access_rules_config2/1, lists:flatten(Config));
transform_access_rules_config(Config) ->
    transform_access_rules_config([Config]).

transform_access_rules_config2(Type) when is_integer(Type); is_atom(Type) ->
    {Type, [all]};
transform_access_rules_config2({Type, ACL}) when is_atom(ACL) ->
    {Type, [{acl, ACL}]};
transform_access_rules_config2({Res, Rules}) when is_list(Rules) ->
    T = lists:map(fun({Type, Args}) when is_list(Args) ->
			  normalize_spec({Type, hd(lists:flatten(Args))});
		     (V) -> normalize_spec(V)
		  end, lists:flatten(Rules)),
    {Res, T};
transform_access_rules_config2({Res, Rule}) ->
    {Res, [Rule]}.

access_rules_validator(Name) when is_atom(Name) ->
    Name;
access_rules_validator(Rules0) ->
    Rules = transform_access_rules_config(Rules0),
    access_shaper_rules_validator(Rules, fun(allow) -> true;
					  (deny) -> true;
					  (_) -> false
				       end),
    throw({replace_with, Rules}).


shaper_rules_validator(Name) when is_atom(Name) ->
    Name;
shaper_rules_validator(Rules0) ->
    Rules = transform_access_rules_config(Rules0),
    access_shaper_rules_validator(Rules, fun(V) when is_atom(V) -> true;
					  (V2) when is_integer(V2) -> true;
					  (_) -> false
				       end),
    throw({replace_with, Rules}).

access_shaper_rules_validator([{Type, Acls} = Rule | Rest], RuleTypeCheck) ->
    case RuleTypeCheck(Type) of
	true ->
	    case acl_rules_verify(Acls, true) of
		true ->
		    access_shaper_rules_validator(Rest, RuleTypeCheck);
		Err ->
		    Err
	    end;
	false ->
	    invalid_syntax(<<"Invalid rule type: ~p in rule ~p">>, [Type, Rule])
    end;
access_shaper_rules_validator([], _RuleTypeCheck) ->
    true;
access_shaper_rules_validator(Value, _RuleTypeCheck) ->
    invalid_syntax(<<"Not a rule definition: ~p">>, [Value]).


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
    {NewAccessOpts, Opts4} = lists:mapfoldl(
                            fun({access_rules, Os}, Acc) ->
                                    {Os, Acc};
                               (O, Acc) ->
                                    {[], [O|Acc]}
                            end, [], Opts3),
    {ShaperOpts, Opts5} = lists:mapfoldl(
                            fun({shaper_rules, Os}, Acc) ->
                                    {Os, Acc};
                               (O, Acc) ->
                                    {[], [O|Acc]}
                            end, [], Opts4),
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
    NewAccessOpts1 = case lists:map(
			    fun({NAName, Os}) ->
				    {NAName, transform_access_rules_config(Os)}
			    end, lists:flatten(NewAccessOpts)) of
			 [] -> [];
			 L3 -> [{access_rules, L3}]
		     end,
    ShaperOpts1 = case lists:map(
			    fun({SName, Ss}) ->
				    {SName, transform_access_rules_config(Ss)}
			    end, lists:flatten(ShaperOpts)) of
			 [] -> [];
			 L4 -> [{shaper_rules, L4}]
		     end,
    ACLOpts2 ++ AccessOpts1 ++ NewAccessOpts1 ++ ShaperOpts1 ++ Opts5.

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
            {resource_glob, R} -> {resource_glob, [b(R)]};
            {resource_regexp, R} -> {resource_regexp, [b(R)]}
        end,
    [{acl, [{Name, [T]}]}|Opts];
transform_options({access, Name, Rules}, Opts) ->
    NewRules = [{ACL, Action} || {Action, ACL} <- Rules],
    [{access, [{Name, NewRules}]}|Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

opt_type(access) -> fun (V) -> V end;
opt_type(access_rules) -> fun (V) -> V end;
opt_type(shaper_rules) -> fun (V) -> V end;
opt_type(acl) -> fun (V) -> V end;
opt_type(_) -> [access, acl, access_rules, shaper_rules].
