%%%----------------------------------------------------------------------
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

-export([start_link/0]).
-export([reload_from_config/0]).
-export([match_rule/3, match_acl/3]).
-export([match_rules/4, match_acls/3]).
-export([access_rules_validator/0, access_validator/0]).
-export([validator/1, validators/0]).
-export([loaded_shared_roster_module/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").

-type state() :: #{hosts := [binary()]}.
-type action() :: allow | deny.
-type ip_mask() :: {inet:ip4_address(), 0..32} | {inet:ip6_address(), 0..128}.
-type access_rule() :: {acl, atom()} | acl_rule().
-type acl_rule() :: {user, {binary(), binary()} | binary()} |
		    {server, binary()} |
		    {resource, binary()} |
		    {user_regexp, {re_mp(), binary()} | re_mp()} |
		    {server_regexp, re_mp()} |
		    {resource_regexp, re_mp()} |
		    {node_regexp, {re_mp(), re_mp()}} |
		    {user_glob, {re_mp(), binary()} | re_mp()} |
		    {server_glob, re_mp()} |
		    {resource_glob, re_mp()} |
		    {node_glob, {re_mp(), re_mp()}} |
		    {shared_group, {binary(), binary()} | binary()} |
		    {ip, ip_mask()}.
-type access() :: [{action(), [access_rule()]}].
-type acl() :: atom() | access().
-type match() :: #{ip => inet:ip_address(),
		   usr => jid:ljid(),
		   atom() => term()}.

-export_type([acl/0, acl_rule/0, access/0, access_rule/0, match/0]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec match_rule(global | binary(), atom() | access(),
                 jid:jid() | jid:ljid() | inet:ip_address() | match()) -> action().
match_rule(_, all, _) ->
    allow;
match_rule(_, none, _) ->
    deny;
match_rule(Host, Access, Match) when is_map(Match) ->
    Rules = if is_atom(Access) -> read_access(Access, Host);
	       true -> Access
	    end,
    match_rules(Host, Rules, Match, deny);
match_rule(Host, Access, IP) when tuple_size(IP) == 4; tuple_size(IP) == 8 ->
    match_rule(Host, Access, #{ip => IP});
match_rule(Host, Access, JID) ->
    match_rule(Host, Access, #{usr => jid:tolower(JID)}).

-spec match_acl(global | binary(), access_rule(), match()) -> boolean().
match_acl(_Host, {acl, all}, _) ->
    true;
match_acl(_Host, {acl, none}, _) ->
    false;
match_acl(Host, {acl, ACLName}, Match) ->
    lists:any(
      fun(ACL) ->
	      match_acl(Host, ACL, Match)
      end, read_acl(ACLName, Host));
match_acl(_Host, {ip, {Net, Mask}}, #{ip := {IP, _Port}}) ->
    misc:match_ip_mask(IP, Net, Mask);
match_acl(_Host, {ip, {Net, Mask}}, #{ip := IP}) ->
    misc:match_ip_mask(IP, Net, Mask);
match_acl(_Host, {user, {U, S}}, #{usr := {U, S, _}}) ->
    true;
match_acl(_Host, {user, U}, #{usr := {U, S, _}}) ->
    ejabberd_router:is_my_host(S);
match_acl(_Host, {server, S}, #{usr := {_, S, _}}) ->
    true;
match_acl(_Host, {resource, R}, #{usr := {_, _, R}}) ->
    true;
match_acl(_Host, {shared_group, {G, H}}, #{usr := {U, S, _}}) ->
    case loaded_shared_roster_module(H) of
	undefined -> false;
	Mod -> Mod:is_user_in_group({U, S}, G, H)
    end;
match_acl(Host, {shared_group, G}, Map) ->
    match_acl(Host, {shared_group, {G, Host}}, Map);
match_acl(_Host, {user_regexp, {UR, S1}}, #{usr := {U, S2, _}}) ->
    S1 == S2 andalso match_regexp(U, UR);
match_acl(_Host, {user_regexp, UR}, #{usr := {U, S, _}}) ->
    ejabberd_router:is_my_host(S) andalso match_regexp(U, UR);
match_acl(_Host, {server_regexp, SR}, #{usr := {_, S, _}}) ->
    match_regexp(S, SR);
match_acl(_Host, {resource_regexp, RR}, #{usr := {_, _, R}}) ->
    match_regexp(R, RR);
match_acl(_Host, {node_regexp, {UR, SR}}, #{usr := {U, S, _}}) ->
    match_regexp(U, UR) andalso match_regexp(S, SR);
match_acl(_Host, {user_glob, {UR, S1}}, #{usr := {U, S2, _}}) ->
    S1 == S2 andalso match_regexp(U, UR);
match_acl(_Host, {user_glob, UR}, #{usr := {U, S, _}}) ->
    ejabberd_router:is_my_host(S) andalso match_regexp(U, UR);
match_acl(_Host, {server_glob, SR}, #{usr := {_, S, _}}) ->
    match_regexp(S, SR);
match_acl(_Host, {resource_glob, RR}, #{usr := {_, _, R}}) ->
    match_regexp(R, RR);
match_acl(_Host, {node_glob, {UR, SR}}, #{usr := {U, S, _}}) ->
    match_regexp(U, UR) andalso match_regexp(S, SR);
match_acl(_, _, _) ->
    false.

-spec match_rules(global | binary(), [{T, [access_rule()]}], match(), T) -> T.
match_rules(Host, [{Return, Rules} | Rest], Match, Default) ->
    case match_acls(Host, Rules, Match) of
	false ->
	    match_rules(Host, Rest, Match, Default);
	true ->
	    Return
    end;
match_rules(_Host, [], _Match, Default) ->
    Default.

-spec match_acls(global | binary(), [access_rule()], match()) -> boolean().
match_acls(_Host, [], _Match) ->
    false;
match_acls(Host, Rules, Match) ->
    lists:all(
      fun(Rule) ->
	      match_acl(Host, Rule, Match)
      end, Rules).

-spec reload_from_config() -> ok.
reload_from_config() ->
    gen_server:call(?MODULE, reload_from_config, timer:minutes(1)).

-spec validator(access_rules | acl) -> econf:validator().
validator(access_rules) ->
    econf:options(
      #{'_' => access_rules_validator()},
      [{disallowed, [all, none]}, unique]);
validator(acl) ->
    econf:options(
      #{'_' => acl_validator()},
      [{disallowed, [all, none]}, unique]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, state()}.
init([]) ->
    create_tab(acl),
    create_tab(access),
    Hosts = ejabberd_option:hosts(),
    load_from_config(Hosts),
    ejabberd_hooks:add(config_reloaded, ?MODULE, reload_from_config, 20),
    {ok, #{hosts => Hosts}}.

-spec handle_call(term(), term(), state()) -> {reply, ok, state()} | {noreply, state()}.
handle_call(reload_from_config, _, State) ->
    NewHosts = ejabberd_option:hosts(),
    load_from_config(NewHosts),
    {reply, ok, State#{hosts => NewHosts}};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, reload_from_config, 20).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
%%% Table management
%%%===================================================================
-spec load_from_config([binary()]) -> ok.
load_from_config(NewHosts) ->
    ?DEBUG("Loading access rules from config", []),
    load_tab(acl, NewHosts, fun ejabberd_option:acl/1),
    load_tab(access, NewHosts, fun ejabberd_option:access_rules/1),
    ?DEBUG("Access rules loaded successfully", []).

-spec create_tab(atom()) -> atom().
create_tab(Tab) ->
    _ = mnesia:delete_table(Tab),
    ets:new(Tab, [named_table, set, {read_concurrency, true}]).

-spec load_tab(atom(), [binary()], fun((global | binary()) -> {atom(), list()})) -> ok.
load_tab(Tab, Hosts, Fun) ->
    Old = ets:tab2list(Tab),
    New = lists:flatmap(
            fun(Host) ->
                    [{{Name, Host}, List} || {Name, List} <- Fun(Host)]
            end, [global|Hosts]),
    ets:insert(Tab, New),
    lists:foreach(
      fun({Key, _}) ->
              case lists:keymember(Key, 1, New) of
                  false -> ets:delete(Tab, Key);
                  true -> ok
              end
      end, Old).

-spec read_access(atom(), global | binary()) -> access().
read_access(Name, Host) ->
    case ets:lookup(access, {Name, Host}) of
	[{_, Access}] -> Access;
	[] -> []
    end.

-spec read_acl(atom(), global | binary()) -> [acl_rule()].
read_acl(Name, Host) ->
    case ets:lookup(acl, {Name, Host}) of
        [{_, ACL}] -> ACL;
        [] -> []
    end.

%%%===================================================================
%%% Validators
%%%===================================================================
validators() ->
    #{ip => econf:list_or_single(econf:ip_mask()),
      user => user_validator(econf:user(), econf:domain()),
      user_regexp => user_validator(econf:re([unicode]), econf:domain()),
      user_glob => user_validator(econf:glob([unicode]), econf:domain()),
      server => econf:list_or_single(econf:domain()),
      server_regexp => econf:list_or_single(econf:re([unicode])),
      server_glob => econf:list_or_single(econf:glob([unicode])),
      resource => econf:list_or_single(econf:resource()),
      resource_regexp => econf:list_or_single(econf:re([unicode])),
      resource_glob => econf:list_or_single(econf:glob([unicode])),
      node_regexp => node_validator(econf:re([unicode]), econf:re([unicode])),
      node_glob => node_validator(econf:glob([unicode]), econf:glob([unicode])),
      shared_group => user_validator(econf:binary(), econf:domain()),
      acl => econf:atom()}.

rule_validator() ->
    rule_validator(validators()).

rule_validator(RVs) ->
    econf:and_then(
      econf:non_empty(econf:options(RVs, [])),
      fun(Rules) ->
	      lists:flatmap(
		fun({Type, Rs}) when is_list(Rs) ->
			[{Type, R} || R <- Rs];
		   (Other) ->
			[Other]
		end, Rules)
      end).

access_validator() ->
    econf:and_then(
      fun(L) when is_list(L) ->
	      lists:map(
		fun({K, V}) -> {(econf:atom())(K), V};
		   (A) -> {acl, (econf:atom())(A)}
		end, lists:flatten(L));
	 (A) ->
	      [{acl, (econf:atom())(A)}]
      end,
      rule_validator()).

access_rules_validator() ->
    econf:and_then(
      fun(L) when is_list(L) ->
	      lists:map(
		fun({K, V}) -> {(econf:atom())(K), V};
		   (A) -> {(econf:atom())(A), [{acl, all}]}
		end, lists:flatten(L));
	 (Bad) ->
	      Bad
      end,
      econf:non_empty(
	econf:options(
	  #{allow => access_validator(),
	    deny => access_validator()},
	  []))).

acl_validator() ->
    econf:and_then(
      fun(L) when is_list(L) -> lists:flatten(L);
	 (Bad) -> Bad
      end,
      rule_validator(maps:remove(acl, validators()))).

user_validator(UV, SV) ->
    econf:and_then(
      econf:list_or_single(
	fun({U, S}) ->
		{UV(U), SV(S)};
	   (M) when is_list(M) ->
		(econf:map(UV, SV))(M);
	   (Val) ->
		US = (econf:binary())(Val),
		case binary:split(US, <<"@">>, [global]) of
		    [U, S] -> {UV(U), SV(S)};
		    [U] -> UV(U);
		    _ -> econf:fail({bad_user, Val})
		end
	end),
      fun lists:flatten/1).

node_validator(UV, SV) ->
    econf:and_then(
      econf:and_then(
	econf:list(econf:any()),
	fun lists:flatten/1),
      econf:map(UV, SV)).

%%%===================================================================
%%% Aux
%%%===================================================================
-spec match_regexp(iodata(), re_mp()) -> boolean().
match_regexp(Data, RegExp) ->
    re:run(Data, RegExp) /= nomatch.

-spec loaded_shared_roster_module(global | binary()) -> atom().
loaded_shared_roster_module(global) ->
    loaded_shared_roster_module(ejabberd_config:get_myname());
loaded_shared_roster_module(Host) ->
    case gen_mod:is_loaded(Host, mod_shared_roster_ldap) of
	true -> mod_shared_roster_ldap;
	false ->
	    case gen_mod:is_loaded(Host, mod_shared_roster) of
		true -> mod_shared_roster;
		false -> undefined
	    end
    end.
