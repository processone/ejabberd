%%%----------------------------------------------------------------------
%%% Purpose: Transform old-style Erlang config to YAML config
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
-module(ejabberd_old_config).

%% API
-export([read_file/1]).

-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
read_file(File) ->
    case consult(File) of
	{ok, Terms1} ->
	    ?INFO_MSG("Converting from old configuration format", []),
	    Terms2 = strings_to_binary(Terms1),
	    Terms3 = transform(Terms2),
	    Terms4 = transform_certfiles(Terms3),
	    Terms5 = transform_host_config(Terms4),
	    {ok, collect_options(Terms5)};
	{error, Reason} ->
	    {error, {old_config, File, Reason}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
collect_options(Opts) ->
    {D, InvalidOpts} =
        lists:foldl(
          fun({K, V}, {D, Os}) when is_list(V) ->
                  {orddict:append_list(K, V, D), Os};
             ({K, V}, {D, Os}) ->
                  {orddict:store(K, V, D), Os};
             (Opt, {D, Os}) ->
                  {D, [Opt|Os]}
          end, {orddict:new(), []}, Opts),
    InvalidOpts ++ orddict:to_list(D).

transform(Opts) ->
    Opts1 = transform_register(Opts),
    Opts2 = transform_s2s(Opts1),
    Opts3 = transform_listeners(Opts2),
    Opts5 = transform_sql(Opts3),
    Opts6 = transform_shaper(Opts5),
    Opts7 = transform_s2s_out(Opts6),
    Opts8 = transform_acl(Opts7),
    Opts9 = transform_modules(Opts8),
    Opts10 = transform_globals(Opts9),
    collect_options(Opts10).

%%%===================================================================
%%% mod_register
%%%===================================================================
transform_register(Opts) ->
    try
        {value, {modules, ModOpts}, Opts1} = lists:keytake(modules, 1, Opts),
        {value, {?MODULE, RegOpts}, ModOpts1} = lists:keytake(?MODULE, 1, ModOpts),
        {value, {ip_access, L}, RegOpts1} = lists:keytake(ip_access, 1, RegOpts),
        true = is_list(L),
        ?WARNING_MSG("Old 'ip_access' format detected. "
                     "The old format is still supported "
                     "but it is better to fix your config: "
                     "use access rules instead.", []),
        ACLs = lists:flatmap(
                 fun({Action, S}) ->
                         ACLName = misc:binary_to_atom(
                                     iolist_to_binary(
                                       ["ip_", S])),
                         [{Action, ACLName},
                          {acl, ACLName, {ip, S}}]
                 end, L),
        Access = {access, mod_register_networks,
                  [{Action, ACLName} || {Action, ACLName} <- ACLs]},
        [ACL || {acl, _, _} = ACL <- ACLs] ++
            [Access,
             {modules,
              [{mod_register,
                [{ip_access, mod_register_networks}|RegOpts1]}
               | ModOpts1]}|Opts1]
    catch error:{badmatch, false} ->
            Opts
    end.

%%%===================================================================
%%% ejabberd_s2s
%%%===================================================================
transform_s2s(Opts) ->
    lists:foldl(fun transform_s2s/2, [], Opts).

transform_s2s({{s2s_host, Host}, Action}, Opts) ->
    ?WARNING_MSG("Option 's2s_host' is deprecated.", []),
    ACLName = misc:binary_to_atom(
                iolist_to_binary(["s2s_access_", Host])),
    [{acl, ACLName, {server, Host}},
     {access, s2s, [{Action, ACLName}]},
     {s2s_access, s2s} |
     Opts];
transform_s2s({s2s_default_policy, Action}, Opts) ->
    ?WARNING_MSG("Option 's2s_default_policy' is deprecated. "
                 "The option is still supported but it is better to "
                 "fix your config: "
                 "use 's2s_access' with an access rule.", []),
    [{access, s2s, [{Action, all}]},
     {s2s_access, s2s} |
     Opts];
transform_s2s(Opt, Opts) ->
    [Opt|Opts].

%%%===================================================================
%%% ejabberd_s2s_out
%%%===================================================================
transform_s2s_out(Opts) ->
    lists:foldl(fun transform_s2s_out/2, [], Opts).

transform_s2s_out({outgoing_s2s_options, Families, Timeout}, Opts) ->
    ?WARNING_MSG("Option 'outgoing_s2s_options' is deprecated. "
                 "The option is still supported "
                 "but it is better to fix your config: "
                 "use 'outgoing_s2s_timeout' and "
                 "'outgoing_s2s_families' instead.", []),
    [{outgoing_s2s_families, Families},
     {outgoing_s2s_timeout, Timeout}
     | Opts];
transform_s2s_out({s2s_dns_options, S2SDNSOpts}, AllOpts) ->
    ?WARNING_MSG("Option 's2s_dns_options' is deprecated. "
                 "The option is still supported "
                 "but it is better to fix your config: "
                 "use 's2s_dns_timeout' and "
                 "'s2s_dns_retries' instead", []),
    lists:foldr(
      fun({timeout, T}, AccOpts) ->
              [{s2s_dns_timeout, T}|AccOpts];
         ({retries, R}, AccOpts) ->
              [{s2s_dns_retries, R}|AccOpts];
         (_, AccOpts) ->
              AccOpts
      end, AllOpts, S2SDNSOpts);
transform_s2s_out(Opt, Opts) ->
    [Opt|Opts].

%%%===================================================================
%%% ejabberd_listener
%%%===================================================================
transform_listeners(Opts) ->
    lists:foldl(fun transform_listeners/2, [], Opts).

transform_listeners({listen, LOpts}, Opts) ->
    [{listen, lists:map(fun transform_listener/1, LOpts)} | Opts];
transform_listeners(Opt, Opts) ->
    [Opt|Opts].

transform_listener({{Port, IP, Transport}, Mod, Opts}) ->
    IPStr = if is_tuple(IP) ->
                    list_to_binary(inet_parse:ntoa(IP));
               true ->
                    IP
            end,
    Opts1 = lists:map(
              fun({ip, IPT}) when is_tuple(IPT) ->
                      {ip, list_to_binary(inet_parse:ntoa(IP))};
                 (ssl) -> {tls, true};
		 (A) when is_atom(A) -> {A, true};
                 (Opt) -> Opt
              end, Opts),
    Opts2 = lists:foldl(
              fun(Opt, Acc) ->
		      transform_listen_option(Mod, Opt, Acc)
              end, [], Opts1),
    TransportOpt = if Transport == tcp -> [];
                      true -> [{transport, Transport}]
                   end,
    IPOpt = if IPStr == <<"0.0.0.0">> -> [];
               true -> [{ip, IPStr}]
            end,
    IPOpt ++ TransportOpt ++ [{port, Port}, {module, Mod} | Opts2];
transform_listener({{Port, Transport}, Mod, Opts})
  when Transport == tcp orelse Transport == udp ->
    transform_listener({{Port, all_zero_ip(Opts), Transport}, Mod, Opts});
transform_listener({{Port, IP}, Mod, Opts}) ->
    transform_listener({{Port, IP, tcp}, Mod, Opts});
transform_listener({Port, Mod, Opts}) ->
    transform_listener({{Port, all_zero_ip(Opts), tcp}, Mod, Opts});
transform_listener(Opt) ->
    Opt.

transform_listen_option(ejabberd_http, captcha, Opts) ->
    [{captcha, true}|Opts];
transform_listen_option(ejabberd_http, register, Opts) ->
    [{register, true}|Opts];
transform_listen_option(ejabberd_http, web_admin, Opts) ->
    [{web_admin, true}|Opts];
transform_listen_option(ejabberd_http, http_bind, Opts) ->
    [{http_bind, true}|Opts];
transform_listen_option(ejabberd_http, http_poll, Opts) ->
    [{http_poll, true}|Opts];
transform_listen_option(ejabberd_http, {request_handlers, Hs}, Opts) ->
    Hs1 = lists:map(
            fun({PList, Mod}) when is_list(PList) ->
                    Path = iolist_to_binary([[$/, P] || P <- PList]),
                    {Path, Mod};
               (Opt) ->
                    Opt
            end, Hs),
    [{request_handlers, Hs1} | Opts];
transform_listen_option(ejabberd_service, {hosts, Hosts, O}, Opts) ->
    case lists:keyfind(hosts, 1, Opts) of
        {_, PrevHostOpts} ->
            NewHostOpts =
                lists:foldl(
                  fun(H, Acc) ->
                          dict:append_list(H, O, Acc)
                  end, dict:from_list(PrevHostOpts), Hosts),
            [{hosts, dict:to_list(NewHostOpts)}|
             lists:keydelete(hosts, 1, Opts)];
        _ ->
            [{hosts, [{H, O} || H <- Hosts]}|Opts]
    end;
transform_listen_option(ejabberd_service, {host, Host, Os}, Opts) ->
    transform_listen_option(ejabberd_service, {hosts, [Host], Os}, Opts);
transform_listen_option(ejabberd_xmlrpc, {access_commands, ACOpts}, Opts) ->
    NewACOpts = lists:map(
                  fun({AName, ACmds, AOpts}) ->
                          {AName, [{commands, ACmds}, {options, AOpts}]};
                     (Opt) ->
                          Opt
                  end, ACOpts),
    [{access_commands, NewACOpts}|Opts];
transform_listen_option(_, Opt, Opts) ->
    [Opt|Opts].

-spec all_zero_ip([proplists:property()]) -> inet:ip_address().
all_zero_ip(Opts) ->
    case proplists:get_bool(inet6, Opts) of
	true -> {0,0,0,0,0,0,0,0};
	false -> {0,0,0,0}
    end.

%%%===================================================================
%%% ejabberd_shaper
%%%===================================================================
transform_shaper(Opts) ->
    lists:foldl(fun transform_shaper/2, [], Opts).

transform_shaper({shaper, Name, {maxrate, N}}, Opts) ->
    [{shaper, [{Name, N}]} | Opts];
transform_shaper({shaper, Name, none}, Opts) ->
    [{shaper, [{Name, none}]} | Opts];
transform_shaper({shaper, List}, Opts) when is_list(List) ->
    R = lists:map(
	  fun({Name, Args}) when is_list(Args) ->
		  MaxRate = proplists:get_value(rate, Args, 1000),
		  BurstSize = proplists:get_value(burst_size, Args, MaxRate),
		  {Name, MaxRate, BurstSize};
	     ({Name, Val}) ->
		  {Name, Val, Val}
	  end, List),
    [{shaper, R} | Opts];
transform_shaper(Opt, Opts) ->
    [Opt | Opts].

%%%===================================================================
%%% acl
%%%===================================================================
transform_acl(Opts) ->
    Opts1 = lists:foldl(fun transform_acl/2, [], Opts),
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
    ACLOpts1 = collect_options(lists:flatten(ACLOpts)),
    AccessOpts1 = case collect_options(lists:flatten(AccessOpts)) of
                      [] -> [];
                      L1 -> [{access, L1}]
                  end,
    ACLOpts2 = case lists:map(
                      fun({ACLName, Os}) ->
                              {ACLName, collect_options(Os)}
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

transform_acl({acl, Name, Type}, Opts) ->
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
transform_acl({access, Name, Rules}, Opts) ->
    NewRules = [{ACL, Action} || {Action, ACL} <- Rules],
    [{access, [{Name, NewRules}]}|Opts];
transform_acl(Opt, Opts) ->
    [Opt|Opts].

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
			  {Type, hd(lists:flatten(Args))};
		     (V) ->
			  V
		  end, lists:flatten(Rules)),
    {Res, T};
transform_access_rules_config2({Res, Rule}) ->
    {Res, [Rule]}.

%%%===================================================================
%%% SQL
%%%===================================================================
-define(PGSQL_PORT, 5432).
-define(MYSQL_PORT, 3306).

transform_sql(Opts) ->
    lists:foldl(fun transform_sql/2, [], Opts).

transform_sql({odbc_server, {Type, Server, Port, DB, User, Pass}}, Opts) ->
    [{sql_type, Type},
     {sql_server, Server},
     {sql_port, Port},
     {sql_database, DB},
     {sql_username, User},
     {sql_password, Pass}|Opts];
transform_sql({odbc_server, {mysql, Server, DB, User, Pass}}, Opts) ->
    transform_sql({odbc_server, {mysql, Server, ?MYSQL_PORT, DB, User, Pass}}, Opts);
transform_sql({odbc_server, {pgsql, Server, DB, User, Pass}}, Opts) ->
    transform_sql({odbc_server, {pgsql, Server, ?PGSQL_PORT, DB, User, Pass}}, Opts);
transform_sql({odbc_server, {sqlite, DB}}, Opts) ->
    [{sql_type, sqlite},
     {sql_database, DB}|Opts];
transform_sql({odbc_pool_size, N}, Opts) ->
    [{sql_pool_size, N}|Opts];
transform_sql(Opt, Opts) ->
    [Opt|Opts].

%%%===================================================================
%%% modules
%%%===================================================================
transform_modules(Opts) ->
    lists:foldl(fun transform_modules/2, [], Opts).

transform_modules({modules, ModOpts}, Opts) ->
    [{modules, lists:map(
		 fun({Mod, Opts1}) ->
			 {Mod, transform_module(Mod, Opts1)};
		    (Other) ->
			 Other
		 end, ModOpts)}|Opts];
transform_modules(Opt, Opts) ->
    [Opt|Opts].

transform_module(mod_disco, Opts) ->
    lists:map(
      fun({server_info, Infos}) ->
              NewInfos = lists:map(
                           fun({Modules, Name, URLs}) ->
                                   [[{modules, Modules},
                                     {name, Name},
                                     {urls, URLs}]];
                              (Opt) ->
                                   Opt
                           end, Infos),
              {server_info, NewInfos};
         (Opt) ->
              Opt
      end, Opts);
transform_module(mod_muc_log, Opts) ->
    lists:map(
      fun({top_link, {S1, S2}}) ->
              {top_link, [{S1, S2}]};
         (Opt) ->
              Opt
      end, Opts);
transform_module(mod_proxy65, Opts) ->
    lists:map(
      fun({ip, IP}) when is_tuple(IP) ->
              {ip, misc:ip_to_list(IP)};
         ({hostname, IP}) when is_tuple(IP) ->
              {hostname, misc:ip_to_list(IP)};
         (Opt) ->
              Opt
      end, Opts);
transform_module(mod_register, Opts) ->
    lists:flatmap(
      fun({welcome_message, {Subj, Body}}) ->
              [{welcome_message, [{subject, Subj}, {body, Body}]}];
         (Opt) ->
              [Opt]
      end, Opts);
transform_module(_Mod, Opts) ->
    Opts.

%%%===================================================================
%%% Host config
%%%===================================================================
transform_host_config(Opts) ->
    Opts1 = lists:foldl(fun transform_host_config/2, [], Opts),
    {HOpts, Opts2} = lists:mapfoldl(
                       fun({host_config, O}, Os) ->
                               {[O], Os};
                          (O, Os) ->
                               {[], [O|Os]}
                       end, [], Opts1),
    {AHOpts, Opts3} = lists:mapfoldl(
                        fun({append_host_config, O}, Os) ->
                                {[O], Os};
                           (O, Os) ->
                                {[], [O|Os]}
                        end, [], Opts2),
    HOpts1 = case collect_options(lists:flatten(HOpts)) of
                 [] ->
                     [];
                 HOs ->
                     [{host_config,
                       [{H, transform(O)} || {H, O} <- HOs]}]
             end,
    AHOpts1 = case collect_options(lists:flatten(AHOpts)) of
                  [] ->
                      [];
                  AHOs ->
                      [{append_host_config,
                        [{H, transform(O)} || {H, O} <- AHOs]}]
              end,
    HOpts1 ++ AHOpts1 ++ Opts3.

transform_host_config({host_config, Host, HOpts}, Opts) ->
    {AddOpts, HOpts1} =
        lists:mapfoldl(
          fun({{add, Opt}, Val}, Os) ->
                  {[{Opt, Val}], Os};
             (O, Os) ->
                  {[], [O|Os]}
          end, [], HOpts),
    [{append_host_config, [{Host, lists:flatten(AddOpts)}]},
     {host_config, [{Host, HOpts1}]}|Opts];
transform_host_config(Opt, Opts) ->
    [Opt|Opts].

%%%===================================================================
%%% Top-level options
%%%===================================================================
transform_globals(Opts) ->
    lists:foldl(fun transform_globals/2, [], Opts).

transform_globals(Opt, Opts) when Opt == override_global;
				  Opt == override_local;
				  Opt == override_acls ->
    ?WARNING_MSG("Option '~ts' has no effect anymore", [Opt]),
    Opts;
transform_globals({node_start, _}, Opts) ->
    ?WARNING_MSG("Option 'node_start' has no effect anymore", []),
    Opts;
transform_globals({iqdisc, {queues, N}}, Opts) ->
    [{iqdisc, N}|Opts];
transform_globals({define_macro, Macro, Val}, Opts) ->
    [{define_macro, [{Macro, Val}]}|Opts];
transform_globals(Opt, Opts) ->
    [Opt|Opts].

%%%===================================================================
%%% Certfiles
%%%===================================================================
transform_certfiles(Opts) ->
    lists:foldl(fun transform_certfiles/2, [], Opts).

transform_certfiles({domain_certfile, Domain, CertFile}, Opts) ->
    [{host_config, [{Domain, [{domain_certfile, CertFile}]}]}|Opts];
transform_certfiles(Opt, Opts) ->
    [Opt|Opts].

%%%===================================================================
%%% Consult file
%%%===================================================================
consult(File) ->
    case file:consult(File) of
	{ok, Terms} ->
	    include_config_files(Terms);
	Err ->
	    Err
    end.

include_config_files(Terms) ->
    include_config_files(Terms, []).

include_config_files([], Res) ->
    {ok, Res};
include_config_files([{include_config_file, Filename} | Terms], Res) ->
    include_config_files([{include_config_file, Filename, []} | Terms], Res);
include_config_files([{include_config_file, Filename, Options} | Terms], Res) ->
    case consult(Filename) of
	{ok, Included_terms} ->
	    Disallow = proplists:get_value(disallow, Options, []),
	    Included_terms2 = delete_disallowed(Disallow, Included_terms),
	    Allow_only = proplists:get_value(allow_only, Options, all),
	    Included_terms3 = keep_only_allowed(Allow_only, Included_terms2),
	    include_config_files(Terms, Res ++ Included_terms3);
	Err ->
	    Err
    end;
include_config_files([Term | Terms], Res) ->
    include_config_files(Terms, Res ++ [Term]).

delete_disallowed(Disallowed, Terms) ->
    lists:foldl(
      fun(Dis, Ldis) ->
	      delete_disallowed2(Dis, Ldis)
      end,
      Terms,
      Disallowed).

delete_disallowed2(Disallowed, [H|T]) ->
    case element(1, H) of
	Disallowed ->
	    delete_disallowed2(Disallowed, T);
	_ ->
	    [H|delete_disallowed2(Disallowed, T)]
    end;
delete_disallowed2(_, []) ->
    [].

keep_only_allowed(all, Terms) ->
    Terms;
keep_only_allowed(Allowed, Terms) ->
    {As, _NAs} = lists:partition(
		   fun(Term) ->
			   lists:member(element(1, Term), Allowed)
		   end, Terms),
    As.

%%%===================================================================
%%% Aux functions
%%%===================================================================
strings_to_binary([]) ->
    [];
strings_to_binary(L) when is_list(L) ->
    case is_string(L) of
        true ->
            list_to_binary(L);
        false ->
            strings_to_binary1(L)
    end;
strings_to_binary({A, B, C, D}) when
	is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    {A, B, C, D};
strings_to_binary(T) when is_tuple(T) ->
    list_to_tuple(strings_to_binary1(tuple_to_list(T)));
strings_to_binary(X) ->
    X.

strings_to_binary1([El|L]) ->
    [strings_to_binary(El)|strings_to_binary1(L)];
strings_to_binary1([]) ->
    [];
strings_to_binary1(T) ->
    T.

is_string([C|T]) when (C >= 0) and (C =< 255) ->
    is_string(T);
is_string([]) ->
    true;
is_string(_) ->
    false.

b(S) ->
    iolist_to_binary(S).
