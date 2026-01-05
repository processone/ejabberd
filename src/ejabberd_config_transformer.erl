%%%----------------------------------------------------------------------
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
-module(ejabberd_config_transformer).

%% API
-export([map_reduce/1]).

-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
map_reduce(Y) ->
    F =
    fun(Y1) ->
	Y2 = (validator())(Y1),
	Y3 = transform(Y2),
	case application:get_env(ejabberd, custom_config_transformer) of
	    {ok, TransMod} when is_atom(TransMod) ->
		TransMod:transform(Y3);
	    _ ->
		Y3
	end
    end,
    econf:validate(F, Y).

%%%===================================================================
%%% Transformer
%%%===================================================================
transform(Y) ->
    {Y1, Acc1} = transform(global, Y, #{}),
    {Y2, Acc2} = update(Y1, Acc1),
    filter(global, Y2, Acc2).

transform(Host, Y, Acc) ->
    filtermapfoldr(
      fun({Opt, HostOpts}, Acc1) when (Opt == host_config orelse
				       Opt == append_host_config)
				      andalso Host == global ->
	      case filtermapfoldr(
		     fun({Host1, Opts}, Acc2) ->
			     case transform(Host1, Opts, Acc2) of
				 {[], Acc3} ->
				     {false, Acc3};
				 {Opts1, Acc3} ->
				     {{true, {Host1, Opts1}}, Acc3}
			     end
		     end, Acc1, HostOpts) of
		  {[], Acc4} ->
		      {false, Acc4};
		  {HostOpts1, Acc4} ->
		      {{true, {Opt, HostOpts1}}, Acc4}
	      end;
	 ({Opt, Val}, Acc1) ->
	      transform(Host, Opt, Val, Acc1)
      end, Acc, Y).

transform(Host, modules, ModOpts, Acc) ->
    {ModOpts1, Acc2} =
	lists:mapfoldr(
	  fun({Mod, Opts}, Acc1) ->
		  Opts1 = transform_module_options(Opts),
		  transform_module(Host, Mod, Opts1, Acc1)
	  end, Acc, ModOpts),
    {{true, {modules, ModOpts1}}, Acc2};
transform(global, listen, Listeners, Acc) ->
    {Listeners1, Acc2} =
	lists:mapfoldr(
	  fun(Opts, Acc1) ->
		  transform_listener(Opts, Acc1)
	  end, Acc, Listeners),
    {{true, {listen, Listeners1}}, Acc2};
transform(_Host, Opt, CertFile, Acc) when (Opt == domain_certfile) orelse
					  (Opt == c2s_certfile) orelse
					  (Opt == s2s_certfile) ->
    ?WARNING_MSG("Option '~ts' is deprecated and was automatically "
		 "appended to 'certfiles' option. ~ts",
		 [Opt, adjust_hint()]),
    CertFiles = maps:get(certfiles, Acc, []),
    Acc1 = maps:put(certfiles, CertFiles ++ [CertFile], Acc),
    {false, Acc1};
transform(_Host, certfiles, CertFiles1, Acc) ->
    CertFiles2 = maps:get(certfiles, Acc, []),
    Acc1 = maps:put(certfiles, CertFiles1 ++ CertFiles2, Acc),
    {true, Acc1};
transform(_Host, acme, ACME, Acc) ->
    ACME1 = lists:map(
	      fun({ca_url, URL} = Opt) ->
                      case misc:uri_parse(URL) of
			  {ok, _, _, "acme-v01.api.letsencrypt.org", _, _, _} ->
			      NewURL = ejabberd_acme:default_directory_url(),
			      ?WARNING_MSG("ACME directory URL ~ts defined in "
					   "option acme->ca_url is deprecated "
					   "and was automatically replaced "
					   "with ~ts. ~ts",
					   [URL, NewURL, adjust_hint()]),
			      {ca_url, NewURL};
			  _ ->
			      Opt
		      end;
		 (Opt) ->
		      Opt
	      end, ACME),
    {{true, {acme, ACME1}}, Acc};
transform(Host, s2s_use_starttls, required_trusted, Acc) ->
    ?WARNING_MSG("The value 'required_trusted' of option "
		 "'s2s_use_starttls' is deprecated and was "
		 "automatically replaced with value 'required'. "
		 "The module 'mod_s2s_dialback' has also "
		 "been automatically removed from the configuration. ~ts",
		 [adjust_hint()]),
    Hosts = maps:get(remove_s2s_dialback, Acc, []),
    Acc1 = maps:put(remove_s2s_dialback, [Host|Hosts], Acc),
    {{true, {s2s_use_starttls, required}}, Acc1};
transform(Host, define_macro, Macro, Acc) when is_binary(Host) ->
    ?WARNING_MSG("The option 'define_macro' is not supported inside 'host_config'. "
		 "Consequently those macro definitions for host '~ts' are unused: ~ts",
		 [Host, io_lib:format("~p", [Macro])]),
    {true, Acc};
transform(_Host, _Opt, _Val, Acc) ->
    {true, Acc}.

update(Y, Acc) ->
    set_certfiles(Y, Acc).

filter(Host, Y, Acc) ->
    lists:filtermap(
      fun({Opt, HostOpts}) when (Opt == host_config orelse
				 Opt == append_host_config)
				andalso Host == global ->
	      HostOpts1 = lists:map(
			    fun({Host1, Opts1}) ->
				    {Host1, filter(Host1, Opts1, Acc)}
			    end, HostOpts),
	      {true, {Opt, HostOpts1}};
	 ({Opt, Val}) ->
	      filter(Host, Opt, Val, Acc)
      end, Y).

filter(_Host, log_rotate_date, _, _) ->
    warn_removed_option(log_rotate_date),
    false;
filter(_Host, log_rate_limit, _, _) ->
    warn_removed_option(log_rate_limit),
    false;
filter(_Host, ca_path, _, _) ->
    warn_removed_option(ca_path, ca_file),
    false;
filter(_Host, iqdisc, _, _) ->
    warn_removed_option(iqdisc),
    false;
filter(_Host, access, _, _) ->
    warn_removed_option(access, access_rules),
    false;
filter(_Host, commands, _, _) ->
    warn_removed_option(commands, api_permissions),
    false;
filter(_Host, ejabberdctl_access_commands, _, _) ->
    warn_removed_option(ejabberdctl_access_commands, api_permissions),
    false;
filter(_Host, commands_admin_access, _, _) ->
    warn_removed_option(commands_admin_access, api_permissions),
    false;
filter(_Host, ldap_group_cache_size, _, _) ->
    warn_removed_option(ldap_group_cache_size, cache_size),
    false;
filter(_Host, ldap_user_cache_size, _, _) ->
    warn_removed_option(ldap_user_cache_size, cache_size),
    false;
filter(_Host, ldap_group_cache_validity, _, _) ->
    warn_removed_option(ldap_group_cache_validity, cache_life_time),
    false;
filter(_Host, ldap_user_cache_validity, _, _) ->
    warn_removed_option(ldap_user_cache_validity, cache_life_time),
    false;
filter(_Host, ldap_local_filter, _, _) ->
    warn_removed_option(ldap_local_filter),
    false;
filter(_Host, deref_aliases, Val, _) ->
    warn_replaced_option(deref_aliases, ldap_deref_aliases),
    {true, {ldap_deref_aliases, Val}};
filter(_Host, default_db, internal, _) ->
    {true, {default_db, mnesia}};
filter(_Host, default_db, odbc, _) ->
    {true, {default_db, sql}};
filter(_Host, auth_method, Ms, _) ->
    Ms1 = lists:map(
	    fun(internal) -> mnesia;
	       (odbc) -> sql;
	       (M) -> M
	    end, Ms),
    {true, {auth_method, Ms1}};
filter(_Host, default_ram_db, internal, _) ->
    {true, {default_ram_db, mnesia}};
filter(_Host, default_ram_db, odbc, _) ->
    {true, {default_ram_db, sql}};
filter(_Host, extauth_cache, _, _) ->
    ?WARNING_MSG("Option 'extauth_cache' is deprecated "
		 "and has no effect, use authentication "
		 "or global cache configuration options: "
		 "auth_use_cache, auth_cache_life_time, "
		 "use_cache, cache_life_time, and so on", []),
    false;
filter(_Host, extauth_instances, Val, _) ->
    warn_replaced_option(extauth_instances, extauth_pool_size),
    {true, {extauth_pool_size, Val}};
filter(_Host, Opt, Val, _) when Opt == outgoing_s2s_timeout;
				Opt == s2s_dns_timeout ->
    warn_huge_timeout(Opt, Val),
    true;
filter(_Host, captcha_host, _, _) ->
    warn_deprecated_option(captcha_host, captcha_url),
    true;
filter(_Host, route_subdomains, _, _) ->
    warn_removed_option(route_subdomains, s2s_access),
    false;
filter(_Host, auth_password_types_hidden_in_scram1, Val, _) ->
    {true, {auth_password_types_hidden_in_sasl1, Val}};
filter(_Host, new_sql_schema, Val, _) ->
    warn_replaced_option(new_sql_schema, sql_schema_multihost),
    {true, {sql_schema_multihost, Val}};
filter(Host, modules, ModOpts, State) ->
    NoDialbackHosts = maps:get(remove_s2s_dialback, State, []),
    ModOpts1 = lists:filter(
		 fun({mod_s2s_dialback, _}) ->
			 not lists:member(Host, NoDialbackHosts);
		    ({mod_echo, _}) ->
			 warn_removed_module(mod_echo),
			 false;
		    (_) ->
			 true
		 end, ModOpts),
    {true, {modules, ModOpts1}};
filter(_, _, _, _) ->
    true.

%%%===================================================================
%%% Listener transformers
%%%===================================================================
transform_listener(Opts, Acc) ->
    Opts1 = transform_request_handlers(Opts),
    Opts2 = transform_turn_ip(Opts1),
    Opts3 = remove_inet_options(Opts2),
    collect_listener_certfiles(Opts3, Acc).

transform_request_handlers(Opts) ->
    case lists:keyfind(module, 1, Opts) of
	{_, ejabberd_http} ->
	    replace_request_handlers(Opts);
	{_, ejabberd_xmlrpc} ->
	    remove_xmlrpc_access_commands(Opts);
	_ ->
	    Opts
    end.

transform_turn_ip(Opts) ->
    case lists:keyfind(module, 1, Opts) of
	{_, ejabberd_stun} ->
	    replace_turn_ip(Opts);
	_ ->
	    Opts
    end.

replace_request_handlers(Opts) ->
    Handlers = proplists:get_value(request_handlers, Opts, []),
    Handlers1 =
	lists:foldl(
	  fun({captcha, IsEnabled}, Acc) ->
		  Handler = {<<"/captcha">>, ejabberd_captcha},
		  warn_replaced_handler(captcha, Handler, IsEnabled),
		  [Handler|Acc];
	     ({register, IsEnabled}, Acc) ->
		  Handler = {<<"/register">>, mod_register_web},
		  warn_replaced_handler(register, Handler, IsEnabled),
		  [Handler|Acc];
	     ({web_admin, IsEnabled}, Acc) ->
		  Handler = {<<"/admin">>, ejabberd_web_admin},
		  warn_replaced_handler(web_admin, Handler, IsEnabled),
		  [Handler|Acc];
	     ({http_bind, IsEnabled}, Acc) ->
		  Handler = {<<"/bosh">>, mod_bosh},
		  warn_replaced_handler(http_bind, Handler, IsEnabled),
		  [Handler|Acc];
	     ({xmlrpc, IsEnabled}, Acc) ->
		  Handler = {<<"/">>, ejabberd_xmlrpc},
		  warn_replaced_handler(xmlrpc, Handler, IsEnabled),
		  Acc ++ [Handler];
	     (_, Acc) ->
		  Acc
	  end, Handlers, Opts),
    Handlers2 = lists:map(
		  fun({Path, mod_http_bind}) ->
			  warn_replaced_module(mod_http_bind, mod_bosh),
			  {Path, mod_bosh};
		     (PathMod) ->
			  PathMod
		  end, Handlers1),
    Opts1 = lists:filtermap(
	      fun({captcha, _}) -> false;
		 ({register, _}) -> false;
		 ({web_admin, _}) -> false;
		 ({http_bind, _}) -> false;
		 ({xmlrpc, _}) -> false;
		 ({http_poll, _}) ->
		      ?WARNING_MSG("Listening option 'http_poll' is "
				   "ignored: HTTP Polling support was "
				   "removed in ejabberd 15.04. ~ts",
				   [adjust_hint()]),
		      false;
		 ({request_handlers, _}) ->
		      false;
		 (_) -> true
	      end, Opts),
    case Handlers2 of
	[] -> Opts1;
	_ -> [{request_handlers, Handlers2}|Opts1]
    end.

remove_xmlrpc_access_commands(Opts) ->
    lists:filter(
      fun({access_commands, _}) ->
	      warn_removed_option(access_commands, api_permissions),
	      false;
	 (_) ->
	      true
      end, Opts).

replace_turn_ip(Opts) ->
    lists:filtermap(
      fun({turn_ip, Val}) ->
	      warn_replaced_option(turn_ip, turn_ipv4_address),
	      {true, {turn_ipv4_address, Val}};
	 (_) ->
	      true
      end, Opts).

remove_inet_options(Opts) ->
    lists:filter(
      fun({Opt, _}) when Opt == inet; Opt == inet6 ->
	      warn_removed_option(Opt, ip),
	      false;
	 (_) ->
	      true
      end, Opts).

collect_listener_certfiles(Opts, Acc) ->
    Mod = proplists:get_value(module, Opts),
    if Mod == ejabberd_http;
       Mod == ejabberd_c2s;
       Mod == ejabberd_s2s_in ->
	    case lists:keyfind(certfile, 1, Opts) of
		{_, CertFile} ->
		    ?WARNING_MSG("Listening option 'certfile' of module ~ts "
				 "is deprecated and was automatically "
				 "appended to global 'certfiles' option. ~ts",
				 [Mod, adjust_hint()]),
		    CertFiles = maps:get(certfiles, Acc, []),
		    {proplists:delete(certfile, Opts),
		     maps:put(certfiles, [CertFile|CertFiles], Acc)};
		false ->
		    {Opts, Acc}
	    end;
       true ->
	    {Opts, Acc}
    end.

%%%===================================================================
%%% Module transformers
%%% NOTE: transform_module_options/1 is called before transform_module/4
%%%===================================================================
transform_module_options(Opts) ->
    lists:filtermap(
      fun({Opt, internal}) when Opt == db_type;
				Opt == ram_db_type ->
	      {true, {Opt, mnesia}};
	 ({Opt, odbc}) when Opt == db_type;
			    Opt == ram_db_type ->
	      {true, {Opt, sql}};
	 ({deref_aliases, Val}) ->
	      warn_replaced_option(deref_aliases, ldap_deref_aliases),
	      {true, {ldap_deref_aliases, Val}};
	 ({ldap_group_cache_size, _}) ->
	      warn_removed_option(ldap_group_cache_size, cache_size),
	      false;
	 ({ldap_user_cache_size, _}) ->
	      warn_removed_option(ldap_user_cache_size, cache_size),
	      false;
	 ({ldap_group_cache_validity, _}) ->
	      warn_removed_option(ldap_group_cache_validity, cache_life_time),
	      false;
	 ({ldap_user_cache_validity, _}) ->
	      warn_removed_option(ldap_user_cache_validity, cache_life_time),
	      false;
	 ({iqdisc, _}) ->
	      warn_removed_option(iqdisc),
	      false;
	 (_) ->
	      true
      end, Opts).

transform_module(Host, mod_http_bind, Opts, Acc) ->
    warn_replaced_module(mod_http_bind, mod_bosh),
    transform_module(Host, mod_bosh, Opts, Acc);
transform_module(Host, mod_vcard_xupdate_odbc, Opts, Acc) ->
    warn_replaced_module(mod_vcard_xupdate_odbc, mod_vcard_xupdate),
    transform_module(Host, mod_vcard_xupdate, Opts, Acc);
transform_module(Host, mod_vcard_ldap, Opts, Acc) ->
    warn_replaced_module(mod_vcard_ldap, mod_vcard, ldap),
    transform_module(Host, mod_vcard, [{db_type, ldap}|Opts], Acc);
transform_module(Host, M, Opts, Acc) when (M == mod_announce_odbc orelse
					   M == mod_blocking_odbc orelse
					   M == mod_caps_odbc orelse
					   M == mod_last_odbc orelse
					   M == mod_muc_odbc orelse
					   M == mod_offline_odbc orelse
					   M == mod_privacy_odbc orelse
					   M == mod_private_odbc orelse
					   M == mod_pubsub_odbc orelse
					   M == mod_roster_odbc orelse
					   M == mod_shared_roster_odbc orelse
					   M == mod_vcard_odbc) ->
    M1 = strip_odbc_suffix(M),
    warn_replaced_module(M, M1, sql),
    transform_module(Host, M1, [{db_type, sql}|Opts], Acc);
transform_module(_Host, mod_blocking, Opts, Acc) ->
    Opts1 = lists:filter(
	      fun({db_type, _}) ->
		      warn_removed_module_option(db_type, mod_blocking),
		      false;
		 (_) ->
		      true
	      end, Opts),
    {{mod_blocking, Opts1}, Acc};
transform_module(_Host, mod_carboncopy, Opts, Acc) ->
    Opts1 = lists:filter(
	      fun({Opt, _}) when Opt == ram_db_type;
				 Opt == use_cache;
				 Opt == cache_size;
				 Opt == cache_missed;
				 Opt == cache_life_time ->
		      warn_removed_module_option(Opt, mod_carboncopy),
		      false;
		 (_) ->
		      true
	      end, Opts),
    {{mod_carboncopy, Opts1}, Acc};
transform_module(_Host, mod_http_api, Opts, Acc) ->
    Opts1 = lists:filter(
	      fun({admin_ip_access, _}) ->
		      warn_removed_option(admin_ip_access, api_permissions),
		      false;
		 (_) ->
		      true
	      end, Opts),
    {{mod_http_api, Opts1}, Acc};
transform_module(_Host, mod_http_upload, Opts, Acc) ->
    Opts1 = lists:filter(
	      fun({service_url, _}) ->
		      warn_deprecated_option(service_url, external_secret),
		      true;
		 (_) ->
		      true
	      end, Opts),
    {{mod_http_upload, Opts1}, Acc};
transform_module(_Host, mod_pubsub, Opts, Acc) ->
    Opts1 = lists:map(
	      fun({plugins, Plugins}) ->
		      {plugins,
		       lists:filter(
			 fun(Plugin) ->
				 case lists:member(
					Plugin,
					[<<"buddy">>, <<"club">>, <<"dag">>,
					 <<"dispatch">>, <<"hometree">>, <<"mb">>,
					 <<"mix">>, <<"online">>, <<"private">>,
					 <<"public">>]) of
				     true ->
					 ?WARNING_MSG(
					    "Plugin '~ts' of mod_pubsub is not "
					    "supported anymore and has been "
					    "automatically removed from 'plugins' "
					    "option. ~ts",
					    [Plugin, adjust_hint()]),
					 false;
				     false ->
					 true
				 end
			 end, Plugins)};
		 (Opt) ->
		      Opt
	      end, Opts),
    {{mod_pubsub, Opts1}, Acc};
transform_module(_Host, Mod, Opts, Acc) ->
    {{Mod, Opts}, Acc}.

strip_odbc_suffix(M) ->
    [_|T] = lists:reverse(string:tokens(atom_to_list(M), "_")),
    list_to_atom(string:join(lists:reverse(T), "_")).

%%%===================================================================
%%% Aux
%%%===================================================================
filtermapfoldr(Fun, Init, List) ->
    lists:foldr(
      fun(X, {Ret, Acc}) ->
	      case Fun(X, Acc) of
		  {true, Acc1} -> {[X|Ret], Acc1};
		  {{true, X1}, Acc1} -> {[X1|Ret], Acc1};
		  {false, Acc1} -> {Ret, Acc1}
	      end
      end, {[], Init}, List).

set_certfiles(Y, #{certfiles := CertFiles} = Acc) ->
    {lists:keystore(certfiles, 1, Y, {certfiles, CertFiles}), Acc};
set_certfiles(Y, Acc) ->
    {Y, Acc}.

%%%===================================================================
%%% Warnings
%%%===================================================================
warn_replaced_module(From, To) ->
    ?WARNING_MSG("Module ~ts is deprecated and was automatically "
		 "replaced by ~ts. ~ts",
		 [From, To, adjust_hint()]).

warn_replaced_module(From, To, Type) ->
    ?WARNING_MSG("Module ~ts is deprecated and was automatically "
		 "replaced by ~ts with db_type: ~ts. ~ts",
		 [From, To, Type, adjust_hint()]).

warn_removed_module(Mod) ->
    ?WARNING_MSG("Module ~ts is deprecated and was automatically "
		 "removed from the configuration. ~ts", [Mod, adjust_hint()]).

warn_replaced_handler(Opt, {Path, Module}, false) ->
    ?WARNING_MSG("Listening option '~ts' is deprecated, "
		 "please use instead the "
		 "HTTP request handler: \"~ts\" -> ~ts. ~ts",
		 [Opt, Path, Module, adjust_hint()]);
warn_replaced_handler(Opt, {Path, Module}, true) ->
    ?WARNING_MSG("Listening option '~ts' is deprecated "
		 "and was automatically replaced by "
		 "HTTP request handler: \"~ts\" -> ~ts. ~ts",
		 [Opt, Path, Module, adjust_hint()]).

warn_deprecated_option(OldOpt, NewOpt) ->
    ?WARNING_MSG("Option '~ts' is deprecated. Use option '~ts' instead.",
		 [OldOpt, NewOpt]).

warn_replaced_option(OldOpt, NewOpt) ->
    ?WARNING_MSG("Option '~ts' is deprecated and was automatically "
		 "replaced by '~ts'. ~ts",
		 [OldOpt, NewOpt, adjust_hint()]).

warn_removed_option(Opt) ->
    ?WARNING_MSG("Option '~ts' is deprecated and has no effect anymore. "
		 "Please remove it from the configuration.", [Opt]).

warn_removed_option(OldOpt, NewOpt) ->
    ?WARNING_MSG("Option '~ts' is deprecated and has no effect anymore. "
		 "Use option '~ts' instead.", [OldOpt, NewOpt]).

warn_removed_module_option(Opt, Mod) ->
    ?WARNING_MSG("Option '~ts' of module ~ts is deprecated "
		 "and has no effect anymore. ~ts",
		 [Opt, Mod, adjust_hint()]).

warn_huge_timeout(Opt, T) when is_integer(T), T >= 1000 ->
    ?WARNING_MSG("Value '~B' of option '~ts' is too big, "
		 "are you sure you have set seconds?",
		 [T, Opt]);
warn_huge_timeout(_, _) ->
    ok.

adjust_hint() ->
    "Please adjust your configuration file accordingly. "
    "Hint: run `ejabberdctl dump-config` command to view current "
    "configuration as it is seen by ejabberd.".

%%%===================================================================
%%% Very raw validator: just to make sure we get properly typed terms
%%% Expand it if you need to transform more options, but don't
%%% abuse complex types: simple and composite types are preferred
%%%===================================================================
validator() ->
    Validators =
	#{s2s_use_starttls => econf:atom(),
	  certfiles => econf:list(econf:any()),
	  c2s_certfile => econf:binary(),
	  s2s_certfile => econf:binary(),
	  domain_certfile => econf:binary(),
	  default_db => econf:atom(),
	  default_ram_db => econf:atom(),
	  auth_method => econf:list_or_single(econf:atom()),
	  acme => econf:options(
		    #{ca_url => econf:binary(),
		      '_' => econf:any()},
		    [unique]),
	  listen =>
	      econf:list(
		econf:options(
		  #{captcha => econf:bool(),
		    register => econf:bool(),
		    web_admin => econf:bool(),
		    http_bind => econf:bool(),
		    http_poll => econf:bool(),
		    xmlrpc => econf:bool(),
		    module => econf:atom(),
		    certfile => econf:binary(),
		    request_handlers =>
			econf:map(econf:binary(), econf:atom()),
		    '_' => econf:any()},
		  [])),
	  modules =>
	      econf:options(
		#{'_' =>
		      econf:options(
			#{db_type => econf:atom(),
			  plugins => econf:list(econf:binary()),
			  '_' => econf:any()},
			[])},
		[]),
	  '_' => econf:any()},
    econf:options(
      Validators#{host_config =>
		      econf:map(econf:binary(),
				econf:options(Validators, [])),
		  append_host_config =>
		      econf:map(econf:binary(),
				econf:options(Validators, []))},
      []).
