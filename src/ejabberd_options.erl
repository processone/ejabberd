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
-module(ejabberd_options).
-behaviour(ejabberd_config).

-export([opt_type/1, options/0, globals/0, doc/0]).

-ifdef(NEW_SQL_SCHEMA).
-define(USE_NEW_SQL_SCHEMA_DEFAULT, true).
-else.
-define(USE_NEW_SQL_SCHEMA_DEFAULT, false).
-endif.

-include_lib("kernel/include/inet.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec opt_type(atom()) -> econf:validator().
opt_type(access_rules) ->
    acl:validator(access_rules);
opt_type(acl) ->
    acl:validator(acl);
opt_type(acme) ->
    econf:options(
      #{ca_url => econf:url(),
	contact => econf:list_or_single(econf:binary("^[a-zA-Z]+:[^:]+$")),
	auto => econf:bool(),
	cert_type => econf:enum([ec, rsa])},
      [unique, {return, map}]);
opt_type(allow_contrib_modules) ->
    econf:bool();
opt_type(allow_multiple_connections) ->
    econf:bool();
opt_type(anonymous_protocol) ->
    econf:enum([sasl_anon, login_anon, both]);
opt_type(api_permissions) ->
    ejabberd_access_permissions:validator();
opt_type(append_host_config) ->
    opt_type(host_config);
opt_type(auth_cache_life_time) ->
    econf:timeout(second, infinity);
opt_type(auth_cache_missed) ->
    econf:bool();
opt_type(auth_cache_size) ->
    econf:pos_int(infinity);
opt_type(auth_method) ->
    econf:list_or_single(econf:db_type(ejabberd_auth));
opt_type(auth_opts) ->
    fun(L) when is_list(L) ->
            lists:map(
              fun({host, V}) when is_binary(V) ->
                      {host, V};
                 ({connection_pool_size, V}) when is_integer(V) ->
                      {connection_pool_size, V};
                 ({connection_opts, V}) when is_list(V) ->
                      {connection_opts, V};
                 ({basic_auth, V}) when is_binary(V) ->
                      {basic_auth, V};
                 ({path_prefix, V}) when is_binary(V) ->
                      {path_prefix, V}
              end, L)
    end;
opt_type(auth_password_format) ->
    econf:enum([plain, scram]);
opt_type(auth_scram_hash) ->
    econf:enum([sha, sha256, sha512]);
opt_type(auth_external_user_exists_check) ->
    econf:bool();
opt_type(auth_use_cache) ->
    econf:bool();
opt_type(c2s_cafile) ->
    econf:file();
opt_type(c2s_ciphers) ->
    fun(L) when is_list(L) ->
            (econf:and_then(
               econf:list(econf:binary(), [unique]),
               concat_binary($:)))(L);
       (B) ->
            (econf:binary())(B)
    end;
opt_type(c2s_dhfile) ->
    econf:file();
opt_type(c2s_protocol_options) ->
    econf:and_then(
      econf:list(econf:binary(), [unique]),
      concat_binary($|));
opt_type(c2s_tls_compression) ->
    econf:bool();
opt_type(ca_file) ->
    econf:pem();
opt_type(cache_life_time) ->
    econf:timeout(second, infinity);
opt_type(cache_missed) ->
    econf:bool();
opt_type(cache_size) ->
    econf:pos_int(infinity);
opt_type(captcha_cmd) ->
    econf:and_then(
	econf:binary(),
	fun(V) ->
		V2 = misc:expand_keyword(<<"@SEMVER@">>, V,
				    ejabberd_option:version()),
		misc:expand_keyword(<<"@VERSION@">>, V2,
				    misc:semver_to_xxyy(ejabberd_option:version()))
	end);
opt_type(captcha_host) ->
    econf:binary();
opt_type(captcha_limit) ->
    econf:pos_int(infinity);
opt_type(captcha_url) ->
    econf:either(
      econf:url(),
      econf:enum([auto, undefined]));
opt_type(certfiles) ->
    econf:list(econf:binary());
opt_type(cluster_backend) ->
    econf:db_type(ejabberd_cluster);
opt_type(cluster_nodes) ->
    econf:list(econf:atom(), [unique]);
opt_type(default_db) ->
    econf:enum([mnesia, sql]);
opt_type(default_ram_db) ->
    econf:enum([mnesia, sql, redis]);
opt_type(define_macro) ->
    econf:any();
opt_type(disable_sasl_scram_downgrade_protection) ->
    econf:bool();
opt_type(disable_sasl_mechanisms) ->
    econf:list_or_single(
      econf:and_then(
	econf:binary(),
	fun str:to_upper/1));
opt_type(domain_balancing) ->
    econf:map(
      econf:domain(),
      econf:options(
	#{component_number => econf:int(2, 1000),
	  type => econf:enum([random, source, destination,
			      bare_source, bare_destination])},
	[{return, map}, unique]),
      [{return, map}]);
opt_type(ext_api_path_oauth) ->
    econf:binary();
opt_type(ext_api_http_pool_size) ->
    econf:pos_int();
opt_type(ext_api_url) ->
    econf:url();
opt_type(ext_api_headers) ->
    econf:binary();
opt_type(extauth_pool_name) ->
    econf:binary();
opt_type(extauth_pool_size) ->
    econf:pos_int();
opt_type(extauth_program) ->
    econf:string();
opt_type(fqdn) ->
    econf:list_or_single(econf:domain());
opt_type(hide_sensitive_log_data) ->
    econf:bool();
opt_type(host_config) ->
    econf:and_then(
      econf:and_then(
        econf:map(econf:domain(), econf:list(econf:any())),
        fun econf:group_dups/1),
      econf:map(
	econf:enum(ejabberd_config:get_option(hosts)),
        validator(),
        [unique]));
opt_type(hosts) ->
    econf:non_empty(econf:list(econf:domain(), [unique]));
opt_type(include_config_file) ->
    econf:any();
opt_type(install_contrib_modules) ->
    econf:list(econf:atom());
opt_type(language) ->
    econf:lang();
opt_type(ldap_backups) ->
    econf:list(econf:domain(), [unique]);
opt_type(ldap_base) ->
    econf:binary();
opt_type(ldap_deref_aliases) ->
    econf:enum([never, searching, finding, always]);
opt_type(ldap_dn_filter) ->
    econf:and_then(
      econf:non_empty(
	econf:map(
	  econf:ldap_filter(),
	  econf:list(econf:binary()))),
      fun hd/1);
opt_type(ldap_encrypt) ->
    econf:enum([tls, starttls, none]);
opt_type(ldap_filter) ->
    econf:ldap_filter();
opt_type(ldap_password) ->
    econf:binary();
opt_type(ldap_port) ->
    econf:port();
opt_type(ldap_rootdn) ->
    econf:binary();
opt_type(ldap_servers) ->
    econf:list(econf:domain(), [unique]);
opt_type(ldap_tls_cacertfile) ->
    econf:pem();
opt_type(ldap_tls_certfile) ->
    econf:pem();
opt_type(ldap_tls_depth) ->
    econf:non_neg_int();
opt_type(ldap_tls_verify) ->
    econf:enum([hard, soft, false]);
opt_type(ldap_uids) ->
    econf:either(
      econf:list(
	econf:and_then(
	  econf:binary(),
	  fun(U) -> {U, <<"%u">>} end)),
      econf:map(econf:binary(), econf:binary(), [unique]));
opt_type(listen) ->
    ejabberd_listener:validator();
opt_type(log_rotate_count) ->
    econf:non_neg_int();
opt_type(log_rotate_size) ->
    econf:pos_int(infinity);
opt_type(log_burst_limit_window_time) ->
    econf:timeout(second);
opt_type(log_burst_limit_count) ->
    econf:pos_int();
opt_type(log_modules_fully) ->
    econf:list(econf:atom());
opt_type(loglevel) ->
    fun(N) when is_integer(N) ->
	    (econf:and_then(
	       econf:int(0, 5),
	       fun ejabberd_logger:convert_loglevel/1))(N);
       (Level) ->
	    (econf:enum([none, emergency, alert, critical,
			 error, warning, notice, info, debug]))(Level)
    end;
opt_type(max_fsm_queue) ->
    econf:pos_int();
opt_type(modules) ->
    econf:map(econf:atom(), econf:any());
opt_type(negotiation_timeout) ->
    econf:timeout(second);
opt_type(net_ticktime) ->
    econf:timeout(second);
opt_type(new_sql_schema) ->
    econf:bool();
opt_type(update_sql_schema) ->
    econf:bool();
opt_type(update_sql_schema_timeout) ->
    econf:timeout(second, infinity);
opt_type(oauth_access) ->
    econf:acl();
opt_type(oauth_cache_life_time) ->
    econf:timeout(second, infinity);
opt_type(oauth_cache_missed) ->
    econf:bool();
opt_type(oauth_cache_rest_failure_life_time) ->
    econf:timeout(second, infinity);
opt_type(oauth_cache_size) ->
    econf:pos_int(infinity);
opt_type(oauth_db_type) ->
    econf:db_type(ejabberd_oauth);
opt_type(oauth_expire) ->
    econf:timeout(second);
opt_type(oauth_use_cache) ->
    econf:bool();
opt_type(oauth_client_id_check) ->
    econf:enum([allow, deny, db]);
opt_type(oom_killer) ->
    econf:bool();
opt_type(oom_queue) ->
    econf:pos_int();
opt_type(oom_watermark) ->
    econf:int(1, 99);
opt_type(outgoing_s2s_families) ->
    econf:and_then(
      econf:non_empty(
	econf:list(econf:enum([ipv4, ipv6]), [unique])),
      fun(L) ->
	      lists:map(
		fun(ipv4) -> inet;
		   (ipv6) -> inet6
		end, L)
      end);
opt_type(outgoing_s2s_ipv4_address) ->
    econf:ipv4();
opt_type(outgoing_s2s_ipv6_address) ->
    econf:ipv6();
opt_type(outgoing_s2s_port) ->
    econf:port();
opt_type(outgoing_s2s_timeout) ->
    econf:timeout(second, infinity);
opt_type(pam_service) ->
    econf:binary();
opt_type(pam_userinfotype) ->
    econf:enum([username, jid]);
opt_type(pgsql_users_number_estimate) ->
    econf:bool();
opt_type(queue_dir) ->
    econf:directory(write);
opt_type(queue_type) ->
    econf:enum([ram, file]);
opt_type(redis_connect_timeout) ->
    econf:timeout(second);
opt_type(redis_db) ->
    econf:non_neg_int();
opt_type(redis_password) ->
    econf:string();
opt_type(redis_pool_size) ->
    econf:pos_int();
opt_type(redis_port) ->
    econf:port();
opt_type(redis_queue_type) ->
    econf:enum([ram, file]);
opt_type(redis_server) ->
    econf:string();
opt_type(registration_timeout) ->
    econf:timeout(second, infinity);
opt_type(resource_conflict) ->
    econf:enum([setresource, closeold, closenew, acceptnew]);
opt_type(router_cache_life_time) ->
    econf:timeout(second, infinity);
opt_type(router_cache_missed) ->
    econf:bool();
opt_type(router_cache_size) ->
    econf:pos_int(infinity);
opt_type(router_db_type) ->
    econf:db_type(ejabberd_router);
opt_type(router_use_cache) ->
    econf:bool();
opt_type(rpc_timeout) ->
    econf:timeout(second);
opt_type(s2s_access) ->
    econf:acl();
opt_type(s2s_cafile) ->
    econf:pem();
opt_type(s2s_ciphers) ->
    opt_type(c2s_ciphers);
opt_type(s2s_dhfile) ->
    econf:file();
opt_type(s2s_dns_retries) ->
    econf:non_neg_int();
opt_type(s2s_dns_timeout) ->
    econf:timeout(second, infinity);
opt_type(s2s_max_retry_delay) ->
    econf:timeout(second);
opt_type(s2s_protocol_options) ->
    opt_type(c2s_protocol_options);
opt_type(s2s_queue_type) ->
    econf:enum([ram, file]);
opt_type(s2s_timeout) ->
    econf:timeout(second, infinity);
opt_type(s2s_tls_compression) ->
    econf:bool();
opt_type(s2s_use_starttls) ->
    econf:either(
      econf:bool(),
      econf:enum([optional, required]));
opt_type(s2s_zlib) ->
    econf:and_then(
      econf:bool(),
      fun(false) -> false;
	 (true) ->
	      ejabberd:start_app(ezlib),
	      true
      end);
opt_type(shaper) ->
    ejabberd_shaper:validator(shaper);
opt_type(shaper_rules) ->
    ejabberd_shaper:validator(shaper_rules);
opt_type(sm_cache_life_time) ->
    econf:timeout(second, infinity);
opt_type(sm_cache_missed) ->
    econf:bool();
opt_type(sm_cache_size) ->
    econf:pos_int(infinity);
opt_type(sm_db_type) ->
    econf:db_type(ejabberd_sm);
opt_type(sm_use_cache) ->
    econf:bool();
opt_type(sql_connect_timeout) ->
    econf:timeout(second);
opt_type(sql_database) ->
    econf:binary();
opt_type(sql_keepalive_interval) ->
    econf:timeout(second);
opt_type(sql_password) ->
    econf:binary();
opt_type(sql_odbc_driver) ->
    econf:binary();
opt_type(sql_pool_size) ->
    econf:pos_int();
opt_type(sql_port) ->
    econf:port();
opt_type(sql_query_timeout) ->
    econf:timeout(second);
opt_type(sql_queue_type) ->
    econf:enum([ram, file]);
opt_type(sql_server) ->
    econf:binary();
opt_type(sql_ssl) ->
    econf:bool();
opt_type(sql_ssl_cafile) ->
    econf:pem();
opt_type(sql_ssl_certfile) ->
    econf:pem();
opt_type(sql_ssl_verify) ->
    econf:bool();
opt_type(sql_start_interval) ->
    econf:timeout(second);
opt_type(sql_type) ->
    econf:enum([mysql, pgsql, sqlite, mssql, odbc]);
opt_type(sql_username) ->
    econf:binary();
opt_type(sql_prepared_statements) ->
    econf:bool();
opt_type(sql_flags) ->
    econf:list_or_single(econf:enum([mysql_alternative_upsert]), [sorted, unique]);
opt_type(trusted_proxies) ->
    econf:either(all, econf:list(econf:ip_mask()));
opt_type(use_cache) ->
    econf:bool();
opt_type(validate_stream) ->
    econf:bool();
opt_type(version) ->
    econf:binary();
opt_type(websocket_origin) ->
    econf:list(
      econf:and_then(
	econf:and_then(
	  econf:binary_sep("\\s+"),
	  econf:list(econf:url(), [unique])),
	fun(L) -> str:join(L, <<" ">>) end),
      [unique]);
opt_type(websocket_ping_interval) ->
    econf:timeout(second);
opt_type(websocket_timeout) ->
    econf:timeout(second);
opt_type(jwt_key) ->
    econf:and_then(
      econf:path(),
      fun(Path) ->
              case file:read_file(Path) of
                  {ok, Data} ->
		      try jose_jwk:from_binary(Data) of
			  {error, _} -> econf:fail({bad_jwt_key, Path});
			  JWK ->
                              case jose_jwk:to_map(JWK) of
                                  {_, #{<<"keys">> := [Key]}} ->
                                      jose_jwk:from_map(Key);
                                  {_, #{<<"keys">> := [_|_]}} ->
                                      econf:fail({bad_jwt_key_set, Path});
				  {_, #{<<"keys">> := _}} ->
				      econf:fail({bad_jwt_key, Path});
                                  _ ->
                                      JWK
                              end
		      catch _:_ ->
			      econf:fail({bad_jwt_key, Path})
		      end;
                  {error, Reason} ->
                      econf:fail({read_file, Reason, Path})
              end
      end);
opt_type(jwt_jid_field) ->
    econf:binary();
opt_type(jwt_auth_only_rule) ->
    econf:atom().

%% We only define the types of options that cannot be derived
%% automatically by tools/opt_type.sh script
-spec options() -> [{s2s_protocol_options, undefined | binary()} |
		    {c2s_protocol_options, undefined | binary()} |
                    {s2s_ciphers, undefined | binary()} |
                    {c2s_ciphers, undefined | binary()} |
		    {websocket_origin, [binary()]} |
		    {disable_sasl_mechanisms, [binary()]} |
		    {s2s_zlib, boolean()} |
		    {loglevel, ejabberd_logger:loglevel()} |
		    {auth_opts, [{any(), any()}]} |
		    {listen, [ejabberd_listener:listener()]} |
		    {modules, [{module(), gen_mod:opts(), integer()}]} |
		    {ldap_uids, [{binary(), binary()}]} |
		    {ldap_dn_filter, {binary(), [binary()]}} |
		    {outgoing_s2s_families, [inet | inet6, ...]} |
		    {acl, [{atom(), [acl:acl_rule()]}]} |
		    {access_rules, [{atom(), acl:access()}]} |
		    {shaper, #{atom() => ejabberd_shaper:shaper_rate()}} |
		    {shaper_rules, [{atom(), [ejabberd_shaper:shaper_rule()]}]} |
		    {api_permissions, [ejabberd_access_permissions:permission()]} |
		    {jwt_key, jose_jwk:key() | undefined} |
		    {append_host_config, [{binary(), any()}]} |
		    {host_config, [{binary(), any()}]} |
		    {define_macro, any()} |
		    {include_config_file, any()} |
		    {atom(), any()}].
options() ->
    [%% Top-priority options
     hosts,
     {loglevel, info},
     {cache_life_time, timer:seconds(3600)},
     {cache_missed, true},
     {cache_size, 1000},
     {use_cache, true},
     {default_db, mnesia},
     {default_ram_db, mnesia},
     {queue_type, ram},
     {version, ejabberd_config:version()},
     %% Other options
     {acl, []},
     {access_rules, []},
     {acme, #{}},
     {allow_contrib_modules, true},
     {install_contrib_modules, []},
     {allow_multiple_connections, false},
     {anonymous_protocol, sasl_anon},
     {api_permissions,
      [{<<"admin access">>,
	{[],
	 [{acl, admin},
	  {oauth, {[<<"ejabberd:admin">>], [{acl, admin}]}}],
	 {all, [start, stop]}}}]},
     {append_host_config, []},
     {auth_cache_life_time,
      fun(Host) -> ejabberd_config:get_option({cache_life_time, Host}) end},
     {auth_cache_missed,
      fun(Host) -> ejabberd_config:get_option({cache_missed, Host}) end},
     {auth_cache_size,
      fun(Host) -> ejabberd_config:get_option({cache_size, Host}) end},
     {auth_method,
      fun(Host) -> [ejabberd_config:default_db(Host, ejabberd_auth)] end},
     {auth_opts, []},
     {auth_password_format, plain},
     {auth_scram_hash, sha},
     {auth_external_user_exists_check, true},
     {auth_use_cache,
      fun(Host) -> ejabberd_config:get_option({use_cache, Host}) end},
     {c2s_cafile, undefined},
     {c2s_ciphers, undefined},
     {c2s_dhfile, undefined},
     {c2s_protocol_options, undefined},
     {c2s_tls_compression, undefined},
     {ca_file, iolist_to_binary(pkix:get_cafile())},
     {captcha_cmd, undefined},
     {captcha_host, <<"">>},
     {captcha_limit, infinity},
     {captcha_url, auto},
     {certfiles, undefined},
     {cluster_backend, mnesia},
     {cluster_nodes, []},
     {define_macro, []},
     {disable_sasl_scram_downgrade_protection, false},
     {disable_sasl_mechanisms, []},
     {domain_balancing, #{}},
     {ext_api_headers, <<>>},
     {ext_api_http_pool_size, 100},
     {ext_api_path_oauth, <<"/oauth">>},
     {ext_api_url, <<"http://localhost/api">>},
     {extauth_pool_name, undefined},
     {extauth_pool_size, undefined},
     {extauth_program, undefined},
     {fqdn, fun fqdn/1},
     {hide_sensitive_log_data, false},
     {host_config, []},
     {include_config_file, []},
     {language, <<"en">>},
     {ldap_backups, []},
     {ldap_base, <<"">>},
     {ldap_deref_aliases, never},
     {ldap_dn_filter, {undefined, []}},
     {ldap_encrypt, none},
     {ldap_filter, <<"">>},
     {ldap_password, <<"">>},
     {ldap_port,
      fun(Host) ->
	      case ejabberd_config:get_option({ldap_encrypt, Host}) of
		  tls -> 636;
		  _ -> 389
	      end
      end},
     {ldap_rootdn, <<"">>},
     {ldap_servers, [<<"localhost">>]},
     {ldap_tls_cacertfile, undefined},
     {ldap_tls_certfile, undefined},
     {ldap_tls_depth, undefined},
     {ldap_tls_verify, false},
     {ldap_uids, [{<<"uid">>, <<"%u">>}]},
     {listen, []},
     {log_rotate_count, 1},
     {log_rotate_size, 10*1024*1024},
     {log_burst_limit_window_time, timer:seconds(1)},
     {log_burst_limit_count, 500},
     {log_modules_fully, []},
     {max_fsm_queue, undefined},
     {modules, []},
     {negotiation_timeout, timer:seconds(120)},
     {net_ticktime, timer:seconds(60)},
     {new_sql_schema, ?USE_NEW_SQL_SCHEMA_DEFAULT},
     {update_sql_schema, true},
     {update_sql_schema_timeout, timer:minutes(5)},
     {oauth_access, none},
     {oauth_cache_life_time,
      fun(Host) -> ejabberd_config:get_option({cache_life_time, Host}) end},
     {oauth_cache_missed,
      fun(Host) -> ejabberd_config:get_option({cache_missed, Host}) end},
     {oauth_cache_size,
      fun(Host) -> ejabberd_config:get_option({cache_size, Host}) end},
     {oauth_cache_rest_failure_life_time, infinity},
     {oauth_db_type,
      fun(Host) -> ejabberd_config:default_db(Host, ejabberd_oauth) end},
     {oauth_expire, 4294967},
     {oauth_use_cache,
      fun(Host) -> ejabberd_config:get_option({use_cache, Host}) end},
     {oauth_client_id_check, allow},
     {oom_killer, true},
     {oom_queue, 10000},
     {oom_watermark, 80},
     {outgoing_s2s_families, [inet6, inet]},
     {outgoing_s2s_ipv4_address, undefined},
     {outgoing_s2s_ipv6_address, undefined},
     {outgoing_s2s_port, 5269},
     {outgoing_s2s_timeout, timer:seconds(10)},
     {pam_service, <<"ejabberd">>},
     {pam_userinfotype, username},
     {pgsql_users_number_estimate, false},
     {queue_dir, undefined},
     {redis_connect_timeout, timer:seconds(1)},
     {redis_db, 0},
     {redis_password, ""},
     {redis_pool_size, 10},
     {redis_port, 6379},
     {redis_queue_type,
      fun(Host) -> ejabberd_config:get_option({queue_type, Host}) end},
     {redis_server, "localhost"},
     {registration_timeout, timer:seconds(600)},
     {resource_conflict, acceptnew},
     {router_cache_life_time,
      fun(Host) -> ejabberd_config:get_option({cache_life_time, Host}) end},
     {router_cache_missed,
      fun(Host) -> ejabberd_config:get_option({cache_missed, Host}) end},
     {router_cache_size,
      fun(Host) -> ejabberd_config:get_option({cache_size, Host}) end},
     {router_db_type,
      fun(Host) -> ejabberd_config:default_ram_db(Host, ejabberd_router) end},
     {router_use_cache,
      fun(Host) -> ejabberd_config:get_option({use_cache, Host}) end},
     {rpc_timeout, timer:seconds(5)},
     {s2s_access, all},
     {s2s_cafile, undefined},
     {s2s_ciphers, undefined},
     {s2s_dhfile, undefined},
     {s2s_dns_retries, 2},
     {s2s_dns_timeout, timer:seconds(10)},
     {s2s_max_retry_delay, timer:seconds(300)},
     {s2s_protocol_options, undefined},
     {s2s_queue_type,
      fun(Host) -> ejabberd_config:get_option({queue_type, Host}) end},
     {s2s_timeout, timer:hours(1)},
     {s2s_tls_compression, undefined},
     {s2s_use_starttls, false},
     {s2s_zlib, false},
     {shaper, #{}},
     {shaper_rules, []},
     {sm_cache_life_time,
      fun(Host) -> ejabberd_config:get_option({cache_life_time, Host}) end},
     {sm_cache_missed,
      fun(Host) -> ejabberd_config:get_option({cache_missed, Host}) end},
     {sm_cache_size,
      fun(Host) -> ejabberd_config:get_option({cache_size, Host}) end},
     {sm_db_type,
      fun(Host) -> ejabberd_config:default_ram_db(Host, ejabberd_sm) end},
     {sm_use_cache,
      fun(Host) -> ejabberd_config:get_option({use_cache, Host}) end},
     {sql_type, odbc},
     {sql_connect_timeout, timer:seconds(5)},
     {sql_database, undefined},
     {sql_keepalive_interval, undefined},
     {sql_password, <<"">>},
     {sql_odbc_driver, <<"libtdsodbc.so">>}, % default is FreeTDS driver
     {sql_pool_size,
      fun(Host) ->
	      case ejabberd_config:get_option({sql_type, Host}) of
		  sqlite -> 1;
		  _ -> 10
	      end
      end},
     {sql_port,
      fun(Host) ->
	      case ejabberd_config:get_option({sql_type, Host}) of
		  mssql -> 1433;
		  mysql -> 3306;
		  pgsql -> 5432;
		  _ -> undefined
	      end
      end},
     {sql_query_timeout, timer:seconds(60)},
     {sql_queue_type,
      fun(Host) -> ejabberd_config:get_option({queue_type, Host}) end},
     {sql_server, <<"localhost">>},
     {sql_ssl, false},
     {sql_ssl_cafile, undefined},
     {sql_ssl_certfile, undefined},
     {sql_ssl_verify, false},
     {sql_start_interval, timer:seconds(30)},
     {sql_username, <<"ejabberd">>},
     {sql_prepared_statements, true},
     {sql_flags, []},
     {trusted_proxies, []},
     {validate_stream, false},
     {websocket_origin, []},
     {websocket_ping_interval, timer:seconds(60)},
     {websocket_timeout, timer:minutes(5)},
     {jwt_key, undefined},
     {jwt_jid_field, <<"jid">>},
     {jwt_auth_only_rule, none}].

-spec globals() -> [atom()].
globals() ->
    [acme,
     allow_contrib_modules,
     api_permissions,
     append_host_config,
     auth_cache_life_time,
     auth_cache_missed,
     auth_cache_size,
     ca_file,
     captcha_cmd,
     captcha_host,
     captcha_limit,
     captcha_url,
     certfiles,
     cluster_backend,
     cluster_nodes,
     domain_balancing,
     ext_api_path_oauth,
     fqdn,
     hosts,
     host_config,
     install_contrib_modules,
     listen,
     loglevel,
     log_rotate_count,
     log_rotate_size,
     log_burst_limit_count,
     log_burst_limit_window_time,
     log_modules_fully,
     negotiation_timeout,
     net_ticktime,
     new_sql_schema,
     update_sql_schema,
     node_start,
     oauth_cache_life_time,
     oauth_cache_missed,
     oauth_cache_size,
     oauth_cache_rest_failure_life_time,
     oauth_db_type,
     oauth_expire,
     oauth_use_cache,
     oom_killer,
     oom_queue,
     oom_watermark,
     queue_dir,
     redis_connect_timeout,
     redis_db,
     redis_password,
     redis_pool_size,
     redis_port,
     redis_queue_type,
     redis_server,
     registration_timeout,
     router_cache_life_time,
     router_cache_missed,
     router_cache_size,
     router_db_type,
     router_use_cache,
     rpc_timeout,
     s2s_max_retry_delay,
     shaper,
     sm_cache_life_time,
     sm_cache_missed,
     sm_cache_size,
     trusted_proxies,
     validate_stream,
     version,
     websocket_origin,
     websocket_ping_interval,
     websocket_timeout].

doc() ->
    ejabberd_options_doc:doc().

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec validator() -> econf:validator().
validator() ->
    Disallowed = ejabberd_config:globals(),
    {Validators, Required} = ejabberd_config:validators(Disallowed),
    econf:and_then(
      fun econf:group_dups/1,
      econf:options(
        Validators,
        [{disallowed, Required ++ Disallowed}, unique])).

-spec fqdn(global | binary()) -> [binary()].
fqdn(global) ->
    {ok, Hostname} = inet:gethostname(),
    case inet:gethostbyname(Hostname) of
	{ok, #hostent{h_name = FQDN}} ->
	    case jid:nameprep(iolist_to_binary(FQDN)) of
		error -> [];
		Domain -> [Domain]
	    end;
	{error, _} ->
	    []
    end;
fqdn(_) ->
    ejabberd_config:get_option(fqdn).

-spec concat_binary(char()) -> fun(([binary()]) -> binary()).
concat_binary(C) ->
    fun(Opts) -> str:join(Opts, <<C>>) end.
