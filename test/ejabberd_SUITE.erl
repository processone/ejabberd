%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  2 Jun 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-module(ejabberd_SUITE).
-compile(export_all).

-import(suite, [init_config/1, connect/1, disconnect/1, recv_message/1,
                recv/1, recv_presence/1, send/2, send_recv/2, my_jid/1,
		server_jid/1, pubsub_jid/1, proxy_jid/1, muc_jid/1,
		muc_room_jid/1, my_muc_jid/1, peer_muc_jid/1,
		mix_jid/1, mix_room_jid/1, get_features/2, recv_iq/1,
		re_register/1, is_feature_advertised/2, subscribe_to_events/1,
                is_feature_advertised/3, set_opt/3,
		auth_SASL/2, auth_SASL/3, auth_SASL/4,
                wait_for_master/1, wait_for_slave/1, flush/1,
                make_iq_result/1, start_event_relay/0, alt_room_jid/1,
                stop_event_relay/1, put_event/2, get_event/1,
                bind/1, auth/1, auth/2, open_session/1, open_session/2,
		zlib/1, starttls/1, starttls/2, close_socket/1, init_stream/1,
		auth_legacy/2, auth_legacy/3, tcp_connect/1, send_text/2,
		set_roster/3, del_roster/1]).
-include("suite.hrl").

suite() ->
    [{timetrap, {seconds, 120}}].

init_per_suite(Config) ->
    NewConfig = init_config(Config),
    DataDir = proplists:get_value(data_dir, NewConfig),
    {ok, CWD} = file:get_cwd(),
    ExtAuthScript = filename:join([DataDir, "extauth.py"]),
    LDIFFile = filename:join([DataDir, "ejabberd.ldif"]),
    {ok, _} = file:copy(ExtAuthScript, filename:join([CWD, "extauth.py"])),
    {ok, _} = ldap_srv:start(LDIFFile),
    inet_db:add_host({127,0,0,1}, [binary_to_list(?S2S_VHOST),
				   binary_to_list(?MNESIA_VHOST),
				   binary_to_list(?UPLOAD_VHOST)]),
    inet_db:set_domain(binary_to_list(p1_rand:get_string())),
    inet_db:set_lookup([file, native]),
    start_ejabberd(NewConfig),
    NewConfig.

start_ejabberd(_) ->
    TestBeams = case filelib:is_dir("../../test/") of
       true -> "../../test/";
       _ -> "../../lib/ejabberd/test/"
    end,
    application:set_env(ejabberd, external_beams, TestBeams),
    {ok, _} = application:ensure_all_started(ejabberd, transient).

end_per_suite(_Config) ->
    application:stop(ejabberd).

init_per_group(Group, Config) ->
    case lists:member(Group, ?BACKENDS) of
        false ->
            %% Not a backend related group, do default init:
            do_init_per_group(Group, Config);
        true ->
            case proplists:get_value(backends, Config) of
                all ->
                    %% All backends enabled
                    do_init_per_group(Group, Config);
                Backends ->
                    %% Skipped backends that were not explicitly enabled
		    case lists:member(Group, Backends) of
			true ->
			    do_init_per_group(Group, Config);
			false ->
			    {skip, {disabled_backend, Group}}
		    end
            end
    end.

do_init_per_group(no_db, Config) ->
    re_register(Config),
    set_opt(persistent_room, false, Config);
do_init_per_group(mnesia, Config) ->
    mod_muc:shutdown_rooms(?MNESIA_VHOST),
    set_opt(server, ?MNESIA_VHOST, Config);
do_init_per_group(redis, Config) ->
    mod_muc:shutdown_rooms(?REDIS_VHOST),
    set_opt(server, ?REDIS_VHOST, Config);
do_init_per_group(mysql, Config) ->
    case catch ejabberd_sql:sql_query(?MYSQL_VHOST, [<<"select 1;">>]) of
        {selected, _, _} ->
            mod_muc:shutdown_rooms(?MYSQL_VHOST),
            update_sql(?MYSQL_VHOST, Config),
            stop_temporary_modules(?MYSQL_VHOST),
            set_opt(server, ?MYSQL_VHOST, Config);
        Err ->
            {skip, {mysql_not_available, Err}}
    end;
do_init_per_group(mssql, Config) ->
    case catch ejabberd_sql:sql_query(?MSSQL_VHOST, [<<"select 1;">>]) of
        {selected, _, _} ->
            mod_muc:shutdown_rooms(?MSSQL_VHOST),
            update_sql(?MSSQL_VHOST, Config),
            stop_temporary_modules(?MSSQL_VHOST),
            set_opt(server, ?MSSQL_VHOST, Config);
        Err ->
            {skip, {mssql_not_available, Err}}
    end;
do_init_per_group(pgsql, Config) ->
    case catch ejabberd_sql:sql_query(?PGSQL_VHOST, [<<"select 1;">>]) of
        {selected, _, _} ->
            mod_muc:shutdown_rooms(?PGSQL_VHOST),
            update_sql(?PGSQL_VHOST, Config),
            stop_temporary_modules(?PGSQL_VHOST),
            set_opt(server, ?PGSQL_VHOST, Config);
        Err ->
            {skip, {pgsql_not_available, Err}}
    end;
do_init_per_group(sqlite, Config) ->
    case catch ejabberd_sql:sql_query(?SQLITE_VHOST, [<<"select 1;">>]) of
        {selected, _, _} ->
            mod_muc:shutdown_rooms(?SQLITE_VHOST),
            set_opt(server, ?SQLITE_VHOST, Config);
        Err ->
            {skip, {sqlite_not_available, Err}}
    end;
do_init_per_group(ldap, Config) ->
    set_opt(server, ?LDAP_VHOST, Config);
do_init_per_group(extauth, Config) ->
    set_opt(server, ?EXTAUTH_VHOST, Config);
do_init_per_group(s2s, Config) ->
    ejabberd_config:set_option({s2s_use_starttls, ?COMMON_VHOST}, required),
    ejabberd_config:set_option(ca_file, "ca.pem"),
    Port = ?config(s2s_port, Config),
    set_opt(server, ?COMMON_VHOST,
	    set_opt(xmlns, ?NS_SERVER,
		    set_opt(type, server,
			    set_opt(server_port, Port,
				    set_opt(stream_from, ?S2S_VHOST,
					    set_opt(lang, <<"">>, Config))))));
do_init_per_group(component, Config) ->
    Server = ?config(server, Config),
    Port = ?config(component_port, Config),
    set_opt(xmlns, ?NS_COMPONENT,
            set_opt(server, <<"component.", Server/binary>>,
                    set_opt(type, component,
                            set_opt(server_port, Port,
                                    set_opt(stream_version, undefined,
                                            set_opt(lang, <<"">>, Config))))));
do_init_per_group(GroupName, Config) ->
    Pid = start_event_relay(),
    NewConfig = set_opt(event_relay, Pid, Config),
    case GroupName of
	anonymous -> set_opt(anonymous, true, NewConfig);
	_ -> NewConfig
    end.

stop_temporary_modules(Host) ->
    Modules = [mod_shared_roster],
    [gen_mod:stop_module(Host, M) || M <- Modules].

end_per_group(mnesia, _Config) ->
    ok;
end_per_group(redis, _Config) ->
    ok;
end_per_group(mysql, Config) ->
    Query = "SELECT COUNT(*) FROM information_schema.tables WHERE table_name = 'mqtt_pub';",
    case catch ejabberd_sql:sql_query(?MYSQL_VHOST, [Query]) of
        {selected, _, [[<<"0">>]]} ->
            ok;
        {selected, _, [[<<"1">>]]} ->
            clear_sql_tables(mysql, Config);
        Other ->
            ct:fail({failed_to_check_table_existence, mysql, Other})
    end,
    ok;
end_per_group(mssql, Config) ->
    Query = "SELECT * FROM sys.tables WHERE name = 'mqtt_pub'",
    case catch ejabberd_sql:sql_query(?MSSQL_VHOST, [Query]) of
        {selected, [t]} ->
            clear_sql_tables(mssql, Config);
        Other ->
            ct:fail({failed_to_check_table_existence, mssql, Other})
    end,
    ok;
end_per_group(pgsql, Config) ->
    Query = "SELECT EXISTS (SELECT 0 FROM information_schema.tables WHERE table_name = 'mqtt_pub');",
    case catch ejabberd_sql:sql_query(?PGSQL_VHOST, [Query]) of
        {selected, [t]} ->
            clear_sql_tables(pgsql, Config);
	{selected, _, [[<<"t">>]]} ->
	    clear_sql_tables(pgsql, Config);
        Other ->
            ct:fail({failed_to_check_table_existence, pgsql, Other})
    end,
    ok;
end_per_group(sqlite, _Config) ->
    ok;
end_per_group(no_db, _Config) ->
    ok;
end_per_group(ldap, _Config) ->
    ok;
end_per_group(extauth, _Config) ->
    ok;
end_per_group(component, _Config) ->
    ok;
end_per_group(s2s, Config) ->
    Server = ?config(server, Config),
    ejabberd_config:set_option({s2s_use_starttls, Server}, false);
end_per_group(_GroupName, Config) ->
    stop_event_relay(Config),
    set_opt(anonymous, false, Config).

init_per_testcase(stop_ejabberd, Config) ->
    NewConfig = set_opt(resource, <<"">>,
			set_opt(anonymous, true, Config)),
    open_session(bind(auth(connect(NewConfig))));
init_per_testcase(TestCase, OrigConfig) ->
    ct:print(80, "Testcase '~p' starting", [TestCase]),
    Test = atom_to_list(TestCase),
    IsMaster = lists:suffix("_master", Test),
    IsSlave = lists:suffix("_slave", Test),
    if IsMaster or IsSlave ->
	    subscribe_to_events(OrigConfig);
       true ->
	    ok
    end,
    TestGroup = proplists:get_value(
		  name, ?config(tc_group_properties, OrigConfig)),
    Server = ?config(server, OrigConfig),
    Resource = case TestGroup of
		   anonymous ->
		       <<"">>;
		   legacy_auth ->
		       p1_rand:get_string();
		   _ ->
		       ?config(resource, OrigConfig)
	       end,
    MasterResource = ?config(master_resource, OrigConfig),
    SlaveResource = ?config(slave_resource, OrigConfig),
    Mode = if IsSlave -> slave;
	      IsMaster -> master;
	      true -> single
	   end,
    IsCarbons = lists:prefix("carbons_", Test),
    IsReplaced = lists:prefix("replaced_", Test),
    User = if IsReplaced -> <<"test_single!#$%^*()`~+-;_=[]{}|\\">>;
	      IsCarbons and not (IsMaster or IsSlave) ->
		   <<"test_single!#$%^*()`~+-;_=[]{}|\\">>;
	      IsMaster or IsCarbons -> <<"test_master!#$%^*()`~+-;_=[]{}|\\">>;
              IsSlave -> <<"test_slave!#$%^*()`~+-;_=[]{}|\\">>;
              true -> <<"test_single!#$%^*()`~+-;_=[]{}|\\">>
           end,
    Nick = if IsSlave -> ?config(slave_nick, OrigConfig);
	      IsMaster -> ?config(master_nick, OrigConfig);
	      true -> ?config(nick, OrigConfig)
	   end,
    MyResource = if IsMaster and IsCarbons -> MasterResource;
		    IsSlave and IsCarbons -> SlaveResource;
		    true -> Resource
		 end,
    Slave = if IsCarbons ->
		    jid:make(<<"test_master!#$%^*()`~+-;_=[]{}|\\">>, Server, SlaveResource);
	       IsReplaced ->
		    jid:make(User, Server, Resource);
	       true ->
		    jid:make(<<"test_slave!#$%^*()`~+-;_=[]{}|\\">>, Server, Resource)
	    end,
    Master = if IsCarbons ->
		     jid:make(<<"test_master!#$%^*()`~+-;_=[]{}|\\">>, Server, MasterResource);
		IsReplaced ->
		     jid:make(User, Server, Resource);
		true ->
		     jid:make(<<"test_master!#$%^*()`~+-;_=[]{}|\\">>, Server, Resource)
	     end,
    Config1 = set_opt(user, User,
		      set_opt(slave, Slave,
			      set_opt(master, Master,
				      set_opt(resource, MyResource,
					      set_opt(nick, Nick,
						      set_opt(mode, Mode, OrigConfig)))))),
    Config2 = if IsSlave ->
		      set_opt(peer_nick, ?config(master_nick, Config1), Config1);
		 IsMaster ->
		      set_opt(peer_nick, ?config(slave_nick, Config1), Config1);
		 true ->
		      Config1
	      end,
    Config = if IsSlave -> set_opt(peer, Master, Config2);
		IsMaster -> set_opt(peer, Slave, Config2);
		true -> Config2
	     end,
    case Test of
        "test_connect" ++ _ ->
            Config;
        "webadmin_" ++ _ ->
            Config;
	"test_legacy_auth_feature" ->
	    connect(Config);
	"test_legacy_auth" ++ _ ->
	    init_stream(set_opt(stream_version, undefined, Config));
        "test_auth" ++ _ ->
            connect(Config);
        "test_starttls" ++ _ ->
            connect(Config);
        "test_zlib" ->
            auth(connect(starttls(connect(Config))));
        "test_register" ->
            connect(Config);
        "auth_md5" ->
            connect(Config);
        "auth_plain" ->
            connect(Config);
	"auth_external" ++ _ ->
	    connect(Config);
	"unauthenticated_" ++ _ ->
	    connect(Config);
        "test_bind" ->
            auth(connect(Config));
	"sm_resume" ->
	    auth(connect(Config));
	"sm_resume_failed" ->
	    auth(connect(Config));
        "test_open_session" ->
            bind(auth(connect(Config)));
	"replaced" ++ _ ->
	    auth(connect(Config));
        "antispam" ++ _ ->
            Password = ?config(password, Config),
            ejabberd_auth:try_register(User, Server, Password),
            open_session(bind(auth(connect(Config))));
        _ when IsMaster or IsSlave ->
            Password = ?config(password, Config),
            ejabberd_auth:try_register(User, Server, Password),
            open_session(bind(auth(connect(Config))));
	_ when TestGroup == s2s_tests ->
	    auth(connect(starttls(connect(Config))));
        _ ->
            open_session(bind(auth(connect(Config))))
    end.

end_per_testcase(_TestCase, _Config) ->
    ok.

legacy_auth_tests() ->
    {legacy_auth, [parallel],
     [test_legacy_auth_feature,
      test_legacy_auth,
      test_legacy_auth_digest,
      test_legacy_auth_no_resource,
      test_legacy_auth_bad_jid,
      test_legacy_auth_fail]}.

no_db_tests() ->
    [{anonymous, [parallel],
      [test_connect_bad_xml,
       test_connect_unexpected_xml,
       test_connect_unknown_ns,
       test_connect_bad_xmlns,
       test_connect_bad_ns_stream,
       test_connect_bad_lang,
       test_connect_bad_to,
       test_connect_missing_to,
       test_connect,
       unauthenticated_iq,
       unauthenticated_message,
       unauthenticated_presence,
       test_starttls,
       test_auth,
       test_zlib,
       test_bind,
       test_open_session,
       codec_failure,
       unsupported_query,
       bad_nonza,
       invalid_from,
       ping,
       version,
       time,
       stats,
       disco]},
     {presence_and_s2s, [sequence],
      [test_auth_fail,
       presence,
       s2s_dialback,
       s2s_optional,
       s2s_required]},
     auth_external,
     auth_external_no_jid,
     auth_external_no_user,
     auth_external_malformed_jid,
     auth_external_wrong_jid,
     auth_external_wrong_server,
     auth_external_invalid_cert,
     commands_tests:single_cases(),
     configtest_tests:single_cases(),
     jidprep_tests:single_cases(),
     sm_tests:single_cases(),
     sm_tests:master_slave_cases(),
     muc_tests:single_cases(),
     muc_tests:master_slave_cases(),
     proxy65_tests:single_cases(),
     proxy65_tests:master_slave_cases(),
     stundisco_tests:single_cases(),
     replaced_tests:master_slave_cases(),
     upload_tests:single_cases(),
     carbons_tests:single_cases(),
     carbons_tests:master_slave_cases()].

db_tests(DB) when DB == mnesia; DB == redis ->
    [{single_user, [sequence],
      [test_register,
       legacy_auth_tests(),
       auth_plain,
       auth_md5,
       presence_broadcast,
       last,
       antispam_tests:single_cases(),
       webadmin_tests:single_cases(),
       roster_tests:single_cases(),
       private_tests:single_cases(),
       privacy_tests:single_cases(),
       vcard_tests:single_cases(),
       pubsub_tests:single_cases(),
       muc_tests:single_cases(),
       offline_tests:single_cases(),
       mam_tests:single_cases(),
       csi_tests:single_cases(),
       push_tests:single_cases(),
       test_pass_change,
       test_unregister]},
     muc_tests:master_slave_cases(),
     privacy_tests:master_slave_cases(),
     pubsub_tests:master_slave_cases(),
     roster_tests:master_slave_cases(),
     offline_tests:master_slave_cases(DB),
     mam_tests:master_slave_cases(),
     vcard_tests:master_slave_cases(),
     announce_tests:master_slave_cases(),
     csi_tests:master_slave_cases(),
     push_tests:master_slave_cases()];
db_tests(DB) ->
    [{single_user, [sequence],
      [test_register,
       legacy_auth_tests(),
       auth_plain,
       auth_md5,
       presence_broadcast,
       last,
       webadmin_tests:single_cases(),
       roster_tests:single_cases(),
       private_tests:single_cases(),
       privacy_tests:single_cases(),
       vcard_tests:single_cases(),
       pubsub_tests:single_cases(),
       muc_tests:single_cases(),
       offline_tests:single_cases(),
       mam_tests:single_cases(),
       push_tests:single_cases(),
       test_pass_change,
       test_unregister]},
     muc_tests:master_slave_cases(),
     privacy_tests:master_slave_cases(),
     pubsub_tests:master_slave_cases(),
     roster_tests:master_slave_cases(),
     offline_tests:master_slave_cases(DB),
     mam_tests:master_slave_cases(),
     vcard_tests:master_slave_cases(),
     announce_tests:master_slave_cases(),
     push_tests:master_slave_cases()].

ldap_tests() ->
    [{ldap_tests, [sequence],
      [test_auth,
       test_auth_fail,
       vcard_get,
       ldap_shared_roster_get]}].

extauth_tests() ->
    [{extauth_tests, [sequence],
      [test_auth,
       test_auth_fail,
       test_unregister]}].

component_tests() ->
    [{component_connect, [parallel],
      [test_connect_bad_xml,
       test_connect_unexpected_xml,
       test_connect_unknown_ns,
       test_connect_bad_xmlns,
       test_connect_bad_ns_stream,
       test_connect_missing_to,
       test_connect,
       test_auth,
       test_auth_fail]},
     {component_tests, [sequence],
      [test_missing_from,
       test_missing_to,
       test_invalid_from,
       test_component_send,
       bad_nonza,
       codec_failure]}].

s2s_tests() ->
    [{s2s_connect, [parallel],
      [test_connect_bad_xml,
       test_connect_unexpected_xml,
       test_connect_unknown_ns,
       test_connect_bad_xmlns,
       test_connect_bad_ns_stream,
       test_connect,
       test_connect_s2s_starttls_required,
       test_starttls,
       test_connect_s2s_unauthenticated_iq,
       test_auth_starttls]},
     {s2s_tests, [sequence],
      [test_missing_from,
       test_missing_to,
       test_invalid_from,
       bad_nonza,
       codec_failure]}].

groups() ->
    [{ldap, [sequence], ldap_tests()},
     {extauth, [sequence], extauth_tests()},
     {no_db, [sequence], no_db_tests()},
     {component, [sequence], component_tests()},
     {s2s, [sequence], s2s_tests()},
     {mnesia, [sequence], db_tests(mnesia)},
     {redis, [sequence], db_tests(redis)},
     {mysql, [sequence], db_tests(mysql)},
     {mssql, [sequence], db_tests(mssql)},
     {pgsql, [sequence], db_tests(pgsql)},
     {sqlite, [sequence], db_tests(sqlite)}].

all() ->
    [{group, ldap},
     {group, no_db},
     {group, mnesia},
     {group, redis},
     {group, mysql},
     {group, mssql},
     {group, pgsql},
     {group, sqlite},
     {group, extauth},
     {group, component},
     {group, s2s},
     stop_ejabberd].

stop_ejabberd(Config) ->
    ok = application:stop(ejabberd),
    ?recv1(#stream_error{reason = 'system-shutdown'}),
    case suite:recv(Config) of
        {xmlstreamend, <<"stream:stream">>} ->
            ok;
        closed ->
            ok;
        Other ->
            suite:match_failure([Other], [closed])
    end,
    Config.

test_connect_bad_xml(Config) ->
    Config0 = tcp_connect(Config),
    send_text(Config0, <<"<'/>">>),
    Version = ?config(stream_version, Config0),
    ?recv1(#stream_start{version = Version}),
    ?recv1(#stream_error{reason = 'not-well-formed'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config0).

test_connect_unexpected_xml(Config) ->
    Config0 = tcp_connect(Config),
    send(Config0, #caps{}),
    Version = ?config(stream_version, Config0),
    ?recv1(#stream_start{version = Version}),
    ?recv1(#stream_error{reason = 'invalid-xml'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config0).

test_connect_unknown_ns(Config) ->
    Config0 = init_stream(set_opt(xmlns, <<"wrong">>, Config)),
    ?recv1(#stream_error{reason = 'invalid-xml'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config0).

test_connect_bad_xmlns(Config) ->
    NS = case ?config(type, Config) of
	     client -> ?NS_SERVER;
	     _ -> ?NS_CLIENT
	 end,
    Config0 = init_stream(set_opt(xmlns, NS, Config)),
    ?recv1(#stream_error{reason = 'invalid-namespace'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config0).

test_connect_bad_ns_stream(Config) ->
    Config0 = init_stream(set_opt(ns_stream, <<"wrong">>, Config)),
    ?recv1(#stream_error{reason = 'invalid-namespace'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config0).

test_connect_bad_lang(Config) ->
    Lang = iolist_to_binary(lists:duplicate(36, $x)),
    Config0 = init_stream(set_opt(lang, Lang, Config)),
    ?recv1(#stream_error{reason = 'invalid-xml'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config0).

test_connect_bad_to(Config) ->
    Config0 = init_stream(set_opt(server, <<"wrong.com">>, Config)),
    ?recv1(#stream_error{reason = 'host-unknown'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config0).

test_connect_missing_to(Config) ->
    Config0 = init_stream(set_opt(server, <<"">>, Config)),
    ?recv1(#stream_error{reason = 'improper-addressing'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config0).

test_connect(Config) ->
    disconnect(connect(Config)).

test_connect_s2s_starttls_required(Config) ->
    Config1 = connect(Config),
    send(Config1, #presence{}),
    ?recv1(#stream_error{reason = 'policy-violation'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config1).

test_connect_s2s_unauthenticated_iq(Config) ->
    Config1 = connect(starttls(connect(Config))),
    unauthenticated_iq(Config1).

test_starttls(Config) ->
    case ?config(starttls, Config) of
        true ->
            disconnect(connect(starttls(Config)));
        _ ->
            {skipped, 'starttls_not_available'}
    end.

test_zlib(Config) ->
    case ?config(compression, Config) of
        [_|_] = Ms ->
            case lists:member(<<"zlib">>, Ms) of
                true ->
                    disconnect(zlib(Config));
                false ->
                    {skipped, 'zlib_not_available'}
            end;
        _ ->
            {skipped, 'compression_not_available'}
    end.

test_register(Config) ->
    case ?config(register, Config) of
        true ->
            disconnect(register(Config));
        _ ->
            {skipped, 'registration_not_available'}
    end.

register(Config) ->
    #iq{type = result,
        sub_els = [#register{username = <<>>,
                             password = <<>>}]} =
        send_recv(Config, #iq{type = get, to = server_jid(Config),
                              sub_els = [#register{}]}),
    #iq{type = result, sub_els = []} =
        send_recv(
          Config,
          #iq{type = set,
              sub_els = [#register{username = ?config(user, Config),
                                   password = ?config(password, Config)}]}),
    Config.

test_pass_change(Config) ->
    case ?config(register, Config) of
	true ->
	    #iq{type = result, sub_els = []} =
		send_recv(
		    Config,
		    #iq{type = set,
			sub_els = [#register{username = ?config(user, Config),
					     password = ?config(password, Config)}]}),
	    #iq{type = result, sub_els = []} =
		send_recv(
		    Config,
		    #iq{type = set,
			sub_els = [#register{username = str:to_upper(?config(user, Config)),
					     password = ?config(password, Config)}]});
	_ ->
	    {skipped, 'registration_not_available'}
    end.

test_unregister(Config) ->
    case ?config(register, Config) of
        true ->
            try_unregister(Config);
        _ ->
            {skipped, 'registration_not_available'}
    end.

try_unregister(Config) ->
    true = is_feature_advertised(Config, ?NS_REGISTER),
    #iq{type = result, sub_els = []} =
        send_recv(
          Config,
          #iq{type = set,
              sub_els = [#register{remove = true}]}),
    ?recv1(#stream_error{reason = conflict}),
    Config.

unauthenticated_presence(Config) ->
    unauthenticated_packet(Config, #presence{}).

unauthenticated_message(Config) ->
    unauthenticated_packet(Config, #message{}).

unauthenticated_iq(Config) ->
    IQ = #iq{type = get, sub_els = [#disco_info{}]},
    unauthenticated_packet(Config, IQ).

unauthenticated_packet(Config, Pkt) ->
    From = my_jid(Config),
    To = server_jid(Config),
    send(Config, xmpp:set_from_to(Pkt, From, To)),
    #stream_error{reason = 'not-authorized'} = recv(Config),
    {xmlstreamend, <<"stream:stream">>} = recv(Config),
    close_socket(Config).

bad_nonza(Config) ->
    %% Unsupported and invalid nonza should be silently dropped.
    send(Config, #caps{}),
    send(Config, #stanza_error{type = wrong}),
    disconnect(Config).

invalid_from(Config) ->
    send(Config, #message{from = jid:make(p1_rand:get_string())}),
    ?recv1(#stream_error{reason = 'invalid-from'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config).

test_missing_from(Config) ->
    Server = server_jid(Config),
    send(Config, #message{to = Server}),
    ?recv1(#stream_error{reason = 'improper-addressing'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config).

test_missing_to(Config) ->
    Server = server_jid(Config),
    send(Config, #message{from = Server}),
    ?recv1(#stream_error{reason = 'improper-addressing'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config).

test_invalid_from(Config) ->
    From = jid:make(p1_rand:get_string()),
    To = jid:make(p1_rand:get_string()),
    send(Config, #message{from = From, to = To}),
    ?recv1(#stream_error{reason = 'invalid-from'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config).

test_component_send(Config) ->
    To = jid:make(?COMMON_VHOST),
    From = server_jid(Config),
    #iq{type = result, from = To, to = From} =
	send_recv(Config, #iq{type = get, to = To, from = From,
			      sub_els = [#ping{}]}),
    disconnect(Config).

s2s_dialback(Config) ->
    Server = ?config(server, Config),
    ejabberd_s2s:stop_s2s_connections(),
    ejabberd_config:set_option({s2s_use_starttls, Server}, false),
    ejabberd_config:set_option({s2s_use_starttls, ?MNESIA_VHOST}, false),
    ejabberd_config:set_option(ca_file, pkix:get_cafile()),
    s2s_ping(Config).

s2s_optional(Config) ->
    Server = ?config(server, Config),
    ejabberd_s2s:stop_s2s_connections(),
    ejabberd_config:set_option({s2s_use_starttls, Server}, optional),
    ejabberd_config:set_option({s2s_use_starttls, ?MNESIA_VHOST}, optional),
    ejabberd_config:set_option(ca_file, pkix:get_cafile()),
    s2s_ping(Config).

s2s_required(Config) ->
    Server = ?config(server, Config),
    ejabberd_s2s:stop_s2s_connections(),
    gen_mod:stop_module(Server, mod_s2s_dialback),
    gen_mod:stop_module(?MNESIA_VHOST, mod_s2s_dialback),
    ejabberd_config:set_option({s2s_use_starttls, Server}, required),
    ejabberd_config:set_option({s2s_use_starttls, ?MNESIA_VHOST}, required),
    ejabberd_config:set_option(ca_file, "ca.pem"),
    s2s_ping(Config).

s2s_ping(Config) ->
    From = my_jid(Config),
    To = jid:make(?MNESIA_VHOST),
    ID = p1_rand:get_string(),
    ejabberd_s2s:route(#iq{from = From, to = To, id = ID,
			   type = get, sub_els = [#ping{}]}),
    #iq{type = result, id = ID, sub_els = []} = recv_iq(Config),
    disconnect(Config).

auth_md5(Config) ->
    Mechs = ?config(mechs, Config),
    case lists:member(<<"DIGEST-MD5">>, Mechs) of
        true ->
            disconnect(auth_SASL(<<"DIGEST-MD5">>, Config));
        false ->
            disconnect(Config),
            {skipped, 'DIGEST-MD5_not_available'}
    end.

auth_plain(Config) ->
    Mechs = ?config(mechs, Config),
    case lists:member(<<"PLAIN">>, Mechs) of
        true ->
            disconnect(auth_SASL(<<"PLAIN">>, Config));
        false ->
            disconnect(Config),
            {skipped, 'PLAIN_not_available'}
    end.

auth_external(Config0) ->
    Config = connect(starttls(Config0)),
    disconnect(auth_SASL(<<"EXTERNAL">>, Config)).

auth_external_no_jid(Config0) ->
    Config = connect(starttls(Config0)),
    disconnect(auth_SASL(<<"EXTERNAL">>, Config, _ShoudFail = false,
			 {<<"">>, <<"">>, <<"">>})).

auth_external_no_user(Config0) ->
    Config = set_opt(user, <<"">>, connect(starttls(Config0))),
    disconnect(auth_SASL(<<"EXTERNAL">>, Config)).

auth_external_malformed_jid(Config0) ->
    Config = connect(starttls(Config0)),
    disconnect(auth_SASL(<<"EXTERNAL">>, Config, _ShouldFail = true,
			 {<<"">>, <<"@">>, <<"">>})).

auth_external_wrong_jid(Config0) ->
    Config = set_opt(user, <<"wrong">>,
		     connect(starttls(Config0))),
    disconnect(auth_SASL(<<"EXTERNAL">>, Config, _ShouldFail = true)).

auth_external_wrong_server(Config0) ->
    Config = connect(starttls(Config0)),
    disconnect(auth_SASL(<<"EXTERNAL">>, Config, _ShouldFail = true,
			 {<<"">>, <<"wrong.com">>, <<"">>})).

auth_external_invalid_cert(Config0) ->
    Config = connect(starttls(
		       set_opt(certfile, "self-signed-cert.pem", Config0))),
    disconnect(auth_SASL(<<"EXTERNAL">>, Config, _ShouldFail = true)).

test_legacy_auth_feature(Config) ->
    true = ?config(legacy_auth, Config),
    disconnect(Config).

test_legacy_auth(Config) ->
    disconnect(auth_legacy(Config, _Digest = false)).

test_legacy_auth_digest(Config) ->
    disconnect(auth_legacy(Config, _Digest = true)).

test_legacy_auth_no_resource(Config0) ->
    Config = set_opt(resource, <<"">>, Config0),
    disconnect(auth_legacy(Config, _Digest = false, _ShouldFail = true)).

test_legacy_auth_bad_jid(Config0) ->
    Config = set_opt(user, <<"@">>, Config0),
    disconnect(auth_legacy(Config, _Digest = false, _ShouldFail = true)).

test_legacy_auth_fail(Config0) ->
    Config = set_opt(user, <<"wrong">>, Config0),
    disconnect(auth_legacy(Config, _Digest = false, _ShouldFail = true)).

test_auth(Config) ->
    disconnect(auth(Config)).

test_auth_starttls(Config) ->
    disconnect(auth(connect(starttls(Config)))).

test_auth_fail(Config0) ->
    Config = set_opt(user, <<"wrong">>,
		     set_opt(password, <<"wrong">>, Config0)),
    disconnect(auth(Config, _ShouldFail = true)).

test_bind(Config) ->
    disconnect(bind(Config)).

test_open_session(Config) ->
    disconnect(open_session(Config, true)).

codec_failure(Config) ->
    JID = my_jid(Config),
    #iq{type = error} =
	send_recv(Config, #iq{type = wrong, from = JID, to = JID}),
    disconnect(Config).

unsupported_query(Config) ->
    ServerJID = server_jid(Config),
    #iq{type = error} = send_recv(Config, #iq{type = get, to = ServerJID}),
    #iq{type = error} = send_recv(Config, #iq{type = get, to = ServerJID,
					      sub_els = [#caps{}]}),
    #iq{type = error} = send_recv(Config, #iq{type = get, to = ServerJID,
					      sub_els = [#roster_query{},
							 #disco_info{},
							 #privacy_query{}]}),
    disconnect(Config).

presence(Config) ->
    JID = my_jid(Config),
    #presence{from = JID, to = JID} = send_recv(Config, #presence{}),
    disconnect(Config).

presence_broadcast(Config) ->
    Feature = <<"p1:tmp:", (p1_rand:get_string())/binary>>,
    Ver = crypto:hash(sha, ["client", $/, "bot", $/, "en", $/,
                            "ejabberd_ct", $<, Feature, $<]),
    B64Ver = base64:encode(Ver),
    Node = <<(?EJABBERD_CT_URI)/binary, $#, B64Ver/binary>>,
    Server = ?config(server, Config),
    Info = #disco_info{identities =
			   [#identity{category = <<"client">>,
				      type = <<"bot">>,
				      lang = <<"en">>,
				      name = <<"ejabberd_ct">>}],
		       node = Node, features = [Feature]},
    Caps = #caps{hash = <<"sha-1">>, node = ?EJABBERD_CT_URI, version = B64Ver},
    send(Config, #presence{sub_els = [Caps]}),
    JID = my_jid(Config),
    %% We receive:
    %% 1) disco#info iq request for CAPS
    %% 2) welcome message
    %% 3) presence broadcast
    IQ = #iq{type = get,
	     from = JID,
	     sub_els = [#disco_info{node = Node}]} = recv_iq(Config),
    #message{type = chat,
             subject = [#text{lang = <<"en">>,data = <<"Welcome!">>}]} = recv_message(Config),
    #presence{from = JID, to = JID} = recv_presence(Config),
    send(Config, #iq{type = result, id = IQ#iq.id,
		     to = JID, sub_els = [Info]}),
    %% We're trying to read our feature from ejabberd database
    %% with exponential back-off as our IQ response may be delayed.
    [Feature] =
	lists:foldl(
	  fun(Time, []) ->
		  timer:sleep(Time),
		  mod_caps:get_features(Server, Caps);
	     (_, Acc) ->
		  Acc
	  end, [], [0, 100, 200, 2000, 5000, 10000]),
    disconnect(Config).

ping(Config) ->
    true = is_feature_advertised(Config, ?NS_PING),
    #iq{type = result, sub_els = []} =
        send_recv(
          Config,
          #iq{type = get, sub_els = [#ping{}], to = server_jid(Config)}),
    disconnect(Config).

version(Config) ->
    true = is_feature_advertised(Config, ?NS_VERSION),
    #iq{type = result, sub_els = [#version{}]} =
        send_recv(
          Config, #iq{type = get, sub_els = [#version{}],
                      to = server_jid(Config)}),
    disconnect(Config).

time(Config) ->
    true = is_feature_advertised(Config, ?NS_TIME),
    #iq{type = result, sub_els = [#time{}]} =
        send_recv(Config, #iq{type = get, sub_els = [#time{}],
                              to = server_jid(Config)}),
    disconnect(Config).

disco(Config) ->
    true = is_feature_advertised(Config, ?NS_DISCO_INFO),
    true = is_feature_advertised(Config, ?NS_DISCO_ITEMS),
    #iq{type = result, sub_els = [#disco_items{items = Items}]} =
        send_recv(
          Config, #iq{type = get, sub_els = [#disco_items{}],
                      to = server_jid(Config)}),
    lists:foreach(
      fun(#disco_item{jid = JID, node = Node}) ->
              #iq{type = result} =
                  send_recv(Config,
                            #iq{type = get, to = JID,
                                sub_els = [#disco_info{node = Node}]})
      end, Items),
    disconnect(Config).

last(Config) ->
    true = is_feature_advertised(Config, ?NS_LAST),
    #iq{type = result, sub_els = [#last{}]} =
        send_recv(Config, #iq{type = get, sub_els = [#last{}],
                              to = server_jid(Config)}),
    disconnect(Config).

vcard_get(Config) ->
    true = is_feature_advertised(Config, ?NS_VCARD),
    %% TODO: check if VCard corresponds to LDIF data from ejabberd.ldif
    #iq{type = result, sub_els = [_VCard]} =
        send_recv(Config, #iq{type = get, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

ldap_shared_roster_get(Config) ->
    Item = #roster_item{jid = jid:decode(<<"user2@ldap.localhost">>), name = <<"Test User 2">>,
                        groups = [<<"group1">>], subscription = both},
    #iq{type = result, sub_els = [#roster_query{items = [Item]}]} =
        send_recv(Config, #iq{type = get, sub_els = [#roster_query{}]}),
    disconnect(Config).

stats(Config) ->
    #iq{type = result, sub_els = [#stats{list = Stats}]} =
        send_recv(Config, #iq{type = get, sub_els = [#stats{}],
                              to = server_jid(Config)}),
    lists:foreach(
      fun(#stat{} = Stat) ->
              #iq{type = result, sub_els = [_|_]} =
                  send_recv(Config, #iq{type = get,
                                        sub_els = [#stats{list = [Stat]}],
                                        to = server_jid(Config)})
      end, Stats),
    disconnect(Config).

%%%===================================================================
%%% Aux functions
%%%===================================================================
bookmark_conference() ->
    #bookmark_conference{name = <<"Some name">>,
                         autojoin = true,
                         jid = jid:make(
                                 <<"some">>,
                                 <<"some.conference.org">>,
                                 <<>>)}.

'$handle_undefined_function'(F, [Config]) when is_list(Config) ->
    case re:split(atom_to_list(F), "_", [{return, list}, {parts, 2}]) of
	[M, T] ->
	    Module = list_to_atom(M ++ "_tests"),
	    Function = list_to_atom(T),
	    case erlang:function_exported(Module, Function, 1) of
		true ->
		    Module:Function(Config);
		false ->
		    erlang:error({undef, F})
	    end;
	_ ->
	    erlang:error({undef, F})
    end;
'$handle_undefined_function'(_, _) ->
    erlang:error(undef).


%%%===================================================================
%%% SQL stuff
%%%===================================================================
update_sql(Host, Config) ->
    case ?config(update_sql, Config) of
        true ->
            mod_admin_update_sql:update_sql(Host);
        false -> ok
    end.

schema_suffix(Config) ->
    case ejabberd_sql:use_new_schema() of
        true ->
            case ?config(update_sql, Config) of
                true ->  ".sql";
                _ -> ".new.sql"
            end;
        _ -> ".sql"
    end.

clear_sql_tables(sqlite, _Config) ->
    ok;
clear_sql_tables(Type, Config) ->
    BaseDir = ?config(base_dir, Config),
    {VHost, File} = case Type of
                        mysql -> {?MYSQL_VHOST, "mysql" ++ schema_suffix(Config)};
                        mssql -> {?MSSQL_VHOST, "mssql" ++ schema_suffix(Config)};
                        pgsql -> {?PGSQL_VHOST, "pg" ++ schema_suffix(Config)}
                    end,
    SQLFile = filename:join([BaseDir, "sql", File]),
    CreationQueries = read_sql_queries(SQLFile),
    ClearTableQueries = clear_table_queries(CreationQueries),
    case ejabberd_sql:sql_transaction(
           VHost, ClearTableQueries) of
        {atomic, ok} ->
            ok;
        Err ->
            ct:fail({failed_to_clear_sql_tables, Type, Err})
    end.

read_sql_queries(File) ->
    case file:open(File, [read, binary]) of
        {ok, Fd} ->
            read_lines(Fd, File, []);
        Err ->
            ct:fail({open_file_failed, File, Err})
    end.

clear_table_queries(Queries) ->
    lists:foldl(
      fun(Query, Acc) ->
              case split(str:to_lower(Query)) of
                  [<<"create">>, <<"table">>, Table|_] ->
                      GlobalRamTables = [<<"bosh">>,
                                         <<"oauth_client">>,
                                         <<"oauth_token">>,
                                         <<"proxy65">>,
                                         <<"route">>],
                      case lists:member(Table, GlobalRamTables) of
                          true ->
                              Acc;
                          false ->
                              [<<"DELETE FROM ", Table/binary, ";">>|Acc]
                      end;
                  _ ->
                      Acc
              end
      end, [], Queries).

read_lines(Fd, File, Acc) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            NewAcc = case str:strip(str:strip(Line, both, $\r), both, $\n) of
                         <<"--", _/binary>> ->
                             Acc;
                         <<>> ->
                             Acc;
                         _ ->
                             [Line|Acc]
                     end,
            read_lines(Fd, File, NewAcc);
        eof ->
            QueryList = str:tokens(list_to_binary(lists:reverse(Acc)), <<";">>),
            lists:flatmap(
              fun(Query) ->
                      case str:strip(str:strip(Query, both, $\r), both, $\n) of
                          <<>> ->
                              [];
                          Q ->
                              [<<Q/binary, $;>>]
                      end
              end, QueryList);
        {error, _} = Err ->
            ct:fail({read_file_failed, File, Err})
    end.

split(Data) ->
    lists:filter(
      fun(<<>>) ->
              false;
         (_) ->
              true
      end, re:split(Data, <<"\s">>)).
