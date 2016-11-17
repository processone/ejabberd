%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2016, ProcessOne
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_SUITE).

-compile(export_all).

-import(suite, [init_config/1, connect/1, disconnect/1, recv_message/1,
                recv/1, recv_presence/1, send/2, send_recv/2, my_jid/1,
		server_jid/1, pubsub_jid/1, proxy_jid/1, muc_jid/1,
		muc_room_jid/1, my_muc_jid/1, peer_muc_jid/1,
		mix_jid/1, mix_room_jid/1, get_features/2, recv_iq/1,
		re_register/1, is_feature_advertised/2, subscribe_to_events/1,
                is_feature_advertised/3, set_opt/3, auth_SASL/2,
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
				   binary_to_list(?MNESIA_VHOST)]),
    inet_db:set_domain(binary_to_list(randoms:get_string())),
    inet_db:set_lookup([file, native]),
    start_ejabberd(NewConfig),
    NewConfig.

start_ejabberd(Config) ->
    case proplists:get_value(backends, Config) of
        all ->
            ok = application:start(ejabberd, transient);
        Backends when is_list(Backends) ->
            Hosts = lists:map(fun(Backend) -> Backend ++ ".localhost" end, Backends),
            application:load(ejabberd),
            AllHosts = Hosts ++ ["localhost"],    %% We always need localhost for the generic no_db tests
            application:set_env(ejabberd, hosts, AllHosts),
            ok = application:start(ejabberd, transient)
    end.

end_per_suite(_Config) ->
    application:stop(ejabberd).

-define(BACKENDS, [mnesia,redis,mysql,pgsql,sqlite,ldap,extauth,riak]).

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
                    %% Skipped backends that were not explicitely enabled
                    case lists:member(atom_to_list(Group), Backends) of
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
            create_sql_tables(mysql, ?config(base_dir, Config)),
            set_opt(server, ?MYSQL_VHOST, Config);
        Err ->
            {skip, {mysql_not_available, Err}}
    end;
do_init_per_group(pgsql, Config) ->
    case catch ejabberd_sql:sql_query(?PGSQL_VHOST, [<<"select 1;">>]) of
        {selected, _, _} ->
            mod_muc:shutdown_rooms(?PGSQL_VHOST),
            create_sql_tables(pgsql, ?config(base_dir, Config)),
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
do_init_per_group(riak, Config) ->
    case ejabberd_riak:is_connected() of
	true ->
	    mod_muc:shutdown_rooms(?RIAK_VHOST),
	    NewConfig = set_opt(server, ?RIAK_VHOST, Config),
	    clear_riak_tables(NewConfig);
	Err ->
	    {skip, {riak_not_available, Err}}
    end;
do_init_per_group(s2s, Config) ->
    ejabberd_config:add_option(s2s_use_starttls, required_trusted),
    ejabberd_config:add_option(domain_certfile, "cert.pem"),
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

end_per_group(mnesia, _Config) ->
    ok;
end_per_group(redis, _Config) ->
    ok;
end_per_group(mysql, _Config) ->
    ok;
end_per_group(pgsql, _Config) ->
    ok;
end_per_group(sqlite, _Config) ->
    ok;
end_per_group(no_db, _Config) ->
    ok;
end_per_group(ldap, _Config) ->
    ok;
end_per_group(extauth, _Config) ->
    ok;
end_per_group(riak, _Config) ->
    ok;
end_per_group(component, _Config) ->
    ok;
end_per_group(s2s, _Config) ->
    ejabberd_config:add_option(s2s_use_starttls, false);
end_per_group(_GroupName, Config) ->
    stop_event_relay(Config),
    set_opt(anonymous, false, Config).

init_per_testcase(stop_ejabberd, Config) ->
    NewConfig = set_opt(resource, <<"">>,
			set_opt(anonymous, true, Config)),
    open_session(bind(auth(connect(NewConfig))));
init_per_testcase(TestCase, OrigConfig) ->
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
		       randoms:get_string();
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
	"test_legacy_auth" ++ _ ->
	    init_stream(set_opt(stream_version, undefined, Config));
        "test_auth" ++ _ ->
            connect(Config);
        "test_starttls" ++ _ ->
            connect(Config);
        "test_zlib" ->
            connect(Config);
        "test_register" ->
            connect(Config);
        "auth_md5" ->
            connect(Config);
        "auth_plain" ->
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
     [test_legacy_auth,
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
       unauthenticated_stanza,
       test_starttls,
       test_zlib,
       test_auth,
       test_bind,
       test_open_session,
       codec_failure,
       unsupported_query,
       bad_nonza,
       invalid_from,
       legacy_iq,
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
       s2s_required,
       s2s_required_trusted]},
     {sm, [sequence],
       [sm,
	sm_resume,
	sm_resume_failed]},
     muc_tests:single_cases(),
     muc_tests:master_slave_cases(),
     {test_proxy65, [parallel],
      [proxy65_master, proxy65_slave]},
     {replaced, [parallel],
      [replaced_master, replaced_slave]}].

pubsub_single_tests() ->
    {pubsub_single, [sequence],
     [test_pubsub_features,
      test_pubsub_create,
      test_pubsub_configure,
      test_pubsub_delete,
      test_pubsub_get_affiliations,
      test_pubsub_get_subscriptions,
      test_pubsub_create_instant,
      test_pubsub_default,
      test_pubsub_create_configure,
      test_pubsub_publish,
      test_pubsub_auto_create,
      test_pubsub_get_items,
      test_pubsub_delete_item,
      test_pubsub_purge,
      test_pubsub_subscribe,
      test_pubsub_unsubscribe]}.

pubsub_multiple_tests() ->
    {pubsub_multiple, [sequence],
     [{pubsub_publish, [parallel],
       [pubsub_publish_master, pubsub_publish_slave]},
      {pubsub_subscriptions, [parallel],
       [pubsub_subscriptions_master, pubsub_subscriptions_slave]},
      {pubsub_affiliations, [parallel],
       [pubsub_affiliations_master, pubsub_affiliations_slave]},
      {pubsub_authorize, [parallel],
       [pubsub_authorize_master, pubsub_authorize_slave]}]}.

db_tests(riak) ->
    %% No support for mod_pubsub
    [{single_user, [sequence],
      [test_register,
       legacy_auth_tests(),
       auth_plain,
       auth_md5,
       presence_broadcast,
       last,
       roster_tests:single_cases(),
       private,
       privacy_tests:single_cases(),
       vcard,
       muc_tests:single_cases(),
       offline_tests:master_slave_cases(),
       test_unregister]},
     muc_tests:master_slave_cases(),
     privacy_tests:master_slave_cases(),
     roster_tests:master_slave_cases(),
     offline_tests:master_slave_cases(),
     {test_announce, [sequence],
      [announce_master, announce_slave]},
     {test_vcard_xupdate, [parallel],
      [vcard_xupdate_master, vcard_xupdate_slave]}];
db_tests(DB) when DB == mnesia; DB == redis ->
    [{single_user, [sequence],
      [test_register,
       legacy_auth_tests(),
       auth_plain,
       auth_md5,
       presence_broadcast,
       last,
       roster_tests:single_cases(),
       private,
       privacy_tests:single_cases(),
       vcard,
       pubsub_single_tests(),
       muc_tests:single_cases(),
       offline_tests:single_cases(),
       test_unregister]},
     muc_tests:master_slave_cases(),
     privacy_tests:master_slave_cases(),
     pubsub_multiple_tests(),
     roster_tests:master_slave_cases(),
     offline_tests:master_slave_cases(),
     {test_mix, [parallel],
      [mix_master, mix_slave]},
     {test_old_mam, [parallel],
      [mam_old_master, mam_old_slave]},
     {test_new_mam, [parallel],
      [mam_new_master, mam_new_slave]},
     {test_carbons, [parallel],
      [carbons_master, carbons_slave]},
     {test_client_state, [parallel],
      [client_state_master, client_state_slave]},
     {test_muc_mam, [parallel],
      [muc_mam_master, muc_mam_slave]},
     {test_announce, [sequence],
      [announce_master, announce_slave]},
     {test_vcard_xupdate, [parallel],
      [vcard_xupdate_master, vcard_xupdate_slave]}];
db_tests(_) ->
    %% No support for carboncopy
    [{single_user, [sequence],
      [test_register,
       legacy_auth_tests(),
       auth_plain,
       auth_md5,
       presence_broadcast,
       last,
       roster_tests:single_cases(),
       private,
       privacy_tests:single_cases(),
       vcard,
       pubsub_single_tests(),
       muc_tests:single_cases(),
       offline_tests:single_cases(),
       test_unregister]},
     muc_tests:master_slave_cases(),
     privacy_tests:master_slave_cases(),
     pubsub_multiple_tests(),
     roster_tests:master_slave_cases(),
     offline_tests:master_slave_cases(),
     {test_mix, [parallel],
      [mix_master, mix_slave]},
     {test_old_mam, [parallel],
      [mam_old_master, mam_old_slave]},
     {test_new_mam, [parallel],
      [mam_new_master, mam_new_slave]},
     {test_muc_mam, [parallel],
      [muc_mam_master, muc_mam_slave]},
     {test_announce, [sequence],
      [announce_master, announce_slave]},
     {test_vcard_xupdate, [parallel],
      [vcard_xupdate_master, vcard_xupdate_slave]}].

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
      [test_missing_address,
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
       test_connect_missing_from,
       test_connect_s2s_unauthenticated_iq,
       test_auth_starttls]},
     {s2s_tests, [sequence],
      [test_missing_address,
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
     {pgsql, [sequence], db_tests(pgsql)},
     {sqlite, [sequence], db_tests(sqlite)},
     {riak, [sequence], db_tests(riak)}].

all() ->
    [{group, ldap},
     {group, no_db},
     {group, mnesia},
     {group, redis},
     {group, mysql},
     {group, pgsql},
     {group, sqlite},
     {group, extauth},
     {group, riak},
     {group, component},
     {group, s2s},
     stop_ejabberd].

stop_ejabberd(Config) ->
    ok = application:stop(ejabberd),
    ?recv1(#stream_error{reason = 'system-shutdown'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
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
    ?recv1(#stream_error{reason = 'policy-violation'}),
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

test_connect_missing_from(Config) ->
    Config1 = starttls(connect(Config)),
    Config2 = set_opt(stream_from, <<"">>, Config1),
    Config3 = init_stream(Config2),
    ?recv1(#stream_error{reason = 'policy-violation'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config3).

test_connect(Config) ->
    disconnect(connect(Config)).

test_connect_s2s_starttls_required(Config) ->
    Config1 = connect(Config),
    send(Config1, #caps{}),
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

unauthenticated_stanza(Config) ->
    %% Unauthenticated stanza should be silently dropped.
    send(Config, #message{to = server_jid(Config)}),
    disconnect(Config).

unauthenticated_iq(Config) ->
    From = my_jid(Config),
    To = server_jid(Config),
    #iq{type = error} =
	send_recv(Config, #iq{type = get, from = From, to = To,
			      sub_els = [#disco_info{}]}),
    disconnect(Config).

bad_nonza(Config) ->
    %% Unsupported and invalid nonza should be silently dropped.
    send(Config, #caps{}),
    send(Config, #stanza_error{type = wrong}),
    disconnect(Config).

invalid_from(Config) ->
    send(Config, #message{from = jid:make(randoms:get_string())}),
    ?recv1(#stream_error{reason = 'invalid-from'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    close_socket(Config).

test_missing_address(Config) ->
    Server = server_jid(Config),
    #iq{type = error} = send_recv(Config, #iq{type = get, from = Server}),
    #iq{type = error} = send_recv(Config, #iq{type = get, to = Server}),
    disconnect(Config).

test_invalid_from(Config) ->
    From = jid:make(randoms:get_string()),
    To = jid:make(randoms:get_string()),
    #iq{type = error} =
	send_recv(Config, #iq{type = get, from = From, to = To}),
    disconnect(Config).

test_component_send(Config) ->
    To = jid:make(?COMMON_VHOST),
    From = server_jid(Config),
    #iq{type = result, from = To, to = From} =
	send_recv(Config, #iq{type = get, to = To, from = From,
			      sub_els = [#ping{}]}),
    disconnect(Config).

s2s_dialback(Config) ->
    ejabberd_s2s:stop_all_connections(),
    ejabberd_config:add_option(s2s_use_starttls, false),
    ejabberd_config:add_option(domain_certfile, "self-signed-cert.pem"),
    s2s_ping(Config).

s2s_optional(Config) ->
    ejabberd_s2s:stop_all_connections(),
    ejabberd_config:add_option(s2s_use_starttls, optional),
    ejabberd_config:add_option(domain_certfile, "self-signed-cert.pem"),
    s2s_ping(Config).

s2s_required(Config) ->
    ejabberd_s2s:stop_all_connections(),
    ejabberd_config:add_option(s2s_use_starttls, required),
    ejabberd_config:add_option(domain_certfile, "self-signed-cert.pem"),
    s2s_ping(Config).

s2s_required_trusted(Config) ->
    ejabberd_s2s:stop_all_connections(),
    ejabberd_config:add_option(s2s_use_starttls, required),
    ejabberd_config:add_option(domain_certfile, "cert.pem"),
    s2s_ping(Config).

s2s_ping(Config) ->
    From = my_jid(Config),
    To = jid:make(?MNESIA_VHOST),
    ID = randoms:get_string(),
    ejabberd_s2s:route(From, To, #iq{id = ID, type = get, sub_els = [#ping{}]}),
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

roster_feature_enabled(Config) ->
    roster_tests:feature_enabled(Config).
roster_iq_set_many_items(Config) ->
    roster_tests:iq_set_many_items(Config).
roster_iq_set_duplicated_groups(Config) ->
    roster_tests:iq_set_duplicated_groups(Config).
roster_iq_set_ask(Config) ->
    roster_tests:iq_set_ask(Config).
roster_iq_get_item(Config) ->
    roster_tests:iq_get_item(Config).
roster_iq_unexpected_element(Config) ->
    roster_tests:iq_unexpected_element(Config).
roster_set_item(Config) ->
    roster_tests:set_item(Config).
roster_version(Config) ->
    roster_tests:version(Config).
roster_subscribe_master(Config) ->
    roster_tests:subscribe_master(Config).
roster_subscribe_slave(Config) ->
    roster_tests:subscribe_slave(Config).

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
    Feature = <<"p1:tmp:", (randoms:get_string())/binary>>,
    Ver = crypto:hash(sha, ["client", $/, "bot", $/, "en", $/,
                            "ejabberd_ct", $<, Feature, $<]),
    B64Ver = base64:encode(Ver),
    Node = <<(?EJABBERD_CT_URI)/binary, $#, B64Ver/binary>>,
    Server = ?config(server, Config),
    ServerJID = server_jid(Config),
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
	     from = ServerJID,
	     sub_els = [#disco_info{node = Node}]} = recv_iq(Config),
    #message{type = normal} = recv_message(Config),
    #presence{from = JID, to = JID} = recv_presence(Config),
    send(Config, #iq{type = result, id = IQ#iq.id,
		     to = ServerJID, sub_els = [Info]}),
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

legacy_iq(Config) ->
    true = is_feature_advertised(Config, ?NS_EVENT),
    ServerJID = server_jid(Config),
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{to = ServerJID, type = get,
			      sub_els = [#xevent{}]}),
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

%% replaced_master(Config0) ->
%%     Config = bind(Config0),
%%     wait_for_slave(Config),
%%     ?recv1(#stream_error{reason = conflict}),
%%     ?recv1({xmlstreamend, <<"stream:stream">>}),
%%     close_socket(Config).

%% replaced_slave(Config0) ->
%%     wait_for_master(Config0),
%%     Config = bind(Config0),
%%     disconnect(Config).

replaced_master(Config) ->
    disconnect(Config).

replaced_slave(Config) ->
    disconnect(Config).

sm(Config) ->
    Server = ?config(server, Config),
    ServerJID = jid:make(<<"">>, Server, <<"">>),
    %% Send messages of type 'headline' so the server discards them silently
    Msg = #message{to = ServerJID, type = headline,
		   body = [#text{data = <<"body">>}]},
    true = ?config(sm, Config),
    %% Enable the session management with resumption enabled
    send(Config, #sm_enable{resume = true, xmlns = ?NS_STREAM_MGMT_3}),
    #sm_enabled{id = ID, resume = true} = recv(Config),
    %% Initial request; 'h' should be 0.
    send(Config, #sm_r{xmlns = ?NS_STREAM_MGMT_3}),
    ?recv1(#sm_a{h = 0}),
    %% sending two messages and requesting again; 'h' should be 3.
    send(Config, Msg),
    send(Config, Msg),
    send(Config, Msg),
    send(Config, #sm_r{xmlns = ?NS_STREAM_MGMT_3}),
    ?recv1(#sm_a{h = 3}),
    close_socket(Config),
    {save_config, set_opt(sm_previd, ID, Config)}.

sm_resume(Config) ->
    {sm, SMConfig} = ?config(saved_config, Config),
    ID = ?config(sm_previd, SMConfig),
    Server = ?config(server, Config),
    ServerJID = jid:make(<<"">>, Server, <<"">>),
    MyJID = my_jid(Config),
    Txt = #text{data = <<"body">>},
    Msg = #message{from = ServerJID, to = MyJID, body = [Txt]},
    %% Route message. The message should be queued by the C2S process.
    ejabberd_router:route(ServerJID, MyJID, Msg),
    send(Config, #sm_resume{previd = ID, h = 0, xmlns = ?NS_STREAM_MGMT_3}),
    ?recv1(#sm_resumed{previd = ID, h = 3}),
    #message{from = ServerJID, to = MyJID, body = [Txt]} = recv_message(Config),
    ?recv1(#sm_r{}),
    send(Config, #sm_a{h = 1, xmlns = ?NS_STREAM_MGMT_3}),
    %% Send another stanza to increment the server's 'h' for sm_resume_failed.
    send(Config, #presence{to = ServerJID}),
    close_socket(Config),
    {save_config, set_opt(sm_previd, ID, Config)}.

sm_resume_failed(Config) ->
    {sm_resume, SMConfig} = ?config(saved_config, Config),
    ID = ?config(sm_previd, SMConfig),
    ct:sleep(5000), % Wait for session to time out.
    send(Config, #sm_resume{previd = ID, h = 1, xmlns = ?NS_STREAM_MGMT_3}),
    ?recv1(#sm_failed{reason = 'item-not-found', h = 4}),
    disconnect(Config).

private(Config) ->
    Conference = #bookmark_conference{name = <<"Some name">>,
                                      autojoin = true,
                                      jid = jid:make(
                                              <<"some">>,
                                              <<"some.conference.org">>,
                                              <<>>)},
    Storage = #bookmark_storage{conference = [Conference]},
    StorageXMLOut = xmpp:encode(Storage),
    WrongEl = #xmlel{name = <<"wrong">>},
    #iq{type = error} =
        send_recv(Config, #iq{type = get,
			      sub_els = [#private{xml_els = [WrongEl]}]}),
    #iq{type = result, sub_els = []} =
        send_recv(
          Config, #iq{type = set,
                      sub_els = [#private{xml_els = [WrongEl, StorageXMLOut]}]}),
    #iq{type = result,
        sub_els = [#private{xml_els = [StorageXMLIn]}]} =
        send_recv(
          Config,
          #iq{type = get,
              sub_els = [#private{xml_els = [xmpp:encode(
                                               #bookmark_storage{})]}]}),
    Storage = xmpp:decode(StorageXMLIn),
    disconnect(Config).

last(Config) ->
    true = is_feature_advertised(Config, ?NS_LAST),
    #iq{type = result, sub_els = [#last{}]} =
        send_recv(Config, #iq{type = get, sub_els = [#last{}],
                              to = server_jid(Config)}),
    disconnect(Config).

privacy_feature_enabled(Config) ->
    privacy_tests:feature_enabled(Config).
privacy_set_get_list(Config) ->
    privacy_tests:set_get_list(Config).
privacy_get_list_non_existent(Config) ->
    privacy_tests:get_list_non_existent(Config).
privacy_set_default(Config) ->
    privacy_tests:set_default(Config).
privacy_del_default(Config) ->
    privacy_tests:del_default(Config).
privacy_set_default_non_existent(Config) ->
    privacy_tests:set_default_non_existent(Config).
privacy_set_active(Config) ->
    privacy_tests:set_active(Config).
privacy_del_active(Config) ->
    privacy_tests:del_active(Config).
privacy_set_active_non_existent(Config) ->
    privacy_tests:set_active_non_existent(Config).
privacy_remove_list(Config) ->
    privacy_tests:remove_list(Config).
privacy_remove_active_list(Config) ->
    privacy_tests:remove_active_list(Config).
privacy_remove_default_list(Config) ->
    privacy_tests:remove_default_list(Config).
privacy_remove_list_non_existent(Config) ->
    privacy_tests:remove_list_non_existent(Config).
privacy_allow_local_server(Config) ->
    privacy_tests:allow_local_server(Config).
privacy_malformed_iq_query(Config) ->
    privacy_tests:malformed_iq_query(Config).
privacy_malformed_get(Config) ->
    privacy_tests:malformed_get(Config).
privacy_malformed_set(Config) ->
    privacy_tests:malformed_set(Config).
privacy_malformed_type_value(Config) ->
    privacy_tests:malformed_type_value(Config).
privacy_set_get_block(Config) ->
    privacy_tests:set_get_block(Config).

privacy_deny_bare_jid_master(Config) ->
    privacy_tests:deny_bare_jid_master(Config).
privacy_deny_bare_jid_slave(Config) ->
    privacy_tests:deny_bare_jid_slave(Config).
privacy_deny_full_jid_master(Config) ->
    privacy_tests:deny_full_jid_master(Config).
privacy_deny_full_jid_slave(Config) ->
    privacy_tests:deny_full_jid_slave(Config).
privacy_deny_server_jid_master(Config) ->
    privacy_tests:deny_server_jid_master(Config).
privacy_deny_server_jid_slave(Config) ->
    privacy_tests:deny_server_jid_slave(Config).
privacy_deny_group_master(Config) ->
    privacy_tests:deny_group_master(Config).
privacy_deny_group_slave(Config) ->
    privacy_tests:deny_group_slave(Config).
privacy_deny_sub_both_master(Config) ->
    privacy_tests:deny_sub_both_master(Config).
privacy_deny_sub_both_slave(Config) ->
    privacy_tests:deny_sub_both_slave(Config).
privacy_deny_sub_from_master(Config) ->
    privacy_tests:deny_sub_from_master(Config).
privacy_deny_sub_from_slave(Config) ->
    privacy_tests:deny_sub_from_slave(Config).
privacy_deny_sub_to_master(Config) ->
    privacy_tests:deny_sub_to_master(Config).
privacy_deny_sub_to_slave(Config) ->
    privacy_tests:deny_sub_to_slave(Config).
privacy_deny_sub_none_master(Config) ->
    privacy_tests:deny_sub_none_master(Config).
privacy_deny_sub_none_slave(Config) ->
    privacy_tests:deny_sub_none_slave(Config).
privacy_deny_all_master(Config) ->
    privacy_tests:deny_all_master(Config).
privacy_deny_all_slave(Config) ->
    privacy_tests:deny_all_slave(Config).
privacy_deny_offline_master(Config) ->
    privacy_tests:deny_offline_master(Config).
privacy_deny_offline_slave(Config) ->
    privacy_tests:deny_offline_slave(Config).
privacy_block_master(Config) ->
    privacy_tests:block_master(Config).
privacy_block_slave(Config) ->
    privacy_tests:block_slave(Config).
privacy_unblock_master(Config) ->
    privacy_tests:unblock_master(Config).
privacy_unblock_slave(Config) ->
    privacy_tests:unblock_slave(Config).
privacy_unblock_all_master(Config) ->
    privacy_tests:unblock_all_master(Config).
privacy_unblock_all_slave(Config) ->
    privacy_tests:unblock_all_slave(Config).

vcard(Config) ->
    true = is_feature_advertised(Config, ?NS_VCARD),
    VCard =
        #vcard_temp{fn = <<"Peter Saint-Andre">>,
               n = #vcard_name{family = <<"Saint-Andre">>,
                               given = <<"Peter">>},
               nickname = <<"stpeter">>,
               bday = <<"1966-08-06">>,
               adr = [#vcard_adr{work = true,
                                 extadd = <<"Suite 600">>,
                                 street = <<"1899 Wynkoop Street">>,
                                 locality = <<"Denver">>,
                                 region = <<"CO">>,
                                 pcode = <<"80202">>,
                                 ctry = <<"USA">>},
                      #vcard_adr{home = true,
                                 locality = <<"Denver">>,
                                 region = <<"CO">>,
                                 pcode = <<"80209">>,
                                 ctry = <<"USA">>}],
               tel = [#vcard_tel{work = true,voice = true,
                                 number = <<"303-308-3282">>},
                      #vcard_tel{home = true,voice = true,
                                 number = <<"303-555-1212">>}],
               email = [#vcard_email{internet = true,pref = true,
                                     userid = <<"stpeter@jabber.org">>}],
               jabberid = <<"stpeter@jabber.org">>,
               title = <<"Executive Director">>,role = <<"Patron Saint">>,
               org = #vcard_org{name = <<"XMPP Standards Foundation">>},
               url = <<"http://www.xmpp.org/xsf/people/stpeter.shtml">>,
               desc = <<"More information about me is located on my "
                        "personal website: http://www.saint-andre.com/">>},
    #iq{type = result, sub_els = []} =
        send_recv(Config, #iq{type = set, sub_els = [VCard]}),
    %% TODO: check if VCard == VCard1.
    #iq{type = result, sub_els = [_VCard1]} =
        send_recv(Config, #iq{type = get, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

vcard_get(Config) ->
    true = is_feature_advertised(Config, ?NS_VCARD),
    %% TODO: check if VCard corresponds to LDIF data from ejabberd.ldif
    #iq{type = result, sub_els = [_VCard]} =
        send_recv(Config, #iq{type = get, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

ldap_shared_roster_get(Config) ->
    Item = #roster_item{jid = jid:from_string(<<"user2@ldap.localhost">>), name = <<"Test User 2">>,
                        groups = [<<"group1">>], subscription = both},
    #iq{type = result, sub_els = [#roster_query{items = [Item]}]} =
        send_recv(Config, #iq{type = get, sub_els = [#roster_query{}]}),
    disconnect(Config).

vcard_xupdate_master(Config) ->
    Img = <<137, "PNG\r\n", 26, $\n>>,
    ImgHash = p1_sha:sha(Img),
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    wait_for_slave(Config),
    #presence{from = MyJID, type = available} = send_recv(Config, #presence{}),
    #presence{from = Peer, type = available} = recv_presence(Config),
    VCard = #vcard_temp{photo = #vcard_photo{type = <<"image/png">>, binval = Img}},
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, sub_els = [VCard]}),
    #presence{from = MyJID, type = available,
	      sub_els = [#vcard_xupdate{hash = ImgHash}]} = recv_presence(Config),
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, sub_els = [#vcard_temp{}]}),
    ?recv2(#presence{from = MyJID, type = available,
		     sub_els = [#vcard_xupdate{hash = undefined}]},
	   #presence{from = Peer, type = unavailable}),
    disconnect(Config).

vcard_xupdate_slave(Config) ->
    Img = <<137, "PNG\r\n", 26, $\n>>,
    ImgHash = p1_sha:sha(Img),
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    #presence{from = MyJID, type = available} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    #presence{from = Peer, type = available} = recv_presence(Config),
    #presence{from = Peer, type = available,
	      sub_els = [#vcard_xupdate{hash = ImgHash}]} = recv_presence(Config),
    #presence{from = Peer, type = available,
	      sub_els = [#vcard_xupdate{hash = undefined}]} = recv_presence(Config),
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

test_pubsub_features(Config) ->
    PJID = pubsub_jid(Config),
    AllFeatures = sets:from_list(get_features(Config, PJID)),
    NeededFeatures = sets:from_list(
		       [?NS_PUBSUB,
			?PUBSUB("access-open"),
			?PUBSUB("access-authorize"),
			?PUBSUB("create-nodes"),
			?PUBSUB("instant-nodes"),
			?PUBSUB("config-node"),
			?PUBSUB("retrieve-default"),
			?PUBSUB("create-and-configure"),
			?PUBSUB("publish"),
			?PUBSUB("auto-create"),
			?PUBSUB("retrieve-items"),
			?PUBSUB("delete-items"),
			?PUBSUB("subscribe"),
			?PUBSUB("retrieve-affiliations"),
			?PUBSUB("modify-affiliations"),
			?PUBSUB("retrieve-subscriptions"),
			?PUBSUB("manage-subscriptions"),
			?PUBSUB("purge-nodes"),
			?PUBSUB("delete-nodes")]),
    true = sets:is_subset(NeededFeatures, AllFeatures),
    disconnect(Config).

test_pubsub_create(Config) ->
    Node = ?config(pubsub_node, Config),
    Node = create_node(Config, Node),
    disconnect(Config).

test_pubsub_create_instant(Config) ->
    Node = create_node(Config, <<>>),
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_configure(Config) ->
    Node = ?config(pubsub_node, Config),
    NodeTitle = ?config(pubsub_node_title, Config),
    NodeConfig = get_node_config(Config, Node),
    MyNodeConfig = set_opts(NodeConfig,
			    [{title, NodeTitle}]),
    set_node_config(Config, Node, MyNodeConfig),
    NewNodeConfig = get_node_config(Config, Node),
    NodeTitle = proplists:get_value(title, NewNodeConfig),
    disconnect(Config).

test_pubsub_default(Config) ->
    get_default_node_config(Config),
    disconnect(Config).

test_pubsub_create_configure(Config) ->
    NodeTitle = ?config(pubsub_node_title, Config),
    DefaultNodeConfig = get_default_node_config(Config),
    CustomNodeConfig = set_opts(DefaultNodeConfig,
				[{title, NodeTitle}]),
    Node = create_node(Config, <<>>, CustomNodeConfig),
    NodeConfig = get_node_config(Config, Node),
    NodeTitle = proplists:get_value(title, NodeConfig),
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_publish(Config) ->
    Node = create_node(Config, <<>>),
    publish_item(Config, Node),
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_auto_create(Config) ->
    Node = randoms:get_string(),
    publish_item(Config, Node),
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_get_items(Config) ->
    Node = create_node(Config, <<>>),
    ItemsIn = [publish_item(Config, Node) || _ <- lists:seq(1, 5)],
    ItemsOut = get_items(Config, Node),
    true = [I || #ps_item{id = I} <- lists:sort(ItemsIn)]
	== [I || #ps_item{id = I} <- lists:sort(ItemsOut)],
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_delete_item(Config) ->
    Node = create_node(Config, <<>>),
    #ps_item{id = I} = publish_item(Config, Node),
    [#ps_item{id = I}] = get_items(Config, Node),
    delete_item(Config, Node, I),
    [] = get_items(Config, Node),
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_subscribe(Config) ->
    Node = create_node(Config, <<>>),
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    [#ps_subscription{node = Node}] = get_subscriptions(Config),
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_unsubscribe(Config) ->
    Node = create_node(Config, <<>>),
    subscribe_node(Config, Node),
    [#ps_subscription{node = Node}] = get_subscriptions(Config),
    unsubscribe_node(Config, Node),
    [] = get_subscriptions(Config),
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_get_affiliations(Config) ->
    Nodes = lists:sort([create_node(Config, <<>>) || _ <- lists:seq(1, 5)]),
    Affs = get_affiliations(Config),
    Nodes = lists:sort([Node || #ps_affiliation{node = Node,
						type = owner} <- Affs]),
    [delete_node(Config, Node) || Node <- Nodes],
    disconnect(Config).

test_pubsub_get_subscriptions(Config) ->
    Nodes = lists:sort([create_node(Config, <<>>) || _ <- lists:seq(1, 5)]),
    [subscribe_node(Config, Node) || Node <- Nodes],
    Subs = get_subscriptions(Config),
    Nodes = lists:sort([Node || #ps_subscription{node = Node} <- Subs]),
    [delete_node(Config, Node) || Node <- Nodes],
    disconnect(Config).

test_pubsub_purge(Config) ->
    Node = create_node(Config, <<>>),
    ItemsIn = [publish_item(Config, Node) || _ <- lists:seq(1, 5)],
    ItemsOut = get_items(Config, Node),
    true = [I || #ps_item{id = I} <- lists:sort(ItemsIn)]
	== [I || #ps_item{id = I} <- lists:sort(ItemsOut)],
    purge_node(Config, Node),
    [] = get_items(Config, Node),
    delete_node(Config, Node),
    disconnect(Config).

test_pubsub_delete(Config) ->
    Node = ?config(pubsub_node, Config),
    delete_node(Config, Node),
    disconnect(Config).

pubsub_publish_master(Config) ->
    Node = create_node(Config, <<>>),
    put_event(Config, Node),
    wait_for_slave(Config),
    #ps_item{id = ID} = publish_item(Config, Node),
    #ps_item{id = ID} = get_event(Config),
    delete_node(Config, Node),
    disconnect(Config).

pubsub_publish_slave(Config) ->
    Node = get_event(Config),
    subscribe_node(Config, Node),
    wait_for_master(Config),
    #message{
       sub_els =
	   [#ps_event{
	       items = #ps_items{node = Node,
				 items = [Item]}}]} = recv_message(Config),
    put_event(Config, Item),
    disconnect(Config).

pubsub_subscriptions_master(Config) ->
    Peer = ?config(slave, Config),
    Node = ?config(pubsub_node, Config),
    Node = create_node(Config, Node),
    [] = get_subscriptions(Config, Node),
    wait_for_slave(Config),
    lists:foreach(
      fun(Type) ->
	      ok = set_subscriptions(Config, Node, [{Peer, Type}]),
	      #ps_item{} = publish_item(Config, Node),
	      case get_subscriptions(Config, Node) of
		  [] when Type == none; Type == pending ->
		      ok;
		  [#ps_subscription{jid = Peer, type = Type}] ->
		      ok
	      end
      end, [subscribed, unconfigured, pending, none]),
    delete_node(Config, Node),
    disconnect(Config).

pubsub_subscriptions_slave(Config) ->
    wait_for_master(Config),
    MyJID = my_jid(Config),
    Node = ?config(pubsub_node, Config),
    lists:foreach(
      fun(subscribed = Type) ->
	      ?recv2(#message{
			sub_els =
			    [#ps_event{
				subscription = #ps_subscription{
						  node = Node,
						  jid = MyJID,
						  type = Type}}]},
		     #message{sub_els = [#ps_event{}]});
	 (Type) ->
	      #message{
		 sub_els =
		     [#ps_event{
			 subscription = #ps_subscription{
					   node = Node,
					   jid = MyJID,
					   type = Type}}]} =
		  recv_message(Config)
      end, [subscribed, unconfigured, pending, none]),
    disconnect(Config).

pubsub_affiliations_master(Config) ->
    Peer = ?config(slave, Config),
    BarePeer = jid:remove_resource(Peer),
    lists:foreach(
      fun(Aff) ->
	      Node = <<(atom_to_binary(Aff, utf8))/binary,
		       $-, (randoms:get_string())/binary>>,
	      create_node(Config, Node, default_node_config(Config)),
	      #ps_item{id = I} = publish_item(Config, Node),
	      ok = set_affiliations(Config, Node, [{Peer, Aff}]),
	      Affs = get_affiliations(Config, Node),
	      case lists:keyfind(BarePeer, #ps_affiliation.jid, Affs) of
		  false when Aff == none ->
		      ok;
		  #ps_affiliation{type = Aff} ->
		      ok
	      end,
	      put_event(Config, {Aff, Node, I}),
	      wait_for_slave(Config),
	      delete_node(Config, Node)
      end, [outcast, none, member, publish_only, publisher, owner]),
    put_event(Config, disconnect),
    disconnect(Config).

pubsub_affiliations_slave(Config) ->
    pubsub_affiliations_slave(Config, get_event(Config)).

pubsub_affiliations_slave(Config, {outcast, Node, ItemID}) ->
    #stanza_error{reason = 'forbidden'} = subscribe_node(Config, Node),
    #stanza_error{} = unsubscribe_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_items(Config, Node),
    #stanza_error{reason = 'forbidden'} = publish_item(Config, Node),
    #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    pubsub_affiliations_slave(Config, get_event(Config));
pubsub_affiliations_slave(Config, {none, Node, ItemID}) ->
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    ok = unsubscribe_node(Config, Node),
    %% This violates the affiliation char from section 4.1
    [_|_] = get_items(Config, Node),
    #stanza_error{reason = 'forbidden'} = publish_item(Config, Node),
    #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    pubsub_affiliations_slave(Config, get_event(Config));
pubsub_affiliations_slave(Config, {member, Node, ItemID}) ->
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    ok = unsubscribe_node(Config, Node),
    [_|_] = get_items(Config, Node),
    #stanza_error{reason = 'forbidden'} = publish_item(Config, Node),
    #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    pubsub_affiliations_slave(Config, get_event(Config));
pubsub_affiliations_slave(Config, {publish_only, Node, ItemID}) ->
    #stanza_error{reason = 'forbidden'} = subscribe_node(Config, Node),
    #stanza_error{} = unsubscribe_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_items(Config, Node),
    #ps_item{id = MyItemID} = publish_item(Config, Node),
    %% BUG: This should be fixed
    %% ?match(ok, delete_item(Config, Node, MyItemID)),
    #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    pubsub_affiliations_slave(Config, get_event(Config));
pubsub_affiliations_slave(Config, {publisher, Node, ItemID}) ->
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    ok = unsubscribe_node(Config, Node),
    [_|_] = get_items(Config, Node),
    #ps_item{id = MyItemID} = publish_item(Config, Node),
    ok = delete_item(Config, Node, MyItemID),
    %% BUG: this should be fixed
    %% #stanza_error{reason = 'forbidden'} = delete_item(Config, Node, ItemID),
    #stanza_error{reason = 'forbidden'} = purge_node(Config, Node),
    #stanza_error{reason = 'forbidden'} = get_node_config(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_node_config(Config, Node, default_node_config(Config)),
    #stanza_error{reason = 'forbidden'} = get_subscriptions(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_subscriptions(Config, Node, [{my_jid(Config), subscribed}]),
    #stanza_error{reason = 'forbidden'} = get_affiliations(Config, Node),
    #stanza_error{reason = 'forbidden'} =
	set_affiliations(Config, Node, [{?config(master, Config), outcast},
					{my_jid(Config), owner}]),
    #stanza_error{reason = 'forbidden'} = delete_node(Config, Node),
    wait_for_master(Config),
    pubsub_affiliations_slave(Config, get_event(Config));
pubsub_affiliations_slave(Config, {owner, Node, ItemID}) ->
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    #ps_subscription{type = subscribed} = subscribe_node(Config, Node),
    ok = unsubscribe_node(Config, Node),
    [_|_] = get_items(Config, Node),
    #ps_item{id = MyItemID} = publish_item(Config, Node),
    ok = delete_item(Config, Node, MyItemID),
    ok = delete_item(Config, Node, ItemID),
    ok = purge_node(Config, Node),
    [_|_] = get_node_config(Config, Node),
    ok = set_node_config(Config, Node, default_node_config(Config)),
    ok = set_subscriptions(Config, Node, []),
    [] = get_subscriptions(Config, Node),
    ok = set_affiliations(Config, Node, [{Peer, outcast}, {MyJID, owner}]),
    [_, _] = get_affiliations(Config, Node),
    ok = delete_node(Config, Node),
    wait_for_master(Config),
    pubsub_affiliations_slave(Config, get_event(Config));
pubsub_affiliations_slave(Config, disconnect) ->
    disconnect(Config).

pubsub_authorize_master(Config) ->
    send(Config, #presence{}),
    #presence{} = recv_presence(Config),
    Peer = ?config(slave, Config),
    PJID = pubsub_jid(Config),
    NodeConfig = set_opts(default_node_config(Config),
			  [{access_model, authorize}]),
    Node = ?config(pubsub_node, Config),
    Node = create_node(Config, Node, NodeConfig),
    wait_for_slave(Config),
    #message{sub_els = [#xdata{fields = F1}]} = recv_message(Config),
    C1 = pubsub_subscribe_authorization:decode(F1),
    Node = proplists:get_value(node, C1),
    Peer = proplists:get_value(subscriber_jid, C1),
    %% Deny it at first
    Deny = #xdata{type = submit,
		  fields = pubsub_subscribe_authorization:encode(
			     [{node, Node},
			      {subscriber_jid, Peer},
			      {allow, false}])},
    send(Config, #message{to = PJID, sub_els = [Deny]}),
    %% We should not have any subscriptions
    [] = get_subscriptions(Config, Node),
    wait_for_slave(Config),
    #message{sub_els = [#xdata{fields = F2}]} = recv_message(Config),
    C2 = pubsub_subscribe_authorization:decode(F2),
    Node = proplists:get_value(node, C2),
    Peer = proplists:get_value(subscriber_jid, C2),
    %% Now we accept is as the peer is very insisting ;)
    Approve = #xdata{type = submit,
		     fields = pubsub_subscribe_authorization:encode(
				[{node, Node},
				 {subscriber_jid, Peer},
				 {allow, true}])},
    send(Config, #message{to = PJID, sub_els = [Approve]}),
    wait_for_slave(Config),
    delete_node(Config, Node),
    disconnect(Config).

pubsub_authorize_slave(Config) ->
    Node = ?config(pubsub_node, Config),
    MyJID = my_jid(Config),
    wait_for_master(Config),
    #ps_subscription{type = pending} = subscribe_node(Config, Node),
    %% We're denied at first
    #message{
       sub_els =
	   [#ps_event{
	       subscription = #ps_subscription{type = none,
					       jid = MyJID}}]} =
	recv_message(Config),
    wait_for_master(Config),
    #ps_subscription{type = pending} = subscribe_node(Config, Node),
    %% Now much better!
    #message{
       sub_els =
	   [#ps_event{
	       subscription = #ps_subscription{type = subscribed,
					       jid = MyJID}}]} =
	recv_message(Config),
    wait_for_master(Config),
    disconnect(Config).

create_node(Config, Node) ->
    create_node(Config, Node, undefined).

create_node(Config, Node, Options) ->
    PJID = pubsub_jid(Config),
    NodeConfig = if is_list(Options) ->
			 #xdata{type = submit,
				fields = pubsub_node_config:encode(Options)};
		    true ->
			 undefined
		 end,
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{create = Node,
					  configure = {<<>>, NodeConfig}}]}) of
	#iq{type = result, sub_els = [#pubsub{create = NewNode}]} ->
	    NewNode;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

delete_node(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{delete = {Node, <<>>}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

purge_node(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{purge = Node}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_default_node_config(Config) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub_owner{default = {<<>>, undefined}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub_owner{default = {<<>>, NodeConfig}}]} ->
	    pubsub_node_config:decode(NodeConfig#xdata.fields);
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_node_config(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub_owner{configure = {Node, undefined}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub_owner{configure = {Node, NodeConfig}}]} ->
	    pubsub_node_config:decode(NodeConfig#xdata.fields);
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

set_node_config(Config, Node, Options) ->
    PJID = pubsub_jid(Config),
    NodeConfig = #xdata{type = submit,
			fields = pubsub_node_config:encode(Options)},
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{configure =
						    {Node, NodeConfig}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

publish_item(Config, Node) ->
    PJID = pubsub_jid(Config),
    ItemID = randoms:get_string(),
    Item = #ps_item{id = ItemID, xml_els = [xmpp:encode(#presence{id = ItemID})]},
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{publish = #ps_publish{
						       node = Node,
						       items = [Item]}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub{publish = #ps_publish{
					    node = Node,
					    items = [#ps_item{id = ItemID}]}}]} ->
	    Item;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_items(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub{items = #ps_items{node = Node}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub{items = #ps_items{node = Node, items = Items}}]} ->
	    Items;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

delete_item(Config, Node, I) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{retract =
					      #ps_retract{
						 node = Node,
						 items = [#ps_item{id = I}]}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

subscribe_node(Config, Node) ->
    PJID = pubsub_jid(Config),
    MyJID = my_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{subscribe = #ps_subscribe{
							 node = Node,
							 jid = MyJID}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub{
			  subscription = #ps_subscription{
					    node = Node,
					    jid = MyJID} = Sub}]} ->
	    Sub;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

unsubscribe_node(Config, Node) ->
    PJID = pubsub_jid(Config),
    MyJID = my_jid(Config),
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub{
				     unsubscribe = #ps_unsubscribe{
						      node = Node,
						      jid = MyJID}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_affiliations(Config) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub{affiliations = {<<>>, []}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub{affiliations = {<<>>, Affs}}]} ->
	    Affs;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_affiliations(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub_owner{affiliations = {Node, []}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub_owner{affiliations = {Node, Affs}}]} ->
	    Affs;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

set_affiliations(Config, Node, JTs) ->
    PJID = pubsub_jid(Config),
    Affs = [#ps_affiliation{jid = J, type = T} || {J, T} <- JTs],
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{affiliations =
						    {Node, Affs}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_subscriptions(Config) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub{subscriptions = {<<>>, []}}]}) of
	#iq{type = result, sub_els = [#pubsub{subscriptions = {<<>>, Subs}}]} ->
	    Subs;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

get_subscriptions(Config, Node) ->
    PJID = pubsub_jid(Config),
    case send_recv(Config,
		   #iq{type = get, to = PJID,
		       sub_els = [#pubsub_owner{subscriptions = {Node, []}}]}) of
	#iq{type = result,
	    sub_els = [#pubsub_owner{subscriptions = {Node, Subs}}]} ->
	    Subs;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

set_subscriptions(Config, Node, JTs) ->
    PJID = pubsub_jid(Config),
    Subs = [#ps_subscription{jid = J, type = T} || {J, T} <- JTs],
    case send_recv(Config,
		   #iq{type = set, to = PJID,
		       sub_els = [#pubsub_owner{subscriptions =
						    {Node, Subs}}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = IQ ->
	    xmpp:get_subtag(IQ, #stanza_error{})
    end.

default_node_config(Config) ->
    [{title, ?config(pubsub_node_title, Config)},
     {notify_delete, false},
     {send_last_published_item, never}].

mix_master(Config) ->
    MIX = mix_jid(Config),
    Room = mix_room_jid(Config),
    MyJID = my_jid(Config),
    MyBareJID = jid:remove_resource(MyJID),
    true = is_feature_advertised(Config, ?NS_MIX_0, MIX),
    #iq{type = result,
	sub_els =
	    [#disco_info{
		identities = [#identity{category = <<"conference">>,
					type = <<"text">>}],
		xdata = [#xdata{type = result, fields = XFields}]}]} =
	send_recv(Config, #iq{type = get, to = MIX, sub_els = [#disco_info{}]}),
    true = lists:any(
	     fun(#xdata_field{var = <<"FORM_TYPE">>,
			      values = [?NS_MIX_SERVICEINFO_0]}) -> true;
		(_) -> false
	     end, XFields),
    %% Joining
    Nodes = [?NS_MIX_NODES_MESSAGES, ?NS_MIX_NODES_PRESENCE,
	     ?NS_MIX_NODES_PARTICIPANTS, ?NS_MIX_NODES_SUBJECT,
	     ?NS_MIX_NODES_CONFIG],
    #iq{type = result,
	sub_els = [#mix_join{subscribe = Nodes, jid = MyBareJID}]} =
	send_recv(Config, #iq{type = set, to = Room,
			      sub_els = [#mix_join{subscribe = Nodes}]}),
    #message{from = Room,
	     sub_els =
		 [#ps_event{
		     items = #ps_items{
				node = ?NS_MIX_NODES_PARTICIPANTS,
				items = [#ps_item{
					    id = ParticipantID,
					    xml_els = [PXML]}]}}]} =
	recv_message(Config),
    #mix_participant{jid = MyBareJID} = xmpp:decode(PXML),
    %% Coming online
    PresenceID = randoms:get_string(),
    Presence = xmpp:encode(#presence{}),
    #iq{type = result,
	sub_els =
	    [#pubsub{
		publish = #ps_publish{
			     node = ?NS_MIX_NODES_PRESENCE,
			     items = [#ps_item{id = PresenceID}]}}]} =
	send_recv(
	  Config,
	  #iq{type = set, to = Room,
	      sub_els =
		  [#pubsub{
		      publish = #ps_publish{
				   node = ?NS_MIX_NODES_PRESENCE,
				   items = [#ps_item{
					       id = PresenceID,
					       xml_els = [Presence]}]}}]}),
    #message{from = Room,
	     sub_els =
		 [#ps_event{
		     items = #ps_items{
				node = ?NS_MIX_NODES_PRESENCE,
				items = [#ps_item{
					    id = PresenceID,
					    xml_els = [Presence]}]}}]} =
	recv_message(Config),
    %% Coming offline
    send(Config, #presence{type = unavailable, to = Room}),
    %% Receiving presence retract event
    #message{from = Room,
	     sub_els = [#ps_event{
			   items = #ps_items{
				      node = ?NS_MIX_NODES_PRESENCE,
				      retract = PresenceID}}]} =
	recv_message(Config),
    %% Leaving
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, to = Room, sub_els = [#mix_leave{}]}),
    #message{from = Room,
	     sub_els =
		 [#ps_event{
		     items = #ps_items{
				node = ?NS_MIX_NODES_PARTICIPANTS,
				retract = ParticipantID}}]} =
	recv_message(Config),
    put_event(Config, disconnect),
    disconnect(Config).

mix_slave(Config) ->
    disconnect = get_event(Config),
    disconnect(Config).

proxy65_master(Config) ->
    Proxy = proxy_jid(Config),
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    wait_for_slave(Config),
    send(Config, #presence{}),
    #presence{from = MyJID, type = available} = recv_presence(Config),
    true = is_feature_advertised(Config, ?NS_BYTESTREAMS, Proxy),
    #iq{type = result, sub_els = [#bytestreams{hosts = [StreamHost]}]} =
        send_recv(
          Config,
          #iq{type = get, sub_els = [#bytestreams{}], to = Proxy}),
    SID = randoms:get_string(),
    Data = crypto:rand_bytes(1024),
    put_event(Config, {StreamHost, SID, Data}),
    Socks5 = socks5_connect(StreamHost, {SID, MyJID, Peer}),
    wait_for_slave(Config),
    #iq{type = result, sub_els = []} =
        send_recv(Config,
                  #iq{type = set, to = Proxy,
                      sub_els = [#bytestreams{activate = Peer, sid = SID}]}),
    socks5_send(Socks5, Data),
    %%?recv1(#presence{type = unavailable, from = Peer}),
    disconnect(Config).

proxy65_slave(Config) ->
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    send(Config, #presence{}),
    #presence{from = MyJID, type = available} = recv_presence(Config),
    wait_for_master(Config),
    {StreamHost, SID, Data} = get_event(Config),
    Socks5 = socks5_connect(StreamHost, {SID, Peer, MyJID}),
    wait_for_master(Config),
    socks5_recv(Socks5, Data),
    disconnect(Config).

send_messages_to_room(Config, Range) ->
    MyNick = ?config(master_nick, Config),
    Room = muc_room_jid(Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    lists:foreach(
      fun(N) ->
              Text = #text{data = integer_to_binary(N)},
	      #message{from = MyNickJID, id = I,
		       type = groupchat,
		       body = [Text]} =
		  send_recv(Config, #message{to = Room, body = [Text],
					     type = groupchat})
      end, Range).

retrieve_messages_from_room_via_mam(Config, Range) ->
    MyNick = ?config(master_nick, Config),
    Room = muc_room_jid(Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    MyJID = my_jid(Config),
    QID = randoms:get_string(),
    Count = length(Range),
    I = send(Config, #iq{type = set, to = Room,
			 sub_els = [#mam_query{xmlns = ?NS_MAM_1, id = QID}]}),
    lists:foreach(
      fun(N) ->
	      Text = #text{data = integer_to_binary(N)},
	      #message{
		 to = MyJID, from = Room,
		 sub_els =
		     [#mam_result{
			 xmlns = ?NS_MAM_1,
			 queryid = QID,
			 sub_els =
			     [#forwarded_decoded{
				 delay = #delay{},
				 sub_els = [#message{
					       from = MyNickJID,
					       type = groupchat,
					       body = [Text]}]}]}]} =
		  recv_message(Config)
      end, Range),
    #iq{from = Room, id = I, type = result,
	sub_els = [#mam_fin{xmlns = ?NS_MAM_1,
			    id = QID,
			    rsm = #rsm_set{count = Count},
			    complete = true}]} = recv_iq(Config).

muc_mam_master(Config) ->
    MyNick = ?config(master_nick, Config),
    Room = muc_room_jid(Config),
    MyNickJID = jid:replace_resource(Room, MyNick),
    %% Joining
    ok = muc_tests:muc_join_new(Config),
    %% MAM feature should not be advertised at this point,
    %% because MAM is not enabled so far
    false = is_feature_advertised(Config, ?NS_MAM_1, Room),
    %% Fill in some history
    send_messages_to_room(Config, lists:seq(1, 21)),
    %% We now should be able to retrieve those via MAM, even though
    %% MAM is disabled. However, only last 20 messages should be received.
    retrieve_messages_from_room_via_mam(Config, lists:seq(2, 21)),
    %% Now enable MAM for the conference
    %% Retrieve config first
    #iq{type = result, sub_els = [#muc_owner{config = #xdata{} = RoomCfg}]} =
        send_recv(Config, #iq{type = get, sub_els = [#muc_owner{}],
                              to = Room}),
    %% Find the MAM field in the config and enable it
    NewFields = lists:flatmap(
		  fun(#xdata_field{var = <<"mam">> = Var}) ->
			  [#xdata_field{var = Var, values = [<<"1">>]}];
		     (_) ->
			  []
		  end, RoomCfg#xdata.fields),
    NewRoomCfg = #xdata{type = submit, fields = NewFields},
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, to = Room,
			      sub_els = [#muc_owner{config = NewRoomCfg}]}),
    #message{from = Room, type = groupchat,
	     sub_els = [#muc_user{status_codes = [104]}]} = recv_message(Config),
    %% Check if MAM has been enabled
    true = is_feature_advertised(Config, ?NS_MAM_1, Room),
    %% We now sending some messages again
    send_messages_to_room(Config, lists:seq(1, 5)),
    %% And retrieve them via MAM again.
    retrieve_messages_from_room_via_mam(Config, lists:seq(1, 5)),
    put_event(Config, disconnect),
    disconnect(Config).

muc_mam_slave(Config) ->
    disconnect = get_event(Config),
    disconnect(Config).

%% OK, I know this is retarded, but I didn't find a better way to
%% split the test cases into different modules
muc_service_presence_error(Config) ->
    muc_tests:muc_service_presence_error(Config).
muc_service_message_error(Config) ->
    muc_tests:muc_service_message_error(Config).
muc_service_unknown_ns_iq_error(Config) ->
    muc_tests:muc_service_unknown_ns_iq_error(Config).
muc_service_iq_set_error(Config) ->
    muc_tests:muc_service_iq_set_error(Config).
muc_service_improper_iq_error(Config) ->
    muc_tests:muc_service_improper_iq_error(Config).
muc_service_features(Config) ->
    muc_tests:muc_service_features(Config).
muc_service_disco_info_node_error(Config) ->
    muc_tests:muc_service_disco_info_node_error(Config).
muc_service_disco_items(Config) ->
    muc_tests:muc_service_disco_items(Config).
muc_service_vcard(Config) ->
    muc_tests:muc_service_vcard(Config).
muc_service_unique(Config) ->
    muc_tests:muc_service_unique(Config).
muc_service_subscriptions(Config) ->
    muc_tests:muc_service_subscriptions(Config).
muc_configure_non_existent(Config) ->
    muc_tests:muc_configure_non_existent(Config).
muc_cancel_configure_non_existent(Config) ->
    muc_tests:muc_cancel_configure_non_existent(Config).

muc_register_master(Config) ->
    muc_tests:muc_register_master(Config).
muc_register_slave(Config) ->
    muc_tests:muc_register_slave(Config).
muc_join_conflict_master(Config) ->
    muc_tests:muc_join_conflict_master(Config).
muc_join_conflict_slave(Config) ->
    muc_tests:muc_join_conflict_slave(Config).
muc_groupchat_msg_master(Config) ->
    muc_tests:muc_groupchat_msg_master(Config).
muc_groupchat_msg_slave(Config) ->
    muc_tests:muc_groupchat_msg_slave(Config).
muc_private_msg_master(Config) ->
    muc_tests:muc_private_msg_master(Config).
muc_private_msg_slave(Config) ->
    muc_tests:muc_private_msg_slave(Config).
muc_set_subject_master(Config) ->
    muc_tests:muc_set_subject_master(Config).
muc_set_subject_slave(Config) ->
    muc_tests:muc_set_subject_slave(Config).
muc_history_master(Config) ->
    muc_tests:muc_history_master(Config).
muc_history_slave(Config) ->
    muc_tests:muc_history_slave(Config).
muc_invite_master(Config) ->
    muc_tests:muc_invite_master(Config).
muc_invite_slave(Config) ->
    muc_tests:muc_invite_slave(Config).
muc_invite_members_only_master(Config) ->
    muc_tests:muc_invite_members_only_master(Config).
muc_invite_members_only_slave(Config) ->
    muc_tests:muc_invite_members_only_slave(Config).
muc_invite_password_protected_master(Config) ->
    muc_tests:muc_invite_password_protected_master(Config).
muc_invite_password_protected_slave(Config) ->
    muc_tests:muc_invite_password_protected_slave(Config).
muc_voice_request_master(Config) ->
    muc_tests:muc_voice_request_master(Config).
muc_voice_request_slave(Config) ->
    muc_tests:muc_voice_request_slave(Config).
muc_change_role_master(Config) ->
    muc_tests:muc_change_role_master(Config).
muc_change_role_slave(Config) ->
    muc_tests:muc_change_role_slave(Config).
muc_kick_master(Config) ->
    muc_tests:muc_kick_master(Config).
muc_kick_slave(Config) ->
    muc_tests:muc_kick_slave(Config).
muc_change_affiliation_master(Config) ->
    muc_tests:muc_change_affiliation_master(Config).
muc_change_affiliation_slave(Config) ->
    muc_tests:muc_change_affiliation_slave(Config).
muc_destroy_master(Config) ->
    muc_tests:muc_destroy_master(Config).
muc_destroy_slave(Config) ->
    muc_tests:muc_destroy_slave(Config).
muc_vcard_master(Config) ->
    muc_tests:muc_vcard_master(Config).
muc_vcard_slave(Config) ->
    muc_tests:muc_vcard_slave(Config).
muc_nick_change_master(Config) ->
    muc_tests:muc_nick_change_master(Config).
muc_nick_change_slave(Config) ->
    muc_tests:muc_nick_change_slave(Config).
muc_config_title_desc_master(Config) ->
    muc_tests:muc_config_title_desc_master(Config).
muc_config_title_desc_slave(Config) ->
    muc_tests:muc_config_title_desc_slave(Config).
muc_config_public_list_master(Config) ->
    muc_tests:muc_config_public_list_master(Config).
muc_config_public_list_slave(Config) ->
    muc_tests:muc_config_public_list_slave(Config).
muc_config_password_master(Config) ->
    muc_tests:muc_config_password_master(Config).
muc_config_password_slave(Config) ->
    muc_tests:muc_config_password_slave(Config).
muc_config_whois_master(Config) ->
    muc_tests:muc_config_whois_master(Config).
muc_config_whois_slave(Config) ->
    muc_tests:muc_config_whois_slave(Config).
muc_config_members_only_master(Config) ->
    muc_tests:muc_config_members_only_master(Config).
muc_config_members_only_slave(Config) ->
    muc_tests:muc_config_members_only_slave(Config).
muc_config_moderated_master(Config) ->
    muc_tests:muc_config_moderated_master(Config).
muc_config_moderated_slave(Config) ->
    muc_tests:muc_config_moderated_slave(Config).
muc_config_private_messages_master(Config) ->
    muc_tests:muc_config_private_messages_master(Config).
muc_config_private_messages_slave(Config) ->
    muc_tests:muc_config_private_messages_slave(Config).
muc_config_query_master(Config) ->
    muc_tests:muc_config_query_master(Config).
muc_config_query_slave(Config) ->
    muc_tests:muc_config_query_slave(Config).
muc_config_allow_invites_master(Config) ->
    muc_tests:muc_config_allow_invites_master(Config).
muc_config_allow_invites_slave(Config) ->
    muc_tests:muc_config_allow_invites_slave(Config).
muc_config_visitor_status_master(Config) ->
    muc_tests:muc_config_visitor_status_master(Config).
muc_config_visitor_status_slave(Config) ->
    muc_tests:muc_config_visitor_status_slave(Config).
muc_config_allow_voice_requests_master(Config) ->
    muc_tests:muc_config_allow_voice_requests_master(Config).
muc_config_allow_voice_requests_slave(Config) ->
    muc_tests:muc_config_allow_voice_requests_slave(Config).
muc_config_voice_request_interval_master(Config) ->
    muc_tests:muc_config_voice_request_interval_master(Config).
muc_config_voice_request_interval_slave(Config) ->
    muc_tests:muc_config_voice_request_interval_slave(Config).
muc_config_visitor_nickchange_master(Config) ->
    muc_tests:muc_config_visitor_nickchange_master(Config).
muc_config_visitor_nickchange_slave(Config) ->
    muc_tests:muc_config_visitor_nickchange_slave(Config).

offline_feature_enabled(Config) ->
    offline_tests:feature_enabled(Config).
offline_check_identity(Config) ->
    offline_tests:check_identity(Config).
offline_send_non_existent(Config) ->
    offline_tests:send_non_existent(Config).
offline_view_non_existent(Config) ->
    offline_tests:view_non_existent(Config).
offline_remove_non_existent(Config) ->
    offline_tests:remove_non_existent(Config).
offline_view_non_integer(Config) ->
    offline_tests:view_non_integer(Config).
offline_remove_non_integer(Config) ->
    offline_tests:remove_non_integer(Config).
offline_malformed_iq(Config) ->
    offline_tests:malformed_iq(Config).
offline_wrong_user(Config) ->
    offline_tests:wrong_user(Config).
offline_unsupported_iq(Config) ->
    offline_tests:unsupported_iq(Config).
offline_flex_master(Config) ->
    offline_tests:flex_master(Config).
offline_flex_slave(Config) ->
    offline_tests:flex_slave(Config).
offline_send_all_master(Config) ->
    offline_tests:send_all_master(Config).
offline_send_all_slave(Config) ->
    offline_tests:send_all_slave(Config).

announce_master(Config) ->
    MyJID = my_jid(Config),
    ServerJID = server_jid(Config),
    MotdJID = jid:replace_resource(ServerJID, <<"announce/motd">>),
    MotdText = #text{data = <<"motd">>},
    #presence{from = MyJID} = send_recv(Config, #presence{}),
    %% Set message of the day
    send(Config, #message{to = MotdJID, body = [MotdText]}),
    %% Receive this message back
    #message{from = ServerJID, body = [MotdText]} = recv_message(Config),
    disconnect(Config).

announce_slave(Config) ->
    MyJID = my_jid(Config),
    ServerJID = server_jid(Config),
    MotdDelJID = jid:replace_resource(ServerJID, <<"announce/motd/delete">>),
    MotdText = #text{data = <<"motd">>},
    #presence{from = MyJID} = send_recv(Config, #presence{}),
    #message{from = ServerJID, body = [MotdText]} = recv_message(Config),
    %% Delete message of the day
    send(Config, #message{to = MotdDelJID}),
    disconnect(Config).

carbons_master(Config) ->
    MyJID = my_jid(Config),
    MyBareJID = jid:remove_resource(MyJID),
    Peer = ?config(slave, Config),
    Txt = #text{data = <<"body">>},
    true = is_feature_advertised(Config, ?NS_CARBONS_2),
    #presence{from = MyJID} = send_recv(Config, #presence{priority = 10}),
    wait_for_slave(Config),
    #presence{from = Peer} = recv_presence(Config),
    %% Enable carbons
    #iq{type = result, sub_els = []} =
	send_recv(Config,
		  #iq{type = set,
		      sub_els = [#carbons_enable{}]}),
    %% Send a message to bare and full JID
    send(Config, #message{to = MyBareJID, type = chat, body = [Txt]}),
    send(Config, #message{to = MyJID, type = chat, body = [Txt]}),
    send(Config, #message{to = MyBareJID, type = chat, body = [Txt],
			  sub_els = [#carbons_private{}]}),
    send(Config, #message{to = MyJID, type = chat, body = [Txt],
			  sub_els = [#carbons_private{}]}),
    %% Receive the messages back
    ?recv4(#message{from = MyJID, to = MyBareJID, type = chat,
		    body = [Txt], sub_els = []},
	   #message{from = MyJID, to = MyJID, type = chat,
		    body = [Txt], sub_els = []},
	   #message{from = MyJID, to = MyBareJID, type = chat,
		    body = [Txt], sub_els = [#carbons_private{}]},
	   #message{from = MyJID, to = MyJID, type = chat,
		    body = [Txt], sub_els = [#carbons_private{}]}),
    %% Disable carbons
    #iq{type = result, sub_els = []} =
	send_recv(Config,
		  #iq{type = set,
		      sub_els = [#carbons_disable{}]}),
    wait_for_slave(Config),
    %% Repeat the same and leave
    send(Config, #message{to = MyBareJID, type = chat, body = [Txt]}),
    send(Config, #message{to = MyJID, type = chat, body = [Txt]}),
    send(Config, #message{to = MyBareJID, type = chat, body = [Txt],
			  sub_els = [#carbons_private{}]}),
    send(Config, #message{to = MyJID, type = chat, body = [Txt],
			  sub_els = [#carbons_private{}]}),
    ?recv4(#message{from = MyJID, to = MyBareJID, type = chat,
		    body = [Txt], sub_els = []},
	   #message{from = MyJID, to = MyJID, type = chat,
		    body = [Txt], sub_els = []},
	   #message{from = MyJID, to = MyBareJID, type = chat,
		    body = [Txt], sub_els = [#carbons_private{}]},
	   #message{from = MyJID, to = MyJID, type = chat,
		    body = [Txt], sub_els = [#carbons_private{}]}),
    disconnect(Config).

carbons_slave(Config) ->
    MyJID = my_jid(Config),
    MyBareJID = jid:remove_resource(MyJID),
    Peer = ?config(master, Config),
    Txt = #text{data = <<"body">>},
    wait_for_master(Config),
    #presence{from = MyJID} = send_recv(Config, #presence{priority = 5}),
    #presence{from = Peer} = recv_presence(Config),
    %% Enable carbons
    #iq{type = result, sub_els = []} =
	send_recv(Config,
		  #iq{type = set,
		      sub_els = [#carbons_enable{}]}),
    %% Receive messages sent by the peer
    ?recv4(
       #message{from = MyBareJID, to = MyJID, type = chat,
		sub_els =
		    [#carbons_sent{
			forwarded = #forwarded_decoded{
				       sub_els =
					   [#message{from = Peer,
						     to = MyBareJID,
						     type = chat,
						     body = [Txt]}]}}]},
       #message{from = MyBareJID, to = MyJID, type = chat,
		sub_els =
		    [#carbons_sent{
			forwarded = #forwarded_decoded{
				       sub_els =
					   [#message{from = Peer,
						     to = Peer,
						     type = chat,
						     body = [Txt]}]}}]},
       #message{from = MyBareJID, to = MyJID, type = chat,
		sub_els =
		    [#carbons_received{
			forwarded = #forwarded_decoded{
				       sub_els =
					   [#message{from = Peer,
						     to = MyBareJID,
						     type = chat,
						     body = [Txt]}]}}]},
       #message{from = MyBareJID, to = MyJID, type = chat,
		sub_els =
		    [#carbons_received{
			forwarded = #forwarded_decoded{
				       sub_els =
					   [#message{from = Peer,
						     to = Peer,
						     type = chat,
						     body = [Txt]}]}}]}),
    %% Disable carbons
    #iq{type = result, sub_els = []} =
	send_recv(Config,
		  #iq{type = set,
		      sub_els = [#carbons_disable{}]}),
    wait_for_master(Config),
    %% Now we should receive nothing but presence unavailable from the peer
    #presence{from = Peer, type = unavailable} = recv_presence(Config),
    disconnect(Config).

mam_old_master(Config) ->
    mam_master(Config, ?NS_MAM_TMP).

mam_new_master(Config) ->
    mam_master(Config, ?NS_MAM_0).

mam_master(Config, NS) ->
    true = is_feature_advertised(Config, NS),
    MyJID = my_jid(Config),
    BareMyJID = jid:remove_resource(MyJID),
    Peer = ?config(slave, Config),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_slave(Config),
    #presence{from = Peer} = recv_presence(Config),
    #iq{type = result, sub_els = [#mam_prefs{xmlns = NS, default = roster}]} =
        send_recv(Config,
                  #iq{type = set,
                      sub_els = [#mam_prefs{xmlns = NS,
					    default = roster,
                                            never = [MyJID]}]}),
    if NS == ?NS_MAM_TMP ->
	    %% NOTE: The server should strip fake archived tags,
	    %% i.e. the sub_els received should be [].
	    FakeArchived = #mam_archived{id = randoms:get_string(),
					 by = server_jid(Config)},
	    #message{body = [#text{data = <<"a">>}], sub_els = []} =
		send_recv(Config, #message{to = MyJID,
					   sub_els = [FakeArchived],
					   body = [#text{data = <<"a">>}]}),
	    #message{body = [#text{data = <<"b">>}], sub_els = []} =
		send_recv(Config, #message{to = BareMyJID,
					   sub_els = [FakeArchived],
					   body = [#text{data = <<"b">>}]});
       true ->
	    ok
    end,
    wait_for_slave(Config),
    lists:foreach(
      fun(N) ->
              Text = #text{data = integer_to_binary(N)},
              send(Config, #message{to = Peer, body = [Text]})
      end, lists:seq(1, 5)),
    #presence{type = unavailable, from = Peer} = recv_presence(Config),
    mam_query_all(Config, NS),
    mam_query_with(Config, Peer, NS),
    %% mam_query_with(Config, jid:remove_resource(Peer)),
    mam_query_rsm(Config, NS),
    #iq{type = result, sub_els = [#mam_prefs{xmlns = NS, default = never}]} =
        send_recv(Config, #iq{type = set,
                              sub_els = [#mam_prefs{xmlns = NS,
						    default = never}]}),
    disconnect(Config).

mam_old_slave(Config) ->
    mam_slave(Config, ?NS_MAM_TMP).

mam_new_slave(Config) ->
    mam_slave(Config, ?NS_MAM_0).

mam_slave(Config, NS) ->
    Peer = ?config(master, Config),
    MyJID = my_jid(Config),
    ServerJID = server_jid(Config),
    wait_for_master(Config),
    #presence{from = MyJID} = send_recv(Config, #presence{}),
    #presence{from = Peer} = recv_presence(Config),
    #iq{type = result, sub_els = [#mam_prefs{xmlns = NS, default = always}]} =
        send_recv(Config,
                  #iq{type = set,
                      sub_els = [#mam_prefs{xmlns = NS, default = always}]}),
    wait_for_master(Config),
    lists:foreach(
      fun(N) ->
              Text = #text{data = integer_to_binary(N)},
	      Msg = #message{from = Peer, body = [Text]} = recv_message(Config),
	      #mam_archived{by = ServerJID} =
		  xmpp:get_subtag(Msg, #mam_archived{}),
	      #stanza_id{by = ServerJID} =
		  xmpp:get_subtag(Msg, #stanza_id{})
      end, lists:seq(1, 5)),
    #iq{type = result, sub_els = [#mam_prefs{xmlns = NS, default = never}]} =
        send_recv(Config, #iq{type = set,
                              sub_els = [#mam_prefs{xmlns = NS, default = never}]}),
    disconnect(Config).

mam_query_all(Config, NS) ->
    QID = randoms:get_string(),
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    Type = case NS of
	       ?NS_MAM_TMP -> get;
	       _ -> set
	   end,
    I = send(Config, #iq{type = Type, sub_els = [#mam_query{xmlns = NS, id = QID}]}),
    maybe_recv_iq_result(Config, NS, I),
    Iter = if NS == ?NS_MAM_TMP -> lists:seq(1, 5);
	      true -> lists:seq(1, 5) ++ lists:seq(1, 5)
	   end,
    lists:foreach(
      fun(N) ->
              Text = #text{data = integer_to_binary(N)},
              #message{to = MyJID,
                       sub_els =
                           [#mam_result{
                               queryid = QID,
                               sub_els =
				   [#forwarded_decoded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]} =
		  recv_message(Config)
      end, Iter),
    if NS == ?NS_MAM_TMP ->
	    #iq{type = result, id = I,
		sub_els = [#mam_query{xmlns = NS, id = QID}]} = recv_iq(Config);
       true ->
	    #message{sub_els = [#mam_fin{complete = true, id = QID}]} =
		recv_message(Config)
    end.

mam_query_with(Config, JID, NS) ->
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    {Query, Type} = if NS == ?NS_MAM_TMP ->
		    {#mam_query{xmlns = NS, with = JID}, get};
	       true ->
		    Fs = [#xdata_field{var = <<"jid">>,
				       values = [jid:to_string(JID)]}],
		    {#mam_query{xmlns = NS,
			       xdata = #xdata{type = submit, fields = Fs}}, set}
	    end,
    I = send(Config, #iq{type = Type, sub_els = [Query]}),
    Iter = if NS == ?NS_MAM_TMP -> lists:seq(1, 5);
	      true -> lists:seq(1, 5) ++ lists:seq(1, 5)
	   end,
    maybe_recv_iq_result(Config, NS, I),
    lists:foreach(
      fun(N) ->
              Text = #text{data = integer_to_binary(N)},
              #message{to = MyJID,
                       sub_els =
                           [#mam_result{
                               sub_els =
				   [#forwarded_decoded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]} =
		  recv_message(Config)
      end, Iter),
    if NS == ?NS_MAM_TMP ->
	    #iq{type = result, id = I,
		sub_els = [#mam_query{xmlns = NS}]} = recv_iq(Config);
       true ->
	    #message{sub_els = [#mam_fin{complete = true}]} =
		recv_message(Config)
    end.

maybe_recv_iq_result(Config, ?NS_MAM_0, I1) ->
    #iq{type = result, id = I1} = recv_iq(Config);
maybe_recv_iq_result(_, _, _) ->
    ok.

mam_query_rsm(Config, NS) ->
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    Type = case NS of
	       ?NS_MAM_TMP -> get;
	       _ -> set
	   end,
    %% Get the first 3 items out of 5
    I1 = send(Config,
              #iq{type = Type,
                  sub_els = [#mam_query{xmlns = NS, rsm = #rsm_set{max = 3}}]}),
    maybe_recv_iq_result(Config, NS, I1),
    lists:foreach(
      fun(N) ->
              Text = #text{data = integer_to_binary(N)},
              #message{to = MyJID,
                       sub_els =
                           [#mam_result{
			       xmlns = NS,
                               sub_els =
				   [#forwarded_decoded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]} =
		  recv_message(Config)
      end, lists:seq(1, 3)),
    if NS == ?NS_MAM_TMP ->
	    #iq{type = result, id = I1,
		sub_els = [#mam_query{xmlns = NS,
				      rsm = #rsm_set{last = Last,
						     count = 5}}]} =
		recv_iq(Config);
       true ->
	    #message{sub_els = [#mam_fin{
				   complete = false,
				   rsm = #rsm_set{last = Last,
						  count = 10}}]} =
		recv_message(Config)
    end,
    %% Get the next items starting from the `Last`.
    %% Limit the response to 2 items.
    I2 = send(Config,
              #iq{type = Type,
                  sub_els = [#mam_query{xmlns = NS,
					rsm = #rsm_set{max = 2,
                                                       'after' = Last}}]}),
    maybe_recv_iq_result(Config, NS, I2),
    lists:foreach(
      fun(N) ->
              Text = #text{data = integer_to_binary(N)},
              #message{to = MyJID,
                       sub_els =
                           [#mam_result{
			       xmlns = NS,
                               sub_els =
				   [#forwarded_decoded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]} =
		  recv_message(Config)
      end, lists:seq(4, 5)),
    if NS == ?NS_MAM_TMP ->
	    #iq{type = result, id = I2,
		sub_els = [#mam_query{
			      xmlns = NS,
			      rsm = #rsm_set{
				       count = 5,
				       first = #rsm_first{data = First}}}]} =
		recv_iq(Config);
       true ->
	    #message{
	       sub_els = [#mam_fin{
			     complete = false,
			     rsm = #rsm_set{
				      count = 10,
				      first = #rsm_first{data = First}}}]} =
		recv_message(Config)
    end,
    %% Paging back. Should receive 3 elements: 1, 2, 3.
    I3 = send(Config,
              #iq{type = Type,
                  sub_els = [#mam_query{xmlns = NS,
					rsm = #rsm_set{max = 3,
                                                       before = First}}]}),
    maybe_recv_iq_result(Config, NS, I3),
    lists:foreach(
      fun(N) ->
              Text = #text{data = integer_to_binary(N)},
              #message{to = MyJID,
                       sub_els =
                           [#mam_result{
			       xmlns = NS,
                               sub_els =
				   [#forwarded_decoded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]} =
		  recv_message(Config)
      end, lists:seq(1, 3)),
    if NS == ?NS_MAM_TMP ->
	    #iq{type = result, id = I3,
		sub_els = [#mam_query{xmlns = NS, rsm = #rsm_set{count = 5}}]} =
		recv_iq(Config);
       true ->
	    #message{
	       sub_els = [#mam_fin{complete = true,
				   rsm = #rsm_set{count = 10}}]} =
		recv_message(Config)
    end,
    %% Getting the item count. Should be 5 (or 10).
    I4 = send(Config,
	      #iq{type = Type,
		  sub_els = [#mam_query{xmlns = NS,
					rsm = #rsm_set{max = 0}}]}),
    maybe_recv_iq_result(Config, NS, I4),
    if NS == ?NS_MAM_TMP ->
	    #iq{type = result, id = I4,
		sub_els = [#mam_query{
			      xmlns = NS,
			      rsm = #rsm_set{count = 5,
					     first = undefined,
					     last = undefined}}]} =
		recv_iq(Config);
       true ->
	    #message{
	       sub_els = [#mam_fin{
			     complete = false,
			     rsm = #rsm_set{count = 10,
					    first = undefined,
					    last = undefined}}]} =
		recv_message(Config)
    end,
    %% Should receive 2 last messages
    I5 = send(Config,
	      #iq{type = Type,
		  sub_els = [#mam_query{xmlns = NS,
					rsm = #rsm_set{max = 2,
						       before = <<"">>}}]}),
    maybe_recv_iq_result(Config, NS, I5),
    lists:foreach(
      fun(N) ->
	      Text = #text{data = integer_to_binary(N)},
	      #message{to = MyJID,
		       sub_els =
			   [#mam_result{
			       xmlns = NS,
			       sub_els =
				   [#forwarded_decoded{
				       delay = #delay{},
				       sub_els =
					   [#message{
					       from = MyJID, to = Peer,
					       body = [Text]}]}]}]} =
		  recv_message(Config)
      end, lists:seq(4, 5)),
    if NS == ?NS_MAM_TMP ->
	    #iq{type = result, id = I5,
		sub_els = [#mam_query{xmlns = NS, rsm = #rsm_set{count = 5}}]} =
		recv_iq(Config);
       true ->
	    #message{
	       sub_els = [#mam_fin{complete = false,
				   rsm = #rsm_set{count = 10}}]} =
		recv_message(Config)
    end.

client_state_master(Config) ->
    true = ?config(csi, Config),
    Peer = ?config(slave, Config),
    Presence = #presence{to = Peer},
    ChatState = #message{to = Peer, thread = <<"1">>,
			 sub_els = [#chatstate{type = active}]},
    Message = ChatState#message{body = [#text{data = <<"body">>}]},
    PepPayload = xmpp:encode(#presence{}),
    PepOne = #message{
		to = Peer,
		sub_els =
		    [#ps_event{
			items =
			    #ps_items{
			       node = <<"foo-1">>,
			       items =
				   [#ps_item{
				       id = <<"pep-1">>,
				       xml_els = [PepPayload]}]}}]},
    PepTwo = #message{
		to = Peer,
		sub_els =
		    [#ps_event{
			items =
			    #ps_items{
			       node = <<"foo-2">>,
			       items =
				   [#ps_item{
				       id = <<"pep-2">>,
				       xml_els = [PepPayload]}]}}]},
    %% Wait for the slave to become inactive.
    wait_for_slave(Config),
    %% Should be queued (but see below):
    send(Config, Presence),
    %% Should replace the previous presence in the queue:
    send(Config, Presence#presence{type = unavailable}),
    %% The following two PEP stanzas should be queued (but see below):
    send(Config, PepOne),
    send(Config, PepTwo),
    %% The following two PEP stanzas should replace the previous two:
    send(Config, PepOne),
    send(Config, PepTwo),
    %% Should be queued (but see below):
    send(Config, ChatState),
    %% Should replace the previous chat state in the queue:
    send(Config, ChatState#message{sub_els = [#chatstate{type = composing}]}),
    %% Should be sent immediately, together with the queued stanzas:
    send(Config, Message),
    %% Wait for the slave to become active.
    wait_for_slave(Config),
    %% Should be delivered, as the client is active again:
    send(Config, ChatState),
    disconnect(Config).

client_state_slave(Config) ->
    Peer = ?config(master, Config),
    change_client_state(Config, inactive),
    wait_for_master(Config),
    #presence{from = Peer, type = unavailable, sub_els = [#delay{}]} =
	recv_presence(Config),
    #message{
       from = Peer,
       sub_els =
	   [#ps_event{
	       items =
		   #ps_items{
		      node = <<"foo-1">>,
		      items =
			  [#ps_item{
			      id = <<"pep-1">>}]}},
	    #delay{}]} = recv_message(Config),
    #message{
       from = Peer,
       sub_els =
	   [#ps_event{
	       items =
		   #ps_items{
		      node = <<"foo-2">>,
		      items =
			  [#ps_item{
			      id = <<"pep-2">>}]}},
	    #delay{}]} = recv_message(Config),
    #message{from = Peer, thread = <<"1">>,
	     sub_els = [#chatstate{type = composing},
			#delay{}]} = recv_message(Config),
    #message{from = Peer, thread = <<"1">>,
	     body = [#text{data = <<"body">>}],
	     sub_els = [#chatstate{type = active}]} = recv_message(Config),
    change_client_state(Config, active),
    wait_for_master(Config),
    #message{from = Peer, thread = <<"1">>,
	     sub_els = [#chatstate{type = active}]} = recv_message(Config),
    disconnect(Config).

%%%===================================================================
%%% Aux functions
%%%===================================================================
change_client_state(Config, NewState) ->
    send(Config, #csi{type = NewState}),
    send_recv(Config, #iq{type = get, to = server_jid(Config),
			  sub_els = [#ping{}]}).

bookmark_conference() ->
    #bookmark_conference{name = <<"Some name">>,
                         autojoin = true,
                         jid = jid:make(
                                 <<"some">>,
                                 <<"some.conference.org">>,
                                 <<>>)}.

socks5_connect(#streamhost{host = Host, port = Port},
               {SID, JID1, JID2}) ->
    Hash = p1_sha:sha([SID, jid:to_string(JID1), jid:to_string(JID2)]),
    {ok, Sock} = gen_tcp:connect(binary_to_list(Host), Port,
                                 [binary, {active, false}]),
    Init = <<?VERSION_5, 1, ?AUTH_ANONYMOUS>>,
    InitAck = <<?VERSION_5, ?AUTH_ANONYMOUS>>,
    Req = <<?VERSION_5, ?CMD_CONNECT, 0,
            ?ATYP_DOMAINNAME, 40, Hash:40/binary, 0, 0>>,
    Resp = <<?VERSION_5, ?SUCCESS, 0, ?ATYP_DOMAINNAME,
             40, Hash:40/binary, 0, 0>>,
    gen_tcp:send(Sock, Init),
    {ok, InitAck} = gen_tcp:recv(Sock, size(InitAck)),
    gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, size(Resp)),
    Sock.

socks5_send(Sock, Data) ->
    ok = gen_tcp:send(Sock, Data).

socks5_recv(Sock, Data) ->
    {ok, Data} = gen_tcp:recv(Sock, size(Data)).

set_opts(Config, Options) ->
    lists:foldl(
      fun({Opt, Val}, Acc) ->
	      lists:keystore(Opt, 1, Acc, {Opt, Val})
      end, Config, Options).

%%%===================================================================
%%% SQL stuff
%%%===================================================================
create_sql_tables(sqlite, _BaseDir) ->
    ok;
create_sql_tables(Type, BaseDir) ->
    {VHost, File} = case Type of
                        mysql ->
                            {?MYSQL_VHOST, "mysql.sql"};
                        pgsql ->
                            {?PGSQL_VHOST, "pg.sql"}
                    end,
    SQLFile = filename:join([BaseDir, "sql", File]),
    CreationQueries = read_sql_queries(SQLFile),
    DropTableQueries = drop_table_queries(CreationQueries),
    case ejabberd_sql:sql_transaction(
           VHost, DropTableQueries ++ CreationQueries) of
        {atomic, ok} ->
            ok;
        Err ->
            ct:fail({failed_to_create_sql_tables, Type, Err})
    end.

read_sql_queries(File) ->
    case file:open(File, [read, binary]) of
        {ok, Fd} ->
            read_lines(Fd, File, []);
        Err ->
            ct:fail({open_file_failed, File, Err})
    end.

drop_table_queries(Queries) ->
    lists:foldl(
      fun(Query, Acc) ->
              case split(str:to_lower(Query)) of
                  [<<"create">>, <<"table">>, Table|_] ->
                      [<<"DROP TABLE IF EXISTS ", Table/binary, ";">>|Acc];
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

clear_riak_tables(Config) ->
    User = ?config(user, Config),
    Server = ?config(server, Config),
    Room = muc_room_jid(Config),
    {URoom, SRoom, _} = jid:tolower(Room),
    ejabberd_auth:remove_user(User, Server),
    ejabberd_auth:remove_user(<<"test_slave">>, Server),
    ejabberd_auth:remove_user(<<"test_master">>, Server),
    mod_muc:forget_room(Server, URoom, SRoom),
    ejabberd_riak:delete(muc_registered, {{<<"test_slave">>, Server}, SRoom}),
    ejabberd_riak:delete(muc_registered, {{<<"test_master">>, Server}, SRoom}),
    Config.
