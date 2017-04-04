%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  2 Jun 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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
end_per_group(riak, Config) ->
    case ejabberd_riak:is_connected() of
	true ->
	    clear_riak_tables(Config);
	false ->
	    Config
    end;
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
	"test_legacy_auth_feature" ->
	    connect(Config);
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
     auth_external,
     auth_external_no_jid,
     auth_external_no_user,
     auth_external_malformed_jid,
     auth_external_wrong_jid,
     auth_external_wrong_server,
     auth_external_invalid_cert,
     sm_tests:single_cases(),
     sm_tests:master_slave_cases(),
     muc_tests:single_cases(),
     muc_tests:master_slave_cases(),
     proxy65_tests:single_cases(),
     proxy65_tests:master_slave_cases(),
     replaced_tests:master_slave_cases()].

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
       vcard_tests:single_cases(),
       muc_tests:single_cases(),
       offline_tests:single_cases(),
       test_unregister]},
     muc_tests:master_slave_cases(),
     privacy_tests:master_slave_cases(),
     roster_tests:master_slave_cases(),
     offline_tests:master_slave_cases(),
     vcard_tests:master_slave_cases(),
     announce_tests:master_slave_cases()];
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
       vcard_tests:single_cases(),
       pubsub_tests:single_cases(),
       muc_tests:single_cases(),
       offline_tests:single_cases(),
       mam_tests:single_cases(),
       carbons_tests:single_cases(),
       csi_tests:single_cases(),
       test_unregister]},
     muc_tests:master_slave_cases(),
     privacy_tests:master_slave_cases(),
     pubsub_tests:master_slave_cases(),
     roster_tests:master_slave_cases(),
     offline_tests:master_slave_cases(),
     mam_tests:master_slave_cases(),
     vcard_tests:master_slave_cases(),
     announce_tests:master_slave_cases(),
     carbons_tests:master_slave_cases(),
     csi_tests:master_slave_cases()];
db_tests(_) ->
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
       vcard_tests:single_cases(),
       pubsub_tests:single_cases(),
       muc_tests:single_cases(),
       offline_tests:single_cases(),
       mam_tests:single_cases(),
       test_unregister]},
     muc_tests:master_slave_cases(),
     privacy_tests:master_slave_cases(),
     pubsub_tests:master_slave_cases(),
     roster_tests:master_slave_cases(),
     offline_tests:master_slave_cases(),
     mam_tests:master_slave_cases(),
     vcard_tests:master_slave_cases(),
     announce_tests:master_slave_cases(),
     carbons_tests:master_slave_cases()].

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
    send(Config, #message{from = jid:make(randoms:get_string())}),
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
    From = jid:make(randoms:get_string()),
    To = jid:make(randoms:get_string()),
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
    Master = <<"test_master!#$%^*()`~+-;_=[]{}|\\">>,
    Slave = <<"test_slave!#$%^*()`~+-;_=[]{}|\\">>,
    ejabberd_auth:remove_user(User, Server),
    ejabberd_auth:remove_user(Master, Server),
    ejabberd_auth:remove_user(Slave, Server),
    ejabberd_riak:delete(muc_room),
    ejabberd_riak:delete(muc_registered),
    timer:sleep(timer:seconds(5)),
    Config.
