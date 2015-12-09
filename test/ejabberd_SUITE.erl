%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2015, ProcessOne
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_SUITE).

-compile(export_all).

-import(suite, [init_config/1, connect/1, disconnect/1,
                recv/0, send/2, send_recv/2, my_jid/1, server_jid/1,
                pubsub_jid/1, proxy_jid/1, muc_jid/1,
                muc_room_jid/1, get_features/2, re_register/1,
                is_feature_advertised/2, subscribe_to_events/1,
                is_feature_advertised/3, set_opt/3, auth_SASL/2,
                wait_for_master/1, wait_for_slave/1,
                make_iq_result/1, start_event_relay/0,
                stop_event_relay/1, put_event/2, get_event/1,
                bind/1, auth/1, open_session/1, zlib/1, starttls/1,
		close_socket/1]).

-include("suite.hrl").

suite() ->
    [{timetrap, {seconds,120}}].

init_per_suite(Config) ->
    NewConfig = init_config(Config),
    DataDir = proplists:get_value(data_dir, NewConfig),
    {ok, CWD} = file:get_cwd(),
    ExtAuthScript = filename:join([DataDir, "extauth.py"]),
    LDIFFile = filename:join([DataDir, "ejabberd.ldif"]),
    {ok, _} = file:copy(ExtAuthScript, filename:join([CWD, "extauth.py"])),
    {ok, _} = ldap_srv:start(LDIFFile),
    ok = application:start(ejabberd),
    NewConfig.

end_per_suite(_Config) ->
    ok.

init_per_group(no_db, Config) ->
    re_register(Config),
    Config;
init_per_group(mnesia, Config) ->
    mod_muc:shutdown_rooms(?MNESIA_VHOST),
    set_opt(server, ?MNESIA_VHOST, Config);
init_per_group(mysql, Config) ->
    case catch ejabberd_odbc:sql_query(?MYSQL_VHOST, [<<"select 1;">>]) of
        {selected, _, _} ->
            mod_muc:shutdown_rooms(?MYSQL_VHOST),
            create_sql_tables(mysql, ?config(base_dir, Config)),
            set_opt(server, ?MYSQL_VHOST, Config);
        Err ->
            {skip, {mysql_not_available, Err}}
    end;
init_per_group(pgsql, Config) ->
    case catch ejabberd_odbc:sql_query(?PGSQL_VHOST, [<<"select 1;">>]) of
        {selected, _, _} ->
            mod_muc:shutdown_rooms(?PGSQL_VHOST),
            create_sql_tables(pgsql, ?config(base_dir, Config)),
            set_opt(server, ?PGSQL_VHOST, Config);
        Err ->
            {skip, {pgsql_not_available, Err}}
    end;
init_per_group(sqlite, Config) ->
    case catch ejabberd_odbc:sql_query(?SQLITE_VHOST, [<<"select 1;">>]) of
        {selected, _, _} ->
            mod_muc:shutdown_rooms(?SQLITE_VHOST),
            set_opt(server, ?SQLITE_VHOST, Config);
        Err ->
            {skip, {sqlite_not_available, Err}}
    end;
init_per_group(ldap, Config) ->
    set_opt(server, ?LDAP_VHOST, Config);
init_per_group(extauth, Config) ->
    set_opt(server, ?EXTAUTH_VHOST, Config);
init_per_group(riak, Config) ->
    case ejabberd_riak:is_connected() of
	true ->
	    mod_muc:shutdown_rooms(?RIAK_VHOST),
	    NewConfig = set_opt(server, ?RIAK_VHOST, Config),
	    clear_riak_tables(NewConfig);
	Err ->
	    {skip, {riak_not_available, Err}}
    end;
init_per_group(_GroupName, Config) ->
    Pid = start_event_relay(),
    set_opt(event_relay, Pid, Config).

end_per_group(mnesia, _Config) ->
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
end_per_group(_GroupName, Config) ->
    stop_event_relay(Config),
    ok.

init_per_testcase(stop_ejabberd, Config) ->
    open_session(bind(auth(connect(Config))));
init_per_testcase(TestCase, OrigConfig) ->
    subscribe_to_events(OrigConfig),
    Server = ?config(server, OrigConfig),
    Resource = ?config(resource, OrigConfig),
    MasterResource = ?config(master_resource, OrigConfig),
    SlaveResource = ?config(slave_resource, OrigConfig),
    Test = atom_to_list(TestCase),
    IsMaster = lists:suffix("_master", Test),
    IsSlave = lists:suffix("_slave", Test),
    IsCarbons = lists:prefix("carbons_", Test),
    User = if IsMaster or IsCarbons -> <<"test_master">>;
              IsSlave -> <<"test_slave">>;
              true -> <<"test_single">>
           end,
    MyResource = if IsMaster and IsCarbons -> MasterResource;
		    IsSlave and IsCarbons -> SlaveResource;
		    true -> Resource
		 end,
    Slave = if IsCarbons ->
		    jlib:make_jid(<<"test_master">>, Server, SlaveResource);
	       true ->
		    jlib:make_jid(<<"test_slave">>, Server, Resource)
	    end,
    Master = if IsCarbons ->
		     jlib:make_jid(<<"test_master">>, Server, MasterResource);
		true ->
		     jlib:make_jid(<<"test_master">>, Server, Resource)
	     end,
    Config = set_opt(user, User,
                     set_opt(slave, Slave,
                             set_opt(master, Master,
				     set_opt(resource, MyResource, OrigConfig)))),
    case TestCase of
        test_connect ->
            Config;
        test_auth ->
            connect(Config);
        test_starttls ->
            connect(Config);
        test_zlib ->
            connect(Config);
        test_register ->
            connect(Config);
        auth_md5 ->
            connect(Config);
        auth_plain ->
            connect(Config);
        test_bind ->
            auth(connect(Config));
	sm_resume ->
	    auth(connect(Config));
        test_open_session ->
            bind(auth(connect(Config)));
        _ when IsMaster or IsSlave ->
            Password = ?config(password, Config),
            ejabberd_auth:try_register(User, Server, Password),
            open_session(bind(auth(connect(Config))));
        _ ->
            open_session(bind(auth(connect(Config))))
    end.

end_per_testcase(_TestCase, _Config) ->
    ok.

no_db_tests() ->
    [{generic, [sequence],
      [test_connect,
       test_starttls,
       test_zlib,
       test_auth,
       test_bind,
       test_open_session,
       presence,
       ping,
       version,
       time,
       stats,
       sm,
       sm_resume,
       disco]},
     {test_proxy65, [parallel],
      [proxy65_master, proxy65_slave]}].

db_tests(riak) ->
    %% No support for mod_pubsub
    [{single_user, [sequence],
      [test_register,
       auth_plain,
       auth_md5,
       presence_broadcast,
       last,
       roster_get,
       private,
       privacy,
       blocking,
       vcard,
       test_unregister]},
     {test_muc_register, [sequence],
      [muc_register_master, muc_register_slave]},
     {test_roster_subscribe, [parallel],
      [roster_subscribe_master,
       roster_subscribe_slave]},
     {test_offline, [sequence],
      [offline_master, offline_slave]},
     {test_muc, [parallel],
      [muc_master, muc_slave]},
     {test_announce, [sequence],
      [announce_master, announce_slave]},
     {test_vcard_xupdate, [parallel],
      [vcard_xupdate_master, vcard_xupdate_slave]},
     {test_roster_remove, [parallel],
      [roster_remove_master,
       roster_remove_slave]}];
db_tests(mnesia) ->
    [{single_user, [sequence],
      [test_register,
       auth_plain,
       auth_md5,
       presence_broadcast,
       last,
       roster_get,
       roster_ver,
       private,
       privacy,
       blocking,
       vcard,
       pubsub,
       test_unregister]},
     {test_muc_register, [sequence],
      [muc_register_master, muc_register_slave]},
     {test_roster_subscribe, [parallel],
      [roster_subscribe_master,
       roster_subscribe_slave]},
     {test_offline, [sequence],
      [offline_master, offline_slave]},
     {test_old_mam, [parallel],
      [mam_old_master, mam_old_slave]},
     {test_new_mam, [parallel],
      [mam_new_master, mam_new_slave]},
     {test_carbons, [parallel],
      [carbons_master, carbons_slave]},
     {test_client_state, [parallel],
      [client_state_master, client_state_slave]},
     {test_muc, [parallel],
      [muc_master, muc_slave]},
     {test_announce, [sequence],
      [announce_master, announce_slave]},
     {test_vcard_xupdate, [parallel],
      [vcard_xupdate_master, vcard_xupdate_slave]},
     {test_roster_remove, [parallel],
      [roster_remove_master,
       roster_remove_slave]}];
db_tests(_) ->
    %% No support for carboncopy
    [{single_user, [sequence],
      [test_register,
       auth_plain,
       auth_md5,
       presence_broadcast,
       last,
       roster_get,
       roster_ver,
       private,
       privacy,
       blocking,
       vcard,
       pubsub,
       test_unregister]},
     {test_muc_register, [sequence],
      [muc_register_master, muc_register_slave]},
     {test_roster_subscribe, [parallel],
      [roster_subscribe_master,
       roster_subscribe_slave]},
     {test_offline, [sequence],
      [offline_master, offline_slave]},
     {test_old_mam, [parallel],
      [mam_old_master, mam_old_slave]},
     {test_new_mam, [parallel],
      [mam_new_master, mam_new_slave]},
     {test_muc, [parallel],
      [muc_master, muc_slave]},
     {test_announce, [sequence],
      [announce_master, announce_slave]},
     {test_vcard_xupdate, [parallel],
      [vcard_xupdate_master, vcard_xupdate_slave]},
     {test_roster_remove, [parallel],
      [roster_remove_master,
       roster_remove_slave]}].

ldap_tests() ->
    [{ldap_tests, [sequence],
      [test_auth,
       vcard_get]}].

extauth_tests() ->
    [{extauth_tests, [sequence],
      [test_auth,
       test_unregister]}].

groups() ->
    [{ldap, [sequence], ldap_tests()},
     {extauth, [sequence], extauth_tests()},
     {no_db, [sequence], no_db_tests()},
     {mnesia, [sequence], db_tests(mnesia)},
     {mysql, [sequence], db_tests(mysql)},
     {pgsql, [sequence], db_tests(pgsql)},
     {sqlite, [sequence], db_tests(sqlite)},
     {riak, [sequence], db_tests(riak)}].

all() ->
    [{group, ldap},
     {group, no_db},
     {group, mnesia},
     {group, mysql},
     {group, pgsql},
     {group, sqlite},
     {group, extauth},
     {group, riak},
     stop_ejabberd].

stop_ejabberd(Config) ->
    ok = application:stop(ejabberd),
    ?recv1(#stream_error{reason = 'system-shutdown'}),
    ?recv1({xmlstreamend, <<"stream:stream">>}),
    Config.

test_connect(Config) ->
    disconnect(connect(Config)).

test_starttls(Config) ->
    case ?config(starttls, Config) of
        true ->
            disconnect(starttls(Config));
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
        sub_els = [#register{username = none,
                             password = none}]} =
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

test_auth(Config) ->
    disconnect(auth(Config)).

test_bind(Config) ->
    disconnect(bind(Config)).

test_open_session(Config) ->
    disconnect(open_session(Config)).

roster_get(Config) ->
    #iq{type = result, sub_els = [#roster{items = []}]} =
        send_recv(Config, #iq{type = get, sub_els = [#roster{}]}),
    disconnect(Config).

roster_ver(Config) ->
    %% Get initial "ver"
    #iq{type = result, sub_els = [#roster{ver = Ver1, items = []}]} =
        send_recv(Config, #iq{type = get,
                              sub_els = [#roster{ver = <<"">>}]}),
    %% Should receive empty IQ-result
    #iq{type = result, sub_els = []} =
        send_recv(Config, #iq{type = get,
                              sub_els = [#roster{ver = Ver1}]}),
    %% Attempting to subscribe to server's JID
    send(Config, #presence{type = subscribe, to = server_jid(Config)}),
    %% Receive a single roster push with the new "ver"
    ?recv1(#iq{type = set, sub_els = [#roster{ver = Ver2}]}),
    %% Requesting roster with the previous "ver". Should receive Ver2 again
    #iq{type = result, sub_els = [#roster{ver = Ver2}]} =
        send_recv(Config, #iq{type = get,
                              sub_els = [#roster{ver = Ver1}]}),
    %% Now requesting roster with the newest "ver". Should receive empty IQ.
    #iq{type = result, sub_els = []} =
        send_recv(Config, #iq{type = get,
                              sub_els = [#roster{ver = Ver2}]}),
    disconnect(Config).

presence(Config) ->
    send(Config, #presence{}),
    JID = my_jid(Config),
    ?recv1(#presence{from = JID, to = JID}),
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
    Caps = #caps{hash = <<"sha-1">>, node = ?EJABBERD_CT_URI, ver = Ver},
    send(Config, #presence{sub_els = [Caps]}),
    JID = my_jid(Config),
    %% We receive:
    %% 1) disco#info iq request for CAPS
    %% 2) welcome message
    %% 3) presence broadcast
    {IQ, _, _} = ?recv3(#iq{type = get,
			    from = ServerJID,
			    sub_els = [#disco_info{node = Node}]},
			#message{type = normal},
			#presence{from = JID, to = JID}),
    send(Config, #iq{type = result, id = IQ#iq.id,
		     to = ServerJID, sub_els = [Info]}),
    %% We're trying to read our feature from ejabberd database
    %% with exponential back-off as our IQ response may be delayed.
    [Feature] =
	lists:foldl(
	  fun(Time, []) ->
		  timer:sleep(Time),
		  mod_caps:get_features(
		    Server,
		    mod_caps:read_caps(
		      [xmpp_codec:encode(Caps)]));
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

sm(Config) ->
    Server = ?config(server, Config),
    ServerJID = jlib:make_jid(<<"">>, Server, <<"">>),
    Msg = #message{to = ServerJID, body = [#text{data = <<"body">>}]},
    true = ?config(sm, Config),
    %% Enable the session management with resumption enabled
    send(Config, #sm_enable{resume = true, xmlns = ?NS_STREAM_MGMT_3}),
    ?recv1(#sm_enabled{id = ID, resume = true}),
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
    ServerJID = jlib:make_jid(<<"">>, Server, <<"">>),
    MyJID = my_jid(Config),
    Txt = #text{data = <<"body">>},
    Msg = #message{from = ServerJID, to = MyJID, body = [Txt]},
    %% Route message. The message should be queued by the C2S process.
    ejabberd_router:route(ServerJID, MyJID, xmpp_codec:encode(Msg)),
    send(Config, #sm_resume{previd = ID, h = 0, xmlns = ?NS_STREAM_MGMT_3}),
    ?recv1(#sm_resumed{previd = ID, h = 3}),
    ?recv1(#message{from = ServerJID, to = MyJID, body = [Txt]}),
    ?recv1(#sm_r{}),
    send(Config, #sm_a{h = 1, xmlns = ?NS_STREAM_MGMT_3}),
    disconnect(Config).

private(Config) ->
    Conference = #bookmark_conference{name = <<"Some name">>,
                                      autojoin = true,
                                      jid = jlib:make_jid(
                                              <<"some">>,
                                              <<"some.conference.org">>,
                                              <<>>)},
    Storage = #bookmark_storage{conference = [Conference]},
    StorageXMLOut = xmpp_codec:encode(Storage),
    #iq{type = error} =
        send_recv(Config, #iq{type = get, sub_els = [#private{}],
                              to = server_jid(Config)}),
    #iq{type = result, sub_els = []} =
        send_recv(
          Config, #iq{type = set,
                      sub_els = [#private{xml_els = [StorageXMLOut]}]}),
    #iq{type = result,
        sub_els = [#private{xml_els = [StorageXMLIn]}]} =
        send_recv(
          Config,
          #iq{type = get,
              sub_els = [#private{xml_els = [xmpp_codec:encode(
                                               #bookmark_storage{})]}]}),
    Storage = xmpp_codec:decode(StorageXMLIn),
    disconnect(Config).

last(Config) ->
    true = is_feature_advertised(Config, ?NS_LAST),
    #iq{type = result, sub_els = [#last{}]} =
        send_recv(Config, #iq{type = get, sub_els = [#last{}],
                              to = server_jid(Config)}),
    disconnect(Config).

privacy(Config) ->
    true = is_feature_advertised(Config, ?NS_PRIVACY),
    #iq{type = result, sub_els = [#privacy{}]} =
        send_recv(Config, #iq{type = get, sub_els = [#privacy{}]}),
    JID = <<"tybalt@example.com">>,
    I1 = send(Config,
              #iq{type = set,
                  sub_els = [#privacy{
                                lists = [#privacy_list{
                                            name = <<"public">>,
                                            items =
                                                [#privacy_item{
                                                    type = jid,
                                                    order = 3,
                                                    action = deny,
                                                    kinds = ['presence-in'],
                                                    value = JID}]}]}]}),
    {Push1, _} =
        ?recv2(
           #iq{type = set,
               sub_els = [#privacy{
                             lists = [#privacy_list{
                                         name = <<"public">>}]}]},
           #iq{type = result, id = I1, sub_els = []}),
    send(Config, make_iq_result(Push1)),
    #iq{type = result, sub_els = []} =
        send_recv(Config, #iq{type = set,
                              sub_els = [#privacy{active = <<"public">>}]}),
    #iq{type = result, sub_els = []} =
        send_recv(Config, #iq{type = set,
                              sub_els = [#privacy{default = <<"public">>}]}),
    #iq{type = result,
        sub_els = [#privacy{default = <<"public">>,
                            active = <<"public">>,
                            lists = [#privacy_list{name = <<"public">>}]}]} =
        send_recv(Config, #iq{type = get, sub_els = [#privacy{}]}),
    #iq{type = result, sub_els = []} =
        send_recv(Config,
                  #iq{type = set, sub_els = [#privacy{default = none}]}),
    #iq{type = result, sub_els = []} =
        send_recv(Config, #iq{type = set, sub_els = [#privacy{active = none}]}),
    I2 = send(Config, #iq{type = set,
                          sub_els = [#privacy{
                                        lists =
                                            [#privacy_list{
                                                name = <<"public">>}]}]}),
    {Push2, _} =
        ?recv2(
           #iq{type = set,
               sub_els = [#privacy{
                             lists = [#privacy_list{
                                         name = <<"public">>}]}]},
           #iq{type = result, id = I2, sub_els = []}),
    send(Config, make_iq_result(Push2)),
    disconnect(Config).

blocking(Config) ->
    true = is_feature_advertised(Config, ?NS_BLOCKING),
    JID = jlib:make_jid(<<"romeo">>, <<"montague.net">>, <<>>),
    #iq{type = result, sub_els = [#block_list{}]} =
        send_recv(Config, #iq{type = get, sub_els = [#block_list{}]}),
    I1 = send(Config, #iq{type = set,
                          sub_els = [#block{items = [JID]}]}),
    {Push1, Push2, _} =
        ?recv3(
           #iq{type = set,
               sub_els = [#privacy{lists = [#privacy_list{}]}]},
           #iq{type = set,
               sub_els = [#block{items = [JID]}]},
           #iq{type = result, id = I1, sub_els = []}),
    send(Config, make_iq_result(Push1)),
    send(Config, make_iq_result(Push2)),
    I2 = send(Config, #iq{type = set,
                          sub_els = [#unblock{items = [JID]}]}),
    {Push3, Push4, _} =
        ?recv3(
           #iq{type = set,
               sub_els = [#privacy{lists = [#privacy_list{}]}]},
           #iq{type = set,
               sub_els = [#unblock{items = [JID]}]},
           #iq{type = result, id = I2, sub_els = []}),
    send(Config, make_iq_result(Push3)),
    send(Config, make_iq_result(Push4)),
    disconnect(Config).

vcard(Config) ->
    true = is_feature_advertised(Config, ?NS_VCARD),
    VCard =
        #vcard{fn = <<"Peter Saint-Andre">>,
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
        send_recv(Config, #iq{type = get, sub_els = [#vcard{}]}),
    disconnect(Config).

vcard_get(Config) ->
    true = is_feature_advertised(Config, ?NS_VCARD),
    %% TODO: check if VCard corresponds to LDIF data from ejabberd.ldif
    #iq{type = result, sub_els = [_VCard]} =
        send_recv(Config, #iq{type = get, sub_els = [#vcard{}]}),
    disconnect(Config).

vcard_xupdate_master(Config) ->
    Img = <<137, "PNG\r\n", 26, $\n>>,
    ImgHash = p1_sha:sha(Img),
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    wait_for_slave(Config),
    send(Config, #presence{}),
    ?recv2(#presence{from = MyJID, type = undefined},
           #presence{from = Peer, type = undefined}),
    VCard = #vcard{photo = #vcard_photo{type = <<"image/png">>, binval = Img}},
    I1 = send(Config, #iq{type = set, sub_els = [VCard]}),
    ?recv2(#iq{type = result, sub_els = [], id = I1},
	   #presence{from = MyJID, type = undefined,
		     sub_els = [#vcard_xupdate{photo = ImgHash}]}),
    I2 = send(Config, #iq{type = set, sub_els = [#vcard{}]}),
    ?recv3(#iq{type = result, sub_els = [], id = I2},
	   #presence{from = MyJID, type = undefined,
		     sub_els = [#vcard_xupdate{photo = undefined}]},
	   #presence{from = Peer, type = unavailable}),
    disconnect(Config).

vcard_xupdate_slave(Config) ->
    Img = <<137, "PNG\r\n", 26, $\n>>,
    ImgHash = p1_sha:sha(Img),
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    send(Config, #presence{}),
    ?recv1(#presence{from = MyJID, type = undefined}),
    wait_for_master(Config),
    ?recv1(#presence{from = Peer, type = undefined}),
    ?recv1(#presence{from = Peer, type = undefined,
	      sub_els = [#vcard_xupdate{photo = ImgHash}]}),
    ?recv1(#presence{from = Peer, type = undefined,
	      sub_els = [#vcard_xupdate{photo = undefined}]}),
    disconnect(Config).

stats(Config) ->
    #iq{type = result, sub_els = [#stats{stat = Stats}]} =
        send_recv(Config, #iq{type = get, sub_els = [#stats{}],
                              to = server_jid(Config)}),
    lists:foreach(
      fun(#stat{} = Stat) ->
              #iq{type = result, sub_els = [_|_]} =
                  send_recv(Config, #iq{type = get,
                                        sub_els = [#stats{stat = [Stat]}],
                                        to = server_jid(Config)})
      end, Stats),
    disconnect(Config).

pubsub(Config) ->
    Features = get_features(Config, pubsub_jid(Config)),
    true = lists:member(?NS_PUBSUB, Features),
    %% Publish <presence/> element within node "presence"
    ItemID = randoms:get_string(),
    Node = <<"presence">>,
    Item = #pubsub_item{id = ItemID,
                        xml_els = [xmpp_codec:encode(#presence{})]},
    #iq{type = result,
        sub_els = [#pubsub{publish = #pubsub_publish{
                             node = Node,
                             items = [#pubsub_item{id = ItemID}]}}]} =
        send_recv(Config,
                  #iq{type = set, to = pubsub_jid(Config),
                      sub_els = [#pubsub{publish = #pubsub_publish{
                                           node = Node,
                                           items = [Item]}}]}),
    %% Subscribe to node "presence"
    I1 = send(Config,
             #iq{type = set, to = pubsub_jid(Config),
                 sub_els = [#pubsub{subscribe = #pubsub_subscribe{
                                      node = Node,
                                      jid = my_jid(Config)}}]}),
    ?recv2(
       #message{sub_els = [#pubsub_event{}, #delay{}]},
       #iq{type = result, id = I1}),
    %% Get subscriptions
    true = lists:member(?PUBSUB("retrieve-subscriptions"), Features),
    #iq{type = result,
        sub_els =
            [#pubsub{subscriptions =
                         {none, [#pubsub_subscription{node = Node}]}}]} =
        send_recv(Config, #iq{type = get, to = pubsub_jid(Config),
                              sub_els = [#pubsub{subscriptions = {none, []}}]}),
    %% Get affiliations
    true = lists:member(?PUBSUB("retrieve-affiliations"), Features),
    #iq{type = result,
        sub_els = [#pubsub{
                      affiliations =
                          [#pubsub_affiliation{node = Node, type = owner}]}]} =
        send_recv(Config, #iq{type = get, to = pubsub_jid(Config),
                              sub_els = [#pubsub{affiliations = []}]}),
    %% Fetching published items from node "presence"
    #iq{type = result,
        sub_els = [#pubsub{items = #pubsub_items{
                             node = Node,
                             items = [Item]}}]} =
        send_recv(Config,
                  #iq{type = get, to = pubsub_jid(Config),
                      sub_els = [#pubsub{items = #pubsub_items{node = Node}}]}),
    %% Deleting the item from the node
    true = lists:member(?PUBSUB("delete-items"), Features),
    I2 = send(Config,
              #iq{type = set, to = pubsub_jid(Config),
                  sub_els = [#pubsub{retract = #pubsub_retract{
                                       node = Node,
                                       items = [#pubsub_item{id = ItemID}]}}]}),
    ?recv2(
       #iq{type = result, id = I2, sub_els = []},
       #message{sub_els = [#pubsub_event{
                              items = [#pubsub_event_items{
                                          node = Node,
                                          retract = [ItemID]}]}]}),
    %% Unsubscribe from node "presence"
    #iq{type = result, sub_els = []} =
        send_recv(Config,
                  #iq{type = set, to = pubsub_jid(Config),
                      sub_els = [#pubsub{unsubscribe = #pubsub_unsubscribe{
                                           node = Node,
                                           jid = my_jid(Config)}}]}),
    disconnect(Config).

roster_subscribe_master(Config) ->
    send(Config, #presence{}),
    ?recv1(#presence{}),
    wait_for_slave(Config),
    Peer = ?config(slave, Config),
    LPeer = jlib:jid_remove_resource(Peer),
    send(Config, #presence{type = subscribe, to = LPeer}),
    Push1 = ?recv1(#iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               ask = subscribe,
                                               subscription = none,
                                               jid = LPeer}]}]}),
    send(Config, make_iq_result(Push1)),
    {Push2, _} = ?recv2(
                    #iq{type = set,
                        sub_els = [#roster{items = [#roster_item{
                                                       subscription = to,
                                                       jid = LPeer}]}]},
                    #presence{type = subscribed, from = LPeer}),
    send(Config, make_iq_result(Push2)),
    ?recv1(#presence{type = undefined, from = Peer}),
    %% BUG: ejabberd sends previous push again. Is it ok?
    Push3 = ?recv1(#iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               subscription = to,
                                               jid = LPeer}]}]}),
    send(Config, make_iq_result(Push3)),
    ?recv1(#presence{type = subscribe, from = LPeer}),
    send(Config, #presence{type = subscribed, to = LPeer}),
    Push4 = ?recv1(#iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               subscription = both,
                                               jid = LPeer}]}]}),
    send(Config, make_iq_result(Push4)),
    %% Move into a group
    Groups = [<<"A">>, <<"B">>],
    Item = #roster_item{jid = LPeer, groups = Groups},
    I1 = send(Config, #iq{type = set, sub_els = [#roster{items = [Item]}]}),
    {Push5, _} = ?recv2(
                   #iq{type = set,
                       sub_els =
                           [#roster{items = [#roster_item{
                                                jid = LPeer,
                                                subscription = both}]}]},
                   #iq{type = result, id = I1, sub_els = []}),
    send(Config, make_iq_result(Push5)),
    #iq{sub_els = [#roster{items = [#roster_item{groups = G1}]}]} = Push5,
    Groups = lists:sort(G1),
    wait_for_slave(Config),
    ?recv1(#presence{type = unavailable, from = Peer}),
    disconnect(Config).

roster_subscribe_slave(Config) ->
    send(Config, #presence{}),
    ?recv1(#presence{}),
    wait_for_master(Config),
    Peer = ?config(master, Config),
    LPeer = jlib:jid_remove_resource(Peer),
    ?recv1(#presence{type = subscribe, from = LPeer}),
    send(Config, #presence{type = subscribed, to = LPeer}),
    Push1 = ?recv1(#iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               subscription = from,
                                               jid = LPeer}]}]}),
    send(Config, make_iq_result(Push1)),
    send(Config, #presence{type = subscribe, to = LPeer}),
    Push2 = ?recv1(#iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               ask = subscribe,
                                               subscription = from,
                                               jid = LPeer}]}]}),
    send(Config, make_iq_result(Push2)),
    {Push3, _} = ?recv2(
                    #iq{type = set,
                        sub_els = [#roster{items = [#roster_item{
                                                       subscription = both,
                                                       jid = LPeer}]}]},
                    #presence{type = subscribed, from = LPeer}),
    send(Config, make_iq_result(Push3)),
    ?recv1(#presence{type = undefined, from = Peer}),
    wait_for_master(Config),
    disconnect(Config).

roster_remove_master(Config) ->
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    LPeer = jlib:jid_remove_resource(Peer),
    Groups = [<<"A">>, <<"B">>],
    wait_for_slave(Config),
    send(Config, #presence{}),
    ?recv2(#presence{from = MyJID, type = undefined},
           #presence{from = Peer, type = undefined}),
    %% The peer removed us from its roster.
    {Push1, Push2, _, _, _} =
        ?recv5(
           %% TODO: I guess this can be optimized, we don't need
           %% to send transient roster push with subscription = 'to'.
           #iq{type = set,
               sub_els =
                   [#roster{items = [#roster_item{
                                        jid = LPeer,
                                        subscription = to}]}]},
           #iq{type = set,
               sub_els =
                   [#roster{items = [#roster_item{
                                        jid = LPeer,
                                        subscription = none}]}]},
           #presence{type = unsubscribe, from = LPeer},
           #presence{type = unsubscribed, from = LPeer},
           #presence{type = unavailable, from = Peer}),
    send(Config, make_iq_result(Push1)),
    send(Config, make_iq_result(Push2)),
    #iq{sub_els = [#roster{items = [#roster_item{groups = G1}]}]} = Push1,
    #iq{sub_els = [#roster{items = [#roster_item{groups = G2}]}]} = Push2,
    Groups = lists:sort(G1), Groups = lists:sort(G2),
    disconnect(Config).

roster_remove_slave(Config) ->
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    LPeer = jlib:jid_remove_resource(Peer),
    send(Config, #presence{}),
    ?recv1(#presence{from = MyJID, type = undefined}),
    wait_for_master(Config),
    ?recv1(#presence{from = Peer, type = undefined}),
    %% Remove the peer from roster.
    Item = #roster_item{jid = LPeer, subscription = remove},
    I = send(Config, #iq{type = set, sub_els = [#roster{items = [Item]}]}),
    {Push, _, _} = ?recv3(
                   #iq{type = set,
                       sub_els =
                           [#roster{items = [#roster_item{
                                                jid = LPeer,
                                                subscription = remove}]}]},
                   #iq{type = result, id = I, sub_els = []},
                   #presence{type = unavailable, from = Peer}),
    send(Config, make_iq_result(Push)),
    disconnect(Config).

proxy65_master(Config) ->
    Proxy = proxy_jid(Config),
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    wait_for_slave(Config),
    send(Config, #presence{}),
    ?recv1(#presence{from = MyJID, type = undefined}),
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
    ?recv1(#presence{from = MyJID, type = undefined}),
    wait_for_master(Config),
    {StreamHost, SID, Data} = get_event(Config),
    Socks5 = socks5_connect(StreamHost, {SID, Peer, MyJID}),
    wait_for_master(Config),
    socks5_recv(Socks5, Data),
    disconnect(Config).

muc_master(Config) ->
    MyJID = my_jid(Config),
    PeerJID = ?config(slave, Config),
    PeerBareJID = jlib:jid_remove_resource(PeerJID),
    PeerJIDStr = jlib:jid_to_string(PeerJID),
    MUC = muc_jid(Config),
    Room = muc_room_jid(Config),
    MyNick = ?config(master_nick, Config),
    MyNickJID = jlib:jid_replace_resource(Room, MyNick),
    PeerNick = ?config(slave_nick, Config),
    PeerNickJID = jlib:jid_replace_resource(Room, PeerNick),
    Subject = ?config(room_subject, Config),
    Localhost = jlib:make_jid(<<"">>, <<"localhost">>, <<"">>),
    true = is_feature_advertised(Config, ?NS_MUC, MUC),
    %% Joining
    send(Config, #presence{to = MyNickJID, sub_els = [#muc{}]}),
    %% As per XEP-0045 we MUST receive stanzas in the following order:
    %% 1. In-room presence from other occupants
    %% 2. In-room presence from the joining entity itself (so-called "self-presence")
    %% 3. Room history (if any)
    %% 4. The room subject
    %% 5. Live messages, presence updates, new user joins, etc.
    %% As this is the newly created room, we receive only the 2nd stanza.
    ?recv1(#presence{
          from = MyNickJID,
          sub_els = [#vcard_xupdate{},
		     #muc_user{
                        status_codes = Codes,
                        items = [#muc_item{role = moderator,
                                           jid = MyJID,
                                           affiliation = owner}]}]}),
    %% 110 -> Inform user that presence refers to itself
    %% 201 -> Inform user that a new room has been created
    [110, 201] = lists:sort(Codes),
    %% Request the configuration
    #iq{type = result, sub_els = [#muc_owner{config = #xdata{} = RoomCfg}]} =
        send_recv(Config, #iq{type = get, sub_els = [#muc_owner{}],
                              to = Room}),
    NewFields =
        lists:flatmap(
          fun(#xdata_field{var = Var, values = OrigVals}) ->
                  Vals = case Var of
                             <<"FORM_TYPE">> ->
                                 OrigVals;
                             <<"muc#roomconfig_roomname">> ->
                                 [<<"Test room">>];
                             <<"muc#roomconfig_roomdesc">> ->
                                 [<<"Trying to break the server">>];
                             <<"muc#roomconfig_persistentroom">> ->
                                 [<<"1">>];
			     <<"members_by_default">> ->
				 [<<"0">>];
			     <<"muc#roomconfig_allowvoicerequests">> ->
				 [<<"1">>];
			     <<"public_list">> ->
				 [<<"1">>];
			     <<"muc#roomconfig_publicroom">> ->
				 [<<"1">>];
                             _ ->
                                 []
                         end,
                  if Vals /= [] ->
                          [#xdata_field{values = Vals, var = Var}];
                     true ->
                          []
                  end
          end, RoomCfg#xdata.fields),
    NewRoomCfg = #xdata{type = submit, fields = NewFields},
    ID = send(Config, #iq{type = set, to = Room,
			  sub_els = [#muc_owner{config = NewRoomCfg}]}),
    ?recv2(#iq{type = result, id = ID},
	   #message{from = Room, type = groupchat,
		    sub_els = [#muc_user{status_codes = [104]}]}),
    %% Set subject
    send(Config, #message{to = Room, type = groupchat,
                          body = [#text{data = Subject}]}),
    ?recv1(#message{from = MyNickJID, type = groupchat,
             body = [#text{data = Subject}]}),
    %% Sending messages (and thus, populating history for our peer)
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
              I = send(Config, #message{to = Room, body = [Text],
					type = groupchat}),
	      ?recv1(#message{from = MyNickJID, id = I,
		       type = groupchat,
		       body = [Text]})
      end, lists:seq(1, 5)),
    %% Inviting the peer
    send(Config, #message{to = Room, type = normal,
			  sub_els =
			      [#muc_user{
				  invites =
				      [#muc_invite{to = PeerJID}]}]}),
    %% Peer is joining
    ?recv1(#presence{from = PeerNickJID,
	      sub_els = [#vcard_xupdate{},
			 #muc_user{
			    items = [#muc_item{role = visitor,
					       jid = PeerJID,
					       affiliation = none}]}]}),
    %% Receiving a voice request
    ?recv1(#message{from = Room,
	     sub_els = [#xdata{type = form,
			       instructions = [_],
			       fields = VoiceReqFs}]}),
    %% Approving the voice request
    ReplyVoiceReqFs =
	lists:map(
	  fun(#xdata_field{var = Var, values = OrigVals}) ->
                  Vals = case {Var, OrigVals} of
			     {<<"FORM_TYPE">>,
			      [<<"http://jabber.org/protocol/muc#request">>]} ->
				 OrigVals;
			     {<<"muc#role">>, [<<"participant">>]} ->
				 [<<"participant">>];
			     {<<"muc#jid">>, [PeerJIDStr]} ->
				 [PeerJIDStr];
			     {<<"muc#roomnick">>, [PeerNick]} ->
				 [PeerNick];
			     {<<"muc#request_allow">>, [<<"0">>]} ->
				 [<<"1">>]
			 end,
		  #xdata_field{values = Vals, var = Var}
	  end, VoiceReqFs),
    send(Config, #message{to = Room,
			  sub_els = [#xdata{type = submit,
					    fields = ReplyVoiceReqFs}]}),
    %% Peer is becoming a participant
    ?recv1(#presence{from = PeerNickJID,
	      sub_els = [#vcard_xupdate{},
			 #muc_user{
			    items = [#muc_item{role = participant,
					       jid = PeerJID,
					       affiliation = none}]}]}),
    %% Receive private message from the peer
    ?recv1(#message{from = PeerNickJID, body = [#text{data = Subject}]}),
    %% Granting membership to the peer and localhost server
    I1 = send(Config,
	      #iq{type = set, to = Room,
		  sub_els =
		      [#muc_admin{
			  items = [#muc_item{jid = Localhost,
					     affiliation = member},
				   #muc_item{nick = PeerNick,
					     jid = PeerBareJID,
					     affiliation = member}]}]}),
    %% Peer became a member
    ?recv1(#presence{from = PeerNickJID,
	      sub_els = [#vcard_xupdate{},
			 #muc_user{
			    items = [#muc_item{affiliation = member,
					       jid = PeerJID,
					       role = participant}]}]}),
    %% BUG: We should not receive any sub_els!
    ?recv1(#iq{type = result, id = I1, sub_els = [_|_]}),
    %% Receive groupchat message from the peer
    ?recv1(#message{type = groupchat, from = PeerNickJID,
	     body = [#text{data = Subject}]}),
    %% Kick the peer
    I2 = send(Config,
	      #iq{type = set, to = Room,
		  sub_els = [#muc_admin{
				items = [#muc_item{nick = PeerNick,
						   role = none}]}]}),
    %% Got notification the peer is kicked
    %% 307 -> Inform user that he or she has been kicked from the room
    ?recv1(#presence{from = PeerNickJID, type = unavailable,
	      sub_els = [#muc_user{
			    status_codes = [307],
			    items = [#muc_item{affiliation = member,
					       jid = PeerJID,
					       role = none}]}]}),
    %% BUG: We should not receive any sub_els!
    ?recv1(#iq{type = result, id = I2, sub_els = [_|_]}),
    %% Destroying the room
    I3 = send(Config,
	      #iq{type = set, to = Room,
		  sub_els = [#muc_owner{
				destroy = #muc_owner_destroy{
					     reason = Subject}}]}),
    %% Kicked off
    ?recv1(#presence{from = MyNickJID, type = unavailable,
              sub_els = [#muc_user{items = [#muc_item{role = none,
						      affiliation = none}],
				   destroy = #muc_user_destroy{
						reason = Subject}}]}),
    %% BUG: We should not receive any sub_els!
    ?recv1(#iq{type = result, id = I3, sub_els = [_|_]}),
    disconnect(Config).

muc_slave(Config) ->
    MyJID = my_jid(Config),
    MyBareJID = jlib:jid_remove_resource(MyJID),
    PeerJID = ?config(master, Config),
    MUC = muc_jid(Config),
    Room = muc_room_jid(Config),
    MyNick = ?config(slave_nick, Config),
    MyNickJID = jlib:jid_replace_resource(Room, MyNick),
    PeerNick = ?config(master_nick, Config),
    PeerNickJID = jlib:jid_replace_resource(Room, PeerNick),
    Subject = ?config(room_subject, Config),
    Localhost = jlib:make_jid(<<"">>, <<"localhost">>, <<"">>),
    %% Receive an invite from the peer
    ?recv1(#message{from = Room, type = normal,
	     sub_els =
		 [#muc_user{invites =
				[#muc_invite{from = PeerJID}]}]}),
    %% But before joining we discover the MUC service first
    %% to check if the room is in the disco list
    #iq{type = result,
	sub_els = [#disco_items{items = [#disco_item{jid = Room}]}]} =
	send_recv(Config, #iq{type = get, to = MUC,
			      sub_els = [#disco_items{}]}),
    %% Now check if the peer is in the room. We check this via disco#items
    #iq{type = result,
	sub_els = [#disco_items{items = [#disco_item{jid = PeerNickJID,
						     name = PeerNick}]}]} =
	send_recv(Config, #iq{type = get, to = Room,
			      sub_els = [#disco_items{}]}),
    %% Now joining
    send(Config, #presence{to = MyNickJID, sub_els = [#muc{}]}),
    %% First presence is from the participant, i.e. from the peer
    ?recv1(#presence{
       from = PeerNickJID,
       sub_els = [#vcard_xupdate{},
		  #muc_user{
		     status_codes = [],
		     items = [#muc_item{role = moderator,
					affiliation = owner}]}]}),
    %% The next is the self-presence (code 110 means it)
    ?recv1(#presence{
       from = MyNickJID,
       sub_els = [#vcard_xupdate{},
		  #muc_user{
		     status_codes = [110],
		     items = [#muc_item{role = visitor,
					affiliation = none}]}]}),
    %% Receive the room subject
    ?recv1(#message{from = PeerNickJID, type = groupchat,
             body = [#text{data = Subject}],
	     sub_els = [#delay{}]}),
    %% Receive MUC history
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
	      ?recv1(#message{from = PeerNickJID,
		       type = groupchat,
		       body = [Text],
		       sub_els = [#delay{}]})
      end, lists:seq(1, 5)),
    %% Sending a voice request
    VoiceReq = #xdata{
		  type = submit,
		  fields =
		      [#xdata_field{
			  var = <<"FORM_TYPE">>,
			  values = [<<"http://jabber.org/protocol/muc#request">>]},
		       #xdata_field{
			  var = <<"muc#role">>,
			  type = 'text-single',
			  values = [<<"participant">>]}]},
    send(Config, #message{to = Room, sub_els = [VoiceReq]}),
    %% Becoming a participant
    ?recv1(#presence{from = MyNickJID,
	      sub_els = [#vcard_xupdate{},
			 #muc_user{
			    items = [#muc_item{role = participant,
					       affiliation = none}]}]}),
    %% Sending private message to the peer
    send(Config, #message{to = PeerNickJID,
			  body = [#text{data = Subject}]}),
    %% Becoming a member
    ?recv1(#presence{from = MyNickJID,
	      sub_els = [#vcard_xupdate{},
			 #muc_user{
			    items = [#muc_item{role = participant,
					       affiliation = member}]}]}),
    %% Retrieving a member list
    #iq{type = result, sub_els = [#muc_admin{items = MemberList}]} =
	send_recv(Config,
		  #iq{type = get, to = Room,
		      sub_els =
			  [#muc_admin{items = [#muc_item{affiliation = member}]}]}),
    [#muc_item{affiliation = member,
	       jid = Localhost},
     #muc_item{affiliation = member,
	       jid = MyBareJID}] = lists:keysort(#muc_item.jid, MemberList),
    %% Sending groupchat message
    send(Config, #message{to = Room, type = groupchat,
			  body = [#text{data = Subject}]}),
    %% Receive this message back
    ?recv1(#message{type = groupchat, from = MyNickJID,
	     body = [#text{data = Subject}]}),
    %% We're kicked off
    %% 307 -> Inform user that he or she has been kicked from the room
    ?recv1(#presence{from = MyNickJID, type = unavailable,
	      sub_els = [#muc_user{
			    status_codes = [307],
			    items = [#muc_item{affiliation = member,
					       role = none}]}]}),
    disconnect(Config).

muc_register_nick(Config, MUC, PrevNick, Nick) ->
    {Registered, PrevNickVals} = if PrevNick /= <<"">> ->
					 {true, [PrevNick]};
				    true ->
					 {false, []}
				 end,
    %% Request register form
    #iq{type = result,
	sub_els = [#register{registered = Registered,
			     xdata = #xdata{type = form,
					    fields = FsWithoutNick}}]} =
	send_recv(Config, #iq{type = get, to = MUC,
			      sub_els = [#register{}]}),
    %% Check if 'nick' field presents
    #xdata_field{type = 'text-single',
		 var = <<"nick">>,
		 values = PrevNickVals} =
	lists:keyfind(<<"nick">>, #xdata_field.var, FsWithoutNick),
    X = #xdata{type = submit,
	       fields = [#xdata_field{var = <<"nick">>, values = [Nick]}]},
    %% Submitting form
    #iq{type = result, sub_els = [_|_]} =
	send_recv(Config, #iq{type = set, to = MUC,
			      sub_els = [#register{xdata = X}]}),
    %% Check if the nick was registered
    #iq{type = result,
	sub_els = [#register{registered = true,
			     xdata = #xdata{type = form,
					    fields = FsWithNick}}]} =
	send_recv(Config, #iq{type = get, to = MUC,
			      sub_els = [#register{}]}),
    #xdata_field{type = 'text-single', var = <<"nick">>,
		 values = [Nick]} =
	lists:keyfind(<<"nick">>, #xdata_field.var, FsWithNick).

muc_register_master(Config) ->
    MUC = muc_jid(Config),
    %% Register nick "master1"
    muc_register_nick(Config, MUC, <<"">>, <<"master1">>),
    %% Unregister nick "master1" via jabber:register
    #iq{type = result, sub_els = [_|_]} =
	send_recv(Config, #iq{type = set, to = MUC,
			      sub_els = [#register{remove = true}]}),
    %% Register nick "master2"
    muc_register_nick(Config, MUC, <<"">>, <<"master2">>),
    %% Now register nick "master"
    muc_register_nick(Config, MUC, <<"master2">>, <<"master">>),
    disconnect(Config).

muc_register_slave(Config) ->
    MUC = muc_jid(Config),
    %% Trying to register occupied nick "master"
    X = #xdata{type = submit,
	       fields = [#xdata_field{var = <<"nick">>,
				      values = [<<"master">>]}]},
    #iq{type = error} =
	send_recv(Config, #iq{type = set, to = MUC,
			      sub_els = [#register{xdata = X}]}),
    disconnect(Config).

announce_master(Config) ->
    MyJID = my_jid(Config),
    ServerJID = server_jid(Config),
    MotdJID = jlib:jid_replace_resource(ServerJID, <<"announce/motd">>),
    MotdText = #text{data = <<"motd">>},
    send(Config, #presence{}),
    ?recv1(#presence{from = MyJID}),
    %% Set message of the day
    send(Config, #message{to = MotdJID, body = [MotdText]}),
    %% Receive this message back
    ?recv1(#message{from = ServerJID, body = [MotdText]}),
    disconnect(Config).

announce_slave(Config) ->
    MyJID = my_jid(Config),
    ServerJID = server_jid(Config),
    MotdDelJID = jlib:jid_replace_resource(ServerJID, <<"announce/motd/delete">>),
    MotdText = #text{data = <<"motd">>},
    send(Config, #presence{}),
    ?recv2(#presence{from = MyJID},
	   #message{from = ServerJID, body = [MotdText]}),
    %% Delete message of the day
    send(Config, #message{to = MotdDelJID}),
    disconnect(Config).

offline_master(Config) ->
    Peer = ?config(slave, Config),
    LPeer = jlib:jid_remove_resource(Peer),
    send(Config, #message{to = LPeer,
                          body = [#text{data = <<"body">>}],
                          subject = [#text{data = <<"subject">>}]}),
    disconnect(Config).

offline_slave(Config) ->
    Peer = ?config(master, Config),
    send(Config, #presence{}),
    {_, #message{sub_els = SubEls}} =
        ?recv2(#presence{},
               #message{from = Peer,
                        body = [#text{data = <<"body">>}],
                        subject = [#text{data = <<"subject">>}]}),
    true = lists:keymember(delay, 1, SubEls),
    disconnect(Config).

carbons_master(Config) ->
    MyJID = my_jid(Config),
    MyBareJID = jlib:jid_remove_resource(MyJID),
    Peer = ?config(slave, Config),
    Txt = #text{data = <<"body">>},
    true = is_feature_advertised(Config, ?NS_CARBONS_2),
    send(Config, #presence{priority = 10}),
    ?recv1(#presence{from = MyJID}),
    wait_for_slave(Config),
    ?recv1(#presence{from = Peer}),
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
    MyBareJID = jlib:jid_remove_resource(MyJID),
    Peer = ?config(master, Config),
    Txt = #text{data = <<"body">>},
    wait_for_master(Config),
    send(Config, #presence{priority = 5}),
    ?recv2(#presence{from = MyJID}, #presence{from = Peer}),
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
			forwarded = #forwarded{
				       sub_els =
					   [#message{from = Peer,
						     to = MyBareJID,
						     type = chat,
						     body = [Txt]}]}}]},
       #message{from = MyBareJID, to = MyJID, type = chat,
		sub_els =
		    [#carbons_sent{
			forwarded = #forwarded{
				       sub_els =
					   [#message{from = Peer,
						     to = Peer,
						     type = chat,
						     body = [Txt]}]}}]},
       #message{from = MyBareJID, to = MyJID, type = chat,
		sub_els =
		    [#carbons_received{
			forwarded = #forwarded{
				       sub_els =
					   [#message{from = Peer,
						     to = MyBareJID,
						     type = chat,
						     body = [Txt]}]}}]},
       #message{from = MyBareJID, to = MyJID, type = chat,
		sub_els =
		    [#carbons_received{
			forwarded = #forwarded{
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
    ?recv1(#presence{from = Peer, type = unavailable}),
    disconnect(Config).

mam_old_master(Config) ->
    mam_master(Config, ?NS_MAM_TMP).

mam_new_master(Config) ->
    mam_master(Config, ?NS_MAM_0).

mam_master(Config, NS) ->
    true = is_feature_advertised(Config, NS),
    MyJID = my_jid(Config),
    BareMyJID = jlib:jid_remove_resource(MyJID),
    Peer = ?config(slave, Config),
    send(Config, #presence{}),
    ?recv1(#presence{}),
    wait_for_slave(Config),
    ?recv1(#presence{from = Peer}),
    #iq{type = result, sub_els = []} =
        send_recv(Config,
                  #iq{type = set,
                      sub_els = [#mam_prefs{xmlns = NS,
					    default = roster,
                                            never = [MyJID]}]}),
    if NS == ?NS_MAM_TMP ->
	    FakeArchived = #mam_archived{id = randoms:get_string(),
					 by = server_jid(Config)},
	    send(Config, #message{to = MyJID,
				  sub_els = [FakeArchived],
				  body = [#text{data = <<"a">>}]}),
	    send(Config, #message{to = BareMyJID,
				  sub_els = [FakeArchived],
				  body = [#text{data = <<"b">>}]}),
	    %% NOTE: The server should strip fake archived tags,
	    %% i.e. the sub_els received should be [].
	    ?recv2(#message{body = [#text{data = <<"a">>}], sub_els = []},
		   #message{body = [#text{data = <<"b">>}], sub_els = []});
       true ->
	    ok
    end,
    wait_for_slave(Config),
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
              send(Config,
                   #message{to = Peer, body = [Text]})
      end, lists:seq(1, 5)),
    ?recv1(#presence{type = unavailable, from = Peer}),
    mam_query_all(Config, NS),
    mam_query_with(Config, Peer, NS),
    %% mam_query_with(Config, jlib:jid_remove_resource(Peer)),
    mam_query_rsm(Config, NS),
    #iq{type = result, sub_els = []} =
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
    ServerJID = server_jid(Config),
    wait_for_master(Config),
    send(Config, #presence{}),
    ?recv2(#presence{}, #presence{from = Peer}),
    #iq{type = result, sub_els = []} =
        send_recv(Config,
                  #iq{type = set,
                      sub_els = [#mam_prefs{xmlns = NS, default = always}]}),
    wait_for_master(Config),
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
	      ?recv1(#message{from = Peer, body = [Text],
			      sub_els = [#mam_archived{by = ServerJID}]})
      end, lists:seq(1, 5)),
    #iq{type = result, sub_els = []} =
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
    maybe_recv_iq_result(NS, I),
    Iter = if NS == ?NS_MAM_TMP -> lists:seq(1, 5);
	      true -> lists:seq(1, 5) ++ lists:seq(1, 5)
	   end,
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
              ?recv1(#message{to = MyJID,
                       sub_els =
                           [#mam_result{
                               queryid = QID,
                               sub_els =
                                   [#forwarded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]})
      end, Iter),
    if NS == ?NS_MAM_TMP ->
	    ?recv1(#iq{type = result, id = I,
		       sub_els = [#mam_query{xmlns = NS, id = QID}]});
       true ->
	    ?recv1(#message{sub_els = [#mam_fin{complete = true, id = QID}]})
    end.

mam_query_with(Config, JID, NS) ->
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    {Query, Type} = if NS == ?NS_MAM_TMP ->
		    {#mam_query{xmlns = NS, with = JID}, get};
	       true ->
		    Fs = [#xdata_field{var = <<"jid">>,
				       values = [jlib:jid_to_string(JID)]}],
		    {#mam_query{xmlns = NS,
			       xdata = #xdata{type = submit, fields = Fs}}, set}
	    end,
    I = send(Config, #iq{type = Type, sub_els = [Query]}),
    Iter = if NS == ?NS_MAM_TMP -> lists:seq(1, 5);
	      true -> lists:seq(1, 5) ++ lists:seq(1, 5)
	   end,
    maybe_recv_iq_result(NS, I),
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
              ?recv1(#message{to = MyJID,
                       sub_els =
                           [#mam_result{
                               sub_els =
                                   [#forwarded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]})
      end, Iter),
    if NS == ?NS_MAM_TMP ->
	    ?recv1(#iq{type = result, id = I,
		       sub_els = [#mam_query{xmlns = NS}]});
       true ->
	    ?recv1(#message{sub_els = [#mam_fin{complete = true}]})
    end.

maybe_recv_iq_result(?NS_MAM_0, I1) ->
    ?recv1(#iq{type = result, id = I1});
maybe_recv_iq_result(_, _) ->
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
    maybe_recv_iq_result(NS, I1),
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
              ?recv1(#message{to = MyJID,
                       sub_els =
                           [#mam_result{
			       xmlns = NS,
                               sub_els =
                                   [#forwarded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]})
      end, lists:seq(1, 3)),
    if NS == ?NS_MAM_TMP ->
	    ?recv1(#iq{type = result, id = I1,
		       sub_els = [#mam_query{xmlns = NS,
					     rsm = #rsm_set{last = Last, count = 5}}]});
       true ->
	    ?recv1(#message{sub_els = [#mam_fin{
					  complete = false,
					  rsm = #rsm_set{last = Last, count = 10}}]})
    end,
    %% Get the next items starting from the `Last`.
    %% Limit the response to 2 items.
    I2 = send(Config,
              #iq{type = Type,
                  sub_els = [#mam_query{xmlns = NS,
					rsm = #rsm_set{max = 2,
                                                       'after' = Last}}]}),
    maybe_recv_iq_result(NS, I2),
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
              ?recv1(#message{to = MyJID,
                       sub_els =
                           [#mam_result{
			       xmlns = NS,
                               sub_els =
                                   [#forwarded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]})
      end, lists:seq(4, 5)),
    if NS == ?NS_MAM_TMP ->
	    ?recv1(#iq{type = result, id = I2,
		       sub_els = [#mam_query{
				     xmlns = NS,
				     rsm = #rsm_set{
					      count = 5,
					      first = #rsm_first{data = First}}}]});
       true ->
	    ?recv1(#message{
		      sub_els = [#mam_fin{
				    complete = false,
				    rsm = #rsm_set{
					      count = 10,
					      first = #rsm_first{data = First}}}]})
    end,
    %% Paging back. Should receive 3 elements: 1, 2, 3.
    I3 = send(Config,
              #iq{type = Type,
                  sub_els = [#mam_query{xmlns = NS,
					rsm = #rsm_set{max = 3,
                                                       before = First}}]}),
    maybe_recv_iq_result(NS, I3),
    lists:foreach(
      fun(N) ->
              Text = #text{data = jlib:integer_to_binary(N)},
              ?recv1(#message{to = MyJID,
                       sub_els =
                           [#mam_result{
			       xmlns = NS,
                               sub_els =
                                   [#forwarded{
                                       delay = #delay{},
                                       sub_els =
                                           [#message{
                                               from = MyJID, to = Peer,
                                               body = [Text]}]}]}]})
      end, lists:seq(1, 3)),
    if NS == ?NS_MAM_TMP ->
	    ?recv1(#iq{type = result, id = I3,
		       sub_els = [#mam_query{xmlns = NS, rsm = #rsm_set{count = 5}}]});
       true ->
	    ?recv1(#message{
		      sub_els = [#mam_fin{complete = true,
					  rsm = #rsm_set{count = 10}}]})
    end,
    %% Getting the item count. Should be 5 (or 10).
    I4 = send(Config,
	      #iq{type = Type,
		  sub_els = [#mam_query{xmlns = NS,
					rsm = #rsm_set{max = 0}}]}),
    maybe_recv_iq_result(NS, I4),
    if NS == ?NS_MAM_TMP ->
	    ?recv1(#iq{type = result, id = I4,
		       sub_els = [#mam_query{
				     xmlns = NS,
				     rsm = #rsm_set{count = 5,
						    first = undefined,
						    last = undefined}}]});
       true ->
	    ?recv1(#message{
		      sub_els = [#mam_fin{
				    complete = false,
				    rsm = #rsm_set{count = 10,
						   first = undefined,
						   last = undefined}}]})
    end,
    %% Should receive 2 last messages
    I5 = send(Config,
	      #iq{type = Type,
		  sub_els = [#mam_query{xmlns = NS,
					rsm = #rsm_set{max = 2,
						       before = none}}]}),
    maybe_recv_iq_result(NS, I5),
    lists:foreach(
      fun(N) ->
	      Text = #text{data = jlib:integer_to_binary(N)},
	      ?recv1(#message{to = MyJID,
			      sub_els =
				  [#mam_result{
				      xmlns = NS,
				      sub_els =
					  [#forwarded{
					      delay = #delay{},
					      sub_els =
						  [#message{
						      from = MyJID, to = Peer,
						      body = [Text]}]}]}]})
      end, lists:seq(4, 5)),
    if NS == ?NS_MAM_TMP ->
	    ?recv1(#iq{type = result, id = I5,
		       sub_els = [#mam_query{xmlns = NS, rsm = #rsm_set{count = 5}}]});
       true ->
	    ?recv1(#message{
		      sub_els = [#mam_fin{complete = false,
					  rsm = #rsm_set{count = 10}}]})
    end.

client_state_master(Config) ->
    true = ?config(csi, Config),
    Peer = ?config(slave, Config),
    Presence = #presence{to = Peer},
    ChatState = #message{to = Peer, thread = <<"1">>,
			 sub_els = [#chatstate{type = active}]},
    Message = ChatState#message{body = [#text{data = <<"body">>}]},
    %% Wait for the slave to become inactive.
    wait_for_slave(Config),
    %% Should be dropped:
    send(Config, ChatState),
    %% Should be queued (but see below):
    send(Config, Presence),
    %% Should replace the previous presence in the queue:
    send(Config, Presence#presence{type = unavailable}),
    %% Should be sent immediately, together with the previous presence:
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
    ?recv1(#presence{from = Peer, type = unavailable,
		     sub_els = [#delay{}]}),
    ?recv1(#message{from = Peer, thread = <<"1">>,
		    body = [#text{data = <<"body">>}],
		    sub_els = [#chatstate{type = active}]}),
    change_client_state(Config, active),
    wait_for_master(Config),
    ?recv1(#message{from = Peer, thread = <<"1">>,
		    sub_els = [#chatstate{type = active}]}),
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
                         jid = jlib:make_jid(
                                 <<"some">>,
                                 <<"some.conference.org">>,
                                 <<>>)}.

socks5_connect(#streamhost{host = Host, port = Port},
               {SID, JID1, JID2}) ->
    Hash = p1_sha:sha([SID, jlib:jid_to_string(JID1), jlib:jid_to_string(JID2)]),
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
    case ejabberd_odbc:sql_transaction(
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
    {URoom, SRoom, _} = jlib:jid_tolower(Room),
    ejabberd_auth:remove_user(User, Server),
    ejabberd_auth:remove_user(<<"test_slave">>, Server),
    ejabberd_auth:remove_user(<<"test_master">>, Server),
    mod_muc:forget_room(Server, URoom, SRoom),
    ejabberd_riak:delete(muc_registered, {{<<"test_slave">>, Server}, SRoom}),
    ejabberd_riak:delete(muc_registered, {{<<"test_master">>, Server}, SRoom}),
    Config.
