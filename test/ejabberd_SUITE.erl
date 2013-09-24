%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2013, Evgeniy Khramtsov
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
                bind/1, auth/1, open_session/1, zlib/1, starttls/1]).

-include("suite.hrl").

suite() ->
    [{timetrap, {seconds,20}}].

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
init_per_group(ldap, Config) ->
    set_opt(server, ?LDAP_VHOST, Config);
init_per_group(extauth, Config) ->
    set_opt(server, ?EXTAUTH_VHOST, Config);
init_per_group(_GroupName, Config) ->
    Pid = start_event_relay(),
    set_opt(event_relay, Pid, Config).

end_per_group(mnesia, _Config) ->
    ok;
end_per_group(mysql, _Config) ->
    ok;
end_per_group(pgsql, _Config) ->
    ok;
end_per_group(no_db, _Config) ->
    ok;
end_per_group(ldap, _Config) ->
    ok;
end_per_group(extauth, _Config) ->
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
    Test = atom_to_list(TestCase),
    IsMaster = lists:suffix("_master", Test),
    IsSlave = lists:suffix("_slave", Test),
    User = if IsMaster -> <<"test_master">>;
              IsSlave -> <<"test_slave">>;
              true -> <<"test_single">>
           end,
    Slave = jlib:make_jid(<<"test_slave">>, Server, Resource),
    Master = jlib:make_jid(<<"test_master">>, Server, Resource),
    Config = set_opt(user, User,
                     set_opt(slave, Slave,
                             set_opt(master, Master, OrigConfig))),
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
       disco]},
     {test_proxy65, [parallel],
      [proxy65_master, proxy65_slave]}].

db_tests() ->
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
       muc_single,
       pubsub,
       test_unregister]},
     {test_roster_subscribe, [parallel],
      [roster_subscribe_master,
       roster_subscribe_slave]},
     {test_offline, [sequence],
      [offline_master, offline_slave]},
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
     {mnesia, [sequence], db_tests()},
     {mysql, [sequence], db_tests()},
     {pgsql, [sequence], db_tests()}].

all() ->
    [{group, ldap},
     {group, no_db},
     {group, mnesia},
     {group, mysql},
     {group, pgsql},
     {group, extauth},
     stop_ejabberd].

stop_ejabberd(Config) ->
    ok = application:stop(ejabberd),
    #stream_error{reason = 'system-shutdown'} = recv(),
    {xmlstreamend, <<"stream:stream">>} = recv(),
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
    #stream_error{reason = conflict} = recv(),
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
    #iq{type = set, sub_els = [#roster{ver = Ver2}]} = recv(),
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
    #presence{from = JID, to = JID} = recv(),
    disconnect(Config).

presence_broadcast(Config) ->
    send(Config, #presence{}),
    JID = my_jid(Config),
    %% We receive the welcome message and the presence broadcast
    ?recv2(#message{type = normal},
           #presence{from = JID, to = JID}),
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
    %% Get subscription options
    true = lists:member(?PUBSUB("subscription-options"), Features),
    #iq{type = result, sub_els = [#pubsub{options = #pubsub_options{
                                            node = Node}}]} =
        send_recv(Config,
                  #iq{type = get, to = pubsub_jid(Config),
                      sub_els = [#pubsub{options = #pubsub_options{
                                           node = Node,
                                           jid = my_jid(Config)}}]}),
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
                                          retract = [ItemID]}]},
                           #shim{headers = [{<<"Collection">>, Node}]}]}),
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
    #presence{} = recv(),
    wait_for_slave(Config),
    Peer = ?config(slave, Config),
    LPeer = jlib:jid_remove_resource(Peer),
    send(Config, #presence{type = subscribe, to = LPeer}),
    Push1 = #iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               ask = subscribe,
                                               subscription = none,
                                               jid = LPeer}]}]} = recv(),
    send(Config, make_iq_result(Push1)),
    {Push2, _} = ?recv2(
                    #iq{type = set,
                        sub_els = [#roster{items = [#roster_item{
                                                       subscription = to,
                                                       jid = LPeer}]}]},
                    #presence{type = subscribed, from = LPeer}),
    send(Config, make_iq_result(Push2)),
    #presence{type = undefined, from = Peer} = recv(),
    %% BUG: ejabberd sends previous push again. Is it ok?
    Push3 = #iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               subscription = to,
                                               jid = LPeer}]}]} = recv(),
    send(Config, make_iq_result(Push3)),
    #presence{type = subscribe, from = LPeer} = recv(),
    send(Config, #presence{type = subscribed, to = LPeer}),
    Push4 = #iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               subscription = both,
                                               jid = LPeer}]}]} = recv(),
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
    #presence{type = unavailable, from = Peer} = recv(),
    disconnect(Config).

roster_subscribe_slave(Config) ->
    send(Config, #presence{}),
    #presence{} = recv(),
    wait_for_master(Config),
    Peer = ?config(master, Config),
    LPeer = jlib:jid_remove_resource(Peer),
    #presence{type = subscribe, from = LPeer} = recv(),
    send(Config, #presence{type = subscribed, to = LPeer}),
    Push1 = #iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               subscription = from,
                                               jid = LPeer}]}]} = recv(),
    send(Config, make_iq_result(Push1)),
    send(Config, #presence{type = subscribe, to = LPeer}),
    Push2 = #iq{type = set,
                sub_els = [#roster{items = [#roster_item{
                                               ask = subscribe,
                                               subscription = from,
                                               jid = LPeer}]}]} = recv(),
    send(Config, make_iq_result(Push2)),
    {Push3, _} = ?recv2(
                    #iq{type = set,
                        sub_els = [#roster{items = [#roster_item{
                                                       subscription = both,
                                                       jid = LPeer}]}]},
                    #presence{type = subscribed, from = LPeer}),
    send(Config, make_iq_result(Push3)),
    #presence{type = undefined, from = Peer} = recv(),
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
    #presence{from = MyJID, type = undefined} = recv(),
    wait_for_master(Config),
    #presence{from = Peer, type = undefined} = recv(),
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
    #presence{from = MyJID, type = undefined} = recv(),
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
    %%#presence{type = unavailable, from = Peer} = recv(),
    disconnect(Config).

proxy65_slave(Config) ->
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    send(Config, #presence{}),
    #presence{from = MyJID, type = undefined} = recv(),
    wait_for_master(Config),
    {StreamHost, SID, Data} = get_event(Config),
    Socks5 = socks5_connect(StreamHost, {SID, Peer, MyJID}),
    wait_for_master(Config),
    socks5_recv(Socks5, Data),
    disconnect(Config).

muc_single(Config) ->
    MyJID = my_jid(Config),
    MUC = muc_jid(Config),
    Room = muc_room_jid(Config),
    Nick = ?config(user, Config),
    NickJID = jlib:jid_replace_resource(Room, Nick),
    true = is_feature_advertised(Config, ?NS_MUC, MUC),
    %% Joining
    send(Config, #presence{to = NickJID, sub_els = [#muc{}]}),
    %% As per XEP-0045 we MUST receive stanzas in the following order:
    %% 1. In-room presence from other occupants
    %% 2. In-room presence from the joining entity itself (so-called "self-presence")
    %% 3. Room history (if any)
    %% 4. The room subject
    %% 5. Live messages, presence updates, new user joins, etc.
    %% As this is the newly created room, we receive only the 2nd stanza.
    #presence{
          from = NickJID,
          sub_els = [#muc_user{
                        status_codes = Codes,
                        items = [#muc_item{role = moderator,
                                           jid = MyJID,
                                           affiliation = owner}]}]} = recv(),
    %% 110 -> Inform user that presence refers to itself
    %% 201 -> Inform user that a new room has been created
    true = lists:member(110, Codes),
    true = lists:member(201, Codes),
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
                             <<"muc#roomconfig_changesubject">> ->
                                 [<<"0">>];
                             <<"muc#roomconfig_allowinvites">> ->
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
    %% BUG: We should not receive any sub_els!
    %% TODO: fix this crap in ejabberd.
    #iq{type = result, sub_els = [_|_]} =
        send_recv(Config, #iq{type = set, to = Room,
                              sub_els = [#muc_owner{config = NewRoomCfg}]}),
    %% Set subject
    send(Config, #message{to = Room, type = groupchat,
                          body = [#text{data = <<"Subject">>}]}),
    #message{from = NickJID, type = groupchat,
             body = [#text{data = <<"Subject">>}]} = recv(),
    %% Leaving
    send(Config, #presence{type = unavailable, to = NickJID}),
    #presence{from = NickJID, type = unavailable,
              sub_els = [#muc_user{status_codes = NewCodes}]} = recv(),
    true = lists:member(110, NewCodes),
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
    true = lists:keymember(legacy_delay, 1, SubEls),
    disconnect(Config).

%%%===================================================================
%%% Aux functions
%%%===================================================================
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
