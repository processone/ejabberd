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

-include_lib("common_test/include/ct.hrl").
-include("xml.hrl").
-include("ns.hrl").
-include("ejabberd.hrl").
-include("mod_proxy65.hrl").
-include("xmpp_codec.hrl").

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns:stream='http://etherx.jabber.org/stream"
	  "s' xmlns='jabber:client' to='~s' version='1.0"
	  "'>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(PUBSUB(Node), <<(?NS_PUBSUB)/binary, "#", Node>>).

-define(recv2(P1, P2),
        (fun() ->
                 case {R1 = recv(), R2 = recv()} of
                     {P1, P2} -> {R1, R2};
                     {P2, P1} -> {R2, R1}
                 end
         end)()).

-define(recv3(P1, P2, P3),
        (fun() ->
                 case R3 = recv() of
                     P1 -> insert(R3, 1, ?recv2(P2, P3));
                     P2 -> insert(R3, 2, ?recv2(P1, P3));
                     P3 -> insert(R3, 3, ?recv2(P1, P2))
                 end
         end)()).

-define(recv4(P1, P2, P3, P4),
        (fun() ->
                 case R4 = recv() of
                     P1 -> insert(R4, 1, ?recv3(P2, P3, P4));
                     P2 -> insert(R4, 2, ?recv3(P1, P3, P4));
                     P3 -> insert(R4, 3, ?recv3(P1, P2, P4));
                     P4 -> insert(R4, 4, ?recv3(P1, P2, P3))
                 end
         end)()).

-define(recv5(P1, P2, P3, P4, P5),
        (fun() ->
                 case R5 = recv() of
                     P1 -> insert(R5, 1, ?recv4(P2, P3, P4, P5));
                     P2 -> insert(R5, 2, ?recv4(P1, P3, P4, P5));
                     P3 -> insert(R5, 3, ?recv4(P1, P2, P4, P5));
                     P4 -> insert(R5, 4, ?recv4(P1, P2, P3, P5));
                     P5 -> insert(R5, 5, ?recv4(P1, P2, P3, P4))
                 end
         end)()).

suite() ->
    [{timetrap, {seconds,10}}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    ConfigPath = filename:join([DataDir, "ejabberd.cfg"]),
    LogPath = filename:join([PrivDir, "ejabberd.log"]),
    SASLPath = filename:join([PrivDir, "sasl.log"]),
    MnesiaDir = filename:join([PrivDir, "mnesia"]),
    CertFile = filename:join([DataDir, "cert.pem"]),
    {ok, CWD} = file:get_cwd(),
    {ok, _} = file:copy(CertFile, filename:join([CWD, "cert.pem"])),
    application:set_env(ejabberd, config, ConfigPath),
    application:set_env(ejabberd, log_path, LogPath),
    application:set_env(sasl, sasl_error_logger, {file, SASLPath}),
    application:set_env(mnesia, dir, MnesiaDir),
    ok = application:start(ejabberd),
    [{server, <<"localhost">>},
     {port, 5222},
     {host, "localhost"},
     {certfile, CertFile},
     {resource, <<"resource">>},
     {password, <<"password">>}
     |Config].

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Pid = start_event_relay(),
    set_opt(event_relay, Pid, Config).

end_per_group(_GroupName, Config) ->
    stop_event_relay(Config),
    ok.

init_per_testcase(stop_ejabberd, OrigConfig) ->
    User = <<"test_stop">>,
    Config = set_opt(user, User, OrigConfig),
    ejabberd_auth:try_register(User,
                               ?config(server, Config),
                               ?config(password, Config)),
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

groups() ->
    [{single_user, [sequence],
      [test_connect,
       test_starttls,
       test_zlib,
       test_register,
       auth_plain,
       auth_md5,
       test_auth,
       test_bind,
       test_open_session,
       roster_get,
       presence_broadcast,
       ping,
       version,
       time,
       stats,
       disco,
       last,
       private,
       privacy,
       blocking,
       vcard,
       pubsub,
       muc_single,
       test_unregister]},
     {test_roster_subscribe, [parallel],
      [roster_subscribe_master,
       roster_subscribe_slave]},
     {test_proxy65, [parallel],
      [proxy65_master, proxy65_slave]},
     {test_offline, [sequence],
      [offline_master, offline_slave]},
     {test_roster_remove, [parallel],
      [roster_remove_master,
       roster_remove_slave]}].

all() ->
    [{group, single_user},
     {group, test_roster_subscribe},
     {group, test_proxy65},
     {group, test_offline},
     {group, test_roster_remove},
     stop_ejabberd].

stop_ejabberd(Config) ->
    ok = application:stop(ejabberd),
    #stream_error{reason = 'system-shutdown'} = recv(),
    {xmlstreamend, <<"stream:stream">>} = recv(),
    Config.

test_connect(Config) ->
    disconnect(connect(Config)).

connect(Config) ->
    {ok, Sock} = ejabberd_socket:connect(
                   ?config(host, Config),
                   ?config(port, Config),
                   [binary, {packet, 0}, {active, false}]),
    init_stream(set_opt(socket, Sock, Config)).

init_stream(Config) ->
    ok = send_text(Config, io_lib:format(?STREAM_HEADER,
                                          [?config(server, Config)])),
    {xmlstreamstart, <<"stream:stream">>, Attrs} = recv(),
    <<"jabber:client">> = xml:get_attr_s(<<"xmlns">>, Attrs),
    <<"1.0">> = xml:get_attr_s(<<"version">>, Attrs),
    #stream_features{sub_els = Fs} = recv(),
    Mechs = lists:flatmap(
              fun(#sasl_mechanisms{list = Ms}) ->
                      Ms;
                 (_) ->
                      []
              end, Fs),
    lists:foldl(
      fun(#feature_register{}, Acc) ->
              set_opt(register, true, Acc);
         (#starttls{}, Acc) ->
              set_opt(starttls, true, Acc);
         (#compression{methods = Ms}, Acc) ->
              set_opt(compression, Ms, Acc);
         (_, Acc) ->
              Acc
      end, set_opt(mechs, Mechs, Config), Fs).

disconnect(Config) ->
    Socket = ?config(socket, Config),
    ok = ejabberd_socket:send(Socket, ?STREAM_TRAILER),
    {xmlstreamend, <<"stream:stream">>} = recv(),
    ejabberd_socket:close(Socket),
    Config.

test_starttls(Config) ->
    case ?config(starttls, Config) of
        true ->
            disconnect(starttls(Config));
        _ ->
            {skipped, 'starttls_not_available'}
    end.

starttls(Config) ->
    send(Config, #starttls{}),
    #starttls_proceed{} = recv(),
    TLSSocket = ejabberd_socket:starttls(
                  ?config(socket, Config),
                  [{certfile, ?config(certfile, Config)},
                   connect]),
    init_stream(set_opt(socket, TLSSocket, Config)).

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

zlib(Config) ->
    send(Config, #compress{methods = [<<"zlib">>]}),
    #compressed{} = recv(),
    ZlibSocket = ejabberd_socket:compress(?config(socket, Config)),
    init_stream(set_opt(socket, ZlibSocket, Config)).

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

test_auth(Config) ->
    disconnect(auth(Config)).

auth(Config) ->
    Mechs = ?config(mechs, Config),
    HaveMD5 = lists:member(<<"DIGEST-MD5">>, Mechs),
    HavePLAIN = lists:member(<<"PLAIN">>, Mechs),
    if HavePLAIN ->
            auth_SASL(<<"PLAIN">>, Config);
       HaveMD5 ->
            auth_SASL(<<"DIGEST-MD5">>, Config);
       true ->
            ct:fail(no_sasl_mechanisms_available)
    end.

test_bind(Config) ->
    disconnect(bind(Config)).

bind(Config) ->
    #iq{type = result, sub_els = [#bind{}]} =
        send_recv(
          Config,
          #iq{type = set,
              sub_els = [#bind{resource = ?config(resource, Config)}]}),
    Config.

test_open_session(Config) ->
    disconnect(open_session(Config)).

open_session(Config) ->
    #iq{type = result, sub_els = []} =
        send_recv(Config, #iq{type = set, sub_els = [#session{}]}),
    Config.

roster_get(Config) ->
    #iq{type = result, sub_els = [#roster{items = []}]} =
        send_recv(Config, #iq{type = get, sub_els = [#roster{}]}),
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
    #iq{type = error} =
        send_recv(Config, #iq{type = get, sub_els = [#private{}],
                              to = server_jid(Config)}),
    #iq{type = result, sub_els = []} =
        send_recv(
          Config, #iq{type = set,
                      sub_els = [#private{sub_els = [Storage]}]}),
    #iq{type = result,
        sub_els = [#private{sub_els = [Storage]}]} =
        send_recv(
          Config,
          #iq{type = get,
              sub_els = [#private{sub_els = [#bookmark_storage{}]}]}),
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
    true = is_feature_advertised(Config, ?NS_PUBSUB),
    %% Publish <presence/> element within node "presence"
    ItemID = randoms:get_string(),
    Node = <<"presence">>,
    Item = #pubsub_item{id = ItemID, sub_els = [#presence{}]},
    #iq{type = result,
        sub_els = [#pubsub{publish = {<<"presence">>,
                                      [#pubsub_item{id = ItemID}]}}]} =
        send_recv(Config,
                  #iq{type = set, to = pubsub_jid(Config),
                      sub_els = [#pubsub{publish = {Node, [Item]}}]}),
    %% Subscribe to node "presence"
    I = send(Config,
             #iq{type = set, to = pubsub_jid(Config),
                 sub_els = [#pubsub{subscribe = {Node, my_jid(Config)}}]}),
    ?recv2(
       #message{sub_els = [#pubsub_event{}, #delay{}]},
       #iq{type = result, id = I}),
    %% Get subscriptions
    true = is_feature_advertised(Config, ?PUBSUB("retrieve-subscriptions")),
    #iq{type = result,
        sub_els =
            [#pubsub{subscriptions =
                         {none, [#pubsub_subscription{node = Node}]}}]} =
        send_recv(Config, #iq{type = get, to = pubsub_jid(Config),
                              sub_els = [#pubsub{subscriptions = {none, []}}]}),
    %% Get affiliations
    true = is_feature_advertised(Config, ?PUBSUB("retrieve-affiliations")),
    #iq{type = result,
        sub_els = [#pubsub{
                      affiliations =
                          [#pubsub_affiliation{node = Node, type = owner}]}]} =
        send_recv(Config, #iq{type = get, to = pubsub_jid(Config),
                              sub_els = [#pubsub{affiliations = []}]}),
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
    {Push, _} = ?recv2(
                   #iq{type = set,
                       sub_els =
                           [#roster{items = [#roster_item{
                                                jid = LPeer,
                                                subscription = remove}]}]},
                   #iq{type = result, id = I, sub_els = []}),
    send(Config, make_iq_result(Push)),
    #presence{type = unavailable, from = Peer} = recv(),
    disconnect(Config).

proxy65_master(Config) ->
    Proxy = proxy_jid(Config),
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    wait_for_slave(Config),
    send(Config, #presence{}),
    ?recv2(#presence{from = MyJID, type = undefined},
           #presence{from = Peer, type = undefined}),
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
    #presence{type = unavailable, from = Peer} = recv(),
    disconnect(Config).

proxy65_slave(Config) ->
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    send(Config, #presence{}),
    #presence{from = MyJID, type = undefined} = recv(),
    wait_for_master(Config),
    #presence{from = Peer, type = undefined} = recv(),
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
    lists:foreach(
      fun(#legacy_delay{}) -> ok;
         (#delay{}) -> ok
      end, SubEls),
    disconnect(Config).

auth_SASL(Mech, Config) ->
    {Response, SASL} = sasl_new(Mech,
                                ?config(user, Config),
                                ?config(server, Config),
                                ?config(password, Config)),
    send(Config, #sasl_auth{mechanism = Mech, text = Response}),
    wait_auth_SASL_result(set_opt(sasl, SASL, Config)).

wait_auth_SASL_result(Config) ->
    case recv() of
        #sasl_success{} ->
            ejabberd_socket:reset_stream(?config(socket, Config)),
            send_text(Config,
                      io_lib:format(?STREAM_HEADER,
                                    [?config(server, Config)])),
            {xmlstreamstart, <<"stream:stream">>, Attrs} = recv(),
            <<"jabber:client">> = xml:get_attr_s(<<"xmlns">>, Attrs),
            <<"1.0">> = xml:get_attr_s(<<"version">>, Attrs),
            #stream_features{} = recv(),
            Config;
        #sasl_challenge{text = ClientIn} ->
            {Response, SASL} = (?config(sasl, Config))(ClientIn),
            send(Config, #sasl_response{text = Response}),
            wait_auth_SASL_result(set_opt(sasl, SASL, Config));
        #sasl_failure{} ->
            ct:fail(sasl_auth_failed)
    end.

%%%===================================================================
%%% Aux functions
%%%===================================================================
re_register(Config) ->
    User = ?config(user, Config),
    Server = ?config(server, Config),
    Pass = ?config(password, Config),
    {atomic, ok} = ejabberd_auth:try_register(User, Server, Pass),
    ok.

recv() ->
    receive
        {'$gen_event', {xmlstreamelement, El}} ->
            Pkt = xmpp_codec:decode(fix_ns(El)),
            ct:pal("recv: ~p ->~n~s", [El, xmpp_codec:pp(Pkt)]),
            Pkt;
        {'$gen_event', Event} ->
            Event
    end.

fix_ns(#xmlel{name = Tag, attrs = Attrs} = El)
  when Tag == <<"stream:features">>; Tag == <<"stream:error">> ->
    NewAttrs = [{<<"xmlns">>, <<"http://etherx.jabber.org/streams">>}
                |lists:keydelete(<<"xmlns">>, 1, Attrs)],
    El#xmlel{attrs = NewAttrs};
fix_ns(#xmlel{name = Tag, attrs = Attrs} = El)
  when Tag == <<"message">>; Tag == <<"iq">>; Tag == <<"presence">> ->
    NewAttrs = [{<<"xmlns">>, <<"jabber:client">>}
                |lists:keydelete(<<"xmlns">>, 1, Attrs)],
    El#xmlel{attrs = NewAttrs};
fix_ns(El) ->
    El.

send_text(Config, Text) ->
    ejabberd_socket:send(?config(socket, Config), Text).

send(State, Pkt) ->
    {NewID, NewPkt} = case Pkt of
                          #message{id = I} ->
                              ID = id(I),
                              {ID, Pkt#message{id = ID}};
                          #presence{id = I} ->
                              ID = id(I),
                              {ID, Pkt#presence{id = ID}};
                          #iq{id = I} ->
                              ID = id(I),
                              {ID, Pkt#iq{id = ID}};
                          _ ->
                              {undefined, Pkt}
                      end,
    El = xmpp_codec:encode(NewPkt),
    ct:pal("sent: ~p <-~n~s", [El, xmpp_codec:pp(NewPkt)]),
    ok = send_text(State, xml:element_to_binary(El)),
    NewID.

send_recv(State, IQ) ->
    ID = send(State, IQ),
    #iq{id = ID} = recv().

sasl_new(<<"PLAIN">>, User, Server, Password) ->
    {<<User/binary, $@, Server/binary, 0, User/binary, 0, Password/binary>>,
     fun (_) -> {error, <<"Invalid SASL challenge">>} end};
sasl_new(<<"DIGEST-MD5">>, User, Server, Password) ->
    {<<"">>,
     fun (ServerIn) ->
	     case cyrsasl_digest:parse(ServerIn) of
	       bad -> {error, <<"Invalid SASL challenge">>};
	       KeyVals ->
		   Nonce = xml:get_attr_s(<<"nonce">>, KeyVals),
		   CNonce = id(),
		   DigestURI = <<"xmpp/", Server/binary>>,
		   Realm = Server,
		   NC = <<"00000001">>,
		   QOP = <<"auth">>,
		   AuthzId = <<"">>,
		   MyResponse = response(User, Password, Nonce, AuthzId,
					 Realm, CNonce, DigestURI, NC, QOP,
					 <<"AUTHENTICATE">>),
		   ServerResponse = response(User, Password, Nonce,
					     AuthzId, Realm, CNonce, DigestURI,
					     NC, QOP, <<"">>),
		   Resp = <<"username=\"", User/binary, "\",realm=\"",
			    Realm/binary, "\",nonce=\"", Nonce/binary,
			    "\",cnonce=\"", CNonce/binary, "\",nc=", NC/binary,
			    ",qop=", QOP/binary, ",digest-uri=\"",
			    DigestURI/binary, "\",response=\"",
			    MyResponse/binary, "\"">>,
		   {Resp,
		    fun (ServerIn2) ->
			    case cyrsasl_digest:parse(ServerIn2) of
			      bad -> {error, <<"Invalid SASL challenge">>};
			      KeyVals2 ->
				  RspAuth = xml:get_attr_s(<<"rspauth">>,
							   KeyVals2),
				  if RspAuth == ServerResponse ->
					 {<<"">>,
					  fun (_) ->
						  {error,
						   <<"Invalid SASL challenge">>}
					  end};
				     true ->
					 {error, <<"Invalid SASL challenge">>}
				  end
			    end
		    end}
	     end
     end}.

hex(S) ->
    sha:to_hexlist(S).

response(User, Passwd, Nonce, AuthzId, Realm, CNonce,
	 DigestURI, NC, QOP, A2Prefix) ->
    A1 = case AuthzId of
	   <<"">> ->
	       <<((crypto:md5(<<User/binary, ":", Realm/binary, ":",
				Passwd/binary>>)))/binary,
		 ":", Nonce/binary, ":", CNonce/binary>>;
	   _ ->
	       <<((crypto:md5(<<User/binary, ":", Realm/binary, ":",
				Passwd/binary>>)))/binary,
		 ":", Nonce/binary, ":", CNonce/binary, ":",
		 AuthzId/binary>>
	 end,
    A2 = case QOP of
	   <<"auth">> ->
	       <<A2Prefix/binary, ":", DigestURI/binary>>;
	   _ ->
	       <<A2Prefix/binary, ":", DigestURI/binary,
		 ":00000000000000000000000000000000">>
	 end,
    T = <<(hex((crypto:md5(A1))))/binary, ":", Nonce/binary,
	  ":", NC/binary, ":", CNonce/binary, ":", QOP/binary,
	  ":", (hex((crypto:md5(A2))))/binary>>,
    hex((crypto:md5(T))).

my_jid(Config) ->
    jlib:make_jid(?config(user, Config),
                  ?config(server, Config),
                  ?config(resource, Config)).

server_jid(Config) ->
    jlib:make_jid(<<>>, ?config(server, Config), <<>>).

pubsub_jid(Config) ->
    Server = ?config(server, Config),
    jlib:make_jid(<<>>, <<"pubsub.", Server/binary>>, <<>>).

proxy_jid(Config) ->
    Server = ?config(server, Config),
    jlib:make_jid(<<>>, <<"proxy.", Server/binary>>, <<>>).

muc_jid(Config) ->
    Server = ?config(server, Config),
    jlib:make_jid(<<>>, <<"conference.", Server/binary>>, <<>>).

muc_room_jid(Config) ->
    Server = ?config(server, Config),
    jlib:make_jid(<<"test">>, <<"conference.", Server/binary>>, <<>>).

id() ->
    id(undefined).

id(undefined) ->
    randoms:get_string();
id(ID) ->
    ID.

is_feature_advertised(Config, Feature) ->
    is_feature_advertised(Config, Feature, server_jid(Config)).

is_feature_advertised(Config, Feature, To) ->
    ID = send(Config, #iq{type = get, sub_els = [#disco_info{}], to = To}),
    #iq{type = result, id = ID,
        sub_els = [#disco_info{feature = Features}]} = recv(),
    lists:member(Feature, Features).

bookmark_conference() ->
    #bookmark_conference{name = <<"Some name">>,
                         autojoin = true,
                         jid = jlib:make_jid(
                                 <<"some">>,
                                 <<"some.conference.org">>,
                                 <<>>)}.

set_opt(Opt, Val, Config) ->
    [{Opt, Val}|lists:keydelete(Opt, 1, Config)].

wait_for_master(Config) ->
    put_event(Config, slave_ready),
    master_ready = get_event(Config).

wait_for_slave(Config) ->
    put_event(Config, master_ready),
    slave_ready = get_event(Config).

make_iq_result(#iq{from = From} = IQ) ->
    IQ#iq{type = result, to = From, from = undefined, sub_els = []}.

socks5_connect(#streamhost{host = Host, port = Port},
               {SID, JID1, JID2}) ->
    Hash = sha:sha([SID, jlib:jid_to_string(JID1), jlib:jid_to_string(JID2)]),
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
%%% Clients puts and gets events via this relay.
%%%===================================================================
start_event_relay() ->
    spawn(fun event_relay/0).

stop_event_relay(Config) ->
    Pid = ?config(event_relay, Config),
    exit(Pid, normal).

event_relay() ->
    event_relay([], []).

event_relay(Events, Subscribers) ->
    receive
        {subscribe, From} ->
            From ! {ok, self()},
            lists:foreach(
              fun(Event) -> From ! {event, Event, self()}
              end, Events),
            event_relay(Events, [From|Subscribers]);
        {put, Event, From} ->
            From ! {ok, self()},
            lists:foreach(
              fun(Pid) when Pid /= From ->
                      Pid ! {event, Event, self()};
                 (_) ->
                      ok
              end, Subscribers),
            event_relay([Event|Events], Subscribers)
    end.

subscribe_to_events(Config) ->
    Relay = ?config(event_relay, Config),
    Relay ! {subscribe, self()},
    receive
        {ok, Relay} ->
            ok
    end.

put_event(Config, Event) ->
    Relay = ?config(event_relay, Config),
    Relay ! {put, Event, self()},
    receive
        {ok, Relay} ->
            ok
    end.

get_event(Config) ->
    Relay = ?config(event_relay, Config),
    receive
        {event, Event, Relay} ->
            Event
    end.

insert(Val, N, Tuple) ->
    L = tuple_to_list(Tuple),
    {H, T} = lists:split(N-1, L),
    list_to_tuple(H ++ [Val|T]).
