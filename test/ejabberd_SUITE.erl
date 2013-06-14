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
-include("xmpp_codec.hrl").

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns:stream='http://etherx.jabber.org/stream"
	  "s' xmlns='jabber:client' to='~s' version='1.0"
	  "'>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(PUBSUB(Node), <<(?NS_PUBSUB)/binary, "#", Node>>).

suite() ->
    [{timetrap,{seconds,30}}].

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
    [{server, <<"localhost">>},
     {port, 5222},
     {user, <<"test_suite">>},
     {password, <<"pass">>},
     {certfile, CertFile}
     |Config].

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(start_ejabberd, Config) ->
    Config;
init_per_testcase(TestCase, OrigConfig) ->
    Resource = list_to_binary(atom_to_list(TestCase)),
    Config = set_opt(resource, Resource, OrigConfig),
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
        _ ->
            open_session(bind(auth(connect(Config))))
    end.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

%%all() -> [start_ejabberd, test_zlib].

all() ->
    [start_ejabberd,
     test_connect,
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
     stop_ejabberd].

start_ejabberd(Config) ->
    ok = application:start(ejabberd),
    Config.

stop_ejabberd(Config) ->
    ok = application:stop(ejabberd),
    #stream_error{reason = 'system-shutdown'} = recv(),
    {xmlstreamend, <<"stream:stream">>} = recv(),
    Config.

test_connect(Config) ->
    disconnect(connect(Config)).

connect(Config) ->
    {ok, Sock} = ejabberd_socket:connect(
                   binary_to_list(?config(server, Config)),
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
            starttls(Config);
        _ ->
            {skipped, 'starttls_not_available'}
    end.

starttls(Config) ->
    _ = send(Config, #starttls{}),
    #starttls_proceed{} = recv(),
    TLSSocket = ejabberd_socket:starttls(
                  ?config(socket, Config),
                  [{certfile, ?config(certfile, Config)},
                   connect]),
    disconnect(init_stream(set_opt(socket, TLSSocket, Config))).

test_zlib(Config) ->
    case ?config(compression, Config) of
        [_|_] = Ms ->
            case lists:member(<<"zlib">>, Ms) of
                true ->
                    zlib(Config);
                false ->
                    {skipped, 'zlib_not_available'}
            end;
        _ ->
            {skipped, 'compression_not_available'}
    end.

zlib(Config) ->
    _ = send(Config, #compress{methods = [<<"zlib">>]}),
    #compressed{} = recv(),
    ZlibSocket = ejabberd_socket:compress(?config(socket, Config)),
    disconnect(init_stream(set_opt(socket, ZlibSocket, Config))).

test_register(Config) ->
    case ?config(register, Config) of
        true ->
            register(Config);
        _ ->
            {skipped, 'registration_not_available'}
    end.

register(Config) ->
    I1 = send(Config,
              #iq{type = get, to = server_jid(Config),
                  sub_els = [#register{}]}),
    #iq{type = result, id = I1,
        sub_els = [#register{username = none,
                             password = none}]} = recv(),
    I2 = send(Config,
              #iq{type = set,
                  sub_els = [#register{username = ?config(user, Config),
                                       password = ?config(password, Config)}]}),
    %% BUG: we should receive empty sub_els
    %% TODO: fix in ejabberd
    %% #iq{type = result, id = I2, sub_els = []} = recv(),
    #iq{type = result, id = I2, sub_els = [#register{}]} = recv(),
    disconnect(Config).

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
    ID = send(Config,
              #iq{type = set,
                  sub_els = [#bind{resource = ?config(resource, Config)}]}),
    #iq{type = result, id = ID, sub_els = [#bind{}]} = recv(),
    Config.

test_open_session(Config) ->
    disconnect(open_session(Config)).

open_session(Config) ->
    ID = send(Config, #iq{type = set, sub_els = [#session{}]}),
    #iq{type = result, id = ID, sub_els = SubEls} = recv(),
    case SubEls of
        [] ->
            ok;
        [#session{}] ->
            %% BUG: we should not receive this!
            %% TODO: should be fixed in ejabberd
            ok
    end,
    Config.

roster_get(Config) ->
    ID = send(Config, #iq{type = get, sub_els = [#roster{}]}),
    #iq{type = result, id = ID,
          sub_els = [#roster{item = []}]} = recv(),
    disconnect(Config).

presence_broadcast(Config) ->
    send(Config, #presence{}),
    JID = my_jid(Config),
    %% We receive the welcome message first
    #message{type = normal} = recv(),
    %% Then we receive back our presence
    #presence{from = JID, to = JID} = recv(),
    disconnect(Config).

ping(Config) ->
    true = is_feature_advertised(Config, ?NS_PING),
    ID = send(Config,
              #iq{type = get, sub_els = [#ping{}], to = server_jid(Config)}),
    #iq{type = result, id = ID, sub_els = []} = recv(),
    disconnect(Config).

version(Config) ->
    true = is_feature_advertised(Config, ?NS_VERSION),
    ID = send(Config, #iq{type = get, sub_els = [#version{}],
                          to = server_jid(Config)}),
    #iq{type = result, id = ID, sub_els = [#version{}]} = recv(),
    disconnect(Config).

time(Config) ->
    true = is_feature_advertised(Config, ?NS_TIME),
    ID = send(Config, #iq{type = get, sub_els = [#time{}],
                          to = server_jid(Config)}),
    #iq{type = result, id = ID, sub_els = [#time{}]} = recv(),
    disconnect(Config).

disco(Config) ->
    true = is_feature_advertised(Config, ?NS_DISCO_INFO),
    true = is_feature_advertised(Config, ?NS_DISCO_ITEMS),
    I1 = send(Config, #iq{type = get, sub_els = [#disco_items{}],
                          to = server_jid(Config)}),
    #iq{type = result, id = I1, sub_els = [#disco_items{items = Items}]} = recv(),
    lists:foreach(
      fun(#disco_item{jid = JID, node = Node}) ->
              I = send(Config,
                       #iq{type = get, to = JID,
                           sub_els = [#disco_info{node = Node}]}),
              #iq{type = result, id = I, sub_els = _} = recv()
      end, Items),
    disconnect(Config).

private(Config) ->
    I1 = send(Config, #iq{type = get, sub_els = [#private{}],
                          to = server_jid(Config)}),
    #iq{type = error, id = I1} = recv(),
    Conference = #bookmark_conference{name = <<"Some name">>,
                                      autojoin = true,
                                      jid = jlib:make_jid(
                                              <<"some">>,
                                              <<"some.conference.org">>,
                                              <<>>)},
    Storage = #bookmark_storage{conference = [Conference]},
    I2 = send(Config, #iq{type = set,
                          sub_els = [#private{sub_els = [Storage]}]}),
    #iq{type = result, id = I2, sub_els = []} = recv(),
    I3 = send(Config,
              #iq{type = get,
                  sub_els = [#private{sub_els = [#bookmark_storage{}]}]}),
    #iq{type = result, id = I3,
        sub_els = [#private{sub_els = [Storage]}]} = recv(),
    disconnect(Config).

last(Config) ->
    true = is_feature_advertised(Config, ?NS_LAST),
    ID = send(Config, #iq{type = get, sub_els = [#last{}],
                          to = server_jid(Config)}),
    #iq{type = result, id = ID, sub_els = [#last{}]} = recv(),
    disconnect(Config).

privacy(Config) ->
    %% BUG: the feature MUST be advertised via disco#info:
    %%      http://xmpp.org/extensions/xep-0016.html#disco
    %% It seems like this bug exists because Privacy Lists
    %% were implemented according to the old RFC where support
    %% needn't be advertised via service discovery.
    %% TODO: fix in ejabberd
    %% true = is_feature_advertised(Config, ?NS_PRIVACY),
    I1 = send(Config, #iq{type = get, sub_els = [#privacy{}]}),
    #iq{type = result, id = I1, sub_els = [#privacy{}]} = recv(),
    JID = <<"tybalt@example.com">>,
    I2 = send(Config,
              #iq{type = set,
                  sub_els = [#privacy{
                                lists = [#privacy_list{
                                            name = <<"public">>,
                                            items =
                                                [#privacy_item{
                                                    type = jid,
                                                    order = 3,
                                                    action = deny,
                                                    stanza = 'presence-in',
                                                    value = JID}]}]}]}),
    #iq{type = result, id = I2, sub_els = []} = recv(),
    _Push1 = #iq{type = set, id = PushI1,
                   sub_els = [#privacy{
                                 lists = [#privacy_list{
                                             name = <<"public">>}]}]} = recv(),
    %% BUG: ejabberd replies on this result
    %% TODO: this should be fixed in ejabberd
    %% _ = send(Config, Push1#iq{type = result, sub_els = []}),
    I3 = send(Config, #iq{type = set,
                          sub_els = [#privacy{active = <<"public">>}]}),
    #iq{type = result, id = I3, sub_els = []} = recv(),
    I4 = send(Config, #iq{type = set,
                          sub_els = [#privacy{default = <<"public">>}]}),
    #iq{type = result, id = I4, sub_els = []} = recv(),
    I5 = send(Config, #iq{type = get, sub_els = [#privacy{}]}),
    #iq{type = result, id = I5,
        sub_els = [#privacy{default = <<"public">>,
                            active = <<"public">>,
                            lists = [#privacy_list{name = <<"public">>}]}]} = recv(),
    I6 = send(Config,
              #iq{type = set, sub_els = [#privacy{default = none}]}),
    #iq{type = result, id = I6, sub_els = []} = recv(),
    I7 = send(Config, #iq{type = set, sub_els = [#privacy{active = none}]}),
    #iq{type = result, id = I7, sub_els = []} = recv(),
    I8 = send(Config, #iq{type = set,
                          sub_els = [#privacy{
                                        lists =
                                            [#privacy_list{
                                                name = <<"public">>}]}]}),
    #iq{type = result, id = I8, sub_els = []} = recv(),
    %% BUG: We should receive this:
    %% _Push2 = #iq{type = set, id = PushI2, sub_els = []} = recv(),
    %% TODO: this should be fixed in ejabberd
    _Push2 = #iq{type = set, id = PushI2,
                   sub_els = [#privacy{
                                 lists = [#privacy_list{
                                             name = <<"public">>}]}]} = recv(),
    disconnect(Config).

blocking(Config) ->
    true = is_feature_advertised(Config, ?NS_BLOCKING),
    JID = jlib:make_jid(<<"romeo">>, <<"montague.net">>, <<>>),
    I1 = send(Config, #iq{type = get, sub_els = [#block_list{}]}),
    #iq{type = result, id = I1, sub_els = [#block_list{}]} = recv(),
    I2 = send(Config, #iq{type = set,
                          sub_els = [#block{items = [JID]}]}),
    #iq{type = result, id = I2, sub_els = []} = recv(),
    #iq{type = set, id = _,
          sub_els = [#privacy{lists = [#privacy_list{}]}]} = recv(),
    #iq{type = set, id = _,
          sub_els = [#block{items = [JID]}]} = recv(),
    I3 = send(Config, #iq{type = set,
                          sub_els = [#unblock{items = [JID]}]}),
    #iq{type = result, id = I3, sub_els = []} = recv(),
    #iq{type = set, id = _,
        sub_els = [#privacy{lists = [#privacy_list{}]}]} = recv(),
    #iq{type = set, id = _,
        sub_els = [#unblock{items = [JID]}]} = recv(),
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
    I1 = send(Config, #iq{type = set, sub_els = [VCard]}),
    #iq{type = result, id = I1, sub_els = []} = recv(),
    I2 = send(Config, #iq{type = get, sub_els = [#vcard{}]}),
    %% TODO: check if VCard == VCard1.
    #iq{type = result, id = I2, sub_els = [_VCard1]} = recv(),
    disconnect(Config).

stats(Config) ->
    ServerJID = server_jid(Config),
    ID = send(Config, #iq{type = get, sub_els = [#stats{}],
                          to = server_jid(Config)}),
    #iq{type = result, id = ID, sub_els = [#stats{stat = Stats}]} = recv(),
    lists:foreach(
      fun(#stat{name = Name} = Stat) ->
              I = send(Config, #iq{type = get,
                                   sub_els = [#stats{stat = [Stat]}],
                                   to = server_jid(Config)}),
              #iq{type = result, id = I, sub_els = [_|_]} = recv()
      end, Stats),
    disconnect(Config).

pubsub(Config) ->
    true = is_feature_advertised(Config, ?NS_PUBSUB),
    %% Publish <presence/> element within node "presence"
    ItemID = randoms:get_string(),
    Node = <<"presence">>,
    Item = #pubsub_item{id = ItemID, sub_els = [#presence{}]},
    I1 = send(Config,
              #iq{type = set, to = pubsub_jid(Config),
                  sub_els = [#pubsub{publish = {Node, [Item]}}]}),
    #iq{type = result, id = I1,
        sub_els = [#pubsub{publish = {<<"presence">>,
                                      [#pubsub_item{id = ItemID}]}}]} = recv(),
    %% Subscribe to node "presence"
    I2 = send(Config,
              #iq{type = set, to = pubsub_jid(Config),
                  sub_els = [#pubsub{subscribe = {Node, my_jid(Config)}}]}),
    #message{sub_els = [#pubsub_event{}, #delay{}]} = recv(),
    #iq{type = result, id = I2} = recv(),
    %% Get subscriptions
    true = is_feature_advertised(Config, ?PUBSUB("retrieve-subscriptions")),
    I3 = send(Config, #iq{type = get, to = pubsub_jid(Config),
                          sub_els = [#pubsub{subscriptions = {none, []}}]}),
    #iq{type = result, id = I3,
        sub_els =
            [#pubsub{subscriptions =
                         {none, [#pubsub_subscription{node = Node}]}}]} = recv(),
    %% Get affiliations
    true = is_feature_advertised(Config, ?PUBSUB("retrieve-affiliations")),
    I4 = send(Config, #iq{type = get, to = pubsub_jid(Config),
                          sub_els = [#pubsub{affiliations = []}]}),
    #iq{type = result, id = I4,
        sub_els = [#pubsub{
                      affiliations =
                          [#pubsub_affiliation{node = Node, type = owner}]}]}
        = recv(),
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
            ct:log("recv: ~p", [El]),
            xmpp_codec:decode(fix_ns(El));
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
    ct:log("sent: ~p", [El]),
    ok = send_text(State, xml:element_to_binary(El)),
    NewID.

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

id() ->
    id(undefined).

id(undefined) ->
    randoms:get_string();
id(ID) ->
    ID.

is_feature_advertised(Config, Feature) ->
    ID = send(Config, #iq{type = get, sub_els = [#disco_info{}],
                          to = server_jid(Config)}),
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
