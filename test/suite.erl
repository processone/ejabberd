%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 27 Jun 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

-module(suite).

%% API
-compile(export_all).

-include("suite.hrl").
-include_lib("kernel/include/file.hrl").
-include("mod_roster.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init_config(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    [_, _|Tail] = lists:reverse(filename:split(DataDir)),
    BaseDir = filename:join(lists:reverse(Tail)),
    ConfigPathTpl = filename:join([DataDir, "ejabberd.yml"]),
    LogPath = filename:join([PrivDir, "ejabberd.log"]),
    SASLPath = filename:join([PrivDir, "sasl.log"]),
    MnesiaDir = filename:join([PrivDir, "mnesia"]),
    CertFile = filename:join([DataDir, "cert.pem"]),
    SelfSignedCertFile = filename:join([DataDir, "self-signed-cert.pem"]),
    CAFile = filename:join([DataDir, "ca.pem"]),
    {ok, CWD} = file:get_cwd(),
    {ok, _} = file:copy(CertFile, filename:join([CWD, "cert.pem"])),
    {ok, _} = file:copy(SelfSignedCertFile,
			filename:join([CWD, "self-signed-cert.pem"])),
    {ok, _} = file:copy(CAFile, filename:join([CWD, "ca.pem"])),
    {ok, CfgContentTpl} = file:read_file(ConfigPathTpl),
    Password = <<"password!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>,
    CfgContent = process_config_tpl(CfgContentTpl, [
                                                    {c2s_port, 5222},
                                                    {loglevel, 4},
                                                    {s2s_port, 5269},
						    {component_port, 5270},
                                                    {web_port, 5280},
						    {password, Password},
                                                    {mysql_server, <<"localhost">>},
                                                    {mysql_port, 3306},
                                                    {mysql_db, <<"ejabberd_test">>},
                                                    {mysql_user, <<"ejabberd_test">>},
                                                    {mysql_pass, <<"ejabberd_test">>},
                                                    {pgsql_server, <<"localhost">>},
                                                    {pgsql_port, 5432},
                                                    {pgsql_db, <<"ejabberd_test">>},
                                                    {pgsql_user, <<"ejabberd_test">>},
                                                    {pgsql_pass, <<"ejabberd_test">>}
                                                   ]),
    ConfigPath = filename:join([CWD, "ejabberd.yml"]),
    ok = file:write_file(ConfigPath, CfgContent),
    setup_ejabberd_lib_path(Config),
    ok = application:load(sasl),
    ok = application:load(mnesia),
    ok = application:load(ejabberd),
    application:set_env(ejabberd, config, ConfigPath),
    application:set_env(ejabberd, log_path, LogPath),
    application:set_env(sasl, sasl_error_logger, {file, SASLPath}),
    application:set_env(mnesia, dir, MnesiaDir),
    [{server_port, ct:get_config(c2s_port, 5222)},
     {server_host, "localhost"},
     {component_port, ct:get_config(component_port, 5270)},
     {s2s_port, ct:get_config(s2s_port, 5269)},
     {server, ?COMMON_VHOST},
     {user, <<"test_single!#$%^*()`~+-;_=[]{}|\\">>},
     {nick, <<"nick!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {master_nick, <<"master_nick!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {slave_nick, <<"slave_nick!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {room_subject, <<"hello, world!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {certfile, CertFile},
     {persistent_room, true},
     {anonymous, false},
     {type, client},
     {xmlns, ?NS_CLIENT},
     {ns_stream, ?NS_STREAM},
     {stream_version, {1, 0}},
     {stream_id, <<"">>},
     {stream_from, <<"">>},
     {db_xmlns, <<"">>},
     {mechs, []},
     {rosterver, false},
     {lang, <<"en">>},
     {base_dir, BaseDir},
     {socket, undefined},
     {pubsub_node, <<"node!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {pubsub_node_title, <<"title!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {resource, <<"resource!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {master_resource, <<"master_resource!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {slave_resource, <<"slave_resource!@#$%^&*()'\"`~<>+-/;:_=[]{}|\\">>},
     {password, Password},
     {backends, get_config_backends()}
     |Config].

find_top_dir(Dir) ->
    case file:read_file_info(filename:join([Dir, ebin])) of
	{ok, #file_info{type = directory}} ->
	    Dir;
	_ ->
	    find_top_dir(filename:dirname(Dir))
    end.

setup_ejabberd_lib_path(Config) ->
    case code:lib_dir(ejabberd) of
	{error, _} ->
	    DataDir = proplists:get_value(data_dir, Config),
	    {ok, CWD} = file:get_cwd(),
	    NewEjPath = filename:join([CWD, "ejabberd-0.0.1"]),
	    TopDir = find_top_dir(DataDir),
	    ok = file:make_symlink(TopDir, NewEjPath),
	    code:replace_path(ejabberd, NewEjPath);
	_ ->
	    ok
    end.

%% Read environment variable CT_DB=riak,mysql to limit the backends to test.
%% You can thus limit the backend you want to test with:
%%  CT_BACKENDS=riak,mysql rebar ct suites=ejabberd
get_config_backends() ->
    case os:getenv("CT_BACKENDS") of
        false  -> all;
        String ->
            Backends0 = string:tokens(String, ","),
            lists:map(fun(Backend) -> string:strip(Backend, both, $ ) end, Backends0)
    end.

process_config_tpl(Content, []) ->
    Content;
process_config_tpl(Content, [{Name, DefaultValue} | Rest]) ->
    Val = case ct:get_config(Name, DefaultValue) of
              V1 when is_integer(V1) ->
                  integer_to_binary(V1);
              V2 when is_atom(V2) ->
                  atom_to_binary(V2, latin1);
              V3 ->
                  V3
          end,
    NewContent = binary:replace(Content,
				<<"@@",(atom_to_binary(Name,latin1))/binary, "@@">>,
				Val, [global]),
    process_config_tpl(NewContent, Rest).

stream_header(Config) ->
    To = case ?config(server, Config) of
	     <<"">> -> undefined;
	     Server -> jid:make(Server)
	 end,
    From = case ?config(stream_from, Config) of
	       <<"">> -> undefined;
	       Frm -> jid:make(Frm)
	   end,
    #stream_start{to = To,
		  from = From,
		  lang = ?config(lang, Config),
		  version = ?config(stream_version, Config),
		  xmlns = ?config(xmlns, Config),
		  db_xmlns = ?config(db_xmlns, Config),
		  stream_xmlns = ?config(ns_stream, Config)}.

connect(Config) ->
    NewConfig = init_stream(Config),
    case ?config(type, NewConfig) of
	client -> process_stream_features(NewConfig);
	server -> process_stream_features(NewConfig);
	component -> NewConfig
    end.

tcp_connect(Config) ->
    case ?config(socket, Config) of
	undefined ->
	    Owner = self(),
	    NS = case ?config(type, Config) of
		     client -> ?NS_CLIENT;
		     server -> ?NS_SERVER;
		     component -> ?NS_COMPONENT
		 end,
	    ReceiverPid = spawn(fun() -> receiver(NS, Owner) end),
	    {ok, Sock} = ejabberd_socket:connect(
			   ?config(server_host, Config),
			   ?config(server_port, Config),
			   [binary, {packet, 0}, {active, false}],
			   infinity, ReceiverPid),
	    set_opt(socket, Sock, Config);
	_ ->
	    Config
    end.

init_stream(Config) ->
    Version = ?config(stream_version, Config),
    NewConfig = tcp_connect(Config),
    send(NewConfig, stream_header(NewConfig)),
    XMLNS = case ?config(type, Config) of
		client -> ?NS_CLIENT;
		component -> ?NS_COMPONENT;
		server -> ?NS_SERVER
	    end,
    receive
	#stream_start{id = ID, xmlns = XMLNS, version = Version} ->
	    set_opt(stream_id, ID, NewConfig)
    end.

process_stream_features(Config) ->
    receive
	#stream_features{sub_els = Fs} ->
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
		 (#legacy_auth_feature{}, Acc) ->
		      set_opt(legacy_auth, true, Acc);
		 (#compression{methods = Ms}, Acc) ->
		      set_opt(compression, Ms, Acc);
		 (_, Acc) ->
		      Acc
	      end, set_opt(mechs, Mechs, Config), Fs)
    end.

disconnect(Config) ->
    ct:comment("Disconnecting"),
    Socket = ?config(socket, Config),
    try
	send_text(Config, ?STREAM_TRAILER)
    catch exit:normal ->
	    ok
    end,
    receive {xmlstreamend, <<"stream:stream">>} -> ok end,
    flush(Config),
    ejabberd_socket:close(Socket),
    ct:comment("Disconnected"),
    set_opt(socket, undefined, Config).

close_socket(Config) ->
    Socket = ?config(socket, Config),
    ejabberd_socket:close(Socket),
    Config.

starttls(Config) ->
    starttls(Config, false).

starttls(Config, ShouldFail) ->
    send(Config, #starttls{}),
    receive
	#starttls_proceed{} when ShouldFail ->
	    ct:fail(starttls_should_have_failed);
	#starttls_failure{} when ShouldFail ->
	    Config;
	#starttls_failure{} ->
	    ct:fail(starttls_failed);
	#starttls_proceed{} ->
	    {ok, TLSSocket} = ejabberd_socket:starttls(
				?config(socket, Config),
				[{certfile, ?config(certfile, Config)},
				 connect]),
	    set_opt(socket, TLSSocket, Config)
    end.

zlib(Config) ->
    send(Config, #compress{methods = [<<"zlib">>]}),
    receive #compressed{} -> ok end,
    {ok, ZlibSocket} = ejabberd_socket:compress(?config(socket, Config)),
    process_stream_features(init_stream(set_opt(socket, ZlibSocket, Config))).

auth(Config) ->
    auth(Config, false).

auth(Config, ShouldFail) ->
    Type = ?config(type, Config),
    IsAnonymous = ?config(anonymous, Config),
    Mechs = ?config(mechs, Config),
    HaveMD5 = lists:member(<<"DIGEST-MD5">>, Mechs),
    HavePLAIN = lists:member(<<"PLAIN">>, Mechs),
    HaveExternal = lists:member(<<"EXTERNAL">>, Mechs),
    HaveAnonymous = lists:member(<<"ANONYMOUS">>, Mechs),
    if HaveAnonymous and IsAnonymous ->
	    auth_SASL(<<"ANONYMOUS">>, Config, ShouldFail);
       HavePLAIN ->
            auth_SASL(<<"PLAIN">>, Config, ShouldFail);
       HaveMD5 ->
            auth_SASL(<<"DIGEST-MD5">>, Config, ShouldFail);
       HaveExternal ->
	    auth_SASL(<<"EXTERNAL">>, Config, ShouldFail);
       Type == client ->
	    auth_legacy(Config, false, ShouldFail);
       Type == component ->
	    auth_component(Config, ShouldFail);
       true ->
	    ct:fail(no_known_sasl_mechanism_available)
    end.

bind(Config) ->
    U = ?config(user, Config),
    S = ?config(server, Config),
    R = ?config(resource, Config),
    case ?config(type, Config) of
	client ->
	    #iq{type = result, sub_els = [#bind{jid = JID}]} =
		send_recv(
		  Config, #iq{type = set, sub_els = [#bind{resource = R}]}),
	    case ?config(anonymous, Config) of
		false ->
		    {U, S, R} = jid:tolower(JID),
		    Config;
		true ->
		    {User, S, Resource} = jid:tolower(JID),
		    set_opt(user, User, set_opt(resource, Resource, Config))
	    end;
	component ->
	    Config
    end.

open_session(Config) ->
    open_session(Config, false).

open_session(Config, Force) ->
    if Force ->
	    #iq{type = result, sub_els = []} =
		send_recv(Config, #iq{type = set, sub_els = [#xmpp_session{}]});
       true ->
	    ok
    end,
    Config.

auth_legacy(Config, IsDigest) ->
    auth_legacy(Config, IsDigest, false).

auth_legacy(Config, IsDigest, ShouldFail) ->
    ServerJID = server_jid(Config),
    U = ?config(user, Config),
    R = ?config(resource, Config),
    P = ?config(password, Config),
    #iq{type = result,
	from = ServerJID,
	sub_els = [#legacy_auth{username = <<"">>,
				password = <<"">>,
				resource = <<"">>} = Auth]} =
	send_recv(Config,
		  #iq{to = ServerJID, type = get,
		      sub_els = [#legacy_auth{}]}),
    Res = case Auth#legacy_auth.digest of
	      <<"">> when IsDigest ->
		  StreamID = ?config(stream_id, Config),
		  D = p1_sha:sha(<<StreamID/binary, P/binary>>),
		  send_recv(Config, #iq{to = ServerJID, type = set,
					sub_els = [#legacy_auth{username = U,
								resource = R,
								digest = D}]});
	      _ when not IsDigest ->
		  send_recv(Config, #iq{to = ServerJID, type = set,
					sub_els = [#legacy_auth{username = U,
								resource = R,
								password = P}]})
	  end,
    case Res of
	#iq{from = ServerJID, type = result, sub_els = []} ->
	    if ShouldFail ->
		    ct:fail(legacy_auth_should_have_failed);
	       true ->
		    Config
	    end;
	#iq{from = ServerJID, type = error} ->
	    if ShouldFail ->
		    Config;
	       true ->
		    ct:fail(legacy_auth_failed)
	    end
    end.

auth_component(Config, ShouldFail) ->
    StreamID = ?config(stream_id, Config),
    Password = ?config(password, Config),
    Digest = p1_sha:sha(<<StreamID/binary, Password/binary>>),
    send(Config, #handshake{data = Digest}),
    receive
	#handshake{} when ShouldFail ->
	    ct:fail(component_auth_should_have_failed);
	#handshake{} ->
	    Config;
	#stream_error{reason = 'not-authorized'} when ShouldFail ->
	    Config;
	#stream_error{reason = 'not-authorized'} ->
	    ct:fail(component_auth_failed)
    end.

auth_SASL(Mech, Config) ->
    auth_SASL(Mech, Config, false).

auth_SASL(Mech, Config, ShouldFail) ->
    Creds = {?config(user, Config),
	     ?config(server, Config),
	     ?config(password, Config)},
    auth_SASL(Mech, Config, ShouldFail, Creds).

auth_SASL(Mech, Config, ShouldFail, Creds) ->
    {Response, SASL} = sasl_new(Mech, Creds),
    send(Config, #sasl_auth{mechanism = Mech, text = Response}),
    wait_auth_SASL_result(set_opt(sasl, SASL, Config), ShouldFail).

wait_auth_SASL_result(Config, ShouldFail) ->
    receive
	#sasl_success{} when ShouldFail ->
	    ct:fail(sasl_auth_should_have_failed);
        #sasl_success{} ->
            ejabberd_socket:reset_stream(?config(socket, Config)),
            send(Config, stream_header(Config)),
	    Type = ?config(type, Config),
	    NS = if Type == client -> ?NS_CLIENT;
		    Type == server -> ?NS_SERVER
		 end,
	    receive #stream_start{xmlns = NS, version = {1,0}} -> ok end,
            receive #stream_features{sub_els = Fs} ->
		    if Type == client ->
			    #xmpp_session{optional = true} =
				lists:keyfind(xmpp_session, 1, Fs);
		       true ->
			    ok
		    end,
		    lists:foldl(
		      fun(#feature_sm{}, ConfigAcc) ->
			      set_opt(sm, true, ConfigAcc);
			 (#feature_csi{}, ConfigAcc) ->
			      set_opt(csi, true, ConfigAcc);
			 (#rosterver_feature{}, ConfigAcc) ->
			      set_opt(rosterver, true, ConfigAcc);
			 (_, ConfigAcc) ->
			      ConfigAcc
		      end, Config, Fs)
	    end;
        #sasl_challenge{text = ClientIn} ->
            {Response, SASL} = (?config(sasl, Config))(ClientIn),
            send(Config, #sasl_response{text = Response}),
            wait_auth_SASL_result(set_opt(sasl, SASL, Config), ShouldFail);
	#sasl_failure{} when ShouldFail ->
	    Config;
        #sasl_failure{} ->
            ct:fail(sasl_auth_failed)
    end.

re_register(Config) ->
    User = ?config(user, Config),
    Server = ?config(server, Config),
    Pass = ?config(password, Config),
    {atomic, ok} = ejabberd_auth:try_register(User, Server, Pass),
    ok.

match_failure(Received, [Match]) when is_list(Match)->
    ct:fail("Received input:~n~n~p~n~ndon't match expected patterns:~n~n~s", [Received, Match]);
match_failure(Received, Matches) ->
    ct:fail("Received input:~n~n~p~n~ndon't match expected patterns:~n~n~p", [Received, Matches]).

recv(_Config) ->
    receive
	{fail, El, Why} ->
	    ct:fail("recv failed: ~p->~n~s",
		    [El, xmpp:format_error(Why)]);
	Event ->
	    Event
    end.

recv_iq(_Config) ->
    receive #iq{} = IQ -> IQ end.

recv_presence(_Config) ->
    receive #presence{} = Pres -> Pres end.

recv_message(_Config) ->
    receive #message{} = Msg -> Msg end.

decode_stream_element(NS, El) ->
    decode(El, NS, []).

format_element(El) ->
    case erlang:function_exported(ct, log, 5) of
	true -> ejabberd_web_admin:pretty_print_xml(El);
	false -> io_lib:format("~p~n", [El])
    end.

decode(El, NS, Opts) ->
    try
	Pkt = xmpp:decode(El, NS, Opts),
	ct:pal("RECV:~n~s~n~s",
	       [format_element(El), xmpp:pp(Pkt)]),
	Pkt
    catch _:{xmpp_codec, Why} ->
	    ct:pal("recv failed: ~p->~n~s",
		   [El, xmpp:format_error(Why)]),
	    erlang:error({xmpp_codec, Why})
    end.

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
    El = xmpp:encode(NewPkt),
    ct:pal("SENT:~n~s~n~s",
	   [format_element(El), xmpp:pp(NewPkt)]),
    Data = case NewPkt of
	       #stream_start{} -> fxml:element_to_header(El);
	       _ -> fxml:element_to_binary(El)
	   end,
    ok = send_text(State, Data),
    NewID.

send_recv(State, #message{} = Msg) ->
    ID = send(State, Msg),
    receive #message{id = ID} = Result -> Result end;
send_recv(State, #presence{} = Pres) ->
    ID = send(State, Pres),
    receive #presence{id = ID} = Result -> Result end;
send_recv(State, #iq{} = IQ) ->
    ID = send(State, IQ),
    receive #iq{id = ID} = Result -> Result end.

sasl_new(<<"PLAIN">>, {User, Server, Password}) ->
    {<<User/binary, $@, Server/binary, 0, User/binary, 0, Password/binary>>,
     fun (_) -> {error, <<"Invalid SASL challenge">>} end};
sasl_new(<<"EXTERNAL">>, {User, Server, _Password}) ->
    {jid:encode(jid:make(User, Server)),
     fun(_) -> ct:fail(sasl_challenge_is_not_expected) end};
sasl_new(<<"ANONYMOUS">>, _) ->
    {<<"">>,
     fun(_) -> ct:fail(sasl_challenge_is_not_expected) end};
sasl_new(<<"DIGEST-MD5">>, {User, Server, Password}) ->
    {<<"">>,
     fun (ServerIn) ->
	     case cyrsasl_digest:parse(ServerIn) of
	       bad -> {error, <<"Invalid SASL challenge">>};
	       KeyVals ->
		   Nonce = fxml:get_attr_s(<<"nonce">>, KeyVals),
		   CNonce = id(),
                   Realm = proplists:get_value(<<"realm">>, KeyVals, Server),
		   DigestURI = <<"xmpp/", Realm/binary>>,
		   NC = <<"00000001">>,
		   QOP = <<"auth">>,
		   AuthzId = <<"">>,
		   MyResponse = response(User, Password, Nonce, AuthzId,
					 Realm, CNonce, DigestURI, NC, QOP,
					 <<"AUTHENTICATE">>),
                   SUser = << <<(case Char of
                                     $" -> <<"\\\"">>;
                                     $\\ -> <<"\\\\">>;
                                     _ -> <<Char>>
                                 end)/binary>> || <<Char>> <= User >>,
		   Resp = <<"username=\"", SUser/binary, "\",realm=\"",
			    Realm/binary, "\",nonce=\"", Nonce/binary,
			    "\",cnonce=\"", CNonce/binary, "\",nc=", NC/binary,
			    ",qop=", QOP/binary, ",digest-uri=\"",
			    DigestURI/binary, "\",response=\"",
			    MyResponse/binary, "\"">>,
		   {Resp,
		    fun (ServerIn2) ->
			    case cyrsasl_digest:parse(ServerIn2) of
			      bad -> {error, <<"Invalid SASL challenge">>};
			      _KeyVals2 ->
                                    {<<"">>,
                                     fun (_) ->
                                             {error,
                                              <<"Invalid SASL challenge">>}
                                     end}
			    end
		    end}
	     end
     end}.

hex(S) ->
    p1_sha:to_hexlist(S).

response(User, Passwd, Nonce, AuthzId, Realm, CNonce,
	 DigestURI, NC, QOP, A2Prefix) ->
    A1 = case AuthzId of
	   <<"">> ->
	       <<((erlang:md5(<<User/binary, ":", Realm/binary, ":",
				Passwd/binary>>)))/binary,
		 ":", Nonce/binary, ":", CNonce/binary>>;
	   _ ->
	       <<((erlang:md5(<<User/binary, ":", Realm/binary, ":",
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
    T = <<(hex((erlang:md5(A1))))/binary, ":", Nonce/binary,
	  ":", NC/binary, ":", CNonce/binary, ":", QOP/binary,
	  ":", (hex((erlang:md5(A2))))/binary>>,
    hex((erlang:md5(T))).

my_jid(Config) ->
    jid:make(?config(user, Config),
	     ?config(server, Config),
	     ?config(resource, Config)).

server_jid(Config) ->
    jid:make(<<>>, ?config(server, Config), <<>>).

pubsub_jid(Config) ->
    Server = ?config(server, Config),
    jid:make(<<>>, <<"pubsub.", Server/binary>>, <<>>).

proxy_jid(Config) ->
    Server = ?config(server, Config),
    jid:make(<<>>, <<"proxy.", Server/binary>>, <<>>).

muc_jid(Config) ->
    Server = ?config(server, Config),
    jid:make(<<>>, <<"conference.", Server/binary>>, <<>>).

muc_room_jid(Config) ->
    Server = ?config(server, Config),
    jid:make(<<"test">>, <<"conference.", Server/binary>>, <<>>).

my_muc_jid(Config) ->
    Nick = ?config(nick, Config),
    RoomJID = muc_room_jid(Config),
    jid:replace_resource(RoomJID, Nick).

peer_muc_jid(Config) ->
    PeerNick = ?config(peer_nick, Config),
    RoomJID = muc_room_jid(Config),
    jid:replace_resource(RoomJID, PeerNick).

alt_room_jid(Config) ->
    Server = ?config(server, Config),
    jid:make(<<"alt">>, <<"conference.", Server/binary>>, <<>>).

mix_jid(Config) ->
    Server = ?config(server, Config),
    jid:make(<<>>, <<"mix.", Server/binary>>, <<>>).

mix_room_jid(Config) ->
    Server = ?config(server, Config),
    jid:make(<<"test">>, <<"mix.", Server/binary>>, <<>>).

id() ->
    id(<<>>).

id(<<>>) ->
    randoms:get_string();
id(ID) ->
    ID.

get_features(Config) ->
    get_features(Config, server_jid(Config)).

get_features(Config, To) ->
    ct:comment("Getting features of ~s", [jid:encode(To)]),
    #iq{type = result, sub_els = [#disco_info{features = Features}]} =
        send_recv(Config, #iq{type = get, sub_els = [#disco_info{}], to = To}),
    Features.

is_feature_advertised(Config, Feature) ->
    is_feature_advertised(Config, Feature, server_jid(Config)).

is_feature_advertised(Config, Feature, To) ->
    Features = get_features(Config, To),
    lists:member(Feature, Features).

set_opt(Opt, Val, Config) ->
    [{Opt, Val}|lists:keydelete(Opt, 1, Config)].

wait_for_master(Config) ->
    put_event(Config, peer_ready),
    case get_event(Config) of
	peer_ready ->
	    ok;
	Other ->
	    suite:match_failure(Other, peer_ready)
    end.

wait_for_slave(Config) ->
    put_event(Config, peer_ready),
    case get_event(Config) of
	peer_ready ->
	    ok;
	Other ->
	    suite:match_failure(Other, peer_ready)
    end.

make_iq_result(#iq{from = From} = IQ) ->
    IQ#iq{type = result, to = From, from = undefined, sub_els = []}.

self_presence(Config, Type) ->
    MyJID = my_jid(Config),
    ct:comment("Sending self-presence"),
    #presence{type = Type, from = MyJID} =
	send_recv(Config, #presence{type = Type}).

set_roster(Config, Subscription, Groups) ->
    MyJID = my_jid(Config),
    {U, S, _} = jid:tolower(MyJID),
    PeerJID = ?config(peer, Config),
    PeerBareJID = jid:remove_resource(PeerJID),
    PeerLJID = jid:tolower(PeerBareJID),
    ct:comment("Adding ~s to roster with subscription '~s' in groups ~p",
	       [jid:encode(PeerBareJID), Subscription, Groups]),
    {atomic, _} = mod_roster:set_roster(#roster{usj = {U, S, PeerLJID},
						us = {U, S},
						jid = PeerLJID,
						subscription = Subscription,
						groups = Groups}),
    Config.

del_roster(Config) ->
    del_roster(Config, ?config(peer, Config)).

del_roster(Config, PeerJID) ->
    MyJID = my_jid(Config),
    {U, S, _} = jid:tolower(MyJID),
    PeerBareJID = jid:remove_resource(PeerJID),
    PeerLJID = jid:tolower(PeerBareJID),
    ct:comment("Removing ~s from roster", [jid:encode(PeerBareJID)]),
    {atomic, _} = mod_roster:del_roster(U, S, PeerLJID),
    Config.

get_roster(Config) ->
    {LUser, LServer, _} = jid:tolower(my_jid(Config)),
    mod_roster:get_roster(LUser, LServer).

receiver(NS, Owner) ->
    MRef = erlang:monitor(process, Owner),
    receiver(NS, Owner, MRef).

receiver(NS, Owner, MRef) ->
    receive
        {'$gen_event', {xmlstreamelement, El}} ->
	    Owner ! decode_stream_element(NS, El),
	    receiver(NS, Owner, MRef);
	{'$gen_event', {xmlstreamstart, Name, Attrs}} ->
	    Owner ! decode(#xmlel{name = Name, attrs = Attrs}, <<>>, []),
	    receiver(NS, Owner, MRef);
	{'$gen_event', Event} ->
            Owner ! Event,
	    receiver(NS, Owner, MRef);
	{'DOWN', MRef, process, Owner, _} ->
	    ok
    end.

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
	    erlang:monitor(process, From),
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
            event_relay([Event|Events], Subscribers);
	{'DOWN', _MRef, process, Pid, _Info} ->
	    case lists:member(Pid, Subscribers) of
		true ->
		    NewSubscribers = lists:delete(Pid, Subscribers),
		    lists:foreach(
		      fun(Subscriber) ->
			      Subscriber ! {event, peer_down, self()}
		      end, NewSubscribers),
		    event_relay(Events, NewSubscribers);
		false ->
		    event_relay(Events, Subscribers)
	    end
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

flush(Config) ->
    receive
	{event, peer_down, _} -> flush(Config);
	closed -> flush(Config);
	Msg -> ct:fail({unexpected_msg, Msg})
    after 0 ->
	    ok
    end.
