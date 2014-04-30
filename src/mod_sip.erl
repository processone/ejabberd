%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_sip).

-behaviour(gen_mod).
-behaviour(esip).

%% API
-export([start/2, stop/1, prepare_request/1, make_response/2,
	 add_certfile/2, add_via/3]).

%% esip_callbacks
-export([data_in/2, data_out/2, message_in/2, message_out/2,
	 request/2, request/3, response/2, locate/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("esip.hrl").

-record(sip_session, {us = {<<"">>, <<"">>} :: {binary(), binary()},
		      socket = #sip_socket{},
		      timestamp = now() :: erlang:timestamp(),
		      tref = make_ref() :: reference(),
		      expires = 0 :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================
start(_Host, _Opts) ->
    ejabberd:start_app(esip),
    esip:set_config_value(max_server_transactions, 10000),
    esip:set_config_value(max_client_transactions, 10000),
    esip:set_config_value(software, <<"ejabberd ", (?VERSION)/binary>>),
    esip:set_config_value(module, ?MODULE),
    Spec = {mod_sip_registrar, {mod_sip_registrar, start_link, []},
	    transient, 2000, worker, [mod_sip_registrar]},
    TmpSupSpec = {mod_sip_proxy_sup,
		  {ejabberd_tmp_sup, start_link,
		   [mod_sip_proxy_sup, mod_sip_proxy]},
		  permanent, infinity, supervisor, [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, Spec),
    supervisor:start_child(ejabberd_sup, TmpSupSpec),
    ok.

stop(_Host) ->
    ok.

data_in(Data, #sip_socket{type = Transport,
                          addr = {MyIP, MyPort},
                          peer = {PeerIP, PeerPort}}) ->
    ?DEBUG(
       "SIP [~p/in] ~s:~p -> ~s:~p:~n~s",
       [Transport, inet_parse:ntoa(PeerIP), PeerPort,
	inet_parse:ntoa(MyIP), MyPort, Data]).

data_out(Data, #sip_socket{type = Transport,
                           addr = {MyIP, MyPort},
                           peer = {PeerIP, PeerPort}}) ->
    ?DEBUG(
       "SIP [~p/out] ~s:~p -> ~s:~p:~n~s",
       [Transport, inet_parse:ntoa(MyIP), MyPort,
	inet_parse:ntoa(PeerIP), PeerPort, Data]).

message_in(#sip{type = request, method = M} = Req, SIPSock)
  when M /= <<"ACK">>, M /= <<"CANCEL">> ->
    case action(Req, SIPSock) of
        {relay, _LServer, _Opts} ->
            ok;
        Action ->
            request(Req, SIPSock, undefined, Action)
    end;
message_in(_, _) ->
    ok.

message_out(_, _) ->
    ok.

response(Resp, SIPSock) ->
    case action(Resp, SIPSock) of
        {relay, LServer, Opts} ->
            case esip:split_hdrs('via', Resp#sip.hdrs) of
                {[_], _} ->
                    ok;
                {[_MyVia|Vias], TailHdrs} ->
		    %% TODO: check if MyVia is really my Via
		    NewResp = Resp#sip{hdrs = [{'via', Vias}|TailHdrs]},
		    case proplists:get_value(socket, Opts) of
			undefined ->
			    case esip:connect(NewResp,
					      add_certfile(LServer, Opts)) of
				{ok, SIPSockOut} ->
				    esip:send(SIPSockOut, NewResp);
				{error, _} ->
				    ok
			    end;
			SIPSockOut ->
			    esip:send(SIPSockOut, NewResp)
		    end
            end;
        _ ->
            ok
    end.

request(#sip{method = <<"ACK">>} = Req, SIPSock) ->
    case action(Req, SIPSock) of
        {relay, LServer, Opts} ->
	    Req1 = prepare_request(Req),
	    case esip:connect(Req1, add_certfile(LServer, Opts)) of
		{ok, SIPSockOut} ->
		    Req2 = add_via(SIPSockOut, LServer, Req1),
		    esip:send(SIPSockOut, Req2);
		{error, _} = Err ->
		    Err
	    end;
        _ ->
            pass
    end;
request(#sip{method = <<"CANCEL">>} = Req, SIPSock) ->
    case action(Req, SIPSock) of
        loop ->
            make_response(Req, #sip{status = 483, type = response});
        {unsupported, Require} ->
            make_response(Req, #sip{status = 420,
                                    type = response,
                                    hdrs = [{'unsupported',
                                             Require}]});
        {relay, LServer, Opts} ->
	    Req1 = prepare_request(Req),
	    case esip:connect(Req1, add_certfile(LServer, Opts)) of
		{ok, SIPSockOut} ->
		    Req2 = add_via(SIPSockOut, LServer, Req1),
		    esip:send(SIPSockOut, Req2);
		{error, _} = Err ->
		    Err
	    end,
            pass;
        _ ->
            pass
    end.

request(Req, SIPSock, TrID) ->
    request(Req, SIPSock, TrID, action(Req, SIPSock)).

request(Req, SIPSock, TrID, Action) ->
    case Action of
        to_me ->
            process(Req, SIPSock);
        register ->
            mod_sip_registrar:request(Req, SIPSock);
        loop ->
            make_response(Req, #sip{status = 483, type = response});
        {unsupported, Require} ->
            make_response(Req, #sip{status = 420,
                                    type = response,
                                    hdrs = [{'unsupported',
                                             Require}]});
        {relay, LServer, Opts} ->
            case mod_sip_proxy:start(LServer, Opts) of
                {ok, Pid} ->
                    mod_sip_proxy:route(Req, SIPSock, TrID, Pid),
                    {mod_sip_proxy, route, [Pid]};
                Err ->
		    ?INFO_MSG("failed to proxy request ~p: ~p", [Req, Err]),
                    Err
            end;
        {proxy_auth, Host} ->
            make_response(
              Req,
              #sip{status = 407,
                   type = response,
                   hdrs = [{'proxy-authenticate',
                            make_auth_hdr(Host)}]});
        {auth, Host} ->
            make_response(
              Req,
              #sip{status = 401,
                   type = response,
                   hdrs = [{'www-authenticate',
                            make_auth_hdr(Host)}]});
        deny ->
            make_response(Req, #sip{status = 403,
                                    type = response});
        not_found ->
            make_response(Req, #sip{status = 480,
                                    type = response})
    end.

locate(_SIPMsg) ->
    ok.

find(#uri{user = User, host = Host}) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Host),
    case mod_sip_registrar:find_session(
	   LUser, LServer) of
	{ok, #sip_session{socket = Sock}} ->
	    {relay, LServer, [{socket, Sock}]};
	error ->
	    not_found
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
action(#sip{type = response, hdrs = Hdrs}, _SIPSock) ->
    {_, ToURI, _} = esip:get_hdr('to', Hdrs),
    {_, FromURI, _} = esip:get_hdr('from', Hdrs),
    case at_my_host(FromURI) of
	true ->
	    case at_my_host(ToURI) of
		true ->
		    find(ToURI);
		false ->
		    LServer = jlib:nameprep(FromURI#uri.host),
		    {relay, LServer, []}
	    end;
	false ->
	    case at_my_host(ToURI) of
		true ->
		    find(ToURI);
		false ->
		    pass
	    end
    end;
action(#sip{method = <<"REGISTER">>, type = request, hdrs = Hdrs,
            uri = #uri{user = <<"">>} = URI} = Req, SIPSock) ->
    case at_my_host(URI) of
	true ->
	    case esip:get_hdrs('require', Hdrs) of
		[_|_] = Require ->
		    {unsupported, Require};
		_ ->
		    {_, ToURI, _} = esip:get_hdr('to', Hdrs),
		    case at_my_host(ToURI) of
			true ->
			    case check_auth(Req, 'authorization', SIPSock) of
				true ->
				    register;
				false ->
				    {auth, ToURI#uri.host}
			    end;
			false ->
			    deny
		    end
	    end;
	false ->
	    deny
    end;
action(#sip{method = Method, hdrs = Hdrs, type = request} = Req, SIPSock) ->
    case esip:get_hdr('max-forwards', Hdrs) of
        0 when Method == <<"OPTIONS">> ->
            to_me;
        0 ->
            loop;
        _ ->
            case esip:get_hdrs('proxy-require', Hdrs) of
                [_|_] = Require ->
                    {unsupported, Require};
                _ ->
                    {_, ToURI, _} = esip:get_hdr('to', Hdrs),
                    {_, FromURI, _} = esip:get_hdr('from', Hdrs),
		    case at_my_host(FromURI) of
			true ->
			    case check_auth(Req, 'proxy-authorization', SIPSock) of
                                true ->
				    case at_my_host(ToURI) of
					true ->
					    find(ToURI);
					false ->
					    LServer = jlib:nameprep(FromURI#uri.host),
					    {relay, LServer, []}
				    end;
                                false ->
                                    {proxy_auth, FromURI#uri.host}
                            end;
			false ->
			    case at_my_host(ToURI) of
				true ->
				    find(ToURI);
				false ->
				    deny
			    end
                    end
            end
    end.

check_auth(#sip{method = <<"CANCEL">>}, _, _SIPSock) ->
    true;
check_auth(#sip{method = Method, hdrs = Hdrs, body = Body}, AuthHdr, _SIPSock) ->
    
    Issuer = case AuthHdr of
                 'authorization' ->
                     to;
                 'proxy-authorization' ->
                     from
             end,
    {_, #uri{user = User, host = Host}, _} = esip:get_hdr(Issuer, Hdrs),
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Host),
    case lists:filter(
           fun({_, Params}) ->
                   Username = esip:get_param(<<"username">>, Params),
                   Realm = esip:get_param(<<"realm">>, Params),
                   (LUser == esip:unquote(Username))
                       and (LServer == esip:unquote(Realm))
           end, esip:get_hdrs(AuthHdr, Hdrs)) of
        [Auth|_] ->
	    case ejabberd_auth:get_password_s(LUser, LServer) of
		<<"">> ->
		    false;
		Password ->
		    esip:check_auth(Auth, Method, Body, Password)
	    end;
        [] ->
            false
    end.

allow() ->
    [<<"OPTIONS">>, <<"REGISTER">>].

process(#sip{method = <<"OPTIONS">>} = Req, _) ->
    make_response(Req, #sip{type = response, status = 200,
                            hdrs = [{'allow', allow()}]});
process(#sip{method = <<"REGISTER">>} = Req, _) ->
    make_response(Req, #sip{type = response, status = 400});
process(Req, _) ->
    make_response(Req, #sip{type = response, status = 405,
			    hdrs = [{'allow', allow()}]}).

prepare_request(#sip{hdrs = Hdrs1} = Req) ->
    MF = esip:get_hdr('max-forwards', Hdrs1),
    Hdrs2 = esip:set_hdr('max-forwards', MF-1, Hdrs1),
    Hdrs3 = lists:filter(
              fun({'proxy-authorization', {_, Params}}) ->
                      Realm = esip:unquote(esip:get_param(<<"realm">>, Params)),
		      not is_my_host(jlib:nameprep(Realm));
                 (_) ->
                      true
              end, Hdrs2),
    Req#sip{hdrs = Hdrs3}.

make_auth_hdr(LServer) ->
    Realm = jlib:nameprep(LServer),
    {<<"Digest">>, [{<<"realm">>, esip:quote(Realm)},
                    {<<"qop">>, esip:quote(<<"auth">>)},
                    {<<"nonce">>, esip:quote(esip:make_hexstr(20))}]}.

make_response(Req, Resp) ->
    esip:make_response(Req, Resp, esip:make_tag()).

at_my_host(#uri{host = Host}) ->
    is_my_host(jlib:nameprep(Host)).

is_my_host(LServer) ->
    gen_mod:is_loaded(LServer, ?MODULE).

add_certfile(LServer, Opts) ->
    case ejabberd_config:get_option({domain_certfile, LServer},
				    fun iolist_to_binary/1) of
	CertFile when is_binary(CertFile), CertFile /= <<"">> ->
	    [{certfile, CertFile}|Opts];
	_ ->
	    Opts
    end.

add_via(#sip_socket{type = Transport}, LServer, #sip{hdrs = Hdrs} = Req) ->
    ConfiguredVias = get_configured_vias(LServer),
    {ViaHost, ViaPort} = proplists:get_value(
			   Transport, ConfiguredVias, {LServer, undefined}),
    ViaTransport = case Transport of
		       tls -> <<"TLS">>;
		       tcp -> <<"TCP">>;
		       udp -> <<"UDP">>
		   end,
    Via = #via{transport = ViaTransport,
	       host = ViaHost,
	       port = ViaPort,
	       params = [{<<"branch">>, esip:make_branch()},
			 {<<"rport">>, <<"">>}]},
    Req#sip{hdrs = [{'via', [Via]}|Hdrs]}.

get_configured_vias(LServer) ->
    gen_mod:get_module_opt(
      LServer, ?MODULE, via,
      fun(L) ->
	      lists:map(
		fun(Opts) ->
			Type = proplists:get_value(type, Opts),
			Host = proplists:get_value(host, Opts),
			Port = proplists:get_value(port, Opts),
			true = (Type == tcp) or (Type == tls) or (Type == udp),
			true = is_binary(Host) and (Host /= <<"">>),
			true = (is_integer(Port)
				and (Port > 0) and (Port < 65536))
			    or (Port == undefined),
			{Type, {Host, Port}}
		end, L)
      end, []).
