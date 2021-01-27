%%%-------------------------------------------------------------------
%%% File    : mod_sip.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : SIP RFC-3261
%%% Created : 21 Apr 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2021   ProcessOne
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
%%%-------------------------------------------------------------------

-module(mod_sip).
-protocol({rfc, 3261}).

-include("logger.hrl").
-include("translate.hrl").

-ifndef(SIP).
-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0]).
start(_, _) ->
    ?CRITICAL_MSG("ejabberd is not compiled with SIP support", []),
    {error, sip_not_compiled}.
stop(_) ->
    ok.
depends(_, _) ->
    [].
mod_options(_) ->
    [].
mod_doc() ->
    #{desc => [?T("SIP support has not been enabled.")]}.
-else.
-behaviour(gen_mod).
-behaviour(esip).

%% API
-export([start/2, stop/1, reload/3,
	 make_response/2, is_my_host/1, at_my_host/1]).

-export([data_in/2, data_out/2, message_in/2,
	 message_out/2, request/2, request/3, response/2,
	 locate/1, mod_opt_type/1, mod_options/1, depends/2,
         mod_doc/0]).

-include_lib("esip/include/esip.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start(_Host, _Opts) ->
    ejabberd:start_app(esip),
    esip:set_config_value(max_server_transactions, 10000),
    esip:set_config_value(max_client_transactions, 10000),
    esip:set_config_value(
      software, <<"ejabberd ", (ejabberd_option:version())/binary>>),
    esip:set_config_value(module, ?MODULE),
    Spec = {mod_sip_registrar, {mod_sip_registrar, start_link, []},
	    transient, 2000, worker, [mod_sip_registrar]},
    TmpSupSpec = {mod_sip_proxy_sup,
		  {ejabberd_tmp_sup, start_link,
		   [mod_sip_proxy_sup, mod_sip_proxy]},
		  permanent, infinity, supervisor, [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_gen_mod_sup, Spec),
    supervisor:start_child(ejabberd_gen_mod_sup, TmpSupSpec),
    ok.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

data_in(Data, #sip_socket{type = Transport,
                          addr = {MyIP, MyPort},
                          peer = {PeerIP, PeerPort}}) ->
    ?DEBUG(
       "SIP [~p/in] ~ts:~p -> ~ts:~p:~n~ts",
       [Transport, inet_parse:ntoa(PeerIP), PeerPort,
	inet_parse:ntoa(MyIP), MyPort, Data]).

data_out(Data, #sip_socket{type = Transport,
                           addr = {MyIP, MyPort},
                           peer = {PeerIP, PeerPort}}) ->
    ?DEBUG(
       "SIP [~p/out] ~ts:~p -> ~ts:~p:~n~ts",
       [Transport, inet_parse:ntoa(MyIP), MyPort,
	inet_parse:ntoa(PeerIP), PeerPort, Data]).

message_in(#sip{type = request, method = M} = Req, SIPSock)
  when M /= <<"ACK">>, M /= <<"CANCEL">> ->
    case action(Req, SIPSock) of
        {relay, _LServer} ->
            ok;
        Action ->
            request(Req, SIPSock, undefined, Action)
    end;
message_in(ping, SIPSock) ->
    mod_sip_registrar:ping(SIPSock);
message_in(_, _) ->
    ok.

message_out(_, _) ->
    ok.

response(_Resp, _SIPSock) ->
    ok.

request(#sip{method = <<"ACK">>} = Req, SIPSock) ->
    case action(Req, SIPSock) of
	{relay, LServer} ->
	    mod_sip_proxy:route(Req, LServer, [{authenticated, true}]);
	{proxy_auth, LServer} ->
	    mod_sip_proxy:route(Req, LServer, [{authenticated, false}]);
	_ ->
	    ok
    end;
request(_Req, _SIPSock) ->
    ok.

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
        {relay, LServer} ->
            case mod_sip_proxy:start(LServer, []) of
                {ok, Pid} ->
                    mod_sip_proxy:route(Req, SIPSock, TrID, Pid),
                    {mod_sip_proxy, route, [Pid]};
                Err ->
		    ?WARNING_MSG("Failed to proxy request ~p: ~p", [Req, Err]),
                    Err
            end;
        {proxy_auth, LServer} ->
            make_response(
              Req,
              #sip{status = 407,
                   type = response,
                   hdrs = [{'proxy-authenticate',
                            make_auth_hdr(LServer)}]});
        {auth, LServer} ->
            make_response(
              Req,
              #sip{status = 401,
                   type = response,
                   hdrs = [{'www-authenticate',
                            make_auth_hdr(LServer)}]});
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
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Host),
    if LUser == <<"">> ->
	    to_me;
       true ->
	    case mod_sip_registrar:find_sockets(LUser, LServer) of
		[] ->
		    not_found;
		[_|_] ->
		    {relay, LServer}
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
action(#sip{method = <<"REGISTER">>, type = request, hdrs = Hdrs,
            uri = #uri{user = <<"">>} = URI} = Req, SIPSock) ->
    case at_my_host(URI) of
	true ->
	    Require = esip:get_hdrs('require', Hdrs) -- supported(),
	    case Require of
		[_|_] ->
		    {unsupported, Require};
		_ ->
		    {_, ToURI, _} = esip:get_hdr('to', Hdrs),
		    case at_my_host(ToURI) of
			true ->
			    case check_auth(Req, 'authorization', SIPSock) of
				true ->
				    register;
				false ->
				    {auth, jid:nameprep(ToURI#uri.host)}
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
	    Require = esip:get_hdrs('proxy-require', Hdrs) -- supported(),
            case Require of
                [_|_] ->
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
					    LServer = jid:nameprep(FromURI#uri.host),
					    {relay, LServer}
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
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Host),
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
		Password when is_binary(Password) ->
		    esip:check_auth(Auth, Method, Body, Password);
		_ScramedPassword ->
		    ?ERROR_MSG("Unable to authenticate ~ts@~ts against SCRAM'ed "
			       "password", [LUser, LServer]),
		    false
	    end;
        [] ->
            false
    end.

allow() ->
    [<<"OPTIONS">>, <<"REGISTER">>].

supported() ->
    [<<"path">>, <<"outbound">>].

process(#sip{method = <<"OPTIONS">>} = Req, _) ->
    make_response(Req, #sip{type = response, status = 200,
                            hdrs = [{'allow', allow()},
				    {'supported', supported()}]});
process(#sip{method = <<"REGISTER">>} = Req, _) ->
    make_response(Req, #sip{type = response, status = 400});
process(Req, _) ->
    make_response(Req, #sip{type = response, status = 405,
			    hdrs = [{'allow', allow()}]}).

make_auth_hdr(LServer) ->
    {<<"Digest">>, [{<<"realm">>, esip:quote(LServer)},
                    {<<"qop">>, esip:quote(<<"auth">>)},
                    {<<"nonce">>, esip:quote(esip:make_hexstr(20))}]}.

make_response(Req, Resp) ->
    esip:make_response(Req, Resp, esip:make_tag()).

at_my_host(#uri{host = Host}) ->
    is_my_host(jid:nameprep(Host)).

is_my_host(LServer) ->
    gen_mod:is_loaded(LServer, ?MODULE).

mod_opt_type(always_record_route) ->
    econf:bool();
mod_opt_type(flow_timeout_tcp) ->
    econf:timeout(second);
mod_opt_type(flow_timeout_udp) ->
    econf:timeout(second);
mod_opt_type(record_route) ->
    econf:sip_uri();
mod_opt_type(routes) ->
    econf:list(econf:sip_uri());
mod_opt_type(via) ->
    econf:list(
      fun(L) when is_list(L) ->
              (econf:and_then(
                 econf:options(
                   #{type => econf:enum([tcp, tls, udp]),
                     host => econf:domain(),
                     port => econf:port()},
                   [{required, [type, host]}]),
                 fun(Opts) ->
                         Type = proplists:get_value(type, Opts),
                         Host = proplists:get_value(host, Opts),
                         Port = proplists:get_value(port, Opts),
                         {Type, {Host, Port}}
                 end))(L);
         (U) ->
              (econf:and_then(
                 econf:url([tls, tcp, udp]),
                 fun(URI) ->
                         {ok, Type, Host, Port, _} =
                            misc:uri_parse(URI),
                         {Type, {unicode:characters_to_binary(Host), Port}}
                 end))(U)
      end, [unique]).

-spec mod_options(binary()) -> [{via, [{tcp | tls | udp, {binary(), 1..65535}}]} |
				{atom(), term()}].
mod_options(Host) ->
    Route = #uri{scheme = <<"sip">>,
		 host = Host,
		 params = [{<<"lr">>, <<>>}]},
    [{always_record_route, true},
     {flow_timeout_tcp, timer:seconds(120)},
     {flow_timeout_udp, timer:seconds(29)},
     {record_route, Route},
     {routes, [Route]},
     {via, []}].

mod_doc() ->
    #{desc =>
          [?T("This module adds SIP proxy/registrar support "
              "for the corresponding virtual host."), "",
           ?T("NOTE: It is not enough to just load this module. "
              "You should also configure listeners and DNS records "
              "properly. For details see the section about the "
              "http://../listen/#ejabberd-sip[ejabberd_sip] listen module "
              "in the ejabberd Documentation.")],
      opts =>
          [{always_record_route,
            #{value => "true | false",
              desc =>
                  ?T("Always insert \"Record-Route\" header into "
                     "SIP messages. This approach allows to bypass "
                     "NATs/firewalls a bit more easily. "
                     "The default value is 'true'.")}},
           {flow_timeout_tcp,
            #{value => "timeout()",
              desc =>
                  ?T("The option sets a keep-alive timer for "
                     "https://tools.ietf.org/html/rfc5626[SIP outbound] "
                     "TCP connections. The default value is '2' minutes.")}},
           {flow_timeout_udp,
            #{value => "timeout()",
              desc =>
                  ?T("The options sets a keep-alive timer for "
                     "https://tools.ietf.org/html/rfc5626[SIP outbound] "
                     "UDP connections. The default value is '29' seconds.")}},
           {record_route,
            #{value => ?T("URI"),
              desc =>
                  ?T("When the option 'always_record_route' is set to "
                     "'true' or when https://tools.ietf.org/html/rfc5626"
                     "[SIP outbound] is utilized, ejabberd inserts "
                     "\"Record-Route\" header field with this 'URI' into "
                     "a SIP message. The default is a SIP URI constructed "
                     "from the virtual host on which the module is loaded.")}},
           {routes,
            #{value => "[URI, ...]",
              desc =>
                  ?T("You can set a list of SIP URIs of routes pointing "
                     "to this SIP proxy server. The default is a list containing "
                     "a single SIP URI constructed from the virtual host "
                     "on which the module is loaded.")}},
           {via,
            #{value => "[URI, ...]",
              desc =>
                  ?T("A list to construct \"Via\" headers for "
                     "inserting them into outgoing SIP messages. "
                     "This is useful if you're running your SIP proxy "
                     "in a non-standard network topology. Every 'URI' "
                     "element in the list must be in the form of "
                     "\"scheme://host:port\", where \"transport\" "
                     "must be 'tls', 'tcp', or 'udp', \"host\" must "
                     "be a domain name or an IP address and \"port\" "
                     "must be an internet port number. Note that all "
                     "parts of the 'URI' are mandatory (e.g. you "
                     "cannot omit \"port\" or \"scheme\").")}}],
      example =>
          ["modules:",
           "  ...",
           "  mod_sip:",
           "    always_record_route: false",
           "    record_route: \"sip:example.com;lr\"",
           "    routes:",
           "      - \"sip:example.com;lr\"",
           "      - \"sip:sip.example.com;lr\"",
           "    flow_timeout_udp: 30 sec",
           "    flow_timeout_tcp: 1 min",
           "    via:",
           "      - tls://sip-tls.example.com:5061",
           "      - tcp://sip-tcp.example.com:5060",
           "      - udp://sip-udp.example.com:5060",
           "  ..."]}.

-endif.
