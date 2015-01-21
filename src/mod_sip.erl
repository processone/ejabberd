%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%% ejabberd, Copyright (C) 2014-2015   ProcessOne
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

%%%-------------------------------------------------------------------
-module(mod_sip).

-behaviour(gen_mod).
-behaviour(esip).

%% API
-export([start/2, stop/1, make_response/2, is_my_host/1, at_my_host/1]).

%% esip_callbacks
-export([data_in/2, data_out/2, message_in/2, message_out/2,
	 request/2, request/3, response/2, locate/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("esip/include/esip.hrl").

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
		    ?INFO_MSG("failed to proxy request ~p: ~p", [Req, Err]),
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
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Host),
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
				    {auth, jlib:nameprep(ToURI#uri.host)}
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
					    LServer = jlib:nameprep(FromURI#uri.host),
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
    is_my_host(jlib:nameprep(Host)).

is_my_host(LServer) ->
    gen_mod:is_loaded(LServer, ?MODULE).
