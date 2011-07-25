%%%-------------------------------------------------------------------
%%% File    : ts_test_recorder.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 20 Mar 2005 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_recorder).

-compile(export_all).

-include("ts_http.hrl").
-include("ts_profile.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(ts_http_common,[parse_req/1, parse_req/2]).

-define(HTTP_GET_RES,{ok, #http_request{method='GET', version="1.0"}, _}).

test()->
    ok.
parse_http_request_test() ->
    ?assertMatch(?HTTP_GET_RES, parse_req("GET / HTTP/1.0\r\n\r\n")).
parse_http_partial_request_test() ->
%    ?log("Testing HTTP request parsing, partial first line ", []),
    {more,H,Res} = parse_req("GET / HTTP/1.0\r"),
    ?assertMatch(?HTTP_GET_RES, parse_req(H,Res ++ "\n\r\n")).
parse_http_partiel_request2_test() ->
    {more,H,Res} = parse_req("GET / HTTP/1.0\r\n"),
    ?assertMatch(?HTTP_GET_RES,parse_req(H,Res ++ "\r\n")).
parse_http_request3_test() ->
    Res = parse_req("POST / HTTP/1.0\r\n\r\nmesdata\r\nsdfsdfs\r\n\r\n"),
    ?assertMatch({ok, #http_request{method='POST', version="1.0"},"mesdata\r\nsdfsdfs\r\n\r\n"},Res).
parse_http_request5_test() ->
%    ?log("Testing HTTP request parsing, POST with content-length ", []),
    {ok, Http, Body} = parse_req("POST / HTTP/1.0\r\n"
                                 ++"Server: www.glop.org\r\n"
                                 ++"Content-length: 16\r\n\r\n"
                                 ++"mesdata\r\nsdfsdfs\r\n\r\n"),
    CL = ts_utils:key1search(Http#http_request.headers,"content-length"),
    ?assertEqual({16, "mesdata\r\nsdfsdfs\r\n\r\n"},{list_to_integer(CL), Body}).
parse_http_request6_test() ->
%    ?log("Testing HTTP request parsing, POST with content-length; partial ", []),
    {more, Http, Body} = parse_req("POST / HTTP/1.0\r\n"
                                   ++"Server: www.glop.org\r\n"
                                   ++"Content-le"),
    Rest = "ngth: 16\r\n\r\n"++"mesdata\r\nsdfsdfs\r\n\r\n",
    {ok, Http2, Body2} = parse_req(Http,Body ++ Rest),
    CL = ts_utils:key1search(Http2#http_request.headers,"content-length"),
    ?assertEqual({16, "mesdata\r\nsdfsdfs\r\n\r\n"},{list_to_integer(CL), Body2}).

parse_http_request7_test() ->
    Req= "GET http://www.niclux.org/ HTTP/1.1\r\nHost: www.niclux.org\r\nUser-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.7.5) Gecko/20041209 Firefox/1.0 (Ubuntu) (Ubuntu package 1.0-2ubuntu4-warty99)\r\nAccept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\nAccept-Language: fr-fr,en-us;q=0.7,en;q=0.3\r\nAccept-Encoding: gzip,deflate\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 300\r\nProxy-Connection: keep-alive\r\n\r\n",
    ?assertMatch({ok, #http_request{method='GET', version="1.1"}, []},parse_req(Req)).

decode_base64_test()->
    Base="QWxhZGRpbjpvcGVuIHNlc2FtZQ==",
    ?assertEqual({"Aladdin","open sesame"}, ts_proxy_http:decode_basic_auth(Base)).

%%TODO: should be in ts_test_http
encode_base64_test()->
    Base="QWxhZGRpbjpvcGVuIHNlc2FtZQ==",
    ?assertEqual(["Authorization: Basic ",Base,?CRLF], ts_http_common:authenticate("Aladdin","open sesame")).

rewrite_http_secure_cookie_test()->
    Data="HTTP/1.1 200 OK\r\nSet-Cookie: JSESSIONID=F949C9182402EB74258F43FDC3F3C63F; Path=/; Secure\r\nLocation: https://foo.bar/\r\nContent-Length: 0\r\n\r\n",
    NewData="HTTP/1.1 200 OK\r\nSet-Cookie: JSESSIONID=F949C9182402EB74258F43FDC3F3C63F; Path=/\r\nLocation: http://-foo.bar/\r\nContent-Length: 0\r\n\r\n",
     ?assertEqual({ok,NewData},
                  ts_utils:from_https(Data)).

rewrite_http_secure_cookies_test()->
    Data="HTTP/1.1 200 OK\r\nSet-Cookie: JSESSIONID=F949C9182402EB74258F43FDC3F3C63F; Path=/; Secure\r\nSet-Cookie: JSESSIONID=32; Path=/foo; Secure\r\nLocation: https://foo.bar/\r\nContent-Length: 0\r\n\r\n",
    NewData="HTTP/1.1 200 OK\r\nSet-Cookie: JSESSIONID=F949C9182402EB74258F43FDC3F3C63F; Path=/\r\nSet-Cookie: JSESSIONID=32; Path=/foo\r\nLocation: http://-foo.bar/\r\nContent-Length: 0\r\n\r\n",
     ?assertMatch({ok,Data2},
                  ts_utils:from_https(Data)).
