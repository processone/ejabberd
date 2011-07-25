%%%-------------------------------------------------------------------
%%% File    : ts_test_recorder.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 20 Mar 2005 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_config).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->
    ok.
read_config_http_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/http_simple.xml",".")).
read_config_http2_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/http_distributed.xml",".")).
read_config_pgsql_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/pgsql.xml",".")).
read_config_jabber_test() ->
    myset_env(),
    ts_user_server:start([]),
    ?assertMatch({ok, Config}, ts_config:read("./examples/jabber.xml",".")).

read_config_jabber_muc_test() ->
    myset_env(),
    ts_user_server:start([]),
    ?assertMatch({ok, Config}, ts_config:read("./examples/jabber_muc.xml",".")).

config_get_session_test() ->
    myset_env(),
    ts_user_server:start([]),
    ts_config_server:start_link(["/tmp"]),
    ok = ts_config_server:read_config("./examples/http_setdynvars.xml"),
    {ok, {Session,IP,Server,1} }  = ts_config_server:get_next_session("localhost"),
    ?assertEqual(1, Session#session.id).

config_get_session_size_test() ->
    myset_env(),
    {ok, {Session,IP,Server,2} }  = ts_config_server:get_next_session("localhost"),
    ?assertEqual(13, Session#session.size).


read_config_badpop_test() ->
    myset_env(),
    ts_user_server:start([]),
    {ok, Config} = ts_config:read("./src/test/badpop.xml","."),
    ?assertMatch({error,[{error,{bad_sum,_,_}}]}, ts_config_server:check_config(Config)).


read_config_thinkfirst_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./src/test/thinkfirst.xml",".")).


config_minmax_test() ->
    myset_env(),
    {ok, {Session,IP,Server,3} }  = ts_config_server:get_next_session("localhost"),
    Id = Session#session.id,
    ?assertMatch({thinktime,{range,2000,4000}}, ts_config_server:get_req(Id,7)).

config_minmax2_test() ->
    myset_env(),
    {ok, {Session,IP,Server,4} }  = ts_config_server:get_next_session("localhost"),
    Id = Session#session.id,
    {thinktime, Req} = ts_config_server:get_req(Id,7),
    Think=ts_client:set_thinktime(Req),
    Resp = receive
         Data-> Data
    end,
    ?assertMatch({timeout,_,end_thinktime}, Resp).

config_thinktime_test() ->
    myset_env(),
    ok = ts_config_server:read_config("./examples/thinks.xml"),
    {ok, {Session,IP,Server,5} }  = ts_config_server:get_next_session("localhost"),
    Id = Session#session.id,
    {thinktime, Req=2000} = ts_config_server:get_req(Id,5),
    {thinktime, 2000} = ts_config_server:get_req(Id,7),
    Think=ts_client:set_thinktime(Req),
    Resp = receive
         Data-> Data
    end,
    ?assertMatch({timeout,_,end_thinktime}, Resp).


config_thinktime2_test() ->
    myset_env(),
    ok = ts_config_server:read_config("./examples/thinks2.xml"),
    {ok, {Session,{IP,0},Server,6} }  = ts_config_server:get_next_session("localhost"),
    Id = Session#session.id,
    {thinktime, Req} = ts_config_server:get_req(Id,5),
    Ref=ts_client:set_thinktime(Req),
    receive
        {timeout,Ref2,end_thinktime} -> ok
    end,
    random:seed(), % reinit seed for others tests
    ?assertMatch({random,1000}, Req).
read_config_maxusers_test() ->
    myset_env(),
    MaxNumber=10,
    ts_config_server:read_config("./src/test/thinkfirst.xml"),
    {ok,{[{_,Max1},{_,_}],_,_}}=ts_config_server:get_client_config("client1"),
    {ok,{[{_,Max2},{_,_}],_,_}}=ts_config_server:get_client_config("client2"),
    {ok,{[{_,Max3},{_,_}],_,_}}=ts_config_server:get_client_config("client3"),
    {ok,{[{_,Max4},{_,_}],_,_}}=ts_config_server:get_client_config("client4"),
    ?assert(Max1+Max2+Max3+Max4 =< MaxNumber).

choose_port_test() ->
    myset_env(),
    {Dict,3} = ts_config_server:choose_port('client',undefined,{3,5}),
    {Dict2,4} = ts_config_server:choose_port('client',Dict,{3,5}),
    {Dict3,5} = ts_config_server:choose_port('client',Dict2,{3,5}),
    {Dict4,3} = ts_config_server:choose_port('client2',Dict3,{3,5}),
    ?assertMatch({_,3}, ts_config_server:choose_port('client',Dict4,{3,5})).

myset_env()->
    myset_env(0).
myset_env(Level)->
    catch  ts_user_server_sup:start_link() ,
    application:set_env(stdlib,debug_level,Level),
    application:set_env(stdlib,warm_time,1000),
    application:set_env(stdlib,thinktime_value,"5"),
    application:set_env(stdlib,thinktime_override,"false"),
    application:set_env(stdlib,thinktime_random,"false").
