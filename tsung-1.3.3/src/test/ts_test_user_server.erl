%%%-------------------------------------------------------------------
%%% File    : ts_test_user_server.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 20 Mar 2005 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_user_server).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ts_profile.hrl").
-include_lib("ts_config.hrl").


test()->
    ok.

next_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(100),
    ts_user_server:get_idle(),
    B=ts_user_server:get_idle(),
    ?assertMatch(B,2).

remove_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(100),
    1=ts_user_server:get_idle(),
    B=ts_user_server:get_idle(),
    ts_user_server:remove_connected(B),
    C=ts_user_server:get_idle(),
    ?assertMatch(C,3).

full_offline_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(3),
    1=ts_user_server:get_idle(),
    2=ts_user_server:get_idle(),
    3=ts_user_server:get_idle(),
    ?assertMatch({error,no_free_userid},ts_user_server:get_idle()).

full_free_offline_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(3),
    1=ts_user_server:get_idle(),
    B=ts_user_server:get_idle(),
    3=ts_user_server:get_idle(),
    {error,no_free_userid}=ts_user_server:get_idle(),
    ts_user_server:remove_connected(B),
    ?assertMatch(B,ts_user_server:get_idle()).

full_free_offline_refull_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(3),
    A=ts_user_server:get_idle(),
    B=ts_user_server:get_idle(),
    3=ts_user_server:get_idle(),
    {error,no_free_userid}=ts_user_server:get_idle(),
    ts_user_server:remove_connected(A),
    ts_user_server:remove_connected(B),
    A=ts_user_server:get_idle(),
    ?assertMatch(B,ts_user_server:get_idle()).

full_huge_offline_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(1000000),
    A=ts_user_server:get_idle(),
    ?assertMatch(2,ts_user_server:get_idle()).

offline_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(3),
    {ok,1}=ts_user_server:get_offline(),
    {ok,2}=ts_user_server:get_offline(),
    {ok,3}=ts_user_server:get_offline(),
    ?assertMatch({ok,1},ts_user_server:get_offline()).
offline_full_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(2),
    ts_user_server:get_idle(),
    ts_user_server:get_idle(),
    ?assertMatch({error,no_offline},ts_user_server:get_offline()).

online_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(3),
    A=ts_user_server:get_idle(),
    B=ts_user_server:get_idle(),
    ts_user_server:add_to_online(A),
    ts_user_server:add_to_online(B),
    ?assertMatch({ok, A},ts_user_server:get_online(B)).

online_full_test() ->
    myset_env(),
    ts_user_server:start(),
    ts_user_server:reset(10),
    A=ts_user_server:get_idle(),
    B=ts_user_server:get_idle(),
    ts_user_server:add_to_online(3),
    ?assertMatch({error,no_online},ts_user_server:get_online(B)).

myset_env()->
    application:set_env(stdlib,debug_level,0).
