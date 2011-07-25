%%%-------------------------------------------------------------------
%%% File    : ts_test_pgsql.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 10 Apr 2008 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_pgsql).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_pgsql.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->
    ok.


utils_md5_test()->
    myset_env(),
    Password="sesame",
    User="benchmd5",
     Salt= << 54,195,212,197 >>,
     Hash= list_to_binary(["md5967c89f451d1d504a1f02fc69fb65cb5",0]),
    PacketSize= 4+size(Hash),
    Bin= <<$p,PacketSize:32/integer, Hash/binary>>,
    ?assertMatch(Bin,  pgsql_proto:encode_message(pass_md5, {User,Password,Salt} ) ).

myset_env()->
    application:set_env(stdlib,debug_level,0).

