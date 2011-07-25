%%%-------------------------------------------------------------------
%%% File    : ts_test_all.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : run all test functions
%%%
%%% Created : 17 Mar 2007 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_all).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

test() -> ok.

all_test_() -> [ts_test_recorder,
                ts_test_config,
                ts_test_dynvars_api,
                ts_test_file_server,
                ts_test_pgsql,
                ts_test_proxy,
                ts_test_http,
                ts_test_jabber,
                ts_test_match,
                ts_test_mon,
                ts_test_user_server,
                ts_test_user_server,
                ts_test_search].
