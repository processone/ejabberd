-module(run_common_test).
-export([ct/0]).

-define(CT_DIR, filename:join([".", "tests"])).
-define(CT_REPORT, filename:join([".", "ct_report"])).
-define(CT_CONFIG, "test.config").
%%-define(CT_INCLUDE, filename:join(["/home/wirenth/projects/ejabberd_otp/test","escalus", "include"])).

ct() ->
    ct:run_test([{config, [?CT_CONFIG]},
    		 {dir, ?CT_DIR},
		 {logdir, ?CT_REPORT}]),
%%                 {include, [?CT_INCLUDE]}]),
    init:stop(0).
