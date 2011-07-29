-module(run_common_test).
-export([ct/0]).

-define(CT_DIR, filename:join([".", "tests"])).
-define(CT_REPORT, filename:join([".", "ct_report"])).
-define(CT_CONFIG, "test.config").

ct() ->
    ct:run_test([
        {config, [?CT_CONFIG]},
        {dir, ?CT_DIR},
        {logdir, ?CT_REPORT},

        {suite, "login_SUITE"},
        {group, [login]},
        {testcase, [log_one_basic_digest]}
        %{group, [unregistered]}

        %{suite, "presence_SUITE"},
        %{group, [presence]}
        %{group, [roster]}
        %{group, [subscribe]}

        %{suite, "privacy_SUITE"}

        %, {group, [management]}
        %, {testcase, get_all_lists_with_active}
        %, {testcase, get_all_lists_with_default}
        %, {testcase, get_many_lists}
        %, {testcase, get_existing_list}
        %, {testcase, activate}

        %, {group, [blocking]}
        %, {testcase, block_jid_message}
        %, {testcase, block_jid_all}
    ]),
    init:stop(0).
