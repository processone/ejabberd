-module(run_common_test).
-export([ct/0]).

-define(CT_DIR, filename:join([".", "tests"])).
-define(CT_REPORT, filename:join([".", "ct_report"])).
-define(CT_CONFIG, "test.config").

ct() ->
    ct:run_test([
        {config, [?CT_CONFIG]},
        {dir, ?CT_DIR},
        {logdir, ?CT_REPORT}

        %{suite, "snmp_session_SUITE"}
        %{group, [session]},
        %{testcase, [auth_failed]}
        
        %{suite, "login_SUITE"},
        %{group, [login]},
        %{testcase, [log_one_basic_digest]}
        %{group, [unregistered]}

        %{suite, "presence_SUITE"},
        %{group, [presence]}
        %{group, [roster]}
        %{group, [subscribe]}

        %{suite, "snmp_c2s_SUITE"}
        %, {group, [errors]}
        %, {testcase, [error_presence]}

        %{suite, "snmp_register_SUITE"}
        
        %, {group, [management]}
        %, {testcase, get_all_lists_with_active}
        %, {testcase, get_all_lists_with_default}
        %, {testcase, get_many_lists}
        %, {testcase, get_existing_list}
        %, {testcase, activate}

        %, {group, [blocking]}
        %, {testcase, block_jid_message}
        %, {testcase, block_jid_all}

        %{repeat, 10},
        %{suite, "snmp_SUITE"}

        %, {group, [mod_privacy]}
        %, {testcase, modPrivacyGets}
        %, {testcase, modPrivacySets}
        %, {testcase, modPrivacySetsActive}
        %, {testcase, modPrivacySetsDefault}
        %, {testcase, modPrivacyStanzaBlocked}
        %, {testcase, modPrivacyStanzaAll}
        %, {testcase, modPrivacyPush}
        %, {testcase, modPrivacyListLength}

        %{suite, "privacy_SUITE"}
    ]),
    init:stop(0).
