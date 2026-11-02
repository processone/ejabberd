%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_antispam_opt).

-export([access_spam/1]).
-export([allow_domains_file/1]).
-export([cache_size/1]).
-export([collect_report_stats/1]).
-export([forward_reports/1]).
-export([forward_reports_to_third_party/1]).
-export([rtbl_services/1]).
-export([spam_domains_file/1]).
-export([spam_dump_file/1]).
-export([spam_jids_file/1]).
-export([spam_urls_file/1]).

-spec access_spam(gen_mod:opts() | global | binary()) -> 'none' | acl:acl().
access_spam(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_spam, Opts);
access_spam(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, access_spam).

-spec allow_domains_file(gen_mod:opts() | global | binary()) -> 'none' | binary().
allow_domains_file(Opts) when is_map(Opts) ->
    gen_mod:get_opt(allow_domains_file, Opts);
allow_domains_file(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, allow_domains_file).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'unlimited' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, cache_size).

-spec collect_report_stats(gen_mod:opts() | global | binary()) -> boolean().
collect_report_stats(Opts) when is_map(Opts) ->
    gen_mod:get_opt(collect_report_stats, Opts);
collect_report_stats(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, collect_report_stats).

-spec forward_reports(gen_mod:opts() | global | binary()) -> boolean().
forward_reports(Opts) when is_map(Opts) ->
    gen_mod:get_opt(forward_reports, Opts);
forward_reports(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, forward_reports).

-spec forward_reports_to_third_party(gen_mod:opts() | global | binary()) -> [jid:jid()].
forward_reports_to_third_party(Opts) when is_map(Opts) ->
    gen_mod:get_opt(forward_reports_to_third_party, Opts);
forward_reports_to_third_party(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, forward_reports_to_third_party).

-spec rtbl_services(gen_mod:opts() | global | binary()) -> [binary() | [{binary(),[{'spam_source_domains_node',binary()}]}]].
rtbl_services(Opts) when is_map(Opts) ->
    gen_mod:get_opt(rtbl_services, Opts);
rtbl_services(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, rtbl_services).

-spec spam_domains_file(gen_mod:opts() | global | binary()) -> 'none' | binary().
spam_domains_file(Opts) when is_map(Opts) ->
    gen_mod:get_opt(spam_domains_file, Opts);
spam_domains_file(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, spam_domains_file).

-spec spam_dump_file(gen_mod:opts() | global | binary()) -> boolean() | binary().
spam_dump_file(Opts) when is_map(Opts) ->
    gen_mod:get_opt(spam_dump_file, Opts);
spam_dump_file(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, spam_dump_file).

-spec spam_jids_file(gen_mod:opts() | global | binary()) -> 'none' | binary().
spam_jids_file(Opts) when is_map(Opts) ->
    gen_mod:get_opt(spam_jids_file, Opts);
spam_jids_file(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, spam_jids_file).

-spec spam_urls_file(gen_mod:opts() | global | binary()) -> 'none' | binary().
spam_urls_file(Opts) when is_map(Opts) ->
    gen_mod:get_opt(spam_urls_file, Opts);
spam_urls_file(Host) ->
    gen_mod:get_module_opt(Host, mod_antispam, spam_urls_file).

