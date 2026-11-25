%%%----------------------------------------------------------------------
%%% File    : mod_antispam.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Filter spam messages based on sender JID and content
%%% Created : 31 Mar 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2026 ProcessOne
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
%%%
%%%----------------------------------------------------------------------

%%| definitions

-module(mod_antispam).
-author('stefan@strigler.de').

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2,
         prep_stop/1,
         stop/1,
         reload/3,
         depends/2,
         mod_doc/0,
         mod_opt_type/1,
         mod_options/1]).

%% gen_server callbacks.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% ejabberd_commands callbacks.
-export([add_blocked_domain/2,
         add_muc_ban/2,
         add_to_spam_filter_cache/2,
         drop_from_spam_filter_cache/2,
         get_blocked_domains/1,
         get_commands_spec/0,
         get_spam_filter_cache/1,
         reload_spam_filter_files/1,
         remove_blocked_domain/2,
         remove_muc_ban/2]).

-export([is_blocked_domain/2,
         update_blocked_items/5,
         cache_tab/1]).

-import(mod_antispam_util, [try_call_by_host/3, for_all_hosts/2, sets_equal/2, format/2, format_s/2]).
-import(mod_antispam_rtbl, [get_rtbl_service_node/3,
                            get_rtbl_services_option/1,
                            set_retry_timer/4,
                            set_rtbl_service_node/4,
                            to_rtbl_service/2,
                            to_rtbl_service_node/2]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include("mod_antispam.hrl").
-include("translate.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-record(state,
        {host = <<>>           :: binary(),
         dump_fd = undefined   :: file:io_device() | undefined,
         rtbl_services = #{}   :: rtbl_services()
        }).

-type state() :: #state{}.

-define(DEFAULT_CACHE_LIFETIME, 5*86400).
-define(DEFAULT_CACHE_SIZE, 10000).

%% @format-begin

%%--------------------------------------------------------------------
%%| gen_mod callbacks

-spec start(binary(), gen_mod:opts()) -> {ok, [gen_mod:registration()]}.
start(Host, Opts) ->
    {ok, _Pid} = gen_mod:start_child(?MODULE, Host, Opts),
    {ok, _Pid2} = mod_antispam_report:start(Host, Opts),
    ok = mod_antispam_rtbl_muc:start(Host),
    {ok,
     [{hook, spam_reported, mod_antispam_report, spam_reported, 50},
      {commands, get_commands_spec()}]}.

-spec prep_stop(binary()) -> ok | {error, any()}.
prep_stop(Host) ->
    case try_call_by_host(Host, ?MODULE, prepare_stop) of
        ready_to_stop ->
            ok
    end.

-spec stop(binary()) -> ok | {error, any()}.
stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(get_commands_spec());
        true ->
            ok
    end,
    mod_antispam_rtbl_muc:stop(Host),
    mod_antispam_report:stop(Host),
    gen_mod:stop_child(?MODULE, Host).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
    ?DEBUG("reloading", []),
    Proc = gen_mod:get_module_proc_check(Host, ?MODULE),
    gen_server:cast(Proc, {reload, NewOpts, OldOpts}).

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_pubsub, soft}].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(access_spam) ->
    econf:acl();
mod_opt_type(allow_domains_file) ->
    econf:either(
        econf:enum([none]), econf:file());
mod_opt_type(cache_lifetime) ->
    econf:pos_int(infinity);
mod_opt_type(cache_size) ->
    econf:pos_int(infinity);
mod_opt_type(forward_reports) ->
    econf:bool();
mod_opt_type(forward_reports_to_third_party) ->
    econf:list(
        econf:jid());
mod_opt_type(rtbl_multi_host_policy) ->
    econf:enum([all, one]);
mod_opt_type(rtbl_services) ->
    econf:list(
        econf:either(
            econf:binary(),
            econf:map(
                econf:binary(),
                econf:map(
                    econf:enum([spam_source_domains_node, muc_bans_node]), econf:binary()))));
mod_opt_type(spam_domains_file) ->
    econf:either(
        econf:enum([none]), econf:file());
mod_opt_type(spam_dump_file) ->
    econf:either(
        econf:bool(), econf:file(write));
mod_opt_type(spam_jids_file) ->
    econf:either(
        econf:enum([none]), econf:file());
mod_opt_type(spam_urls_file) ->
    econf:either(
        econf:enum([none]), econf:file()).

-spec mod_options(binary()) -> [{rtbl_services, [tuple()]} | {atom(), any()}].
mod_options(_Host) ->
    [{access_spam, none},
     {allow_domains_file, none},
     {cache_lifetime, ?DEFAULT_CACHE_LIFETIME},
     {cache_size, ?DEFAULT_CACHE_SIZE},
     {forward_reports, true},
     {forward_reports_to_third_party, []},
     {rtbl_multi_host_policy, one},
     {rtbl_services, []},
     {spam_domains_file, none},
     {spam_dump_file, false},
     {spam_jids_file, none},
     {spam_urls_file, none}].

mod_doc() ->
    #{desc =>
          ?T("Filter spam messages and subscription requests received from "
             "remote servers based on "
             "https://xmppbl.org/[Real-Time Block Lists (RTBL)], "
             "lists of known spammer JIDs and/or URLs mentioned in spam messages. "
             "Traffic classified as spam is rejected with an error "
             "(and an '[info]' message is logged) unless the sender "
             "is subscribed to the recipient's presence."),
      note => "added in 25.07",
      opts =>
          [{access_spam,
            #{value => ?T("Access"),
              desc =>
                  ?T("Access rule that controls what accounts may receive spam messages. "
                     "If the rule returns 'allow' for a given recipient, "
                     "spam messages aren't rejected for that recipient. "
                     "The default value is 'none', which means that all recipients "
                     "are subject to spam filtering verification.")}},
           {allow_domains_file,
            #{value => ?T("none | Path"),
              desc =>
                  ?T("Path to a file containing a list of "
                     "domains to remove from list of blocked domains, one per line. "
                     "If either it is in 'spam_domains_file' or more realistically "
                     "in a domain sent by a RTBL host (see option 'rtbl_services') "
                     "then this domain will be ignored and stanzas from there won't be blocked. "
                     "Use an absolute path, or the '@CONFIG_PATH@' "
                     "https://docs.ejabberd.im/admin/configuration/file-format/#predefined-keywords[predefined keyword] "
                     "if the file is available in the configuration directory. "
                     "The default value is 'none'.")}},
           {cache_lifetime,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Lifetime of cache entries for jids cache in seconds, i.e. how long entries are kept in cache before they expire."
                     "Default is '5*86400' (5 days).")}},
           {cache_size,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Maximum number of JIDs that will be cached due to sending spam URLs. "
                     "If that limit is exceeded, the least recently used "
                     "entries are removed from the cache. "
                     "The default value is '10000'.")}},
           {forward_reports,
            #{value => "boolean()",
              desc =>
                  ?T("Whether to forward spam and abuse reports to the originating server, if "
                     "allowed by reporter. Default is 'true'")}},
           {forward_reports_to_third_party,
            #{value => ?T("[Service]"),
              desc =>
                  ?T("List of third-party services to forward spam and abuse reports to, if allowed"
                     " by reporter. Default is '[]'.")}},
           {rtbl_multi_host_policy,
            #{value => "all | one",
              desc =>
                  ?T("If there's more than one RTBL service with the same node type configured, "
                     "setting this to 'all' will require that an entry is being blocked if all "
                     "services publish it rather than just one. Default is 'one'.")}},
           {rtbl_services,
            #{value => ?T("[Service]"),
              example =>
                  ["rtbl_services:",
                   "  - pubsub.server1.localhost:",
                   "      spam_source_domains_node: some_custom_pubsub_node"
                   "      muc_bans_node: some_other_pubsub_node"],
              desc =>
                  ?T("Query a RTBL service to get domains and MUC jids to block, as provided by "
                     "https://xmppbl.org/[xmppbl.org]. "
                     "If only the host is provided, the default node names ('spam_source_domains' "
                     "and 'muc_bans_sha256' as found on 'xmppbl.org') will be assumed."
                     "For using custom node name, e.g. different than 'spam_source_domains', "
                     "you can setup the custom node name with the options 'spam_source_domains_node' or 'muc_bans_node'. In that case you have to set a value for both nodes though, unless you want to disable one of the features. Setting a node's value to 'none' disables this feauture too."
                     "The default value is an empty list of services.")}},
           {spam_domains_file,
            #{value => ?T("none | Path"),
              desc =>
                  ?T("Path to a plain text file containing a list of "
                     "known spam domains, one domain per line. "
                     "Messages and subscription requests sent from one of the listed domains "
                     "are classified as spam if sender is not in recipient's roster. "
                     "This list of domains gets merged with the one retrieved "
                     "by an RTBL host if any given. "
                     "Use an absolute path, or the '@CONFIG_PATH@' "
                     "https://docs.ejabberd.im/admin/configuration/file-format/#predefined-keywords[predefined keyword] "
                     "if the file is available in the configuration directory. "
                     "The default value is 'none'.")}},
           {spam_dump_file,
            #{value => ?T("false | true | Path"),
              desc =>
                  ?T("Path to the file to store blocked messages. "
                     "Use an absolute path, or the '@LOG_PATH@' "
                     "https://docs.ejabberd.im/admin/configuration/file-format/#predefined-keywords[predefined keyword] "
                     "to store logs "
                     "in the same place that the other ejabberd log files. "
                     "If set to 'false', it doesn't dump stanzas, which is the default. "
                     "If set to 'true', it stores in '\"@LOG_PATH@/spam_dump_@HOST@.log\"'.")}},
           {spam_jids_file,
            #{value => ?T("none | Path"),
              desc =>
                  ?T("Path to a plain text file containing a list of "
                     "known spammer JIDs, one JID per line. "
                     "Messages and subscription requests sent from one of "
                     "the listed JIDs are classified as spam. "
                     "Messages containing at least one of the listed JIDs"
                     "are classified as spam as well. "
                     "Furthermore, the sender's JID will be cached, "
                     "so that future traffic originating from that JID will also be classified as spam. "
                     "Use an absolute path, or the '@CONFIG_PATH@' "
                     "https://docs.ejabberd.im/admin/configuration/file-format/#predefined-keywords[predefined keyword] "
                     "if the file is available in the configuration directory. "
                     "The default value is 'none'.")}},
           {spam_urls_file,
            #{value => ?T("none | Path"),
              desc =>
                  ?T("Path to a plain text file containing a list of "
                     "URLs known to be mentioned in spam message bodies. "
                     "Messages containing at least one of the listed URLs are classified as spam. "
                     "Furthermore, the sender's JID will be cached, "
                     "so that future traffic originating from that JID will be classified as spam as well. "
                     "Use an absolute path, or the '@CONFIG_PATH@' "
                     "https://docs.ejabberd.im/admin/configuration/file-format/#predefined-keywords[predefined keyword] "
                     "if the file is available in the configuration directory. "
                     "The default value is 'none'.")}}],
      example =>
          ["modules:",
           "  mod_antispam:",
           "    rtbl_services:",
           "      - xmppbl.org",
           "    spam_jids_file: \"@CONFIG_PATH@/spam_jids.txt\"",
           "    spam_dump_file: \"@LOG_PATH@/spam/host-@HOST@.log\""]}.

%%--------------------------------------------------------------------
%%| gen_server callbacks

-spec init(list()) -> {ok, state()} | {stop, term()}.
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    mod_antispam_files:init_files(Host),

    #{jid := JIDsSet,
      url := URLsSet,
      domains := SpamDomainsSet,
      allow_domains := AllowDomainsSet} =
        read_files(Host),

    ets_cache:new(cache_tab(blocked_jids)),
    insert_many_for_host(cache_tab(blocked_jids),
                         Host,
                         set_to_map(JIDsSet, #{source => spam_jids_file})),

    ets_cache:new(cache_tab(blocked_urls)),
    insert_many_for_host(cache_tab(blocked_urls),
                         Host,
                         set_to_map(URLsSet, #{source => spam_urls_file})),

    %% the actual cache where we collect observed, actively sending jids to block
    ets_cache:new(cache_tab(jids_cache),
                  [{life_time, mod_antispam_opt:cache_lifetime(Opts) * 1000},
                   {max_size, mod_antispam_opt:cache_size(Opts)}]),

    ets_cache:new(cache_tab(blocked_items)),
    insert_many_for_host(cache_tab(blocked_items),
                         Host,
                         spam_source_domains,
                         set_to_map(SpamDomainsSet, #{spam_domains_file => true})),

    ets_cache:new(cache_tab(allow_domains)),
    insert_many_for_host(cache_tab(allow_domains),
                         Host,
                         set_to_map(AllowDomainsSet, #{source => allow_domains_file})),

    ejabberd_hooks:add(local_send_to_resource_hook,
                       Host,
                       mod_antispam_rtbl,
                       pubsub_event_handler,
                       50),
    mod_antispam_filter:init_filtering(Host),
    RTBLServices = get_rtbl_services_option(Opts),
    mod_antispam_rtbl:get_items(Host, RTBLServices),
    {ok,
     #state{host = Host,
            dump_fd = mod_antispam_dump:init_dumping(Host),
            rtbl_services = RTBLServices}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, {spam_filter, term()}, state()} | {noreply, state()}.
handle_call(reload_spam_files, _From, State) ->
    {Result, State1} = reload_files(State),
    {reply, {spam_filter, Result}, State1};
handle_call(prepare_stop,
            _From,
            #state{host = Host, rtbl_services = RTBLServices} = State) ->
    mod_antispam_rtbl:unsubscribe(Host, RTBLServices),
    {reply, ready_to_stop, State};
handle_call({insert_many_for_host, [Tab, Type, Items]},
            _From,
            #state{host = Host} = State) ->
    insert_many_for_host(Tab, Host, Type, Items),
    {reply, ok, State};
handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({dump_stanza, XML}, #state{dump_fd = Fd} = State) ->
    mod_antispam_dump:write_stanza_dump(Fd, XML),
    {noreply, State};
handle_cast(reopen_log, #state{host = Host, dump_fd = Fd} = State) ->
    {noreply, State#state{dump_fd = mod_antispam_dump:reopen_dump_file(Host, Fd)}};
handle_cast({reload, NewOpts, OldOpts},
            #state{host = Host,
                   dump_fd = Fd,
                   rtbl_services = OldRTBLServices} =
                State) ->
    State1 =
        State#state{dump_fd = mod_antispam_dump:reload_dumping(Host, Fd, OldOpts, NewOpts)},
    {_Result, State2} = reload_files(State1),
    MaxSize = mod_antispam_opt:cache_size(NewOpts),
    LifeTime = mod_antispam_opt:cache_lifetime(NewOpts) * 1000,
    ets_cache:setopts(cache_tab(jids_cache), [{max_size, MaxSize}, {life_time, LifeTime}]),
    mod_antispam_rtbl:cancel_timers(OldRTBLServices),
    ok = mod_antispam_rtbl:unsubscribe(Host, OldRTBLServices),
    ets_cache_clear(cache_tab(blocked_items), Host),
    RTBLServices = get_rtbl_services_option(NewOpts),
    ok = mod_antispam_rtbl:get_items(Host, RTBLServices),
    {noreply, State2#state{rtbl_services = RTBLServices}};
handle_cast(Request, State) ->
    ?ERROR_MSG("Got unexpected request from: ~p", [Request]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({iq_reply, timeout, {get_items, RTBLHost, RTBLNode}},
            #state{rtbl_services = RTBLServices} = State) ->
    ?WARNING_MSG("Fetching RTBL items failed: host=~s node=~s~nFetch timeout. Retrying in 60 seconds.",
                 [RTBLHost, RTBLNode]),
    {noreply,
     State#state{rtbl_services =
                     set_retry_timer(RTBLServices,
                                     RTBLHost,
                                     RTBLNode,
                                     erlang:send_after(60000, self(), request_items))}};
handle_info({iq_reply, #iq{type = error} = IQ, {get_items, RTBLHost, RTBLNode}},
            #state{rtbl_services = RTBLServices} = State) ->
    ?WARNING_MSG("Fetching blocked domains failed: ~p. Retrying in 60 seconds",
                 [xmpp:format_stanza_error(
                      xmpp:get_error(IQ))]),
    {noreply,
     State#state{rtbl_services =
                     set_retry_timer(RTBLServices,
                                     RTBLHost,
                                     RTBLNode,
                                     erlang:send_after(60000, self(), request_items))}};
handle_info({iq_reply, IQReply, {get_items, RTBLHost, RTBLNode}},
            #state{rtbl_services = RTBLServices, host = Host} = State) ->
    RTBLServiceNode = get_rtbl_service_node(RTBLServices, RTBLHost, RTBLNode),
    case mod_antispam_rtbl:parse_iq_result(IQReply,
                                           RTBLNode,
                                           RTBLServiceNode#rtbl_service_node.type)
    of
        undefined ->
            ?WARNING_MSG("Fetching initial list failed: invalid result payload", []),
            {noreply,
             State#state{rtbl_services =
                             set_retry_timer(RTBLServices, RTBLHost, RTBLNode, undefined)}};
        Items ->
            insert_many_for_host(cache_tab(blocked_items),
                                 Host,
                                 RTBLServiceNode#rtbl_service_node.type,
                                 set_source({RTBLHost, RTBLNode}, Items)),
            ok = mod_antispam_rtbl:subscribe(Host, RTBLHost, RTBLNode),
            {noreply,
             State#state{rtbl_services =
                             set_rtbl_service_node(RTBLServices,
                                                   RTBLHost,
                                                   RTBLNode,
                                                   RTBLServiceNode#rtbl_service_node{retry_timer =
                                                                                         undefined,
                                                                                     subscribed =
                                                                                         true})}}
    end;
handle_info({iq_reply, timeout, {subscribe_result, RTBLHost, RTBLNode}},
            #state{rtbl_services = RTBLServices} = State) ->
    ?WARNING_MSG("Subscription error: request timeout", []),
    RTBLServiceNode = get_rtbl_service_node(RTBLServices, RTBLHost, RTBLNode),
    {noreply,
     State#state{rtbl_services =
                     set_rtbl_service_node(RTBLServices,
                                           RTBLHost,
                                           RTBLNode,
                                           RTBLServiceNode#rtbl_service_node{subscribed = false})}};
handle_info({iq_reply, #iq{type = error} = IQ, {subscribe_result, RTBLHost, RTBLNode}},
            #state{rtbl_services = RTBLServices} = State) ->
    ?WARNING_MSG("Subscription error: ~p",
                 [xmpp:format_stanza_error(
                      xmpp:get_error(IQ))]),
    RTBLServiceNode = get_rtbl_service_node(RTBLServices, RTBLHost, RTBLNode),
    {noreply,
     State#state{rtbl_services =
                     set_rtbl_service_node(RTBLServices,
                                           RTBLHost,
                                           RTBLNode,
                                           RTBLServiceNode#rtbl_service_node{subscribed = false})}};
handle_info({iq_reply, IQReply, {subscribe_result, RTBLHost, RTBLNode}},
            #state{rtbl_services = RTBLServices} = State) ->
    ?DEBUG("Got subscribe result: ~p", [IQReply]),
    RTBLServiceNode = get_rtbl_service_node(RTBLServices, RTBLHost, RTBLNode),
    {noreply,
     State#state{rtbl_services =
                     set_rtbl_service_node(RTBLServices,
                                           RTBLHost,
                                           RTBLNode,
                                           RTBLServiceNode#rtbl_service_node{subscribed = true})}};
handle_info({iq_reply, _IQReply, {unsubscribe_result, RTBLHost, RTBLNode}},
            #state{rtbl_services = RTBLServices} = State) ->
    %% FIXME: we should check it's true (of type `result`, not `error`), but at that point, what
    %% would we do?
    RTBLServiceNode = get_rtbl_service_node(RTBLServices, RTBLHost, RTBLNode),
    {noreply,
     State#state{rtbl_services =
                     set_rtbl_service_node(RTBLServices,
                                           RTBLHost,
                                           RTBLNode,
                                           RTBLServiceNode#rtbl_service_node{subscribed = false})}};
handle_info(request_items, #state{host = Host, rtbl_services = RTBLServices} = State) ->
    mod_antispam_rtbl:get_items(Host, RTBLServices),
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason,
          #state{host = Host,
                 dump_fd = Fd,
                 rtbl_services = RTBLServices} =
              _State) ->
    ?DEBUG("Stopping spam filter process for ~s: ~p", [Host, Reason]),
    mod_antispam_rtbl:cancel_timers(RTBLServices),
    mod_antispam_dump:terminate_dumping(Host, Fd),
    mod_antispam_files:terminate_files(Host),
    mod_antispam_filter:terminate_filtering(Host),
    ejabberd_hooks:delete(local_send_to_resource_hook,
                          Host,
                          mod_antispam_rtbl,
                          pubsub_event_handler,
                          50),
    mod_antispam_rtbl:unsubscribe(Host, RTBLServices),
    ok.

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{host = Host} = State, _Extra) ->
    ?DEBUG("Updating spam filter process for ~s", [Host]),
    {ok, State}.

%%--------------------------------------------------------------------
%%| ejabberd command callbacks

-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = add_blocked_domain,
                        tags = [spam],
                        desc = "Add domain to list of blocked domains",
                        module = ?MODULE,
                        function = add_blocked_domain,
                        note = "added in 25.07",
                        args = [{host, binary}, {domain, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = add_muc_ban,
                        tags = [spam],
                        desc =
                            "Add JID to MUC bans, JID can be either in cleartext or as a sha256 hash.",
                        module = ?MODULE,
                        function = add_muc_ban,
                        note = "added in 26.12",
                        args = [{host, binary}, {jid, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = add_to_spam_filter_cache,
                        tags = [spam],
                        desc = "Add JID to spam filter cache",
                        module = ?MODULE,
                        function = add_to_spam_filter_cache,
                        note = "added in 25.07",
                        args = [{host, binary}, {jid, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = drop_from_spam_filter_cache,
                        tags = [spam],
                        desc = "Drop JID from spam filter cache",
                        module = ?MODULE,
                        function = drop_from_spam_filter_cache,
                        note = "added in 25.07",
                        args = [{host, binary}, {jid, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = get_blocked_domains,
                        tags = [spam],
                        desc = "Get list of domains being blocked",
                        module = ?MODULE,
                        function = get_blocked_domains,
                        note = "added in 25.07",
                        args = [{host, binary}],
                        result = {blocked_domains, {list, {jid, string}}}},
     #ejabberd_commands{name = get_spam_filter_cache,
                        tags = [spam],
                        desc = "Show spam filter cache contents",
                        module = ?MODULE,
                        function = get_spam_filter_cache,
                        note = "added in 25.07",
                        args = [{host, binary}],
                        result =
                            {spammers,
                             {list, {spammer, {tuple, [{jid, string}, {timestamp, string}]}}}}},
     #ejabberd_commands{name = reload_spam_filter_files,
                        tags = [spam],
                        desc = "Reload spam JID/URL files",
                        module = ?MODULE,
                        function = reload_spam_filter_files,
                        note = "added in 25.07",
                        args = [{host, binary}],
                        result = {res, rescode}},
     #ejabberd_commands{name = remove_blocked_domain,
                        tags = [spam],
                        desc = "Remove domain from list of blocked domains",
                        module = ?MODULE,
                        function = remove_blocked_domain,
                        note = "added in 25.07",
                        args = [{host, binary}, {domain, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = remove_muc_ban,
                        tags = [spam],
                        desc = "Remove domain from list of blocked domains",
                        module = ?MODULE,
                        function = remove_muc_ban,
                        note = "added in 26.12",
                        args = [{host, binary}, {jid, binary}],
                        result = {res, restuple}}].

-define(check_host(BLOCK),
        maybe
            true ?= is_host(Host),
            BLOCK
        else
            false ->
                {error, format_s("Host invalid or unknown: ~s", [binary_to_list(Host)])}
        end).

is_host(Host) ->
    try jid:nameprep(Host) of
        _ ->
            lists:member(Host, ejabberd_option:hosts())
    catch
        _:_ ->
            false
    end.

-spec reload_spam_filter_files(binary()) -> ok | {error, string()}.
reload_spam_filter_files(<<"global">>) ->
    for_all_hosts(fun reload_spam_filter_files/1, []);
reload_spam_filter_files(Host) ->
    ?check_host(begin
                    case try_call_by_host(Host, ?MODULE, reload_spam_files) of
                        {spam_filter, ok} ->
                            ok;
                        {spam_filter, {error, Txt}} ->
                            {error, Txt};
                        {error, _R} = Error ->
                            Error
                    end
                end).

-spec get_blocked_domains(binary()) -> [binary()].
get_blocked_domains(Host) ->
    ?check_host(begin
                    BlockedDomains =
                        ets:select(cache_tab(blocked_items),
                                   [{{{Host, '$1', spam_source_domains}, '_', '_'}, [], ['$1']}]),
                    AllowDomains =
                        ets:select(cache_tab(allow_domains),
                                   [{{{Host, '$1'}, '_', '_'}, [], ['$1']}]),
                    BlockedDomains -- AllowDomains
                end).

-spec add_blocked_domain(binary(), binary()) -> {ok, string()}.
add_blocked_domain(HostOrGlobal, Domain) ->
    add_blocked_item(HostOrGlobal, Domain, spam_source_domains).

-spec add_muc_ban(binary(), binary()) -> {ok, string()}.
add_muc_ban(HostOrGlobal, JID) ->
    add_blocked_item(HostOrGlobal, JID, muc_bans).

-spec add_blocked_item(binary(), binary(), atom()) -> {ok, string()}.
add_blocked_item(<<"global">>, Item, Type) ->
    for_all_hosts(fun add_blocked_item/3, [Item, Type]);
add_blocked_item(Host, Item, Type) ->
    ?check_host(
       begin
           Val = #{add_blocked_item => true},
           ets_upsert(cache_tab(blocked_items),
                      {Host, Item, Type},
                      Val,
                      ejabberd_cluster:get_nodes()),
           ejabberd_hooks:run(antispam_rtbl_items,
                              Host,
                              [Host, {Type, {[], #{Item => Val}}}]),
           Txt = format("~s added to blocked items of type ~p", [Item, Type]),
           {ok, Txt}
       end).

-spec remove_blocked_domain(binary(), binary()) -> {ok, string()}.
remove_blocked_domain(HostOrGlobal, Domain) ->
    remove_blocked_item(HostOrGlobal, Domain, spam_source_domains).

remove_muc_ban(HostOrGlobal, JID) ->
    remove_blocked_item(HostOrGlobal, JID, muc_bans).

-spec remove_blocked_item(binary(), binary(), atom()) -> {ok, string()}.
remove_blocked_item(<<"global">>, Item, Type) ->
    for_all_hosts(fun remove_blocked_item/3, [Item, Type]);
remove_blocked_item(Host, Item, Type) ->
    ?check_host(
       begin
           ets_cache:delete(cache_tab(blocked_items),
                            {Host, Item, Type},
                            ejabberd_cluster:get_nodes()),
           ejabberd_hooks:run(antispam_rtbl_items,
                              Host,
                              [Host, {Type, {[Item], #{}}}]),
           Txt = format("~s removed from blocked items of type ~p", [Item, Type]),
           {ok, Txt}
       end).

-spec get_spam_filter_cache(binary()) -> [{binary(), string()}] | {error, string()}.
get_spam_filter_cache(Host) ->
    ?check_host(begin
                    Cache =
                        ets:select(cache_tab(jids_cache),
                                   [{{{Host, '$1'}, '_', '$2'}, [], [{{'$1', '$2'}}]}]),
                    [{jid:encode(JID), encode_timestamp(TS)} || {JID, TS} <- Cache]
                end).

-spec encode_timestamp(TS :: integer()) -> string().
encode_timestamp(TS) ->
    encode_datetime(calendar:now_to_datetime({TS div 1000000000,
                                              TS rem 1000000000 div 1000,
                                              TS rem 1000 * 1000})).

-spec encode_datetime(calendar:datetime()) -> string().
encode_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Minute, Second]).

-spec add_to_spam_filter_cache(binary(), binary()) ->
                                  [{binary(), integer()}] | {error, string()}.
add_to_spam_filter_cache(<<"global">>, JID) ->
    for_all_hosts(fun add_to_spam_filter_cache/2, [JID]);
add_to_spam_filter_cache(Host, EncJID) ->
    ?check_host(begin
                    try jid:decode(EncJID) of
                        #jid{} = JID ->
                            LJID =
                                jid:remove_resource(
                                    jid:tolower(JID)),
                            case ets_cache:insert(cache_tab(jids_cache),
                                                  {Host, LJID},
                                                  #{source => cmd},
                                                  ejabberd_cluster:get_nodes())
                            of
                                true ->
                                    Txt = format("~s added to cache", [jid:encode(LJID)]),
                                    {ok, Txt};
                                false ->
                                    Txt = format_s("failed to add ~s to cache", [jid:encode(LJID)]),
                                    {error, Txt}
                            end
                    catch
                        _:{bad_jid, _} ->
                            {error, "Not a valid JID: " ++ binary_to_list(EncJID)}
                    end
                end).

-spec drop_from_spam_filter_cache(binary(), binary()) -> {ok | error, string()}.
drop_from_spam_filter_cache(<<"global">>, JID) ->
    for_all_hosts(fun drop_from_spam_filter_cache/2, [JID]);
drop_from_spam_filter_cache(Host, EncJID) ->
    ?check_host(begin
                    try jid:decode(EncJID) of
                        #jid{} = JID ->
                            LJID =
                                jid:remove_resource(
                                    jid:tolower(JID)),
                            case ets_cache:lookup(cache_tab(jids_cache), {Host, LJID}) of
                                error ->
                                    Txt = format_s("~s wasn't cached", [jid:encode(LJID)]),
                                    {error, Txt};
                                {ok, _} ->
                                    ets_cache:delete(cache_tab(jids_cache),
                                                     {Host, LJID},
                                                     ejabberd_cluster:get_nodes()),
                                    Txt = format("~s removed from cache", [jid:encode(LJID)]),
                                    {ok, Txt}
                            end
                    catch
                        _:{bad_jid, _} ->
                            {error, "Not a valid JID: " ++ binary_to_list(EncJID)}
                    end
                end).

%%--------------------------------------------------------------------
%%| API

is_blocked_domain(Host, Domain) ->
    maybe
        true ?= ets_cache:lookup(cache_tab(allow_domains), {Host, Domain}) =:= error,
        Res ?= ets_cache:lookup(cache_tab(blocked_items), {Host, Domain, spam_source_domains}),
        true ?= Res =/= error,
        Policy = mod_antispam_opt:rtbl_multi_host_policy(Host),
        false ?= Policy == one,
        {ok, Blocked} = Res,
        false ?= maps:filter(fun(K, _V) -> not is_tuple(K) end, Blocked) =/= #{},
        Services = get_rtbl_services_option(Host),
        Nodes =
            maps:fold(fun(K, V, A) ->
                         maps:fold(fun (K2, #rtbl_service_node{type = spam_source_domains}, A2) ->
                                           [{K, K2} | A2];
                                       (_, _, A2) ->
                                           A2
                                   end,
                                   A,
                                   V)
                      end,
                      [],
                      Services),
        Nodes
        -- maps:keys(
               maps:filter(fun(K, _V) -> is_tuple(K) end, Blocked))
        == []
    end.

update_blocked_items(Host, RTBLHost, RTBLNode, RTBLNodeType, {RetractIds, NewItems}) ->
    lists:foreach(fun(RetractId) ->
                     ets_cache:delete(cache_tab(blocked_items), {Host, RetractId, RTBLNodeType})
                  end,
                  RetractIds),
    safe_insert_many_for_host(cache_tab(blocked_items),
                              Host,
                              RTBLNodeType,
                              set_source({RTBLHost, RTBLNode}, NewItems)).

%%--------------------------------------------------------------------
%%| Internal functions

-spec reload_files(state()) -> {ok | {error, binary()}, state()}.
reload_files(#state{host = Host} = State) ->
    case read_files(Host) of
        #{jid := JIDsSet,
          url := URLsSet,
          domains := SpamDomainsSet,
          allow_domains := AllowDomainsSet} ->
            case sets_equal(JIDsSet, set_from_ets(cache_tab(blocked_jids), Host)) of
                true ->
                    ?INFO_MSG("Reloaded spam JIDs for ~s (unchanged)", [Host]);
                false ->
                    ?INFO_MSG("Reloaded spam JIDs for ~s (changed)", [Host])
            end,
            case sets_equal(URLsSet, set_from_ets(cache_tab(blocked_urls), Host)) of
                true ->
                    ?INFO_MSG("Reloaded spam URLs for ~s (unchanged)", [Host]);
                false ->
                    ?INFO_MSG("Reloaded spam URLs for ~s (changed)", [Host])
            end,
            insert_many_for_host(cache_tab(blocked_items),
                                 Host,
                                 spam_source_domains,
                                 set_to_map(SpamDomainsSet, #{source => spam_domains_file})),
            ets_cache_clear(cache_tab(allow_domains), Host),
            insert_many_for_host(cache_tab(allow_domains),
                                 Host,
                                 set_to_map(AllowDomainsSet, #{source => allow_domains_file})),
            {ok, State};
        {config_error, ErrorText} ->
            {{error, ErrorText}, State}
    end.

set_from_ets(Tab, Host) ->
    lists:foldl(fun sets:add_element/2,
                sets:new(),
                ets:select(Tab, [{{{Host, '$1'}, '_', '_'}, [], ['$1']}])).

set_to_map(Set, V) ->
    sets:fold(fun(K, M) -> M#{K => V} end, #{}, Set).

read_files(Host) ->
    AccInitial =
        #{jid => sets:new(),
          url => sets:new(),
          domains => sets:new(),
          allow_domains => sets:new()},
    Files =
        #{jid => mod_antispam_opt:spam_jids_file(Host),
          url => mod_antispam_opt:spam_urls_file(Host),
          domains => mod_antispam_opt:spam_domains_file(Host),
          allow_domains => mod_antispam_opt:allow_domains_file(Host)},
    ejabberd_hooks:run_fold(antispam_get_lists, Host, AccInitial, [Files]).

cache_tab(Name) ->
    %% elp:ignore W0023
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Name)).

inside_gen_server(Host) ->
    whereis(gen_mod:get_module_proc_check(Host, ?MODULE)) =:= self().

safe_insert_many_for_host(Tab, Host, Type, Items) ->
    %% If the pubsub node we're subscribing to is hosted on our local ejabberd processing of
    %% messages happens inside the gen_server itself from within handle_info({iq_result, ...}, _, _).
    %% I.e. mod_antispam_rtbl:pubsub_event_handler/2 is triggered from within.
    case inside_gen_server(Host) of
        true ->
            insert_many_for_host(Tab, Host, Type, Items);
        false ->
            %% This is needed because we do an upsert in insert_many_for_host/X, so if we handle inserts
            %% outside of the gen_server we need to make sure not overwrite concurrent inserts.
            case try_call_by_host(Host, ?MODULE, {insert_many_for_host, [Tab, Type, Items]}) of
                ok ->
                    ok;
                {error, _R} = Error ->
                    Error
            end
    end.

insert_many_for_host(Tab, Host, Type, Items) ->
    maps:foreach(ets_cache_upsert_fun(Tab, fun(K) -> {Host, K, Type} end), Items).

insert_many_for_host(Tab, Host, Items) ->
    maps:foreach(ets_cache_upsert_fun(Tab, fun(K) -> {Host, K} end), Items).

ets_cache_upsert_fun(Tab, KeyF) ->
    fun(K, V) -> ets_upsert(Tab, KeyF(K), V) end.

ets_upsert(Tab, Key, Val) ->
    ets_cache_upsert(Tab, Key, Val, [node()]).

ets_cache_upsert(Tab, Key, Val, Nodes) ->
    %% FIXME
    %% This is only necessary because while ets_cache allows setting a table
    %% type of 'bag' it doesn't actually support it.
    case ets_cache:lookup(Tab, Key) of
        {ok, OldVal} ->
            ets_cache:delete(Tab, Key, Nodes),
            ets_cache:insert(Tab, Key, maps:merge(OldVal, Val), Nodes);
        error ->
            ets_cache:insert(Tab, Key, Val, Nodes)
    end.

ets_cache_clear(Tab, Host) ->
    Keys = ets:select(Tab, [{{{Host, '$1'}, '_', '_'}, [], ['$1']}]),
    lists:foreach(fun(Key) -> ets:delete(Tab, Key) end, Keys).

set_source(Source, Items) ->
    maps:map(fun(_K, V) -> #{Source => V} end, Items).
