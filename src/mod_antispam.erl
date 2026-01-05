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
-author('holger@zedat.fu-berlin.de').
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

-export([get_rtbl_services_option/1]).

%% ejabberd_commands callbacks.
-export([add_blocked_domain/2,
	 add_to_spam_filter_cache/2,
	 drop_from_spam_filter_cache/2,
	 expire_spam_filter_cache/2,
	 get_blocked_domains/1,
	 get_commands_spec/0,
	 get_spam_filter_cache/1,
	 reload_spam_filter_files/1,
	 remove_blocked_domain/2]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include("mod_antispam.hrl").
-include("translate.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-record(state,
	{host = <<>>			:: binary(),
	 dump_fd = undefined		:: file:io_device() | undefined,
	 url_set = sets:new()		:: url_set(),
	 jid_set = sets:new()		:: jid_set(),
	 jid_cache = #{}		:: map(),
	 max_cache_size = 0		:: non_neg_integer() | unlimited,
	 rtbl_host = none		:: binary() | none,
	 rtbl_subscribed = false	:: boolean(),
	 rtbl_retry_timer = undefined	:: reference() | undefined,
	 rtbl_domains_node		:: binary(),
	 blocked_domains = #{}		:: #{binary() => any()},
	 whitelist_domains = #{}	:: #{binary() => false}
	}).

-type state() :: #state{}.

-define(COMMAND_TIMEOUT, timer:seconds(30)).
-define(DEFAULT_CACHE_SIZE, 10000).

%% @format-begin

%%--------------------------------------------------------------------
%%| gen_mod callbacks

-spec start(binary(), gen_mod:opts()) -> ok | {error, any()}.
start(Host, Opts) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:register_commands(?MODULE, get_commands_spec());
        true ->
            ok
    end,
    gen_mod:start_child(?MODULE, Host, Opts).

-spec prep_stop(binary()) -> ok | {error, any()}.
prep_stop(Host) ->
    case try_call_by_host(Host, prepare_stop) of
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
    gen_mod:stop_child(?MODULE, Host).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
    ?DEBUG("reloading", []),
    Proc = get_proc_name(Host),
    gen_server:cast(Proc, {reload, NewOpts, OldOpts}).

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_pubsub, soft}].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(access_spam) ->
    econf:acl();
mod_opt_type(cache_size) ->
    econf:pos_int(unlimited);
mod_opt_type(rtbl_services) ->
    econf:list(
        econf:either(
            econf:binary(),
            econf:map(
                econf:binary(),
                econf:map(
                    econf:enum([spam_source_domains_node]), econf:binary()))));
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
        econf:enum([none]), econf:file());
mod_opt_type(whitelist_domains_file) ->
    econf:either(
        econf:enum([none]), econf:file()).

-spec mod_options(binary()) -> [{rtbl_services, [tuple()]} | {atom(), any()}].
mod_options(_Host) ->
    [{access_spam, none},
     {cache_size, ?DEFAULT_CACHE_SIZE},
     {rtbl_services, []},
     {spam_domains_file, none},
     {spam_dump_file, false},
     {spam_jids_file, none},
     {spam_urls_file, none},
     {whitelist_domains_file, none}].

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
           {cache_size,
            #{value => "pos_integer()",
              desc =>
                  ?T("Maximum number of JIDs that will be cached due to sending spam URLs. "
                     "If that limit is exceeded, the least recently used "
                     "entries are removed from the cache. "
                     "Setting this option to '0' disables the caching feature. "
                     "Note that separate caches are used for each virtual host, "
                     " and that the caches aren't distributed across cluster nodes. "
                     "The default value is '10000'.")}},
           {rtbl_services,
            #{value => ?T("[Service]"),
              example =>
                  ["rtbl_services:",
                   "  - pubsub.server1.localhost:",
                   "      spam_source_domains_node: actual_custom_pubsub_node"],
              desc =>
                  ?T("Query a RTBL service to get domains to block, as provided by "
                     "https://xmppbl.org/[xmppbl.org]. "
                     "Please note right now this option only supports one service in that list. "
                     "For blocking spam and abuse on MUC channels, please use _`mod_muc_rtbl`_ for now. "
                     "If only the host is provided, the default node names will be assumed. "
                     "If the node name is different than 'spam_source_domains', "
                     "you can setup the custom node name with the option 'spam_source_domains_node'. "
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
                     "The default value is 'none'.")}},
           {whitelist_domains_file,
            #{value => ?T("none | Path"),
              desc =>
                  ?T("Path to a file containing a list of "
                     "domains to whitelist from being blocked, one per line. "
                     "If either it is in 'spam_domains_file' or more realistically "
                     "in a domain sent by a RTBL host (see option 'rtbl_services') "
                     "then this domain will be ignored and stanzas from there won't be blocked. "
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
    FilesResults = read_files(Host),
    #{jid := JIDsSet,
      url := URLsSet,
      domains := SpamDomainsSet,
      whitelist_domains := WhitelistDomains} =
        FilesResults,
    ejabberd_hooks:add(local_send_to_resource_hook,
                       Host,
                       mod_antispam_rtbl,
                       pubsub_event_handler,
                       50),
    [#rtbl_service{host = RTBLHost, node = RTBLDomainsNode}] = get_rtbl_services_option(Opts),
    mod_antispam_filter:init_filtering(Host),
    InitState =
        #state{host = Host,
               jid_set = JIDsSet,
               url_set = URLsSet,
               dump_fd = mod_antispam_dump:init_dumping(Host),
               max_cache_size = gen_mod:get_opt(cache_size, Opts),
               blocked_domains = set_to_map(SpamDomainsSet),
               whitelist_domains = set_to_map(WhitelistDomains, false),
               rtbl_host = RTBLHost,
               rtbl_domains_node = RTBLDomainsNode},
    mod_antispam_rtbl:request_blocked_domains(RTBLHost, RTBLDomainsNode, Host),
    {ok, InitState}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, {spam_filter, term()}, state()} | {noreply, state()}.
handle_call({check_jid, From}, _From, #state{jid_set = JIDsSet} = State) ->
    {Result, State1} = filter_jid(From, JIDsSet, State),
    {reply, {spam_filter, Result}, State1};
handle_call({check_body, URLs, JIDs, From},
            _From,
            #state{url_set = URLsSet, jid_set = JIDsSet} = State) ->
    {Result1, State1} = filter_body(URLs, URLsSet, From, State),
    {Result2, State2} = filter_body(JIDs, JIDsSet, From, State1),
    Result =
        if Result1 == spam ->
               Result1;
           true ->
               Result2
        end,
    {reply, {spam_filter, Result}, State2};
handle_call(reload_spam_files, _From, State) ->
    {Result, State1} = reload_files(State),
    {reply, {spam_filter, Result}, State1};
handle_call({expire_cache, Age}, _From, State) ->
    {Result, State1} = expire_cache(Age, State),
    {reply, {spam_filter, Result}, State1};
handle_call({add_to_cache, JID}, _From, State) ->
    {Result, State1} = add_to_cache(JID, State),
    {reply, {spam_filter, Result}, State1};
handle_call({drop_from_cache, JID}, _From, State) ->
    {Result, State1} = drop_from_cache(JID, State),
    {reply, {spam_filter, Result}, State1};
handle_call(get_cache, _From, #state{jid_cache = Cache} = State) ->
    {reply, {spam_filter, maps:to_list(Cache)}, State};
handle_call({add_blocked_domain, Domain},
            _From,
            #state{blocked_domains = BlockedDomains} = State) ->
    BlockedDomains1 = maps:merge(BlockedDomains, #{Domain => true}),
    Txt = format("~s added to blocked domains", [Domain]),
    {reply, {spam_filter, {ok, Txt}}, State#state{blocked_domains = BlockedDomains1}};
handle_call({remove_blocked_domain, Domain},
            _From,
            #state{blocked_domains = BlockedDomains} = State) ->
    BlockedDomains1 = maps:remove(Domain, BlockedDomains),
    Txt = format("~s removed from blocked domains", [Domain]),
    {reply, {spam_filter, {ok, Txt}}, State#state{blocked_domains = BlockedDomains1}};
handle_call(get_blocked_domains,
            _From,
            #state{blocked_domains = BlockedDomains, whitelist_domains = WhitelistDomains} =
                State) ->
    {reply, {blocked_domains, maps:merge(BlockedDomains, WhitelistDomains)}, State};
handle_call({is_blocked_domain, Domain},
            _From,
            #state{blocked_domains = BlockedDomains, whitelist_domains = WhitelistDomains} =
                State) ->
    {reply,
     maps:get(Domain, maps:merge(BlockedDomains, WhitelistDomains), false) =/= false,
     State};
handle_call(prepare_stop,
            _From,
            #state{host = Host,
                   rtbl_host = RTBLHost,
                   rtbl_domains_node = RTBLDomainsNode} =
                State) ->
    mod_antispam_rtbl:unsubscribe(RTBLHost, RTBLDomainsNode, Host),
    {reply, ready_to_stop, State};
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
                   rtbl_host = OldRTBLHost,
                   rtbl_domains_node = OldRTBLDomainsNode,
                   rtbl_retry_timer = RTBLRetryTimer} =
                State) ->
    misc:cancel_timer(RTBLRetryTimer),
    State1 =
        State#state{dump_fd = mod_antispam_dump:reload_dumping(Host, Fd, OldOpts, NewOpts)},
    State2 =
        case {gen_mod:get_opt(cache_size, OldOpts), gen_mod:get_opt(cache_size, NewOpts)} of
            {OldMax, NewMax} when NewMax < OldMax ->
                shrink_cache(State1#state{max_cache_size = NewMax});
            {OldMax, NewMax} when NewMax > OldMax ->
                State1#state{max_cache_size = NewMax};
            {_OldMax, _NewMax} ->
                State1
        end,
    ok = mod_antispam_rtbl:unsubscribe(OldRTBLHost, OldRTBLDomainsNode, Host),
    {_Result, State3} = reload_files(State2#state{blocked_domains = #{}}),
    [#rtbl_service{host = RTBLHost, node = RTBLDomainsNode}] =
        get_rtbl_services_option(NewOpts),
    ok = mod_antispam_rtbl:request_blocked_domains(RTBLHost, RTBLDomainsNode, Host),
    {noreply, State3#state{rtbl_host = RTBLHost, rtbl_domains_node = RTBLDomainsNode}};
handle_cast({update_blocked_domains, NewItems},
            #state{blocked_domains = BlockedDomains} = State) ->
    {noreply, State#state{blocked_domains = maps:merge(BlockedDomains, NewItems)}};
handle_cast(Request, State) ->
    ?ERROR_MSG("Got unexpected request from: ~p", [Request]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({iq_reply, timeout, blocked_domains}, State) ->
    ?WARNING_MSG("Fetching blocked domains failed: fetch timeout. Retrying in 60 seconds",
                 []),
    {noreply,
     State#state{rtbl_retry_timer =
                     erlang:send_after(60000, self(), request_blocked_domains)}};
handle_info({iq_reply, #iq{type = error} = IQ, blocked_domains}, State) ->
    ?WARNING_MSG("Fetching blocked domains failed: ~p. Retrying in 60 seconds",
                 [xmpp:format_stanza_error(
                      xmpp:get_error(IQ))]),
    {noreply,
     State#state{rtbl_retry_timer =
                     erlang:send_after(60000, self(), request_blocked_domains)}};
handle_info({iq_reply, IQReply, blocked_domains},
            #state{blocked_domains = OldBlockedDomains,
                   rtbl_host = RTBLHost,
                   rtbl_domains_node = RTBLDomainsNode,
                   host = Host} =
                State) ->
    case mod_antispam_rtbl:parse_blocked_domains(IQReply) of
        undefined ->
            ?WARNING_MSG("Fetching initial list failed: invalid result payload", []),
            {noreply, State#state{rtbl_retry_timer = undefined}};
        NewBlockedDomains ->
            ok = mod_antispam_rtbl:subscribe(RTBLHost, RTBLDomainsNode, Host),
            {noreply,
             State#state{rtbl_retry_timer = undefined,
                         rtbl_subscribed = true,
                         blocked_domains = maps:merge(OldBlockedDomains, NewBlockedDomains)}}
    end;
handle_info({iq_reply, timeout, subscribe_result}, State) ->
    ?WARNING_MSG("Subscription error: request timeout", []),
    {noreply, State#state{rtbl_subscribed = false}};
handle_info({iq_reply, #iq{type = error} = IQ, subscribe_result}, State) ->
    ?WARNING_MSG("Subscription error: ~p",
                 [xmpp:format_stanza_error(
                      xmpp:get_error(IQ))]),
    {noreply, State#state{rtbl_subscribed = false}};
handle_info({iq_reply, IQReply, subscribe_result}, State) ->
    ?DEBUG("Got subscribe result: ~p", [IQReply]),
    {noreply, State#state{rtbl_subscribed = true}};
handle_info({iq_reply, _IQReply, unsubscribe_result}, State) ->
    %% FIXME: we should check it's true (of type `result`, not `error`), but at that point, what
    %% would we do?
    {noreply, State#state{rtbl_subscribed = false}};
handle_info(request_blocked_domains,
            #state{host = Host,
                   rtbl_host = RTBLHost,
                   rtbl_domains_node = RTBLDomainsNode} =
                State) ->
    mod_antispam_rtbl:request_blocked_domains(RTBLHost, RTBLDomainsNode, Host),
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason,
          #state{host = Host,
                 dump_fd = Fd,
                 rtbl_host = RTBLHost,
                 rtbl_domains_node = RTBLDomainsNode,
                 rtbl_retry_timer = RTBLRetryTimer} =
              _State) ->
    ?DEBUG("Stopping spam filter process for ~s: ~p", [Host, Reason]),
    misc:cancel_timer(RTBLRetryTimer),
    mod_antispam_dump:terminate_dumping(Host, Fd),
    mod_antispam_files:terminate_files(Host),
    mod_antispam_filter:terminate_filtering(Host),
    ejabberd_hooks:delete(local_send_to_resource_hook,
                          Host,
                          mod_antispam_rtbl,
                          pubsub_event_handler,
                          50),
    mod_antispam_rtbl:unsubscribe(RTBLHost, RTBLDomainsNode, Host),
    ok.

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{host = Host} = State, _Extra) ->
    ?DEBUG("Updating spam filter process for ~s", [Host]),
    {ok, State}.

%%--------------------------------------------------------------------
%%| Internal functions

-spec filter_jid(ljid(), jid_set(), state()) -> {ham | spam, state()}.
filter_jid(From, Set, #state{host = Host} = State) ->
    case sets:is_element(From, Set) of
        true ->
            ?DEBUG("Spam JID found: ~s", [jid:encode(From)]),
            ejabberd_hooks:run(spam_found, Host, [{jid, From}]),
            {spam, State};
        false ->
            case cache_lookup(From, State) of
                {true, State1} ->
                    ?DEBUG("Spam JID found: ~s", [jid:encode(From)]),
                    ejabberd_hooks:run(spam_found, Host, [{jid, From}]),
                    {spam, State1};
                {false, State1} ->
                    ?DEBUG("JID not listed: ~s", [jid:encode(From)]),
                    {ham, State1}
            end
    end.

-spec filter_body({urls, [url()]} | {jids, [ljid()]} | none,
                  url_set() | jid_set(),
                  jid(),
                  state()) ->
                     {ham | spam, state()}.
filter_body({_, Addrs}, Set, From, #state{host = Host} = State) ->
    case lists:any(fun(Addr) -> sets:is_element(Addr, Set) end, Addrs) of
        true ->
            ?DEBUG("Spam addresses found: ~p", [Addrs]),
            ejabberd_hooks:run(spam_found, Host, [{body, Addrs}]),
            {spam, cache_insert(From, State)};
        false ->
            ?DEBUG("Addresses not listed: ~p", [Addrs]),
            {ham, State}
    end;
filter_body(none, _Set, _From, State) ->
    {ham, State}.

-spec reload_files(state()) -> {ok | {error, binary()}, state()}.
reload_files(#state{host = Host, blocked_domains = BlockedDomains} = State) ->
    case read_files(Host) of
        #{jid := JIDsSet,
          url := URLsSet,
          domains := SpamDomainsSet,
          whitelist_domains := WhitelistDomains} ->
            case sets_equal(JIDsSet, State#state.jid_set) of
                true ->
                    ?INFO_MSG("Reloaded spam JIDs for ~s (unchanged)", [Host]);
                false ->
                    ?INFO_MSG("Reloaded spam JIDs for ~s (changed)", [Host])
            end,
            case sets_equal(URLsSet, State#state.url_set) of
                true ->
                    ?INFO_MSG("Reloaded spam URLs for ~s (unchanged)", [Host]);
                false ->
                    ?INFO_MSG("Reloaded spam URLs for ~s (changed)", [Host])
            end,
            {ok,
             State#state{jid_set = JIDsSet,
                         url_set = URLsSet,
                         blocked_domains = maps:merge(BlockedDomains, set_to_map(SpamDomainsSet)),
                         whitelist_domains = set_to_map(WhitelistDomains, false)}};
        {config_error, ErrorText} ->
            {{error, ErrorText}, State}
    end.

set_to_map(Set) ->
    set_to_map(Set, true).

set_to_map(Set, V) ->
    sets:fold(fun(K, M) -> M#{K => V} end, #{}, Set).

read_files(Host) ->
    AccInitial =
        #{jid => sets:new(),
          url => sets:new(),
          domains => sets:new(),
          whitelist_domains => sets:new()},
    Files =
        #{jid => gen_mod:get_module_opt(Host, ?MODULE, spam_jids_file),
          url => gen_mod:get_module_opt(Host, ?MODULE, spam_urls_file),
          domains => gen_mod:get_module_opt(Host, ?MODULE, spam_domains_file),
          whitelist_domains => gen_mod:get_module_opt(Host, ?MODULE, whitelist_domains_file)},
    ejabberd_hooks:run_fold(antispam_get_lists, Host, AccInitial, [Files]).

get_rtbl_services_option(Host) when is_binary(Host) ->
    get_rtbl_services_option(gen_mod:get_module_opts(Host, ?MODULE));
get_rtbl_services_option(Opts) when is_map(Opts) ->
    Services = gen_mod:get_opt(rtbl_services, Opts),
    case length(Services) =< 1 of
        true ->
            ok;
        false ->
            ?WARNING_MSG("Option rtbl_services only supports one service, but several "
                         "were configured. Will use only first one",
                         [])
    end,
    case Services of
        [] ->
            [#rtbl_service{}];
        [Host | _] when is_binary(Host) ->
            [#rtbl_service{host = Host, node = ?DEFAULT_RTBL_DOMAINS_NODE}];
        [[{Host, [{spam_source_domains_node, Node}]}] | _] ->
            [#rtbl_service{host = Host, node = Node}]
    end.

-spec get_proc_name(binary()) -> atom().
get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

-spec get_spam_filter_hosts() -> [binary()].
get_spam_filter_hosts() ->
    [H || H <- ejabberd_option:hosts(), gen_mod:is_loaded(H, ?MODULE)].

-spec sets_equal(sets:set(), sets:set()) -> boolean().
sets_equal(A, B) ->
    sets:is_subset(A, B) andalso sets:is_subset(B, A).

-spec format(io:format(), [term()]) -> binary().
format(Format, Data) ->
    iolist_to_binary(io_lib:format(Format, Data)).

%%--------------------------------------------------------------------
%%| Caching

-spec cache_insert(ljid(), state()) -> state().
cache_insert(_LJID, #state{max_cache_size = 0} = State) ->
    State;
cache_insert(LJID, #state{jid_cache = Cache, max_cache_size = MaxSize} = State)
    when MaxSize /= unlimited, map_size(Cache) >= MaxSize ->
    cache_insert(LJID, shrink_cache(State));
cache_insert(LJID, #state{jid_cache = Cache} = State) ->
    ?INFO_MSG("Caching spam JID: ~s", [jid:encode(LJID)]),
    Cache1 = Cache#{LJID => erlang:monotonic_time(second)},
    State#state{jid_cache = Cache1}.

-spec cache_lookup(ljid(), state()) -> {boolean(), state()}.
cache_lookup(LJID, #state{jid_cache = Cache} = State) ->
    case Cache of
        #{LJID := _Timestamp} ->
            Cache1 = Cache#{LJID => erlang:monotonic_time(second)},
            State1 = State#state{jid_cache = Cache1},
            {true, State1};
        #{} ->
            {false, State}
    end.

-spec shrink_cache(state()) -> state().
shrink_cache(#state{jid_cache = Cache, max_cache_size = MaxSize} = State) ->
    ShrinkedSize = round(MaxSize / 2),
    N = map_size(Cache) - ShrinkedSize,
    L = lists:keysort(2, maps:to_list(Cache)),
    Cache1 =
        maps:from_list(
            lists:nthtail(N, L)),
    State#state{jid_cache = Cache1}.

-spec expire_cache(integer(), state()) -> {{ok, binary()}, state()}.
expire_cache(Age, #state{jid_cache = Cache} = State) ->
    Threshold = erlang:monotonic_time(second) - Age,
    Cache1 = maps:filter(fun(_, TS) -> TS >= Threshold end, Cache),
    NumExp = map_size(Cache) - map_size(Cache1),
    Txt = format("Expired ~B cache entries", [NumExp]),
    {{ok, Txt}, State#state{jid_cache = Cache1}}.

-spec add_to_cache(ljid(), state()) -> {{ok, binary()}, state()}.
add_to_cache(LJID, State) ->
    State1 = cache_insert(LJID, State),
    Txt = format("~s added to cache", [jid:encode(LJID)]),
    {{ok, Txt}, State1}.

-spec drop_from_cache(ljid(), state()) -> {{ok, binary()}, state()}.
drop_from_cache(LJID, #state{jid_cache = Cache} = State) ->
    Cache1 = maps:remove(LJID, Cache),
    if map_size(Cache1) < map_size(Cache) ->
           Txt = format("~s removed from cache", [jid:encode(LJID)]),
           {{ok, Txt}, State#state{jid_cache = Cache1}};
       true ->
           Txt = format("~s wasn't cached", [jid:encode(LJID)]),
           {{ok, Txt}, State}
    end.

%%--------------------------------------------------------------------
%%| ejabberd command callbacks

-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = reload_spam_filter_files,
                        tags = [spam],
                        desc = "Reload spam JID/URL files",
                        module = ?MODULE,
                        function = reload_spam_filter_files,
                        note = "added in 25.07",
                        args = [{host, binary}],
                        result = {res, rescode}},
     #ejabberd_commands{name = get_spam_filter_cache,
                        tags = [spam],
                        desc = "Show spam filter cache contents",
                        module = ?MODULE,
                        function = get_spam_filter_cache,
                        note = "added in 25.07",
                        args = [{host, binary}],
                        result =
                            {spammers,
                             {list, {spammer, {tuple, [{jid, string}, {timestamp, integer}]}}}}},
     #ejabberd_commands{name = expire_spam_filter_cache,
                        tags = [spam],
                        desc = "Remove old/unused spam JIDs from cache",
                        module = ?MODULE,
                        function = expire_spam_filter_cache,
                        note = "added in 25.07",
                        args = [{host, binary}, {seconds, integer}],
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
     #ejabberd_commands{name = add_blocked_domain,
                        tags = [spam],
                        desc = "Add domain to list of blocked domains",
                        module = ?MODULE,
                        function = add_blocked_domain,
                        note = "added in 25.07",
                        args = [{host, binary}, {domain, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = remove_blocked_domain,
                        tags = [spam],
                        desc = "Remove domain from list of blocked domains",
                        module = ?MODULE,
                        function = remove_blocked_domain,
                        note = "added in 25.07",
                        args = [{host, binary}, {domain, binary}],
                        result = {res, restuple}}].

for_all_hosts(F, A) ->
    try lists:map(fun(Host) -> apply(F, [Host | A]) end, get_spam_filter_hosts()) of
        List ->
            case lists:filter(fun ({error, _}) ->
                                      true;
                                  (_) ->
                                      false
                              end,
                              List)
            of
                [] ->
                    hd(List);
                Errors ->
                    hd(Errors)
            end
    catch
        error:{badmatch, {error, _Reason} = Error} ->
            Error
    end.

try_call_by_host(Host, Call) ->
    LServer = jid:nameprep(Host),
    Proc = get_proc_name(LServer),
    try gen_server:call(Proc, Call, ?COMMAND_TIMEOUT) of
        Result ->
            Result
    catch
        exit:{noproc, _} ->
            {error, "Not configured for " ++ binary_to_list(Host)};
        exit:{timeout, _} ->
            {error, "Timeout while querying ejabberd"}
    end.

-spec reload_spam_filter_files(binary()) -> ok | {error, string()}.
reload_spam_filter_files(<<"global">>) ->
    for_all_hosts(fun reload_spam_filter_files/1, []);
reload_spam_filter_files(Host) ->
    case try_call_by_host(Host, reload_spam_files) of
        {spam_filter, ok} ->
            ok;
        {spam_filter, {error, Txt}} ->
            {error, Txt};
        {error, _R} = Error ->
            Error
    end.

-spec get_blocked_domains(binary()) -> [binary()].
get_blocked_domains(Host) ->
    case try_call_by_host(Host, get_blocked_domains) of
        {blocked_domains, BlockedDomains} ->
            maps:keys(
                maps:filter(fun (_, false) ->
                                    false;
                                (_, _) ->
                                    true
                            end,
                            BlockedDomains));
        {error, _R} = Error ->
            Error
    end.

-spec add_blocked_domain(binary(), binary()) -> {ok, string()}.
add_blocked_domain(<<"global">>, Domain) ->
    for_all_hosts(fun add_blocked_domain/2, [Domain]);
add_blocked_domain(Host, Domain) ->
    case try_call_by_host(Host, {add_blocked_domain, Domain}) of
        {spam_filter, {Status, Txt}} ->
            {Status, binary_to_list(Txt)};
        {error, _R} = Error ->
            Error
    end.

-spec remove_blocked_domain(binary(), binary()) -> {ok, string()}.
remove_blocked_domain(<<"global">>, Domain) ->
    for_all_hosts(fun remove_blocked_domain/2, [Domain]);
remove_blocked_domain(Host, Domain) ->
    case try_call_by_host(Host, {remove_blocked_domain, Domain}) of
        {spam_filter, {Status, Txt}} ->
            {Status, binary_to_list(Txt)};
        {error, _R} = Error ->
            Error
    end.

-spec get_spam_filter_cache(binary()) -> [{binary(), integer()}] | {error, string()}.
get_spam_filter_cache(Host) ->
    case try_call_by_host(Host, get_cache) of
        {spam_filter, Cache} ->
            [{jid:encode(JID), TS + erlang:time_offset(second)} || {JID, TS} <- Cache];
        {error, _R} = Error ->
            Error
    end.

-spec expire_spam_filter_cache(binary(), integer()) -> {ok | error, string()}.
expire_spam_filter_cache(<<"global">>, Age) ->
    for_all_hosts(fun expire_spam_filter_cache/2, [Age]);
expire_spam_filter_cache(Host, Age) ->
    case try_call_by_host(Host, {expire_cache, Age}) of
        {spam_filter, {Status, Txt}} ->
            {Status, binary_to_list(Txt)};
        {error, _R} = Error ->
            Error
    end.

-spec add_to_spam_filter_cache(binary(), binary()) ->
                                  [{binary(), integer()}] | {error, string()}.
add_to_spam_filter_cache(<<"global">>, JID) ->
    for_all_hosts(fun add_to_spam_filter_cache/2, [JID]);
add_to_spam_filter_cache(Host, EncJID) ->
    try jid:decode(EncJID) of
        #jid{} = JID ->
            LJID =
                jid:remove_resource(
                    jid:tolower(JID)),
            case try_call_by_host(Host, {add_to_cache, LJID}) of
                {spam_filter, {Status, Txt}} ->
                    {Status, binary_to_list(Txt)};
                {error, _R} = Error ->
                    Error
            end
    catch
        _:{bad_jid, _} ->
            {error, "Not a valid JID: " ++ binary_to_list(EncJID)}
    end.

-spec drop_from_spam_filter_cache(binary(), binary()) -> {ok | error, string()}.
drop_from_spam_filter_cache(<<"global">>, JID) ->
    for_all_hosts(fun drop_from_spam_filter_cache/2, [JID]);
drop_from_spam_filter_cache(Host, EncJID) ->
    try jid:decode(EncJID) of
        #jid{} = JID ->
            LJID =
                jid:remove_resource(
                    jid:tolower(JID)),
            case try_call_by_host(Host, {drop_from_cache, LJID}) of
                {spam_filter, {Status, Txt}} ->
                    {Status, binary_to_list(Txt)};
                {error, _R} = Error ->
                    Error
            end
    catch
        _:{bad_jid, _} ->
            {error, "Not a valid JID: " ++ binary_to_list(EncJID)}
    end.

%%--------------------------------------------------------------------

%%| vim: set foldmethod=marker foldmarker=%%|,%%-:
