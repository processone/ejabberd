%%%----------------------------------------------------------------------
%%% File    : mod_antispam.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Filter spam messages based on sender JID and content
%%% Created : 31 Mar 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2025 ProcessOne
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

%% ejabberd_hooks callbacks.
-export([s2s_in_handle_info/2,
	 s2s_receive_packet/1,
	 sm_receive_packet/1]).

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
-include("translate.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-type url() :: binary().
-type filename() :: binary() | none | false.
-type jid_set() :: sets:set(ljid()).
-type url_set() :: sets:set(url()).
-type s2s_in_state() :: ejabberd_s2s_in:state().

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
-define(HTTPC_TIMEOUT, timer:seconds(3)).
-define(DEFAULT_RTBL_DOMAINS_NODE, <<"spam_source_domains">>).
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
mod_opt_type(spam_domains_file) ->
    econf:either(
        econf:enum([none]), econf:file());
mod_opt_type(whitelist_domains_file) ->
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
mod_opt_type(access_spam) ->
    econf:acl();
mod_opt_type(cache_size) ->
    econf:pos_int(unlimited);
mod_opt_type(rtbl_host) ->
    econf:either(
        econf:enum([none]), econf:host());
mod_opt_type(rtbl_domains_node) ->
    econf:non_empty(
        econf:binary()).

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_Host) ->
    [{spam_domains_file, none},
     {spam_dump_file, false},
     {spam_jids_file, none},
     {spam_urls_file, none},
     {whitelist_domains_file, none},
     {access_spam, none},
     {cache_size, ?DEFAULT_CACHE_SIZE},
     {rtbl_host, none},
     {rtbl_domains_node, ?DEFAULT_RTBL_DOMAINS_NODE}].

mod_doc() ->
    #{desc => ?T("Reads from text file and RTBL, filters stanzas and writes dump file."),
      note => "added in 25.xx",
      opts =>
          [{spam_dump_file,
            #{value => ?T("false | true | Path"),
              desc =>
                  ?T("Path to the file to store blocked messages. "
                     "Use an absolute path, or the '@LOG_PATH@' macro to store logs "
                     "in the same place that the other ejabberd log files. "
                     "If set to 'false', does not dump stanzas, this is the default. "
                     "If set to 'true', it stores in '\"@LOG_PATH@/spam_dump_@HOST@.log\"'.")}}],
      example =>
          ["modules:",
           "  mod_antispam:",
           "    spam_dump_file: \"@LOG_PATH@/spam/host-@HOST@.log\""]}.

%%--------------------------------------------------------------------
%%| gen_server callbacks

-spec init(list()) -> {ok, state()} | {stop, term()}.
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    Files =
        #{domains => gen_mod:get_opt(spam_domains_file, Opts),
          jid => gen_mod:get_opt(spam_jids_file, Opts),
          url => gen_mod:get_opt(spam_urls_file, Opts),
          whitelist_domains => gen_mod:get_opt(whitelist_domains_file, Opts)},
    try read_files(Files) of
        #{jid := JIDsSet,
          url := URLsSet,
          domains := SpamDomainsSet,
          whitelist_domains := WhitelistDomains} ->
            ejabberd_hooks:add(s2s_in_handle_info, Host, ?MODULE, s2s_in_handle_info, 90),
            ejabberd_hooks:add(s2s_receive_packet, Host, ?MODULE, s2s_receive_packet, 50),
            ejabberd_hooks:add(sm_receive_packet, Host, ?MODULE, sm_receive_packet, 50),
            ejabberd_hooks:add(local_send_to_resource_hook,
                               Host,
                               mod_antispam_rtbl,
                               pubsub_event_handler,
                               50),
            RTBLHost = gen_mod:get_opt(rtbl_host, Opts),
            RTBLDomainsNode = gen_mod:get_opt(rtbl_domains_node, Opts),
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
            {ok, InitState}
    catch
        {Op, File, Reason} when Op == open; Op == read ->
            ?CRITICAL_MSG("Cannot ~s ~s: ~s", [Op, File, format_error(Reason)]),
            {stop, config_error}
    end.

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
handle_call({resolve_redirects, URLs}, _From, State) ->
    ResolvedURLs = do_resolve_redirects(URLs, []),
    {reply, {spam_filter, ResolvedURLs}, State};
handle_call({reload_files, Files}, _From, State) ->
    {Result, State1} = reload_files(Files, State),
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
    Files =
        #{domains => gen_mod:get_opt(spam_domains_file, NewOpts),
          jid => gen_mod:get_opt(spam_jids_file, NewOpts),
          url => gen_mod:get_opt(spam_urls_file, NewOpts),
          whitelist_domains => gen_mod:get_opt(whitelist_domains_file, NewOpts)},
    {_Result, State3} = reload_files(Files, State2#state{blocked_domains = #{}}),
    RTBLHost = gen_mod:get_opt(rtbl_host, NewOpts),
    RTBLDomainsNode = gen_mod:get_opt(rtbl_domains_node, NewOpts),
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
    ejabberd_hooks:delete(s2s_receive_packet, Host, ?MODULE, s2s_receive_packet, 50),
    ejabberd_hooks:delete(sm_receive_packet, Host, ?MODULE, sm_receive_packet, 50),
    ejabberd_hooks:delete(s2s_in_handle_info, Host, ?MODULE, s2s_in_handle_info, 90),
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
%%| Hook callbacks

-spec s2s_receive_packet({stanza() | drop, s2s_in_state()}) ->
                            {stanza() | drop, s2s_in_state()} | {stop, {drop, s2s_in_state()}}.
s2s_receive_packet({A, State}) ->
    case sm_receive_packet(A) of
        {stop, drop} ->
            {stop, {drop, State}};
        Result ->
            {Result, State}
    end.

-spec sm_receive_packet(stanza() | drop) -> stanza() | drop | {stop, drop}.
sm_receive_packet(drop = Acc) ->
    Acc;
sm_receive_packet(#message{from = From,
                           to = #jid{lserver = LServer} = To,
                           type = Type} =
                      Msg)
    when Type /= groupchat, Type /= error ->
    do_check(From, To, LServer, Msg);
sm_receive_packet(#presence{from = From,
                            to = #jid{lserver = LServer} = To,
                            type = subscribe} =
                      Presence) ->
    do_check(From, To, LServer, Presence);
sm_receive_packet(Acc) ->
    Acc.

do_check(From, To, LServer, Stanza) ->
    case needs_checking(From, To) of
        true ->
            case check_from(LServer, From) of
                ham ->
                    case check_stanza(LServer, From, Stanza) of
                        ham ->
                            Stanza;
                        spam ->
                            reject(Stanza),
                            {stop, drop}
                    end;
                spam ->
                    reject(Stanza),
                    {stop, drop}
            end;
        false ->
            Stanza
    end.

check_stanza(LServer, From, #message{body = Body}) ->
    check_body(LServer, From, xmpp:get_text(Body));
check_stanza(_, _, _) ->
    ham.

-spec s2s_in_handle_info(s2s_in_state(), any()) ->
                            s2s_in_state() | {stop, s2s_in_state()}.
s2s_in_handle_info(State, {_Ref, {spam_filter, _}}) ->
    ?DEBUG("Dropping expired spam filter result", []),
    {stop, State};
s2s_in_handle_info(State, _) ->
    State.

%%--------------------------------------------------------------------
%%| Internal functions

-spec needs_checking(jid(), jid()) -> boolean().
needs_checking(#jid{lserver = FromHost} = From, #jid{lserver = LServer} = To) ->
    case gen_mod:is_loaded(LServer, ?MODULE) of
        true ->
            Access = gen_mod:get_module_opt(LServer, ?MODULE, access_spam),
            case acl:match_rule(LServer, Access, To) of
                allow ->
                    ?DEBUG("Spam not filtered for ~s", [jid:encode(To)]),
                    false;
                deny ->
                    ?DEBUG("Spam is filtered for ~s", [jid:encode(To)]),
                    not mod_roster:is_subscribed(From, To)
                    andalso not
                                mod_roster:is_subscribed(
                                    jid:make(<<>>, FromHost),
                                    To) % likely a gateway
            end;
        false ->
            ?DEBUG("~s not loaded for ~s", [?MODULE, LServer]),
            false
    end.

-spec check_from(binary(), jid()) -> ham | spam.
check_from(Host, From) ->
    Proc = get_proc_name(Host),
    LFrom =
        {_, FromDomain, _} =
            jid:remove_resource(
                jid:tolower(From)),
    try
        case gen_server:call(Proc, {is_blocked_domain, FromDomain}) of
            true ->
                ?DEBUG("Spam JID found in blocked domains: ~p", [From]),
                ejabberd_hooks:run(spam_found, Host, [{jid, From}]),
                spam;
            false ->
                case gen_server:call(Proc, {check_jid, LFrom}) of
                    {spam_filter, Result} ->
                        Result
                end
        end
    catch
        exit:{timeout, _} ->
            ?WARNING_MSG("Timeout while checking ~s against list of blocked domains or spammers",
                         [jid:encode(From)]),
            ham
    end.

-spec check_body(binary(), jid(), binary()) -> ham | spam.
check_body(Host, From, Body) ->
    case {extract_urls(Host, Body), extract_jids(Body)} of
        {none, none} ->
            ?DEBUG("No JIDs/URLs found in message", []),
            ham;
        {URLs, JIDs} ->
            Proc = get_proc_name(Host),
            LFrom =
                jid:remove_resource(
                    jid:tolower(From)),
            try gen_server:call(Proc, {check_body, URLs, JIDs, LFrom}) of
                {spam_filter, Result} ->
                    Result
            catch
                exit:{timeout, _} ->
                    ?WARNING_MSG("Timeout while checking body", []),
                    ham
            end
    end.

-spec extract_urls(binary(), binary()) -> {urls, [url()]} | none.
extract_urls(Host, Body) ->
    RE = <<"https?://\\S+">>,
    Options = [global, {capture, all, binary}],
    case re:run(Body, RE, Options) of
        {match, Captured} when is_list(Captured) ->
            Urls = resolve_redirects(Host, lists:flatten(Captured)),
            {urls, Urls};
        nomatch ->
            none
    end.

-spec resolve_redirects(binary(), [url()]) -> [url()].
resolve_redirects(Host, URLs) ->
    Proc = get_proc_name(Host),
    try gen_server:call(Proc, {resolve_redirects, URLs}) of
        {spam_filter, ResolvedURLs} ->
            ResolvedURLs
    catch
        exit:{timeout, _} ->
            ?WARNING_MSG("Timeout while resolving redirects: ~p", [URLs]),
            URLs
    end.

-spec do_resolve_redirects([url()], [url()]) -> [url()].
do_resolve_redirects([], Result) ->
    Result;
do_resolve_redirects([URL | Rest], Acc) ->
    case httpc:request(get,
                       {URL, [{"user-agent", "curl/8.7.1"}]},
                       [{autoredirect, false}, {timeout, ?HTTPC_TIMEOUT}],
                       [])
    of
        {ok, {{_, StatusCode, _}, Headers, _Body}} when StatusCode >= 300, StatusCode < 400 ->
            Location = proplists:get_value("location", Headers),
            case Location == undefined orelse lists:member(Location, Acc) of
                true ->
                    do_resolve_redirects(Rest, [URL | Acc]);
                false ->
                    do_resolve_redirects([Location | Rest], [URL | Acc])
            end;
        _Res ->
            do_resolve_redirects(Rest, [URL | Acc])
    end.

-spec extract_jids(binary()) -> {jids, [ljid()]} | none.
extract_jids(Body) ->
    RE = <<"\\S+@\\S+">>,
    Options = [global, {capture, all, binary}],
    case re:run(Body, RE, Options) of
        {match, Captured} when is_list(Captured) ->
            {jids, lists:filtermap(fun try_decode_jid/1, lists:flatten(Captured))};
        nomatch ->
            none
    end.

-spec try_decode_jid(binary()) -> {true, ljid()} | false.
try_decode_jid(S) ->
    try jid:decode(S) of
        #jid{} = JID ->
            {true,
             jid:remove_resource(
                 jid:tolower(JID))}
    catch
        _:{bad_jid, _} ->
            false
    end.

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

-spec reload_files(#{Type :: atom() => filename()}, state()) ->
                      {ok | {error, binary()}, state()}.
reload_files(Files, #state{host = Host, blocked_domains = BlockedDomains} = State) ->
    try read_files(Files) of
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
                         whitelist_domains = set_to_map(WhitelistDomains, false)}}
    catch
        {Op, File, Reason} when Op == open; Op == read ->
            Txt = format("Cannot ~s ~s for ~s: ~s", [Op, File, Host, format_error(Reason)]),
            ?ERROR_MSG("~s", [Txt]),
            {{error, Txt}, State}
    end.

set_to_map(Set) ->
    set_to_map(Set, true).

set_to_map(Set, V) ->
    sets:fold(fun(K, M) -> M#{K => V} end, #{}, Set).

-spec read_files(#{Type => filename()}) ->
                    #{jid => jid_set(),
                      url => url_set(),
                      Type => sets:set(binary())}
    when Type :: atom().
read_files(Files) ->
    maps:map(fun(Type, Filename) -> read_file(Filename, line_parser(Type)) end, Files).

-spec line_parser(Type :: atom()) -> fun((binary()) -> binary()).
line_parser(jid) ->
    fun parse_jid/1;
line_parser(url) ->
    fun parse_url/1;
line_parser(_) ->
    fun trim/1.

-spec read_file(filename(), fun((binary()) -> ljid() | url())) -> jid_set() | url_set().
read_file(none, _ParseLine) ->
    sets:new();
read_file(File, ParseLine) ->
    case file:open(File, [read, binary, raw, {read_ahead, 65536}]) of
        {ok, Fd} ->
            try
                read_line(Fd, ParseLine, sets:new())
            catch
                E ->
                    throw({read, File, E})
            after
                ok = file:close(Fd)
            end;
        {error, Reason} ->
            throw({open, File, Reason})
    end.

-spec read_line(file:io_device(),
                fun((binary()) -> ljid() | url()),
                jid_set() | url_set()) ->
                   jid_set() | url_set().
read_line(Fd, ParseLine, Set) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            read_line(Fd, ParseLine, sets:add_element(ParseLine(Line), Set));
        {error, Reason} ->
            throw(Reason);
        eof ->
            Set
    end.

-spec parse_jid(binary()) -> ljid().
parse_jid(S) ->
    try jid:decode(trim(S)) of
        #jid{} = JID ->
            jid:remove_resource(
                jid:tolower(JID))
    catch
        _:{bad_jid, _} ->
            throw({bad_jid, S})
    end.

-spec parse_url(binary()) -> url().
parse_url(S) ->
    URL = trim(S),
    RE = <<"https?://\\S+$">>,
    Options = [anchored, caseless, {capture, none}],
    case re:run(URL, RE, Options) of
        match ->
            URL;
        nomatch ->
            throw({bad_url, S})
    end.

-spec trim(binary()) -> binary().
trim(S) ->
    re:replace(S, <<"\\s+$">>, <<>>, [{return, binary}]).

-spec reject(stanza()) -> ok.
reject(#message{from = From,
                to = To,
                type = Type,
                lang = Lang} =
           Msg)
    when Type /= groupchat, Type /= error ->
    ?INFO_MSG("Rejecting unsolicited message from ~s to ~s",
              [jid:encode(From), jid:encode(To)]),
    Txt = <<"Your message is unsolicited">>,
    Err = xmpp:err_policy_violation(Txt, Lang),
    ejabberd_hooks:run(spam_stanza_rejected, To#jid.lserver, [Msg]),
    ejabberd_router:route_error(Msg, Err);
reject(#presence{from = From,
                 to = To,
                 lang = Lang} =
           Presence) ->
    ?INFO_MSG("Rejecting unsolicited presence from ~s to ~s",
              [jid:encode(From), jid:encode(To)]),
    Txt = <<"Your traffic is unsolicited">>,
    Err = xmpp:err_policy_violation(Txt, Lang),
    ejabberd_router:route_error(Presence, Err);
reject(_) ->
    ok.

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

-spec format_error(atom() | tuple()) -> binary().
format_error({bad_jid, JID}) ->
    <<"Not a valid JID: ", JID/binary>>;
format_error({bad_url, URL}) ->
    <<"Not an HTTP(S) URL: ", URL/binary>>;
format_error(Reason) ->
    list_to_binary(file:format_error(Reason)).

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
                        tags = [filter],
                        desc = "Reload spam JID/URL files",
                        module = ?MODULE,
                        function = reload_spam_filter_files,
                        args = [{host, binary}],
                        result = {res, rescode}},
     #ejabberd_commands{name = get_spam_filter_cache,
                        tags = [filter],
                        desc = "Show spam filter cache contents",
                        module = ?MODULE,
                        function = get_spam_filter_cache,
                        args = [{host, binary}],
                        result =
                            {spammers,
                             {list, {spammer, {tuple, [{jid, string}, {timestamp, integer}]}}}}},
     #ejabberd_commands{name = expire_spam_filter_cache,
                        tags = [filter],
                        desc = "Remove old/unused spam JIDs from cache",
                        module = ?MODULE,
                        function = expire_spam_filter_cache,
                        args = [{host, binary}, {seconds, integer}],
                        result = {res, restuple}},
     #ejabberd_commands{name = add_to_spam_filter_cache,
                        tags = [filter],
                        desc = "Add JID to spam filter cache",
                        module = ?MODULE,
                        function = add_to_spam_filter_cache,
                        args = [{host, binary}, {jid, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = drop_from_spam_filter_cache,
                        tags = [filter],
                        desc = "Drop JID from spam filter cache",
                        module = ?MODULE,
                        function = drop_from_spam_filter_cache,
                        args = [{host, binary}, {jid, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = get_blocked_domains,
                        tags = [filter],
                        desc = "Get list of domains being blocked",
                        module = ?MODULE,
                        function = get_blocked_domains,
                        args = [{host, binary}],
                        result = {blocked_domains, {list, {jid, string}}}},
     #ejabberd_commands{name = add_blocked_domain,
                        tags = [filter],
                        desc = "Add domain to list of blocked domains",
                        module = ?MODULE,
                        function = add_blocked_domain,
                        args = [{host, binary}, {domain, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = remove_blocked_domain,
                        tags = [filter],
                        desc = "Remove domain from list of blocked domains",
                        module = ?MODULE,
                        function = remove_blocked_domain,
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
    LServer = jid:nameprep(Host),
    Files =
        #{domains => gen_mod:get_module_opt(LServer, ?MODULE, spam_domains_file),
          jid => gen_mod:get_module_opt(LServer, ?MODULE, spam_jids_file),
          url => gen_mod:get_module_opt(LServer, ?MODULE, spam_urls_file)},
    case try_call_by_host(Host, {reload_files, Files}) of
        {spam_filter, ok} ->
            ok;
        {spam_filter, {error, Txt}} ->
            {error, binary_to_list(Txt)};
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
