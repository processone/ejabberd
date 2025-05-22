%%%-------------------------------------------------------------------
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Created : 8 May 2025 by Stefan Strigler
%%%
%%%
%%% ejabberd, Copyright (C) 2025   ProcessOne
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
-module(antispam_tests).

-compile(export_all).

-import(suite, [recv_presence/1, send_recv/2, my_jid/1, muc_room_jid/1,
		send/2, recv_message/1, recv_iq/1, muc_jid/1,
		alt_room_jid/1, wait_for_slave/1, wait_for_master/1,
		disconnect/1, put_event/2, get_event/1, peer_muc_jid/1,
		my_muc_jid/1, get_features/2, set_opt/3]).
-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single tests
%%%===================================================================
single_cases() ->
    {antispam_single, [sequence],
    [single_test(spam_files),
     single_test(blocked_domains),
     single_test(jid_cache),
     single_test(rtbl_domains)]}.

spam_files(Config) ->
    Host = ?config(server, Config),
    To = my_jid(Config),

    SpamJID = jid:make(<<"spammer_jid">>, <<"localhost">>, <<"spam_client">>),
    SpamJIDMsg = #message{from = SpamJID, to = To, type = chat, body = [#text{data = <<"hello world">>}]},
    is_spam(SpamJIDMsg),

    Spammer = jid:make(<<"spammer">>, <<"localhost">>, <<"spam_client">>),
    NoSpamMsg = #message{from = Spammer, to = To, type = chat, body = [#text{data = <<"hello world">>}]},
    is_not_spam(NoSpamMsg),
    SpamMsg = #message{from = Spammer, to = To, type = chat, body = [#text{data = <<"hello world\nhttps://spam.domain.url">>}]},
    is_spam(SpamMsg),
    %% now check this mischief is in jid_cache
    is_spam(NoSpamMsg),
    mod_antispam:drop_from_spam_filter_cache(Host, jid:to_string(Spammer)),
    is_not_spam(NoSpamMsg),

    ?retry(100, 10,
           ?match(true, (has_spam_domain(<<"spam_domain.org">>))(Host))),

    SpamDomain = jid:make(<<"spammer">>, <<"spam_domain.org">>, <<"spam_client">>),
    SpamDomainMsg = #message{from = SpamDomain, to = To, type = chat, body = [#text{data = <<"hello world">>}]},
    is_spam(SpamDomainMsg),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(Host, <<"spam_domain.org">>)),
    ?match([], mod_antispam:get_blocked_domains(Host)),
    is_not_spam(SpamDomainMsg),
    disconnect(Config).

blocked_domains(Config) ->
    Host = ?config(server, Config),
    ?match([], mod_antispam:get_blocked_domains(Host)),
    SpamFrom = jid:make(<<"spammer">>, <<"spam.domain">>, <<"spam_client">>),
    To = my_jid(Config),
    Msg = #message{from = SpamFrom, to = To, type = chat, body = [#text{data = <<"hello world">>}]},
    is_not_spam(Msg),
    ?match({ok, _},  mod_antispam:add_blocked_domain(<<"global">>, <<"spam.domain">>)),
    is_spam(Msg),
    Vhosts = [H || H <- ejabberd_option:hosts(), gen_mod:is_loaded(H, mod_antispam)],
    NumVhosts = length(Vhosts),
    ?match(NumVhosts, length(lists:filter(has_spam_domain(<<"spam.domain">>), Vhosts))),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(Host, <<"spam.domain">>)),
    ?match([], mod_antispam:get_blocked_domains(Host)),
    is_not_spam(Msg),
    ?match(NumVhosts, length(lists:filter(has_spam_domain(<<"spam.domain">>), Vhosts)) + 1),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(<<"global">>, <<"spam.domain">>)),
    ?match([], lists:filter(has_spam_domain(<<"spam.domain">>), Vhosts)),
    ?match({ok, _}, mod_antispam:add_blocked_domain(Host, <<"spam.domain">>)),
    ?match([Host], lists:filter(has_spam_domain(<<"spam.domain">>), Vhosts)),
    is_spam(Msg),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(Host, <<"spam.domain">>)),
    is_not_spam(Msg),
    disconnect(Config).

jid_cache(Config) ->
    Host = ?config(server, Config),
    SpamFrom = jid:make(<<"spammer">>, Host, <<"spam_client">>),
    To = my_jid(Config),
    Msg = #message{from = SpamFrom, to = To, type = chat, body = [#text{data = <<"hello world">>}]},
    is_not_spam(Msg),
    mod_antispam:add_to_spam_filter_cache(Host, jid:to_string(SpamFrom)),
    is_spam(Msg),
    mod_antispam:drop_from_spam_filter_cache(Host, jid:to_string(SpamFrom)),
    is_not_spam(Msg),
    disconnect(Config).

rtbl_domains(Config) ->
    Host = ?config(server, Config),
    RTBLHost = jid:to_string(suite:pubsub_jid(Config)),
    RTBLDomainsNode = <<"spam_source_domains">>,
    OldOpts = gen_mod:get_module_opts(Host, mod_antispam),
    NewOpts = maps:merge(OldOpts, #{rtbl_host => RTBLHost, rtbl_domains_node => RTBLDomainsNode}),
    Owner = jid:make(?config(user, Config), ?config(server, Config), <<>>),
    {result, _} = mod_pubsub:create_node(RTBLHost, ?config(server, Config), RTBLDomainsNode, Owner, <<"flat">>),
    {result, _} = mod_pubsub:publish_item(RTBLHost, ?config(server, Config), RTBLDomainsNode, Owner, <<"spam.source.domain">>,
                                          [xmpp:encode(#ps_item{id = <<"spam.source.domain">>, sub_els = []})]),
    mod_antispam:reload(Host, OldOpts, NewOpts),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(Host, <<"spam_domain.org">>)),
    ?retry(100, 10,
           ?match([<<"spam.source.domain">>], mod_antispam:get_blocked_domains(Host))),
    {result, _} = mod_pubsub:publish_item(RTBLHost, ?config(server, Config), RTBLDomainsNode, Owner, <<"spam.source.another">>,
                                          [xmpp:encode(#ps_item{id = <<"spam.source.another">>, sub_els = []})]),
    ?retry(100, 10,
           ?match(true, (has_spam_domain(<<"spam.source.another">>))(Host))),
    {result, _} = mod_pubsub:delete_item(RTBLHost, RTBLDomainsNode, Owner, <<"spam.source.another">>, true),
    ?retry(100, 10,
           ?match(false, (has_spam_domain(<<"spam.source.another">>))(Host))),
    {result, _} = mod_pubsub:delete_node(RTBLHost, RTBLDomainsNode, Owner),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("antispam_" ++ atom_to_list(T)).

has_spam_domain(Domain) ->
    fun(Host) ->
            lists:member(Domain, mod_antispam:get_blocked_domains(Host))
    end.

is_not_spam(Msg) ->
    ?match({Msg, undefined}, mod_antispam:s2s_receive_packet({Msg, undefined})).

is_spam(Spam) ->
    ?match({stop, {drop, undefined}}, mod_antispam:s2s_receive_packet({Spam, undefined})).
