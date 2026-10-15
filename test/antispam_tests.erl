%%%-------------------------------------------------------------------
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Created : 8 May 2025 by Stefan Strigler
%%%
%%%
%%% ejabberd, Copyright (C) 2026  ProcessOne
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
-include("mod_roster.hrl").
-include("mod_antispam.hrl").

%% @format-begin

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single tests
%%%===================================================================

single_cases() ->
    {antispam_single,
     [sequence],
     [single_test(block_by_jid),
      single_test(block_by_url),
      single_test(blocked_jid_is_cached),
      single_test(uncache_blocked_jid),
      single_test(check_blocked_domain),
      single_test(check_not_blocked_friend),
      single_test(check_not_blocked_room_member),
      single_test(unblock_domain),
      single_test(empty_domain_list),
      single_test(block_domain_globally),
      single_test(check_domain_blocked_globally),
      single_test(unblock_domain_in_vhost),
      single_test(unblock_domain_globally),
      single_test(block_domain_in_vhost),
      single_test(unblock_domain_in_vhost2),
      single_test(jid_cache),
      single_test(rtbl_domains),
      single_test(rtbl_domains_whitelisted),
      single_test(spam_dump_file)]}.

%%%===================================================================

block_by_jid(Config) ->
    Server = ?config(server, Config),
    is_spam(message_hello(<<"spammer_jid">>, <<"localhost">>, Config), Server).

block_by_url(Config) ->
    Server = ?config(server, Config),
    From = jid:make(<<"spammer">>, <<"localhost">>, <<"spam_client">>),
    To = my_jid(Config),
    is_not_spam(message_hello(<<"spammer">>, <<"localhost">>, Config), Server),
    is_spam(message(From, To, <<"hello world\nhttps://spam.domain.url">>), Server).

blocked_jid_is_cached(Config) ->
    Server = ?config(server, Config),
    is_spam(message_hello(<<"spammer">>, <<"localhost">>, Config), Server).

uncache_blocked_jid(Config) ->
    Server = ?config(server, Config),
    Spammer = jid:make(<<"spammer">>, <<"localhost">>, <<"">>),
    mod_antispam:drop_from_spam_filter_cache(Server, jid:encode(Spammer)),
    is_not_spam(message_hello(<<"spammer">>, <<"localhost">>, Config), Server).

check_blocked_domain(Config) ->
    Server = ?config(server, Config),
    is_spam(message_hello(<<"other_spammer">>, <<"spam_domain.org">>, Config), Server).

check_not_blocked_friend(Config) ->
    Server = ?config(server, Config),
    is_spam(message_hello(<<"friend">>, <<"spam_domain.org">>, Config), Server),
    #jid{luser = LUser, lserver = LServer} = MyJid = my_jid(Config),
    Ljid = jid:tolower(MyJid),
    mod_roster:set_roster(#roster{usj = {LUser, LServer, Ljid},
                                  us = {LUser, LServer},
                                  jid =
                                      jid:tolower(
                                          jid:make(<<"friend">>, <<"spam_domain.org">>)),
                                  subscription = both}),
    is_not_spam(message_hello(<<"friend">>, <<"spam_domain.org">>, Config), Server),
    mod_roster:remove_user(LUser, LServer).

check_not_blocked_room_member(Config) ->
    Server = ?config(server, Config),
    UserJid = jid:make(<<"room_member">>, <<"spam_domain.org">>),
    RoomName = <<"room">>,
    MucHost = <<"conference.localhost">>,
    RoomJid = jid:make(RoomName, MucHost),
    is_spam(message(UserJid, RoomJid, <<"hello room">>), Server),
    ok = mod_muc:create_room(MucHost, RoomName, [{persistent, true}]),
    {ok, Pid} = mod_muc:find_online_room(RoomName, MucHost),
    {ok, _State} = mod_muc_room:change_item(Pid, UserJid, affiliation, member, <<>>),
    is_not_spam(message(UserJid, RoomJid, <<"hello room">>), Server).

unblock_domain(Config) ->
    Server = ?config(server, Config),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(Server, <<"spam_domain.org">>)),
    ?match([], mod_antispam:get_blocked_domains(Server)),
    is_not_spam(message_hello(<<"spammer">>, <<"spam_domain.org">>, Config), Server).

%%%===================================================================

empty_domain_list(Config) ->
    Server = ?config(server, Config),
    ?match([], mod_antispam:get_blocked_domains(Server)),
    SpamFrom = jid:make(<<"spammer">>, <<"spam.domain">>, <<"spam_client">>),
    To = my_jid(Config),
    Msg = message(SpamFrom, To, <<"hello world">>),
    is_not_spam(Msg, Server).

block_domain_globally(Config) ->
    Server = ?config(server, Config),
    ?match({ok, _}, mod_antispam:add_blocked_domain(<<"global">>, <<"spam.domain">>)),
    SpamFrom = jid:make(<<"spammer">>, <<"spam.domain">>, <<"spam_client">>),
    To = my_jid(Config),
    is_spam(message(SpamFrom, To, <<"hello world">>), Server).

check_domain_blocked_globally(_Config) ->
    Vhosts = [H || H <- ejabberd_option:hosts(), gen_mod:is_loaded(H, mod_antispam)],
    NumVhosts = length(Vhosts),
    ?match(NumVhosts, length(lists:filter(has_spam_domain(<<"spam.domain">>), Vhosts))).

unblock_domain_in_vhost(Config) ->
    Server = ?config(server, Config),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(Server, <<"spam.domain">>)),
    ?match([], mod_antispam:get_blocked_domains(Server)),
    SpamFrom = jid:make(<<"spammer">>, <<"spam.domain">>, <<"spam_client">>),
    To = my_jid(Config),
    is_not_spam(message(SpamFrom, To, <<"hello world">>), Server).

unblock_domain_globally(_Config) ->
    Vhosts = [H || H <- ejabberd_option:hosts(), gen_mod:is_loaded(H, mod_antispam)],
    NumVhosts = length(Vhosts),
    ?match(NumVhosts, length(lists:filter(has_spam_domain(<<"spam.domain">>), Vhosts)) + 1),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(<<"global">>, <<"spam.domain">>)),
    ?match([], lists:filter(has_spam_domain(<<"spam.domain">>), Vhosts)).

block_domain_in_vhost(Config) ->
    Server = ?config(server, Config),
    Vhosts = [H || H <- ejabberd_option:hosts(), gen_mod:is_loaded(H, mod_antispam)],
    ?match({ok, _}, mod_antispam:add_blocked_domain(Server, <<"spam.domain">>)),
    ?match([Server], lists:filter(has_spam_domain(<<"spam.domain">>), Vhosts)),
    SpamFrom = jid:make(<<"spammer">>, <<"spam.domain">>, <<"spam_client">>),
    To = my_jid(Config),
    is_spam(message(SpamFrom, To, <<"hello world">>), Server).

unblock_domain_in_vhost2(Config) ->
    Server = ?config(server, Config),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(Server, <<"spam.domain">>)),
    SpamFrom = jid:make(<<"spammer">>, <<"spam.domain">>, <<"spam_client">>),
    To = my_jid(Config),
    is_not_spam(message(SpamFrom, To, <<"hello world">>), Server),
    disconnect(Config).

%%%===================================================================

jid_cache(Config) ->
    Server = ?config(server, Config),
    SpamFrom = jid:make(<<"spammer">>, Server, <<"spam_client">>),
    is_not_spam(message_hello(<<"spammer">>, Server, Config), Server),
    mod_antispam:add_to_spam_filter_cache(Server, jid:encode(SpamFrom)),
    is_spam(message_hello(<<"spammer">>, Server, Config), Server),
    mod_antispam:drop_from_spam_filter_cache(Server, jid:encode(SpamFrom)),
    is_not_spam(message_hello(<<"spammer">>, Server, Config), Server),
    disconnect(Config).

%%%===================================================================

rtbl_domains(Config) ->
    Server = ?config(server, Config),
    RTBLHost =
        jid:encode(
            suite:pubsub_jid(Config)),
    RTBLDomainsNode = <<"spam_source_domains">>,
    OldOpts = gen_mod:get_module_opts(Server, mod_antispam),
    NewOpts =
        maps:merge(OldOpts,
                   #{rtbl_services => [#rtbl_service{host = RTBLHost, node = RTBLDomainsNode}]}),
    Owner = jid:make(?config(user, Config), ?config(server, Config), <<>>),
    {result, _} =
        mod_pubsub:create_node(RTBLHost,
                               ?config(server, Config),
                               RTBLDomainsNode,
                               Owner,
                               <<"flat">>),
    {result, _} =
        mod_pubsub:publish_item(RTBLHost,
                                ?config(server, Config),
                                RTBLDomainsNode,
                                Owner,
                                <<"spam.source.domain">>,
                                [xmpp:encode(#ps_item{id = <<"spam.source.domain">>,
                                                      sub_els = []})]),
    mod_antispam:reload(Server, OldOpts, NewOpts),
    ?match({ok, _}, mod_antispam:remove_blocked_domain(Server, <<"spam_domain.org">>)),
    ?retry(100,
           10,
           ?match([<<"spam.source.domain">>], mod_antispam:get_blocked_domains(Server))),
    {result, _} =
        mod_pubsub:publish_item(RTBLHost,
                                ?config(server, Config),
                                RTBLDomainsNode,
                                Owner,
                                <<"spam.source.another">>,
                                [xmpp:encode(#ps_item{id = <<"spam.source.another">>,
                                                      sub_els = []})]),
    ?retry(100, 10, ?match(true, (has_spam_domain(<<"spam.source.another">>))(Server))),
    {result, _} =
        mod_pubsub:delete_item(RTBLHost, RTBLDomainsNode, Owner, <<"spam.source.another">>, true),
    ?retry(100, 10, ?match(false, (has_spam_domain(<<"spam.source.another">>))(Server))),
    {result, _} = mod_pubsub:delete_node(RTBLHost, RTBLDomainsNode, Owner),
    disconnect(Config).

rtbl_domains_whitelisted(Config) ->
    Server = ?config(server, Config),
    RTBLHost =
        jid:encode(
            suite:pubsub_jid(Config)),
    RTBLDomainsNode = <<"spam_source_domains">>,
    OldOpts = gen_mod:get_module_opts(Server, mod_antispam),
    NewOpts =
        maps:merge(OldOpts,
                   #{rtbl_services => [#rtbl_service{host = RTBLHost, node = RTBLDomainsNode}]}),
    Owner = jid:make(?config(user, Config), ?config(server, Config), <<>>),
    {result, _} =
        mod_pubsub:create_node(RTBLHost,
                               ?config(server, Config),
                               RTBLDomainsNode,
                               Owner,
                               <<"flat">>),
    {result, _} =
        mod_pubsub:publish_item(RTBLHost,
                                ?config(server, Config),
                                RTBLDomainsNode,
                                Owner,
                                <<"whitelisted.domain">>,
                                [xmpp:encode(#ps_item{id = <<"whitelisted.domain">>,
                                                      sub_els = []})]),
    mod_antispam:reload(Server, OldOpts, NewOpts),
    {result, _} =
        mod_pubsub:publish_item(RTBLHost,
                                ?config(server, Config),
                                RTBLDomainsNode,
                                Owner,
                                <<"yetanother.domain">>,
                                [xmpp:encode(#ps_item{id = <<"yetanother.domain">>,
                                                      sub_els = []})]),
    ?retry(100, 10, ?match(true, (has_spam_domain(<<"yetanother.domain">>))(Server))),
    %% we assume that the previous "whitelisted.domain" pubsub item has been consumed by now, so we
    %% can check that it doesn't exist
    ?match(false, (has_spam_domain(<<"whitelisted.domain">>))(Server)),
    {result, _} = mod_pubsub:delete_node(RTBLHost, RTBLDomainsNode, Owner),
    disconnect(Config).

%%%===================================================================

spam_dump_file(Config) ->
    Server = ?config(server, Config),
    {ok, CWD} = file:get_cwd(),
    Filename = filename:join([CWD, "spam.log"]),
    ?retry(100, 100, ?match(true, size(get_bytes(Filename)) > 0)),
    From = jid:make(<<"spammer_jid">>, <<"localhost">>, <<"spam_client">>),
    To = my_jid(Config),
    is_spam(message(From, To, <<"A very specific spam message">>), Server),
    ?retry(100,
           100,
           ?match({match, _}, re:run(get_bytes(Filename), <<"A very specific spam message">>))).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("antispam_" ++ atom_to_list(T)).

has_spam_domain(Domain) ->
    fun(Host) -> lists:member(Domain, mod_antispam:get_blocked_domains(Host)) end.

is_not_spam(Msg, Host) ->
    ?match({Msg, _}, mod_antispam_filter:s2s_receive_packet({Msg, #{server_host => Host}})).

is_spam(Spam, Host) ->
    ?match({stop, {drop, _}},
           mod_antispam_filter:s2s_receive_packet({Spam, #{server_host => Host}})).

message_hello(Username, Host, Config) ->
    SpamFrom = jid:make(Username, Host, <<"spam_client">>),
    To = my_jid(Config),
    message(SpamFrom, To, <<"hello world">>).

message(From, To, BodyText) ->
    #message{from = From,
             to = To,
             type = chat,
             body = [#text{data = BodyText}]}.

get_bytes(Filename) ->
    {ok, Bytes} = file:read_file(Filename),
    Bytes.
