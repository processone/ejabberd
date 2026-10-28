%%%-------------------------------------------------------------------
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Created : Thu Oct 22 14:30:22 CEST 2026
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
-module(antispam_report_tests).

-compile(export_all).

-import(suite, [recv_presence/1, send_recv/2, my_jid/1, muc_room_jid/1,
		send/2, recv_message/1, recv_iq/1, muc_jid/1,
		alt_room_jid/1, wait_for_peer/1,
		disconnect/1, put_event/2, get_event/1, peer_muc_jid/1,
		my_muc_jid/1, get_features/2, set_opt/3]).
-include("suite.hrl").

%% @format-begin

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single tests
%%%===================================================================

single_cases() ->
    {antispam_report_single, [sequence], [single_test(report_spam)]}.

%%%===================================================================

report_spam(Config) ->
    Server = ?config(server, Config),
    SpamJID = jid:make(<<"spammer">>, Server),
    ejabberd_auth:try_register(<<"abuse">>, Server, <<"topSecret">>),
    ejabberd_auth:try_register(<<"reports">>, Server, <<"topSecret">>),

    do_report(Config, SpamJID, false, false),

    ?match(0, mod_offline:count_offline_messages(<<"abuse">>, Server)),

    unblock(Config, SpamJID),

    do_report(Config, SpamJID, true, false),

    await(1, {mod_offline, count_offline_messages, [<<"abuse">>, Server]}, 10, 500),
    ?match(0, mod_offline:count_offline_messages(<<"reports">>, Server)),

    unblock(Config, SpamJID),

    do_report(Config, SpamJID, true, true),

    await(2, {mod_offline, count_offline_messages, [<<"abuse">>, Server]}, 10, 500),
    await(1, {mod_offline, count_offline_messages, [<<"reports">>, Server]}, 10, 500),

    AbuseEls = mod_offline:get_offline_els(<<"abuse">>, Server),
    ct:pal("got messages: ~p", [AbuseEls]),

    Els = mod_offline:get_offline_els(<<"reports">>, Server),
    ct:pal("got messages: ~p", [Els]),

    unblock(Config, SpamJID),
    ejabberd_auth:remove_user(<<"abuse">>, Server),
    ejabberd_auth:remove_user(<<"reports">>, Server),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
couple_cases() ->
    {antispam_report_couple, [sequence],
     [couple_test(report_stanza_id)]}.

report_stanza_id_romeo(Config) ->
    wait_for_peer(Config),
    Peer = ?config(peer, Config),
    send(Config, #message{to = Peer, body = [#text{data = <<"make $$ fast!">>}]}),
    clean(disconnect(Config)).

report_stanza_id_julia(Config) ->
    Server = ?config(server, Config),
    ejabberd_auth:try_register(<<"abuse">>, Server, <<"topSecret">>),
    Peer = ?config(peer, Config),
    ok = mam_tests:set_default(Config, always),

    wait_for_peer(Config),
    #message{from = Peer,
             sub_els = [#mam_archived{id = ArchiveId} | _]} = Msg = recv_message(Config),
    ct:pal("julia got message: ~p", [Msg]),
    peer_down = get_event(Config),

    do_report(Config, Peer, true, false, ArchiveId),

    await(1, {mod_offline, count_offline_messages, [<<"abuse">>, Server]}, 10, 500),

    [#message{sub_els = [ReceivedReport | _]}] = mod_offline:get_offline_els(<<"abuse">>, Server),
    #received_report{
        stanzas =
            [#forwarded{sub_els =
                            [Message]}]}
        = xmpp:decode(ReceivedReport),
    #message{body = [#text{data = <<"make $$ fast!">>}]} = xmpp:decode(Message),
    unblock(Config, Peer),
    ejabberd_auth:remove_user(<<"abuse">>, Server),
    clean(disconnect(Config)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("antispam_report." ++ atom_to_list(T)).

couple_test(T) ->
    {list_to_atom("antispam_report_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("antispam_report." ++ atom_to_list(T) ++ "_romeo"),
      list_to_atom("antispam_report." ++ atom_to_list(T) ++ "_julia")]}.

report_spam_iq(JID, ReportOrigin, ThirdParty, StanzaIDs) ->
    #iq{type = set,
        sub_els =
            [#block{items =
                        [#block_item{jid = JID,
                                     spam_report =
                                         #report{reason = spam,
                                                 report_origin = ReportOrigin,
                                                 third_party = ThirdParty,
                                                 stanza_ids = [#stanza_id{by = JID,
                                                                          id = Id} || Id <- StanzaIDs]
                                                }}]}]}.

unblock_iq(JID) ->
    #iq{type = set, sub_els = [#unblock{items = [#block_item{jid = JID}]}]}.

do_report(Config, SpamJID, ReportOrigin, ThirdParty) ->
    do_report(Config, SpamJID, ReportOrigin, ThirdParty, []).

do_report(Config, SpamJID, ReportOrigin, ThirdParty, StanzaID) when not is_list(StanzaID) ->
    do_report(Config, SpamJID, ReportOrigin, ThirdParty, [StanzaID]);
do_report(Config, SpamJID, ReportOrigin, ThirdParty, StanzaIDs) ->
    ?match(#iq{type = result},
           send_recv(Config, report_spam_iq(SpamJID, ReportOrigin, ThirdParty, StanzaIDs))),
    ?recv1(#iq{type = set, sub_els = [#privacy_query{}]}),
    ?recv1(#iq{type = set, sub_els = [#block{items = [#block_item{jid = SpamJID}]}]}).

unblock(Config, SpamJID) ->
    ?match(#iq{type = result}, send_recv(Config, unblock_iq(SpamJID))),
    ?recv1(#iq{type = set, sub_els = [#privacy_query{}]}),
    ?recv1(#iq{type = set, sub_els = [#unblock{items = [#block_item{jid = SpamJID}]}]}).

await(What, MFA, _Sleep, _Steps = 0) ->
    ct:fail("Timeout waiting for ~p on ~p", [What, MFA]);
await(What, {M, F, A} = MFA, Sleep, Steps) ->
    case apply(M, F, A) of
        What ->
            ok;
        _Else ->
            timer:sleep(Sleep),
            await(What, MFA, Sleep, Steps - 1)
    end.

clean(Config) ->
    {U, S, _} = jid:tolower(my_jid(Config)),
    mod_mam:remove_user(U, S),
    Config.
