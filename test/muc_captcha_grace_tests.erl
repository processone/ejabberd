%% EUnit tests for mod_muc_room's CAPTCHA grace period: once a user solves a
%% captcha_protected room's CAPTCHA, the room does not re-challenge it (by bare
%% JID) on a rejoin within captcha_grace_period, and a current occupant or
%% subscriber is never challenged for a join/subscribe it already made. These
%% cover the waiver decision (captcha_waived/2) and the pure grace bookkeeping;
%% the end-to-end behaviour is exercised by the integration tests.
-module(muc_captcha_grace_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_muc_room.hrl").

-define(U, <<"alice">>).
-define(S, <<"example.com">>).
-define(BARE, {?U, ?S, <<>>}).

j(Res) -> jid:make(?U, ?S, Res).

%% A #state built without touching ejabberd_config (the #config default `lang`
%% would read it): everything the waiver looks at, nothing it does not.
st() -> #state{config = #config{lang = <<"en">>}}.

%% ---- captcha_waived/2: the real waiver, over a minimal #state{} ----

%% A stranger with no pass, not in the room, is challenged (not waived).
stranger_is_challenged_test() ->
    ?assertNot(mod_muc_room:captcha_waived(j(<<"r1">>), st())).

%% A current occupant is waived -- an occupant that then subscribes reaches the
%% check as a non-subscriber and must not be re-challenged.
occupant_is_waived_test() ->
    SD = (st())#state{users = #{jid:tolower(j(<<"r1">>)) => online}},
    ?assert(mod_muc_room:captcha_waived(j(<<"r1">>), SD)).

%% An existing subscriber is waived.
subscriber_is_waived_test() ->
    Subs = #muc_subscribers{subscribers = #{?BARE => sub}},
    SD = (st())#state{muc_subscribers = Subs},
    ?assert(mod_muc_room:captcha_waived(j(<<"r1">>), SD)).

%% A valid, unexpired pass waives -- even on a reconnect's new resource, since
%% the pass is kept by bare JID.
valid_pass_waives_any_resource_test() ->
    Exp = erlang:system_time(millisecond) + 60000,
    SD = (st())#state{captcha_passed = #{?BARE => Exp}},
    ?assert(mod_muc_room:captcha_waived(j(<<"r1">>), SD)),
    ?assert(mod_muc_room:captcha_waived(j(<<"other-resource">>), SD)).

%% An expired pass does not waive.
expired_pass_does_not_waive_test() ->
    Exp = erlang:system_time(millisecond) - 1,
    SD = (st())#state{captcha_passed = #{?BARE => Exp}},
    ?assertNot(mod_muc_room:captcha_waived(j(<<"r1">>), SD)).

%% A whitelisted JID is waived.
whitelisted_is_waived_test() ->
    Cfg = #config{lang = <<"en">>, captcha_whitelist = gb_sets:from_list([?BARE])},
    SD = (st())#state{config = Cfg},
    ?assert(mod_muc_room:captcha_waived(j(<<"r1">>), SD)).

%% ---- the pure grace bookkeeping ----

unknown_is_not_valid_test() ->
    ?assertNot(mod_muc_room:captcha_pass_valid(?BARE, #{}, 1000)).

grace_window_test() ->
    Now = 1000, Grace = 5000,
    P = mod_muc_room:remember_captcha_pass(?BARE, #{}, Now, Grace),
    ?assert(mod_muc_room:captcha_pass_valid(?BARE, P, Now)),
    ?assert(mod_muc_room:captcha_pass_valid(?BARE, P, Now + Grace - 1)),
    ?assertNot(mod_muc_room:captcha_pass_valid(?BARE, P, Now + Grace)),
    ?assertNot(mod_muc_room:captcha_pass_valid({<<"bob">>, ?S, <<>>}, P, Now)).

infinity_never_expires_test() ->
    P = mod_muc_room:remember_captcha_pass(?BARE, #{}, 1000, infinity),
    ?assert(mod_muc_room:captcha_pass_valid(?BARE, P, 1000)),
    ?assert(mod_muc_room:captcha_pass_valid(?BARE, P, 1 bsl 60)).

%% A later pass (a solve, or a leave) refreshes the window -- this is what keeps
%% a client reconnecting every few minutes from being re-challenged each hour.
refresh_extends_test() ->
    P1 = mod_muc_room:remember_captcha_pass(?BARE, #{}, 1000, 5000),
    ?assertNot(mod_muc_room:captcha_pass_valid(?BARE, P1, 6000)),
    P2 = mod_muc_room:remember_captcha_pass(?BARE, P1, 6000, 5000),
    ?assert(mod_muc_room:captcha_pass_valid(?BARE, P2, 10999)).

%% Recording a new pass purges entries that have expired, so the map is bounded.
purge_on_record_test() ->
    K = {<<"bob">>, ?S, <<>>},
    P1 = mod_muc_room:remember_captcha_pass(K, #{}, 0, 1000),
    ?assertEqual([K], maps:keys(P1)),
    P2 = mod_muc_room:remember_captcha_pass(?BARE, P1, 2000, 5000),
    ?assertEqual([?BARE], maps:keys(P2)).

purge_direct_test() ->
    Map = #{?BARE => 500, {<<"bob">>, ?S, <<>>} => infinity},
    ?assertEqual(#{{<<"bob">>, ?S, <<>>} => infinity},
		 mod_muc_room:purge_captcha_passes(Map, 1000)),
    ?assertEqual(Map, mod_muc_room:purge_captcha_passes(Map, 100)).

%% ---- a kick or ban revokes the pass (set_role to none) ----

%% A kick (role none, no affiliation change) revokes the pass, so the kicked
%% user is challenged again instead of walking back in on it.
kick_revokes_pass_test() ->
    ?assert(mod_muc_room:kick_revokes_captcha_pass(none, undefined)).

%% A ban (role none, affiliation outcast) revokes it too.
ban_revokes_pass_test() ->
    Reason = {affiliation, #affiliation{affiliation = outcast}},
    ?assert(mod_muc_room:kick_revokes_captcha_pass(none, Reason)).

%% A config-change removal (e.g. the room turning members-only) is not a kick
%% and leaves the pass alone.
config_change_keeps_pass_test() ->
    ?assertNot(mod_muc_room:kick_revokes_captcha_pass(none, config_change)).

%% A role that is not 'none' (a grant, a nick/presence update) never revokes.
non_removal_keeps_pass_test() ->
    ?assertNot(mod_muc_room:kick_revokes_captcha_pass(participant, undefined)),
    ?assertNot(mod_muc_room:kick_revokes_captcha_pass(visitor, config_change)).
