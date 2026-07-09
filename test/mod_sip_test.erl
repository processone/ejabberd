%%%-------------------------------------------------------------------
%%% @doc Unit tests for mod_sip security fixes (GHSA-8j9p-hpfg-5cg3)
%%%
%%% Tests validate:
%%% 1. Rejection of SIP messages with multiple From/To headers (RFC 3261)
%%%
%%% The authentication fix for external senders (Issue #2) is tested in
%%% sip_tests.erl within the Common Test suite, where ejabberd is running
%%% and mod_sip can be properly loaded for host verification.
%%%-------------------------------------------------------------------
-module(mod_sip_test).

-ifdef(SIP).

-include_lib("eunit/include/eunit.hrl").
-include_lib("esip/include/esip.hrl").

%%%===================================================================
%%% Test helpers
%%%===================================================================

make_from_hdr(User, Host) ->
    {'from', {<<"Test">>, #uri{user = User, host = Host}, [{<<"tag">>, <<"abc123">>}]}}.

make_to_hdr(User, Host) ->
    {'to', {<<"Test">>, #uri{user = User, host = Host}, []}}.

make_sip_request(FromHdrs, ToHdrs) ->
    #sip{
        type = request,
        method = <<"MESSAGE">>,
        hdrs = [{'max-forwards', 70} | FromHdrs ++ ToHdrs],
        uri = #uri{user = <<"test">>, host = <<"localhost">>}
    }.

mock_sip_socket() ->
    #sip_socket{
        type = udp,
        addr = {{127,0,0,1}, 5060},
        peer = {{192,168,1,100}, 5060}
    }.

%%%===================================================================
%%% Test: mod_sip:action/2 rejects multiple From headers
%%% Security fix for GHSA-8j9p-hpfg-5cg3 Issue #1 (RFC 3261 8.1.1.3)
%%%===================================================================
action_rejects_multiple_from_test() ->
    %% Setup: Two From headers (attacker trying to spoof identity)
    From1 = make_from_hdr(<<"attacker">>, <<"external.com">>),
    From2 = make_from_hdr(<<"admin">>, <<"localhost">>),
    To = make_to_hdr(<<"victim">>, <<"localhost">>),
    Req = make_sip_request([From1, From2], [To]),
    SIPSock = mock_sip_socket(),
    
    %% Action should return 'malformed' for multiple From headers
    ?assertEqual(malformed, mod_sip:action(Req, SIPSock)).

%%%===================================================================
%%% Test: mod_sip:action/2 rejects multiple To headers
%%% Security fix for GHSA-8j9p-hpfg-5cg3 Issue #1 (RFC 3261 8.1.1.4)
%%%===================================================================
action_rejects_multiple_to_test() ->
    %% Setup: Two To headers
    From = make_from_hdr(<<"alice">>, <<"localhost">>),
    To1 = make_to_hdr(<<"bob">>, <<"localhost">>),
    To2 = make_to_hdr(<<"charlie">>, <<"localhost">>),
    Req = make_sip_request([From], [To1, To2]),
    SIPSock = mock_sip_socket(),
    
    %% Action should return 'malformed' for multiple To headers
    ?assertEqual(malformed, mod_sip:action(Req, SIPSock)).

%%%===================================================================
%%% Test: mod_sip:action/2 rejects zero From headers
%%%===================================================================
action_rejects_zero_from_test() ->
    %% Setup: No From header
    To = make_to_hdr(<<"bob">>, <<"localhost">>),
    Req = make_sip_request([], [To]),
    SIPSock = mock_sip_socket(),
    
    %% Action should return 'malformed' for missing From header
    ?assertEqual(malformed, mod_sip:action(Req, SIPSock)).

%%%===================================================================
%%% Test: mod_sip:action/2 rejects zero To headers
%%%===================================================================
action_rejects_zero_to_test() ->
    %% Setup: No To header
    From = make_from_hdr(<<"alice">>, <<"localhost">>),
    Req = make_sip_request([From], []),
    SIPSock = mock_sip_socket(),
    
    %% Action should return 'malformed' for missing To header
    ?assertEqual(malformed, mod_sip:action(Req, SIPSock)).

%%%===================================================================
%%% Test: Verify TEST exports exist for integration testing
%%%===================================================================
test_exports_available_test() ->
    {module, mod_sip} = code:ensure_loaded(mod_sip),
    Exports = mod_sip:module_info(exports),
    
    %% action/2 should be exported in TEST mode for testing
    ?assert(lists:member({action, 2}, Exports)),
    
    %% action_route/4 should be exported in TEST mode for integration tests
    ?assert(lists:member({action_route, 4}, Exports)).

-endif.
