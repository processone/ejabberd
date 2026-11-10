-module(ejabberd_service_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, <<"xmpp.example.org">>).

managed_tls_options_test_() ->
    {setup,
     fun() -> meck:new(ejabberd_pkix, [no_link]) end,
     fun(_) -> meck:unload(ejabberd_pkix) end,
     [fun managed_certificate/0,
      fun explicit_certificate/0,
      fun replaced_certificate/0,
      fun missing_certificate/0]}.

managed_certificate() ->
    ok = meck:expect(ejabberd_pkix, get_certfile,
                     fun(?SERVER) -> {ok, <<"managed.pem">>} end),
    Options = [compression_none, {certfile, undefined}, {ciphers, <<"HIGH">>}],
    Result = ejabberd_service:tls_options(state(Options)),
    ?assertEqual([compression_none, {certfile, <<"managed.pem">>}, {ciphers, <<"HIGH">>}], Result).

explicit_certificate() ->
    ok = meck:expect(ejabberd_pkix, get_certfile,
                     fun(_) -> erlang:error(unexpected_certificate_lookup) end),
    Options = [compression_none, {certfile, <<"explicit.pem">>}],
    ?assertEqual(Options, ejabberd_service:tls_options(state(Options))).

replaced_certificate() ->
    Options = [compression_none],
    State = state(Options),
    ok = meck:expect(ejabberd_pkix, get_certfile,
                     fun(?SERVER) -> {ok, <<"old-acme.pem">>} end),
    ?assertEqual(<<"old-acme.pem">>, proplists:get_value(certfile, ejabberd_service:tls_options(State))),
    ok = meck:expect(ejabberd_pkix, get_certfile,
                     fun(?SERVER) -> {ok, <<"renewed-acme.pem">>} end),
    ?assertEqual(<<"renewed-acme.pem">>, proplists:get_value(certfile, ejabberd_service:tls_options(State))).

missing_certificate() ->
    ok = meck:expect(ejabberd_pkix, get_certfile, fun(?SERVER) -> error end),
    Options = [compression_none, {certfile, undefined}],
    ?assertEqual(Options, ejabberd_service:tls_options(state(Options))).

state(Options) ->
    #{server => ?SERVER, tls_options => Options}.
