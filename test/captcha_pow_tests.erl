-module(captcha_pow_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/include/xmpp.hrl").

%% Fixed vector, computed with sha256sum: the lowercase hex digest of
%% "example.com0" ends with "e56d2", so it is a valid proof for that label.
-define(PREFIX, <<"example.com">>).
-define(SOLUTION, <<"example.com0">>).
-define(LABEL, <<"e56d2">>).

pow_label_shape_test() ->
    L = ejabberd_captcha:pow_label(5),
    ?assertEqual(5, byte_size(L)),
    <<First, _/binary>> = L,
    ?assert((First >= $8 andalso First =< $9)
	    orelse (First >= $a andalso First =< $f)),
    ?assert(lists:all(fun(C) ->
			      (C >= $0 andalso C =< $9)
				  orelse (C >= $a andalso C =< $f)
		      end, binary_to_list(L))).

verify_pow_test() ->
    ?assert(ejabberd_captcha:verify_pow(?SOLUTION, ?PREFIX, ?LABEL)),
    ?assertNot(ejabberd_captcha:verify_pow(?SOLUTION, ?PREFIX, <<"12345">>)),
    ?assertNot(ejabberd_captcha:verify_pow(?SOLUTION, <<"evil.com">>, ?LABEL)),
    ?assertNot(ejabberd_captcha:verify_pow(<<"garbage">>, ?PREFIX, ?LABEL)),
    Big = <<?PREFIX/binary, (binary:copy(<<"a">>, 300))/binary>>,
    ?assertNot(ejabberd_captcha:verify_pow(Big, ?PREFIX, ?LABEL)).

process_reply_roundtrip_test() ->
    Id = <<"pow-test-id">>,
    ok = ejabberd_captcha:insert_pow_challenge(Id, ?PREFIX, ?LABEL),
    ?assertEqual(ok, ejabberd_captcha:process_reply(reply_form(Id, ?SOLUTION))),
    %% single-use
    ?assertEqual({error, not_found},
		 ejabberd_captcha:process_reply(reply_form(Id, ?SOLUTION))),
    %% a wrong solution (fails the prefix bind) is a bad match
    ok = ejabberd_captcha:insert_pow_challenge(Id, ?PREFIX, ?LABEL),
    ?assertEqual({error, bad_match},
		 ejabberd_captcha:process_reply(
		   reply_form(Id, <<"notprefix-", ?SOLUTION/binary>>))).

reply_form(Id, Solution) ->
    Fields = captcha_form:encode(
	       [{challenge, Id}, {'SHA-256', Solution}], <<"en">>, [challenge]),
    #xcaptcha{xdata = #xdata{type = submit, fields = Fields}}.
