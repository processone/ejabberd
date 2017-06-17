
-record(challenge, {
        type  = <<"http-01">> :: bitstring(),
        status = pending :: pending | valid | invalid,
        uri = <<"">> :: bitstring(),
        token = <<"">> :: bitstring()
        }).

-type nonce() :: string().
-type url() :: string().
-type proplist() :: [{_, _}].
-type jws() :: map().
-type handle_resp_fun() :: fun(({ok, proplist(), proplist()}) -> {ok, _, nonce()}).

-type acme_challenge() :: #challenge{}.
