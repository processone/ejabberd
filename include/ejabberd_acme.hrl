
-record(challenge, {
	  type  = <<"http-01">> :: bitstring(),
	  status = pending :: pending | valid | invalid,
	  uri = "" :: url(),
	  token = <<"">> :: bitstring()
	 }).

-record(data_acc, {
	  id  :: list(),
	  key :: jose_jwk:key()
	 }).

-record(data, {
	  account = none :: #data_acc{} | 'none' 
	 }).



-type nonce() :: string().
-type url() :: string().
-type proplist() :: [{_, _}].
-type dirs() :: #{string() => url()}.
-type jws() :: map().
-type handle_resp_fun() :: fun(({ok, proplist(), proplist()}) -> {ok, _, nonce()}).

-type acme_challenge() :: #challenge{}.
