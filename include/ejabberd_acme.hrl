
-record(challenge, {
	  type  = <<"http-01">> :: bitstring(),
	  status = pending :: pending | valid | invalid,
	  uri = "" :: url(),
	  token = <<"">> :: bitstring()
	 }).

-record(data_acc, {
	  id     :: list(),
	  ca_url :: url(),
	  key    :: jose_jwk:key()
	 }).
-type data_acc() :: #data_acc{}.

-record(data_cert, {
	  domain  :: bitstring(),
	  pem     :: pem(),
	  path    :: string()
	 }).
-type data_cert() :: #data_cert{}.

%%
%% Types
%%

%% Acme configuration
-type acme_config() :: [{ca_url, url()} | {contact, bitstring()}].

%% The main data type that ejabberd_acme keeps
-type acme_data() :: proplist().

%% The list of certificates kept in data
-type data_certs() :: proplist(bitstring(), data_cert()).

%% The certificate saved in pem format
-type pem() :: bitstring().

-type nonce() :: string().
-type url() :: string().
-type proplist() :: [{_, _}].
-type proplist(X,Y) :: [{X,Y}].
-type dirs() :: #{string() => url()}.
-type jws() :: map().
-type handle_resp_fun() :: fun(({ok, proplist(), proplist()}) -> {ok, _, nonce()}).

-type acme_challenge() :: #challenge{}.

%% Options
-type account_opt() :: string().
-type verbose_opt() :: string().
-type domains_opt() :: string().

