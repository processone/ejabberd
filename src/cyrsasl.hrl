%% @type saslparams() = {sasl_params, Host, Realm, GetPassword, CheckPassword, CheckPasswordDigest}
%%     Host = string()
%%     Realm = string()
%%     GetPassword = function()
%%     CheckPassword = function()
%%     CheckPasswordDigest = any().
%% Parameters for SASL.

-record(sasl_params, {
	  host,
	  realm,
	  get_password,
	  check_password,
	  check_password_digest,
	  socket}).
