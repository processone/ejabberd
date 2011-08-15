-module(scram).
-author('stephen.roettger@googlemail.com').

%% External exports
-export([salted_password/3,
	 stored_key/1,
	 server_key/1,
	 server_signature/2,
	 client_signature/2,
	 client_key/1,
	 client_key/2
	]).

salted_password(Password, Salt, IterationCount) ->
	hi(jlib:nameprep(Password), Salt, IterationCount).

client_key(SaltedPassword) ->
	crypto:sha_mac(SaltedPassword, "Client Key").

stored_key(ClientKey) ->
	crypto:sha(ClientKey).

server_key(SaltedPassword) ->
	crypto:sha_mac(SaltedPassword, "Server Key").

client_signature(StoredKey, AuthMessage) ->
	crypto:sha_mac(StoredKey, AuthMessage).

client_key(ClientProof, ClientSignature) ->
	binary:list_to_bin(lists:zipwith(fun(X, Y) ->
					X bxor Y
				  end,
				  binary:bin_to_list(ClientProof),
				  binary:bin_to_list(ClientSignature))).

server_signature(ServerKey, AuthMessage) ->
	crypto:sha_mac(ServerKey, AuthMessage).

hi(Password, Salt, IterationCount) ->
	U1 = crypto:sha_mac(Password, string:concat(binary:bin_to_list(Salt), [0,0,0,1])),
	binary:list_to_bin(lists:zipwith(fun(X, Y) ->
					X bxor Y
				  end,
				  binary:bin_to_list(U1),
				  binary:bin_to_list(hi_round(Password, U1, IterationCount-1)))).

hi_round(Password, UPrev, 1) ->
	crypto:sha_mac(Password, UPrev);
hi_round(Password, UPrev, IterationCount) ->
	U = crypto:sha_mac(Password, UPrev),
	binary:list_to_bin(lists:zipwith(fun(X, Y) ->
					X bxor Y
				  end,
				  binary:bin_to_list(U),
				  binary:bin_to_list(hi_round(Password, U, IterationCount-1)))).
