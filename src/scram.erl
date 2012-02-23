%%%----------------------------------------------------------------------
%%% File    : scram.erl
%%% Author  : Stephen Röttger <stephen.roettger@googlemail.com>
%%% Purpose : SCRAM (RFC 5802)
%%% Created : 7 Aug 2011 by Stephen Röttger <stephen.roettger@googlemail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

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

%% ejabberd doesn't implement SASLPREP, so we use the similar RESOURCEPREP instead
salted_password(Password, Salt, IterationCount) ->
	hi(exmpp_stringprep:resourceprep(Password), Salt, IterationCount).

client_key(SaltedPassword) ->
	crypto:sha_mac(SaltedPassword, "Client Key").

stored_key(ClientKey) ->
	crypto:sha(ClientKey).

server_key(SaltedPassword) ->
	crypto:sha_mac(SaltedPassword, "Server Key").

client_signature(StoredKey, AuthMessage) ->
	crypto:sha_mac(StoredKey, AuthMessage).

client_key(ClientProof, ClientSignature) ->
	list_to_binary(lists:zipwith(fun(X, Y) ->
					X bxor Y
				  end,
				  binary_to_list(ClientProof),
				  binary_to_list(ClientSignature))).

server_signature(ServerKey, AuthMessage) ->
	crypto:sha_mac(ServerKey, AuthMessage).

hi(Password, Salt, IterationCount) ->
	U1 = crypto:sha_mac(Password, string:concat(binary_to_list(Salt), [0,0,0,1])),
	list_to_binary(lists:zipwith(fun(X, Y) ->
					X bxor Y
				  end,
				  binary_to_list(U1),
				  binary_to_list(hi_round(Password, U1, IterationCount-1)))).

hi_round(Password, UPrev, 1) ->
	crypto:sha_mac(Password, UPrev);
hi_round(Password, UPrev, IterationCount) ->
	U = crypto:sha_mac(Password, UPrev),
	list_to_binary(lists:zipwith(fun(X, Y) ->
					X bxor Y
				  end,
				  binary_to_list(U),
				  binary_to_list(hi_round(Password, U, IterationCount-1)))).
