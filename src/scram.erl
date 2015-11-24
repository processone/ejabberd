%%%----------------------------------------------------------------------
%%% File    : scram.erl
%%% Author  : Stephen Röttger <stephen.roettger@googlemail.com>
%%% Purpose : SCRAM (RFC 5802)
%%% Created : 7 Aug 2011 by Stephen Röttger <stephen.roettger@googlemail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(scram).

-author('stephen.roettger@googlemail.com').

%% External exports
%% ejabberd doesn't implement SASLPREP, so we use the similar RESOURCEPREP instead
-export([salted_password/3, stored_key/1, server_key/1,
	 server_signature/2, client_signature/2, client_key/1,
	 client_key/2]).

-spec salted_password(binary(), binary(), non_neg_integer()) -> binary().

salted_password(Password, Salt, IterationCount) ->
    hi(jid:resourceprep(Password), Salt, IterationCount).

-spec client_key(binary()) -> binary().

client_key(SaltedPassword) ->
    sha_mac(SaltedPassword, <<"Client Key">>).

-spec stored_key(binary()) -> binary().

stored_key(ClientKey) -> p1_sha:sha1(ClientKey).

-spec server_key(binary()) -> binary().

server_key(SaltedPassword) ->
    sha_mac(SaltedPassword, <<"Server Key">>).

-spec client_signature(binary(), binary()) -> binary().

client_signature(StoredKey, AuthMessage) ->
    sha_mac(StoredKey, AuthMessage).

-spec client_key(binary(), binary()) -> binary().

client_key(ClientProof, ClientSignature) ->
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
				 binary_to_list(ClientProof),
				 binary_to_list(ClientSignature))).

-spec server_signature(binary(), binary()) -> binary().

server_signature(ServerKey, AuthMessage) ->
    sha_mac(ServerKey, AuthMessage).

hi(Password, Salt, IterationCount) ->
    U1 = sha_mac(Password, <<Salt/binary, 0, 0, 0, 1>>),
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
				 binary_to_list(U1),
				 binary_to_list(hi_round(Password, U1,
							 IterationCount - 1)))).

hi_round(Password, UPrev, 1) ->
    sha_mac(Password, UPrev);
hi_round(Password, UPrev, IterationCount) ->
    U = sha_mac(Password, UPrev),
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
				 binary_to_list(U),
				 binary_to_list(hi_round(Password, U,
							 IterationCount - 1)))).

sha_mac(Key, Data) ->
    crypto:hmac(sha, Key, Data).
