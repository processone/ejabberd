%%%----------------------------------------------------------------------
%%% File    : cyrsasl_scram.erl
%%% Author  : Stephen Röttger <stephen.roettger@googlemail.com>
%%% Purpose : SASL SCRAM authentication
%%% Created : 7 Aug 2011 by Stephen Röttger <stephen.roettger@googlemail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(cyrsasl_scram).

-author('stephen.roettger@googlemail.com').

-protocol({rfc, 5802}).

-export([start/1, stop/0, mech_new/4, mech_step/2, format_error/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-behaviour(cyrsasl).

-record(state,
	{step = 2              :: 2 | 4,
         stored_key = <<"">>   :: binary(),
         server_key = <<"">>   :: binary(),
         username = <<"">>     :: binary(),
	 auth_module           :: module(),
         get_password          :: fun((binary()) ->
				  {false | ejabberd_auth:password(), module()}),
         auth_message = <<"">> :: binary(),
         client_nonce = <<"">> :: binary(),
	 server_nonce = <<"">> :: binary()}).

-define(SALT_LENGTH, 16).
-define(NONCE_LENGTH, 16).

-type error_reason() :: unsupported_extension | bad_username |
			not_authorized | saslprep_failed |
			parser_failed | bad_attribute |
			nonce_mismatch | bad_channel_binding.

-export_type([error_reason/0]).

start(_Opts) ->
    cyrsasl:register_mechanism(<<"SCRAM-SHA-1">>, ?MODULE,
			       scram).

stop() -> ok.

-spec format_error(error_reason()) -> {atom(), binary()}.
format_error(unsupported_extension) ->
    {'bad-protocol', <<"Unsupported extension">>};
format_error(bad_username) ->
    {'invalid-authzid', <<"Malformed username">>};
format_error(not_authorized) ->
    {'not-authorized', <<"Invalid username or password">>};
format_error(saslprep_failed) ->
    {'not-authorized', <<"SASLprep failed">>};
format_error(parser_failed) ->
    {'bad-protocol', <<"Response decoding failed">>};
format_error(bad_attribute) ->
    {'bad-protocol', <<"Malformed or unexpected attribute">>};
format_error(nonce_mismatch) ->
    {'bad-protocol', <<"Nonce mismatch">>};
format_error(bad_channel_binding) ->
    {'bad-protocol', <<"Invalid channel binding">>}.

mech_new(_Host, GetPassword, _CheckPassword,
	 _CheckPasswordDigest) ->
    {ok, #state{step = 2, get_password = GetPassword}}.

mech_step(#state{step = 2} = State, ClientIn) ->
    case re:split(ClientIn, <<",">>, [{return, binary}]) of
      [_CBind, _AuthorizationIdentity, _UserNameAttribute, _ClientNonceAttribute, ExtensionAttribute | _]
	  when ExtensionAttribute /= <<"">> ->
	  {error, unsupported_extension};
      [CBind, _AuthorizationIdentity, UserNameAttribute, ClientNonceAttribute | _]
	  when (CBind == <<"y">>) or (CBind == <<"n">>) ->
	  case parse_attribute(UserNameAttribute) of
	    {error, Reason} -> {error, Reason};
	    {_, EscapedUserName} ->
		case unescape_username(EscapedUserName) of
		  error -> {error, bad_username};
		  UserName ->
		      case parse_attribute(ClientNonceAttribute) of
			{$r, ClientNonce} ->
			    {Pass, AuthModule} = (State#state.get_password)(UserName),
			    LPass = if is_binary(Pass) -> jid:resourceprep(Pass);
				       true -> Pass
				    end,
			    if Pass == false ->
				  {error, not_authorized, UserName};
			       LPass == error ->
				  {error, saslprep_failed, UserName};
			       true ->
				  {StoredKey, ServerKey, Salt, IterationCount} =
				      if is_tuple(Pass) -> Pass;
					 true ->
					     TempSalt =
						 randoms:bytes(?SALT_LENGTH),
					     SaltedPassword =
						 scram:salted_password(Pass,
								       TempSalt,
								       ?SCRAM_DEFAULT_ITERATION_COUNT),
					     {scram:stored_key(scram:client_key(SaltedPassword)),
					      scram:server_key(SaltedPassword),
					      TempSalt,
					      ?SCRAM_DEFAULT_ITERATION_COUNT}
				      end,
				  ClientFirstMessageBare =
				      str:substr(ClientIn,
                                                 str:str(ClientIn, <<"n=">>)),
				  ServerNonce =
				      misc:encode_base64(randoms:bytes(?NONCE_LENGTH)),
				  ServerFirstMessage =
                                        iolist_to_binary(
                                          ["r=",
                                           ClientNonce,
                                           ServerNonce,
                                           ",", "s=",
                                           misc:encode_base64(Salt),
                                           ",", "i=",
                                           integer_to_list(IterationCount)]),
				  {continue, ServerFirstMessage,
				   State#state{step = 4, stored_key = StoredKey,
					       server_key = ServerKey,
					       auth_module = AuthModule,
					       auth_message =
						   <<ClientFirstMessageBare/binary,
						     ",", ServerFirstMessage/binary>>,
					       client_nonce = ClientNonce,
					       server_nonce = ServerNonce,
					       username = UserName}}
			    end;
			  _ -> {error, bad_attribute}
		      end
		end
	  end;
      _Else -> {error, parser_failed}
    end;
mech_step(#state{step = 4} = State, ClientIn) ->
    case str:tokens(ClientIn, <<",">>) of
      [GS2ChannelBindingAttribute, NonceAttribute,
       ClientProofAttribute] ->
	  case parse_attribute(GS2ChannelBindingAttribute) of
	    {$c, CVal} ->
		ChannelBindingSupport = binary:at(misc:decode_base64(CVal), 0),
		if (ChannelBindingSupport == $n)
		  or (ChannelBindingSupport == $y) ->
		    Nonce = <<(State#state.client_nonce)/binary,
				(State#state.server_nonce)/binary>>,
		    case parse_attribute(NonceAttribute) of
			{$r, CompareNonce} when CompareNonce == Nonce ->
			    case parse_attribute(ClientProofAttribute) of
			    {$p, ClientProofB64} ->
				  ClientProof = misc:decode_base64(ClientProofB64),
				  AuthMessage = iolist_to_binary(
						    [State#state.auth_message,
						     ",",
						     str:substr(ClientIn, 1,
								    str:str(ClientIn, <<",p=">>)
								    - 1)]),
				  ClientSignature =
				    scram:client_signature(State#state.stored_key,
							     AuthMessage),
				  ClientKey = scram:client_key(ClientProof,
							     ClientSignature),
				  CompareStoredKey = scram:stored_key(ClientKey),
				  if CompareStoredKey == State#state.stored_key ->
					 ServerSignature =
					     scram:server_signature(State#state.server_key,
								    AuthMessage),
					 {ok, [{username, State#state.username},
					       {auth_module, State#state.auth_module},
					       {authzid, State#state.username}],
					  <<"v=",
					    (misc:encode_base64(ServerSignature))/binary>>};
				     true -> {error, not_authorized, State#state.username}
				  end;
			    _ -> {error, bad_attribute}
			    end;
			{$r, _} -> {error, nonce_mismatch};
			_ -> {error, bad_attribute}
		    end;
		  true -> {error, bad_channel_binding}
		end;
	    _ -> {error, bad_attribute}
	  end;
      _ -> {error, parser_failed}
    end.

parse_attribute(<<Name, $=, Val/binary>>) when Val /= <<>> ->
    case is_alpha(Name) of
	true -> {Name, Val};
	false -> {error, bad_attribute}
    end;
parse_attribute(_) ->
    {error, bad_attribute}.

unescape_username(<<"">>) -> <<"">>;
unescape_username(EscapedUsername) ->
    Pos = str:str(EscapedUsername, <<"=">>),
    if Pos == 0 -> EscapedUsername;
       true ->
	   Start = str:substr(EscapedUsername, 1, Pos - 1),
	   End = str:substr(EscapedUsername, Pos),
	   EndLen = byte_size(End),
	   if EndLen < 3 -> error;
	      true ->
		  case str:substr(End, 1, 3) of
		    <<"=2C">> ->
			<<Start/binary, ",",
			  (unescape_username(str:substr(End, 4)))/binary>>;
		    <<"=3D">> ->
			<<Start/binary, "=",
			  (unescape_username(str:substr(End, 4)))/binary>>;
		    _Else -> error
		  end
	   end
    end.

is_alpha(Char) when Char >= $a, Char =< $z -> true;
is_alpha(Char) when Char >= $A, Char =< $Z -> true;
is_alpha(_) -> false.
