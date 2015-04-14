%%%----------------------------------------------------------------------
%%% File    : cyrsasl_plain.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : PLAIN SASL mechanism
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(cyrsasl_plain).

-author('alexey@process-one.net').

-export([start/1, stop/0, mech_new/4, mech_step/2, parse/1]).

-behaviour(cyrsasl).

-record(state, {check_password}).

start(_Opts) ->
    cyrsasl:register_mechanism(<<"PLAIN">>, ?MODULE, plain),
    ok.

stop() -> ok.

mech_new(_Host, _GetPassword, CheckPassword, _CheckPasswordDigest) ->
    {ok, #state{check_password = CheckPassword}}.

mech_step(State, ClientIn) ->
    case prepare(ClientIn) of
      [AuthzId, User, Password] ->
	  case (State#state.check_password)(User, Password) of
	    {true, AuthModule} ->
		{ok,
		 [{username, User}, {authzid, AuthzId},
		  {auth_module, AuthModule}]};
	    _ -> {error, <<"not-authorized">>, User}
	  end;
      _ -> {error, <<"bad-protocol">>}
    end.

prepare(ClientIn) ->
    case parse(ClientIn) of
      [<<"">>, UserMaybeDomain, Password] ->
	  case parse_domain(UserMaybeDomain) of
	    %% <NUL>login@domain<NUL>pwd
	    [User, _Domain] -> [UserMaybeDomain, User, Password];
	    %% <NUL>login<NUL>pwd
	    [User] -> [<<"">>, User, Password]
	  end;
      %% login@domain<NUL>login<NUL>pwd
      [AuthzId, User, Password] -> [AuthzId, User, Password];
      _ -> error
    end.

parse(S) -> parse1(binary_to_list(S), "", []).

parse1([0 | Cs], S, T) ->
    parse1(Cs, "", [list_to_binary(lists:reverse(S)) | T]);
parse1([C | Cs], S, T) -> parse1(Cs, [C | S], T);
%parse1([], [], T) ->
%    lists:reverse(T);
parse1([], S, T) ->
    lists:reverse([list_to_binary(lists:reverse(S)) | T]).

parse_domain(S) -> parse_domain1(binary_to_list(S), "", []).

parse_domain1([$@ | Cs], S, T) ->
    parse_domain1(Cs, "", [list_to_binary(lists:reverse(S)) | T]);
parse_domain1([C | Cs], S, T) ->
    parse_domain1(Cs, [C | S], T);
parse_domain1([], S, T) ->
    lists:reverse([list_to_binary(lists:reverse(S)) | T]).
