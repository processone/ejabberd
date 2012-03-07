%%%----------------------------------------------------------------------
%%% File    : cyrsasl_plain.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : PLAIN SASL mechanism
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(cyrsasl_plain).
-author('alexey@process-one.net').

-export([start/1, stop/0, mech_new/1, mech_step/2, parse/1]).

-include("cyrsasl.hrl").

-behaviour(cyrsasl).

%% @type mechstate() = {state, CheckPassword}
%%     CheckPassword = function().

-record(state, {check_password}).

%% @spec (Opts) -> true
%%     Opts = term()

start(_Opts) ->
    cyrsasl:register_mechanism("PLAIN", ?MODULE, plain),
    ok.

%% @spec () -> ok

stop() ->
    ok.

mech_new(#sasl_params{check_password = CheckPassword}) ->
    {ok, #state{check_password = CheckPassword}}.

%% @spec (State, ClientIn) -> Ok | Error
%%     State = mechstate()
%%     ClientIn = string()
%%     Ok = {ok, Props}
%%         Props = [Prop]
%%         Prop = {username, Username} | {authzid, AuthzId} | {auth_module, AuthModule}
%%         Username = string()
%%         AuthzId = string()
%%         AuthModule = atom()
%%     Error = {error, Reason} | {error, Reason, Text, Username}

mech_step(State, ClientIn) ->
    case prepare(ClientIn) of
	[AuthzId, User, Password] ->
	    case (State#state.check_password)(User, Password) of
		{true, AuthModule} ->
		    {ok, [{username, User}, {authzid, AuthzId},
			  {auth_module, AuthModule}]};
		{false, ReasonAuthFail} when is_list(ReasonAuthFail) ->
		    {error, 'not-authorized', ReasonAuthFail, User};
		_ ->
		    {error, 'not-authorized', "", User}
	    end;
	_ ->
	    {error, 'malformed-request'}
    end.

prepare(ClientIn) ->
    case parse(ClientIn) of
	[[], UserMaybeDomain, Password] ->
	    case parse_domain(UserMaybeDomain) of
		%% <NUL>login@domain<NUL>pwd
		[User, _Domain] ->
		    [UserMaybeDomain, User, Password];
		%% <NUL>login<NUL>pwd
		[User] ->
		    ["", User, Password]
	    end;
	%% login@domain<NUL>login<NUL>pwd
	[AuthzId, User, Password] ->
	    [AuthzId, User, Password];
	_ ->
	    error
    end.


%% @hidden

parse(S) ->
    parse1(S, "", []).

%% @hidden

parse1([0 | Cs], S, T) ->
    parse1(Cs, "", [lists:reverse(S) | T]);
parse1([C | Cs], S, T) ->
    parse1(Cs, [C | S], T);
%parse1([], [], T) ->
%    lists:reverse(T);
parse1([], S, T) ->
    lists:reverse([lists:reverse(S) | T]).


parse_domain(S) ->
    parse_domain1(S, "", []).

parse_domain1([$@ | Cs], S, T) ->
    parse_domain1(Cs, "", [lists:reverse(S) | T]);
parse_domain1([C | Cs], S, T) ->
    parse_domain1(Cs, [C | S], T);
parse_domain1([], S, T) ->
    lists:reverse([lists:reverse(S) | T]).
