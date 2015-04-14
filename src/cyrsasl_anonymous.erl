%%%----------------------------------------------------------------------
%%% File    : cyrsasl_anonymous.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : ANONYMOUS SASL mechanism
%%%  See http://www.ietf.org/internet-drafts/draft-ietf-sasl-anon-05.txt
%%% Created : 23 Aug 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
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

-module(cyrsasl_anonymous).

-export([start/1, stop/0, mech_new/4, mech_step/2]).

-behaviour(cyrsasl).

-record(state, {server = <<"">> :: binary()}).

start(_Opts) ->
    cyrsasl:register_mechanism(<<"ANONYMOUS">>, ?MODULE, plain),
    ok.

stop() -> ok.

mech_new(Host, _GetPassword, _CheckPassword, _CheckPasswordDigest) ->
    {ok, #state{server = Host}}.

mech_step(#state{server = Server}, _ClientIn) ->
    User = iolist_to_binary([randoms:get_string()
			     | [jlib:integer_to_binary(X)
				|| X <- tuple_to_list(now())]]),
    case ejabberd_auth:is_user_exists(User, Server) of
        true  -> {error, <<"not-authorized">>};
        false -> {ok, [{username, User}, {auth_module, ejabberd_auth_anonymous}]}
    end.
