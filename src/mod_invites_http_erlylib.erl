%%%----------------------------------------------------------------------
%%% File    : mod_invites_http_erlylib.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Elydtl custom tags and filters
%%% Created : Mon Nov 10 2025 by Stefan Strigler <stefan@strigler.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2026 ProcessOne
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
-module(mod_invites_http_erlylib).

-behaviour(erlydtl_library).

-export([version/0, inventory/1]).
-export([jid/1, user/1, strip_protocol/1]).

-include("logger.hrl").

version() ->
    1.

inventory(tags) ->
    [];
inventory(filters) ->
    [{jid, jid}, {user, user}, {token_uri, {mod_invites, token_uri}}, {strip_protocol, strip_protocol}].

jid({User, Server}) ->
    jid:encode(jid:make(User, Server)).

strip_protocol(Uri) ->
    re:replace(Uri, <<"xmpp:">>, <<>>, [{return, binary}]).

user({User, _Server}) ->
    User.
