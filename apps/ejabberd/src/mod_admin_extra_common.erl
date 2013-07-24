%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_common.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%%-------------------------------------------------------------------

-module(mod_admin_extra_common).
-author('badlop@process-one.net').

-export([
    get_lastactivity_module/1,
    kick_session/4,
    prepare_reason/1
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

get_lastactivity_module(Server) ->
    case lists:member(mod_last, gen_mod:loaded_modules(Server)) of
        true -> mod_last;
        _ -> mod_last_odbc
    end.

kick_session(User, Server, Resource, ReasonText) ->
    kick_this_session(User, Server, Resource, prepare_reason(ReasonText)),
    ok.

kick_this_session(User, Server, Resource, Reason) ->
    ejabberd_router:route(
        jlib:make_jid(<<"">>, <<"">>, <<"">>),
        jlib:make_jid(User, Server, Resource),
        #xmlel{name = <<"broadcast">>, children=[{exit, Reason}]}).

prepare_reason([]) ->
    <<"Kicked by administrator">>;
prepare_reason([Reason]) ->
    prepare_reason(Reason);
prepare_reason(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
prepare_reason(Reason) when is_binary(Reason) ->
    Reason;
prepare_reason(StringList) ->
    prepare_reason(string:join(StringList, "_")).
