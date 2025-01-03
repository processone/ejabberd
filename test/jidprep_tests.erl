%%%-------------------------------------------------------------------
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Created : 11 Sep 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2025   ProcessOne
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

-module(jidprep_tests).

%% API
-compile(export_all).
-import(suite, [send_recv/2, disconnect/1, is_feature_advertised/2,
		server_jid/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {jidprep_single, [sequence],
     [single_test(feature_enabled),
      single_test(normalize_jid)]}.

feature_enabled(Config) ->
    true = is_feature_advertised(Config, ?NS_JIDPREP_0),
    disconnect(Config).

normalize_jid(Config) ->
    ServerJID = server_jid(Config),
    OrigJID = jid:decode(<<"Romeo@Example.COM/Orchard">>),
    NormJID = jid:decode(<<"romeo@example.com/Orchard">>),
    Request = #jidprep{jid = OrigJID},
    #iq{type = result, sub_els = [#jidprep{jid = NormJID}]} =
        send_recv(Config, #iq{type = get, to = ServerJID,
			      sub_els = [Request]}),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("jidprep_" ++ atom_to_list(T)).
