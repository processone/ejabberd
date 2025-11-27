%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-record(serialize_mam_v1, {
    serverhost :: binary(),
    username :: binary(),
    timestamp :: integer(),
    peer :: binary(),
    type :: chat | groupchat,
    nick :: binary(),
    origin_id :: binary(),
    packet :: binary()
}).
-record(serialize_mam_prefs_v1, {
    serverhost :: binary(),
    username :: binary(),
    default :: atom(),
    always :: term(),
    never :: term()
}).

-record(serialize_roster_v1, {
    serverhost :: binary(),
    username :: binary(),
    version :: binary() | undefined,
    entries :: [{binary(),
		 binary(),
		 [binary()],
		 both | from | to | none,
		 subscribe | unsubscribe | both | in | out | none,
		 binary()}]
}).

-record(serialize_auth_v1, {
    serverhost :: binary(),
    username :: binary(),
    passwords :: [binary() | {atom(), binary(), binary(), binary(), integer()}]
}).

-record(serialize_muc_room_v1, {
    serverhost :: binary(),
    name :: binary(),
    host :: binary(),
    options:: [{atom(), term()}]
}).

-record(serialize_muc_registrations_v1, {
    serverhost :: binary(),
    host :: binary(),
    jid :: binary(),
    nick :: binary()
}).
