%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
		 boolean(),
		 subscribe | unsubscribe | both | in | out | none,
		 binary()}]
}).

-record(serialize_roster_v2, {
    serverhost :: binary(),
    username :: binary(),
    version :: binary() | undefined,
    entries :: [{binary(),
                 binary(),
                 [binary()],
                 both | from | to | none,
                 boolean(),
                 subscribe | unsubscribe | both | in | out | none,
                 binary(),
                 boolean()}]
}).

-record(serialize_auth_v1, {
    serverhost :: binary(),
    username :: binary(),
    passwords :: [binary() | {sha | sha256 | sha512, binary(), binary(), binary(), integer()}]
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

-record(serialize_privacy_v1, {
    serverhost :: binary(),
    username :: binary(),
    default :: binary(),
    lists :: [{binary(), [{
        nothing | none | both | from | to | binary(),
        allow | deny,
        integer(),
        boolean(),
        boolean(),
        boolean(),
        boolean(),
        boolean()}]}]
}).

-record(serialize_pubsub_subscription_v1, {
    subid :: binary(),
    subscription :: none | subscribed | pending | unconfigured,
    options :: [{atom(), term()}]
}).

-record(serialize_pubsub_state_v1, {
    jid :: binary(),
    items :: [binary()],
    affiliation :: none | owner | publisher | publish_only | member | outcast,
    subscriptions = [#serialize_pubsub_subscription_v1{}]
}).

-record(serialize_pubsub_item_v1, {
    id :: binary(),
    created :: {binary(), binary()} | undefined,
    modified :: {binary(), binary()} | undefined,
    xml :: binary()
}).

-record(serialize_pubsub_v1, {
    serverhost :: binary(),
    jid :: binary(),
    node :: binary(),
    parents :: [binary()],
    plugin :: binary(),
    options :: [{atom(), term()}],
    states :: [#serialize_pubsub_state_v1{}],
    items :: [#serialize_pubsub_item_v1{}]
}).
