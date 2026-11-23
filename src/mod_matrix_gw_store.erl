%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_store.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : What a Matrix room of the gateway keeps across a restart
%%% Created : 25 Oct 2026 by Paavel A. <pd@narayana.im>
%%%
%%% The gateway of 26.09 keeps everything in its room processes and in
%%% ram_copies tables, so a restart forgets every room and every direct
%%% chat: the next PDU of a room is refused ("Room doesn't exist") and the
%%% next message from XMPP to a Matrix user makes a new direct room. Here,
%%% in mnesia disc_copies, each room's
%%%
%%%   matrix_room_meta   the room version, what the room is to the gateway
%%%                      (a direct chat between a local and a remote user,
%%%                      or a multi-user room) and its forward extremities
%%%   matrix_room_event  the events a room process needs to go on from
%%%                      there: every state event it has stored (which
%%%                      holds every auth chain) and the forward
%%%                      extremities with their state maps
%%%
%%% Message events are not kept once they stop being extremities: history
%%% after a restart is the XMPP archive's (direct chats) or a backfill
%%% (rooms). The room module decides what to save; this one only stores,
%%% loads and forgets.
%%%
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
%%%-------------------------------------------------------------------
-module(mod_matrix_gw_store).

-export([create_db/0, save_meta/4, save_events/3, delete_events/2,
         load/1, rooms/0, delete/1]).

-record(matrix_room_meta,
        {room_id :: binary(),
         version :: binary(),
         kind :: kind(),
         latest = [] :: [binary()]}).

-record(matrix_room_event,
        {key :: {binary(), binary() | '$1'} | '$1',
         json :: map() | '_' | '$2',
         state_map :: map() | undefined | '_' | '$3'}).

-type kind() :: {direct, {binary(), binary()}, binary(), atom()} | multi.
-export_type([kind/0]).

create_db() ->
    ejabberd_mnesia:create(
      ?MODULE, matrix_room_meta,
      [{disc_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_room_meta)}]),
    ejabberd_mnesia:create(
      ?MODULE, matrix_room_event,
      [{disc_only_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_room_event)}]),
    ok.

%% The room's version, kind and forward extremities. This and every state
%% event (a join, a leave) is synced to disk at once (a room is saved so
%% once, or when its kind changes; state events are few): mnesia flushes its
%% log only every few seconds and dets its buffers when it pleases, so a
%% room joined just before a crash would come back without the join, or not
%% at all. A message is not synced: losing the last of them only makes the
%% next event's prev_events older.
-spec save_meta(binary(), binary(), kind(), [binary()]) -> ok.
save_meta(RoomID, Version, Kind, Latest) ->
    mnesia:dirty_write(#matrix_room_meta{room_id = RoomID, version = Version,
                                         kind = Kind, latest = lists:sort(Latest)}),
    sync().

%% Events as {EventID, JSON, StateMap | undefined}.
-spec save_events(binary(), [{binary(), map(), map() | undefined}], [binary()]) -> ok.
save_events(RoomID, Events, Latest) ->
    lists:foreach(
      fun({EventID, JSON, StateMap}) ->
              mnesia:dirty_write(#matrix_room_event{key = {RoomID, EventID}, json = JSON,
                                                    state_map = StateMap})
      end, Events),
    case mnesia:dirty_read(matrix_room_meta, RoomID) of
        [Meta] -> mnesia:dirty_write(Meta#matrix_room_meta{latest = lists:sort(Latest)});
        [] -> ok
    end,
    case lists:any(fun({_, JSON, _}) -> is_map_key(<<"state_key">>, JSON) end, Events) of
        true -> sync();
        false -> ok
    end.

-spec delete_events(binary(), [binary()]) -> ok.
delete_events(RoomID, EventIDs) ->
    lists:foreach(fun(EventID) -> mnesia:dirty_delete(matrix_room_event, {RoomID, EventID}) end,
                  EventIDs).

%% Everything kept of a room, or error.
-spec load(binary()) -> {ok, #{version := binary(), kind := kind(), latest := [binary()],
                               events := [{binary(), map(), map() | undefined}]}} | error.
load(RoomID) ->
    case mnesia:dirty_read(matrix_room_meta, RoomID) of
        [#matrix_room_meta{version = Version, kind = Kind, latest = Latest}] ->
            Events = mnesia:dirty_select(
                       matrix_room_event,
                       [{#matrix_room_event{key = {RoomID, '$1'}, json = '$2', state_map = '$3'},
                         [], [{{'$1', '$2', '$3'}}]}]),
            {ok, #{version => Version, kind => Kind, latest => Latest, events => Events}};
        [] ->
            error
    end.

-spec rooms() -> [binary()].
rooms() ->
    mnesia:dirty_all_keys(matrix_room_meta).

%% Forgets the room: the gateway's users have left it.
-spec delete(binary()) -> ok.
delete(RoomID) ->
    mnesia:dirty_delete(matrix_room_meta, RoomID),
    Keys = mnesia:dirty_select(
             matrix_room_event,
             [{#matrix_room_event{key = '$1', _ = '_'},
               [{'=:=', {element, 1, '$1'}, {const, RoomID}}], ['$1']}]),
    lists:foreach(fun(K) -> mnesia:dirty_delete(matrix_room_event, K) end, Keys),
    sync().

sync() ->
    _ = mnesia:sync_log(),
    _ = catch dets:sync(matrix_room_event),
    ok.
