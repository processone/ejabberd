%%%----------------------------------------------------------------------
%%% File    : mod_muc_occupantid.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Add Occupant Ids to stanzas in anonymous MUC rooms (XEP-0421)
%%% Created :
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

-module(mod_muc_occupantid).

-author('badlop@process-one.net').

-protocol({xep, 421, '0.1.0', '23.10', "", ""}).

-behaviour(gen_mod).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").
-include("mod_muc_room.hrl").

-export([start/2, stop/1,
	 mod_options/1, mod_doc/0, depends/2]).
-export([filter_packet/3, remove_room/3]).

%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
    create_table(),
    {ok, [{hook, muc_filter_presence, filter_packet, 10},
          {hook, muc_filter_message, filter_packet, 10},
          {hook, remove_room, remove_room, 50}]}.

stop(_Host) ->
    ok.

%%%
%%% Hooks
%%%

filter_packet(Packet, State, _Nick) ->
    add_occupantid_packet(Packet, State#state.jid).

remove_room(_LServer, Name, Host) ->
    delete_salt(jid:make(Name, Host)).

%%%
%%% XEP-0421 Occupant-id
%%%

add_occupantid_packet(Packet, RoomJid) ->
    From = xmpp:get_from(Packet),
    OccupantId = calculate_occupantid(From, RoomJid),
    OccupantElement = #occupant_id{id = OccupantId},
    xmpp:set_subtag(Packet, OccupantElement).

calculate_occupantid(From, RoomJid) ->
    Term = {jid:remove_resource(From), get_salt(RoomJid)},
    misc:term_to_base64(crypto:hash(sha256, io_lib:format("~p", [Term]))).

%%%
%%% Table storing rooms' salt
%%%

-record(muc_occupant_id, {room_jid, salt}).

create_table() ->
    ejabberd_mnesia:create(?MODULE, muc_occupant_id,
			   [{ram_copies, [node()]},
			    {local_content, true},
			    {attributes, record_info(fields, muc_occupant_id)},
			    {type, set}]).


get_salt(RoomJid) ->
    case mnesia:dirty_read(muc_occupant_id, RoomJid) of
        [] ->
            Salt = p1_rand:get_string(),
            ok = write_salt(RoomJid, Salt),
            Salt;
        [#muc_occupant_id{salt = Salt}] ->
            Salt
    end.

write_salt(RoomJid, Salt) ->
    mnesia:dirty_write(#muc_occupant_id{room_jid = RoomJid, salt = Salt}).

delete_salt(RoomJid) ->
    mnesia:dirty_delete(muc_occupant_id, RoomJid).

%%%
%%% Doc
%%%

mod_options(_Host) ->
    [].

mod_doc() ->
    #{desc =>
          [?T("This module implements "
              "https://xmpp.org/extensions/xep-0421.html"
              "[XEP-0421: Anonymous unique occupant identifiers for MUCs]."), "",
           ?T("When the module is enabled, the feature is enabled "
              "in all semi-anonymous rooms."), "",
           ?T("This module is available since ejabberd 23.10.")]
     }.

depends(_, _) ->
    [{mod_muc, hard}].
