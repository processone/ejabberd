%%%----------------------------------------------------------------------
%%% File    : mod_muc_occupantid.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Add Occupant Ids to stanzas in anonymous MUC rooms (XEP-0421)
%%% Created :
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
%%%----------------------------------------------------------------------

-module(mod_muc_occupantid).

-author('badlop@process-one.net').

-protocol({xep, 421, '1.0.1', '23.10', "complete", ""}).

-behaviour(gen_mod).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").
-include("mod_muc_room.hrl").

-export([start/2, stop/1,
	 mod_options/1, mod_doc/0, depends/2]).
-export([filter_packet/3, clean_obsolete_salts/0]).

%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
    prepare_table(),
    {ok, [{hook, muc_filter_presence, filter_packet, 10},
          {hook, muc_filter_message, filter_packet, 10},
          {hook, config_reloaded, clean_obsolete_salts, 90, global}]}.

stop(_Host) ->
    ok.

%%%
%%% Hooks
%%%

filter_packet(Packet, State, _Nick) ->
    add_occupantid_packet(Packet, State#state.jid).

%%%
%%% XEP-0421 Occupant-id
%%%

add_occupantid_packet(Packet, RoomJid) ->
    From = xmpp:get_from(Packet),
    OccupantId = calculate_occupantid(From, RoomJid),
    OccupantElement = #occupant_id{id = OccupantId},
    xmpp:append_subtags(xmpp:remove_subtag(Packet, OccupantElement), [OccupantElement]).

calculate_occupantid(From, RoomJid) ->
    Term = {get_salt(RoomJid#jid.lserver), RoomJid, jid:remove_resource(From)},
    misc:term_to_base64(crypto:hash(sha256, io_lib:format("~p", [Term]))).

%%%
%%% Table storing rooms' salt
%%%

-record(muc_occupant_id, {service_jid, salt}).

prepare_table() ->
    try
        mnesia:table_info(muc_occupant_id, attributes),
        %% Wait some time to ensure mod_muc is started in all hosts
        timer:apply_after(60000, ?MODULE, clean_obsolete_salts, [])
    catch
        exit:{aborted, {no_exists, _, _}} ->
            create_table()
    end.


create_table() ->
    ejabberd_mnesia:create(?MODULE, muc_occupant_id,
			   [{disc_copies, [node()]},
			    {attributes, record_info(fields, muc_occupant_id)},
			    {type, set}]).


get_salt(ServiceJid) ->
    case mnesia:dirty_read(muc_occupant_id, ServiceJid) of
        [] ->
            Salt = p1_rand:get_string(),
            ok = write_salt(ServiceJid, Salt),
            Salt;
        [#muc_occupant_id{salt = Salt}] ->
            Salt
    end.

write_salt(ServiceJid, Salt) ->
    mnesia:dirty_write(#muc_occupant_id{service_jid = ServiceJid, salt = Salt}).

%% @format-begin
clean_obsolete_salts() ->
    Hosts = ejabberd_option:hosts(),
    MucHosts =
        lists:foldl(fun(Host, Acc) -> gen_mod:get_module_opt_hosts(Host, mod_muc) ++ Acc end,
                    [],
                    Hosts),

    F = fun() ->
           mnesia:write_lock_table(muc_occupant_id),
           Ft = fun(#muc_occupant_id{service_jid = Service, salt = _Salt} = Rec, Acc) ->
                   case lists:member(Service, MucHosts) of
                       true ->
                           Acc;
                       false ->
                           mnesia:delete_object(Rec),
                           [Service | Acc]
                   end
                end,
           mnesia:foldl(Ft, [], muc_occupant_id)
        end,
    case mnesia:transaction(F) of
        {atomic, Res} ->
            ?DEBUG("Deleted Salts for occupant-id of MUC services: ~p", [Res]),
            ok;
        _ ->
            ok
    end.
%% @format-end

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
              "in all semi-anonymous rooms.")],
      note => "added in 23.10"
     }.

depends(_, _) ->
    [{mod_muc, hard}].
