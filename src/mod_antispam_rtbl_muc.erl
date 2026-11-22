%%%----------------------------------------------------------------------
%%% File    : mod_antispam_rtbl_muc.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : handle RTBL MUC bans
%%% Created : 20 Nov 2026 by Stefan Strigler <stefan@strigler.de>
%%%
%%% Most of this code was taken and ported from mod_muc_rtbl
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
-module(mod_antispam_rtbl_muc).
-author("stefan@strigler.de").

-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").
-include("mod_muc_room.hrl").

-export([start/1,
         stop/1]).

-export([antispam_rtbl_items/2,
         muc_presence_filter/3,
         muc_process_iq/2]).

start(Host) ->
    ejabberd_hooks:add(antispam_rtbl_items, Host,
               ?MODULE, antispam_rtbl_items, 50),
    ejabberd_hooks:add(muc_filter_presence, Host,
               ?MODULE, muc_presence_filter, 50),
    ejabberd_hooks:add(muc_process_iq, Host,
               ?MODULE, muc_process_iq, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(antispam_rtbl_items, Host,
              ?MODULE, antispam_rtbl_items, 50),
    ejabberd_hooks:delete(muc_filter_presence, Host,
              ?MODULE, muc_presence_filter, 50),
    ejabberd_hooks:delete(muc_process_iq, Host,
              ?MODULE, muc_process_iq, 50),
    ok.

antispam_rtbl_items(Host, {muc_bans, {_Rectracted, Added}}) ->
    case maps:size(Added) of
        0 -> ok;
        _ -> notify_rooms(Host, Added)
    end;
antispam_rtbl_items(_Host, _Other) ->
    ok.

muc_presence_filter(#presence{from = #jid{lserver = Server} = From, lang = Lang} = Packet,
                    #state{server_host = ServerHost}, _Nick) ->
    Allowed = maybe
                  JidClean = jid:encode(jid:tolower(jid:remove_resource(From))),
                  true ?= not is_banned(ServerHost, sha256(Server)),
                  true ?= not is_banned(ServerHost, sha256(JidClean)),
                  true ?= not is_banned(ServerHost, Server),
                  not is_banned(ServerHost, JidClean)
              end,
    case Allowed of
        true -> Packet;
        _ ->
            ErrText = ?T("You have been banned from this room"),
            Err = xmpp:err_forbidden(ErrText, Lang),
            ejabberd_router:route_error(Packet, Err),
            drop
    end.

is_banned(Host, Item) ->
    ets_cache:lookup(mod_antispam:cache_tab(blocked_items), {Host, Item, muc_bans}) =/= error.

muc_process_iq(#iq{type = set, sub_els = [{rtbl_update, Items}]}, #state{users = Users} = State0) ->
    {NewState, _} =
    maps:fold(
      fun(_, #user{role = moderator}, {State, HostHashes}) ->
              {State, HostHashes};
         ({_, S, _} = LJid, #user{jid = JID}, {State, HostHashes}) ->
                {Ban, HH2} =
                    case HostHashes of
                        #{S := Sha} ->
                            {maps:is_key(Sha, Items), HostHashes};
                        _ ->
                            Sha = sha256(S),
                            {maps:is_key(Sha, Items), HostHashes#{S => Sha}}
                    end,
                Ban2 =
                  maybe
                      false ?= Ban,
                      JidClean = jid:encode(jid:remove_resource(LJid)),
                      Sha2 = sha256(JidClean),
                      false ?= maps:is_key(Sha2, Items),
                      maps:is_key(JidClean, Items)
                  end,
              case Ban2 of
                  true ->
                      {_, _, State2} = mod_muc_room:handle_event({process_item_change,
                                                                  {JID, role, none, <<"Banned by RTBL">>},
                                                                  undefined},
                                                                 normal_state, State),
                      {State2, HH2};
                  _ ->
                      {State, HH2}
              end
      end, {State0, #{}}, Users),
    {stop, {ignore, NewState}};
muc_process_iq(IQ, _State) ->
    IQ.

sha256(Data) ->
    Bin = crypto:hash(sha256, Data),
    str:to_hexlist(Bin).

notify_rooms(Host, Items) ->
    IQ = #iq{type = set, to = jid:make(Host), sub_els = [{rtbl_update, Items}]},
    lists:foreach(
    fun(CHost) ->
        lists:foreach(
        fun({_, _, Pid}) when node(Pid) == node() ->
            mod_muc_room:route(Pid, IQ);
           (_) ->
               ok
        end, mod_muc:get_online_rooms(CHost))
    end, mod_muc_admin:find_hosts(Host)).
