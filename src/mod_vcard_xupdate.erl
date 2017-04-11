%%%----------------------------------------------------------------------
%%% File    : mod_vcard_xupdate.erl
%%% Author  : Igor Goryachev <igor@goryachev.org>
%%% Purpose : Add avatar hash in presence on behalf of client (XEP-0153)
%%% Created : 9 Mar 2007 by Igor Goryachev <igor@goryachev.org>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(mod_vcard_xupdate).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3]).

-export([update_presence/1, vcard_set/3, export/1,
	 import_info/0, import/5, import_start/2,
	 mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), [binary()]) -> ok.
-callback add_xupdate(binary(), binary(), binary()) -> {atomic, any()}.
-callback get_xupdate(binary(), binary()) -> binary() | undefined.
-callback remove_xupdate(binary(), binary()) -> {atomic, any()}.

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(c2s_self_presence, Host, ?MODULE,
		       update_presence, 100),
    ejabberd_hooks:add(vcard_set, Host, ?MODULE, vcard_set,
		       100),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_self_presence, Host,
			  ?MODULE, update_presence, 100),
    ejabberd_hooks:delete(vcard_set, Host, ?MODULE,
			  vcard_set, 100),
    ok.

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    ok.

depends(_Host, _Opts) ->
    [].

%%====================================================================
%% Hooks
%%====================================================================
-spec update_presence({presence(), ejabberd_c2s:state()})
      -> {presence(), ejabberd_c2s:state()}.
update_presence({#presence{type = available} = Pres,
		 #{jid := #jid{luser = LUser, lserver = LServer}} = State}) ->
    Hash = get_xupdate(LUser, LServer),
    Pres1 = xmpp:set_subtag(Pres, #vcard_xupdate{hash = Hash}),
    {Pres1, State};
update_presence(Acc) ->
    Acc.

-spec vcard_set(binary(), binary(), xmlel()) -> ok.
vcard_set(LUser, LServer, VCARD) ->
    US = {LUser, LServer},
    case fxml:get_path_s(VCARD,
			[{elem, <<"PHOTO">>}, {elem, <<"BINVAL">>}, cdata])
	of
      <<>> -> remove_xupdate(LUser, LServer);
      BinVal ->
	  add_xupdate(LUser, LServer,
		      str:sha(misc:decode_base64(BinVal)))
    end,
    ejabberd_sm:force_update_presence(US).

%%====================================================================
%% Storage
%%====================================================================

add_xupdate(LUser, LServer, Hash) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:add_xupdate(LUser, LServer, Hash).

get_xupdate(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_xupdate(LUser, LServer).

remove_xupdate(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_xupdate(LUser, LServer).

import_info() ->
    [{<<"vcard_xupdate">>, 3}].

import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []).

import(LServer, {sql, _}, DBType, Tab, [LUser, Hash, TimeStamp]) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Tab, [LUser, Hash, TimeStamp]).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

%%====================================================================
%% Options
%%====================================================================
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(_) -> [db_type].
