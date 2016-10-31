%%%----------------------------------------------------------------------
%%% File    : mod_vcard_xupdate.erl
%%% Author  : Igor Goryachev <igor@goryachev.org>
%%% Purpose : Add avatar hash in presence on behalf of client (XEP-0153)
%%% Created : 9 Mar 2007 by Igor Goryachev <igor@goryachev.org>
%%%----------------------------------------------------------------------

-module(mod_vcard_xupdate).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

-export([update_presence/3, vcard_set/3, export/1,
	 import/1, import/3, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("mod_vcard_xupdate.hrl").
-include("jlib.hrl").

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #vcard_xupdate{}) -> ok | pass.
-callback add_xupdate(binary(), binary(), binary()) -> {atomic, any()}.
-callback get_xupdate(binary(), binary()) -> binary() | undefined.
-callback remove_xupdate(binary(), binary()) -> {atomic, any()}.

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(c2s_update_presence, Host, ?MODULE,
		       update_presence, 100),
    ejabberd_hooks:add(vcard_set, Host, ?MODULE, vcard_set,
		       100),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_update_presence, Host,
			  ?MODULE, update_presence, 100),
    ejabberd_hooks:delete(vcard_set, Host, ?MODULE,
			  vcard_set, 100),
    ok.

depends(_Host, _Opts) ->
    [].

%%====================================================================
%% Hooks
%%====================================================================

update_presence(#xmlel{name = <<"presence">>, attrs = Attrs} = Packet,
  User, Host) ->
    case fxml:get_attr_s(<<"type">>, Attrs) of
      <<>> -> presence_with_xupdate(Packet, User, Host);
      _ -> Packet
    end;
update_presence(Packet, _User, _Host) -> Packet.

vcard_set(LUser, LServer, VCARD) ->
    US = {LUser, LServer},
    case fxml:get_path_s(VCARD,
			[{elem, <<"PHOTO">>}, {elem, <<"BINVAL">>}, cdata])
	of
      <<>> -> remove_xupdate(LUser, LServer);
      BinVal ->
	  add_xupdate(LUser, LServer,
		      p1_sha:sha(jlib:decode_base64(BinVal)))
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

%%%----------------------------------------------------------------------
%%% Presence stanza rebuilding
%%%----------------------------------------------------------------------

presence_with_xupdate(#xmlel{name = <<"presence">>,
			     attrs = Attrs, children = Els},
		      User, Host) ->
    XPhotoEl = build_xphotoel(User, Host),
    Els2 = presence_with_xupdate2(Els, [], XPhotoEl),
    #xmlel{name = <<"presence">>, attrs = Attrs,
	   children = Els2}.

presence_with_xupdate2([], Els2, XPhotoEl) ->
    lists:reverse([XPhotoEl | Els2]);
%% This clause assumes that the x element contains only the XMLNS attribute:
presence_with_xupdate2([#xmlel{name = <<"x">>,
			       attrs = [{<<"xmlns">>, ?NS_VCARD_UPDATE}]}
			| Els],
		       Els2, XPhotoEl) ->
    presence_with_xupdate2(Els, Els2, XPhotoEl);
presence_with_xupdate2([El | Els], Els2, XPhotoEl) ->
    presence_with_xupdate2(Els, [El | Els2], XPhotoEl).

build_xphotoel(User, Host) ->
    Hash = get_xupdate(User, Host),
    PhotoSubEls = case Hash of
		    Hash when is_binary(Hash) -> [{xmlcdata, Hash}];
		    _ -> []
		  end,
    PhotoEl = [#xmlel{name = <<"photo">>, attrs = [],
		      children = PhotoSubEls}],
    #xmlel{name = <<"x">>,
	   attrs = [{<<"xmlns">>, ?NS_VCARD_UPDATE}],
	   children = PhotoEl}.

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, LA) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, LA).

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(_) -> [db_type].
