%%%----------------------------------------------------------------------
%%% File    : mod_vcard_xupdate.erl
%%% Author  : Igor Goryachev <igor@goryachev.org>
%%% Purpose : Add avatar hash in presence on behalf of client (XEP-0153)
%%% Created : 9 Mar 2007 by Igor Goryachev <igor@goryachev.org>
%%%----------------------------------------------------------------------

-module(mod_vcard_xupdate).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% hooks
-export([update_presence/3,
	 vcard_set/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(vcard_xupdate, {us, hash}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, _Opts) ->
    mnesia:create_table(vcard_xupdate,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, vcard_xupdate)}]),
    ejabberd_hooks:add(c2s_update_presence, Host,
		       ?MODULE, update_presence, 100),
    ejabberd_hooks:add(vcard_set, Host,
		       ?MODULE, vcard_set, 100),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_update_presence, Host,
			  ?MODULE, update_presence, 100),
    ejabberd_hooks:delete(vcard_set, Host,
			  ?MODULE, vcard_set, 100),
    ok.

%%====================================================================
%% Hooks
%%====================================================================

update_presence({xmlelement, "presence", Attrs, _Els} = Packet, User, Host) ->
    case xml:get_attr_s("type", Attrs) of
        [] ->
	    presence_with_xupdate(Packet, User, Host);
        _ ->
            Packet
    end;
update_presence(Packet, _User, _Host) ->
    Packet.

vcard_set(LUser, LServer, VCARD) ->
    US = {LUser, LServer},
    case xml:get_path_s(VCARD, [{elem, "PHOTO"}, {elem, "BINVAL"}, cdata]) of
	[] ->
	    remove_xupdate(LUser, LServer);
	BinVal ->
	    add_xupdate(LUser, LServer, sha:sha(jlib:decode_base64(BinVal)))
    end,
    ejabberd_sm:force_update_presence(US).

%%====================================================================
%% Mnesia storage
%%====================================================================

add_xupdate(LUser, LServer, Hash) ->
    F = fun() ->
                mnesia:write(#vcard_xupdate{us = {LUser, LServer}, hash = Hash})
        end,
    mnesia:transaction(F).

get_xupdate(LUser, LServer) ->
    case mnesia:dirty_read(vcard_xupdate, {LUser, LServer}) of
        [#vcard_xupdate{hash = Hash}] ->
            Hash;
        _ ->
            undefined
    end.

remove_xupdate(LUser, LServer) ->
    F = fun() ->
                mnesia:delete({vcard_xupdate, {LUser, LServer}})
        end,
    mnesia:transaction(F).

%%%----------------------------------------------------------------------
%%% Presence stanza rebuilding
%%%----------------------------------------------------------------------

presence_with_xupdate({xmlelement, "presence", Attrs, Els}, User, Host) ->
    XPhotoEl = build_xphotoel(User, Host),
    Els2 = presence_with_xupdate2(Els, [], XPhotoEl),
    {xmlelement, "presence", Attrs, Els2}.

presence_with_xupdate2([], Els2, XPhotoEl) ->
    lists:reverse([XPhotoEl | Els2]);
%% This clause assumes that the x element contains only the XMLNS attribute:
presence_with_xupdate2([{xmlelement, "x", [{"xmlns", ?NS_VCARD_UPDATE}], _}
			| Els], Els2, XPhotoEl) ->
    presence_with_xupdate2(Els, Els2, XPhotoEl);
presence_with_xupdate2([El | Els], Els2, XPhotoEl) ->
    presence_with_xupdate2(Els, [El | Els2], XPhotoEl).

build_xphotoel(User, Host) ->
    Hash = get_xupdate(User, Host),
    PhotoSubEls = case Hash of
		      Hash when is_list(Hash) ->
			  [{xmlcdata, Hash}];
		      _ ->
			  []
		  end,
    PhotoEl = [{xmlelement, "photo", [], PhotoSubEls}],
    {xmlelement, "x", [{"xmlns", ?NS_VCARD_UPDATE}], PhotoEl}.
