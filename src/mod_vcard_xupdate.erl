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
-include_lib("exmpp/include/exmpp.hrl").

-record(vcard_xupdate, {us, hash}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, _Opts) ->
    HostB = list_to_binary(Host),
    mnesia:create_table(vcard_xupdate,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, vcard_xupdate)}]),
    ejabberd_hooks:add(c2s_update_presence, HostB,
		       ?MODULE, update_presence, 100),
    ejabberd_hooks:add(vcard_set, HostB,
		       ?MODULE, vcard_set, 100),
    ok.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(c2s_update_presence, HostB,
			  ?MODULE, update_presence, 100),
    ejabberd_hooks:delete(vcard_set, HostB,
			  ?MODULE, vcard_set, 100),
    ok.

%%====================================================================
%% Hooks
%%====================================================================

update_presence(Packet, User, Host) ->
    case exmpp_presence:is_presence(Packet) andalso
	exmpp_xml:get_attribute_as_binary(Packet, type, undefined)
	== undefined of
        true ->
	    presence_with_xupdate(Packet, User, Host);
        false ->
            Packet
    end.

vcard_set(User, Server, VCARD) ->
    US = {User, Server},
    case exmpp_xml:get_path(VCARD, [{element, "PHOTO"}, {element, "BINVAL"}, cdata_as_list]) of
	[] ->
	    remove_xupdate(User, Server);
	BinVal ->
	    add_xupdate(User, Server, sha:sha(jlib:decode_base64(BinVal)))
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

presence_with_xupdate(Stanza, User, Host) ->
    XPhotoEl = build_xphotoel(User, Host),
    StanzaReduced = exmpp_xml:remove_element(Stanza, ?NS_VCARD_UPDATE, x),
    exmpp_xml:append_child(StanzaReduced, XPhotoEl).

build_xphotoel(User, Host) ->
    Hash = get_xupdate(User, Host),
    PhotoSubEls = case Hash of
		      Hash when is_list(Hash) ->
			  [exmpp_xml:cdata(Hash)];
		      _ ->
			  []
		  end,
    PhotoEl = [exmpp_xml:element(?NS_VCARD_UPDATE, photo, [], PhotoSubEls)],
    exmpp_xml:element(?NS_VCARD_UPDATE, x, [], PhotoEl).
