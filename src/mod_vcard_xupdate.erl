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

start(Host, Opts) ->
    case gen_mod:db_type(Opts) of
        mnesia ->
            mnesia:create_table(vcard_xupdate,
                                [{disc_copies, [node()]},
                                 {attributes,
                                  record_info(fields, vcard_xupdate)}]);
        _ ->
            ok
    end,
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
%% Storage
%%====================================================================

add_xupdate(LUser, LServer, Hash) ->
    add_xupdate(LUser, LServer, Hash, gen_mod:db_type(LServer, ?MODULE)).

add_xupdate(LUser, LServer, Hash, mnesia) ->
    F = fun() ->
                mnesia:write(#vcard_xupdate{us = {LUser, LServer}, hash = Hash})
        end,
    mnesia:transaction(F);
add_xupdate(LUser, LServer, Hash, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    SHash = ejabberd_odbc:escape(Hash),
    F = fun() ->
                odbc_queries:update_t(
                  "vcard_xupdate",
                  ["username", "hash"],
                  [Username, SHash],
                  ["username='", Username, "'"])
        end,
    ejabberd_odbc:sql_transaction(LServer, F).

get_xupdate(LUser, LServer) ->
    get_xupdate(LUser, LServer, gen_mod:db_type(LServer, ?MODULE)).

get_xupdate(LUser, LServer, mnesia) ->
    case mnesia:dirty_read(vcard_xupdate, {LUser, LServer}) of
        [#vcard_xupdate{hash = Hash}] ->
            Hash;
        _ ->
            undefined
    end;
get_xupdate(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case ejabberd_odbc:sql_query(
           LServer, ["select hash from vcard_xupdate "
                     "where username='", Username, "';"]) of
        {selected, ["hash"], [{Hash}]} ->
            Hash;
        _ ->
            undefined
    end.

remove_xupdate(LUser, LServer) ->
    remove_xupdate(LUser, LServer, gen_mod:db_type(LServer, ?MODULE)).

remove_xupdate(LUser, LServer, mnesia) ->
    F = fun() ->
                mnesia:delete({vcard_xupdate, {LUser, LServer}})
        end,
    mnesia:transaction(F);
remove_xupdate(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    F = fun() ->
                ejabberd_odbc:sql_query_t(
                  ["delete from vcard_xupdate where "
                   "username='", Username, "';"])
        end,
    ejabberd_odbc:sql_transaction(LServer, F).

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
