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
	 import/1, import/3, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-record(vcard_xupdate, {us = {<<>>, <<>>} :: {binary(), binary()},
                        hash = <<>>       :: binary()}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    case gen_mod:db_type(Host, Opts) of
      mnesia ->
	  mnesia:create_table(vcard_xupdate,
			      [{disc_copies, [node()]},
			       {attributes,
				record_info(fields, vcard_xupdate)}]),
          update_table();
      _ -> ok
    end,
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

%%====================================================================
%% Hooks
%%====================================================================

update_presence(#xmlel{name = <<"presence">>, attrs = Attrs} = Packet,
  User, Host) ->
    case xml:get_attr_s(<<"type">>, Attrs) of
      <<>> -> presence_with_xupdate(Packet, User, Host);
      _ -> Packet
    end;
update_presence(Packet, _User, _Host) -> Packet.

vcard_set(LUser, LServer, VCARD) ->
    US = {LUser, LServer},
    case xml:get_path_s(VCARD,
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
    add_xupdate(LUser, LServer, Hash,
		gen_mod:db_type(LServer, ?MODULE)).

add_xupdate(LUser, LServer, Hash, mnesia) ->
    F = fun () ->
		mnesia:write(#vcard_xupdate{us = {LUser, LServer},
					    hash = Hash})
	end,
    mnesia:transaction(F);
add_xupdate(LUser, LServer, Hash, riak) ->
    {atomic, ejabberd_riak:put(#vcard_xupdate{us = {LUser, LServer},
                                              hash = Hash},
			       vcard_xupdate_schema())};
add_xupdate(LUser, LServer, Hash, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    SHash = ejabberd_odbc:escape(Hash),
    F = fun () ->
		odbc_queries:update_t(<<"vcard_xupdate">>,
				      [<<"username">>, <<"hash">>],
				      [Username, SHash],
				      [<<"username='">>, Username, <<"'">>])
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

get_xupdate(LUser, LServer) ->
    get_xupdate(LUser, LServer,
		gen_mod:db_type(LServer, ?MODULE)).

get_xupdate(LUser, LServer, mnesia) ->
    case mnesia:dirty_read(vcard_xupdate, {LUser, LServer})
	of
      [#vcard_xupdate{hash = Hash}] -> Hash;
      _ -> undefined
    end;
get_xupdate(LUser, LServer, riak) ->
    case ejabberd_riak:get(vcard_xupdate, vcard_xupdate_schema(),
			   {LUser, LServer}) of
        {ok, #vcard_xupdate{hash = Hash}} -> Hash;
        _ -> undefined
    end;
get_xupdate(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case ejabberd_odbc:sql_query(LServer,
				 [<<"select hash from vcard_xupdate where "
				    "username='">>,
				  Username, <<"';">>])
	of
      {selected, [<<"hash">>], [[Hash]]} -> Hash;
      _ -> undefined
    end.

remove_xupdate(LUser, LServer) ->
    remove_xupdate(LUser, LServer,
		   gen_mod:db_type(LServer, ?MODULE)).

remove_xupdate(LUser, LServer, mnesia) ->
    F = fun () ->
		mnesia:delete({vcard_xupdate, {LUser, LServer}})
	end,
    mnesia:transaction(F);
remove_xupdate(LUser, LServer, riak) ->
    {atomic, ejabberd_riak:delete(vcard_xupdate, {LUser, LServer})};
remove_xupdate(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    F = fun () ->
		ejabberd_odbc:sql_query_t([<<"delete from vcard_xupdate where username='">>,
					   Username, <<"';">>])
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

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

vcard_xupdate_schema() ->
    {record_info(fields, vcard_xupdate), #vcard_xupdate{}}.

update_table() ->
    Fields = record_info(fields, vcard_xupdate),
    case mnesia:table_info(vcard_xupdate, attributes) of
      Fields ->
            ejabberd_config:convert_table_to_binary(
              vcard_xupdate, Fields, set,
              fun(#vcard_xupdate{us = {U, _}}) -> U end,
              fun(#vcard_xupdate{us = {U, S}, hash = Hash} = R) ->
                      R#vcard_xupdate{us = {iolist_to_binary(U),
                                            iolist_to_binary(S)},
                                      hash = iolist_to_binary(Hash)}
              end);
        _ ->            
            ?INFO_MSG("Recreating vcard_xupdate table", []),
            mnesia:transform_table(vcard_xupdate, ignore, Fields)
    end.

export(_Server) ->
    [{vcard_xupdate,
      fun(Host, #vcard_xupdate{us = {LUser, LServer}, hash = Hash})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              SHash = ejabberd_odbc:escape(Hash),
              [[<<"delete from vcard_xupdate where username='">>,
                Username, <<"';">>],
               [<<"insert into vcard_xupdate(username, "
                  "hash) values ('">>,
                Username, <<"', '">>, SHash, <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, hash from vcard_xupdate;">>,
      fun([LUser, Hash]) ->
              #vcard_xupdate{us = {LUser, LServer}, hash = Hash}
      end}].

import(_LServer, mnesia, #vcard_xupdate{} = R) ->
    mnesia:dirty_write(R);
import(_LServer, riak, #vcard_xupdate{} = R) ->
    ejabberd_riak:put(R, vcard_xupdate_schema());
import(_, _, _) ->
    pass.

mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(_) -> [db_type].
