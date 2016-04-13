%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_vcard_xupdate_mnesia).
-behaviour(mod_vcard_xupdate).

%% API
-export([init/2, import/2, add_xupdate/3, get_xupdate/2, remove_xupdate/2]).

-include("mod_vcard_xupdate.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    mnesia:create_table(vcard_xupdate,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, vcard_xupdate)}]),
    update_table().

add_xupdate(LUser, LServer, Hash) ->
    F = fun () ->
		mnesia:write(#vcard_xupdate{us = {LUser, LServer},
					    hash = Hash})
	end,
    mnesia:transaction(F).

get_xupdate(LUser, LServer) ->
    case mnesia:dirty_read(vcard_xupdate, {LUser, LServer})
	of
      [#vcard_xupdate{hash = Hash}] -> Hash;
      _ -> undefined
    end.

remove_xupdate(LUser, LServer) ->
    F = fun () ->
		mnesia:delete({vcard_xupdate, {LUser, LServer}})
	end,
    mnesia:transaction(F).

import(_LServer, #vcard_xupdate{} = R) ->
    mnesia:dirty_write(R).

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
