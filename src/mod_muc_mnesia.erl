%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_muc_mnesia).

-behaviour(mod_muc).

%% API
-export([init/2, import/2, store_room/4, restore_room/3, forget_room/3,
	 can_use_nick/4, get_rooms/2, get_nick/3, set_nick/4]).

-include("mod_muc.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, Opts) ->
    MyHost = proplists:get_value(host, Opts),
    mnesia:create_table(muc_room,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, muc_room)}]),
    mnesia:create_table(muc_registered,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, muc_registered)}]),
    update_tables(MyHost),
    mnesia:add_table_index(muc_registered, nick).

store_room(_LServer, Host, Name, Opts) ->
    F = fun () ->
		mnesia:write(#muc_room{name_host = {Name, Host},
				       opts = Opts})
	end,
    mnesia:transaction(F).

restore_room(_LServer, Host, Name) ->
    case catch mnesia:dirty_read(muc_room, {Name, Host}) of
	[#muc_room{opts = Opts}] -> Opts;
	_ -> error
    end.

forget_room(_LServer, Host, Name) ->
    F = fun () -> mnesia:delete({muc_room, {Name, Host}})
	end,
    mnesia:transaction(F).

can_use_nick(_LServer, Host, JID, Nick) ->
    {LUser, LServer, _} = jid:tolower(JID),
    LUS = {LUser, LServer},
    case catch mnesia:dirty_select(muc_registered,
				   [{#muc_registered{us_host = '$1',
						     nick = Nick, _ = '_'},
				     [{'==', {element, 2, '$1'}, Host}],
				     ['$_']}])
	of
      {'EXIT', _Reason} -> true;
      [] -> true;
      [#muc_registered{us_host = {U, _Host}}] -> U == LUS
    end.

get_rooms(_LServer, Host) ->
    mnesia:dirty_select(muc_room,
			[{#muc_room{name_host = {'_', Host},
				    _ = '_'},
			  [], ['$_']}]).

get_nick(_LServer, Host, From) ->
    {LUser, LServer, _} = jid:tolower(From),
    LUS = {LUser, LServer},
    case mnesia:dirty_read(muc_registered, {LUS, Host}) of
	[] -> error;
	[#muc_registered{nick = Nick}] -> Nick
    end.

set_nick(_LServer, Host, From, Nick) ->
    {LUser, LServer, _} = jid:tolower(From),
    LUS = {LUser, LServer},
    F = fun () ->
		case Nick of
		    <<"">> ->
			mnesia:delete({muc_registered, {LUS, Host}}),
			ok;
		    _ ->
			Allow = case mnesia:select(
				       muc_registered,
				       [{#muc_registered{us_host =
							     '$1',
							 nick = Nick,
							 _ = '_'},
					 [{'==', {element, 2, '$1'},
					   Host}],
					 ['$_']}]) of
				    [] -> true;
				    [#muc_registered{us_host = {U, _Host}}] ->
					U == LUS
				end,
			if Allow ->
				mnesia:write(#muc_registered{
						us_host = {LUS, Host},
						nick = Nick}),
				ok;
			   true ->
				false
			end
		end
	end,
    mnesia:transaction(F).

import(_LServer, #muc_room{} = R) ->
    mnesia:dirty_write(R);
import(_LServer, #muc_registered{} = R) ->
    mnesia:dirty_write(R).

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_tables(Host) ->
    update_muc_room_table(Host),
    update_muc_registered_table(Host).

update_muc_room_table(_Host) ->
    Fields = record_info(fields, muc_room),
    case mnesia:table_info(muc_room, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            muc_room, Fields, set,
            fun(#muc_room{name_host = {N, _}}) -> N end,
            fun(#muc_room{name_host = {N, H},
                          opts = Opts} = R) ->
                    R#muc_room{name_host = {iolist_to_binary(N),
                                            iolist_to_binary(H)},
                               opts = mod_muc:opts_to_binary(Opts)}
            end);
      _ ->
	  ?INFO_MSG("Recreating muc_room table", []),
	  mnesia:transform_table(muc_room, ignore, Fields)
    end.

update_muc_registered_table(_Host) ->
    Fields = record_info(fields, muc_registered),
    case mnesia:table_info(muc_registered, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            muc_registered, Fields, set,
            fun(#muc_registered{us_host = {_, H}}) -> H end,
            fun(#muc_registered{us_host = {{U, S}, H},
                                nick = Nick} = R) ->
                    R#muc_registered{us_host = {{iolist_to_binary(U),
                                                 iolist_to_binary(S)},
                                                iolist_to_binary(H)},
                                     nick = iolist_to_binary(Nick)}
            end);
      _ ->
	  ?INFO_MSG("Recreating muc_registered table", []),
	  mnesia:transform_table(muc_registered, ignore, Fields)
    end.
