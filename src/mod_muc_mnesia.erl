%%%-------------------------------------------------------------------
%%% File    : mod_muc_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_muc_mnesia).

-behaviour(mod_muc).
-behaviour(mod_muc_room).

%% API
-export([init/2, import/3, store_room/4, restore_room/3, forget_room/3,
	 can_use_nick/4, get_rooms/2, get_nick/3, set_nick/4]).
-export([set_affiliation/6, set_affiliations/4, get_affiliation/5,
	 get_affiliations/3, search_affiliation/4]).

-include("jlib.hrl").
-include("mod_muc.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, Opts) ->
    MyHost = proplists:get_value(host, Opts),
    ejabberd_mnesia:create(?MODULE, muc_room,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, muc_room)}]),
    ejabberd_mnesia:create(?MODULE, muc_registered,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, muc_registered)},
			 {index, [nick]}]),
    update_tables(MyHost).

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

set_affiliation(_ServerHost, _Room, _Host, _JID, _Affiliation, _Reason) ->
    {error, not_implemented}.

set_affiliations(_ServerHost, _Room, _Host, _Affiliations) ->
    {error, not_implemented}.

get_affiliation(_ServerHost, _Room, _Host, _LUser, _LServer) ->
    {error, not_implemented}.

get_affiliations(_ServerHost, _Room, _Host) ->
    {error, not_implemented}.

search_affiliation(_ServerHost, _Room, _Host, _Affiliation) ->
    {error, not_implemented}.

import(_LServer, <<"muc_room">>,
       [Name, RoomHost, SOpts, _TimeStamp]) ->
    Opts = mod_muc:opts_to_binary(ejabberd_sql:decode_term(SOpts)),
    mnesia:dirty_write(
      #muc_room{name_host = {Name, RoomHost},
                opts = Opts});
import(_LServer, <<"muc_registered">>,
       [J, RoomHost, Nick, _TimeStamp]) ->
    #jid{user = U, server = S} = jid:from_string(J),
    mnesia:dirty_write(
      #muc_registered{us_host = {{U, S}, RoomHost},
                      nick = Nick}).

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
