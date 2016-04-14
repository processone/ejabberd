%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_roster_mnesia).

-behaviour(mod_roster).

%% API
-export([init/2, read_roster_version/2, write_roster_version/4,
	 get_roster/2, get_roster_by_jid/3, get_only_items/2,
	 roster_subscribe/4, get_roster_by_jid_with_groups/3,
	 remove_user/2, update_roster/4, del_roster/3, transaction/2,
	 read_subscription_and_groups/3, import/2]).

-include("jlib.hrl").
-include("mod_roster.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    mnesia:create_table(roster,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, roster_version)}]),
    update_tables(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us).

read_roster_version(LUser, LServer) ->
    US = {LUser, LServer},
    case mnesia:dirty_read(roster_version, US) of
	[#roster_version{version = V}] -> V;
	[] -> error
    end.

write_roster_version(LUser, LServer, InTransaction, Ver) ->
    US = {LUser, LServer},
    if InTransaction ->
	    mnesia:write(#roster_version{us = US, version = Ver});
       true ->
	    mnesia:dirty_write(#roster_version{us = US, version = Ver})
    end.

get_roster(LUser, LServer) ->
    mnesia:dirty_index_read(roster, {LUser, LServer}, #roster.us).

get_roster_by_jid(LUser, LServer, LJID) ->
    case mnesia:read({roster, {LUser, LServer, LJID}}) of
	[] ->
	    #roster{usj = {LUser, LServer, LJID},
		    us = {LUser, LServer}, jid = LJID};
	[I] ->
	    I#roster{jid = LJID, name = <<"">>, groups = [],
		     xs = []}
    end.

get_only_items(LUser, LServer) ->
    get_roster(LUser, LServer).

roster_subscribe(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

get_roster_by_jid_with_groups(LUser, LServer, LJID) ->
    case mnesia:read({roster, {LUser, LServer, LJID}}) of
	[] ->
	    #roster{usj = {LUser, LServer, LJID},
		    us = {LUser, LServer}, jid = LJID};
	[I] -> I
    end.

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () ->
		lists:foreach(
		  fun (R) -> mnesia:delete_object(R) end,
		  mnesia:index_read(roster, US, #roster.us))
	end,
    mnesia:transaction(F).

update_roster(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

del_roster(LUser, LServer, LJID) ->
    mnesia:delete({roster, {LUser, LServer, LJID}}).

read_subscription_and_groups(LUser, LServer, LJID) ->
    case mnesia:dirty_read(roster, {LUser, LServer, LJID}) of
	[#roster{subscription = Subscription, groups = Groups}] ->
	    {Subscription, Groups};
	_ ->
	    error
    end.

transaction(_LServer, F) ->
    mnesia:transaction(F).

import(_LServer, #roster{} = R) ->
    mnesia:dirty_write(R);
import(_LServer, #roster_version{} = RV) ->
    mnesia:dirty_write(RV).

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_tables() ->
    update_roster_table(),
    update_roster_version_table().

update_roster_table() ->
    Fields = record_info(fields, roster),
    case mnesia:table_info(roster, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            roster, Fields, set,
            fun(#roster{usj = {U, _, _}}) -> U end,
            fun(#roster{usj = {U, S, {LU, LS, LR}},
                        us = {U1, S1},
                        jid = {U2, S2, R2},
                        name = Name,
                        groups = Gs,
                        askmessage = Ask,
                        xs = Xs} = R) ->
                    R#roster{usj = {iolist_to_binary(U),
                                    iolist_to_binary(S),
                                    {iolist_to_binary(LU),
                                     iolist_to_binary(LS),
                                     iolist_to_binary(LR)}},
                             us = {iolist_to_binary(U1),
                                   iolist_to_binary(S1)},
                             jid = {iolist_to_binary(U2),
                                    iolist_to_binary(S2),
                                    iolist_to_binary(R2)},
                             name = iolist_to_binary(Name),
                             groups = [iolist_to_binary(G) || G <- Gs],
                             askmessage = try iolist_to_binary(Ask)
					  catch _:_ -> <<"">> end,
                             xs = [fxml:to_xmlel(X) || X <- Xs]}
            end);
      _ ->
	  ?INFO_MSG("Recreating roster table", []),
	  mnesia:transform_table(roster, ignore, Fields)
    end.

%% Convert roster table to support virtual host
%% Convert roster table: xattrs fields become
update_roster_version_table() ->
    Fields = record_info(fields, roster_version),
    case mnesia:table_info(roster_version, attributes) of
        Fields ->
            ejabberd_config:convert_table_to_binary(
              roster_version, Fields, set,
              fun(#roster_version{us = {U, _}}) -> U end,
              fun(#roster_version{us = {U, S}, version = Ver} = R) ->
                      R#roster_version{us = {iolist_to_binary(U),
                                             iolist_to_binary(S)},
                                       version = iolist_to_binary(Ver)}
              end);
        _ ->
            ?INFO_MSG("Recreating roster_version table", []),
            mnesia:transform_table(roster_version, ignore, Fields)
    end.
