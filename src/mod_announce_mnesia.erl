%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_announce_mnesia).
-behaviour(mod_announce).

%% API
-export([init/2, set_motd_users/2, set_motd/2, delete_motd/1,
	 get_motd/1, is_motd_user/2, set_motd_user/2, import/2]).

-include("jlib.hrl").
-include("mod_announce.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    mnesia:create_table(motd,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, motd)}]),
    mnesia:create_table(motd_users,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, motd_users)}]),
    update_tables().

set_motd_users(_LServer, USRs) ->
    F = fun() ->
		lists:foreach(
		  fun({U, S, _R}) ->
			  mnesia:write(#motd_users{us = {U, S}})
		  end, USRs)
	end,
    mnesia:transaction(F).

set_motd(LServer, Packet) ->
    F = fun() ->
		mnesia:write(#motd{server = LServer, packet = Packet})
	end,
    mnesia:transaction(F).

delete_motd(LServer) ->
    F = fun() ->
		mnesia:delete({motd, LServer}),
		mnesia:write_lock_table(motd_users),
		Users = mnesia:select(
			  motd_users,
			  [{#motd_users{us = '$1', _ = '_'},
			    [{'==', {element, 2, '$1'}, LServer}],
			    ['$1']}]),
		lists:foreach(fun(US) ->
				      mnesia:delete({motd_users, US})
			      end, Users)
	end,
    mnesia:transaction(F).

get_motd(LServer) ->
    case mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Packet}] ->
	    {ok, Packet};
	_ ->
	    error
    end.

is_motd_user(LUser, LServer) ->
    case mnesia:dirty_read({motd_users, {LUser, LServer}}) of
	[#motd_users{}] -> true;
	_ -> false
    end.

set_motd_user(LUser, LServer) ->
    F = fun() ->
		mnesia:write(#motd_users{us = {LUser, LServer}})
	end,
    mnesia:transaction(F).

import(_LServer, #motd{} = Motd) ->
    mnesia:dirty_write(Motd);
import(_LServer, #motd_users{} = Users) ->
    mnesia:dirty_write(Users).

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_tables() ->
    update_motd_table(),
    update_motd_users_table().

update_motd_table() ->
    Fields = record_info(fields, motd),
    case mnesia:table_info(motd, attributes) of
	Fields ->
            ejabberd_config:convert_table_to_binary(
              motd, Fields, set,
              fun(#motd{server = S}) -> S end,
              fun(#motd{server = S, packet = P} = R) ->
                      NewS = iolist_to_binary(S),
                      NewP = fxml:to_xmlel(P),
                      R#motd{server = NewS, packet = NewP}
              end);
	_ ->
	    ?INFO_MSG("Recreating motd table", []),
	    mnesia:transform_table(motd, ignore, Fields)
    end.


update_motd_users_table() ->
    Fields = record_info(fields, motd_users),
    case mnesia:table_info(motd_users, attributes) of
	Fields ->
	    ejabberd_config:convert_table_to_binary(
              motd_users, Fields, set,
              fun(#motd_users{us = {U, _}}) -> U end,
              fun(#motd_users{us = {U, S}} = R) ->
                      NewUS = {iolist_to_binary(U),
                               iolist_to_binary(S)},
                      R#motd_users{us = NewUS}
              end);
	_ ->
	    ?INFO_MSG("Recreating motd_users table", []),
	    mnesia:transform_table(motd_users, ignore, Fields)
    end.
