%%%-------------------------------------------------------------------
%%% File    : mod_roster_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(mod_roster_mnesia).

-behaviour(mod_roster).

%% API
-export([init/2, read_roster_version/2, write_roster_version/4,
	 get_roster/2, get_roster_item/3, roster_subscribe/4,
	 remove_user/2, update_roster/4, del_roster/3, transaction/2,
	 read_subscription_and_groups/3, import/3, create_roster/1,
	 process_rosteritems/5,
	 use_cache/2]).
-export([need_transform/1, transform/1]).

-include("mod_roster.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, roster,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, roster)},
			 {index, [us]}]),
    ejabberd_mnesia:create(?MODULE, roster_version,
			[{disc_only_copies, [node()]},
			 {attributes,
			  record_info(fields, roster_version)}]).

use_cache(Host, Table) ->
    case mnesia:table_info(Table, storage_type) of
	disc_only_copies ->
	    mod_roster_opt:use_cache(Host);
	_ ->
	    false
    end.

read_roster_version(LUser, LServer) ->
    US = {LUser, LServer},
    case mnesia:dirty_read(roster_version, US) of
	[#roster_version{version = V}] -> {ok, V};
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
    {ok, mnesia:dirty_index_read(roster, {LUser, LServer}, #roster.us)}.

get_roster_item(LUser, LServer, LJID) ->
    case mnesia:read({roster, {LUser, LServer, LJID}}) of
	[I] -> {ok, I};
	[] -> error
    end.

roster_subscribe(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

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
	[#roster{subscription = Subscription, ask = Ask, groups = Groups}] ->
	    {ok, {Subscription, Ask, Groups}};
	_ ->
	    error
    end.

transaction(_LServer, F) ->
    mnesia:transaction(F).

create_roster(RItem) ->
    mnesia:dirty_write(RItem).

import(_LServer, <<"rosterusers">>, #roster{} = R) ->
    mnesia:dirty_write(R);
import(LServer, <<"roster_version">>, [LUser, Ver]) ->
    RV = #roster_version{us = {LUser, LServer}, version = Ver},
    mnesia:dirty_write(RV).

need_transform({roster, {U, S, _}, _, _, _, _, _, _, _, _})
  when is_list(U) orelse is_list(S) ->
    ?INFO_MSG("Mnesia table 'roster' will be converted to binary", []),
    true;
need_transform({roster_version, {U, S}, Ver})
  when is_list(U) orelse is_list(S) orelse is_list(Ver) ->
    ?INFO_MSG("Mnesia table 'roster_version' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#roster{usj = {U, S, {LU, LS, LR}},
		  us = {U1, S1},
		  jid = {U2, S2, R2},
		  name = Name,
		  groups = Gs,
		  askmessage = Ask,
		  xs = Xs} = R) ->
    R#roster{usj = {iolist_to_binary(U), iolist_to_binary(S),
		    {iolist_to_binary(LU),
		     iolist_to_binary(LS),
		     iolist_to_binary(LR)}},
	     us = {iolist_to_binary(U1), iolist_to_binary(S1)},
	     jid = {iolist_to_binary(U2),
		    iolist_to_binary(S2),
		    iolist_to_binary(R2)},
	     name = iolist_to_binary(Name),
	     groups = [iolist_to_binary(G) || G <- Gs],
	     askmessage = try iolist_to_binary(Ask)
			  catch _:_ -> <<"">> end,
	     xs = [fxml:to_xmlel(X) || X <- Xs]};
transform(#roster_version{us = {U, S}, version = Ver} = R) ->
    R#roster_version{us = {iolist_to_binary(U), iolist_to_binary(S)},
		     version = iolist_to_binary(Ver)}.

%%%===================================================================

process_rosteritems(ActionS, SubsS, AsksS, UsersS, ContactsS) ->
    Action = case ActionS of
                 "list" -> list;
                 "delete" -> delete
             end,
    Subs = lists:foldl(
             fun(any, _) -> [none, from, to, both];
                (Sub, Subs) -> [Sub | Subs]
             end,
             [],
             [list_to_atom(S) || S <- string:tokens(SubsS, ":")]
            ),
    Asks = lists:foldl(
             fun(any, _) -> [none, out, in];
                (Ask, Asks) -> [Ask | Asks]
             end,
             [],
             [list_to_atom(S) || S <- string:tokens(AsksS, ":")]
            ),
    Users = lists:foldl(
              fun("any", _) -> ["*", "*@*"];
                 (U, Us) -> [U | Us]
              end,
              [],
              [S || S <- string:tokens(UsersS, ":")]
             ),
    Contacts = lists:foldl(
                 fun("any", _) -> ["*", "*@*"];
                    (U, Us) -> [U | Us]
                 end,
                 [],
                 [S || S <- string:tokens(ContactsS, ":")]
                ),
    rosteritem_purge({Action, Subs, Asks, Users, Contacts}).

rosteritem_purge(Options) ->
    Num_rosteritems = mnesia:table_info(roster, size),
    io:format("There are ~p roster items in total.~n", [Num_rosteritems]),
    Key = mnesia:dirty_first(roster),
    rip(Key, Options, {0, Num_rosteritems, 0, 0}, []).

rip('$end_of_table', _Options, Counters, Res) ->
    print_progress_line(Counters),
    Res;
rip(Key, Options, {Pr, NT, NV, ND}, Res) ->
    Key_next = mnesia:dirty_next(roster, Key),
    {Action, _, _, _, _} = Options,
    {ND2, Res2} = case decide_rip(Key, Options) of
              true ->
                  Jids = apply_action(Action, Key),
                  {ND+1, [Jids | Res]};
              false ->
                  {ND, Res}
          end,
    NV2 = NV+1,
    Pr2 = print_progress_line({Pr, NT, NV2, ND2}),
    rip(Key_next, Options, {Pr2, NT, NV2, ND2}, Res2).

apply_action(list, Key) ->
    {User, Server, JID} = Key,
    {RUser, RServer, _} = JID,
    Jid1string = <<User/binary, "@", Server/binary>>,
    Jid2string = <<RUser/binary, "@", RServer/binary>>,
    io:format("Matches: ~ts ~ts~n", [Jid1string, Jid2string]),
    {Jid1string, Jid2string};
apply_action(delete, Key) ->
    R = apply_action(list, Key),
    mnesia:dirty_delete(roster, Key),
    R.

print_progress_line({_Pr, 0, _NV, _ND}) ->
    ok;
print_progress_line({Pr, NT, NV, ND}) ->
    Pr2 = trunc((NV/NT)*100),
    case Pr == Pr2 of
        true ->
            ok;
        false ->
            io:format("Progress ~p% - visited ~p - deleted ~p~n", [Pr2, NV, ND])
    end,
    Pr2.

decide_rip(Key, {_Action, Subs, Asks, User, Contact}) ->
    case catch mnesia:dirty_read(roster, Key) of
        [RI] ->
            lists:member(RI#roster.subscription, Subs)
                andalso lists:member(RI#roster.ask, Asks)
                andalso decide_rip_jid(RI#roster.us, User)
                andalso decide_rip_jid(RI#roster.jid, Contact);
        _ ->
            false
    end.

%% Returns true if the server of the JID is included in the servers
decide_rip_jid({UName, UServer, _UResource}, Match_list) ->
    decide_rip_jid({UName, UServer}, Match_list);
decide_rip_jid({UName, UServer}, Match_list) ->
    lists:any(
      fun(Match_string) ->
              MJID = jid:decode(list_to_binary(Match_string)),
              MName = MJID#jid.luser,
              MServer = MJID#jid.lserver,
              Is_server = is_glob_match(UServer, MServer),
              case MName of
                  <<>> when UName == <<>> ->
                      Is_server;
                  <<>> ->
                      false;
                  _ ->
                      Is_server
                          andalso is_glob_match(UName, MName)
              end
      end,
      Match_list).

%% Copied from ejabberd-2.0.0/src/acl.erl
is_regexp_match(String, RegExp) ->
    case ejabberd_regexp:run(String, RegExp) of
        nomatch ->
            false;
        match ->
            true;
        {error, ErrDesc} ->
            io:format(
              "Wrong regexp ~p in ACL: ~p",
              [RegExp, ErrDesc]),
            false
    end.
is_glob_match(String, <<"!", Glob/binary>>) ->
    not is_regexp_match(String, ejabberd_regexp:sh_to_awk(Glob));
is_glob_match(String, Glob) ->
    is_regexp_match(String, ejabberd_regexp:sh_to_awk(Glob)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
