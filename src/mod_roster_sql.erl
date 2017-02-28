%%%-------------------------------------------------------------------
%%% File    : mod_roster_sql.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_roster_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_roster).

%% API
-export([init/2, read_roster_version/2, write_roster_version/4,
	 get_roster/2, get_roster_by_jid/3,
	 roster_subscribe/4, get_roster_by_jid_with_groups/3,
	 remove_user/2, update_roster/4, del_roster/3, transaction/2,
	 read_subscription_and_groups/3, get_only_items/2,
	 import/3, export/1, raw_to_record/2]).

-include("mod_roster.hrl").
-include("ejabberd_sql_pt.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

read_roster_version(LUser, LServer) ->
    case sql_queries:get_roster_version(LServer, LUser) of
	{selected, [{Version}]} -> Version;
	{selected, []} -> error
    end.

write_roster_version(LUser, LServer, InTransaction, Ver) ->
    if InTransaction ->
	    sql_queries:set_roster_version(LUser, Ver);
       true ->
	    sql_queries:sql_transaction(
	      LServer,
	      fun () ->
		      sql_queries:set_roster_version(LUser, Ver)
	      end)
    end.

get_roster(LUser, LServer) ->
    case catch sql_queries:get_roster(LServer, LUser) of
        {selected, Items} when is_list(Items) ->
            JIDGroups = case catch sql_queries:get_roster_jid_groups(
                                     LServer, LUser) of
                            {selected, JGrps} when is_list(JGrps) ->
                                JGrps;
                            _ ->
				[]
                        end,
            GroupsDict = lists:foldl(fun({J, G}, Acc) ->
                                             dict:append(J, G, Acc)
                                     end,
                                     dict:new(), JIDGroups),
	    lists:flatmap(
	      fun(I) ->
		      case raw_to_record(LServer, I) of
			  %% Bad JID in database:
			  error -> [];
			  R ->
			      SJID = jid:encode(R#roster.jid),
			      Groups = case dict:find(SJID, GroupsDict) of
					   {ok, Gs} -> Gs;
					   error -> []
				       end,
			      [R#roster{groups = Groups}]
		      end
	      end, Items);
        _ ->
	    []
    end.

get_roster_by_jid(LUser, LServer, LJID) ->
    {selected, Res} =
	sql_queries:get_roster_by_jid(LServer, LUser, jid:encode(LJID)),
    case Res of
	[] ->
	    #roster{usj = {LUser, LServer, LJID},
		    us = {LUser, LServer}, jid = LJID};
	[I] ->
	    R = raw_to_record(LServer, I),
	    case R of
		%% Bad JID in database:
		error ->
		    #roster{usj = {LUser, LServer, LJID},
			    us = {LUser, LServer}, jid = LJID};
		_ ->
		    R#roster{usj = {LUser, LServer, LJID},
			     us = {LUser, LServer}, jid = LJID, name = <<"">>}
	    end
    end.

get_only_items(LUser, LServer) ->
    case catch sql_queries:get_roster(LServer, LUser) of
	{selected, Is} when is_list(Is) ->
	    lists:flatmap(
	      fun(I) ->
		      case raw_to_record(LServer, I) of
			  error -> [];
			  R -> [R]
		      end
	      end, Is);
	_ -> []
    end.

roster_subscribe(_LUser, _LServer, _LJID, Item) ->
    ItemVals = record_to_row(Item),
    sql_queries:roster_subscribe(ItemVals).

transaction(LServer, F) ->
    ejabberd_sql:sql_transaction(LServer, F).

get_roster_by_jid_with_groups(LUser, LServer, LJID) ->
    SJID = jid:encode(LJID),
    case sql_queries:get_roster_by_jid(LServer, LUser, SJID) of
	{selected, [I]} ->
            case raw_to_record(LServer, I) of
		error ->
		    #roster{usj = {LUser, LServer, LJID},
			    us = {LUser, LServer}, jid = LJID};
		R ->
		    Groups =
			case sql_queries:get_roster_groups(LServer, LUser, SJID) of
			    {selected, JGrps} when is_list(JGrps) ->
				[JGrp || {JGrp} <- JGrps];
			    _ -> []
			end,
		    R#roster{groups = Groups}
	    end;
	{selected, []} ->
	    #roster{usj = {LUser, LServer, LJID},
		    us = {LUser, LServer}, jid = LJID}
    end.

remove_user(LUser, LServer) ->
    sql_queries:del_user_roster_t(LServer, LUser),
    {atomic, ok}.

update_roster(LUser, LServer, LJID, Item) ->
    SJID = jid:encode(LJID),
    ItemVals = record_to_row(Item),
    ItemGroups = Item#roster.groups,
    sql_queries:update_roster(LServer, LUser, SJID, ItemVals,
                               ItemGroups).

del_roster(LUser, LServer, LJID) ->
    SJID = jid:encode(LJID),
    sql_queries:del_roster(LServer, LUser, SJID).

read_subscription_and_groups(LUser, LServer, LJID) ->
    SJID = jid:encode(LJID),
    case catch sql_queries:get_subscription(LServer, LUser, SJID) of
	{selected, [{SSubscription}]} ->
	    Subscription = case SSubscription of
			       <<"B">> -> both;
			       <<"T">> -> to;
			       <<"F">> -> from;
			       <<"N">> -> none;
			       <<"">> -> none;
			       _ ->
				   ?ERROR_MSG("~s", [format_row_error(
						       LUser, LServer,
						       {subscription, SSubscription})]),
				   none
			   end,
	    Groups = case catch sql_queries:get_rostergroup_by_jid(
				  LServer, LUser, SJID) of
			 {selected, JGrps} when is_list(JGrps) ->
			     [JGrp || {JGrp} <- JGrps];
			 _ -> []
		     end,
	    {Subscription, Groups};
	_ ->
	    error
    end.

export(_Server) ->
    [{roster,
      fun(Host, #roster{usj = {_LUser, LServer, _LJID}} = R)
            when LServer == Host ->
              ItemVals = record_to_row(R),
              ItemGroups = R#roster.groups,
              sql_queries:update_roster_sql(ItemVals, ItemGroups);
        (_Host, _R) ->
              []
      end},
     {roster_version,
      fun(Host, #roster_version{us = {LUser, LServer}, version = Ver})
            when LServer == Host ->
              [?SQL("delete from roster_version where username=%(LUser)s;"),
               ?SQL("insert into roster_version(username, version) values("
                    " %(LUser)s, %(Ver)s);")];
         (_Host, _R) ->
              []
      end}].

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
raw_to_record(LServer,
	      [User, SJID, Nick, SSubscription, SAsk, SAskMessage,
	       _SServer, _SSubscribe, _SType]) ->
    raw_to_record(LServer,
                  {User, SJID, Nick, SSubscription, SAsk, SAskMessage,
                   _SServer, _SSubscribe, _SType});
raw_to_record(LServer,
	      {User, SJID, Nick, SSubscription, SAsk, SAskMessage,
	       _SServer, _SSubscribe, _SType}) ->
    try jid:decode(SJID) of
      JID ->
	  LJID = jid:tolower(JID),
	  Subscription = case SSubscription of
			     <<"B">> -> both;
			     <<"T">> -> to;
			     <<"F">> -> from;
			     <<"N">> -> none;
			     <<"">> -> none;
			     _ ->
				 ?ERROR_MSG("~s", [format_row_error(
						     User, LServer,
						     {subscription, SSubscription})]),
				 none
			 end,
	  Ask = case SAsk of
		    <<"S">> -> subscribe;
		    <<"U">> -> unsubscribe;
		    <<"B">> -> both;
		    <<"O">> -> out;
		    <<"I">> -> in;
		    <<"N">> -> none;
		    <<"">> -> none;
		    _ ->
			?ERROR_MSG("~s", [format_row_error(User, LServer, {ask, SAsk})]),
			none
		end,
	  #roster{usj = {User, LServer, LJID},
		  us = {User, LServer}, jid = LJID, name = Nick,
		  subscription = Subscription, ask = Ask,
		  askmessage = SAskMessage}
    catch _:{bad_jid, _} ->
	    ?ERROR_MSG("~s", [format_row_error(User, LServer, {jid, SJID})]),
	    error
    end.

record_to_row(
  #roster{us = {LUser, _LServer},
          jid = JID, name = Name, subscription = Subscription,
          ask = Ask, askmessage = AskMessage}) ->
    SJID = jid:encode(jid:tolower(JID)),
    SSubscription = case Subscription of
		      both -> <<"B">>;
		      to -> <<"T">>;
		      from -> <<"F">>;
		      none -> <<"N">>
		    end,
    SAsk = case Ask of
	     subscribe -> <<"S">>;
	     unsubscribe -> <<"U">>;
	     both -> <<"B">>;
	     out -> <<"O">>;
	     in -> <<"I">>;
	     none -> <<"N">>
	   end,
    {LUser, SJID, Name, SSubscription, SAsk, AskMessage}.

format_row_error(User, Server, Why) ->
    [case Why of
	 {jid, JID} -> ["Malformed 'jid' field with value '", JID, "'"];
	 {subscription, Sub} -> ["Malformed 'subscription' field with value '", Sub, "'"];
	 {ask, Ask} -> ["Malformed 'ask' field with value '", Ask, "'"]
     end,
     " detected for ", User, "@", Server, " in table 'rosterusers'"].
