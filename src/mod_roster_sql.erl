%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_roster_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_roster).

%% API
-export([init/2, read_roster_version/2, write_roster_version/4,
	 get_roster/2, get_roster_by_jid/3,
	 roster_subscribe/4, get_roster_by_jid_with_groups/3,
	 remove_user/2, update_roster/4, del_roster/3, transaction/2,
	 read_subscription_and_groups/3, get_only_items/2,
	 import/1, import/2, export/1]).

-include("mod_roster.hrl").
-include("ejabberd_sql_pt.hrl").

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
			      SJID = jid:to_string(R#roster.jid),
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
	sql_queries:get_roster_by_jid(LServer, LUser, jid:to_string(LJID)),
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
	    lists:map(fun(I) -> raw_to_record(LServer, I) end, Is);
	_ -> []
    end.

roster_subscribe(_LUser, _LServer, _LJID, Item) ->
    ItemVals = record_to_row(Item),
    sql_queries:roster_subscribe(ItemVals).

transaction(LServer, F) ->
    ejabberd_sql:sql_transaction(LServer, F).

get_roster_by_jid_with_groups(LUser, LServer, LJID) ->
    SJID = jid:to_string(LJID),
    case sql_queries:get_roster_by_jid(LServer, LUser, SJID) of
	{selected, [I]} ->
            R = raw_to_record(LServer, I),
            Groups =
                case sql_queries:get_roster_groups(LServer, LUser, SJID) of
                    {selected, JGrps} when is_list(JGrps) ->
                        [JGrp || {JGrp} <- JGrps];
                    _ -> []
                end,
            R#roster{groups = Groups};
	{selected, []} ->
	    #roster{usj = {LUser, LServer, LJID},
		    us = {LUser, LServer}, jid = LJID}
    end.

remove_user(LUser, LServer) ->
    sql_queries:del_user_roster_t(LServer, LUser),
    {atomic, ok}.

update_roster(LUser, LServer, LJID, Item) ->
    SJID = jid:to_string(LJID),
    ItemVals = record_to_row(Item),
    ItemGroups = Item#roster.groups,
    sql_queries:update_roster(LServer, LUser, SJID, ItemVals,
                               ItemGroups).

del_roster(LUser, LServer, LJID) ->
    SJID = jid:to_string(LJID),
    sql_queries:del_roster(LServer, LUser, SJID).

read_subscription_and_groups(LUser, LServer, LJID) ->
    SJID = jid:to_string(LJID),
    case catch sql_queries:get_subscription(LServer, LUser, SJID) of
	{selected, [{SSubscription}]} ->
	    Subscription = case SSubscription of
			       <<"B">> -> both;
			       <<"T">> -> to;
			       <<"F">> -> from;
			       _ -> none
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

import(LServer) ->
    [{<<"select username, jid, nick, subscription, "
        "ask, askmessage, server, subscribe, type from rosterusers;">>,
      fun([LUser, JID|_] = Row) ->
              Item = raw_to_record(LServer, Row),
              Username = ejabberd_sql:escape(LUser),
              SJID = ejabberd_sql:escape(JID),
              {selected, _, Rows} =
                  ejabberd_sql:sql_query_t(
                    [<<"select grp from rostergroups where username='">>,
                     Username, <<"' and jid='">>, SJID, <<"'">>]),
              Groups = [Grp || [Grp] <- Rows],
              Item#roster{groups = Groups}
      end},
     {<<"select username, version from roster_version;">>,
      fun([LUser, Ver]) ->
              #roster_version{us = {LUser, LServer}, version = Ver}
      end}].

import(_, _) ->
    pass.

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
    case jid:from_string(SJID) of
      error -> error;
      JID ->
	  LJID = jid:tolower(JID),
	  Subscription = case SSubscription of
			   <<"B">> -> both;
			   <<"T">> -> to;
			   <<"F">> -> from;
			   _ -> none
			 end,
	  Ask = case SAsk of
		  <<"S">> -> subscribe;
		  <<"U">> -> unsubscribe;
		  <<"B">> -> both;
		  <<"O">> -> out;
		  <<"I">> -> in;
		  _ -> none
		end,
	  #roster{usj = {User, LServer, LJID},
		  us = {User, LServer}, jid = LJID, name = Nick,
		  subscription = Subscription, ask = Ask,
		  askmessage = SAskMessage}
    end.

record_to_row(
  #roster{us = {LUser, _LServer},
          jid = JID, name = Name, subscription = Subscription,
          ask = Ask, askmessage = AskMessage}) ->
    SJID = jid:to_string(jid:tolower(JID)),
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
