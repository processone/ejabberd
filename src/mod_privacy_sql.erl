%%%-------------------------------------------------------------------
%%% File    : mod_privacy_sql.erl
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

-module(mod_privacy_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_privacy).

%% API
-export([init/2, process_lists_get/2, process_list_get/3,
	 process_default_set/3, process_active_set/3,
	 remove_privacy_list/3, set_privacy_list/1,
	 set_privacy_list/4, get_user_list/2, get_user_lists/2,
	 remove_user/2, import/1, export/1]).

-export([item_to_raw/1, raw_to_item/1,
	 sql_add_privacy_list/2,
	 sql_get_default_privacy_list/2,
	 sql_get_default_privacy_list_t/1,
	 sql_get_privacy_list_data/3,
	 sql_get_privacy_list_data_by_id_t/1,
	 sql_get_privacy_list_id_t/2,
	 sql_set_default_privacy_list/2, sql_set_privacy_list/2]).

-include("xmpp.hrl").
-include("mod_privacy.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

process_lists_get(LUser, LServer) ->
    Default = case catch sql_get_default_privacy_list(LUser, LServer) of
		{selected, []} -> none;
		{selected, [{DefName}]} -> DefName;
		_ -> none
	      end,
    case catch sql_get_privacy_list_names(LUser, LServer) of
      {selected, Names} ->
	  LItems = lists:map(fun ({N}) -> N end, Names),
	  {Default, LItems};
      _ -> error
    end.

process_list_get(LUser, LServer, Name) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
      {selected, []} -> not_found;
      {selected, [{ID}]} ->
	  case catch sql_get_privacy_list_data_by_id(ID, LServer) of
	    {selected, RItems} ->
		lists:flatmap(fun raw_to_item/1, RItems);
	    _ -> error
	  end;
      _ -> error
    end.

process_default_set(LUser, LServer, none) ->
    case catch sql_unset_default_privacy_list(LUser,
					      LServer)
	of
      {'EXIT', _Reason} -> {atomic, error};
      {error, _Reason} -> {atomic, error};
      _ -> {atomic, ok}
    end;
process_default_set(LUser, LServer, Name) ->
    F = fun () ->
		case sql_get_privacy_list_names_t(LUser) of
		  {selected, []} -> not_found;
		  {selected, Names} ->
		      case lists:member({Name}, Names) of
			true -> sql_set_default_privacy_list(LUser, Name), ok;
			false -> not_found
		      end
		end
	end,
    sql_queries:sql_transaction(LServer, F).

process_active_set(LUser, LServer, Name) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
      {selected, []} -> error;
      {selected, [{ID}]} ->
	  case catch sql_get_privacy_list_data_by_id(ID, LServer) of
	    {selected, RItems} ->
		lists:flatmap(fun raw_to_item/1, RItems);
	    _ -> error
	  end;
      _ -> error
    end.

remove_privacy_list(LUser, LServer, Name) ->
    F = fun () ->
		case sql_get_default_privacy_list_t(LUser) of
		  {selected, []} ->
		      sql_remove_privacy_list(LUser, Name), ok;
		  {selected, [{Default}]} ->
		      if Name == Default -> conflict;
			 true -> sql_remove_privacy_list(LUser, Name), ok
		      end
		end
	end,
    sql_queries:sql_transaction(LServer, F).

set_privacy_list(#privacy{us = {LUser, LServer},
			  default = Default,
			  lists = Lists}) ->
    F = fun() ->
		lists:foreach(
		  fun({Name, List}) ->
			  sql_add_privacy_list(LUser, Name),
			  {selected, [<<"id">>], [[I]]} =
			      sql_get_privacy_list_id_t(LUser, Name),
			  RItems = lists:map(fun item_to_raw/1, List),
			  sql_set_privacy_list(I, RItems),
			  if is_binary(Default) ->
				  sql_set_default_privacy_list(LUser, Default),
				  ok;
			     true ->
				  ok
			  end
		  end, Lists)
	end,
    sql_queries:sql_transaction(LServer, F).

set_privacy_list(LUser, LServer, Name, List) ->
    RItems = lists:map(fun item_to_raw/1, List),
    F = fun () ->
		ID = case sql_get_privacy_list_id_t(LUser, Name) of
                         {selected, []} ->
			   sql_add_privacy_list(LUser, Name),
			   {selected, [{I}]} =
			       sql_get_privacy_list_id_t(LUser, Name),
			   I;
		       {selected, [{I}]} -> I
		     end,
		sql_set_privacy_list(ID, RItems),
		ok
	end,
    sql_queries:sql_transaction(LServer, F).

get_user_list(LUser, LServer) ->
    case catch sql_get_default_privacy_list(LUser, LServer)
	of
      {selected, []} -> {none, []};
      {selected, [{Default}]} ->
	  case catch sql_get_privacy_list_data(LUser, LServer,
					       Default) of
              {selected, RItems} ->
		{Default, lists:flatmap(fun raw_to_item/1, RItems)};
	    _ -> {none, []}
	  end;
      _ -> {none, []}
    end.

get_user_lists(LUser, LServer) ->
    Default = case catch sql_get_default_privacy_list(LUser, LServer) of
                  {selected, []} ->
                      none;
                  {selected, [{DefName}]} ->
                      DefName;
                  _ ->
                      none
	      end,
    case catch sql_get_privacy_list_names(LUser, LServer) of
        {selected, Names} ->
            Lists =
                lists:flatmap(
                  fun({Name}) ->
                          case catch sql_get_privacy_list_data(
                                       LUser, LServer, Name) of
                              {selected, RItems} ->
                                  [{Name, lists:flatmap(fun raw_to_item/1, RItems)}];
                              _ ->
                                  []
                          end
                  end, Names),
            {ok, #privacy{default = Default,
                          us = {LUser, LServer},
                          lists = Lists}};
        _ ->
            error
    end.

remove_user(LUser, LServer) ->
    sql_del_privacy_lists(LUser, LServer).

export(Server) ->
    case catch ejabberd_sql:sql_query(jid:nameprep(Server),
				 [<<"select id from privacy_list order by "
				    "id desc limit 1;">>]) of
        {selected, [<<"id">>], [[I]]} ->
            put(id, binary_to_integer(I));
        _ ->
            put(id, 0)
    end,
    [{privacy,
      fun(Host, #privacy{us = {LUser, LServer}, lists = Lists,
                         default = Default})
            when LServer == Host ->
              if Default /= none ->
                      [?SQL("delete from privacy_default_list where"
                            " username=%(LUser)s;"),
                       ?SQL("insert into privacy_default_list(username, name) "
                            "values (%(LUser)s, %(Default)s);")];
                 true ->
                      []
              end ++
                  lists:flatmap(
                    fun({Name, List}) ->
                            RItems = lists:map(fun item_to_raw/1, List),
                            ID = get_id(),
                            [?SQL("delete from privacy_list where"
                                  " username=%(LUser)s and"
                                  " name=%(Name)s;"),
                             ?SQL("insert into privacy_list(username, "
                                  "name, id) values ("
                                  "%(LUser)s, %(Name)s, %(ID)d);"),
                             ?SQL("delete from privacy_list_data where"
                                  " id=%(ID)d;")] ++
                                [?SQL("insert into privacy_list_data(id, t, "
                                      "value, action, ord, match_all, match_iq, "
                                      "match_message, match_presence_in, "
                                      "match_presence_out) "
                                      "values (%(ID)d, %(SType)s, %(SValue)s, %(SAction)s,"
                                      " %(Order)d, %(MatchAll)b, %(MatchIQ)b,"
                                      " %(MatchMessage)b, %(MatchPresenceIn)b,"
                                      " %(MatchPresenceOut)b);")
                                 || {SType, SValue, SAction, Order,
                                     MatchAll, MatchIQ,
                                     MatchMessage, MatchPresenceIn,
                                     MatchPresenceOut} <- RItems]
                    end,
                    Lists);
         (_Host, _R) ->
              []
      end}].

get_id() ->
    ID = get(id),
    put(id, ID + 1),
    ID + 1.

import(_) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
raw_to_item({SType, SValue, SAction, Order, MatchAll,
	     MatchIQ, MatchMessage, MatchPresenceIn,
	     MatchPresenceOut} = Row) ->
    try
        {Type, Value} = case SType of
                            <<"n">> -> {none, none};
                            <<"j">> ->
                                JID = jid:decode(SValue),
				{jid, jid:tolower(JID)};
                            <<"g">> -> {group, SValue};
                            <<"s">> ->
                                case SValue of
                                    <<"none">> -> {subscription, none};
                                    <<"both">> -> {subscription, both};
                                    <<"from">> -> {subscription, from};
                                    <<"to">> -> {subscription, to}
                                end
                        end,
        Action = case SAction of
                     <<"a">> -> allow;
                     <<"d">> -> deny
                 end,
        [#listitem{type = Type, value = Value, action = Action,
                   order = Order, match_all = MatchAll, match_iq = MatchIQ,
                   match_message = MatchMessage,
                   match_presence_in = MatchPresenceIn,
                   match_presence_out = MatchPresenceOut}]
    catch _:_ ->
            ?WARNING_MSG("failed to parse row: ~p", [Row]),
            []
    end.

item_to_raw(#listitem{type = Type, value = Value,
		      action = Action, order = Order, match_all = MatchAll,
		      match_iq = MatchIQ, match_message = MatchMessage,
		      match_presence_in = MatchPresenceIn,
		      match_presence_out = MatchPresenceOut}) ->
    {SType, SValue} = case Type of
			none -> {<<"n">>, <<"">>};
			jid -> {<<"j">>, jid:encode(Value)};
			group -> {<<"g">>, Value};
			subscription ->
			    case Value of
			      none -> {<<"s">>, <<"none">>};
			      both -> {<<"s">>, <<"both">>};
			      from -> {<<"s">>, <<"from">>};
			      to -> {<<"s">>, <<"to">>}
			    end
		      end,
    SAction = case Action of
		allow -> <<"a">>;
		deny -> <<"d">>
	      end,
    {SType, SValue, SAction, Order, MatchAll, MatchIQ,
     MatchMessage, MatchPresenceIn, MatchPresenceOut}.

sql_get_default_privacy_list(LUser, LServer) ->
    sql_queries:get_default_privacy_list(LServer, LUser).

sql_get_default_privacy_list_t(LUser) ->
    sql_queries:get_default_privacy_list_t(LUser).

sql_get_privacy_list_names(LUser, LServer) ->
    sql_queries:get_privacy_list_names(LServer, LUser).

sql_get_privacy_list_names_t(LUser) ->
    sql_queries:get_privacy_list_names_t(LUser).

sql_get_privacy_list_id(LUser, LServer, Name) ->
    sql_queries:get_privacy_list_id(LServer, LUser, Name).

sql_get_privacy_list_id_t(LUser, Name) ->
    sql_queries:get_privacy_list_id_t(LUser, Name).

sql_get_privacy_list_data(LUser, LServer, Name) ->
    sql_queries:get_privacy_list_data(LServer, LUser, Name).

sql_get_privacy_list_data_by_id(ID, LServer) ->
    sql_queries:get_privacy_list_data_by_id(LServer, ID).

sql_get_privacy_list_data_by_id_t(ID) ->
    sql_queries:get_privacy_list_data_by_id_t(ID).

sql_set_default_privacy_list(LUser, Name) ->
    sql_queries:set_default_privacy_list(LUser, Name).

sql_unset_default_privacy_list(LUser, LServer) ->
    sql_queries:unset_default_privacy_list(LServer, LUser).

sql_remove_privacy_list(LUser, Name) ->
    sql_queries:remove_privacy_list(LUser, Name).

sql_add_privacy_list(LUser, Name) ->
    sql_queries:add_privacy_list(LUser, Name).

sql_set_privacy_list(ID, RItems) ->
    sql_queries:set_privacy_list(ID, RItems).

sql_del_privacy_lists(LUser, LServer) ->
    sql_queries:del_privacy_lists(LServer, LUser).
