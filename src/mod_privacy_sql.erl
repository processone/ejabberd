%%%-------------------------------------------------------------------
%%% File    : mod_privacy_sql.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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


-behaviour(mod_privacy).

%% API
-export([init/2, set_default/3, unset_default/2, set_lists/1,
	 set_list/4, get_lists/2, get_list/3, remove_lists/2,
	 remove_list/3, import/1, export/1]).

-export([item_to_raw/1, raw_to_item/1]).

-export([sql_schemas/0]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_privacy.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(Host, _Opts) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, sql_schemas()),
    ok.

sql_schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"privacy_default_list">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"name">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"server_host">>, <<"username">>],
                              unique = true}]},
             #sql_table{
                name = <<"privacy_list">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"name">>, type = text},
                     #sql_column{name = <<"id">>, type = bigserial},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"id">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"server_host">>, <<"username">>,
                                         <<"name">>],
                              unique = true}]},
             #sql_table{
                name = <<"privacy_list_data">>,
                columns =
                    [#sql_column{name = <<"id">>, type = bigint,
                                 opts = [#sql_references{
                                            table = <<"privacy_list">>,
                                            column = <<"id">>}]},
                     #sql_column{name = <<"t">>, type = {char, 1}},
                     #sql_column{name = <<"value">>, type = text},
                     #sql_column{name = <<"action">>, type = {char, 1}},
                     #sql_column{name = <<"ord">>, type = numeric},
                     #sql_column{name = <<"match_all">>, type = boolean},
                     #sql_column{name = <<"match_iq">>, type = boolean},
                     #sql_column{name = <<"match_message">>, type = boolean},
                     #sql_column{name = <<"match_presence_in">>, type = boolean},
                     #sql_column{name = <<"match_presence_out">>, type = boolean}],
                indices = [#sql_index{columns = [<<"id">>]}]}]}].

unset_default(LUser, LServer) ->
    case unset_default_privacy_list(LUser, LServer) of
	ok ->
	    ok;
	_Err ->
	    {error, db_failure}
    end.

set_default(LUser, LServer, Name) ->
    F = fun () ->
		case get_privacy_list_names_t(LUser, LServer) of
		    {selected, []} ->
			{error, notfound};
		    {selected, Names} ->
			case lists:member({Name}, Names) of
			    true ->
				set_default_privacy_list(LUser, LServer, Name);
			    false ->
				{error, notfound}
			end
		end
	end,
    transaction(LServer, F).

remove_list(LUser, LServer, Name) ->
    F = fun () ->
		case get_default_privacy_list_t(LUser, LServer) of
		    {selected, []} ->
			remove_privacy_list_t(LUser, LServer, Name);
		    {selected, [{Default}]} ->
			if Name == Default ->
				{error, conflict};
			   true ->
				remove_privacy_list_t(LUser, LServer, Name)
			end
		end
	end,
    transaction(LServer, F).

set_lists(#privacy{us = {LUser, LServer},
		   default = Default,
		   lists = Lists}) ->
    F = fun() ->
		lists:foreach(
		  fun({Name, List}) ->
			  add_privacy_list(LUser, LServer, Name),
			  {selected, [{I}]} =
			      get_privacy_list_id_t(LUser, LServer, Name),
			  RItems = lists:map(fun item_to_raw/1, List),
			  set_privacy_list(I, RItems),
			  if is_binary(Default) ->
				  set_default_privacy_list(
                                    LUser, LServer, Default);
			     true ->
				  ok
			  end
		  end, Lists)
	end,
    transaction(LServer, F).

set_list(LUser, LServer, Name, List) ->
    RItems = lists:map(fun item_to_raw/1, List),
    F = fun() ->
	{ID, New} = case get_privacy_list_id_t(LUser, LServer, Name) of
			{selected, []} ->
			    add_privacy_list(LUser, LServer, Name),
			    {selected, [{I}]} =
			    get_privacy_list_id_t(LUser, LServer, Name),
			    {I, true};
			{selected, [{I}]} -> {I, false}
		    end,
	case New of
	    false ->
		set_privacy_list(ID, RItems);
	    _ ->
		set_privacy_list_new(ID, RItems)
	end
	end,
    transaction(LServer, F).

get_list(LUser, LServer, default) ->
    case get_default_privacy_list(LUser, LServer) of
	{selected, []} ->
	    error;
	{selected, [{Default}]} ->
	    get_list(LUser, LServer, Default);
	_Err ->
	    {error, db_failure}
    end;
get_list(LUser, LServer, Name) ->
    case get_privacy_list_data(LUser, LServer, Name) of
	{selected, []} ->
	    error;
	{selected, RItems} ->
	    {ok, {Name, lists:flatmap(fun raw_to_item/1, RItems)}};
	_Err ->
	    {error, db_failure}
    end.

get_lists(LUser, LServer) ->
    case get_default_privacy_list(LUser, LServer) of
	{selected, Selected} ->
	    Default = case Selected of
			  [] -> none;
			  [{DefName}] -> DefName
		      end,
	    case get_privacy_list_names(LUser, LServer) of
		{selected, Names} ->
		    case lists:foldl(
			   fun(_, {error, _} = Err) ->
				   Err;
			      ({Name}, Acc) ->
				   case get_privacy_list_data(LUser, LServer, Name) of
				       {selected, RItems} ->
					   Items = lists:flatmap(
						     fun raw_to_item/1,
						     RItems),
					   [{Name, Items}|Acc];
				       _Err ->
					   {error, db_failure}
				   end
			   end, [], Names) of
			{error, Reason} ->
			    {error, Reason};
			Lists ->
			    {ok, #privacy{default = Default,
					  us = {LUser, LServer},
					  lists = Lists}}
		    end;
		_Err ->
		    {error, db_failure}
	    end;
	_Err ->
	    {error, db_failure}
    end.

remove_lists(LUser, LServer) ->
    case del_privacy_lists(LUser, LServer) of
	ok ->
	    ok;
	_Err ->
	    {error, db_failure}
    end.

export(Server) ->
    SqlType = ejabberd_option:sql_type(Server),
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
                            " username=%(LUser)s and %(LServer)H;"),
                       ?SQL_INSERT(
                          "privacy_default_list",
                          ["username=%(LUser)s",
                           "server_host=%(LServer)s",
                           "name=%(Default)s"])];
                 true ->
                      []
              end ++
                  lists:flatmap(
                    fun({Name, List}) ->
                            RItems = lists:map(fun item_to_raw/1, List),
                            ID = get_id(),
                            [?SQL("delete from privacy_list where"
                                  " username=%(LUser)s and %(LServer)H and"
                                  " name=%(Name)s;"),
                             ?SQL_INSERT(
                                "privacy_list",
                                ["username=%(LUser)s",
                                 "server_host=%(LServer)s",
                                 "name=%(Name)s",
                                 "id=%(ID)d"]),
                             ?SQL("delete from privacy_list_data where"
                                  " id=%(ID)d;")] ++
                            case SqlType of
                                pgsql ->
                                [?SQL("insert into privacy_list_data(id, t, "
                                      "value, action, ord, match_all, match_iq, "
                                      "match_message, match_presence_in, "
                                      "match_presence_out) "
                                      "values (%(ID)d, %(SType)s, %(SValue)s, %(SAction)s,"
                                      " %(Order)d, CAST(%(MatchAll)b as boolean), CAST(%(MatchIQ)b as boolean),"
                                      " CAST(%(MatchMessage)b as boolean), CAST(%(MatchPresenceIn)b as boolean),"
                                      " CAST(%(MatchPresenceOut)b as boolean));")
                                 || {SType, SValue, SAction, Order,
                                     MatchAll, MatchIQ,
                                     MatchMessage, MatchPresenceIn,
                                     MatchPresenceOut} <- RItems];
                                _ ->
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
                            end
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
transaction(LServer, F) ->
    case ejabberd_sql:sql_transaction(LServer, F) of
	{atomic, Res} -> Res;
	{aborted, _Reason} -> {error, db_failure}
    end.

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
            ?WARNING_MSG("Failed to parse row: ~p", [Row]),
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

get_default_privacy_list(LUser, LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(name)s from privacy_default_list "
           "where username=%(LUser)s and %(LServer)H")).

get_default_privacy_list_t(LUser, LServer) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(name)s from privacy_default_list "
           "where username=%(LUser)s and %(LServer)H")).

get_privacy_list_names(LUser, LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(name)s from privacy_list"
           " where username=%(LUser)s and %(LServer)H")).

get_privacy_list_names_t(LUser, LServer) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(name)s from privacy_list"
           " where username=%(LUser)s and %(LServer)H")).

get_privacy_list_id_t(LUser, LServer, Name) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(id)d from privacy_list"
           " where username=%(LUser)s and %(LServer)H and name=%(Name)s")).

get_privacy_list_data(LUser, LServer, Name) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(t)s, @(value)s, @(action)s, @(ord)d, @(match_all)b, "
           "@(match_iq)b, @(match_message)b, @(match_presence_in)b, "
           "@(match_presence_out)b from privacy_list_data "
           "where id ="
           " (select id from privacy_list"
           " where username=%(LUser)s and %(LServer)H and name=%(Name)s) "
           "order by ord")).

set_default_privacy_list(LUser, LServer, Name) ->
    ?SQL_UPSERT_T(
       "privacy_default_list",
       ["!username=%(LUser)s",
        "!server_host=%(LServer)s",
        "name=%(Name)s"]).

unset_default_privacy_list(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from privacy_default_list"
		" where username=%(LUser)s and %(LServer)H")) of
	{updated, _} -> ok;
	Err -> Err
    end.

remove_privacy_list_t(LUser, LServer, Name) ->
    case ejabberd_sql:sql_query_t(
	   ?SQL("delete from privacy_list where"
		" username=%(LUser)s and %(LServer)H and name=%(Name)s")) of
	{updated, 0} -> {error, notfound};
	{updated, _} -> ok;
	Err -> Err
    end.

add_privacy_list(LUser, LServer, Name) ->
    ejabberd_sql:sql_query_t(
      ?SQL_INSERT(
         "privacy_list",
         ["username=%(LUser)s",
          "server_host=%(LServer)s",
          "name=%(Name)s"])).

set_privacy_list_new(ID, RItems) ->
    lists:foreach(
	fun({SType, SValue, SAction, Order, MatchAll, MatchIQ,
	     MatchMessage, MatchPresenceIn, MatchPresenceOut}) ->
	    ejabberd_sql:sql_query_t(
		?SQL("insert into privacy_list_data(id, t, "
		     "value, action, ord, match_all, match_iq, "
		     "match_message, match_presence_in, match_presence_out) "
		     "values (%(ID)d, %(SType)s, %(SValue)s, %(SAction)s,"
		     " %(Order)d, %(MatchAll)b, %(MatchIQ)b,"
		     " %(MatchMessage)b, %(MatchPresenceIn)b,"
		     " %(MatchPresenceOut)b)"))
	end,
	RItems).

calculate_difference(List1, List2) ->
    Set1 = gb_sets:from_list(List1),
    Set2 = gb_sets:from_list(List2),
    {gb_sets:to_list(gb_sets:subtract(Set1, Set2)),
     gb_sets:to_list(gb_sets:subtract(Set2, Set1))}.

set_privacy_list(ID, RItems) ->
    case ejabberd_sql:sql_query_t(
	?SQL("select @(t)s, @(value)s, @(action)s, @(ord)d, @(match_all)b, "
	     "@(match_iq)b, @(match_message)b, @(match_presence_in)b, "
	     "@(match_presence_out)b from privacy_list_data "
	     "where id=%(ID)d")) of
	{selected, ExistingItems} ->
	    {ToAdd2, ToDelete2} = calculate_difference(RItems, ExistingItems),
	    ToAdd3 = if
			 ToDelete2 /= [] ->
			     ejabberd_sql:sql_query_t(
				 ?SQL("delete from privacy_list_data where id=%(ID)d")),
			     RItems;
			 true ->
			     ToAdd2
		     end,
	    lists:foreach(
		fun({SType, SValue, SAction, Order, MatchAll, MatchIQ,
		     MatchMessage, MatchPresenceIn, MatchPresenceOut}) ->
		    ejabberd_sql:sql_query_t(
			?SQL("insert into privacy_list_data(id, t, "
			     "value, action, ord, match_all, match_iq, "
			     "match_message, match_presence_in, match_presence_out) "
			     "values (%(ID)d, %(SType)s, %(SValue)s, %(SAction)s,"
			     " %(Order)d, %(MatchAll)b, %(MatchIQ)b,"
			     " %(MatchMessage)b, %(MatchPresenceIn)b,"
			     " %(MatchPresenceOut)b)"))
		end,
		ToAdd3);
	Err ->
	    Err
    end.

del_privacy_lists(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from privacy_list where username=%(LUser)s and %(LServer)H")) of
	{updated, _} ->
	    case ejabberd_sql:sql_query(
		   LServer,
		   ?SQL("delete from privacy_default_list "
			"where username=%(LUser)s and %(LServer)H")) of
		{updated, _} -> ok;
		Err -> Err
	    end;
	Err ->
	    Err
    end.
