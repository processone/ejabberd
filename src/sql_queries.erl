%%%----------------------------------------------------------------------
%%% File    : sql_queries.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : ODBC queries dependind on back-end
%%% Created : by Mickael Remond <mremond@process-one.net>
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

-module(sql_queries).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(ejabberd_config).

-author("mremond@process-one.net").

-export([get_db_type/0, update/5, update_t/4,
	 sql_transaction/2, get_last/2, set_last_t/4, del_last/2,
	 get_password/2, get_password_scram/2, set_password_t/3,
	 set_password_scram_t/6, add_user/3, add_user_scram/6,
	 del_user/2, del_user_return_password/3, list_users/1,
	 list_users/2, users_number/1, users_number/2,
	 add_spool_sql/2, add_spool/2, get_and_del_spool_msg_t/2,
	 del_spool_msg/2, get_roster/2, get_roster_jid_groups/2,
	 get_roster_groups/3, del_user_roster_t/2,
	 get_roster_by_jid/3, get_rostergroup_by_jid/3,
	 del_roster/3, del_roster_sql/2, update_roster/5,
	 update_roster_sql/2, roster_subscribe/1,
	 get_subscription/3, set_private_data/4,
	 set_private_data_sql/3, get_private_data/3,
	 get_private_data/2, del_user_private_storage/2,
	 get_default_privacy_list/2,
	 get_default_privacy_list_t/1, get_privacy_list_names/2,
	 get_privacy_list_names_t/1, get_privacy_list_id/3,
	 get_privacy_list_id_t/2, get_privacy_list_data/3,
	 get_privacy_list_data_by_id/2,
	 get_privacy_list_data_t/2,
	 get_privacy_list_data_by_id_t/1,
	 set_default_privacy_list/2,
	 unset_default_privacy_list/2, remove_privacy_list/2,
	 add_privacy_list/2, set_privacy_list/2,
	 del_privacy_lists/2, set_vcard/26, get_vcard/2,
	 escape/1, count_records_where/3, get_roster_version/2,
	 set_roster_version/2, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%% Almost a copy of string:join/2.
%% We use this version because string:join/2 is relatively
%% new function (introduced in R12B-0).
join([], _Sep) -> [];
join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].

get_db_type() -> generic.

%% Safe atomic update.
update_t(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_sql:sql_query_t([<<"update ">>, Table,
				    <<" set ">>, join(UPairs, <<", ">>),
				    <<" where ">>, Where, <<";">>])
	of
      {updated, 1} -> ok;
      _ ->
		Res = ejabberd_sql:sql_query_t([<<"insert into ">>, Table,
				     <<"(">>, join(Fields, <<", ">>),
				     <<") values ('">>, join(Vals, <<"', '">>),
				     <<"');">>]),
		case Res of
			{updated,1} -> ok;
			_ -> Res
		end
    end.

update(LServer, Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_sql:sql_query(LServer,
				 [<<"update ">>, Table, <<" set ">>,
				  join(UPairs, <<", ">>), <<" where ">>, Where,
				  <<";">>])
	of
      {updated, 1} -> ok;
      _ ->
		Res = ejabberd_sql:sql_query(LServer,
				  [<<"insert into ">>, Table, <<"(">>,
				   join(Fields, <<", ">>), <<") values ('">>,
				   join(Vals, <<"', '">>), <<"');">>]),
		case Res of
			{updated,1} -> ok;
			_ -> Res
		end		   
    end.

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from the ejabberd_sql module to this one (sql_queries)
sql_transaction(LServer, F) ->
    ejabberd_sql:sql_transaction(LServer, F).

get_last(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(seconds)d, @(state)s from last"
           " where username=%(LUser)s")).

set_last_t(LServer, LUser, TimeStamp, Status) ->
    ?SQL_UPSERT(LServer, "last",
                ["!username=%(LUser)s",
                 "seconds=%(TimeStamp)d",
                 "state=%(Status)s"]).

del_last(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from last where username=%(LUser)s")).

get_password(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(password)s from users where username=%(LUser)s")).

get_password_scram(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(password)s, @(serverkey)s, @(salt)s, @(iterationcount)d"
           " from users"
           " where username=%(LUser)s")).

set_password_t(LServer, LUser, Password) ->
    ejabberd_sql:sql_transaction(
      LServer,
      fun () ->
              ?SQL_UPSERT_T(
                 "users",
                 ["!username=%(LUser)s",
                  "password=%(Password)s"])
      end).

set_password_scram_t(LServer, LUser,
                     StoredKey, ServerKey, Salt, IterationCount) ->
    ejabberd_sql:sql_transaction(
      LServer,
      fun () ->
              ?SQL_UPSERT_T(
                 "users",
                 ["!username=%(LUser)s",
                  "password=%(StoredKey)s",
                  "serverkey=%(ServerKey)s",
                  "salt=%(Salt)s",
                  "iterationcount=%(IterationCount)d"])
      end).

add_user(LServer, LUser, Password) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("insert into users(username, password) "
           "values (%(LUser)s, %(Password)s)")).

add_user_scram(LServer, LUser,
               StoredKey, ServerKey, Salt, IterationCount) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("insert into users(username, password, serverkey, salt, "
           "iterationcount) "
           "values (%(LUser)s, %(StoredKey)s, %(ServerKey)s,"
           " %(Salt)s, %(IterationCount)d)")).

del_user(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from users where username=%(LUser)s")).

del_user_return_password(_LServer, LUser, Password) ->
    P =
	ejabberd_sql:sql_query_t(
          ?SQL("select @(password)s from users where username=%(LUser)s")),
    ejabberd_sql:sql_query_t(
      ?SQL("delete from users"
           " where username=%(LUser)s and password=%(Password)s")),
    P.

list_users(LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s from users")).

list_users(LServer, [{from, Start}, {to, End}])
    when is_integer(Start) and is_integer(End) ->
    list_users(LServer,
	       [{limit, End - Start + 1}, {offset, Start - 1}]);
list_users(LServer,
	   [{prefix, Prefix}, {from, Start}, {to, End}])
    when is_binary(Prefix) and is_integer(Start) and
	   is_integer(End) ->
    list_users(LServer,
	       [{prefix, Prefix}, {limit, End - Start + 1},
		{offset, Start - 1}]);
list_users(LServer, [{limit, Limit}, {offset, Offset}])
    when is_integer(Limit) and is_integer(Offset) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s from users "
           "order by username "
           "limit %(Limit)d offset %(Offset)d"));
list_users(LServer,
	   [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
    when is_binary(Prefix) and is_integer(Limit) and
	   is_integer(Offset) ->
    SPrefix = ejabberd_sql:escape_like_arg_circumflex(Prefix),
    SPrefix2 = <<SPrefix/binary, $%>>,
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s from users "
           "where username like %(SPrefix2)s escape '^' "
           "order by username "
           "limit %(Limit)d offset %(Offset)d")).

users_number(LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      fun(pgsql, _) ->
              case
                  ejabberd_config:get_option(
                    {pgsql_users_number_estimate, LServer},
                    fun(V) when is_boolean(V) -> V end,
                    false) of
                  true ->
                      ejabberd_sql:sql_query_t(
                        ?SQL("select @(reltuples :: bigint)d from pg_class"
                             " where oid = 'users'::regclass::oid"));
                  _ ->
                      ejabberd_sql:sql_query_t(
                        ?SQL("select @(count(*))d from users"))
	  end;
         (_Type, _) ->
              ejabberd_sql:sql_query_t(
                ?SQL("select @(count(*))d from users"))
      end).

users_number(LServer, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    SPrefix = ejabberd_sql:escape_like_arg_circumflex(Prefix),
    SPrefix2 = <<SPrefix/binary, $%>>,
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(count(*))d from users "
           "where username like %(SPrefix2)s escape '^'"));
users_number(LServer, []) ->
    users_number(LServer).

add_spool_sql(LUser, XML) ->
    ?SQL("insert into spool(username, xml) values (%(LUser)s, %(XML)s)").

add_spool(LServer, Queries) ->
    ejabberd_sql:sql_transaction(LServer, Queries).

get_and_del_spool_msg_t(LServer, LUser) ->
    F = fun () ->
		Result =
		    ejabberd_sql:sql_query_t(
                      ?SQL("select @(username)s, @(xml)s from spool where "
                           "username=%(LUser)s order by seq;")),
		ejabberd_sql:sql_query_t(
                  ?SQL("delete from spool where username=%(LUser)s;")),
		Result
	end,
    ejabberd_sql:sql_transaction(LServer, F).

del_spool_msg(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from spool where username=%(LUser)s")).

get_roster(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s, @(jid)s, @(nick)s, @(subscription)s, "
           "@(ask)s, @(askmessage)s, @(server)s, @(subscribe)s, "
           "@(type)s from rosterusers where username=%(LUser)s")).

get_roster_jid_groups(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(jid)s, @(grp)s from rostergroups where "
           "username=%(LUser)s")).

get_roster_groups(_LServer, LUser, SJID) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(grp)s from rostergroups"
           " where username=%(LUser)s and jid=%(SJID)s")).

del_user_roster_t(LServer, LUser) ->
    ejabberd_sql:sql_transaction(
      LServer,
      fun () ->
              ejabberd_sql:sql_query_t(
                ?SQL("delete from rosterusers where username=%(LUser)s")),
              ejabberd_sql:sql_query_t(
                ?SQL("delete from rostergroups where username=%(LUser)s"))
      end).

get_roster_by_jid(_LServer, LUser, SJID) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(username)s, @(jid)s, @(nick)s, @(subscription)s,"
           " @(ask)s, @(askmessage)s, @(server)s, @(subscribe)s,"
           " @(type)s from rosterusers"
           " where username=%(LUser)s and jid=%(SJID)s")).

get_rostergroup_by_jid(LServer, LUser, SJID) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(grp)s from rostergroups"
           " where username=%(LUser)s and jid=%(SJID)s")).

del_roster(_LServer, LUser, SJID) ->
    ejabberd_sql:sql_query_t(
      ?SQL("delete from rosterusers"
           " where username=%(LUser)s and jid=%(SJID)s")),
    ejabberd_sql:sql_query_t(
      ?SQL("delete from rostergroups"
           " where username=%(LUser)s and jid=%(SJID)s")).

del_roster_sql(Username, SJID) ->
    [[<<"delete from rosterusers       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>],
     [<<"delete from rostergroups       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>]].

update_roster(_LServer, LUser, SJID, ItemVals,
	      ItemGroups) ->
    roster_subscribe(ItemVals),
    ejabberd_sql:sql_query_t(
      ?SQL("delete from rostergroups"
           " where username=%(LUser)s and jid=%(SJID)s")),
    lists:foreach(
      fun(ItemGroup) ->
              ejabberd_sql:sql_query_t(
                ?SQL("insert into rostergroups(username, jid, grp) "
                     "values (%(LUser)s, %(SJID)s, %(ItemGroup)s)"))
      end,
      ItemGroups).

update_roster_sql({LUser, SJID, Name, SSubscription, SAsk, AskMessage},
		  ItemGroups) ->
    [?SQL("delete from rosterusers where"
          " username=%(LUser)s and jid=%(SJID)s;"),
     ?SQL("insert into rosterusers("
          " username, jid, nick,"
          " subscription, ask, askmessage,"
          " server, subscribe, type) "
          "values ("
          "%(LUser)s, "
          "%(SJID)s, "
          "%(Name)s, "
          "%(SSubscription)s, "
          "%(SAsk)s, "
          "%(AskMessage)s, "
          "'N', '', 'item');"),
     ?SQL("delete from rostergroups where"
          " username=%(LUser)s and jid=%(SJID)s;")]
      ++
      [?SQL("insert into rostergroups(username, jid, grp) "
            "values (%(LUser)s, %(SJID)s, %(ItemGroup)s)")
       || ItemGroup <- ItemGroups].

roster_subscribe({LUser, SJID, Name, SSubscription, SAsk, AskMessage}) ->
    ?SQL_UPSERT_T(
       "rosterusers",
       ["!username=%(LUser)s",
        "!jid=%(SJID)s",
        "nick=%(Name)s",
        "subscription=%(SSubscription)s",
        "ask=%(SAsk)s",
        "askmessage=%(AskMessage)s",
        "server='N'",
        "subscribe=''",
        "type='item'"]).

get_subscription(LServer, LUser, SJID) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(subscription)s from rosterusers "
           "where username=%(LUser)s and jid=%(SJID)s")).

set_private_data(_LServer, LUser, XMLNS, SData) ->
    ?SQL_UPSERT_T(
       "private_storage",
       ["!username=%(LUser)s",
        "!namespace=%(XMLNS)s",
        "data=%(SData)s"]).

set_private_data_sql(LUser, XMLNS, SData) ->
    [?SQL("delete from private_storage where"
          " username=%(LUser)s and namespace=%(XMLNS)s;"),
     ?SQL("insert into private_storage(username, "
          "namespace, data) values ("
          "%(LUser)s, %(XMLNS)s, %(SData)s);")].

get_private_data(LServer, LUser, XMLNS) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(data)s from private_storage"
           " where username=%(LUser)s and namespace=%(XMLNS)s")).

get_private_data(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(namespace)s, @(data)s from private_storage"
           " where username=%(LUser)s")).

del_user_private_storage(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from private_storage"
           " where username=%(LUser)s")).

set_vcard(LServer, LUser, BDay, CTRY, EMail, FN,
	  Family, Given, LBDay, LCTRY, LEMail, LFN,
	  LFamily, LGiven, LLocality, LMiddle, LNickname,
	  LOrgName, LOrgUnit, Locality, Middle, Nickname,
	  OrgName, OrgUnit, SVCARD, User) ->
    ejabberd_sql:sql_transaction(
      LServer,
      fun() ->
              ?SQL_UPSERT(LServer, "vcard",
                          ["!username=%(LUser)s",
                           "vcard=%(SVCARD)s"]),
              ?SQL_UPSERT(LServer, "vcard_search",
                          ["username=%(User)s",
                           "!lusername=%(LUser)s",
                           "fn=%(FN)s",
                           "lfn=%(LFN)s",
                           "family=%(Family)s",
                           "lfamily=%(LFamily)s",
                           "given=%(Given)s",
                           "lgiven=%(LGiven)s",
                           "middle=%(Middle)s",
                           "lmiddle=%(LMiddle)s",
                           "nickname=%(Nickname)s",
                           "lnickname=%(LNickname)s",
                           "bday=%(BDay)s",
                           "lbday=%(LBDay)s",
                           "ctry=%(CTRY)s",
                           "lctry=%(LCTRY)s",
                           "locality=%(Locality)s",
                           "llocality=%(LLocality)s",
                           "email=%(EMail)s",
                           "lemail=%(LEMail)s",
                           "orgname=%(OrgName)s",
                           "lorgname=%(LOrgName)s",
                           "orgunit=%(OrgUnit)s",
                           "lorgunit=%(LOrgUnit)s"])
      end).

get_vcard(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(vcard)s from vcard where username=%(LUser)s")).

get_default_privacy_list(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(name)s from privacy_default_list "
           "where username=%(LUser)s")).

get_default_privacy_list_t(LUser) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(name)s from privacy_default_list "
           "where username=%(LUser)s")).

get_privacy_list_names(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(name)s from privacy_list"
           " where username=%(LUser)s")).

get_privacy_list_names_t(LUser) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(name)s from privacy_list"
           " where username=%(LUser)s")).

get_privacy_list_id(LServer, LUser, Name) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(id)d from privacy_list"
           " where username=%(LUser)s and name=%(Name)s")).

get_privacy_list_id_t(LUser, Name) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(id)d from privacy_list"
           " where username=%(LUser)s and name=%(Name)s")).

get_privacy_list_data(LServer, LUser, Name) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(t)s, @(value)s, @(action)s, @(ord)d, @(match_all)b, "
           "@(match_iq)b, @(match_message)b, @(match_presence_in)b, "
           "@(match_presence_out)b from privacy_list_data "
           "where id ="
           " (select id from privacy_list"
           " where username=%(LUser)s and name=%(Name)s) "
           "order by ord")).

%% Not used?
get_privacy_list_data_t(LUser, Name) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(t)s, @(value)s, @(action)s, @(ord)d, @(match_all)b, "
           "@(match_iq)b, @(match_message)b, @(match_presence_in)b, "
           "@(match_presence_out)b from privacy_list_data "
           "where id ="
           " (select id from privacy_list"
           " where username=%(LUser)s and name=%(Name)s) "
           "order by ord")).

get_privacy_list_data_by_id(LServer, ID) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(t)s, @(value)s, @(action)s, @(ord)d, @(match_all)b, "
           "@(match_iq)b, @(match_message)b, @(match_presence_in)b, "
           "@(match_presence_out)b from privacy_list_data "
           "where id=%(ID)d order by ord")).

get_privacy_list_data_by_id_t(ID) ->
    ejabberd_sql:sql_query_t(
      ?SQL("select @(t)s, @(value)s, @(action)s, @(ord)d, @(match_all)b, "
           "@(match_iq)b, @(match_message)b, @(match_presence_in)b, "
           "@(match_presence_out)b from privacy_list_data "
           "where id=%(ID)d order by ord")).

set_default_privacy_list(LUser, Name) ->
    ?SQL_UPSERT_T(
       "privacy_default_list",
       ["!username=%(LUser)s",
        "name=%(Name)s"]).

unset_default_privacy_list(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from privacy_default_list"
           " where username=%(LUser)s")).

remove_privacy_list(LUser, Name) ->
    ejabberd_sql:sql_query_t(
      ?SQL("delete from privacy_list where"
           " username=%(LUser)s and name=%(Name)s")).

add_privacy_list(LUser, Name) ->
    ejabberd_sql:sql_query_t(
      ?SQL("insert into privacy_list(username, name) "
           "values (%(LUser)s, %(Name)s)")).

set_privacy_list(ID, RItems) ->
    ejabberd_sql:sql_query_t(
      ?SQL("delete from privacy_list_data where id=%(ID)d")),
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

del_privacy_lists(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from privacy_list where username=%(LUser)s")),
    %US = <<LUser/binary, "@", LServer/binary>>,
    %ejabberd_sql:sql_query(
    %  LServer,
    %  ?SQL("delete from privacy_list_data where value=%(US)s")),
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from privacy_default_list where username=%(LUser)s")).

%% Characters to escape
escape($\000) -> <<"\\0">>;
escape($\n) -> <<"\\n">>;
escape($\t) -> <<"\\t">>;
escape($\b) -> <<"\\b">>;
escape($\r) -> <<"\\r">>;
escape($') -> <<"''">>;
escape($") -> <<"\\\"">>;
escape($\\) -> <<"\\\\">>;
escape(C) -> <<C>>.

%% Count number of records in a table given a where clause
count_records_where(LServer, Table, WhereClause) ->
    ejabberd_sql:sql_query(LServer,
			    [<<"select count(*) from ">>, Table, <<" ">>,
			     WhereClause, <<";">>]).

get_roster_version(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(version)s from roster_version"
           " where username = %(LUser)s")).

set_roster_version(LUser, Version) ->
    ?SQL_UPSERT_T(
       "roster_version",
       ["!username=%(LUser)s",
        "version=%(Version)s"]).

opt_type(sql_type) ->
    fun (pgsql) -> pgsql;
	(mysql) -> mysql;
	(sqlite) -> sqlite;
	(mssql) -> mssql;
	(odbc) -> odbc
    end;
opt_type(pgsql_users_number_estimate) ->
    fun (V) when is_boolean(V) -> V end;
opt_type(_) -> [sql_type, pgsql_users_number_estimate].
