%% Copyrigh 2006, Process-one
%% This module is intended to take into account relational databases behaviour
%% differences
-module(odbc_queries).
-author("mickael.remond@process-one.net").

-export([get_db_type/0,
	 sql_transaction/2,
	 get_last/2,
	 set_last_t/4,
	 del_last/2,
	 get_password/2,
	 set_password_t/3,
	 add_user/3,
	 del_user/2,
	 del_user_return_password/3,
	 list_users/1,
	 add_spool_sql/2,
	 add_spool/2,
	 get_and_del_spool_msg_t/2,
	 del_spool_msg/2,
	 get_roster/2,
	 get_roster_jid_groups/2,
	 get_roster_groups/3,
	 del_user_roster_t/2,
	 get_roster_by_jid/3,
	 get_rostergroup_by_jid/3,
	 del_roster/3,
	 del_roster_sql/2,
	 update_roster/5,
	 update_roster_sql/4,
	 roster_subscribe/4,
	 get_subscription/3,
	 escape/1]).

%-define(generic, true).
%-define(mssql, true).

%% -----------------
%% Generic queries
-ifdef(generic).

get_db_type() ->
    generic.

sql_transaction(LServer, F) ->
    ejabberd_odbc:sql_transaction(LServer, F).

get_last(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select seconds, state from last "
       "where username='", Username, "'"]).

set_last_t(LServer, Username, Seconds, State) ->
    %% MREMOND: I think this should be turn into a non transactional behaviour
    ejabberd_odbc:sql_transaction(
      LServer,
      [["delete from last where username='", Username, "';"],
       ["insert into last(username, seconds, state) "
	"values ('", Username, "', '", Seconds, "', '", State, "');"]]).

del_last(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["delete from last where username='", Username, "'"]).

get_password(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select password from users "
       "where username='", Username, "';"]).

set_password_t(LServer, Username, Pass) ->
    ejabberd_odbc:sql_transaction(
      LServer,
      [["delete from users where username='", Username ,"';"],
       ["insert into users(username, password) "
	"values ('", Username, "', '", Pass, "');"]]).

add_user(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["insert into users(username, password) "
       "values ('", Username, "', '", Pass, "');"]).

del_user(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["delete from users where username='", Username ,"';"]).

del_user_return_password(_LServer, Username, Pass) ->
    ejabberd_odbc:sql_query_t(
      ["select password from users where username='",
       Username, "';"]),
    ejabberd_odbc:sql_query_t(["delete from users "
			       "where username='", Username,
			       "' and password='", Pass, "';"]).

list_users(LServer) ->
    ejabberd_odbc:sql_query(
      LServer,
      "select username from users").

add_spool_sql(Username, XML) ->
    ["insert into spool(username, xml) "
     "values ('", Username, "', '",
     XML,
     "');"].

add_spool(LServer, Queries) ->
    ejabberd_odbc:sql_transaction(
      LServer, Queries).

get_and_del_spool_msg_t(LServer, Username) ->
    F = fun() ->
		Result = ejabberd_odbc:sql_query_t(
			   ["select username, xml from spool where username='", Username, "'"
			    "  order by seq;"]),
		ejabberd_odbc:sql_query_t(
		  ["delete from spool where username='", Username, "';"]),
		Result
	end,
    ejabberd_odbc:sql_transaction(LServer,F).

del_spool_msg(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["delete from spool where username='", Username, "';"]).

get_roster(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select username, jid, nick, subscription, ask, "
       "askmessage, server, subscribe, type from rosterusers "
       "where username='", Username, "'"]).

get_roster_jid_groups(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select jid, grp from rostergroups "
       "where username='", Username, "'"]).

get_roster_groups(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t(
      ["select grp from rostergroups "
       "where username='", Username, "' "
       "and jid='", SJID, "';"]).

del_user_roster_t(LServer, Username) ->
    ejabberd_odbc:sql_transaction(
      LServer,
      fun() ->
	      ejabberd_odbc:sql_query_t(
		["delete from rosterusers "
		 "      where username='", Username, "';"]),
	      ejabberd_odbc:sql_query_t(
		["delete from rostergroups "
		 "      where username='", Username, "';"])
      end).

get_roster_by_jid(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t(
    ["select username, jid, nick, subscription, "
     "ask, askmessage, server, subscribe, type from rosterusers "
     "where username='", Username, "' "
     "and jid='", SJID, "';"]).

get_rostergroup_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select grp from rostergroups "
       "where username='", Username, "' "
       "and jid='", SJID, "'"]).

del_roster(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t(
      ["delete from rosterusers "
       "      where username='", Username, "' "
       "        and jid='", SJID, "';"]),
    ejabberd_odbc:sql_query_t(
      ["delete from rostergroups "
       "      where username='", Username, "' "
       "        and jid='", SJID, "';"]).

del_roster_sql(Username, SJID) ->
    [["delete from rosterusers "
      "      where username='", Username, "' "
      "        and jid='", SJID, "';"],
     ["delete from rostergroups "
      "      where username='", Username, "' "
      "        and jid='", SJID, "';"]].

update_roster(_LServer, Username, SJID, ItemVals, ItemGroups) ->
    ejabberd_odbc:sql_query_t(
      ["delete from rosterusers "
       "      where username='", Username, "' "
       "        and jid='", SJID, "';"]),
    ejabberd_odbc:sql_query_t(
      ["insert into rosterusers("
       "              username, jid, nick, "
       "              subscription, ask, askmessage, "
       "              server, subscribe, type) "
       " values (", ItemVals, ");"]),
    ejabberd_odbc:sql_query_t(
      ["delete from rostergroups "
       "      where username='", Username, "' "
       "        and jid='", SJID, "';"]),
    lists:foreach(fun(ItemGroup) ->
			  ejabberd_odbc:sql_query_t(
			    ["insert into rostergroups("
			     "              username, jid, grp) "
			     " values (", ItemGroup, ");"])
		  end,
		  ItemGroups).

update_roster_sql(Username, SJID, ItemVals, ItemGroups) ->
    [["delete from rosterusers "
      "      where username='", Username, "' "
      "        and jid='", SJID, "';"],
     ["insert into rosterusers("
      "              username, jid, nick, "
      "              subscription, ask, askmessage"
      "              server, subscribe, type) "
      " values (", ItemVals, ");"],
     ["delete from rostergroups "
      "      where username='", Username, "' "
      "        and jid='", SJID, "';"],
     [["insert into rostergroups("
       "              username, jid, grp) "
       " values (", ItemGroup, ");"] ||
	 ItemGroup <- ItemGroups]].

roster_subscribe(_LServer, Username, SJID, ItemVals) ->
    ejabberd_odbc:sql_query_t(
      ["delete from rosterusers "
       "      where username='", Username, "' "
       "        and jid='", SJID, "';"]),
    ejabberd_odbc:sql_query_t(
      ["insert into rosterusers("
       "              username, jid, nick, "
       "              subscription, ask, askmessage, "
       "              server, subscribe, type) "
       " values (", ItemVals, ");"]).

get_subscription(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select subscription from rosterusers "
       "where username='", Username, "' "
       "and jid='", SJID, "'"]).

%% Characters to escape
escape($\0) -> "\\0";
escape($\n) -> "\\n";
escape($\t) -> "\\t";
escape($\b) -> "\\b";
escape($\r) -> "\\r";
escape($')  -> "\\'";
escape($")  -> "\\\"";
escape($\\) -> "\\\\";
escape(C)   -> C.

-endif.

%% -----------------
%% MSSQL queries
-ifdef(mssql).

get_db_type() ->
    mssql.

sql_transaction(_LServer, F) ->
    {atomic, catch F()}.

get_last(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_last '", Username, "'"]).

set_last_t(LServer, Username, Seconds, State) ->
    Result = ejabberd_odbc:sql_query(
		 LServer,
		 ["EXECUTE dbo.set_last '", Username, "', '", Seconds,
		  "', '", State, "'"]),
    {atomic, Result}.

del_last(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,      
      ["EXECUTE dbo.del_last '", Username, "'"]).

get_password(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_password '", Username, "'"]).

set_password_t(LServer, Username, Pass) ->
    Result = ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.set_password '", Username, "', '", Pass, "'"]),
    {atomic, Result}.

add_user(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.add_user '", Username, "', '", Pass, "'"]).

del_user(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_user '", Username ,"'"]).

del_user_return_password(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_user_return_password '", Username, "'"]),
    Pass.

list_users(LServer) ->
    ejabberd_odbc:sql_query(
      LServer,
      "EXECUTE dbo.list_users").

add_spool_sql(Username, XML) ->
    ["EXECUTE dbo.add_spool '", Username, "' , '",XML,"'"].

add_spool(LServer, Queries) ->
    lists:foreach(fun(Query) ->
			  ejabberd_odbc:sql_query(LServer, Query)
		  end,
		  Queries).

get_and_del_spool_msg_t(LServer, Username) ->
    [Result] = case ejabberd_odbc:sql_query(
		    LServer,
		    ["EXECUTE dbo.get_and_del_spool_msg '", Username, "'"]) of
		   Rs when list(Rs) ->
		     lists:filter(fun({selected, _Header, _Row}) ->
					  true;
				     ({updated, _N}) ->
					  false
				  end,
				  Rs);
		   Rs -> [Rs]
	       end,
    {atomic, Result}.

del_spool_msg(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_spool_msg '", Username, "'"]).

get_roster(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster '", Username, "'"]).

get_roster_jid_groups(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster_jid_groups '", Username, "'"]).

get_roster_groups(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster_groups '", Username, "' , '", SJID, "'"]).
    
del_user_roster_t(LServer, Username) ->
    Result = ejabberd_odbc:sql_query(
		      LServer,
		      ["EXECUTE dbo.del_user_roster '", Username, "'"]),
    {atomic, Result}.

get_roster_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_roster_by_jid '", Username, "' , '", SJID, "'"]).

get_rostergroup_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_rostergroup_by_jid '", Username, "' , '", SJID, "'"]).

del_roster(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.del_roster '", Username, "', '", SJID, "'"]).

del_roster_sql(Username, SJID) ->
    ["EXECUTE dbo.del_roster '", Username, "', '", SJID, "'"].

update_roster(LServer, Username, SJID, ItemVals, ItemGroups) ->
    Query1 = ["EXECUTE dbo.del_roster '", Username, "', '", SJID, "' "],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query1)),
    Query2 = ["EXECUTE dbo.add_roster_user ", ItemVals],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query2)),
    Query3 = ["EXECUTE dbo.del_roster_groups '", Username, "', '", SJID, "' "],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query3)),
    lists:foreach(fun(ItemGroup) ->
			  Query = ["EXECUTE dbo.add_roster_group ",
				   ItemGroup],
			  ejabberd_odbc:sql_query(LServer,
						  lists:flatten(Query))
		  end,
		  ItemGroups).

update_roster_sql(Username, SJID, ItemVals, ItemGroups) ->
    ["BEGIN TRANSACTION ",
     "EXECUTE dbo.del_roster_groups '", Username, "','", SJID, "' ",
     "EXECUTE dbo.add_roster_user ", ItemVals, " "] ++
	[lists:flatten("EXECUTE dbo.add_roster_group ", ItemGroup, " ")
	 || ItemGroup <- ItemGroups] ++
	["COMMIT"].

roster_subscribe(LServer, _Username, _SJID, ItemVals) ->
    catch ejabberd_odbc:sql_query(
	    LServer,
	    ["EXECUTE dbo.add_roster_user ", ItemVals]).

get_subscription(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["EXECUTE dbo.get_subscription '", Username, "' , '", SJID, "'"]).

%% Characters to escape
escape($\0) -> "\\0";
escape($\t) -> "\\t";
escape($\b) -> "\\b";
escape($\r) -> "\\r";
escape($')  -> "\''";
escape($")  -> "\\\"";
escape(C)   -> C.
-endif.
