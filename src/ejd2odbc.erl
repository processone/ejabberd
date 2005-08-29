%%%----------------------------------------------------------------------
%%% File    : ejd2odbc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Import of jabberd1.4 user spool file
%%% Created : 22 Aug 2005 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejd2odbc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([export_passwd/1,
	 export_roster/1,
	 export_offline/1,
	 export_last/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").

-record(offline_msg, {us, timestamp, expire, from, to, packet}).
-record(last_activity, {us, timestamp, status}).

-define(MAX_RECORDS_PER_TRANSACTION, 1000).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

export_passwd(Server) ->
    export_common(
      Server, passwd,
      fun(Host, {passwd, {LUser, LServer}, Password} = R)
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      Pass = ejabberd_odbc:escape(Password),
	      ["delete from users where username='", Username ,"';"
	       "insert into users(username, password) "
	       "values ('", Username, "', '", Pass, "');"];
	 (_Host, _R) ->
	      []
      end).

export_roster(Server) ->
    export_common(
      Server, roster,
      fun(Host, #roster{usj = {LUser, LServer, LJID}} = R)
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      SJID = ejabberd_odbc:escape(jlib:jid_to_string(LJID)),
	      ItemVals = record_to_string(R),
	      ItemGroups = groups_to_string(R),
	      ["delete from rosterusers "
	       "      where username='", Username, "' "
	       "        and jid='", SJID, "';"
	       "insert into rosterusers("
	       "              username, jid, nick, "
	       "              subscription, ask, "
	       "              server, subscribe, type) "
	       " values ", ItemVals, ";"
	       "delete from rostergroups "
	       "      where username='", Username, "' "
	       "        and jid='", SJID, "';",
	       [["insert into rostergroups("
		 "              username, jid, grp) "
		 " values ", ItemGroup, ";"] ||
		   ItemGroup <- ItemGroups]];
	 (_Host, _R) ->
	      []
      end).

export_offline(Server) ->
    export_common(
      Server, offline_msg,
      fun(Host, #offline_msg{us = {LUser, LServer},
			     timestamp = TimeStamp,
			     from = From,
			     to = To,
			     packet = Packet})
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      {xmlelement, Name, Attrs, Els} = Packet,
	      Attrs2 = jlib:replace_from_to_attrs(
			 jlib:jid_to_string(From),
			 jlib:jid_to_string(To),
			 Attrs),
	      NewPacket = {xmlelement, Name, Attrs2,
			   Els ++
			   [jlib:timestamp_to_xml(
			      calendar:now_to_universal_time(TimeStamp))]},
	      XML =
		  ejabberd_odbc:escape(
		    lists:flatten(
		      xml:element_to_string(NewPacket))),
	      ["insert into spool(username, xml) "
	       "values ('", Username, "', '",
	       XML,
	       "');"];
	 (_Host, _R) ->
	      []
      end).

export_last(Server) ->
    export_common(
      Server, last_activity,
      fun(Host, #last_activity{us = {LUser, LServer},
			       timestamp = TimeStamp,
			       status = Status})
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      Seconds = ejabberd_odbc:escape(integer_to_list(TimeStamp)),
	      State = ejabberd_odbc:escape(Status),
	      ["delete from last where username='", Username, "';"
	       "insert into last(username, seconds, state) "
	       "values ('", Username, "', '", Seconds, "', '", State, "');"];
	 (_Host, _R) ->
	      []
      end).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

export_common(Server, Table, ConvertFun) ->
    mnesia:transaction(
      fun() ->
	      mnesia:read_lock_table(passwd),
	      LServer = jlib:nameprep(Server),
	      {_N, SQLs} =
		  mnesia:foldl(
		    fun(R, {N, SQLs} = Acc) ->
			    case ConvertFun(LServer, R) of
				[] ->
				    Acc;
				SQL ->
				    if
					N < ?MAX_RECORDS_PER_TRANSACTION - 1 ->
					    {N + 1, [SQL | SQLs]};
					true ->
					    catch ejabberd_odbc:sql_query(
						    LServer,
						    ["begin;",
						     lists:reverse([SQL | SQLs]),
						     "commit"]),
					    {0, []}
				    end
			    end
		    end, {0, []}, Table),
	      catch ejabberd_odbc:sql_query(
		      LServer,
		      ["begin;",
		       lists:reverse(SQLs),
		       "commit"])
      end).

record_to_string(#roster{usj = {User, Server, JID},
			 name = Name,
			 subscription = Subscription,
			 ask = Ask}) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(jlib:jid_to_string(JID)),
    Nick = ejabberd_odbc:escape(Name),
    SSubscription = case Subscription of
			both -> "B";
			to   -> "T";
			from -> "F";
			none -> "N"
		    end,
    SAsk = case Ask of
	       subscribe   -> "S";
	       unsubscribe -> "U";
	       both	   -> "B";
	       out	   -> "O";
	       in	   -> "I";
	       none	   -> "N"
	   end,
    ["("
     "'", Username, "',"
     "'", SJID, "',"
     "'", Nick, "',"
     "'", SSubscription, "',"
     "'", SAsk, "',"
     "'N', '', 'item')"].

groups_to_string(#roster{usj = {User, Server, JID},
			 groups = Groups}) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(jlib:jid_to_string(JID)),
    [["("
      "'", Username, "',"
      "'", SJID, "',"
      "'", ejabberd_odbc:escape(Group), "')"] || Group <- Groups].

