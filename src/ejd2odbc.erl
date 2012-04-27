%%%----------------------------------------------------------------------
%%% File    : ejd2odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Export some mnesia tables to SQL DB
%%% Created : 22 Aug 2005 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejd2odbc).
-author('alexey@process-one.net').

%% External exports
-export([export_passwd/2,
	 export_roster/2,
	 export_offline/2,
	 export_last/2,
	 export_vcard/2,
	 export_vcard_search/2,
         export_vcard_xupdate/2,
	 export_private_storage/2,
         export_privacy/2,
         export_motd/2,
         export_motd_users/2,
         export_irc_custom/2,
         export_sr_group/2,
         export_sr_user/2,
         export_muc_room/2,
         export_muc_registered/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("mod_privacy.hrl").

-record(offline_msg, {us, timestamp, expire, from, to, packet}).
-record(last_activity, {us, timestamp, status}).
-record(vcard, {us, vcard}).
-record(vcard_xupdate, {us, hash}).
-record(vcard_search, {us,
		       user,     luser,
		       fn,	 lfn,
		       family,	 lfamily,
		       given,	 lgiven,
		       middle,	 lmiddle,
		       nickname, lnickname,
		       bday,	 lbday,
		       ctry,	 lctry,
		       locality, llocality,
		       email,	 lemail,
		       orgname,	 lorgname,
		       orgunit,	 lorgunit
		      }).
-record(private_storage, {usns, xml}).
-record(irc_custom, {us_host, data}).
-record(muc_room, {name_host, opts}).
-record(muc_registered, {us_host, nick}).
-record(sr_group, {group_host, opts}).
-record(sr_user, {us, group_host}).
-record(motd, {server, packet}).
-record(motd_users, {us, dummy = []}).

-define(MAX_RECORDS_PER_TRANSACTION, 1000).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%% How to use:
%%% A table can be converted from Mnesia to an ODBC database by calling
%%% one of the API function with the following parameters:
%%% - Server is the server domain you want to convert
%%% - Output can be either odbc to export to the configured relational
%%%   database or "Filename" to export to text file.

export_passwd(Server, Output) ->
    export_common(
      Server, passwd, Output,
      fun(Host, {passwd, {LUser, LServer}, Password} = _R)
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      Pass = ejabberd_odbc:escape(Password),
	      ["delete from users where username='", Username ,"';"
	       "insert into users(username, password) "
	       "values ('", Username, "', '", Pass, "');"];
	 (_Host, _R) ->
	      []
      end).

export_roster(Server, Output) ->
    export_common(
      Server, roster, Output,
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
	       "              subscription, ask, askmessage, "
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

export_offline(Server, Output) ->
    export_common(
      Server, offline_msg, Output,
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
			      calendar:now_to_universal_time(TimeStamp),
			      utc,
			      jlib:make_jid("", Server, ""),
			      "Offline Storage"),
			    %% TODO: Delete the next three lines once XEP-0091 is Obsolete
			    jlib:timestamp_to_xml(
			      calendar:now_to_universal_time(
				TimeStamp))]},
	      XML =
		  ejabberd_odbc:escape(
		    xml:element_to_binary(NewPacket)),
	      ["insert into spool(username, xml) "
	       "values ('", Username, "', '",
	       XML,
	       "');"];
	 (_Host, _R) ->
	      []
      end).

export_last(Server, Output) ->
    export_common(
      Server, last_activity, Output,
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

export_vcard(Server, Output) ->
    export_common(
      Server, vcard, Output,
      fun(Host, #vcard{us = {LUser, LServer},
		       vcard = VCARD})
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      SVCARD = ejabberd_odbc:escape(
			 xml:element_to_binary(VCARD)),
	      ["delete from vcard where username='", Username, "';"
	       "insert into vcard(username, vcard) "
	       "values ('", Username, "', '", SVCARD, "');"];
	 (_Host, _R) ->
	      []
      end).

export_vcard_search(Server, Output) ->
    export_common(
      Server, vcard_search, Output,
      fun(Host, #vcard_search{user      = {User, LServer},
			      luser     = LUser,
			      fn        = FN,       lfn        = LFN,       
			      family    = Family,   lfamily    = LFamily,   
			      given     = Given,    lgiven     = LGiven,    
			      middle    = Middle,   lmiddle    = LMiddle,   
			      nickname  = Nickname, lnickname  = LNickname, 
			      bday      = BDay,     lbday      = LBDay,     
			      ctry      = CTRY,     lctry      = LCTRY,     
			      locality  = Locality, llocality  = LLocality, 
			      email     = EMail,    lemail     = LEMail,    
			      orgname   = OrgName,  lorgname   = LOrgName,  
			      orgunit   = OrgUnit,  lorgunit   = LOrgUnit   
			     })
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(User),
	      LUsername = ejabberd_odbc:escape(LUser),

	      SFN = ejabberd_odbc:escape(FN),
	      SLFN = ejabberd_odbc:escape(LFN),
	      SFamily = ejabberd_odbc:escape(Family),
	      SLFamily = ejabberd_odbc:escape(LFamily),
	      SGiven = ejabberd_odbc:escape(Given),
	      SLGiven = ejabberd_odbc:escape(LGiven),
	      SMiddle = ejabberd_odbc:escape(Middle),
	      SLMiddle = ejabberd_odbc:escape(LMiddle),
	      SNickname = ejabberd_odbc:escape(Nickname),
	      SLNickname = ejabberd_odbc:escape(LNickname),
	      SBDay = ejabberd_odbc:escape(BDay),
	      SLBDay = ejabberd_odbc:escape(LBDay),
	      SCTRY = ejabberd_odbc:escape(CTRY),
	      SLCTRY = ejabberd_odbc:escape(LCTRY),
	      SLocality = ejabberd_odbc:escape(Locality),
	      SLLocality = ejabberd_odbc:escape(LLocality),
	      SEMail = ejabberd_odbc:escape(EMail),
	      SLEMail = ejabberd_odbc:escape(LEMail),
	      SOrgName = ejabberd_odbc:escape(OrgName),
	      SLOrgName = ejabberd_odbc:escape(LOrgName),
	      SOrgUnit = ejabberd_odbc:escape(OrgUnit),
	      SLOrgUnit = ejabberd_odbc:escape(LOrgUnit),

	      ["delete from vcard_search where lusername='", LUsername, "';"
	       "insert into vcard_search("
	       "        username, lusername, fn, lfn, family, lfamily,"
	       "        given, lgiven, middle, lmiddle, nickname, lnickname,"
	       "        bday, lbday, ctry, lctry, locality, llocality,"
	       "        email, lemail, orgname, lorgname, orgunit, lorgunit)"
	       "values (",
	       "        '", Username, "', '",  LUsername, "'," 
	       "        '", SFN,       "', '", SLFN,       "'," 
	       "        '", SFamily,   "', '", SLFamily,   "',"
	       "        '", SGiven,    "', '", SLGiven,	 "',"
	       "        '", SMiddle,   "', '", SLMiddle,   "',"
	       "        '", SNickname, "', '", SLNickname, "',"
	       "        '", SBDay,     "', '", SLBDay,	 "',"
	       "        '", SCTRY,     "', '", SLCTRY,	 "',"
	       "        '", SLocality, "', '", SLLocality, "',"
	       "        '", SEMail,    "', '", SLEMail,	 "',"
	       "        '", SOrgName,  "', '", SLOrgName,  "',"
	       "        '", SOrgUnit,  "', '", SLOrgUnit,  "');"];
	 (_Host, _R) ->
	      []
      end).

export_vcard_xupdate(Server, Output) ->
    export_common(
      Server, vcard_xupdate, Output,
      fun(Host, #vcard_xupdate{us = {LUser, LServer}, hash = Hash})
            when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
	      SHash = ejabberd_odbc:escape(Hash),
	      ["delete from vcard_xupdate where username='", Username, "';"
	       "insert into vcard_xupdate(username, hash) "
	       "values ('", Username, "', '", SHash, "');"];
	 (_Host, _R) ->
	      []
      end).

export_private_storage(Server, Output) ->
    export_common(
      Server, private_storage, Output,
      fun(Host, #private_storage{usns = {LUser, LServer, XMLNS},
				 xml = Data})
	 when LServer == Host ->
	      Username = ejabberd_odbc:escape(LUser),
      	      LXMLNS = ejabberd_odbc:escape(XMLNS),
	      SData = ejabberd_odbc:escape(
			xml:element_to_binary(Data)),
      	      odbc_queries:set_private_data_sql(Username, LXMLNS, SData);
	 (_Host, _R) ->
      	      []
      end).

export_muc_room(Server, Output) ->
    export_common(
      Server, muc_room, Output,
      fun(Host, #muc_room{name_host = {Name, RoomHost}, opts = Opts}) ->
              case lists:suffix(Host, RoomHost) of
                  true ->
                      SName = ejabberd_odbc:escape(Name),
                      SRoomHost = ejabberd_odbc:escape(RoomHost),
                      SOpts = ejabberd_odbc:encode_term(Opts),
                      ["delete from muc_room where name='", SName,
                       "' and host='", SRoomHost, "';",
                       "insert into muc_room(name, host, opts) values (",
                       "'", SName, "', '", SRoomHost, "', '", SOpts, "');"];
                  false ->
                      []
              end
      end).

export_muc_registered(Server, Output) ->
    export_common(
      Server, muc_registered, Output,
      fun(Host, #muc_registered{us_host = {{U, S}, RoomHost}, nick = Nick}) ->
              case lists:suffix(Host, RoomHost) of
                  true ->
                      SJID = ejabberd_odbc:escape(
                               jlib:jid_to_string(
                                 jlib:make_jid(U, S, ""))),
                      SNick = ejabberd_odbc:escape(Nick),
                      SRoomHost = ejabberd_odbc:escape(RoomHost),
                      ["delete from muc_registered where jid='", SJID,
                       "' and host='", SRoomHost, "';"
                       "insert into muc_registered(jid, host, nick) values ("
                       "'", SJID, "', '", SRoomHost, "', '", SNick, "');"];
                  false ->
                      []
              end
      end).

export_irc_custom(Server, Output) ->
    export_common(
      Server, irc_custom, Output,
      fun(Host, #irc_custom{us_host = {{U, S}, IRCHost}, data = Data}) ->
              case lists:suffix(Host, IRCHost) of
                  true ->
                      SJID = ejabberd_odbc:escape(
                               jlib:jid_to_string(
                                 jlib:make_jid(U, S, ""))),
                      SIRCHost = ejabberd_odbc:escape(IRCHost),
                      SData = ejabberd_odbc:encode_term(Data),
                      ["delete from irc_custom where jid='", SJID,
                       "' and host='", SIRCHost, "';"
                       "insert into irc_custom(jid, host, data) values ("
                       "'", SJID, "', '", SIRCHost, "', '", SData, "');"];
                  false ->
                      []
              end
      end).

export_privacy(Server, Output) ->
    case ejabberd_odbc:sql_query(
           jlib:nameprep(Server),
           ["select id from privacy_list order by id desc limit 1;"]) of
        {selected, ["id"], [{I}]} ->
            put(id, list_to_integer(I));
        _ ->
            put(id, 0)
    end,
    export_common(
      Server, privacy, Output,
      fun(Host, #privacy{us = {LUser, LServer},
                         lists = Lists,
                         default = Default}) when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              if Default /= none ->
                      SDefault = ejabberd_odbc:escape(Default),
                      ["delete from privacy_default_list where ",
                       "username='", Username, "';",
                       "insert into privacy_default_list(username, name) ",
                       "values ('", Username, "', '", SDefault, "');"];
                 true ->
                      []
              end ++
                  lists:flatmap(
                    fun({Name, List}) ->
                            SName = ejabberd_odbc:escape(Name),
                            RItems = lists:map(
                                       fun mod_privacy_odbc:item_to_raw/1,
                                       List),
                            ID = integer_to_list(get_id()),
                            ["delete from privacy_list "
                             "where username='", Username, "' and name='", SName, "';"
                             "insert into privacy_list(username, name, id) "
                             "values ('", Username, "', '", SName, "', '", ID, "');",
                             "delete from privacy_list_data where id='", ID, "';"
                             |[["insert into privacy_list_data("
                                "id, t, value, action, ord, match_all, match_iq, "
                                "match_message, match_presence_in, "
                                "match_presence_out) values ('", ID, "', '",
                                string:join(Items, "', '"), "');"] || Items <- RItems]]
                    end, Lists);
         (_Host, _R) ->
              []
      end).

export_sr_group(Server, Output) ->
    export_common(
      Server, sr_group, Output,
      fun(Host, #sr_group{group_host = {Group, LServer}, opts = Opts})
            when LServer == Host ->
              SGroup = ejabberd_odbc:escape(Group),
              SOpts = ejabberd_odbc:encode_term(Opts),
              ["delete from sr_group where name='", Group, "';"
               "insert into sr_group(name, opts) values ('",
               SGroup, "', '", SOpts, "');"];
         (_Host, _R) ->
              []
      end).

export_sr_user(Server, Output) ->
    export_common(
      Server, sr_user, Output,
      fun(Host, #sr_user{us = {U, S}, group_host = {Group, LServer}})
            when LServer == Host ->
              SGroup = ejabberd_odbc:escape(Group),
              SJID = ejabberd_odbc:escape(
                       jlib:jid_to_string(
                         jlib:jid_tolower(
                           jlib:make_jid(U, S, "")))),
              ["delete from sr_user where jid='", SJID,
               "'and grp='", Group, "';"
               "insert into sr_user(jid, grp) values ('",
               SJID, "', '", SGroup, "');"];
         (_Host, _R) ->
              []
      end).

export_motd(Server, Output) ->
    export_common(
      Server, motd, Output,
      fun(Host, #motd{server = LServer, packet = El})
            when LServer == Host ->
              ["delete from motd where username='';"
               "insert into motd(username, xml) values ('', '",
               ejabberd_odbc:escape(xml:element_to_binary(El)), "');"];
         (_Host, _R) ->
              []
      end).

export_motd_users(Server, Output) ->
    export_common(
      Server, motd_users, Output,
      fun(Host, #motd_users{us = {LUser, LServer}})
            when LServer == Host, LUser /= "" ->
              Username = ejabberd_odbc:escape(LUser),
              ["delete from motd where username='", Username, "';"
               "insert into motd(username, xml) values ('",
               Username, "', '');"];
         (_Host, _R) ->
              []
      end).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

export_common(Server, Table, Output, ConvertFun) ->
    IO = case Output of
	     odbc ->
		 odbc;
	     _ ->
		 {ok, IODevice} = file:open(Output, [write, raw]),
		 IODevice
	 end,
    mnesia:transaction(
      fun() ->
	      mnesia:read_lock_table(Table),
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
					    %% Execute full SQL transaction
					    output(LServer, IO,
						   ["begin;",
						    lists:reverse([SQL | SQLs]),
						    "commit"]),
					    {0, []}
				    end
			    end
		    end, {0, []}, Table),
		  %% Execute SQL transaction with remaining records
	      output(LServer, IO,
		     ["begin;",
		      lists:reverse(SQLs),
		      "commit"])
      end).

output(LServer, IO, SQL) ->
    case IO of
	odbc ->
	    catch ejabberd_odbc:sql_query(LServer, SQL);
	_ ->
	    file:write(IO, [SQL, $;, $\n])
    end.

record_to_string(#roster{usj = {User, _Server, JID},
			 name = Name,
			 subscription = Subscription,
			 ask = Ask,
			 askmessage = AskMessage}) ->
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
    SAskMessage =
	case catch ejabberd_odbc:escape(
		     binary_to_list(list_to_binary([AskMessage]))) of
	    {'EXIT', _Reason} ->
		[];
	    SAM ->
		SAM
	end,
    ["("
     "'", Username, "',"
     "'", SJID, "',"
     "'", Nick, "',"
     "'", SSubscription, "',"
     "'", SAsk, "',"
     "'", SAskMessage, "',"
     "'N', '', 'item')"].

groups_to_string(#roster{usj = {User, _Server, JID},
			 groups = Groups}) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(jlib:jid_to_string(JID)),
    [["("
      "'", Username, "',"
      "'", SJID, "',"
      "'", ejabberd_odbc:escape(Group), "')"] || Group <- Groups].

get_id() ->
    ID = get(id),
    put(id, ID+1),
    ID+1.
