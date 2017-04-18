%%%-------------------------------------------------------------------
%%% File    : mod_mam_sql.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_mam_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_mam).

%% API
-export([init/2, remove_user/2, remove_room/3, delete_old_messages/3,
	 extended_fields/0, store/7, write_prefs/4, get_prefs/2, select/6]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("xmpp.hrl").
-include("mod_mam.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

remove_user(LUser, LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from archive where username=%(LUser)s")),
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from archive_prefs where username=%(LUser)s")).

remove_room(LServer, LName, LHost) ->
    LUser = jid:encode({LName, LHost, <<>>}),
    remove_user(LUser, LServer).

delete_old_messages(ServerHost, TimeStamp, Type) ->
    TypeClause = if Type == all -> <<"">>;
		    true -> [<<" and kind='">>, misc:atom_to_binary(Type), <<"'">>]
		 end,
    TS = integer_to_binary(now_to_usec(TimeStamp)),
    ejabberd_sql:sql_query(
      ServerHost, [<<"delete from archive where timestamp<">>,
		   TS, TypeClause, <<";">>]),
    ok.

extended_fields() ->
    [{withtext, <<"">>}].

store(Pkt, LServer, {LUser, LHost}, Type, Peer, Nick, _Dir) ->
    TSinteger = p1_time_compat:system_time(micro_seconds),
    ID = integer_to_binary(TSinteger),
    SUser = case Type of
		chat -> LUser;
		groupchat -> jid:encode({LUser, LHost, <<>>})
	    end,
    BarePeer = jid:encode(
		 jid:tolower(
		   jid:remove_resource(Peer))),
    LPeer = jid:encode(
	      jid:tolower(Peer)),
    XML = fxml:element_to_binary(Pkt),
    Body = fxml:get_subtag_cdata(Pkt, <<"body">>),
    SType = misc:atom_to_binary(Type),
    case ejabberd_sql:sql_query(
           LServer,
           ?SQL("insert into archive (username, timestamp,"
                " peer, bare_peer, xml, txt, kind, nick) values ("
		"%(SUser)s, "
		"%(TSinteger)d, "
		"%(LPeer)s, "
		"%(BarePeer)s, "
		"%(XML)s, "
		"%(Body)s, "
		"%(SType)s, "
		"%(Nick)s)")) of
	{updated, _} ->
	    {ok, ID};
	Err ->
	    Err
    end.

write_prefs(LUser, _LServer, #archive_prefs{default = Default,
					   never = Never,
					   always = Always},
	    ServerHost) ->
    SDefault = erlang:atom_to_binary(Default, utf8),
    SAlways = misc:term_to_expr(Always),
    SNever = misc:term_to_expr(Never),
    case ?SQL_UPSERT(
            ServerHost,
            "archive_prefs",
            ["!username=%(LUser)s",
             "def=%(SDefault)s",
             "always=%(SAlways)s",
             "never=%(SNever)s"]) of
	{updated, _} ->
	    ok;
	Err ->
	    Err
    end.

get_prefs(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(def)s, @(always)s, @(never)s from archive_prefs"
                " where username=%(LUser)s")) of
	{selected, [{SDefault, SAlways, SNever}]} ->
	    Default = erlang:binary_to_existing_atom(SDefault, utf8),
	    Always = ejabberd_sql:decode_term(SAlways),
	    Never = ejabberd_sql:decode_term(SNever),
	    {ok, #archive_prefs{us = {LUser, LServer},
		    default = Default,
		    always = Always,
		    never = Never}};
	_ ->
	    error
    end.

select(LServer, JidRequestor, #jid{luser = LUser} = JidArchive,
       MAMQuery, RSM, MsgType) ->
    User = case MsgType of
	       chat -> LUser;
	       {groupchat, _Role, _MUCState} -> jid:encode(JidArchive)
	   end,
    {Query, CountQuery} = make_sql_query(User, LServer, MAMQuery, RSM),
    % TODO from XEP-0313 v0.2: "To conserve resources, a server MAY place a
    % reasonable limit on how many stanzas may be pushed to a client in one
    % request. If a query returns a number of stanzas greater than this limit
    % and the client did not specify a limit using RSM then the server should
    % return a policy-violation error to the client." We currently don't do this
    % for v0.2 requests, but we do limit #rsm_in.max for v0.3 and newer.
    case {ejabberd_sql:sql_query(LServer, Query),
	  ejabberd_sql:sql_query(LServer, CountQuery)} of
	{{selected, _, Res}, {selected, _, [[Count]]}} ->
	    {Max, Direction, _} = get_max_direction_id(RSM),
	    {Res1, IsComplete} =
		if Max >= 0 andalso Max /= undefined andalso length(Res) > Max ->
			if Direction == before ->
				{lists:nthtail(1, Res), false};
			   true ->
				{lists:sublist(Res, Max), false}
			end;
		   true ->
			{Res, true}
		end,
	    {lists:flatmap(
	       fun([TS, XML, PeerBin, Kind, Nick]) ->
		       case make_archive_el(
			      TS, XML, PeerBin, Kind, Nick,
			      MsgType, JidRequestor, JidArchive) of
			   {ok, El} ->
			       [{TS, binary_to_integer(TS), El}];
			   {error, _} ->
			       []
		       end
	       end, Res1), IsComplete, binary_to_integer(Count)};
	_ ->
	    {[], false, 0}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.

usec_to_now(Int) ->
    Secs = Int div 1000000,
    USec = Int rem 1000000,
    MSec = Secs div 1000000,
    Sec = Secs rem 1000000,
    {MSec, Sec, USec}.

make_sql_query(User, LServer, MAMQuery, RSM) ->
    Start = proplists:get_value(start, MAMQuery),
    End = proplists:get_value('end', MAMQuery),
    With = proplists:get_value(with, MAMQuery),
    WithText = proplists:get_value(withtext, MAMQuery),
    {Max, Direction, ID} = get_max_direction_id(RSM),
    ODBCType = ejabberd_config:get_option(
		 {sql_type, LServer},
		 ejabberd_sql:opt_type(sql_type)),
    Escape =
        case ODBCType of
            mssql -> fun ejabberd_sql:standard_escape/1;
            sqlite -> fun ejabberd_sql:standard_escape/1;
            _ -> fun ejabberd_sql:escape/1
        end,
    LimitClause = if is_integer(Max), Max >= 0, ODBCType /= mssql ->
			  [<<" limit ">>, integer_to_binary(Max+1)];
		     true ->
			  []
		  end,
    TopClause = if is_integer(Max), Max >= 0, ODBCType == mssql ->
			  [<<" TOP ">>, integer_to_binary(Max+1)];
		     true ->
			  []
		  end,
    WithTextClause = if is_binary(WithText), WithText /= <<>> ->
			     [<<" and match (txt) against ('">>,
			      Escape(WithText), <<"')">>];
			true ->
			     []
		     end,
    WithClause = case catch jid:tolower(With) of
		     {_, _, <<>>} ->
			 [<<" and bare_peer='">>,
			  Escape(jid:encode(With)),
			  <<"'">>];
		     {_, _, _} ->
			 [<<" and peer='">>,
			  Escape(jid:encode(With)),
			  <<"'">>];
		     _ ->
			 []
		 end,
    PageClause = case catch binary_to_integer(ID) of
		     I when is_integer(I), I >= 0 ->
			 case Direction of
			     before ->
				 [<<" AND timestamp < ">>, ID];
			     'after' ->
				 [<<" AND timestamp > ">>, ID];
			     _ ->
				 []
			 end;
		     _ ->
			 []
		 end,
    StartClause = case Start of
		      {_, _, _} ->
			  [<<" and timestamp >= ">>,
			   integer_to_binary(now_to_usec(Start))];
		      _ ->
			  []
		  end,
    EndClause = case End of
		    {_, _, _} ->
			[<<" and timestamp <= ">>,
			 integer_to_binary(now_to_usec(End))];
		    _ ->
			[]
		end,
    SUser = Escape(User),

    Query = [<<"SELECT ">>, TopClause, <<" timestamp, xml, peer, kind, nick"
	      " FROM archive WHERE username='">>,
	     SUser, <<"'">>, WithClause, WithTextClause, StartClause, EndClause,
	     PageClause],

    QueryPage =
	case Direction of
	    before ->
		% ID can be empty because of
		% XEP-0059: Result Set Management
		% 2.5 Requesting the Last Page in a Result Set
		[<<"SELECT timestamp, xml, peer, kind, nick FROM (">>, Query,
		 <<" ORDER BY timestamp DESC ">>,
		 LimitClause, <<") AS t ORDER BY timestamp ASC;">>];
	    _ ->
		[Query, <<" ORDER BY timestamp ASC ">>,
		 LimitClause, <<";">>]
	end,
    {QueryPage,
     [<<"SELECT COUNT(*) FROM archive WHERE username='">>,
      SUser, <<"'">>, WithClause, WithTextClause, StartClause, EndClause, <<";">>]}.

-spec get_max_direction_id(rsm_set() | undefined) ->
				  {integer() | undefined,
				   before | 'after' | undefined,
				   binary()}.
get_max_direction_id(RSM) ->
    case RSM of
	#rsm_set{max = Max, before = Before} when is_binary(Before) ->
	    {Max, before, Before};
	#rsm_set{max = Max, 'after' = After} when is_binary(After) ->
	    {Max, 'after', After};
	#rsm_set{max = Max} ->
	    {Max, undefined, <<>>};
	_ ->
	    {undefined, undefined, <<>>}
    end.

-spec make_archive_el(binary(), binary(), binary(), binary(),
		      binary(), _, jid(), jid()) ->
			     {ok, xmpp_element()} | {error, invalid_jid |
						     invalid_timestamp |
						     invalid_xml}.
make_archive_el(TS, XML, Peer, Kind, Nick, MsgType, JidRequestor, JidArchive) ->
    case fxml_stream:parse_element(XML) of
	#xmlel{} = El ->
	    try binary_to_integer(TS) of
		TSInt ->
		    try jid:decode(Peer) of
			PeerJID ->
			    Now = usec_to_now(TSInt),
			    PeerLJID = jid:tolower(PeerJID),
			    T = case Kind of
				    <<"">> -> chat;
				    null -> chat;
				    _ -> misc:binary_to_atom(Kind)
				end,
			    mod_mam:msg_to_el(
			      #archive_msg{timestamp = Now,
					   id = TS,
					   packet = El,
					   type = T,
					   nick = Nick,
					   peer = PeerLJID},
			      MsgType, JidRequestor, JidArchive)
		    catch _:{bad_jid, _} ->
			    ?ERROR_MSG("Malformed 'peer' field with value "
				       "'~s' detected for user ~s in table "
				       "'archive': invalid JID",
				       [Peer, jid:encode(JidArchive)]),
			    {error, invalid_jid}
		    end
	    catch _:_ ->
		    ?ERROR_MSG("Malformed 'timestamp' field with value '~s' "
			       "detected for user ~s in table 'archive': "
			       "not an integer",
			       [TS, jid:encode(JidArchive)]),
		    {error, invalid_timestamp}
	    end;
	{error, {_, Reason}} ->
	    ?ERROR_MSG("Malformed 'xml' field with value '~s' detected "
		       "for user ~s in table 'archive': ~s",
		       [XML, jid:encode(JidArchive), Reason]),
	    {error, invalid_xml}
    end.
