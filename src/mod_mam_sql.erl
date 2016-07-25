%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_mam_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_mam).

%% API
-export([init/2, remove_user/2, remove_room/3, delete_old_messages/3,
	 extended_fields/0, store/7, write_prefs/4, get_prefs/2, select/5]).

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
    LUser = jid:to_string({LName, LHost, <<>>}),
    remove_user(LUser, LServer).

delete_old_messages(ServerHost, TimeStamp, Type) ->
    TypeClause = if Type == all -> <<"">>;
		    true -> [<<" and kind='">>, jlib:atom_to_binary(Type), <<"'">>]
		 end,
    TS = integer_to_binary(now_to_usec(TimeStamp)),
    ejabberd_sql:sql_query(
      ServerHost, [<<"delete from archive where timestamp<">>,
		   TS, TypeClause, <<";">>]),
    ok.

extended_fields() ->
    [#xdata_field{type = 'text-single', var = <<"withtext">>}].

store(Pkt, LServer, {LUser, LHost}, Type, Peer, Nick, _Dir) ->
    TSinteger = p1_time_compat:system_time(micro_seconds),
    ID = jlib:integer_to_binary(TSinteger),
    SUser = case Type of
		chat -> LUser;
		groupchat -> jid:to_string({LUser, LHost, <<>>})
	    end,
    BarePeer = jid:to_string(
		 jid:tolower(
		   jid:remove_resource(Peer))),
    LPeer = jid:to_string(
	      jid:tolower(Peer)),
    XML = fxml:element_to_binary(Pkt),
    Body = fxml:get_subtag_cdata(Pkt, <<"body">>),
    SType = jlib:atom_to_binary(Type),
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
    SAlways = jlib:term_to_expr(Always),
    SNever = jlib:term_to_expr(Never),
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
       MAMQuery, MsgType) ->
    User = case MsgType of
	       chat -> LUser;
	       {groupchat, _Role, _MUCState} -> jid:to_string(JidArchive)
	   end,
    {Query, CountQuery} = make_sql_query(User, LServer, MAMQuery),
    % TODO from XEP-0313 v0.2: "To conserve resources, a server MAY place a
    % reasonable limit on how many stanzas may be pushed to a client in one
    % request. If a query returns a number of stanzas greater than this limit
    % and the client did not specify a limit using RSM then the server should
    % return a policy-violation error to the client." We currently don't do this
    % for v0.2 requests, but we do limit #rsm_in.max for v0.3 and newer.
    case {ejabberd_sql:sql_query(LServer, Query),
	  ejabberd_sql:sql_query(LServer, CountQuery)} of
	{{selected, _, Res}, {selected, _, [[Count]]}} ->
	    {Max, Direction, _} = get_max_direction_id(MAMQuery#mam_query.rsm),
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
		       try
			   #xmlel{} = El = fxml_stream:parse_element(XML),
			   Now = usec_to_now(jlib:binary_to_integer(TS)),
			   PeerJid = jid:tolower(jid:from_string(PeerBin)),
			   T = case Kind of
				   <<"">> -> chat;
				   null -> chat;
				   _ -> jlib:binary_to_atom(Kind)
			       end,
			   [{TS, jlib:binary_to_integer(TS),
			     mod_mam:msg_to_el(#archive_msg{timestamp = Now,
							    packet = El,
							    type = T,
							    nick = Nick,
							    peer = PeerJid},
					       MsgType, JidRequestor, JidArchive)}]
		       catch _:Err ->
			       ?ERROR_MSG("failed to parse data from SQL: ~p. "
					  "The data was: "
					  "timestamp = ~s, xml = ~s, "
					  "peer = ~s, kind = ~s, nick = ~s",
					  [Err, TS, XML, PeerBin, Kind, Nick]),
			       []
		       end
	       end, Res1), IsComplete, jlib:binary_to_integer(Count)};
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

make_sql_query(User, LServer,
	       #mam_query{start = Start, 'end' = End, with = With,
			  withtext = WithText, rsm = RSM}) ->
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
			  [<<" limit ">>, jlib:integer_to_binary(Max+1)];
		     true ->
			  []
		  end,
    TopClause = if is_integer(Max), Max >= 0, ODBCType == mssql ->
			  [<<" TOP ">>, jlib:integer_to_binary(Max+1)];
		     true ->
			  []
		  end,
    WithTextClause = case WithText of
			 {text, <<>>} ->
			     [];
			 {text, Txt} ->
			     [<<" and match (txt) against ('">>,
			      Escape(Txt), <<"')">>];
			 undefined ->
			     []
		     end,
    WithClause = case catch jid:tolower(With) of
		     {_, _, <<>>} ->
			 [<<" and bare_peer='">>,
			  Escape(jid:to_string(With)),
			  <<"'">>];
		     {_, _, _} ->
			 [<<" and peer='">>,
			  Escape(jid:to_string(With)),
			  <<"'">>];
		     _ ->
			 []
		 end,
    PageClause = case catch jlib:binary_to_integer(ID) of
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
			   jlib:integer_to_binary(now_to_usec(Start))];
		      _ ->
			  []
		  end,
    EndClause = case End of
		    {_, _, _} ->
			[<<" and timestamp <= ">>,
			 jlib:integer_to_binary(now_to_usec(End))];
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
