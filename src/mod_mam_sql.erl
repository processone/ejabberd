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
	 extended_fields/0, store/7, write_prefs/4, get_prefs/2, select/8]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("jlib.hrl").
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
    [#xmlel{name = <<"field">>,
	    attrs = [{<<"type">>, <<"text-single">>},
		     {<<"var">>, <<"withtext">>}]}].

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
    SUser = ejabberd_sql:escape(LUser),
    SDefault = erlang:atom_to_binary(Default, utf8),
    SAlways = ejabberd_sql:encode_term(Always),
    SNever = ejabberd_sql:encode_term(Never),
    case update(ServerHost, <<"archive_prefs">>,
		[<<"username">>, <<"def">>, <<"always">>, <<"never">>],
		[SUser, SDefault, SAlways, SNever],
		[<<"username='">>, SUser, <<"'">>]) of
	{updated, _} ->
	    ok;
	Err ->
	    Err
    end.

get_prefs(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   [<<"select def, always, never from archive_prefs ">>,
	    <<"where username='">>,
	    ejabberd_sql:escape(LUser), <<"';">>]) of
	{selected, _, [[SDefault, SAlways, SNever]]} ->
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
       Start, End, With, RSM, MsgType) ->
    User = case MsgType of
	       chat -> LUser;
	       {groupchat, _Role, _MUCState} -> jid:to_string(JidArchive)
	   end,
    {Query, CountQuery} = make_sql_query(User, LServer,
					 Start, End, With, RSM),
    % TODO from XEP-0313 v0.2: "To conserve resources, a server MAY place a
    % reasonable limit on how many stanzas may be pushed to a client in one
    % request. If a query returns a number of stanzas greater than this limit
    % and the client did not specify a limit using RSM then the server should
    % return a policy-violation error to the client." We currently don't do this
    % for v0.2 requests, but we do limit #rsm_in.max for v0.3 and newer.
    case {ejabberd_sql:sql_query(LServer, Query),
	  ejabberd_sql:sql_query(LServer, CountQuery)} of
	{{selected, _, Res}, {selected, _, [[Count]]}} ->
	    {Max, Direction} = case RSM of
				   #rsm_in{max = M, direction = D} -> {M, D};
				   _ -> {undefined, undefined}
			       end,
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

make_sql_query(User, LServer, Start, End, With, RSM) ->
    {Max, Direction, ID} = case RSM of
	#rsm_in{} ->
	    {RSM#rsm_in.max,
		RSM#rsm_in.direction,
		RSM#rsm_in.id};
	none ->
	    {none, none, <<>>}
    end,
    ODBCType = ejabberd_config:get_option(
		 {sql_type, LServer},
		 ejabberd_sql:opt_type(sql_type)),
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
    WithClause = case With of
		     {text, <<>>} ->
			 [];
		     {text, Txt} ->
			 [<<" and match (txt) against ('">>,
			  ejabberd_sql:escape(Txt), <<"')">>];
		     {_, _, <<>>} ->
			 [<<" and bare_peer='">>,
			  ejabberd_sql:escape(jid:to_string(With)),
			  <<"'">>];
		     {_, _, _} ->
			 [<<" and peer='">>,
			  ejabberd_sql:escape(jid:to_string(With)),
			  <<"'">>];
		     none ->
			 []
		 end,
    PageClause = case catch jlib:binary_to_integer(ID) of
		     I when is_integer(I), I >= 0 ->
			 case Direction of
			     before ->
				 [<<" AND timestamp < ">>, ID];
			     aft ->
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
    SUser = ejabberd_sql:escape(User),

    Query = [<<"SELECT ">>, TopClause, <<" timestamp, xml, peer, kind, nick"
	      " FROM archive WHERE username='">>,
	     SUser, <<"'">>, WithClause, StartClause, EndClause,
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
      SUser, <<"'">>, WithClause, StartClause, EndClause, <<";">>]}.

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
	{updated, 1} -> {updated, 1};
	_ ->
	    ejabberd_sql:sql_query(LServer,
				    [<<"insert into ">>, Table, <<"(">>,
				     join(Fields, <<", ">>), <<") values ('">>,
				     join(Vals, <<"', '">>), <<"');">>])
    end.

%% Almost a copy of string:join/2.
join([], _Sep) -> [];
join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].
