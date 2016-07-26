%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_offline_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_offline).

-export([init/2, store_messages/5, pop_messages/2, remove_expired_messages/1,
	 remove_old_messages/2, remove_user/2, read_message_headers/2,
	 read_message/3, remove_message/3, read_all_messages/2,
	 remove_all_messages/2, count_messages/2, import/1, import/2,
	 export/1]).

-include("xmpp.hrl").
-include("mod_offline.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

store_messages(Host, {User, _Server}, Msgs, Len, MaxOfflineMsgs) ->
    Count = if MaxOfflineMsgs =/= infinity ->
		    Len + count_messages(User, Host);
	       true -> 0
	    end,
    if Count > MaxOfflineMsgs -> {atomic, discard};
       true ->
	    Query = lists:map(
		      fun(M) ->
			      LUser = (M#offline_msg.to)#jid.luser,
			      From = M#offline_msg.from,
			      To = M#offline_msg.to,
			      Packet =
				  jlib:replace_from_to(From, To,
						       M#offline_msg.packet),
			      NewPacket =
				  jlib:add_delay_info(Packet, Host,
						      M#offline_msg.timestamp,
						      <<"Offline Storage">>),
			      XML = fxml:element_to_binary(NewPacket),
                              sql_queries:add_spool_sql(LUser, XML)
		      end,
		      Msgs),
	    sql_queries:add_spool(Host, Query)
    end.

pop_messages(LUser, LServer) ->
    case sql_queries:get_and_del_spool_msg_t(LServer, LUser) of
	{atomic, {selected, Rs}} ->
	    {ok, lists:flatmap(
		   fun({_, XML}) ->
			   case xml_to_offline_msg(XML) of
			       {ok, Msg} ->
				   [Msg];
			       _Err ->
				   []
			   end
		   end, Rs)};
	Err ->
	    {error, Err}
    end.

remove_expired_messages(_LServer) ->
    %% TODO
    {atomic, ok}.

remove_old_messages(Days, LServer) ->
    case catch ejabberd_sql:sql_query(
		 LServer,
		 [<<"DELETE FROM spool"
		   " WHERE created_at < "
		   "NOW() - INTERVAL '">>,
		  integer_to_list(Days), <<"';">>]) of
	{updated, N} ->
	    ?INFO_MSG("~p message(s) deleted from offline spool", [N]);
	_Error ->
	    ?ERROR_MSG("Cannot delete message in offline spool: ~p", [_Error])
    end,
    {atomic, ok}.

remove_user(LUser, LServer) ->
    sql_queries:del_spool_msg(LServer, LUser).

read_message_headers(LUser, LServer) ->
    case catch ejabberd_sql:sql_query(
		 LServer,
                 ?SQL("select @(xml)s, @(seq)d from spool"
                      " where username=%(LUser)s order by seq")) of
	{selected, Rows} ->
	    lists:flatmap(
	      fun({XML, Seq}) ->
		      case xml_to_offline_msg(XML) of
			  {ok, #offline_msg{from = From,
					    to = To,
					    packet = El}} ->
			      [{Seq, From, To, El}];
			  _ ->
			      []
		      end
	      end, Rows);
	_Err ->
	    []
    end.

read_message(LUser, LServer, Seq) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(xml)s from spool where username=%(LUser)s"
                " and seq=%(Seq)d")) of
	{selected, [{RawXML}|_]} ->
	    case xml_to_offline_msg(RawXML) of
		{ok, Msg} ->
		    {ok, Msg};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

remove_message(LUser, LServer, Seq) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from spool where username=%(LUser)s"
           " and seq=%(Seq)d")),
    ok.

read_all_messages(LUser, LServer) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(xml)s from spool where "
                      "username=%(LUser)s order by seq")) of
        {selected, Rs} ->
            lists:flatmap(
              fun({XML}) ->
		      case xml_to_offline_msg(XML) of
			  {ok, Msg} -> [Msg];
			  _ -> []
		      end
              end, Rs);
        _ ->
	    []
    end.

remove_all_messages(LUser, LServer) ->
    sql_queries:del_spool_msg(LServer, LUser),
    {atomic, ok}.

count_messages(LUser, LServer) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(count(*))d from spool "
                      "where username=%(LUser)s")) of
        {selected, [{Res}]} ->
            Res;
        _ -> 0
    end.

export(_Server) ->
    [{offline_msg,
      fun(Host, #offline_msg{us = {LUser, LServer},
                             timestamp = TimeStamp, from = From, to = To,
                             packet = Packet})
            when LServer == Host ->
              Packet1 = jlib:replace_from_to(From, To, Packet),
              Packet2 = jlib:add_delay_info(Packet1, LServer, TimeStamp,
                                            <<"Offline Storage">>),
              XML = fxml:element_to_binary(Packet2),
              [?SQL("delete from spool where username=%(LUser)s;"),
               ?SQL("insert into spool(username, xml) values ("
                    "%(LUser)s, %(XML)s);")];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, xml from spool;">>,
      fun([LUser, XML]) ->
              El = #xmlel{} = fxml_stream:parse_element(XML),
              From = #jid{} = jid:from_string(
                                fxml:get_attr_s(<<"from">>, El#xmlel.attrs)),
              To = #jid{} = jid:from_string(
                              fxml:get_attr_s(<<"to">>, El#xmlel.attrs)),
              Stamp = fxml:get_path_s(El, [{elem, <<"delay">>},
                                          {attr, <<"stamp">>}]),
              TS = case jlib:datetime_string_to_timestamp(Stamp) of
                       {_, _, _} = Now ->
                           Now;
                       undefined ->
                           p1_time_compat:timestamp()
                   end,
              Expire = mod_offline:find_x_expire(TS, El#xmlel.children),
              #offline_msg{us = {LUser, LServer},
                           from = From, to = To,
			   packet = El,
                           timestamp = TS, expire = Expire}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
xml_to_offline_msg(XML) ->
    case fxml_stream:parse_element(XML) of
	#xmlel{} = El ->
	    el_to_offline_msg(El);
	Err ->
	    ?ERROR_MSG("got ~p when parsing XML packet ~s",
		       [Err, XML]),
	    Err
    end.

el_to_offline_msg(El) ->
    To_s = fxml:get_tag_attr_s(<<"to">>, El),
    From_s = fxml:get_tag_attr_s(<<"from">>, El),
    To = jid:from_string(To_s),
    From = jid:from_string(From_s),
    if To == error ->
	    ?ERROR_MSG("failed to get 'to' JID from offline XML ~p", [El]),
	    {error, bad_jid_to};
       From == error ->
	    ?ERROR_MSG("failed to get 'from' JID from offline XML ~p", [El]),
	    {error, bad_jid_from};
       true ->
	    {ok, #offline_msg{us = {To#jid.luser, To#jid.lserver},
			      from = From,
			      to = To,
			      timestamp = undefined,
			      expire = undefined,
			      packet = El}}
    end.
