%%%-------------------------------------------------------------------
%%% File    : mod_offline_sql.erl
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

-module(mod_offline_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_offline).

-export([init/2, store_message/1, pop_messages/2, remove_expired_messages/1,
	 remove_old_messages/2, remove_user/2, read_message_headers/2,
	 read_message/3, remove_message/3, read_all_messages/2,
	 remove_all_messages/2, count_messages/2, import/1, export/1]).

-include("xmpp.hrl").
-include("mod_offline.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

store_message(#offline_msg{us = {LUser, LServer}} = M) ->
    From = M#offline_msg.from,
    To = M#offline_msg.to,
    Packet = xmpp:set_from_to(M#offline_msg.packet, From, To),
    NewPacket = xmpp_util:add_delay_info(
		  Packet, jid:make(LServer),
		  M#offline_msg.timestamp,
		  <<"Offline Storage">>),
    XML = fxml:element_to_binary(
	    xmpp:encode(NewPacket)),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("insert into spool(username, xml) values "
		"(%(LUser)s, %(XML)s)")) of
	{updated, _} ->
	    ok;
	_ ->
	    {error, db_failure}
    end.

pop_messages(LUser, LServer) ->
    case get_and_del_spool_msg_t(LServer, LUser) of
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
    case ejabberd_sql:sql_query(
	   LServer,
	   [<<"DELETE FROM spool"
	      " WHERE created_at < "
	      "NOW() - INTERVAL '">>,
	    integer_to_list(Days), <<"' DAY;">>]) of
	{updated, N} ->
	    ?INFO_MSG("~p message(s) deleted from offline spool", [N]);
	_Error ->
	    ?ERROR_MSG("Cannot delete message in offline spool: ~p", [_Error])
    end,
    {atomic, ok}.

remove_user(LUser, LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from spool where username=%(LUser)s")).

read_message_headers(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(xml)s, @(seq)d from spool"
		" where username=%(LUser)s order by seq")) of
	{selected, Rows} ->
	    lists:flatmap(
	      fun({XML, Seq}) ->
		      case xml_to_offline_msg(XML) of
			  {ok, #offline_msg{from = From,
					    to = To,
					    timestamp = TS,
					    packet = El}} ->
			      [{Seq, From, To, TS, El}];
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
    case ejabberd_sql:sql_query(
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
    remove_user(LUser, LServer),
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
      fun(Host, #offline_msg{us = {LUser, LServer}})
            when LServer == Host ->
		      [?SQL("delete from spool where username=%(LUser)s;")];
         (_Host, _R) ->
              []
      end},
     {offline_msg,
      fun(Host, #offline_msg{us = {LUser, LServer},
                             timestamp = TimeStamp, from = From, to = To,
                             packet = El})
            when LServer == Host ->
	      try xmpp:decode(El, ?NS_CLIENT, [ignore_els]) of
		  Packet ->
		      Packet1 = xmpp:set_from_to(Packet, From, To),
		      Packet2 = xmpp_util:add_delay_info(
				  Packet1, jid:make(LServer),
				  TimeStamp, <<"Offline Storage">>),
		      XML = fxml:element_to_binary(xmpp:encode(Packet2)),
		      [?SQL("insert into spool(username, xml) values ("
			    "%(LUser)s, %(XML)s);")]
	      catch _:{xmpp_codec, Why} ->
		      ?ERROR_MSG("failed to decode packet ~p of user ~s@~s: ~s",
				 [El, LUser, LServer, xmpp:format_error(Why)]),
		      []
	      end;
         (_Host, _R) ->
              []
      end}].

import(_) ->
    ok.

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
    try
	To = jid:decode(To_s),
	From = jid:decode(From_s),
	{ok, #offline_msg{us = {To#jid.luser, To#jid.lserver},
			  from = From,
			  to = To,
			  packet = El}}
    catch _:{bad_jid, To_s} ->
	    ?ERROR_MSG("failed to get 'to' JID from offline XML ~p", [El]),
	    {error, bad_jid_to};
	  _:{bad_jid, From_s} ->
	    ?ERROR_MSG("failed to get 'from' JID from offline XML ~p", [El]),
	    {error, bad_jid_from}
    end.

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
