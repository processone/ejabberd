%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_offline_riak).

-behaviour(mod_offline).

-export([init/2, store_messages/5, pop_messages/2, remove_expired_messages/1,
	 remove_old_messages/2, remove_user/2, read_message_headers/2,
	 read_message/3, remove_message/3, read_all_messages/2,
	 remove_all_messages/2, count_messages/2, import/2]).

-include("jlib.hrl").
-include("mod_offline.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

store_messages(Host, {User, _}, Msgs, Len, MaxOfflineMsgs) ->
    Count = if MaxOfflineMsgs =/= infinity ->
                    Len + count_messages(User, Host);
               true -> 0
            end,
    if
        Count > MaxOfflineMsgs ->
            {atomic, discard};
        true ->
	    try
		lists:foreach(
		  fun(#offline_msg{us = US,
				   timestamp = TS} = M) ->
			  ok = ejabberd_riak:put(
				 M, offline_msg_schema(),
				 [{i, TS}, {'2i', [{<<"us">>, US}]}])
		  end, Msgs),
		{atomic, ok}
	    catch _:{badmatch, Err} ->
		    {atomic, Err}
	    end
    end.

pop_messages(LUser, LServer) ->
    case ejabberd_riak:get_by_index(offline_msg, offline_msg_schema(),
                                    <<"us">>, {LUser, LServer}) of
        {ok, Rs} ->
	    try
                lists:foreach(
                  fun(#offline_msg{timestamp = T}) ->
                          ok = ejabberd_riak:delete(offline_msg, T)
                  end, Rs),
		{ok, lists:keysort(#offline_msg.timestamp, Rs)}
	    catch _:{badmatch, Err} ->
		    Err
	    end;
	Err ->
	    Err
    end.

remove_expired_messages(_LServer) ->
    %% TODO
    {atomic, ok}.

remove_old_messages(_Days, _LServer) ->
    %% TODO
    {atomic, ok}.

remove_user(LUser, LServer) ->
    {atomic, ejabberd_riak:delete_by_index(offline_msg,
                                           <<"us">>, {LUser, LServer})}.

read_message_headers(LUser, LServer) ->
    case ejabberd_riak:get_by_index(
           offline_msg, offline_msg_schema(),
	   <<"us">>, {LUser, LServer}) of
        {ok, Rs} ->
	    Hdrs = lists:map(
		     fun(#offline_msg{from = From, to = To, packet = Pkt,
				      timestamp = TS}) ->
			     Seq = now_to_integer(TS),
			     NewPkt = jlib:add_delay_info(
					Pkt, LServer, TS, <<"Offline Storage">>),
			     {Seq, From, To, NewPkt}
		     end, Rs),
	    lists:keysort(1, Hdrs);
	_Err ->
	    []
    end.

read_message(_LUser, _LServer, I) ->
    TS = integer_to_now(I),
    case ejabberd_riak:get(offline_msg, offline_msg_schema(), TS) of
	{ok, Msg} ->
	    {ok, Msg};
	_ ->
	    error
    end.

remove_message(_LUser, _LServer, I) ->
    TS = integer_to_now(I),
    ejabberd_riak:delete(offline_msg, TS),
    ok.

read_all_messages(LUser, LServer) ->
    case ejabberd_riak:get_by_index(
           offline_msg, offline_msg_schema(),
	   <<"us">>, {LUser, LServer}) of
        {ok, Rs} ->
            lists:keysort(#offline_msg.timestamp, Rs);
        _Err ->
            []
    end.

remove_all_messages(LUser, LServer) ->
    Res = ejabberd_riak:delete_by_index(offline_msg,
                                        <<"us">>, {LUser, LServer}),
    {atomic, Res}.

count_messages(LUser, LServer) ->
    case ejabberd_riak:count_by_index(
           offline_msg, <<"us">>, {LUser, LServer}) of
        {ok, Res} ->
            Res;
        _ ->
            0
    end.

import(_LServer, #offline_msg{us = US, timestamp = TS} = M) ->
    ejabberd_riak:put(M, offline_msg_schema(),
		      [{i, TS}, {'2i', [{<<"us">>, US}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
offline_msg_schema() ->
    {record_info(fields, offline_msg), #offline_msg{}}.

now_to_integer({MS, S, US}) ->
    (MS * 1000000 + S) * 1000000 + US.

integer_to_now(Int) ->
    Secs = Int div 1000000,
    USec = Int rem 1000000,
    MSec = Secs div 1000000,
    Sec = Secs rem 1000000,
    {MSec, Sec, USec}.
