%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_mam_mnesia).

-behaviour(mod_mam).

%% API
-export([init/2, remove_user/2, remove_room/3, delete_old_messages/3,
	 extended_fields/0, store/7, write_prefs/4, get_prefs/2, select/8]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("jlib.hrl").
-include("mod_mam.hrl").

-define(BIN_GREATER_THAN(A, B),
	((A > B andalso byte_size(A) == byte_size(B))
	 orelse byte_size(A) > byte_size(B))).
-define(BIN_LESS_THAN(A, B),
	((A < B andalso byte_size(A) == byte_size(B))
	 orelse byte_size(A) < byte_size(B))).

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    mnesia:create_table(archive_msg,
			[{disc_only_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, archive_msg)}]),
    mnesia:create_table(archive_prefs,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, archive_prefs)}]).

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:delete({archive_msg, US}),
		mnesia:delete({archive_prefs, US})
	end,
    mnesia:transaction(F).

remove_room(_LServer, LName, LHost) ->
    remove_user(LName, LHost).

delete_old_messages(global, TimeStamp, Type) ->
    MS = ets:fun2ms(fun(#archive_msg{timestamp = MsgTS,
				     type = MsgType} = Msg)
			  when MsgTS < TimeStamp,
			       MsgType == Type orelse Type == all ->
			    Msg
		    end),
    OldMsgs = mnesia:dirty_select(archive_msg, MS),
    lists:foreach(fun(Rec) ->
			  ok = mnesia:dirty_delete_object(Rec)
		  end, OldMsgs).

extended_fields() ->
    [].

store(Pkt, _, {LUser, LServer}, Type, Peer, Nick, _Dir) ->
    LPeer = {PUser, PServer, _} = jid:tolower(Peer),
    TS = p1_time_compat:timestamp(),
    ID = jlib:integer_to_binary(now_to_usec(TS)),
    case mnesia:dirty_write(
	   #archive_msg{us = {LUser, LServer},
			id = ID,
			timestamp = TS,
			peer = LPeer,
			bare_peer = {PUser, PServer, <<>>},
			type = Type,
			nick = Nick,
			packet = Pkt}) of
	ok ->
	    {ok, ID};
	Err ->
	    Err
    end.

write_prefs(_LUser, _LServer, Prefs, _ServerHost) ->
    mnesia:dirty_write(Prefs).

get_prefs(LUser, LServer) ->
    case mnesia:dirty_read(archive_prefs, {LUser, LServer}) of
	[Prefs] ->
	    {ok, Prefs};
	_ ->
	    error
    end.

select(_LServer, JidRequestor,
       #jid{luser = LUser, lserver = LServer} = JidArchive,
       Start, End, With, RSM, MsgType) ->
    MS = make_matchspec(LUser, LServer, Start, End, With),
    Msgs = mnesia:dirty_select(archive_msg, MS),
    SortedMsgs = lists:keysort(#archive_msg.timestamp, Msgs),
    {FilteredMsgs, IsComplete} = filter_by_rsm(SortedMsgs, RSM),
    Count = length(Msgs),
    {lists:map(
       fun(Msg) ->
	       {Msg#archive_msg.id,
		jlib:binary_to_integer(Msg#archive_msg.id),
		mod_mam:msg_to_el(Msg, MsgType, JidRequestor, JidArchive)}
       end, FilteredMsgs), IsComplete, Count}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.

make_matchspec(LUser, LServer, Start, End, {_, _, <<>>} = With) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       bare_peer = BPeer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer},
		 BPeer == With ->
	      Msg
      end);
make_matchspec(LUser, LServer, Start, End, {_, _, _} = With) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       peer = Peer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer},
		 Peer == With ->
	      Msg
      end);
make_matchspec(LUser, LServer, Start, End, none) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       peer = Peer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer} ->
	      Msg
      end).

filter_by_rsm(Msgs, none) ->
    {Msgs, true};
filter_by_rsm(_Msgs, #rsm_in{max = Max}) when Max < 0 ->
    {[], true};
filter_by_rsm(Msgs, #rsm_in{max = Max, direction = Direction, id = ID}) ->
    NewMsgs = case Direction of
		  aft when ID /= <<"">> ->
		      lists:filter(
			fun(#archive_msg{id = I}) ->
				?BIN_GREATER_THAN(I, ID)
			end, Msgs);
		  before when ID /= <<"">> ->
		      lists:foldl(
			fun(#archive_msg{id = I} = Msg, Acc)
				when ?BIN_LESS_THAN(I, ID) ->
				[Msg|Acc];
			   (_, Acc) ->
				Acc
			end, [], Msgs);
		  before when ID == <<"">> ->
		      lists:reverse(Msgs);
		  _ ->
		      Msgs
	      end,
    filter_by_max(NewMsgs, Max).

filter_by_max(Msgs, undefined) ->
    {Msgs, true};
filter_by_max(Msgs, Len) when is_integer(Len), Len >= 0 ->
    {lists:sublist(Msgs, Len), length(Msgs) =< Len};
filter_by_max(_Msgs, _Junk) ->
    {[], true}.
