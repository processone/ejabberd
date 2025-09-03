%%%-------------------------------------------------------------------
%%% File    : mod_mam_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-module(mod_mam_mnesia).

-behaviour(mod_mam).

%% API
-export([init/2,
         remove_user/2,
         remove_room/3,
         delete_old_messages/3,
         extended_fields/1,
         store/10,
         write_prefs/4,
         get_prefs/2,
         select/6,
         remove_from_archive/3,
         is_empty_for_user/2,
         is_empty_for_room/3,
         delete_old_messages_batch/5,
         transform/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-include("logger.hrl").
-include("mod_mam.hrl").

-define(BIN_GREATER_THAN(A, B),
        ((A > B andalso byte_size(A) == byte_size(B)) orelse
         byte_size(A) > byte_size(B))).
-define(BIN_LESS_THAN(A, B),
        ((A < B andalso byte_size(A) == byte_size(B)) orelse
         byte_size(A) < byte_size(B))).

-define(TABLE_SIZE_LIMIT, 2000000000).  % A bit less than 2 GiB.


%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    try
        {atomic, _} = ejabberd_mnesia:create(
                        ?MODULE,
                        archive_msg,
                        [{disc_only_copies, [node()]},
                         {type, bag},
                         {attributes, record_info(fields, archive_msg)}]),
        {atomic, _} = ejabberd_mnesia:create(
                        ?MODULE,
                        archive_prefs,
                        [{disc_only_copies, [node()]},
                         {attributes, record_info(fields, archive_prefs)}]),
        ok
    catch
        _:{badmatch, _} ->
            {error, db_failure}
    end.


remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() ->
                mnesia:delete({archive_msg, US}),
                mnesia:delete({archive_prefs, US})
        end,
    mnesia:transaction(F).


remove_room(_LServer, LName, LHost) ->
    remove_user(LName, LHost).


remove_from_archive(LUser, LHost, Key) when is_binary(LUser) ->
    remove_from_archive({LUser, LHost}, LHost, Key);
remove_from_archive(US, _LServer, none) ->
    case mnesia:transaction(fun() -> mnesia:delete({archive_msg, US}) end) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end;
remove_from_archive(US, _LServer, #jid{} = WithJid) ->
    Peer = jid:remove_resource(jid:split(WithJid)),
    F = fun() ->
                Msgs = mnesia:select(
                         archive_msg,
                         ets:fun2ms(
                           fun(#archive_msg{us = US1, bare_peer = Peer1} = Msg)
                                 when US1 == US, Peer1 == Peer -> Msg
                           end)),
                lists:foreach(fun mnesia:delete_object/1, Msgs)
        end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end;
remove_from_archive(US, _LServer, StanzaId) ->
    Timestamp = misc:usec_to_now(StanzaId),
    F = fun() ->
                Msgs = mnesia:select(
                         archive_msg,
                         ets:fun2ms(
                           fun(#archive_msg{us = US1, timestamp = Timestamp1} = Msg)
                                 when US1 == US, Timestamp1 == Timestamp -> Msg
                           end)),
                lists:foreach(fun mnesia:delete_object/1, Msgs)
        end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.


delete_old_messages(global, TimeStamp, Type) ->
    mnesia:change_table_copy_type(archive_msg, node(), disc_copies),
    Result = delete_old_user_messages(mnesia:dirty_first(archive_msg), TimeStamp, Type),
    mnesia:change_table_copy_type(archive_msg, node(), disc_only_copies),
    Result.


delete_old_user_messages('$end_of_table', _TimeStamp, _Type) ->
    ok;
delete_old_user_messages(User, TimeStamp, Type) ->
    F = fun() ->
                Msgs = mnesia:read(archive_msg, User),
                Keep = lists:filter(
                         fun(#archive_msg{
                               timestamp = MsgTS,
                               type = MsgType
                              }) ->
                                 MsgTS >= TimeStamp orelse (Type /= all andalso
                                                            Type /= MsgType)
                         end,
                         Msgs),
                if
                    length(Keep) < length(Msgs) ->
                        mnesia:delete({archive_msg, User}),
                        lists:foreach(fun(Msg) -> mnesia:write(Msg) end, Keep);
                    true ->
                        ok
                end
        end,
    NextRecord = mnesia:dirty_next(archive_msg, User),
    case mnesia:transaction(F) of
        {atomic, ok} ->
            delete_old_user_messages(NextRecord, TimeStamp, Type);
        {aborted, Err} ->
            ?ERROR_MSG("Cannot delete old MAM messages: ~ts", [Err]),
            Err
    end.


delete_batch('$end_of_table', _LServer, _TS, _Type, Num) ->
    {Num, '$end_of_table'};
delete_batch(LastUS, _LServer, _TS, _Type, 0) ->
    {0, LastUS};
delete_batch(none, LServer, TS, Type, Num) ->
    delete_batch(mnesia:first(archive_msg), LServer, TS, Type, Num);
delete_batch({_, LServer2} = LastUS, LServer, TS, Type, Num) when LServer /= LServer2 ->
    delete_batch(mnesia:next(archive_msg, LastUS), LServer, TS, Type, Num);
delete_batch(LastUS, LServer, TS, Type, Num) ->
    Left =
        lists:foldl(
          fun(_, 0) ->
                  0;
             (#archive_msg{timestamp = TS2, type = Type2} = O, Num2) when TS2 < TS, (Type == all orelse Type == Type2) ->
                  mnesia:delete_object(O),
                  Num2 - 1;
             (_, Num2) ->
                  Num2
          end,
          Num,
          mnesia:wread({archive_msg, LastUS})),
    case Left of
        0 -> {0, LastUS};
        _ -> delete_batch(mnesia:next(archive_msg, LastUS), LServer, TS, Type, Left)
    end.


delete_old_messages_batch(LServer, TimeStamp, Type, Batch, LastUS) ->
    R = mnesia:transaction(
          fun() ->
                  {Num, NextUS} = delete_batch(LastUS, LServer, TimeStamp, Type, Batch),
                  {Batch - Num, NextUS}
          end),
    case R of
        {atomic, {Num, State}} ->
            {ok, State, Num};
        {aborted, Err} ->
            {error, Err}
    end.


extended_fields(_) ->
    [].


store(Pkt,
      _,
      {LUser, LServer},
      Type,
      Peer,
      Nick,
      _Dir,
      TS,
      OriginID,
      Retract) ->
    case Retract of
        {true, RID} ->
            mnesia:transaction(
              fun() ->
                      {PUser, PServer, _} = jid:tolower(Peer),
                      Msgs = mnesia:select(
                               archive_msg,
                               ets:fun2ms(
                                 fun(#archive_msg{
                                       us = US1,
                                       bare_peer = Peer1,
                                       origin_id = OriginID1
                                      } = Msg)
                                       when US1 == {LUser, LServer},
                                            Peer1 == {PUser, PServer, <<>>},
                                            OriginID1 == RID -> Msg
                                 end)),
                      lists:foreach(fun mnesia:delete_object/1, Msgs)
              end);
        false -> ok
    end,
    case {mnesia:table_info(archive_msg, disc_only_copies),
          mnesia:table_info(archive_msg, memory)} of
        {[_ | _], TableSize} when TableSize > ?TABLE_SIZE_LIMIT ->
            ?ERROR_MSG("MAM archives too large, won't store message for ~ts@~ts",
                       [LUser, LServer]),
            {error, overflow};
        _ ->
            LPeer = {PUser, PServer, _} = jid:tolower(Peer),
            F = fun() ->
                        mnesia:write(
                          #archive_msg{
                            us = {LUser, LServer},
                            id = integer_to_binary(TS),
                            timestamp = misc:usec_to_now(TS),
                            peer = LPeer,
                            bare_peer = {PUser, PServer, <<>>},
                            type = Type,
                            nick = Nick,
                            packet = Pkt,
                            origin_id = OriginID
                           })
                end,
            case mnesia:transaction(F) of
                {atomic, ok} ->
                    ok;
                {aborted, Err} ->
                    ?ERROR_MSG("Cannot add message to MAM archive of ~ts@~ts: ~ts",
                               [LUser, LServer, Err]),
                    Err
            end
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


select(_LServer,
       JidRequestor,
       #jid{luser = LUser, lserver = LServer} = JidArchive,
       Query,
       RSM,
       MsgType) ->
    Start = proplists:get_value(start, Query),
    End = proplists:get_value('end', Query),
    With = proplists:get_value(with, Query),
    LWith = if
                With /= undefined -> jid:tolower(With);
                true -> undefined
            end,
    MS = make_matchspec(LUser, LServer, Start, End, LWith),
    Msgs = mnesia:dirty_select(archive_msg, MS),
    SortedMsgs = lists:keysort(#archive_msg.timestamp, Msgs),
    {FilteredMsgs, IsComplete} = filter_by_rsm(SortedMsgs, RSM),
    Count = length(Msgs),
    Result = {lists:flatmap(
                fun(Msg) ->
                        case mod_mam:msg_to_el(
                               Msg, MsgType, JidRequestor, JidArchive) of
                            {ok, El} ->
                                [{Msg#archive_msg.id,
                                  binary_to_integer(Msg#archive_msg.id),
                                  El}];
                            {error, _} ->
                                []
                        end
                end,
                FilteredMsgs),
              IsComplete,
              Count},
    erlang:garbage_collect(),
    Result.


is_empty_for_user(LUser, LServer) ->
    mnesia:dirty_read(archive_msg, {LUser, LServer}) == [].


is_empty_for_room(_LServer, LName, LHost) ->
    is_empty_for_user(LName, LHost).


%%%===================================================================
%%% Internal functions
%%%===================================================================
make_matchspec(LUser, LServer, Start, undefined, With) ->
    %% List is always greater than a tuple
    make_matchspec(LUser, LServer, Start, [], With);
make_matchspec(LUser, LServer, Start, End, {_, _, <<>>} = With) ->
    ets:fun2ms(
      fun(#archive_msg{
            timestamp = TS,
            us = US,
            bare_peer = BPeer
           } = Msg)
            when Start =< TS,
                 End >= TS,
                 US == {LUser, LServer},
                 BPeer == With ->
              Msg
      end);
make_matchspec(LUser, LServer, Start, End, {_, _, _} = With) ->
    ets:fun2ms(
      fun(#archive_msg{
            timestamp = TS,
            us = US,
            peer = Peer
           } = Msg)
            when Start =< TS,
                 End >= TS,
                 US == {LUser, LServer},
                 Peer == With ->
              Msg
      end);
make_matchspec(LUser, LServer, Start, End, undefined) ->
    ets:fun2ms(
      fun(#archive_msg{
            timestamp = TS,
            us = US,
            peer = Peer
           } = Msg)
            when Start =< TS,
                 End >= TS,
                 US == {LUser, LServer} ->
              Msg
      end).


filter_by_rsm(Msgs, undefined) ->
    {Msgs, true};
filter_by_rsm(_Msgs, #rsm_set{max = Max}) when Max < 0 ->
    {[], true};
filter_by_rsm(Msgs, #rsm_set{max = Max, before = Before, 'after' = After}) ->
    NewMsgs = if
                  is_binary(After), After /= <<"">> ->
                      lists:filter(
                        fun(#archive_msg{id = I}) ->
                                ?BIN_GREATER_THAN(I, After)
                        end,
                        Msgs);
                  is_binary(Before), Before /= <<"">> ->
                      lists:foldl(
                        fun(#archive_msg{id = I} = Msg, Acc)
                              when ?BIN_LESS_THAN(I, Before) ->
                                [Msg | Acc];
                           (_, Acc) ->
                                Acc
                        end,
                        [],
                        Msgs);
                  is_binary(Before), Before == <<"">> ->
                      lists:reverse(Msgs);
                  true ->
                      Msgs
              end,
    filter_by_max(NewMsgs, Max).


filter_by_max(Msgs, undefined) ->
    {Msgs, true};
filter_by_max(Msgs, Len) when is_integer(Len), Len >= 0 ->
    {lists:sublist(Msgs, Len), length(Msgs) =< Len};
filter_by_max(_Msgs, _Junk) ->
    {[], true}.


transform({archive_msg, US, ID, Timestamp, Peer, BarePeer,
                        Packet, Nick, Type}) ->
    #archive_msg{
      us = US,
      id = ID,
      timestamp = Timestamp,
      peer = Peer,
      bare_peer = BarePeer,
      packet = Packet,
      nick = Nick,
      type = Type,
      origin_id = <<"">>
     };
transform(Other) ->
    Other.
