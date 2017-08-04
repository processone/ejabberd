%%%-------------------------------------------------------------------
%%% @author sngyai <progyang@gmail.com>
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2016 3:22 PM
%%%-------------------------------------------------------------------
-module(mod_mam_redis).
-author("sngyai").

-behaviour(mod_mam).

%% API
-export([
  init/2,
  remove_user/2,
  remove_room/3,
  delete_old_messages/3,
  extended_fields/0,
  store/7,
  write_prefs/4,
  get_prefs/2,
  select/8
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("mod_mam.hrl").

-define(BIN_GREATER_THAN(A, B),
  ((A > B andalso byte_size(A) == byte_size(B))
    orelse byte_size(A) > byte_size(B))).
-define(BIN_LESS_THAN(A, B),
  ((A < B andalso byte_size(A) == byte_size(B))
    orelse byte_size(A) < byte_size(B))).

-define(ARCHIVE_PREFIX, "ejabberd:mam:archive:").
-define(TTL, 30).

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
  ok.

remove_user(LUser, LServer) ->
  USKey = us_to_key_archive({LUser, LServer}),
  case ejabberd_redis:q(["ZREMRANGEBYSCORE", USKey, "-inf", "+inf"]) of
    {ok, _} ->
      ok;
    Errs ->
      ?ERROR_MSG("failed to remove_user redis table for "
      "server ~s, user ~s: ~p", [LServer, LUser, Errs])
  end.

remove_room(_LServer, LName, LHost) ->
  remove_user(LName, LHost).

delete_old_messages(global, TimeStamp, Type) ->
  case all_keys_prefix("us:*") of
    {ok, USKeys} ->
      lists:foreach(
        fun(USKeyBin) ->
          USKey = term_to_binary(USKeyBin),
          case ejabberd_redis:q(["ZRANGE", USKey, "-inf", TimeStamp]) of
            {ok, TempVals} ->
              case store_to_temp(TempVals) of
                [] ->
                  [];
                {ID, TempTypeKey} ->
                  TempStoreResultKey = temp_result_key(ID),
                  TypeKey = type_to_key_archive(Type),
                  SearchKeys = [TempTypeKey] ++ [TypeKey],
                  case interstore_to_temp(TempStoreResultKey, SearchKeys) of
                    {ok, Vals} ->
                      delete_old_messages_us(USKey, Vals),
                      delete_old_messages_type(TypeKey, Vals),
                      delete_old_messages_bare_peer(Vals),
                      delete_old_messages_lpeer(Vals);
                    [] ->
                      []
                  end
              end;
            Errs ->
              ?ERROR_MSG("failed to remove_user redis table for "
              "~s: ~p", [USKey, Errs])
          end
        end, USKeys);
    Errs ->
      ?ERROR_MSG("delete_old_messages ERROR ~p", [Errs])
  end.
delete_old_messages_us(USKey, Vals) ->
  [["SREM", USKey, Val]||Val<-Vals].
delete_old_messages_type(TypeKey, Vals) ->
  [["SREM", TypeKey, Val]||Val<-Vals].
delete_old_messages_bare_peer(Vals) ->
  case all_keys_prefix("barepeer:*") of
    {ok, BarePeerKeys} ->
      lists:map(
        fun(BarePeerKey)->
          [["SREM", BarePeerKey, Val]||Val<-Vals]
        end, BarePeerKeys);
    [] ->
      []
  end.
delete_old_messages_lpeer(Vals) ->
  case all_keys_prefix("lpeer:*") of
    {ok, LPeerKeys} ->
      lists:map(
        fun(LPeerKey)->
          [["SREM", LPeerKey, Val]||Val<-Vals]
        end, LPeerKeys);
    [] ->
      []
  end.

all_keys_prefix(Prefix) ->
  ejabberd_redis:q(["KEYS", lists:append(?ARCHIVE_PREFIX, Prefix)]).


extended_fields() ->
  [].

store(Pkt, _, {LUser, LServer}, Type, Peer, Nick, _Dir) ->
  TS = p1_time_compat:timestamp(),
  TSInteger = now_to_usec(TS),
  ID = jlib:integer_to_binary(TSInteger),
  BarePeer = jid:to_string(jid:tolower(jid:remove_resource(Peer))),
  LPeer = jid:to_string(jid:tolower(Peer)),
  Message =
    #archive_msg{us = {LUser, LServer},
      id = ID,
      timestamp = TS,
      peer = LPeer,
      bare_peer = BarePeer,
      type = Type,
      nick = Nick,
      packet = Pkt},
  T = term_to_binary(Message),

  USKey = us_to_key_archive({LUser, LServer}),
  BarePeerKey = bare_peer_to_key_archive(BarePeer),
  LPeerKey = lpeer_to_key_archive(LPeer),
  TypeKey = type_to_key_archive(Type),

  case ejabberd_redis:qp([
    ["ZADD", USKey, TSInteger, T],
    ["SADD", BarePeerKey, T],
    ["SADD", LPeerKey, T],
    ["SADD", TypeKey, T]
  ]) of
    [{ok, _},{ok, _},{ok, _},{ok, _}] ->
      ok;
    Err ->
      ?ERROR_MSG("failed to store mam for redis: ~p", [Err])
  end.

write_prefs(LUser, LServer, Prefs, _ServerHost) ->
  T = term_to_binary(Prefs),
  USKey = us_to_key_prefs({LUser, LServer}),
  case ejabberd_redis:q(["HSET", USKey, key, T]) of
    {ok, _} ->
      ok;
    Err ->
      ?ERROR_MSG("failed to set prefs for redis: ~p", [Err])
  end.

get_prefs(LUser, LServer) ->
  USKey = us_to_key_prefs({LUser, LServer}),
  case ejabberd_redis:q(["HGET", USKey, key]) of
    {ok, undefined} ->
      error;
    {ok, Val} ->
      binary_to_term(Val);
    Err ->
      ?ERROR_MSG("failed to get prefs from redis: ~p", [Err]),
      []
  end.

select(_LServer, JidRequestor,
    #jid{luser = LUser, lserver = LServer} = JidArchive,
    Start, End, With, RSM, MsgType) ->

  Msgs =
    case us_start_end(LUser, LServer, Start, End) of
      [] ->
        [];
      USStartEnd ->
        case store_to_temp(USStartEnd) of
          [] ->
            [];
          {ID, TempTypeKey} ->
            TempStoreResultKey = temp_result_key(ID),
            SearchKeys = search_keys(TempTypeKey, With, MsgType),
            case interstore_to_temp(TempStoreResultKey, SearchKeys) of
              {ok, Vals} ->
                decode_val_list(Vals);
              [] ->
                []
            end
        end
    end,
  SortedMsgs = lists:keysort(#archive_msg.timestamp, Msgs),

  {FilteredMsgs, IsComplete} = filter_by_rsm(SortedMsgs, RSM),
  Count = length(Msgs),
  ResultMsgs = lists:map(
    fun(Msg) ->
      {Msg#archive_msg.id,
        jlib:binary_to_integer(Msg#archive_msg.id),
        mod_mam:msg_to_el(Msg, MsgType, JidRequestor, JidArchive)}
    end, FilteredMsgs),
  ?DEBUG("select result msgs:~p IsComplete:~p Count:~p~n",
    [ResultMsgs, IsComplete, Count]),
  {ResultMsgs, IsComplete, Count}.

us_start_end(LUser, LServer, Start, End) ->
  USKey = us_to_key_archive({LUser, LServer}),
  RealStart =
    if
      Start =:= none -> "-inf";
      true -> Start
    end,
  RealEnd =
    if
      End =:= [] -> "+inf";
      true -> End
    end,
  case ejabberd_redis:q(["ZRANGEBYSCORE", USKey, RealStart, RealEnd]) of
    {ok, _Vals} when is_list(_Vals) ->
      _Vals;
    Err ->
      ?ERROR_MSG("failed to get mam from redis start ~p end ~p: ~p", [RealStart, RealEnd, Err]),
      []
  end.

search_keys(TempTypeKey, With, MsgType) ->
  WithKey =
    case With of
      {_, _, <<>>} ->
        bare_peer_to_key_archive(jid:to_string(With));
      {_, _, _} ->
        lpeer_to_key_archive(jid:to_string(With));
      none ->
        []
    end,
  TypeKey = type_to_key_archive(MsgType),
  [WithKey] ++ [TempTypeKey] ++ [TypeKey].

store_to_temp(Vals) ->
  ID = jlib:integer_to_binary(now_to_usec(p1_time_compat:timestamp())),
  TempTypeKey = temp_store_key(ID),
  StoreElements = [["SADD", TempTypeKey, Val]||Val<-Vals],
  ResultList = ejabberd_redis:qp(lists:append(StoreElements, [["EXPIRE", TempTypeKey, ?TTL]])),
  case lists:all(fun(Result) -> element(1, Result) == ok end, ResultList) of
    true ->
      {ID, TempTypeKey};
    false ->
      ?ERROR_MSG("failed to store_to_temp to redis ResultList ~p: ~p", [Vals, ResultList]),
      []
  end.

interstore_to_temp(TempStoreResultKey, Keys) ->
  case ejabberd_redis:qp([["SINTERSTORE", TempStoreResultKey] ++ Keys, ["EXPIRE", TempStoreResultKey, ?TTL]]) of
    [{ok, CntBin},{ok, _}] ->
      if
        CntBin > <<"0">> ->
          case ejabberd_redis:q(["SMEMBERS", TempStoreResultKey]) of
            {ok, ResVals} ->
              {ok, ResVals};
            Error ->
              ?DEBUG("failed to get final result from redis TempStoreResultKey ~p ResVals: ~p : ~p~n", [TempStoreResultKey, Error]),
              []
          end;
        true ->
          ?DEBUG("failed to get final result from redis TempStoreResultKey ~p CntBin: ~p : ~p~n", [TempStoreResultKey, CntBin]),
          []
      end;
    Err2 ->
      ?DEBUG("failed to interstore to redis TempStoreResultKey ~p SearchKeys: ~p : ~p~n", [TempStoreResultKey, Keys, Err2]),
      []
  end.

now_to_usec({MSec, Sec, USec}) ->
  (MSec*1000000 + Sec)*1000000 + USec.

us_to_key_archive({LUser, LServer}) ->
  <<?ARCHIVE_PREFIX, "us:", LUser/binary, "@", LServer/binary>>.

temp_store_key(ID) ->
  <<?ARCHIVE_PREFIX, "temp:store", ID/binary>>.

temp_result_key(ID) ->
  <<?ARCHIVE_PREFIX, "temp:result:", ID/binary>>.

bare_peer_to_key_archive(BarePeer) ->
  <<?ARCHIVE_PREFIX, "barepeer:", BarePeer/binary>>.

lpeer_to_key_archive(LPeer) ->
  <<?ARCHIVE_PREFIX, "lpeer:", LPeer/binary>>.

type_to_key_archive(Type) ->
  TypeBin = atom_to_binary(Type, utf8),
  <<?ARCHIVE_PREFIX, "type:", TypeBin/binary>>.

us_to_key_prefs({LUser, LServer}) ->
  <<"ejabberd:mam:prefs:", LUser/binary, "@", LServer/binary>>.

decode_val_list([Val|T]) ->
  [binary_to_term(Val)|decode_val_list(T)];
decode_val_list([]) ->
  [].

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

