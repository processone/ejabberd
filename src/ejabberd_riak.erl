%%%----------------------------------------------------------------------
%%% File    : ejabberd_riak.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve Riak connection
%%% Created : 29 Dec 2011 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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

-module(ejabberd_riak).
-author('alexey@process-one.net').

%% External exports
-export([start_link/1,
         put/4,
         put/5,
         get_object/3,
         get/3,
         get_objects_by_index/4,
         get_by_index/4,
         get_keys_by_index/4,
         count_by_index/4,
         delete/3]).

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(StartInterval) ->
    {ok, Pid} = riakc_pb_socket:start_link(
                  "127.0.0.1", 8081,
                  [auto_reconnect]),
    ejabberd_riak_sup:add_pid(Pid),
    {ok, Pid}.

make_bucket(Host, Table) ->
    iolist_to_binary([Host, $@, Table]).

put(Host, Table, Key, Value) ->
    Bucket = make_bucket(Host, Table),
    Obj = riakc_obj:new(Bucket, Key, Value),
    riakc_pb_socket:put(ejabberd_riak_sup:get_random_pid(), Obj).

put(Host, Table, Key, Value, Indexes) ->
    Bucket = make_bucket(Host, Table),
    Obj = riakc_obj:new(Bucket, Key, Value),
    MetaData = dict:store(<<"index">>, Indexes, dict:new()),
    Obj2 = riakc_obj:update_metadata(Obj, MetaData),
    riakc_pb_socket:put(ejabberd_riak_sup:get_random_pid(), Obj2).

get_object(Host, Table, Key) ->
    Bucket = make_bucket(Host, Table),
    riakc_pb_socket:get(ejabberd_riak_sup:get_random_pid(), Bucket, Key).

get(Host, Table, Key) ->
    case get_object(Host, Table, Key) of
        {ok, Obj} ->
            {ok, riakc_obj:get_value(Obj)};
        Error ->
            Error
    end.

get_objects_by_index(Host, Table, Index, Key) ->
    Bucket = make_bucket(Host, Table),
    case riakc_pb_socket:mapred(
           ejabberd_riak_sup:get_random_pid(),
           {index, Bucket, Index, Key},
           [{map, {modfun, riak_kv_mapreduce, map_identity}, none, true}]) of
        {ok, [{_, Objs}]} ->
            {ok, Objs};
        Error ->
            Error
    end.

get_by_index(Host, Table, Index, Key) ->
    Bucket = make_bucket(Host, Table),
    case riakc_pb_socket:mapred(
           ejabberd_riak_sup:get_random_pid(),
           {index, Bucket, Index, Key},
           [{map, {modfun, riak_kv_mapreduce, map_object_value},
             none, true}]) of
        {ok, [{_, Objs}]} ->
            {ok, Objs};
        Error ->
            Error
    end.

get_keys_by_index(Host, Table, Index, Key) ->
    Bucket = make_bucket(Host, Table),
    case riakc_pb_socket:mapred(
           ejabberd_riak_sup:get_random_pid(),
           {index, Bucket, Index, Key},
           []) of
        {ok, [{_, Ls}]} ->
            {ok, [K || {_, K} <- Ls]};
        Error ->
            Error
    end.

count_by_index(Host, Table, Index, Key) ->
    Bucket = make_bucket(Host, Table),
    case riakc_pb_socket:mapred(
           ejabberd_riak_sup:get_random_pid(),
           {index, Bucket, Index, Key},
           [{reduce, {modfun, riak_kv_mapreduce, reduce_count_inputs},
             none, true}]) of
        {ok, [{_, [Cnt]}]} ->
            {ok, Cnt};
        Error ->
            Error
    end.

delete(Host, Table, Key) ->
    Bucket = make_bucket(Host, Table),
    riakc_pb_socket:delete(ejabberd_riak_sup:get_random_pid(), Bucket, Key).

