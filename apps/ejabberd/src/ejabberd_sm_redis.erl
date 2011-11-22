%%%-------------------------------------------------------------------
%%% @author Konrad Kaplita <konrad.kaplita@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Implementation of Redis-based session manager
%%%
%%% @end
%%% Created : 17 Nov 2011 by Konrad Kaplita <konrad.kaplita@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_redis).

-behavior(ejabberd_gen_sm).

-include("ejabberd.hrl").

-export([start/1,
         get_sessions/2,
         get_sessions/3,
         create_session/4,
         update_session/4,
         delete_session/4,
         cleanup/1,
         total_count/0,
         unique_count/0]).

-spec start(list()) -> any().
start(Opts) ->
    ejabberd_redis:start_link(Opts).

-spec get_sessions(binary(), binary()) -> list(#session{}).
get_sessions(User, Server) ->
    Sessions = ejabberd_redis:cmd(["HVALS", hash(User, Server)]),
    lists:map(fun(S) ->
                      binary_to_term(S)
              end, Sessions).

-spec get_sessions(binary(), binary(), binary()) -> list(#session{}).
get_sessions(User, Server, Resource) ->
    case ejabberd_redis:cmd(["HGET", hash(User, Server), Resource]) of
        undefined ->
            [];
        Session ->
            [binary_to_term(Session)]
    end.

-spec create_session(binary(), binary(), binary(), #session{}) -> ok | {error, term()}.
create_session(User, Server, Resource, Session) ->
    update_session(User, Server, Resource, Session).

-spec update_session(binary(), binary(), binary(), #session{}) -> ok | {error, term()}.
update_session(User, Server, Resource, Session) ->
    ejabberd_redis:cmd(["SADD", n(node()), hash(User, Server, Resource)]),
    ejabberd_redis:cmd(["HSET", hash(User, Server), Resource, term_to_binary(Session)]).

-spec delete_session(tuple(), binary(), binary(), binary()) -> ok.
delete_session(_SID, User, Server, Resource) ->
    ejabberd_redis:cmd(["HDEL", hash(User, Server), Resource]),
    ejabberd_redis:cmd(["SREM", n(node()), hash(User, Server, Resource)]).

-spec cleanup(atom()) -> ok.
cleanup(Node) ->
    Hashes = ejabberd_redis:cmd(["SMEMBERS", n(Node)]),
    ejabberd_redis:cmd(["DEL", n(Node)]),
    lists:foreach(fun(H) ->
                          [_, U, S, R] = re:split(H, ":"),
                          ejabberd_redis:cmd(["HDEL", hash(U, S), R])
                  end, Hashes).

-spec total_count() -> integer().
total_count() ->
    {Counts, _} = rpc:multicall(supervisor, count_children, [ejabberd_c2s_sup]),
    lists:sum([proplists:get_value(active, Count, 0) || Count <- Counts, is_list(Count)]).

-spec unique_count() -> integer().
unique_count() ->
    length(ejabberd_redis:cmd(["KEYS", "s:*"])).

%% Internal functions
-spec hash(binary(), binary()) -> iolist().
hash(Val1, Val2) ->
    ["s:", Val1, ":", Val2].

-spec hash(binary(), binary(), binary()) -> iolist().
hash(Val1, Val2, Val3) ->
    ["s:", Val1, ":", Val2, ":", Val3].

-spec n(atom()) -> iolist().
n(Node) ->
    ["n:", atom_to_list(Node)].
