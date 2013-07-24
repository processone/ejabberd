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
         get_sessions/0,
         get_sessions/1,
         get_sessions/2,
         get_sessions/3,
         create_session/4,
         delete_session/4,
         cleanup/1,
         total_count/0,
         unique_count/0]).

-spec start(list()) -> any().
start(Opts) ->
    ejabberd_redis:start_link(Opts).

-spec get_sessions() -> list(list(term())).
get_sessions() ->
    Keys = ejabberd_redis:cmd(["KEYS", hash(<<"*">>)]),
    Res = lists:map(fun(K) ->
                Sessions = ejabberd_redis:cmd(["SMEMBERS", K]),
                lists:map(fun(S) ->
                            Session = binary_to_term(S),
                            { Session#session.usr,
                              Session#session.sid,
                              Session#session.priority,
                              Session#session.info }
                          end,
                Sessions)
          end, Keys),
    lists:flatten(Res).

-spec get_sessions(binary()) -> list(tuple()).
get_sessions(Server) ->
    Keys = ejabberd_redis:cmd(["KEYS", hash(Server)]),
    Res = lists:map(fun(K) ->
                Sessions = ejabberd_redis:cmd(["SMEMBERS", K]),
                lists:map(fun(S) ->
                            Session = binary_to_term(S),
                            {Session#session.usr,
                             Session#session.sid,
                             Session#session.priority,
                             Session#session.info}
                          end,
                Sessions)
          end, Keys),
    lists:flatten(Res).

-spec get_sessions(binary(), binary()) -> list(#session{}).
get_sessions(User, Server) ->
    Sessions = ejabberd_redis:cmd(["SMEMBERS", hash(User, Server)]),

    lists:map(fun(S) ->
                      binary_to_term(S)
              end, Sessions).

-spec get_sessions(binary(), binary(), binary()) -> list(#session{}).
get_sessions(User, Server, Resource) ->
    Sessions = ejabberd_redis:cmd(["SMEMBERS", hash(User, Server, Resource)]),

    lists:map(fun(S) ->
                      binary_to_term(S)
              end, Sessions).

-spec create_session(binary(), binary(), binary(), #session{}) -> ok | {error, term()}.
create_session(User, Server, Resource, Session) ->
    OldSessions = get_sessions(User, Server, Resource),
    BSession = term_to_binary(Session),
    case lists:keysearch(Session#session.sid, #session.sid, OldSessions) of
        {value, OldSession} ->
            BOldSession = term_to_binary(OldSession),

            ejabberd_redis:cmd([["SADD", n(node()), hash(User, Server, Resource, Session#session.sid)],
                                ["SREM", hash(User, Server), BOldSession],
                                ["SREM", hash(User, Server, Resource), BOldSession],
                                ["SADD", hash(User, Server), BSession],
                                ["SADD", hash(User, Server, Resource), BSession]]);
        false ->
            ejabberd_redis:cmd([["SADD", n(node()), hash(User, Server, Resource, Session#session.sid)],
                                ["SADD", hash(User, Server), BSession],
                                ["SADD", hash(User, Server, Resource), BSession]])
    end.

-spec delete_session(tuple(), binary(), binary(), binary()) -> ok.
delete_session(SID, User, Server, Resource) ->
    Sessions = get_sessions(User, Server, Resource),
    case lists:keysearch(SID, #session.sid, Sessions) of
        {value, Session} ->
            BSession = term_to_binary(Session),

            ejabberd_redis:cmd([["SREM", hash(User, Server), BSession],
                                ["SREM", hash(User, Server, Resource), BSession],
                                ["SREM", n(node()), hash(User, Server, Resource, SID)]]);
        false ->
            ok
    end.

-spec cleanup(atom()) -> ok.
cleanup(Node) ->
    Hashes = ejabberd_redis:cmd(["SMEMBERS", n(Node)]),
    ejabberd_redis:cmd(["DEL", n(Node)]),
    lists:foreach(fun(H) ->
                          [_, U, S, R | SID] = re:split(H, ":"),
                          delete_session(SID, U, S, R)
                  end, Hashes).

-spec total_count() -> integer().
total_count() ->
    {Counts, _} = rpc:multicall(supervisor, count_children, [ejabberd_c2s_sup]),
    lists:sum([proplists:get_value(active, Count, 0) || Count <- Counts, is_list(Count)]).

-spec unique_count() -> integer().
unique_count() ->
    length(ejabberd_redis:cmd(["KEYS", "s2:*"])).

%% Internal functions
-spec hash(binary()) -> iolist().
hash(Val1) ->
    ["s3:*:", Val1, ":*"].

-spec hash(binary(), binary()) -> iolist().
hash(Val1, Val2) ->
    ["s2:", Val1, ":", Val2].

-spec hash(binary(), binary(), binary()) -> iolist().
hash(Val1, Val2, Val3) ->
    ["s3:", Val1, ":", Val2, ":", Val3].

-spec hash(binary(), binary(), binary(), binary()) -> iolist().
hash(Val1, Val2, Val3, Val4) ->
    ["s4:", Val1, ":", Val2, ":", Val3, ":", term_to_binary(Val4)].

-spec n(atom()) -> iolist().
n(Node) ->
    ["n:", atom_to_list(Node)].
