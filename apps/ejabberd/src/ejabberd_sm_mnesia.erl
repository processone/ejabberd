%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Implementation of Mnesia-based session manager
%%%
%%% @end
%%% Created : 17 Nov 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_mnesia).

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
start(_Opts) ->
    mnesia:create_table(session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies).

-spec get_sessions() -> list(list(term())).
get_sessions() ->
    mnesia:activity(transaction,
        fun() ->
            mnesia:foldl(fun(#session{ usr = Usr, sid = Sid, priority = Pri, info = Inf}, AccIn) ->
                    [{Usr, Sid, Pri, Inf}|AccIn]
                end,
                [],
                session)
        end).

-spec get_sessions(binary()) -> list(tuple()).
get_sessions(Server) ->
    mnesia:dirty_select(
        session,
        [{#session{usr = '$1', _ = '_' },
          [{'==', {element, 2, '$1'}, Server}],
          ['$1']}]).

-spec get_sessions(binary(), binary()) -> list(#session{}).
get_sessions(User, Server) ->
    mnesia:dirty_index_read(session, {User, Server}, #session.us).

-spec get_sessions(binary(), binary(), binary()) -> list(#session{}).
get_sessions(User, Server, Resource) ->
    mnesia:dirty_index_read(session, {User, Server, Resource}, #session.usr).

-spec create_session(binary(), binary(), binary(), #session{}) -> ok | {error, term()}.
create_session(_User, _Server, _Resource, Session) ->
    mnesia:sync_dirty(fun() ->
                              mnesia:write(Session)
                      end).

-spec delete_session(tuple(), binary(), binary(), binary()) -> ok.
delete_session(SID, _User, _Server, _Resource) ->
    mnesia:sync_dirty(fun() ->
                              mnesia:delete({session, SID})
                      end).

-spec cleanup(atom()) -> any().
cleanup(Node) ->
    F = fun() ->
                Es = mnesia:select(
                       session,
                       [{#session{sid = {'_', '$1'}, _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete({session, E#session.sid})
                              end, Es)

        end,
    mnesia:async_dirty(F).

-spec total_count() -> integer().
total_count() ->
    mnesia:table_info(session, size).

-spec unique_count() -> integer().
unique_count() ->
    compute_unique(mnesia:dirty_first(session),
                   sets:new()).

-spec compute_unique(term(), set()) -> integer().
compute_unique('$end_of_table', Set) ->
    sets:size(Set);
compute_unique(Key, Set) ->
    NewSet = case mnesia:dirty_read(session, Key) of
                 [Session] ->
                     sets:add_element(Session#session.us, Set);
                 _ ->
                     Set
             end,
    compute_unique(mnesia:dirty_next(session, Key), NewSet).
