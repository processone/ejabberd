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
         get_sessions/2,
         get_session/3,
         set_session/4,
         create_session/4,
         delete_session/4,
         cleanup/1,
         count/0]).

-spec start(list()) -> any().
start(_Opts) ->
    mnesia:create_table(session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies).

-spec get_sessions(binary(), binary()) -> list(#session{}).
get_sessions(User, Server) ->
    mnesia:dirty_index_read(session, {User, Server}, #session.us).

-spec get_session(binary(), binary(), binary()) -> #session{}.
get_session(User, Server, Resource) ->
    mnesia:dirty_index_read(session, {User, Server, Resource}, #session.us).

-spec set_session(binary(), binary(), binary(), #session{}) -> ok | {error, term()}.
set_session(_User, _Server, _Resource, Session) ->
    mnesia:sync_dirty(fun() ->
                              mnesia:write(Session)
                      end).

-spec create_session(binary(), binary(), binary(), #session{}) -> ok | {error, term()}.
create_session(User, Server, Resource, Session) ->
    set_session(User, Server, Resource, Session).

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

-spec count() -> integer().
count() ->
    mnesia:table_info(session, size).
