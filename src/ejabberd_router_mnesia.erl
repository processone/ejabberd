%%%-------------------------------------------------------------------
%%% Created : 11 Jan 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_router_mnesia).
-behaviour(ejabberd_router).
-behaviour(gen_server).

%% API
-export([init/0,
         register_route/5,
         unregister_route/3,
         find_routes/1,
         get_all_routes/0,
         use_cache/0]).
%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3,
         start_link/0]).

-include("ejabberd_router.hrl").
-include("logger.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {}).


%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    Spec = {?MODULE, {?MODULE, start_link, []},
                     transient,
                     5000,
                     worker,
                     [?MODULE]},
    case supervisor:start_child(ejabberd_backend_sup, Spec) of
        {ok, _Pid} -> ok;
        Err -> Err
    end.


-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


use_cache() ->
    false.


register_route(Domain, ServerHost, LocalHint, undefined, Pid) ->
    F = fun() ->
                mnesia:write(#route{
                               domain = Domain,
                               pid = Pid,
                               server_host = ServerHost,
                               local_hint = LocalHint
                              })
        end,
    transaction(F);
register_route(Domain, ServerHost, _LocalHint, N, Pid) ->
    F = fun() ->
                case mnesia:wread({route, Domain}) of
                    [] ->
                        mnesia:write(#route{
                                       domain = Domain,
                                       server_host = ServerHost,
                                       pid = Pid,
                                       local_hint = 1
                                      }),
                        lists:foreach(
                          fun(I) ->
                                  mnesia:write(
                                    #route{
                                      domain = Domain,
                                      pid = undefined,
                                      server_host = ServerHost,
                                      local_hint = I
                                     })
                          end,
                          lists:seq(2, N));
                    Rs ->
                        lists:any(
                          fun(#route{
                                pid = undefined,
                                local_hint = I
                               } = R) ->
                                  mnesia:write(
                                    #route{
                                      domain = Domain,
                                      pid = Pid,
                                      server_host = ServerHost,
                                      local_hint = I
                                     }),
                                  mnesia:delete_object(R),
                                  true;
                             (_) -> false
                          end,
                          Rs)
                end
        end,
    transaction(F).


unregister_route(Domain, undefined, Pid) ->
    F = fun() ->
                case mnesia:select(
                       route,
                       ets:fun2ms(
                         fun(#route{domain = D, pid = P} = R)
                               when D == Domain, P == Pid -> R
                         end)) of
                    [R] -> mnesia:delete_object(R);
                    _ -> ok
                end
        end,
    transaction(F);
unregister_route(Domain, _, Pid) ->
    F = fun() ->
                case mnesia:select(
                       route,
                       ets:fun2ms(
                         fun(#route{domain = D, pid = P} = R)
                               when D == Domain, P == Pid -> R
                         end)) of
                    [R] ->
                        I = R#route.local_hint,
                        ServerHost = R#route.server_host,
                        mnesia:write(#route{
                                       domain = Domain,
                                       server_host = ServerHost,
                                       pid = undefined,
                                       local_hint = I
                                      }),
                        mnesia:delete_object(R);
                    _ -> ok
                end
        end,
    transaction(F).


find_routes(Domain) ->
    {ok, mnesia:dirty_read(route, Domain)}.


get_all_routes() ->
    {ok, mnesia:dirty_select(
           route,
           ets:fun2ms(
             fun(#route{domain = Domain, server_host = ServerHost})
                   when Domain /= ServerHost -> Domain
             end))}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    update_tables(),
    ejabberd_mnesia:create(?MODULE,
                           route,
                           [{ram_copies, [node()]},
                            {type, bag},
                            {attributes, record_info(fields, route)}]),
    mnesia:subscribe({table, route, simple}),
    lists:foreach(
      fun(Pid) -> erlang:monitor(process, Pid) end,
      mnesia:dirty_select(
        route,
        ets:fun2ms(
          fun(#route{pid = Pid}) -> Pid end))),
    {ok, #state{}}.


handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.


handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.


handle_info({mnesia_table_event,
             {write, #route{pid = Pid}, _ActivityId}},
            State) ->
    erlang:monitor(process, Pid),
    {noreply, State};
handle_info({mnesia_table_event, _}, State) ->
    {noreply, State};
handle_info({'DOWN', _Ref, _Type, Pid, _Info}, State) ->
    F = fun() ->
                Es = mnesia:select(
                       route,
                       ets:fun2ms(
                         fun(#route{pid = P} = E)
                               when P == Pid -> E
                         end)),
                lists:foreach(
                  fun(E) ->
                          if
                              is_integer(E#route.local_hint) ->
                                  LDomain = E#route.domain,
                                  I = E#route.local_hint,
                                  ServerHost = E#route.server_host,
                                  mnesia:write(#route{
                                                 domain = LDomain,
                                                 server_host = ServerHost,
                                                 pid = undefined,
                                                 local_hint = I
                                                }),
                                  mnesia:delete_object(E);
                              true ->
                                  mnesia:delete_object(E)
                          end
                  end,
                  Es)
        end,
    transaction(F),
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, _} ->
            ok;
        {aborted, Reason} ->
            ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
            {error, db_failure}
    end.


-spec update_tables() -> ok.
update_tables() ->
    try
        mnesia:transform_table(route, ignore, record_info(fields, route))
    catch
        exit:{aborted, {no_exists, _}} ->
            ok
    end,
    case lists:member(local_route, mnesia:system_info(tables)) of
        true -> mnesia:delete_table(local_route);
        false -> ok
    end.
