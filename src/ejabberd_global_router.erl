%%%----------------------------------------------------------------------
%%% File    : ejabberd_global_router.erl
%%% Author  : Geoff Cant <gcant@process-one.net>
%%% Purpose : Router for virtual global routes
%%% Created : 07 Aug 2002 by Geoff Cant <gcant@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
-module(ejabberd_global_router).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("ejabberd_global_router.hrl").

%% API
-export([start_link/0
         ,route/3
         ,route/4
         ,find_route/1
         ,register_route/1
         ,register_route/2
         ,unregister_route/1
         ,unregister_route/2
         ,expand_routes/1
         ,all_prefixes/0
         ,server_host/2
        ]).

%% Eunit test exports.
-export([remove_prefix/2
         ,find_prefix/2
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

route(From, To, Packet) ->
    case find_route(To) of
        Route = #global_route{} ->
            route(Route, From, To, Packet);
        no_route ->
            ok
    end.

route(#global_route{pids=Pids}, From, To, Packet) ->
    Pid = pick_route(Pids),
    Pid ! {route, From, To, Packet},
    ok.

find_route(Domain) ->
    case prefix_match(Domain, all_prefixes()) of
        nomatch ->
            no_route;
        Prefix when is_list(Prefix) ->
             dirty_get_route(Prefix)
    end.

register_route(Prefix) ->
    register_route(Prefix, self()).

register_route(Prefix, Pid) ->
    Route = #global_route{prefix=Prefix,
                          pids=[Pid]},
    {atomic, ok} = mnesia:transaction(fun write_route/1,
                                      [Route]),
    ok.

unregister_route(Prefix) ->
    unregister_route(Prefix, self()).

unregister_route(Prefix, Pid) ->
    mnesia:transaction(fun delete_route/2, [Prefix, Pid]).

expand_routes(Domain) ->
    [expand_route(Prefix, Domain) || Prefix <- all_prefixes()].

expand_route(Prefix, Domain) ->
    lists:append([Prefix, ".", Domain]).

all_prefixes() ->
    mnesia:dirty_all_keys(global_route).

server_host(Host, Pid) ->
    #global_route{prefix=P,pids=Pids} = find_route(Host),
    true = lists:member(Pid, Pids),
    remove_prefix(P, Host).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initialises the server's state
%% @end
%%--------------------------------------------------------------------
init([]) ->
    update_tables(),
    mnesia:create_table(global_route,
			[{ram_copies, [node()]},
			 {type, set},
			 {attributes,
			  record_info(fields, global_route)}]),
    mnesia:add_table_copy(global_route, node(), ram_copies),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @spec
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Call message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_call(Call, _From, State) ->
    ?WARNING_MSG("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Cast message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Non gen-server message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

pick_route(Pids) when is_list(Pids) ->
    pick_local_route(Pids).

pick_local_route(Pids) when is_list(Pids) ->
    case [Pid || Pid <- Pids,
                 node(Pid) =:= node()] of
        [] -> pick_remote_route(Pids);
        LocalPids -> pick_one_route(LocalPids)
    end.

pick_remote_route(Pids) when is_list(Pids) -> pick_one_route(Pids).

pick_one_route(Pids) when is_list(Pids) ->
    hd(Pids). % should pick random pid?


update_tables() ->
    CorrectAttributes = record_info(fields, global_route),
    case catch mnesia:table_info(global_route, attributes) of
        CorrectAttributes ->
            ok;
        IncorrectAttributes when is_list(IncorrectAttributes) ->
	    mnesia:delete_table(global_route);
	{'EXIT', _} ->
	    ok
    end.

write_route(R = #global_route{prefix=Prefix}) ->
    NewRoute = case mnesia:read({global_route, Prefix}) of
                   [] -> R;
                   [OldRoute] ->
                       combine_routes(OldRoute, R)
               end,
    ok = mnesia:write(NewRoute).

delete_route(Prefix, Pid) ->
    case mnesia:read({global_route, Prefix}, write) of
        [] -> ok;
        [#global_route{pids=[Pid]}] ->
            mnesia:delete({global_route, Prefix});
        [Route = #global_route{pids=Pids}] ->
            mnesia:write(Route#global_route{pids=Pids -- [Pid]})
    end.

combine_routes(R = #global_route{prefix=P, pids=APids},
               #global_route{prefix=P, pids=BPids}) ->
    R#global_route{pids=lists:usort(APids ++ BPids)}.

prefix_match(Domain, Prefixes) ->
    case find_prefix(Domain, Prefixes) of
        [] -> nomatch;
        [Prefix] -> Prefix;
        _Prefixes ->
            erlang:error(multiple_prefix_matches)
    end.

dirty_get_route(Prefix) ->
    case mnesia:dirty_read({global_route, Prefix}) of
        [Route = #global_route{}] ->
            Route;
        _ -> no_route
    end.

find_prefix(Domain, PossiblePrefixes) ->
    [Prefix
     || Prefix <- PossiblePrefixes,
        lists:prefix(Prefix, Domain)].

remove_prefix(Prefix, Domain) ->
    [$. | BareDomain] = Domain -- Prefix,
    BareDomain.

%%--------------------------------------------------------------------
%%% Eunit Tests.
%%--------------------------------------------------------------------
