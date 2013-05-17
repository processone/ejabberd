%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_router).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([route/3,
         route_error/4,
         register_route/1,
         register_route/2,
         register_routes/1,
         unregister_route/1,
         unregister_routes/1,
         dirty_get_all_routes/0,
         dirty_get_all_domains/0
        ]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(route, {domain, handler}).
-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nwhen processing: ~p",
                       [Reason, {From, To, Packet}]);
        _ ->
            ok
    end.

%% Route the error packet only if the originating packet is not an error itself.
%% RFC3920 9.3.1
route_error(From, To, ErrPacket, OrigPacket) ->
    #xmlel{attrs = Attrs} = OrigPacket,
    case <<"error">> == xml:get_attr_s(<<"type">>, Attrs) of
        false ->
            route(From, To, ErrPacket);
        true ->
            ok
    end.

register_route(Domain) ->
    register_route(Domain, undefined).

register_route(Domain, Handler) ->
    register_route_to_ldomain(jlib:nameprep(Domain), Domain, Handler).

register_routes(Domains) ->
    lists:foreach(fun(Domain) ->
                      register_route(Domain)
                  end,
                  Domains).

register_route_to_ldomain(error, Domain, _) ->
    erlang:error({invalid_domain, Domain});
register_route_to_ldomain(LDomain, _, HandlerOrUndef) ->
    Handler = make_handler(HandlerOrUndef),
    mnesia:dirty_write(#route{domain = LDomain, handler = Handler}).

make_handler(undefined) ->
    Pid = self(),
    {apply_fun, fun(From, To, Packet) ->
                    Pid ! {route, From, To, Packet}
                end};
make_handler({apply_fun, Fun} = Handler) when is_function(Fun, 3) ->
    Handler;
make_handler({apply, Module, Function} = Handler)
    when is_atom(Module),
         is_atom(Function) ->
    Handler.

unregister_route(Domain) ->
    case jlib:nameprep(Domain) of
        error ->
            erlang:error({invalid_domain, Domain});
        LDomain ->
            mnesia:dirty_delete(route, LDomain)
    end.

unregister_routes(Domains) ->
    lists:foreach(fun(Domain) ->
                      unregister_route(Domain)
                  end,
                  Domains).


dirty_get_all_routes() ->
    lists:usort(mnesia:dirty_all_keys(route)) -- ?MYHOSTS.

dirty_get_all_domains() ->
    lists:usort(mnesia:dirty_all_keys(route)).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    update_tables(),
    mnesia:create_table(route,
                        [{ram_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, route)},
                         {local_content, true}]),
    mnesia:add_table_copy(route, node(), ram_copies),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State)
%%              -> {reply, Reply, State} |
%%                 {reply, Reply, State, Timeout} |
%%                 {noreply, State} |
%%                 {noreply, State, Timeout} |
%%                 {stop, Reason, Reply, State} |
%%                 {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nwhen processing: ~p",
                       [Reason, {From, To, Packet}]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_route(OrigFrom, OrigTo, OrigPacket) ->
    ?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n",
           [OrigFrom, OrigTo, OrigPacket]),
    case ejabberd_hooks:run_fold(filter_packet,
                                 {OrigFrom, OrigTo, OrigPacket}, []) of
        {From, To, Packet} ->
            LDstDomain = To#jid.lserver,
            case mnesia:dirty_read(route, LDstDomain) of
                [] ->
                    ejabberd_s2s:route(From, To, Packet);
                [#route{handler=Handler}] ->
                    case Handler of
                        {apply_fun, Fun} ->
                            Fun(From, To, Packet);
                        {apply, Module, Function} ->
                            Module:Function(From, To, Packet)
                    end
            end;
        drop ->
            ejabberd_hooks:run(xmpp_stanza_dropped, 
                               OrigFrom#jid.lserver,
                               [OrigFrom, OrigTo, OrigPacket]),
            ok
    end.

update_tables() ->
    case catch mnesia:table_info(route, attributes) of
        [domain, node, pid] ->
            mnesia:delete_table(route);
        [domain, pid] ->
            mnesia:delete_table(route);
        [domain, pid, local_hint] ->
            mnesia:delete_table(route);
        [domain, handler] ->
            ok;
        {'EXIT', _} ->
            ok
    end,
    case lists:member(local_route, mnesia:system_info(tables)) of
        true ->
            mnesia:delete_table(local_route);
        false ->
            ok
    end.

