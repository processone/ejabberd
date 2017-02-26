%%%----------------------------------------------------------------------
%%% File    : ejabberd_router_multicast.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Multicast router
%%% Created : 11 Aug 2007 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(ejabberd_router_multicast).
-author('alexey@process-one.net').
-author('badlop@process-one.net').

-behaviour(gen_server).

%% API
-export([route_multicast/4,
	 register_route/1,
	 unregister_route/1
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-record(route_multicast, {domain = <<"">> :: binary() | '_',
			  pid = self() :: pid()}).
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

-spec route_multicast(jid(), binary(), [jid()], stanza()) -> ok.
route_multicast(From, Domain, Destinations, Packet) ->
    case catch do_route(Domain, Destinations, xmpp:set_from(Packet, From)) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, Domain, Destinations, Packet}]);
	_ ->
	    ok
    end.

-spec register_route(binary()) -> any().
register_route(Domain) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    Pid = self(),
	    F = fun() ->
			mnesia:write(#route_multicast{domain = LDomain,
						      pid = Pid})
		end,
	    mnesia:transaction(F)
    end.

-spec unregister_route(binary()) -> any().
unregister_route(Domain) ->
    case jid:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    Pid = self(),
	    F = fun() ->
		    case mnesia:select(route_multicast,
		       [{#route_multicast{pid = Pid, domain = LDomain, _ = '_'},
			 [],
			 ['$_']}]) of
			    [R] -> mnesia:delete_object(R);
			    _ -> ok
		    end
		end,
	    mnesia:transaction(F)
    end.


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
    ejabberd_mnesia:create(?MODULE, route_multicast,
			[{ram_copies, [node()]},
			 {type, bag},
			 {attributes,
			  record_info(fields, route_multicast)}]),
    mnesia:add_table_copy(route_multicast, node(), ram_copies),
    mnesia:subscribe({table, route_multicast, simple}),
    lists:foreach(
      fun(Pid) ->
	      erlang:monitor(process, Pid)
      end,
      mnesia:dirty_select(route_multicast, [{{route_multicast, '_', '$1'}, [], ['$1']}])),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
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
handle_info({route_multicast, Domain, Destinations, Packet}, State) ->
    case catch do_route(Domain, Destinations, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {Domain, Destinations, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({mnesia_table_event, {write, #route_multicast{pid = Pid}, _ActivityId}},
	    State) ->
    erlang:monitor(process, Pid),
    {noreply, State};
handle_info({'DOWN', _Ref, _Type, Pid, _Info}, State) ->
    F = fun() ->
		Es = mnesia:select(
		       route_multicast,
		       [{#route_multicast{pid = Pid, _ = '_'},
			 [],
			 ['$_']}]),
		lists:foreach(
		  fun(E) ->
			  mnesia:delete_object(E)
		  end, Es)
	end,
    mnesia:transaction(F),
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
%% From = #jid
%% Destinations = [#jid]
-spec do_route(binary(), [jid()], stanza()) -> any().
do_route(Domain, Destinations, Packet) ->
    ?DEBUG("route multicast:~n~s~nDomain: ~s~nDestinations: ~s~n",
	   [xmpp:pp(Packet), Domain,
	    str:join([jid:encode(To) || To <- Destinations], <<", ">>)]),
    %% Try to find an appropriate multicast service
    case mnesia:dirty_read(route_multicast, Domain) of

	%% If no multicast service is available in this server, send manually
	[] -> do_route_normal(Destinations, Packet);

	%% If some is available, send the packet using multicast service
	Rs when is_list(Rs) ->
	    Pid = pick_multicast_pid(Rs),
	    Pid ! {route_trusted, Destinations, Packet}
    end.

-spec pick_multicast_pid([#route_multicast{}]) -> pid().
pick_multicast_pid(Rs) ->
    List = case [R || R <- Rs, node(R#route_multicast.pid) == node()] of
	[] -> Rs;
	RLocals -> RLocals
    end,
    (hd(List))#route_multicast.pid.

-spec do_route_normal([jid()], stanza()) -> any().
do_route_normal(Destinations, Packet) ->
    [ejabberd_router:route(xmpp:set_to(Packet, To)) || To <- Destinations].
