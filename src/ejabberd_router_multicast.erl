%%%----------------------------------------------------------------------
%%% File    : ejabberd_router_multicast.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Multicast router
%%% Created : 11 Aug 2007 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
-export([route_multicast/5,
	 register_route/1,
	 unregister_route/1
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, update_to_in_wrapped/2]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

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

-spec route_multicast(jid(), binary(), [jid()], stanza(), boolean()) -> ok.
route_multicast(From0, Domain0, Destinations0, Packet0, Wrapped0) ->
    {From, Domain, Destinations, Packet, Wrapped} =
    ejabberd_hooks:run_fold(multicast_route, Domain0, {From0, Domain0, Destinations0, Packet0, Wrapped0}, []),
    case catch do_route(Domain, Destinations, xmpp:set_from(Packet, From), Wrapped) of
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
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route_multicast, Domain, Destinations, Packet}, State) ->
    case catch do_route(Domain, Destinations, Packet, false) of
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
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
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

-spec update_to_in_wrapped(stanza(), jid()) -> stanza().
update_to_in_wrapped(Packet, To) ->
    case Packet of
	#message{sub_els = [#ps_event{
	    items = #ps_items{
		items = [#ps_item{
		    sub_els = [Internal]
		} = PSItem]
	    } = PSItems
	} = PSEvent]} ->
	    Internal2 = xmpp:set_to(Internal, To),
	    PSItem2 = PSItem#ps_item{sub_els = [Internal2]},
	    PSItems2 = PSItems#ps_items{items = [PSItem2]},
	    PSEvent2 = PSEvent#ps_event{items = PSItems2},
	    xmpp:set_to(Packet#message{sub_els = [PSEvent2]}, To);
	_ ->
	    xmpp:set_to(Packet, To)
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% From = #jid
%% Destinations = [#jid]
-spec do_route(binary(), [jid()], stanza(), boolean()) -> any().
do_route(Domain, Destinations, Packet, true) ->
    ?DEBUG("Route multicast:~n~ts~nDomain: ~ts~nDestinations: ~ts~n",
	   [xmpp:pp(Packet), Domain,
	    str:join([jid:encode(To) || To <- Destinations], <<", ">>)]),
    lists:foreach(
	fun(To) ->
	    Packet2 = update_to_in_wrapped(Packet, To),
	    ejabberd_router:route(Packet2)
	end, Destinations);
do_route(Domain, Destinations, Packet, false) ->
    ?DEBUG("Route multicast:~n~ts~nDomain: ~ts~nDestinations: ~ts~n",
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
    lists:foreach(
	fun(To) ->
	    ejabberd_router:route(xmpp:set_to(Packet, To))
	end, Destinations).
