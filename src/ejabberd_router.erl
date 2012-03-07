%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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
	 force_unregister_route/1,
	 unregister_routes/1,
	 dirty_get_all_routes/0,
	 dirty_get_all_domains/0,
	 read_route/1,
	 make_id/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

-record(route, {domain, pid, local_hint}).
-record(state, {}).

%% "rr" stands for Record-Route.
-define(ROUTE_PREFIX, "rr-").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% #xmlelement{} used for retro-compatibility
route(FromOld, ToOld, #xmlelement{} = PacketOld) ->
    catch throw(for_stacktrace), % To have a stacktrace.
    io:format("~nROUTER: old #xmlelement:~n~p~n~p~n~n",
      [PacketOld, erlang:get_stacktrace()]),
    % XXX OLD FORMAT: From, To, Packet.
    From = jlib:from_old_jid(FromOld),
    To = jlib:from_old_jid(ToOld),
    Packet = exmpp_xml:xmlelement_to_xmlel(PacketOld, [?NS_JABBER_CLIENT],
      [{?NS_XMPP, ?NS_XMPP_pfx}]),
    route(From, To, Packet);
route(From, To, Packet) ->
    case catch route_check_id(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end.

%% Route the error packet only if the originating packet is not an error itself.
%% RFC3920 9.3.1
route_error(From, To, ErrPacket, OrigPacket) ->
    case exmpp_stanza:is_stanza_error(OrigPacket) of
	false ->
	    route(From, To, ErrPacket);
	true ->
	    ok
    end.

register_route({global, Prefix}) ->
    ejabberd_global_router:register_route(Prefix);
register_route(Domain) when is_list(Domain) ->
    register_route(Domain, undefined).

register_route(Domain, LocalHint) when is_list(Domain) ->
    try
	LDomain = exmpp_stringprep:nameprep(Domain),
	LDomainB = list_to_binary(LDomain),
	Pid = self(),
	case get_component_number(LDomain) of
	    undefined ->
		mnesia:transaction(fun register_simple_route/3, [LDomainB, Pid, LocalHint]);
	    N ->
		mnesia:transaction(fun register_balanced_route/3, [LDomainB, Pid, N])
	end
    catch
	_ ->
	    erlang:error({invalid_domain, Domain})
    end.

register_routes(Domains) ->
    lists:foreach(fun(Domain) ->
			  register_route(Domain)
		  end, Domains).

register_simple_route(LDomain, Pid, LocalHint) ->
    mnesia:write(#route{domain = LDomain,
                        pid = Pid,
                        local_hint = LocalHint}).

register_balanced_route(LDomain, Pid, N) ->
    case mnesia:read({route, LDomain}) of
        [] ->
            mnesia:write(
              #route{domain = LDomain,
                     pid = Pid,
                     local_hint = 1}),
            lists:foreach(
              fun(I) ->
                      mnesia:write(
                        #route{domain = LDomain,
                               pid = undefined,
                               local_hint = I})
              end, lists:seq(2, N));
        Rs ->
            lists:any(
              fun(#route{pid = undefined,
                         local_hint = I} = R) ->
                      mnesia:write(
                        #route{domain = LDomain,
                               pid = Pid,
                               local_hint = I}),
                      mnesia:delete_object(R),
                      true;
                 (_) ->
                      false
              end, Rs)
    end.

unregister_route({global, Prefix}) ->
    ejabberd_global_router:unregister_route(Prefix);
unregister_route(Domain) when is_list(Domain) ->
    try
	LDomain = exmpp_stringprep:nameprep(Domain),
	LDomainB = list_to_binary(LDomain),
	Pid = self(),
	case get_component_number(LDomain) of
	    undefined ->
		    mnesia:transaction(fun delete_simple_route/2, [LDomainB, Pid]);
	    _ ->
		    mnesia:transaction(fun delete_balanced_route/2, [LDomainB, Pid])
	end
    catch
	_ ->
	    erlang:error({invalid_domain, Domain})
    end.

delete_simple_route(LDomain, Pid) ->
    case mnesia:match_object(#route{domain = LDomain,
                                    pid = Pid,
                                    _ = '_'}) of
        [R] ->
            mnesia:delete_object(R);
        _ ->
            ok
    end.

delete_balanced_route(LDomain, Pid) ->
    case mnesia:match_object(#route{domain=LDomain,
                                    pid = Pid,
                                    _ = '_'}) of
        [R] ->
            I = R#route.local_hint,
            ok = mnesia:write(
                   #route{domain = LDomain,
                          pid = undefined,
                          local_hint = I}),
            mnesia:delete_object(R);
        _ ->
            ok
    end.


force_unregister_route(Domain) when is_binary(Domain) ->
    LDomain = exmpp_stringprep:nameprep(Domain),
    F = fun() ->
		case mnesia:match_object(
		       #route{domain = LDomain,
			      _ = '_'}) of
		    Rs when is_list(Rs) ->
			lists:foreach(fun(R) ->
					      mnesia:delete_object(R)
				      end, Rs);
		    _ ->
			ok
		end
	end,
    mnesia:transaction(F).

unregister_routes(Domains) ->
    lists:foreach(fun(Domain) ->
			  unregister_route(Domain)
		  end, Domains).

read_route(Domain) ->
    [{D,P,H}
     || #route{domain=D, pid=P, local_hint=H} <- mnesia:dirty_read({route, Domain})].

dirty_get_all_routes() ->
    lists:usort(
        lists:map(fun erlang:binary_to_list/1, 
                  mnesia:dirty_all_keys(route))) -- ?MYHOSTS.

dirty_get_all_domains() ->
    lists:usort(
        lists:map(fun erlang:binary_to_list/1, 
                  mnesia:dirty_all_keys(route))).

make_id() ->
    ?ROUTE_PREFIX ++ randoms:get_string() ++ "-" ++ ejabberd_cluster:node_id().

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
			 {type, bag},
			 {attributes,
			  record_info(fields, route)}]),
    mnesia:add_table_copy(route, node(), ram_copies),
    mnesia:subscribe({table, route, simple}),
    lists:foreach(
      fun(Pid) ->
	      erlang:monitor(process, Pid)
      end,
      mnesia:dirty_select(route, [{{route, '_', '$1', '_'}, [], ['$1']}])),
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
%% #xmlelement{} used for retro-compatibility
handle_info({route, FromOld, ToOld, #xmlelement{} = PacketOld}, State) ->
    catch throw(for_stacktrace), % To have a stacktrace.
    io:format("~nROUTER: old #xmlelement:~n~p~n~p~n~n",
      [PacketOld, erlang:get_stacktrace()]),
    % XXX OLD FORMAT: From, To, Packet.
    From = jlib:from_old_jid(FromOld),
    To = jlib:from_old_jid(ToOld),
    Packet = exmpp_xml:xmlelement_to_xmlel(PacketOld, [?NS_JABBER_CLIENT],
      [{?NS_XMPP, ?NS_XMPP_pfx}]),
    handle_info({route, From, To, Packet}, State);
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({mnesia_table_event, {write, #route{pid = Pid}, _ActivityId}},
	    State) ->
    erlang:monitor(process, Pid),
    {noreply, State};
handle_info({'DOWN', _Ref, _Type, Pid, _Info}, State) ->
    F = fun() ->
		Es = mnesia:select(
		       route,
		       [{#route{pid = Pid, _ = '_'},
			 [],
			 ['$_']}]),
		lists:foreach(
		  fun(E) ->
			  if
			      is_integer(E#route.local_hint) ->
				  LDomain = E#route.domain,
				  I = E#route.local_hint,
				  mnesia:write(
				    #route{domain = LDomain,
					   pid = undefined,
					   local_hint = I}),
				  mnesia:delete_object(E);
			      true ->
				  mnesia:delete_object(E)
			  end
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
route_check_id(From, To, #xmlel{name = iq} = Packet) ->
    case exmpp_xml:get_attribute_as_list(Packet, <<"id">>, "") of
	?ROUTE_PREFIX ++ Rest ->
	    Type = exmpp_xml:get_attribute_as_list(Packet, <<"type">>, ""),
	    if Type == "error"; Type == "result" ->
		    case string:tokens(Rest, "-") of
			[_, NodeID] ->
			    case ejabberd_cluster:get_node_by_id(NodeID) of
				Node when Node == node() ->
				    do_route(From, To, Packet);
				Node ->
				    {ejabberd_router, Node} !
					{route, From, To, Packet}
			    end;
			_ ->
			    do_route(From, To, Packet)
		    end;
	       true ->
		    do_route(From, To, Packet)
	    end;
	_ ->
	    do_route(From, To, Packet)
    end;
route_check_id(From, To, Packet) ->
    do_route(From, To, Packet).

do_route(OrigFrom, OrigTo, OrigPacket) ->
    ?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n",
	   [OrigFrom, OrigTo, OrigPacket]),
    case ejabberd_hooks:run_fold(filter_packet,
				 {OrigFrom, OrigTo, OrigPacket}, []) of
	{From, To, Packet} ->
	    LDstDomain = exmpp_jid:prep_domain_as_list(To),
	    Destination = ejabberd:normalize_host(LDstDomain),
	    case mnesia:dirty_read(route, list_to_binary(Destination)) of
		[] ->
		    case ejabberd_global_router:find_route(Destination) of
			no_route ->
			    ejabberd_s2s:route(From, To, Packet);
			Route ->
			    ejabberd_global_router:route(Route, From, To, Packet)
		    end;
		[R] ->
		    Pid = R#route.pid,
		    if
			node(Pid) == node() ->
			    case R#route.local_hint of
				{apply, Module, Function} ->
				    Module:Function(From, To, Packet);
				_ ->
				    Pid ! {route, From, To, Packet}
			    end;
			is_pid(Pid) ->
			    Pid ! {route, From, To, Packet};
			true ->
			    drop
		    end;
		Rs ->
		    Value = case ejabberd_config:get_local_option
                                ({domain_balancing, LDstDomain}) of
				undefined -> now();
				random -> now();
				source -> jlib:short_prepd_jid(From);
				destination -> jlib:short_prepd_jid(To);
				bare_source ->
				    jlib:short_prepd_bare_jid(From);
				bare_destination ->
				    jlib:short_prepd_bare_jid(To)
			    end,
		    case get_component_number(LDstDomain) of
			undefined ->
			    case [R || R <- Rs, node(R#route.pid) == node()] of
				[] ->
				    R = lists:nth(erlang:phash(Value, length(Rs)), Rs),
				    Pid = R#route.pid,
				    if
					is_pid(Pid) ->
					    Pid ! {route, From, To, Packet};
					true ->
					    drop
				    end;
				LRs ->
				    R = lists:nth(erlang:phash(Value, length(LRs)), LRs),
				    Pid = R#route.pid,
				    case R#route.local_hint of
					{apply, Module, Function} ->
					    Module:Function(From, To, Packet);
					_ ->
					    Pid ! {route, From, To, Packet}
				    end
			    end;
			_ ->
			    SRs = lists:ukeysort(#route.local_hint, Rs),
			    R = lists:nth(erlang:phash(Value, length(SRs)), SRs),
			    Pid = R#route.pid,
			    if
				is_pid(Pid) ->
				    Pid ! {route, From, To, Packet};
				true ->
				    drop
			    end
		    end
	    end;
	drop ->
	    ok
    end.

get_component_number(LDomain) ->
    case ejabberd_config:get_local_option
        ({domain_balancing_component_number, LDomain}) of
	N when is_integer(N),
	       N > 1 ->
	    N;
	_ ->
	    undefined
    end.

update_tables() ->
    case catch mnesia:table_info(route, attributes) of
	[domain, node, pid] ->
	    mnesia:delete_table(route);
	[domain, pid] ->
	    mnesia:delete_table(route);
	[domain, pid, local_hint] ->
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
