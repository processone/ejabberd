%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_router).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([route/3,
	 register_route/1,
	 unregister_route/1,
	 dirty_get_all_routes/0,
	 dirty_get_all_domains/0
	]).

-export([start_link/0, init/0]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(route, {domain, pid}).


start_link() ->
    Pid = proc_lib:spawn_link(ejabberd_router, init, []),
    register(ejabberd_router, Pid),
    {ok, Pid}.

init() ->
    update_tables(),
    mnesia:create_table(route,
			[{ram_copies, [node()]},
			 {type, bag},
			 {attributes,
			  record_info(fields, route)}]),
    mnesia:add_table_copy(route, node(), ram_copies),
    mnesia:subscribe({table, route, simple}),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p~nwhen processing: ~p",
			       [Reason, {From, To, Packet}]);
		_ ->
		    ok
	    end,
	    loop();
	{register_route, Domain, Pid} ->
	    F = fun() ->
			mnesia:write(#route{domain = Domain,
					    pid = Pid})
		end,
	    mnesia:transaction(F),
	    loop();
	{unregister_route, Domain, Pid} ->
	    F = fun() ->
			mnesia:delete_object(#route{domain = Domain,
						    pid = Pid})
		end,
	    mnesia:transaction(F),
	    loop();
	{mnesia_table_event, {write, #route{pid = Pid}, _ActivityId}} ->
	    erlang:monitor(process, Pid),
	    loop();
	{'DOWN', _Ref, _Type, Pid, _Info} ->
	    F = fun() ->
	    		Es = mnesia:select(
			       route,
			       [{#route{pid = Pid, _ = '_'},
				 [],
				 ['$_']}]),
			lists:foreach(fun(E) ->
					      mnesia:delete_object(E)
				      end, Es)
		end,
	    mnesia:transaction(F),
	    loop();
	_ ->
	    loop()
    end.

do_route(From, To, Packet) ->
    ?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n", [From, To, Packet]),
    LDstDomain = To#jid.lserver,
    case mnesia:dirty_read(route, LDstDomain) of
	[] ->
	    ejabberd_s2s ! {route, From, To, Packet};
	[R] ->
	    Pid = R#route.pid,
	    ?DEBUG("routed to process ~p~n", [Pid]),
	    Pid ! {route, From, To, Packet};
	Rs ->
	    Rs1 = case [R || R <- Rs, node(R#route.pid) == node()] of
		      [] -> Rs;
		      LRs -> LRs
		  end,
	    R = lists:nth(erlang:phash(now(), length(Rs1)), Rs1),
	    Pid = R#route.pid,
	    ?DEBUG("routed to process ~p~n", [Pid]),
	    Pid ! {route, From, To, Packet}
    end.


route(From, To, Packet) ->
    ejabberd_router ! {route, From, To, Packet}.

register_route(Domain) ->
    ejabberd_router ! {register_route, Domain, self()}.

unregister_route(Domain) ->
    ejabberd_router ! {unregister_route, Domain, self()}.


dirty_get_all_routes() ->
    lists:delete(?MYNAME, lists:usort(mnesia:dirty_all_keys(route))).

dirty_get_all_domains() ->
    lists:usort(mnesia:dirty_all_keys(route)).



update_tables() ->
    case catch mnesia:table_info(route, attributes) of
	[domain, node, pid] ->
	    mnesia:delete_table(route);
	[domain, pid] ->
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

