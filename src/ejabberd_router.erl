%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_router).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([route/3,
	 register_route/1,
	 register_local_route/1,
	 unregister_route/1,
	 unregister_local_route/1,
	 dirty_get_all_routes/0,
	 dirty_get_all_domains/0
	]).

-export([start_link/0, init/0]).

-include("ejabberd.hrl").

-record(route, {domain, node, pid}).
-record(local_route, {domain, pid}).


start_link() ->
    {ok, proc_lib:spawn_link(ejabberd_router, init, [])}.

init() ->
    register(ejabberd_router, self()),
    mnesia:create_table(route,
			[{ram_copies, [node()]},
			 {attributes,
			  record_info(fields, route)}]),
    mnesia:create_table(local_route,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes,
			  record_info(fields, local_route)}]),
    mnesia:add_table_copy(local_route, node(), ram_copies),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop();
	{register_route, Domain, Pid, Node} ->
	    F = fun() ->
			%case mnesia:wread({route, Domain}) of
			%    [] ->
			%	ok;
			%    [Old] ->
			%	% TODO: notify
			%	ok
			%end,
			mnesia:write(#route{domain = Domain,
					    node = Node,
					    pid = Pid})
		end,
	    mnesia:transaction(F),
	    loop();
	{register_local_route, Domain, Pid} ->
	    F = fun() ->
			mnesia:write(#local_route{domain = Domain,
						  pid = Pid})
		end,
	    mnesia:transaction(F),
	    loop();
	{unregister_route, Domain} ->
	    F = fun() ->
			%case mnesia:wread({route, Domain}) of
			%    [] ->
			%	ok;
			%    [Old] ->
			%	% TODO: notify
			%	ok
			%end,
			mnesia:delete({route, Domain})
		end,
	    mnesia:transaction(F),
	    loop();
	{unregister_local_route, Domain} ->
	    F = fun() ->
			mnesia:delete({local_route, Domain})
		end,
	    mnesia:transaction(F),
	    loop();
	_ ->
	    loop()
    end.

do_route(From, To, Packet) ->
    ?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n", [From, To, Packet]),
    {DstNode, DstDomain, DstResourse} = To,
    LDstDomain = jlib:tolower(DstDomain),
    case mnesia:dirty_read({local_route, LDstDomain}) of
	[] ->
	    case mnesia:dirty_read({route, LDstDomain}) of
		[] ->
		    ejabberd_s2s ! {route, From, To, Packet};
		[R] ->
		    Node = R#route.node,
		    case node() of
			Node ->
			    Pid = R#local_route.pid,
			    ?DEBUG("routed to process ~p~n", [Pid]),
			    Pid ! {route, From, To, Packet};
			_ ->
			    ?DEBUG("routed to node ~p~n", [Node]),
			    {ejabberd_router, Node} ! {route, From, To, Packet}
		    end
	    end;
	[R] ->
	    Pid = R#local_route.pid,
	    ?DEBUG("routed to process ~p~n", [Pid]),
	    Pid ! {route, From, To, Packet}
    end.


route(From, To, Packet) ->
    ejabberd_router ! {route, From, To, Packet}.

register_route(Domain) ->
    ejabberd_router ! {register_route, Domain, self(), node()}.

register_local_route(Domain) ->
    ejabberd_router ! {register_local_route, Domain, self()}.

unregister_route(Domain) ->
    ejabberd_router ! {unregister_route, Domain}.

unregister_local_route(Domain) ->
    ejabberd_router ! {unregister_local_route, Domain}.


dirty_get_all_routes() ->
    lists:delete(?MYNAME,
		 lists:umerge(lists:sort(mnesia:dirty_all_keys(route)),
			      lists:sort(mnesia:dirty_all_keys(local_route)))).

dirty_get_all_domains() ->
    lists:umerge(lists:sort(mnesia:dirty_all_keys(route)),
		 lists:sort(mnesia:dirty_all_keys(local_route))).

