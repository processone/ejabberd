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

-export([route/3, register_route/1, register_local_route/1]).

-export([start/0, init/0]).

-include("ejabberd.hrl").

-record(route, {domain, node, pid}).
-record(local_route, {domain, pid}).


start() ->
    spawn(ejabberd_router, init, []).

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
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    % TODO
	    do_route(From, To, Packet),
	    loop();
	{register_route, Domain, Pid, Node} ->
	    F = fun() ->
			case mnesia:wread({route, Domain}) of
			    [] ->
				ok;
			    [Old] ->
				% TODO: notify
				ok
			end,
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
			case mnesia:wread({route, Domain}) of
			    [] ->
				ok;
			    [Old] ->
				% TODO: notify
				ok
			end,
			mnesia:delete({route, Domain})
		end,
	    mnesia:transaction(F),
	    loop();
	_ ->
	    loop()
    end.


do_route(From, To, Packet) ->
    ?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n", [From, To, Packet]),
    {DstNode, DstDomain, DstResourse} = To,
    F = fun() ->
		case mnesia:read({local_route, DstDomain}) of
		    [] ->
			case mnesia:read({route, DstDomain}) of
			    [] ->
				error;
			    [R] ->
				{ok, R#route.node, R#route.pid}
			end;
		    [R] ->
			{ok, node(), R#local_route.pid}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, error} ->
	    ejabberd_s2s ! {route, From, To, Packet};
	{atomic, {ok, Node, Pid}} ->
	    case node() of
		Node ->
		    ?DEBUG("routed to process ~p~n", [Pid]),
		    Pid ! {route, From, To, Packet};
		_ ->
		    ?DEBUG("routed to node ~p~n", [Node]),
		    {ejabberd_router, Node} ! {route, From, To, Packet}
	    end;
	_ ->
	    % TODO
	    error
    end.


route(From, To, Packet) ->
    ejabberd_router ! {route, From, To, Packet}.

register_route(Domain) ->
    ejabberd_router ! {register_route, Domain, self(), node()}.

register_local_route(Domain) ->
    ejabberd_router ! {register_local_route, Domain, self()}.

