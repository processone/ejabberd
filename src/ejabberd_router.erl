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

-export([route/3]).

-export([start/0, init/0]).

-include("ejabberd.hrl").

-record(route, {domain, node, pid}).


start() ->
    spawn(ejabberd_router, init, []).

init() ->
    register(ejabberd_router, self()),
    mnesia:create_table(route,
			[{ram_copies, [node()]},
			 {attributes,
			  record_info(fields, route)}]),
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
		case mnesia:read({route, DstDomain}) of
		    [] ->
			error;
		    [R] ->
			{ok, R#route.node, R#route.pid}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, error} ->
	    % TODO: start s2s instead of below
	    {xmlelement, Name, Attrs, SubTags} = Packet,
	    case xml:get_attr_s("type", Attrs) of
		"error" ->
		    ok;
		_ ->
		    Err = jlib:make_error_reply(Packet,
						502, "Service Unavailable"),
		    ejabberd_router ! {route, To, From, Err}
	    end;
	{atomic, {ok, Node, Pid}} ->
	    {Pid, Node} ! {packet, From, To, Packet};
	_ ->
	    % TODO
	    error
    end.


route(From, To, Packet) ->
    ejabberd_router ! {route, From, To, Packet}.

