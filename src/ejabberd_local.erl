%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_local).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%%-export([Function/Arity, ...]).

-export([start/0,init/0]).

-include("ejabberd.hrl").


start() ->
    spawn(ejabberd_local, init, []).

init() ->
    ejabberd_router:register_local_route("localhost"),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    do_route(From, To, Packet),
	    loop()
    end.


do_route(From, To, Packet) ->
    ?DEBUG("local route~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    case To of
	{"", _, _} ->
	    ok;
	_ ->
	    ejabberd_sm ! {route, From, To, Packet}
    end,
    ok.

