%%%----------------------------------------------------------------------
%%% File    : ejabberd_listener.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_listener).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, init/0]).

start() ->
    register(ejabberd_listener, spawn(?MODULE, init, [])).

init() ->
    {ok, ListenSocket} = gen_tcp:listen(5522, [binary,
					       {packet, 0}, 
					       {active, false},
					       {reuseaddr, true}]),
    accept(ListenSocket).

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok,Socket} ->
	    ejabberd_c2s:start(Socket),
	    accept(ListenSocket)
    end.



do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
	    do_recv(Sock, [Bs, B]);
        {error, closed} ->
	    {ok, list_to_binary(Bs)}
    end.


