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

-export([start/0, init/2]).

start() ->
    register(ejabberd_listener_c2s,
	     spawn(?MODULE, init, [5522, ejabberd_c2s])),
    register(ejabberd_listener_s2s,
	     spawn(?MODULE, init, [5269, ejabberd_s2s_in])).

init(Port, CallbackModule) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary,
					       {packet, 0}, 
					       {active, false},
					       {reuseaddr, true}]),
    accept(ListenSocket, CallbackModule).

accept(ListenSocket, CallbackModule) ->
    case gen_tcp:accept(ListenSocket) of
	{ok,Socket} ->
	    apply(CallbackModule, start, [Socket]),
	    %ejabberd_c2s:start(Socket),
	    accept(ListenSocket, CallbackModule)
    end.


