%%%----------------------------------------------------------------------
%%% File    : ejabberd.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0]).

start() ->
    register(ejabberd, self()),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    ok = erl_ddll:load_driver(".", expat_erl),
    Port = open_port({spawn, expat_erl}, [binary]),
    ejabberd_listener:start(),
    loop(Port).


loop(Port) ->
    receive
	_ ->
	    loop(Port)
    end.
