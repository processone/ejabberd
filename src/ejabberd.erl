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

-export([start/0, init/0]).

start() ->
    spawn(?MODULE, init, []).

init() ->
    register(ejabberd, self()),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    ok = erl_ddll:load_driver(".", expat_erl),
    Port = open_port({spawn, expat_erl}, [binary]),
    db_init(),
    ejabberd_auth:start(),
    ejabberd_listener:start(),
    loop(Port).


loop(Port) ->
    receive
	_ ->
	    loop(Port)
    end.

db_init() ->
    mnesia:create_schema([node()]),
    mnesia:start().
