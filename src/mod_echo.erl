%%%----------------------------------------------------------------------
%%% File    : mod_echo.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 15 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_echo).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, init/0]).

-include("ejabberd.hrl").
-include("namespaces.hrl").



start() ->
    spawn(?MODULE, init, []).

init() ->
    ejabberd_router:register_local_route("echo." ++ ?MYNAME),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    ejabberd_router:route(To, From, Packet),
	    loop();
	_ ->
	    loop()
    end.

