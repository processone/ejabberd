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

-behaviour(gen_mod).

-export([start/1, init/1]).

-include("ejabberd.hrl").
-include("namespaces.hrl").



start(Opts) ->
    %Host = gen_mod:get_opt(host, Opts),
    Host = gen_mod:get_opt(host, Opts, "echo." ++ ?MYNAME),
    spawn(?MODULE, init, [Host]).

init(Host) ->
    ejabberd_router:register_local_route(Host),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    ejabberd_router:route(To, From, Packet),
	    loop();
	_ ->
	    loop()
    end.

