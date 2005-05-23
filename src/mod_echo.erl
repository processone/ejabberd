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

-export([start/1, init/1, stop/0]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_echo).

start(Opts) ->
    %Host = gen_mod:get_opt(host, Opts),
    Host = gen_mod:get_opt(host, Opts, "echo." ++ ?MYNAME),
    register(?PROCNAME, spawn(?MODULE, init, [Host])).

init(Host) ->
    ejabberd_router:register_route(Host),
    loop(Host).

loop(Host) ->
    receive
	{route, From, To, Packet} ->
	    ejabberd_router:route(To, From, Packet),
	    loop(Host);
	stop ->
	    ejabberd_router:unregister_route(Host),
	    ok;
	_ ->
	    loop(Host)
    end.

stop() ->
    ?PROCNAME ! stop,
    {wait, ?PROCNAME}.

