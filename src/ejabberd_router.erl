%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_router).
-author('alexey@sevcom.net').

%%-export([Function/Arity, ...]).

-export([start/0, init/0]).

-record(service, {domain, node, pid}).


start() ->
    spawn(ejabberd_router, init, []).

init() ->
    register(ejabberd_router, self()),
    mnesia:create_table(service,
			[{ram_copies, [node()]},
			 {attributes,
			  record_info(fields, service)}]),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    % TODO
	    loop();
	{register_service, Domain, Pid, Node} ->
	    % TODO
	    loop();
	{unregister_service, Domain} ->
	    % TODO
	    loop();
	_ ->
	    loop()
    end.
