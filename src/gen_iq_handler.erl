%%%----------------------------------------------------------------------
%%% File    : gen_iq_handler.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 22 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(gen_iq_handler).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 add_iq_handler/5,
	 remove_iq_handler/2,
	 stop_iq_handler/3,
	 handle/6,
	 process_iq/5,
	 queue_init/2]).

-include("ejabberd.hrl").

start() ->
    ok.

add_iq_handler(Component, NS, Module, Function, Type) ->
    case Type of
	no_queue ->
	    Component:register_iq_handler(NS, Module, Function, no_queue);
	one_queue ->
	    Pid = spawn(?MODULE, queue_init, [Module, Function]),
	    Component:register_iq_handler(NS, Module, Function,
					  {one_queue, Pid});
	parallel ->
	    Component:register_iq_handler(NS, Module, Function, parallel)
    end.

remove_iq_handler(Component, NS) ->
    Component:unregister_iq_handler(NS).

stop_iq_handler(Module, Function, Opts) ->
    case Opts of
	{one_queue, Pid} ->
	    exit(Pid, kill);
	_ ->
	    ok
    end.

handle(Module, Function, Opts, From, To, IQ) ->
    case Opts of
	no_queue ->
	    process_iq(Module, Function, From, To, IQ);
	{one_queue, Pid} ->
	    Pid ! {process_iq, From, To, IQ};
	parallel ->
	    spawn(?MODULE, process_iq, [Module, Function, From, To, IQ]);
	_ ->
	    todo
    end.


process_iq(Module, Function, From, To, IQ) ->
    case catch Module:Function(From, To, IQ) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	ResIQ ->
	    if
		ResIQ /= ignore ->
		    ejabberd_router:route(To, From,
					  jlib:iq_to_xml(ResIQ));
		true ->
		    ok
	    end
    end.

queue_init(Module, Function) ->
    queue_loop(Module, Function).

% TODO: use gen_event
queue_loop(Module, Function) ->
    receive
	{process_iq, From, To, IQ} ->
	    process_iq(Module, Function, From, To, IQ),
	    queue_loop(Module, Function);
	_ ->
	    queue_loop(Module, Function)
    end.
