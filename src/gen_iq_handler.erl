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
	 start_link/3,
	 add_iq_handler/6,
	 remove_iq_handler/3,
	 stop_iq_handler/3,
	 handle/7,
	 process_iq/6,
	 queue_init/3]).

-include("ejabberd.hrl").

start() ->
    ok.

add_iq_handler(Component, Host, NS, Module, Function, Type) ->
    case Type of
	no_queue ->
	    Component:register_iq_handler(Host, NS, Module, Function, no_queue);
	one_queue ->
	    {ok, Pid} = supervisor:start_child(ejabberd_iq_sup,
					       [Host, Module, Function]),
	    Component:register_iq_handler(Host, NS, Module, Function,
					  {one_queue, Pid});
	parallel ->
	    Component:register_iq_handler(Host, NS, Module, Function, parallel)
    end.

remove_iq_handler(Component, Host, NS) ->
    Component:unregister_iq_handler(Host, NS).

stop_iq_handler(Module, Function, Opts) ->
    case Opts of
	{one_queue, Pid} ->
	    exit(Pid, kill);
	_ ->
	    ok
    end.

handle(Host, Module, Function, Opts, From, To, IQ) ->
    case Opts of
	no_queue ->
	    process_iq(Host, Module, Function, From, To, IQ);
	{one_queue, Pid} ->
	    Pid ! {process_iq, From, To, IQ};
	parallel ->
	    spawn(?MODULE, process_iq, [Host, Module, Function, From, To, IQ]);
	_ ->
	    todo
    end.


process_iq(_Host, Module, Function, From, To, IQ) ->
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

start_link(Host, Module, Function) ->
  {ok, proc_lib:spawn_link(?MODULE, queue_init, [Host, Module, Function])}.

queue_init(Host, Module, Function) ->
    queue_loop(Host, Module, Function).

% TODO: use gen_event
queue_loop(Host, Module, Function) ->
    receive
	{process_iq, From, To, IQ} ->
	    process_iq(Host, Module, Function, From, To, IQ),
	    queue_loop(Host, Module, Function);
	_ ->
	    queue_loop(Host, Module, Function)
    end.
