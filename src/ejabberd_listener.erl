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

-export([start/0, init/1, start/4, init/4]).

start() ->
    supervisor:start_link({local, ejabberd_listeners}, ?MODULE, []).


init(_) ->
    case ejabberd_config:get_local_option(listen) of
	undefined ->
	    ignore;
	Ls ->
	    {ok, {{one_for_one, 10, 1},
		  lists:map(
		    fun({Port, Module, Fun, Args}) ->
			    {Port,
			     {?MODULE, start, [Port, Module, Fun, Args]},
			     permanent,
			     brutal_kill,
			     worker,
			     [Module]}
		    end, Ls)}}
    end.


start(Port, Module, Fun, Args) ->
    {ok, spawn_link(?MODULE, init, [Port, Module, Fun, Args])}.

init(Port, Module, Fun, Args) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary,
					       {packet, 0}, 
					       {active, false},
					       {reuseaddr, true}]),
    accept(ListenSocket, Module, Fun, Args).

accept(ListenSocket, Module, Fun, Args) ->
    case gen_tcp:accept(ListenSocket) of
	{ok,Socket} ->
	    apply(Module, Fun, [Socket] ++ Args),
	    %ejabberd_c2s:start(Socket),
	    accept(ListenSocket, Module, Fun, Args)
    end.


