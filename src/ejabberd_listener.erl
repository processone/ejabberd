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

-export([start_link/0, init/1, start/4,
	 init/4,
	 init_ssl/5
	]).

start_link() ->
    supervisor:start_link({local, ejabberd_listeners}, ?MODULE, []).


init(_) ->
    case ejabberd_config:get_local_option(listen) of
	undefined ->
	    ignore;
	Ls ->
	    {ok, {{one_for_one, 10, 1},
		  lists:map(
		    fun({Port, Module, Fun, Opts}) ->
			    {Port,
			     {?MODULE, start, [Port, Module, Fun, Opts]},
			     permanent,
			     brutal_kill,
			     worker,
			     [Module]}
		    end, Ls)}}
    end.


start(Port, Module, Fun, Opts) ->
    case lists:keysearch(ssl, 1, Opts) of
	{value, {ssl, SSLOpts}} ->
	    {ok, spawn_link(?MODULE, init_ssl,
			    [Port, Module, Fun, Opts, SSLOpts])};
	_ ->
	    {ok, spawn_link(?MODULE, init, [Port, Module, Fun, Opts])}
    end.

init(Port, Module, Fun, Opts) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary,
					       {packet, 0}, 
					       {active, false},
					       {reuseaddr, true}]),
    accept(ListenSocket, Module, Fun, Opts).

accept(ListenSocket, Module, Fun, Opts) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    {ok, Pid} = apply(Module, Fun, [{gen_tcp, Socket}, Opts]),
	    gen_tcp:controlling_process(Socket, Pid),
	    accept(ListenSocket, Module, Fun, Opts)
    end.


init_ssl(Port, Module, Fun, Opts, SSLOpts) ->
    {ok, ListenSocket} = ssl:listen(Port, [binary,
					   {packet, 0}, 
					   {active, false},
					   {nodelay, true},
					   {backlog, 0},
					   {cachetimout, 0} |
					   SSLOpts]),
    accept_ssl(ListenSocket, Module, Fun, Opts).

accept_ssl(ListenSocket, Module, Fun, Opts) ->
    case ssl:accept(ListenSocket) of
	{ok, Socket} ->
	    apply(Module, Fun, [{ssl, Socket}, Opts]),
	    accept_ssl(ListenSocket, Module, Fun, Opts)
    end.


