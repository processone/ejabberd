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

-export([start_link/0, init/1, start/3,
	 init/3,
	 init_ssl/4
	]).

-include("ejabberd.hrl").

start_link() ->
    supervisor:start_link({local, ejabberd_listeners}, ?MODULE, []).


init(_) ->
    case ejabberd_config:get_local_option(listen) of
	undefined ->
	    ignore;
	Ls ->
	    {ok, {{one_for_one, 10, 1},
		  lists:map(
		    fun({Port, Module, Opts}) ->
			    {Port,
			     {?MODULE, start, [Port, Module, Opts]},
			     permanent,
			     brutal_kill,
			     worker,
			     [?MODULE]}
		    end, Ls)}}
    end.


start(Port, Module, Opts) ->
    case lists:keysearch(ssl, 1, Opts) of
	{value, {ssl, SSLOpts}} ->
	    {ok, proc_lib:spawn_link(?MODULE, init_ssl,
				     [Port, Module, Opts, SSLOpts])};
	_ ->
	    case lists:member(ssl, Opts) of
		true ->
		    {ok, proc_lib:spawn_link(?MODULE, init_ssl,
					     [Port, Module, Opts, []])};
		false ->
		    {ok, proc_lib:spawn_link(?MODULE, init,
					     [Port, Module, Opts])}
	    end
    end.

init(Port, Module, Opts) ->
    SockOpts = lists:filter(fun({ip, _}) -> true;
			       (inet6) -> true;
			       (inet) -> true;
			       (_) -> false
			    end, Opts),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary,
					       {packet, 0}, 
					       {active, false},
					       {reuseaddr, true},
					       {nodelay, true} |
					       SockOpts]),
    accept(ListenSocket, Module, Opts).

accept(ListenSocket, Module, Opts) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    case {inet:sockname(Socket), inet:peername(Socket)} of
		{{ok, Addr}, {ok, PAddr}} ->
		    ?INFO_MSG("(~w) Accepted connection ~w -> ~w",
			      [Socket, PAddr, Addr]);
		_ ->
		    ok
	    end,
	    {ok, Pid} = Module:start({gen_tcp, Socket}, Opts),
	    gen_tcp:controlling_process(Socket, Pid),
	    accept(ListenSocket, Module, Opts);
	{error, Reason} ->
	    ?INFO_MSG("(~w) Failed TCP accept: ~w",
		      [ListenSocket, Reason]),
	    accept(ListenSocket, Module, Opts)
    end.


init_ssl(Port, Module, Opts, SSLOpts) ->
    SockOpts = lists:filter(fun({ip, _}) -> true;
			       (inet6) -> true;
			       (inet) -> true;
			       ({verify, _}) -> true;
			       ({depth, _}) -> true;
			       ({certfile, _}) -> true;
			       ({keyfile, _}) -> true;
			       ({password, _}) -> true;
			       ({cacertfile, _}) -> true;
			       ({ciphers, _}) -> true;
			       (_) -> false
			    end, Opts),
    {ok, ListenSocket} = ssl:listen(Port, [binary,
					   {packet, 0}, 
					   {active, false},
					   {nodelay, true} |
					   SockOpts ++ SSLOpts]),
    accept_ssl(ListenSocket, Module, Opts).

accept_ssl(ListenSocket, Module, Opts) ->
    case ssl:accept(ListenSocket, 200) of
	{ok, Socket} ->
	    case {ssl:sockname(Socket), ssl:peername(Socket)} of
		{{ok, Addr}, {ok, PAddr}} ->
		    ?INFO_MSG("(~w) Accepted SSL connection ~w -> ~w",
			      [Socket, PAddr, Addr]);
		_ ->
		    ok
	    end,
	    {ok, Pid} = Module:start({ssl, Socket}, Opts),
	    ssl:controlling_process(Socket, Pid),
	    accept_ssl(ListenSocket, Module, Opts);
	{error, timeout} ->
	    accept_ssl(ListenSocket, Module, Opts);
	{error, Reason} ->
	    ?INFO_MSG("(~w) Failed SSL handshake: ~w",
		      [ListenSocket, Reason]),
	    accept_ssl(ListenSocket, Module, Opts)
    end.


