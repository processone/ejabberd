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
	    {ok, proc_lib:spawn_link(?MODULE, init, [Port, Module, Opts])}
    end.

init(Port, Module, Opts) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary,
					       {packet, 0}, 
					       {active, false},
					       {reuseaddr, true},
					       {nodelay, true}]),
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
	    %{ok, Pid} =
	    %    supervisor:start_child(
	    %      ejabberd_tmp_sup,
	    %      {{Module, Socket},
	    %       {Module, start_link, [{gen_tcp, Socket}, Opts]},
	    %       transient,
	    %       brutal_kill,
	    %       worker,
	    %       [Module]}),
	    gen_tcp:controlling_process(Socket, Pid),
	    accept(ListenSocket, Module, Opts)
    end.


init_ssl(Port, Module, Opts, SSLOpts) ->
    {ok, ListenSocket} = ssl:listen(Port, [binary,
					   {packet, 0}, 
					   {active, false},
					   {nodelay, true} |
					   SSLOpts]),
    accept_ssl(ListenSocket, Module, Opts).

accept_ssl(ListenSocket, Module, Opts) ->
    case ssl:accept(ListenSocket) of
	{ok, Socket} ->
	    case {ssl:sockname(Socket), ssl:peername(Socket)} of
		{{ok, Addr}, {ok, PAddr}} ->
		    ?INFO_MSG("(~w) Accepted SSL connection ~w -> ~w",
			      [Socket, PAddr, Addr]);
		_ ->
		    ok
	    end,
	    apply(Module, start_link, [{ssl, Socket}, Opts]),
	    accept_ssl(ListenSocket, Module, Opts)
    end.


