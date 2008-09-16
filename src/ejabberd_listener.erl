%%%----------------------------------------------------------------------
%%% File    : ejabberd_listener.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage socket listener
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_listener).
-author('alexey@process-one.net').

-export([start_link/0, init/1, start/3,
	 init/3,
	 start_listener/3,
	 stop_listener/1,
	 add_listener/3,
	 delete_listener/1
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
			     transient,
			     brutal_kill,
			     worker,
			     [?MODULE]}
		    end, Ls)}}
    end.


start(Port, Module, Opts) ->
    case includes_deprecated_ssl_option(Opts) of
	false ->
	    {ok, proc_lib:spawn_link(?MODULE, init,
				     [Port, Module, Opts])};
	true ->
            SSLErr="There is a problem with your ejabberd configuration file: "
		"the option 'ssl' for listening sockets is no longer available."
		"To get SSL encryption use the option 'tls'.",
	    ?ERROR_MSG(SSLErr, []),
	    {error, SSLErr}
    end.

%% Parse the options of the socket,
%% and return if the deprecated option 'ssl' is included
%% @spec(Opts::[opt()]) -> true | false
includes_deprecated_ssl_option(Opts) ->
    case lists:keysearch(ssl, 1, Opts) of
	{value, {ssl, _SSLOpts}} ->
	    true;
	_ ->
	    lists:member(ssl, Opts)
    end.

init(Port, Module, Opts) ->
    SockOpts = lists:filter(fun({ip, _}) -> true;
			       (inet6) -> true;
			       (inet) -> true;
			       (_) -> false
			    end, Opts),

    Res = gen_tcp:listen(Port, [binary,
				{packet, 0}, 
				{active, false},
				{reuseaddr, true},
				{nodelay, true},
				{keepalive, true} |
				SockOpts]),
    case Res of
	{ok, ListenSocket} ->
	    accept(ListenSocket, Module, Opts);
	{error, Reason} ->
	    ?ERROR_MSG("Failed to open socket for ~p: ~p",
		       [{Port, Module, Opts}, Reason]),
	    error
    end.

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
	    case Module of
		{frontend, Mod} ->
		    ejabberd_frontend_socket:start(Mod, gen_tcp, Socket, Opts);
		_ ->
		    ejabberd_socket:start(Module, gen_tcp, Socket, Opts)
	    end,
	    accept(ListenSocket, Module, Opts);
	{error, Reason} ->
	    ?INFO_MSG("(~w) Failed TCP accept: ~w",
		      [ListenSocket, Reason]),
	    accept(ListenSocket, Module, Opts)
    end.

start_listener(Port, Module, Opts) ->
    ChildSpec = {Port,
		 {?MODULE, start, [Port, Module, Opts]},
		 transient,
		 brutal_kill,
		 worker,
		 [?MODULE]},
    supervisor:start_child(ejabberd_listeners, ChildSpec).

stop_listener(Port) ->
    supervisor:terminate_child(ejabberd_listeners, Port),
    supervisor:delete_child(ejabberd_listeners, Port).

add_listener(Port, Module, Opts) ->
    Ports = case ejabberd_config:get_local_option(listen) of
		undefined ->
		    [];
		Ls ->
		    Ls
	    end,
    Ports1 = lists:keydelete(Port, 1, Ports),
    Ports2 = [{Port, Module, Opts} | Ports1],
    ejabberd_config:add_local_option(listen, Ports2),
    start_listener(Port, Module, Opts).

delete_listener(Port) ->
    Ports = case ejabberd_config:get_local_option(listen) of
		undefined ->
		    [];
		Ls ->
		    Ls
	    end,
    Ports1 = lists:keydelete(Port, 1, Ports),
    ejabberd_config:add_local_option(listen, Ports1),
    stop_listener(Port).

