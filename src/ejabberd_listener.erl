%%%----------------------------------------------------------------------
%%% File    : ejabberd_listener.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage socket listener
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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
	 start_listeners/0,
	 start_listener/3,
	 stop_listener/2,
	 parse_listener_portip/2,
	 add_listener/3,
	 delete_listener/2
	]).

-include("ejabberd.hrl").

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

start_link() ->
    supervisor:start_link({local, ejabberd_listeners}, ?MODULE, []).


init(_) ->
    {ok, {{one_for_one, 10, 1}, []}}.

start_listeners() ->
    case ejabberd_config:get_local_option(listen) of
	undefined ->
	    ignore;
	Ls ->
	    Ls2 = lists:map(
	        fun({Port, Module, Opts}) ->
		        start_listener(Port, Module, Opts)
		    end, Ls),
	    report_duplicated_portips(Ls),
	    {ok, {{one_for_one, 10, 1}, Ls2}}
    end.

report_duplicated_portips(L) ->
    LKeys = [Port || {Port, _, _} <- L],
    LNoDupsKeys = proplists:get_keys(L),
    case LKeys -- LNoDupsKeys of
	[] -> ok;
	Dups ->
	    ?CRITICAL_MSG("In the ejabberd configuration there are duplicated "
			  "Port number + IP address:~n  ~p",
			  [Dups])
  end.

start(Port, Module, Opts) ->
    %% Check if the module is an ejabberd listener or an independent listener
    ModuleRaw = strip_frontend(Module),
    case ModuleRaw:socket_type() of
	independent -> ModuleRaw:start_listener(Port, Opts);
	_ -> start_dependent(Port, Module, Opts)
    end.

%% -> {ok, Pid} | {error, ErrorMessage}
start_dependent(Port, Module, Opts) ->
    case includes_deprecated_ssl_option(Opts) of
	false ->
	    proc_lib:start_link(?MODULE, init, [Port, Module, Opts]);
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

init(PortIP, Module, Opts1) ->
    {Port, IPT, IPS, IPV, OptsClean} = parse_listener_portip(PortIP, Opts1),
    %% The first inet|inet6 and the last {ip, _} work,
    %% so overriding those in Opts
    Opts = [IPV | OptsClean] ++ [{ip, IPT}],
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
				{send_timeout, ?TCP_SEND_TIMEOUT},
				{keepalive, true} |
				SockOpts]),
    case Res of
	{ok, ListenSocket} ->
		%% Inform my parent that this port was opened succesfully
		proc_lib:init_ack({ok, self()}),
		%% And now start accepting connection attempts
	    accept(ListenSocket, Module, Opts);
	{error, Reason} ->
	    ReasonT = case Reason of
			  eaddrnotavail -> "IP address not available: " ++ IPS;
			  _ -> atom_to_list(Reason)
		      end,
	    ?ERROR_MSG("Failed to open socket:~n  ~p~nReason: ~s",
		       [{Port, Module, SockOpts}, ReasonT]),
	    throw(ReasonT)
    end.

%% @spec (PortIP, Opts) -> {Port, IPT, IPS, IPV, OptsClean}
%% where
%%      PortIP = Port | {Port, IPT | IPS}
%%      Port = integer()
%%      IPT = tuple()
%%      IPS = string()
%%      IPV = inet | inet6
%%      Opts = [IPV | {ip, IPT} | atom() | tuple()]
%%      OptsClean = [atom() | tuple()]
%% @doc Parse any kind of ejabberd listener specification.
%% The parsed options are returned in several formats.
%% OptsClean does not include inet/inet6 or ip options.
%% Opts can include the options inet6 and {ip, Tuple},
%% but they are only used when no IP address was specified in the PortIP.
%% The IP version (either IPv4 or IPv6) is inferred from the IP address type,
%% so the option inet/inet6 is only used when no IP is specified at all.
parse_listener_portip(PortIP, Opts) ->
    {IPOpt, Opts2} = strip_ip_option(Opts),
    {IPVOpt, OptsClean} = case lists:member(inet6, Opts2) of
			      true -> {inet6, Opts2 -- [inet6]};
			      false -> {inet, Opts2}
			  end,
    {Port, IPT, IPS} = case PortIP of
			   P when is_integer(P) ->
			       T = get_ip_tuple(IPOpt, IPVOpt),
			       S = inet_parse:ntoa(T),
			       {P, T, S};
			   {P, T} when is_integer(P) and is_tuple(T) ->
			       S = inet_parse:ntoa(T),
			       {P, T, S};
			   {P, S} when is_integer(P) and is_list(S) ->
			       [S | _] = string:tokens(S, "/"),
			       {ok, T} = inet_parse:address(S),
			       {P, T, S}
		       end,
    IPV = case size(IPT) of
	      4 -> inet;
	      8 -> inet6
	  end,
    {Port, IPT, IPS, IPV, OptsClean}.

strip_ip_option(Opts) ->
    {IPL, OptsNoIP} = lists:partition(
			fun({ip, _}) -> true;
			   (_) -> false
			end,
			Opts),
    case IPL of
	%% Only the first ip option is considered
	[{ip, T1} | _] when is_tuple(T1) ->
	    {T1, OptsNoIP};
	[] ->
	    {no_ip_option, OptsNoIP}
    end.

get_ip_tuple(no_ip_option, inet) ->
    {0, 0, 0, 0};
get_ip_tuple(no_ip_option, inet6) ->
    {0, 0, 0, 0, 0, 0, 0, 0};
get_ip_tuple(IPOpt, _IPVOpt) ->
    IPOpt.

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
	    CallMod = case is_frontend(Module) of
			  true -> ejabberd_frontend_socket;
			  false -> ejabberd_socket
		      end,
	    CallMod:start(strip_frontend(Module), gen_tcp, Socket, Opts),
	    accept(ListenSocket, Module, Opts);
	{error, Reason} ->
	    ?INFO_MSG("(~w) Failed TCP accept: ~w",
		      [ListenSocket, Reason]),
	    accept(ListenSocket, Module, Opts)
    end.

%% @spec (Port, Module, Opts) -> {ok, Pid} | {error, Error}
start_listener(Port, Module, Opts) ->
    %% It is only required to start the supervisor in some cases.
    %% But it doesn't hurt to attempt to start it for any listener.
    %% So, it's normal (and harmless) that in most cases this call returns: {error, {already_started, pid()}}
    start_module_sup(Port, Module),
    start_listener_sup(Port, Module, Opts).

start_module_sup(_Port, Module) ->
    Proc1 = gen_mod:get_module_proc("sup", Module),
    ChildSpec1 =
	{Proc1,
	 {ejabberd_tmp_sup, start_link, [Proc1, strip_frontend(Module)]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec1).

start_listener_sup(Port, Module, Opts) ->
    ChildSpec = {Port,
		 {?MODULE, start, [Port, Module, Opts]},
		 transient,
		 brutal_kill,
		 worker,
		 [?MODULE]},
    supervisor:start_child(ejabberd_listeners, ChildSpec).

stop_listener(Port, Module) ->
    supervisor:terminate_child(ejabberd_listeners, Port),
    supervisor:delete_child(ejabberd_listeners, Port),
    Proc1 = gen_mod:get_module_proc("sup", Module),
    supervisor:terminate_child(ejabberd_sup, Proc1),
    supervisor:delete_child(ejabberd_sup, Proc1).

%% @spec (PortIP, Module, Opts) -> {ok, Pid} | {error, Error}
%% where
%%      PortIP = {Port, IPT | IPS}
%%      Port = integer()
%%      IPT = tuple()
%%      IPS = string()
%%      IPV = inet | inet6
%%      Module = atom()
%%      Opts = [IPV | {ip, IPT} | atom() | tuple()]
%% @doc Add a listener and store in config if success
add_listener(PortIP, Module, Opts) ->
    case start_listener(PortIP, Module, Opts) of
	{ok, _Pid} ->
	    Ports = case ejabberd_config:get_local_option(listen) of
			undefined ->
			    [];
			Ls ->
			    Ls
		    end,
	    Ports1 = lists:keydelete(PortIP, 1, Ports),
	    Ports2 = [{PortIP, Module, Opts} | Ports1],
	    ejabberd_config:add_local_option(listen, Ports2),
		ok;
	{error, {already_started, _Pid}} ->
	    {error, {already_started, PortIP}};
	{error, Error} ->
	    {error, Error}
    end.
  
%% @spec (PortIP, Module) -> ok
%% where
%%      PortIP = {Port, IPT | IPS}
%%      Port = integer()
%%      IPT = tuple()
%%      IPS = string()
%%      Module = atom()
delete_listener(PortIP, Module) ->
    Ports = case ejabberd_config:get_local_option(listen) of
		undefined ->
		    [];
		Ls ->
		    Ls
	    end,
    Ports1 = lists:keydelete(PortIP, 1, Ports),
    ejabberd_config:add_local_option(listen, Ports1),
    stop_listener(PortIP, Module).

is_frontend({frontend, _Module}) -> true;
is_frontend(_) -> false.

%% @doc(FrontMod) -> atom()
%% where FrontMod = atom() | {frontend, atom()}
strip_frontend({frontend, Module}) -> Module;
strip_frontend(Module) when is_atom(Module) -> Module.
