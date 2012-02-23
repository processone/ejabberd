%%%----------------------------------------------------------------------
%%% File    : ejabberd_listener.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage socket listener
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
	 stop_listeners/0,
	 stop_listener/2,
	 parse_listener_portip/2,
	 add_listener/3,
	 delete_listener/2,
	 rate_limit/2
	]).

-include("ejabberd.hrl").

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

start_link() ->
    supervisor:start_link({local, ejabberd_listeners}, ?MODULE, []).


init(_) ->
    ets:new(listen_sockets, [named_table, public]),
    bind_tcp_ports(),
    {ok, {{one_for_one, 10, 1}, []}}.

bind_tcp_ports() ->
    case ejabberd_config:get_local_option(listen) of
	undefined ->
	    ignore;
	Ls ->
	    lists:foreach(
	      fun({Port, Module, Opts}) ->
		      ModuleRaw = strip_frontend(Module),
		      case ModuleRaw:socket_type() of
			  independent -> ok;
			  _ ->
			      bind_tcp_port(Port, Module, Opts)
		      end
	      end, Ls)
    end.

bind_tcp_port(PortIP, Module, RawOpts) ->
    try check_listener_options(RawOpts) of
	ok ->
	    {Port, IPT, IPS, IPV, Proto, OptsClean} = parse_listener_portip(PortIP, RawOpts),
	    {_Opts, SockOpts} = prepare_opts(IPT, IPV, OptsClean),
	    case Proto of
		udp -> ok;
		_ ->
		    ListenSocket = listen_tcp(PortIP, Module, SockOpts, Port, IPS),
		    ets:insert(listen_sockets, {PortIP, ListenSocket})
	    end
    catch
	throw:{error, Error} ->
	    ?ERROR_MSG(Error, [])
    end.

start_listeners() ->
    case ejabberd_config:get_local_option(listen) of
	undefined ->
	    ignore;
	Ls ->
	    Ls2 = lists:map(
	        fun({Port, Module, Opts}) ->
		        case start_listener(Port, Module, Opts) of
			    {ok, _Pid} = R -> R;
			    {error, Error} ->
				throw(Error)
			end
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

%% @spec(Port, Module, Opts) -> {ok, Pid} | {error, ErrorMessage}
start_dependent(Port, Module, Opts) ->
    try check_listener_options(Opts) of
	ok ->
	    proc_lib:start_link(?MODULE, init, [Port, Module, Opts])
    catch
	throw:{error, Error} ->
	    ?ERROR_MSG(Error, []),
	    {error, Error}
    end.

init(PortIP, Module, RawOpts) ->
    {Port, IPT, IPS, IPV, Proto, OptsClean} = parse_listener_portip(PortIP, RawOpts),
    {Opts, SockOpts} = prepare_opts(IPT, IPV, OptsClean),
    if Proto == udp ->
	    init_udp(PortIP, Module, Opts, SockOpts, Port, IPS);
       true ->
	    init_tcp(PortIP, Module, Opts, SockOpts, Port, IPS)
    end.

init_udp(PortIP, Module, Opts, SockOpts, Port, IPS) ->
    case gen_udp:open(Port, [binary,
			     {active, false},
			     {reuseaddr, true} |
			     SockOpts]) of
	{ok, Socket} ->
	    %% Inform my parent that this port was opened succesfully
	    proc_lib:init_ack({ok, self()}),
	    udp_recv(Socket, Module, Opts);
	{error, Reason} ->
	    socket_error(Reason, PortIP, Module, SockOpts, Port, IPS)
    end.

init_tcp(PortIP, Module, Opts, SockOpts, Port, IPS) ->
    ListenSocket = listen_tcp(PortIP, Module, SockOpts, Port, IPS),
    %% Inform my parent that this port was opened succesfully
    proc_lib:init_ack({ok, self()}),
    %% And now start accepting connection attempts
    accept(ListenSocket, Module, Opts).

listen_tcp(PortIP, Module, SockOpts, Port, IPS) ->
    case ets:lookup(listen_sockets, PortIP) of
	[{PortIP, ListenSocket}] ->
	    ?INFO_MSG("Reusing listening port for ~p", [Port]),
	    ets:delete(listen_sockets, Port),
	    ListenSocket;
	_ ->
	    SockOpts2 = try erlang:system_info(otp_release) >= "R13B" of
			    true -> [{send_timeout_close, true} | SockOpts];
			    false -> SockOpts
			catch
			    _:_ -> []
			end,
	    Res = gen_tcp:listen(Port, [binary,
					{packet, 0},
					{active, false},
					{reuseaddr, true},
					{nodelay, true},
					{send_timeout, ?TCP_SEND_TIMEOUT},
					{keepalive, true} |
					SockOpts2]),
	    case Res of
		{ok, ListenSocket} ->
		    ListenSocket;
		{error, Reason} ->
		    socket_error(Reason, PortIP, Module, SockOpts, Port, IPS)
	    end
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
    {Port, IPT, IPS, Proto} =
	case add_proto(PortIP, Opts) of
	    {P, Prot} ->
		T = get_ip_tuple(IPOpt, IPVOpt),
		S = inet_parse:ntoa(T),
		{P, T, S, Prot};
	    {P, T, Prot} when is_integer(P) and is_tuple(T) ->
		S = inet_parse:ntoa(T),
		{P, T, S, Prot};
	    {P, S, Prot} when is_integer(P) and is_list(S) ->
		[S | _] = string:tokens(S, "/"),
		{ok, T} = inet_parse:address(S),
		{P, T, S, Prot}
	end,
    IPV = case size(IPT) of
	      4 -> inet;
	      8 -> inet6
	  end,
    {Port, IPT, IPS, IPV, Proto, OptsClean}.

prepare_opts(IPT, IPV, OptsClean) ->
    %% The first inet|inet6 and the last {ip, _} work,
    %% so overriding those in Opts
    Opts = [IPV | OptsClean] ++ [{ip, IPT}],
    SockOpts = lists:filter(fun({ip, _}) -> true;
			       (inet6) -> true;
			       (inet) -> true;
			       ({backlog, _}) -> true;
			       (_) -> false
			    end, Opts),
    {Opts, SockOpts}.

add_proto(Port, Opts) when is_integer(Port) ->
    {Port, get_proto(Opts)};
add_proto({Port, Proto}, _Opts) when is_atom(Proto) ->
    {Port, normalize_proto(Proto)};
add_proto({Port, Addr}, Opts) ->
    {Port, Addr, get_proto(Opts)};
add_proto({Port, Addr, Proto}, _Opts) ->
    {Port, Addr, normalize_proto(Proto)}.

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
    accept(ListenSocket, Module, Opts, 0).
accept(ListenSocket, Module, Opts, Interval) ->
    NewInterval = check_rate_limit(Interval),
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
	    accept(ListenSocket, Module, Opts, NewInterval);
	{error, Reason} ->
	    ?INFO_MSG("(~w) Failed TCP accept: ~w",
		      [ListenSocket, Reason]),
	    accept(ListenSocket, Module, Opts, NewInterval)
    end.

udp_recv(Socket, Module, Opts) ->
    case gen_udp:recv(Socket, 0) of
	{ok, {Addr, Port, Packet}} ->
	    case catch Module:udp_recv(Socket, Addr, Port, Packet, Opts) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("failed to process UDP packet:~n"
			       "** Source: {~p, ~p}~n"
			       "** Reason: ~p~n** Packet: ~p",
			       [Addr, Port, Reason, Packet]);
		_ ->
		    ok
	    end,
	    udp_recv(Socket, Module, Opts);
	{error, Reason} ->
	    ?ERROR_MSG("unexpected UDP error: ~s", [format_error(Reason)]),
	    throw({error, Reason})
    end.

%% @spec (Port, Module, Opts) -> {ok, Pid} | {error, Error}
start_listener(Port, Module, Opts) ->
    case start_listener2(Port, Module, Opts) of
	{ok, _Pid} = R -> R;
	{error, {{'EXIT', {undef, [{M, _F, _A}|_]}}, _} = Error} ->
	    ?ERROR_MSG("Error starting the ejabberd listener: ~p.~n"
		       "It could not be loaded or is not an ejabberd listener.~n"
		       "Error: ~p~n", [Module, Error]),
	    {error, {module_not_available, M}};
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	{error, Error} ->
	    {error, Error}
    end.

%% @spec (Port, Module, Opts) -> {ok, Pid} | {error, Error}
start_listener2(Port, Module, Opts) ->
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

stop_listeners() ->
    Ports = ejabberd_config:get_local_option(listen),
    lists:foreach(
      fun({PortIpNetp, Module, _Opts}) ->
	      delete_listener(PortIpNetp, Module)
      end,
      Ports).

%% @spec (PortIP, Module) -> ok
%% where
%%      PortIP = {Port, IPT | IPS}
%%      Port = integer()
%%      IPT = tuple()
%%      IPS = string()
%%      Module = atom()
stop_listener(PortIP, _Module) ->
    supervisor:terminate_child(ejabberd_listeners, PortIP),
    supervisor:delete_child(ejabberd_listeners, PortIP).

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
    {Port, IPT, _, _, Proto, _} = parse_listener_portip(PortIP, Opts),
    PortIP1 = {Port, IPT, Proto},
    case start_listener(PortIP1, Module, Opts) of
	{ok, _Pid} ->
	    Ports = case ejabberd_config:get_local_option(listen) of
			undefined ->
			    [];
			Ls ->
			    Ls
		    end,
	    Ports1 = lists:keydelete(PortIP1, 1, Ports),
	    Ports2 = [{PortIP1, Module, Opts} | Ports1],
	    ejabberd_config:add_local_option(listen, Ports2),
	    ok;
	{error, {already_started, _Pid}} ->
	    {error, {already_started, PortIP}};
	{error, Error} ->
	    {error, Error}
    end.

delete_listener(PortIP, Module) ->
    delete_listener(PortIP, Module, []).

%% @spec (PortIP, Module, Opts) -> ok
%% where
%%      PortIP = {Port, IPT | IPS}
%%      Port = integer()
%%      IPT = tuple()
%%      IPS = string()
%%      Module = atom()
%%      Opts = [term()]
delete_listener(PortIP, Module, Opts) ->
    {Port, IPT, _, _, Proto, _} = parse_listener_portip(PortIP, Opts),
    PortIP1 = {Port, IPT, Proto},
    Ports = case ejabberd_config:get_local_option(listen) of
		undefined ->
		    [];
		Ls ->
		    Ls
	    end,
    Ports1 = lists:keydelete(PortIP1, 1, Ports),
    ejabberd_config:add_local_option(listen, Ports1),
    stop_listener(PortIP1, Module).

is_frontend({frontend, _Module}) -> true;
is_frontend(_) -> false.

%% @doc(FrontMod) -> atom()
%% where FrontMod = atom() | {frontend, atom()}
strip_frontend({frontend, Module}) -> Module;
strip_frontend(Module) when is_atom(Module) -> Module.


%%%
%%% Check options
%%%

check_listener_options(Opts) ->
    case includes_deprecated_ssl_option(Opts) of
	false -> ok;
	true ->
	    Error = "There is a problem with your ejabberd configuration file: "
		"the option 'ssl' for listening sockets is no longer available."
		" To get SSL encryption use the option 'tls'.",
	    throw({error, Error})
    end,
    case certfile_readable(Opts) of
	true -> ok;
	{false, Path} ->
            ErrorText = "There is a problem in the configuration: "
		"the specified file is not readable: ",
	    throw({error, ErrorText ++ Path})
    end,
    ok.

%% Parse the options of the socket,
%% and return if the deprecated option 'ssl' is included
%% @spec (Opts) -> true | false
includes_deprecated_ssl_option(Opts) ->
    case lists:keysearch(ssl, 1, Opts) of
	{value, {ssl, _SSLOpts}} ->
	    true;
	_ ->
	    lists:member(ssl, Opts)
    end.

%% @spec (Opts) -> true | {false, Path::string()}
certfile_readable(Opts) ->
    case proplists:lookup(certfile, Opts) of
	none -> true;
	{certfile, Path} ->
	    case ejabberd_config:is_file_readable(Path) of
		true -> true;
		false -> {false, Path}
	    end
    end.

get_proto(Opts) ->
    case proplists:get_value(proto, Opts) of
	undefined ->
	    tcp;
	Proto ->
	    normalize_proto(Proto)
    end.

normalize_proto(tcp) -> tcp;
normalize_proto(udp) -> udp;
normalize_proto(UnknownProto) ->
    ?WARNING_MSG("There is a problem in the configuration: "
		 "~p is an unknown IP protocol. Using tcp as fallback",
		 [UnknownProto]),
    tcp.

socket_error(Reason, PortIP, Module, SockOpts, Port, IPS) ->
    ReasonT = case Reason of
		  eaddrnotavail ->
		      "IP address not available: " ++ IPS;
		  eaddrinuse ->
		      "IP address and port number already used: "
			  ++IPS++" "++integer_to_list(Port);
		  _ ->
		      format_error(Reason)
	      end,
    ?ERROR_MSG("Failed to open socket:~n  ~p~nReason: ~s",
	       [{Port, Module, SockOpts}, ReasonT]),
    throw({Reason, PortIP}).

format_error(Reason) ->
    case inet:format_error(Reason) of
	"unknown POSIX error" ->
	    atom_to_list(Reason);
	ReasonStr ->
	    ReasonStr
    end.

%% Set interval between two accepts on given port
rate_limit([], _Interval) ->
    ok;
rate_limit([Port|Ports], Interval) ->
    rate_limit(Port, Interval),
    rate_limit(Ports, Interval);
rate_limit(Port, Interval) ->
    case get_listener_pid_by_port(Port) of
	undefined -> no_listener;
	Pid -> Pid ! {rate_limit, Interval}, ok
    end.

get_listener_pid_by_port(Port) ->
    ListenerPids = [Pid || {{P,_,_},Pid,_,_} <-
			       supervisor:which_children(erlang:whereis(ejabberd_listeners)),
			   P == Port],
    ListenerPid = case ListenerPids of
		      [] -> undefined;
		      [LPid|_] -> LPid
		  end,
    ListenerPid.

check_rate_limit(Interval) ->
    NewInterval = receive
		      {rate_limit, AcceptInterval} ->
			  AcceptInterval
		  after 0 ->
			  Interval
		  end,
    case NewInterval of
	0  -> ok;
	Ms ->
	    timer:sleep(Ms)
    end,
    NewInterval.
