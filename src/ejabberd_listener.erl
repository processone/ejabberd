%%%----------------------------------------------------------------------
%%% File    : ejabberd_listener.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage socket listener
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_listener).

-behaviour(ejabberd_config).
-author('alexey@process-one.net').

-export([start_link/0, init/1, start/3, init/3,
	 start_listeners/0, start_listener/3, stop_listeners/0,
	 stop_listener/2, parse_listener_portip/2,
	 add_listener/3, delete_listener/2, transform_options/1,
	 validate_cfg/1, opt_type/1, config_reloaded/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    ets:new(?MODULE, [named_table, public]),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50),
    {ok, {{one_for_one, 10, 1}, listeners_childspec()}}.

listeners_childspec() ->
    Ls = ejabberd_config:get_option(listen, []),
    Specs = lists:map(
	      fun({Port, Module, Opts}) ->
		      ets:insert(?MODULE, {Port, Module, Opts}),
		      {Port,
		       {?MODULE, start, [Port, Module, Opts]},
		       transient,
		       brutal_kill,
		       worker,
		       [?MODULE]}
	      end, Ls),
    report_duplicated_portips(Ls),
    Specs.

start_listeners() ->
    lists:foreach(
      fun(Spec) ->
	      supervisor:start_child(?MODULE, Spec)
      end, listeners_childspec()).

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
    NewOpts = validate_module_options(Module, Opts),
    %% Check if the module is an ejabberd listener or an independent listener
    case Module:socket_type() of
	independent -> Module:start_listener(Port, NewOpts);
	_ -> start_dependent(Port, Module, NewOpts)
    end.

%% @spec(Port, Module, Opts) -> {ok, Pid} | {error, ErrorMessage}
start_dependent(Port, Module, Opts) ->
    proc_lib:start_link(?MODULE, init, [Port, Module, Opts]).

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
	    application:ensure_started(ejabberd),
	    start_module_sup(Port, Module),
	    ?INFO_MSG("Start accepting UDP connections at ~s for ~p",
		      [format_portip(PortIP), Module]),
	    case erlang:function_exported(Module, udp_init, 2) of
		false ->
		    udp_recv(Socket, Module, Opts);
		true ->
		    case catch Module:udp_init(Socket, Opts) of
			{'EXIT', _} = Err ->
			    ?ERROR_MSG("failed to process callback function "
				       "~p:~s(~p, ~p): ~p",
				       [Module, udp_init, Socket, Opts, Err]),
			    udp_recv(Socket, Module, Opts);
			NewOpts ->
			    udp_recv(Socket, Module, NewOpts)
		    end
	    end;
	{error, Reason} ->
	    socket_error(Reason, PortIP, Module, SockOpts, Port, IPS)
    end.

init_tcp(PortIP, Module, Opts, SockOpts, Port, IPS) ->
    ListenSocket = listen_tcp(PortIP, Module, SockOpts, Port, IPS),
    %% Inform my parent that this port was opened succesfully
    proc_lib:init_ack({ok, self()}),
    application:ensure_started(ejabberd),
    start_module_sup(Port, Module),
    ?INFO_MSG("Start accepting TCP connections at ~s for ~p",
	      [format_portip(PortIP), Module]),
    case erlang:function_exported(Module, tcp_init, 2) of
	false ->
	    accept(ListenSocket, Module, Opts);
	true ->
	    case catch Module:tcp_init(ListenSocket, Opts) of
		{'EXIT', _} = Err ->
		    ?ERROR_MSG("failed to process callback function "
			       "~p:~s(~p, ~p): ~p",
			       [Module, tcp_init, ListenSocket, Opts, Err]),
		    accept(ListenSocket, Module, Opts);
		NewOpts ->
		    accept(ListenSocket, Module, NewOpts)
	    end
    end.

listen_tcp(PortIP, Module, SockOpts, Port, IPS) ->
    Res = gen_tcp:listen(Port, [binary,
				{packet, 0},
				{active, false},
				{reuseaddr, true},
				{nodelay, true},
				{send_timeout, ?TCP_SEND_TIMEOUT},
				{send_timeout_close, true},
				{keepalive, true} |
				SockOpts]),
    case Res of
	{ok, ListenSocket} ->
	    ListenSocket;
	{error, Reason} ->
	    socket_error(Reason, PortIP, Module, SockOpts, Port, IPS)
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
    {IPVOpt, OptsClean} = case proplists:get_bool(inet6, Opts2) of
			      true -> {inet6, proplists:delete(inet6, Opts2)};
			      false -> {inet, Opts2}
			  end,
    {Port, IPT, IPS, Proto} =
	case add_proto(PortIP, Opts) of
	    {P, Prot} ->
		T = get_ip_tuple(IPOpt, IPVOpt),
		S = misc:ip_to_list(T),
		{P, T, S, Prot};
	    {P, T, Prot} when is_integer(P) and is_tuple(T) ->
		S = misc:ip_to_list(T),
		{P, T, S, Prot};
	    {P, S, Prot} when is_integer(P) and is_binary(S) ->
		[S | _] = str:tokens(S, <<"/">>),
		{ok, T} = inet_parse:address(binary_to_list(S)),
		{P, T, S, Prot}
	end,
    IPV = case tuple_size(IPT) of
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
	[{ip, T1} | _] ->
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
    IntervalOpt =
        case proplists:get_value(accept_interval, Opts) of
            [{linear, [I1_, T1_, T2_, I2_]}] ->
                {linear, I1_, T1_, T2_, I2_};
            I_ -> I_
        end,
    Interval =
        case IntervalOpt of
            undefined ->
                0;
            I when is_integer(I), I >= 0 ->
                I;
            {linear, I1, T1, T2, I2}
            when is_integer(I1),
                 is_integer(T1),
                 is_integer(T2),
                 is_integer(I2),
                 I1 >= 0,
                 I2 >= 0,
                 T2 > 0 ->
                {MSec, Sec, _USec} = os:timestamp(),
                TS = MSec * 1000000 + Sec,
                {linear, I1, TS + T1, T2, I2};
            I ->
                ?WARNING_MSG("There is a problem in the configuration: "
                             "~p is a wrong accept_interval value.  "
                             "Using 0 as fallback",
                             [I]),
                0
        end,
    accept(ListenSocket, Module, Opts, Interval).

accept(ListenSocket, Module, Opts, Interval) ->
    NewInterval = check_rate_limit(Interval),
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    case {inet:sockname(Socket), inet:peername(Socket)} of
		{{ok, {Addr, Port}}, {ok, {PAddr, PPort}}} ->
		    Receiver = case ejabberd_socket:start(Module,
							  gen_tcp, Socket, Opts) of
				   {ok, RecvPid} -> RecvPid;
				   _ -> none
			       end,
		    ?INFO_MSG("(~p) Accepted connection ~s:~p -> ~s:~p",
			      [Receiver,
			       ejabberd_config:may_hide_data(inet_parse:ntoa(PAddr)),
			       PPort, inet_parse:ntoa(Addr), Port]);
		_ ->
		    gen_tcp:close(Socket)
	    end,
	    accept(ListenSocket, Module, Opts, NewInterval);
	{error, Reason} ->
	    ?ERROR_MSG("(~w) Failed TCP accept: ~s",
                       [ListenSocket, inet:format_error(Reason)]),
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
			       [Addr, Port, Reason, Packet]),
		    udp_recv(Socket, Module, Opts);
		NewOpts ->
		    udp_recv(Socket, Module, NewOpts)
	    end;
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
    start_listener_sup(Port, Module, Opts).

start_module_sup(_Port, Module) ->
    Proc1 = gen_mod:get_module_proc(<<"sup">>, Module),
    ChildSpec1 =
	{Proc1,
	 {ejabberd_tmp_sup, start_link, [Proc1, Module]},
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
    supervisor:start_child(?MODULE, ChildSpec).

stop_listeners() ->
    Ports = ejabberd_config:get_option(listen, []),
    lists:foreach(
      fun({PortIpNetp, Module, _Opts}) ->
	      delete_listener(PortIpNetp, Module)
      end,
      Ports).

stop_listener({_, _, Transport} = PortIP, Module) ->
    case supervisor:terminate_child(?MODULE, PortIP) of
	ok ->
	    ?INFO_MSG("Stop accepting ~s connections at ~s for ~p",
		      [case Transport of udp -> "UDP"; tcp -> "TCP" end,
		       format_portip(PortIP), Module]),
	    ets:delete(?MODULE, PortIP),
	    supervisor:delete_child(?MODULE, PortIP);
	Err ->
	    Err
    end.

add_listener(PortIP, Module, Opts) ->
    {Port, IPT, _, _, Proto, _} = parse_listener_portip(PortIP, Opts),
    PortIP1 = {Port, IPT, Proto},
    case start_listener(PortIP1, Module, Opts) of
	{ok, _Pid} ->
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
    stop_listener(PortIP1, Module).

config_reloaded() ->
    New = ejabberd_config:get_option(listen, []),
    Old = ets:tab2list(?MODULE),
    lists:foreach(
      fun({PortIP, Module, _Opts}) ->
	      case lists:keyfind(PortIP, 1, New) of
		  false ->
		      stop_listener(PortIP, Module);
		  _ ->
		      ok
	      end
      end, Old),
    lists:foreach(
      fun({PortIP, Module, Opts}) ->
	      case lists:keyfind(PortIP, 1, Old) of
		  {_, Module, Opts} ->
		      ok;
		  {_, OldModule, _} ->
		      stop_listener(PortIP, OldModule),
		      ets:insert(?MODULE, {PortIP, Module, Opts}),
		      start_listener(PortIP, Module, Opts);
		  false ->
		      ets:insert(?MODULE, {PortIP, Module, Opts}),
		      start_listener(PortIP, Module, Opts)
	      end
      end, New).

%%%
%%% Check options
%%%
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
		      "IP address not available: " ++ binary_to_list(IPS);
		  eaddrinuse ->
		      "IP address and port number already used: "
			  ++binary_to_list(IPS)++" "++integer_to_list(Port);
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

format_portip({Port, IP, _Transport}) ->
    IPStr = case tuple_size(IP) of
		4 -> inet:ntoa(IP);
		8 -> "[" ++ inet:ntoa(IP) ++ "]"
	    end,
    IPStr ++ ":" ++ integer_to_list(Port).

check_rate_limit(Interval) ->
    NewInterval = receive
		      {rate_limit, AcceptInterval} ->
			  AcceptInterval
		  after 0 ->
			  Interval
		  end,
    case NewInterval of
	0  -> ok;
	Ms when is_integer(Ms) ->
	    timer:sleep(Ms);
        {linear, I1, T1, T2, I2} ->
            {MSec, Sec, _USec} = os:timestamp(),
            TS = MSec * 1000000 + Sec,
            I =
                if
                    TS =< T1 -> I1;
                    TS >= T1 + T2 -> I2;
                    true ->
                        round((I2 - I1) * (TS - T1) / T2 + I1)
                end,
            timer:sleep(I)
    end,
    NewInterval.

-define(IS_CHAR(C), (is_integer(C) and (C >= 0) and (C =< 255))).
-define(IS_UINT(U), (is_integer(U) and (U >= 0) and (U =< 65535))).
-define(IS_PORT(P), (is_integer(P) and (P > 0) and (P =< 65535))).
-define(IS_TRANSPORT(T), ((T == tcp) or (T == udp))).

transform_option({{Port, IP, Transport}, Mod, Opts}) ->
    IPStr = if is_tuple(IP) ->
                    list_to_binary(inet_parse:ntoa(IP));
               true ->
                    IP
            end,
    Opts1 = lists:map(
              fun({ip, IPT}) when is_tuple(IPT) ->
                      {ip, list_to_binary(inet_parse:ntoa(IP))};
                 (ssl) -> {tls, true};
		 (A) when is_atom(A) -> {A, true};
                 (Opt) -> Opt
              end, Opts),
    Opts2 = lists:foldl(
              fun(Opt, Acc) ->
                      try
                          Mod:transform_listen_option(Opt, Acc)
                      catch error:undef ->
                              [Opt|Acc]
                      end
              end, [], Opts1),
    TransportOpt = if Transport == tcp -> [];
                      true -> [{transport, Transport}]
                   end,
    IPOpt = if IPStr == <<"0.0.0.0">> -> [];
               true -> [{ip, IPStr}]
            end,
    IPOpt ++ TransportOpt ++ [{port, Port}, {module, Mod} | Opts2];
transform_option({{Port, Transport}, Mod, Opts})
  when ?IS_TRANSPORT(Transport) ->
    transform_option({{Port, all_zero_ip(Opts), Transport}, Mod, Opts});
transform_option({{Port, IP}, Mod, Opts}) ->
    transform_option({{Port, IP, tcp}, Mod, Opts});
transform_option({Port, Mod, Opts}) ->
    transform_option({{Port, all_zero_ip(Opts), tcp}, Mod, Opts});
transform_option(Opt) ->
    Opt.

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({listen, LOpts}, Opts) ->
    [{listen, lists:map(fun transform_option/1, LOpts)} | Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

-spec validate_module_options(module(), [{atom(), any()}]) -> [{atom(), any()}].
validate_module_options(Module, Opts) ->
    try Module:listen_opt_type('') of
	_ ->
	    lists:filtermap(
	      fun({Opt, Val}) ->
		      case validate_module_option(Module, Opt, Val) of
			  {ok, NewVal} -> {true, {Opt, NewVal}};
			  error -> false
		      end
	      end, Opts)
    catch _:undef ->
	    ?WARNING_MSG("module '~s' doesn't export listen_opt_type/1",
			 [Module]),
	    Opts
    end.

-spec validate_module_option(module(), atom(), any()) -> {ok, any()} | error.
validate_module_option(Module, Opt, Val) ->
    case Module:listen_opt_type(Opt) of
	VFun when is_function(VFun) ->
	    try VFun(Val) of
		NewVal -> {ok, NewVal}
	    catch {invalid_syntax, Error} ->
		    ?ERROR_MSG("ignoring listen option '~s' with "
			       "invalid value: ~p: ~s",
			       [Opt, Val, Error]),
		    error;
		  _:_ ->
		    ?ERROR_MSG("ignoring listen option '~s' with "
			       "invalid value: ~p",
			       [Opt, Val]),
		    error
	    end;
	KnownOpts when is_list(KnownOpts) ->
	    ?ERROR_MSG("unknown listen option '~s' for '~s' will be likely "
		       "ignored, available options are: ~s",
		       [Opt, Module, misc:join_atoms(KnownOpts, <<", ">>)]),
	    {ok, Val}
    end.

-type transport() :: udp | tcp.
-type port_ip_transport() :: inet:port_number() |
                             {inet:port_number(), transport()} |
                             {inet:port_number(), inet:ip_address()} |
                             {inet:port_number(), inet:ip_address(),
                              transport()}.
-spec validate_cfg(list()) -> [{port_ip_transport(), module(), list()}].

validate_cfg(L) ->
    lists:map(
      fun(LOpts) ->
              lists:foldl(
                fun({port, Port}, {{_, IP, T}, Mod, Opts}) ->
                        true = ?IS_PORT(Port),
                        {{Port, IP, T}, Mod, Opts};
                   ({ip, IP}, {{Port, _, T}, Mod, Opts}) ->
                        {{Port, prepare_ip(IP), T}, Mod, Opts};
                   ({transport, T}, {{Port, IP, _}, Mod, Opts}) ->
                        true = ?IS_TRANSPORT(T),
                        {{Port, IP, T}, Mod, Opts};
                   ({module, Mod}, {Port, _, Opts}) ->
                        {Port, Mod, Opts};
                   (Opt, {Port, Mod, Opts}) ->
                        {Port, Mod, [Opt|Opts]}
                end, {{5222, all_zero_ip(LOpts), tcp}, ejabberd_c2s, []}, LOpts)
      end, L).

prepare_ip({A, B, C, D} = IP)
  when ?IS_CHAR(A) and ?IS_CHAR(B) and ?IS_CHAR(C) and ?IS_CHAR(D) ->
    IP;
prepare_ip({A, B, C, D, E, F, G, H} = IP)
  when ?IS_UINT(A) and ?IS_UINT(B) and ?IS_UINT(C) and ?IS_UINT(D)
       and ?IS_UINT(E) and ?IS_UINT(F) and ?IS_UINT(G) and ?IS_UINT(H) ->
    IP;
prepare_ip(IP) when is_list(IP) ->
    {ok, Addr} = inet_parse:address(IP),
    Addr;
prepare_ip(IP) when is_binary(IP) ->
    prepare_ip(binary_to_list(IP)).

all_zero_ip(Opts) ->
    case proplists:get_bool(inet6, Opts) of
	true -> {0,0,0,0,0,0,0,0};
	false -> {0,0,0,0}
    end.

opt_type(listen) -> fun validate_cfg/1;
opt_type(_) -> [listen].
