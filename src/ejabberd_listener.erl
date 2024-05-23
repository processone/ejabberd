%%%----------------------------------------------------------------------
%%% File    : ejabberd_listener.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage socket listener
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
-behaviour(supervisor).
-author('alexey@process-one.net').
-author('ekhramtsov@process-one.net').

-export([start_link/0, init/1, stop/0, start/3, init/3,
	 start_listeners/0, start_listener/3, stop_listeners/0,
	 add_listener/3, delete_listener/2,
	 config_reloaded/0]).
-export([listen_options/0, listen_opt_type/1, validator/0]).
-export([tls_listeners/0]).

-include("logger.hrl").

-type transport() :: tcp | udp.
-type endpoint()  :: {inet:port_number(), inet:ip_address(), transport()}.
-type list_opts() :: [{atom(), term()}].
-type opts() :: #{atom() => term()}.
-type listener() :: {endpoint(), module(), opts()}.
-type sockmod() :: gen_tcp.
-type socket() :: inet:socket().
-type state() :: term().

-export_type([listener/0]).

-callback start(sockmod(), socket(), state()) ->
    {ok, pid()} | {error, any()} | ignore.
-callback start_link(sockmod(), socket(), state()) ->
    {ok, pid()} | {error, any()} | ignore.
-callback accept(pid()) -> any().
-callback listen_opt_type(atom()) -> econf:validator().
-callback listen_options() -> [{atom(), term()} | atom()].
-callback tcp_init(socket(), list_opts()) -> state().
-callback udp_init(socket(), list_opts()) -> state().

-optional_callbacks([listen_opt_type/1, tcp_init/2, udp_init/2]).

-define(TCP_SEND_TIMEOUT, 15000).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    _ = ets:new(?MODULE, [named_table, public]),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50),
    Listeners = ejabberd_option:listen(),
    {ok, {{one_for_one, 10, 1}, listeners_childspec(Listeners)}}.

stop() ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 50),
    stop_listeners(),
    ejabberd_sup:stop_child(?MODULE).

-spec listeners_childspec([listener()]) -> [supervisor:child_spec()].
listeners_childspec(Listeners) ->
    lists:map(
      fun({EndPoint, Module, Opts}) ->
	      ets:insert(?MODULE, {EndPoint, Module, Opts}),
	      {EndPoint,
	       {?MODULE, start, [EndPoint, Module, Opts]},
	       transient, brutal_kill, worker, [?MODULE]}
      end, Listeners).

-spec start_listeners() -> ok.
start_listeners() ->
    Listeners = ejabberd_option:listen(),
    lists:foreach(
      fun(Spec) ->
	      supervisor:start_child(?MODULE, Spec)
      end, listeners_childspec(Listeners)).

-spec start(endpoint(), module(), opts()) -> term().
start(EndPoint, Module, Opts) ->
    proc_lib:start_link(?MODULE, init, [EndPoint, Module, Opts]).

-spec init(endpoint(), module(), opts()) -> ok.
init({_, _, Transport} = EndPoint, Module, AllOpts) ->
    {ModuleOpts, SockOpts} = split_opts(Transport, AllOpts),
    init(EndPoint, Module, ModuleOpts, SockOpts).

-spec init(endpoint(), module(), opts(), [gen_tcp:option()]) -> ok.
init({Port, _, udp} = EndPoint, Module, Opts, SockOpts) ->
    {Port2, ExtraOpts} = case Port of
			     <<"unix:", Path/binary>> ->
				 SO = lists:keydelete(ip, 1, SockOpts),
                                 setup_provisional_udsocket_dir(Path),
				 file:delete(Path),
				 {0, [{ip, {local, Path}} | SO]};
			     _ ->
				 {Port, SockOpts}
			 end,
    ExtraOpts2 = lists:keydelete(send_timeout, 1, ExtraOpts),
    case gen_udp:open(Port2, [binary,
			     {active, false},
			     {reuseaddr, true} |
			     ExtraOpts2]) of
	{ok, Socket} ->
            set_definitive_udsocket(Port, Opts),
            misc:set_proc_label({?MODULE, udp, Port}),
	    case inet:sockname(Socket) of
		{ok, {Addr, Port1}} ->
		    proc_lib:init_ack({ok, self()}),
		    case application:ensure_started(ejabberd) of
			ok ->
			    ?INFO_MSG("Start accepting ~ts connections at ~ts for ~p",
				      [format_transport(udp, Opts),
				       format_endpoint({Port1, Addr, udp}), Module]),
			    Opts1 = opts_to_list(Module, Opts),
			    case erlang:function_exported(Module, udp_init, 2) of
				false ->
				    udp_recv(Socket, Module, Opts1);
				true ->
				    State = Module:udp_init(Socket, Opts1),
				    udp_recv(Socket, Module, State)
			    end;
			{error, _} ->
			    ok
		    end;
		{error, Reason} = Err ->
		    report_socket_error(Reason, EndPoint, Module),
		    proc_lib:init_ack(Err)
	    end;
	{error, Reason} = Err ->
	    report_socket_error(Reason, EndPoint, Module),
	    proc_lib:init_ack(Err)
    end;
init({Port, _, tcp} = EndPoint, Module, Opts, SockOpts) ->
    case listen_tcp(Port, SockOpts) of
	{ok, ListenSocket} ->
            set_definitive_udsocket(Port, Opts),
	    case inet:sockname(ListenSocket) of
		{ok, {Addr, Port1}} ->
		    proc_lib:init_ack({ok, self()}),
                    misc:set_proc_label({?MODULE, tcp, Port}),
		    case application:ensure_started(ejabberd) of
			ok ->
			    Sup = start_module_sup(Module, Opts),
			    Interval = maps:get(accept_interval, Opts),
			    Proxy = maps:get(use_proxy_protocol, Opts),
			    ?INFO_MSG("Start accepting ~ts connections at ~ts for ~p",
				      [format_transport(tcp, Opts),
				       format_endpoint({Port1, Addr, tcp}), Module]),
			    Opts1 = opts_to_list(Module, Opts),
			    case erlang:function_exported(Module, tcp_init, 2) of
				false ->
				    accept(ListenSocket, Module, Opts1, Sup, Interval, Proxy);
				true ->
				    State = Module:tcp_init(ListenSocket, Opts1),
				    accept(ListenSocket, Module, State, Sup, Interval, Proxy)
			    end;
			{error, _} ->
			    ok
		    end;
		{error, Reason} = Err ->
		    report_socket_error(Reason, EndPoint, Module),
		    proc_lib:init_ack(Err)
	    end;
	{error, Reason} = Err ->
	    report_socket_error(Reason, EndPoint, Module),
	    proc_lib:init_ack(Err)
    end.

-spec listen_tcp(inet:port_number(), [gen_tcp:option()]) ->
	        {ok, inet:socket()} | {error, system_limit | inet:posix()}.
listen_tcp(Port, SockOpts) ->
    {Port2, ExtraOpts} = case Port of
			     <<"unix:", Path/binary>> ->
				 SO = lists:keydelete(ip, 1, SockOpts),
                                 Prov = setup_provisional_udsocket_dir(Path),
				 file:delete(Path),
				 file:delete(Prov),
				 {0, [{ip, {local, Prov}} | SO]};
			     _ ->
				 {Port, SockOpts}
			 end,
    Res = gen_tcp:listen(Port2, [binary,
				{packet, 0},
				{active, false},
				{reuseaddr, true},
				{nodelay, true},
				{send_timeout_close, true},
				{keepalive, true} | ExtraOpts]),
    case Res of
	{ok, ListenSocket} ->
	    {ok, ListenSocket};
	{error, _} = Err ->
	    Err
    end.

%%%
%%% Unix Domain Socket utility functions
%%%

setup_provisional_udsocket_dir(DefinitivePath) ->
    ProvisionalPath = get_provisional_udsocket_path(DefinitivePath),
    SocketDir = filename:dirname(ProvisionalPath),
    file:make_dir(SocketDir),
    file:change_mode(SocketDir, 8#00700),
    ?DEBUG("Creating a Unix Domain Socket provisional file at ~ts for the definitive path ~s",
              [ProvisionalPath, DefinitivePath]),
    ProvisionalPath.

get_provisional_udsocket_path(Path) ->
    MnesiaDir = mnesia:system_info(directory),
    SocketDir = filename:join(MnesiaDir, "socket"),
    PathBase64 = misc:term_to_base64(Path),
    PathBuild = filename:join(SocketDir, PathBase64),
    %% Shorthen the path, a long path produces a crash when opening the socket.
    binary:part(PathBuild, {0, erlang:min(107, byte_size(PathBuild))}).

get_definitive_udsocket_path(<<"unix", _>> = Unix) ->
    Unix;
get_definitive_udsocket_path(ProvisionalPath) ->
    PathBase64 = filename:basename(ProvisionalPath),
    {term, Path} = misc:base64_to_term(PathBase64),
    Path.

set_definitive_udsocket(<<"unix:", Path/binary>>, Opts) ->
    Prov = get_provisional_udsocket_path(Path),
    timer:sleep(5000),
    Usd = maps:get(unix_socket, Opts),
    case maps:get(mode, Usd, undefined) of
        undefined -> ok;
        Mode -> ok = file:change_mode(Prov, Mode)
    end,
    case maps:get(owner, Usd, undefined) of
        undefined -> ok;
        Owner ->
            try
                ok = file:change_owner(Prov, Owner)
            catch
                error:{badmatch, {error, eperm}} ->
                    ?ERROR_MSG("Error trying to set owner ~p for socket ~p", [Owner, Prov]),
                    throw({error_setting_socket_owner, Owner, Prov})
            end
    end,
    case maps:get(group, Usd, undefined) of
        undefined -> ok;
        Group ->
            try
                ok = file:change_group(Prov, Group)
            catch
                error:{badmatch, {error, eperm}} ->
                    ?ERROR_MSG("Error trying to set group ~p for socket ~p", [Group, Prov]),
                    throw({error_setting_socket_group, Group, Prov})
            end
    end,
    file:rename(Prov, Path);
set_definitive_udsocket(_Port, _Opts) ->
    ok.

%%%
%%%
%%%

-spec split_opts(transport(), opts()) -> {opts(), [gen_tcp:option()]}.
split_opts(Transport, Opts) ->
    maps:fold(
      fun(Opt, Val, {ModOpts, SockOpts}) ->
	      case OptVal = {Opt, Val} of
		  {ip, _} ->
		      {ModOpts, [OptVal|SockOpts]};
		  {backlog, _} when Transport == tcp ->
		      {ModOpts, [OptVal|SockOpts]};
		  {backlog, _} ->
		      {ModOpts, SockOpts};
		  _ ->
		      {ModOpts#{Opt => Val}, SockOpts}
	      end
      end, {#{}, []}, Opts).

-spec accept(inet:socket(), module(), state(), atom(),
	     non_neg_integer(), boolean()) -> no_return().
accept(ListenSocket, Module, State, Sup, Interval, Proxy) ->
    Arity = case erlang:function_exported(Module, start, 3) of
		true -> 3;
		false -> 2
	    end,
    accept(ListenSocket, Module, State, Sup, Interval, Proxy, Arity).

-spec accept(inet:socket(), module(), state(), atom(),
	     non_neg_integer(), boolean(), 2|3) -> no_return().
accept(ListenSocket, Module, State, Sup, Interval, Proxy, Arity) ->
    NewInterval = apply_rate_limit(Interval),
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} when Proxy ->
	    case proxy_protocol:decode(gen_tcp, Socket, 10000) of
		{error, Err} ->
		    ?ERROR_MSG("(~w) Proxy protocol parsing failed: ~ts",
			       [ListenSocket, format_error(Err)]),
		    gen_tcp:close(Socket);
		{undefined, undefined} ->
		    gen_tcp:close(Socket);
		{{Addr, Port}, {PAddr, PPort}} = SP ->
		    %% THIS IS WRONG
		    State2 = [{sock_peer_name, SP} | State],
		    Receiver = case start_connection(Module, Arity, Socket, State2, Sup) of
				   {ok, RecvPid} ->
				       RecvPid;
				   _ ->
				       gen_tcp:close(Socket),
				       none
			       end,
		    ?INFO_MSG("(~p) Accepted proxied connection ~ts -> ~ts",
			      [Receiver,
			       ejabberd_config:may_hide_data(
				 format_endpoint({PPort, PAddr, tcp})),
			       format_endpoint({Port, Addr, tcp})])
	    end,
	    accept(ListenSocket, Module, State, Sup, NewInterval, Proxy, Arity);
	{ok, Socket} ->
	    case {inet:sockname(Socket), inet:peername(Socket)} of
		{{ok, {Addr, Port}}, {ok, {PAddr, PPort}}} ->
		    Receiver = case start_connection(Module, Arity, Socket, State, Sup) of
				   {ok, RecvPid} ->
				       RecvPid;
				   _ ->
				       gen_tcp:close(Socket),
				       none
			       end,
		    ?INFO_MSG("(~p) Accepted connection ~ts -> ~ts",
			      [Receiver,
			       ejabberd_config:may_hide_data(
				 format_endpoint({PPort, PAddr, tcp})),
			       format_endpoint({Port, Addr, tcp})]);
		_ ->
		    gen_tcp:close(Socket)
	    end,
	    accept(ListenSocket, Module, State, Sup, NewInterval, Proxy, Arity);
	{error, Reason} ->
	    ?ERROR_MSG("(~w) Failed TCP accept: ~ts",
		       [ListenSocket, format_error(Reason)]),
	    accept(ListenSocket, Module, State, Sup, NewInterval, Proxy, Arity)
    end.

-spec udp_recv(inet:socket(), module(), state()) -> no_return().
udp_recv(Socket, Module, State) ->
    case gen_udp:recv(Socket, 0) of
	{ok, {Addr, Port, Packet}} ->
	    case catch Module:udp_recv(Socket, Addr, Port, Packet, State) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("Failed to process UDP packet:~n"
			       "** Source: {~p, ~p}~n"
			       "** Reason: ~p~n** Packet: ~p",
			       [Addr, Port, Reason, Packet]),
		    udp_recv(Socket, Module, State);
		NewState ->
		    udp_recv(Socket, Module, NewState)
	    end;
	{error, Reason} ->
	    ?ERROR_MSG("Unexpected UDP error: ~ts", [format_error(Reason)]),
	    throw({error, Reason})
    end.

-spec start_connection(module(), 2|3, inet:socket(), state(), atom()) ->
		      {ok, pid()} | {error, any()} | ignore.
start_connection(Module, Arity, Socket, State, Sup) ->
    Res = case Sup of
	      undefined when Arity == 3 ->
		  Module:start(gen_tcp, Socket, State);
	      undefined ->
		  Module:start({gen_tcp, Socket}, State);
	      _ when Arity == 3 ->
		  supervisor:start_child(Sup, [gen_tcp, Socket, State]);
	      _ ->
		  supervisor:start_child(Sup, [{gen_tcp, Socket}, State])
	  end,
    case Res of
	{ok, Pid, preowned_socket} ->
	    Module:accept(Pid),
	    {ok, Pid};
	{ok, Pid} ->
	    case gen_tcp:controlling_process(Socket, Pid) of
		ok ->
		    Module:accept(Pid),
		    {ok, Pid};
		Err ->
		    case Sup of
			undefined ->
			    exit(Pid, kill);
			_ ->
			    supervisor:terminate_child(Sup, Pid)
		    end,
		    Err
	    end;
	Err ->
	    Err
    end.

-spec start_listener(endpoint(), module(), opts()) ->
		    {ok, pid()} | {error, any()}.
start_listener(EndPoint, Module, Opts) ->
    %% It is only required to start the supervisor in some cases.
    %% But it doesn't hurt to attempt to start it for any listener.
    %% So, it's normal (and harmless) that in most cases this
    %% call returns: {error, {already_started, pid()}}
    case start_listener_sup(EndPoint, Module, Opts) of
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

-spec start_module_sup(module(), opts()) -> atom().
start_module_sup(Module, Opts) ->
    case maps:get(supervisor, Opts) of
	true ->
	    Proc = list_to_atom(atom_to_list(Module) ++ "_sup"),
	    ChildSpec =	{Proc, {ejabberd_tmp_sup, start_link, [Proc, Module]},
			 permanent,
			 infinity,
			 supervisor,
			 [ejabberd_tmp_sup]},
	    case supervisor:start_child(ejabberd_sup, ChildSpec) of
		{ok, _} -> Proc;
		{error, {already_started, _}} -> Proc;
		_ -> undefined
	    end;
	false ->
	    undefined
    end.

-spec start_listener_sup(endpoint(), module(), opts()) ->
			{ok, pid()} | {error, any()}.
start_listener_sup(EndPoint, Module, Opts) ->
    ChildSpec = {EndPoint,
		 {?MODULE, start, [EndPoint, Module, Opts]},
		 transient,
		 brutal_kill,
		 worker,
		 [?MODULE]},
    supervisor:start_child(?MODULE, ChildSpec).

-spec stop_listeners() -> ok.
stop_listeners() ->
    Ports = ejabberd_option:listen(),
    lists:foreach(
      fun({PortIpNetp, Module, _Opts}) ->
	      delete_listener(PortIpNetp, Module)
      end,
      Ports).

-spec stop_listener(endpoint(), module(), opts()) -> ok | {error, any()}.
stop_listener({_, _, Transport} = EndPoint, Module, Opts) ->
    case supervisor:terminate_child(?MODULE, EndPoint) of
	ok ->
	    ?INFO_MSG("Stop accepting ~ts connections at ~ts for ~p",
		      [format_transport(Transport, Opts),
		       format_endpoint(EndPoint), Module]),
	    ets:delete(?MODULE, EndPoint),
	    supervisor:delete_child(?MODULE, EndPoint);
	Err ->
	    Err
    end.

-spec add_listener(endpoint(), module(), opts()) -> ok | {error, any()}.
add_listener(EndPoint, Module, Opts) ->
    Opts1 = apply_defaults(Module, Opts),
    case start_listener(EndPoint, Module, Opts1) of
	{ok, _Pid} ->
	    ok;
	{error, {already_started, _Pid}} ->
	    {error, {already_started, EndPoint}};
	{error, Error} ->
	    {error, Error}
    end.

-spec delete_listener(endpoint(), module()) -> ok | {error, any()}.
delete_listener(EndPoint, Module) ->
    try ets:lookup_element(?MODULE, EndPoint, 3) of
	Opts -> stop_listener(EndPoint, Module, Opts)
    catch _:badarg ->
	    ok
    end.

-spec tls_listeners() -> [module()].
tls_listeners() ->
    lists:usort(
      lists:filtermap(
	fun({_, Module, #{tls := true}}) -> {true, Module};
	   ({_, Module, #{starttls := true}}) -> {true, Module};
	   (_) -> false
	end, ets:tab2list(?MODULE))).

-spec config_reloaded() -> ok.
config_reloaded() ->
    New = ejabberd_option:listen(),
    Old = ets:tab2list(?MODULE),
    lists:foreach(
      fun({EndPoint, Module, Opts}) ->
	      case lists:keyfind(EndPoint, 1, New) of
		  false ->
		      stop_listener(EndPoint, Module, Opts);
		  _ ->
		      ok
	      end
      end, Old),
    lists:foreach(
      fun({EndPoint, Module, Opts}) ->
	      case lists:keyfind(EndPoint, 1, Old) of
		  {_, Module, Opts} ->
		      ok;
		  {_, OldModule, OldOpts} ->
		      _ = stop_listener(EndPoint, OldModule, OldOpts),
		      case start_listener(EndPoint, Module, Opts) of
			  {ok, _} ->
			      ets:insert(?MODULE, {EndPoint, Module, Opts});
			  _ ->
			      ok
		      end;
		  false ->
		      case start_listener(EndPoint, Module, Opts) of
			  {ok, _} ->
			      ets:insert(?MODULE, {EndPoint, Module, Opts});
			  _ ->
			      ok
		      end
	      end
      end, New).

-spec report_socket_error(inet:posix(), endpoint(), module()) -> ok.
report_socket_error(Reason, EndPoint, Module) ->
    ?ERROR_MSG("Failed to open socket at ~ts for ~ts: ~ts",
	       [format_endpoint(EndPoint), Module, format_error(Reason)]).

-spec format_error(inet:posix() | atom()) -> string().
format_error(Reason) ->
    case inet:format_error(Reason) of
	"unknown POSIX error" ->
	    atom_to_list(Reason);
	ReasonStr ->
	    ReasonStr
    end.

-spec format_endpoint(endpoint()) -> string().
format_endpoint({Port, IP, _Transport}) ->
    case Port of
        <<"unix:", _/binary>> ->
            Port;
	Unix when is_binary(Unix) ->
            Def = get_definitive_udsocket_path(Unix),
            <<"unix:", Def/binary>>;
	_ ->
	    IPStr = case tuple_size(IP) of
			4 -> inet:ntoa(IP);
			8 -> "[" ++ inet:ntoa(IP) ++ "]"
		    end,
	    IPStr ++ ":" ++ integer_to_list(Port)
    end.

-spec format_transport(transport(), opts()) -> string().
format_transport(Transport, Opts) ->
    case maps:get(tls, Opts, false) of
	true when Transport == tcp -> "TLS";
	true when Transport == udp -> "DTLS";
	false when Transport == tcp -> "TCP";
	false when Transport == udp -> "UDP"
    end.

-spec apply_rate_limit(non_neg_integer()) -> non_neg_integer().
apply_rate_limit(Interval) ->
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

-spec validator() -> econf:validator().
validator() ->
    econf:and_then(
      econf:list(
	econf:and_then(
	  econf:options(
	    #{module => listen_opt_type(module),
	      transport => listen_opt_type(transport),
	      '_' => econf:any()},
	    [{required, [module]}]),
	  fun(Opts) ->
		  M = proplists:get_value(module, Opts),
		  T = proplists:get_value(transport, Opts, tcp),
		  (validator(M, T))(Opts)
	  end)),
      fun prepare_opts/1).

-spec validator(module(), transport()) -> econf:validator().
validator(M, T) ->
    Options = listen_options() ++ M:listen_options(),
    Required = lists:usort([Opt || Opt <- Options, is_atom(Opt)]),
    Disallowed = if T == udp ->
			 [backlog, use_proxy_protocol, accept_interval];
		    true ->
			 []
		 end,
    Validator = maps:from_list(
		  lists:map(
		    fun(Opt) ->
			    try {Opt, M:listen_opt_type(Opt)}
			    catch _:_ when M /= ?MODULE ->
				    {Opt, listen_opt_type(Opt)}
			    end
		    end, proplists:get_keys(Options))),
    econf:options(
      Validator,
      [{required, Required}, {disallowed, Disallowed},
       {return, map}, unique]).

-spec prepare_opts([opts()]) -> [listener()].
prepare_opts(Listeners) ->
    check_overlapping_listeners(
      lists:map(
	fun(Opts1) ->
		{Opts2, Opts3} = partition(
				   fun({port, _}) -> true;
				      ({transport, _}) -> true;
				      ({module, _}) -> true;
				      (_) -> false
				   end, Opts1),
		Mod = maps:get(module, Opts2),
		Port = maps:get(port, Opts2),
		Transport = maps:get(transport, Opts2, tcp),
		IP = maps:get(ip, Opts3, {0,0,0,0}),
		Opts4 = apply_defaults(Mod, Opts3),
		{{Port, IP, Transport}, Mod, Opts4}
	end, Listeners)).

-spec check_overlapping_listeners([listener()]) -> [listener()].
check_overlapping_listeners(Listeners) ->
    _ = lists:foldl(
	  fun({{Port, IP, Transport} = Key, _, _}, Acc) ->
		  case lists:member(Key, Acc) of
		      true ->
			  econf:fail({listener_dup, {IP, Port}});
		      false ->
			  ZeroIP = case size(IP) of
				       8 -> {0,0,0,0,0,0,0,0};
				       4 -> {0,0,0,0}
				   end,
			  Key1 = {Port, ZeroIP, Transport},
			  case lists:member(Key1, Acc) of
			      true ->
				  econf:fail({listener_conflict,
					      {IP, Port}, {ZeroIP, Port}});
			      false ->
				  [Key|Acc]
			  end
		  end
	  end, [], Listeners),
    Listeners.

-spec apply_defaults(module(), opts()) -> opts().
apply_defaults(Mod, Opts) ->
    lists:foldl(
      fun({Opt, Default}, M) ->
	      case maps:is_key(Opt, M) of
		  true -> M;
		  false -> M#{Opt => Default}
	      end;
	 (_, M) ->
	      M
      end, Opts, Mod:listen_options() ++ listen_options()).

%% Convert options to list with removing defaults
-spec opts_to_list(module(), opts()) -> list_opts().
opts_to_list(Mod, Opts) ->
    Defaults = Mod:listen_options() ++ listen_options(),
    maps:fold(
      fun(Opt, Val, Acc) ->
	      case proplists:get_value(Opt, Defaults) of
		  Val -> Acc;
		  _ -> [{Opt, Val}|Acc]
	      end
      end, [], Opts).

-spec partition(fun(({atom(), term()}) -> boolean()), opts()) -> {opts(), opts()}.
partition(Fun, Opts) ->
    maps:fold(
      fun(Opt, Val, {True, False}) ->
	      case Fun({Opt, Val}) of
		  true -> {True#{Opt => Val}, False};
		  false -> {True, False#{Opt => Val}}
	      end
      end, {#{}, #{}}, Opts).

-spec listen_opt_type(atom()) -> econf:validator().
listen_opt_type(port) ->
    econf:either(
	econf:int(0, 65535),
	econf:binary("^unix:.*"));
listen_opt_type(module) ->
    econf:beam([[{start, 3}, {start, 2}],
		[{start_link, 3}, {start_link, 2}],
		{accept, 1}, {listen_options, 0}]);
listen_opt_type(ip) ->
    econf:ip();
listen_opt_type(transport) ->
    econf:enum([tcp, udp]);
listen_opt_type(accept_interval) ->
    econf:non_neg_int();
listen_opt_type(backlog) ->
    econf:non_neg_int();
listen_opt_type(supervisor) ->
    econf:bool();
listen_opt_type(ciphers) ->
    econf:binary();
listen_opt_type(dhfile) ->
    econf:file();
listen_opt_type(cafile) ->
    econf:pem();
listen_opt_type(certfile) ->
    econf:pem();
listen_opt_type(protocol_options) ->
    econf:and_then(
      econf:list(econf:binary()),
      fun(Options) -> str:join(Options, <<"|">>) end);
listen_opt_type(tls_compression) ->
    econf:bool();
listen_opt_type(tls) ->
    econf:bool();
listen_opt_type(max_stanza_size) ->
    econf:pos_int(infinity);
listen_opt_type(max_fsm_queue) ->
    econf:pos_int();
listen_opt_type(send_timeout) ->
    econf:timeout(second, infinity);
listen_opt_type(shaper) ->
    econf:shaper();
listen_opt_type(access) ->
    econf:acl();
listen_opt_type(unix_socket) ->
    econf:options(
      #{group => econf:non_neg_int(),
       owner => econf:non_neg_int(),
       mode => econf:octal()},
      [unique, {return, map}]);
listen_opt_type(use_proxy_protocol) ->
    econf:bool().

listen_options() ->
    [module, port,
     {transport, tcp},
     {ip, {0,0,0,0}},
     {accept_interval, 0},
     {send_timeout, 15000},
     {backlog, 128},
     {unix_socket, #{}},
     {use_proxy_protocol, false},
     {supervisor, true}].
