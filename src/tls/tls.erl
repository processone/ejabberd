%%%----------------------------------------------------------------------
%%% File    : tls.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Interface to openssl
%%% Created : 24 Jul 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(tls).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_server).

-export([start/0, start_link/0,
	 tcp_to_tls/2, tls_to_tcp/1,
	 send/2,
	 recv/2, recv/3,
	 close/1,
	 test/0]).

%% Internal exports, call-back functions.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-define(SET_CERTIFICATE_FILE, 1).
-define(SET_ENCRYPTED_INPUT,  2).
-define(SET_DECRYPTED_OUTPUT, 3).
-define(GET_ENCRYPTED_OUTPUT, 4).
-define(GET_DECRYPTED_INPUT,  5).

-record(tlssock, {tcpsock, tlsport}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = erl_ddll:load_driver(ejabberd:get_so_path(), tls_drv),
    Port = open_port({spawn, tls_drv}, [binary]),
    Res = port_control(Port, ?SET_CERTIFICATE_FILE, "./ssl.pem" ++ [0]),
    case Res of
	<<0>> ->
	    %ets:new(iconv_table, [set, public, named_table]),
	    %ets:insert(iconv_table, {port, Port}),
	    {ok, Port};
	<<1, Error/binary>> ->
	    {error, binary_to_list(Error)}
    end.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, Port) ->
    {noreply, Port};

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_died, Reason}, Port};
handle_info(_, State) ->
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(_Reason, Port) ->
    Port ! {self, close},
    ok.


tcp_to_tls(TCPSocket, Options) ->
    case lists:keysearch(certfile, 1, Options) of
	{value, {certfile, CertFile}} ->
	    ok = erl_ddll:load_driver(ejabberd:get_so_path(), tls_drv),
	    Port = open_port({spawn, tls_drv}, [binary]),
	    io:format("open_port: ~p~n", [Port]),
	    case port_control(Port, ?SET_CERTIFICATE_FILE,
			      CertFile ++ [0]) of
		<<0>> ->
		    {ok, #tlssock{tcpsock = TCPSocket, tlsport = Port}};
		<<1, Error/binary>> ->
		    {error, binary_to_list(Error)}
	    end;
	false ->
	    {error, no_certfile}
    end.
    
tls_to_tcp(#tlssock{tcpsock = TCPSocket, tlsport = Port}) ->
    port_close(Port),
    TCPSocket.

recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#tlssock{tcpsock = TCPSocket, tlsport = Port}, Length, Timeout) ->
    case gen_tcp:recv(TCPSocket, Length, Timeout) of
	{ok, Packet} ->
	    case port_control(Port, ?SET_ENCRYPTED_INPUT, Packet) of
		<<0>> ->
		    case port_control(Port, ?GET_DECRYPTED_INPUT, []) of
			<<0, In/binary>> ->
			    case port_control(Port, ?GET_ENCRYPTED_OUTPUT, []) of
				<<0, Out/binary>> ->
				    case gen_tcp:send(TCPSocket, Out) of
					ok ->
					    {ok, In};
					Error ->
					    Error
				    end;
				<<1, Error/binary>> ->
				    {error, binary_to_list(Error)}
			    end;
			<<1, Error/binary>> ->
			    {error, binary_to_list(Error)}
		    end;
		<<1, Error/binary>> ->
		    {error, binary_to_list(Error)}
	    end;
	{error, _Reason} = Error ->
	    Error
    end.

send(#tlssock{tcpsock = TCPSocket, tlsport = Port}, Packet) ->
    case port_control(Port, ?SET_DECRYPTED_OUTPUT, Packet) of
	<<0>> ->
	    case port_control(Port, ?GET_ENCRYPTED_OUTPUT, []) of
		<<0, Out/binary>> ->
		    gen_tcp:send(TCPSocket, Out);
		<<1, Error/binary>> ->
		    {error, binary_to_list(Error)}
	    end;
	<<1, Error/binary>> ->
	    {error, binary_to_list(Error)}
    end.


close(#tlssock{tcpsock = TCPSocket, tlsport = Port}) ->
    gen_tcp:close(TCPSocket),
    port_close(Port).


test() ->
    ok = erl_ddll:load_driver(ejabberd:get_so_path(), tls_drv),
    Port = open_port({spawn, tls_drv}, [binary]),
    io:format("open_port: ~p~n", [Port]),
    PCRes = port_control(Port, ?SET_CERTIFICATE_FILE, "./ssl.pem" ++ [0]),
    io:format("port_control: ~p~n", [PCRes]),
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary,
					       {packet, 0}, 
					       {active, true},
					       {reuseaddr, true},
					       {nodelay, true}]),
    io:format("listen: ~p~n", [ListenSocket]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("accept: ~p~n", [Socket]),
    loop(Port, Socket).


loop(Port, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    %io:format("read: ~p~n", [Data]),
	    Res = port_control(Port, ?SET_ENCRYPTED_INPUT, Data),
	    io:format("SET_ENCRYPTED_INPUT: ~p~n", [Res]),

	    DIRes = port_control(Port, ?GET_DECRYPTED_INPUT, Data),
	    io:format("GET_DECRYPTED_INPUT: ~p~n", [DIRes]),
	    case DIRes of
		<<0, In/binary>> ->
		    io:format("input: ~s~n", [binary_to_list(In)]);
		<<1, DIError/binary>> ->
		    io:format("GET_DECRYPTED_INPUT error: ~p~n", [binary_to_list(DIError)])
	    end,

	    EORes = port_control(Port, ?GET_ENCRYPTED_OUTPUT, Data),
	    io:format("GET_ENCRYPTED_OUTPUT: ~p~n", [EORes]),
	    case EORes of
		<<0, Out/binary>> ->
		    gen_tcp:send(Socket, Out);
		<<1, EOError/binary>> ->
		    io:format("GET_ENCRYPTED_OUTPUT error: ~p~n", [binary_to_list(EOError)])
	    end,
		    

	    loop(Port, Socket);
	Msg ->
	    io:format("receive: ~p~n", [Msg]),
	    loop(Port, Socket)
    end.


