%%%----------------------------------------------------------------------
%%% File    : extauth.erl
%%% Author  : Leif Johansson <leifj@it.su.se>
%%% Purpose : External authentication using a simple port-driver
%%% Created : 30 Jul 2004 by Leif Johansson <leifj@it.su.se>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(extauth).
-author('leifj@it.su.se').

-export([start/1, stop/0, init/1,
	 check_password/2, set_password/2, is_user_exists/1 ]).


start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

init(ExtPrg) ->
    register(eauth,self()),
    process_flag(trap_exit,true),
    Port = open_port({spawn, ExtPrg}, [{packet,2}]),
    loop(Port).

stop() ->
    eauth ! stop.

check_password(User,Password) ->
    call_port(["auth",User,Password]).

is_user_exists(User) ->
    call_port(["isuser",User]).

set_password(User,Password) ->
    call_port(["setpass",User,Password]).

call_port(Msg) ->
    eauth ! {call, self(), Msg},
    receive
	{eauth,Result}->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {eauth, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("~p ~n", [Reason]),
	    exit(port_terminated)
    end.

join(List, Sep) ->
    lists:foldl(fun(A, "") -> A;
		   (A, Acc) -> Acc ++ Sep ++ A
		end, "", List).

encode(L) ->
    join(L,":").

decode([0,0]) ->
    false;
decode([0,1]) ->
    true.

