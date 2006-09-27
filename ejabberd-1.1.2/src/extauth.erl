%%%----------------------------------------------------------------------
%%% File    : extauth.erl
%%% Author  : Leif Johansson <leifj@it.su.se>
%%% Purpose : External authentication using a simple port-driver
%%% Created : 30 Jul 2004 by Leif Johansson <leifj@it.su.se>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(extauth).
-author('leifj@it.su.se').

-export([start/2, stop/1, init/2,
	 check_password/3, set_password/3, is_user_exists/2]).


start(Host, ExtPrg) ->
    spawn(?MODULE, init, [Host, ExtPrg]).

init(Host, ExtPrg) ->
    register(gen_mod:get_module_proc(Host, eauth), self()),
    process_flag(trap_exit,true),
    Port = open_port({spawn, ExtPrg}, [{packet,2}]),
    loop(Port).

stop(Host) ->
    gen_mod:get_module_proc(Host, eauth) ! stop.

check_password(User, Server, Password) ->
    call_port(Server, ["auth", User, Server, Password]).

is_user_exists(User, Server) ->
    call_port(Server, ["isuser", User, Server]).

set_password(User, Server, Password) ->
    call_port(Server, ["setpass", User, Server, Password]).

call_port(Server, Msg) ->
    LServer = jlib:nameprep(Server),
    gen_mod:get_module_proc(LServer, eauth) ! {call, self(), Msg},
    receive
	{eauth,Result} ->
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

