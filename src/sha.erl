%%%----------------------------------------------------------------------
%%% File    : sha.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 20 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(sha).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, init/0, sha/1]).

start() ->
    register(sha, spawn(?MODULE, init, [])).

init() ->
    ok = erl_ddll:load_driver(".", sha_erl),
    Port = open_port({spawn, sha_erl}, [binary]),
    loop(Port).

loop(Port) ->
    receive
	{From, {text, Str}} ->
	    Port ! {self(), {command, Str}},
	    SHA = receive
		      {Port, {data, Bin}} ->
			  binary_to_term(Bin)
		  end,
	    From ! {sha, SHA},
	    loop(Port)
    end.

sha(Text) ->
    sha ! {self(), {text, Text}},
    receive
	{sha, S} ->
	    S
    end.


