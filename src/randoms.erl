%%%----------------------------------------------------------------------
%%% File    : randoms.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 13 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(randoms).
-author('alexey@sevcom.net').

-export([get_string/0]).

-export([start/0, init/0]).


start() ->
    register(random_generator, spawn(randoms, init, [])).

init() ->
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    loop().

loop() ->
    receive
	{From, get_random, N} ->
	    From ! {random, random:uniform(N)},
	    loop();
	_ ->
	    loop()
    end.


get_string() ->
    random_generator ! {self(), get_random, 65536*65536},
    receive
	{random, R} ->
	    integer_to_list(R)
    end.

