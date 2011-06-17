%%%----------------------------------------------------------------------
%%% File    : randoms.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Random generation number wrapper
%%% Created : 13 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
-module(randoms).
-author('alexey@process-one.net').

-export([get_string/0]).

-export([start_link/0, init/1]).

-define(PROC_NAME, random_generator).

-spec start_link() -> {ok, pid()}.
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

-spec get_string() -> string().
get_string() ->
    ?PROC_NAME ! {self(), get_random, 65536*65536},
    receive
        {random, R} ->
            integer_to_list(R)
    end.

-spec init(pid()) -> no_return().
init(Parent) ->
    true = register(?PROC_NAME, self()),

    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),

    proc_lib:init_ack(Parent, {ok, self()}),

    loop().

-spec loop() -> no_return().
loop() ->
    receive
        {From, get_random, N} ->
            From ! {random, random:uniform(N)},
            loop();
        _ ->
            loop()
    end.
