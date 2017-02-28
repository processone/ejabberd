%%%----------------------------------------------------------------------
%%% File    : randoms.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Random generation number wrapper
%%% Created : 13 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(randoms).

-author('alexey@process-one.net').

-export([get_string/0, uniform/0, uniform/1, bytes/1]).

-define(THRESHOLD, 16#10000000000000000).

get_string() ->
    R = crypto:rand_uniform(0, ?THRESHOLD),
    integer_to_binary(R).

uniform() ->
    crypto:rand_uniform(0, ?THRESHOLD)/?THRESHOLD.

uniform(N) ->
    crypto:rand_uniform(1, N+1).

-ifdef(STRONG_RAND_BYTES).
bytes(N) ->
    crypto:strong_rand_bytes(N).
-else.
bytes(N) ->
    crypto:rand_bytes(N).
-endif.
