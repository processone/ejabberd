%%%----------------------------------------------------------------------
%%% File    : str.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Provide binary string manipulations
%%% Created : 23 Feb 2012 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(str).

-author('ekhramtsov@process-one.net').

%% API
-export([equal/2,
         concat/2,
         rchr/2,
         str/2,
         rstr/2,
         span/2,
         cspan/2,
         copies/2,
         words/1,
         words/2,
         sub_word/2,
         sub_word/3,
         strip/1,
         strip/2,
         len/1,
         tokens/2,
         left/2,
         left/3,
         right/2,
         right/3,
         centre/2,
         centre/3,
         sub_string/2,
         sub_string/3,
         to_upper/1,
         join/2,
         substr/2,
         chr/2,
         chars/3,
         chars/2,
         substr/3,
         strip/3,
         to_lower/1,
         to_float/1,
         prefix/2,
         suffix/2,
         to_integer/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec len(binary()) -> non_neg_integer().

len(B) ->
    byte_size(B).

-spec equal(binary(), binary()) -> boolean().

equal(B1, B2) ->
    B1 == B2.

-spec concat(binary(), binary()) -> binary().

concat(B1, B2) ->
    <<B1/binary, B2/binary>>.

-spec rchr(binary(), char()) -> non_neg_integer().

rchr(B, C) ->
    string:rchr(binary_to_list(B), C).

-spec str(binary(), binary()) -> non_neg_integer().

str(B1, B2) ->
    string:str(binary_to_list(B1), binary_to_list(B2)).

-spec rstr(binary(), binary()) -> non_neg_integer().

rstr(B1, B2) ->
    string:rstr(binary_to_list(B1), binary_to_list(B2)).

-spec span(binary(), binary()) -> non_neg_integer().

span(B1, B2) ->
    string:span(binary_to_list(B1), binary_to_list(B2)).

-spec cspan(binary(), binary()) -> non_neg_integer().

cspan(B1, B2) ->
    string:cspan(binary_to_list(B1), binary_to_list(B2)).

-spec copies(binary(), non_neg_integer()) -> binary().

copies(B, N) ->
    iolist_to_binary(string:copies(binary_to_list(B), N)).

-spec words(binary()) -> pos_integer().

words(B) ->
    string:words(binary_to_list(B)).

-spec words(binary(), char()) -> pos_integer().

words(B, C) ->
    string:words(binary_to_list(B), C).

-spec sub_word(binary(), integer()) -> binary().

sub_word(B, N) ->
    iolist_to_binary(string:sub_word(binary_to_list(B), N)).

-spec sub_word(binary(), integer(), char()) -> binary().

sub_word(B, N, C) ->
    iolist_to_binary(string:sub_word(binary_to_list(B), N, C)).

-spec strip(binary()) -> binary().

strip(B) ->
    iolist_to_binary(string:strip(binary_to_list(B))).

-spec strip(binary(), both | left | right) -> binary().

strip(B, D) ->
    iolist_to_binary(string:strip(binary_to_list(B), D)).

-spec left(binary(), non_neg_integer()) -> binary().

left(B, N) ->
    iolist_to_binary(string:left(binary_to_list(B), N)).

-spec left(binary(), non_neg_integer(), char()) -> binary().

left(B, N, C) ->
    iolist_to_binary(string:left(binary_to_list(B), N, C)).

-spec right(binary(), non_neg_integer()) -> binary().

right(B, N) ->
    iolist_to_binary(string:right(binary_to_list(B), N)).

-spec right(binary(), non_neg_integer(), char()) -> binary().

right(B, N, C) ->
    iolist_to_binary(string:right(binary_to_list(B), N, C)).

-spec centre(binary(), non_neg_integer()) -> binary().

centre(B, N) ->
    iolist_to_binary(string:centre(binary_to_list(B), N)).

-spec centre(binary(), non_neg_integer(), char()) -> binary().

centre(B, N, C) ->
    iolist_to_binary(string:centre(binary_to_list(B), N, C)).

-spec sub_string(binary(), pos_integer()) -> binary().

sub_string(B, N) ->
    iolist_to_binary(string:sub_string(binary_to_list(B), N)).

-spec sub_string(binary(), pos_integer(), pos_integer()) -> binary().

sub_string(B, S, E) ->
    iolist_to_binary(string:sub_string(binary_to_list(B), S, E)).

-spec to_upper(binary()) -> binary();
              (char()) -> char().

to_upper(B) when is_binary(B) ->
    iolist_to_binary(string:to_upper(binary_to_list(B)));
to_upper(C) ->
    string:to_upper(C).

-spec join([binary()], binary() | char()) -> binary().

join(L, Sep) ->
    iolist_to_binary(join_s(L, Sep)).

-spec substr(binary(), pos_integer()) -> binary().

substr(B, N) ->
    iolist_to_binary(string:substr(binary_to_list(B), N)).

-spec chr(binary(), char()) -> non_neg_integer().

chr(B, C) ->
    string:chr(binary_to_list(B), C).

-spec chars(char(), non_neg_integer(), binary()) -> binary().

chars(C, N, B) ->
    iolist_to_binary(string:chars(C, N, binary_to_list(B))).

-spec chars(char(), non_neg_integer()) -> binary().

chars(C, N) ->
    iolist_to_binary(string:chars(C, N)).

-spec substr(binary(), pos_integer(), non_neg_integer()) -> binary().

substr(B, S, E) ->
    iolist_to_binary(string:substr(binary_to_list(B), S, E)).

-spec strip(binary(), both | left | right, char()) -> binary().

strip(B, D, C) ->
    iolist_to_binary(string:strip(binary_to_list(B), D, C)).

-spec to_lower(binary()) -> binary();
              (char()) -> char().

to_lower(B) when is_binary(B) ->
    iolist_to_binary(string:to_lower(binary_to_list(B)));
to_lower(C) ->
    string:to_lower(C).

-spec tokens(binary(), binary()) -> [binary()].

tokens(B1, B2) ->
    [iolist_to_binary(T) ||
        T <- string:tokens(binary_to_list(B1), binary_to_list(B2))].

-spec to_float(binary()) -> {float(), binary()} | {error, no_float}.

to_float(B) ->
    case string:to_float(binary_to_list(B)) of
        {error, R} ->
            {error, R};
        {Float, Rest} ->
            {Float, iolist_to_binary(Rest)}
    end.

-spec to_integer(binary()) -> {integer(), binary()} | {error, no_integer}.

to_integer(B) ->
    case string:to_integer(binary_to_list(B)) of
        {error, R} ->
            {error, R};
        {Int, Rest} ->
            {Int, iolist_to_binary(Rest)}
    end.

-spec prefix(binary(), binary()) -> boolean().

prefix(Prefix, B) ->
    Size = byte_size(Prefix),
    case B of
        <<Prefix:Size/binary, _/binary>> ->
            true;
        _ ->
            false
    end.

-spec suffix(binary(), binary()) -> boolean().

suffix(B1, B2) ->
    lists:suffix(binary_to_list(B1), binary_to_list(B2)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
join_s([], _Sep) ->
    [];
join_s([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].
