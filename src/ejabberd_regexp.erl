%%%----------------------------------------------------------------------
%%% File    : ejabberd_regexp.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Frontend to Re OTP module
%%% Created : 8 Dec 2011 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-module(ejabberd_regexp).

-export([run/2, split/2, replace/3, greplace/3, sh_to_awk/1]).


-spec run(binary(), binary()) -> match | nomatch | {error, any()}.

run(String, Regexp) ->
    re:run(String, Regexp, [{capture, none}, unicode]).


-spec split(binary(), binary()) -> [binary()].

split(String, Regexp) ->
    re:split(String, Regexp, [{return, binary}]).


-spec replace(binary(), binary(), binary()) -> binary().

replace(String, Regexp, New) ->
    re:replace(String, Regexp, New, [{return, binary}]).


-spec greplace(binary(), binary(), binary()) -> binary().

greplace(String, Regexp, New) ->
    re:replace(String, Regexp, New, [global, {return, binary}]).


%% This code was copied and adapted from xmerl_regexp.erl


-spec sh_to_awk(binary()) -> binary().
sh_to_awk(Sh) ->
    iolist_to_binary([<<"^(">>, sh_to_awk_1(Sh)]).  %Fix the beginning


sh_to_awk_1(<<"*", Sh/binary>>) ->  %This matches any string
    [<<".*">>, sh_to_awk_1(Sh)];
sh_to_awk_1(<<"?", Sh/binary>>) ->  %This matches any character
    [$., sh_to_awk_1(Sh)];
sh_to_awk_1(<<"[^]", Sh/binary>>) ->  %This takes careful handling
    [<<"\\^">>, sh_to_awk_1(Sh)];
%% Must move '^' to end.
sh_to_awk_1(<<"[^", Sh/binary>>) ->
    [$[, sh_to_awk_2(Sh, true)];
sh_to_awk_1(<<"[!", Sh/binary>>) ->
    [<<"[^">>, sh_to_awk_2(Sh, false)];
sh_to_awk_1(<<"[", Sh/binary>>) ->
    [$[, sh_to_awk_2(Sh, false)];
sh_to_awk_1(<<C:8, Sh/binary>>) ->  %% Unspecialise everything else which is not an escape character.
    case sh_special_char(C) of
        true -> [$\\, C | sh_to_awk_1(Sh)];
        false -> [C | sh_to_awk_1(Sh)]
    end;
sh_to_awk_1(<<>>) ->
    <<")$">>.  %Fix the end


sh_to_awk_2(<<"]", Sh/binary>>, UpArrow) ->
    [$] | sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_2(Sh, UpArrow) ->
    sh_to_awk_3(Sh, UpArrow).


sh_to_awk_3(<<"]", Sh/binary>>, true) ->
    [<<"^]">>, sh_to_awk_1(Sh)];
sh_to_awk_3(<<"]", Sh/binary>>, false) ->
    [$] | sh_to_awk_1(Sh)];
sh_to_awk_3(<<C:8, Sh/binary>>, UpArrow) ->
    [C | sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_3(<<>>, true) ->
    [$^ | sh_to_awk_1(<<>>)];
sh_to_awk_3(<<>>, false) ->
    sh_to_awk_1(<<>>).


%%  Test if a character is a special character.
-spec sh_special_char(char()) -> boolean().
sh_special_char($|) -> true;
sh_special_char($*) -> true;
sh_special_char($+) -> true;
sh_special_char($?) -> true;
sh_special_char($() -> true;
sh_special_char($)) -> true;
sh_special_char($\\) -> true;
sh_special_char($^) -> true;
sh_special_char($$) -> true;
sh_special_char($.) -> true;
sh_special_char($[) -> true;
sh_special_char($]) -> true;
sh_special_char($") -> true;
sh_special_char(_C) -> false.
