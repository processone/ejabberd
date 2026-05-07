%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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

%% This uses Macros Overloading, see
%% https://www.erlang.org/doc/system/macros.html#macros-overloading

%% @format-begin

-define(CATCH_MFA(Module, Function, Arguments),
        try apply(Module, Function, Arguments)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p:~p~p~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Module, Function, Arguments, TCX, TCY]),
                {TCX, TCY}
        end).

-define(CATCH_TRY(Function, A1),
        try Function(A1)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p(~p)~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Function, A1, TCX, TCY]),
                {TCX, TCY}
        end).

-define(CATCH_TRY(Function, A1, A2),
        try Function(A1, A2)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p(~p, ~p)~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Function, A1, A2, TCX, TCY]),
                {TCX, TCY}
        end).

-define(CATCH_TRY(Function, A1, A2, A3),
        try Function(A1, A2, A3)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p(~p, ~p, ~p)~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Function, A1, A2, A3, TCX, TCY]),
                {TCX, TCY}
        end).

-define(CATCH_TRY(Function, A1, A2, A3, A4),
        try Function(A1, A2, A3, A4)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p(~p, ~p, ~p, ~p)~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Function, A1, A2, A3, A4, TCX, TCY]),
                {TCX, TCY}
        end).

-define(CATCH_TRY(Function, A1, A2, A3, A4, A5),
        try Function(A1, A2, A3, A4, A5)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p(~p, ~p, ~p, ~p, ~p)~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Function, A1, A2, A3, A4, A5, TCX, TCY]),
                {TCX, TCY}
        end).

-define(CATCH_TRY(Function, A1, A2, A3, A4, A5, A6),
        try Function(A1, A2, A3, A4, A5, A6)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p(~p, ~p, ~p, ~p, ~p, ~p)~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Function, A1, A2, A3, A4, A5, A6, TCX, TCY]),
                {TCX, TCY}
        end).

-define(CATCH_TRY(Function, A1, A2, A3, A4, A5, A6, A7),
        try Function(A1, A2, A3, A4, A5, A6, A7)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p(~p, ~p, ~p, ~p, ~p, ~p, ~p)~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Function, A1, A2, A3, A4, A5, A6, A7, TCX, TCY]),
                {TCX, TCY}
        end).

-define(CATCH_TRY(Function, A1, A2, A3, A4, A5, A6, A7, A8),
        try Function(A1, A2, A3, A4, A5, A6, A7, A8)
        catch
            TCX:TCY ->
                ?WARNING_MSG("Catched exception~n  in: ~p:~p/~p (line ~p)~n"
                             "  calling: ~p(~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p)~n  catched: ~p:~p",
                               [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                                Function, A1, A2, A3, A4, A5, A6, A7, A8, TCX, TCY]),
                {TCX, TCY}
        end).
