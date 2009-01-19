%%%----------------------------------------------------------------------
%%% File    : ejabberd_loglevel.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Loglevel switcher.
%%%           Be careful: you should not have any ejabberd_logger module
%%%           as ejabberd_loglevel switcher is compiling and loading
%%%           dynamically a "virtual" ejabberd_logger module (Described
%%%           in a string at the end of this module).
%%% Created : 29 Nov 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

-module(ejabberd_loglevel).
-author('mickael.remond@process-one.net').

-export([set/1]).

-include("ejabberd.hrl").

-define(LOGMODULE, "error_logger").

%% Error levels:
%% 0 -> No log
%% 1 -> Critical
%% 2 -> Error
%% 3 -> Warning
%% 4 -> Info
%% 5 -> Debug
set(Loglevel) when is_integer(Loglevel) ->
   Forms = compile_string(?LOGMODULE, ejabberd_logger_src(Loglevel)),
   load_logger(Forms, ?LOGMODULE, Loglevel);
set(_) ->
    exit("Loglevel must be an integer").
                
%% --------------------------------------------------------------  
%% Compile a string into a module and returns the binary
compile_string(Mod, Str) ->
    Fname = Mod ++ ".erl",
    {ok, Fd} = open_ram_file(Fname),
    file:write(Fd, Str),
    file:position(Fd, 0),
    case epp_dodger:parse(Fd) of
	{ok, Tree} ->
	    Forms = revert_tree(Tree),
	    close_ram_file(Fd),
	    Forms;
	Error ->
	    close_ram_file(Fd),
	    Error
    end.
   
open_ram_file(Fname) ->
    ram_file_io_server:start(self(), Fname, [read,write]).

close_ram_file(Fd) ->
    file:close(Fd).

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- Tree].

load_logger(Forms, Mod, Loglevel) ->
    Fname = Mod ++ ".erl",
    case compile:forms(Forms, [binary, {d,'LOGLEVEL',Loglevel}]) of
        {ok, M, Bin} ->
            code:load_binary(M, Fname, Bin);
        Error ->
            ?CRITICAL_MSG("Error ~p~n", [Error])
    end.

%% --------------------------------------------------------------
%% Code of the ejabberd logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.        
ejabberd_logger_src(Loglevel) ->
    L = integer_to_list(Loglevel),
    "-module(ejabberd_logger).
    -author('mickael.remond@process-one.net').

    -export([debug_msg/4,
             info_msg/4,
             warning_msg/4,
             error_msg/4,
             critical_msg/4]).

    %% Helper functions
    debug_msg(Module, Line, Format, Args) when " ++ L ++ " >= 5 ->
            notify(info_msg,
                   \"D(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    debug_msg(_,_,_,_) -> ok.

    info_msg(Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            notify(info_msg,
                   \"I(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    info_msg(_,_,_,_) -> ok.

    warning_msg(Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            notify(error,
                   \"W(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    warning_msg(_,_,_,_) -> ok.

    error_msg(Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
            notify(error,
                   \"E(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    error_msg(_,_,_,_) -> ok.

    critical_msg(Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            notify(error,
                   \"C(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    critical_msg(_,_,_,_) -> ok.

    %% Distribute the message to the Erlang error logger
    notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
