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
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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

-export([set/1, get/0]).

-include("ejabberd.hrl").

-define(LOGMODULE, "error_logger").

%% Error levels:
-record(loglevel, {ordinal,
		   name,
		   description,
		   function = no_log,
		   event_type = no_log,
		   msg_prefix = no_log}).

-define(LOG_LEVELS,
	[#loglevel{ordinal = 0, name = no_log, description = "No log"},
	 #loglevel{ordinal = 1, name = critical, description = "Critical",
		   function = critical_msg, event_type = error, msg_prefix = "C"},
	 #loglevel{ordinal = 2, name = error, description = "Error",
		   function = error_msg, event_type = error, msg_prefix = "E"},
	 #loglevel{ordinal = 3, name = warning, description = "Warning",
		   function = warning_msg, event_type = warning_msg, msg_prefix = "W"},
	 #loglevel{ordinal = 4, name = info, description = "Info",
		   function = info_msg, event_type = info_msg, msg_prefix = "I"},
	 #loglevel{ordinal = 5, name = debug, description = "Debug",
		   function = debug_msg, event_type = info_msg, msg_prefix = "D"}]).

get() ->
    Level = ejabberd_logger:get(),
    case lists:keysearch(Level, #loglevel.ordinal, ?LOG_LEVELS) of
        {value, Result = #loglevel{}} ->
	    {Result#loglevel.ordinal, Result#loglevel.name, Result#loglevel.description};
        _ ->
	    erlang:error({no_such_loglevel, Level})
    end.


set(LogLevel) when is_atom(LogLevel) ->
    set(level_to_integer(LogLevel));
set(Loglevel) when is_integer(Loglevel) ->
    try
        {Mod,Code} = dynamic_compile:from_string(ejabberd_logger_src(Loglevel)),
        code:load_binary(Mod, ?LOGMODULE ++ ".erl", Code)
    catch
        Type:Error -> ?CRITICAL_MSG("Error compiling logger (~p): ~p~n", [Type, Error])
    end;
set(_) ->
    exit("Loglevel must be an integer").

level_to_integer(Level) ->
    case lists:keysearch(Level, #loglevel.name, ?LOG_LEVELS) of
        {value, #loglevel{ordinal = Int}} -> Int;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the ejabberd logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
ejabberd_logger_src(Loglevel) ->
    lists:flatten([header_src(),
		   get_src(Loglevel),
		   [log_src(Loglevel, LevelSpec) || LevelSpec <- ?LOG_LEVELS],
		   notify_src()]).

header_src() ->
    "-module(ejabberd_logger).
    -author('mickael.remond@process-one.net').

    -export([debug_msg/4,
             info_msg/4,
             warning_msg/4,
             error_msg/4,
             critical_msg/4,
             get/0]).
    ".

get_src(Loglevel) ->
    ["get() -> ", integer_to_list(Loglevel), ".
     "].

log_src(_Loglevel, #loglevel{function = no_log}) ->
    [];
log_src(Loglevel, Spec = #loglevel{ordinal = MinLevel})
  when Loglevel < MinLevel ->
    [atom_to_list(Spec#loglevel.function), "(_, _, _, _) -> ok.
     "];
log_src(_Loglevel, Spec = #loglevel{}) ->
    [atom_to_list(Spec#loglevel.function), "(Module, Line, Format, Args) ->
        notify(", atom_to_list(Spec#loglevel.event_type), ",
               \"", Spec#loglevel.msg_prefix, "(~p:~p:~p) : \"++Format++\"~n\",
               [self(), Module, Line | Args]).
        "].

notify_src() ->
    %% Distribute the message to the Erlang error logger
    "notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
