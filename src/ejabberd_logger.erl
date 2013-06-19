%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2013, Evgeniy Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 12 May 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%% ejabberd, Copyright (C) 2013   ProcessOne
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

%%%-------------------------------------------------------------------
-module(ejabberd_logger).

%% API
-export([start/0, set_logfile/1, reopen_log/0, get/0, set/1,
         debug_msg/4, info_msg/4, warning_msg/4, error_msg/4,
         critical_msg/4]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    ok.

set_logfile(FileName) ->
    error_logger:add_report_handler(p1_logger_h, FileName).

reopen_log() ->
    %% TODO: Use the Reopen log API for logger_h ?
    p1_logger_h:reopen_log(),
    case application:get_env(sasl,sasl_error_logger) of
	{ok, {file, SASLfile}} ->
	    error_logger:delete_report_handler(sasl_report_file_h),
	    p1_logger_h:rotate_log(SASLfile),
	    error_logger:add_report_handler(sasl_report_file_h,
	        {SASLfile, get_sasl_error_logger_type()});
	_ -> false
	end,
    ok.

get() ->
    p1_loglevel:get().

set(LogLevel) ->
    p1_loglevel:set(LogLevel).

debug_msg(Mod, Line, Format, Args) ->
    p1_logger:debug_msg(Mod, Line, Format, Args).

info_msg(Mod, Line, Format, Args) ->
    p1_logger:info_msg(Mod, Line, Format, Args).

warning_msg(Mod, Line, Format, Args) ->
    p1_logger:warning_msg(Mod, Line, Format, Args).

error_msg(Mod, Line, Format, Args) ->
    p1_logger:error_msg(Mod, Line, Format, Args).

critical_msg(Mod, Line, Format, Args) ->
    p1_logger:critical_msg(Mod, Line, Format, Args).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Function copied from Erlang/OTP lib/sasl/src/sasl.erl which doesn't export it
get_sasl_error_logger_type () ->
    case application:get_env (sasl, errlog_type) of
	{ok, error} -> error;
	{ok, progress} -> progress;
	{ok, all} -> all;
	{ok, Bad} -> exit ({bad_config, {sasl, {errlog_type, Bad}}});
	_ -> all
    end.
