%%%-------------------------------------------------------------------
%%% File    : ejabberd_logger.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : ejabberd logger wrapper
%%% Created : 12 May 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2019   ProcessOne
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

%%%-------------------------------------------------------------------
-module(ejabberd_logger).
-compile({no_auto_import, [get/0]}).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start/0, get/0, set/1, get_log_path/0, flush/0]).
-export([convert_loglevel/1, loglevels/0]).
-export([progress_filter/2]).
%% Deprecated functions
-export([restart/0, reopen_log/0, rotate_log/0]).
-deprecated([{restart, 0},
	     {reopen_log, 0},
	     {rotate_log, 0}]).

-type loglevel() :: none | logger:level().

-define(is_loglevel(L),
	((L == none) or (L == emergency) or (L == alert)
	 or (L == critical) or (L == error) or (L == warning)
	 or (L == notice) or (L == info) or (L == debug))).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_log_path() -> string().
get_log_path() ->
    case ejabberd_config:env_binary_to_list(ejabberd, log_path) of
	{ok, Path} ->
	    Path;
	undefined ->
	    case os:getenv("EJABBERD_LOG_PATH") of
		false ->
		    "ejabberd.log";
		Path ->
		    Path
	    end
    end.

-spec get_integer_env(atom(), T) -> T.
get_integer_env(Name, Default) ->
    case application:get_env(ejabberd, Name) of
        {ok, I} when is_integer(I), I>0 ->
            I;
        undefined ->
            Default;
        {ok, Junk} ->
            ?LOG_ERROR("Wrong value for ~ts: ~p; "
		       "using ~p as a fallback",
		       [Name, Junk, Default]),
            Default
    end.

-spec loglevels() -> [loglevel(), ...].
loglevels() ->
    [none, emergency, alert, critical, error, warning, notice, info, debug].

-spec convert_loglevel(0..5) -> loglevel().
convert_loglevel(0) -> none;
convert_loglevel(1) -> critical;
convert_loglevel(2) -> error;
convert_loglevel(3) -> warning;
convert_loglevel(4) -> info;
convert_loglevel(5) -> debug.

-spec start() -> ok.
start() ->
    start(info).

-spec start(loglevel()) -> ok.
start(Level) ->
    EjabberdLog = get_log_path(),
    Dir = filename:dirname(EjabberdLog),
    ErrorLog = filename:join([Dir, "error.log"]),
    LogRotateSize = get_integer_env(log_rotate_size, 10*1024*1024),
    LogRotateCount = get_integer_env(log_rotate_count, 1),
    Config = #{max_no_bytes => LogRotateSize,
	       max_no_files => LogRotateCount,
	       filesync_repeat_interval => no_repeat,
	       sync_mode_qlen => 1000,
	       drop_mode_qlen => 1000,
	       flush_qlen => 5000},
    FmtConfig = #{legacy_header => false,
		  time_designator => $ ,
		  max_size => 100*1024,
		  single_line => false},
    FileFmtConfig = FmtConfig#{template => file_template()},
    ConsoleFmtConfig = FmtConfig#{template => console_template()},
    try
	ok = logger:set_primary_config(level, Level),
	ok = logger:update_formatter_config(default, ConsoleFmtConfig),
	case logger:add_primary_filter(progress_report,
				       {fun ?MODULE:progress_filter/2, stop}) of
	    ok -> ok;
	    {error, {already_exist, _}} -> ok
	end,
	case logger:add_handler(ejabberd_log, logger_std_h,
				#{level => all,
				  config => Config#{file => EjabberdLog},
				  formatter => {logger_formatter, FileFmtConfig}}) of
	    ok -> ok;
	    {error, {already_exist, _}} -> ok
	end,
	case logger:add_handler(error_log, logger_std_h,
				#{level => error,
				  config => Config#{file => ErrorLog},
				  formatter => {logger_formatter, FileFmtConfig}}) of
	    ok -> ok;
	    {error, {already_exist, _}} -> ok
	end
    catch _:{Tag, Err} when Tag == badmatch; Tag == case_clause ->
	    ?LOG_CRITICAL("Failed to set logging: ~p", [Err]),
	    Err
    end.

restart() ->
    ok.

progress_filter(#{level:=info,msg:={report,#{label:={_,progress}}}} = Event, _) ->
    case get() of
	debug ->
	    logger_filters:progress(Event#{level => debug}, log);
	_ ->
	    stop
    end;
progress_filter(Event, _) ->
    Event.

console_template() ->
    [time, " [", level, "] " | msg()].

file_template() ->
    [time, " [", level, "] ", pid,
     {mfa, ["@", mfa, {line, [":", line], []}], []}, " " | msg()].

msg() ->
    [{logger_formatter, [[logger_formatter, title], ":", io_lib:nl()], []},
     msg, io_lib:nl()].

reopen_log() ->
    ok.

rotate_log() ->
    ok.

-spec get() -> loglevel().
get() ->
    #{level := Level} = logger:get_primary_config(),
    Level.

-spec set(0..5 | loglevel()) -> ok.
set(N) when is_integer(N), N>=0, N=<5 ->
    set(convert_loglevel(N));
set(Level) when ?is_loglevel(Level) ->
    case get() of
	Level -> ok;
	PrevLevel ->
	    ?LOG_NOTICE("Changing loglevel from '~s' to '~s'",
			[PrevLevel, Level]),
	    logger:set_primary_config(level, Level),
	    case Level of
		debug -> xmpp:set_config([{debug, true}]);
		_ -> xmpp:set_config([{debug, false}])
	    end
    end.

-spec flush() -> ok.
flush() ->
    lists:foreach(
      fun(#{id := HandlerId, module := logger_std_h}) ->
	      logger_std_h:filesync(HandlerId);
	 (#{id := HandlerId, module := logger_disk_log_h}) ->
	      logger_disk_log_h:filesync(HandlerId);
	 (_) ->
	      ok
      end, logger:get_handler_config()).
