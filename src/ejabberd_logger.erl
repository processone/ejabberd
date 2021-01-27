%%%-------------------------------------------------------------------
%%% File    : ejabberd_logger.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : ejabberd logger wrapper
%%% Created : 12 May 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2021   ProcessOne
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

%% API
-export([start/0, get/0, set/1, get_log_path/0, flush/0]).
-export([convert_loglevel/1, loglevels/0]).
-ifndef(LAGER).
-export([progress_filter/2]).
-endif.
%% Deprecated functions
-export([restart/0, reopen_log/0, rotate_log/0]).
-deprecated([{restart, 0},
	     {reopen_log, 0},
	     {rotate_log, 0}]).

-type loglevel() :: none | emergency | alert | critical |
		    error | warning | notice | info | debug.

-define(is_loglevel(L),
	((L == none) or (L == emergency) or (L == alert)
	 or (L == critical) or (L == error) or (L == warning)
	 or (L == notice) or (L == info) or (L == debug))).

-export_type([loglevel/0]).

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

quiet_mode() ->
    case application:get_env(ejabberd, quiet) of
	{ok, true} -> true;
	_ -> false
    end.

-spec get_integer_env(atom(), T) -> T.
get_integer_env(Name, Default) ->
    case application:get_env(ejabberd, Name) of
        {ok, I} when is_integer(I), I>=0 ->
            I;
        {ok, infinity} ->
            infinity;
        undefined ->
            Default;
        {ok, Junk} ->
            error_logger:error_msg("wrong value for ~ts: ~p; "
                                   "using ~p as a fallback~n",
                                   [Name, Junk, Default]),
            Default
    end.

-ifdef(LAGER).
-spec get_string_env(atom(), T) -> T.
get_string_env(Name, Default) ->
    case application:get_env(ejabberd, Name) of
        {ok, L} when is_list(L) ->
            L;
        undefined ->
            Default;
        {ok, Junk} ->
            error_logger:error_msg("wrong value for ~ts: ~p; "
                                   "using ~p as a fallback~n",
                                   [Name, Junk, Default]),
            Default
    end.

-spec start() -> ok.
start() ->
    start(info).

start(Level) ->
    StartedApps = application:which_applications(5000),
    case lists:keyfind(logger, 1, StartedApps) of
        %% Elixir logger is started. We assume everything is in place
        %% to use lager to Elixir logger bridge.
        {logger, _, _} ->
            error_logger:info_msg("Ignoring ejabberd logger options, using Elixir Logger.", []),
            %% Do not start lager, we rely on Elixir Logger
            do_start_for_logger(Level);
        _ ->
            do_start(Level)
    end.

do_start_for_logger(Level) ->
    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, false),
    application:load(lager),
    application:set_env(lager, error_logger_redirect, false),
    application:set_env(lager, error_logger_whitelist, ['Elixir.Logger.ErrorHandler']),
    application:set_env(lager, crash_log, false),
    application:set_env(lager, handlers, [{elixir_logger_backend, [{level, Level}]}]),
    ejabberd:start_app(lager),
    ok.

do_start(Level) ->
    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, false),
    application:load(lager),
    ConsoleLog = get_log_path(),
    Dir = filename:dirname(ConsoleLog),
    ErrorLog = filename:join([Dir, "error.log"]),
    CrashLog = filename:join([Dir, "crash.log"]),
    LogRotateDate = get_string_env(log_rotate_date, ""),
    LogRotateSize = case get_integer_env(log_rotate_size, 10*1024*1024) of
                        infinity -> 0;
                        V -> V
                    end,
    LogRotateCount = get_integer_env(log_rotate_count, 1),
    LogRateLimit = get_integer_env(log_rate_limit, 100),
    ConsoleLevel0 = case quiet_mode() of
		        true -> critical;
		        _ -> Level
		    end,
    ConsoleLevel = case get_lager_version() >= "3.6.0" of
		       true -> [{level, ConsoleLevel0}];
		       false -> ConsoleLevel0
		   end,
    application:set_env(lager, error_logger_hwm, LogRateLimit),
    application:set_env(
      lager, handlers,
      [{lager_console_backend, ConsoleLevel},
       {lager_file_backend, [{file, ConsoleLog}, {level, Level}, {date, LogRotateDate},
                             {count, LogRotateCount}, {size, LogRotateSize}]},
       {lager_file_backend, [{file, ErrorLog}, {level, error}, {date, LogRotateDate},
                             {count, LogRotateCount}, {size, LogRotateSize}]}]),
    application:set_env(lager, crash_log, CrashLog),
    application:set_env(lager, crash_log_date, LogRotateDate),
    application:set_env(lager, crash_log_size, LogRotateSize),
    application:set_env(lager, crash_log_count, LogRotateCount),
    ejabberd:start_app(lager),
    lists:foreach(fun(Handler) ->
			  lager:set_loghwm(Handler, LogRateLimit)
		  end, gen_event:which_handlers(lager_event)).

-spec restart() -> ok.
restart() ->
    Level = ejabberd_option:loglevel(),
    application:stop(lager),
    start(Level).

-spec reopen_log() -> ok.
reopen_log() ->
    ok.

-spec rotate_log() -> ok.
rotate_log() ->
    catch lager_crash_log ! rotate,
    lists:foreach(
      fun({lager_file_backend, File}) ->
              whereis(lager_event) ! {rotate, File};
         (_) ->
              ok
      end, gen_event:which_handlers(lager_event)).

-spec get() -> loglevel().
get() ->
    Handlers = get_lager_handlers(),
    lists:foldl(fun(lager_console_backend, _Acc) ->
                        lager:get_loglevel(lager_console_backend);
                   (elixir_logger_backend, _Acc) ->
                        lager:get_loglevel(elixir_logger_backend);
                   (_, Acc) ->
                        Acc
                end,
                none, Handlers).

-spec set(0..5 | loglevel()) -> ok.
set(N) when is_integer(N), N>=0, N=<5 ->
    set(convert_loglevel(N));
set(Level) when ?is_loglevel(Level) ->
    case get() of
        Level ->
            ok;
        _ ->
            ConsoleLog = get_log_path(),
            QuietMode = quiet_mode(),
            lists:foreach(
              fun({lager_file_backend, File} = H) when File == ConsoleLog ->
                      lager:set_loglevel(H, Level);
                 (lager_console_backend = H) when not QuietMode ->
                      lager:set_loglevel(H, Level);
                 (elixir_logger_backend = H) ->
                      lager:set_loglevel(H, Level);
                 (_) ->
                      ok
              end, get_lager_handlers())
    end,
    case Level of
	debug -> xmpp:set_config([{debug, true}]);
	_ -> xmpp:set_config([{debug, false}])
    end.

get_lager_handlers() ->
    case catch gen_event:which_handlers(lager_event) of
        {'EXIT',noproc} ->
            [];
        Result ->
            Result
    end.

-spec get_lager_version() -> string().
get_lager_version() ->
    Apps = application:loaded_applications(),
    case lists:keyfind(lager, 1, Apps) of
	{_, _, Vsn} -> Vsn;
	false -> "0.0.0"
    end.

-spec flush() -> ok.
flush() ->
    application:stop(lager),
    application:stop(sasl).

-else.
-include_lib("kernel/include/logger.hrl").

-spec start() -> ok | {error, term()}.
start() ->
    start(info).

start(Level) ->
    EjabberdLog = get_log_path(),
    Dir = filename:dirname(EjabberdLog),
    ErrorLog = filename:join([Dir, "error.log"]),
    LogRotateSize = get_integer_env(log_rotate_size, 10*1024*1024),
    LogRotateCount = get_integer_env(log_rotate_count, 1),
    Config = #{max_no_bytes => LogRotateSize,
	       max_no_files => LogRotateCount,
	       filesync_repeat_interval => no_repeat,
	       file_check => 1000,
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
	case quiet_mode() of
	    true ->
		ok = logger:set_handler_config(default, level, critical);
	    _ ->
		ok
	end,
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

-spec restart() -> ok.
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

-spec reopen_log() -> ok.
reopen_log() ->
    ok.

-spec rotate_log() -> ok.
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

-endif.
