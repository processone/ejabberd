%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
-define(PRINT(Format, Args), io:format(Format, Args)).

-ifdef(LAGER).
-compile([{parse_transform, lager_transform}]).

-define(DEBUG(Format, Args),
	begin lager:debug(Format, Args), ok end).

-define(INFO_MSG(Format, Args),
	begin lager:info(Format, Args), ok end).

-define(WARNING_MSG(Format, Args),
	begin lager:warning(Format, Args), ok end).

-define(ERROR_MSG(Format, Args),
	begin lager:error(Format, Args), ok end).

-define(CRITICAL_MSG(Format, Args),
	begin lager:critical(Format, Args), ok end).
-else.
-include_lib("kernel/include/logger.hrl").

-define(DEBUG(Format, Args),
	begin ?LOG_DEBUG(Format, Args), ok end).

-define(INFO_MSG(Format, Args),
	begin ?LOG_INFO(Format, Args), ok end).

-define(WARNING_MSG(Format, Args),
	begin ?LOG_WARNING(Format, Args), ok end).

-define(ERROR_MSG(Format, Args),
	begin ?LOG_ERROR(Format, Args), ok end).

-define(CRITICAL_MSG(Format, Args),
	begin ?LOG_CRITICAL(Format, Args), ok end).
-endif.

%% Use only when trying to troubleshoot test problem with ExUnit
-define(EXUNIT_LOG(Format, Args),
        case lists:keyfind(logger, 1, application:loaded_applications()) of
            false -> ok;
            _ -> 'Elixir.Logger':bare_log(error, io_lib:format(Format, Args), [?MODULE])
        end).

-type re_mp() :: {re_pattern, _, _, _, _}. % Copied from re.erl

%% Uncomment if you want to debug p1_fsm/gen_fsm
%%-define(DBGFSM, true).
