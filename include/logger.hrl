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
-define(PRINT(Format, Args), io:format(Format, Args)).

-include_lib("kernel/include/logger.hrl").

-define(CLEAD,    "\e[1").    % bold
-define(CMID,     "\e[0").    % normal
-define(CCLEAN,   "\e[0m").   % clean

-define(CDEFAULT, ";49;95m"). % light magenta
-define(CDEBUG,   ";49;90m"). % dark gray
-define(CINFO,    ";49;92m"). % green
-define(CWARNING, ";49;93m"). % light yellow
-define(CERROR,   ";49;91m"). % light magenta
-define(CCRITICAL,";49;31m"). % light red

-define(DEBUG(Format, Args),
	begin ?LOG_DEBUG(Format, Args,
                        #{clevel => ?CLEAD ++ ?CDEBUG,
                          ctext => ?CMID ++ ?CDEBUG}),
              ok end).

-define(INFO_MSG(Format, Args),
	begin ?LOG_INFO(Format, Args,
                        #{clevel => ?CLEAD ++ ?CINFO,
                          ctext => ?CCLEAN}),
              ok end).

-define(WARNING_MSG(Format, Args),
	begin ?LOG_WARNING(Format, Args,
                        #{clevel => ?CLEAD ++ ?CWARNING,
                          ctext => ?CMID ++ ?CWARNING}),
              ok end).

-define(ERROR_MSG(Format, Args),
	begin ?LOG_ERROR(Format, Args,
                        #{clevel => ?CLEAD ++ ?CERROR,
                          ctext => ?CMID ++ ?CERROR}),
              ok end).

-define(CRITICAL_MSG(Format, Args),
	begin ?LOG_CRITICAL(Format, Args,
                        #{clevel => ?CLEAD++ ?CCRITICAL,
                          ctext => ?CMID ++ ?CCRITICAL}),
              ok end).

%% Use only when trying to troubleshoot test problem with ExUnit
-define(EXUNIT_LOG(Format, Args),
        case lists:keyfind(logger, 1, application:loaded_applications()) of
            false -> ok;
            _ -> 'Elixir.Logger':bare_log(error, io_lib:format(Format, Args), [?MODULE])
        end).

%% Uncomment if you want to debug p1_fsm/gen_fsm
%%-define(DBGFSM, true).
