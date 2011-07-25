%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

-module(tsung).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([start/2, stop/1]).
-behaviour(application).

-include("ts_profile.hrl").

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%%----------------------------------------------------------------------
start(_Type, _StartArgs) ->
% error_logger:tty(false),
    ?LOG("open logfile  ~n",?DEB),
    LogFileEnc = ts_config_server:decode_filename(?config(log_file)),
    LogFile = filename:join(LogFileEnc, atom_to_list(node()) ++ ".log"),
    LogDir = filename:dirname(LogFile),
    ok = ts_utils:make_dir_rec(LogDir),
    error_logger:logfile({open, LogFile}),
    ?LOG("ok~n",?DEB),
    case ts_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            ?LOGF("Can't start ! ~p ~n",[Error],?ERR),
            Error
    end.


%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) ->
    stop.
