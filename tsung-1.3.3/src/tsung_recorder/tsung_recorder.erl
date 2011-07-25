%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 22 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%%-------------------------------------------------------------------
%%% File    : tsung_recorder.erl
%%% Author  :  <nicolas.niclaussse@IDEALX.com>
%%% Description : tsung_recorder application
%%% Created : 22 Dec 2003 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------

-module(tsung_recorder).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([start/2,stop/1, stop_all/1]).
-behaviour(application).

-include("ts_profile.hrl").

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%%----------------------------------------------------------------------
start(_Type, _StartArgs) ->
    error_logger:tty(false),
    error_logger:logfile({open, ?config(log_file) ++ "-" ++ atom_to_list(node())}),
    case ts_recorder_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            ?LOGF("Can't start ! ~p ~n",[Error], ?ERR),
            Error
    end.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) ->
    stop.

%%----------------------------------------------------------------------
%% Func: stop_all/1
%% Returns: any
%%----------------------------------------------------------------------
stop_all(Arg) ->
    ts_utils:stop_all(Arg,'ts_proxy_listener', "tsung recorder",
                       fun ts_proxy_recorder:stop/1).
