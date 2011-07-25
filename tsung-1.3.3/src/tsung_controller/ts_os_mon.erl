%%%  This code was developped by Mickael Remond
%%%  <mickael.remond@erlang-fr.org> and contributors (their names can
%%%  be found in the CONTRIBUTORS file).  Copyright (C) 2003 Mickael
%%%  Remond
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

%%%  Created :  23 Dec 2003 by Mickael Remond <mickael.remond@erlang-fr.org>

-module(ts_os_mon).
-author('mickael.remond@erlang-fr.org').
-modifiedby('nicolas@niclux.org').
-vc('$Id$ ').


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_profile.hrl").
-include("ts_os_mon.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([activate/0, send/2]).

%%% send data back to the controlling node
send(Mon_Server, Data) when is_pid(Mon_Server) ->
    Mon_Server ! {add, Data};
send(Mon_Server, Data) ->
    gen_server:cast(Mon_Server, {add, Data}).

%%--------------------------------------------------------------------
%% Function: activate/0
%% Purpose: This is used by tsung to start the cluster monitor service
%% It will only be started if there are cluster/monitor@host element
%% in the config file.
%%--------------------------------------------------------------------
activate() ->
    case ts_config_server:get_monitor_hosts() of
        [] ->
           ?LOG("os_mon disabled",?NOTICE),
            ok;
        Hosts ->
            Fun = fun({HostStr,{Type,Options}}) ->
                          Args= {HostStr, Options, ?INTERVAL,{global, ts_mon}},
                          ts_os_mon_sup:start_child(Type, Args)
                  end,
            lists:foreach(Fun,Hosts)
    end.


