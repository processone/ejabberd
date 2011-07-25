%%%  Copyright (C) 2009 Nicolas Niclausse
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

-module(ts_os_mon_sup).
-vc('$Id: ts_client_sup.erl 953 2008-11-23 16:57:05Z nniclausse $ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(supervisor).

-include("ts_profile.hrl").

%% External exports
-export([start_link/1, start_child/2]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Plugin) ->
    Module=get_module(Plugin),
    supervisor:start_link({local,Module}, ?MODULE, [Plugin]).

start_child(Plugin, Args) ->
    ?LOGF("Starting child for plugin ~p with args ~p~n",[Plugin,Args], ?DEB),
    Module=get_module(Plugin),
    supervisor:start_child(Module,[Args]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
init([Plugin]) ->
    ?LOGF("Starting with args ~p~n",[Plugin], ?INFO),
    SupFlags = {simple_one_for_one, 20, 20},
    {ok, {SupFlags, get_spec(Plugin)}}.

%% internal funs

get_spec(Plugin) ->
    Module=get_module(Plugin),
    [{Module,{Module, start, []}, permanent, 2000, worker,[Module]}].

get_module(Plugin)  when is_atom(Plugin)->
    ModuleStr="ts_os_mon_" ++ atom_to_list(Plugin),
    list_to_atom(ModuleStr).
