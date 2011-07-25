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
%%%
%%%  ts_user_server_sup.erl
%%%  @author Pablo Polvorin
%%%  @doc
%%%  created on 2008-09-09

-module(ts_user_server_sup).

-export([start_link/0,init/1,start_user_server/1,all_children/0]).

-behaviour(supervisor).

start_link() ->
    {ok,Pid} = supervisor:start_link({global,?MODULE},?MODULE,[]),
    start_default_user_server(),
    %default user_server is always started
    {ok,Pid}.

init([]) ->
    SupFlags = {simple_one_for_one,1,1 },
    ChildSpec = [
                 {ts_user_server,{ts_user_server, start, []},
                  temporary,2000,worker,[ts_user_server]}
                ],
    {ok, {SupFlags, ChildSpec}}.

start_user_server(Name) ->
   supervisor:start_child({global,?MODULE},[Name]).

start_default_user_server() ->
   supervisor:start_child({global,?MODULE},[]).


all_children() ->
    [ Pid ||{_,Pid,_,_} <- supervisor:which_children({global,?MODULE})].
