%%%-------------------------------------------------------------------
%%% File    : eldap_pool.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : LDAP connections pool
%%% Created : 12 Nov 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
%%%-------------------------------------------------------------------

-module(eldap_pool).

-author('xram@jabber.ru').

%% API
-export([start_link/7,
         bind/3,
         search/2,
         modify_passwd/3]).

-include("logger.hrl").

-ifdef(USE_OLD_PG2).


pg_create(PoolName) -> pg2:create(PoolName).


pg_join(PoolName, Pid) -> pg2:join(PoolName, Pid).


pg_get_closest_pid(Name) -> pg2:get_closest_pid(Name).


-else.


pg_create(_) -> pg:start_link().


pg_join(PoolName, Pid) -> pg:join(PoolName, Pid).


pg_get_closest_pid(Group) ->
    case pg:get_local_members(Group) of
        [] ->
            case pg:get_members(Group) of
                [] -> {error, {no_process, Group}};
                [Pid | _] -> Pid
            end;
        [Pid | _] -> Pid
    end.


-endif.


%%====================================================================
%% API
%%====================================================================
bind(PoolName, DN, Passwd) ->
    do_request(PoolName, {bind, [DN, Passwd]}).


search(PoolName, Opts) ->
    do_request(PoolName, {search, [Opts]}).


modify_passwd(PoolName, DN, Passwd) ->
    do_request(PoolName, {modify_passwd, [DN, Passwd]}).


start_link(Name,
           Hosts,
           Backups,
           Port,
           Rootdn,
           Passwd,
           Opts) ->
    PoolName = make_id(Name),
    pg_create(PoolName),
    lists:foreach(fun(Host) ->
                          ID = list_to_binary(erlang:ref_to_list(make_ref())),
                          case catch eldap:start_link(ID,
                                                      [Host | Backups],
                                                      Port,
                                                      Rootdn,
                                                      Passwd,
                                                      Opts) of
                              {ok, Pid} -> pg_join(PoolName, Pid);
                              Err ->
                                  ?ERROR_MSG("Err = ~p", [Err]),
                                  error
                          end
                  end,
                  Hosts).


%%====================================================================
%% Internal functions
%%====================================================================
do_request(Name, {F, Args}) ->
    case pg_get_closest_pid(make_id(Name)) of
        Pid when is_pid(Pid) ->
            case catch apply(eldap, F, [Pid | Args]) of
                {'EXIT', {timeout, _}} ->
                    ?ERROR_MSG("LDAP request failed: timed out", []);
                {'EXIT', Reason} ->
                    ?ERROR_MSG("LDAP request failed: eldap:~p(~p)~nReason: ~p",
                               [F, Args, Reason]),
                    {error, Reason};
                Reply -> Reply
            end;
        Err -> Err
    end.


make_id(Name) ->
    misc:binary_to_atom(<<"eldap_pool_", Name/binary>>).
