%%%----------------------------------------------------------------------
%%% File    : mod_ip_blacklist.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Download blacklists from ProcessOne
%%% Created : 5 May 2008 by Mickael Remond <mremond@process-one.net>
%%% Usage   : Add the following line in modules section of ejabberd.cfg:
%%%              {mod_ip_blacklist, []}
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_ip_blacklist).
-author('mremond@process-one.net').

-behaviour(gen_mod).

%% API:
-export([start/2,
         preinit/2,
         init/1,
         stop/1]).
-export([update_bl_c2s/0]).
%% Hooks:
-export([is_ip_in_c2s_blacklist/2]).

-include("ejabberd.hrl").

-define(PROCNAME, ?MODULE).
-define(BLC2S, "http://xaai.process-one.net/bl_c2s.txt").
-define(UPDATE_INTERVAL, 6). %% in hours

-record(state, {timer}).
-record(bl_c2s, {ip}).

%% Start once for all vhost
start(_Host, _Opts) ->
   Pid = spawn(?MODULE, preinit, [self(), #state{}]),
   receive {ok, Pid, PreinitResult} ->
       PreinitResult
   end.

preinit(Parent, State) ->
    Pid = self(),
    try register(?PROCNAME, Pid) of
        true ->
            Parent ! {ok, Pid, true},
            init(State)
    catch error:_ ->
        Parent ! {ok, Pid, true}
    end.

%% TODO:
stop(_Host) ->
    ok.

init(State)->
    inets:start(),
    ets:new(bl_c2s, [named_table, public, {keypos, #bl_c2s.ip}]),
    update_bl_c2s(),
    %% Register hooks for blacklist
    ejabberd_hooks:add(check_bl_c2s, ?MODULE, is_ip_in_c2s_blacklist, 50),
    %% Set timer: Download the blacklist file every 6 hours
    timer:apply_interval(timer:hours(?UPDATE_INTERVAL), ?MODULE, update_bl_c2s, []),
    loop(State).

%% Remove timer when stop is received.
loop(_State) ->
    receive
	stop ->
	    ok
    end.

%% Download blacklist file from ProcessOne XAAI
%% and update the table internal table
%% TODO: Support comment lines starting by %
update_bl_c2s() ->
    ?INFO_MSG("Updating C2S Blacklist", []),
    case httpc:request(?BLC2S) of
	{ok, {{_Version, 200, _Reason}, _Headers, Body}} ->
	    IPs = string:tokens(Body,"\n"),
	    ets:delete_all_objects(bl_c2s),
	    lists:foreach(
	      fun(IP) ->
		      ets:insert(bl_c2s, #bl_c2s{ip=list_to_binary(IP)})
	      end, IPs);
	{error, Reason} ->
	    ?ERROR_MSG("Cannot download C2S blacklist file. Reason: ~p",
		       [Reason])
    end.

%% Hook is run with:
%% ejabberd_hooks:run_fold(check_bl_c2s, false, [IP]),
%% Return: false: IP not blacklisted
%%         true: IP is blacklisted
%% IPV4 IP tuple:
is_ip_in_c2s_blacklist(_Val, IP) when is_tuple(IP) ->
    BinaryIP = list_to_binary(jlib:ip_to_list(IP)),
    case ets:lookup(bl_c2s, BinaryIP) of
	[] -> %% Not in blacklist
	    false;
	[_] -> %% Blacklisted!
	    {stop, true}
    end;
is_ip_in_c2s_blacklist(_Val, _IP) ->
    false.

%% TODO:
%% - For now, we do not kick user already logged on a given IP after
%%    we update the blacklist.
