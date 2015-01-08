%%%----------------------------------------------------------------------
%%% File    : mod_ip_blacklist.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Download blacklists from ProcessOne
%%% Created : 5 May 2008 by Mickael Remond <mremond@process-one.net>
%%% Usage   : Add the following line in modules section of ejabberd.cfg:
%%%              {mod_ip_blacklist, []}
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(mod_ip_blacklist).

-author('mremond@process-one.net').

-behaviour(gen_mod).

%% API:
-export([start/2, preinit/2, init/1, stop/1]).

-export([update_bl_c2s/0]).

%% Hooks:
-export([is_ip_in_c2s_blacklist/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(PROCNAME, ?MODULE).

-define(BLC2S,
	<<"http://xaai.process-one.net/bl_c2s.txt">>).

-define(UPDATE_INTERVAL, 6).

-record(state, {timer}).

%% Start once for all vhost
-record(bl_c2s, {ip = <<"">> :: binary()}).

start(_Host, _Opts) ->
    Pid = spawn(?MODULE, preinit, [self(), #state{}]),
    receive {ok, Pid, PreinitResult} -> PreinitResult end.

preinit(Parent, State) ->
    Pid = self(),
    try register(?PROCNAME, Pid) of
      true -> Parent ! {ok, Pid, true}, init(State)
    catch
      error:_ -> Parent ! {ok, Pid, true}
    end.

%% TODO:
stop(_Host) -> ok.

init(State) ->
    ets:new(bl_c2s,
	    [named_table, public, {keypos, #bl_c2s.ip}]),
    update_bl_c2s(),
    ejabberd_hooks:add(check_bl_c2s, ?MODULE,
		       is_ip_in_c2s_blacklist, 50),
    timer:apply_interval(timer:hours(?UPDATE_INTERVAL),
			 ?MODULE, update_bl_c2s, []),
    loop(State).

%% Remove timer when stop is received.
loop(_State) -> receive stop -> ok end.

%% Download blacklist file from ProcessOne XAAI
%% and update the table internal table
%% TODO: Support comment lines starting by %
update_bl_c2s() ->
    ?INFO_MSG("Updating C2S Blacklist", []),
    case httpc:request(?BLC2S) of
      {ok, 200, _Headers, Body} ->
	  IPs = str:tokens(Body, <<"\n">>),
	  ets:delete_all_objects(bl_c2s),
	  lists:foreach(fun (IP) ->
				ets:insert(bl_c2s,
					   #bl_c2s{ip = IP})
			end,
			IPs);
      {error, Reason} ->
	  ?ERROR_MSG("Cannot download C2S blacklist file. "
		     "Reason: ~p",
		     [Reason])
    end.

%% Hook is run with:
%% ejabberd_hooks:run_fold(check_bl_c2s, false, [IP]),
%% Return: false: IP not blacklisted
%%         true: IP is blacklisted
%% IPV4 IP tuple:
is_ip_in_c2s_blacklist(_Val, IP, Lang) when is_tuple(IP) ->
    BinaryIP = jlib:ip_to_list(IP),
    case ets:lookup(bl_c2s, BinaryIP) of
      [] -> %% Not in blacklist
	  false;
      [_] ->
	  LogReason = io_lib:fwrite(
			"This IP address is blacklisted in ~s",
			[?BLC2S]),
	  ReasonT = io_lib:fwrite(
		      translate:translate(
			Lang,
			<<"This IP address is blacklisted in ~s">>),
		      [?BLC2S]),
	  {stop, {true, LogReason, ReasonT}}
    end;
is_ip_in_c2s_blacklist(_Val, _IP, _Lang) -> false.

%% TODO:
%% - For now, we do not kick user already logged on a given IP after
%%    we update the blacklist.

