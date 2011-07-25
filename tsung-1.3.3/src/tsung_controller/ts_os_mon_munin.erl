%%%
%%%  Copyright 2008 © Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nicolas.nniclausse@niclux.org>
%%%  Created: 21 oct 2008 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

-module(ts_os_mon_munin).
-vc('$Id: ts_os_mon_snmp.erl,v 0.0 2008/10/21 12:57:49 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc munin plugin for ts_os_mon
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("ts_profile.hrl").
-include("ts_os_mon.hrl").

-define(READ_TIMEOUT,2500). % 2.5 sec
-define(SEND_TIMEOUT,5000).

%% if interval is more than this, we must send ping to avoid closed
%% connection from munin node server (default timeout is 10s in recent
%% version of munin-node):
-define(MAX_INTERVAL,8000).
-define(PING_INTERVAL,5000).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{
          mon,        % pid of mon server
          interval,   % interval in msec between gathering of data
          socket,     % tcp socket
          port,       % tcp port of munin-node server
          host,       % remote munin-node hostname
          addr,       % remote munin-node IP addr
          ncpus       % number of cpus of remote server
         }).

start(Args) ->
    ?LOGF("starting os_mon_munin with args ~p",[Args],?NOTICE),
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init({HostStr, {Port}, Interval, MonServer}) ->
    ?LOGF("Starting munin mgr on ~p:~p~n", [HostStr,Port], ?DEB),
    {ok, IP} = inet:getaddr(HostStr, inet),
    erlang:start_timer(?INIT_WAIT, self(), connect ),
    {ok, #state{mon=MonServer, host=HostStr, interval=Interval, addr=IP, port=Port}}.


%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {unknown_message, Msg}, State}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({timeout,_Ref,connect},State=#state{addr=IP,port=Port,host=HostStr}) ->
    Opts=[list,
          {active, false},
          {packet, line},
          {send_timeout, ?SEND_TIMEOUT},
          {keepalive, true}
         ],
    case gen_tcp:connect(IP, Port, Opts) of
        {ok, Socket} ->
            case gen_tcp:recv(Socket,0, ?READ_TIMEOUT) of
                {ok, "# munin node at "++ Str} ->
                    MuninHost = ts_utils:chop(Str),
                    ?LOGF("Connected to ~p~n", [MuninHost], ?INFO),
                    %% We want CPU value ranging from 0 to 100, so we need the max value :
                    gen_tcp:send(Socket,"config cpu\n"),
                    ConfigCPU=read_munin_data(Socket),
                    NCPUs = case proplists:get_value('user.max',ConfigCPU) of
                                Num when  is_number(Num) ->
                                    Num/100 ;
                                _ ->
                                    ?LOG("can't find the number of CPU, assume one~n",?NOTICE),
                                    1
                            end,
                    ?LOGF("first fetch succesful to ~p~n", [MuninHost], ?INFO),
                    case (State#state.interval > ?MAX_INTERVAL) of
                        true ->
                            erlang:start_timer(?PING_INTERVAL, self(), ping );
                        _ ->
                            ok
                    end,
                    erlang:start_timer(State#state.interval, self(), send_request ),
                    {noreply, State#state{socket=Socket,host=MuninHost,ncpus=NCPUs}};
                {error, Reason} ->
                    ?LOGF("Error while connecting to munin server: ~p~n", [Reason], ?ERR),
                    {stop, Reason, State}
            end;
        {error, Reason} ->
            ?LOGF("Can't connect to munin server on ~p, reason:~p~n", [HostStr, Reason], ?ERR),
            {stop, Reason, State}
    end;

handle_info({timeout, _Ref, ping},  State=#state{socket=Socket} ) ->
    gen_tcp:send(Socket,"\n"),
    gen_tcp:recv(Socket,0,?READ_TIMEOUT),
    erlang:start_timer(?PING_INTERVAL, self(), ping ),
    {noreply, State};

handle_info({timeout, _Ref, send_request},  State=#state{socket=Socket,host=Hostname} ) ->
    %% Currenly, fetch only cpu and memory
    %% FIXME: should be customizable in XML config file
    ?LOGF("Fetching munin for cpu on host ~p~n", [Hostname], ?DEB),
    gen_tcp:send(Socket,"fetch cpu\n"),
    AllCPU=read_munin_data(Socket),
    ?LOGF("Fetching munin for memory on host ~p~n", [Hostname], ?DEB),
    gen_tcp:send(Socket,"fetch memory\n"),
    AllMem=read_munin_data(Socket),
    ?LOGF("Fetching munin for load on host ~p~n", [Hostname], ?DEB),
    gen_tcp:send(Socket,"fetch load\n"),
    AllLoad=read_munin_data(Socket),
    %% sum all cpu types, except idle.
    NonIdle=lists:keydelete('idle.value',1,AllCPU),
    RawCpu = lists:foldl(fun({_Key,Val},Acc) when is_integer(Val)->
                                 Acc+Val
                         end,0,NonIdle) / (State#state.interval div 1000),
    Cpu=check_value(RawCpu,{Hostname,"cpu"})/State#state.ncpus,
    ?LOGF(" munin cpu on host ~p is  ~p~n", [Hostname,Cpu], ?DEB),
    %% returns free + buffer + cache
    FunFree = fun({Key,Val},Acc) when ((Key=='buffers.value') or
                                       (Key=='free.value')    or
                                       (Key=='cached.value') ) ->
                      Acc+Val;
                 (_, Acc) -> Acc
              end,
    FreeMem=check_value(lists:foldl(FunFree,0,AllMem),{Hostname,"memory"})/1048576,%MBytes
    ?LOGF(" munin memory on host ~p is ~p~n", [Hostname,FreeMem], ?DEB),
    %% load only has one value at present
    Load = lists:foldl(fun({_Key,Val},Acc) -> Acc+Val end,0,AllLoad),
    ?LOGF(" munin load on host ~p is ~p~n", [Hostname,Load], ?DEB),
    ts_os_mon:send(State#state.mon,[{sample_counter, {cpu, Hostname}, Cpu},
                                    {sample, {freemem, Hostname}, FreeMem},
                                    {sample, {load, Hostname}, Load}]),
    erlang:start_timer(State#state.interval, self(), send_request ),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=undefined}) ->
    ok;
terminate(_Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket).

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

read_munin_data(Socket)->
    read_munin_data(Socket,gen_tcp:recv(Socket,0,?READ_TIMEOUT),[]).

read_munin_data(_Socket,{ok,".\n"}, Acc)->
    Acc;
read_munin_data(Socket,{ok, "graph_args --base "++ Data}, Acc) when is_list(Acc)->
    %% special case for getting the number of cpus
    NewAcc = case re:run(Data,"--upper-limit (\\d+)",[{capture,all_but_first,list}]) of
                 {match,[Val]} when length(Val) > 0 ->
                     ?LOGF("the munin node has ~p CPUs ~n",[Val],?INFO),
                     [{'user.max',list_to_integer(Val)}| Acc];
                 _ ->
                     ?LOGF("upper-limit don't match ~p~n",[Data],?WARN),
                     Acc
             end,
    read_munin_data(Socket,gen_tcp:recv(Socket,0,?READ_TIMEOUT), NewAcc);
read_munin_data(Socket,{ok, Data}, Acc) when is_list(Acc)->
    ?DebugF("Parse munin data: ~p~n",[Data]),
    NewAcc = case string:tokens(Data," \n") of
                 [Key, Value] ->
                     try ts_utils:list_to_number(Value) of
                         Num when is_number(Num) ->
                             [{list_to_atom(Key), Num }|Acc]
                     catch
                         _Type:_Exp ->
                             Acc
                     end;
                 [_Key| _Rest] ->
                      Acc;
                 _ ->
                     ?LOGF("Unknown data received from munin server: ~p~n",[Data],?WARN),
                     Acc
             end,
    read_munin_data(Socket,gen_tcp:recv(Socket,0,?READ_TIMEOUT), NewAcc).
%% no clause for errors: let it crash ! (OTP supervision tree rules ...)

%% check is this a valid value (positive at least)
check_value(Val,_) when Val > 0 -> Val;
check_value(Val,{Host, Type})  ->
    ?LOGF("munin: bad ~s value on host ~p: ~p~n", [Type, Host, Val],?WARN),
    0.
