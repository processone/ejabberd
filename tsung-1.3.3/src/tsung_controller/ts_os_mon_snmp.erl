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

-module(ts_os_mon_snmp).
-vc('$Id: ts_os_mon_snmp.erl,v 0.0 2008/10/21 12:57:49 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).


-include("ts_profile.hrl").
-include("ts_os_mon.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{
          mon_server,        % pid of mon server
          dnscache=[],
          interval,
          pid,               % pid of snmp_mgr
          host,
          version,
          port,
          community,
          addr
         }).

%% SNMP definitions
%% FIXME: make this customizable in the XML config file ?

-define(SNMP_CPU_RAW_USER, [1,3,6,1,4,1,2021,11,50,0]).
-define(SNMP_CPU_RAW_SYSTEM, [1,3,6,1,4,1,2021,11,52,0]).
-define(SNMP_CPU_RAW_IDLE, [1,3,6,1,4,1,2021,11,53,0]).
-define(SNMP_CPU_LOAD1, [1,3,6,1,4,1,2021,10,1,5,1]).
-define(SNMP_MEM_BUFFER, [1,3,6,1,4,1,2021,4,14,0]).
-define(SNMP_MEM_CACHED, [1,3,6,1,4,1,2021,4,15,0]).
-define(SNMP_MEM_AVAIL, [1,3,6,1,4,1,2021,4,6,0]).
-define(SNMP_MEM_TOTAL, [1,3,6,1,4,1,2021,4,5,0]).

start(Args) ->
    ?LOGF("starting os_mon_snmp with args ~p",[Args],?NOTICE),
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init({HostStr, {Port, Community, Version}, Interval, MonServer}) ->
    {ok, IP} = inet:getaddr(HostStr, inet),
    ts_utils:init_seed(),
    %% wait randomly a few miliseconds to avoid concurrents starts of
    %% the snmp_mgr (see the unregister call later)
    Wait = random:uniform(?INIT_WAIT),
    erlang:start_timer(Wait, self(), connect ),
    ?LOGF("Starting SNMP mgr on ~p (~p)~n", [IP,Wait], ?DEB),
    {ok, #state{ mon_server = MonServer,
                 host       = HostStr,
                 port       = Port,
                 addr       = IP,
                 community  = Community,
                 version    = Version,
                 interval   = Interval}}.

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
handle_info({timeout,_Ref,connect},State=#state{addr=IP,port=Port,community=Community,version=Version}) ->
    {ok, Pid} = snmp_mgr:start_link([{agent, IP},
                                     {agent_udp, Port},
                                     {community, Community},
                                     {receive_type, msg},
                                     Version,
                                     quiet
                                    ]),
    %% since snmp_mgr can handle only a single snmp server, change the
    %% registered name to start several smp_mgr at once !
    unregister(snmp_mgr),
    ?LOGF("SNMP mgr started; remote node is ~p~n", [IP],?INFO),
    erlang:start_timer(State#state.interval, self(), send_request ),
    {noreply, State#state{pid=Pid}};

handle_info({timeout,_Ref,send_request},State=#state{pid=Pid, host=Host}) ->
    ?LOGF("SNMP mgr; get data from host ~p~n", [Host],?DEB),
    snmp_get(Pid,
             [?SNMP_CPU_RAW_SYSTEM,
              ?SNMP_CPU_RAW_USER,
              ?SNMP_MEM_AVAIL,
              ?SNMP_CPU_LOAD1]),
    erlang:start_timer(State#state.interval, self(), send_request ),
    {noreply,State};

handle_info({snmp_msg, Msg, Ip, _Udp}, State) ->
    PDU = snmp_mgr_misc:get_pdu(Msg),
    case PDU#pdu.type of
        'get-response' ->
            ?LOGF("Got SNMP PDU ~p from ~p~n",[PDU, Ip],?DEB),
            {Hostname, NewCache} = ts_utils:resolve(Ip, State#state.dnscache),
            analyse_snmp_data(PDU#pdu.varbinds, Hostname, State),
            {noreply, State#state{dnscache=NewCache}};
        _ ->
            ?LOGF("Got unknown SNMP data ~p from ~p~n",[PDU, Ip],?WARN),
            {noreply, State}
    end;

handle_info(Message, State) ->
    {stop, {unknown_message, Message} , State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

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

%%--------------------------------------------------------------------
%% Function: analyse_snmp_data/3
%% Returns: any (send msg to ts_mon)
%%--------------------------------------------------------------------
analyse_snmp_data(Args, Host, State) ->
    analyse_snmp_data(Args, Host, [], State).

%% Function: analyse_snmp_data/4
analyse_snmp_data([], _Host, Resp, State) ->
    ts_os_mon:send(State#state.mon_server,Resp);

analyse_snmp_data([Val=#varbind{value='NULL'}| Tail], Host, Stats, State) ->
    ?LOGF("SNMP: Skip void result (~p) ~n", [Val],?DEB),
    analyse_snmp_data(Tail, Host, Stats, State);

%% FIXME: this may not be accurate: if we lost packets (the server is
%% overloaded), the value will be inconsistent, since we assume a
%% constant time across samples ($INTERVAL)

analyse_snmp_data([#varbind{oid=?SNMP_CPU_RAW_SYSTEM, value=Val}| Tail], Host, Stats, State) ->
    {value, User} = lists:keysearch(?SNMP_CPU_RAW_USER, #varbind.oid, Tail),
    Value = Val + User#varbind.value,
    CountName = {cpu , Host},
    NewValue = Value/(State#state.interval/1000),
    NewTail = lists:keydelete(?SNMP_CPU_RAW_USER, #varbind.oid, Tail),
    analyse_snmp_data(NewTail, Host, [{sample_counter, CountName, NewValue}| Stats], State);

analyse_snmp_data([User=#varbind{oid=?SNMP_CPU_RAW_USER}| Tail], Host, Stats, State) ->
    %%put this entry at the end, this will be used when SYSTEM match
    analyse_snmp_data(Tail ++ [User], Host, Stats, State);

analyse_snmp_data([#varbind{oid=OID, value=Val}| Tail], Host, Stats, State) ->
    {Type, Name, Value}= oid_to_statname(OID, Host, Val),
    ?LOGF("Analyse SNMP: ~p:~p:~p ~n", [Type, Name, Value],?DEB),
   analyse_snmp_data(Tail, Host, [{Type, Name, Value}| Stats], State).

%%--------------------------------------------------------------------
%% Function: oid_to_statname/3
%%--------------------------------------------------------------------
oid_to_statname(?SNMP_CPU_RAW_IDLE, Name, Value) ->
    CountName = {cpu_idle, Name},
    ?DebugF("Adding counter value for ~p~n",[CountName]),
    {sample_counter, CountName, Value/(?INTERVAL/1000)}; % FIXME ? Interval ??
oid_to_statname(?SNMP_MEM_AVAIL, Name, Value)->
    CountName = {freemem, Name},
    ?DebugF("Adding counter value for ~p~n",[CountName]),
    {sample,CountName, Value/1000};
oid_to_statname(?SNMP_CPU_LOAD1, Name, Value)->
    CountName = {load, Name},
    ?DebugF("Adding counter value for ~p~n",[CountName]),
    {sample,CountName, Value/100}.

%%--------------------------------------------------------------------
%% Function: snmp_get/2
%% Description: ask a list of OIDs to the given snmp_mgr
%%--------------------------------------------------------------------
snmp_get(Pid, Oids) ->
    ?DebugF("send snmp get for oid ~p to pid ~p ",[Oids,Pid]),
    Pid ! {get, Oids}.

