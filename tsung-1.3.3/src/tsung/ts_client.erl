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
%%%  Created : 15 Feb 2001 by Nicolas Niclausse <nicolas@niclux.org>

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

-module(ts_client).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').
-modified_by('jflecomte@IDEALX.com').

-behaviour(gen_fsm). % two state: wait_ack | think

%%% if bidi is true (for bidirectional), the server can send data
%%% to the client at anytime (full bidirectional protocol, as jabber
%%% for ex)

-include("ts_profile.hrl").
-include("ts_config.hrl").

-define(MAX_RETRIES,3). % max number of connection retries
-define(RETRY_TIMEOUT,10000). % waiting time between retries (msec)

%% External exports
-export([start/1, next/1]).

%% gen_server callbacks

-export([init/1, wait_ack/2, handle_sync_event/4, handle_event/3,
         handle_info/3, terminate/3, code_change/4]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec start(Opts::{Session::#session{},IP::tuple(),Server::#server{},
%%       Id::integer()}) -> {ok, Pid::pid()} | ignore | {error, Error::term()}
%% @doc Start a new session
start(Opts) ->
    ?DebugF("Starting with opts: ~p~n",[Opts]),
    gen_fsm:start_link(?MODULE, Opts, []).

%%----------------------------------------------------------------------
%% @spec next({pid()}) -> ok
%% @doc Purpose: continue with the next request (used for global ack)
%% @end
%%----------------------------------------------------------------------
next({Pid}) ->
    gen_fsm:send_event(Pid, next_msg).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, State}          |
%%          {ok, StateName, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init({#session{id           = SessionId,
               persistent   = Persistent,
               bidi         = Bidi,
               hibernate    = Hibernate,
               proto_opts   = ProtoOpts,
               size         = Count,
               type         = CType}, IP, Server,Id}) ->
    ?DebugF("Init ... started with count = ~p~n",[Count]),
   ts_utils:init_seed(),

    ?DebugF("Get dynparams for ~p~n",[CType]),
    DynData = CType:init_dynparams(),
    NewDynVars = ts_dynvars:set(tsung_userid,integer_to_list(Id),
                                DynData#dyndata.dynvars),
    NewDynData = DynData#dyndata{dynvars=NewDynVars},
    StartTime= now(),
    set_thinktime(?short_timeout),
    {ok, think, #state_rcv{ port       = Server#server.port,
                            host       = Server#server.host,
                            session_id = SessionId,
                            bidi       = Bidi,
                            protocol   = Server#server.type,
                            clienttype = CType,
                            session    = CType:new_session(),
                            persistent = Persistent,
                            starttime  = StartTime,
                            dump       = ?config(dump),
                            proto_opts = ProtoOpts,
                            count      = Count,
                            ip         = IP,
                            id         = Id,
                            hibernate  = Hibernate,
                            maxcount   = Count,
                            dyndata    = NewDynData
                           }}.

%%--------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%--------------------------------------------------------------------
wait_ack(next_msg,State=#state_rcv{request=R}) when R#ts_request.ack==global->
    NewSocket = ts_utils:inet_setopts(State#state_rcv.protocol,
                                      State#state_rcv.socket,
                                      [{active, once} ]),
    {PageTimeStamp, _} = update_stats(State),
    handle_next_action(State#state_rcv{socket=NewSocket,
                                       page_timestamp=PageTimeStamp});
wait_ack(timeout,State) ->
    ?LOG("Error: timeout receive in state wait_ack~n", ?ERR),
    ts_mon:add({ count, timeout }),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%--------------------------------------------------------------------
handle_event(Event, SName, StateData) ->
    ?LOGF("Unknown event (~p) received in state ~p, abort",[Event,SName],?ERR),
    {stop, unknown_event, StateData}.

%%--------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {next_state, StateName, State}          |
%%          {next_state, StateName, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% inet data
handle_info({NetEvent, _Socket, Data}, wait_ack, State) when NetEvent==tcp;
                                                             NetEvent==ssl ->
    ?DebugF("TCP data received: size=~p ~n",[size(Data)]),
    case handle_data_msg(Data, State) of
        {NewState=#state_rcv{ack_done=true}, Opts} ->
            NewSocket = ts_utils:inet_setopts(NewState#state_rcv.protocol,
                                              NewState#state_rcv.socket,
                                              [{active, once} | Opts]),
            handle_next_action(NewState#state_rcv{socket=NewSocket,
                                                  ack_done=false});
        {NewState, Opts} ->
            NewSocket = ts_utils:inet_setopts(NewState#state_rcv.protocol,
                                              NewState#state_rcv.socket,
                                              [{active, once} | Opts]),
            TimeOut=(NewState#state_rcv.proto_opts)#proto_opts.idle_timeout,
            {next_state, wait_ack, NewState#state_rcv{socket=NewSocket}, TimeOut}
    end;
handle_info({erlang, _Socket, Data}, wait_ack, State) ->
    ?DebugF("erlang function result received: size=~p ~n",[size(term_to_binary(Data))]),
    case handle_data_msg(Data, State) of
        {NewState=#state_rcv{ack_done=true}, _Opts} ->
            handle_next_action(NewState#state_rcv{ack_done=false});
        {NewState, _Opts} ->
            TimeOut=(NewState#state_rcv.proto_opts)#proto_opts.idle_timeout,
            {next_state, wait_ack, NewState, TimeOut}
    end;
handle_info({udp, Socket,_IP,_InPortNo, Data}, StateName, State) ->
    ?DebugF("UDP packet received: size=~p ~n",[size(Data)]),
    %% we don't care about IP,InPortNo, do the same as for a tcp connection:
    handle_info({tcp, Socket, Data}, StateName, State);
%% inet close messages; persistent session, waiting for ack
handle_info({NetEvent, _Socket}, wait_ack,
            State = #state_rcv{persistent=true}) when NetEvent==tcp_closed;
                                                      NetEvent==ssl_closed ->
    ?LOG("connection closed while waiting for ack",?INFO),
    set_connected_status(false),
    {NewState, _Opts} = handle_data_msg(closed, State),
    %% socket should be closed in handle_data_msg
    handle_next_action(NewState#state_rcv{socket=none});

%% inet close messages; persistent session
handle_info({NetEvent, Socket}, think,
            State = #state_rcv{persistent=true}) when NetEvent==tcp_closed;
                                                      NetEvent==ssl_closed ->
    ?LOG("connection closed, stay alive (persistent)",?INFO),
    set_connected_status(false),
    catch ts_utils:close_socket(State#state_rcv.protocol, Socket), % mandatory for ssl
    {next_state, think, State#state_rcv{socket = none}};

%% inet close messages
handle_info({NetEvent, Socket}, _StateName, State) when NetEvent==tcp_closed;
                                                       NetEvent==ssl_closed ->
    ?LOG("connection closed, abort", ?WARN),
    %% the connexion was closed after the last msg was sent, stop quietly
    ts_mon:add({ count, error_closed }),
    set_connected_status(false),
    ts_utils:close_socket(State#state_rcv.protocol, Socket), % mandatory for ssl
    {stop, normal, State#state_rcv{socket = none}};

%% inet errors
handle_info({NetError, _Socket, Reason}, wait_ack, State)  when NetError==tcp_error;
                                                               NetError==ssl_error ->
    ?LOGF("Net error (~p): ~p~n",[NetError, Reason], ?WARN),
    CountName="error_inet_"++atom_to_list(Reason),
    ts_mon:add({ count, list_to_atom(CountName) }),
    set_connected_status(false),
    {stop, normal, State};

%% timer expires, no more messages to send
handle_info({timeout, _Ref, end_thinktime}, think, State= #state_rcv{ count=0 })  ->
    ?LOG("Session ending ~n", ?INFO),
    {stop, normal, State};

%% the timer expires
handle_info({timeout, _Ref, end_thinktime}, think, State ) ->
    handle_next_action(State);

handle_info(timeout, StateName, State ) ->
    ?LOGF("Error: timeout receive in state ~p~n",[StateName], ?ERR),
    ts_mon:add({ count, timeout }),
    {stop, normal, State};
% bidirectional protocol
handle_info({NetEvent, Socket, Data}, think,State=#state_rcv{
  clienttype=Type, bidi=true,host=Host,port=Port})  when ((NetEvent == tcp) or (NetEvent==ssl)) ->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    ts_mon:add({ sum, size_rcv, size(Data)}),
    Proto = State#state_rcv.protocol,
    ?LOG("Data received from socket (bidi) in state think~n",?INFO),
    NewState = case Type:parse_bidi(Data, State) of
                   {nodata, State2} ->
                       ?LOG("Bidi: no data ~n",?DEB),
                       ts_mon:add({count, async_unknown_data_rcv}),
                       State2;
                   {Data2, State2} ->
                       ts_mon:add([{ sum, size_sent, size(Data2)},{count, async_data_rcv}]),
                       ts_mon:sendmes({State#state_rcv.dump, self(), Data2}),
                       ?LOG("Bidi: send data back to server~n",?DEB),
                       send(Proto,Socket,Data2,Host,Port), %FIXME: handle errors ?
                       State2
               end,
    NewSocket = ts_utils:inet_setopts(Proto, State#state_rcv.socket,
                                      [{active, once}]),
    {next_state, think, NewState#state_rcv{socket=NewSocket}};
% bidi is false, but parse is also false: continue even if we get data
handle_info({NetEvent, _Socket, Data}, think, State = #state_rcv{request=Req} )
  when (Req#ts_request.ack /= parse) and ((NetEvent == tcp) or (NetEvent==ssl)) ->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    ts_mon:add({ sum, size_rcv, size(Data)}),
    ?LOGF("Data receive from socket in state think, ack=~p, skip~n",
          [Req#ts_request.ack],?NOTICE),
    ?DebugF("Data was ~p~n",[Data]),
    NewSocket = ts_utils:inet_setopts(State#state_rcv.protocol, State#state_rcv.socket,
                                      [{active, once}]),
    {next_state, think, State#state_rcv{socket=NewSocket}};
handle_info({NetEvent, _Socket, Data}, think, State)
  when (NetEvent == tcp) or (NetEvent==ssl) ->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    ts_mon:add({ count, error_unknown_data }),
    ?LOG("Data receive from socket in state think, stop~n", ?ERR),
    ?DebugF("Data was ~p~n",[Data]),
    {stop, normal, State};
handle_info({inet_reply, _Socket,ok}, StateName, State ) ->
    ?LOGF("inet_reply ok received in state ~p~n",[StateName],?NOTICE),
    {next_state, StateName, State};
handle_info(Msg, StateName, State ) ->
    ?LOGF("Error: Unknown msg ~p receive in state ~p, stop~n", [Msg,StateName], ?ERR),
    ts_mon:add({ count, error_unknown_msg }),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%--------------------------------------------------------------------
terminate(normal, _StateName,State) ->
    finish_session(State);
terminate(Reason, StateName, State) ->
    ?LOGF("Stop in state ~p, reason= ~p~n",[StateName,Reason],?NOTICE),
    ts_mon:add({ count, error_unknown }),
    finish_session(State).

%%--------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: handle_next_action/1
%% Purpose: handle next action: thinktime, transaction or #ts_request
%% Args: State
%%----------------------------------------------------------------------
handle_next_action(State=#state_rcv{count=0}) ->
    ?LOG("Session ending ~n", ?INFO),
    {stop, normal, State};
handle_next_action(State) ->
    Count = State#state_rcv.count-1,
    case set_profile(State#state_rcv.maxcount,State#state_rcv.count,State#state_rcv.session_id) of
        {thinktime, Think} ->
            ?DebugF("Starting new thinktime ~p~n", [Think]),
            case (set_thinktime(Think) >= State#state_rcv.hibernate) of
                true ->
                    {next_state, think, State#state_rcv{count=Count},hibernate};
                _ ->
                    {next_state, think, State#state_rcv{count=Count}}
            end;
        {transaction, start, Tname} ->
            Now = now(),
            ?LOGF("Starting new transaction ~p (now~p)~n", [Tname,Now], ?INFO),
            TrList = State#state_rcv.transactions,
            NewState = State#state_rcv{transactions=[{Tname,Now}|TrList],
                                   count=Count},
            handle_next_action(NewState);
        {transaction, stop, Tname} ->
            Now = now(),
            ?LOGF("Stopping transaction ~p (~p)~n", [Tname, Now], ?INFO),
            TrList = State#state_rcv.transactions,
            {value, {_, Tr}} = lists:keysearch(Tname, 1, TrList),
            Elapsed = ts_utils:elapsed(Tr, Now),
            ts_mon:add({sample, Tname, Elapsed}),
            NewState = State#state_rcv{transactions=lists:keydelete(Tname,1,TrList),
                                   count=Count},
            handle_next_action(NewState);
        {setdynvars,SourceType,Args,VarNames} ->
            DynData=State#state_rcv.dyndata,
            Result = set_dynvars(SourceType,Args,VarNames,DynData),
            NewDynVars = ts_dynvars:set(VarNames,Result,DynData#dyndata.dynvars),
            NewDynData = DynData#dyndata{dynvars=NewDynVars},
            ?DebugF("set dynvars: ~p ~p~n",[NewDynVars, NewDynData]),
            handle_next_action(State#state_rcv{dyndata = NewDynData, count=Count});
        {ctrl_struct,CtrlData} ->
            ctrl_struct(CtrlData,State,Count);
        Request=#ts_request{} ->
            handle_next_request(Request, State);
        {change_type, NewCType, Server, Port, PType, Store, Restore} ->
            ?DebugF("Change client type, use: ~p ~p~n",[NewCType, [Server , Port, PType, Store, Restore]]),
            DynData=State#state_rcv.dyndata,
            case Store of
                true -> % keep state
                    put({state, State#state_rcv.clienttype} , {State#state_rcv.socket,State#state_rcv.session,DynData#dyndata.proto});
                false -> % don't keep state of old type, close connection
                    ts_utils:close_socket(State#state_rcv.protocol, State#state_rcv.socket),
                    set_connected_status(false)
            end,
            {Socket,Session,ProtoDynData} = case {Restore, get({state,NewCType})} of
                     {true,{OldSocket, OldSession,OldProtoDynData}} -> % restore is true and we have something stored
                         {OldSocket, OldSession,OldProtoDynData};
                     {_,_} -> % nothing to restore, or no restore asked, set new session
                         DD=NewCType:init_dynparams(),
                         {none,NewCType:new_session(),DD#dyndata.proto}
                 end,
            NewDynData=DynData#dyndata{proto=ProtoDynData},
            NewState=State#state_rcv{session=Session,socket=Socket,count=Count,clienttype=NewCType,protocol=PType,port=Port,host=Server,dyndata=NewDynData},
            handle_next_action(NewState);
        Other ->
            ?LOGF("Error: set profile return value is ~p (count=~p)~n",[Other,Count],?ERR),
            {stop, set_profile_error, State}
    end.


%%----------------------------------------------------------------------
%% @spec set_dynvars (Type::erlang|random|urandom|file, Args::tuple(),
%%                    Variables::list(), DynData::#dyndata{}) -> list()
%% @doc setting the value of several dynamic variables at once.
%% @end
%%----------------------------------------------------------------------
set_dynvars(erlang,{Module,Callback},_Vars,DynData) ->
    Module:Callback({self(),DynData#dyndata.dynvars});
set_dynvars(code,Fun,_Vars,DynData) ->
    Fun({self(),DynData#dyndata.dynvars});
set_dynvars(random,{number,Start,End},Vars,_DynData) ->
    lists:map(fun(_) -> integer_to_list(Start+random:uniform(End-Start)) end,Vars);
set_dynvars(random,{string,Length},Vars,_DynData) ->
    R = fun(_) -> ts_utils:randomstr(Length) end,
    lists:map(R,Vars);
set_dynvars(urandom,{string,Length},Vars,_DynData) ->
    %% not random, but much faster
    RS= ts_utils:urandomstr(Length),
    N=length(Vars),
    lists:duplicate(N,RS);
set_dynvars(file,{random,FileId,Delimiter},_Vars,_DynData) ->
    {ok,Line} = ts_file_server:get_random_line(FileId),
    ts_utils:split(Line,Delimiter);
set_dynvars(file,{iter,FileId,Delimiter},_Vars,_DynData) ->
    {ok,Line} = ts_file_server:get_next_line(FileId),
    ts_utils:split(Line,Delimiter).

%% @spec ctrl_struct(CtrlData::term(),State::#state_rcv{},Count::integer) ->
%%          {next_state, NextStateName::atom(), NextState::#state_rcv{}} |
%%          {next_state, NextStateName::atom(), NextState::#state_rcv{},
%%                       Timeout::integer() | infinity} |
%%          {stop, Reason::term(), NewState::#state_rcv{}}
%% @doc Common code for flow control actions (repeat,for)
%%      Count is the next action-id, if this action doesn't result
%%      in a jump to another place
%% @end
ctrl_struct(CtrlData,State,Count) ->
    case ctrl_struct_impl(CtrlData,State#state_rcv.dyndata) of
        {next,NewDynData} ->
            handle_next_action(State#state_rcv{dyndata=NewDynData,count=Count});
        {jump,Target,NewDynData} ->
            %%UGLY HACK:
            %% because set_profile/3 works by counting down starting at maxcount,
            %% we need to calculate the correct value to actually make a jump to
            %% the desired target.
            %% In set_profile/3, actionId = MaxCount-Count+1 =>
            %% Count = MaxCount-Target +1
            Next = State#state_rcv.maxcount - Target + 1,
            handle_next_action(State#state_rcv{dyndata=NewDynData,count=Next})
    end.


%%----------------------------------------------------------------------
%% @spec ctrl_struct_impl(ControlStruct::term(),DynData::#dyndata{}) ->
%%        {next,NewDynData::#dyndata{}} |
%%        {jump, Target::integer(), NewDynData::#dyndata{}}
%% @doc return {next,NewDynData} to continue with the sequential flow,
%%             {jump,Target,NewDynData} to jump to action number 'Target'
%% @end
%%----------------------------------------------------------------------
ctrl_struct_impl({for_start,InitialValue,VarName},DynData=#dyndata{dynvars=DynVars}) ->
    NewDynVars = ts_dynvars:set(VarName,InitialValue,DynVars),
    {next,DynData#dyndata{dynvars=NewDynVars}};
ctrl_struct_impl({for_end,VarName,EndValue,Increment,Target},DynData=#dyndata{dynvars=DynVars}) ->
    case ts_dynvars:lookup(VarName,DynVars) of
        {ok,Value}  when Value >= EndValue -> % Reach final value, end loop
            {next,DynData};
        {ok,Value} ->  % New iteration
            NewValue = Value + Increment,
            NewDynVars = ts_dynvars:set(VarName,NewValue,DynVars),
            {jump,Target,DynData#dyndata{dynvars=NewDynVars}}
    end;


ctrl_struct_impl({if_start,Rel, VarName, Value, Target},DynData=#dyndata{dynvars=DynVars}) ->
    case ts_dynvars:lookup(VarName,DynVars) of
        {ok,VarValue} ->
            ?DebugF("If found ~p; value is ~p~n",[VarName,VarValue]),
            ?DebugF("Calling need_jump with args ~p ~p ~p~n",[Rel,Value,VarValue]),
            Jump = need_jump('if',rel(Rel,Value,VarValue)),
            jump_if(Jump,Target,DynData#dyndata{dynvars=DynVars});
        false ->
            ts_mon:add({ count, 'error_if_undef'}),
            {next,DynData}
    end;

ctrl_struct_impl({repeat,RepeatName, _,_,_,_,_,_},DynData=#dyndata{dynvars=[]}) ->
    Msg= list_to_atom("error_repeat_"++atom_to_list(RepeatName)++"_undef"),
    ts_mon:add({ count, Msg}),
    {next,DynData};
ctrl_struct_impl({repeat,RepeatName, While,Rel,VarName,Value,Target,Max},
                 DynData=#dyndata{dynvars=DynVars}) ->
    Iteration = case ts_dynvars:lookup(RepeatName,DynVars) of
                    {ok,Val} -> Val;
                    false ->  1
                end,
    ?DebugF("Repeat (name=~p) iteration: ~p~n",[RepeatName,Iteration]),
    case Iteration > Max of
        true ->
            ?LOGF("Max repeat (name=~p) reached ~p~n",[VarName,Iteration],?NOTICE),
            ts_mon:add({ count, max_repeat}),
            {next,DynData};
        false ->
            case ts_dynvars:lookup(VarName,DynVars) of
                {ok,VarValue} ->
                    ?DebugF("Repeat (name=~p) found; value is ~p~n",[VarName,VarValue]),
                    ?DebugF("Calling need_jump with args ~p ~p ~p ~p~n",[While,Rel,Value,VarValue]),
                    Jump = need_jump(While,rel(Rel,Value,VarValue)),
                    NewValue = 1 + Iteration,
                    NewDynVars = ts_dynvars:set(RepeatName,NewValue,DynVars),
                    jump_if(Jump,Target,DynData#dyndata{dynvars=NewDynVars});
                false ->
                    Msg= list_to_atom("error_repeat_"++atom_to_list(RepeatName)++"undef"),
                    ts_mon:add({ count, Msg}),
                    {next,DynData}
            end
    end.

rel('eq',A,B)  -> A == B;
rel('neq',A,B) -> A /= B.

need_jump('while',F) -> F;
need_jump('until',F) -> not F;
need_jump('if',F) -> not F.

jump_if(true,Target,DynData)   -> {jump,Target,DynData};
jump_if(false,_Target,DynData) -> {next,DynData}.


%%----------------------------------------------------------------------
%% Func: handle_next_request/2
%% Args: Request, State
%%----------------------------------------------------------------------
handle_next_request(Request, State) ->
    Count = State#state_rcv.count-1,
    Type  = State#state_rcv.clienttype,

    {PrevHost, PrevPort, PrevProto} = case Request of
        #ts_request{host=undefined, port=undefined, scheme=undefined} ->
            %% host/port/scheme not defined in request, use the current ones.
            {State#state_rcv.host,State#state_rcv.port, State#state_rcv.protocol};
        #ts_request{host=H1, port=P1, scheme=S1} ->
            {H1,P1,S1}
    end,

    {Param, {Host,Port,Protocol}} =
        case Type:add_dynparams(Request#ts_request.subst,
                                State#state_rcv.dyndata,
                                Request#ts_request.param,
                                {PrevHost, PrevPort}) of
            {Par, NewServer} -> % substitution has changed server setup
                ?DebugF("Dynparam, new server:  ~p~n",[NewServer]),
                {Par, NewServer};
            P ->
                {P, {PrevHost, PrevPort, PrevProto}}
        end,

    %% need to reconnect if the server/port/scheme has changed
    Socket = case {State#state_rcv.host,State#state_rcv.port,State#state_rcv.protocol} of
                 {Host, Port, Protocol} -> % server setup unchanged
                     State#state_rcv.socket;
                 _ ->
                     ?Debug("Change server configuration inside a session ~n"),
                     ts_utils:close_socket(State#state_rcv.protocol,
                                           State#state_rcv.socket),
                     set_connected_status(false),
                     none
             end,

    Message = Type:get_message(Param),
    Now = now(),

    %% reconnect if needed
    Proto = {Protocol,State#state_rcv.proto_opts},
    case reconnect(Socket,Host,Port,Proto,State#state_rcv.ip) of
        {ok, NewSocket} ->
            case catch send(Protocol, NewSocket, Message, Host, Port) of
                ok ->
                    PageTimeStamp = case State#state_rcv.page_timestamp of
                                        0 -> Now; %first request of a page
                                        _ -> %page already started
                                            State#state_rcv.page_timestamp
                                    end,
                    ts_mon:add({ sum, size_sent, size_msg(Message)}),
                    ts_mon:sendmes({State#state_rcv.dump, self(), Message}),
                    NewState = State#state_rcv{socket   = NewSocket,
                                               protocol = Protocol,
                                               host     = Host,
                                               request  = Request,
                                               port     = Port,
                                               count    = Count,
                                               page_timestamp= PageTimeStamp,
                                               send_timestamp= Now,
                                               timestamp= Now },
                    case Request#ts_request.ack of
                        no_ack ->
                            {PTimeStamp, _} = update_stats_noack(NewState),
                            handle_next_action(NewState#state_rcv{ack_done=true, page_timestamp=PTimeStamp});
                        global ->
                            ts_timer:connected(self()),
                            {next_state, wait_ack, NewState};
                        _ ->
                            {next_state, wait_ack, NewState}
                        end;
                {error, closed} ->
                    ?LOG("connection close while sending message !~n", ?WARN),
                    handle_close_while_sending(State#state_rcv{socket=NewSocket,
                                                               protocol=Protocol,
                                                               host=Host,
                                                               port=Port});
                {error, Reason} ->
                    ?LOGF("Error: Unable to send data, reason: ~p~n",[Reason],?ERR),
                    CountName="error_send_"++atom_to_list(Reason),
                    ts_mon:add({ count, list_to_atom(CountName) }),
                     handle_timeout_while_sending(State);
                {'EXIT', {noproc, _Rest}} ->
                    ?LOG("EXIT from ssl app while sending message !~n", ?WARN),
                    handle_close_while_sending(State#state_rcv{socket=NewSocket,
                                                               protocol=Protocol,
                                                               host=Host,
                                                               port=Port});
                Exit ->
                    ?LOGF("EXIT Error: Unable to send data, reason: ~p~n",
                          [Exit], ?ERR),
                    ts_mon:add({ count, error_send }),
                    {stop, normal, State}
            end;
        {error,_Reason} when State#state_rcv.retries < ?MAX_RETRIES ->
            Retries= State#state_rcv.retries +1,
            % simplified exponential backoff algorithm: we increase
            % the timeout when the number of retries increase, with a
            % simple rule: number of retries * retry_timeout
            set_thinktime(?RETRY_TIMEOUT *  Retries ),
            {next_state, think, State#state_rcv{retries=Retries}};
        {error,_Reason}  ->
            ts_mon:add({ count, error_abort_max_conn_retries }),
            {stop, normal, State}
    end.


%% @spec size_msg(Data::term) -> integer()
size_msg(Data) when is_binary(Data) ->
     size(Data);
size_msg({_Mod,_Fun,_Args,Size}) -> Size.

%%----------------------------------------------------------------------
%% Func: finish_session/1
%% Args: State
%%----------------------------------------------------------------------
finish_session(State) ->
    Now = now(),
    set_connected_status(false),
    Elapsed = ts_utils:elapsed(State#state_rcv.starttime, Now),
    case State#state_rcv.transactions of
        [] -> % no pending transactions, do nothing
            ok;
        TrList -> % pending transactions (an error has probably occured)
            ?LOGF("Pending transactions: ~p, compute transaction time~n",[TrList],?NOTICE),
            lists:foreach(fun({Tname,StartTime}) ->
                                  ts_mon:add({sample,Tname,ts_utils:elapsed(StartTime,Now)})
                          end,
                          TrList)
    end,
    ts_mon:endclient({State#state_rcv.id, Now, Elapsed}).

%%----------------------------------------------------------------------
%% Func: handle_close_while_sending/1
%% Args: State
%% Purpose: the connection has just be closed a few msec before we
%%          send a message, restart in a few moment (this time we will
%%          reconnect before sending)
%%----------------------------------------------------------------------
handle_close_while_sending(State=#state_rcv{persistent = true,
                                            protocol   = Proto,
                                            proto_opts = PO})->
    ts_utils:close_socket(Proto, State#state_rcv.socket),
    set_connected_status(false),
    Think = PO#proto_opts.retry_timeout,
    %%FIXME: report the error to ts_mon ?
    ?LOGF("Server must have closed connection upon us, waiting ~p msec~n",
          [Think], ?NOTICE),
    set_thinktime(Think),
    {next_state, think, State#state_rcv{socket=none}};
handle_close_while_sending(State) ->
    {stop, error, State}.


%%----------------------------------------------------------------------
%% Func: handle_timeout_while_sending/1
%% Args: State
%% Purpose: retry if a timeout occurs during a send
%%----------------------------------------------------------------------
handle_timeout_while_sending(State=#state_rcv{persistent = true,
                                              proto_opts = PO})->
    Think = PO#proto_opts.retry_timeout,
    set_thinktime(Think),
    {next_state, think, State};
handle_timeout_while_sending(State) ->
    ?LOG("Not persistent, abort client because of send timeout~n", ?INFO),
    {stop, normal, State}.


%%----------------------------------------------------------------------
%% Func: set_profile/2
%% Args: MaxCount, Count (integer), ProfileId (integer)
%%----------------------------------------------------------------------
set_profile(MaxCount, Count, ProfileId) when is_integer(ProfileId) ->
    ts_session_cache:get_req(ProfileId, MaxCount-Count+1).

%%----------------------------------------------------------------------
%% Func: reconnect/4
%% Returns: {Socket   }          |
%%          {stop, Reason}
%% purpose: try to reconnect if this is needed (when the socket is set to none)
%%----------------------------------------------------------------------
reconnect(none, ServerName, Port, {Protocol, Proto_opts}, {IP,CPort}) ->
    ?DebugF("Try to (re)connect to: ~p:~p from ~p using protocol ~p~n",
            [ServerName,Port,IP,Protocol]),
    Opts = protocol_options(Protocol, Proto_opts)  ++ [{ip, IP},{port,CPort}],
    Before= now(),
    case connect(Protocol,ServerName, Port, Opts) of
        {ok, Socket} ->
            Elapsed = ts_utils:elapsed(Before, now()),
            ts_mon:add({ sample, connect, Elapsed }),
            set_connected_status(true),
            ?Debug("(Re)connected~n"),
            {ok, Socket};
        {error, Reason} ->
                    {A,B,C,D} = IP,
            ?LOGF("(Re)connect from ~p.~p.~p.~p to ~s:~p, Error: ~p~n",
                  [A,B,C,D, ServerName, Port, Reason],?ERR),
            CountName="error_connect_"++atom_to_list(Reason),
            ts_mon:add({ count, list_to_atom(CountName) }),
            {error, Reason}
    end;
reconnect(Socket, _Server, _Port, _Protocol, _IP) ->
    {ok, Socket}.

%%----------------------------------------------------------------------
%% Func: send/5
%% Purpose: wrapper function for send
%% Return: ok | {error, Reason}
%%----------------------------------------------------------------------
send(gen_tcp,Socket,Message,_,_) -> gen_tcp:send(Socket,Message);
send(ssl,Socket,Message,_,_)     -> ssl:send(Socket,Message);
send(gen_udp,Socket,Message,Host,Port) ->gen_udp:send(Socket,Host,Port,Message);
send(erlang,Pid,Message,_,_) ->
    Pid ! Message,
    ok.

%%----------------------------------------------------------------------
%% Func: connect/4
%% Return: {ok, Socket} | {error, Reason}
%%----------------------------------------------------------------------
connect(gen_tcp,Server, Port, Opts)  -> gen_tcp:connect(Server, Port, Opts);
connect(ssl,Server, Port,Opts)       -> ssl:connect(Server, Port, Opts);
connect(gen_udp,_Server, _Port, Opts)-> gen_udp:open(0,Opts);
connect(erlang,Server,Port,Opts)     ->
    Pid=spawn_link(ts_erlang,client,[self(),Server,Port,Opts]),
    {ok, Pid}.


%%----------------------------------------------------------------------
%% Func: protocol_options/1
%% Purpose: set connection's options for the given protocol
%%----------------------------------------------------------------------
protocol_options(ssl,#proto_opts{ssl_ciphers=negociate}) ->
    [binary, {active, once} ];
protocol_options(ssl,#proto_opts{ssl_ciphers=Ciphers}) ->
    ?DebugF("cipher is ~p~n",[Ciphers]),
    [binary, {active, once}, {ciphers, Ciphers} ];

protocol_options(gen_tcp,#proto_opts{tcp_rcv_size=Rcv, tcp_snd_size=Snd}) ->
    [binary,
     {active, once},
     {recbuf, Rcv},
     {sndbuf, Snd},
     {keepalive, true} %% FIXME: should be an option
    ];
protocol_options(gen_udp,#proto_opts{udp_rcv_size=Rcv, udp_snd_size=Snd}) ->
    [binary,
     {active, once},
     {recbuf, Rcv},
     {sndbuf, Snd}
    ];
protocol_options(erlang,_) -> [].



%%----------------------------------------------------------------------
%% Func: set_thinktime/1
%% Purpose: set a timer for thinktime if it is not infinite
%%          returns the choosen thinktime in msec
%%----------------------------------------------------------------------
set_thinktime(infinity) -> infinity;
set_thinktime({random, Think}) ->
    set_thinktime(round(ts_stats:exponential(1/Think)));
set_thinktime({range, Min, Max}) ->
    set_thinktime(ts_stats:uniform(Min,Max));
set_thinktime(Think) ->
%% dot not use timer:send_after because it does not scale well:
%% http://www.erlang.org/ml-archive/erlang-questions/200202/msg00024.html
    ?DebugF("thinktime of ~p~n",[Think]),
    erlang:start_timer(Think, self(), end_thinktime ),
    Think.


%%----------------------------------------------------------------------
%% Func: handle_data_msg/2
%% Args: Data (binary), State ('state_rcv' record)
%% Returns: {NewState ('state_rcv' record), Socket options (list)}
%% Purpose: handle data received from a socket
%%----------------------------------------------------------------------
handle_data_msg(Data, State=#state_rcv{request=Req}) when Req#ts_request.ack==no_ack->
    ?Debug("data received while previous msg was no_ack~n"),
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    {State, []};

handle_data_msg(Data,State=#state_rcv{request=Req,clienttype=Type,maxcount=MaxCount})
  when Req#ts_request.ack==parse->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),

    {NewState, Opts, Close} = Type:parse(Data, State),
    NewBuffer=set_new_buffer(Req, NewState#state_rcv.buffer, Data),

    ?DebugF("Dyndata is now ~p~n",[NewState#state_rcv.dyndata]),
    case NewState#state_rcv.ack_done of
        true ->
            ?DebugF("Response done:~p~n", [NewState#state_rcv.datasize]),
            {PageTimeStamp, DynVars} = update_stats(NewState#state_rcv{buffer=NewBuffer}),
            MatchArgs={NewState#state_rcv.count, MaxCount,
                       NewState#state_rcv.session_id,
                       NewState#state_rcv.id},
            NewDynVars=ts_dynvars:merge(DynVars,(NewState#state_rcv.dyndata)#dyndata.dynvars),
            NewCount  =ts_search:match(Req#ts_request.match,NewBuffer,MatchArgs,NewDynVars),
            NewDynData=(NewState#state_rcv.dyndata)#dyndata{dynvars=NewDynVars},
            case Close of
                true ->
                    ?Debug("Close connection required by protocol~n"),
                    ts_utils:close_socket(State#state_rcv.protocol,State#state_rcv.socket),
                    set_connected_status(false),
                    {NewState#state_rcv{ page_timestamp = PageTimeStamp,
                                         socket = none,
                                         datasize = 0,
                                         size_mon_thresh= ?size_mon_thresh,
                                         count = NewCount,
                                         dyndata = NewDynData,
                                         buffer = <<>>}, Opts};
                false ->
                    {NewState#state_rcv{ page_timestamp = PageTimeStamp,
                                         count = NewCount,
                                         size_mon_thresh= ?size_mon_thresh,
                                         datasize = 0,
                                         dyndata = NewDynData,
                                         buffer = <<>>}, Opts}
            end;
        _ ->
            ?DebugF("Response: continue:~p~n",[NewState#state_rcv.datasize]),
            %% For size_rcv stats, we don't want to update this stats
            %% for every packet received (ts_mon will be overloaded),
            %% so we will update the stats at the end of the
            %% request. But this is a problem with very big response
            %% (several megabytes for ex.), because it will create
            %% artificial spikes in the stats (O B/sec for a long time
            %% and lot's of MB/s at the end of the req). So we update
            %% the stats each time a 512Ko threshold is raised.
            case NewState#state_rcv.datasize > NewState#state_rcv.size_mon_thresh of
                true ->
                    ?Debug("Threshold raised, update size_rcv stats~n"),
                    ts_mon:add({ sum, size_rcv, ?size_mon_thresh}),
                    NewThresh=NewState#state_rcv.size_mon_thresh+ ?size_mon_thresh,
                    {NewState#state_rcv{buffer=NewBuffer,size_mon_thresh=NewThresh}, Opts};
                false->
                    {NewState#state_rcv{buffer=NewBuffer}, Opts}
            end
    end;

handle_data_msg(closed,State) ->
    {State,[]};

%% ack = global
handle_data_msg(Data,State=#state_rcv{request=Req,datasize=OldSize})
  when Req#ts_request.ack==global ->
    %% FIXME: we do not report size now (but after receiving the
    %% global ack), the size stats may be not very accurate.
    %% FIXME: should we set buffer and parse for dynvars ?
    DataSize = size(Data),
    {State#state_rcv{ datasize = OldSize + DataSize},[]};

%% local ack, special case for jabber: skip keepalive msg (single space char)
handle_data_msg(<<32>>, State=#state_rcv{clienttype=ts_jabber}) ->
    {State#state_rcv{ack_done = false},[]};
%% local ack, set ack_done to true
handle_data_msg(Data, State=#state_rcv{request=Req, maxcount= MaxCount}) ->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    NewBuffer= set_new_buffer(Req, State#state_rcv.buffer, Data),
    DataSize = size(Data),
    {PageTimeStamp, DynVars} = update_stats(State#state_rcv{datasize=DataSize,
                                                            buffer=NewBuffer}),
    MatchArgs={State#state_rcv.count,MaxCount,State#state_rcv.session_id,
               State#state_rcv.id},
    NewDynVars=ts_dynvars:merge(DynVars,(State#state_rcv.dyndata)#dyndata.dynvars),
    NewCount  =ts_search:match(Req#ts_request.match, NewBuffer, MatchArgs,NewDynVars),
    NewDynData=(State#state_rcv.dyndata)#dyndata{dynvars=NewDynVars},
    {State#state_rcv{ack_done = true, buffer= NewBuffer, dyndata = NewDynData,
                     page_timestamp= PageTimeStamp, count=NewCount},[]}.


%%----------------------------------------------------------------------
%% Func: set_new_buffer/3
%%----------------------------------------------------------------------
set_new_buffer(#ts_request{match=[], dynvar_specs=[]},_,_) ->
    << >>;
set_new_buffer(_, Buffer,closed) ->
    Buffer;
set_new_buffer(_, OldBuffer, Data) when is_binary(OldBuffer)->
    ?Debug("Bufferize response~n"),
    << OldBuffer/binary, Data/binary >>;
set_new_buffer(_, _, Data) -> % don't need buffer for non binary responses (erlang fun case)
    Data.

%%----------------------------------------------------------------------
%% Func: set_connected_status/1
%% Args: true|false
%% Returns: -
%% Purpose: update the statistics for connected users
%%----------------------------------------------------------------------
set_connected_status(S) ->
    set_connected_status(S,get(connected)).
set_connected_status(true, true) ->
    ok;
set_connected_status(true, Old) when Old==undefined; Old==false ->
    put(connected,true),
    ts_mon:add({sum, connected, 1});
set_connected_status(false, true) ->
    put(connected,false),
    ts_mon:add({sum, connected, -1});
set_connected_status(false, Old) when Old==undefined; Old==false ->
    ok.


%%----------------------------------------------------------------------
%% Func: update_stats_noack/1
%% Args: State
%% Returns: {TimeStamp, DynVars}
%% Purpose: update the statistics for no_ack requests
%%----------------------------------------------------------------------
update_stats_noack(#state_rcv{page_timestamp=PageTime,request=Request}) ->
    Now = now(),
    Stats= [{ count, request_noack}], % count and not sample because response time is not defined in this case
    case Request#ts_request.endpage of
        true -> % end of a page, compute page reponse time
            PageElapsed = ts_utils:elapsed(PageTime, Now),
            ts_mon:add(lists:append([Stats,[{sample, page, PageElapsed}]])),
            {0, []};
        _ ->
            ts_mon:add(Stats),
            {PageTime, []}
    end.

%%----------------------------------------------------------------------
%% Func: update_stats/1
%% Args: State
%% Returns: {TimeStamp, DynVars}
%% Purpose: update the statistics
%%----------------------------------------------------------------------
update_stats(State=#state_rcv{page_timestamp=PageTime,send_timestamp=SendTime}) ->
    Now = now(),
    Elapsed = ts_utils:elapsed(SendTime, Now),
    Stats = case   State#state_rcv.size_mon_thresh > ?size_mon_thresh of
                true ->
                    LastSize=State#state_rcv.datasize-State#state_rcv.size_mon_thresh+?size_mon_thresh,
                    [{ sample, request, Elapsed},
                     { sum, size_rcv, LastSize}];
                false->
                    [{ sample, request, Elapsed},
                     { sum, size_rcv, State#state_rcv.datasize}]
            end,
    Request = State#state_rcv.request,
    DynVars = ts_search:parse_dynvar(Request#ts_request.dynvar_specs,
                                     State#state_rcv.buffer),
    case Request#ts_request.endpage of
        true -> % end of a page, compute page reponse time
            PageElapsed = ts_utils:elapsed(PageTime, Now),
            ts_mon:add(lists:append([Stats,[{sample, page, PageElapsed}]])),
            {0, DynVars};
        _ ->
            ts_mon:add(Stats),
            {PageTime, DynVars}
    end.

