%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(snmp_mgr).

%%----------------------------------------------------------------------
%% This module implements a simple SNMP manager for Erlang.
%%----------------------------------------------------------------------

%% c(snmp_mgr).
%% snmp_mgr:start().
%% snmp_mgr:g([[sysContact,0]]).

%% snmp_mgr:start([{engine_id, "mbjk's engine"}, v3, {agent, "clip"}, {mibs, ["../mibs/SNMPv2-MIB"]}]).

%% snmp_mgr:start([{engine_id, "agentEngine"}, {user, "iwl_test"}, {dir, "mgr_conf"}, {sec_level, authPriv}, v3, {agent, "clip"}]).

%% User interface
-export([start_link/1, start/1, stop/0, 
	 d/0, g/1, s/1, gn/1, gn/0, r/0, gb/3, rpl/1,
	 send_bytes/1,
	 expect/2,expect/3,expect/4,expect/6,get_response/2, 
	 receive_response/0,
	 oid_to_name/1, name_to_oid/1]).

%% Internal exports
-export([get_oid_from_varbind/1, 
	 var_and_value_to_varbind/2, flatten_oid/2, make_vb/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/src/misc/snmp_debug.hrl").
-include_lib("snmp/include/STANDARD-MIB.hrl").

-record(state,{dbg         = true,
	       quiet,
	       parent,
	       timeout     = 3500,
	       print_traps = true,
	       mini_mib,
	       packet_server, 
	       last_sent_pdu, 
	       last_received_pdu}).

start_link(Options) ->
    gen_server:start_link({local, snmp_mgr}, snmp_mgr, {Options, self()}, []).

start(Options) ->
    gen_server:start({local, snmp_mgr}, snmp_mgr, {Options, self()}, []).

stop() ->
    gen_server:call(snmp_mgr, stop, infinity).

d() ->
    gen_server:call(snmp_mgr,discovery,infinity).

g(Oids) ->
    snmp_mgr ! {get, Oids}, ok.

%% VarsAndValues is: {PlainOid, o|s|i, Value} (unknown mibs) | {Oid, Value} 
s(VarsAndValues) ->
    snmp_mgr ! {set, VarsAndValues}, ok.

gn(Oids) when list(Oids) ->
    snmp_mgr ! {get_next, Oids}, ok;
gn(N) when integer(N) ->
    snmp_mgr ! {iter_get_next, N}, ok.
gn() ->
    snmp_mgr ! iter_get_next, ok.

r() ->
    snmp_mgr ! resend_pdu, ok.

gb(NonRepeaters, MaxRepetitions, Oids) ->
    snmp_mgr ! {bulk, {NonRepeaters, MaxRepetitions, Oids}}, ok.

rpl(RespPdu) ->
    snmp_mgr ! {response, RespPdu}.

send_bytes(Bytes) ->
    snmp_mgr ! {send_bytes, Bytes}, ok.

oid_to_name(Oid) ->
    gen_server:call(snmp_mgr, {oid_to_name, Oid}, infinity).

name_to_oid(Name) ->
    gen_server:call(snmp_mgr, {name_to_oid, Name}, infinity).


%%----------------------------------------------------------------------
%% Purpose: For writing test sequences
%% Args: Y=any (varbinds) | trap | timeout | VarBinds | ErrStatus
%% Returns: ok|{error, Id, Reason}
%%----------------------------------------------------------------------
expect(Id,Y) -> echo_errors(expect_impl(Id,Y)).
expect(Id,v2trap,VBs) -> echo_errors(expect_impl(Id,v2trap,VBs));
expect(Id,report,VBs) -> echo_errors(expect_impl(Id,report,VBs));
expect(Id,{inform, Reply},VBs) ->
    echo_errors(expect_impl(Id,{inform,Reply},VBs)).
expect(Id,Err,Idx,VBs) -> echo_errors(expect_impl(Id,Err,Idx,VBs)).
expect(Id,trap, Enterp, Generic, Specific, ExpectedVarbinds) ->
    echo_errors(expect_impl(Id,trap,Enterp,Generic,
			    Specific,ExpectedVarbinds)).

%%-----------------------------------------------------------------
%% Purpose: For writing test sequences
%%-----------------------------------------------------------------
get_response(Id, Vars) -> echo_errors(get_response_impl(Id, Vars)).

%%----------------------------------------------------------------------
%% Receives a response from the agent.
%% Returns: a PDU or {error, Reason}.
%% It doesn't receive traps though.
%%----------------------------------------------------------------------
receive_response() ->
    receive_response(get_timeout()).

receive_response(Timeout) ->
    d("await reponse within ~w ms",[Timeout]),
    receive
	{snmp_pdu, PDU} when record(PDU, pdu) ->
	    d("received PDU: ~n\t~p",[PDU]),
	    PDU
    after Timeout ->
	    d("response timeout",[]),
	    {error, timeout}
    end.


get_timeout() ->
    get_timeout(os:type()).

get_timeout(vxworks) -> 7000;
get_timeout(_)       -> 3500.

%%----------------------------------------------------------------------
%% Receives a trap from the agent.
%% Returns: TrapPdu|{error, Reason}
%%----------------------------------------------------------------------
receive_trap(Timeout) ->
    d("await trap within ~w ms",[Timeout]),
    receive
	{snmp_pdu, PDU} when record(PDU, trappdu) ->
	    d("received trap-PDU: ~n\t~p",[PDU]),
	    PDU
    after Timeout ->
	    d("trap timeout",[]),
	    {error, timeout}
    end.

%%----------------------------------------------------------------------
%% Options: List of
%%  {agent_udp, UDPPort},  {agent, Agent}
%%  Optional: 
%%  {community, String ("public" is default}, quiet,
%%  {mibs, List of Filenames}, {trap_udp, UDPPort (default 5000)},
%%----------------------------------------------------------------------
init({Options, CallerPid}) ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    case (catch is_options_ok(Options)) of
	true ->
	    put(debug,get_value(debug,Options,false)),
	    d("init -> (~p) extract options",[self()]),
 	    PacksDbg = get_value(packet_server_debug,Options,false),
	    RecBufSz = get_value(recbuf,Options,1024),
	    Mibs     = get_value(mibs, Options, []),
	    Udp      = get_value(agent_udp, Options, 4000),
	    User     = get_value(user, Options, "initial"),
	    EngineId = get_value(engine_id, Options, "agentEngine"),
	    CtxEngineId = get_value(context_engine_id, Options, EngineId),
	    TrapUdp  = get_value(trap_udp, Options, 5000),
	    Dir      = get_value(dir, Options, "."),
	    SecLevel = get_value(sec_level, Options, noAuthNoPriv),
        MiniMIB  = case ts_utils:release_is_newer_or_eq("5.5") of
                       true ->
                           snmp_mini_mib:create(Mibs);
                       false ->
                           snmp_misc:make_mini_mib(Mibs)
                   end,
	    Version  = case lists:member(v2,Options) of
			   true -> 'version-2';
			   false -> 
			       case lists:member(v3,Options) of
				   true -> 'version-3';
				   false -> 'version-1'
			       end
		       end,
	    Com = case Version of
		      'version-3' ->
			  get_value(context, Options, "");
		      _ ->
			  get_value(community, Options, "public")
		  end,
	    VsnHdrD = 
		{Com, User, EngineId, CtxEngineId, mk_seclevel(SecLevel)},
	    AgIp = case snmp_misc:assq(agent, Options) of
		       {value, Tuple4} when tuple(Tuple4),size(Tuple4)==4 ->
			   Tuple4;
		       {value, Host} when list(Host) ->
			   {ok, Ip} = snmp_misc:ip(Host),
			   Ip
		   end,
	    Quiet = lists:member(quiet, Options),
	    PackServ = start_packet_server(Quiet, Options, CallerPid, 
					   AgIp, Udp, TrapUdp, 
					   VsnHdrD, Version, Dir, RecBufSz, 
					   PacksDbg),
	    d("init -> packet server: ~p",[PackServ]),
	    State = #state{parent        = CallerPid,
			   quiet         = Quiet,
			   mini_mib      = MiniMIB, 
			   packet_server = PackServ},
	    d("init -> done",[]),
	    {ok, State};
	
	{error, Reason} -> 
	    {stop,Reason}
    end.

start_packet_server(false, _Options, _CallerPid, AgIp, Udp, TrapUdp, 
		    VsnHdrD, Version, Dir, RecBufSz, PacksDbg) ->
    d("start_packet_server -> entry", []),
    snmp_mgr_misc:start_link_packet({msg, self()}, 
				    AgIp, Udp, TrapUdp, 
				    VsnHdrD, Version, Dir, RecBufSz,
				    PacksDbg);
start_packet_server(true, Options, CallerPid, AgIp, Udp, TrapUdp, 
		    VsnHdrD, Version, Dir, RecBufSz, PacksDbg) ->
    Type =  get_value(receive_type, Options, pdu),
    d("start_packet_server -> entry with"
      "~n   CallerPid: ~p"
      "~n   when"
      "~n   Type:      ~p",[CallerPid, Type]),
    snmp_mgr_misc:start_link_packet({Type, CallerPid}, 
				    AgIp, Udp, TrapUdp,
				    VsnHdrD, Version, Dir, RecBufSz,
				    PacksDbg).

is_options_ok([{mibs,List}|Opts]) when list(List) ->
    is_options_ok(Opts);
is_options_ok([quiet|Opts])  ->
    is_options_ok(Opts);
is_options_ok([{agent,_}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{agent_udp,Int}|Opts]) when integer(Int) ->
    is_options_ok(Opts);
is_options_ok([{trap_udp,Int}|Opts]) when integer(Int) ->
    is_options_ok(Opts);
is_options_ok([{community,List}|Opts]) when list(List) ->
    is_options_ok(Opts);
is_options_ok([{dir,List}|Opts]) when list(List) ->
    is_options_ok(Opts);
is_options_ok([{sec_level,noAuthNoPriv}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{sec_level,authNoPriv}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{sec_level,authPriv}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{context,List}|Opts]) when list(List) ->
    is_options_ok(Opts);
is_options_ok([{user,List}|Opts]) when list(List) ->
    is_options_ok(Opts);
is_options_ok([{engine_id,List}|Opts]) when list(List) ->
    is_options_ok(Opts);
is_options_ok([{context_engine_id,List}|Opts]) when list(List) ->
    is_options_ok(Opts);
is_options_ok([v1|Opts]) ->
    is_options_ok(Opts);
is_options_ok([v2|Opts]) ->
    is_options_ok(Opts);
is_options_ok([v3|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{debug,Bool}|Opts]) ->
    case is_bool(Bool) of
	ok ->
	    is_options_ok(Opts);
	error ->
	    {error, {bad_option, debug, Bool}}
    end;
is_options_ok([{packet_server_debug,Bool}|Opts]) ->
    case is_bool(Bool) of
	ok ->
	    is_options_ok(Opts);
	error ->
	    {error, {bad_option, packet_server_debug, Bool}}
    end;
is_options_ok([{recbuf,Sz}|Opts]) when 0 < Sz, Sz =< 65535 ->
    is_options_ok(Opts);
is_options_ok([{receive_type, msg}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{receive_type, pdu}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([InvOpt|_]) ->
    {error,{invalid_option,InvOpt}};
is_options_ok([]) -> true.

is_bool(true)  -> ok;
is_bool(false) -> ok;
is_bool(_)     -> error.

mk_seclevel(noAuthNoPriv) -> 0;
mk_seclevel(authNoPriv) -> 1;
mk_seclevel(authPriv) -> 3.
    

handle_info({get, Oids}, State) ->
    d("handle_info -> get request for ~p",[Oids]),
    {noreply, execute_request(get, Oids, State)};

handle_info({set, VariablesAndValues}, State) ->
    d("handle_info -> set request for ~p",[VariablesAndValues]),
    {noreply, execute_request(set, VariablesAndValues, State)};

handle_info({bulk, Args}, State) ->
    d("handle_info -> bulk request for ~p",[Args]),
    {noreply, execute_request(bulk, Args, State)};

handle_info({response, RespPdu}, State) ->
    d("handle_info -> response request with ~p",[RespPdu]),
    snmp_mgr_misc:send_pdu(RespPdu, State#state.packet_server),
    {noreply, State};

handle_info({snmp_msg, Msg, Ip, Udp}, State) ->
    io:format("* Got PDU: ~s", [snmp_mgr_misc:format_hdr(Msg)]),
    PDU = snmp_mgr_misc:get_pdu(Msg),
    echo_pdu(PDU, State#state.mini_mib),
    case PDU#pdu.type of
	'inform-request' ->
	    %% Generate a response...
	    RespPDU = PDU#pdu{type = 'get-response',
			      error_status = noError,
			      error_index = 0},
	    RespMsg = snmp_mgr_misc:set_pdu(Msg, RespPDU),
	    snmp_mgr_misc:send_msg(RespMsg, State#state.packet_server, Ip, Udp);
	_Else ->
	    ok
    end,
    {noreply, State#state{last_received_pdu = PDU}};

handle_info({get_next, Oids}, State) ->
    d("handle_info -> get-next request for ~p",[Oids]),
    {noreply, execute_request(get_next, Oids, State)};

handle_info(resend_pdu, State) ->
    PDU = State#state.last_sent_pdu,
    d("handle_info -> resend_pdu request when last sent pdu: ~n\t~p",[PDU]),
    send_pdu(PDU#pdu{request_id = make_request_id()},
	     State#state.mini_mib,
	     State#state.packet_server),
    {noreply, State};

handle_info(iter_get_next, State)
  when record(State#state.last_received_pdu, pdu) ->
    d("handle_info -> iter_get_next request",[]),
    PrevPDU = State#state.last_received_pdu,
    Oids = snmp_misc:map({snmp_mgr, get_oid_from_varbind}, [],
			 PrevPDU#pdu.varbinds),
    {noreply, execute_request(get_next, Oids, State)};

handle_info(iter_get_next, State) ->
    snmp_mgr_misc:error("[Iterated get-next] No Response PDU to "
			"start iterating from.", []),
    {noreply, State};

handle_info({iter_get_next, N}, State) ->
    d("handle_info -> iter_get_next(~p) request",[N]),
    if
	record(State#state.last_received_pdu, pdu) ->
	    PDU = get_next_iter_impl(N, State#state.last_received_pdu,
				     State#state.mini_mib,
				     State#state.packet_server),
	    {noreply, State#state{last_received_pdu = PDU}};
	true ->
	    snmp_mgr_misc:error("[Iterated get-next] No Response PDU to "
				"start iterating from.", []),
	    {noreply, State}
    end;

handle_info({send_bytes, Bytes}, State) ->
    d("handle_info -> send-bytes request for ~p bytes",
      [sizeOf(Bytes)]),
    snmp_mgr_misc:send_bytes(Bytes, State#state.packet_server),
    {noreply, State}.


handle_call({find_pure_oid, XOid}, _From, State) ->
    d("handle_call -> find_pure_oid for ~p",[XOid]),
    {reply, catch flatten_oid(XOid, State#state.mini_mib), State};

handle_call({oid_to_name, Oid}, _From, State) ->
    d("handle_call -> oid_to_name for Oid: ~p",[Oid]),
    Reply = 
	case lists:keysearch(Oid, 1, State#state.mini_mib) of
	    {value, {_Oid, Name, _Type}} ->
		{ok, Name};
	    false ->
		{error, {no_such_oid, Oid}}
	end,
    {reply, Reply, State};

handle_call({name_to_oid, Name}, _From, State) ->
    d("handle_call -> name_to_oid for Name: ~p",[Name]),
    Reply = 
	case lists:keysearch(Name, 2, State#state.mini_mib) of
	    {value, {Oid, _Name, _Type}} ->
		{ok, Oid};
	    false ->
		{error, {no_such_name, Name}}
	end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    d("handle_call -> stop request",[]),
    {stop, normal, ok, State};

handle_call(discovery, _From, State) ->
    d("handle_call -> discovery",[]),
    {Reply, NewState} = execute_discovery(State),
    {reply, Reply, NewState}.
    
handle_cast(Msg, State) ->
    d("handle_cast -> unknown message: ~n\t~p",[Msg]),
    {noreply, State}.

terminate(Reason, State) ->
    d("terminate -> with Reason: ~n\t~p",[Reason]),
    snmp_mgr_misc:stop(State#state.packet_server).


%%----------------------------------------------------------------------
%% Returns: A new State
%%----------------------------------------------------------------------
execute_discovery(State) ->
    Pdu   = make_discovery_pdu(),
    Reply = snmp_mgr_misc:send_discovery_pdu(Pdu,State#state.packet_server),
    {Reply,State#state{last_sent_pdu = Pdu}}.


execute_request(Operation, Data, State) ->
    case (catch make_pdu(Operation, Data, State#state.mini_mib)) of
	{error, {Format, Data2}} ->
	    report_error(State, Format, Data2),
	    State;
	{error, _Reason} -> 
	    State;
	PDU when record(PDU, pdu) ->
	    send_pdu(PDU, State#state.mini_mib, State#state.packet_server),
	    State#state{last_sent_pdu = PDU}
    end.
    
report_error(#state{quiet = true, parent = Pid}, Format, Args) ->
    Reason = lists:flatten(io_lib:format(Format, Args)),
    Pid ! {oid_error, Reason};
report_error(_, Format, Args) ->
    snmp_mgr_misc:error(Format, Args).


get_oid_from_varbind(#varbind{oid = Oid}) -> Oid.

send_pdu(PDU, _MiniMIB, PackServ) ->
    snmp_mgr_misc:send_pdu(PDU, PackServ).

%%----------------------------------------------------------------------
%% Purpose: Unnesting of oids like [myTable, 3, 4, "hej", 45] to
%%          [1,2,3,3,4,104,101,106,45]
%%----------------------------------------------------------------------
flatten_oid(XOid, DB)  ->
    Oid2 = case XOid of
	       [A|T] when atom(A) -> [remove_atom(A, DB)|T];
	       L when list(L) -> XOid;
	       Shit -> 
		   throw({error,
			  {"Invalid oid, not a list of integers: ~w", [Shit]}})
	   end,
    check_is_pure_oid(lists:flatten(Oid2)).

remove_atom(AliasName, DB) when atom(AliasName) ->
    case snmp_misc:oid(DB, AliasName) of
	false ->
	    throw({error, {"Unknown aliasname in oid: ~w", [AliasName]}});
	Oid -> Oid
    end;
remove_atom(X, _DB) -> X.

%%----------------------------------------------------------------------
%% Throws if not a list of integers
%%----------------------------------------------------------------------
check_is_pure_oid([]) -> [];
check_is_pure_oid([X | T]) when integer(X), X >= 0 ->
    [X | check_is_pure_oid(T)];
check_is_pure_oid([X | _T]) ->
    throw({error, {"Invalid oid, it contains a non-integer: ~w", [X]}}).

get_next_iter_impl(0, PrevPDU, _MiniMIB, _PackServ) -> PrevPDU;
get_next_iter_impl(N, PrevPDU, MiniMIB, PackServ) ->
    Oids = snmp_misc:map({snmp_mgr, get_oid_from_varbind}, [],
			 PrevPDU#pdu.varbinds),
    PDU = make_pdu(get_next, Oids, MiniMIB),
    send_pdu(PDU, MiniMIB, PackServ),
    case receive_response() of
	{error, timeout} ->
	    io:format("(timeout)~n"),
	    get_next_iter_impl(N, PrevPDU, MiniMIB, PackServ);
	{error, _Reason} ->
	    PrevPDU;
	RPDU when record(RPDU, pdu) ->
	    io:format("(~w)", [N]),
	    echo_pdu(RPDU, MiniMIB),
	    get_next_iter_impl(N-1, RPDU, MiniMIB, PackServ)
    end.
	
%%--------------------------------------------------
%% Used to resend a PDU. Takes the old PDU and
%% generates a fresh one (with a new requestID).
%%--------------------------------------------------

make_pdu(set, VarsAndValues, MiniMIB) ->
    VBs = snmp_misc:map({snmp_mgr, var_and_value_to_varbind}, [MiniMIB],
			VarsAndValues),
    make_pdu_impl(set, VBs);
make_pdu(bulk, {NonRepeaters, MaxRepetitions, Oids}, MiniMIB) ->
    Foids = snmp_misc:map({snmp_mgr, flatten_oid}, [MiniMIB], Oids),
    #pdu{type = 'get-bulk-request',request_id = make_request_id(),
	 error_status = NonRepeaters, error_index = MaxRepetitions,
	 varbinds = snmp_misc:map({snmp_mgr, make_vb}, [], Foids)};
make_pdu(Operation, Oids, MiniMIB) ->
    make_pdu_impl(Operation,
		  snmp_misc:map({snmp_mgr, flatten_oid}, [MiniMIB], Oids)).

make_pdu_impl(get, Oids) ->
    #pdu{type = 'get-request',request_id = make_request_id(),
	 error_status = noError, error_index = 0,
	 varbinds = snmp_misc:map({snmp_mgr, make_vb}, [], Oids)};

make_pdu_impl(get_next, Oids) ->
    #pdu{type = 'get-next-request', request_id = make_request_id(), 
	 error_status = noError, error_index = 0,
	 varbinds = snmp_misc:map({snmp_mgr, make_vb}, [], Oids)};

make_pdu_impl(set, Varbinds) ->
    #pdu{type = 'set-request', request_id = make_request_id(),
	 error_status = noError, error_index = 0, varbinds = Varbinds}.

make_discovery_pdu() ->
    #pdu{type = 'get-request',request_id = make_request_id(),
	 error_status = noError, error_index = 0,
	 varbinds = snmp_misc:map({snmp_mgr, make_vb}, [], 
				  [?sysDescr_instance])}.

var_and_value_to_varbind({Oid, Type, Value}, MiniMIB) ->
    Oid2 = flatten_oid(Oid, MiniMIB), 
    #varbind{oid = Oid2, variabletype = char_to_type(Type), value = Value};
var_and_value_to_varbind({XOid, Value}, MiniMIB) ->
    Oid = flatten_oid(XOid, MiniMIB), 
    #varbind{oid = Oid, variabletype = snmp_misc:type(MiniMIB, Oid),
	     value = Value}.

char_to_type(o) ->
    'OBJECT IDENTIFIER';
char_to_type(i) ->
    'INTEGER';
char_to_type(u) ->
    'Unsigned32';
char_to_type(g) -> % Gauge, Gauge32
    'Unsigned32';
char_to_type(s) ->
    'OCTET STRING'.

make_vb(Oid) ->
    #varbind{oid = Oid, variabletype = 'NULL', value = 'NULL'}.

make_request_id() ->
    random:uniform(16#FFFFFFF-1).

echo_pdu(PDU,MiniMIB) ->
    io:format("~s",[snmp_misc:format_pdu(PDU,MiniMIB)]).

%%----------------------------------------------------------------------
%% Test Sequence
%%----------------------------------------------------------------------
echo_errors({error, Id, {ExpectedFormat, ExpectedData}, {Format, Data}})->
    io:format("* Unexpected Behaviour * Id: ~w.~n"
	      "  Expected: " ++ ExpectedFormat ++ "~n"
	      "  Got:      " ++ Format ++ "~n", 
	      [Id] ++ ExpectedData ++ Data),
    {error, Id, {ExpectedFormat, ExpectedData}, {Format, Data}};
echo_errors(ok) -> ok;
echo_errors({ok, Val}) -> {ok, Val}.

get_response_impl(Id, Vars) ->
    case receive_response() of
	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
	    match_vars(Id, find_pure_oids2(Vars), VBs, []);

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w",
			 ['get-response', noError, 0, ReqId]},
	     {"Type: ~w ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end.

    

%%----------------------------------------------------------------------
%% Returns: ok | {error, Id, {ExpectedFormat, ExpectedData}, {Format, Data}}
%%----------------------------------------------------------------------
expect_impl(Id, any) -> 
    case receive_response() of
	PDU when record(PDU, pdu) -> ok;
	{error, Reason} -> format_reason(Id, Reason)
    end;

expect_impl(Id, return) -> 
    case receive_response() of
	PDU when record(PDU, pdu) -> {ok, PDU};
	{error, Reason} -> format_reason(Id, Reason)
    end;

expect_impl(Id, trap) -> 
    case receive_trap(3500) of
	PDU when record(PDU, trappdu) -> ok;
	{error, Reason} -> format_reason(Id, Reason)
    end;

expect_impl(Id, timeout) -> 
    receive
	X -> {error, Id, {"Timeout", []}, {"Message ~w",  [X]}}
    after 3500 ->
	    ok
    end;

expect_impl(Id, Err) when atom(Err) ->
    case receive_response() of
	#pdu{error_status = Err} -> 
	    ok;

	#pdu{request_id   = ReqId, 
	     error_status = OtherErr} ->
	    {error, Id, {"ErrorStatus: ~w, RequestId: ~w", [Err,ReqId]},
	     {"ErrorStatus: ~w", [OtherErr]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end;

expect_impl(Id, ExpectedVarbinds) when list(ExpectedVarbinds) ->
    case receive_response() of
	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
	    check_vars(Id, find_pure_oids(ExpectedVarbinds), VBs);

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 ['get-response', noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end.

expect_impl(Id, v2trap, ExpectedVarbinds) when list(ExpectedVarbinds) ->
    case receive_response() of
	#pdu{type         = 'snmpv2-trap', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
	    check_vars(Id, find_pure_oids(ExpectedVarbinds), VBs);

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 ['snmpv2-trap', noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end;

expect_impl(Id, report, ExpectedVarbinds) when list(ExpectedVarbinds) ->
    case receive_response() of
	#pdu{type         = 'report', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
	    check_vars(Id, find_pure_oids(ExpectedVarbinds), VBs);

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 [report, noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end;

expect_impl(Id, {inform, Reply}, ExpectedVarbinds) when
  list(ExpectedVarbinds) ->
    Resp = receive_response(),
    case Resp of
	#pdu{type         = 'inform-request', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
	    case check_vars(Id, find_pure_oids(ExpectedVarbinds), VBs) of
		ok when Reply == true ->
		    RespPDU = Resp#pdu{type = 'get-response',
				       error_status = noError,
				       error_index = 0},
		    snmp_mgr:rpl(RespPDU),
		    ok;
		ok when element(1, Reply) == error ->
		    {error, Status, Index} = Reply,
		    RespPDU = Resp#pdu{type = 'get-response',
				       error_status = Status,
				       error_index = Index},
		    snmp_mgr:rpl(RespPDU),
		    ok;
		ok when Reply == false ->
		    ok;
		Else ->
		    Else
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 ['inform-request', noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end.

expect_impl(Id, Err, Index, any) ->
    case receive_response() of
	#pdu{type         = 'get-response', 
	     error_status = Err, 
	     error_index  = Index} -> 
	    ok;

	#pdu{type = 'get-response', error_status = Err} when Index == any -> 
	    ok;

	#pdu{type         = 'get-response', 
	     request_id   = ReqId, 
	     error_status = Err, 
	     error_index  = Idx} when list(Index) ->
	    case lists:member(Idx, Index) of
		true -> 
		    ok;
		false ->
		    {error, Id, {"ErrStat: ~w, Idx: ~w, RequestId: ~w", 
				 [Err, Index, ReqId]},
		     {"ErrStat: ~w, Idx: ~w", [Err, Idx]}}
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 ['get-response', Err, Index, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end;

expect_impl(Id, Err, Index, ExpectedVarbinds) ->
    PureVBs = find_pure_oids(ExpectedVarbinds),
    case receive_response() of
	#pdu{type         = 'get-response', 
	     error_status = Err, 
	     error_index  = Index,
	     varbinds     = VBs} ->
	    check_vars(Id, PureVBs, VBs);

	#pdu{type         = 'get-response', 
	     error_status = Err, 
	     varbinds     = VBs} when Index == any ->
	    check_vars(Id, PureVBs, VBs);

	#pdu{type         = 'get-response', 
	     request_id   = ReqId, 
	     error_status = Err, 
	     error_index  = Idx,
	     varbinds     = VBs} when list(Index) ->
	    case lists:member(Idx, Index) of
		true ->
		    check_vars(Id, PureVBs, VBs);
		false ->
		    {error,Id,
		     {"ErrStat: ~w, Idx: ~w, Varbinds: ~w, RequestId: ~w",
		      [Err,Index,PureVBs,ReqId]},
		     {"ErrStat: ~w, Idx: ~w, Varbinds: ~w",
		      [Err,Idx,VBs]}}
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2, 
	     varbinds     = VBs} ->
	    {error,Id,
	     {"Type: ~w, ErrStat: ~w, Idx: ~w, Varbinds: ~w, RequestId: ~w",
	      ['get-response',Err,Index,PureVBs,ReqId]},
	     {"Type: ~w, ErrStat: ~w Idx: ~w Varbinds: ~w",
	      [Type2,Err2,Index2,VBs]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end.

expect_impl(Id, trap, Enterp, Generic, Specific, ExpectedVarbinds) ->
    PureE = find_pure_oid(Enterp),
    case receive_trap(3500) of
	#trappdu{enterprise    = PureE, 
		 generic_trap  = Generic,
		 specific_trap = Specific, 
		 varbinds      = VBs} ->
	    check_vars(Id, find_pure_oids(ExpectedVarbinds), VBs);

	#trappdu{enterprise    = Ent2, 
		 generic_trap  = G2,
		 specific_trap = Spec2, 
		 varbinds      = VBs} ->
	    {error, Id,
	     {"Enterprise: ~w, Generic: ~w, Specific: ~w, Varbinds: ~w",
	      [PureE, Generic, Specific, ExpectedVarbinds]},
	     {"Enterprise: ~w, Generic: ~w, Specific: ~w, Varbinds: ~w",
	      [Ent2, G2, Spec2, VBs]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end.

format_reason(Id, Reason) ->
    {error, Id, {"?", []}, {"~w", [Reason]}}.

%%----------------------------------------------------------------------
%% Args: Id, ExpectedVarbinds, GotVarbinds
%% Returns: ok
%% Fails: if not ok
%%----------------------------------------------------------------------
check_vars(_Id,[], []) -> 
    ok;
check_vars(Id,Vars, []) ->
    {error, Id, {"More Varbinds (~w)", [Vars]}, {"Too few", []}};
check_vars(Id,[], Varbinds) ->
    {error,Id, {"Fewer Varbinds", []}, {"Too many (~w)", [Varbinds]}};
check_vars(Id,[{_XOid, any} | Vars], [#varbind{oid = _Oid} |Vbs]) ->
    check_vars(Id,Vars, Vbs);
check_vars(Id,[{Oid, Val} | Vars], [#varbind{oid = Oid, value = Val} |Vbs]) ->
    check_vars(Id,Vars, Vbs);
check_vars(Id,[{Oid, Val} | _], [#varbind{oid = Oid, value = Val2} |_]) ->
    {error, Id, {" Varbind: ~w = ~w", [Oid, Val]}, {"Value: ~w", [Val2]}};
check_vars(Id,[{Oid, _Val} | _], [#varbind{oid = Oid2, value = _Val2} |_]) ->
    {error, Id, {"Oid: ~w", [Oid]}, {"Oid: ~w", [Oid2]}}.

match_vars(Id, [Oid|T], [#varbind{oid = Oid, value = Value} | Vbs], Res) ->
    match_vars(Id, T, Vbs, [Value | Res]);
match_vars(_Id, [], [], Res) ->
    {ok, lists:reverse(Res)};
match_vars(Id, [Oid | _], [#varbind{oid = Oid2}], _Res) ->
    {error, Id, {" Oid: ~w", [Oid]}, {"Oid2: ~w", [Oid2]}};
match_vars(Id, Vars, [], _Res) ->
    {error, Id, {"More Varbinds (~w)", [Vars]}, {"Too few", []}};
match_vars(Id, [], Varbinds, _Res) ->
    {error,Id, {"Fewer Varbinds", []}, {"Too many (~w)", [Varbinds]}}.

    

find_pure_oids([]) -> [];
find_pure_oids([{XOid, Q}|T]) ->
    [{find_pure_oid(XOid), Q} | find_pure_oids(T)].

find_pure_oids2([]) -> [];
find_pure_oids2([XOid|T]) ->
    [find_pure_oid(XOid) | find_pure_oids2(T)].

%%----------------------------------------------------------------------
%% Returns: Oid
%% Fails: malformed oids
%%----------------------------------------------------------------------
find_pure_oid(XOid) ->
    case gen_server:call(snmp_mgr, {find_pure_oid, XOid}, infinity) of
	{error, {Format, Data}} ->
	    ok = io:format(Format, Data),
	    exit(malformed_oid);
	Oid when list(Oid) -> Oid
    end.

get_value(Opt, Opts, Default) ->
    case snmp_misc:assq(Opt,Opts) of
	{value, C} -> C;
	false -> Default
    end.


%%----------------------------------------------------------------------
%% Debug
%%----------------------------------------------------------------------

sizeOf(L) when list(L) ->
    length(lists:flatten(L));
sizeOf(B) when binary(B) ->
    size(B).

d(F,A) -> d(get(debug),F,A).

d(true,F,A) ->
    io:format("MGR_DBG:" ++ F ++ "~n",A);
d(_,_F,_A) -> 
    ok.
