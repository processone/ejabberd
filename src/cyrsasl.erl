%%%----------------------------------------------------------------------
%%% File    : cyrsasl.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Cyrus SASL-like library
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(cyrsasl).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 register_mechanism/2,
	 listmech/0,
	 server_new/4,
	 server_start/3,
	 server_step/2]).

-record(sasl_mechanism, {mechanism, module}).
-record(sasl_state, {service, myname, realm, mech_mod, mech_state}).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{mech_new, 0},
     {mech_step, 2}];
behaviour_info(Other) ->
    undefined.

start() ->
    ets:new(sasl_mechanism, [named_table,
			     public,
			     {keypos, #sasl_mechanism.mechanism}]),
    cyrsasl_plain:start([]),
    ok.

register_mechanism(Mechanism, Module) ->
    ets:insert(sasl_mechanism, #sasl_mechanism{mechanism = Mechanism,
					       module = Module}).

listmech() ->
    ets:select(sasl_mechanism,
	       [{#sasl_mechanism{mechanism = '$1', _ = '_'}, [], ['$1']}]).


server_new(Service, ServerFQDN, UserRealm, SecFlags) ->
    #sasl_state{service = Service,
		myname = ServerFQDN,
		realm = UserRealm}.

server_start(State, Mech, ClientIn) ->
    case ets:lookup(sasl_mechanism, Mech) of
	[#sasl_mechanism{module = Module}] ->
	    MechState = Module:mech_new(),
	    server_step(State#sasl_state{mech_mod = Module,
					 mech_state = MechState},
			ClientIn);
	_ ->
	    {error, "454"}
    end.

server_step(State, ClientIn) ->
    Module = State#sasl_state.mech_mod,
    MechState = State#sasl_state.mech_state,
    case Module:mech_step(MechState, ClientIn) of
	{ok, Props} ->
	    {ok, Props};
	{continue, ServerOut, NewMechState} ->
	    {continue, ServerOut,
	     State#sasl_state{mech_state = NewMechState}};
	{error, Code} ->
	    {error, Code}
    end.



