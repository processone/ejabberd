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
    cyrsasl_digest:start([]),
    ok.

register_mechanism(Mechanism, Module) ->
    ets:insert(sasl_mechanism, #sasl_mechanism{mechanism = Mechanism,
					       module = Module}).

% TODO: use callbacks
-include("ejabberd.hrl").
-include("jlib.hrl").
check_authzid(State, Props) ->
    AuthzId = xml:get_attr_s(authzid, Props),
    case jlib:string_to_jid(AuthzId) of
	error ->
	    {error, "invalid-authzid"};
	JID ->
	    LUser = xml:get_attr_s(username, Props),
	    {U, S, R} = jlib:jid_tolower(JID),
	    case R of
		"" ->
		    {error, "invalid-authzid"};
		_ ->
		    case {LUser, ?MYNAME} of
			{U, S} ->
			    ok;
			_ ->
			    {error, "invalid-authzid"}
		    end
	    end
    end.

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
	    {ok, MechState} = Module:mech_new(),
	    server_step(State#sasl_state{mech_mod = Module,
					 mech_state = MechState},
			ClientIn);
	_ ->
	    {error, "no-mechanism"}
    end.

server_step(State, ClientIn) ->
    Module = State#sasl_state.mech_mod,
    MechState = State#sasl_state.mech_state,
    case Module:mech_step(MechState, ClientIn) of
	{ok, Props} ->
	    case check_authzid(State, Props) of
		ok ->
		    {ok, Props};
		{error, Error} ->
		    {error, Error}
	    end;
	{continue, ServerOut, NewMechState} ->
	    {continue, ServerOut,
	     State#sasl_state{mech_state = NewMechState}};
	{error, Error} ->
	    {error, Error}
    end.

