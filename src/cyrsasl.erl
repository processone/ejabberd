%%%----------------------------------------------------------------------
%%% File    : cyrsasl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Cyrus SASL-like library
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

-module(cyrsasl).
-author('alexey@process-one.net').

-export([start/0,
	 register_mechanism/3,
	 listmech/1,
	 server_new/6,
	 server_start/3,
	 server_step/2]).

%% @type saslmechanism() = {sasl_mechanism, Mechanism, Module, Require_Plain}
%%     Mechanism = string()
%%     Module = atom()
%%     Require_Plain = bool().
%% Registry entry of a supported SASL mechanism.

-record(sasl_mechanism, {mechanism, module, require_plain_password}).

%% @type saslstate() = {sasl_state, Service, Myname, Realm, GetPassword, CheckPassword, Mech_Mod, Mech_State}
%%     Service = string()
%%     Myname = string()
%%     Realm = string()
%%     GetPassword = function()
%%     CheckPassword = function()
%%     Mech_Mod = atom()
%%     Mech_State = term().
%% State of this process.

-record(sasl_state, {service, myname, realm,
		     get_password, check_password,
		     mech_mod, mech_state}).

-export([behaviour_info/1]).

%% @hidden

behaviour_info(callbacks) ->
    [{mech_new, 3}, {mech_step, 2}];
behaviour_info(_Other) ->
    undefined.

%% @spec () -> ok

start() ->
    ets:new(sasl_mechanism, [named_table,
			     public,
			     {keypos, #sasl_mechanism.mechanism}]),
    cyrsasl_plain:start([]),
    cyrsasl_digest:start([]),
    cyrsasl_anonymous:start([]),
    ok.

%% @spec (Mechanism, Module, Require_Plain) -> true
%%     Mechanism = string()
%%     Module = atom()
%%     Require_Plain = bool()

register_mechanism(Mechanism, Module, RequirePlainPassword) ->
    ets:insert(sasl_mechanism,
	       #sasl_mechanism{mechanism = Mechanism,
			       module = Module,
			       require_plain_password = RequirePlainPassword}).

% TODO use callbacks
%-include("ejabberd.hrl").
%-include("jlib.hrl").
%check_authzid(_State, Props) ->
%    AuthzId = xml:get_attr_s(authzid, Props),
%    case jlib:string_to_jid(AuthzId) of
%	error ->
%	    {error, "invalid-authzid"};
%	JID ->
%	    LUser = jlib:nodeprep(xml:get_attr_s(username, Props)),
%	    {U, S, R} = jlib:jid_tolower(JID),
%	    case R of
%		"" ->
%		    {error, "invalid-authzid"};
%		_ ->
%		    case {LUser, ?MYNAME} of
%			{U, S} ->
%			    ok;
%			_ ->
%			    {error, "invalid-authzid"}
%		    end
%	    end
%    end.

%% @spec (State, Props) -> ok | {error, 'not-authorized'}
%%     State = saslstate()
%%     Props = [{Key, Value}]
%%         Key = atom()
%%         Value = string()

check_credentials(_State, Props) ->
    case proplists:get_value(username, Props) of
	undefined ->
	    {error, 'not-authorized'};
	User ->
	    case exmpp_stringprep:is_node(User) of
		false -> {error, 'not-authorized'};
		true  -> ok
	    end
    end.

%% @spec (Host) -> [Mechanism]
%%     Host = string()
%%     Mechanism = string()

listmech(Host) ->
    RequirePlainPassword = ejabberd_auth:plain_password_required(Host),

    Mechs = ets:select(sasl_mechanism,
		       [{#sasl_mechanism{mechanism = '$1',
					 require_plain_password = '$2',
					 _ = '_'},
			 if
			     RequirePlainPassword ->
				 [{'==', '$2', false}];
			     true ->
				 []
			 end,
			 ['$1']}]),
    filter_anonymous(Host, Mechs).

%% @spec (Service, ServerFQDN, UserRealm, SecFlags, GetPassword, CheckPassword) -> saslstate()
%%     Service = string()
%%     ServerFQDN = string()
%%     UserRealm = string()
%%     SecFlags = [term()]
%%     GetPassword = function()
%%     CheckPassword = function()

server_new(Service, ServerFQDN, UserRealm, _SecFlags,
	   GetPassword, CheckPassword) ->
    #sasl_state{service = Service,
		myname = ServerFQDN,
		realm = UserRealm,
		get_password = GetPassword,
		check_password = CheckPassword}.

%% @spec (State, Mech, ClientIn) -> Ok | Continue | Error
%%     State = saslstate()
%%     Mech = string()
%%     ClientIn = string()
%%     Ok = {ok, Props}
%%         Props = [Prop]
%%         Prop = [{Key, Value}]
%%         Key = atom()
%%         Value = string()
%%     Continue = {continue, ServerOut, New_State}
%%         ServerOut = string()
%%         New_State = saslstate()
%%     Error = {error, Reason} | {error, Username, Reason}
%%         Reason = term()
%%         Username = string()

server_start(State, Mech, ClientIn) ->
    case lists:member(Mech, listmech(State#sasl_state.myname)) of
	true ->
	    case ets:lookup(sasl_mechanism, Mech) of
		[#sasl_mechanism{module = Module}] ->
		    {ok, MechState} = Module:mech_new(
					State#sasl_state.myname,
					State#sasl_state.get_password,
					State#sasl_state.check_password),
		    server_step(State#sasl_state{mech_mod = Module,
						 mech_state = MechState},
				ClientIn);
		_ ->
		    {error, 'invalid-mechanism'}
	    end;
	false ->
	    {error, 'invalid-mechanism'}
    end.

%% @spec (State, ClientIn) -> Ok | Continue | Error
%%     State = saslstate()
%%     ClientIn = string()
%%     Ok = {ok, Props}
%%         Props = [Prop]
%%         Prop = [{Key, Value}]
%%         Key = atom()
%%         Value = string()
%%     Continue = {continue, ServerOut, New_State}
%%         ServerOut = string()
%%         New_State = saslstate()
%%     Error = {error, Reason} | {error, Username, Reason}
%%         Reason = term()
%%         Username = string()

server_step(State, ClientIn) ->
    Module = State#sasl_state.mech_mod,
    MechState = State#sasl_state.mech_state,
    case Module:mech_step(MechState, ClientIn) of
	{ok, Props} ->
	    case check_credentials(State, Props) of
		ok ->
		    {ok, Props};
		{error, Error} ->
		    {error, Error}
	    end;
	{continue, ServerOut, NewMechState} ->
	    {continue, ServerOut,
	     State#sasl_state{mech_state = NewMechState}};
	{error, Error, Username} ->
	    {error, Error, Username};
	{error, Error} ->
	    {error, Error}
    end.

%% @spec (Host, Mechs) -> [Filtered_Mechs]
%%     Host = string()
%%     Mechs = [Mech]
%%         Mech = string()
%%     Filtered_Mechs = [Mech]
%%
%% @doc Remove the anonymous mechanism from the list if not enabled for
%% the given host.

filter_anonymous(Host, Mechs) ->
    case ejabberd_auth_anonymous:is_sasl_anonymous_enabled(Host) of
	true  -> Mechs;
	false -> Mechs -- ["ANONYMOUS"]
    end.
