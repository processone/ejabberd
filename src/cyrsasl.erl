%%%----------------------------------------------------------------------
%%% File    : cyrsasl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Cyrus SASL-like library
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(cyrsasl).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start/0, register_mechanism/3, listmech/1,
	 server_new/7, server_start/3, server_step/2,
	 get_mech/1, format_error/2, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(sasl_mechanism,
        {mechanism = <<"">>    :: mechanism() | '$1',
         module                :: atom(),
         password_type = plain :: password_type() | '$2'}).

-type(mechanism() :: binary()).
-type(mechanisms() :: [mechanism(),...]).
-type(password_type() :: plain | digest | scram).
-type sasl_property() :: {username, binary()} |
			 {authzid, binary()} |
			 {mechanism, binary()} |
			 {auth_module, atom()}.
-type sasl_return() :: {ok, [sasl_property()]} |
		       {ok, [sasl_property()], binary()} |
		       {continue, binary(), sasl_state()} |
		       {error, atom(), binary()}.

-type(sasl_mechanism() :: #sasl_mechanism{}).
-type error_reason() :: cyrsasl_digest:error_reason() |
			cyrsasl_oauth:error_reason() |
			cyrsasl_plain:error_reason() |
			cyrsasl_scram:error_reason() |
			unsupported_mechanism | nodeprep_failed |
			empty_username | aborted.
-record(sasl_state,
{
    service,
    myname,
    realm,
    get_password,
    check_password,
    check_password_digest,
    mech_name = <<"">>,
    mech_mod,
    mech_state
}).
-type sasl_state() :: #sasl_state{}.
-export_type([mechanism/0, mechanisms/0, sasl_mechanism/0, error_reason/0,
	      sasl_state/0, sasl_return/0, sasl_property/0]).

-callback mech_new(binary(), fun(), fun(), fun()) -> any().
-callback mech_step(any(), binary()) -> sasl_return().

start() ->
    ets:new(sasl_mechanism,
	    [named_table, public,
	     {keypos, #sasl_mechanism.mechanism}]),
    cyrsasl_plain:start([]),
    cyrsasl_digest:start([]),
    cyrsasl_scram:start([]),
    cyrsasl_anonymous:start([]),
    cyrsasl_oauth:start([]),
    ok.

-spec format_error(mechanism() | sasl_state(), error_reason()) -> {atom(), binary()}.
format_error(_, unsupported_mechanism) ->
    {'invalid-mechanism', <<"Unsupported mechanism">>};
format_error(_, nodeprep_failed) ->
    {'bad-protocol', <<"Nodeprep failed">>};
format_error(_, empty_username) ->
    {'bad-protocol', <<"Empty username">>};
format_error(_, aborted) ->
    {'aborted', <<"Aborted">>};
format_error(#sasl_state{mech_mod = Mod}, Reason) ->
    Mod:format_error(Reason);
format_error(Mech, Reason) ->
    case ets:lookup(sasl_mechanism, Mech) of
	[#sasl_mechanism{module = Mod}] ->
	    Mod:format_error(Reason);
	[] ->
	    {'invalid-mechanism', <<"Unsupported mechanism">>}
    end.

-spec register_mechanism(Mechanim :: mechanism(), Module :: module(),
			 PasswordType :: password_type()) -> any().

register_mechanism(Mechanism, Module, PasswordType) ->
    case is_disabled(Mechanism) of
      false ->
	  ets:insert(sasl_mechanism,
		     #sasl_mechanism{mechanism = Mechanism, module = Module,
				     password_type = PasswordType});
      true ->
	  ?DEBUG("SASL mechanism ~p is disabled", [Mechanism]),
	  true
    end.

check_credentials(_State, Props) ->
    User = proplists:get_value(authzid, Props, <<>>),
    case jid:nodeprep(User) of
      error -> {error, nodeprep_failed};
      <<"">> -> {error, empty_username};
      _LUser -> ok
    end.

-spec listmech(Host ::binary()) -> Mechanisms::mechanisms().

listmech(Host) ->
    Mechs = ets:select(sasl_mechanism,
		       [{#sasl_mechanism{mechanism = '$1',
					 password_type = '$2', _ = '_'},
			 case catch ejabberd_auth:store_type(Host) of
			   external -> [{'==', '$2', plain}];
			   scram -> [{'/=', '$2', digest}];
			   {'EXIT', {undef, [{Module, store_type, []} | _]}} ->
			       ?WARNING_MSG("~p doesn't implement the function store_type/0",
					    [Module]),
			       [];
			   _Else -> []
			 end,
			 ['$1']}]),
    filter_anonymous(Host, Mechs).

-spec server_new(binary(), binary(), binary(), term(),
		 fun(), fun(), fun()) -> sasl_state().
server_new(Service, ServerFQDN, UserRealm, _SecFlags,
	   GetPassword, CheckPassword, CheckPasswordDigest) ->
    #sasl_state{service = Service, myname = ServerFQDN,
		realm = UserRealm, get_password = GetPassword,
		check_password = CheckPassword,
		check_password_digest = CheckPasswordDigest}.

-spec server_start(sasl_state(), mechanism(), binary()) -> sasl_return().
server_start(State, Mech, ClientIn) ->
    case lists:member(Mech,
		      listmech(State#sasl_state.myname))
	of
      true ->
	  case ets:lookup(sasl_mechanism, Mech) of
	    [#sasl_mechanism{module = Module}] ->
		{ok, MechState} =
		    Module:mech_new(State#sasl_state.myname,
				    State#sasl_state.get_password,
				    State#sasl_state.check_password,
				    State#sasl_state.check_password_digest),
		server_step(State#sasl_state{mech_mod = Module,
					     mech_name = Mech,
					     mech_state = MechState},
			    ClientIn);
	    _ -> {error, unsupported_mechanism, <<"">>}
	  end;
      false -> {error, unsupported_mechanism, <<"">>}
    end.

-spec server_step(sasl_state(), binary()) -> sasl_return().
server_step(State, ClientIn) ->
    Module = State#sasl_state.mech_mod,
    MechState = State#sasl_state.mech_state,
    case Module:mech_step(MechState, ClientIn) of
        {ok, Props} ->
            case check_credentials(State, Props) of
                ok             -> {ok, Props};
                {error, Error} -> {error, Error, <<"">>}
            end;
        {ok, Props, ServerOut} ->
            case check_credentials(State, Props) of
                ok             -> {ok, Props, ServerOut};
                {error, Error} -> {error, Error, <<"">>}
            end;
        {continue, ServerOut, NewMechState} ->
            {continue, ServerOut, State#sasl_state{mech_state = NewMechState}};
        {error, Error, Username} ->
            {error, Error, Username};
        {error, Error} ->
            {error, Error, <<"">>}
    end.

-spec get_mech(sasl_state()) -> binary().
get_mech(#sasl_state{mech_name = Mech}) ->
    Mech.

%% Remove the anonymous mechanism from the list if not enabled for the given
%% host
%%
-spec filter_anonymous(Host :: binary(), Mechs :: mechanisms()) -> mechanisms().

filter_anonymous(Host, Mechs) ->
    case ejabberd_auth_anonymous:is_sasl_anonymous_enabled(Host) of
      true  -> Mechs;
      false -> Mechs -- [<<"ANONYMOUS">>]
    end.

-spec is_disabled(Mechanism :: mechanism()) -> boolean().

is_disabled(Mechanism) ->
    Disabled = ejabberd_config:get_option(
		 disable_sasl_mechanisms,
		 fun(V) when is_list(V) ->
			 lists:map(fun(M) -> str:to_upper(M) end, V);
		    (V) ->
			 [str:to_upper(V)]
		 end, []),
    lists:member(Mechanism, Disabled).

opt_type(disable_sasl_mechanisms) ->
    fun (V) when is_list(V) ->
	    lists:map(fun (M) -> str:to_upper(M) end, V);
	(V) -> [str:to_upper(V)]
    end;
opt_type(_) -> [disable_sasl_mechanisms].
