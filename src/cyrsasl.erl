%%%----------------------------------------------------------------------
%%% File    : cyrsasl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Cyrus SASL-like library
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(cyrsasl).

-author('alexey@process-one.net').

-export([start/0, register_mechanism/3, listmech/1,
	 server_new/7, server_start/3, server_step/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%
-export_type([
    mechanism/0,
    mechanisms/0,
    sasl_mechanism/0
]).

-record(sasl_mechanism,
        {mechanism = <<"">>    :: mechanism() | '$1',
         module                :: atom(),
         password_type = plain :: password_type() | '$2'}).

-type(mechanism() :: binary()).
-type(mechanisms() :: [mechanism(),...]).
-type(password_type() :: plain | digest | scram).
-type(props() :: [{username, binary()} |
                  {authzid, binary()} |
                  {auth_module, atom()}]).

-type(sasl_mechanism() :: #sasl_mechanism{}).

-record(sasl_state,
{
    service,
    myname,
    realm,
    get_password,
    check_password,
    check_password_digest,
    mech_mod,
    mech_state
}).

-callback mech_new(binary(), fun(), fun(), fun()) -> any().
-callback mech_step(any(), binary()) -> {ok, props()} |
                                        {ok, props(), binary()} |
                                        {continue, binary(), any()} |
                                        {error, binary()} |
                                        {error, binary(), binary()}.

start() ->
    ets:new(sasl_mechanism,
	    [named_table, public,
	     {keypos, #sasl_mechanism.mechanism}]),
    cyrsasl_plain:start([]),
    cyrsasl_digest:start([]),
    cyrsasl_scram:start([]),
    cyrsasl_anonymous:start([]),
    ok.

%%
-spec(register_mechanism/3 ::
(
  Mechanim     :: mechanism(),
  Module       :: module(),
  PasswordType :: password_type())
    -> any()
).

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

%%% TODO: use callbacks
%%-include("ejabberd.hrl").
%%-include("jlib.hrl").
%%check_authzid(_State, Props) ->
%%    AuthzId = xml:get_attr_s(authzid, Props),
%%    case jlib:string_to_jid(AuthzId) of
%%	error ->
%%	    {error, "invalid-authzid"};
%%	JID ->
%%	    LUser = jlib:nodeprep(xml:get_attr_s(username, Props)),
%%	    {U, S, R} = jlib:jid_tolower(JID),
%%	    case R of
%%		"" ->
%%		    {error, "invalid-authzid"};
%%		_ ->
%%		    case {LUser, ?MYNAME} of
%%			{U, S} ->
%%			    ok;
%%			_ ->
%%			    {error, "invalid-authzid"}
%%		    end
%%	    end
%%    end.

check_credentials(_State, Props) ->
    User = proplists:get_value(username, Props, <<>>),
    case jlib:nodeprep(User) of
      error -> {error, <<"not-authorized">>};
      <<"">> -> {error, <<"not-authorized">>};
      _LUser -> ok
    end.

-spec(listmech/1 ::
(
  Host ::binary())
    -> Mechanisms::mechanisms()
).

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

server_new(Service, ServerFQDN, UserRealm, _SecFlags,
	   GetPassword, CheckPassword, CheckPasswordDigest) ->
    #sasl_state{service = Service, myname = ServerFQDN,
		realm = UserRealm, get_password = GetPassword,
		check_password = CheckPassword,
		check_password_digest = CheckPasswordDigest}.

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
					     mech_state = MechState},
			    ClientIn);
	    _ -> {error, <<"no-mechanism">>}
	  end;
      false -> {error, <<"no-mechanism">>}
    end.

server_step(State, ClientIn) ->
    Module = State#sasl_state.mech_mod,
    MechState = State#sasl_state.mech_state,
    case Module:mech_step(MechState, ClientIn) of
        {ok, Props} ->
            case check_credentials(State, Props) of
                ok             -> {ok, Props};
                {error, Error} -> {error, Error}
            end;
        {ok, Props, ServerOut} ->
            case check_credentials(State, Props) of
                ok             -> {ok, Props, ServerOut};
                {error, Error} -> {error, Error}
            end;
        {continue, ServerOut, NewMechState} ->
            {continue, ServerOut, State#sasl_state{mech_state = NewMechState}};
        {error, Error, Username} ->
            {error, Error, Username};
        {error, Error} ->
            {error, Error}
    end.

%% Remove the anonymous mechanism from the list if not enabled for the given
%% host
%%
-spec(filter_anonymous/2 ::
(
  Host  :: binary(),
  Mechs :: mechanisms())
    -> mechanisms()
).

filter_anonymous(Host, Mechs) ->
    case ejabberd_auth_anonymous:is_sasl_anonymous_enabled(Host) of
      true  -> Mechs;
      false -> Mechs -- [<<"ANONYMOUS">>]
    end.

-spec(is_disabled/1 ::
(
  Mechanism :: mechanism())
    -> boolean()
).

is_disabled(Mechanism) ->
    Disabled = ejabberd_config:get_option(
		 disable_sasl_mechanisms,
		 fun(V) when is_list(V) ->
			 lists:map(fun(M) -> str:to_upper(M) end, V);
		    (V) ->
			 [str:to_upper(V)]
		 end, []),
    lists:member(Mechanism, Disabled).
