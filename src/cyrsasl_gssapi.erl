%%%----------------------------------------------------------------------
%%% File    : cyrsasl_gssapi.erl
%%% Author  : Mikael Magnusson <mikma@users.sourceforge.net>
%%% Purpose : GSSAPI SASL mechanism
%%% Created : 1 June 2007 by Mikael Magnusson <mikma@users.sourceforge.net>
%%%
%%%
%%% Copyright (C) 2007  Mikael Magnusson <mikma@users.sourceforge.net>
%%%
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without
%%% restriction, including without limitation the rights to use, copy,
%%% modify, merge, publish, distribute, sublicense, and/or sell copies
%%% of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%

%%%
%%% configuration options:
%%% sasl_realm: "<Kerberos realm>"
%%% sasl_keytab: "<Path to Kerberos keytab>"
%%%

-module(cyrsasl_gssapi).
-author('mikma@users.sourceforge.net').

-include("ejabberd.hrl").
-include("logger.hrl").

-export([start/1,
	 stop/0,
	 mech_new/4,
	 mech_step/2,
         opt_type/1]).

-behaviour(cyrsasl).

-define(SERVER, cyrsasl_gssapi).

-record(state, {sasl,
		needsmore=true,
		step=0,
		host,
		authid,
		authzid,
		authrealm}).

start(_Opts) ->
    KeyTab = ejabberd_config:get_option(sasl_keytab, fun(F) -> F end),
    Ccname = "", % default ccname
    ChildSpec =
	{?SERVER,
	 {esasl, start_link, [{local, ?SERVER}, KeyTab, Ccname]},
	 transient,
	 1000,
	 worker,
	 [esasl]},

    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec),

    cyrsasl:register_mechanism(<<"GSSAPI">>, ?MODULE, plain).

stop() ->
    esasl:stop(?SERVER),
    supervisor:terminate_child(ejabberd_sup, ?SERVER),
    supervisor:delete_child(ejabberd_sup, ?SERVER).

mech_new(Host, _GetPassword, _CheckPassword, _CheckPasswordDigest) ->
    ?DEBUG("Host [~p]~n", [Host]),
    % FIXME Should we use sasl_realm value from config instead of Host?
    {ok, Sasl} = esasl:server_start(?SERVER, "GSSAPI", "xmpp", Host),
    {ok, #state{sasl=Sasl,host=Host}}.

mech_step(State, ClientIn) when is_binary(ClientIn) ->
    ?DEBUG("ClientIn [~p]~n", [ClientIn]),
    catch do_step(State, ClientIn).

do_step(#state{needsmore=false}=State, _) ->
    check_user(State);
do_step(#state{needsmore=true,sasl=Sasl,step=Step}=State, ClientIn) ->
    ?DEBUG("ClientIn [~p]~n", [ClientIn]),
    case esasl:step(Sasl, ClientIn) of
	{ok, RspAuth} ->
	    ?DEBUG("ok: ~p~n", [RspAuth]),
	    {ok, Display_name} = esasl:property_get(Sasl, gssapi_display_name),
	    {ok, Authzid} = esasl:property_get(Sasl, authzid),
	    {Authid1, [$@ | Auth_realm1]} =
		lists:splitwith(fun(E)->E =/= $@ end, Display_name),
            Authid = list_to_binary(Authid1),
            Auth_realm = list_to_binary(Auth_realm1),
            ?DEBUG("Auth info: ~p ~p ~p ~p~n", [Authzid, Authid, Auth_realm, Display_name]),
            %% TODO: gsasl 1.8.0 / esasl 0.1 always return empty Authzid, so use Authid as Authzid
	    State1 = State#state{authid=Authid,
                                 authzid=Authid,
				 authrealm=Auth_realm},
	    handle_step_ok(State1, RspAuth);
	{needsmore, RspAuth} ->
	    ?DEBUG("needsmore~n", []),
	    if (Step > 0) and (ClientIn =:= <<>>) and (RspAuth =:= <<>>) ->
		    {error, <<"not-authorized">>};
		true ->
		    {continue, RspAuth,
		     State#state{step=Step+1}}
	    end;
	{error, _} ->
	    {error, <<"not-authorized">>}
    end.

handle_step_ok(State, <<>>) ->
    ?DEBUG("call check_user~n", []),
    check_user(State);
handle_step_ok(#state{step=Step}=State, RspAuth) ->
    ?DEBUG("continue~n", []),
    {continue, RspAuth, State#state{needsmore=false,step=Step+1}}.

check_user(#state{authid=Authid,authzid=Authzid,
		  authrealm=Auth_realm,host=Host}) ->
    Realm = ejabberd_config:get_option({sasl_realm, Host}, fun(F) -> F end),

    if Realm =/= Auth_realm ->
	    ?DEBUG("bad realm ~p (expected ~p)~n",[Auth_realm, Realm]),
	    throw({error, <<"not-authorized">>});
       true ->
	    ok
    end,
    ?DEBUG("checkuser: ~p ~p~n", [Authid, Host]),
    case ejabberd_auth:is_user_exists(Authid, Host) of
	false ->
	    ?DEBUG("bad user ~p~n",[Authid]),
	    throw({error, <<"not-authorized">>});
	true ->
	    ok
    end,

    ?DEBUG("GSSAPI authenticated ~p ~p~n", [Authid, Authzid]),
    {ok, [{username, Authid}, {authzid, Authzid}]}.

opt_type(sasl_realm) -> fun iolist_to_binary/1;
opt_type(_) -> [sasl_realm].
