%%%----------------------------------------------------------------------
%%% File    : cyrsasl_gssapi.erl
%%% Author  : Mikael Magnusson <mikma@users.sourceforge.net>
%%% Purpose : GSSAPI SASL mechanism
%%% Created : 1 June 2007 by Mikael Magnusson <mikma@users.sourceforge.net>
%%% Id      : $Id: $
%%%----------------------------------------------------------------------
%%%
%%% Copyright (C) 2007-2009  Mikael Magnusson <mikma@users.sourceforge.net>
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
%%% {sasl_realm, "<Kerberos realm>"}.
%%%
%%% environment variables:
%%% KRB5_KTNAME
%%%

-module(cyrsasl_gssapi).
-author('mikma@users.sourceforge.net').
-vsn('$Revision: $ ').

-export([start/1,
	 stop/0,
	 mech_new/1,
	 mech_step/2]).

-include("ejabberd.hrl").
-include("cyrsasl.hrl").

-behaviour(cyrsasl).

-define(SERVER, ?MODULE).
-define(MSG, ?DEBUG).
-define(SERVICE, "xmpp").

-record(state, {sasl,
		needsmore=true,
		step=0,
		host,
		realm,
		authid,
		authzid,
		authrealm,
		error}).

start(_Opts) ->
    ChildSpec =
	{?SERVER,
	 {esasl, start_link, [{local, ?SERVER}]},
	 transient,
	 1000,
	 worker,
	 [esasl]},

    case supervisor:start_child(ejabberd_sup, ChildSpec) of
	{ok, _Pid} ->
	    cyrsasl:register_mechanism("GSSAPI", ?MODULE, false);
	{error, Error} = E ->
	    ?ERROR_MSG("esasl failed: ~p", [Error]),
	    E
    end.


stop() ->
    catch esasl:stop(?SERVER),
    supervisor:terminate_child(ejabberd_sup, ?SERVER),
    supervisor:delete_child(ejabberd_sup, ?SERVER).

mech_new(#sasl_params{host=Host, realm=Realm, socket=Socket}) ->
    case ejabberd_socket:gethostname(Socket) of
	{ok, FQDN} ->
	    ?MSG("mech_new ~p ~p ~p~n", [Host, Realm, FQDN]),
	    case esasl:server_start(?SERVER, "GSSAPI", ?SERVICE, FQDN) of
		{ok, Sasl} ->
		    {ok, #state{sasl=Sasl,host=Host,realm=Realm}};
		{error, {gsasl_error, Error}} ->
		    {ok, Str} = esasl:str_error(?SERVER, Error),
		    ?MSG("esasl error: ~p", [Str]),
		    {ok, #state{needsmore=error,error="internal-server-error"}};
		{error, Error} ->
		    ?MSG("esasl error: ~p", [Error]),
		    {ok, #state{needsmore=error,error="internal-server-error"}}
	    end;
	{error, Error} ->
	    ?MSG("gethostname error: ~p", [Error]),
	    {ok, #state{needsmore=error,error="internal-server-error"}}
    end.

mech_step(State, ClientIn) when is_list(ClientIn) ->
    catch do_step(State, ClientIn).

do_step(#state{needsmore=error,error=Error}=State, _) ->
    {error, Error};
do_step(#state{needsmore=false}=State, _) ->
    check_user(State);
do_step(#state{needsmore=true,sasl=Sasl,step=Step}=State, ClientIn) ->
    ?MSG("mech_step~n", []),
    case esasl:step(Sasl, list_to_binary(ClientIn)) of
	{ok, RspAuth} ->
	    ?MSG("ok~n", []),
	    {ok, Display_name} = esasl:property_get(Sasl, gssapi_display_name),
	    {ok, Authzid} = esasl:property_get(Sasl, authzid),
	    {Authid, [$@ | Auth_realm]} =
		lists:splitwith(fun(E)->E =/= $@ end, Display_name),
	    State1 = State#state{authid=Authid,
				 authzid=Authzid,
				 authrealm=Auth_realm},
	    handle_step_ok(State1, binary_to_list(RspAuth));
	{needsmore, RspAuth} ->
	    ?MSG("needsmore~n", []),
	    if (Step > 0) and (ClientIn =:= []) and (RspAuth =:= <<>>) ->
		    {error, "not-authorized"};
		true ->
		    {continue, binary_to_list(RspAuth),
		     State#state{step=Step+1}}
	    end;
	{error, _} ->
	    {error, "not-authorized"}
    end.

handle_step_ok(State, []) ->
    check_user(State);
handle_step_ok(#state{step=Step}=State, RspAuth) ->
    ?MSG("continue~n", []),
    {continue, RspAuth, State#state{needsmore=false,step=Step+1}}.

check_user(#state{authid=Authid,authzid=Authzid,
		  authrealm=Auth_realm,host=Host,realm=Realm}) ->
    if Realm =/= Auth_realm ->
	    ?MSG("bad realm ~p (expected ~p)~n",[Auth_realm, Realm]),
	    throw({error, "not-authorized"});
       true ->
	    ok
    end,

    case ejabberd_auth:is_user_exists(Authid, Host) of
	false ->
	    ?MSG("bad user ~p~n",[Authid]),
	    throw({error, "not-authorized"});
	true ->
	    ok
    end,

    ?MSG("GSSAPI authenticated ~p ~p~n", [Authid, Authzid]),
    {ok, [{username, Authid}, {authzid, Authzid}]}.
