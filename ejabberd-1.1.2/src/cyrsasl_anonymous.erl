%%%----------------------------------------------------------------------
%%% File    : cyrsasl_anonymous.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : ANONYMOUS SASL mechanism
%%% Created : 23 Aug 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

%% See http://www.ietf.org/internet-drafts/draft-ietf-sasl-anon-05.txt

-module(cyrsasl_anonymous).
-vsn('$Revision$').

-export([start/1, stop/0, mech_new/3, mech_step/2]).

-behaviour(cyrsasl).

-record(state, {server}).

start(_Opts) ->
    cyrsasl:register_mechanism("ANONYMOUS", ?MODULE, false),
    ok.

stop() ->
    ok.

mech_new(Host, _GetPassword, _CheckPassword) ->
    {ok, #state{server = Host}}.

mech_step(State, _ClientIn) ->
    %% We generate a random username:
    User = lists:concat([randoms:get_string() | tuple_to_list(now())]),
    Server = State#state.server,
    
    %% Checks that the username is available
    case ejabberd_auth:is_user_exists(User, Server) of
	true  -> {error, "not-authorized"};
	false -> {ok, [{username, User}]}
    end.
