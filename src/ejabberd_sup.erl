%%%----------------------------------------------------------------------
%%% File    : ejabberd_sup.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_sup).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Router = {ejabberd_router,
	      {ejabberd_router, start_link, []},
	      permanent,
	      brutal_kill,
	      worker,
	      [ejabberd_router]},
    SM = {ejabberd_sm,
	  {ejabberd_sm, start_link, []},
	  permanent,
	  brutal_kill,
	  worker,
	  [ejabberd_sm]},
    S2S = {ejabberd_s2s,
	   {ejabberd_s2s, start_link, []},
	   permanent,
	   brutal_kill,
	   worker,
	   [ejabberd_s2s]},
    Local = {ejabberd_local,
	     {ejabberd_local, start_link, []},
	     permanent,
	     brutal_kill,
	     worker,
	     [ejabberd_local]},
    Listener = {ejabberd_listener,
		{ejabberd_listener, start_link, []},
		permanent,
		brutal_kill,
		supervisor,
		[ejabberd_listener]},
    {ok, {{one_for_one, 10, 1}, [Router, SM, S2S, Local, Listener]}}.


