%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc_sup.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : ODBC connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_odbc_sup).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% API
-export([start_link/1,
	 init/1,
	 get_pids/1,
	 get_random_pid/1
	]).

-include("ejabberd.hrl").

-define(DEFAULT_POOL_SIZE, 10).

start_link(Host) ->
    supervisor:start_link({local, gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, [Host]).

init([Host]) ->
    N = case ejabberd_config:get_local_option({odbc_pool_size, Host}) of
	    I when is_integer(I) ->
		I;
	    undefined ->
		?DEFAULT_POOL_SIZE;
	    Other ->
		?ERROR_MSG("Wrong odbc_pool_size definition '~p' for host ~p, default to ~p~n",
			   [Other, Host, ?DEFAULT_POOL_SIZE]),
		?DEFAULT_POOL_SIZE
	end,
    {ok, {{one_for_one, 10, 6},
	  lists:map(
	    fun(I) ->
		    {I,
		     {ejabberd_odbc, start_link, [Host]},
		     transient,
		     brutal_kill,
		     worker,
		     [?MODULE]}
	    end, lists:seq(1, N))}}.

get_pids(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    [Child ||
	{_Id, Child, _Type, _Modules} <- supervisor:which_children(Proc),
	Child /= undefined].

get_random_pid(Host) ->
    Pids = get_pids(Host),
    lists:nth(erlang:phash(now(), length(Pids)), Pids).
