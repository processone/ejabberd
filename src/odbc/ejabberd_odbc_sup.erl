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

-export([start_link/0,
	 init/1,
	 get_pids/0,
	 get_random_pid/0
	]).

-include("ejabberd.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    % TODO
    N = 10,
    {ok, {{one_for_one, 10, 1},
	  lists:map(
	    fun(I) ->
		    {I,
		     {ejabberd_odbc, start_link, []},
		     transient,
		     brutal_kill,
		     worker,
		     [?MODULE]}
	    end, lists:seq(1, N))}}.

get_pids() ->
    [Child ||
	{_Id, Child, _Type, _Modules} <- supervisor:which_children(?MODULE),
	Child /= undefined].

get_random_pid() ->
    Pids = get_pids(),
    lists:nth(erlang:phash(now(), length(Pids)), Pids).

