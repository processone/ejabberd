%%%----------------------------------------------------------------------
%%% File    : ejabberd_tmp_sup.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Supervisor for temporary processess
%%% Created : 18 Jul 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_tmp_sup).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start_link/2, init/1]).

start_link(Name, Module) ->
    supervisor:start_link({local, Name}, ?MODULE, Module).


init(Module) ->
    {ok, {{simple_one_for_one, 10, 1},
	  [{undefined, {Module, start_link, []},
	    temporary, brutal_kill, worker, [Module]}]}}.
