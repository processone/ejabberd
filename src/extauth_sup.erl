%%%-------------------------------------------------------------------
%%% Created : 7 May 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
%%%-------------------------------------------------------------------
-module(extauth_sup).
-behaviour(supervisor).

%% API
-export([start/1, stop/1, reload/1, start_link/3]).
%% Supervisor callbacks
-export([init/1]).

-include("logger.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================
start(Host) ->
    case extauth:prog_name(Host) of
	undefined ->
	    ?ERROR_MSG("Option 'extauth_program' is not set for '~ts'",
		       [Host]),
	    ignore;
	Prog ->
	    Pool = extauth:pool_name(Host),
	    ChildSpec = {Pool, {?MODULE, start_link, [Host, Prog, Pool]},
			 permanent, infinity, supervisor, [?MODULE]},
	    supervisor:start_child(ejabberd_backend_sup, ChildSpec)
    end.

stop(Host) ->
    Pool = extauth:pool_name(Host),
    supervisor:terminate_child(ejabberd_backend_sup, Pool),
    supervisor:delete_child(ejabberd_backend_sup, Pool).

reload(Host) ->
    Pool = extauth:pool_name(Host),
    Prog = extauth:prog_name(Host),
    PoolSize = extauth:pool_size(Host),
    try process_info(whereis(Pool), dictionary) of
	{dictionary, Dict} ->
	    case proplists:get_value(extauth_program, Dict) of
		Prog ->
		    OldPoolSize = try supervisor:which_children(Pool) of
				      Children -> length(Children)
				  catch _:_ -> PoolSize
				  end,
		    if OldPoolSize > PoolSize ->
			    lists:foreach(
			      fun(I) ->
				      Worker = extauth:worker_name(Pool, I),
				      supervisor:terminate_child(Pool, Worker),
				      supervisor:delete_child(Pool, Worker)
			      end, lists:seq(PoolSize+1, OldPoolSize));
		       OldPoolSize < PoolSize ->
			    lists:foreach(
			      fun(I) ->
				      Spec = worker_spec(Pool, Prog, I),
				      supervisor:start_child(Pool, Spec)
			      end, lists:seq(OldPoolSize+1, PoolSize));
		       OldPoolSize == PoolSize ->
			    ok
		    end;
		_ ->
		    stop(Host),
		    start(Host)
	    end
    catch _:badarg ->
	    ok
    end.

start_link(Host, Prog, Pool) ->
    supervisor:start_link({local, Pool}, ?MODULE, [Host, Prog, Pool]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([Host, Prog, Pool]) ->
    PoolSize = extauth:pool_size(Host),
    Children = lists:map(
		 fun(I) ->
			 worker_spec(Pool, Prog, I)
		 end, lists:seq(1, PoolSize)),
    put(extauth_program, Prog),
    {ok, {{one_for_one, PoolSize, 1}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
worker_spec(Pool, Prog, I) ->
    Worker = extauth:worker_name(Pool, I),
    {Worker, {extauth, start_link, [Worker, Prog]},
     permanent, 5000, worker, [extauth]}.
