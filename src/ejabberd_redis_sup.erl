%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  6 Apr 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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
-module(ejabberd_redis_sup).

-behaviour(supervisor).

%% API
-export([start/0, stop/0, start_link/0]).
-export([get_pool_size/0, config_reloaded/0]).

%% Supervisor callbacks
-export([init/1]).

-include("logger.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================
start() ->
    case is_started() of
	true -> ok;
	false ->
	    ejabberd:start_app(eredis),
	    Spec = {?MODULE, {?MODULE, start_link, []},
		    permanent, infinity, supervisor, [?MODULE]},
	    case supervisor:start_child(ejabberd_db_sup, Spec) of
		{ok, _} -> ok;
		{error, {already_started, Pid}} ->
                    %% Wait for the supervisor to fully start
                    _ = supervisor:count_children(Pid),
                    ok;
		{error, Why} = Err ->
		    ?ERROR_MSG("Failed to start ~ts: ~p", [?MODULE, Why]),
		    Err
	    end
    end.

stop() ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 20),
    _ = supervisor:terminate_child(ejabberd_db_sup, ?MODULE),
    _ = supervisor:delete_child(ejabberd_db_sup, ?MODULE),
    ok.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

config_reloaded() ->
    case is_started() of
	true ->
	    lists:foreach(
	      fun(Spec) ->
		      supervisor:start_child(?MODULE, Spec)
	      end, get_specs()),
	    PoolSize = get_pool_size(),
	    lists:foreach(
	      fun({Id, _, _, _}) when Id > PoolSize ->
		      case supervisor:terminate_child(?MODULE, Id) of
			  ok -> supervisor:delete_child(?MODULE, Id);
			  _ -> ok
		      end;
		 (_) ->
		      ok
	      end, supervisor:which_children(?MODULE));
	false ->
	    ok
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),
    {ok, {{one_for_one, 500, 1}, get_specs()}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_specs() ->
    lists:map(
      fun(I) ->
	      {I, {ejabberd_redis, start_link, [I]},
	       transient, 2000, worker, [?MODULE]}
      end, lists:seq(1, get_pool_size())).

get_pool_size() ->
    ejabberd_option:redis_pool_size() + 1.

is_started() ->
    whereis(?MODULE) /= undefined.
