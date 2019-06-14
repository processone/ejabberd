%%%----------------------------------------------------------------------
%%% File    : ejabberd_riak_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Riak connections supervisor
%%% Created : 29 Dec 2011 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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

-module(ejabberd_riak_sup).

-behaviour(supervisor).
-author('alexey@process-one.net').

-export([start/0, start_link/0, init/1, get_pids/0,
	 get_random_pid/0, config_reloaded/0]).

-include("logger.hrl").

% time to wait for the supervisor to start its child before returning
% a timeout error to the request
-define(CONNECT_TIMEOUT, 500). % milliseconds

start() ->
    case is_started() of
	true -> ok;
	false ->
	    ejabberd:start_app(riakc),
	    Spec = {?MODULE, {?MODULE, start_link, []},
		    permanent, infinity, supervisor, [?MODULE]},
	    case supervisor:start_child(ejabberd_db_sup, Spec) of
		{ok, _} -> ok;
		{error, {already_started, _}} -> ok;
		{error, Why} = Err ->
		    ?ERROR_MSG("Failed to start ~s: ~p",
			       [?MODULE, Why]),
		    Err
	    end
    end.

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

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),
    {ok, {{one_for_one, 500, 1}, get_specs()}}.

is_started() ->
    whereis(?MODULE) /= undefined.

-spec get_specs() -> [supervisor:child_spec()].
get_specs() ->
    PoolSize = get_pool_size(),
    StartInterval = get_start_interval(),
    Server = get_riak_server(),
    Port = get_riak_port(),
    CACertFile = get_riak_cacertfile(),
    Username = get_riak_username(),
    Password = get_riak_password(),
    Options = lists:filter(
		fun(X) -> X /= nil end,
		[auto_reconnect,
		 {keepalive, true},
		 if CACertFile /= nil -> {cacertfile ,CACertFile};
		    true -> nil
		 end,
		 if (Username /= nil) and (Password /= nil) ->
			 {credentials, Username, Password};
		    true -> nil
		 end]),
    lists:map(
      fun(I) ->
	      {ejabberd_riak:get_proc(I),
	       {ejabberd_riak, start_link,
		[I, Server, Port, StartInterval, Options]},
	       transient, 2000, worker, [?MODULE]}
      end, lists:seq(1, PoolSize)).

get_start_interval() ->
    ejabberd_option:riak_start_interval().

get_pool_size() ->
    ejabberd_option:riak_pool_size().

get_riak_server() ->
    ejabberd_option:riak_server().

get_riak_cacertfile() ->
    ejabberd_option:riak_cacertfile().

get_riak_username() ->
    ejabberd_option:riak_username().

get_riak_password() ->
    ejabberd_option:riak_password().

get_riak_port() ->
    ejabberd_option:riak_port().

get_pids() ->
    [ejabberd_riak:get_proc(I) || I <- lists:seq(1, get_pool_size())].

get_random_pid() ->
    I = p1_rand:round_robin(get_pool_size()) + 1,
    ejabberd_riak:get_proc(I).
