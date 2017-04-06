%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  6 Apr 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
-export([start_link/0, get_pool_size/0,
	 host_up/1, config_reloaded/0, opt_type/1]).

%% Supervisor callbacks
-export([init/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(DEFAULT_POOL_SIZE, 10).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

host_up(Host) ->
    case is_redis_configured(Host) of
	true ->
	    ejabberd:start_app(eredis),
	    lists:foreach(
	      fun(Spec) ->
		      supervisor:start_child(?MODULE, Spec)
	      end, get_specs());
	false ->
	    ok
    end.

config_reloaded() ->
    case is_redis_configured() of
	true ->
	    ejabberd:start_app(eredis),
	    lists:foreach(
	      fun(Spec) ->
		      supervisor:start_child(?MODULE, Spec)
	      end, get_specs()),
	    PoolSize = get_pool_size(),
	    lists:foreach(
	      fun({Id, _, _, _}) when Id > PoolSize ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id);
		 (_) ->
		      ok
	      end, supervisor:which_children(?MODULE));
	false ->
	    lists:foreach(
	      fun({Id, _, _, _}) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id)
	      end, supervisor:which_children(?MODULE))
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),
    ejabberd_hooks:add(host_up, ?MODULE, host_up, 20),
    Specs = case is_redis_configured() of
		true ->
		    ejabberd:start_app(eredis),
		    get_specs();
		false ->
		    []
	    end,
    {ok, {{one_for_one, 500, 1}, Specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_redis_configured() ->
    lists:any(fun is_redis_configured/1, ?MYHOSTS).

is_redis_configured(Host) ->
    ServerConfigured = ejabberd_config:has_option({redis_server, Host}),
    PortConfigured = ejabberd_config:has_option({redis_port, Host}),
    DBConfigured = ejabberd_config:has_option({redis_db, Host}),
    PassConfigured = ejabberd_config:has_option({redis_password, Host}),
    PoolSize = ejabberd_config:has_option({redis_pool_size, Host}),
    ConnTimeoutConfigured = ejabberd_config:has_option(
			      {redis_connect_timeout, Host}),
    Modules = ejabberd_config:get_option(
		{modules, Host},
		fun(L) when is_list(L) -> L end, []),
    SMConfigured = ejabberd_config:get_option(
		     {sm_db_type, Host},
		     fun(V) -> V end) == redis,
    RouterConfigured = ejabberd_config:get_option(
			 {router_db_type, Host},
			 fun(V) -> V end) == redis,
    ModuleWithRedisDBConfigured =
	lists:any(
	  fun({Module, Opts}) ->
		  gen_mod:db_type(Host, Opts, Module) == redis
	  end, Modules),
    ServerConfigured or PortConfigured or DBConfigured or PassConfigured or
	PoolSize or ConnTimeoutConfigured or
	SMConfigured or RouterConfigured or ModuleWithRedisDBConfigured.

get_specs() ->
    lists:map(
      fun(I) ->
	      {I, {ejabberd_redis, start_link, [I]},
	       transient, 2000, worker, [?MODULE]}
      end, lists:seq(1, get_pool_size())).

get_pool_size() ->
    ejabberd_config:get_option(
      redis_pool_size,
      fun(N) when is_integer(N), N >= 1 -> N end,
      ?DEFAULT_POOL_SIZE).

iolist_to_list(IOList) ->
    binary_to_list(iolist_to_binary(IOList)).

opt_type(redis_connect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_db) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
opt_type(redis_password) -> fun iolist_to_list/1;
opt_type(redis_port) ->
    fun (P) when is_integer(P), P > 0, P < 65536 -> P end;
opt_type(redis_server) -> fun iolist_to_list/1;
opt_type(redis_pool_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_queue_type) ->
    fun(ram) -> ram; (file) -> file end;
opt_type(_) ->
    [redis_connect_timeout, redis_db, redis_password,
     redis_port, redis_pool_size, redis_server,
     redis_pool_size, redis_queue_type].
