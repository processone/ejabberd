%%%----------------------------------------------------------------------
%%% File    : ejabberd_riak_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Riak connections supervisor
%%% Created : 29 Dec 2011 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-behaviour(ejabberd_config).
-author('alexey@process-one.net').

-export([start_link/0, init/1, get_pids/0,
	 transform_options/1, get_random_pid/0,
	 host_up/1, config_reloaded/0, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_RIAK_START_INTERVAL, 30). % 30 seconds
-define(DEFAULT_RIAK_HOST, "127.0.0.1").
-define(DEFAULT_RIAK_PORT, 8087).

% time to wait for the supervisor to start its child before returning
% a timeout error to the request
-define(CONNECT_TIMEOUT, 500). % milliseconds

host_up(Host) ->
    case is_riak_configured(Host) of
	true ->
	    ejabberd:start_app(riakc),
	    lists:foreach(
	      fun(Spec) ->
		      supervisor:start_child(?MODULE, Spec)
	      end, get_specs());
	false ->
	    ok
    end.

config_reloaded() ->
    case is_riak_configured() of
	true ->
	    ejabberd:start_app(riakc),
	    lists:foreach(
	      fun(Spec) ->
		      supervisor:start_child(?MODULE, Spec)
	      end, get_specs());
	false ->
	    lists:foreach(
	      fun({Id, _, _, _}) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id)
	      end, supervisor:which_children(?MODULE))
    end.

is_riak_configured() ->
    lists:any(fun is_riak_configured/1, ?MYHOSTS).

is_riak_configured(Host) ->
    ServerConfigured = ejabberd_config:has_option({riak_server, Host}),
    PortConfigured = ejabberd_config:has_option({riak_port, Host}),
    StartIntervalConfigured = ejabberd_config:has_option({riak_start_interval, Host}),
    PoolConfigured = ejabberd_config:has_option({riak_pool_size, Host}),
    CacertConfigured = ejabberd_config:has_option({riak_cacertfile, Host}),
    UserConfigured = ejabberd_config:has_option({riak_username, Host}),
    PassConfigured = ejabberd_config:has_option({riak_password, Host}),
    AuthConfigured = lists:member(
		       ejabberd_auth_riak,
		       ejabberd_auth:auth_modules(Host)),
    SMConfigured = ejabberd_config:get_option({sm_db_type, Host}) == riak,
    RouterConfigured = ejabberd_config:get_option({router_db_type, Host}) == riak,
    ServerConfigured or PortConfigured or StartIntervalConfigured
	or PoolConfigured or CacertConfigured
	or UserConfigured or PassConfigured
	or SMConfigured or RouterConfigured
	or AuthConfigured.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),
    ejabberd_hooks:add(host_up, ?MODULE, host_up, 20),
    Specs = case is_riak_configured() of
		true ->
		    ejabberd:start_app(riakc),
		    get_specs();
		false ->
		    []
	    end,
    {ok, {{one_for_one, 500, 1}, Specs}}.

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
		[I, Server, Port, StartInterval*1000, Options]},
	       transient, 2000, worker, [?MODULE]}
      end, lists:seq(1, PoolSize)).

get_start_interval() ->
    ejabberd_config:get_option(riak_start_interval, ?DEFAULT_RIAK_START_INTERVAL).

get_pool_size() ->
    ejabberd_config:get_option(riak_pool_size, ?DEFAULT_POOL_SIZE).

get_riak_server() ->
    ejabberd_config:get_option(riak_server, ?DEFAULT_RIAK_HOST).

get_riak_cacertfile() ->
    ejabberd_config:get_option(riak_cacertfile, nil).

get_riak_username() ->
    ejabberd_config:get_option(riak_username, nil).

get_riak_password() ->
    ejabberd_config:get_option(riak_password, nil).

get_riak_port() ->
    ejabberd_config:get_option(riak_port, ?DEFAULT_RIAK_PORT).

get_pids() ->
    [ejabberd_riak:get_proc(I) || I <- lists:seq(1, get_pool_size())].

get_random_pid() ->
    I = randoms:round_robin(get_pool_size()) + 1,
    ejabberd_riak:get_proc(I).

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({riak_server, {S, P}}, Opts) ->
    [{riak_server, S}, {riak_port, P}|Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

-spec opt_type(riak_pool_size) -> fun((pos_integer()) -> pos_integer());
	      (riak_port) -> fun((0..65535) -> 0..65535);
	      (riak_server) -> fun((binary()) -> binary());
	      (riak_start_interval) -> fun((pos_integer()) -> pos_integer());
	      (riak_cacertfile) -> fun((binary()) -> binary());
	      (riak_username) -> fun((binary()) -> binary());
	      (riak_password) -> fun((binary()) -> binary());
	      (atom()) -> [atom()].
opt_type(riak_pool_size) ->
    fun (N) when is_integer(N), N >= 1 -> N end;
opt_type(riak_port) ->
    fun(P) when is_integer(P), P > 0, P < 65536 -> P end;
opt_type(riak_server) ->
    fun(S) -> binary_to_list(iolist_to_binary(S)) end;
opt_type(riak_start_interval) ->
    fun (N) when is_integer(N), N >= 1 -> N end;
opt_type(riak_cacertfile) ->
    fun(S) -> binary_to_list(iolist_to_binary(S)) end;
opt_type(riak_username) ->
    fun(S) -> binary_to_list(iolist_to_binary(S)) end;
opt_type(riak_password) ->
    fun(S) -> binary_to_list(iolist_to_binary(S)) end;
opt_type(_) ->
    [riak_pool_size, riak_port, riak_server,
     riak_start_interval, riak_cacertfile, riak_username, riak_password].
