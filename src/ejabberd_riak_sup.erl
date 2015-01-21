%%%----------------------------------------------------------------------
%%% File    : ejabberd_riak_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Riak connections supervisor
%%% Created : 29 Dec 2011 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_riak_sup).
-author('alexey@process-one.net').

%% API
-export([start/0,
         start_link/0,
	 init/1,
	 get_pids/0,
         transform_options/1,
	 get_random_pid/0,
	 get_random_pid/1
	]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_RIAK_START_INTERVAL, 30). % 30 seconds
-define(DEFAULT_RIAK_HOST, "127.0.0.1").
-define(DEFAULT_RIAK_PORT, 8087).

% time to wait for the supervisor to start its child before returning
% a timeout error to the request
-define(CONNECT_TIMEOUT, 500). % milliseconds

start() ->
    case lists:any(
	   fun(Host) ->
		   is_riak_configured(Host)
	   end, ?MYHOSTS) of
	true ->
	    ejabberd:start_app(riakc),
            do_start();
	false ->
	    ok
    end.

is_riak_configured(Host) ->
    ServerConfigured = ejabberd_config:get_option(
			 {riak_server, Host},
			 fun(_) -> true end, false),
    PortConfigured = ejabberd_config:get_option(
		       {riak_port, Host},
		       fun(_) -> true end, false),
    AuthConfigured = lists:member(
		       ejabberd_auth_riak,
		       ejabberd_auth:auth_modules(Host)),
    Modules = ejabberd_config:get_option(
		{modules, Host},
		fun(L) when is_list(L) -> L end, []),
    ModuleWithRiakDBConfigured = lists:any(
				   fun({_Module, Opts}) ->
					   gen_mod:db_type(Opts) == riak
				   end, Modules),
    ServerConfigured or PortConfigured
	or AuthConfigured or ModuleWithRiakDBConfigured.

do_start() ->
    SupervisorName = ?MODULE,
    ChildSpec =
	{SupervisorName,
	 {?MODULE, start_link, []},
	 transient,
	 infinity,
	 supervisor,
	 [?MODULE]},
    case supervisor:start_child(ejabberd_sup, ChildSpec) of
	{ok, _PID} ->
	    ok;
	_Error ->
	    ?ERROR_MSG("Start of supervisor ~p failed:~n~p~nRetrying...~n",
                       [SupervisorName, _Error]),
            timer:sleep(5000),
	    start()
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PoolSize = get_pool_size(),
    StartInterval = get_start_interval(),
    Server = get_riak_server(),
    Port = get_riak_port(),
    {ok, {{one_for_one, PoolSize*10, 1},
	  lists:map(
	    fun(I) ->
		    {ejabberd_riak:get_proc(I),
		     {ejabberd_riak, start_link,
                      [I, Server, Port, StartInterval*1000]},
		     transient, 2000, worker, [?MODULE]}
	    end, lists:seq(1, PoolSize))}}.

get_start_interval() ->
    ejabberd_config:get_option(
      riak_start_interval,
      fun(N) when is_integer(N), N >= 1 -> N end,
      ?DEFAULT_RIAK_START_INTERVAL).

get_pool_size() ->
    ejabberd_config:get_option(
      riak_pool_size,
      fun(N) when is_integer(N), N >= 1 -> N end,
      ?DEFAULT_POOL_SIZE).

get_riak_server() ->
    ejabberd_config:get_option(
      riak_server,
      fun(S) ->
	      binary_to_list(iolist_to_binary(S))
      end, ?DEFAULT_RIAK_HOST).

get_riak_port() ->
    ejabberd_config:get_option(
      riak_port,
      fun(P) when is_integer(P), P > 0, P < 65536 -> P end,
      ?DEFAULT_RIAK_PORT).

get_pids() ->
    [ejabberd_riak:get_proc(I) || I <- lists:seq(1, get_pool_size())].

get_random_pid() ->
    get_random_pid(now()).

get_random_pid(Term) ->
    I = erlang:phash2(Term, get_pool_size()) + 1,
    ejabberd_riak:get_proc(I).

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({riak_server, {S, P}}, Opts) ->
    [{riak_server, S}, {riak_port, P}|Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].
