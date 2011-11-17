%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Redis wrapper for ejabberd
%%%
%%% @end
%%% Created : 17 Nov 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ejabberd_redis).

-export([start_link/1]).
-export([cmd/1, cmd/2]).

-define(POOL_NAME, redis_pool).

-spec start_link(list()) -> {ok, pid()}.
start_link(Opts) ->
    PoolSize = proplists:get_value(pool_size, Opts, 10),
    RedoOpts = proplists:get_value(worker_config, Opts, []),
    ChildMods = [redo, redo_redis_proto, redo_uri],
    ChildMFA = {redo, start_link, [undefined, RedoOpts]},

    supervisor:start_child(ejabberd_sup,
                           {ejabberd_redis_sup,
                            {cuesport, start_link,
                             [?POOL_NAME, PoolSize, ChildMods, ChildMFA]},
                            transient, 2000, supervisor, [cuesport | ChildMods]}).

-spec cmd(iolist()) -> iolist() | integer().
cmd(Cmd) ->
    redo:cmd(cuesport:get_worker(?POOL_NAME), Cmd).

-spec cmd(iolist(), list()) -> iolist() | integer().
cmd(Cmd, Opts) ->
    redo:cmd(cuesport:get_worker(?POOL_NAME), Cmd, Opts).
