%%%-------------------------------------------------------------------
%%% File    : ejabberd_cluster.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description :
%%%
%%% Created :  2 Apr 2010 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_cluster).

-behaviour(gen_server).

%% API
-export([start_link/0, get_node/1, get_node_new/1,
	 announce/1, shutdown/0, node_id/0, get_node_by_id/1,
	 get_nodes/0, rehash_timeout/0, start/0,
	 shutdown_migrate/1, migrate_timeout/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(HASHTBL, nodes_hash).

-define(HASHTBL_NEW, nodes_hash_new).

-define(POINTS, 64).

-define(REHASH_TIMEOUT, timer:seconds(30)).

-define(MIGRATE_TIMEOUT, timer:minutes(2)).

-define(LOCK, {migrate, node()}).

-record(state, {}).
-record(?HASHTBL, {hash, node}).
-record(?HASHTBL_NEW, {hash, node}).

start() ->
    ChildSpec = {?MODULE, {?MODULE, start_link, []},
		 permanent, brutal_kill, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

start_link() -> gen_server:start_link(?MODULE, [], []).

-spec get_node(any()) -> atom().

get_node(Key) ->
    Hash = erlang:phash2(Key),
    get_node_by_hash(?HASHTBL, Hash).

-spec get_node_new(any()) -> atom().

get_node_new(Key) ->
    Hash = erlang:phash2(Key),
    get_node_by_hash(?HASHTBL_NEW, Hash).

-spec get_nodes() -> [atom()].

get_nodes() -> mnesia:system_info(running_db_nodes).

-spec announce(pid()) -> any().

announce(Pid) ->
    gen_server:call(Pid, announce, infinity).

node_id() ->
    jlib:integer_to_binary(erlang:phash2(node())).

rehash_timeout() ->
    case ejabberd_config:get_local_option(
           rehash_timeout,
           fun(I) when is_integer(I), I > 0 -> I end) of
        undefined -> ?REHASH_TIMEOUT;
        Secs -> timer:seconds(Secs)
    end.

migrate_timeout() ->
    case ejabberd_config:get_local_option(
           migrate_timeout,
           fun(N) when is_integer(N), N > 0 -> N end) of
        undefined -> ?MIGRATE_TIMEOUT;
        Secs -> timer:seconds(Secs)
    end.

-spec get_node_by_id(binary() | atom()) -> atom().

get_node_by_id(NodeID) when is_binary(NodeID) ->
    case catch list_to_existing_atom(binary_to_list(NodeID)) of
      {'EXIT', _} -> node();
      Res -> get_node_by_id(Res)
    end;
get_node_by_id(NodeID) ->
    case global:whereis_name(NodeID) of
      Pid when is_pid(Pid) -> node(Pid);
      _ -> node()
    end.

shutdown() ->
    lists:foreach(fun (Node) when Node /= node() ->
			  {ejabberd_cluster, Node} ! {node_down, node()};
		      (_) -> ok
		  end,
		  get_nodes()).

shutdown_migrate(WaitTime) ->
    delete_node(?HASHTBL_NEW, node()),
    ejabberd_hooks:run(node_down, [node()]),
    shutdown(),
    delete_node(?HASHTBL, node()),
    ejabberd_hooks:run(node_hash_update,
		       [node(), down, WaitTime]),
    ?INFO_MSG("Waiting ~p seconds for the migration "
	      "to be completed.",
	      [WaitTime div 1000]),
    timer:sleep(WaitTime),
    ok.

init([]) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    net_kernel:monitor_nodes(true, [{node_type, visible}]),
    mnesia:create_table(?HASHTBL,
                        [{ram_copies, [node()]},
                         {type, ordered_set},
			 {local_content, true},
			 {attributes, record_info(fields, ?HASHTBL)}]),
    mnesia:create_table(?HASHTBL_NEW,
                        [{ram_copies, [node()]},
                         {type, ordered_set},
			 {local_content, true},
			 {attributes, record_info(fields, ?HASHTBL_NEW)}]),
    mnesia:add_table_copy(?HASHTBL, node(), ram_copies),
    mnesia:add_table_copy(?HASHTBL_NEW, node(), ram_copies),
    mnesia:clear_table(?HASHTBL),
    mnesia:clear_table(?HASHTBL_NEW),
    register_node(),
    AllNodes = get_nodes(),
    OtherNodes = case AllNodes of
		   [_MyNode] -> AllNodes;
		   _ -> AllNodes -- [node()]
		 end,
    append_nodes(?HASHTBL, OtherNodes),
    append_nodes(?HASHTBL_NEW, AllNodes),
    {ok, #state{}}.

handle_call(announce, _From, State) ->
    Migrate_timeout = migrate_timeout(),
    case global:set_lock(?LOCK, get_nodes(), 0) of
      false ->
	  ?WARNING_MSG("Another node is recently attached to "
		       "the cluster and is being rebalanced. "
		       "Waiting for the rebalancing to be completed "
		       "before starting this node. This will "
		       "take at least ~p seconds. Please, be "
		       "patient.",
		       [Migrate_timeout div 1000]),
	  global:set_lock(?LOCK, get_nodes(), infinity);
      true -> ok
    end,
    case get_nodes() of
      [_MyNode] ->
	  register(?MODULE, self()), global:del_lock(?LOCK);
      Nodes ->
	  OtherNodes = Nodes -- [node()],
	  ?INFO_MSG("waiting for migration from nodes: ~w",
		    [OtherNodes]),
	  {_Res, BadNodes} = gen_server:multi_call(OtherNodes,
						   ?MODULE,
						   {node_ready, node()},
						   ?REHASH_TIMEOUT),
	  append_node(?HASHTBL, node()),
	  register(?MODULE, self()),
	  case OtherNodes -- BadNodes of
	    [] -> global:del_lock(?LOCK);
	    WorkingNodes ->
		gen_server:abcast(WorkingNodes, ?MODULE,
				  {node_ready, node()}),
		erlang:send_after(Migrate_timeout, self(), del_lock)
	  end
    end,
    {reply, ok, State};
handle_call({node_ready, Node}, _From, State) ->
    ?INFO_MSG("node ~p is ready, preparing migration",
	      [Node]),
    append_node(?HASHTBL_NEW, Node),
    ejabberd_hooks:run(node_up, [Node]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_cast({node_ready, Node}, State) ->
    ?INFO_MSG("adding node ~p to hash and starting "
	      "migration",
	      [Node]),
    append_node(?HASHTBL, Node),
    ejabberd_hooks:run(node_hash_update,
		       [Node, up, migrate_timeout()]),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(del_lock, State) ->
    global:del_lock(?LOCK), {noreply, State};
handle_info({node_down, Node}, State) ->
    delete_node(?HASHTBL, Node),
    delete_node(?HASHTBL_NEW, Node),
    {noreply, State};
handle_info({nodedown, Node, _}, State) ->
    ?INFO_MSG("node ~p goes down", [Node]),
    ejabberd_hooks:run(node_down, [Node]),
    delete_node(?HASHTBL, Node),
    delete_node(?HASHTBL_NEW, Node),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

append_nodes(Tab, Nodes) ->
    lists:foreach(fun (Node) -> append_node(Tab, Node) end,
		  Nodes).

append_node(Tab, Node) ->
    lists:foreach(
      fun(I) ->
              Hash = erlang:phash2({I, Node}),
              mnesia:dirty_write({Tab, Hash, Node})
      end, lists:seq(1, ?POINTS)).

delete_node(Tab, Node) ->
    lists:foreach(
      fun(I) ->
              Hash = erlang:phash2({I, Node}),
              mnesia:dirty_delete(Tab, Hash)
      end, lists:seq(1, ?POINTS)).

get_node_by_hash(Tab, Hash) ->
    NodeHash = case ets:next(Tab, Hash) of
		 '$end_of_table' -> ets:first(Tab);
		 NH -> NH
	       end,
    if NodeHash == '$end_of_table' ->
            node();
       true ->
	    case ets:lookup(Tab, NodeHash) of
		[] ->
		    get_node_by_hash(Tab, Hash);
		[{_, _, Node}] ->
		    Node
	    end
    end.

register_node() ->
    global:register_name(jlib:binary_to_atom(node_id()),
			 self()).
