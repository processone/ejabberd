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
-export([start_link/0, get_node/1, get_node_new/1, announce/0,
	 node_id/0, get_node_by_id/1, get_nodes/0, rehash_timeout/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").

-define(HASHTBL, nodes_hash).
-define(HASHTBL_NEW, nodes_hash_new).
-define(POINTS, 64).
-define(REHASH_TIMEOUT, 30000).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_node(Key) ->
    Hash = erlang:phash2(Key),
    get_node_by_hash(?HASHTBL, Hash).

get_node_new(Key) ->
    Hash = erlang:phash2(Key),
    get_node_by_hash(?HASHTBL_NEW, Hash).

get_nodes() ->
    %% TODO
    mnesia:system_info(running_db_nodes).

announce() ->
    gen_server:call(?MODULE, announce, infinity).

node_id() ->
    integer_to_list(erlang:phash2(node())).

rehash_timeout() ->
    ?REHASH_TIMEOUT.

get_node_by_id(NodeID) when is_list(NodeID) ->
    case catch list_to_existing_atom(NodeID) of
	{'EXIT', _} ->
	    node();
	Res ->
	    get_node_by_id(Res)
    end;
get_node_by_id(NodeID) ->
    case global:whereis_name(NodeID) of
	Pid when is_pid(Pid) ->
	    node(Pid);
	_ ->
	    node()
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    net_kernel:monitor_nodes(true, [{node_type, visible}]),
    ets:new(?HASHTBL, [named_table, public, ordered_set]),
    ets:new(?HASHTBL_NEW, [named_table, public, ordered_set]),
    register_node(),
    AllNodes = mnesia:system_info(running_db_nodes),
    OtherNodes = case AllNodes of
		     [_] ->
			 AllNodes;
		     _ ->
			 AllNodes -- [node()]
		 end,
    append_nodes(?HASHTBL, OtherNodes),
    append_nodes(?HASHTBL_NEW, AllNodes),
    {ok, #state{}}.

handle_call(announce, _From, State) ->
    case mnesia:system_info(running_db_nodes) of
	[_MyNode] ->
	    ok;
	Nodes ->
	    OtherNodes = Nodes -- [node()],
	    lists:foreach(
	      fun(Node) ->
		      {?MODULE, Node} ! {node_ready, node()}
	      end, OtherNodes),
	    ?INFO_MSG("waiting for migration from nodes: ~w",
		      [OtherNodes]),
	    timer:sleep(?REHASH_TIMEOUT),
	    append_node(?HASHTBL, node())
    end,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({node_ready, Node}, State) ->
    ?INFO_MSG("node ~p is ready, starting migration", [Node]),
    append_node(?HASHTBL_NEW, Node),
    ejabberd_hooks:run(node_hash_update, [?REHASH_TIMEOUT]),
    timer:sleep(?REHASH_TIMEOUT),
    ?INFO_MSG("adding node ~p to hash", [Node]),
    append_node(?HASHTBL, Node),
    {noreply, State};
handle_info({nodedown, Node, _}, State) ->
    ?INFO_MSG("node ~p goes down", [Node]),
    delete_node(?HASHTBL, Node),
    delete_node(?HASHTBL_NEW, Node),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
append_nodes(Tab, Nodes) ->
    lists:foreach(
      fun(Node) ->
	      append_node(Tab, Node)
      end, Nodes).

append_node(Tab, Node) ->
    lists:foreach(
      fun(I) ->
	      Hash = erlang:phash2({I, Node}),
	      ets:insert(Tab, {Hash, Node})
      end, lists:seq(1, ?POINTS)).

delete_node(Tab, Node) ->
    lists:foreach(
      fun(I) ->
	      Hash = erlang:phash2({I, Node}),
	      ets:delete(Tab, Hash)
      end, lists:seq(1, ?POINTS)).

get_node_by_hash(Tab, Hash) ->
    NodeHash = case ets:next(Tab, Hash) of
		   '$end_of_table' ->
		       ets:first(Tab);
		   NH ->
		       NH
	       end,
    if NodeHash == '$end_of_table' ->
	    erlang:error(no_running_nodes);
       true ->
	    case ets:lookup(Tab, NodeHash) of
		[] ->
		    get_node_by_hash(Tab, Hash);
		[{_, Node}] ->
		    Node
	    end
    end.

register_node() ->
    global:register_name(list_to_atom(node_id()), self()).
