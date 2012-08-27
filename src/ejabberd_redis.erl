-module(ejabberd_redis).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("ejabberd.hrl").

%% API
-export([start_link/5, stop/0, multi/1, getter/1, setter/2, set_add/2, set_rem/2, set_members/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link(Server, Port, Database, Password, PoolSize) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Server, Port, Database, Password, PoolSize], []).

%%--------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc Stops the server
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% @spec multi() -> ok
%% @doc Begin a transaction
%% @end
%%--------------------------------------------------------------------
multi(Transaction) ->
    {ok, Worker} = gen_server:call(?SERVER, worker),
    gen_server:call(Worker, {multi, Transaction}).

%%--------------------------------------------------------------------
%% @spec getter() -> [Binary1, Binary2, ..., BinaryN] | Binary
%% @doc Gets a value, given by its key
%% @end
%%--------------------------------------------------------------------
getter(Key) ->
    {ok, Worker} = gen_server:call(?SERVER, worker),
    gen_server:call(Worker, {get, Key}).

set_add(Key, Value) ->
    {ok, Worker} = gen_server:call(?SERVER, worker),
    gen_server:call(Worker, {sadd, Key, Value}).

set_rem(Key, Value) ->
    {ok, Worker} = gen_server:call(?SERVER, worker),
    gen_server:call(Worker, {srem, Key, Value}).

set_members(Key) ->
    {ok, Worker} = gen_server:call(?SERVER, worker),
    gen_server:call(Worker, {smembers, Key}).

%%--------------------------------------------------------------------
%% @spec setter() -> ok
%% @doc Sets a given value for a given key
%% @end
%%--------------------------------------------------------------------
setter(Key, Value) ->
    {ok, Worker} = gen_server:call(?SERVER, worker),
    gen_server:call(Worker, {set, Key, Value}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([Server, Port, Database, Password, PoolSize]) ->
    Pool = lists:map(fun(_X) ->
        {ok, Pid} = ejabberd_redis_worker:start_link(Server, Port, Database, Password),
        Pid
    end, lists:seq(1, PoolSize)),
    {ok, #state{pool = Pool}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(worker, _From, #state{pool=[C|Rest]}=State) ->
    {reply, {ok, C}, State#state{pool=Rest ++ [C]}};
handle_call(_Request, _From, State) ->
    Reply = noproc,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    lists:foreach(fun(X) ->
        gen_server:call(X, stop)
    end, State#state.pool),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
