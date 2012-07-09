-module(ejabberd_redis).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("ejabberd.hrl").

%% API
-export([start_link/4, stop/0, multi/1, getter/1, setter/2, set_add/2, set_rem/2, set_members/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {server, port, database, password, conn}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link(Server, Port, Database, Password) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Server, Port, Database, Password], []).

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
    gen_server:call(?SERVER, {multi, Transaction}).

%%--------------------------------------------------------------------
%% @spec getter() -> [Binary1, Binary2, ..., BinaryN] | Binary
%% @doc Gets a value, given by its key
%% @end
%%--------------------------------------------------------------------
getter(Key) ->
    gen_server:call(?SERVER, {get, Key}).

set_add(Key, Value) ->
    gen_server:call(?SERVER, {sadd, Key, Value}).

set_rem(Key, Value) ->
    gen_server:call(?SERVER, {srem, Key, Value}).

set_members(Key) ->
    gen_server:call(?SERVER, {smembers, Key}).

%%--------------------------------------------------------------------
%% @spec setter() -> ok
%% @doc Sets a given value for a given key
%% @end
%%--------------------------------------------------------------------
setter(Key, Value) ->
    gen_server:call(?SERVER, {set, Key, Value}).

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
init([Server, Port, Database, Password]) ->
    {ok, C} = eredis:start_link(Server, Port, Database, Password),
    {ok, #state{server=Server, port=Port, database=Database, password=Password, conn=C}}.

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
handle_call({smembers, Key}, _From, #state{conn=C}=State) ->
    {ok, Reply} = eredis:q(C, ["SMEMBERS", Key]),
    {reply, Reply, State};
handle_call({srem, Key, Value}, _From, #state{conn=C}=State) ->
    {ok, _} = eredis:q(C, ["SREM", Key, Value]),
    {reply, ok, State};
handle_call({sadd, Key, Value}, _From, #state{conn=C}=State) ->
    {ok, _} = eredis:q(C, ["SADD", Key, Value]),
    {reply, ok, State};
handle_call({multi, Transaction}, _From, #state{conn=C}=State) ->
    eredis:q(C, ["MULTI"]),
    lists:map(fun(X) -> eredis:q(C, X) end, Transaction),
    {ok, Reply} = eredis:q(C, ["EXEC"]),
    {reply, Reply, State};
handle_call({get, Key}, _From, #state{conn=C}=State) ->
    {ok, Reply} = eredis:q(C, ["GET", Key]),
    {reply, Reply, State};
handle_call({set, Key, Value}, _From, #state{conn=C}=State) ->
    {ok, <<"OK">>} = eredis:q(C, ["SET", Key, Value]),
    {reply, ok, State};
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
handle_cast({set, Key, Value}, #state{conn=C}=State) ->
    {ok, <<"OK">>} = eredis:q(C, ["SET", Key, Value]),
    {noreply, State};
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
terminate(_Reason, _State) ->
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
