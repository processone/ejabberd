%%%------------------------------------------------------------------------
%%% File:   ejabberd_snmp_rt.erl
%%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%%%         Radoslaw Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>
%%% Description: Module for snmp counters that require periodical calculations
%%%
%%% Created: 5 Aug 2011 by <aleksandra.lipiec@erlang-solutions.com>
%%%-----------------------------------------------------------------------
-module(ejabberd_snmp_rt).

-behaviour(gen_server).


-include("ejabberd.hrl").
-include("jlib.hrl").

%%--------------------
%% API
%%--------------------

%% will potentially include function for updating temporary window counters
-export([start_link/1,
         stop/0]).

-define(DEFAULT_INTERVAL, 60).
-define(MIN_INTERVAL, 10).
-define(COUNTERS, [globalSessionCount,       %% counters computed by this module
                   globalUniqueSessionCount,
                   modRosterSize]). 


%% Internal exports (gen_server)
-export([init/1,
         start_link2/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {timer_ref,         % ref to timer
                computing_num}).   % number of counters in computation

-record(session, {sid, usr, us, priority, info}).

%%--------------------
%% Implementation
%%--------------------

-spec start_link(integer()) -> {ok, pid()} | {error, term()}.
start_link(Interval) ->                
    NewInterval = case Interval of
                      I when I < ?MIN_INTERVAL ->
                          ?MIN_INTERVAL;
                      I when is_integer(I) ->
                          I;
                      _ ->
                          ?DEFAULT_INTERVAL
                  end,
    ?INFO_MSG("NewInterval: ~p", [NewInterval]),
    Spec = {?MODULE, {?MODULE, start_link2, [NewInterval]},
            transient, 2000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_sup, Spec) of
        {error, Error} ->
            {error, Error};
        Ok ->
            Ok
    end.

-spec start_link2(integer()) -> {ok, pid()} | {error, term()}.
start_link2(Interval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Interval], []).

-spec stop() -> term().
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

%%----------------------
%% Callbacks
%%----------------------


init([Interval]) ->
    ets:new(?MODULE, [public, named_table]),
    {ok, TickRef} = start_timer(Interval, tick),
    {ok, #state{timer_ref = TickRef,
                computing_num = 0}}.

%%-------------------

handle_call({change_interval, NewInterval}, _From, 
            #state{timer_ref = TickRef} = State) ->
    timer:cancel(TickRef),
    {ok, NewTickRef} = start_timer(NewInterval, tick),
    {reply, ok, State#state{timer_ref = NewTickRef}};

handle_call(Request, _From, State) ->
    ?WARNING_MSG("~p received unknown call ~p ", [?MODULE, Request]),
    {noreply, State}.

%%-------------------

handle_cast({compute, globalSessionCount}, 
            #state{computing_num = Num} = State) ->
    Count = ets:info(session, size),
    ejabberd_snmp_core:set_counter(globalSessionCount, Count),
    {noreply, State#state{computing_num = Num - 1}};

handle_cast({compute, globalUniqueSessionCount}, 
            #state{computing_num = Num} = State) ->
    ets:delete_all_objects(?MODULE),
    Count = compute_unique(ets:first(session)),
    ejabberd_snmp_core:set_counter(globalUniqueSessionCount, Count),
    {noreply, State#state{computing_num = Num - 1}};

handle_cast({compute, modRosterSize}, 
            #state{computing_num = Num} = State) ->
    %% TODO
    %% ejabberd_snmp_core:set_counter(modRosterSize, X),
    {noreply, State#state{computing_num = Num - 1}};


%% Similar for all rt counters

handle_cast(Request, State) ->
    ?WARNING_MSG("~p received unknown cast ~p ", [?MODULE, Request]),
    {noreply, State}.

%%-------------------

handle_info(tick, #state{computing_num = 0} = State) ->
    Num = lists:foldl(fun(Counter, Res) ->
                              gen_server:cast(?MODULE, {compute, Counter}),
                              Res +1
                      end, 0,  ?COUNTERS),
    {noreply, State#state{computing_num = Num}};

handle_info(tick, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?WARNING_MSG("~p received unknown info ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.

%%-------------------

terminate(_Reason, _State) ->
    ok.

%%-------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------
%% Helpers
%%--------------------

start_timer(Interval, Type) ->
    timer:send_interval(Interval*1000, self(), Type).

compute_unique('$end_of_table') ->
    ets:info(?MODULE, size);
compute_unique(Key) ->
    case ets:lookup(session, Key) of
        [#session{usr = {User, Server, _}}] ->
            ets:insert(?MODULE, {list_to_binary([User, Server])});
        _ ->
            ok
    end,
    compute_unique(ets:next(session, Key)).
