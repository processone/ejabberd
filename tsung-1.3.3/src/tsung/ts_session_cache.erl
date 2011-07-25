%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%%-------------------------------------------------------------------
%%% File    : ts_session_cache.erl
%%% Author  : Nicolas Niclausse <nniclausse@schultze.ird.idealx.com>
%%% Description : cache sessions request from ts_config_server
%%%
%%% Created :  2 Dec 2003 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------


-module(ts_session_cache).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start/0, get_req/2, get_user_agent/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


-record(state, {
          table, % ets table
          hit  =0.0, % number of hits
          total=0.0  % total number of requests
         }).

-define(DUMP_STATS_INTERVAL, 500). % in milliseconds

-include("ts_profile.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    ?LOG("Starting~n",?INFO),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: get_req/2
%% Description: get next request from session 'Id'
%%--------------------------------------------------------------------
get_req(Id, Count)->
    gen_server:call(?MODULE,{get_req, Id, Count}).

%%--------------------------------------------------------------------
%% Function: get_user_agent/0
%%--------------------------------------------------------------------
get_user_agent()->
    gen_server:call(?MODULE,{get_user_agent}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    Table = ets:new(sessiontable, [set, private]),
    {ok, #state{table=Table}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% get Nth request from given session Id
handle_call({get_req, Id, N}, _From, State) ->
    Tab = State#state.table,
    Total = State#state.total+1,
    ?DebugF("look for ~p th request in session ~p for ~p~n",[N,Id,_From]),
    case ets:lookup(Tab, {Id, N}) of
        [{_Key, Session}] ->
            Hit = State#state.hit+1,
            ?DebugF("ok, found in cache for ~p~n",[_From]),
            ?DebugF("hitrate is ~.3f~n",[100.0*Hit/Total]),
            {reply, Session, State#state{hit= Hit, total = Total}};
        [] -> %% no match, ask the config_server
            ?DebugF("not found in cache (~p th request in session ~p for ~p)~n",[N,Id,_From]),
            case catch ts_config_server:get_req(Id, N) of
                {'EXIT',Reason}  ->
                    {reply, {error, Reason}, State};
                Reply ->
                    %% cache the response FIXME: handle bad response ?
                    ets:insert(Tab, {{Id, N}, Reply}),
                    {reply, Reply, State#state{total = Total}}
            end;
        Other -> %%
            ?LOGF("error ! (~p)~n",[Other],?WARN),
            {reply, {error, Other}, State}
    end;

handle_call({get_user_agent}, _From, State) ->
    Tab = State#state.table,
    case ets:lookup(Tab, {http_user_agent, value}) of
        [] -> %% no match, ask the config_server
            ?Debug("user agents not found in cache~n"),
            UserAgents = ts_config_server:get_user_agents(),
            %% cache the response FIXME: handle bad response ?
            ?DebugF("Useragents: got from config_server~p~n",[UserAgents]),
            ets:insert(Tab, {{http_user_agent, value}, UserAgents}),
            {ok, Reply} = choose_user_agent(UserAgents),
            {reply, Reply, State};
        [{_, [{_Freq, Value}]}] -> %single user agent defined
            {reply, Value, State};
        [{_, empty }] ->
            {reply, "tsung", State};
        [{_, UserAgents }] when is_list(UserAgents)->
            {ok, Reply} = choose_user_agent(UserAgents),
            {reply, Reply, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    ?LOGF("Die ! (~p)~n",[Reason],?ERR),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

choose_user_agent(empty) -> {ok, "tsung"};
choose_user_agent([{_P, Val}]) -> {ok, Val};
choose_user_agent(UserAgents) ->
    choose_user_agent(UserAgents, random:uniform(100),0).

choose_user_agent([{P, Val} | _],Rand, Cur) when Rand =< P+Cur->
    {ok, Val};
choose_user_agent([{P, _Val} | SList], Rand, Cur) ->
    choose_user_agent(SList, Rand, Cur+P).

