%%%
%%%  Copyright (C) 2008 Nicolas Niclausse
%%%
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


%%----------------------------------------------------------------------
%% @copyright 2008 Nicolas Niclausse
%% @author Nicolas Niclausse <nicolas@niclux.org>
%% @since 1.3.1 , 19 Nov 2008
%% @doc log match entries @end
%% ----------------------------------------------------------------------

-module(ts_match_logger).
-author('nicolas@niclux.org').
-vc('$Id: ts_mon.erl 774 2007-11-20 09:36:13Z nniclausse $ ').

-behaviour(gen_server).

-include("ts_profile.hrl").
-include("ts_config.hrl").

-define(DELAYED_WRITE_SIZE,524288). % 512KB
-define(DELAYED_WRITE_DELAY,5000).  % 5 sec

%% External exports, API
-export([start/1, stop/0, add/1 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {filename,      % log filename
                level,        % type of backend: text|rrdtool|fullstats
                fd            % file descriptor
               }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec start(LogDir::string()) -> ok | throw({error, Reason})
%% @doc Start the monitoring process
%% @end
%%----------------------------------------------------------------------
start(LogDir) ->
    ?LOG("starting match logger, global ~n",?NOTICE),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).

stop() ->
    gen_server:cast({global, ?MODULE}, {stop}).

%%----------------------------------------------------------------------
%% @spec add(Data::list()| {UserId::integer(),SessionId::integer(),
%%  RequestId::integer(),TimeStamp::tuple(),{count, Val::atom()}}) -> ok
%% @doc log match entries
%% @end
%%----------------------------------------------------------------------
add(Data) ->
    gen_server:cast({global, ?MODULE}, {add, Data}).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([LogDir]) ->
    ?LOG("starting match logger~n",?NOTICE),
    Base = filename:basename(?config(match_log_file)),
    Filename = filename:join(LogDir, Base),
    case file:open(Filename,[write, {delayed_write, ?DELAYED_WRITE_SIZE, ?DELAYED_WRITE_DELAY}]) of
        {ok, Fd} ->
            ?LOG("starting match logger~n",?NOTICE),
            io:format(Fd,"# timestamp userid sessionid requestid event~n",[]),
            {ok, #state{ fd       = Fd,
                         filename = Filename
                       }};
        {error, Reason} ->
            ?LOGF("Can't open match log file! ~p~n",[Reason], ?ERR),
            {stop, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, _From, State) ->
    ?LOGF("Unknown call ~p !~n",[Request],?ERR),
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({add, List}, State) when is_list(List)->
    lists:foreach(fun(X)-> log(X,State#state.fd) end, List),
    {noreply,State};

handle_cast({add, Data}, State) when is_tuple(Data)->
    log(Data,State#state.fd),
    {noreply,State};

handle_cast({stop}, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
    ?LOGF("Unknown msg ~p !~n",[Msg], ?WARN),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ?LOGF("stoping match logger (~p)~n",[Reason],?NOTICE),
    file:close(State#state.fd),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

log({UserId,SessionId,RequestId,TimeStamp,{count, Val}}, File) ->
    TS=ts_utils:time2sec(TimeStamp),
    io:format(File,"~B ~B ~B ~B ~p~n",[TS,UserId,SessionId,RequestId,Val]).


