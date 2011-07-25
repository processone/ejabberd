%%%
%%%  @copyright IDEALX S.A.S. 2003-2005
%%%
%%%  @author Nicolas Niclausse <nicolas@niclux.org>
%%%  @doc    Record request by calling the plugin involved
%%%  @since  1.0.beta1, 22 Dec 2003 by Nicolas Niclausse
%%%  @version {@version}
%%%  @end
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

-module(ts_proxy_recorder).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_profile.hrl").
-include("ts_http.hrl").
-include("ts_recorder.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start/1, dorecord/1, recordtag/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/1
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Config) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% Function: stop/1
%%--------------------------------------------------------------------
stop(_) ->
    gen_server:call({global, ?MODULE},{stop}).

%%--------------------------------------------------------------------
%% Function: dorecord/1
%% Description: record a new request
%%--------------------------------------------------------------------
dorecord(Args)->
    gen_server:cast({global, ?MODULE},{record, Args}).

%%--------------------------------------------------------------------
%% Function: recordtag/1
%% Description: record a string (for use on the command line)
%%--------------------------------------------------------------------
recordtag([Host,Args]) when is_list(Host)->
    recordtag(list_to_atom(Host), Args).

%% @spec recordtag(Host::string(), Args::term()) -> ok
recordtag(Host, Args) when is_list(Args)->
    _List = net_adm:world_list([Host]),
    global:sync(),
    gen_server:cast({global,?MODULE},{record, Args}).

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
init(Filename) ->
    Date = ts_utils:datestr(),
    %% add date to filename
    File = case regexp:gsub(Filename,"\.xml$", Date ++ ".xml") of %% "
               {ok, RealName, _ } -> RealName;
               _ ->  Date ++ "-" ++ Filename
           end,
    case file:open(File,[write]) of
        {ok, Stream} ->
            Plugin = ?config(plugin),
            erlang:display(lists:flatten(["Record file: ",File])),
            ?LOGF("starting recorder with plugin ~s : ~s~n",[Plugin,File],?NOTICE),
            {ok, #state_rec{ log_file = File,
                             logfd    = Stream,
                             ext_file_id=1,
                             plugin   = Plugin
                            }};
        {error, Reason} ->
            ?LOGF("Can't open log file ~p! ~p~n",[File,Reason], ?ERR),
            {stop, Reason}
    end.

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
handle_call({stop}, _From, State) ->
    io:format(State#state_rec.logfd,"</session>~n",[]),
    file:close(State#state_rec.logfd),
    {stop, normal, ok, State};

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
handle_cast({record, endsession}, State) ->
    io:format(State#state_rec.logfd,"</session>"),
    {noreply, State};

handle_cast({record, {Request}}, State=#state_rec{timestamp=0,plugin=Plugin}) -> % first record
    Name= ts_utils:datestr(),
    Type = Plugin:gettype(),
    io:format(State#state_rec.logfd,"<session name='~s' probability='100' "++
              " type='~s'>~n",["rec"++Name, Type]),
    {ok, NewState} = Plugin:record_request(State, Request),
    {noreply, NewState#state_rec{timestamp=now()}};

handle_cast({record, {Request}}, State=#state_rec{plugin=Plugin}) ->
    TimeStamp=now(),
    Elapsed = ts_utils:elapsed(State#state_rec.timestamp,TimeStamp),
    case Elapsed < State#state_rec.thinktime_low of
        true ->
            ?LOGF("skip too low thinktime, assuming it's an embedded object (~p)~n",
                  [Elapsed],?INFO);
        false ->
            io:format(State#state_rec.logfd,
                      "~n<thinktime random='true' value='~p'/>~n~n",
                      [round(Elapsed/1000)])
    end,
    {ok, NewState} = Plugin:record_request(State, Request),
    {noreply, NewState#state_rec{timestamp=TimeStamp}};

handle_cast({record, String}, State) when is_list(String)->
    ?LOGF("Record string ~p~n",[String], ?NOTICE),
    io:format(State#state_rec.logfd, "~n~s~n", [String]),
    {noreply, State};

handle_cast(Msg, State) ->
    ?LOGF("IGNORE Msg ~p~n",[Msg], ?WARN),
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
terminate(_Reason, _State) ->
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


