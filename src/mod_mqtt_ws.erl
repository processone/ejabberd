%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2022 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(mod_mqtt_ws).
-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).

%% API
-export([socket_handoff/3]).
-export([start/1, start_link/1]).
-export([peername/1, setopts/2, send/2, close/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").
-include("logger.hrl").

-define(SEND_TIMEOUT, timer:seconds(15)).

-record(state, {socket :: socket(),
		ws_pid :: pid(),
		mqtt_session :: undefined | pid()}).

-type peername() :: {inet:ip_address(), inet:port_number()}.
-type socket() :: {http_ws, pid(), peername()}.
-export_type([socket/0]).

%%%===================================================================
%%% API
%%%===================================================================
socket_handoff(LocalPath, Request, Opts) ->
    ejabberd_websocket:socket_handoff(
      LocalPath, Request, Opts, ?MODULE, fun get_human_html_xmlel/0).

start({#ws{http_opts = Opts}, _} = WS) ->
    ?GEN_SERVER:start(?MODULE, [WS], ejabberd_config:fsm_limit_opts(Opts)).

start_link({#ws{http_opts = Opts}, _} = WS) ->
    ?GEN_SERVER:start_link(?MODULE, [WS], ejabberd_config:fsm_limit_opts(Opts)).

-spec peername(socket()) -> {ok, peername()}.
peername({http_ws, _, IP}) ->
    {ok, IP}.

-spec setopts(socket(), list()) -> ok.
setopts(_WSock, _Opts) ->
    ok.

-spec send(socket(), iodata()) -> ok | {error, timeout | einval}.
send({http_ws, Pid, _}, Data) ->
    try ?GEN_SERVER:call(Pid, {send, Data}, ?SEND_TIMEOUT)
    catch exit:{timeout, {?GEN_SERVER, _, _}} ->
	    {error, timeout};
	  exit:{_, {?GEN_SERVER, _, _}} ->
	    {error, einval}
    end.

-spec close(socket()) -> ok.
close({http_ws, Pid, _}) ->
    ?GEN_SERVER:cast(Pid, close).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([{#ws{ip = IP, http_opts = ListenOpts}, WsPid}]) ->
    Socket = {http_ws, self(), IP},
    case mod_mqtt_session:start(?MODULE, Socket, ListenOpts) of
	{ok, Pid} ->
	    erlang:monitor(process, Pid),
	    erlang:monitor(process, WsPid),
	    mod_mqtt_session:accept(Pid),
	    State = #state{socket = Socket,
			   ws_pid = WsPid,
			   mqtt_session = Pid},
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason};
	ignore ->
	    ignore
    end.

handle_call({send, Data}, _From, #state{ws_pid = WsPid} = State) ->
    WsPid ! {data, Data},
    {reply, ok, State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(close, State) ->
    {stop, normal, State#state{mqtt_session = undefined}};
handle_cast(Request, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info(closed, State) ->
    {stop, normal, State};
handle_info({received, Data}, State) ->
    State#state.mqtt_session ! {tcp, State#state.socket, Data},
    {noreply, State};
handle_info({'DOWN', _, process, Pid, _}, State)
  when Pid == State#state.mqtt_session orelse Pid == State#state.ws_pid ->
    {stop, normal, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    if State#state.mqtt_session /= undefined ->
	    State#state.mqtt_session ! {tcp_closed, State#state.socket};
       true ->
	    ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_human_html_xmlel() -> xmlel().
get_human_html_xmlel() ->
    Heading = <<"ejabberd mod_mqtt">>,
    #xmlel{name = <<"html">>,
           attrs =
               [{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>}],
           children =
               [#xmlel{name = <<"head">>, attrs = [],
                       children =
                           [#xmlel{name = <<"title">>, attrs = [],
                                   children = [{xmlcdata, Heading}]}]},
                #xmlel{name = <<"body">>, attrs = [],
                       children =
                           [#xmlel{name = <<"h1">>, attrs = [],
                                   children = [{xmlcdata, Heading}]},
                            #xmlel{name = <<"p">>, attrs = [],
                                   children =
                                       [{xmlcdata, <<"An implementation of ">>},
                                        #xmlel{name = <<"a">>,
                                               attrs =
                                                   [{<<"href">>,
                                                     <<"http://tools.ietf.org/html/rfc6455">>}],
                                               children =
                                                   [{xmlcdata,
                                                     <<"WebSocket protocol">>}]}]},
                            #xmlel{name = <<"p">>, attrs = [],
                                   children =
                                       [{xmlcdata,
                                         <<"This web page is only informative. To "
                                           "use WebSocket connection you need an MQTT "
                                           "client that supports it.">>}]}]}]}.
