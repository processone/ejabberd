%%%----------------------------------------------------------------------
%%% File    : ejabberd_websocket.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : XMPP Websocket support
%%% Created : 09-10-2010 by Eric Cestari <ecestari@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(ejabberd_http_ws).
-author('ecestari@process-one.net').
-behaviour(xmpp_socket).
-behaviour(p1_fsm).

-export([start/1, start_link/1, init/1, handle_event/3,
	 handle_sync_event/4, code_change/4, handle_info/3,
	 terminate/3, send_xml/2, setopts/2, sockname/1,
	 peername/1, controlling_process/2, get_owner/1,
	 reset_stream/1, close/1, change_shaper/2,
	 socket_handoff/3, get_transport/1]).

-include("logger.hrl").

-include("xmpp.hrl").

-include("ejabberd_http.hrl").

-record(state,
        {socket                       :: ws_socket(),
         ping_interval                :: non_neg_integer(),
         ping_timer = make_ref()      :: reference(),
         pong_expected = false        :: boolean(),
         timeout                      :: non_neg_integer(),
         timer = make_ref()           :: reference(),
         input = []                   :: list(),
	 active = false               :: boolean(),
	 c2s_pid                      :: pid(),
         ws                           :: {#ws{}, pid()},
         rfc_compilant = undefined    :: boolean() | undefined}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

-type ws_socket() :: {http_ws, pid(), {inet:ip_address(), inet:port_number()}}.
-export_type([ws_socket/0]).

start(WS) ->
    p1_fsm:start(?MODULE, [WS], ?FSMOPTS).

start_link(WS) ->
    p1_fsm:start_link(?MODULE, [WS], ?FSMOPTS).

send_xml({http_ws, FsmRef, _IP}, Packet) ->
    case catch p1_fsm:sync_send_all_state_event(FsmRef,
						    {send_xml, Packet},
						    15000)
    of
	{'EXIT', {timeout, _}} -> {error, timeout};
	{'EXIT', _} -> {error, einval};
	Res -> Res
    end.

setopts({http_ws, FsmRef, _IP}, Opts) ->
    case lists:member({active, once}, Opts) of
      true ->
	  p1_fsm:send_all_state_event(FsmRef,
				       {activate, self()});
      _ -> ok
    end.

sockname(_Socket) -> {ok, {{0, 0, 0, 0}, 0}}.

peername({http_ws, _FsmRef, IP}) -> {ok, IP}.

controlling_process(_Socket, _Pid) -> ok.

close({http_ws, FsmRef, _IP}) ->
    catch p1_fsm:sync_send_all_state_event(FsmRef, close).

reset_stream({http_ws, _FsmRef, _IP} = Socket) ->
    Socket.

change_shaper({http_ws, FsmRef, _IP}, Shaper) ->
    p1_fsm:send_all_state_event(FsmRef, {new_shaper, Shaper}).

get_transport(_Socket) ->
    websocket.

get_owner({http_ws, FsmRef, _IP}) ->
    FsmRef.

socket_handoff(LocalPath, Request, Opts) ->
    ejabberd_websocket:socket_handoff(LocalPath, Request, Opts, ?MODULE, fun get_human_html_xmlel/0).

%%% Internal

init([{#ws{ip = IP, http_opts = HOpts}, _} = WS]) ->
    SOpts = lists:filtermap(fun({stream_management, _}) -> true;
                               ({max_ack_queue, _}) -> true;
                               ({ack_timeout, _}) -> true;
                               ({resume_timeout, _}) -> true;
                               ({max_resume_timeout, _}) -> true;
                               ({resend_on_timeout, _}) -> true;
                               ({access, _}) -> true;
                               (_) -> false
                            end, HOpts),
    Opts = ejabberd_c2s_config:get_c2s_limits() ++ SOpts,
    PingInterval = ejabberd_option:websocket_ping_interval(),
    WSTimeout = ejabberd_option:websocket_timeout(),
    Socket = {http_ws, self(), IP},
    ?DEBUG("Client connected through websocket ~p",
	   [Socket]),
    case ejabberd_c2s:start(?MODULE, Socket, [{receiver, self()}|Opts]) of
	{ok, C2SPid} ->
	    ejabberd_c2s:accept(C2SPid),
	    Timer = erlang:start_timer(WSTimeout, self(), []),
	    {ok, loop,
	     #state{socket = Socket, timeout = WSTimeout,
		    timer = Timer, ws = WS, c2s_pid = C2SPid,
		    ping_interval = PingInterval}};
	{error, Reason} ->
	    {stop, Reason};
	ignore ->
	    ignore
    end.

handle_event({activate, From}, StateName, State) ->
    State1 = case State#state.input of
		 [] -> State#state{active = true};
		 Input ->
		     lists:foreach(
		       fun(I) when is_binary(I)->
			       From ! {tcp, State#state.socket, I};
			  (I2) ->
			       From ! {tcp, State#state.socket, [I2]}
		       end, Input),
		     State#state{active = false, input = []}
	     end,
    {next_state, StateName, State1#state{c2s_pid = From}};
handle_event({new_shaper, Shaper}, StateName, #state{ws = {_, WsPid}} = StateData) ->
    WsPid ! {new_shaper, Shaper},
    {next_state, StateName, StateData}.

handle_sync_event({send_xml, Packet}, _From, StateName,
		  #state{ws = {_, WsPid}, rfc_compilant = R} = StateData) ->
    Packet2 = case {case R of undefined -> true; V -> V end, Packet} of
                  {true, {xmlstreamstart, _, Attrs}} ->
                      Attrs2 = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-framing">>} |
                                lists:keydelete(<<"xmlns">>, 1, lists:keydelete(<<"xmlns:stream">>, 1, Attrs))],
                      {xmlstreamelement, #xmlel{name = <<"open">>, attrs = Attrs2}};
                  {true, {xmlstreamend, _}} ->
                      {xmlstreamelement, #xmlel{name = <<"close">>,
                                                attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-framing">>}]}};
                  {true, {xmlstreamraw, <<"\r\n\r\n">>}} -> % cdata ping
                      skip;
                  {true, {xmlstreamelement, #xmlel{name=Name2} = El2}} ->
                      El3 = case Name2 of
                                <<"stream:", _/binary>> ->
                                    fxml:replace_tag_attr(<<"xmlns:stream">>, ?NS_STREAM, El2);
                                _ ->
                                    case fxml:get_tag_attr_s(<<"xmlns">>, El2) of
                                        <<"">> ->
                                            fxml:replace_tag_attr(<<"xmlns">>, <<"jabber:client">>, El2);
                                        _ ->
                                            El2
                                    end
                            end,
                      {xmlstreamelement , El3};
                  _ ->
                      Packet
              end,
    case Packet2 of
        {xmlstreamstart, Name, Attrs3} ->
            B = fxml:element_to_binary(#xmlel{name = Name, attrs = Attrs3}),
            route_text(WsPid, <<(binary:part(B, 0, byte_size(B)-2))/binary, ">">>);
        {xmlstreamend, Name} ->
            route_text(WsPid, <<"</", Name/binary, ">">>);
        {xmlstreamelement, El} ->
            route_text(WsPid, fxml:element_to_binary(El));
        {xmlstreamraw, Bin} ->
            route_text(WsPid, Bin);
        {xmlstreamcdata, Bin2} ->
            route_text(WsPid, Bin2);
        skip ->
            ok
    end,
    SN2 = case Packet2 of
              {xmlstreamelement, #xmlel{name = <<"close">>}} ->
                  stream_end_sent;
              _ ->
                  StateName
          end,
    {reply, ok, SN2, StateData};
handle_sync_event(close, _From, StateName, #state{ws = {_, WsPid}, rfc_compilant = true} = StateData)
  when StateName /= stream_end_sent ->
    Close = #xmlel{name = <<"close">>,
                   attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-framing">>}]},
    route_text(WsPid, fxml:element_to_binary(Close)),
    {stop, normal, StateData};
handle_sync_event(close, _From, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_info(closed, _StateName, StateData) ->
    {stop, normal, StateData};
handle_info({received, Packet}, StateName, StateDataI) ->
    {StateData, Parsed} = parse(StateDataI, Packet),
    SD = case StateData#state.active of
             false ->
                 Input = StateData#state.input ++ if is_binary(Parsed) -> [Parsed]; true -> Parsed end,
                 StateData#state{input = Input};
             true ->
                 StateData#state.c2s_pid ! {tcp, StateData#state.socket, Parsed},
                 setup_timers(StateData#state{active = false})
         end,
    {next_state, StateName, SD};
handle_info(PingPong, StateName, StateData) when PingPong == ping orelse
                                                 PingPong == pong ->
    StateData2 = setup_timers(StateData),
    {next_state, StateName,
     StateData2#state{pong_expected = false}};
handle_info({timeout, Timer, _}, _StateName,
	    #state{timer = Timer} = StateData) ->
    ?DEBUG("Closing websocket connection from hitting inactivity timeout", []),
    {stop, normal, StateData};
handle_info({timeout, Timer, _}, StateName,
	    #state{ping_timer = Timer, ws = {_, WsPid}} = StateData) ->
    case StateData#state.pong_expected of
        false ->
            misc:cancel_timer(StateData#state.ping_timer),
            PingTimer = erlang:start_timer(StateData#state.ping_interval,
                                           self(), []),
            WsPid ! {ping, <<>>},
            {next_state, StateName,
             StateData#state{ping_timer = PingTimer, pong_expected = true}};
        true ->
	    ?DEBUG("Closing websocket connection from missing pongs", []),
            {stop, normal, StateData}
    end;
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, StateData) ->
    StateData#state.c2s_pid ! {tcp_closed, StateData#state.socket}.

setup_timers(StateData) ->
    misc:cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(StateData#state.timeout,
                               self(), []),
    misc:cancel_timer(StateData#state.ping_timer),
    PingTimer = case StateData#state.ping_interval of
                    0 -> StateData#state.ping_timer;
                    V -> erlang:start_timer(V, self(), [])
                end,
     StateData#state{timer = Timer, ping_timer = PingTimer,
                     pong_expected = false}.

get_human_html_xmlel() ->
    Heading = <<"ejabberd ", (misc:atom_to_binary(?MODULE))/binary>>,
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
                                           "use WebSocket connection you need a Jabber/XMPP "
                                           "client that supports it.">>}]}]}]}.


parse(#state{rfc_compilant = C} = State, Data) ->
    case C of
        undefined ->
            P = fxml_stream:new(self()),
            P2 = fxml_stream:parse(P, Data),
            fxml_stream:close(P2),
            case parsed_items([]) of
                error ->
                    {State#state{rfc_compilant = true}, <<"parse error">>};
                [] ->
                    {State#state{rfc_compilant = true}, <<"parse error">>};
                [{xmlstreamstart, <<"open">>, _} | _] ->
                    parse(State#state{rfc_compilant = true}, Data);
                _ ->
                    parse(State#state{rfc_compilant = false}, Data)
            end;
        true ->
            El = fxml_stream:parse_element(Data),
            case El of
                #xmlel{name = <<"open">>, attrs = Attrs} ->
                    Attrs2 = [{<<"xmlns:stream">>, ?NS_STREAM}, {<<"xmlns">>, <<"jabber:client">>} |
                              lists:keydelete(<<"xmlns">>, 1, lists:keydelete(<<"xmlns:stream">>, 1, Attrs))],
                    {State, [{xmlstreamstart, <<"stream:stream">>, Attrs2}]};
                #xmlel{name = <<"close">>} ->
                    {State, [{xmlstreamend, <<"stream:stream">>}]};
                {error, _} ->
                    {State, <<"parse error">>};
                _ ->
                    {State, [El]}
            end;
        false ->
            {State, Data}
    end.

parsed_items(List) ->
    receive
        {'$gen_event', El}
          when element(1, El) == xmlel;
               element(1, El) == xmlstreamstart;
               element(1, El) == xmlstreamelement;
               element(1, El) == xmlstreamcdata;
               element(1, El) == xmlstreamend ->
            parsed_items([El | List]);
        {'$gen_event', {xmlstreamerror, _}} ->
            error
    after 0 ->
            lists:reverse(List)
    end.

-spec route_text(pid(), binary()) -> ok.
route_text(Pid, Data) ->
    Pid ! {text, Data},
    ok.
