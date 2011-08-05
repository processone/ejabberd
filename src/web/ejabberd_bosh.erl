%%%-------------------------------------------------------------------
%%% File    : ejabberd_bosh.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Manage BOSH sockets
%%% Created : 20 Jul 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------
-module(ejabberd_bosh).

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% API
-export([start/2, start_link/2]).
-export([send_xml/2, setopts/2, controlling_process/2,
         custom_receiver/1, become_controller/2, reset_stream/1,
         change_shaper/2, monitor/1, close/1, sockname/1,
         peername/1, process_request/2, send/2]).

%% gen_fsm callbacks
-export([init/1, wait_for_session/2, wait_for_session/3,
         active/2, active/3, handle_event/3, print_state/1,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("bosh.hrl").

%%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(BOSH_VERSION, "1.10").
-define(NS_CLIENT, "jabber:client").
-define(NS_BOSH, "urn:xmpp:xbosh").
-define(NS_HTTP_BIND, "http://jabber.org/protocol/httpbind").

-define(DEFAULT_MAXPAUSE, 120). %% secs
-define(DEFAULT_WAIT, 300). %% secs
-define(DEFAULT_HOLD, 1). %% num
-define(DEFAULT_POLLING, 2). %% secs
-define(DEFAULT_INACTIVITY, 30). %% secs

-define(MAX_SHAPED_REQUESTS_QUEUE_LEN, 1000).

-record(state, {host,
                socket,
                el_ibuf,
                el_obuf,
                shaper_state,
                c2s_pid,
                xmpp_ver,
                inactivity_timer,
                wait_timer,
                wait_timeout = ?DEFAULT_WAIT,
                inactivity_timeout,
                prev_rid,
                prev_key,
                prev_poll,
                responses = gb_trees:empty(),
                receivers = gb_trees:empty(),
                shaped_receivers = queue:new(),
                max_requests,
                ip}).

-record(body, {http_reason = "", %% Using HTTP reason phrase is
                                 %% a hack, but we need a clue why
                                 %% a connection gets terminated:
                                 %% 'condition' attribute is not enough
               attrs = [],
               els = [],
               size = 0}).

%%%===================================================================
%%% API
%%%===================================================================
%% TODO: If compile with no supervisor option, start the session without
%%       supervisor
start(#body{attrs = Attrs} = Body, IP) ->
    XMPPDomain = get_attr('to', Attrs),
    SupervisorProc = gen_mod:get_module_proc(XMPPDomain, ?PROCNAME),
    case catch supervisor:start_child(SupervisorProc, [Body, IP]) of
    	{ok, Pid} ->
            {ok, Pid};
        {'EXIT', {noproc, _}} ->
            check_bosh_module(XMPPDomain),
            {error, module_not_loaded};
        Err ->
            ?ERROR_MSG("Failed to start BOSH session: ~p", [Err]),
            {error, Err}
    end.

start_link(Body, IP) ->
    ?GEN_FSM:start_link(?MODULE, [Body, IP], ?FSMOPTS).

send({http_bind, FsmRef, _IP}, Packet) ->
    ?GEN_FSM:sync_send_all_state_event(FsmRef, {send, Packet}).

send_xml({http_bind, FsmRef, _IP}, Packet) ->
    ?GEN_FSM:sync_send_all_state_event(FsmRef, {send_xml, Packet}).

setopts({http_bind, FsmRef, _IP}, Opts) ->
    case lists:member({active, once}, Opts) of
	true ->
	    ?GEN_FSM:send_all_state_event(FsmRef, {activate, self()});
	_ ->
	    ok
    end.

controlling_process(_Socket, _Pid) ->
    ok.

custom_receiver({http_bind, FsmRef, _IP}) ->
    {receiver, ?MODULE, FsmRef}.

become_controller(FsmRef, C2SPid) ->
    ?GEN_FSM:send_all_state_event(FsmRef, {become_controller, C2SPid}).

reset_stream({http_bind, _FsmRef, _IP}) ->
    ok.

change_shaper({http_bind, FsmRef, _IP}, Shaper) ->
    ?GEN_FSM:send_all_state_event(FsmRef, {change_shaper, Shaper}).

monitor({http_bind, FsmRef, _IP}) ->
    erlang:monitor(process, FsmRef).

close({http_bind, FsmRef, _IP}) ->
    catch ?GEN_FSM:sync_send_all_state_event(FsmRef, close).

sockname(_Socket) ->
    {ok, {{0,0,0,0}, 0}}.

peername({http_bind, _FsmRef, IP}) ->
    {ok, IP}.

process_request(Data, IP) ->
    Opts1 = ejabberd_c2s_config:get_c2s_limits(),
    Opts = [{xml_socket, true} | Opts1],
    MaxStanzaSize =
	case lists:keysearch(max_stanza_size, 1, Opts) of
	    {value, {_, Size}} -> Size;
	    _ -> infinity
	end,
    PayloadSize = iolist_size(Data),
    if PayloadSize > MaxStanzaSize ->
            http_error(403, "Request Too Large");
       true ->
            case decode_body(Data, PayloadSize) of
                {ok, #body{attrs = Attrs} = Body} ->
                    SID = get_attr('sid', Attrs),
                    To = get_attr('to', Attrs),
                    if SID == "", To == "" ->
                            %% Initial request which lacks "to" attribute
                            bosh_response(
                              #body{http_reason = "Missing 'to' attribute",
                                    attrs = [{type, "terminate"},
                                             {condition, "improper-addressing"}]});
                       SID == "" ->
                            %% Initial request
                            case start(Body, IP) of
                                {ok, Pid} ->
                                    process_request(Pid, Body, IP);
                                _Err ->
                                    bosh_response(
                                      #body{http_reason =
                                                "Failed to start BOSH session",
                                            attrs = [{type, "terminate"},
                                                     {condition,
                                                      "internal-server-error"}]})
                            end;
                       true ->
                            case find_session(SID) of
                                {ok, Pid} ->
                                    process_request(Pid, Body, IP);
                                error ->
                                    bosh_response(
                                      #body{http_reason = "Session ID mismatch",
                                            attrs = [{type, "terminate"},
                                                     {condition,
                                                      "item-not-found"}]})
                            end
                    end;
                {error, Reason} ->
                    http_error(400, Reason)
            end
    end.

process_request(Pid, Req, _IP) ->
    case catch ?GEN_FSM:sync_send_event(Pid, Req, infinity) of
        #body{} = Resp ->
            bosh_response(Resp);
        {'EXIT', {Reason, _}} when Reason == noproc; Reason == normal ->
            bosh_response(#body{http_reason = "BOSH session not found",
                                attrs = [{type, "terminate"},
                                         {condition,
                                          "item-not-found"}]});
        {'EXIT', _} ->
            bosh_response(#body{http_reason = "Unexpected error",
                                attrs = [{type, "terminate"},
                                         {condition, "internal-server-error"}]})
    end.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([#body{attrs = Attrs}, IP]) ->
    %% Read c2s options from the first ejabberd_c2s configuration in
    %% the config file listen section
    %% TODO: We should have different access and shaper values for
    %% each connector. The default behaviour should be however to use
    %% the default c2s restrictions if not defined for the current
    %% connector.
    Opts1 = ejabberd_c2s_config:get_c2s_limits(),
    Opts2 = [{xml_socket, true} | Opts1],
    Shaper = none,
    ShaperState = shaper:new(Shaper),
    Socket = {http_bind, self(), IP},
    XMPPVer = get_attr('xmpp:version', Attrs),
    XMPPDomain = get_attr('to', Attrs),
    {InBuf, Opts} = case gen_mod:get_module_opt(XMPPDomain, mod_bosh,
                                                prebind, false) of
                        true ->
                            JID = make_random_jid(XMPPDomain),
                            {buf_new(), [{jid, JID} | Opts2]};
                        false ->
                            {buf_in([make_xmlstreamstart(XMPPDomain, XMPPVer)],
                                    buf_new()),
                             Opts2}
                    end,
    ejabberd_socket:start(ejabberd_c2s, ?MODULE, Socket, Opts),
    Inactivity = gen_mod:get_module_opt(XMPPDomain, mod_bosh,
                                        max_inactivity, ?DEFAULT_INACTIVITY),
    State = #state{host = XMPPDomain,
                   xmpp_ver = XMPPVer,
                   socket = Socket,
                   el_ibuf = InBuf,
                   el_obuf = buf_new(),
                   inactivity_timeout = Inactivity,
                   shaper_state = ShaperState},
    NewState = restart_inactivity_timer(State),
    {ok, wait_for_session, NewState}.

wait_for_session(_Event, State) ->
    ?ERROR_MSG("unexpected event in 'wait_for_session': ~p", [_Event]),
    {next_state, wait_for_session, State}.

wait_for_session(#body{attrs = Attrs} = Req, From, State) ->
    RID = get_attr('rid', Attrs),
    ?DEBUG("got request:~n"
           "** RequestID: ~p~n"
           "** Request: ~p~n"
           "** From: ~p~n"
           "** State: ~p",
           [RID, Req, From, State]),
    Wait = min(get_attr('wait', Attrs, undefined), ?DEFAULT_WAIT),
    Hold = min(get_attr('hold', Attrs, undefined), ?DEFAULT_HOLD),
    NewKey = get_attr('newkey', Attrs),
    Type = get_attr('type', Attrs),
    Requests = Hold + 1,
    {PollTime, Polling} = if Wait == 0, Hold == 0 ->
                                  {now(), [{polling, ?DEFAULT_POLLING}]};
                             true ->
                                  {undefined, []}
                          end,
    MaxPause = gen_mod:get_module_opt(State#state.host, mod_bosh,
                                      max_pause, ?DEFAULT_MAXPAUSE),
    Resp = #body{attrs = [{sid, make_sid(self())},
                          {wait, Wait},
                          {ver, ?BOSH_VERSION},
                          {polling, ?DEFAULT_POLLING},
                          {inactivity, State#state.inactivity_timeout},
                          {hold, Hold},
                          {'xmpp:restartlogic', true},
                          {requests, Requests},
                          {secure, true},
                          {maxpause, MaxPause},
                          {'xmlns:xmpp', ?NS_BOSH},
                          {'xmlns:stream', ?NS_STREAM},
                          {from, State#state.host}|Polling]},
    {ShaperState, _} = shaper:update(State#state.shaper_state, Req#body.size),
    State1 = State#state{wait_timeout = Wait,
                         prev_rid = RID,
                         prev_key = NewKey,
                         prev_poll = PollTime,
                         shaper_state = ShaperState,
                         max_requests = Requests},
    Els = maybe_add_xmlstreamend(Req#body.els, Type),
    State2 = route_els(State1, Els),
    {State3, RespEls} = get_response_els(State2),
    State4 = stop_inactivity_timer(State3),
    case RespEls of
        [] ->
            State5 = restart_wait_timer(State4),
            Receivers = gb_trees:insert(RID, {From, Resp},
                                        State5#state.receivers),
            {next_state, active, State5#state{receivers = Receivers}};
        _ ->
            reply_next_state(State4, Resp#body{els = RespEls}, RID, From)
    end;
wait_for_session(_Event, _From, State) ->
    ?ERROR_MSG("unexpected sync event in 'wait_for_session': ~p", [_Event]),
    {reply, {error, badarg}, wait_for_session, State}.

active({#body{} = Body, From}, State) ->
    active1(Body, From, State);
active(_Event, State) ->
    ?ERROR_MSG("unexpected event in 'active': ~p", [_Event]),
    {next_state, active, State}.

active(#body{attrs = Attrs, size = Size} = Req, From, State) ->
    ?DEBUG("got request:~n"
           "** Request: ~p~n"
           "** From: ~p~n"
           "** State: ~p",
           [Req, From, State]),
    {ShaperState, Pause} = shaper:update(State#state.shaper_state, Size),
    State1 = State#state{shaper_state = ShaperState},
    if Pause > 0 ->
            QLen = queue:len(State1#state.shaped_receivers),
            if QLen < ?MAX_SHAPED_REQUESTS_QUEUE_LEN ->
                    TRef = start_shaper_timer(Pause),
                    Q = queue:in({TRef, From, Req}, State1#state.shaped_receivers),
                    State2 = stop_inactivity_timer(State1),
                    {next_state, active, State2#state{shaped_receivers = Q}};
               true ->
                    RID = get_attr('rid', Attrs),
                    reply_stop(State1,
                               #body{http_reason = "Too many requests",
                                     attrs = [{"type", "terminate"},
                                              {"condition", "policy-violation"}]},
                               From, RID)
            end;
       true ->
            active1(Req, From, State1)
    end;
active(_Event, _From, State) ->
    ?ERROR_MSG("unexpected sync event in 'active': ~p", [_Event]),
    {reply, {error, badarg}, active, State}.

active1(#body{attrs = Attrs} = Req, From, State) ->
    RID = get_attr('rid', Attrs),
    Key = get_attr('key', Attrs),
    IsValidKey = is_valid_key(State#state.prev_key, Key),
    IsOveractivity = is_overactivity(State#state.prev_poll),
    Type = get_attr('type', Attrs),
    if RID > State#state.prev_rid + State#state.max_requests ->
            reply_stop(State,
                       #body{http_reason = "Request ID is out of range",
                             attrs = [{"type", "terminate"},
                                      {"condition", "item-not-found"}]},
                       From, RID);
       RID > State#state.prev_rid + 1 ->
            State1 = restart_inactivity_timer(State),
            %% TODO: gb_trees:insert/3 may raise an exception
            Receivers = gb_trees:insert(RID, {From, Req},
                                        State1#state.receivers),
            {next_state, active, State1#state{receivers = Receivers}};
       RID =< State#state.prev_rid ->
            %% TODO: do we need to check 'key' here? It seems so...
            case gb_trees:lookup(RID, State#state.responses) of
                {value, PrevBody} ->
                    {next_state, active, do_reply(State, From, PrevBody, RID)};
                none ->
                    reply_stop(State, 
                               #body{http_reason = "Request ID is out of range",
                                     attrs = [{"type", "terminate"},
                                              {"condition", "item-not-found"}]},
                               From, RID)
            end;
       not IsValidKey ->
            reply_stop(State,
                       #body{http_reason = "Session key mismatch",
                             attrs = [{"type", "terminate"},
                                      {"condition", "item-not-found"}]},
                       From, RID);
       IsOveractivity ->
            reply_stop(State,
                       #body{http_reason = "Too many requests",
                             attrs = [{"type", "terminate"},
                                      {"condition", "policy-violation"}]},
                       From, RID);
       true ->
            State1 = stop_inactivity_timer(State),
            State2 = stop_wait_timer(State1),
            Els = case get_attr('xmpp:restart', Attrs, false) of
                      true ->
                          XMPPDomain = get_attr('to', Attrs,
                                                State#state.host),
                          XMPPVer = get_attr('xmpp:version', Attrs,
                                             State#state.xmpp_ver),
                          [make_xmlstreamstart(XMPPDomain, XMPPVer)];
                      false ->
                          Req#body.els
                  end,
            State3 = route_els(State2, maybe_add_xmlstreamend(Els, Type)),
            {State4, RespEls} = get_response_els(State3),
            NewKey = get_attr('newkey', Attrs, Key),
            Pause = get_attr('pause', Attrs, undefined),
            NewPoll = case State#state.prev_poll of
                          undefined -> undefined;
                          _ -> now()
                      end,
            State5 = State4#state{prev_poll = NewPoll,
                                  prev_key = NewKey},
            if Type == "terminate" ->
                    reply_stop(State5, #body{http_reason = "Session close",
                                             attrs = [{"type", "terminate"}],
                                             els = RespEls}, From, RID);
               Pause /= undefined ->
                    State6 = drop_holding_receiver(State5),
                    State7 = restart_inactivity_timer(State6, Pause),
                    InBuf = buf_in(RespEls, State7#state.el_ibuf),
                    {next_state, active,
                     State7#state{prev_rid = RID,
                                  el_ibuf = InBuf}};
               RespEls == [] ->
                    State6 = drop_holding_receiver(State5),
                    State7 = restart_wait_timer(State6),
                    %% TODO: gb_trees:insert/3 may raise an exception
                    Receivers = gb_trees:insert(RID, {From, #body{}},
                                                State7#state.receivers),
                    {next_state, active, State7#state{prev_rid = RID,
                                                      receivers = Receivers}};
               true ->
                    State6 = drop_holding_receiver(State5),
                    reply_next_state(State6#state{prev_rid = RID},
                                     #body{els = RespEls}, RID, From)
            end
    end.

handle_event({become_controller, C2SPid}, StateName, State) ->
    State1 = route_els(State#state{c2s_pid = C2SPid}),
    {next_state, StateName, State1};
handle_event({change_shaper, Shaper}, StateName, State) ->
    NewShaperState = shaper:new(Shaper),
    {next_state, StateName, State#state{shaper_state = NewShaperState}};
handle_event(_Event, StateName, State) ->
    ?ERROR_MSG("unexpected event in '~s': ~p", [StateName, _Event]),
    {next_state, StateName, State}.

handle_sync_event({send_xml, {xmlstreamstart, _, _} = El}, _From,
                  StateName, State) when State#state.xmpp_ver >= "1.0" ->
    %% Avoid sending empty <body/> element
    OutBuf = buf_in([El], State#state.el_obuf),
    {reply, ok, StateName, State#state{el_obuf = OutBuf}};
handle_sync_event({send_xml, El}, _From, StateName, State) ->
    case gb_trees:lookup(State#state.prev_rid, State#state.receivers) of
        {value, {From, Body}} ->
            OutBuf = buf_to_list(buf_in([El], State#state.el_obuf)),
            State1 = State#state{el_obuf = buf_new()},
            {reply, ok, StateName, reply(State1, Body#body{els = OutBuf},
                                         State1#state.prev_rid, From)};
        none ->
            State1 = case queue:out(State#state.shaped_receivers) of
                         {{value, {TRef, From, Body}}, Q} ->
                             cancel_timer(TRef),
                             ?GEN_FSM:send_event(self(), {Body, From}),
                             State#state{shaped_receivers = Q};
                         _ ->
                             State
                     end,
            OutBuf = buf_in([El], State1#state.el_obuf),
            {reply, ok, StateName, State1#state{el_obuf = OutBuf}}
    end;
handle_sync_event(peername, _From, StateName, State) ->
    {reply, {ok, State#state.ip}, StateName, State};
handle_sync_event(close, _From, _StateName, State) ->
    {stop, normal, State};
handle_sync_event(_Event, _From, StateName, State) ->
    ?ERROR_MSG("unexpected sync event in '~s': ~p", [StateName, _Event]),
    {reply, {error, badarg}, StateName, State}.

handle_info({timeout, TRef, wait_timeout}, StateName,
            #state{wait_timer = TRef} = State) ->
    {next_state, StateName, drop_holding_receiver(State)};
handle_info({timeout, TRef, inactive}, _StateName,
            #state{inactivity_timer = TRef} = State) ->
    {stop, normal, State};
handle_info({timeout, TRef, shaper_timeout}, StateName, State) ->
    case queue:out(State#state.shaped_receivers) of
        {{value, {TRef, From, Req}}, Q} ->
            ?GEN_FSM:send_event(self(), {Req, From}),
            {next_state, StateName, State#state{shaped_receivers = Q}};
        {{value, _}, _} ->
            ?ERROR_MSG("shaper_timeout mismatch:~n"
                       "** TRef: ~p~n"
                       "** State: ~p",
                       [TRef, State]),
            {stop, normal, State};
        _ ->
            {next_state, StateName, State}
    end;
handle_info(_Info, StateName, State) ->
    ?ERROR_MSG("unexpected info:~n"
               "** Msg: ~p~n"
               "** StateName: ~p",
               [_Info, StateName]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    case State#state.c2s_pid of
        C2SPid when is_pid(C2SPid) ->
            ?GEN_FSM:send_event(C2SPid, closed);
        _ ->
            ok
    end,
    bounce_receivers(State),
    bounce_els_from_obuf(State).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

print_state(State) ->
    State.

%%%===================================================================
%%% Internal functions
%%%===================================================================
route_els(#state{el_ibuf = Buf} = State) ->
    route_els(State#state{el_ibuf = buf_new()}, buf_to_list(Buf)).

route_els(State, Els) ->
    case State#state.c2s_pid of
        C2SPid when is_pid(C2SPid) ->
            lists:foreach(
              fun(El) ->
                      ?GEN_FSM:send_event(C2SPid, El)
              end, Els),
            State;
        _ ->
            InBuf = buf_in(Els, State#state.el_ibuf),
            State#state{el_ibuf = InBuf}
    end.

get_response_els(#state{el_obuf = OutBuf} = State) ->
    {State#state{el_obuf = buf_new()}, buf_to_list(OutBuf)}.

reply(State, Body, RID, From) ->
    State1 = restart_inactivity_timer(State),
    Receivers = gb_trees:delete_any(RID, State1#state.receivers),
    State2 = do_reply(State1, From, Body, RID),
    case catch gb_trees:take_smallest(Receivers) of
        {NextRID, {From1, Req}, Receivers1} when NextRID == RID + 1 ->
            ?GEN_FSM:send_event(self(), {Req, From1}),
            State2#state{receivers = Receivers1};
        _ ->
            State2#state{receivers = Receivers}
    end.

reply_next_state(State, Body, RID, From) ->
    State1 = restart_inactivity_timer(State),
    Receivers = gb_trees:delete_any(RID, State1#state.receivers),
    State2 = do_reply(State1, From, Body, RID),
    case catch gb_trees:take_smallest(Receivers) of
        {NextRID, {From1, Req}, Receivers1} when NextRID == RID + 1 ->
            active(Req, From1, State2#state{receivers = Receivers1});
        _ ->
            {next_state, active, State2#state{receivers = Receivers}}
    end.

reply_stop(State, Body, From, RID) ->
    {stop, normal, do_reply(State, From, Body, RID)}.

drop_holding_receiver(State) ->
    RID = State#state.prev_rid,
    case gb_trees:lookup(RID, State#state.receivers) of
        {value, {From, Body}} ->
            State1 = restart_inactivity_timer(State),
            Receivers = gb_trees:delete_any(RID, State1#state.receivers),
            State2 = State1#state{receivers = Receivers},
            do_reply(State2, From, Body, RID);
        none ->
            State
    end.

do_reply(State, From, Body, RID) ->
    ?DEBUG("send reply:~n"
           "** RequestID: ~p~n"
           "** Reply: ~p~n"
           "** To: ~p~n"
           "** State: ~p",
           [RID, Body, From, State]),
    ?GEN_FSM:reply(From, Body),
    Responses = gb_trees:delete_any(RID, State#state.responses),
    Responses1 = case gb_trees:size(Responses) of
                     N when N < State#state.max_requests; N == 0 ->
                         Responses;
                     _ ->
                         element(3, gb_trees:take_smallest(Responses))
                 end,
    Responses2 = gb_trees:insert(RID, Body, Responses1),
    State#state{responses = Responses2}.

bounce_receivers(State) ->
    Receivers = gb_trees:to_list(State#state.receivers),
    ShapedReceivers = lists:map(
                        fun({_, From, #body{attrs = Attrs} = Body}) ->
                                RID = get_attr('rid', Attrs),
                                {RID, {From, Body}}
                        end, queue:to_list(State#state.shaped_receivers)),
    lists:foreach(
      fun({RID, {From, _Body}}) ->
              do_reply(State, From,
                       #body{http_reason = "Session close",
                             attrs = [{type, "terminate"},
                                      {condition, "other-request"}]},
                       RID)
      end, Receivers ++ ShapedReceivers).

bounce_els_from_obuf(State) ->
    lists:foreach(
      fun({xmlstreamelement, El}) ->
              case El of
                  {xmlelement, Name, Attrs, _}
                    when Name == "presence";
                         Name == "message";
                         Name == "iq" ->
                      FromS = xml:get_attr_s("from", Attrs),
                      ToS = xml:get_attr_s("to", Attrs),
                      case {jlib:string_to_jid(FromS),
                            jlib:string_to_jid(ToS)} of
                          {#jid{} = From, #jid{} = To} ->
                              ejabberd_router:route(From, To, El);
                          _ ->
                              ok
                      end;
                  _ ->
                      ok
              end;
         (_) ->
              ok
      end, State#state.el_obuf).

is_valid_key("", "") ->
    true;
is_valid_key([_|_] = PrevKey, [_|_] = Key) ->
    sha:sha(Key) == PrevKey;
is_valid_key(_, _) ->
    false.

is_overactivity(undefined) ->
    false;
is_overactivity(PrevPoll) ->
    PollPeriod = timer:now_diff(now(), PrevPoll) div 1000000,
    if PollPeriod < ?DEFAULT_POLLING ->
            true;
       true ->
            false
    end.

make_xmlstreamstart(XMPPDomain, Version) ->
    VersionEl = case Version of
                    "" -> [];
                    _ -> [{"version", Version}]
                end,
    {xmlstreamstart, "stream:stream",
     [{"to", XMPPDomain},
      {"xmlns", ?NS_CLIENT},
      {"xmlns:stream", ?NS_STREAM}|VersionEl]}.

maybe_add_xmlstreamend(Els, "terminate") ->
    Els ++ [{xmlstreamend, "stream:stream"}];
maybe_add_xmlstreamend(Els, _) ->
    Els.

encode_body(#body{attrs = Attrs, els = Els}) ->
    Attrs1 = lists:map(
               fun({K, V}) when is_atom(K) ->
                       AmK = atom_to_list(K),
                       case V of
                           true -> {AmK, "true"};
                           false -> {AmK, "false"};
                           [_|_] -> {AmK, V};
                           I when is_integer(I), I >= 0 ->
                               {AmK, integer_to_list(I)}
                       end;
                  ({K, V}) ->
                       {K, V}
               end, Attrs),
    Attrs2 = [{"xmlns", ?NS_HTTP_BIND}|Attrs1],
    {Attrs3, XMLs} =
        lists:foldr(
          fun({xmlstreamraw, XML}, {AttrsAcc, XMLBuf}) ->
                  {AttrsAcc, [XML|XMLBuf]};
             ({xmlstreamelement, {xmlelement, "stream:error", _, _} = El},
              {AttrsAcc, XMLBuf}) ->
                  {[{"type", "terminate"},
                    {"condition", "remote-stream-error"},
                    {"xmlns:stream", ?NS_STREAM}|AttrsAcc],
                   [xml:element_to_binary(El)|XMLBuf]};
             ({xmlstreamelement, El}, {AttrsAcc, XMLBuf}) ->
                  {AttrsAcc, [xml:element_to_binary(El)|XMLBuf]};
             ({xmlstreamend, _}, {AttrsAcc, XMLBuf}) ->
                  {[{"type", "terminate"},
                    {"condition", "remote-stream-error"}|AttrsAcc], XMLBuf};
             ({xmlstreamstart, "stream:stream", SAttrs}, {AttrsAcc, XMLBuf}) ->
                  StreamID = xml:get_attr_s("id", SAttrs),
                  NewAttrs = case xml:get_attr_s("version", SAttrs) of
                                 "" ->
                                     [{"authid", StreamID}|AttrsAcc];
                                 V ->
                                     [{"xmpp:version", V},
                                      {"authid", StreamID} | AttrsAcc]
                             end,
                  {NewAttrs, XMLBuf};
             ({xmlstreamerror, _}, {AttrsAcc, XMLBuf}) ->
                  {[{"type", "terminate"},
                    {"condition", "remote-stream-error"}|AttrsAcc],
                   XMLBuf};
             (_, Acc) ->
                  Acc
          end, {Attrs2, []}, Els),
    case XMLs of
        [] ->
            ["<body", attrs_to_list(Attrs3), "/>"];
        _ ->
            ["<body", attrs_to_list(Attrs3), $>, XMLs, "</body>"]
    end.

decode_body(BodyXML, Size) ->
    case xml_stream:parse_element(BodyXML) of
        {xmlelement, "body", Attrs, Els} ->
            case attrs_to_body_attrs(Attrs) of
                {error, _} = Err ->
                    Err;
                BodyAttrs ->
                    case get_attr(rid, BodyAttrs) of
                        "" ->
                            {error, "Missing \"rid\" attribute"};
                        _ ->
                            Els1 = lists:flatmap(
                                     fun({xmlelement, _, _, _} = El) ->
                                             [{xmlstreamelement, El}];
                                        (_) ->
                                             []
                                     end, Els),
                            {ok, #body{attrs = BodyAttrs,
                                       size = Size,
                                       els = Els1}}
                    end
            end;
        {xmlelement, _, _, _} ->
            {error, "Unexpected payload"};
        _ ->
            {error, "XML is not well-formed"}
    end.

attrs_to_body_attrs(Attrs) ->
    lists:foldl(
      fun(_, {error, Reason}) ->
              {error, Reason};
         ({Attr, Val}, Acc) ->
              try
                  case Attr of
                      "ver" -> [{ver, Val}|Acc];
                      "xmpp:version" -> [{'xmpp:version', Val}|Acc];
                      "type" -> [{type, Val}|Acc];
                      "key" -> [{key, Val}|Acc];
                      "newkey" -> [{newkey, Val}|Acc];
                      "xmlns" -> Val = ?NS_HTTP_BIND, Acc;
                      "secure" -> [{secure, to_bool(Val)}|Acc];
                      "xmpp:restart" -> [{'xmpp:restart', to_bool(Val)}|Acc];
                      "to" -> [{to, [_|_] = jlib:nameprep(Val)}|Acc];
                      "wait" -> [{wait, to_int(Val, 0)}|Acc];
                      "ack" -> [{ack, to_int(Val, 0)}|Acc];
                      "sid" -> [{sid, Val}|Acc];
                      "hold" -> [{hold, to_int(Val, 0)}|Acc];
                      "rid" -> [{rid, to_int(Val, 0)}|Acc];
                      "pause" -> [{pause, to_int(Val, 0)}|Acc];
                      _ -> [{Attr, Val}|Acc]
                  end
              catch _:_ ->
                      {error, "Invalid \"" ++ Attr ++ "\" attribute"}
              end
      end, [], Attrs).

to_int(S, Min) ->
    case list_to_integer(S) of
        I when I >= Min ->
            I;
        _ ->
            erlang:error(badarg)
    end.

to_bool("true") -> true;
to_bool("1") -> true;
to_bool("false") -> false;
to_bool("0") -> false.

attrs_to_list(Attrs) ->
    [attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
    [$\s, Name, $=, $', xml:crypt(Value), $'].

bosh_response(Body) ->
    {200, Body#body.http_reason, ?HEADER, encode_body(Body)}.

http_error(Status, Reason) ->
    {Status, Reason, ?HEADER, ""}.

make_sid(Pid) ->
    Key = ejabberd_config:get_local_option(shared_key),
    base64:encode_to_string(crypto:rc4_encrypt(Key, term_to_binary(Pid))).

find_session(SID) ->
    Key = ejabberd_config:get_local_option(shared_key),
    try binary_to_term(crypto:rc4_encrypt(Key, base64:decode(SID))) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            error
    catch _:_ ->
            error
    end.

-compile({no_auto_import,[min/2]}).
min(A, undefined) -> A;
min(undefined, B) -> B;
min(A, B) -> erlang:min(A, B).

%% Check that mod_bosh has been defined in config file.
%% Print a warning in log file if this is not the case.
check_bosh_module(XmppDomain) ->
    case gen_mod:is_loaded(XmppDomain, mod_bosh) of
	true -> ok;
	false -> ?ERROR_MSG("You are trying to use BOSH (HTTP Bind) in host ~p,"
			    " but the module mod_bosh is not started in"
			    " that host. Configure your BOSH client to connect"
			    " to the correct host, or add your desired host to"
			    " the configuration, or check your 'modules'"
			    " section in your ejabberd configuration file.",
			    [XmppDomain])
    end.

get_attr(Attr, Attrs) ->
    get_attr(Attr, Attrs, "").

get_attr(Attr, Attrs, Default) ->
    case lists:keysearch(Attr, 1, Attrs) of
        {value, {_, Val}} ->
            Val;
        _ ->
            Default
    end.

buf_new() ->
    [].

buf_in(Xs, Buf) ->
    lists:foldl(
      fun(X, Acc) ->
              [X|Acc]
      end, Buf, Xs).

buf_to_list(Buf) ->
    lists:reverse(Buf).

cancel_timer(TRef) when is_reference(TRef) ->
    ?GEN_FSM:cancel_timer(TRef);
cancel_timer(_) ->
    false.

restart_timer(TRef, Timeout, Msg) ->
    cancel_timer(TRef),
    erlang:start_timer(timer:seconds(Timeout), self(), Msg).

restart_inactivity_timer(#state{inactivity_timeout = Timeout} = State) ->
    restart_inactivity_timer(State, Timeout).

restart_inactivity_timer(#state{inactivity_timer = TRef} = State, Timeout) ->
    NewTRef = restart_timer(TRef, Timeout, inactive),
    State#state{inactivity_timer = NewTRef}.

stop_inactivity_timer(#state{inactivity_timer = TRef} = State) ->
    cancel_timer(TRef),
    State#state{inactivity_timer = undefined}.

restart_wait_timer(#state{wait_timer = TRef,
                          wait_timeout = Timeout} = State) ->
    NewTRef = restart_timer(TRef, Timeout, wait_timeout),
    State#state{wait_timer = NewTRef}.

stop_wait_timer(#state{wait_timer = TRef} = State) ->
    cancel_timer(TRef),
    State#state{wait_timer = undefined}.

start_shaper_timer(Timeout) ->
    erlang:start_timer(Timeout, self(), shaper_timeout).

make_random_jid(Host) ->
    %% Copied from cyrsasl_anonymous.erl
    User = lists:concat([randoms:get_string() | tuple_to_list(now())]),
    jlib:make_jid(User, Host, randoms:get_string()).
