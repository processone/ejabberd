%%%-------------------------------------------------------------------
%%% File    : ejabberd_bosh.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Manage BOSH sockets
%%% Created : 20 Jul 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%%-------------------------------------------------------------------
-module(ejabberd_bosh).

-protocol({xep, 124, '1.11'}).
-protocol({xep, 206, '1.4'}).

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% API
-export([start/2, start/3, start_link/3]).

-export([send_xml/2, setopts/2, controlling_process/2,
	 migrate/3, custom_receiver/1, become_controller/2,
	 reset_stream/1, change_shaper/2, monitor/1, close/1,
	 sockname/1, peername/1, process_request/3, send/2,
	 change_controller/2]).

%% gen_fsm callbacks
-export([init/1, wait_for_session/2, wait_for_session/3,
	 active/2, active/3, handle_event/3, print_state/1,
	 handle_sync_event/4, handle_info/3, terminate/3,
	 code_change/4]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("ejabberd_http.hrl").

-include("bosh.hrl").

%%-define(DBGFSM, true).
-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

-define(BOSH_VERSION, <<"1.11">>).

-define(NS_BOSH, <<"urn:xmpp:xbosh">>).

-define(NS_HTTP_BIND,
	<<"http://jabber.org/protocol/httpbind">>).

-define(DEFAULT_MAXPAUSE, 120).

-define(DEFAULT_WAIT, 300).

-define(DEFAULT_HOLD, 1).

-define(DEFAULT_POLLING, 2).

-define(DEFAULT_INACTIVITY, 30).

-define(MAX_SHAPED_REQUESTS_QUEUE_LEN, 1000).

-define(SEND_TIMEOUT, 15000).

-type bosh_socket() :: {http_bind, pid(),
                        {inet:ip_address(),
                         inet:port_number()}}.

-export_type([bosh_socket/0]).

-record(state,
	{host = <<"">>                            :: binary(),
         sid = <<"">>                             :: binary(),
         el_ibuf                                  :: p1_queue:queue(),
         el_obuf                                  :: p1_queue:queue(),
         shaper_state = none                      :: shaper:shaper(),
         c2s_pid                                  :: pid() | undefined,
	 xmpp_ver = <<"">>                        :: binary(),
         inactivity_timer                         :: reference() | undefined,
         wait_timer                               :: reference() | undefined,
	 wait_timeout = ?DEFAULT_WAIT             :: timeout(),
         inactivity_timeout = ?DEFAULT_INACTIVITY :: timeout(),
	 prev_rid = 0                             :: non_neg_integer(),
         prev_key = <<"">>                        :: binary(),
         prev_poll                                :: erlang:timestamp() | undefined,
         max_concat = unlimited                   :: unlimited | non_neg_integer(),
	 responses = gb_trees:empty()             :: ?TGB_TREE,
	 receivers = gb_trees:empty()             :: ?TGB_TREE,
	 shaped_receivers                         :: p1_queue:queue(),
         ip                                       :: inet:ip_address(),
         max_requests = 1                         :: non_neg_integer()}).

-record(body,
	{http_reason = <<"">> :: binary(),
         attrs = []           :: [{any(), any()}],
         els = []             :: [fxml_stream:xml_stream_el()],
         size = 0             :: non_neg_integer()}).

start(#body{attrs = Attrs} = Body, IP, SID) ->
    XMPPDomain = get_attr(to, Attrs),
    SupervisorProc = gen_mod:get_module_proc(XMPPDomain, mod_bosh),
    case catch supervisor:start_child(SupervisorProc,
				      [Body, IP, SID])
	of
      {ok, Pid} -> {ok, Pid};
      {'EXIT', {noproc, _}} ->
	  check_bosh_module(XMPPDomain),
	  {error, module_not_loaded};
      Err ->
	  ?ERROR_MSG("Failed to start BOSH session: ~p", [Err]),
	  {error, Err}
    end.

start(StateName, State) ->
    (?GEN_FSM):start_link(?MODULE, [StateName, State],
			  ?FSMOPTS).

start_link(Body, IP, SID) ->
    (?GEN_FSM):start_link(?MODULE, [Body, IP, SID],
			  ?FSMOPTS).

send({http_bind, FsmRef, IP}, Packet) ->
    send_xml({http_bind, FsmRef, IP}, Packet).

send_xml({http_bind, FsmRef, _IP}, Packet) ->
    case catch (?GEN_FSM):sync_send_all_state_event(FsmRef,
						    {send_xml, Packet},
						    ?SEND_TIMEOUT)
	of
      {'EXIT', {timeout, _}} -> {error, timeout};
      {'EXIT', _} -> {error, einval};
      Res -> Res
    end.

setopts({http_bind, FsmRef, _IP}, Opts) ->
    case lists:member({active, once}, Opts) of
      true ->
	  (?GEN_FSM):send_all_state_event(FsmRef,
					  {activate, self()});
      _ ->
	  case lists:member({active, false}, Opts) of
	    true ->
		case catch (?GEN_FSM):sync_send_all_state_event(FsmRef,
								deactivate_socket)
		    of
		  {'EXIT', _} -> {error, einval};
		  Res -> Res
		end;
	    _ -> ok
	  end
    end.

controlling_process(_Socket, _Pid) -> ok.

custom_receiver({http_bind, FsmRef, _IP}) ->
    {receiver, ?MODULE, FsmRef}.

become_controller(FsmRef, C2SPid) ->
    (?GEN_FSM):send_all_state_event(FsmRef,
				    {become_controller, C2SPid}).

change_controller({http_bind, FsmRef, _IP}, C2SPid) ->
    become_controller(FsmRef, C2SPid).

reset_stream({http_bind, _FsmRef, _IP}) -> ok.

change_shaper({http_bind, FsmRef, _IP}, Shaper) ->
    (?GEN_FSM):send_all_state_event(FsmRef,
				    {change_shaper, Shaper}).

monitor({http_bind, FsmRef, _IP}) ->
    erlang:monitor(process, FsmRef).

close({http_bind, FsmRef, _IP}) ->
    catch (?GEN_FSM):sync_send_all_state_event(FsmRef,
					       close).

sockname(_Socket) -> {ok, {{0, 0, 0, 0}, 0}}.

peername({http_bind, _FsmRef, IP}) -> {ok, IP}.

migrate(FsmRef, Node, After) when node(FsmRef) == node() ->
    catch erlang:send_after(After, FsmRef, {migrate, Node});
migrate(_FsmRef, _Node, _After) ->
    ok.

process_request(Data, IP, Type) ->
    Opts1 = ejabberd_c2s_config:get_c2s_limits(),
    Opts = case Type of
               xml ->
                   [{xml_socket, true} | Opts1];
               json ->
                   Opts1
           end,
    MaxStanzaSize = case lists:keysearch(max_stanza_size, 1,
					 Opts)
			of
		      {value, {_, Size}} -> Size;
		      _ -> infinity
		    end,
    PayloadSize = iolist_size(Data),
    if PayloadSize > MaxStanzaSize ->
	   http_error(403, <<"Request Too Large">>, Type);
       true ->
	   case decode_body(Data, PayloadSize, Type) of
	     {ok, #body{attrs = Attrs} = Body} ->
		 SID = get_attr(sid, Attrs),
		 To = get_attr(to, Attrs),
		 if SID == <<"">>, To == <<"">> ->
			bosh_response_with_msg(#body{http_reason =
						<<"Missing 'to' attribute">>,
					    attrs =
						[{type, <<"terminate">>},
						 {condition,
						  <<"improper-addressing">>}]},
                                      Type, Body);
		    SID == <<"">> ->
			case start(Body, IP, make_sid()) of
			  {ok, Pid} -> process_request(Pid, Body, IP, Type);
			  _Err ->
			      bosh_response_with_msg(#body{http_reason =
						      <<"Failed to start BOSH session">>,
						  attrs =
						      [{type, <<"terminate">>},
						       {condition,
							<<"internal-server-error">>}]},
                                            Type, Body)
			end;
		    true ->
			case mod_bosh:find_session(SID) of
			  {ok, Pid} -> process_request(Pid, Body, IP, Type);
			  error ->
			      bosh_response_with_msg(#body{http_reason =
						      <<"Session ID mismatch">>,
						  attrs =
						      [{type, <<"terminate">>},
						       {condition,
							<<"item-not-found">>}]},
                                            Type, Body)
			end
		 end;
	     {error, Reason} -> http_error(400, Reason, Type)
	   end
    end.

process_request(Pid, Req, _IP, Type) ->
    case catch (?GEN_FSM):sync_send_event(Pid, Req,
					  infinity)
	of
      #body{} = Resp -> bosh_response(Resp, Type);
      {'EXIT', {Reason, _}}
	  when Reason == noproc; Reason == normal ->
	  bosh_response(#body{http_reason =
				  <<"BOSH session not found">>,
			      attrs =
				  [{type, <<"terminate">>},
				   {condition, <<"item-not-found">>}]},
                        Type);
      {'EXIT', _} ->
	  bosh_response(#body{http_reason =
				  <<"Unexpected error">>,
			      attrs =
				  [{type, <<"terminate">>},
				   {condition, <<"internal-server-error">>}]},
                        Type)
    end.

init([#body{attrs = Attrs}, IP, SID]) ->
    Opts1 = ejabberd_c2s_config:get_c2s_limits(),
    Opts2 = [{xml_socket, true} | Opts1],
    Shaper = none,
    ShaperState = shaper:new(Shaper),
    Socket = make_socket(self(), IP),
    XMPPVer = get_attr('xmpp:version', Attrs),
    XMPPDomain = get_attr(to, Attrs),
    {InBuf, Opts} = case gen_mod:get_module_opt(
                           XMPPDomain,
                           mod_bosh, prebind,
                           fun(B) when is_boolean(B) -> B end,
                           false) of
                        true ->
                            JID = make_random_jid(XMPPDomain),
                            {buf_new(XMPPDomain), [{jid, JID} | Opts2]};
                        false ->
                            {buf_in([make_xmlstreamstart(XMPPDomain, XMPPVer)],
                                    buf_new(XMPPDomain)),
                             Opts2}
		    end,
    ejabberd_socket:start(ejabberd_c2s, ?MODULE, Socket,
			  Opts),
    Inactivity = gen_mod:get_module_opt(XMPPDomain,
					mod_bosh, max_inactivity,
                                        fun(I) when is_integer(I), I>0 -> I end,
					?DEFAULT_INACTIVITY),
    MaxConcat = gen_mod:get_module_opt(XMPPDomain, mod_bosh, max_concat,
                                       fun(unlimited) -> unlimited;
                                          (N) when is_integer(N), N>0 -> N
                                       end, unlimited),
    ShapedReceivers = buf_new(XMPPDomain, ?MAX_SHAPED_REQUESTS_QUEUE_LEN),
    State = #state{host = XMPPDomain, sid = SID, ip = IP,
		   xmpp_ver = XMPPVer, el_ibuf = InBuf,
		   max_concat = MaxConcat, el_obuf = buf_new(XMPPDomain),
		   inactivity_timeout = Inactivity,
		   shaped_receivers = ShapedReceivers,
		   shaper_state = ShaperState},
    NewState = restart_inactivity_timer(State),
    mod_bosh:open_session(SID, self()),
    {ok, wait_for_session, NewState};
init([StateName, State]) ->
    mod_bosh:open_session(State#state.sid, self()),
    case State#state.c2s_pid of
      C2SPid when is_pid(C2SPid) ->
	  NewSocket = make_socket(self(), State#state.ip),
	  C2SPid ! {change_socket, NewSocket},
	  NewState = restart_inactivity_timer(State),
	  {ok, StateName, NewState};
      _ -> {stop, normal}
    end.

wait_for_session(_Event, State) ->
    ?ERROR_MSG("unexpected event in 'wait_for_session': ~p",
	       [_Event]),
    {next_state, wait_for_session, State}.

wait_for_session(#body{attrs = Attrs} = Req, From,
		 State) ->
    RID = get_attr(rid, Attrs),
    ?DEBUG("got request:~n** RequestID: ~p~n** Request: "
	   "~p~n** From: ~p~n** State: ~p",
	   [RID, Req, From, State]),
    Wait = min(get_attr(wait, Attrs, undefined),
	       ?DEFAULT_WAIT),
    Hold = min(get_attr(hold, Attrs, undefined),
	       ?DEFAULT_HOLD),
    NewKey = get_attr(newkey, Attrs),
    Type = get_attr(type, Attrs),
    Requests = Hold + 1,
    {PollTime, Polling} = if Wait == 0, Hold == 0 ->
				 {p1_time_compat:timestamp(), [{polling, ?DEFAULT_POLLING}]};
			     true -> {undefined, []}
			  end,
    MaxPause = gen_mod:get_module_opt(State#state.host,
				      mod_bosh, max_pause,
                                      fun(I) when is_integer(I), I>0 -> I end,
                                      ?DEFAULT_MAXPAUSE),
    Resp = #body{attrs =
		     [{sid, State#state.sid}, {wait, Wait},
		      {ver, ?BOSH_VERSION}, {polling, ?DEFAULT_POLLING},
		      {inactivity, State#state.inactivity_timeout},
		      {hold, Hold}, {'xmpp:restartlogic', true},
		      {requests, Requests}, {secure, true},
		      {maxpause, MaxPause}, {'xmlns:xmpp', ?NS_BOSH},
		      {'xmlns:stream', ?NS_STREAM}, {from, State#state.host}
		      | Polling]},
    {ShaperState, _} =
	shaper:update(State#state.shaper_state, Req#body.size),
    State1 = State#state{wait_timeout = Wait,
			 prev_rid = RID, prev_key = NewKey,
			 prev_poll = PollTime, shaper_state = ShaperState,
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
	  {next_state, active,
	   State5#state{receivers = Receivers}};
      _ ->
	  reply_next_state(State4, Resp#body{els = RespEls}, RID,
			   From)
    end;
wait_for_session(_Event, _From, State) ->
    ?ERROR_MSG("unexpected sync event in 'wait_for_session': ~p",
	       [_Event]),
    {reply, {error, badarg}, wait_for_session, State}.

active({#body{} = Body, From}, State) ->
    active1(Body, From, State);
active(_Event, State) ->
    ?ERROR_MSG("unexpected event in 'active': ~p",
	       [_Event]),
    {next_state, active, State}.

active(#body{attrs = Attrs, size = Size} = Req, From,
       State) ->
    ?DEBUG("got request:~n** Request: ~p~n** From: "
	   "~p~n** State: ~p",
	   [Req, From, State]),
    {ShaperState, Pause} =
	shaper:update(State#state.shaper_state, Size),
    State1 = State#state{shaper_state = ShaperState},
    if Pause > 0 ->
	    TRef = start_shaper_timer(Pause),
	    try p1_queue:in({TRef, From, Req},
			    State1#state.shaped_receivers) of
		Q ->
		    State2 = stop_inactivity_timer(State1),
		    {next_state, active,
		     State2#state{shaped_receivers = Q}}
	    catch error:full ->
		  cancel_timer(TRef),
		  RID = get_attr(rid, Attrs),
		  reply_stop(State1,
			     #body{http_reason = <<"Too many requests">>,
				   attrs =
				       [{<<"type">>, <<"terminate">>},
					{<<"condition">>,
					 <<"policy-violation">>}]},
			     From, RID)
	   end;
       true -> active1(Req, From, State1)
    end;
active(_Event, _From, State) ->
    ?ERROR_MSG("unexpected sync event in 'active': ~p",
	       [_Event]),
    {reply, {error, badarg}, active, State}.

active1(#body{attrs = Attrs} = Req, From, State) ->
    RID = get_attr(rid, Attrs),
    Key = get_attr(key, Attrs),
    IsValidKey = is_valid_key(State#state.prev_key, Key),
    IsOveractivity = is_overactivity(State#state.prev_poll),
    Type = get_attr(type, Attrs),
    if RID >
	 State#state.prev_rid + State#state.max_requests ->
	   reply_stop(State,
		      #body{http_reason = <<"Request ID is out of range">>,
			    attrs =
				[{<<"type">>, <<"terminate">>},
				 {<<"condition">>, <<"item-not-found">>}]},
		      From, RID);
       RID > State#state.prev_rid + 1 ->
	   State1 = restart_inactivity_timer(State),
	   Receivers = gb_trees:insert(RID, {From, Req},
				       State1#state.receivers),
	   {next_state, active,
	    State1#state{receivers = Receivers}};
       RID =< State#state.prev_rid ->
            %% TODO: do we need to check 'key' here? It seems so...
            case gb_trees:lookup(RID, State#state.responses) of
                {value, PrevBody} ->
                    {next_state, active,
                     do_reply(State, From, PrevBody, RID)};
                none ->
                    State1 = drop_holding_receiver(State),
                    State2 = stop_inactivity_timer(State1),
                    State3 = restart_wait_timer(State2),
                    Receivers = gb_trees:insert(RID, {From, Req},
                                                State3#state.receivers),
                    {next_state, active, State3#state{receivers = Receivers}}
            end;
       not IsValidKey ->
	   reply_stop(State,
		      #body{http_reason = <<"Session key mismatch">>,
			    attrs =
				[{<<"type">>, <<"terminate">>},
				 {<<"condition">>, <<"item-not-found">>}]},
		      From, RID);
       IsOveractivity ->
	   reply_stop(State,
		      #body{http_reason = <<"Too many requests">>,
			    attrs =
				[{<<"type">>, <<"terminate">>},
				 {<<"condition">>, <<"policy-violation">>}]},
		      From, RID);
       true ->
	   State1 = stop_inactivity_timer(State),
	   State2 = stop_wait_timer(State1),
	   Els = case get_attr('xmpp:restart', Attrs, false) of
		   true ->
		       XMPPDomain = get_attr(to, Attrs, State#state.host),
		       XMPPVer = get_attr('xmpp:version', Attrs,
					  State#state.xmpp_ver),
		       [make_xmlstreamstart(XMPPDomain, XMPPVer)];
		   false -> Req#body.els
		 end,
	   State3 = route_els(State2,
			      maybe_add_xmlstreamend(Els, Type)),
	   {State4, RespEls} = get_response_els(State3),
	   NewKey = get_attr(newkey, Attrs, Key),
	   Pause = get_attr(pause, Attrs, undefined),
	   NewPoll = case State#state.prev_poll of
		       undefined -> undefined;
		       _ -> p1_time_compat:timestamp()
		     end,
	   State5 = State4#state{prev_poll = NewPoll,
				 prev_key = NewKey},
	   if Type == <<"terminate">> ->
		  reply_stop(State5,
			     #body{http_reason = <<"Session close">>,
				   attrs = [{<<"type">>, <<"terminate">>}],
				   els = RespEls},
			     From, RID);
	      Pause /= undefined ->
		  State6 = drop_holding_receiver(State5),
		  State7 = restart_inactivity_timer(State6, Pause),
		  InBuf = buf_in(RespEls, State7#state.el_ibuf),
		  {next_state, active,
		   State7#state{prev_rid = RID, el_ibuf = InBuf}};
	      RespEls == [] ->
		  State6 = drop_holding_receiver(State5),
		  State7 = stop_inactivity_timer(State6),
		  State8 = restart_wait_timer(State7),
		  Receivers = gb_trees:insert(RID, {From, #body{}},
					      State8#state.receivers),
		  {next_state, active,
		   State8#state{prev_rid = RID, receivers = Receivers}};
	      true ->
		  State6 = drop_holding_receiver(State5),
		  reply_next_state(State6#state{prev_rid = RID},
				   #body{els = RespEls}, RID, From)
	   end
    end.

handle_event({become_controller, C2SPid}, StateName,
	     State) ->
    State1 = route_els(State#state{c2s_pid = C2SPid}),
    {next_state, StateName, State1};
handle_event({change_shaper, Shaper}, StateName,
	     State) ->
    NewShaperState = shaper:new(Shaper),
    {next_state, StateName,
     State#state{shaper_state = NewShaperState}};
handle_event(_Event, StateName, State) ->
    ?ERROR_MSG("unexpected event in '~s': ~p",
	       [StateName, _Event]),
    {next_state, StateName, State}.

handle_sync_event({send_xml,
		   {xmlstreamstart, _, _} = El},
		  _From, StateName, State)
    when State#state.xmpp_ver >= <<"1.0">> ->
    OutBuf = buf_in([El], State#state.el_obuf),
    {reply, ok, StateName, State#state{el_obuf = OutBuf}};
handle_sync_event({send_xml, El}, _From, StateName,
		  State) ->
    OutBuf = buf_in([El], State#state.el_obuf),
    State1 = State#state{el_obuf = OutBuf},
    case gb_trees:lookup(State1#state.prev_rid,
			 State1#state.receivers)
	of
      {value, {From, Body}} ->
	  {State2, Els} = get_response_els(State1),
	  {reply, ok, StateName,
	   reply(State2, Body#body{els = Els},
		 State2#state.prev_rid, From)};
      none ->
	  State2 = case p1_queue:out(State1#state.shaped_receivers)
		       of
		     {{value, {TRef, From, Body}}, Q} ->
			 cancel_timer(TRef),
			 (?GEN_FSM):send_event(self(), {Body, From}),
			 State1#state{shaped_receivers = Q};
		     _ -> State1
		   end,
	  {reply, ok, StateName, State2}
    end;
handle_sync_event(close, _From, _StateName, State) ->
    {stop, normal, State};
handle_sync_event(deactivate_socket, _From, StateName,
		  StateData) ->
    {reply, ok, StateName,
     StateData#state{c2s_pid = undefined}};
handle_sync_event(_Event, _From, StateName, State) ->
    ?ERROR_MSG("unexpected sync event in '~s': ~p",
	       [StateName, _Event]),
    {reply, {error, badarg}, StateName, State}.

handle_info({timeout, TRef, wait_timeout}, StateName,
	    #state{wait_timer = TRef} = State) ->
    {next_state, StateName, drop_holding_receiver(State)};
handle_info({timeout, TRef, inactive}, _StateName,
	    #state{inactivity_timer = TRef} = State) ->
    {stop, normal, State};
handle_info({timeout, TRef, shaper_timeout}, StateName,
	    State) ->
    case p1_queue:out(State#state.shaped_receivers) of
      {{value, {TRef, From, Req}}, Q} ->
	  (?GEN_FSM):send_event(self(), {Req, From}),
	  {next_state, StateName,
	   State#state{shaped_receivers = Q}};
      {{value, _}, _} ->
	  ?ERROR_MSG("shaper_timeout mismatch:~n** TRef: ~p~n** "
		     "State: ~p",
		     [TRef, State]),
	  {stop, normal, State};
      _ -> {next_state, StateName, State}
    end;
handle_info({migrate, Node}, StateName, State) ->
    if Node /= node() ->
	   NewState = bounce_receivers(State, migrated),
	   {migrate, NewState,
	    {Node, ?MODULE, start, [StateName, NewState]}, 0};
       true -> {next_state, StateName, State}
    end;
handle_info(_Info, StateName, State) ->
    ?ERROR_MSG("unexpected info:~n** Msg: ~p~n** StateName: ~p",
	       [_Info, StateName]),
    {next_state, StateName, State}.

terminate({migrated, ClonePid}, _StateName, State) ->
    ?INFO_MSG("Migrating session \"~s\" (c2s_pid = "
	      "~p) to ~p on node ~p",
	      [State#state.sid, State#state.c2s_pid, ClonePid,
	       node(ClonePid)]),
    mod_bosh:close_session(State#state.sid);
terminate(_Reason, _StateName, State) ->
    mod_bosh:close_session(State#state.sid),
    case State#state.c2s_pid of
      C2SPid when is_pid(C2SPid) ->
	  (?GEN_FSM):send_event(C2SPid, closed);
      _ -> ok
    end,
    bounce_receivers(State, closed),
    bounce_els_from_obuf(State).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

print_state(State) -> State.

route_els(#state{el_ibuf = Buf, c2s_pid = C2SPid} = State) ->
    NewBuf = p1_queue:dropwhile(
	       fun(El) ->
		       ?GEN_FSM:send_event(C2SPid, El),
		       true
	       end, Buf),
    State#state{el_ibuf = NewBuf}.

route_els(State, Els) ->
    case State#state.c2s_pid of
      C2SPid when is_pid(C2SPid) ->
	  lists:foreach(fun (El) ->
				(?GEN_FSM):send_event(C2SPid, El)
			end,
			Els),
	  State;
      _ ->
	  InBuf = buf_in(Els, State#state.el_ibuf),
	  State#state{el_ibuf = InBuf}
    end.

get_response_els(#state{el_obuf = OutBuf,
			max_concat = MaxConcat} =
		     State) ->
    {Els, NewOutBuf} = buf_out(OutBuf, MaxConcat),
    {State#state{el_obuf = NewOutBuf}, Els}.

reply(State, Body, RID, From) ->
    State1 = restart_inactivity_timer(State),
    Receivers = gb_trees:delete_any(RID,
				    State1#state.receivers),
    State2 = do_reply(State1, From, Body, RID),
    case catch gb_trees:take_smallest(Receivers) of
      {NextRID, {From1, Req}, Receivers1}
	  when NextRID == RID + 1 ->
	  (?GEN_FSM):send_event(self(), {Req, From1}),
	  State2#state{receivers = Receivers1};
      _ -> State2#state{receivers = Receivers}
    end.

reply_next_state(State, Body, RID, From) ->
    State1 = restart_inactivity_timer(State),
    Receivers = gb_trees:delete_any(RID,
				    State1#state.receivers),
    State2 = do_reply(State1, From, Body, RID),
    case catch gb_trees:take_smallest(Receivers) of
      {NextRID, {From1, Req}, Receivers1}
	  when NextRID == RID + 1 ->
	  active(Req, From1,
		 State2#state{receivers = Receivers1});
      _ ->
	  {next_state, active,
	   State2#state{receivers = Receivers}}
    end.

reply_stop(State, Body, From, RID) ->
    {stop, normal, do_reply(State, From, Body, RID)}.

drop_holding_receiver(State) ->
    RID = State#state.prev_rid,
    case gb_trees:lookup(RID, State#state.receivers) of
      {value, {From, Body}} ->
	  State1 = restart_inactivity_timer(State),
	  Receivers = gb_trees:delete_any(RID,
					  State1#state.receivers),
	  State2 = State1#state{receivers = Receivers},
	  do_reply(State2, From, Body, RID);
      none -> State
    end.

do_reply(State, From, Body, RID) ->
    ?DEBUG("send reply:~n** RequestID: ~p~n** Reply: "
	   "~p~n** To: ~p~n** State: ~p",
	   [RID, Body, From, State]),
    (?GEN_FSM):reply(From, Body),
    Responses = gb_trees:delete_any(RID,
				    State#state.responses),
    Responses1 = case gb_trees:size(Responses) of
		   N when N < State#state.max_requests; N == 0 ->
		       Responses;
		   _ -> element(3, gb_trees:take_smallest(Responses))
		 end,
    Responses2 = gb_trees:insert(RID, Body, Responses1),
    State#state{responses = Responses2}.

bounce_receivers(State, Reason) ->
    Receivers = gb_trees:to_list(State#state.receivers),
    ShapedReceivers = lists:map(fun ({_, From,
				      #body{attrs = Attrs} = Body}) ->
					RID = get_attr(rid, Attrs),
					{RID, {From, Body}}
				end,
				p1_queue:to_list(State#state.shaped_receivers)),
    lists:foldl(fun ({RID, {From, Body}}, AccState) ->
			NewBody = if Reason == closed ->
					 #body{http_reason =
						   <<"Session closed">>,
					       attrs =
						   [{type, <<"terminate">>},
						    {condition,
						     <<"other-request">>}]};
				     Reason == migrated ->
					 Body#body{http_reason =
						       <<"Session migrated">>}
				  end,
			do_reply(AccState, From, NewBody, RID)
		end,
		State, Receivers ++ ShapedReceivers).

bounce_els_from_obuf(State) ->
    p1_queue:foreach(
      fun({xmlstreamelement, El}) ->
	      try xmpp:decode(El, ?NS_CLIENT, [ignore_els]) of
		  Pkt when ?is_stanza(Pkt) ->
		      case {xmpp:get_from(Pkt), xmpp:get_to(Pkt)} of
			  {#jid{}, #jid{}} ->
			      ejabberd_router:route(Pkt);
			  _ ->
			      ok
		      end;
		  _ ->
		      ok
	      catch _:{xmpp_codec, _} ->
		      ok
	      end;
	 (_) ->
	      ok
      end, State#state.el_obuf).

is_valid_key(<<"">>, <<"">>) -> true;
is_valid_key(PrevKey, Key) ->
    str:sha(Key) == PrevKey.

is_overactivity(undefined) -> false;
is_overactivity(PrevPoll) ->
    PollPeriod = timer:now_diff(p1_time_compat:timestamp(), PrevPoll) div
		   1000000,
    if PollPeriod < (?DEFAULT_POLLING) -> true;
       true -> false
    end.

make_xmlstreamstart(XMPPDomain, Version) ->
    VersionEl = case Version of
		  <<"">> -> [];
		  _ -> [{<<"version">>, Version}]
		end,
    {xmlstreamstart, <<"stream:stream">>,
     [{<<"to">>, XMPPDomain}, {<<"xmlns">>, ?NS_CLIENT},
      {<<"xmlns:xmpp">>, ?NS_BOSH},
      {<<"xmlns:stream">>, ?NS_STREAM}
      | VersionEl]}.

maybe_add_xmlstreamend(Els, <<"terminate">>) ->
    Els ++ [{xmlstreamend, <<"stream:stream">>}];
maybe_add_xmlstreamend(Els, _) -> Els.

encode_body(#body{attrs = Attrs, els = Els}, Type) ->
    Attrs1 = lists:map(fun ({K, V}) when is_atom(K) ->
			       AmK = iolist_to_binary(atom_to_list(K)),
			       case V of
				 true -> {AmK, <<"true">>};
				 false -> {AmK, <<"false">>};
				 I when is_integer(I), I >= 0 ->
				     {AmK, integer_to_binary(I)};
				 _ -> {AmK, V}
			       end;
			   ({K, V}) -> {K, V}
		       end,
		       Attrs),
    Attrs2 = [{<<"xmlns">>, ?NS_HTTP_BIND} | Attrs1],
    {Attrs3, XMLs} = lists:foldr(fun ({xmlstreamraw, XML},
				      {AttrsAcc, XMLBuf}) ->
					 {AttrsAcc, [XML | XMLBuf]};
				     ({xmlstreamelement,
				       #xmlel{name = <<"stream:error">>} = El},
				      {AttrsAcc, XMLBuf}) ->
					 {[{<<"type">>, <<"terminate">>},
					   {<<"condition">>,
					    <<"remote-stream-error">>},
					   {<<"xmlns:stream">>, ?NS_STREAM}
					   | AttrsAcc],
					  [encode_element(El, Type) | XMLBuf]};
				     ({xmlstreamelement,
				       #xmlel{name = <<"stream:features">>} =
					   El},
				      {AttrsAcc, XMLBuf}) ->
					 {lists:keystore(<<"xmlns:stream">>, 1,
							 AttrsAcc,
							 {<<"xmlns:stream">>,
							  ?NS_STREAM}),
					  [encode_element(El, Type) | XMLBuf]};
                                     ({xmlstreamelement,
                                       #xmlel{name = Name, attrs = EAttrs} = El},
                                      {AttrsAcc, XMLBuf})
                                       when Name == <<"message">>;
                                            Name == <<"presence">>;
                                            Name == <<"iq">> ->
                                         NewAttrs = lists:keystore(
                                                      <<"xmlns">>, 1, EAttrs,
                                                      {<<"xmlns">>, ?NS_CLIENT}),
                                         NewEl = El#xmlel{attrs = NewAttrs},
                                         {AttrsAcc,
                                          [encode_element(NewEl, Type) | XMLBuf]};
				     ({xmlstreamelement, El},
				      {AttrsAcc, XMLBuf}) ->
                                         {AttrsAcc,
                                          [encode_element(El, Type) | XMLBuf]};
				     ({xmlstreamend, _}, {AttrsAcc, XMLBuf}) ->
					 {[{<<"type">>, <<"terminate">>},
					   {<<"condition">>,
					    <<"remote-stream-error">>}
					   | AttrsAcc],
					  XMLBuf};
				     ({xmlstreamstart, <<"stream:stream">>,
				       SAttrs},
				      {AttrsAcc, XMLBuf}) ->
					 StreamID = fxml:get_attr_s(<<"id">>,
								   SAttrs),
					 NewAttrs = case
						      fxml:get_attr_s(<<"version">>,
								     SAttrs)
							of
						      <<"">> ->
							  [{<<"authid">>,
							    StreamID}
							   | AttrsAcc];
						      V ->
							  lists:keystore(<<"xmlns:xmpp">>,
									 1,
									 [{<<"xmpp:version">>,
									   V},
									  {<<"authid">>,
									   StreamID}
									  | AttrsAcc],
									 {<<"xmlns:xmpp">>,
									  ?NS_BOSH})
						    end,
					 {NewAttrs, XMLBuf};
				     ({xmlstreamerror, _},
				      {AttrsAcc, XMLBuf}) ->
					 {[{<<"type">>, <<"terminate">>},
					   {<<"condition">>,
					    <<"remote-stream-error">>}
					   | AttrsAcc],
					  XMLBuf};
				     (_, Acc) -> Acc
				 end,
				 {Attrs2, []}, Els),
    case XMLs of
      [] when Type == xml ->
            [<<"<body">>, attrs_to_list(Attrs3), <<"/>">>];
      _ when Type == xml ->
            [<<"<body">>, attrs_to_list(Attrs3), $>, XMLs,
	     <<"</body>">>]
    end.

encode_element(El, xml) ->
    fxml:element_to_binary(El);
encode_element(El, json) ->
    El.

decode_body(Data, Size, Type) ->
    case decode(Data, Type) of
      #xmlel{name = <<"body">>, attrs = Attrs,
	     children = Els} ->
	  case attrs_to_body_attrs(Attrs) of
	    {error, _} = Err -> Err;
	    BodyAttrs ->
		case get_attr(rid, BodyAttrs) of
		  <<"">> -> {error, <<"Missing \"rid\" attribute">>};
		  _ ->
		      Els1 = lists:flatmap(fun (#xmlel{} = El) ->
						   [{xmlstreamelement, El}];
					       (_) -> []
					   end,
					   Els),
		      {ok, #body{attrs = BodyAttrs, size = Size, els = Els1}}
		end
	  end;
      #xmlel{} -> {error, <<"Unexpected payload">>};
      _ when Type == xml ->
            {error, <<"XML is not well-formed">>};
      _ when Type == json ->
            {error, <<"JSON is not well-formed">>}
    end.

decode(Data, xml) ->
    fxml_stream:parse_element(Data).

attrs_to_body_attrs(Attrs) ->
    lists:foldl(fun (_, {error, Reason}) -> {error, Reason};
		    ({Attr, Val}, Acc) ->
			try case Attr of
			      <<"ver">> -> [{ver, Val} | Acc];
			      <<"xmpp:version">> ->
				  [{'xmpp:version', Val} | Acc];
			      <<"type">> -> [{type, Val} | Acc];
			      <<"key">> -> [{key, Val} | Acc];
			      <<"newkey">> -> [{newkey, Val} | Acc];
			      <<"xmlns">> -> Val = (?NS_HTTP_BIND), Acc;
			      <<"secure">> -> [{secure, to_bool(Val)} | Acc];
			      <<"xmpp:restart">> ->
				  [{'xmpp:restart', to_bool(Val)} | Acc];
			      <<"to">> ->
				  [{to, jid:nameprep(Val)} | Acc];
			      <<"wait">> -> [{wait, to_int(Val, 0)} | Acc];
			      <<"ack">> -> [{ack, to_int(Val, 0)} | Acc];
			      <<"sid">> -> [{sid, Val} | Acc];
			      <<"hold">> -> [{hold, to_int(Val, 0)} | Acc];
			      <<"rid">> -> [{rid, to_int(Val, 0)} | Acc];
			      <<"pause">> -> [{pause, to_int(Val, 0)} | Acc];
			      _ -> [{Attr, Val} | Acc]
			    end
			catch
			  _:_ ->
			      {error,
			       <<"Invalid \"", Attr/binary, "\" attribute">>}
			end
		end,
		[], Attrs).

to_int(S, Min) ->
    case binary_to_integer(S) of
      I when I >= Min -> I;
      _ -> erlang:error(badarg)
    end.

to_bool(<<"true">>) -> true;
to_bool(<<"1">>) -> true;
to_bool(<<"false">>) -> false;
to_bool(<<"0">>) -> false.

attrs_to_list(Attrs) -> [attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
    [$\s, Name, $=, $', fxml:crypt(Value), $'].

bosh_response(Body, Type) ->
    CType = case Type of
                xml -> ?CT_XML;
                json -> ?CT_JSON
            end,
    {200, Body#body.http_reason, ?HEADER(CType),
     encode_body(Body, Type)}.

bosh_response_with_msg(Body, Type, RcvBody) ->
    ?DEBUG("send error reply:~p~n** Receiced body: ~p",
	   [Body, RcvBody]),
    bosh_response(Body, Type).

http_error(Status, Reason, Type) ->
    CType = case Type of
                xml -> ?CT_XML;
                json -> ?CT_JSON
            end,
    {Status, Reason, ?HEADER(CType), <<"">>}.

make_sid() -> str:sha(randoms:get_string()).

-compile({no_auto_import, [{min, 2}]}).

min(undefined, B) -> B;
min(A, B) -> erlang:min(A, B).

check_bosh_module(XmppDomain) ->
    case gen_mod:is_loaded(XmppDomain, mod_bosh) of
      true -> ok;
      false ->
	  ?ERROR_MSG("You are trying to use BOSH (HTTP Bind) "
		     "in host ~p, but the module mod_bosh "
		     "is not started in that host. Configure "
		     "your BOSH client to connect to the correct "
		     "host, or add your desired host to the "
		     "configuration, or check your 'modules' "
		     "section in your ejabberd configuration "
		     "file.",
		     [XmppDomain])
    end.

get_attr(Attr, Attrs) -> get_attr(Attr, Attrs, <<"">>).

get_attr(Attr, Attrs, Default) ->
    case lists:keysearch(Attr, 1, Attrs) of
      {value, {_, Val}} -> Val;
      _ -> Default
    end.

buf_new(Host) ->
    buf_new(Host, unlimited).

buf_new(Host, Limit) ->
    QueueType = case gen_mod:get_module_opt(
		       Host, mod_bosh, queue_type,
		       mod_bosh:mod_opt_type(queue_type)) of
		    undefined -> ejabberd_config:default_queue_type(Host);
		    T -> T
		end,
    p1_queue:new(QueueType, Limit).

buf_in(Xs, Buf) ->
    lists:foldl(fun p1_queue:in/2, Buf, Xs).

buf_out(Buf, Num) when is_integer(Num), Num > 0 ->
    buf_out(Buf, Num, []);
buf_out(Buf, _) -> {p1_queue:to_list(Buf), p1_queue:clear(Buf)}.

buf_out(Buf, 0, Els) -> {lists:reverse(Els), Buf};
buf_out(Buf, I, Els) ->
    case p1_queue:out(Buf) of
      {{value, El}, NewBuf} ->
	  buf_out(NewBuf, I - 1, [El | Els]);
      {empty, _} -> buf_out(Buf, 0, Els)
    end.

cancel_timer(TRef) when is_reference(TRef) ->
    (?GEN_FSM):cancel_timer(TRef);
cancel_timer(_) -> false.

restart_timer(TRef, Timeout, Msg) ->
    cancel_timer(TRef),
    erlang:start_timer(timer:seconds(Timeout), self(), Msg).

restart_inactivity_timer(#state{inactivity_timeout =
				    Timeout} =
			     State) ->
    restart_inactivity_timer(State, Timeout).

restart_inactivity_timer(#state{inactivity_timer =
				    TRef} =
			     State,
			 Timeout) ->
    NewTRef = restart_timer(TRef, Timeout, inactive),
    State#state{inactivity_timer = NewTRef}.

stop_inactivity_timer(#state{inactivity_timer = TRef} =
			  State) ->
    cancel_timer(TRef),
    State#state{inactivity_timer = undefined}.

restart_wait_timer(#state{wait_timer = TRef,
			  wait_timeout = Timeout} =
		       State) ->
    NewTRef = restart_timer(TRef, Timeout, wait_timeout),
    State#state{wait_timer = NewTRef}.

stop_wait_timer(#state{wait_timer = TRef} = State) ->
    cancel_timer(TRef), State#state{wait_timer = undefined}.

start_shaper_timer(Timeout) ->
    erlang:start_timer(Timeout, self(), shaper_timeout).

make_random_jid(Host) ->
    User = randoms:get_string(),
    jid:make(User, Host, randoms:get_string()).

make_socket(Pid, IP) -> {http_bind, Pid, IP}.
