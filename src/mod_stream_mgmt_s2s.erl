-module(mod_stream_mgmt_s2s).
-behaviour(gen_mod).
-author('amuhar3@gmail.com').
-protocol({xep, 198, '1.5.2'}).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1]).
%% client part hooks
-export([s2s_out_stream_init/2, s2s_out_stream_features/2,
         s2s_out_packet/2, s2s_out_handle_recv/3, s2s_out_handle_send/3,
         s2s_out_handle_info/2, s2s_out_closed/2, 
         s2s_out_terminate/2, s2s_out_established/1]).
%% server part hooks
-export([s2s_in_stream_started/1, s2s_in_stream_features/2,
         s2s_in_unauthenticated_packet/2, s2s_in_authenticated_packet/2,
         s2s_in_handle_call/3, s2s_in_handle_info/2,
         s2s_in_closed/2, s2s_in_terminate/2]).

-include("xmpp.hrl").
-include("logger.hrl").
-include("p1_queue.hrl").

-define(is_sm_packet(Pkt),
        is_record(Pkt, sm_enable) or
        is_record(Pkt, sm_enabled) or
        is_record(Pkt, sm_resume) or
        is_record(Pkt, sm_resumed) or
        is_record(Pkt, sm_a) or
        is_record(Pkt, sm_r)).

% replace to separate file

-record(s2s, {fromto = {<<"">>, <<"">>} :: {binary(), binary()} | '_',
              pid = self()              :: pid() | '_' | '$1'}).

-type state() :: map().

%%%=============================================================================
%%% API
%%%=============================================================================
start(Host, _Opts) ->
    ejabberd_hooks:add(s2s_out_init, Host, ?MODULE, s2s_out_stream_init, 50),
    % ejabberd_hooks:add(s2s_out_authenticated_features, 
    %                    Host, ?MODULE, s2s_out_stream_features, 50),
    ejabberd_hooks:add(s2s_out_packet, Host, ?MODULE, s2s_out_packet, 50),
    ejabberd_hooks:add(s2s_out_handle_recv, 
                       Host, ?MODULE, s2s_out_handle_recv, 50),
    ejabberd_hooks:add(s2s_out_handle_send, 
                       Host, ?MODULE, s2s_out_handle_send, 50),
    ejabberd_hooks:add(s2s_out_handle_info,
                       Host, ?MODULE, s2s_out_handle_info, 50),
    ejabberd_hooks:add(s2s_out_closed,
                       Host, ?MODULE, s2s_out_closed, 50),
    ejabberd_hooks:add(s2s_out_terminate,
                       Host, ?MODULE, s2s_out_terminate, 50),
    ejabberd_hooks:add(s2s_out_established,
                       Host, ?MODULE, s2s_out_established, 50),
    %% server part
    ejabberd_hooks:add(s2s_in_stream_started,
                       Host, ?MODULE, s2s_in_stream_started, 50),
    ejabberd_hooks:add(s2s_in_post_auth_features,
                       Host, ?MODULE, s2s_in_stream_features, 50),
    ejabberd_hooks:add(s2s_in_unauthenticated_packet,
                       Host, ?MODULE, s2s_in_unauthenticated_packet, 50),
    ejabberd_hooks:add(s2s_in_authenticated_packet,
                       Host, ?MODULE, s2s_in_authenticated_packet, 50),
    ejabberd_hooks:add(s2s_in_handle_call,
                       Host, ?MODULE, s2s_in_handle_call, 50),
    ejabberd_hooks:add(s2s_in_handle_info,
                       Host, ?MODULE, s2s_in_handle_info, 50),
    ejabberd_hooks:add(s2s_in_closed,
                       Host, ?MODULE, s2s_in_closed, 50),
    ejabberd_hooks:add(s2s_in_terminate,
                       Host, ?MODULE, s2s_in_terminate, 50).

stop(Host) ->
    ejabberd_hooks:delete(s2s_out_init, Host, ?MODULE, s2s_out_stream_init, 50),
    % ejabberd_hooks:delete(s2s_out_authenticated_features, 
    %                       Host, ?MODULE, s2s_out_stream_features, 50),
    ejabberd_hooks:delete(s2s_out_packet, Host, ?MODULE, s2s_out_packet, 50),
    ejabberd_hooks:delete(s2s_out_handle_recv, 
                          Host, ?MODULE, s2s_out_handle_recv, 50),
    ejabberd_hooks:delete(s2s_out_handle_send, 
                          Host, ?MODULE, s2s_out_handle_send, 50),
    ejabberd_hooks:delete(s2s_out_handle_info,
                          Host, ?MODULE, s2s_out_handle_info, 50),
    ejabberd_hooks:delete(s2s_out_closed,
                          Host, ?MODULE, s2s_out_closed, 50),
    ejabberd_hooks:delete(s2s_out_terminate,
                       Host, ?MODULE, s2s_out_terminate, 50),
    ejabberd_hooks:delete(s2s_out_established,
                          Host, ?MODULE, s2s_out_established, 50),
    %% server part
    ejabberd_hooks:delete(s2s_in_stream_started,
                          Host, ?MODULE, s2s_in_stream_started, 50),
    ejabberd_hooks:delete(s2s_in_post_auth_features,
                          Host, ?MODULE, s2s_in_stream_features, 50),
    ejabberd_hooks:delete(s2s_in_unauthenticated_packet,
                          Host, ?MODULE, s2s_in_unauthenticated_packet, 50),
    ejabberd_hooks:delete(s2s_in_authenticated_packet,
                          Host, ?MODULE, s2s_in_authenticated_packet, 50),
    ejabberd_hooks:delete(s2s_in_handle_call,
                          Host, ?MODULE, s2s_in_handle_call, 50),
    ejabberd_hooks:delete(s2s_in_handle_info,
                          Host, ?MODULE, s2s_in_handle_info, 50),
    ejabberd_hooks:delete(s2s_in_closed,
                          Host, ?MODULE, s2s_in_closed, 50),
    ejabberd_hooks:delete(s2s_in_terminate,
                          Host, ?MODULE, s2s_in_terminate, 50).

reload(_Host, _NewOpts, _OldOpts) ->
    ?WARNING_MSG("module ~s is reloaded, but new configuration will take "
                 "effect for newly created s2s connections only", [?MODULE]).

depends(_Host, _Opts) -> [].

%% client part
s2s_out_stream_init({ok, #{server_host := ServerHost} = State}, Opts) -> 
    State1 = State#{mgmt_timeout => get_resume_timeout(ServerHost),
                    mgmt_queue_type => get_queue_type(ServerHost),
                    mgmt_max_queue => get_max_ack_queue(ServerHost),
                    mgmt_ack_timeout => get_ack_timeout(ServerHost),
                    mgmt_connection_timeout => get_connection_timeout(ServerHost),
                    mgmt_stanzas_out => 0,
                    mgmt_stanzas_req => 0},
    
    case proplists:get_value(resume, Opts) of
        OldState when OldState /= undefined ->
            {ok, State1#{mgmt_state => connecting, mgmt_old_state => OldState}};
        _ ->
            {ok, State1#{mgmt_state => inactive}}
    end;            
s2s_out_stream_init(Acc, _Opts) ->
    Acc.

s2s_out_stream_features(#{mgmt_timeout := Timeout,
                          mgmt_queue_type := QueueType} = State, 
                        #stream_features{sub_els = SubEls}) ->
    case check_stream_mgmt_support(SubEls) of
        Xmlns when Xmlns == ?NS_STREAM_MGMT_2; Xmlns == ?NS_STREAM_MGMT_3 ->
            case State of
                #{mgmt_old_state := OldState} ->
                    #{mgmt_previd := Id} = OldState,
                    State1 = State#{mgmt_state => pending,
                                    mgmt_xmlns => Xmlns},
                    send(State1, #sm_resume{h = 0,
                                            xmlns = Xmlns,
                                            previd = Id});
                _ ->
                    Res = if Timeout > 0 ->
                                  #sm_enable{xmlns = Xmlns,
                                             resume = true,
                                             max = Timeout};
                             true ->
                                  #sm_enable{xmlns = Xmlns}
                          end,
                    State1 = State#{mgmt_state => wait_for_enabled,
                                    mgmt_xmlns => Xmlns,
                                    mgmt_queue => p1_queue:new(QueueType)},
                    send(State1, Res)
            end;
        _ ->
            State
    end;
s2s_out_stream_features(State, _) ->
    State.

s2s_out_established(#{mgmt_state := connecting,
                      mgmt_old_state := OldState} = State) ->
    #{mgmt_previd := Id} = OldState,
    State1 = State#{mgmt_state => pending,
                    mgmt_xmlns => ?NS_STREAM_MGMT_3},
    send(State1, #sm_resume{h = 0,
                            xmlns = ?NS_STREAM_MGMT_3,
                            previd = Id});
s2s_out_established(#{mgmt_timeout := Timeout,
                      mgmt_queue_type := QueueType} = State) ->
    Xmlns = ?NS_STREAM_MGMT_3,
    Res = 
        if Timeout > 0 ->
                #sm_enable{xmlns = Xmlns,
                           resume = true,
                           max = Timeout};
           true ->
                #sm_enable{xmlns = Xmlns}
        end,

    State1 = State#{mgmt_state => wait_for_enabled,
                    mgmt_xmlns => Xmlns,
                    mgmt_queue => p1_queue:new(QueueType)},
    send(State1, Res); 
s2s_out_established(State) ->
    State.

s2s_out_packet(#{mgmt_state := pending} = State, #sm_resumed{} = Pkt) ->
    {stop, handle_resumed(Pkt, State)};
s2s_out_packet(#{mgmt_state := MgmtState} = State, Pkt)
  when ?is_sm_packet(Pkt) ->
    if MgmtState == active ->
            {stop, perform_stream_mgmt(Pkt, State)};
       MgmtState == wait_for_enabled ->
            {stop, negotiate_stream_mgmt(Pkt, State)};
       true ->
            {stop, State}
    end;
s2s_out_packet(State, _Pkt) ->
    State.

s2s_out_handle_send(#{mgmt_state := MgmtState, lang := Lang} = State, Pkt, SendResult)
  when MgmtState == active;
       MgmtState == wait_for_enabled ->
    case Pkt of
        _ when ?is_stanza(Pkt) ->
            Meta = xmpp:get_meta(Pkt),
            case maps:get(mgmt_is_resent, Meta, false) of
                false ->
                    case mod_stream_mgmt:mgmt_queue_add(State, Pkt) of
                        #{mgmt_max_queue := exceeded} = State1 ->
                            Err = xmpp:serr_policy_violation(
                                        <<"Too many unacked stanzas">>, Lang),
                            send(State1, Err);
                        State1 when MgmtState == active, SendResult == ok ->
                            send_rack(State1);
                        State1 ->
                            State1
                    end;
                true ->
                    State
            end;
        _ ->
            State
    end;
s2s_out_handle_send(State, _, _) ->
    State.

s2s_out_handle_info(#{mgmt_ack_timer := TRef, remote_server := RServer,
                      mod := Mod} = State, {timeout, TRef, ack_timeout}) ->
    ?DEBUG("Timed out waiting for stream management "
           "acknowledgement of ~s", [RServer]),
    Mod:stop(State);
s2s_out_handle_info(#{mgmt_state := connecting,
                      remote_server := RServer, mod := Mod} = State,
                    {timeout, _TRef, connection_timeout}) ->
    ?DEBUG("Timed out waiting for connection "
           "establishment for resumption previous session with ~s", [RServer]),
    Mod:stop(State#{mgmt_state => timeout});
s2s_out_handle_info(State, _) ->
    State.

s2s_out_handle_recv(#{mgmt_state := wait_for_enabled,
                      remote_server := RServer} = State, _El, #sm_failed{}) ->
    ?DEBUG("Remote server ~s can't enable stream management", [RServer]),
    State#{mgmt_state => inactive};
s2s_out_handle_recv(#{mgmt_state := pending,
                      remote_server := RServer,
                      mgmt_timeout := Timeout,
                      mgmt_xmlns := Xmlns,
                      mgmt_queue_type := QueueType,
                      mgmt_old_state := OldState} = State, _El, #sm_failed{}) ->
    ?DEBUG("Remote server ~s can't resume previous session", [RServer]),
    #{mgmt_queue := Queue} = OldState,
    State1 = State#{mgmt_state => wait_for_enabled,
                    mgmt_queue => p1_queue:new(QueueType)},
    State2 = maps:remove(mgmt_old_state, State1),
    State3 =
        if Timeout > 0 ->
                send(State2, #sm_enable{xmlns = Xmlns,
                                        resume = true,
                                        max = Timeout});
           true ->
                send(State2, #sm_enable{xmlns = Xmlns})
    end,
    resend_unacked_stanzas(State3, Queue);
s2s_out_handle_recv(State, _El, _Pkt) ->
    State.

s2s_out_closed(#{mgmt_state := connecting} = State, _) ->
    {stop, transition_to_resume(State#{stream_state => connecting})};
s2s_out_closed(State, _) ->
    State.

s2s_out_terminate(#{mgmt_state := active} = State, _Reason) ->
    transition_to_resume(State);
s2s_out_terminate(#{mgmt_state := timeout,
                    mgmt_old_state := OldState} = State, _Reason) ->
    #{mgmt_queue := Queue} = OldState,
    bounce_errors(State, Queue),
    State;
s2s_out_terminate(#{mgmt_state := pending,
                    mgmt_old_state := OldState} = State, _Reason) ->
    #{mgmt_queue := Queue} = OldState,
    route_unacked_stanzas(State, Queue),
    State;
s2s_out_terminate(State, _Reason) ->
    State.

%% client part

%% server part

s2s_in_stream_started(#{server_host := Host} = State) ->
    Timeout = get_resume_timeout(Host),
    MaxTimeout = get_max_resume_timeout(Host, Timeout),
    State#{mgmt_state => inactive,
           mgmt_timeout => Timeout,
           mgmt_max_timeout => MaxTimeout,
           mgmt_stanzas_in => 0};
s2s_in_stream_started(State) ->
    State.

s2s_in_stream_features(Acc, _Host) ->
    [#feature_sm{xmlns = ?NS_STREAM_MGMT_2},
     #feature_sm{xmlns = ?NS_STREAM_MGMT_3}| Acc].

s2s_in_unauthenticated_packet(State, Pkt) when ?is_sm_packet(Pkt) ->
    Err = #sm_failed{reason = 'unexpected-request',
                     xmlns = ?NS_STREAM_MGMT_3},
    {stop, send(State, Err)};
s2s_in_unauthenticated_packet(State, _Pkt) ->
    State.

s2s_in_authenticated_packet(#{mgmt_state := inactive} = State,
                            #sm_resume{} = Pkt) ->
    {stop, handle_resume(State, Pkt)};
s2s_in_authenticated_packet(#{mgmt_state := MgmtState} = State, Pkt)
  when ?is_sm_packet(Pkt) ->
    if MgmtState == active; MgmtState == pending ->
            {stop, perform_stream_mgmt(Pkt, State)};
       true ->
            {stop, negotiate_stream_mgmt(Pkt, State)}
    end;
s2s_in_authenticated_packet(State, Pkt) ->
    mod_stream_mgmt:update_num_stanzas_in(State, Pkt).

s2s_in_handle_call(#{mgmt_state := pending, mod:= Mod,
                     remote_server := RServer, unique_id := UniqueId} = State,
                    {resume_session, RServer, UniqueId}, From) ->
    Mod:reply(From, {resume, State}),
    {stop, State#{mgmt_state => resumed}};
s2s_in_handle_call(#{mod := Mod} = State, {resume_session, _, _}, From) ->
    Mod:reply(From, error),
    {stop, State};
s2s_in_handle_call(State, _Call, _From) ->
    State.

s2s_in_handle_info(#{mgmt_state := pending, remote_server := RServer,
                     mod := Mod} = State, {timeout, _, pending_timeout}) ->
    ?DEBUG("Timed out waiting for resumption of stream for ~s", [RServer]),
    Mod:stop(State);
s2s_in_handle_info(State, _Msg) ->
  State.

s2s_in_closed(#{mgmt_state := active} = State, _Reason) ->
    {stop, transition_to_pending(State)};
s2s_in_closed(State, _Reason) ->
    State.

% it isn't important: we can delete it and delete from ejabberd_s2s_in module
% ejabberd_hooks:run_fold(..)
s2s_in_terminate(#{mgmt_state := resumed,
                   remote_server := RServer} = State, _Reason) ->
    ?INFO_MSG("Closing former stream of resumed session for ~s", [RServer]),
    State;
s2s_in_terminate(State, _Reason) ->
    State.

%% server part end


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec check_stream_mgmt_support(Els :: [xmlel()]) -> binary().
check_stream_mgmt_support(Els) ->
    check_stream_mgmt_support(Els, <<>>).

-spec check_stream_mgmt_support(Els :: [xmlel()], 
                                Res :: binary()) -> binary().
check_stream_mgmt_support([El | Els], Res) ->
    case El of 
        #xmlel{name = <<"sm">>, attrs = Attrs} ->
            case fxml:get_attr(<<"xmlns">>, Attrs) of
                {value, ?NS_STREAM_MGMT_3} ->
                    ?NS_STREAM_MGMT_3;
                {value, ?NS_STREAM_MGMT_2} ->
                    check_stream_mgmt_support(Els, ?NS_STREAM_MGMT_2);
                _ ->
                    check_stream_mgmt_support(Els, Res)
            end;
        _ -> 
            check_stream_mgmt_support(Els, Res)
    end;
check_stream_mgmt_support([], Res) -> Res.

-spec negotiate_stream_mgmt(xmpp_element(), state()) -> state().
negotiate_stream_mgmt(Pkt, State) ->
    {ServerPart, Xmlns} = 
        case State of
            #{mgmt_xmlns := MgmtXmlns} ->
                {false, MgmtXmlns};
            _ ->
                {true, xmpp:get_ns(Pkt)}
        end,

    case Pkt of
        #sm_enable{} when ServerPart ->
            handle_enable(State#{mgmt_xmlns => Xmlns}, Pkt);
        #sm_enabled{} when not ServerPart ->
            handle_enabled(State, Pkt);
        _ when is_record(Pkt, sm_a);
               is_record(Pkt, sm_r);
               (is_record(Pkt, sm_resume) andalso ServerPart);
               (is_record(Pkt, sm_resumed) andalso not ServerPart) -> 
            Err = #sm_failed{reason = 'unexpected-request', xmlns = Xmlns},
            send(State, Err);
        _ ->
            Err = #sm_failed{reason = 'bad-request', xmlns = Xmlns},
            send(State, Err)
    end.

-spec perform_stream_mgmt(xmpp_element(), state()) -> state().
perform_stream_mgmt(Pkt, #{mgmt_xmlns := Xmlns} = State) ->
    case xmpp:get_ns(Pkt) of
        Xmlns ->
            case Pkt of
                #sm_a{} ->
                    handle_a(State, Pkt);
                #sm_r{} ->
                    handle_r(State);
                _ ->
                    send(State, #sm_failed{reason = 'bad-request', xmlns = Xmlns})
            end;
        _ ->
            send(State, #sm_failed{reason = 'unsupported-version', xmlns = Xmlns})
    end.

handle_enable(#{remote_server := RServer,
                mgmt_timeout := DefaultTimeout,
                mgmt_max_timeout := MaxTimeout,
                mgmt_xmlns := Xmlns} = State,
              #sm_enable{resume = Resume, max = Max}) ->
    Timeout =
        if Resume == false ->
                0;
           Max /= undefined, Max > 0, Max =< MaxTimeout ->
                Max;
           true ->
                DefaultTimeout
        end,
    Res = if Timeout > 0 ->
                  ?INFO_MSG("Stream management with "
                            "resumption enabled for ~s", [RServer]),
                  #sm_enabled{resume = true,
                              id = make_resume_id(State),
                              max = Timeout, xmlns = Xmlns};
             true ->
                  ?INFO_MSG("Stream management enabled for ~s", [RServer]),
                  #sm_enabled{xmlns = Xmlns}
          end,
    State1 = State#{mgmt_state => active,
                    mgmt_timeout => Timeout},
    send(State1, Res).

-spec handle_enabled(state(), sm_enabled()) -> state().
handle_enabled(#{remote_server := RServer,
                 mgmt_timeout :=  DefaultTimeout,
                 mgmt_queue := Queue} = State,
               #sm_enabled{resume = Resume, max = Max, id = Id}) ->
    Timeout = if Resume == false ->
                    0;
                 Max /= undefined ->
                    Max;
                 true ->
                    DefaultTimeout
              end,
    State1 = if Timeout > 0 ->
                    ?INFO_MSG("Stream management with "
                              "resumption enabled for ~s", [RServer]),
                    State#{mgmt_state => active,
                           mgmt_previd => Id,
                           mgmt_timeout => Timeout};
                true ->
                    ?INFO_MSG("Stream management enabled for ~s", [RServer]),
                    State#{mgmt_state => active, mgmt_timeout => Timeout}
             end,

    case not p1_queue:is_empty(Queue) of
        true ->
            send_rack(State1);
        _ ->
            State1
    end.

-spec handle_r(state()) -> state().
handle_r(#{mgmt_stanzas_in := H,
           mgmt_xmlns := Xmlns} = State) ->
    send(State, #sm_a{h = H, xmlns = Xmlns}).

-spec handle_a(state(), sm_a()) -> state().
handle_a(State, #sm_a{h = H}) ->
    State1 = check_h_attribute(State, H),
    resend_rack(State1).

-spec make_resume_id(state()) -> binary().
make_resume_id(#{remote_server := RServer, unique_id := UniqueId}) ->
    misc:term_to_base64({RServer, UniqueId}).

-spec transition_to_resume(state()) -> state().
transition_to_resume(#{mgmt_state := active,
                       mgmt_queue := Queue,
                       mgmt_timeout := 0} = State) ->
    route_unacked_stanzas(State, Queue),
    State;
transition_to_resume(#{mgmt_state := active,
                       server_host := Server,
                       remote_server := RServer,
                       mgmt_connection_timeout := Timeout} = State) ->
    State1 = mod_stream_mgmt:cancel_ack_timer(State),
    ?DEBUG("Try to connect to remote server ~s", [RServer]),
    case resume(Server, RServer, [{resume, State1}]) of
      {ok, Pid} ->
        erlang:start_timer(Timeout, Pid, connection_timeout),
        State1;
      _ ->
        State1
    end;
transition_to_resume(#{mgmt_state := connecting, mod := Mod} = State) ->    
    Mod:connect(self()),
    State;
transition_to_resume(State) ->
    State.

-spec transition_to_pending(state()) -> state().
transition_to_pending(#{mgmt_state := active, mod := Mod,
                        mgmt_timeout := 0} = State) ->
    Mod:stop(State);
transition_to_pending(#{mgmt_state := active,
                        remote_server := RServer, mgmt_timeout := Timeout} = State) ->
    ?INFO_MSG("Waiting for resumption of stream for ~s", [RServer]),
    erlang:start_timer(timer:seconds(Timeout), self(), pending_timeout),
    State#{mgmt_state => pending};
transition_to_pending(State) ->
    State.

resume(From, To, Opts) ->
    {ok, Pid} = ejabberd_s2s_out:start(From, To, Opts),
    F = fun() ->
          mnesia:write(#s2s{fromto = {From, To}, pid = Pid}),
          Pid
        end,
    TRes = mnesia:transaction(F),
    case TRes of
      {atomic, Pid} ->
          ejabberd_s2s_out:connect(Pid),
          {ok, Pid};
      {aborted, _Reason} ->
          ejabberd_s2s_out:stop(Pid),
          error
    end.

-spec resume_session(pid(), {binary(), integer()}) -> {resume, state()} | error.
resume_session(Pid, {RServer, UniqueId}) ->
    ejabberd_s2s_in:call(Pid, {resume_session, RServer, UniqueId}, timer:seconds(15)).

-spec inherit_session_state({binary(), binary()}, list()) -> {ok, state()}|
                                                             {error, binary()}.
inherit_session_state(_ResumeId, [])  ->
    {error, <<"Previous session PID not found">>};
inherit_session_state({RServer, UniqueId} = ResumeId, [{_, Pid, _, _}|Specs])
  when is_pid(Pid) ->
    try resume_session(Pid, {RServer, UniqueId}) of
        {resume, OldState} ->
            ejabberd_s2s_in:stop(Pid),
            {ok, OldState};
        error ->
            inherit_session_state(ResumeId, Specs)
    catch
        _:_ ->
            inherit_session_state(ResumeId, Specs)
    end;
inherit_session_state(ResumeId, [_H|L] ) ->
    inherit_session_state(ResumeId, L).

handle_resume(#{lang := Lang, remote_server := RServer} = State,
              #sm_resume{previd = ResumeId, xmlns = Xmlns}) ->
    case misc:base64_to_term(ResumeId) of
        {term, {RServer, UniqueId}} ->
            case inherit_session_state({RServer, UniqueId},
                        supervisor:which_children(ejabberd_s2s_in_sup)) of
                {ok, OldState} ->
                    #{mgmt_stanzas_in := H,
                      mgmt_timeout := Timeout,
                      mgmt_xmlns := AttrXmlns} = OldState,

                    State1 = State#{mgmt_state => active,
                                    mgmt_stanzas_in => H,
                                    mgmt_timeout => Timeout,
                                    mgmt_xmlns => AttrXmlns,
                                    unique_id => UniqueId},

                    State2 = send(State1, #sm_resumed{previd = ResumeId,
                                                      h = H,
                                                      xmlns = AttrXmlns}),
                    ?INFO_MSG("Resumed session for ~s", [RServer]),
                    State2;
                {error, Msg} ->
                    ?INFO_MSG("Cannot resume session for ~s: ~s", [RServer, Msg]),
                    Err = #sm_failed{reason = 'item-not-found',
                                     text = xmpp:mk_text(Msg, Lang),
                                     xmlns = Xmlns},
                    send(State, Err)
            end;
        _ ->
            Msg = <<"Invalid 'previd' value">>,
            ?INFO_MSG("Cannot resume session for ~s: ~s", [RServer, Msg]),
            Err = #sm_failed{reason = 'item-not-found',
                             text = xmpp:mk_text(Msg, Lang),
                             xmlns = Xmlns},
            send(State, Err)
    end.                           

-spec handle_resumed(sm_resumed(), state()) -> state().
handle_resumed(#sm_resumed{h = H, previd = _Id}, 
               #{remote_server := RServer,
                 mgmt_old_state := OldState} = State) ->
    ResumedState = copy_state(OldState, State),
    #{mgmt_xmlns := Xmlns, mgmt_queue := Queue} = ResumedState, 
    State1 = check_h_attribute(ResumedState, H),
    State2 = resend_unacked_stanzas(State1#{mgmt_state => active}, Queue),
    ?DEBUG("Resumed session for ~s", [RServer]),
    send(State2, #sm_r{xmlns = Xmlns}).

resend_unacked_stanzas(#{remote_server := RServer} = State, Queue) 
  when ?qlen(Queue) > 0 ->
    ?DEBUG("Resending ~B unacknowledged stanza(s) to ~s",
           [p1_queue:len(Queue), RServer]),
    p1_queue:foldl(
        fun({_, Time, Pkt}, AccState) ->
            NewPkt = add_resent_delay_info(AccState, Pkt, Time),
            send(AccState, xmpp:put_meta(NewPkt, mgmt_is_resent, true))
        end, State, Queue);
resend_unacked_stanzas(State, _) ->
    State.

route_unacked_stanzas(#{remote_server := RServer} = State, Queue)
  when ?qlen(Queue) > 0 ->
    ?DEBUG("Re-rout ~B unacknowledged stanza(s) to ~s",
           [p1_queue:len(Queue), RServer]),
    p1_queue:foreach(
        fun({_, Time, Pkt}) ->
            NewPkt = add_resent_delay_info(State, Pkt, Time),
            ejabberd_router:route(xmpp:put_meta(NewPkt, mgmt_is_resent, true))
        end, Queue);
route_unacked_stanzas(_State, _Queue) ->
  ok.

bounce_errors(_State, Queue)
  when ?qlen(Queue) > 0 ->
    p1_queue:foreach(
        fun({_, _, Pkt}) ->
            Error = xmpp:err_remote_server_timeout(),
            ejabberd_router:route_error(Pkt, Error)
        end, Queue);
bounce_errors(_State, _) ->
    ok.

-spec copy_state(state(), state()) -> state().
copy_state(#{mgmt_xmlns := Xmlns,
             mgmt_queue := Queue,
             mgmt_stanzas_out := NumStanzasOut,
             mgmt_previd := Id} = _OldState,
           #{mgmt_queue_type := QueueType} = NewState) ->

    Queue1 = case QueueType of
                 ram -> p1_queue:file_to_ram(Queue);
                 _ -> p1_queue:ram_to_file(Queue)
             end,

    NewState#{mgmt_xmlns => Xmlns,
              mgmt_queue => Queue1,
              mgmt_stanzas_out => NumStanzasOut,
              mgmt_previd => Id}.

-spec check_h_attribute(state(), non_neg_integer()) -> state().
check_h_attribute(#{mgmt_stanzas_out := NumStanzasOut,
                    remote_server := RServer} = State, H)
  when H > NumStanzasOut ->
    ?DEBUG("~s acknowledged ~B stanzas," 
           "but only ~B were sent ", [RServer, H, NumStanzasOut]),
    mod_stream_mgmt:mgmt_queue_drop(State#{mgmt_stanzas_out => H}, NumStanzasOut);
check_h_attribute(#{mgmt_stanzas_out := NumStanzasOut,
                    remote_server := RServer} = State, H) ->
    ?DEBUG("~s acknowledged ~B of ~B "
           "stanzas", [RServer, H, NumStanzasOut]),
    mod_stream_mgmt:mgmt_queue_drop(State, H).

-spec add_resent_delay_info(state(), stanza(), erlang:timestamp()) -> stanza().
add_resent_delay_info(#{server_host := LServer}, El, Time) ->
    xmpp_util:add_delay_info(El, jid:make(LServer), Time, <<"Resent">>);
add_resent_delay_info(_State, El, _Time) ->
    El.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% todo: import from mod_stream_mgmt.erl

-spec send(state(), xmpp_element()) -> state().
send(#{mod := Mod} = State, Pkt) ->
    Mod:send(State, Pkt).

send_rack(#{mgmt_ack_timer := _} = State) ->
    State;
send_rack(#{mgmt_xmlns := Xmlns,
            mgmt_stanzas_out := NumStanzasOut,
            mgmt_ack_timeout := AckTimeout} = State) ->
    TRef = erlang:start_timer(AckTimeout, self(), ack_timeout),
    State1 = State#{mgmt_ack_timer => TRef, mgmt_stanzas_req => NumStanzasOut},
    send(State1, #sm_r{xmlns = Xmlns}).

resend_rack(#{mgmt_ack_timer := _,
              mgmt_queue := Queue,
              mgmt_stanzas_out := NumStanzasOut,
              mgmt_stanzas_req := NumStanzasReq} = State) ->
    State1 = mod_stream_mgmt:cancel_ack_timer(State),
    case NumStanzasReq < NumStanzasOut andalso not p1_queue:is_empty(Queue) of
        true -> send_rack(State1);
        false -> State1
    end;
resend_rack(State) ->
    State.

%%%=============================================================================
%%% Configuration processing
%%%=============================================================================

get_resume_timeout(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, resume_timeout, 300).

get_max_resume_timeout(Host, ResumeTimeout) ->
    case gen_mod:get_module_opt(Host, ?MODULE, max_resume_timeout) of
        undefined -> ResumeTimeout;
        Max when Max >= ResumeTimeout -> Max;
        _ -> ResumeTimeout
    end.

get_queue_type(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, queue_type) of
        undefined -> ejabberd_config:default_queue_type(Host);
        Type -> Type
    end.

get_max_ack_queue(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, max_ack_queue, 1000).

get_ack_timeout(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, ack_timeout, 60) of % change default
        infinity -> infinity;
        T -> timer:seconds(T)
    end.

get_connection_timeout(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, connection_timeout, 120000).

mod_opt_type(connection_timeout) ->
    fun(I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(max_ack_queue) ->
    fun(I) when is_integer(I), I > 0 -> I;
       (infinity) -> infinity
    end;
mod_opt_type(ack_timeout) ->
    fun(I) when is_integer(I), I > 0 -> I;
       (infinity) -> infinity
    end;
mod_opt_type(resume_timeout) ->
    fun(I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(max_resume_timeout) ->
    fun(I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(queue_type) ->
    fun(file) -> file;
       (ram) -> ram
    end;
mod_opt_type(_) -> [max_ack_queue, ack_timeout, resume_timeout, max_resume_timeout,
                    queue_type, connection_timeout].
