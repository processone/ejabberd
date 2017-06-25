-module(mod_stream_mgmt_s2s).
-behaviour(gen_mod).
-author('amuhar3@gmail.com').
-protocol({xep, 198, '1.5.2'}).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1]).
%% hooks
-export([s2s_out_stream_init/2, s2s_out_stream_features/2,
         s2s_out_packet/2, s2s_out_handle_recv/3, s2s_out_handle_send/3,
         s2s_out_handle_info/2, s2s_out_closed/2,
         s2s_out_terminate/2, s2s_out_established/1]).

-include("xmpp.hrl").
-include("logger.hrl").
-include("p1_queue.hrl").

% replace ?
-define(is_sm_packet(Pkt),
        is_record(Pkt, sm_enabled) or
        is_record(Pkt, sm_resumed) or
        is_record(Pkt, sm_a) or
        is_record(Pkt, sm_r)).

-type state() :: ejabberd_s2s_out:state().

%%%=============================================================================
%%% API
%%%=============================================================================
start(Host, _Opts) ->
    ejabberd_hooks:add(s2s_out_init, Host, ?MODULE, s2s_out_stream_init, 50),
    ejabberd_hooks:add(s2s_out_authenticated_features, 
                       Host, ?MODULE, s2s_out_stream_features, 50),
    ejabberd_hooks:add(s2s_out_packet, Host, ?MODULE, s2s_out_packet, 5),
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
    % delete after implementation of server part
    ejabberd_hooks:add(s2s_out_established,
                       Host, ?MODULE, s2s_out_established, 50).

stop(Host) ->
    ejabberd_hooks:delete(s2s_out_init, Host, ?MODULE, s2s_out_stream_init, 50),
    ejabberd_hooks:delete(s2s_out_authenticated_features, 
                          Host, ?MODULE, s2s_out_stream_features, 50),
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
    % delete after implementation of server part
    ejabberd_hooks:delete(s2s_out_established,
                          Host, ?MODULE, s2s_out_established, 50).

reload(_Host, _NewOpts, _OldOpts) ->
    ?WARNING_MSG("module ~s is reloaded, but new configuration will take "
                 "effect for newly created s2s connections only", [?MODULE]).

depends(_Host, _Opts) -> [].

s2s_out_stream_init({ok, #{server_host := ServerHost, mod := Mod} = State}, Opts) ->
    case proplists:get_value(resume, Opts) of
        OldState when OldState /= undefined ->
            #{mgmt_stanzas_in := H, mgmt_stanzas_out := NumStanzasOut, 
              mgmt_queue := Queue, mgmt_privid := Id} = OldState,
            State1 = State#{mgmt_queue => Queue,
                            mgmt_stanzas_out => NumStanzasOut,
                            mgmt_stanzas_in => H,
                            mgmt_privid => Id},
            {ok, State1#{mgmt_state => resume, mgmt_old_session => OldState}};
        _ ->
            {ok, State#{mgmt_state => inactive,
                        mgmt_timeout => get_resume_timeout(ServerHost),
                        mgmt_queue_type => get_queue_type(ServerHost),
                        mgmt_max_queue => get_max_ack_queue(ServerHost),
                        mgmt_ack_timeout => get_ack_timeout(ServerHost),
                        mgmt_unacked_stanzas => get_max_unacked_stanzas(ServerHost),
                        mgmt_connection_timeout => get_connection_timeout(ServerHost),
                        mgmt_stanzas_in => 0,
                        mgmt_stanzas_out => 0,
                        mgmt_stanzas_req => 0}}
    end;
s2s_out_stream_init(Acc, _Opts) ->
    Acc.

s2s_out_stream_features(#{mgmt_state := MgmtState,
                          mgmt_timeout := Resume,
                          mgmt_queue_type := QueueType} = State, 
                        #stream_features{sub_els = SubEls}) ->
    case check_stream_mgmt_support(SubEls) of
        Xmlns when Xmlns == ?NS_STREAM_MGMT_2; Xmlns == ?NS_STREAM_MGMT_3 ->
            case MgmtState of
                inactive -> 
                    State1 =                   
                        if Resume > 0 ->
                                send(State, #sm_enable{xmlns = Xmlns,
                                                       resume = true,
                                                       max = Resume});
                           true ->
                                send(State, #sm_enable{xmlns = Xmlns})
                        end,
                    State1#{mgmt_xmlns => Xmlns,
                            mgmt_state => wait_for_enabled,
                            mgmt_queue => p1_queue:new(QueueType)};
                _ -> % resume
                    #{mgmt_stanzas_in := H, mgmt_privid := Id} = State,
                    State1 = send(State, #sm_resume{h = H, previd = Id, xmlns = Xmlns}),
                    State1#{mgmt_xmlns => Xmlns, mgmt_state => pending}
            end;
        _ ->
            State
    end;
s2s_out_stream_features(State, _) ->
    State.

s2s_out_established(#{mgmt_state := resume} = State) ->
    % register connection ?
    #{mgmt_stanzas_in := H, mgmt_privid := Id} = State,
    State1 = send(State, #sm_resume{h = H, previd = Id, xmlns = ?NS_STREAM_MGMT_3}),
    State1#{mgmt_xmlns => ?NS_STREAM_MGMT_3, mgmt_state => pending};
s2s_out_established(#{mgmt_timeout := Resume,
                      mgmt_queue_type := QueueType} = State) ->
    Xmlns = ?NS_STREAM_MGMT_3,
    State1 = 
        if Resume > 0 ->
                send(State, #sm_enable{xmlns = Xmlns,
                                       resume = true,
                                       max = Resume});
           true ->
                send(State, #sm_enable{xmlns = Xmlns})
        end,

    State1#{mgmt_state => wait_for_enabled,
            mgmt_xmlns => Xmlns,
            mgmt_queue => p1_queue:new(QueueType)}; 
s2s_out_established(State) ->
    State.

s2s_out_packet(#{mgmt_state := pending} = State, #sm_resumed{} = Pkt) ->
    {stop, handle_resumed(Pkt, State)};
s2s_out_packet(#{mgmt_state := MgmtState} = State, Pkt) 
  when ?is_sm_packet(Pkt) ->
    if MgmtState == active -> % ; pending
            {stop, perform_stream_mgmt(Pkt, State)};
       MgmtState == wait_for_enabled ->
            {stop, negotiate_stream_mgmt(Pkt, State)};
       true ->
            {stop, State}
    end;
s2s_out_packet(State, Pkt) ->
    update_num_stanzas_in(State, Pkt).

s2s_out_handle_recv(#{mgmt_state := wait_for_enabled,
                      remote_server := RServer} = State, _El, #sm_failed{}) ->
    ?DEBUG("Remote server ~s can't enable stream management", [RServer]),
    State#{mgmt_state => inactive};
s2s_out_handle_recv(#{mgmt_state := pending,
                      remote_server := RServer,
                      mgmt_old_session := OldState} = State, _El, #sm_failed{}) ->
    ?DEBUG("Remote server ~s can't resume previous session", [RServer]),
    State;
s2s_out_handle_recv(#{lang := Lang} = State, El, {error, Why}) ->
    Xmlns = xmpp:get_ns(El),
    if Xmlns == ?NS_STREAM_MGMT_2; Xmlns == ?NS_STREAM_MGMT_3 ->
            Txt = xmpp:io_format_error(Why),
            Err = #sm_failed{reason = 'bad-request',
                             text = xmpp:mk_text(Txt, Lang),
                             xmlns = Xmlns},
            send(State, Err);
       true ->
            State
    end;
s2s_out_handle_recv(State, El, Pkt) ->
    State.
 
s2s_out_handle_send(#{mgmt_state := MgmtState, lang := Lang} = State, Pkt, SendResult)
  when MgmtState == active; MgmtState == wait_for_enabled ->
    case Pkt of
        _ when ?is_stanza(Pkt) ->
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
    % State1 = Mod:close(State),
    % {stop, transition_to_resume(State1)};
s2s_out_handle_info(#{mgmt_state := resume,
                      remote_server := RServer, mod := Mod} = State,
                     {timeout, TRef, connection_timeout}) ->
    ?DEBUG("Timed out waiting for connection "
           "establishment with ~s for resumption", [RServer]),
    % Mod:stop(State),
    State;
s2s_out_handle_info(State, _) ->
    State.

s2s_out_closed(#{mgmt_state := resume, mod := Mod} = State, _) ->
    {stop, transition_to_resume(State#{stream_state => connecting})};
% s2s_out_closed(#{mgmt_state := active} = State, _) ->
%     State;
s2s_out_closed(State, _) ->
    State.

% terminate - Mod:stop
% fix: route messages if timeout = 0 or in pending state

% s2s_out_terminate(#{mgmt_state := resumed} = State, _Reason) ->
%     {stop, State};
% s2s_out_terminate(#{mgmt_state := resume} = State, _Reason) ->
%     % bounce_errors(State),
%     State;
% s2s_out_terminate(#{mgmt_state := active} = State, _Reason) ->
%     transition_to_resume(State);

s2s_out_terminate(State, _Reason) ->
    State.

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

% mgmt_state := active, pending and pkt is sm packet
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

-spec negotiate_stream_mgmt(xmpp_element(), state()) -> state().
negotiate_stream_mgmt(Pkt, #{mgmt_xmlns := Xmlns, 
                             mgmt_state := MgmtState} = State) ->
    case Pkt of
        #sm_enabled{} ->
            handle_enabled(State, Pkt);
        _ when is_record(Pkt, sm_r);
               is_record(Pkt, sm_a) ->
            Err = #sm_failed{reason = 'unexpected-request', xmlns = Xmlns},
            send(State, Err);
        _ ->
            Err = #sm_failed{reason = 'bad-request', xmlns = Xmlns},
            send(State, Err)
    end.

-spec handle_resumed(sm_resumed(), state()) -> state().
handle_resumed(#sm_resumed{h = H, previd = Id}, 
               #{mgmt_xmlns := Xmlns, remote_server := RServer,
                 mgmt_old_session := OldState, mod := Mod} = State) ->

    State1 = check_h_attribute(State, H),

    State2 = resend_unacked_stanzas(State1),

    State3 = send(State2#{mgmt_state => resumed}, #sm_r{xmlns = Xmlns}),

    ?DEBUG("Resumed session with ~s", [RServer]),

    {ok, State3}.

-spec resend_unacked_stanzas(state()) -> state().
resend_unacked_stanzas(#{mgmt_state := MgmtState,
                         mgmt_queue := Queue,
                         remote_server := RServer} = State) 
    when MgmtState == pending andalso ?qlen(Queue) > 0 ->
    p1_queue:foldl(
        fun({_, _Time, Pkt}, AccState) ->
            % set is_resent =  true like in mod_stream_mgmt
            send(AccState, Pkt)
        end, State, Queue);
resend_unacked_stanzas(State) -> 
    State.

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
                    State#{mgmt_privid => Id};
                true ->
                    ?INFO_MSG("Stream management enabled for ~s", [RServer]),
                    State
             end,
    
    State2 = 
        case not p1_queue:is_empty(Queue) of
            true ->
                send_rack(State1);
            _ ->
                State1
        end,

    State2#{mgmt_state => active, mgmt_timeout => Timeout}.

-spec handle_r(state()) -> state().
handle_r(#{mgmt_stanzas_in := H,
           mgmt_xmlns := Xmlns} = State) ->
    send(State, #sm_a{h = H, xmlns = Xmlns}).

-spec handle_a(state(), sm_a()) -> state().
handle_a(#{mgmt_stanzas_out := NumStanzasOut, 
           remote_server := RServer} = State, #sm_a{h = H}) ->
    State1 = check_h_attribute(State, H),
    resend_rack(State1).

-spec transition_to_resume(state()) -> state().
transition_to_resume(#{mgmt_state := active, mod := Mod,
                       mgmt_timeout := 0} = State) ->
    State; % route messages from queue
transition_to_resume(#{mgmt_state := active, mod := Mod,
                       remote_server := RServer,server_host := Server,
                       mgmt_connection_timeout := Timeout} = State) ->
    State1 = mod_stream_mgmt:cancel_ack_timer(State),
    ?DEBUG("Try to connect to remote server ~s", [RServer]),
    {ok, Pid} = 
        ejabberd_s2s:start_connection(jid:make(Server), 
                                      jid:make(RServer),
                                      [{resume, State1}]),

    erlang:start_timer(Timeout, Pid, connection_timeout),

    State1;
transition_to_resume(#{mgmt_state := resume,
                       mod := Mod,
                       remote_server := RServer} = State) ->    
    Mod:connect(self()),
    State;
transition_to_resume(State) ->
    State.

%% fix: filter some messages

bounce_errors(#{mgmt_state := resume,
                mgmt_queue := Queue} = State) 
  when ?qlen(Queue) > 0 ->
    p1_queue:foreach(
        fun({_, _, Pkt}) ->
                Error = xmpp:err_remote_server_timeout(),
                ejabberd_router:route_error(Pkt, Error)
        end, Queue);
bounce_errors(State) ->
    ok.

-spec route_unacked_stanzas(state()) -> state().
route_unacked_stanzas(#{mgmt_queue := Queue, 
                        mgmt_state := pending, 
                        mgmt_xmlns := Xmlns} = State) 
  when ?qlen(Queue) > 0 ->
    State1 = send(State, #sm_enable{xmlns = Xmlns}),
    p1_queue:foldl(
        fun({_, _, Pkt}, AccState) ->
                % set is_resend = true like in mod_stream_mgmt
                send(AccState, Pkt)
        end, State1, Queue);
route_unacked_stanzas(_State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% todo: import from mod_stream_mgmt.erl

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
    State1 = State, % mod_stream_mgmt:cancel_ack_timer(State),
    case NumStanzasReq < NumStanzasOut andalso not p1_queue:is_empty(Queue) of
        true -> send_rack(State1);
        false -> State1
    end;
resend_rack(State) ->
    State.

-spec update_num_stanzas_in(state(), xmpp_element()) -> state().
update_num_stanzas_in(#{mgmt_state := MgmtState,
                        mgmt_stanzas_in := NumStanzasIn} = State, El)
  when MgmtState == active ->
    NewNum = case {xmpp:is_stanza(El), NumStanzasIn} of
         {true, 4294967295} ->
             0;
         {true, Num} ->
             Num + 1;
         {false, Num} ->
             Num
         end,
    State#{mgmt_stanzas_in => NewNum};
update_num_stanzas_in(State, _El) ->
    State.

%%%=============================================================================
%%% Configuration processing
%%%=============================================================================

get_resume_timeout(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, resume_timeout, 300).

get_queue_type(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, queue_type) of
        undefined -> ejabberd_config:default_queue_type(Host);
        Type -> Type
    end.

get_max_ack_queue(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, max_ack_queue, 1000).

get_ack_timeout(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, ack_timeout, 10) of % change default
        infinity -> infinity;
        T -> timer:seconds(T)
    end.

get_max_unacked_stanzas(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, max_unacked_stanzas, 0).

get_connection_timeout(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, connection_timeout, 120000).

mod_opt_type(connection_timeout) ->
    fun(I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(max_unacked_stanzas) ->
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
mod_opt_type(_) -> [max_ack_queue, ack_timeout, resume_timeout,
                    queue_type, max_unacked_stanzas, connection_timeout].
