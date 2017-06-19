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
    ejabberd_hooks:add(s2s_out_terminate,
                       Host, ?MODULE, s2s_out_terminate, 50),
    % delete after implementation of server part
    ejabberd_hooks:delete(s2s_out_established,
                          Host, ?MODULE, s2s_out_established, 50).

reload(_Host, _NewOpts, _OldOpts) ->
    ?WARNING_MSG("module ~s is reloaded, but new configuration will take "
                 "effect for newly created s2s connections only", [?MODULE]).

depends(_Host, _Opts) -> [].

s2s_out_stream_init({ok, #{server_host := ServerHost} = State}, Opts) ->
    case proplists:get_value(resume, Opts) of
        OldState when OldState /= undefined ->
            {ok, State#{mgmt_state => resume, mgmt_old_session => OldState}};
        _ ->
            Resume = get_resume_timeout(ServerHost),
            MaxResume = get_max_resume_timeout(ServerHost, Resume),
            {ok, State#{mgmt_state => inactive,
                        mgmt_timeout => Resume,
                        mgmt_max_timeout => MaxResume,
                        mgmt_queue_type => get_queue_type(ServerHost),
                        mgmt_max_queue => get_max_ack_queue(ServerHost),
                        mgmt_ack_timeout => get_ack_timeout(ServerHost),
                        mgmt_unacked_stanzas => get_max_unacked_stanzas(ServerHost),
                        mgmt_stanzas_in => 0,
                        mgmt_stanzas_out => 0,
                        mgmt_stanzas_req => 0}}
    end;
s2s_out_stream_init(Acc, _Opts) ->
    Acc.

s2s_out_stream_features(#{mgmt_state := MgmtState,
                          mgmt_timeout := Resume,
                          mgmt_max_timeout := MaxResume,
                          mgmt_old_session := OldState,
                          mgmt_queue_type := QueueType} = State, 
                        #stream_features{sub_els = SubEls}) ->
    case check_stream_mgmt_support(SubEls, <<>>) of
        Xmlns when Xmlns == ?NS_STREAM_MGMT_2; Xmlns == ?NS_STREAM_MGMT_3 ->
            case MgmtState of
                inactive -> 
                    State1 =                   
                        if Resume > 0 ->
                                send(State, #sm_enable{xmlns = Xmlns,
                                                       resume = true,
                                                       max = MaxResume});
                           true ->
                                send(State, #sm_enable{xmlns = Xmlns})
                        end,
                    State1#{mgmt_xmlns => Xmlns,
                            mgmt_state => wait_for_enabled,
                            mgmt_queue => p1_queue:new(QueueType)};
                _ -> % resume
                    #{mgmt_privid := Id, mgmt_stanzas_in := H} = OldState,
                    State1 = send(State, #sm_resume{h = H, previd = Id, xmlns = Xmlns}),
                    State1#{mgmt_xmlns := Xmlns, mgmt_state => pending}
            end;
        _ ->
            State
    end;
s2s_out_stream_features(State, _) ->
    State.

s2s_out_established(#{mgmt_state := resume,
                      mgmt_old_session := OldState} = State) ->
    #{mgmt_privid := Id, mgmt_stanzas_in := H} = OldState,
    State1 = send(State, #sm_resume{h = H, previd = Id, xmlns = ?NS_STREAM_MGMT_3}),
    State1#{mgmt_state => pending};
s2s_out_established(#{mgmt_timeout := Resume,
                      mgmt_max_timeout := MaxResume,
                      mgmt_queue_type := QueueType} = State) ->
    Xmlns = ?NS_STREAM_MGMT_3,
    State1 = 
        if Resume > 0 ->
                send(State, #sm_enable{xmlns = Xmlns,
                                       resume = true,
                                       max = MaxResume});
           true ->
                send(State, #sm_enable{xmlns = Xmlns})
        end,

    State1#{mgmt_state => wait_for_enabled,
            mgmt_xmlns => Xmlns,
            mgmt_queue => p1_queue:new(QueueType)}; 
s2s_out_established(State) ->
    State.

s2s_out_packet(#{mgmt_state := MgmtState} = State, #sm_resumed{} = Pkt) ->
    handle_resumed(Pkt, State);
s2s_out_packet(#{mgmt_state := MgmtState} = State, Pkt) 
  when ?is_sm_packet(Pkt) ->
    if MgmtState == active; MgmtState == pending ->
            {stop, perform_stream_mgmt(Pkt, State)};
       MgmtState == wait_for_enabled ->
            {stop, negotiate_stream_mgmt(Pkt, State)};
       true ->
            {stop, State}
    end;
% s2s_out_packet(#{mgmt_state := wait_for_enabled,
%                  remote_server := RServer} = State, #sm_failed{}) ->
%     ?DEBUG("Remote server ~s can't enable stream management", [RServer]),
%     State#{mgmt_state => inactive};
% s2s_out_packet(#{mgmt_state := pending,
%                  remote_server := RServer, mod := Mod} = State, #sm_failed{}) ->
%     ?DEBUG("Remote server ~s can't resume previous session", [RServer]),
%     Mod:stop(State#{mgmt_state => resume_failed}); % temp session
s2s_out_packet(State, Pkt) ->
    update_num_stanzas_in(State, Pkt).

% s2s_out_handle_recv(#{lang := Lang} = State, El, {error, Why}) ->
%     Xmlns = xmpp:get_ns(El),
%     ?INFO_MSG("kjuh~p", [El]),
%     if Xmlns == ?NS_STREAM_MGMT_2; Xmlns == ?NS_STREAM_MGMT_3 ->
%             Txt = xmpp:io_format_error(Why),
%             Err = #sm_failed{reason = 'bad-request',
%                              text = xmpp:mk_text(Txt, Lang),
%                              xmlns = Xmlns},
%             send(State, Err);
%        true ->
%             State
%     end;
s2s_out_handle_recv(State, El, Pkt) ->
    State.
 
s2s_out_handle_send(#{mgmt_state := MgmtState, lang := Lang} = State, Pkt, ok)
  when MgmtState == active; MgmtState == wait_for_enabled ->
    case Pkt of
        _ when ?is_stanza(Pkt) ->
            case mgmt_queue_add(State, Pkt) of
                #{mgmt_max_queue := exceeded} = State1 ->
                    Err = xmpp:serr_policy_violation(
                                <<"Too many unacked stanzas">>, Lang),
                    send(State1, Err);
                State1 when MgmtState == active ->
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
    ?DEBUG("Timed out waiting for stream management acknowledgement of ~p", [RServer]),
    State1 = Mod:close(State),
    {stop, new_connection(State1)};
s2s_out_handle_info(State, _) ->
    State.

% handle_stream_end Mod:close 
s2s_out_closed(State, {stream, _}) ->
    State;
s2s_out_closed(#{mgmt_state := active, mod := Mod} = State, _) ->
    {stop, new_connection(State)};
s2s_out_closed(State, _) ->
    State.

% terminate - Mod:stop 
s2s_out_terminate(#{mgmt_state := resumed} = State, _Reason) ->
    {stop, State};
s2s_out_terminate(#{mgmt_state := MgmtState,
                    mgmt_old_session := OldState} = State, _Reason) ->

    % route_unacked_stanzas(State),
    State;
s2s_out_terminate(State, _Reason) ->
    State.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

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
               #{mgmt_xmlns := Xmlns, mgmt_old_session := OldState} = State) ->
    
    % session is resumed

    #{mgmt_stanzas_out := NumStanzasOut, mgmt_queue := Queue} = OldState,

    State1 = check_h_attribute(State#{mgmt_stanzas_out => NumStanzasOut,
                                      mgmt_state => resumed,
                                      mgmt_queue => Queue}, H),

    State2 = resend_unacked_stanzas(State1),

    {ok, send(State2, #sm_r{xmlns = Xmlns})}.

-spec resend_unacked_stanzas(state()) -> state().
resend_unacked_stanzas(#{mgmt_state := MgmtState,
                         mgmt_queue := Queue,
                         remote_server := RServer} = State) 
    when MgmtState == pending andalso ?qlen(Queue) > 0 ->
    p1_queue:foldl(
        fun({_, Time, Pkt}, AccState) ->
            send(AccState, Pkt)
        end, State, Queue);
resend_unacked_stanzas(State) -> 
    State.

-spec handle_enabled(state(), sm_enabled()) -> state().
handle_enabled(#{remote_server := RHost,
                 mgmt_timeout :=  DefaultTimeout,
                 mgmt_max_timeout := MaxTimeout,
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
                              "resumption enabled for ~s", [RHost]),
                    State#{mgmt_privid => Id};
                true ->
                    ?INFO_MSG("Stream management enabled for ~s", [RHost]),
                    State
             end,
    % Do we need it?
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
    check_h_attribute(State, H).

-spec new_connection(state()) -> state().
new_connection(#{mgmt_state := active, mod := Mod,
                 mgmt_timeout := 0} = State) ->
    Mod:stop(State);
% we could stop this connection after resumption
new_connection(#{remote_server := RServer, server_host := Server,
                 mgmt_stanzas_in := H,mgmt_privid := Id} = State) ->
    {ok, Pid} = ejabberd_s2s_out:start(Server, RServer, [{resume, State}]),
    ejabberd_s2s_out:connect(Pid),
    State;
new_connection(State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% todo: import from mod_stream_mgmt.erl

-spec check_h_attribute(state(), non_neg_integer()) -> state().
check_h_attribute(#{mgmt_stanzas_out := NumStanzasOut,
                    remote_server := RServer} = State, H)
  when H > NumStanzasOut ->
    ?DEBUG("~s acknowledged ~B stanzas," 
           "but only ~B were sent ", [RServer, H, NumStanzasOut]),
    mgmt_queue_drop(State#{mgmt_stanzas_out => H}, NumStanzasOut);
check_h_attribute(#{mgmt_stanzas_out := NumStanzasOut,
                    remote_server := RServer} = State, H) ->
    ?DEBUG("~s acknowledged ~B of ~B "
           "stanzas", [RServer, H, NumStanzasOut]),
    mgmt_queue_drop(State, H).

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

-spec mgmt_queue_add(state(), xmpp_element()) -> state().
mgmt_queue_add(#{mgmt_stanzas_out := NumStanzasOut,
                 mgmt_queue := Queue} = State, Pkt) ->
    NewNum = case NumStanzasOut of
                 4294967295 -> 0;
                 Num -> Num + 1
             end,
    Queue1 = p1_queue:in({NewNum, p1_time_compat:timestamp(), Pkt}, Queue),
    State1 = State#{mgmt_queue => Queue1, mgmt_stanzas_out => NewNum},
    check_queue_length(State1).

-spec mgmt_queue_drop(state(), non_neg_integer()) -> state().
mgmt_queue_drop(#{mgmt_queue := Queue} = State, NumHandled) ->
    NewQueue = p1_queue:dropwhile(
                 fun({N, _T, _E}) -> N =< NumHandled end, Queue),
    State#{mgmt_queue => NewQueue}.

-spec check_queue_length(state()) -> state().
check_queue_length(#{mgmt_max_queue := Limit} = State)
  when Limit == infinity; Limit == exceeded ->
    State;
check_queue_length(#{mgmt_queue := Queue, mgmt_max_queue := Limit} = State) ->
    case p1_queue:len(Queue) > Limit of
        true ->
            State#{mgmt_max_queue => exceeded};
        false ->
            State
    end.

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

-spec cancel_ack_timer(state()) -> state().
cancel_ack_timer(#{mgmt_ack_timer := TRef} = State) ->
    case erlang:cancel_timer(TRef) of
        false -> 
            receive {timeout, TRef, _} -> ok
            after 0 -> ok
            end;
        _ ->
            ok
    end,
    maps:remove(mgmt_ack_timer, State);
cancel_ack_timer(State) ->
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
    case gen_mod:get_module_opt(Host, ?MODULE, ack_timeout, 10) of
        infinity -> infinity;
        T -> timer:seconds(T)
    end.

get_max_unacked_stanzas(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, max_unacked_stanzas, 0).

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
                    max_resume_timeout, queue_type, max_unacked_stanzas ].
