%%%----------------------------------------------------------------------
%%% File    : ejabberd_service.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : External component management (XEP-0114)
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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

-module(ejabberd_service).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-protocol({xep, 114, '1.6'}).

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% External exports
-export([start/2, start_link/2, send_text/2,
         send_element/2, socket_type/0, transform_listen_option/2]).

-export([init/1, wait_for_stream/2,
         wait_for_handshake/2, stream_established/2,
         handle_event/3, handle_sync_event/4, code_change/4,
         handle_info/3, terminate/3, print_state/1, opt_type/1]).

-export([process_presence/4, process_roster_presence/3,
         get_delegated_ns/1]).


-include("ejabberd_service.hrl").

-include("mod_privacy.hrl").

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

-define(STREAM_HEADER,
        <<"<?xml version='1.0'?><stream:stream "
          "xmlns:stream='http://etherx.jabber.org/stream"
          "s' xmlns='jabber:component:accept' id='~s' "
          "from='~s'>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_HEADER_ERR,
        <<"<stream:stream xmlns:stream='http://etherx.ja"
          "bber.org/streams'><stream:error>Invalid "
          "Stream Header</stream:error></stream:stream>">>).

-define(INVALID_HANDSHAKE_ERR,
        <<"<stream:error><not-authorized xmlns='urn:ietf"
          ":params:xml:ns:xmpp-streams'/><text "
          "xmlns='urn:ietf:params:xml:ns:xmpp-streams' "
          "xml:lang='en'>Invalid Handshake</text></strea"
          "m:error></stream:stream>">>).

-define(INVALID_XML_ERR,
        fxml:element_to_binary(?SERR_XML_NOT_WELL_FORMED)).

-define(INVALID_NS_ERR,
        fxml:element_to_binary(?SERR_INVALID_NAMESPACE)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_service_sup,
                           [SockData, Opts]).

start_link(SockData, Opts) ->
    (?GEN_FSM):start_link(ejabberd_service,
                          [SockData, Opts], fsm_limit_opts(Opts) ++ (?FSMOPTS)).

socket_type() -> xml_stream.

get_delegated_ns(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef, {get_delegated_ns}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([{SockMod, Socket}, Opts]) ->
    ?INFO_MSG("(~w) External service connected", [Socket]),
    Access = case lists:keysearch(access, 1, Opts) of
                 {value, {_, A}} -> A;
                 _ -> all
             end,
    HostOpts = case lists:keyfind(hosts, 1, Opts) of
                   {hosts, HOpts} ->
                       lists:foldl(
                         fun({H, Os}, D) ->
                                 P = proplists:get_value(
                                       password, Os,
                                       p1_sha:sha(crypto:rand_bytes(20))),
                                 dict:store(H,P,D)
                         end, dict:new(), HOpts);
                   false ->
                       Pass = proplists:get_value(password, Opts,
                                          p1_sha:sha(crypto:rand_bytes(20))),
                       dict:from_list([{global,Pass}])
               end,
    %% Component can have privilege access to entities with server domain
    %%  in ServerHosts domain list 
    ServerHosts = case lists:keysearch(server_hosts, 1, Opts) of
                      {value, {_, H}} -> H;
                      _ -> ?MYHOSTS
                  end,
    %% privilege access to entities data
    PrivAccess = case lists:keysearch(privilege_access, 1, Opts) of
                     {value, {_, PrivAcc}} -> PrivAcc;
                     _ -> []
                 end,
    Delegations = case lists:keyfind(delegations, 1, Opts) of
                      {delegations, Del} ->
                          lists:foldl(
                            fun({Ns, FiltAttr}, D) ->
                                Attr = proplists:get_value(filtering,
                                                           FiltAttr, []),
                                D ++ [{Ns, Attr}]
                            end, [], Del); 
                      false -> []
                  end, 
    Shaper = case lists:keysearch(shaper_rule, 1, Opts) of
                 {value, {_, S}} -> S;
                 _ -> none
             end,
    CheckFrom = case lists:keysearch(service_check_from, 1, Opts) of
                    {value, {_, CF}} -> CF;
                    _ -> true
                end,
    SockMod:change_shaper(Socket, Shaper),

    %% add host option to config file
    add_hooks(ServerHosts),

    case ets:info(registered_services) of
        undefined ->
            %% table contains {service domain, Pid}
            ets:new(registered_services, [named_table, public]);
        _ ->
            ok
    end,
    {ok, wait_for_stream,
     #state{socket = Socket, sockmod = SockMod,
            streamid = new_id(), host_opts = HostOpts, access = Access,
            check_from = CheckFrom, server_hosts = ServerHosts,
            privilege_access = PrivAccess, delegations = Delegations}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
      <<"jabber:component:accept">> ->
          To = fxml:get_attr_s(<<"to">>, Attrs),
          Host = jid:nameprep(To),
          if Host == error ->
                  Header = io_lib:format(?STREAM_HEADER,
                                         [<<"none">>, ?MYNAME]),
                  send_text(StateData,
                            <<(list_to_binary(Header))/binary,
                              (?INVALID_XML_ERR)/binary,
                              (?STREAM_TRAILER)/binary>>),
                  {stop, normal, StateData};
             true ->
                  Header = io_lib:format(?STREAM_HEADER,
                                         [StateData#state.streamid, fxml:crypt(To)]),
                  send_text(StateData, Header),
                  HostOpts = case dict:is_key(Host, StateData#state.host_opts) of
                                  true ->
                                      StateData#state.host_opts;
                                  false ->
                                      case dict:find(global, StateData#state.host_opts) of
                                          {ok, GlobalPass} ->
                                              dict:from_list([{Host, GlobalPass}]);
                                          error ->
                                              StateData#state.host_opts
                                      end
                             end,
                  {next_state, wait_for_handshake,
                   StateData#state{host = Host, host_opts = HostOpts}}
          end;
      _ ->
          send_text(StateData, ?INVALID_HEADER_ERR),
          {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    Header = io_lib:format(?STREAM_HEADER,
                           [<<"none">>, ?MYNAME]),
    send_text(StateData,
              <<(list_to_binary(Header))/binary, (?INVALID_XML_ERR)/binary,
                (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_handshake({xmlstreamelement, El}, StateData) ->
    #xmlel{name = Name, children = Els} = El,
    case {Name, fxml:get_cdata(Els)} of
        {<<"handshake">>, Digest} ->
            case dict:find(StateData#state.host, StateData#state.host_opts) of
                {ok, Password} ->
                    case p1_sha:sha(<<(StateData#state.streamid)/binary,
                                       Password/binary>>) of
                        Digest ->
                            send_text(StateData, <<"<handshake/>">>),
                            lists:foreach(
                                fun (H) ->
                                  ejabberd_router:register_route(H, ?MYNAME),
                                  ?INFO_MSG("Route registered for service ~p~n", [H])
                                end, dict:fetch_keys(StateData#state.host_opts)),

                            advertise_perm(StateData),
                            namespace_delegation:advertise_delegations(StateData),
                            %% send initial presences from all server users
                            case get_prop(presence, StateData#state.privilege_access) of
                                Priv when (Priv == <<"managed_entity">>) or
                                          (Priv == <<"roster">>) ->
                                        initial_presence(StateData);
                                _ -> ok
                            end,
                            ets:insert(registered_services, {StateData#state.host, self()}), 
                            {next_state, stream_established, StateData}; 
                        _ ->

                            send_text(StateData, ?INVALID_HANDSHAKE_ERR),
                            {stop, normal, StateData}
                    end;
                _ ->
                    send_text(StateData, ?INVALID_HANDSHAKE_ERR),
                    {stop, normal, StateData}
            end;
        _ -> {next_state, wait_for_handshake, StateData}
    end;
wait_for_handshake({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
wait_for_handshake({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(?INVALID_XML_ERR)/binary,
                (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_handshake(closed, StateData) ->
    {stop, normal, StateData}.

stream_established({xmlstreamelement, El}, StateData) ->
    % ?INFO_MSG(" message from comp ~p~n" , [El]),
    NewEl = jlib:remove_attr(<<"xmlns">>, El),
    #xmlel{name = Name, attrs = Attrs} = NewEl,
    From = fxml:get_attr_s(<<"from">>, Attrs),
    FromJID = case StateData#state.check_from of
                %% If the admin does not want to check the from field
                %% when accept packets from any address.
                %% In this case, the component can send packet of
                %% behalf of the server users.
                false -> jid:from_string(From);
                %% The default is the standard behaviour in XEP-0114
                _ ->
                    FromJID1 = jid:from_string(From),
                    case FromJID1 of
                      #jid{lserver = Server} ->
                          case dict:is_key(Server, StateData#state.host_opts) of
                            true -> FromJID1;
                            false -> error
                          end;
                      _ -> error
                    end
              end,
    To = fxml:get_attr_s(<<"to">>, Attrs),
    ToJID = case To of
              <<"">> -> error;
              _ -> jid:from_string(To) 
            end,

    if  (Name == <<"iq">>) and (ToJID /= error) and (FromJID /= error) ->
            process_iq(StateData, FromJID,ToJID, NewEl);                  
        (Name == <<"presence">>) and (ToJID /= error) and (FromJID /= error) ->
            ejabberd_router:route(FromJID, ToJID, NewEl);
        (Name == <<"message">>) and (ToJID /= error) and (FromJID /= error) ->
            process_message(StateData, FromJID,ToJID, NewEl);
        true ->
            Lang = fxml:get_tag_attr_s(<<"xml:lang">>, El),
            Txt = <<"Incorrect stanza name or from/to JID">>,
            Err = jlib:make_error_reply(NewEl, ?ERRT_BAD_REQUEST(Lang, Txt)),
            send_element(StateData, Err),
            error
    end,
    {next_state, stream_established, StateData};
stream_established({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(?INVALID_XML_ERR)/binary,
                (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    {stop, normal, StateData}.

%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event({get_delegated_ns}, _From, stream_established, StateData) ->
    Reply =  StateData#state.delegations,
    {reply, Reply, stream_established, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok, {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({send_element, El}, StateName, StateData) ->
    send_element(StateData, El),
    {next_state, StateName, StateData};
handle_info({route, From, To, Packet}, StateName,
        StateData) ->
    case acl:match_rule(global, StateData#state.access, From) of
      allow ->
          #xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
          Attrs2 =
              jlib:replace_from_to_attrs(jid:to_string(From),
                                         jid:to_string(To), Attrs),
          Text = fxml:element_to_binary(#xmlel{name = Name,
                                               attrs = Attrs2, children = Els}),
          send_text(StateData, Text);
      deny ->
          Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
          Txt = <<"Denied by ACL">>,
          Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ALLOWED(Lang, Txt)),
          ejabberd_router:route_error(To, From, Err, Packet)
    end,
    {next_state, StateName, StateData};

handle_info({user_presence, Packet, FromJID}, 
            stream_established, StateData) ->
    AccessType = get_prop(presence, StateData#state.privilege_access),
    case AccessType of
        T when (T == <<"managed_entity">>) or
               (T == <<"roster">> ) ->
            lists:foreach(fun (H) ->
                            ToJID = jid:from_string(H),
                            PacketNew = 
                                jlib:replace_from_to(FromJID, ToJID, Packet),
                            send_element(StateData, PacketNew)
                          end, dict:fetch_keys(StateData#state.host_opts));
        _ -> ok
    end,
    {next_state, stream_established, StateData};

handle_info({roster_presence, From, Packet}, 
            stream_established, StateData) ->
    AccessType = get_prop(presence, StateData#state.privilege_access),
    RosterAccessType = get_prop(roster, StateData#state.privilege_access),
    case {AccessType, RosterAccessType} of
        {P, R} when (P == <<"roster">>) and 
                    ((R == <<"both">>) or (R == <<"get">>)) ->
            %% check that current presence stanza is equivalent to last
            NewPacket = jlib:remove_attr(<<"to">>, Packet),
            Dict = StateData#state.last_pres,
            LastPresence = 
                try dict:fetch(From, Dict)
                catch _:_ -> 
                    undefined
                end,
            case compare_presences(LastPresence, NewPacket) of
                false ->
                    lists:foreach(fun (H) ->
                                    PacketNew = replace_to(H, Packet),
                                    send_element(StateData, PacketNew)
                                  end, 
                                  dict:fetch_keys(StateData#state.host_opts));
                _ ->
                    ok
            end,
            DictNew = dict:store(From, NewPacket, Dict),
            StateDataNew = StateData#state{last_pres = DictNew},
            {next_state, stream_established, StateDataNew};
        _ -> 
            {next_state, stream_established, StateData}    
    end;

handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    ?INFO_MSG("terminated: ~p", [Reason]),
    case StateName of
      stream_established ->
          lists:foreach(fun (H) ->
                            ejabberd_router:unregister_route(H)
                        end,
                        dict:fetch_keys(StateData#state.host_opts)),
          ets:delete(registered_services, StateData#state.host);
      _ -> ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) -> State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket,Text).

send_element(StateData, El) ->
    send_text(StateData, fxml:element_to_binary(El)).

new_id() -> randoms:get_string().

transform_listen_option({hosts, Hosts, O}, Opts) ->
    case lists:keyfind(hosts, 1, Opts) of
        {_, PrevHostOpts} ->
            NewHostOpts =
                lists:foldl(
                  fun(H, Acc) ->
                          dict:append_list(H, O, Acc)
                  end, dict:from_list(PrevHostOpts), Hosts),
            [{hosts, dict:to_list(NewHostOpts)}|
             lists:keydelete(hosts, 1, Opts)];
        _ ->
            [{hosts, [{H, O} || H <- Hosts]}|Opts]
    end;
transform_listen_option({host, Host, Os}, Opts) ->
    transform_listen_option({hosts, [Host], Os}, Opts);
transform_listen_option(Opt, Opts) ->
    [Opt|Opts].

fsm_limit_opts(Opts) ->
    case lists:keysearch(max_fsm_queue, 1, Opts) of
        {value, {_, N}} when is_integer(N) ->
            [{max_queue, N}];
        _ ->
            case ejabberd_config:get_option(
                   max_fsm_queue,
                   fun(I) when is_integer(I), I > 0 -> I end) of
                undefined -> [];
                N -> [{max_queue, N}]
            end
    end.

opt_type(max_fsm_queue) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(_) -> [max_fsm_queue].

-spec get_prop(atom(), list()) -> atom().
get_prop(Prop, Props ) ->
    proplists:get_value(Prop, Props, none).

%% add to jlib 
-spec replace_to_attr(binary(), [attr()]) -> [attr()].

replace_to_attr(To, Attrs) ->
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = [{<<"to">>, To} | Attrs1],
    Attrs2.

-spec replace_to(binary(), xmlel()) -> xmlel().

replace_to(To, #xmlel{name = Name, attrs = Attrs, children = Els}) ->
    NewAttrs = 
        replace_to_attr(To, Attrs),
    #xmlel{name = Name, attrs = NewAttrs, children = Els}.

%%------------------------------------------------------------------------
%% XEP-0356

add_hooks(Hosts) ->
    lists:foreach(fun(Host) -> add_hooks2(Host) end, Hosts).

add_hooks2(Host) ->
    %% these hooks are used for receiving presences for privilege services XEP-0356
    ejabberd_hooks:add(user_send_packet, Host,
                       ?MODULE, process_presence, 10),
    ejabberd_hooks:add(user_send_packet, Host,
                       namespace_delegation, process_packet, 10),
    ejabberd_hooks:add(s2s_receive_packet, Host,
                       ejabberd_service, process_roster_presence, 10).

-spec permissions(binary(), list()) -> #xmlel{}.
permissions(Id, PrivAccess) ->
    Perms = lists:map(fun({Access, Type}) -> 
                          #xmlel{name = <<"perm">>, 
                                 attrs = [{<<"access">>, 
                                           atom_to_binary(Access,latin1)},
                                          {<<"type">>, Type}]}
                      end, PrivAccess),
    Stanza = #xmlel{name = <<"privilege">>, 
                    attrs = [{<<"xmlns">> ,?NS_PRIVILEGE}],
                    children = Perms},
    #xmlel{name = <<"message">>, attrs = [{<<"id">>, Id}], children = [Stanza]}.
    
advertise_perm(StateData) ->
    case StateData#state.privilege_access of 
        [] -> ok;
        PrivAccess ->
            Stanza = permissions(StateData#state.streamid, PrivAccess),
            lists:foreach(fun (Host) ->
                            #xmlel{attrs = Attrs} = Stanza,
                            Attrs2 =
                                jlib:replace_from_to_attrs(hd(StateData#state.server_hosts),
                                                           Host, Attrs),
                            send_element(StateData, Stanza#xmlel{attrs = Attrs2}),
                            ?INFO_MSG("Advertise service of allowed permissions ~p~n",[Host])
                          end, dict:fetch_keys(StateData#state.host_opts))
    end.

process_iq(StateData, FromJID, ToJID, Packet) ->
    IQ = jlib:iq_query_or_response_info(Packet),
    case IQ of 
        #iq{xmlns = ?NS_ROSTER} ->
            case (ToJID#jid.luser /= <<"">>) and
                 lists:member(ToJID#jid.lserver, StateData#state.server_hosts) and
                 %% only component.host, or user@component.host ?
                 (FromJID#jid.luser == <<"">>) of 
                true ->
                    AccessType = get_prop(roster, StateData#state.privilege_access),
                    Type = IQ#iq.type,
                    case {Type, AccessType} of
                        {get, T} when ( (T == <<"both">>) or (T == <<"get">>)) -> 
                            Roster = mod_roster:process_iq(ToJID,ToJID, IQ),
                            send_iq_result(StateData, ToJID,
                                           FromJID, jlib:iq_to_xml(Roster));
                        {set, T} when ( (T == <<"both">>) or (T == <<"set">>)) ->
                            %% check if user ToJID  exist
                            #jid{lserver= Server, luser = User} = ToJID,
                            case ejabberd_auth:is_user_exists(User,Server) of
                                true ->
                                    Access = 
                                      gen_mod:get_module_opt(Server, mod_roster, access,
                                                             fun(A) when is_atom(A) -> A 
                                                             end, all),
                                    case acl:match_rule(Server, Access, ToJID) of
                                        deny ->
                                            ok;
                                        allow ->
                                            ResIQ = mod_roster:process_iq(ToJID, ToJID, IQ),
                                            send_iq_result(StateData, ToJID, 
                                                           FromJID, jlib:iq_to_xml(ResIQ))
                                    end;
                                _ ->
                                    ok 
                            end;
                        _ ->
                            Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN), 
                            send_element(StateData, Err)
                    end;
                _ ->
                    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN), 
                    send_element(StateData, Err)
            end;
        #iq{type = Type, id = Id} when (Type == error) or (Type == result) ->
            case (ToJID#jid.luser == <<"">>) and
                 lists:member(ToJID#jid.lserver, StateData#state.server_hosts) and
                 (FromJID#jid.luser == <<"">>) of
                true ->
                    Hook = hook_name(Type, Id),
                    ejabberd_hooks:run(Hook, ToJID#jid.lserver, [Packet]);
                _ ->
                    ejabberd_router:route(FromJID, ToJID, Packet)
            end;
        _ ->
            
            ejabberd_router:route(FromJID, ToJID, Packet)
    end.

-spec hook_name(atom(), binary()) -> atom().

hook_name(Type, Id) ->
    Hook0 =  atom_to_binary(Type, 'latin1'),
    Hook = << <<"iq_">>/binary, Hook0/binary, Id/binary >>,
    binary_to_atom(Hook, 'latin1').

send_iq_result(StateData, From, To, #xmlel{attrs = Attrs } = Packet) ->
    Attrs2 = jlib:replace_from_to_attrs(jid:to_string(From),
                                        jid:to_string(To), Attrs),
    Text = fxml:element_to_binary(Packet#xmlel{attrs = Attrs2}),
    send_text(StateData, Text).

process_message(StateData, FromJID, ToJID, #xmlel{children = Children} = Packet) ->
%% if presence was send from service to server,
    PrivAccess = StateData#state.privilege_access,
    case lists:member(ToJID#jid.lserver, StateData#state.server_hosts) and
         (ToJID#jid.luser == <<"">>) and
         (FromJID#jid.luser == <<"">>) of %% service
        true -> 
            %% if stanza contains privilege element
            case Children of
                [#xmlel{name = <<"privilege">>, 
                        attrs = [{<<"xmlns">>, ?NS_PRIVILEGE}],
                        children = [#xmlel{name = <<"forwarded">>,
                                           attrs = [{<<"xmlns">>, ?NS_FORWARD}],
                                           children = Children2}]}] ->
                            %% 1 case : privilege service send subscription message
                            %% on behalf of the client
                            %% 2 case : privilege service send message on behalf
                            %% of the client
                            case Children2 of
                                %% it isn't case of 0356 extension
                                [#xmlel{name = <<"presence">>} = Child] ->
                                    forward_subscribe(StateData, Child, PrivAccess , Packet);
                                [#xmlel{name = <<"message">>} = Child] -> %% xep-0356
                                    forward_message(StateData, Child, PrivAccess , Packet);
                                _ -> 
                                    Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
                                    Txt = <<"invalid forwarded element">>,
                                    Err = 
                                     jlib:make_error_reply(Packet, 
                                                           ?ERRT_BAD_REQUEST(Lang, Txt)),
                                    send_element(StateData, Err)

                            end;
                _ ->
                    ejabberd_router:route(FromJID, ToJID, Packet)
            end;

        _ ->
            ejabberd_router:route(FromJID, ToJID, Packet)
    end.

forward_subscribe(StateData, Presence, PrivAccess, Packet) ->
    T = get_prop(roster, PrivAccess),
    Type = fxml:get_attr_s(<<"type">>, Presence#xmlel.attrs),
    if  ((T == <<"both">>) or (T == <<"set">>)) and
        (Type == <<"subscribe">>) ->
            From = fxml:get_attr_s(<<"from">>, Presence#xmlel.attrs),
            FromJ = jid:from_string(From),
            To = fxml:get_attr_s(<<"to">>, Presence#xmlel.attrs),
            ToJ = case To of 
                      <<"">> -> error;
                      _ -> jid:from_string(To)
                  end,
            if  (ToJ /= error) and (FromJ /= error) ->
                    Server = FromJ#jid.lserver,
                    User = FromJ#jid.luser,
                    case (FromJ#jid.lresource == <<"">>) and 
                         lists:member(Server, StateData#state.server_hosts) of
                        true ->
                            if  (Server /= ToJ#jid.lserver) or
                                (User /= ToJ#jid.luser) ->
                                    %% 0356 server MUST NOT allow the privileged entity
                                    %% to do anything that the managed entity could not do
                                    try_roster_subscribe(Server,User, FromJ,
                                                         ToJ, Presence);   
                                true -> %% we don't want presence sent to self
                                    ok
                            end;
                        _ ->
                            Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
                            send_element(StateData, Err)
                    end;
                true ->
                    Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
                    Txt = <<"Incorrect stanza from/to JID">>,
                    Err = 
                     jlib:make_error_reply(Packet, 
                                           ?ERRT_BAD_REQUEST(Lang, Txt)),
                    send_element(StateData, Err)
            end;
        true ->
            Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
            send_element(StateData, Err)
    end.

forward_message(StateData, Message, PrivAccess, Packet) ->
    T = get_prop(message, PrivAccess),
    if  (T == <<"outgoing">>) ->            
            From = fxml:get_attr_s(<<"from">>, Message#xmlel.attrs),
            FromJ = jid:from_string(From),
            To = fxml:get_attr_s(<<"to">>, Message#xmlel.attrs),
            ToJ = case To of 
                      <<"">> -> FromJ;
                      _ -> jid:from_string(To)
                  end,
            if  (ToJ /= error) and (FromJ /= error) ->
                    Server = FromJ#jid.server,
                    User = FromJ#jid.user,
                    case (FromJ#jid.lresource == <<"">>) and 
                         lists:member(Server, StateData#state.server_hosts) of
                        true ->
                            %% there are no restriction on to attribute
                            PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
                                                               Server, #userlist{},
                                                               [User, Server]),
                            check_privacy_route(Server, User, PrivList,
                                                FromJ, ToJ, Message);
                        _ ->
                            Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
                            send_element(StateData, Err)
                    end;
                true ->
                    Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
                    Txt = <<"Incorrect stanza from/to JID">>,
                    Err = 
                     jlib:make_error_reply(Packet, 
                                           ?ERRT_BAD_REQUEST(Lang, Txt)),
                    send_element(StateData, Err)
            end;
        true ->
            Err = jlib:make_error_reply(Packet,?ERR_FORBIDDEN),
            send_element(StateData, Err)
    end.

initial_presence(StateData) ->
    Pids = ejabberd_sm:get_all_pids(),
    lists:foreach(fun(Pid) ->
                    {User, Server, Resource, PresenceLast} = 
                        ejabberd_c2s:get_last_presence(Pid),
                    case lists:member(Server, StateData#state.server_hosts) of
                        true ->
                            From = #jid{user = User,
                                        server = Server,
                                        resource = Resource},
                            lists:foreach(fun (H) ->
                                            To = jid:from_string(H),
                                            PacketNew = 
                                                jlib:replace_from_to(From,To, PresenceLast),
                                            send_element(StateData, PacketNew)
                                          end, 
                                          dict:fetch_keys(StateData#state.host_opts));
                        _ -> ok
                    end
                  end, Pids).

%% hook user_send_packet(Packet, C2SState, From, To) -> Packet
%% for Managed Entity Presence
process_presence(#xmlel{name = <<"presence">>} = Packet, _C2SState, From, _To) ->
    case fxml:get_attr_s(<<"type">>, Packet#xmlel.attrs) of
        T when (T == <<"">>) or (T == <<"unavailable">>) ->
            case ets:info(registered_services) of
                undefined -> ok;
                _ ->
                    lists:foreach(fun({_ServiceHost, Pid}) -> 
                                      Pid ! {user_presence, Packet, From} 
                                  end,
                                  ets:tab2list(registered_services))
            end;                   
        _ -> ok
    end,
    Packet;
process_presence(Packet, _C2SState, _From, _To) ->
    Packet.

%% s2s_receive_packet(From, To, Packet) -> ok
%% for Roster Presence
%% From subscription "from" or "both"
process_roster_presence(From, To, #xmlel{name = <<"presence">>} = Packet) ->
    case fxml:get_attr_s(<<"type">>, Packet#xmlel.attrs) of
        T when (T == <<"">>) or (T == <<"unavailable">>) ->
            case ets:info(registered_services) of
                undefined -> ok;
                _ ->
                    Server = To#jid.server,
                    User = To#jid.user,
                    PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
                                                       Server, #userlist{},
                                                       [User, Server]),
                    case privacy_check_packet(Server, User, PrivList,
                                              From, To, Packet, in) of
                        allow ->
                            lists:foreach(fun({_ServiceHost, Pid}) -> 
                                              Pid ! {roster_presence, From, Packet}
                                  end,
                                  ets:tab2list(registered_services));
                        _ -> ok

                    end
            end;
        _ -> ok
    end;
process_roster_presence(_From, _To, _Packet) -> ok.
               
compare_presences(undefined, _Presence) -> false;                       
compare_presences(#xmlel{attrs = Attrs, children = Child},
                  #xmlel{attrs = Attrs2, children = Child2}) ->
    Id1 = fxml:get_attr_s(<<"id">>, Attrs),
    Id2 = fxml:get_attr_s(<<"id">>, Attrs2),
    case not compare_arrts(Attrs, Attrs2) of
        true -> false;
        _ -> 
            case (Id1 /= <<"">>) and (Id1 == Id2) of
                true -> true;
                _ -> 
                    compare_elements(Child, Child2)
            end
    end.

compare_elements([],[]) -> true;
compare_elements(Tags1, Tags2) when length(Tags1) == length(Tags2) ->
    compare_tags(Tags1,Tags2);
compare_elements(_Tags1, _Tags2) -> false.

compare_tags([],[]) -> true;
compare_tags([{xmlcdata, CData}|Tags1], [{xmlcdata, CData}|Tags2]) ->
    compare_tags(Tags1, Tags2);
compare_tags([{xmlcdata, _CData1}|_Tags1], [{xmlcdata, _CData2}|_Tags2]) ->
    false;
compare_tags([#xmlel{} = Stanza1|Tags1], [#xmlel{} = Stanza2|Tags2]) ->
    case (Stanza1#xmlel.name == Stanza2#xmlel.name) and
        compare_arrts(Stanza1#xmlel.attrs, Stanza2#xmlel.attrs) and
        compare_tags(Stanza1#xmlel.children, Stanza2#xmlel.children) of
        true -> 
            compare_tags(Tags1,Tags2);
        false ->
            false
    end.

%% attr() :: {Name, Value}
-spec compare_arrts([attr()], [attr()]) -> boolean().
compare_arrts([],[]) -> true;
compare_arrts(Attrs1, Attrs2) when length(Attrs1) == length(Attrs2) ->
    lists:foldl(fun(Attr,Acc) -> lists:member(Attr, Attrs2) and Acc end, true, Attrs1);
compare_arrts(_Attrs1, _Attrs2) -> false.

%% Check if privacy rules allow this delivery
%% from ejabberd_c2s.erl
privacy_check_packet(Server, User, PrivList, From, To, Packet , Dir) ->
    ejabberd_hooks:run_fold(privacy_check_packet,
                            Server, allow, [User, Server, PrivList, 
                            {From, To, Packet}, Dir]).

check_privacy_route(Server, User, PrivList, From, To, Packet) ->
    case privacy_check_packet(Server, User, PrivList, From, To, Packet, out) of
        allow ->
            ejabberd_router:route(From, To, Packet);
        _ -> ok %% who should receive error : service or user?
    end.

try_roster_subscribe(Server,User, From, To, Packet) ->
    Access = 
      gen_mod:get_module_opt(Server, mod_roster, access,
                             fun(A) when is_atom(A) -> A end, all),
    case acl:match_rule(Server, Access, From) of
        deny ->
            ok;
        allow ->
            ejabberd_hooks:run(roster_out_subscription, Server,
                               [User, Server, To, subscribe]),
            PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
                                               Server,
                                               #userlist{},
                                               [User, Server]),
            check_privacy_route(Server, User, PrivList, From, To, Packet)            
    end.
