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


-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-record(state,
        {socket :: ejabberd_socket:socket_state(),
         sockmod = ejabberd_socket :: ejabberd_socket | ejabberd_frontend_socket,
         streamid = <<"">>         :: binary(),
         host_opts = dict:new()    :: ?TDICT,
         host = <<"">>             :: binary(),
         access                    :: atom(),
         check_from = true         :: boolean()}).

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

-define(NS_PRIVILEGE, <<"urn:xmpp:privilege:1">>).

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
                 P = proplists:get_value(password, Os,
                                         p1_sha:sha(crypto:rand_bytes(20))),
                  %% privilege access to  entities data
                 PrivAccess = proplists:get_value(privilege_access, Os, []),
                 dict:store(H,[{password, P}] ++ PrivAccess, D)
             end, dict:new(), HOpts);
           false ->
               Pass = proplists:get_value(password, Opts,
                                          p1_sha:sha(crypto:rand_bytes(20))),
               PrivAccess = proplists:get_value(privilege_access, Opts, []),
               
               dict:from_list([{global, [{password, Pass}] ++ PrivAccess}])
           end,
    Shaper = case lists:keysearch(shaper_rule, 1, Opts) of
           {value, {_, S}} -> S;
           _ -> none
         end,
    CheckFrom = case lists:keysearch(service_check_from, 1,
                     Opts)
            of
          {value, {_, CF}} -> CF;
          _ -> true
        end,
    SockMod:change_shaper(Socket, Shaper),
    {ok, wait_for_stream,
     #state{socket = Socket, sockmod = SockMod,
        streamid = new_id(), host_opts = HostOpts,
        access = Access, check_from = CheckFrom}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, _Name, Attrs},
        StateData) ->
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
                                          {ok, Props} ->
                                              dict:from_list([{Host, Props}]);
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
                {ok, HostProps} ->
                    Password = get_prop(password, HostProps),
                    case p1_sha:sha(<<(StateData#state.streamid)/binary,
                                       Password/binary>>) of
                        Digest ->
                            send_text(StateData, <<"<handshake/>">>),
                            lists:foreach(
                              fun (H) ->
                                      ejabberd_router:register_route(H, ?MYNAME),
                                      ?INFO_MSG("Route registered for service ~p~n",
                                                [H])
                              end, dict:fetch_keys(StateData#state.host_opts)), 
                            %% Why we need to register all hosts ?
                            %% TODO: do it for each host in dict if it is necessary
                            %% server advertises service of allowed permission
                            advertise_perm(StateData, proplists:delete(password,HostProps)),

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
    %% TODO: check conditions        

    %% xep-0356 : The server MUST check that the privileged entity has
    %% right to get or set the roster of managed entity, 
    %% and MUST return a <forbidden/> error if it is not the case

    if  (Name == <<"iq">>) and (ToJID /= error) and (FromJID /= error) ->
            IQ = jlib:iq_query_info(NewEl),
            case IQ of
                #iq{xmlns = ?NS_ROSTER} ->
                    case (ToJID#jid.luser /= <<"">>) and
                         (ToJID#jid.lserver == ?MYNAME) and %% ?MYHOSTS
                         %% only component.host, or user@component.host ?
                         (FromJID#jid.luser == <<"">>) of 
                             true ->
                                 process_iq(StateData, FromJID,ToJID, NewEl);
                             false -> 
                                 ejabberd_router:route(FromJID, ToJID, NewEl)
                    end;
                 _ ->
                     ejabberd_router:route(FromJID, ToJID, NewEl)
            end;
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
handle_sync_event(_Event, _From, StateName,
          StateData) ->
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
                        dict:fetch_keys(StateData#state.host_opts));
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

%%------------------------------------------------------------------------
%% XEP-0356

-spec permissions(binary(), binary(), list()) -> #xmlel{}.
permissions(Id, Host, HostOpts) ->
    Stanza = #xmlel{name = <<"message">>, 
                    attrs = [{<<"from">>,?MYNAME},
                             {<<"to">>, Host},
                             {<<"id">>, Id}]},
    Stanza0 = #xmlel{name = <<"privilege">>, 
                     attrs = [{<<"xmlns">> ,?NS_PRIVILEGE}]},
    Perms = lists:map(fun({Access, Type}) -> 
                              #xmlel{name = <<"perm">>, 
                                     attrs = [{<<"access">>, 
                                               atom_to_binary(Access,latin1)},
                                              {<<"type">>, Type}]}
                          end, HostOpts),
    Stanza1 = Stanza0#xmlel{children = Perms},
    Stanza#xmlel{children = [Stanza1]}.
    

advertise_perm(StateData, HostOpts) ->
    case HostOpts of 
        [] -> ok;
        _ -> 
            Stanza = permissions(StateData#state.streamid,
                                 StateData#state.host, HostOpts),
            Text = fxml:element_to_binary(Stanza),
            send_text(StateData, Text),
            ?INFO_MSG("Advertise service of allowed permissions ~p~n",
                     [StateData#state.host])
    end.

%% TODO:  maybe add hook ? Anyway organize work in another module
process_iq(StateData, FromJID, ToJID, Packet) ->
    %% check privileges
    {ok, HOpts} = dict:find(StateData#state.host, StateData#state.host_opts),
    AccessType = get_prop(roster, HOpts),
    Type = fxml:get_attr_s(<<"type">>, Packet#xmlel.attrs),
    IQ = jlib:iq_query_info(Packet),
    case {Type, AccessType} of
        {<<"get">>, T} when ( (T == <<"both">>) or (T == <<"get">>)) -> 
            Roster = mod_roster:process_iq(ToJID,ToJID, IQ),
            send_iq_result(StateData, ToJID, FromJID, jlib:iq_to_xml(Roster));
        {<<"set">>, T} when ( (T == <<"both">>) or (T == <<"set">>)) ->
            %% check if user ToJID  exist
            #jid{lserver= Server, luser = User} = ToJID,
            case ejabberd_auth:is_user_exists(User,Server) of
                true ->
                    ResIQ = mod_roster:process_iq(ToJID, ToJID, IQ),
                    send_iq_result(StateData, ToJID, FromJID, jlib:iq_to_xml(ResIQ));
                false -> 
                    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN), %%  RFC 6121 2.1.5
                    send_element(StateData, Err)
            end;
        _ ->
            Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN), %%  RFC 6121 2.1.5
            send_element(StateData, Err)
    end.
   
send_iq_result(StateData, From, To, #xmlel{attrs = Attrs } = Packet) ->
    Attrs2 = jlib:replace_from_to_attrs(jid:to_string(From),
                                        jid:to_string(To), Attrs),
    Text = fxml:element_to_binary(Packet#xmlel{attrs = Attrs2}),
    send_text(StateData, Text).

%% TODO : rewrite this function 
process_message(StateData, FromJID, ToJID, #xmlel{children = Children} = Packet) ->
%% if presence was send from service to server,
    {ok, HOpts} = dict:find(StateData#state.host, StateData#state.host_opts),
    case (ToJID#jid.lserver == ?MYNAME) and %% ?in ?MYHOSTS
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
                                    %% check privilege access
                                    T = get_prop(roster, HOpts),
                                    Type = fxml:get_attr_s(<<"type">>, Child#xmlel.attrs),
                                    if  ((T == <<"both">>) or (T == <<"set">>)) and 
                                        (Type == <<"subscribe">>) ->
                                            %% we can send presence on behalf
                                            %% of the server client
                                            %% now send presence anyway
                                            %% TODO: check is it important to 
                                            %% filter presence messages
                                            From = fxml:get_attr_s(<<"from">>, 
                                                                   Child#xmlel.attrs),
                                            To = fxml:get_attr_s(<<"to">>, 
                                                                 Child#xmlel.attrs),
                                            FromJ = jid:from_string(From),
                                            ToJ = jid:from_string(To),

                                            if (FromJ /= ToJ) ->
                                                  case (FromJ#jid.lresource == <<"">>) and 
                                                       (FromJ#jid.lserver == ?MYNAME) of
                                                            true ->
                                                                ejabberd_router:route(FromJ,ToJ, Child);
                                                            _ ->
                                                                Err = 
                                                                  jlib:make_error_reply(Child,
                                                                                        ?ERR_FORBIDDEN),
                                                                send_element(StateData, Err)
                                                  end;
                                                true ->
                                                    ok %% we don't want presence sent to self
                                            end;
                                        true ->
                                            Err = jlib:make_error_reply(Child, ?ERR_FORBIDDEN),
                                            send_element(StateData, Err)
                                    end;        
                                [#xmlel{name = <<"message">>} = Child] -> 
                                    T = get_prop(message, HOpts),
                                    ChildNew = jlib:remove_attr(<<"xmlns">>, Child),
                                    if  (T == <<"outgoing">>) ->
                                            %% xep-0356
                                            From = fxml:get_attr_s(<<"from">>, 
                                                                   ChildNew#xmlel.attrs),
                                            To = fxml:get_attr_s(<<"to">>, 
                                                                 ChildNew#xmlel.attrs),
                                            FromJ = jid:from_string(From),
                                            ToJ = jid:from_string(To),                            
                                            case (FromJ#jid.lresource == <<"">>) and 
                                                 (FromJ#jid.lserver == ?MYNAME) of %% check it
                                                      true ->
                                                          %% there are no restriction on to attribute
                                                          ejabberd_router:route(FromJ,ToJ, ChildNew); 
                                                      _ ->
                                                          Err = 
                                                           jlib:make_error_reply(ChildNew,
                                                                                 ?ERR_FORBIDDEN),
                                                          send_element(StateData, Err)
                                            end;
                                        true ->
                                            Err = jlib:make_error_reply(ChildNew,?ERR_FORBIDDEN),
                                            send_element(StateData, Err)
                                    end;
                                _ -> 
                                    ejabberd_router:route(FromJID, ToJID, Packet)
                            end;
                _ ->
                    ejabberd_router:route(FromJID, ToJID, Packet)
            end;

        false ->
            ejabberd_router:route(FromJID, ToJID, Packet)
    end.