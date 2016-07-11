-module(namespace_delegation).

-author('amuhar3@gmail.com').

-protocol({xep, 0355, '0.2.1'}).

-export([advertise_delegations/1, process_packet/4 ]).

-include("ejabberd_service.hrl").

%%%--------------------------------------------------------------------------------------
%%%  server advertise delegated namespaces 4.2
%%%--------------------------------------------------------------------------------------
attribute_tag([]) -> [];
attribute_tag(Attrs) ->
    lists:map(fun(Attr) ->
                  #xmlel{name = <<"attribute">>, 
                         attrs = [{<<"name">> , Attr}]}
              end, Attrs).

delegations(Id, Delegations) ->
    Elem0 = lists:map(fun({Ns, FilterAttr}) ->
                          #xmlel{name = <<"delegated">>, 
                                 attrs = [{<<"xmlns">>, Ns}],
                                 children = attribute_tag(FilterAttr)}
                      end, Delegations),
    Elem1 = #xmlel{name = <<"delegation">>, 
                     attrs = [{<<"xmlns">>, ?NS_DELEGATION}],
                     children = Elem0},
    #xmlel{name = <<"message">>, attrs = [{<<"id">>, Id}], children = [Elem1]}.

delegation_ns_debug(Host, Delegations) ->
    lists:foreach(fun({Ns, []}) ->
    	              ?DEBUG("namespace ~s is delegated to ~s with"
    	                     " no filtering attributes",[Ns, Host]);
    	             ({Ns, Attr}) ->
                      ?DEBUG("namespace ~s is delegated to ~s with"
    	                     " ~p filtering attributes ~n",[Ns, Host, Attr])
    	          end, Delegations).
advertise_delegations(#state{delegations = []}) -> ok;
advertise_delegations(StateData) ->
    Delegated = delegations(StateData#state.streamid, StateData#state.delegations),
    lists:foreach(fun(H) ->
    	              Attrs =
    	                  jlib:replace_from_to_attrs(hd(StateData#state.server_hosts),
    	                                             H, Delegated#xmlel.attrs),
                      ejabberd_service:send_element(StateData, 
                      	                            Delegated#xmlel{attrs = Attrs}),
                      delegation_ns_debug(H,StateData#state.delegations)
                  end, dict:fetch_keys(StateData#state.host_opts)).

%%%--------------------------------------------------------------------------------------
%%%  Delegated namespaces hook
%%%--------------------------------------------------------------------------------------

check_filter_attr([], _Children) -> true;
check_filter_attr(_FilterAttr, []) -> false;
check_filter_attr(FilterAttr, [#xmlel{} = Stanza|_]) ->
    Attrs = proplists:get_keys(Stanza#xmlel.attrs),
    lists:all(fun(Attr) ->
                  lists:member(Attr, Attrs)
              end, FilterAttr);
check_filter_attr(_FilterAttr, _Children) -> false.

check_delegation([], _Ns, _Children) -> false;
check_delegation(Delegations, Ns, Children) ->
    case lists:keysearch(Ns, 1, Delegations) of
        {value, {Ns, FilterAttr}} ->
            check_filter_attr(FilterAttr, Children);
    	  false-> false
    end.

check_tab() ->
    case ets:info(registered_services) of
      undefined ->
          false;
      _ ->
          true
    end.

-spec get_client_server([attr()]) -> {jid(), jid()}. 

get_client_server(Attrs) ->
    Client = fxml:get_attr_s(<<"from">>, Attrs),
    ClientJID = jid:from_string(Client),
    ServerJID = jid:from_string(ClientJID#jid.lserver),
    {ClientJID, ServerJID}.

-spec hook_name(binary(), binary()) -> atom().

hook_name(Name, Id) ->
    Hook = << Name/binary, Id/binary >>,
    binary_to_atom(Hook, 'latin1').

decapsulate_result(#xmlel{children = []}) -> ok;
decapsulate_result(#xmlel{children = Children}) ->
    decapsulate_result0(Children).

decapsulate_result0([]) -> ok;
decapsulate_result0([#xmlel{name = <<"delegation">>, 
                          attrs = [{<<"xmlns">>, ?NS_DELEGATION}]} = Packet]) ->
    decapsulate_result1(Packet#xmlel.children);
decapsulate_result0(_Children) -> ok.

decapsulate_result1([]) -> ok;
decapsulate_result1([#xmlel{name = <<"forwarded">>,
                            attrs = [{<<"xmlns">>, ?NS_FORWARD}]} = Packet]) ->
    decapsulate_result2(Packet#xmlel.children);
decapsulate_result1(_Children) -> ok.

decapsulate_result2([]) -> ok;
decapsulate_result2([#xmlel{name = <<"iq">>, attrs = Attrs} = Packet]) ->
    Ns = fxml:get_attr_s(<<"xmlns">>, Attrs),
    if
      Ns /= <<"jabber:client">> ->
        ok;
      true -> Packet
    end;
decapsulate_result2(_Children) -> ok.

-spec check_iq(#xmlel{}, #xmlel{}) -> #xmlel{} | ignore.

check_iq(#xmlel{attrs = Attrs} = Packet,
         #xmlel{attrs = AttrsOrigin} = OriginPacket) ->
    % Id attribute of OriginPacket Must be equil to Packet Id attribute
    Id1 = fxml:get_attr_s(<<"id">>, Attrs),
    Id2 = fxml:get_attr_s(<<"id">>, AttrsOrigin),
    % From attribute of OriginPacket Must be equil to Packet To attribute
    From = fxml:get_attr_s(<<"from">>, AttrsOrigin),
    To = fxml:get_attr_s(<<"from">>, Attrs),
    % Type attribute Must be error or result
    Type = fxml:get_attr_s(<<"type">>, Attrs),
    if
      ((Type == <<"result">>) or (Type == <<"error">>)),
        Id1 == Id2,  
        To == From ->
        NewPacket = jlib:remove_attr(<<"xmlns">>, Packet),
        %% We can send the decapsulated stanza from Server to Client (To)
        NewPacket;
      true ->
        %% service-unavailable
        Err = jlib:make_error_reply(OriginPacket, ?ERR_SERVICE_UNAVAILABLE),
        Err
    end;
check_iq(_Packet, _OriginPacket) -> ignore.


-spec manage_service_result(atom(), atom(), binary(), #xmlel{}) -> ok.

manage_service_result(HookRes, HookErr, Service, OriginPacket) ->
    fun(Packet) ->,
        {ClientJID, ServerJID} = get_client_server(OriginPacket#xmlel.attrs),
        Server = ClientJID#jid.lserver,
        ejabberd_hooks:delete(HookRes, Server, 
                              manage_service_result(HookRes, HookErr,
                                                    Service, OriginPacket), 10),
        ejabberd_hooks:delete(HookErr, Server, 
                              manage_service_error(HookRes, HookErr,
                                                   Service, OriginPacket), 10),
        % Check Packet from attribute
        % It Must be equil to current service host
        From = fxml:get_attr_s(<<"from">> , Packet#xmlel.attrs),
        if
          From == Service  ->
              % decapsulate iq result
              ResultIQ = decapsulate_result(Packet),
              ServResponse = check_iq(ResultIQ, OriginPacket),
              if
                ServResponse /= ignore ->
                  ejabberd_router:route(ServerJID, ClientJID, ServResponse);
                true -> ok
              end;
          true ->
              % service unavailable
              Err = jlib:make_error_reply(OriginPacket, ?ERR_SERVICE_UNAVAILABLE),
              ejabberd_router:route(ServerJID, ClientJID, Err) 
        end       
    end.

-spec manage_service_error(atom(), atom(), binary(), #xmlel{}) -> ok.

manage_service_error(HookRes, HookErr, Service, OriginPacket) ->
    fun(_Packet) ->
        {ClientJID, ServerJID} = get_client_server(OriginPacket#xmlel.attrs),
        Server = ClientJID#jid.lserver,
        ejabberd_hooks:delete(HookRes, Server, 
                              manage_service_result(HookRes, HookErr,
                                                    Service, OriginPacket), 10),
        ejabberd_hooks:delete(HookErr, Server, 
                              manage_service_error(HookRes, HookErr,
                                                   Service, OriginPacket), 10),
        Err = jlib:make_error_reply(OriginPacket, ?ERR_SERVICE_UNAVAILABLE),
        ejabberd_router:route(ServerJID, ClientJID, Err)        
    end.


-spec forward_iq(binary(), binary(), #xmlel{}) -> ok.

forward_iq(Server, Service, Packet) ->
    Elem0 = #xmlel{name = <<"forwarded">>,
                   attrs = [{<<"xmlns">>, ?NS_FORWARD}], children = [Packet]},
    Elem1 = #xmlel{name = <<"delegation">>, 
                   attrs = [{<<"xmlns">>, ?NS_DELEGATION}], children = [Elem0]},
    Id = randoms:get_string(),
    Elem2 = #xmlel{name = <<"iq">>,
                   attrs = [{<<"from">>, Server}, {<<"to">>, Service},
                            {<<"type">>, <<"set">>}, {<<"id">>, Id}],
                   children = [Elem1]},

    HookRes = hook_name(<<"iq_result">>, Id),
    HookErr = hook_name(<<"iq_error">>, Id),

    FunRes = manage_service_result(HookRes, HookErr, Service, Packet),
    FunErr = manage_service_error(HookRes, HookErr, Service, Packet),

    ejabberd_hooks:add(HookRes, Server, FunRes, 10),
    ejabberd_hooks:add(HookErr, Server, FunErr, 10),

    From = jid:make(<<"">>, Server, <<"">>),
    To = jid:make(<<"">>, Service, <<"">>),
    ejabberd_router:route(From, To, Elem2).

%% hook user_send_packet(Packet, C2SState, From, To) -> Packet
process_packet(#xmlel{name = <<"iq">>, attrs = Attrs,
                      children = Children} = Packet, _C2SState, From, To) ->
    Type = fxml:get_attr_s(<<"type">>, Packet#xmlel.attrs),
    %% check if stanza directed to server
    %% or directed to the bare JID of the sender
    case ((From#jid.user == To#jid.user) and
       	  (From#jid.server == To#jid.server) or
          lists:member(To#jid.server, ?MYHOSTS)) and
         ((Type == <<"get">>) or (Type == <<"set">>)) and check_tab() of
        true ->
            AttrsNew = [{<<"xmlns">>, <<"jabber:client">>} | 
                        lists:keydelete(<<"xmlns">>, 1, Attrs)],
            AttrsNew2 = jlib:replace_from_to_attrs(jid:to_string(From),
                                                   jid:to_string(To), AttrsNew),
            Ns = jlib:get_iq_namespace(Packet),
            lists:foreach(fun({ServiceHost, Pid}) -> 
            	    	          Delegations = 
            	    	              ejabberd_service:get_delegated_ns(Pid),
                              case check_delegation(Delegations,
                                                    Ns,
                                                    Children) of
                                  true ->
                                      forward_iq(From#jid.server, ServiceHost,
                                                 Packet#xmlel{attrs = AttrsNew2});
                                  _ -> ok
                              end
                          end,
                          ets:tab2list(registered_services)),
            Packet;
        _ -> Packet
    end;
process_packet(Packet, _C2SState, _From, _To) ->
    Packet.