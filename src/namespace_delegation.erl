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
                                 attrs = [{<<"namespace">>, Ns}],
                                 children = attribute_tag(FilterAttr)}
                      end, Delegations),
    Elem1 = #xmlel{name = <<"delegation">>, 
                     attrs = [{<<"namespace">>, ?NS_DELEGATION}],
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

check_filter_attr([], _StanzaAttr) -> true;
check_filter_attr(_FilterAttr, []) -> false;
check_filter_attr(FilterAttr, StanzaAttr) ->
    lists:all(fun(Attr) ->
                  lists:member(Attr, StanzaAttr)
              end, FilterAttr).

check_delegation([], _Ns, _StanzaAttr) -> false;
check_delegation(Delegations, Ns, StanzaAttr) ->
    case lists:keysearch(Ns, 1, Delegations) of
        {value, {Ns, FilterAttr}} ->
            check_filter_attr(FilterAttr, StanzaAttr);
    	false-> false
    end.

 -spec forward_iq(binary(), binary(), #xmlel{}) -> ok.   
forward_iq(Server, Service, Packet) ->
    Elem0 = #xmlel{name = <<"forwarded">>,
                   attrs = [{<<"xmlns">>, ?NS_FORWARD}], children = [Packet]},
    Elem1 = #xmlel{name = <<"delegation">>, 
                   attrs = [{<<"xmlns">>, ?NS_DELEGATION}], children = [Elem0]},
    ID = randoms:get_string(),
    Elem2 = #xmlel{name = <<"iq">>,
                   attrs = [{<<"from">>, Server}, {<<"to">>, Service},
                            {<<"type">>, <<"set">>}, {<<"id">>, ID}],
                   children = [Elem1]},

    % Hook = << <<"iq_result_">>/binary, IQ_ID/binary >>,

    % Process_ent_result = fun(From, PacketRes) ->
    %                          if (From == Service ) -> 

    % ejabberd_hooks:add(binary_to_atom(Hook, 'latin1'), Server, Process_ent_result, 10),
    % ?INFO_MSG("send iq ~p~n", [Elem2]),

    From = jid:make(<<"">>, Server, <<"">>),
    To = jid:make(<<"">>, Service, <<"">>),
    ejabberd_router:route(From, To, Elem2).


% decapsulate_result(#xmlel{children = []]} = Packet) -> ok;
% decapsulate_result(#xmlel{children = Children} = Packet) ->
%     decapsulate_result0(Children).

% decapsulate_result0([]) -> ok;
% decapsulate_result0([#xmlel{name = <<"delegation">>, 
% 	                        attrs = [{<<"namespace">>, ?NS_DELEGATION}]} = Packet]) ->
%     decapsulate_result1(Packet#xmlel.children);
% decapsulate_result0(Packet) -> ok.

% decapsulate_result1([]) -> ok;
% decapsulate_result1([#xmlel{name = <<"forwarded">>,
%                             attrs = [{<<"xmlns">>, ?NS_FORWARD}], 
%                             children = []}]) -> ok;
% decapsulate_result1([#xmlel{name = <<"forwarded">>,
%                             attrs = [{<<"xmlns">>, ?NS_FORWARD}], 
%                             children = Children}]) -> Children;
% decapsulate_result1(Packet) -> ok.



%% hook user_send_packet(Packet, C2SState, From, To) -> Packet
process_packet(#xmlel{name = <<"iq">>, attrs = Attrs} = Packet, _C2SState, From, To) ->
    Type = fxml:get_attr_s(<<"type">>, Packet#xmlel.attrs),
    IQ = jlib:iq_query_info(Packet),
    CheckTab = 
        case ets:info(registered_services) of
            undefined ->
                false;
            _ ->
                true
        end,
    %% check if stanza directed to server
    %% or directed to the bare JID of the sender
    case ((From#jid.user == To#jid.user) and
       	  (From#jid.server == To#jid.server) or
          lists:member(To#jid.server, ?MYHOSTS)) and
         ((Type == <<"get">>) or (Type == <<"set">>)) and CheckTab of
        true ->
            lists:foreach(fun({ServiceHost, Pid}) -> 
            	    	      Delegations = 
            	    	        ejabberd_service:get_delegated_ns(Pid),
                                case check_delegation(Delegations,
                                      	              IQ#iq.xmlns,
                                      	              IQ#iq.sub_el) of
                                    true ->
                                        AttrsNew = 
                                            Attrs ++ [{<<"xmlns">>, 
                                                       <<"jabber:client">>}],
                                        AttrsNew2 = 
                                            jlib:replace_from_to_attrs(
                                                jid:to_string(From),
                                                jid:to_string(To), AttrsNew),
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