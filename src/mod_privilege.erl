%%%--------------------------------------------------------------------------------------
%%% File    : mod_privilege.erl
%%% Author  : Anna Mukharram <amuhar3@gmail.com>
%%% Purpose : This module is an implementation for XEP-0356: Privileged Entity
%%%--------------------------------------------------------------------------------------

-module(mod_privilege).

-author('amuhar3@gmail.com').

-protocol({xep, 0356, '0.2.1'}).

-export([advertise_permissions/1, initial_presences/1, process_presence/1, 
         process_roster_presence/1, compare_presences/2,
         process_message/4, process_iq/4]).

-include("ejabberd_service.hrl").

-include("mod_privacy.hrl").

%%%--------------------------------------------------------------------------------------
%%% Functions to advertise services of allowed permission
%%%--------------------------------------------------------------------------------------

-spec permissions(binary(), binary(), list()) -> xmlel().

permissions(From, To, PrivAccess) ->
    Perms = lists:map(fun({Access, Type}) ->
                          ?DEBUG("Advertise service ~s of allowed permission: ~s = ~s~n",
                                 [To, Access, Type]),
                          #xmlel{name = <<"perm">>, 
                                 attrs = [{<<"access">>, 
                                           atom_to_binary(Access,latin1)},
                                          {<<"type">>, Type}]}
                      end, PrivAccess),
    Stanza = #xmlel{name = <<"privilege">>, 
                    attrs = [{<<"xmlns">> ,?NS_PRIVILEGE}],
                    children = Perms},
    Id = randoms:get_string(),
    #xmlel{name = <<"message">>, 
           attrs = [{<<"id">>, Id}, {<<"from">>, From}, {<<"to">>, To}],
           children = [Stanza]}.

advertise_permissions(#state{privilege_access = []}) -> ok;
advertise_permissions(StateData) ->
    Stanza =
      permissions(?MYNAME, StateData#state.host, StateData#state.privilege_access),
    ejabberd_service:send_element(StateData, Stanza).

%%%--------------------------------------------------------------------------------------
%%%  Process presences
%%%--------------------------------------------------------------------------------------

initial_presences(StateData) ->
    Pids = ejabberd_sm:get_all_pids(),
    lists:foreach(
      fun(Pid) ->
          {User, Server, Resource, PresenceLast} = ejabberd_c2s:get_last_presence(Pid),
          From = #jid{user = User, server = Server, resource = Resource},
          To = jid:from_string(StateData#state.host),
          PacketNew = jlib:replace_from_to(From, To, PresenceLast),
          ejabberd_service:send_element(StateData, PacketNew)
      end, Pids).

%% hook user_send_packet(Packet, C2SState, From, To) -> Packet
%% for Managed Entity Presence
process_presence(Pid) ->
    fun(#xmlel{name = <<"presence">>} = Packet, _C2SState, From, _To) ->
          case fxml:get_attr_s(<<"type">>, Packet#xmlel.attrs) of
            T when (T == <<"">>) or (T == <<"unavailable">>) ->
              Pid ! {user_presence, Packet, From};   
            _ -> ok
          end,
          Packet;
       (Packet, _C2SState, _From, _To) ->
          Packet
    end.
%% s2s_receive_packet(From, To, Packet) -> ok
%% for Roster Presence
%% From subscription "from" or "both"
process_roster_presence(Pid) ->  
    fun(From, To, #xmlel{name = <<"presence">>} = Packet) ->
          case fxml:get_attr_s(<<"type">>, Packet#xmlel.attrs) of
            T when (T == <<"">>) or (T == <<"unavailable">>) ->
              Server = To#jid.server,
              User = To#jid.user,
              PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
                                                 Server, #userlist{}, [User, Server]),
              case privacy_check_packet(Server, User, PrivList, From, To, Packet, in) of
                allow ->
                  Pid ! {roster_presence, Packet, From};
                _ -> ok
              end,
              ok;
            _ -> ok
          end;
       (_From, _To, _Packet) -> ok
    end.

%%%--------------------------------------------------------------------------------------
%%%  Manage Roster
%%%--------------------------------------------------------------------------------------

process_iq(StateData, FromJID, ToJID, Packet) ->
    IQ = jlib:iq_query_or_response_info(Packet),
    case IQ of
      #iq{xmlns = ?NS_ROSTER} -> 
        case (ToJID#jid.luser /= <<"">>) and
             (FromJID#jid.luser == <<"">>) and
             lists:member(ToJID#jid.lserver, ?MYHOSTS) of
          true ->
            AccessType = 
              proplists:get_value(roster, StateData#state.privilege_access, none),
            case IQ#iq.type of
              get when (AccessType == <<"both">>) or (AccessType == <<"get">>) -> 
                RosterIQ = roster_management(ToJID, FromJID, IQ),
                ejabberd_service:send_element(StateData, RosterIQ);
              set when (AccessType == <<"both">>) or (AccessType == <<"set">>) ->
                %% check if user ToJID  exist
                #jid{lserver = Server, luser = User} = ToJID,
                case ejabberd_auth:is_user_exists(User,Server) of
                  true ->
                    ResIQ = roster_management(ToJID, FromJID, IQ),
                    ejabberd_service:send_element(StateData, ResIQ);
                  _ -> ok
                end;
              _ ->
                Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN), 
                ejabberd_service:send_element(StateData, Err)
            end;
          _ ->
            ejabberd_router:route(FromJID, ToJID, Packet)
        end;
      #iq{type = Type, id = Id} when (Type == error) or (Type == result) -> % for XEP-0355
        Hook = {iq, Type, Id},
        Host = ToJID#jid.lserver,
        case (ToJID#jid.luser == <<"">>) and
             (FromJID#jid.luser == <<"">>) and
             lists:member(ToJID#jid.lserver, ?MYHOSTS) of
          true ->
            case ets:lookup(hooks_tmp, {Hook, Host}) of
              [{_, Function, _Timestamp}] -> 
                catch apply(Function, [Packet]);
              [] -> 
                ejabberd_router:route(FromJID, ToJID, Packet)
            end;
          _ ->
            ejabberd_router:route(FromJID, ToJID, Packet)
        end;
      _ ->
        ejabberd_router:route(FromJID, ToJID, Packet)
    end.

roster_management(FromJID, ToJID, IQ) ->
    ResIQ = mod_roster:process_iq(FromJID, FromJID, IQ),
    ResXml = jlib:iq_to_xml(ResIQ),
    jlib:replace_from_to(FromJID, ToJID, ResXml).

%%%--------------------------------------------------------------------------------------
%%%  Message permission
%%%--------------------------------------------------------------------------------------

process_message(StateData, FromJID, ToJID, #xmlel{children = Children} = Packet) ->
    %% if presence was send from service to server,
    case lists:member(ToJID#jid.lserver, ?MYHOSTS) and
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
                    forward_subscribe(StateData, Child, Packet);
                [#xmlel{name = <<"message">>} = Child] -> %% xep-0356
                    forward_message(StateData, Child, Packet);
                _ -> 
                    Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
                    Txt = <<"invalid forwarded element">>,
                    Err = jlib:make_error_reply(Packet, ?ERRT_BAD_REQUEST(Lang, Txt)),
                    ejabberd_service:send_element(StateData, Err)
              end;
          _ ->
              ejabberd_router:route(FromJID, ToJID, Packet)
        end;

      _ ->
        ejabberd_router:route(FromJID, ToJID, Packet)
    end.

forward_subscribe(StateData, Presence, Packet) ->
    PrivAccess = StateData#state.privilege_access,
    T = proplists:get_value(roster, PrivAccess, none),
    Type = fxml:get_attr_s(<<"type">>, Presence#xmlel.attrs),
    if
      ((T == <<"both">>) or (T == <<"set">>)) and (Type == <<"subscribe">>) ->
        From = fxml:get_attr_s(<<"from">>, Presence#xmlel.attrs),
        FromJ = jid:from_string(From),
        To = fxml:get_attr_s(<<"to">>, Presence#xmlel.attrs),
        ToJ = case To of 
                <<"">> -> error;
                _ -> jid:from_string(To)
              end,
        if  
          (ToJ /= error) and (FromJ /= error) ->
            Server = FromJ#jid.lserver,
            User = FromJ#jid.luser,
            case (FromJ#jid.lresource == <<"">>) and 
                  lists:member(Server, ?MYHOSTS) of
              true ->
                if  
                  (Server /= ToJ#jid.lserver) or
                  (User /= ToJ#jid.luser) ->
                    %% 0356 server MUST NOT allow the privileged entity
                    %% to do anything that the managed entity could not do
                    try_roster_subscribe(Server,User, FromJ, ToJ, Presence);   
                  true -> %% we don't want presence sent to self
                    ok
                end;
              _ ->
                Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
                ejabberd_service:send_element(StateData, Err)
            end;
          true ->
            Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
            Txt = <<"Incorrect stanza from/to JID">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_BAD_REQUEST(Lang, Txt)),
            ejabberd_service:send_element(StateData, Err)
            end;
      true ->
        Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
        ejabberd_service:send_element(StateData, Err)
    end.

forward_message(StateData, Message, Packet) ->
    PrivAccess = StateData#state.privilege_access,
    T = proplists:get_value(message, PrivAccess, none),
    if  
      (T == <<"outgoing">>) ->            
        From = fxml:get_attr_s(<<"from">>, Message#xmlel.attrs),
        FromJ = jid:from_string(From),
        To = fxml:get_attr_s(<<"to">>, Message#xmlel.attrs),
        ToJ = case To of 
                <<"">> -> FromJ;
                _ -> jid:from_string(To)
              end,
        if  
          (ToJ /= error) and (FromJ /= error) ->
            Server = FromJ#jid.server,
            User = FromJ#jid.user,
            case (FromJ#jid.lresource == <<"">>) and 
                 lists:member(Server, ?MYHOSTS) of
              true ->
                %% there are no restriction on to attribute
                PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
                                                   Server, #userlist{},
                                                   [User, Server]),
                check_privacy_route(Server, User, PrivList,
                                                  FromJ, ToJ, Message);
              _ ->
                Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
                ejabberd_service:send_element(StateData, Err)
            end;
          true ->
            Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
            Txt = <<"Incorrect stanza from/to JID">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_BAD_REQUEST(Lang, Txt)),
            ejabberd_service:send_element(StateData, Err)
        end;
      true ->
        Err = jlib:make_error_reply(Packet,?ERR_FORBIDDEN),
        ejabberd_service:send_element(StateData, Err)
    end.

%%%--------------------------------------------------------------------------------------
%%%  helper functions
%%%--------------------------------------------------------------------------------------

compare_presences(undefined, _Presence) -> false;                       
compare_presences(#xmlel{attrs = Attrs, children = Child},
                  #xmlel{attrs = Attrs2, children = Child2}) ->
    Id1 = fxml:get_attr_s(<<"id">>, Attrs),
    Id2 = fxml:get_attr_s(<<"id">>, Attrs2),
    if
      (Id1 /= Id2) ->
        false;
      (Id1 /= <<"">>) and (Id1 == Id2) -> 
        true;
      true -> 
        case not compare_attrs(Attrs, Attrs2) of
          true -> false;
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
         compare_attrs(Stanza1#xmlel.attrs, Stanza2#xmlel.attrs) and
         compare_tags(Stanza1#xmlel.children, Stanza2#xmlel.children) of
      true -> 
        compare_tags(Tags1,Tags2);
      false ->
        false
    end.

%% attr() :: {Name, Value}
-spec compare_attrs([attr()], [attr()]) -> boolean().
compare_attrs([],[]) -> true;
compare_attrs(Attrs1, Attrs2) when length(Attrs1) == length(Attrs2) ->
    lists:foldl(fun(Attr,Acc) -> lists:member(Attr, Attrs2) and Acc end, true, Attrs1);
compare_attrs(_Attrs1, _Attrs2) -> false.

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
