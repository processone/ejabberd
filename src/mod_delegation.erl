%%%--------------------------------------------------------------------------------------
%%% File    : mod_delegation.erl
%%% Author  : Anna Mukharram <amuhar3@gmail.com>
%%% Purpose : This module is an implementation for XEP-0355: Namespace Delegation
%%%--------------------------------------------------------------------------------------

-module(mod_delegation).

-author('amuhar3@gmail.com').

-behaviour(gen_mod).

-protocol({xep, 0355, '0.3'}).

-export([start/2, stop/1, depends/2, mod_opt_type/1]).

-export([advertise_delegations/1, process_iq/3,
         disco_local_features/5, disco_sm_features/5,
         disco_local_identity/5, disco_sm_identity/5, disco_info/5, clean/0]).

-include_lib("stdlib/include/ms_transform.hrl").

-include("ejabberd_service.hrl").

-define(CLEAN_INTERVAL, timer:minutes(10)).

%%%--------------------------------------------------------------------------------------
%%%  API
%%%--------------------------------------------------------------------------------------

start(Host, _Opts) ->
    mod_disco:register_feature(Host, ?NS_DELEGATION),
    %% start timer for hooks_tmp table cleaning 
    timer:apply_after(?CLEAN_INTERVAL, ?MODULE, clean, []),

    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
                       disco_local_features, 500), %% This hook should be the last
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE,
                       disco_local_identity, 500),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE,
                       disco_sm_identity, 500),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, 
                       disco_sm_features, 500),
    ejabberd_hooks:add(disco_info, Host, ?MODULE,
                       disco_info, 500).


stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_DELEGATION),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE,
                          disco_local_features, 500), 
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE,
                          disco_local_identity, 500),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE,
                          disco_sm_identity, 500),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, 
                          disco_sm_features, 500),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE,
                          disco_info, 500).

depends(_Host, _Opts) -> [].

mod_opt_type(_Opt) -> [].

%%%--------------------------------------------------------------------------------------
%%% 4.2 Functions to advertise service of delegated namespaces
%%%--------------------------------------------------------------------------------------
attribute_tag(Attrs) ->
    lists:map(fun(Attr) ->
                #xmlel{name = <<"attribute">>, attrs = [{<<"name">> , Attr}]}
              end, Attrs).

delegations(From, To, Delegations) ->
    {Elem0, DelegatedNs} = 
      lists:foldl(fun({Ns, FiltAttr}, {Acc, AccNs}) ->
                    case ets:insert_new(delegated_namespaces, 
                                        {Ns, FiltAttr, self(), To, {}, {}}) of
                      true ->
                        Attrs = 
                          if
                            FiltAttr == [] ->
                              ?DEBUG("namespace ~s is delegated to ~s with"
                                     " no filtering attributes ~n",[Ns, To]),
                              [];
                            true ->
                              ?DEBUG("namespace ~s is delegated to ~s with"
                                     " ~p filtering attributes ~n",[Ns, To, FiltAttr]),
                              attribute_tag(FiltAttr)
                          end,
                        add_iq_handlers(Ns),
                        {[#xmlel{name = <<"delegated">>, 
                                 attrs = [{<<"namespace">>, Ns}],
                                 children = Attrs}| Acc], [{Ns, FiltAttr}|AccNs]};
                      false -> {Acc, AccNs}
                    end
                  end, {[], []}, Delegations),
    case Elem0 of
      [] -> {ignore, DelegatedNs};
      _ -> 
        Elem1 = #xmlel{name = <<"delegation">>, 
                       attrs = [{<<"xmlns">>, ?NS_DELEGATION}],
                       children = Elem0},
        Id = randoms:get_string(),
        {#xmlel{name = <<"message">>, 
                attrs = [{<<"id">>, Id}, {<<"from">>, From}, {<<"to">>, To}],
                children = [Elem1]}, DelegatedNs}
    end.

add_iq_handlers(Ns) ->
    lists:foreach(fun(Host) ->
                    IQDisc = 
                      gen_mod:get_module_opt(Host, ?MODULE, iqdisc,
                                             fun gen_iq_handler:check_type/1, one_queue),
                    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                                  Ns, ?MODULE, 
                                                  process_iq, IQDisc),
                    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                                                  Ns, ?MODULE,
                                                  process_iq, IQDisc)
                  end, ?MYHOSTS).

advertise_delegations(#state{delegations = []}) -> [];
advertise_delegations(StateData) ->
    {Delegated, DelegatedNs} = 
      delegations(?MYNAME, StateData#state.host, StateData#state.delegations),
    if 
      Delegated /= ignore ->
        ejabberd_service:send_element(StateData, Delegated),
        % server asks available features for delegated namespaces 
        disco_info(StateData#state{delegations = DelegatedNs});
      true -> ok
    end,
    DelegatedNs.

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

-spec get_client_server([attr()]) -> {jid(), jid()}. 

get_client_server(Attrs) ->
    Client = fxml:get_attr_s(<<"from">>, Attrs),
    ClientJID = jid:from_string(Client),
    ServerJID = jid:from_string(ClientJID#jid.lserver),
    {ClientJID, ServerJID}.

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

-spec check_iq(xmlel(), xmlel()) -> xmlel() | ignore.

check_iq(#xmlel{attrs = Attrs} = Packet,
         #xmlel{attrs = AttrsOrigin} = OriginPacket) ->
    % Id attribute of OriginPacket Must be equil to Packet Id attribute
    Id1 = fxml:get_attr_s(<<"id">>, Attrs),
    Id2 = fxml:get_attr_s(<<"id">>, AttrsOrigin),
    % From attribute of OriginPacket Must be equil to Packet To attribute
    From = fxml:get_attr_s(<<"from">>, AttrsOrigin),
    To = fxml:get_attr_s(<<"to">>, Attrs),
    % Type attribute Must be error or result
    Type = fxml:get_attr_s(<<"type">>, Attrs),
    if
      ((Type == <<"result">>) or (Type == <<"error">>)),
      Id1 == Id2, To == From ->
        NewPacket = jlib:remove_attr(<<"xmlns">>, Packet),
        %% We can send the decapsulated stanza from Server to Client (To)
        NewPacket;
      true ->
        %% service-unavailable
        Err = jlib:make_error_reply(OriginPacket, ?ERR_SERVICE_UNAVAILABLE),
        Err
    end;
check_iq(_Packet, _OriginPacket) -> ignore.

-spec manage_service_result(atom(), atom(), binary(), xmlel()) -> ok.

manage_service_result(HookRes, HookErr, Service, OriginPacket) ->
    fun(Packet) ->
        {ClientJID, ServerJID} = get_client_server(OriginPacket#xmlel.attrs),
        Server = ClientJID#jid.lserver,

        ets:delete(hooks_tmp, {HookRes, Server}),
        ets:delete(hooks_tmp, {HookErr, Server}),
        % Check Packet "from" attribute
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

-spec manage_service_error(atom(), atom(), xmlel()) -> ok.

manage_service_error(HookRes, HookErr, OriginPacket) ->
    fun(_Packet) ->
        {ClientJID, ServerJID} = get_client_server(OriginPacket#xmlel.attrs),
        Server = ClientJID#jid.lserver,
        ets:delete(hooks_tmp, {HookRes, Server}),
        ets:delete(hooks_tmp, {HookErr, Server}),
        Err = jlib:make_error_reply(OriginPacket, ?ERR_SERVICE_UNAVAILABLE),
        ejabberd_router:route(ServerJID, ClientJID, Err)        
    end.


-spec forward_iq(binary(), binary(), xmlel()) -> ok.

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

    HookRes = {iq, result, Id},
    HookErr = {iq, error, Id},

    FunRes = manage_service_result(HookRes, HookErr, Service, Packet),
    FunErr = manage_service_error(HookRes, HookErr, Packet),
    
    Timestamp = p1_time_compat:system_time(seconds),
    ets:insert(hooks_tmp, {{HookRes, Server}, FunRes, Timestamp}),
    ets:insert(hooks_tmp, {{HookErr, Server}, FunErr, Timestamp}),

    From = jid:make(<<"">>, Server, <<"">>),
    To = jid:make(<<"">>, Service, <<"">>),
    ejabberd_router:route(From, To, Elem2).

process_iq(From, #jid{lresource = <<"">>} = To, 
               #iq{type = Type, xmlns = XMLNS} = IQ) ->
    %% check if stanza directed to server
    %% or directed to the bare JID of the sender
    case ((Type == get) or (Type == set)) of
        true ->
            Packet = jlib:iq_to_xml(IQ),
            #xmlel{name = <<"iq">>, attrs = Attrs, children = Children} = Packet,
            AttrsNew = [{<<"xmlns">>, <<"jabber:client">>} | Attrs],
            AttrsNew2 = jlib:replace_from_to_attrs(jid:to_string(From),
                                                   jid:to_string(To), AttrsNew),
            case ets:lookup(delegated_namespaces, XMLNS) of
              [{XMLNS, FiltAttr, _Pid, ServiceHost, _, _}] ->
                case check_filter_attr(FiltAttr, Children) of
                    true ->
                        forward_iq(From#jid.server, ServiceHost,
                                   Packet#xmlel{attrs = AttrsNew2});
                    _ -> ok
                end;
              [] -> ok
            end, 
            ignore;
        _ -> 
            ignore
    end;
process_iq(_From, _To, _IQ) -> ignore.

%%%--------------------------------------------------------------------------------------
%%%  7. Discovering Support
%%%--------------------------------------------------------------------------------------

decapsulate_features(#xmlel{attrs = Attrs} = Packet, Node) ->
  case fxml:get_attr_s(<<"node">>, Attrs) of 
      Node ->
          PREFIX = << ?NS_DELEGATION/binary, "::" >>,
          Size = byte_size(PREFIX),
          BARE_PREFIX = << ?NS_DELEGATION/binary, ":bare:" >>,
          SizeBare = byte_size(BARE_PREFIX),

          Features = [Feat || #xmlel{attrs = [{<<"var">>, Feat}]} <-
                              fxml:get_subtags(Packet, <<"feature">>)],
                       
          Identity = [I || I <- fxml:get_subtags(Packet, <<"identity">>)],

          Exten = [I || I <- fxml:get_subtags_with_xmlns(Packet, <<"x">>, ?NS_XDATA)],

          case Node of
            << PREFIX:Size/binary, NS/binary >> ->
              ets:update_element(delegated_namespaces, NS,
                                 {5, {Features, Identity, Exten}});
            << BARE_PREFIX:SizeBare/binary, NS/binary >> ->
              ets:update_element(delegated_namespaces, NS,
                                 {6, {Features, Identity, Exten}});
               _ -> ok
          end;
      _ -> ok 
  end;
decapsulate_features(_Packet, _Node) -> ok.
    
-spec disco_result(atom(), atom(), binary()) -> ok.

disco_result(HookRes, HookErr, Node) ->
    fun(Packet) ->
        Tag = fxml:get_subtag_with_xmlns(Packet, <<"query">>, ?NS_DISCO_INFO),
        decapsulate_features(Tag, Node),
        
        ets:delete(hooks_tmp, {HookRes, ?MYNAME}),
        ets:delete(hooks_tmp, {HookErr, ?MYNAME})
    end.

-spec disco_error(atom(), atom()) -> ok.

disco_error(HookRes, HookErr) ->
    fun(_Packet) ->
        ets:delete(hooks_tmp, {HookRes, ?MYNAME}),
        ets:delete(hooks_tmp, {HookErr, ?MYNAME})
    end.

-spec disco_info(state()) -> ok.

disco_info(StateData) -> 
    disco_info(StateData, <<"::">>),
    disco_info(StateData, <<":bare:">>).

-spec disco_info(state(), binary()) -> ok.

disco_info(StateData, Sep) ->
    lists:foreach(fun({Ns, _FilterAttr}) ->
                    Id = randoms:get_string(),
                    Node = << ?NS_DELEGATION/binary, Sep/binary, Ns/binary >>,

                    HookRes = {iq, result, Id},
                    HookErr = {iq, error, Id},

                    FunRes = disco_result(HookRes, HookErr, Node),
                    FunErr = disco_error(HookRes, HookErr),

                    Timestamp = p1_time_compat:system_time(seconds),
                    ets:insert(hooks_tmp, {{HookRes, ?MYNAME}, FunRes, Timestamp}),
                    ets:insert(hooks_tmp, {{HookErr, ?MYNAME}, FunErr, Timestamp}),

                    Tag = #xmlel{name = <<"query">>,
                                 attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}, 
                                          {<<"node">>, Node}],
                                 children = []},
                    DiscoReq = #xmlel{name = <<"iq">>,
                                      attrs = [{<<"type">>, <<"get">>}, {<<"id">>, Id},
                                               {<<"from">>, ?MYNAME},
                                               {<<"to">>, StateData#state.host }],
                                      children = [Tag]},
                    ejabberd_service:send_element(StateData, DiscoReq)

                  end, StateData#state.delegations).


disco_features(Acc, Bare) ->
    Fun = fun(Feat) ->
            ets:foldl(fun({Ns, _, _, _, _, _}, A) ->  
                          A or str:prefix(Ns, Feat)
                      end, false, delegated_namespaces)
          end,
    % delete feature namespace which is delegated to service
    Features = lists:filter(fun ({{Feature, _Host}}) ->
                                  not Fun(Feature);
                                (Feature) when is_binary(Feature) ->
                                  not Fun(Feature)
                            end, Acc),
    % add service features
    FeaturesList =
      ets:foldl(fun({_, _, _, _, {Feats, _, _}, {FeatsBare, _, _}}, A) ->
                      if
                        Bare -> A ++ FeatsBare;
                        true -> A ++ Feats
                      end;
                   (_, A) -> A
                end, Features, delegated_namespaces),
    {result, FeaturesList}.

disco_identity(Acc, Bare) ->
    % filter delegated identites
    Fun = fun(Ident) ->
            ets:foldl(fun({_, _, _, _, {_ , I, _}, {_ , IBare, _}}, A) ->
                            Identity = 
                              if
                                Bare -> IBare;
                                true -> I
                              end,
                            (fxml:get_attr_s(<<"category">> , Ident) ==
                             fxml:get_attr_s(<<"category">>, Identity)) and
                            (fxml:get_attr_s(<<"type">> , Ident) ==
                              fxml:get_attr_s(<<"type">>, Identity)) or A;
                            (_, A) -> A
                      end, false, delegated_namespaces)
          end,

    Identities =
      lists:filter(fun (#xmlel{attrs = Attrs}) ->
                      not Fun(Attrs)
                   end, Acc),
    % add service features
    ets:foldl(fun({_, _, _, _, {_, I, _}, {_, IBare, _}}, A) ->
                    if
                      Bare -> A ++ IBare;
                      true -> A ++ I
                    end;
                  (_, A) -> A
              end, Identities, delegated_namespaces).

%% xmlns from value element

-spec get_field_value([xmlel()]) -> binary().

get_field_value([]) -> <<"">>;
get_field_value([Elem| Elems]) ->
    case (fxml:get_attr_s(<<"var">>, Elem#xmlel.attrs) == <<"FORM_TYPE">>) and
         (fxml:get_attr_s(<<"type">>, Elem#xmlel.attrs) == <<"hidden">>) of
      true ->
        Ns = fxml:get_subtag_cdata(Elem, <<"value">>),
        if
          Ns /= <<"">> -> Ns;
          true -> get_field_value(Elems)
        end;
      _ -> get_field_value(Elems)
    end.

get_info(Acc, Bare) ->
    Fun = fun(Feat) ->
            ets:foldl(fun({Ns, _, _, _, _, _}, A) ->  
                        (A or str:prefix(Ns, Feat))
                      end, false, delegated_namespaces)
          end,
    Exten = lists:filter(fun(Xmlel) ->
                           Tags = fxml:get_subtags(Xmlel, <<"field">>),
                           case get_field_value(Tags) of
                             <<"">> -> true;
                             Value -> not Fun(Value)
                           end
                         end, Acc),
    ets:foldl(fun({_, _, _, _, {_, _, Ext}, {_, _, ExtBare}}, A) ->
                    if
                      Bare -> A ++ ExtBare;
                      true -> A ++ Ext
                      end;
                 (_, A) -> A
              end, Exten, delegated_namespaces).
    
%% 7.2.1 General Case

disco_local_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
disco_local_features(Acc, _From, _To, <<>>, _Lang) ->
    FeatsOld = case Acc of
                 {result, I} -> I;
                 _ -> []
               end,
    disco_features(FeatsOld, false);
disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_local_identity(Acc, _From, _To, <<>>, _Lang) ->
    disco_identity(Acc, false);
disco_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%% 7.2.2 Rediction Of Bare JID Disco Info

disco_sm_features({error, ?ERR_ITEM_NOT_FOUND}, _From,
                  #jid{lresource = <<"">>}, <<>>, _Lang) ->
    disco_features([], true);
disco_sm_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
disco_sm_features(Acc, _From, #jid{lresource = <<"">>}, <<>>, _Lang) ->
    FeatsOld = case Acc of
                 {result, I} -> I;
                 _ -> []
               end,
    disco_features(FeatsOld, true);
disco_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_sm_identity(Acc, _From, #jid{lresource = <<"">>}, <<>>, _Lang) ->
    disco_identity(Acc, true);
disco_sm_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

disco_info(Acc, #jid{}, #jid{lresource = <<"">>}, <<>>, _Lang) ->
    get_info(Acc, true);
disco_info(Acc, _Host, _Mod, <<>>, _Lang) ->
    get_info(Acc, false);
disco_info(Acc, _Host, _Mod, _Node, _Lang) ->
    Acc.

%% clean hooks_tmp table

clean() ->
    ?DEBUG("cleaning ~p ETS table~n", [hooks_tmp]),
    Now = p1_time_compat:system_time(seconds),
    catch ets:select_delete(hooks_tmp, 
                            ets:fun2ms(fun({_, _, Timestamp}) -> 
                                         Now - 300 >= Timestamp
                                       end)),
    %% start timer for table cleaning 
    timer:apply_after(?CLEAN_INTERVAL, ?MODULE, clean, []).
