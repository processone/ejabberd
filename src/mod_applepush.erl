%%%----------------------------------------------------------------------
%%% File    : mod_applepush.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Push module support
%%% Created :  5 Jun 2009 by Alexey Shchepin <alexey@process-one.net>
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
%%%----------------------------------------------------------------------

-module(mod_applepush).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 push_notification/8,
	 push_notification_with_custom_fields/9,
	 enable_offline_notification/5,
	 disable_notification/3,
	 receive_offline_packet/3,
         %% other clients may be online, but we still want to push to this one
         send_offline_packet_notification/5, 
	 resend_badge/1,
	 multi_resend_badge/1,
	 offline_resend_badge/0,
         device_reset_badge/5,
         apply_on_all_ios_for/3,
	 remove_user/2,
         process_sm_iq/3]).


%% Debug commands
-export([get_tokens_by_jid/1]).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-define(NS_P1_PUSH, <<"p1:push">>).
-define(NS_P1_PUSHED, <<"p1:pushed">>).
-define(NS_P1_ATTACHMENT, <<"http://process-one.net/attachement">>).

-record(applepush_cache, {us, device_id, options}).


start(Host, Opts) ->
    case init_host(Host) of
	true ->
	    mnesia:create_table(
	      applepush_cache,
	      [{disc_copies, [node()]},
	       {attributes, record_info(fields, applepush_cache)}, {type, bag}]),
            mnesia:add_table_index(applepush_cache, device_id),
	    mnesia:add_table_copy(applepush_cache, node(), ram_copies),
	    ejabberd_hooks:add(p1_push_notification, Host,
			       ?MODULE, push_notification, 50),
	    ejabberd_hooks:add(p1_push_notification_custom, Host,
			       ?MODULE, push_notification_with_custom_fields, 50),
	    ejabberd_hooks:add(p1_push_enable_offline, Host,
			       ?MODULE, enable_offline_notification, 50),
	    ejabberd_hooks:add(p1_push_disable, Host,
			       ?MODULE, disable_notification, 50),
            ejabberd_hooks:add(remove_user, Host,
                               ?MODULE, remove_user, 50),
	    ejabberd_hooks:add(offline_message_hook, Host,
			       ?MODULE, receive_offline_packet, 35),
            IQDisc = gen_mod:get_opt(
                       iqdisc, Opts, fun gen_iq_handler:check_type/1,
                       one_queue),
            gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_P1_PUSH,
                                          ?MODULE, process_sm_iq, IQDisc);
	false ->
	    ?ERROR_MSG("Cannot start ~s on host ~s. 
               Check you had the correct license for the domain and # of 
               registered users", [?MODULE, Host]),
	    ok
    end.

stop(Host) ->
    ejabberd_hooks:delete(p1_push_notification, Host,
			  ?MODULE, push_notification, 50),
    ejabberd_hooks:delete(p1_push_notification_custom, Host,
			  ?MODULE, push_notification_with_custom_fields, 50),
    ejabberd_hooks:delete(p1_push_enable_offline, Host,
                          ?MODULE, enable_offline_notification, 50),
    ejabberd_hooks:delete(p1_push_disable, Host,
			  ?MODULE, disable_notification, 50),
    ejabberd_hooks:delete(remove_user, Host,
                          ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, receive_offline_packet, 35),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_P1_PUSH).



push_notification(Host, JID, Notification, Msg, Unread, Sound, AppID, Sender) ->
    push_notification_with_custom_fields(Host, JID, Notification, Msg, Unread, Sound, AppID, Sender, []).

push_notification_with_custom_fields(Host, JID, Notification, Msg, Unread, Sound, AppID, Sender, CustomFields) ->
    Type = xml:get_path_s(Notification, [{elem, <<"type">>}, cdata]),
    case Type of
	<<"applepush">> ->
	    DeviceID = xml:get_path_s(Notification, [{elem, <<"id">>}, cdata]),
            PushPacket = build_push_packet(DeviceID, Msg, Unread, Sound, Sender, JID, CustomFields),
            route_push_notification(Host, JID, AppID, PushPacket),
	    stop;
	_ ->
	    ok
    end.
build_push_packet(DeviceID, Msg, Unread, Sound, Sender, JID, CustomFields) ->
    Badge = jlib:integer_to_binary(Unread),
    SSound =
        if
            Sound -> <<"true">>;
            true -> <<"false">>
        end,
    Receiver = jlib:jid_to_string(JID),
    #xmlel{name = <<"message">>,
           attrs = [],
           children =
           [#xmlel{name = <<"push">>, attrs = [{<<"xmlns">>, ?NS_P1_PUSH}],
                   children =
                   [#xmlel{name = <<"id">>, attrs = [],
                           children = [{xmlcdata, DeviceID}]},
                    #xmlel{name = <<"msg">>, attrs = [],
                           children = [{xmlcdata, Msg}]},
                    #xmlel{name = <<"badge">>, attrs = [],
                           children = [{xmlcdata, Badge}]},
                    #xmlel{name = <<"sound">>, attrs = [],
                           children = [{xmlcdata, SSound}]},
                    #xmlel{name = <<"from">>, attrs = [],
                           children = [{xmlcdata, Sender}]},
                    #xmlel{name = <<"to">>, attrs = [],
                           children = [{xmlcdata, Receiver}]}] ++
                   build_custom(CustomFields)
                  }
           ]}.

build_custom([]) -> [];
build_custom(Fields) ->
    [#xmlel{name = <<"custom">>, attrs = [], 
            children =
            [#xmlel{name = <<"field">>, attrs = [{<<"name">>, Name}], 
                    children =
                    [{xmlcdata, Value}]} || {Name, Value} <- Fields]}].


route_push_notification(Host, JID, AppId, PushPacket) ->
    PushService = get_push_service(Host, JID, AppId),
    ServiceJID = jlib:make_jid(<<"">>, PushService, <<"">>),
    ejabberd_router:route(JID, ServiceJID, PushPacket).


enable_offline_notification(JID, Notification, SendBody, SendFrom, AppID1) ->
    Type = xml:get_path_s(Notification, [{elem, <<"type">>}, cdata]),
    case Type of
	<<"applepush">> ->
	    DeviceID = xml:get_path_s(Notification, [{elem, <<"id">>}, cdata]),
	    case catch erlang:list_to_integer(binary_to_list(DeviceID), 16) of
		ID1 when is_integer(ID1) ->
		    AppID =
			case xml:get_path_s(Notification,
					    [{elem, <<"appid">>}, cdata]) of
			    <<"">> -> AppID1;
			    A -> A
			end,
		    {MegaSecs, Secs, _MicroSecs} = now(),
		    TimeStamp = MegaSecs * 1000000 + Secs,
		    store_cache(JID, ID1, AppID, SendBody, SendFrom, TimeStamp);
		_ ->
		    ok
	    end,
	    stop;
	_ ->
	    ok
    end.

disable_notification(JID, Notification, _AppID) ->
    Type = xml:get_path_s(Notification, [{elem, <<"type">>}, cdata]),
    case Type of
	<<"applepush">> ->
	    DeviceID = xml:get_path_s(Notification, [{elem, <<"id">>}, cdata]),
	    case catch erlang:list_to_integer(binary_to_list(DeviceID), 16) of
		ID1 when is_integer(ID1) ->
	            delete_cache(JID, ID1),
        	    stop;
                _ ->
                    ok
            end;
	_ ->
	    ok
    end.

do_send_offline_packet_notification(From, To, Packet, ID, AppID, SendBody, SendFrom, BadgeCount) ->
        Host = To#jid.lserver,
        Body1 = xml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
        Body =
            case check_x_attachment(Packet) of
                true ->
                    case Body1 of
                        <<"">> -> <<238, 128, 136>>;
                        _ ->
                            <<238, 128, 136, 32, Body1/binary>>
                    end;
                false ->
                    Body1
            end,
        Pushed = check_x_pushed(Packet),
        PushService = get_push_service(Host, To, AppID),
        ServiceJID = jlib:make_jid(<<"">>, PushService, <<"">>),
        if
            Body == <<"">>;
            Pushed ->
                ok;
            true ->
                BFrom = jlib:jid_remove_resource(From),
                SFrom = jlib:jid_to_string(BFrom),
                IncludeBody =
                    case SendBody of
                        all ->
                            true;
                        first_per_user ->
                            BadgeCount == 1;
                        first ->
                            BadgeCount == 1;
                        none ->
                            false
                    end,
                Msg =
                    if
                        IncludeBody ->
                            CBody = utf8_cut(Body, 100),
                            case SendFrom of
                                jid ->
                                    prepend_sender(SFrom, CBody);
                                username ->
                                    UnescapedFrom = unescape(BFrom#jid.user),
                                    prepend_sender(
                                      UnescapedFrom, CBody); 
                                name ->
                                    Name = get_roster_name(
                                             To, BFrom),
                                    prepend_sender(Name, CBody);
                                _ -> CBody
                            end;
                        true ->
                            <<"">>
                    end,
                SSound =
                    if
                        IncludeBody -> <<"true">>;
                        true -> <<"false">>
                    end,
                Badge = jlib:integer_to_binary(BadgeCount),
                DeviceID = jlib:integer_to_binary(ID, 16),
                STo = jlib:jid_to_string(To),
                Packet1 =
                    #xmlel{name = <<"message">>,
                           attrs = [],
                           children =
                           [#xmlel{name = <<"push">>,
                                   attrs = [{<<"xmlns">>, ?NS_P1_PUSH}],
                                   children =
                                   [#xmlel{name = <<"id">>, attrs = [],
                                           children = [{xmlcdata, DeviceID}]},
                                    #xmlel{name = <<"msg">>, attrs = [],
                                           children = [{xmlcdata, Msg}]},
                                    #xmlel{name = <<"badge">>, attrs = [],
                                           children = [{xmlcdata, Badge}]},
                                    #xmlel{name = <<"sound">>, attrs = [],
                                           children = [{xmlcdata, SSound}]},
                                    #xmlel{name = <<"from">>, attrs = [],
                                           children = [{xmlcdata, SFrom}]},
                                    #xmlel{name = <<"to">>, attrs = [],
                                           children = [{xmlcdata, STo}]}]
                                  }
                           ]},
                ejabberd_router:route(To, ServiceJID, Packet1)
        end.

send_offline_packet_notification(From, To, Packet, SDeviceID, BadgeCount) ->
    DeviceID =
        if
            is_binary(SDeviceID) -> binary_to_list(SDeviceID);
            is_list(SDeviceID) -> SDeviceID
        end,
    case catch erlang:list_to_integer(DeviceID, 16) of
        ID1 when is_integer(ID1) ->
            case lookup_cache(To, ID1) of
                [{ID, AppID, SendBody, SendFrom}] ->
                    do_send_offline_packet_notification(From, To, Packet, ID, AppID, SendBody, SendFrom, BadgeCount);
                _ ->
                    ok
            end;
        _ -> 
            ok
    end.
receive_offline_packet(From, To, Packet) ->
    ?DEBUG("mod_applepush offline~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    Host = To#jid.lserver,
    case gen_mod:is_loaded(Host, mod_applepush) of
	true ->
	    case lookup_cache(To) of
		false ->
		    ok;
                DeviceList ->
                    Offline = ejabberd_hooks:run_fold(
                                count_offline_messages,
                                Host,
                                0,
                                [To#jid.luser, Host]) + 1,
                    lists:foreach(fun({ID, AppID, SendBody, SendFrom}) ->
                                          ?DEBUG("lookup: ~p~n", [{ID, AppID, SendBody, SendFrom}]),
                                          do_send_offline_packet_notification(From, To, Packet, ID, AppID, SendBody, SendFrom, Offline)
                                  end, DeviceList)
            end;
	false ->
	    ok
    end.


process_sm_iq(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
	{set, #xmlel{name = <<"disable">>}} ->
            Host = To#jid.lserver,
            SDeviceID = xml:get_tag_attr_s(<<"id">>, SubEl),
            DeviceID =
                erlang:list_to_integer(binary_to_list(SDeviceID), 16),
            case lookup_cache(To, DeviceID) of
                [{_ID, AppID, _SendBody, _SendFrom}] ->
                    PushService = get_push_service(Host, To, AppID),
                    ServiceJID = jlib:make_jid(<<"">>, PushService, <<"">>),
                    if
                        From#jid.lserver == ServiceJID#jid.lserver ->
                            delete_cache(To, DeviceID),
                            IQ#iq{type = result, sub_el = []};
                        true ->
                            IQ#iq{type = error,
                                  sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
                    end
            end;
        {set, _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
        {get, _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.


device_reset_badge(Host, To, DeviceID, AppID, Badge) ->
    ?INFO_MSG("Sending reset badge (~p) push to ~p ~p", [Badge, To, DeviceID]),
    PushService = get_push_service(Host, To, AppID),
    ServiceJID = jlib:make_jid(<<"">>, PushService, <<"">>),
    LBadge = jlib:integer_to_binary(Badge),
    Packet1 =
        #xmlel{name = <<"message">>, attrs = [],
               children =
               [#xmlel{name = <<"push">>, attrs = [{<<"xmlns">>, ?NS_P1_PUSH}],
                       children =
                       [#xmlel{name = <<"id">>, attrs = [],
                               children =
                               [{xmlcdata, DeviceID}]},
                        #xmlel{name = <<"badge">>, attrs = [],
                               children =
                               [{xmlcdata, LBadge}]}]}]},
    ejabberd_router:route(To, ServiceJID, Packet1).



resend_badge(To) ->
    Host = To#jid.lserver,
    case gen_mod:is_loaded(Host, mod_applepush) of
	true ->
	    case lookup_cache(To) of
		false ->
		    {error, "no cached data for the user"};
                DeviceList ->
                    lists:foreach(fun({ID, AppID, SendBody, SendFrom}) ->
                        ?DEBUG("lookup: ~p~n", [{ID, AppID, SendBody, SendFrom}]),
                        PushService = get_push_service(Host, To, AppID),
                        ServiceJID = jlib:make_jid(<<"">>, PushService, <<"">>),
                        Offline = ejabberd_hooks:run_fold(
                                    count_offline_messages,
                                    Host,
                                    0,
                                    [To#jid.luser, Host]),
                        if
                            Offline == 0 ->
                                ok;
                            true ->
                                Badge = jlib:integer_to_binary(Offline),
                                DeviceID = jlib:integer_to_binary(ID, 16),
                                Packet1 =
                                    #xmlel{name = <<"message">>,
                                           attrs = [],
                                           children =
                                           [#xmlel{name = <<"push">>,
                                                   attrs = [{<<"xmlns">>, ?NS_P1_PUSH}],
                                                   children =
                                                   [#xmlel{name = <<"id">>,
                                                           attrs = [],
                                                           children =
                                                           [{xmlcdata, DeviceID}]},
                                                    #xmlel{name = <<"badge">>,
                                                           attrs = [],
                                                           children =
                                                           [{xmlcdata, Badge}]}]}]},
                                ejabberd_router:route(To, ServiceJID, Packet1)
                        end
                    end, DeviceList)
                end;
	false ->
	    {error, "mod_applepush is not loaded"}
    end.

multi_resend_badge(JIDs) ->
    lists:foreach(fun resend_badge/1, JIDs).

offline_resend_badge() ->
    USs = mnesia:dirty_all_keys(applepush_cache),
    JIDs = lists:map(fun({U, S}) -> jlib:make_jid(U, S, <<"">>) end, USs),
    multi_resend_badge(JIDs).

lookup_cache(JID) ->
    case get_storage(JID#jid.lserver) of
        mnesia ->
            lookup_cache_mnesia(JID);
        sql ->
            lookup_cache_sql(JID)
    end.

lookup_cache_mnesia(JID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LUS = {LUser, LServer},
    do_lookup_cache_mnesia(#applepush_cache{us = LUS, _ = '_'}).

lookup_cache_sql(JID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    Username = ejabberd_odbc:escape(LUser),
    do_lookup_cache_sql(
      LServer,
      [<<"select device_id, app_id, send_body, send_from from applepush_cache "
        "where username='">>, Username, <<"'">>]).

lookup_cache(JID, DeviceID) ->
    case get_storage(JID#jid.lserver) of
        mnesia ->
            lookup_cache_mnesia(JID, DeviceID);
        sql ->
            lookup_cache_sql(JID, DeviceID)
    end.

lookup_cache_mnesia(JID, DeviceID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LUS = {LUser, LServer},
    do_lookup_cache_mnesia(#applepush_cache{device_id = DeviceID, us = LUS, _ = '_'}).

lookup_cache_sql(JID, DeviceID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    Username = ejabberd_odbc:escape(LUser),
    SDeviceID = erlang:integer_to_list(DeviceID, 16),
    do_lookup_cache_sql(
      LServer,
      [<<"select device_id, app_id, send_body, send_from from applepush_cache "
        "where username='">>, Username, <<"' and device_id='">>,
       SDeviceID, <<"'">>]).

do_lookup_cache_mnesia(MatchSpec) ->
    case mnesia:dirty_match_object(MatchSpec) of
	EntryList when is_list(EntryList) ->
            lists:map(fun(#applepush_cache{device_id = DeviceID, options = Options}) ->
        	    AppID = proplists:get_value(appid, Options, <<"applepush.localhost">>),
	            SendBody = proplists:get_value(send_body, Options, none),
        	    SendFrom = proplists:get_value(send_from, Options, true),
	            {DeviceID, AppID, SendBody, SendFrom}
            end, EntryList);
	_ ->
	    false
    end.

do_lookup_cache_sql(LServer, Query) ->
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"device_id">>, <<"app_id">>, <<"send_body">>, <<"send_from">>],
         EntryList} ->
            lists:map(
              fun({SDeviceID, AppID, SSendBody, SSendFrom}) ->
                      DeviceID = erlang:list_to_integer(SDeviceID, 16),
                      SendBody =
                          case SSendBody of
                              <<"A">> -> all;
                              <<"U">> -> first_per_user;
                              <<"F">> -> first;
                              _ -> none
                          end,
                      SendFrom =
                          case SSendFrom of
                              <<"J">> -> jid;
                              <<"U">> -> username;
                              <<"N">> -> name;
                              _ -> none
                          end,
                      {DeviceID, AppID, SendBody, SendFrom}
            end, EntryList);
	_ ->
	    false
    end.


store_cache(JID, DeviceID, AppID, SendBody, SendFrom, TimeStamp) ->
    case get_storage(JID#jid.lserver) of
        mnesia ->
            Options =
                [{appid, AppID},
                 {send_body, SendBody},
                 {send_from, SendFrom},
                 {timestamp, TimeStamp}],
            store_cache_mnesia(JID, DeviceID, Options);
        sql ->
            store_cache_sql(JID, DeviceID, AppID, SendBody, SendFrom)
    end.


store_cache_mnesia(JID, DeviceID, Options) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LUS = {LUser, LServer},
    R = #applepush_cache{us = LUS,
			 device_id = DeviceID,
			 options = Options},
    case catch mnesia:dirty_read(applepush_cache, LUS) of
	[R] ->
	    ok;
	_ ->
            lists:foreach(
              fun(R1) ->
                      mnesia:dirty_delete_object(R1)
              end,
              mnesia:dirty_index_read(
                applepush_cache, DeviceID, #applepush_cache.device_id)),
	    catch mnesia:dirty_write(R)
    end.

store_cache_sql(JID, DeviceID, AppID, SendBody, SendFrom) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    Username = ejabberd_odbc:escape(LUser),
    SDeviceID = erlang:integer_to_list(DeviceID, 16),
    SAppID = ejabberd_odbc:escape(AppID),
    SSendBody =
        case SendBody of
            all -> <<"A">>;
            first_per_user -> <<"U">>;
            first -> <<"F">>;
            _ -> <<"-">>
        end,
    SSendFrom =
        case SendFrom of
            jid -> <<"J">>;
            username -> <<"U">>;
            name -> <<"N">>;
            _ -> <<"-">>
        end,
    F = fun() ->
                ejabberd_odbc:sql_query_t(
                  [<<"delete from applepush_cache "
                    "where username='">>, Username, <<"' and ">>,
                   <<"device_id='">>, SDeviceID, <<"';">>]),
                ejabberd_odbc:sql_query_t(
                  [<<"insert into applepush_cache(username, device_id, app_id, "
                    "                            send_body, send_from) "
                    "values ('">>, Username, <<"', '">>, SDeviceID, <<"', '">>,
                   SAppID, <<"', '">>, SSendBody, <<"', '">>, SSendFrom, <<"');">>])
        end,
    odbc_queries:sql_transaction(LServer, F).

delete_cache(JID, DeviceID) ->
    case get_storage(JID#jid.lserver) of
        mnesia ->
            delete_cache_mnesia(JID, DeviceID);
        sql ->
            delete_cache_sql(JID, DeviceID)
    end.

delete_cache_mnesia(JID, DeviceID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LUS = {LUser, LServer},
    [ mnesia:dirty_delete_object(Obj) || Obj <-  mnesia:dirty_match_object(#applepush_cache{device_id = DeviceID, us = LUS, _ = '_'})].

delete_cache_sql(JID, DeviceID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    Username = ejabberd_odbc:escape(LUser),
    SDeviceID = erlang:integer_to_list(DeviceID, 16),
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from applepush_cache "
        "where username='">>, Username, <<"' and ">>,
       <<"device_id='">>, SDeviceID, <<"';">>]).

delete_cache(JID) ->
    case get_storage(JID#jid.lserver) of
        mnesia ->
            delete_cache_mnesia(JID);
        sql ->
            delete_cache_sql(JID)
    end.

delete_cache_mnesia(JID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LUS = {LUser, LServer},
    catch mnesia:dirty_delete(applepush_cache, LUS).

delete_cache_sql(JID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from applepush_cache "
        "where username='">>, Username, <<"';">>]).


remove_user(User, Server) ->
    delete_cache(jlib:make_jid(User, Server, <<"">>)).


prepend_sender(<<"">>, Body) ->
    Body;
prepend_sender(From, Body) ->
    <<From/binary, ": ", Body/binary>>.

utf8_cut(S, Bytes) -> utf8_cut(S, <<>>, <<>>, Bytes + 1).

utf8_cut(_S, _Cur, Prev, 0) -> Prev;
utf8_cut(<<>>, Cur, _Prev, _Bytes) -> Cur;
utf8_cut(<<C, S/binary>>, Cur, Prev, Bytes) ->
    if C bsr 6 == 2 ->
	   utf8_cut(S, <<Cur/binary, C>>, Prev, Bytes - 1);
       true -> utf8_cut(S, <<Cur/binary, C>>, Cur, Bytes - 1)
    end.

-include("mod_roster.hrl").

get_roster_name(To, JID) ->
    User = To#jid.luser,
    Server = To#jid.lserver,
    RosterItems = ejabberd_hooks:run_fold(
                    roster_get, Server, [], [{User, Server}]),
    JUser = JID#jid.luser,
    JServer = JID#jid.lserver,
    Item =
        lists:foldl(
          fun(_, Res = #roster{}) ->
                  Res;
             (I, false) ->
                  case I#roster.jid of
                      {JUser, JServer, _} ->
                          I;
                      _ ->
                          false
                  end
          end, false, RosterItems),
    case Item of
        false ->
            unescape(JID#jid.user);
        #roster{} ->
            Item#roster.name
    end.

unescape(<<"">>) -> <<"">>;
unescape(<<"\\20", S/binary>>) ->
    <<"\s", (unescape(S))/binary>>;
unescape(<<"\\22", S/binary>>) ->
    <<"\"", (unescape(S))/binary>>;
unescape(<<"\\26", S/binary>>) ->
    <<"&", (unescape(S))/binary>>;
unescape(<<"\\27", S/binary>>) ->
    <<"'", (unescape(S))/binary>>;
unescape(<<"\\2f", S/binary>>) ->
    <<"/", (unescape(S))/binary>>;
unescape(<<"\\3a", S/binary>>) ->
    <<":", (unescape(S))/binary>>;
unescape(<<"\\3c", S/binary>>) ->
    <<"<", (unescape(S))/binary>>;
unescape(<<"\\3e", S/binary>>) ->
    <<">", (unescape(S))/binary>>;
unescape(<<"\\40", S/binary>>) ->
    <<"@", (unescape(S))/binary>>;
unescape(<<"\\5c", S/binary>>) ->
    <<"\\", (unescape(S))/binary>>;
unescape(<<C, S/binary>>) -> <<C, (unescape(S))/binary>>.

check_x_pushed(#xmlel{children = Els}) ->
    check_x_pushed1(Els).

check_x_pushed1([]) ->
    false;
check_x_pushed1([{xmlcdata, _} | Els]) ->
    check_x_pushed1(Els);
check_x_pushed1([El | Els]) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
	?NS_P1_PUSHED ->
	    true;
	_ ->
	    check_x_pushed1(Els)
    end.

check_x_attachment(#xmlel{children = Els}) ->
    check_x_attachment1(Els).

check_x_attachment1([]) ->
    false;
check_x_attachment1([{xmlcdata, _} | Els]) ->
    check_x_attachment1(Els);
check_x_attachment1([El | Els]) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
	?NS_P1_ATTACHMENT ->
	    true;
	_ ->
	    check_x_attachment1(Els)
    end.


get_push_service(Host, JID, AppID) ->
    PushServices =
	gen_mod:get_module_opt(
	  Host, ?MODULE,
	  push_services,
          fun(L) when is_list(L) -> L end,
          []),
    PushService =
	case lists:keysearch(AppID, 1, PushServices) of
	    false ->
		DefaultServices =
		    gen_mod:get_module_opt(
		      Host, ?MODULE,
		      default_services,
                      fun(L) when is_list(L) -> L end,
                      []),
		case lists:keysearch(JID#jid.lserver, 1, DefaultServices) of
		    false ->
			gen_mod:get_module_opt(
			  Host, ?MODULE,
			  default_service,
                          fun(S) when is_binary(S) -> S end,
                          <<"applepush.localhost">>);
		    {value, {_, PS}} ->
			PS
		end;
	    {value, {AppID, PS}} ->
		PS
	end,
    PushService.

get_storage(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, backend,
                           fun(mnesia) -> mnesia;
                              (sql) -> sql
                           end,
                           mnesia).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal module protection

-define(VALID_HOSTS, []). % default is unlimited use
-define(MAX_USERS, 0). % default is unlimited use

init_host(VHost) ->
    case ?VALID_HOSTS of
    [] -> % unlimited use
        true;
    ValidList -> % limited use
        init_host(VHost, ValidList)
    end.
init_host([], _) ->
    false;
init_host(VHost, ValidEncryptedList) ->
    EncryptedHost = erlang:md5(lists:reverse(VHost)),
    case lists:member(EncryptedHost, ValidEncryptedList) of
    true ->
	case ?MAX_USERS of
	0 -> true;
	N -> ejabberd_auth:get_vh_registered_users_number(VHost) =< N
	end;
    false ->
	case string:chr(VHost, $.) of
	0 -> false;
	Pos -> init_host(string:substr(VHost, Pos+1), ValidEncryptedList)
	end
    end.

%% Debug commands
%% JID is of form
get_tokens_by_jid(JIDString) when is_binary(JIDString) ->
	get_tokens_by_jid(jlib:string_to_jid(JIDString));
get_tokens_by_jid(#jid{luser = LUser, lserver = LServer}) ->
    LUS = {LUser, LServer},
    [erlang:integer_to_list(I, 16) || {applepush_cache,_,I,_} <- 
       mnesia:dirty_read(applepush_cache, LUS)].


%% Apply OnlineFun([{DeviceID, Pid}]) on all online or standby, ios, sessions of the given jid
%% and   OfflineFun([DeviceID]) on all offline sessions of the given jid
apply_on_all_ios_for(JID, OnlineFun, OfflineFun) when is_binary(JID) ->
    apply_on_all_ios_for(jlib:string_to_jid(JID), OnlineFun, OfflineFun);

apply_on_all_ios_for(JID = #jid{luser = User, lserver = Server}, OnlineFun, OfflineFun) ->
    case lookup_cache(JID) of
	    false ->
		    ok;
	    Devices -> 
		    %% as integer
		    AlliOSDeviceIdsInt = [ ID || {ID, _AppID, _SendBody, _SendFrom} <- Devices],
		    Pids = [ejabberd_sm:get_session_pid(User, Server, Resource) || 
                		Resource <- ejabberd_sm:get_user_resources(User, Server)],
		     %%catch because the c2s process may have been died

		    %%In HEX format 
		    OnlineiOSDeviceIDs = [  {D,P} ||  
			    {D, P} <-  [ {catch ejabberd_c2s:get_ios_clientid(Pid), Pid}  || Pid <- Pids], is_list(D)],
		    %%As integer
		    OnlineiOSDevicesIDsInt = [{erlang:list_to_integer(D, 16), P} || {D, P} <- OnlineiOSDeviceIDs],
 		    OfflineiOSDevicesInt = lists:filter(fun(ID) -> not lists:keymember(ID, 1, OnlineiOSDevicesIDsInt) end, 
			    AlliOSDeviceIdsInt),
		    OfflineiOSDevices = [erlang:integer_to_list(I, 16) || I <- OfflineiOSDevicesInt],
		    OnlineFun(OnlineiOSDeviceIDs),
		    OfflineFun(OfflineiOSDevices)
    end.



