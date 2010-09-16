%%%----------------------------------------------------------------------
%%% File    : mod_applepush.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Push module support 
%%% Created :  5 Jun 2009 by Alexey Shchepin <alexey@process-one.net>
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
%%%----------------------------------------------------------------------

-module(mod_applepush).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 push_notification/8,
	 enable_offline_notification/5,
	 disable_notification/3,
	 receive_offline_packet/3,
	 resend_badge/1,
	 multi_resend_badge/1,
	 offline_resend_badge/0]).

%% Debug commands
-export([get_token_by_jid/1]).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-define(NS_P1_PUSH, "p1:push").
-define(NS_P1_PUSHED, "p1:pushed").
-define(NS_P1_ATTACHMENT, "http://process-one.net/attachement").

-record(applepush_cache, {us, device_id, options}).

start(Host, _Opts) ->
    case init_host(Host) of
	true ->
	    mnesia:create_table(
	      applepush_cache,
	      [{disc_copies, [node()]},
	       {attributes, record_info(fields, applepush_cache)}]),
	    mnesia:add_table_copy(muc_online_room, node(), ram_copies),
	    ejabberd_hooks:add(p1_push_notification, Host,
			       ?MODULE, push_notification, 50),
	    ejabberd_hooks:add(p1_push_enable_offline, Host,
			       ?MODULE, enable_offline_notification, 50),
	    ejabberd_hooks:add(p1_push_disable, Host,
			       ?MODULE, disable_notification, 50),
	    ejabberd_hooks:add(offline_message_hook, Host,
			       ?MODULE, receive_offline_packet, 35);
	false ->
	    ok
    end.

stop(Host) ->
    ejabberd_hooks:delete(p1_push_notification, Host,
			  ?MODULE, push_notification, 50),
    ejabberd_hooks:delete(p1_push_disable, Host,
			  ?MODULE, disable_notification, 50),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, receive_offline_packet, 35).


push_notification(Host, JID, Notification, Msg, Unread, Sound, AppID, Sender) ->
    Type = xml:get_path_s(Notification, [{elem, "type"}, cdata]),
    case Type of
	"applepush" ->
	    DeviceID = xml:get_path_s(Notification, [{elem, "id"}, cdata]),
	    PushService = get_push_service(Host, JID, AppID),
	    ServiceJID = jlib:make_jid("", PushService, ""),
	    Badge = integer_to_list(Unread),
	    SSound =
		if
		    Sound -> "true";
		    true -> "false"
		end,
	    Receiver = jlib:jid_to_string(JID),
	    Packet =
		{xmlelement, "message", [],
		 [{xmlelement, "push", [{"xmlns", ?NS_P1_PUSH}],
		   [{xmlelement, "id", [], [{xmlcdata, DeviceID}]},
		    {xmlelement, "msg", [], [{xmlcdata, Msg}]},
		    {xmlelement, "badge", [], [{xmlcdata, Badge}]},
		    {xmlelement, "sound", [], [{xmlcdata, SSound}]},
		    {xmlelement, "from", [], [{xmlcdata, Sender}]},
		    {xmlelement, "to", [], [{xmlcdata, Receiver}]}]}]},
	    ejabberd_router:route(JID, ServiceJID, Packet),
	    stop;
	_ ->
	    ok
    end.

enable_offline_notification(JID, Notification, SendBody, SendFrom, AppID1) ->
    Type = xml:get_path_s(Notification, [{elem, "type"}, cdata]),
    case Type of
	"applepush" ->
	    DeviceID = xml:get_path_s(Notification, [{elem, "id"}, cdata]),
	    case catch erlang:list_to_integer(DeviceID, 16) of
		ID1 when is_integer(ID1) ->
		    AppID =
			case xml:get_path_s(Notification,
					    [{elem, "appid"}, cdata]) of
			    "" -> AppID1;
			    A -> A
			end,
		    {MegaSecs, Secs, _MicroSecs} = now(),
		    TimeStamp = MegaSecs * 1000000 + Secs,
		    Options =
			[{appid, AppID},
			 {send_body, SendBody},
			 {send_from, SendFrom},
			 {timestamp, TimeStamp}],
		    store_cache(JID, ID1, Options);
		_ ->
		    ok
	    end,
	    stop;
	_ ->
	    ok
    end.

disable_notification(JID, Notification, _AppID) ->
    Type = xml:get_path_s(Notification, [{elem, "type"}, cdata]),
    case Type of
	"applepush" ->
	    delete_cache(JID),
	    stop;
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
		{ID, AppID, SendBody, SendFrom} ->
		    ?DEBUG("lookup: ~p~n", [{ID, AppID, SendBody, SendFrom}]),
		    Body1 = xml:get_path_s(Packet, [{elem, "body"}, cdata]),
		    Body =
			case check_x_attachment(Packet) of
			    true ->
				case Body1 of
				    "" -> [238, 128, 136];
				    _ ->
					[238, 128, 136, 32 | Body1]
				end;
			    false ->
				Body1
			end,
		    Pushed = check_x_pushed(Packet),
		    PushService = get_push_service(Host, To, AppID),
		    ServiceJID = jlib:make_jid("", PushService, ""),
		    if
			Body == "";
			Pushed ->
			    if
				From#jid.lserver == ServiceJID#jid.lserver ->
				    Disable =
					xml:get_path_s(
					  Packet, [{elem, "disable"}]) /= "",
				    if
					Disable ->
					    delete_cache(To);
					true ->
					    ok
				    end;
				true ->
				    ok
			    end,
			    ok;
			true ->
			    BFrom = jlib:jid_remove_resource(From),
			    SFrom = jlib:jid_to_string(BFrom),
			    Offline = ejabberd_hooks:run_fold(
					count_offline_messages,
					Host,
					0,
					[To#jid.luser, Host]),
			    IncludeBody =
				case SendBody of
				    all ->
					true;
				    first_per_user ->
					Offline == 0;
				    first ->
					Offline == 0;
				    none ->
					false
				end,
			    Msg =
				if
				    IncludeBody ->
					CBody = utf8_cut(Body, 100),
                                        case SendFrom of
                                            jid -> SFrom ++ ": " ++ CBody;
                                            username -> BFrom#jid.user ++ ": " ++ CBody; 
                                            _ -> CBody
                                        end;
				    true ->
					""
				end,
			    SSound =
				if
				    IncludeBody -> "true";
				    true -> "false"
				end,
			    Badge = integer_to_list(Offline + 1),
			    DeviceID = erlang:integer_to_list(ID, 16),
			    Packet1 =
				{xmlelement, "message", [],
				 [{xmlelement, "push", [{"xmlns", ?NS_P1_PUSH}],
				   [{xmlelement, "id", [],
				     [{xmlcdata, DeviceID}]},
				    {xmlelement, "msg", [],
				     [{xmlcdata, Msg}]},
				    {xmlelement, "badge", [],
				     [{xmlcdata, Badge}]},
				    {xmlelement, "sound", [],
				     [{xmlcdata, SSound}]},
				    {xmlelement, "from", [],
				     [{xmlcdata, SFrom}]}]}]},
			    ejabberd_router:route(To, ServiceJID, Packet1)
		    end
	    end;
	false ->
	    ok
    end.

resend_badge(To) ->
    Host = To#jid.lserver,
    case gen_mod:is_loaded(Host, mod_applepush) of
	true ->
	    case lookup_cache(To) of
		false ->
		    {error, "no cached data for the user"};
		{ID, AppID, SendBody, SendFrom} ->
		    ?DEBUG("lookup: ~p~n", [{ID, AppID, SendBody, SendFrom}]),
		    PushService = get_push_service(Host, To, AppID),
		    ServiceJID = jlib:make_jid("", PushService, ""),
		    Offline = ejabberd_hooks:run_fold(
				count_offline_messages,
				Host,
				0,
				[To#jid.luser, Host]),
		    if
			Offline == 0 ->
			    ok;
			true ->
			    Badge = integer_to_list(Offline),
			    DeviceID = erlang:integer_to_list(ID, 16),
			    Packet1 =
				{xmlelement, "message", [],
				 [{xmlelement, "push", [{"xmlns", ?NS_P1_PUSH}],
				   [{xmlelement, "id", [],
				     [{xmlcdata, DeviceID}]},
				    {xmlelement, "badge", [],
				     [{xmlcdata, Badge}]}]}]},
			    ejabberd_router:route(To, ServiceJID, Packet1)
		    end
	    end;
	false ->
	    {error, "mod_applepush is not loaded"}
    end.

multi_resend_badge(JIDs) ->
    lists:foreach(fun resend_badge/1, JIDs).

offline_resend_badge() ->
    USs = mnesia:dirty_all_keys(applepush_cache),
    JIDs = lists:map(fun({U, S}) -> jlib:make_jid(U, S, "") end, USs),
    multi_resend_badge(JIDs).

lookup_cache(JID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LUS = {LUser, LServer},
    case catch mnesia:dirty_read(applepush_cache, LUS) of
	[#applepush_cache{device_id = DeviceID, options = Options}] ->
	    AppID = proplists:get_value(appid, Options, "applepush.localhost"),
	    SendBody = proplists:get_value(send_body, Options, none),
	    SendFrom = proplists:get_value(send_from, Options, true),
	    {DeviceID, AppID, SendBody, SendFrom};
	_ ->
	    false
    end.

store_cache(JID, DeviceID, Options) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LUS = {LUser, LServer},
    R = #applepush_cache{us = LUS,
			 device_id = DeviceID,
			 options = Options},
    case catch mnesia:dirty_read(applepush_cache, LUS) of
	[R] ->
	    ok;
	_ ->
	    catch mnesia:dirty_write(R)
    end.

delete_cache(JID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LUS = {LUser, LServer},
    catch mnesia:dirty_delete(applepush_cache, LUS).


utf8_cut(S, Bytes) ->
    utf8_cut(S, [], [], Bytes + 1).

utf8_cut(_S, _Cur, Prev, 0) ->
    lists:reverse(Prev);
utf8_cut([], Cur, _Prev, _Bytes) ->
    lists:reverse(Cur);
utf8_cut([C | S], Cur, Prev, Bytes) ->
    if
	C bsr 6 == 2 ->
	    utf8_cut(S, [C | Cur], Prev, Bytes - 1);
        true ->
	    utf8_cut(S, [C | Cur], Cur, Bytes - 1)
    end.

check_x_pushed({xmlelement, _Name, _Attrs, Els}) ->
    check_x_pushed1(Els).

check_x_pushed1([]) ->
    false;
check_x_pushed1([{xmlcdata, _} | Els]) ->
    check_x_pushed1(Els);
check_x_pushed1([El | Els]) ->
    case xml:get_tag_attr_s("xmlns", El) of
	?NS_P1_PUSHED ->
	    true;
	_ ->
	    check_x_pushed1(Els)
    end.

check_x_attachment({xmlelement, _Name, _Attrs, Els}) ->
    check_x_attachment1(Els).

check_x_attachment1([]) ->
    false;
check_x_attachment1([{xmlcdata, _} | Els]) ->
    check_x_attachment1(Els);
check_x_attachment1([El | Els]) ->
    case xml:get_tag_attr_s("xmlns", El) of
	?NS_P1_ATTACHMENT ->
	    true;
	_ ->
	    check_x_attachment1(Els)
    end.


get_push_service(Host, JID, AppID) ->
    PushServices =
	gen_mod:get_module_opt(
	  Host, ?MODULE,
	  push_services, []),
    PushService =
	case lists:keysearch(AppID, 1, PushServices) of
	    false ->
		DefaultServices =
		    gen_mod:get_module_opt(
		      Host, ?MODULE,
		      default_services, []),
		case lists:keysearch(JID#jid.lserver, 1, DefaultServices) of
		    false ->
			gen_mod:get_module_opt(
			  Host, ?MODULE,
			  default_service, "applepush.localhost");
		    {value, {_, PS}} ->
			PS
		end;
	    {value, {AppID, PS}} ->
		PS
	end,
    PushService.



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
get_token_by_jid(JIDString) ->
    #jid{luser = LUser, lserver = LServer} = jlib:string_to_jid(JIDString),
    LUS = {LUser, LServer},
    case mnesia:dirty_read(applepush_cache, LUS) of
        [{applepush_cache,_,I,_}] -> 
                erlang:integer_to_list(I, 16);
        _ ->
                undefined 
    end.

