%%%----------------------------------------------------------------------
%%% File    : mod_muc_room.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC room stuff
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-module(mod_muc_room).

-author('alexey@process-one.net').

-protocol({xep, 317, '0.1'}).

-behaviour(p1_fsm).

%% External exports
-export([start_link/10,
	 start_link/8,
	 start/10,
	 start/8,
	 supervisor/1,
	 get_role/2,
	 get_affiliation/2,
	 is_occupant_or_admin/2,
	 route/2,
	 expand_opts/1,
	 config_fields/0,
	 destroy/1,
	 destroy/2,
	 shutdown/1,
	 get_config/1,
	 set_config/2,
	 get_state/1,
	 get_info/1,
	 change_item/5,
	 change_item_async/5,
	 config_reloaded/1,
	 subscribe/4,
	 unsubscribe/2,
	 is_subscribed/2,
	 get_subscribers/1,
	 service_message/2,
	 get_disco_item/4]).

%% gen_fsm callbacks
-export([init/1,
	 normal_state/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").
-include("mod_muc_room.hrl").
-include("ejabberd_stacktrace.hrl").

-define(MAX_USERS_DEFAULT_LIST,
	[5, 10, 20, 30, 50, 100, 200, 500, 1000, 2000, 5000]).

-define(DEFAULT_MAX_USERS_PRESENCE,1000).

-define(MUC_HAT_ADD_CMD, <<"http://prosody.im/protocol/hats#add">>).
-define(MUC_HAT_REMOVE_CMD, <<"http://prosody.im/protocol/hats#remove">>).
-define(MUC_HAT_LIST_CMD, <<"p1:hats#list">>).
-define(MAX_HATS_USERS, 100).
-define(MAX_HATS_PER_USER, 10).
-define(CLEAN_ROOM_TIMEOUT, 30000).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

-type state() :: #state{}.
-type fsm_stop() :: {stop, normal, state()}.
-type fsm_next() :: {next_state, normal_state, state()}.
-type fsm_transition() :: fsm_stop() | fsm_next().
-type disco_item_filter() ::  only_non_empty | all | non_neg_integer().
-type admin_action() :: {jid(), affiliation | role, affiliation() | role(), binary()}.
-export_type([state/0, disco_item_filter/0]).

-callback set_affiliation(binary(), binary(), binary(), jid(), affiliation(),
			  binary()) -> ok | {error, any()}.
-callback set_affiliations(binary(), binary(), binary(),
			   affiliations()) -> ok | {error, any()}.
-callback get_affiliation(binary(), binary(), binary(),
			  binary(), binary()) -> {ok, affiliation()} | {error, any()}.
-callback get_affiliations(binary(), binary(), binary()) -> {ok, affiliations()} | {error, any()}.
-callback search_affiliation(binary(), binary(), binary(), affiliation()) ->
    {ok, [{ljid(), {affiliation(), binary()}}]} | {error, any()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start(binary(), binary(), mod_muc:access(), binary(), non_neg_integer(),
	    atom(), jid(), binary(), [{atom(), term()}], ram | file) ->
		   {ok, pid()} | {error, any()}.
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
      Creator, Nick, DefRoomOpts, QueueType) ->
    supervisor:start_child(
      supervisor(ServerHost),
      [Host, ServerHost, Access, Room, HistorySize,
       RoomShaper, Creator, Nick, DefRoomOpts, QueueType]).

-spec start(binary(), binary(), mod_muc:access(), binary(), non_neg_integer(),
	    atom(), [{atom(), term()}], ram | file) ->
		   {ok, pid()} | {error, any()}.
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts, QueueType) ->
    supervisor:start_child(
      supervisor(ServerHost),
      [Host, ServerHost, Access, Room, HistorySize,
       RoomShaper, Opts, QueueType]).

-spec start_link(binary(), binary(), mod_muc:access(), binary(), non_neg_integer(),
		 atom(), jid(), binary(), [{atom(), term()}], ram | file) ->
			{ok, pid()} | {error, any()}.
start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
	   Creator, Nick, DefRoomOpts, QueueType) ->
    p1_fsm:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				 RoomShaper, Creator, Nick, DefRoomOpts, QueueType],
		       ?FSMOPTS).

-spec start_link(binary(), binary(), mod_muc:access(), binary(), non_neg_integer(),
		 atom(), [{atom(), term()}], ram | file) ->
			{ok, pid()} | {error, any()}.
start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts, QueueType) ->
    p1_fsm:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				 RoomShaper, Opts, QueueType],
		       ?FSMOPTS).

-spec supervisor(binary()) -> atom().
supervisor(Host) ->
    gen_mod:get_module_proc(Host, mod_muc_room_sup).

-spec destroy(pid()) -> ok.
destroy(Pid) ->
    p1_fsm:send_all_state_event(Pid, destroy).

-spec destroy(pid(), binary()) -> ok.
destroy(Pid, Reason) ->
    p1_fsm:send_all_state_event(Pid, {destroy, Reason}).

-spec shutdown(pid()) -> boolean().
shutdown(Pid) ->
    ejabberd_cluster:send(Pid, shutdown).

-spec config_reloaded(pid()) -> boolean().
config_reloaded(Pid) ->
    ejabberd_cluster:send(Pid, config_reloaded).

-spec get_config(pid()) -> {ok, config()} | {error, notfound | timeout}.
get_config(Pid) ->
    try p1_fsm:sync_send_all_state_event(Pid, get_config)
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, timeout};
	  _:{_, {p1_fsm, _, _}} ->
	    {error, notfound}
    end.

-spec set_config(pid(), config()) -> {ok, config()} | {error, notfound | timeout}.
set_config(Pid, Config) ->
    try p1_fsm:sync_send_all_state_event(Pid, {change_config, Config})
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, timeout};
	  _:{_, {p1_fsm, _, _}} ->
	    {error, notfound}
    end.

-spec change_item(pid(), jid(), affiliation | role, affiliation() | role(), binary()) ->
			 {ok, state()} | {error, notfound | timeout}.
change_item(Pid, JID, Type, AffiliationOrRole, Reason) ->
    try p1_fsm:sync_send_all_state_event(
	  Pid, {process_item_change, {JID, Type, AffiliationOrRole, Reason}, undefined})
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, timeout};
	  _:{_, {p1_fsm, _, _}} ->
	    {error, notfound}
    end.

-spec change_item_async(pid(), jid(), affiliation | role, affiliation() | role(), binary()) -> ok.
change_item_async(Pid, JID, Type, AffiliationOrRole, Reason) ->
    p1_fsm:send_all_state_event(
      Pid, {process_item_change, {JID, Type, AffiliationOrRole, Reason}, undefined}).

-spec get_state(pid()) -> {ok, state()} | {error, notfound | timeout}.
get_state(Pid) ->
    try p1_fsm:sync_send_all_state_event(Pid, get_state)
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, timeout};
	  _:{_, {p1_fsm, _, _}} ->
	    {error, notfound}
    end.

-spec get_info(pid()) -> {ok, #{occupants_number => integer()}} |
                         {error, notfound | timeout}.
get_info(Pid) ->
    try
        {ok, p1_fsm:sync_send_all_state_event(Pid, get_info)}
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, timeout};
	  _:{_, {p1_fsm, _, _}} ->
	    {error, notfound}
    end.

-spec subscribe(pid(), jid(), binary(), [binary()]) -> {ok, [binary()]} | {error, binary()}.
subscribe(Pid, JID, Nick, Nodes) ->
    try p1_fsm:sync_send_all_state_event(Pid, {muc_subscribe, JID, Nick, Nodes})
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, ?T("Request has timed out")};
	  _:{_, {p1_fsm, _, _}} ->
	    {error, ?T("Conference room does not exist")}
    end.

-spec unsubscribe(pid(), jid()) -> ok | {error, binary()}.
unsubscribe(Pid, JID) ->
    try p1_fsm:sync_send_all_state_event(Pid, {muc_unsubscribe, JID})
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, ?T("Request has timed out")};
	  exit:{normal, {p1_fsm, _, _}} ->
	    ok;
	  _:{_, {p1_fsm, _, _}} ->
	    {error, ?T("Conference room does not exist")}
    end.

-spec is_subscribed(pid(), jid()) -> {true, binary(), [binary()]} | false.
is_subscribed(Pid, JID) ->
    try p1_fsm:sync_send_all_state_event(Pid, {is_subscribed, JID})
    catch _:{_, {p1_fsm, _, _}} -> false
    end.

-spec get_subscribers(pid()) -> {ok, [jid()]} | {error, notfound | timeout}.
get_subscribers(Pid) ->
    try p1_fsm:sync_send_all_state_event(Pid, get_subscribers)
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, timeout};
	  _:{_, {p1_fsm, _, _}} ->
	    {error, notfound}
    end.

-spec service_message(pid(), binary()) -> ok.
service_message(Pid, Text) ->
    p1_fsm:send_all_state_event(Pid, {service_message, Text}).

-spec get_disco_item(pid(), disco_item_filter(), jid(), binary()) ->
			    {ok, binary()} | {error, notfound | timeout}.
get_disco_item(Pid, Filter, JID, Lang) ->
    Timeout = 100,
    Time = erlang:system_time(millisecond),
    Query = {get_disco_item, Filter, JID, Lang, Time+Timeout},
    try p1_fsm:sync_send_all_state_event(Pid, Query, Timeout) of
	{item, Desc} ->
	    {ok, Desc};
	false ->
	    {error, notfound}
    catch _:{timeout, {p1_fsm, _, _}} ->
	    {error, timeout};
	  _:{_, {p1_fsm, _, _}} ->
	    {error, notfound}
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([Host, ServerHost, Access, Room, HistorySize,
      RoomShaper, Creator, _Nick, DefRoomOpts, QueueType]) ->
    process_flag(trap_exit, true),
    Shaper = ejabberd_shaper:new(RoomShaper),
    RoomQueue = room_queue_new(ServerHost, Shaper, QueueType),
    State = set_affiliation(Creator, owner,
	    #state{host = Host, server_host = ServerHost,
		   access = Access, room = Room,
		   history = lqueue_new(HistorySize, QueueType),
		   jid = jid:make(Room, Host),
		   just_created = true,
		   room_queue = RoomQueue,
		   room_shaper = Shaper}),
    State1 = set_opts(DefRoomOpts, State),
    store_room(State1),
    ?INFO_MSG("Created MUC room ~ts@~ts by ~ts",
	      [Room, Host, jid:encode(Creator)]),
    add_to_log(room_existence, created, State1),
    add_to_log(room_existence, started, State1),
    ejabberd_hooks:run(start_room, ServerHost, [ServerHost, Room, Host]),
    erlang:send_after(?CLEAN_ROOM_TIMEOUT, self(),
                      close_room_if_temporary_and_empty),
    {ok, normal_state, reset_hibernate_timer(State1)};
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts, QueueType]) ->
    process_flag(trap_exit, true),
    Shaper = ejabberd_shaper:new(RoomShaper),
    RoomQueue = room_queue_new(ServerHost, Shaper, QueueType),
    State = set_opts(Opts, #state{host = Host,
				  server_host = ServerHost,
				  access = Access,
				  room = Room,
				  history = lqueue_new(HistorySize, QueueType),
				  jid = jid:make(Room, Host),
				  room_queue = RoomQueue,
				  room_shaper = Shaper}),
    add_to_log(room_existence, started, State),
    ejabberd_hooks:run(start_room, ServerHost, [ServerHost, Room, Host]),
    State1 = cleanup_affiliations(State),
    erlang:send_after(?CLEAN_ROOM_TIMEOUT, self(),
                      close_room_if_temporary_and_empty),
    {ok, normal_state, reset_hibernate_timer(State1)}.

normal_state({route, <<"">>,
	      #message{from = From, type = Type, lang = Lang} = Packet},
	     StateData) ->
    case is_user_online(From, StateData) orelse
	is_subscriber(From, StateData) orelse
	is_user_allowed_message_nonparticipant(From, StateData) of
	true when Type == groupchat ->
	    Activity = get_user_activity(From, StateData),
	    Now = erlang:system_time(microsecond),
	    MinMessageInterval = trunc(mod_muc_opt:min_message_interval(StateData#state.server_host) * 1000000),
	    Size = element_size(Packet),
	    {MessageShaper, MessageShaperInterval} =
		ejabberd_shaper:update(Activity#activity.message_shaper, Size),
	    if Activity#activity.message /= undefined ->
		    ErrText = ?T("Traffic rate limit is exceeded"),
		    Err = xmpp:err_resource_constraint(ErrText, Lang),
		    ejabberd_router:route_error(Packet, Err),
		    {next_state, normal_state, StateData};
	       Now >= Activity#activity.message_time + MinMessageInterval,
	       MessageShaperInterval == 0 ->
		    {RoomShaper, RoomShaperInterval} =
			ejabberd_shaper:update(StateData#state.room_shaper, Size),
		    RoomQueueEmpty = case StateData#state.room_queue of
					 undefined -> true;
					 RQ -> p1_queue:is_empty(RQ)
				     end,
		    if RoomShaperInterval == 0, RoomQueueEmpty ->
			    NewActivity = Activity#activity{
					    message_time = Now,
					    message_shaper = MessageShaper},
			    StateData1 = store_user_activity(From,
							     NewActivity,
							     StateData),
			    StateData2 = StateData1#state{room_shaper =
							      RoomShaper},
			    process_groupchat_message(Packet,
						      StateData2);
		       true ->
			    StateData1 = if RoomQueueEmpty ->
						 erlang:send_after(RoomShaperInterval,
								   self(),
								   process_room_queue),
						 StateData#state{room_shaper =
								     RoomShaper};
					    true -> StateData
					 end,
			    NewActivity = Activity#activity{
					    message_time = Now,
					    message_shaper = MessageShaper,
					    message = Packet},
			    RoomQueue = p1_queue:in({message, From},
						    StateData#state.room_queue),
			    StateData2 = store_user_activity(From,
							     NewActivity,
							     StateData1),
			    StateData3 = StateData2#state{room_queue = RoomQueue},
			    {next_state, normal_state, StateData3}
		    end;
	       true ->
		    MessageInterval = (Activity#activity.message_time +
					   MinMessageInterval - Now) div 1000,
		    Interval = lists:max([MessageInterval,
					  MessageShaperInterval]),
		    erlang:send_after(Interval, self(),
				      {process_user_message, From}),
		    NewActivity = Activity#activity{
				    message = Packet,
				    message_shaper = MessageShaper},
		    StateData1 = store_user_activity(From, NewActivity,	StateData),
		    {next_state, normal_state, StateData1}
	    end;
	true when Type == error ->
	    case is_user_online(From, StateData) of
		true ->
		    ErrorText = ?T("It is not allowed to send error messages to the"
				   " room. The participant (~s) has sent an error "
				   "message (~s) and got kicked from the room"),
		    NewState = expulse_participant(Packet, From, StateData,
						   translate:translate(Lang,
								       ErrorText)),
		    close_room_if_temporary_and_empty(NewState);
		_ ->
		    {next_state, normal_state, StateData}
	    end;
	true when Type == chat ->
	    ErrText = ?T("It is not allowed to send private messages "
			 "to the conference"),
	    Err = xmpp:err_not_acceptable(ErrText, Lang),
	    ejabberd_router:route_error(Packet, Err),
	    {next_state, normal_state, StateData};
	true when Type == normal ->
	    {next_state, normal_state,
	     try xmpp:decode_els(Packet) of
		 Pkt -> process_normal_message(From, Pkt, StateData)
	     catch _:{xmpp_codec, Why} ->
		     Txt = xmpp:io_format_error(Why),
		     Err = xmpp:err_bad_request(Txt, Lang),
		     ejabberd_router:route_error(Packet, Err),
		     StateData
	     end};
	true ->
	    ErrText = ?T("Improper message type"),
	    Err = xmpp:err_not_acceptable(ErrText, Lang),
	    ejabberd_router:route_error(Packet, Err),
	    {next_state, normal_state, StateData};
	false when Type /= error ->
	    handle_roommessage_from_nonparticipant(Packet, StateData, From),
	    {next_state, normal_state, StateData};
	false ->
	    {next_state, normal_state, StateData}
    end;
normal_state({route, <<"">>,
	      #iq{from = From, type = Type, lang = Lang, sub_els = [_]} = IQ0},
	     StateData) when Type == get; Type == set ->
    try
	case ejabberd_hooks:run_fold(
	       muc_process_iq,
	       StateData#state.server_host,
	       xmpp:set_from_to(xmpp:decode_els(IQ0),
				From, StateData#state.jid),
	       [StateData]) of
	    ignore ->
		{next_state, normal_state, StateData};
	    #iq{type = T} = IQRes when T == error; T == result ->
		ejabberd_router:route(IQRes),
		{next_state, normal_state, StateData};
	    #iq{sub_els = [SubEl]} = IQ ->
		Res1 = case SubEl of
			   #muc_admin{} ->
			       process_iq_admin(From, IQ, StateData);
			   #muc_owner{} ->
			       process_iq_owner(From, IQ, StateData);
			   #disco_info{} ->
			       process_iq_disco_info(From, IQ, StateData);
			   #disco_items{} ->
			       process_iq_disco_items(From, IQ, StateData);
			   #vcard_temp{} ->
			       process_iq_vcard(From, IQ, StateData);
			   #muc_subscribe{} ->
			       process_iq_mucsub(From, IQ, StateData);
			   #muc_unsubscribe{} ->
			       process_iq_mucsub(From, IQ, StateData);
			   #muc_subscriptions{} ->
			       process_iq_mucsub(From, IQ, StateData);
			   #xcaptcha{} ->
			       process_iq_captcha(From, IQ, StateData);
			   #adhoc_command{} ->
			       process_iq_adhoc(From, IQ, StateData);
			   _ ->
			       Txt = ?T("The feature requested is not "
					"supported by the conference"),
			       {error, xmpp:err_service_unavailable(Txt, Lang)}
		       end,
		{IQRes, NewStateData} =
		    case Res1 of
			{result, Res, SD} ->
			    {xmpp:make_iq_result(IQ, Res), SD};
			{result, Res} ->
			    {xmpp:make_iq_result(IQ, Res), StateData};
			{ignore, SD} ->
			    {ignore, SD};
			{error, Error} ->
			    {xmpp:make_error(IQ0, Error), StateData}
		    end,
		if IQRes /= ignore ->
			ejabberd_router:route(IQRes);
		   true ->
			ok
		end,
		case NewStateData of
		    stop ->
			Conf = StateData#state.config,
			{stop, normal, StateData#state{config = Conf#config{persistent = false}}};
		    _ when NewStateData#state.just_created ->
			close_room_if_temporary_and_empty(NewStateData);
		    _ ->
			{next_state, normal_state, NewStateData}
		end
	end
    catch _:{xmpp_codec, Why} ->
	    ErrTxt = xmpp:io_format_error(Why),
	    Err = xmpp:err_bad_request(ErrTxt, Lang),
	    ejabberd_router:route_error(IQ0, Err),
	    {next_state, normal_state, StateData}
    end;
normal_state({route, <<"">>, #iq{} = IQ}, StateData) ->
    Err = xmpp:err_bad_request(),
    ejabberd_router:route_error(IQ, Err),
    case StateData#state.just_created of
	true -> {stop, normal, StateData};
	_ -> {next_state, normal_state, StateData}
    end;
normal_state({route, Nick, #presence{from = From} = Packet}, StateData) ->
    Activity = get_user_activity(From, StateData),
    Now = erlang:system_time(microsecond),
    MinPresenceInterval =
	trunc(mod_muc_opt:min_presence_interval(StateData#state.server_host) * 1000000),
    if (Now >= Activity#activity.presence_time + MinPresenceInterval)
       and (Activity#activity.presence == undefined) ->
	    NewActivity = Activity#activity{presence_time = Now},
	    StateData1 = store_user_activity(From, NewActivity,
					     StateData),
	    process_presence(Nick, Packet, StateData1);
       true ->
	    if Activity#activity.presence == undefined ->
		    Interval = (Activity#activity.presence_time +
				    MinPresenceInterval - Now) div 1000,
		    erlang:send_after(Interval, self(),
				      {process_user_presence, From});
	       true -> ok
	    end,
	    NewActivity = Activity#activity{presence = {Nick, Packet}},
	    StateData1 = store_user_activity(From, NewActivity,
					     StateData),
	    {next_state, normal_state, StateData1}
    end;
normal_state({route, ToNick,
	      #message{from = From, type = Type, lang = Lang} = Packet},
	     StateData) ->
    case decide_fate_message(Packet, From, StateData) of
	{expulse_sender, Reason} ->
	    ?DEBUG(Reason, []),
	    ErrorText = ?T("It is not allowed to send error messages to the"
			   " room. The participant (~s) has sent an error "
			   "message (~s) and got kicked from the room"),
	    NewState = expulse_participant(Packet, From, StateData,
					   translate:translate(Lang, ErrorText)),
	    {next_state, normal_state, NewState};
	forget_message ->
	    {next_state, normal_state, StateData};
	continue_delivery ->
	    case {(StateData#state.config)#config.allow_private_messages,
		  is_user_online(From, StateData) orelse
		  is_subscriber(From, StateData) orelse
		  is_user_allowed_message_nonparticipant(From, StateData)} of
		{true, true} when Type == groupchat ->
		    ErrText = ?T("It is not allowed to send private messages "
				 "of type \"groupchat\""),
		    Err = xmpp:err_bad_request(ErrText, Lang),
		    ejabberd_router:route_error(Packet, Err);
		{true, true} ->
		    case find_jids_by_nick(ToNick, StateData) of
			[] ->
			    ErrText = ?T("Recipient is not in the conference room"),
			    Err = xmpp:err_item_not_found(ErrText, Lang),
			    ejabberd_router:route_error(Packet, Err);
			ToJIDs ->
			    SrcIsVisitor = is_visitor(From, StateData),
			    DstIsModerator = is_moderator(hd(ToJIDs), StateData),
			    PmFromVisitors =
				(StateData#state.config)#config.allow_private_messages_from_visitors,
			    if SrcIsVisitor == false;
			       PmFromVisitors == anyone;
			       (PmFromVisitors == moderators) and
			       DstIsModerator ->
				   {FromNick, _} = get_participant_data(From, StateData),
				    FromNickJID =
					jid:replace_resource(StateData#state.jid,
							     FromNick),
				    X = #muc_user{},
				    PrivMsg = xmpp:set_from(
						xmpp:set_subtag(Packet, X),
						FromNickJID),
				    lists:foreach(
				      fun(ToJID) ->
					      ejabberd_router:route(xmpp:set_to(PrivMsg, ToJID))
				      end, ToJIDs);
			       true ->
				    ErrText = ?T("It is not allowed to send private messages"),
				    Err = xmpp:err_forbidden(ErrText, Lang),
				    ejabberd_router:route_error(Packet, Err)
			    end
		    end;
		{true, false} ->
		    ErrText = ?T("Only occupants are allowed to send messages "
				 "to the conference"),
		    Err = xmpp:err_not_acceptable(ErrText, Lang),
		    ejabberd_router:route_error(Packet, Err);
		{false, _} ->
		    ErrText = ?T("It is not allowed to send private messages"),
		    Err = xmpp:err_forbidden(ErrText, Lang),
		    ejabberd_router:route_error(Packet, Err)
	    end,
	  {next_state, normal_state, StateData}
    end;
normal_state({route, ToNick,
	      #iq{from = From, lang = Lang} = Packet},
	     #state{config = #config{allow_query_users = AllowQuery}} = StateData) ->
    try maps:get(jid:tolower(From), StateData#state.users) of
	#user{nick = FromNick} when AllowQuery orelse ToNick == FromNick ->
	    case find_jid_by_nick(ToNick, StateData) of
		false ->
		    ErrText = ?T("Recipient is not in the conference room"),
		    Err = xmpp:err_item_not_found(ErrText, Lang),
		    ejabberd_router:route_error(Packet, Err);
		To ->
		    FromJID = jid:replace_resource(StateData#state.jid, FromNick),
		    case direct_iq_type(Packet) of
			vcard ->
			    ejabberd_router:route_iq(
			      xmpp:set_from_to(Packet, FromJID, jid:remove_resource(To)),
			      Packet, self());
			ping when ToNick == FromNick ->
			    %% Self-ping optimization from XEP-0410
			    ejabberd_router:route(xmpp:make_iq_result(Packet));
			response ->
			    ejabberd_router:route(xmpp:set_from_to(Packet, FromJID, To));
			#stanza_error{} = Err ->
			    ejabberd_router:route_error(Packet, Err);
			_OtherRequest ->
			    ejabberd_router:route_iq(
			      xmpp:set_from_to(Packet, FromJID, To), Packet, self())
		    end
	    end;
	_ ->
	    ErrText = ?T("Queries to the conference members are "
			 "not allowed in this room"),
	    Err = xmpp:err_not_allowed(ErrText, Lang),
	    ejabberd_router:route_error(Packet, Err)
    catch _:{badkey, _} ->
	    ErrText = ?T("Only occupants are allowed to send queries "
			 "to the conference"),
	    Err = xmpp:err_not_acceptable(ErrText, Lang),
	    ejabberd_router:route_error(Packet, Err)
    end,
    {next_state, normal_state, StateData};
normal_state(hibernate, StateData) ->
    case maps:size(StateData#state.users) of
	0 ->
	    store_room_no_checks(StateData, [], true),
	    ?INFO_MSG("Hibernating room ~ts@~ts", [StateData#state.room, StateData#state.host]),
	    {stop, normal, StateData#state{hibernate_timer = hibernating}};
	_ ->
	    {next_state, normal_state, StateData}
    end;
normal_state(_Event, StateData) ->
    {next_state, normal_state, StateData}.

handle_event({service_message, Msg}, _StateName,
	     StateData) ->
    MessagePkt = #message{type = groupchat, body = xmpp:mk_text(Msg)},
    send_wrapped_multiple(
      StateData#state.jid,
      get_users_and_subscribers_with_node(?NS_MUCSUB_NODES_MESSAGES, StateData),
      MessagePkt,
      ?NS_MUCSUB_NODES_MESSAGES,
      StateData),
    NSD = add_message_to_history(<<"">>,
				 StateData#state.jid, MessagePkt, StateData),
    {next_state, normal_state, NSD};
handle_event({destroy, Reason}, _StateName,
	     StateData) ->
    _ = destroy_room(#muc_destroy{xmlns = ?NS_MUC_OWNER, reason = Reason}, StateData),
    ?INFO_MSG("Destroyed MUC room ~ts with reason: ~p",
	      [jid:encode(StateData#state.jid), Reason]),
    add_to_log(room_existence, destroyed, StateData),
    Conf = StateData#state.config,
    {stop, shutdown, StateData#state{config = Conf#config{persistent = false}}};
handle_event(destroy, StateName, StateData) ->
    ?INFO_MSG("Destroyed MUC room ~ts",
	      [jid:encode(StateData#state.jid)]),
    handle_event({destroy, <<"">>}, StateName, StateData);
handle_event({set_affiliations, Affiliations},
	     StateName, StateData) ->
    NewStateData = set_affiliations(Affiliations, StateData),
    {next_state, StateName, NewStateData};
handle_event({process_item_change, Item, UJID}, StateName, StateData) ->
    case process_item_change(Item, StateData, UJID) of
	{error, _} ->
            {next_state, StateName, StateData};
        StateData ->
            {next_state, StateName, StateData};
	NSD ->
	    store_room(NSD),
            {next_state, StateName, NSD}
    end;
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event({get_disco_item, Filter, JID, Lang, Time}, _From, StateName, StateData) ->
    Len = maps:size(StateData#state.nicks),
    Reply = case (Filter == all) or (Filter == Len) or ((Filter /= 0) and (Len /= 0)) of
	true ->
	    get_roomdesc_reply(JID, StateData,
			       get_roomdesc_tail(StateData, Lang));
	false ->
	    false
    end,
    CurrentTime = erlang:system_time(millisecond),
    if CurrentTime < Time ->
	    {reply, Reply, StateName, StateData};
       true ->
	    {next_state, StateName, StateData}
    end;
%% These two clauses are only for backward compatibility with nodes running old code
handle_sync_event({get_disco_item, JID, Lang}, From, StateName, StateData) ->
    handle_sync_event({get_disco_item, any, JID, Lang}, From, StateName, StateData);
handle_sync_event({get_disco_item, Filter, JID, Lang}, From, StateName, StateData) ->
    handle_sync_event({get_disco_item, Filter, JID, Lang, infinity}, From, StateName, StateData);
handle_sync_event(get_config, _From, StateName,
		  StateData) ->
    {reply, {ok, StateData#state.config}, StateName,
     StateData};
handle_sync_event(get_state, _From, StateName,
		  StateData) ->
    {reply, {ok, StateData}, StateName, StateData};
handle_sync_event(get_info, _From, StateName,
		  StateData) ->
    Result = #{occupants_number => maps:size(StateData#state.users)},
    {reply, Result, StateName, StateData};
handle_sync_event({change_config, Config}, _From,
		  StateName, StateData) ->
    {result, undefined, NSD} = change_config(Config, StateData),
    {reply, {ok, NSD#state.config}, StateName, NSD};
handle_sync_event({change_state, NewStateData}, _From,
		  StateName, _StateData) ->
    Mod = gen_mod:db_mod(NewStateData#state.server_host, mod_muc),
    case erlang:function_exported(Mod, get_subscribed_rooms, 3) of
	true ->
	    ok;
	_ ->
	    erlang:put(muc_subscribers, NewStateData#state.muc_subscribers#muc_subscribers.subscribers)
    end,
    {reply, {ok, NewStateData}, StateName, NewStateData};
handle_sync_event({process_item_change, Item, UJID}, _From, StateName, StateData) ->
    case process_item_change(Item, StateData, UJID) of
	{error, _} = Err ->
	    {reply, Err, StateName, StateData};
        StateData ->
            {reply, {ok, StateData}, StateName, StateData};
	NSD ->
	    store_room(NSD),
	    {reply, {ok, NSD}, StateName, NSD}
    end;
handle_sync_event(get_subscribers, _From, StateName, StateData) ->
    JIDs = muc_subscribers_fold(
             fun(_LBareJID, #subscriber{jid = JID}, Acc) ->
                     [JID | Acc]
             end, [], StateData#state.muc_subscribers),
    {reply, {ok, JIDs}, StateName, StateData};
handle_sync_event({muc_subscribe, From, Nick, Nodes}, _From,
		  StateName, StateData) ->
    IQ = #iq{type = set, id = p1_rand:get_string(),
	     from = From, sub_els = [#muc_subscribe{nick = Nick,
						    events = Nodes}]},
    Config = StateData#state.config,
    CaptchaRequired = Config#config.captcha_protected,
    PasswordProtected = Config#config.password_protected,
    TmpConfig = Config#config{captcha_protected = false,
			       password_protected = false},
    TmpState = StateData#state{config = TmpConfig},
    case process_iq_mucsub(From, IQ, TmpState) of
	{result, #muc_subscribe{events = NewNodes}, NewState} ->
	    NewConfig = (NewState#state.config)#config{
			  captcha_protected = CaptchaRequired,
			  password_protected = PasswordProtected},
	    {reply, {ok, NewNodes}, StateName,
	     NewState#state{config = NewConfig}};
	{ignore, NewState} ->
	    NewConfig = (NewState#state.config)#config{
			  captcha_protected = CaptchaRequired,
			  password_protected = PasswordProtected},
	    {reply, {error, ?T("Request is ignored")},
	     NewState#state{config = NewConfig}};
	{error, Err} ->
	    {reply, {error, get_error_text(Err)}, StateName, StateData}
    end;
handle_sync_event({muc_unsubscribe, From}, _From, StateName,
		  #state{config = Conf} = StateData) ->
    IQ = #iq{type = set, id = p1_rand:get_string(),
	     from = From, sub_els = [#muc_unsubscribe{}]},
    case process_iq_mucsub(From, IQ, StateData) of
	{result, _, stop} ->
	    {stop, normal, StateData#state{config = Conf#config{persistent = false}}};
	{result, _, NewState} ->
	    {reply, ok, StateName, NewState};
	{ignore, NewState} ->
	    {reply, {error, ?T("Request is ignored")}, NewState};
	{error, Err} ->
	    {reply, {error, get_error_text(Err)}, StateName, StateData}
    end;
handle_sync_event({is_subscribed, From}, _From, StateName, StateData) ->
    IsSubs = try muc_subscribers_get(
                   jid:split(From), StateData#state.muc_subscribers) of
		 #subscriber{nick = Nick, nodes = Nodes} -> {true, Nick, Nodes}
	     catch _:{badkey, _} -> false
	     end,
    {reply, IsSubs, StateName, StateData};
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_info({process_user_presence, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = p1_queue:is_empty(StateData#state.room_queue),
    RoomQueue = p1_queue:in({presence, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if RoomQueueEmpty ->
	   StateData2 = prepare_room_queue(StateData1),
	   {next_state, normal_state, StateData2};
       true -> {next_state, normal_state, StateData1}
    end;
handle_info({process_user_message, From},
	    normal_state = _StateName, StateData) ->
    RoomQueueEmpty =
	p1_queue:is_empty(StateData#state.room_queue),
    RoomQueue = p1_queue:in({message, From},
			    StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if RoomQueueEmpty ->
	   StateData2 = prepare_room_queue(StateData1),
	   {next_state, normal_state, StateData2};
       true -> {next_state, normal_state, StateData1}
    end;
handle_info(process_room_queue,
	    normal_state = StateName, StateData) ->
    case p1_queue:out(StateData#state.room_queue) of
      {{value, {message, From}}, RoomQueue} ->
	  Activity = get_user_activity(From, StateData),
	  Packet = Activity#activity.message,
	  NewActivity = Activity#activity{message = undefined},
	  StateData1 = store_user_activity(From, NewActivity,
					   StateData),
	  StateData2 = StateData1#state{room_queue = RoomQueue},
	  StateData3 = prepare_room_queue(StateData2),
	  process_groupchat_message(Packet, StateData3);
      {{value, {presence, From}}, RoomQueue} ->
	  Activity = get_user_activity(From, StateData),
	  {Nick, Packet} = Activity#activity.presence,
	  NewActivity = Activity#activity{presence = undefined},
	  StateData1 = store_user_activity(From, NewActivity,
					   StateData),
	  StateData2 = StateData1#state{room_queue = RoomQueue},
	  StateData3 = prepare_room_queue(StateData2),
	  process_presence(Nick, Packet, StateData3);
      {empty, _} -> {next_state, StateName, StateData}
    end;
handle_info({captcha_succeed, From}, normal_state,
	    StateData) ->
    NewState = case maps:get(From, StateData#state.robots, passed) of
		   {Nick, Packet} ->
		       Robots = maps:put(From, passed, StateData#state.robots),
		       add_new_user(From, Nick, Packet,
				    StateData#state{robots = Robots});
		   passed ->
		       StateData
	       end,
    {next_state, normal_state, NewState};
handle_info({captcha_failed, From}, normal_state,
	    StateData) ->
    NewState = case maps:get(From, StateData#state.robots, passed) of
		   {_Nick, Packet} ->
		       Robots = maps:remove(From, StateData#state.robots),
		       Txt = ?T("The CAPTCHA verification has failed"),
		       Lang = xmpp:get_lang(Packet),
		       Err = xmpp:err_not_authorized(Txt, Lang),
		       ejabberd_router:route_error(Packet, Err),
		       StateData#state{robots = Robots};
		   passed ->
		       StateData
	       end,
    {next_state, normal_state, NewState};
handle_info(close_room_if_temporary_and_empty, _StateName, StateData) ->
    close_room_if_temporary_and_empty(StateData);
handle_info(shutdown, _StateName, StateData) ->
    {stop, shutdown, StateData};
handle_info({iq_reply, #iq{type = Type, sub_els = Els},
	     #iq{from = From, to = To} = IQ}, StateName, StateData) ->
    ejabberd_router:route(
      xmpp:set_from_to(
	IQ#iq{type = Type, sub_els = Els},
	To, From)),
    {next_state, StateName, StateData};
handle_info({iq_reply, timeout, IQ}, StateName, StateData) ->
    Txt = ?T("Request has timed out"),
    Err = xmpp:err_recipient_unavailable(Txt, IQ#iq.lang),
    ejabberd_router:route_error(IQ, Err),
    {next_state, StateName, StateData};
handle_info(config_reloaded, StateName, StateData) ->
    Max = mod_muc_opt:history_size(StateData#state.server_host),
    History1 = StateData#state.history,
    Q1 = History1#lqueue.queue,
    Q2 = case p1_queue:len(Q1) of
	     Len when Len > Max ->
		 lqueue_cut(Q1, Len-Max);
	     _ ->
		 Q1
	 end,
    History2 = History1#lqueue{queue = Q2, max = Max},
    {next_state, StateName, StateData#state{history = History2}};
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, _StateName,
	  #state{server_host = LServer, host = Host, room = Room} = StateData) ->
    try
	?INFO_MSG("Stopping MUC room ~ts@~ts", [Room, Host]),
	ReasonT = case Reason of
		      shutdown ->
			  ?T("You are being removed from the room "
			     "because of a system shutdown");
		      _ -> ?T("Room terminates")
		  end,
	Packet = #presence{
		    type = unavailable,
		    sub_els = [#muc_user{items = [#muc_item{affiliation = none,
							    reason = ReasonT,
							    role = none}],
					 status_codes = [332,110]}]},
	maps:fold(
	  fun(_, #user{nick = Nick, jid = JID}, _) ->
		  case Reason of
		      shutdown ->
			  send_wrapped(jid:replace_resource(StateData#state.jid, Nick),
				       JID, Packet,
				       ?NS_MUCSUB_NODES_PARTICIPANTS,
				       StateData);
		      _ -> ok
		  end,
		  tab_remove_online_user(JID, StateData)
	  end, [], get_users_and_subscribers_with_node(
                     ?NS_MUCSUB_NODES_PARTICIPANTS, StateData)),

	disable_hibernate_timer(StateData),
	case StateData#state.hibernate_timer of
	    hibernating ->
		ok;
	    _ ->
		add_to_log(room_existence, stopped, StateData),
		case (StateData#state.config)#config.persistent of
		    false ->
			ejabberd_hooks:run(room_destroyed, LServer, [LServer, Room, Host]);
		    _ ->
			ok
		end
	end
    catch ?EX_RULE(E, R, St) ->
	    StackTrace = ?EX_STACK(St),
	    ?ERROR_MSG("Got exception on room termination:~n** ~ts",
		       [misc:format_exception(2, E, R, StackTrace)])
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
-spec route(pid(), stanza()) -> ok.
route(Pid, Packet) ->
    ?DEBUG("Routing to MUC room ~p:~n~ts", [Pid, xmpp:pp(Packet)]),
    #jid{lresource = Nick} = xmpp:get_to(Packet),
    p1_fsm:send_event(Pid, {route, Nick, Packet}).

-spec process_groupchat_message(message(), state()) -> fsm_next().
process_groupchat_message(#message{from = From, lang = Lang} = Packet, StateData) ->
    IsSubscriber = is_subscriber(From, StateData),
    case is_user_online(From, StateData) orelse IsSubscriber orelse
	   is_user_allowed_message_nonparticipant(From, StateData)
	of
      true ->
	  {FromNick, Role} = get_participant_data(From, StateData),
	  #config{moderated = Moderated} = StateData#state.config,
	  AllowedByModerationRules =
	  case {Role == moderator orelse Role == participant orelse
		not Moderated, IsSubscriber} of
	      {true, _} -> true;
	      {_, true} ->
		  case get_default_role(get_affiliation(From, StateData),
					StateData) of
		      moderator -> true;
		      participant -> true;
		      _ -> false
		  end;
	      _ ->
		  false
	  end,
	  if AllowedByModerationRules ->
	      Subject = check_subject(Packet),
	      {NewStateData1, IsAllowed} =
	      case Subject of
		  [] ->
		      {StateData, true};
		  _ ->
		      case
			  can_change_subject(Role,
					     IsSubscriber,
					     StateData)
		      of
			  true ->
			      NSD =
			      StateData#state{subject = Subject,
					      subject_author = FromNick},
			      store_room(NSD),
			      {NSD, true};
			  _ -> {StateData, false}
		      end
	      end,
	      case IsAllowed of
		   true ->
		       case
			 ejabberd_hooks:run_fold(muc_filter_message,
						 StateData#state.server_host,
						 Packet,
						 [StateData, FromNick])
			   of
			 drop ->
			     {next_state, normal_state, StateData};
			 NewPacket1 ->
			     NewPacket = xmpp:put_meta(xmpp:remove_subtag(NewPacket1, #nick{}),
				 muc_sender_real_jid, From),
			     Node = if Subject == [] -> ?NS_MUCSUB_NODES_MESSAGES;
				       true -> ?NS_MUCSUB_NODES_SUBJECT
				    end,
			     send_wrapped_multiple(
			       jid:replace_resource(StateData#state.jid, FromNick),
			       get_users_and_subscribers_with_node(Node, StateData),
			       NewPacket, Node, NewStateData1),
			     NewStateData2 = case has_body_or_subject(NewPacket) of
					       true ->
						   add_message_to_history(FromNick, From,
									  NewPacket,
									  NewStateData1);
					       false ->
						   NewStateData1
					     end,
			     {next_state, normal_state, NewStateData2}
		       end;
		   _ ->
		       Err = case (StateData#state.config)#config.allow_change_subj of
			       true ->
				   xmpp:err_forbidden(
				     ?T("Only moderators and participants are "
					"allowed to change the subject in this "
					"room"), Lang);
			       _ ->
				   xmpp:err_forbidden(
				     ?T("Only moderators are allowed to change "
					"the subject in this room"), Lang)
			     end,
		       ejabberd_router:route_error(Packet, Err),
		       {next_state, normal_state, StateData}
		 end;
	     true ->
		 ErrText = ?T("Visitors are not allowed to send messages "
			      "to all occupants"),
		 Err = xmpp:err_forbidden(ErrText, Lang),
		 ejabberd_router:route_error(Packet, Err),
		 {next_state, normal_state, StateData}
	  end;
      false ->
	  ErrText = ?T("Only occupants are allowed to send messages "
		       "to the conference"),
	  Err = xmpp:err_not_acceptable(ErrText, Lang),
	  ejabberd_router:route_error(Packet, Err),
	  {next_state, normal_state, StateData}
    end.

-spec process_normal_message(jid(), message(), state()) -> state().
process_normal_message(From, #message{lang = Lang} = Pkt, StateData) ->
    Action = lists:foldl(
	       fun(_, {error, _} = Err) ->
		       Err;
		  (_, {ok, _} = Result) ->
		       Result;
		  (#muc_user{invites = [_|_] = Invites}, _) ->
		       case check_invitation(From, Invites, Lang, StateData) of
			   ok ->
			       {ok, Invites};
			   {error, _} = Err ->
			       Err
		       end;
		  (#xdata{type = submit, fields = Fs}, _) ->
		       try {ok, muc_request:decode(Fs)}
		       catch _:{muc_request, Why} ->
			       Txt = muc_request:format_error(Why),
			       {error, xmpp:err_bad_request(Txt, Lang)}
		       end;
		  (_, Acc) ->
		       Acc
	       end, ok, xmpp:get_els(Pkt)),
    case Action of
	{ok, [#muc_invite{}|_] = Invitations} ->
	    lists:foldl(
	      fun(Invitation, AccState) ->
		      process_invitation(From, Pkt, Invitation, Lang, AccState)
	      end, StateData, Invitations);
	{ok, [{role, participant}]} ->
	    process_voice_request(From, Pkt, StateData);
	{ok, VoiceApproval} ->
	    process_voice_approval(From, Pkt, VoiceApproval, StateData);
	{error, Err} ->
	    ejabberd_router:route_error(Pkt, Err),
	    StateData;
	ok ->
	    StateData
    end.

-spec process_invitation(jid(), message(), muc_invite(), binary(), state()) -> state().
process_invitation(From, Pkt, Invitation, Lang, StateData) ->
    IJID = route_invitation(From, Pkt, Invitation, Lang, StateData),
    Config = StateData#state.config,
    case Config#config.members_only of
	true ->
	    case get_affiliation(IJID, StateData) of
		none ->
		    NSD = set_affiliation(IJID, member, StateData),
		    send_affiliation(IJID, member, StateData),
		    store_room(NSD),
		    NSD;
		_ ->
		    StateData
	    end;
	false ->
	    StateData
    end.

-spec process_voice_request(jid(), message(), state()) -> state().
process_voice_request(From, Pkt, StateData) ->
    Lang = xmpp:get_lang(Pkt),
    case (StateData#state.config)#config.allow_voice_requests of
	true ->
	    MinInterval = (StateData#state.config)#config.voice_request_min_interval,
	    BareFrom = jid:remove_resource(jid:tolower(From)),
	    NowPriority = -erlang:system_time(microsecond),
	    CleanPriority = NowPriority + MinInterval * 1000000,
	    Times = clean_treap(StateData#state.last_voice_request_time,
				CleanPriority),
	    case treap:lookup(BareFrom, Times) of
		error ->
		    Times1 = treap:insert(BareFrom,
					  NowPriority,
					  true, Times),
		    NSD = StateData#state{last_voice_request_time = Times1},
		    send_voice_request(From, Lang, NSD),
		    NSD;
		{ok, _, _} ->
		    ErrText = ?T("Please, wait for a while before sending "
				 "new voice request"),
		    Err = xmpp:err_resource_constraint(ErrText, Lang),
		    ejabberd_router:route_error(Pkt, Err),
		    StateData#state{last_voice_request_time = Times}
	    end;
	false ->
	    ErrText = ?T("Voice requests are disabled in this conference"),
	    Err = xmpp:err_forbidden(ErrText, Lang),
	    ejabberd_router:route_error(Pkt, Err),
	    StateData
    end.

-spec process_voice_approval(jid(), message(), [muc_request:property()], state()) -> state().
process_voice_approval(From, Pkt, VoiceApproval, StateData) ->
    Lang = xmpp:get_lang(Pkt),
    case is_moderator(From, StateData) of
	true ->
	    case lists:keyfind(jid, 1, VoiceApproval) of
		{_, TargetJid} ->
		    Allow = proplists:get_bool(request_allow, VoiceApproval),
		    case is_visitor(TargetJid, StateData) of
			true when Allow ->
			    Reason = <<>>,
			    NSD = set_role(TargetJid, participant, StateData),
			    catch send_new_presence(
				    TargetJid, Reason, NSD, StateData),
			    NSD;
			_ ->
			    StateData
		    end;
		false ->
		    ErrText = ?T("Failed to extract JID from your voice "
				 "request approval"),
		    Err = xmpp:err_bad_request(ErrText, Lang),
		    ejabberd_router:route_error(Pkt, Err),
		    StateData
	    end;
	false ->
	    ErrText = ?T("Only moderators can approve voice requests"),
	    Err = xmpp:err_not_allowed(ErrText, Lang),
	    ejabberd_router:route_error(Pkt, Err),
	    StateData
    end.

-spec direct_iq_type(iq()) -> vcard | ping | request | response | stanza_error().
direct_iq_type(#iq{type = T, sub_els = SubEls, lang = Lang}) when T == get; T == set ->
    case SubEls of
	[El] ->
	    case xmpp:get_ns(El) of
		?NS_VCARD when T == get -> vcard;
		?NS_PING when T == get -> ping;
		_ -> request
	    end;
	[] ->
	    xmpp:err_bad_request(?T("No child elements found"), Lang);
	[_|_] ->
	    xmpp:err_bad_request(?T("Too many child elements"), Lang)
    end;
direct_iq_type(#iq{}) ->
    response.

%% @doc Check if this non participant can send message to room.
%%
%% XEP-0045 v1.23:
%% 7.9 Sending a Message to All Occupants
%% an implementation MAY allow users with certain privileges
%% (e.g., a room owner, room admin, or service-level admin)
%% to send messages to the room even if those users are not occupants.
-spec is_user_allowed_message_nonparticipant(jid(), state()) -> boolean().
is_user_allowed_message_nonparticipant(JID,
				       StateData) ->
    case get_service_affiliation(JID, StateData) of
      owner -> true;
      _ -> false
    end.

%% @doc Get information of this participant, or default values.
%% If the JID is not a participant, return values for a service message.
-spec get_participant_data(jid(), state()) -> {binary(), role()}.
get_participant_data(From, StateData) ->
    try maps:get(jid:tolower(From), StateData#state.users) of
	#user{nick = FromNick, role = Role} ->
	    {FromNick, Role}
    catch _:{badkey, _} ->
	    try muc_subscribers_get(jid:tolower(jid:remove_resource(From)),
                                    StateData#state.muc_subscribers) of
		#subscriber{nick = FromNick} ->
		    {FromNick, none}
	    catch _:{badkey, _} ->
		    {From#jid.luser, moderator}
	    end
    end.

-spec process_presence(binary(), presence(), state()) -> fsm_transition().
process_presence(Nick, #presence{from = From, type = Type0} = Packet0, StateData) ->
    IsOnline = is_user_online(From, StateData),
    if Type0 == available;
       IsOnline and ((Type0 == unavailable) or (Type0 == error)) ->
	   case ejabberd_hooks:run_fold(muc_filter_presence,
					StateData#state.server_host,
					Packet0,
					[StateData, Nick]) of
	     drop ->
		 {next_state, normal_state, StateData};
	     #presence{} = Packet ->
		 close_room_if_temporary_and_empty(
		   do_process_presence(Nick, Packet, StateData))
	   end;
       true ->
	    {next_state, normal_state, StateData}
    end.

-spec do_process_presence(binary(), presence(), state()) -> state().
do_process_presence(Nick, #presence{from = From, type = available, lang = Lang} = Packet,
		    StateData) ->
    case is_user_online(From, StateData) of
	false ->
	    add_new_user(From, Nick, Packet, StateData);
	true ->
	    case is_nick_change(From, Nick, StateData) of
		true ->
		    case {nick_collision(From, Nick, StateData),
			  mod_muc:can_use_nick(StateData#state.server_host,
					       StateData#state.host,
					       From, Nick),
			  {(StateData#state.config)#config.allow_visitor_nickchange,
			   is_visitor(From, StateData)}} of
			{_, _, {false, true}} ->
			    Packet1 = Packet#presence{sub_els = [#muc{}]},
			    ErrText = ?T("Visitors are not allowed to change their "
					 "nicknames in this room"),
			    Err = xmpp:err_not_allowed(ErrText, Lang),
			    ejabberd_router:route_error(Packet1, Err),
			    StateData;
			{true, _, _} ->
			    Packet1 = Packet#presence{sub_els = [#muc{}]},
			    ErrText = ?T("That nickname is already in use by another "
					 "occupant"),
			    Err = xmpp:err_conflict(ErrText, Lang),
			    ejabberd_router:route_error(Packet1, Err),
			    StateData;
			{_, false, _} ->
			    Packet1 = Packet#presence{sub_els = [#muc{}]},
			    Err = case Nick of
				      <<>> ->
					  xmpp:err_jid_malformed(?T("Nickname can't be empty"),
								 Lang);
				      _ ->
					  xmpp:err_conflict(?T("That nickname is registered"
							       " by another person"), Lang)
				  end,
			    ejabberd_router:route_error(Packet1, Err),
			    StateData;
			_ ->
			    change_nick(From, Nick, StateData)
		    end;
		false ->
		    Stanza = maybe_strip_status_from_presence(
			       From, Packet, StateData),
		    NewState = add_user_presence(From, Stanza,
						 StateData),
		    case xmpp:has_subtag(Packet, #muc{}) of
			true ->
			    send_initial_presences_and_messages(
			      From, Nick, Packet, NewState, StateData);
			false ->
			    send_new_presence(From, NewState, StateData)
		    end,
		    NewState
	    end
    end;
do_process_presence(Nick, #presence{from = From, type = unavailable} = Packet,
		    StateData) ->
    NewPacket = case {(StateData#state.config)#config.allow_visitor_status,
		      is_visitor(From, StateData)} of
		    {false, true} ->
			strip_status(Packet);
		    _ -> Packet
		end,
    NewState = add_user_presence_un(From, NewPacket, StateData),
    case maps:get(Nick, StateData#state.nicks, []) of
	[_, _ | _] ->
	    Aff = get_affiliation(From, StateData),
	    Item = #muc_item{affiliation = Aff, role = none, jid = From},
	    Pres = xmpp:set_subtag(
		     Packet, #muc_user{items = [Item],
				       status_codes = [110]}),
	    send_wrapped(jid:replace_resource(StateData#state.jid, Nick),
			 From, Pres, ?NS_MUCSUB_NODES_PRESENCE, StateData);
	_ ->
	    send_new_presence(From, NewState, StateData)
    end,
    Reason = xmpp:get_text(NewPacket#presence.status),
    remove_online_user(From, NewState, Reason);
do_process_presence(_Nick, #presence{from = From, type = error, lang = Lang} = Packet,
		    StateData) ->
    ErrorText = ?T("It is not allowed to send error messages to the"
		   " room. The participant (~s) has sent an error "
		   "message (~s) and got kicked from the room"),
    expulse_participant(Packet, From, StateData,
			translate:translate(Lang, ErrorText)).

-spec maybe_strip_status_from_presence(jid(), presence(),
				       state()) -> presence().
maybe_strip_status_from_presence(From, Packet, StateData) ->
    case {(StateData#state.config)#config.allow_visitor_status,
	  is_visitor(From, StateData)} of
	{false, true} ->
	    strip_status(Packet);
	_Allowed -> Packet
    end.

-spec close_room_if_temporary_and_empty(state()) -> fsm_transition().
close_room_if_temporary_and_empty(StateData1) ->
    case not (StateData1#state.config)#config.persistent
	andalso maps:size(StateData1#state.users) == 0
	andalso muc_subscribers_size(StateData1#state.muc_subscribers) == 0 of
      true ->
	  ?INFO_MSG("Destroyed MUC room ~ts because it's temporary "
		    "and empty",
		    [jid:encode(StateData1#state.jid)]),
	  add_to_log(room_existence, destroyed, StateData1),
	  forget_room(StateData1),
	  {stop, normal, StateData1};
      _ -> {next_state, normal_state, StateData1}
    end.

-spec get_users_and_subscribers(state()) -> users().
get_users_and_subscribers(StateData) ->
    get_users_and_subscribers_aux(
      StateData#state.muc_subscribers#muc_subscribers.subscribers,
      StateData).

-spec get_users_and_subscribers_with_node(binary(), state()) -> users().
get_users_and_subscribers_with_node(Node, StateData) ->
    get_users_and_subscribers_aux(
      muc_subscribers_get_by_node(Node, StateData#state.muc_subscribers),
      StateData).

get_users_and_subscribers_aux(Subscribers, StateData) ->
    OnlineSubscribers = maps:fold(
			   fun(LJID, _, Acc) ->
				   LBareJID = jid:remove_resource(LJID),
				   case is_subscriber(LBareJID, StateData) of
				       true ->
					   ?SETS:add_element(LBareJID, Acc);
				       false ->
					   Acc
				   end
			   end, ?SETS:new(), StateData#state.users),
    maps:fold(
       fun(LBareJID, #subscriber{nick = Nick}, Acc) ->
	       case ?SETS:is_element(LBareJID, OnlineSubscribers) of
		   false ->
		       maps:put(LBareJID,
				#user{jid = jid:make(LBareJID),
				      nick = Nick,
				      role = none,
				      last_presence = undefined},
				Acc);
		   true ->
		       Acc
	       end
       end, StateData#state.users, Subscribers).

-spec is_user_online(jid(), state()) -> boolean().
is_user_online(JID, StateData) ->
    LJID = jid:tolower(JID),
    maps:is_key(LJID, StateData#state.users).

-spec is_subscriber(jid(), state()) -> boolean().
is_subscriber(JID, StateData) ->
    LJID = jid:tolower(jid:remove_resource(JID)),
    muc_subscribers_is_key(LJID, StateData#state.muc_subscribers).

%% Check if the user is occupant of the room, or at least is an admin or owner.
-spec is_occupant_or_admin(jid(), state()) -> boolean().
is_occupant_or_admin(JID, StateData) ->
    FAffiliation = get_affiliation(JID, StateData),
    FRole = get_role(JID, StateData),
    case FRole /= none orelse
	   FAffiliation == member orelse
	   FAffiliation == admin orelse FAffiliation == owner
	of
      true -> true;
      _ -> false
    end.

%% Check if the user is an admin or owner.
-spec is_admin(jid(), state()) -> boolean().
is_admin(JID, StateData) ->
    FAffiliation = get_affiliation(JID, StateData),
    FAffiliation == admin orelse FAffiliation == owner.

%% Decide the fate of the message and its sender
%% Returns: continue_delivery | forget_message | {expulse_sender, Reason}
-spec decide_fate_message(message(), jid(), state()) ->
				 continue_delivery | forget_message |
				 {expulse_sender, binary()}.
decide_fate_message(#message{type = error} = Msg,
		    From, StateData) ->
    Err = xmpp:get_error(Msg),
    PD = case check_error_kick(Err) of
	   %% If this is an error stanza and its condition matches a criteria
	   true ->
	       Reason = str:format("This participant is considered a ghost "
				   "and is expulsed: ~s",
				   [jid:encode(From)]),
	       {expulse_sender, Reason};
	   false -> continue_delivery
	 end,
    case PD of
      {expulse_sender, R} ->
	  case is_user_online(From, StateData) of
	    true -> {expulse_sender, R};
	    false -> forget_message
	  end;
      Other -> Other
    end;
decide_fate_message(_, _, _) -> continue_delivery.

%% Check if the elements of this error stanza indicate
%% that the sender is a dead participant.
%% If so, return true to kick the participant.
-spec check_error_kick(stanza_error()) -> boolean().
check_error_kick(#stanza_error{reason = Reason}) ->
    case Reason of
	#gone{} -> true;
	'internal-server-error' -> true;
	'item-not-found' -> true;
	'jid-malformed' -> true;
	'recipient-unavailable' -> true;
	#redirect{} -> true;
	'remote-server-not-found' -> true;
	'remote-server-timeout' -> true;
	'service-unavailable' -> true;
	_ -> false
    end;
check_error_kick(undefined) ->
    false.

-spec get_error_condition(stanza_error()) -> string().
get_error_condition(#stanza_error{reason = Reason}) ->
    case Reason of
	#gone{} -> "gone";
	#redirect{} -> "redirect";
	Atom -> atom_to_list(Atom)
    end;
get_error_condition(undefined) ->
    "undefined".

-spec get_error_text(stanza_error()) -> binary().
get_error_text(#stanza_error{text = Txt}) ->
    xmpp:get_text(Txt).

-spec make_reason(stanza(), jid(), state(), binary()) -> binary().
make_reason(Packet, From, StateData, Reason1) ->
    #user{nick = FromNick} = maps:get(jid:tolower(From), StateData#state.users),
    Condition = get_error_condition(xmpp:get_error(Packet)),
    Reason2 = unicode:characters_to_list(Reason1),
    str:format(Reason2, [FromNick, Condition]).

-spec expulse_participant(stanza(), jid(), state(), binary()) ->
				 state().
expulse_participant(Packet, From, StateData, Reason1) ->
    Reason2 = make_reason(Packet, From, StateData, Reason1),
    NewState = add_user_presence_un(From,
				    #presence{type = unavailable,
					      status = xmpp:mk_text(Reason2)},
				    StateData),
    LJID = jid:tolower(From),
    #user{nick = Nick} = maps:get(LJID, StateData#state.users),
    case maps:get(Nick, StateData#state.nicks, []) of
	[_, _ | _] ->
	    Aff = get_affiliation(From, StateData),
	    Item = #muc_item{affiliation = Aff, role = none, jid = From},
	    Pres = xmpp:set_subtag(
		     Packet, #muc_user{items = [Item],
				       status_codes = [110]}),
	    send_wrapped(jid:replace_resource(StateData#state.jid, Nick),
			 From, Pres, ?NS_MUCSUB_NODES_PRESENCE, StateData);
	_ ->
	    send_new_presence(From, NewState, StateData)
    end,
    remove_online_user(From, NewState).

-spec set_affiliation(jid(), affiliation(), state()) -> state().
set_affiliation(JID, Affiliation, StateData) ->
    set_affiliation(JID, Affiliation, StateData, <<"">>).

-spec set_affiliation(jid(), affiliation(), state(), binary()) -> state().
set_affiliation(JID, Affiliation,
		#state{config = #config{persistent = false}} = StateData,
		Reason) ->
    set_affiliation_fallback(JID, Affiliation, StateData, Reason);
set_affiliation(JID, Affiliation, StateData, Reason) ->
    ServerHost = StateData#state.server_host,
    Room = StateData#state.room,
    Host = StateData#state.host,
    Mod = gen_mod:db_mod(ServerHost, mod_muc),
    case Mod:set_affiliation(ServerHost, Room, Host, JID, Affiliation, Reason) of
	ok ->
	    StateData;
	{error, _} ->
	    set_affiliation_fallback(JID, Affiliation, StateData, Reason)
    end.

-spec set_affiliation_fallback(jid(), affiliation(), state(), binary()) -> state().
set_affiliation_fallback(JID, Affiliation, StateData, Reason) ->
    LJID = jid:remove_resource(jid:tolower(JID)),
    Affiliations = case Affiliation of
		       none ->
			   maps:remove(LJID, StateData#state.affiliations);
		       _ ->
			   maps:put(LJID, {Affiliation, Reason},
				    StateData#state.affiliations)
		   end,
    StateData#state{affiliations = Affiliations}.

-spec set_affiliations(affiliations(), state()) -> state().
set_affiliations(Affiliations,
                 #state{config = #config{persistent = false}} = StateData) ->
    set_affiliations_fallback(Affiliations, StateData);
set_affiliations(Affiliations, StateData) ->
    Room = StateData#state.room,
    Host = StateData#state.host,
    ServerHost = StateData#state.server_host,
    Mod = gen_mod:db_mod(ServerHost, mod_muc),
    case Mod:set_affiliations(ServerHost, Room, Host, Affiliations) of
	ok ->
	    StateData;
	{error, _} ->
	    set_affiliations_fallback(Affiliations, StateData)
    end.

-spec set_affiliations_fallback(affiliations(), state()) -> state().
set_affiliations_fallback(Affiliations, StateData) ->
    StateData#state{affiliations = Affiliations}.

-spec get_affiliation(ljid() | jid(), state()) -> affiliation().
get_affiliation(#jid{} = JID, StateData) ->
    case get_service_affiliation(JID, StateData) of
        owner ->
            owner;
        none ->
            case do_get_affiliation(JID, StateData) of
                {Affiliation, _Reason} -> Affiliation;
                Affiliation -> Affiliation
            end
    end;
get_affiliation(LJID, StateData) ->
    get_affiliation(jid:make(LJID), StateData).

-spec do_get_affiliation(jid(), state()) -> affiliation() | {affiliation(), binary()}.
do_get_affiliation(JID, #state{config = #config{persistent = false}} = StateData) ->
    do_get_affiliation_fallback(JID, StateData);
do_get_affiliation(JID, StateData) ->
    Room = StateData#state.room,
    Host = StateData#state.host,
    LServer = JID#jid.lserver,
    LUser = JID#jid.luser,
    ServerHost = StateData#state.server_host,
    Mod = gen_mod:db_mod(ServerHost, mod_muc),
    case Mod:get_affiliation(ServerHost, Room, Host, LUser, LServer) of
	{error, _} ->
	    do_get_affiliation_fallback(JID, StateData);
	{ok, Affiliation} ->
	    Affiliation
    end.

-spec do_get_affiliation_fallback(jid(), state()) -> affiliation() | {affiliation(),  binary()}.
do_get_affiliation_fallback(JID, StateData) ->
    LJID = jid:tolower(JID),
    try maps:get(LJID, StateData#state.affiliations)
    catch _:{badkey, _} ->
            BareLJID = jid:remove_resource(LJID),
            try maps:get(BareLJID, StateData#state.affiliations)
	    catch _:{badkey, _} ->
                    DomainLJID = setelement(1, LJID, <<"">>),
                    try maps:get(DomainLJID, StateData#state.affiliations)
		    catch _:{badkey, _} ->
                            DomainBareLJID = jid:remove_resource(DomainLJID),
                            try maps:get(DomainBareLJID, StateData#state.affiliations)
			    catch _:{badkey, _} -> none
                            end
                    end
            end
    end.

-spec get_affiliations(state()) -> affiliations().
get_affiliations(#state{config = #config{persistent = false}} = StateData) ->
    get_affiliations_fallback(StateData);
get_affiliations(StateData) ->
    Room = StateData#state.room,
    Host = StateData#state.host,
    ServerHost = StateData#state.server_host,
    Mod = gen_mod:db_mod(ServerHost, mod_muc),
    case Mod:get_affiliations(ServerHost, Room, Host) of
	{error, _} ->
	    get_affiliations_fallback(StateData);
	{ok, Affiliations} ->
	    Affiliations
    end.

-spec get_affiliations_fallback(state()) -> affiliations().
get_affiliations_fallback(StateData) ->
    StateData#state.affiliations.

-spec get_service_affiliation(jid(), state()) -> owner | none.
get_service_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin,
     _AccessPersistent, _AccessMam} =
	StateData#state.access,
    case acl:match_rule(StateData#state.server_host,
			AccessAdmin, JID)
	of
      allow -> owner;
      _ -> none
    end.

-spec set_role(jid(), role(), state()) -> state().
set_role(JID, Role, StateData) ->
    LJID = jid:tolower(JID),
    LJIDs = case LJID of
	      {U, S, <<"">>} ->
		  maps:fold(fun (J, _, Js) ->
				    case J of
					{U, S, _} -> [J | Js];
					_ -> Js
				    end
			    end, [], StateData#state.users);
	      _ ->
		  case maps:is_key(LJID, StateData#state.users) of
		    true -> [LJID];
		    _ -> []
		  end
	    end,
    {Users, Nicks} =
	case Role of
	    none ->
		lists:foldl(
		  fun (J, {Us, Ns}) ->
			  NewNs = try maps:get(J, Us) of
				      #user{nick = Nick} ->
					  maps:remove(Nick, Ns)
				  catch _:{badkey, _} ->
					  Ns
				  end,
			  {maps:remove(J, Us), NewNs}
		  end,
		  {StateData#state.users, StateData#state.nicks}, LJIDs);
	    _ ->
		{lists:foldl(
		   fun (J, Us) ->
			   User = maps:get(J, Us),
			   if User#user.last_presence == undefined ->
				   Us;
			      true ->
				   maps:put(J, User#user{role = Role}, Us)
			   end
		   end, StateData#state.users, LJIDs),
		 StateData#state.nicks}
	end,
    StateData#state{users = Users, nicks = Nicks}.

-spec get_role(jid(), state()) -> role().
get_role(JID, StateData) ->
    LJID = jid:tolower(JID),
    try maps:get(LJID, StateData#state.users) of
	#user{role = Role} -> Role
    catch _:{badkey, _} -> none
    end.

-spec get_default_role(affiliation(), state()) -> role().
get_default_role(Affiliation, StateData) ->
    case Affiliation of
      owner -> moderator;
      admin -> moderator;
      member -> participant;
      outcast -> none;
      none ->
	  case (StateData#state.config)#config.members_only of
	    true -> none;
	    _ ->
		case (StateData#state.config)#config.members_by_default
		    of
		  true -> participant;
		  _ -> visitor
		end
	  end
    end.

-spec is_visitor(jid(), state()) -> boolean().
is_visitor(Jid, StateData) ->
    get_role(Jid, StateData) =:= visitor.

-spec is_moderator(jid(), state()) -> boolean().
is_moderator(Jid, StateData) ->
    get_role(Jid, StateData) =:= moderator.

-spec get_max_users(state()) -> non_neg_integer().
get_max_users(StateData) ->
    MaxUsers = (StateData#state.config)#config.max_users,
    ServiceMaxUsers = get_service_max_users(StateData),
    if MaxUsers =< ServiceMaxUsers -> MaxUsers;
       true -> ServiceMaxUsers
    end.

-spec get_service_max_users(state()) -> pos_integer().
get_service_max_users(StateData) ->
    mod_muc_opt:max_users(StateData#state.server_host).

-spec get_max_users_admin_threshold(state()) -> pos_integer().
get_max_users_admin_threshold(StateData) ->
    mod_muc_opt:max_users_admin_threshold(StateData#state.server_host).

-spec room_queue_new(binary(), ejabberd_shaper:shaper(), _) -> p1_queue:queue({message | presence, jid()}) | undefined.
room_queue_new(ServerHost, Shaper, QueueType) ->
    HaveRoomShaper = Shaper /= none,
    HaveMessageShaper = mod_muc_opt:user_message_shaper(ServerHost) /= none,
    HavePresenceShaper = mod_muc_opt:user_presence_shaper(ServerHost) /= none,
    HaveMinMessageInterval = mod_muc_opt:min_message_interval(ServerHost) /= 0,
    HaveMinPresenceInterval = mod_muc_opt:min_presence_interval(ServerHost) /= 0,
    if HaveRoomShaper or HaveMessageShaper or HavePresenceShaper
       or HaveMinMessageInterval or HaveMinPresenceInterval ->
	    p1_queue:new(QueueType);
       true ->
	    undefined
    end.

-spec get_user_activity(jid(), state()) -> #activity{}.
get_user_activity(JID, StateData) ->
    case treap:lookup(jid:tolower(JID),
		      StateData#state.activity)
	of
      {ok, _P, A} -> A;
      error ->
	  MessageShaper =
	      ejabberd_shaper:new(mod_muc_opt:user_message_shaper(StateData#state.server_host)),
	  PresenceShaper =
	      ejabberd_shaper:new(mod_muc_opt:user_presence_shaper(StateData#state.server_host)),
	  #activity{message_shaper = MessageShaper,
		    presence_shaper = PresenceShaper}
    end.

-spec store_user_activity(jid(), #activity{}, state()) -> state().
store_user_activity(JID, UserActivity, StateData) ->
    MinMessageInterval =
	trunc(mod_muc_opt:min_message_interval(StateData#state.server_host) * 1000),
    MinPresenceInterval =
	trunc(mod_muc_opt:min_presence_interval(StateData#state.server_host) * 1000),
    Key = jid:tolower(JID),
    Now = erlang:system_time(microsecond),
    Activity1 = clean_treap(StateData#state.activity,
			    {1, -Now}),
    Activity = case treap:lookup(Key, Activity1) of
		 {ok, _P, _A} -> treap:delete(Key, Activity1);
		 error -> Activity1
	       end,
    StateData1 = case MinMessageInterval == 0 andalso
			MinPresenceInterval == 0 andalso
			  UserActivity#activity.message_shaper == none andalso
			    UserActivity#activity.presence_shaper == none
			      andalso
			      UserActivity#activity.message == undefined andalso
				UserActivity#activity.presence == undefined
		     of
		   true -> StateData#state{activity = Activity};
		   false ->
		       case UserActivity#activity.message == undefined andalso
			      UserActivity#activity.presence == undefined
			   of
			 true ->
			     {_, MessageShaperInterval} =
				 ejabberd_shaper:update(UserActivity#activity.message_shaper,
					       100000),
			     {_, PresenceShaperInterval} =
				 ejabberd_shaper:update(UserActivity#activity.presence_shaper,
					       100000),
			     Delay = lists:max([MessageShaperInterval,
						PresenceShaperInterval,
						MinMessageInterval,
						MinPresenceInterval])
				       * 1000,
			     Priority = {1, -(Now + Delay)},
			     StateData#state{activity =
						 treap:insert(Key, Priority,
							      UserActivity,
							      Activity)};
			 false ->
			     Priority = {0, 0},
			     StateData#state{activity =
						 treap:insert(Key, Priority,
							      UserActivity,
							      Activity)}
		       end
		 end,
    reset_hibernate_timer(StateData1).

-spec clean_treap(treap:treap(), integer() | {1, integer()}) -> treap:treap().
clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
      true -> Treap;
      false ->
	  {_Key, Priority, _Value} = treap:get_root(Treap),
	  if Priority > CleanPriority ->
		 clean_treap(treap:delete_root(Treap), CleanPriority);
	     true -> Treap
	  end
    end.

-spec prepare_room_queue(state()) -> state().
prepare_room_queue(StateData) ->
    case p1_queue:out(StateData#state.room_queue) of
      {{value, {message, From}}, _RoomQueue} ->
	  Activity = get_user_activity(From, StateData),
	  Packet = Activity#activity.message,
	  Size = element_size(Packet),
	  {RoomShaper, RoomShaperInterval} =
	      ejabberd_shaper:update(StateData#state.room_shaper, Size),
	  erlang:send_after(RoomShaperInterval, self(),
			    process_room_queue),
	  StateData#state{room_shaper = RoomShaper};
      {{value, {presence, From}}, _RoomQueue} ->
	  Activity = get_user_activity(From, StateData),
	  {_Nick, Packet} = Activity#activity.presence,
	  Size = element_size(Packet),
	  {RoomShaper, RoomShaperInterval} =
	      ejabberd_shaper:update(StateData#state.room_shaper, Size),
	  erlang:send_after(RoomShaperInterval, self(),
			    process_room_queue),
	  StateData#state{room_shaper = RoomShaper};
      {empty, _} -> StateData
    end.

-spec update_online_user(jid(), #user{}, state()) -> state().
update_online_user(JID, #user{nick = Nick} = User, StateData) ->
    LJID = jid:tolower(JID),
    add_to_log(join, Nick, StateData),
    Nicks1 = try maps:get(LJID, StateData#state.users) of
		 #user{nick = OldNick} ->
		     case lists:delete(
			    LJID, maps:get(OldNick, StateData#state.nicks)) of
			 [] ->
			     maps:remove(OldNick, StateData#state.nicks);
			 LJIDs ->
			     maps:put(OldNick, LJIDs, StateData#state.nicks)
		     end
	     catch _:{badkey, _} ->
		     StateData#state.nicks
	     end,
    Nicks = maps:update_with(Nick,
			     fun (LJIDs) -> [LJID|LJIDs -- [LJID]] end,
			     [LJID], Nicks1),
    Users = maps:update_with(LJID,
			     fun(U) ->
				     U#user{nick = Nick}
			     end, User, StateData#state.users),
    NewStateData = StateData#state{users = Users, nicks = Nicks},
    case {maps:get(LJID, StateData#state.users, error),
	  maps:get(LJID, NewStateData#state.users, error)} of
	{#user{nick = Old}, #user{nick = New}} when Old /= New ->
	    send_nick_changing(JID, Old, NewStateData, true, true);
	_ ->
	    ok
    end,
    NewStateData.

-spec set_subscriber(jid(), binary(), [binary()], state()) -> state().
set_subscriber(JID, Nick, Nodes,
	       #state{room = Room, host = Host, server_host = ServerHost} = StateData) ->
    BareJID = jid:remove_resource(JID),
    LBareJID = jid:tolower(BareJID),
    MUCSubscribers =
        muc_subscribers_put(
          #subscriber{jid = BareJID,
                      nick = Nick,
                      nodes = Nodes},
          StateData#state.muc_subscribers),
    NewStateData = StateData#state{muc_subscribers = MUCSubscribers},
    store_room(NewStateData, [{add_subscription, BareJID, Nick, Nodes}]),
    case not muc_subscribers_is_key(LBareJID, StateData#state.muc_subscribers) of
	true ->
	    Packet1a = #message{
		sub_els = [#ps_event{
		    items = #ps_items{
			node = ?NS_MUCSUB_NODES_SUBSCRIBERS,
			items = [#ps_item{
			    id = p1_rand:get_string(),
			    sub_els = [#muc_subscribe{jid = BareJID, nick = Nick}]}]}}]},
	    Packet1b = #message{
		sub_els = [#ps_event{
		    items = #ps_items{
			node = ?NS_MUCSUB_NODES_SUBSCRIBERS,
			items = [#ps_item{
			    id = p1_rand:get_string(),
			    sub_els = [#muc_subscribe{nick = Nick}]}]}}]},
	    {Packet2a, Packet2b} = ejabberd_hooks:run_fold(muc_subscribed, ServerHost, {Packet1a, Packet1b},
							   [ServerHost, Room, Host, BareJID, StateData]),
	    send_subscriptions_change_notifications(Packet2a, Packet2b, NewStateData);
	_ ->
	    ok
    end,
    NewStateData.

-spec add_online_user(jid(), binary(), role(), state()) -> state().
add_online_user(JID, Nick, Role, StateData) ->
    tab_add_online_user(JID, StateData),
    User = #user{jid = JID, nick = Nick, role = Role},
    reset_hibernate_timer(update_online_user(JID, User, StateData)).

-spec remove_online_user(jid(), state()) -> state().
remove_online_user(JID, StateData) ->
    remove_online_user(JID, StateData, <<"">>).

-spec remove_online_user(jid(), state(), binary()) -> state().
remove_online_user(JID, StateData, Reason) ->
    LJID = jid:tolower(JID),
    #user{nick = Nick} = maps:get(LJID, StateData#state.users),
    add_to_log(leave, {Nick, Reason}, StateData),
    tab_remove_online_user(JID, StateData),
    Users = maps:remove(LJID, StateData#state.users),
    Nicks = try maps:get(Nick, StateData#state.nicks) of
		[LJID] ->
		    maps:remove(Nick, StateData#state.nicks);
		U ->
		    maps:put(Nick, U -- [LJID], StateData#state.nicks)
	    catch _:{badkey, _} ->
		    StateData#state.nicks
	    end,
    reset_hibernate_timer(StateData#state{users = Users, nicks = Nicks}).

-spec filter_presence(presence()) -> presence().
filter_presence(Presence) ->
    Els = lists:filter(
	    fun(El) ->
		    XMLNS = xmpp:get_ns(El),
		    case catch binary:part(XMLNS, 0, size(?NS_MUC)) of
			?NS_MUC -> false;
			_ -> XMLNS /= ?NS_HATS
		    end
	    end, xmpp:get_els(Presence)),
    xmpp:set_els(Presence, Els).

-spec strip_status(presence()) -> presence().
strip_status(Presence) ->
    Presence#presence{status = []}.

-spec add_user_presence(jid(), presence(), state()) -> state().
add_user_presence(JID, Presence, StateData) ->
    LJID = jid:tolower(JID),
    FPresence = filter_presence(Presence),
    Users = maps:update_with(LJID,
			     fun (#user{} = User) ->
				     User#user{last_presence = FPresence}
			     end, StateData#state.users),
    StateData#state{users = Users}.

-spec add_user_presence_un(jid(), presence(), state()) -> state().
add_user_presence_un(JID, Presence, StateData) ->
    LJID = jid:tolower(JID),
    FPresence = filter_presence(Presence),
    Users = maps:update_with(LJID,
			     fun (#user{} = User) ->
				     User#user{last_presence = FPresence,
					       role = none}
			     end, StateData#state.users),
    StateData#state{users = Users}.

%% Find and return a list of the full JIDs of the users of Nick.
%% Return jid record.
-spec find_jids_by_nick(binary(), state()) -> [jid()].
find_jids_by_nick(Nick, StateData) ->
    Users = case maps:get(Nick, StateData#state.nicks, []) of
                [] -> muc_subscribers_get_by_nick(
                        Nick, StateData#state.muc_subscribers);
		Us -> Us
	    end,
    [jid:make(LJID) || LJID <- Users].

%% Find and return the full JID of the user of Nick with
%% highest-priority presence.  Return jid record.
-spec find_jid_by_nick(binary(), state()) -> jid() | false.
find_jid_by_nick(Nick, StateData) ->
    try maps:get(Nick, StateData#state.nicks) of
	[User] -> jid:make(User);
	[FirstUser | Users] ->
	    #user{last_presence = FirstPresence} =
		maps:get(FirstUser, StateData#state.users),
	    {LJID, _} = lists:foldl(
			  fun(Compare, {HighestUser, HighestPresence}) ->
				  #user{last_presence = P1} =
				      maps:get(Compare, StateData#state.users),
				  case higher_presence(P1, HighestPresence) of
				      true -> {Compare, P1};
				      false -> {HighestUser, HighestPresence}
				  end
			  end, {FirstUser, FirstPresence}, Users),
	    jid:make(LJID)
    catch _:{badkey, _} ->
	    false
    end.

-spec higher_presence(undefined | presence(),
		      undefined | presence()) -> boolean().
higher_presence(Pres1, Pres2) when Pres1 /= undefined, Pres2 /= undefined ->
    Pri1 = get_priority_from_presence(Pres1),
    Pri2 = get_priority_from_presence(Pres2),
    Pri1 > Pri2;
higher_presence(Pres1, Pres2) ->
    Pres1 > Pres2.

-spec get_priority_from_presence(presence()) -> integer().
get_priority_from_presence(#presence{priority = Prio}) ->
    case Prio of
        undefined -> 0;
        _ -> Prio
    end.

-spec find_nick_by_jid(jid(), state()) -> binary().
find_nick_by_jid(JID, StateData) ->
    LJID = jid:tolower(JID),
    #user{nick = Nick} = maps:get(LJID, StateData#state.users),
    Nick.

-spec is_nick_change(jid(), binary(), state()) -> boolean().
is_nick_change(JID, Nick, StateData) ->
    LJID = jid:tolower(JID),
    case Nick of
      <<"">> -> false;
      _ ->
	  #user{nick = OldNick} = maps:get(LJID, StateData#state.users),
	  Nick /= OldNick
    end.

-spec nick_collision(jid(), binary(), state()) -> boolean().
nick_collision(User, Nick, StateData) ->
    UserOfNick = case find_jid_by_nick(Nick, StateData) of
		     false ->
                         case muc_subscribers_get_by_nick(Nick, StateData#state.muc_subscribers) of
                             [J] -> J;
                             [] -> false
                         end;
		     J -> J
		 end,
    (UserOfNick /= false andalso
      jid:remove_resource(jid:tolower(UserOfNick))
	/= jid:remove_resource(jid:tolower(User))).

-spec add_new_user(jid(), binary(), presence(), state()) -> state();
		  (jid(), binary(), iq(), state()) -> {error, stanza_error()} |
						      {ignore, state()} |
						      {result, muc_subscribe(), state()}.
add_new_user(From, Nick, Packet, StateData) ->
    Lang = xmpp:get_lang(Packet),
    MaxUsers = get_max_users(StateData),
    MaxAdminUsers = MaxUsers +
		      get_max_users_admin_threshold(StateData),
    NUsers = maps:size(StateData#state.users),
    Affiliation = get_affiliation(From, StateData),
    ServiceAffiliation = get_service_affiliation(From,
						 StateData),
    NConferences = tab_count_user(From, StateData),
    MaxConferences =
	mod_muc_opt:max_user_conferences(StateData#state.server_host),
    Collision = nick_collision(From, Nick, StateData),
    IsSubscribeRequest = not is_record(Packet, presence),
    case {(ServiceAffiliation == owner orelse
	     ((Affiliation == admin orelse Affiliation == owner)
	       andalso NUsers < MaxAdminUsers)
	       orelse NUsers < MaxUsers)
	    andalso NConferences < MaxConferences,
	  Collision,
	  mod_muc:can_use_nick(StateData#state.server_host,
			       StateData#state.host, From, Nick),
	  get_default_role(Affiliation, StateData)}
	of
      {false, _, _, _} when NUsers >= MaxUsers orelse NUsers >= MaxAdminUsers ->
	  Txt = ?T("Too many users in this conference"),
	  Err = xmpp:err_resource_constraint(Txt, Lang),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route_error(Packet, Err),
		  StateData;
	     true ->
		  {error, Err}
	  end;
      {false, _, _, _} when NConferences >= MaxConferences ->
	  Txt = ?T("You have joined too many conferences"),
	  Err = xmpp:err_resource_constraint(Txt, Lang),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route_error(Packet, Err),
		  StateData;
	     true ->
		  {error, Err}
	  end;
      {false, _, _, _} ->
	  Err = xmpp:err_service_unavailable(),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route_error(Packet, Err),
		  StateData;
	     true ->
		  {error, Err}
	  end;
      {_, _, _, none} ->
	  Err = case Affiliation of
		    outcast ->
			ErrText = ?T("You have been banned from this room"),
			xmpp:err_forbidden(ErrText, Lang);
		    _ ->
			ErrText = ?T("Membership is required to enter this room"),
			xmpp:err_registration_required(ErrText, Lang)
		end,
	  if not IsSubscribeRequest ->
		  ejabberd_router:route_error(Packet, Err),
		  StateData;
	     true ->
		  {error, Err}
	  end;
      {_, true, _, _} ->
	  ErrText = ?T("That nickname is already in use by another occupant"),
	  Err = xmpp:err_conflict(ErrText, Lang),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route_error(Packet, Err),
		  StateData;
	     true ->
		  {error, Err}
	  end;
      {_, _, false, _} ->
	  Err = case Nick of
			<<>> ->
			    xmpp:err_jid_malformed(?T("Nickname can't be empty"),
						   Lang);
			_ ->
			    xmpp:err_conflict(?T("That nickname is registered"
						 " by another person"), Lang)
		    end,
	  if not IsSubscribeRequest ->
		  ejabberd_router:route_error(Packet, Err),
		  StateData;
	     true ->
		  {error, Err}
	  end;
      {_, _, _, Role} ->
	  case check_password(ServiceAffiliation, Affiliation,
			      Packet, From, StateData)
	      of
	    true ->
		Nodes = get_subscription_nodes(Packet),
		NewStateData =
		      if not IsSubscribeRequest ->
			      NewState = add_user_presence(
					   From, Packet,
					   add_online_user(From, Nick, Role,
							   StateData)),
			      send_initial_presences_and_messages(
				From, Nick, Packet, NewState, StateData),
			      NewState;
			 true ->
			      set_subscriber(From, Nick, Nodes, StateData)
		      end,
		  ResultState =
		      case NewStateData#state.just_created of
			  true ->
			      NewStateData#state{just_created = erlang:system_time(microsecond)};
			  _ ->
			      Robots = maps:remove(From, StateData#state.robots),
			      NewStateData#state{robots = Robots}
		      end,
		  if not IsSubscribeRequest -> ResultState;
		     true -> {result, subscribe_result(Packet), ResultState}
		  end;
	    need_password ->
		ErrText = ?T("A password is required to enter this room"),
		Err = xmpp:err_not_authorized(ErrText, Lang),
		if not IsSubscribeRequest ->
			ejabberd_router:route_error(Packet, Err),
			StateData;
		   true ->
			{error, Err}
		end;
	    captcha_required ->
		SID = xmpp:get_id(Packet),
		RoomJID = StateData#state.jid,
		To = jid:replace_resource(RoomJID, Nick),
		Limiter = {From#jid.luser, From#jid.lserver},
		case ejabberd_captcha:create_captcha(SID, RoomJID, To,
						     Lang, Limiter, From)
                   of
		  {ok, ID, Body, CaptchaEls} ->
		      MsgPkt = #message{from = RoomJID,
					to = From,
					id = ID, body = Body,
					sub_els = CaptchaEls},
		      Robots = maps:put(From, {Nick, Packet},
					StateData#state.robots),
		      ejabberd_router:route(MsgPkt),
		      NewState = StateData#state{robots = Robots},
		      if not IsSubscribeRequest ->
			      NewState;
			 true ->
			      {ignore, NewState}
		      end;
		  {error, limit} ->
		      ErrText = ?T("Too many CAPTCHA requests"),
		      Err = xmpp:err_resource_constraint(ErrText, Lang),
		      if not IsSubscribeRequest ->
			      ejabberd_router:route_error(Packet, Err),
			      StateData;
			 true ->
			      {error, Err}
		      end;
		  _ ->
		      ErrText = ?T("Unable to generate a CAPTCHA"),
		      Err = xmpp:err_internal_server_error(ErrText, Lang),
		      if not IsSubscribeRequest ->
			      ejabberd_router:route_error(Packet, Err),
			      StateData;
			 true ->
			      {error, Err}
		      end
		end;
	    _ ->
		ErrText = ?T("Incorrect password"),
		Err = xmpp:err_not_authorized(ErrText, Lang),
		if not IsSubscribeRequest ->
			ejabberd_router:route_error(Packet, Err),
			StateData;
		   true ->
			{error, Err}
		end
	  end
    end.

-spec check_password(affiliation(), affiliation(),
		     presence() | iq(), jid(), state()) ->
      boolean() | need_password | captcha_required.
check_password(owner, _Affiliation, _Packet, _From,
	       _StateData) ->
    %% Don't check pass if user is owner in MUC service (access_admin option)
    true;
check_password(_ServiceAffiliation, Affiliation, Packet,
	       From, StateData) ->
    case (StateData#state.config)#config.password_protected
	of
      false -> check_captcha(Affiliation, From, StateData);
      true ->
	  Pass = extract_password(Packet),
	  case Pass of
	    false -> need_password;
	    _ ->
		case (StateData#state.config)#config.password of
		  Pass -> true;
		  _ -> false
		end
	  end
    end.

-spec check_captcha(affiliation(), jid(), state()) -> true | captcha_required.
check_captcha(Affiliation, From, StateData) ->
    case (StateData#state.config)#config.captcha_protected
	   andalso ejabberd_captcha:is_feature_available()
	of
      true when Affiliation == none ->
	  case maps:get(From, StateData#state.robots, error) of
	      passed -> true;
	      _ ->
		WList =
		    (StateData#state.config)#config.captcha_whitelist,
		#jid{luser = U, lserver = S, lresource = R} = From,
		case (?SETS):is_element({U, S, R}, WList) of
		  true -> true;
		  false ->
		      case (?SETS):is_element({U, S, <<"">>}, WList) of
			true -> true;
			false ->
			    case (?SETS):is_element({<<"">>, S, <<"">>}, WList)
				of
			      true -> true;
			      false -> captcha_required
			    end
		      end
		end
	  end;
      _ -> true
    end.

-spec extract_password(presence() | iq()) -> binary() | false.
extract_password(#presence{} = Pres) ->
    case xmpp:get_subtag(Pres, #muc{}) of
	#muc{password = Password} when is_binary(Password) ->
	    Password;
	_ ->
	    false
    end;
extract_password(#iq{} = IQ) ->
    case xmpp:get_subtag(IQ, #muc_subscribe{}) of
	#muc_subscribe{password = Password} when Password /= <<"">> ->
	    Password;
	_ ->
	    false
    end.

-spec get_history(binary(), stanza(), state()) -> [lqueue_elem()].
get_history(Nick, Packet, #state{history = History}) ->
    case xmpp:get_subtag(Packet, #muc{}) of
	#muc{history = #muc_history{} = MUCHistory} ->
	    Now = erlang:timestamp(),
	    Q = History#lqueue.queue,
	    filter_history(Q, Now, Nick, MUCHistory);
	_ ->
	    p1_queue:to_list(History#lqueue.queue)
    end.

-spec filter_history(p1_queue:queue(lqueue_elem()), erlang:timestamp(),
		     binary(), muc_history()) -> [lqueue_elem()].
filter_history(Queue, Now, Nick,
	       #muc_history{since = Since,
			    seconds = Seconds,
			    maxstanzas = MaxStanzas,
			    maxchars = MaxChars}) ->
    {History, _, _} =
	lists:foldr(
	  fun({_, _, _, TimeStamp, Size} = Elem,
	      {Elems, NumStanzas, NumChars} = Acc) ->
		  NowDiff = timer:now_diff(Now, TimeStamp) div 1000000,
		  Chars = Size + byte_size(Nick) + 1,
		  if (NumStanzas < MaxStanzas) andalso
		     (TimeStamp > Since) andalso
		     (NowDiff =< Seconds) andalso
		     (NumChars + Chars =< MaxChars) ->
			  {[Elem|Elems], NumStanzas + 1, NumChars + Chars};
		     true ->
			  Acc
		  end
	  end, {[], 0, 0}, p1_queue:to_list(Queue)),
    History.

-spec is_room_overcrowded(state()) -> boolean().
is_room_overcrowded(StateData) ->
    MaxUsersPresence = mod_muc_opt:max_users_presence(StateData#state.server_host),
    maps:size(StateData#state.users) > MaxUsersPresence.

-spec presence_broadcast_allowed(jid(), state()) -> boolean().
presence_broadcast_allowed(JID, StateData) ->
    Role = get_role(JID, StateData),
    lists:member(Role, (StateData#state.config)#config.presence_broadcast).

-spec send_initial_presences_and_messages(
	jid(), binary(), presence(), state(), state()) -> ok.
send_initial_presences_and_messages(From, Nick, Presence, NewState, OldState) ->
    advertise_entity_capabilities(From, NewState),
    send_existing_presences(From, NewState),
    send_self_presence(From, NewState, OldState),
    History = get_history(Nick, Presence, NewState),
    send_history(From, History, NewState),
    send_subject(From, OldState).

-spec advertise_entity_capabilities(jid(), state()) -> ok.
advertise_entity_capabilities(JID, State) ->
    AvatarHash = (State#state.config)#config.vcard_xupdate,
    DiscoInfo = make_disco_info(JID, State),
    Extras = iq_disco_info_extras(<<"en">>, State, true),
    DiscoInfo1 = DiscoInfo#disco_info{xdata = [Extras]},
    DiscoHash = mod_caps:compute_disco_hash(DiscoInfo1, sha),
    Els1 = [#caps{hash = <<"sha-1">>,
		  node = ejabberd_config:get_uri(),
		  version = DiscoHash}],
    Els2 = if is_binary(AvatarHash) ->
		   [#vcard_xupdate{hash = AvatarHash}|Els1];
	      true ->
		   Els1
	   end,
    ejabberd_router:route(#presence{from = State#state.jid, to = JID,
				    id = p1_rand:get_string(),
				    sub_els = Els2}).

-spec send_self_presence(jid(), state(), state()) -> ok.
send_self_presence(NJID, StateData, OldStateData) ->
    send_new_presence(NJID, <<"">>, true, StateData, OldStateData).

-spec send_update_presence(jid(), state(), state()) -> ok.
send_update_presence(JID, StateData, OldStateData) ->
    send_update_presence(JID, <<"">>, StateData, OldStateData).

-spec send_update_presence(jid(), binary(), state(), state()) -> ok.
send_update_presence(JID, Reason, StateData, OldStateData) ->
    case is_room_overcrowded(StateData) of
	true -> ok;
	false -> send_update_presence1(JID, Reason, StateData, OldStateData)
    end.

-spec send_update_presence1(jid(), binary(), state(), state()) -> ok.
send_update_presence1(JID, Reason, StateData, OldStateData) ->
    LJID = jid:tolower(JID),
    LJIDs = case LJID of
	      {U, S, <<"">>} ->
		    maps:fold(fun (J, _, Js) ->
				      case J of
					  {U, S, _} -> [J | Js];
					  _ -> Js
				      end
			      end, [], StateData#state.users);
	      _ ->
		  case maps:is_key(LJID, StateData#state.users) of
		    true -> [LJID];
		    _ -> []
		  end
	    end,
    lists:foreach(fun (J) ->
			  send_new_presence(J, Reason, false, StateData,
					    OldStateData)
		  end,
		  LJIDs).

-spec send_new_presence(jid(), state(), state()) -> ok.
send_new_presence(NJID, StateData, OldStateData) ->
    send_new_presence(NJID, <<"">>, false, StateData, OldStateData).

-spec send_new_presence(jid(), binary(), state(), state()) -> ok.
send_new_presence(NJID, Reason, StateData, OldStateData) ->
    send_new_presence(NJID, Reason, false, StateData, OldStateData).

-spec is_ra_changed(jid(), boolean(), state(), state()) -> boolean().
is_ra_changed(_, _IsInitialPresence = true, _, _) ->
    false;
is_ra_changed(JID, _IsInitialPresence = false, NewStateData, OldStateData) ->
    NewRole = get_role(JID, NewStateData),
    NewAff = get_affiliation(JID, NewStateData),
    OldRole = get_role(JID, OldStateData),
    OldAff = get_affiliation(JID, OldStateData),
    if (NewRole == none) and (NewAff == OldAff) ->
	    %% A user is leaving the room;
	    false;
       true ->
	    (NewRole /= OldRole) or (NewAff /= OldAff)
    end.

-spec send_new_presence(jid(), binary(), boolean(), state(), state()) -> ok.
send_new_presence(NJID, Reason, IsInitialPresence, StateData, OldStateData) ->
    LNJID = jid:tolower(NJID),
    #user{nick = Nick} = maps:get(LNJID, StateData#state.users),
    LJID = find_jid_by_nick(Nick, StateData),
    #user{jid = RealJID, role = Role0,
	  last_presence = Presence0} = UserInfo =
	maps:get(jid:tolower(LJID), StateData#state.users),
    {Role1, Presence1} =
        case (presence_broadcast_allowed(NJID, StateData) orelse
         presence_broadcast_allowed(NJID, OldStateData)) of
            true -> {Role0, Presence0};
            false -> {none, #presence{type = unavailable}}
        end,
    Affiliation = get_affiliation(LJID, StateData),
    Node1 = case is_ra_changed(NJID, IsInitialPresence, StateData, OldStateData) of
                true -> ?NS_MUCSUB_NODES_AFFILIATIONS;
                false -> ?NS_MUCSUB_NODES_PRESENCE
            end,
    Node2 = ?NS_MUCSUB_NODES_PARTICIPANTS,
    UserMap =
        case is_room_overcrowded(StateData) orelse
	     (not (presence_broadcast_allowed(NJID, StateData) orelse
		   presence_broadcast_allowed(NJID, OldStateData))) of
            true ->
                #{LNJID => UserInfo};
            false ->
                %% TODO: optimize further
                UM1 = get_users_and_subscribers_with_node(Node1, StateData),
                UM2 = get_users_and_subscribers_with_node(Node2, StateData),
                maps:merge(UM1, UM2)
        end,
    maps:fold(
      fun(LUJID, Info, _) ->
	      IsSelfPresence = LNJID == LUJID,
	      {Role, Presence} = if IsSelfPresence -> {Role0, Presence0};
				    true -> {Role1, Presence1}
				 end,
	      Item0 = #muc_item{affiliation = Affiliation,
				role = Role},
	      Item1 = case Info#user.role == moderator orelse
			  (StateData#state.config)#config.anonymous
			  == false orelse IsSelfPresence of
			  true -> Item0#muc_item{jid = RealJID};
			  false -> Item0
		      end,
	      Item = Item1#muc_item{reason = Reason},
	      StatusCodes = status_codes(IsInitialPresence, IsSelfPresence,
					 StateData),
	      Pres = if Presence == undefined -> #presence{};
			true -> Presence
		     end,
              Packet = xmpp:set_subtag(
                         add_presence_hats(NJID, Pres, StateData),
                         #muc_user{items = [Item],
                                   status_codes = StatusCodes}),
	      send_wrapped(jid:replace_resource(StateData#state.jid, Nick),
			   Info#user.jid, Packet, Node1, StateData),
	      Type = xmpp:get_type(Packet),
	      IsSubscriber = is_subscriber(Info#user.jid, StateData),
	      IsOccupant = Info#user.last_presence /= undefined,
	      if (IsSubscriber and not IsOccupant) and
		 (IsInitialPresence or (Type == unavailable)) ->
		      send_wrapped(jid:replace_resource(StateData#state.jid, Nick),
				   Info#user.jid, Packet, Node2, StateData);
		 true ->
		      ok
	      end
      end, ok, UserMap).

-spec send_existing_presences(jid(), state()) -> ok.
send_existing_presences(ToJID, StateData) ->
    case is_room_overcrowded(StateData) of
	true -> ok;
	false -> send_existing_presences1(ToJID, StateData)
    end.

-spec send_existing_presences1(jid(), state()) -> ok.
send_existing_presences1(ToJID, StateData) ->
    LToJID = jid:tolower(ToJID),
    #user{jid = RealToJID, role = Role} = maps:get(LToJID, StateData#state.users),
    maps:fold(
      fun(FromNick, _Users, _) ->
	      LJID = find_jid_by_nick(FromNick, StateData),
	      #user{jid = FromJID, role = FromRole,
		    last_presence = Presence} =
		  maps:get(jid:tolower(LJID), StateData#state.users),
	      PresenceBroadcast =
		  lists:member(
		    FromRole, (StateData#state.config)#config.presence_broadcast),
	      case {RealToJID, PresenceBroadcast} of
		  {FromJID, _} -> ok;
		  {_, false} -> ok;
		  _ ->
		      FromAffiliation = get_affiliation(LJID, StateData),
		      Item0 = #muc_item{affiliation = FromAffiliation,
					role = FromRole},
		      Item = case Role == moderator orelse
				 (StateData#state.config)#config.anonymous
				 == false of
				 true -> Item0#muc_item{jid = FromJID};
				 false -> Item0
			     end,
		      Packet = xmpp:set_subtag(
                                 add_presence_hats(
                                   FromJID, Presence, StateData),
                                 #muc_user{items = [Item]}),
		      send_wrapped(jid:replace_resource(StateData#state.jid, FromNick),
				   RealToJID, Packet, ?NS_MUCSUB_NODES_PRESENCE, StateData)
	      end
      end, ok, StateData#state.nicks).

-spec set_nick(jid(), binary(), state()) -> state().
set_nick(JID, Nick, State) ->
    LJID = jid:tolower(JID),
    #user{nick = OldNick} = maps:get(LJID, State#state.users),
    Users = maps:update_with(LJID,
			     fun (#user{} = User) -> User#user{nick = Nick} end,
			     State#state.users),
    OldNickUsers = maps:get(OldNick, State#state.nicks),
    NewNickUsers = maps:get(Nick, State#state.nicks, []),
    Nicks = case OldNickUsers of
		[LJID] ->
		    maps:put(Nick, [LJID | NewNickUsers -- [LJID]],
			     maps:remove(OldNick, State#state.nicks));
		[_ | _] ->
		    maps:put(Nick, [LJID | NewNickUsers -- [LJID]],
			     maps:put(OldNick, OldNickUsers -- [LJID],
				      State#state.nicks))
	    end,
    State#state{users = Users, nicks = Nicks}.

-spec change_nick(jid(), binary(), state()) -> state().
change_nick(JID, Nick, StateData) ->
    LJID = jid:tolower(JID),
    #user{nick = OldNick} = maps:get(LJID, StateData#state.users),
    OldNickUsers = maps:get(OldNick, StateData#state.nicks),
    NewNickUsers = maps:get(Nick, StateData#state.nicks, []),
    SendOldUnavailable = length(OldNickUsers) == 1,
    SendNewAvailable = SendOldUnavailable orelse NewNickUsers == [],
    NewStateData = set_nick(JID, Nick, StateData),
    case presence_broadcast_allowed(JID, NewStateData) of
        true ->
            send_nick_changing(JID, OldNick, NewStateData,
                               SendOldUnavailable, SendNewAvailable);
        false -> ok
    end,
    add_to_log(nickchange, {OldNick, Nick}, StateData),
    NewStateData.

-spec send_nick_changing(jid(), binary(), state(), boolean(), boolean()) -> ok.
send_nick_changing(JID, OldNick, StateData,
		   SendOldUnavailable, SendNewAvailable) ->
    #user{jid = RealJID, nick = Nick, role = Role,
	  last_presence = Presence} =
	maps:get(jid:tolower(JID), StateData#state.users),
    Affiliation = get_affiliation(JID, StateData),
    maps:fold(
      fun(LJID, Info, _) when Presence /= undefined ->
	      IsSelfPresence = LJID == jid:tolower(JID),
	      Item0 = #muc_item{affiliation = Affiliation, role = Role},
	      Item = case Info#user.role == moderator orelse
			 (StateData#state.config)#config.anonymous
			 == false orelse IsSelfPresence of
			 true -> Item0#muc_item{jid = RealJID};
			 false -> Item0
		     end,
	      Status110 = case IsSelfPresence of
			      true -> [110];
			      false -> []
			  end,
	      Packet1 = #presence{
			   type = unavailable,
			   sub_els = [#muc_user{
					 items = [Item#muc_item{nick = Nick}],
					 status_codes = [303|Status110]}]},
	      Packet2 = xmpp:set_subtag(Presence,
					#muc_user{items = [Item],
						  status_codes = Status110}),
	      if SendOldUnavailable ->
		      send_wrapped(
			jid:replace_resource(StateData#state.jid, OldNick),
			Info#user.jid, Packet1, ?NS_MUCSUB_NODES_PRESENCE,
			StateData);
		 true -> ok
	      end,
	      if SendNewAvailable ->
		      send_wrapped(
			jid:replace_resource(StateData#state.jid, Nick),
			Info#user.jid, Packet2, ?NS_MUCSUB_NODES_PRESENCE,
			StateData);
		 true -> ok
	      end;
	 (_, _, _) ->
	      ok
      end, ok, get_users_and_subscribers_with_node(
                 ?NS_MUCSUB_NODES_PRESENCE, StateData)).

-spec maybe_send_affiliation(jid(), affiliation(), state()) -> ok.
maybe_send_affiliation(JID, Affiliation, StateData) ->
    LJID = jid:tolower(JID),
    %% TODO: there should be a better way to check IsOccupant
    Users = get_users_and_subscribers(StateData),
    IsOccupant = case LJID of
		     {LUser, LServer, <<"">>} ->
			 #{} /= maps:filter(
				  fun({U, S, _}, _) ->
					  U == LUser andalso
					      S == LServer
				  end, Users);
		     {_LUser, _LServer, _LResource} ->
			 maps:is_key(LJID, Users)
		 end,
    case IsOccupant of
      true ->
	  ok; % The new affiliation is published via presence.
      false ->
	  send_affiliation(JID, Affiliation, StateData)
    end.

-spec send_affiliation(jid(), affiliation(), state()) -> ok.
send_affiliation(JID, Affiliation, StateData) ->
    Item = #muc_item{jid = JID,
		     affiliation = Affiliation,
		     role = none},
    Message = #message{id = p1_rand:get_string(),
		       sub_els = [#muc_user{items = [Item]}]},
    Users = get_users_and_subscribers_with_node(
              ?NS_MUCSUB_NODES_AFFILIATIONS, StateData),
    Recipients = case (StateData#state.config)#config.anonymous of
		   true ->
		       maps:filter(fun(_, #user{role = moderator}) ->
					   true;
				      (_, _) ->
					   false
				   end, Users);
		   false ->
		       Users
		 end,
    send_wrapped_multiple(StateData#state.jid, Recipients, Message,
			  ?NS_MUCSUB_NODES_AFFILIATIONS, StateData).

-spec status_codes(boolean(), boolean(), state()) -> [pos_integer()].
status_codes(IsInitialPresence, _IsSelfPresence = true, StateData) ->
    S0 = [110],
    case IsInitialPresence of
	true ->
	    S1 = case StateData#state.just_created of
		     true -> [201|S0];
		     _ -> S0
		 end,
	    S2 = case (StateData#state.config)#config.anonymous of
		     true -> S1;
		     false -> [100|S1]
		 end,
	    S3 = case (StateData#state.config)#config.logging of
		     true -> [170|S2];
		     false -> S2
		 end,
	    S3;
	false -> S0
    end;
status_codes(_IsInitialPresence, _IsSelfPresence = false, _StateData) -> [].

-spec lqueue_new(non_neg_integer(), ram | file) -> lqueue().
lqueue_new(Max, Type) ->
    #lqueue{queue = p1_queue:new(Type), max = Max}.

-spec lqueue_in(lqueue_elem(), lqueue()) -> lqueue().
%% If the message queue limit is set to 0, do not store messages.
lqueue_in(_Item, LQ = #lqueue{max = 0}) -> LQ;
%% Otherwise, rotate messages in the queue store.
lqueue_in(Item, #lqueue{queue = Q1, max = Max}) ->
    Len = p1_queue:len(Q1),
    Q2 = p1_queue:in(Item, Q1),
    if Len >= Max ->
	   Q3 = lqueue_cut(Q2, Len - Max + 1),
	   #lqueue{queue = Q3, max = Max};
       true -> #lqueue{queue = Q2, max = Max}
    end.

-spec lqueue_cut(p1_queue:queue(lqueue_elem()), non_neg_integer()) -> p1_queue:queue(lqueue_elem()).
lqueue_cut(Q, 0) -> Q;
lqueue_cut(Q, N) ->
    {_, Q1} = p1_queue:out(Q),
    lqueue_cut(Q1, N - 1).

-spec add_message_to_history(binary(), jid(), message(), state()) -> state().
add_message_to_history(FromNick, FromJID, Packet, StateData) ->
    add_to_log(text, {FromNick, Packet}, StateData),
    case check_subject(Packet) of
	[] ->
	    TimeStamp = erlang:timestamp(),
	    AddrPacket = case (StateData#state.config)#config.anonymous of
			     true -> Packet;
			     false ->
				 Addresses = #addresses{
						list = [#address{type = ofrom,
								 jid = FromJID}]},
				 xmpp:set_subtag(Packet, Addresses)
			 end,
	    TSPacket = misc:add_delay_info(
			 AddrPacket, StateData#state.jid, TimeStamp),
	    SPacket = xmpp:set_from_to(
			TSPacket,
			jid:replace_resource(StateData#state.jid, FromNick),
			StateData#state.jid),
	    Size = element_size(SPacket),
	    Q1 = lqueue_in({FromNick, TSPacket, false,
			    TimeStamp, Size},
			   StateData#state.history),
	    StateData#state{history = Q1, just_created = erlang:system_time(microsecond)};
	_ ->
	    StateData#state{just_created = erlang:system_time(microsecond)}
    end.

-spec send_history(jid(), [lqueue_elem()], state()) -> ok.
send_history(JID, History, StateData) ->
    lists:foreach(
      fun({Nick, Packet, _HaveSubject, _TimeStamp, _Size}) ->
	      ejabberd_router:route(
		xmpp:set_from_to(
		  Packet,
		  jid:replace_resource(StateData#state.jid, Nick),
		  JID))
      end, History).

-spec send_subject(jid(), state()) -> ok.
send_subject(JID, #state{subject_author = Nick} = StateData) ->
    Subject = case StateData#state.subject of
		  [] -> [#text{}];
		  [_|_] = S -> S
	      end,
    Packet = #message{from = jid:replace_resource(StateData#state.jid, Nick),
		      to = JID, type = groupchat, subject = Subject},
    ejabberd_router:route(Packet).

-spec check_subject(message()) -> [text()].
check_subject(#message{subject = [_|_] = Subj, body = [],
		       thread = undefined}) ->
    Subj;
check_subject(_) ->
    [].

-spec can_change_subject(role(), boolean(), state()) -> boolean().
can_change_subject(Role, IsSubscriber, StateData) ->
    case (StateData#state.config)#config.allow_change_subj
	of
      true -> Role == moderator orelse Role == participant orelse IsSubscriber == true;
      _ -> Role == moderator
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Admin stuff

-spec process_iq_admin(jid(), iq(), #state{}) -> {error, stanza_error()} |
						 {result, undefined, #state{}} |
						 {result, muc_admin()}.
process_iq_admin(_From, #iq{lang = Lang, sub_els = [#muc_admin{items = []}]},
		 _StateData) ->
    Txt = ?T("No 'item' element found"),
    {error, xmpp:err_bad_request(Txt, Lang)};
process_iq_admin(_From, #iq{type = get, lang = Lang,
			    sub_els = [#muc_admin{items = [_, _|_]}]},
		 _StateData) ->
    ErrText = ?T("Too many <item/> elements"),
    {error, xmpp:err_bad_request(ErrText, Lang)};
process_iq_admin(From, #iq{type = set, lang = Lang,
			   sub_els = [#muc_admin{items = Items}]},
		 StateData) ->
    process_admin_items_set(From, Items, Lang, StateData);
process_iq_admin(From, #iq{type = get, lang = Lang,
			   sub_els = [#muc_admin{items = [Item]}]},
		 StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    FRole = get_role(From, StateData),
    case Item of
	#muc_item{role = undefined, affiliation = undefined} ->
	    Txt = ?T("Neither 'role' nor 'affiliation' attribute found"),
	    {error, xmpp:err_bad_request(Txt, Lang)};
	#muc_item{role = undefined, affiliation = Affiliation} ->
	    if (FAffiliation == owner) or
	       (FAffiliation == admin) or
	       ((FAffiliation == member) and
		not (StateData#state.config)#config.anonymous) ->
		    Items = items_with_affiliation(Affiliation, StateData),
		    {result, #muc_admin{items = Items}};
	       true ->
		    ErrText = ?T("Administrator privileges required"),
		    {error, xmpp:err_forbidden(ErrText, Lang)}
	    end;
	#muc_item{role = Role} ->
	    if FRole == moderator ->
		    Items = items_with_role(Role, StateData),
		    {result, #muc_admin{items = Items}};
	       true ->
		    ErrText = ?T("Moderator privileges required"),
		    {error, xmpp:err_forbidden(ErrText, Lang)}
	    end
    end.

-spec items_with_role(role(), state()) -> [muc_item()].
items_with_role(SRole, StateData) ->
    lists:map(fun ({_, U}) -> user_to_item(U, StateData)
	      end,
	      search_role(SRole, StateData)).

-spec items_with_affiliation(affiliation(), state()) -> [muc_item()].
items_with_affiliation(SAffiliation, StateData) ->
    lists:map(
      fun({JID, {Affiliation, Reason}}) ->
	      #muc_item{affiliation = Affiliation, jid = jid:make(JID),
			reason = Reason};
	 ({JID, Affiliation}) ->
	      #muc_item{affiliation = Affiliation, jid = jid:make(JID)}
      end,
      search_affiliation(SAffiliation, StateData)).

-spec user_to_item(#user{}, state()) -> muc_item().
user_to_item(#user{role = Role, nick = Nick, jid = JID},
	     StateData) ->
    Affiliation = get_affiliation(JID, StateData),
    #muc_item{role = Role,
	      affiliation = Affiliation,
	      nick = Nick,
	      jid = JID}.

-spec search_role(role(), state()) -> [{ljid(), #user{}}].
search_role(Role, StateData) ->
    lists:filter(fun ({_, #user{role = R}}) -> Role == R
		 end,
		 maps:to_list(StateData#state.users)).

-spec search_affiliation(affiliation(), state()) ->
			 [{ljid(),
			   affiliation() | {affiliation(), binary()}}].
search_affiliation(Affiliation,
                   #state{config = #config{persistent = false}} = StateData) ->
    search_affiliation_fallback(Affiliation, StateData);
search_affiliation(Affiliation, StateData) ->
    Room = StateData#state.room,
    Host = StateData#state.host,
    ServerHost = StateData#state.server_host,
    Mod = gen_mod:db_mod(ServerHost, mod_muc),
    case Mod:search_affiliation(ServerHost, Room, Host, Affiliation) of
	{ok, AffiliationList} ->
	    AffiliationList;
	{error, _} ->
	    search_affiliation_fallback(Affiliation, StateData)
    end.

-spec search_affiliation_fallback(affiliation(), state()) ->
				  [{ljid(),
				    affiliation() | {affiliation(), binary()}}].
search_affiliation_fallback(Affiliation, StateData) ->
    lists:filter(
      fun({_, A}) ->
	      case A of
		  {A1, _Reason} -> Affiliation == A1;
		  _ -> Affiliation == A
	      end
      end, maps:to_list(StateData#state.affiliations)).

-spec process_admin_items_set(jid(), [muc_item()], binary(),
			      #state{}) -> {result, undefined, #state{}} |
					   {error, stanza_error()}.
process_admin_items_set(UJID, Items, Lang, StateData) ->
    UAffiliation = get_affiliation(UJID, StateData),
    URole = get_role(UJID, StateData),
    case catch find_changed_items(UJID, UAffiliation, URole,
				  Items, Lang, StateData, [])
	of
      {result, Res} ->
	  ?INFO_MSG("Processing MUC admin query from ~ts in "
		    "room ~ts:~n ~p",
		    [jid:encode(UJID),
		     jid:encode(StateData#state.jid), Res]),
	  case lists:foldl(process_item_change(UJID),
			   StateData, lists:flatten(Res)) of
	      {error, _} = Err ->
		  Err;
	      NSD ->
		  store_room(NSD),
		  {result, undefined, NSD}
	  end;
	{error, Err} -> {error, Err}
    end.

-spec process_item_change(jid()) -> fun((admin_action(), state() | {error, stanza_error()}) ->
					       state() | {error, stanza_error()}).
process_item_change(UJID) ->
    fun(_, {error, _} = Err) ->
	    Err;
       (Item, SD) ->
	    process_item_change(Item, SD, UJID)
    end.

-spec process_item_change(admin_action(), state(), undefined | jid()) -> state() | {error, stanza_error()}.
process_item_change(Item, SD, UJID) ->
    try case Item of
	    {JID, affiliation, owner, _} when JID#jid.luser == <<"">> ->
		%% If the provided JID does not have username,
		%% forget the affiliation completely
		SD;
	    {JID, role, none, Reason} ->
		send_kickban_presence(UJID, JID, Reason, 307, SD),
		set_role(JID, none, SD);
	    {JID, affiliation, none, Reason} ->
                case get_affiliation(JID, SD) of
                    none -> SD;
                    _ ->
                        case (SD#state.config)#config.members_only of
                            true ->
                                send_kickban_presence(UJID, JID, Reason, 321, none, SD),
                                maybe_send_affiliation(JID, none, SD),
                                SD1 = set_affiliation(JID, none, SD),
                                set_role(JID, none, SD1);
                            _ ->
                                SD1 = set_affiliation(JID, none, SD),
                                SD2 = case (SD1#state.config)#config.moderated of
                                          true -> set_role(JID, visitor, SD1);
                                          false -> set_role(JID, participant, SD1)
                                      end,
                                send_update_presence(JID, Reason, SD2, SD),
                                maybe_send_affiliation(JID, none, SD2),
                                SD2
                        end
                end;
	    {JID, affiliation, outcast, Reason} ->
		send_kickban_presence(UJID, JID, Reason, 301, outcast, SD),
		maybe_send_affiliation(JID, outcast, SD),
                {result, undefined, SD2} =
                    process_iq_mucsub(JID,
                                      #iq{type = set,
                                          sub_els = [#muc_unsubscribe{}]}, SD),
		set_affiliation(JID, outcast, set_role(JID, none, SD2), Reason);
	    {JID, affiliation, A, Reason} when (A == admin) or (A == owner) ->
		SD1 = set_affiliation(JID, A, SD, Reason),
		SD2 = set_role(JID, moderator, SD1),
		send_update_presence(JID, Reason, SD2, SD),
		maybe_send_affiliation(JID, A, SD2),
		SD2;
	    {JID, affiliation, member, Reason} ->
		SD1 = set_affiliation(JID, member, SD, Reason),
		SD2 = set_role(JID, participant, SD1),
		send_update_presence(JID, Reason, SD2, SD),
		maybe_send_affiliation(JID, member, SD2),
		SD2;
	    {JID, role, Role, Reason} ->
		SD1 = set_role(JID, Role, SD),
		send_new_presence(JID, Reason, SD1, SD),
		SD1;
	    {JID, affiliation, A, _Reason} ->
		SD1 = set_affiliation(JID, A, SD),
		send_update_presence(JID, SD1, SD),
		maybe_send_affiliation(JID, A, SD1),
		SD1
	end
    catch ?EX_RULE(E, R, St) ->
	    StackTrace = ?EX_STACK(St),
	    FromSuffix = case UJID of
			     #jid{} ->
				 JidString = jid:encode(UJID),
				 <<" from ", JidString/binary>>;
			     undefined ->
				 <<"">>
			 end,
	    ?ERROR_MSG("Failed to set item ~p~ts:~n** ~ts",
		       [Item, FromSuffix,
			misc:format_exception(2, E, R, StackTrace)]),
	    {error, xmpp:err_internal_server_error()}
    end.

-spec find_changed_items(jid(), affiliation(), role(),
			 [muc_item()], binary(), state(), [admin_action()]) ->
				{result, [admin_action()]}.
find_changed_items(_UJID, _UAffiliation, _URole, [],
		   _Lang, _StateData, Res) ->
    {result, Res};
find_changed_items(_UJID, _UAffiliation, _URole,
		   [#muc_item{jid = undefined, nick = <<"">>}|_],
		   Lang, _StateData, _Res) ->
    Txt = ?T("Neither 'jid' nor 'nick' attribute found"),
    throw({error, xmpp:err_bad_request(Txt, Lang)});
find_changed_items(_UJID, _UAffiliation, _URole,
		   [#muc_item{role = undefined, affiliation = undefined}|_],
		   Lang, _StateData, _Res) ->
    Txt = ?T("Neither 'role' nor 'affiliation' attribute found"),
    throw({error, xmpp:err_bad_request(Txt, Lang)});
find_changed_items(UJID, UAffiliation, URole,
		   [#muc_item{jid = J, nick = Nick, reason = Reason,
			      role = Role, affiliation = Affiliation}|Items],
		   Lang, StateData, Res) ->
    [JID | _] = JIDs =
	if J /= undefined ->
		[J];
	   Nick /= <<"">> ->
		case find_jids_by_nick(Nick, StateData) of
		    [] ->
			ErrText = {?T("Nickname ~s does not exist in the room"),
				   [Nick]},
			throw({error, xmpp:err_not_acceptable(ErrText, Lang)});
		    JIDList ->
			JIDList
		end
	end,
    {RoleOrAff, RoleOrAffValue} = if Role == undefined ->
					  {affiliation, Affiliation};
				     true ->
					  {role, Role}
				  end,
    TAffiliation = get_affiliation(JID, StateData),
    TRole = get_role(JID, StateData),
    ServiceAf = get_service_affiliation(JID, StateData),
    UIsSubscriber = is_subscriber(UJID, StateData),
    URole1 = case {URole, UIsSubscriber} of
	{none, true} -> subscriber;
	{UR, _} -> UR
    end,
    CanChangeRA = case can_change_ra(UAffiliation,
				     URole1,
				     TAffiliation,
				     TRole, RoleOrAff, RoleOrAffValue,
				     ServiceAf) of
		      nothing -> nothing;
		      true -> true;
		      check_owner ->
			  case search_affiliation(owner, StateData) of
			      [{OJID, _}] ->
				  jid:remove_resource(OJID)
				      /=
				      jid:tolower(jid:remove_resource(UJID));
			      _ -> true
			  end;
		      _ -> false
		  end,
    case CanChangeRA of
	nothing ->
	    find_changed_items(UJID, UAffiliation, URole,
			       Items, Lang, StateData,
			       Res);
	true ->
	    MoreRes = case RoleOrAff of
			  affiliation ->
			      [{jid:remove_resource(Jidx),
				RoleOrAff, RoleOrAffValue, Reason}
			       || Jidx <- JIDs];
			  role ->
			      [{Jidx, RoleOrAff, RoleOrAffValue, Reason}
			       || Jidx <- JIDs]
		      end,
	    find_changed_items(UJID, UAffiliation, URole,
			       Items, Lang, StateData,
			       MoreRes ++ Res);
	false ->
	    Txt = ?T("Changing role/affiliation is not allowed"),
	    throw({error, xmpp:err_not_allowed(Txt, Lang)})
    end.

-spec can_change_ra(affiliation(), role(), affiliation(), role(),
		    affiliation, affiliation(), affiliation()) -> boolean() | nothing | check_owner;
		   (affiliation(), role(), affiliation(), role(),
		    role, role(), affiliation()) -> boolean() | nothing | check_owner.
can_change_ra(_FAffiliation, _FRole, owner, _TRole,
	      affiliation, owner, owner) ->
    %% A room owner tries to add as persistent owner a
    %% participant that is already owner because he is MUC admin
    true;
can_change_ra(_FAffiliation, _FRole, _TAffiliation,
	      _TRole, _RoleorAffiliation, _Value, owner) ->
    %% Nobody can decrease MUC admin's role/affiliation
    false;
can_change_ra(_FAffiliation, _FRole, TAffiliation,
	      _TRole, affiliation, Value, _ServiceAf)
    when TAffiliation == Value ->
    nothing;
can_change_ra(_FAffiliation, _FRole, _TAffiliation,
	      TRole, role, Value, _ServiceAf)
    when TRole == Value ->
    nothing;
can_change_ra(FAffiliation, _FRole, outcast, _TRole,
	      affiliation, none, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole, outcast, _TRole,
	      affiliation, member, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole, outcast, _TRole,
	      affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole, outcast, _TRole,
	      affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole, none, _TRole,
	      affiliation, outcast, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole, none, _TRole,
	      affiliation, member, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole, none, _TRole, affiliation,
	      admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole, none, _TRole, affiliation,
	      owner, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole, member, _TRole,
	      affiliation, outcast, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole, member, _TRole,
	      affiliation, none, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole, member, _TRole,
	      affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole, member, _TRole,
	      affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole, admin, _TRole, affiliation,
	      _Affiliation, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole, owner, _TRole, affiliation,
	      _Affiliation, _ServiceAf) ->
    check_owner;
can_change_ra(_FAffiliation, _FRole, _TAffiliation,
	      _TRole, affiliation, _Value, _ServiceAf) ->
    false;
can_change_ra(_FAffiliation, moderator, _TAffiliation,
	      visitor, role, none, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, subscriber, _TAffiliation,
	      visitor, role, none, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator, _TAffiliation,
	      visitor, role, participant, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, subscriber, _TAffiliation,
	      visitor, role, participant, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole, _TAffiliation,
	      visitor, role, moderator, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator, _TAffiliation,
	      participant, role, none, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, subscriber, _TAffiliation,
	      participant, role, none, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator, _TAffiliation,
	      participant, role, visitor, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, subscriber, _TAffiliation,
	      participant, role, visitor, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole, _TAffiliation,
	      participant, role, moderator, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, _FRole, owner, moderator,
	      role, visitor, _ServiceAf) ->
    false;
can_change_ra(owner, _FRole, _TAffiliation, moderator,
	      role, visitor, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole, admin, moderator,
	      role, visitor, _ServiceAf) ->
    false;
can_change_ra(admin, _FRole, _TAffiliation, moderator,
	      role, visitor, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole, owner, moderator,
	      role, participant, _ServiceAf) ->
    false;
can_change_ra(owner, _FRole, _TAffiliation, moderator,
	      role, participant, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole, admin, moderator,
	      role, participant, _ServiceAf) ->
    false;
can_change_ra(admin, _FRole, _TAffiliation, moderator,
	      role, participant, _ServiceAf) ->
    true;
can_change_ra(owner, moderator, TAffiliation,
	      moderator, role, none, _ServiceAf)
    when TAffiliation /= owner ->
    true;
can_change_ra(owner, subscriber, TAffiliation,
	      moderator, role, none, _ServiceAf)
    when TAffiliation /= owner ->
    true;
can_change_ra(admin, moderator, TAffiliation,
	      moderator, role, none, _ServiceAf)
    when (TAffiliation /= owner) and
         (TAffiliation /= admin) ->
    true;
can_change_ra(admin, subscriber, TAffiliation,
	      moderator, role, none, _ServiceAf)
    when (TAffiliation /= owner) and
         (TAffiliation /= admin) ->
    true;
can_change_ra(_FAffiliation, _FRole, _TAffiliation,
	      _TRole, role, _Value, _ServiceAf) ->
    false.

-spec send_kickban_presence(undefined | jid(), jid(), binary(),
			    pos_integer(), state()) -> ok.
send_kickban_presence(UJID, JID, Reason, Code, StateData) ->
    NewAffiliation = get_affiliation(JID, StateData),
    send_kickban_presence(UJID, JID, Reason, Code, NewAffiliation,
			  StateData).

-spec send_kickban_presence(undefined | jid(), jid(), binary(), pos_integer(),
			    affiliation(), state()) -> ok.
send_kickban_presence(UJID, JID, Reason, Code, NewAffiliation,
		      StateData) ->
    LJID = jid:tolower(JID),
    LJIDs = case LJID of
		{U, S, <<"">>} ->
		    maps:fold(fun (J, _, Js) ->
				      case J of
					  {U, S, _} -> [J | Js];
					  _ -> Js
				      end
			      end, [], StateData#state.users);
		_ ->
		    case maps:is_key(LJID, StateData#state.users) of
			true -> [LJID];
			_ -> []
		    end
	    end,
    lists:foreach(fun (LJ) ->
			  #user{nick = Nick, jid = J} = maps:get(LJ, StateData#state.users),
			  add_to_log(kickban, {Nick, Reason, Code}, StateData),
			  tab_remove_online_user(J, StateData),
			  send_kickban_presence1(UJID, J, Reason, Code,
						 NewAffiliation, StateData)
		  end,
		  LJIDs).

-spec send_kickban_presence1(undefined | jid(), jid(), binary(), pos_integer(),
			     affiliation(), state()) -> ok.
send_kickban_presence1(MJID, UJID, Reason, Code, Affiliation,
		       StateData) ->
    #user{jid = RealJID, nick = Nick} = maps:get(jid:tolower(UJID), StateData#state.users),
    ActorNick = get_actor_nick(MJID, StateData),
    %% TODO: optimize further
    UserMap =
        maps:merge(
          get_users_and_subscribers_with_node(
            ?NS_MUCSUB_NODES_AFFILIATIONS, StateData),
          get_users_and_subscribers_with_node(
            ?NS_MUCSUB_NODES_PARTICIPANTS, StateData)),
    maps:fold(
      fun(LJID, Info, _) ->
	      IsSelfPresence = jid:tolower(UJID) == LJID,
	      Item0 = #muc_item{affiliation = Affiliation,
				role = none},
	      Item1 = case Info#user.role == moderator orelse
			  (StateData#state.config)#config.anonymous
			  == false orelse IsSelfPresence of
			  true -> Item0#muc_item{jid = RealJID};
			  false -> Item0
		      end,
	      Item2 = Item1#muc_item{reason = Reason},
	      Item = case ActorNick of
			 <<"">> -> Item2;
			 _ -> Item2#muc_item{actor = #muc_actor{nick = ActorNick}}
		     end,
	      Codes = if IsSelfPresence -> [110, Code];
			 true -> [Code]
		      end,
	      Packet = #presence{type = unavailable,
				 sub_els = [#muc_user{items = [Item],
						      status_codes = Codes}]},
	      RoomJIDNick = jid:replace_resource(StateData#state.jid, Nick),
	      send_wrapped(RoomJIDNick, Info#user.jid, Packet,
			   ?NS_MUCSUB_NODES_AFFILIATIONS, StateData),
			  IsSubscriber = is_subscriber(Info#user.jid, StateData),
	      IsOccupant = Info#user.last_presence /= undefined,
	      if (IsSubscriber and not IsOccupant) ->
		      send_wrapped(RoomJIDNick, Info#user.jid, Packet,
				   ?NS_MUCSUB_NODES_PARTICIPANTS, StateData);
		 true ->
		      ok
	      end
      end, ok, UserMap).

-spec get_actor_nick(undefined | jid(), state()) -> binary().
get_actor_nick(undefined, _StateData) ->
    <<"">>;
get_actor_nick(MJID, StateData) ->
    try maps:get(jid:tolower(MJID), StateData#state.users) of
	#user{nick = ActorNick} -> ActorNick
    catch _:{badkey, _} -> <<"">>
    end.

-spec convert_legacy_fields([xdata_field()]) -> [xdata_field()].
convert_legacy_fields(Fs) ->
    lists:map(
      fun(#xdata_field{var = Var} = F) ->
	      NewVar = case Var of
			   <<"muc#roomconfig_allowvisitorstatus">> ->
			       <<"allow_visitor_status">>;
			   <<"muc#roomconfig_allowvisitornickchange">> ->
			       <<"allow_visitor_nickchange">>;
			   <<"muc#roomconfig_allowvoicerequests">> ->
			       <<"allow_voice_requests">>;
			   <<"muc#roomconfig_allow_subscription">> ->
			       <<"allow_subscription">>;
			   <<"muc#roomconfig_voicerequestmininterval">> ->
			       <<"voice_request_min_interval">>;
			   <<"muc#roomconfig_captcha_whitelist">> ->
			       <<"captcha_whitelist">>;
			   <<"muc#roomconfig_mam">> ->
			       <<"mam">>;
			   _ ->
			       Var
		       end,
	      F#xdata_field{var = NewVar}
      end, Fs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff
-spec process_iq_owner(jid(), iq(), state()) ->
			      {result, undefined | muc_owner()} |
			      {result, undefined | muc_owner(), state() | stop} |
			      {error, stanza_error()}.
process_iq_owner(From, #iq{type = set, lang = Lang,
			   sub_els = [#muc_owner{destroy = Destroy,
						 config = Config,
						 items = Items}]},
		 StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    if FAffiliation /= owner ->
	    ErrText = ?T("Owner privileges required"),
	    {error, xmpp:err_forbidden(ErrText, Lang)};
       Destroy /= undefined, Config == undefined, Items == [] ->
	    ?INFO_MSG("Destroyed MUC room ~ts by the owner ~ts",
		      [jid:encode(StateData#state.jid), jid:encode(From)]),
	    add_to_log(room_existence, destroyed, StateData),
	    destroy_room(Destroy, StateData);
       Config /= undefined, Destroy == undefined, Items == [] ->
	    case Config of
		#xdata{type = cancel} ->
		    {result, undefined};
		#xdata{type = submit, fields = Fs} ->
		    Fs1 = convert_legacy_fields(Fs),
		    try muc_roomconfig:decode(Fs1) of
			Options ->
			    case is_allowed_log_change(Options, StateData, From) andalso
				is_allowed_persistent_change(Options, StateData, From) andalso
				is_allowed_mam_change(Options, StateData, From) andalso
				is_allowed_string_limits(Options, StateData) andalso
				is_password_settings_correct(Options, StateData) of
				true ->
				    set_config(Options, StateData, Lang);
				false ->
				    {error, xmpp:err_not_acceptable()}
			    end
		    catch _:{muc_roomconfig, Why} ->
			    Txt = muc_roomconfig:format_error(Why),
			    {error, xmpp:err_bad_request(Txt, Lang)}
		    end;
		_ ->
		    Txt = ?T("Incorrect data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end;
       Items /= [], Config == undefined, Destroy == undefined ->
	    process_admin_items_set(From, Items, Lang, StateData);
       true ->
	    {error, xmpp:err_bad_request()}
    end;
process_iq_owner(From, #iq{type = get, lang = Lang,
			   sub_els = [#muc_owner{destroy = Destroy,
						 config = Config,
						 items = Items}]},
		 StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    if FAffiliation /= owner ->
	    ErrText = ?T("Owner privileges required"),
	    {error, xmpp:err_forbidden(ErrText, Lang)};
       Destroy == undefined, Config == undefined ->
	    case Items of
		[] ->
		    {result,
		     #muc_owner{config = get_config(Lang, StateData, From)}};
		[#muc_item{affiliation = undefined}] ->
		    Txt = ?T("No 'affiliation' attribute found"),
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[#muc_item{affiliation = Affiliation}] ->
		    Items = items_with_affiliation(Affiliation, StateData),
		    {result, #muc_owner{items = Items}};
		[_|_] ->
		    Txt = ?T("Too many <item/> elements"),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end;
       true ->
	    {error, xmpp:err_bad_request()}
    end.

-spec is_allowed_log_change(muc_roomconfig:result(), state(), jid()) -> boolean().
is_allowed_log_change(Options, StateData, From) ->
    case proplists:is_defined(enablelogging, Options) of
	false -> true;
	true ->
	    allow ==
		mod_muc_log:check_access_log(StateData#state.server_host,
					     From)
    end.

-spec is_allowed_persistent_change(muc_roomconfig:result(), state(), jid()) -> boolean().
is_allowed_persistent_change(Options, StateData, From) ->
    case proplists:is_defined(persistentroom, Options) of
      false -> true;
      true ->
	  {_AccessRoute, _AccessCreate, _AccessAdmin,
	   AccessPersistent, _AccessMam} =
	      StateData#state.access,
	  allow ==
	    acl:match_rule(StateData#state.server_host,
			   AccessPersistent, From)
    end.

-spec is_allowed_mam_change(muc_roomconfig:result(), state(), jid()) -> boolean().
is_allowed_mam_change(Options, StateData, From) ->
    case proplists:is_defined(mam, Options) of
      false -> true;
      true ->
	  {_AccessRoute, _AccessCreate, _AccessAdmin,
	   _AccessPersistent, AccessMam} =
	      StateData#state.access,
	  allow ==
	    acl:match_rule(StateData#state.server_host,
			   AccessMam, From)
    end.

%% Check if the string fields defined in the Data Form
%% are conformant to the configured limits
-spec is_allowed_string_limits(muc_roomconfig:result(), state()) -> boolean().
is_allowed_string_limits(Options, StateData) ->
    RoomName = proplists:get_value(roomname, Options, <<"">>),
    RoomDesc = proplists:get_value(roomdesc, Options, <<"">>),
    Password = proplists:get_value(roomsecret, Options, <<"">>),
    CaptchaWhitelist = proplists:get_value(captcha_whitelist, Options, []),
    CaptchaWhitelistSize = lists:foldl(
      fun(Jid, Sum) -> byte_size(jid:encode(Jid)) + Sum end,
      0, CaptchaWhitelist),
    MaxRoomName = mod_muc_opt:max_room_name(StateData#state.server_host),
    MaxRoomDesc = mod_muc_opt:max_room_desc(StateData#state.server_host),
    MaxPassword = mod_muc_opt:max_password(StateData#state.server_host),
    MaxCaptchaWhitelist = mod_muc_opt:max_captcha_whitelist(StateData#state.server_host),
    (byte_size(RoomName) =< MaxRoomName)
    andalso (byte_size(RoomDesc) =< MaxRoomDesc)
    andalso (byte_size(Password) =< MaxPassword)
    andalso (CaptchaWhitelistSize =< MaxCaptchaWhitelist).

%% Return false if:
%% "the password for a password-protected room is blank"
-spec is_password_settings_correct(muc_roomconfig:result(), state()) -> boolean().
is_password_settings_correct(Options, StateData) ->
    Config = StateData#state.config,
    OldProtected = Config#config.password_protected,
    OldPassword = Config#config.password,
    NewProtected = proplists:get_value(passwordprotectedroom, Options),
    NewPassword = proplists:get_value(roomsecret, Options),
    case {OldProtected, NewProtected, OldPassword, NewPassword} of
	{true, undefined, <<"">>, undefined} -> false;
	{true, undefined, _, <<"">>} -> false;
	{_, true, <<"">>, undefined} -> false;
	{_, true, _, <<"">>} -> false;
	_ -> true
    end.

-spec get_default_room_maxusers(state()) -> non_neg_integer().
get_default_room_maxusers(RoomState) ->
    DefRoomOpts =
	mod_muc_opt:default_room_options(RoomState#state.server_host),
    RoomState2 = set_opts(DefRoomOpts, RoomState),
    (RoomState2#state.config)#config.max_users.

-spec get_config(binary(), state(), jid()) -> xdata().
get_config(Lang, StateData, From) ->
    {_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent, _AccessMam} =
	StateData#state.access,
    ServiceMaxUsers = get_service_max_users(StateData),
    DefaultRoomMaxUsers = get_default_room_maxusers(StateData),
    Config = StateData#state.config,
    MaxUsersRoom = get_max_users(StateData),
    Title = str:translate_and_format(
	      Lang, ?T("Configuration of room ~s"),
	      [jid:encode(StateData#state.jid)]),
    Fs = [{roomname, Config#config.title},
	  {roomdesc, Config#config.description},
	  {lang, Config#config.lang}] ++
	case acl:match_rule(StateData#state.server_host, AccessPersistent, From) of
	    allow -> [{persistentroom, Config#config.persistent}];
	    deny -> []
	end ++
	[{publicroom, Config#config.public},
	 {public_list, Config#config.public_list},
	 {passwordprotectedroom, Config#config.password_protected},
	 {roomsecret, case Config#config.password_protected of
			  true -> Config#config.password;
			  false -> <<"">>
		      end},
	 {maxusers, MaxUsersRoom,
	  [if is_integer(ServiceMaxUsers) -> [];
	      true -> [{?T("No limit"), <<"none">>}]
	   end] ++ [{integer_to_binary(N), N}
		    || N <- lists:usort([ServiceMaxUsers,
					 DefaultRoomMaxUsers,
					 MaxUsersRoom
					 | ?MAX_USERS_DEFAULT_LIST]),
		       N =< ServiceMaxUsers]},
	 {whois, if Config#config.anonymous -> moderators;
		    true -> anyone
		 end},
	 {presencebroadcast, Config#config.presence_broadcast},
	 {membersonly, Config#config.members_only},
	 {moderatedroom, Config#config.moderated},
	 {members_by_default, Config#config.members_by_default},
	 {changesubject, Config#config.allow_change_subj},
	 {allow_private_messages, Config#config.allow_private_messages},
	 {allow_private_messages_from_visitors,
	  Config#config.allow_private_messages_from_visitors},
	 {allow_query_users, Config#config.allow_query_users},
	 {allowinvites, Config#config.allow_user_invites},
	 {allow_visitor_status, Config#config.allow_visitor_status},
	 {allow_visitor_nickchange, Config#config.allow_visitor_nickchange},
	 {allow_voice_requests, Config#config.allow_voice_requests},
	 {allow_subscription, Config#config.allow_subscription},
	 {voice_request_min_interval, Config#config.voice_request_min_interval},
	 {pubsub, Config#config.pubsub},
	 {enable_hats, Config#config.enable_hats}]
	++
	case ejabberd_captcha:is_feature_available() of
	    true ->
		[{captcha_protected, Config#config.captcha_protected},
		 {captcha_whitelist,
		  lists:map(
		    fun jid:make/1,
		    ?SETS:to_list(Config#config.captcha_whitelist))}];
	    false ->
		[]
	end
	++
	case mod_muc_log:check_access_log(StateData#state.server_host, From) of
	    allow -> [{enablelogging, Config#config.logging}];
	    deny -> []
	end,
    Fields = ejabberd_hooks:run_fold(get_room_config,
				     StateData#state.server_host,
				     Fs,
				     [StateData, From, Lang]),
    #xdata{type = form, title = Title,
	   fields = muc_roomconfig:encode(Fields, Lang)}.

-spec set_config(muc_roomconfig:result(), state(), binary()) ->
			{error, stanza_error()} | {result, undefined, state()}.
set_config(Options, StateData, Lang) ->
    try
	#config{} = Config = set_config(Options, StateData#state.config,
					StateData#state.server_host, Lang),
	{result, _, NSD} = Res = change_config(Config, StateData),
	Type = case {(StateData#state.config)#config.logging,
		     Config#config.logging}
	       of
		   {true, false} -> roomconfig_change_disabledlogging;
		   {false, true} -> roomconfig_change_enabledlogging;
		   {_, _} -> roomconfig_change
	       end,
	Users = [{U#user.jid, U#user.nick, U#user.role}
		 || U <- maps:values(StateData#state.users)],
	add_to_log(Type, Users, NSD),
	Res
    catch  _:{badmatch, {error, #stanza_error{}} = Err} ->
	    Err
    end.

-spec get_config_opt_name(pos_integer()) -> atom().
get_config_opt_name(Pos) ->
    Fs = [config|record_info(fields, config)],
    lists:nth(Pos, Fs).

-spec set_config([muc_roomconfig:property()], #config{},
		  binary(), binary()) -> #config{} | {error, stanza_error()}.
set_config(Opts, Config, ServerHost, Lang) ->
    lists:foldl(
      fun(_, {error, _} = Err) -> Err;
	 ({roomname, Title}, C) -> C#config{title = Title};
	 ({roomdesc, Desc}, C) -> C#config{description = Desc};
	 ({changesubject, V}, C) -> C#config{allow_change_subj = V};
	 ({allow_query_users, V}, C) -> C#config{allow_query_users = V};
	 ({allow_private_messages, V}, C) ->
	      C#config{allow_private_messages = V};
	 ({allow_private_messages_from_visitors, V}, C) ->
	      C#config{allow_private_messages_from_visitors = V};
	 ({allow_visitor_status, V}, C) -> C#config{allow_visitor_status = V};
	 ({allow_visitor_nickchange, V}, C) ->
	      C#config{allow_visitor_nickchange = V};
	 ({publicroom, V}, C) -> C#config{public = V};
	 ({public_list, V}, C) -> C#config{public_list = V};
	 ({persistentroom, V}, C) -> C#config{persistent = V};
	 ({moderatedroom, V}, C) -> C#config{moderated = V};
	 ({members_by_default, V}, C) -> C#config{members_by_default = V};
	 ({membersonly, V}, C) -> C#config{members_only = V};
	 ({captcha_protected, V}, C) -> C#config{captcha_protected = V};
	 ({allowinvites, V}, C) -> C#config{allow_user_invites = V};
	 ({allow_subscription, V}, C) -> C#config{allow_subscription = V};
	 ({passwordprotectedroom, V}, C) -> C#config{password_protected = V};
	 ({roomsecret, V}, C) -> C#config{password = V};
	 ({anonymous, V}, C) -> C#config{anonymous = V};
	 ({presencebroadcast, V}, C) -> C#config{presence_broadcast = V};
	 ({allow_voice_requests, V}, C) -> C#config{allow_voice_requests = V};
	 ({voice_request_min_interval, V}, C) ->
	      C#config{voice_request_min_interval = V};
	 ({whois, moderators}, C) -> C#config{anonymous = true};
	 ({whois, anyone}, C) -> C#config{anonymous = false};
	 ({maxusers, V}, C) -> C#config{max_users = V};
	 ({enablelogging, V}, C) -> C#config{logging = V};
	 ({pubsub, V}, C) -> C#config{pubsub = V};
	 ({enable_hats, V}, C) -> C#config{enable_hats = V};
	 ({lang, L}, C) -> C#config{lang = L};
	 ({captcha_whitelist, Js}, C) ->
	      LJIDs = [jid:tolower(J) || J <- Js],
	      C#config{captcha_whitelist = ?SETS:from_list(LJIDs)};
	 ({O, V} = Opt, C) ->
	      case ejabberd_hooks:run_fold(set_room_option,
					   ServerHost,
					   {0, undefined},
					   [Opt, Lang]) of
		  {0, undefined} ->
		      ?ERROR_MSG("set_room_option hook failed for "
				 "option '~ts' with value ~p", [O, V]),
		      Txt = {?T("Failed to process option '~s'"), [O]},
		      {error, xmpp:err_internal_server_error(Txt, Lang)};
		  {Pos, Val} ->
		      setelement(Pos, C, Val)
	      end
      end, Config, Opts).

-spec change_config(#config{}, state()) -> {result, undefined, state()}.
change_config(Config, StateData) ->
    send_config_change_info(Config, StateData),
    StateData0 = StateData#state{config = Config},
    StateData1 = remove_subscriptions(StateData0),
    StateData2 =
        case {(StateData#state.config)#config.persistent,
              Config#config.persistent} of
            {WasPersistent, true} ->
                if not WasPersistent ->
                        set_affiliations(StateData1#state.affiliations,
                                         StateData1);
                   true ->
                        ok
                end,
                store_room(StateData1),
                StateData1;
            {true, false} ->
		Affiliations = get_affiliations(StateData),
		maybe_forget_room(StateData),
		StateData1#state{affiliations = Affiliations};
	    _ ->
		StateData1
        end,
    case {(StateData#state.config)#config.members_only,
	  Config#config.members_only} of
        {false, true} ->
            StateData3 = remove_nonmembers(StateData2),
            {result, undefined, StateData3};
        _ ->
            {result, undefined, StateData2}
    end.

-spec send_config_change_info(#config{}, state()) -> ok.
send_config_change_info(Config, #state{config = Config}) -> ok;
send_config_change_info(New, #state{config = Old} = StateData) ->
    Codes = case {Old#config.logging, New#config.logging} of
	      {false, true} -> [170];
	      {true, false} -> [171];
	      _ -> []
	    end
	      ++
	      case {Old#config.anonymous, New#config.anonymous} of
		{true, false} -> [172];
		{false, true} -> [173];
		_ -> []
	      end
		++
		case Old#config{anonymous = New#config.anonymous,
				logging = New#config.logging} of
		  New -> [];
		  _ -> [104]
		end,
    if Codes /= [] ->
	    maps:fold(
	      fun(_LJID, #user{jid = JID}, _) ->
		      advertise_entity_capabilities(JID, StateData#state{config = New})
	      end, ok, StateData#state.users),
	    Message = #message{type = groupchat,
			       id = p1_rand:get_string(),
			       sub_els = [#muc_user{status_codes = Codes}]},
	    send_wrapped_multiple(StateData#state.jid,
				  get_users_and_subscribers_with_node(
                                    ?NS_MUCSUB_NODES_CONFIG, StateData),
				  Message,
				  ?NS_MUCSUB_NODES_CONFIG,
				  StateData);
       true ->
	    ok
    end.

-spec remove_nonmembers(state()) -> state().
remove_nonmembers(StateData) ->
    maps:fold(
      fun(_LJID, #user{jid = JID}, SD) ->
	      Affiliation = get_affiliation(JID, SD),
	      case Affiliation of
		  none ->
		      catch send_kickban_presence(undefined, JID, <<"">>, 322, SD),
		      set_role(JID, none, SD);
		  _ -> SD
	      end
      end, StateData, get_users_and_subscribers(StateData)).

-spec set_opts([{atom(), any()}], state()) -> state().
set_opts([], StateData) ->
    set_vcard_xupdate(StateData);
set_opts([{vcard, Val} | Opts], StateData)
  when is_record(Val, vcard_temp) ->
    %% default_room_options is setting a default room vcard
    ValRaw = fxml:element_to_binary(xmpp:encode(Val)),
    set_opts([{vcard, ValRaw} | Opts], StateData);
set_opts([{Opt, Val} | Opts], StateData) ->
    NSD = case Opt of
	    title ->
		StateData#state{config =
				    (StateData#state.config)#config{title =
									Val}};
	    description ->
		StateData#state{config =
				    (StateData#state.config)#config{description
									= Val}};
	    allow_change_subj ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_change_subj
									= Val}};
	    allow_query_users ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_query_users
									= Val}};
	    allow_private_messages ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_private_messages
									= Val}};
	    allow_private_messages_from_visitors ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_private_messages_from_visitors
									= Val}};
	    allow_visitor_nickchange ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_visitor_nickchange
									= Val}};
	    allow_visitor_status ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_visitor_status
									= Val}};
	    public ->
		StateData#state{config =
				    (StateData#state.config)#config{public =
									Val}};
	    public_list ->
		StateData#state{config =
				    (StateData#state.config)#config{public_list
									= Val}};
	    persistent ->
		StateData#state{config =
				    (StateData#state.config)#config{persistent =
									Val}};
	    moderated ->
		StateData#state{config =
				    (StateData#state.config)#config{moderated =
									Val}};
	    members_by_default ->
		StateData#state{config =
				    (StateData#state.config)#config{members_by_default
									= Val}};
	    members_only ->
		StateData#state{config =
				    (StateData#state.config)#config{members_only
									= Val}};
	    allow_user_invites ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_user_invites
									= Val}};
	    password_protected ->
		StateData#state{config =
				    (StateData#state.config)#config{password_protected
									= Val}};
	    captcha_protected ->
		StateData#state{config =
				    (StateData#state.config)#config{captcha_protected
									= Val}};
	    password ->
		StateData#state{config =
				    (StateData#state.config)#config{password =
									Val}};
	    anonymous ->
		StateData#state{config =
				    (StateData#state.config)#config{anonymous =
									Val}};
	    presence_broadcast ->
		StateData#state{config =
				    (StateData#state.config)#config{presence_broadcast =
									Val}};
	    logging ->
		StateData#state{config =
				    (StateData#state.config)#config{logging =
									Val}};
	    mam ->
		StateData#state{config =
				    (StateData#state.config)#config{mam = Val}};
	    captcha_whitelist ->
		StateData#state{config =
				    (StateData#state.config)#config{captcha_whitelist
									=
									(?SETS):from_list(Val)}};
	    allow_voice_requests ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_voice_requests
									= Val}};
	    voice_request_min_interval ->
		StateData#state{config =
				    (StateData#state.config)#config{voice_request_min_interval
									= Val}};
	    max_users ->
		ServiceMaxUsers = get_service_max_users(StateData),
		MaxUsers = if Val =< ServiceMaxUsers -> Val;
			      true -> ServiceMaxUsers
			   end,
		StateData#state{config =
				    (StateData#state.config)#config{max_users =
									MaxUsers}};
	    vcard ->
		StateData#state{config =
				    (StateData#state.config)#config{vcard =
									Val}};
	    vcard_xupdate ->
		StateData#state{config =
				    (StateData#state.config)#config{vcard_xupdate =
									Val}};
	    pubsub ->
		StateData#state{config =
				    (StateData#state.config)#config{pubsub = Val}};
	    allow_subscription ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_subscription = Val}};
            enable_hats ->
                StateData#state{config =
                                    (StateData#state.config)#config{enable_hats = Val}};
	    lang ->
		StateData#state{config =
				    (StateData#state.config)#config{lang = Val}};
	    subscribers ->
                  MUCSubscribers =
                      lists:foldl(
                        fun({JID, Nick, Nodes}, MUCSubs) ->
                                BareJID =
                                    case JID of
                                        #jid{} -> jid:remove_resource(JID);
                                        _ ->
                                            ?ERROR_MSG("Invalid subscriber JID in set_opts ~p", [JID]),
                                            jid:remove_resource(jid:make(JID))
                                    end,
                                muc_subscribers_put(
                                  #subscriber{jid = BareJID,
                                              nick = Nick,
                                              nodes = Nodes},
                                  MUCSubs)
                        end, muc_subscribers_new(), Val),
                  StateData#state{muc_subscribers = MUCSubscribers};
	    affiliations ->
		StateData#state{affiliations = maps:from_list(Val)};
	    subject ->
		  Subj = if Val == <<"">> -> [];
			    is_binary(Val) -> [#text{data = Val}];
			    is_list(Val) -> Val
			 end,
		  StateData#state{subject = Subj};
	    subject_author -> StateData#state{subject_author = Val};
            hats_users ->
                  Hats = maps:from_list(
                           lists:map(fun({U, H}) -> {U, maps:from_list(H)} end,
                                     Val)),
                  StateData#state{hats_users = Hats};
	    _ -> StateData
	  end,
    set_opts(Opts, NSD).

-spec set_vcard_xupdate(state()) -> state().
set_vcard_xupdate(#state{config =
			     #config{vcard = VCardRaw,
				     vcard_xupdate = undefined} = Config} = State)
  when VCardRaw /= <<"">> ->
    case fxml_stream:parse_element(VCardRaw) of
	{error, _} ->
	    State;
	El ->
	    Hash = mod_vcard_xupdate:compute_hash(El),
	    State#state{config = Config#config{vcard_xupdate = Hash}}
    end;
set_vcard_xupdate(State) ->
    State.

-define(MAKE_CONFIG_OPT(Opt),
	{get_config_opt_name(Opt), element(Opt, Config)}).

-spec make_opts(state(), boolean()) -> [{atom(), any()}].
make_opts(StateData, Hibernation) ->
    Config = StateData#state.config,
    Subscribers = muc_subscribers_fold(
		    fun(_LJID, Sub, Acc) ->
			    [{Sub#subscriber.jid,
			      Sub#subscriber.nick,
			      Sub#subscriber.nodes}|Acc]
		    end, [], StateData#state.muc_subscribers),
    [?MAKE_CONFIG_OPT(#config.title), ?MAKE_CONFIG_OPT(#config.description),
     ?MAKE_CONFIG_OPT(#config.allow_change_subj),
     ?MAKE_CONFIG_OPT(#config.allow_query_users),
     ?MAKE_CONFIG_OPT(#config.allow_private_messages),
     ?MAKE_CONFIG_OPT(#config.allow_private_messages_from_visitors),
     ?MAKE_CONFIG_OPT(#config.allow_visitor_status),
     ?MAKE_CONFIG_OPT(#config.allow_visitor_nickchange),
     ?MAKE_CONFIG_OPT(#config.public), ?MAKE_CONFIG_OPT(#config.public_list),
     ?MAKE_CONFIG_OPT(#config.persistent),
     ?MAKE_CONFIG_OPT(#config.moderated),
     ?MAKE_CONFIG_OPT(#config.members_by_default),
     ?MAKE_CONFIG_OPT(#config.members_only),
     ?MAKE_CONFIG_OPT(#config.allow_user_invites),
     ?MAKE_CONFIG_OPT(#config.password_protected),
     ?MAKE_CONFIG_OPT(#config.captcha_protected),
     ?MAKE_CONFIG_OPT(#config.password), ?MAKE_CONFIG_OPT(#config.anonymous),
     ?MAKE_CONFIG_OPT(#config.logging), ?MAKE_CONFIG_OPT(#config.max_users),
     ?MAKE_CONFIG_OPT(#config.allow_voice_requests),
     ?MAKE_CONFIG_OPT(#config.allow_subscription),
     ?MAKE_CONFIG_OPT(#config.mam),
     ?MAKE_CONFIG_OPT(#config.presence_broadcast),
     ?MAKE_CONFIG_OPT(#config.voice_request_min_interval),
     ?MAKE_CONFIG_OPT(#config.vcard),
     ?MAKE_CONFIG_OPT(#config.vcard_xupdate),
     ?MAKE_CONFIG_OPT(#config.pubsub),
     ?MAKE_CONFIG_OPT(#config.enable_hats),
     ?MAKE_CONFIG_OPT(#config.lang),
     {captcha_whitelist,
      (?SETS):to_list((StateData#state.config)#config.captcha_whitelist)},
     {affiliations,
      maps:to_list(StateData#state.affiliations)},
     {subject, StateData#state.subject},
     {subject_author, StateData#state.subject_author},
     {hats_users,
      lists:map(fun({U, H}) -> {U, maps:to_list(H)} end,
                maps:to_list(StateData#state.hats_users))},
     {hibernation_time, if Hibernation -> erlang:system_time(microsecond); true -> undefined end},
     {subscribers, Subscribers}].

expand_opts(CompactOpts) ->
    DefConfig = #config{},
    Fields = record_info(fields, config),
    {_, Opts1} =
        lists:foldl(
          fun(Field, {Pos, Opts}) ->
                  case lists:keyfind(Field, 1, CompactOpts) of
                      false ->
                          DefV = element(Pos, DefConfig),
                          DefVal = case (?SETS):is_set(DefV) of
                                       true -> (?SETS):to_list(DefV);
                                       false -> DefV
                                   end,
                          {Pos+1, [{Field, DefVal}|Opts]};
                      {_, Val} ->
                          {Pos+1, [{Field, Val}|Opts]}
                  end
          end, {2, []}, Fields),
    SubjectAuthor = proplists:get_value(subject_author, CompactOpts, <<"">>),
    Subject = proplists:get_value(subject, CompactOpts, <<"">>),
    Subscribers = proplists:get_value(subscribers, CompactOpts, []),
    HibernationTime = proplists:get_value(hibernation_time, CompactOpts, 0),
    [{subject, Subject},
     {subject_author, SubjectAuthor},
     {subscribers, Subscribers},
     {hibernation_time, HibernationTime}
     | lists:reverse(Opts1)].

config_fields() ->
    [subject, subject_author, subscribers, hibernate_time | record_info(fields, config)].

-spec destroy_room(muc_destroy(), state()) -> {result, undefined, stop}.
destroy_room(DEl, StateData) ->
    Destroy = DEl#muc_destroy{xmlns = ?NS_MUC_USER},
    maps:fold(
      fun(_LJID, Info, _) ->
	      Nick = Info#user.nick,
	      Item = #muc_item{affiliation = none,
			       role = none},
	      Packet = #presence{
			  type = unavailable,
			  sub_els = [#muc_user{items = [Item],
					       destroy = Destroy}]},
	      send_wrapped(jid:replace_resource(StateData#state.jid, Nick),
			   Info#user.jid, Packet,
			   ?NS_MUCSUB_NODES_CONFIG, StateData)
      end, ok, get_users_and_subscribers_with_node(
                 ?NS_MUCSUB_NODES_CONFIG, StateData)),
    forget_room(StateData),
    {result, undefined, stop}.

-spec forget_room(state()) -> state().
forget_room(StateData) ->
    mod_muc:forget_room(StateData#state.server_host,
			StateData#state.host,
			StateData#state.room),
    StateData.

-spec maybe_forget_room(state()) -> state().
maybe_forget_room(StateData) ->
    Forget = case (StateData#state.config)#config.persistent of
		 true ->
		     true;
		 _ ->
		     Mod = gen_mod:db_mod(StateData#state.server_host, mod_muc),
		     erlang:function_exported(Mod, get_subscribed_rooms, 3)
	     end,
    case Forget of
	true ->
	    forget_room(StateData);
	_ ->
	    StateData
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disco

-define(CONFIG_OPT_TO_FEATURE(Opt, Fiftrue, Fiffalse),
	case Opt of
	  true -> Fiftrue;
	  false -> Fiffalse
	end).

-spec make_disco_info(jid(), state()) -> disco_info().
make_disco_info(_From, StateData) ->
    Config = StateData#state.config,
    Feats = [?NS_VCARD, ?NS_MUC, ?NS_DISCO_INFO, ?NS_DISCO_ITEMS,
             ?NS_COMMANDS,
	     ?CONFIG_OPT_TO_FEATURE((Config#config.public),
				    <<"muc_public">>, <<"muc_hidden">>),
	     ?CONFIG_OPT_TO_FEATURE((Config#config.persistent),
				    <<"muc_persistent">>, <<"muc_temporary">>),
	     ?CONFIG_OPT_TO_FEATURE((Config#config.members_only),
				    <<"muc_membersonly">>, <<"muc_open">>),
	     ?CONFIG_OPT_TO_FEATURE((Config#config.anonymous),
				    <<"muc_semianonymous">>, <<"muc_nonanonymous">>),
	     ?CONFIG_OPT_TO_FEATURE((Config#config.moderated),
				    <<"muc_moderated">>, <<"muc_unmoderated">>),
	     ?CONFIG_OPT_TO_FEATURE((Config#config.password_protected),
				    <<"muc_passwordprotected">>, <<"muc_unsecured">>)]
	++ case Config#config.allow_subscription of
	       true -> [?NS_MUCSUB];
	       false -> []
	   end
	++ case {gen_mod:is_loaded(StateData#state.server_host, mod_mam),
		 Config#config.mam} of
	       {true, true} ->
		   [?NS_MAM_TMP, ?NS_MAM_0, ?NS_MAM_1, ?NS_MAM_2, ?NS_SID_0];
	       _ ->
		   []
	   end,
    #disco_info{identities = [#identity{category = <<"conference">>,
					type = <<"text">>,
					name = (StateData#state.config)#config.title}],
		features = Feats}.

-spec process_iq_disco_info(jid(), iq(), state()) ->
				   {result, disco_info()} | {error, stanza_error()}.
process_iq_disco_info(_From, #iq{type = set, lang = Lang}, _StateData) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    {error, xmpp:err_not_allowed(Txt, Lang)};
process_iq_disco_info(From, #iq{type = get, lang = Lang,
				sub_els = [#disco_info{node = <<>>}]},
		      StateData) ->
    DiscoInfo = make_disco_info(From, StateData),
    Extras = iq_disco_info_extras(Lang, StateData, false),
    {result, DiscoInfo#disco_info{xdata = [Extras]}};
process_iq_disco_info(From, #iq{type = get, lang = Lang,
				sub_els = [#disco_info{node = ?NS_COMMANDS}]},
		      StateData) ->
    case (StateData#state.config)#config.enable_hats andalso
        is_admin(From, StateData)
    of
        true ->
            {result,
             #disco_info{
                identities = [#identity{category = <<"automation">>,
                                        type = <<"command-list">>,
                                        name = translate:translate(
                                                 Lang, ?T("Commands"))}]}};
        false ->
            Txt = ?T("Node not found"),
            {error, xmpp:err_item_not_found(Txt, Lang)}
    end;
process_iq_disco_info(From, #iq{type = get, lang = Lang,
				sub_els = [#disco_info{node = ?MUC_HAT_ADD_CMD}]},
		      StateData) ->
    case (StateData#state.config)#config.enable_hats andalso
        is_admin(From, StateData)
    of
        true ->
            {result,
             #disco_info{
                identities = [#identity{category = <<"automation">>,
                                        type = <<"command-node">>,
                                        name = translate:translate(
                                              Lang, ?T("Add a hat to a user"))}],
                features = [?NS_COMMANDS]}};
        false ->
            Txt = ?T("Node not found"),
            {error, xmpp:err_item_not_found(Txt, Lang)}
    end;
process_iq_disco_info(From, #iq{type = get, lang = Lang,
				sub_els = [#disco_info{node = ?MUC_HAT_REMOVE_CMD}]},
		      StateData) ->
    case (StateData#state.config)#config.enable_hats andalso
        is_admin(From, StateData)
    of
        true ->
            {result,
             #disco_info{
                identities = [#identity{category = <<"automation">>,
                                        type = <<"command-node">>,
                                        name = translate:translate(
                                              Lang, ?T("Remove a hat from a user"))}],
                features = [?NS_COMMANDS]}};
        false ->
            Txt = ?T("Node not found"),
            {error, xmpp:err_item_not_found(Txt, Lang)}
    end;
process_iq_disco_info(From, #iq{type = get, lang = Lang,
				sub_els = [#disco_info{node = ?MUC_HAT_LIST_CMD}]},
		      StateData) ->
    case (StateData#state.config)#config.enable_hats andalso
        is_admin(From, StateData)
    of
        true ->
            {result,
             #disco_info{
                identities = [#identity{category = <<"automation">>,
                                        type = <<"command-node">>,
                                        name = translate:translate(
                                              Lang, ?T("List users with hats"))}],
                features = [?NS_COMMANDS]}};
        false ->
            Txt = ?T("Node not found"),
            {error, xmpp:err_item_not_found(Txt, Lang)}
    end;
process_iq_disco_info(From, #iq{type = get, lang = Lang,
				sub_els = [#disco_info{node = Node}]},
		      StateData) ->
    try
	true = mod_caps:is_valid_node(Node),
	DiscoInfo = make_disco_info(From, StateData),
	Extras = iq_disco_info_extras(Lang, StateData, true),
	DiscoInfo1 = DiscoInfo#disco_info{xdata = [Extras]},
	Hash = mod_caps:compute_disco_hash(DiscoInfo1, sha),
	Node = <<(ejabberd_config:get_uri())/binary, $#, Hash/binary>>,
	{result, DiscoInfo1#disco_info{node = Node}}
    catch _:{badmatch, _} ->
	    Txt = ?T("Invalid node name"),
	    {error, xmpp:err_item_not_found(Txt, Lang)}
    end.

-spec iq_disco_info_extras(binary(), state(), boolean()) -> xdata().
iq_disco_info_extras(Lang, StateData, Static) ->
    Config = StateData#state.config,
    AllowPM = case Config#config.allow_private_messages of
		  false -> none;
		  true ->
		      case Config#config.allow_private_messages_from_visitors of
			  nobody -> participants;
			  _ -> anyone
		      end
	      end,
    Fs1 = [{roomname, Config#config.title},
	   {description, Config#config.description},
	   {changesubject, Config#config.allow_change_subj},
	   {allowinvites, Config#config.allow_user_invites},
	   {allowpm, AllowPM},
	   {lang, Config#config.lang}],
    Fs2 = case Config#config.pubsub of
	      Node when is_binary(Node), Node /= <<"">> ->
		  [{pubsub, Node}|Fs1];
	      _ ->
		  Fs1
	  end,
    Fs3 = case Static of
	      false ->
		  [{occupants, maps:size(StateData#state.nicks)}|Fs2];
	      true ->
		  Fs2
	  end,
    Fs4 = case Config#config.logging of
	      true ->
		  case mod_muc_log:get_url(StateData) of
		      {ok, URL} ->
			  [{logs, URL}|Fs3];
		      error ->
			  Fs3
		  end;
	      false ->
		  Fs3
	  end,
    #xdata{type = result,
	   fields = muc_roominfo:encode(Fs4, Lang)}.

-spec process_iq_disco_items(jid(), iq(), state()) ->
				    {error, stanza_error()} | {result, disco_items()}.
process_iq_disco_items(_From, #iq{type = set, lang = Lang}, _StateData) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    {error, xmpp:err_not_allowed(Txt, Lang)};
process_iq_disco_items(From, #iq{type = get, sub_els = [#disco_items{node = <<>>}]},
		       StateData) ->
    case (StateData#state.config)#config.public_list of
      true ->
	  {result, get_mucroom_disco_items(StateData)};
      _ ->
	  case is_occupant_or_admin(From, StateData) of
	    true ->
		{result, get_mucroom_disco_items(StateData)};
	    _ ->
		%% If the list of occupants is private,
		%% the room MUST return an empty <query/> element
		%% (http://xmpp.org/extensions/xep-0045.html#disco-roomitems)
		{result, #disco_items{}}
	  end
    end;
process_iq_disco_items(From, #iq{type = get, lang = Lang,
                                 sub_els = [#disco_items{node = ?NS_COMMANDS}]},
		       StateData) ->
    case (StateData#state.config)#config.enable_hats andalso
        is_admin(From, StateData)
    of
        true ->
            {result,
             #disco_items{
                items = [#disco_item{jid = StateData#state.jid,
                                     node = ?MUC_HAT_ADD_CMD,
                                     name = translate:translate(
                                              Lang, ?T("Add a hat to a user"))},
                         #disco_item{jid = StateData#state.jid,
                                     node = ?MUC_HAT_REMOVE_CMD,
                                     name = translate:translate(
                                              Lang, ?T("Remove a hat from a user"))},
                         #disco_item{jid = StateData#state.jid,
                                     node = ?MUC_HAT_LIST_CMD,
                                     name = translate:translate(
                                              Lang, ?T("List users with hats"))}]}};
        false ->
            Txt = ?T("Node not found"),
            {error, xmpp:err_item_not_found(Txt, Lang)}
    end;
process_iq_disco_items(From, #iq{type = get, lang = Lang,
                                 sub_els = [#disco_items{node = Node}]},
		       StateData)
  when Node == ?MUC_HAT_ADD_CMD;
       Node == ?MUC_HAT_REMOVE_CMD;
       Node == ?MUC_HAT_LIST_CMD ->
    case (StateData#state.config)#config.enable_hats andalso
        is_admin(From, StateData)
    of
        true ->
            {result, #disco_items{}};
        false ->
            Txt = ?T("Node not found"),
            {error, xmpp:err_item_not_found(Txt, Lang)}
    end;
process_iq_disco_items(_From, #iq{lang = Lang}, _StateData) ->
    Txt = ?T("Node not found"),
    {error, xmpp:err_item_not_found(Txt, Lang)}.

-spec process_iq_captcha(jid(), iq(), state()) -> {error, stanza_error()} |
						  {result, undefined}.
process_iq_captcha(_From, #iq{type = get, lang = Lang}, _StateData) ->
    Txt = ?T("Value 'get' of 'type' attribute is not allowed"),
    {error, xmpp:err_not_allowed(Txt, Lang)};
process_iq_captcha(_From, #iq{type = set, lang = Lang, sub_els = [SubEl]},
		   _StateData) ->
    case ejabberd_captcha:process_reply(SubEl) of
      ok -> {result, undefined};
      {error, malformed} ->
	    Txt = ?T("Incorrect CAPTCHA submit"),
	    {error, xmpp:err_bad_request(Txt, Lang)};
      _ ->
	    Txt = ?T("The CAPTCHA verification has failed"),
	    {error, xmpp:err_not_allowed(Txt, Lang)}
    end.

-spec process_iq_vcard(jid(), iq(), state()) ->
			      {result, vcard_temp() | xmlel()} |
			      {result, undefined, state()} |
			      {error, stanza_error()}.
process_iq_vcard(_From, #iq{type = get}, StateData) ->
    #state{config = #config{vcard = VCardRaw}} = StateData,
    case fxml_stream:parse_element(VCardRaw) of
	#xmlel{} = VCard ->
	    {result, VCard};
	{error, _} ->
	    {error, xmpp:err_item_not_found()}
    end;
process_iq_vcard(From, #iq{type = set, lang = Lang, sub_els = [Pkt]},
		 StateData) ->
    case get_affiliation(From, StateData) of
	owner ->
	    SubEl = xmpp:encode(Pkt),
	    VCardRaw = fxml:element_to_binary(SubEl),
	    Hash = mod_vcard_xupdate:compute_hash(SubEl),
	    Config = StateData#state.config,
	    NewConfig = Config#config{vcard = VCardRaw, vcard_xupdate = Hash},
	    change_config(NewConfig, StateData);
	_ ->
	    ErrText = ?T("Owner privileges required"),
	    {error, xmpp:err_forbidden(ErrText, Lang)}
    end.

-spec process_iq_mucsub(jid(), iq(), state()) ->
      {error, stanza_error()} |
      {result, undefined | muc_subscribe() | muc_subscriptions(), stop | state()} |
      {ignore, state()}.
process_iq_mucsub(_From, #iq{type = set, lang = Lang,
			     sub_els = [#muc_subscribe{}]},
		  #state{just_created = Just, config = #config{allow_subscription = false}}) when Just /= true ->
    {error, xmpp:err_not_allowed(?T("Subscriptions are not allowed"), Lang)};
process_iq_mucsub(From,
		  #iq{type = set, lang = Lang,
		      sub_els = [#muc_subscribe{jid = #jid{} = SubJid} = Mucsub]},
		  StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    FRole = get_role(From, StateData),
    if FRole == moderator; FAffiliation == owner; FAffiliation == admin ->
	    process_iq_mucsub(SubJid,
			      #iq{type = set, lang = Lang,
				  sub_els = [Mucsub#muc_subscribe{jid = undefined}]},
			      StateData);
       true ->
	    Txt = ?T("Moderator privileges required"),
	    {error, xmpp:err_forbidden(Txt, Lang)}
    end;
process_iq_mucsub(From,
		  #iq{type = set, lang = Lang,
		      sub_els = [#muc_subscribe{nick = Nick}]} = Packet,
		  StateData) ->
    LBareJID = jid:tolower(jid:remove_resource(From)),
    try muc_subscribers_get(LBareJID, StateData#state.muc_subscribers) of
	#subscriber{nick = Nick1} when Nick1 /= Nick ->
	    Nodes = get_subscription_nodes(Packet),
	    case nick_collision(From, Nick, StateData) of
                true ->
		    ErrText = ?T("That nickname is already in use by another occupant"),
		    {error, xmpp:err_conflict(ErrText, Lang)};
                false ->
                    case mod_muc:can_use_nick(StateData#state.server_host,
                                              StateData#state.host,
                                              From, Nick) of
                        false ->
                            Err = case Nick of
                                      <<>> ->
                                          xmpp:err_jid_malformed(
                                            ?T("Nickname can't be empty"),
                                            Lang);
                                      _ ->
                                          xmpp:err_conflict(
                                            ?T("That nickname is registered"
                                               " by another person"), Lang)
                                  end,
                            {error, Err};
                        true ->
                            NewStateData =
                                set_subscriber(From, Nick, Nodes, StateData),
                            {result, subscribe_result(Packet), NewStateData}
                    end
            end;
	#subscriber{} ->
	    Nodes = get_subscription_nodes(Packet),
	    NewStateData = set_subscriber(From, Nick, Nodes, StateData),
	    {result, subscribe_result(Packet), NewStateData}
    catch _:{badkey, _} ->
	    SD2 = StateData#state{config = (StateData#state.config)#config{allow_subscription = true}},
	    add_new_user(From, Nick, Packet, SD2)
    end;
process_iq_mucsub(From, #iq{type = set, lang = Lang,
			    sub_els = [#muc_unsubscribe{jid = #jid{} = UnsubJid}]},
		  StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    FRole = get_role(From, StateData),
    if FRole == moderator; FAffiliation == owner; FAffiliation == admin ->
	    process_iq_mucsub(UnsubJid,
			      #iq{type = set, lang = Lang,
				  sub_els = [#muc_unsubscribe{jid = undefined}]},
			      StateData);
       true ->
	    Txt = ?T("Moderator privileges required"),
	    {error, xmpp:err_forbidden(Txt, Lang)}
    end;
process_iq_mucsub(From, #iq{type = set, sub_els = [#muc_unsubscribe{}]},
		  #state{room = Room, host = Host, server_host = ServerHost} = StateData) ->
    BareJID = jid:remove_resource(From),
    LBareJID = jid:tolower(BareJID),
    try muc_subscribers_remove_exn(LBareJID, StateData#state.muc_subscribers) of
	{MUCSubscribers, #subscriber{nick = Nick}} ->
	    NewStateData = StateData#state{muc_subscribers = MUCSubscribers},
	    store_room(NewStateData, [{del_subscription, LBareJID}]),
	    Packet1a = #message{
		sub_els = [#ps_event{
		    items = #ps_items{
			node = ?NS_MUCSUB_NODES_SUBSCRIBERS,
			items = [#ps_item{
			    id = p1_rand:get_string(),
			    sub_els = [#muc_subscribe{jid = BareJID, nick = Nick}]}]}}]},
	    Packet1b = #message{
		sub_els = [#ps_event{
		    items = #ps_items{
			node = ?NS_MUCSUB_NODES_SUBSCRIBERS,
			items = [#ps_item{
			    id = p1_rand:get_string(),
			    sub_els = [#muc_subscribe{nick = Nick}]}]}}]},
	    {Packet2a, Packet2b} = ejabberd_hooks:run_fold(muc_unsubscribed, ServerHost, {Packet1a, Packet1b},
							   [ServerHost, Room, Host, BareJID, StateData]),
	    send_subscriptions_change_notifications(Packet2a, Packet2b, StateData),
	    NewStateData2 = case close_room_if_temporary_and_empty(NewStateData) of
		{stop, normal, _} -> stop;
		{next_state, normal_state, SD} -> SD
	    end,
	    {result, undefined, NewStateData2}
	catch _:{badkey, _} ->
	    {result, undefined, StateData}
    end;
process_iq_mucsub(From, #iq{type = get, lang = Lang,
			    sub_els = [#muc_subscriptions{}]},
		  StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    FRole = get_role(From, StateData),
    IsModerator = FRole == moderator orelse FAffiliation == owner orelse
		  FAffiliation == admin,
    case IsModerator orelse is_subscriber(From, StateData) of
	true ->
	    ShowJid = IsModerator orelse
		      (StateData#state.config)#config.anonymous == false,
	    Subs = muc_subscribers_fold(
		     fun(_, #subscriber{jid = J, nick = N, nodes = Nodes}, Acc) ->
			 case ShowJid of
			     true ->
				 [#muc_subscription{jid = J, nick = N, events = Nodes}|Acc];
			     _ ->
				 [#muc_subscription{nick = N, events = Nodes}|Acc]
			 end
		     end, [], StateData#state.muc_subscribers),
	    {result, #muc_subscriptions{list = Subs}, StateData};
	_ ->
	    Txt = ?T("Moderator privileges required"),
	    {error, xmpp:err_forbidden(Txt, Lang)}
    end;
process_iq_mucsub(_From, #iq{type = get, lang = Lang}, _StateData) ->
    Txt = ?T("Value 'get' of 'type' attribute is not allowed"),
    {error, xmpp:err_bad_request(Txt, Lang)}.

-spec remove_subscriptions(state()) -> state().
remove_subscriptions(StateData) ->
    if not (StateData#state.config)#config.allow_subscription ->
	    StateData#state{muc_subscribers = muc_subscribers_new()};
       true ->
	    StateData
    end.

-spec get_subscription_nodes(stanza()) -> [binary()].
get_subscription_nodes(#iq{sub_els = [#muc_subscribe{events = Nodes}]}) ->
    lists:filter(
      fun(Node) ->
	      lists:member(Node, [?NS_MUCSUB_NODES_PRESENCE,
				  ?NS_MUCSUB_NODES_MESSAGES,
				  ?NS_MUCSUB_NODES_AFFILIATIONS,
				  ?NS_MUCSUB_NODES_SUBJECT,
				  ?NS_MUCSUB_NODES_CONFIG,
				  ?NS_MUCSUB_NODES_PARTICIPANTS,
				  ?NS_MUCSUB_NODES_SUBSCRIBERS])
      end, Nodes);
get_subscription_nodes(_) ->
    [].

-spec subscribe_result(iq()) -> muc_subscribe().
subscribe_result(#iq{sub_els = [#muc_subscribe{nick = Nick}]} = Packet) ->
    #muc_subscribe{nick = Nick, events = get_subscription_nodes(Packet)}.

-spec get_title(state()) -> binary().
get_title(StateData) ->
    case (StateData#state.config)#config.title of
      <<"">> -> StateData#state.room;
      Name -> Name
    end.

-spec get_roomdesc_reply(jid(), state(), binary()) -> {item, binary()} | false.
get_roomdesc_reply(JID, StateData, Tail) ->
    IsOccupantOrAdmin = is_occupant_or_admin(JID,
					     StateData),
    if (StateData#state.config)#config.public or
	 IsOccupantOrAdmin ->
	   if (StateData#state.config)#config.public_list or
		IsOccupantOrAdmin ->
		  {item, <<(get_title(StateData))/binary,Tail/binary>>};
	      true -> {item, get_title(StateData)}
	   end;
       true -> false
    end.

-spec get_roomdesc_tail(state(), binary()) -> binary().
get_roomdesc_tail(StateData, Lang) ->
    Desc = case (StateData#state.config)#config.public of
	     true -> <<"">>;
	     _ -> translate:translate(Lang, ?T("private, "))
	   end,
    Len = maps:size(StateData#state.nicks),
    <<" (", Desc/binary, (integer_to_binary(Len))/binary, ")">>.

-spec get_mucroom_disco_items(state()) -> disco_items().
get_mucroom_disco_items(StateData) ->
    Items = maps:fold(
	       fun(Nick, _, Acc) ->
		       [#disco_item{jid = jid:make(StateData#state.room,
						   StateData#state.host,
						   Nick),
				    name = Nick}|Acc]
	       end, [], StateData#state.nicks),
    #disco_items{items = Items}.

-spec process_iq_adhoc(jid(), iq(), state()) ->
			      {result, adhoc_command()} |
			      {result, adhoc_command(), state()} |
			      {error, stanza_error()}.
process_iq_adhoc(_From, #iq{type = get}, _StateData) ->
    {error, xmpp:err_bad_request()};
process_iq_adhoc(From, #iq{type = set, lang = Lang1,
                           sub_els = [#adhoc_command{} = Request]},
		 StateData) ->
    % Ad-Hoc Commands are used only for Hats here
    case (StateData#state.config)#config.enable_hats andalso
        is_admin(From, StateData)
    of
        true ->
            #adhoc_command{lang = Lang2, node = Node,
                           action = Action, xdata = XData} = Request,
            Lang = case Lang2 of
                       <<"">> -> Lang1;
                       _ -> Lang2
                   end,
            case {Node, Action} of
                {_, cancel} ->
                    {result,
                     xmpp_util:make_adhoc_response(
                       Request,
                       #adhoc_command{status = canceled, lang = Lang,
                                      node = Node})};
                {?MUC_HAT_ADD_CMD, execute} ->
                    Form =
                        #xdata{
                           title = translate:translate(
                                     Lang, ?T("Add a hat to a user")),
                           type = form,
                           fields =
                               [#xdata_field{
                                   type = 'jid-single',
                                   label = translate:translate(Lang, ?T("Jabber ID")),
                                   required = true,
                                   var = <<"jid">>},
                                #xdata_field{
                                   type = 'text-single',
                                   label = translate:translate(Lang, ?T("Hat title")),
                                   var = <<"hat_title">>},
                                #xdata_field{
                                   type = 'text-single',
                                   label = translate:translate(Lang, ?T("Hat URI")),
                                   required = true,
                                   var = <<"hat_uri">>}
                               ]},
                    {result,
                     xmpp_util:make_adhoc_response(
                       Request,
                       #adhoc_command{
                          status = executing,
                          xdata = Form})};
                {?MUC_HAT_ADD_CMD, complete} when XData /= undefined ->
                    JID = try
                              jid:decode(hd(xmpp_util:get_xdata_values(
                                              <<"jid">>, XData)))
                          catch _:_ -> error
                          end,
                    URI = try
                              hd(xmpp_util:get_xdata_values(
                                   <<"hat_uri">>, XData))
                          catch _:_ -> error
                          end,
                    Title = case xmpp_util:get_xdata_values(
                                   <<"hat_title">>, XData) of
                                [] -> <<"">>;
                                [T] -> T
                            end,
                    if
                        (JID /= error) and (URI /= error) ->
                            case add_hat(JID, URI, Title, StateData) of
                                {ok, NewStateData} ->
                                    store_room(NewStateData),
                                    send_update_presence(
                                      JID, NewStateData, StateData),
                                    {result,
                                     xmpp_util:make_adhoc_response(
                                       Request,
                                       #adhoc_command{status = completed}),
                                     NewStateData};
                                {error, size_limit} ->
                                    Txt = ?T("Hats limit exceeded"),
                                    {error, xmpp:err_not_allowed(Txt, Lang)}
                            end;
                        true ->
                            {error, xmpp:err_bad_request()}
                    end;
                {?MUC_HAT_ADD_CMD, complete} ->
                    {error, xmpp:err_bad_request()};
                {?MUC_HAT_ADD_CMD, _} ->
                    Txt = ?T("Incorrect value of 'action' attribute"),
                    {error, xmpp:err_bad_request(Txt, Lang)};
                {?MUC_HAT_REMOVE_CMD, execute} ->
                    Form =
                        #xdata{
                           title = translate:translate(
                                     Lang, ?T("Remove a hat from a user")),
                           type = form,
                           fields =
                               [#xdata_field{
                                   type = 'jid-single',
                                   label = translate:translate(Lang, ?T("Jabber ID")),
                                   required = true,
                                   var = <<"jid">>},
                                #xdata_field{
                                   type = 'text-single',
                                   label = translate:translate(Lang, ?T("Hat URI")),
                                   required = true,
                                   var = <<"hat_uri">>}
                               ]},
                    {result,
                     xmpp_util:make_adhoc_response(
                       Request,
                       #adhoc_command{
                          status = executing,
                          xdata = Form})};
                {?MUC_HAT_REMOVE_CMD, complete} when XData /= undefined ->
                    JID = try
                              jid:decode(hd(xmpp_util:get_xdata_values(
                                              <<"jid">>, XData)))
                          catch _:_ -> error
                          end,
                    URI = try
                              hd(xmpp_util:get_xdata_values(
                                   <<"hat_uri">>, XData))
                          catch _:_ -> error
                          end,
                    if
                        (JID /= error) and (URI /= error) ->
                            NewStateData = del_hat(JID, URI, StateData),
                            store_room(NewStateData),
                            send_update_presence(
                              JID, NewStateData, StateData),
                            {result,
                             xmpp_util:make_adhoc_response(
                               Request,
                               #adhoc_command{status = completed}),
                             NewStateData};
                        true ->
                            {error, xmpp:err_bad_request()}
                    end;
                {?MUC_HAT_REMOVE_CMD, complete} ->
                    {error, xmpp:err_bad_request()};
                {?MUC_HAT_REMOVE_CMD, _} ->
                    Txt = ?T("Incorrect value of 'action' attribute"),
                    {error, xmpp:err_bad_request(Txt, Lang)};
                {?MUC_HAT_LIST_CMD, execute} ->
                    Hats = get_all_hats(StateData),
                    Items =
                        lists:map(
                          fun({JID, URI, Title}) ->
                                  [#xdata_field{
                                      var = <<"jid">>,
                                      values = [jid:encode(JID)]},
                                   #xdata_field{
                                      var = <<"hat_title">>,
                                      values = [URI]},
                                   #xdata_field{
                                      var = <<"hat_uri">>,
                                      values = [Title]}]
                          end, Hats),
                    Form =
                        #xdata{
                           title = translate:translate(
                                     Lang, ?T("List of users with hats")),
                           type = result,
                           reported =
                               [#xdata_field{
                                   label = translate:translate(Lang, ?T("Jabber ID")),
                                   var = <<"jid">>},
                                #xdata_field{
                                   label = translate:translate(Lang, ?T("Hat title")),
                                   var = <<"hat_title">>},
                                #xdata_field{
                                   label = translate:translate(Lang, ?T("Hat URI")),
                                   var = <<"hat_uri">>}],
                           items = Items},
                    {result,
                     xmpp_util:make_adhoc_response(
                       Request,
                       #adhoc_command{
                          status = completed,
                          xdata = Form})};
                {?MUC_HAT_LIST_CMD, _} ->
                    Txt = ?T("Incorrect value of 'action' attribute"),
                    {error, xmpp:err_bad_request(Txt, Lang)};
                _ ->
                    {error, xmpp:err_item_not_found()}
            end;
	_ ->
	    {error, xmpp:err_forbidden()}
    end.

-spec add_hat(jid(), binary(), binary(), state()) ->
                     {ok, state()} | {error, size_limit}.
add_hat(JID, URI, Title, StateData) ->
    Hats = StateData#state.hats_users,
    LJID = jid:remove_resource(jid:tolower(JID)),
    UserHats = maps:get(LJID, Hats, #{}),
    UserHats2 = maps:put(URI, Title, UserHats),
    USize = maps:size(UserHats2),
    if
        USize =< ?MAX_HATS_PER_USER ->
            Hats2 = maps:put(LJID, UserHats2, Hats),
            Size = maps:size(Hats2),
            if
                Size =< ?MAX_HATS_USERS ->
                    {ok, StateData#state{hats_users = Hats2}};
                true ->
                    {error, size_limit}
            end;
        true ->
            {error, size_limit}
    end.

-spec del_hat(jid(), binary(), state()) -> state().
del_hat(JID, URI, StateData) ->
    Hats = StateData#state.hats_users,
    LJID = jid:remove_resource(jid:tolower(JID)),
    UserHats = maps:get(LJID, Hats, #{}),
    UserHats2 = maps:remove(URI, UserHats),
    Hats2 =
        case maps:size(UserHats2) of
            0 ->
                maps:remove(LJID, Hats);
            _ ->
                maps:put(LJID, UserHats2, Hats)
        end,
    StateData#state{hats_users = Hats2}.

-spec get_all_hats(state()) -> list({jid(), binary(), binary()}).
get_all_hats(StateData) ->
    lists:flatmap(
      fun({LJID, H}) ->
              JID = jid:make(LJID),
              lists:map(fun({URI, Title}) -> {JID, URI, Title} end,
                        maps:to_list(H))
      end,
      maps:to_list(StateData#state.hats_users)).

-spec add_presence_hats(jid(), #presence{}, state()) -> #presence{}.
add_presence_hats(JID, Pres, StateData) ->
    case (StateData#state.config)#config.enable_hats of
        true ->
            Hats = StateData#state.hats_users,
            LJID = jid:remove_resource(jid:tolower(JID)),
            UserHats = maps:get(LJID, Hats, #{}),
            case maps:size(UserHats) of
                0 -> Pres;
                _ ->
                    Items =
                        lists:map(fun({URI, Title}) ->
                                          #muc_hat{uri = URI, title = Title}
                                  end,
                                  maps:to_list(UserHats)),
                    xmpp:set_subtag(Pres,
                                    #muc_hats{hats = Items})
            end;
        false ->
            Pres
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Voice request support

-spec prepare_request_form(jid(), binary(), binary()) -> message().
prepare_request_form(Requester, Nick, Lang) ->
    Title = translate:translate(Lang, ?T("Voice request")),
    Instruction = translate:translate(
		    Lang, ?T("Either approve or decline the voice request.")),
    Fs = muc_request:encode([{role, participant},
			     {jid, Requester},
			     {roomnick, Nick},
			     {request_allow, false}],
			    Lang),
    #message{type = normal,
	     sub_els = [#xdata{type = form,
			       title = Title,
			       instructions = [Instruction],
			       fields = Fs}]}.

-spec send_voice_request(jid(), binary(), state()) -> ok.
send_voice_request(From, Lang, StateData) ->
    Moderators = search_role(moderator, StateData),
    FromNick = find_nick_by_jid(From, StateData),
    lists:foreach(
      fun({_, User}) ->
	      ejabberd_router:route(
		xmpp:set_from_to(
		  prepare_request_form(From, FromNick, Lang),
		  StateData#state.jid, User#user.jid))
      end, Moderators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Invitation support
-spec check_invitation(jid(), [muc_invite()], binary(), state()) ->
			      ok | {error, stanza_error()}.
check_invitation(From, Invitations, Lang, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    CanInvite = (StateData#state.config)#config.allow_user_invites orelse
	        FAffiliation == admin orelse FAffiliation == owner,
    case CanInvite of
	true ->
	    case lists:all(
		   fun(#muc_invite{to = #jid{}}) -> true;
		      (_) -> false
		   end, Invitations) of
		true ->
		    ok;
		false ->
		    Txt = ?T("No 'to' attribute found in the invitation"),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end;
	false ->
	    Txt = ?T("Invitations are not allowed in this conference"),
	    {error, xmpp:err_not_allowed(Txt, Lang)}
    end.

-spec route_invitation(jid(), message(), muc_invite(), binary(), state()) -> jid().
route_invitation(From, Pkt, Invitation, Lang, StateData) ->
    #muc_invite{to = JID, reason = Reason} = Invitation,
    Invite = Invitation#muc_invite{to = undefined, from = From},
    Password = case (StateData#state.config)#config.password_protected of
		   true ->
		       (StateData#state.config)#config.password;
		   false ->
		       undefined
	       end,
    XUser = #muc_user{password = Password, invites = [Invite]},
    XConference = #x_conference{jid = jid:make(StateData#state.room,
					       StateData#state.host),
				reason = Reason},
    Body = iolist_to_binary(
	     [io_lib:format(
		translate:translate(
		  Lang,
		  ?T("~s invites you to the room ~s")),
		[jid:encode(From),
		 jid:encode({StateData#state.room, StateData#state.host, <<"">>})]),
	      case (StateData#state.config)#config.password_protected of
		  true ->
		      <<", ",
			(translate:translate(
			   Lang, ?T("the password is")))/binary,
			" '",
			((StateData#state.config)#config.password)/binary,
			"'">>;
		  _ -> <<"">>
	      end,
	      case Reason of
		  <<"">> -> <<"">>;
		  _ -> <<" (", Reason/binary, ") ">>
	      end]),
    Msg = #message{from = StateData#state.jid,
		   to = JID,
		   type = normal,
		   body = xmpp:mk_text(Body),
		   sub_els = [XUser, XConference]},
    Msg2 = ejabberd_hooks:run_fold(muc_invite,
				   StateData#state.server_host,
				   Msg,
				   [StateData#state.jid, StateData#state.config,
				    From, JID, Reason, Pkt]),
    ejabberd_router:route(Msg2),
    JID.

%% Handle a message sent to the room by a non-participant.
%% If it is a decline, send to the inviter.
%% Otherwise, an error message is sent to the sender.
-spec handle_roommessage_from_nonparticipant(message(), state(), jid()) -> ok.
handle_roommessage_from_nonparticipant(Packet, StateData, From) ->
    try xmpp:try_subtag(Packet, #muc_user{}) of
	#muc_user{decline = #muc_decline{to = #jid{} = To} = Decline} = XUser ->
	    NewDecline = Decline#muc_decline{to = undefined, from = From},
	    NewXUser = XUser#muc_user{decline = NewDecline},
	    NewPacket = xmpp:set_subtag(Packet, NewXUser),
	    ejabberd_router:route(
	      xmpp:set_from_to(NewPacket, StateData#state.jid, To));
	_ ->
	    ErrText = ?T("Only occupants are allowed to send messages "
			 "to the conference"),
	    Err = xmpp:err_not_acceptable(ErrText, xmpp:get_lang(Packet)),
	    ejabberd_router:route_error(Packet, Err)
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:io_format_error(Why),
	    Err = xmpp:err_bad_request(Txt, xmpp:get_lang(Packet)),
	    ejabberd_router:route_error(Packet, Err)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging

add_to_log(Type, Data, StateData)
    when Type == roomconfig_change_disabledlogging ->
    mod_muc_log:add_to_log(StateData#state.server_host,
			   roomconfig_change, Data, StateData#state.jid,
			   make_opts(StateData, false));
add_to_log(Type, Data, StateData) ->
    case (StateData#state.config)#config.logging of
      true ->
	  mod_muc_log:add_to_log(StateData#state.server_host,
				 Type, Data, StateData#state.jid,
				 make_opts(StateData, false));
      false -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Users number checking

-spec tab_add_online_user(jid(), state()) -> any().
tab_add_online_user(JID, StateData) ->
    Room = StateData#state.room,
    Host = StateData#state.host,
    ServerHost = StateData#state.server_host,
    ejabberd_hooks:run(join_room, ServerHost, [ServerHost, Room, Host, JID]),
    mod_muc:register_online_user(ServerHost, jid:tolower(JID), Room, Host).

-spec tab_remove_online_user(jid(), state()) -> any().
tab_remove_online_user(JID, StateData) ->
    Room = StateData#state.room,
    Host = StateData#state.host,
    ServerHost = StateData#state.server_host,
    ejabberd_hooks:run(leave_room, ServerHost, [ServerHost, Room, Host, JID]),
    mod_muc:unregister_online_user(ServerHost, jid:tolower(JID), Room, Host).

-spec tab_count_user(jid(), state()) -> non_neg_integer().
tab_count_user(JID, StateData) ->
    ServerHost = StateData#state.server_host,
    {LUser, LServer, _} = jid:tolower(JID),
    mod_muc:count_online_rooms_by_user(ServerHost, LUser, LServer).

-spec element_size(stanza()) -> non_neg_integer().
element_size(El) ->
    byte_size(fxml:element_to_binary(xmpp:encode(El, ?NS_CLIENT))).

-spec store_room(state()) -> ok.
store_room(StateData) ->
    store_room(StateData, []).
store_room(StateData, ChangesHints) ->
    % Let store persistent rooms or on those backends that have get_subscribed_rooms
    Mod = gen_mod:db_mod(StateData#state.server_host, mod_muc),
    HasGSR = erlang:function_exported(Mod, get_subscribed_rooms, 3),
    case HasGSR of
	true ->
	    ok;
	_ ->
	    erlang:put(muc_subscribers, StateData#state.muc_subscribers#muc_subscribers.subscribers)
    end,
    ShouldStore = case (StateData#state.config)#config.persistent of
		      true ->
			  true;
		      _ ->
			  case ChangesHints of
			      [] ->
				  false;
			      _ ->
				  HasGSR
			  end
		  end,
    if ShouldStore ->
            case erlang:function_exported(Mod, store_changes, 4) of
                true when ChangesHints /= [] ->
                    mod_muc:store_changes(
                      StateData#state.server_host,
                      StateData#state.host, StateData#state.room,
                      ChangesHints);
                _ ->
                    store_room_no_checks(StateData, ChangesHints, false),
		    ok
            end;
       true ->
	    ok
    end.

-spec store_room_no_checks(state(), list(), boolean()) -> {atomic, any()}.
store_room_no_checks(StateData, ChangesHints, Hibernation) ->
    mod_muc:store_room(StateData#state.server_host,
		       StateData#state.host, StateData#state.room,
		       make_opts(StateData, Hibernation),
		       ChangesHints).

-spec send_subscriptions_change_notifications(stanza(), stanza(), state()) -> ok.
send_subscriptions_change_notifications(Packet, PacketWithoutJid, State) ->
    {WJ, WN} =
        maps:fold(
          fun(_, #subscriber{jid = JID}, {WithJid, WithNick}) ->
                  case (State#state.config)#config.anonymous == false orelse
                      get_role(JID, State) == moderator orelse
                      get_default_role(get_affiliation(JID, State), State) == moderator of
                      true ->
                          {[JID | WithJid], WithNick};
                      _ ->
                          {WithJid, [JID | WithNick]}
                  end
          end, {[], []},
          muc_subscribers_get_by_node(?NS_MUCSUB_NODES_SUBSCRIBERS,
                                      State#state.muc_subscribers)),
    if WJ /= [] ->
	ejabberd_router_multicast:route_multicast(State#state.jid, State#state.server_host,
						  WJ, Packet, false);
	true -> ok
    end,
    if WN /= [] ->
	ejabberd_router_multicast:route_multicast(State#state.jid, State#state.server_host,
						  WN, PacketWithoutJid, false);
	true -> ok
    end.

-spec send_wrapped(jid(), jid(), stanza(), binary(), state()) -> ok.
send_wrapped(From, To, Packet, Node, State) ->
    LTo = jid:tolower(To),
    LBareTo = jid:tolower(jid:remove_resource(To)),
    IsOffline = case maps:get(LTo, State#state.users, error) of
		    #user{last_presence = undefined} -> true;
		    error -> true;
		    _ -> false
		end,
    if IsOffline ->
	    try muc_subscribers_get(LBareTo, State#state.muc_subscribers) of
		#subscriber{nodes = Nodes, jid = JID} ->
		    case lists:member(Node, Nodes) of
			true ->
			    MamEnabled = (State#state.config)#config.mam,
			    Id = case xmpp:get_subtag(Packet, #stanza_id{by = #jid{}}) of
				     #stanza_id{id = Id2} ->
					 Id2;
				     _ ->
					 p1_rand:get_string()
				 end,
			    NewPacket = wrap(From, JID, Packet, Node, Id),
			    NewPacket2 = xmpp:put_meta(NewPacket, in_muc_mam, MamEnabled),
			    ejabberd_router:route(
			      xmpp:set_from_to(NewPacket2, State#state.jid, JID));
			false ->
			    ok
		    end
	    catch _:{badkey, _} ->
		    ok
	    end;
       true ->
	    case Packet of
		#presence{type = unavailable} ->
		    case xmpp:get_subtag(Packet, #muc_user{}) of
			#muc_user{destroy = Destroy,
				  status_codes = Codes} ->
			    case Destroy /= undefined orelse
				 (lists:member(110,Codes) andalso
				  not lists:member(303, Codes)) of
				true ->
				    ejabberd_router:route(
				      #presence{from = State#state.jid, to = To,
						id = p1_rand:get_string(),
						type = unavailable});
				false ->
				    ok
			    end;
			_ ->
			    false
		    end;
		_ ->
		    ok
	    end,
	    ejabberd_router:route(xmpp:set_from_to(Packet, From, To))
    end.

-spec wrap(jid(), undefined | jid(), stanza(), binary(), binary()) -> message().
wrap(From, To, Packet, Node, Id) ->
    El = xmpp:set_from_to(Packet, From, To),
    #message{
	id = Id,
	sub_els = [#ps_event{
	    items = #ps_items{
		node = Node,
		items = [#ps_item{
		    id = Id,
		    sub_els = [El]}]}}]}.

-spec send_wrapped_multiple(jid(), users(), stanza(), binary(), state()) -> ok.
send_wrapped_multiple(From, Users, Packet, Node, State) ->
    {Dir, Wra} =
    maps:fold(
	fun(_, #user{jid = To, last_presence = LP}, {Direct, Wrapped} = Res) ->
	    IsOffline = LP == undefined,
	    if IsOffline ->
		LBareTo = jid:tolower(jid:remove_resource(To)),
		case muc_subscribers_find(LBareTo, State#state.muc_subscribers) of
		    {ok, #subscriber{nodes = Nodes}} ->
			case lists:member(Node, Nodes) of
			    true ->
				{Direct, [To | Wrapped]};
			    _ ->
                                %% TODO: check that this branch is never called
				Res
			end;
		    _ ->
			Res
		end;
		true ->
		    {[To | Direct], Wrapped}
	    end
	end, {[],[]}, Users),
    case Dir of
	[] -> ok;
	_ ->
	    case Packet of
		#presence{type = unavailable} ->
		    case xmpp:get_subtag(Packet, #muc_user{}) of
			#muc_user{destroy = Destroy,
				  status_codes = Codes} ->
			    case Destroy /= undefined orelse
				 (lists:member(110,Codes) andalso
				  not lists:member(303, Codes)) of
				true ->
				    ejabberd_router_multicast:route_multicast(
					From, State#state.server_host, Dir,
					#presence{id = p1_rand:get_string(),
						  type = unavailable}, false);
				false ->
				    ok
			    end;
			_ ->
			    false
		    end;
		_ ->
		    ok
	    end,
	    ejabberd_router_multicast:route_multicast(From, State#state.server_host,
						      Dir, Packet, false)
    end,
    case Wra of
	[] -> ok;
	_ ->
	    MamEnabled = (State#state.config)#config.mam,
	    Id = case xmpp:get_subtag(Packet, #stanza_id{by = #jid{}}) of
		     #stanza_id{id = Id2} ->
			 Id2;
		     _ ->
			 p1_rand:get_string()
		 end,
	    NewPacket = wrap(From, undefined, Packet, Node, Id),
	    NewPacket2 = xmpp:put_meta(NewPacket, in_muc_mam, MamEnabled),
	    ejabberd_router_multicast:route_multicast(State#state.jid, State#state.server_host,
						      Wra, NewPacket2, true)
    end.

%%%----------------------------------------------------------------------
%%% #muc_subscribers API
%%%----------------------------------------------------------------------

-spec muc_subscribers_new() -> #muc_subscribers{}.
muc_subscribers_new() ->
    #muc_subscribers{}.

-spec muc_subscribers_get(ljid(), #muc_subscribers{}) -> #subscriber{}.
muc_subscribers_get({_, _, _} = LJID, MUCSubscribers) ->
    maps:get(LJID, MUCSubscribers#muc_subscribers.subscribers).

-spec muc_subscribers_find(ljid(), #muc_subscribers{}) ->
                                  {ok, #subscriber{}} | error.
muc_subscribers_find({_, _, _} = LJID, MUCSubscribers) ->
    maps:find(LJID, MUCSubscribers#muc_subscribers.subscribers).

-spec muc_subscribers_is_key(ljid(), #muc_subscribers{}) -> boolean().
muc_subscribers_is_key({_, _, _} = LJID, MUCSubscribers) ->
    maps:is_key(LJID, MUCSubscribers#muc_subscribers.subscribers).

-spec muc_subscribers_size(#muc_subscribers{}) -> integer().
muc_subscribers_size(MUCSubscribers) ->
    maps:size(MUCSubscribers#muc_subscribers.subscribers).

-spec muc_subscribers_fold(Fun, Acc, #muc_subscribers{}) -> Acc when
    Fun :: fun((ljid(), #subscriber{}, Acc) -> Acc).
muc_subscribers_fold(Fun, Init, MUCSubscribers) ->
    maps:fold(Fun, Init, MUCSubscribers#muc_subscribers.subscribers).

-spec muc_subscribers_get_by_nick(binary(), #muc_subscribers{}) -> [#subscriber{}].
muc_subscribers_get_by_nick(Nick, MUCSubscribers) ->
    maps:get(Nick, MUCSubscribers#muc_subscribers.subscriber_nicks, []).

-spec muc_subscribers_get_by_node(binary(), #muc_subscribers{}) -> subscribers().
muc_subscribers_get_by_node(Node, MUCSubscribers) ->
    maps:get(Node, MUCSubscribers#muc_subscribers.subscriber_nodes, #{}).

-spec muc_subscribers_remove_exn(ljid(), #muc_subscribers{}) ->
                                        {#muc_subscribers{}, #subscriber{}}.
muc_subscribers_remove_exn({_, _, _} = LJID, MUCSubscribers) ->
    #muc_subscribers{subscribers = Subs,
                     subscriber_nicks = SubNicks,
                     subscriber_nodes = SubNodes} = MUCSubscribers,
    Subscriber = maps:get(LJID, Subs),
    #subscriber{nick = Nick, nodes = Nodes} = Subscriber,
    NewSubNicks = maps:remove(Nick, SubNicks),
    NewSubs = maps:remove(LJID, Subs),
    NewSubNodes =
        lists:foldl(
          fun(Node, Acc) ->
                  NodeSubs = maps:get(Node, Acc, #{}),
                  NodeSubs2 = maps:remove(LJID, NodeSubs),
                  maps:put(Node, NodeSubs2, Acc)
          end, SubNodes, Nodes),
    {#muc_subscribers{subscribers = NewSubs,
                      subscriber_nicks = NewSubNicks,
                      subscriber_nodes = NewSubNodes}, Subscriber}.

-spec muc_subscribers_put(#subscriber{}, #muc_subscribers{}) ->
                                 #muc_subscribers{}.
muc_subscribers_put(Subscriber, MUCSubscribers) ->
    #subscriber{jid = JID,
                nick = Nick,
                nodes = Nodes} = Subscriber,
    #muc_subscribers{subscribers = Subs,
                     subscriber_nicks = SubNicks,
                     subscriber_nodes = SubNodes} = MUCSubscribers,
    LJID = jid:tolower(JID),
    NewSubs = maps:put(LJID, Subscriber, Subs),
    NewSubNicks = maps:put(Nick, [LJID], SubNicks),
    NewSubNodes =
        lists:foldl(
          fun(Node, Acc) ->
                  NodeSubs = maps:get(Node, Acc, #{}),
                  NodeSubs2 = maps:put(LJID, Subscriber, NodeSubs),
                  maps:put(Node, NodeSubs2, Acc)
          end, SubNodes, Nodes),
    #muc_subscribers{subscribers = NewSubs,
                     subscriber_nicks = NewSubNicks,
                     subscriber_nodes = NewSubNodes}.


cleanup_affiliations(State) ->
    case mod_muc_opt:cleanup_affiliations_on_start(State#state.server_host) of
        true ->
            Affiliations =
                maps:filter(
                  fun({LUser, LServer, _}, _) ->
                          case ejabberd_router:is_my_host(LServer) of
                              true ->
                                  ejabberd_auth:user_exists(LUser, LServer);
                              false ->
                                  true
                          end
                  end, State#state.affiliations),
            State#state{affiliations = Affiliations};
        false ->
            State
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Detect messange stanzas that don't have meaningful content
-spec has_body_or_subject(message()) -> boolean().
has_body_or_subject(#message{body = Body, subject = Subj}) ->
    Body /= [] orelse Subj /= [].

-spec reset_hibernate_timer(state()) -> state().
reset_hibernate_timer(State) ->
    case State#state.hibernate_timer of
	hibernating ->
	    ok;
	_ ->
	    disable_hibernate_timer(State),
	    NewTimer = case {mod_muc_opt:hibernation_timeout(State#state.server_host),
			     maps:size(State#state.users)} of
			   {infinity, _} ->
			       none;
			   {Timeout, 0} ->
			       p1_fsm:send_event_after(Timeout, hibernate);
			   _ ->
			       none
		       end,
	    State#state{hibernate_timer = NewTimer}
    end.


-spec disable_hibernate_timer(state()) -> ok.
disable_hibernate_timer(State) ->
    case State#state.hibernate_timer of
	Ref when is_reference(Ref) ->
	    p1_fsm:cancel_timer(Ref),
	    ok;
	_ ->
	    ok
    end.
