%%%----------------------------------------------------------------------
%%% File    : mod_muc_room.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC room stuff
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_room).
-author('alexey@process-one.net').

-behaviour(gen_fsm).


%% External exports
-export([start_link/9,
	 start_link/7,
	 start/9,
	 start/7,
	 route/4]).

%% gen_fsm callbacks
-export([init/1,
	 normal_state/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(MAX_USERS_DEFAULT, 200).
-define(MAX_USERS_DEFAULT_LIST,
	[5, 10, 20, 30, 50, 100, 200, 500, 1000, 2000, 5000]).

-define(SETS, gb_sets).
-define(DICT, dict).

-record(lqueue, {queue, len, max}).

-record(config, {title = "",
		 description = "",
		 allow_change_subj = true,
		 allow_query_users = true,
		 allow_private_messages = true,
		 public = true,
		 public_list = true,
		 persistent = false,
		 moderated = true,
		 members_by_default = true,
		 members_only = false,
		 allow_user_invites = false,
		 password_protected = false,
		 password = "",
		 anonymous = true,
		 max_users = ?MAX_USERS_DEFAULT,
		 logging = false
		}).

-record(user, {jid,
	       nick,
	       role,
	       last_presence}).

-record(activity, {message_time = 0,
		   presence_time = 0,
		   message_shaper,
		   presence_shaper,
		   message,
		   presence}).

-record(state, {room,
		host,
		server_host,
		access,
		jid,
		config = #config{},
		users = ?DICT:new(),
		affiliations = ?DICT:new(),
		history = lqueue_new(20),
		subject = "",
		subject_author = "",
		just_created = false,
		activity = ?DICT:new(),
		room_shaper,
		room_queue = queue:new()}).

-record(muc_online_users, {us,
			   room,
			   host}).


%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, 
	gen_fsm:start(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				RoomShaper, Creator, Nick, DefRoomOpts],
		      ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, 
	Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
	supervisor:start_child(
	  Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,
		       Creator, Nick, DefRoomOpts])).
-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
      Creator, Nick, DefRoomOpts) ->
    ?SUPERVISOR_START.

start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    supervisor:start_child(
      Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,
		   Opts]).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
	   Creator, Nick, DefRoomOpts) ->
    gen_fsm:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				 RoomShaper, Creator, Nick, DefRoomOpts],
		       ?FSMOPTS).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    gen_fsm:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				 RoomShaper, Opts],
		       ?FSMOPTS).

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
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Creator, _Nick, DefRoomOpts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_affiliation(Creator, owner,
			    #state{host = Host,
				   server_host = ServerHost,
				   access = Access,
				   room = Room,
				   history = lqueue_new(HistorySize),
				   jid = jlib:make_jid(Room, Host, ""),
				   just_created = true,
				   room_shaper = Shaper}),
    State1 = set_opts(DefRoomOpts, State),
    ?INFO_MSG("Created MUC room ~s@~s by ~s", 
	      [Room, Host, jlib:jid_to_string(Creator)]),
    {ok, normal_state, State1};
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_opts(Opts, #state{host = Host,
				  server_host = ServerHost,
				  access = Access,
				  room = Room,
				  history = lqueue_new(HistorySize),
				  jid = jlib:make_jid(Room, Host, ""),
				  room_shaper = Shaper}),
    {ok, normal_state, State}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
normal_state({route, From, "",
	      {xmlelement, "message", Attrs, Els} = Packet},
	     StateData) ->
    Lang = xml:get_attr_s("xml:lang", Attrs),
    case is_user_online(From, StateData) orelse
	is_user_allowed_message_nonparticipant(From, StateData) of
	true ->
	    case xml:get_attr_s("type", Attrs) of
		"groupchat" ->
		    Activity = get_user_activity(From, StateData),
		    Now = now_to_usec(now()),
		    MinMessageInterval =
			trunc(gen_mod:get_module_opt(
				StateData#state.server_host,
				mod_muc, min_message_interval, 0) * 1000000),
		    Size = lists:flatlength(xml:element_to_string(Packet)),
		    {MessageShaper, MessageShaperInterval} =
			shaper:update(Activity#activity.message_shaper, Size),
		    if
			Activity#activity.message /= undefined ->
			    ErrText = "Traffic rate limit is exceeded",
			    Err = jlib:make_error_reply(
				    Packet, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)),
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, Err),
			    {next_state, normal_state, StateData};
			Now >= Activity#activity.message_time + MinMessageInterval,
			MessageShaperInterval == 0 ->
			    {RoomShaper, RoomShaperInterval} =
				shaper:update(StateData#state.room_shaper, Size),
			    RoomQueueEmpty = queue:is_empty(
					       StateData#state.room_queue),
			    if
				RoomShaperInterval == 0,
				RoomQueueEmpty ->
				    NewActivity = Activity#activity{
						    message_time = Now,
						    message_shaper = MessageShaper},
				    StateData1 =
					StateData#state{
					  activity = ?DICT:store(
							jlib:jid_tolower(From),
							NewActivity,
							StateData#state.activity),
					  room_shaper = RoomShaper},
				    process_groupchat_message(From, Packet, StateData1);
				true ->
				    StateData1 =
					if
					    RoomQueueEmpty ->
						erlang:send_after(
						  RoomShaperInterval, self(),
						  process_room_queue),
						StateData#state{
						  room_shaper = RoomShaper};
					    true ->
						StateData
					end,
				    NewActivity = Activity#activity{
						    message_time = Now,
						    message_shaper = MessageShaper,
						    message = Packet},
				    RoomQueue = queue:in(
						  {message, From},
						  StateData#state.room_queue),
				    StateData2 =
					StateData1#state{
					  activity = ?DICT:store(
							jlib:jid_tolower(From),
							NewActivity,
							StateData#state.activity),
					  room_queue = RoomQueue},
				    {next_state, normal_state, StateData2}
			    end;
			true ->
			    MessageInterval =
				(Activity#activity.message_time +
				 MinMessageInterval - Now) div 1000,
			    Interval = lists:max([MessageInterval,
						  MessageShaperInterval]),
			    erlang:send_after(
			      Interval, self(), {process_user_message, From}),
			    NewActivity = Activity#activity{
					    message = Packet,
					    message_shaper = MessageShaper},
			    StateData1 =
				StateData#state{
				  activity = ?DICT:store(
						jlib:jid_tolower(From),
						NewActivity,
						StateData#state.activity)},
			    {next_state, normal_state, StateData1}
		    end;
		"error" ->
		    case is_user_online(From, StateData) of
			true ->
			    ErrorText = "This participant is kicked from the room because "
				"he sent an error message",
			    NewState = expulse_participant(Packet, From, StateData, 
					 translate:translate(Lang, ErrorText)),
			    {next_state, normal_state, NewState};
			_ ->
			    {next_state, normal_state, StateData}
		    end;
		"chat" ->
		    ErrText = "It is not allowed to send private messages to the conference",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, Err),
		    {next_state, normal_state, StateData};
		Type when (Type == "") or (Type == "normal") ->
		    case catch check_invitation(From, Els, Lang, StateData) of
			{error, Error} ->
			    Err = jlib:make_error_reply(
				    Packet, Error),
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, Err),
			    {next_state, normal_state, StateData};
			IJID ->
			    Config = StateData#state.config,
			    case Config#config.members_only of
				true ->
				    case get_affiliation(IJID, StateData) of
					none ->
					    NSD = set_affiliation(
						    IJID,
						    member,
						    StateData),
					    case (NSD#state.config)#config.persistent of
						true ->
						    mod_muc:store_room(
						      NSD#state.host,
						      NSD#state.room,
						      make_opts(NSD));
						_ ->
						    ok
					    end,
					    {next_state, normal_state, NSD};
					_ ->
					    {next_state, normal_state,
					     StateData}
				    end;
				false ->
				    {next_state, normal_state, StateData}
			    end
		    end;
		_ ->
		    ErrText = "Improper message type",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, Err),
		    {next_state, normal_state, StateData}
	    end;
	_ ->
	    case xml:get_attr_s("type", Attrs) of
		"error" ->
		    ok;
		_ ->
		    handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From)
	    end,
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, "",
	      {xmlelement, "iq", _Attrs, _Els} = Packet},
	     StateData) ->
    case jlib:iq_query_info(Packet) of
	#iq{type = Type, xmlns = XMLNS, lang = Lang, sub_el = SubEl} = IQ when
	      (XMLNS == ?NS_MUC_ADMIN) or
	      (XMLNS == ?NS_MUC_OWNER) or
	      (XMLNS == ?NS_DISCO_INFO) or
	      (XMLNS == ?NS_DISCO_ITEMS) ->
	    Res1 = case XMLNS of
		       ?NS_MUC_ADMIN ->
			   process_iq_admin(From, Type, Lang, SubEl, StateData);
		       ?NS_MUC_OWNER ->
			   process_iq_owner(From, Type, Lang, SubEl, StateData);
		       ?NS_DISCO_INFO ->
			   process_iq_disco_info(From, Type, Lang, StateData);
		       ?NS_DISCO_ITEMS ->
			   process_iq_disco_items(From, Type, Lang, StateData)
		   end,
	    {IQRes, NewStateData} =
		case Res1 of
		    {result, Res, SD} ->
			{IQ#iq{type = result,
			       sub_el = [{xmlelement, "query",
					  [{"xmlns", XMLNS}],
					  Res
					 }]},
			 SD};
		    {error, Error} ->
			{IQ#iq{type = error,
			       sub_el = [SubEl, Error]},
			 StateData}
		end,
	    ejabberd_router:route(StateData#state.jid,
				  From,
				  jlib:iq_to_xml(IQRes)),
	    case NewStateData of
		stop ->
		    {stop, normal, StateData};
		_ ->
		    {next_state, normal_state, NewStateData}
	    end;
	reply ->
	    {next_state, normal_state, StateData};
	_ ->
	    Err = jlib:make_error_reply(
		    Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
	    ejabberd_router:route(StateData#state.jid, From, Err),
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, Nick,
	      {xmlelement, "presence", _Attrs, _Els} = Packet},
	     StateData) ->
    Activity = get_user_activity(From, StateData),
    Now = now_to_usec(now()),
    MinPresenceInterval =
	trunc(gen_mod:get_module_opt(
		StateData#state.server_host,
		mod_muc, min_presence_interval, 0) * 1000000),
    if
	(Now >= Activity#activity.presence_time + MinPresenceInterval) and
	(Activity#activity.presence == undefined) ->
	    NewActivity = Activity#activity{presence_time = Now},
	    StateData1 =
		StateData#state{
		  activity = ?DICT:store(
				jlib:jid_tolower(From),
				NewActivity,
				StateData#state.activity)},
	    process_presence(From, Nick, Packet, StateData1);
	true ->
	    if
		Activity#activity.presence == undefined ->
		    Interval = (Activity#activity.presence_time +
				MinPresenceInterval - Now) div 1000,
		    erlang:send_after(
		      Interval, self(), {process_user_presence, From});
		true ->
		    ok
	    end,
	    NewActivity = Activity#activity{presence = {Nick, Packet}},
	    StateData1 =
		StateData#state{
		  activity = ?DICT:store(
				jlib:jid_tolower(From),
				NewActivity,
				StateData#state.activity)},
	    {next_state, normal_state, StateData1}
    end;

normal_state({route, From, ToNick,
	      {xmlelement, "message", Attrs, _} = Packet},
	     StateData) ->
    Type = xml:get_attr_s("type", Attrs),
    Lang = xml:get_attr_s("xml:lang", Attrs),
    case decide_fate_message(Type, Packet, From, StateData) of
	{expulse_sender, Reason} ->
	    ?DEBUG(Reason, []),
	    ErrorText = "This participant is kicked from the room because "
		"he sent an error message to another participant",
	    NewState = expulse_participant(Packet, From, StateData, 
					   translate:translate(Lang, ErrorText)),
	    {next_state, normal_state, NewState};
	forget_message ->
	    {next_state, normal_state, StateData};
	continue_delivery ->
	    case {(StateData#state.config)#config.allow_private_messages,
		is_user_online(From, StateData)} of
		{true, true} ->
		    case Type of
			"groupchat" ->
			    ErrText = "It is not allowed to send private "
				"messages of type \"groupchat\"",
			    Err = jlib:make_error_reply(
				    Packet, ?ERRT_BAD_REQUEST(Lang, ErrText)),
			    ejabberd_router:route(
			      jlib:jid_replace_resource(
				StateData#state.jid,
				ToNick),
			      From, Err);
			_ ->
			    case find_jid_by_nick(ToNick, StateData) of
				false ->
				    ErrText = "Recipient is not in the conference room",
				    Err = jlib:make_error_reply(
					    Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
				    ejabberd_router:route(
				      jlib:jid_replace_resource(
					StateData#state.jid,
					ToNick),
				      From, Err);
				ToJID ->
				    {ok, #user{nick = FromNick}} =
					?DICT:find(jlib:jid_tolower(From),
						   StateData#state.users),
				    ejabberd_router:route(
				      jlib:jid_replace_resource(
					StateData#state.jid,
					FromNick),
				      ToJID, Packet)
			    end
		    end;
		{true, false} ->
		    ErrText = "Only occupants are allowed to send messages to the conference",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(
			StateData#state.jid,
			ToNick),
		      From, Err);
		{false, _} ->
		    ErrText = "It is not allowed to send private messages",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(
			StateData#state.jid,
			ToNick),
		      From, Err)
	    end,
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, ToNick,
	      {xmlelement, "iq", Attrs, _Els} = Packet},
	     StateData) ->
    Lang = xml:get_attr_s("xml:lang", Attrs),
    case {(StateData#state.config)#config.allow_query_users,
	  is_user_online(From, StateData)} of
	{true, true} ->
	    case find_jid_by_nick(ToNick, StateData) of
		false ->
		    case jlib:iq_query_info(Packet) of
			reply ->
			    ok;
			_ ->
			    ErrText = "Recipient is not in the conference room",
			    Err = jlib:make_error_reply(
				    Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
			    ejabberd_router:route(
			      jlib:jid_replace_resource(
				StateData#state.jid, ToNick),
			      From, Err)
		    end;
		ToJID ->
		    {ok, #user{nick = FromNick}} =
			?DICT:find(jlib:jid_tolower(From),
				   StateData#state.users),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(StateData#state.jid, FromNick),
		      ToJID, Packet)
	    end;
	{_, false} ->
	    case jlib:iq_query_info(Packet) of
		reply ->
		    ok;
		_ ->
		    ErrText = "Only occupants are allowed to send queries to the conference",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(StateData#state.jid, ToNick),
		      From, Err)
	    end;
	_ ->
	    case jlib:iq_query_info(Packet) of
		reply ->
		    ok;
		_ ->
		    ErrText = "Queries to the conference members are not allowed in this room",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ALLOWED(Lang, ErrText)),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(StateData#state.jid, ToNick),
		      From, Err)
	    end
    end,
    {next_state, normal_state, StateData};

normal_state(_Event, StateData) ->
    {next_state, normal_state, StateData}.



%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event({service_message, Msg}, _StateName, StateData) ->
    MessagePkt = {xmlelement, "message",
		  [{"type", "groupchat"}],
		  [{xmlelement, "body", [], [{xmlcdata, Msg}]}]},
    lists:foreach(
      fun({_LJID, Info}) ->
	      ejabberd_router:route(
		StateData#state.jid,
		Info#user.jid,
		MessagePkt)
      end,
      ?DICT:to_list(StateData#state.users)),
    NSD = add_message_to_history("",
				 MessagePkt,
				 StateData),
    {next_state, normal_state, NSD};

handle_event({destroy, Reason}, _StateName, StateData) ->
    {result, [], stop} =
        destroy_room(
          {xmlelement, "destroy",
           [{"xmlns", ?NS_MUC_OWNER}],
           case Reason of
               none -> [];
               _Else ->
                   [{xmlelement, "reason",
                     [], [{xmlcdata, Reason}]}]
           end}, StateData),
    ?INFO_MSG("Destroyed MUC room ~s with reason: ~p", 
	      [jlib:jid_to_string(StateData#state.jid), Reason]),
    {stop, normal, StateData};
handle_event(destroy, StateName, StateData) ->
    ?INFO_MSG("Destroyed MUC room ~s", 
	      [jlib:jid_to_string(StateData#state.jid)]),
    handle_event({destroy, none}, StateName, StateData);

handle_event({set_affiliations, Affiliations}, StateName, StateData) ->
    {next_state, StateName, StateData#state{affiliations = Affiliations}};

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
handle_sync_event({get_disco_item, JID, Lang}, _From, StateName, StateData) ->
    FAffiliation = get_affiliation(JID, StateData),
    FRole = get_role(JID, StateData),
    Tail =
	case ((StateData#state.config)#config.public_list == true) orelse
	    (FRole /= none) orelse
	    (FAffiliation == admin) orelse
	    (FAffiliation == owner) of
	    true ->
		Desc = case (StateData#state.config)#config.public of
			   true ->
			       "";
			   _ ->
			       translate:translate(Lang, "private, ")
		       end,
		Len = ?DICT:fold(fun(_, _, Acc) -> Acc + 1 end, 0,
				 StateData#state.users),
		" (" ++ Desc ++ integer_to_list(Len) ++ ")";
	    _ ->
		" (n/a)"
	end,
    Reply = case ((StateData#state.config)#config.public == true) orelse
		(FRole /= none) orelse
		(FAffiliation == admin) orelse
		(FAffiliation == owner) of
		true ->
		    {item, get_title(StateData) ++ Tail};
		_ ->
		    false
	    end,
    {reply, Reply, StateName, StateData};
handle_sync_event(get_config, _From, StateName, StateData) ->
    {reply, {ok, StateData#state.config}, StateName, StateData};
handle_sync_event(get_state, _From, StateName, StateData) ->
    {reply, {ok, StateData}, StateName, StateData};
handle_sync_event({change_config, Config}, _From, StateName, StateData) ->
    {result, [], NSD} = change_config(Config, StateData),
    {reply, {ok, NSD#state.config}, StateName, NSD};
handle_sync_event({change_state, NewStateData}, _From, StateName, _StateData) ->
    {reply, {ok, NewStateData}, StateName, NewStateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({process_user_presence, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({presence, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if
	RoomQueueEmpty ->
	    StateData2 = prepare_room_queue(StateData1),
	    {next_state, normal_state, StateData2};
	true ->
	    {next_state, normal_state, StateData1}
    end;
handle_info({process_user_message, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({message, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if
	RoomQueueEmpty ->
	    StateData2 = prepare_room_queue(StateData1),
	    {next_state, normal_state, StateData2};
	true ->
	    {next_state, normal_state, StateData1}
    end;
handle_info(process_room_queue, normal_state = StateName, StateData) ->
    case queue:out(StateData#state.room_queue) of
	{{value, {message, From}}, RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    Packet = Activity#activity.message,
	    NewActivity = Activity#activity{message = undefined},
	    StateData1 =
		StateData#state{
		  activity = ?DICT:store(
				jlib:jid_tolower(From),
				NewActivity,
				StateData#state.activity),
		  room_queue = RoomQueue},
	    StateData2 = prepare_room_queue(StateData1),
	    process_groupchat_message(From, Packet, StateData2);
	{{value, {presence, From}}, RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    {Nick, Packet} = Activity#activity.presence,
	    NewActivity = Activity#activity{presence = undefined},
	    StateData1 =
		StateData#state{
		  activity = ?DICT:store(
				jlib:jid_tolower(From),
				NewActivity,
				StateData#state.activity),
		  room_queue = RoomQueue},
	    StateData2 = prepare_room_queue(StateData1),
	    process_presence(From, Nick, Packet, StateData2);
	{empty, _} ->
	    {next_state, StateName, StateData}
    end;
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, StateData) ->
    ?DICT:fold(
       fun(J, _, _) ->
	       tab_remove_online_user(J, StateData)
       end, [], StateData#state.users),
    mod_muc:room_destroyed(StateData#state.host, StateData#state.room, self(),
			   StateData#state.server_host),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

route(Pid, From, ToNick, Packet) ->
    gen_fsm:send_event(Pid, {route, From, ToNick, Packet}).

process_groupchat_message(From, {xmlelement, "message", Attrs, _Els} = Packet,
			  StateData) ->
    Lang = xml:get_attr_s("xml:lang", Attrs),
    case is_user_online(From, StateData) orelse
	is_user_allowed_message_nonparticipant(From, StateData) of
	true ->
	    {FromNick, Role} = get_participant_data(From, StateData),
	    if
		(Role == moderator) or (Role == participant) 
		or ((StateData#state.config)#config.moderated == false) ->
		    {NewStateData1, IsAllowed} =
			case check_subject(Packet) of
			    false ->
				{StateData, true};
			    Subject ->
				case can_change_subject(Role,
							StateData) of
				    true ->
					NSD =
					    StateData#state{
					      subject = Subject,
					      subject_author =
					      FromNick},
					case (NSD#state.config)#config.persistent of
					    true ->
						mod_muc:store_room(
						  NSD#state.host,
						  NSD#state.room,
						  make_opts(NSD));
					    _ ->
						ok
					end,
					{NSD, true};
				    _ ->
					{StateData, false}
				end
			end,
		    case IsAllowed of
			true ->
			    lists:foreach(
			      fun({_LJID, Info}) ->
				      ejabberd_router:route(
					jlib:jid_replace_resource(
					  StateData#state.jid,
					  FromNick),
					Info#user.jid,
					Packet)
			      end,
			      ?DICT:to_list(StateData#state.users)),
			    NewStateData2 =
				add_message_to_history(FromNick,
						       Packet,
						       NewStateData1),
			    {next_state, normal_state, NewStateData2};
			_ ->
			    Err =
				case (StateData#state.config)#config.allow_change_subj of
				    true ->
					?ERRT_FORBIDDEN(
					   Lang,
					   "Only moderators and participants "
					   "are allowed to change subject in this room");
				    _ ->
					?ERRT_FORBIDDEN(
					   Lang,
					   "Only moderators "
					   "are allowed to change subject in this room")
				end,
			    ejabberd_router:route(
			      StateData#state.jid,
			      From,
			      jlib:make_error_reply(Packet, Err)),
			    {next_state, normal_state, StateData}
		    end;
		true ->
		    ErrText = "Visitors are not allowed to send messages to all occupants",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, Err),
		    {next_state, normal_state, StateData}
	    end;
	false ->
	    ErrText = "Only occupants are allowed to send messages to the conference",
	    Err = jlib:make_error_reply(
		    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
	    ejabberd_router:route(StateData#state.jid, From, Err),
	    {next_state, normal_state, StateData}
    end.


%% @doc Check if this non participant can send message to room.
%%
%% XEP-0045 v1.23:
%% 7.9 Sending a Message to All Occupants
%% an implementation MAY allow users with certain privileges
%% (e.g., a room owner, room admin, or service-level admin)
%% to send messages to the room even if those users are not occupants.
%%
%% Check the mod_muc option access_message_nonparticipant and wether this JID
%% is allowed or denied
is_user_allowed_message_nonparticipant(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} = StateData#state.access,
    case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
	allow ->
	    true;
	_ -> false
    end.

%% @doc Get information of this participant, or default values.
%% If the JID is not a participant, return values for a service message.
get_participant_data(From, StateData) ->
    case ?DICT:find(jlib:jid_tolower(From), StateData#state.users) of
	{ok, #user{nick = FromNick, role = Role}} ->
	    {FromNick, Role};
	error ->
	    {"", moderator}
    end.


process_presence(From, Nick, {xmlelement, "presence", Attrs, _Els} = Packet,
		 StateData) ->
    Type = xml:get_attr_s("type", Attrs),
    Lang = xml:get_attr_s("xml:lang", Attrs),
    StateData1 =
	case Type of
	    "unavailable" ->
		case is_user_online(From, StateData) of
		    true ->
			NewState =
			    add_user_presence_un(From, Packet, StateData),
			send_new_presence(From, NewState),
			Reason = case xml:get_subtag(Packet, "status") of
				false -> "";
				Status_el -> xml:get_tag_cdata(Status_el)
			end,
			remove_online_user(From, NewState, Reason);
		    _ ->
			StateData
		end;
	    "error" ->
		case is_user_online(From, StateData) of
		    true ->
			ErrorText = "This participant is kicked from the room because "
			    "he sent an error presence",
			expulse_participant(Packet, From, StateData,
					    translate:translate(Lang, ErrorText));
		    _ ->
			StateData
		end;
	    "" ->
		case is_user_online(From, StateData) of
		    true ->
			case is_nick_change(From, Nick, StateData) of
			    true ->
				case {is_nick_exists(Nick, StateData),
				      mod_muc:can_use_nick(
					StateData#state.host, From, Nick)} of
				    {true, _} ->
					Lang = xml:get_attr_s("xml:lang", Attrs),
					ErrText = "Nickname is already in use by another occupant",
					Err = jlib:make_error_reply(
						Packet,
						?ERRT_CONFLICT(Lang, ErrText)),
					ejabberd_router:route(
					  jlib:jid_replace_resource(
					    StateData#state.jid,
					    Nick), % TODO: s/Nick/""/
					  From, Err),
					StateData;
				    {_, false} ->
					ErrText = "Nickname is registered by another person",
					Err = jlib:make_error_reply(
						Packet,
						?ERRT_CONFLICT(Lang, ErrText)),
					ejabberd_router:route(
					  % TODO: s/Nick/""/
					  jlib:jid_replace_resource(
					    StateData#state.jid,
					    Nick),
					  From, Err),
					StateData;
				    _ ->
					change_nick(From, Nick, StateData)
				end;
			    _ ->
				NewState =
				    add_user_presence(From, Packet, StateData),
				send_new_presence(From, NewState),
				NewState
			end;
		    _ ->
			add_new_user(From, Nick, Packet, StateData)
		end;
	    _ ->
		StateData
	end,
    case (not (StateData1#state.config)#config.persistent) andalso
	(?DICT:to_list(StateData1#state.users) == []) of
	true ->
	    ?INFO_MSG("Destroyed MUC room ~s because it's temporary and empty", 
		      [jlib:jid_to_string(StateData#state.jid)]),
	    {stop, normal, StateData1};
	_ ->
	    {next_state, normal_state, StateData1}
    end.

is_user_online(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    ?DICT:is_key(LJID, StateData#state.users).

role_to_list(Role) ->
    case Role of
	moderator ->   "moderator";
	participant -> "participant";
	visitor ->     "visitor";
	none ->        "none"
    end.

affiliation_to_list(Affiliation) ->
    case Affiliation of
	owner ->   "owner";
	admin ->   "admin";
	member ->  "member";
	outcast -> "outcast";
	none ->    "none"
    end.

list_to_role(Role) ->
    case Role of
	"moderator" ->   moderator;
	"participant" -> participant;
	"visitor" ->     visitor;
	"none" ->        none
    end.

list_to_affiliation(Affiliation) ->
    case Affiliation of
	"owner" ->   owner;
	"admin" ->   admin;
	"member" ->  member;
	"outcast" -> outcast;
	"none" ->    none
    end.

%% Decide the fate of the message and its sender
%% Returns: continue_delivery | forget_message | {expulse_sender, Reason}
decide_fate_message("error", Packet, From, StateData) ->
    %% Make a preliminary decision
    PD = case check_error_kick(Packet) of
	     %% If this is an error stanza and its condition matches a criteria
	     true ->
		 Reason = io_lib:format("This participant is considered a ghost and is expulsed: ~s",
					[jlib:jid_to_string(From)]),
		 {expulse_sender, Reason};
	     false ->
		 continue_delivery
	 end,
    case PD of
	{expulse_sender, R} ->
	    case is_user_online(From, StateData) of
		true ->
		    {expulse_sender, R};
		false ->
		    forget_message
	    end;
	Other ->
	    Other
    end;

decide_fate_message(_, _, _, _) ->
    continue_delivery.

%% Check if the elements of this error stanza indicate
%% that the sender is a dead participant.
%% If so, return true to kick the participant.
check_error_kick(Packet) ->
    case get_error_condition(Packet) of
	"gone" -> true;
	"internal-server-error" -> true;
	"item-not-found" -> true;
	"jid-malformed" -> true;
	"recipient-unavailable" -> true;
	"redirect" -> true;
	"remote-server-not-found" -> true;
	"remote-server-timeout" -> true;
	"service-unavailable" -> true;
	_ -> false
    end.

get_error_condition(Packet) ->
	case catch get_error_condition2(Packet) of
	     {condition, ErrorCondition} ->
		ErrorCondition;
	     {'EXIT', _} ->
		"badformed error stanza"
	end.
get_error_condition2(Packet) ->
	{xmlelement, _, _, EEls} = xml:get_subtag(Packet, "error"),
	[Condition] = [Name || {xmlelement, Name, [{"xmlns", ?NS_STANZAS}], []} <- EEls],
	{condition, Condition}.

expulse_participant(Packet, From, StateData, Reason1) ->
	ErrorCondition = get_error_condition(Packet),
	Reason2 = io_lib:format(Reason1 ++ ": " ++ "~s", [ErrorCondition]),
	NewState = add_user_presence_un(
		From,
		{xmlelement, "presence",
		[{"type", "unavailable"}],
		[{xmlelement, "status", [],
		[{xmlcdata, Reason2}]
		}]},
	StateData),
	send_new_presence(From, NewState),
	remove_online_user(From, NewState).


set_affiliation(JID, Affiliation, StateData) ->
    LJID = jlib:jid_remove_resource(jlib:jid_tolower(JID)),
    Affiliations = case Affiliation of
		       none ->
			   ?DICT:erase(LJID,
				       StateData#state.affiliations);
		       _ ->
			   ?DICT:store(LJID,
				       Affiliation,
				       StateData#state.affiliations)
		   end,
    StateData#state{affiliations = Affiliations}.

set_affiliation_and_reason(JID, Affiliation, Reason, StateData) ->
    LJID = jlib:jid_remove_resource(jlib:jid_tolower(JID)),
    Affiliations = case Affiliation of
		       none ->
			   ?DICT:erase(LJID,
				       StateData#state.affiliations);
		       _ ->
			   ?DICT:store(LJID,
				       {Affiliation, Reason},
				       StateData#state.affiliations)
		   end,
    StateData#state{affiliations = Affiliations}.

get_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} = StateData#state.access,
    Res =
	case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
	    allow ->
		owner;
	    _ ->
		LJID = jlib:jid_tolower(JID),
		case ?DICT:find(LJID, StateData#state.affiliations) of
		    {ok, Affiliation} ->
			Affiliation;
		    _ ->
			LJID1 = jlib:jid_remove_resource(LJID),
			case ?DICT:find(LJID1, StateData#state.affiliations) of
			    {ok, Affiliation} ->
				Affiliation;
			    _ ->
				LJID2 = setelement(1, LJID, ""),
				case ?DICT:find(LJID2, StateData#state.affiliations) of
				    {ok, Affiliation} ->
					Affiliation;
				    _ ->
					LJID3 = jlib:jid_remove_resource(LJID2),
					case ?DICT:find(LJID3, StateData#state.affiliations) of
					    {ok, Affiliation} ->
						Affiliation;
					    _ ->
						none
					end
				end
			end
		end
	end,
    case Res of
	{A, _Reason} ->
	    A;
	_ ->
	    Res
    end.

get_service_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} =
	StateData#state.access,
    case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
	allow ->
	    owner;
	_ ->
	    none
    end.

set_role(JID, Role, StateData) ->
    LJID = jlib:jid_tolower(JID),
    LJIDs = case LJID of
		{U, S, ""} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], StateData#state.users);
		_ ->
		    case ?DICT:is_key(LJID, StateData#state.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    Users = case Role of
		none ->
		    lists:foldl(fun(J, Us) ->
					?DICT:erase(J,
						    Us)
				end, StateData#state.users, LJIDs);
		_ ->
		    lists:foldl(fun(J, Us) ->
					{ok, User} = ?DICT:find(J, Us),
					?DICT:store(J,
						    User#user{role = Role},
						    Us)
				end, StateData#state.users, LJIDs)
	    end,
    StateData#state{users = Users}.

get_role(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    case ?DICT:find(LJID, StateData#state.users) of
	{ok, #user{role = Role}} ->
	    Role;
	_ ->
	    none
    end.

get_default_role(Affiliation, StateData) ->
    case Affiliation of
	owner ->   moderator;
	admin ->   moderator;
	member ->  participant;
	outcast -> none;
	none ->
	    case (StateData#state.config)#config.members_only of
		true ->
		    none;
		_ ->
		    case (StateData#state.config)#config.members_by_default of
			true ->
			    participant;
			_ ->
			    visitor
		    end
	    end
    end.

get_max_users(StateData) ->
    MaxUsers = (StateData#state.config)#config.max_users,
    ServiceMaxUsers = get_service_max_users(StateData),
    if
	MaxUsers =< ServiceMaxUsers -> MaxUsers;
	true -> ServiceMaxUsers
    end.

get_service_max_users(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
			   mod_muc, max_users, ?MAX_USERS_DEFAULT).

get_max_users_admin_threshold(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
			   mod_muc, max_users_admin_threshold, 5).

get_user_activity(JID, StateData) ->
    case ?DICT:find(jlib:jid_tolower(JID),
		    StateData#state.activity) of
	{ok, A} -> A;
	error ->
	    MessageShaper =
		shaper:new(gen_mod:get_module_opt(
			     StateData#state.server_host,
			     mod_muc, user_message_shaper, none)),
	    PresenceShaper =
		shaper:new(gen_mod:get_module_opt(
			     StateData#state.server_host,
			     mod_muc, user_presence_shaper, none)),
	    #activity{message_shaper = MessageShaper,
		      presence_shaper = PresenceShaper}
    end.

prepare_room_queue(StateData) ->
    case queue:out(StateData#state.room_queue) of
	{{value, {message, From}}, _RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    Packet = Activity#activity.message,
	    Size = lists:flatlength(xml:element_to_string(Packet)),
	    {RoomShaper, RoomShaperInterval} =
		shaper:update(StateData#state.room_shaper, Size),
	    erlang:send_after(
	      RoomShaperInterval, self(),
	      process_room_queue),
	    StateData#state{
	      room_shaper = RoomShaper};
	{{value, {presence, From}}, _RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    {_Nick, Packet} = Activity#activity.presence,
	    Size = lists:flatlength(xml:element_to_string(Packet)),
	    {RoomShaper, RoomShaperInterval} =
		shaper:update(StateData#state.room_shaper, Size),
	    erlang:send_after(
	      RoomShaperInterval, self(),
	      process_room_queue),
	    StateData#state{
	      room_shaper = RoomShaper};
	{empty, _} ->
	    StateData
    end.


add_online_user(JID, Nick, Role, StateData) ->
    LJID = jlib:jid_tolower(JID),
    Users = ?DICT:store(LJID,
			#user{jid = JID,
			      nick = Nick,
			      role = Role},
			StateData#state.users),
    add_to_log(join, Nick, StateData),
    tab_add_online_user(JID, StateData),
    StateData#state{users = Users}.

remove_online_user(JID, StateData) ->
	remove_online_user(JID, StateData, "").

remove_online_user(JID, StateData, Reason) ->
    LJID = jlib:jid_tolower(JID),
    {ok, #user{nick = Nick}} =
    	?DICT:find(LJID, StateData#state.users),
    add_to_log(leave, {Nick, Reason}, StateData),
    tab_remove_online_user(JID, StateData),
    Users = ?DICT:erase(LJID, StateData#state.users),
    StateData#state{users = Users}.


filter_presence({xmlelement, "presence", Attrs, Els}) ->
    FEls = lists:filter(
	     fun(El) ->
		     case El of
			 {xmlcdata, _} ->
			     false;
			 {xmlelement, _Name1, Attrs1, _Els1} ->
			     XMLNS = xml:get_attr_s("xmlns", Attrs1),
			     case XMLNS of
				 ?NS_MUC ++ _ ->
				     false;
				 _ ->
				     true
			     end
		     end
	     end, Els),
    {xmlelement, "presence", Attrs, FEls}.


add_user_presence(JID, Presence, StateData) ->
    LJID = jlib:jid_tolower(JID),
    FPresence = filter_presence(Presence),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{last_presence = FPresence}
	   end, StateData#state.users),
    StateData#state{users = Users}.

add_user_presence_un(JID, Presence, StateData) ->
    LJID = jlib:jid_tolower(JID),
    FPresence = filter_presence(Presence),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{last_presence = FPresence,
			     role = none}
	   end, StateData#state.users),
    StateData#state{users = Users}.


is_nick_exists(Nick, StateData) ->
    ?DICT:fold(fun(_, #user{nick = N}, B) ->
		       B orelse (N == Nick)
	       end, false, StateData#state.users).

find_jid_by_nick(Nick, StateData) ->
    ?DICT:fold(fun(_, #user{jid = JID, nick = N}, R) ->
		       case Nick of
			   N -> JID;
			   _ -> R
		       end
	       end, false, StateData#state.users).

is_nick_change(JID, Nick, StateData) ->
    LJID = jlib:jid_tolower(JID),
    case Nick of
	"" ->
	    false;
	_ ->
	    {ok, #user{nick = OldNick}} =
		?DICT:find(LJID, StateData#state.users),
	    Nick /= OldNick
    end.

add_new_user(From, Nick, {xmlelement, _, Attrs, Els} = Packet, StateData) ->
    Lang = xml:get_attr_s("xml:lang", Attrs),
    MaxUsers = get_max_users(StateData),
    MaxAdminUsers = MaxUsers + get_max_users_admin_threshold(StateData),
    NUsers = dict:fold(fun(_, _, Acc) -> Acc + 1 end, 0,
		       StateData#state.users),
    Affiliation = get_affiliation(From, StateData),
    ServiceAffiliation = get_service_affiliation(From, StateData),
    NConferences = tab_count_user(From),
    MaxConferences = gen_mod:get_module_opt(
		       StateData#state.server_host,
		       mod_muc, max_user_conferences, 10),
    case {(ServiceAffiliation == owner orelse
	   MaxUsers == none orelse
	   ((Affiliation == admin orelse Affiliation == owner) andalso
	    NUsers < MaxAdminUsers) orelse
	   NUsers < MaxUsers) andalso
	  NConferences < MaxConferences,
	  is_nick_exists(Nick, StateData),
	  mod_muc:can_use_nick(StateData#state.host, From, Nick),
	  get_default_role(Affiliation, StateData)} of
	{false, _, _, _} ->
	    % max user reached and user is not admin or owner
	    Err = jlib:make_error_reply(
		    Packet,
		    ?ERR_SERVICE_UNAVAILABLE),
	    ejabberd_router:route( % TODO: s/Nick/""/
	      jlib:jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, _, none} ->
	    Err = jlib:make_error_reply(
		    Packet,
		    case Affiliation of
			outcast ->
			    ErrText = "You have been banned from this room",
			    ?ERRT_FORBIDDEN(Lang, ErrText);
			_ ->
			    ErrText = "Membership required to enter this room",
			    ?ERRT_REGISTRATION_REQUIRED(Lang, ErrText)
		    end),
	    ejabberd_router:route( % TODO: s/Nick/""/
	      jlib:jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, true, _, _} ->
	    ErrText = "Nickname is already in use by another occupant",
	    Err = jlib:make_error_reply(Packet, ?ERRT_CONFLICT(Lang, ErrText)),
	    ejabberd_router:route(
	      % TODO: s/Nick/""/
	      jlib:jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, false, _} ->
	    ErrText = "Nickname is registered by another person",
	    Err = jlib:make_error_reply(Packet, ?ERRT_CONFLICT(Lang, ErrText)),
	    ejabberd_router:route(
	      % TODO: s/Nick/""/
	      jlib:jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, _, Role} ->
	    case check_password(Affiliation, Els, StateData) of
		true ->
		    NewState =
			add_user_presence(
			  From, Packet,
			  add_online_user(From, Nick, Role, StateData)),
		    if not (NewState#state.config)#config.anonymous ->
			    WPacket = {xmlelement, "message", [{"type", "groupchat"}],
				       [{xmlelement, "body", [],
					 [{xmlcdata, translate:translate(
						       Lang,
						       "This room is not anonymous")}]},
					{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
					 [{xmlelement, "status", [{"code", "100"}], []}]}]},
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, WPacket);
			true ->
			    ok
		    end,
		    send_existing_presences(From, NewState),
		    send_new_presence(From, NewState),
		    Shift = count_stanza_shift(Nick, Els, NewState),
		    case send_history(From, Shift, NewState) of
			true ->
			    ok;
			_ ->
			    send_subject(From, Lang, StateData)
		    end,
		    case NewState#state.just_created of
			true ->
			    NewState#state{just_created = false};
			false ->
			    NewState
		    end;
		nopass ->
		    ErrText = "Password required to enter this room",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_AUTHORIZED(Lang, ErrText)),
		    ejabberd_router:route( % TODO: s/Nick/""/
		      jlib:jid_replace_resource(
			StateData#state.jid, Nick),
		      From, Err),
		    StateData;
		_ ->
		    ErrText = "Incorrect password",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_AUTHORIZED(Lang, ErrText)),
		    ejabberd_router:route( % TODO: s/Nick/""/
		      jlib:jid_replace_resource(
			StateData#state.jid, Nick),
		      From, Err),
		    StateData
	   end
    end.

check_password(owner, _Els, _StateData) ->
    true;
check_password(_Affiliation, Els, StateData) ->
    case (StateData#state.config)#config.password_protected of
	false ->
	    true;
	true ->
	    Pass = extract_password(Els),
	    case Pass of
		false ->
		    nopass;
		_ ->
		    case (StateData#state.config)#config.password of
			Pass ->
			    true;
			_ ->
			false
		    end
	    end
    end.

extract_password([]) ->
    false;
extract_password([{xmlelement, _Name, Attrs, _SubEls} = El | Els]) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_MUC ->
	    case xml:get_subtag(El, "password") of
		false ->
		    false;
		SubEl ->
		    xml:get_tag_cdata(SubEl)
	    end;
	_ ->
	    extract_password(Els)
    end;
extract_password([_ | Els]) ->
    extract_password(Els).

count_stanza_shift(Nick, Els, StateData) ->
    HL = lqueue_to_list(StateData#state.history),
    Since = extract_history(Els, "since"),
    Shift0 = case Since of
		 false ->
		     0;
		 _ ->
		     Sin = calendar:datetime_to_gregorian_seconds(Since),
		     count_seconds_shift(Sin, HL)
	     end,
    Seconds = extract_history(Els, "seconds"),
    Shift1 = case Seconds of
		 false ->
		     0;
		 _ ->
		     Sec = calendar:datetime_to_gregorian_seconds(
			     calendar:now_to_universal_time(now())) - Seconds,
		     count_seconds_shift(Sec, HL)
	     end,
    MaxStanzas = extract_history(Els, "maxstanzas"),
    Shift2 = case MaxStanzas of
		 false ->
		     0;
		 _ ->
		     count_maxstanzas_shift(MaxStanzas, HL)
	     end,
    MaxChars = extract_history(Els, "maxchars"),
    Shift3 = case MaxChars of
		 false ->
		     0;
		 _ ->
		     count_maxchars_shift(Nick, MaxChars, HL)
	     end,
    lists:max([Shift0, Shift1, Shift2, Shift3]).

count_seconds_shift(Seconds, HistoryList) ->
    lists:sum(
      lists:map(
	fun({_Nick, _Packet, _HaveSubject, TimeStamp, _Size}) ->
	    T = calendar:datetime_to_gregorian_seconds(TimeStamp),
	    if
		T < Seconds ->
		    1;
		true ->
		    0
	    end
	end, HistoryList)).

count_maxstanzas_shift(MaxStanzas, HistoryList) ->
    S = length(HistoryList) - MaxStanzas,
    if
	S =< 0 ->
	    0;
	true ->
	    S
    end.

count_maxchars_shift(Nick, MaxSize, HistoryList) ->
    NLen = string:len(Nick) + 1,
    Sizes = lists:map(
	      fun({_Nick, _Packet, _HaveSubject, _TimeStamp, Size}) ->
		  Size + NLen
	      end, HistoryList),
    calc_shift(MaxSize, Sizes).

calc_shift(MaxSize, Sizes) ->
    Total = lists:sum(Sizes),
    calc_shift(MaxSize, Total, 0, Sizes).

calc_shift(_MaxSize, _Size, Shift, []) ->
    Shift;
calc_shift(MaxSize, Size, Shift, [S | TSizes]) ->
    if
	MaxSize >= Size ->
	    Shift;
	true ->
	    calc_shift(MaxSize, Size - S, Shift + 1, TSizes)
    end.

extract_history([], _Type) ->
    false;
extract_history([{xmlelement, _Name, Attrs, _SubEls} = El | Els], Type) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_MUC ->
	    AttrVal = xml:get_path_s(El,
		       [{elem, "history"}, {attr, Type}]),
	    case Type of
		"since" ->
		    case jlib:datetime_string_to_timestamp(AttrVal) of
			undefined ->
			    false;
			TS ->
			    calendar:now_to_universal_time(TS)
		    end;
		_ ->
		    case catch list_to_integer(AttrVal) of
			IntVal when is_integer(IntVal) and (IntVal >= 0) ->
			    IntVal;
			_ ->
			    false
		    end
	    end;
	_ ->
	    extract_history(Els, Type)
    end;
extract_history([_ | Els], Type) ->
    extract_history(Els, Type).


send_update_presence(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    LJIDs = case LJID of
		{U, S, ""} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], StateData#state.users);
		_ ->
		    case ?DICT:is_key(LJID, StateData#state.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    lists:foreach(fun(J) ->
			  send_new_presence(J, StateData)
		  end, LJIDs).

send_new_presence(NJID, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick,
	       role = Role,
	       last_presence = Presence}} =
	?DICT:find(jlib:jid_tolower(NJID), StateData#state.users),
    Affiliation = get_affiliation(NJID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    SRole = role_to_list(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [{"jid", jlib:jid_to_string(RealJID)},
			   {"affiliation", SAffiliation},
			   {"role", SRole}];
		      _ ->
			  [{"affiliation", SAffiliation},
			   {"role", SRole}]
		  end,
	      Status = case StateData#state.just_created of
			   true ->
			       [{xmlelement, "status", [{"code", "201"}], []}];
			   false ->
			       []
		       end,
	      Packet = append_subtags(
			 Presence,
			 [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			   [{xmlelement, "item", ItemAttrs, []} | Status]}]),
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).


send_existing_presences(ToJID, StateData) ->
    LToJID = jlib:jid_tolower(ToJID),
    {ok, #user{jid = RealToJID,
	       role = Role}} =
	?DICT:find(LToJID, StateData#state.users),
    lists:foreach(
      fun({LJID, #user{jid = FromJID,
		       nick = FromNick,
		       role = FromRole,
		       last_presence = Presence
		      }}) ->
	      case RealToJID of
		  FromJID ->
		      ok;
		  _ ->
		      FromAffiliation = get_affiliation(LJID, StateData),
		      ItemAttrs =
			  case (Role == moderator) orelse
			      ((StateData#state.config)#config.anonymous ==
			       false) of
			      true ->
				  [{"jid", jlib:jid_to_string(FromJID)},
				   {"affiliation",
				    affiliation_to_list(FromAffiliation)},
				   {"role", role_to_list(FromRole)}];
			      _ ->
				  [{"affiliation",
				    affiliation_to_list(FromAffiliation)},
				   {"role", role_to_list(FromRole)}]
			  end,
		      Packet = append_subtags(
				 Presence,
				 [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
				   [{xmlelement, "item", ItemAttrs, []}]}]),
		      ejabberd_router:route(
			jlib:jid_replace_resource(
			  StateData#state.jid, FromNick),
			RealToJID,
			Packet)
	      end
      end, ?DICT:to_list(StateData#state.users)).


append_subtags({xmlelement, Name, Attrs, SubTags1}, SubTags2) ->
    {xmlelement, Name, Attrs, SubTags1 ++ SubTags2}.


now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.


change_nick(JID, Nick, StateData) ->
    LJID = jlib:jid_tolower(JID),
    {ok, #user{nick = OldNick}} =
	?DICT:find(LJID, StateData#state.users),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{nick = Nick}
	   end, StateData#state.users),
    NewStateData = StateData#state{users = Users},
    send_nick_changing(JID, OldNick, NewStateData),
    add_to_log(nickchange, {OldNick, Nick}, StateData),
    NewStateData.

send_nick_changing(JID, OldNick, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick,
	       role = Role,
	       last_presence = Presence}} =
	?DICT:find(jlib:jid_tolower(JID), StateData#state.users),
    Affiliation = get_affiliation(JID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    SRole = role_to_list(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs1 =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [{"jid", jlib:jid_to_string(RealJID)},
			   {"affiliation", SAffiliation},
			   {"role", SRole},
			   {"nick", Nick}];
		      _ ->
			  [{"affiliation", SAffiliation},
			   {"role", SRole},
			   {"nick", Nick}]
		  end,
	      ItemAttrs2 =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [{"jid", jlib:jid_to_string(RealJID)},
			   {"affiliation", SAffiliation},
			   {"role", SRole}];
		      _ ->
			  [{"affiliation", SAffiliation},
			   {"role", SRole}]
		  end,
	      Packet1 =
		  {xmlelement, "presence", [{"type", "unavailable"}],
		   [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
		     [{xmlelement, "item", ItemAttrs1, []},
		      {xmlelement, "status", [{"code", "303"}], []}]}]},
	      Packet2 = append_subtags(
			  Presence,
			  [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			    [{xmlelement, "item", ItemAttrs2, []}]}]),
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, OldNick),
		Info#user.jid,
		Packet1),
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet2)
      end, ?DICT:to_list(StateData#state.users)).


lqueue_new(Max) ->
    #lqueue{queue = queue:new(),
	    len = 0,
	    max = Max}.

%% If the message queue limit is set to 0, do not store messages.
lqueue_in(_Item, LQ = #lqueue{max = 0}) ->
    LQ;
%% Otherwise, rotate messages in the queue store.
lqueue_in(Item, #lqueue{queue = Q1, len = Len, max = Max}) ->
    Q2 = queue:in(Item, Q1),
    if
	Len >= Max ->
	    Q3 = lqueue_cut(Q2, Len - Max + 1),
	    #lqueue{queue = Q3, len = Max, max = Max};
	true ->
	    #lqueue{queue = Q2, len = Len + 1, max = Max}
    end.

lqueue_cut(Q, 0) ->
    Q;
lqueue_cut(Q, N) ->
    {_, Q1} = queue:out(Q),
    lqueue_cut(Q1, N - 1).

lqueue_to_list(#lqueue{queue = Q1}) ->
    queue:to_list(Q1).


add_message_to_history(FromNick, Packet, StateData) ->
    HaveSubject = case xml:get_subtag(Packet, "subject") of
		      false ->
			  false;
		      _ ->
			  true
		  end,
    TimeStamp = calendar:now_to_universal_time(now()),
    TSPacket = append_subtags(Packet,
			      [jlib:timestamp_to_xml(TimeStamp)]),
    SPacket = jlib:replace_from_to(
		jlib:jid_replace_resource(StateData#state.jid, FromNick),
		StateData#state.jid,
		TSPacket),
    Size = lists:flatlength(xml:element_to_string(SPacket)),
    Q1 = lqueue_in({FromNick, TSPacket, HaveSubject, TimeStamp, Size},
		   StateData#state.history),
    add_to_log(text, {FromNick, Packet}, StateData),
    StateData#state{history = Q1}.

send_history(JID, Shift, StateData) ->
    lists:foldl(
      fun({Nick, Packet, HaveSubject, _TimeStamp, _Size}, B) ->
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		JID,
		Packet),
	      B or HaveSubject
      end, false, lists:nthtail(Shift, lqueue_to_list(StateData#state.history))).


send_subject(JID, Lang, StateData) ->
    case StateData#state.subject_author of
	"" ->
	    ok;
	Nick ->
	    Subject = StateData#state.subject,
	    Packet = {xmlelement, "message", [{"type", "groupchat"}],
		      [{xmlelement, "subject", [], [{xmlcdata, Subject}]},
		       {xmlelement, "body", [],
			[{xmlcdata,
			  Nick ++
			  translate:translate(Lang,
					      " has set the subject to: ") ++
			  Subject}]}]},
	    ejabberd_router:route(
	      StateData#state.jid,
	      JID,
	      Packet)
    end.

check_subject(Packet) ->
    case xml:get_subtag(Packet, "subject") of
	false ->
	    false;
	SubjEl ->
	    xml:get_tag_cdata(SubjEl)
    end.

can_change_subject(Role, StateData) ->
    case (StateData#state.config)#config.allow_change_subj of
	true ->
	    (Role == moderator) orelse (Role == participant);
	_ ->
	    Role == moderator
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Admin stuff

process_iq_admin(From, set, Lang, SubEl, StateData) ->
    {xmlelement, _, _, Items} = SubEl,
    process_admin_items_set(From, Items, Lang, StateData);

process_iq_admin(From, get, Lang, SubEl, StateData) ->
    case xml:get_subtag(SubEl, "item") of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	Item ->
	    FAffiliation = get_affiliation(From, StateData),
	    FRole = get_role(From, StateData),
	    case xml:get_tag_attr("role", Item) of
		false ->
		    case xml:get_tag_attr("affiliation", Item) of
			false ->
			    {error, ?ERR_BAD_REQUEST};
			{value, StrAffiliation} ->
			    case catch list_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    {error, ?ERR_BAD_REQUEST};
				SAffiliation ->
				    if
					(FAffiliation == owner) or
					(FAffiliation == admin) ->
					    Items = items_with_affiliation(
						      SAffiliation, StateData),
					    {result, Items, StateData};
					true ->
					    ErrText = "Administrator privileges required",
					    {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
				    end
			    end
		    end;
		{value, StrRole} ->
		    case catch list_to_role(StrRole) of
			{'EXIT', _} ->
			    {error, ?ERR_BAD_REQUEST};
			SRole ->
			    if
				FRole == moderator ->
				    Items = items_with_role(SRole, StateData),
				    {result, Items, StateData};
				true ->
				    ErrText = "Moderator privileges required",
				    {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
			    end
		    end
	    end
    end.


items_with_role(SRole, StateData) ->
    lists:map(
      fun({_, U}) ->
	      user_to_item(U, StateData)
      end, search_role(SRole, StateData)).

items_with_affiliation(SAffiliation, StateData) ->
    lists:map(
      fun({JID, {Affiliation, Reason}}) ->
	      {xmlelement, "item",
	       [{"affiliation", affiliation_to_list(Affiliation)},
		{"jid", jlib:jid_to_string(JID)}],
	       [{xmlelement, "reason", [], [{xmlcdata, Reason}]}]};
	 ({JID, Affiliation}) ->
	      {xmlelement, "item",
	       [{"affiliation", affiliation_to_list(Affiliation)},
		{"jid", jlib:jid_to_string(JID)}],
	       []}
      end, search_affiliation(SAffiliation, StateData)).

user_to_item(#user{role = Role,
		   nick = Nick,
		   jid = JID
		  }, StateData) ->
    Affiliation = get_affiliation(JID, StateData),
    {xmlelement, "item",
     [{"role", role_to_list(Role)},
      {"affiliation", affiliation_to_list(Affiliation)},
      {"nick", Nick},
      {"jid", jlib:jid_to_string(JID)}],
     []}.

search_role(Role, StateData) ->
    lists:filter(
      fun({_, #user{role = R}}) ->
	      Role == R
      end, ?DICT:to_list(StateData#state.users)).

search_affiliation(Affiliation, StateData) ->
    lists:filter(
      fun({_, A}) ->
	      case A of
		  {A1, _Reason} ->
		      Affiliation == A1;
		  _ ->
		      Affiliation == A
	      end
      end, ?DICT:to_list(StateData#state.affiliations)).


process_admin_items_set(UJID, Items, Lang, StateData) ->
    UAffiliation = get_affiliation(UJID, StateData),
    URole = get_role(UJID, StateData),
    case find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, []) of
	{result, Res} ->
	    ?INFO_MSG("Processing MUC admin query from ~s in room ~s:~n ~p",
		      [jlib:jid_to_string(UJID), jlib:jid_to_string(StateData#state.jid), Res]),
	    NSD =
		lists:foldl(
		  fun(E, SD) ->
			  case catch (
				 case E of
				     {JID, affiliation, owner, _} 
				     when (JID#jid.luser == "") ->
					 %% If the provided JID does not have username,
					 %% forget the affiliation completely
					 SD;
				     {JID, role, none, Reason} ->
					 catch send_kickban_presence(
						 JID, Reason, "307", SD),
					 set_role(JID, none, SD);
				     {JID, affiliation, none, Reason} ->
					 case (SD#state.config)#config.members_only of
					     true ->
						 catch send_kickban_presence(
							 JID, Reason, "321", SD),
						 SD1 = set_affiliation(JID, none, SD),
						 set_role(JID, none, SD1);
					     _ ->
						 SD1 = set_affiliation(JID, none, SD),
						 send_update_presence(JID, SD1),
						 SD1
					 end;
				     {JID, affiliation, outcast, Reason} ->
					 catch send_kickban_presence(
						 JID, Reason, "301", SD),
					 set_affiliation_and_reason(
					   JID, outcast, Reason,
					   set_role(JID, none, SD));
				     {JID, affiliation, A, _Reason} when
					   (A == admin) or (A == owner) ->
					 SD1 = set_affiliation(JID, A, SD),
					 SD2 = set_role(JID, moderator, SD1),
					 send_update_presence(JID, SD2),
					 SD2;
				     {JID, affiliation, member, _Reason} ->
					 SD1 = set_affiliation(
						 JID, member, SD),
					 SD2 = set_role(JID, participant, SD1),
					 send_update_presence(JID, SD2),
					 SD2;
				     {JID, role, R, _Reason} ->
					 SD1 = set_role(JID, R, SD),
					 catch send_new_presence(JID, SD1),
					 SD1;
				     {JID, affiliation, A, _Reason} ->
					 SD1 = set_affiliation(JID, A, SD),
					 send_update_presence(JID, SD1),
					 SD1
				       end
				) of
			      {'EXIT', ErrReason} ->
				  ?ERROR_MSG("MUC ITEMS SET ERR: ~p~n",
					     [ErrReason]),
				  SD;
			      NSD ->
				  NSD
			  end
		  end, StateData, Res),
	    case (NSD#state.config)#config.persistent of
		true ->
		    mod_muc:store_room(NSD#state.host, NSD#state.room,
				       make_opts(NSD));
		_ ->
		    ok
	    end,
	    {result, [], NSD};
	Err ->
	    Err
    end.


find_changed_items(_UJID, _UAffiliation, _URole, [], _Lang, _StateData, Res) ->
    {result, Res};
find_changed_items(UJID, UAffiliation, URole, [{xmlcdata, _} | Items],
		   Lang, StateData, Res) ->
    find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, Res);
find_changed_items(UJID, UAffiliation, URole,
		   [{xmlelement, "item", Attrs, _Els} = Item | Items],
		   Lang, StateData, Res) ->
    TJID = case xml:get_attr("jid", Attrs) of
	       {value, S} ->
		   case jlib:string_to_jid(S) of
		       error ->
			   ErrText = io_lib:format(
				       translate:translate(
					 Lang,
					 "JID ~s is invalid"), [S]),
			   {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
		       J ->
			   {value, J}
		   end;
	       _ ->
		   case xml:get_attr("nick", Attrs) of
		       {value, N} ->
			   case find_jid_by_nick(N, StateData) of
			       false ->
				   ErrText =
				       io_lib:format(
					 translate:translate(
					   Lang,
					   "Nickname ~s does not exist in the room"),
					 [N]),
				   {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
			       J ->
				   {value, J}
			   end;
		       _ ->
			   {error, ?ERR_BAD_REQUEST}
		   end
	   end,
    case TJID of
	{value, JID} ->
	    TAffiliation = get_affiliation(JID, StateData),
	    TRole = get_role(JID, StateData),
	    case xml:get_attr("role", Attrs) of
		false ->
		    case xml:get_attr("affiliation", Attrs) of
			false ->
			    {error, ?ERR_BAD_REQUEST};
			{value, StrAffiliation} ->
			    case catch list_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    ErrText1 =
					io_lib:format(
					  translate:translate(
					    Lang,
					    "Invalid affiliation: ~s"),
					    [StrAffiliation]),
				    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText1)};
				SAffiliation ->
				    CanChangeRA =
					case can_change_ra(
					       UAffiliation, URole,
					       TAffiliation, TRole,
					       affiliation, SAffiliation) of
					    nothing ->
						nothing;
					    true ->
						true;
					    check_owner ->
						case search_affiliation(
						       owner, StateData) of
						    [{OJID, _}] ->
							jlib:jid_remove_resource(OJID) /=
							    jlib:jid_tolower(jlib:jid_remove_resource(UJID));
						    _ ->
							true
						end;
					    _ ->
						false
					end,
				    case CanChangeRA of
					nothing ->
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, Lang, StateData,
					      Res);
					true ->
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, Lang, StateData,
					      [{jlib:jid_remove_resource(JID),
						affiliation,
						SAffiliation,
						xml:get_path_s(
						  Item, [{elem, "reason"},
							 cdata])} | Res]);
					false ->
					    {error, ?ERR_NOT_ALLOWED}
				    end
			    end
		    end;
		{value, StrRole} ->
		    case catch list_to_role(StrRole) of
			{'EXIT', _} ->
			    ErrText1 =
				io_lib:format(
				  translate:translate(
				    Lang,
				    "Invalid role: ~s"),
				  [StrRole]),
			    {error, ?ERRT_BAD_REQUEST(Lang, ErrText1)};
			SRole ->
			    CanChangeRA =
				case can_change_ra(
				       UAffiliation, URole,
				       TAffiliation, TRole,
				       role, SRole) of
				    nothing ->
					nothing;
				    true ->
					true;
				    check_owner ->
					case search_affiliation(
					       owner, StateData) of
					    [{OJID, _}] ->
						jlib:jid_remove_resource(OJID) /=
						    jlib:jid_tolower(jlib:jid_remove_resource(UJID));
					    _ ->
						true
					end;
				    _ ->
					false
			    end,
			    case CanChangeRA of
				nothing ->
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, Lang, StateData,
				      Res);
				true ->
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, Lang, StateData,
				      [{JID, role, SRole,
					xml:get_path_s(
					  Item, [{elem, "reason"},
						 cdata])} | Res]);
				_ ->
				    {error, ?ERR_NOT_ALLOWED}
			    end
		    end
	    end;
	Err ->
	    Err
    end;
find_changed_items(_UJID, _UAffiliation, _URole, _Items,
		   _Lang, _StateData, _Res) ->
    {error, ?ERR_BAD_REQUEST}.


can_change_ra(_FAffiliation, _FRole,
	      TAffiliation, _TRole,
	      affiliation, Value)
  when (TAffiliation == Value) ->
    nothing;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, TRole,
	      role, Value)
  when (TRole == Value) ->
    nothing;
can_change_ra(FAffiliation, _FRole,
	      outcast, _TRole,
	      affiliation, none)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      outcast, _TRole,
	      affiliation, member)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      outcast, _TRole,
	      affiliation, admin) ->
    true;
can_change_ra(owner, _FRole,
	      outcast, _TRole,
	      affiliation, owner) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      none, _TRole,
	      affiliation, outcast)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      none, _TRole,
	      affiliation, member)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      none, _TRole,
	      affiliation, admin) ->
    true;
can_change_ra(owner, _FRole,
	      none, _TRole,
	      affiliation, owner) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      member, _TRole,
	      affiliation, outcast)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      member, _TRole,
	      affiliation, none)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      member, _TRole,
	      affiliation, admin) ->
    true;
can_change_ra(owner, _FRole,
	      member, _TRole,
	      affiliation, owner) ->
    true;
can_change_ra(owner, _FRole,
	      admin, _TRole,
	      affiliation, _Affiliation) ->
    true;
can_change_ra(owner, _FRole,
	      owner, _TRole,
	      affiliation, _Affiliation) ->
    check_owner;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, _TRole,
	      affiliation, _Value) ->
    false;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, visitor,
	      role, none) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, visitor,
	      role, participant) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      _TAffiliation, visitor,
	      role, moderator)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, participant,
	      role, none) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, participant,
	      role, visitor) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      _TAffiliation, participant,
	      role, moderator)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      owner, moderator,
	      role, visitor) ->
    false;
can_change_ra(owner, _FRole,
	      _TAffiliation, moderator,
	      role, visitor) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      admin, moderator,
	      role, visitor) ->
    false;
can_change_ra(admin, _FRole,
	      _TAffiliation, moderator,
	      role, visitor) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      owner, moderator,
	      role, participant) ->
    false;
can_change_ra(owner, _FRole,
	      _TAffiliation, moderator,
	      role, participant) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      admin, moderator,
	      role, participant) ->
    false;
can_change_ra(admin, _FRole,
	      _TAffiliation, moderator,
	      role, participant) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, _TRole,
	      role, _Value) ->
    false.


send_kickban_presence(JID, Reason, Code, StateData) ->
    LJID = jlib:jid_tolower(JID),
    LJIDs = case LJID of
		{U, S, ""} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], StateData#state.users);
		_ ->
		    case ?DICT:is_key(LJID, StateData#state.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    lists:foreach(fun(J) ->
			  {ok, #user{nick = Nick}} =
			      ?DICT:find(J, StateData#state.users),
			  add_to_log(kickban, {Nick, Reason, Code}, StateData),
			  tab_remove_online_user(J, StateData),
			  send_kickban_presence1(J, Reason, Code, StateData)
		  end, LJIDs).

send_kickban_presence1(UJID, Reason, Code, StateData) ->
    {ok, #user{jid = _RealJID,
	       nick = Nick}} =
	?DICT:find(jlib:jid_tolower(UJID), StateData#state.users),
    Affiliation = get_affiliation(UJID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs = [{"affiliation", SAffiliation},
			   {"role", "none"}],
	      ItemEls = case Reason of
			    "" ->
				[];
			    _ ->
				[{xmlelement, "reason", [],
				  [{xmlcdata, Reason}]}]
			end,
	      Packet = {xmlelement, "presence", [{"type", "unavailable"}],
			[{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			  [{xmlelement, "item", ItemAttrs, ItemEls},
			   {xmlelement, "status", [{"code", Code}], []}]}]},
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff

process_iq_owner(From, set, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
	owner ->
	    {xmlelement, _Name, _Attrs, Els} = SubEl,
	    case xml:remove_cdata(Els) of
		[{xmlelement, "x", _Attrs1, _Els1} = XEl] ->
		    case {xml:get_tag_attr_s("xmlns", XEl),
			  xml:get_tag_attr_s("type", XEl)} of
			{?NS_XDATA, "cancel"} ->
			    {result, [], StateData};
			{?NS_XDATA, "submit"} ->
			    case {check_allowed_log_change(XEl, StateData, From),
					check_allowed_persistent_change(XEl, StateData, From)} of
					{allow, allow} -> set_config(XEl, StateData);
					_ -> {error, ?ERR_BAD_REQUEST}
				end;
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		[{xmlelement, "destroy", _Attrs1, _Els1} = SubEl1] ->
		    ?INFO_MSG("Destroyed MUC room ~s by the owner ~s", 
			      [jlib:jid_to_string(StateData#state.jid), jlib:jid_to_string(From)]),
		    destroy_room(SubEl1, StateData);
		Items ->
		    process_admin_items_set(From, Items, Lang, StateData)
	    end;
	_ ->
	    ErrText = "Owner privileges required",
	    {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
    end;

process_iq_owner(From, get, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
	owner ->
	    {xmlelement, _Name, _Attrs, Els} = SubEl,
	    case xml:remove_cdata(Els) of
		[] ->
		    get_config(Lang, StateData, From);
		[Item] ->
		    case xml:get_tag_attr("affiliation", Item) of
			false ->
			    {error, ?ERR_BAD_REQUEST};
			{value, StrAffiliation} ->
			    case catch list_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    ErrText =
					io_lib:format(
					  translate:translate(
					    Lang,
					    "Invalid affiliation: ~s"),
					  [StrAffiliation]),
				    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
				SAffiliation ->
				    Items = items_with_affiliation(
					      SAffiliation, StateData),
				    {result, Items, StateData}
			    end
		    end;
		_ ->
		    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	    end;
	_ ->
	    ErrText = "Owner privileges required",
	    {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
    end.

check_allowed_log_change(XEl, StateData, From) ->
    case lists:keymember("muc#roomconfig_enablelogging", 1,
			 jlib:parse_xdata_submit(XEl)) of
	false ->
	    allow;
	true ->
	    mod_muc_log:check_access_log(
	      StateData#state.server_host, From)
    end.

check_allowed_persistent_change(XEl, StateData, From) ->
    case lists:keymember("muc#roomconfig_persistentroom", 1,
			 jlib:parse_xdata_submit(XEl)) of
	false ->
	    allow;
	true ->
		{_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = StateData#state.access,
		acl:match_rule(StateData#state.server_host, AccessPersistent, From)
    end.

-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(BOOLXFIELD(Label, Var, Val),
	?XFIELD("boolean", Label, Var,
		case Val of
		    true -> "1";
		    _ -> "0"
		end)).

-define(STRINGXFIELD(Label, Var, Val),
	?XFIELD("text-single", Label, Var, Val)).

-define(PRIVATEXFIELD(Label, Var, Val),
	?XFIELD("text-private", Label, Var, Val)).


get_config(Lang, StateData, From) ->
    {_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = StateData#state.access,
    ServiceMaxUsers = get_service_max_users(StateData),
    Config = StateData#state.config,
    Res =
	[{xmlelement, "title", [],
	  [{xmlcdata, translate:translate(Lang, "Configuration for ") ++
	    jlib:jid_to_string(StateData#state.jid)}]},
	 {xmlelement, "field", [{"type", "hidden"},
				{"var", "FORM_TYPE"}],
	  [{xmlelement, "value", [],
	    [{xmlcdata, "http://jabber.org/protocol/muc#roomconfig"}]}]},
	 ?STRINGXFIELD("Room title",
		       "muc#roomconfig_roomname",
		       Config#config.title),
	 ?STRINGXFIELD("Room description",
		       "muc#roomconfig_roomdesc",
		       Config#config.description)
	] ++
	 case acl:match_rule(StateData#state.server_host, AccessPersistent, From) of
		allow ->
			[?BOOLXFIELD(
			 "Make room persistent",
			 "muc#roomconfig_persistentroom",
			 Config#config.persistent)];
		_ -> []
	 end ++ [
	 ?BOOLXFIELD("Make room public searchable",
		     "muc#roomconfig_publicroom",
		     Config#config.public),
	 ?BOOLXFIELD("Make participants list public",
		     "public_list",
		     Config#config.public_list),
	 ?BOOLXFIELD("Make room password protected",
		     "muc#roomconfig_passwordprotectedroom",
		     Config#config.password_protected),
	 ?PRIVATEXFIELD("Password",
			"muc#roomconfig_roomsecret",
			case Config#config.password_protected of
			    true -> Config#config.password;
			    false -> ""
			end),
	 {xmlelement, "field",
	  [{"type", "list-single"},
	   {"label", translate:translate(Lang, "Maximum Number of Occupants")},
	   {"var", "muc#roomconfig_maxusers"}],
	  [{xmlelement, "value", [], [{xmlcdata,
				       case get_max_users(StateData) of
					   N when is_integer(N) ->
					       erlang:integer_to_list(N);
					   _ -> "none"
				       end
				      }]}] ++
	  if
	      is_integer(ServiceMaxUsers) -> [];
	      true ->
		  [{xmlelement, "option",
		    [{"label", translate:translate(Lang, "No limit")}],
		    [{xmlelement, "value", [], [{xmlcdata, "none"}]}]}]
	  end ++
	  [{xmlelement, "option", [{"label", erlang:integer_to_list(N)}],
	    [{xmlelement, "value", [],
	      [{xmlcdata, erlang:integer_to_list(N)}]}]} ||
	      N <- ?MAX_USERS_DEFAULT_LIST, N =< ServiceMaxUsers]
	 },
	 {xmlelement, "field",
	  [{"type", "list-single"},
	   {"label", translate:translate(Lang, "Present real JIDs to")},
	   {"var", "muc#roomconfig_whois"}],
	  [{xmlelement, "value", [], [{xmlcdata,
				       if Config#config.anonymous ->
					       "moderators";
					  true ->
					       "anyone"
				       end}]},
	   {xmlelement, "option", [{"label", translate:translate(Lang, "moderators only")}],
	    [{xmlelement, "value", [], [{xmlcdata, "moderators"}]}]},
	   {xmlelement, "option", [{"label", translate:translate(Lang, "anyone")}],
	    [{xmlelement, "value", [], [{xmlcdata, "anyone"}]}]}]},
	 ?BOOLXFIELD("Make room members-only",
		     "muc#roomconfig_membersonly",
		     Config#config.members_only),
	 ?BOOLXFIELD("Make room moderated",
		     "muc#roomconfig_moderatedroom",
		     Config#config.moderated),
	 ?BOOLXFIELD("Default users as participants",
		     "members_by_default",
		     Config#config.members_by_default),
	 ?BOOLXFIELD("Allow users to change subject",
		     "muc#roomconfig_changesubject",
		     Config#config.allow_change_subj),
	 ?BOOLXFIELD("Allow users to send private messages",
		     "allow_private_messages",
		     Config#config.allow_private_messages),
	 ?BOOLXFIELD("Allow users to query other users",
		     "allow_query_users",
		     Config#config.allow_query_users),
	 ?BOOLXFIELD("Allow users to send invites",
		     "muc#roomconfig_allowinvites",
		     Config#config.allow_user_invites)
	] ++
	case mod_muc_log:check_access_log(
	       StateData#state.server_host, From) of
	    allow ->
		[?BOOLXFIELD(
		    "Enable logging",
		    "muc#roomconfig_enablelogging",
		    Config#config.logging)];
	    _ -> []
	end,
    {result, [{xmlelement, "instructions", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "You need an x:data capable client to configure room")}]},
	      {xmlelement, "x", [{"xmlns", ?NS_XDATA},
				 {"type", "form"}],
	       Res}],
     StateData}.



set_config(XEl, StateData) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
	invalid ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    case set_xoption(XData, StateData#state.config) of
		#config{} = Config ->
		    Res = change_config(Config, StateData),
		    {result, _, NSD} = Res,
		    add_to_log(roomconfig_change, [], NSD),
		    Res;
		Err ->
		    Err
	    end
    end.

-define(SET_BOOL_XOPT(Opt, Val),
	case Val of
	    "0" -> set_xoption(Opts, Config#config{Opt = false});
	    "false" -> set_xoption(Opts, Config#config{Opt = false});
	    "1" -> set_xoption(Opts, Config#config{Opt = true});
	    "true" -> set_xoption(Opts, Config#config{Opt = true});
	    _ -> {error, ?ERR_BAD_REQUEST}
	end).

-define(SET_NAT_XOPT(Opt, Val),
	case catch list_to_integer(Val) of
	    I when is_integer(I),
	           I > 0 ->
		set_xoption(Opts, Config#config{Opt = I});
	    _ ->
		{error, ?ERR_BAD_REQUEST}
	end).

-define(SET_STRING_XOPT(Opt, Val),
	set_xoption(Opts, Config#config{Opt = Val})).


set_xoption([], Config) ->
    Config;
set_xoption([{"muc#roomconfig_roomname", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(title, Val);
set_xoption([{"muc#roomconfig_roomdesc", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(description, Val);
set_xoption([{"muc#roomconfig_changesubject", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_change_subj, Val);
set_xoption([{"allow_query_users", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_query_users, Val);
set_xoption([{"allow_private_messages", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_private_messages, Val);
set_xoption([{"muc#roomconfig_publicroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public, Val);
set_xoption([{"public_list", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public_list, Val);
set_xoption([{"muc#roomconfig_persistentroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(persistent, Val);
set_xoption([{"muc#roomconfig_moderatedroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(moderated, Val);
set_xoption([{"members_by_default", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_by_default, Val);
set_xoption([{"muc#roomconfig_membersonly", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_only, Val);
set_xoption([{"muc#roomconfig_allowinvites", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_user_invites, Val);
set_xoption([{"muc#roomconfig_passwordprotectedroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(password_protected, Val);
set_xoption([{"muc#roomconfig_roomsecret", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(password, Val);
set_xoption([{"anonymous", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(anonymous, Val);
set_xoption([{"muc#roomconfig_whois", [Val]} | Opts], Config) ->
    case Val of
	"moderators" ->
	    ?SET_BOOL_XOPT(anonymous, "1");
	"anyone" ->
	    ?SET_BOOL_XOPT(anonymous, "0");
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end;
set_xoption([{"muc#roomconfig_maxusers", [Val]} | Opts], Config) ->
    case Val of
	"none" ->
	    ?SET_STRING_XOPT(max_users, none);
	_ ->
	    ?SET_NAT_XOPT(max_users, Val)
    end;
set_xoption([{"muc#roomconfig_enablelogging", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(logging, Val);
set_xoption([{"FORM_TYPE", _} | Opts], Config) ->
    %% Ignore our FORM_TYPE
    set_xoption(Opts, Config);
set_xoption([_ | _Opts], _Config) ->
    {error, ?ERR_BAD_REQUEST}.


change_config(Config, StateData) ->
    NSD = StateData#state{config = Config},
    case {(StateData#state.config)#config.persistent,
	  Config#config.persistent} of
	{_, true} ->
	    mod_muc:store_room(NSD#state.host, NSD#state.room, make_opts(NSD));
	{true, false} ->
	    mod_muc:forget_room(NSD#state.host, NSD#state.room);
	{false, false} ->
	    ok
    end,
    case {(StateData#state.config)#config.members_only,
          Config#config.members_only} of
	{false, true} ->
	    NSD1 = remove_nonmembers(NSD),
	    {result, [], NSD1};
	_ ->
	    {result, [], NSD}
    end.

remove_nonmembers(StateData) ->
    lists:foldl(
      fun({_LJID, #user{jid = JID}}, SD) ->
	    Affiliation = get_affiliation(JID, SD),
	    case Affiliation of
		none ->
		    catch send_kickban_presence(
			    JID, "", "322", SD),
		    set_role(JID, none, SD);
		_ ->
		    SD
	    end
      end, StateData, ?DICT:to_list(StateData#state.users)).


-define(CASE_CONFIG_OPT(Opt),
	Opt -> StateData#state{
		 config = (StateData#state.config)#config{Opt = Val}}).

set_opts([], StateData) ->
    StateData;
set_opts([{Opt, Val} | Opts], StateData) ->
    NSD = case Opt of
	      title -> StateData#state{config = (StateData#state.config)#config{title = Val}};
	      description -> StateData#state{config = (StateData#state.config)#config{description = Val}};
	      allow_change_subj -> StateData#state{config = (StateData#state.config)#config{allow_change_subj = Val}};
	      allow_query_users -> StateData#state{config = (StateData#state.config)#config{allow_query_users = Val}};
	      allow_private_messages -> StateData#state{config = (StateData#state.config)#config{allow_private_messages = Val}};
	      public -> StateData#state{config = (StateData#state.config)#config{public = Val}};
	      public_list -> StateData#state{config = (StateData#state.config)#config{public_list = Val}};
	      persistent -> StateData#state{config = (StateData#state.config)#config{persistent = Val}};
	      moderated -> StateData#state{config = (StateData#state.config)#config{moderated = Val}};
	      members_by_default -> StateData#state{config = (StateData#state.config)#config{members_by_default = Val}};
	      members_only -> StateData#state{config = (StateData#state.config)#config{members_only = Val}};
	      allow_user_invites -> StateData#state{config = (StateData#state.config)#config{allow_user_invites = Val}};
	      password_protected -> StateData#state{config = (StateData#state.config)#config{password_protected = Val}};
	      password -> StateData#state{config = (StateData#state.config)#config{password = Val}};
	      anonymous -> StateData#state{config = (StateData#state.config)#config{anonymous = Val}};
	      logging -> StateData#state{config = (StateData#state.config)#config{logging = Val}};
	      max_users ->
		  ServiceMaxUsers = get_service_max_users(StateData),
		  MaxUsers = if
				 Val =< ServiceMaxUsers -> Val;
				 true -> ServiceMaxUsers
			     end,
		  StateData#state{
		    config = (StateData#state.config)#config{
			       max_users = MaxUsers}};
	      affiliations ->
		  StateData#state{affiliations = ?DICT:from_list(Val)};
	      subject ->
		  StateData#state{subject = Val};
	      subject_author ->
		  StateData#state{subject_author = Val};
	      _ -> StateData
	  end,
    set_opts(Opts, NSD).

-define(MAKE_CONFIG_OPT(Opt), {Opt, Config#config.Opt}).

make_opts(StateData) ->
    Config = StateData#state.config,
    [
     ?MAKE_CONFIG_OPT(title),
     ?MAKE_CONFIG_OPT(description),
     ?MAKE_CONFIG_OPT(allow_change_subj),
     ?MAKE_CONFIG_OPT(allow_query_users),
     ?MAKE_CONFIG_OPT(allow_private_messages),
     ?MAKE_CONFIG_OPT(public),
     ?MAKE_CONFIG_OPT(public_list),
     ?MAKE_CONFIG_OPT(persistent),
     ?MAKE_CONFIG_OPT(moderated),
     ?MAKE_CONFIG_OPT(members_by_default),
     ?MAKE_CONFIG_OPT(members_only),
     ?MAKE_CONFIG_OPT(allow_user_invites),
     ?MAKE_CONFIG_OPT(password_protected),
     ?MAKE_CONFIG_OPT(password),
     ?MAKE_CONFIG_OPT(anonymous),
     ?MAKE_CONFIG_OPT(logging),
     ?MAKE_CONFIG_OPT(max_users),
     {affiliations, ?DICT:to_list(StateData#state.affiliations)},
     {subject, StateData#state.subject},
     {subject_author, StateData#state.subject_author}
    ].



destroy_room(DEl, StateData) ->
    lists:foreach(
      fun({_LJID, Info}) ->
	      Nick = Info#user.nick,
	      ItemAttrs = [{"affiliation", "none"},
			   {"role", "none"}],
	      Packet = {xmlelement, "presence", [{"type", "unavailable"}],
			[{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			  [{xmlelement, "item", ItemAttrs, []}, DEl]}]},
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)),
    case (StateData#state.config)#config.persistent of
	true ->
	    mod_muc:forget_room(StateData#state.host, StateData#state.room);
	false ->
	    ok
	end,
    {result, [], stop}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disco

-define(FEATURE(Var), {xmlelement, "feature", [{"var", Var}], []}).

-define(CONFIG_OPT_TO_FEATURE(Opt, Fiftrue, Fiffalse),
    case Opt of
	true ->
	    ?FEATURE(Fiftrue);
	false ->
	    ?FEATURE(Fiffalse)
    end).

process_iq_disco_info(_From, set, _Lang, _StateData) ->
    {error, ?ERR_NOT_ALLOWED};

process_iq_disco_info(_From, get, Lang, StateData) ->
    Config = StateData#state.config,
    {result, [{xmlelement, "identity",
	       [{"category", "conference"},
		{"type", "text"},
		{"name", get_title(StateData)}], []},
	      {xmlelement, "feature",
	       [{"var", ?NS_MUC}], []},
	      ?CONFIG_OPT_TO_FEATURE(Config#config.public,
				     "muc_public", "muc_hidden"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.persistent,
				     "muc_persistent", "muc_temporary"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.members_only,
				     "muc_membersonly", "muc_open"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.anonymous,
				     "muc_semianonymous", "muc_nonanonymous"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.moderated,
				     "muc_moderated", "muc_unmoderated"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.password_protected,
				     "muc_passwordprotected", "muc_unsecured")
	     ] ++ iq_disco_info_extras(Lang, StateData), StateData}.

-define(RFIELDT(Type, Var, Val),
	{xmlelement, "field", [{"type", Type}, {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(RFIELD(Label, Var, Val),
	{xmlelement, "field", [{"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

iq_disco_info_extras(Lang, StateData) ->
    Len = length(?DICT:to_list(StateData#state.users)),
    RoomDescription = (StateData#state.config)#config.description,
    [{xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "result"}],
      [?RFIELDT("hidden", "FORM_TYPE",
		"http://jabber.org/protocol/muc#roominfo"),
       ?RFIELD("Room description", "muc#roominfo_description",
	       RoomDescription),
       ?RFIELD("Number of occupants", "muc#roominfo_occupants",
	       integer_to_list(Len))
      ]}].

process_iq_disco_items(_From, set, _Lang, _StateData) ->
    {error, ?ERR_NOT_ALLOWED};

process_iq_disco_items(From, get, _Lang, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    FRole = get_role(From, StateData),
    case ((StateData#state.config)#config.public_list == true) orelse
	(FRole /= none) orelse
	(FAffiliation == admin) orelse
	(FAffiliation == owner) of
	true ->
	    UList =
		lists:map(
		  fun({_LJID, Info}) ->
			  Nick = Info#user.nick,
			  {xmlelement, "item",
			   [{"jid", jlib:jid_to_string(
				      {StateData#state.room,
				       StateData#state.host,
				       Nick})},
			    {"name", Nick}], []}
		  end,
		  ?DICT:to_list(StateData#state.users)),
	    {result, UList, StateData};
	_ ->
	    {error, ?ERR_FORBIDDEN}
    end.

get_title(StateData) ->
    case (StateData#state.config)#config.title of
	"" ->
	    StateData#state.room;
	Name ->
	    Name
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Invitation support

check_invitation(From, Els, Lang, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    CanInvite = (StateData#state.config)#config.allow_user_invites
	orelse (FAffiliation == admin) orelse (FAffiliation == owner),
    InviteEl = case xml:remove_cdata(Els) of
		   [{xmlelement, "x", _Attrs1, Els1} = XEl] ->
		       case xml:get_tag_attr_s("xmlns", XEl) of
			   ?NS_MUC_USER ->
			       ok;
			   _ ->
			       throw({error, ?ERR_BAD_REQUEST})
		       end,
		       case xml:remove_cdata(Els1) of
			   [{xmlelement, "invite", _Attrs2, _Els2} = InviteEl1] ->
			       InviteEl1;
			   _ ->
			       throw({error, ?ERR_BAD_REQUEST})
		       end;
		   _ ->
		       throw({error, ?ERR_BAD_REQUEST})
	       end,
    JID = case jlib:string_to_jid(
		 xml:get_tag_attr_s("to", InviteEl)) of
	      error ->
		  throw({error, ?ERR_JID_MALFORMED});
	      JID1 ->
		  JID1
	  end,
    case CanInvite of
	false ->
	    throw({error, ?ERR_NOT_ALLOWED});
	true ->
	    Reason =
		xml:get_path_s(
		  InviteEl,
		  [{elem, "reason"}, cdata]),
	    ContinueEl =
		case xml:get_path_s(
		       InviteEl,
		       [{elem, "continue"}]) of
		    [] -> [];
		    Continue1 -> [Continue1]
		end,
	    IEl =
		[{xmlelement, "invite",
		  [{"from",
		    jlib:jid_to_string(From)}],
		  [{xmlelement, "reason", [],
		    [{xmlcdata, Reason}]}] ++ ContinueEl}],
	    PasswdEl =
		case (StateData#state.config)#config.password_protected of
		    true ->
			[{xmlelement, "password", [],
			  [{xmlcdata, (StateData#state.config)#config.password}]}];
		    _ ->
			[]
		end,
	    Body =
		{xmlelement, "body", [],
		 [{xmlcdata,
		   lists:flatten(
		     io_lib:format(
		       translate:translate(
			 Lang,
			 "~s invites you to the room ~s"),
		       [jlib:jid_to_string(From),
			jlib:jid_to_string({StateData#state.room,
					    StateData#state.host,
					    ""})
		       ])) ++
		   case (StateData#state.config)#config.password_protected of
		       true ->
			   ", " ++
			       translate:translate(Lang, "the password is") ++
			       " '" ++
			       (StateData#state.config)#config.password ++ "'";
		       _ ->
			   ""
		   end ++
		   case Reason of
		       "" -> "";
		       _ -> " (" ++ Reason ++ ") "
		   end
		  }]},
	    Msg =
		{xmlelement, "message",
		 [{"type", "normal"}],
		 [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}], IEl ++ PasswdEl},
		  {xmlelement, "x",
		   [{"xmlns", ?NS_XCONFERENCE},
		    {"jid", jlib:jid_to_string(
			      {StateData#state.room,
			       StateData#state.host,
			       ""})}],
		   [{xmlcdata, Reason}]},
		  Body]},
	    ejabberd_router:route(StateData#state.jid, JID, Msg),
	    JID
    end.

%% Handle a message sent to the room by a non-participant.
%% If it is a decline, send to the inviter.
%% Otherwise, an error message is sent to the sender.
handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From) ->
    case catch check_decline_invitation(Packet) of
	{true, Decline_data} ->
	    send_decline_invitation(Decline_data, StateData#state.jid, From);
	_ ->
	    send_error_only_occupants(Packet, Lang, StateData#state.jid, From)
    end.

%% Check in the packet is a decline.
%% If so, also returns the splitted packet.
%% This function must be catched, 
%% because it crashes when the packet is not a decline message.
check_decline_invitation(Packet) ->
    {xmlelement, "message", _, _} = Packet,
    XEl = xml:get_subtag(Packet, "x"),
    ?NS_MUC_USER = xml:get_tag_attr_s("xmlns", XEl),
    DEl = xml:get_subtag(XEl, "decline"),
    ToString = xml:get_tag_attr_s("to", DEl),
    ToJID = jlib:string_to_jid(ToString),
    {true, {Packet, XEl, DEl, ToJID}}.

%% Send the decline to the inviter user.
%% The original stanza must be slightly modified.
send_decline_invitation({Packet, XEl, DEl, ToJID}, RoomJID, FromJID) ->
    FromString = jlib:jid_to_string(FromJID),
    {xmlelement, "decline", DAttrs, DEls} = DEl,
    DAttrs2 = lists:keydelete("to", 1, DAttrs),
    DAttrs3 = [{"from", FromString} | DAttrs2],
    DEl2 = {xmlelement, "decline", DAttrs3, DEls},
    XEl2 = replace_subelement(XEl, DEl2),
    Packet2 = replace_subelement(Packet, XEl2),
    ejabberd_router:route(RoomJID, ToJID, Packet2).

%% Given an element and a new subelement, 
%% replace the instance of the subelement in element with the new subelement.
replace_subelement({xmlelement, Name, Attrs, SubEls}, NewSubEl) ->
    {_, NameNewSubEl, _, _} = NewSubEl,
    SubEls2 = lists:keyreplace(NameNewSubEl, 2, SubEls, NewSubEl),
    {xmlelement, Name, Attrs, SubEls2}.

send_error_only_occupants(Packet, Lang, RoomJID, From) ->
    ErrText = "Only occupants are allowed to send messages to the conference",
    Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
    ejabberd_router:route(RoomJID, From, Err).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging

add_to_log(Type, Data, StateData) ->
    case (StateData#state.config)#config.logging of
	true ->
	    mod_muc_log:add_to_log(
	      StateData#state.server_host, Type, Data,
	      StateData#state.jid, make_opts(StateData));
	false ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Users number checking

tab_add_online_user(JID, StateData) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:insert(
	    muc_online_users,
	    #muc_online_users{us = US, room = Room, host = Host}).


tab_remove_online_user(JID, StateData) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:delete_object(
	    muc_online_users,
	    #muc_online_users{us = US, room = Room, host = Host}).

tab_count_user(JID) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    case catch ets:select(
		 muc_online_users,
		 [{#muc_online_users{us = US, _ = '_'}, [], [[]]}]) of
	Res when is_list(Res) ->
	    length(Res);
	_ ->
	    0
    end.
