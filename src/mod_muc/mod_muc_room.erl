%%%----------------------------------------------------------------------
%%% File    : mod_muc_room.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC room stuff
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

-define(GEN_FSM, p1_fsm).


%% External exports
-export([start_link/9,
	 start_link/7,
	 start_link/2,
	 start/9,
	 start/7,
	 start/2,
	 migrate/3,
	 route/4]).

%% gen_fsm callbacks
-export([init/1,
	 normal_state/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 print_state/1,
	 code_change/4]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_muc_room.hrl").
-include("jlib.hrl"). %% Used for captcha

-define(MAX_USERS_DEFAULT_LIST,
	[5, 10, 20, 30, 50, 100, 200, 500, 1000, 2000, 5000]).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START(Args), 
	?GEN_FSM:start(?MODULE, Args, ?FSMOPTS)).
-else.
-define(SUPERVISOR_START(Args), 
	Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
	supervisor:start_child(Supervisor, Args)).
-endif.

-define(ERR(Packet,Type, Lang, ErrText),
    exmpp_stanza:error(Packet#xmlel.ns,
                                   Type,
                                   {Lang, translate:translate(Lang, ErrText)})).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
      Creator, Nick, DefRoomOpts) ->
    ?SUPERVISOR_START([Host, ServerHost, Access, Room, HistorySize,
		       RoomShaper, Creator, Nick, DefRoomOpts]).

start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    supervisor:start_child(
      Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,
		   Opts]).

start(StateName, StateData) ->
    ServerHost = StateData#state.server_host,
    ?SUPERVISOR_START([StateName, StateData]).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
	   Creator, Nick, DefRoomOpts) ->
    ?GEN_FSM:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				  RoomShaper, Creator, Nick, DefRoomOpts],
			?FSMOPTS).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    ?GEN_FSM:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				  RoomShaper, Opts],
			?FSMOPTS).

start_link(StateName, StateData) ->
    ?GEN_FSM:start_link(?MODULE, [StateName, StateData], ?FSMOPTS).

migrate(FsmRef, Node, After) ->
    ?GEN_FSM:send_all_state_event(FsmRef, {migrate, Node, After}).

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
				   jid = exmpp_jid:make(Room, Host),
				   just_created = true,
				   room_shaper = Shaper}),
    State1 = set_opts(DefRoomOpts, State),
    ?INFO_MSG("Created MUC room ~s@~s by ~s", 
	      [Room, Host, exmpp_jid:to_binary(Creator)]),
    add_to_log(room_existence, created, State1),
    add_to_log(room_existence, started, State1),
    {ok, normal_state, State1};
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_opts(Opts, #state{host = Host,
				  server_host = ServerHost,
				  access = Access,
				  room = Room,
				  history = lqueue_new(HistorySize),
				  jid = exmpp_jid:make(Room, Host),
				  room_shaper = Shaper}),
    add_to_log(room_existence, started, State),
    {ok, normal_state, State};
init([StateName, #state{room = Room, host = Host} = StateData]) ->
    process_flag(trap_exit, true),
    mod_muc:register_room(Host, Room, self()),
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
normal_state({route, From, undefined,
	      #xmlel{name = 'message'} = Packet},
	     StateData) ->
    Lang = exmpp_stanza:get_lang(Packet),
    case is_user_online(From, StateData) orelse
	is_user_allowed_message_nonparticipant(From, StateData) of
	true ->
	    case exmpp_message:get_type(Packet) of
		groupchat ->
		    Activity = get_user_activity(From, StateData),
		    Now = now_to_usec(now()),
		    MinMessageInterval =
			trunc(gen_mod:get_module_opt(
				StateData#state.server_host,
				mod_muc, min_message_interval, 0) * 1000000),
		    Size = erlang:iolist_size(exmpp_xml:document_to_iolist(Packet)),
		    {MessageShaper, MessageShaperInterval} =
			shaper:update(Activity#activity.message_shaper, Size),
		    if
			Activity#activity.message /= undefined ->
			    ErrText = "Traffic rate limit is exceeded",
			    Err = exmpp_stanza:error(Packet#xmlel.ns,
			      'resource-constraint',
			      {Lang, translate:translate(Lang, ErrText)}),
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, exmpp_stanza:reply_with_error(Packet, Err)),
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
					store_user_activity(
					  From, NewActivity, StateData),
				    StateData2 =
					StateData1#state{
					  room_shaper = RoomShaper},
				    process_groupchat_message(From, Packet, StateData2);
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
				    RoomQueue =
                                        queue:in({message, From},
                                                 StateData#state.room_queue),
				    StateData2 =
					store_user_activity(
					  From, NewActivity, StateData1),
				    StateData3 =
					StateData2#state{
					  room_queue = RoomQueue},
				    {next_state, normal_state, StateData3}
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
				store_user_activity(
				  From, NewActivity, StateData),
			    {next_state, normal_state, StateData1}
		    end;
		error ->
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
		chat ->
		    ErrText = "It is not allowed to send private messages to the room",
		    Err = exmpp_stanza:error(Packet#xmlel.ns,
		      'not-acceptable',
		      {Lang, translate:translate(Lang, ErrText)}),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, exmpp_stanza:reply_with_error(Packet, Err)),
		    {next_state, normal_state, StateData};
        %%TODO: currently exmpp_message:get_type/1 never returns 'undefined'
		Type when (Type == 'normal') orelse (Type == 'undefined') ->
		    case catch check_invitation(From, 
			  exmpp_xml:get_child_elements(Packet), 
			  Lang, 
			  StateData) of
			{error, Error} ->
			    Err = exmpp_stanza:reply_with_error(Packet, Error),
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
		    Err = exmpp_stanza:error(Packet#xmlel.ns,
		      'not-acceptable',
		      {Lang, translate:translate(Lang, ErrText)}),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, exmpp_stanza:reply_with_error(Packet, Err)),
		    {next_state, normal_state, StateData}
	    end;
	_ ->
	    case exmpp_stanza:is_stanza_error(Packet) of
		true ->
		    ok;
		false ->
		    handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From)
	    end,
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, undefined,
	      #xmlel{name = 'iq'} = Packet},
	     StateData) ->
    case exmpp_iq:xmlel_to_iq(Packet) of
	#iq{kind = request, type = Type, ns = XMLNS, lang = Lang, payload = SubEl} = IQ when
	      (XMLNS == ?NS_MUC_ADMIN) or
	      (XMLNS == ?NS_MUC_OWNER) or
	      (XMLNS == ?NS_DISCO_INFO) or
	      (XMLNS == ?NS_DISCO_ITEMS) or
	      (XMLNS == ?NS_CAPTCHA) ->
	    Res1 = case XMLNS of
		       ?NS_MUC_ADMIN ->
			   process_iq_admin(From, Type, Lang, SubEl, StateData);
		       ?NS_MUC_OWNER ->
			   process_iq_owner(From, Type, Lang, SubEl, StateData);
		       ?NS_DISCO_INFO ->
			   process_iq_disco_info(From, Type, Lang, StateData);
		       ?NS_DISCO_ITEMS ->
			   process_iq_disco_items(From, Type, Lang, StateData);
		       ?NS_CAPTCHA ->
			   process_iq_captcha(From, Type, Lang, SubEl, StateData)
		   end,
	    {IQRes, NewStateData} =
		case Res1 of
		    {result, [], SD} ->
			{exmpp_iq:result(IQ), SD};
		    {result, Res, SD} ->
			{exmpp_iq:result(IQ,#xmlel{ns = XMLNS, 
			      name = 'query', 
			      children = Res}), SD};
		    {error, Error} ->
			{exmpp_iq:error(IQ, Error), StateData}
		end,
	    ejabberd_router:route(StateData#state.jid,
				  From,
				  exmpp_iq:iq_to_xmlel(IQRes)),
	    case NewStateData of
		stop ->
		    {stop, normal, StateData};
		_ ->
		    {next_state, normal_state, NewStateData}
	    end;
	#iq{kind = response} ->
	    {next_state, normal_state, StateData};
	_ ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'feature-not-implemented'),
	    ejabberd_router:route(StateData#state.jid, From, Err),
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, Nick,
	      #xmlel{name = 'presence'} = Packet},
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
	    StateData1 = store_user_activity(From, NewActivity, StateData),
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
	    StateData1 = store_user_activity(From, NewActivity, StateData),
	    {next_state, normal_state, StateData1}
    end;

normal_state({route, From, ToNick,
	      #xmlel{name = 'message'} = Packet},
	     StateData) ->
    Type = exmpp_message:get_type(Packet),
    Lang = exmpp_stanza:get_lang(Packet),
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
			'groupchat' ->
			    ErrText = "It is not allowed to send private "
				"messages of type \"groupchat\"",
			    Err = exmpp_stanza:reply_with_error(Packet,
			      exmpp_stanza:error(Packet#xmlel.ns, 'bad-request',
				{Lang, translate:translate(Lang, ErrText)})),
			    ejabberd_router:route(
			      jid_replace_resource(
				StateData#state.jid,
				ToNick),
			      From, Err);
			_ ->
			    ToJIDs = find_jids_by_nick(ToNick, StateData),
			    case ToJIDs of
				false ->
				    ErrText = "Recipient is not in the conference room",
				    Err = exmpp_stanza:reply_with_error(Packet,
									exmpp_stanza:error(Packet#xmlel.ns, 'item-not-found',
											   {Lang, translate:translate(Lang, ErrText)})),
				    ejabberd_router:route(
				      jid_replace_resource(
					StateData#state.jid,
					ToNick),
				      From, Err);
				_ ->
				    SrcIsVisitor = is_visitor(From, StateData),
				    DstIsModerator = is_moderator(hd(ToJIDs), StateData),
				    PmFromVisitors = (StateData#state.config)#config.allow_private_messages_from_visitors,
				    if SrcIsVisitor == false;
				       PmFromVisitors == anyone;
				       (PmFromVisitors == moderators) and (DstIsModerator) ->
					    {ok, #user{nick = FromNick}} =
						?DICT:find(jlib:jid_tolower(From),
							   StateData#state.users),
					      FromNickJID = jid_replace_resource(StateData#state.jid, FromNick),
					    [ejabberd_router:route(FromNickJID, ToJID, Packet) || ToJID <- ToJIDs];
				       true ->
					    ErrText = "It is not allowed to send private messages",
					    Err = exmpp_stanza:reply_with_error(Packet,
					      exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
						{Lang, translate:translate(Lang, ErrText)})),
					    ejabberd_router:route(
					      jid_replace_resource(
						StateData#state.jid,
						ToNick),
					      From, Err)

				    end
			    end
		    end;
		{true, false} ->
		    ErrText = "Only occupants are allowed to send messages to the room",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'not-acceptable',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      jid_replace_resource(
			StateData#state.jid,
			ToNick),
		      From, Err);
		{false, _} ->
		    ErrText = "It is not allowed to send private messages",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      jid_replace_resource(
			StateData#state.jid,
			ToNick),
		      From, Err)
	    end,
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, ToNick,
	      #xmlel{name = 'iq'} = Packet},
	     StateData) ->
    Lang = exmpp_stanza:get_lang(Packet),
    StanzaId = get_stanza_id(Packet),
    case {(StateData#state.config)#config.allow_query_users,
	  is_user_online_iq(StanzaId, From, StateData)} of
	{true, {true, NewId, FromFull}} ->
	    case find_jid_by_nick(ToNick, StateData) of
		false ->
		    case exmpp_iq:get_type(Packet) of
			result ->
			    ok;
			error ->
			    ok;
			_ ->
			    ErrText = "Recipient is not in the room",
			    Err = exmpp_stanza:reply_with_error(Packet,
			      exmpp_stanza:error(Packet#xmlel.ns, 'item-not-found',
				{Lang, translate:translate(Lang, ErrText)})),
			    ejabberd_router:route(
			      jid_replace_resource(
				StateData#state.jid, ToNick),
			      From, Err)
		    end;
		ToJID ->
		    {ok, #user{nick = FromNick}} =
			?DICT:find(jlib:short_prepd_jid(FromFull),
				   StateData#state.users),
		    {ToJID2, Packet2} = handle_iq_vcard(FromFull, ToJID,
							StanzaId, NewId,Packet),
		    ejabberd_router:route(
		      jid_replace_resource(StateData#state.jid, FromNick),
		      ToJID2, Packet2)
	    end;
	{_, {false, _, _}} ->
	    case exmpp_iq:get_type(Packet) of
		result ->
		    ok;
		error ->
		    ok;
		_ ->
		    ErrText = "Only occupants are allowed to send queries to the room",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'not-acceptable',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      jid_replace_resource(StateData#state.jid, ToNick),
		      From, Err)
	    end;
	_ ->
	    case exmpp_iq:get_type(Packet) of
		result ->
		    ok;
		error ->
		    ok;
		_ ->
		    ErrText = "Queries to the room members are not allowed in this room",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'not-allowed',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      jid_replace_resource(StateData#state.jid, ToNick),
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
    MessagePkt = #xmlel{name = 'message',
      attrs = [?XMLATTR(<<"type">>, <<"groupchat">>)],
      children = [#xmlel{name = 'body',
	  children = [#xmlcdata{cdata = Msg}]}]},
    send_multiple(
      StateData#state.jid,
      StateData#state.server_host,
      StateData#state.users,
      MessagePkt),
    NSD = add_message_to_history("",
				 StateData#state.jid,
				 MessagePkt,
				 StateData),
    {next_state, normal_state, NSD};

handle_event({destroy, Reason}, _StateName, StateData) ->
    {result, [], stop} =
    destroy_room(
      #xmlel{ns = ?NS_MUC_OWNER, name = 'destroy',
	children = case Reason of
	    none -> [];
	    _Else -> [#xmlel{name = 'reason',
		    children = [#xmlcdata{cdata = Reason}]}]
	end}, StateData),

    ?INFO_MSG("Destroyed MUC room ~s with reason: ~p", 
	      [exmpp_jid:to_binary(StateData#state.jid), Reason]),
    add_to_log(room_existence, destroyed, StateData),
    {stop, shutdown, StateData};
handle_event(destroy, StateName, StateData) ->
    ?INFO_MSG("Destroyed MUC room ~s", 
	      [exmpp_jid:to_binary(StateData#state.jid)]),
    handle_event({destroy, none}, StateName, StateData);

handle_event({set_affiliations, Affiliations}, StateName, StateData) ->
    {next_state, StateName, StateData#state{affiliations = Affiliations}};

handle_event({migrate, Node, After}, StateName, StateData) when Node /= node() ->
    {migrate, StateData,
     {Node, ?MODULE, start, [StateName, StateData]}, After * 2};
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
    Reply = get_roomdesc_reply(JID, StateData,
			       get_roomdesc_tail(StateData, Lang)),
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

print_state(StateData) ->
    StateData.

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
		store_user_activity(
		  From, NewActivity, StateData),
	    StateData2 =
		StateData1#state{
		  room_queue = RoomQueue},
	    StateData3 = prepare_room_queue(StateData2),
	    process_groupchat_message(From, Packet, StateData3);
	{{value, {presence, From}}, RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    {Nick, Packet} = Activity#activity.presence,
	    NewActivity = Activity#activity{presence = undefined},
	    StateData1 =
		store_user_activity(
		  From, NewActivity, StateData),
	    StateData2 =
		StateData1#state{
		  room_queue = RoomQueue},
	    StateData3 = prepare_room_queue(StateData2),
	    process_presence(From, Nick, Packet, StateData3);
	{empty, _} ->
	    {next_state, StateName, StateData}
    end;
handle_info({captcha_succeed, From}, normal_state, StateData) ->
    NewState = case ?DICT:find(From, StateData#state.robots) of
		   {ok, {Nick, Packet}} ->
		       Robots = ?DICT:store(From, passed, StateData#state.robots),
		       add_new_user(From, Nick, Packet, StateData#state{robots=Robots});
		   _ ->
		       StateData
	       end,
    {next_state, normal_state, NewState};
handle_info({captcha_failed, From}, normal_state, StateData) ->
    NewState = case ?DICT:find(From, StateData#state.robots) of
		   {ok, {Nick, Packet}} ->
		       Robots = ?DICT:erase(From, StateData#state.robots),
		       Err = exmpp_stanza:reply_with_error(
			    Packet, ?ERR(Packet, 'not-authorized', undefined, "")),
		       ejabberd_router:route( % TODO: s/Nick/""/
			 jid_replace_resource(
			   StateData#state.jid, Nick),
			 From, Err),
		       StateData#state{robots=Robots};
		   _ ->
		       StateData
	       end,
    {next_state, normal_state, NewState};
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate({migrated, Clone}, _StateName, StateData) ->
    ?INFO_MSG("Migrating room ~s@~s to ~p on node ~p",
	      [StateData#state.room, StateData#state.host,
	       Clone, node(Clone)]),
    mod_muc:room_destroyed(StateData#state.host, StateData#state.room,
			   self(), StateData#state.server_host),
    ok;
terminate(Reason, _StateName, StateData) ->
    ?INFO_MSG("Stopping MUC room ~s@~s",
	      [StateData#state.room, StateData#state.host]),
    ReasonT = case Reason of
		  shutdown -> <<"You are being removed from the room because"
				  " of a system shutdown">>;
		  _ -> <<"Room terminates">>
	      end,
    ReasonEl = #xmlel{name = 'reason', children = [#xmlcdata{cdata = ReasonT}]},
    ItemAttrs = [?XMLATTR(<<"affiliation">>, <<"none">>),
		 ?XMLATTR(<<"role">>, <<"none">>)],
    XEls = [#xmlel{ns = ?NS_MUC_USER, name = 'item',
		   attrs = ItemAttrs,
		   children = [ReasonEl]},
	    #xmlel{ns = ?NS_MUC_USER, name = 'status',
		   attrs = [?XMLATTR(<<"code">>, <<"332">>)]}],
    Packet = #xmlel{ns = ?NS_JABBER_CLIENT,
                    name = 'presence',
                    attrs = [?XMLATTR(<<"type">>, <<"unavailable">>)],
                    children = [#xmlel{ns = ?NS_MUC_USER, name = 'x',
				       children = XEls}
			       ]},
    ?DICT:fold(
       fun(LJID, Info, _) ->
	       Nick = Info#user.nick,
	       case Reason of
		   shutdown ->
		       ejabberd_router:route(
			 exmpp_jid:full(StateData#state.jid, Nick),
			 Info#user.jid,
			 Packet);
		   _ -> ok
	       end,
	       tab_remove_online_user(LJID, StateData)
       end, [], StateData#state.users),
    add_to_log(room_existence, stopped, StateData),
    mod_muc:room_destroyed(StateData#state.host, StateData#state.room, self(),
			   StateData#state.server_host),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

route(Pid, From, ToNick, Packet) ->
    ?GEN_FSM:send_event(Pid, {route, From, ToNick, Packet}).

process_groupchat_message(From, #xmlel{name = 'message'} = Packet,
			  StateData) ->
    Lang = exmpp_stanza:get_lang(Packet),
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
			    send_multiple(
			      jid_replace_resource(StateData#state.jid, FromNick),
			      StateData#state.server_host,
			      StateData#state.users,
			      Packet),
			    NewStateData2 =
				add_message_to_history(FromNick,
						       From,
						       Packet,
						       NewStateData1),
			    {next_state, normal_state, NewStateData2};
			_ ->
			    Err =
				case (StateData#state.config)#config.allow_change_subj of
				    true ->
					exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
					    {Lang, translate:translate(Lang,
						"Only moderators and participants "
						"are allowed to change the subject in this room")}));
				    _ ->
					exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
					    {Lang, translate:translate(Lang,
						"Only moderators "
						"are allowed to change the subject in this room")}))
				end,
			    ejabberd_router:route(
			      StateData#state.jid,
			      From,
			      Err),
			    {next_state, normal_state, StateData}
		    end;
		true ->
		    ErrText = "Visitors are not allowed to send messages to all occupants",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, Err),
		    {next_state, normal_state, StateData}
	    end;
	false ->
	    ErrText = "Only occupants are allowed to send messages to the room",
	    Err = exmpp_stanza:reply_with_error(Packet,
	      exmpp_stanza:error(Packet#xmlel.ns, 'not-acceptable',
		{Lang, translate:translate(Lang, ErrText)})),
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
is_user_allowed_message_nonparticipant(JID, StateData) ->
    case get_service_affiliation(JID, StateData) of
	owner ->
	    true;
	_ -> false
    end.

%% @doc Get information of this participant, or default values.
%% If the JID is not a participant, return values for a service message.
get_participant_data(From, StateData) ->
    case ?DICT:find(jlib:short_prepd_jid(From), StateData#state.users) of
	{ok, #user{nick = FromNick, role = Role}} ->
	    {FromNick, Role};
	error ->
	    {<<>>, moderator}
    end.

%% Check if the user is occupant of the room, or at least is an admin or owner.
is_occupant_or_admin(JID, StateData) ->
    FAffiliation = get_affiliation(JID, StateData),
    FRole = get_role(JID, StateData),
    case (FRole /= none) orelse
	(FAffiliation == admin) orelse
	(FAffiliation == owner) of
        true ->
	    true;
        _ ->
	    false
    end.

process_presence(From, Nick, #xmlel{name = 'presence'} = Packet,
		 StateData) ->
    Type = exmpp_presence:get_type(Packet),
    Lang = exmpp_stanza:get_lang(Packet),
    StateData1 =
	case Type of
	    unavailable ->
		case is_user_online(From, StateData) of
		    true ->
			NewState =
			    add_user_presence_un(From, Packet, StateData),
			case ?DICT:find(Nick, StateData#state.nicks) of
			    {ok, [_, _ | _]} -> ok;
			    _ -> send_new_presence(From, NewState)
			end,
			Reason = case exmpp_xml:get_element(Packet, 'status') of
				undefined -> <<>>;
				Status_el -> exmpp_xml:get_cdata(Status_el)
			end,
			remove_online_user(From, NewState, Reason);
		    _ ->
			StateData
		end;
	    error ->
		case is_user_online(From, StateData) of
		    true ->
			ErrorText = "This participant is kicked from the room because "
			    "he sent an error presence",
			expulse_participant(Packet, From, StateData,
					    translate:translate(Lang, ErrorText));
		    _ ->
			StateData
		end;
	    'available' ->
		case is_user_online(From, StateData) of
		    true ->
			case is_nick_change(From, Nick, StateData) of
			    true ->
				case {nick_collision(From, Nick, StateData),
				      mod_muc:can_use_nick(
					     StateData#state.host, From, Nick),
				      {(StateData#state.config)#config.allow_visitor_nickchange,
					is_visitor(From, StateData)}} of
				    {_, _, {false, true}} ->
					ErrText = "Visitors are not allowed to change their nicknames in this room",
					Err = exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'not-allowed',
					    {Lang, translate:translate(Lang, ErrText)})),
					ejabberd_router:route(
					  % TODO: s/Nick/""/
					  jid_replace_resource(
					    StateData#state.jid,
					    Nick),
					  From, Err),
					StateData;
				    {true, _, _} ->
					Lang = exmpp_stanza:get_lang(Packet),
					ErrText = "That nickname is already in use by another occupant",
					Err = exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'conflict',
					    {Lang, translate:translate(Lang, ErrText)})),
					ejabberd_router:route(
					  jid_replace_resource(
					    StateData#state.jid,
					    Nick), % TODO: s/Nick/""/
					  From, Err),
					StateData;
				    {_, false, _} ->
					ErrText = "That nickname is registered by another person",
					Err = exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'conflict',
					    {Lang, translate:translate(Lang, ErrText)})),
					ejabberd_router:route(
					  % TODO: s/Nick/""/
					  jid_replace_resource(
					    StateData#state.jid,
					    Nick),
					  From, Err),
					StateData;
				    _ ->
					change_nick(From, Nick, StateData)
				end;
			    _NotNickChange ->
                                Stanza = case {(StateData#state.config)#config.allow_visitor_status,
                                               is_visitor(From, StateData)} of
                                             {false, true} ->
                                                 strip_status(Packet);
                                             _Allowed ->
                                                 Packet
                                         end,
                                NewState = add_user_presence(From, Stanza, StateData),
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
		      [exmpp_jid:to_binary(StateData#state.jid)]),
	    add_to_log(room_existence, destroyed, StateData),
	    {stop, normal, StateData1};
	_ ->
	    {next_state, normal_state, StateData1}
    end.

is_user_online(JID, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    ?DICT:is_key(LJID, StateData#state.users).

%%%
%%% Handle IQ queries of vCard
%%%
get_stanza_id(Packet) ->
    case exmpp_stanza:get_id(Packet) of
	Id when is_binary(Id) -> binary_to_list(Id);
	undefined -> ""
    end.

is_user_online_iq(StanzaId, JID, StateData) ->
    case exmpp_jid:resource(JID) of
	undefined -> is_user_online_iq2(StanzaId, JID, StateData);
	_ -> {is_user_online(JID, StateData), StanzaId, JID}
    end.

is_user_online_iq2(StanzaId, JID, StateData) ->
    try stanzaid_unpack(StanzaId) of
	{OriginalId, Resource} ->
	    JIDWithResource = jid_replace_resource(JID, Resource),
	    {is_user_online(JIDWithResource, StateData),
	     OriginalId, JIDWithResource}
    catch
	_:_ ->
	    {is_user_online(JID, StateData), StanzaId, JID}
    end.

handle_iq_vcard(FromFull, ToJID, StanzaId, NewId, Packet) ->
    ToBareJID = exmpp_jid:bare(ToJID),
    IsToJIDBare = exmpp_jid:full_compare(ToJID, ToBareJID),
    IQ = exmpp_iq:xmlel_to_iq(Packet),
    handle_iq_vcard2(FromFull, ToJID, ToBareJID, IsToJIDBare, StanzaId, NewId, IQ, Packet).
handle_iq_vcard2(_FromFull, ToJID, ToBareJID, IsToJIDBare, StanzaId, _NewId,
		 #iq{type = get, ns = ?NS_VCARD}, Packet)
  when IsToJIDBare == false ->
    {ToBareJID, change_stanzaid(StanzaId, ToJID, Packet)};
handle_iq_vcard2(_FromFull, ToJID, _ToBareJID, _IsToJIDBare, _StanzaId, NewId, _IQ, Packet) ->
    {ToJID, change_stanzaid(NewId, Packet)}.

stanzaid_pack(OriginalId, Resource) ->
    "berd"++base64:encode_to_string("ejab\0" ++ OriginalId ++ "\0" ++ Resource).
stanzaid_unpack("berd"++StanzaIdBase64) ->
    StanzaId = base64:decode_to_string(StanzaIdBase64),
    ["ejab", OriginalId, Resource] = string:tokens(StanzaId, "\0"),
    {OriginalId, Resource}.

change_stanzaid(NewId, Packet) ->
    exmpp_stanza:set_id(Packet, NewId).
change_stanzaid(PreviousId, ToJID, Packet) ->
    NewId = stanzaid_pack(PreviousId, exmpp_jid:resource_as_list(ToJID)),
    change_stanzaid(NewId, Packet).
%%%
%%%

role_to_binary(Role) ->
    case Role of
	moderator ->   <<"moderator">>;
	participant -> <<"participant">>;
	visitor ->     <<"visitor">>;
	none ->        <<"none">>
    end.

affiliation_to_binary(Affiliation) ->
    case Affiliation of
	owner ->   <<"owner">>;
	admin ->   <<"admin">>;
	member ->  <<"member">>;
	outcast -> <<"outcast">>;
	none ->    <<"none">>
    end.

binary_to_role(Role) ->
    case Role of
	<<"moderator">> ->   moderator;
	<<"participant">> -> participant;
	<<"visitor">> ->     visitor;
	<<"none">> ->        none
    end.

binary_to_affiliation(Affiliation) ->
    case Affiliation of
	<<"owner">> ->   owner;
	<<"admin">> ->   admin;
	<<"member">> ->  member;
	<<"outcast">> -> outcast;
	<<"none">> ->    none
    end.

%% Decide the fate of the message and its sender
%% Returns: continue_delivery | forget_message | {expulse_sender, Reason}
decide_fate_message(error, Packet, From, StateData) ->
    %% Make a preliminary decision
    PD = case check_error_kick(Packet) of
	     %% If this is an error stanza and its condition matches a criteria
	     true ->
		 Reason = io_lib:format("This participant is considered a ghost and is expulsed: ~s",
					[exmpp_jid:to_binary(From)]),
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
	'gone' -> true;
	'internal-server-error' -> true;
	'item-not-found' -> true;
	'jid-malformed' -> true;
	'recipient-unavailable' -> true;
	'redirect' -> true;
	'remote-server-not-found' -> true;
	'remote-server-timeout' -> true;
	'service-unavailable' -> true;
	_ -> false
    end.

get_error_condition(Packet) ->
	try exmpp_stanza:get_condition(Packet) of
	      ErrorCondition -> ErrorCondition
    catch
	     _:_ ->
		'badformed-error-stanza'
	end.

expulse_participant(Packet, From, StateData, Reason1) ->
	ErrorCondition = get_error_condition(Packet),
	Reason2 = io_lib:format(Reason1 ++ ": " ++ "~s", [ErrorCondition]),
	NewState = add_user_presence_un(
		From,
        exmpp_presence:presence('unavailable',Reason2),
	StateData),
	send_new_presence(From, NewState),
	remove_online_user(From, NewState).


set_affiliation(JID, Affiliation, StateData) ->
    LJID = jlib:short_prepd_bare_jid(JID),
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

set_affiliation_and_reason(JID, Affiliation, Reason, StateData)
  when Affiliation /= none ->
    LJID = jlib:short_prepd_bare_jid(JID),
    Affiliations = ?DICT:store(LJID, {Affiliation, Reason},
			       StateData#state.affiliations),
    StateData#state{affiliations = Affiliations}.

get_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} = StateData#state.access,
    Res =
	case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
	    allow ->
		owner;
	    _ ->
		LJID = jlib:short_prepd_jid(JID),
		case ?DICT:find(LJID, StateData#state.affiliations) of
		    {ok, Affiliation} ->
			Affiliation;
		    _ ->
            LJID1 = jlib:short_prepd_bare_jid(JID),
			case ?DICT:find(LJID1, StateData#state.affiliations) of
			    {ok, Affiliation} ->
				Affiliation;
			    _ ->
				LJID2 = setelement(1, LJID, undefined),
				case ?DICT:find(LJID2, StateData#state.affiliations) of
				    {ok, Affiliation} ->
					Affiliation;
				    _ ->
                    LJID3 = setelement(1,jlib:short_prepd_bare_jid(JID),undefined),
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
    LJID = jlib:short_prepd_jid(JID),
    LJIDs = case LJID of
		{U, S, undefined} ->
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
    {Users, Nicks}
	= case Role of
	      none ->
		  lists:foldl(fun(J, {Us, Ns}) ->
				      NewNs =
					  case ?DICT:find(J, Us) of
					      {ok, #user{nick = Nick}} ->
						  ?DICT:erase(Nick, Ns);
					      _ ->
						  Ns
					  end,
				      {?DICT:erase(J, Us), NewNs}
			      end,
			      {StateData#state.users, StateData#state.nicks},
			      LJIDs);
	      _ ->
		  {lists:foldl(fun(J, Us) ->
				       {ok, User} = ?DICT:find(J, Us),
				       ?DICT:store(J,
						   User#user{role = Role},
						   Us)
			       end, StateData#state.users, LJIDs),
		   StateData#state.nicks}
	  end,
    StateData#state{users = Users, nicks = Nicks}.

get_role(JID, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
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

is_visitor(Jid, StateData) ->
    get_role(Jid, StateData) =:= visitor.

is_moderator(Jid, StateData) ->
    get_role(Jid, StateData) =:= moderator.

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
    case treap:lookup(jlib:short_prepd_jid(JID),
		      StateData#state.activity) of
	{ok, _P, A} -> A;
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

store_user_activity(JID, UserActivity, StateData) ->
    MinMessageInterval =
	gen_mod:get_module_opt(
	  StateData#state.server_host,
	  mod_muc, min_message_interval, 0),
    MinPresenceInterval =
	gen_mod:get_module_opt(
	  StateData#state.server_host,
	  mod_muc, min_presence_interval, 0),
    Key = jlib:short_prepd_jid(JID),
    Now = now_to_usec(now()),
    Activity1 = clean_treap(StateData#state.activity, {1, -Now}),
    Activity =
	case treap:lookup(Key, Activity1) of
	    {ok, _P, _A} ->
		treap:delete(Key, Activity1);
	    error ->
		Activity1
	end,
    StateData1 =
	case (MinMessageInterval == 0) andalso
	    (MinPresenceInterval == 0) andalso
	    (UserActivity#activity.message_shaper == none) andalso
	    (UserActivity#activity.presence_shaper == none) andalso
	    (UserActivity#activity.message == undefined) andalso
	    (UserActivity#activity.presence == undefined) of
	    true ->
		StateData#state{activity = Activity};
	    false ->
		case (UserActivity#activity.message == undefined) andalso
		    (UserActivity#activity.presence == undefined) of
		    true ->
			{_, MessageShaperInterval} =
			    shaper:update(UserActivity#activity.message_shaper,
					  100000),
			{_, PresenceShaperInterval} =
			    shaper:update(UserActivity#activity.presence_shaper,
					  100000),
			Delay = lists:max([MessageShaperInterval,
					   PresenceShaperInterval,
					   MinMessageInterval * 1000,
					   MinPresenceInterval * 1000]) * 1000,
			Priority = {1, -(Now + Delay)},
			StateData#state{
			  activity = treap:insert(
				       Key,
				       Priority,
				       UserActivity,
				       Activity)};
		    false ->
			Priority = {0, 0},
			StateData#state{
			  activity = treap:insert(
				       Key,
				       Priority,
				       UserActivity,
				       Activity)}
		end
	end,
    StateData1.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
	true ->
	    Treap;
	false ->
	    {_Key, Priority, _Value} = treap:get_root(Treap),
	    if
		Priority > CleanPriority ->
		    clean_treap(treap:delete_root(Treap), CleanPriority);
		true ->
		    Treap
	    end
    end.


prepare_room_queue(StateData) ->
    case queue:out(StateData#state.room_queue) of
	{{value, {message, From}}, _RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    Packet = Activity#activity.message,
	    Size = erlang:iolist_size(exmpp_xml:document_to_iolist(Packet)),
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
	    Size = erlang:iolist_size(exmpp_xml:document_to_iolist(Packet)),
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
    LJID = jlib:short_prepd_jid(JID),
    Users = ?DICT:store(LJID,
			#user{jid = JID,
			      nick = Nick,
			      role = Role},
			StateData#state.users),
    add_to_log(join, Nick, StateData),
    Nicks = ?DICT:update(Nick,
			 fun(Entry) ->
				 case lists:member(LJID, Entry) of
				     true ->
					 Entry;
				     false ->
					 [LJID|Entry]
				 end
			 end,
			 [LJID],
			 StateData#state.nicks),
    tab_add_online_user(JID, StateData),
    StateData#state{users = Users, nicks = Nicks}.

remove_online_user(JID, StateData) ->
	remove_online_user(JID, StateData, <<>>).

remove_online_user(JID, StateData, Reason) ->
    LJID = jlib:short_prepd_jid(JID),
    {ok, #user{nick = Nick}} =
    	?DICT:find(LJID, StateData#state.users),
    add_to_log(leave, {Nick, Reason}, StateData),
    tab_remove_online_user(JID, StateData),
    Users = ?DICT:erase(LJID, StateData#state.users),
    Nicks = case ?DICT:find(Nick, StateData#state.nicks) of
		{ok, [LJID]} ->
		    ?DICT:erase(Nick, StateData#state.nicks);
		{ok, U} ->
		    ?DICT:store(Nick, U -- [LJID], StateData#state.nicks);
		false ->
		    StateData#state.nicks
	    end,
    StateData#state{users = Users, nicks = Nicks}.


filter_presence(#xmlel{name = 'presence'} = Packet) ->
    FEls = lists:filter(
	     fun(El) ->
            case El of
			 #xmlel{ns = XMLNS} ->
			     case atom_to_list(XMLNS) of
				 ?NS_MUC_s ++ _ ->
				     false;
				 _ ->
				     true
			     end
		     end
	     end, exmpp_xml:get_child_elements(Packet)),
    exmpp_xml:set_children(Packet, FEls).

strip_status(#xmlel{name = 'presence', children = Children} = Packet) ->
    FEls = lists:filter(
	     fun(#xmlel{name = 'status'}) ->
                     false;
                (_) -> true
	     end, Children),
    exmpp_xml:set_children(Packet,FEls).

add_user_presence(JID, Presence, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    FPresence = filter_presence(Presence),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{last_presence = FPresence}
	   end, StateData#state.users),
    StateData#state{users = Users}.

add_user_presence_un(JID, Presence, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    FPresence = filter_presence(Presence),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{last_presence = FPresence,
			     role = none}
	   end, StateData#state.users),
    StateData#state{users = Users}.


%% Find and return a list of the full JIDs of the users of Nick.
%% Return jid record.
find_jids_by_nick(Nick, StateData) ->
    case ?DICT:find(Nick, StateData#state.nicks) of
	{ok, [User]} ->
	    [exmpp_jid:make(User)];
	{ok, Users} ->
	    [exmpp_jid:make(LJID) || LJID <- Users];
	error ->
	    false
    end.

%% @spec(Nick::binary(), StateData) -> JID::jid() | false
%% Find and return the full JID of the user of Nick with
%% highest-priority presence.  Return jid record.
find_jid_by_nick(Nick, StateData) ->
    case ?DICT:find(Nick, StateData#state.nicks) of
	{ok, [User]} ->
	    exmpp_jid:make(User);
	{ok, [FirstUser|Users]} ->
	    #user{last_presence = FirstPresence} =
		?DICT:fetch(FirstUser, StateData#state.users),
	    {LJID, _} =
		lists:foldl(fun(Compare, {HighestUser, HighestPresence}) ->
				    #user{last_presence = P1} =
					?DICT:fetch(Compare, StateData#state.users),
				    case higher_presence(P1, HighestPresence) of
					true ->
					    {Compare, P1};
					false ->
					    {HighestUser, HighestPresence}
				    end
			    end, {FirstUser, FirstPresence}, Users),
	    exmpp_jid:make(LJID);
	error ->
	    false
    end.

higher_presence(Pres1, Pres2) ->
    Pri1 = exmpp_presence:get_priority(Pres1),
    Pri2 = exmpp_presence:get_priority(Pres2),
    Pri1 > Pri2.

is_nick_change(JID, Nick, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    case Nick of
	<<>> ->
	    false;
	_ ->
	    {ok, #user{nick = OldNick}} =
		?DICT:find(LJID, StateData#state.users),
	    Nick /= OldNick
    end.

nick_collision(User, Nick, StateData) ->
    UserOfNick = find_jid_by_nick(Nick, StateData),
    %% if nick is not used, or is used by another resource of the same
    %% user, it's ok.
    UserOfNick /= false andalso
	not exmpp_jid:bare_compare(UserOfNick, User).

add_new_user(From, Nick, Packet, StateData) ->
    Lang = exmpp_stanza:get_lang(Packet),
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
    Collision = nick_collision(From, Nick, StateData),
    case {(ServiceAffiliation == owner orelse
	   MaxUsers < 0 orelse
	   ((Affiliation == admin orelse Affiliation == owner) andalso
	    NUsers < MaxAdminUsers) orelse
	   NUsers < MaxUsers) andalso
	  NConferences < MaxConferences,
	  Collision,
	  mod_muc:can_use_nick(StateData#state.host, From, Nick),
	  get_default_role(Affiliation, StateData)} of
	{false, _, _, _} ->
	    % max user reached and user is not admin or owner
	    Err = exmpp_stanza:reply_with_error(
		    Packet,
		    'service-unavailable'),
	    ejabberd_router:route( % TODO: s/Nick/""/
	      jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, _, none} ->
	    Err = exmpp_stanza:reply_with_error(
		    Packet,
		    case Affiliation of
			outcast ->
			    ErrText = "You have been banned from this room",
			    exmpp_stanza:error(Packet#xmlel.ns,
			      'forbidden',
			      {Lang, translate:translate(Lang, ErrText)});
			_ ->
			    ErrText = "Membership is required to enter this room",
			    exmpp_stanza:error(Packet#xmlel.ns,
			      'registration-required',
			      {Lang, translate:translate(Lang, ErrText)})
		    end),
	    ejabberd_router:route( % TODO: s/Nick/""/
	      jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, true, _, _} ->
	    ErrText = "That nickname is already in use by another occupant",
	    Err = exmpp_stanza:reply_with_error(Packet, 
	      exmpp_stanza:error(Packet#xmlel.ns,
		'conflict',
		{Lang, translate:translate(Lang, ErrText)})),
	    ejabberd_router:route(
	      % TODO: s/Nick/""/
	      jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, false, _} ->
	    ErrText = "That nickname is registered by another person",
	    Err = exmpp_stanza:reply_with_error(Packet, 
	      ?ERR(Packet, 'conflict', Lang, ErrText)),
	    ejabberd_router:route(
	      % TODO: s/Nick/""/
	      jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, _, Role} ->
	    case check_password(ServiceAffiliation, Affiliation,
		  exmpp_xml:get_child_elements(Packet), From,
		  StateData) of
		true ->
		    NewState =
			add_user_presence(
			  From, Packet,
			  add_online_user(From, Nick, Role, StateData)),
		    if not (NewState#state.config)#config.anonymous ->
			    WPacket = 
			    #xmlel{name = 'message',
			      attrs = [?XMLATTR(<<"type">>, <<"groupchat">>)],
			      children = [
				#xmlel{name = 'body',
				  children = [#xmlcdata{cdata =
				      translate:translate(Lang,
					"This room is not anonymous")}]}]},
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, WPacket);
			true ->
			    ok
		    end,
		    send_existing_presences(From, NewState),
		    send_new_presence(From, NewState),
		    Shift = count_stanza_shift(Nick, 
		      exmpp_xml:get_child_elements(Packet), 
		      NewState),
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
			    Robots = ?DICT:erase(From, StateData#state.robots),
			    NewState#state{robots = Robots}
		    end;
		nopass ->
		    ErrText = "A password is required to enter this room",
		    Err = exmpp_stanza:reply_with_error(
			    Packet, ?ERR(Packet, 'not-authorized', Lang, ErrText)),
		    ejabberd_router:route( % TODO: s/Nick/""/
		      jid_replace_resource(
			StateData#state.jid, Nick),
		      From, Err),
		    StateData;
		captcha_required ->
		    SID = case exmpp_stanza:get_id(Packet) of
			      undefined -> <<"">>;
			      SID1 -> SID1
			  end,
		    RoomJID = StateData#state.jid,
		    To = jid_replace_resource(RoomJID, Nick),
		    case ejabberd_captcha:create_captcha(
			   SID, RoomJID, To, Lang, From) of
			{ok, ID, CaptchaEls} ->
                            MsgPkt = #xmlel{name = 'message',
                                attrs = [#xmlattr{name = <<"id">>, value = list_to_binary(ID)}],
                                children = CaptchaEls},
			    Robots = ?DICT:store(From,
						 {Nick, Packet}, StateData#state.robots),
			    ejabberd_router:route(RoomJID, From, MsgPkt),
			    StateData#state{robots = Robots};
			error ->
			    ErrText = "Unable to generate a captcha",
		            Err = exmpp_stanza:reply_with_error(
			            Packet, ?ERR(Packet, 'internal-server-error', Lang, ErrText)),
			    ejabberd_router:route( % TODO: s/Nick/""/
			      jid_replace_resource(
				StateData#state.jid, Nick),
			      From, Err),
			    StateData
		    end;
		_ ->
		    ErrText = "Incorrect password",
		    Err = exmpp_stanza:reply_with_error(
			    Packet, ?ERR(Packet, 'not-authorized', Lang, ErrText)),
		    ejabberd_router:route( % TODO: s/Nick/""/
		      jid_replace_resource(
			StateData#state.jid, Nick),
		      From, Err),
		    StateData
	   end
    end.

check_password(owner, _Affiliation, _Els, _From, _StateData) ->
    %% Don't check pass if user is owner in MUC service (access_admin option)
    true;
check_password(_ServiceAffiliation, Affiliation, Els, From, StateData) ->
    case (StateData#state.config)#config.password_protected of
	false ->
	    check_captcha(Affiliation, From, StateData);
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

check_captcha(Affiliation, From, StateData) ->
    case (StateData#state.config)#config.captcha_protected
	andalso ejabberd_captcha:is_feature_available() of
	true when Affiliation == none ->
	    case ?DICT:find(From, StateData#state.robots) of
		{ok, passed} ->
		    true;
		_ ->
		    captcha_required
	    end;
	_ ->
	    true
    end.

extract_password([]) ->
    false;
extract_password([#xmlel{ns = XMLNS} = El | Els]) ->
    case XMLNS of
	?NS_MUC ->
	    case exmpp_xml:get_element(El, 'password') of
		undefined ->
		    false;
		SubEl ->
		    exmpp_xml:get_cdata(SubEl)
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
    NLen = size(Nick) + 1,
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
extract_history([#xmlel{ns = XMLNS} = El | Els], Type) ->
    case XMLNS of
	?NS_MUC ->
	    AttrVal = exmpp_xml:get_path(El,
		       [{element, 'history'}, {attribute, Type,""}]),
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
    send_update_presence(JID, <<>>, StateData).

send_update_presence(JID, Reason, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    LJIDs = case LJID of
		{U, S, undefined} ->
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
			  send_new_presence(J, Reason, StateData)
		  end, LJIDs).

send_new_presence(NJID, StateData) ->
    send_new_presence(NJID, <<>>, StateData).


%% @spec(NJID::jid(), Reason::binary(), StateData) ->
send_new_presence({U, S, R}, Reason, StateData) ->
    send_new_presence(exmpp_jid:make(U, S, R), Reason, StateData);
send_new_presence(NJID1, Reason, StateData) ->
    NJID = {exmpp_jid:node(NJID1), exmpp_jid:domain(NJID1), exmpp_jid:resource(NJID1)},
    %% First, find the nick associated with this JID.
    #user{nick = Nick} = ?DICT:fetch(NJID, StateData#state.users),
    %% Then find the JID using this nick with highest priority.
    LJID1 = find_jid_by_nick(Nick, StateData),
    LJID = {exmpp_jid:node(LJID1), exmpp_jid:domain(LJID1), exmpp_jid:resource(LJID1)},
    %% Then we get the presence data we're supposed to send.
    {ok, #user{jid = RealJID,
	       role = Role,
	       last_presence = Presence}} =
	?DICT:find(LJID, StateData#state.users),
    Affiliation = get_affiliation(LJID1, StateData),
    SAffiliation = affiliation_to_binary(Affiliation),
    SRole = role_to_binary(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(RealJID)),
			   ?XMLATTR(<<"affiliation">>, SAffiliation),
			   ?XMLATTR(<<"role">>, SRole)];
		      _ ->
			  [?XMLATTR(<<"affiliation">>, SAffiliation),
			   ?XMLATTR(<<"role">>, SRole)]
		  end,
	      ItemEls = case Reason of
			    <<>> ->
				[];
			    _ ->
                [#xmlel{name = 'reason',
                       children = [#xmlcdata{cdata = Reason}]}]
			end,
	      Status = case StateData#state.just_created of
			   true ->
			       [#xmlel{name = 'status', 
                           attrs = [?XMLATTR(<<"code">>, <<"201">>)]}];
			   false ->
			       []
		       end,
	      Status2 = case ((StateData#state.config)#config.anonymous==false)
			    andalso (NJID == Info#user.jid) of
			    true ->
				[#xmlel{name = 'status',
					attrs = [?XMLATTR(<<"code">>, <<"100">>)]}
				 | Status];
			    false ->
				Status
			end,
	      Status3 = case NJID == Info#user.jid of
			    true ->
				[#xmlel{name = 'status',
					attrs = [?XMLATTR(<<"code">>, <<"110">>)]}
				 | Status2];
			    false ->
				Status2
			end,
          Packet = exmpp_xml:append_child(Presence,
             #xmlel{ns = ?NS_MUC_USER, name = 'x',
                   children = [#xmlel{ns = ?NS_MUC_USER, name ='item',
                                      attrs = ItemAttrs,
                                      children = ItemEls} | Status3]}),
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).


send_existing_presences(ToJID, StateData) ->
    LToJID = {exmpp_jid:node(ToJID), exmpp_jid:domain(ToJID), exmpp_jid:resource(ToJID)},
    {ok, #user{jid = RealToJID,
	       role = Role}} =
	?DICT:find(LToJID, StateData#state.users),
    lists:foreach(
      fun({FromNick, _Users}) ->
	      LJID1 = find_jid_by_nick(FromNick, StateData),
	      LJID = {exmpp_jid:node(LJID1), exmpp_jid:domain(LJID1), exmpp_jid:resource(LJID1)},
	      #user{jid = FromJID,
		    role = FromRole,
		    last_presence = Presence
		   } = ?DICT:fetch(LJID, StateData#state.users),
	      case RealToJID of
		  FromJID ->
		      ok;
		  _ ->
		      {N,D,R} = LJID,
		      FromAffiliation = get_affiliation(exmpp_jid:make(N,D,R), 
                                                StateData),
		      ItemAttrs =
			  case (Role == moderator) orelse
			      ((StateData#state.config)#config.anonymous ==
			       false) of
			      true ->
				  [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(FromJID)),
				   ?XMLATTR(<<"affiliation">>, 
                           affiliation_to_binary(FromAffiliation)),
				   ?XMLATTR(<<"role">>, role_to_binary(FromRole))];
			      _ ->
				  [?XMLATTR(<<"affiliation">>, 
                           affiliation_to_binary(FromAffiliation)),
				   ?XMLATTR(<<"role">>, role_to_binary(FromRole))]
			  end,
		      Packet = exmpp_xml:append_child(Presence,
				 #xmlel{ns = ?NS_MUC_USER, name = 'x',
                       children = [#xmlel{ns = ?NS_MUC_USER, name ='item',
                                          attrs = ItemAttrs}]}),
		      ejabberd_router:route(
			jid_replace_resource(
			  StateData#state.jid, FromNick),
			RealToJID,
			Packet)
	      end
      end, ?DICT:to_list(StateData#state.nicks)).



now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.


change_nick(JID, Nick, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    {ok, #user{nick = OldNick}} =
	?DICT:find(LJID, StateData#state.users),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{nick = Nick}
	   end, StateData#state.users),
    OldNickUsers = ?DICT:fetch(OldNick, StateData#state.nicks),
    NewNickUsers = case ?DICT:find(Nick, StateData#state.nicks) of
		       {ok, U} -> U;
		       error -> []
		   end,
    %% Send unavailable presence from the old nick if it's no longer
    %% used.
    SendOldUnavailable = length(OldNickUsers) == 1,
    %% If we send unavailable presence from the old nick, we should
    %% probably send presence from the new nick, in order not to
    %% confuse clients.  Otherwise, do it only if the new nick was
    %% unused.
    SendNewAvailable = SendOldUnavailable orelse
	NewNickUsers == [],
    Nicks =
	case OldNickUsers of
	    [LJID] ->
		?DICT:store(Nick, [LJID|NewNickUsers],
			     ?DICT:erase(OldNick, StateData#state.nicks));
	    [_|_] ->
		?DICT:store(Nick, [LJID|NewNickUsers],
			     ?DICT:store(OldNick, OldNickUsers -- [LJID],
					 StateData#state.nicks))
	end,
    NewStateData = StateData#state{users = Users, nicks = Nicks},
    send_nick_changing(JID, OldNick, NewStateData, SendOldUnavailable, SendNewAvailable),
    add_to_log(nickchange, {OldNick, Nick}, StateData),
    NewStateData.

send_nick_changing(JID, OldNick, StateData,
		  SendOldUnavailable, SendNewAvailable) ->
    {ok, #user{jid = RealJID,
	       nick = Nick,
	       role = Role,
	       last_presence = Presence}} =
	?DICT:find(jlib:short_prepd_jid(JID), StateData#state.users),
    Affiliation = get_affiliation(JID, StateData),
    SAffiliation = affiliation_to_binary(Affiliation),
    SRole = role_to_binary(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs1 =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(RealJID)),
			   ?XMLATTR(<<"affiliation">>, SAffiliation),
			   ?XMLATTR(<<"role">>, SRole),
			   ?XMLATTR(<<"nick">>, Nick)];
		      _ ->
			  [?XMLATTR(<<"affiliation">>, SAffiliation),
			   ?XMLATTR(<<"role">>, SRole),
			   ?XMLATTR(<<"nick">>, Nick)]
		  end,
	      ItemAttrs2 =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(RealJID)),
			   ?XMLATTR(<<"affiliation">>, SAffiliation),
			   ?XMLATTR(<<"role">>, SRole)];
		      _ ->
			  [?XMLATTR(<<"affiliation">>, SAffiliation),
			   ?XMLATTR(<<"role">>, SRole)]
		  end,
	      Packet1 =
               #xmlel{ns = ?NS_JABBER_CLIENT,
                    name = 'presence', 
                    attrs = [?XMLATTR(<<"type">>, <<"unavailable">>)],
                    children = [#xmlel{ns = ?NS_MUC_USER, name = 'x',
                                 children = [
                                  #xmlel{ns = ?NS_MUC_USER, name = 'item',
                                         attrs = ItemAttrs1},
                                  #xmlel{ns = ?NS_MUC_USER, name = 'status',
                                         attrs = [?XMLATTR(<<"code">>, 
                                                         <<"303">>)]}]}]},

	      Packet2 = exmpp_xml:append_child(
			  Presence,
			  #xmlel{ns = ?NS_MUC_USER, name = 'x',
                     children =[#xmlel{ns = ?NS_MUC_USER, 
                                       name = 'item', 
                                       attrs = ItemAttrs2}]}),
	      if SendOldUnavailable ->
		      ejabberd_router:route(
			jid_replace_resource(StateData#state.jid, OldNick),
			Info#user.jid,
			Packet1);
		 true ->
		      ok
	      end,
	      if SendNewAvailable ->
		      ejabberd_router:route(
			jid_replace_resource(StateData#state.jid, Nick),
			Info#user.jid,
			Packet2);
		 true ->
		      ok
	      end
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


add_message_to_history(FromNick, FromJID, Packet, StateData) ->
    HaveSubject = exmpp_xml:has_element(Packet, 'subject'),
    TimeStamp = calendar:now_to_universal_time(now()),
    %% Chatroom history is stored as XMPP packets, so
    %% the decision to include the original sender's JID or not is based on the
    %% chatroom configuration when the message was originally sent.
    %% Also, if the chatroom is anonymous, even moderators will not get the real JID
    SenderJid = case ((StateData#state.config)#config.anonymous) of
	true -> StateData#state.jid;
	false -> FromJID
    end,
    TSPacket = exmpp_xml:append_children(Packet,
			      [jlib:timestamp_to_xml(TimeStamp, utc, SenderJid, ""),
			       %% TODO: Delete the next line once XEP-0091 is Obsolete
			       jlib:timestamp_to_xml(TimeStamp)]),
    SPacket = exmpp_stanza:set_recipient(
                    exmpp_stanza:set_sender(TSPacket,
		                jid_replace_resource(StateData#state.jid, FromNick)),
		            StateData#state.jid),

    Size = erlang:iolist_size(exmpp_xml:document_to_iolist(SPacket)),
    Q1 = lqueue_in({FromNick, TSPacket, HaveSubject, TimeStamp, Size},
		   StateData#state.history),
    add_to_log(text, {FromNick, Packet}, StateData),
    StateData#state{history = Q1}.

send_history(JID, Shift, StateData) ->
    lists:foldl(
      fun({Nick, Packet, HaveSubject, _TimeStamp, _Size}, B) ->
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
		JID,
		Packet),
	      B or HaveSubject
      end, false, lists:nthtail(Shift, lqueue_to_list(StateData#state.history))).


send_subject(JID, Lang, StateData) ->
    case StateData#state.subject_author of
	<<>> ->
	    ok;
	Nick ->
	    Subject = StateData#state.subject,
        Packet = exmpp_message:groupchat(Subject,
                    Nick ++ translate:translate(Lang,
                              " has set the subject to: ") ++ Subject),
	    ejabberd_router:route(
	      StateData#state.jid,
	      JID,
	      Packet)
    end.

check_subject(Packet) ->
    case exmpp_message:get_subject(Packet) of
	undefined ->
	    false;
	Subj ->
	    Subj
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
    #xmlel{children = Items} = SubEl,
    process_admin_items_set(From, Items, Lang, StateData);

process_iq_admin(From, get, Lang, SubEl, StateData) ->
    case exmpp_xml:get_element(SubEl, 'item') of
	'undefined' ->
	    {error, 'bad-request'};
	Item ->
	    FAffiliation = get_affiliation(From, StateData),
	    FRole = get_role(From, StateData),
	    case exmpp_xml:get_attribute_as_binary(Item, <<"role">>, false) of
		false ->
		    case exmpp_xml:get_attribute_as_binary(Item, <<"affiliation">>, false) of
			false ->
			    {error, 'bad-request'};
			StrAffiliation ->
			    case catch binary_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    {error, 'bad-request'};
				SAffiliation ->
				    if
					(FAffiliation == owner) or
					(FAffiliation == admin) ->
					    Items = items_with_affiliation(
						      SAffiliation, StateData),
					    {result, Items, StateData};
					true ->
					    ErrText = "Administrator privileges required",
					    {error, ?ERR(SubEl, 'forbidden', Lang, ErrText)}
				    end
			    end
		    end;
		StrRole ->
		    case catch binary_to_role(StrRole) of
			{'EXIT', _} ->
			    {error, 'bad-request'};
			SRole ->
			    if
				FRole == moderator ->
				    Items = items_with_role(SRole, StateData),
				    {result, Items, StateData};
				true ->
				    ErrText = "Moderator privileges required",
				    {error, ?ERR(SubEl, 'forbidden', Lang, ErrText)}
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
        {N, D, R} = JID,
        #xmlel{name = 'item', 
               ns = ?NS_MUC_ADMIN,
               attrs = [?XMLATTR(<<"affiliation">>, 
                                 affiliation_to_binary(Affiliation)),
                        ?XMLATTR(<<"jid">>,
                                 exmpp_jid:to_binary(N, D, R))],
               children = [ #xmlel{name = 'reason',
                                   ns = ?NS_MUC_ADMIN,
                                   children = [#xmlcdata{cdata = Reason}]}]};

	 ({JID, Affiliation}) ->
        {N, D, R} = JID,
        #xmlel{name = 'item', 
               ns = ?NS_MUC_ADMIN,
               attrs = [?XMLATTR(<<"affiliation">>, 
                                 affiliation_to_binary(Affiliation)),
                        ?XMLATTR(<<"jid">>,
                                 exmpp_jid:to_binary(N, D, R))]}
      end, search_affiliation(SAffiliation, StateData)).

user_to_item(#user{role = Role,
		   nick = Nick,
		   jid = JID
		  }, StateData) ->
    Affiliation = get_affiliation(JID, StateData),
    #xmlel{name = 'item',
            ns =?NS_MUC_ADMIN,
           attrs = [
      ?XMLATTR(<<"role">>, role_to_binary(Role)),
      ?XMLATTR(<<"affiliation">>, affiliation_to_binary(Affiliation)),
      ?XMLATTR(<<"nick">>, Nick),
      ?XMLATTR(<<"jid">>, exmpp_jid:to_binary(JID))]
     }.

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
		      [exmpp_jid:to_binary(UJID), exmpp_jid:to_binary(StateData#state.jid), Res]),
	    NSD =
		lists:foldl(
		  fun(E, SD) ->
			process_admin_items_set(E, SD)
		  end, StateData, lists:flatten(Res)),
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

process_admin_items_set({JID, affiliation, owner, _} = E, SD) ->
    case exmpp_jid:prep_node(JID) of
	%% TODO: <<>> or 'undefined' ?
	%% TODO: double case on the E var, because
	%%       exmpp_jid:prep_node/1 can't be used in guards
	%% If the provided JID does not have username,
	%% forget the affiliation completely
	<<>> ->
	    SD;
	_ ->
	    process_admin_items_set2(E, SD)
    end;
process_admin_items_set(E, SD) ->
    process_admin_items_set2(E, SD).

process_admin_items_set2(E, SD) ->
    try process_admin_items_set3(E, SD)
    catch
	'EXIT':ErrReason ->
	    ?ERROR_MSG("MUC ITEMS SET ERR: ~p~n",
		       [E, ErrReason]),
	    SD
    end.

process_admin_items_set3({JID, role, none, Reason}, SD) ->
    catch send_kickban_presence(
	    JID, Reason, "307", SD),
    set_role(JID, none, SD);

process_admin_items_set3({JID, affiliation, none, Reason}, SD) ->
    case (SD#state.config)#config.members_only of
	true ->
    catch send_kickban_presence(
	    JID, Reason, "321", none, SD),
	    SD1 = set_affiliation(JID, none, SD),
	    set_role(JID, none, SD1);
	_ ->
	    SD1 = set_affiliation(JID, none, SD),
	    send_update_presence(JID, SD1),
	    SD1
    end;

process_admin_items_set3({JID, affiliation, outcast, Reason}, SD) ->
    catch send_kickban_presence(
	    JID, Reason, "301", outcast, SD),
    set_affiliation_and_reason(
      JID, outcast, Reason,
      set_role(JID, none, SD));

process_admin_items_set3({JID, affiliation, A, Reason}, SD)
  when (A == admin) or (A == owner) ->
    SD1 = set_affiliation_and_reason(JID, A, Reason, SD),
    SD2 = set_role(JID, moderator, SD1),
    send_update_presence(JID, Reason, SD2),
    SD2;

process_admin_items_set3({JID, affiliation, member, Reason}, SD) ->
    SD1 = set_affiliation_and_reason( JID, member, Reason, SD),
    SD2 = set_role(JID, participant, SD1),
    send_update_presence(JID, Reason, SD2),
    SD2;

process_admin_items_set3({JID, role, Role, Reason}, SD) ->
    SD1 = set_role(JID, Role, SD),
    catch send_new_presence(JID, Reason, SD1),
    SD1;

process_admin_items_set3({JID, affiliation, A, _Reason}, SD) ->
    SD1 = set_affiliation(JID, A, SD),
    send_update_presence(JID, SD1),
    SD1.

find_changed_items(_UJID, _UAffiliation, _URole, [], _Lang, _StateData, Res) ->
    {result, Res};
find_changed_items(UJID, UAffiliation, URole, [#xmlcdata{} | Items],
		   Lang, StateData, Res) ->
    find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, Res);
find_changed_items(UJID, UAffiliation, URole,
		   [#xmlel{name = 'item'} = Item | Items],
		   Lang, StateData, Res) ->
    TJID = case exmpp_xml:get_attribute_as_binary(Item, <<"jid">>,false) of
	       S when S =/= false ->
		   try exmpp_jid:parse(S) of
		       J ->
			   {value, [J]}
            catch
		       _:_ ->
			   ErrText = io_lib:format(
				       translate:translate(
					 Lang,
					 "Jabber ID ~s is invalid"), [S]),
			   {error, ?ERR(Item, 'not-acceptable', Lang, ErrText)}
		   end;
	       _ ->
		   case exmpp_xml:get_attribute(Item, <<"nick">>, false) of
		       N when N =/= false ->
			   case find_jids_by_nick(N, StateData) of
			       false ->
				   ErrText =
				       io_lib:format(
					 translate:translate(
					   Lang,
					   "Nickname ~s does not exist in the room"),
					 [binary_to_list(N)]),
				   {error, ?ERR(Item, 'not-acceptable', Lang, ErrText)};
			       J ->
				   {value, J}
			   end;
		       _ ->
			   {error, 'bad-request'}
		   end
	   end,
    case TJID of
	{value, [JID|_]=JIDs} ->
	    TAffiliation = get_affiliation(JID, StateData),
	    TRole = get_role(JID, StateData),
	    case exmpp_xml:get_attribute_as_binary(Item, <<"role">>,false) of
		false ->
		    case exmpp_xml:get_attribute_as_binary(Item, <<"affiliation">>, false) of
			false ->
			    {error, 'bad-request'};
			StrAffiliation ->
			    case catch binary_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    ErrText1 =
					io_lib:format(
					  translate:translate(
					    Lang,
					    "Invalid affiliation: ~s"),
					    [StrAffiliation]),
				    {error, ?ERR(Item, 'not-acceptable', Lang, ErrText1)};
				SAffiliation ->
				    ServiceAf = get_service_affiliation(JID, StateData),
				    CanChangeRA =
					case can_change_ra(
					       UAffiliation, URole,
					       TAffiliation, TRole,
					       affiliation, SAffiliation,
						   ServiceAf) of
					    nothing ->
						nothing;
					    true ->
						true;
					    check_owner ->
						case search_affiliation(
						       owner, StateData) of
						    [{OJID, _}] ->
                            jlib:short_bare_jid(OJID) /= 
                                jlib:short_prepd_bare_jid(UJID);
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
					    Reason = exmpp_xml:get_path(Item, [{element, 'reason'}, cdata]),
					    MoreRes = [{exmpp_jid:bare(Jidx), affiliation, SAffiliation, Reason} || Jidx <- JIDs],
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, Lang, StateData,
					      [MoreRes | Res]);
					false ->
					    {error, 'not-allowed'}
				    end
			    end
		    end;
		StrRole ->
		    case catch binary_to_role(StrRole) of
			{'EXIT', _} ->
			    ErrText1 =
				io_lib:format(
				  translate:translate(
				    Lang,
				    "Invalid role: ~s"),
				  [StrRole]),
			    {error, ?ERR(Item, 'bad-request', Lang, ErrText1)};
			SRole ->
			    ServiceAf = get_service_affiliation(JID, StateData),
			    CanChangeRA =
				case can_change_ra(
				       UAffiliation, URole,
				       TAffiliation, TRole,
				       role, SRole,
					   ServiceAf) of
				    nothing ->
					nothing;
				    true ->
					true;
				    check_owner ->
					case search_affiliation(
					       owner, StateData) of
					    [{OJID, _}] ->
                        jlib:short_bare_jid(OJID) /= 
                            jlib:short_prepd_bare_jid(UJID);
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
				    Reason = exmpp_xml:get_path(Item, [{element, 'reason'}, cdata]),
				    MoreRes = [{Jidx, role, SRole, Reason} || Jidx <- JIDs],
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, Lang, StateData,
				      [MoreRes | Res]);
				_ ->
				    {error, 'not-allowed'}
			    end
		    end
	    end;
	Err ->
	    Err
    end;
find_changed_items(_UJID, _UAffiliation, _URole, _Items,
		   _Lang, _StateData, _Res) ->
    {error, 'bad-request'}.


can_change_ra(_FAffiliation, _FRole,
	      owner, _TRole,
	      affiliation, owner, owner) ->
    %% A room owner tries to add as persistent owner a
    %% participant that is already owner because he is MUC admin
    true;
can_change_ra(_FAffiliation, _FRole,
	      TAffiliation, _TRole,
	      affiliation, Value, _ServiceAf)
  when (TAffiliation == Value) ->
    nothing;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, TRole,
	      role, Value, _ServiceAf)
  when (TRole == Value) ->
    nothing;
can_change_ra(FAffiliation, _FRole,
	      outcast, _TRole,
	      affiliation, none, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      outcast, _TRole,
	      affiliation, member, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      outcast, _TRole,
	      affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      outcast, _TRole,
	      affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      none, _TRole,
	      affiliation, outcast, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      none, _TRole,
	      affiliation, member, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      none, _TRole,
	      affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      none, _TRole,
	      affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      member, _TRole,
	      affiliation, outcast, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      member, _TRole,
	      affiliation, none, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      member, _TRole,
	      affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      member, _TRole,
	      affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      admin, _TRole,
	      affiliation, _Affiliation, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      owner, _TRole,
	      affiliation, _Affiliation, _ServiceAf) ->
    check_owner;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, _TRole,
	      affiliation, _Value, _ServiceAf) ->
    false;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, visitor,
	      role, none, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, visitor,
	      role, participant, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      _TAffiliation, visitor,
	      role, moderator, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, participant,
	      role, none, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, participant,
	      role, visitor, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      _TAffiliation, participant,
	      role, moderator, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      owner, moderator,
	      role, visitor, _ServiceAf) ->
    false;
can_change_ra(owner, _FRole,
	      _TAffiliation, moderator,
	      role, visitor, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      admin, moderator,
	      role, visitor, _ServiceAf) ->
    false;
can_change_ra(admin, _FRole,
	      _TAffiliation, moderator,
	      role, visitor, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      owner, moderator,
	      role, participant, _ServiceAf) ->
    false;
can_change_ra(owner, _FRole,
	      _TAffiliation, moderator,
	      role, participant, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      admin, moderator,
	      role, participant, _ServiceAf) ->
    false;
can_change_ra(admin, _FRole,
	      _TAffiliation, moderator,
	      role, participant, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, _TRole,
	      role, _Value, _ServiceAf) ->
    false.


send_kickban_presence(JID, Reason, Code, StateData) ->
    NewAffiliation = get_affiliation(JID, StateData),
	send_kickban_presence(JID, Reason, Code, NewAffiliation, StateData).

send_kickban_presence(JID, Reason, Code, NewAffiliation, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    LJIDs = case LJID of
		{U, S, undefined} ->
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
			  send_kickban_presence1(J, Reason, Code, NewAffiliation, StateData)
		  end, LJIDs).

send_kickban_presence1(UJID, Reason, Code, Affiliation, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick}} = ?DICT:find(UJID, StateData#state.users),
    SAffiliation = affiliation_to_binary(Affiliation),
    BannedJID = exmpp_jid:to_binary(RealJID),
    lists:foreach(
      fun({_LJID, Info}) ->
	      JidAttrList = case (Info#user.role == moderator) orelse
				((StateData#state.config)#config.anonymous
				 == false) of
				true -> [?XMLATTR(<<"jid">>, BannedJID)];
				false -> []
			    end,
	      ItemAttrs = [?XMLATTR(<<"affiliation">>, SAffiliation),
			           ?XMLATTR(<<"role">>, <<"none">>)] ++ JidAttrList,
	      ItemEls = case Reason of
			    "" ->
				[];
			    _ ->
				[#xmlel{name = 'reason',
                        children = [#xmlcdata{cdata = Reason}]}]
                end,
          Packet = 
            #xmlel{ns = ?NS_JABBER_CLIENT,
                  name = 'presence',
                  attrs = [?XMLATTR(<<"type">>, <<"unavailable">>)],
                  children = [#xmlel{ns = ?NS_MUC_USER, name = 'x',
                                 children = [
                                 #xmlel{ns = ?NS_MUC_USER, name = 'item',
                                        attrs = ItemAttrs,
                                        children = ItemEls},
                                 #xmlel{ns = ?NS_MUC_USER, name = 'status',
                                        attrs = [?XMLATTR(<<"code">>, 
                                                          Code)]}]}]},
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff

process_iq_owner(From, set, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
	owner ->
	    case exmpp_xml:get_child_elements(SubEl) of
		[#xmlel{ns = XMLNS, name = 'x'} = XEl] ->
		    case {XMLNS, exmpp_xml:get_attribute_as_binary(XEl, <<"type">>,false)} of
			{?NS_DATA_FORMS, <<"cancel">>} ->
			    {result, [], StateData};
			{?NS_DATA_FORMS, <<"submit">>} ->
			    case is_allowed_log_change(XEl, StateData, From)
				andalso
				is_allowed_persistent_change(XEl, StateData,
							     From)
				andalso
				is_allowed_room_name_desc_limits(XEl,
								 StateData)
				andalso
				is_password_settings_correct(XEl, StateData) of
				true -> set_config(XEl, StateData);
				false -> {error, 'not-acceptable'}
			    end;
			_ ->
			    {error, 'bad-request'}
		    end;
		[#xmlel{name = 'destroy'} = SubEl1] ->
		    ?INFO_MSG("Destroyed MUC room ~s by the owner ~s", 
			      [exmpp_jid:to_binary(StateData#state.jid), exmpp_jid:to_binary(From)]),
		    add_to_log(room_existence, destroyed, StateData),
		    destroy_room(SubEl1, StateData);
		Items ->
		    process_admin_items_set(From, Items, Lang, StateData)
	    end;
	_ ->
	    ErrText = "Owner privileges required",
	    {error, ?ERR(SubEl, 'forbidden', Lang, ErrText)}
    end;

process_iq_owner(From, get, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
	owner ->
	    case exmpp_xml:get_child_elements(SubEl) of
		[] ->
		    get_config(Lang, StateData, From);
		[Item] ->
		    case exmpp_xml:get_attribute_as_binary(Item, <<"affiliation">>,false) of
			false ->
			    {error, 'bad-request'};
			StrAffiliation ->
			    case catch binary_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    ErrText =
					io_lib:format(
					  translate:translate(
					    Lang,
					    "Invalid affiliation: ~s"),
					  [StrAffiliation]),
				    {error, ?ERR(SubEl, 'not-acceptable', Lang, ErrText)};
				SAffiliation ->
				    Items = items_with_affiliation(
					      SAffiliation, StateData),
				    {result, Items, StateData}
			    end
		    end;
		_ ->
		    {error, 'feature-not-implemented'}
	    end;
	_ ->
	    ErrText = "Owner privileges required",
	    {error, ?ERR(SubEl, 'forbidden', Lang, ErrText)}
    end.

is_allowed_log_change(XEl, StateData, From) ->
    case lists:keymember("muc#roomconfig_enablelogging", 1,
			 jlib:parse_xdata_submit(XEl)) of
	false ->
	    true;
	true ->
	    (allow == mod_muc_log:check_access_log(
	      StateData#state.server_host, From))
    end.

is_allowed_persistent_change(XEl, StateData, From) ->
    case lists:keymember("muc#roomconfig_persistentroom", 1,
			 jlib:parse_xdata_submit(XEl)) of
	false ->
	    true;
	true ->
		{_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = StateData#state.access,
		(allow == acl:match_rule(StateData#state.server_host, AccessPersistent, From))
    end.

%% Check if the Room Name and Room Description defined in the Data Form
%% are conformant to the configured limits
is_allowed_room_name_desc_limits(XEl, StateData) ->
    IsNameAccepted =
	case lists:keysearch("muc#roomconfig_roomname", 1,
			     jlib:parse_xdata_submit(XEl)) of
	    {value, {_, [N]}} ->
		length(N) =< gen_mod:get_module_opt(StateData#state.server_host,
						    mod_muc, max_room_name,
						    infinite);
	    _ ->
		true
	end,
    IsDescAccepted =
	case lists:keysearch("muc#roomconfig_roomdesc", 1,
			     jlib:parse_xdata_submit(XEl)) of
	    {value, {_, [D]}} ->
		length(D) =< gen_mod:get_module_opt(StateData#state.server_host,
						    mod_muc, max_room_desc,
						    infinite);
	    _ ->
		true
	end,
    IsNameAccepted and IsDescAccepted.

%% Return false if:
%% "the password for a password-protected room is blank"
is_password_settings_correct(XEl, StateData) ->
    Config = StateData#state.config,
    OldProtected = Config#config.password_protected,
    OldPassword = Config#config.password,
    NewProtected =
	case lists:keysearch("muc#roomconfig_passwordprotectedroom", 1,
			     jlib:parse_xdata_submit(XEl)) of
	    {value, {_, ["1"]}} ->
		true;
	    {value, {_, ["0"]}} ->
		false;
	    _ ->
		undefined
	end,
    NewPassword =
	case lists:keysearch("muc#roomconfig_roomsecret", 1,
			     jlib:parse_xdata_submit(XEl)) of
	    {value, {_, [P]}} ->
		P;
	    _ ->
		undefined
	end,
    case {OldProtected, NewProtected, OldPassword, NewPassword} of
	{true, undefined, "", undefined} ->
	    false;
	{true, undefined, _, ""} ->
	    false;
	{_, true , "", undefined} ->
	    false;
	{_, true, _, ""} ->
	    false;
	_ ->
	    true
    end.


-define(XFIELD(Type, Label, Var, Val),
    #xmlel{name = 'field', 
           attrs = [?XMLATTR(<<"type">>, Type),
                    ?XMLATTR(<<"label">>, translate:translate(Lang, Label)),
                    ?XMLATTR(<<"var">>, Var)],
           children = [#xmlel{name = 'value',
                              children = [#xmlcdata{cdata = Val} ]}]}).


-define(BOOLXFIELD(Label, Var, Val),
	?XFIELD("boolean", Label, Var,
		case Val of
		    true -> <<"1">>;
		    _ -> <<"0">>
		end)).

-define(STRINGXFIELD(Label, Var, Val),
	?XFIELD("text-single", Label, Var, Val)).

-define(PRIVATEXFIELD(Label, Var, Val),
	?XFIELD("text-private", Label, Var, Val)).


get_default_room_maxusers(RoomState) ->
    DefRoomOpts = gen_mod:get_module_opt(RoomState#state.server_host, mod_muc, default_room_options, []),
    RoomState2 = set_opts(DefRoomOpts, RoomState),
    (RoomState2#state.config)#config.max_users.

get_config(Lang, StateData, From) ->
    {_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = StateData#state.access,
    ServiceMaxUsers = get_service_max_users(StateData),
    DefaultRoomMaxUsers = get_default_room_maxusers(StateData),
    Config = StateData#state.config,
    {MaxUsersRoomInteger, MaxUsersRoomString} =
	case get_max_users(StateData) of
	    N when is_integer(N) ->
		{N, erlang:integer_to_list(N)};
	    _ -> {0, "none"}
	end,
    Res =
	[#xmlel{name = 'title', children = [ #xmlcdata{cdata =
	        list_to_binary(io_lib:format(translate:translate(Lang, "Configuration of room ~s"),
		    [exmpp_jid:to_list(StateData#state.jid)]))
         }]},
    #xmlel{name = 'field', attrs = [?XMLATTR(<<"type">>, <<"hidden">>),
                                  ?XMLATTR(<<"var">>, <<"FORM_TYPE">>)],
           children = [#xmlel{name = 'value', children = [#xmlcdata{cdata = 
                               <<"http://jabber.org/protocol/muc#roomconfig">>
                        }]}]},
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
     #xmlel{name = 'field', attrs = [
                ?XMLATTR(<<"type">>, <<"list-single">>),
                ?XMLATTR(<<"label">>, translate:translate(Lang,
                                "Maximum Number of Occupants")),
                ?XMLATTR(<<"var">>, <<"muc#roomconfig_maxusers">>)],
            children = [#xmlel{name = 'value',
                               children = [#xmlcdata{cdata = 
                                list_to_binary(MaxUsersRoomString)}]}] ++
	  if
	      is_integer(ServiceMaxUsers) -> [];
	      true ->
          [#xmlel{name = 'option', attrs = [?XMLATTR(<<"label">>, 
                        translate:translate(Lang, "No limit"))],
                  children = [#xmlel{name = 'value',
                                     children = [#xmlcdata{cdata = <<"none">>}]}]}]
	  end ++
      [#xmlel{name = 'option', attrs = [?XMLATTR(<<"label">>, N)],
              children = [#xmlel{name = 'value', children = [
                #xmlcdata{cdata = list_to_binary(erlang:integer_to_list(N))}]}]} ||
              N <- lists:usort([ServiceMaxUsers, DefaultRoomMaxUsers, MaxUsersRoomInteger |
                               ?MAX_USERS_DEFAULT_LIST]), N =< ServiceMaxUsers]}, 
    #xmlel{name = 'field', attrs = [
                ?XMLATTR(<<"type">>, <<"list-single">>),
                ?XMLATTR(<<"label">>, 
                    translate:translate(Lang, "Present real Jabber IDs to")),
                ?XMLATTR(<<"var">>, <<"muc#roomconfig_whois">>)],
          children = [#xmlel{name = 'value',
                        children = [#xmlcdata{cdata = 
                                if Config#config.anonymous -> <<"moderators">>;
                                   true -> <<"anyone">>
                                end}]},
                     #xmlel{name = 'option', attrs = [
                            ?XMLATTR(<<"label">>,
                              translate:translate(Lang, "moderators only"))],
                           children = [#xmlel{name = 'value', 
                                       children = [#xmlcdata{cdata = 
                                                        <<"moderators">>}]}]},
                     #xmlel{name = 'option', attrs = [
                            ?XMLATTR(<<"label">>,
                              translate:translate(Lang, "anyone"))],
                           children = [#xmlel{name = 'value', 
                                       children = [#xmlcdata{cdata = 
                                                        <<"anyone">>}]}]}]},
         ?BOOLXFIELD("Make room members-only",
                 "muc#roomconfig_membersonly",
                 Config#config.members_only),
         ?BOOLXFIELD("Make room moderated",
                 "muc#roomconfig_moderatedroom",
                 Config#config.moderated),
         ?BOOLXFIELD("Default users as participants",
                 "members_by_default",
                 Config#config.members_by_default),
         ?BOOLXFIELD("Allow users to change the subject",
                 "muc#roomconfig_changesubject",
                 Config#config.allow_change_subj),
         ?BOOLXFIELD("Allow users to send private messages",
                 "allow_private_messages",
                 Config#config.allow_private_messages),
    #xmlel{name = 'field', attrs = [
                ?XMLATTR(<<"type">>, <<"list-single">>),
                ?XMLATTR(<<"label">>,
                    translate:translate(Lang, "Allow visitors to send private messages to")),
                ?XMLATTR(<<"var">>, <<"allow_private_messages_from_visitors">>)],
          children = [#xmlel{name = 'value',
                        children = [#xmlcdata{cdata =
				       case Config#config.allow_private_messages_from_visitors of
					   anyone ->
					       <<"anyone">>;
					   moderators ->
					       <<"moderators">>;
					   nobody ->
					       <<"nobody">>
				       end}]},
                     #xmlel{name = 'option', attrs = [
                            ?XMLATTR(<<"label">>,
                              translate:translate(Lang, "nobody"))],
                           children = [#xmlel{name = 'value',
                                       children = [#xmlcdata{cdata =
                                                        <<"nobody">>}]}]},
                     #xmlel{name = 'option', attrs = [
                            ?XMLATTR(<<"label">>,
                              translate:translate(Lang, "moderators only"))],
                           children = [#xmlel{name = 'value',
                                       children = [#xmlcdata{cdata =
                                                        <<"moderators">>}]}]},
                     #xmlel{name = 'option', attrs = [
                            ?XMLATTR(<<"label">>,
                              translate:translate(Lang, "anyone"))],
                           children = [#xmlel{name = 'value',
                                       children = [#xmlcdata{cdata =
                                                        <<"anyone">>}]}]}]},
         ?BOOLXFIELD("Allow users to query other users",
                 "allow_query_users",
                 Config#config.allow_query_users),
         ?BOOLXFIELD("Allow users to send invites",
                 "muc#roomconfig_allowinvites",
                 Config#config.allow_user_invites),
         ?BOOLXFIELD("Allow visitors to send status text in presence updates",
                 "muc#roomconfig_allowvisitorstatus",
                 Config#config.allow_visitor_status),
         ?BOOLXFIELD("Allow visitors to change nickname",
                 "muc#roomconfig_allowvisitornickchange",
                 Config#config.allow_visitor_nickchange)
	] ++
	case ejabberd_captcha:is_feature_available() of
	    true ->
	        [?BOOLXFIELD("Make room captcha protected",
			     "captcha_protected",
			     Config#config.captcha_protected)];
	    false -> []
	end ++
	case mod_muc_log:check_access_log(
       StateData#state.server_host, From) of
	    allow ->
		[?BOOLXFIELD(
		    "Enable logging",
		    "muc#roomconfig_enablelogging",
		    Config#config.logging)];
	    _ -> []
	end,
    {result , [#xmlel{name = 'instructions', children = [
                #xmlcdata{cdata = translate:translate(Lang,
		         "You need an x:data capable client to configure room")}]},
                 #xmlel{ns = ?NS_DATA_FORMS, name = 'x', 
                attrs = [?XMLATTR(<<"type">>, <<"form">>)],
                children = Res}],
        StateData}.



set_config(XEl, StateData) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
	invalid ->
	    {error, 'bad-request'};
	_ ->
	    case set_xoption(XData, StateData#state.config) of
		#config{} = Config ->
		    Res = change_config(Config, StateData),
		    {result, _, NSD} = Res,
		    Type = case {(StateData#state.config)#config.logging,
				 Config#config.logging} of
			       {true, false} ->
				   roomconfig_change_disabledlogging;
			       {false, true} ->
				   roomconfig_change_enabledlogging;
			       {_, _} ->
				   roomconfig_change
			   end,
		    Users = [{U#user.jid, U#user.nick, U#user.role} ||
				{_, U} <- ?DICT:to_list(StateData#state.users)],
		    add_to_log(Type, Users, NSD),
		    Res;
		Err ->
		    Err
	    end
    end.

-define(SET_BOOL_XOPT_FALSE(Opt), set_xoption(Opts, Config#config{Opt = false})).
-define(SET_BOOL_XOPT_TRUE (Opt), set_xoption(Opts, Config#config{Opt = true})).

-define(SET_BOOL_XOPT(Opt, Val),
	case Val of
	    "0" -> ?SET_BOOL_XOPT_FALSE(Opt);
	    "false" -> ?SET_BOOL_XOPT_FALSE(Opt);
	    "1" -> ?SET_BOOL_XOPT_TRUE(Opt);
	    "true" -> ?SET_BOOL_XOPT_TRUE(Opt);
	    _ -> {error, 'bad-request'}
	end).

-define(SET_NAT_XOPT(Opt, Val),
	case catch list_to_integer(Val) of
	    I when is_integer(I),
	           I > 0 ->
		set_xoption(Opts, Config#config{Opt = I});
	    _ ->
		{error, 'bad-request'}
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
set_xoption([{"allow_private_messages_from_visitors", [Val]} | Opts], Config) ->
    case Val of
	"anyone" ->
	    ?SET_STRING_XOPT(allow_private_messages_from_visitors, anyone);
	"moderators" ->
	    ?SET_STRING_XOPT(allow_private_messages_from_visitors, moderators);
	"nobody" ->
	    ?SET_STRING_XOPT(allow_private_messages_from_visitors, nobody);
	_ ->
	    {error, 'bad-request'}
    end;
set_xoption([{"muc#roomconfig_allowvisitorstatus", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_visitor_status, Val);
set_xoption([{"muc#roomconfig_allowvisitornickchange", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_visitor_nickchange, Val);
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
set_xoption([{"captcha_protected", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(captcha_protected, Val);
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
	    ?SET_BOOL_XOPT_TRUE(anonymous);
	"anyone" ->
	    ?SET_BOOL_XOPT_FALSE(anonymous);
	_ ->
	    {error, 'bad-request'}
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
    {error, 'bad-request'}.


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
%% TODO: fix the calls to this function, so this clause isn't needed
set_opts([#muc_room_opt{opt = Opt, val = Val} | Opts], StateData) ->
    set_opts([{Opt, Val} | Opts], StateData);
set_opts([{Opt, Val} | Opts], StateData) ->
    NSD = case Opt of
	      title -> StateData#state{config = (StateData#state.config)#config{title = Val}};
	      description -> StateData#state{config = (StateData#state.config)#config{description = Val}};
	      allow_change_subj -> StateData#state{config = (StateData#state.config)#config{allow_change_subj = Val}};
	      allow_query_users -> StateData#state{config = (StateData#state.config)#config{allow_query_users = Val}};
	      allow_private_messages -> StateData#state{config = (StateData#state.config)#config{allow_private_messages = Val}};
	      allow_private_messages_from_visitors -> StateData#state{config = (StateData#state.config)#config{allow_private_messages_from_visitors = Val}};
	      allow_visitor_nickchange -> StateData#state{config = (StateData#state.config)#config{allow_visitor_nickchange = Val}};
	      allow_visitor_status -> StateData#state{config = (StateData#state.config)#config{allow_visitor_status = Val}};
	      public -> StateData#state{config = (StateData#state.config)#config{public = Val}};
	      public_list -> StateData#state{config = (StateData#state.config)#config{public_list = Val}};
	      persistent -> StateData#state{config = (StateData#state.config)#config{persistent = Val}};
	      moderated -> StateData#state{config = (StateData#state.config)#config{moderated = Val}};
	      members_by_default -> StateData#state{config = (StateData#state.config)#config{members_by_default = Val}};
	      members_only -> StateData#state{config = (StateData#state.config)#config{members_only = Val}};
	      allow_user_invites -> StateData#state{config = (StateData#state.config)#config{allow_user_invites = Val}};
	      password_protected -> StateData#state{config = (StateData#state.config)#config{password_protected = Val}};
	      captcha_protected -> StateData#state{config = (StateData#state.config)#config{captcha_protected = Val}};
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
     ?MAKE_CONFIG_OPT(allow_private_messages_from_visitors),
     ?MAKE_CONFIG_OPT(allow_visitor_status),
     ?MAKE_CONFIG_OPT(allow_visitor_nickchange),
     ?MAKE_CONFIG_OPT(public),
     ?MAKE_CONFIG_OPT(public_list),
     ?MAKE_CONFIG_OPT(persistent),
     ?MAKE_CONFIG_OPT(moderated),
     ?MAKE_CONFIG_OPT(members_by_default),
     ?MAKE_CONFIG_OPT(members_only),
     ?MAKE_CONFIG_OPT(allow_user_invites),
     ?MAKE_CONFIG_OPT(password_protected),
     ?MAKE_CONFIG_OPT(captcha_protected),
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
	      ItemAttrs = [?XMLATTR(<<"affiliation">>, <<"none">>),
			   ?XMLATTR(<<"role">>, <<"none">>)],
          Packet = #xmlel{ns = ?NS_JABBER_CLIENT,
                          name = 'presence', 
                          attrs = [?XMLATTR(<<"type">>, 
                                            <<"unavailable">>)],
                          children = [
                            #xmlel{ns = ?NS_MUC_USER, name = 'x', children =
                                    [#xmlel{name = 'item', attrs = ItemAttrs},
                                     DEl]}]},
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
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

-define(FEATURE(Var), #xmlel{name = 'feature', 
                        attrs = [?XMLATTR(<<"var">>, Var)]}).

-define(CONFIG_OPT_TO_FEATURE(Opt, Fiftrue, Fiffalse),
    case Opt of
	true ->
	    ?FEATURE(Fiftrue);
	false ->
	    ?FEATURE(Fiffalse)
    end).

process_iq_disco_info(_From, set, _Lang, _StateData) ->
    {error, 'not-allowed'};

process_iq_disco_info(_From, get, Lang, StateData) ->
    Config = StateData#state.config,
    {result, [ #xmlel{name = 'identity',
                      attrs = [?XMLATTR(<<"category">>, 
                                        <<"conference">>),
                               ?XMLATTR(<<"type">>, <<"text">>),
                        	   ?XMLATTR(<<"name">>, 
                                        get_title(StateData))]},
               #xmlel{name = 'feature', 
                      attrs = [?XMLATTR(<<"var">>, ?NS_MUC_s)]},
    
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
    #xmlel{name = 'field', attrs = [?XMLATTR(<<"type">>, Type),
                                    ?XMLATTR(<<"var">>, Var)],
           children = [#xmlel{name = 'value', 
                             children = [#xmlcdata{cdata = list_to_binary(Val)}]}]}).

-define(RFIELD(Label, Var, Val),
    #xmlel{name = 'field', attrs = [?XMLATTR(<<"label">>,
                                        translate:translate(Lang, Label)),
                                    ?XMLATTR(<<"var">>, Var)],
            children = [#xmlel{name = 'value', children = [
                            #xmlcdata{cdata = list_to_binary(Val)}]}]}).

iq_disco_info_extras(Lang, StateData) ->
    Len = ?DICT:size(StateData#state.users),
    RoomDescription = (StateData#state.config)#config.description,
    [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', 
             attrs = [?XMLATTR(<<"type">>, <<"result">>)],
             children = 
      [?RFIELDT("hidden", "FORM_TYPE",
		"http://jabber.org/protocol/muc#roominfo"),
       ?RFIELD("Room description", "muc#roominfo_description",
	       RoomDescription),
       ?RFIELD("Number of occupants", "muc#roominfo_occupants",
	       integer_to_list(Len))
      ]}].

process_iq_disco_items(_From, set, _Lang, _StateData) ->
    {error, 'not-allowed'};

process_iq_disco_items(From, get, _Lang, StateData) ->
    case (StateData#state.config)#config.public_list of
	true ->
	    {result, get_mucroom_disco_items(StateData), StateData};
	_ ->
	    case is_occupant_or_admin(From, StateData) of
		true ->
		    {result, get_mucroom_disco_items(StateData), StateData};
		_ ->
		    {error, 'forbidden'}
	    end
    end.

process_iq_captcha(_From, get, _Lang, _SubEl, _StateData) ->
    {error, 'not-allowed'};

process_iq_captcha(_From, set, _Lang, SubEl, StateData) ->
    case ejabberd_captcha:process_reply(SubEl) of
	ok ->
	    {result, [], StateData};
	_ ->
	    {error, 'not-acceptable'}
    end.

get_title(StateData) ->
    case (StateData#state.config)#config.title of
	"" ->
	    binary_to_list(StateData#state.room);
	Name ->
	    Name
    end.

get_roomdesc_reply(JID, StateData, Tail) ->
    IsOccupantOrAdmin = is_occupant_or_admin(JID, StateData),
    if (StateData#state.config)#config.public or IsOccupantOrAdmin ->
	    if (StateData#state.config)#config.public_list or IsOccupantOrAdmin ->
		    {item, get_title(StateData) ++ Tail};
	       true ->
		    {item, get_title(StateData)}
	    end;
       true ->
	    false
    end.

get_roomdesc_tail(StateData, Lang) ->
    Desc = case (StateData#state.config)#config.public of
	       true ->
		   "";
	       _ ->
		   translate:translate(Lang, "private, ")
	   end,
    Len = ?DICT:fold(fun(_, _, Acc) -> Acc + 1 end, 0, StateData#state.users),
    " (" ++ Desc ++ integer_to_list(Len) ++ ")".

get_mucroom_disco_items(StateData) ->
    lists:map(
      fun({_LJID, Info}) ->
	      Nick = Info#user.nick,
              #xmlel{name = 'item', attrs = [?XMLATTR(<<"jid">>, 
                                            exmpp_jid:to_binary(
                                                    StateData#state.room,
                              				        StateData#state.host,
                            				        Nick)),
                                            ?XMLATTR(<<"name">>, 
                                                     Nick)]}
      end,
      ?DICT:to_list(StateData#state.users)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Invitation support

check_invitation(From, Els, Lang, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    CanInvite = (StateData#state.config)#config.allow_user_invites
	orelse (FAffiliation == admin) orelse (FAffiliation == owner),
    InviteEl = case Els of
            [#xmlel{ns = XMLNS, name = 'x'} = XEl] ->
                case XMLNS of
                    ?NS_MUC_USER -> ok;
                    _ -> throw({error, 'bad-request'})
                end,
                case exmpp_xml:get_child_elements(XEl) of
                    [#xmlel{name = 'invite'} = InviteEl1] ->
                        InviteEl1;
                    _ ->
                        throw({error, 'bad-request'})
                end;
            _ -> 
                throw({error, 'bad-request'})
            end,
    JID = try exmpp_jid:parse(exmpp_xml:get_attribute_as_binary(InviteEl,
                                                            <<"to">>,
                                                            false)) of
	        JID1 -> JID1
          catch
	      _:_ ->
    		  throw({error, 'jid-malformed'})
	      end,
    case CanInvite of
	false ->
	    throw({error, 'not-allowed'});
	true ->
	    Reason =
		exmpp_xml:get_path(
		  InviteEl,
		  [{element, 'reason'}, cdata]),
	    ContinueEl =
		case exmpp_xml:get_path(
		       InviteEl,
		       [{element, 'continue'}]) of
		    'undefined' -> [];
		    Continue1 -> [Continue1]
		end,
	    IEl =
	    [#xmlel{ns = ?NS_MUC_USER,
                name = 'invite', 
		        attrs = [?XMLATTR(<<"from">>, 
		                          exmpp_jid:to_binary(From))],
 		        children = [#xmlel{ns =?NS_MUC_USER, name = 'reason', 
		                            children = [#xmlcdata{cdata = Reason} ]}] 
  		                    ++ ContinueEl}],
	    PasswdEl =
		case (StateData#state.config)#config.password_protected of
		    true ->
			[#xmlel{ns = ?NS_MUC_USER, name = 'password',
			    children = [#xmlcdata{cdata = 
				(StateData#state.config)#config.password}]}];
		    _ ->
			[]
		end,
	    Body =
	    #xmlel{name = 'body',
	      children = [#xmlcdata{cdata =
		  list_to_binary([
		    io_lib:format(
		      translate:translate(Lang,
			"~s invites you to the room ~s"),
		      [exmpp_jid:to_binary(From),
			exmpp_jid:to_binary(StateData#state.room,
			  StateData#state.host)
		      ]), 
		   case (StateData#state.config)#config.password_protected of
		       true ->
			   ", " ++
			       translate:translate(Lang, "the password is") ++
			       " '" ++
			       (StateData#state.config)#config.password ++ "'";
		       _ ->
			   ""
		   end,
		   case Reason of
		       <<>> -> "";
		       _ -> [" (",  Reason, ") "]
		   end
          ])}]},
        %%TODO: always NS_JABBER_CLIENT?
	    Msg =
	    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message', 
	      attrs = [?XMLATTR(<<"type">>, <<"normal">>)], 
	      children = [#xmlel{ns = ?NS_MUC_USER, name = 'x',
		  children = IEl ++ PasswdEl},
		#xmlel{ns = 'jabber:x:conference', name = 'x',
		  attrs = [?XMLATTR(<<"jid">>,
		      exmpp_jid:to_binary(
			StateData#state.room,
			StateData#state.host)
			)],
		  children = [#xmlcdata{cdata = Reason}]},
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
    #xmlel{name = 'message'} = Packet,
    #xmlel{ns = ?NS_MUC_USER} = XEl = exmpp_xml:get_element(Packet, 'x'),
    DEl = exmpp_xml:get_element(XEl, 'decline'),
    ToString = exmpp_xml:get_attribute_as_binary(DEl, <<"to">>, false),
    ToJID = exmpp_jid:parse(ToString),
    {true, {Packet, XEl, DEl, ToJID}}.

%% Send the decline to the inviter user.
%% The original stanza must be slightly modified.
send_decline_invitation({Packet, XEl, DEl = #xmlel{name='decline'}, ToJID}, 
                            RoomJID, FromJID) ->
    FromString = exmpp_jid:bare_to_binary(FromJID),
    DEl1 = exmpp_xml:remove_attribute(DEl, <<"to">>),
    DEl2 = exmpp_xml:set_attribute(DEl1, <<"from">>,FromString),
    XEl2 = replace_subelement(XEl,DEl2),
    Packet2 = replace_subelement(Packet,XEl2),
    ejabberd_router:route(RoomJID, ToJID, Packet2).

%% Given an element and a new subelement, 
%% replace the instance of the subelement in element with the new subelement.
replace_subelement(#xmlel{children = Els} = El, #xmlel{name = Name} = NewSubEl) ->
    Els2 = lists:map(fun(#xmlel{name = Name2}) when Name2 =:= Name -> NewSubEl;
                        (S) -> S
                     end, Els),
    exmpp_xml:set_children(El, Els2).

send_error_only_occupants(Packet, Lang, RoomJID, From) ->
    ErrText = "Only occupants are allowed to send messages to the room",
    Err = exmpp_stanza:reply_with_error(
			    Packet, ?ERR(Packet, 'not-acceptable', Lang, ErrText)),
    ejabberd_router:route(RoomJID, From, Err).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging

add_to_log(Type, Data, StateData)
  when Type == roomconfig_change_disabledlogging ->
    %% When logging is disabled, the config change message must be logged:
    mod_muc_log:add_to_log(
      StateData#state.server_host, roomconfig_change, Data,
      StateData#state.jid, make_opts(StateData));
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
    LUser = exmpp_jid:prep_node(JID),
    LServer = exmpp_jid:prep_domain(JID),
    LResource = exmpp_jid:prep_resource(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:insert(
	    muc_online_users,
	    #muc_online_users{us = US, resource = LResource, room = Room, host = Host}).



tab_remove_online_user(JID, StateData) when ?IS_JID(JID) ->
 LUser = exmpp_jid:prep_node(JID),
 LServer = exmpp_jid:prep_domain(JID),
    LResource = exmpp_jid:prep_resource(JID),
    tab_remove_online_user({LUser, LServer, LResource},StateData);

tab_remove_online_user({LUser, LServer, LResource}, StateData) ->
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:delete_object(
	    muc_online_users,
	    #muc_online_users{us = US, resource = LResource, room = Room, host = Host}).

tab_count_user(JID) ->
    LUser = exmpp_jid:prep_node(JID),
    LServer = exmpp_jid:prep_domain(JID),
    US = {LUser, LServer},
    case catch ets:select(
		 muc_online_users,
		 [{#muc_online_users{us = US, _ = '_'}, [], [[]]}]) of
	Res when is_list(Res) ->
	    length(Res);
	_ ->
	    0
    end.
    
jid_replace_resource(JID, Resource) ->
    exmpp_jid:full(JID, Resource).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Multicast

send_multiple(From, Server, Users, Packet) ->
    JIDs = [ User#user.jid || {_, User} <- ?DICT:to_list(Users)],
    ejabberd_router_multicast:route_multicast(From, Server, JIDs, Packet).
