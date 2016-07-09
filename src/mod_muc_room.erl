%%%----------------------------------------------------------------------
%%% File    : mod_muc_room.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC room stuff
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_muc_room).

-author('alexey@process-one.net').

-behaviour(gen_fsm).

%% External exports
-export([start_link/9,
	 start_link/7,
	 start/9,
	 start/7,
	 get_role/2,
	 get_affiliation/2,
	 is_occupant_or_admin/2,
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
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_muc_room.hrl").

-define(MAX_USERS_DEFAULT_LIST,
	[5, 10, 20, 30, 50, 100, 200, 500, 1000, 2000, 5000]).

-define(DEFAULT_MAX_USERS_PRESENCE,1000).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
      Creator, Nick, DefRoomOpts) ->
    gen_fsm:start(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
			    RoomShaper, Creator, Nick, DefRoomOpts],
		    ?FSMOPTS).

start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    gen_fsm:start(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
			    RoomShaper, Opts],
		    ?FSMOPTS).

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

init([Host, ServerHost, Access, Room, HistorySize,
      RoomShaper, Creator, _Nick, DefRoomOpts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_affiliation(Creator, owner,
	    #state{host = Host, server_host = ServerHost,
		   access = Access, room = Room,
		   history = lqueue_new(HistorySize),
		   jid = jid:make(Room, Host, <<"">>),
		   just_created = true,
		   room_shaper = Shaper}),
    State1 = set_opts(DefRoomOpts, State),
    store_room(State1),
    ?INFO_MSG("Created MUC room ~s@~s by ~s",
	      [Room, Host, jid:to_string(Creator)]),
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
				  jid = jid:make(Room, Host, <<"">>),
				  room_shaper = Shaper}),
    add_to_log(room_existence, started, State),
    {ok, normal_state, State}.

normal_state({route, From, <<"">>,
	      #xmlel{name = <<"message">>, attrs = Attrs,
		     children = Els} =
		  Packet},
	     StateData) ->
    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
    case is_user_online(From, StateData) orelse
	   is_user_allowed_message_nonparticipant(From, StateData)
	of
      true ->
	  case fxml:get_attr_s(<<"type">>, Attrs) of
	    <<"groupchat">> ->
		Activity = get_user_activity(From, StateData),
		Now = p1_time_compat:system_time(micro_seconds),
		MinMessageInterval =
		    trunc(gen_mod:get_module_opt(StateData#state.server_host,
						 mod_muc, min_message_interval, fun(MMI) when is_number(MMI) -> MMI end, 0)
                          * 1000000),
		Size = element_size(Packet),
		{MessageShaper, MessageShaperInterval} =
		    shaper:update(Activity#activity.message_shaper, Size),
		if Activity#activity.message /= undefined ->
		       ErrText = <<"Traffic rate limit is exceeded">>,
		       Err = jlib:make_error_reply(Packet,
						   ?ERRT_RESOURCE_CONSTRAINT(Lang,
									     ErrText)),
		       ejabberd_router:route(StateData#state.jid, From, Err),
		       {next_state, normal_state, StateData};
		   Now >=
		     Activity#activity.message_time + MinMessageInterval,
		   MessageShaperInterval == 0 ->
		       {RoomShaper, RoomShaperInterval} =
			   shaper:update(StateData#state.room_shaper, Size),
		       RoomQueueEmpty =
			   queue:is_empty(StateData#state.room_queue),
		       if RoomShaperInterval == 0, RoomQueueEmpty ->
			      NewActivity = Activity#activity{message_time =
								  Now,
							      message_shaper =
								  MessageShaper},
			      StateData1 = store_user_activity(From,
							       NewActivity,
							       StateData),
			      StateData2 = StateData1#state{room_shaper =
								RoomShaper},
			      process_groupchat_message(From, Packet,
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
			      NewActivity = Activity#activity{message_time =
								  Now,
							      message_shaper =
								  MessageShaper,
							      message = Packet},
			      RoomQueue = queue:in({message, From},
						   StateData#state.room_queue),
			      StateData2 = store_user_activity(From,
							       NewActivity,
							       StateData1),
			      StateData3 = StateData2#state{room_queue =
								RoomQueue},
			      {next_state, normal_state, StateData3}
		       end;
		   true ->
		       MessageInterval = (Activity#activity.message_time +
					    MinMessageInterval
					    - Now)
					   div 1000,
		       Interval = lists:max([MessageInterval,
					     MessageShaperInterval]),
		       erlang:send_after(Interval, self(),
					 {process_user_message, From}),
		       NewActivity = Activity#activity{message = Packet,
						       message_shaper =
							   MessageShaper},
		       StateData1 = store_user_activity(From, NewActivity,
							StateData),
		       {next_state, normal_state, StateData1}
		end;
	    <<"error">> ->
		case is_user_online(From, StateData) of
		  true ->
		      ErrorText = <<"It is not allowed to send error messages to the"
				    " room. The participant (~s) has sent an error "
				    "message (~s) and got kicked from the room">>,
		      NewState = expulse_participant(Packet, From, StateData,
						     translate:translate(Lang,
									 ErrorText)),
		      close_room_if_temporary_and_empty(NewState);
		  _ -> {next_state, normal_state, StateData}
		end;
	    <<"chat">> ->
		ErrText =
		    <<"It is not allowed to send private messages "
		      "to the conference">>,
		Err = jlib:make_error_reply(Packet,
					    ?ERRT_NOT_ACCEPTABLE(Lang,
								 ErrText)),
		ejabberd_router:route(StateData#state.jid, From, Err),
		{next_state, normal_state, StateData};
	    Type when (Type == <<"">>) or (Type == <<"normal">>) ->
		IsInvitation = is_invitation(Els),
		IsVoiceRequest = is_voice_request(Els) and
				   is_visitor(From, StateData),
		IsVoiceApprovement = is_voice_approvement(Els) and
				       not is_visitor(From, StateData),
		if IsInvitation ->
		       case catch check_invitation(From, Packet, Lang, StateData)
			   of
			 {error, Error} ->
			     Err = jlib:make_error_reply(Packet, Error),
			     ejabberd_router:route(StateData#state.jid, From, Err),
			     {next_state, normal_state, StateData};
			 IJID ->
			     Config = StateData#state.config,
			     case Config#config.members_only of
			       true ->
				   case get_affiliation(IJID, StateData) of
				     none ->
					 NSD = set_affiliation(IJID, member,
							       StateData),
					 send_affiliation(IJID, member,
							  StateData),
					 store_room(NSD),
					 {next_state, normal_state, NSD};
				     _ -> {next_state, normal_state, StateData}
				   end;
			       false -> {next_state, normal_state, StateData}
			     end
		       end;
		   IsVoiceRequest ->
		       NewStateData = case
					(StateData#state.config)#config.allow_voice_requests
					  of
					true ->
					    MinInterval =
						(StateData#state.config)#config.voice_request_min_interval,
					    BareFrom =
						jid:remove_resource(jid:tolower(From)),
					    NowPriority = -p1_time_compat:system_time(micro_seconds),
					    CleanPriority = NowPriority +
							      MinInterval *
								1000000,
					    Times =
						clean_treap(StateData#state.last_voice_request_time,
							    CleanPriority),
					    case treap:lookup(BareFrom, Times)
						of
					      error ->
						  Times1 =
						      treap:insert(BareFrom,
								   NowPriority,
								   true, Times),
						  NSD =
						      StateData#state{last_voice_request_time
									  =
									  Times1},
						  send_voice_request(From, NSD),
						  NSD;
					      {ok, _, _} ->
						  ErrText =
						      <<"Please, wait for a while before sending "
							"new voice request">>,
						  Err =
						      jlib:make_error_reply(Packet,
									    ?ERRT_NOT_ACCEPTABLE(Lang,
												 ErrText)),
						  ejabberd_router:route(StateData#state.jid,
							       From, Err),
						  StateData#state{last_voice_request_time
								      = Times}
					    end;
					false ->
					    ErrText =
						<<"Voice requests are disabled in this "
						  "conference">>,
					    Err = jlib:make_error_reply(Packet,
									?ERRT_FORBIDDEN(Lang,
											ErrText)),
					    ejabberd_router:route(StateData#state.jid,
							 From, Err),
					    StateData
				      end,
		       {next_state, normal_state, NewStateData};
		   IsVoiceApprovement ->
		       NewStateData = case is_moderator(From, StateData) of
					true ->
					    case
					      extract_jid_from_voice_approvement(Els)
						of
					      error ->
						  ErrText =
						      <<"Failed to extract JID from your voice "
							"request approval">>,
						  Err =
						      jlib:make_error_reply(Packet,
									    ?ERRT_BAD_REQUEST(Lang,
											      ErrText)),
						  ejabberd_router:route(StateData#state.jid,
							       From, Err),
						  StateData;
					      {ok, TargetJid} ->
						  case is_visitor(TargetJid,
								  StateData)
						      of
						    true ->
							Reason = <<>>,
							NSD =
							    set_role(TargetJid,
								     participant,
								     StateData),
							catch
							  send_new_presence(TargetJid,
									    Reason,
									    NSD,
                                                                            StateData),
							NSD;
						    _ -> StateData
						  end
					    end;
					_ ->
					    ErrText =
						<<"Only moderators can approve voice requests">>,
					    Err = jlib:make_error_reply(Packet,
									?ERRT_NOT_ALLOWED(Lang,
											  ErrText)),
					    ejabberd_router:route(StateData#state.jid,
							 From, Err),
					    StateData
				      end,
		       {next_state, normal_state, NewStateData};
		   true -> {next_state, normal_state, StateData}
		end;
	    _ ->
		ErrText = <<"Improper message type">>,
		Err = jlib:make_error_reply(Packet,
					    ?ERRT_NOT_ACCEPTABLE(Lang,
								 ErrText)),
		ejabberd_router:route(StateData#state.jid, From, Err),
		{next_state, normal_state, StateData}
	  end;
      _ ->
	  case fxml:get_attr_s(<<"type">>, Attrs) of
	    <<"error">> -> ok;
	    _ ->
		handle_roommessage_from_nonparticipant(Packet, Lang,
						       StateData, From)
	  end,
	  {next_state, normal_state, StateData}
    end;
normal_state({route, From, <<"">>,
	      #xmlel{name = <<"iq">>} = Packet},
	     StateData) ->
    case jlib:iq_query_info(Packet) of
	reply ->
	    {next_state, normal_state, StateData};
	IQ0 ->
	    case ejabberd_hooks:run_fold(
		   muc_process_iq,
		   StateData#state.server_host,
		   IQ0, [StateData, From, StateData#state.jid]) of
		ignore ->
		    {next_state, normal_state, StateData};
		#iq{type = T} = IQRes when T == error; T == result ->
		    ejabberd_router:route(StateData#state.jid, From, jlib:iq_to_xml(IQRes)),
		    {next_state, normal_state, StateData};
		#iq{type = Type, xmlns = XMLNS, lang = Lang,
		    sub_el = #xmlel{name = SubElName, attrs = Attrs} = SubEl} = IQ
		  when (XMLNS == (?NS_MUC_ADMIN)) or
		       (XMLNS == (?NS_MUC_OWNER))
		       or (XMLNS == (?NS_DISCO_INFO))
		       or (XMLNS == (?NS_DISCO_ITEMS))
		       or (XMLNS == (?NS_VCARD))
		       or (XMLNS == (?NS_MUCSUB))
		       or (XMLNS == (?NS_CAPTCHA)) ->
		    Res1 = case XMLNS of
			       ?NS_MUC_ADMIN ->
				   process_iq_admin(From, Type, Lang, SubEl, StateData);
			       ?NS_MUC_OWNER ->
				   process_iq_owner(From, Type, Lang, SubEl, StateData);
			       ?NS_DISCO_INFO ->
				   case fxml:get_attr(<<"node">>, Attrs) of
				       false -> process_iq_disco_info(From, Type, Lang, StateData);
				       {value, _} ->
					   Txt = <<"Disco info is not available for this node">>,
					   {error, ?ERRT_SERVICE_UNAVAILABLE(Lang, Txt)}
				   end;
			       ?NS_DISCO_ITEMS ->
				   process_iq_disco_items(From, Type, Lang, StateData);
			       ?NS_VCARD ->
				   process_iq_vcard(From, Type, Lang, SubEl, StateData);
			       ?NS_MUCSUB ->
				   process_iq_mucsub(From, Packet, IQ, StateData);
			       ?NS_CAPTCHA ->
				   process_iq_captcha(From, Type, Lang, SubEl, StateData)
			   end,
		    {IQRes, NewStateData} =
			case Res1 of
			    {result, Res, SD} ->
				{IQ#iq{type = result,
				       sub_el =
					   [#xmlel{name = SubElName,
						   attrs =
						       [{<<"xmlns">>,
							 XMLNS}],
						   children = Res}]},
				 SD};
			    {ignore, SD} -> {ignore, SD};
			    {error, Error, ResStateData} ->
				{IQ#iq{type = error,
				       sub_el = [SubEl, Error]},
				 ResStateData};
			    {error, Error} ->
				{IQ#iq{type = error,
				       sub_el = [SubEl, Error]},
				 StateData}
			end,
		    if IQRes /= ignore ->
			    ejabberd_router:route(
			      StateData#state.jid, From, jlib:iq_to_xml(IQRes));
		       true ->
			    ok
		    end,
		    case NewStateData of
			stop -> {stop, normal, StateData};
			_ -> {next_state, normal_state, NewStateData}
		    end;
		_ ->
		    Err = jlib:make_error_reply(Packet,
						?ERR_FEATURE_NOT_IMPLEMENTED),
		    ejabberd_router:route(StateData#state.jid, From, Err),
		    {next_state, normal_state, StateData}
	    end
    end;
normal_state({route, From, Nick,
	      #xmlel{name = <<"presence">>} = Packet},
	     StateData) ->
    Activity = get_user_activity(From, StateData),
    Now = p1_time_compat:system_time(micro_seconds),
    MinPresenceInterval =
	trunc(gen_mod:get_module_opt(StateData#state.server_host,
				     mod_muc, min_presence_interval,
                                     fun(I) when is_number(I), I>=0 ->
                                             I
                                     end, 0)
              * 1000000),
    if (Now >=
	  Activity#activity.presence_time + MinPresenceInterval)
	 and (Activity#activity.presence == undefined) ->
	   NewActivity = Activity#activity{presence_time = Now},
	   StateData1 = store_user_activity(From, NewActivity,
					    StateData),
	   process_presence(From, Nick, Packet, StateData1);
       true ->
	   if Activity#activity.presence == undefined ->
		  Interval = (Activity#activity.presence_time +
				MinPresenceInterval
				- Now)
			       div 1000,
		  erlang:send_after(Interval, self(),
				    {process_user_presence, From});
	      true -> ok
	   end,
	   NewActivity = Activity#activity{presence =
					       {Nick, Packet}},
	   StateData1 = store_user_activity(From, NewActivity,
					    StateData),
	   {next_state, normal_state, StateData1}
    end;
normal_state({route, From, ToNick,
	      #xmlel{name = <<"message">>, attrs = Attrs} = Packet},
	     StateData) ->
    Type = fxml:get_attr_s(<<"type">>, Attrs),
    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
    case decide_fate_message(Type, Packet, From, StateData)
	of
      {expulse_sender, Reason} ->
	  ?DEBUG(Reason, []),
	  ErrorText = <<"It is not allowed to send error messages to the"
		    " room. The participant (~s) has sent an error "
		    "message (~s) and got kicked from the room">>,
	  NewState = expulse_participant(Packet, From, StateData,
					 translate:translate(Lang, ErrorText)),
	  {next_state, normal_state, NewState};
      forget_message -> {next_state, normal_state, StateData};
      continue_delivery ->
	  case
	    {(StateData#state.config)#config.allow_private_messages,
	     is_user_online(From, StateData)}
	      of
	    {true, true} ->
		case Type of
		  <<"groupchat">> ->
		      ErrText =
			  <<"It is not allowed to send private messages "
			    "of type \"groupchat\"">>,
		      Err = jlib:make_error_reply(Packet,
						  ?ERRT_BAD_REQUEST(Lang,
								    ErrText)),
		      ejabberd_router:route(jid:replace_resource(StateData#state.jid,
							     ToNick),
				   From, Err);
		  _ ->
		      case find_jids_by_nick(ToNick, StateData) of
			false ->
			    ErrText =
				<<"Recipient is not in the conference room">>,
			    Err = jlib:make_error_reply(Packet,
							?ERRT_ITEM_NOT_FOUND(Lang,
									     ErrText)),
			    ejabberd_router:route(jid:replace_resource(StateData#state.jid,
								   ToNick),
					 From, Err);
			ToJIDs ->
			    SrcIsVisitor = is_visitor(From, StateData),
			    DstIsModerator = is_moderator(hd(ToJIDs),
							  StateData),
			    PmFromVisitors =
				(StateData#state.config)#config.allow_private_messages_from_visitors,
			    if SrcIsVisitor == false;
			       PmFromVisitors == anyone;
			       (PmFromVisitors == moderators) and
				 DstIsModerator ->
				   {ok, #user{nick = FromNick}} =
				       (?DICT):find(jid:tolower(From),
						    StateData#state.users),
				   FromNickJID =
				       jid:replace_resource(StateData#state.jid,
								 FromNick),
				   X = #xmlel{name = <<"x">>,
					      attrs = [{<<"xmlns">>, ?NS_MUC_USER}]},
				   PrivMsg = fxml:append_subtags(Packet, [X]),
				   [ejabberd_router:route(FromNickJID, ToJID, PrivMsg)
				    || ToJID <- ToJIDs];
			       true ->
				   ErrText =
				       <<"It is not allowed to send private messages">>,
				   Err = jlib:make_error_reply(Packet,
							       ?ERRT_FORBIDDEN(Lang,
									       ErrText)),
				   ejabberd_router:route(jid:replace_resource(StateData#state.jid,
									  ToNick),
						From, Err)
			    end
		      end
		end;
	    {true, false} ->
		ErrText =
		    <<"Only occupants are allowed to send messages "
		      "to the conference">>,
		Err = jlib:make_error_reply(Packet,
					    ?ERRT_NOT_ACCEPTABLE(Lang,
								 ErrText)),
		ejabberd_router:route(jid:replace_resource(StateData#state.jid,
						       ToNick),
			     From, Err);
	    {false, _} ->
		ErrText =
		    <<"It is not allowed to send private messages">>,
		Err = jlib:make_error_reply(Packet,
					    ?ERRT_FORBIDDEN(Lang, ErrText)),
		ejabberd_router:route(jid:replace_resource(StateData#state.jid,
						       ToNick),
			     From, Err)
	  end,
	  {next_state, normal_state, StateData}
    end;
normal_state({route, From, ToNick,
	      #xmlel{name = <<"iq">>, attrs = Attrs} = Packet},
	     StateData) ->
    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
    StanzaId = fxml:get_attr_s(<<"id">>, Attrs),
    case {(StateData#state.config)#config.allow_query_users,
	  is_user_online_iq(StanzaId, From, StateData)}
	of
      {true, {true, NewId, FromFull}} ->
	  case find_jid_by_nick(ToNick, StateData) of
	    false ->
		case jlib:iq_query_info(Packet) of
		  reply -> ok;
		  _ ->
		      ErrText = <<"Recipient is not in the conference room">>,
		      Err = jlib:make_error_reply(Packet,
						  ?ERRT_ITEM_NOT_FOUND(Lang,
								       ErrText)),
		      ejabberd_router:route(jid:replace_resource(StateData#state.jid,
							     ToNick),
				   From, Err)
		end;
	    ToJID ->
		{ok, #user{nick = FromNick}} =
		    (?DICT):find(jid:tolower(FromFull),
				 StateData#state.users),
		{ToJID2, Packet2} = handle_iq_vcard(FromFull, ToJID,
						    StanzaId, NewId, Packet),
		ejabberd_router:route(jid:replace_resource(StateData#state.jid,
						       FromNick),
			     ToJID2, Packet2)
	  end;
      {_, {false, _, _}} ->
	  case jlib:iq_query_info(Packet) of
	    reply -> ok;
	    _ ->
		ErrText =
		    <<"Only occupants are allowed to send queries "
		      "to the conference">>,
		Err = jlib:make_error_reply(Packet,
					    ?ERRT_NOT_ACCEPTABLE(Lang,
								 ErrText)),
		ejabberd_router:route(jid:replace_resource(StateData#state.jid,
						       ToNick),
			     From, Err)
	  end;
      _ ->
	  case jlib:iq_query_info(Packet) of
	    reply -> ok;
	    _ ->
		ErrText = <<"Queries to the conference members are "
			    "not allowed in this room">>,
		Err = jlib:make_error_reply(Packet,
					    ?ERRT_NOT_ALLOWED(Lang, ErrText)),
		ejabberd_router:route(jid:replace_resource(StateData#state.jid,
						       ToNick),
			     From, Err)
	  end
    end,
    {next_state, normal_state, StateData};
normal_state(_Event, StateData) ->
    {next_state, normal_state, StateData}.

handle_event({service_message, Msg}, _StateName,
	     StateData) ->
    MessagePkt = #xmlel{name = <<"message">>,
			attrs = [{<<"type">>, <<"groupchat">>}],
			children =
			    [#xmlel{name = <<"body">>, attrs = [],
				    children = [{xmlcdata, Msg}]}]},
    send_wrapped_multiple(
      StateData#state.jid,
      StateData#state.users,
      MessagePkt,
      ?NS_MUCSUB_NODES_MESSAGES,
      StateData),
    NSD = add_message_to_history(<<"">>,
				 StateData#state.jid, MessagePkt, StateData),
    {next_state, normal_state, NSD};
handle_event({destroy, Reason}, _StateName,
	     StateData) ->
    {result, [], stop} = destroy_room(#xmlel{name =
						 <<"destroy">>,
					     attrs =
						 [{<<"xmlns">>, ?NS_MUC_OWNER}],
					     children =
						 case Reason of
						   none -> [];
						   _Else ->
						       [#xmlel{name =
								   <<"reason">>,
							       attrs = [],
							       children =
								   [{xmlcdata,
								     Reason}]}]
						 end},
				      StateData),
    ?INFO_MSG("Destroyed MUC room ~s with reason: ~p",
	      [jid:to_string(StateData#state.jid), Reason]),
    add_to_log(room_existence, destroyed, StateData),
    {stop, shutdown, StateData};
handle_event(destroy, StateName, StateData) ->
    ?INFO_MSG("Destroyed MUC room ~s",
	      [jid:to_string(StateData#state.jid)]),
    handle_event({destroy, none}, StateName, StateData);
handle_event({set_affiliations, Affiliations},
	     StateName, StateData) ->
    {next_state, StateName,
     StateData#state{affiliations = Affiliations}};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event({get_disco_item, Filter, JID, Lang}, _From, StateName, StateData) ->
    Len = ?DICT:fold(fun(_, _, Acc) -> Acc + 1 end, 0,
                    StateData#state.users),
    Reply = case (Filter == all) or (Filter == Len) or ((Filter /= 0) and (Len /= 0)) of
	true ->
	    get_roomdesc_reply(JID, StateData,
			       get_roomdesc_tail(StateData, Lang));
	false ->
	    false
    end,
    {reply, Reply, StateName, StateData};
%% This clause is only for backwards compatibility
handle_sync_event({get_disco_item, JID, Lang}, From, StateName, StateData) ->
    handle_sync_event({get_disco_item, any, JID, Lang}, From, StateName, StateData);
handle_sync_event(get_config, _From, StateName,
		  StateData) ->
    {reply, {ok, StateData#state.config}, StateName,
     StateData};
handle_sync_event(get_state, _From, StateName,
		  StateData) ->
    {reply, {ok, StateData}, StateName, StateData};
handle_sync_event({change_config, Config}, _From,
		  StateName, StateData) ->
    {result, [], NSD} = change_config(Config, StateData),
    {reply, {ok, NSD#state.config}, StateName, NSD};
handle_sync_event({change_state, NewStateData}, _From,
		  StateName, _StateData) ->
    {reply, {ok, NewStateData}, StateName, NewStateData};
handle_sync_event({process_item_change, Item, UJID}, _From, StateName, StateData) ->
    NSD = process_item_change(Item, StateData, UJID),
    {reply, {ok, NSD}, StateName, NSD};
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_info({process_user_presence, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({presence, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if RoomQueueEmpty ->
	   StateData2 = prepare_room_queue(StateData1),
	   {next_state, normal_state, StateData2};
       true -> {next_state, normal_state, StateData1}
    end;
handle_info({process_user_message, From},
	    normal_state = _StateName, StateData) ->
    RoomQueueEmpty =
	queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({message, From},
			 StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if RoomQueueEmpty ->
	   StateData2 = prepare_room_queue(StateData1),
	   {next_state, normal_state, StateData2};
       true -> {next_state, normal_state, StateData1}
    end;
handle_info(process_room_queue,
	    normal_state = StateName, StateData) ->
    case queue:out(StateData#state.room_queue) of
      {{value, {message, From}}, RoomQueue} ->
	  Activity = get_user_activity(From, StateData),
	  Packet = Activity#activity.message,
	  NewActivity = Activity#activity{message = undefined},
	  StateData1 = store_user_activity(From, NewActivity,
					   StateData),
	  StateData2 = StateData1#state{room_queue = RoomQueue},
	  StateData3 = prepare_room_queue(StateData2),
	  process_groupchat_message(From, Packet, StateData3);
      {{value, {presence, From}}, RoomQueue} ->
	  Activity = get_user_activity(From, StateData),
	  {Nick, Packet} = Activity#activity.presence,
	  NewActivity = Activity#activity{presence = undefined},
	  StateData1 = store_user_activity(From, NewActivity,
					   StateData),
	  StateData2 = StateData1#state{room_queue = RoomQueue},
	  StateData3 = prepare_room_queue(StateData2),
	  process_presence(From, Nick, Packet, StateData3);
      {empty, _} -> {next_state, StateName, StateData}
    end;
handle_info({captcha_succeed, From}, normal_state,
	    StateData) ->
    NewState = case (?DICT):find(From,
				 StateData#state.robots)
		   of
		 {ok, {Nick, Packet}} ->
		     Robots = (?DICT):store(From, passed,
					    StateData#state.robots),
		     add_new_user(From, Nick, Packet,
				  StateData#state{robots = Robots});
		 _ -> StateData
	       end,
    {next_state, normal_state, NewState};
handle_info({captcha_failed, From}, normal_state,
	    StateData) ->
    NewState = case (?DICT):find(From,
				 StateData#state.robots)
		   of
		 {ok, {Nick, Packet}} ->
		     Robots = (?DICT):erase(From, StateData#state.robots),
		     Txt = <<"The CAPTCHA verification has failed">>,
		     Err = jlib:make_error_reply(
			     Packet, ?ERRT_NOT_AUTHORIZED(?MYLANG, Txt)),
		     ejabberd_router:route % TODO: s/Nick/""/
				 (jid:replace_resource(StateData#state.jid,
							    Nick),
				  From, Err),
		     StateData#state{robots = Robots};
		 _ -> StateData
	       end,
    {next_state, normal_state, NewState};
handle_info(shutdown, _StateName, StateData) ->
    {stop, shutdown, StateData};
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, _StateName, StateData) ->
    ?INFO_MSG("Stopping MUC room ~s@~s",
	      [StateData#state.room, StateData#state.host]),
    ReasonT = case Reason of
		shutdown ->
		    <<"You are being removed from the room "
		      "because of a system shutdown">>;
		_ -> <<"Room terminates">>
	      end,
    ItemAttrs = [{<<"affiliation">>, <<"none">>},
		 {<<"role">>, <<"none">>}],
    ReasonEl = #xmlel{name = <<"reason">>, attrs = [],
		      children = [{xmlcdata, ReasonT}]},
    Packet = #xmlel{name = <<"presence">>,
		    attrs = [{<<"type">>, <<"unavailable">>}],
		    children =
			[#xmlel{name = <<"x">>,
				attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
				children =
				    [#xmlel{name = <<"item">>,
					    attrs = ItemAttrs,
					    children = [ReasonEl]},
				     #xmlel{name = <<"status">>,
					    attrs = [{<<"code">>, <<"332">>}],
					    children = []}]}]},
    (?DICT):fold(fun (LJID, Info, _) ->
			 Nick = Info#user.nick,
			 case Reason of
			   shutdown ->
			       send_wrapped(jid:replace_resource(StateData#state.jid,
								 Nick),
					    Info#user.jid, Packet,
					    ?NS_MUCSUB_NODES_PARTICIPANTS,
					    StateData);
			   _ -> ok
			 end,
			 tab_remove_online_user(LJID, StateData)
		 end,
		 [], StateData#state.users),
    add_to_log(room_existence, stopped, StateData),
    mod_muc:room_destroyed(StateData#state.host, StateData#state.room, self(),
			   StateData#state.server_host),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

route(Pid, From, ToNick, Packet) ->
    gen_fsm:send_event(Pid, {route, From, ToNick, Packet}).

process_groupchat_message(From,
			  #xmlel{name = <<"message">>, attrs = Attrs} = Packet,
			  StateData) ->
    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
    case is_user_online(From, StateData) orelse
	   is_user_allowed_message_nonparticipant(From, StateData)
	of
      true ->
	  {FromNick, Role, IsSubscriber} = get_participant_data(From, StateData),
	  if (Role == moderator) or (Role == participant) or IsSubscriber or
	       ((StateData#state.config)#config.moderated == false) ->
		 Subject = check_subject(Packet),
		 {NewStateData1, IsAllowed} = case Subject of
						false -> {StateData, true};
						_ ->
						    case
						      can_change_subject(Role,
									 StateData)
							of
						      true ->
							  NSD =
							      StateData#state{subject
										  =
										  Subject,
									      subject_author
										  =
										  FromNick},
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
						 [StateData,
						  StateData#state.jid,
						  From, FromNick])
			   of
			 drop ->
			     {next_state, normal_state, StateData};
			 NewPacket1 ->
			     NewPacket = fxml:remove_subtags(NewPacket1, <<"nick">>, {<<"xmlns">>, ?NS_NICK}),
			     Node = if Subject == false -> ?NS_MUCSUB_NODES_MESSAGES;
				       true -> ?NS_MUCSUB_NODES_SUBJECT
				    end,
			     send_wrapped_multiple(
			       jid:replace_resource(StateData#state.jid, FromNick),
			       StateData#state.users,
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
		       Err = case
			       (StateData#state.config)#config.allow_change_subj
				 of
			       true ->
				   ?ERRT_FORBIDDEN(Lang,
						   <<"Only moderators and participants are "
						     "allowed to change the subject in this "
						     "room">>);
			       _ ->
				   ?ERRT_FORBIDDEN(Lang,
						   <<"Only moderators are allowed to change "
						     "the subject in this room">>)
			     end,
		       ejabberd_router:route(StateData#state.jid, From,
				    jlib:make_error_reply(Packet, Err)),
		       {next_state, normal_state, StateData}
		 end;
	     true ->
		 ErrText = <<"Visitors are not allowed to send messages "
			     "to all occupants">>,
		 Err = jlib:make_error_reply(Packet,
					     ?ERRT_FORBIDDEN(Lang, ErrText)),
		 ejabberd_router:route(StateData#state.jid, From, Err),
		 {next_state, normal_state, StateData}
	  end;
      false ->
	  ErrText =
	      <<"Only occupants are allowed to send messages "
		"to the conference">>,
	  Err = jlib:make_error_reply(Packet,
				      ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
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
is_user_allowed_message_nonparticipant(JID,
				       StateData) ->
    case get_service_affiliation(JID, StateData) of
      owner -> true;
      _ -> false
    end.

%% @doc Get information of this participant, or default values.
%% If the JID is not a participant, return values for a service message.
get_participant_data(From, StateData) ->
    case (?DICT):find(jid:tolower(From),
		      StateData#state.users)
	of
      {ok, #user{nick = FromNick, role = Role, is_subscriber = IsSubscriber}} ->
	  {FromNick, Role, IsSubscriber};
      error -> {<<"">>, moderator, false}
    end.

process_presence(From, Nick,
		 #xmlel{name = <<"presence">>, attrs = Attrs0} = Packet0,
		 StateData) ->
    Type0 = fxml:get_attr_s(<<"type">>, Attrs0),
    IsOnline = is_user_online(From, StateData),
    IsSubscriber = is_subscriber(From, StateData),
    if Type0 == <<"">>;
       IsOnline and ((Type0 == <<"unavailable">>) or (Type0 == <<"error">>)) ->
	   case ejabberd_hooks:run_fold(muc_filter_presence,
					StateData#state.server_host,
					Packet0,
					[StateData,
					 StateData#state.jid,
					 From, Nick]) of
	     drop ->
		 {next_state, normal_state, StateData};
	     #xmlel{attrs = Attrs} = Packet ->
		 Type = fxml:get_attr_s(<<"type">>, Attrs),
		 Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
		 StateData1 = case Type of
				<<"unavailable">> ->
				    NewPacket = case
						  {(StateData#state.config)#config.allow_visitor_status,
						   is_visitor(From, StateData)}
						    of
						  {false, true} ->
						      strip_status(Packet);
						  _ -> Packet
						end,
				    NewState = add_user_presence_un(From, NewPacket,
								    StateData),
				    case (?DICT):find(Nick, StateData#state.nicks) of
				      {ok, [_, _ | _]} -> ok;
				      _ -> send_new_presence(From, NewState, StateData)
				    end,
				    Reason = case fxml:get_subtag(NewPacket,
								 <<"status">>)
						 of
					       false -> <<"">>;
					       Status_el ->
						   fxml:get_tag_cdata(Status_el)
					     end,
				    remove_online_user(From, NewState, IsSubscriber, Reason);
				<<"error">> ->
				    ErrorText = <<"It is not allowed to send error messages to the"
					" room. The participant (~s) has sent an error "
					"message (~s) and got kicked from the room">>,
				    expulse_participant(Packet, From, StateData,
							translate:translate(Lang,
									    ErrorText));
				<<"">> ->
				    if not IsOnline ->
					   add_new_user(From, Nick, Packet, StateData);
				       true ->
					   case is_nick_change(From, Nick, StateData) of
					     true ->
						 case {nick_collision(From, Nick, StateData),
						       mod_muc:can_use_nick(StateData#state.server_host,
									    StateData#state.host,
									    From, Nick),
						       {(StateData#state.config)#config.allow_visitor_nickchange,
							is_visitor(From, StateData)}}
						     of
						   {_, _, {false, true}} ->
						       ErrText =
							   <<"Visitors are not allowed to change their "
							     "nicknames in this room">>,
						       Err = jlib:make_error_reply(Packet,
										   ?ERRT_NOT_ALLOWED(Lang,
												     ErrText)),
						       ejabberd_router:route(jid:replace_resource(StateData#state.jid,
											      Nick),
								    From, Err),
						       StateData;
						   {true, _, _} ->
						       Lang = fxml:get_attr_s(<<"xml:lang">>,
									     Attrs),
						       ErrText =
							   <<"That nickname is already in use by another "
							     "occupant">>,
						       Err = jlib:make_error_reply(Packet,
										   ?ERRT_CONFLICT(Lang,
												  ErrText)),
						       ejabberd_router:route(jid:replace_resource(StateData#state.jid,
											      Nick), % TODO: s/Nick/""/
								    From, Err),
						       StateData;
						   {_, false, _} ->
						       ErrText =
							   <<"That nickname is registered by another "
							     "person">>,
						       Err = jlib:make_error_reply(Packet,
										   ?ERRT_CONFLICT(Lang,
												  ErrText)),
						       ejabberd_router:route(jid:replace_resource(StateData#state.jid,
											      Nick),
								   From, Err),
						       StateData;
						   _ ->
						       case is_initial_presence(From, StateData) of
							   true ->
							       subscriber_becomes_available(
								 From, Nick, Packet, StateData);
							   false ->
							       change_nick(From, Nick, StateData)
						       end
						 end;
					     _NotNickChange ->
						 case is_initial_presence(From, StateData) of
						     true ->
							 subscriber_becomes_available(
							   From, Nick, Packet, StateData);
						     false ->
							 Stanza = maybe_strip_status_from_presence(
								    From, Packet, StateData),
							 NewState = add_user_presence(From, Stanza,
										      StateData),
							 send_new_presence(From, NewState, StateData),
							 NewState
						 end
					   end
				    end
			      end,
		 close_room_if_temporary_and_empty(StateData1)
	   end;
       true ->
	   {next_state, normal_state, StateData}
    end.

maybe_strip_status_from_presence(From, Packet, StateData) ->
    case {(StateData#state.config)#config.allow_visitor_status,
	  is_visitor(From, StateData)} of
	{false, true} ->
	    strip_status(Packet);
	_Allowed -> Packet
    end.

subscriber_becomes_available(From, Nick, Packet, StateData) ->
    Stanza = maybe_strip_status_from_presence(From, Packet, StateData),
    State1 = add_user_presence(From, Stanza, StateData),
    Aff = get_affiliation(From, State1),
    Role = get_default_role(Aff, State1),
    State2 = set_role(From, Role, State1),
    State3 = set_nick(From, Nick, State2),
    send_existing_presences(From, State3),
    send_initial_presence(From, State3, StateData),
    State3.

close_room_if_temporary_and_empty(StateData1) ->
    case not (StateData1#state.config)#config.persistent
	   andalso (?DICT):to_list(StateData1#state.users) == []
	of
      true ->
	  ?INFO_MSG("Destroyed MUC room ~s because it's temporary "
		    "and empty",
		    [jid:to_string(StateData1#state.jid)]),
	  add_to_log(room_existence, destroyed, StateData1),
	  {stop, normal, StateData1};
      _ -> {next_state, normal_state, StateData1}
    end.

is_user_online(JID, StateData) ->
    LJID = jid:tolower(JID),
    (?DICT):is_key(LJID, StateData#state.users).

is_subscriber(JID, StateData) ->
    LJID = jid:tolower(JID),
    case (?DICT):find(LJID, StateData#state.users) of
	{ok, #user{is_subscriber = IsSubscriber}} ->
	    IsSubscriber;
	_ ->
	    false
    end.

%% Check if the user is occupant of the room, or at least is an admin or owner.
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

%%%
%%% Handle IQ queries of vCard
%%%
is_user_online_iq(StanzaId, JID, StateData)
    when JID#jid.lresource /= <<"">> ->
    {is_user_online(JID, StateData), StanzaId, JID};
is_user_online_iq(StanzaId, JID, StateData)
    when JID#jid.lresource == <<"">> ->
    try stanzaid_unpack(StanzaId) of
      {OriginalId, Resource} ->
	  JIDWithResource = jid:replace_resource(JID,
						      Resource),
	  {is_user_online(JIDWithResource, StateData), OriginalId,
	   JIDWithResource}
    catch
      _:_ -> {is_user_online(JID, StateData), StanzaId, JID}
    end.

handle_iq_vcard(FromFull, ToJID, StanzaId, NewId,
		Packet) ->
    ToBareJID = jid:remove_resource(ToJID),
    IQ = jlib:iq_query_info(Packet),
    handle_iq_vcard2(FromFull, ToJID, ToBareJID, StanzaId,
		     NewId, IQ, Packet).

handle_iq_vcard2(_FromFull, ToJID, ToBareJID, StanzaId,
		 _NewId, #iq{type = get, xmlns = ?NS_VCARD}, Packet)
    when ToBareJID /= ToJID ->
    {ToBareJID, change_stanzaid(StanzaId, ToJID, Packet)};
handle_iq_vcard2(_FromFull, ToJID, _ToBareJID,
		 _StanzaId, NewId, _IQ, Packet) ->
    {ToJID, change_stanzaid(NewId, Packet)}.

stanzaid_pack(OriginalId, Resource) ->
    <<"berd",
      (jlib:encode_base64(<<"ejab\000",
		       OriginalId/binary, "\000",
		       Resource/binary>>))/binary>>.

stanzaid_unpack(<<"berd", StanzaIdBase64/binary>>) ->
    StanzaId = jlib:decode_base64(StanzaIdBase64),
    [<<"ejab">>, OriginalId, Resource] =
	str:tokens(StanzaId, <<"\000">>),
    {OriginalId, Resource}.

change_stanzaid(NewId, Packet) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} =
	jlib:remove_attr(<<"id">>, Packet),
    #xmlel{name = Name, attrs = [{<<"id">>, NewId} | Attrs],
	   children = Els}.

change_stanzaid(PreviousId, ToJID, Packet) ->
    NewId = stanzaid_pack(PreviousId, ToJID#jid.lresource),
    change_stanzaid(NewId, Packet).

%%%
%%%

role_to_list(Role) ->
    case Role of
      moderator -> <<"moderator">>;
      participant -> <<"participant">>;
      visitor -> <<"visitor">>;
      none -> <<"none">>
    end.

affiliation_to_list(Affiliation) ->
    case Affiliation of
      owner -> <<"owner">>;
      admin -> <<"admin">>;
      member -> <<"member">>;
      outcast -> <<"outcast">>;
      none -> <<"none">>
    end.

list_to_role(Role) ->
    case Role of
      <<"moderator">> -> moderator;
      <<"participant">> -> participant;
      <<"visitor">> -> visitor;
      <<"none">> -> none
    end.

list_to_affiliation(Affiliation) ->
    case Affiliation of
      <<"owner">> -> owner;
      <<"admin">> -> admin;
      <<"member">> -> member;
      <<"outcast">> -> outcast;
      <<"none">> -> none
    end.

%% Decide the fate of the message and its sender
%% Returns: continue_delivery | forget_message | {expulse_sender, Reason}
decide_fate_message(<<"error">>, Packet, From,
		    StateData) ->
    PD = case check_error_kick(Packet) of
	   %% If this is an error stanza and its condition matches a criteria
	   true ->
	       Reason =
		   io_lib:format("This participant is considered a ghost "
				 "and is expulsed: ~s",
				 [jid:to_string(From)]),
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
decide_fate_message(_, _, _, _) -> continue_delivery.

%% Check if the elements of this error stanza indicate
%% that the sender is a dead participant.
%% If so, return true to kick the participant.
check_error_kick(Packet) ->
    case get_error_condition(Packet) of
      <<"gone">> -> true;
      <<"internal-server-error">> -> true;
      <<"item-not-found">> -> true;
      <<"jid-malformed">> -> true;
      <<"recipient-unavailable">> -> true;
      <<"redirect">> -> true;
      <<"remote-server-not-found">> -> true;
      <<"remote-server-timeout">> -> true;
      <<"service-unavailable">> -> true;
      _ -> false
    end.

get_error_condition(Packet) ->
    case catch get_error_condition2(Packet) of
      {condition, ErrorCondition} -> ErrorCondition;
      {'EXIT', _} -> <<"badformed error stanza">>
    end.

get_error_condition2(Packet) ->
    #xmlel{children = EEls} = fxml:get_subtag(Packet,
					     <<"error">>),
    [Condition] = [Name
		   || #xmlel{name = Name,
			     attrs = [{<<"xmlns">>, ?NS_STANZAS}],
			     children = []}
			  <- EEls],
    {condition, Condition}.

make_reason(Packet, From, StateData, Reason1) ->
    {ok, #user{nick = FromNick}} = (?DICT):find(jid:tolower(From), StateData#state.users),
    Condition = get_error_condition(Packet),
    iolist_to_binary(io_lib:format(Reason1, [FromNick, Condition])).

expulse_participant(Packet, From, StateData, Reason1) ->
    IsSubscriber = is_subscriber(From, StateData),
    Reason2 = make_reason(Packet, From, StateData, Reason1),
    NewState = add_user_presence_un(From,
				    #xmlel{name = <<"presence">>,
					   attrs =
					       [{<<"type">>,
						 <<"unavailable">>}],
					   children =
					       [#xmlel{name = <<"status">>,
						       attrs = [],
						       children =
							   [{xmlcdata,
							     Reason2}]}]},
				    StateData),
    send_new_presence(From, NewState, StateData),
    remove_online_user(From, NewState, IsSubscriber).

set_affiliation(JID, Affiliation, StateData) ->
    set_affiliation(JID, Affiliation, StateData, <<"">>).

set_affiliation(JID, Affiliation, StateData, Reason) ->
    LJID = jid:remove_resource(jid:tolower(JID)),
    Affiliations = case Affiliation of
		     none ->
			 (?DICT):erase(LJID, StateData#state.affiliations);
		     _ ->
			 (?DICT):store(LJID, {Affiliation, Reason},
				       StateData#state.affiliations)
		   end,
    StateData#state{affiliations = Affiliations}.

get_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin,
     _AccessPersistent} =
	StateData#state.access,
    Res = case acl:match_rule(StateData#state.server_host,
			      AccessAdmin, JID)
	      of
	    allow -> owner;
	    _ ->
		LJID = jid:tolower(JID),
		case (?DICT):find(LJID, StateData#state.affiliations) of
		  {ok, Affiliation} -> Affiliation;
		  _ ->
		      LJID1 = jid:remove_resource(LJID),
		      case (?DICT):find(LJID1, StateData#state.affiliations)
			  of
			{ok, Affiliation} -> Affiliation;
			_ ->
			    LJID2 = setelement(1, LJID, <<"">>),
			    case (?DICT):find(LJID2,
					      StateData#state.affiliations)
				of
			      {ok, Affiliation} -> Affiliation;
			      _ ->
				  LJID3 = jid:remove_resource(LJID2),
				  case (?DICT):find(LJID3,
						    StateData#state.affiliations)
				      of
				    {ok, Affiliation} -> Affiliation;
				    _ -> none
				  end
			    end
		      end
		end
	  end,
    case Res of
      {A, _Reason} -> A;
      _ -> Res
    end.

get_service_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin,
     _AccessPersistent} =
	StateData#state.access,
    case acl:match_rule(StateData#state.server_host,
			AccessAdmin, JID)
	of
      allow -> owner;
      _ -> none
    end.

set_role(JID, Role, StateData) ->
    LJID = jid:tolower(JID),
    LJIDs = case LJID of
	      {U, S, <<"">>} ->
		  (?DICT):fold(fun (J, _, Js) ->
				       case J of
					 {U, S, _} -> [J | Js];
					 _ -> Js
				       end
			       end,
			       [], StateData#state.users);
	      _ ->
		  case (?DICT):is_key(LJID, StateData#state.users) of
		    true -> [LJID];
		    _ -> []
		  end
	    end,
    {Users, Nicks} = case Role of
		       none ->
			   lists:foldl(fun (J, {Us, Ns}) ->
					       NewNs = case (?DICT):find(J, Us)
							   of
							 {ok,
							  #user{nick = Nick}} ->
							     (?DICT):erase(Nick,
									   Ns);
							 _ -> Ns
						       end,
					       {(?DICT):erase(J, Us), NewNs}
				       end,
				       {StateData#state.users,
					StateData#state.nicks},
				       LJIDs);
		       _ ->
			   {lists:foldl(
			      fun (J, Us) ->
				      {ok, User} = (?DICT):find(J, Us),
				      if User#user.last_presence == undefined ->
					      Us;
					 true ->
					      (?DICT):store(J, User#user{role = Role}, Us)
				      end
			      end,
			      StateData#state.users, LJIDs),
			    StateData#state.nicks}
		     end,
    StateData#state{users = Users, nicks = Nicks}.

get_role(JID, StateData) ->
    LJID = jid:tolower(JID),
    case (?DICT):find(LJID, StateData#state.users) of
      {ok, #user{role = Role}} -> Role;
      _ -> none
    end.

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

is_visitor(Jid, StateData) ->
    get_role(Jid, StateData) =:= visitor.

is_moderator(Jid, StateData) ->
    get_role(Jid, StateData) =:= moderator.

get_max_users(StateData) ->
    MaxUsers = (StateData#state.config)#config.max_users,
    ServiceMaxUsers = get_service_max_users(StateData),
    if MaxUsers =< ServiceMaxUsers -> MaxUsers;
       true -> ServiceMaxUsers
    end.

get_service_max_users(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
			   mod_muc, max_users,
                           fun(I) when is_integer(I), I>0 -> I end,
                           ?MAX_USERS_DEFAULT).

get_max_users_admin_threshold(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
			   mod_muc, max_users_admin_threshold,
                           fun(I) when is_integer(I), I>0 -> I end,
                           5).

get_user_activity(JID, StateData) ->
    case treap:lookup(jid:tolower(JID),
		      StateData#state.activity)
	of
      {ok, _P, A} -> A;
      error ->
	  MessageShaper =
	      shaper:new(gen_mod:get_module_opt(StateData#state.server_host,
						mod_muc, user_message_shaper,
                                                fun(A) when is_atom(A) -> A end,
						none)),
	  PresenceShaper =
	      shaper:new(gen_mod:get_module_opt(StateData#state.server_host,
						mod_muc, user_presence_shaper,
                                                fun(A) when is_atom(A) -> A end,
						none)),
	  #activity{message_shaper = MessageShaper,
		    presence_shaper = PresenceShaper}
    end.

store_user_activity(JID, UserActivity, StateData) ->
    MinMessageInterval =
	trunc(gen_mod:get_module_opt(StateData#state.server_host,
				     mod_muc, min_message_interval,
				     fun(I) when is_number(I), I>=0 -> I end,
				     0)
	      * 1000),
    MinPresenceInterval =
	trunc(gen_mod:get_module_opt(StateData#state.server_host,
				     mod_muc, min_presence_interval,
				     fun(I) when is_number(I), I>=0 -> I end,
				     0)
	      * 1000),
    Key = jid:tolower(JID),
    Now = p1_time_compat:system_time(micro_seconds),
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
				 shaper:update(UserActivity#activity.message_shaper,
					       100000),
			     {_, PresenceShaperInterval} =
				 shaper:update(UserActivity#activity.presence_shaper,
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
    StateData1.

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

prepare_room_queue(StateData) ->
    case queue:out(StateData#state.room_queue) of
      {{value, {message, From}}, _RoomQueue} ->
	  Activity = get_user_activity(From, StateData),
	  Packet = Activity#activity.message,
	  Size = element_size(Packet),
	  {RoomShaper, RoomShaperInterval} =
	      shaper:update(StateData#state.room_shaper, Size),
	  erlang:send_after(RoomShaperInterval, self(),
			    process_room_queue),
	  StateData#state{room_shaper = RoomShaper};
      {{value, {presence, From}}, _RoomQueue} ->
	  Activity = get_user_activity(From, StateData),
	  {_Nick, Packet} = Activity#activity.presence,
	  Size = element_size(Packet),
	  {RoomShaper, RoomShaperInterval} =
	      shaper:update(StateData#state.room_shaper, Size),
	  erlang:send_after(RoomShaperInterval, self(),
			    process_room_queue),
	  StateData#state{room_shaper = RoomShaper};
      {empty, _} -> StateData
    end.

update_online_user(JID, #user{nick = Nick, subscriptions = Nodes,
			      is_subscriber = IsSubscriber} = User, StateData) ->
    LJID = jid:tolower(JID),
    Nicks1 = case (?DICT):find(LJID, StateData#state.users) of
		 {ok, #user{nick = OldNick}} ->
		     case lists:delete(
			    LJID, ?DICT:fetch(OldNick, StateData#state.nicks)) of
			 [] ->
			     ?DICT:erase(OldNick, StateData#state.nicks);
			 LJIDs ->
			     ?DICT:store(OldNick, LJIDs, StateData#state.nicks)
		     end;
		 error ->
		     StateData#state.nicks
	     end,
    Nicks = (?DICT):update(Nick,
			   fun (LJIDs) -> [LJID|LJIDs -- [LJID]] end,
			   [LJID], Nicks1),
    Users = (?DICT):update(LJID,
			   fun(U) ->
				   U#user{nick = Nick,
					  subscriptions = Nodes,
					  is_subscriber = IsSubscriber}
			   end, User, StateData#state.users),
    NewStateData = StateData#state{users = Users, nicks = Nicks},
    case {?DICT:find(LJID, StateData#state.users),
	  ?DICT:find(LJID, NewStateData#state.users)} of
	{{ok, #user{nick = Old}}, {ok, #user{nick = New}}} when Old /= New ->
	    send_nick_changing(JID, Old, NewStateData, true, true);
	_ ->
	    ok
    end,
    NewStateData.

add_online_user(JID, Nick, Role, IsSubscriber, Nodes, StateData) ->
    tab_add_online_user(JID, StateData),
    User = #user{jid = JID, nick = Nick, role = Role,
		 is_subscriber = IsSubscriber, subscriptions = Nodes},
    StateData1 = update_online_user(JID, User, StateData),
    if IsSubscriber ->
	    store_room(StateData1);
       true ->
	    ok
    end,
    StateData1.

remove_online_user(JID, StateData, IsSubscriber) ->
    remove_online_user(JID, StateData, IsSubscriber, <<"">>).

remove_online_user(JID, StateData, _IsSubscriber = true, _Reason) ->
    LJID = jid:tolower(JID),
    Users = case (?DICT):find(LJID, StateData#state.users) of
		{ok, U} ->
		    (?DICT):store(LJID, U#user{last_presence = undefined},
				  StateData#state.users);
		error ->
		    StateData#state.users
	    end,
    StateData#state{users = Users};
remove_online_user(JID, StateData, _IsSubscriber, Reason) ->
    LJID = jid:tolower(JID),
    {ok, #user{nick = Nick}} = (?DICT):find(LJID,
					    StateData#state.users),
    add_to_log(leave, {Nick, Reason}, StateData),
    tab_remove_online_user(JID, StateData),
    Users = (?DICT):erase(LJID, StateData#state.users),
    Nicks = case (?DICT):find(Nick, StateData#state.nicks)
		of
	      {ok, [LJID]} ->
		  (?DICT):erase(Nick, StateData#state.nicks);
	      {ok, U} ->
		  (?DICT):store(Nick, U -- [LJID], StateData#state.nicks);
	      error -> StateData#state.nicks
	    end,
    StateData#state{users = Users, nicks = Nicks}.

filter_presence(#xmlel{name = <<"presence">>,
		       attrs = Attrs, children = Els}) ->
    FEls = lists:filter(fun (El) ->
				case El of
				  {xmlcdata, _} -> false;
				  #xmlel{attrs = Attrs1} ->
                                        XMLNS = fxml:get_attr_s(<<"xmlns">>,
                                                               Attrs1),
                                        NS_MUC = ?NS_MUC,
                                        Size = byte_size(NS_MUC),
                                        case XMLNS of
                                            <<NS_MUC:Size/binary, _/binary>> ->
                                                false;
                                            _ ->
                                                true
                                        end
				end
			end,
			Els),
    #xmlel{name = <<"presence">>, attrs = Attrs,
	   children = FEls}.

strip_status(#xmlel{name = <<"presence">>,
		    attrs = Attrs, children = Els}) ->
    FEls = lists:filter(fun (#xmlel{name = <<"status">>}) ->
				false;
			    (_) -> true
			end,
			Els),
    #xmlel{name = <<"presence">>, attrs = Attrs,
	   children = FEls}.

add_user_presence(JID, Presence, StateData) ->
    LJID = jid:tolower(JID),
    FPresence = filter_presence(Presence),
    Users = (?DICT):update(LJID,
			   fun (#user{} = User) ->
				   User#user{last_presence = FPresence}
			   end,
			   StateData#state.users),
    StateData#state{users = Users}.

add_user_presence_un(JID, Presence, StateData) ->
    LJID = jid:tolower(JID),
    FPresence = filter_presence(Presence),
    Users = (?DICT):update(LJID,
			   fun (#user{} = User) ->
				   User#user{last_presence = FPresence,
					     role = none}
			   end,
			   StateData#state.users),
    StateData#state{users = Users}.

%% Find and return a list of the full JIDs of the users of Nick.
%% Return jid record.
find_jids_by_nick(Nick, StateData) ->
    case (?DICT):find(Nick, StateData#state.nicks) of
      {ok, [User]} -> [jid:make(User)];
      {ok, Users} -> [jid:make(LJID) || LJID <- Users];
      error -> false
    end.

%% Find and return the full JID of the user of Nick with
%% highest-priority presence.  Return jid record.
find_jid_by_nick(Nick, StateData) ->
    case (?DICT):find(Nick, StateData#state.nicks) of
      {ok, [User]} -> jid:make(User);
      {ok, [FirstUser | Users]} ->
	  #user{last_presence = FirstPresence} =
	      (?DICT):fetch(FirstUser, StateData#state.users),
	  {LJID, _} = lists:foldl(fun (Compare,
				       {HighestUser, HighestPresence}) ->
					  #user{last_presence = P1} =
					      (?DICT):fetch(Compare,
							    StateData#state.users),
					  case higher_presence(P1,
							       HighestPresence)
					      of
					    true -> {Compare, P1};
					    false ->
						{HighestUser, HighestPresence}
					  end
				  end,
				  {FirstUser, FirstPresence}, Users),
	  jid:make(LJID);
      error -> false
    end.

higher_presence(Pres1, Pres2) when Pres1 /= undefined, Pres2 /= undefined ->
    Pri1 = get_priority_from_presence(Pres1),
    Pri2 = get_priority_from_presence(Pres2),
    Pri1 > Pri2;
higher_presence(Pres1, Pres2) ->
    Pres1 > Pres2.

get_priority_from_presence(PresencePacket) ->
    case fxml:get_subtag(PresencePacket, <<"priority">>) of
      false -> 0;
      SubEl ->
	  case catch
		 jlib:binary_to_integer(fxml:get_tag_cdata(SubEl))
	      of
	    P when is_integer(P) -> P;
	    _ -> 0
	  end
    end.

find_nick_by_jid(Jid, StateData) ->
    [{_, #user{nick = Nick}}] = lists:filter(fun ({_,
						   #user{jid = FJid}}) ->
						     FJid == Jid
					     end,
					     (?DICT):to_list(StateData#state.users)),
    Nick.

is_nick_change(JID, Nick, StateData) ->
    LJID = jid:tolower(JID),
    case Nick of
      <<"">> -> false;
      _ ->
	  {ok, #user{nick = OldNick}} = (?DICT):find(LJID,
						     StateData#state.users),
	  Nick /= OldNick
    end.

nick_collision(User, Nick, StateData) ->
    UserOfNick = find_jid_by_nick(Nick, StateData),
    (UserOfNick /= false andalso
      jid:remove_resource(jid:tolower(UserOfNick))
	/= jid:remove_resource(jid:tolower(User))).

add_new_user(From, Nick,
	     #xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
	     StateData) ->
    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
    UserRoomJID = jid:replace_resource(StateData#state.jid, Nick),
    MaxUsers = get_max_users(StateData),
    MaxAdminUsers = MaxUsers +
		      get_max_users_admin_threshold(StateData),
    NUsers = dict:fold(fun (_, _, Acc) -> Acc + 1 end, 0,
		       StateData#state.users),
    Affiliation = get_affiliation(From, StateData),
    ServiceAffiliation = get_service_affiliation(From,
						 StateData),
    NConferences = tab_count_user(From),
    MaxConferences =
	gen_mod:get_module_opt(StateData#state.server_host,
			       mod_muc, max_user_conferences,
                               fun(I) when is_integer(I), I>0 -> I end,
                               10),
    Collision = nick_collision(From, Nick, StateData),
    IsSubscribeRequest = Name /= <<"presence">>,
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
	  Txt = <<"Too many users in this conference">>,
	  Err = ?ERRT_RESOURCE_CONSTRAINT(Lang, Txt),
	  ErrPacket = jlib:make_error_reply(Packet, Err),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route(UserRoomJID, From, ErrPacket),
		  StateData;
	     true ->
		  {error, Err, StateData}
	  end;
      {false, _, _, _} when NConferences >= MaxConferences ->
	  Txt = <<"You have joined too many conferences">>,
	  Err = ?ERRT_RESOURCE_CONSTRAINT(Lang, Txt),
	  ErrPacket = jlib:make_error_reply(Packet, Err),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route(UserRoomJID, From, ErrPacket),
		  StateData;
	     true ->
		  {error, Err, StateData}
	  end;
      {false, _, _, _} ->
	  Err = ?ERR_SERVICE_UNAVAILABLE,
	  ErrPacket = jlib:make_error_reply(Packet, Err),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route(UserRoomJID, From, ErrPacket),
		  StateData;
	     true ->
		  {error, Err, StateData}
	  end;
      {_, _, _, none} ->
	  Err = case Affiliation of
		    outcast ->
			ErrText = <<"You have been banned from this room">>,
			?ERRT_FORBIDDEN(Lang, ErrText);
		    _ ->
			ErrText = <<"Membership is required to enter this room">>,
			?ERRT_REGISTRATION_REQUIRED(Lang, ErrText)
		end,
	  ErrPacket = jlib:make_error_reply(Packet, Err),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route(UserRoomJID, From, ErrPacket),
		  StateData;
	     true ->
		  {error, Err, StateData}
	  end;
      {_, true, _, _} ->
	  ErrText = <<"That nickname is already in use by another occupant">>,
	  Err = ?ERRT_CONFLICT(Lang, ErrText),
	  ErrPacket = jlib:make_error_reply(Packet, Err),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route(UserRoomJID, From, ErrPacket),
		  StateData;
	     true ->
		  {error, Err, StateData}
	  end;
      {_, _, false, _} ->
	  ErrText = <<"That nickname is registered by another person">>,
	  Err = ?ERRT_CONFLICT(Lang, ErrText),
	  ErrPacket = jlib:make_error_reply(Packet, Err),
	  if not IsSubscribeRequest ->
		  ejabberd_router:route(UserRoomJID, From, ErrPacket),
		  StateData;
	     true ->
		  {error, Err, StateData}
	  end;
      {_, _, _, Role} ->
	  case check_password(ServiceAffiliation, Affiliation,
			      Els, From, StateData)
	      of
	    true ->
		Nodes = get_subscription_nodes(Packet),
		NewStateData =
		      if not IsSubscribeRequest ->
			      NewState = add_user_presence(
					   From, Packet,
					   add_online_user(From, Nick, Role,
							   IsSubscribeRequest,
							   Nodes, StateData)),
			      send_existing_presences(From, NewState),
			      send_initial_presence(From, NewState, StateData),
			      Shift = count_stanza_shift(Nick, Els, NewState),
			      case send_history(From, Shift, NewState) of
				  true -> ok;
				  _ -> send_subject(From, StateData)
			      end,
			      NewState;
			 true ->
			      add_online_user(From, Nick, none,
					      IsSubscribeRequest,
					      Nodes, StateData)
		      end,
		  ResultState =
		      case NewStateData#state.just_created of
			  true ->
			      NewStateData#state{just_created = false};
			  false ->
			      Robots = (?DICT):erase(From, StateData#state.robots),
			      NewStateData#state{robots = Robots}
		      end,
		  if not IsSubscribeRequest -> ResultState;
		     true -> {result, subscription_nodes_to_events(Nodes), ResultState}
		  end;
	    nopass ->
		ErrText = <<"A password is required to enter this room">>,
		Err = ?ERRT_NOT_AUTHORIZED(Lang, ErrText),
		ErrPacket = jlib:make_error_reply(Packet, Err),
		if not IsSubscribeRequest ->
			ejabberd_router:route(UserRoomJID, From, ErrPacket),
			StateData;
		   true ->
			{error, Err, StateData}
		end;
	    captcha_required ->
		SID = fxml:get_attr_s(<<"id">>, Attrs),
		RoomJID = StateData#state.jid,
		To = jid:replace_resource(RoomJID, Nick),
		Limiter = {From#jid.luser, From#jid.lserver},
		case ejabberd_captcha:create_captcha(SID, RoomJID, To,
						     Lang, Limiter, From)
                   of
		  {ok, ID, CaptchaEls} ->
		      MsgPkt = #xmlel{name = <<"message">>,
				      attrs = [{<<"id">>, ID}],
				      children = CaptchaEls},
		      Robots = (?DICT):store(From, {Nick, Packet},
					     StateData#state.robots),
		      ejabberd_router:route(RoomJID, From, MsgPkt),
		      NewState = StateData#state{robots = Robots},
		      if not IsSubscribeRequest ->
			      NewState;
			 true ->
			      {ignore, NewState}
		      end;
		  {error, limit} ->
		      ErrText = <<"Too many CAPTCHA requests">>,
		      Err = ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText),
		      ErrPacket = jlib:make_error_reply(Packet, Err),
		      if not IsSubscribeRequest ->
			      ejabberd_router:route(UserRoomJID, From, ErrPacket),
			      StateData;
			 true ->
			      {error, Err, StateData}
		      end;
		  _ ->
		      ErrText = <<"Unable to generate a CAPTCHA">>,
		      Err = ?ERRT_INTERNAL_SERVER_ERROR(Lang, ErrText),
		      ErrPacket = jlib:make_error_reply(Packet, Err),
		      if not IsSubscribeRequest ->
			      ejabberd_router:route(UserRoomJID, From, ErrPacket),
			      StateData;
			 true ->
			      {error, Err, StateData}
		      end
		end;
	    _ ->
		ErrText = <<"Incorrect password">>,
		Err = ?ERRT_NOT_AUTHORIZED(Lang, ErrText),
		ErrPacket = jlib:make_error_reply(Packet, Err),
		if not IsSubscribeRequest ->
			ejabberd_router:route(UserRoomJID, From, ErrPacket),
			StateData;
		   true ->
			{error, Err, StateData}
		end
	  end
    end.

check_password(owner, _Affiliation, _Els, _From,
	       _StateData) ->
    %% Don't check pass if user is owner in MUC service (access_admin option)
    true;
check_password(_ServiceAffiliation, Affiliation, Els,
	       From, StateData) ->
    case (StateData#state.config)#config.password_protected
	of
      false -> check_captcha(Affiliation, From, StateData);
      true ->
	  Pass = extract_password(Els),
	  case Pass of
	    false -> nopass;
	    _ ->
		case (StateData#state.config)#config.password of
		  Pass -> true;
		  _ -> false
		end
	  end
    end.

check_captcha(Affiliation, From, StateData) ->
    case (StateData#state.config)#config.captcha_protected
	   andalso ejabberd_captcha:is_feature_available()
	of
      true when Affiliation == none ->
	  case (?DICT):find(From, StateData#state.robots) of
	    {ok, passed} -> true;
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

extract_password([]) -> false;
extract_password([#xmlel{attrs = Attrs} = El | Els]) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
      ?NS_MUC ->
	  case fxml:get_subtag(El, <<"password">>) of
	    false -> false;
	    SubEl -> fxml:get_tag_cdata(SubEl)
	  end;
      _ -> extract_password(Els)
    end;
extract_password([_ | Els]) -> extract_password(Els).

count_stanza_shift(Nick, Els, StateData) ->
    HL = lqueue_to_list(StateData#state.history),
    Since = extract_history(Els, <<"since">>),
    Shift0 = case Since of
	       false -> 0;
	       _ ->
		   Sin = calendar:datetime_to_gregorian_seconds(Since),
		   count_seconds_shift(Sin, HL)
	     end,
    Seconds = extract_history(Els, <<"seconds">>),
    Shift1 = case Seconds of
	       false -> 0;
	       _ ->
		   Sec = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
                         - Seconds,
		   count_seconds_shift(Sec, HL)
	     end,
    MaxStanzas = extract_history(Els, <<"maxstanzas">>),
    Shift2 = case MaxStanzas of
	       false -> 0;
	       _ -> count_maxstanzas_shift(MaxStanzas, HL)
	     end,
    MaxChars = extract_history(Els, <<"maxchars">>),
    Shift3 = case MaxChars of
	       false -> 0;
	       _ -> count_maxchars_shift(Nick, MaxChars, HL)
	     end,
    lists:max([Shift0, Shift1, Shift2, Shift3]).

count_seconds_shift(Seconds, HistoryList) ->
    lists:sum(lists:map(fun ({_Nick, _Packet, _HaveSubject,
			      TimeStamp, _Size}) ->
				T =
				    calendar:datetime_to_gregorian_seconds(TimeStamp),
				if T < Seconds -> 1;
				   true -> 0
				end
			end,
			HistoryList)).

count_maxstanzas_shift(MaxStanzas, HistoryList) ->
    S = length(HistoryList) - MaxStanzas,
    if S =< 0 -> 0;
       true -> S
    end.

count_maxchars_shift(Nick, MaxSize, HistoryList) ->
    NLen = byte_size(Nick) + 1,
    Sizes = lists:map(fun ({_Nick, _Packet, _HaveSubject,
			    _TimeStamp, Size}) ->
			      Size + NLen
		      end,
		      HistoryList),
    calc_shift(MaxSize, Sizes).

calc_shift(MaxSize, Sizes) ->
    Total = lists:sum(Sizes),
    calc_shift(MaxSize, Total, 0, Sizes).

calc_shift(_MaxSize, _Size, Shift, []) -> Shift;
calc_shift(MaxSize, Size, Shift, [S | TSizes]) ->
    if MaxSize >= Size -> Shift;
       true -> calc_shift(MaxSize, Size - S, Shift + 1, TSizes)
    end.

extract_history([], _Type) -> false;
extract_history([#xmlel{attrs = Attrs} = El | Els],
		Type) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
      ?NS_MUC ->
	  AttrVal = fxml:get_path_s(El,
				   [{elem, <<"history">>}, {attr, Type}]),
	  case Type of
	    <<"since">> ->
		case jlib:datetime_string_to_timestamp(AttrVal) of
		  undefined -> false;
		  TS -> calendar:now_to_universal_time(TS)
		end;
	    _ ->
		case catch jlib:binary_to_integer(AttrVal) of
		  IntVal when is_integer(IntVal) and (IntVal >= 0) ->
		      IntVal;
		  _ -> false
		end
	  end;
      _ -> extract_history(Els, Type)
    end;
extract_history([_ | Els], Type) ->
    extract_history(Els, Type).

is_room_overcrowded(StateData) ->
    MaxUsersPresence = gen_mod:get_module_opt(StateData#state.server_host,
	mod_muc, max_users_presence,
	fun(MUP) when is_integer(MUP) -> MUP end,
	?DEFAULT_MAX_USERS_PRESENCE),
    (?DICT):size(StateData#state.users) > MaxUsersPresence.

presence_broadcast_allowed(JID, StateData) ->
    Role = get_role(JID, StateData),
    lists:member(Role, (StateData#state.config)#config.presence_broadcast).

is_initial_presence(From, StateData) ->
    LJID = jid:tolower(From),
    case (?DICT):find(LJID, StateData#state.users) of
	{ok, #user{last_presence = Pres}} when Pres /= undefined ->
	    false;
	_ ->
	    true
    end.

send_initial_presence(NJID, StateData, OldStateData) ->
    send_new_presence1(NJID, <<"">>, true, StateData, OldStateData).

send_update_presence(JID, StateData, OldStateData) ->
    send_update_presence(JID, <<"">>, StateData, OldStateData).

send_update_presence(JID, Reason, StateData, OldStateData) ->
    case is_room_overcrowded(StateData) of
	true -> ok;
	false -> send_update_presence1(JID, Reason, StateData, OldStateData)
    end.

send_update_presence1(JID, Reason, StateData, OldStateData) ->
    LJID = jid:tolower(JID),
    LJIDs = case LJID of
	      {U, S, <<"">>} ->
		  (?DICT):fold(fun (J, _, Js) ->
				       case J of
					 {U, S, _} -> [J | Js];
					 _ -> Js
				       end
			       end,
			       [], StateData#state.users);
	      _ ->
		  case (?DICT):is_key(LJID, StateData#state.users) of
		    true -> [LJID];
		    _ -> []
		  end
	    end,
    lists:foreach(fun (J) ->
			  send_new_presence1(J, Reason, false, StateData,
					     OldStateData)
		  end,
		  LJIDs).

send_new_presence(NJID, StateData, OldStateData) ->
    send_new_presence(NJID, <<"">>, false, StateData, OldStateData).

send_new_presence(NJID, Reason, StateData, OldStateData) ->
    send_new_presence(NJID, Reason, false, StateData, OldStateData).

send_new_presence(NJID, Reason, IsInitialPresence, StateData, OldStateData) ->
    case is_room_overcrowded(StateData) of
	true -> ok;
	false -> send_new_presence1(NJID, Reason, IsInitialPresence, StateData,
				    OldStateData)
    end.

is_ra_changed(_, _IsInitialPresence = true, _, _) ->
    false;
is_ra_changed(LJID, _IsInitialPresence = false, NewStateData, OldStateData) ->
    JID = case LJID of
	      #jid{} -> LJID;
	      _ -> jid:make(LJID)
	  end,
    NewRole = get_role(LJID, NewStateData),
    NewAff = get_affiliation(JID, NewStateData),
    OldRole = get_role(LJID, OldStateData),
    OldAff = get_affiliation(JID, OldStateData),
    if (NewRole == none) and (NewAff == OldAff) ->
	    %% A user is leaving the room;
	    false;
       true ->
	    (NewRole /= OldRole) or (NewAff /= OldAff)
    end.

send_new_presence1(NJID, Reason, IsInitialPresence, StateData, OldStateData) ->
    LNJID = jid:tolower(NJID),
    #user{nick = Nick} = (?DICT):fetch(LNJID, StateData#state.users),
    LJID = find_jid_by_nick(Nick, StateData),
    {ok,
     #user{jid = RealJID, role = Role0,
	   last_presence = Presence0} = UserInfo} =
	(?DICT):find(jid:tolower(LJID),
		     StateData#state.users),
    {Role1, Presence1} =
        case presence_broadcast_allowed(NJID, StateData) of
            true -> {Role0, Presence0};
            false ->
                {none,
                 #xmlel{name = <<"presence">>,
                        attrs = [{<<"type">>, <<"unavailable">>}],
                        children = []}
                }
        end,
    Affiliation = get_affiliation(LJID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    UserList =
        case not (presence_broadcast_allowed(NJID, StateData) orelse
             presence_broadcast_allowed(NJID, OldStateData)) of
            true ->
                [{LNJID, UserInfo}];
            false ->
                (?DICT):to_list(StateData#state.users)
        end,
    lists:foreach(
      fun({LUJID, Info}) ->
	      {Role, Presence} = if LNJID == LUJID -> {Role0, Presence0};
				    true -> {Role1, Presence1}
				 end,
	      SRole = role_to_list(Role),
	      ItemAttrs = case Info#user.role == moderator orelse
			      (StateData#state.config)#config.anonymous
			      == false
			  of
			      true ->
				  [{<<"jid">>,
				    jid:to_string(RealJID)},
				   {<<"affiliation">>, SAffiliation},
				   {<<"role">>, SRole}];
			      _ ->
				  [{<<"affiliation">>, SAffiliation},
				   {<<"role">>, SRole}]
			  end,
	      ItemEls = case Reason of
			    <<"">> -> [];
			    _ ->
				[#xmlel{name = <<"reason">>,
					attrs = [],
					children =
					    [{xmlcdata, Reason}]}]
			end,
	      StatusEls = status_els(IsInitialPresence, NJID, Info,
				     StateData),
	      Pres = if Presence == undefined -> #xmlel{name = <<"presence">>};
			true -> Presence
		     end,
	      Packet = fxml:append_subtags(Pres,
					   [#xmlel{name = <<"x">>,
						   attrs =
						       [{<<"xmlns">>,
							 ?NS_MUC_USER}],
						   children =
						       [#xmlel{name =
								   <<"item">>,
							       attrs
							       =
								   ItemAttrs,
							       children
							       =
								   ItemEls}
							| StatusEls]}]),
	      Node1 = case is_ra_changed(NJID, IsInitialPresence, StateData, OldStateData) of
			  true -> ?NS_MUCSUB_NODES_AFFILIATIONS;
			  false -> ?NS_MUCSUB_NODES_PRESENCE
		      end,
	      send_wrapped(jid:replace_resource(StateData#state.jid, Nick),
			   Info#user.jid, Packet, Node1, StateData),
	      Type = fxml:get_tag_attr_s(<<"type">>, Packet),
	      IsSubscriber = Info#user.is_subscriber,
	      IsOccupant = Info#user.last_presence /= undefined,
	      if (IsSubscriber and not IsOccupant) and
		 (IsInitialPresence or (Type == <<"unavailable">>)) ->
		      Node2 = ?NS_MUCSUB_NODES_PARTICIPANTS,
		      send_wrapped(jid:replace_resource(StateData#state.jid, Nick),
				   Info#user.jid, Packet, Node2, StateData);
		 true ->
		      ok
	      end
      end,
      UserList).

send_existing_presences(ToJID, StateData) ->
    case is_room_overcrowded(StateData) of
	true -> ok;
	false -> send_existing_presences1(ToJID, StateData)
    end.

send_existing_presences1(ToJID, StateData) ->
    LToJID = jid:tolower(ToJID),
    {ok, #user{jid = RealToJID, role = Role}} =
	(?DICT):find(LToJID, StateData#state.users),
    lists:foreach(
      fun({FromNick, _Users}) ->
	      LJID = find_jid_by_nick(FromNick, StateData),
	      #user{jid = FromJID, role = FromRole,
		    last_presence = Presence} =
		  (?DICT):fetch(jid:tolower(LJID),
				StateData#state.users),
	      PresenceBroadcast =
		  lists:member(
		    FromRole, (StateData#state.config)#config.presence_broadcast),
	      case {RealToJID, PresenceBroadcast} of
		  {FromJID, _} -> ok;
		  {_, false} -> ok;
		  _ ->
		      FromAffiliation = get_affiliation(LJID, StateData),
		      ItemAttrs = case Role == moderator orelse
				      (StateData#state.config)#config.anonymous
				      == false
				  of
				      true ->
					  [{<<"jid">>,
					    jid:to_string(FromJID)},
					   {<<"affiliation">>,
					    affiliation_to_list(FromAffiliation)},
					   {<<"role">>,
					    role_to_list(FromRole)}];
				      _ ->
					  [{<<"affiliation">>,
					    affiliation_to_list(FromAffiliation)},
					   {<<"role">>,
					    role_to_list(FromRole)}]
				  end,
		      Packet = fxml:append_subtags(
				 Presence,
				 [#xmlel{name =
					     <<"x">>,
					 attrs =
					     [{<<"xmlns">>,
					       ?NS_MUC_USER}],
					 children =
					     [#xmlel{name
						     =
							 <<"item">>,
						     attrs
						     =
							 ItemAttrs,
						     children
						     =
							 []}]}]),
		      send_wrapped(jid:replace_resource(StateData#state.jid, FromNick),
				   RealToJID, Packet, ?NS_MUCSUB_NODES_PRESENCE, StateData)
	      end
      end,
      (?DICT):to_list(StateData#state.nicks)).

set_nick(JID, Nick, State) ->
    LJID = jid:tolower(JID),
    {ok, #user{nick = OldNick}} = (?DICT):find(LJID, State#state.users),
    Users = (?DICT):update(LJID,
			   fun (#user{} = User) -> User#user{nick = Nick} end,
			   State#state.users),
    OldNickUsers = (?DICT):fetch(OldNick, State#state.nicks),
    NewNickUsers = case (?DICT):find(Nick, State#state.nicks) of
		       {ok, U} -> U;
		       error -> []
		   end,
    Nicks = case OldNickUsers of
		[LJID] ->
		    (?DICT):store(Nick, [LJID | NewNickUsers -- [LJID]],
				  (?DICT):erase(OldNick, State#state.nicks));
		[_ | _] ->
		    (?DICT):store(Nick, [LJID | NewNickUsers -- [LJID]],
				  (?DICT):store(OldNick, OldNickUsers -- [LJID],
						State#state.nicks))
	    end,
    State#state{users = Users, nicks = Nicks}.

change_nick(JID, Nick, StateData) ->
    LJID = jid:tolower(JID),
    {ok, #user{nick = OldNick}} = (?DICT):find(LJID, StateData#state.users),
    OldNickUsers = (?DICT):fetch(OldNick, StateData#state.nicks),
    NewNickUsers = case (?DICT):find(Nick, StateData#state.nicks) of
		       {ok, U} -> U;
		       error -> []
		   end,
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

send_nick_changing(JID, OldNick, StateData,
		   SendOldUnavailable, SendNewAvailable) ->
    {ok,
     #user{jid = RealJID, nick = Nick, role = Role,
	   last_presence = Presence}} =
	(?DICT):find(jid:tolower(JID),
		     StateData#state.users),
    Affiliation = get_affiliation(JID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    SRole = role_to_list(Role),
    lists:foreach(fun ({_LJID, Info}) when Presence /= undefined ->
			  ItemAttrs1 = case Info#user.role == moderator orelse
					      (StateData#state.config)#config.anonymous
						== false
					   of
					 true ->
					     [{<<"jid">>,
					       jid:to_string(RealJID)},
					      {<<"affiliation">>, SAffiliation},
					      {<<"role">>, SRole},
					      {<<"nick">>, Nick}];
					 _ ->
					     [{<<"affiliation">>, SAffiliation},
					      {<<"role">>, SRole},
					      {<<"nick">>, Nick}]
				       end,
			  ItemAttrs2 = case Info#user.role == moderator orelse
					      (StateData#state.config)#config.anonymous
						== false
					   of
					 true ->
					     [{<<"jid">>,
					       jid:to_string(RealJID)},
					      {<<"affiliation">>, SAffiliation},
					      {<<"role">>, SRole}];
					 _ ->
					     [{<<"affiliation">>, SAffiliation},
					      {<<"role">>, SRole}]
				       end,
			  Status110 = case JID == Info#user.jid of
					true ->
					    [#xmlel{name = <<"status">>,
						    attrs = [{<<"code">>, <<"110">>}]
								       }];
					false ->
					    []
				    end,
			  Packet1 = #xmlel{name = <<"presence">>,
					   attrs =
					       [{<<"type">>,
						 <<"unavailable">>}],
					   children =
					       [#xmlel{name = <<"x">>,
						       attrs =
							   [{<<"xmlns">>,
							     ?NS_MUC_USER}],
						       children =
							   [#xmlel{name =
								       <<"item">>,
								   attrs =
								       ItemAttrs1,
								   children =
								       []},
							    #xmlel{name =
								       <<"status">>,
								   attrs =
								       [{<<"code">>,
									 <<"303">>}],
								   children =
								       []}|Status110]}]},
			  Packet2 = fxml:append_subtags(Presence,
						       [#xmlel{name = <<"x">>,
							       attrs =
								   [{<<"xmlns">>,
								     ?NS_MUC_USER}],
							       children =
								   [#xmlel{name
									       =
									       <<"item">>,
									   attrs
									       =
									       ItemAttrs2,
									   children
									       =
									       []}|Status110]}]),
			  if SendOldUnavailable ->
				  send_wrapped(jid:replace_resource(StateData#state.jid,
								    OldNick),
					       Info#user.jid, Packet1,
					       ?NS_MUCSUB_NODES_PRESENCE,
					       StateData);
			     true -> ok
			  end,
			  if SendNewAvailable ->
				  send_wrapped(jid:replace_resource(StateData#state.jid,
								    Nick),
					       Info#user.jid, Packet2,
					       ?NS_MUCSUB_NODES_PRESENCE,
					       StateData);
			     true -> ok
			  end;
		      (_) ->
			  ok
		  end,
		  (?DICT):to_list(StateData#state.users)).

maybe_send_affiliation(JID, Affiliation, StateData) ->
    LJID = jid:tolower(JID),
    IsOccupant = case LJID of
		   {LUser, LServer, <<"">>} ->
		       not (?DICT):is_empty(
			     (?DICT):filter(fun({U, S, _}, _) ->
						    U == LUser andalso
						      S == LServer
					    end, StateData#state.users));
		   {_LUser, _LServer, _LResource} ->
		       (?DICT):is_key(LJID, StateData#state.users)
		 end,
    case IsOccupant of
      true ->
	  ok; % The new affiliation is published via presence.
      false ->
	  send_affiliation(LJID, Affiliation, StateData)
    end.

send_affiliation(LJID, Affiliation, StateData) ->
    ItemAttrs = [{<<"jid">>, jid:to_string(LJID)},
		 {<<"affiliation">>, affiliation_to_list(Affiliation)},
		 {<<"role">>, <<"none">>}],
    Message = #xmlel{name = <<"message">>,
		     attrs = [{<<"id">>, randoms:get_string()}],
		     children =
			 [#xmlel{name = <<"x">>,
				 attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
				 children =
				     [#xmlel{name = <<"item">>,
					     attrs = ItemAttrs}]}]},
    Recipients = case (StateData#state.config)#config.anonymous of
		   true ->
		       (?DICT):filter(fun(_, #user{role = moderator}) ->
					      true;
					 (_, _) ->
					      false
				      end, StateData#state.users);
		   false ->
		       StateData#state.users
		 end,
    send_multiple(StateData#state.jid,
		  StateData#state.server_host,
		  Recipients, Message).

status_els(IsInitialPresence, JID, #user{jid = JID}, StateData) ->
    Status = case IsInitialPresence of
	       true ->
		   S1 = case StateData#state.just_created of
			  true ->
			      [#xmlel{name = <<"status">>,
				      attrs = [{<<"code">>, <<"201">>}],
				      children = []}];
			  false -> []
			end,
		   S2 = case (StateData#state.config)#config.anonymous of
			  true -> S1;
			  false ->
			      [#xmlel{name = <<"status">>,
				      attrs = [{<<"code">>, <<"100">>}],
				      children = []} | S1]
			end,
		   S3 = case (StateData#state.config)#config.logging of
			  true ->
			      [#xmlel{name = <<"status">>,
				      attrs = [{<<"code">>, <<"170">>}],
				      children = []} | S2];
			  false -> S2
			end,
		   S3;
	       false -> []
	     end,
    [#xmlel{name = <<"status">>,
	    attrs =
		[{<<"code">>,
		  <<"110">>}],
	    children = []} | Status];
status_els(_IsInitialPresence, _JID, _Info, _StateData) -> [].

lqueue_new(Max) ->
    #lqueue{queue = queue:new(), len = 0, max = Max}.

%% If the message queue limit is set to 0, do not store messages.
lqueue_in(_Item, LQ = #lqueue{max = 0}) -> LQ;
%% Otherwise, rotate messages in the queue store.
lqueue_in(Item,
	  #lqueue{queue = Q1, len = Len, max = Max}) ->
    Q2 = queue:in(Item, Q1),
    if Len >= Max ->
	   Q3 = lqueue_cut(Q2, Len - Max + 1),
	   #lqueue{queue = Q3, len = Max, max = Max};
       true -> #lqueue{queue = Q2, len = Len + 1, max = Max}
    end.

lqueue_cut(Q, 0) -> Q;
lqueue_cut(Q, N) ->
    {_, Q1} = queue:out(Q), lqueue_cut(Q1, N - 1).

lqueue_to_list(#lqueue{queue = Q1}) ->
    queue:to_list(Q1).


add_message_to_history(FromNick, FromJID, Packet, StateData) ->
    HaveSubject = case fxml:get_subtag(Packet, <<"subject">>)
		      of
		    false -> false;
		    _ -> true
		  end,
    TimeStamp = p1_time_compat:timestamp(),
    AddrPacket = case (StateData#state.config)#config.anonymous of
		   true -> Packet;
		   false ->
		       Address = #xmlel{name = <<"address">>,
					attrs = [{<<"type">>, <<"ofrom">>},
						 {<<"jid">>,
						  jid:to_string(FromJID)}],
					children = []},
		       Addresses = #xmlel{name = <<"addresses">>,
					  attrs = [{<<"xmlns">>, ?NS_ADDRESS}],
					  children = [Address]},
		       fxml:append_subtags(Packet, [Addresses])
		 end,
    TSPacket = jlib:add_delay_info(AddrPacket, StateData#state.jid, TimeStamp),
    SPacket =
	jlib:replace_from_to(jid:replace_resource(StateData#state.jid,
						       FromNick),
			     StateData#state.jid, TSPacket),
    Size = element_size(SPacket),
    Q1 = lqueue_in({FromNick, TSPacket, HaveSubject,
		    calendar:now_to_universal_time(TimeStamp), Size},
		   StateData#state.history),
    add_to_log(text, {FromNick, Packet}, StateData),
    StateData#state{history = Q1}.

send_history(JID, Shift, StateData) ->
    lists:foldl(fun ({Nick, Packet, HaveSubject, _TimeStamp,
		      _Size},
		     B) ->
			ejabberd_router:route(jid:replace_resource(StateData#state.jid,
							       Nick),
				     JID, Packet),
			B or HaveSubject
		end,
		false,
		lists:nthtail(Shift,
			      lqueue_to_list(StateData#state.history))).

send_subject(_JID, #state{subject_author = <<"">>}) -> ok;
send_subject(JID, #state{subject_author = Nick} = StateData) ->
    Subject = StateData#state.subject,
    Packet = #xmlel{name = <<"message">>,
		    attrs = [{<<"type">>, <<"groupchat">>}],
		    children =
			[#xmlel{name = <<"subject">>, attrs = [],
				children = [{xmlcdata, Subject}]}]},
    ejabberd_router:route(jid:replace_resource(StateData#state.jid, Nick), JID,
			  Packet).

check_subject(Packet) ->
    case fxml:get_subtag(Packet, <<"subject">>) of
      false -> false;
      SubjEl -> fxml:get_tag_cdata(SubjEl)
    end.

can_change_subject(Role, StateData) ->
    case (StateData#state.config)#config.allow_change_subj
	of
      true -> Role == moderator orelse Role == participant;
      _ -> Role == moderator
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Admin stuff

process_iq_admin(From, set, Lang, SubEl, StateData) ->
    #xmlel{children = Items} = SubEl,
    process_admin_items_set(From, Items, Lang, StateData);
process_iq_admin(From, get, Lang, SubEl, StateData) ->
    case fxml:get_subtag(SubEl, <<"item">>) of
      false ->
	  Txt = <<"No 'item' element found">>,
	  {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
      Item ->
	  FAffiliation = get_affiliation(From, StateData),
	  FRole = get_role(From, StateData),
	  case fxml:get_tag_attr(<<"role">>, Item) of
	    false ->
		case fxml:get_tag_attr(<<"affiliation">>, Item) of
		  false ->
		      Txt = <<"No 'affiliation' attribute found">>,
		      {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
		  {value, StrAffiliation} ->
		      case catch list_to_affiliation(StrAffiliation) of
			{'EXIT', _} -> {error, ?ERR_BAD_REQUEST};
			SAffiliation ->
			    if (FAffiliation == owner) or
				 (FAffiliation == admin) or
				 ((FAffiliation == member) and not
				  (StateData#state.config)#config.anonymous) ->
				   Items = items_with_affiliation(SAffiliation,
								  StateData),
				   {result, Items, StateData};
			       true ->
				   ErrText =
				       <<"Administrator privileges required">>,
				   {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
			    end
		      end
		end;
	    {value, StrRole} ->
		case catch list_to_role(StrRole) of
		  {'EXIT', _} ->
		      Txt = <<"Incorrect value of 'role' attribute">>,
		      {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
		  SRole ->
		      if FRole == moderator ->
			     Items = items_with_role(SRole, StateData),
			     {result, Items, StateData};
			 true ->
			     ErrText = <<"Moderator privileges required">>,
			     {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
		      end
		end
	  end
    end.

items_with_role(SRole, StateData) ->
    lists:map(fun ({_, U}) -> user_to_item(U, StateData)
	      end,
	      search_role(SRole, StateData)).

items_with_affiliation(SAffiliation, StateData) ->
    lists:map(fun ({JID, {Affiliation, Reason}}) ->
		      #xmlel{name = <<"item">>,
			     attrs =
				 [{<<"affiliation">>,
				   affiliation_to_list(Affiliation)},
				  {<<"jid">>, jid:to_string(JID)}],
			     children =
				 [#xmlel{name = <<"reason">>, attrs = [],
					 children = [{xmlcdata, Reason}]}]};
		  ({JID, Affiliation}) ->
		      #xmlel{name = <<"item">>,
			     attrs =
				 [{<<"affiliation">>,
				   affiliation_to_list(Affiliation)},
				  {<<"jid">>, jid:to_string(JID)}],
			     children = []}
	      end,
	      search_affiliation(SAffiliation, StateData)).

user_to_item(#user{role = Role, nick = Nick, jid = JID},
	     StateData) ->
    Affiliation = get_affiliation(JID, StateData),
    #xmlel{name = <<"item">>,
	   attrs =
	       [{<<"role">>, role_to_list(Role)},
		{<<"affiliation">>, affiliation_to_list(Affiliation)},
		{<<"nick">>, Nick},
		{<<"jid">>, jid:to_string(JID)}],
	   children = []}.

search_role(Role, StateData) ->
    lists:filter(fun ({_, #user{role = R}}) -> Role == R
		 end,
		 (?DICT):to_list(StateData#state.users)).

search_affiliation(Affiliation, StateData) ->
    lists:filter(fun ({_, A}) ->
			 case A of
			   {A1, _Reason} -> Affiliation == A1;
			   _ -> Affiliation == A
			 end
		 end,
		 (?DICT):to_list(StateData#state.affiliations)).

process_admin_items_set(UJID, Items, Lang, StateData) ->
    UAffiliation = get_affiliation(UJID, StateData),
    URole = get_role(UJID, StateData),
    case find_changed_items(UJID, UAffiliation, URole,
			    Items, Lang, StateData, [])
	of
      {result, Res} ->
	  ?INFO_MSG("Processing MUC admin query from ~s in "
		    "room ~s:~n ~p",
		    [jid:to_string(UJID),
		     jid:to_string(StateData#state.jid), Res]),
	  NSD = lists:foldl(process_item_change(UJID),
			    StateData, lists:flatten(Res)),
	  store_room(NSD),
	  {result, [], NSD};
      Err -> Err
    end.

process_item_change(UJID) ->
    fun(E, SD) ->
        process_item_change(E, SD, UJID)
    end.

process_item_change(E, SD, UJID) ->
    case catch case E of
        {JID, affiliation, owner, _} when JID#jid.luser == <<"">> ->
            %% If the provided JID does not have username,
            %% forget the affiliation completely
            SD;
        {JID, role, none, Reason} ->
            catch
                send_kickban_presence(UJID, JID,
                    Reason,
                    <<"307">>,
                    SD),
            set_role(JID, none, SD);
        {JID, affiliation, none, Reason} ->
            case (SD#state.config)#config.members_only of
                true ->
                    catch
                        send_kickban_presence(UJID, JID,
                            Reason,
                            <<"321">>,
                            none,
                            SD),
                    maybe_send_affiliation(JID, none, SD),
                    SD1 = set_affiliation(JID, none, SD),
                    set_role(JID, none, SD1);
                _ ->
                    SD1 = set_affiliation(JID, none, SD),
                    send_update_presence(JID, SD1, SD),
                    maybe_send_affiliation(JID, none, SD1),
                    SD1
            end;
        {JID, affiliation, outcast, Reason} ->
            catch
                send_kickban_presence(UJID, JID,
                    Reason,
                    <<"301">>,
                    outcast,
                    SD),
            maybe_send_affiliation(JID, outcast, SD),
            set_affiliation(JID,
                outcast,
                set_role(JID, none, SD),
                Reason);
        {JID, affiliation, A, Reason}
            when (A == admin) or (A == owner) ->
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
            catch
                send_new_presence(JID, Reason, SD1, SD),
            SD1;
        {JID, affiliation, A, _Reason} ->
            SD1 = set_affiliation(JID, A, SD),
            send_update_presence(JID, SD1, SD),
            maybe_send_affiliation(JID, A, SD1),
            SD1
    end
    of
        {'EXIT', ErrReason} ->
            ?ERROR_MSG("MUC ITEMS SET ERR: ~p~n", [ErrReason]),
            SD;
        NSD -> NSD
    end.

find_changed_items(_UJID, _UAffiliation, _URole, [],
		   _Lang, _StateData, Res) ->
    {result, Res};
find_changed_items(UJID, UAffiliation, URole,
		   [{xmlcdata, _} | Items], Lang, StateData, Res) ->
    find_changed_items(UJID, UAffiliation, URole, Items,
		       Lang, StateData, Res);
find_changed_items(UJID, UAffiliation, URole,
		   [#xmlel{name = <<"item">>, attrs = Attrs} = Item
		    | Items],
		   Lang, StateData, Res) ->
    TJID = case fxml:get_attr(<<"jid">>, Attrs) of
	     {value, S} ->
		 case jid:from_string(S) of
		   error ->
		       ErrText = iolist_to_binary(
                                   io_lib:format(translate:translate(
                                                   Lang,
                                                   <<"Jabber ID ~s is invalid">>),
                                                 [S])),
		       {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
		   J -> {value, [J]}
		 end;
	     _ ->
		 case fxml:get_attr(<<"nick">>, Attrs) of
		   {value, N} ->
		       case find_jids_by_nick(N, StateData) of
			 false ->
			     ErrText = iolist_to_binary(
                                         io_lib:format(
                                           translate:translate(
                                             Lang,
                                             <<"Nickname ~s does not exist in the room">>),
                                           [N])),
			     {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
			 J -> {value, J}
		       end;
		   _ ->
		       Txt1 = <<"No 'nick' attribute found">>,
		       {error, ?ERRT_BAD_REQUEST(Lang, Txt1)}
		 end
	   end,
    case TJID of
      {value, [JID | _] = JIDs} ->
	  TAffiliation = get_affiliation(JID, StateData),
	  TRole = get_role(JID, StateData),
	  case fxml:get_attr(<<"role">>, Attrs) of
	    false ->
		case fxml:get_attr(<<"affiliation">>, Attrs) of
		  false ->
		      Txt2 = <<"No 'affiliation' attribute found">>,
		      {error, ?ERRT_BAD_REQUEST(Lang, Txt2)};
		  {value, StrAffiliation} ->
		      case catch list_to_affiliation(StrAffiliation) of
			{'EXIT', _} ->
			    ErrText1 = iolist_to_binary(
                                         io_lib:format(
                                           translate:translate(
                                             Lang,
                                             <<"Invalid affiliation: ~s">>),
                                           [StrAffiliation])),
			    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText1)};
			SAffiliation ->
			    ServiceAf = get_service_affiliation(JID, StateData),
			    CanChangeRA = case can_change_ra(UAffiliation,
							     URole,
							     TAffiliation,
							     TRole, affiliation,
							     SAffiliation,
							     ServiceAf)
					      of
					    nothing -> nothing;
					    true -> true;
					    check_owner ->
						case search_affiliation(owner,
									StateData)
						    of
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
				  Reason = fxml:get_path_s(Item,
							  [{elem, <<"reason">>},
							   cdata]),
				  MoreRes = [{jid:remove_resource(Jidx),
					      affiliation, SAffiliation, Reason}
					     || Jidx <- JIDs],
				  find_changed_items(UJID, UAffiliation, URole,
						     Items, Lang, StateData,
						     [MoreRes | Res]);
			      false ->
				 Txt3 = <<"Changing role/affiliation is not allowed">>,
				 {error, ?ERRT_NOT_ALLOWED(Lang, Txt3)}
			    end
		      end
		end;
	    {value, StrRole} ->
		case catch list_to_role(StrRole) of
		  {'EXIT', _} ->
		      ErrText1 = iolist_to_binary(
                                   io_lib:format(translate:translate(
                                                   Lang,
                                                   <<"Invalid role: ~s">>),
                                                 [StrRole])),
		      {error, ?ERRT_BAD_REQUEST(Lang, ErrText1)};
		  SRole ->
		      ServiceAf = get_service_affiliation(JID, StateData),
		      CanChangeRA = case can_change_ra(UAffiliation, URole,
						       TAffiliation, TRole,
						       role, SRole, ServiceAf)
					of
				      nothing -> nothing;
				      true -> true;
				      check_owner ->
					  case search_affiliation(owner,
								  StateData)
					      of
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
			    find_changed_items(UJID, UAffiliation, URole, Items,
					       Lang, StateData, Res);
			true ->
			    Reason = fxml:get_path_s(Item,
						    [{elem, <<"reason">>},
						     cdata]),
			    MoreRes = [{Jidx, role, SRole, Reason}
				       || Jidx <- JIDs],
			    find_changed_items(UJID, UAffiliation, URole, Items,
					       Lang, StateData,
					       [MoreRes | Res]);
			_ ->
			   Txt4 = <<"Changing role/affiliation is not allowed">>,
			   {error, ?ERRT_NOT_ALLOWED(Lang, Txt4)}
		      end
		end
	  end;
      Err -> Err
    end;
find_changed_items(_UJID, _UAffiliation, _URole, _Items,
		   _Lang, _StateData, _Res) ->
    {error, ?ERR_BAD_REQUEST}.

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
can_change_ra(_FAffiliation, moderator, _TAffiliation,
	      visitor, role, participant, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole, _TAffiliation,
	      visitor, role, moderator, _ServiceAf)
    when (FAffiliation == owner) or
	   (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator, _TAffiliation,
	      participant, role, none, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, moderator, _TAffiliation,
	      participant, role, visitor, _ServiceAf) ->
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
can_change_ra(_FAffiliation, _FRole, _TAffiliation,
	      _TRole, role, _Value, _ServiceAf) ->
    false.

send_kickban_presence(UJID, JID, Reason, Code, StateData) ->
    NewAffiliation = get_affiliation(JID, StateData),
    send_kickban_presence(UJID, JID, Reason, Code, NewAffiliation,
			  StateData).

send_kickban_presence(UJID, JID, Reason, Code, NewAffiliation,
		      StateData) ->
    LJID = jid:tolower(JID),
    LJIDs = case LJID of
	      {U, S, <<"">>} ->
		  (?DICT):fold(fun (J, _, Js) ->
				       case J of
					 {U, S, _} -> [J | Js];
					 _ -> Js
				       end
			       end,
			       [], StateData#state.users);
	      _ ->
		  case (?DICT):is_key(LJID, StateData#state.users) of
		    true -> [LJID];
		    _ -> []
		  end
	    end,
    lists:foreach(fun (J) ->
			  {ok, #user{nick = Nick}} = (?DICT):find(J,
								  StateData#state.users),
			  add_to_log(kickban, {Nick, Reason, Code}, StateData),
			  tab_remove_online_user(J, StateData),
			  send_kickban_presence1(UJID, J, Reason, Code,
						 NewAffiliation, StateData)
		  end,
		  LJIDs).

send_kickban_presence1(MJID, UJID, Reason, Code, Affiliation,
		       StateData) ->
    {ok, #user{jid = RealJID, nick = Nick}} =
	(?DICT):find(jid:tolower(UJID),
		     StateData#state.users),
    SAffiliation = affiliation_to_list(Affiliation),
    BannedJIDString = jid:to_string(RealJID),
    ActorNick = get_actor_nick(MJID, StateData),
    lists:foreach(fun ({_LJID, Info}) ->
			  JidAttrList = case Info#user.role == moderator orelse
					       (StateData#state.config)#config.anonymous
						 == false
					    of
					  true ->
					      [{<<"jid">>, BannedJIDString}];
					  false -> []
					end,
			  ItemAttrs = [{<<"affiliation">>, SAffiliation},
				       {<<"role">>, <<"none">>}]
					++ JidAttrList,
			  ItemEls = case Reason of
				      <<"">> -> [];
				      _ ->
					  [#xmlel{name = <<"reason">>,
						  attrs = [],
						  children =
						      [{xmlcdata, Reason}]}]
				    end,
			  ItemElsActor = case MJID of
					   <<"">> -> [];
					   _ -> [#xmlel{name = <<"actor">>,
						attrs =
						    [{<<"nick">>, ActorNick}]}]
				    end,
			  Packet = #xmlel{name = <<"presence">>,
					  attrs =
					      [{<<"type">>, <<"unavailable">>}],
					  children =
					      [#xmlel{name = <<"x">>,
						      attrs =
							  [{<<"xmlns">>,
							    ?NS_MUC_USER}],
						      children =
							  [#xmlel{name =
								      <<"item">>,
								  attrs =
								      ItemAttrs,
								  children =
								       ItemElsActor ++  ItemEls},
							   #xmlel{name =
								      <<"status">>,
								  attrs =
								      [{<<"code">>,
									Code}],
								  children =
								      []}]}]},
			  RoomJIDNick = jid:replace_resource(
					  StateData#state.jid, Nick),
			  send_wrapped(RoomJIDNick, Info#user.jid, Packet,
				       ?NS_MUCSUB_NODES_AFFILIATIONS, StateData),
			  IsSubscriber = Info#user.is_subscriber,
			  IsOccupant = Info#user.last_presence /= undefined,
			  if (IsSubscriber and not IsOccupant) ->
				  send_wrapped(RoomJIDNick, Info#user.jid, Packet,
					       ?NS_MUCSUB_NODES_PARTICIPANTS, StateData);
			     true ->
				  ok
			  end
		  end,
		  (?DICT):to_list(StateData#state.users)).

get_actor_nick(<<"">>, _StateData) ->
    <<"">>;
get_actor_nick(MJID, StateData) ->
    case (?DICT):find(jid:tolower(MJID), StateData#state.users) of
	{ok, #user{nick = ActorNick}} -> ActorNick;
	_ -> <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff

process_iq_owner(From, set, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
      owner ->
	  #xmlel{children = Els} = SubEl,
	  case fxml:remove_cdata(Els) of
	    [#xmlel{name = <<"x">>} = XEl] ->
		case {fxml:get_tag_attr_s(<<"xmlns">>, XEl),
		      fxml:get_tag_attr_s(<<"type">>, XEl)}
		    of
		  {?NS_XDATA, <<"cancel">>} -> {result, [], StateData};
		  {?NS_XDATA, <<"submit">>} ->
		      case is_allowed_log_change(XEl, StateData, From) andalso
			     is_allowed_persistent_change(XEl, StateData, From)
			       andalso
			       is_allowed_room_name_desc_limits(XEl, StateData)
				 andalso
				 is_password_settings_correct(XEl, StateData)
			  of
			true -> set_config(XEl, StateData, Lang);
			false -> {error, ?ERR_NOT_ACCEPTABLE}
		      end;
		  _ ->
		     Txt = <<"Incorrect data form">>,
		     {error, ?ERRT_BAD_REQUEST(Lang, Txt)}
		end;
	    [#xmlel{name = <<"destroy">>} = SubEl1] ->
		?INFO_MSG("Destroyed MUC room ~s by the owner ~s",
			  [jid:to_string(StateData#state.jid),
			   jid:to_string(From)]),
		add_to_log(room_existence, destroyed, StateData),
		destroy_room(SubEl1, StateData);
	    Items ->
		process_admin_items_set(From, Items, Lang, StateData)
	  end;
      _ ->
	  ErrText = <<"Owner privileges required">>,
	  {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
    end;
process_iq_owner(From, get, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
      owner ->
	  #xmlel{children = Els} = SubEl,
	  case fxml:remove_cdata(Els) of
	    [] -> get_config(Lang, StateData, From);
	    [Item] ->
		case fxml:get_tag_attr(<<"affiliation">>, Item) of
		  false ->
		      Txt = <<"No 'affiliation' attribute found">>,
		      {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
		  {value, StrAffiliation} ->
		      case catch list_to_affiliation(StrAffiliation) of
			{'EXIT', _} ->
			    ErrText = iolist_to_binary(
                                        io_lib:format(
                                          translate:translate(
                                            Lang,
                                            <<"Invalid affiliation: ~s">>),
                                          [StrAffiliation])),
			    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
			SAffiliation ->
			    Items = items_with_affiliation(SAffiliation,
							   StateData),
			    {result, Items, StateData}
		      end
		end;
	    _ -> {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
	  end;
      _ ->
	  ErrText = <<"Owner privileges required">>,
	  {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
    end.

is_allowed_log_change(XEl, StateData, From) ->
    case lists:keymember(<<"muc#roomconfig_enablelogging">>,
			 1, jlib:parse_xdata_submit(XEl))
	of
      false -> true;
      true ->
	  allow ==
	    mod_muc_log:check_access_log(StateData#state.server_host,
					 From)
    end.

is_allowed_persistent_change(XEl, StateData, From) ->
    case
      lists:keymember(<<"muc#roomconfig_persistentroom">>, 1,
		      jlib:parse_xdata_submit(XEl))
	of
      false -> true;
      true ->
	  {_AccessRoute, _AccessCreate, _AccessAdmin,
	   AccessPersistent} =
	      StateData#state.access,
	  allow ==
	    acl:match_rule(StateData#state.server_host,
			   AccessPersistent, From)
    end.

%% Check if the Room Name and Room Description defined in the Data Form
%% are conformant to the configured limits
is_allowed_room_name_desc_limits(XEl, StateData) ->
    IsNameAccepted = case
		       lists:keysearch(<<"muc#roomconfig_roomname">>, 1,
				       jlib:parse_xdata_submit(XEl))
			 of
		       {value, {_, [N]}} ->
			   byte_size(N) =<
			     gen_mod:get_module_opt(StateData#state.server_host,
						    mod_muc, max_room_name,
                                                    fun(infinity) -> infinity;
                                                       (I) when is_integer(I),
                                                                I>0 -> I
                                                    end, infinity);
		       _ -> true
		     end,
    IsDescAccepted = case
		       lists:keysearch(<<"muc#roomconfig_roomdesc">>, 1,
				       jlib:parse_xdata_submit(XEl))
			 of
		       {value, {_, [D]}} ->
			   byte_size(D) =<
			     gen_mod:get_module_opt(StateData#state.server_host,
						    mod_muc, max_room_desc,
                                                    fun(infinity) -> infinity;
                                                       (I) when is_integer(I),
                                                                I>0 ->
                                                            I
                                                    end, infinity);
		       _ -> true
		     end,
    IsNameAccepted and IsDescAccepted.

%% Return false if:
%% "the password for a password-protected room is blank"
is_password_settings_correct(XEl, StateData) ->
    Config = StateData#state.config,
    OldProtected = Config#config.password_protected,
    OldPassword = Config#config.password,
    NewProtected = case
		     lists:keysearch(<<"muc#roomconfig_passwordprotectedroom">>,
				     1, jlib:parse_xdata_submit(XEl))
		       of
		     {value, {_, [<<"1">>]}} -> true;
		     {value, {_, [<<"0">>]}} -> false;
		     _ -> undefined
		   end,
    NewPassword = case
		    lists:keysearch(<<"muc#roomconfig_roomsecret">>, 1,
				    jlib:parse_xdata_submit(XEl))
		      of
		    {value, {_, [P]}} -> P;
		    _ -> undefined
		  end,
    case {OldProtected, NewProtected, OldPassword,
	  NewPassword}
	of
      {true, undefined, <<"">>, undefined} -> false;
      {true, undefined, _, <<"">>} -> false;
      {_, true, <<"">>, undefined} -> false;
      {_, true, _, <<"">>} -> false;
      _ -> true
    end.

-define(XFIELD(Type, Label, Var, Val),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type},
		    {<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

-define(BOOLXFIELD(Label, Var, Val),
	?XFIELD(<<"boolean">>, Label, Var,
		case Val of
		  true -> <<"1">>;
		  _ -> <<"0">>
		end)).

-define(STRINGXFIELD(Label, Var, Val),
	?XFIELD(<<"text-single">>, Label, Var, Val)).

-define(PRIVATEXFIELD(Label, Var, Val),
	?XFIELD(<<"text-private">>, Label, Var, Val)).

-define(JIDMULTIXFIELD(Label, Var, JIDList),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, <<"jid-multi">>},
		    {<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, jid:to_string(JID)}]}
		    || JID <- JIDList]}).

get_default_room_maxusers(RoomState) ->
    DefRoomOpts =
	gen_mod:get_module_opt(RoomState#state.server_host,
			       mod_muc, default_room_options,
                               fun(L) when is_list(L) -> L end,
                               []),
    RoomState2 = set_opts(DefRoomOpts, RoomState),
    (RoomState2#state.config)#config.max_users.

get_config(Lang, StateData, From) ->
    {_AccessRoute, _AccessCreate, _AccessAdmin,
     AccessPersistent} =
	StateData#state.access,
    ServiceMaxUsers = get_service_max_users(StateData),
    DefaultRoomMaxUsers =
	get_default_room_maxusers(StateData),
    Config = StateData#state.config,
    {MaxUsersRoomInteger, MaxUsersRoomString} = case
						  get_max_users(StateData)
						    of
						  N when is_integer(N) ->
						      {N,
						       jlib:integer_to_binary(N)};
						  _ -> {0, <<"none">>}
						end,
    Res = [#xmlel{name = <<"title">>, attrs = [],
		  children =
		      [{xmlcdata,
			iolist_to_binary(
                          io_lib:format(
                            translate:translate(
                              Lang,
                              <<"Configuration of room ~s">>),
                            [jid:to_string(StateData#state.jid)]))}]},
	   #xmlel{name = <<"field">>,
		  attrs =
		      [{<<"type">>, <<"hidden">>},
		       {<<"var">>, <<"FORM_TYPE">>}],
		  children =
		      [#xmlel{name = <<"value">>, attrs = [],
			      children =
				  [{xmlcdata,
				    <<"http://jabber.org/protocol/muc#roomconfig">>}]}]},
	   ?STRINGXFIELD(<<"Room title">>,
			 <<"muc#roomconfig_roomname">>, (Config#config.title)),
	   ?STRINGXFIELD(<<"Room description">>,
			 <<"muc#roomconfig_roomdesc">>,
			 (Config#config.description))]
	    ++
	    case acl:match_rule(StateData#state.server_host,
				AccessPersistent, From)
		of
	      allow ->
		  [?BOOLXFIELD(<<"Make room persistent">>,
			       <<"muc#roomconfig_persistentroom">>,
			       (Config#config.persistent))];
	      _ -> []
	    end
	      ++
	      [?BOOLXFIELD(<<"Make room public searchable">>,
			   <<"muc#roomconfig_publicroom">>,
			   (Config#config.public)),
	       ?BOOLXFIELD(<<"Make participants list public">>,
			   <<"public_list">>, (Config#config.public_list)),
	       ?BOOLXFIELD(<<"Make room password protected">>,
			   <<"muc#roomconfig_passwordprotectedroom">>,
			   (Config#config.password_protected)),
	       ?PRIVATEXFIELD(<<"Password">>,
			      <<"muc#roomconfig_roomsecret">>,
			      case Config#config.password_protected of
				true -> Config#config.password;
				false -> <<"">>
			      end),
	       #xmlel{name = <<"field">>,
		      attrs =
			  [{<<"type">>, <<"list-single">>},
			   {<<"label">>,
			    translate:translate(Lang,
						<<"Maximum Number of Occupants">>)},
			   {<<"var">>, <<"muc#roomconfig_maxusers">>}],
		      children =
			  [#xmlel{name = <<"value">>, attrs = [],
				  children = [{xmlcdata, MaxUsersRoomString}]}]
			    ++
			    if is_integer(ServiceMaxUsers) -> [];
			       true ->
				   [#xmlel{name = <<"option">>,
					   attrs =
					       [{<<"label">>,
						 translate:translate(Lang,
								     <<"No limit">>)}],
					   children =
					       [#xmlel{name = <<"value">>,
						       attrs = [],
						       children =
							   [{xmlcdata,
							     <<"none">>}]}]}]
			    end
			      ++
			      [#xmlel{name = <<"option">>,
				      attrs =
					  [{<<"label">>,
					    jlib:integer_to_binary(N)}],
				      children =
					  [#xmlel{name = <<"value">>,
						  attrs = [],
						  children =
						      [{xmlcdata,
							jlib:integer_to_binary(N)}]}]}
			       || N
				      <- lists:usort([ServiceMaxUsers,
						      DefaultRoomMaxUsers,
						      MaxUsersRoomInteger
						      | ?MAX_USERS_DEFAULT_LIST]),
				  N =< ServiceMaxUsers]},
	       #xmlel{name = <<"field">>,
		      attrs =
			  [{<<"type">>, <<"list-single">>},
			   {<<"label">>,
			    translate:translate(Lang,
						<<"Present real Jabber IDs to">>)},
			   {<<"var">>, <<"muc#roomconfig_whois">>}],
		      children =
			  [#xmlel{name = <<"value">>, attrs = [],
				  children =
				      [{xmlcdata,
					if Config#config.anonymous ->
					       <<"moderators">>;
					   true -> <<"anyone">>
					end}]},
			   #xmlel{name = <<"option">>,
				  attrs =
				      [{<<"label">>,
					translate:translate(Lang,
							    <<"moderators only">>)}],
				  children =
				      [#xmlel{name = <<"value">>, attrs = [],
					      children =
						  [{xmlcdata,
						    <<"moderators">>}]}]},
			   #xmlel{name = <<"option">>,
				  attrs =
				      [{<<"label">>,
					translate:translate(Lang,
							    <<"anyone">>)}],
				  children =
				      [#xmlel{name = <<"value">>, attrs = [],
					      children =
						  [{xmlcdata,
						    <<"anyone">>}]}]}]},
	       #xmlel{name = <<"field">>,
		      attrs =
			  [{<<"type">>, <<"list-multi">>},
			   {<<"label">>,
			    translate:translate(Lang,
						<<"Roles for which Presence is Broadcasted">>)},
			   {<<"var">>, <<"muc#roomconfig_presencebroadcast">>}],
		      children =
                          lists:map(
                            fun(Role) ->
                                    #xmlel{name = <<"value">>, attrs = [],
                                           children =
                                               [{xmlcdata,
                                                 atom_to_binary(Role, utf8)}]}
                            end, Config#config.presence_broadcast
                           ) ++
			  [#xmlel{name = <<"option">>,
				  attrs =
				      [{<<"label">>,
					translate:translate(Lang,
							    <<"Moderator">>)}],
				  children =
				      [#xmlel{name = <<"value">>, attrs = [],
					      children =
						  [{xmlcdata,
						    <<"moderator">>}]}]},
			   #xmlel{name = <<"option">>,
				  attrs =
				      [{<<"label">>,
					translate:translate(Lang,
							    <<"Participant">>)}],
				  children =
				      [#xmlel{name = <<"value">>, attrs = [],
					      children =
						  [{xmlcdata,
						    <<"participant">>}]}]},
			   #xmlel{name = <<"option">>,
				  attrs =
				      [{<<"label">>,
					translate:translate(Lang,
							    <<"Visitor">>)}],
				  children =
				      [#xmlel{name = <<"value">>, attrs = [],
					      children =
						  [{xmlcdata,
						    <<"visitor">>}]}]}
                          ]},
	       ?BOOLXFIELD(<<"Make room members-only">>,
			   <<"muc#roomconfig_membersonly">>,
			   (Config#config.members_only)),
	       ?BOOLXFIELD(<<"Make room moderated">>,
			   <<"muc#roomconfig_moderatedroom">>,
			   (Config#config.moderated)),
	       ?BOOLXFIELD(<<"Default users as participants">>,
			   <<"members_by_default">>,
			   (Config#config.members_by_default)),
	       ?BOOLXFIELD(<<"Allow users to change the subject">>,
			   <<"muc#roomconfig_changesubject">>,
			   (Config#config.allow_change_subj)),
	       ?BOOLXFIELD(<<"Allow users to send private messages">>,
			   <<"allow_private_messages">>,
			   (Config#config.allow_private_messages)),
	       #xmlel{name = <<"field">>,
		      attrs =
			  [{<<"type">>, <<"list-single">>},
			   {<<"label">>,
			    translate:translate(Lang,
						<<"Allow visitors to send private messages to">>)},
			   {<<"var">>,
			    <<"allow_private_messages_from_visitors">>}],
		      children =
			  [#xmlel{name = <<"value">>, attrs = [],
				  children =
				      [{xmlcdata,
					case
					  Config#config.allow_private_messages_from_visitors
					    of
					  anyone -> <<"anyone">>;
					  moderators -> <<"moderators">>;
					  nobody -> <<"nobody">>
					end}]},
			   #xmlel{name = <<"option">>,
				  attrs =
				      [{<<"label">>,
					translate:translate(Lang,
							    <<"nobody">>)}],
				  children =
				      [#xmlel{name = <<"value">>, attrs = [],
					      children =
						  [{xmlcdata, <<"nobody">>}]}]},
			   #xmlel{name = <<"option">>,
				  attrs =
				      [{<<"label">>,
					translate:translate(Lang,
							    <<"moderators only">>)}],
				  children =
				      [#xmlel{name = <<"value">>, attrs = [],
					      children =
						  [{xmlcdata,
						    <<"moderators">>}]}]},
			   #xmlel{name = <<"option">>,
				  attrs =
				      [{<<"label">>,
					translate:translate(Lang,
							    <<"anyone">>)}],
				  children =
				      [#xmlel{name = <<"value">>, attrs = [],
					      children =
						  [{xmlcdata,
						    <<"anyone">>}]}]}]},
	       ?BOOLXFIELD(<<"Allow users to query other users">>,
			   <<"allow_query_users">>,
			   (Config#config.allow_query_users)),
	       ?BOOLXFIELD(<<"Allow users to send invites">>,
			   <<"muc#roomconfig_allowinvites">>,
			   (Config#config.allow_user_invites)),
	       ?BOOLXFIELD(<<"Allow visitors to send status text in "
			     "presence updates">>,
			   <<"muc#roomconfig_allowvisitorstatus">>,
			   (Config#config.allow_visitor_status)),
	       ?BOOLXFIELD(<<"Allow visitors to change nickname">>,
			   <<"muc#roomconfig_allowvisitornickchange">>,
			   (Config#config.allow_visitor_nickchange)),
	       ?BOOLXFIELD(<<"Allow visitors to send voice requests">>,
			   <<"muc#roomconfig_allowvoicerequests">>,
			   (Config#config.allow_voice_requests)),
	       ?BOOLXFIELD(<<"Allow subscription">>,
			   <<"muc#roomconfig_allow_subscription">>,
			   (Config#config.allow_subscription)),
	       ?STRINGXFIELD(<<"Minimum interval between voice requests "
			       "(in seconds)">>,
			     <<"muc#roomconfig_voicerequestmininterval">>,
			     (jlib:integer_to_binary(Config#config.voice_request_min_interval)))]
		++
		case ejabberd_captcha:is_feature_available() of
		  true ->
		      [?BOOLXFIELD(<<"Make room CAPTCHA protected">>,
				   <<"captcha_protected">>,
				   (Config#config.captcha_protected))];
		  false -> []
		end ++
		  [?JIDMULTIXFIELD(<<"Exclude Jabber IDs from CAPTCHA challenge">>,
				   <<"muc#roomconfig_captcha_whitelist">>,
				   ((?SETS):to_list(Config#config.captcha_whitelist)))]
		    ++
		    case
		      mod_muc_log:check_access_log(StateData#state.server_host,
						   From)
			of
		      allow ->
			  [?BOOLXFIELD(<<"Enable logging">>,
				       <<"muc#roomconfig_enablelogging">>,
				       (Config#config.logging))];
		      _ -> []
		    end,
    X = ejabberd_hooks:run_fold(get_room_config,
				StateData#state.server_host,
				Res,
				[StateData, From, Lang]),
    {result,
     [#xmlel{name = <<"instructions">>, attrs = [],
	     children =
		 [{xmlcdata,
		   translate:translate(Lang,
				       <<"You need an x:data capable client to "
					 "configure room">>)}]},
      #xmlel{name = <<"x">>,
	     attrs =
		 [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
	     children = X}],
     StateData}.

set_config(XEl, StateData, Lang) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
      invalid -> {error, ?ERRT_BAD_REQUEST(Lang, <<"Incorrect data form">>)};
      _ ->
	  case set_xoption(XData, StateData#state.config,
			   StateData#state.server_host, Lang) of
	    #config{} = Config ->
		Res = change_config(Config, StateData),
		{result, _, NSD} = Res,
		Type = case {(StateData#state.config)#config.logging,
			     Config#config.logging}
			   of
			 {true, false} -> roomconfig_change_disabledlogging;
			 {false, true} -> roomconfig_change_enabledlogging;
			 {_, _} -> roomconfig_change
		       end,
		Users = [{U#user.jid, U#user.nick, U#user.role}
			 || {_, U} <- (?DICT):to_list(StateData#state.users)],
		add_to_log(Type, Users, NSD),
		Res;
	    Err -> Err
	  end
    end.

-define(SET_BOOL_XOPT(Opt, Val),
	case Val of
	  <<"0">> ->
	      set_xoption(Opts, Config#config{Opt = false}, ServerHost, Lang);
	  <<"false">> ->
	      set_xoption(Opts, Config#config{Opt = false}, ServerHost, Lang);
	  <<"1">> -> set_xoption(Opts, Config#config{Opt = true}, ServerHost, Lang);
	  <<"true">> ->
	      set_xoption(Opts, Config#config{Opt = true}, ServerHost, Lang);
	  _ ->
	      Txt = <<"Value of '~s' should be boolean">>,
	      ErrTxt = iolist_to_binary(io_lib:format(Txt, [Opt])),
	      {error, ?ERRT_BAD_REQUEST(Lang, ErrTxt)}
	end).

-define(SET_NAT_XOPT(Opt, Val),
	case catch jlib:binary_to_integer(Val) of
	  I when is_integer(I), I > 0 ->
	      set_xoption(Opts, Config#config{Opt = I}, ServerHost, Lang);
	  _ ->
	      Txt = <<"Value of '~s' should be integer">>,
	      ErrTxt = iolist_to_binary(io_lib:format(Txt, [Opt])),
	      {error, ?ERRT_BAD_REQUEST(Lang, ErrTxt)}
	end).

-define(SET_STRING_XOPT(Opt, Val),
	set_xoption(Opts, Config#config{Opt = Val}, ServerHost, Lang)).

-define(SET_JIDMULTI_XOPT(Opt, Vals),
	begin
	  Set = lists:foldl(fun ({U, S, R}, Set1) ->
				    (?SETS):add_element({U, S, R}, Set1);
				(#jid{luser = U, lserver = S, lresource = R},
				 Set1) ->
				    (?SETS):add_element({U, S, R}, Set1);
				(_, Set1) -> Set1
			    end,
			    (?SETS):empty(), Vals),
	  set_xoption(Opts, Config#config{Opt = Set}, ServerHost, Lang)
	end).

set_xoption([], Config, _ServerHost, _Lang) -> Config;
set_xoption([{<<"muc#roomconfig_roomname">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_STRING_XOPT(title, Val);
set_xoption([{<<"muc#roomconfig_roomdesc">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_STRING_XOPT(description, Val);
set_xoption([{<<"muc#roomconfig_changesubject">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(allow_change_subj, Val);
set_xoption([{<<"allow_query_users">>, [Val]} | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(allow_query_users, Val);
set_xoption([{<<"allow_private_messages">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(allow_private_messages, Val);
set_xoption([{<<"allow_private_messages_from_visitors">>,
	      [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    case Val of
      <<"anyone">> ->
	  ?SET_STRING_XOPT(allow_private_messages_from_visitors,
			   anyone);
      <<"moderators">> ->
	  ?SET_STRING_XOPT(allow_private_messages_from_visitors,
			   moderators);
      <<"nobody">> ->
	  ?SET_STRING_XOPT(allow_private_messages_from_visitors,
			   nobody);
      _ ->
	  Txt = <<"Value of 'allow_private_messages_from_visitors' "
		  "should be anyone|moderators|nobody">>,
	  {error, ?ERRT_BAD_REQUEST(Lang, Txt)}
    end;
set_xoption([{<<"muc#roomconfig_allowvisitorstatus">>,
	      [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(allow_visitor_status, Val);
set_xoption([{<<"muc#roomconfig_allowvisitornickchange">>,
	      [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(allow_visitor_nickchange, Val);
set_xoption([{<<"muc#roomconfig_publicroom">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(public, Val);
set_xoption([{<<"public_list">>, [Val]} | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(public_list, Val);
set_xoption([{<<"muc#roomconfig_persistentroom">>,
	      [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(persistent, Val);
set_xoption([{<<"muc#roomconfig_moderatedroom">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(moderated, Val);
set_xoption([{<<"members_by_default">>, [Val]} | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(members_by_default, Val);
set_xoption([{<<"muc#roomconfig_membersonly">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(members_only, Val);
set_xoption([{<<"captcha_protected">>, [Val]} | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(captcha_protected, Val);
set_xoption([{<<"muc#roomconfig_allowinvites">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(allow_user_invites, Val);
set_xoption([{<<"muc#roomconfig_allow_subscription">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(allow_subscription, Val);
set_xoption([{<<"muc#roomconfig_passwordprotectedroom">>,
	      [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(password_protected, Val);
set_xoption([{<<"muc#roomconfig_roomsecret">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_STRING_XOPT(password, Val);
set_xoption([{<<"anonymous">>, [Val]} | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(anonymous, Val);
set_xoption([{<<"muc#roomconfig_presencebroadcast">>, Vals} | Opts],
	    Config, ServerHost, Lang) ->
    Roles =
        lists:foldl(
          fun(_S, error) -> error;
             (S, {M, P, V}) ->
                  case S of
                      <<"moderator">> -> {true, P, V};
                      <<"participant">> -> {M, true, V};
                      <<"visitor">> -> {M, P, true};
                      _ -> error
                  end
          end, {false, false, false}, Vals),
    case Roles of
        error ->
	    Txt = <<"Value of 'muc#roomconfig_presencebroadcast' should "
		    "be moderator|participant|visitor">>,
	    {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
        {M, P, V} ->
            Res =
                if M -> [moderator]; true -> [] end ++
                if P -> [participant]; true -> [] end ++
                if V -> [visitor]; true -> [] end,
            set_xoption(Opts, Config#config{presence_broadcast = Res},
			ServerHost, Lang)
    end;
set_xoption([{<<"muc#roomconfig_allowvoicerequests">>,
	      [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(allow_voice_requests, Val);
set_xoption([{<<"muc#roomconfig_voicerequestmininterval">>,
	      [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_NAT_XOPT(voice_request_min_interval, Val);
set_xoption([{<<"muc#roomconfig_whois">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    case Val of
      <<"moderators">> ->
	  ?SET_BOOL_XOPT(anonymous,
			 (iolist_to_binary(integer_to_list(1))));
      <<"anyone">> ->
	  ?SET_BOOL_XOPT(anonymous,
			 (iolist_to_binary(integer_to_list(0))));
      _ ->
	  Txt = <<"Value of 'muc#roomconfig_whois' should be "
		  "moderators|anyone">>,
	  {error, ?ERRT_BAD_REQUEST(Lang, Txt)}
    end;
set_xoption([{<<"muc#roomconfig_maxusers">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    case Val of
      <<"none">> -> ?SET_STRING_XOPT(max_users, none);
      _ -> ?SET_NAT_XOPT(max_users, Val)
    end;
set_xoption([{<<"muc#roomconfig_enablelogging">>, [Val]}
	     | Opts],
	    Config, ServerHost, Lang) ->
    ?SET_BOOL_XOPT(logging, Val);
set_xoption([{<<"muc#roomconfig_captcha_whitelist">>,
	      Vals}
	     | Opts],
	    Config, ServerHost, Lang) ->
    JIDs = [jid:from_string(Val) || Val <- Vals],
    ?SET_JIDMULTI_XOPT(captcha_whitelist, JIDs);
set_xoption([{<<"FORM_TYPE">>, _} | Opts], Config, ServerHost, Lang) ->
    set_xoption(Opts, Config, ServerHost, Lang);
set_xoption([{Opt, Vals} | Opts], Config, ServerHost, Lang) ->
    Txt = <<"Unknown option '~s'">>,
    ErrTxt = iolist_to_binary(io_lib:format(Txt, [Opt])),
    Err = {error, ?ERRT_BAD_REQUEST(Lang, ErrTxt)},
    case ejabberd_hooks:run_fold(set_room_option,
				 ServerHost,
				 Err,
				 [Opt, Vals, Lang]) of
	{error, Reason} ->
	    {error, Reason};
	{Pos, Val} ->
	    set_xoption(Opts, setelement(Pos, Config, Val), ServerHost, Lang)
    end.

change_config(Config, StateData) ->
    send_config_change_info(Config, StateData),
    NSD = remove_subscriptions(StateData#state{config = Config}),
    case {(StateData#state.config)#config.persistent,
	  Config#config.persistent}
	of
      {_, true} ->
	  mod_muc:store_room(NSD#state.server_host,
			     NSD#state.host, NSD#state.room, make_opts(NSD));
      {true, false} ->
	  mod_muc:forget_room(NSD#state.server_host,
			      NSD#state.host, NSD#state.room);
      {false, false} -> ok
    end,
    case {(StateData#state.config)#config.members_only,
	  Config#config.members_only}
	of
      {false, true} ->
	  NSD1 = remove_nonmembers(NSD), {result, [], NSD1};
      _ -> {result, [], NSD}
    end.

send_config_change_info(Config, #state{config = Config}) -> ok;
send_config_change_info(New, #state{config = Old} = StateData) ->
    Codes = case {Old#config.logging, New#config.logging} of
	      {false, true} -> [<<"170">>];
	      {true, false} -> [<<"171">>];
	      _ -> []
	    end
	      ++
	      case {Old#config.anonymous, New#config.anonymous} of
		{true, false} -> [<<"172">>];
		{false, true} -> [<<"173">>];
		_ -> []
	      end
		++
		case Old#config{anonymous = New#config.anonymous,
				logging = New#config.logging} of
		  New -> [];
		  _ -> [<<"104">>]
		end,
    StatusEls = [#xmlel{name = <<"status">>,
			attrs = [{<<"code">>, Code}],
			children = []} || Code <- Codes],
    Message = #xmlel{name = <<"message">>,
		     attrs = [{<<"type">>, <<"groupchat">>},
			      {<<"id">>, randoms:get_string()}],
		     children = [#xmlel{name = <<"x">>,
					attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
					children = StatusEls}]},
    send_wrapped_multiple(StateData#state.jid,
			  StateData#state.users,
			  Message,
			  ?NS_MUCSUB_NODES_CONFIG,
			  StateData).

remove_nonmembers(StateData) ->
    lists:foldl(fun ({_LJID, #user{jid = JID}}, SD) ->
			Affiliation = get_affiliation(JID, SD),
			case Affiliation of
			  none ->
			      catch send_kickban_presence(<<"">>, JID, <<"">>,
							  <<"322">>, SD),
			      set_role(JID, none, SD);
			  _ -> SD
			end
		end,
		StateData, (?DICT):to_list(StateData#state.users)).

set_opts([], StateData) -> StateData;
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
	    allow_subscription ->
		StateData#state{config =
				    (StateData#state.config)#config{allow_subscription = Val}};
	    subscribers ->
		lists:foldl(
		  fun({JID, Nick, Nodes}, State) ->
			  User = #user{jid = JID, nick = Nick,
				       subscriptions = Nodes,
				       is_subscriber = true,
				       role = none},
			  update_online_user(JID, User, State)
		  end, StateData, Val);
	    affiliations ->
		StateData#state{affiliations = (?DICT):from_list(Val)};
	    subject -> StateData#state{subject = Val};
	    subject_author -> StateData#state{subject_author = Val};
	    _ -> StateData
	  end,
    set_opts(Opts, NSD).

-define(MAKE_CONFIG_OPT(Opt), {Opt, Config#config.Opt}).


make_opts(StateData) ->
    Config = StateData#state.config,
    Subscribers = (?DICT):fold(
		    fun(_LJID, #user{is_subscriber = true} = User, Acc) ->
			    [{User#user.jid, User#user.nick,
			      User#user.subscriptions}|Acc];
		       (_, _, Acc) ->
			    Acc
		    end, [], StateData#state.users),
    [?MAKE_CONFIG_OPT(title), ?MAKE_CONFIG_OPT(description),
     ?MAKE_CONFIG_OPT(allow_change_subj),
     ?MAKE_CONFIG_OPT(allow_query_users),
     ?MAKE_CONFIG_OPT(allow_private_messages),
     ?MAKE_CONFIG_OPT(allow_private_messages_from_visitors),
     ?MAKE_CONFIG_OPT(allow_visitor_status),
     ?MAKE_CONFIG_OPT(allow_visitor_nickchange),
     ?MAKE_CONFIG_OPT(public), ?MAKE_CONFIG_OPT(public_list),
     ?MAKE_CONFIG_OPT(persistent),
     ?MAKE_CONFIG_OPT(moderated),
     ?MAKE_CONFIG_OPT(members_by_default),
     ?MAKE_CONFIG_OPT(members_only),
     ?MAKE_CONFIG_OPT(allow_user_invites),
     ?MAKE_CONFIG_OPT(password_protected),
     ?MAKE_CONFIG_OPT(captcha_protected),
     ?MAKE_CONFIG_OPT(password), ?MAKE_CONFIG_OPT(anonymous),
     ?MAKE_CONFIG_OPT(logging), ?MAKE_CONFIG_OPT(max_users),
     ?MAKE_CONFIG_OPT(allow_voice_requests),
     ?MAKE_CONFIG_OPT(mam),
     ?MAKE_CONFIG_OPT(voice_request_min_interval),
     ?MAKE_CONFIG_OPT(vcard),
     {captcha_whitelist,
      (?SETS):to_list((StateData#state.config)#config.captcha_whitelist)},
     {affiliations,
      (?DICT):to_list(StateData#state.affiliations)},
     {subject, StateData#state.subject},
     {subject_author, StateData#state.subject_author},
     {subscribers, Subscribers}].

destroy_room(DEl, StateData) ->
    lists:foreach(fun ({_LJID, Info}) ->
			  Nick = Info#user.nick,
			  ItemAttrs = [{<<"affiliation">>, <<"none">>},
				       {<<"role">>, <<"none">>}],
			  Packet = #xmlel{name = <<"presence">>,
					  attrs =
					      [{<<"type">>, <<"unavailable">>}],
					  children =
					      [#xmlel{name = <<"x">>,
						      attrs =
							  [{<<"xmlns">>,
							    ?NS_MUC_USER}],
						      children =
							  [#xmlel{name =
								      <<"item">>,
								  attrs =
								      ItemAttrs,
								  children =
								      []},
							   DEl]}]},
			  send_wrapped(jid:replace_resource(StateData#state.jid,
							    Nick),
				       Info#user.jid, Packet,
				       ?NS_MUCSUB_NODES_CONFIG, StateData)
		  end,
		  (?DICT):to_list(StateData#state.users)),
    case (StateData#state.config)#config.persistent of
      true ->
	  mod_muc:forget_room(StateData#state.server_host,
			      StateData#state.host, StateData#state.room);
      false -> ok
    end,
    {result, [], stop}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disco

-define(FEATURE(Var),
	#xmlel{name = <<"feature">>, attrs = [{<<"var">>, Var}],
	       children = []}).

-define(CONFIG_OPT_TO_FEATURE(Opt, Fiftrue, Fiffalse),
	case Opt of
	  true -> ?FEATURE(Fiftrue);
	  false -> ?FEATURE(Fiffalse)
	end).

process_iq_disco_info(_From, set, Lang, _StateData) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    {error, ?ERRT_NOT_ALLOWED(Lang, Txt)};
process_iq_disco_info(_From, get, Lang, StateData) ->
    Config = StateData#state.config,
    {result,
     [#xmlel{name = <<"identity">>,
	     attrs =
		 [{<<"category">>, <<"conference">>},
		  {<<"type">>, <<"text">>},
		  {<<"name">>, get_title(StateData)}],
	     children = []},
      #xmlel{name = <<"feature">>,
	     attrs = [{<<"var">>, ?NS_VCARD}], children = []},
      #xmlel{name = <<"feature">>,
	     attrs = [{<<"var">>, ?NS_MUC}], children = []},
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
	      true -> [?FEATURE(?NS_MUCSUB)];
	      false -> []
	  end
       ++ case {gen_mod:is_loaded(StateData#state.server_host, mod_mam),
		Config#config.mam} of
	    {true, true} ->
		[?FEATURE(?NS_MAM_TMP),
		 ?FEATURE(?NS_MAM_0),
		 ?FEATURE(?NS_MAM_1)];
	    _ ->
		[]
	  end
       ++ iq_disco_info_extras(Lang, StateData),
     StateData}.

-define(RFIELDT(Type, Var, Val),
	#xmlel{name = <<"field">>,
	       attrs = [{<<"type">>, Type}, {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

-define(RFIELD(Label, Var, Val),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

iq_disco_info_extras(Lang, StateData) ->
    Len = (?DICT):size(StateData#state.users),
    RoomDescription =
	(StateData#state.config)#config.description,
    [#xmlel{name = <<"x">>,
	    attrs =
		[{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
	    children =
		[?RFIELDT(<<"hidden">>, <<"FORM_TYPE">>,
			  <<"http://jabber.org/protocol/muc#roominfo">>),
		 ?RFIELD(<<"Room description">>,
			 <<"muc#roominfo_description">>, RoomDescription),
		 ?RFIELD(<<"Number of occupants">>,
			 <<"muc#roominfo_occupants">>,
			 (iolist_to_binary(integer_to_list(Len))))]}].

process_iq_disco_items(_From, set, Lang, _StateData) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    {error, ?ERRT_NOT_ALLOWED(Lang, Txt)};
process_iq_disco_items(From, get, Lang, StateData) ->
    case (StateData#state.config)#config.public_list of
      true ->
	  {result, get_mucroom_disco_items(StateData), StateData};
      _ ->
	  case is_occupant_or_admin(From, StateData) of
	    true ->
		{result, get_mucroom_disco_items(StateData), StateData};
	    _ ->
	        Txt = <<"Only occupants or administrators can perform this query">>,
		{error, ?ERRT_FORBIDDEN(Lang, Txt)}
	  end
    end.

process_iq_captcha(_From, get, Lang, _SubEl,
		   _StateData) ->
    Txt = <<"Value 'get' of 'type' attribute is not allowed">>,
    {error, ?ERRT_NOT_ALLOWED(Lang, Txt)};
process_iq_captcha(_From, set, Lang, SubEl,
		   StateData) ->
    case ejabberd_captcha:process_reply(SubEl) of
      ok -> {result, [], StateData};
      {error, malformed} ->
	    Txt = <<"Incorrect CAPTCHA submit">>,
	    {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
      _ ->
	    Txt = <<"The CAPTCHA verification has failed">>,
	    {error, ?ERRT_NOT_ALLOWED(Lang, Txt)}
    end.

process_iq_vcard(_From, get, _Lang, _SubEl, StateData) ->
    #state{config = #config{vcard = VCardRaw}} = StateData,
    case fxml_stream:parse_element(VCardRaw) of
	#xmlel{children = VCardEls} ->
	    {result, VCardEls, StateData};
	{error, _} ->
	    {result, [], StateData}
    end;
process_iq_vcard(From, set, Lang, SubEl, StateData) ->
    case get_affiliation(From, StateData) of
	owner ->
	    VCardRaw = fxml:element_to_binary(SubEl),
	    Config = StateData#state.config,
	    NewConfig = Config#config{vcard = VCardRaw},
	    change_config(NewConfig, StateData);
	_ ->
	    ErrText = <<"Owner privileges required">>,
	    {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
    end.

process_iq_mucsub(From, Packet,
		  #iq{type = set, lang = Lang,
		      sub_el = #xmlel{name = <<"subscribe">>} = SubEl},
		  #state{config = Config} = StateData) ->
    case fxml:get_tag_attr_s(<<"nick">>, SubEl) of
	<<"">> ->
	    Err = ?ERRT_BAD_REQUEST(Lang, <<"Missing 'nick' attribute">>),
	    {error, Err};
	Nick when Config#config.allow_subscription ->
	    LJID = jid:tolower(From),
	    case (?DICT):find(LJID, StateData#state.users) of
		{ok, #user{role = Role, nick = Nick1}} when Nick1 /= Nick ->
		    Nodes = get_subscription_nodes(Packet),
		    case {nick_collision(From, Nick, StateData),
			  mod_muc:can_use_nick(StateData#state.server_host,
					       StateData#state.host,
					       From, Nick)} of
			{true, _} ->
			    ErrText = <<"That nickname is already in use by another occupant">>,
			    {error, ?ERRT_CONFLICT(Lang, ErrText)};
			{_, false} ->
			    ErrText = <<"That nickname is registered by another person">>,
			    {error, ?ERRT_CONFLICT(Lang, ErrText)};
			_ ->
			    NewStateData = add_online_user(
					     From, Nick, Role, true, Nodes, StateData),
			    {result, subscription_nodes_to_events(Nodes), NewStateData}
		    end;
		{ok, #user{role = Role}} ->
		    Nodes = get_subscription_nodes(Packet),
		    NewStateData = add_online_user(
				     From, Nick, Role, true, Nodes, StateData),
		    {result, subscription_nodes_to_events(Nodes), NewStateData};
		error ->
		    add_new_user(From, Nick, Packet, StateData)
	    end;
	_ ->
	    Err = ?ERRT_NOT_ALLOWED(Lang, <<"Subscriptions are not allowed">>),
	    {error, Err}
    end;
process_iq_mucsub(From, _Packet,
		  #iq{type = set,
		      sub_el = #xmlel{name = <<"unsubscribe">>}},
		  StateData) ->
    LJID = jid:tolower(From),
    case ?DICT:find(LJID, StateData#state.users) of
	{ok, #user{is_subscriber = true} = User} ->
	    NewStateData = remove_subscription(From, User, StateData),
	    store_room(NewStateData),
	    {result, [], NewStateData};
	_ ->
	    {result, [], StateData}
    end;
process_iq_mucsub(_From, _Packet, #iq{type = set, lang = Lang}, _StateData) ->
    Txt = <<"Unrecognized subscription command">>,
    {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
process_iq_mucsub(_From, _Packet, #iq{type = get, lang = Lang}, _StateData) ->
    Txt = <<"Value 'get' of 'type' attribute is not allowed">>,
    {error, ?ERRT_BAD_REQUEST(Lang, Txt)}.

remove_subscription(JID, #user{is_subscriber = true} = User, StateData) ->
    case User#user.last_presence of
	undefined ->
	    remove_online_user(JID, StateData, false);
	_ ->
	    LJID = jid:tolower(JID),
	    Users = ?DICT:store(LJID, User#user{is_subscriber = false},
				StateData#state.users),
	    StateData#state{users = Users}
    end;
remove_subscription(_JID, #user{}, StateData) ->
    StateData.

remove_subscriptions(StateData) ->
    if not (StateData#state.config)#config.allow_subscription ->
	    dict:fold(
	      fun(_LJID, User, State) ->
		      remove_subscription(User#user.jid, User, State)
	      end, StateData, StateData#state.users);
       true ->
	    StateData
    end.

get_subscription_nodes(#xmlel{name = <<"iq">>} = Packet) ->
    case fxml:get_subtag_with_xmlns(Packet, <<"subscribe">>, ?NS_MUCSUB) of
	#xmlel{children = Els} ->
	    lists:flatmap(
	      fun(#xmlel{name = <<"event">>, attrs = Attrs}) ->
		      Node = fxml:get_attr_s(<<"node">>, Attrs),
		      case lists:member(Node, [?NS_MUCSUB_NODES_PRESENCE,
					       ?NS_MUCSUB_NODES_MESSAGES,
					       ?NS_MUCSUB_NODES_AFFILIATIONS,
					       ?NS_MUCSUB_NODES_SUBJECT,
					       ?NS_MUCSUB_NODES_CONFIG,
					       ?NS_MUCSUB_NODES_PARTICIPANTS]) of
			  true ->
			      [Node];
			  false ->
			      []
		      end;
		 (_) ->
		      []
	      end, Els);
	false ->
	    []
    end;
get_subscription_nodes(_) ->
    [].

subscription_nodes_to_events(Nodes) ->
    [#xmlel{name = <<"event">>, attrs = [{<<"node">>, Node}]} || Node <- Nodes].

get_title(StateData) ->
    case (StateData#state.config)#config.title of
      <<"">> -> StateData#state.room;
      Name -> Name
    end.

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

get_roomdesc_tail(StateData, Lang) ->
    Desc = case (StateData#state.config)#config.public of
	     true -> <<"">>;
	     _ -> translate:translate(Lang, <<"private, ">>)
	   end,
    Len = (?DICT):fold(fun (_, _, Acc) -> Acc + 1 end, 0,
		       StateData#state.users),
    <<" (", Desc/binary,
      (iolist_to_binary(integer_to_list(Len)))/binary, ")">>.

get_mucroom_disco_items(StateData) ->
    lists:map(fun ({_LJID, Info}) ->
		      Nick = Info#user.nick,
		      #xmlel{name = <<"item">>,
			     attrs =
				 [{<<"jid">>,
				   jid:to_string({StateData#state.room,
						       StateData#state.host,
						       Nick})},
				  {<<"name">>, Nick}],
			     children = []}
	      end,
	      (?DICT):to_list(StateData#state.users)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Voice request support

is_voice_request(Els) ->
    lists:foldl(fun (#xmlel{name = <<"x">>, attrs = Attrs} =
			 El,
		     false) ->
			case fxml:get_attr_s(<<"xmlns">>, Attrs) of
			  ?NS_XDATA ->
			      case jlib:parse_xdata_submit(El) of
				[_ | _] = Fields ->
				    case {lists:keysearch(<<"FORM_TYPE">>, 1,
							  Fields),
					  lists:keysearch(<<"muc#role">>, 1,
							  Fields)}
					of
				      {{value,
					{_,
					 [<<"http://jabber.org/protocol/muc#request">>]}},
				       {value, {_, [<<"participant">>]}}} ->
					  true;
				      _ -> false
				    end;
				_ -> false
			      end;
			  _ -> false
			end;
		    (_, Acc) -> Acc
		end,
		false, Els).

prepare_request_form(Requester, Nick, Lang) ->
    #xmlel{name = <<"message">>,
	   attrs = [{<<"type">>, <<"normal">>}],
	   children =
	       [#xmlel{name = <<"x">>,
		       attrs =
			   [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
		       children =
			   [#xmlel{name = <<"title">>, attrs = [],
				   children =
				       [{xmlcdata,
					 translate:translate(Lang,
							     <<"Voice request">>)}]},
			    #xmlel{name = <<"instructions">>, attrs = [],
				   children =
				       [{xmlcdata,
					 translate:translate(Lang,
							     <<"Either approve or decline the voice "
							       "request.">>)}]},
			    #xmlel{name = <<"field">>,
				   attrs =
				       [{<<"var">>, <<"FORM_TYPE">>},
					{<<"type">>, <<"hidden">>}],
				   children =
				       [#xmlel{name = <<"value">>, attrs = [],
					       children =
						   [{xmlcdata,
						     <<"http://jabber.org/protocol/muc#request">>}]}]},
			    #xmlel{name = <<"field">>,
				   attrs =
				       [{<<"var">>, <<"muc#role">>},
					{<<"type">>, <<"hidden">>}],
				   children =
				       [#xmlel{name = <<"value">>, attrs = [],
					       children =
						   [{xmlcdata,
						     <<"participant">>}]}]},
			    ?STRINGXFIELD(<<"User JID">>, <<"muc#jid">>,
					  (jid:to_string(Requester))),
			    ?STRINGXFIELD(<<"Nickname">>, <<"muc#roomnick">>,
					  Nick),
			    ?BOOLXFIELD(<<"Grant voice to this person?">>,
					<<"muc#request_allow">>,
					(jlib:binary_to_atom(<<"false">>)))]}]}.

send_voice_request(From, StateData) ->
    Moderators = search_role(moderator, StateData),
    FromNick = find_nick_by_jid(From, StateData),
    lists:foreach(fun ({_, User}) ->
			  ejabberd_router:route(
			    StateData#state.jid, User#user.jid,
			    prepare_request_form(From, FromNick, <<"">>))
		  end,
		  Moderators).

is_voice_approvement(Els) ->
    lists:foldl(fun (#xmlel{name = <<"x">>, attrs = Attrs} =
			 El,
		     false) ->
			case fxml:get_attr_s(<<"xmlns">>, Attrs) of
			  ?NS_XDATA ->
			      case jlib:parse_xdata_submit(El) of
				[_ | _] = Fs ->
				    case {lists:keysearch(<<"FORM_TYPE">>, 1,
							  Fs),
					  lists:keysearch(<<"muc#role">>, 1,
							  Fs),
					  lists:keysearch(<<"muc#request_allow">>,
							  1, Fs)}
					of
				      {{value,
					{_,
					 [<<"http://jabber.org/protocol/muc#request">>]}},
				       {value, {_, [<<"participant">>]}},
				       {value, {_, [Flag]}}}
					  when Flag == <<"true">>;
					       Flag == <<"1">> ->
					  true;
				      _ -> false
				    end;
				_ -> false
			      end;
			  _ -> false
			end;
		    (_, Acc) -> Acc
		end,
		false, Els).

extract_jid_from_voice_approvement(Els) ->
    lists:foldl(fun (#xmlel{name = <<"x">>} = El, error) ->
			Fields = case jlib:parse_xdata_submit(El) of
				   invalid -> [];
				   Res -> Res
				 end,
			lists:foldl(fun ({<<"muc#jid">>, [JIDStr]}, error) ->
					    case jid:from_string(JIDStr) of
					      error -> error;
					      J -> {ok, J}
					    end;
					(_, Acc) -> Acc
				    end,
				    error, Fields);
		    (_, Acc) -> Acc
		end,
		error, Els).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Invitation support

is_invitation(Els) ->
    lists:foldl(fun (#xmlel{name = <<"x">>, attrs = Attrs} =
			 El,
		     false) ->
			case fxml:get_attr_s(<<"xmlns">>, Attrs) of
			  ?NS_MUC_USER ->
			      case fxml:get_subtag(El, <<"invite">>) of
				false -> false;
				_ -> true
			      end;
			  _ -> false
			end;
		    (_, Acc) -> Acc
		end,
		false, Els).

check_invitation(From, Packet, Lang, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    CanInvite =
	(StateData#state.config)#config.allow_user_invites
	  orelse
	  FAffiliation == admin orelse FAffiliation == owner,
    InviteEl = case fxml:get_subtag_with_xmlns(Packet, <<"x">>, ?NS_MUC_USER) of
		   false ->
		       Txt1 = <<"No 'x' element found">>,
		       throw({error, ?ERRT_BAD_REQUEST(Lang, Txt1)});
		   XEl ->
		       case fxml:get_subtag(XEl, <<"invite">>) of
			   false ->
			       Txt2 = <<"No 'invite' element found">>,
			       throw({error, ?ERRT_BAD_REQUEST(Lang, Txt2)});
			   InviteEl1 ->
			       InviteEl1
		       end
	       end,
    JID = case
	    jid:from_string(fxml:get_tag_attr_s(<<"to">>,
						  InviteEl))
	      of
	    error ->
		  Txt = <<"Incorrect value of 'to' attribute">>,
		  throw({error, ?ERRT_JID_MALFORMED(Lang, Txt)});
	    JID1 -> JID1
	  end,
    case CanInvite of
      false ->
	  Txt3 = <<"Invitations are not allowed in this conference">>,
	  throw({error, ?ERRT_NOT_ALLOWED(Lang, Txt3)});
      true ->
	  Reason = fxml:get_path_s(InviteEl,
				  [{elem, <<"reason">>}, cdata]),
	  ContinueEl = case fxml:get_path_s(InviteEl,
					   [{elem, <<"continue">>}])
			   of
			 <<>> -> [];
			 Continue1 -> [Continue1]
		       end,
	  IEl = [#xmlel{name = <<"invite">>,
			attrs = [{<<"from">>, jid:to_string(From)}],
			children =
			    [#xmlel{name = <<"reason">>, attrs = [],
				    children = [{xmlcdata, Reason}]}]
			      ++ ContinueEl}],
	  PasswdEl = case
		       (StateData#state.config)#config.password_protected
			 of
		       true ->
			   [#xmlel{name = <<"password">>, attrs = [],
				   children =
				       [{xmlcdata,
					 (StateData#state.config)#config.password}]}];
		       _ -> []
		     end,
	  Body = #xmlel{name = <<"body">>, attrs = [],
			children =
			    [{xmlcdata,
			      iolist_to_binary(
                                [io_lib:format(
                                  translate:translate(
                                    Lang,
                                    <<"~s invites you to the room ~s">>),
                                  [jid:to_string(From),
                                   jid:to_string({StateData#state.room,
                                                       StateData#state.host,
                                                       <<"">>})]),
				case
				  (StateData#state.config)#config.password_protected
				    of
				  true ->
				      <<", ",
					(translate:translate(Lang,
							     <<"the password is">>))/binary,
					" '",
					((StateData#state.config)#config.password)/binary,
					"'">>;
				  _ -> <<"">>
				end
				  ,
				  case Reason of
				    <<"">> -> <<"">>;
				    _ -> <<" (", Reason/binary, ") ">>
				  end])}]},
	  Msg = #xmlel{name = <<"message">>,
		       attrs = [{<<"type">>, <<"normal">>}],
		       children =
			   [#xmlel{name = <<"x">>,
				   attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
				   children = IEl ++ PasswdEl},
			    #xmlel{name = <<"x">>,
				   attrs =
				       [{<<"xmlns">>, ?NS_XCONFERENCE},
					{<<"jid">>,
					 jid:to_string({StateData#state.room,
							     StateData#state.host,
							     <<"">>})}],
				   children = [{xmlcdata, Reason}]},
			    Body]},
	  ejabberd_router:route(StateData#state.jid, JID, Msg),
	  JID
    end.

%% Handle a message sent to the room by a non-participant.
%% If it is a decline, send to the inviter.
%% Otherwise, an error message is sent to the sender.
handle_roommessage_from_nonparticipant(Packet, Lang,
				       StateData, From) ->
    case catch check_decline_invitation(Packet) of
      {true, Decline_data} ->
	  send_decline_invitation(Decline_data,
				  StateData#state.jid, From);
      _ ->
	  send_error_only_occupants(Packet, Lang,
				    StateData#state.jid, From)
    end.

%% Check in the packet is a decline.
%% If so, also returns the splitted packet.
%% This function must be catched,
%% because it crashes when the packet is not a decline message.
check_decline_invitation(Packet) ->
    #xmlel{name = <<"message">>} = Packet,
    XEl = fxml:get_subtag(Packet, <<"x">>),
    (?NS_MUC_USER) = fxml:get_tag_attr_s(<<"xmlns">>, XEl),
    DEl = fxml:get_subtag(XEl, <<"decline">>),
    ToString = fxml:get_tag_attr_s(<<"to">>, DEl),
    ToJID = jid:from_string(ToString),
    {true, {Packet, XEl, DEl, ToJID}}.

%% Send the decline to the inviter user.
%% The original stanza must be slightly modified.
send_decline_invitation({Packet, XEl, DEl, ToJID},
			RoomJID, FromJID) ->
    FromString =
	jid:to_string(jid:remove_resource(FromJID)),
    #xmlel{name = <<"decline">>, attrs = DAttrs,
	   children = DEls} =
	DEl,
    DAttrs2 = lists:keydelete(<<"to">>, 1, DAttrs),
    DAttrs3 = [{<<"from">>, FromString} | DAttrs2],
    DEl2 = #xmlel{name = <<"decline">>, attrs = DAttrs3,
		  children = DEls},
    XEl2 = replace_subelement(XEl, DEl2),
    Packet2 = replace_subelement(Packet, XEl2),
    ejabberd_router:route(RoomJID, ToJID, Packet2).

%% Given an element and a new subelement,
%% replace the instance of the subelement in element with the new subelement.
replace_subelement(#xmlel{name = Name, attrs = Attrs,
			  children = SubEls},
		   NewSubEl) ->
    {_, NameNewSubEl, _, _} = NewSubEl,
    SubEls2 = lists:keyreplace(NameNewSubEl, 2, SubEls, NewSubEl),
    #xmlel{name = Name, attrs = Attrs, children = SubEls2}.

send_error_only_occupants(Packet, Lang, RoomJID, From) ->
    ErrText =
	<<"Only occupants are allowed to send messages "
	  "to the conference">>,
    Err = jlib:make_error_reply(Packet,
				?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
    ejabberd_router:route(RoomJID, From, Err).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging

add_to_log(Type, Data, StateData)
    when Type == roomconfig_change_disabledlogging ->
    mod_muc_log:add_to_log(StateData#state.server_host,
			   roomconfig_change, Data, StateData#state.jid,
			   make_opts(StateData));
add_to_log(Type, Data, StateData) ->
    case (StateData#state.config)#config.logging of
      true ->
	  mod_muc_log:add_to_log(StateData#state.server_host,
				 Type, Data, StateData#state.jid,
				 make_opts(StateData));
      false -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Users number checking

tab_add_online_user(JID, StateData) ->
    {LUser, LServer, LResource} = jid:tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:insert(muc_online_users,
		     #muc_online_users{us = US, resource = LResource,
				       room = Room, host = Host}).

tab_remove_online_user(JID, StateData) ->
    {LUser, LServer, LResource} = jid:tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:delete_object(muc_online_users,
			    #muc_online_users{us = US, resource = LResource,
					      room = Room, host = Host}).

tab_count_user(JID) ->
    {LUser, LServer, _} = jid:tolower(JID),
    US = {LUser, LServer},
    case catch ets:select(muc_online_users,
			  [{#muc_online_users{us = US, _ = '_'}, [], [[]]}])
	of
      Res when is_list(Res) -> length(Res);
      _ -> 0
    end.

element_size(El) ->
    byte_size(fxml:element_to_binary(El)).

store_room(StateData) ->
    if (StateData#state.config)#config.persistent ->
	    mod_muc:store_room(StateData#state.server_host,
			       StateData#state.host, StateData#state.room,
			       make_opts(StateData));
       true ->
	    ok
    end.

send_wrapped(From, To, Packet, Node, State) ->
    LTo = jid:tolower(To),
    case ?DICT:find(LTo, State#state.users) of
	{ok, #user{is_subscriber = true,
		   subscriptions = Nodes,
		   last_presence = undefined}} ->
	    case lists:member(Node, Nodes) of
		true ->
		    NewPacket = wrap(From, To, Packet, Node),
		    ejabberd_router:route(State#state.jid, To, NewPacket);
		false ->
		    ok
	    end;
	_ ->
	    ejabberd_router:route(From, To, Packet)
    end.

wrap(From, To, Packet, Node) ->
    Pkt1 = jlib:replace_from_to(From, To, Packet),
    Pkt2 = #xmlel{attrs = Attrs} = jlib:remove_attr(<<"xmlns">>, Pkt1),
    Pkt3 = Pkt2#xmlel{attrs = [{<<"xmlns">>, <<"jabber:client">>}|Attrs]},
    Item = #xmlel{name = <<"item">>,
		  attrs = [{<<"id">>, randoms:get_string()}],
		  children = [Pkt3]},
    Items = #xmlel{name = <<"items">>, attrs = [{<<"node">>, Node}],
		   children = [Item]},
    Event = #xmlel{name = <<"event">>,
		   attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}],
		   children = [Items]},
    #xmlel{name = <<"message">>, children = [Event]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Multicast

send_multiple(From, Server, Users, Packet) ->
    JIDs = [ User#user.jid || {_, User} <- ?DICT:to_list(Users)],
    ejabberd_router_multicast:route_multicast(From, Server, JIDs, Packet).

send_wrapped_multiple(From, Users, Packet, Node, State) ->
    lists:foreach(
      fun({_, #user{jid = To}}) ->
	      send_wrapped(From, To, Packet, Node, State)
      end, ?DICT:to_list(Users)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Detect messange stanzas that don't have meaninful content

has_body_or_subject(Packet) ->
    [] /= lists:dropwhile(fun
	(#xmlel{name = <<"body">>}) -> false;
	(#xmlel{name = <<"subject">>}) -> false;
	(_) -> true
    end, Packet#xmlel.children).
