%%%----------------------------------------------------------------------
%%% File    : mod_muc_room.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : MUC room stuff
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_muc_room).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_fsm).


%% External exports
-export([start/4,
	 init/1,
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

-define(SETS, gb_sets).
-define(DICT, dict).

-record(lqueue, {queue, len, max}).

-record(user, {jid,
	       nick,
	       role,
	       last_presence}).

-record(state, {room,
		host,
		config,
		users = ?DICT:new(),
		affiliations = ?DICT:new(),
		history = lqueue_new(10),
		subject = "",
		subject_author = ""}).


-define(OLD_ERROR(Code, Desc),
	{xmlelement, "error",
	 [{"code", Code}],
	 [{xmlcdata, Desc}]}).

-define(ERR_MUC_NICK_CONFLICT,
	?OLD_ERROR("409", "Please choose a different nickname.")).
-define(ERR_MUC_NICK_CHANGE_CONFLICT,
	?OLD_ERROR("409", "Nickname already in use.")).
-define(ERR_MUC_BANNED,
	?OLD_ERROR("403", "You have been banned from this room.")).


-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host, Room, Creator, Nick) ->
    gen_fsm:start(?MODULE, [Host, Room, Creator, Nick], ?FSMOPTS).

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
init([Host, Room, Creator, Nick]) ->
    LCreator = jlib:jid_tolower(Creator),
    State = set_affiliation(Creator, owner,
			    #state{host = Host,
				   room = Room}),
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
    case is_user_online(From, StateData) of
	true ->
	    case xml:get_attr_s("type", Attrs) of
		"groupchat" ->
		    {ok, #user{nick = FromNick, role = Role}} =
			?DICT:find(jlib:jid_tolower(From),
				   StateData#state.users),
		    if
			(Role == moderator) or (Role == participant) ->
			    {NewStateData1, IsAllowed} =
				case check_subject(Packet) of
				    false ->
					{StateData, true};
				    Subject ->
					case can_change_subject(Role,
								StateData) of
					    true ->
						{StateData#state{
						   subject = Subject,
						   subject_author = FromNick},
						 true};
					    _ ->
						{StateData, false}
					end
				end,
			    case IsAllowed of
				true ->
				    lists:foreach(
				      fun({LJID, Info}) ->
					      ejabberd_router:route(
						{StateData#state.room,
						 StateData#state.host,
						 FromNick},
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
				    Err = jlib:make_error_reply(
					    Packet, ?ERR_NOT_ALLOWED),
				    ejabberd_router:route(
				      {StateData#state.room,
				       StateData#state.host, ""},
				      From, Err),
			    {next_state, normal_state, StateData}
			    end;
			true ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_NOT_ALLOWED),
			    ejabberd_router:route(
			      {StateData#state.room, StateData#state.host, ""},
			      From, Err),
			    {next_state, normal_state, StateData}
		    end;
		"error" ->
		    {next_state, normal_state, StateData};
		_ ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_NOT_ALLOWED),
		    ejabberd_router:route(
		      {StateData#state.room, StateData#state.host, ""},
		      From, Err),
		    {next_state, normal_state, StateData}
	    end;
	_ ->
	    Err = jlib:make_error_reply(
		    Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(
	      {StateData#state.room, StateData#state.host, ""}, From, Err),
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, "",
	      {xmlelement, "iq", Attrs, Els} = Packet},
	     StateData) ->
    case jlib:iq_query_info(Packet) of
	{iq, ID, Type, ?NS_MUC_ADMIN = XMLNS, SubEl} ->
	    {IQRes, NewStateData} =
		case process_iq_admin(From, Type, SubEl, StateData) of
		    {result, Res, SD} ->
			{{iq, ID, result, XMLNS,
			  [{xmlelement, "query", [{"xmlns", XMLNS}],
			    Res
			   }]},
			 SD};
		    {error, Error} ->
			{{iq, ID, error, XMLNS,
			  [SubEl, Error]},
			 StateData}
		end,
	    ejabberd_router:route({StateData#state.room,
				   StateData#state.host,
				   ""},
				  From,
				  jlib:iq_to_xml(IQRes)),
	    {next_state, normal_state, NewStateData};
	_ ->
	    Err = jlib:make_error_reply(
		    Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
	    ejabberd_router:route(
	      {StateData#state.room, StateData#state.host, ""}, From, Err),
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, Nick,
	      {xmlelement, "presence", Attrs, Els} = Packet},
	     StateData) ->
    Type = xml:get_attr_s("type", Attrs),
    StateData1 =
	case Type of
	    "unavailable" ->
		case is_user_online(From, StateData) of
		    true ->
			NewState =
			    add_user_presence_un(From, Packet, StateData),
			send_new_presence(From, NewState),
			remove_online_user(From, NewState);
		    _ ->
			StateData
		end;
	    "error" -> % TODO
		StateData;
	    "" ->
		case is_user_online(From, StateData) of
		    true ->
			case is_nick_change(From, Nick, StateData) of
			    true ->
				case is_nick_exists(Nick, StateData) of
				    true ->
					Err = jlib:make_error_reply(
					Packet, ?ERR_MUC_NICK_CHANGE_CONFLICT),
					ejabberd_router:route(
					  {StateData#state.room,
					   StateData#state.host,
					   Nick}, % TODO: s/Nick/""/
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
			case is_nick_exists(Nick, StateData) of
			    true ->
				Err = jlib:make_error_reply(
					Packet, ?ERR_MUC_NICK_CONFLICT),
				ejabberd_router:route(
				  {StateData#state.room,
				   StateData#state.host,
				   Nick}, % TODO: s/Nick/""/
				  From, Err),
				StateData;
			    _ ->
				Affiliation =
				    get_affiliation(From, StateData),
				Role =
				    get_default_role(Affiliation, StateData),
				case Role of
				    none ->
					Err = jlib:make_error_reply(
					Packet, ?ERR_MUC_BANNED),
					ejabberd_router:route(
					  {StateData#state.room,
					   StateData#state.host,
					   Nick}, % TODO: s/Nick/""/
					  From, Err),
					StateData;
				    _ ->
					NewState =
					    add_user_presence(
					      From, Packet,
					      add_online_user(
						From, Nick, Role,
						StateData)),
					send_new_presence(From, NewState),
					send_existing_presences(
					  From, NewState),
					case send_history(From, NewState) of
					    true ->
						ok;
					    _ ->
						send_subject(From, StateData)
					end,
					send_join_messages_end(
					  From, StateData),
					NewState
				end
			end
		end;
	    _ ->
		StateData
	end,
    %io:format("STATE1: ~p~n", [?DICT:to_list(StateData#state.users)]),
    %io:format("STATE2: ~p~n", [?DICT:to_list(StateData1#state.users)]),
    {next_state, normal_state, StateData1};

normal_state({route, From, ToNick,
	      {xmlelement, "message", Attrs, Els} = Packet},
	     StateData) ->
    case is_user_online(From, StateData) of
	true ->
	    case find_jid_by_nick(ToNick, StateData) of
		false ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_JID_NOT_FOUND),
		    ejabberd_router:route(
		      {StateData#state.room, StateData#state.host, ToNick},
		      From, Err);
		ToJID ->
		    {ok, #user{nick = FromNick}} =
			?DICT:find(jlib:jid_tolower(From),
				   StateData#state.users),
		    ejabberd_router:route(
		      {StateData#state.room, StateData#state.host, FromNick},
		      ToJID, Packet)
	    end;
	_ ->
	    Err = jlib:make_error_reply(
		    Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(
	      {StateData#state.room, StateData#state.host, ToNick}, From, Err)
    end,
    {next_state, normal_state, StateData};

normal_state({route, From, ToNick,
	      {xmlelement, "iq", Attrs, Els} = Packet},
	     StateData) ->
    Err = jlib:make_error_reply(
	    Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
    ejabberd_router:route(
      {StateData#state.room, StateData#state.host, ToNick}, From, Err),
    {next_state, normal_state, StateData};

normal_state(Event, StateData) ->
    io:format("MUC: unknown event ~p~n", [Event]),
    {next_state, normal_state, StateData}.








%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
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
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    mod_muc:room_destroyed(StateData#state.room),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

route(Pid, From, ToNick, Packet) ->
    gen_fsm:send_event(Pid, {route, From, ToNick, Packet}).


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



set_affiliation(JID, Affiliation, StateData) ->
    LJID = jlib:jid_tolower(JID),
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

get_affiliation(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    case ?DICT:find(LJID, StateData#state.affiliations) of
	{ok, Affiliation} ->
	    Affiliation;
	_ ->
	    none
    end.

set_role(JID, Role, StateData) ->
    LJID = jlib:jid_tolower(JID),
    Users = case Role of
		none ->
		    ?DICT:erase(LJID,
				StateData#state.users);
		_ ->
		    {ok, User} = ?DICT:find(LJID, StateData#state.users),
		    ?DICT:store(LJID,
				User#user{role = Role},
				StateData#state.users)
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
	none ->    participant
    end.


add_online_user(JID, Nick, Role, StateData) ->
    LJID = jlib:jid_tolower(JID),
    Users = ?DICT:store(LJID,
			#user{jid = JID,
			      nick = Nick,
			      role = Role},
			StateData#state.users),
    StateData#state{users = Users}.

remove_online_user(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    Users = ?DICT:erase(LJID, StateData#state.users),
    StateData#state{users = Users}.


filter_presence({xmlelement, "presence", Attrs, Els}) ->
    FEls = lists:filter(
	     fun(El) ->
		     case El of
			 {xmlcdata, _} ->
			     false;
			 {xmlelement, Name1, Attrs1, Els1} ->
			     XMLNS = xml:get_attr_s("xmlns", Attrs),
			     case {Name1, XMLNS} of
				 {"show", ""} ->
				     true;
				 {"status", ""} ->
				     true;
				 _ ->
				     false
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
      fun({LJID, Info}) ->
	      ItemAttrs = case Info#user.role of
			      moderator ->
				  [{"jid", jlib:jid_to_string(RealJID)},
				   {"affiliation", SAffiliation},
				   {"role", SRole}];
			      _ ->
				  [{"affiliation", SAffiliation},
				   {"role", SRole}]
			  end,
	      Packet = append_subtags(
			 Presence,
			 [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			   [{xmlelement, "item", ItemAttrs, []}]}]),
	      ejabberd_router:route(
		{StateData#state.room, StateData#state.host, Nick},
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
		      ItemAttrs = case Role of
				      moderator ->
					  [{"jid",
					    jlib:jid_to_string(FromJID)},
					   {"affiliation",
					    affiliation_to_list(
					      FromAffiliation)},
					   {"role", role_to_list(FromRole)}];
				      _ ->
					  [{"affiliation", affiliation_to_list(
							     FromAffiliation)},
					   {"role", role_to_list(FromRole)}]
				  end,
		      Packet = append_subtags(
				 Presence,
				 [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
				   [{xmlelement, "item", ItemAttrs, []}]}]),
		      ejabberd_router:route(
			{StateData#state.room, StateData#state.host, FromNick},
			RealToJID,
			Packet)
	      end
      end, ?DICT:to_list(StateData#state.users)).


append_subtags({xmlelement, Name, Attrs, SubTags1}, SubTags2) ->
    {xmlelement, Name, Attrs, SubTags1 ++ SubTags2}.


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
      fun({LJID, Info}) ->
	      ItemAttrs1 = case Info#user.role of
			       moderator ->
				   [{"jid", jlib:jid_to_string(RealJID)},
				    {"affiliation", SAffiliation},
				    {"role", SRole},
				    {"nick", Nick}];
			       _ ->
				   [{"affiliation", SAffiliation},
				    {"role", SRole},
				    {"nick", Nick}]
			   end,
	      ItemAttrs2 = case Info#user.role of
			       moderator ->
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
		{StateData#state.room, StateData#state.host, OldNick},
		Info#user.jid,
		Packet1),
	      ejabberd_router:route(
		{StateData#state.room, StateData#state.host, Nick},
		Info#user.jid,
		Packet2)
      end, ?DICT:to_list(StateData#state.users)).


lqueue_new(Max) ->
    #lqueue{queue = queue:new(),
	    len = 0,
	    max = Max}.

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
    TSPacket = append_subtags(Packet,
			      [jlib:timestamp_to_xml(
				 calendar:now_to_universal_time(
				   now()))]),
    Q1 = lqueue_in({FromNick, TSPacket, HaveSubject}, StateData#state.history),
    StateData#state{history = Q1}.

send_history(JID, StateData) ->
    lists:foldl(
      fun({Nick, Packet, HaveSubject}, B) ->
	      ejabberd_router:route(
		{StateData#state.room, StateData#state.host, Nick},
		JID,
		Packet),
	      B or HaveSubject
      end, false, lqueue_to_list(StateData#state.history)).


send_subject(JID, StateData) ->
    case StateData#state.subject_author of
	"" ->
	    ok;
	Nick ->
	    Subject = StateData#state.subject,
	    Packet = {xmlelement, "message", [{"type", "groupchat"}],
		      [{xmlelement, "subject", [], [{xmlcdata, Subject}]},
		       {xmlelement, "body", [],
			[{xmlcdata,
			  Nick ++ " has set the topic to: " ++ Subject}]}]},
	    ejabberd_router:route(
	      {StateData#state.room, StateData#state.host, ""},
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

send_join_messages_end(JID, StateData) ->
    Packet = {xmlelement, "message", [{"type", "groupchat"}],
	      [{xmlelement, "body", [],
		[{xmlcdata, "-"}]}]},
    ejabberd_router:route(
      {StateData#state.room, StateData#state.host, ""},
      JID,
      Packet).

can_change_subject(Role, StateData) ->
    (Role == moderator) orelse (Role == participant).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Admin stuff

process_iq_admin(From, set, SubEl, StateData) ->
    {xmlelement, _, _, Items} = SubEl,
    process_admin_items_set(From, Items, StateData);

process_iq_admin(From, get, SubEl, StateData) ->
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
					    {error, ?ERR_NOT_ALLOWED}
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
				    {error, ?ERR_NOT_ALLOWED}
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
      fun({JID, Affiliation}) ->
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
	      Affiliation == A
      end, ?DICT:to_list(StateData#state.affiliations)).


process_admin_items_set(UJID, Items, StateData) ->
    UAffiliation = get_affiliation(UJID, StateData),
    URole = get_role(UJID, StateData),
    case find_changed_items(UJID, UAffiliation, URole, Items, StateData, []) of
	{result, Res} ->
	    NSD =
		lists:foldl(
		  fun(E, SD) ->
			  case catch (
				 case E of
				     {JID, role, none} ->
					 catch send_kickban_presence(
						 JID, "307", SD),
					 set_role(JID, none, SD);
				     {JID, affiliation, outcast} ->
					 catch send_kickban_presence(
						 JID, "301", SD),
					 set_affiliation(
					   JID, outcast,
					   set_role(JID, none, SD));
				     {JID, role, R} ->
					 SD1 = set_role(JID, R, SD),
					 catch send_new_presence(JID, SD1),
					 SD1;
				     {JID, affiliation, A} ->
					 SD1 = set_affiliation(JID, A, SD),
					 catch send_new_presence(JID, SD1),
					 SD1
				       end
				) of
			      {'EXIT', Reason} ->
				  io:format("MUC ITEMS SET ERR: ~p~n",
					    [Reason]),
				  SD;
			      NSD ->
				  NSD
			  end
		  end, StateData, Res),
	    io:format("MUC SET: ~p~n", [Res]),
	    {result, [], NSD};
	Err ->
	    Err
    end.

    
find_changed_items(UJID, UAffiliation, URole, [], StateData, Res) ->
    {result, Res};
find_changed_items(UJID, UAffiliation, URole, [{xmlcdata, _} | Items],
		   StateData, Res) ->
    find_changed_items(UJID, UAffiliation, URole, Items, StateData, Res);
find_changed_items(UJID, UAffiliation, URole,
		   [{xmlelement, "item", Attrs, Els} | Items],
		   StateData, Res) ->
    TJID = case xml:get_attr("jid", Attrs) of
	       {value, S} ->
		   case jlib:string_to_jid(S) of
		       error ->
			   {error, ?ERR_BAD_REQUEST};
		       J ->
			   {value, J}
		   end;
	       _ ->
		   case xml:get_attr("nick", Attrs) of
		       {value, N} ->
			   case find_jid_by_nick(N, StateData) of
			       false ->
				   {error, ?ERR_NOT_ALLOWED};
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
				    {error, ?ERR_BAD_REQUEST};
				SAffiliation ->
				    case can_change_ra(
					   UAffiliation, URole,
					   TAffiliation, TRole,
					   affiliation, SAffiliation) of
					nothing ->
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, StateData,
					      Res);
					true ->
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, StateData,
					      [{JID,
						affiliation,
						SAffiliation} | Res]);
					_ ->
					    {error, ?ERR_NOT_ALLOWED}
				    end
			    end
		    end;
		{value, StrRole} ->
		    case catch list_to_role(StrRole) of
			{'EXIT', _} ->
			    {error, ?ERR_BAD_REQUEST};
			SRole ->
			    case can_change_ra(
				   UAffiliation, URole,
				   TAffiliation, TRole,
				   role, SRole) of
				nothing ->
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, StateData,
				      Res);
				true ->
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, StateData,
				      [{JID, role, SRole} | Res]);
				_ ->
				    {error, ?ERR_NOT_ALLOWED}
			    end
		    end
	    end;
	Err ->
	    Err
    end;
find_changed_items(UJID, UAffiliation, URole, Items, StateData, Res) ->
    {error, ?ERR_BAD_REQUEST}.






can_change_ra(FAffiliation, FRole,
	      TAffiliation, TRole,
	      affiliation, Value)
  when (TAffiliation == Value) ->
    nothing;
can_change_ra(FAffiliation, FRole,
	      TAffiliation, TRole,
	      role, Value)
  when (TRole == Value) ->
    nothing;
can_change_ra(FAffiliation, FRole,
	      outcast, TRole,
	      affiliation, none)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, FRole,
	      none, TRole,
	      affiliation, outcast)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, FRole,
	      none, TRole,
	      affiliation, member)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, FRole,
	      none, TRole,
	      affiliation, admin) ->
    true;
can_change_ra(owner, FRole,
	      none, TRole,
	      affiliation, owner) ->
    true;
can_change_ra(FAffiliation, FRole,
	      member, TRole,
	      affiliation, outcast)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, FRole,
	      member, TRole,
	      affiliation, none)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, FRole,
	      member, TRole,
	      affiliation, admin) ->
    true;
can_change_ra(owner, FRole,
	      member, TRole,
	      affiliation, owner) ->
    true;
can_change_ra(owner, FRole,
	      admin, TRole,
	      affiliation, member) ->
    true;
can_change_ra(owner, FRole,
	      admin, TRole,
	      affiliation, owner) ->
    true;
can_change_ra(owner, FRole,
	      owner, TRole,
	      affiliation, admin) ->
    true;
can_change_ra(FAffiliation, FRole,
	      TAffiliation, TRole,
	      affiliation, Value) ->
    false;
can_change_ra(FAffiliation, moderator,
	      TAffiliation, visitor,
	      role, none) ->
    true;
can_change_ra(FAffiliation, moderator,
	      TAffiliation, visitor,
	      role, participant) ->
    true;
can_change_ra(FAffiliation, FRole,
	      TAffiliation, visitor,
	      role, moderator)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, moderator,
	      TAffiliation, participant,
	      role, none) ->
    true;
can_change_ra(FAffiliation, moderator,
	      TAffiliation, participant,
	      role, visitor) ->
    true;
can_change_ra(FAffiliation, FRole,
	      TAffiliation, participant,
	      role, moderator)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, FRole,
	      TAffiliation, moderator,
	      role, participant)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, FRole,
	      TAffiliation, TRole,
	      role, Value) ->
    false.


send_kickban_presence(UJID, Code, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick}} =
	?DICT:find(jlib:jid_tolower(UJID), StateData#state.users),
    Affiliation = get_affiliation(UJID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    lists:foreach(
      fun({LJID, Info}) ->
	      ItemAttrs = [{"affiliation", SAffiliation},
			   {"role", "none"}],
	      Packet = {xmlelement, "presence", [{"type", "unavailable"}],
			[{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			  [{xmlelement, "item", ItemAttrs, []},
			   {xmlelement, "status", [{"code", Code}], []}]}]},
	      ejabberd_router:route(
		{StateData#state.room, StateData#state.host, Nick},
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).




