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
	 start/3,
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

-record(config, {title = "",
		 allow_change_subj = true,
		 allow_query_users = true,
		 allow_private_messages = true,
		 public = true,
		 persistent = false,
		 moderated = false, % TODO
		 members_by_default = true,
		 members_only = false,
		 allow_user_invites = false,
		 password_protected = false, % TODO
		 anonymous = true,
		 logging = false % TODO
		}).

-record(user, {jid,
	       nick,
	       role,
	       last_presence}).

-record(state, {room,
		host,
		config = #config{},
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
-define(ERR_MUC_NOT_MEMBER,
	?OLD_ERROR("407", "Membership required to enter this room.")).


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

start(Host, Room, Opts) ->
    gen_fsm:start(?MODULE, [Host, Room, Opts], ?FSMOPTS).

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
    {ok, normal_state, State};
init([Host, Room, Opts]) ->
    State = set_opts(Opts, #state{host = Host,
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
		    case is_user_online(From, StateData) of
			true ->
			    NewState =
				add_user_presence_un(
				  From,
				  {xmlelement, "presence",
				   [{"type", "unavailable"}], []},
			      StateData),
			    send_new_presence(From, NewState),
			    {next_state, normal_state,
			     remove_online_user(From, NewState)};
			_ ->
			    {next_state, normal_state, StateData}
		    end;
		Type when (Type == "") or (Type == "normal") ->
		    case check_invitation(From, Els, StateData) of
			error ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_NOT_ALLOWED),
			    ejabberd_router:route(
			      {StateData#state.room, StateData#state.host, ""},
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
	{iq, ID, Type, XMLNS, SubEl} when
	      (XMLNS == ?NS_MUC_ADMIN) or
	      (XMLNS == ?NS_MUC_OWNER) or
	      (XMLNS == ?NS_DISCO_INFO) or
	      (XMLNS == ?NS_DISCO_ITEMS) ->
	    Res1 = case XMLNS of
		       ?NS_MUC_ADMIN ->
			   process_iq_admin(From, Type, SubEl, StateData);
		       ?NS_MUC_OWNER ->
			   process_iq_owner(From, Type, SubEl, StateData);
		       ?NS_DISCO_INFO ->
			   process_iq_disco_info(From, Type, StateData);
		       ?NS_DISCO_ITEMS ->
			   process_iq_disco_items(From, Type, StateData)
		   end,
	    {IQRes, NewStateData} =
		case Res1 of
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
	    "error" ->
		case is_user_online(From, StateData) of
		    true ->
			NewState =
			    add_user_presence_un(
			      From,
			      {xmlelement, "presence",
			       [{"type", "unavailable"}], []},
			      StateData),
			send_new_presence(From, NewState),
			remove_online_user(From, NewState);
		    _ ->
			StateData
		end;
	    "" ->
		case is_user_online(From, StateData) of
		    true ->
			case is_nick_change(From, Nick, StateData) of
			    true ->
				case is_nick_exists(Nick, StateData) of
				    true ->
					Err = jlib:make_error_reply(
						Packet,
						?ERR_MUC_NICK_CHANGE_CONFLICT),
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
			case is_nick_exists(Nick, StateData) or
			    not mod_muc:can_use_nick(From, Nick) of
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
						Packet,
						case Affiliation of
						    outcast ->
							?ERR_MUC_BANNED;
						    _ ->
							?ERR_MUC_NOT_MEMBER
						end),
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
    case xml:get_attr_s("type", Attrs) of
	"error" ->
	    case is_user_online(From, StateData) of
		true ->
		    NewState =
			add_user_presence_un(
			  From,
			  {xmlelement, "presence",
			   [{"type", "unavailable"}], []},
			  StateData),
		    send_new_presence(From, NewState),
		    {next_state, normal_state,
		     remove_online_user(From, NewState)};
		_ ->
		    {next_state, normal_state, StateData}
	    end;
	_ ->
	    case (StateData#state.config)#config.allow_private_messages
		andalso is_user_online(From, StateData) of
		true ->
		    case find_jid_by_nick(ToNick, StateData) of
			false ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_JID_NOT_FOUND),
			    ejabberd_router:route(
			      {StateData#state.room,
			       StateData#state.host,
			       ToNick},
			      From, Err);
			ToJID ->
			    {ok, #user{nick = FromNick}} =
				?DICT:find(jlib:jid_tolower(From),
					   StateData#state.users),
			    ejabberd_router:route(
			      {StateData#state.room,
			       StateData#state.host,
			       FromNick},
			      ToJID, Packet)
		    end,
		    {next_state, normal_state, StateData};
		_ ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_NOT_ALLOWED),
		    ejabberd_router:route(
		      {StateData#state.room,
		       StateData#state.host,
		       ToNick}, From, Err),
		    {next_state, normal_state, StateData}
	    end
    end;

normal_state({route, From, ToNick,
	      {xmlelement, "iq", Attrs, Els} = Packet},
	     StateData) ->
    case (StateData#state.config)#config.allow_query_users
	andalso is_user_online(From, StateData) of
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
handle_sync_event(get_disco_item, From, StateName, StateData) ->
    Reply = case (StateData#state.config)#config.public of
		true ->
		    {item, get_title(StateData)};
		_ ->
		    false
	    end,
    {reply, Reply, StateName, StateData};
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

get_affiliation(JID, StateData) ->
    case acl:match_rule(muc_admin, JID) of
	allow ->
	    owner;
	_ ->
	    LJID = jlib:jid_remove_resource(jlib:jid_tolower(JID)),
	    case ?DICT:find(LJID, StateData#state.affiliations) of
		{ok, Affiliation} ->
		    Affiliation;
		_ ->
		    none
	    end
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
      fun({LJID, Info}) ->
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
    case (StateData#state.config)#config.allow_change_subj of
	true ->
	    (Role == moderator) orelse (Role == participant);
	_ ->
	    Role == moderator
    end.

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
				     {JID, role, none, Reason} ->
					 catch send_kickban_presence(
						 JID, Reason, "307", SD),
					 set_role(JID, none, SD);
				     {JID, affiliation, outcast, Reason} ->
					 catch send_kickban_presence(
						 JID, Reason, "301", SD),
					 set_affiliation(
					   JID, outcast,
					   set_role(JID, none, SD));
				     {JID, affiliation, A, Reason} when
					   (A == admin) or (A == owner)->
					 SD1 = set_affiliation(JID, A, SD),
					 SD2 = set_role(JID, moderator, SD1),
					 send_update_presence(JID, SD2),
					 SD2;
				     {JID, affiliation, member, Reason} ->
					 SD1 = set_affiliation(
						 JID, member, SD),
					 SD2 = set_role(JID, participant, SD1),
					 send_update_presence(JID, SD2),
					 SD2;
				     {JID, role, R, Reason} ->
					 SD1 = set_role(JID, R, SD),
					 catch send_new_presence(JID, SD1),
					 SD1;
				     {JID, affiliation, A, Reason} ->
					 SD1 = set_affiliation(JID, A, SD),
					 send_update_presence(JID, SD1),
					 SD1
				       end
				) of
			      {'EXIT', ErrReason} ->
				  io:format("MUC ITEMS SET ERR: ~p~n",
					    [ErrReason]),
				  SD;
			      NSD ->
				  NSD
			  end
		  end, StateData, Res),
	    io:format("MUC SET: ~p~n", [Res]),
	    case (NSD#state.config)#config.persistent of
		true ->
		    mod_muc:store_room(NSD#state.room, make_opts(NSD));
		_ ->
		    ok
	    end,
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
		   [{xmlelement, "item", Attrs, Els} = Item | Items],
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
					      [{jlib:jid_remove_resource(JID),
						affiliation,
						SAffiliation,
						xml:get_path_s(
						  Item, [{elem, "reason"},
							 cdata])} | Res]);
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
			  send_kickban_presence1(J, Reason, Code, StateData)
		  end, LJIDs).

send_kickban_presence1(UJID, Reason, Code, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick}} =
	?DICT:find(jlib:jid_tolower(UJID), StateData#state.users),
    Affiliation = get_affiliation(UJID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    lists:foreach(
      fun({LJID, Info}) ->
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
		{StateData#state.room, StateData#state.host, Nick},
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff

process_iq_owner(From, set, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
	owner ->
	    {xmlelement, Name, Attrs, Els} = SubEl,
	    Lang = xml:get_tag_attr_s("xml:lang", SubEl),
	    case xml:remove_cdata(Els) of
		[{xmlelement, "x", Attrs1, Els1} = XEl] ->
		    case {xml:get_tag_attr_s("xmlns", XEl),
			  xml:get_tag_attr_s("type", XEl)} of
			{?NS_XDATA, "cancel"} ->
			    {result, [], StateData};
			{?NS_XDATA, "submit"} ->
			    set_config(XEl, StateData);
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		[{xmlelement, "destroy", Attrs1, Els1}] ->
		    destroy_room(Els1, StateData);
		Items ->
		    process_admin_items_set(From, Items, StateData)
	    end;
	_ ->
	    {error, ?ERR_FORBIDDEN}
    end;
%    {xmlelement, _, _, Items} = SubEl,
%    process_admin_items_set(From, Items, StateData);
%    {error, ?ERR_FEATURE_NOT_IMPLEMENTED};

process_iq_owner(From, get, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
	owner ->
	    {xmlelement, Name, Attrs, Els} = SubEl,
	    Lang = xml:get_tag_attr_s("xml:lang", SubEl),
	    case xml:remove_cdata(Els) of
		[] ->
		    get_config(Lang, StateData);
		[Item] ->
		    case xml:get_tag_attr("affiliation", Item) of
			false ->
			    {error, ?ERR_BAD_REQUEST};
			{value, StrAffiliation} ->
			    case catch list_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    {error, ?ERR_BAD_REQUEST};
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
	    {error, ?ERR_NOT_ALLOWED}
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


get_config(Lang, StateData) ->
    Config = StateData#state.config,
    Res =
	[?STRINGXFIELD("Room title",
		     "title",
		     Config#config.title),
	 ?BOOLXFIELD("Allow users to change subject?",
		     "allow_change_subj",
		     Config#config.allow_change_subj),
	 ?BOOLXFIELD("Allow users to query other users?",
		     "allow_query_users",
		     Config#config.allow_query_users),
	 ?BOOLXFIELD("Allow users to send private messages?",
		     "allow_private_messages",
		     Config#config.allow_private_messages),
	 ?BOOLXFIELD("Make room public searchable?",
		     "public",
		     Config#config.public),
	 ?BOOLXFIELD("Make room persistent?",
		     "persistent",
		     Config#config.persistent),
	 ?BOOLXFIELD("Make room moderated?",
		     "moderated",
		     Config#config.moderated),
	 ?BOOLXFIELD("Default users as members?",
		     "members_by_default",
		     Config#config.members_by_default),
	 ?BOOLXFIELD("Make room members only?",
		     "members_only",
		     Config#config.members_only),
	 ?BOOLXFIELD("Allow users to send invites?",
		     "allow_user_invites",
		     Config#config.allow_user_invites),
	 ?BOOLXFIELD("Make room password protected?",
		     "password_protected",
		     Config#config.password_protected),
	 ?BOOLXFIELD("Make room anonymous?",
		     "anonymous",
		     Config#config.anonymous),
	 ?BOOLXFIELD("Enable logging?",
		     "logging",
		     Config#config.logging)
	],
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}], Res}], StateData}.



set_config(XEl, StateData) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
	invalid ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    case set_xoption(XData, StateData#state.config) of
		#config{} = Config ->
		    change_config(Config, StateData);
		Err ->
		    Err
	    end
    end.

-define(SET_BOOL_XOPT(Opt, Val),
	case Val of
	    "0" -> set_xoption(Opts, Config#config{Opt = false});
	    "1" -> set_xoption(Opts, Config#config{Opt = true});
	    _ -> {error, ?ERR_BAD_REQUEST}
	end).

-define(SET_STRING_XOPT(Opt, Val),
	set_xoption(Opts, Config#config{Opt = Val})).



set_xoption([], Config) ->
    Config;
set_xoption([{"title", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(title, Val);
set_xoption([{"allow_change_subj", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_change_subj, Val);
set_xoption([{"allow_query_users", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_query_users, Val);
set_xoption([{"allow_private_messages", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_private_messages, Val);
set_xoption([{"public", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public, Val);
set_xoption([{"persistent", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(persistent, Val);
set_xoption([{"moderated", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(moderated, Val);
set_xoption([{"members_by_default", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_by_default, Val);
set_xoption([{"members_only", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_only, Val);
set_xoption([{"allow_user_invites", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_user_invites, Val);
set_xoption([{"password_protected", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(password_protected, Val);
set_xoption([{"anonymous", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(anonymous, Val);
set_xoption([{"logging", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(logging, Val);
set_xoption([_ | Opts], Config) ->
    {error, ?ERR_BAD_REQUEST}.


change_config(Config, StateData) ->
    NSD = StateData#state{config = Config},
    case {(StateData#state.config)#config.persistent,
	  Config#config.persistent} of
	{_, true} ->
	    mod_muc:store_room(NSD#state.room, make_opts(NSD));
	{true, false} ->
	    mod_muc:forget_room(NSD#state.room);
	{false, false} ->
	    ok
	end,
    {result, [], NSD}.


-define(CASE_CONFIG_OPT(Opt),
	Opt -> StateData#state{
		 config = (StateData#state.config)#config{Opt = Val}}).

set_opts([], StateData) ->
    StateData;
set_opts([{Opt, Val} | Opts], StateData) ->
    NSD = case Opt of
	      ?CASE_CONFIG_OPT(title);
	      ?CASE_CONFIG_OPT(allow_change_subj);
	      ?CASE_CONFIG_OPT(allow_query_users);
	      ?CASE_CONFIG_OPT(allow_private_messages);
	      ?CASE_CONFIG_OPT(public);
	      ?CASE_CONFIG_OPT(persistent);
	      ?CASE_CONFIG_OPT(moderated);
	      ?CASE_CONFIG_OPT(members_by_default);
	      ?CASE_CONFIG_OPT(members_only);
	      ?CASE_CONFIG_OPT(allow_user_invites);
	      ?CASE_CONFIG_OPT(password_protected);
	      ?CASE_CONFIG_OPT(anonymous);
	      ?CASE_CONFIG_OPT(logging);
	      affiliations ->
		  StateData#state{affiliations = ?DICT:from_list(Val)};
	      _ -> StateData
	  end,
    set_opts(Opts, NSD).

-define(MAKE_CONFIG_OPT(Opt), {Opt, Config#config.Opt}).

make_opts(StateData) ->
    Config = StateData#state.config,
    [
     ?MAKE_CONFIG_OPT(title),
     ?MAKE_CONFIG_OPT(allow_change_subj),
     ?MAKE_CONFIG_OPT(allow_query_users),
     ?MAKE_CONFIG_OPT(allow_private_messages),
     ?MAKE_CONFIG_OPT(public),
     ?MAKE_CONFIG_OPT(persistent),
     ?MAKE_CONFIG_OPT(moderated),
     ?MAKE_CONFIG_OPT(members_by_default),
     ?MAKE_CONFIG_OPT(members_only),
     ?MAKE_CONFIG_OPT(allow_user_invites),
     ?MAKE_CONFIG_OPT(password_protected),
     ?MAKE_CONFIG_OPT(anonymous),
     ?MAKE_CONFIG_OPT(logging),
     {affiliations, ?DICT:to_list(StateData#state.affiliations)}
    ].



destroy_room(DEls, StateData) ->
    lists:foreach(
      fun({LJID, Info}) ->
	      Nick = Info#user.nick,
	      ItemAttrs = [{"affiliation", "none"},
			   {"role", "none"}],
	      Packet = {xmlelement, "presence", [{"type", "unavailable"}],
			[{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			  [{xmlelement, "item", ItemAttrs, []},
			   {xmlelement, "destroy", [],
			    DEls}]}]},
	      ejabberd_router:route(
		{StateData#state.room, StateData#state.host, Nick},
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)),
    {result, [], StateData}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disco

process_iq_disco_info(From, set, StateData) ->
    {error, ?ERR_NOT_ALLOWED};

process_iq_disco_info(From, get, StateData) ->
    {result, [{xmlelement, "identity",
	       [{"category", "conference"},
		{"type", "text"},
		{"name", get_title(StateData)}], []},
	      {xmlelement, "feature",
	       [{"var", ?NS_MUC}], []}], StateData}.


process_iq_disco_items(From, set, StateData) ->
    {error, ?ERR_NOT_ALLOWED};

process_iq_disco_items(From, get, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    FRole = get_role(From, StateData),
    case (FRole /= none) or
	(FAffiliation == admin) or
	(FAffiliation == owner) of
	true ->
	    UList =
		lists:map(
		  fun({LJID, Info}) ->
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
	    {error, ?ERR_NOT_ALLOWED}
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

check_invitation(From, Els, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    CanInvite = (StateData#state.config)#config.allow_user_invites
	orelse (FAffiliation == admin) orelse (FAffiliation == owner),
    case xml:remove_cdata(Els) of
	[{xmlelement, "x", Attrs1, Els1} = XEl] ->
	    case xml:get_tag_attr_s("xmlns", XEl) of
		?NS_MUC_USER ->
		    case xml:remove_cdata(Els1) of
			[{xmlelement, "invite", Attrs2, Els2} = InviteEl] ->
			    case jlib:string_to_jid(
				   xml:get_attr_s("to", Attrs2)) of
				error ->
				    error;
				JID ->
				    case CanInvite of
					true ->
					    Reason =
						xml:get_path_s(
						  InviteEl,
						  [{elem, "reason"}, cdata]),
					    IEl =
						[{xmlelement, "invite",
						  [{"from",
						    jlib:jid_to_string(From)}],
						  [{xmlelement, "reason", [],
						    [{xmlcdata, Reason}]}]}],
					    PasswdEl = [],
					    Msg =
						{xmlelement, "message",
						 [{"type", "normal"}],
						 [{xmlelement, "x",
						   [{"xmlns", ?NS_MUC_USER}],
						   IEl ++ PasswdEl},
						  {xmlelement, "x",
						   [{"xmlns",
						     ?NS_XCONFERENCE},
						    {"jid",
						     jlib:jid_to_string(
						       {StateData#state.room,
							StateData#state.host,
							""})}],
						   [{xmlcdata, Reason}]}]},
					    ejabberd_router:route(
					      {StateData#state.room,
					       StateData#state.host,
					       ""},
					      JID,
					      Msg),
					    JID;
					_ ->
					    error
				    end
			    end;
			_ ->
			    error
		    end;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.




