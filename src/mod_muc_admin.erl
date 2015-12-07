%%%----------------------------------------------------------------------
%%% File    : mod_muc_admin.erl
%%% Author  : Badlop <badlop@ono.com>
%%% Purpose : Tools for additional MUC administration
%%% Created : 8 Sep 2007 by Badlop <badlop@ono.com>
%%% Id      : $Id: mod_muc_admin.erl 1133 2012-10-17 22:13:06Z badlop $
%%%----------------------------------------------------------------------

-module(mod_muc_admin).
-author('badlop@ono.com').

-behaviour(gen_mod).

-export([start/2, stop/1, muc_online_rooms/1,
	 muc_unregister_nick/1, create_room/3, destroy_room/2,
	 create_rooms_file/1, destroy_rooms_file/1,
	 rooms_unused_list/2, rooms_unused_destroy/2,
	 get_user_rooms/2, get_room_occupants/2,
	 get_room_occupants_number/2, send_direct_invitation/5,
	 change_room_option/4, get_room_options/2,
	 set_room_affiliation/4, get_room_affiliations/2,
	 web_menu_main/2, web_page_main/2, web_menu_host/3,
	 web_page_host/3, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("ejabberd_commands.hrl").

%% Copied from mod_muc/mod_muc.erl
-record(muc_online_room, {name_host, pid}).

%%----------------------------
%% gen_mod
%%----------------------------

start(Host, _Opts) ->
    ejabberd_commands:register_commands(commands()),
    ejabberd_hooks:add(webadmin_menu_main, ?MODULE, web_menu_main, 50),
    ejabberd_hooks:add(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:add(webadmin_page_main, ?MODULE, web_page_main, 50),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE, web_page_host, 50).

stop(Host) ->
    ejabberd_commands:unregister_commands(commands()),
    ejabberd_hooks:delete(webadmin_menu_main, ?MODULE, web_menu_main, 50),
    ejabberd_hooks:delete(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:delete(webadmin_page_main, ?MODULE, web_page_main, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host, ?MODULE, web_page_host, 50).

%%%
%%% Register commands
%%%

commands() ->
    [
     #ejabberd_commands{name = muc_online_rooms, tags = [muc],
		       desc = "List existing rooms ('global' to get all vhosts)",
                       policy = admin,
		       module = ?MODULE, function = muc_online_rooms,
		       args = [{host, binary}],
		       result = {rooms, {list, {room, string}}}},
     #ejabberd_commands{name = muc_unregister_nick, tags = [muc],
		       desc = "Unregister the nick in the MUC service",
		       module = ?MODULE, function = muc_unregister_nick,
		       args = [{nick, binary}],
		       result = {res, rescode}},

     #ejabberd_commands{name = create_room, tags = [muc_room],
		       desc = "Create a MUC room name@service in host",
		       module = ?MODULE, function = create_room,
		       args = [{name, binary}, {service, binary},
			       {host, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = destroy_room, tags = [muc_room],
		       desc = "Destroy a MUC room",
		       module = ?MODULE, function = destroy_room,
		       args = [{name, binary}, {service, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = create_rooms_file, tags = [muc],
		       desc = "Create the rooms indicated in file",
		       longdesc = "Provide one room JID per line. Rooms will be created after restart.",
		       module = ?MODULE, function = create_rooms_file,
		       args = [{file, string}],
		       result = {res, rescode}},
     #ejabberd_commands{name = destroy_rooms_file, tags = [muc],
		       desc = "Destroy the rooms indicated in file",
		       longdesc = "Provide one room JID per line.",
		       module = ?MODULE, function = destroy_rooms_file,
		       args = [{file, string}],
		       result = {res, rescode}},
     #ejabberd_commands{name = rooms_unused_list, tags = [muc],
		       desc = "List the rooms that are unused for many days in host",
		       module = ?MODULE, function = rooms_unused_list,
		       args = [{host, binary}, {days, integer}],
		       result = {rooms, {list, {room, string}}}},
     #ejabberd_commands{name = rooms_unused_destroy, tags = [muc],
		       desc = "Destroy the rooms that are unused for many days in host",
		       module = ?MODULE, function = rooms_unused_destroy,
		       args = [{host, binary}, {days, integer}],
		       result = {rooms, {list, {room, string}}}},

     #ejabberd_commands{name = get_user_rooms, tags = [muc],
			desc = "Get the list of rooms where this user is occupant",
			module = ?MODULE, function = get_user_rooms,
			args = [{user, binary}, {host, binary}],
		        result = {rooms, {list, {room, string}}}},

     #ejabberd_commands{name = get_room_occupants, tags = [muc_room],
			desc = "Get the list of occupants of a MUC room",
			module = ?MODULE, function = get_room_occupants,
			args = [{name, binary}, {service, binary}],
			result = {occupants, {list,
					      {occupant, {tuple,
							  [{jid, string},
							   {nick, string},
							   {role, string}
							  ]}}
					     }}},

     #ejabberd_commands{name = get_room_occupants_number, tags = [muc_room],
			desc = "Get the number of occupants of a MUC room",
			module = ?MODULE, function = get_room_occupants_number,
			args = [{name, binary}, {service, binary}],
			result = {occupants, integer}},

     #ejabberd_commands{name = send_direct_invitation, tags = [muc_room],
			desc = "Send a direct invitation to several destinations",
			longdesc = "Password and Message can also be: none. Users JIDs are separated with : ",
			module = ?MODULE, function = send_direct_invitation,
		        args = [{name, binary}, {service, binary}, {password, binary}, {reason, binary}, {users, binary}],
		        result = {res, rescode}},

     #ejabberd_commands{name = change_room_option, tags = [muc_room],
		       desc = "Change an option in a MUC room",
		       module = ?MODULE, function = change_room_option,
		       args = [{name, binary}, {service, binary},
			       {option, binary}, {value, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = get_room_options, tags = [muc_room],
		        desc = "Get options from a MUC room",
		        module = ?MODULE, function = get_room_options,
		        args = [{name, binary}, {service, binary}],
			result = {options, {list,
						 {option, {tuple,
								[{name, string},
								 {value, string}
								]}}
						}}},

     #ejabberd_commands{name = set_room_affiliation, tags = [muc_room],
		       desc = "Change an affiliation in a MUC room",
		       module = ?MODULE, function = set_room_affiliation,
		       args = [{name, binary}, {service, binary},
			       {jid, binary}, {affiliation, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = get_room_affiliations, tags = [muc_room],
			desc = "Get the list of affiliations of a MUC room",
			module = ?MODULE, function = get_room_affiliations,
			args = [{name, binary}, {service, binary}],
			result = {affiliations, {list,
						 {affiliation, {tuple,
								[{username, string},
								 {domain, string},
								 {affiliation, atom},
								 {reason, string}
								]}}
						}}}
    ].


%%%
%%% ejabberd commands
%%%

muc_online_rooms(ServerHost) ->
    MUCHost = find_host(ServerHost),
    Rooms = ets:tab2list(muc_online_room),
    lists:foldl(
      fun({_, {Roomname, Host}, _}, Results) ->
	      case MUCHost of
		  global ->
		      [<<Roomname/binary, "@", Host/binary>> | Results];
		  Host ->
		      [<<Roomname/binary, "@", Host/binary>> | Results];
		  _ ->
		      Results
	      end
      end,
      [],
      Rooms).

muc_unregister_nick(Nick) ->
    F2 = fun(N) ->
		 [{_,Key,_}] = mnesia:index_read(muc_registered, N, 3),
		 mnesia:delete({muc_registered, Key})
	 end,
    case mnesia:transaction(F2, [Nick], 1) of
	{atomic, ok} ->
	    ok;
	{aborted, _Error} ->
	    error
    end.

get_user_rooms(LUser, LServer) ->
    US = {LUser, LServer},
    case catch ets:select(muc_online_users,
                          [{#muc_online_users{us = US, room='$1', host='$2', _ = '_'}, [], [{{'$1', '$2'}}]}])
        of
      Res when is_list(Res) ->
	[<<R/binary, "@", H/binary>> || {R, H} <- Res];
      _ -> []
    end.

%%----------------------------
%% Ad-hoc commands
%%----------------------------


%%----------------------------
%% Web Admin
%%----------------------------

%%---------------
%% Web Admin Menu

web_menu_main(Acc, Lang) ->
    Acc ++ [{<<"muc">>, ?T(<<"Multi-User Chat">>)}].

web_menu_host(Acc, _Host, Lang) ->
    Acc ++ [{<<"muc">>, ?T(<<"Multi-User Chat">>)}].


%%---------------
%% Web Admin Page

-define(TDTD(L, N),
	?XE(<<"tr">>, [?XCT(<<"td">>, L),
		       ?XC(<<"td">>, jlib:integer_to_binary(N))
		      ])).

web_page_main(_, #request{path=[<<"muc">>], lang = Lang} = _Request) ->
    Res = [?XCT(<<"h1">>, <<"Multi-User Chat">>),
	   ?XCT(<<"h3">>, <<"Statistics">>),
	   ?XAE(<<"table">>, [],
		[?XE(<<"tbody">>, [?TDTD(<<"Total rooms">>, ets:info(muc_online_room, size)),
				   ?TDTD(<<"Permanent rooms">>, mnesia:table_info(muc_room, size)),
				   ?TDTD(<<"Registered nicknames">>, mnesia:table_info(muc_registered, size))
				  ])
		]),
	   ?XE(<<"ul">>, [?LI([?ACT(<<"rooms">>, <<"List of rooms">>)])])
	  ],
    {stop, Res};

web_page_main(_, #request{path=[<<"muc">>, <<"rooms">>], q = Q, lang = Lang} = _Request) ->
    Sort_query = get_sort_query(Q),
    Res = make_rooms_page(global, Lang, Sort_query),
    {stop, Res};

web_page_main(Acc, _) -> Acc.

web_page_host(_, Host,
	      #request{path = [<<"muc">>],
		       q = Q,
		       lang = Lang} = _Request) ->
    Sort_query = get_sort_query(Q),
    Res = make_rooms_page(find_host(Host), Lang, Sort_query),
    {stop, Res};
web_page_host(Acc, _, _) -> Acc.


%% Returns: {normal | reverse, Integer}
get_sort_query(Q) ->
    case catch get_sort_query2(Q) of
	{ok, Res} -> Res;
	_ -> {normal, 1}
    end.

get_sort_query2(Q) ->
    {value, {_, String}} = lists:keysearch(<<"sort">>, 1, Q),
    Integer = list_to_integer(binary_to_list(String)),
    case Integer >= 0 of
	true -> {ok, {normal, Integer}};
	false -> {ok, {reverse, abs(Integer)}}
    end.

make_rooms_page(Host, Lang, {Sort_direction, Sort_column}) ->
    Rooms_names = get_rooms(Host),
    Rooms_infos = build_info_rooms(Rooms_names),
    Rooms_sorted = sort_rooms(Sort_direction, Sort_column, Rooms_infos),
    Rooms_prepared = prepare_rooms_infos(Rooms_sorted),
    TList = lists:map(
	      fun(Room) ->
		      ?XE(<<"tr">>, [?XC(<<"td">>, E) || E <- Room])
	      end, Rooms_prepared),
    Titles = [<<"Jabber ID">>,
	      <<"# participants">>,
	      <<"Last message">>,
	      <<"Public">>,
	      <<"Persistent">>,
	      <<"Logging">>,
	      <<"Just created">>,
	      <<"Room title">>],
    {Titles_TR, _} =
	lists:mapfoldl(
	  fun(Title, Num_column) ->
		  NCS = jlib:integer_to_binary(Num_column),
		  TD = ?XE(<<"td">>, [?CT(Title),
				      ?C(<<" ">>),
				      ?AC(<<"?sort=", NCS/binary>>, <<"<">>),
				      ?C(<<" ">>),
				      ?AC(<<"?sort=-", NCS/binary>>, <<">">>)]),
		  {TD, Num_column+1}
	  end,
	  1,
	  Titles),
    [?XCT(<<"h1">>, <<"Multi-User Chat">>),
     ?XCT(<<"h2">>, <<"Chatrooms">>),
     ?XE(<<"table">>,
	 [?XE(<<"thead">>,
	      [?XE(<<"tr">>, Titles_TR)]
	     ),
	  ?XE(<<"tbody">>, TList)
	 ]
	)
    ].

sort_rooms(Direction, Column, Rooms) ->
    Rooms2 = lists:keysort(Column, Rooms),
    case Direction of
	normal -> Rooms2;
	reverse -> lists:reverse(Rooms2)
    end.

build_info_rooms(Rooms) ->
    [build_info_room(Room) || Room <- Rooms].

build_info_room({Name, Host, Pid}) ->
    C = get_room_config(Pid),
    Title = C#config.title,
    Public = C#config.public,
    Persistent = C#config.persistent,
    Logging = C#config.logging,

    S = get_room_state(Pid),
    Just_created = S#state.just_created,
    Num_participants = length(dict:fetch_keys(S#state.users)),

    History = (S#state.history)#lqueue.queue,
    Ts_last_message =
	case queue:is_empty(History) of
	    true ->
		<<"A long time ago">>;
	    false ->
		Last_message1 = queue:last(History),
		{_, _, _, Ts_last, _} = Last_message1,
		jlib:timestamp_to_iso(Ts_last)
	end,

    {<<Name/binary, "@", Host/binary>>,
     Num_participants,
     Ts_last_message,
     Public,
     Persistent,
     Logging,
     Just_created,
     Title}.

prepare_rooms_infos(Rooms) ->
    [prepare_room_info(Room) || Room <- Rooms].
prepare_room_info(Room_info) ->
    {NameHost,
     Num_participants,
     Ts_last_message,
     Public,
     Persistent,
     Logging,
     Just_created,
     Title} = Room_info,
    [NameHost,
     jlib:integer_to_binary(Num_participants),
     Ts_last_message,
     jlib:atom_to_binary(Public),
     jlib:atom_to_binary(Persistent),
     jlib:atom_to_binary(Logging),
     jlib:atom_to_binary(Just_created),
     Title].


%%----------------------------
%% Create/Delete Room
%%----------------------------

%% @spec (Name::binary(), Host::binary(), ServerHost::binary()) ->
%%       ok | error
%% @doc Create a room immediately with the default options.
create_room(Name, Host, ServerHost) ->

    %% Get the default room options from the muc configuration
    DefRoomOpts = gen_mod:get_module_opt(ServerHost, mod_muc,
					 default_room_options, fun(X) -> X end, []),

    %% Store the room on the server, it is not started yet though at this point
    mod_muc:store_room(ServerHost, Host, Name, DefRoomOpts),

    %% Get all remaining mod_muc parameters that might be utilized
    Access = gen_mod:get_module_opt(ServerHost, mod_muc, access, fun(X) -> X end, all),
    AcCreate = gen_mod:get_module_opt(ServerHost, mod_muc, access_create, fun(X) -> X end, all),
    AcAdmin = gen_mod:get_module_opt(ServerHost, mod_muc, access_admin, fun(X) -> X end, none),
    AcPer = gen_mod:get_module_opt(ServerHost, mod_muc, access_persistent, fun(X) -> X end, all),
    HistorySize = gen_mod:get_module_opt(ServerHost, mod_muc, history_size, fun(X) -> X end, 20),
    RoomShaper = gen_mod:get_module_opt(ServerHost, mod_muc, room_shaper, fun(X) -> X end, none),

    %% If the room does not exist yet in the muc_online_room
    case mnesia:dirty_read(muc_online_room, {Name, Host}) of
        [] ->
	    %% Start the room
	    {ok, Pid} = mod_muc_room:start(
			  Host,
			  ServerHost,
			  {Access, AcCreate, AcAdmin, AcPer},
			  Name,
			  HistorySize,
			  RoomShaper,
			  DefRoomOpts),
	    {atomic, ok} = register_room(Host, Name, Pid),
	    ok;
	_ ->
	    error
    end.

register_room(Host, Name, Pid) ->
    F = fun() ->
		mnesia:write(#muc_online_room{name_host = {Name, Host},
					      pid = Pid})
	end,
    mnesia:transaction(F).

%% Create the room only in the database.
%% It is required to restart the MUC service for the room to appear.
muc_create_room(ServerHost, {Name, Host, _}, DefRoomOpts) ->
    io:format("Creating room ~s@~s~n", [Name, Host]),
    mod_muc:store_room(ServerHost, Host, Name, DefRoomOpts).

%% @spec (Name::binary(), Host::binary()) ->
%%       ok | {error, room_not_exists}
%% @doc Destroy the room immediately.
%% If the room has participants, they are not notified that the room was destroyed;
%% they will notice when they try to chat and receive an error that the room doesn't exist.
destroy_room(Name, Service) ->
    case mnesia:dirty_read(muc_online_room, {Name, Service}) of
	[R] ->
	    Pid = R#muc_online_room.pid,
	    gen_fsm:send_all_state_event(Pid, destroy),
	    ok;
	[] ->
	    error
    end.

destroy_room({N, H, SH}) ->
    io:format("Destroying room: ~s@~s - vhost: ~s~n", [N, H, SH]),
    destroy_room(N, H).


%%----------------------------
%% Destroy Rooms in File
%%----------------------------

%% The format of the file is: one chatroom JID per line
%% The file encoding must be UTF-8

destroy_rooms_file(Filename) ->
    {ok, F} = file:open(Filename, [read]),
    RJID = read_room(F),
    Rooms = read_rooms(F, RJID, []),
    file:close(F),
    [destroy_room(A) || A <- Rooms],
	ok.

read_rooms(_F, eof, L) ->
    L;

read_rooms(F, RJID, L) ->
    RJID2 = read_room(F),
    read_rooms(F, RJID2, [RJID | L]).

read_room(F) ->
    case io:get_line(F, "") of
	eof -> eof;
	String ->
	    case io_lib:fread("~s", String) of
		{ok, [RoomJID], _} -> split_roomjid(RoomJID);
		{error, What} ->
		    io:format("Parse error: what: ~p~non the line: ~p~n~n", [What, String])
	    end
    end.

%% This function is quite rudimentary
%% and may not be accurate
split_roomjid(RoomJID) ->
    [Name, Host] = string:tokens(RoomJID, "@"),
    [_MUC_service_name | ServerHostList] = string:tokens(Host, "."),
    ServerHost = join(ServerHostList, "."),
    {list_to_binary(Name), list_to_binary(Host), list_to_binary(ServerHost)}.

%% This function is copied from string:join/2 in Erlang/OTP R12B-1
%% Note that string:join/2 is not implemented in Erlang/OTP R11B
join([H|T], Sep) ->
    H ++ lists:concat([Sep ++ X || X <- T]).


%%----------------------------
%% Create Rooms in File
%%----------------------------

create_rooms_file(Filename) ->
    {ok, F} = file:open(Filename, [read]),
    RJID = read_room(F),
    Rooms = read_rooms(F, RJID, []),
    file:close(F),
    %% Read the default room options defined for the first virtual host
    DefRoomOpts = gen_mod:get_module_opt(?MYNAME, mod_muc,
					 default_room_options,
					 fun(L) when is_list(L) -> L end, []),
    [muc_create_room(?MYNAME, A, DefRoomOpts) || A <- Rooms],
	ok.


%%----------------------------
%% List/Delete Unused Rooms
%%----------------------------

%%---------------
%% Control

rooms_unused_list(Host, Days) ->
    rooms_unused_report(list, Host, Days).
rooms_unused_destroy(Host, Days) ->
    rooms_unused_report(destroy, Host, Days).

rooms_unused_report(Action, Host, Days) ->
    {NA, NP, RP} = muc_unused(Action, Host, Days),
    io:format("Unused rooms: ~p out of ~p~n", [NP, NA]),
    [<<R/binary, "@", H/binary>> || {R, H, _P} <- RP].

muc_unused(Action, ServerHost, Days) ->
    Host = find_host(ServerHost),
    muc_unused2(Action, ServerHost, Host, Days).

muc_unused2(Action, ServerHost, Host, Last_allowed) ->
    %% Get all required info about all existing rooms
    Rooms_all = get_rooms(Host),

    %% Decide which ones pass the requirements
    Rooms_pass = decide_rooms(Rooms_all, Last_allowed),

    Num_rooms_all = length(Rooms_all),
    Num_rooms_pass = length(Rooms_pass),

    %% Perform the desired action for matching rooms
    act_on_rooms(Action, Rooms_pass, ServerHost),

    {Num_rooms_all, Num_rooms_pass, Rooms_pass}.

%%---------------
%% Get info

get_rooms(Host) ->
    Get_room_names = fun(Room_reg, Names) ->
			     Pid = Room_reg#muc_online_room.pid,
			     case {Host, Room_reg#muc_online_room.name_host} of
				 {Host, {Name1, Host}} ->
				     [{Name1, Host, Pid} | Names];
				 {global, {Name1, Host1}} ->
				     [{Name1, Host1, Pid} | Names];
				 _ ->
				     Names
			     end
		     end,
    ets:foldr(Get_room_names, [], muc_online_room).

get_room_config(Room_pid) ->
    {ok, R} = gen_fsm:sync_send_all_state_event(Room_pid, get_config),
    R.

get_room_state(Room_pid) ->
    {ok, R} = gen_fsm:sync_send_all_state_event(Room_pid, get_state),
    R.

%%---------------
%% Decide

decide_rooms(Rooms, Last_allowed) ->
    Decide = fun(R) -> decide_room(R, Last_allowed) end,
    lists:filter(Decide, Rooms).

decide_room({_Room_name, _Host, Room_pid}, Last_allowed) ->
    C = get_room_config(Room_pid),
    Persistent = C#config.persistent,

    S = get_room_state(Room_pid),
    Just_created = S#state.just_created,

    Room_users = S#state.users,
    Num_users = length(?DICT:to_list(Room_users)),

    History = (S#state.history)#lqueue.queue,
    Ts_now = calendar:universal_time(),
    Ts_uptime = uptime_seconds(),
    {Has_hist, Last} = case queue:is_empty(History) of
			   true ->
			       {false, Ts_uptime};
			   false ->
			       Last_message = queue:last(History),
			       {_, _, _, Ts_last, _} = Last_message,
			       Ts_diff =
				   calendar:datetime_to_gregorian_seconds(Ts_now)
				   - calendar:datetime_to_gregorian_seconds(Ts_last),
			       {true, Ts_diff}
		       end,

    case {Persistent, Just_created, Num_users, Has_hist, seconds_to_days(Last)} of
	{_true, false, 0, _, Last_days}
	when Last_days >= Last_allowed ->
	    true;
	_ ->
	    false
    end.

seconds_to_days(S) ->
    S div (60*60*24).

%%---------------
%% Act

act_on_rooms(Action, Rooms, ServerHost) ->
    ServerHosts = [ {A, find_host(A)} || A <- ?MYHOSTS ],
    Delete = fun({_N, H, _Pid} = Room) ->
		     SH = case ServerHost of
			      global -> find_serverhost(H, ServerHosts);
			      O -> O
			  end,

		     act_on_room(Action, Room, SH)
	     end,
    lists:foreach(Delete, Rooms).

find_serverhost(Host, ServerHosts) ->
    {value, {ServerHost, Host}} = lists:keysearch(Host, 2, ServerHosts),
    ServerHost.

act_on_room(destroy, {N, H, Pid}, SH) ->
    gen_fsm:send_all_state_event(
      Pid, {destroy, <<"Room destroyed by rooms_unused_destroy.">>}),
    mod_muc:room_destroyed(H, N, Pid, SH),
    mod_muc:forget_room(SH, H, N);

act_on_room(list, _, _) ->
    ok.


%%----------------------------
%% Change Room Option
%%----------------------------

get_room_occupants(Room, Host) ->
    case get_room_pid(Room, Host) of
	room_not_found -> throw({error, room_not_found});
	Pid -> get_room_occupants(Pid)
    end.

get_room_occupants(Pid) ->
    S = get_room_state(Pid),
    lists:map(
      fun({_LJID, Info}) ->
	      {jid:to_string(Info#user.jid),
	       Info#user.nick,
	       atom_to_list(Info#user.role)}
      end,
      dict:to_list(S#state.users)).

get_room_occupants_number(Room, Host) ->
    length(get_room_occupants(Room, Host)).

%%----------------------------
%% Send Direct Invitation
%%----------------------------
%% http://xmpp.org/extensions/xep-0249.html

send_direct_invitation(RoomName, RoomService, Password, Reason, UsersString) ->
    RoomJid = jid:make(RoomName, RoomService, <<"">>),
    RoomString = jid:to_string(RoomJid),
    XmlEl = build_invitation(Password, Reason, RoomString),
    UsersStrings = get_users_to_invite(RoomJid, binary_to_list(UsersString)),
    [send_direct_invitation(RoomJid, jid:from_string(list_to_binary(UserStrings)), XmlEl)
     || UserStrings <- UsersStrings],
    timer:sleep(1000),
    ok.

get_users_to_invite(RoomJid, UsersString) ->
    UsersStrings = string:tokens(UsersString, ":"),
    OccupantsTuples = get_room_occupants(RoomJid#jid.luser,
					 RoomJid#jid.lserver),
    OccupantsJids = [jid:from_string(JidString)
		     || {JidString, _Nick, _} <- OccupantsTuples],
    lists:filter(
	fun(UserString) ->
	    UserJid = jid:from_string(list_to_binary(UserString)),
	    %% [{"badlop@localhost/work","badlop","moderator"}]
	    lists:all(fun(OccupantJid) ->
		UserJid#jid.luser /= OccupantJid#jid.luser
		orelse UserJid#jid.lserver /= OccupantJid#jid.lserver
	    end,
	    OccupantsJids)
	end,
	UsersStrings).

build_invitation(Password, Reason, RoomString) ->
    PasswordAttrList = case Password of
	<<"none">> -> [];
	_ -> [{<<"password">>, Password}]
    end,
    ReasonAttrList = case Reason of
	<<"none">> -> [];
	_ -> [{<<"reason">>, Reason}]
    end,
    XAttrs = [{<<"xmlns">>, ?NS_XCONFERENCE},
	      {<<"jid">>, RoomString}]
	++ PasswordAttrList
	++ ReasonAttrList,
    XEl = {xmlel, <<"x">>, XAttrs, []},
    {xmlel, <<"message">>, [], [XEl]}.

send_direct_invitation(FromJid, UserJid, XmlEl) ->
    ejabberd_router:route(FromJid, UserJid, XmlEl).

%%----------------------------
%% Change Room Option
%%----------------------------

%% @spec(Name::string(), Service::string(), Option::string(), Value) -> ok
%%       Value = atom() | integer() | string()
%% @doc Change an option in an existing room.
%% Requires the name of the room, the MUC service where it exists,
%% the option to change (for example title or max_users),
%% and the value to assign to the new option.
%% For example:
%%   change_room_option("testroom", "conference.localhost", "title", "Test Room")
change_room_option(Name, Service, Option, Value) when is_atom(Option) ->
    Pid = get_room_pid(Name, Service),
    {ok, _} = change_room_option(Pid, Option, Value),
    ok;
change_room_option(Name, Service, OptionString, ValueString) ->
    Option = jlib:binary_to_atom(OptionString),
    Value = case Option of
		title -> ValueString;
		description -> ValueString;
		password -> ValueString;
		subject ->ValueString;
		subject_author ->ValueString;
		max_users -> jlib:binary_to_integer(ValueString);
		_ -> jlib:binary_to_atom(ValueString)
	    end,
    change_room_option(Name, Service, Option, Value).

change_room_option(Pid, Option, Value) ->
    Config = get_room_config(Pid),
    Config2 = change_option(Option, Value, Config),
    gen_fsm:sync_send_all_state_event(Pid, {change_config, Config2}).

%% @doc Get the Pid of an existing MUC room, or 'room_not_found'.
get_room_pid(Name, Service) ->
    case mnesia:dirty_read(muc_online_room, {Name, Service}) of
	[] ->
	    room_not_found;
	[Room] ->
	    Room#muc_online_room.pid
    end.

%% It is required to put explicitely all the options because
%% the record elements are replaced at compile time.
%% So, this can't be parametrized.
change_option(Option, Value, Config) ->
    case Option of
	allow_change_subj -> Config#config{allow_change_subj = Value};
	allow_private_messages -> Config#config{allow_private_messages = Value};
	allow_private_messages_from_visitors -> Config#config{allow_private_messages_from_visitors = Value};
	allow_query_users -> Config#config{allow_query_users = Value};
	allow_user_invites -> Config#config{allow_user_invites = Value};
	allow_visitor_nickchange -> Config#config{allow_visitor_nickchange = Value};
	allow_visitor_status -> Config#config{allow_visitor_status = Value};
	allow_voice_requests -> Config#config{allow_voice_requests = Value};
	anonymous -> Config#config{anonymous = Value};
	captcha_protected -> Config#config{captcha_protected = Value};
	description -> Config#config{description = Value};
	logging -> Config#config{logging = Value};
	max_users -> Config#config{max_users = Value};
	members_by_default -> Config#config{members_by_default = Value};
	members_only -> Config#config{members_only = Value};
	moderated -> Config#config{moderated = Value};
	password -> Config#config{password = Value};
	password_protected -> Config#config{password_protected = Value};
	persistent -> Config#config{persistent = Value};
	public -> Config#config{public = Value};
	public_list -> Config#config{public_list = Value};
	title -> Config#config{title = Value};
	vcard -> Config#config{vcard = Value};
	voice_request_min_interval -> Config#config{voice_request_min_interval = Value}
    end.

%%----------------------------
%% Get Room Options
%%----------------------------

get_room_options(Name, Service) ->
    case get_room_pid(Name, Service) of
        room_not_found -> [];
        Pid -> get_room_options(Pid)
    end.

get_room_options(Pid) ->
    Config = get_room_config(Pid),
    get_options(Config).

get_options(Config) ->
    Fields = record_info(fields, config),
    [config | Values] = tuple_to_list(Config),
    lists:zip(Fields, Values).

%%----------------------------
%% Get Room Affiliations
%%----------------------------

%% @spec(Name::binary(), Service::binary()) ->
%%    [{JID::string(), Domain::string(), Role::string(), Reason::string()}]
%% @doc Get the affiliations of  the room Name@Service.
get_room_affiliations(Name, Service) ->
    case mnesia:dirty_read(muc_online_room, {Name, Service}) of
	[R] ->
	    %% Get the PID of the online room, then request its state
	    Pid = R#muc_online_room.pid,
	    {ok, StateData} = gen_fsm:sync_send_all_state_event(Pid, get_state),
	    Affiliations = ?DICT:to_list(StateData#state.affiliations),
	    lists:map(
	      fun({{Uname, Domain, _Res}, {Aff, Reason}}) when is_atom(Aff)->
		      {Uname, Domain, Aff, Reason};
		 ({{Uname, Domain, _Res}, Aff}) when is_atom(Aff)->
		      {Uname, Domain, Aff, <<>>}
	      end, Affiliations);
	[] ->
	    throw({error, "The room does not exist."})
    end.

%%----------------------------
%% Change Room Affiliation
%%----------------------------

%% @spec(Name, Service, JID, AffiliationString) -> ok | {error, Error}
%%       Name = binary()
%%       Service = binary()
%%       JID = binary()
%%       AffiliationString = "outcast" | "none" | "member" | "admin" | "owner"
%% @doc Set the affiliation of JID in the room Name@Service.
%% If the affiliation is 'none', the action is to remove,
%% In any other case the action will be to create the affiliation.
set_room_affiliation(Name, Service, JID, AffiliationString) ->
    Affiliation = jlib:binary_to_atom(AffiliationString),
    case mnesia:dirty_read(muc_online_room, {Name, Service}) of
	[R] ->
	    %% Get the PID for the online room so we can get the state of the room
	    Pid = R#muc_online_room.pid,
	    {ok, StateData} = gen_fsm:sync_send_all_state_event(Pid, {process_item_change, {jid:from_string(JID), affiliation, Affiliation, <<"">>}, <<"">>}),
	    mod_muc:store_room(StateData#state.server_host, StateData#state.host, StateData#state.room, make_opts(StateData)),
	    ok;
	[] ->
	    error
    end.

make_opts(StateData) ->
    Config = StateData#state.config,
    [
     {title, Config#config.title},
     {allow_change_subj, Config#config.allow_change_subj},
     {allow_query_users, Config#config.allow_query_users},
     {allow_private_messages, Config#config.allow_private_messages},
     {public, Config#config.public},
     {public_list, Config#config.public_list},
     {persistent, Config#config.persistent},
     {moderated, Config#config.moderated},
     {members_by_default, Config#config.members_by_default},
     {members_only, Config#config.members_only},
     {allow_user_invites, Config#config.allow_user_invites},
     {password_protected, Config#config.password_protected},
     {password, Config#config.password},
     {anonymous, Config#config.anonymous},
     {logging, Config#config.logging},
     {max_users, Config#config.max_users},
     {affiliations, ?DICT:to_list(StateData#state.affiliations)},
     {subject, StateData#state.subject},
     {subject_author, StateData#state.subject_author}
    ].


%%----------------------------
%% Utils
%%----------------------------

uptime_seconds() ->
    trunc(element(1, erlang:statistics(wall_clock))/1000).

find_host(global) ->
    global;
find_host("global") ->
    global;
find_host(<<"global">>) ->
    global;
find_host(ServerHost) when is_list(ServerHost) ->
    find_host(list_to_binary(ServerHost));
find_host(ServerHost) ->
    gen_mod:get_module_opt_host(ServerHost, mod_muc, <<"conference.@HOST@">>).

mod_opt_type(_) -> [].
