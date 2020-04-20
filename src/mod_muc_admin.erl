%%%----------------------------------------------------------------------
%%% File    : mod_muc_admin.erl
%%% Author  : Badlop <badlop@ono.com>
%%% Purpose : Tools for additional MUC administration
%%% Created : 8 Sep 2007 by Badlop <badlop@ono.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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

-module(mod_muc_admin).
-author('badlop@ono.com').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, depends/2, mod_doc/0,
	 muc_online_rooms/1, muc_online_rooms_by_regex/2,
	 muc_register_nick/3, muc_unregister_nick/2,
	 create_room_with_opts/4, create_room/3, destroy_room/2,
	 create_rooms_file/1, destroy_rooms_file/1,
	 rooms_unused_list/2, rooms_unused_destroy/2,
	 rooms_empty_list/1, rooms_empty_destroy/1,
	 get_user_rooms/2, get_room_occupants/2,
	 get_room_occupants_number/2, send_direct_invitation/5,
	 change_room_option/4, get_room_options/2,
	 set_room_affiliation/4, get_room_affiliations/2, get_room_affiliation/3,
	 web_menu_main/2, web_page_main/2, web_menu_host/3,
	 subscribe_room/4, unsubscribe_room/2, get_subscribers/2,
	 web_page_host/3, mod_options/1, get_commands_spec/0, find_hosts/1]).

-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_muc.hrl").
-include("mod_muc_room.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("ejabberd_commands.hrl").
-include("translate.hrl").

%%----------------------------
%% gen_mod
%%----------------------------

start(Host, _Opts) ->
    ejabberd_commands:register_commands(get_commands_spec()),
    ejabberd_hooks:add(webadmin_menu_main, ?MODULE, web_menu_main, 50),
    ejabberd_hooks:add(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:add(webadmin_page_main, ?MODULE, web_page_main, 50),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE, web_page_host, 50).

stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(get_commands_spec());
        true ->
            ok
    end,
    ejabberd_hooks:delete(webadmin_menu_main, ?MODULE, web_menu_main, 50),
    ejabberd_hooks:delete(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:delete(webadmin_page_main, ?MODULE, web_page_main, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host, ?MODULE, web_page_host, 50).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [{mod_muc, hard}].

%%%
%%% Register commands
%%%

get_commands_spec() ->
    [
     #ejabberd_commands{name = muc_online_rooms, tags = [muc],
		       desc = "List existing rooms ('global' to get all vhosts)",
                       policy = admin,
		       module = ?MODULE, function = muc_online_rooms,
		       args_desc = ["MUC service, or 'global' for all"],
		       args_example = ["muc.example.com"],
		       result_desc = "List of rooms",
		       result_example = ["room1@muc.example.com", "room2@muc.example.com"],
		       args = [{service, binary}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},
	#ejabberd_commands{name = muc_online_rooms_by_regex, tags = [muc],
		       desc = "List existing rooms ('global' to get all vhosts) by regex",
                       policy = admin,
		       module = ?MODULE, function = muc_online_rooms_by_regex,
		       args_desc = ["MUC service, or 'global' for all",
			   				"Regex pattern for room name"],
		       args_example = ["muc.example.com", "^prefix"],
		       result_desc = "List of rooms with summary",
		       result_example = [{"room1@muc.example.com", "true", 10},
			   					 {"room2@muc.example.com", "false", 10}],
		       args = [{service, binary}, {regex, binary}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, {tuple,
							  [{jid, string},
							   {public, string},
							   {participants, integer}
							  ]}}}}},
     #ejabberd_commands{name = muc_register_nick, tags = [muc],
		       desc = "Register a nick to a User JID in a MUC service",
		       module = ?MODULE, function = muc_register_nick,
		       args_desc = ["Nick", "User JID", "Service"],
		       args_example = [<<"Tim">>, <<"tim@example.org">>, <<"muc.example.org">>],
		       args = [{nick, binary}, {jid, binary}, {service, binary}],
		       args_rename = [{host, service}],
		       result = {res, rescode}},
     #ejabberd_commands{name = muc_unregister_nick, tags = [muc],
		       desc = "Unregister the nick registered by that account in the MUC service",
		       module = ?MODULE, function = muc_unregister_nick,
		       args_desc = ["User JID", "MUC service"],
		       args_example = [<<"tim@example.org">>, <<"muc.example.org">>],
		       args = [{jid, binary}, {service, binary}],
		       args_rename = [{host, service}],
		       result = {res, rescode}},

     #ejabberd_commands{name = create_room, tags = [muc_room],
		       desc = "Create a MUC room name@service in host",
		       module = ?MODULE, function = create_room,
		       args_desc = ["Room name", "MUC service", "Server host"],
		       args_example = ["room1", "muc.example.com", "example.com"],
		       args = [{name, binary}, {service, binary},
			       {host, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = destroy_room, tags = [muc_room],
		       desc = "Destroy a MUC room",
		       module = ?MODULE, function = destroy_room,
		       args_desc = ["Room name", "MUC service"],
		       args_example = ["room1", "muc.example.com"],
		       args = [{name, binary}, {service, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = create_rooms_file, tags = [muc],
		       desc = "Create the rooms indicated in file",
		       longdesc = "Provide one room JID per line. Rooms will be created after restart.",
		       module = ?MODULE, function = create_rooms_file,
		       args_desc = ["Path to the text file with one room JID per line"],
		       args_example = ["/home/ejabberd/rooms.txt"],
		       args = [{file, string}],
		       result = {res, rescode}},
     #ejabberd_commands{name = create_room_with_opts, tags = [muc_room],
		       desc = "Create a MUC room name@service in host with given options",
		       module = ?MODULE, function = create_room_with_opts,
		       args_desc = ["Room name", "MUC service", "Server host", "List of options"],
		       args_example = ["room1", "muc.example.com", "localhost", [{"members_only","true"}]],
		       args = [{name, binary}, {service, binary},
			       {host, binary},
			       {options, {list,
					  {option, {tuple,
						    [{name, binary},
						     {value, binary}
						    ]}}
					 }}],
		       result = {res, rescode}},
     #ejabberd_commands{name = destroy_rooms_file, tags = [muc],
		       desc = "Destroy the rooms indicated in file",
		       longdesc = "Provide one room JID per line.",
		       module = ?MODULE, function = destroy_rooms_file,
		       args_desc = ["Path to the text file with one room JID per line"],
		       args_example = ["/home/ejabberd/rooms.txt"],
		       args = [{file, string}],
		       result = {res, rescode}},
     #ejabberd_commands{name = rooms_unused_list, tags = [muc],
		       desc = "List the rooms that are unused for many days in the service",
		       longdesc = "The room recent history is used, so it's recommended "
			    " to wait a few days after service start before running this."
			    " The MUC service argument can be 'global' to get all hosts.",
		       module = ?MODULE, function = rooms_unused_list,
		       args_desc = ["MUC service, or 'global' for all", "Number of days"],
		       args_example = ["muc.example.com", 31],
		       result_desc = "List of unused rooms",
		       result_example = ["room1@muc.example.com", "room2@muc.example.com"],
		       args = [{service, binary}, {days, integer}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},
     #ejabberd_commands{name = rooms_unused_destroy, tags = [muc],
		       desc = "Destroy the rooms that are unused for many days in the service",
		       longdesc = "The room recent history is used, so it's recommended "
			    " to wait a few days after service start before running this."
			    " The MUC service argument can be 'global' to get all hosts.",
		       module = ?MODULE, function = rooms_unused_destroy,
		       args_desc = ["MUC service, or 'global' for all", "Number of days"],
		       args_example = ["muc.example.com", 31],
		       result_desc = "List of unused rooms that has been destroyed",
		       result_example = ["room1@muc.example.com", "room2@muc.example.com"],
		       args = [{service, binary}, {days, integer}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},

     #ejabberd_commands{name = rooms_empty_list, tags = [muc],
		       desc = "List the rooms that have no messages in archive",
		       longdesc = "The MUC service argument can be 'global' to get all hosts.",
		       module = ?MODULE, function = rooms_empty_list,
		       args_desc = ["MUC service, or 'global' for all"],
		       args_example = ["muc.example.com"],
		       result_desc = "List of empty rooms",
		       result_example = ["room1@muc.example.com", "room2@muc.example.com"],
		       args = [{service, binary}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},
     #ejabberd_commands{name = rooms_empty_destroy, tags = [muc],
		       desc = "Destroy the rooms that have no messages in archive",
		       longdesc = "The MUC service argument can be 'global' to get all hosts.",
		       module = ?MODULE, function = rooms_empty_destroy,
		       args_desc = ["MUC service, or 'global' for all"],
		       args_example = ["muc.example.com"],
		       result_desc = "List of empty rooms that have been destroyed",
		       result_example = ["room1@muc.example.com", "room2@muc.example.com"],
		       args = [{service, binary}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},

     #ejabberd_commands{name = get_user_rooms, tags = [muc],
			desc = "Get the list of rooms where this user is occupant",
			module = ?MODULE, function = get_user_rooms,
		        args_desc = ["Username", "Server host"],
		        args_example = ["tom", "example.com"],
		        result_example = ["room1@muc.example.com", "room2@muc.example.com"],
			args = [{user, binary}, {host, binary}],
		        result = {rooms, {list, {room, string}}}},

     #ejabberd_commands{name = get_room_occupants, tags = [muc_room],
			desc = "Get the list of occupants of a MUC room",
			module = ?MODULE, function = get_room_occupants,
		        args_desc = ["Room name", "MUC service"],
		        args_example = ["room1", "muc.example.com"],
		        result_desc = "The list of occupants with JID, nick and affiliation",
		        result_example = [{"user1@example.com/psi", "User 1", "owner"}],
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
		        args_desc = ["Room name", "MUC service"],
		        args_example = ["room1", "muc.example.com"],
		        result_desc = "Number of room occupants",
		        result_example = 7,
			args = [{name, binary}, {service, binary}],
			result = {occupants, integer}},

     #ejabberd_commands{name = send_direct_invitation, tags = [muc_room],
			desc = "Send a direct invitation to several destinations",
			longdesc = "Password and Message can also be: none. Users JIDs are separated with : ",
			module = ?MODULE, function = send_direct_invitation,
		        args_desc = ["Room name", "MUC service", "Password, or none",
			 "Reason text, or none", "Users JIDs separated with : characters"],
			args_example = [<<"room1">>, <<"muc.example.com">>,
					<<>>, <<"Check this out!">>,
					"user2@localhost:user3@example.com"],
			args = [{name, binary}, {service, binary}, {password, binary},
				{reason, binary}, {users, binary}],
		        result = {res, rescode}},

     #ejabberd_commands{name = change_room_option, tags = [muc_room],
		       desc = "Change an option in a MUC room",
		       module = ?MODULE, function = change_room_option,
		       args_desc = ["Room name", "MUC service", "Option name", "Value to assign"],
		       args_example = ["room1", "muc.example.com", "members_only", "true"],
		       args = [{name, binary}, {service, binary},
			       {option, binary}, {value, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = get_room_options, tags = [muc_room],
		        desc = "Get options from a MUC room",
		        module = ?MODULE, function = get_room_options,
		        args_desc = ["Room name", "MUC service"],
		        args_example = ["room1", "muc.example.com"],
		        result_desc = "List of room options tuples with name and value",
		        result_example = [{"members_only", "true"}],
		        args = [{name, binary}, {service, binary}],
			result = {options, {list,
						 {option, {tuple,
								[{name, string},
								 {value, string}
								]}}
						}}},
     #ejabberd_commands{name = subscribe_room, tags = [muc_room],
			desc = "Subscribe to a MUC conference",
			module = ?MODULE, function = subscribe_room,
			args_desc = ["User JID", "a user's nick",
			    "the room to subscribe", "nodes separated by commas: ,"],
			args_example = ["tom@localhost", "Tom", "room1@conference.localhost",
			    "urn:xmpp:mucsub:nodes:messages,urn:xmpp:mucsub:nodes:affiliations"],
			result_desc = "The list of nodes that has subscribed",
			result_example = ["urn:xmpp:mucsub:nodes:messages",
			    "urn:xmpp:mucsub:nodes:affiliations"],
			args = [{user, binary}, {nick, binary}, {room, binary},
				{nodes, binary}],
			result = {nodes, {list, {node, string}}}},
     #ejabberd_commands{name = unsubscribe_room, tags = [muc_room],
			desc = "Unsubscribe from a MUC conference",
			module = ?MODULE, function = unsubscribe_room,
			args_desc = ["User JID", "the room to subscribe"],
			args_example = ["tom@localhost", "room1@conference.localhost"],
			args = [{user, binary}, {room, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = get_subscribers, tags = [muc_room],
			desc = "List subscribers of a MUC conference",
			module = ?MODULE, function = get_subscribers,
		        args_desc = ["Room name", "MUC service"],
		        args_example = ["room1", "muc.example.com"],
		        result_desc = "The list of users that are subscribed to that room",
		        result_example = ["user2@example.com", "user3@example.com"],
			args = [{name, binary}, {service, binary}],
			result = {subscribers, {list, {jid, string}}}},
     #ejabberd_commands{name = set_room_affiliation, tags = [muc_room],
		       desc = "Change an affiliation in a MUC room",
		       module = ?MODULE, function = set_room_affiliation,
		       args_desc = ["Room name", "MUC service", "User JID", "Affiliation to set"],
		       args_example = ["room1", "muc.example.com", "user2@example.com", "member"],
		       args = [{name, binary}, {service, binary},
			       {jid, binary}, {affiliation, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = get_room_affiliations, tags = [muc_room],
			desc = "Get the list of affiliations of a MUC room",
			module = ?MODULE, function = get_room_affiliations,
		        args_desc = ["Room name", "MUC service"],
		        args_example = ["room1", "muc.example.com"],
		        result_desc = "The list of affiliations with username, domain, affiliation and reason",
			result_example = [{"user1", "example.com", member, "member"}],
			args = [{name, binary}, {service, binary}],
			result = {affiliations, {list,
						 {affiliation, {tuple,
								[{username, string},
								 {domain, string},
								 {affiliation, atom},
								 {reason, string}
								]}}
						}}},
	 #ejabberd_commands{name = get_room_affiliation, tags = [muc_room],
			desc = "Get affiliation of a user in MUC room",
			module = ?MODULE, function = get_room_affiliation,
			args_desc = ["Room name", "MUC service", "User JID"],
			args_example = ["room1", "muc.example.com", "user1@example.com"],
			result_desc = "Affiliation of the user",
			result_example = member,
			args = [{name, binary}, {service, binary}, {jid, binary}],
			result = {affiliation, atom}}
	].


%%%
%%% ejabberd commands
%%%

muc_online_rooms(ServiceArg) ->
    Hosts = find_services(ServiceArg),
    lists:flatmap(
      fun(Host) ->
	      [<<Name/binary, "@", Host/binary>>
	       || {Name, _, _} <- mod_muc:get_online_rooms(Host)]
      end, Hosts).

muc_online_rooms_by_regex(ServiceArg, Regex) ->
    {_, P} = re:compile(Regex),
    Hosts = find_services(ServiceArg),
    lists:flatmap(
      fun(Host) ->
	      [build_summary_room(Name, RoomHost, Pid)
	       || {Name, RoomHost, Pid} <- mod_muc:get_online_rooms(Host),
		   is_name_match(Name, P)]
      end, Hosts).

is_name_match(Name, P) ->
	case re:run(Name, P) of
		{match, _} -> true;
		nomatch -> false
	end.

build_summary_room(Name, Host, Pid) ->
    C = get_room_config(Pid),
    Public = C#config.public,
    S = get_room_state(Pid),
    Participants = maps:size(S#state.users),
    {<<Name/binary, "@", Host/binary>>,
	 misc:atom_to_binary(Public),
     Participants
    }.

muc_register_nick(Nick, FromBinary, Service) ->
    ServerHost = get_room_serverhost(Service),
    From = jid:decode(FromBinary),
    Lang = <<"en">>,
    case mod_muc:iq_set_register_info(ServerHost, Service, From, Nick, Lang) of
	{result, undefined} -> ok;
	E -> E
    end.

muc_unregister_nick(FromBinary, Service) ->
    muc_register_nick(<<"">>, FromBinary, Service).

get_user_rooms(User, Server) ->
    lists:flatmap(
      fun(ServerHost) ->
	      case gen_mod:is_loaded(ServerHost, mod_muc) of
		  true ->
		      Rooms = mod_muc:get_online_rooms_by_user(
				ServerHost, jid:nodeprep(User), jid:nodeprep(Server)),
		      [<<Name/binary, "@", Host/binary>>
			   || {Name, Host} <- Rooms];
		  false ->
		      []
	      end
      end, ejabberd_option:hosts()).

%%----------------------------
%% Ad-hoc commands
%%----------------------------


%%----------------------------
%% Web Admin
%%----------------------------

%%---------------
%% Web Admin Menu

web_menu_main(Acc, Lang) ->
    Acc ++ [{<<"muc">>, translate:translate(Lang, ?T("Multi-User Chat"))}].

web_menu_host(Acc, _Host, Lang) ->
    Acc ++ [{<<"muc">>, translate:translate(Lang, ?T("Multi-User Chat"))}].


%%---------------
%% Web Admin Page

-define(TDTD(L, N),
	?XE(<<"tr">>, [?XCT(<<"td">>, L),
		       ?XC(<<"td">>, integer_to_binary(N))
		      ])).

web_page_main(_, #request{path=[<<"muc">>], lang = Lang} = _Request) ->
    OnlineRoomsNumber = lists:foldl(
			  fun(Host, Acc) ->
				  Acc + mod_muc:count_online_rooms(Host)
			  end, 0, find_hosts(global)),
    PageTitle = translate:translate(Lang, ?T("Multi-User Chat")),
    Res = ?H1GL(PageTitle, <<"modules/#mod-muc">>, <<"mod_muc">>) ++
	  [?XCT(<<"h3">>, ?T("Statistics")),
	   ?XAE(<<"table">>, [],
		[?XE(<<"tbody">>, [?TDTD(?T("Total rooms"), OnlineRoomsNumber)
				  ])
		]),
	   ?XE(<<"ul">>, [?LI([?ACT(<<"rooms/">>, ?T("List of rooms"))])])
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
    Res = make_rooms_page(Host, Lang, Sort_query),
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
    Integer = binary_to_integer(String),
    case Integer >= 0 of
	true -> {ok, {normal, Integer}};
	false -> {ok, {reverse, abs(Integer)}}
    end.

make_rooms_page(Host, Lang, {Sort_direction, Sort_column}) ->
    Service = find_service(Host),
    Rooms_names = get_rooms(Service),
    Rooms_infos = build_info_rooms(Rooms_names),
    Rooms_sorted = sort_rooms(Sort_direction, Sort_column, Rooms_infos),
    Rooms_prepared = prepare_rooms_infos(Rooms_sorted),
    TList = lists:map(
	      fun(Room) ->
		      ?XE(<<"tr">>, [?XC(<<"td">>, E) || E <- Room])
	      end, Rooms_prepared),
    Titles = [?T("Jabber ID"),
	      ?T("# participants"),
	      ?T("Last message"),
	      ?T("Public"),
	      ?T("Persistent"),
	      ?T("Logging"),
	      ?T("Just created"),
	      ?T("Room title")],
    {Titles_TR, _} =
	lists:mapfoldl(
	  fun(Title, Num_column) ->
		  NCS = integer_to_binary(Num_column),
		  TD = ?XE(<<"td">>, [?CT(Title),
				      ?C(<<" ">>),
				      ?AC(<<"?sort=", NCS/binary>>, <<"<">>),
				      ?C(<<" ">>),
				      ?AC(<<"?sort=-", NCS/binary>>, <<">">>)]),
		  {TD, Num_column+1}
	  end,
	  1,
	  Titles),
    PageTitle = translate:translate(Lang, ?T("Multi-User Chat")),
    ?H1GL(PageTitle, <<"modules/#mod-muc">>, <<"mod_muc">>) ++
    [?XCT(<<"h2">>, ?T("Chatrooms")),
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

build_info_room({Name, Host, _ServerHost, Pid}) ->
    C = get_room_config(Pid),
    Title = C#config.title,
    Public = C#config.public,
    Persistent = C#config.persistent,
    Logging = C#config.logging,

    S = get_room_state(Pid),
    Just_created = S#state.just_created,
    Num_participants = maps:size(S#state.users),

    History = (S#state.history)#lqueue.queue,
    Ts_last_message =
	case p1_queue:is_empty(History) of
	    true ->
		<<"A long time ago">>;
	    false ->
		Last_message1 = get_queue_last(History),
		{_, _, _, Ts_last, _} = Last_message1,
		xmpp_util:encode_timestamp(Ts_last)
	end,

    {<<Name/binary, "@", Host/binary>>,
     Num_participants,
     Ts_last_message,
     Public,
     Persistent,
     Logging,
     Just_created,
     Title}.

get_queue_last(Queue) ->
    List = p1_queue:to_list(Queue),
    lists:last(List).

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
     integer_to_binary(Num_participants),
     Ts_last_message,
     misc:atom_to_binary(Public),
     misc:atom_to_binary(Persistent),
     misc:atom_to_binary(Logging),
     justcreated_to_binary(Just_created),
     Title].

justcreated_to_binary(J) when is_integer(J) ->
    JNow = misc:usec_to_now(J),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(JNow),
    str:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
	       [Year, Month, Day, Hour, Minute, Second]);
justcreated_to_binary(J) when is_atom(J) ->
    misc:atom_to_binary(J).

%%----------------------------
%% Create/Delete Room
%%----------------------------

%% @spec (Name::binary(), Host::binary(), ServerHost::binary()) ->
%%       ok | error
%% @doc Create a room immediately with the default options.
create_room(Name1, Host1, ServerHost) ->
    create_room_with_opts(Name1, Host1, ServerHost, []).

create_room_with_opts(Name1, Host1, ServerHost, CustomRoomOpts) ->
    true = (error /= (Name = jid:nodeprep(Name1))),
    true = (error /= (Host = jid:nodeprep(Host1))),

    %% Get the default room options from the muc configuration
    DefRoomOpts = mod_muc_opt:default_room_options(ServerHost),
    %% Change default room options as required
    FormattedRoomOpts = [format_room_option(Opt, Val) || {Opt, Val}<-CustomRoomOpts],
    RoomOpts = lists:ukeymerge(1,
                               lists:keysort(1, FormattedRoomOpts),
                               lists:keysort(1, DefRoomOpts)),

    %% Store the room on the server, it is not started yet though at this point
    case lists:keyfind(persistent, 1, RoomOpts) of
	{persistent, true} ->
	    mod_muc:store_room(ServerHost, Host, Name, RoomOpts);
	_ ->
	    ok
    end,

    %% Get all remaining mod_muc parameters that might be utilized
    Access = mod_muc_opt:access(ServerHost),
    AcCreate = mod_muc_opt:access_create(ServerHost),
    AcAdmin = mod_muc_opt:access_admin(ServerHost),
    AcPer = mod_muc_opt:access_persistent(ServerHost),
    AcMam = mod_muc_opt:access_mam(ServerHost),
    HistorySize = mod_muc_opt:history_size(ServerHost),
    RoomShaper = mod_muc_opt:room_shaper(ServerHost),
    QueueType = mod_muc_opt:queue_type(ServerHost),

    %% If the room does not exist yet in the muc_online_room
    case mod_muc:find_online_room(Name, Host) of
	error ->
	    %% Start the room
	    {ok, Pid} = mod_muc_room:start(
			  Host,
			  ServerHost,
			  {Access, AcCreate, AcAdmin, AcPer, AcMam},
			  Name,
			  HistorySize,
			  RoomShaper,
			  RoomOpts,
			  QueueType),
	    mod_muc:register_online_room(Name, Host, Pid),
	    ok;
	{ok, _} ->
	    error
    end.

%% Create the room only in the database.
%% It is required to restart the MUC service for the room to appear.
muc_create_room(ServerHost, {Name, Host, _}, DefRoomOpts) ->
    io:format("Creating room ~ts@~ts~n", [Name, Host]),
    mod_muc:store_room(ServerHost, Host, Name, DefRoomOpts).

%% @spec (Name::binary(), Host::binary()) ->
%%       ok | {error, room_not_exists}
%% @doc Destroy the room immediately.
%% If the room has participants, they are not notified that the room was destroyed;
%% they will notice when they try to chat and receive an error that the room doesn't exist.
destroy_room(Name, Service) ->
    case mod_muc:find_online_room(Name, Service) of
	{ok, Pid} ->
	    mod_muc_room:destroy(Pid);
	error ->
	    error
    end.

destroy_room({N, H, SH}) ->
    io:format("Destroying room: ~ts@~ts - vhost: ~ts~n", [N, H, SH]),
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
read_rooms(F, no_room, L) ->
    RJID2 = read_room(F),
    read_rooms(F, RJID2, L);
read_rooms(F, RJID, L) ->
    RJID2 = read_room(F),
    read_rooms(F, RJID2, [RJID | L]).

read_room(F) ->
    case io:get_line(F, "") of
	eof -> eof;
	String ->
	    case io_lib:fread("~ts", String) of
		{ok, [RoomJID], _} -> split_roomjid(list_to_binary(RoomJID));
		{error, What} ->
		    io:format("Parse error: what: ~p~non the line: ~p~n~n", [What, String])
	    end
    end.

%% This function is quite rudimentary
%% and may not be accurate
split_roomjid(RoomJID) ->
    split_roomjid2(binary:split(RoomJID, <<"@">>)).
split_roomjid2([Name, Host]) ->
    [_MUC_service_name, ServerHost] = binary:split(Host, <<".">>),
    {Name, Host, ServerHost};
split_roomjid2(_) ->
    no_room.

%%----------------------------
%% Create Rooms in File
%%----------------------------

create_rooms_file(Filename) ->
    {ok, F} = file:open(Filename, [read]),
    RJID = read_room(F),
    Rooms = read_rooms(F, RJID, []),
    file:close(F),
    %% Read the default room options defined for the first virtual host
    DefRoomOpts = mod_muc_opt:default_room_options(ejabberd_config:get_myname()),
    [muc_create_room(ejabberd_config:get_myname(), A, DefRoomOpts) || A <- Rooms],
	ok.


%%---------------------------------
%% List/Delete Unused/Empty Rooms
%%---------------------------------

%%---------------
%% Control

rooms_unused_list(Service, Days) ->
    rooms_report(unused, list, Service, Days).
rooms_unused_destroy(Service, Days) ->
    rooms_report(unused, destroy, Service, Days).

rooms_empty_list(Service) ->
    rooms_report(empty, list, Service, 0).
rooms_empty_destroy(Service) ->
    rooms_report(empty, destroy, Service, 0).


rooms_report(Method, Action, Service, Days) ->
    {NA, NP, RP} = muc_unused(Method, Action, Service, Days),
    io:format("rooms ~ts: ~p out of ~p~n", [Method, NP, NA]),
    [<<R/binary, "@", H/binary>> || {R, H, _SH, _P} <- RP].

muc_unused(Method, Action, Service, Last_allowed) ->
    %% Get all required info about all existing rooms
    Rooms_all = get_rooms(Service),

    %% Decide which ones pass the requirements
    Rooms_pass = decide_rooms(Method, Rooms_all, Last_allowed),

    Num_rooms_all = length(Rooms_all),
    Num_rooms_pass = length(Rooms_pass),

    %% Perform the desired action for matching rooms
    act_on_rooms(Method, Action, Rooms_pass),

    {Num_rooms_all, Num_rooms_pass, Rooms_pass}.

%%---------------
%% Get info

get_rooms(ServiceArg) ->
    Hosts = find_services(ServiceArg),
    lists:flatmap(
      fun(Host) ->
	      [{RoomName, RoomHost, ejabberd_router:host_of_route(Host), Pid}
	       || {RoomName, RoomHost, Pid} <- mod_muc:get_online_rooms(Host)]
      end, Hosts).

get_room_config(Room_pid) ->
    {ok, R} = mod_muc_room:get_config(Room_pid),
    R.

get_room_state(Room_pid) ->
    {ok, R} = mod_muc_room:get_state(Room_pid),
    R.

%%---------------
%% Decide

decide_rooms(Method, Rooms, Last_allowed) ->
    Decide = fun(R) -> decide_room(Method, R, Last_allowed) end,
    lists:filter(Decide, Rooms).

decide_room(unused, {_Room_name, _Host, ServerHost, Room_pid}, Last_allowed) ->
    C = get_room_config(Room_pid),
    Persistent = C#config.persistent,

    S = get_room_state(Room_pid),
    Just_created = S#state.just_created,

    Room_users = S#state.users,
    Num_users = maps:size(Room_users),

    History = (S#state.history)#lqueue.queue,
    Ts_now = calendar:universal_time(),
    HistorySize = mod_muc_opt:history_size(ServerHost),
    {Has_hist, Last} = case p1_queue:is_empty(History) of
			   true when (HistorySize == 0) or (Just_created == true) ->
			       {false, 0};
			   true ->
			       Ts_diff = (erlang:system_time(microsecond)
				    - Just_created) div 1000000,
			       {false, Ts_diff};
			   false ->
			       Last_message = get_queue_last(History),
			       Ts_last = calendar:now_to_universal_time(
					   element(4, Last_message)),
			       Ts_diff =
				   calendar:datetime_to_gregorian_seconds(Ts_now)
				   - calendar:datetime_to_gregorian_seconds(Ts_last),
			       {true, Ts_diff}
		       end,
    case {Persistent, Just_created, Num_users, Has_hist, seconds_to_days(Last)} of
	{_true, JC, 0, _, Last_days}
	when (Last_days >= Last_allowed) and (JC /= true) ->
	    true;
	_ ->
	    false
    end;
decide_room(empty, {Room_name, Host, ServerHost, _Room_pid}, _Last_allowed) ->
    case gen_mod:is_loaded(ServerHost, mod_mam) of
	true ->
	    Room_options = get_room_options(Room_name, Host),
	    case lists:keyfind(<<"mam">>, 1, Room_options) of
		{<<"mam">>, <<"true">>} ->
		    mod_mam:is_empty_for_room(ServerHost, Room_name, Host);
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

seconds_to_days(S) ->
    S div (60*60*24).

%%---------------
%% Act

act_on_rooms(Method, Action, Rooms) ->
    Delete = fun(Room) ->
		     act_on_room(Method, Action, Room)
	     end,
    lists:foreach(Delete, Rooms).

act_on_room(Method, destroy, {N, H, SH, Pid}) ->
    Message = iolist_to_binary(io_lib:format(
        <<"Room destroyed by rooms_~s_destroy.">>, [Method])),
    mod_muc_room:destroy(Pid, Message),
    mod_muc:room_destroyed(H, N, Pid, SH),
    mod_muc:forget_room(SH, H, N);

act_on_room(_Method, list, _) ->
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
	      {jid:encode(Info#user.jid),
	       Info#user.nick,
	       atom_to_list(Info#user.role)}
      end,
      maps:to_list(S#state.users)).

get_room_occupants_number(Room, Host) ->
    case get_room_pid(Room, Host) of
	room_not_found ->
	    throw({error, room_not_found});
	Pid ->
	    S = get_room_state(Pid),
	    maps:size(S#state.users)
    end.

%%----------------------------
%% Send Direct Invitation
%%----------------------------
%% http://xmpp.org/extensions/xep-0249.html

send_direct_invitation(RoomName, RoomService, Password, Reason, UsersString) ->
    RoomJid = jid:make(RoomName, RoomService),
    XmlEl = build_invitation(Password, Reason, RoomJid),
    Users = get_users_to_invite(RoomJid, UsersString),
    [send_direct_invitation(RoomJid, UserJid, XmlEl)
     || UserJid <- Users],
    timer:sleep(1000),
    ok.

get_users_to_invite(RoomJid, UsersString) ->
    UsersStrings = binary:split(UsersString, <<":">>, [global]),
    OccupantsTuples = get_room_occupants(RoomJid#jid.luser,
					 RoomJid#jid.lserver),
    OccupantsJids = [jid:decode(JidString)
		     || {JidString, _Nick, _} <- OccupantsTuples],
    lists:filtermap(
      fun(UserString) ->
	      UserJid = jid:decode(UserString),
	      Val = lists:all(fun(OccupantJid) ->
				      UserJid#jid.luser /= OccupantJid#jid.luser
					  orelse UserJid#jid.lserver /= OccupantJid#jid.lserver
			      end,
			      OccupantsJids),
	      case {UserJid#jid.luser, Val} of
		  {<<>>, _} -> false;
		  {_, true} -> {true, UserJid};
		  _ -> false
	      end
      end,
      UsersStrings).

build_invitation(Password, Reason, RoomJid) ->
    Invite = #x_conference{jid = RoomJid,
			   password = case Password of
					  <<"none">> -> <<>>;
					  _ -> Password
				      end,
			   reason = case Reason of
					<<"none">> -> <<>>;
					_ -> Reason
				    end},
    #message{sub_els = [Invite]}.

send_direct_invitation(FromJid, UserJid, Msg) ->
    ejabberd_router:route(xmpp:set_from_to(Msg, FromJid, UserJid)).

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
%%   change_room_option(<<"testroom">>, <<"conference.localhost">>, <<"title">>, <<"Test Room">>)
change_room_option(Name, Service, OptionString, ValueString) ->
    case get_room_pid(Name, Service) of
	room_not_found ->
	    room_not_found;
	Pid ->
	    {Option, Value} = format_room_option(OptionString, ValueString),
	    change_room_option(Pid, Option, Value)
    end.

change_room_option(Pid, Option, Value) ->
    case {Option,
	  gen_mod:is_loaded((get_room_state(Pid))#state.server_host, mod_muc_log)} of
	{logging, false} ->
	    mod_muc_log_not_enabled;
	_ ->
	    Config = get_room_config(Pid),
	    Config2 = change_option(Option, Value, Config),
	    {ok, _} = mod_muc_room:set_config(Pid, Config2),
	    ok
    end.

format_room_option(OptionString, ValueString) ->
    Option = misc:binary_to_atom(OptionString),
    Value = case Option of
		title -> ValueString;
		description -> ValueString;
		password -> ValueString;
		subject ->ValueString;
		subject_author ->ValueString;
		presence_broadcast ->misc:expr_to_term(ValueString);
		max_users -> binary_to_integer(ValueString);
		voice_request_min_interval -> binary_to_integer(ValueString);
		vcard -> ValueString;
		vcard_xupdate when ValueString /= <<"undefined">>,
				   ValueString /= <<"external">> ->
		    ValueString;
		lang -> ValueString;
		pubsub -> ValueString;
		_ -> misc:binary_to_atom(ValueString)
	    end,
    {Option, Value}.

%% @doc Get the Pid of an existing MUC room, or 'room_not_found'.
get_room_pid(Name, Service) ->
    case mod_muc:find_online_room(Name, Service) of
	error ->
	    room_not_found;
	{ok, Pid} ->
	    Pid
    end.

%% It is required to put explicitly all the options because
%% the record elements are replaced at compile time.
%% So, this can't be parametrized.
change_option(Option, Value, Config) ->
    case Option of
	allow_change_subj -> Config#config{allow_change_subj = Value};
	allow_private_messages -> Config#config{allow_private_messages = Value};
	allow_private_messages_from_visitors -> Config#config{allow_private_messages_from_visitors = Value};
	allow_query_users -> Config#config{allow_query_users = Value};
	allow_subscription -> Config#config{allow_subscription = Value};
	allow_user_invites -> Config#config{allow_user_invites = Value};
	allow_visitor_nickchange -> Config#config{allow_visitor_nickchange = Value};
	allow_visitor_status -> Config#config{allow_visitor_status = Value};
	allow_voice_requests -> Config#config{allow_voice_requests = Value};
	anonymous -> Config#config{anonymous = Value};
	captcha_protected -> Config#config{captcha_protected = Value};
	description -> Config#config{description = Value};
	logging -> Config#config{logging = Value};
	mam -> Config#config{mam = Value};
	max_users -> Config#config{max_users = Value};
	members_by_default -> Config#config{members_by_default = Value};
	members_only -> Config#config{members_only = Value};
	moderated -> Config#config{moderated = Value};
	password -> Config#config{password = Value};
	password_protected -> Config#config{password_protected = Value};
	persistent -> Config#config{persistent = Value};
	presence_broadcast -> Config#config{presence_broadcast = Value};
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
    Fields = [misc:atom_to_binary(Field) || Field <- record_info(fields, config)],
    [config | ValuesRaw] = tuple_to_list(Config),
    Values = lists:map(fun(V) when is_atom(V) -> misc:atom_to_binary(V);
                          (V) when is_integer(V) -> integer_to_binary(V);
                          (V) when is_tuple(V); is_list(V) -> list_to_binary(hd(io_lib:format("~w", [V])));
                          (V) -> V end, ValuesRaw),
    lists:zip(Fields, Values).

%%----------------------------
%% Get Room Affiliations
%%----------------------------

%% @spec(Name::binary(), Service::binary()) ->
%%    [{JID::string(), Domain::string(), Role::string(), Reason::string()}]
%% @doc Get the affiliations of  the room Name@Service.
get_room_affiliations(Name, Service) ->
    case mod_muc:find_online_room(Name, Service) of
	{ok, Pid} ->
	    %% Get the PID of the online room, then request its state
	    {ok, StateData} = mod_muc_room:get_state(Pid),
	    Affiliations = maps:to_list(StateData#state.affiliations),
	    lists:map(
	      fun({{Uname, Domain, _Res}, {Aff, Reason}}) when is_atom(Aff)->
		      {Uname, Domain, Aff, Reason};
		 ({{Uname, Domain, _Res}, Aff}) when is_atom(Aff)->
		      {Uname, Domain, Aff, <<>>}
	      end, Affiliations);
	error ->
	    throw({error, "The room does not exist."})
    end.

%%----------------------------
%% Get Room Affiliation
%%----------------------------

%% @spec(Name::binary(), Service::binary(), JID::binary()) ->
%%    {Affiliation::string()}
%% @doc Get affiliation of a user in the room Name@Service.

get_room_affiliation(Name, Service, JID) ->
	case mod_muc:find_online_room(Name, Service) of
	{ok, Pid} ->
		%% Get the PID of the online room, then request its state
		{ok, StateData} = mod_muc_room:get_state(Pid),
		UserJID = jid:decode(JID),
		mod_muc_room:get_affiliation(UserJID, StateData);
	error ->
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
    Affiliation = misc:binary_to_atom(AffiliationString),
    case mod_muc:find_online_room(Name, Service) of
	{ok, Pid} ->
	    %% Get the PID for the online room so we can get the state of the room
	    {ok, StateData} = mod_muc_room:change_item(Pid, jid:decode(JID), affiliation, Affiliation, <<"">>),
	    mod_muc:store_room(StateData#state.server_host, StateData#state.host, StateData#state.room, make_opts(StateData)),
	    ok;
	error ->
	    error
    end.

%%%
%%% MUC Subscription
%%%

subscribe_room(_User, Nick, _Room, _Nodes) when Nick == <<"">> ->
    throw({error, "Nickname must be set"});
subscribe_room(User, Nick, Room, Nodes) ->
    NodeList = re:split(Nodes, "\\h*,\\h*"),
    try jid:decode(Room) of
	#jid{luser = Name, lserver = Host} when Name /= <<"">> ->
	    try jid:decode(User) of
		UserJID1 ->
		    UserJID = jid:replace_resource(UserJID1, <<"modmucadmin">>),
		    case get_room_pid(Name, Host) of
			Pid when is_pid(Pid) ->
			    case mod_muc_room:subscribe(
				   Pid, UserJID, Nick, NodeList) of
				{ok, SubscribedNodes} ->
				    SubscribedNodes;
				{error, Reason} ->
				    throw({error, binary_to_list(Reason)})
			    end;
			_ ->
			    throw({error, "The room does not exist"})
		    end
	    catch _:{bad_jid, _} ->
		    throw({error, "Malformed user JID"})
	    end;
	_ ->
	    throw({error, "Malformed room JID"})
    catch _:{bad_jid, _} ->
	    throw({error, "Malformed room JID"})
    end.

unsubscribe_room(User, Room) ->
    try jid:decode(Room) of
	#jid{luser = Name, lserver = Host} when Name /= <<"">> ->
	    try jid:decode(User) of
		UserJID ->
		    case get_room_pid(Name, Host) of
			Pid when is_pid(Pid) ->
			    case mod_muc_room:unsubscribe(Pid, UserJID) of
				ok ->
				    ok;
				{error, Reason} ->
				    throw({error, binary_to_list(Reason)})
			    end;
			_ ->
			    throw({error, "The room does not exist"})
		    end
	    catch _:{bad_jid, _} ->
		    throw({error, "Malformed user JID"})
	    end;
	_ ->
	    throw({error, "Malformed room JID"})
    catch _:{bad_jid, _} ->
	    throw({error, "Malformed room JID"})
    end.

get_subscribers(Name, Host) ->
    case get_room_pid(Name, Host) of
	Pid when is_pid(Pid) ->
	    {ok, JIDList} = mod_muc_room:get_subscribers(Pid),
	    [jid:encode(jid:remove_resource(J)) || J <- JIDList];
	_ ->
	    throw({error, "The room does not exist"})
    end.

%% Copied from mod_muc_room.erl
get_config_opt_name(Pos) ->
    Fs = [config|record_info(fields, config)],
    lists:nth(Pos, Fs).
-define(MAKE_CONFIG_OPT(Opt),
        {get_config_opt_name(Opt), element(Opt, Config)}).
make_opts(StateData) ->
    Config = StateData#state.config,
    Subscribers = maps:fold(
                    fun(_LJID, Sub, Acc) ->
                            [{Sub#subscriber.jid,
                              Sub#subscriber.nick,
                              Sub#subscriber.nodes}|Acc]
                    end, [], StateData#state.subscribers),
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
     {captcha_whitelist,
      (?SETS):to_list((StateData#state.config)#config.captcha_whitelist)},
     {affiliations,
      maps:to_list(StateData#state.affiliations)},
     {subject, StateData#state.subject},
     {subject_author, StateData#state.subject_author},
     {subscribers, Subscribers}].


%%----------------------------
%% Utils
%%----------------------------

find_service(global) ->
    global;
find_service(ServerHost) ->
    hd(gen_mod:get_module_opt_hosts(ServerHost, mod_muc)).

find_services(Global) when Global == global;
			Global == <<"global">> ->
    lists:flatmap(
      fun(ServerHost) ->
	      case gen_mod:is_loaded(ServerHost, mod_muc) of
		  true ->
		      [find_service(ServerHost)];
		  false ->
		      []
	      end
      end, ejabberd_option:hosts());
find_services(Service) when is_binary(Service) ->
    [Service].

get_room_serverhost(Service) when is_binary(Service) ->
  ejabberd_router:host_of_route(Service).

find_host(ServerHost) ->
    hd(gen_mod:get_module_opt_hosts(ServerHost, mod_muc)).

find_hosts(Global) when Global == global;
			Global == <<"global">> ->
    lists:flatmap(
      fun(ServerHost) ->
	      case gen_mod:is_loaded(ServerHost, mod_muc) of
		  true ->
		      [find_host(ServerHost)];
		  false ->
		      []
	      end
      end, ejabberd_option:hosts());
find_hosts(ServerHost) ->
    case gen_mod:is_loaded(ServerHost, mod_muc) of
	true ->
	    [find_host(ServerHost)];
	false ->
	    []
    end.

mod_options(_) -> [].

mod_doc() ->
    #{desc =>
	  [?T("This module provides commands to administer local MUC "
	      "services and their MUC rooms. It also provides simple "
	      "WebAdmin pages to view the existing rooms."), "",
	   ?T("This module depends on 'mod_muc'.")]}.
