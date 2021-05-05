%%%----------------------------------------------------------------------
%%% File    : mod_muc_admin.erl
%%% Author  : Badlop <badlop@ono.com>
%%% Purpose : Tools for additional MUC administration
%%% Created : 8 Sep 2007 by Badlop <badlop@ono.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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
	 get_user_rooms/2, get_user_subscriptions/2, get_room_occupants/2,
	 get_room_occupants_number/2, send_direct_invitation/5,
	 change_room_option/4, get_room_options/2,
	 set_room_affiliation/4, get_room_affiliations/2, get_room_affiliation/3,
	 web_menu_main/2, web_page_main/2, web_menu_host/3,
	 subscribe_room/4, unsubscribe_room/2, get_subscribers/2,
	 web_page_host/3, mod_options/1, get_commands_spec/0, find_hosts/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
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
     #ejabberd_commands{name = get_user_subscriptions, tags = [muc],
			desc = "Get the list of rooms where this user is subscribed",
			note = "added in 21.04",
			module = ?MODULE, function = get_user_subscriptions,
		        args_desc = ["Username", "Server host"],
		        args_example = ["tom", "example.com"],
		        result_example = [{"room1@muc.example.com", "Tommy", ["mucsub:config"]}],
			args = [{user, binary}, {host, binary}],
		        result = {rooms,
                                  {list,
                                   {room,
                                    {tuple,
                                     [{roomjid, string},
                                      {usernick, string},
                                      {nodes, {list, {node, string}}}
                                     ]}}
                                  }}},

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
			longdesc = "Since ejabberd 20.10, this command is "
                        "asynchronous: the API call may return before the "
                        "server has send all the invitations.\n\n"
                        "Password and Message can also be: none. "
                        "Users JIDs are separated with : ",
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
    try {get_room_serverhost(Service), jid:decode(FromBinary)} of
	{ServerHost, From} ->
	    Lang = <<"en">>,
	    case mod_muc:iq_set_register_info(ServerHost, Service, From, Nick, Lang) of
		{result, undefined} -> ok;
		{error, #stanza_error{reason = 'conflict'}} ->
		    throw({error, "Nick already registered"});
		{error, _} ->
		    throw({error, "Database error"})
	    end
	catch
	error:{invalid_domain, _} ->
	    throw({error, "Invalid 'service'"});
	error:{unregistered_route, _} ->
	    throw({error, "Invalid 'service'"});
	error:{bad_jid, _} ->
	    throw({error, "Invalid 'jid'"});
	_ ->
	    throw({error, "Internal error"})
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

get_user_subscriptions(User, Server) ->
    Services = find_services(global),
    UserJid = jid:make(jid:nodeprep(User), jid:nodeprep(Server)),
    lists:flatmap(
      fun(ServerHost) ->
              {ok, Rooms} = mod_muc:get_subscribed_rooms(ServerHost, UserJid),
              [{jid:encode(RoomJid), UserNick, Nodes}
               || {RoomJid, UserNick, Nodes} <- Rooms]
      end, Services).

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
    {value, {_, Binary}} = lists:keysearch(<<"sort">>, 1, Q),
    Integer = list_to_integer(string:strip(binary_to_list(Binary), right, $/)),
    case Integer >= 0 of
	true -> {ok, {normal, Integer}};
	false -> {ok, {reverse, abs(Integer)}}
    end.

make_rooms_page(Host, Lang, {Sort_direction, Sort_column}) ->
    Service = find_service(Host),
    Rooms_names = get_online_rooms(Service),
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
	      ?T("Room title"),
	      ?T("Node")],
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
    Node = node(Pid),

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
     Title,
     Node}.

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
     Title,
     Node} = Room_info,
    [NameHost,
     integer_to_binary(Num_participants),
     Ts_last_message,
     misc:atom_to_binary(Public),
     misc:atom_to_binary(Persistent),
     misc:atom_to_binary(Logging),
     justcreated_to_binary(Just_created),
     Title,
     misc:atom_to_binary(Node)].

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

create_room_with_opts(Name1, Host1, ServerHost1, CustomRoomOpts) ->
    case {jid:nodeprep(Name1), jid:nodeprep(Host1), jid:nodeprep(ServerHost1)} of
	{error, _, _} ->
	    throw({error, "Invalid 'name'"});
	{_, error, _} ->
	    throw({error, "Invalid 'host'"});
	{_, _, error} ->
	    throw({error, "Invalid 'serverhost'"});
	{Name, Host, ServerHost} ->
	    case get_room_pid(Name, Host) of
		room_not_found ->
		    %% Get the default room options from the muc configuration
		    DefRoomOpts = mod_muc_opt:default_room_options(ServerHost),
		    %% Change default room options as required
		    FormattedRoomOpts = [format_room_option(Opt, Val) || {Opt, Val}<-CustomRoomOpts],
		    RoomOpts = lists:ukeymerge(1,
					       lists:keysort(1, FormattedRoomOpts),
					       lists:keysort(1, DefRoomOpts)),
		    case mod_muc:create_room(Host, Name, RoomOpts) of
			ok ->
			    ok;
			{error, _} ->
			    throw({error, "Unable to start room"})
		    end;
		_ ->
		    throw({error, "Room already exists"})
	    end
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
destroy_room(Name1, Service1) ->
    case {jid:nodeprep(Name1), jid:nodeprep(Service1)} of
	{error, _} ->
	    throw({error, "Invalid 'name'"});
	{_, error} ->
	    throw({error, "Invalid 'service'"});
	{Name, Service} ->
	    case get_room_pid(Name, Service) of
		room_not_found ->
		    throw({error, "Room doesn't exists"});
		invalid_service ->
		    throw({error, "Invalid 'service'"});
		Pid ->
		    mod_muc_room:destroy(Pid),
		    ok
	    end
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
    Rooms_all = get_all_rooms(Service),

    %% Decide which ones pass the requirements
    Rooms_pass = decide_rooms(Method, Rooms_all, Last_allowed),

    Num_rooms_all = length(Rooms_all),
    Num_rooms_pass = length(Rooms_pass),

    %% Perform the desired action for matching rooms
    act_on_rooms(Method, Action, Rooms_pass),

    {Num_rooms_all, Num_rooms_pass, Rooms_pass}.

%%---------------
%% Get info

get_online_rooms(ServiceArg) ->
    Hosts = find_services(ServiceArg),
    lists:flatmap(
      fun(Host) ->
	  ServerHost = get_room_serverhost(Host),
	  [{RoomName, RoomHost, ServerHost, Pid}
	   || {RoomName, RoomHost, Pid} <- mod_muc:get_online_rooms(Host)]
      end, Hosts).

get_all_rooms(Host) ->
    ServerHost = ejabberd_router:host_of_route(Host),
    OnlineRooms = get_online_rooms(Host),
    OnlineMap = lists:foldl(
	fun({Room, _, _, _}, Map) ->
	    Map#{Room => 1}
	end, #{}, OnlineRooms),

    Mod = gen_mod:db_mod(ServerHost, mod_muc),
    DbRooms =
    case erlang:function_exported(Mod, get_rooms_without_subscribers, 2) of
	true ->
	    Mod:get_rooms_without_subscribers(ServerHost, Host);
	_ ->
	    Mod:get_rooms(ServerHost, Host)
    end,
    StoredRooms = lists:filtermap(
	fun(#muc_room{name_host = {Room, _}, opts = Opts}) ->
	    case maps:is_key(Room, OnlineMap) of
		true ->
		    false;
		_ ->
		    {true, {Room, Host, ServerHost, Opts}}
	    end
	end, DbRooms),
    OnlineRooms ++ StoredRooms.

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
    NodeStartTime = erlang:system_time(microsecond) -
		    1000000*(erlang:monotonic_time(second)-ejabberd_config:get_node_start()),
    OnlyHibernated = case mod_muc_opt:hibernation_timeout(ServerHost) of
	Value when Value < Last_allowed*24*60*60*1000 ->
	    true;
	_ ->
	    false
	end,
    {Just_created, Num_users} =
    case Room_pid of
	Pid when is_pid(Pid) andalso OnlyHibernated ->
	    {erlang:system_time(microsecond), 0};
	Pid when is_pid(Pid) ->
	    case mod_muc_room:get_state(Room_pid) of
		{ok, #state{just_created = JC, users = U}} ->
		    {JC, maps:size(U)};
		_ ->
		    {erlang:system_time(microsecond), 0}
	    end;
	Opts ->
	    case lists:keyfind(hibernation_time, 1, Opts) of
		false ->
		    {NodeStartTime, 0};
		{_, T} ->
		    {T, 0}
	    end
    end,
    Last = case Just_created of
	       true ->
		   0;
	       _ ->
		   (erlang:system_time(microsecond)
		    - Just_created) div 1000000
	   end,
    case {Num_users, seconds_to_days(Last)} of
	{0, Last_days} when (Last_days >= Last_allowed) ->
	    true;
	_ ->
	    false
    end;
decide_room(empty, {Room_name, Host, ServerHost, Room_pid}, _Last_allowed) ->
    case gen_mod:is_loaded(ServerHost, mod_mam) of
	true ->
	    Room_options = case Room_pid of
			       _ when is_pid(Room_pid) ->
				   get_room_options(Room_pid);
			       Opts ->
				   Opts
			   end,
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

act_on_room(Method, destroy, {N, H, _SH, Pid}) ->
    Message = iolist_to_binary(io_lib:format(
        <<"Room destroyed by rooms_~s_destroy.">>, [Method])),
    case Pid of
	V when is_pid(V) ->
	    mod_muc_room:destroy(Pid, Message);
	_ ->
	    case get_room_pid(N, H) of
		Pid2 when is_pid(Pid2) ->
		    mod_muc_room:destroy(Pid2, Message);
		_ ->
		    ok
	    end
    end;
act_on_room(_Method, list, _) ->
    ok.


%%----------------------------
%% Change Room Option
%%----------------------------

get_room_occupants(Room, Host) ->
    case get_room_pid(Room, Host) of
	Pid when is_pid(Pid) -> get_room_occupants(Pid);
	_ -> throw({error, room_not_found})
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
	Pid when is_pid(Pid )->
	    S = get_room_state(Pid),
	    maps:size(S#state.users);
	_ ->
	    throw({error, room_not_found})
    end.

%%----------------------------
%% Send Direct Invitation
%%----------------------------
%% http://xmpp.org/extensions/xep-0249.html

send_direct_invitation(RoomName, RoomService, Password, Reason, UsersString) ->
    case jid:make(RoomName, RoomService) of
	error ->
	    throw({error, "Invalid 'roomname' or 'service'"});
	RoomJid ->
	    XmlEl = build_invitation(Password, Reason, RoomJid),
	    Users = get_users_to_invite(RoomJid, UsersString),
	    [send_direct_invitation(RoomJid, UserJid, XmlEl)
	     || UserJid <- Users],
	    ok
    end.

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
%% `change_room_option(<<"testroom">>, <<"conference.localhost">>, <<"title">>, <<"Test Room">>)'
change_room_option(Name, Service, OptionString, ValueString) ->
    case get_room_pid(Name, Service) of
	room_not_found ->
	    throw({error, "Room not found"});
	invalid_service ->
	    throw({error, "Invalid 'service'"});
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
-spec get_room_pid(binary(), binary()) -> pid() | room_not_found | invalid_service.
get_room_pid(Name, Service) ->
    try get_room_serverhost(Service) of
	ServerHost ->
	    case mod_muc:unhibernate_room(ServerHost, Service, Name) of
		error ->
		    room_not_found;
		{ok, Pid} ->
		    Pid
	    end
    catch
	error:{invalid_domain, _} ->
	    invalid_service;
	error:{unregistered_route, _} ->
	    invalid_service
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
        Pid when is_pid(Pid) -> get_room_options(Pid);
	_ -> []
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
    case get_room_pid(Name, Service) of
	Pid when is_pid(Pid) ->
	    %% Get the PID of the online room, then request its state
	    {ok, StateData} = mod_muc_room:get_state(Pid),
	    Affiliations = maps:to_list(StateData#state.affiliations),
	    lists:map(
	      fun({{Uname, Domain, _Res}, {Aff, Reason}}) when is_atom(Aff)->
		      {Uname, Domain, Aff, Reason};
		 ({{Uname, Domain, _Res}, Aff}) when is_atom(Aff)->
		      {Uname, Domain, Aff, <<>>}
	      end, Affiliations);
	_ ->
	    throw({error, "The room does not exist."})
    end.

%%----------------------------
%% Get Room Affiliation
%%----------------------------

%% @spec(Name::binary(), Service::binary(), JID::binary()) ->
%%    {Affiliation::string()}
%% @doc Get affiliation of a user in the room Name@Service.

get_room_affiliation(Name, Service, JID) ->
	case get_room_pid(Name, Service) of
	    Pid when is_pid(Pid) ->
		%% Get the PID of the online room, then request its state
		{ok, StateData} = mod_muc_room:get_state(Pid),
		UserJID = jid:decode(JID),
		mod_muc_room:get_affiliation(UserJID, StateData);
	    _ ->
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
    Affiliation = case AffiliationString of
                      <<"outcast">> -> outcast;
                      <<"none">> -> none;
                      <<"member">> -> member;
                      <<"admin">> -> admin;
                      <<"owner">> -> owner;
                      _ ->
                          throw({error, "Invalid affiliation"})
                  end,
    case get_room_pid(Name, Service) of
	Pid when is_pid(Pid) ->
	    %% Get the PID for the online room so we can get the state of the room
	    case mod_muc_room:change_item(Pid, jid:decode(JID), affiliation, Affiliation, <<"">>) of
		{ok, _} ->
		    ok;
		{error, notfound} ->
		    throw({error, "Room doesn't exists"});
		{error, _} ->
		    throw({error, "Unable to perform change"})
	    end;
	room_not_found ->
	    throw({error, "Room doesn't exists"});
	invalid_service ->
	    throw({error, "Invalid 'service'"})
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
