%%%----------------------------------------------------------------------
%%% File    : mod_muc_admin.erl
%%% Author  : Badlop <badlop@ono.com>
%%% Purpose : Tools for additional MUC administration
%%% Created : 8 Sep 2007 by Badlop <badlop@ono.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
	 rooms_empty_list/1, rooms_empty_destroy/1, rooms_empty_destroy_restuple/1,
	 get_user_rooms/2, get_user_subscriptions/2, get_room_occupants/2,
	 get_room_occupants_number/2, send_direct_invitation/5,
	 change_room_option/4, get_room_options/2,
	 set_room_affiliation/4, get_room_affiliations/2, get_room_affiliation/3,
	 subscribe_room/4, subscribe_room_many/3,
	 unsubscribe_room/2, get_subscribers/2,
	 get_room_serverhost/1,
	 web_menu_main/2, web_page_main/2,
         web_menu_host/3, web_page_host/3,
         web_menu_hostuser/4, web_page_hostuser/4,
         webadmin_muc/2,
	 mod_opt_type/1, mod_options/1,
	 get_commands_spec/0, find_hosts/1, room_diagnostics/2,
	 get_room_pid/2, get_room_history/2]).

-import(ejabberd_web_admin, [make_command/4, make_command_raw_value/3, make_table/4]).

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

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(?MODULE, get_commands_spec()),
    {ok, [{hook, webadmin_menu_main, web_menu_main, 50, global},
	  {hook, webadmin_page_main, web_page_main, 50, global},
	  {hook, webadmin_menu_host, web_menu_host, 50},
	  {hook, webadmin_page_host, web_page_host, 50},
	  {hook, webadmin_menu_hostuser, web_menu_hostuser, 50},
	  {hook, webadmin_page_hostuser, web_page_hostuser, 50}
         ]}.

stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(get_commands_spec());
        true ->
            ok
    end.

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
		       desc = "List existing rooms",
		       longdesc = "Ask for a specific host, or `global` to use all vhosts.",
                       policy = admin,
		       module = ?MODULE, function = muc_online_rooms,
		       args_desc = ["MUC service, or `global` for all"],
		       args_example = ["conference.example.com"],
		       result_desc = "List of rooms",
		       result_example = ["room1@conference.example.com", "room2@conference.example.com"],
		       args = [{service, binary}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},
	#ejabberd_commands{name = muc_online_rooms_by_regex, tags = [muc],
		       desc = "List existing rooms filtered by regexp",
		       longdesc = "Ask for a specific host, or `global` to use all vhosts.",
                       policy = admin,
		       module = ?MODULE, function = muc_online_rooms_by_regex,
		       args_desc = ["MUC service, or `global` for all",
				    "Regex pattern for room name"],
		       args_example = ["conference.example.com", "^prefix"],
		       result_desc = "List of rooms with summary",
		       result_example = [{"room1@conference.example.com", "true", 10},
					 {"room2@conference.example.com", "false", 10}],
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
		       args_example = [<<"Tim">>, <<"tim@example.org">>, <<"conference.example.org">>],
		       args = [{nick, binary}, {jid, binary}, {service, binary}],
		       args_rename = [{host, service}],
		       result = {res, rescode}},
     #ejabberd_commands{name = muc_unregister_nick, tags = [muc],
		       desc = "Unregister the nick registered by that account in the MUC service",
		       module = ?MODULE, function = muc_unregister_nick,
		       args_desc = ["User JID", "MUC service"],
		       args_example = [<<"tim@example.org">>, <<"conference.example.org">>],
		       args = [{jid, binary}, {service, binary}],
		       args_rename = [{host, service}],
		       result = {res, rescode}},

     #ejabberd_commands{name = create_room, tags = [muc_room],
		       desc = "Create a MUC room name@service in host",
		       module = ?MODULE, function = create_room,
		       args_desc = ["Room name", "MUC service", "Server host"],
		       args_example = ["room1", "conference.example.com", "example.com"],
		       args = [{name, binary}, {service, binary},
			       {host, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = destroy_room, tags = [muc_room],
		       desc = "Destroy a MUC room",
		       module = ?MODULE, function = destroy_room,
		       args_desc = ["Room name", "MUC service"],
		       args_example = ["room1", "conference.example.com"],
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
     #ejabberd_commands{name = create_room_with_opts, tags = [muc_room, muc_sub],
		       desc = "Create a MUC room name@service in host with given options",
		       longdesc =
                        "The syntax of `affiliations` is: `Type:JID,Type:JID`. "
                        "The syntax of `subscribers` is: `JID:Nick:Node:Node2:Node3,JID:Nick:Node`.",
		       module = ?MODULE, function = create_room_with_opts,
		       args_desc = ["Room name", "MUC service", "Server host", "List of options"],
		       args_example = ["room1", "conference.example.com", "localhost",
				       [{"members_only","true"},
                                        {"affiliations", "owner:bob@example.com,member:peter@example.com"},
                                        {"subscribers", "bob@example.com:Bob:messages:subject,anne@example.com:Anne:messages"}]],
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
			    " The MUC service argument can be `global` to get all hosts.",
		       module = ?MODULE, function = rooms_unused_list,
		       args_desc = ["MUC service, or `global` for all", "Number of days"],
		       args_example = ["conference.example.com", 31],
		       result_desc = "List of unused rooms",
		       result_example = ["room1@conference.example.com", "room2@conference.example.com"],
		       args = [{service, binary}, {days, integer}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},
     #ejabberd_commands{name = rooms_unused_destroy, tags = [muc],
		       desc = "Destroy the rooms that are unused for many days in the service",
		       longdesc = "The room recent history is used, so it's recommended "
			    " to wait a few days after service start before running this."
			    " The MUC service argument can be `global` to get all hosts.",
		       module = ?MODULE, function = rooms_unused_destroy,
		       args_desc = ["MUC service, or `global` for all", "Number of days"],
		       args_example = ["conference.example.com", 31],
		       result_desc = "List of unused rooms that has been destroyed",
		       result_example = ["room1@conference.example.com", "room2@conference.example.com"],
		       args = [{service, binary}, {days, integer}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},

     #ejabberd_commands{name = rooms_empty_list, tags = [muc],
		       desc = "List the rooms that have no messages in archive",
		       longdesc = "The MUC service argument can be `global` to get all hosts.",
		       module = ?MODULE, function = rooms_empty_list,
		       args_desc = ["MUC service, or `global` for all"],
		       args_example = ["conference.example.com"],
		       result_desc = "List of empty rooms",
		       result_example = ["room1@conference.example.com", "room2@conference.example.com"],
		       args = [{service, binary}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},
     #ejabberd_commands{name = rooms_empty_destroy, tags = [muc],
		       desc = "Destroy the rooms that have no messages in archive",
		       longdesc = "The MUC service argument can be `global` to get all hosts.",
		       module = ?MODULE, function = rooms_empty_destroy,
		       args_desc = ["MUC service, or `global` for all"],
		       args_example = ["conference.example.com"],
		       result_desc = "List of empty rooms that have been destroyed",
		       result_example = ["room1@conference.example.com", "room2@conference.example.com"],
		       args = [{service, binary}],
		       args_rename = [{host, service}],
		       result = {rooms, {list, {room, string}}}},
     #ejabberd_commands{name = rooms_empty_destroy, tags = [muc],
		       desc = "Destroy the rooms that have no messages in archive",
		       longdesc = "The MUC service argument can be `global` to get all hosts.",
		       module = ?MODULE, function = rooms_empty_destroy_restuple,
                       version = 2,
                       note = "modified in 24.06",
		       args_desc = ["MUC service, or `global` for all"],
		       args_example = ["conference.example.com"],
		       result_desc = "List of empty rooms that have been destroyed",
		       result_example = {ok, <<"Destroyed rooms: 2">>},
		       args = [{service, binary}],
		       args_rename = [{host, service}],
		       result = {res, restuple}},

     #ejabberd_commands{name = get_user_rooms, tags = [muc],
			desc = "Get the list of rooms where this user is occupant",
			module = ?MODULE, function = get_user_rooms,
		        args_desc = ["Username", "Server host"],
		        args_example = ["tom", "example.com"],
		        result_example = ["room1@conference.example.com", "room2@conference.example.com"],
			args = [{user, binary}, {host, binary}],
		        result = {rooms, {list, {room, string}}}},
     #ejabberd_commands{name = get_user_subscriptions, tags = [muc, muc_sub],
			desc = "Get the list of rooms where this user is subscribed",
			note = "added in 21.04",
			module = ?MODULE, function = get_user_subscriptions,
		        args_desc = ["Username", "Server host"],
		        args_example = ["tom", "example.com"],
		        result_example = [{"room1@conference.example.com", "Tommy", ["mucsub:config"]}],
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
		        args_example = ["room1", "conference.example.com"],
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
		        args_example = ["room1", "conference.example.com"],
		        result_desc = "Number of room occupants",
		        result_example = 7,
			args = [{name, binary}, {service, binary}],
			result = {occupants, integer}},

     #ejabberd_commands{name = send_direct_invitation, tags = [muc_room],
			desc = "Send a direct invitation to several destinations",
			longdesc = "Since ejabberd 20.12, this command is "
                        "asynchronous: the API call may return before the "
                        "server has send all the invitations.\n\n"
                        "Password and Message can also be: `none`. "
                        "Users JIDs are separated with `:`.",
			module = ?MODULE, function = send_direct_invitation,
		        args_desc = ["Room name", "MUC service", "Password, or `none`",
			 "Reason text, or `none`", "Users JIDs separated with `:` characters"],
			args_example = [<<"room1">>, <<"conference.example.com">>,
					<<>>, <<"Check this out!">>,
					"user2@localhost:user3@example.com"],
			args = [{name, binary}, {service, binary}, {password, binary},
				{reason, binary}, {users, binary}],
		        result = {res, rescode}},
     #ejabberd_commands{name = send_direct_invitation, tags = [muc_room],
			desc = "Send a direct invitation to several destinations",
			longdesc = "Since ejabberd 20.12, this command is "
                        "asynchronous: the API call may return before the "
                        "server has send all the invitations.\n\n"
                        "`password` and `message` can be set to `none`.",
			module = ?MODULE, function = send_direct_invitation,
			version = 1,
			note = "updated in 24.02",
		        args_desc = ["Room name", "MUC service", "Password, or `none`",
			 "Reason text, or `none`", "List of users JIDs"],
			args_example = [<<"room1">>, <<"conference.example.com">>,
					<<>>, <<"Check this out!">>,
					["user2@localhost", "user3@example.com"]],
			args = [{name, binary}, {service, binary}, {password, binary},
				{reason, binary}, {users, {list, {jid, binary}}}],
		        result = {res, rescode}},

     #ejabberd_commands{name = change_room_option, tags = [muc_room],
		       desc = "Change an option in a MUC room",
		       module = ?MODULE, function = change_room_option,
		       args_desc = ["Room name", "MUC service", "Option name", "Value to assign"],
		       args_example = ["room1", "conference.example.com", "members_only", "true"],
		       args = [{name, binary}, {service, binary},
			       {option, binary}, {value, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = get_room_options, tags = [muc_room],
		        desc = "Get options from a MUC room",
		        module = ?MODULE, function = get_room_options,
		        args_desc = ["Room name", "MUC service"],
		        args_example = ["room1", "conference.example.com"],
		        result_desc = "List of room options tuples with name and value",
		        result_example = [{"members_only", "true"}],
		        args = [{name, binary}, {service, binary}],
			result = {options, {list,
						 {option, {tuple,
								[{name, string},
								 {value, string}
								]}}
						}}},
     #ejabberd_commands{name = subscribe_room, tags = [muc_room, muc_sub],
			desc = "Subscribe to a MUC conference",
			module = ?MODULE, function = subscribe_room,
			args_desc = ["User JID", "a user's nick",
			    "the room to subscribe", "nodes separated by commas: `,`"],
			args_example = ["tom@localhost", "Tom", "room1@conference.localhost",
			    "urn:xmpp:mucsub:nodes:messages,urn:xmpp:mucsub:nodes:affiliations"],
			result_desc = "The list of nodes that has subscribed",
			result_example = ["urn:xmpp:mucsub:nodes:messages",
			    "urn:xmpp:mucsub:nodes:affiliations"],
			args = [{user, binary}, {nick, binary}, {room, binary},
				{nodes, binary}],
			result = {nodes, {list, {node, string}}}},
     #ejabberd_commands{name = subscribe_room, tags = [muc_room, muc_sub],
			desc = "Subscribe to a MUC conference",
			module = ?MODULE, function = subscribe_room,
			version = 1,
			note = "updated in 24.02",
			args_desc = ["User JID", "a user's nick",
			    "the room to subscribe", "list of nodes"],
			args_example = ["tom@localhost", "Tom", "room1@conference.localhost",
			    ["urn:xmpp:mucsub:nodes:messages", "urn:xmpp:mucsub:nodes:affiliations"]],
			result_desc = "The list of nodes that has subscribed",
			result_example = ["urn:xmpp:mucsub:nodes:messages",
			    "urn:xmpp:mucsub:nodes:affiliations"],
			args = [{user, binary}, {nick, binary}, {room, binary},
				{nodes, {list, {node, binary}}}],
			result = {nodes, {list, {node, string}}}},
     #ejabberd_commands{name = subscribe_room_many, tags = [muc_room, muc_sub],
			desc = "Subscribe several users to a MUC conference",
			note = "added in 22.05",
			longdesc = "This command accepts up to 50 users at once "
                            "(this is configurable with the _`mod_muc_admin`_ option "
                            "`subscribe_room_many_max_users`)",
			module = ?MODULE, function = subscribe_room_many,
			args_desc = ["Users JIDs and nicks",
                                     "the room to subscribe",
                                     "nodes separated by commas: `,`"],
			args_example = [[{"tom@localhost", "Tom"},
                                         {"jerry@localhost", "Jerry"}],
                                        "room1@conference.localhost",
                                        "urn:xmpp:mucsub:nodes:messages,urn:xmpp:mucsub:nodes:affiliations"],
			args = [{users, {list,
                                         {user, {tuple,
                                                 [{jid, binary},
                                                  {nick, binary}
                                                 ]}}
                                        }},
                                {room, binary},
				{nodes, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = subscribe_room_many, tags = [muc_room, muc_sub],
			desc = "Subscribe several users to a MUC conference",
			longdesc = "This command accepts up to 50 users at once "
                            "(this is configurable with the _`mod_muc_admin`_ option "
                            "`subscribe_room_many_max_users`)",
			module = ?MODULE, function = subscribe_room_many,
			version = 1,
			note = "updated in 24.02",
			args_desc = ["Users JIDs and nicks",
                                     "the room to subscribe",
                                     "nodes separated by commas: `,`"],
			args_example = [[{"tom@localhost", "Tom"},
                                         {"jerry@localhost", "Jerry"}],
                                        "room1@conference.localhost",
                                        ["urn:xmpp:mucsub:nodes:messages", "urn:xmpp:mucsub:nodes:affiliations"]],
			args = [{users, {list,
                                         {user, {tuple,
                                                 [{jid, binary},
                                                  {nick, binary}
                                                 ]}}
                                        }},
                                {room, binary},
				{nodes, {list, {node, binary}}}],
			result = {res, rescode}},
     #ejabberd_commands{name = unsubscribe_room, tags = [muc_room, muc_sub],
			desc = "Unsubscribe from a MUC conference",
			module = ?MODULE, function = unsubscribe_room,
			args_desc = ["User JID", "the room to subscribe"],
			args_example = ["tom@localhost", "room1@conference.localhost"],
			args = [{user, binary}, {room, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = get_subscribers, tags = [muc_room, muc_sub],
			desc = "List subscribers of a MUC conference",
			module = ?MODULE, function = get_subscribers,
		        args_desc = ["Room name", "MUC service"],
		        args_example = ["room1", "conference.example.com"],
		        result_desc = "The list of users that are subscribed to that room",
		        result_example = ["user2@example.com", "user3@example.com"],
			args = [{name, binary}, {service, binary}],
			result = {subscribers, {list, {jid, string}}}},
     #ejabberd_commands{name = set_room_affiliation, tags = [muc_room],
		       desc = "Change an affiliation in a MUC room",
		       module = ?MODULE, function = set_room_affiliation,
		       args_desc = ["Room name", "MUC service", "User JID", "Affiliation to set"],
		       args_example = ["room1", "conference.example.com", "user2@example.com", "member"],
		       args = [{name, binary}, {service, binary},
			       {jid, binary}, {affiliation, binary}],
		       result = {res, rescode}},
     #ejabberd_commands{name = get_room_affiliations, tags = [muc_room],
			desc = "Get the list of affiliations of a MUC room",
			module = ?MODULE, function = get_room_affiliations,
		        args_desc = ["Room name", "MUC service"],
		        args_example = ["room1", "conference.example.com"],
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
			args_example = ["room1", "conference.example.com", "user1@example.com"],
			result_desc = "Affiliation of the user",
			result_example = member,
			args = [{name, binary}, {service, binary}, {jid, binary}],
			result = {affiliation, atom}},
         #ejabberd_commands{name = get_room_history, tags = [muc_room],
			desc = "Get history of messages stored inside MUC room state",
			note = "added in 23.04",
			module = ?MODULE, function = get_room_history,
			args_desc = ["Room name", "MUC service"],
			args_example = ["room1", "conference.example.com"],
			args = [{name, binary}, {service, binary}],
			result = {history, {list,
					    {entry, {tuple,
						     [{timestamp, string},
						      {message, string}]}}}}},

         #ejabberd_commands{name = webadmin_muc, tags = [internal],
			desc = "Generate WebAdmin MUC Rooms HTML",
			module = ?MODULE, function = webadmin_muc,
			args = [{request, any}, {lang, binary}],
			result = {res, any}}
	].


%%%
%%% ejabberd commands
%%%

muc_online_rooms(ServiceArg) ->
    Hosts = find_services_validate(ServiceArg, <<"serverhost">>),
    lists:flatmap(
      fun(Host) ->
	      [<<Name/binary, "@", Host/binary>>
	       || {Name, _, _} <- mod_muc:get_online_rooms(Host)]
      end, Hosts).

muc_online_rooms_by_regex(ServiceArg, Regex) ->
    {_, P} = re:compile(Regex),
    Hosts = find_services_validate(ServiceArg, <<"serverhost">>),
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
	    throw({error, "Invalid value of 'service'"});
	error:{unregistered_route, _} ->
	    throw({error, "Unknown host in 'service'"});
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
    User2 = validate_user(User, <<"user">>),
    Server2 = validate_host(Server, <<"host">>),
    Services = find_services(global),
    UserJid = jid:make(User2, Server2),
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

%% @format-begin

%%---------------
%% Web Admin Menu

web_menu_main(Acc, Lang) ->
    Acc ++ [{<<"muc">>, translate:translate(Lang, ?T("Multi-User Chat"))}].

web_menu_host(Acc, _Host, Lang) ->
    Acc ++ [{<<"muc">>, translate:translate(Lang, ?T("Multi-User Chat"))}].

%%---------------
%% Web Admin Page

web_page_main(_, #request{path = [<<"muc">>], lang = Lang} = R) ->
    PageTitle = translate:translate(Lang, ?T("Multi-User Chat")),
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Res = [make_command(webadmin_muc, R, [{<<"request">>, R}, {<<"lang">>, Lang}], [])],
    {stop, Title ++ Res};
web_page_main(Acc, _) ->
    Acc.

web_page_host(_, Host, #request{path = [<<"muc">> | RPath], lang = Lang} = R) ->
    PageTitle = translate:translate(Lang, ?T("Multi-User Chat")),
    Service = find_service(Host),
    Level = length(RPath),
    Res = webadmin_muc_host(Host, Service, RPath, R, Lang, Level, PageTitle),
    {stop, Res};
web_page_host(Acc, _, _) ->
    Acc.

%%---------------
%% WebAdmin MUC Host Page

webadmin_muc_host(Host,
                  Service,
                  [<<"create-room">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb = make_breadcrumb({service_section, Level, Service, <<"Create Room">>, RPath}),
    Set = [make_command(create_room, R, [{<<"service">>, Service}, {<<"host">>, Host}], []),
           make_command(create_room_with_opts,
                        R,
                        [{<<"service">>, Service}, {<<"host">>, Host}],
                        [])],
    Title ++ Breadcrumb ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"nick-register">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({service_section, Level, Service, <<"Nick Register">>, RPath}),
    Set = [make_command(muc_register_nick, R, [{<<"service">>, Service}], []),
           make_command(muc_unregister_nick, R, [{<<"service">>, Service}], [])],
    Title ++ Breadcrumb ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms-empty">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb = make_breadcrumb({service_section, Level, Service, <<"Rooms Empty">>, RPath}),
    Set = [make_command(rooms_empty_list,
                        R,
                        [{<<"service">>, Service}],
                        [{table_options, {2, RPath}},
                         {result_links, [{room, room, 3 + Level, <<"">>}]}]),
           make_command(rooms_empty_destroy, R, [{<<"service">>, Service}], [])],
    Title ++ Breadcrumb ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms-unused">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({service_section, Level, Service, <<"Rooms Unused">>, RPath}),
    Set = [make_command(rooms_unused_list,
                        R,
                        [{<<"service">>, Service}],
                        [{result_links, [{room, room, 3 + Level, <<"">>}]}]),
           make_command(rooms_unused_destroy, R, [{<<"service">>, Service}], [])],
    Title ++ Breadcrumb ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms-regex">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({service_section, Level, Service, <<"Rooms by Regex">>, RPath}),
    Set = [make_command(muc_online_rooms_by_regex,
                        R,
                        [{<<"service">>, Service}],
                        [{result_links, [{jid, room, 3 + Level, <<"">>}]}])],
    Title ++ Breadcrumb ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms">>, <<"room">>, Name, <<"affiliations">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({room_section, Level, Service, <<"Affiliations">>, Name, R, RPath}),
    Set = [make_command(set_room_affiliation,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [])],
    Get = [make_command(get_room_affiliations,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [{table_options, {20, RPath}}])],
    Title ++ Breadcrumb ++ Get ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms">>, <<"room">>, Name, <<"history">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({room_section, Level, Service, <<"History">>, Name, R, RPath}),
    Get = [make_command(get_room_history,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [{table_options, {10, RPath}},
                         {result_links, [{message, paragraph, 1, <<"">>}]}])],
    Title ++ Breadcrumb ++ Get;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms">>, <<"room">>, Name, <<"invite">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({room_section, Level, Service, <<"Invite">>, Name, R, RPath}),
    Set = [make_command(send_direct_invitation,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [])],
    Title ++ Breadcrumb ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms">>, <<"room">>, Name, <<"occupants">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({room_section, Level, Service, <<"Occupants">>, Name, R, RPath}),
    Get = [make_command(get_room_occupants,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [{table_options, {20, RPath}},
                         {result_links, [{jid, user, 3 + Level, <<"">>}]}])],
    Title ++ Breadcrumb ++ Get;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms">>, <<"room">>, Name, <<"options">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({room_section, Level, Service, <<"Options">>, Name, R, RPath}),
    Set = [make_command(change_room_option,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [])],
    Get = [make_command(get_room_options,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [])],
    Title ++ Breadcrumb ++ Get ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms">>, <<"room">>, Name, <<"subscribers">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title =
        ?H1GLraw(PageTitle,
                 <<"developer/xmpp-clients-bots/extensions/muc-sub/">>,
                 <<"MUC/Sub Extension">>),
    Breadcrumb =
        make_breadcrumb({room_section, Level, Service, <<"Subscribers">>, Name, R, RPath}),
    Set = [make_command(subscribe_room,
                        R,
                        [{<<"room">>, jid:encode({Name, Service, <<"">>})}],
                        []),
           make_command(unsubscribe_room,
                        R,
                        [{<<"room">>, jid:encode({Name, Service, <<"">>})}],
                        [{style, danger}])],
    Get = [make_command(get_subscribers,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [{table_options, {20, RPath}},
                         {result_links, [{jid, user, 3 + Level, <<"">>}]}])],
    Title ++ Breadcrumb ++ Get ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms">>, <<"room">>, Name, <<"destroy">> | RPath],
                  R,
                  _Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb =
        make_breadcrumb({room_section, Level, Service, <<"Destroy">>, Name, R, RPath}),
    Set = [make_command(destroy_room,
                        R,
                        [{<<"name">>, Name}, {<<"service">>, Service}],
                        [{style, danger}])],
    Title ++ Breadcrumb ++ Set;
webadmin_muc_host(_Host,
                  Service,
                  [<<"rooms">>, <<"room">>, Name | _RPath],
                  _R,
                  Lang,
                  Level,
                  PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb = make_breadcrumb({room, Level, Service, Name}),
    MenuItems =
        [{<<"affiliations/">>, <<"Affiliations">>},
         {<<"history/">>, <<"History">>},
         {<<"invite/">>, <<"Invite">>},
         {<<"occupants/">>, <<"Occupants">>},
         {<<"options/">>, <<"Options">>},
         {<<"subscribers/">>, <<"Subscribers">>},
         {<<"destroy/">>, <<"Destroy">>}],
    Get = [?XE(<<"ul">>, [?LI([?ACT(MIU, MIN)]) || {MIU, MIN} <- MenuItems])],
    Title ++ Breadcrumb ++ Get;
webadmin_muc_host(_Host, Service, [<<"rooms">> | RPath], R, _Lang, Level, PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb = make_breadcrumb({service_section, Level, Service, <<"Rooms">>, RPath}),
    Columns = [<<"jid">>, <<"occupants">>],
    Rows =
        lists:map(fun(NameService) ->
                     #jid{user = Name} = jid:decode(NameService),
                     {make_command(echo,
                                   R,
                                   [{<<"sentence">>, jid:encode({Name, Service, <<"">>})}],
                                   [{only, raw_and_value},
                                    {result_links, [{sentence, room, 3 + Level, <<"">>}]}]),
                      make_command(get_room_occupants_number,
                                   R,
                                   [{<<"name">>, Name}, {<<"service">>, Service}],
                                   [{only, raw_and_value}])}
                  end,
                  make_command_raw_value(muc_online_rooms, R, [{<<"service">>, Service}])),
    Get = [make_command(muc_online_rooms, R, [], [{only, presentation}]),
           make_command(get_room_occupants_number, R, [], [{only, presentation}]),
           make_table(20, RPath, Columns, Rows)],
    Title ++ Breadcrumb ++ Get;
webadmin_muc_host(_Host, Service, [], _R, Lang, _Level, PageTitle) ->
    Title = ?H1GL(PageTitle, <<"modules/#mod_muc">>, <<"mod_muc">>),
    Breadcrumb = make_breadcrumb({service, Service}),
    MenuItems =
        [{<<"create-room/">>, <<"Create Room">>},
         {<<"rooms/">>, <<"Rooms">>},
         {<<"rooms-regex/">>, <<"Rooms by Regex">>},
         {<<"rooms-empty/">>, <<"Rooms Empty">>},
         {<<"rooms-unused/">>, <<"Rooms Unused">>},
         {<<"nick-register/">>, <<"Nick Register">>}],
    Get = [?XE(<<"ul">>, [?LI([?ACT(MIU, MIN)]) || {MIU, MIN} <- MenuItems])],
    Title ++ Breadcrumb ++ Get;
webadmin_muc_host(_Host, _Service, _RPath, _R, _Lang, _Level, _PageTitle) ->
    [].

make_breadcrumb({service, Service}) ->
    make_breadcrumb([Service]);
make_breadcrumb({service_section, Level, Service, Section, RPath}) ->
    make_breadcrumb([{Level, Service}, separator, Section | RPath]);
make_breadcrumb({room, Level, Service, Name}) ->
    make_breadcrumb([{Level, Service},
                     separator,
                     {Level - 1, <<"Rooms">>},
                     separator,
                     jid:encode({Name, Service, <<"">>})]);
make_breadcrumb({room_section, Level, Service, Section, Name, R, RPath}) ->
    make_breadcrumb([{Level, Service},
                     separator,
                     {Level - 1, <<"Rooms">>},
                     separator,
                     make_command(echo,
                                  R,
                                  [{<<"sentence">>, jid:encode({Name, Service, <<"">>})}],
                                  [{only, value},
                                   {result_links, [{sentence, room, 3 + Level, <<"">>}]}]),
                     separator,
                     Section
                     | RPath]);
make_breadcrumb(Elements) ->
    lists:map(fun ({xmlel, _, _, _} = Xmlel) ->
                      Xmlel;
                  (<<"sort">>) ->
                      ?C(<<" +">>);
                  (<<"page">>) ->
                      ?C(<<" #">>);
                  (separator) ->
                      ?C(<<" > ">>);
                  (Bin) when is_binary(Bin) ->
                      ?C(Bin);
                  ({Level, Bin}) when is_integer(Level) and is_binary(Bin) ->
                      ?AC(binary:copy(<<"../">>, Level), Bin)
              end,
              Elements).

%%---------------
%%

%% Returns: {normal | reverse, Integer}
get_sort_query(Q) ->
    case catch get_sort_query2(Q) of
        {ok, Res} ->
            Res;
        _ ->
            {normal, 1}
    end.

get_sort_query2(Q) ->
    {value, {_, Binary}} = lists:keysearch(<<"sort">>, 1, Q),
    Integer = list_to_integer(string:strip(binary_to_list(Binary), right, $/)),
    case Integer >= 0 of
        true ->
            {ok, {normal, Integer}};
        false ->
            {ok, {reverse, abs(Integer)}}
    end.

webadmin_muc(#request{q = Q} = R, Lang) ->
    {Sort_direction, Sort_column} = get_sort_query(Q),
    Host = global,
    Service = find_service(Host),
    Rooms_names = get_online_rooms(Service),
    Rooms_infos = build_info_rooms(Rooms_names),
    Rooms_sorted = sort_rooms(Sort_direction, Sort_column, Rooms_infos),
    Rooms_prepared = prepare_rooms_infos(Rooms_sorted),
    TList =
        lists:map(fun([RoomJid | Room]) ->
                     JidLink =
                         make_command(echo,
                                      R,
                                      [{<<"sentence">>, RoomJid}],
                                      [{only, value},
                                       {result_links, [{sentence, room, 1, <<"">>}]}]),
                     ?XE(<<"tr">>, [?XE(<<"td">>, [JidLink]) | [?XC(<<"td">>, E) || E <- Room]])
                  end,
                  Rooms_prepared),
    Titles =
        [?T("Jabber ID"),
         ?T("# participants"),
         ?T("Last message"),
         ?T("Public"),
         ?T("Persistent"),
         ?T("Logging"),
         ?T("Just created"),
         ?T("Room title"),
         ?T("Node")],
    {Titles_TR, _} =
        lists:mapfoldl(fun(Title, Num_column) ->
                          NCS = integer_to_binary(Num_column),
                          TD = ?XE(<<"td">>,
                                   [?CT(Title),
                                    ?C(<<" ">>),
                                    ?AC(<<"?sort=", NCS/binary>>, <<"<">>),
                                    ?C(<<" ">>),
                                    ?AC(<<"?sort=-", NCS/binary>>, <<">">>)]),
                          {TD, Num_column + 1}
                       end,
                       1,
                       Titles),
    [?XCT(<<"h2">>, ?T("Chatrooms")),
     ?XE(<<"table">>,
         [?XE(<<"thead">>, [?XE(<<"tr">>, Titles_TR)]), ?XE(<<"tbody">>, TList)])].

sort_rooms(Direction, Column, Rooms) ->
    Rooms2 = lists:keysort(Column, Rooms),
    case Direction of
        normal ->
            Rooms2;
        reverse ->
            lists:reverse(Rooms2)
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

    History = S#state.history#lqueue.queue,
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
     Node} =
        Room_info,
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

%%--------------------
%% Web Admin Host User

web_menu_hostuser(Acc, _Host, _Username, _Lang) ->
    Acc
    ++ [{<<"muc-rooms">>, <<"MUC Rooms Online">>},
        {<<"muc-affiliations">>, <<"MUC Rooms Affiliations">>},
        {<<"muc-sub">>, <<"MUC Rooms Subscriptions">>},
        {<<"muc-register">>, <<"MUC Service Registration">>}].

web_page_hostuser(_, Host, User, #request{path = [<<"muc-rooms">> | RPath]} = R) ->
    Level = 5 + length(RPath),
    Res = ?H1GL(<<"MUC Rooms Online">>, <<"modules/#mod_muc">>, <<"mod_muc">>)
          ++ [make_command(get_user_rooms,
                           R,
                           [{<<"user">>, User}, {<<"host">>, Host}],
                           [{table_options, {2, RPath}},
                            {result_links, [{room, room, Level, <<"">>}]}])],
    {stop, Res};
web_page_hostuser(_, Host, User, #request{path = [<<"muc-affiliations">>]} = R) ->
    Jid = jid:encode(
              jid:make(User, Host)),
    Res = ?H1GL(<<"MUC Rooms Affiliations">>, <<"modules/#mod_muc">>, <<"mod_muc">>)
          ++ [make_command(set_room_affiliation, R, [{<<"jid">>, Jid}], []),
              make_command(get_room_affiliation, R, [{<<"jid">>, Jid}], [])],
    {stop, Res};
web_page_hostuser(_, Host, User, #request{path = [<<"muc-sub">> | RPath]} = R) ->
    Title =
        ?H1GLraw(<<"MUC Rooms Subscriptions">>,
                 <<"developer/xmpp-clients-bots/extensions/muc-sub/">>,
                 <<"MUC/Sub">>),
    Level = 5 + length(RPath),
    Set = [make_command(subscribe_room, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
           make_command(unsubscribe_room, R, [{<<"user">>, User}, {<<"host">>, Host}], [])],
    Get = [make_command(get_user_subscriptions,
                        R,
                        [{<<"user">>, User}, {<<"host">>, Host}],
                        [{table_options, {20, RPath}},
                         {result_links, [{roomjid, room, Level, <<"">>}]}])],
    {stop, Title ++ Get ++ Set};
web_page_hostuser(_, Host, User, #request{path = [<<"muc-register">>]} = R) ->
    Jid = jid:encode(
              jid:make(User, Host)),
    Res = ?H1GL(<<"MUC Service Registration">>, <<"modules/#mod_muc">>, <<"mod_muc">>)
          ++ [make_command(muc_register_nick, R, [{<<"jid">>, Jid}], []),
              make_command(muc_unregister_nick, R, [{<<"jid">>, Jid}], [])],
    {stop, Res};
web_page_hostuser(Acc, _, _, _) ->
    Acc.
%% @format-end

%%----------------------------
%% Create/Delete Room
%%----------------------------

-spec create_room(Name::binary(), Host::binary(), ServerHost::binary()) -> ok | error.
%% @doc Create a room immediately with the default options.
create_room(Name1, Host1, ServerHost) ->
    create_room_with_opts(Name1, Host1, ServerHost, []).

create_room_with_opts(Name1, Host1, ServerHost1, CustomRoomOpts) ->
    ServerHost = validate_host(ServerHost1, <<"serverhost">>),
    case get_room_pid_validate(Name1, Host1, <<"name">>, <<"host">>) of
	{room_not_found, Name, Host} ->
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
    end.

%% Create the room only in the database.
%% It is required to restart the MUC service for the room to appear.
muc_create_room(ServerHost, {Name, Host, _}, DefRoomOpts) ->
    io:format("Creating room ~ts@~ts~n", [Name, Host]),
    mod_muc:store_room(ServerHost, Host, Name, DefRoomOpts).

-spec destroy_room(Name::binary(), Host::binary()) -> ok | {error, room_not_exists}.
%% @doc Destroy the room immediately.
%% If the room has participants, they are not notified that the room was destroyed;
%% they will notice when they try to chat and receive an error that the room doesn't exist.
destroy_room(Name1, Service1) ->
    case get_room_pid_validate(Name1, Service1, <<"name">>, <<"service">>) of
	{room_not_found, _, _} ->
	    throw({error, "Room doesn't exists"});
	{Pid, _, _} ->
	    mod_muc_room:destroy(Pid),
	    ok
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

rooms_empty_destroy_restuple(Service) ->
    DestroyedRooms = rooms_report(empty, destroy, Service, 0),
    NumberBin = integer_to_binary(length(DestroyedRooms)),
    {ok, <<"Destroyed rooms: ", NumberBin/binary>>}.

rooms_report(Method, Action, Service, Days) ->
    {NA, NP, RP} = muc_unused(Method, Action, Service, Days),
    io:format("rooms ~ts: ~p out of ~p~n", [Method, NP, NA]),
    [<<R/binary, "@", H/binary>> || {R, H, _SH, _P} <- RP].

muc_unused(Method, Action, Service, Last_allowed) ->
    %% Get all required info about all existing rooms
    Rooms_all = get_all_rooms(Service, erlang:system_time(microsecond) - Last_allowed*24*60*60*1000),

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

get_all_rooms(ServiceArg, Timestamp) ->
    Hosts = find_services(ServiceArg),
    lists:flatmap(
      fun(Host) ->
              get_all_rooms2(Host, Timestamp)
      end, Hosts).

get_all_rooms2(Host, Timestamp) ->
    ServerHost = ejabberd_router:host_of_route(Host),
    OnlineRooms = get_online_rooms(Host),
    OnlineMap = lists:foldl(
	fun({Room, _, _, _}, Map) ->
	    Map#{Room => 1}
	end, #{}, OnlineRooms),

    Mod = gen_mod:db_mod(ServerHost, mod_muc),
    DbRooms =
    case {erlang:function_exported(Mod, get_rooms_without_subscribers, 2),
	  erlang:function_exported(Mod, get_hibernated_rooms_older_than, 3)} of
	{_, true} ->
	    Mod:get_hibernated_rooms_older_than(ServerHost, Host, Timestamp);
	{true, _} ->
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
		{_, undefined} ->
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
    case get_room_pid_validate(Room, Host, <<"name">>, <<"service">>) of
	{Pid, _, _} when is_pid(Pid) -> get_room_occupants(Pid);
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
    case get_room_pid_validate(Room, Host, <<"name">>, <<"service">>) of
	{Pid, _, _} when is_pid(Pid)->
	    {ok, #{occupants_number := N}} = mod_muc_room:get_info(Pid),
	    N;
	_ ->
	    throw({error, room_not_found})
    end.

%%----------------------------
%% Send Direct Invitation
%%----------------------------
%% http://xmpp.org/extensions/xep-0249.html

send_direct_invitation(RoomName, RoomService, Password, Reason, UsersString) when is_binary(UsersString) ->
    UsersStrings = binary:split(UsersString, <<":">>, [global]),
    send_direct_invitation(RoomName, RoomService, Password, Reason, UsersStrings);
send_direct_invitation(RoomName, RoomService, Password, Reason, UsersStrings) ->
    case jid:make(RoomName, RoomService) of
	error ->
	    throw({error, "Invalid 'roomname' or 'service'"});
	RoomJid ->
	    XmlEl = build_invitation(Password, Reason, RoomJid),
	    Users = get_users_to_invite(RoomJid, UsersStrings),
	    [send_direct_invitation(RoomJid, UserJid, XmlEl)
	     || UserJid <- Users],
	    ok
    end.

get_users_to_invite(RoomJid, UsersStrings) ->
    OccupantsTuples = get_room_occupants(RoomJid#jid.luser,
					 RoomJid#jid.lserver),
    OccupantsJids = try [jid:decode(JidString)
			 || {JidString, _Nick, _} <- OccupantsTuples]
		    catch _:{bad_jid, _} -> throw({error, "Malformed JID of invited user"})
		    end,
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

-spec change_room_option(Name::binary(), Service::binary(), Option::binary(),
                         Value::atom() | integer() | string()) -> ok | mod_muc_log_not_enabled.
%% @doc Change an option in an existing room.
%% Requires the name of the room, the MUC service where it exists,
%% the option to change (for example title or max_users),
%% and the value to assign to the new option.
%% For example:
%% `change_room_option(<<"testroom">>, <<"conference.localhost">>, <<"title">>, <<"Test Room">>)'
change_room_option(Name, Service, OptionString, ValueString) ->
    case get_room_pid_validate(Name, Service, <<"name">>, <<"service">>) of
	{room_not_found, _, _} ->
	    throw({error, "Room not found"});
	{Pid, _, _} ->
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
		affiliations ->
		    [parse_affiliation_string(Opt) || Opt <- str:tokens(ValueString, <<",">>)];
		subscribers ->
		    [parse_subscription_string(Opt) || Opt <- str:tokens(ValueString, <<",">>)];
		_ -> misc:binary_to_atom(ValueString)
	    end,
    {Option, Value}.

parse_affiliation_string(String) ->
    {Type, JidS} = case String of
		       <<"owner:", Jid/binary>> -> {owner, Jid};
		       <<"admin:", Jid/binary>> -> {admin, Jid};
		       <<"member:", Jid/binary>> -> {member, Jid};
		       <<"outcast:", Jid/binary>> -> {outcast, Jid};
		       _ -> throw({error, "Invalid 'affiliation'"})
		   end,
    try jid:decode(JidS) of
	#jid{luser = U, lserver = S, lresource = R} ->
	    {{U, S, R}, {Type, <<>>}}
    catch _:{bad_jid, _} ->
	throw({error, "Malformed JID in affiliation"})
    end.

parse_subscription_string(String) ->
    case str:tokens(String, <<":">>) of
	[_] ->
	    throw({error, "Invalid 'subscribers' - missing nick"});
	[_, _] ->
	    throw({error, "Invalid 'subscribers' - missing nodes"});
	[JidS, Nick | Nodes] ->
	    Nodes2 = parse_nodes(Nodes, []),
	    try jid:decode(JidS) of
		Jid ->
		    {Jid, Nick, Nodes2}
	    catch _:{bad_jid, _} ->
		throw({error, "Malformed JID in 'subscribers'"})
	    end
    end.

parse_nodes([], Acc) ->
    Acc;
parse_nodes([<<"presence">> | Rest], Acc) ->
    parse_nodes(Rest, [?NS_MUCSUB_NODES_PRESENCE | Acc]);
parse_nodes([<<"messages">> | Rest], Acc) ->
    parse_nodes(Rest, [?NS_MUCSUB_NODES_MESSAGES | Acc]);
parse_nodes([<<"participants">> | Rest], Acc) ->
    parse_nodes(Rest, [?NS_MUCSUB_NODES_PARTICIPANTS | Acc]);
parse_nodes([<<"affiliations">> | Rest], Acc) ->
    parse_nodes(Rest, [?NS_MUCSUB_NODES_AFFILIATIONS | Acc]);
parse_nodes([<<"subject">> | Rest], Acc) ->
    parse_nodes(Rest, [?NS_MUCSUB_NODES_SUBJECT | Acc]);
parse_nodes([<<"config">> | Rest], Acc) ->
    parse_nodes(Rest, [?NS_MUCSUB_NODES_CONFIG | Acc]);
parse_nodes([<<"system">> | Rest], Acc) ->
    parse_nodes(Rest, [?NS_MUCSUB_NODES_SYSTEM | Acc]);
parse_nodes([<<"subscribers">> | Rest], Acc) ->
    parse_nodes(Rest, [?NS_MUCSUB_NODES_SUBSCRIBERS | Acc]);
parse_nodes(_, _) ->
    throw({error, "Invalid 'subscribers' - unknown node name used"}).

-spec get_room_pid_validate(binary(), binary(), binary(), binary()) ->
    {pid() | room_not_found, binary(), binary()}.
get_room_pid_validate(Name, Service, NameArg, ServiceArg) ->
    Name2 = validate_room(Name, NameArg),
    {ServerHost, Service2} = validate_muc2(Service, ServiceArg),
    case mod_muc:unhibernate_room(ServerHost, Service2, Name2) of
	error ->
	    {room_not_found, Name2, Service2};
	{ok, Pid} ->
	    {Pid, Name2, Service2}
    end.

%% @doc Get the Pid of an existing MUC room, or 'room_not_found'.
-spec get_room_pid(binary(), binary()) -> pid() | room_not_found | invalid_service | unknown_service.
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
	    unknown_service
    end.

room_diagnostics(Name, Service) ->
    try get_room_serverhost(Service) of
	ServerHost ->
	    RMod = gen_mod:ram_db_mod(ServerHost, mod_muc),
	    case RMod:find_online_room(ServerHost, Name, Service) of
		error ->
		    room_hibernated;
		{ok, Pid} ->
		    case rpc:pinfo(Pid, [current_stacktrace, message_queue_len, messages]) of
			[{_, R}, {_, QL}, {_, Q}] ->
			    #{stacktrace => R, queue_size => QL, queue => lists:sublist(Q, 10)};
			_ ->
			    unable_to_probe_process
		    end
	    end
    catch
	error:{invalid_domain, _} ->
	    invalid_service;
	error:{unregistered_route, _} ->
	    unknown_service
    end.

%% It is required to put explicitly all the options because
%% the record elements are replaced at compile time.
%% So, this can't be parametrized.
change_option(Option, Value, Config) ->
    case Option of
	allow_change_subj -> Config#config{allow_change_subj = Value};
	allowpm -> Config#config{allowpm = Value};
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
	lang -> Config#config{lang = Value};
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
	pubsub -> Config#config{pubsub = Value};
	title -> Config#config{title = Value};
	vcard -> Config#config{vcard = Value};
	vcard_xupdate -> Config#config{vcard_xupdate = Value};
	voice_request_min_interval -> Config#config{voice_request_min_interval = Value}
    end.

%%----------------------------
%% Get Room Options
%%----------------------------

get_room_options(Name, Service) ->
    case get_room_pid_validate(Name, Service, <<"name">>, <<"service">>) of
	{Pid, _, _} when is_pid(Pid) -> get_room_options(Pid);
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
    case get_room_pid_validate(Name, Service, <<"name">>, <<"service">>) of
	{Pid, _, _} when is_pid(Pid) ->
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

get_room_history(Name, Service) ->
    case get_room_pid_validate(Name, Service, <<"name">>, <<"service">>) of
	{Pid, _, _} when is_pid(Pid) ->
	    case mod_muc_room:get_state(Pid) of
		{ok, StateData} ->
		    History = p1_queue:to_list((StateData#state.history)#lqueue.queue),
		    lists:map(
			fun({_Nick, Packet, _HaveSubject, TimeStamp, _Size}) ->
			    {xmpp_util:encode_timestamp(TimeStamp),
                             ejabberd_web_admin:pretty_print_xml(xmpp:encode(Packet))}
			end, History);
		_ ->
		    throw({error, "Unable to fetch room state."})
	    end;
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
    case get_room_pid_validate(Name, Service, <<"name">>, <<"service">>) of
	{Pid, _, _} when is_pid(Pid) ->
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
    case get_room_pid_validate(Name, Service, <<"name">>, <<"service">>) of
	{Pid, _, _} when is_pid(Pid) ->
	    %% Get the PID for the online room so we can get the state of the room
	    case mod_muc_room:change_item(Pid, jid:decode(JID), affiliation, Affiliation, <<"">>) of
		{ok, _} ->
		    ok;
		{error, notfound} ->
		    throw({error, "Room doesn't exists"});
		{error, _} ->
		    throw({error, "Unable to perform change"})
	    end;
	_ ->
	    throw({error, "Room doesn't exists"})
    end.

%%%
%%% MUC Subscription
%%%

subscribe_room(_User, Nick, _Room, _Nodes) when Nick == <<"">> ->
    throw({error, "Nickname must be set"});
subscribe_room(User, Nick, Room, Nodes) when is_binary(Nodes) ->
    NodeList = re:split(Nodes, "\\h*,\\h*"),
    subscribe_room(User, Nick, Room, NodeList);
subscribe_room(User, Nick, Room, NodeList) ->
    try jid:decode(Room) of
	#jid{luser = Name, lserver = Host} when Name /= <<"">> ->
	    try jid:decode(User) of
		UserJID1 ->
		    UserJID = jid:replace_resource(UserJID1, <<"modmucadmin">>),
		    case get_room_pid_validate(Name, Host, <<"name">>, <<"room">>) of
			{Pid, _, _} when is_pid(Pid) ->
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

subscribe_room_many(Users, Room, Nodes) ->
    MaxUsers = mod_muc_admin_opt:subscribe_room_many_max_users(global),
    if
        length(Users) > MaxUsers ->
            throw({error, "Too many users in subscribe_room_many command"});
        true ->
            lists:foreach(
              fun({User, Nick}) ->
                      subscribe_room(User, Nick, Room, Nodes)
              end, Users)
    end.

unsubscribe_room(User, Room) ->
    try jid:decode(Room) of
	#jid{luser = Name, lserver = Host} when Name /= <<"">> ->
	    try jid:decode(User) of
		UserJID ->
		    case get_room_pid_validate(Name, Host, <<"name">>, <<"room">>) of
			{Pid, _, _} when is_pid(Pid) ->
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
    case get_room_pid_validate(Name, Host, <<"name">>, <<"service">>) of
	{Pid, _, _} when is_pid(Pid) ->
	    {ok, JIDList} = mod_muc_room:get_subscribers(Pid),
	    [jid:encode(jid:remove_resource(J)) || J <- JIDList];
	_ ->
	    throw({error, "The room does not exist"})
    end.

%%----------------------------
%% Utils
%%----------------------------

-spec validate_host(Name :: binary(), ArgName::binary()) -> binary().
validate_host(Name, ArgName) ->
    case jid:nameprep(Name) of
	error ->
	    throw({error, <<"Invalid value of '",ArgName/binary,"'">>});
	Name2 ->
	    case lists:member(Name2, ejabberd_option:hosts()) of
		false ->
		    throw({error, <<"Unknown host passed in '",ArgName/binary,"'">>});
		_ ->
		    Name2
	    end
    end.

-spec validate_user(Name :: binary(), ArgName::binary()) -> binary().
validate_user(Name, ArgName) ->
    case jid:nodeprep(Name) of
	error ->
	    throw({error, <<"Invalid value of '",ArgName/binary,"'">>});
	Name2 ->
	    Name2
    end.

-spec validate_muc(Name :: binary(), ArgName::binary()) -> binary().
validate_muc(Name, ArgName) ->
    case jid:nameprep(Name) of
	error ->
	    throw({error, <<"Invalid value of '",ArgName/binary,"'">>});
	Name2 ->
	    try get_room_serverhost(Name2) of
		_ -> Name2
	    catch
		error:{invalid_domain, _} ->
		    throw({error, <<"Unknown host passed in '",ArgName/binary,"'">>});
		error:{unregistered_route, _} ->
		    throw({error, <<"Unknown host passed in '",ArgName/binary,"'">>})
	    end
    end.

-spec validate_muc2(Name :: binary(), ArgName::binary()) -> {binary(), binary()}.
validate_muc2(Name, ArgName) ->
    case jid:nameprep(Name) of
	error ->
	    throw({error, <<"Invalid value of '",ArgName/binary,"'">>});
	Name2 ->
	    try get_room_serverhost(Name2) of
		Host -> {Host, Name2}
	    catch
		error:{invalid_domain, _} ->
		    throw({error, <<"Unknown host passed in '",ArgName/binary,"'">>});
		error:{unregistered_route, _} ->
		    throw({error, <<"Unknown host passed in '",ArgName/binary,"'">>})
	    end
    end.

-spec validate_room(Name :: binary(), ArgName :: binary()) -> binary().
validate_room(Name, ArgName) ->
    case jid:nodeprep(Name) of
	error ->
	    throw({error, <<"Invalid value of '",ArgName/binary,"'">>});
	Name2 ->
	    Name2
    end.

find_service(global) ->
    global;
find_service(ServerHost) ->
    hd(gen_mod:get_module_opt_hosts(ServerHost, mod_muc)).

find_services_validate(Global, _Name) when Global == global;
    Global == <<"global">> ->
    find_services(Global);
find_services_validate(Service, Name) ->
    case validate_muc(Service, Name) of
	Service2 -> find_services(Service2)
    end.

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

mod_opt_type(subscribe_room_many_max_users) ->
    econf:int().

mod_options(_) ->
    [{subscribe_room_many_max_users, 50}].

mod_doc() ->
    #{desc =>
	  [?T("This module provides commands to administer local MUC "
	      "services and their MUC rooms. It also provides simple "
	      "WebAdmin pages to view the existing rooms."), "",
	   ?T("This module depends on _`mod_muc`_.")],
    opts =>
          [{subscribe_room_many_max_users,
            #{value => ?T("Number"),
              note => "added in 22.05",
              desc =>
                  ?T("How many users can be subscribed to a room at once using "
                     "the 'subscribe_room_many' command. "
                     "The default value is '50'.")}}]}.
