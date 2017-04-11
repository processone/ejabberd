%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%%-------------------------------------------------------------------

-module(mod_admin_extra).
-author('badlop@process-one.net').

-behaviour(gen_mod).

-include("logger.hrl").

-export([start/2, stop/1, reload/3, mod_opt_type/1,
	 get_commands_spec/0, depends/2]).

% Commands API
-export([
	 % Adminsys
	 compile/1, get_cookie/0, export2sql/2,
	 restart_module/2,

	 % Sessions
	 num_active_users/2, num_resources/2, resource_num/3,
	 kick_session/4, status_num/2, status_num/1,
	 status_list/2, status_list/1, connected_users_info/0,
	 connected_users_vhost/1, set_presence/7,
	 get_presence/2, user_sessions_info/2, get_last/2,

	 % Accounts
	 set_password/3, check_password_hash/4, delete_old_users/1,
	 delete_old_users_vhost/2, ban_account/3, check_password/3,

	 % vCard
	 set_nickname/3, get_vcard/3,
	 get_vcard/4, get_vcard_multi/4, set_vcard/4,
	 set_vcard/5,

	 % Roster
	 add_rosteritem/7, delete_rosteritem/4,
	 process_rosteritems/5, get_roster/2, push_roster/3,
	 push_roster_all/1, push_alltoall/2,

	 % Private storage
	 private_get/4, private_set/3,

	 % Shared roster
	 srg_create/5,
	 srg_delete/2, srg_list/1, srg_get_info/2,
	 srg_get_members/2, srg_user_add/4, srg_user_del/4,

	 % Send message
	 send_message/5, send_stanza/3, send_stanza_c2s/4,

	 % Privacy list
	 privacy_set/3,

	 % Stats
	 stats/1, stats/2
	]).


-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("mod_privacy.hrl").
-include("ejabberd_sm.hrl").
-include("xmpp.hrl").

%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(get_commands_spec()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(get_commands_spec()).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

%%%
%%% Register commands
%%%

get_commands_spec() ->
    Vcard1FieldsString = "Some vcard field names in get/set_vcard are:\n"
	" FN		- Full Name\n"
	" NICKNAME	- Nickname\n"
	" BDAY		- Birthday\n"
	" TITLE		- Work: Position\n"
	" ROLE		- Work: Role",

    Vcard2FieldsString = "Some vcard field names and subnames in get/set_vcard2 are:\n"
	" N FAMILY	- Family name\n"
	" N GIVEN	- Given name\n"
	" N MIDDLE	- Middle name\n"
	" ADR CTRY	- Address: Country\n"
	" ADR LOCALITY	- Address: City\n"
	" TEL HOME      - Telephone: Home\n"
	" TEL CELL      - Telephone: Cellphone\n"
	" TEL WORK      - Telephone: Work\n"
	" TEL VOICE     - Telephone: Voice\n"
	" EMAIL USERID	- E-Mail Address\n"
	" ORG ORGNAME	- Work: Company\n"
	" ORG ORGUNIT	- Work: Department",

    VcardXEP = "For a full list of vCard fields check XEP-0054: vcard-temp at "
	"http://www.xmpp.org/extensions/xep-0054.html",

    [
     #ejabberd_commands{name = compile, tags = [erlang],
			desc = "Recompile and reload Erlang source code file",
			module = ?MODULE, function = compile,
			args = [{file, string}],
			args_example = ["/home/me/srcs/ejabberd/mod_example.erl"],
			args_desc = ["Filename of erlang source file to compile"],
			result = {res, rescode},
			result_example = ok,
			result_desc = "Status code: 0 on success, 1 otherwise"},
     #ejabberd_commands{name = get_cookie, tags = [erlang],
			desc = "Get the Erlang cookie of this node",
			module = ?MODULE, function = get_cookie,
			args = [],
			result = {cookie, string},
			result_example = "MWTAVMODFELNLSMYXPPD",
			result_desc = "Erlang cookie used for authentication by ejabberd"},
     #ejabberd_commands{name = export2sql, tags = [mnesia],
			desc = "Export Mnesia tables to files in directory",
			module = ?MODULE, function = export2sql,
			args = [{host, string}, {path, string}],
			args_example = ["myserver.com","/tmp/export/sql"],
			args_desc = ["Server name", "File to write sql export"],
			result = {res, rescode},
			result_example = ok,
			result_desc = "Status code: 0 on success, 1 otherwise"},
    #ejabberd_commands{name = restart_module, tags = [erlang],
			desc = "Stop an ejabberd module, reload code and start",
			module = ?MODULE, function = restart_module,
			args = [{host, binary}, {module, binary}],
			args_example = ["myserver.com","mod_admin_extra"],
			args_desc = ["Server name", "Module to restart"],
			result = {res, integer},
			result_example = 0,
			result_desc = "Returns integer code:\n"
				      " - 0: code reloaded, module restarted\n"
				      " - 1: error: module not loaded\n"
				      " - 2: code not reloaded, but module restarted"},
     #ejabberd_commands{name = num_active_users, tags = [accounts, stats],
			desc = "Get number of users active in the last days",
			policy = admin,
			module = ?MODULE, function = num_active_users,
			args = [{host, binary}, {days, integer}],
			args_example = [<<"myserver.com">>, 3],
			args_desc = ["Name of host to check", "Number of days to calculate sum"],
			result = {users, integer},
			result_example = 123,
			result_desc = "Number of users active on given server in last n days"},
     #ejabberd_commands{name = delete_old_users, tags = [accounts, purge],
			desc = "Delete users that didn't log in last days, or that never logged",
			longdesc = "To protect admin accounts, configure this for example:\n"
			    "access_rules:\n"
			    "  delete_old_users:\n"
			    "    - deny: admin\n"
			    "    - allow: all\n",
			module = ?MODULE, function = delete_old_users,
			args = [{days, integer}],
			args_example = [30],
			args_desc = ["Last login age in days of accounts that should be removed"],
			result = {res, restuple},
			result_example = {ok, <<"Deleted 2 users: [\"oldman@myserver.com\", \"test@myserver.com\"]">>},
			result_desc = "Result tuple"},
     #ejabberd_commands{name = delete_old_users_vhost, tags = [accounts, purge],
			desc = "Delete users that didn't log in last days in vhost, or that never logged",
			longdesc = "To protect admin accounts, configure this for example:\n"
			    "access_rules:\n"
			    "  delete_old_users:\n"
			    "    - deny: admin\n"
			    "    - allow: all\n",
			module = ?MODULE, function = delete_old_users_vhost,
			args = [{host, binary}, {days, integer}],
			args_example = [<<"myserver.com">>, 30],
			args_desc = ["Server name",
				     "Last login age in days of accounts that should be removed"],
			result = {res, restuple},
			result_example = {ok, <<"Deleted 2 users: [\"oldman@myserver.com\", \"test@myserver.com\"]">>},
			result_desc = "Result tuple"},
     #ejabberd_commands{name = check_account, tags = [accounts],
			desc = "Check if an account exists or not",
			module = ejabberd_auth, function = is_user_exists,
			args = [{user, binary}, {host, binary}],
			args_example = [<<"peter">>, <<"myserver.com">>],
			args_desc = ["User name to check", "Server to check"],
			result = {res, rescode},
			result_example = ok,
			result_desc = "Status code: 0 on success, 1 otherwise"},
     #ejabberd_commands{name = check_password, tags = [accounts],
			desc = "Check if a password is correct",
			module = ?MODULE, function = check_password,
			args = [{user, binary}, {host, binary}, {password, binary}],
			args_example = [<<"peter">>, <<"myserver.com">>, <<"secret">>],
			args_desc = ["User name to check", "Server to check", "Password to check"],
			result = {res, rescode},
			result_example = ok,
			result_desc = "Status code: 0 on success, 1 otherwise"},
     #ejabberd_commands{name = check_password_hash, tags = [accounts],
			desc = "Check if the password hash is correct",
			longdesc = "Allowed hash methods: md5, sha.",
			module = ?MODULE, function = check_password_hash,
			args = [{user, binary}, {host, binary}, {passwordhash, binary},
				{hashmethod, binary}],
			args_example = [<<"peter">>, <<"myserver.com">>,
					<<"5ebe2294ecd0e0f08eab7690d2a6ee69">>, <<"md5">>],
			args_desc = ["User name to check", "Server to check",
				     "Password's hash value", "Name of hash method"],
			result = {res, rescode},
			result_example = ok,
			result_desc = "Status code: 0 on success, 1 otherwise"},
     #ejabberd_commands{name = change_password, tags = [accounts],
			desc = "Change the password of an account",
			module = ?MODULE, function = set_password,
			args = [{user, binary}, {host, binary}, {newpass, binary}],
			args_example = [<<"peter">>, <<"myserver.com">>, <<"blank">>],
			args_desc = ["User name", "Server name",
				     "New password for user"],
			result = {res, rescode},
			result_example = ok,
			result_desc = "Status code: 0 on success, 1 otherwise"},
     #ejabberd_commands{name = ban_account, tags = [accounts],
			desc = "Ban an account: kick sessions and set random password",
			module = ?MODULE, function = ban_account,
			args = [{user, binary}, {host, binary}, {reason, binary}],
			args_example = [<<"attacker">>, <<"myserver.com">>, <<"Spaming other users">>],
			args_desc = ["User name to ban", "Server name",
				     "Reason for banning user"],
			result = {res, rescode},
			result_example = ok,
			result_desc = "Status code: 0 on success, 1 otherwise"},
     #ejabberd_commands{name = num_resources, tags = [session],
			desc = "Get the number of resources of a user",
			module = ?MODULE, function = num_resources,
			args = [{user, binary}, {host, binary}],
			args_example = [<<"peter">>, <<"myserver.com">>],
			args_desc = ["User name", "Server name"],
			result = {resources, integer},
			result_example = 5,
			result_desc = "Number of active resources for a user"},
     #ejabberd_commands{name = resource_num, tags = [session],
			desc = "Resource string of a session number",
			module = ?MODULE, function = resource_num,
			args = [{user, binary}, {host, binary}, {num, integer}],
			args_example = [<<"peter">>, <<"myserver.com">>, 2],
			args_desc = ["User name", "Server name", "ID of resource to return"],
			result = {resource, string},
			result_example = <<"Psi">>,
			result_desc = "Name of user resource"},
     #ejabberd_commands{name = kick_session, tags = [session],
			desc = "Kick a user session",
			module = ?MODULE, function = kick_session,
			args = [{user, binary}, {host, binary}, {resource, binary}, {reason, binary}],
			args_example = [<<"peter">>, <<"myserver.com">>, <<"Psi">>,
					<<"Stuck connection">>],
			args_desc = ["User name", "Server name", "User's resource",
				     "Reason for closing session"],
			result = {res, rescode},
			result_example = ok,
			result_desc = "Status code: 0 on success, 1 otherwise"},
     #ejabberd_commands{name = status_num_host, tags = [session, stats],
			desc = "Number of logged users with this status in host",
			policy = admin,
			module = ?MODULE, function = status_num,
			args = [{host, binary}, {status, binary}],
			args_example = [<<"myserver.com">>, <<"dnd">>],
			args_desc = ["Server name", "Status type to check"],
			result = {users, integer},
			result_example = 23,
			result_desc = "Number of connected sessions with given status type"},
     #ejabberd_commands{name = status_num, tags = [session, stats],
			desc = "Number of logged users with this status",
			policy = admin,
			module = ?MODULE, function = status_num,
			args = [{status, binary}],
			args_example = [<<"dnd">>],
			args_desc = ["Status type to check"],
			result = {users, integer},
			result_example = 23,
			result_desc = "Number of connected sessions with given status type"},
     #ejabberd_commands{name = status_list_host, tags = [session],
			desc = "List of users logged in host with their statuses",
			module = ?MODULE, function = status_list,
			args = [{host, binary}, {status, binary}],
			result = {users, {list,
					  {userstatus, {tuple, [
								{user, string},
								{host, string},
								{resource, string},
								{priority, integer},
								{status, string}
							       ]}}
					 }}},
     #ejabberd_commands{name = status_list, tags = [session],
			desc = "List of logged users with this status",
			module = ?MODULE, function = status_list,
			args = [{status, binary}],
			result = {users, {list,
					  {userstatus, {tuple, [
								{user, string},
								{host, string},
								{resource, string},
								{priority, integer},
								{status, string}
							       ]}}
					 }}},
     #ejabberd_commands{name = connected_users_info,
			tags = [session],
			desc = "List all established sessions and their information",
			module = ?MODULE, function = connected_users_info,
			args = [],
			result = {connected_users_info,
				  {list,
				   {sessions, {tuple,
					       [{jid, string},
						{connection, string},
						{ip, string},
						{port, integer},
						{priority, integer},
						{node, string},
						{uptime, integer}
					       ]}}
				  }}},
     #ejabberd_commands{name = connected_users_vhost,
			tags = [session],
			desc = "Get the list of established sessions in a vhost",
			module = ?MODULE, function = connected_users_vhost,
			args = [{host, binary}],
			result = {connected_users_vhost, {list, {sessions, string}}}},
     #ejabberd_commands{name = user_sessions_info,
			tags = [session],
			desc = "Get information about all sessions of a user",
			module = ?MODULE, function = user_sessions_info,
			args = [{user, binary}, {host, binary}],
			result = {sessions_info,
				  {list,
				   {session, {tuple,
					      [{connection, string},
					       {ip, string},
					       {port, integer},
					       {priority, integer},
					       {node, string},
					       {uptime, integer},
					       {status, string},
					       {resource, string},
					       {statustext, string}
					      ]}}
				  }}},

     #ejabberd_commands{name = get_presence, tags = [session],
			desc =
			    "Retrieve the resource with highest priority, "
			    "and its presence (show and status message) "
			    "for a given user.",
			longdesc =
			    "The 'jid' value contains the user jid "
			    "with resource.\nThe 'show' value contains "
			    "the user presence flag. It can take "
			    "limited values:\n - available\n - chat "
			    "(Free for chat)\n - away\n - dnd (Do "
			    "not disturb)\n - xa (Not available, "
			    "extended away)\n - unavailable (Not "
			    "connected)\n\n'status' is a free text "
			    "defined by the user client.",
			module = ?MODULE, function = get_presence,
			args = [{user, binary}, {server, binary}],
			result =
			    {presence,
			     {tuple,
			      [{jid, string}, {show, string},
			       {status, string}]}}},
     #ejabberd_commands{name = set_presence,
			tags = [session],
			desc = "Set presence of a session",
			module = ?MODULE, function = set_presence,
			args = [{user, binary}, {host, binary},
				{resource, binary}, {type, binary},
				{show, binary}, {status, binary},
				{priority, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = set_nickname, tags = [vcard],
			desc = "Set nickname in a user's vCard",
			module = ?MODULE, function = set_nickname,
			args = [{user, binary}, {host, binary}, {nickname, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = get_vcard, tags = [vcard],
			desc = "Get content from a vCard field",
			longdesc = Vcard1FieldsString ++ "\n" ++ Vcard2FieldsString ++ "\n\n" ++ VcardXEP,
			module = ?MODULE, function = get_vcard,
			args = [{user, binary}, {host, binary}, {name, binary}],
			result = {content, string}},
     #ejabberd_commands{name = get_vcard2, tags = [vcard],
			desc = "Get content from a vCard field",
			longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
			module = ?MODULE, function = get_vcard,
			args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}],
			result = {content, string}},
     #ejabberd_commands{name = get_vcard2_multi, tags = [vcard],
			desc = "Get multiple contents from a vCard field",
			longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
			module = ?MODULE, function = get_vcard_multi,
			args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}],
			result = {contents, {list, {value, string}}}},

     #ejabberd_commands{name = set_vcard, tags = [vcard],
			desc = "Set content in a vCard field",
			longdesc = Vcard1FieldsString ++ "\n" ++ Vcard2FieldsString ++ "\n\n" ++ VcardXEP,
			module = ?MODULE, function = set_vcard,
			args = [{user, binary}, {host, binary}, {name, binary}, {content, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = set_vcard2, tags = [vcard],
			desc = "Set content in a vCard subfield",
			longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
			module = ?MODULE, function = set_vcard,
			args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}, {content, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = set_vcard2_multi, tags = [vcard],
			desc = "Set multiple contents in a vCard subfield",
			longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
			module = ?MODULE, function = set_vcard,
			args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}, {contents, {list, {value, binary}}}],
			result = {res, rescode}},

     #ejabberd_commands{name = add_rosteritem, tags = [roster],
			desc = "Add an item to a user's roster (supports ODBC)",
			longdesc = "Group can be several groups separated by ; for example: \"g1;g2;g3\"",
			module = ?MODULE, function = add_rosteritem,
			args = [{localuser, binary}, {localserver, binary},
				{user, binary}, {server, binary},
				{nick, binary}, {group, binary},
				{subs, binary}],
			result = {res, rescode}},
     %%{"", "subs= none, from, to or both"},
     %%{"", "example: add-roster peter localhost mike server.com MiKe Employees both"},
     %%{"", "will add mike@server.com to peter@localhost roster"},
     #ejabberd_commands{name = delete_rosteritem, tags = [roster],
			desc = "Delete an item from a user's roster (supports ODBC)",
			module = ?MODULE, function = delete_rosteritem,
			args = [{localuser, binary}, {localserver, binary},
				{user, binary}, {server, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = process_rosteritems, tags = [roster],
			desc = "List/delete rosteritems that match filter (only Mnesia)",
			longdesc = "Explanation of each argument:\n"
			" - action: what to do with each rosteritem that "
			"matches all the filtering options\n"
			" - subs: subscription type\n"
			" - asks: pending subscription\n"
			" - users: the JIDs of the local user\n"
			" - contacts: the JIDs of the contact in the roster\n"
			"\n"
			"Allowed values in the arguments:\n"
			"  ACTION = list | delete\n"
			"  SUBS = SUB[:SUB]* | any\n"
			"  SUB = none | from | to | both\n"
			"  ASKS = ASK[:ASK]* | any\n"
			"  ASK = none | out | in\n"
			"  USERS = JID[:JID]* | any\n"
			"  CONTACTS = JID[:JID]* | any\n"
			"  JID = characters valid in a JID, and can use the "
			"globs: *, ?, ! and [...]\n"
			"\n"
			"This example will list roster items with subscription "
			"'none', 'from' or 'to' that have any ask property, of "
			"local users which JID is in the virtual host "
			"'example.org' and that the contact JID is either a "
			"bare server name (without user part) or that has a "
			"user part and the server part contains the word 'icq'"
			":\n  list none:from:to any *@example.org *:*@*icq*",
			module = ?MODULE, function = process_rosteritems,
			args = [{action, string}, {subs, string},
				{asks, string}, {users, string},
				{contacts, string}],
			result = {response,
				  {list,
				   {pairs, {tuple,
					       [{user, string},
						{contact, string}
					       ]}}
				  }}},
     #ejabberd_commands{name = get_roster, tags = [roster],
			desc = "Get roster of a local user",
                        policy = user,
			module = ?MODULE, function = get_roster,
			args = [],
			result = {contacts, {list, {contact, {tuple, [
								      {jid, string},
								      {nick, string},
								      {subscription, string},
								      {ask, string},
								      {group, string}
								     ]}}}}},
     #ejabberd_commands{name = push_roster, tags = [roster],
			desc = "Push template roster from file to a user",
			module = ?MODULE, function = push_roster,
			args = [{file, binary}, {user, binary}, {host, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = push_roster_all, tags = [roster],
			desc = "Push template roster from file to all those users",
			module = ?MODULE, function = push_roster_all,
			args = [{file, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = push_alltoall, tags = [roster],
			desc = "Add all the users to all the users of Host in Group",
			module = ?MODULE, function = push_alltoall,
			args = [{host, binary}, {group, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = get_last, tags = [last],
			desc = "Get last activity information",
			longdesc = "Timestamp is UTC and XEP-0082 format, for example: "
			    "2017-02-23T22:25:28.063062Z     ONLINE",
			module = ?MODULE, function = get_last,
			args = [{user, binary}, {host, binary}],
			result = {last_activity,
				  {tuple, [{timestamp, string},
					   {status, string}
					  ]}}},
     #ejabberd_commands{name = set_last, tags = [last],
			desc = "Set last activity information",
			longdesc = "Timestamp is the seconds since"
			"1970-01-01 00:00:00 UTC, for example: date +%s",
			module = mod_last, function = store_last_info,
			args = [{user, binary}, {host, binary}, {timestamp, integer}, {status, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = private_get, tags = [private],
			desc = "Get some information from a user private storage",
			module = ?MODULE, function = private_get,
			args = [{user, binary}, {host, binary}, {element, binary}, {ns, binary}],
			result = {res, string}},
     #ejabberd_commands{name = private_set, tags = [private],
			desc = "Set to the user private storage",
			module = ?MODULE, function = private_set,
			args = [{user, binary}, {host, binary}, {element, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = srg_create, tags = [shared_roster_group],
			desc = "Create a Shared Roster Group",
			longdesc = "If you want to specify several group "
			"identifiers in the Display argument,\n"
			"put  \\ \" around the argument and\nseparate the "
			"identifiers with \\ \\ n\n"
			"For example:\n"
			"  ejabberdctl srg_create group3 localhost "
			"name desc \\\"group1\\\\ngroup2\\\"",
			module = ?MODULE, function = srg_create,
			args = [{group, binary}, {host, binary},
				{name, binary}, {description, binary}, {display, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = srg_delete, tags = [shared_roster_group],
			desc = "Delete a Shared Roster Group",
			module = ?MODULE, function = srg_delete,
			args = [{group, binary}, {host, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = srg_list, tags = [shared_roster_group],
			desc = "List the Shared Roster Groups in Host",
			module = ?MODULE, function = srg_list,
			args = [{host, binary}],
			result = {groups, {list, {id, string}}}},
     #ejabberd_commands{name = srg_get_info, tags = [shared_roster_group],
			desc = "Get info of a Shared Roster Group",
			module = ?MODULE, function = srg_get_info,
			args = [{group, binary}, {host, binary}],
			result = {informations, {list, {information, {tuple, [{key, string}, {value, string}]}}}}},
     #ejabberd_commands{name = srg_get_members, tags = [shared_roster_group],
			desc = "Get members of a Shared Roster Group",
			module = ?MODULE, function = srg_get_members,
			args = [{group, binary}, {host, binary}],
			result = {members, {list, {member, string}}}},
     #ejabberd_commands{name = srg_user_add, tags = [shared_roster_group],
			desc = "Add the JID user@host to the Shared Roster Group",
			module = ?MODULE, function = srg_user_add,
			args = [{user, binary}, {host, binary}, {group, binary}, {grouphost, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = srg_user_del, tags = [shared_roster_group],
			desc = "Delete this JID user@host from the Shared Roster Group",
			module = ?MODULE, function = srg_user_del,
			args = [{user, binary}, {host, binary}, {group, binary}, {grouphost, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = get_offline_count,
			tags = [offline],
			desc = "Get the number of unread offline messages",
			policy = user,
			module = mod_offline, function = count_offline_messages,
			args = [],
			result = {value, integer}},
     #ejabberd_commands{name = send_message, tags = [stanza],
			desc = "Send a message to a local or remote bare of full JID",
			module = ?MODULE, function = send_message,
			args = [{type, binary}, {from, binary}, {to, binary},
				{subject, binary}, {body, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = send_stanza_c2s, tags = [stanza],
			desc = "Send a stanza as if sent from a c2s session",
			module = ?MODULE, function = send_stanza_c2s,
			args = [{user, binary}, {host, binary}, {resource, binary}, {stanza, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = send_stanza, tags = [stanza],
			desc = "Send a stanza; provide From JID and valid To JID",
			module = ?MODULE, function = send_stanza,
			args = [{from, binary}, {to, binary}, {stanza, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = privacy_set, tags = [stanza],
			desc = "Send a IQ set privacy stanza for a local account",
			module = ?MODULE, function = privacy_set,
			args = [{user, binary}, {host, binary}, {xmlquery, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = stats, tags = [stats],
			desc = "Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds processes",
			policy = admin,
			module = ?MODULE, function = stats,
			args = [{name, binary}],
			result = {stat, integer}},
     #ejabberd_commands{name = stats_host, tags = [stats],
			desc = "Get statistical value for this host: registeredusers onlineusers",
			policy = admin,
			module = ?MODULE, function = stats,
			args = [{name, binary}, {host, binary}],
			result = {stat, integer}}
    ].


%%%
%%% Adminsys
%%%

compile(File) ->
    Ebin = filename:join(code:lib_dir(ejabberd), "ebin"),
    case ext_mod:compile_erlang_file(Ebin, File) of
	{ok, Module} ->
	    code:purge(Module),
	    code:load_file(Module),
	    ok;
	_ ->
	    error
    end.

get_cookie() ->
    atom_to_list(erlang:get_cookie()).

restart_module(Host, Module) when is_binary(Module) ->
    restart_module(Host, misc:binary_to_atom(Module));
restart_module(Host, Module) when is_atom(Module) ->
    List = gen_mod:loaded_modules_with_opts(Host),
    case proplists:get_value(Module, List) of
	undefined ->
	    % not a running module, force code reload anyway
	    code:purge(Module),
	    code:delete(Module),
	    code:load_file(Module),
	    1;
	Opts ->
	    gen_mod:stop_module(Host, Module),
	    case code:soft_purge(Module) of
		true ->
		    code:delete(Module),
		    code:load_file(Module),
		    gen_mod:start_module(Host, Module, Opts),
		    0;
		false ->
		    gen_mod:start_module(Host, Module, Opts),
		    2
	    end
    end.

export2sql(Host, Directory) ->
    Tables = [{export_last, last},
	      {export_offline, offline},
	      {export_passwd, passwd},
	      {export_private_storage, private_storage},
	      {export_roster, roster},
	      {export_vcard, vcard},
	      {export_vcard_search, vcard_search}],
    Export = fun({TableFun, Table}) ->
		     Filename = filename:join([Directory, atom_to_list(Table)++".txt"]),
		     io:format("Trying to export Mnesia table '~p' on Host '~s' to file '~s'~n", [Table, Host, Filename]),
		     Res = (catch ejd2sql:TableFun(Host, Filename)),
		     io:format("  Result: ~p~n", [Res])
	     end,
    lists:foreach(Export, Tables),
    ok.

%%%
%%% Accounts
%%%

set_password(User, Host, Password) ->
    Fun = fun () -> ejabberd_auth:set_password(User, Host, Password) end,
    user_action(User, Host, Fun, ok).

check_password(User, Host, Password) ->
    ejabberd_auth:check_password(User, <<>>, Host, Password).

%% Copied some code from ejabberd_commands.erl
check_password_hash(User, Host, PasswordHash, HashMethod) ->
    AccountPass = ejabberd_auth:get_password_s(User, Host),
    AccountPassHash = case {AccountPass, HashMethod} of
			  {A, _} when is_tuple(A) -> scrammed;
			  {_, <<"md5">>} -> get_md5(AccountPass);
			  {_, <<"sha">>} -> get_sha(AccountPass);
			  {_, Method} ->
			      ?ERROR_MSG("check_password_hash called "
					 "with hash method: ~p", [Method]),
			      undefined
		      end,
    case AccountPassHash of
	scrammed ->
	    ?ERROR_MSG("Passwords are scrammed, and check_password_hash cannot work.", []),
	    throw(passwords_scrammed_command_cannot_work);
	undefined -> throw(unkown_hash_method);
	PasswordHash -> ok;
	_ -> false
    end.
get_md5(AccountPass) ->
    iolist_to_binary([io_lib:format("~2.16.0B", [X])
                      || X <- binary_to_list(erlang:md5(AccountPass))]).
get_sha(AccountPass) ->
    iolist_to_binary([io_lib:format("~2.16.0B", [X])
		      || X <- binary_to_list(crypto:hash(sha, AccountPass))]).

num_active_users(Host, Days) ->
    DB_Type = gen_mod:db_type(Host, mod_last),
    list_last_activity(Host, true, Days, DB_Type).

%% Code based on ejabberd/src/web/ejabberd_web_admin.erl
list_last_activity(Host, Integral, Days, mnesia) ->
    TimeStamp = p1_time_compat:system_time(seconds),
    TS = TimeStamp - Days * 86400,
    case catch mnesia:dirty_select(
		 last_activity, [{{last_activity, {'_', Host}, '$1', '_'},
				  [{'>', '$1', TS}],
				  [{'trunc', {'/',
					      {'-', TimeStamp, '$1'},
					      86400}}]}]) of
							      {'EXIT', _Reason} ->
		 [];
	       Vals ->
		 Hist = histogram(Vals, Integral),
		 if
		     Hist == [] ->
			 0;
		     true ->
			 Left = Days - length(Hist),
			 Tail = if
				    Integral ->
					lists:duplicate(Left, lists:last(Hist));
				    true ->
					lists:duplicate(Left, 0)
				end,
			 lists:nth(Days, Hist ++ Tail)
		 end
	 end;
list_last_activity(_Host, _Integral, _Days, DB_Type) ->
    throw({error, iolist_to_binary(io_lib:format("Unsupported backend: ~p",
						 [DB_Type]))}).

histogram(Values, Integral) ->
    histogram(lists:sort(Values), Integral, 0, 0, []).
histogram([H | T], Integral, Current, Count, Hist) when Current == H ->
    histogram(T, Integral, Current, Count + 1, Hist);
histogram([H | _] = Values, Integral, Current, Count, Hist) when Current < H ->
    if
	Integral ->
	    histogram(Values, Integral, Current + 1, Count, [Count | Hist]);
	true ->
	    histogram(Values, Integral, Current + 1, 0, [Count | Hist])
    end;
histogram([], _Integral, _Current, Count, Hist) ->
    if
	Count > 0 ->
	    lists:reverse([Count | Hist]);
	true ->
	    lists:reverse(Hist)
    end.


delete_old_users(Days) ->
    %% Get the list of registered users
    Users = ejabberd_auth:dirty_get_registered_users(),

    {removed, N, UR} = delete_old_users(Days, Users),
    {ok, io_lib:format("Deleted ~p users: ~p", [N, UR])}.

delete_old_users_vhost(Host, Days) ->
    %% Get the list of registered users
    Users = ejabberd_auth:get_vh_registered_users(Host),

    {removed, N, UR} = delete_old_users(Days, Users),
    {ok, io_lib:format("Deleted ~p users: ~p", [N, UR])}.

delete_old_users(Days, Users) ->
    SecOlder = Days*24*60*60,
    TimeStamp_now = p1_time_compat:system_time(seconds),
    TimeStamp_oldest = TimeStamp_now - SecOlder,
    F = fun({LUser, LServer}) ->
	    case catch delete_or_not(LUser, LServer, TimeStamp_oldest) of
		true ->
		    ejabberd_auth:remove_user(LUser, LServer),
		    true;
		_ ->
		    false
	    end
	end,
    Users_removed = lists:filter(F, Users),
    {removed, length(Users_removed), Users_removed}.

delete_or_not(LUser, LServer, TimeStamp_oldest) ->
    allow = acl:match_rule(LServer, delete_old_users, jid:make(LUser, LServer)),
    [] = ejabberd_sm:get_user_resources(LUser, LServer),
    case mod_last:get_last_info(LUser, LServer) of
        {ok, TimeStamp, _Status} ->
	    if TimeStamp_oldest < TimeStamp ->
		    false;
		true ->
		    true
	    end;
	not_found ->
	    true
    end.

%%
%% Ban account

ban_account(User, Host, ReasonText) ->
    Reason = prepare_reason(ReasonText),
    kick_sessions(User, Host, Reason),
    set_random_password(User, Host, Reason),
    ok.

kick_sessions(User, Server, Reason) ->
    lists:map(
      fun(Resource) ->
	      kick_this_session(User, Server, Resource, Reason)
      end,
      ejabberd_sm:get_user_resources(User, Server)).

set_random_password(User, Server, Reason) ->
    NewPass = build_random_password(Reason),
    set_password_auth(User, Server, NewPass).

build_random_password(Reason) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Date = str:format("~4..0B~2..0B~2..0BT~2..0B:~2..0B:~2..0B",
		      [Year, Month, Day, Hour, Minute, Second]),
    RandomString = randoms:get_string(),
    <<"BANNED_ACCOUNT--", Date/binary, "--", RandomString/binary, "--", Reason/binary>>.

set_password_auth(User, Server, Password) ->
    ok = ejabberd_auth:set_password(User, Server, Password).

prepare_reason([]) ->
    <<"Kicked by administrator">>;
prepare_reason([Reason]) ->
    Reason;
prepare_reason(Reason) when is_binary(Reason) ->
    Reason.

%%%
%%% Sessions
%%%

num_resources(User, Host) ->
    length(ejabberd_sm:get_user_resources(User, Host)).

resource_num(User, Host, Num) ->
    Resources = ejabberd_sm:get_user_resources(User, Host),
    case (0<Num) and (Num=<length(Resources)) of
	true ->
	    lists:nth(Num, Resources);
	false ->
            throw({bad_argument,
                   lists:flatten(io_lib:format("Wrong resource number: ~p", [Num]))})
    end.

kick_session(User, Server, Resource, ReasonText) ->
    kick_this_session(User, Server, Resource, prepare_reason(ReasonText)),
    ok.

kick_this_session(User, Server, Resource, Reason) ->
    ejabberd_sm:route(jid:make(User, Server, Resource),
                      {exit, Reason}).

status_num(Host, Status) ->
    length(get_status_list(Host, Status)).
status_num(Status) ->
    status_num(<<"all">>, Status).
status_list(Host, Status) ->
    Res = get_status_list(Host, Status),
    [{U, S, R, P, St} || {U, S, R, P, St} <- Res].
status_list(Status) ->
    status_list(<<"all">>, Status).


get_status_list(Host, Status_required) ->
    %% Get list of all logged users
    Sessions = ejabberd_sm:dirty_get_my_sessions_list(),
    %% Reformat the list
    Sessions2 = [ {Session#session.usr, Session#session.sid, Session#session.priority} || Session <- Sessions],
    Fhost = case Host of
		<<"all">> ->
		    %% All hosts are requested, so dont filter at all
		    fun(_, _) -> true end;
		_ ->
		    %% Filter the list, only Host is interesting
		    fun(A, B) -> A == B end
	    end,
    Sessions3 = [ {Pid, Server, Priority} || {{_User, Server, _Resource}, {_, Pid}, Priority} <- Sessions2, apply(Fhost, [Server, Host])],
    %% For each Pid, get its presence
    Sessions4 = [ {catch get_presence(Pid), Server, Priority} || {Pid, Server, Priority} <- Sessions3],
    %% Filter by status
    Fstatus = case Status_required of
		  <<"all">> ->
		      fun(_, _) -> true end;
		  _ ->
		      fun(A, B) -> A == B end
	      end,
    [{User, Server, Resource, Priority, stringize(Status_text)}
     || {{User, Resource, Status, Status_text}, Server, Priority} <- Sessions4,
	apply(Fstatus, [Status, Status_required])].

connected_users_info() ->
    USRIs = dirty_get_sessions_list2(),
    CurrentSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    lists:map(
      fun([{U, S, R}, {Now, Pid}, Priority, Info]) ->
	      Conn = proplists:get_value(conn, Info),
	      {Ip, Port} = proplists:get_value(ip, Info),
	      IPS = inet_parse:ntoa(Ip),
	      NodeS = atom_to_list(node(Pid)),
	      Uptime = CurrentSec - calendar:datetime_to_gregorian_seconds(
				      calendar:now_to_local_time(Now)),
	      PriorityI = case Priority of
			      PI when is_integer(PI) -> PI;
			      _ -> nil
			  end,
	      {binary_to_list(<<U/binary, $@, S/binary, $/, R/binary>>),
	       atom_to_list(Conn), IPS, Port, PriorityI, NodeS, Uptime}
      end,
      USRIs).

connected_users_vhost(Host) ->
    USRs = ejabberd_sm:get_vh_session_list(Host),
    [ [U, $@, S, $/, R] || {U, S, R} <- USRs].

%% Code copied from ejabberd_sm.erl and customized
dirty_get_sessions_list2() ->
    Ss = mnesia:dirty_select(
      session,
	   [{#session{usr = '$1', sid = '$2', priority = '$3', info = '$4',
		 _ = '_'},
	     [],
	     [['$1', '$2', '$3', '$4']]}]),
    lists:filter(fun([_USR, _SID, _Priority, Info]) ->
			 not proplists:get_bool(offline, Info)
		 end, Ss).

%% Make string more print-friendly
stringize(String) ->
    %% Replace newline characters with other code
    ejabberd_regexp:greplace(String, <<"\n">>, <<"\\n">>).

get_presence(Pid) ->
    Pres = #presence{from = From} = ejabberd_c2s:get_presence(Pid),
    Show = case Pres of
	       #presence{type = unavailable} -> <<"unavailable">>;
	       #presence{show = undefined} -> <<"available">>;
	       #presence{show = S} -> atom_to_binary(S, utf8)
	   end,
    Status = xmpp:get_text(Pres#presence.status),
    {From#jid.user, From#jid.resource, Show, Status}.

get_presence(U, S) ->
    Pids = [ejabberd_sm:get_session_pid(U, S, R)
	    || R <- ejabberd_sm:get_user_resources(U, S)],
    OnlinePids = [Pid || Pid <- Pids, Pid=/=none],
    case OnlinePids of
	[] ->
	    {jid:encode({U, S, <<>>}), <<"unavailable">>, <<"">>};
	[SessionPid|_] ->
	    {_User, Resource, Show, Status} = get_presence(SessionPid),
	    FullJID = jid:encode({U, S, Resource}),
	    {FullJID, Show, Status}
    end.

set_presence(User, Host, Resource, Type, Show, Status, Priority)
        when is_integer(Priority) ->
    BPriority = integer_to_binary(Priority),
    set_presence(User, Host, Resource, Type, Show, Status, BPriority);
set_presence(User, Host, Resource, Type, Show, Status, Priority0) ->
    Priority = if is_integer(Priority0) -> Priority0;
		  true -> binary_to_integer(Priority0)
	       end,
    case ejabberd_sm:get_session_pid(User, Host, Resource) of
	none ->
	    error;
	Pid ->
	    From = jid:make(User, Host, Resource),
	    To = jid:make(User, Host),
	    Presence = #presence{from = From, to = To,
				 type = misc:binary_to_atom(Type),
				 show = misc:binary_to_atom(Show),
				 status = xmpp:mk_text(Status),
				 priority = Priority},
	    Pid ! {route, Presence},
	    ok
    end.

user_sessions_info(User, Host) ->
    CurrentSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    US = {User, Host},
    Sessions = case catch mnesia:dirty_index_read(session, US, #session.us) of
		   {'EXIT', _Reason} ->
		       [];
		   Ss ->
		       lists:filter(fun(#session{info = Info}) ->
					    not proplists:get_bool(offline, Info)
				    end, Ss)
	       end,
    lists:map(
      fun(Session) ->
	      {_U, _S, Resource} = Session#session.usr,
	      {Now, Pid} = Session#session.sid,
	      {_U, _Resource, Status, StatusText} = get_presence(Pid),
	      Info = Session#session.info,
	      Priority = Session#session.priority,
	      Conn = proplists:get_value(conn, Info),
	      {Ip, Port} = proplists:get_value(ip, Info),
	      IPS = inet_parse:ntoa(Ip),
	      NodeS = atom_to_list(node(Pid)),
	      Uptime = CurrentSec - calendar:datetime_to_gregorian_seconds(
				      calendar:now_to_local_time(Now)),
	      {atom_to_list(Conn), IPS, Port, Priority, NodeS, Uptime, Status, Resource, StatusText}
      end,
      Sessions).


%%%
%%% Vcard
%%%

set_nickname(User, Host, Nickname) ->
    VCard = xmpp:encode(#vcard_temp{nickname = Nickname}),
    case mod_vcard:set_vcard(User, jid:nameprep(Host), VCard) of
	{error, badarg} ->
	    error;
	ok ->
	    ok
    end.

get_vcard(User, Host, Name) ->
    [Res | _] = get_vcard_content(User, Host, [Name]),
    Res.

get_vcard(User, Host, Name, Subname) ->
    [Res | _] = get_vcard_content(User, Host, [Name, Subname]),
    Res.

get_vcard_multi(User, Host, Name, Subname) ->
    get_vcard_content(User, Host, [Name, Subname]).

set_vcard(User, Host, Name, SomeContent) ->
    set_vcard_content(User, Host, [Name], SomeContent).

set_vcard(User, Host, Name, Subname, SomeContent) ->
    set_vcard_content(User, Host, [Name, Subname], SomeContent).


%%
%% Internal vcard

get_vcard_content(User, Server, Data) ->
    case mod_vcard:get_vcard(jid:nodeprep(User), jid:nameprep(Server)) of
	[El|_] ->
	    case get_vcard(Data, El) of
		[false] -> throw(error_no_value_found_in_vcard);
		ElemList -> ?DEBUG("ELS ~p", [ElemList]), [fxml:get_tag_cdata(Elem) || Elem <- ElemList]
	    end;
	[] ->
	    throw(error_no_vcard_found);
	error ->
	    throw(database_failure)
    end.

get_vcard([<<"TEL">>, TelType], {_, _, _, OldEls}) ->
    {TakenEl, _NewEls} = take_vcard_tel(TelType, OldEls, [], not_found),
    [TakenEl];

get_vcard([Data1, Data2], A1) ->
    case get_subtag(A1, Data1) of
	[false] -> [false];
	A2List ->
	    lists:flatten([get_vcard([Data2], A2) || A2 <- A2List])
    end;

get_vcard([Data], A1) ->
    get_subtag(A1, Data).

get_subtag(Xmlelement, Name) ->
    [fxml:get_subtag(Xmlelement, Name)].

set_vcard_content(User, Server, Data, SomeContent) ->
    ContentList = case SomeContent of
	[Bin | _] when is_binary(Bin) -> SomeContent;
	Bin when is_binary(Bin) -> [SomeContent]
    end,
    %% Get old vcard
    A4 = case mod_vcard:get_vcard(jid:nodeprep(User), jid:nameprep(Server)) of
	     [A1] ->
		 {_, _, _, A2} = A1,
		 update_vcard_els(Data, ContentList, A2);
	     [] ->
		 update_vcard_els(Data, ContentList, []);
	     error ->
		 throw(database_failure)
	 end,
    %% Build new vcard
    SubEl = {xmlel, <<"vCard">>, [{<<"xmlns">>,<<"vcard-temp">>}], A4},
    mod_vcard:set_vcard(User, jid:nameprep(Server), SubEl),
    ok.

take_vcard_tel(TelType, [{xmlel, <<"TEL">>, _, SubEls}=OldEl | OldEls], NewEls, Taken) ->
    {Taken2, NewEls2} = case lists:keymember(TelType, 2, SubEls) of
	true -> {fxml:get_subtag(OldEl, <<"NUMBER">>), NewEls};
	false -> {Taken, [OldEl | NewEls]}
    end,
    take_vcard_tel(TelType, OldEls, NewEls2, Taken2);
take_vcard_tel(TelType, [OldEl | OldEls], NewEls, Taken) ->
    take_vcard_tel(TelType, OldEls, [OldEl | NewEls], Taken);
take_vcard_tel(_TelType, [], NewEls, Taken) ->
    {Taken, NewEls}.

update_vcard_els([<<"TEL">>, TelType], [TelValue], OldEls) ->
    {_, NewEls} = take_vcard_tel(TelType, OldEls, [], not_found),
    NewEl = {xmlel,<<"TEL">>,[],
             [{xmlel,TelType,[],[]},
              {xmlel,<<"NUMBER">>,[],[{xmlcdata,TelValue}]}]},
    [NewEl | NewEls];

update_vcard_els(Data, ContentList, Els1) ->
    Els2 = lists:keysort(2, Els1),
    [Data1 | Data2] = Data,
    NewEls = case Data2 of
		[] ->
		    [{xmlel, Data1, [], [{xmlcdata,Content}]} || Content <- ContentList];
		[D2] ->
		    OldEl = case lists:keysearch(Data1, 2, Els2) of
				{value, A} -> A;
				false -> {xmlel, Data1, [], []}
			    end,
		    {xmlel, _, _, ContentOld1} = OldEl,
		    Content2 = [{xmlel, D2, [], [{xmlcdata,Content}]} || Content <- ContentList],
		    ContentOld2 = [A || {_, X, _, _} = A <- ContentOld1, X/=D2],
		    ContentOld3 = lists:keysort(2, ContentOld2),
		    ContentNew = lists:keymerge(2, Content2, ContentOld3),
		    [{xmlel, Data1, [], ContentNew}]
	    end,
    Els3 = lists:keydelete(Data1, 2, Els2),
    lists:keymerge(2, NewEls, Els3).


%%%
%%% Roster
%%%

add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs) ->
    case add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs, []) of
	{atomic, ok} ->
	    push_roster_item(LocalUser, LocalServer, User, Server, {add, Nick, Subs, Group}),
	    ok;
	_ ->
	    error
    end.

add_rosteritem(LU, LS, User, Server, Nick, Group, Subscription, Xattrs) ->
    subscribe(LU, LS, User, Server, Nick, Group, Subscription, Xattrs).

subscribe(LU, LS, User, Server, Nick, Group, Subscription, _Xattrs) ->
    ItemEl = build_roster_item(User, Server, {add, Nick, Subscription, Group}),
    mod_roster:set_items(LU, LS, #roster_query{items = [ItemEl]}).

delete_rosteritem(LocalUser, LocalServer, User, Server) ->
    case unsubscribe(LocalUser, LocalServer, User, Server) of
	{atomic, ok} ->
	    push_roster_item(LocalUser, LocalServer, User, Server, remove),
	    ok;
	_  ->
	    error
    end.

unsubscribe(LU, LS, User, Server) ->
    ItemEl = build_roster_item(User, Server, remove),
    mod_roster:set_items(LU, LS, #roster_query{items = [ItemEl]}).

%% -----------------------------
%% Get Roster
%% -----------------------------

get_roster(User, Server) ->
    Items = ejabberd_hooks:run_fold(roster_get, Server, [], [{User, Server}]),
    make_roster_xmlrpc(Items).

%% Note: if a contact is in several groups, the contact is returned
%% several times, each one in a different group.
make_roster_xmlrpc(Roster) ->
    lists:foldl(
      fun(Item, Res) ->
	      JIDS = jid:encode(Item#roster.jid),
	      Nick = Item#roster.name,
	      Subs = atom_to_list(Item#roster.subscription),
	      Ask = atom_to_list(Item#roster.ask),
	      Groups = case Item#roster.groups of
			   [] -> [<<>>];
			   Gs -> Gs
		       end,
	      ItemsX = [{JIDS, Nick, Subs, Ask, Group} || Group <- Groups],
	      ItemsX ++ Res
      end,
      [],
      Roster).


%%-----------------------------
%% Push Roster from file
%%-----------------------------

push_roster(File, User, Server) ->
    {ok, [Roster]} = file:consult(File),
    subscribe_roster({User, Server, <<>>, User}, Roster).

push_roster_all(File) ->
    {ok, [Roster]} = file:consult(File),
    subscribe_all(Roster).

subscribe_all(Roster) ->
    subscribe_all(Roster, Roster).
subscribe_all([], _) ->
    ok;
subscribe_all([User1 | Users], Roster) ->
    subscribe_roster(User1, Roster),
    subscribe_all(Users, Roster).

subscribe_roster(_, []) ->
    ok;
%% Do not subscribe a user to itself
subscribe_roster({Name, Server, Group, Nick}, [{Name, Server, _, _} | Roster]) ->
    subscribe_roster({Name, Server, Group, Nick}, Roster);
%% Subscribe Name2 to Name1
subscribe_roster({Name1, Server1, Group1, Nick1}, [{Name2, Server2, Group2, Nick2} | Roster]) ->
    subscribe(Name1, Server1, iolist_to_binary(Name2), iolist_to_binary(Server2),
	iolist_to_binary(Nick2), iolist_to_binary(Group2), <<"both">>, []),
    subscribe_roster({Name1, Server1, Group1, Nick1}, Roster).

push_alltoall(S, G) ->
    Users = ejabberd_auth:get_vh_registered_users(S),
    Users2 = build_list_users(G, Users, []),
    subscribe_all(Users2),
    ok.

build_list_users(_Group, [], Res) ->
    Res;
build_list_users(Group, [{User, Server}|Users], Res) ->
    build_list_users(Group, Users, [{User, Server, Group, User}|Res]).

%% @spec(LU, LS, U, S, Action) -> ok
%%       Action = {add, Nick, Subs, Group} | remove
%% @doc Push to the roster of account LU@LS the contact U@S.
%% The specific action to perform is defined in Action.
push_roster_item(LU, LS, U, S, Action) ->
    lists:foreach(fun(R) ->
			  push_roster_item(LU, LS, R, U, S, Action)
		  end, ejabberd_sm:get_user_resources(LU, LS)).

push_roster_item(LU, LS, R, U, S, Action) ->
    LJID = jid:make(LU, LS, R),
    BroadcastEl = build_broadcast(U, S, Action),
    ejabberd_sm:route(LJID, BroadcastEl),
    Item = build_roster_item(U, S, Action),
    ResIQ = build_iq_roster_push(Item),
    ejabberd_router:route(
      xmpp:set_from_to(ResIQ, jid:remove_resource(LJID), LJID)).

build_roster_item(U, S, {add, Nick, Subs, Group}) ->
    Groups = binary:split(Group,<<";">>, [global]),
    #roster_item{jid = jid:make(U, S),
		 name = Nick,
		 subscription = misc:binary_to_atom(Subs),
		 groups = Groups};
build_roster_item(U, S, remove) ->
    #roster_item{jid = jid:make(U, S), subscription = remove}.

build_iq_roster_push(Item) ->
    #iq{type = set, id = <<"push">>,
	sub_els = [#roster_query{items = [Item]}]}.

build_broadcast(U, S, {add, _Nick, Subs, _Group}) ->
    build_broadcast(U, S, list_to_atom(binary_to_list(Subs)));
build_broadcast(U, S, remove) ->
    build_broadcast(U, S, none);
%% @spec (U::binary(), S::binary(), Subs::atom()) -> any()
%% Subs = both | from | to | none
build_broadcast(U, S, SubsAtom) when is_atom(SubsAtom) ->
    {item, {U, S, <<>>}, SubsAtom}.

%%%
%%% Last Activity
%%%

get_last(User, Server) ->
    {Now, Status} = case ejabberd_sm:get_user_resources(User, Server) of
        [] ->
            case mod_last:get_last_info(User, Server) of
                not_found ->
		    {p1_time_compat:timestamp(), "NOT FOUND"};
                {ok, Shift, Status1} ->
                    {{Shift div 1000000, Shift rem 1000000, 0}, Status1}
            end;
        _ ->
	    {p1_time_compat:timestamp(), "ONLINE"}
    end,
    {xmpp_util:encode_timestamp(Now), Status}.

%%%
%%% Private Storage
%%%

%% Example usage:
%% $ ejabberdctl private_set badlop localhost "\<aa\ xmlns=\'bb\'\>Cluth\</aa\>"
%% $ ejabberdctl private_get badlop localhost aa bb
%% <aa xmlns='bb'>Cluth</aa>

private_get(Username, Host, Element, Ns) ->
    Els = mod_private:get_data(jid:nodeprep(Username), jid:nameprep(Host),
			       [Ns, Element]),
    binary_to_list(fxml:element_to_binary(xmpp:encode(#private{xml_els = Els}))).

private_set(Username, Host, ElementString) ->
    case fxml_stream:parse_element(ElementString) of
	{error, Error} ->
	    io:format("Error found parsing the element:~n  ~p~nError: ~p~n",
		      [ElementString, Error]),
	    error;
	Xml ->
	    private_set2(Username, Host, Xml)
    end.

private_set2(Username, Host, Xml) ->
    NS = fxml:get_tag_attr_s(<<"xmlns">>, Xml),
    mod_private:set_data(jid:nodeprep(Username), jid:nameprep(Host),
			 [{NS, Xml}]),
    ok.

%%%
%%% Shared Roster Groups
%%%

srg_create(Group, Host, Name, Description, Display) ->
    DisplayList = case Display of
	<<>> -> [];
	_ -> ejabberd_regexp:split(Display, <<"\\\\n">>)
    end,
    Opts = [{name, Name},
	    {displayed_groups, DisplayList},
	    {description, Description}],
    {atomic, _} = mod_shared_roster:create_group(Host, Group, Opts),
    ok.

srg_delete(Group, Host) ->
    {atomic, _} = mod_shared_roster:delete_group(Host, Group),
    ok.

srg_list(Host) ->
    lists:sort(mod_shared_roster:list_groups(Host)).

srg_get_info(Group, Host) ->
    Opts = case mod_shared_roster:get_group_opts(Host,Group) of
	Os when is_list(Os) -> Os;
	error -> []
    end,
    [{misc:atom_to_binary(Title), btl(Value)} || {Title, Value} <- Opts].

btl([]) -> [];
btl([B|L]) -> [btl(B)|btl(L)];
btl(B) -> binary_to_list(B).

srg_get_members(Group, Host) ->
    Members = mod_shared_roster:get_group_explicit_users(Host,Group),
    [jid:encode(jid:make(MUser, MServer))
     || {MUser, MServer} <- Members].

srg_user_add(User, Host, Group, GroupHost) ->
    {atomic, _} = mod_shared_roster:add_user_to_group(GroupHost, {User, Host}, Group),
    ok.

srg_user_del(User, Host, Group, GroupHost) ->
    {atomic, _} = mod_shared_roster:remove_user_from_group(GroupHost, {User, Host}, Group),
    ok.


%%%
%%% Stanza
%%%

%% @doc Send a message to a Jabber account.
%% @spec (Type::binary(), From::binary(), To::binary(), Subject::binary(), Body::binary()) -> ok
send_message(Type, From, To, Subject, Body) ->
    FromJID = jid:decode(From),
    ToJID = jid:decode(To),
    Packet = build_packet(Type, Subject, Body),
    ejabberd_router:route(xmpp:set_from_to(Packet, FromJID, ToJID)).

build_packet(Type, Subject, Body) ->
    #message{type = misc:binary_to_atom(Type),
	     body = xmpp:mk_text(Body),
	     subject = xmpp:mk_text(Subject)}.

send_stanza(FromString, ToString, Stanza) ->
    try
	#xmlel{} = El = fxml_stream:parse_element(Stanza),
	From = jid:decode(FromString),
	To = jid:decode(ToString),
	Pkt = xmpp:decode(El, ?NS_CLIENT, [ignore_els]),
	ejabberd_router:route(xmpp:set_from_to(Pkt, From, To))
    catch _:{xmpp_codec, Why} ->
	    io:format("incorrect stanza: ~s~n", [xmpp:format_error(Why)]),
	    {error, Why};
	  _:{badmatch, {error, Why}} ->
	    io:format("invalid xml: ~p~n", [Why]),
	    {error, Why};
	  _:{bad_jid, S} ->
	    io:format("malformed JID: ~s~n", [S]),
	    {error, "JID malformed"}
    end.

send_stanza_c2s(Username, Host, Resource, Stanza) ->
    case {fxml_stream:parse_element(Stanza),
          ejabberd_sm:get_session_pid(Username, Host, Resource)}
    of
	{{error, Error}, _} ->
	    {error, Error};
	{_, none} ->
	    {error, no_session};
	{XmlEl, C2sPid} ->
	    p1_fsm:send_event(C2sPid, {xmlstreamelement, XmlEl})
    end.

privacy_set(Username, Host, QueryS) ->
    From = jid:make(Username, Host),
    To = jid:make(Host),
    QueryEl = fxml_stream:parse_element(QueryS),
    SubEl = xmpp:decode(QueryEl),
    IQ = #iq{type = set, id = <<"push">>, sub_els = [SubEl],
	     from = From, to = To},
    ejabberd_hooks:run_fold(privacy_iq_set,
			    Host,
			    {error, xmpp:err_feature_not_implemented()},
			    [IQ, #userlist{}]),
    ok.

%%%
%%% Stats
%%%

stats(Name) ->
    case Name of
	<<"uptimeseconds">> -> trunc(element(1, erlang:statistics(wall_clock))/1000);
	<<"processes">> -> length(erlang:processes());
	<<"registeredusers">> -> lists:foldl(fun(Host, Sum) -> ejabberd_auth:get_vh_registered_users_number(Host) + Sum end, 0, ?MYHOSTS);
	<<"onlineusersnode">> -> length(ejabberd_sm:dirty_get_my_sessions_list());
	<<"onlineusers">> -> length(ejabberd_sm:dirty_get_sessions_list())
    end.

stats(Name, Host) ->
    case Name of
	<<"registeredusers">> -> ejabberd_auth:get_vh_registered_users_number(Host);
	<<"onlineusers">> -> length(ejabberd_sm:get_vh_session_list(Host))
    end.



%%-----------------------------
%% Purge roster items
%%-----------------------------

process_rosteritems(ActionS, SubsS, AsksS, UsersS, ContactsS) ->
    Action = case ActionS of
		 "list" -> list;
		 "delete" -> delete
	     end,

    Subs = lists:foldl(
	     fun(any, _) -> [none, from, to, both];
		(Sub, Subs) -> [Sub | Subs]
	     end,
	     [],
	     [list_to_atom(S) || S <- string:tokens(SubsS, ":")]
	    ),

    Asks = lists:foldl(
	     fun(any, _) -> [none, out, in];
		(Ask, Asks) -> [Ask | Asks]
	     end,
	     [],
	     [list_to_atom(S) || S <- string:tokens(AsksS, ":")]
	    ),

    Users = lists:foldl(
	      fun("any", _) -> ["*", "*@*"];
		 (U, Us) -> [U | Us]
	      end,
	      [],
	      [S || S <- string:tokens(UsersS, ":")]
	     ),

    Contacts = lists:foldl(
		 fun("any", _) -> ["*", "*@*"];
		    (U, Us) -> [U | Us]
		 end,
		 [],
		 [S || S <- string:tokens(ContactsS, ":")]
		),

    rosteritem_purge({Action, Subs, Asks, Users, Contacts}).

%% @spec ({Action::atom(), Subs::[atom()], Asks::[atom()], User::string(), Contact::string()}) -> {atomic, ok}
rosteritem_purge(Options) ->
    Num_rosteritems = mnesia:table_info(roster, size),
    io:format("There are ~p roster items in total.~n", [Num_rosteritems]),
    Key = mnesia:dirty_first(roster),
    rip(Key, Options, {0, Num_rosteritems, 0, 0}, []).

rip('$end_of_table', _Options, Counters, Res) ->
    print_progress_line(Counters),
    Res;
rip(Key, Options, {Pr, NT, NV, ND}, Res) ->
    Key_next = mnesia:dirty_next(roster, Key),
    {Action, _, _, _, _} = Options,
    {ND2, Res2} = case decide_rip(Key, Options) of
	      true ->
		  Jids = apply_action(Action, Key),
		  {ND+1, [Jids | Res]};
	      false ->
		  {ND, Res}
	  end,
    NV2 = NV+1,
    Pr2 = print_progress_line({Pr, NT, NV2, ND2}),
    rip(Key_next, Options, {Pr2, NT, NV2, ND2}, Res2).

apply_action(list, Key) ->
    {User, Server, JID} = Key,
    {RUser, RServer, _} = JID,
    Jid1string = <<User/binary, "@", Server/binary>>,
    Jid2string = <<RUser/binary, "@", RServer/binary>>,
    io:format("Matches: ~s ~s~n", [Jid1string, Jid2string]),
    {Jid1string, Jid2string};
apply_action(delete, Key) ->
    R = apply_action(list, Key),
    mnesia:dirty_delete(roster, Key),
    R.

print_progress_line({_Pr, 0, _NV, _ND}) ->
    ok;
print_progress_line({Pr, NT, NV, ND}) ->
    Pr2 = trunc((NV/NT)*100),
    case Pr == Pr2 of
	true ->
	    ok;
	false ->
	    io:format("Progress ~p% - visited ~p - deleted ~p~n", [Pr2, NV, ND])
    end,
    Pr2.

decide_rip(Key, {_Action, Subs, Asks, User, Contact}) ->
    case catch mnesia:dirty_read(roster, Key) of
	[RI] ->
	    lists:member(RI#roster.subscription, Subs)
		andalso lists:member(RI#roster.ask, Asks)
		andalso decide_rip_jid(RI#roster.us, User)
		andalso decide_rip_jid(RI#roster.jid, Contact);
	_ ->
	    false
    end.

%% Returns true if the server of the JID is included in the servers
decide_rip_jid({UName, UServer, _UResource}, Match_list) ->
    decide_rip_jid({UName, UServer}, Match_list);
decide_rip_jid({UName, UServer}, Match_list) ->
    lists:any(
      fun(Match_string) ->
	      MJID = jid:decode(list_to_binary(Match_string)),
	      MName = MJID#jid.luser,
	      MServer = MJID#jid.lserver,
	      Is_server = is_glob_match(UServer, MServer),
	      case MName of
		  <<>> when UName == <<>> ->
		      Is_server;
		  <<>> ->
		      false;
		  _ ->
		      Is_server
			  andalso is_glob_match(UName, MName)
	      end
      end,
      Match_list).

user_action(User, Server, Fun, OK) ->
    case ejabberd_auth:is_user_exists(User, Server) of
        true ->
 	    case catch Fun() of
                OK -> ok;
 		{error, Error} -> throw(Error);
                Error ->
                    ?ERROR_MSG("Command returned: ~p", [Error]),
 		    1
 	    end;
 	false ->
 	    throw({not_found, "unknown_user"})
    end.

%% Copied from ejabberd-2.0.0/src/acl.erl
is_regexp_match(String, RegExp) ->
    case ejabberd_regexp:run(String, RegExp) of
	nomatch ->
	    false;
	match ->
	    true;
	{error, ErrDesc} ->
	    io:format(
	      "Wrong regexp ~p in ACL: ~p",
	      [RegExp, ErrDesc]),
	    false
    end.
is_glob_match(String, <<"!", Glob/binary>>) ->
    not is_regexp_match(String, ejabberd_regexp:sh_to_awk(Glob));
is_glob_match(String, Glob) ->
    is_regexp_match(String, ejabberd_regexp:sh_to_awk(Glob)).

mod_opt_type(_) -> [].
