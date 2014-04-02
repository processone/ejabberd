%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%%-------------------------------------------------------------------

-module(mod_admin_extra).
-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 %% Node
	 compile/1,
	 load_config/1,
	 get_cookie/0,
	 remove_node/1,
	 export2odbc/2,
	 %% Accounts
	 set_password/3,
	 check_password_hash/4,
	 delete_old_users/1,
	 delete_old_users_vhost/2,
	 ban_account/3,
	 num_active_users/2,
	 %% Sessions
	 num_resources/2,
	 resource_num/3,
	 kick_session/4,
	 status_num/2, status_num/1,
	 status_list/2, status_list/1,
	 connected_users_info/0,
	 connected_users_vhost/1,
	 set_presence/7,
	 user_sessions_info/2,
	 %% Vcard
	 set_nickname/3,
	 get_vcard/3,
	 get_vcard/4,
	 get_vcard_multi/4,
	 set_vcard/4,
	 set_vcard/5,
	 %% Roster
	 add_rosteritem/7,
	 delete_rosteritem/4,
	 process_rosteritems/5,
	 get_roster/2,
	 push_roster/3,
	 push_roster_all/1,
	 push_alltoall/2,
	 %% mod_last
	 get_last/2,
	 set_last/4,
	 %% mod_private
	 private_get/4,
	 private_set/3,
	 %% mod_shared_roster
	 srg_create/5,
	 srg_delete/2,
	 srg_list/1,
	 srg_get_info/2,
	 srg_get_members/2,
	 srg_user_add/4,
	 srg_user_del/4,
	 %% Stanza
	 send_message_headline/4,
	 send_message_chat/3,
	 send_stanza_c2s/4,
	 privacy_set/3,
	 %% Stats
	 stats/1, stats/2
	]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").

%% Copied from ejabberd_sm.erl
-record(session, {sid, usr, us, priority, info}).


%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).


%%%
%%% Register commands
%%%

commands() ->
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
			result = {res, rescode}},
     #ejabberd_commands{name = load_config, tags = [server],
			desc = "Load ejabberd configuration file",
			module = ?MODULE, function = load_config,
			args = [{file, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = get_cookie, tags = [erlang],
			desc = "Get the Erlang cookie of this node",
			module = ?MODULE, function = get_cookie,
			args = [],
			result = {cookie, string}},
     #ejabberd_commands{name = remove_node, tags = [erlang],
			desc = "Remove an ejabberd node from Mnesia clustering config",
			module = ?MODULE, function = remove_node,
			args = [{node, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = export2odbc, tags = [mnesia], %% Copied to ejabberd 2.1.x after 11
			desc = "Export Mnesia tables to files in directory",
			module = ?MODULE, function = export2odbc,
			args = [{host, string}, {path, string}],
			result = {res, rescode}},

     #ejabberd_commands{name = num_active_users, tags = [accounts, stats],
			desc = "Get number of users active in the last days",
			module = ?MODULE, function = num_active_users,
			args = [{host, binary}, {days, integer}],
			result = {users, integer}},
     #ejabberd_commands{name = delete_old_users, tags = [accounts, purge],
			desc = "Delete users that didn't log in last days, or that never logged",
			module = ?MODULE, function = delete_old_users,
			args = [{days, integer}],
			result = {res, restuple}},
     #ejabberd_commands{name = delete_old_users_vhost, tags = [accounts, purge],
			desc = "Delete users that didn't log in last days in vhost, or that never logged",
			module = ?MODULE, function = delete_old_users_vhost,
			args = [{host, binary}, {days, integer}],
			result = {res, restuple}},

     #ejabberd_commands{name = check_account, tags = [accounts],
			desc = "Check if an account exists or not",
			module = ejabberd_auth, function = is_user_exists,
			args = [{user, binary}, {host, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = check_password, tags = [accounts],
			desc = "Check if a password is correct",
			module = ejabberd_auth, function = check_password,
			args = [{user, binary}, {host, binary}, {password, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = check_password_hash, tags = [accounts],
			desc = "Check if the password hash is correct",
			longdesc = "Allowed hash methods: md5, sha.",
			module = ?MODULE, function = check_password_hash,
			args = [{user, binary}, {host, binary}, {passwordhash, binary}, {hashmethod, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = change_password, tags = [accounts],
			desc = "Change the password of an account",
			module = ?MODULE, function = set_password,
			args = [{user, binary}, {host, binary}, {newpass, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = ban_account, tags = [accounts],
			desc = "Ban an account: kick sessions and set random password",
			module = ?MODULE, function = ban_account,
			args = [{user, binary}, {host, binary}, {reason, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = num_resources, tags = [session],
			desc = "Get the number of resources of a user",
			module = ?MODULE, function = num_resources,
			args = [{user, binary}, {host, binary}],
			result = {resources, integer}},
     #ejabberd_commands{name = resource_num, tags = [session],
			desc = "Resource string of a session number",
			module = ?MODULE, function = resource_num,
			args = [{user, binary}, {host, binary}, {num, integer}],
			result = {resource, string}},
     #ejabberd_commands{name = kick_session, tags = [session],
			desc = "Kick a user session",
			module = ?MODULE, function = kick_session,
			args = [{user, string}, {host, string}, {resource, string}, {reason, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = status_num_host, tags = [session, stats],
			desc = "Number of logged users with this status in host",
			module = ?MODULE, function = status_num,
			args = [{host, string}, {status, string}],
			result = {users, integer}},
     #ejabberd_commands{name = status_num, tags = [session, stats],
			desc = "Number of logged users with this status",
			module = ?MODULE, function = status_num,
			args = [{status, string}],
			result = {users, integer}},
     #ejabberd_commands{name = status_list_host, tags = [session],
			desc = "List of users logged in host with their statuses",
			module = ?MODULE, function = status_list,
			args = [{host, string}, {status, string}],
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
			args = [{status, string}],
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
                       args = [{host, string}],
                       result = {connected_users_vhost, {list, {sessions, string}}}},
     #ejabberd_commands{name = user_sessions_info,
			tags = [session],
			desc = "Get information about all sessions of a user",
			module = ?MODULE, function = user_sessions_info,
			args = [{user, string}, {host, string}],
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

     #ejabberd_commands{name = set_presence,
			tags = [session],
			desc = "Set presence of a session",
			module = ?MODULE, function = set_presence,
			args = [{user, string}, {host, string},
				{resource, string}, {type, string},
				{show, string}, {status, string},
				{priority, string}],
			result = {res, rescode}},

     #ejabberd_commands{name = set_nickname, tags = [vcard],
			desc = "Set nickname in a user's vCard",
			module = ?MODULE, function = set_nickname,
			args = [{user, string}, {host, string}, {nickname, string}],
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
			desc = "Get multiple contents from a vCard field (requires exmpp installed)",
			longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
			module = ?MODULE, function = get_vcard_multi,
			args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}],
			result = {contents, {list, string}}},

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
			args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}, {contents, {list, binary}}],
			result = {res, rescode}},

     #ejabberd_commands{name = add_rosteritem, tags = [roster],
			desc = "Add an item to a user's roster (supports ODBC)",
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
			desc = "List or delete rosteritems that match filtering options",
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
			module = ?MODULE, function = get_roster,
			args = [{user, binary}, {host, binary}],
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
			args = [{file, string}, {user, string}, {host, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = push_roster_all, tags = [roster],
			desc = "Push template roster from file to all those users",
			module = ?MODULE, function = push_roster_all,
			args = [{file, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = push_alltoall, tags = [roster],
			desc = "Add all the users to all the users of Host in Group",
			module = ?MODULE, function = push_alltoall,
			args = [{host, string}, {group, string}],
			result = {res, rescode}},

     #ejabberd_commands{name = get_last, tags = [last],
			desc = "Get last activity information",
			longdesc = "Timestamp is the seconds since"
			"1970-01-01 00:00:00 UTC, for example: date +%s",
			module = ?MODULE, function = get_last,
			args = [{user, binary}, {host, binary}],
			result = {last_activity, string}},
     #ejabberd_commands{name = set_last, tags = [last],
			desc = "Set last activity information",
			longdesc = "Timestamp is the seconds since"
			"1970-01-01 00:00:00 UTC, for example: date +%s",
			module = ?MODULE, function = set_last,
			args = [{user, string}, {host, string}, {timestamp, integer}, {status, string}],
			result = {res, rescode}},

     #ejabberd_commands{name = private_get, tags = [private],
			desc = "Get some information from a user private storage",
			module = ?MODULE, function = private_get,
			args = [{user, string}, {host, string}, {element, string}, {ns, string}],
			result = {res, string}},
     #ejabberd_commands{name = private_set, tags = [private],
			desc = "Set to the user private storage",
			module = ?MODULE, function = private_set,
			args = [{user, string}, {host, string}, {element, string}],
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

     #ejabberd_commands{name = send_message_chat, tags = [stanza],
			desc = "Send a chat message to a local or remote bare of full JID",
			module = ?MODULE, function = send_message_chat,
			args = [{from, string}, {to, string}, {body, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = send_message_headline, tags = [stanza],
			desc = "Send a headline message to a local or remote bare of full JID",
			module = ?MODULE, function = send_message_headline,
			args = [{from, string}, {to, string},
				{subject, string}, {body, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = send_stanza_c2s, tags = [stanza],
			desc = "Send a stanza as if sent from a c2s session",
			module = ?MODULE, function = send_stanza_c2s,
			args = [{user, string}, {host, string}, {resource, string}, {stanza, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = privacy_set, tags = [stanza],
			desc = "Send a IQ set privacy stanza for a local account",
			module = ?MODULE, function = privacy_set,
			args = [{user, string}, {host, string}, {xmlquery, string}],
			result = {res, rescode}},

     #ejabberd_commands{name = stats, tags = [stats],
			desc = "Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds",
			module = ?MODULE, function = stats,
			args = [{name, string}],
			result = {stat, integer}},
     #ejabberd_commands{name = stats_host, tags = [stats],
			desc = "Get statistical value for this host: registeredusers onlineusers",
			module = ?MODULE, function = stats,
			args = [{name, string}, {host, string}],
			result = {stat, integer}}
    ].


%%%
%%% Node
%%%

compile(File) ->
    case compile:file(File) of
	ok -> ok;
	_ -> error
    end.

load_config(Path) ->
    ok = ejabberd_config:load_file(Path).

get_cookie() ->
    atom_to_list(erlang:get_cookie()).

remove_node(Node) ->
    mnesia:del_table_copy(schema, list_to_atom(Node)),
    ok.

export2odbc(Host, Directory) ->
    Tables = [
	      {export_last, last},
	      {export_offline, offline},
	      {export_passwd, passwd},
	      {export_private_storage, private_storage},
	      {export_roster, roster},
	      {export_vcard, vcard},
	      {export_vcard_search, vcard_search}],
    Export = fun({TableFun, Table}) ->
		     Filename = filename:join([Directory, atom_to_list(Table)++".txt"]),
		     io:format("Trying to export Mnesia table '~p' on Host '~s' to file '~s'~n", [Table, Host, Filename]),
		     Res = (catch ejd2odbc:TableFun(Host, Filename)),
		     io:format("  Result: ~p~n", [Res])
	     end,
    lists:foreach(Export, Tables),
    ok.


%%%
%%% Accounts
%%%

set_password(User, Host, Password) ->
    case ejabberd_auth:set_password(User, Host, Password) of
	ok ->
	    ok;
	_ ->
	    error
    end.

%% Copied some code from ejabberd_commands.erl
check_password_hash(User, Host, PasswordHash, HashMethod) ->
    AccountPass = ejabberd_auth:get_password_s(User, Host),
    AccountPassHash = case HashMethod of
			  "md5" -> get_md5(AccountPass);
			  "sha" -> get_sha(AccountPass);
			  _ -> undefined
		      end,
    case AccountPassHash of
	undefined -> error;
	PasswordHash -> ok;
	_ -> error
    end.
get_md5(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
		   || X <- binary_to_list(crypto:md5(AccountPass))]).
get_sha(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
		   || X <- binary_to_list(crypto:sha(AccountPass))]).

num_active_users(Host, Days) ->
    list_last_activity(Host, true, Days).

%% Code based on ejabberd/src/web/ejabberd_web_admin.erl
list_last_activity(Host, Integral, Days) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
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
	 end.
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
    %% Convert older time
    SecOlder = Days*24*60*60,

    %% Get current time
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp_now = MegaSecs * 1000000 + Secs,

    %% For a user, remove if required and answer true
    F = fun({LUser, LServer}) ->
		%% Check if the user is logged
		case ejabberd_sm:get_user_resources(LUser, LServer) of
		    %% If it isnt
		    [] ->
			%% Look for his last_activity
			case (get_lastactivity_module(LServer)):get_last_info(LUser, LServer) of
			    %% If it is
			    %% existent:
			    {ok, TimeStamp, _Status} ->
				%% get his age
				Sec = TimeStamp_now - TimeStamp,
				%% If he is
				if
				    %% younger than SecOlder:
				    Sec < SecOlder ->
					%% do nothing
					false;
				    %% older:
				    true ->
					%% remove the user
					ejabberd_auth:remove_user(LUser, LServer),
					true
				end;
			    %% nonexistent:
			    not_found ->
				%% remove the user
				ejabberd_auth:remove_user(LUser, LServer),
				true
			end;
		    %% Else
		    _ ->
			%% do nothing
			false
		end
	end,
    %% Apply the function to every user in the list
    Users_removed = lists:filter(F, Users),
    {removed, length(Users_removed), Users_removed}.

get_lastactivity_module(Server) ->
    case lists:member(mod_last, gen_mod:loaded_modules(Server)) of
        true -> mod_last;
        _ -> mod_last_odbc
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
      get_resources(User, Server)).

get_resources(User, Server) ->
    lists:map(
      fun(Session) ->
	      element(3, Session#session.usr)
      end,
      get_sessions(User, Server)).

get_sessions(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Sessions =  mnesia:dirty_index_read(session, {LUser, LServer}, #session.us),
    true = is_list(Sessions),
    Sessions.

set_random_password(User, Server, Reason) ->
    NewPass = build_random_password(Reason),
    set_password_auth(User, Server, NewPass).

build_random_password(Reason) ->
    Date = jlib:timestamp_to_iso(calendar:universal_time()),
    RandomString = randoms:get_string(),
    "BANNED_ACCOUNT--" ++ Date ++ "--" ++ RandomString ++ "--" ++ Reason.

set_password_auth(User, Server, Password) ->
    ok = ejabberd_auth:set_password(User, Server, Password).

prepare_reason([]) ->
    "Kicked by administrator";
prepare_reason([Reason]) ->
    Reason;
prepare_reason(Reason) when is_list(Reason) ->
    Reason;
prepare_reason(StringList) ->
    string:join(StringList, "_").


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
	    lists:flatten(io_lib:format("Error: Wrong resource number: ~p", [Num]))
    end.

kick_session(User, Server, Resource, ReasonText) ->
    kick_this_session(User, Server, Resource, prepare_reason(ReasonText)),
    ok.

kick_this_session(User, Server, Resource, Reason) ->
    ejabberd_router:route(
      jlib:make_jid("", "", ""),
      jlib:make_jid(User, Server, Resource),
      {xmlelement, "broadcast", [], [{exit, Reason}]}).


status_num(Host, Status) ->
    length(get_status_list(Host, Status)).
status_num(Status) ->
    status_num("all", Status).
status_list(Host, Status) ->
    Res = get_status_list(Host, Status),
    [{U, S, R, P, St} || {U, S, R, P, St} <- Res].
status_list(Status) ->
    status_list("all", Status).


get_status_list(Host, Status_required) ->
    %% Get list of all logged users
    Sessions = ejabberd_sm:dirty_get_my_sessions_list(),
    %% Reformat the list
    Sessions2 = [ {Session#session.usr, Session#session.sid, Session#session.priority} || Session <- Sessions],
    Fhost = case Host of
		"all" ->
		    %% All hosts are requested, so dont filter at all
		    fun(_, _) -> true end;
		_ ->
		    %% Filter the list, only Host is interesting
		    fun(A, B) -> A == B end
	    end,
    Sessions3 = [ {Pid, Server, Priority} || {{_User, Server, _Resource}, {_, Pid}, Priority} <- Sessions2, apply(Fhost, [Server, Host])],
    %% For each Pid, get its presence
    Sessions4 = [ {ejabberd_c2s:get_presence(Pid), Server, Priority} || {Pid, Server, Priority} <- Sessions3],
    %% Filter by status
    Fstatus = case Status_required of
		  "all" ->
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
	      {[U, $@, S, $/, R], atom_to_list(Conn), IPS, Port, Priority, NodeS, Uptime}
      end,
      USRIs).

connected_users_vhost(Host) ->
    USRs = ejabberd_sm:get_vh_session_list(Host),
    [ [U, $@, S, $/, R] || {U, S, R} <- USRs].

%% Code copied from ejabberd_sm.erl and customized
dirty_get_sessions_list2() ->
    mnesia:dirty_select(
      session,
      [{#session{usr = '$1', sid = '$2', priority = '$3', info = '$4', _ = '_'},
	[],
	[['$1', '$2', '$3', '$4']]}]).

%% Make string more print-friendly
stringize(String) ->
    %% Replace newline characters with other code
    ejabberd_regexp:greplace(String, "\n", "\\n").

set_presence(User, Host, Resource, Type, Show, Status, Priority) ->
    Pid = ejabberd_sm:get_session_pid(User, Host, Resource),
    USR = User ++ "@" ++ Host ++ "/" ++ Resource,
    US = User ++ "@" ++ Host,
    Message = {route_xmlstreamelement,
	       {xmlelement, "presence",
		[{"from", USR}, {"to", US}, {"type", Type}],
		[{xmlelement, "show", [], [{xmlcdata, Show}]},
		 {xmlelement, "status", [], [{xmlcdata, Status}]},
		 {xmlelement, "priority", [], [{xmlcdata, Priority}]}]}},
    Pid ! Message.

user_sessions_info(User, Host) ->
    CurrentSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    US = {User, Host},
    Sessions = case catch mnesia:dirty_index_read(session, US, #session.us) of
		   {'EXIT', _Reason} ->
		       [];
		   Ss ->
		       Ss
	       end,
    lists:map(
      fun(Session) ->
	      {_U, _S, Resource} = Session#session.usr,
	      {Now, Pid} = Session#session.sid,
	      {_U, _Resource, Status, StatusText} = ejabberd_c2s:get_presence(Pid),
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
    R = mod_vcard:process_sm_iq(
	  {jid, User, Host, "", User, Host, ""},
	  {jid, User, Host, "", User, Host, ""},
	  {iq, "", set, "", "en",
	   {xmlelement, "vCard",
	    [{"xmlns", "vcard-temp"}], [
					{xmlelement, "NICKNAME", [], [{xmlcdata, Nickname}]}
				       ]
	   }}),
    case R of
	{iq, [], result, [], _L, []} ->
	    ok;
	_ ->
	    error
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

get_module_resource(Server) ->
    case gen_mod:get_module_opt(Server, ?MODULE, module_resource, fun(A) -> A end, none) of
	none -> list_to_binary(atom_to_list(?MODULE));
	R when is_binary(R) -> R
    end.

get_vcard_content(User, Server, Data) ->
    [{_, Module, Function, _Opts}] = ets:lookup(sm_iqtable, {?NS_VCARD, Server}),
    JID = jlib:make_jid(User, Server, get_module_resource(Server)),
    IQ = #iq{type = get, xmlns = ?NS_VCARD},
    IQr = Module:Function(JID, JID, IQ),
    case IQr#iq.sub_el of
	[A1] ->
	    case get_vcard(Data, A1) of
		[] -> throw(error_no_value_found_in_vcard);
		ElemList -> [xml:get_tag_cdata(Elem) || Elem <- ElemList]
	    end;
	[] ->
	    throw(error_no_vcard_found)
    end.

get_vcard([Data1, Data2], A1) ->
    case get_subtag(A1, Data1) of
    	false -> false;
	A2List -> lists:flatten([get_vcard([Data2], A2) || A2 <- A2List])
    end;

get_vcard([Data], A1) ->
    get_subtag(A1, Data).

get_subtag(Xmlelement, Name) ->
    case code:ensure_loaded(exmpp_xml) of
	{error, _} ->
	    [get_subtag_xml(Xmlelement, Name)];
	{module, exmpp_xml} ->
	    get_subtag_exmpp(Xmlelement, Name)
    end.

get_subtag_xml(Xmlelement, Name) ->
    xml:get_subtag(Xmlelement, Name).

get_subtag_exmpp(Xmlelement, Name) ->
    Xmlel = exmpp_xml:xmlelement_to_xmlel(Xmlelement),
    XmlelList = exmpp_xml:get_elements(Xmlel, Name),
    [exmpp_xml:xmlel_to_xmlelement(Xmlel2) || Xmlel2 <- XmlelList].

set_vcard_content(User, Server, Data, SomeContent) ->
    ContentList = case SomeContent of
	[Bin | _] when is_binary(Bin) -> SomeContent;
	Bin when is_binary(Bin) -> [SomeContent]
    end,
    [{_, Module, Function, _Opts}] = ets:lookup(sm_iqtable, {?NS_VCARD, Server}),
    JID = jlib:make_jid(User, Server, get_module_resource(Server)),
    IQ = #iq{type = get, xmlns = ?NS_VCARD},
    IQr = Module:Function(JID, JID, IQ),

    %% Get old vcard
    A4 = case IQr#iq.sub_el of
	     [A1] ->
		 {_, _, _, A2} = A1,
		 update_vcard_els(Data, ContentList, A2);
	     [] ->
		 update_vcard_els(Data, ContentList, [])
	 end,

    %% Build new vcard
    SubEl = {xmlel, <<"vCard">>, [{<<"xmlns">>,<<"vcard-temp">>}], A4},
    IQ2 = #iq{type=set, sub_el = SubEl},

    Module:Function(JID, JID, IQ2),
    ok.

update_vcard_els(Data, ContentList, Els1) ->
    Els2 = lists:keysort(2, Els1),
    [Data1 | Data2] = Data,
    NewEls = case Data2 of
		[] ->
		    [{xmlel, Data1, [], [{xmlcdata,Content}]} || Content <- ContentList];
		[D2] ->
		    OldEl = case lists:keysearch(Data1, 2, Els2) of
				{value, A} -> A;
				false -> {xmlelement, Data1, [], []}
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
    mod_roster:set_items(
	LU, LS,
	{xmlel, <<"query">>,
            [{<<"xmlns">>, <<"jabber:iq:roster">>}],
            [ItemEl]}).

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
    mod_roster:set_items(
	LU, LS,
	{xmlel, <<"query">>,
            [{<<"xmlns">>, <<"jabber:iq:roster">>}],
            [ItemEl]}).

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
	      JIDS = jlib:jid_to_string(Item#roster.jid),
	      Nick = Item#roster.name,
	      Subs = atom_to_list(Item#roster.subscription),
	      Ask = atom_to_list(Item#roster.ask),
	      Groups = case Item#roster.groups of
			   [] -> [""];
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
    subscribe_roster({User, Server, "", User}, Roster).

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
    subscribe(Name1, Server1, Name2, Server2, Nick2, Group2, <<"both">>, []),
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
    LJID = jlib:make_jid(LU, LS, R),
    BroadcastEl = build_broadcast(U, S, Action),
    ejabberd_router:route(LJID, LJID, BroadcastEl),
    Item = build_roster_item(U, S, Action),
    ResIQ = build_iq_roster_push(Item),
    ejabberd_router:route(LJID, LJID, ResIQ).

build_roster_item(U, S, {add, Nick, Subs, Group}) ->
    {xmlel, <<"item">>,
     [{<<"jid">>, jlib:jid_to_string(jlib:make_jid(U, S, <<>>))},
      {<<"name">>, Nick},
      {<<"subscription">>, Subs}],
     [{xmlel, <<"group">>, [], [{xmlcdata, Group}]}]
    };
build_roster_item(U, S, remove) ->
    {xmlel, <<"item">>,
     [{<<"jid">>, jlib:jid_to_string(jlib:make_jid(U, S, <<>>))},
      {<<"subscription">>, <<"remove">>}],
     []
    }.

build_iq_roster_push(Item) ->
    {xmlel, <<"iq">>,
     [{<<"type">>, <<"set">>}, {<<"id">>, <<"pushaaa">>}],
     [{xmlel, <<"query">>,
       [{<<"xmlns">>, ?NS_ROSTER}],
       [Item]
      }
     ]
    }.

build_broadcast(U, S, {add, _Nick, Subs, _Group}) ->
    build_broadcast(U, S, list_to_atom(binary_to_list(Subs)));
build_broadcast(U, S, remove) ->
    build_broadcast(U, S, none);
%% @spec (U::string(), S::string(), Subs::atom()) -> any()
%% Subs = both | from | to | none
build_broadcast(U, S, SubsAtom) when is_atom(SubsAtom) ->
    {broadcast, {item, {U, S, <<>>}, SubsAtom}
    }.

%%%
%%% Last Activity
%%%

get_last(User, Server) ->
    Mod = get_lastactivity_module(Server),
    case ejabberd_sm:get_user_resources(User, Server) of
        [] ->
            case Mod:get_last_info(User, Server) of
                not_found ->
                    "Never";
                {ok, Shift, _Status} ->
                    TimeStamp = {Shift div 1000000,
                        Shift rem 1000000,
                        0},
                    {{Year, Month, Day}, {Hour, Minute, Second}} =
                        calendar:now_to_local_time(TimeStamp),
                    lists:flatten(
                        io_lib:format(
                            "~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                            [Year, Month, Day, Hour, Minute, Second]))
            end;
        _ ->
            "Online"
    end.

set_last(User, Server, Timestamp, Status) ->
    Mod = get_lastactivity_module(Server),
    Mod:store_last_info(User, Server, Timestamp, Status).

%%%
%%% Private Storage
%%%

%% Example usage:
%% $ ejabberdctl private_set badlop localhost "\<aa\ xmlns=\'bb\'\>Cluth\</aa\>"
%% $ ejabberdctl private_get badlop localhost aa bb
%% <aa xmlns='bb'>Cluth</aa>

private_get(Username, Host, Element, Ns) ->
    From = jlib:make_jid(Username, Host, ""),
    To = jlib:make_jid(Username, Host, ""),
    IQ = {iq, "", get, ?NS_PRIVATE, "",
	  {xmlelement,"query",
	   [{"xmlns",?NS_PRIVATE}],
	   [{xmlelement, Element, [{"xmlns", Ns}], []}]}},
    ResIq = mod_private:process_sm_iq(From, To, IQ),
    [{xmlelement,"query",
      [{"xmlns","jabber:iq:private"}],
      [SubEl]}] = ResIq#iq.sub_el,
    xml:element_to_string(SubEl).

private_set(Username, Host, ElementString) ->
    case xml_stream:parse_element(ElementString) of
	{error, Error} ->
	    io:format("Error found parsing the element:~n  ~p~nError: ~p~n",
		      [ElementString, Error]),
	    error;
	Xml ->
	    private_set2(Username, Host, Xml)
    end.

private_set2(Username, Host, Xml) ->
    From = jlib:make_jid(Username, Host, ""),
    To = jlib:make_jid(Username, Host, ""),
    IQ = {iq, "", set, ?NS_PRIVATE, "",
	  {xmlelement,"query",
	   [{"xmlns",?NS_PRIVATE}],
	   [Xml]}},
    mod_private:process_sm_iq(From, To, IQ),
    ok.

%%%
%%% Shared Roster Groups
%%%

srg_create(Group, Host, Name, Description, Display) ->
    DisplayList = case Display of
	[] -> [];
	_ -> ejabberd_regexp:split(Display, <<"\\\\n">>)
    end,
    Opts = [{name, Name},
	    {displayed_groups, DisplayList},
	    {description, Description}],
    {atomic, ok} = mod_shared_roster:create_group(Host, Group, Opts),
    ok.

srg_delete(Group, Host) ->
    {atomic, ok} = mod_shared_roster:delete_group(Host, Group),
    ok.

srg_list(Host) ->
    lists:sort(mod_shared_roster:list_groups(Host)).

srg_get_info(Group, Host) ->
    Opts = case mod_shared_roster:get_group_opts(Host,Group) of
	Os when is_list(Os) -> Os;
	error -> []
    end,
    [{io_lib:format("~p", [Title]),
      io_lib:format("~p", [Value])} || {Title, Value} <- Opts].

srg_get_members(Group, Host) ->
    Members = mod_shared_roster:get_group_explicit_users(Host,Group),
    [jlib:jid_to_string(jlib:make_jid(MUser, MServer, <<"">>))
     || {MUser, MServer} <- Members].

srg_user_add(User, Host, Group, GroupHost) ->
    {atomic, ok} = mod_shared_roster:add_user_to_group(GroupHost, {User, Host}, Group),
    ok.

srg_user_del(User, Host, Group, GroupHost) ->
    {atomic, ok} = mod_shared_roster:remove_user_from_group(GroupHost, {User, Host}, Group),
    ok.


%%%
%%% Stanza
%%%

%% @doc Send a chat message to a Jabber account.
%% @spec (From::string(), To::string(), Body::string()) -> ok
send_message_chat(From, To, Body) ->
    Packet = build_packet(message_chat, [Body]),
    send_packet_all_resources(From, To, Packet).

%% @doc Send a headline message to a Jabber account.
%% @spec (From::string(), To::string(), Subject::string(), Body::string()) -> ok
send_message_headline(From, To, Subject, Body) ->
    Packet = build_packet(message_headline, [Subject, Body]),
    send_packet_all_resources(From, To, Packet).

%% @doc Send a packet to a Jabber account.
%% If a resource was specified in the JID,
%% the packet is sent only to that specific resource.
%% If no resource was specified in the JID,
%% and the user is remote or local but offline,
%% the packet is sent to the bare JID.
%% If the user is local and is online in several resources,
%% the packet is sent to all its resources.
send_packet_all_resources(FromJIDString, ToJIDString, Packet) ->
    FromJID = jlib:string_to_jid(FromJIDString),
    ToJID = jlib:string_to_jid(ToJIDString),
    ToUser = ToJID#jid.user,
    ToServer = ToJID#jid.server,
    case ToJID#jid.resource of
	"" ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, Packet);
	Res ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, Res, Packet)
    end.

send_packet_all_resources(FromJID, ToUser, ToServer, Packet) ->
    case ejabberd_sm:get_user_resources(ToUser, ToServer) of
	[] ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, "", Packet);
	ToResources ->
	    lists:foreach(
	      fun(ToResource) ->
		      send_packet_all_resources(FromJID, ToUser, ToServer,
						ToResource, Packet)
	      end,
	      ToResources)
    end.

send_packet_all_resources(FromJID, ToU, ToS, ToR, Packet) ->
    ToJID = jlib:make_jid(ToU, ToS, ToR),
    ejabberd_router:route(FromJID, ToJID, Packet).


build_packet(message_chat, [Body]) ->
    {xmlelement, "message",
     [{"type", "chat"}, {"id", randoms:get_string()}],
     [{xmlelement, "body", [], [{xmlcdata, Body}]}]
    };
build_packet(message_headline, [Subject, Body]) ->
    {xmlelement, "message",
     [{"type", "headline"}, {"id", randoms:get_string()}],
     [{xmlelement, "subject", [], [{xmlcdata, Subject}]},
      {xmlelement, "body", [], [{xmlcdata, Body}]}
     ]
    }.

send_stanza_c2s(Username, Host, Resource, Stanza) ->
    C2sPid = ejabberd_sm:get_session_pid(Username, Host, Resource),
    XmlEl = xml_stream:parse_element(Stanza),
    p1_fsm:send_event(C2sPid, {xmlstreamelement, XmlEl}).

privacy_set(Username, Host, QueryS) ->
    From = jlib:string_to_jid(Username ++ "@" ++ Host),
    To = jlib:string_to_jid(Host),
    QueryEl = xml_stream:parse_element(QueryS),
    StanzaEl = {xmlelement, "iq", [{"type", "set"}], [QueryEl]},
    IQ = jlib:iq_query_info(StanzaEl),
    ejabberd_hooks:run_fold(
		     privacy_iq_set,
		     Host,
		     {error, ?ERR_FEATURE_NOT_IMPLEMENTED},
		     [From, To, IQ]
		    ),
    ok.

%%%
%%% Stats
%%%

stats(Name) ->
    case Name of
	"uptimeseconds" -> trunc(element(1, erlang:statistics(wall_clock))/1000);
	"registeredusers" -> length(ejabberd_auth:dirty_get_registered_users());
	"onlineusersnode" -> length(ejabberd_sm:dirty_get_my_sessions_list());
	"onlineusers" -> length(ejabberd_sm:dirty_get_sessions_list())
    end.

stats(Name, Host) ->
    case Name of
	"registeredusers" -> length(ejabberd_auth:get_vh_registered_users(Host));
	"onlineusers" -> length(ejabberd_sm:get_vh_session_list(Host))
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

    case rosteritem_purge({Action, Subs, Asks, Users, Contacts}) of
	{atomic, Res} ->
	    Res;
	{error, Reason} ->
	    io:format("Error purging rosteritems: ~p~n", [Reason]),
	    error;
	{badrpc, Reason} ->
	    io:format("BadRPC purging rosteritems: ~p~n", [Reason]),
	    error
    end.

%% @spec ({Action::atom(), Subs::[atom()], Asks::[atom()], User::string(), Contact::string()}) -> {atomic, ok}
rosteritem_purge(Options) ->
    Num_rosteritems = mnesia:table_info(roster, size),
    io:format("There are ~p roster items in total.~n", [Num_rosteritems]),
    Key = mnesia:dirty_first(roster),
    Res = rip(Key, Options, {0, Num_rosteritems, 0, 0}, []),
    {atomic, Res}.

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
    Jid1string = User ++ "@" ++ Server,
    Jid2string = RUser ++ "@" ++ RServer,
    io:format("Matches: ~s ~s~n", [Jid1string, Jid2string]),
    {Jid1string, Jid2string};
apply_action(delete, Key) ->
    R = apply_action(list, Key),
    mnesia:dirty_delete(roster, Key),
    R.

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
	      MJID = jlib:string_to_jid(Match_string),
	      MName = MJID#jid.luser,
	      MServer = MJID#jid.lserver,
	      Is_server = is_glob_match(UServer, MServer),
	      case MName of
		  [] when UName == [] ->
		      Is_server;
		  [] ->
		      false;
		  _ ->
		      Is_server
			  andalso is_glob_match(UName, MName)
	      end
      end,
      Match_list).

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
is_glob_match(String, [$! | Glob]) ->
    not is_regexp_match(String, ejabberd_regexp:sh_to_awk(Glob));
is_glob_match(String, Glob) ->
    is_regexp_match(String, ejabberd_regexp:sh_to_awk(Glob)).
