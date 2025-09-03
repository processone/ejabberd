%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-include("translate.hrl").

-export([start/2,
         stop/1,
         reload/3,
         mod_options/1,
         get_commands_spec/0,
         depends/2,
         mod_doc/0]).

% Commands API
-export([
         % Adminsys
         compile/1,
         get_cookie/0,
         restart_module/2,

         % Sessions
         num_resources/2,
         resource_num/3,
         kick_session/4,
         status_num/2, status_num/1,
         status_list/2,
         status_list_v3/2,
         status_list/1,
         status_list_v3/1,
         connected_users_info/0,
         connected_users_vhost/1,
         set_presence/7,
         get_presence/2,
         user_sessions_info/2,
         get_last/2,
         set_last/4,

         % Accounts
         set_password/3,
         check_password_hash/4,
         delete_old_users/1,
         delete_old_users_vhost/2,
         check_password/3,
         ban_account/3,
         ban_account_v2/3,
         get_ban_details/2,
         unban_account/2,

         % vCard
         set_nickname/3,
         get_vcard/3, get_vcard/4,
         get_vcard_multi/4,
         set_vcard/4, set_vcard/5,

         % Roster
         add_rosteritem/7,
         delete_rosteritem/4,
         get_roster/2,
         get_roster_count/2,
         push_roster/3,
         push_roster_all/1,
         push_alltoall/2,
         push_roster_item/5,
         build_roster_item/3,

         % Private storage
         private_get/4,
         private_set/3,

         % Shared roster
         srg_create/5,
         srg_add/2,
         srg_delete/2,
         srg_list/1,
         srg_get_info/2,
         srg_set_info/4,
         srg_get_displayed/2,
         srg_add_displayed/3,
         srg_del_displayed/3,
         srg_get_members/2,
         srg_user_add/4,
         srg_user_del/4,

         % Send message
         send_message/5,
         send_stanza/3,
         send_stanza_c2s/4,

         % Privacy list
         privacy_set/3,

         % Stats
         stats/1, stats/2]).
-export([web_menu_main/2,
         web_page_main/2,
         web_menu_host/3,
         web_page_host/3,
         web_menu_hostuser/4,
         web_page_hostuser/4,
         web_menu_hostnode/4,
         web_page_hostnode/4,
         web_menu_node/3,
         web_page_node/3]).

-import(ejabberd_web_admin, [make_command/4, make_table/2]).

-include("ejabberd_commands.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("mod_roster.hrl").
-include("mod_privacy.hrl").
-include("ejabberd_sm.hrl").

-include_lib("xmpp/include/scram.hrl").
-include_lib("xmpp/include/xmpp.hrl").

%%%
%%% gen_mod
%%%


start(_Host, _Opts) ->
    {ok, [{commands, get_commands_spec()},
          {hook, webadmin_menu_main, web_menu_main, 50, global},
          {hook, webadmin_page_main, web_page_main, 50, global},
          {hook, webadmin_menu_host, web_menu_host, 50},
          {hook, webadmin_page_host, web_page_host, 50},
          {hook, webadmin_menu_hostuser, web_menu_hostuser, 50},
          {hook, webadmin_page_hostuser, web_page_hostuser, 50},
          {hook, webadmin_menu_hostnode, web_menu_hostnode, 50},
          {hook, webadmin_page_hostnode, web_page_hostnode, 50},
          {hook, webadmin_menu_node, web_menu_node, 50, global},
          {hook, webadmin_page_node, web_page_node, 50, global}]}.


stop(_Host) ->
    ok.


reload(_Host, _NewOpts, _OldOpts) ->
    ok.


depends(_Host, _Opts) ->
    [].


%%%
%%% Register commands
%%%


get_commands_spec() ->
    Vcard1FieldsString = "Some vcard field names in `get`/`set_vcard` are:\n\n"
                         "* FN           - Full Name\n"
                         "* NICKNAME     - Nickname\n"
                         "* BDAY         - Birthday\n"
                         "* TITLE        - Work: Position\n"
                         "* ROLE         - Work: Role\n",

    Vcard2FieldsString = "Some vcard field names and subnames in `get`/`set_vcard2` are:\n\n"
                         "* N FAMILY     - Family name\n"
                         "* N GIVEN      - Given name\n"
                         "* N MIDDLE     - Middle name\n"
                         "* ADR CTRY     - Address: Country\n"
                         "* ADR LOCALITY - Address: City\n"
                         "* TEL HOME     - Telephone: Home\n"
                         "* TEL CELL     - Telephone: Cellphone\n"
                         "* TEL WORK     - Telephone: Work\n"
                         "* TEL VOICE    - Telephone: Voice\n"
                         "* EMAIL USERID - E-Mail Address\n"
                         "* ORG ORGNAME  - Work: Company\n"
                         "* ORG ORGUNIT  - Work: Department\n",

    VcardXEP = "For a full list of vCard fields check [XEP-0054: vcard-temp]"
               "(https://xmpp.org/extensions/xep-0054.html)",

    [#ejabberd_commands{
       name = compile,
       tags = [erlang],
       desc = "Recompile and reload Erlang source code file",
       module = ?MODULE,
       function = compile,
       args = [{file, string}],
       args_example = ["/home/me/srcs/ejabberd/mod_example.erl"],
       args_desc = ["Filename of erlang source file to compile"],
       result = {res, rescode},
       result_example = ok
      },
     #ejabberd_commands{
       name = get_cookie,
       tags = [erlang],
       desc = "Get the Erlang cookie of this node",
       module = ?MODULE,
       function = get_cookie,
       args = [],
       result = {cookie, string},
       result_example = "MWTAVMODFELNLSMYXPPD",
       result_desc = "Erlang cookie used for authentication by ejabberd"
      },
     #ejabberd_commands{
       name = restart_module,
       tags = [erlang],
       desc = "Stop an ejabberd module, reload code and start",
       module = ?MODULE,
       function = restart_module,
       args = [{host, binary}, {module, binary}],
       args_example = ["myserver.com", "mod_admin_extra"],
       args_desc = ["Server name", "Module to restart"],
       result = {res, integer},
       result_example = 0,
       result_desc = "Returns integer code:\n"
                     " - `0`: code reloaded, module restarted\n"
                     " - `1`: error: module not loaded\n"
                     " - `2`: code not reloaded, but module restarted"
      },
     #ejabberd_commands{
       name = delete_old_users,
       tags = [accounts, purge],
       desc = "Delete users that didn't log in last days, or that never logged",
       longdesc = "To protect admin accounts, configure this for example:\n"
                  "``` yaml\n"
                  "access_rules:\n"
                  "  protect_old_users:\n"
                  "    - allow: admin\n"
                  "    - deny: all\n"
                  "```\n",
       module = ?MODULE,
       function = delete_old_users,
       args = [{days, integer}],
       args_example = [30],
       args_desc = ["Last login age in days of accounts that should be removed"],
       result = {res, restuple},
       result_example = {ok, <<"Deleted 2 users: [\"oldman@myserver.com\", \"test@myserver.com\"]">>},
       result_desc = "Result tuple"
      },
     #ejabberd_commands{
       name = delete_old_users_vhost,
       tags = [accounts, purge],
       desc = "Delete users that didn't log in last days in vhost, or that never logged",
       longdesc = "To protect admin accounts, configure this for example:\n"
                  "``` yaml\n"
                  "access_rules:\n"
                  "  delete_old_users:\n"
                  "    - deny: admin\n"
                  "    - allow: all\n"
                  "```\n",
       module = ?MODULE,
       function = delete_old_users_vhost,
       args = [{host, binary}, {days, integer}],
       args_example = [<<"myserver.com">>, 30],
       args_desc = ["Server name",
                    "Last login age in days of accounts that should be removed"],
       result = {res, restuple},
       result_example = {ok, <<"Deleted 2 users: [\"oldman@myserver.com\", \"test@myserver.com\"]">>},
       result_desc = "Result tuple"
      },
     #ejabberd_commands{
       name = check_account,
       tags = [accounts],
       desc = "Check if an account exists or not",
       module = ejabberd_auth,
       function = user_exists,
       args = [{user, binary}, {host, binary}],
       args_example = [<<"peter">>, <<"myserver.com">>],
       args_desc = ["User name to check", "Server to check"],
       result = {res, rescode},
       result_example = ok
      },
     #ejabberd_commands{
       name = check_password,
       tags = [accounts],
       desc = "Check if a password is correct",
       module = ?MODULE,
       function = check_password,
       args = [{user, binary}, {host, binary}, {password, binary}],
       args_example = [<<"peter">>, <<"myserver.com">>, <<"secret">>],
       args_desc = ["User name to check", "Server to check", "Password to check"],
       result = {res, rescode},
       result_example = ok
      },
     #ejabberd_commands{
       name = check_password_hash,
       tags = [accounts],
       desc = "Check if the password hash is correct",
       longdesc = "Allows hash methods from the Erlang/OTP "
                  "[crypto](https://www.erlang.org/doc/apps/crypto/crypto.html) application.",
       module = ?MODULE,
       function = check_password_hash,
       args = [{user, binary},
               {host, binary},
               {passwordhash, binary},
               {hashmethod, binary}],
       args_example = [<<"peter">>,
                       <<"myserver.com">>,
                       <<"5ebe2294ecd0e0f08eab7690d2a6ee69">>,
                       <<"md5">>],
       args_desc = ["User name to check", "Server to check",
                    "Password's hash value", "Name of hash method"],
       result = {res, rescode},
       result_example = ok
      },
     #ejabberd_commands{
       name = change_password,
       tags = [accounts],
       desc = "Change the password of an account",
       module = ?MODULE,
       function = set_password,
       args = [{user, binary}, {host, binary}, {newpass, binary}],
       args_example = [<<"peter">>, <<"myserver.com">>, <<"blank">>],
       args_desc = ["User name", "Server name",
                    "New password for user"],
       result = {res, rescode},
       result_example = ok
      },

     #ejabberd_commands{
       name = ban_account,
       tags = [accounts],
       desc = "Ban an account: kick sessions and set random password",
       longdesc = "This simply sets a random password.",
       module = ?MODULE,
       function = ban_account,
       args = [{user, binary}, {host, binary}, {reason, binary}],
       args_example = [<<"attacker">>, <<"myserver.com">>, <<"Spaming other users">>],
       args_desc = ["User name to ban", "Server name",
                    "Reason for banning user"],
       result = {res, rescode},
       result_example = ok
      },
     #ejabberd_commands{
       name = ban_account,
       tags = [accounts],
       desc = "Ban an account",
       longdesc = "This command kicks the account sessions, "
                  "stores ban details in the account private storage, "
                  "which blocks login to the account. "
                  "This command requires _`mod_private`_ to be enabled. "
                  "Check also _`get_ban_details`_ API "
                  "and _`unban_account`_ API.",
       module = ?MODULE,
       function = ban_account_v2,
       version = 2,
       note = "improved in 25.08",
       args = [{user, binary}, {host, binary}, {reason, binary}],
       args_example = [<<"attacker">>, <<"myserver.com">>, <<"Spaming other users">>],
       args_desc = ["User name to ban", "Server name",
                    "Reason for banning user"],
       result = {res, rescode},
       result_example = ok
      },
     #ejabberd_commands{
       name = get_ban_details,
       tags = [accounts],
       desc = "Get ban details about an account",
       longdesc = "Check _`ban_account`_ API.",
       module = ?MODULE,
       function = get_ban_details,
       version = 2,
       note = "added in 24.06",
       args = [{user, binary}, {host, binary}],
       args_example = [<<"attacker">>, <<"myserver.com">>],
       args_desc = ["User name to unban", "Server name"],
       result = {ban_details, {list,
                               {detail, {tuple, [{name, string},
                                                 {value, string}]}}}},
       result_example = [{"reason", "Spamming other users"},
                         {"bandate", "2024-04-22T09:16:47.975312Z"},
                         {"lastdate", "2024-04-22T08:39:12Z"},
                         {"lastreason", "Connection reset by peer"}]
      },
     #ejabberd_commands{
       name = unban_account,
       tags = [accounts],
       desc = "Remove the ban from an account",
       longdesc = "Check _`ban_account`_ API.",
       module = ?MODULE,
       function = unban_account,
       version = 2,
       note = "added in 24.06",
       args = [{user, binary}, {host, binary}],
       args_example = [<<"gooduser">>, <<"myserver.com">>],
       args_desc = ["User name to unban", "Server name"],
       result = {res, rescode},
       result_example = ok
      },

     #ejabberd_commands{
       name = num_resources,
       tags = [session],
       desc = "Get the number of resources of a user",
       module = ?MODULE,
       function = num_resources,
       args = [{user, binary}, {host, binary}],
       args_example = [<<"peter">>, <<"myserver.com">>],
       args_desc = ["User name", "Server name"],
       result = {resources, integer},
       result_example = 5,
       result_desc = "Number of active resources for a user"
      },
     #ejabberd_commands{
       name = resource_num,
       tags = [session],
       desc = "Resource string of a session number",
       module = ?MODULE,
       function = resource_num,
       args = [{user, binary}, {host, binary}, {num, integer}],
       args_example = [<<"peter">>, <<"myserver.com">>, 2],
       args_desc = ["User name", "Server name", "ID of resource to return"],
       result = {resource, string},
       result_example = <<"Psi">>,
       result_desc = "Name of user resource"
      },
     #ejabberd_commands{
       name = kick_session,
       tags = [session],
       desc = "Kick a user session",
       module = ?MODULE,
       function = kick_session,
       args = [{user, binary}, {host, binary}, {resource, binary}, {reason, binary}],
       args_example = [<<"peter">>,
                       <<"myserver.com">>,
                       <<"Psi">>,
                       <<"Stuck connection">>],
       args_desc = ["User name", "Server name", "User's resource",
                    "Reason for closing session"],
       result = {res, rescode},
       result_example = ok
      },
     #ejabberd_commands{
       name = status_num_host,
       tags = [session, statistics],
       desc = "Number of logged users with this status in host",
       policy = admin,
       module = ?MODULE,
       function = status_num,
       args = [{host, binary}, {status, binary}],
       args_example = [<<"myserver.com">>, <<"dnd">>],
       args_desc = ["Server name", "Status type to check"],
       result = {users, integer},
       result_example = 23,
       result_desc = "Number of connected sessions with given status type"
      },
     #ejabberd_commands{
       name = status_num,
       tags = [session, statistics],
       desc = "Number of logged users with this status",
       policy = admin,
       module = ?MODULE,
       function = status_num,
       args = [{status, binary}],
       args_example = [<<"dnd">>],
       args_desc = ["Status type to check"],
       result = {users, integer},
       result_example = 23,
       result_desc = "Number of connected sessions with given status type"
      },
     #ejabberd_commands{
       name = status_list_host,
       tags = [session],
       desc = "List of users logged in host with their statuses",
       module = ?MODULE,
       function = status_list,
       args = [{host, binary}, {status, binary}],
       args_example = [<<"myserver.com">>, <<"dnd">>],
       args_desc = ["Server name", "Status type to check"],
       result_example = [{<<"peter">>, <<"myserver.com">>, <<"tka">>, 6, <<"Busy">>}],
       result = {users, {list,
                         {userstatus, {tuple, [{user, string},
                                               {host, string},
                                               {resource, string},
                                               {priority, integer},
                                               {status, string}]}}}}
      },
     #ejabberd_commands{
       name = status_list_host,
       tags = [session],
       desc = "List of users logged in host with their statuses",
       module = ?MODULE,
       function = status_list_v3,
       version = 3,
       note = "updated in 24.12",
       args = [{host, binary}, {status, binary}],
       args_example = [<<"myserver.com">>, <<"dnd">>],
       args_desc = ["Server name", "Status type to check"],
       result_example = [{<<"peter@myserver.com/tka">>, 6, <<"Busy">>}],
       result = {users, {list,
                         {userstatus, {tuple, [{jid, string},
                                               {priority, integer},
                                               {status, string}]}}}}
      },
     #ejabberd_commands{
       name = status_list,
       tags = [session],
       desc = "List of logged users with this status",
       module = ?MODULE,
       function = status_list,
       args = [{status, binary}],
       args_example = [<<"dnd">>],
       args_desc = ["Status type to check"],
       result_example = [{<<"peter">>, <<"myserver.com">>, <<"tka">>, 6, <<"Busy">>}],
       result = {users, {list,
                         {userstatus, {tuple, [{user, string},
                                               {host, string},
                                               {resource, string},
                                               {priority, integer},
                                               {status, string}]}}}}
      },
     #ejabberd_commands{
       name = status_list,
       tags = [session],
       desc = "List of logged users with this status",
       module = ?MODULE,
       function = status_list_v3,
       version = 3,
       note = "updated in 24.12",
       args = [{status, binary}],
       args_example = [<<"dnd">>],
       args_desc = ["Status type to check"],
       result_example = [{<<"peter@myserver.com/tka">>, 6, <<"Busy">>}],
       result = {users, {list,
                         {userstatus, {tuple, [{jid, string},
                                               {priority, integer},
                                               {status, string}]}}}}
      },
     #ejabberd_commands{
       name = connected_users_info,
       tags = [session],
       desc = "List all established sessions and their information",
       module = ?MODULE,
       function = connected_users_info,
       args = [],
       result_example = [{"user1@myserver.com/tka",
                          "c2s",
                          "127.0.0.1",
                          42656,
                          8,
                          "ejabberd@localhost",
                          231,
                          <<"dnd">>,
                          <<"tka">>,
                          <<>>}],
       result = {connected_users_info,
                 {list,
                  {session, {tuple,
                             [{jid, string},
                              {connection, string},
                              {ip, string},
                              {port, integer},
                              {priority, integer},
                              {node, string},
                              {uptime, integer},
                              {status, string},
                              {resource, string},
                              {statustext, string}]}}}}
      },

     #ejabberd_commands{
       name = connected_users_vhost,
       tags = [session],
       desc = "Get the list of established sessions in a vhost",
       module = ?MODULE,
       function = connected_users_vhost,
       args_example = [<<"myexample.com">>],
       args_desc = ["Server name"],
       args = [{host, binary}],
       result_example = [<<"user1@myserver.com/tka">>, <<"user2@localhost/tka">>],
       result_desc = "List of sessions full JIDs",
       result = {connected_users_vhost, {list, {sessions, string}}}
      },
     #ejabberd_commands{
       name = user_sessions_info,
       tags = [session],
       desc = "Get information about all sessions of a user",
       module = ?MODULE,
       function = user_sessions_info,
       args = [{user, binary}, {host, binary}],
       args_example = [<<"peter">>, <<"myserver.com">>],
       args_desc = ["User name", "Server name"],
       result_example = [{"c2s",
                          "127.0.0.1",
                          42656,
                          8,
                          "ejabberd@localhost",
                          231,
                          <<"dnd">>,
                          <<"tka">>,
                          <<>>}],
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
                              {statustext, string}]}}}}
      },

     #ejabberd_commands{
       name = get_presence,
       tags = [session],
       desc =
           "Retrieve the resource with highest priority, "
           "and its presence (show and status message) "
           "for a given user.",
       longdesc =
           "The `jid` value contains the user JID "
           "with resource.\n\nThe `show` value contains "
           "the user presence flag. It can take "
           "limited values:\n\n - `available`\n - `chat` "
           "(Free for chat)\n - `away`\n - `dnd` (Do "
           "not disturb)\n - `xa` (Not available, "
           "extended away)\n - `unavailable` (Not "
           "connected)\n\n`status` is a free text "
           "defined by the user client.",
       module = ?MODULE,
       function = get_presence,
       args = [{user, binary}, {host, binary}],
       args_rename = [{server, host}],
       args_example = [<<"peter">>, <<"myexample.com">>],
       args_desc = ["User name", "Server name"],
       result_example = {<<"user1@myserver.com/tka">>, <<"dnd">>, <<"Busy">>},
       result =
           {presence,
            {tuple,
             [{jid, string},
              {show, string},
              {status, string}]}}
      },
     #ejabberd_commands{
       name = set_presence,
       tags = [session],
       desc = "Set presence of a session",
       module = ?MODULE,
       function = set_presence,
       args = [{user, binary},
               {host, binary},
               {resource, binary},
               {type, binary},
               {show, binary},
               {status, binary},
               {priority, binary}],
       args_example = [<<"user1">>,
                       <<"myserver.com">>,
                       <<"tka1">>,
                       <<"available">>,
                       <<"away">>,
                       <<"BB">>,
                       <<"7">>],
       args_desc = ["User name", "Server name", "Resource",
                    "Type: `available`, `error`, `probe`...",
                    "Show: `away`, `chat`, `dnd`, `xa`.", "Status text",
                    "Priority, provide this value as an integer"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = set_presence,
       tags = [session],
       desc = "Set presence of a session",
       module = ?MODULE,
       function = set_presence,
       version = 1,
       note = "updated in 24.02",
       args = [{user, binary},
               {host, binary},
               {resource, binary},
               {type, binary},
               {show, binary},
               {status, binary},
               {priority, integer}],
       args_example = [<<"user1">>,
                       <<"myserver.com">>,
                       <<"tka1">>,
                       <<"available">>,
                       <<"away">>,
                       <<"BB">>,
                       7],
       args_desc = ["User name", "Server name", "Resource",
                    "Type: `available`, `error`, `probe`...",
                    "Show: `away`, `chat`, `dnd`, `xa`.", "Status text",
                    "Priority, provide this value as an integer"],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = set_nickname,
       tags = [vcard],
       desc = "Set nickname in a user's vCard",
       module = ?MODULE,
       function = set_nickname,
       args = [{user, binary}, {host, binary}, {nickname, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"User 1">>],
       args_desc = ["User name", "Server name", "Nickname"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = get_vcard,
       tags = [vcard],
       desc = "Get content from a vCard field",
       longdesc = Vcard1FieldsString ++ "\n" ++ VcardXEP,
       module = ?MODULE,
       function = get_vcard,
       args = [{user, binary}, {host, binary}, {name, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"NICKNAME">>],
       args_desc = ["User name", "Server name", "Field name"],
       result_example = "User 1",
       result_desc = "Field content",
       result = {content, string}
      },
     #ejabberd_commands{
       name = get_vcard2,
       tags = [vcard],
       desc = "Get content from a vCard subfield",
       longdesc = Vcard2FieldsString ++ "\n" ++ VcardXEP,
       module = ?MODULE,
       function = get_vcard,
       args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"N">>, <<"FAMILY">>],
       args_desc = ["User name", "Server name", "Field name", "Subfield name"],
       result_example = "Schubert",
       result_desc = "Field content",
       result = {content, string}
      },
     #ejabberd_commands{
       name = get_vcard2_multi,
       tags = [vcard],
       desc = "Get multiple contents from a vCard field",
       longdesc = Vcard2FieldsString ++ "\n" ++ VcardXEP,
       module = ?MODULE,
       function = get_vcard_multi,
       args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}],
       result = {contents, {list, {value, string}}}
      },

     #ejabberd_commands{
       name = set_vcard,
       tags = [vcard],
       desc = "Set content in a vCard field",
       longdesc = Vcard1FieldsString ++ "\n" ++ VcardXEP,
       module = ?MODULE,
       function = set_vcard,
       args = [{user, binary}, {host, binary}, {name, binary}, {content, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"URL">>, <<"www.example.com">>],
       args_desc = ["User name", "Server name", "Field name", "Value"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = set_vcard2,
       tags = [vcard],
       desc = "Set content in a vCard subfield",
       longdesc = Vcard2FieldsString ++ "\n" ++ VcardXEP,
       module = ?MODULE,
       function = set_vcard,
       args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}, {content, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"TEL">>, <<"NUMBER">>, <<"123456">>],
       args_desc = ["User name", "Server name", "Field name", "Subfield name", "Value"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = set_vcard2_multi,
       tags = [vcard],
       desc = "Set multiple contents in a vCard subfield",
       longdesc = Vcard2FieldsString ++ "\n" ++ VcardXEP,
       module = ?MODULE,
       function = set_vcard,
       args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}, {contents, {list, {value, binary}}}],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = add_rosteritem,
       tags = [roster],
       desc = "Add an item to a user's roster (supports ODBC)",
       longdesc = "Group can be several groups separated by `;` for example: `g1;g2;g3`",
       module = ?MODULE,
       function = add_rosteritem,
       args = [{localuser, binary},
               {localhost, binary},
               {user, binary},
               {host, binary},
               {nick, binary},
               {group, binary},
               {subs, binary}],
       args_rename = [{localserver, localhost}, {server, host}],
       args_example = [<<"user1">>,
                       <<"myserver.com">>,
                       <<"user2">>,
                       <<"myserver.com">>,
                       <<"User 2">>,
                       <<"Friends">>,
                       <<"both">>],
       args_desc = ["User name", "Server name", "Contact user name", "Contact server name",
                    "Nickname", "Group", "Subscription"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = add_rosteritem,
       tags = [roster],
       desc = "Add an item to a user's roster (supports ODBC)",
       module = ?MODULE,
       function = add_rosteritem,
       version = 1,
       note = "updated in 24.02",
       args = [{localuser, binary},
               {localhost, binary},
               {user, binary},
               {host, binary},
               {nick, binary},
               {groups, {list, {group, binary}}},
               {subs, binary}],
       args_rename = [{localserver, localhost}, {server, host}],
       args_example = [<<"user1">>,
                       <<"myserver.com">>,
                       <<"user2">>,
                       <<"myserver.com">>,
                       <<"User 2">>,
                       [<<"Friends">>, <<"Team 1">>],
                       <<"both">>],
       args_desc = ["User name", "Server name", "Contact user name", "Contact server name",
                    "Nickname", "Groups", "Subscription"],
       result = {res, rescode}
      },
     %%{"", "subs= none, from, to or both"},
     %%{"", "example: add-roster peter localhost mike server.com MiKe Employees both"},
     %%{"", "will add mike@server.com to peter@localhost roster"},
     #ejabberd_commands{
       name = delete_rosteritem,
       tags = [roster],
       desc = "Delete an item from a user's roster (supports ODBC)",
       module = ?MODULE,
       function = delete_rosteritem,
       args = [{localuser, binary},
               {localhost, binary},
               {user, binary},
               {host, binary}],
       args_rename = [{localserver, localhost}, {server, host}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"user2">>, <<"myserver.com">>],
       args_desc = ["User name", "Server name", "Contact user name", "Contact server name"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = process_rosteritems,
       tags = [roster],
       desc = "List/delete rosteritems that match filter",
       longdesc = "Explanation of each argument:\n\n"
                  "* `action`: what to do with each rosteritem that "
                  "matches all the filtering options\n"
                  "* `subs`: subscription type\n"
                  "* `asks`: pending subscription\n"
                  "* `users`: the JIDs of the local user\n"
                  "* `contacts`: the JIDs of the contact in the roster\n"
                  "\n"
                  "**Mnesia backend:**\n"
                  "\n"
                  "Allowed values in the arguments:\n\n"
                  "* `action` = `list` | `delete`\n"
                  "* `subs` = `any` | SUB[:SUB]*\n"
                  "* `asks` = `any` | ASK[:ASK]*\n"
                  "* `users` = `any` | JID[:JID]*\n"
                  "* `contacts` = `any` | JID[:JID]*\n"
                  "\nwhere\n\n"
                  "* SUB = `none` | `from `| `to` | `both`\n"
                  "* ASK = `none` | `out` | `in`\n"
                  "* JID = characters valid in a JID, and can use the "
                  "globs: `*`, `?`, `!` and `[...]`\n"
                  "\n"
                  "This example will list roster items with subscription "
                  "`none`, `from` or `to` that have any ask property, of "
                  "local users which JID is in the virtual host "
                  "`example.org` and that the contact JID is either a "
                  "bare server name (without user part) or that has a "
                  "user part and the server part contains the word `icq`"
                  ":\n  `list none:from:to any *@example.org *:*@*icq*`"
                  "\n\n"
                  "**SQL backend:**\n"
                  "\n"
                  "Allowed values in the arguments:\n\n"
                  "* `action` = `list` | `delete`\n"
                  "* `subs` = `any` | SUB\n"
                  "* `asks` = `any` | ASK\n"
                  "* `users` = JID\n"
                  "* `contacts` = JID\n"
                  "\nwhere\n\n"
                  "* SUB = `none` | `from` | `to` | `both`\n"
                  "* ASK = `none` | `out` | `in`\n"
                  "* JID = characters valid in a JID, and can use the "
                  "globs: `_` and `%`\n"
                  "\n"
                  "This example will list roster items with subscription "
                  "`to` that have any ask property, of "
                  "local users which JID is in the virtual host "
                  "`example.org` and that the contact JID's "
                  "server part contains the word `icq`"
                  ":\n  `list to any %@example.org %@%icq%`",
       module = mod_roster,
       function = process_rosteritems,
       args = [{action, string},
               {subs, string},
               {asks, string},
               {users, string},
               {contacts, string}],
       result = {response,
                 {list,
                  {pairs, {tuple,
                           [{user, string},
                            {contact, string}]}}}}
      },
     #ejabberd_commands{
       name = get_roster,
       tags = [roster],
       desc = "Get list of contacts in a local user roster",
       longdesc =
           "`subscription` can be: `none`, `from`, `to`, `both`.\n\n"
           "`pending` can be: `in`, `out`, `none`.",
       note = "improved in 23.10",
       policy = user,
       module = ?MODULE,
       function = get_roster,
       args = [],
       args_rename = [{server, host}],
       result_example = [{<<"user2@localhost">>, <<"User 2">>, <<"none">>, <<"subscribe">>, [<<"Group1">>]}],
       result = {contacts, {list, {contact, {tuple, [{jid, string},
                                                     {nick, string},
                                                     {subscription, string},
                                                     {pending, string},
                                                     {groups, {list, {group, string}}}]}}}}
      },
     #ejabberd_commands{
       name = get_roster_count,
       tags = [roster],
       desc = "Get number of contacts in a local user roster",
       note = "added in 24.06",
       policy = user,
       module = ?MODULE,
       function = get_roster_count,
       args = [],
       args_rename = [{server, host}],
       result_example = 5,
       result_desc = "Number",
       result = {value, integer}
      },
     #ejabberd_commands{
       name = push_roster,
       tags = [roster],
       desc = "Push template roster from file to a user",
       longdesc = "The text file must contain an erlang term: a list "
                  "of tuples with username, servername, group and nick. For example:\n"
                  "`[{\"user1\", \"localhost\", \"Workers\", \"User 1\"},\n"
                  " {\"user2\", \"localhost\", \"Workers\", \"User 2\"}].`\n\n"
                  "If there are problems parsing UTF8 character encoding, "
                  "provide the corresponding string with the `<<\"STRING\"/utf8>>` syntax, for example:\n"
                  "`[{\"user2\", \"localhost\", \"Workers\", <<\"User 2\"/utf8>>}]`.",
       module = ?MODULE,
       function = push_roster,
       args = [{file, binary}, {user, binary}, {host, binary}],
       args_example = [<<"/home/ejabberd/roster.txt">>, <<"user1">>, <<"localhost">>],
       args_desc = ["File path", "User name", "Server name"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = push_roster_all,
       tags = [roster],
       desc = "Push template roster from file to all those users",
       longdesc = "The text file must contain an erlang term: a list "
                  "of tuples with username, servername, group and nick. Example:\n"
                  "`[{\"user1\", \"localhost\", \"Workers\", \"User 1\"},\n"
                  " {\"user2\", \"localhost\", \"Workers\", \"User 2\"}].`",
       module = ?MODULE,
       function = push_roster_all,
       args = [{file, binary}],
       args_example = [<<"/home/ejabberd/roster.txt">>],
       args_desc = ["File path"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = push_alltoall,
       tags = [roster],
       desc = "Add all the users to all the users of Host in Group",
       module = ?MODULE,
       function = push_alltoall,
       args = [{host, binary}, {group, binary}],
       args_example = [<<"myserver.com">>, <<"Everybody">>],
       args_desc = ["Server name", "Group name"],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = get_last,
       tags = [last],
       desc = "Get last activity information",
       longdesc = "Timestamp is UTC and "
                  "[XEP-0082](https://xmpp.org/extensions/xep-0082.html)"
                  " format, for example: "
                  "`2017-02-23T22:25:28.063062Z     ONLINE`",
       module = ?MODULE,
       function = get_last,
       args = [{user, binary}, {host, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>],
       args_desc = ["User name", "Server name"],
       result_example = {<<"2017-06-30T14:32:16.060684Z">>, "ONLINE"},
       result_desc = "Last activity timestamp and status",
       result = {last_activity,
                 {tuple, [{timestamp, string},
                          {status, string}]}}
      },
     #ejabberd_commands{
       name = set_last,
       tags = [last],
       desc = "Set last activity information",
       longdesc = "Timestamp is the seconds since "
                  "`1970-01-01 00:00:00 UTC`. For example value see `date +%s`",
       module = ?MODULE,
       function = set_last,
       args = [{user, binary}, {host, binary}, {timestamp, integer}, {status, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, 1500045311, <<"GoSleeping">>],
       args_desc = ["User name", "Server name", "Number of seconds since epoch", "Status message"],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = private_get,
       tags = [private],
       desc = "Get some information from a user private storage",
       module = ?MODULE,
       function = private_get,
       args = [{user, binary}, {host, binary}, {element, binary}, {ns, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"storage">>, <<"storage:rosternotes">>],
       args_desc = ["User name", "Server name", "Element name", "Namespace"],
       result = {res, string}
      },
     #ejabberd_commands{
       name = private_set,
       tags = [private],
       desc = "Set to the user private storage",
       module = ?MODULE,
       function = private_set,
       args = [{user, binary}, {host, binary}, {element, binary}],
       args_example = [<<"user1">>,
                       <<"myserver.com">>,
                       <<"<storage xmlns='storage:rosternotes'/>">>],
       args_desc = ["User name", "Server name", "XML storage element"],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = srg_create,
       tags = [shared_roster_group],
       desc = "Create a Shared Roster Group",
       longdesc = "If you want to specify several group "
                  "identifiers in the Display argument,\n"
                  "put `\\ \"` around the argument and\nseparate the "
                  "identifiers with `\\ \\ n`\n"
                  "For example:\n"
                  "  `ejabberdctl srg_create group3 myserver.com "
                  "name desc \\\"group1\\\\ngroup2\\\"`",
       note = "changed in 21.07",
       module = ?MODULE,
       function = srg_create,
       args = [{group, binary},
               {host, binary},
               {label, binary},
               {description, binary},
               {display, binary}],
       args_rename = [{name, label}],
       args_example = [<<"group3">>,
                       <<"myserver.com">>,
                       <<"Group3">>,
                       <<"Third group">>,
                       <<"group1\\\\ngroup2">>],
       args_desc = ["Group identifier", "Group server name", "Group name",
                    "Group description", "Groups to display"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = srg_create,
       tags = [shared_roster_group],
       desc = "Create a Shared Roster Group",
       module = ?MODULE,
       function = srg_create,
       version = 1,
       note = "updated in 24.02",
       args = [{group, binary},
               {host, binary},
               {label, binary},
               {description, binary},
               {display, {list, {group, binary}}}],
       args_rename = [{name, label}],
       args_example = [<<"group3">>,
                       <<"myserver.com">>,
                       <<"Group3">>,
                       <<"Third group">>,
                       [<<"group1">>, <<"group2">>]],
       args_desc = ["Group identifier", "Group server name", "Group name",
                    "Group description", "List of groups to display"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = srg_add,
       tags = [shared_roster_group],
       desc = "Add/Create a Shared Roster Group (without details)",
       module = ?MODULE,
       function = srg_add,
       note = "added in 24.06",
       args = [{group, binary}, {host, binary}],
       args_example = [<<"group3">>, <<"myserver.com">>],
       args_desc = ["Group identifier", "Group server name"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = srg_delete,
       tags = [shared_roster_group],
       desc = "Delete a Shared Roster Group",
       module = ?MODULE,
       function = srg_delete,
       args = [{group, binary}, {host, binary}],
       args_example = [<<"group3">>, <<"myserver.com">>],
       args_desc = ["Group identifier", "Group server name"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = srg_list,
       tags = [shared_roster_group],
       desc = "List the Shared Roster Groups in Host",
       module = ?MODULE,
       function = srg_list,
       args = [{host, binary}],
       args_example = [<<"myserver.com">>],
       args_desc = ["Server name"],
       result_example = [<<"group1">>, <<"group2">>],
       result_desc = "List of group identifiers",
       result = {groups, {list, {id, string}}}
      },
     #ejabberd_commands{
       name = srg_get_info,
       tags = [shared_roster_group],
       desc = "Get info of a Shared Roster Group",
       module = ?MODULE,
       function = srg_get_info,
       args = [{group, binary}, {host, binary}],
       args_example = [<<"group3">>, <<"myserver.com">>],
       args_desc = ["Group identifier", "Group server name"],
       result_example = [{<<"name">>, "Group 3"}, {<<"displayed_groups">>, "group1"}],
       result_desc = "List of group information, as key and value",
       result = {informations, {list, {information, {tuple, [{key, string}, {value, string}]}}}}
      },
     #ejabberd_commands{
       name = srg_set_info,
       tags = [shared_roster_group],
       desc = "Set info of a Shared Roster Group",
       module = ?MODULE,
       function = srg_set_info,
       note = "added in 24.06",
       args = [{group, binary}, {host, binary}, {key, binary}, {value, binary}],
       args_example = [<<"group3">>, <<"myserver.com">>, <<"label">>, <<"Family">>],
       args_desc = ["Group identifier", "Group server name",
                    "Information key: label, description",
                    "Information value"],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = srg_get_displayed,
       tags = [shared_roster_group],
       desc = "Get displayed groups of a Shared Roster Group",
       module = ?MODULE,
       function = srg_get_displayed,
       note = "added in 24.06",
       args = [{group, binary}, {host, binary}],
       args_example = [<<"group3">>, <<"myserver.com">>],
       args_desc = ["Group identifier", "Group server name"],
       result_example = [<<"group1">>, <<"group2">>],
       result_desc = "List of groups to display",
       result = {display, {list, {group, binary}}}
      },
     #ejabberd_commands{
       name = srg_add_displayed,
       tags = [shared_roster_group],
       desc = "Add a group to displayed_groups of a Shared Roster Group",
       module = ?MODULE,
       function = srg_add_displayed,
       note = "added in 24.06",
       args = [{group, binary},
               {host, binary},
               {add, binary}],
       args_example = [<<"group3">>, <<"myserver.com">>, <<"group1">>],
       args_desc = ["Group identifier", "Group server name",
                    "Group to add to displayed_groups"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = srg_del_displayed,
       tags = [shared_roster_group],
       desc = "Delete a group from displayed_groups of a Shared Roster Group",
       module = ?MODULE,
       function = srg_del_displayed,
       note = "added in 24.06",
       args = [{group, binary},
               {host, binary},
               {del, binary}],
       args_example = [<<"group3">>, <<"myserver.com">>, <<"group1">>],
       args_desc = ["Group identifier", "Group server name",
                    "Group to delete from displayed_groups"],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = srg_get_members,
       tags = [shared_roster_group],
       desc = "Get members of a Shared Roster Group",
       module = ?MODULE,
       function = srg_get_members,
       args = [{group, binary}, {host, binary}],
       args_example = [<<"group3">>, <<"myserver.com">>],
       args_desc = ["Group identifier", "Group server name"],
       result_example = [<<"user1@localhost">>, <<"user2@localhost">>],
       result_desc = "List of group identifiers",
       result = {members, {list, {member, string}}}
      },
     #ejabberd_commands{
       name = srg_user_add,
       tags = [shared_roster_group],
       desc = "Add the JID user@host to the Shared Roster Group",
       module = ?MODULE,
       function = srg_user_add,
       args = [{user, binary}, {host, binary}, {group, binary}, {grouphost, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"group3">>, <<"myserver.com">>],
       args_desc = ["Username", "User server name", "Group identifier", "Group server name"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = srg_user_del,
       tags = [shared_roster_group],
       desc = "Delete this JID user@host from the Shared Roster Group",
       module = ?MODULE,
       function = srg_user_del,
       args = [{user, binary}, {host, binary}, {group, binary}, {grouphost, binary}],
       args_example = [<<"user1">>, <<"myserver.com">>, <<"group3">>, <<"myserver.com">>],
       args_desc = ["Username", "User server name", "Group identifier", "Group server name"],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = get_offline_count,
       tags = [offline],
       desc = "Get the number of unread offline messages",
       policy = user,
       module = mod_offline,
       function = count_offline_messages,
       args = [],
       args_rename = [{server, host}],
       result_example = 5,
       result_desc = "Number",
       result = {value, integer}
      },
     #ejabberd_commands{
       name = get_offline_messages,
       tags = [internal, offline],
       desc = "Get the offline messages",
       policy = user,
       module = mod_offline,
       function = get_offline_messages,
       args = [],
       result = {queue, {list, {messages, {tuple, [{time, string},
                                                   {from, string},
                                                   {to, string},
                                                   {packet, string}]}}}}
      },

     #ejabberd_commands{
       name = send_message,
       tags = [stanza],
       desc = "Send a message to a local or remote bare of full JID",
       longdesc = "When sending a groupchat message to a MUC room, "
                  "`from` must be the full JID of a room occupant, "
                  "or the bare JID of a MUC service admin, "
                  "or the bare JID of a MUC/Sub subscribed user.",
       module = ?MODULE,
       function = send_message,
       args = [{type, binary},
               {from, binary},
               {to, binary},
               {subject, binary},
               {body, binary}],
       args_example = [<<"headline">>,
                       <<"admin@localhost">>,
                       <<"user1@localhost">>,
                       <<"Restart">>,
                       <<"In 5 minutes">>],
       args_desc = ["Message type: `normal`, `chat`, `headline`, `groupchat`", "Sender JID",
                    "Receiver JID", "Subject, or empty string", "Body"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = send_stanza_c2s,
       tags = [stanza],
       desc = "Send a stanza from an existing C2S session",
       longdesc = "`user`@`host`/`resource` must be an existing C2S session."
                  " As an alternative, use _`send_stanza`_ API instead.",
       module = ?MODULE,
       function = send_stanza_c2s,
       args = [{user, binary}, {host, binary}, {resource, binary}, {stanza, binary}],
       args_example = [<<"admin">>,
                       <<"myserver.com">>,
                       <<"bot">>,
                       <<"<message to='user1@localhost'><ext attr='value'/></message>">>],
       args_desc = ["Username", "Server name", "Resource", "Stanza"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = send_stanza,
       tags = [stanza],
       desc = "Send a stanza; provide From JID and valid To JID",
       module = ?MODULE,
       function = send_stanza,
       args = [{from, binary}, {to, binary}, {stanza, binary}],
       args_example = [<<"admin@localhost">>,
                       <<"user1@localhost">>,
                       <<"<message><ext attr='value'/></message>">>],
       args_desc = ["Sender JID", "Destination JID", "Stanza"],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = privacy_set,
       tags = [stanza],
       desc = "Send a IQ set privacy stanza for a local account",
       module = ?MODULE,
       function = privacy_set,
       args = [{user, binary}, {host, binary}, {xmlquery, binary}],
       args_example = [<<"user1">>,
                       <<"myserver.com">>,
                       <<"<query xmlns='jabber:iq:privacy'>...">>],
       args_desc = ["Username", "Server name", "Query XML element"],
       result = {res, rescode}
      },

     #ejabberd_commands{
       name = stats,
       tags = [statistics],
       desc = "Get some statistical value for the whole ejabberd server",
       longdesc = "Allowed statistics `name` are: `registeredusers`, "
                  "`onlineusers`, `onlineusersnode`, `uptimeseconds`, `processes`.",
       policy = admin,
       module = ?MODULE,
       function = stats,
       args = [{name, binary}],
       args_example = [<<"registeredusers">>],
       args_desc = ["Statistic name"],
       result_example = 6,
       result_desc = "Integer statistic value",
       result = {stat, integer}
      },
     #ejabberd_commands{
       name = stats_host,
       tags = [statistics],
       desc = "Get some statistical value for this host",
       longdesc = "Allowed statistics `name` are: `registeredusers`, `onlineusers`.",
       policy = admin,
       module = ?MODULE,
       function = stats,
       args = [{name, binary}, {host, binary}],
       args_example = [<<"registeredusers">>, <<"example.com">>],
       args_desc = ["Statistic name", "Server JID"],
       result_example = 6,
       result_desc = "Integer statistic value",
       result = {stat, integer}
      }].


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
    case gen_mod:is_loaded(Host, Module) of
        false ->
            % not a running module, force code reload anyway
            code:purge(Module),
            code:delete(Module),
            code:load_file(Module),
            1;
        true ->
            gen_mod:stop_module(Host, Module),
            case code:soft_purge(Module) of
                true ->
                    code:delete(Module),
                    code:load_file(Module),
                    gen_mod:start_module(Host, Module),
                    0;
                false ->
                    gen_mod:start_module(Host, Module),
                    2
            end
    end.


%%%
%%% Accounts
%%%


set_password(User, Host, Password) ->
    Fun = fun() -> ejabberd_auth:set_password(User, Host, Password) end,
    user_action(User, Host, Fun, ok).


check_password(User, Host, Password) ->
    ejabberd_auth:check_password(User, <<>>, Host, Password).


%% Copied some code from ejabberd_commands.erln
check_password_hash(User, Host, PasswordHash, HashMethod) ->
    AccountPass = ejabberd_auth:get_password_s(User, Host),
    Methods = lists:map(fun(A) -> atom_to_binary(A, latin1) end,
                        proplists:get_value(hashs, crypto:supports())),
    MethodAllowed = lists:member(HashMethod, Methods),
    AccountPassHash = case {AccountPass, MethodAllowed} of
                          {A, _} when is_tuple(A) -> scrammed;
                          {_, true} -> get_hash(AccountPass, HashMethod);
                          {_, false} ->
                              ?ERROR_MSG("Check_password_hash called "
                                         "with hash method: ~p",
                                         [HashMethod]),
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


get_hash(AccountPass, Method) ->
    iolist_to_binary([ io_lib:format("~2.16.0B", [X])
                       || X <- binary_to_list(
                                 crypto:hash(binary_to_atom(Method, latin1), AccountPass)) ]).


delete_old_users(Days) ->
    %% Get the list of registered users
    Users = ejabberd_auth:get_users(),

    {removed, N, UR} = delete_old_users(Days, Users),
    {ok, io_lib:format("Deleted ~p users: ~p", [N, UR])}.


delete_old_users_vhost(Host, Days) ->
    %% Get the list of registered users
    Users = ejabberd_auth:get_users(Host),

    {removed, N, UR} = delete_old_users(Days, Users),
    {ok, io_lib:format("Deleted ~p users: ~p", [N, UR])}.


delete_old_users(Days, Users) ->
    SecOlder = Days * 24 * 60 * 60,
    TimeStamp_now = erlang:system_time(second),
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
    deny = acl:match_rule(LServer, protect_old_users, jid:make(LUser, LServer)),
    [] = ejabberd_sm:get_user_resources(LUser, LServer),
    case mod_last:get_last_info(LUser, LServer) of
        {ok, TimeStamp, _Status} ->
            if
                TimeStamp_oldest < TimeStamp ->
                    false;
                true ->
                    true
            end;
        not_found ->
            true
    end.


%%
%% Ban account v0


ban_account(User, Host, ReasonText) ->
    Reason = prepare_reason(ReasonText),
    kick_sessions(User, Host, Reason),
    set_random_password(User, Host, Reason),
    ok.


kick_sessions(User, Server, Reason) ->
    ejabberd_hooks:run(sm_kick_user, Server, [User, Server]),
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
    RandomString = p1_rand:get_string(),
    <<"BANNED_ACCOUNT--", Date/binary, "--", RandomString/binary, "--", Reason/binary>>.


set_password_auth(User, Server, Password) ->
    ok = ejabberd_auth:set_password(User, Server, Password).


prepare_reason([]) ->
    <<"Kicked by administrator">>;
prepare_reason([Reason]) ->
    Reason;
prepare_reason(Reason) when is_binary(Reason) ->
    Reason.


%%
%% Ban account v2


ban_account_v2(User, Host, ReasonText) ->
    IsPrivateEnabled = gen_mod:is_loaded(Host, mod_private),
    Exists = ejabberd_auth:user_exists(User, Host),
    IsBanned = is_banned(User, Host),
    case {IsPrivateEnabled, Exists, IsBanned} of
        {true, true, false} ->
            ban_account_v2_b(User, Host, ReasonText);
        {false, _, _} ->
            mod_private_is_required_but_disabled;
        {_, false, _} ->
            account_does_not_exist;
        {_, _, true} ->
            account_was_already_banned;
        {_, _, _} ->
            other_error
    end.


ban_account_v2_b(User, Host, ReasonText) ->
    Reason = prepare_reason(ReasonText),
    Last = get_last(User, Host),
    BanDate = xmpp_util:encode_timestamp(erlang:timestamp()),
    Hash = get_hash_value(User, Host),
    BanPrivateXml = build_ban_xmlel(Reason, Last, BanDate, Hash),
    ok = private_set2(User, Host, BanPrivateXml),
    kick_sessions(User, Host, Reason),
    ok.


get_hash_value(User, Host) ->
    Cookie = misc:atom_to_binary(erlang:get_cookie()),
    misc:term_to_base64(crypto:hash(sha256, <<User/binary, Host/binary, Cookie/binary>>)).


build_ban_xmlel(Reason, {LastDate, LastReason}, BanDate, Hash) ->
    #xmlel{
      name = <<"banned">>,
      attrs = [{<<"xmlns">>, <<"jabber:ejabberd:banned">>}],
      children = [#xmlel{name = <<"reason">>, attrs = [], children = [{xmlcdata, Reason}]},
                  #xmlel{name = <<"lastdate">>, attrs = [], children = [{xmlcdata, LastDate}]},
                  #xmlel{name = <<"lastreason">>, attrs = [], children = [{xmlcdata, LastReason}]},
                  #xmlel{name = <<"bandate">>, attrs = [], children = [{xmlcdata, BanDate}]},
                  #xmlel{name = <<"hash">>, attrs = [], children = [{xmlcdata, Hash}]}]
     }.


%%
%% Get ban details


get_ban_details(User, Host) ->
    case private_get2(User, Host, <<"banned">>, <<"jabber:ejabberd:banned">>) of
        [El] ->
            get_ban_details(User, Host, El);
        [] ->
            []
    end.


get_ban_details(User, Host, El) ->
    Reason = fxml:get_subtag_cdata(El, <<"reason">>),
    LastDate = fxml:get_subtag_cdata(El, <<"lastdate">>),
    LastReason = fxml:get_subtag_cdata(El, <<"lastreason">>),
    BanDate = fxml:get_subtag_cdata(El, <<"bandate">>),
    Hash = fxml:get_subtag_cdata(El, <<"hash">>),
    case Hash == get_hash_value(User, Host) of
        true ->
            [{"reason", Reason},
             {"bandate", BanDate},
             {"lastdate", LastDate},
             {"lastreason", LastReason}];
        false ->
            []
    end.


is_banned(User, Host) ->
    case lists:keyfind("bandate", 1, get_ban_details(User, Host)) of
        {_, BanDate} when BanDate /= <<>> ->
            true;
        _ ->
            false
    end.


%%
%% Unban account


unban_account(User, Host) ->
    IsPrivateEnabled = gen_mod:is_loaded(Host, mod_private),
    Exists = ejabberd_auth:user_exists(User, Host),
    IsBanned = is_banned(User, Host),
    case {IsPrivateEnabled, Exists, IsBanned} of
        {true, true, true} ->
            unban_account2(User, Host);
        {false, _, _} ->
            mod_private_is_required_but_disabled;
        {_, false, _} ->
            account_does_not_exist;
        {_, _, false} ->
            account_was_not_banned;
        {_, _, _} ->
            other_error
    end.


unban_account2(User, Host) ->
    UnBanPrivateXml = build_unban_xmlel(),
    private_set2(User, Host, UnBanPrivateXml).


build_unban_xmlel() ->
    #xmlel{name = <<"banned">>, attrs = [{<<"xmlns">>, <<"jabber:ejabberd:banned">>}]}.


%%%
%%% Sessions
%%%


num_resources(User, Host) ->
    length(ejabberd_sm:get_user_resources(User, Host)).


resource_num(User, Host, Num) ->
    Resources = ejabberd_sm:get_user_resources(User, Host),
    case (0 < Num) and (Num =< length(Resources)) of
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
    [ {U, S, R, num_prio(P), St} || {U, S, R, P, St} <- Res ].


status_list(Status) ->
    status_list(<<"all">>, Status).


status_list_v3(ArgHost, Status) ->
    List = status_list(ArgHost, Status),
    [ {jid:encode(jid:make(User, Host, Resource)), Priority, StatusText}
      || {User, Host, Resource, Priority, StatusText} <- List ].


status_list_v3(Status) ->
    status_list_v3(<<"all">>, Status).


get_status_list(Host, Status_required) ->
    %% Get list of all logged users
    Sessions = ejabberd_sm:dirty_get_my_sessions_list(),
    %% Reformat the list
    Sessions2 = [ {Session#session.usr, Session#session.sid, Session#session.priority} || Session <- Sessions ],
    Fhost = case Host of
                <<"all">> ->
                    %% All hosts are requested, so don't filter at all
                    fun(_, _) -> true end;
                _ ->
                    %% Filter the list, only Host is interesting
                    fun(A, B) -> A == B end
            end,
    Sessions3 = [ {Pid, Server, Priority} || {{_User, Server, _Resource}, {_, Pid}, Priority} <- Sessions2, apply(Fhost, [Server, Host]) ],
    %% For each Pid, get its presence
    Sessions4 = [ {catch get_presence(Pid), Server, Priority} || {Pid, Server, Priority} <- Sessions3 ],
    %% Filter by status
    Fstatus = case Status_required of
                  <<"all">> ->
                      fun(_, _) -> true end;
                  _ ->
                      fun(A, B) -> A == B end
              end,
    [ {User, Server, Resource, num_prio(Priority), stringize(Status_text)}
      || {{User, Resource, Status, Status_text}, Server, Priority} <- Sessions4,
         apply(Fstatus, [Status, Status_required]) ].


connected_users_info() ->
    lists:filtermap(
      fun({U, S, R}) ->
              case user_session_info(U, S, R) of
                  offline ->
                      false;
                  Info ->
                      Jid = jid:encode(jid:make(U, S, R)),
                      {true, erlang:insert_element(1, Info, Jid)}
              end
      end,
      ejabberd_sm:dirty_get_sessions_list()).


connected_users_vhost(Host) ->
    USRs = ejabberd_sm:get_vh_session_list(Host),
    [ jid:encode(jid:make(USR)) || USR <- USRs ].


%% Make string more print-friendly
stringize(String) ->
    %% Replace newline characters with other code
    ejabberd_regexp:greplace(String, <<"\n">>, <<"\\n">>).


get_presence(Pid) ->
    try get_presence2(Pid) of
        {_, _, _, _} = Res ->
            Res
    catch
        _:_ -> {<<"">>, <<"">>, <<"offline">>, <<"">>}
    end.


get_presence2(Pid) ->
    Pres = #presence{from = From} = ejabberd_c2s:get_presence(Pid),
    Show = case Pres of
               #presence{type = unavailable} -> <<"unavailable">>;
               #presence{show = undefined} -> <<"available">>;
               #presence{show = S} -> atom_to_binary(S, utf8)
           end,
    Status = xmpp:get_text(Pres#presence.status),
    {From#jid.user, From#jid.resource, Show, Status}.


get_presence(U, S) ->
    Pids = [ ejabberd_sm:get_session_pid(U, S, R)
             || R <- ejabberd_sm:get_user_resources(U, S) ],
    OnlinePids = [ Pid || Pid <- Pids, Pid =/= none ],
    case OnlinePids of
        [] ->
            {jid:encode({U, S, <<>>}), <<"unavailable">>, <<"">>};
        [SessionPid | _] ->
            {_User, Resource, Show, Status} = get_presence(SessionPid),
            FullJID = jid:encode({U, S, Resource}),
            {FullJID, Show, Status}
    end.


set_presence(User, Host, Resource, Type, Show, Status, Priority) when is_binary(Priority) ->
    set_presence(User, Host, Resource, Type, Show, Status, binary_to_integer(Priority));

set_presence(User, Host, Resource, Type, Show, Status, Priority) ->
    Pres = #presence{
             from = jid:make(User, Host, Resource),
             to = jid:make(User, Host),
             type = misc:binary_to_atom(Type),
             status = xmpp:mk_text(Status),
             show = misc:binary_to_atom(Show),
             priority = Priority,
             sub_els = []
            },
    case ejabberd_sm:get_session_pid(User, Host, Resource) of
        none -> throw({error, "User session not found"});
        Ref -> ejabberd_c2s:set_presence(Ref, Pres)
    end.


user_sessions_info(User, Host) ->
    lists:filtermap(fun(Resource) ->
                            case user_session_info(User, Host, Resource) of
                                offline -> false;
                                Info -> {true, Info}
                            end
                    end,
                    ejabberd_sm:get_user_resources(User, Host)).


user_session_info(User, Host, Resource) ->
    CurrentSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    case ejabberd_sm:get_user_info(User, Host, Resource) of
        offline ->
            offline;
        Info ->
            Now = proplists:get_value(ts, Info),
            Pid = proplists:get_value(pid, Info),
            {_U, _Resource, Status, StatusText} = get_presence(Pid),
            Priority = proplists:get_value(priority, Info),
            Conn = proplists:get_value(conn, Info),
            {Ip, Port} = proplists:get_value(ip, Info),
            IPS = inet_parse:ntoa(Ip),
            NodeS = atom_to_list(node(Pid)),
            Uptime = CurrentSec - calendar:datetime_to_gregorian_seconds(
                                    calendar:now_to_local_time(Now)),
            {atom_to_list(Conn), IPS, Port, num_prio(Priority), NodeS, Uptime, Status, Resource, StatusText}
    end.


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
%% Room vcard


is_muc_service(Domain) ->
    try mod_muc_admin:get_room_serverhost(Domain) of
        Domain -> false;
        Service when is_binary(Service) -> true
    catch
        _:{unregistered_route, _} ->
            throw(error_wrong_hostname)
    end.


get_room_vcard(Name, Service) ->
    case mod_muc_admin:get_room_options(Name, Service) of
        [] ->
            throw(error_no_vcard_found);
        Opts ->
            case lists:keyfind(<<"vcard">>, 1, Opts) of
                false ->
                    throw(error_no_vcard_found);
                {_, VCardRaw} ->
                    [fxml_stream:parse_element(VCardRaw)]
            end
    end.


%%
%% Internal vcard


get_vcard_content(User, Server, Data) ->
    case get_vcard_element(User, Server) of
        [El | _] ->
            case get_vcard(Data, El) of
                [false] -> throw(error_no_value_found_in_vcard);
                ElemList -> ?DEBUG("ELS ~p", [ElemList]), [ fxml:get_tag_cdata(Elem) || Elem <- ElemList ]
            end;
        [] ->
            throw(error_no_vcard_found);
        error ->
            throw(database_failure)
    end.


get_vcard_element(User, Server) ->
    case is_muc_service(Server) of
        true ->
            get_room_vcard(User, Server);
        false ->
            mod_vcard:get_vcard(jid:nodeprep(User), jid:nameprep(Server))
    end.


get_vcard([<<"TEL">>, TelType], {_, _, _, OldEls}) ->
    {TakenEl, _NewEls} = take_vcard_tel(TelType, OldEls, [], not_found),
    [TakenEl];

get_vcard([Data1, Data2], A1) ->
    case get_subtag(A1, Data1) of
        [false] -> [false];
        A2List ->
            lists:flatten([ get_vcard([Data2], A2) || A2 <- A2List ])
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
    SubEl = {xmlel, <<"vCard">>, [{<<"xmlns">>, <<"vcard-temp">>}], A4},
    mod_vcard:set_vcard(User, jid:nameprep(Server), SubEl).


take_vcard_tel(TelType, [{xmlel, <<"TEL">>, _, SubEls} = OldEl | OldEls], NewEls, Taken) ->
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
    NewEl = {xmlel, <<"TEL">>,
                    [],
                    [{xmlel, TelType, [], []},
                     {xmlel, <<"NUMBER">>, [], [{xmlcdata, TelValue}]}]},
    [NewEl | NewEls];

update_vcard_els(Data, ContentList, Els1) ->
    Els2 = lists:keysort(2, Els1),
    [Data1 | Data2] = Data,
    NewEls = case Data2 of
                 [] ->
                     [ {xmlel, Data1, [], [{xmlcdata, Content}]} || Content <- ContentList ];
                 [D2] ->
                     OldEl = case lists:keysearch(Data1, 2, Els2) of
                                 {value, A} -> A;
                                 false -> {xmlel, Data1, [], []}
                             end,
                     {xmlel, _, _, ContentOld1} = OldEl,
                     Content2 = [ {xmlel, D2, [], [{xmlcdata, Content}]} || Content <- ContentList ],
                     ContentOld2 = [ A || {_, X, _, _} = A <- ContentOld1, X /= D2 ],
                     ContentOld3 = lists:keysort(2, ContentOld2),
                     ContentNew = lists:keymerge(2, Content2, ContentOld3),
                     [{xmlel, Data1, [], ContentNew}]
             end,
    Els3 = lists:keydelete(Data1, 2, Els2),
    lists:keymerge(2, NewEls, Els3).


%%%
%%% Roster
%%%


add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs) when is_binary(Group) ->
    add_rosteritem(LocalUser, LocalServer, User, Server, Nick, [Group], Subs);
add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Groups, Subs) ->
    case {jid:make(LocalUser, LocalServer), jid:make(User, Server)} of
        {error, _} ->
            throw({error, "Invalid 'localuser'/'localserver'"});
        {_, error} ->
            throw({error, "Invalid 'user'/'server'"});
        {Jid, _Jid2} ->
            RosterItem = build_roster_item(User, Server, {add, Nick, Subs, Groups}),
            case mod_roster:set_item_and_notify_clients(Jid, RosterItem, true) of
                ok -> ok;
                _ -> error
            end
    end.


subscribe(LU, LS, User, Server, Nick, Group, Subscription, _Xattrs) ->
    case {jid:make(LU, LS), jid:make(User, Server)} of
        {error, _} ->
            throw({error, "Invalid 'localuser'/'localserver'"});
        {_, error} ->
            throw({error, "Invalid 'user'/'server'"});
        {_Jid, _Jid2} ->
            ItemEl = build_roster_item(User, Server, {add, Nick, Subscription, Group}),
            mod_roster:set_items(LU, LS, #roster_query{items = [ItemEl]})
    end.


delete_rosteritem(LocalUser, LocalServer, User, Server) ->
    case {jid:make(LocalUser, LocalServer), jid:make(User, Server)} of
        {error, _} ->
            throw({error, "Invalid 'localuser'/'localserver'"});
        {_, error} ->
            throw({error, "Invalid 'user'/'server'"});
        {Jid, _Jid2} ->
            RosterItem = build_roster_item(User, Server, remove),
            case mod_roster:set_item_and_notify_clients(Jid, RosterItem, true) of
                ok -> ok;
                _ -> error
            end
    end.


%% -----------------------------
%% Get Roster
%% -----------------------------


get_roster(User, Server) ->
    case jid:make(User, Server) of
        error ->
            throw({error, "Invalid 'user'/'server'"});
        #jid{luser = U, lserver = S} ->
            Items = ejabberd_hooks:run_fold(roster_get, S, [], [{U, S}]),
            make_roster_xmlrpc(Items)
    end.


make_roster_xmlrpc(Roster) ->
    lists:map(
      fun(#roster_item{jid = JID, name = Nick, subscription = Sub, ask = Ask, groups = Groups}) ->
              JIDS = jid:encode(JID),
              Subs = atom_to_list(Sub),
              Asks = atom_to_list(Ask),
              {JIDS, Nick, Subs, Asks, Groups}
      end,
      Roster).


get_roster_count(User, Server) ->
    case jid:make(User, Server) of
        error ->
            throw({error, "Invalid 'user'/'server'"});
        #jid{luser = U, lserver = S} ->
            Items = ejabberd_hooks:run_fold(roster_get, S, [], [{U, S}]),
            length(Items)
    end.


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
    subscribe(iolist_to_binary(Name1),
              iolist_to_binary(Server1),
              iolist_to_binary(Name2),
              iolist_to_binary(Server2),
              iolist_to_binary(Nick2),
              iolist_to_binary(Group2),
              <<"both">>,
              []),
    subscribe_roster({Name1, Server1, Group1, Nick1}, Roster).


push_alltoall(S, G) ->
    Users = ejabberd_auth:get_users(S),
    Users2 = build_list_users(G, Users, []),
    subscribe_all(Users2),
    ok.


build_list_users(_Group, [], Res) ->
    Res;
build_list_users(Group, [{User, Server} | Users], Res) ->
    build_list_users(Group, Users, [{User, Server, Group, User} | Res]).


%% @spec(LU, LS, U, S, Action) -> ok
%%       Action = {add, Nick, Subs, Group} | remove
%% @doc Push to the roster of account LU@LS the contact U@S.
%% The specific action to perform is defined in Action.
push_roster_item(LU, LS, U, S, Action) ->
    lists:foreach(fun(R) ->
                          push_roster_item(LU, LS, R, U, S, Action)
                  end,
                  ejabberd_sm:get_user_resources(LU, LS)).


push_roster_item(LU, LS, R, U, S, Action) ->
    LJID = jid:make(LU, LS, R),
    BroadcastEl = build_broadcast(U, S, Action),
    ejabberd_sm:route(LJID, BroadcastEl),
    Item = build_roster_item(U, S, Action),
    ResIQ = build_iq_roster_push(Item),
    ejabberd_router:route(
      xmpp:set_from_to(ResIQ, jid:remove_resource(LJID), LJID)).


build_roster_item(U, S, {add, Nick, Subs, Groups}) when is_list(Groups) ->
    #roster_item{
      jid = jid:make(U, S),
      name = Nick,
      subscription = misc:binary_to_atom(Subs),
      groups = Groups
     };
build_roster_item(U, S, {add, Nick, Subs, Group}) ->
    Groups = binary:split(Group, <<";">>, [global, trim]),
    #roster_item{
      jid = jid:make(U, S),
      name = Nick,
      subscription = misc:binary_to_atom(Subs),
      groups = Groups
     };
build_roster_item(U, S, remove) ->
    #roster_item{jid = jid:make(U, S), subscription = remove}.


build_iq_roster_push(Item) ->
    #iq{
      type = set,
      id = <<"push">>,
      sub_els = [#roster_query{items = [Item]}]
     }.


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
                                    {erlang:timestamp(), "NOT FOUND"};
                                {ok, Shift, Status1} ->
                                    {{Shift div 1000000, Shift rem 1000000, 0}, Status1}
                            end;
                        _ ->
                            {erlang:timestamp(), "ONLINE"}
                    end,
    {xmpp_util:encode_timestamp(Now), Status}.


set_last(User, Server, Timestamp, Status) ->
    case mod_last:store_last_info(User, Server, Timestamp, Status) of
        {ok, _} -> ok;
        Error -> Error
    end.


%%%
%%% Private Storage
%%%

%% Example usage:
%% $ ejabberdctl private_set badlop localhost "\<aa\ xmlns=\'bb\'\>Cluth\</aa\>"
%% $ ejabberdctl private_get badlop localhost aa bb
%% <aa xmlns='bb'>Cluth</aa>


private_get(Username, Host, Element, Ns) ->
    Els = private_get2(Username, Host, Element, Ns),
    binary_to_list(fxml:element_to_binary(xmpp:encode(#private{sub_els = Els}))).


private_get2(Username, Host, Element, Ns) ->
    case gen_mod:is_loaded(Host, mod_private) of
        true -> private_get3(Username, Host, Element, Ns);
        false -> []
    end.


private_get3(Username, Host, Element, Ns) ->
    ElementXml = #xmlel{name = Element, attrs = [{<<"xmlns">>, Ns}]},
    mod_private:get_data(jid:nodeprep(Username),
                         jid:nameprep(Host),
                         [{Ns, ElementXml}]).


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
    JID = jid:make(Username, Host),
    mod_private:set_data(JID, [{NS, Xml}]).


%%%
%%% Shared Roster Groups
%%%


srg_create(Group, Host, Label, Description, Display) when is_binary(Display) ->
    DisplayList = case Display of
                      <<>> -> [];
                      _ -> ejabberd_regexp:split(Display, <<"\\\\n">>)
                  end,
    srg_create(Group, Host, Label, Description, DisplayList);

srg_create(Group, Host, Label, Description, DisplayList) ->
    {_DispGroups, WrongDispGroups} = filter_groups_existence(Host, DisplayList),
    case (WrongDispGroups -- [Group]) /= [] of
        true ->
            {wrong_displayed_groups, WrongDispGroups};
        false ->
            srg_create2(Group, Host, Label, Description, DisplayList)
    end.


srg_create2(Group, Host, Label, Description, DisplayList) ->
    Opts = [{label, Label},
            {displayed_groups, DisplayList},
            {description, Description}],
    case mod_shared_roster:create_group(Host, Group, Opts) of
        {atomic, _} -> ok;
        {error, Err} -> Err
    end.


srg_add(Group, Host) ->
    Opts = [{label, <<"">>},
            {description, <<"">>},
            {displayed_groups, []}],
    case mod_shared_roster:create_group(Host, Group, Opts) of
        {atomic, _} -> ok;
        {error, Err} -> Err
    end.


srg_delete(Group, Host) ->
    {atomic, _} = mod_shared_roster:delete_group(Host, Group),
    ok.


srg_list(Host) ->
    lists:sort(mod_shared_roster:list_groups(Host)).


srg_get_info(Group, Host) ->
    Opts = case mod_shared_roster:get_group_opts(Host, Group) of
               Os when is_list(Os) -> Os;
               error -> []
           end,
    [ {misc:atom_to_binary(Title), to_list(Value)} || {Title, Value} <- Opts ].


to_list([]) -> [];
to_list([H | _] = List) when is_binary(H) -> lists:join(", ", [ to_list(E) || E <- List ]);
to_list(E) when is_atom(E) -> atom_to_list(E);
to_list(E) when is_binary(E) -> binary_to_list(E).


%% @format-begin


srg_set_info(Group, Host, Key, Value) ->
    Opts =
        case mod_shared_roster:get_group_opts(Host, Group) of
            Os when is_list(Os) ->
                Os;
            error ->
                []
        end,
    Opts2 = srg_set_info(Key, Value, Opts),
    case mod_shared_roster:set_group_opts(Host, Group, Opts2) of
        {atomic, ok} ->
            ok;
        Problem ->
            ?INFO_MSG("Problem: ~n  ~p", [Problem]),  %+++
            error
    end.


srg_set_info(<<"description">>, Value, Opts) ->
    [{description, Value} | proplists:delete(description, Opts)];
srg_set_info(<<"label">>, Value, Opts) ->
    [{label, Value} | proplists:delete(label, Opts)];
srg_set_info(<<"all_users">>, <<"true">>, Opts) ->
    [{all_users, true} | proplists:delete(all_users, Opts)];
srg_set_info(<<"online_users">>, <<"true">>, Opts) ->
    [{online_users, true} | proplists:delete(online_users, Opts)];
srg_set_info(<<"all_users">>, _, Opts) ->
    proplists:delete(all_users, Opts);
srg_set_info(<<"online_users">>, _, Opts) ->
    proplists:delete(online_users, Opts);
srg_set_info(Key, _Value, Opts) ->
    ?ERROR_MSG("Unknown Key in srg_set_info: ~p", [Key]),
    Opts.


srg_get_displayed(Group, Host) ->
    Opts =
        case mod_shared_roster:get_group_opts(Host, Group) of
            Os when is_list(Os) ->
                Os;
            error ->
                []
        end,
    proplists:get_value(displayed_groups, Opts, []).


srg_add_displayed(Group, Host, NewGroup) ->
    Opts =
        case mod_shared_roster:get_group_opts(Host, Group) of
            Os when is_list(Os) ->
                Os;
            error ->
                []
        end,
    {DispGroups, WrongDispGroups} = filter_groups_existence(Host, [NewGroup]),
    case WrongDispGroups /= [] of
        true ->
            {wrong_displayed_groups, WrongDispGroups};
        false ->
            DisplayedOld = proplists:get_value(displayed_groups, Opts, []),
            Opts2 =
                [{displayed_groups, lists:flatten(DisplayedOld, DispGroups)} | proplists:delete(displayed_groups, Opts)],
            case mod_shared_roster:set_group_opts(Host, Group, Opts2) of
                {atomic, ok} ->
                    ok;
                Problem ->
                    ?INFO_MSG("Problem: ~n  ~p", [Problem]),  %+++
                    error
            end
    end.


srg_del_displayed(Group, Host, OldGroup) ->
    Opts =
        case mod_shared_roster:get_group_opts(Host, Group) of
            Os when is_list(Os) ->
                Os;
            error ->
                []
        end,
    DisplayedOld = proplists:get_value(displayed_groups, Opts, []),
    {DispGroups, OldDispGroups} = lists:partition(fun(G) -> G /= OldGroup end, DisplayedOld),
    case OldDispGroups == [] of
        true ->
            {inexistent_displayed_groups, OldGroup};
        false ->
            Opts2 = [{displayed_groups, DispGroups} | proplists:delete(displayed_groups, Opts)],
            case mod_shared_roster:set_group_opts(Host, Group, Opts2) of
                {atomic, ok} ->
                    ok;
                Problem ->
                    ?INFO_MSG("Problem: ~n  ~p", [Problem]),  %+++
                    error
            end
    end.


filter_groups_existence(Host, Groups) ->
    lists:partition(fun(Group) -> error /= mod_shared_roster:get_group_opts(Host, Group) end,
                    Groups).
%% @format-end


srg_get_members(Group, Host) ->
    Members = mod_shared_roster:get_group_explicit_users(Host, Group),
    [ jid:encode(jid:make(MUser, MServer))
      || {MUser, MServer} <- Members ].


srg_user_add(User, Host, Group, GroupHost) ->
    mod_shared_roster:add_user_to_group(GroupHost, {User, Host}, Group),
    ok.


srg_user_del(User, Host, Group, GroupHost) ->
    mod_shared_roster:remove_user_from_group(GroupHost, {User, Host}, Group),
    ok.


%%%
%%% Stanza
%%%


%% @doc Send a message to an XMPP account.
-spec send_message(Type :: binary(),
                   From :: binary(),
                   To :: binary(),
                   Subject :: binary(),
                   Body :: binary()) -> ok.
send_message(Type, From, To, Subject, Body) ->
    CodecOpts = ejabberd_config:codec_options(),
    try xmpp:decode(
          #xmlel{
            name = <<"message">>,
            attrs = [{<<"to">>, To},
                     {<<"from">>, From},
                     {<<"type">>, Type},
                     {<<"id">>, p1_rand:get_string()}],
            children =
                [#xmlel{
                   name = <<"subject">>,
                   children = [{xmlcdata, Subject}]
                  },
                 #xmlel{
                   name = <<"body">>,
                   children = [{xmlcdata, Body}]
                  }]
           },
          ?NS_CLIENT,
          CodecOpts) of
        #message{from = JID, subject = SubjectEl, body = BodyEl} = Msg ->
            Msg2 = case {xmpp:get_text(SubjectEl), xmpp:get_text(BodyEl)} of
                       {Subject, <<>>} -> Msg;
                       {<<>>, Body} -> Msg#message{subject = []};
                       _ -> Msg
                   end,
            State = #{jid => JID},
            ejabberd_hooks:run_fold(user_send_packet, JID#jid.lserver, {Msg2, State}, []),
            ejabberd_router:route(Msg2)
    catch
        _:{xmpp_codec, Why} ->
            {error, xmpp:format_error(Why)}
    end.


send_stanza(FromString, ToString, Stanza) ->
    try
        #xmlel{} = El = fxml_stream:parse_element(Stanza),
        From = jid:decode(FromString),
        To = jid:decode(ToString),
        CodecOpts = ejabberd_config:codec_options(),
        Pkt = xmpp:decode(El, ?NS_CLIENT, CodecOpts),
        Pkt2 = xmpp:set_from_to(Pkt, From, To),
        State = #{jid => From},
        ejabberd_hooks:run_fold(user_send_packet,
                                From#jid.lserver,
                                {Pkt2, State},
                                []),
        ejabberd_router:route(Pkt2)
    catch
        _:{xmpp_codec, Why} ->
            io:format("incorrect stanza: ~ts~n", [xmpp:format_error(Why)]),
            {error, Why};
        _:{badmatch, {error, {Code, Why}}} when is_integer(Code) ->
            io:format("invalid xml: ~p~n", [Why]),
            {error, Why};
        _:{badmatch, {error, Why}} ->
            io:format("invalid xml: ~p~n", [Why]),
            {error, Why};
        _:{bad_jid, S} ->
            io:format("malformed JID: ~ts~n", [S]),
            {error, "JID malformed"}
    end.


-spec send_stanza_c2s(binary(), binary(), binary(), binary()) -> ok | {error, any()}.
send_stanza_c2s(Username, Host, Resource, Stanza) ->
    try
        #xmlel{} = El = fxml_stream:parse_element(Stanza),
        CodecOpts = ejabberd_config:codec_options(),
        Pkt = xmpp:decode(El, ?NS_CLIENT, CodecOpts),
        case ejabberd_sm:get_session_pid(Username, Host, Resource) of
            Pid when is_pid(Pid) ->
                ejabberd_c2s:send(Pid, Pkt);
            _ ->
                {error, no_session}
        end
    catch
        _:{badmatch, {error, Why} = Err} ->
            io:format("invalid xml: ~p~n", [Why]),
            Err;
        _:{xmpp_codec, Why} ->
            io:format("incorrect stanza: ~ts~n", [xmpp:format_error(Why)]),
            {error, Why}
    end.


privacy_set(Username, Host, QueryS) ->
    Jid = jid:make(Username, Host),
    QueryEl = fxml_stream:parse_element(QueryS),
    SubEl = xmpp:decode(QueryEl),
    IQ = #iq{
           type = set,
           id = <<"push">>,
           sub_els = [SubEl],
           from = Jid,
           to = Jid
          },
    Result = mod_privacy:process_iq(IQ),
    Result#iq.type == result.


%%%
%%% Stats
%%%


stats(Name) ->
    case Name of
        <<"uptimeseconds">> -> trunc(element(1, erlang:statistics(wall_clock)) / 1000);
        <<"processes">> -> length(erlang:processes());
        <<"registeredusers">> -> lists:foldl(fun(Host, Sum) -> ejabberd_auth:count_users(Host) + Sum end, 0, ejabberd_option:hosts());
        <<"onlineusersnode">> -> length(ejabberd_sm:dirty_get_my_sessions_list());
        <<"onlineusers">> -> length(ejabberd_sm:dirty_get_sessions_list())
    end.


stats(Name, Host) ->
    case Name of
        <<"registeredusers">> -> ejabberd_auth:count_users(Host);
        <<"onlineusers">> -> length(ejabberd_sm:get_vh_session_list(Host))
    end.


user_action(User, Server, Fun, OK) ->
    case ejabberd_auth:user_exists(User, Server) of
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


num_prio(Priority) when is_integer(Priority) ->
    Priority;
num_prio(_) ->
    -1.


%%%
%%% Web Admin
%%%

%% @format-begin

%%% Main


web_menu_main(Acc, _Lang) ->
    Acc ++ [{<<"stats">>, <<"Statistics">>}].


web_page_main(_, #request{path = [<<"stats">>]} = R) ->
    Res = ?H1GL(<<"Statistics">>, <<"modules/#mod_stats">>, <<"mod_stats">>) ++
        [make_command(stats_host, R, [], [{only, presentation}]),
         make_command(incoming_s2s_number, R, [], [{only, presentation}]),
         make_command(outgoing_s2s_number, R, [], [{only, presentation}]),
         make_table([<<"stat name">>, {<<"stat value">>, right}],
                    [{?C(<<"Registered Users:">>),
                      make_command(stats,
                                   R,
                                   [{<<"name">>, <<"registeredusers">>}],
                                   [{only, value}])},
                     {?C(<<"Online Users:">>),
                      make_command(stats,
                                   R,
                                   [{<<"name">>, <<"onlineusers">>}],
                                   [{only, value}])},
                     {?C(<<"S2S Connections Incoming:">>),
                      make_command(incoming_s2s_number, R, [], [{only, value}])},
                     {?C(<<"S2S Connections Outgoing:">>),
                      make_command(outgoing_s2s_number, R, [], [{only, value}])}])],
    {stop, Res};
web_page_main(Acc, _) ->
    Acc.


%%% Host


web_menu_host(Acc, _Host, _Lang) ->
    Acc ++ [{<<"purge">>, <<"Purge">>}, {<<"stats">>, <<"Statistics">>}].


web_page_host(_, Host, #request{path = [<<"purge">>]} = R) ->
    Head = [?XC(<<"h1">>, <<"Purge">>)],
    Set = [ejabberd_web_admin:make_command(delete_old_users_vhost,
                                           R,
                                           [{<<"host">>, Host}],
                                           [])],
    {stop, Head ++ Set};
web_page_host(_, Host, #request{path = [<<"stats">>]} = R) ->
    Res = ?H1GL(<<"Statistics">>, <<"modules/#mod_stats">>, <<"mod_stats">>) ++
        [make_command(stats_host, R, [], [{only, presentation}]),
         make_table([<<"stat name">>, {<<"stat value">>, right}],
                    [{?C(<<"Registered Users:">>),
                      make_command(stats_host,
                                   R,
                                   [{<<"host">>, Host}, {<<"name">>, <<"registeredusers">>}],
                                   [{only, value},
                                    {result_links, [{stat, arg_host, 3, <<"users">>}]}])},
                     {?C(<<"Online Users:">>),
                      make_command(stats_host,
                                   R,
                                   [{<<"host">>, Host}, {<<"name">>, <<"onlineusers">>}],
                                   [{only, value},
                                    {result_links,
                                     [{stat, arg_host, 3, <<"online-users">>}]}])}])],
    {stop, Res};
web_page_host(Acc, _, _) ->
    Acc.


%%% HostUser


web_menu_hostuser(Acc, _Host, _Username, _Lang) ->
    Acc ++ [{<<"auth">>, <<"Authentication">>}, {<<"session">>, <<"Sessions">>}].


web_page_hostuser(_, Host, User, #request{path = [<<"auth">>]} = R) ->
    Ban = make_command(ban_account,
                       R,
                       [{<<"user">>, User}, {<<"host">>, Host}],
                       [{style, danger}]),
    Unban = make_command(unban_account, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
    Res = ?H1GLraw(<<"Authentication">>,
                   <<"admin/configuration/authentication/">>,
                   <<"Authentication">>) ++
        [make_command(register, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
         make_command(check_account, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
         ?X(<<"hr">>),
         make_command(check_password, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
         make_command(check_password_hash, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
         make_command(change_password,
                      R,
                      [{<<"user">>, User}, {<<"host">>, Host}],
                      [{style, danger}]),
         ?X(<<"hr">>),
         make_command(get_ban_details, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
         Ban,
         Unban,
         ?X(<<"hr">>),
         make_command(unregister,
                      R,
                      [{<<"user">>, User}, {<<"host">>, Host}],
                      [{style, danger}])],
    {stop, Res};
web_page_hostuser(_, Host, User, #request{path = [<<"session">>]} = R) ->
    Head = [?XC(<<"h1">>, <<"Sessions">>), ?BR],
    Set = [make_command(resource_num, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
           make_command(set_presence, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
           make_command(kick_user, R, [{<<"user">>, User}, {<<"host">>, Host}], [{style, danger}]),
           make_command(kick_session,
                        R,
                        [{<<"user">>, User}, {<<"host">>, Host}],
                        [{style, danger}])],
    timer:sleep(100),  % kicking sessions takes a while, let's delay the get commands
    Get = [make_command(user_sessions_info,
                        R,
                        [{<<"user">>, User}, {<<"host">>, Host}],
                        [{result_links, [{node, node, 5, <<>>}]}]),
           make_command(user_resources, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
           make_command(get_presence, R, [{<<"user">>, User}, {<<"host">>, Host}], []),
           make_command(num_resources, R, [{<<"user">>, User}, {<<"host">>, Host}], [])],
    {stop, Head ++ Get ++ Set};
web_page_hostuser(Acc, _, _, _) ->
    Acc.


%%% HostNode


web_menu_hostnode(Acc, _Host, _Username, _Lang) ->
    Acc ++ [{<<"modules">>, <<"Modules">>}].


web_page_hostnode(_, Host, Node, #request{path = [<<"modules">>]} = R) ->
    Res = ?H1GLraw(<<"Modules">>, <<"admin/configuration/modules/">>, <<"Modules Options">>) ++
        [ejabberd_cluster:call(Node,
                               ejabberd_web_admin,
                               make_command,
                               [restart_module, R, [{<<"host">>, Host}], []])],
    {stop, Res};
web_page_hostnode(Acc, _Host, _Node, _Request) ->
    Acc.


%%% Node


web_menu_node(Acc, _Node, _Lang) ->
    Acc ++ [{<<"stats">>, <<"Statistics">>}].


web_page_node(_, Node, #request{path = [<<"stats">>]} = R) ->
    UpSecs =
        ejabberd_cluster:call(Node,
                              ejabberd_web_admin,
                              make_command,
                              [stats, R, [{<<"name">>, <<"uptimeseconds">>}], [{only, value}]]),
    UpDaysBin =
        integer_to_binary(binary_to_integer(fxml:get_tag_cdata(UpSecs)) div
                          86400),  % 24*60*60
    UpDays =
        #xmlel{
          name = <<"code">>,
          attrs = [],
          children = [{xmlcdata, UpDaysBin}]
         },
    Res = ?H1GL(<<"Statistics">>, <<"modules/#mod_stats">>, <<"mod_stats">>) ++
        [make_command(stats, R, [], [{only, presentation}]),
         make_table([<<"stat name">>, {<<"stat value">>, right}],
                    [{?C(<<"Online Users in this node:">>),
                      ejabberd_cluster:call(Node,
                                            ejabberd_web_admin,
                                            make_command,
                                            [stats,
                                             R,
                                             [{<<"name">>, <<"onlineusersnode">>}],
                                             [{only, value}]])},
                     {?C(<<"Uptime Seconds:">>), UpSecs},
                     {?C(<<"Uptime Seconds (rounded to days):">>), UpDays},
                     {?C(<<"Processes:">>),
                      ejabberd_cluster:call(Node,
                                            ejabberd_web_admin,
                                            make_command,
                                            [stats,
                                             R,
                                             [{<<"name">>, <<"processes">>}],
                                             [{only, value}]])}])],
    {stop, Res};
web_page_node(Acc, _, _) ->
    Acc.
%% @format-end


%%%
%%% Document
%%%


mod_options(_) -> [].


mod_doc() ->
    #{
      desc =>
          [?T("This module provides additional administrative commands."),
           "",
           ?T("Details for some commands:"),
           "",
           ?T("_`ban_account`_ API:"),
           ?T("This command kicks all the connected sessions of the account "
              "from the server. It also changes their password to a randomly "
              "generated one, so they can't login anymore unless a server "
              "administrator changes their password again. It is possible to "
              "define the reason of the ban. The new password also includes "
              "the reason and the date and time of the ban. See an example below."),
           "",
           ?T("_`push_roster`_ API (and _`push_roster_all`_ API):"),
           ?T("The roster file must be placed, if using Windows, on the "
              "directory where you installed ejabberd: "
              "`C:/Program Files/ejabberd` or similar. If you use other "
              "Operating System, place the file on the same directory where "
              "the .beam files are installed. See below an example roster file."),
           "",
           ?T("_`srg_create`_ API:"),
           ?T("If you want to put a group Name with blank spaces, use the "
              "characters '\"\'' and '\'\"' to define when the Name starts and "
              "ends. See an example below.")],
      example =>
          [{?T("With this configuration, vCards can only be modified with "
               "mod_admin_extra commands:"),
            ["acl:",
             "  adminextraresource:",
             "    - resource: \"modadminextraf8x,31ad\"",
             "access_rules:",
             "  vcard_set:",
             "    - allow: adminextraresource",
             "modules:",
             "  mod_admin_extra: {}",
             "  mod_vcard:",
             "    access_set: vcard_set"]},
           {?T("Content of roster file for _`push_roster`_ API:"),
            ["[{<<\"bob\">>, <<\"example.org\">>, <<\"workers\">>, <<\"Bob\">>},",
             "{<<\"mart\">>, <<\"example.org\">>, <<\"workers\">>, <<\"Mart\">>},",
             "{<<\"Rich\">>, <<\"example.org\">>, <<\"bosses\">>, <<\"Rich\">>}]."]},
           {?T("With this call, the sessions of the local account which JID is "
               "'boby@example.org' will be kicked, and its password will be set "
               "to something like "
               "'BANNED_ACCOUNT--20080425T21:45:07--2176635--Spammed_rooms'"),
            ["ejabberdctl vhost example.org ban_account boby \"Spammed rooms\""]},
           {?T("Call to _`srg_create`_ API using double-quotes and single-quotes:"),
            ["ejabberdctl srg_create g1 example.org \"\'Group number 1\'\" this_is_g1 g1"]}]
     }.
