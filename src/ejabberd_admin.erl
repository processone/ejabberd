%%%-------------------------------------------------------------------
%%% File    : ejabberd_admin.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Administrative functions and commands
%%% Created :  7 May 2006 by Mickael Remond <mremond@process-one.net>
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

-module(ejabberd_admin).
-author('mickael.remond@process-one.net').

-behaviour(gen_server).

-export([start_link/0,
	 %% Server
	 status/0, stop/0, restart/0,
	 reopen_log/0, rotate_log/0,
	 set_loglevel/1,
	 evacuate_kindly/2,
         restart_kindly/2,
	 stop_kindly/2, send_service_message_all_mucs/2,
	 registered_vhosts/0,
	 reload_config/0,
	 dump_config/1,
	 convert_to_yaml/2,
	 %% Cluster
	 join_cluster/1, leave_cluster/1,
         join_cluster_here/1,
	 list_cluster/0, list_cluster_detailed/0,
	 get_cluster_node_details3/0,
	 %% Erlang
	 update_list/0, update/1, update/0,
	 %% Accounts
	 register/3, unregister/2,
	 registered_users/1,
	 %% Migration jabberd1.4
	 import_file/1, import_dir/1,
	 %% Purge DB
	 delete_expired_messages/0, delete_old_messages/1,
	 %% Mnesia
	 get_master/0, set_master/1,
	 backup_mnesia/1, restore_mnesia/1,
	 dump_mnesia/1, dump_table/2, load_mnesia/1,
	 mnesia_info/0, mnesia_table_info/1,
	 install_fallback_mnesia/1,
	 dump_to_textfile/1, dump_to_textfile/2,
	 mnesia_change_nodename/4,
	 restore/1, % Still used by some modules
	 clear_cache/0,
	 gc/0,
	 get_commands_spec/0,
	 delete_old_messages_batch/4, delete_old_messages_status/1, delete_old_messages_abort/1,
	 %% Internal
	 mnesia_list_tables/0,
	 mnesia_table_details/1,
	 mnesia_table_change_storage/2,
	 mnesia_table_clear/1,
	 mnesia_table_delete/1,
	 echo/1, echo3/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([web_menu_main/2, web_page_main/2,
         web_menu_node/3, web_page_node/3]).

-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("logger.hrl").
-include("translate.hrl"). %+++ TODO

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    ejabberd_commands:register_commands(get_commands_spec()),
    ejabberd_hooks:add(webadmin_menu_main, ?MODULE, web_menu_main, 50),
    ejabberd_hooks:add(webadmin_page_main, ?MODULE, web_page_main, 50),
    ejabberd_hooks:add(webadmin_menu_node, ?MODULE, web_menu_node, 50),
    ejabberd_hooks:add(webadmin_page_node, ?MODULE, web_page_node, 50),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(webadmin_menu_main, ?MODULE, web_menu_main, 50),
    ejabberd_hooks:delete(webadmin_page_main, ?MODULE, web_page_main, 50),
    ejabberd_hooks:delete(webadmin_menu_node, ?MODULE, web_menu_node, 50),
    ejabberd_hooks:delete(webadmin_page_node, ?MODULE, web_page_node, 50),
    ejabberd_commands:unregister_commands(get_commands_spec()).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% ejabberd commands
%%%

get_commands_spec() ->
    [
     %% The commands status, stop and restart are implemented also in ejabberd_ctl
     %% They are defined here so that other interfaces can use them too
     #ejabberd_commands{name = status, tags = [server],
			desc = "Get status of the ejabberd server",
			module = ?MODULE, function = status,
			result_desc = "Result tuple",
			result_example = {ok, <<"The node ejabberd@localhost is started with status: started"
						"ejabberd X.X is running in that node">>},
			args = [], result = {res, restuple}},
     #ejabberd_commands{name = stop, tags = [server],
			desc = "Stop ejabberd gracefully",
			module = ?MODULE, function = stop,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = halt, tags = [server],
			desc = "Halt ejabberd abruptly with status code 1",
			note = "added in 23.10",
			module = ejabberd, function = halt,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = restart, tags = [server],
			desc = "Restart ejabberd gracefully",
			module = ?MODULE, function = restart,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = reopen_log, tags = [logs],
			desc = "Reopen maybe the log files after being renamed",
			longdesc = "Has no effect on ejabberd main log files, "
			"only on log files generated by some modules.\n"
			"This can be useful when an external tool is "
			"used for log rotation. See "
			"_`../../admin/guide/troubleshooting.md#log-files|Log Files`_.",
			policy = admin,
			module = ?MODULE, function = reopen_log,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = rotate_log, tags = [logs],
			desc = "Rotate maybe log file of some module",
			longdesc = "Has no effect on ejabberd main log files, "
			"only on log files generated by some modules.",
			module = ?MODULE, function = rotate_log,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = evacuate_kindly, tags = [server],
			desc = "Evacuate kindly all users (kick and prevent login)",
			longdesc = "Inform users and rooms, don't allow login, wait, "
			"restart the server, and don't allow new logins.\n"
			"Provide the delay in seconds, and the "
			"announcement quoted, for example: \n"
			"`ejabberdctl evacuate_kindly 60 "
			"\\\"The server will stop in one minute.\\\"`",
			note = "added in 24.12",
			module = ?MODULE, function = evacuate_kindly,
			args_desc = ["Seconds to wait", "Announcement to send, with quotes"],
			args_example = [60, <<"Server will stop now.">>],
			args = [{delay, integer}, {announcement, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = restart_kindly, tags = [server, async],
			desc = "Restart kindly the server",
			longdesc = "Inform users and rooms, wait, and restart the server.\n"
                        "Provide the delay in seconds, and the "
			"announcement quoted, for example: \n"
			"`ejabberdctl restart_kindly 60 "
			"\\\"The server will stop in one minute.\\\"`",
			note = "added in 25.10",
			module = ?MODULE, function = restart_kindly,
			args_desc = ["Seconds to wait", "Announcement to send, with quotes"],
			args_example = [60, <<"Server will restart now.">>],
			args = [{delay, integer}, {announcement, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = stop_kindly, tags = [server, async],
			desc = "Stop kindly the server (informing users)",
			longdesc = "Inform users and rooms, wait, and stop the server.\n"
                        "Provide the delay in seconds, and the "
			"announcement quoted, for example: \n"
			"`ejabberdctl stop_kindly 60 "
			"\\\"The server will stop in one minute.\\\"`",
			module = ?MODULE, function = stop_kindly,
			args_desc = ["Seconds to wait", "Announcement to send, with quotes"],
			args_example = [60, <<"Server will stop now.">>],
			args = [{delay, integer}, {announcement, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = get_loglevel, tags = [logs],
			desc = "Get the current loglevel",
			module = ejabberd_logger, function = get,
			result_desc = "Tuple with the log level number, its keyword and description",
			result_example = warning,
			args = [],
                        result = {levelatom, atom}},
     #ejabberd_commands{name = set_loglevel, tags = [logs],
			desc = "Set the loglevel",
			longdesc = "Possible loglevels: `none`, `emergency`, `alert`, `critical`,
			           `error`, `warning`, `notice`, `info`, `debug`.",
			module = ?MODULE, function = set_loglevel,
			args_desc = ["Desired logging level"],
			args_example = ["debug"],
			args = [{loglevel, string}],
			result = {res, rescode}},

     #ejabberd_commands{name = update_list, tags = [server],
			desc = "List modified modules that can be updated",
			module = ?MODULE, function = update_list,
			args = [],
			result_example = ["mod_configure", "mod_vcard"],
			result = {modules, {list, {module, string}}}},
     #ejabberd_commands{name = update, tags = [server],
			desc = "Update the given module",
			longdesc = "To update all the possible modules, use `all`.",
			note = "improved in 24.10",
			module = ?MODULE, function = update,
			args_example = ["all"],
			args = [{module, string}],
			result_example = {ok, <<"Updated modules: mod_configure, mod_vcard">>},
			result = {res, restuple}},

     #ejabberd_commands{name = register, tags = [accounts],
			desc = "Register a user",
			policy = admin,
			module = ?MODULE, function = register,
			args_desc = ["Username", "Local vhost served by ejabberd", "Password"],
			args_example = [<<"bob">>, <<"example.com">>, <<"SomEPass44">>],
			args = [{user, binary}, {host, binary}, {password, binary}],
			result = {res, restuple}},
     #ejabberd_commands{name = unregister, tags = [accounts],
			desc = "Unregister a user",
			longdesc = "This deletes the authentication and all the "
                        "data associated to the account (roster, vcard...).",
                        policy = admin,
			module = ?MODULE, function = unregister,
			args_desc = ["Username", "Local vhost served by ejabberd"],
			args_example = [<<"bob">>, <<"example.com">>],
			args = [{user, binary}, {host, binary}],
			result = {res, restuple}},
     #ejabberd_commands{name = registered_users, tags = [accounts],
			desc = "List all registered users in HOST",
			module = ?MODULE, function = registered_users,
			args_desc = ["Local vhost"],
			args_example = [<<"example.com">>],
			result_desc = "List of registered accounts usernames",
			result_example = [<<"user1">>, <<"user2">>],
			args = [{host, binary}],
			result = {users, {list, {username, string}}}},
     #ejabberd_commands{name = registered_vhosts, tags = [server],
			desc = "List all registered vhosts in SERVER",
			module = ?MODULE, function = registered_vhosts,
			result_desc = "List of available vhosts",
			result_example = [<<"example.com">>, <<"anon.example.com">>],
			args = [],
			result = {vhosts, {list, {vhost, string}}}},
     #ejabberd_commands{name = reload_config, tags = [config],
			desc = "Reload config file in memory",
			module = ?MODULE, function = reload_config,
			args = [],
			result = {res, rescode}},

     #ejabberd_commands{name = join_cluster, tags = [cluster],
			desc = "Join our local node into the cluster handled by Node",
			longdesc = "This command returns immediately,
			even before the joining process has
			completed. Consequently, if you are using
			`ejabberdctl` (or some `CTL_ON_` container
			environment variables) to run more commands
			afterwards, you may want to precede them with
			the `started`
			_`../../admin/guide/managing.md#ejabberdctl-commands|ejabberdctl command`_
                        to ensure the
			clustering process has completed before
			proceeding. For example: `join_cluster
			ejabberd@main` > `started` > `list_cluster`.",
			note = "improved in 24.06",
			module = ?MODULE, function = join_cluster,
			args_desc = ["Nodename of the node to join"],
			args_example = [<<"ejabberd1@machine7">>],
			args = [{node, binary}],
			result = {res, restuple}},
     #ejabberd_commands{name = join_cluster_here, tags = [cluster],
			desc = "Join a remote Node here, into our cluster",
			note = "added in 24.06",
			module = ?MODULE, function = join_cluster_here,
			args_desc = ["Nodename of the node to join here"],
			args_example = [<<"ejabberd1@machine7">>],
			args = [{node, binary}],
			result = {res, restuple}},
     #ejabberd_commands{name = leave_cluster, tags = [cluster],
			desc = "Remove and shutdown Node from the running cluster",
			longdesc = "This command can be run from any running "
			"node of the cluster, even the node to be removed. "
			"In the removed node, this command works only when "
			"using ejabberdctl, not _`mod_http_api`_ or other code that "
			"runs inside the same ejabberd node that will leave.",
			module = ?MODULE, function = leave_cluster,
			args_desc = ["Nodename of the node to kick from the cluster"],
			args_example = [<<"ejabberd1@machine8">>],
			args = [{node, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = list_cluster, tags = [cluster],
			desc = "List running nodes that are part of this cluster",
			module = ?MODULE, function = list_cluster,
			result_example = [ejabberd1@machine7, ejabberd1@machine8],
			args = [],
			result = {nodes, {list, {node, atom}}}},
     #ejabberd_commands{name = list_cluster_detailed, tags = [cluster],
			desc = "List nodes (both running and known) and some stats",
			note = "added in 24.06",
			module = ?MODULE, function = list_cluster_detailed,
			args = [],
			result_example = [{'ejabberd@localhost', "true",
                                       "The node ejabberd is started. Status...",
                                       7, 348, 60, none}],
			result = {nodes, {list, {node, {tuple, [{name, atom},
                                                                {running, string},
                                                                {status, string},
                                                                {online_users, integer},
                                                                {processes, integer},
                                                                {uptime_seconds, integer},
                                                                {master_node, atom}
                                                               ]}}}}},

     #ejabberd_commands{name = import_file, tags = [mnesia],
			desc = "Import user data from jabberd14 spool file",
			module = ?MODULE, function = import_file,
			args_desc = ["Full path to the jabberd14 spool file"],
			args_example = ["/var/lib/ejabberd/jabberd14.spool"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = import_dir, tags = [mnesia],
			desc = "Import users data from jabberd14 spool dir",
			module = ?MODULE, function = import_dir,
			args_desc = ["Full path to the jabberd14 spool directory"],
			args_example = ["/var/lib/ejabberd/jabberd14/"],
			args = [{file, string}],
			result = {res, restuple}},
     #ejabberd_commands{name = import_piefxis, tags = [mnesia],
			desc = "Import users data from a PIEFXIS file (XEP-0227)",
			module = ejabberd_piefxis, function = import_file,
			args_desc = ["Full path to the PIEFXIS file"],
			args_example = ["/var/lib/ejabberd/example.com.xml"],
			args = [{file, binary}], result = {res, rescode}},
     #ejabberd_commands{name = export_piefxis, tags = [mnesia],
			desc = "Export data of all users in the server to PIEFXIS files (XEP-0227)",
			module = ejabberd_piefxis, function = export_server,
			args_desc = ["Full path to a directory"],
			args_example = ["/var/lib/ejabberd/"],
			args = [{dir, binary}], result = {res, rescode}},
     #ejabberd_commands{name = export_piefxis_host, tags = [mnesia],
			desc = "Export data of users in a host to PIEFXIS files (XEP-0227)",
			module = ejabberd_piefxis, function = export_host,
			args_desc = ["Full path to a directory", "Vhost to export"],
			args_example = ["/var/lib/ejabberd/", "example.com"],
			args = [{dir, binary}, {host, binary}], result = {res, rescode}},

     #ejabberd_commands{name = delete_mnesia, tags = [mnesia],
                        desc = "Delete elements in Mnesia database for a given vhost",
                        module = ejd2sql, function = delete,
			args_desc = ["Vhost which content will be deleted in Mnesia database"],
			args_example = ["example.com"],
                        args = [{host, string}], result = {res, rescode}},
     #ejabberd_commands{name = convert_to_scram, tags = [sql],
			desc = "Convert the passwords of users to SCRAM",
			module = ejabberd_auth, function = convert_to_scram,
			args_desc = ["Vhost which users' passwords will be scrammed"],
			args_example = ["example.com"],
			args = [{host, binary}], result = {res, rescode}},
     #ejabberd_commands{name = import_prosody, tags = [mnesia, sql],
			desc = "Import data from Prosody",
			longdesc = "Note: this requires ejabberd to be "
                        "compiled with `./configure --enable-lua` "
			"(which installs the `luerl` library).",
			module = prosody2ejabberd, function = from_dir,
			args_desc = ["Full path to the Prosody data directory"],
			args_example = ["/var/lib/prosody/datadump/"],
			args = [{dir, string}], result = {res, rescode}},

     #ejabberd_commands{name = convert_to_yaml, tags = [config],
                        desc = "Convert the input file from Erlang to YAML format",
                        module = ?MODULE, function = convert_to_yaml,
			args_desc = ["Full path to the original configuration file", "And full path to final file"],
			args_example = ["/etc/ejabberd/ejabberd.cfg", "/etc/ejabberd/ejabberd.yml"],
                        args = [{in, string}, {out, string}],
                        result = {res, rescode}},
     #ejabberd_commands{name = dump_config, tags = [config],
			desc = "Dump configuration in YAML format as seen by ejabberd",
			module = ?MODULE, function = dump_config,
			args_desc = ["Full path to output file"],
			args_example = ["/tmp/ejabberd.yml"],
			args = [{out, string}],
			result = {res, rescode}},

     #ejabberd_commands{name = delete_expired_messages, tags = [offline, purge],
			desc = "Delete expired offline messages from database",
			module = ?MODULE, function = delete_expired_messages,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = delete_old_messages, tags = [offline, purge],
			desc = "Delete offline messages older than DAYS",
			module = ?MODULE, function = delete_old_messages,
			args_desc = ["Number of days"],
			args_example = [31],
			args = [{days, integer}], result = {res, rescode}},
     #ejabberd_commands{name = delete_old_messages_batch, tags = [offline, purge],
			desc = "Delete offline messages older than DAYS",
			note = "added in 22.05",
			module = ?MODULE, function = delete_old_messages_batch,
			args_desc = ["Name of host where messages should be deleted",
				     "Days to keep messages",
				     "Number of messages to delete per batch",
				     "Desired rate of messages to delete per minute"],
			args_example = [<<"localhost">>, 31, 1000, 10000],
			args = [{host, binary}, {days, integer}, {batch_size, integer}, {rate, integer}],
			result = {res, restuple},
			result_desc = "Result tuple",
			result_example = {ok, <<"Removal of 5000 messages in progress">>}},
     #ejabberd_commands{name = delete_old_messages_status, tags = [offline, purge],
			desc = "Status of delete old offline messages operation",
			note = "added in 22.05",
			module = ?MODULE, function = delete_old_messages_status,
			args_desc = ["Name of host where messages should be deleted"],
			args_example = [<<"localhost">>],
			args = [{host, binary}],
			result = {status, string},
			result_desc = "Status test",
			result_example = "Operation in progress, delete 5000 messages"},
     #ejabberd_commands{name = abort_delete_old_messages, tags = [offline, purge],
			desc = "Abort currently running delete old offline messages operation",
			note = "added in 22.05",
			module = ?MODULE, function = delete_old_messages_abort,
			args_desc = ["Name of host where operation should be aborted"],
			args_example = [<<"localhost">>],
			args = [{host, binary}],
			result = {status, string},
			result_desc = "Status text",
			result_example = "Operation aborted"},

     #ejabberd_commands{name = export_db, tags = [db],
			desc = "Export database records for host to files",
			note = "added in 25.XX",
			module = ejabberd_db_serialize, function = export,
			args_desc = ["Name of host that should be exported",
				     "Directory name where exported files should be created"],
			args_example = [<<"localhost">>, <<"/home/ejabberd/export">>],
			args = [{host, binary}, {dir, binary}],
			result = {res, restuple},
			result_desc = "Result tuple",
			result_example = {ok, <<"Export started">>}},
     #ejabberd_commands{name = export_db_status, tags = [db],
			desc = "Return current status of export operation",
			note = "added in 22.XX",
			module = ejabberd_db_serialize, function = export_status,
			args_desc = ["Name of host where export is performed"],
			args_example = [<<"localhost">>],
			args = [{host, binary}],
			result = {status, string},
			result_desc = "Current operation status",
			result_example = "Operation in progress: 'Exporting mod_mam', exported 5000 records so far"},
     #ejabberd_commands{name = export_db_abort, tags = [db],
			desc = "Abort currently running export peration",
			note = "added in 22.XX",
			module = ejabberd_db_serialize, function = export_abort,
			args_desc = ["Name of host where export is performed"],
			args_example = [<<"localhost">>],
			args = [{host, binary}],
			result = {status, string},
			result_desc = "Operation status",
			result_example = "Operation aborted"},

     #ejabberd_commands{name = import_db, tags = [db],
			desc = "Import database records for host to files",
			note = "added in 25.XX",
			module = ejabberd_db_serialize, function = import,
			args_desc = ["Name of host that should be imported",
				     "Directory name where imported files should be created"],
			args_example = [<<"localhost">>, <<"/home/ejabberd/export">>],
			args = [{host, binary}, {dir, binary}],
			result = {res, restuple},
			result_desc = "Result tuple",
			result_example = {ok, <<"Import started">>}},
     #ejabberd_commands{name = import_db_status, tags = [db],
			desc = "Return current status of import operation",
			note = "added in 22.XX",
			module = ejabberd_db_serialize, function = import_status,
			args_desc = ["Name of host where import is performed"],
			args_example = [<<"localhost">>],
			args = [{host, binary}],
			result = {status, string},
			result_desc = "Current operation status",
			result_example = "Operation in progress: 'Importing mod_mam', imported 5000 records so far"},
     #ejabberd_commands{name = import_db_abort, tags = [db],
			desc = "Abort currently running import peration",
			note = "added in 22.XX",
			module = ejabberd_db_serialize, function = import_abort,
			args_desc = ["Name of host where import is performed"],
			args_example = [<<"localhost">>],
			args = [{host, binary}],
			result = {status, string},
			result_desc = "Operation status",
			result_example = "Operation aborted"},

     #ejabberd_commands{name = export2sql, tags = [mnesia],
			desc = "Export virtual host information from Mnesia tables to SQL file",
			longdesc = "Configure the modules to use SQL, then call this command. "
			           "After correctly exported the database of a vhost, "
                                   "you may want to delete from mnesia with "
			           "the _`delete_mnesia`_ API.",
			module = ejd2sql, function = export,
			args_desc = ["Vhost", "Full path to the destination SQL file"],
			args_example = ["example.com", "/var/lib/ejabberd/example.com.sql"],
			args = [{host, string}, {file, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = get_master, tags = [cluster],
			desc = "Get master node of the clustered Mnesia tables",
			note = "added in 24.06",
			longdesc = "If there is no master, returns `none`.",
			module = ?MODULE, function = get_master,
			result = {nodename, atom}},
     #ejabberd_commands{name = set_master, tags = [cluster],
			desc = "Set master node of the clustered Mnesia tables",
			longdesc = "If `nodename` is set to `self`, then this "
			"node will be set as its own master.",
			module = ?MODULE, function = set_master,
			args_desc = ["Name of the erlang node that will be considered master of this node"],
			args_example = ["ejabberd@machine7"],
			args = [{nodename, string}], result = {res, restuple}},
     #ejabberd_commands{name = mnesia_change_nodename, tags = [mnesia],
			desc = "Change the erlang node name in a backup file",
			module = ?MODULE, function = mnesia_change_nodename,
			args_desc = ["Name of the old erlang node", "Name of the new node",
				     "Path to old backup file", "Path to the new backup file"],
			args_example = ["ejabberd@machine1", "ejabberd@machine2",
					"/var/lib/ejabberd/old.backup", "/var/lib/ejabberd/new.backup"],
			args = [{oldnodename, string}, {newnodename, string},
				{oldbackup, string}, {newbackup, string}],
			result = {res, restuple}},
     #ejabberd_commands{name = backup, tags = [mnesia],
			desc = "Backup the Mnesia database to a binary file",
			module = ?MODULE, function = backup_mnesia,
			args_desc = ["Full path for the destination backup file"],
			args_example = ["/var/lib/ejabberd/database.backup"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = restore, tags = [mnesia],
			desc = "Restore the Mnesia database from a binary backup file",
			longdesc = "This restores immediately from a "
			"binary backup file the internal Mnesia "
			"database. This will consume a lot of memory if "
			"you have a large database, you may prefer "
			"_`install_fallback`_ API.",
			module = ?MODULE, function = restore_mnesia,
			args_desc = ["Full path to the backup file"],
			args_example = ["/var/lib/ejabberd/database.backup"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump, tags = [mnesia],
			desc = "Dump the Mnesia database to a text file",
			module = ?MODULE, function = dump_mnesia,
			args_desc = ["Full path for the text file"],
			args_example = ["/var/lib/ejabberd/database.txt"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump_table, tags = [mnesia],
			desc = "Dump a Mnesia table to a text file",
			module = ?MODULE, function = dump_table,
			args_desc = ["Full path for the text file", "Table name"],
			args_example = ["/var/lib/ejabberd/table-muc-registered.txt", "muc_registered"],
			args = [{file, string}, {table, string}], result = {res, restuple}},
     #ejabberd_commands{name = load, tags = [mnesia],
			desc = "Restore Mnesia database from a text dump file",
			longdesc = "Restore immediately. This is not "
			"recommended for big databases, as it will "
			"consume much time, memory and processor. In "
			"that case it's preferable to use "
			"_`backup`_ API and "
			"_`install_fallback`_ API.",
			module = ?MODULE, function = load_mnesia,
			args_desc = ["Full path to the text file"],
			args_example = ["/var/lib/ejabberd/database.txt"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = mnesia_info, tags = [mnesia],
			desc = "Dump info on global Mnesia state",
			module = ?MODULE, function = mnesia_info,
			args = [], result = {res, string}},
     #ejabberd_commands{name = mnesia_table_info, tags = [mnesia],
			desc = "Dump info on Mnesia table state",
			module = ?MODULE, function = mnesia_table_info,
			args_desc = ["Mnesia table name"],
			args_example = ["roster"],
			args = [{table, string}], result = {res, string}},
     #ejabberd_commands{name = install_fallback, tags = [mnesia],
			desc = "Install Mnesia database from a binary backup file",
			longdesc = "The binary backup file is "
			"installed as fallback: it will be used to "
			"restore the database at the next ejabberd "
			"start. This means that, after running this "
			"command, you have to restart ejabberd. This "
			"command requires less memory than "
			"_`restore`_ API.",
			module = ?MODULE, function = install_fallback_mnesia,
			args_desc = ["Full path to the fallback file"],
			args_example = ["/var/lib/ejabberd/database.fallback"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = clear_cache, tags = [server],
			desc = "Clear database cache on all nodes",
			module = ?MODULE, function = clear_cache,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = gc, tags = [server],
			desc = "Force full garbage collection",
			note = "added in 20.01",
			module = ?MODULE, function = gc,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = man, tags = [documentation],
                        desc = "Generate Unix manpage for current ejabberd version",
                        note = "added in 20.01",
                        module = ejabberd_doc, function = man,
                        args = [], result = {res, restuple}},

     #ejabberd_commands{name = webadmin_host_user_queue, tags = [offline, internal],
			desc = "Generate WebAdmin offline queue HTML",
			module = mod_offline, function = webadmin_host_user_queue,
			args = [{user, binary}, {host, binary}, {query, any}, {lang, binary}],
			result = {res, any}},

     #ejabberd_commands{name = webadmin_host_last_activity, tags = [internal],
			desc = "Generate WebAdmin Last Activity HTML",
			module = ejabberd_web_admin, function = webadmin_host_last_activity,
			args = [{host, binary}, {query, any}, {lang, binary}],
			result = {res, any}},
     #ejabberd_commands{name = webadmin_host_srg, tags = [internal],
			desc = "Generate WebAdmin Shared Roster Group HTML",
			module = mod_shared_roster, function = webadmin_host_srg,
			args = [{host, binary}, {query, any}, {lang, binary}],
			result = {res, any}},
     #ejabberd_commands{name = webadmin_host_srg_group, tags = [internal],
			desc = "Generate WebAdmin Shared Roster Group HTML for a group",
			module = mod_shared_roster, function = webadmin_host_srg_group,
			args = [{host, binary}, {group, binary}, {query, any}, {lang, binary}],
			result = {res, any}},

     #ejabberd_commands{name = webadmin_node_contrib, tags = [internal],
			desc = "Generate WebAdmin ejabberd-contrib HTML",
			module = ext_mod, function = webadmin_node_contrib,
			args = [{node, atom}, {query, any}, {lang, binary}],
			result = {res, any}},
     #ejabberd_commands{name = webadmin_node_db, tags = [internal],
			desc = "Generate WebAdmin Mnesia database HTML",
			module = ejabberd_web_admin, function = webadmin_node_db,
			args = [{node, atom}, {query, any}, {lang, binary}],
			result = {res, any}},
     #ejabberd_commands{name = webadmin_node_db_table, tags = [internal],
			desc = "Generate WebAdmin Mnesia database HTML for a table",
			module = ejabberd_web_admin, function = webadmin_node_db_table,
			args = [{node, atom}, {table, binary}, {lang, binary}],
			result = {res, any}},
     #ejabberd_commands{name = webadmin_node_db_table_page, tags = [internal],
			desc = "Generate WebAdmin Mnesia database HTML for a table content",
			module = ejabberd_web_admin, function = webadmin_node_db_table_page,
			args = [{node, atom}, {table, binary}, {page, integer}],
			result = {res, any}},

     #ejabberd_commands{name = mnesia_list_tables, tags = [mnesia],
                        desc = "List of Mnesia tables",
			note = "added in 25.03",
                        module = ?MODULE, function = mnesia_list_tables,
			result = {tables, {list, {table, {tuple, [{name, atom},
                                                                {storage_type, binary},
                                                                {elements, integer},
                                                                {memory_kb, integer},
                                                                {memory_mb, integer}
                                                               ]}}}}},
     #ejabberd_commands{name = mnesia_table_details, tags = [internal, mnesia],
                        desc = "Get details of a Mnesia table",
                        module = ?MODULE, function = mnesia_table_details,
			args = [{table, binary}],
			result = {details, {list, {detail, {tuple, [{name, atom},
                                                                {value, binary}
                                                               ]}}}}},

     #ejabberd_commands{name = mnesia_table_change_storage, tags = [mnesia],
                        desc = "Change storage type of a Mnesia table",
			note = "added in 25.03",
			longdesc = "Storage type can be: `ram_copies`, `disc_copies`, `disc_only_copies`, `remote_copy`.",
                        module = ?MODULE, function = mnesia_table_change_storage,
			args = [{table, binary}, {storage_type, binary}],
			result = {res, restuple}},
     #ejabberd_commands{name = mnesia_table_clear, tags = [internal, mnesia],
                        desc = "Delete all content in a Mnesia table",
                        module = ?MODULE, function = mnesia_table_clear,
			args = [{table, binary}],
			result = {res, restuple}},
     #ejabberd_commands{name = mnesia_table_destroy, tags = [internal, mnesia],
                        desc = "Destroy a Mnesia table",
                        module = ?MODULE, function = mnesia_table_destroy,
			args = [{table, binary}],
			result = {res, restuple}},
     #ejabberd_commands{name = echo, tags = [internal],
                        desc = "Return the same sentence that was provided",
                        module = ?MODULE, function = echo,
                        args_desc = ["Sentence to echoe"],
                        args_example = [<<"Test Sentence">>],
                        args = [{sentence, binary}],
                        result = {sentence, string},
                        result_example = "Test Sentence"},
     #ejabberd_commands{name = echo3, tags = [internal],
                        desc = "Return the same sentence that was provided",
                        module = ?MODULE, function = echo3,
                        args_desc = ["First argument", "Second argument", "Sentence to echoe"],
                        args_example = [<<"example.com">>, <<"Group1">>, <<"Test Sentence">>],
                        args = [{first, binary}, {second, binary}, {sentence, binary}],
                        result = {sentence, string},
                        result_example = "Test Sentence"}
    ].

%%%
%%% Server management
%%%

status() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    String1 = io_lib:format("The node ~p is ~p. Status: ~p",
			    [node(), InternalStatus, ProvidedStatus]),
    {Is_running, String2} =
	case lists:keysearch(ejabberd, 1, application:which_applications()) of
	    false ->
		{ejabberd_not_running, "ejabberd is not running in that node."};
	    {value, {_, _, Version}} ->
		{ok, io_lib:format("ejabberd ~s is running in that node", [Version])}
	end,
    {Is_running, String1 ++ "\n" ++String2}.

stop() ->
    _ = supervisor:terminate_child(ejabberd_sup, ejabberd_sm),
    timer:sleep(1000),
    init:stop().

restart() ->
    _ = supervisor:terminate_child(ejabberd_sup, ejabberd_sm),
    timer:sleep(1000),
    init:restart().

reopen_log() ->
    ejabberd_hooks:run(reopen_log_hook, []).

rotate_log() ->
    ejabberd_hooks:run(rotate_log_hook, []).

set_loglevel(LogLevel) ->
    try binary_to_existing_atom(iolist_to_binary(LogLevel), latin1) of
	Level ->
	    case lists:member(Level, ejabberd_logger:loglevels()) of
		true ->
		    ejabberd_logger:set(Level);
		false ->
		    {error, "Invalid log level"}
	    end
    catch _:_ ->
	    {error, "Invalid log level"}
    end.

%%%
%%% Stop Kindly
%%%

evacuate_kindly(DelaySeconds, AnnouncementTextString) ->
    perform_kindly(DelaySeconds, AnnouncementTextString, evacuate).

restart_kindly(DelaySeconds, AnnouncementTextString) ->
    perform_kindly(DelaySeconds, AnnouncementTextString, restart).

stop_kindly(DelaySeconds, AnnouncementTextString) ->
    perform_kindly(DelaySeconds, AnnouncementTextString, stop).

perform_kindly(DelaySeconds, AnnouncementTextString, Action) ->
    Subject = str:format("Server stop in ~p seconds!", [DelaySeconds]),
    WaitingDesc = str:format("Waiting ~p seconds", [DelaySeconds]),
    AnnouncementText = list_to_binary(AnnouncementTextString),
    PreSteps =
        [{"Stopping ejabberd port listeners", ejabberd_listener, stop_listeners, []},
         {"Sending announcement to connected users",
          mod_announce,
          send_announcement_to_all,
          [ejabberd_config:get_myname(), Subject, AnnouncementText]},
         {"Sending service message to MUC rooms",
          ejabberd_admin,
          send_service_message_all_mucs,
          [Subject, AnnouncementText]},
         {WaitingDesc, timer, sleep, [DelaySeconds * 1000]}
        ],
    SpecificSteps =
        case Action of
            evacuate ->
                [{"Stopping ejabberd", application, stop, [ejabberd]},
                 {"Starting ejabberd", application, start, [ejabberd]},
                 {"Stopping ejabberd port listeners", ejabberd_listener, stop_listeners, []}];
            restart ->
                [{"Restarting Erlang node", init, restart, []}];
            stop ->
                [
                 {"Stopping Erlang node", init, stop, []}]
        end,
    Steps = PreSteps ++ SpecificSteps,
    NumberLast = length(Steps),
    TimestampStart = calendar:datetime_to_gregorian_seconds({date(), time()}),
    lists:foldl(fun({Desc, Mod, Func, Args}, NumberThis) ->
                   SecondsDiff =
                       calendar:datetime_to_gregorian_seconds({date(), time()}) - TimestampStart,
                   io:format("~s[~p/~p ~ps]~s ~ts...~s ",
                             [?CLEAD ++ ?CINFO, NumberThis, NumberLast, SecondsDiff,
                              ?CMID ++ ?CINFO, Desc, ?CCLEAN]),
                   Result = (catch apply(Mod, Func, Args)),
                   io:format("~p~n", [Result]),
                   NumberThis + 1
                end,
                1,
                Steps),
    ok.

send_service_message_all_mucs(Subject, AnnouncementText) ->
    Message = str:format("~s~n~s", [Subject, AnnouncementText]),
    lists:foreach(
      fun(ServerHost) ->
	      MUCHosts = gen_mod:get_module_opt_hosts(ServerHost, mod_muc),
	      lists:foreach(
		fun(MUCHost) ->
			mod_muc:broadcast_service_message(ServerHost, MUCHost, Message)
		end, MUCHosts)
      end,
      ejabberd_option:hosts()).

%%%
%%% ejabberd_update
%%%

update_list() ->
    {ok, _Dir, UpdatedBeams, _Script, _LowLevelScript, _Check} =
	ejabberd_update:update_info(),
    [atom_to_list(Beam) || Beam <- UpdatedBeams].

update("all") ->
    ResList = [{ModStr, update_module(ModStr)} || ModStr <- update_list()],
    String = case string:join([Mod || {Mod, {ok, _}} <- ResList], ", ") of
                 [] ->
                     "No modules updated";
                 ModulesString ->
                     "Updated modules: " ++ ModulesString
             end,
    {ok, String};
update(ModStr) ->
    update_module(ModStr).

update_module(ModuleNameBin) when is_binary(ModuleNameBin) ->
    update_module(binary_to_list(ModuleNameBin));
update_module(ModuleNameString) ->
    ModuleName = list_to_atom(ModuleNameString),
    case ejabberd_update:update([ModuleName]) of
	{ok, []} ->
            {ok, "Not updated: "++ModuleNameString};
	{ok, [ModuleName]} ->
            {ok, "Updated: "++ModuleNameString};
	{error, Reason} -> {error, Reason}
    end.

update() ->
    io:format("Compiling ejabberd...~n", []),
    os:cmd("make"),
    Mods = ejabberd_admin:update_list(),
    io:format("Updating modules: ~p~n", [Mods]),
    ejabberd_admin:update("all"),
    Mods2 = Mods -- ejabberd_admin:update_list(),
    io:format("Updated modules: ~p~n", [Mods2]),
    ok.

%%%
%%% Account management
%%%

register(User, Host, Password) ->
    case is_my_host(Host) of
	true ->
	    case ejabberd_auth:try_register(User, Host, Password) of
		ok ->
		    {ok, io_lib:format("User ~s@~s successfully registered", [User, Host])};
		{error, exists} ->
		    Msg = io_lib:format("User ~s@~s already registered", [User, Host]),
		    {error, conflict, 10090, Msg};
		{error, Reason} ->
		    String = io_lib:format("Can't register user ~s@~s at node ~p: ~s",
					   [User, Host, node(),
					    mod_register:format_error(Reason)]),
		    {error, cannot_register, 10001, String}
	    end;
	false ->
	    {error, cannot_register, 10001, "Unknown virtual host"}
    end.

unregister(User, Host) ->
    case is_my_host(Host) of
	true ->
	    ejabberd_auth:remove_user(User, Host),
	    {ok, ""};
	false ->
	    {error, "Unknown virtual host"}
    end.

registered_users(Host) ->
    case is_my_host(Host) of
	true ->
	    Users = ejabberd_auth:get_users(Host),
	    SUsers = lists:sort(Users),
	    lists:map(fun({U, _S}) -> U end, SUsers);
	false ->
	    {error, "Unknown virtual host"}
    end.

registered_vhosts() ->
    ejabberd_option:hosts().

reload_config() ->
    case ejabberd_config:reload() of
	ok -> ok;
	Err ->
	    Reason = ejabberd_config:format_error(Err),
	    {error, Reason}
    end.

dump_config(Path) ->
    case ejabberd_config:dump(Path) of
	ok -> ok;
	Err ->
	    Reason = ejabberd_config:format_error(Err),
	    {error, Reason}
    end.

convert_to_yaml(In, Out) ->
    case ejabberd_config:convert_to_yaml(In, Out) of
	ok -> {ok, ""};
	Err ->
	    Reason = ejabberd_config:format_error(Err),
	    {error, Reason}
    end.

%%%
%%% Cluster management
%%%

join_cluster(NodeBin) when is_binary(NodeBin) ->
    join_cluster(list_to_atom(binary_to_list(NodeBin)));
join_cluster(Node) when is_atom(Node) ->
    IsNodes = lists:member(Node, ejabberd_cluster:get_nodes()),
    IsKnownNodes = lists:member(Node, ejabberd_cluster:get_known_nodes()),
    Ping = net_adm:ping(Node),
    join_cluster(Node, IsNodes, IsKnownNodes, Ping).

join_cluster(_Node, true, _IsKnownNodes, _Ping) ->
    {error, "This node already joined that running node."};
join_cluster(_Node, _IsNodes, true, _Ping) ->
    {error, "This node already joined that known node."};
join_cluster(_Node, _IsNodes, _IsKnownNodes, pang) ->
    {error, "This node cannot reach that node."};
join_cluster(Node, false, false, pong) ->
    case timer:apply_after(1000, ejabberd_cluster, join, [Node]) of
        {ok, _} ->
            {ok, "Trying to join that cluster, wait a few seconds and check the list of nodes."};
        Error ->
            {error, io_lib:format("Can't join that cluster: ~p", [Error])}
    end.

join_cluster_here(NodeBin) ->
    Node = list_to_atom(binary_to_list(NodeBin)),
    IsNodes = lists:member(Node, ejabberd_cluster:get_nodes()),
    IsKnownNodes = lists:member(Node, ejabberd_cluster:get_known_nodes()),
    Ping = net_adm:ping(Node),
    join_cluster_here(Node, IsNodes, IsKnownNodes, Ping).

join_cluster_here(_Node, true, _IsKnownNodes, _Ping) ->
    {error, "This node already joined that running node."};
join_cluster_here(_Node, _IsNodes, true, _Ping) ->
    {error, "This node already joined that known node."};
join_cluster_here(_Node, _IsNodes, _IsKnownNodes, pang) ->
    {error, "This node cannot reach that node."};
join_cluster_here(Node, false, false, pong) ->
    case ejabberd_cluster:call(Node, ejabberd_admin, join_cluster, [misc:atom_to_binary(node())]) of
        {ok, _} ->
            {ok, "Trying to join node to this cluster, wait a few seconds and check the list of nodes."};
        Error ->
            {error, io_lib:format("Can't join node to this cluster: ~p", [Error])}
    end.

leave_cluster(NodeBin) when is_binary(NodeBin) ->
    leave_cluster(list_to_atom(binary_to_list(NodeBin)));
leave_cluster(Node) ->
    ejabberd_cluster:leave(Node).

list_cluster() ->
    ejabberd_cluster:get_nodes().

list_cluster_detailed() ->
    KnownNodes = ejabberd_cluster:get_known_nodes(),
    RunningNodes = ejabberd_cluster:get_nodes(),
    [get_cluster_node_details(Node, RunningNodes) || Node <- KnownNodes].

get_cluster_node_details(Node, RunningNodes) ->
    get_cluster_node_details2(Node, lists:member(Node, RunningNodes)).

get_cluster_node_details2(Node, false) ->
    {Node, "false", "", -1, -1, -1, unknown};
get_cluster_node_details2(Node, true) ->
    try ejabberd_cluster:call(Node, ejabberd_admin, get_cluster_node_details3, []) of
        Result -> Result
    catch
        E:R ->
            Status = io_lib:format("~p: ~p", [E, R]),
            {Node, "true", Status, -1, -1, -1, unknown}
    end.

get_cluster_node_details3() ->
    {ok, StatusString} = status(),
    UptimeSeconds = mod_admin_extra:stats(<<"uptimeseconds">>),
    Processes = mod_admin_extra:stats(<<"processes">>),
    OnlineUsers = mod_admin_extra:stats(<<"onlineusersnode">>),
    GetMaster = get_master(),
    {node(), "true", StatusString, OnlineUsers, Processes, UptimeSeconds, GetMaster}.

%%%
%%% Migration management
%%%

import_file(Path) ->
    case jd2ejd:import_file(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't import jabberd14 spool file ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_import_file, String}
    end.

import_dir(Path) ->
    case jd2ejd:import_dir(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't import jabberd14 spool dir ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_import_dir, String}
    end.

%%%
%%% Purge DB
%%%

delete_expired_messages() ->
    lists:foreach(
      fun(Host) ->
              {atomic, ok} = mod_offline:remove_expired_messages(Host)
      end, ejabberd_option:hosts()).

delete_old_messages(Days) ->
    lists:foreach(
      fun(Host) ->
              {atomic, _} = mod_offline:remove_old_messages(Days, Host)
      end, ejabberd_option:hosts()).

delete_old_messages_batch(Server, Days, BatchSize, Rate) ->
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, mod_offline),
    case ejabberd_batch:register_task({spool, LServer}, 0, Rate, {LServer, Days, BatchSize, none},
				      fun({L, Da, B, IS} = S) ->
					  case {erlang:function_exported(Mod, remove_old_messages_batch, 3),
						erlang:function_exported(Mod, remove_old_messages_batch, 4)} of
					      {true, _} ->
						  case Mod:remove_old_messages_batch(L, Da, B) of
						      {ok, Count} ->
							  {ok, S, Count, undefined};
						      {error, _} = E ->
							  E
						  end;
					      {_, true} ->
						  case Mod:remove_old_messages_batch(L, Da, B, IS) of
						      {ok, IS2, Count, undefined} ->
							  {ok, {L, Da, B, IS2}, Count};
						      {error, _} = E ->
							  E
						  end;
					      _ ->
						  {error, not_implemented_for_backend}
					  end
				      end) of
	ok ->
	    {ok, ""};
	{error, in_progress} ->
	    {error, "Operation in progress"}
    end.

delete_old_messages_status(Server) ->
    LServer = jid:nameprep(Server),
    Msg = case ejabberd_batch:task_status({spool, LServer}) of
	      not_started ->
		  "Operation not started";
	      {failed, Steps, Error} ->
		  io_lib:format("Operation failed after deleting ~p messages with error ~p",
				[Steps, misc:format_val(Error)]);
	      {aborted, Steps, _} ->
		  io_lib:format("Operation was aborted after deleting ~p messages",
				[Steps]);
	      {working, Steps, _} ->
		  io_lib:format("Operation in progress, deleted ~p messages",
				[Steps]);
	      {completed, Steps, _} ->
		  io_lib:format("Operation was completed after deleting ~p messages",
				[Steps])
	  end,
    lists:flatten(Msg).

delete_old_messages_abort(Server) ->
    LServer = jid:nameprep(Server),
    case ejabberd_batch:abort_task({spool, LServer}) of
	aborted -> "Operation aborted";
	not_started -> "No task running"
    end.

%%%
%%% Mnesia management
%%%

get_master() ->
    case mnesia:table_info(session, master_nodes) of
    [] -> none;
    [Node] -> Node
    end.

set_master("self") ->
    set_master(node());
set_master(NodeString) when is_list(NodeString) ->
    set_master(list_to_atom(NodeString));
set_master(Node) when is_atom(Node) ->
    case mnesia:set_master_nodes([Node]) of
        ok ->
	    {ok, "ok"};
	{error, Reason} ->
	    String = io_lib:format("Can't set master node ~p at node ~p:~n~p",
				   [Node, node(), Reason]),
	    {error, String}
    end.

backup_mnesia(Path) ->
    case mnesia:backup(Path) of
        ok ->
	    {ok, ""};
	{error, Reason} ->
	    String = io_lib:format("Can't store backup in ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_backup, String}
    end.

restore_mnesia(Path) ->
    case ejabberd_admin:restore(Path) of
	{atomic, _} ->
	    {ok, ""};
	{aborted,{no_exists,Table}} ->
	    String = io_lib:format("Can't restore backup from ~p at node ~p: Table ~p does not exist.",
				   [filename:absname(Path), node(), Table]),
	    {table_not_exists, String};
	{aborted,enoent} ->
	    String = io_lib:format("Can't restore backup from ~p at node ~p: File not found.",
				   [filename:absname(Path), node()]),
	    {file_not_found, String}
    end.

%% Mnesia database restore
%% This function is called from ejabberd_ctl, ejabberd_web_admin and
%% mod_configure/adhoc
restore(Path) ->
    mnesia:restore(Path, [{keep_tables,keep_tables()},
			  {default_op, skip_tables}]).

%% This function return a list of tables that should be kept from a previous
%% version backup.
%% Obsolete tables or tables created by module who are no longer used are not
%% restored and are ignored.
keep_tables() ->
    lists:flatten([acl, passwd, config,
		   keep_modules_tables()]).

%% Returns the list of modules tables in use, according to the list of actually
%% loaded modules
keep_modules_tables() ->
    lists:map(fun(Module) -> module_tables(Module) end,
	      gen_mod:loaded_modules(ejabberd_config:get_myname())).

%% TODO: This mapping should probably be moved to a callback function in each
%% module.
%% Mapping between modules and their tables
module_tables(mod_announce) -> [motd, motd_users];
module_tables(mod_last) -> [last_activity];
module_tables(mod_muc) -> [muc_room, muc_registered];
module_tables(mod_offline) -> [offline_msg];
module_tables(mod_privacy) -> [privacy];
module_tables(mod_private) -> [private_storage];
module_tables(mod_pubsub) -> [pubsub_node];
module_tables(mod_roster) -> [roster];
module_tables(mod_shared_roster) -> [sr_group, sr_user];
module_tables(mod_vcard) -> [vcard, vcard_search];
module_tables(_Other) -> [].

get_local_tables() ->
    Tabs1 = lists:delete(schema, mnesia:system_info(local_tables)),
    Tabs = lists:filter(
	     fun(T) ->
		     case mnesia:table_info(T, storage_type) of
			 disc_copies -> true;
			 disc_only_copies -> true;
			 _ -> false
		     end
	     end, Tabs1),
    Tabs.

dump_mnesia(Path) ->
    Tabs = get_local_tables(),
    dump_tables(Path, Tabs).

dump_table(Path, STable) ->
    Table = list_to_atom(STable),
    dump_tables(Path, [Table]).

dump_tables(Path, Tables) ->
    case dump_to_textfile(Path, Tables) of
	ok ->
	    {ok, ""};
	{error, Reason} ->
            String = io_lib:format("Can't store dump in ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_dump, String}
    end.

dump_to_textfile(File) ->
    Tabs = get_local_tables(),
    dump_to_textfile(File, Tabs).

dump_to_textfile(File, Tabs) ->
    dump_to_textfile(mnesia:system_info(is_running), Tabs, file:open(File, [write])).
dump_to_textfile(yes, Tabs, {ok, F}) ->
    Defs = lists:map(
	     fun(T) -> {T, [{record_name, mnesia:table_info(T, record_name)},
			    {attributes, mnesia:table_info(T, attributes)}]}
	     end,
	     Tabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
    file:close(F);
dump_to_textfile(_, _, {ok, F}) ->
    file:close(F),
    {error, mnesia_not_running};
dump_to_textfile(_, _, {error, Reason}) ->
    {error, Reason}.

dump_tab(F, T) ->
    W = mnesia:table_info(T, wild_pattern),
    {atomic,All} = mnesia:transaction(
		     fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(
      fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).

load_mnesia(Path) ->
    case mnesia:load_textfile(Path) of
        {atomic, ok} ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't load dump in ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_load, String}
    end.

mnesia_info() ->
    lists:flatten(io_lib:format("~p", [mnesia:system_info(all)])).

mnesia_table_info(Table) ->
    ATable = list_to_atom(Table),
    lists:flatten(io_lib:format("~p", [mnesia:table_info(ATable, all)])).

install_fallback_mnesia(Path) ->
    case mnesia:install_fallback(Path) of
	ok ->
	    {ok, ""};
	{error, Reason} ->
	    String = io_lib:format("Can't install fallback from ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_fallback, String}
    end.

mnesia_change_nodename(FromString, ToString, Source, Target) ->
    From = list_to_atom(FromString),
    To = list_to_atom(ToString),
    Switch =
	fun
	    (Node) when Node == From ->
		io:format("     - Replacing nodename: '~p' with: '~p'~n", [From, To]),
		To;
	    (Node) when Node == To ->
		%% throw({error, already_exists});
		io:format("     - Node: '~p' will not be modified (it is already '~p')~n", [Node, To]),
		Node;
	    (Node) ->
		io:format("     - Node: '~p' will not be modified (it is not '~p')~n", [Node, From]),
		Node
	end,
    Convert =
	fun
	    ({schema, db_nodes, Nodes}, Acc) ->
		io:format(" +++ db_nodes ~p~n", [Nodes]),
		{[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
	    ({schema, version, Version}, Acc) ->
		io:format(" +++ version: ~p~n", [Version]),
		{[{schema, version, Version}], Acc};
	    ({schema, cookie, Cookie}, Acc) ->
		io:format(" +++ cookie: ~p~n", [Cookie]),
		{[{schema, cookie, Cookie}], Acc};
	    ({schema, Tab, CreateList}, Acc) ->
		io:format("~n * Checking table: '~p'~n", [Tab]),
		Keys = [ram_copies, disc_copies, disc_only_copies],
		OptSwitch =
		    fun({Key, Val}) ->
			    case lists:member(Key, Keys) of
				true ->
				    io:format("   + Checking key: '~p'~n", [Key]),
				    {Key, lists:map(Switch, Val)};
				false-> {Key, Val}
			    end
		    end,
		Res = {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc},
		Res;
	    (Other, Acc) ->
		{[Other], Acc}
	end,
    mnesia:traverse_backup(Source, Target, Convert, switched).

clear_cache() ->
    Nodes = ejabberd_cluster:get_nodes(),
    lists:foreach(fun(T) -> ets_cache:clear(T, Nodes) end, ets_cache:all()).

gc() ->
    lists:foreach(fun erlang:garbage_collect/1, processes()).

-spec is_my_host(binary()) -> boolean().
is_my_host(Host) ->
    try ejabberd_router:is_my_host(Host)
    catch _:{invalid_domain, _} -> false
    end.

%%%
%%% Internal
%%%

%% @format-begin

mnesia_table_change_storage(STable, SType) ->
    Table = binary_to_existing_atom(STable, latin1),
    Type =
        case SType of
            <<"remote_copy">> ->
                remote_copy;
            <<"ram_copies">> ->
                ram_copies;
            <<"disc_copies">> ->
                disc_copies;
            <<"disc_only_copies">> ->
                disc_only_copies;
            _ ->
                false
        end,
    Node = node(),
    Result =
        case Type of
            false ->
                "Nothing to do";
            remote_copy ->
                mnesia:del_table_copy(Table, Node),
                "Deleted table copy";
            _ ->
                case mnesia:add_table_copy(Table, Node, Type) of
                    {aborted, _} ->
                        mnesia:change_table_copy_type(Table, Node, Type),
                        "Changed table copy type";
                    _ ->
                        "Added table copy"
                end
        end,
    {ok, Result}.

mnesia_table_clear(STable) ->
    Table = binary_to_existing_atom(STable, latin1),
    mnesia:clear_table(Table).

mnesia_table_delete(STable) ->
    Table = binary_to_existing_atom(STable, latin1),
    mnesia:delete_table(Table).

mnesia_table_details(STable) ->
    Table = binary_to_existing_atom(STable, latin1),
    [{Name, iolist_to_binary(str:format("~p", [Value]))}
     || {Name, Value} <- mnesia:table_info(Table, all)].

mnesia_list_tables() ->
    STables =
        lists:sort(
            mnesia:system_info(tables)),
    lists:map(fun(Table) ->
                 TInfo = mnesia:table_info(Table, all),
                 {value, {storage_type, Type}} = lists:keysearch(storage_type, 1, TInfo),
                 {value, {size, Size}} = lists:keysearch(size, 1, TInfo),
                 {value, {memory, Memory}} = lists:keysearch(memory, 1, TInfo),
                 MemoryB = Memory * erlang:system_info(wordsize),
                 MemoryKB = MemoryB div 1024,
                 MemoryMB = MemoryKB div 1024,
                 {Table, storage_type_bin(Type), Size, MemoryKB, MemoryMB}
              end,
              STables).

storage_type_bin(ram_copies) ->
    <<"RAM copy">>;
storage_type_bin(disc_copies) ->
    <<"RAM and disc copy">>;
storage_type_bin(disc_only_copies) ->
    <<"Disc only copy">>;
storage_type_bin(unknown) ->
    <<"Remote copy">>.

echo(Sentence) ->
    Sentence.

echo3(_, _, Sentence) ->
    Sentence.

%%%
%%% Web Admin: Main
%%%

web_menu_main(Acc, _Lang) ->
    Acc ++ [{<<"purge">>, <<"Purge">>}, {<<"stanza">>, <<"Stanza">>}].

web_page_main(_, #request{path = [<<"purge">>]} = R) ->
    Types =
        [{<<"#erlang">>, <<"Erlang">>},
         {<<"#users">>, <<"Users">>},
         {<<"#offline">>, <<"Offline">>},
         {<<"#mam">>, <<"MAM">>},
         {<<"#pubsub">>, <<"PubSub">>},
         {<<"#push">>, <<"Push">>}],
    Head = [?XC(<<"h1">>, <<"Purge">>)],
    Set = [?XE(<<"ul">>, [?LI([?AC(MIU, MIN)]) || {MIU, MIN} <- Types]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"erlang">>}], <<"Erlang">>),
           ?XE(<<"blockquote">>,
               [ejabberd_web_admin:make_command(clear_cache, R),
                ejabberd_web_admin:make_command(gc, R)]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"users">>}], <<"Users">>),
           ?XE(<<"blockquote">>, [ejabberd_web_admin:make_command(delete_old_users, R)]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"offline">>}], <<"Offline">>),
           ?XE(<<"blockquote">>,
               [ejabberd_web_admin:make_command(delete_expired_messages, R),
                ejabberd_web_admin:make_command(delete_old_messages, R),
                ejabberd_web_admin:make_command(delete_old_messages_batch, R),
                ejabberd_web_admin:make_command(delete_old_messages_status, R)]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"mam">>}], <<"MAM">>),
           ?XE(<<"blockquote">>,
               [ejabberd_web_admin:make_command(delete_old_mam_messages, R),
                ejabberd_web_admin:make_command(delete_old_mam_messages_batch, R),
                ejabberd_web_admin:make_command(delete_old_mam_messages_status, R)]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"pubsub">>}], <<"PubSub">>),
           ?XE(<<"blockquote">>,
               [ejabberd_web_admin:make_command(delete_expired_pubsub_items, R),
                ejabberd_web_admin:make_command(delete_old_pubsub_items, R)]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"push">>}], <<"Push">>),
           ?XE(<<"blockquote">>, [ejabberd_web_admin:make_command(delete_old_push_sessions, R)])],
    {stop, Head ++ Set};
web_page_main(_, #request{path = [<<"stanza">>]} = R) ->
    Head = [?XC(<<"h1">>, <<"Stanza">>)],
    Set = [ejabberd_web_admin:make_command(send_message, R),
           ejabberd_web_admin:make_command(send_stanza, R),
           ejabberd_web_admin:make_command(send_stanza_c2s, R)],
    {stop, Head ++ Set};
web_page_main(Acc, _) ->
    Acc.

%%%
%%% Web Admin: Node
%%%

web_menu_node(Acc, _Node, _Lang) ->
    Acc
    ++ [{<<"cluster">>, <<"Clustering">>},
        {<<"update">>, <<"Code Update">>},
        {<<"config-file">>, <<"Configuration File">>},
        {<<"logs">>, <<"Logs">>},
        {<<"stop">>, <<"Stop Node">>}].

web_page_node(_, Node, #request{path = [<<"cluster">>]} = R) ->
    {ok, Names} = net_adm:names(),
    NodeNames = lists:join(", ", [Name || {Name, _Port} <- Names]),
    Hint =
        list_to_binary(io_lib:format("Hint: Erlang nodes found in this machine that may be running ejabberd: ~s",
                                     [NodeNames])),
    Head = ?H1GLraw(<<"Clustering">>, <<"admin/guide/clustering/">>, <<"Clustering">>),
    Set1 =
        [ejabberd_cluster:call(Node,
                               ejabberd_web_admin,
                               make_command,
                               [join_cluster_here, R, [], []]),
         ?XE(<<"blockquote">>, [?C(Hint)]),
         ejabberd_cluster:call(Node,
                               ejabberd_web_admin,
                               make_command,
                               [join_cluster, R, [], [{style, danger}]]),
         ?XE(<<"blockquote">>, [?C(Hint)]),
         ejabberd_cluster:call(Node,
                               ejabberd_web_admin,
                               make_command,
                               [leave_cluster, R, [], [{style, danger}]])],
    Set2 =
        [ejabberd_cluster:call(Node,
                               ejabberd_web_admin,
                               make_command,
                               [set_master, R, [], [{style, danger}]])],
    timer:sleep(100), % leaving a cluster takes a while, let's delay the get commands
    Get1 =
        [ejabberd_cluster:call(Node,
                               ejabberd_web_admin,
                               make_command,
                               [list_cluster_detailed,
                                R,
                                [],
                                [{result_links, [{name, node, 3, <<"">>}]}]])],
    Get2 =
        [ejabberd_cluster:call(Node,
                               ejabberd_web_admin,
                               make_command,
                               [get_master,
                                R,
                                [],
                                [{result_named, true},
                                 {result_links, [{nodename, node, 3, <<"">>}]}]])],
    {stop, Head ++ Get1 ++ Set1 ++ Get2 ++ Set2};
web_page_node(_, Node, #request{path = [<<"update">>]} = R) ->
    Head = [?XC(<<"h1">>, <<"Code Update">>)],
    Set = [ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [update, R])],
    Get = [ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [update_list, R])],
    {stop, Head ++ Get ++ Set};
web_page_node(_, Node, #request{path = [<<"config-file">>]} = R) ->
    Res = ?H1GLraw(<<"Configuration File">>,
                   <<"admin/configuration/file-format/">>,
                   <<"File Format">>)
          ++ [ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [convert_to_yaml, R]),
              ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [dump_config, R]),
              ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [reload_config, R])],
    {stop, Res};
web_page_node(_, Node, #request{path = [<<"stop">>]} = R) ->
    Res = [?XC(<<"h1">>, <<"Stop This Node">>),
           ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [restart, R, [], [{style, danger}]]),
           ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [stop_kindly, R, [], [{style, danger}]]),
           ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [stop, R, [], [{style, danger}]]),
           ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [halt, R, [], [{style, danger}]])],
    {stop, Res};
web_page_node(_, Node, #request{path = [<<"logs">>]} = R) ->
    Res = ?H1GLraw(<<"Logs">>, <<"admin/configuration/basic/#logging">>, <<"Logging">>)
          ++ [ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [set_loglevel, R]),
              ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [get_loglevel, R]),
              ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [reopen_log, R]),
              ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [rotate_log, R])],
    {stop, Res};
web_page_node(Acc, _, _) ->
    Acc.
