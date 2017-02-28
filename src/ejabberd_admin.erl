%%%-------------------------------------------------------------------
%%% File    : ejabberd_admin.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Administrative functions and commands
%%% Created :  7 May 2006 by Mickael Remond <mremond@process-one.net>
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

-module(ejabberd_admin).
-author('mickael.remond@process-one.net').

-behaviour(gen_server).

-export([start_link/0,
	 %% Server
	 status/0, reopen_log/0, rotate_log/0,
	 set_loglevel/1,
	 stop_kindly/2, send_service_message_all_mucs/2,
	 registered_vhosts/0,
	 reload_config/0,
	 %% Cluster
	 join_cluster/1, leave_cluster/1, list_cluster/0,
	 %% Erlang
	 update_list/0, update/1,
	 %% Accounts
	 register/3, unregister/2,
	 registered_users/1,
	 %% Migration jabberd1.4
	 import_file/1, import_dir/1,
	 %% Purge DB
	 delete_expired_messages/0, delete_old_messages/1,
	 %% Mnesia
	 set_master/1,
	 backup_mnesia/1, restore_mnesia/1,
	 dump_mnesia/1, dump_table/2, load_mnesia/1,
	 install_fallback_mnesia/1,
	 dump_to_textfile/1, dump_to_textfile/2,
	 mnesia_change_nodename/4,
	 restore/1, % Still used by some modules
	 get_commands_spec/0
	]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_commands.hrl").

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    ejabberd_commands:register_commands(get_commands_spec()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
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
			module = init, function = stop,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = restart, tags = [server],
			desc = "Restart ejabberd gracefully",
			module = init, function = restart,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = reopen_log, tags = [logs, server],
			desc = "Reopen the log files",
			policy = admin,
			module = ?MODULE, function = reopen_log,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = rotate_log, tags = [logs, server],
			desc = "Rotate the log files",
			module = ?MODULE, function = rotate_log,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = stop_kindly, tags = [server],
			desc = "Inform users and rooms, wait, and stop the server",
			longdesc = "Provide the delay in seconds, and the "
			    "announcement quoted, for example: \n"
			    "ejabberdctl stop_kindly 60 "
			    "\\\"The server will stop in one minute.\\\"",
			module = ?MODULE, function = stop_kindly,
			args_desc = ["Seconds to wait", "Announcement to send, with quotes"],
			args_example = [60, <<"Server will stop now.">>],
			args = [{delay, integer}, {announcement, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = get_loglevel, tags = [logs, server],
			desc = "Get the current loglevel",
			module = ejabberd_logger, function = get,
			result_desc = "Tuple with the log level number, its keyword and description",
			result_example = {4, <<"info">>, <<"Info">>},
			args = [],
                        result = {leveltuple, {tuple, [{levelnumber, integer},
                                                       {levelatom, atom},
                                                       {leveldesc, string}
                                                      ]}}},
     #ejabberd_commands{name = set_loglevel, tags = [logs, server],
			desc = "Set the loglevel (0 to 5)",
			module = ?MODULE, function = set_loglevel,
			args_desc = ["Integer of the desired logging level, between 1 and 5"],
			args_example = [5],
			result_desc = "The type of logger module used",
			result_example = lager,
			args = [{loglevel, integer}],
			result = {logger, atom}},

     #ejabberd_commands{name = update_list, tags = [server],
			desc = "List modified modules that can be updated",
			module = ?MODULE, function = update_list,
			args = [],
			result_example = ["mod_configure", "mod_vcard"],
			result = {modules, {list, {module, string}}}},
     #ejabberd_commands{name = update, tags = [server],
			desc = "Update the given module, or use the keyword: all",
			module = ?MODULE, function = update,
			args_example = ["mod_vcard"],
			args = [{module, string}],
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
     #ejabberd_commands{name = reload_config, tags = [server],
			desc = "Reload config file in memory",
			module = ?MODULE, function = reload_config,
			args = [],
			result = {res, rescode}},

     #ejabberd_commands{name = join_cluster, tags = [cluster],
			desc = "Join this node into the cluster handled by Node",
			module = ?MODULE, function = join_cluster,
			args_desc = ["Nodename of the node to join"],
			args_example = [<<"ejabberd1@machine7">>],
			args = [{node, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = leave_cluster, tags = [cluster],
			desc = "Remove and shutdown Node from the running cluster",
			longdesc = "This command can be run from any running node of the cluster, "
			    "even the node to be removed.",
			module = ?MODULE, function = leave_cluster,
			args_desc = ["Nodename of the node to kick from the cluster"],
			args_example = [<<"ejabberd1@machine8">>],
			args = [{node, binary}],
			result = {res, rescode}},

     #ejabberd_commands{name = list_cluster, tags = [cluster],
			desc = "List nodes that are part of the cluster handled by Node",
			module = ?MODULE, function = list_cluster,
			result_example = [ejabberd1@machine7, ejabberd1@machine8],
			args = [],
			result = {nodes, {list, {node, atom}}}},

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
			args = [{file, string}], result = {res, rescode}},
     #ejabberd_commands{name = export_piefxis, tags = [mnesia],
			desc = "Export data of all users in the server to PIEFXIS files (XEP-0227)",
			module = ejabberd_piefxis, function = export_server,
			args_desc = ["Full path to a directory"],
			args_example = ["/var/lib/ejabberd/"],
			args = [{dir, string}], result = {res, rescode}},
     #ejabberd_commands{name = export_piefxis_host, tags = [mnesia],
			desc = "Export data of users in a host to PIEFXIS files (XEP-0227)",
			module = ejabberd_piefxis, function = export_host,
			args_desc = ["Full path to a directory", "Vhost to export"],
			args_example = ["/var/lib/ejabberd/", "example.com"],
			args = [{dir, string}, {host, string}], result = {res, rescode}},

     #ejabberd_commands{name = delete_mnesia, tags = [mnesia, sql],
                        desc = "Delete elements in Mnesia database for a given vhost",
                        module = ejd2sql, function = delete,
			args_desc = ["Vhost which content will be deleted in Mnesia database"],
			args_example = ["example.com"],
                        args = [{host, string}], result = {res, rescode}},
     #ejabberd_commands{name = convert_to_scram, tags = [sql],
			desc = "Convert the passwords in 'users' ODBC table to SCRAM",
			module = ejabberd_auth_sql, function = convert_to_scram,
			args_desc = ["Vhost which users' passwords will be scrammed"],
			args_example = ["example.com"],
			args = [{host, binary}], result = {res, rescode}},

     #ejabberd_commands{name = import_prosody, tags = [mnesia, sql, riak],
			desc = "Import data from Prosody",
			module = prosody2ejabberd, function = from_dir,
			args_desc = ["Full path to the Prosody data directory"],
			args_example = ["/var/lib/prosody/datadump/"],
			args = [{dir, string}], result = {res, rescode}},

     #ejabberd_commands{name = convert_to_yaml, tags = [config],
                        desc = "Convert the input file from Erlang to YAML format",
                        module = ejabberd_config, function = convert_to_yaml,
			args_desc = ["Full path to the original configuration file", "And full path to final file"],
			args_example = ["/etc/ejabberd/ejabberd.cfg", "/etc/ejabberd/ejabberd.yml"],
                        args = [{in, string}, {out, string}],
                        result = {res, rescode}},

     #ejabberd_commands{name = delete_expired_messages, tags = [purge],
			desc = "Delete expired offline messages from database",
			module = ?MODULE, function = delete_expired_messages,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = delete_old_messages, tags = [purge],
			desc = "Delete offline messages older than DAYS",
			module = ?MODULE, function = delete_old_messages,
			args_desc = ["Number of days"],
			args_example = [31],
			args = [{days, integer}], result = {res, rescode}},

     #ejabberd_commands{name = export2sql, tags = [mnesia],
			desc = "Export virtual host information from Mnesia tables to SQL file",
			module = ejd2sql, function = export,
			args_desc = ["Vhost", "Full path to the destination SQL file"],
			args_example = ["example.com", "/var/lib/ejabberd/example.com.sql"],
			args = [{host, string}, {file, string}],
			result = {res, rescode}},
     #ejabberd_commands{name = set_master, tags = [mnesia],
			desc = "Set master node of the clustered Mnesia tables",
			longdesc = "If you provide as nodename \"self\", this "
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
			desc = "Store the database to backup file",
			module = ?MODULE, function = backup_mnesia,
			args_desc = ["Full path for the destination backup file"],
			args_example = ["/var/lib/ejabberd/database.backup"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = restore, tags = [mnesia],
			desc = "Restore the database from backup file",
			module = ?MODULE, function = restore_mnesia,
			args_desc = ["Full path to the backup file"],
			args_example = ["/var/lib/ejabberd/database.backup"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump, tags = [mnesia],
			desc = "Dump the database to a text file",
			module = ?MODULE, function = dump_mnesia,
			args_desc = ["Full path for the text file"],
			args_example = ["/var/lib/ejabberd/database.txt"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump_table, tags = [mnesia],
			desc = "Dump a table to a text file",
			module = ?MODULE, function = dump_table,
			args_desc = ["Full path for the text file", "Table name"],
			args_example = ["/var/lib/ejabberd/table-muc-registered.txt", "muc_registered"],
			args = [{file, string}, {table, string}], result = {res, restuple}},
     #ejabberd_commands{name = load, tags = [mnesia],
			desc = "Restore the database from a text file",
			module = ?MODULE, function = load_mnesia,
			args_desc = ["Full path to the text file"],
			args_example = ["/var/lib/ejabberd/database.txt"],
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = install_fallback, tags = [mnesia],
			desc = "Install the database from a fallback file",
			module = ?MODULE, function = install_fallback_mnesia,
			args_desc = ["Full path to the fallback file"],
			args_example = ["/var/lib/ejabberd/database.fallback"],
			args = [{file, string}], result = {res, restuple}}
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
    {Is_running, String1 ++ String2}.

reopen_log() ->
    ejabberd_hooks:run(reopen_log_hook, []),
    ejabberd_logger:reopen_log().

rotate_log() ->
    ejabberd_hooks:run(rotate_log_hook, []),
    ejabberd_logger:rotate_log().

set_loglevel(LogLevel) ->
    {module, Module} = ejabberd_logger:set(LogLevel),
    Module.


%%%
%%% Stop Kindly
%%%

stop_kindly(DelaySeconds, AnnouncementTextString) ->
    Subject = (str:format("Server stop in ~p seconds!", [DelaySeconds])),
    WaitingDesc = (str:format("Waiting ~p seconds", [DelaySeconds])),
    AnnouncementText = list_to_binary(AnnouncementTextString),
    Steps = [
	     {"Stopping ejabberd port listeners",
	      ejabberd_listener, stop_listeners, []},
	     {"Sending announcement to connected users",
	      mod_announce, send_announcement_to_all,
	      [?MYNAME, Subject, AnnouncementText]},
	     {"Sending service message to MUC rooms",
	      ejabberd_admin, send_service_message_all_mucs,
	      [Subject, AnnouncementText]},
	     {WaitingDesc, timer, sleep, [DelaySeconds * 1000]},
	     {"Stopping ejabberd", application, stop, [ejabberd]},
	     {"Stopping Mnesia", mnesia, stop, []},
	     {"Stopping Erlang node", init, stop, []}
    ],
    NumberLast = length(Steps),
    TimestampStart = calendar:datetime_to_gregorian_seconds({date(), time()}),
    lists:foldl(
      fun({Desc, Mod, Func, Args}, NumberThis) ->
	      SecondsDiff =
		  calendar:datetime_to_gregorian_seconds({date(), time()})
		  - TimestampStart,
	      io:format("[~p/~p ~ps] ~s... ",
			[NumberThis, NumberLast, SecondsDiff, Desc]),
	      Result = (catch apply(Mod, Func, Args)),
	      io:format("~p~n", [Result]),
	      NumberThis+1
      end,
      1,
      Steps),
    ok.

send_service_message_all_mucs(Subject, AnnouncementText) ->
    Message = str:format("~s~n~s", [Subject, AnnouncementText]),
    lists:foreach(
      fun(ServerHost) ->
	      MUCHost = gen_mod:get_module_opt_host(
			  ServerHost, mod_muc, <<"conference.@HOST@">>),
	      mod_muc:broadcast_service_message(ServerHost, MUCHost, Message)
      end,
      ?MYHOSTS).

%%%
%%% ejabberd_update
%%%

update_list() ->
    {ok, _Dir, UpdatedBeams, _Script, _LowLevelScript, _Check} =
	ejabberd_update:update_info(),
    [atom_to_list(Beam) || Beam <- UpdatedBeams].

update("all") ->
    [update_module(ModStr) || ModStr <- update_list()],
    {ok, []};
update(ModStr) ->
    update_module(ModStr).

update_module(ModuleNameBin) when is_binary(ModuleNameBin) ->
    update_module(binary_to_list(ModuleNameBin));
update_module(ModuleNameString) ->
    ModuleName = list_to_atom(ModuleNameString),
    case ejabberd_update:update([ModuleName]) of
          {ok, _Res} -> {ok, []};
          {error, Reason} -> {error, Reason}
    end.

%%%
%%% Account management
%%%

register(User, Host, Password) ->
    case ejabberd_auth:try_register(User, Host, Password) of
	{atomic, ok} ->
	    {ok, io_lib:format("User ~s@~s successfully registered", [User, Host])};
	{atomic, exists} ->
	    Msg = io_lib:format("User ~s@~s already registered", [User, Host]),
	    {error, conflict, 10090, Msg};
	{error, Reason} ->
	    String = io_lib:format("Can't register user ~s@~s at node ~p: ~p",
				   [User, Host, node(), Reason]),
	    {error, cannot_register, 10001, String}
    end.

unregister(User, Host) ->
    ejabberd_auth:remove_user(User, Host),
    {ok, ""}.

registered_users(Host) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort(Users),
    lists:map(fun({U, _S}) -> U end, SUsers).

registered_vhosts() ->
	?MYHOSTS.

reload_config() ->
    ejabberd_config:reload_file().

%%%
%%% Cluster management
%%%

join_cluster(NodeBin) ->
    ejabberd_cluster:join(list_to_atom(binary_to_list(NodeBin))).

leave_cluster(NodeBin) ->
    ejabberd_cluster:leave(list_to_atom(binary_to_list(NodeBin))).

list_cluster() ->
    ejabberd_cluster:get_nodes().

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
      end, ?MYHOSTS).

delete_old_messages(Days) ->
    lists:foreach(
      fun(Host) ->
              {atomic, _} = mod_offline:remove_old_messages(Days, Host)
      end, ?MYHOSTS).

%%%
%%% Mnesia management
%%%

set_master("self") ->
    set_master(node());
set_master(NodeString) when is_list(NodeString) ->
    set_master(list_to_atom(NodeString));
set_master(Node) when is_atom(Node) ->
    case mnesia:set_master_nodes([Node]) of
        ok ->
	    {ok, ""};
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
	{error, Reason} ->
	    String = io_lib:format("Can't restore backup from ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_restore, String};
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
    lists:flatten([acl, passwd, config, local_config,
		   keep_modules_tables()]).

%% Returns the list of modules tables in use, according to the list of actually
%% loaded modules
keep_modules_tables() ->
    lists:map(fun(Module) -> module_tables(Module) end,
	      gen_mod:loaded_modules(?MYNAME)).

%% TODO: This mapping should probably be moved to a callback function in each
%% module.
%% Mapping between modules and their tables
module_tables(mod_announce) -> [motd, motd_users];
module_tables(mod_irc) -> [irc_custom];
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
