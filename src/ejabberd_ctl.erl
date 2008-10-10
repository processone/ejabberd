%%%----------------------------------------------------------------------
%%% File    : ejabberd_ctl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Ejabberd admin tool
%%% Created : 11 Jan 2004 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_ctl).
-author('alexey@process-one.net').

-export([start/0,
	 init/0,
	 process/1,
	 dump_to_textfile/1,
	 register_commands/3,
	 register_commands/4,
	 unregister_commands/3,
	 unregister_commands/4]).

-include("ejabberd_ctl.hrl").
-include("ejabberd.hrl").

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    SNode1 = case string:tokens(SNode, "@") of
		[_Node, _Server] ->
		    SNode;
		_ ->
		    case net_kernel:longnames() of
			 true ->
			     SNode ++ "@" ++ inet_db:gethostname() ++
				      "." ++ inet_db:res_option(domain);
			 false ->
			     SNode ++ "@" ++ inet_db:gethostname();
			 _ ->
			     SNode
		     end
	    end,
	    Node = list_to_atom(SNode1),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     ?PRINT("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.

init() ->
    ets:new(ejabberd_ctl_cmds, [named_table, set, public]),
    ets:new(ejabberd_ctl_host_cmds, [named_table, set, public]).


process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p. Status: ~p~n",
              [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(ejabberd, 1, application:which_applications()) of
        false ->
            ?PRINT("ejabberd is not running~n", []),
            ?STATUS_ERROR;
        {value,_Version} ->
            ?PRINT("ejabberd is running~n", []),
            ?STATUS_SUCCESS
    end;

process(["stop"]) ->
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(["reopen-log"]) ->
    ejabberd_hooks:run(reopen_log_hook, []),
    lists:foreach(fun(Host) ->
			  ejabberd_hooks:run(reopen_log_hook, Host, [Host])
		  end, ?MYHOSTS),
    %% TODO: Use the Reopen log API for logger_h ?
    ejabberd_logger_h:reopen_log(),
    ?STATUS_SUCCESS;

process(["register", User, Server, Password]) ->
    case ejabberd_auth:try_register(User, Server, Password) of
	{atomic, ok} ->
	    ?STATUS_SUCCESS;
	{atomic, exists} ->
	    ?PRINT("User ~p already registered at node ~p~n",
		      [User ++ "@" ++ Server, node()]),
	    ?STATUS_ERROR;
	{error, Reason} ->
	    ?PRINT("Can't register user ~p at node ~p: ~p~n",
		      [User ++ "@" ++ Server, node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["unregister", User, Server]) ->
    case ejabberd_auth:remove_user(User, Server) of
	{error, Reason} ->
	    ?PRINT("Can't unregister user ~p at node ~p: ~p~n",
		      [User ++ "@" ++ Server, node(), Reason]),
	    ?STATUS_ERROR;
	_ ->
	    ?STATUS_SUCCESS
    end;

process(["backup", Path]) ->
    case mnesia:backup(Path) of
        ok ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    ?PRINT("Can't store backup in ~p at node ~p: ~p~n",
		      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["dump", Path]) ->
    case dump_to_textfile(Path) of
	ok ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
            ?PRINT("Can't store dump in ~p at node ~p: ~p~n",
                      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["load", Path]) ->
    case mnesia:load_textfile(Path) of
        {atomic, ok} ->
            ?STATUS_SUCCESS;
        {error, Reason} ->
            ?PRINT("Can't load dump in ~p at node ~p: ~p~n",
                      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["restore", Path]) ->
    case ejabberd_admin:restore(Path) of
	{atomic, _} ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    ?PRINT("Can't restore backup from ~p at node ~p: ~p~n",
		      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR;
	{aborted,{no_exists,Table}} ->
	    ?PRINT("Can't restore backup from ~p at node ~p: Table ~p does not exist.~n",
		      [filename:absname(Path), node(), Table]),
	    ?STATUS_ERROR;
	{aborted,enoent} ->
	    ?PRINT("Can't restore backup from ~p at node ~p: File not found.~n",
		      [filename:absname(Path), node()]),
	    ?STATUS_ERROR
    end;

process(["install-fallback", Path]) ->
    case mnesia:install_fallback(Path) of
	ok ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    ?PRINT("Can't install fallback from ~p at node ~p: ~p~n",
		      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["import-file", Path]) ->
    case jd2ejd:import_file(Path) of
        ok ->
            ?STATUS_SUCCESS;
        {error, Reason} ->
            ?PRINT("Can't import jabberd 1.4 spool file ~p at node ~p: ~p~n",
                      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["import-dir", Path]) ->
    case jd2ejd:import_dir(Path) of
        ok ->
            ?STATUS_SUCCESS;
        {error, Reason} ->
            ?PRINT("Can't import jabberd 1.4 spool dir ~p at node ~p: ~p~n",
                      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["delete-expired-messages"]) ->
    mod_offline:remove_expired_messages(),
    ?STATUS_SUCCESS;

process(["mnesia"]) ->
    ?PRINT("~p~n", [mnesia:system_info(all)]),
    ?STATUS_SUCCESS;

process(["mnesia", "info"]) ->
    mnesia:info(),
    ?STATUS_SUCCESS;

process(["mnesia", Arg]) when is_list(Arg) ->
    case catch mnesia:system_info(list_to_atom(Arg)) of
	{'EXIT', Error} -> ?PRINT("Error: ~p~n", [Error]);
	Return -> ?PRINT("~p~n", [Return])
    end,
    ?STATUS_SUCCESS;

process(["delete-old-messages", Days]) ->
    case catch list_to_integer(Days) of
	{'EXIT',{Reason, _Stack}} ->
            ?PRINT("Can't delete old messages (~p). Please pass an integer as parameter.~n",
                      [Reason]),
	    ?STATUS_ERROR;
	Integer when Integer >= 0 ->
	    {atomic, _} = mod_offline:remove_old_messages(Integer),
	    ?PRINT("Removed messages older than ~s days~n", [Days]),
	    ?STATUS_SUCCESS;
	_Integer ->
	    ?PRINT("Can't delete old messages. Please pass a positive integer as parameter.~n", []),
	    ?STATUS_ERROR
    end;

process(["vhost", H | Args]) ->
    try
	Host = exmpp_stringprep:nameprep(H),
	case ejabberd_hooks:run_fold(
	       ejabberd_ctl_process, Host, false, [Host, Args]) of
	    false ->
		print_vhost_usage(Host),
		?STATUS_USAGE;
	    Status ->
		Status
	end
    catch
	_ ->
	    ?PRINT("Bad hostname: ~p~n", [H]),
	    ?STATUS_ERROR
    end;

process(Args) ->
    case ejabberd_hooks:run_fold(ejabberd_ctl_process, false, [Args]) of
	false ->
	    print_usage(),
	    ?STATUS_USAGE;
	Status ->
	    Status
    end.


print_usage() ->
    CmdDescs =
	[{"status", "get ejabberd status"},
	 {"stop", "stop ejabberd"},
	 {"restart", "restart ejabberd"},
	 {"reopen-log", "reopen log file"},
	 {"register user server password", "register a user"},
	 {"unregister user server", "unregister a user"},
	 {"backup file", "store a database backup to file"},
	 {"restore file", "restore a database backup from file"},
	 {"install-fallback file", "install a database fallback from file"},
	 {"dump file", "dump a database to a text file"},
	 {"load file", "restore a database from a text file"},
	 {"import-file file", "import user data from jabberd 1.4 spool file"},
	 {"import-dir dir", "import user data from jabberd 1.4 spool directory"},
	 {"delete-expired-messages", "delete expired offline messages from database"},
	 {"delete-old-messages n", "delete offline messages older than n days from database"},
	 {"mnesia [info]", "show information of Mnesia system"},
	 {"vhost host ...", "execute host-specific commands"}] ++
	ets:tab2list(ejabberd_ctl_cmds),
    MaxCmdLen =
	lists:max(lists:map(
		    fun({Cmd, _Desc}) ->
			    length(Cmd)
		    end, CmdDescs)),
    NewLine = io_lib:format("~n", []),
    FmtCmdDescs =
	lists:map(
	  fun({Cmd, Desc}) ->
		  ["  ", Cmd, string:chars($\s, MaxCmdLen - length(Cmd) + 2),
		   Desc, NewLine]
	  end, CmdDescs),
    ?PRINT(
      "Usage: ejabberdctl [--node nodename] command [options]~n"
      "~n"
      "Available commands in this ejabberd node:~n"
      ++ FmtCmdDescs ++
      "~n"
      "Examples:~n"
      "  ejabberdctl restart~n"
      "  ejabberdctl --node ejabberd@host restart~n"
      "  ejabberdctl vhost jabber.example.org ...~n",
     []).

print_vhost_usage(Host) ->
    CmdDescs =
	ets:select(ejabberd_ctl_host_cmds,
		   [{{{Host, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]),
    MaxCmdLen =
	if
	    CmdDescs == [] ->
		0;
	    true ->
		lists:max(lists:map(
			    fun({Cmd, _Desc}) ->
				    length(Cmd)
			    end, CmdDescs))
	end,
    NewLine = io_lib:format("~n", []),
    FmtCmdDescs =
	lists:map(
	  fun({Cmd, Desc}) ->
		  ["  ", Cmd, string:chars($\s, MaxCmdLen - length(Cmd) + 2),
		   Desc, NewLine]
	  end, CmdDescs),
    ?PRINT(
      "Usage: ejabberdctl [--node nodename] vhost hostname command [options]~n"
      "~n"
      "Available commands in this ejabberd node and this vhost:~n"
      ++ FmtCmdDescs ++
      "~n"
      "Examples:~n"
      "  ejabberdctl vhost "++Host++" registered-users~n",
     []).

register_commands(CmdDescs, Module, Function) ->
    ets:insert(ejabberd_ctl_cmds, CmdDescs),
    ejabberd_hooks:add(ejabberd_ctl_process,
		       Module, Function, 50),
    ok.

register_commands(Host, CmdDescs, Module, Function) ->
    ets:insert(ejabberd_ctl_host_cmds,
	       [{{Host, Cmd}, Desc} || {Cmd, Desc} <- CmdDescs]),
    ejabberd_hooks:add(ejabberd_ctl_process, Host,
		       Module, Function, 50),
    ok.

unregister_commands(CmdDescs, Module, Function) ->
    lists:foreach(fun(CmdDesc) ->
			  ets:delete_object(ejabberd_ctl_cmds, CmdDesc)
		  end, CmdDescs),
    ejabberd_hooks:delete(ejabberd_ctl_process,
			  Module, Function, 50),
    ok.

unregister_commands(Host, CmdDescs, Module, Function) ->
    lists:foreach(fun({Cmd, Desc}) ->
			  ets:delete_object(ejabberd_ctl_host_cmds,
					    {{Host, Cmd}, Desc})
		  end, CmdDescs),
    ejabberd_hooks:delete(ejabberd_ctl_process,
			  Module, Function, 50),
    ok.

dump_to_textfile(File) ->
    dump_to_textfile(mnesia:system_info(is_running), file:open(File, write)).
dump_to_textfile(yes, {ok, F}) ->
    Tabs1 = lists:delete(schema, mnesia:system_info(local_tables)),
    Tabs = lists:filter(
	     fun(T) ->
		     case mnesia:table_info(T, storage_type) of
			 disc_copies -> true;
			 disc_only_copies -> true;
			 _ -> false
		     end
	     end, Tabs1),
    Defs = lists:map(
	     fun(T) -> {T, [{record_name, mnesia:table_info(T, record_name)},
			    {attributes, mnesia:table_info(T, attributes)}]}
	     end,
	     Tabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
    file:close(F);
dump_to_textfile(_, {ok, F}) ->
    file:close(F),
    {error, mnesia_not_running};
dump_to_textfile(_, {error, Reason}) ->
    {error, Reason}.


dump_tab(F, T) ->
    W = mnesia:table_info(T, wild_pattern),
    {atomic,All} = mnesia:transaction(
		     fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(
      fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).
