%%%----------------------------------------------------------------------
%%% File    : ejabberd_ctl.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Ejabberd admin tool
%%% Created : 11 Jan 2004 by Alexey Shchepin <alex@alex.sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_ctl).
-author('alexey@sevcom.net').

-export([start/0,
	 init/0,
	 process/1,
	 register_commands/3,
	 register_commands/4,
	 unregister_commands/3,
	 unregister_commands/4]).

-include("ejabberd_ctl.hrl").

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    Node = list_to_atom(SNode),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     io:format("RPC failed on the node ~p: ~p~n",
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
    io:format("Node ~p is ~p. Status: ~p~n",
              [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(ejabberd, 1, application:which_applications()) of
        false ->
            io:format("ejabberd is not running~n", []),
            ?STATUS_ERROR;
        {value,_Version} ->
            io:format("ejabberd is running~n", []),
            ?STATUS_SUCCESS
    end;

process(["stop"]) ->
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(["reopen-log"]) ->
    ejabberd_logger_h:reopen_log(),
    ?STATUS_SUCCESS;

process(["register", User, Server, Password]) ->
    case ejabberd_auth:try_register(User, Server, Password) of
	{atomic, ok} ->
	    ?STATUS_SUCCESS;
	{atomic, exists} ->
	    io:format("User ~p already registered at node ~p~n",
		      [User ++ "@" ++ Server, node()]),
	    ?STATUS_ERROR;
	{error, Reason} ->
	    io:format("Can't register user ~p at node ~p: ~p~n",
		      [User ++ "@" ++ Server, node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["unregister", User, Server]) ->
    case ejabberd_auth:remove_user(User, Server) of
	{error, Reason} ->
	    io:format("Can't unregister user ~p at node ~p: ~p~n",
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
	    io:format("Can't store backup in ~p at node ~p: ~p~n",
		      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["dump", Path]) ->
    case dump_to_textfile(Path) of
	ok ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
            io:format("Can't store dump in ~p at node ~p: ~p~n",
                      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["load", Path]) ->
    case mnesia:load_textfile(Path) of
        {atomic, ok} ->
            ?STATUS_SUCCESS;
        {error, Reason} ->
            io:format("Can't load dump in ~p at node ~p: ~p~n",
                      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["restore", Path]) ->
    case mnesia:restore(Path, [{default_op, keep_tables}]) of
	{atomic, _} ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    io:format("Can't restore backup from ~p at node ~p: ~p~n",
		      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["install-fallback", Path]) ->
    case mnesia:install_fallback(Path) of
	ok ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    io:format("Can't install fallback from ~p at node ~p: ~p~n",
		      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["import-file", Path]) ->
    case jd2ejd:import_file(Path) of
        ok ->
            ?STATUS_SUCCESS;
        {error, Reason} ->
            io:format("Can't import jabberd 1.4 spool file ~p at node ~p: ~p~n",
                      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["import-dir", Path]) ->
    case jd2ejd:import_dir(Path) of
        ok ->
            ?STATUS_SUCCESS;
        {error, Reason} ->
            io:format("Can't import jabberd 1.4 spool dir ~p at node ~p: ~p~n",
                      [filename:absname(Path), node(), Reason]),
	    ?STATUS_ERROR
    end;

process(["delete-expired-messages"]) ->
    mod_offline:remove_expired_messages(),
    ?STATUS_SUCCESS;

process(["vhost", H | Args]) ->
    case jlib:nameprep(H) of
	false ->
	    io:format("Bad hostname: ~p~n", [H]),
	    ?STATUS_ERROR;
	Host ->
	    case ejabberd_hooks:run_fold(
		   ejabberd_ctl_process, Host, false, [Host, Args]) of
		false ->
		    print_vhost_usage(Host),
		    ?STATUS_USAGE;
		Status ->
		    Status
	    end
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
    io:format(
      "Usage: ejabberdctl node command~n"
      "~n"
      "Available commands:~n"
      ++ FmtCmdDescs ++
      "~n"
      "Example:~n"
      "  ejabberdctl ejabberd@host restart~n"
     ).

print_vhost_usage(Host) ->
    CmdDescs =
	ets:select(ejabberd_ctl_host_cmds,
		   [{{Host, '$1', '$2'}, [], [{{'$1', '$2'}}]}]),
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
    io:format(
      "Usage: ejabberdctl node vhost host command~n"
      "~n"
      "Available commands:~n"
      ++ FmtCmdDescs ++
      "~n"
     ).

register_commands(CmdDescs, Module, Function) ->
    ets:insert(ejabberd_ctl_cmds, CmdDescs),
    ejabberd_hooks:add(ejabberd_ctl_process,
		       Module, Function, 50),
    ok.

register_commands(Host, CmdDescs, Module, Function) ->
    ets:insert(ejabberd_ctl_host_cmds,
	       [{Host, Cmd, Desc} || {Cmd, Desc} <- CmdDescs]),
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
					    {Host, Cmd, Desc})
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

