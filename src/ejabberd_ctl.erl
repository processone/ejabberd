%%%----------------------------------------------------------------------
%%% File    : ejabberd_ctl.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Ejabberd admin tool
%%% Created : 11 Jan 2004 by Alexey Shchepin <alex@alex.sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_ctl).
-author('alexey@sevcom.net').

-export([start/0]).

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    Node = list_to_atom(SNode),
	    Status = process(Node, Args),
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.


process(Node, ["status"]) ->
    case rpc:call(Node, init, get_status, []) of
	{badrpc, Reason} ->
	    io:format("Can't get node ~p status: ~p~n",
		      [Node, Reason]),
	    ?STATUS_BADRPC;
	{InternalStatus, ProvidedStatus} ->
	    io:format("Node ~p is ~p. Status: ~p~n",
		      [Node, InternalStatus, ProvidedStatus]),
	    ?STATUS_SUCCESS
    end;

process(Node, ["stop"]) ->
    case rpc:call(Node, init, stop, []) of
	{badrpc, Reason} ->
	    io:format("Can't stop node ~p: ~p~n",
		      [Node, Reason]),
	    ?STATUS_BADRPC;
	_ ->
	    ?STATUS_SUCCESS
    end;

process(Node, ["restart"]) ->
    case rpc:call(Node, init, restart, []) of
	{badrpc, Reason} ->
	    io:format("Can't restart node ~p: ~p~n",
		      [Node, Reason]),
	    ?STATUS_BADRPC;
	_ ->
	    ?STATUS_SUCCESS
    end;

process(Node, ["reopen-log"]) ->
    case rpc:call(Node, ejabberd_logger_h, reopen_log, []) of
	{badrpc, Reason} ->
	    io:format("Can't reopen node ~p log: ~p~n",
		      [Node, Reason]),
	    ?STATUS_BADRPC;
	_ ->
	    ?STATUS_SUCCESS
    end;

process(Node, ["register", User, Password]) ->
    case rpc:call(Node, ejabberd_auth, try_register, [User, Password]) of
	{atomic, ok} ->
	    ?STATUS_SUCCESS;
	{atomic, exists} ->
	    io:format("User ~p already registered on node ~p~n",
		      [User, Node]),
	    ?STATUS_ERROR;
	{error, Reason} ->
	    io:format("Can't register user ~p on node ~p: ~p~n",
		      [User, Node, Reason]),
	    ?STATUS_ERROR;
	{badrpc, Reason} ->
	    io:format("Can't register user ~p on node ~p: ~p~n",
		      [User, Node, Reason]),
	    ?STATUS_BADRPC
    end;

process(Node, ["unregister", User]) ->
    case rpc:call(Node, ejabberd_auth, remove_user, [User]) of
	{atomic, ok} ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    io:format("Can't unregister user ~p on node ~p: ~p~n",
		      [User, Node, Reason]),
	    ?STATUS_ERROR;
	{badrpc, Reason} ->
	    io:format("Can't unregister user ~p on node ~p: ~p~n",
		      [User, Node, Reason]),
	    ?STATUS_BADRPC
    end;

process(Node, ["backup", Path]) ->
    case rpc:call(Node, mnesia, backup, [Path]) of
        ok ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    io:format("Can't store backup in ~p on node ~p: ~p~n",
		      [Path, Node, Reason]),
	    ?STATUS_ERROR;
	{badrpc, Reason} ->
	    io:format("Can't store backup in ~p on node ~p: ~p~n",
		      [Path, Node, Reason]),
	    ?STATUS_BADRPC
    end;

process(Node, ["dump", Path]) ->
    case rpc:call(Node, mnesia, dump_to_textfile, [Path]) of
	ok ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
            io:format("Can't store dump in ~p on node ~p: ~p~n",
                      [Path, Node, Reason]),
	    ?STATUS_ERROR;
        {badrpc, Reason} ->
            io:format("Can't store dump in ~p on node ~p: ~p~n",
                      [Path, Node, Reason]),
	    ?STATUS_BADRPC
    end;

process(Node, ["load", Path]) ->
    case rpc:call(Node, mnesia, load_textfile, [Path]) of
        ok ->
            ?STATUS_SUCCESS;
        {error, Reason} ->
            io:format("Can't load dump in ~p on node ~p: ~p~n",
                      [Path, Node, Reason]),
	    ?STATUS_ERROR;
        {badrpc, Reason} ->
            io:format("Can't load dump in ~p on node ~p: ~p~n",
                      [Path, Node, Reason]),
	    ?STATUS_BADRPC
    end;

process(Node, ["restore", Path]) ->
    case rpc:call(Node,
		  mnesia, restore, [Path, [{default_op, keep_tables}]]) of
	{atomic, _} ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    io:format("Can't restore backup from ~p on node ~p: ~p~n",
		      [Path, Node, Reason]),
	    ?STATUS_ERROR;
	{badrpc, Reason} ->
	    io:format("Can't restore backup from ~p on node ~p: ~p~n",
		      [Path, Node, Reason]),
	    ?STATUS_BADRPC
    end;

process(Node, ["install-fallback", Path]) ->
    case rpc:call(Node, mnesia, install_fallback, [Path]) of
	{atomic, ok} ->
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    io:format("Can't install fallback from ~p on node ~p: ~p~n",
		      [Path, Node, Reason]),
	    ?STATUS_ERROR;
	{badrpc, Reason} ->
	    io:format("Can't install fallback from ~p on node ~p: ~p~n",
		      [Path, Node, Reason]),
	    ?STATUS_BADRPC
    end;

process(Node, ["registered-users"]) ->
    case rpc:call(Node, ejabberd_auth, dirty_get_registered_users, []) of
	Users when is_list(Users) ->
	    NewLine = io_lib:format("~n", []),
	    SUsers = lists:sort(Users),
	    FUsers = lists:map(fun(U) -> [U, NewLine] end, SUsers),
	    io:format("~s", [FUsers]),
	    ?STATUS_SUCCESS;
	{error, Reason} ->
	    io:format("Can't get list of registered users on node ~p: ~p~n",
		      [Node, Reason]),
	    ?STATUS_ERROR;
	{badrpc, Reason} ->
	    io:format("Can't get list of registered users on node ~p: ~p~n",
		      [Node, Reason]),
	    ?STATUS_BADRPC
    end;

process(Node, ["delete-expired-messages"]) ->
    case rpc:call(Node, mod_offline, remove_expired_messages, []) of
	{badrpc, Reason} ->
	    io:format("Can't delete expired messages at node ~p: ~p~n",
		      [Node, Reason]),
	    ?STATUS_BADRPC;
	_ ->
	    ?STATUS_SUCCESS
    end;

process(_Node, _Args) ->
    print_usage(),
    ?STATUS_USAGE.



print_usage() ->
    io:format(
      "Usage: ejabberdctl node command~n"
      "~n"
      "Available commands:~n"
      "  status\t\t\tget ejabberd status~n"
      "  stop\t\t\t\tstop ejabberd~n"
      "  restart\t\t\trestart ejabberd~n"
      "  reopen-log\t\t\treopen log file~n"
      "  register user password\tregister a user~n"
      "  unregister user\t\tunregister a user~n"
      "  backup file\t\t\tstore a database backup in file~n"
      "  restore file\t\t\trestore a database backup from file~n"
      "  install-fallback file\t\tinstall a database fallback from file~n"
      "  dump file\t\t\tdump a database in a text file~n"
      "  load file\t\t\trestore a database from a text file~n"
      "  registered-users\t\tlist all registered users~n"
      "  delete-expired-messages\tdelete expired offline messages from database~n"
      "~n"
      "Example:~n"
      "  ejabberdctl ejabberd@host restart~n"
     ).
