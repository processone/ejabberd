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

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    Node = list_to_atom(SNode),
	    process(Node, Args);
	_ ->
	    print_usage()
    end,
    halt().


process(Node, ["stop"]) ->
    case rpc:call(Node, init, stop, []) of
	{badrpc, Reason} ->
	    io:format("Can't stop node ~p: ~p~n",
		      [Node, Reason]);
	_ ->
	    ok
    end;

process(Node, ["restart"]) ->
    case rpc:call(Node, init, restart, []) of
	{badrpc, Reason} ->
	    io:format("Can't restart node ~p: ~p~n",
		      [Node, Reason]);
	_ ->
	    ok
    end;

process(Node, ["register", User, Password]) ->
    case rpc:call(Node, ejabberd_auth, try_register, [User, Password]) of
	{atomic, ok} ->
	    ok;
	{atomic, exists} ->
	    io:format("User ~p already registered on node ~p~n",
		      [User, Node]);
	{error, Reason} ->
	    io:format("Can't register user ~p on node ~p: ~p~n",
		      [User, Node, Reason]);
	{badrpc, Reason} ->
	    io:format("Can't register user ~p on node ~p: ~p~n",
		      [User, Node, Reason])
    end;

process(Node, ["unregister", User]) ->
    case rpc:call(Node, ejabberd_auth, remove_user, [User]) of
	{atomic, ok} ->
	    ok;
	{error, Reason} ->
	    io:format("Can't unregister user ~p on node ~p: ~p~n",
		      [User, Node, Reason]);
	{badrpc, Reason} ->
	    io:format("Can't unregister user ~p on node ~p: ~p~n",
		      [User, Node, Reason])
    end;

process(_Node, _Args) ->
    print_usage().



print_usage() ->
    io:format("Usage: ejabberdctl node command~n"
	      "~n"
	      "Available commands:~n"
	      "  stop\t\t\t\tstop ejabberd~n"
	      "  restart\t\t\trestart ejabberd~n"
	      "  register user password\tregister user~n"
	      "  unregister user\t\tunregister user~n"
	      "~n"
	      "Example:~n"
	      "  ejabberdctl ejabberd@host restart~n"
	     ).
