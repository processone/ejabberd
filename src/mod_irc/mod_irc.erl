%%%----------------------------------------------------------------------
%%% File    : mod_irc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : IRC transport
%%% Created : 15 Feb 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_irc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1, init/1, stop/0, closed_conection/2]).

-include("ejabberd.hrl").
-include("namespaces.hrl").

-record(irc_connection, {userserver, pid}).

start(Opts) ->
    iconv:start(),
    Host = gen_mod:get_opt(host, Opts, "irc." ++ ?MYNAME),
    register(ejabberd_mod_irc, spawn(?MODULE, init, [Host])).

init(Host) ->
    catch ets:new(irc_connection, [named_table,
				   public,
				   {keypos, #irc_connection.userserver}]),
    ejabberd_router:register_route(Host),
    loop(Host).

loop(Host) ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(Host, From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Host);
	stop ->
	    ejabberd_router:unregister_global_route(Host),
	    ok;
	_ ->
	    loop(Host)
    end.


do_route(Host, From, To, Packet) ->
    {ChanServ, _, Resource} = To,
    case ChanServ of
	"" ->
	    case Resource of
		"" ->
		    case jlib:iq_query_info(Packet) of
			{iq, ID, get, ?NS_DISCO_INFO = XMLNS, SubEl} ->
			    Res = {iq, ID, result, XMLNS,
				   [{xmlelement, "query",
				     [{"xmlns", XMLNS}],
				     [{xmlelement, "identity",
				       [{"category", "conference"},
					{"type", "irc"},
					{"name", "ejabberd"}], []},
				      {xmlelement, "feature",
				       [{"var", ?NS_MUC}], []}]}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, "503", "Service Unavailable"),
			    ejabberd_router:route(To, From, Err)
		    end;
		_ ->
		    Err = jlib:make_error_reply(Packet,
						"406", "Not Acceptable"),
		    ejabberd_router:route(To, From, Err)
	    end;
	_ ->
	    case string:tokens(ChanServ, "%") of
		[[_ | _] = Channel, [_ | _] = Server] ->
		    case ets:lookup(irc_connection, {From, Server}) of
			[] ->
			    io:format("open new connection~n"),
			    {ok, Pid} = mod_irc_connection:start(
					  From, Host, Server),
			    ets:insert(
			      irc_connection,
			      #irc_connection{userserver = {From, Server},
					      pid = Pid}),
			    mod_irc_connection:route(
			      Pid, Channel, Resource, Packet),
			    ok;
			[R] ->
			    Pid = R#irc_connection.pid,
			    io:format("send to process ~p~n",
				      [Pid]),
			    mod_irc_connection:route(
			      Pid, Channel, Resource, Packet),
			    ok
		    end;
		_ ->
		    Err = jlib:make_error_reply(
			    Packet, "406", "Not Acceptable"),
		    ejabberd_router:route(To, From, Err)
	    end
    end.




stop() ->
    ejabberd_mod_irc ! stop,
    ok.

closed_conection(From, Server) ->
    ets:delete(irc_connection, {From, Server}).

