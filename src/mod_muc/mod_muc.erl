%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : MUC support (JEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_muc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 init/1,
	 stop/0,
	 room_destroyed/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-record(muc_room, {name, opts}).
-record(muc_online_room, {name, pid}).

start(Opts) ->
    mnesia:create_table(muc_room,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, muc_room)}]),
    Host = gen_mod:get_opt(host, Opts, "conference." ++ ?MYNAME),
    register(ejabberd_mod_muc, spawn(?MODULE, init, [Host])).



init(Host) ->
    catch ets:new(muc_online_room, [named_table,
				    public,
				    {keypos, #muc_online_room.name}]),
    ejabberd_router:register_route(Host),
    % TODO: load permanent groups
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
	{room_destroyed, Room} ->
	    ets:delete(muc_online_room, Room);
	stop ->
	    % TODO
	    ejabberd_router:unregister_global_route(Host),
	    ok;
	_ ->
	    loop(Host)
    end.


do_route(Host, From, To, Packet) ->
    {Room, _, Nick} = To,
    case Room of
	"" ->
	    case Nick of
		"" ->
		    case jlib:iq_query_info(Packet) of
			{iq, ID, get, ?NS_DISCO_INFO = XMLNS, SubEl} ->
			    Res = {iq, ID, result, XMLNS,
				   [{xmlelement, "query",
				     [{"xmlns", XMLNS}],
				     iq_disco()}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		    end;
		_ ->
		    Err = jlib:make_error_reply(Packet,
						?ERR_JID_NOT_FOUND),
		    ejabberd_router:route(To, From, Err)
	    end;
	_ ->
	    case ets:lookup(muc_online_room, Room) of
		[] ->
		    {xmlelement, Name, Attrs, Els} = Packet,
		    Type = xml:get_attr_s("type", Attrs),
		    case {Name, Type} of
			{"presence", ""} ->
			    io:format("MUC: open new room '~s'~n", [Room]),
			    {ok, Pid} = mod_muc_room:start(
					  Host, Room, From, Nick),
			    ets:insert(
			      muc_online_room,
			      #muc_online_room{name = Room, pid = Pid}),
			    mod_muc_room:route(Pid, From, Nick, Packet),
			    ok;
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		    end;
		[R] ->
		    Pid = R#muc_online_room.pid,
		    io:format("MUC: send to process ~p~n", [Pid]),
		    mod_muc_room:route(Pid, From, Nick, Packet),
		    ok
	    end
    end.




room_destroyed(Room) ->
    ejabberd_mod_muc ! {room_destroyed, Room},
    ok.

stop() ->
    ejabberd_mod_muc ! stop,
    ok.


iq_disco() ->
    [{xmlelement, "identity",
      [{"category", "conference"},
       {"type", "text"},
       {"name", "ejabberd/mod_muc"}], []},
     {xmlelement, "feature",
      [{"var", ?NS_MUC}], []}].






