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
	 room_destroyed/1,
	 store_room/2,
	 restore_room/1,
	 forget_room/1,
	 process_iq_disco_items/5]).

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
    load_permanent_rooms(Host),
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
				     iq_disco_info()}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			{iq, ID, get, ?NS_DISCO_ITEMS = XMLNS, SubEl} ->
			    spawn(?MODULE,
				  process_iq_disco_items,
				  [Host, From, To, ID, SubEl]);
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


store_room(Name, Opts) ->
    F = fun() ->
		mnesia:write(#muc_room{name = Name,
				       opts = Opts})
	end,
    mnesia:transaction(F).

restore_room(Name) ->
    case catch mnesia:dirty_read(muc_room, Name) of
	[#muc_room{opts = Opts}] ->
	    Opts;
	_ ->
	    error
    end.


forget_room(Name) ->
    F = fun() ->
		mnesia:delete({muc_room, Name})
	end,
    mnesia:transaction(F).


load_permanent_rooms(Host) ->
    case catch mnesia:dirty_select(muc_room, [{'_', [], ['$_']}]) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    ok;
	Rs ->
	    lists:foreach(fun(R) ->
				  Room = R#muc_room.name,
				  {ok, Pid} = mod_muc_room:start(
						Host,
						Room,
						R#muc_room.opts),
				  ets:insert(
				    muc_online_room,
				    #muc_online_room{name = Room, pid = Pid})
			  end, Rs)
    end.


iq_disco_info() ->
    [{xmlelement, "identity",
      [{"category", "conference"},
       {"type", "text"},
       {"name", "ejabberd/mod_muc"}], []},
     {xmlelement, "feature",
      [{"var", ?NS_MUC}], []}].


process_iq_disco_items(Host, From, To, ID, SubEl) ->
    Res = {iq, ID, result, ?NS_DISCO_ITEMS,
	   [{xmlelement, "query",
	     [{"xmlns", ?NS_DISCO_ITEMS}],
	     iq_disco_items(Host, From)}]},
    ejabberd_router:route(To,
			  From,
			  jlib:iq_to_xml(Res)).

% TODO: ask more info from room processes
iq_disco_items(Host, From) ->
    lists:zf(fun(#muc_online_room{name = Name, pid = Pid}) ->
		     case catch gen_fsm:sync_send_all_state_event(
				  Pid, get_disco_item, 100) of
			 {item, Desc} ->
			     {true,
			      {xmlelement, "item",
			       [{"jid", jlib:jid_to_string({Name, Host, ""})},
				{"name", Desc}], []}};
			 _ ->
			     false
		     end
	     end, ets:tab2list(muc_online_room)).




