%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_sm).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start_link/0, init/0,
	 route/3,
	 open_session/3, close_session/3,
	 bounce_offline_message/3,
	 disconnect_removed_user/2,
	 get_user_resources/2,
	 set_presence/4,
	 unset_presence/4,
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 get_vh_session_list/1,
	 register_iq_handler/3,
	 register_iq_handler/4,
	 unregister_iq_handler/1
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(session, {usr, us, pid}).
-record(presence, {usr, us, priority}).

start_link() ->
    Pid = proc_lib:spawn_link(ejabberd_sm, init, []),
    register(ejabberd_sm, Pid),
    {ok, Pid}.

init() ->
    update_tables(),
    mnesia:create_table(session, [{ram_copies, [node()]},
				  {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies),
    mnesia:create_table(presence,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, presence)}]),
    mnesia:add_table_index(presence, us),
    mnesia:subscribe(system),
    ets:new(sm_iqtable, [named_table]),
    ejabberd_hooks:add(offline_message_hook,
		       ejabberd_sm, bounce_offline_message, 100),
    ejabberd_hooks:add(remove_user,
		       ejabberd_sm, disconnect_removed_user, 100),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p~nwhen processing: ~p",
			       [Reason, {From, To, Packet}]);
		_ ->
		    ok
	    end,
	    loop();
	{mnesia_system_event, {mnesia_down, Node}} ->
	    clean_table_from_bad_node(Node),
	    loop();
	{register_iq_handler, XMLNS, Module, Function} ->
	    ets:insert(sm_iqtable, {XMLNS, Module, Function}),
	    loop();
	{register_iq_handler, XMLNS, Module, Function, Opts} ->
	    ets:insert(sm_iqtable, {XMLNS, Module, Function, Opts}),
	    loop();
	{unregister_iq_handler, XMLNS} ->
	    case ets:lookup(sm_iqtable, XMLNS) of
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:stop_iq_handler(Module, Function, Opts);
		_ ->
		    ok
	    end,
	    ets:delete(sm_iqtable, XMLNS),
	    loop();
	_ ->
	    loop()
    end.


route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end.

open_session(User, Server, Resource) ->
    register_connection(User, Server, Resource, self()).

close_session(User, Server, Resource) ->
    remove_connection(User, Server, Resource).


register_connection(User, Server, Resource, Pid) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    F = fun() ->
		Ss = mnesia:wread({session, USR}),
		mnesia:write(#session{usr = USR, us = US, pid = Pid}),
		Ss
        end,
    case mnesia:transaction(F) of
	{atomic, Ss} ->
	    lists:foreach(
	      fun(R) ->
		      R#session.pid ! replaced
	      end, Ss);
	_ ->
	    false
    end.


remove_connection(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LResource = jlib:resourceprep(Resource),
    LServer = jlib:nameprep(Server),
    USR = {LUser, LServer, LResource},
    F = fun() ->
		mnesia:delete({session, USR})
        end,
    mnesia:transaction(F).


clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       session,
		       [{#session{pid = '$1', _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
        end,
    mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer, lresource = LResource} = To,
    {xmlelement, Name, Attrs, _Els} = Packet,
    case LResource of
	"" ->
	    case Name of
		"presence" ->
		    {Pass, Subsc} =
			case xml:get_attr_s("type", Attrs) of
			    "subscribe" ->
				{ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   false,
				   [User, Server, From, subscribe]),
				 true};
			    "subscribed" ->
				{ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   false,
				   [User, Server, From, subscribed]),
				 true};
			    "unsubscribe" ->
				{ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   false,
				   [User, Server, From, unsubscribe]),
				 true};
			    "unsubscribed" ->
				{ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   false,
				   [User, Server, From, unsubscribed]),
				 true};
			    _ ->
				{true, false}
			end,
		    if Pass ->
			    LFrom = jlib:jid_tolower(From),
			    PResources = get_user_present_resources(
					   LUser, LServer),
			    if
				PResources /= [] ->
				    lists:foreach(
				      fun({_, R}) ->
					      if LFrom /=
						 {LUser, LServer, R} ->
						      do_route(
							From,
							jlib:jid_replace_resource(To, R),
							Packet);
						 true ->
						      ok
					      end
				      end, PResources);
				true ->
				    if
					Subsc ->
					    case ejabberd_auth:is_user_exists(
						   LUser, LServer) of
						true ->
						    ejabberd_hooks:run(
						      offline_subscription_hook,
						      [From, To, Packet]);
						_ ->
						    Err = jlib:make_error_reply(
							    Packet, ?ERR_SERVICE_UNAVAILABLE),
						    ejabberd_router:route(To, From, Err)
					    end;
					true ->
					    ok
				    end
			    end;
		       true ->
			    ok
		    end;
		"message" ->
		    route_message(From, To, Packet);
		"iq" ->
		    process_iq(From, To, Packet);
		"broadcast" ->
		    lists:foreach(
		      fun(R) ->
			      do_route(From,
				       jlib:jid_replace_resource(To, R),
				       Packet)
		      end, get_user_resources(User, Server));
		_ ->
		    ok
	    end;
	_ ->
	    USR = {LUser, LServer, LResource},
	    case mnesia:dirty_read({session, USR}) of
		[] ->
		    case Name of
			"message" ->
			    route_message(From, To, Packet);
			"iq" ->
			    case xml:get_attr_s("type", Attrs) of
				"error" -> ok;
				"result" -> ok;
				_ ->
				    Err =
					jlib:make_error_reply(
					  Packet, ?ERR_RECIPIENT_UNAVAILABLE),
				    ejabberd_router:route(To, From, Err)
			    end;
			_ ->
			    ?DEBUG("packet droped~n", [])
		    end;
		[Sess] ->
		    Pid = Sess#session.pid,
		    ?DEBUG("sending to process ~p~n", [Pid]),
		    Pid ! {route, From, To, Packet}
	    end
    end.

route_message(From, To, Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    case catch lists:max(get_user_present_resources(LUser, LServer)) of
	{Priority, R} when is_integer(Priority),
			   Priority >= 0 ->
	    LResource = jlib:resourceprep(R),
	    USR = {LUser, LServer, LResource},
	    case mnesia:dirty_read({session, USR}) of
		[] ->
		    ok;				% Race condition
		[Sess] ->
		    Pid = Sess#session.pid,
		    ?DEBUG("sending to process ~p~n", [Pid]),
		    Pid ! {route, From, To, Packet}
	    end;
	_ ->
	    case xml:get_tag_attr_s("type", Packet) of
		"error" ->
		    ok;
		_ ->
		    case ejabberd_auth:is_user_exists(LUser, LServer) of
			true ->
			    ejabberd_hooks:run(offline_message_hook,
					       [From, To, Packet]);
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		    end
	    end
    end.

bounce_offline_message(From, To, Packet) ->
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err),
    stop.

disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jlib:make_jid("", "", ""),
		      jlib:make_jid(User, Server, ""),
		      {xmlelement, "broadcast", [],
		       [{exit, "User removed"}]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_user_resources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US, #session.us) of
	{'EXIT', _Reason} ->
	    [];
	Rs ->
	    lists:map(fun(R) ->
			      element(3, R#session.usr)
		      end, Rs)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_presence(User, Server, Resource, Priority) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    USR = {User, Server, Resource},
    US = {LUser, LServer},
    F = fun() ->
		mnesia:write(#presence{usr = USR, us = US,
				       priority = Priority})
	end,
    mnesia:transaction(F).

unset_presence(User, Server, Resource, Status) ->
    USR = {User, Server, Resource},
    F = fun() ->
		mnesia:delete({presence, USR})
	end,
    mnesia:transaction(F),
    ejabberd_hooks:run(unset_presence_hook, [User, Server, Resource, Status]).

get_user_present_resources(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(presence, US, #presence.us) of
	{'EXIT', _Reason} ->
	    [];
	Rs ->
	    lists:map(fun(R) ->
			      {R#presence.priority, element(3, R#presence.usr)}
		      end, Rs)
    end.

dirty_get_sessions_list() ->
    mnesia:dirty_all_keys(session).

dirty_get_my_sessions_list() ->
    mnesia:dirty_select(
      session,
      [{#session{pid = '$1', _ = '_'},
	[{'==', {node, '$1'}, node()}],
	['$_']}]).

get_vh_session_list(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(
      session,
      [{#session{usr = '$1', _ = '_'},
	[{'==', {element, 2, '$1'}, LServer}],
	['$1']}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
	#iq{xmlns = XMLNS} ->
	    case ets:lookup(sm_iqtable, XMLNS) of
		[{_, Module, Function}] ->
		    ResIQ = Module:Function(From, To, IQ),
		    if
			ResIQ /= ignore ->
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(ResIQ));
			true ->
			    ok
		    end;
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:handle(Module, Function, Opts,
					  From, To, IQ);
		[] ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
		    ejabberd_router:route(To, From, Err)
	    end;
	reply ->
	    ok;
	_ ->
	    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	    ejabberd_router:route(To, From, Err),
	    ok
    end.

register_iq_handler(XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, XMLNS, Module, Fun}.

register_iq_handler(XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, XMLNS, Module, Fun, Opts}.

unregister_iq_handler(XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, XMLNS}.



update_tables() ->
    case catch mnesia:table_info(session, attributes) of
	[ur, user, node] ->
	    mnesia:delete_table(session);
	[ur, user, pid] ->
	    mnesia:delete_table(session);
	[usr, us, pid] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end,
    case catch mnesia:table_info(presence, attributes) of
	[ur, user, priority] ->
	    mnesia:delete_table(presence);
	[usr, us, priority] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end,
    case lists:member(local_session, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_session);
	false ->
	    ok
    end.

