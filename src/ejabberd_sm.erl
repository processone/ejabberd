%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_sm).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start_link/0, init/0, open_session/2, close_session/2,
	 get_user_resources/1,
	 set_presence/3,
	 unset_presence/2,
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 register_iq_handler/3,
	 register_iq_handler/4,
	 unregister_iq_handler/1
	]).

-include_lib("mnemosyne/include/mnemosyne.hrl").
-include("ejabberd.hrl").

-record(session, {ur, user, node}).
-record(local_session, {ur, pid}).
-record(presence, {ur, user, priority}).

start_link() ->
    {ok, proc_lib:spawn_link(ejabberd_sm, init, [])}.

init() ->
    register(ejabberd_sm, self()),
    mnesia:create_table(session, [{ram_copies, [node()]},
				  {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, user),
    mnesia:add_table_index(session, node),
    mnesia:create_table(local_session,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, local_session)}]),
    mnesia:add_table_copy(local_session, node(), ram_copies),
    mnesia:create_table(presence,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, presence)}]),
    mnesia:add_table_index(presence, user),
    mnesia:subscribe(system),
    ets:new(sm_iqtable, [named_table]),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop();
	{open_session, User, Resource, From} ->
	    replace_and_register_my_connection(User, Resource, From),
	    replace_alien_connection(User, Resource),
	    loop();
	{close_session, User, Resource} ->
	    remove_connection(User, Resource),
	    loop();
	{replace, User, Resource} ->
	    replace_my_connection(User, Resource),
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
	    mod_disco:unregister_feature(XMLNS),
	    loop();
	_ ->
	    loop()
    end.


open_session(User, Resource) ->
    ejabberd_sm ! {open_session, User, Resource, self()}.

close_session(User, Resource) ->
    ejabberd_sm ! {close_session, User, Resource}.

replace_alien_connection(User, Resource) ->
    LUser = jlib:tolower(User),
    F = fun() ->
		UR = {LUser, Resource},
		Es = mnesia:read({session, UR}),
		mnesia:write(#session{ur = UR, user = LUser, node = node()}),
		Es
        end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:foreach(
	      fun(R) ->
		      if R#session.node /= node() ->
			      {ejabberd_sm, R#session.node} !
				  {replace, User, Resource};
			 true ->
			      ok
		      end
	      end, Rs);
	_ ->
	    false
    end.


replace_my_connection(User, Resource) ->
    LUser = jlib:tolower(User),
    F = fun() ->
		UR = {LUser, Resource},
		Es = mnesia:read({local_session, UR}),
		mnesia:delete({local_session, UR}),
		Es
        end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:foreach(
	      fun(R) ->
		      R#local_session.pid ! replaced
	      end, Rs);
	_ ->
	    false
    end.

remove_connection(User, Resource) ->
    LUser = jlib:tolower(User),
    F = fun() ->
		UR = {LUser, Resource},
		mnesia:delete({local_session, UR}),
		mnesia:delete({session, UR})
        end,
    mnesia:transaction(F).

replace_and_register_my_connection(User, Resource, Pid) ->
    LUser = jlib:tolower(User),
    F = fun() ->
		UR = {LUser, Resource},
		Es = mnesia:read({local_session, UR}),
		mnesia:write(#local_session{ur = UR, pid = Pid}),
		Es
        end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:foreach(
	      fun(R) ->
		      R#local_session.pid ! replaced
	      end, Rs);
	_ ->
	    false
    end.


clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:index_read(session, Node, #session.node),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(session, E, write),
				      mnesia:delete(
					{user_resource, E#session.ur})
			      end, Es)
        end,
    mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    {User, Server, Resource} = To,
    {xmlelement, Name, Attrs, Els} = Packet,
    case Resource of
	"" ->
	    % TODO
	    case Name of
		"presence" ->
		    {FU, FS, FR} = From,
		    {Pass, Subsc} =
			case xml:get_attr_s("type", Attrs) of
			    "subscribe" ->
				{mod_roster:in_subscription(User,
							    {FU, FS, ""},
							    subscribe),
				 true};
			    "subscribed" ->
				{mod_roster:in_subscription(User,
							    {FU, FS, ""},
							    subscribed),
				 true};
			    "unsubscribe" ->
				{mod_roster:in_subscription(User,
							    {FU, FS, ""},
							    unsubscribe),
				 true};
			    "unsubscribed" ->
				{mod_roster:in_subscription(User,
							    {FU, FS, ""},
							    unsubscribed),
				 true};
			    _ ->
				{true, false}
			end,
		    if Pass ->
			    LFrom = jlib:jid_tolower(From),
			    LUser = jlib:tolower(User),
			    LServer = jlib:tolower(Server),
			    Resources = get_user_resources(User),
			    if
				Resources /= [] ->
				    lists:foreach(
				      fun(R) ->
					      if LFrom /=
						 {LUser, LServer, R} ->
						      ejabberd_sm !
							  {route,
							   From,
							   {User, Server, R},
							   Packet};
						 true ->
						      ok
					      end
				      end, Resources);
				true ->
				    if
					Subsc ->
					    mod_offline:store_packet(
					      From, To, Packet);
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
			      ejabberd_sm ! {route,
					     From,
					     {User, Server, R},
					     Packet}
		      end, get_user_resources(User));
		_ ->
		    ok
	    end;
	_ ->
	    LUR = {jlib:tolower(User), Resource},
	    Sess = mnesia:dirty_read({session, LUR}),
	    case Sess of
		[] ->
		    if
			Name == "message" ->
			    route_message(From, To, Packet);
			true ->
			    ?DEBUG("packet droped~n", [])
		    end;
		[Ses] ->
		    case mnesia:dirty_read({local_session, LUR}) of
			[] ->
			    Node = Ses#session.node,
			    ?DEBUG("sending to node ~p~n", [Node]),
			    {ejabberd_sm, Node} ! {route, From, To, Packet};
			[El] ->
			    Pid = El#local_session.pid,
			    ?DEBUG("sending to process ~p~n", [Pid]),
			    Pid ! {route, From, To, Packet}
		    end
	    end
    end.

route_message(From, To, Packet) ->
    {User, Server, Resource} = To,
    case catch lists:max(get_user_present_resources(User)) of
	{'EXIT', _} ->
	    case ejabberd_auth:is_user_exists(User) of
		true ->
		    mod_offline:store_packet(From, To, Packet);
		_ ->
		    ?DEBUG("packet droped~n", [])
	    end;
	{_, R} ->
	    ejabberd_sm ! {route,
			   From,
			   {User, Server, R},
			   Packet}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_user_resources(User) ->
    LUser = jlib:tolower(User),
    case catch mnesia:dirty_index_read(session, LUser, #session.user) of
	{'EXIT', Reason} ->
	    [];
	Rs ->
	    lists:map(fun(R) ->
			      element(2, R#session.ur)
		      end, Rs)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_presence(User, Resource, Priority) ->
    LUser = jlib:tolower(User),
    F = fun() ->
		UR = {User, Resource},
		mnesia:write(#presence{ur = UR, user = LUser,
				       priority = Priority})
	end,
    mnesia:transaction(F).

unset_presence(User, Resource) ->
    LUser = jlib:tolower(User),
    F = fun() ->
		UR = {User, Resource},
		mnesia:delete({presence, UR})
	end,
    mnesia:transaction(F).

get_user_present_resources(User) ->
    LUser = jlib:tolower(User),
    case catch mnesia:dirty_index_read(presence, LUser, #presence.user) of
	{'EXIT', Reason} ->
	    [];
	Rs ->
	    lists:map(fun(R) ->
			      {R#presence.priority, element(2, R#presence.ur)}
		      end, Rs)
    end.

dirty_get_sessions_list() ->
    mnesia:dirty_all_keys(session).

dirty_get_my_sessions_list() ->
    mnesia:dirty_all_keys(local_session).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
	{iq, ID, Type, XMLNS, SubEl} ->
	    case ets:lookup(sm_iqtable, XMLNS) of
		[{_, Module, Function}] ->
		    ResIQ = apply(Module, Function, [From, To, IQ]),
		    if
			ResIQ /= ignore ->
			    ejabberd_router ! {route,
					       To,
					       From,
					       jlib:iq_to_xml(ResIQ)};
			true ->
			    ok
		    end;
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:handle(Module, Function, Opts,
					  From, To, IQ);
		[] ->
		    Err = jlib:make_error_reply(
			    Packet, "501", "Not Implemented"),
		    ejabberd_router ! {route, To, From, Err}
	    end;
	reply ->
	    ok;
	_ ->
	    Err = jlib:make_error_reply(Packet, "400", "Bad Request"),
	    ejabberd_router ! {route, To, From, Err},
	    ok
    end.

register_iq_handler(XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, XMLNS, Module, Fun}.

register_iq_handler(XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, XMLNS, Module, Fun, Opts}.

unregister_iq_handler(XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, XMLNS}.

