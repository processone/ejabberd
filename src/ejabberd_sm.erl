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

-export([start/0, init/0, open_session/2, close_session/2]).

-include_lib("mnemosyne/include/mnemosyne.hrl").
-include("ejabberd.hrl").

-record(user_resource, {id, user, resource}).
-record(user_resource_id_seq, {name = value, id}).
-record(session, {id, node}).
-record(mysession, {id, info}).

-record(mysession_info, {pid}).


start() ->
    spawn(ejabberd_sm, init, []).

init() ->
    register(ejabberd_sm, self()),
    mnesia:create_table(user_resource_id_seq,
			[{ram_copies, [node()]},
			 {attributes,
			  record_info(fields, user_resource_id_seq)}]),
    init_seq(),
    mnesia:create_table(user_resource,[{ram_copies, [node()]},
				       {attributes,
					record_info(fields, user_resource)}]),
    mnesia:add_table_index(user_resource, user),
    mnesia:create_table(session,[{ram_copies, [node()]},
				 {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, node),
    mnesia:create_table(mysession,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, mysession)}]),
    mnesia:subscribe(system),
    %ejabberd_router:register_local_route("localhost"),
    loop().

loop() ->
    receive
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
	{route, From, To, Packet} ->
	    do_route(From, To, Packet),
	    loop();
	_ ->
	    loop()
    end.


open_session(User, Resource) ->
    ejabberd_sm ! {open_session, User, Resource, self()}.

close_session(User, Resource) ->
    ejabberd_sm ! {close_session, User, Resource}.

replace_alien_connection(User, Resource) ->
    F = fun() ->
		[ID] = mnemosyne:eval(query [X.id || X <- table(user_resource),
						     X.user = User,
						     X.resource = Resource]
				      end),
		Es = mnesia:read({session, ID}),
		mnesia:write(#session{id = ID, node = node()}),
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
    F = fun() ->
		[ID] = mnemosyne:eval(query [X.id || X <- table(user_resource),
						     X.user = User,
						     X.resource = Resource]
				      end),

		Es = mnesia:read({mysession, ID}),
		mnesia:delete({mysession, ID}),
		Es
        end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:foreach(
	      fun(R) ->
		      (R#mysession.info)#mysession_info.pid ! replaced
	      end, Rs);
	_ ->
	    false
    end.

remove_connection(User, Resource) ->
    F = fun() ->
		[ID] = mnemosyne:eval(query [X.id || X <- table(user_resource),
						     X.user = User,
						     X.resource = Resource]
				      end),

		mnesia:delete({mysession, ID}),
		mnesia:delete({session, ID}),
		mnesia:delete({user_resource, ID})
        end,
    mnesia:transaction(F).

replace_and_register_my_connection(User, Resource, Pid) ->
    F = fun() ->
		IDs = mnemosyne:eval(query [X.id || X <- table(user_resource),
						    X.user = User,
						    X.resource = Resource]
				     end),

		ID = case IDs of
			 [Id] -> Id;
			 [] ->
			     [CurID] = 
				 mnemosyne:eval(
				   query [X.id ||
					     X <- table(user_resource_id_seq)]
				     end),
			     mnesia:write(
			       #user_resource_id_seq{id = CurID + 1}),
			     mnesia:write(
			       #user_resource{id = CurID,
					      user = User,
					      resource = Resource}),
			     CurID
		     end,
		Es = mnesia:read({mysession, ID}),
		mnesia:write(#mysession{id = ID,
					info = #mysession_info{pid = Pid}}),
		Es
        end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:foreach(
	      fun(R) ->
		      (R#mysession.info)#mysession_info.pid ! replaced
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
					{user_resource, E#session.id})
			      end, Es)
        end,
    mnesia:transaction(F).

init_seq() ->
    F = fun() ->
		[] = mnesia:read({user_resource_id_seq, value}),
		mnesia:write(#user_resource_id_seq{id = 0})
        end,
    mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    {User, _, Resource} = To,
    F = fun() ->
		IDs = mnemosyne:eval(query [X.id || X <- table(user_resource),
						    X.user = User,
						    X.resource = Resource]
				     end),
		case IDs of
		    [] ->
			not_exists;
		    [ID] ->
			case mnesia:read({mysession, ID}) of
			    [] ->
				[Er] = mnesia:read({session, ID}),
				{remote, Er#session.node};
			    [El] ->
				{local, (El#mysession.info)#mysession_info.pid}
			end
		end
        end,
    case mnesia:transaction(F) of
	{atomic, {local, Pid}} ->
	    ?DEBUG("sending to process ~p~n", [Pid]),
	    % TODO
	    {xmlelement, Name, Attrs, Els} = Packet,
	    NewAttrs = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						  jlib:jid_to_string(To),
						  Attrs),
	    ejabberd_c2s:send_element(Pid, {xmlelement, Name, NewAttrs, Els}),
	    ?DEBUG("sended~n", []),
	    ok;
	{atomic, {remote, Node}} ->
	    ?DEBUG("sending to node ~p~n", [Node]),
	    {ejabberd_sm, Node} ! {route, From, To, Packet},
	    ok;
	{atomic, not_exists} ->
	    ?DEBUG("packet droped~n", []),
	    ok;
	{aborted, Reason} ->
	    ?DEBUG("delivery failed: ~p~n", [Reason]),
	    false
    end.

