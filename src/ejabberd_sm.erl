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

-export([start/0, init/0, open_session/2, close_session/2,
	get_user_resources/1]).

-include_lib("mnemosyne/include/mnemosyne.hrl").
-include("ejabberd.hrl").

-record(session, {ur, user, node}).
-record(mysession, {ur, pid}).


start() ->
    spawn(ejabberd_sm, init, []).

init() ->
    register(ejabberd_sm, self()),
    mnesia:create_table(session,[{ram_copies, [node()]},
				 {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, user),
    mnesia:add_table_index(session, node),
    mnesia:create_table(mysession,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, mysession)}]),
    mnesia:subscribe(system),
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
		UR = {User, Resource},
		Es = mnesia:read({session, UR}),
		mnesia:write(#session{ur = UR, user = User, node = node()}),
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
		UR = {User, Resource},
		Es = mnesia:read({mysession, UR}),
		mnesia:delete({mysession, UR}),
		Es
        end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:foreach(
	      fun(R) ->
		      R#mysession.pid ! replaced
	      end, Rs);
	_ ->
	    false
    end.

remove_connection(User, Resource) ->
    F = fun() ->
		UR = {User, Resource},
		mnesia:delete({mysession, UR}),
		mnesia:delete({session, UR})
        end,
    mnesia:transaction(F).

replace_and_register_my_connection(User, Resource, Pid) ->
    F = fun() ->
		UR = {User, Resource},
		Es = mnesia:read({mysession, UR}),
		mnesia:write(#mysession{ur = UR, pid = Pid}),
		Es
        end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:foreach(
	      fun(R) ->
		      R#mysession.pid ! replaced
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
    F = fun() ->
		UR = {User, Resource},
		Sess = mnesia:read({session, UR}),
		case Sess of
		    [] ->
			not_exists;
		    [Ses] ->
			case mnesia:read({mysession, UR}) of
			    [] ->
				{remote, Ses#session.node};
			    [El] ->
				{local, El#mysession.pid}
			end
		end
        end,
    {xmlelement, Name, Attrs, Els} = Packet,
    case Resource of
	"" ->
	    % TODO
	    case Name of
		"presence" ->
		    {FU, FS, FR} = From,
		    Pass = case xml:get_attr_s("type", Attrs) of
			       "subscribe" ->
				   mod_roster:in_subscription(User,
							      {FU, FS, ""},
							      subscribe);
			       "subscribed" ->
				   mod_roster:in_subscription(User,
							      {FU, FS, ""},
							      subscribed);
			       "unsubscribe" ->
				   mod_roster:in_subscription(User,
							      {FU, FS, ""},
							      unsubscribe);
			       "unsubscribed" ->
				   mod_roster:in_subscription(User,
							      {FU, FS, ""},
							      unsubscribed);
			       _ ->
				   true
			   end,
		    if Pass ->
			    LFrom = jlib:jid_tolower(From),
			    LUser = jlib:tolower(User),
			    LServer = jlib:tolower(Server),
			    lists:foreach(
			      fun(R) ->
				      if LFrom /= {LUser, LServer, R} ->
					      ejabberd_sm ! {route,
							     From,
							     {User, Server, R},
							     Packet};
					 true ->
					      ok
				      end
			      end, get_user_resources(User));
		       true ->
			    ok
		    end;
		"message" ->
		    % TODO
		    ok;
		"iq" ->
		    % TODO
		    ok;
		_ ->
		    ok
	    end;
	_ ->
	    case mnesia:transaction(F) of
		{atomic, {local, Pid}} ->
		    ?DEBUG("sending to process ~p~n", [Pid]),
		    % TODO
		    Pid ! {route, From, To, Packet},
		    %NewAttrs =
		    %    jlib:replace_from_to_attrs(jlib:jid_to_string(From),
		    %    			   jlib:jid_to_string(To),
		    %    			   Attrs),
		    %ejabberd_c2s:send_element(
		    %  Pid, {xmlelement, Name, NewAttrs, Els}),
		    %?DEBUG("sended~n", []),
		    ok;
		{atomic, {remote, Node}} ->
		    ?DEBUG("sending to node ~p~n", [Node]),
		    {ejabberd_sm, Node} ! {route, From, To, Packet},
		    ok;
		{atomic, not_exists} ->
		    % TODO
		    ?DEBUG("packet droped~n", []),
		    ok;
		{aborted, Reason} ->
		    ?DEBUG("delivery failed: ~p~n", [Reason]),
		    false
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_user_resources(User) ->
    F = fun() ->
		mnemosyne:eval(query [X.ur || X <- table(session),
					      X.user = User]
			       end)
	end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:map(fun(R) -> element(2, R) end, Rs);
	{aborted, Reason} ->
	    ?DEBUG("delivery failed: ~p~n", [Reason]),
	    []
    end.


