%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_s2s).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, init/0, open_session/2, close_session/2,
	 have_connection/1,
	 get_key/1]).

-include_lib("mnemosyne/include/mnemosyne.hrl").
-include("ejabberd.hrl").

-record(s2s, {server, node, key}).
-record(mys2s, {server, pid}).


start() ->
    spawn(ejabberd_s2s, init, []).

init() ->
    register(ejabberd_s2s, self()),
    mnesia:create_table(s2s,[{ram_copies, [node()]},
			     {attributes, record_info(fields, s2s)}]),
    mnesia:add_table_index(session, node),
    mnesia:create_table(mys2s,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, mys2s)}]),
    mnesia:subscribe(system),
    loop().

loop() ->
    receive
	%{open_connection, User, Resource, From} ->
	%    replace_and_register_my_connection(User, Resource, From),
	%    replace_alien_connection(User, Resource),
	%    loop();
	{closed_conection, Server} ->
	    remove_connection(Server),
	    loop();
	%{replace, User, Resource} ->
	%    replace_my_connection(User, Resource),
	%    loop();
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
    ejabberd_s2s ! {open_session, User, Resource, self()}.

close_session(User, Resource) ->
    ejabberd_s2s ! {close_session, User, Resource}.

%replace_alien_connection(User, Resource) ->
%    F = fun() ->
%		[ID] = mnemosyne:eval(query [X.id || X <- table(user_resource),
%						     X.user = User,
%						     X.resource = Resource]
%				      end),
%		Es = mnesia:read({session, ID}),
%		mnesia:write(#session{id = ID, node = node()}),
%		Es
%        end,
%    case mnesia:transaction(F) of
%	{atomic, Rs} ->
%	    lists:foreach(
%	      fun(R) ->
%		      if R#session.node /= node() ->
%			      {ejabberd_s2s, R#session.node} !
%				  {replace, User, Resource};
%			 true ->
%			      ok
%		      end
%	      end, Rs);
%	_ ->
%	    false
%    end.
%
%
%replace_my_connection(User, Resource) ->
%    F = fun() ->
%		[ID] = mnemosyne:eval(query [X.id || X <- table(user_resource),
%						     X.user = User,
%						     X.resource = Resource]
%				      end),
%
%		Es = mnesia:read({mysession, ID}),
%		mnesia:delete({mysession, ID}),
%		Es
%        end,
%    case mnesia:transaction(F) of
%	{atomic, Rs} ->
%	    lists:foreach(
%	      fun(R) ->
%		      (R#mysession.info)#mysession_info.pid ! replaced
%	      end, Rs);
%	_ ->
%	    false
%    end.

remove_connection(Server) ->
    F = fun() ->
		mnesia:delete({mys2s, Server}),
		mnesia:delete({s2s, Server})
	end,
    mnesia:transaction(F).

%replace_and_register_my_connection(User, Resource, Pid) ->
%    F = fun() ->
%		IDs = mnemosyne:eval(query [X.id || X <- table(user_resource),
%						    X.user = User,
%						    X.resource = Resource]
%				     end),
%
%		ID = case IDs of
%			 [Id] -> Id;
%			 [] ->
%			     [CurID] = 
%				 mnemosyne:eval(
%				   query [X.id ||
%					     X <- table(user_resource_id_seq)]
%				     end),
%			     mnesia:write(
%			       #user_resource_id_seq{id = CurID + 1}),
%			     mnesia:write(
%			       #user_resource{id = CurID,
%					      user = User,
%					      resource = Resource}),
%			     CurID
%		     end,
%		Es = mnesia:read({mysession, ID}),
%		mnesia:write(#mysession{id = ID,
%					info = #mysession_info{pid = Pid}}),
%		Es
%        end,
%    case mnesia:transaction(F) of
%	{atomic, Rs} ->
%	    lists:foreach(
%	      fun(R) ->
%		      (R#mysession.info)#mysession_info.pid ! replaced
%	      end, Rs);
%	_ ->
%	    false
%    end.


clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:index_read(s2s, Node, #s2s.node),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(s2s, E, write)
			      end, Es)
        end,
    mnesia:transaction(F).

have_connection(Server) ->
    F = fun() ->
		[E] = mnesia:read({s2s, Server})
        end,
    case mnesia:transaction(F) of
	{atomic, _} ->
	    true;
	_ ->
	    false
    end.

get_key(Server) ->
    F = fun() ->
		[E] = mnesia:read({s2s, Server}),
		E
        end,
    case mnesia:transaction(F) of
	{atomic, E} ->
	    E#s2s.key;
	_ ->
	    ""
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, Packet) ->
    ?DEBUG("s2s manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    {User, Server, Resource} = To,
    Key = lists:flatten(io_lib:format("~p", [random:uniform(65536*65536)])),
    F = fun() ->
		case mnesia:read({mys2s, Server}) of
		    [] ->
			case mnesia:read({s2s, Server}) of
			    [Er] ->
				{remote, Er#s2s.node};
			    [] ->
				% TODO
				mnesia:write(#s2s{server = Server,
						  node = node(),
						  key = Key}),
				new
			end;
		    [El] ->
			{local, El#mys2s.pid}
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
	    send_element(Pid, {xmlelement, Name, NewAttrs, Els}),
	    ok;
	{atomic, {remote, Node}} ->
	    ?DEBUG("sending to node ~p~n", [Node]),
	    {ejabberd_s2s, Node} ! {route, From, To, Packet},
	    ok;
	{atomic, new} ->
	    ?DEBUG("starting new s2s connection~n", []),
	    Pid = ejabberd_s2s_out:start(Server, {new, Key}),
	    mnesia:transaction(fun() -> mnesia:write(#mys2s{server = Server,
							    pid = Pid}) end),
	    {xmlelement, Name, Attrs, Els} = Packet,
	    NewAttrs = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						  jlib:jid_to_string(To),
						  Attrs),
	    send_element(Pid, {xmlelement, Name, NewAttrs, Els}),
	    ok;
	{atomic, not_exists} ->
	    ?DEBUG("packet droped~n", []),
	    ok;
	{aborted, Reason} ->
	    ?DEBUG("delivery failed: ~p~n", [Reason]),
	    false
    end.

send_element(Pid, El) ->
    Pid ! {send_element, El}.
