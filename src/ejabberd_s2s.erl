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

-export([start_link/0, init/0,
	 have_connection/1,
	 get_key/1,
	 try_register/1,
	 dirty_get_connections/0]).

-include_lib("mnemosyne/include/mnemosyne.hrl").
-include("ejabberd.hrl").

-record(s2s, {fromto, node, key}).
-record(local_s2s, {fromto, pid}).


start_link() ->
    {ok, proc_lib:spawn_link(ejabberd_s2s, init, [])}.

init() ->
    register(ejabberd_s2s, self()),
    mnesia:create_table(s2s,[{ram_copies, [node()]},
			     {attributes, record_info(fields, s2s)}]),
    mnesia:add_table_index(session, node),
    mnesia:create_table(local_s2s,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, local_s2s)}]),
    mnesia:add_table_copy(local_s2s, node(), ram_copies),
    mnesia:subscribe(system),
    loop().

loop() ->
    receive
	%{open_connection, User, Resource, From} ->
	%    replace_and_register_my_connection(User, Resource, From),
	%    replace_alien_connection(User, Resource),
	%    loop();
	{closed_conection, FromTo} ->
	    remove_connection(FromTo),
	    loop();
	%{replace, User, Resource} ->
	%    replace_my_connection(User, Resource),
	%    loop();
	{mnesia_system_event, {mnesia_down, Node}} ->
	    clean_table_from_bad_node(Node),
	    loop();
	{route, From, To, Packet} ->
	    case catch do_route(From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop();
	_ ->
	    loop()
    end.


%open_session(User, Resource) ->
%    ejabberd_s2s ! {open_session, User, Resource, self()}.
%
%close_session(User, Resource) ->
%    ejabberd_s2s ! {close_session, User, Resource}.


remove_connection(FromTo) ->
    F = fun() ->
		mnesia:delete({local_s2s, FromTo}),
		mnesia:delete({s2s, FromTo})
	end,
    mnesia:transaction(F).



clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:index_read(s2s, Node, #s2s.node),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(s2s, E, write)
			      end, Es)
        end,
    mnesia:transaction(F).

%have_connection(FromTo) ->
%    F = fun() ->
%		[E] = mnesia:read({s2s, FromTo})
%        end,
%    case mnesia:transaction(F) of
%	{atomic, _} ->
%	    true;
%	_ ->
%	    false
%    end.

have_connection(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
	[_] ->
	    true;
	_ ->
	    false
    end.

get_key(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
	[E] ->
	    E#s2s.key;
	_ ->
	    ""
    end.

try_register(FromTo) ->
    Key = randoms:get_string(),
    F = fun() ->
		case mnesia:read({s2s, FromTo}) of
		    [] ->
			mnesia:write(#s2s{fromto = FromTo,
					  node = node(),
					  key = Key}),
			mnesia:write(#local_s2s{fromto = FromTo,
						pid = self()}),
			{key, Key};
		    _ ->
			false
		end
        end,
    case mnesia:transaction(F) of
	{atomic, Res} ->
	    Res;
	_ ->
	    false
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, Packet) ->
    ?DEBUG("s2s manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    {_, MyServer, _} = From,
    {User, Server, Resource} = To,
    FromTo = {MyServer, Server},
    Key = randoms:get_string(),
    case find_connection(FromTo, Key) of
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
	    Pid = ejabberd_s2s_out:start(MyServer, Server, {new, Key}),
	    mnesia:transaction(fun() ->
				       mnesia:write(#local_s2s{fromto = FromTo,
							       pid = Pid})
			       end),
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

find_connection(FromTo, Key) ->
    F = fun() ->
		case mnesia:read({local_s2s, FromTo}) of
		    [] ->
			case mnesia:read({s2s, FromTo}) of
			    [Er] ->
				{remote, Er#s2s.node};
			    [] ->
				mnesia:write(#s2s{fromto = FromTo,
						  node = node(),
						  key = Key}),
				new
			end;
		    [El] ->
			{local, El#local_s2s.pid}
		end
        end,
    case catch mnesia:dirty_read({local_s2s, FromTo}) of
	{'EXIT', Reason} ->
	    {aborted, Reason};
	[] ->
	    case catch mnesia:dirty_read({s2s, FromTo}) of
		[Er] ->
		    {atomic, {remote, Er#s2s.node}};
		[] ->
		    mnesia:transaction(F)
	    end;
	[El] ->
	    {atomic, {local, El#local_s2s.pid}}
    end.


send_element(Pid, El) ->
    Pid ! {send_element, El}.


dirty_get_connections() ->
    mnesia:dirty_all_keys(s2s).

