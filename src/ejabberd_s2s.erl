%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : S2S connections manager
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

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(s2s, {fromto, pid, key}).


start_link() ->
    Pid = proc_lib:spawn_link(ejabberd_s2s, init, []),
    register(ejabberd_s2s, Pid),
    {ok, Pid}.

init() ->
    update_tables(),
    mnesia:create_table(s2s,[{ram_copies, [node()]},
			     {attributes, record_info(fields, s2s)}]),
    mnesia:add_table_copy(s2s, node(), ram_copies),
    mnesia:subscribe(system),
    loop().

loop() ->
    receive
	{closed_conection, FromTo} ->
	    remove_connection(FromTo),
	    loop();
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



remove_connection(FromTo) ->
    F = fun() ->
		mnesia:delete({s2s, FromTo})
	end,
    mnesia:transaction(F).



clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       s2s,
		       [{#s2s{pid = '$1', _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
        end,
    mnesia:transaction(F).

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
	    error
    end.

try_register(FromTo) ->
    Key = randoms:get_string(),
    F = fun() ->
		case mnesia:read({s2s, FromTo}) of
		    [] ->
			mnesia:write(#s2s{fromto = FromTo,
					  pid = self(),
					  key = Key}),
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
    case find_connection(From, To) of
	{atomic, Pid} when pid(Pid) ->
	    ?DEBUG("sending to process ~p~n", [Pid]),
	    % TODO
	    {xmlelement, Name, Attrs, Els} = Packet,
	    NewAttrs = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						  jlib:jid_to_string(To),
						  Attrs),
	    send_element(Pid, {xmlelement, Name, NewAttrs, Els}),
	    ok;
	{aborted, Reason} ->
	    ?DEBUG("delivery failed: ~p~n", [Reason]),
	    false
    end.

find_connection(From, To) ->
    #jid{lserver = MyServer} = From,
    #jid{lserver = Server} = To,
    FromTo = {MyServer, Server},
    case catch mnesia:dirty_read(s2s, FromTo) of
	{'EXIT', Reason} ->
	    {aborted, Reason};
	[] ->
	    ?DEBUG("starting new s2s connection~n", []),
	    Key = randoms:get_string(),
	    {ok, Pid} = ejabberd_s2s_out:start(MyServer, Server, {new, Key}),
	    F = fun() ->
			case mnesia:read({s2s, FromTo}) of
			    [El] ->
				El#s2s.pid;
			    [] ->
				mnesia:write(#s2s{fromto = FromTo,
						  pid = Pid,
						  key = Key}),
				Pid
			end
		end,
	    mnesia:transaction(F);
	[El] ->
	    {atomic, El#s2s.pid}
    end.


send_element(Pid, El) ->
    Pid ! {send_element, El}.


dirty_get_connections() ->
    mnesia:dirty_all_keys(s2s).


update_tables() ->
    case catch mnesia:table_info(s2s, attributes) of
	[fromto, node, key] ->
	    mnesia:transform_table(s2s, ignore, [fromto, pid, key]),
	    mnesia:clear_table(s2s);
	[fromto, pid, key] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end,
    case lists:member(local_s2s, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_s2s);
	false ->
	    ok
    end.

