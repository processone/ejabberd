%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_local).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, init/0]).

-export([register_iq_handler/3]).

-include("ejabberd.hrl").

-record(state, {mydomain, iqtable}).

start() ->
    register(ejabberd_local, spawn(ejabberd_local, init, [])),
    mod_register:start(),
    mod_roster:start(),
    mod_configure:start(),
    mod_disco:start(),
    mod_stats:start(),
    mod_vcard:start(),
    mod_offline:start(),
    mod_echo:start(),
    mod_private:start(),
    mod_time:start(),
    mod_version:start(),
    ok.

init() ->
    MyDomain = ?MYNAME,
    ejabberd_router:register_local_route(MyDomain),
    loop(#state{mydomain = MyDomain,
		iqtable = ets:new(local_iqtable, [named_table])}).

loop(State) ->
    receive
	{route, From, To, Packet} ->
	    do_route(State, From, To, Packet),
	    loop(State);
	{register_iq_handler, XMLNS, Module, Function} ->
	    ets:insert(State#state.iqtable, {XMLNS, Module, Function}),
	    mod_disco:register_feature(XMLNS),
	    loop(State)
    end.


do_route(State, From, To, Packet) ->
    ?DEBUG("local route~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    case To of
	{"", _, ""} ->
	    {xmlelement, Name, Attrs, Els} = Packet,
	    case Name of
		"iq" ->
		    process_iq(State, From, To, Packet);
		"message" ->
		    ok;
		"presence" ->
		    ok;
		_ ->
		    ok
	    end;
	{"", _, _} ->
	    Err = jlib:make_error_reply(Packet, "404", "Not Found"),
	    ejabberd_router ! {route,
			       {"", State#state.mydomain, ""}, From, Err},
	    ok;
	_ ->
	    ejabberd_sm ! {route, From, To, Packet}
    end.

process_iq(State, From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
	{iq, ID, Type, XMLNS, SubEl} ->
	    case jlib:is_iq_request_type(Type) of
		true ->
		    % TODO
		    case ets:lookup(State#state.iqtable, XMLNS) of
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
			[] ->
			    Err = jlib:make_error_reply(
				    Packet, "501", "Not Implemented"),
			    ejabberd_router ! {route,
					       {"", State#state.mydomain, ""},
					       From,
					       Err}
		    end;
		_ ->
		    % TODO
		    ok
	    end;
	reply ->
	    ok;
	_ ->
	    Err = jlib:make_error_reply(Packet, "400", "Bad Request"),
	    ejabberd_router ! {route,
			       {"", State#state.mydomain, ""}, From, Err},
	    ok
    end.

register_iq_handler(XMLNS, Module, Fun) ->
    ejabberd_local ! {register_iq_handler, XMLNS, Module, Fun}.
