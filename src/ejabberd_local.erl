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

-export([start_link/0, init/0]).

-export([register_iq_handler/3,
	 register_iq_handler/4,
	 unregister_iq_handler/1
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {mydomain, iqtable}).

start_link() ->
    register(ejabberd_local,
	     Pid = proc_lib:spawn_link(ejabberd_local, init, [])),
    {ok, Pid}.

init() ->
    MyDomain = ?MYNAME,
    ejabberd_router:register_local_route(MyDomain),
    catch ets:new(local_iqtable, [named_table, public]),
    loop(#state{mydomain = MyDomain,
		iqtable = local_iqtable}).

loop(State) ->
    receive
	{route, From, To, Packet} ->
	    do_route(State, From, To, Packet),
	    loop(State);
	{register_iq_handler, XMLNS, Module, Function} ->
	    ets:insert(State#state.iqtable, {XMLNS, Module, Function}),
	    mod_disco:register_feature(XMLNS),
	    loop(State);
	{register_iq_handler, XMLNS, Module, Function, Opts} ->
	    ets:insert(State#state.iqtable, {XMLNS, Module, Function, Opts}),
	    mod_disco:register_feature(XMLNS),
	    loop(State);
	{unregister_iq_handler, XMLNS} ->
	    case ets:lookup(State#state.iqtable, XMLNS) of
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:stop_iq_handler(Module, Function, Opts);
		_ ->
		    ok
	    end,
	    ets:delete(State#state.iqtable, XMLNS),
	    mod_disco:unregister_feature(XMLNS),
	    loop(State);
	_ ->
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
	    Err = jlib:make_error_reply(Packet, ?ERR_JID_NOT_FOUND),
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
			[{_, Module, Function, Opts}] ->
			    gen_iq_handler:handle(Module, Function, Opts,
						  From, To, IQ);
			[] ->
			    Err = jlib:make_error_reply(
				    Packet, "501", "Not Implemented"),
			    ejabberd_router ! {route,
					       {"", State#state.mydomain, ""},
					       From,
					       Err}
		    end;
		_ ->
		    ok
	    end;
	reply ->
	    ok;
	_ ->
	    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	    ejabberd_router ! {route,
			       {"", State#state.mydomain, ""}, From, Err},
	    ok
    end.

register_iq_handler(XMLNS, Module, Fun) ->
    ejabberd_local ! {register_iq_handler, XMLNS, Module, Fun}.

register_iq_handler(XMLNS, Module, Fun, Opts) ->
    ejabberd_local ! {register_iq_handler, XMLNS, Module, Fun, Opts}.

unregister_iq_handler(XMLNS) ->
    ejabberd_local ! {unregister_iq_handler, XMLNS}.

