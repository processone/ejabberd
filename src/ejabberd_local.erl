%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Route local packets
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_local).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start_link/0, init/0]).

-export([route/3,
	 register_iq_handler/3,
	 register_iq_handler/4,
	 unregister_iq_handler/1,
	 refresh_iq_handlers/0,
	 bounce_resource_packet/3
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(IQTABLE, local_iqtable).

start_link() ->
    register(ejabberd_local,
	     Pid = proc_lib:spawn_link(ejabberd_local, init, [])),
    {ok, Pid}.

init() ->
    ejabberd_router:register_route(?MYNAME, {apply, ?MODULE, route}),
    catch ets:new(?IQTABLE, [named_table, public]),
    ejabberd_hooks:add(local_send_to_resource_hook,
		       ?MODULE, bounce_resource_packet, 100),
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
	{register_iq_handler, XMLNS, Module, Function} ->
	    ets:insert(?IQTABLE, {XMLNS, Module, Function}),
	    catch mod_disco:register_feature(XMLNS),
	    loop();
	{register_iq_handler, XMLNS, Module, Function, Opts} ->
	    ets:insert(?IQTABLE, {XMLNS, Module, Function, Opts}),
	    catch mod_disco:register_feature(XMLNS),
	    loop();
	{unregister_iq_handler, XMLNS} ->
	    case ets:lookup(?IQTABLE, XMLNS) of
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:stop_iq_handler(Module, Function, Opts);
		_ ->
		    ok
	    end,
	    ets:delete(?IQTABLE, XMLNS),
	    catch mod_disco:unregister_feature(XMLNS),
	    loop();
	refresh_iq_handlers ->
	    lists:foreach(
	      fun(T) ->
		      case T of
			  {XMLNS, _Module, _Function, _Opts} ->
			      catch mod_disco:register_feature(XMLNS);
			  {XMLNS, _Module, _Function} ->
			      catch mod_disco:register_feature(XMLNS);
			  _ ->
			      ok
		      end
	      end, ets:tab2list(?IQTABLE)),
	    loop();
	_ ->
	    loop()
    end.


do_route(From, To, Packet) ->
    ?DEBUG("local route~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    if
	To#jid.luser /= "" ->
	    ejabberd_sm:route(From, To, Packet);
	To#jid.lresource == "" ->
	    {xmlelement, Name, Attrs, _Els} = Packet,
	    case Name of
		"iq" ->
		    process_iq(From, To, Packet);
		"message" ->
		    ok;
		"presence" ->
		    ok;
		_ ->
		    ok
	    end;
	true ->
	    {xmlelement, Name, Attrs, _Els} = Packet,
	    case xml:get_attr_s("type", Attrs) of
		"error" -> ok;
		"result" -> ok;
		_ ->
		    ejabberd_hooks:run(local_send_to_resource_hook,
				       [From, To, Packet])
	    end
	end.

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
	#iq{xmlns = XMLNS} ->
	    case ets:lookup(?IQTABLE, XMLNS) of
		[{_, Module, Function}] ->
		    ResIQ = Module:Function(From, To, IQ),
		    if
			ResIQ /= ignore ->
			    ejabberd_router:route(
			      To, From, jlib:iq_to_xml(ResIQ));
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

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end.

register_iq_handler(XMLNS, Module, Fun) ->
    ejabberd_local ! {register_iq_handler, XMLNS, Module, Fun}.

register_iq_handler(XMLNS, Module, Fun, Opts) ->
    ejabberd_local ! {register_iq_handler, XMLNS, Module, Fun, Opts}.

unregister_iq_handler(XMLNS) ->
    ejabberd_local ! {unregister_iq_handler, XMLNS}.

refresh_iq_handlers() ->
    ejabberd_local ! refresh_iq_handlers.

bounce_resource_packet(From, To, Packet) ->
    Err = jlib:make_error_reply(Packet, ?ERR_ITEM_NOT_FOUND),
    ejabberd_router:route(To, From, Err),
    stop.
