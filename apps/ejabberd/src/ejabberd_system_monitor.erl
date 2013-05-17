%%%-------------------------------------------------------------------
%%% File    : ejabberd_system_monitor.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Description : Ejabberd watchdog
%%% Created : 21 Mar 2007 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%           Copyright (C) 2013        Erlang Solutions
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(ejabberd_system_monitor).
-author('alexey@process-one.net').

-behaviour(gen_event).

%% API
-export([add_handler/0, process_command/3, process_remote_command/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds this event handler to alarm_handler
%% @end
%%--------------------------------------------------------------------
-spec add_handler() ->  ok | {'EXIT', term()} | term().
add_handler() ->
    gen_event:add_handler(alarms_utils:event_manager(), ?MODULE, []).

process_command(From, To, Packet) ->
    case To of
    #jid{luser = <<"">>, lresource = <<"watchdog">>} ->
	    #xmlel{name = Name} = Packet,
	    case Name of
        <<"message">> ->
		    LFrom = jlib:jid_tolower(jlib:jid_remove_resource(From)),
		    case lists:member(LFrom, get_admin_jids()) of
			true ->
			    Body = xml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
			    spawn(fun() ->
					  process_flag(priority, high),
					  process_command1(From, To, binary_to_list(Body))
				  end),
			    stop;
			false ->
			    ok
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.

%%====================================================================
%% gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_) ->
    lists:foreach(
      fun(Host) ->
	      ejabberd_hooks:add(local_send_to_resource_hook, Host,
				 ?MODULE, process_command, 50)
      end, ?MYHOSTS),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({set_alarm, {large_heap, {Pid, Info}}}, State) ->
    spawn(fun() ->
                  process_flag(priority, high),
                  process_large_heap(Pid, Info)
          end),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call({get, large_heap}, State) ->
    {ok, get_large_heap(), State};
handle_call({set, large_heap, NewValue}, State) ->
    Options = alarms_server:get_monitor_options(),
    OldValue = proplists:get_value(large_heap, Options),
    NewOptions = lists:keystore(large_heap, 1, Options, {large_heap, NewValue}),
    alarms_server:set_monitor_options(NewOptions),
    {ok, {lh_changed, OldValue, get_large_heap()}, State};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_large_heap() ->
    Options = alarms_server:get_monitor_options(),
    proplists:get_value(large_heap, Options).

process_large_heap(Pid, Info) ->
    Host = ?MYNAME,
    case ejabberd_config:get_local_option(watchdog_admins) of
	JIDs when is_list(JIDs),
		  JIDs /= [] ->
	    DetailedInfo = detailed_info(Pid),
	    Body = io_lib:format(
		     "(~w) The process ~w is consuming too much memory:~n~p~n"
		     "~s",
		     [node(), Pid, Info, DetailedInfo]),
        From = jlib:make_jid(<<"">>, Host, <<"watchdog">>),
	    lists:foreach(
	      fun(S) ->
		      case jlib:binary_to_jid(list_to_binary(S)) of
			  error -> ok;
			  JID -> send_message(From, JID, Body)
		      end
	      end, JIDs);
	_ ->
	    ok
    end.

send_message(From, To, Body) ->
    BodyEl = #xmlel{name = <<"body">>,
                    children = [#xmlcdata{content = iolist_to_binary(Body)}]},
    El = #xmlel{name = <<"message">>,
                attrs = [{<<"type">>, <<"chat">>}],
                children = [BodyEl]},
    ejabberd_router:route(From, To, El).

-spec get_admin_jids() -> [#jid{}].
get_admin_jids() ->
    case ejabberd_config:get_local_option(watchdog_admins) of
	JIDs when is_list(JIDs) ->
	    lists:flatmap(
	      fun(S) ->
		      case jlib:binary_to_jid(list_to_binary(S)) of
			  error -> [];
			  JID -> [jlib:jid_tolower(JID)]
		      end
	      end, JIDs);
	_ ->
	    []
    end.

detailed_info(Pid) ->
    case process_info(Pid, dictionary) of
	{dictionary, Dict} ->
	    case lists:keysearch('$ancestors', 1, Dict) of
		{value, {'$ancestors', [Sup | _]}} ->
		    case Sup of
			ejabberd_c2s_sup ->
			    c2s_info(Pid);
			ejabberd_s2s_out_sup ->
			    s2s_out_info(Pid);
			ejabberd_service_sup ->
			    service_info(Pid);
			_ ->
			    detailed_info1(Pid)
		    end;
		_ ->
		    detailed_info1(Pid)
	    end;
	_ ->
	    detailed_info1(Pid)
    end.

detailed_info1(Pid) ->
    io_lib:format(
      "~p", [[process_info(Pid, current_function),
	      process_info(Pid, initial_call),
	      process_info(Pid, message_queue_len),
	      process_info(Pid, links),
	      process_info(Pid, heap_size),
	      process_info(Pid, stack_size)
	     ]]).

c2s_info(Pid) ->
    ["Process type: c2s",
     check_send_queue(Pid),
     "\n",
     io_lib:format("Command to kill this process: kill ~s ~w",
		   [atom_to_list(node()), Pid])].

s2s_out_info(Pid) ->
    FromTo = mnesia:dirty_select(
	       s2s, [{{s2s, '$1', Pid, '_'}, [], ['$1']}]),
    ["Process type: s2s_out",
     case FromTo of
	 [{From, To}] ->
	     "\n" ++ io_lib:format("S2S connection: from ~s to ~s",
				   [From, To]);
	 _ ->
	     ""
     end,
     check_send_queue(Pid),
     "\n",
     io_lib:format("Command to kill this process: kill ~s ~w",
		   [atom_to_list(node()), Pid])].

service_info(Pid) ->
    Routes = mnesia:dirty_select(
	       route, [{{route, '$1', Pid, '_'}, [], ['$1']}]),
    ["Process type: s2s_out",
     case Routes of
	 [Route] ->
	     "\nServiced domain: " ++ Route;
	 _ ->
	     ""
     end,
     check_send_queue(Pid),
     "\n",
     io_lib:format("Command to kill this process: kill ~s ~w",
		   [atom_to_list(node()), Pid])].

check_send_queue(Pid) ->
    case {process_info(Pid, current_function),
	  process_info(Pid, message_queue_len)} of
	{{current_function, MFA}, {message_queue_len, MLen}} ->
	    if
		MLen > 100 ->
		    case MFA of
			{prim_inet, send, 2} ->
			    "\nPossible reason: the process is blocked "
				"trying to send data over its TCP connection.";
			{M, F, A} ->
			    ["\nPossible reason: the process can't process "
			     "messages faster than they arrive.  ",
			     io_lib:format("Current function is ~w:~w/~w", [M, F, A])
			    ]
		    end;
		true ->
		    ""
	    end;
	_ ->
	    ""
    end.

process_command1(From, To, Body) ->
    process_command2(string:tokens(Body, " "), From, To).

process_command2(["kill", SNode, SPid], From, To) ->
    Node = list_to_atom(SNode),
    remote_command(Node, [kill, SPid], From, To);
process_command2(["showlh", SNode], From, To) ->
    Node = list_to_atom(SNode),
    remote_command(Node, [showlh], From, To);
process_command2(["setlh", SNode, NewValueString], From, To) ->
    Node = list_to_atom(SNode),
    NewValue = list_to_integer(NewValueString),
    remote_command(Node, [setlh, NewValue], From, To);
process_command2(["help"], From, To) ->
    send_message(To, From, help());
process_command2(_, From, To) ->
    send_message(To, From, help()).


help() ->
    "Commands:\n"
	"  kill <node> <pid>\n"
	"  showlh <node>\n"
	"  setlh <node> <integer>".


remote_command(Node, Args, From, To) ->
    Message =
	case rpc:call(Node, ?MODULE, process_remote_command, [Args]) of
	    {badrpc, Reason} ->
		io_lib:format("Command failed:~n~p", [Reason]);
	    Result ->
		Result
	end,
    send_message(To, From, Message).

process_remote_command([kill, SPid]) ->
    exit(list_to_pid(SPid), kill),
    "ok";
process_remote_command([showlh]) ->
    Res = gen_event:call(alarms_utils:event_manager(),
                         ?MODULE, {get, large_heap}),
    io_lib:format("Current large heap: ~p", [Res]);
process_remote_command([setlh, NewValue]) ->
    {lh_changed, OldLH, NewLH} =
        gen_event:call(alarms_utils:event_manager(),
                       ?MODULE, {set, large_heap, NewValue}),
    io_lib:format("Result of set large heap: ~p --> ~p", [OldLH, NewLH]);
process_remote_command(_) ->
    throw(unknown_command).
