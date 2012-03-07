%%%-------------------------------------------------------------------
%%% File    : ejabberd_system_monitor.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Description : Ejabberd watchdog
%%% Created : 21 Mar 2007 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

-behaviour(gen_server).

%% API
-export([start_link/0,
	 process_command/3,
	 process_remote_command/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    LH = case ejabberd_config:get_local_option(watchdog_large_heap) of
	I when is_integer(I) -> I;
	_ -> 1000000
end,
    Opts = [{large_heap, LH}],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

process_command(From, To, Packet) ->
    case {exmpp_jid:prep_node(To), exmpp_jid:prep_resource(To) } of
	  {undefined, <<"watchdog">>} ->
	    case Packet#xmlel.name of
		'message' ->
		    case lists:any(fun(J) -> 
                            exmpp_jid:compare(J,From) 
                           end, get_admin_jids()) of
			true ->
			    Body = exmpp_xml:get_path(
				     Packet, [{element, 'body'}, cdata_as_list]),
			    spawn(fun() ->
					  process_flag(priority, high),
					  process_command1(From, To, Body)
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
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Opts) ->
    LH = proplists:get_value(large_heap, Opts),
    process_flag(priority, high),
    erlang:system_monitor(self(), [{large_heap, LH}]),
    lists:foreach(
      fun(Host) ->
	      ejabberd_hooks:add(local_send_to_resource_hook, 
                  list_to_binary(Host),
				 ?MODULE, process_command, 50)
      end, ?MYHOSTS),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, large_heap}, _From, State) ->
    {reply, get_large_heap(), State};
handle_call({set, large_heap, NewValue}, _From, State) ->
    MonSettings = erlang:system_monitor(self(), [{large_heap, NewValue}]),
    OldLH = get_large_heap(MonSettings),
    NewLH = get_large_heap(),
    {reply, {lh_changed, OldLH, NewLH}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

get_large_heap() ->
    MonSettings = erlang:system_monitor(),
    get_large_heap(MonSettings).
get_large_heap(MonSettings) ->
    {_MonitorPid, Options} = MonSettings,
    proplists:get_value(large_heap, Options).

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({monitor, Pid, large_heap, Info}, State) ->
    spawn(fun() ->
		  process_flag(priority, high),
		  process_large_heap(Pid, Info)
	  end),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

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
	    From = exmpp_jid:make(undefined, Host, <<"watchdog">>),
	    lists:foreach(
	      fun(S) ->
		      try
			  JID = exmpp_jid:parse(S),
			  send_message(From, JID, Body)
		      catch
			  _ ->
			      ok
		      end
	      end, JIDs);
	_ ->
	    ok
    end.

send_message(From, To, Body) ->
    ejabberd_router:route(
      From, To,
      exmpp_message:chat(lists:flatten(Body))).

get_admin_jids() ->
    case ejabberd_config:get_local_option(watchdog_admins) of
	JIDs when is_list(JIDs) ->
	    lists:flatmap(
	      fun(S) ->
		      try
			  JID = exmpp_jid:parse(S),
			  [{exmpp_jid:prep_node(JID), 
                exmpp_jid:prep_domain(JID), 
                exmpp_jid:prep_resource(JID)}]
		      catch
			  _ ->
			      []
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
	      process_info(Pid, dictionary),
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
			     io_lib:format("Current function is ~w:~w/~w",
					   [M, F, A])
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
    Res = gen_server:call(ejabberd_system_monitor, {get, large_heap}),
    io_lib:format("Current large heap: ~p", [Res]);
process_remote_command([setlh, NewValue]) ->
    {lh_changed, OldLH, NewLH} = gen_server:call(ejabberd_system_monitor, {set, large_heap, NewValue}),
    io_lib:format("Result of set large heap: ~p --> ~p", [OldLH, NewLH]);
process_remote_command(_) ->
    throw(unknown_command).

