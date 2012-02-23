%%%----------------------------------------------------------------------
%%% File    : gen_iq_handler.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IQ handler support
%%% Created : 22 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(gen_iq_handler).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/3,
	 add_iq_handler/6,
	 remove_iq_handler/3,
	 stop_iq_handler/3,
	 handle/7,
	 process_iq/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").

-record(state, {host,
		module,
		function}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Module, Function) ->
    gen_server:start_link(?MODULE, [Host, Module, Function], []).

add_iq_handler(Component, Host, NS, Module, Function, Type) ->
    case Type of
	no_queue ->
	    Component:register_iq_handler(Host, NS, Module, Function, no_queue);
	one_queue ->
	    {ok, Pid} = supervisor:start_child(ejabberd_iq_sup,
					       [Host, Module, Function]),
	    Component:register_iq_handler(Host, NS, Module, Function,
					  {one_queue, Pid});
	{queues, N} ->
	    Pids =
		lists:map(
		  fun(_) ->
			  {ok, Pid} = supervisor:start_child(
					ejabberd_iq_sup,
					[Host, Module, Function]),
			  Pid
		  end, lists:seq(1, N)),
	    Component:register_iq_handler(Host, NS, Module, Function,
					  {queues, Pids});
	parallel ->
	    Component:register_iq_handler(Host, NS, Module, Function, parallel)
    end.

remove_iq_handler(Component, Host, NS) ->
    Component:unregister_iq_handler(Host, NS).

stop_iq_handler(_Module, _Function, Opts) ->
    case Opts of
	{one_queue, Pid} ->
	    gen_server:call(Pid, stop);
	{queues, Pids} ->
	    lists:foreach(fun(Pid) ->
				  catch gen_server:call(Pid, stop)
			  end, Pids);
	_ ->
	    ok
    end.

handle(Host, Module, Function, Opts, From, To, IQ) ->
    case Opts of
	no_queue ->
	    process_iq(Host, Module, Function, From, To, IQ);
	{one_queue, Pid} ->
	    Pid ! {process_iq, From, To, IQ};
	{queues, Pids} ->
	    Pid = lists:nth(erlang:phash(now(), length(Pids)), Pids),
	    Pid ! {process_iq, From, To, IQ};
	parallel ->
	    spawn(?MODULE, process_iq, [Host, Module, Function, From, To, IQ]);
	_ ->
	    todo
    end.


process_iq(_Host, Module, Function, From, To, IQ) ->
    case catch Module:Function(From, To, IQ) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	ResIQ ->
	    if
		ResIQ /= ignore ->
		    ejabberd_router:route(To, From,
					  jlib:iq_to_xml(ResIQ));
		true ->
		    ok
	    end
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
init([Host, Module, Function]) ->
    {ok, #state{host = Host,
		module = Module,
		function = Function}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    Reply = ok,
    {stop, normal, Reply, State}.

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
handle_info({process_iq, From, To, IQ},
	    #state{host = Host,
		   module = Module,
		   function = Function} = State) ->
    process_iq(Host, Module, Function, From, To, IQ),
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
