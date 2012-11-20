%% Copyright (C) 2003 Joakim Grebenö <jocke@gleipnir.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(tcp_serv).
-vsn("1.13").
-author('jocke@gleipnir.com').
-export([start_link/1, start_link/2, stop/1, stop/2]).
-export([init/2, start_session/3]).
-export([system_continue/3, system_terminate/4]).

-include("log.hrl").

-record(state, {
	  %% int()
	  max_sessions,
	  %% {M, F, A}
	  %% M = F = atom()
	  %% A = [term()]
	  session_handler,
	  %% [pid()]
	  session_list,
	  %% socket()
	  listen_socket,
	  %% pid()
	  parent,
	  %% term()
	  debug_info
	 }).

%% Exported: start_link/{1,2}

start_link(Args) -> start_link(Args, 60000).
    
start_link(Args, Timeout) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), Args]),
    receive
	{Pid, started} -> {ok, Pid};
	{Pid, Reason} -> {error, Reason}
    after Timeout -> {error, timeout}
    end.

%% Exported: stop/{1,2}

stop(Pid) -> stop(Pid, 15000).

stop(Pid, Timeout) ->
    Pid ! {self(), stop}, 
    receive
	{Pid, Reply} -> Reply
    after
	Timeout -> {error, timeout}
    end.

%% Exported: init/2

init(Parent, [Port, MaxSessions, OptionList, SessionHandler]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, OptionList) of
	{ok, ListenSocket} ->
	    self() ! start_session,
	    Parent ! {self(), started},
	    loop(#state{max_sessions = MaxSessions,
			session_handler = SessionHandler,
			session_list = [],
			listen_socket = ListenSocket,
			parent = Parent});
	Reason -> Parent ! {self(), {not_started, Reason}}
    end.

loop(#state{session_list = SessionList, listen_socket = ListenSocket,
	    parent = Parent} = State) ->
    receive
	{From, stop} ->
	    cleanup(State),
	    From ! {self(), ok};
	start_session when length(SessionList) > State#state.max_sessions ->
	    timer:sleep(5000),
	    self() ! start_session,
	    loop(State);
	start_session ->
	    A = [self(), State#state.session_handler, ListenSocket],
	    Pid = proc_lib:spawn_link(?MODULE, start_session, A),
	    loop(State#state{session_list = [Pid|SessionList]});
        {'EXIT', Parent, Reason} ->
	    cleanup(State),
            exit(Reason);
	{'EXIT', Pid, Reason} ->
	    case lists:member(Pid, SessionList) of
		true ->
		    PurgedSessionList = lists:delete(Pid, SessionList),
		    loop(State#state{session_list = PurgedSessionList});
		false ->
		    ?ERROR_LOG({ignoring, {'EXIT', Pid, Reason}}),
		    loop(State)
	    end;
	{system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE,
				  State#state.debug_info, State);	
	UnknownMessage ->
	    ?ERROR_LOG({unknown_message, UnknownMessage}),
	    loop(State)
    end.

cleanup(State) -> gen_tcp:close(State#state.listen_socket).

%% Exported: start_seesion/3

start_session(Parent, {M, F, A}, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    Parent ! start_session,
	    case apply(M, F, [Socket|A]) of
		ok -> gen_tcp:close(Socket);
		{error, closed} -> ok;
		{error, Reason} ->
		    ?ERROR_LOG({M, F, Reason}),
		    gen_tcp:close(Socket)
	    end;
	{error, Reason} ->
	    timer:sleep(5000),
	    Parent ! start_session
    end.

%% Exported: system_continue/3

system_continue(Parent, DebugInfo, State) ->
    loop(State#state{parent = Parent, debug_info = DebugInfo}).

%% Exported: system_terminate/3

system_terminate(Reason, Parent, DebugInfo, State) ->
    cleanup(State),
    exit(Reason).
