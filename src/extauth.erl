%%%-------------------------------------------------------------------
%%% Created : 7 May 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------
-module(extauth).

-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).

-define(CALL_TIMEOUT, timer:seconds(30)).

%% API
-export([start/1, stop/1, reload/1, start_link/2]).
-export([check_password/3, set_password/3, try_register/3, remove_user/2,
	 remove_user/3, user_exists/2, check_certificate/3]).
-export([prog_name/1, pool_name/1, worker_name/2, pool_size/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").

-record(state, {port :: port(),
		prog :: string(),
		start_time :: integer(),
		os_pid :: integer() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
start(Host) ->
    extauth_sup:start(Host).

stop(Host) ->
    extauth_sup:stop(Host).

reload(Host) ->
    extauth_sup:reload(Host).

start_link(Name, Prog) ->
    ?GEN_SERVER:start_link({local, Name}, ?MODULE, [Prog], []).

check_password(User, Server, Password) ->
    call_port(Server, [<<"auth">>, User, Server, Password]).

check_certificate(User, Server, Certificate) ->
    call_port(Server, [<<"certauth">>, User, Server, Certificate]).

user_exists(User, Server) ->
    call_port(Server, [<<"isuser">>, User, Server]).

set_password(User, Server, Password) ->
    call_port(Server, [<<"setpass">>, User, Server, Password]).

try_register(User, Server, Password) ->
    call_port(Server, [<<"tryregister">>, User, Server, Password]).

remove_user(User, Server) ->
    call_port(Server, [<<"removeuser">>, User, Server]).

remove_user(User, Server, Password) ->
    call_port(Server, [<<"removeuser3">>, User, Server, Password]).

-spec prog_name(binary()) -> string() | undefined.
prog_name(Host) ->
    ejabberd_option:extauth_program(Host).

-spec pool_name(binary()) -> atom().
pool_name(Host) ->
    case ejabberd_option:extauth_pool_name(Host) of
	undefined ->
	    list_to_atom("extauth_pool_" ++ binary_to_list(Host));
	Name ->
	    list_to_atom("extauth_pool_" ++ binary_to_list(Name))
    end.

-spec worker_name(atom(), integer()) -> atom().
worker_name(Pool, N) ->
    list_to_atom(atom_to_list(Pool) ++ "_" ++ integer_to_list(N)).

-spec pool_size(binary()) -> pos_integer().
pool_size(Host) ->
    case ejabberd_option:extauth_pool_size(Host) of
	undefined -> misc:logical_processors();
	Size ->
	    Size
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Prog]) ->
    process_flag(trap_exit, true),
    {Port, OSPid} = start_port(Prog),
    Time = curr_time(),
    {ok, #state{port = Port, start_time = Time,
		prog = Prog, os_pid = OSPid}}.

handle_call({cmd, Cmd, EndTime}, _From, State) ->
    Timeout = EndTime - curr_time(),
    if Timeout > 0 ->
	    Port = State#state.port,
	    port_command(Port, Cmd),
	    receive
		{Port, {data, [0, N] = Data}} when N == 0; N == 1 ->
		    ?DEBUG("Received response from external authentication "
			   "program: ~p", [Data]),
		    {reply, decode_bool(N), State};
		{Port, Data} ->
		    ?ERROR_MSG("Received unexpected response from external "
			       "authentication program '~ts': ~p "
			       "(port = ~p, pid = ~w)",
			       [State#state.prog, Data, Port, State#state.os_pid]),
		    {reply, {error, unexpected_response}, State};
		{'EXIT', Port, Reason} ->
		    handle_info({'EXIT', Port, Reason}, State)
	    after Timeout ->
		    {stop, normal, State}
	    end;
       true ->
	    {noreply, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, _Reason}, #state{port = Port,
					    start_time = Time} = State) ->
    case curr_time() - Time of
	Diff when Diff < 1000 ->
	    ?ERROR_MSG("Failed to start external authentication program '~ts'",
		       [State#state.prog]),
	    {stop, normal, State};
	_ ->
	    ?ERROR_MSG("External authentication program '~ts' has terminated "
		       "unexpectedly (pid=~w), restarting via supervisor...",
		       [State#state.prog, State#state.os_pid]),
	    {stop, normal, State}
    end;
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    catch port_close(State#state.port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec curr_time() -> non_neg_integer().
curr_time() ->
    erlang:monotonic_time(millisecond).

-spec start_port(string()) -> {port(), integer() | undefined}.
start_port(Path) ->
    Port = open_port({spawn, Path}, [{packet, 2}]),
    link(Port),
    case erlang:port_info(Port, os_pid) of
	{os_pid, OSPid} ->
	    {Port, OSPid};
	undefined ->
	    {Port, undefined}
    end.

call_port(Server, Args) ->
    call_port(Server, Args, ?CALL_TIMEOUT).

call_port(Server, Args, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    Pool = pool_name(Server),
    PoolSize = pool_size(Server),
    I = p1_rand:round_robin(PoolSize),
    Cmd = str:join(Args, <<":">>),
    do_call(Cmd, I, I + PoolSize, Pool, PoolSize,
	    StartTime + Timeout, StartTime).

do_call(_, Max, Max, _, _, _, _) ->
    {error, disconnected};
do_call(Cmd, I, Max, Pool, PoolSize, EndTime, CurrTime) ->
    Timeout = EndTime - CurrTime,
    if Timeout > 0 ->
	    Proc = worker_name(Pool, (I rem PoolSize) + 1),
	    try ?GEN_SERVER:call(Proc, {cmd, Cmd, EndTime}, Timeout)
	    catch exit:{timeout, {?GEN_SERVER, call, _}} ->
		    {error, timeout};
		  exit:{_, {?GEN_SERVER, call, _}} ->
		    do_call(Cmd, I+1, Max, Pool, PoolSize, EndTime, curr_time())
	    end;
       true ->
	    {error, timeout}
    end.

decode_bool(0) -> false;
decode_bool(1) -> true.
