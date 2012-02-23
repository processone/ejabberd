%%%-------------------------------------------------------------------
%%% File    : epam.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : PAM authentication and accounting management
%%% Created : 5 Jul 2007 by Evgeniy Khramtsov <xram@jabber.ru>
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

-module(epam).
-author('xram@jabber.ru').

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include("ejabberd.hrl").

%% API
-export([start_link/0, start/0, stop/0]).
-export([authenticate/3, acct_mgmt/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(WARNING, "File ~p is world-wide executable. "
	"This is a possible security hole in your system. "
	"This file must be setted root on execution "
	"and only ejabberd must be able to read/execute it. "
	"You have been warned :)").

-define(PROCNAME, ?MODULE).
-define(CMD_AUTH, 0).
-define(CMD_ACCT, 1).
-record(state, {port}).

%%====================================================================
%% API
%%====================================================================
start() ->
    ChildSpec = {
      ?PROCNAME, {?MODULE, start_link, []},
      transient, 1000, worker, [?MODULE]
     },
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop() ->
    gen_server:call(?PROCNAME, stop),
    supervisor:terminate_child(ejabberd_sup, ?PROCNAME),
    supervisor:delete_child(ejabberd_sup, ?PROCNAME).

start_link() ->
    gen_server:start_link({local, ?PROCNAME}, ?MODULE, [], []).

authenticate(Srv, User, Pass) when is_list(Srv), is_list(User), is_list(Pass) ->
    gen_server:call(?PROCNAME, {authenticate, Srv, User, Pass}).

acct_mgmt(Srv, User) when is_list(Srv), is_list(User) ->
    gen_server:call(?PROCNAME, {acct_mgmt, Srv, User}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    FileName = filename:join(ejabberd:get_bin_path(), "epam"),
    case file:read_file_info(FileName) of
	{ok, Info} ->
	    Mode = Info#file_info.mode band 16#801,
	    if Mode == 16#801 ->
		    ?WARNING_MSG(?WARNING, [FileName]);
	       true -> ok
	    end,
	    Port = open_port({spawn, FileName}, [{packet, 2}, binary, exit_status]),
	    {ok, #state{port = Port}};
	{error, Reason} ->
	    ?ERROR_MSG("Can't open file ~p: ~p", [FileName, Reason]),
	    error
    end.

terminate(_Reason, #state{port = Port}) ->
    catch port_close(Port),
    ok.

handle_call({authenticate, Srv, User, Pass}, From, State) ->
    Port = State#state.port,
    Data = term_to_binary({?CMD_AUTH, From, {Srv, User, Pass}}),
    port_command(Port, Data),
    {noreply, State};
handle_call({acct_mgmt, Srv, User}, From, State) ->
    Port = State#state.port,
    Data = term_to_binary({?CMD_ACCT, From, {Srv, User}}),
    port_command(Port, Data),
    {noreply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_info({Port, {data, Data}}, #state{port=Port} = State) ->
    case binary_to_term(Data) of
	{Cmd, To, Reply} when Cmd==?CMD_AUTH; Cmd==?CMD_ACCT ->
	    gen_server:reply(To, Reply);
	Err ->
	    ?ERROR_MSG("Got invalid reply from ~p: ~p", [Port, Err])
    end,
    {noreply, State};
handle_info({Port, {exit_status, _}}, #state{port=Port} = State) ->
    %% We can restart the port here, but, I think, it is not a good idea,
    %% since we can run into infinity loop. So let the supervisor restart us.
    {stop, port_died, State};
handle_info(Msg, State) ->
    ?WARNING_MSG("Got unexpected message: ~p", [Msg]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
