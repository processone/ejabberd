%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 22 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%%-------------------------------------------------------------------
%%% File    : ts_client_proxy.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : handle communication with client and server.
%%%
%%% Created : 22 Dec 2003 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------


-module(ts_client_proxy).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_profile.hrl").
-include("ts_recorder.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, peername/1, send/3]).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the gen_server with the socket given by the listener
%%--------------------------------------------------------------------
start(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Socket]) ->
    ?LOGF("Parent proxy: ~p~n",[?config(parent_proxy)],?DEB),
    {ok, #proxy{clientsock=Socket, plugin=?config(plugin),
                parent_proxy=?config(parent_proxy)}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
% client data, parse and send it to the server.
handle_info({tcp, ClientSock, String}, State=#proxy{plugin=Plugin})
  when ClientSock == State#proxy.clientsock ->
    ts_utils:inet_setopts(tcp, ClientSock,[{active, once}]),
    {ok, NewState}  = Plugin:parse(State,ClientSock,State#proxy.serversock,String),
    {noreply, NewState, ?lifetime};

% server data, send it to the client
handle_info({Type, ServerSock, String}, State=#proxy{plugin=Plugin})
  when ServerSock == State#proxy.serversock, ((Type == tcp) or (Type == ssl)) ->
    ts_utils:inet_setopts(Type, ServerSock,[{active, once}]),
    ?LOGF("Received data from server: ~s~n",[String],?DEB),
    {ok,NewString} = Plugin:rewrite_serverdata(String),
    send(State#proxy.clientsock, NewString, Plugin),
    case regexp:first_match(NewString, "[cC]onnection: [cC]lose") of
        nomatch ->
            {noreply, State, ?lifetime};
        _ ->
            ?LOG("Connection close received,set close=true~n",?DEB),
            {noreply, State#proxy{close=true}, ?lifetime}
    end;

%%%%%%%%%%%% Errors and termination %%%%%%%%%%%%%%%%%%%

% Log who did close the connection, and exit.
handle_info({Msg, Socket}, #proxy{ serversock = Socket, close = true }) when Msg==tcp_close; Msg==ssl_closed ->
    ?LOG("socket closed by server, close client socket also~n",?INFO),
    {stop, normal, ?lifetime};% close ask by server in previous request
handle_info({Msg,Socket},State=#proxy{http_version = HTTPVersion,
                                      serversock = Socket
                                     }) when Msg==tcp_close; Msg==ssl_closed ->
    ?LOG("socket closed by server~n",?INFO),
    case HTTPVersion of
        "HTTP/1.0" ->
            {stop, normal, ?lifetime};%Disconnect client if it requires HTTP/1.0
        _ ->
            {noreply, State#proxy{serversock=undefined}, ?lifetime}
    end;

handle_info({Msg, _Socket}, State) when Msg == tcp_closed;
                                       Msg == ssl_closed->
    ?LOG("socket closed by client~n",?INFO),
    {stop, normal, State};

% Log properly who caused an error, and exit.
handle_info({Msg, Socket, Reason}, State) when Msg == tcp_error;
                                               Msg == ssl_error ->
    ?LOGF("error on socket ~p ~p~n",[Socket,Reason],?ERR),
    {stop, {error, sockname(Socket,State), Reason}, State};

handle_info(timeout, State) ->
    {stop, timeout, State};

handle_info(Info, State) ->
    ?LOGF("Uknown data  ~p~n",[Info],?ERR),
    {stop, unknown, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
%    ts_proxy_recorder:dorecord(endsession),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% Function: sockname/2
%%           sockname(Socket,State)
%% Purpose: decides whether some socket is the client or the server
%% Description: State contains two fields, "serversock" and "clientsock".
%%              This function searches Socket among the two, and returns
%%              an appropriate atom among 'server', 'client' and 'unknown'.
%% Returns: Sockname
%% Types:   Sockname -> server | client | unknown
%%          State -> state_record()

sockname(Socket,#proxy{serversock=Socket})-> server;
sockname(Socket,#proxy{clientsock=Socket})-> client;
sockname(_Socket,_State)-> unknown.


peername({sslsocket,A,B})-> ssl:peername({sslsocket,A,B});
peername(Socket)         -> prim_inet:peername(Socket).


send(_,[],_) -> ok; % no data
send({sslsocket,A,B},String, Plugin) ->
    ?LOGF("Received data to send to an ssl socket ~p, using plugin ~p ~n", [String,Plugin],?DEB),
    {ok, RealString } = Plugin:rewrite_ssl({request,String}),
    ?LOGF("Sending data to ssl socket ~p ~p (~p)~n", [A, B, RealString],?DEB),
    ssl:send({sslsocket,A,B}, RealString);
send(undefined,_,_) ->
    ?LOG("No socket ! Error ~n",?CRIT),
    erlang:error(error_no_socket_open);
send(Socket,String,_) ->
    gen_tcp:send(Socket,String).


