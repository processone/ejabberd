%%%
%%%  Copyright (C) Nicolas Niclausse 2005
%%%
%%%  Author : Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%  Created: 09 Nov 2005 by Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
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

-module(ts_proxy_pgsql).
-vc('$Id$ ').
-author('Nicolas.Niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_pgsql.hrl").
-include("ts_recorder.hrl").


-export([parse/4, record_request/2, socket_opts/0, gettype/0]).

-export([rewrite_serverdata/1]).
-export([rewrite_ssl/1]).

%%--------------------------------------------------------------------
%% Func: socket_opts/0
%%--------------------------------------------------------------------
socket_opts() -> [].

%%--------------------------------------------------------------------
%% Func: gettype/0
%%--------------------------------------------------------------------
gettype() -> "ts_pgsql".

%%--------------------------------------------------------------------
%% Func: rewrite_serverdata/1
%%--------------------------------------------------------------------
rewrite_serverdata(Data)->{ok, Data}.

%%--------------------------------------------------------------------
%% Func: rewrite_ssl/1
%%--------------------------------------------------------------------
rewrite_ssl(Data)->{ok, Data}.

%%--------------------------------------------------------------------
%% Func: parse/4
%% Purpose: parse PGSQL request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
parse(State=#proxy{parse_status=Status},_,_SSocket,String=[0,0,0,8,4,210,22,47]) when Status==new ->
    ?LOG("SSL req: ~n",?DEB),
    Socket = connect(undefined),
    ts_client_proxy:send(Socket, String, ?MODULE),
    {ok, State#proxy{buffer=[],serversock = Socket }};
parse(State=#proxy{parse_status=Status},_,ServerSocket,String) when Status==new ->
    Data = list_to_binary(String),
    <<PacketSize:32/integer, StartupPacket/binary>> = Data,
    ?LOGF("Received data from client: size=~p [~p]~n",[PacketSize, StartupPacket],?DEB),
    <<ProtoMaj:16/integer, ProtoMin:16/integer, Data2/binary>> = StartupPacket,
    ?LOGF("Received data from client: proto maj=~p min=~p~n",[ProtoMaj, ProtoMin],?DEB),
    Res= pgsql_util:split_pair_rec(Data2),
    Req = get_db_user(Res),
    ?LOGF("Received data from client: split = ~p~n",[Res],?DEB),
    ts_proxy_recorder:dorecord({Req#pgsql_request{type=connect}}),
    Socket = connect(ServerSocket),
    ts_client_proxy:send(Socket, Data, ?MODULE),
    {ok, State#proxy{parse_status=open, buffer=[], serversock = Socket} };
parse(State=#proxy{},_,ServerSocket,String) ->
    NewString = lists:append(State#proxy.buffer, String),
    Data = list_to_binary(NewString),
    ?LOGF("Received data from client: ~p~n",[Data],?DEB),
    Reply = case process_data(Data) of
                more ->
                    ?LOG("need more~n",?DEB),
                    {ok, State#proxy{buffer=NewString} };
                {ok, {sql, SQL}, _Tail } ->
                    SQLStr= binary_to_list(SQL),
                    ?LOGF("sql = ~s~n",[SQLStr],?DEB),
                    ts_proxy_recorder:dorecord({#pgsql_request{type=sql, sql=SQLStr}}),
                    {ok, State#proxy{buffer=[]}};
                {ok, terminate, _Tail } ->
                    ts_proxy_recorder:dorecord({#pgsql_request{type=close}}),
                    {ok, State#proxy{buffer=[]}};
                {ok, {password, Password}, _Tail } ->
                    PwdStr= binary_to_list(Password),
                    ?LOGF("password = ~s~n",[PwdStr],?DEB),
                    ts_proxy_recorder:dorecord({#pgsql_request{type=authenticate, passwd=PwdStr}}),
                    {ok, State#proxy{buffer=[]}}
            end,
    ts_client_proxy:send(ServerSocket, String, ?MODULE),
    Reply.

process_data(<<Code:8/integer, Size:4/integer-unit:8, Tail/binary>>) ->
    ?LOGF("PGSQL: received [~p]  size=~p Pckt size= ~p ~n",[Code, Size, size(Tail)],?DEB),
    RealSize = Size-4,
    case RealSize =< size(Tail) of
        true ->
            << Packet:RealSize/binary, Data/binary >> = Tail,
            {ok, Pair} = decode_packet(Code, Packet),
            ?LOGF("PGSQL: Pair=~p ~n",[Pair],?DEB),
            {ok, Pair, Data };
        false -> more
    end.

get_db_user(Arg) ->
    get_db_user(Arg,#pgsql_request{}).

get_db_user([], Req)-> Req;
get_db_user([{"user",User}| Rest], Req)->
    get_db_user(Rest,Req#pgsql_request{username=User});
get_db_user([{"database",DB}| Rest], Req) ->
    get_db_user(Rest,Req#pgsql_request{database=DB}).

decode_packet($Q, Data)->
    Size= size(Data)-1,
    <<SQL:Size/binary, 0:8 >> = Data,
    {ok, {sql, SQL}};
decode_packet($p, Data) ->
    Size= size(Data)-1,
    <<Password:Size/binary, 0:8 >> = Data,
    {ok, {password, Password}};
decode_packet($X, _) ->
    {ok, terminate}.

%%--------------------------------------------------------------------
%% Func: record_request/2
%% Purpose: record request given State=#state_rec and Request=#pgsql_request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
record_request(State=#state_rec{logfd=Fd},
               #pgsql_request{type=connect, username=User, database=DB})->
    io:format(Fd,"<request><pgsql type='connect' database='~s' username='~s'/>", [DB,User]),
    io:format(Fd,"</request>~n",[]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=sql, sql=SQL})->
    io:format(Fd,"<request><pgsql type='sql'><![CDATA[~s]]></pgsql>", [SQL]),
    io:format(Fd,"</request>~n",[]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=close})->
    io:format(Fd,"<request><pgsql type='close'/></request>", []),
    {ok,State};
record_request(State=#state_rec{logfd=Fd},
               #pgsql_request{type = authenticate , passwd  = Pass }) ->

    Fd = State#state_rec.logfd,
    io:format(Fd,"<request><pgsql type='authenticate' password='~s'>", [Pass]),
    io:format(Fd,"</pgsql></request>~n",[]),
    {ok,State}.


connect(undefined) ->
    {ok, Socket} = gen_tcp:connect(?config(pgsql_server),?config(pgsql_port),
                                   [{active, once},
                                    {recbuf, ?tcp_buffer},
                                    {sndbuf, ?tcp_buffer}
                                   ]),
    ?LOGF("ok, connected  ~p~n",[Socket],?DEB),
    Socket;
connect(Socket) -> Socket.
