%%%  Created :  July 2008 by Grégoire Reboul <gregoire.reboul@laposte.net>
%%%  From     :    ts_pgsql.erl by Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Note    :  Based on erlang-mysql by Magnus Ahltorp & Fredrik Thulin <ft@it.su.se>
%%
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

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

%%% ---------------------------------------------------------------------
%%% Purpose: plugin for mysql >= 4.1
%%% Dependancies: none
%%% Note: Packet fragmentation isnt implemented yet
%%% ---------------------------------------------------------------------
-module(ts_mysql).
-vc('$Id:$ ').
-author('gregoire.reboul@laposte.net').

-include("ts_profile.hrl").
-include("ts_mysql.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/1,
         session_defaults/0,
         parse/2,
         parse_config/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session
%% Returns: {ok, ack_type = parse|no_ack|local, persistent = true|false}
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #mysql{}.

%%----------------------------------------------------------------------
%% Function: get_message/21
%% Purpose: Build a message/request ,
%% Args:    record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#mysql_request{type=connect}) ->
    Packet=list_to_binary([]),
    ?LOGF("Opening socket. ~p ~n",[Packet], ?DEB),
    Packet;
get_message(#mysql_request{type=authenticate, database=Database, username=Username, passwd=Password, salt=Salt}) ->
    Packet=add_header(make_auth(Username, Password, Database, Salt),1),
    ?LOGF("Auth packet: ~p (~s)~n",[Packet,Packet], ?DEB),
    Packet;
get_message(#mysql_request{type=sql,sql=Query}) ->
    Packet=add_header([?MYSQL_QUERY_OP, Query],0),
    ?LOGF("Query packet: ~p (~s)~n",[Packet,Packet], ?DEB),
    Packet;
get_message(#mysql_request{type=close}) ->
    Packet=add_header([?MYSQL_CLOSE_OP],0),
    ?LOGF("Close packet: ~p (~s)~n",[Packet,Packet], ?DEB),
    Packet.



%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:    Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    ?LOG("Parsing> socket closed ~n", ?WARN),
    {State#state_rcv{ack_done = true, datasize=0}, [], true};

parse(Data, State)->
    <<PacketSize:24/little,_PacketNum:8/little,PacketBody/binary>> = Data,
    case PacketSize =< size(PacketBody) of
        true ->
            ?LOG("Parsing> full packet ~n",?DEB),
            Request = State#state_rcv.request,
            Param = Request#ts_request.param,
            case Param#mysql_request.type of
                connect ->
                    parse_greeting(PacketBody,State);
                authenticate ->
                    parse_result(PacketBody,State);
                sql ->
                    parse_result(PacketBody,State);
                close ->
                    {State#state_rcv{ack_done = true, datasize=size(Data)},[],false}
            end;
        false ->
            ?LOGF("Parsing> incomplete packet: size->~p body->~p ~n",[PacketSize,size(PacketBody)], ?WARN),
            {State#state_rcv{ack_done = false, datasize=size(Data), acc=PacketBody},[],false}
    end.

parse_greeting(Data, State=#state_rcv{acc = [],dyndata=DynData, datasize= 0}) ->
    ?LOGF("Parsing greeting ~p ~n",[Data], ?DEB),
    Salt= get_salt(Data),
    NewDynData=DynData#dyndata{proto=#mysql_dyndata{salt=Salt}},
    {State#state_rcv{ack_done = true, datasize=size(Data), dyndata=NewDynData},[],false}.

parse_result(Data,State)->
           case Data of
                <<Fieldcount:8, Rest2/binary>> ->
                    case Fieldcount of
                        0 ->
                            %% No Tabular data
                            <<AffectedRows:8, _Rest2/binary>> = Rest2,
                            ?LOGF("OK, No Data, Row affected: ~p (~s)~n", [AffectedRows,Data], ?DEB);
                        255 ->
                            <<Errno:16/little, _Marker:8, SQLState:5/binary, Message/binary>>  = Rest2,
                            ?LOGF("Error: ~p ~s ~s ~n", [Errno,SQLState, Message], ?WARN),
                            %% FIXME: should we stop if an error occurs ?
                            ts_mon:add({ count, list_to_atom("error_mysql_"++integer_to_list(Errno))});
                        254 when size(Rest2) < 9 ->
                            ?LOGF("EOF: (~p) ~n", [Rest2], ?DEB);
                        _ ->
                            ?LOGF("OK, Tabular Data, Columns count: ~p (~s)~n", [Fieldcount,Data], ?DEB)
                    end,
                    {State#state_rcv{ack_done = true,datasize=size(Data)},[],false};
                _ ->
                   ?LOG("Bad packet ", ?ERR),
                   ts_mon:add({ count, error_mysql_badpacket}),
                   {State#state_rcv{ack_done = true,datasize=size(Data)},[],false}
           end.

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
    ts_config_mysql:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%          (this is used for ex. for Cookies in HTTP)
%%           for postgres, use this to store the auth method and salt
%% Args: Subst (true|false), DynData = #dyndata, Param = #myproto_request
%%                                               Host  = String
%% Returns: #mysql_request
%%----------------------------------------------------------------------
add_dynparams(false, DynData, Param, HostData) ->
    add_dynparams(DynData#dyndata.proto, Param, HostData);
add_dynparams(true, DynData, Param, HostData) ->
    NewParam = subst(Param, DynData#dyndata.dynvars),
    add_dynparams(DynData#dyndata.proto,NewParam, HostData).
add_dynparams(DynMysql, Param, _HostData) ->
    Param#mysql_request{salt=DynMysql#mysql_dyndata.salt}.

%%----------------------------------------------------------------------
%% Function: init_dynparams/0
%% Purpose:  initial dynamic parameters value
%% Returns:  #dyndata
%%----------------------------------------------------------------------
init_dynparams() ->
    #dyndata{proto=#mysql_dyndata{}}.

%%----------------------------------------------------------------------
%% Function: subst/2
%% Purpose: Replace on the fly dynamic element of the request.
%% Returns: #mysql_request
%%----------------------------------------------------------------------
subst(Req=#mysql_request{sql=SQL}, DynData) ->
    Req#mysql_request{sql=ts_search:subst(SQL, DynData)}.


%%% -- Internal funs --------------------
add_header(Packet,SeqNum) ->
    BPacket=list_to_binary(Packet),
    <<(size(BPacket)):24/little, SeqNum:8, BPacket/binary>>.

get_salt(PacketBody) ->
    << _Protocol:8/little, Rest/binary>> = PacketBody,
    {_Version, Rest2} = asciz_binary(Rest,[]),
    <<_TreadID:32/little, Rest3/binary>> = Rest2,
    {Salt, Rest4} = asciz_binary(Rest3,[]),
    <<_Caps:16/little, Rest5/binary>> = Rest4,
    <<_ServerChar:16/binary-unit:8, Rest6/binary>> = Rest5,
    {Salt2, _Rest7} = asciz_binary(Rest6,[]),
    Salt ++ Salt2.

make_auth(User, Password, Database, Salt) ->
    EncryptedPassword = encrypt_password(Password, Salt),
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?PROTOCOL_41 bor ?TRANSACTIONS
            bor ?SECURE_CONNECTION bor ?CONNECT_WITH_DB,
    Maxsize = ?MAX_PACKET_SIZE,
    UserB = list_to_binary(User),
    PasswordL = size(EncryptedPassword),
    DatabaseB = list_to_binary(Database),
    binary_to_list(<<Caps:32/little, Maxsize:32/little, 8:8, 0:23/integer-unit:8,
    UserB/binary, 0:8, PasswordL:8, EncryptedPassword/binary, DatabaseB/binary>>).

encrypt_password(Password, Salt) ->
    Stage1= case catch crypto:sha(Password) of
                {'EXIT',_} ->
                    crypto:start(),
                    crypto:sha(Password);
                Sha -> Sha
            end,
    Stage2 = crypto:sha(Stage1),
    Res = crypto:sha_final(
        crypto:sha_update(
          crypto:sha_update(crypto:sha_init(), Salt),
          Stage2)
       ),
    bxor_binary(Res, Stage1).

%% @doc Find the first zero-byte in Data and add everything before it
%%   to Acc, as a string.
%%
%% @spec asciz_binary(Data::binary(), Acc::list()) ->
%%   {NewList::list(), Rest::binary()}

asciz_binary(<<>>, Acc) ->
    {lists:reverse(Acc)};
asciz_binary(<<0:8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) ->
    asciz_binary(Rest, [C | Acc]).

dualmap(_F, [], []) ->
    [];
dualmap(F, [E1 | R1], [E2 | R2]) ->
    [F(E1, E2) | dualmap(F, R1, R2)].

bxor_binary(B1, B2) ->
    list_to_binary(dualmap(fun (E1, E2) ->
                                   E1 bxor E2
                           end, binary_to_list(B1), binary_to_list(B2))).
