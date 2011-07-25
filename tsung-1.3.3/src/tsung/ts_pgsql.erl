%%%
%%%  Copyright (C) Nicolas Niclausse 2005
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 6 Nov 2005 by Nicolas Niclausse <nicolas.niclausse@niclux.org>

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
%%% Purpose: plugin for postgresql
%%% Dependancies: pgsql modules from jungerl (pgsql_proto and pgsql_util)
%%% ---------------------------------------------------------------------

-module(ts_pgsql).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_pgsql.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/1,
         session_defaults/0,
         parse/2,
         parse_config/2,
         to_pairs/1,
         find_pair/2,
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
    #pgsql{}.

%%----------------------------------------------------------------------
%% Function: get_message/21
%% Purpose: Build a message/request ,
%% Args:    record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#pgsql_request{type=connect, database=DB, username=UserName}) ->
    Version = <<?PROTOCOL_MAJOR:16/integer, ?PROTOCOL_MINOR:16/integer>>,
    User = pgsql_util:make_pair(user, UserName),
    put(username,UserName), %% needed for md5
    Database = pgsql_util:make_pair(database, DB),
    StartupPacket = <<Version/binary,
                      User/binary,
                      Database/binary,
                      0>>,
    PacketSize = 4 + size(StartupPacket),
    <<PacketSize:32/integer, StartupPacket/binary>>;
get_message(#pgsql_request{type=sql,sql=Query}) ->
    pgsql_proto:encode_message(squery, Query);
get_message(#pgsql_request{type=close}) ->
    pgsql_proto:encode_message(terminate, "");
get_message(#pgsql_request{type=authenticate, auth_method={?PG_AUTH_PASSWD, _Salt},passwd=PassString}) ->
    ?LOGF("PGSQL: Must authenticate (passwd= ~p) ~n",[PassString],?DEB),
    pgsql_proto:encode_message(pass_plain, PassString);
get_message(#pgsql_request{type=authenticate, auth_method= {?PG_AUTH_MD5, Salt},passwd=PassString}) ->
    User=get(username),
    ?LOGF("PGSQL: Must authenticate user ~p with md5 (passwd= ~p, salt=~p) ~n",
          [User,PassString,Salt],?DEB),
    pgsql_proto:encode_message(pass_md5, {User,PassString,Salt});
get_message(#pgsql_request{type=authenticate, auth_method=AuthType}) ->
    ?LOGF("PGSQL: Authentication method not implemented ! [~p] ~n",[AuthType],?ERR),
    <<>>.


%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:    Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize=0}, [], true};
%% new response, compute data size (for stats)
parse(Data, State=#state_rcv{acc = [], datasize= 0}) ->
    parse(Data, State#state_rcv{datasize= size(Data)});
parse(Data, State=#state_rcv{acc = [], dyndata=DynData}) ->
    case process_head(Data) of
        {ok, {ready_for_query, idle}, _ } ->
            {State#state_rcv{ack_done = true},[],false};

        {ok, {ready_for_query, transaction}, _ } ->
            ?Debug("PGSQL: Transaction ~n"),
            {State#state_rcv{ack_done = true},[],false};

        {ok, {ready_for_query, failed_transaction}, _ } ->
            ?LOG("PGSQL: Failed Transaction ~n",?NOTICE),
            ts_mon:add({ count, pgsql_failed_transaction }),
            {State#state_rcv{ack_done = true},[],false};

        {ok, {authenticate, {0, _Salt}}, Tail } -> % auth OK, continue to parse resp.
            parse(Tail, State);

        {ok, {error_message, ErrMsg}, Tail } ->
            ts_mon:add({ count, error_pgsql }),
            ?LOGF("PGSQL: Got Error Msg from postgresql [~p] ~n",[ErrMsg],?NOTICE),
            case Tail of
                << >> ->
                    {State#state_rcv{ack_done = false},[],false};
                _ ->
                    parse(Tail, State)
            end;

        {ok, {authenticate, AuthType}, _ } ->
            NewDynData=DynData#dyndata{proto=#pgsql_dyndata{auth_method=AuthType}},
            {State#state_rcv{ack_done = true, dyndata=NewDynData},[],false};

        {ok, _Pair, Tail } ->
            parse(Tail, State);

        more ->
            ?LOG("PGSQL: need more data from socket ~n",?DEB),
            {State#state_rcv{ack_done = false, acc=Data},[],false}
    end;
%% more data, add this to accumulator and parse, update datasize
parse(Data, State=#state_rcv{acc=Acc, datasize=DataSize}) ->
    NewSize= DataSize + size(Data),
    parse(<< Acc/binary,Data/binary >>, State#state_rcv{acc=[], datasize=NewSize}).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
    ts_config_pgsql:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%          (this is used for ex. for Cookies in HTTP)
%%           for postgres, use this to store the auth method and salt
%% Args: Subst (true|false), DynData = #dyndata, Param = #myproto_request
%%                                               Host  = String
%% Returns: #pgsql_request
%%----------------------------------------------------------------------
add_dynparams(false, DynData, Param, HostData) ->
    add_dynparams(DynData#dyndata.proto, Param, HostData);
add_dynparams(true, DynData, Param, HostData) ->
    NewParam = subst(Param, DynData#dyndata.dynvars),
    add_dynparams(DynData#dyndata.proto,NewParam, HostData).

add_dynparams(DynPgsql, Param, _HostData) ->
    ?DebugF("Dyndata=~p, param=~p~n",[DynPgsql, Param]),
    Param#pgsql_request{auth_method=DynPgsql#pgsql_dyndata.auth_method,
                        salt=DynPgsql#pgsql_dyndata.salt}.

%%----------------------------------------------------------------------
%% Function: init_dynparams/0
%% Purpose:  initial dynamic parameters value
%% Returns:  #dyndata
%%----------------------------------------------------------------------
init_dynparams() ->
    #dyndata{proto=#pgsql_dyndata{}}.

%%----------------------------------------------------------------------
%% Function: subst/2
%% Purpose: Replace on the fly dynamic element of the request.
%% Returns: #pgsql_request
%%----------------------------------------------------------------------
subst(Req=#pgsql_request{sql=SQL}, DynData) ->
    Req#pgsql_request{sql=ts_search:subst(SQL, DynData)}.


%%% -- Internal funs --------------------

%%----------------------------------------------------------------------
%% @spec process_head(Bin::binary()) -> {ok, Pair::list(), Rest::binary()} |more
%% @doc parse postgresql binary, and return a tuple or more if the
%%      response is not complete
%% ----------------------------------------------------------------------
process_head(<<Code:8/integer, Size:4/integer-unit:8, Tail/binary>>) ->
    ?DebugF("PGSQL: received [~p]  size=~p Pckt size= ~p ~n",[Code, Size, size(Tail)]),
    RealSize = Size-4,
    case RealSize =< size(Tail) of
        true ->
            << Packet:RealSize/binary, Data/binary >> = Tail,
            {ok, Pair} = pgsql_proto:decode_packet(Code, Packet),
            ?LOGF("PGSQL: Pair=~p ~n",[Pair],?DEB),
            {ok, Pair, Data };
        false -> more
    end;
process_head(_) -> more.

%%% -- funs related to dyn_variables

%% @spec to_pairs(Bin::binary()) -> list()
%% @doc transform postgres binary into list of pairs
to_pairs(Bin) ->
    to_pairs(Bin,[]).
%% internal fun, with accumulator
to_pairs(<< >>, Acc) -> lists:reverse(Acc);
to_pairs(<<Code:8/integer, Size:4/integer-unit:8, Tail/binary>>, Acc) ->
    RealSize = Size-4,
    case RealSize =< size(Tail) of
        true ->
            << Packet:RealSize/binary, Data/binary >> = Tail,
            {ok, Pair} = pgsql_proto:decode_packet(Code, Packet),
            to_pairs(Data, [Pair| Acc] );
        false ->
            %% partial bin, should not happen; anyway send the current accumulated pairs
            ?LOGF("real size too small, abort ?!~p (Tail was~p)~n",[Acc,Tail], ?NOTICE),
            lists:reverse(Acc) %
    end.

%% @spec find_pairs(Expr::string(),Pairs::list()) -> term()
%% @doc Expr: expression like data_row[4][2], Pairs: list of pairs
%%      extracted by pgsql_proto:decode_packet.
find_pair(Expr,Pairs)->
    Fun= fun(A) ->
                case catch list_to_integer(A) of
                    I when is_integer(I) ->
                        I;
                    _  ->
                        list_to_atom(A)
                end
          end,
    Str=re:replace(Expr,"\\[(\\d+)\\]","\.\\1",[{return,list},global]),
    Keys=lists:map(Fun, string:tokens(Str,".")),
    find_pair_real(Keys,Pairs,1).

find_pair_real([Key,Row,ColName],Pairs,CurRow) when is_atom(ColName)->
    case get_col_id(atom_to_list(ColName),Pairs) of
        Col when is_integer(Col) ->
            find_pair_real([Key,Row,Col],Pairs,CurRow);
        _ ->
            undefined
    end;
find_pair_real([Key,SameRow,Y,Z],[{Key,Value}|_],SameRow) when is_atom(Key), is_list(Value) ->
    case lists:nth(Y,Value) of
        L when is_list(L) ->
            lists:nth(Z,L);
        T when is_tuple(T) ->
            element(Z,T);
        _ ->
            undefined
    end;
find_pair_real([Key,Row,Col],[{Key,Val}|_],Row) when is_atom(Key),is_list(Val),is_integer(Col)->
    lists:nth(Col,Val);
find_pair_real([Key,Row,Col|_],[{Key,Val}|_],Row) when is_atom(Key),is_tuple(Val)->
    element(Col,Val);
find_pair_real(A=[Key|_],[{Key,_Value}|Pairs],CurRow) -> %same key,different row
    find_pair_real(A,Pairs,CurRow+1);
find_pair_real(Expr,[_|Pairs],Row) ->% not the same key
    find_pair_real(Expr,Pairs,Row);
find_pair_real(_,_,_)  ->
    undefined.

get_col_id(ColName,Pairs) ->
    Desc=proplists:get_value(row_description,Pairs),
    case lists:keysearch(ColName,1,Desc) of
        {value,T} ->
            element(3,T); % column id is the third element of the tuple.
        false ->
            undefined
    end.
