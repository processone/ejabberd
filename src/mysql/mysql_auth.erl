%%%-------------------------------------------------------------------
%%% File    : mysql_auth.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: MySQL client authentication functions.
%%% Created :  4 Aug 2005 by Fredrik Thulin <ft@it.su.se>
%%%
%%% Note    : All MySQL code was written by Magnus Ahltorp, originally
%%%           in the file mysql.erl - I just moved it here.
%%%
%%% Copyright (c) 2001-2004 Kungliga Tekniska Högskolan
%%% See the file COPYING
%%%
%%%-------------------------------------------------------------------
-module(mysql_auth).

%%--------------------------------------------------------------------
%% External exports (should only be used by the 'mysql_conn' module)
%%--------------------------------------------------------------------
-export([
	 do_old_auth/7,
	 do_new_auth/8
	]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(LONG_PASSWORD, 1).
-define(FOUND_ROWS, 2).
-define(LONG_FLAG, 4).
-define(PROTOCOL_41, 512).
-define(TRANSACTIONS, 8192).
-define(SECURE_CONNECTION, 32768).
-define(CONNECT_WITH_DB, 8).

-define(MAX_PACKET_SIZE, 1000000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: do_old_auth(Sock, RecvPid, SeqNum, User, Password, Salt1,
%%                       LogFun)
%%           Sock     = term(), gen_tcp socket
%%           RecvPid  = pid(), receiver process pid
%%           SeqNum   = integer(), first sequence number we should use
%%           User     = string(), MySQL username
%%           Password = string(), MySQL password
%%           Salt1    = string(), salt 1 from server greeting
%%           LogFun   = undefined | function() of arity 3
%% Descrip.: Perform old-style MySQL authentication.
%% Returns : result of mysql_conn:do_recv/3
%%--------------------------------------------------------------------
do_old_auth(Sock, RecvPid, SeqNum, User, Password, Salt1, LogFun) ->
    Auth = password_old(Password, Salt1),
    Packet2 = make_auth(User, Auth),
    do_send(Sock, Packet2, SeqNum, LogFun),
    mysql_conn:do_recv(LogFun, RecvPid, SeqNum).

%%--------------------------------------------------------------------
%% Function: do_new_auth(Sock, RecvPid, SeqNum, User, Password, Salt1,
%%                       Salt2, LogFun)
%%           Sock     = term(), gen_tcp socket
%%           RecvPid  = pid(), receiver process pid
%%           SeqNum   = integer(), first sequence number we should use
%%           User     = string(), MySQL username
%%           Password = string(), MySQL password
%%           Salt1    = string(), salt 1 from server greeting
%%           Salt2    = string(), salt 2 from server greeting
%%           LogFun   = undefined | function() of arity 3
%% Descrip.: Perform MySQL authentication.
%% Returns : result of mysql_conn:do_recv/3
%%--------------------------------------------------------------------
do_new_auth(Sock, RecvPid, SeqNum, User, Password, Salt1, Salt2, LogFun) ->
    Auth = password_new(Password, Salt1 ++ Salt2),
    Packet2 = make_new_auth(User, Auth, none),
    do_send(Sock, Packet2, SeqNum, LogFun),
    case mysql_conn:do_recv(LogFun, RecvPid, SeqNum) of
	{ok, Packet3, SeqNum2} ->
	    case Packet3 of
		<<254:8>> ->
		    AuthOld = password_old(Password, Salt1),
		    do_send(Sock, <<AuthOld/binary, 0:8>>, SeqNum2 + 1, LogFun),
		    mysql_conn:do_recv(LogFun, RecvPid, SeqNum2 + 1);
		_ ->
		    {ok, Packet3, SeqNum2}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

password_old(Password, Salt) ->
    {P1, P2} = hash(Password),
    {S1, S2} = hash(Salt),
    Seed1 = P1 bxor S1,
    Seed2 = P2 bxor S2,
    List = rnd(9, Seed1, Seed2),
    {L, [Extra]} = lists:split(8, List),
    list_to_binary(lists:map(fun (E) ->
				     E bxor (Extra - 64)
			     end, L)).

%% part of do_old_auth/4, which is part of mysql_init/4
make_auth(User, Password) ->
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG
	bor ?TRANSACTIONS bor ?FOUND_ROWS,
    Maxsize = 0,
    UserB = list_to_binary(User),
    PasswordB = Password,
    <<Caps:16/little, Maxsize:24/little, UserB/binary, 0:8,
    PasswordB/binary>>.

%% part of do_new_auth/4, which is part of mysql_init/4
make_new_auth(User, Password, Database) ->
    DBCaps = case Database of
		 none ->
		     0;
		 _ ->
		     ?CONNECT_WITH_DB
	     end,
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS bor
	?PROTOCOL_41 bor ?SECURE_CONNECTION bor DBCaps
	bor ?FOUND_ROWS,
    Maxsize = ?MAX_PACKET_SIZE,
    UserB = list_to_binary(User),
    PasswordL = size(Password),
    DatabaseB = case Database of
		    none ->
			<<>>;
		    _ ->
			list_to_binary(Database)
		end,
    <<Caps:32/little, Maxsize:32/little, 8:8, 0:23/integer-unit:8,
    UserB/binary, 0:8, PasswordL:8, Password/binary, DatabaseB/binary>>.

hash(S) ->
    hash(S, 1345345333, 305419889, 7).

hash([C | S], N1, N2, Add) ->
    N1_1 = N1 bxor (((N1 band 63) + Add) * C + N1 * 256),
    N2_1 = N2 + ((N2 * 256) bxor N1_1),
    Add_1 = Add + C,
    hash(S, N1_1, N2_1, Add_1);
hash([], N1, N2, _Add) ->
    Mask = (1 bsl 31) - 1,
    {N1 band Mask , N2 band Mask}.

rnd(N, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    rnd(N, [], Seed1 rem Mod, Seed2 rem Mod).

rnd(0, List, _, _) ->
    lists:reverse(List);
rnd(N, List, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    NSeed1 = (Seed1 * 3 + Seed2) rem Mod,
    NSeed2 = (NSeed1 + Seed2 + 33) rem Mod,
    Float = (float(NSeed1) / float(Mod))*31,
    Val = trunc(Float)+64,
    rnd(N - 1, [Val | List], NSeed1, NSeed2).



dualmap(_F, [], []) ->
    [];
dualmap(F, [E1 | R1], [E2 | R2]) ->
    [F(E1, E2) | dualmap(F, R1, R2)].

bxor_binary(B1, B2) ->
    list_to_binary(dualmap(fun (E1, E2) ->
				   E1 bxor E2
			   end, binary_to_list(B1), binary_to_list(B2))).

password_new(Password, Salt) ->
    Stage1 = crypto:sha(Password),
    Stage2 = crypto:sha(Stage1),
    Res = crypto:sha_final(
	    crypto:sha_update(
	      crypto:sha_update(crypto:sha_init(), Salt),
	      Stage2)
	   ),
    bxor_binary(Res, Stage1).


do_send(Sock, Packet, Num, LogFun) ->
    mysql:log(LogFun, debug, "mysql_auth send packet ~p: ~p", [Num, Packet]),
    Data = <<(size(Packet)):24/little, Num:8, Packet/binary>>,
    gen_tcp:send(Sock, Data).
