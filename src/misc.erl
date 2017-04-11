%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @doc
%%%   This is the place for some unsorted auxiliary functions
%%%   Some functions from jlib.erl are moved here
%%%   Mild rubbish heap is accepted ;)
%%% @end
%%% Created : 30 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
-module(misc).

%% API
-export([tolower/1, term_to_base64/1, base64_to_term/1,
	 decode_base64/1, encode_base64/1, ip_to_list/1,
	 hex_to_bin/1, hex_to_base64/1, expand_keyword/3,
	 atom_to_binary/1, binary_to_atom/1, tuple_to_binary/1,
	 l2i/1, i2l/1, i2l/2, expr_to_term/1, term_to_expr/1,
	 encode_pid/1, decode_pid/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec tolower(binary()) -> binary().
tolower(B) ->
    iolist_to_binary(tolower_s(binary_to_list(B))).

tolower_s([C | Cs]) ->
    if C >= $A, C =< $Z -> [C + 32 | tolower_s(Cs)];
       true -> [C | tolower_s(Cs)]
    end;
tolower_s([]) -> [].

-spec term_to_base64(term()) -> binary().
term_to_base64(Term) ->
    encode_base64(term_to_binary(Term)).

-spec base64_to_term(binary()) -> {term, term()} | error.
base64_to_term(Base64) ->
    case catch binary_to_term(decode_base64(Base64), [safe]) of
      {'EXIT', _} ->
	  error;
      Term ->
	  {term, Term}
    end.

-spec decode_base64(binary()) -> binary().
decode_base64(S) ->
    case catch binary:last(S) of
      C when C == $\n; C == $\s ->
	  decode_base64(binary:part(S, 0, byte_size(S) - 1));
      _ ->
	  decode_base64_bin(S, <<>>)
    end.

take_without_spaces(Bin, Count) ->
    take_without_spaces(Bin, Count, <<>>).

take_without_spaces(Bin, 0, Acc) ->
    {Acc, Bin};
take_without_spaces(<<>>, _, Acc) ->
    {Acc, <<>>};
take_without_spaces(<<$\s, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\t, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\n, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\r, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<Char:8, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count-1, <<Acc/binary, Char:8>>).

decode_base64_bin(<<>>, Acc) ->
    Acc;
decode_base64_bin(Bin, Acc) ->
    case take_without_spaces(Bin, 4) of
        {<<A, B, $=, $=>>, _} ->
            <<Acc/binary, (d(A)):6, (d(B) bsr 4):2>>;
        {<<A, B, C, $=>>, _} ->
            <<Acc/binary, (d(A)):6, (d(B)):6, (d(C) bsr 2):4>>;
        {<<A, B, C, D>>, Tail} ->
            Acc2 = <<Acc/binary, (d(A)):6, (d(B)):6, (d(C)):6, (d(D)):6>>,
            decode_base64_bin(Tail, Acc2);
        _ ->
            <<"">>
    end.

d(X) when X >= $A, X =< $Z -> X - 65;
d(X) when X >= $a, X =< $z -> X - 71;
d(X) when X >= $0, X =< $9 -> X + 4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.


%% Convert Erlang inet IP to list
-spec encode_base64(binary()) -> binary().
encode_base64(Data) ->
    encode_base64_bin(Data, <<>>).

encode_base64_bin(<<A:6, B:6, C:6, D:6, Tail/binary>>, Acc) ->
    encode_base64_bin(Tail, <<Acc/binary, (e(A)):8, (e(B)):8, (e(C)):8, (e(D)):8>>);
encode_base64_bin(<<A:6, B:6, C:4>>, Acc) ->
    <<Acc/binary, (e(A)):8, (e(B)):8, (e(C bsl 2)):8, $=>>;
encode_base64_bin(<<A:6, B:2>>, Acc) ->
    <<Acc/binary, (e(A)):8, (e(B bsl 4)):8, $=, $=>>;
encode_base64_bin(<<>>, Acc) ->
    Acc.

e(X) when X >= 0, X < 26 -> X + 65;
e(X) when X > 25, X < 52 -> X + 71;
e(X) when X > 51, X < 62 -> X - 4;
e(62) -> $+;
e(63) -> $/;
e(X) -> exit({bad_encode_base64_token, X}).

-spec ip_to_list(inet:ip_address() | undefined |
                 {inet:ip_address(), inet:port_number()}) -> binary().

ip_to_list({IP, _Port}) ->
    ip_to_list(IP);
%% This function clause could use inet_parse too:
ip_to_list(undefined) ->
    <<"unknown">>;
ip_to_list(IP) ->
    list_to_binary(inet_parse:ntoa(IP)).

-spec hex_to_bin(binary()) -> binary().
hex_to_bin(Hex) ->
    hex_to_bin(binary_to_list(Hex), []).

-spec hex_to_bin(list(), list()) -> binary().
hex_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hex_to_bin([H1, H2 | T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [H1, H2]),
    hex_to_bin(T, [V | Acc]).

-spec hex_to_base64(binary()) -> binary().
hex_to_base64(Hex) ->
    encode_base64(hex_to_bin(Hex)).

-spec expand_keyword(binary(), binary(), binary()) -> binary().
expand_keyword(Keyword, Input, Replacement) ->
    Parts = binary:split(Input, Keyword, [global]),
    str:join(Parts, Replacement).

binary_to_atom(Bin) ->
    erlang:binary_to_atom(Bin, utf8).

tuple_to_binary(T) ->
    iolist_to_binary(tuple_to_list(T)).

atom_to_binary(A) ->
    erlang:atom_to_binary(A, utf8).

expr_to_term(Expr) ->
    Str = binary_to_list(<<Expr/binary, ".">>),
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

term_to_expr(Term) ->
    list_to_binary(io_lib:print(Term)).

l2i(I) when is_integer(I) -> I;
l2i(L) when is_binary(L) -> binary_to_integer(L).

i2l(I) when is_integer(I) -> integer_to_binary(I);
i2l(L) when is_binary(L) -> L.

i2l(I, N) when is_integer(I) -> i2l(i2l(I), N);
i2l(L, N) when is_binary(L) ->
    case str:len(L) of
      N -> L;
      C when C > N -> L;
      _ -> i2l(<<$0, L/binary>>, N)
    end.

-spec encode_pid(pid()) -> binary().
encode_pid(Pid) ->
    list_to_binary(erlang:pid_to_list(Pid)).

-spec decode_pid(binary(), binary()) -> pid().
decode_pid(PidBin, NodeBin) ->
    PidStr = binary_to_list(PidBin),
    Pid = erlang:list_to_pid(PidStr),
    case erlang:binary_to_atom(NodeBin, latin1) of
	Node when Node == node() ->
	    Pid;
	Node ->
	    try set_node_id(PidStr, NodeBin)
	    catch _:badarg ->
		    erlang:error({bad_node, Node})
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec set_node_id(string(), binary()) -> pid().
set_node_id(PidStr, NodeBin) ->
    ExtPidStr = erlang:pid_to_list(
		  binary_to_term(
		    <<131,103,100,(size(NodeBin)):16,NodeBin/binary,0:72>>)),
    [H|_] = string:tokens(ExtPidStr, "."),
    [_|T] = string:tokens(PidStr, "."),
    erlang:list_to_pid(string:join([H|T], ".")).
