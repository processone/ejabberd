%%% File    : pgsql_util.erl
%%% Author  : Christian Sunesson
%%% Description : utility functions used in implementation of 
%%%               postgresql driver.
%%% Created : 11 May 2005 by Blah <cos@local>

-module(pgsql_util).

%% Key-Value handling
-export([option/2]).

%% Networking
-export([socket/1]).
-export([send/2, send_int/2, send_msg/3]).
-export([recv_msg/2, recv_msg/1, recv_byte/2, recv_byte/1]).

%% Protocol packing
-export([string/1, make_pair/2, split_pair/1]).
-export([split_pair_rec/1]).
-export([count_string/1, to_string/1]).
-export([oids/2, coldescs/2, datacoldescs/3]).
-export([decode_row/2, decode_descs/1]).
-export([errordesc/1]).

-export([zip/2]).

%% Constructing authentication messages.
-export([pass_plain/1, pass_md5/3]).
-import(erlang, [md5/1]).
-export([hexlist/2]).

%% Lookup key in a plist stored in process dictionary under 'options'.
%% Default is returned if there is no value for Key in the plist.
option(Key, Default) ->
    Plist = get(options),
    case proplists:get_value(Key, Plist, Default) of
	Default ->
	    Default;
	Value ->
	    Value
    end.


%% Open a TCP connection
socket({tcp, Host, Port}) ->
    gen_tcp:connect(Host, Port, [{active, false}, binary, {packet, raw}], 5000).

send(Sock, Packet) ->
    gen_tcp:send(Sock, Packet).
send_int(Sock, Int) ->
    Packet = <<Int:32/integer>>,
    gen_tcp:send(Sock, Packet).

send_msg(Sock, Code, Packet) when binary(Packet) ->
    Len = size(Packet) + 4,
    Msg = <<Code:8/integer, Len:4/integer-unit:8, Packet/binary>>,
    gen_tcp:send(Sock, Msg).

recv_msg(Sock, Timeout) ->
    {ok, Head} = gen_tcp:recv(Sock, 5, Timeout),
    <<Code:8/integer, Size:4/integer-unit:8>> = Head,
    %%io:format("Code: ~p, Size: ~p~n", [Code, Size]),
    if 
	Size > 4 ->
	    {ok, Packet} = gen_tcp:recv(Sock, Size-4, Timeout),
	    {ok, Code, Packet};
	true ->
	    {ok, Code, <<>>}
    end.
recv_msg(Sock) ->
    recv_msg(Sock, infinity).


recv_byte(Sock) ->
    recv_byte(Sock, infinity).
recv_byte(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 1, Timeout) of
	{ok, <<Byte:1/integer-unit:8>>} ->
	    {ok, Byte};
	E={error, _Reason} ->
	    throw(E)
    end.

%% Convert String to binary
string(String) when list(String) ->
    Bin = list_to_binary(String),
    <<Bin/binary, 0/integer>>;
string(Bin) when binary(Bin) ->
    <<Bin/binary, 0/integer>>.

%%% Two zero terminated strings.
make_pair(Key, Value) when atom(Key) ->
    make_pair(atom_to_list(Key), Value);
make_pair(Key, Value) when atom(Value) ->
    make_pair(Key, atom_to_list(Value));
make_pair(Key, Value) when list(Key), list(Value) ->
    BinKey = list_to_binary(Key),
    BinValue = list_to_binary(Value),
    make_pair(BinKey, BinValue);
make_pair(Key, Value) when binary(Key), binary(Value) ->
    <<Key/binary, 0/integer, 
     Value/binary, 0/integer>>.

split_pair(Bin) when binary(Bin) ->
    split_pair(binary_to_list(Bin));
split_pair(Str)  ->
    split_pair_rec(Str, norec).

split_pair_rec(Bin) when binary(Bin) ->
    split_pair_rec(binary_to_list(Bin));
split_pair_rec(Arg)  ->
    split_pair_rec(Arg,[]).

split_pair_rec([], Acc) ->
    lists:reverse(Acc);
split_pair_rec([0], Acc) ->
    lists:reverse(Acc);
split_pair_rec(S, Acc) ->
    Fun = fun(C) -> C /= 0 end,
    {Key, [0|S1]} = lists:splitwith(Fun, S),
    {Value, [0|Tail]} = lists:splitwith(Fun, S1),
    case Acc of 
        norec -> {Key, Value};
        _ ->
            split_pair_rec(Tail, [{Key, Value}| Acc])
    end.


count_string(Bin) when binary(Bin) ->
    count_string(Bin, 0).

count_string(<<>>, N) ->
    {N, <<>>};
count_string(<<0/integer, Rest/binary>>, N) ->
    {N, Rest};
count_string(<<_C/integer, Rest/binary>>, N) ->
    count_string(Rest, N+1).

to_string(Bin) when binary(Bin) ->    
    {Count, _} = count_string(Bin, 0),
    <<String:Count/binary, _/binary>> = Bin,
    {binary_to_list(String), Count}.

oids(<<>>, Oids) ->
    lists:reverse(Oids);
oids(<<Oid:32/integer, Rest/binary>>, Oids) ->
    oids(Rest, [Oid|Oids]).
    
coldescs(<<>>, Descs) ->
    lists:reverse(Descs);
coldescs(Bin, Descs) ->
    {Name, Count} = to_string(Bin),
    <<_:Count/binary, 0/integer,
     TableOID:32/integer,
     ColumnNumber:16/integer,
     TypeId:32/integer,
     TypeSize:16/integer-signed,
     TypeMod:32/integer-signed,
     FormatCode:16/integer,
     Rest/binary>> = Bin,
    Format = case FormatCode of 
		 0 -> text; 
		 1 -> binary 
	     end,
    Desc = {Name, Format, ColumnNumber, 
	    TypeId, TypeSize, TypeMod, 
	    TableOID},
    coldescs(Rest, [Desc|Descs]).

datacoldescs(N, 
	     <<Len:32/integer, Data:Len/binary, Rest/binary>>, 
	     Descs) when N >= 0 ->
    datacoldescs(N-1, Rest, [Data|Descs]);
datacoldescs(_N, _, Descs) ->
    lists:reverse(Descs).

decode_descs(Cols) ->
    decode_descs(Cols, []).
decode_descs([], Descs) ->
    {ok, lists:reverse(Descs)};
decode_descs([Col|ColTail], Descs) ->
    OidMap = get(oidmap),
    {Name, Format, ColNumber, Oid, _, _, _} = Col,
    OidName = dict:fetch(Oid, OidMap),
    decode_descs(ColTail, [{Name, Format, ColNumber, OidName, [], [], []}|Descs]).

decode_row(Types, Values) ->
    decode_row(Types, Values, []).
decode_row([], [], Out) ->
    {ok, lists:reverse(Out)};
decode_row([Type|TypeTail], [Value|ValueTail], Out0) ->
    Out1 = decode_col(Type, Value),
    decode_row(TypeTail, ValueTail, [Out1|Out0]).

decode_col({_, text, _, _, _, _, _}, Value) ->
    binary_to_list(Value);
decode_col({_Name, _Format, _ColNumber, varchar, _Size, _Modifier, _TableOID}, Value) ->
    binary_to_list(Value);
decode_col({_Name, _Format, _ColNumber, int4, _Size, _Modifier, _TableOID}, Value) ->
    <<Int4:32/integer>> = Value,
    Int4;
decode_col({_Name, _Format, _ColNumber, Oid, _Size, _Modifier, _TableOID}, Value) ->
    {Oid, Value}.

errordesc(Bin) ->
    errordesc(Bin, []).

errordesc(<<0/integer, _Rest/binary>>, Lines) ->
    lists:reverse(Lines);
errordesc(<<Code/integer, Rest/binary>>, Lines) ->
    {String, Count} = to_string(Rest),
    <<_:Count/binary, 0, Rest1/binary>> = Rest,
    Msg = case Code of 
	      $S ->
		  {severity, list_to_atom(String)};
	      $C ->
		  {code, String};
	      $M ->
		  {message, String};
	      $D ->
		  {detail, String};
	      $H ->
		  {hint, String};
	      $P ->
		  {position, list_to_integer(String)};
	      $p ->
		  {internal_position, list_to_integer(String)};
	      $W ->
		  {where, String};
	      $F ->
		  {file, String};
	      $L ->
		  {line, list_to_integer(String)};
	      $R ->
		  {routine, String};
	      Unknown ->
		  {Unknown, String}
	  end,
    errordesc(Rest1, [Msg|Lines]).

%%% Zip two lists together
zip(List1, List2) ->
    zip(List1, List2, []).
zip(List1, List2, Result) when List1 =:= []; 
			       List2 =:= [] ->
    lists:reverse(Result);
zip([H1|List1], [H2|List2], Result) ->
    zip(List1, List2, [{H1, H2}|Result]).

%%% Authentication utils

pass_plain(Password) ->
	Pass = [Password, 0],
	list_to_binary(Pass). 

%% MD5 authentication patch from
%%    Juhani Rankimies <juhani@juranki.com>
%% (patch slightly rewritten, new bugs are mine :] /Christian Sunesson)

%%
%% MD5(MD5(password + user) + salt)
%%

pass_md5(User, Password, Salt) ->
    Digest = hex(md5([Password, User])),
    Encrypt = hex(md5([Digest, Salt])),
    Pass = ["md5", Encrypt, 0],
    list_to_binary(Pass).

hex(B) when binary(B) ->
    hexlist(binary_to_list(B), []).

hexlist([], Acc) ->
    lists:reverse(Acc);
hexlist([N|Rest], Acc) ->
    HighNibble = (N band 16#f0) bsr 4,
    LowNibble = (N band 16#0f),
    hexlist(Rest, [hexdigit(LowNibble), hexdigit(HighNibble)|Acc]).

hexdigit(0) -> $0;
hexdigit(1) -> $1;
hexdigit(2) -> $2;
hexdigit(3) -> $3;
hexdigit(4) -> $4;
hexdigit(5) -> $5;
hexdigit(6) -> $6;
hexdigit(7) -> $7;
hexdigit(8) -> $8;
hexdigit(9) -> $9;
hexdigit(10) -> $a;
hexdigit(11) -> $b;
hexdigit(12) -> $c;
hexdigit(13) -> $d;
hexdigit(14) -> $e;
hexdigit(15) -> $f.
