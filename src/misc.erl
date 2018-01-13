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
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-export([tolower/1, term_to_base64/1, base64_to_term/1, ip_to_list/1,
	 hex_to_bin/1, hex_to_base64/1, expand_keyword/3,
	 atom_to_binary/1, binary_to_atom/1, tuple_to_binary/1,
	 l2i/1, i2l/1, i2l/2, expr_to_term/1, term_to_expr/1,
	 now_to_usec/1, usec_to_now/1, encode_pid/1, decode_pid/2,
	 compile_exprs/2, join_atoms/2, try_read_file/1, have_eimp/0,
	 css_dir/0, img_dir/0, js_dir/0, read_css/1, read_img/1, read_js/1]).

%% Deprecated functions
-export([decode_base64/1, encode_base64/1]).
-deprecated([{decode_base64, 1},
	     {encode_base64, 1}]).

-include("logger.hrl").
-include_lib("kernel/include/file.hrl").

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
    try binary_to_term(base64:decode(Base64), [safe]) of
	Term -> {term, Term}
    catch _:badarg ->
	    error
    end.

-spec decode_base64(binary()) -> binary().
decode_base64(S) ->
    try base64:mime_decode(S)
    catch _:badarg -> <<>>
    end.

-spec encode_base64(binary()) -> binary().
encode_base64(Data) ->
    base64:encode(Data).

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
    base64:encode(hex_to_bin(Hex)).

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

-spec now_to_usec(erlang:timestamp()) -> non_neg_integer().
now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.

-spec usec_to_now(non_neg_integer()) -> erlang:timestamp().
usec_to_now(Int) ->
    Secs = Int div 1000000,
    USec = Int rem 1000000,
    MSec = Secs div 1000000,
    Sec = Secs rem 1000000,
    {MSec, Sec, USec}.

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

-spec compile_exprs(module(), [string()]) -> ok | {error, any()}.
compile_exprs(Mod, Exprs) ->
    try
	Forms = lists:map(
		  fun(Expr) ->
			  {ok, Tokens, _} = erl_scan:string(lists:flatten(Expr)),
			  {ok, Form} = erl_parse:parse_form(Tokens),
			  Form
		  end, Exprs),
	{ok, Code} = case compile:forms(Forms, []) of
			 {ok, Mod, Bin} -> {ok, Bin};
			 {ok, Mod, Bin, _Warnings} -> {ok, Bin};
			 Error -> Error
		     end,
	{module, Mod} = code:load_binary(Mod, "nofile", Code),
	ok
    catch _:{badmatch, {error, ErrInfo, _ErrLocation}} ->
	    {error, ErrInfo};
	  _:{badmatch, {error, _} = Err} ->
	    Err;
	  _:{badmatch, error} ->
	    {error, compile_failed}
    end.

-spec join_atoms([atom()], binary()) -> binary().
join_atoms(Atoms, Sep) ->
    str:join([io_lib:format("~p", [A]) || A <- Atoms], Sep).

%% @doc Checks if the file is readable and converts its name to binary.
%%      Fails with `badarg` otherwise. The function is intended for usage
%%      in configuration validators only.
-spec try_read_file(file:filename_all()) -> binary().
try_read_file(Path) ->
    case file:open(Path, [read]) of
	{ok, Fd} ->
	    file:close(Fd),
	    iolist_to_binary(Path);
	{error, Why} ->
	    ?ERROR_MSG("Failed to read ~s: ~s", [Path, file:format_error(Why)]),
	    erlang:error(badarg)
    end.

-ifdef(GRAPHICS).
have_eimp() -> true.
-else.
have_eimp() -> false.
-endif.

-spec css_dir() -> file:filename().
css_dir() ->
    case os:getenv("EJABBERD_CSS_PATH") of
	false ->
	    case code:priv_dir(ejabberd) of
		{error, _} -> filename:join(["priv", "css"]);
		Path -> filename:join([Path, "css"])
	    end;
	Path -> Path
    end.

-spec img_dir() -> file:filename().
img_dir() ->
    case os:getenv("EJABBERD_IMG_PATH") of
	false ->
	    case code:priv_dir(ejabberd) of
		{error, _} -> filename:join(["priv", "img"]);
		Path -> filename:join([Path, "img"])
	    end;
	Path -> Path
    end.

-spec js_dir() -> file:filename().
js_dir() ->
    case os:getenv("EJABBERD_JS_PATH") of
	false ->
	    case code:priv_dir(ejabberd) of
		{error, _} -> filename:join(["priv", "js"]);
		Path -> filename:join([Path, "js"])
	    end;
	Path -> Path
    end.

-spec read_css(file:filename()) -> {ok, binary()} | {error, file:posix()}.
read_css(File) ->
    read_file(filename:join(css_dir(), File)).

-spec read_img(file:filename()) -> {ok, binary()} | {error, file:posix()}.
read_img(File) ->
    read_file(filename:join(img_dir(), File)).

-spec read_js(file:filename()) -> {ok, binary()} | {error, file:posix()}.
read_js(File) ->
    read_file(filename:join(js_dir(), File)).

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

-spec read_file(file:filename()) -> {ok, binary()} | {error, file:posix()}.
read_file(Path) ->
    case file:read_file(Path) of
	{ok, Data} ->
	    {ok, Data};
	{error, Why} = Err ->
	    ?ERROR_MSG("Failed to read file ~s: ~s",
		       [Path, file:format_error(Why)]),
	    Err
    end.
