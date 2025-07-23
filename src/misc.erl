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
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-export([add_delay_info/3, add_delay_info/4,
	 unwrap_carbon/1, unwrap_mucsub_message/1, is_standalone_chat_state/1,
	 tolower/1, term_to_base64/1, base64_to_term/1, ip_to_list/1,
	 hex_to_bin/1, hex_to_base64/1, url_encode/1, expand_keyword/3,
	 atom_to_binary/1, binary_to_atom/1, tuple_to_binary/1,
	 l2i/1, i2l/1, i2l/2, expr_to_term/1, term_to_expr/1,
	 now_to_usec/1, usec_to_now/1, encode_pid/1, decode_pid/2,
	 compile_exprs/2, join_atoms/2, try_read_file/1, get_descr/2,
	 get_home/0, warn_unset_home/0,
	 css_dir/0, img_dir/0, js_dir/0, msgs_dir/0, sql_dir/0, lua_dir/0,
	 read_css/1, read_img/1, read_js/1, read_lua/1,
	 intersection/2, format_val/1, cancel_timer/1, unique_timestamp/0,
	 is_mucsub_message/1, best_match/2, pmap/2, peach/2, format_exception/4,
	 get_my_ipv4_address/0, get_my_ipv6_address/0, parse_ip_mask/1,
	 crypto_hmac/3, crypto_hmac/4, uri_parse/1, uri_parse/2, uri_quote/1,
	 uri_decode/1,
         json_encode/1, json_decode/1,
	 set_proc_label/1,
	 match_ip_mask/3, format_hosts_list/1, format_cycle/1, delete_dir/1,
	 semver_to_xxyy/1, logical_processors/0, get_mucsub_event_type/1,
         lists_uniq/1]).

%% Deprecated functions
-export([decode_base64/1, encode_base64/1]).
-deprecated([{decode_base64, 1},
	     {encode_base64, 1}]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include_lib("kernel/include/file.hrl").

-ifdef(OTP_BELOW_27).
%% Copied from erlang/otp/lib/stdlib/src/re.erl
-type re_mp() :: {re_pattern, _, _, _, _}.
-type json_value() :: jiffy:json_value().
-else.
-type re_mp() :: re:mp().
-type json_value() :: json:encode_value().
-endif.
-export_type([re_mp/0]).
-export_type([json_value/0]).

-type distance_cache() :: #{{string(), string()} => non_neg_integer()}.

-spec uri_parse(binary()|string()) -> {ok, string(), string(), string(), number(), string(), string()} | {error, term()}.
uri_parse(URL) ->
    yconf:parse_uri(URL).

uri_parse(URL, Protocols) ->
    yconf:parse_uri(URL, Protocols).

-ifdef(OTP_BELOW_25).
-ifdef(OTP_BELOW_24).
uri_quote(Data) ->
    Data.
-else.
uri_quote(Data) ->
    http_uri:encode(Data).
-endif.
-else.
uri_quote(Data) ->
    uri_string:quote(Data).
-endif.

%% @doc Decode a part of the URL and return string()
%% -spec url_decode(binary()) -> bitstring().

-ifdef(OTP_BELOW_24).
uri_decode(Path) -> uri_decode(Path, <<>>).

uri_decode(<<$%, Hi, Lo, Tail/binary>>, Acc) ->
    Hex = list_to_integer([Hi, Lo], 16),
    if Hex == 0 -> exit(badurl);
       true -> ok
    end,
    uri_decode(Tail, <<Acc/binary, Hex>>);
uri_decode(<<H, T/binary>>, Acc) when H /= 0 ->
    uri_decode(T, <<Acc/binary, H>>);
uri_decode(<<>>, Acc) -> Acc.
-else.
uri_decode(Path) ->
    uri_string:percent_decode(Path).
-endif.

-ifdef(USE_OLD_CRYPTO_HMAC).
crypto_hmac(Type, Key, Data) -> crypto:hmac(Type, Key, Data).
crypto_hmac(Type, Key, Data, MacL) -> crypto:hmac(Type, Key, Data, MacL).
-else.
crypto_hmac(Type, Key, Data) -> crypto:mac(hmac, Type, Key, Data).
crypto_hmac(Type, Key, Data, MacL) -> crypto:macN(hmac, Type, Key, Data, MacL).
-endif.

-ifdef(OTP_BELOW_27).
json_encode(Term) ->
    jiffy:encode(Term).
json_decode(Bin) ->
    jiffy:decode(Bin, [return_maps]).
-else.
json_encode({[{_Key, _Value} | _]} = Term) ->
    iolist_to_binary(json:encode(Term,
		     fun({Val}, Encoder) when is_list(Val) ->
			 json:encode_key_value_list(Val, Encoder);
			(Val, Encoder) ->
			 json:encode_value(Val, Encoder)
		     end));
json_encode({[]}) ->
    %% Jiffy was able to handle this case, but Json library does not
    <<"{}">>;
json_encode(Term) ->
    iolist_to_binary(json:encode(Term)).
json_decode(Bin) ->
    json:decode(Bin).
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-spec add_delay_info(stanza(), jid(), erlang:timestamp()) -> stanza().
add_delay_info(Stz, From, Time) ->
    add_delay_info(Stz, From, Time, <<"">>).

-spec add_delay_info(stanza(), jid(), erlang:timestamp(), binary()) -> stanza().
add_delay_info(Stz, From, Time, Desc) ->
    Delays = xmpp:get_subtags(Stz, #delay{stamp = {0,0,0}}),
    Matching = lists:any(
	fun(#delay{from = OldFrom}) when is_record(OldFrom, jid) ->
	       jid:tolower(From) == jid:tolower(OldFrom);
	   (_) ->
	       false
	end, Delays),
    case Matching of
	true ->
	    Stz;
	_ ->
	    NewDelay = #delay{stamp = Time, from = From, desc = Desc},
	    xmpp:append_subtags(Stz, [NewDelay])
    end.

-spec unwrap_carbon(stanza()) -> xmpp_element().
unwrap_carbon(#message{} = Msg) ->
    try
	case xmpp:get_subtag(Msg, #carbons_sent{forwarded = #forwarded{}}) of
	    #carbons_sent{forwarded = #forwarded{sub_els = [El]}} ->
		xmpp:decode(El, ?NS_CLIENT, [ignore_els]);
	    _ ->
		case xmpp:get_subtag(Msg, #carbons_received{
					      forwarded = #forwarded{}}) of
		    #carbons_received{forwarded = #forwarded{sub_els = [El]}} ->
			xmpp:decode(El, ?NS_CLIENT, [ignore_els]);
		    _ ->
			Msg
		end
	end
    catch _:{xmpp_codec, _} ->
	    Msg
    end;
unwrap_carbon(Stanza) -> Stanza.

-spec unwrap_mucsub_message(xmpp_element()) -> message() | false.
unwrap_mucsub_message(#message{} = OuterMsg) ->
    case xmpp:get_subtag(OuterMsg, #ps_event{}) of
	#ps_event{
	    items = #ps_items{
		node = Node,
		items = [
		    #ps_item{
			sub_els = [#message{} = InnerMsg]} | _]}}
	    when Node == ?NS_MUCSUB_NODES_MESSAGES;
		 Node == ?NS_MUCSUB_NODES_SUBJECT ->
	    InnerMsg;
	_ ->
	    false
    end;
unwrap_mucsub_message(_Packet) ->
    false.

-spec is_mucsub_message(xmpp_element()) -> boolean().
is_mucsub_message(Packet) ->
    get_mucsub_event_type(Packet) /= false.

-spec get_mucsub_event_type(xmpp_element()) -> binary() | false.
get_mucsub_event_type(#message{} = OuterMsg) ->
    case xmpp:get_subtag(OuterMsg, #ps_event{}) of
	#ps_event{
	    items = #ps_items{
		node = Node}}
	    when Node == ?NS_MUCSUB_NODES_MESSAGES;
		 Node == ?NS_MUCSUB_NODES_SUBJECT;
		 Node == ?NS_MUCSUB_NODES_AFFILIATIONS;
		 Node == ?NS_MUCSUB_NODES_CONFIG;
		 Node == ?NS_MUCSUB_NODES_PARTICIPANTS;
		 Node == ?NS_MUCSUB_NODES_PRESENCE;
		 Node == ?NS_MUCSUB_NODES_SUBSCRIBERS ->
	    Node;
	_ ->
	    false
    end;
get_mucsub_event_type(_Packet) ->
    false.

-spec is_standalone_chat_state(stanza()) -> boolean().
is_standalone_chat_state(Stanza) ->
    case unwrap_carbon(Stanza) of
	#message{body = [], subject = [], sub_els = Els} ->
	    IgnoreNS = [?NS_CHATSTATES, ?NS_DELAY, ?NS_EVENT, ?NS_HINTS],
	    Stripped = [El || El <- Els,
			      not lists:member(xmpp:get_ns(El), IgnoreNS)],
	    Stripped == [];
	_ ->
	    false
    end.

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
    catch _:_ ->
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
ip_to_list(local) ->
    <<"unix">>;
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

-spec url_encode(binary()) -> binary().
url_encode(A) ->
    url_encode(A, <<>>).

-spec expand_keyword(iodata(), iodata(), iodata()) -> binary().
expand_keyword(Keyword, Input, Replacement) ->
    re:replace(Input, Keyword, Replacement,
	       [{return, binary}, global]).

binary_to_atom(Bin) ->
    erlang:binary_to_atom(Bin, utf8).

tuple_to_binary(T) ->
    iolist_to_binary(tuple_to_list(T)).

%% erlang:atom_to_binary/1 is available since OTP 23
%% https://www.erlang.org/doc/apps/erts/erlang#atom_to_binary/1
%% Let's use /2 for backwards compatibility.
atom_to_binary(A) ->
    erlang:atom_to_binary(A, utf8).

expr_to_term(Expr) ->
    Str = binary_to_list(<<Expr/binary, ".">>),
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

term_to_expr(Term) ->
    list_to_binary(io_lib:print(Term, 1, 999999, -1)).

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
    str:join([io_lib:format("~p", [A]) || A <- lists:sort(Atoms)], Sep).

%% @doc Checks if the file is readable and converts its name to binary.
%%      Fails with `badarg' otherwise. The function is intended for usage
%%      in configuration validators only.
-spec try_read_file(file:filename_all()) -> binary().
try_read_file(Path) ->
    case file:open(Path, [read]) of
	{ok, Fd} ->
	    file:close(Fd),
	    iolist_to_binary(Path);
	{error, Why} ->
	    ?ERROR_MSG("Failed to read ~ts: ~ts", [Path, file:format_error(Why)]),
	    erlang:error(badarg)
    end.

-spec css_dir() -> file:filename().
css_dir() ->
    get_dir("css").

-spec img_dir() -> file:filename().
img_dir() ->
    get_dir("img").

-spec js_dir() -> file:filename().
js_dir() ->
    get_dir("js").

-spec msgs_dir() -> file:filename().
msgs_dir() ->
    get_dir("msgs").

-spec sql_dir() -> file:filename().
sql_dir() ->
    get_dir("sql").

-spec lua_dir() -> file:filename().
lua_dir() ->
    get_dir("lua").

-spec read_css(file:filename()) -> {ok, binary()} | {error, file:posix()}.
read_css(File) ->
    read_file(filename:join(css_dir(), File)).

-spec read_img(file:filename()) -> {ok, binary()} | {error, file:posix()}.
read_img(File) ->
    read_file(filename:join(img_dir(), File)).

-spec read_js(file:filename()) -> {ok, binary()} | {error, file:posix()}.
read_js(File) ->
    read_file(filename:join(js_dir(), File)).

-spec read_lua(file:filename()) -> {ok, binary()} | {error, file:posix()}.
read_lua(File) ->
    read_file(filename:join(lua_dir(), File)).

-spec get_descr(binary(), binary()) -> binary().
get_descr(Lang, Text) ->
    Desc = translate:translate(Lang, Text),
    Copyright = ejabberd_config:get_copyright(),
    <<Desc/binary, $\n, Copyright/binary>>.

-spec get_home() -> string().
get_home() ->
    case init:get_argument(home) of
        {ok, [[Home]]} ->
            Home;
        error ->
            mnesia:system_info(directory)
    end.

warn_unset_home() ->
    case init:get_argument(home) of
        {ok, [[_Home]]} ->
            ok;
        error ->
            ?INFO_MSG("The 'HOME' environment variable is not set, "
                 "ejabberd will use as HOME the Mnesia directory: ~s.",
                 [mnesia:system_info(directory)])
    end.

-spec intersection(list(), list()) -> list().
intersection(L1, L2) ->
    lists:filter(
      fun(E) ->
              lists:member(E, L2)
      end, L1).

-spec format_val(any()) -> iodata().
format_val({yaml, S}) when is_integer(S); is_binary(S); is_atom(S) ->
    format_val(S);
format_val({yaml, YAML}) ->
    S = try fast_yaml:encode(YAML)
	catch _:_ -> YAML
	end,
    format_val(S);
format_val(I) when is_integer(I) ->
    integer_to_list(I);
format_val(B) when is_atom(B) ->
    erlang:atom_to_binary(B, utf8);
format_val(Term) ->
    S = try iolist_to_binary(Term)
	catch _:_ -> list_to_binary(io_lib:format("~p", [Term]))
	end,
    case binary:match(S, <<"\n">>) of
	nomatch -> S;
	_ -> [io_lib:nl(), S]
    end.

-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(TRef) when is_reference(TRef) ->
    case erlang:cancel_timer(TRef) of
	false ->
	    receive {timeout, TRef, _} -> ok
	    after 0 -> ok
	    end;
	_ ->
	    ok
    end;
cancel_timer(_) ->
    ok.

-spec best_match(atom() | binary() | string(),
		 [atom() | binary() | string()]) -> string().
best_match(Pattern, []) ->
    Pattern;
best_match(Pattern, Opts) ->
    String = to_string(Pattern),
    {Ds, _} = lists:mapfoldl(
		fun(Opt, Cache) ->
			SOpt = to_string(Opt),
			{Distance, Cache1} = ld(String, SOpt, Cache),
			{{Distance, SOpt}, Cache1}
		end, #{}, Opts),
    element(2, lists:min(Ds)).

-spec logical_processors() -> non_neg_integer().
logical_processors() ->
    case erlang:system_info(logical_processors) of
	V when is_integer(V), V >= 2  -> V;
	_ -> 1
    end.

-spec pmap(fun((T1) -> T2), [T1]) -> [T2].
pmap(Fun, [_,_|_] = List) ->
    case logical_processors() of
	1 -> lists:map(Fun, List);
	_ ->
	    Self = self(),
	    lists:map(
	      fun({Pid, Ref}) ->
		      receive
			  {Pid, Ret} ->
			      receive
				  {'DOWN', Ref, _, _, _} ->
				      Ret
			      end;
			  {'DOWN', Ref, _, _, Reason} ->
			      exit(Reason)
		      end
	      end, [spawn_monitor(
		      fun() -> Self ! {self(), Fun(X)} end)
		    || X <- List])
    end;
pmap(Fun, List) ->
    lists:map(Fun, List).

-spec peach(fun((T) -> any()), [T]) -> ok.
peach(Fun, [_,_|_] = List) ->
    case logical_processors() of
	1 -> lists:foreach(Fun, List);
	_ ->
	    Self = self(),
	    lists:foreach(
	      fun({Pid, Ref}) ->
		      receive
			  Pid ->
			      receive
				  {'DOWN', Ref, _, _, _} ->
				      ok
			      end;
			  {'DOWN', Ref, _, _, Reason} ->
			      exit(Reason)
		      end
	      end, [spawn_monitor(
		      fun() -> Fun(X), Self ! self() end)
		    || X <- List])
    end;
peach(Fun, List) ->
    lists:foreach(Fun, List).

-ifdef(HAVE_ERL_ERROR).
format_exception(Level, Class, Reason, Stacktrace) ->
    erl_error:format_exception(
      Level, Class, Reason, Stacktrace,
      fun(_M, _F, _A) -> false end,
      fun(Term, I) ->
	      io_lib:print(Term, I, 80, -1)
      end).
-else.
format_exception(Level, Class, Reason, Stacktrace) ->
    lib:format_exception(
      Level, Class, Reason, Stacktrace,
      fun(_M, _F, _A) -> false end,
      fun(Term, I) ->
	      io_lib:print(Term, I, 80, -1)
      end).
-endif.

-spec get_my_ipv4_address() -> inet:ip4_address().
get_my_ipv4_address() ->
    {ok, MyHostName} = inet:gethostname(),
    case inet:getaddr(MyHostName, inet) of
	{ok, Addr} -> Addr;
	{error, _} -> {127, 0, 0, 1}
    end.

-spec get_my_ipv6_address() -> inet:ip6_address().
get_my_ipv6_address() ->
    {ok, MyHostName} = inet:gethostname(),
    case inet:getaddr(MyHostName, inet6) of
	{ok, Addr} -> Addr;
	{error, _} -> {0, 0, 0, 0, 0, 0, 0, 1}
    end.

-spec parse_ip_mask(binary()) -> {ok, {inet:ip4_address(), 0..32}} |
				 {ok, {inet:ip6_address(), 0..128}} |
				 error.
parse_ip_mask(S) ->
    case econf:validate(econf:ip_mask(), S) of
	{ok, _} = Ret -> Ret;
	_ -> error
    end.

-spec match_ip_mask(inet:ip_address(), inet:ip_address(), 0..128) -> boolean().
match_ip_mask({_, _, _, _} = IP, {_, _, _, _} = Net, Mask) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot (1 bsl (32 - Mask) - 1),
    IPInt band M =:= NetInt band M;
match_ip_mask({_, _, _, _, _, _, _, _} = IP,
		{_, _, _, _, _, _, _, _} = Net, Mask) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot (1 bsl (128 - Mask) - 1),
    IPInt band M =:= NetInt band M;
match_ip_mask({_, _, _, _} = IP,
		{0, 0, 0, 0, 0, 16#FFFF, _, _} = Net, Mask) ->
    IPInt = ip_to_integer({0, 0, 0, 0, 0, 16#FFFF, 0, 0}) + ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot (1 bsl (128 - Mask) - 1),
    IPInt band M =:= NetInt band M;
match_ip_mask({0, 0, 0, 0, 0, 16#FFFF, _, _} = IP,
		{_, _, _, _} = Net, Mask) ->
    IPInt = ip_to_integer(IP) - ip_to_integer({0, 0, 0, 0, 0, 16#FFFF, 0, 0}),
    NetInt = ip_to_integer(Net),
    M = bnot (1 bsl (32 - Mask) - 1),
    IPInt band M =:= NetInt band M;
match_ip_mask(_, _, _) ->
    false.

-spec format_hosts_list([binary(), ...]) -> iolist().
format_hosts_list([Host]) ->
    Host;
format_hosts_list([H1, H2]) ->
    [H1, " and ", H2];
format_hosts_list([H1, H2, H3]) ->
    [H1, ", ", H2, " and ", H3];
format_hosts_list([H1, H2|Hs]) ->
    io_lib:format("~ts, ~ts and ~B more hosts",
		  [H1, H2, length(Hs)]).

-spec format_cycle([atom(), ...]) -> iolist().
format_cycle([M1]) ->
    atom_to_list(M1);
format_cycle([M1, M2]) ->
    [atom_to_list(M1), " and ", atom_to_list(M2)];
format_cycle([M|Ms]) ->
    atom_to_list(M) ++ ", " ++ format_cycle(Ms).

-spec delete_dir(file:filename_all()) -> ok | {error, file:posix()}.
delete_dir(Dir) ->
    try
	{ok, Entries} = file:list_dir(Dir),
	lists:foreach(fun(Path) ->
			      case filelib:is_dir(Path) of
				  true ->
				      ok = delete_dir(Path);
				  false ->
				      ok = file:delete(Path)
			      end
		      end, [filename:join(Dir, Entry) || Entry <- Entries]),
	ok = file:del_dir(Dir)
    catch
	_:{badmatch, {error, Error}} ->
	    {error, Error}
    end.

-spec semver_to_xxyy(binary()) -> binary().
semver_to_xxyy(<<Y1, Y2, $., M2, $., $0>>) ->
    <<Y1, Y2, $., $0, M2>>;
semver_to_xxyy(<<Y1, Y2, $., M2, $., Patch/binary>>) ->
    <<Y1, Y2, $., $0, M2, $., Patch/binary>>;
semver_to_xxyy(<<Y1, Y2, $., M1, M2, $., $0>>) ->
    <<Y1, Y2, $., M1, M2>>;
semver_to_xxyy(Version) when is_binary(Version) ->
    Version.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec url_encode(binary(), binary()) -> binary().
url_encode(<<H:8, T/binary>>, Acc) when
  (H >= $a andalso H =< $z) orelse
  (H >= $A andalso H =< $Z) orelse
  (H >= $0 andalso H =< $9) orelse
  H == $_ orelse
  H == $. orelse
  H == $- orelse
  H == $/ orelse
  H == $: ->
    url_encode(T, <<Acc/binary, H>>);
url_encode(<<H:8, T/binary>>, Acc) ->
    case integer_to_list(H, 16) of
	[X, Y] -> url_encode(T, <<Acc/binary, $%, X, Y>>);
	[X] -> url_encode(T, <<Acc/binary, $%, $0, X>>)
    end;
url_encode(<<>>, Acc) ->
    Acc.

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
	    ?ERROR_MSG("Failed to read file ~ts: ~ts",
		       [Path, file:format_error(Why)]),
	    Err
    end.

-spec get_dir(string()) -> file:filename().
get_dir(Type) ->
    Env = "EJABBERD_" ++ string:to_upper(Type) ++ "_PATH",
    case os:getenv(Env) of
	false ->
	    case code:priv_dir(ejabberd) of
		{error, _} -> filename:join(["priv", Type]);
		Path -> filename:join([Path, Type])
	    end;
	Path ->
	    Path
    end.

%% Generates erlang:timestamp() that is guaranteed to unique
-spec unique_timestamp() -> erlang:timestamp().
unique_timestamp() ->
    {MS, S, _} = erlang:timestamp(),
    {MS, S, erlang:unique_integer([positive, monotonic]) rem 1000000}.

%% Levenshtein distance
-spec ld(string(), string(), distance_cache()) -> {non_neg_integer(), distance_cache()}.
ld([] = S, T, Cache) ->
    {length(T), maps:put({S, T}, length(T), Cache)};
ld(S, [] = T, Cache) ->
    {length(S), maps:put({S, T}, length(S), Cache)};
ld([X|S], [X|T], Cache) ->
    ld(S, T, Cache);
ld([_|ST] = S, [_|TT] = T, Cache) ->
    try {maps:get({S, T}, Cache), Cache}
    catch _:{badkey, _} ->
            {L1, C1} = ld(S, TT, Cache),
            {L2, C2} = ld(ST, T, C1),
            {L3, C3} = ld(ST, TT, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, maps:put({S, T}, L, C3)}
    end.

-spec ip_to_integer(inet:ip_address()) -> non_neg_integer().
ip_to_integer({IP1, IP2, IP3, IP4}) ->
    IP1 bsl 8 bor IP2 bsl 8 bor IP3 bsl 8 bor IP4;
ip_to_integer({IP1, IP2, IP3, IP4, IP5, IP6, IP7,
	       IP8}) ->
    IP1 bsl 16 bor IP2 bsl 16 bor IP3 bsl 16 bor IP4 bsl 16
	bor IP5 bsl 16 bor IP6 bsl 16 bor IP7 bsl 16 bor IP8.

-spec to_string(atom() | binary() | string()) -> string().
to_string(A) when is_atom(A) ->
    atom_to_list(A);
to_string(B) when is_binary(B) ->
    binary_to_list(B);
to_string(S) ->
    S.

-ifdef(OTP_BELOW_27).
set_proc_label(_Label) ->
    ok.
-else.
set_proc_label(Label) ->
    proc_lib:set_label(Label).
-endif.

-ifdef(OTP_BELOW_25).
-spec lists_uniq([term()]) -> [term()].
lists_uniq(List) ->
    lists_uniq_int(List, #{}).

lists_uniq_int([El | Rest], Existing) ->
    case maps:is_key(El, Existing) of
        true -> lists_uniq_int(Rest, Existing);
        _ -> [El | lists_uniq_int(Rest, Existing#{El => true})]
    end;
lists_uniq_int([], _) ->
    [].
-else.
-spec lists_uniq([term()]) -> [term()].
lists_uniq(List) ->
    lists:uniq(List).
-endif.
