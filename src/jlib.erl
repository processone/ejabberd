%%%----------------------------------------------------------------------
%%% File    : jlib.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(jlib).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([make_result_iq_reply/1,
	 make_error_reply/3,
	 make_error_reply/2,
	 make_error_element/2,
	 make_correct_from_to_attrs/3,
	 replace_from_to_attrs/3,
	 replace_from_to/3,
	 remove_attr/2,
	 string_to_jid/1,
	 jid_to_string/1,
	 is_nodename/1,
	 tolower/1,
	 jid_tolower/1,
	 jid_remove_resource/1,
	 get_iq_namespace/1,
	 iq_query_info/1,
	 is_iq_request_type/1,
	 iq_to_xml/1,
	 parse_xdata_submit/1,
	 timestamp_to_iso/1,
	 timestamp_to_xml/1,
	 decode_base64/1,
	 encode_base64/1]).

-include("jlib.hrl").

%send_iq(From, To, ID, SubTags) ->
%    ok.

make_result_iq_reply({xmlelement, Name, Attrs, SubTags}) ->
    NewAttrs = make_result_iq_reply_attrs(Attrs),
    {xmlelement, Name, NewAttrs, SubTags}.

make_result_iq_reply_attrs(Attrs) ->
    To = xml:get_attr("to", Attrs),
    From = xml:get_attr("from", Attrs),
    Attrs1 = lists:keydelete("to", 1, Attrs),
    Attrs2 = lists:keydelete("from", 1, Attrs1),
    Attrs3 = case To of
		 {value, ToVal} ->
		      [{"from", ToVal} | Attrs2];
		 _ ->
		     Attrs2
	     end,
    Attrs4 = case From of
		 {value, FromVal} ->
		      [{"to", FromVal} | Attrs3];
		 _ ->
		     Attrs3
	     end,
    Attrs5 = lists:keydelete("type", 1, Attrs4),
    Attrs6 = [{"type", "result"} | Attrs5],
    Attrs6.

make_error_reply({xmlelement, Name, Attrs, SubTags}, Code, Desc) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    {xmlelement, Name, NewAttrs, SubTags ++ [{xmlelement, "error",
					      [{"code", Code}],
					      [{xmlcdata, Desc}]}]}.

make_error_reply({xmlelement, Name, Attrs, SubTags}, Error) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    {xmlelement, Name, NewAttrs, SubTags ++ [Error]}.

make_error_reply_attrs(Attrs) ->
    To = xml:get_attr("to", Attrs),
    From = xml:get_attr("from", Attrs),
    Attrs1 = lists:keydelete("to", 1, Attrs),
    Attrs2 = lists:keydelete("from", 1, Attrs1),
    Attrs3 = case To of
		 {value, ToVal} ->
		      [{"from", ToVal} | Attrs2];
		 _ ->
		     Attrs2
	     end,
    Attrs4 = case From of
		 {value, FromVal} ->
		      [{"to", FromVal} | Attrs3];
		 _ ->
		     Attrs3
	     end,
    Attrs5 = lists:keydelete("type", 1, Attrs4),
    Attrs6 = [{"type", "error"} | Attrs5],
    Attrs6.

make_error_element(Code, Desc) ->
    {xmlelement, "error",
     [{"code", Code}],
     [{xmlcdata, Desc}]}.

make_correct_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete("from", 1, Attrs),
    Attrs2 = case xml:get_attr("to", Attrs) of
		 {value, _} ->
		     Attrs1;
		 _ ->
		     [{"to", To} | Attrs1]
	     end,
    Attrs3 = [{"from", From} | Attrs2],
    Attrs3.


replace_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete("to", 1, Attrs),
    Attrs2 = lists:keydelete("from", 1, Attrs1),
    Attrs3 = [{"to", To} | Attrs2],
    Attrs4 = [{"from", From} | Attrs3],
    Attrs4.

replace_from_to(From, To, {xmlelement, Name, Attrs, Els}) ->
    NewAttrs = replace_from_to_attrs(jlib:jid_to_string(From),
				     jlib:jid_to_string(To),
				     Attrs),
    {xmlelement, Name, NewAttrs, Els}.


remove_attr(Attr, {xmlelement, Name, Attrs, Els}) ->
    NewAttrs = lists:keydelete(Attr, 1, Attrs),
    {xmlelement, Name, NewAttrs, Els}.

string_to_jid(J) ->
    string_to_jid1(J, "").

string_to_jid1([$@ | J], "") ->
    error;
string_to_jid1([$@ | J], N) ->
    string_to_jid2(J, lists:reverse(N), "");
string_to_jid1([$/ | J], "") ->
    error;
string_to_jid1([$/ | J], N) ->
    string_to_jid3(J, "", lists:reverse(N), "");
string_to_jid1([C | J], N)
  when (C =< 32) or
       (C == $") or
       (C == $&) or
       (C == $') or
       (C == $:) or
       (C == $<) or
       (C == $>) or
       (C == 127)
       ->
    error;
string_to_jid1([C | J], N) ->
    string_to_jid1(J, [C | N]);
string_to_jid1([], "") ->
    error;
string_to_jid1([], N) ->
    {"", lists:reverse(N), ""}.

string_to_jid2([$/ | J], N, "") ->
    error;
string_to_jid2([$/ | J], N, S) ->
    string_to_jid3(J, N, lists:reverse(S), "");
string_to_jid2([C | J], N, S) ->
    string_to_jid2(J, N, [C | S]);
string_to_jid2([], N, "") ->
    error;
string_to_jid2([], N, S) ->
    {N, lists:reverse(S), ""}.

string_to_jid3([C | J], N, S, R) ->
    string_to_jid3(J, N, S, [C | R]);
string_to_jid3([], N, S, R) ->
    {N, S, lists:reverse(R)}.

jid_to_string({Node, Server, Resource}) ->
    S1 = case Node of
	     "" ->
		 "";
	     _ ->
		 Node ++ "@"
	 end,
    S2 = S1 ++ Server,
    S3 = case Resource of
	     "" ->
		 S2;
	     _ ->
		 S2 ++ "/" ++ Resource
	 end,
    S3.


is_nodename([]) ->
    false;
is_nodename(J) ->
    is_nodename1(J).

is_nodename1([C | J])
  when (C =< 32) or
       (C == $") or
       (C == $&) or
       (C == $') or
       (C == $:) or
       (C == $<) or
       (C == $>) or
       (C == $@) or
       (C == $/) or
       (C == 127)
       ->
    false;
is_nodename1([C | J]) ->
    is_nodename1(J);
is_nodename1([]) ->
    true.



% TODO: UNICODE support
%tolower_c(C) when C >= $A, C =< $Z ->
%    C + 32;
%tolower_c(C) ->
%    C.

-define(LOWER(Char),
        if
            Char >= $A, Char =< $Z ->
                Char + 32;
            true ->
                Char
        end).

%tolower(S) ->
%    lists:map(fun tolower_c/1, S).

%tolower(S) ->
%    [?LOWER(Char) || Char <- S].

% Not tail-recursive but it seems works faster than variants above
tolower([C | Cs]) ->
    if
	C >= $A, C =< $Z ->
	    [C + 32 | tolower(Cs)];
	true ->
	    [C | tolower(Cs)]
    end;
tolower([]) ->
    [].

%tolower([C | Cs]) when C >= $A, C =< $Z ->
%    [C + 32 | tolower(Cs)];
%tolower([C | Cs]) ->
%    [C | tolower(Cs)];
%tolower([]) ->
%    [].



jid_tolower({U, S, R}) ->
    {tolower(U), tolower(S), R}.

jid_remove_resource({U, S, R}) ->
    {U, S, ""}.

get_iq_namespace({xmlelement, Name, Attrs, Els}) when Name == "iq" ->
    case xml:remove_cdata(Els) of
	[{xmlelement, Name2, Attrs2, Els2}] ->
	    xml:get_attr_s("xmlns", Attrs2);
	_ ->
	    ""
    end;
get_iq_namespace(_) ->
    "".

iq_query_info({xmlelement, Name, Attrs, Els}) when Name == "iq" ->
    ID = xml:get_attr_s("id", Attrs),
    Type = xml:get_attr_s("type", Attrs),
    case xml:remove_cdata(Els) of
	[{xmlelement, Name2, Attrs2, Els2}] ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs2),
	    Type1 = case Type of
			"set" -> set;
			"get" -> get;
			"result" -> reply;
			"error" -> reply;
			_ -> invalid
		    end,
	    if
		(Type1 /= invalid) and (Type1 /= reply) and (XMLNS /= "") ->
		    {iq, ID, Type1, XMLNS, {xmlelement, Name2, Attrs2, Els2}};
		true ->
		    if
			Type1 == reply ->
			    reply;
			true ->
			    invalid
		    end
	    end;
	_ ->
	    invalid
    end;
iq_query_info(_) ->
    not_iq.

is_iq_request_type(set) -> true;
is_iq_request_type(get) -> true;
is_iq_request_type(_) -> false.

iq_type_to_string(set) -> "set";
iq_type_to_string(get) -> "get";
iq_type_to_string(result) -> "result";
iq_type_to_string(error) -> "error";
iq_type_to_string(_) -> invalid.


iq_to_xml({iq, ID, Type, _, SubEl}) ->
    if
	ID /= "" ->
	    {xmlelement, "iq",
	     [{"id", ID}, {"type", iq_type_to_string(Type)}], SubEl};
	true ->
	    {xmlelement, "iq",
	     [{"type", iq_type_to_string(Type)}], SubEl}
    end.


parse_xdata_submit(El) ->
    {xmlelement, Name, Attrs, Els} = El,
    case xml:get_attr_s("type", Attrs) of
	"submit" ->
	    lists:reverse(parse_xdata_fields(Els, []));
	_ ->
	    invalid
    end.

parse_xdata_fields([], Res) ->
    Res;
parse_xdata_fields([{xmlelement, Name, Attrs, SubEls} | Els], Res) ->
    case Name of
	"field" ->
	    case xml:get_attr_s("var", Attrs) of
		"" ->
		    parse_xdata_fields(Els, Res);
		Var ->
		    Field =
			{Var, lists:reverse(parse_xdata_values(SubEls, []))},
		    parse_xdata_fields(Els, [Field | Res])
	    end;
	_ ->
	    parse_xdata_fields(Els, Res)
    end;
parse_xdata_fields([_ | Els], Res) ->
    parse_xdata_fields(Els, Res).

parse_xdata_values([], Res) ->
    Res;
parse_xdata_values([{xmlelement, Name, Attrs, SubEls} | Els], Res) ->
    case Name of
	"value" ->
	    Val = xml:get_cdata(SubEls),
	    parse_xdata_values(Els, [Val | Res]);
	_ ->
	    parse_xdata_values(Els, Res)
    end;
parse_xdata_values([_ | Els], Res) ->
    parse_xdata_values(Els, Res).


timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
      io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
		    [Year, Month, Day, Hour, Minute, Second])).

timestamp_to_xml({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    {xmlelement, "x",
     [{"xmlns", ?NS_DELAY},
      {"stamp", lists:flatten(
		  io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
				[Year, Month, Day, Hour, Minute, Second]))}],
     []}.


%
% Base64 stuff (based on httpd_util.erl)
%

decode_base64(S) ->
    decode1_base64([C || C <- S,
			 C /= $ ,
			 C /= $\t,
			 C /= $\n,
			 C /= $\r]).

decode1_base64([]) ->
    [];
decode1_base64([Sextet1,Sextet2,$=,$=|Rest]) ->
    Bits2x6=
	(d(Sextet1) bsl 18) bor
	(d(Sextet2) bsl 12),
    Octet1=Bits2x6 bsr 16,
    [Octet1|decode_base64(Rest)];
decode1_base64([Sextet1,Sextet2,Sextet3,$=|Rest]) ->
    Bits3x6=
	(d(Sextet1) bsl 18) bor
	(d(Sextet2) bsl 12) bor
	(d(Sextet3) bsl 6),
    Octet1=Bits3x6 bsr 16,
    Octet2=(Bits3x6 bsr 8) band 16#ff,
    [Octet1,Octet2|decode_base64(Rest)];
decode1_base64([Sextet1,Sextet2,Sextet3,Sextet4|Rest]) ->
    Bits4x6=
	(d(Sextet1) bsl 18) bor
	(d(Sextet2) bsl 12) bor
	(d(Sextet3) bsl 6) bor
	d(Sextet4),
    Octet1=Bits4x6 bsr 16,
    Octet2=(Bits4x6 bsr 8) band 16#ff,
    Octet3=Bits4x6 band 16#ff,
    [Octet1,Octet2,Octet3|decode_base64(Rest)];
decode1_base64(CatchAll) ->
    "".

d(X) when X >= $A, X =<$Z ->
    X-65;
d(X) when X >= $a, X =<$z ->
    X-71;
d(X) when X >= $0, X =<$9 ->
    X+4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.


encode_base64([]) ->
    [];
encode_base64([A]) ->
    [e(A bsr 2), e((A band 3) bsl 4), $=, $=];
encode_base64([A,B]) ->
    [e(A bsr 2), e(((A band 3) bsl 4) bor (B bsr 4)), e((B band 15) bsl 2), $=];
encode_base64([A,B,C|Ls]) ->
    encode_base64_do(A,B,C, Ls).
encode_base64_do(A,B,C, Rest) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [e(BB bsr 18), e((BB bsr 12) band 63), 
     e((BB bsr 6) band 63), e(BB band 63)|encode_base64(Rest)].

e(X) when X >= 0, X < 26 -> X+65;
e(X) when X>25, X<52 ->     X+71;
e(X) when X>51, X<62 ->     X-4;
e(62) ->                    $+;
e(63) ->                    $/;
e(X) ->                     exit({bad_encode_base64_token, X}).
