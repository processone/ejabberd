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
	 make_correct_from_to_attrs/3,
	 replace_from_to_attrs/3,
	 replace_from_to/3,
	 remove_attr/2,
	 string_to_jid/1,
	 jid_to_string/1,
	 tolower/1,
	 get_iq_namespace/1,
	 iq_query_info/1,
	 iq_to_xml/1,
	 get_subtag/2]).


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



% TODO: UNICODE support
tolower_c(C) when C >= $A, C =< $Z ->
    C + 32;
tolower_c(C) ->
    C.

tolower(S) ->
    lists:map(fun tolower_c/1, S).



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
			_ -> invalid
		    end,
	    if
		(Type1 /= invalid) and (XMLNS /= "") ->
		    {iq, ID, Type1, XMLNS, {xmlelement, Name2, Attrs2, Els2}};
		true ->
		    invalid
	    end;
	_ ->
	    invalid
    end;
iq_query_info(_) ->
    not_iq.

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


get_subtag({xmlelement, _, _, Els}, Name) ->
    get_subtag1(Els, Name).

get_subtag1([El | Els], Name) ->
    case El of
	{xmlelement, Name, _, _} ->
	    El;
	_ ->
	    get_subtag1(Els, Name)
    end;
get_subtag1([], _) ->
    false.


