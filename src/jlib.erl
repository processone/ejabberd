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

-export([make_error_iq_reply/3, make_correct_from_to_attrs/3,
	 replace_from_to_attrs/3, string_to_jid/1, tolower/1]).


%send_iq(From, To, ID, SubTags) ->
%    ok.

make_error_iq_reply({xmlelement, Name, Attrs, SubTags}, Code, Desc)
  when Name == "iq" ->
    NewAttrs = make_error_iq_reply_attrs(Attrs),
    {xmlelement, Name, NewAttrs, SubTags ++ [{xmlelement, "error",
					      [{"code", Code}],
					      [{xmlcdata, Desc}]}]}.

make_error_iq_reply_attrs(Attrs) ->
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


replace_from_to_attrs(From,To,Attrs) ->
    Attrs1 = lists:keydelete("to", 1, Attrs),
    Attrs2 = lists:keydelete("from", 1, Attrs1),
    Attrs3 = [{"to", To} | Attrs2],
    Attrs4 = [{"from", From} | Attrs3],
    Attrs4.


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


% TODO: UNICODE support
tolower_c(C) when C >= $A, C =< $Z ->
    C + 32;
tolower_c(C) ->
    C.

tolower(S) ->
    lists:map(fun tolower_c/1, S).

