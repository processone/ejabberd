%%%----------------------------------------------------------------------
%%% File    : jlib.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : General XMPP library.
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(jlib).
-author('alexey@process-one.net').

-export([make_result_iq_reply/1,
	 make_error_reply/3,
	 make_error_reply/2,
	 make_error_element/2,
	 make_correct_from_to_attrs/3,
	 replace_from_to_attrs/3,
	 replace_from_to/3,
	 replace_from_attrs/2,
	 replace_from/2,
	 remove_attr/2,
	 make_jid/3,
	 make_jid/1,
	 string_to_jid/1,
	 jid_to_string/1,
	 is_nodename/1,
	 tolower/1,
	 nodeprep/1,
	 nameprep/1,
	 resourceprep/1,
	 jid_tolower/1,
	 jid_remove_resource/1,
	 jid_replace_resource/2,
	 get_iq_namespace/1,
	 iq_query_info/1,
	 iq_query_or_response_info/1,
	 is_iq_request_type/1,
	 iq_to_xml/1,
	 parse_xdata_submit/1,
	 timestamp_to_iso/1, % TODO: Remove once XEP-0091 is Obsolete
	 timestamp_to_iso/2,
	 timestamp_to_xml/4,
	 timestamp_to_xml/1, % TODO: Remove once XEP-0091 is Obsolete
	 now_to_utc_string/1,
	 now_to_local_string/1,
	 datetime_string_to_timestamp/1,
	 decode_base64/1,
	 encode_base64/1,
	 ip_to_list/1,
	 rsm_encode/1,
	 rsm_encode/2,
	 rsm_decode/1]).

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

replace_from_attrs(From, Attrs) ->
    Attrs1 = lists:keydelete("from", 1, Attrs),
    [{"from", From} | Attrs1].

replace_from(From, {xmlelement, Name, Attrs, Els}) ->
    NewAttrs = replace_from_attrs(jlib:jid_to_string(From), Attrs),
    {xmlelement, Name, NewAttrs, Els}.

remove_attr(Attr, {xmlelement, Name, Attrs, Els}) ->
    NewAttrs = lists:keydelete(Attr, 1, Attrs),
    {xmlelement, Name, NewAttrs, Els}.


make_jid(User, Server, Resource) ->
    case nodeprep(User) of
	error -> error;
	LUser ->
	    case nameprep(Server) of
		error -> error;
		LServer ->
		    case resourceprep(Resource) of
			error -> error;
			LResource ->
			    #jid{user = User,
				 server = Server,
				 resource = Resource,
				 luser = LUser,
				 lserver = LServer,
				 lresource = LResource}
		    end
	    end
    end.

make_jid({User, Server, Resource}) ->
    make_jid(User, Server, Resource).

string_to_jid(J) ->
    string_to_jid1(J, "").

string_to_jid1([$@ | _J], "") ->
    error;
string_to_jid1([$@ | J], N) ->
    string_to_jid2(J, lists:reverse(N), "");
string_to_jid1([$/ | _J], "") ->
    error;
string_to_jid1([$/ | J], N) ->
    string_to_jid3(J, "", lists:reverse(N), "");
string_to_jid1([C | J], N) ->
    string_to_jid1(J, [C | N]);
string_to_jid1([], "") ->
    error;
string_to_jid1([], N) ->
    make_jid("", lists:reverse(N), "").

%% Only one "@" is admitted per JID
string_to_jid2([$@ | _J], _N, _S) ->
    error;
string_to_jid2([$/ | _J], _N, "") ->
    error;
string_to_jid2([$/ | J], N, S) ->
    string_to_jid3(J, N, lists:reverse(S), "");
string_to_jid2([C | J], N, S) ->
    string_to_jid2(J, N, [C | S]);
string_to_jid2([], _N, "") ->
    error;
string_to_jid2([], N, S) ->
    make_jid(N, lists:reverse(S), "").

string_to_jid3([C | J], N, S, R) ->
    string_to_jid3(J, N, S, [C | R]);
string_to_jid3([], N, S, R) ->
    make_jid(N, S, lists:reverse(R)).

jid_to_string(#jid{user = User, server = Server, resource = Resource}) ->
    jid_to_string({User, Server, Resource});
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
    nodeprep(J) /= error.


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


nodeprep(S) when length(S) < 1024 ->
    R = stringprep:nodeprep(S),
    if
	length(R) < 1024 -> R;
	true -> error
    end;
nodeprep(_) ->
    error.

nameprep(S) when length(S) < 1024 ->
    R = stringprep:nameprep(S),
    if
	length(R) < 1024 -> R;
	true -> error
    end;
nameprep(_) ->
    error.

resourceprep(S) when length(S) < 1024 ->
    R = stringprep:resourceprep(S),
    if
	length(R) < 1024 -> R;
	true -> error
    end;
resourceprep(_) ->
    error.


jid_tolower(#jid{luser = U, lserver = S, lresource = R}) ->
    {U, S, R};
jid_tolower({U, S, R}) ->
    case nodeprep(U) of
	error -> error;
	LUser ->
	    case nameprep(S) of
		error -> error;
		LServer ->
		    case resourceprep(R) of
			error -> error;
			LResource ->
			    {LUser, LServer, LResource}
		    end
	    end
    end.

jid_remove_resource(#jid{} = JID) ->
    JID#jid{resource = "", lresource = ""};
jid_remove_resource({U, S, _R}) ->
    {U, S, ""}.

jid_replace_resource(JID, Resource) ->
    case resourceprep(Resource) of
	error -> error;
	LResource ->
	    JID#jid{resource = Resource, lresource = LResource}
    end.


get_iq_namespace({xmlelement, Name, _Attrs, Els}) when Name == "iq" ->
    case xml:remove_cdata(Els) of
	[{xmlelement, _Name2, Attrs2, _Els2}] ->
	    xml:get_attr_s("xmlns", Attrs2);
	_ ->
	    ""
    end;
get_iq_namespace(_) ->
    "".

%% @spec (xmlelement()) -> iq() | reply | invalid | not_iq

iq_query_info(El) ->
    iq_info_internal(El, request).

iq_query_or_response_info(El) ->
    iq_info_internal(El, any).

iq_info_internal({xmlelement, Name, Attrs, Els}, Filter) when Name == "iq" ->
    %% Filter is either request or any.  If it is request, any replies
    %% are converted to the atom reply.
    ID = xml:get_attr_s("id", Attrs),
    Type = xml:get_attr_s("type", Attrs),
    Lang = xml:get_attr_s("xml:lang", Attrs),
    {Type1, Class} = case Type of
			 "set" -> {set, request};
			 "get" -> {get, request};
			 "result" -> {result, reply};
			 "error" -> {error, reply};
			 _ -> {invalid, invalid}
		     end,
    if
	Type1 == invalid ->
	    invalid;
	Class == request; Filter == any ->
	    %% The iq record is a bit strange.  The sub_el field is an
	    %% XML tuple for requests, but a list of XML tuples for
	    %% responses.
	    FilteredEls = xml:remove_cdata(Els),
	    {XMLNS, SubEl} =
		case {Class, FilteredEls} of
		    {request, [{xmlelement, _Name2, Attrs2, _Els2}]} ->
			{xml:get_attr_s("xmlns", Attrs2),
			 hd(FilteredEls)};
		    {reply, _} ->
			%% Find the namespace of the first non-error
			%% element, if there is one.
			NonErrorEls = [El || 
					  {xmlelement, SubName, _, _} = El
					      <- FilteredEls, 
					  SubName /= "error"],
			{case NonErrorEls of
			     [NonErrorEl] ->
				 xml:get_tag_attr_s("xmlns", NonErrorEl);
			     _ ->
				 ""
			 end,
			 FilteredEls};
		    _ ->
			{"", []}
		end,
	    if XMLNS == "", Class == request ->
		    invalid;
	       true ->
		    #iq{id = ID,
			type = Type1,
			xmlns = XMLNS,
			lang = Lang,
			sub_el = SubEl}
	    end;
	Class == reply, Filter /= any ->
	    reply
    end;
iq_info_internal(_, _) ->
    not_iq.

is_iq_request_type(set) -> true;
is_iq_request_type(get) -> true;
is_iq_request_type(_) -> false.

iq_type_to_string(set) -> "set";
iq_type_to_string(get) -> "get";
iq_type_to_string(result) -> "result";
iq_type_to_string(error) -> "error";
iq_type_to_string(_) -> invalid.


iq_to_xml(#iq{id = ID, type = Type, sub_el = SubEl}) ->
    if
	ID /= "" ->
	    {xmlelement, "iq",
	     [{"id", ID}, {"type", iq_type_to_string(Type)}], SubEl};
	true ->
	    {xmlelement, "iq",
	     [{"type", iq_type_to_string(Type)}], SubEl}
    end.


parse_xdata_submit(El) ->
    {xmlelement, _Name, Attrs, Els} = El,
    case xml:get_attr_s("type", Attrs) of
	"submit" ->
	    lists:reverse(parse_xdata_fields(Els, []));
	"form" -> %% This is a workaround to accept Psi's wrong forms
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
parse_xdata_values([{xmlelement, Name, _Attrs, SubEls} | Els], Res) ->
    case Name of
	"value" ->
	    Val = xml:get_cdata(SubEls),
	    parse_xdata_values(Els, [Val | Res]);
	_ ->
	    parse_xdata_values(Els, Res)
    end;
parse_xdata_values([_ | Els], Res) ->
    parse_xdata_values(Els, Res).

rsm_decode(#iq{sub_el=SubEl})->
	rsm_decode(SubEl);
rsm_decode({xmlelement, _,_,_}=SubEl)->
	case xml:get_subtag(SubEl,"set") of
		false ->
			none;
		{xmlelement, "set", _Attrs, SubEls}->
			lists:foldl(fun rsm_parse_element/2, #rsm_in{}, SubEls)
	end.

rsm_parse_element({xmlelement, "max",[], _}=Elem, RsmIn)->
    CountStr = xml:get_tag_cdata(Elem),
    {Count, _} = string:to_integer(CountStr),
    RsmIn#rsm_in{max=Count};

rsm_parse_element({xmlelement, "before", [], _}=Elem, RsmIn)->
    UID = xml:get_tag_cdata(Elem),
    RsmIn#rsm_in{direction=before, id=UID};

rsm_parse_element({xmlelement, "after", [], _}=Elem, RsmIn)->
    UID = xml:get_tag_cdata(Elem),
    RsmIn#rsm_in{direction=aft, id=UID};

rsm_parse_element({xmlelement, "index",[], _}=Elem, RsmIn)->
    IndexStr = xml:get_tag_cdata(Elem),
    {Index, _} = string:to_integer(IndexStr),
    RsmIn#rsm_in{index=Index};


rsm_parse_element(_, RsmIn)->
    RsmIn.

rsm_encode(#iq{sub_el=SubEl}=IQ,RsmOut)->
    Set = {xmlelement, "set", [{"xmlns", ?NS_RSM}],
	   lists:reverse(rsm_encode_out(RsmOut))},
    {xmlelement, Name, Attrs, SubEls} = SubEl,
    New = {xmlelement, Name, Attrs, [Set | SubEls]},
    IQ#iq{sub_el=New}.

rsm_encode(none)->
    [];
rsm_encode(RsmOut)->
    [{xmlelement, "set", [{"xmlns", ?NS_RSM}], lists:reverse(rsm_encode_out(RsmOut))}].
rsm_encode_out(#rsm_out{count=Count, index=Index, first=First, last=Last})->
    El = rsm_encode_first(First, Index, []),
    El2 = rsm_encode_last(Last,El),
    rsm_encode_count(Count, El2).

rsm_encode_first(undefined, undefined, Arr) ->
    Arr;
rsm_encode_first(First, undefined, Arr) ->
    [{xmlelement, "first",[], [{xmlcdata, First}]}|Arr];
rsm_encode_first(First, Index, Arr) ->
    [{xmlelement, "first",[{"index", i2l(Index)}], [{xmlcdata, First}]}|Arr].

rsm_encode_last(undefined, Arr) -> Arr;
rsm_encode_last(Last, Arr) ->
    [{xmlelement, "last",[], [{xmlcdata, Last}]}|Arr].

rsm_encode_count(undefined, Arr)-> Arr;
rsm_encode_count(Count, Arr)->
    [{xmlelement, "count",[], [{xmlcdata, i2l(Count)}]} | Arr].

i2l(I) when is_integer(I) -> integer_to_list(I);
i2l(L) when is_list(L)    -> L.

%% Timezone = utc | {Sign::string(), {Hours, Minutes}} | {Hours, Minutes}
%% Hours = integer()
%% Minutes = integer()
timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}, Timezone) ->
    Timestamp_string =
	lists:flatten(
	  io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
			[Year, Month, Day, Hour, Minute, Second])),
    Timezone_string =
	case Timezone of
	    utc -> "Z";
	    {Sign, {TZh, TZm}} ->
		io_lib:format("~s~2..0w:~2..0w", [Sign, TZh, TZm]);
	    {TZh, TZm} ->
		Sign = case TZh >= 0 of
			   true -> "+";
			   false -> "-"
		       end,
		io_lib:format("~s~2..0w:~2..0w", [Sign, abs(TZh),TZm])
	end,
    {Timestamp_string, Timezone_string}.

timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
      io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
		    [Year, Month, Day, Hour, Minute, Second])).

timestamp_to_xml(DateTime, Timezone, FromJID, Desc) ->
    {T_string, Tz_string} = timestamp_to_iso(DateTime, Timezone),
    Text = [{xmlcdata, Desc}],
    From = jlib:jid_to_string(FromJID),
    {xmlelement, "delay",
     [{"xmlns", ?NS_DELAY},
      {"from", From},
      {"stamp", T_string ++ Tz_string}],
     Text}.

%% TODO: Remove this function once XEP-0091 is Obsolete
timestamp_to_xml({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    {xmlelement, "x",
     [{"xmlns", ?NS_DELAY91},
      {"stamp", lists:flatten(
		  io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
				[Year, Month, Day, Hour, Minute, Second]))}],
     []}.

now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
		    [Year, Month, Day, Hour, Minute, Second, MicroSecs])).

now_to_local_string({MegaSecs, Secs, MicroSecs}) ->
    LocalTime = calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
    UTCTime = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    Seconds = calendar:datetime_to_gregorian_seconds(LocalTime) -
            calendar:datetime_to_gregorian_seconds(UTCTime),
    {{H, M, _}, Sign} = if
			    Seconds < 0 ->
				{calendar:seconds_to_time(-Seconds), "-"};
			    true ->
				{calendar:seconds_to_time(Seconds), "+"}
    end,
    {{Year, Month, Day}, {Hour, Minute, Second}} = LocalTime,
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0w~s~2..0w:~2..0w",
		    [Year, Month, Day, Hour, Minute, Second, MicroSecs, Sign, H, M])).


% yyyy-mm-ddThh:mm:ss[.sss]{Z|{+|-}hh:mm} -> {MegaSecs, Secs, MicroSecs}
datetime_string_to_timestamp(TimeStr) ->
    case catch parse_datetime(TimeStr) of
	{'EXIT', _Err} ->
	    undefined;
	TimeStamp ->
	    TimeStamp
    end.

parse_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

% yyyy-mm-dd
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
	true ->
	    Date1;
	_ ->
	    false
    end.

% hh:mm:ss[.sss]TZD
parse_time(Time) ->
    case string:str(Time, "Z") of
	0 ->
	    parse_time_with_timezone(Time);
	_ ->
	    [T | _] = string:tokens(Time, "Z"),
	    {TT, MS} = parse_time1(T),
	    {TT, MS, 0, 0}
    end.

parse_time_with_timezone(Time) ->
    case string:str(Time, "+") of
	0 ->
	    case string:str(Time, "-") of
		0 ->
		    false;
		_ ->
		    parse_time_with_timezone(Time, "-")
	    end;
	_ ->
	    parse_time_with_timezone(Time, "+")
    end.

parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = string:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
	"-" ->
	    {TT, MS, -TZH, -TZM};
	"+" ->
	    {TT, MS, TZH, TZM}
    end.

parse_timezone(TZ) ->
    [H, M] = string:tokens(TZ, ":"),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.

parse_time1(Time) ->
    [HMS | T] =  string:tokens(Time, "."),
    MS = case T of
	     [] ->
		 0;
	     [Val] ->
		 list_to_integer(string:left(Val, 6, $0))
	 end,
    [H, M, S] = string:tokens(HMS, ":"),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
    {{H1, M1, S1}, MS}.

check_list(List) ->
    lists:mapfoldl(
      fun({L, N}, B)->
	  V = list_to_integer(L),
	  if
	      (V >= 0) and (V =< N) ->
		  {V, B};
	      true ->
		  {false, false}
	  end
      end, true, List).


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
    [Octet1|decode1_base64(Rest)];
decode1_base64([Sextet1,Sextet2,Sextet3,$=|Rest]) ->
    Bits3x6=
	(d(Sextet1) bsl 18) bor
	(d(Sextet2) bsl 12) bor
	(d(Sextet3) bsl 6),
    Octet1=Bits3x6 bsr 16,
    Octet2=(Bits3x6 bsr 8) band 16#ff,
    [Octet1,Octet2|decode1_base64(Rest)];
decode1_base64([Sextet1,Sextet2,Sextet3,Sextet4|Rest]) ->
    Bits4x6=
	(d(Sextet1) bsl 18) bor
	(d(Sextet2) bsl 12) bor
	(d(Sextet3) bsl 6) bor
	d(Sextet4),
    Octet1=Bits4x6 bsr 16,
    Octet2=(Bits4x6 bsr 8) band 16#ff,
    Octet3=Bits4x6 band 16#ff,
    [Octet1,Octet2,Octet3|decode1_base64(Rest)];
decode1_base64(_CatchAll) ->
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

%% Convert Erlang inet IP to list
ip_to_list({IP, _Port}) ->
    ip_to_list(IP);
ip_to_list({_,_,_,_,_,_,_,_} = Ipv6Address) ->
    inet_parse:ntoa(Ipv6Address);
%% This function clause could use inet_parse too:
ip_to_list({A,B,C,D}) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w",[A,B,C,D]));
ip_to_list(IP) ->
    lists:flatten(io_lib:format("~w", [IP])).
