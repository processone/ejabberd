%%%----------------------------------------------------------------------
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
%%%----------------------------------------------------------------------

-define(X(Name),
	#xmlel{name = Name, attrs = [], children = []}).

-define(XA(Name, Attrs),
	#xmlel{name = Name, attrs = Attrs, children = []}).

-define(XE(Name, Els),
	#xmlel{name = Name, attrs = [], children = Els}).

-define(XAE(Name, Attrs, Els),
	#xmlel{name = Name, attrs = Attrs, children = Els}).

-define(C(Text), {xmlcdata, Text}).

-define(XC(Name, Text), ?XE(Name, [?C(Text)])).

-define(XAC(Name, Attrs, Text),
	?XAE(Name, Attrs, [?C(Text)])).

-define(T(Text), translate:translate(Lang, Text)).

-define(CT(Text), ?C((?T(Text)))).

-define(XCT(Name, Text), ?XC(Name, (?T(Text)))).

-define(XACT(Name, Attrs, Text),
	?XAC(Name, Attrs, (?T(Text)))).

-define(LI(Els), ?XE(<<"li">>, Els)).

-define(A(URL, Els),
	?XAE(<<"a">>, [{<<"href">>, URL}], Els)).

-define(AC(URL, Text), ?A(URL, [?C(Text)])).

-define(ACT(URL, Text), ?AC(URL, (?T(Text)))).

-define(P, ?X(<<"p">>)).

-define(BR, ?X(<<"br">>)).

-define(INPUT(Type, Name, Value),
	?XA(<<"input">>,
	    [{<<"type">>, Type}, {<<"name">>, Name},
	     {<<"value">>, Value}])).

-define(INPUTT(Type, Name, Value),
	?INPUT(Type, Name, (?T(Value)))).

-define(INPUTS(Type, Name, Value, Size),
	?XA(<<"input">>,
	    [{<<"type">>, Type}, {<<"name">>, Name},
	     {<<"value">>, Value}, {<<"size">>, Size}])).

-define(INPUTST(Type, Name, Value, Size),
	?INPUT(Type, Name, (?T(Value)), Size)).

-define(ACLINPUT(Text),
	?XE(<<"td">>,
	    [?INPUT(<<"text">>, <<"value", ID/binary>>, Text)])).

-define(TEXTAREA(Name, Rows, Cols, Value),
	?XAC(<<"textarea">>,
	     [{<<"name">>, Name}, {<<"rows">>, Rows},
	      {<<"cols">>, Cols}],
	     Value)).

%% Build an xmlelement for result
-define(XRES(Text),
	?XAC(<<"p">>, [{<<"class">>, <<"result">>}], Text)).

%% Guide Link
-define(XREST(Text), ?XRES((?T(Text)))).

-define(GL(Ref, Title),
	?XAE(<<"div">>, [{<<"class">>, <<"guidelink">>}],
	     [?XAE(<<"a">>,
		   [{<<"href">>, <<"/admin/doc/guide.html#", Ref/binary>>},
		    {<<"target">>, <<"_blank">>}],
		   [?C(<<"[Guide: ", Title/binary, "]">>)])])).

%% h1 with a Guide Link
-define(H1GL(Name, Ref, Title),
	[?XC(<<"h1">>, Name), ?GL(Ref, Title)]).
