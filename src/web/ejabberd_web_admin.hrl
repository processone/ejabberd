%%%----------------------------------------------------------------------
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

-define(X(Name), #xmlel{ns = ?NS_XHTML, name = Name}).
-define(XA(Name, Attrs), #xmlel{ns = ?NS_XHTML, name = Name, attrs = Attrs}).
-define(XE(Name, Els), #xmlel{ns = ?NS_XHTML, name = Name, children = Els}).
-define(XAE(Name, Attrs, Els), #xmlel{ns = ?NS_XHTML, name = Name,
    attrs = Attrs, children = Els}).
-define(C(Text), #xmlcdata{cdata = list_to_binary(Text)}).
-define(XC(Name, Text), ?XE(Name, [?C(Text)])).
-define(XAC(Name, Attrs, Text), ?XAE(Name, Attrs, [?C(Text)])).

-define(T(Text), translate:translate(Lang, Text)).
-define(CT(Text), ?C(?T(Text))).
-define(XCT(Name, Text), ?XC(Name, ?T(Text))).
-define(XACT(Name, Attrs, Text), ?XAC(Name, Attrs, ?T(Text))).

-define(LI(Els), ?XE('li', Els)).
-define(A(URL, Els), ?XAE('a', [exmpp_xml:attribute(<<"href">>, URL)], Els)).
-define(AC(URL, Text), ?A(URL, [?C(Text)])).
-define(ACT(URL, Text), ?AC(URL, ?T(Text))).
-define(P, ?X('p')).
-define(BR, ?X('br')).
-define(INPUT(Type, Name, Value),
	?XA('input', [exmpp_xml:attribute(<<"type">>, Type),
		      exmpp_xml:attribute(<<"name">>, Name),
		      exmpp_xml:attribute(<<"value">>, Value)])).

-define(INPUTT(Type, Name, Value), ?INPUT(Type, Name, ?T(Value))).
-define(INPUTS(Type, Name, Value, Size),
	?XA('input', [exmpp_xml:attribute(<<"type">>, Type),
		      exmpp_xml:attribute(<<"name">>, Name),
		      exmpp_xml:attribute(<<"value">>, Value),
		      exmpp_xml:attribute(<<"size">>, Size)])).
-define(INPUTST(Type, Name, Value, Size), ?INPUT(Type, Name, ?T(Value), Size)).
-define(ACLINPUT(Text), ?XE('td', [?INPUT("text", "value" ++ ID, Text)])).

-define(TEXTAREA(Name, Rows, Cols, Value),
	?XAC('textarea', [exmpp_xml:attribute(<<"name">>, list_to_binary(Name)),
			  exmpp_xml:attribute(<<"rows">>, list_to_binary(Rows)),
			  exmpp_xml:attribute(<<"cols">>, list_to_binary(Cols))],
	     Value)).

%% Build an xmlelement for result
-define(XRES(Text), ?XAC('p', [exmpp_xml:attribute(<<"class">>, <<"result">>)], Text)).
-define(XREST(Text), ?XRES(?T(Text))).

%% Guide Link
-define(GL(Ref, Title),
	?XAE('div',
	     [exmpp_xml:attribute(<<"class">>, <<"guidelink">>)],
	     [?XAE('a',
		   [exmpp_xml:attribute(<<"href">>, list_to_binary("/admin/doc/guide.html#"++ Ref)),
                     exmpp_xml:attribute(<<"target">>, <<"_blank">>)],
		   [?C("[Guide: " ++ Title ++ "]")])
	     ])).


%% h1 with a Guide Link
-define(H1GL(Name, Ref, Title), [?XC('h1', Name), ?GL(Ref, Title)]).
