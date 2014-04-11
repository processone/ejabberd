%%%----------------------------------------------------------------------
%%% File    : ejabberd_web.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Purpose :
%%% Created : 28 Feb 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
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

-module(ejabberd_web).

-author('alexey@process-one.net').

%% External exports
-export([make_xhtml/1, make_xhtml/2, error/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

%% XXX bard: there are variants of make_xhtml in ejabberd_http and
%% ejabberd_web_admin.  It might be a good idea to centralize it here
%% and also create an ejabberd_web.hrl file holding the macros, so
%% that third parties can use ejabberd_web as an "utility" library.

make_xhtml(Els) -> make_xhtml([], Els).

make_xhtml(HeadEls, Els) ->
    #xmlel{name = <<"html">>,
	   attrs =
	       [{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>},
		{<<"xml:lang">>, <<"en">>}, {<<"lang">>, <<"en">>}],
	   children =
	       [#xmlel{name = <<"head">>, attrs = [],
		       children =
			   [#xmlel{name = <<"meta">>,
				   attrs =
				       [{<<"http-equiv">>, <<"Content-Type">>},
					{<<"content">>,
					 <<"text/html; charset=utf-8">>}],
				   children = []}
			    | HeadEls]},
		#xmlel{name = <<"body">>, attrs = [], children = Els}]}.

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

-define(LI(Els), ?XE(<<"li">>, Els)).

-define(A(URL, Els),
	?XAE(<<"a">>, [{<<"href">>, URL}], Els)).

-define(AC(URL, Text), ?A(URL, [?C(Text)])).

-define(P, ?X(<<"p">>)).

-define(BR, ?X(<<"br">>)).

-define(INPUT(Type, Name, Value),
	?XA(<<"input">>,
	    [{<<"type">>, Type}, {<<"name">>, Name},
	     {<<"value">>, Value}])).

error(not_found) ->
    {404, [],
     make_xhtml([?XC(<<"h1">>, <<"404 Not Found">>)])};
error(not_allowed) ->
    {401, [],
     make_xhtml([?XC(<<"h1">>, <<"401 Unauthorized">>)])}.
