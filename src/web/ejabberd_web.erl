%%%----------------------------------------------------------------------
%%% File    : ejabberd_web.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose :
%%% Created : 28 Feb 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_web).
-author('alexey@sevcom.net').
-vsn('$Revision$  ').

%% External exports
-export([make_xhtml/1,
         error/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").


%% XXX bard: there are variants of make_xhtml in ejabberd_http and
%% ejabberd_web_admin.  It might be a good idea to centralize it here
%% and also create an ejabberd_web.hrl file holding the macros, so
%% that third parties can use ejabberd_web as an "utility" library.

make_xhtml(Els) ->
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"},
			  {"xml:lang", "en"},
			  {"lang", "en"}],
     [{xmlelement, "head", [],
       [{xmlelement, "meta", [{"http-equiv", "Content-Type"},
			      {"content", "text/html; charset=utf-8"}], []}]},
      {xmlelement, "body", [], Els}
     ]}.


-define(X(Name), {xmlelement, Name, [], []}).
-define(XA(Name, Attrs), {xmlelement, Name, Attrs, []}).
-define(XE(Name, Els), {xmlelement, Name, [], Els}).
-define(XAE(Name, Attrs, Els), {xmlelement, Name, Attrs, Els}).
-define(C(Text), {xmlcdata, Text}).
-define(XC(Name, Text), ?XE(Name, [?C(Text)])).
-define(XAC(Name, Attrs, Text), ?XAE(Name, Attrs, [?C(Text)])).

-define(LI(Els), ?XE("li", Els)).
-define(A(URL, Els), ?XAE("a", [{"href", URL}], Els)).
-define(AC(URL, Text), ?A(URL, [?C(Text)])).
-define(P, ?X("p")).
-define(BR, ?X("br")).
-define(INPUT(Type, Name, Value),
	?XA("input", [{"type", Type},
		      {"name", Name},
		      {"value", Value}])).

error(bad_request) ->
    {400, [], make_xhtml([?XC("h1", "400 Bad Request")])};
error(not_allowed) ->
    {401, [], make_xhtml([?XC("h1", "401 Unauthorized")])};
error(not_found) ->
    {404, [], make_xhtml([?XC("h1", "404 Not Found")])};
error(internal) ->
    {500, [], make_xhtml([?XC("h1", "500 Internal Error")])}.
