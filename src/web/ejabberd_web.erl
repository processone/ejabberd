%%%----------------------------------------------------------------------
%%% File    : ejabberd_web.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 28 Feb 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_web).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([make_xhtml/1,
	 process_get/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").


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


process_get(#request{user = User,
		     path = ["admin" | RPath],
		     q = Query,
		     lang = Lang} = Request) ->
    if
	User /= undefined ->
	    case acl:match_rule(configure, jlib:make_jid(User, ?MYNAME, "")) of
		deny ->
		    {401, [], make_xhtml([?XC("h1", "Not Allowed")])};
		allow ->
		    ejabberd_web_admin:process_admin(
		      Request#request{path = RPath})
	    end;
	true ->
	    {401,
	     [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	     ejabberd_web:make_xhtml([{xmlelement, "h1", [],
				       [{xmlcdata, "401 Unauthorized"}]}])}
    end;

process_get(#request{user = User,
		     path = ["http-poll" | RPath],
		     q = Query,
		     lang = Lang} = Request) ->
    ejabberd_http_poll:process_request(Request#request{path = RPath});

process_get(_Request) ->
    {404, [], make_xhtml([?XC("h1", "Not found")])}.


