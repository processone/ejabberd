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



process_get(#request{user = User,
		     path = ["config" | RPath],
		     q = Query,
		     lang = Lang} = Request) ->
    if
	User /= undefined ->
	    case acl:match_rule(configure, jlib:make_jid(User, ?MYNAME, "")) of
		deny ->
		    {401, [], make_xhtml([?XC("h1", "Not Allowed")])};
		allow ->
		    process_config(Request#request{path = RPath})
	    end;
	true ->
	    {401,
	     [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	     ejabberd_web:make_xhtml([{xmlelement, "h1", [],
				       [{xmlcdata, "401 Unauthorized"}]}])}
    end;

process_get(_Request) ->
    {404, [], make_xhtml([?XC("h1", "Not found")])}.



process_config(#request{user = User,
			path = [],
			q = Query,
			lang = Lang} = Request) ->
    make_xhtml([?XC("h1", "ejabberd configuration"),
		?XE("ul",
		    [?LI([?AC("acls/", "Access Control Lists")]),
		     ?LI([?AC("access/", "Access Rules")]),
		     ?LI([?AC("users/", "Users")]),
		     ?LI([?AC("nodes/", "Nodes")])
		    ])
	       ]);

process_config(#request{user = User,
			path = ["acls"],
			q = Query,
			lang = Lang} = Request) ->
    case acl:match_rule(configure, jlib:make_jid(User, ?MYNAME, "")) of
	deny ->
	    {401, [], make_xhtml([?XC("h1", "Not Allowed")])};
	allow ->
	    Res = case lists:keysearch("acls", 1, Query) of
		      {value, {_, String}} ->
			  case erl_scan:string(String) of
			      {ok, Tokens, _} ->
				  case erl_parse:parse_term(Tokens) of
				      {ok, NewACLs} ->
					  case acl:add_list(NewACLs, true) of
					      ok ->
						  ok;
					      _ ->
						  error
					  end;
				      _ ->
					  error
				  end;
			      _ ->
				  error
			  end;
		      _ ->
			  nothing
		  end,
	    ACLs = lists:flatten(io_lib:format("~p.", [ets:tab2list(acl)])),
	    make_xhtml([?XC("h1", "ejabberd ACLs configuration")] ++
		       case Res of
			   ok -> [?C("submited"), ?P];
			   error -> [?C("bad format"), ?P];
			   nothing -> []
		       end ++
		       [?XAE("form", [{"method", "post"}],
			     [?XAC("textarea", [{"name", "acls"},
						{"rows", "16"},
						{"cols", "80"}],
				   ACLs),
			      ?BR,
			      ?XA("input", [{"type", "submit"}])
			     ])
		       ])
    end;

process_config(_Request) ->
    {404, [], make_xhtml([?XC("h1", "Not found")])}.



