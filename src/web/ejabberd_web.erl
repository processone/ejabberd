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
		     path = ["admin" | RPath],
		     q = Query,
		     lang = Lang} = Request) ->
    if
	User /= undefined ->
	    case acl:match_rule(configure, jlib:make_jid(User, ?MYNAME, "")) of
		deny ->
		    {401, [], make_xhtml([?XC("h1", "Not Allowed")])};
		allow ->
		    process_admin(Request#request{path = RPath})
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



process_admin(#request{user = User,
			path = [],
			q = Query,
			lang = Lang} = Request) ->
    make_xhtml([?XC("h1", "ejabberd administration"),
		?XE("ul",
		    [?LI([?AC("acls/", "Access Control Lists"), ?C(" "),
			  ?AC("acls-raw/", "(raw)")]),
		     ?LI([?AC("access/", "Access Rules"), ?C(" "),
			  ?AC("access-raw/", "(raw)")]),
		     ?LI([?AC("users/", "Users")]),
		     ?LI([?AC("nodes/", "Nodes")]),
		     ?LI([?AC("stats/", "Statistics")])
		    ])
	       ]);

process_admin(#request{user = User,
			path = ["acls-raw"],
			q = Query,
			lang = Lang} = Request) ->
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
	       ]);

process_admin(#request{method = Method,
			user = User,
			path = ["acls"],
			q = Query,
			lang = Lang} = Request) ->
    ?INFO_MSG("query: ~p", [Query]),
    Res = case Method of
	      'POST' ->
		  case catch acl_parse_query(Query) of
		      {'EXIT', _} ->
			  error;
		      NewACLs ->
			  ?INFO_MSG("NewACLs: ~p", [NewACLs]),
			  case acl:add_list(NewACLs, true) of
			      ok ->
				  ?INFO_MSG("NewACLs: ok", []),
				  ok;
			      _ ->
				  error
			  end
		  end;
	      _ ->
		  nothing
	  end,
    ACLs = lists:keysort(2, ets:tab2list(acl)),
    make_xhtml([?XC("h1", "ejabberd ACLs configuration")] ++
	       case Res of
		   ok -> [?C("submited"), ?P];
		   error -> [?C("bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"method", "post"}],
		     [acls_to_xhtml(ACLs),
		      ?BR,
		      ?XA("input", [{"type", "submit"},
				    {"name", "delete"},
				    {"value", "Delete Selected"}]),
		      ?C(" "),
		      ?XA("input", [{"type", "submit"},
				    {"name", "submit"},
				    {"value", "Submit"}])
		     ])
	       ]);

process_admin(#request{user = User,
			path = ["access-raw"],
			q = Query,
			lang = Lang} = Request) ->
    SetAccess =
	fun(Rs) ->
		mnesia:transaction(
		  fun() ->
			  Os = mnesia:select(config,
					     [{{config, {access, '$1'}, '$2'},
					       [],
					       ['$_']}]),
			  lists:foreach(fun(O) ->
						mnesia:delete_object(O)
					end, Os),
			  lists:foreach(
			    fun({access, Name, Rules}) ->
				    mnesia:write({config,
						  {access, Name},
						  Rules})
			    end, Rs)
		  end)
	end,
    Res = case lists:keysearch("access", 1, Query) of
	      {value, {_, String}} ->
		  case erl_scan:string(String) of
		      {ok, Tokens, _} ->
			  case erl_parse:parse_term(Tokens) of
			      {ok, Rs} ->
				  case SetAccess(Rs) of
				      {atomic, _} ->
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
    Access =
	lists:flatten(
	  io_lib:format(
	    "~p.", [ets:select(config,
			       [{{config, {access, '$1'}, '$2'},
				 [],
				 [{{access, '$1', '$2'}}]}])])),
    make_xhtml([?XC("h1", "ejabberd access rules configuration")] ++
	       case Res of
		   ok -> [?C("submited"), ?P];
		   error -> [?C("bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"method", "post"}],
		     [?XAC("textarea", [{"name", "access"},
					{"rows", "16"},
					{"cols", "80"}],
			   Access),
		      ?BR,
		      ?XA("input", [{"type", "submit"}])
		     ])
	       ]);

process_admin(#request{method = Method,
			user = User,
			path = ["access"],
			q = Query,
			lang = Lang} = Request) ->
    ?INFO_MSG("query: ~p", [Query]),
    Res = nothing,
    AccessRules =
	ets:select(config,
		   [{{config, {access, '$1'}, '$2'},
		     [],
		     [{{access, '$1', '$2'}}]}]),
    make_xhtml([?XC("h1", "ejabberd access rules configuration")] ++
	       case Res of
		   ok -> [?C("submited"), ?P];
		   error -> [?C("bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"method", "post"}],
		     [access_rules_to_xhtml(AccessRules),
		      ?BR,
		      ?XA("input", [{"type", "submit"},
				    {"name", "delete"},
				    {"value", "Delete Selected"}])
		     ])
	       ]);

process_admin(#request{user = User,
			path = ["users"],
			q = Query,
			lang = Lang} = Request) ->
    Res = list_users(),
    make_xhtml([?XC("h1", "ejabberd users")] ++ Res);

process_admin(#request{user = User,
			path = ["users", Diap],
			q = Query,
			lang = Lang} = Request) ->
    Res = list_users_in_diapason(Diap),
    make_xhtml([?XC("h1", "ejabberd users")] ++ Res);

process_admin(#request{user = User,
			path = ["stats"],
			q = Query,
			lang = Lang} = Request) ->
    Res = get_stats(),
    make_xhtml([?XC("h1", "ejabberd stats")] ++ Res);

process_admin(_Request) ->
    {404, [], make_xhtml([?XC("h1", "Not found")])}.



acls_to_xhtml(ACLs) ->
    ?XAE("table", [],
	 [?XE("tbody",
	      lists:map(
		fun({acl, Name, Spec} = ACL) ->
			SName = atom_to_list(Name),
			ID = term_to_id(ACL),
			?XE("tr",
			    [?XE("td",
				 [?XA("input", [{"type", "checkbox"},
						{"name", "selected"},
						{"value", ID}])]),
			     ?XC("td", SName)] ++
			    acl_spec_to_xhtml(ID, Spec)
			   )
		end, ACLs) ++
	      [?XE("tr",
		   [?X("td"),
		    ?XE("td", 
			[?XA("input", [{"type", "text"},
				       {"name", "namenew"},
				       {"value", ""}])]
		       )] ++
		   acl_spec_to_xhtml("new", {user, ""})
		  )]
	     )]).

-define(ACLINPUT(Text), ?XE("td", [?XA("input", [{"type", "text"},
						 {"name", "value" ++ ID},
						 {"value", Text}])])).

acl_spec_to_text({user, U}) ->
    {user, U};

acl_spec_to_text({server, S}) ->
    {server, S};

acl_spec_to_text({user, U, S}) ->
    {user, U ++ "@" ++ S};

acl_spec_to_text(Spec) ->
    {raw, term_to_string(Spec)}.

acl_spec_to_xhtml(ID, Spec) ->
    {Type, Str} = acl_spec_to_text(Spec),
    [acl_spec_select(ID, Type), ?ACLINPUT(Str)].

acl_spec_select(ID, Opt) ->
    ?XE("td",
	[?XAE("select", [{"name", "type" ++ ID}],
	      lists:map(
		fun(O) ->
			Sel = if
				  O == Opt -> [{"selected", "selected"}];
				  true -> []
			      end,
			?XAC("option",
			     Sel ++ [{"value", atom_to_list(O)}],
			     atom_to_list(O))
		end, [user, server, user_server, raw]))]).


term_to_string(T) ->
    lists:flatten(io_lib:format("~1000000p", [T])).

term_to_id(T) ->
    jlib:encode_base64(binary_to_list(term_to_binary(T))).


acl_parse_query(Query) ->
    ACLs = ets:tab2list(acl),
    case lists:keysearch("submit", 1, Query) of
	{value, _} ->
	    acl_parse_submit(ACLs, Query);
	_ ->
	    case lists:keysearch("delete", 1, Query) of
		{value, _} ->
		    acl_parse_delete(ACLs, Query)
	    end
    end.

acl_parse_submit(ACLs, Query) ->
    NewACLs =
	lists:map(
	  fun({acl, Name, Spec} = ACL) ->
		  SName = atom_to_list(Name),
		  ID = term_to_id(ACL),
		  case {lists:keysearch("type" ++ ID, 1, Query),
			lists:keysearch("value" ++ ID, 1, Query)} of
		      {{value, {_, T}}, {value, {_, V}}} ->
			  {Type, Str} = acl_spec_to_text(Spec),
			  case {atom_to_list(Type), Str} of
			      {T, V} ->
				  ACL;
			      _ ->
				  NewSpec = string_to_spec(T, V),
				  {acl, Name, NewSpec}
			  end;
		      _ ->
			  ACL
		  end
	  end, ACLs),
    NewACL = case {lists:keysearch("namenew", 1, Query),
		   lists:keysearch("typenew", 1, Query),
		   lists:keysearch("valuenew", 1, Query)} of
		 {{value, {_, ""}}, _, _} ->
		     [];
		 {{value, {_, N}}, {value, {_, T}}, {value, {_, V}}} ->
		     NewName = list_to_atom(N),
		     NewSpec = string_to_spec(T, V),
		     [{acl, NewName, NewSpec}];
		 _ ->
		     []
	     end,
    NewACLs ++ NewACL.

string_to_spec("user", Val) ->
    {user, Val};
string_to_spec("server", Val) ->
    {server, Val};
string_to_spec("user_server", Val) ->
    #jid{luser = U, lserver = S, resource = ""} = jlib:string_to_jid(Val),
    {user_server, U, S};
string_to_spec("raw", Val) ->
    {ok, Tokens, _} = erl_scan:string(Val ++ "."),
    {ok, NewSpec} = erl_parse:parse_term(Tokens),
    NewSpec.


acl_parse_delete(ACLs, Query) ->
    NewACLs =
	lists:filter(
	  fun({acl, Name, Spec} = ACL) ->
		  ID = term_to_id(ACL),
		  not lists:member({"selected", ID}, Query)
	  end, ACLs),
    NewACLs.


access_rules_to_xhtml(AccessRules) ->
    ?XAE("table", [],
	 [?XE("tbody",
	      lists:map(
		fun({access, Name, Rules} = Access) ->
			SName = atom_to_list(Name),
			ID = term_to_id(Access),
			?XE("tr",
			    [?XE("td",
				 [?XA("input", [{"type", "checkbox"},
						{"name", "selected"},
						{"value", ID}])]),
			     ?XE("td", [?AC(SName ++ "/", SName)]),
			     ?XC("td", term_to_string(Rules))
			    ]
			   )
		end, AccessRules) ++
	      [?XE("tr",
		   [?X("td"),
		    ?XE("td",
			[?XA("input", [{"type", "text"},
				       {"name", "namenew"},
				       {"value", ""}])]
		       ),
		    ?XE("td",
			[?XA("input", [{"type", "submit"},
				       {"name", "addnew"},
				       {"value", "Add New"}])])
		   ]
		  )]
	     )]).




list_users() ->
    Users = ejabberd_auth:dirty_get_registered_users(),
    SUsers = lists:sort(Users),
    case length(SUsers) of
	N when N =< 100 ->
	    lists:flatmap(
	      fun(U) ->
		      [?AC("../user/" ++ U ++ "/", U), ?BR]
	      end, SUsers);
	N ->
	    NParts = trunc(math:sqrt(N * 0.618)) + 1,
	    M = trunc(N / NParts) + 1,
	    lists:flatmap(
	      fun(K) ->
		      L = K + M - 1,
		      Node = integer_to_list(K) ++ "-" ++ integer_to_list(L),
		      Last = if L < N -> lists:nth(L, SUsers);
				true -> lists:last(SUsers)
			     end,
		      Name = 
			  lists:nth(K, SUsers) ++ [$\s, 226, 128, 148, $\s] ++
			  Last,
		      [?AC(Node ++ "/", Name), ?BR]
	      end, lists:seq(1, N, M))
    end.

list_users_in_diapason(Diap) ->
    Users = ejabberd_auth:dirty_get_registered_users(),
    SUsers = lists:sort(Users),
    {ok, [S1, S2]} = regexp:split(Diap, "-"),
    N1 = list_to_integer(S1),
    N2 = list_to_integer(S2),
    Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
    lists:flatmap(
      fun(U) ->
	      [?AC("../../user/" ++ U ++ "/", U), ?BR]
      end, Sub).



get_stats() ->
    OnlineUsers = mnesia:table_info(presence, size),
    AuthUsers = mnesia:table_info(session, size),
    RegisteredUsers = mnesia:table_info(passwd, size),
    S2SConns = ejabberd_s2s:dirty_get_connections(),
    S2SConnections = length(S2SConns),
    S2SServers = length(lists:usort([element(2, C) || C <- S2SConns])),
    
    [?XAE("table", [],
	  [?XE("tbody",
	       [?XE("tr", [?XC("td", "Registered users"),
			   ?XC("td", integer_to_list(RegisteredUsers))]),
		?XE("tr", [?XC("td", "Authentificated users"),
			   ?XC("td", integer_to_list(AuthUsers))]),
		?XE("tr", [?XC("td", "Online users"),
			   ?XC("td", integer_to_list(OnlineUsers))]),
		?XE("tr", [?XC("td", "Outgoing S2S connections"),
			   ?XC("td", integer_to_list(S2SConnections))]),
		?XE("tr", [?XC("td", "Outgoing S2S servers"),
			   ?XC("td", integer_to_list(S2SServers))])
	       ])
	  ])].
