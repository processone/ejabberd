%%%----------------------------------------------------------------------
%%% File    : ejabberd_web_admin.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Administration web interface
%%% Created :  9 Apr 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------
%%% Copyright (c) 2004 Alexey Shchepin
%%% Copyright (c) 2004 Process One
%%%----------------------------------------------------------------------

-module(ejabberd_web_admin).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([process_admin/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

-define(X(Name), {xmlelement, Name, [], []}).
-define(XA(Name, Attrs), {xmlelement, Name, Attrs, []}).
-define(XE(Name, Els), {xmlelement, Name, [], Els}).
-define(XAE(Name, Attrs, Els), {xmlelement, Name, Attrs, Els}).
-define(C(Text), {xmlcdata, Text}).
-define(XC(Name, Text), ?XE(Name, [?C(Text)])).
-define(XAC(Name, Attrs, Text), ?XAE(Name, Attrs, [?C(Text)])).

-define(T(Text), translate:translate(Lang, Text)).
-define(CT(Text), ?C(?T(Text))).
-define(XCT(Name, Text), ?XC(Name, ?T(Text))).
-define(XACT(Name, Attrs, Text), ?XAC(Name, Attrs, ?T(Text))).


-define(LI(Els), ?XE("li", Els)).
-define(A(URL, Els), ?XAE("a", [{"href", URL}], Els)).
-define(AC(URL, Text), ?A(URL, [?C(Text)])).
-define(ACT(URL, Text), ?AC(URL, ?T(Text))).
-define(P, ?X("p")).
-define(BR, ?X("br")).
-define(INPUT(Type, Name, Value),
	?XA("input", [{"type", Type},
		      {"name", Name},
		      {"value", Value}])).
-define(INPUTT(Type, Name, Value), ?INPUT(Type, Name, ?T(Value))).
-define(INPUTS(Type, Name, Value, Size),
	?XA("input", [{"type", Type},
		      {"name", Name},
		      {"value", Value},
		      {"size", Size}])).
-define(INPUTST(Type, Name, Value, Size), ?INPUT(Type, Name, ?T(Value), Size)).

make_xhtml(Els, Lang) ->
    {200, [html],
     {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"},
			   {"xml:lang", Lang},
			   {"lang", Lang}],
      [{xmlelement, "head", [],
	[{xmlelement, "meta", [{"http-equiv", "Content-Type"},
			       {"content", "text/html; charset=utf-8"}], []},
	 {xmlelement, "link", [{"href", "/admin/style.css"},
			       {"type", "text/css"},
			       {"rel", "stylesheet"}], []}]},
       ?XE("body",
	   [?XAE("table",
		 [{"id", "main"}],
		 [?XE("tbody",
		      [?XAE("tr",
			    [{"id", "top"}],
			    [?XE("td",
				 [?XE("table",
				      [?XE("tbody",
					   [?XE("tr",
						[?XE("td",
						     [?XAE("a", [{"href", "/admin/"}],
							   [?XA("img", [{"src", "/admin/logo.png"},
									{"width", "343"},
									{"height", "55"},
									{"alt", "ejabberd"},
									{"border", "0"}])])]),
						 ?XAE("td", [{"width", "100%"},
							     {"background", "/admin/logo-fill.png"}],
						     [?XAE("a", [{"href", "/admin/"}],
							   [?XA("img", [{"src", "/admin/1x1tr.gif"},
									{"width", "100%"},
									{"height", "55"},
									{"alt", ""},
									{"border", "0"}])])]
						     )])])
				      ])])]),
		       ?XAE("tr",
			    [{"id", "middle"}],
			    [?XE("td",
				 [?XAE("table",
				       [{"id", "middle-table"}],
				       [?XE("tbody",
					    [?XE("tr",
						 [?XAE("td",
						       [{"id", "middle-td1"}],
						       [?XAE("ul",
							     [{"id", "navlist"}],
							     [?LI([?ACT("/admin/acls/", "Access Control Lists")]),
							      ?LI([?ACT("/admin/access/", "Access Rules")]),
							      ?LI([?ACT("/admin/users/", "Users")]),
							      ?LI([?ACT("/admin/online-users/", "Online Users")]),
							      ?LI([?ACT("/admin/nodes/", "Nodes")]),
							      ?LI([?ACT("/admin/stats/", "Statistics")])
							     ])]),
						  ?XAE("td",
						       [{"id", "middle-td2"}],
						       [?XAE("div", [{"id", "content"}], Els)])])])
				       ])])]),
		       ?XAE("tr",
			    [{"id", "bottom"}],
			    [?XE("td",
				 [?XE("table",
				      [?XE("tbody",
					   [?XE("tr",
						[?XCT("td",
						      "ejabberd (c) 2002-2004 Alexey Shchepin, 2004 Process One")
						])])
				      ])])])])])])
      ]}}.

css() -> "
body {
  margin-left: 0;
  margin-right: 0;
  margin-top: 0;
  margin-bottom: 0;
}

#main {
  border: none;
  border-spacing: 0;
  border-collapse: collapse;
  background-color: #fe8a00;
  width: 100%;
  height: 100%;
  padding: 0;
}

#main > tbody > tr > td {
  padding: 0;
}

#top > td > table {
  border: none;
  border-spacing: 0;
  border-collapse: collapse;
  background-color: #fe8a00;
  width: 100%;
  padding-top: 2px;
}

#top table {
  border: none;
  border-spacing: 0;
  border-collapse: collapse;
  background-color: #fe8a00;
  width: 100%;
  padding-top: 2px;
}

#top td {
  padding: 0;
}

#top img {
  margin-bottom: 0px;
}

#middle {
  height: 100%;
}

#middle-table {
  border: none;
  border-spacing: 0;
  border-collapse: collapse;
  width: 100%;
  height: 100%;
  empty-cells: show;
}

#middle-td1, #middle-td2 {
  padding: 0;
  background-color: #ffffff;
  vertical-align: top;
}

#middle-td2 {
  width: 100%;
}

#bottom table {
  border: none;
  border-spacing: 0;
  border-collapse: collapse;
  width: 100%;
}

#bottom table td {
  padding: 0;
  color: #ffffff;
  background-color: #fe8a00;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 7pt;
  font-weight: bold;
  text-align: center;
}

    /*td{
      font-size: 3pt;
    }
    td.a{
      color: #fc8800;
      background-color: #fe8a00;
    }
    td.b{
      color: #333333;
      background-color: #000000;
    }
    td.c{
      color: #743300;
      background-color: #723100;
    }
    td.d{
      color: #fdc58a;
      background-color: #ffc78c;
    }
    td.e{
      color: #fde1c7;
      background-color: #ffe3c9;
    }
    td.f{
      color: #fdfdfd;
      background-color: #ffffff;
    }*/
    td.copy{
      color: #ffffff;
      background-color: #fe8a00;
      font-family: Verdana, Arial, Helvetica, sans-serif; 
      font-size: 7pt;
      font-weight: bold;
      text-align: center;
    }

    #navlist
    {
    padding: 0 1px 1px;
    margin-left: 0;
    font: bold 10px Verdana, sans-serif;
    background: #d47911;
    width: 13em;
    }

    #navlist li
    {
    list-style: none;
    margin: 0;
    text-align: left;
    display: inline;
    }

    #navlist li a
    {
    display: block;
    padding: 0.25em 0.5em 0.25em 0.75em;
    border-left: 1em solid #ffc78c;
    border-top: 1px solid gray;
    background: #ffe3c9;
    text-decoration: none;
    }

    #navlist li a:link { color: #844; }
    #navlist li a:visited { color: #766; }

    #navlist li a:hover
    {
    border-color: #fc8800;
    color: #FFF;
    background: #332;
    }

input {
    border: 1px solid #d6760e;
    color: #723202;
    background-color: #fff2e8;
    vertical-align: middle;
    margin-bottom: 0px;
    padding: 0.1em;
}

input[type=submit] {
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 7pt;
  font-weight: bold;
  color: #ffffff;
  background-color: #fe8a00;
  border: 1px solid #d6760e;
}

textarea {
    border: 1px solid #93a6c7;  
    color: #556655;
    background-color: #ffffff;
    vertical-align: middle;
    margin-top: 7px;
    margin-left: 7px;
    margin-right: 7px;
    margin-bottom: 5px;
    padding: 0.1em;
}

select {
    border: 1px solid #d6760e;
    color: #723202;
    background-color: #fff2e8;
    vertical-align: middle;
    margin-bottom: 0px; 
    padding: 0.1em;
}

thead {
  color: #000000;
  background-color: #ffffff;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: bold;
}

tr.head {
  color: #ffffff;
  background-color: #3b547a;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 9pt;
  font-weight: bold;
  text-align: center;
}

tr.oddraw {
  color: #412c75;
  background-color: #ccd4df;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 9pt;
  font-weight: normal;
  text-align: center;
}

tr.evenraw {
  color: #412c75;
  background-color: #dbe0e8;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 9pt;
  font-weight: normal;
  text-align: center;
}

td.leftheader {
  color: #412c75;
  background-color: #ccccc1;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 9pt;
  font-weight: bold;
  padding-left: 5px;
  padding-top: 2px;
  padding-bottom: 2px;
  margin-top: 0px;
  margin-bottom: 0px;
}

td.leftcontent {
  color: #000044;
  background-color: #e6e6df;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 7pt;
  font-weight: normal;
  padding-left: 5px;
  padding-right: 5px;
  padding-top: 2px;
  padding-bottom: 2px;
  margin-top: 0px;
  margin-bottom: 0px;
}

td.rightcontent {
  color: #000044;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: normal;
  text-align: justify;
  padding-left: 10px;
  padding-right: 10px;
  padding-bottom: 5px;
}


h1 {
  color: #000044;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 14pt;
  font-weight: bold;
  text-align: center;
  padding-left: 5px;
  padding-top: 2px;
  padding-bottom: 2px;
  margin-top: 0px;
  margin-bottom: 0px;
}

h2 {
  color: #000044;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 12pt;
  font-weight: bold;
  text-align: center;
  padding-left: 5px;
  padding-top: 2px;
  padding-bottom: 2px;
  margin-top: 0px;
  margin-bottom: 0px;
}

h3 {
  color: #000044;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: bold;
  text-align: left;
  padding-left: 5px;
  padding-top: 20px;
  padding-bottom: 2px;
  margin-top: 0px;
  margin-bottom: 0px;
}

#content a:link {
  color: #444466; 
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: bold;
  text-decoration: underlined;
}
#content a:visited {
  color: #444466;  
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: bold;
  text-decoration: underlined;
}
#content a:hover {
  color: #222266;  
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: bold;
  text-decoration: underlined;
}


#content li {
  list-style-type: dot;
  font-size: 10pt;
  /*font-size: 7pt;*/
  padding-left: 10px;
}

#content li.big {
  font-size: 10pt;
}

div#content {
  margin-left: 10px;
  margin-top: 5px;
}

*.alignright {
  text-align: right;
}

".

logo() ->
    jlib:decode_base64(
      "iVBORw0KGgoAAAANSUhEUgAAAVcAAAA3CAMAAACPbPnEAAAAYFBMVEX///8CAgJyMgL+vm7Wdg7+igL+/v7+slb+qkb+4sr+ojP+nir+lhr+1qb+khL+wnb+wn7+zpb+jgb+yoz+xo7+tmL+pj7+mib+jg7+5sb+rlL+rkr+mh7+tl7+2q7+umpJ0uikAAAAAXRSTlMAQObYZgAAAAFiS0dEAIgFHUgAAAAJcEhZcwAACxIAAAsSAdLdfvwAAAAHdElNRQfUBAUJBhWzc9qJAAABQ0lEQVR42u2bXU/CQBBFUUZFURAU5Ev4//+S3Ow+tFl3s6adtE3Oebghzc4DJ/Nw04WZgQczexJkz4lXvOKVxKuXV6APTCFXAq94xSte8ermFYbrA6+ilemZRxGz+fxBxMydL0/Vz5anvkUrPfb1IPCKV7ziFa9uXsG/DzyLPz7ndjS3tc3tSbcwPdl9tmYq3dHmk9x3r8mtiM11KfCKV7ziFa9uXmEc7wf+u6+5TtlXf62fKu9rl3wX9ibsLPCKV7ziFa9uXmF87wf67aBT6a+hp4bOehFxU0/CbgKveMUrXvHq5hXG+vuBcpss75zH/VZ5X7vcb4W7q5A/wvbCXoTNhX0JvOIVr3jFq5tX4P8Fw2V6g7UQ9itsLeKmfgi84hWveMWrm1egDwyX6Q3WTtinsI2wq7CjwCte8YpXvLp5BQ/utIiGbwh9RAEAAAAASUVORK5CYII=").

logo_fill() ->
    jlib:decode_base64(
      "iVBORw0KGgoAAAANSUhEUgAAAAYAAAA3BAMAAADdxCZzAAAAIVBMVEX////Wdg7+igL+khL+jg7+nir+rkr+umr+yoz+1qb+5sYp3v/aAAAAAXRSTlMAQObYZgAAAAFiS0dEAIgFHUgAAAAJcEhZcwAACxEAAAsRAX9kX5EAAAAHdElNRQfUBAYHDzOol2bZAAAASElEQVR42mMQFBRkUFJSxMAgcWNjQwwMEndxccTAIPHQ0EAMDBJPS0vEwCDx8vJCDAwS7+hoxMAg8ZkzJ2JgkPiqVQsxMFAcABvNNugXg2QkAAAAAElFTkSuQmCC").

empty() ->
    jlib:decode_base64(
      "R0lGODlhAQABAIAAAP///////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAh+QQBCgABACwAAAAAAQABAAACAkwBADs=").

process_admin(#request{user = User,
			path = [],
			q = Query,
			lang = Lang} = Request) ->
    make_xhtml([?XCT("h1", "ejabberd administration"),
		?XE("ul",
		    [?LI([?ACT("acls/", "Access Control Lists"), ?C(" "),
			  ?ACT("acls-raw/", "(raw)")]),
		     ?LI([?ACT("access/", "Access Rules"), ?C(" "),
			  ?ACT("access-raw/", "(raw)")]),
		     ?LI([?ACT("users/", "Users")]),
		     ?LI([?ACT("online-users/", "Online Users")]),
		     ?LI([?ACT("nodes/", "Nodes")]),
		     ?LI([?ACT("stats/", "Statistics")])
		    ])
	       ], Lang);

process_admin(#request{user = User,
		       path = ["style.css"],
		       q = Query,
		       lang = Lang} = Request) ->
    {200, [{"Content-Type", "text/css"}], css()};

process_admin(#request{user = User,
		       path = ["logo.png"],
		       q = Query,
		       lang = Lang} = Request) ->
    {200, [{"Content-Type", "image/png"}], logo()};

process_admin(#request{user = User,
		       path = ["logo-fill.png"],
		       q = Query,
		       lang = Lang} = Request) ->
    {200, [{"Content-Type", "image/png"}], logo_fill()};

process_admin(#request{user = User,
		       path = ["1x1tr.gif"],
		       q = Query,
		       lang = Lang} = Request) ->
    {200, [{"Content-Type", "image/gif"}], empty()};

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
    make_xhtml([?XCT("h1", "ejabberd access control lists configuration")] ++
	       case Res of
		   ok -> [?CT("submitted"), ?P];
		   error -> [?CT("bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"method", "post"}],
		     [?XAC("textarea", [{"name", "acls"},
					{"rows", "16"},
					{"cols", "80"}],
			   ACLs),
		      ?BR,
		      ?INPUT("submit", "submit", "Submit")
		     ])
	       ], Lang);

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
    make_xhtml([?XCT("h1", "ejabberd access control lists configuration")] ++
	       case Res of
		   ok -> [?CT("submitted"), ?P];
		   error -> [?CT("bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XE("p", [?ACT("../acls-raw/", "raw")])] ++
	       [?XAE("form", [{"method", "post"}],
		     [acls_to_xhtml(ACLs),
		      ?BR,
		      ?INPUTT("submit", "delete", "Delete Selected"),
		      ?C(" "),
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Lang);

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
    make_xhtml([?XCT("h1", "ejabberd access rules configuration")] ++
	       case Res of
		   ok -> [?CT("submitted"), ?P];
		   error -> [?CT("bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"method", "post"}],
		     [?XAC("textarea", [{"name", "access"},
					{"rows", "16"},
					{"cols", "80"}],
			   Access),
		      ?BR,
		      ?INPUT("submit", "submit", "Submit")
		     ])
	       ], Lang);

process_admin(#request{method = Method,
		       user = User,
		       path = ["access"],
		       q = Query,
		       lang = Lang} = Request) ->
    ?INFO_MSG("query: ~p", [Query]),
    Res = case Method of
	      'POST' ->
		  case catch access_parse_query(Query) of
		      {'EXIT', _} ->
			  error;
		      ok ->
			  ok
		  end;
	      _ ->
		  nothing
	  end,
    AccessRules =
	ets:select(config,
		   [{{config, {access, '$1'}, '$2'},
		     [],
		     [{{access, '$1', '$2'}}]}]),
    make_xhtml([?XCT("h1", "ejabberd access rules configuration")] ++
	       case Res of
		   ok -> [?CT("submitted"), ?P];
		   error -> [?CT("bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XE("p", [?ACT("../access-raw/", "raw")])] ++
	       [?XAE("form", [{"method", "post"}],
		     [access_rules_to_xhtml(AccessRules, Lang),
		      ?BR,
		      ?INPUTT("submit", "delete", "Delete Selected")
		     ])
	       ], Lang);

process_admin(#request{method = Method,
		       user = User,
		       path = ["access", SName],
		       q = Query,
		       lang = Lang} = Request) ->
    ?INFO_MSG("query: ~p", [Query]),
    Name = list_to_atom(SName),
    Res = case lists:keysearch("rules", 1, Query) of
	      {value, {_, String}} ->
		  case parse_access_rule(String) of
		      {ok, Rs} ->
			  ejabberd_config:add_global_option(
			    {access, Name}, Rs),
			  ok;
		      _ ->
			  error
		  end;
	      _ ->
		  nothing
	  end,
    Rules = case ejabberd_config:get_global_option({access, Name}) of
		undefined ->
		    [];
		Rs1 ->
		    Rs1
	    end,
    make_xhtml([?XC("h1",
		    io_lib:format(?T("~s access rule configuration"), [SName]))] ++
	       case Res of
		   ok -> [?CT("submitted"), ?P];
		   error -> [?CT("bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"method", "post"}],
		     [access_rule_to_xhtml(Rules),
		      ?BR,
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Lang);

process_admin(#request{user = User,
			path = ["users"],
			q = Query,
			lang = Lang} = Request) ->
    Res = list_users(Query, Lang),
    make_xhtml([?XCT("h1", "ejabberd users")] ++ Res, Lang);

process_admin(#request{user = User,
		       path = ["users", Diap],
		       q = Query,
		       lang = Lang} = Request) ->
    Res = list_users_in_diapason(Diap, Lang),
    make_xhtml([?XCT("h1", "ejabberd users")] ++ Res, Lang);

process_admin(#request{user = User,
			path = ["online-users"],
			q = Query,
			lang = Lang} = Request) ->
    Res = list_online_users(Lang),
    make_xhtml([?XCT("h1", "ejabberd users")] ++ Res, Lang);

process_admin(#request{user = User,
		       path = ["stats"],
		       q = Query,
		       lang = Lang} = Request) ->
    Res = get_stats(Lang),
    make_xhtml([?XCT("h1", "ejabberd stats")] ++ Res, Lang);

process_admin(#request{user = User,
		       path = ["user", U],
		       q = Query,
		       lang = Lang} = Request) ->
    Res = user_info(U, Query, Lang),
    make_xhtml(Res, Lang);

process_admin(#request{user = User,
		       path = ["user", U, "queue"],
		       q = Query,
		       lang = Lang} = Request) ->
    Res = user_queue(U, Query, Lang),
    make_xhtml(Res, Lang);

process_admin(#request{user = User,
		       path = ["user", U, "roster"],
		       q = Query,
		       lang = Lang} = Request) ->
    Res = user_roster(U, Query, Lang, true),
    make_xhtml(Res, Lang);

process_admin(#request{user = User,
		       path = ["nodes"],
		       q = Query,
		       lang = Lang} = Request) ->
    Res = get_nodes(Lang),
    make_xhtml(Res, Lang);

process_admin(#request{user = User,
		       path = ["node", SNode | NPath],
		       q = Query,
		       lang = Lang} = Request) ->
    case search_running_node(SNode) of
	false ->
	    make_xhtml([?XCT("h1", "Node not found")], Lang);
	Node ->
	    Res = get_node(Node, NPath, Query, Lang),
	    make_xhtml(Res, Lang)
    end;

process_admin(#request{lang = Lang}) ->
    setelement(1, make_xhtml([?XC("h1", "Not found")], Lang), 404).



acls_to_xhtml(ACLs) ->
    ?XAE("table", [],
	 [?XE("tbody",
	      lists:map(
		fun({acl, Name, Spec} = ACL) ->
			SName = atom_to_list(Name),
			ID = term_to_id(ACL),
			?XE("tr",
			    [?XE("td", [?INPUT("checkbox", "selected", ID)]),
			     ?XC("td", SName)] ++
			    acl_spec_to_xhtml(ID, Spec)
			   )
		end, ACLs) ++
	      [?XE("tr",
		   [?X("td"),
		    ?XE("td", [?INPUT("text", "namenew", "")])
		   ] ++
		   acl_spec_to_xhtml("new", {user, ""})
		  )]
	     )]).

-define(ACLINPUT(Text), ?XE("td", [?INPUT("text", "value" ++ ID, Text)])).

acl_spec_to_text({user, U}) ->
    {user, U};

acl_spec_to_text({server, S}) ->
    {server, S};

acl_spec_to_text({user, U, S}) ->
    {user_server, U ++ "@" ++ S};

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


access_rules_to_xhtml(AccessRules, Lang) ->
    ?XAE("table", [],
	 [?XE("tbody",
	      lists:map(
		fun({access, Name, Rules} = Access) ->
			SName = atom_to_list(Name),
			ID = term_to_id(Access),
			?XE("tr",
			    [?XE("td", [?INPUT("checkbox", "selected", ID)]),
			     ?XE("td", [?AC(SName ++ "/", SName)]),
			     ?XC("td", term_to_string(Rules))
			    ]
			   )
		end, AccessRules) ++
	      [?XE("tr",
		   [?X("td"),
		    ?XE("td", [?INPUT("text", "namenew", "")]),
		    ?XE("td", [?INPUTT("submit", "addnew", "Add New")])
		   ]
		  )]
	     )]).

access_parse_query(Query) ->
    AccessRules =
	ets:select(config,
		   [{{config, {access, '$1'}, '$2'},
		     [],
		     [{{access, '$1', '$2'}}]}]),
    case lists:keysearch("addnew", 1, Query) of
	{value, _} ->
	    access_parse_addnew(AccessRules, Query);
	_ ->
	    case lists:keysearch("delete", 1, Query) of
		{value, _} ->
		    access_parse_delete(AccessRules, Query)
	    end
    end.

access_parse_addnew(AccessRules, Query) ->
    case lists:keysearch("namenew", 1, Query) of
	{value, {_, String}} when String /= "" ->
	    Name = list_to_atom(String),
	    ejabberd_config:add_global_option({access, Name}, []),
	    ok
    end.

access_parse_delete(AccessRules, Query) ->
    lists:foreach(
      fun({access, Name, _Rules} = AccessRule) ->
	      ID = term_to_id(AccessRule),
	      case lists:member({"selected", ID}, Query) of
		  true ->
		      mnesia:transaction(
			fun() ->
				mnesia:delete({config, {access, Name}})
			end);
		  _ ->
		      ok
	      end
      end, AccessRules),
    ok.




access_rule_to_xhtml(Rules) ->
    Text = lists:flatmap(
	     fun({Access, ACL} = Rule) ->
		     SAccess = atom_to_list(Access),
		     SACL = atom_to_list(ACL),
		     SAccess ++ "\t" ++ SACL ++ "\n"
	     end, Rules),
    ?XAC("textarea", [{"name", "rules"},
		      {"rows", "16"},
		      {"cols", "80"}],
	 Text).

parse_access_rule(Text) ->
    Strings = string:tokens(Text, "\r\n"),
    case catch lists:flatmap(
		 fun(String) ->
			 case string:tokens(String, "\s\t") of
			     [Access, ACL] ->
				 [{list_to_atom(Access), list_to_atom(ACL)}];
			     [] ->
				 []
			 end
		 end, Strings) of
	{'EXIT', _Reason} ->
	    error;
	Rs ->
	    {ok, Rs}
    end.




list_users(Query, Lang) ->
    Res = list_users_parse_query(Query),
    Users = ejabberd_auth:dirty_get_registered_users(),
    SUsers = lists:sort(Users),
    FUsers =
	case length(SUsers) of
	    N when N =< 100 ->
		[list_given_users(SUsers, "../", Lang)];
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
	end,
    case Res of
	ok -> [?CT("submitted"), ?P];
	error -> [?CT("bad format"), ?P];
	nothing -> []
    end ++
	[?XAE("form", [{"method", "post"}],
	      [?XE("table",
		   [?XE("tr",
			[?XC("td", ?T("User") ++ ":"),
			 ?XE("td", [?INPUT("text", "newusername", "")])
			]),
		    ?XE("tr",
			[?XC("td", ?T("Password") ++ ":"),
			 ?XE("td", [?INPUT("password", "newuserpassword", "")])
			]),
		    ?XE("tr",
			[?X("td"),
			 ?XAE("td", [{"class", "alignright"}],
			      [?INPUTT("submit", "addnewuser", "Add User")])
			])]),
	       ?P] ++
	      FUsers)].

list_users_parse_query(Query) ->
    case lists:keysearch("addnewuser", 1, Query) of
	{value, _} ->
	    {value, {_, User}} =
		lists:keysearch("newusername", 1, Query),
	    {value, {_, Password}} =
		lists:keysearch("newuserpassword", 1, Query),
	    case jlib:nodeprep(User) of
		error ->
		    error;
		"" ->
		    error;
		_ ->
		    ejabberd_auth:try_register(User, Password),
		    ok
	    end;
	false ->
	    nothing
    end.

list_users_in_diapason(Diap, Lang) ->
    Users = ejabberd_auth:dirty_get_registered_users(),
    SUsers = lists:sort(Users),
    {ok, [S1, S2]} = regexp:split(Diap, "-"),
    N1 = list_to_integer(S1),
    N2 = list_to_integer(S2),
    Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
    [list_given_users(Sub, "../../", Lang)].

list_given_users(Users, Prefix, Lang) ->
    ?XE("table",
	[?XE("thead",
	     [?XE("tr",
		  [?XCT("td", "User"),
		   ?XCT("td", "Offline messages"),
		   ?XCT("td", "Last Activity")])]),
	 ?XE("tbody",
	     lists:map(
	       fun(User) ->
		       QueueLen = length(mnesia:dirty_read({offline_msg, User})),
		       FQueueLen = [?AC(Prefix ++ "user/" ++ User ++ "/queue/",
					integer_to_list(QueueLen))],
		       FLast =
			   case ejabberd_sm:get_user_resources(User) of
			       [] ->
				   case mnesia:dirty_read({last_activity, User}) of
				       [] ->
					   ?T("Never");
				       [E] ->
					   Shift = element(3, E),
					   TimeStamp = {Shift div 1000000,
							Shift rem 1000000,
							0},
					   {{Year, Month, Day}, {Hour, Minute, Second}} =
					       calendar:now_to_local_time(TimeStamp),
					   lists:flatten(
					     io_lib:format(
					       "~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
					       [Year, Month, Day, Hour, Minute, Second]))
				   end;
			       _ ->
				   ?T("Online")
			   end,
		       ?XE("tr",
			   [?XE("td", [?AC(Prefix ++ "user/" ++ User ++ "/",
					   User)]),
			    ?XE("td", FQueueLen),
			    ?XC("td", FLast)])
	       end, Users)
	    )]).


get_stats(Lang) ->
    OnlineUsers = mnesia:table_info(presence, size),
    AuthUsers = mnesia:table_info(session, size),
    RegisteredUsers = mnesia:table_info(passwd, size),
    S2SConns = ejabberd_s2s:dirty_get_connections(),
    S2SConnections = length(S2SConns),
    S2SServers = length(lists:usort([element(2, C) || C <- S2SConns])),
    
    [?XAE("table", [],
	  [?XE("tbody",
	       [?XE("tr", [?XCT("td", "Registered users"),
			   ?XC("td", integer_to_list(RegisteredUsers))]),
		?XE("tr", [?XCT("td", "Authentificated users"),
			   ?XC("td", integer_to_list(AuthUsers))]),
		?XE("tr", [?XCT("td", "Online users"),
			   ?XC("td", integer_to_list(OnlineUsers))]),
		?XE("tr", [?XCT("td", "Outgoing S2S connections"),
			   ?XC("td", integer_to_list(S2SConnections))]),
		?XE("tr", [?XCT("td", "Outgoing S2S servers"),
			   ?XC("td", integer_to_list(S2SServers))])
	       ])
	  ])].


list_online_users(_Lang) ->
    Users = lists:map(fun({U, R}) -> U end,
		      ejabberd_sm:dirty_get_sessions_list()),
    SUsers = lists:usort(Users),
    lists:flatmap(
      fun(U) ->
	      [?AC("../user/" ++ U ++ "/", U), ?BR]
      end, SUsers).

user_info(User, Query, Lang) ->
    Res = user_parse_query(User, Query),
    Resources = ejabberd_sm:get_user_resources(User),
    FResources =
	case Resources of
	    [] ->
		[?CT("None")];
	    _ ->
		[?XE("ul",
		     lists:map(fun(R) ->
				       ?LI([?C(R)])
			       end, lists:sort(Resources)))]
	end,
    Password = ejabberd_auth:get_password_s(User),
    FPassword = [?INPUT("password", "password", Password), ?C(" "),
		 ?INPUTT("submit", "chpassword", "Change Password")],
    QueueLen = length(mnesia:dirty_read({offline_msg, User})),
    FQueueLen = [?AC("queue/",
		     integer_to_list(QueueLen))],
    [?XC("h1", ?T("User ") ++ User)] ++
	case Res of
	    ok -> [?CT("submitted"), ?P];
	    error -> [?CT("bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"method", "post"}],
	      [?XCT("h3", "Connected Resources:")] ++ FResources ++
	      [?XCT("h3", "Password:")] ++ FPassword ++
	      [?XCT("h3", "Offline messages:")] ++ FQueueLen ++
	      [?XE("h3", [?ACT("roster/", "Roster")])] ++
	      [?BR, ?INPUTT("submit", "removeuser", "Remove User")])].


user_parse_query(User, Query) ->
    case lists:keysearch("chpassword", 1, Query) of
	{value, _} ->
	    case lists:keysearch("password", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, Password}} ->
		    ejabberd_auth:set_password(User, Password),
		    ok;
		_ ->
		    error
	    end;
	_ ->
	    case lists:keysearch("removeuser", 1, Query) of
		{value, _} ->
		    ejabberd_auth:remove_user(User),
		    ok;
		false ->
		    nothing
	    end
    end.


user_queue(User, Query, Lang) ->
    Res = user_queue_parse_query(User, Query),
    Msgs = lists:keysort(3, mnesia:dirty_read({offline_msg, User})),
    FMsgs =
	lists:map(
	  fun({offline_msg, _User, TimeStamp, _Expire, From, To,
	       {xmlelement, Name, Attrs, Els}} = Msg) ->
		  ID = jlib:encode_base64(binary_to_list(term_to_binary(Msg))),
		  {{Year, Month, Day}, {Hour, Minute, Second}} =
		      calendar:now_to_local_time(TimeStamp),
		  Time = lists:flatten(
			   io_lib:format(
			     "~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
			     [Year, Month, Day, Hour, Minute, Second])),
		  SFrom = jlib:jid_to_string(From),
		  STo = jlib:jid_to_string(To),
		  Attrs2 = jlib:replace_from_to_attrs(SFrom, STo, Attrs),
		  Packet = jlib:remove_attr(
			     "jeai-id", {xmlelement, Name, Attrs2, Els}),
		  FPacket = pretty_print(Packet),
		  ?XE("tr",
		      [?XAE("td", [{"class", "valign"}], [?INPUT("checkbox", "selected", ID)]),
		       ?XAC("td", [{"class", "valign"}], Time),
		       ?XAC("td", [{"class", "valign"}], SFrom),
		       ?XAC("td", [{"class", "valign"}], STo),
		       ?XAE("td", [{"class", "valign"}], [?XC("pre", FPacket)])]
		     )
	  end, Msgs),
    [?XC("h1", io_lib:format(?T("~s offline messages queue"), [User]))] ++
	case Res of
	    ok -> [?CT("submitted"), ?P];
	    error -> [?CT("bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"method", "post"}],
	      [?XE("table",
		   [?XE("thead",
			[?XE("tr",
			     [?X("td"),
			      ?XCT("td", "Time"),
			      ?XCT("td", "From"),
			      ?XCT("td", "To"),
			      ?XCT("td", "Packet")
			     ])]),
		    ?XE("tbody", FMsgs)]),
	       ?BR,
	       ?INPUTT("submit", "delete", "Delete Selected")
	      ])].

user_queue_parse_query(User, Query) ->
    case lists:keysearch("delete", 1, Query) of
	{value, _} ->
	    Msgs = lists:keysort(3, mnesia:dirty_read({offline_msg, User})),
	    F = fun() ->
			lists:foreach(
			  fun(Msg) ->
				  ID = jlib:encode_base64(
					 binary_to_list(term_to_binary(Msg))),
				  case lists:member({"selected", ID}, Query) of
				      true ->
					  mnesia:delete_object(Msg);
				      false ->
					  ok
				  end
			  end, Msgs)
		end,
	    mnesia:transaction(F),
	    ok;
	false ->
	    nothing
    end.



-record(roster, {uj,
		 user,
		 jid,
		 name = "",
		 subscription = none,
		 ask = none,
		 groups = [],
		 xattrs = [],
		 xs = []}).

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.


user_roster(User, Query, Lang, Admin) ->
    LUser = jlib:nameprep(User),
    Items1 = mnesia:dirty_index_read(roster, LUser, #roster.user),
    Res = user_roster_parse_query(User, Items1, Query, Admin),
    Items = mnesia:dirty_index_read(roster, LUser, #roster.user),
    SItems = lists:sort(Items),
    FItems =
	case SItems of
	    [] ->
		[?CT("None")];
	    _ ->
		[?XE("table",
		     [?XE("thead",
			  [?XE("tr",
			       [?XCT("td", "JID"),
				?XCT("td", "Nickname"),
				?XCT("td", "Subscription"),
				?XCT("td", "Pending"),
				?XCT("td", "Groups")
			       ])]),
		      ?XE("tbody",
			  lists:map(
			    fun(R) ->
				    Groups =
					lists:flatmap(
					  fun(Group) ->
						  [?C(Group), ?BR]
					  end, R#roster.groups),
				    Pending = ask_to_pending(R#roster.ask),
				    ?XE("tr",
					[?XAC("td", [{"class", "valign"}],
					      jlib:jid_to_string(R#roster.jid)),
					 ?XAC("td", [{"class", "valign"}],
					      R#roster.name),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(R#roster.subscription)),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(Pending)),
					 ?XAE("td", [{"class", "valign"}], Groups),
					 if
					     Pending == in ->
						 ?XAE("td", [{"class", "valign"}],
						      [?INPUTT("submit",
							       "validate" ++
							       term_to_id(R#roster.jid),
							       "Validate")]);
					     true ->
						 ?X("td")
					 end,
					 ?XAE("td", [{"class", "valign"}],
					      [?INPUTT("submit",
						       "remove" ++
						       term_to_id(R#roster.jid),
						       "Remove")])])
			    end, SItems))])]
	end,
    [?XC("h1", ?T("Roster of ") ++ User)] ++
	case Res of
	    ok -> [?CT("submitted"), ?P];
	    error -> [?CT("bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"method", "post"}],
	      FItems ++
	      [?P,
	       ?INPUT("text", "newjid", ""), ?C(" "),
	       ?INPUTT("submit", "addjid", "Add JID")
	      ])].

user_roster_parse_query(User, Items, Query, Admin) ->
    case lists:keysearch("addjid", 1, Query) of
	{value, _} ->
	    case lists:keysearch("newjid", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, SJID}} ->
		    case jlib:string_to_jid(SJID) of
			JID when is_record(JID, jid) ->
			    user_roster_subscribe_jid(User, JID),
			    ok;
			error ->
			    error
		    end;
		false ->
		    error
	    end;
	false ->
	    case lists:keysearch("adduser", 1, Query) of
		{value, _} ->
		    case lists:keysearch("newuser", 1, Query) of
			{value, {_, undefined}} ->
			    error;
			{value, {_, U}} ->
			    if
				Admin ->
				    user_roster_subscribe_users(User, U);
				true ->
				    case jlib:make_jid(U, ?MYNAME, "") of
					JID when is_record(JID, jid) ->
					    user_roster_subscribe_jid(
					      User, JID),
					    ok;
					false ->
					    error
				    end
			    end;
			false ->
			    error
		    end;
		false ->
		    case catch user_roster_item_parse_query(
				 User, Items, Query) of
			submitted ->
			    ok;
			{'EXIT', _Reason} ->
			    error;
			_ ->
			    nothing
		    end
	    end
    end.

user_roster_subscribe_users(User1, User2) ->
    case jlib:make_jid(User1, ?MYNAME, "") of
	JID1 when is_record(JID1, jid) ->
	    case jlib:make_jid(User2, ?MYNAME, "") of
		JID2 when is_record(JID2, jid) ->
		    mod_roster:out_subscription(User1, JID2, subscribe),
		    mod_roster:in_subscription(User2, JID1, subscribe),
		    mod_roster:out_subscription(User2, JID1, subscribe),
		    mod_roster:in_subscription(User1, JID2, subscribe),
		    mod_roster:out_subscription(User1, JID2, subscribed),
		    mod_roster:in_subscription(User2, JID1, subscribed),
		    mod_roster:out_subscription(User2, JID1, subscribed),
		    mod_roster:in_subscription(User1, JID2, subscribed),
		    ok;
		false ->
		    error
	    end;
	false ->
	    error
    end.

user_roster_subscribe_jid(User, JID) ->
    mod_roster:out_subscription(User, JID, subscribe),
    UJID = jlib:make_jid(User, ?MYNAME, ""),
    ejabberd_router:route(
      UJID, JID, {xmlelement, "presence", [{"type", "subscribe"}], []}).

user_roster_item_parse_query(User, Items, Query) ->
    lists:foreach(
      fun(R) ->
	      JID = R#roster.jid,
	      case lists:keysearch(
		     "validate" ++ term_to_id(JID), 1, Query) of
		  {value, _} ->
		      JID1 = jlib:make_jid(JID),
		      mod_roster:out_subscription(User, JID1, subscribed),
		      UJID = jlib:make_jid(User, ?MYNAME, ""),
		      ejabberd_router:route(
			UJID, JID1, {xmlelement, "presence",
				     [{"type", "subscribed"}], []}),
		      throw(submitted);
		  false ->
		      case lists:keysearch(
			     "remove" ++ term_to_id(JID), 1, Query) of
			  {value, _} ->
			      UJID = jlib:make_jid(User, ?MYNAME, ""),
			      mod_roster:process_iq(
				UJID, UJID,
				#iq{type = set,
				    sub_el = {xmlelement, "query",
					      [{"xmlns", ?NS_ROSTER}],
					      [{xmlelement, "item",
						[{"jid", jlib:jid_to_string(JID)},
						 {"subscription", "remove"}],
						[]}]}}),
			      throw(submitted);
			  false ->
			      ok
		      end

	      end
      end, Items),
    nothing.



get_nodes(Lang) ->
    RunningNodes = mnesia:system_info(running_db_nodes),
    StoppedNodes = lists:usort(mnesia:system_info(db_nodes) ++
			       mnesia:system_info(extra_db_nodes)) --
	RunningNodes,
    FRN = if
	      RunningNodes == [] ->
		  ?CT("None");
	      true ->
		  ?XE("ul",
		      lists:map(
			fun(N) ->
				S = atom_to_list(N),
				?LI([?AC("../node/" ++ S ++ "/", S)])
			end, lists:sort(RunningNodes)))
	  end,
    FSN = if
	      StoppedNodes == [] ->
		  ?CT("None");
	      true ->
		  ?XE("ul",
		      lists:map(
			fun(N) ->
				S = atom_to_list(N),
				?LI([?C(S)])
			end, lists:sort(StoppedNodes)))
	  end,
    [?XCT("h1", "Nodes"),
     ?XCT("h3", "Running Nodes"),
     FRN,
     ?XCT("h3", "Stopped Nodes"),
     FSN].

search_running_node(SNode) ->
    search_running_node(SNode, mnesia:system_info(running_db_nodes)).

search_running_node(_, []) ->
    false;
search_running_node(SNode, [Node | Nodes]) ->
    case atom_to_list(Node) of
	SNode ->
	    Node;
	_ ->
	    search_running_node(SNode, Nodes)
    end.

get_node(Node, [], Query, Lang) ->
    Res = node_parse_query(Node, Query),
    [?XC("h1", ?T("Node ") ++ atom_to_list(Node))] ++
	case Res of
	    ok -> [?CT("submitted"), ?P];
	    error -> [?CT("bad format"), ?P];
	    nothing -> []
	end ++
	[?XE("ul",
	     [?LI([?ACT("db/", "DB Management")]),
	      ?LI([?ACT("backup/", "Backup Management")]),
	      ?LI([?ACT("ports/", "Listened Ports Management")]),
	      ?LI([?ACT("stats/", "Statistics")])
	     ]),
	 ?XAE("form", [{"method", "post"}],
	      [?INPUTT("submit", "restart", "Restart"),
	       ?C(" "),
	       ?INPUTT("submit", "stop", "Stop")])
	];

get_node(Node, ["db"], Query, Lang) ->
    case rpc:call(Node, mnesia, system_info, [tables]) of
	{badrpc, _Reason} ->
	    [?XCT("h1", "RPC call error")];
	Tables ->
	    Res = node_db_parse_query(Node, Tables, Query),
	    STables = lists:sort(Tables),
	    Rows = lists:map(
		     fun(Table) ->
			     STable = atom_to_list(Table),
			     TInfo =
				 case rpc:call(Node,
					       mnesia,
					       table_info,
					       [Table, all]) of
				     {badrpc, _} ->
					 [];
				     I ->
					 I
				 end,
			     {Type, Size, Memory} =
				 case {lists:keysearch(storage_type, 1, TInfo),
				       lists:keysearch(size, 1, TInfo),
				       lists:keysearch(memory, 1, TInfo)} of
				     {{value, {storage_type, T}},
				      {value, {size, S}},
				      {value, {memory, M}}} ->
					 {T, S, M};
				     _ ->
					 {unknown, 0, 0}
				 end,
			     ?XE("tr",
				 [?XC("td", STable),
				  ?XE("td", [db_storage_select(
					       STable, Type, Lang)]),
				  ?XAC("td", [{"class", "alignright"}],
				       integer_to_list(Size)),
				  ?XAC("td", [{"class", "alignright"}],
				       integer_to_list(Memory))
				 ])
		     end, STables),
	    [?XC("h1", ?T("DB Tables at ") ++ atom_to_list(Node))] ++
		case Res of
		    ok -> [?CT("submitted"), ?P];
		    error -> [?CT("bad format"), ?P];
		    nothing -> []
		end ++
		[?XAE("form", [{"method", "post"}],
		      [?XAE("table", [],
			    [?XE("thead",
				 [?XE("tr",
				      [?XCT("td", "Name"),
				       ?XCT("td", "Storage Type"),
				       ?XCT("td", "Size"),
				       ?XCT("td", "Memory")
				      ])]),
			     ?XE("tbody",
				 Rows ++
				 [?XE("tr",
				      [?XAE("td", [{"colspan", "4"},
						   {"class", "alignright"}],
					    [?INPUTT("submit", "submit",
						     "Submit")])
				      ])]
				)])])]
    end;

get_node(Node, ["backup"], Query, Lang) ->
    Res = node_backup_parse_query(Node, Query),
    [?XC("h1", ?T("Backup Management at ") ++ atom_to_list(Node)),
     ?XAE("form", [{"method", "post"}],
	  [?XAE("table", [],
		[?XE("tbody",
		     [?XE("tr",
			  [?XCT("td", "Store a backup in a file"),
			   ?XE("td", [?INPUT("text", "storepath",
					     "ejabberd.backup")]),
			   ?XE("td", [?INPUTT("submit", "store",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td", "Restore a backup from a file"),
			   ?XE("td", [?INPUT("text", "restorepath",
					     "ejabberd.backup")]),
			   ?XE("td", [?INPUTT("submit", "restore",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td",
				"Install a database fallback from a file"),
			   ?XE("td", [?INPUT("text", "fallbackpath",
					     "ejabberd.backup")]),
			   ?XE("td", [?INPUTT("submit", "fallback",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td", "Dump a database in a text file"),
			   ?XE("td", [?INPUT("text", "dumppath",
					     "ejabberd.dump")]),
			   ?XE("td", [?INPUTT("submit", "dump",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td", "Restore a database from a text file"),
			   ?XE("td", [?INPUT("text", "loadpath",
					     "ejabberd.dump")]),
			   ?XE("td", [?INPUTT("submit", "load",
					      "OK")])
			  ])
		     ])
		])])];

get_node(Node, ["ports"], Query, Lang) ->
    Ports = rpc:call(Node, ejabberd_config, get_local_option, [listen]),
    Res = case catch node_ports_parse_query(Node, Ports, Query) of
	      submitted ->
		  ok;
	      {'EXIT', _Reason} ->
		  error;
	      _ ->
		  nothing
	  end,
    NewPorts = lists:sort(
		 rpc:call(Node, ejabberd_config, get_local_option, [listen])),
    [?XC("h1", ?T("Listened Ports at ") ++ atom_to_list(Node))] ++
	case Res of
	    ok -> [?CT("submitted"), ?P];
	    error -> [?CT("bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"method", "post"}],
	      [node_ports_to_xhtml(NewPorts, Lang)])
	];

get_node(Node, ["stats"], Query, Lang) ->
    UpTime = rpc:call(Node, erlang, statistics, [wall_clock]),
    UpTimeS = io_lib:format("~.3f", [element(1, UpTime)/1000]),
    CPUTime = rpc:call(Node, erlang, statistics, [runtime]),
    CPUTimeS = io_lib:format("~.3f", [element(1, CPUTime)/1000]),
    Users = length(
	      rpc:call(Node, ejabberd_sm, dirty_get_my_sessions_list, [])),
    TransactionsCommited =
	rpc:call(Node, mnesia, system_info, [transaction_commits]),
    TransactionsAborted =
	rpc:call(Node, mnesia, system_info, [transaction_failures]),
    TransactionsRestarted =
	rpc:call(Node, mnesia, system_info, [transaction_restarts]),
    TransactionsLogged =
	rpc:call(Node, mnesia, system_info, [transaction_log_writes]),
    
    [?XC("h1", io_lib:format(?T("~p statistics"), [Node])),
     ?XAE("table", [],
	  [?XE("tbody",
	       [?XE("tr", [?XCT("td", "Uptime"),
			   ?XAC("td", [{"class", "alignright"}],
				UpTimeS)]),
		?XE("tr", [?XCT("td", "CPU Time"),
			   ?XAC("td", [{"class", "alignright"}],
				CPUTimeS)]),
		?XE("tr", [?XCT("td", "Authentificated users"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(Users))]),
		?XE("tr", [?XCT("td", "Transactions commited"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(TransactionsCommited))]),
		?XE("tr", [?XCT("td", "Transactions aborted"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(TransactionsAborted))]),
		?XE("tr", [?XCT("td", "Transactions restarted"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(TransactionsRestarted))]),
		?XE("tr", [?XCT("td", "Transactions logged"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(TransactionsLogged))])
	       ])
	  ])];

get_node(Node, NPath, Query, Lang) ->
    [?XCT("h1", "Not found")].


node_parse_query(Node, Query) ->
    case lists:keysearch("restart", 1, Query) of
	{value, _} ->
	    case rpc:call(Node, init, restart, []) of
		{badrpc, _Reason} ->
		    error;
		_ ->
		    ok
	    end;
	_ ->
	    case lists:keysearch("stop", 1, Query) of
		{value, _} ->
		    case rpc:call(Node, init, stop, []) of
			{badrpc, _Reason} ->
			    error;
			_ ->
			    ok
		    end;
		_ ->
		    nothing
	    end
    end.


db_storage_select(ID, Opt, Lang) ->
    ?XAE("select", [{"name", "table" ++ ID}],
	 lists:map(
	   fun({O, Desc}) ->
		   Sel = if
			     O == Opt -> [{"selected", "selected"}];
			     true -> []
			 end,
		   ?XACT("option",
			 Sel ++ [{"value", atom_to_list(O)}],
			 Desc)
	   end, [{ram_copies, "RAM copy"},
		 {disc_copies, "RAM and disc copy"},
		 {disc_only_copies, "Disc only copy"},
		 {unknown, "Remote copy"}])).

node_db_parse_query(Node, Tables, Query) ->
    lists:foreach(
      fun(Table) ->
	      STable = atom_to_list(Table),
	      case lists:keysearch("table" ++ STable, 1, Query) of
		  {value, {_, SType}} ->
		      Type = case SType of
				 "unknown" -> unknown;
				 "ram_copies" -> ram_copies;
				 "disc_copies" -> disc_copies;
				 "disc_only_copies" -> disc_only_copies;
				 _ -> false
			     end,
		      if
			  Type == false ->
			      ok;
			  Type == unknown ->
			      mnesia:del_table_copy(Table, Node);
			  true ->
			      case mnesia:add_table_copy(Table, Node, Type) of
				  {aborted, _} ->
				      mnesia:change_table_copy_type(
					Table, Node, Type);
				  _ ->
				      ok
			      end
		      end;
		  _ ->
		      ok
	      end
      end, Tables),
    ok.

node_backup_parse_query(Node, Query) ->
    lists:foldl(
      fun(Action, nothing) ->
	      case lists:keysearch(Action, 1, Query) of
		  {value, _} ->
		      case lists:keysearch(Action ++ "path", 1, Query) of
			  {value, {_, Path}} ->
			      Res =
				  case Action of
				      "store" ->
					  rpc:call(Node, mnesia,
						   backup, [Path]);
				      "restore" ->
					  rpc:call(Node, mnesia,
						   restore,
						   [Path, [{default_op,
							    keep_tables}]]);
				      "fallback" ->
					  rpc:call(Node, mnesia,
						   install_fallback, [Path]);
				      "dump" ->
					  rpc:call(Node, mnesia,
						   dump_to_textfile, [Path]);
				      "load" ->
					  rpc:call(Node, mnesia,
						   load_textfile, [Path])
				  end,
			      case Res of
				  {error, _Reason} ->
				      error;
				  {badrpc, _Reason} ->
				      error;
				  _ ->
				      ok
			      end;
			  _ ->
			      error
		      end;
		  _ ->
		      nothing
	      end;
	 (_Action, Res) ->
	      Res
      end, nothing, ["store", "restore", "fallback", "dump", "load"]).


node_ports_to_xhtml(Ports, Lang) ->
    ?XAE("table", [],
	 [?XE("thead",
	      [?XE("tr",
		   [?XCT("td", "Port"),
		    ?XCT("td", "Module"),
		    ?XCT("td", "Options")
		   ])]),
	  ?XE("tbody",
	      lists:map(
		fun({Port, Module, Opts} = E) ->
			SPort = integer_to_list(Port),
			SModule = atom_to_list(Module),
			ID = term_to_id(E),
			?XE("tr",
			    [?XC("td", SPort),
			     ?XE("td", [?INPUT("text", "module" ++ SPort,
					       SModule)]),
			     ?XE("td", [?INPUTS("text", "opts" ++ SPort,
						term_to_string(Opts), "40")]),
			     ?XE("td", [?INPUTT("submit", "add" ++ SPort,
						"Update")]),
			     ?XE("td", [?INPUTT("submit", "delete" ++ SPort,
						"Delete")])
			    ]
			   )
		end, Ports) ++
	      [?XE("tr",
		   [?XE("td", [?INPUTS("text", "portnew", "", "6")]),
		    ?XE("td", [?INPUT("text", "modulenew", "")]),
		    ?XE("td", [?INPUTS("text", "optsnew", "", "40")]),
		    ?XAE("td", [{"colspan", "2"}],
			 [?INPUTT("submit", "addnew", "Add New")])
		   ]
		  )]
	     )]).


node_ports_parse_query(Node, Ports, Query) ->
    lists:foreach(
      fun({Port, _Module1, _Opts1}) ->
	      SPort = integer_to_list(Port),
	      case lists:keysearch("add" ++ SPort, 1, Query) of
		  {value, _} ->
		      {{value, {_, SModule}}, {value, {_, SOpts}}} =
			  {lists:keysearch("module" ++ SPort, 1, Query),
			   lists:keysearch("opts" ++ SPort, 1, Query)},
		      Module = list_to_atom(SModule),
		      {ok, Tokens, _} = erl_scan:string(SOpts ++ "."),
		      {ok, Opts} = erl_parse:parse_term(Tokens),
		      ejabberd_listener:delete_listener(Port),
		      ejabberd_listener:add_listener(Port, Module, Opts),
		      throw(submitted);
		  _ ->
		      case lists:keysearch("delete" ++ SPort, 1, Query) of
			  {value, _} ->
			      ejabberd_listener:delete_listener(Port),
			      throw(submitted);
			  _ ->
			      ok
		      end
	      end
      end, Ports),
    case lists:keysearch("addnew", 1, Query) of
	{value, _} ->
	    {{value, {_, SPort}},
	     {value, {_, SModule}},
	     {value, {_, SOpts}}} =
		{lists:keysearch("portnew", 1, Query),
		 lists:keysearch("modulenew", 1, Query),
		 lists:keysearch("optsnew", 1, Query)},
	    Port = list_to_integer(SPort),
	    Module = list_to_atom(SModule),
	    {ok, Tokens, _} = erl_scan:string(SOpts ++ "."),
	    {ok, Opts} = erl_parse:parse_term(Tokens),
	    ejabberd_listener:add_listener(Port, Module, Opts),
	    throw(submitted);
	_ ->
	    ok
    end.



pretty_print(El) ->
    lists:flatten(pretty_print(El, "")).

pretty_print({xmlcdata, CData}, Prefix) ->
    [Prefix, CData, $\n];
pretty_print({xmlelement, Name, Attrs, Els}, Prefix) ->
    [Prefix, $<, Name,
     case Attrs of
	 [] ->
	     [];
	 [{Attr, Val} | RestAttrs] ->
	     AttrPrefix = [Prefix,
			   string:copies(" ", length(Name) + 2)],
	     [$\s, Attr, $=, $', xml:crypt(Val), $' |
	      lists:map(fun({Attr1, Val1}) ->
				[$\n, AttrPrefix,
				 Attr1, $=, $', xml:crypt(Val1), $']
			end, RestAttrs)]
     end,
     if
	 Els == [] ->
	     "/>\n";
	 true ->
	     OnlyCData = lists:all(fun({xmlcdata, _}) -> true;
				      ({xmlelement, _, _, _}) -> false
				   end, Els),
	     if
		 OnlyCData ->
		     [$>,
		      xml:get_cdata(Els),
		      $<, $/, Name, $>, $\n
		     ];
		 true ->
		     [$>, $\n,
		      lists:map(fun(E) ->
					pretty_print(E, [Prefix, "  "])
				end, Els),
		      Prefix, $<, $/, Name, $>, $\n
		     ]
	     end
     end].

