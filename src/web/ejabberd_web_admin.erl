%%%----------------------------------------------------------------------
%%% File    : ejabberd_web_admin.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Administration web interface
%%% Created :  9 Apr 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
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

-module(ejabberd_web_admin).
-author('alexey@process-one.net').

%% External exports
-export([process/2,
	 list_users/4,
	 list_users_in_diapason/4,
	 pretty_print_xml/1,
	 term_to_id/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").


process(["server", SHost | RPath], #request{auth = Auth} = Request) ->
    Host = jlib:nameprep(SHost),
    case lists:member(Host, ?MYHOSTS) of
	true ->
	    case get_auth(Auth) of
		{User, Server} ->
		    case acl:match_rule(
			   Host, configure, jlib:make_jid(User, Server, "")) of
			deny ->
                            ejabberd_web:error(not_allowed);
			allow ->
			    process_admin(
			      Host, Request#request{path = RPath,
						    us = {User, Server}})
		    end;
		unauthorized ->
		    {401,
		     [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
		     ejabberd_web:make_xhtml([{xmlelement, "h1", [],
                                               [{xmlcdata, "401 Unauthorized"}]}])}
	    end;
	false ->
            ejabberd_web:error(not_found)
    end;

process(RPath, #request{auth = Auth} = Request) ->
    case get_auth(Auth) of
	{User, Server} ->
	    case acl:match_rule(
		   global, configure, jlib:make_jid(User, Server, "")) of
		deny ->
                    ejabberd_web:error(not_allowed);
		allow ->
		    process_admin(
		      global, Request#request{path = RPath,
					      us = {User, Server}})
	    end;
	unauthorized ->
            %% XXX bard: any reason to send this data now and not
            %% always in case of an 401? ought to check http specs...
	    {401, 
	     [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	     ejabberd_web:make_xhtml([{xmlelement, "h1", [],
				       [{xmlcdata, "401 Unauthorized"}]}])}
    end.

get_auth(Auth) ->
    case Auth of
        {SJID, P} ->
            case jlib:string_to_jid(SJID) of
                error ->
                    unauthorized;
                #jid{user = U, server = S} ->
                    case ejabberd_auth:check_password(U, S, P) of
                        true ->
                            {U, S};
                        false ->
                            unauthorized
                    end
            end;
         _ ->
            unauthorized
    end.

make_xhtml(Els, global, Lang) ->
    MenuItems1 = ejabberd_hooks:run_fold(webadmin_menu_main, [], [Lang]),
    MenuItems2 = [?LI([?AC("/admin/"++MI_uri++"/", MI_name)]) || {MI_uri, MI_name} <- MenuItems1],
    {200, [html],
     {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"},
			   {"xml:lang", Lang},
			   {"lang", Lang}],
      [{xmlelement, "head", [],
	[?XCT("title", "ejabberd Web Admin"),
	 {xmlelement, "meta", [{"http-equiv", "Content-Type"},
			       {"content", "text/html; charset=utf-8"}], []},
	 {xmlelement, "link", [{"href", "/admin/favicon.ico"},
			       {"type", "image/x-icon"},
			       {"rel", "shortcut icon"}], []},
	 {xmlelement, "link", [{"href", "/admin/style.css"},
			       {"type", "text/css"},
			       {"rel", "stylesheet"}], []}]},
       ?XE("body",
           [?XAE("div",
		 [{"id", "container"}],
		 [?XAE("div",
		       [{"id", "header"}],
		       [?XE("h1",
			    [?ACT("/admin/", "Administration")]
			   )]),
		  ?XAE("div",
		       [{"id", "navigation"}],
		       [?XE("ul",
			    [?LI([?ACT("/admin/acls/", "Access Control Lists")]),
			     ?LI([?ACT("/admin/access/", "Access Rules")]),
			     ?LI([?ACT("/admin/vhosts/", "Virtual Hosts")]),
			     ?LI([?ACT("/admin/nodes/", "Nodes")]),
			     ?LI([?ACT("/admin/stats/", "Statistics")])
			    ] ++ MenuItems2
			   )]),
		  ?XAE("div",
		       [{"id", "content"}],
		       Els),
		  ?XAE("div",
		       [{"id", "clearcopyright"}],
		       [{xmlcdata, ""}])]),
	    ?XAE("div",
		 [{"id", "copyrightouter"}],
		 [?XAE("div",
		       [{"id", "copyright"}],
		       [?XC("p",
			     "ejabberd (c) 2002-2008 ProcessOne")
		       ])])])
      ]}};

make_xhtml(Els, Host, Lang) ->
    Base = "/admin/server/" ++ Host ++ "/",
    MenuItems1 = ejabberd_hooks:run_fold(webadmin_menu_host, Host, [], [Host, Lang]),
    MenuItems2 = [?LI([?AC(Base ++ MI_uri ++ "/", MI_name)]) || {MI_uri, MI_name} <- MenuItems1],
    {200, [html],
     {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"},
			   {"xml:lang", Lang},
			   {"lang", Lang}],
      [{xmlelement, "head", [],
	[?XCT("title", "ejabberd Web Admin"),
	 {xmlelement, "meta", [{"http-equiv", "Content-Type"},
			       {"content", "text/html; charset=utf-8"}], []},
	 {xmlelement, "link", [{"href", "/admin/favicon.ico"},
			       {"type", "image/x-icon"},
			       {"rel", "shortcut icon"}], []},
	 {xmlelement, "link", [{"href", Base ++ "style.css"},
			       {"type", "text/css"},
			       {"rel", "stylesheet"}], []}]},
       ?XE("body",
           [?XAE("div",
		 [{"id", "container"}],
		 [?XAE("div",
		       [{"id", "header"}],
		       [?XE("h1",
			    [?ACT("/admin/", "Administration")]
			   )]),
		  ?XAE("div",
		       [{"id", "navigation"}],
		       [?XE("ul",
			    [?LI([?XAE("div",
			        [{"id", "navheadhost"}],
			        [?AC(Base, Host)]
			     )]),
			     ?LI([?ACT(Base ++ "acls/", "Access Control Lists")]),
			     ?LI([?ACT(Base ++ "access/", "Access Rules")]),
			     ?LI([?ACT(Base ++ "users/", "Users")]),
			     ?LI([?ACT(Base ++ "online-users/", "Online Users")]),
			     ?LI([?ACT(Base ++ "last-activity/", "Last Activity")]),
			     ?LI([?ACT(Base ++ "nodes/", "Nodes")]),
			     ?LI([?ACT(Base ++ "stats/", "Statistics")])
			    ] ++ MenuItems2
			   )]),
		  ?XAE("div",
		       [{"id", "content"}],
		       Els),
		  ?XAE("div",
		       [{"id", "clearcopyright"}],
		       [{xmlcdata, ""}])]),
	    ?XAE("div",
		 [{"id", "copyrightouter"}],
		 [?XAE("div",
		       [{"id", "copyright"}],
		       [?XC("p",
			     "ejabberd (c) 2002-2008 ProcessOne")
		       ])])])
      ]}}.

css(Host) ->
    Base = case Host of
	       global ->
		   "/admin/";
	       _ ->
		   "/admin/server/" ++ Host ++ "/"
	   end,
    "
html,body {
  background: white;
  margin: 0;
  padding: 0;
  height: 100%;
}

#container {
  padding: 0;
  margin: 0;
  min-height: 100%;
  height: 100%;
  margin-bottom: -30px;
}

html>body #container {
  height: auto;
}

#header h1 {
  width: 100%;
  height: 55px;
  padding: 0;
  margin: 0;
  background: transparent url(\"" ++ Base ++ "logo-fill.png\");
}

#header h1 a {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 55px;
  padding: 0;
  margin: 0;
  background: transparent url(\"" ++ Base ++ "logo.png\") no-repeat;
  display: block;
  text-indent: -700em;
}

#clearcopyright {
  display: block;
  width: 100%;
  height: 30px;
}

#copyrightouter {
  display: table;
  width: 100%;
  height: 30px;
}

#copyright {
  display: table-cell;
  vertical-align: bottom;
  width: 100%;
  height: 30px;
}

#copyright p {
  margin-left: 0;
  margin-right: 0;
  margin-top: 5px;
  margin-bottom: 0;
  padding-left: 0;
  padding-right: 0;
  padding-top: 1px;
  padding-bottom: 1px;
  width: 100%;
  color: #ffffff;
  background-color: #fe8a00;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 7pt;
  font-weight: bold;
  text-align: center;
}

#navigation ul {
  position: absolute;
  top: 54px;
  left: 0;
  padding: 0 1px 1px 1px;
  margin: 0;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 8pt;
  font-weight: bold;
  background: #d47911;
  width: 13em;
}

#navigation ul li {
  list-style: none;
  margin: 0;
  text-align: left;
  display: inline;
}

#navigation ul li a {
  margin: 0;
  display: block;
  padding: 3px 6px 3px 9px;
  border-left: 1em solid #ffc78c;
  border-top: 1px solid #d47911;
  background: #ffe3c9;
  text-decoration: none;
}

#navigation ul li a:link {
  color: #844;
}

#navigation ul li a:visited {
 color: #766;
}

#navigation ul li a:hover {
  border-color: #fc8800;
  color: #FFF;
  background: #332;
}

#navheadhost {
  text-align: left;
  border-bottom: 2px solid #d47911;
}

#lastactivity li {
  font-weight: bold;
  border: 1px solid #d6760e;
  background-color: #fff2e8;
  padding: 2px;
  margin-bottom: -1px;
}

td.copy {
  color: #ffffff;
  background-color: #fe8a00;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 7pt;
  font-weight: bold;
  text-align: center;
}

input {
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  border: 1px solid #d6760e;
  color: #723202;
  background-color: #fff2e8;
  vertical-align: middle;
  margin-bottom: 0px;
  padding: 0.1em;
}

input[type=submit] {
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 8pt;
  font-weight: bold;
  color: #ffffff;
  background-color: #fe8a00;
  border: 1px solid #d6760e;
}

textarea {
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  border: 1px solid #d6760e;
  color: #723202;
  background-color: #fff2e8;
  vertical-align: middle;
  margin-top: 7px;
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
  padding-top: 20px;
  padding-bottom: 2px;
  margin-top: 0px;
  margin-bottom: 0px;
}

#content a:link {
  color: #990000; 
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: bold;
  text-decoration: underline;
}
#content a:visited {
  color: #990000;  
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: bold;
  text-decoration: underline;
}
#content a:hover {
  color: #cc6600;  
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  font-weight: bold;
  text-decoration: underline;
}


#content ul li {
  list-style-type: disc;
  font-size: 10pt;
  /*font-size: 7pt;*/
  padding-left: 10px;
}

#content li.big {
  font-size: 10pt;
}

#content {
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  padding-left: 13em;
  padding-top: 5px;
}

*.alignright {
  text-align: right;
}

".

favicon() ->
    jlib:decode_base64(
      "AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAA"
      "AAAAAAAAAAAAAAAAAAAAAAAAJf+cAAIPsAAGC8gAVhecAAIr8ACiR7wBBmO"
      "cAUKPsAFun8ABhqeoAgLryAJLB8ACz1PcAv9r7AMvi+gAAAAAAAgICARMhI"
      "CAkJCQkQkFCQgICN2d2cSMgJCRevdvVQkICAlqYh5MgICQkXrRCQkJCMgI7"
      "kiAjICAUFF2swkFBQRQUXazCQUFBAgI7kiAgICAkJF60QkJCQgICOpiHkyA"
      "gJCRevdvlQkICAjdndnMgICQkJCRCQkJCAgICARAgICAAAAAAAAAAAAAAAA"
      "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      "AAAAAAAAAAA").

logo() ->
    jlib:decode_base64(
      "iVBORw0KGgoAAAANSUhEUgAAAVcAAAA3CAMAAACPbPnEAAAAYFBMVEX///8C"
      "AgJyMgL+vm7Wdg7+igL+/v7+slb+qkb+4sr+ojP+nir+lhr+1qb+khL+wnb+"
      "wn7+zpb+jgb+yoz+xo7+tmL+pj7+mib+jg7+5sb+rlL+rkr+mh7+tl7+2q7+"
      "umpJ0uikAAAAAXRSTlMAQObYZgAAAAFiS0dEAIgFHUgAAAAJcEhZcwAACxIA"
      "AAsSAdLdfvwAAAAHdElNRQfUBAUJBhWzc9qJAAABQ0lEQVR42u2bXU/CQBBF"
      "UUZFURAU5Ev4//+S3Ow+tFl3s6adtE3Oebghzc4DJ/Nw04WZgQczexJkz4lX"
      "vOKVxKuXV6APTCFXAq94xSte8ermFYbrA6+ilemZRxGz+fxBxMydL0/Vz5an"
      "vkUrPfb1IPCKV7ziFa9uXsG/DzyLPz7ndjS3tc3tSbcwPdl9tmYq3dHmk9x3"
      "r8mtiM11KfCKV7ziFa9uXmEc7wf+u6+5TtlXf62fKu9rl3wX9ibsLPCKV7zi"
      "Fa9uXmF87wf67aBT6a+hp4bOehFxU0/CbgKveMUrXvHq5hXG+vuBcpss75zH"
      "/VZ5X7vcb4W7q5A/wvbCXoTNhX0JvOIVr3jFq5tX4P8Fw2V6g7UQ9itsLeKm"
      "fgi84hWveMWrm1egDwyX6Q3WTtinsI2wq7CjwCte8YpXvLp5BQ/utIiGbwh9"
      "RAEAAAAASUVORK5CYII=").

logo_fill() ->
    jlib:decode_base64(
      "iVBORw0KGgoAAAANSUhEUgAAAAYAAAA3BAMAAADdxCZzAAAAHlBMVEXWdg7+"
      "igL+jg7+khL+nir+rkr+umr+yoz+1qb+5sbOf9L8AAAACXBIWXMAAA9hAAAP"
      "YQGoP6dpAAAAQUlEQVQI12XDSxHAIBAFQT6BJEcsYAELWMACFtYCFnAL7zxd"
      "1c5dvhSU2BpKqBXl6R0ljYGS50R5zVC+tVD+vfE6YyUexE9x7g4AAAAASUVO"
      "RK5CYII=").

process_admin(global,
	      #request{path = [],
		       lang = Lang}) ->
    MenuItems1 = ejabberd_hooks:run_fold(webadmin_menu_main, [], [Lang]),
    MenuItems2 = [?LI([?AC("/admin/"++MI_uri++"/", MI_name)]) || {MI_uri, MI_name} <- MenuItems1],
    make_xhtml([?XCT("h1", "Administration"),
		?XE("ul",
		    [?LI([?ACT("/admin/acls/", "Access Control Lists"), ?C(" "),
			  ?ACT("/admin/acls-raw/", "(Raw)")]),
		     ?LI([?ACT("/admin/access/", "Access Rules"), ?C(" "),
			  ?ACT("/admin/access-raw/", "(Raw)")]),
		     ?LI([?ACT("/admin/vhosts/", "Virtual Hosts")]),
		     ?LI([?ACT("/admin/nodes/", "Nodes")]),
		     ?LI([?ACT("/admin/stats/", "Statistics")])
		    ] ++ MenuItems2
		   )
	       ], global, Lang);

process_admin(Host,
	      #request{path = [],
		       lang = Lang}) ->
    Base = "/admin/server/" ++ Host ++ "/",
    MenuItems1 = ejabberd_hooks:run_fold(webadmin_menu_host, Host, [], [Host, Lang]),
    MenuItems2 = [?LI([?AC(Base ++ MI_uri ++ "/", MI_name)]) || {MI_uri, MI_name} <- MenuItems1],
    make_xhtml([?XCT("h1", "Administration"),
		?XE("ul",
		    [?LI([?ACT(Base ++ "acls/", "Access Control Lists"), ?C(" "),
			  ?ACT(Base ++ "acls-raw/", "(Raw)")]),
		     ?LI([?ACT(Base ++ "access/", "Access Rules"), ?C(" "),
			  ?ACT(Base ++ "access-raw/", "(Raw)")]),
		     ?LI([?ACT(Base ++ "users/", "Users")]),
		     ?LI([?ACT(Base ++ "online-users/", "Online Users")]),
		     ?LI([?ACT(Base ++ "last-activity/", "Last Activity")]),
		     ?LI([?ACT(Base ++ "nodes/", "Nodes")]),
		     ?LI([?ACT(Base ++ "stats/", "Statistics")])
		    ] ++ MenuItems2
		   )
	       ], Host, Lang);

process_admin(Host, #request{path = ["style.css"]}) ->
    {200, [{"Content-Type", "text/css"}, last_modified(), cache_control_public()], css(Host)};

process_admin(_Host, #request{path = ["favicon.ico"]}) ->
    {200, [{"Content-Type", "image/x-icon"}, last_modified(), cache_control_public()], favicon()};

process_admin(_Host, #request{path = ["logo.png"]}) ->
    {200, [{"Content-Type", "image/png"}, last_modified(), cache_control_public()], logo()};

process_admin(_Host, #request{path = ["logo-fill.png"]}) ->
    {200, [{"Content-Type", "image/png"}, last_modified(), cache_control_public()], logo_fill()};

process_admin(Host,
	      #request{path = ["acls-raw"],
		       q = Query,
		       lang = Lang}) ->
    Res = case lists:keysearch("acls", 1, Query) of
	      {value, {_, String}} ->
		  case erl_scan:string(String) of
		      {ok, Tokens, _} ->
			  case erl_parse:parse_term(Tokens) of
			      {ok, NewACLs} ->
				  case acl:add_list(Host, NewACLs, true) of
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
    ACLs = lists:flatten(
	     io_lib:format(
	       "~p.", [lists:keysort(
			 2, ets:select(acl, [{{acl, {'$1', Host}, '$2'},
					      [], [{{acl, '$1', '$2'}}]}]))])),
    make_xhtml([?XCT("h1", "Access Control Lists")] ++
	       case Res of
		   ok -> [?CT("Submitted"), ?P];
		   error -> [?CT("Bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [?XAC("textarea", [{"name", "acls"},
					{"rows", "16"},
					{"cols", "80"}],
			   ACLs),
		      ?BR,
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Host, Lang);

process_admin(Host,
	      #request{method = Method,
			path = ["acls"],
			q = Query,
			lang = Lang}) ->
    ?DEBUG("query: ~p", [Query]),
    Res = case Method of
	      'POST' ->
		  case catch acl_parse_query(Host, Query) of
		      {'EXIT', _} ->
			  error;
		      NewACLs ->
			  ?INFO_MSG("NewACLs at ~s: ~p", [Host, NewACLs]),
			  case acl:add_list(Host, NewACLs, true) of
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
    ACLs = lists:keysort(
	     2, ets:select(acl, [{{acl, {'$1', Host}, '$2'},
				  [], [{{acl, '$1', '$2'}}]}])),
    make_xhtml([?XCT("h1", "Access Control Lists")] ++
	       case Res of
		   ok -> [?CT("Submitted"), ?P];
		   error -> [?CT("Bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XE("p", [?ACT("../acls-raw/", "Raw")])] ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [acls_to_xhtml(ACLs),
		      ?BR,
		      ?INPUTT("submit", "delete", "Delete Selected"),
		      ?C(" "),
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Host, Lang);

process_admin(Host,
	      #request{path = ["access-raw"],
			q = Query,
			lang = Lang}) ->
    SetAccess =
	fun(Rs) ->
		mnesia:transaction(
		  fun() ->
			  Os = mnesia:select(
				 config,
				 [{{config, {access, '$1', Host}, '$2'},
				   [],
				   ['$_']}]),
			  lists:foreach(fun(O) ->
						mnesia:delete_object(O)
					end, Os),
			  lists:foreach(
			    fun({access, Name, Rules}) ->
				    mnesia:write({config,
						  {access, Name, Host},
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
			       [{{config, {access, '$1', Host}, '$2'},
				 [],
				 [{{access, '$1', '$2'}}]}])])),
    make_xhtml([?XCT("h1", "Access Rules")] ++
	       case Res of
		   ok -> [?CT("Submitted"), ?P];
		   error -> [?CT("Bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [?XAC("textarea", [{"name", "access"},
					{"rows", "16"},
					{"cols", "80"}],
			   Access),
		      ?BR,
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Host, Lang);

process_admin(Host,
	      #request{method = Method,
		       path = ["access"],
		       q = Query,
		       lang = Lang}) ->
    ?DEBUG("query: ~p", [Query]),
    Res = case Method of
	      'POST' ->
		  case catch access_parse_query(Host, Query) of
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
		   [{{config, {access, '$1', Host}, '$2'},
		     [],
		     [{{access, '$1', '$2'}}]}]),
    make_xhtml([?XCT("h1", "Access Rules")] ++
	       case Res of
		   ok -> [?CT("Submitted"), ?P];
		   error -> [?CT("Bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XE("p", [?ACT("../access-raw/", "Raw")])] ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [access_rules_to_xhtml(AccessRules, Lang),
		      ?BR,
		      ?INPUTT("submit", "delete", "Delete Selected")
		     ])
	       ], Host, Lang);

process_admin(Host,
	      #request{path = ["access", SName],
		       q = Query,
		       lang = Lang}) ->
    ?DEBUG("query: ~p", [Query]),
    Name = list_to_atom(SName),
    Res = case lists:keysearch("rules", 1, Query) of
	      {value, {_, String}} ->
		  case parse_access_rule(String) of
		      {ok, Rs} ->
			  ejabberd_config:add_global_option(
			    {access, Name, Host}, Rs),
			  ok;
		      _ ->
			  error
		  end;
	      _ ->
		  nothing
	  end,
    Rules = case ejabberd_config:get_global_option({access, Name, Host}) of
		undefined ->
		    [];
		Rs1 ->
		    Rs1
	    end,
    make_xhtml([?XC("h1",
		    io_lib:format(?T("~s access rule configuration"), [SName]))] ++
	       case Res of
		   ok -> [?CT("Submitted"), ?P];
		   error -> [?CT("Bad format"), ?P];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [access_rule_to_xhtml(Rules),
		      ?BR,
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Host, Lang);

process_admin(global,
	      #request{path = ["vhosts"],
		       lang = Lang}) ->
    Res = list_vhosts(Lang),
    make_xhtml([?XCT("h1", "ejabberd virtual hosts")] ++ Res, global, Lang);

process_admin(Host,
	      #request{path = ["users"],
		       q = Query,
		       lang = Lang}) when is_list(Host) ->
    Res = list_users(Host, Query, Lang, fun url_func/1),
    make_xhtml([?XCT("h1", "Users")] ++ Res, Host, Lang);

process_admin(Host,
	      #request{path = ["users", Diap],
		       lang = Lang}) when is_list(Host) ->
    Res = list_users_in_diapason(Host, Diap, Lang, fun url_func/1),
    make_xhtml([?XCT("h1", "Users")] ++ Res, Host, Lang);

process_admin(Host,
	      #request{
		       path = ["online-users"],
		       lang = Lang}) when is_list(Host) ->
    Res = list_online_users(Host, Lang),
    make_xhtml([?XCT("h1", "Online Users")] ++ Res, Host, Lang);

process_admin(Host,
	      #request{path = ["last-activity"],
		       q = Query,
		       lang = Lang}) when is_list(Host) ->
    ?DEBUG("query: ~p", [Query]),
    Month = case lists:keysearch("period", 1, Query) of
		{value, {_, Val}} ->
		    Val;
		_ ->
		    "month"
	    end,
    Res = case lists:keysearch("ordinary", 1, Query) of
	      {value, {_, _}} ->
		  list_last_activity(Host, Lang, false, Month);
	      _ ->
		  list_last_activity(Host, Lang, true, Month)
	  end,
    make_xhtml([?XCT("h1", "Users Last Activity")] ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [?CT("Period: "),
		      ?XAE("select", [{"name", "period"}],
			   lists:map(
			     fun({O, V}) ->
				    Sel = if
					      O == Month -> [{"selected", "selected"}];
					      true -> []
					  end,
				    ?XAC("option",
					 Sel ++ [{"value", O}], V)
			     end, [{"month", ?T("Last month")},
				   {"year", ?T("Last year")},
				   {"all", ?T("All activity")}])),
		      ?C(" "),
		      ?INPUTT("submit", "ordinary", "Show Ordinary Table"),
		      ?C(" "),
		      ?INPUTT("submit", "integral", "Show Integral Table")
		     ])] ++
	       Res, Host, Lang);

process_admin(Host,
	      #request{path = ["stats"],
		       lang = Lang}) ->
    Res = get_stats(Host, Lang),
    make_xhtml([?XCT("h1", "Statistics")] ++ Res, Host, Lang);

process_admin(Host,
	      #request{path = ["user", U],
		       q = Query,
		       lang = Lang}) ->
    Res = user_info(U, Host, Query, Lang),
    make_xhtml(Res, Host, Lang);

process_admin(Host,
	      #request{path = ["nodes"],
		       lang = Lang}) ->
    Res = get_nodes(Lang),
    make_xhtml(Res, Host, Lang);

process_admin(Host,
	      #request{path = ["node", SNode | NPath],
		       q = Query,
		       lang = Lang}) ->
    case search_running_node(SNode) of
	false ->
	    make_xhtml([?XCT("h1", "Node not found")], Host, Lang);
	Node ->
	    Res = get_node(Host, Node, NPath, Query, Lang),
	    make_xhtml(Res, Host, Lang)
    end;

process_admin(Host, #request{lang = Lang} = Request) ->
    {Hook, Opts} = case Host of
		       global -> {webadmin_page_main, [Request]};
		       Host -> {webadmin_page_host, [Host, Request]}
		   end,
    case ejabberd_hooks:run_fold(Hook, Host, [], Opts) of
	[] -> setelement(1, make_xhtml([?XC("h1", "Not Found")], Host, Lang), 404);
	Res -> make_xhtml(Res, Host, Lang)
    end.



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

acl_spec_to_text({user, U}) ->
    {user, U};

acl_spec_to_text({server, S}) ->
    {server, S};

acl_spec_to_text({user, U, S}) ->
    {user, U ++ "@" ++ S};

acl_spec_to_text({user_regexp, RU}) ->
    {user_regexp, RU};

acl_spec_to_text({user_regexp, RU, S}) ->
    {user_regexp, RU ++ "@" ++ S};

acl_spec_to_text({server_regexp, RS}) ->
    {server_regexp, RS};

acl_spec_to_text({node_regexp, RU, RS}) ->
    {node_regexp, RU ++ "@" ++ RS};

acl_spec_to_text({user_glob, RU}) ->
    {user_glob, RU};

acl_spec_to_text({user_glob, RU, S}) ->
    {user_glob, RU ++ "@" ++ S};

acl_spec_to_text({server_glob, RS}) ->
    {server_glob, RS};

acl_spec_to_text({node_glob, RU, RS}) ->
    {node_glob, RU ++ "@" ++ RS};

acl_spec_to_text(all) ->
    {all, ""};

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
		end, [user, server, user_regexp, server_regexp, 
		      node_regexp, user_glob, server_glob, node_glob, all, raw]))]).


term_to_string(T) ->
    StringParagraph = lists:flatten(io_lib:format("~1000000p", [T])),
    %% Remove from the string all the carriage returns characters
    {ok, StringLine, _} = regexp:gsub(StringParagraph, "\\n ", ""),
    StringLine.

term_to_id(T) ->
    jlib:encode_base64(binary_to_list(term_to_binary(T))).


acl_parse_query(Host, Query) ->
    ACLs = ets:select(acl, [{{acl, {'$1', Host}, '$2'},
			     [], [{{acl, '$1', '$2'}}]}]),
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
		  %%SName = atom_to_list(Name),
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
    string_to_spec2(user, Val);
string_to_spec("server", Val) ->
    {server, Val};
string_to_spec("user_regexp", Val) ->
    string_to_spec2(user_regexp, Val);
string_to_spec("server_regexp", Val) ->
    {server_regexp, Val};
string_to_spec("node_regexp", Val) ->
    #jid{luser = U, lserver = S, resource = ""} = jlib:string_to_jid(Val),
    {node_regexp, U, S};
string_to_spec("user_glob", Val) ->
    string_to_spec2(user_glob, Val);
string_to_spec("server_glob", Val) ->
    {server_glob, Val};
string_to_spec("node_glob", Val) ->
    #jid{luser = U, lserver = S, resource = ""} = jlib:string_to_jid(Val),
    {node_glob, U, S};
string_to_spec("all", _) ->
    all;
string_to_spec("raw", Val) ->
    {ok, Tokens, _} = erl_scan:string(Val ++ "."),
    {ok, NewSpec} = erl_parse:parse_term(Tokens),
    NewSpec.

string_to_spec2(ACLName, Val) ->
    #jid{luser = U, lserver = S, resource = ""} = jlib:string_to_jid(Val),
    case U of
	"" -> 
	    {ACLName, S};
	_ -> 
	    {ACLName, U, S}
    end.


acl_parse_delete(ACLs, Query) ->
    NewACLs =
	lists:filter(
	  fun({acl, _Name, _Spec} = ACL) ->
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

access_parse_query(Host, Query) ->
    AccessRules =
	ets:select(config,
		   [{{config, {access, '$1', Host}, '$2'},
		     [],
		     [{{access, '$1', '$2'}}]}]),
    case lists:keysearch("addnew", 1, Query) of
	{value, _} ->
	    access_parse_addnew(AccessRules, Host, Query);
	_ ->
	    case lists:keysearch("delete", 1, Query) of
		{value, _} ->
		    access_parse_delete(AccessRules, Host, Query)
	    end
    end.

access_parse_addnew(_AccessRules, Host, Query) ->
    case lists:keysearch("namenew", 1, Query) of
	{value, {_, String}} when String /= "" ->
	    Name = list_to_atom(String),
	    ejabberd_config:add_global_option({access, Name, Host}, []),
	    ok
    end.

access_parse_delete(AccessRules, Host, Query) ->
    lists:foreach(
      fun({access, Name, _Rules} = AccessRule) ->
	      ID = term_to_id(AccessRule),
	      case lists:member({"selected", ID}, Query) of
		  true ->
		      mnesia:transaction(
			fun() ->
				mnesia:delete({config, {access, Name, Host}})
			end);
		  _ ->
		      ok
	      end
      end, AccessRules),
    ok.




access_rule_to_xhtml(Rules) ->
    Text = lists:flatmap(
	     fun({Access, ACL} = _Rule) ->
		     SAccess = element_to_list(Access),
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
				 [{list_to_element(Access), list_to_atom(ACL)}];
			     [] ->
				 []
			 end
		 end, Strings) of
	{'EXIT', _Reason} ->
	    error;
	Rs ->
	    {ok, Rs}
    end.


list_vhosts(Lang) ->
    Hosts = ?MYHOSTS,
    SHosts = lists:sort(Hosts),
    [?XE("table",
	 [?XE("thead",
	      [?XE("tr",
		   [?XCT("td", "Host"),
		    ?XCT("td", "Registered Users"),
		    ?XCT("td", "Online Users")
		   ])]),
	  ?XE("tbody",
	      lists:map(
		fun(Host) ->
			OnlineUsers =
			    length(ejabberd_sm:get_vh_session_list(Host)),
			RegisteredUsers =
			    ejabberd_auth:get_vh_registered_users_number(Host),
			?XE("tr",
			    [?XE("td", [?AC("../server/" ++ Host ++ "/", Host)]),
			     ?XC("td", integer_to_list(RegisteredUsers)),
			     ?XC("td", integer_to_list(OnlineUsers))
			    ])
		end, SHosts)
	     )])].


list_users(Host, Query, Lang, URLFunc) ->
    Res = list_users_parse_query(Query, Host),
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
    FUsers =
	case length(SUsers) of
	    N when N =< 100 ->
		[list_given_users(SUsers, "../", Lang, URLFunc)];
	    N ->
		NParts = trunc(math:sqrt(N * 0.618)) + 1,
		M = trunc(N / NParts) + 1,
		lists:flatmap(
		  fun(K) ->
			  L = K + M - 1,
			  %%Node = integer_to_list(K) ++ "-" ++ integer_to_list(L),
			  Last = if L < N -> su_to_list(lists:nth(L, SUsers));
				    true -> su_to_list(lists:last(SUsers))
				 end,
			  Name = 
			      su_to_list(lists:nth(K, SUsers)) ++
			      [$\s, 226, 128, 148, $\s] ++
			      Last,
			  [?AC(URLFunc({user_diapason, K, L}), Name), ?BR]
		  end, lists:seq(1, N, M))
	end,
    case Res of
	ok -> [?CT("Submitted"), ?P];
	error -> [?CT("Bad format"), ?P];
	nothing -> []
    end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [?XE("table",
		   [?XE("tr",
			[?XC("td", ?T("User") ++ ":"),
			 ?XE("td", [?INPUT("text", "newusername", "")]),
			 ?XE("td", [?C([" @ ", Host])])
			]),
		    ?XE("tr",
			[?XC("td", ?T("Password") ++ ":"),
			 ?XE("td", [?INPUT("password", "newuserpassword", "")]),
			 ?X("td")
			]),
		    ?XE("tr",
			[?X("td"),
			 ?XAE("td", [{"class", "alignright"}],
			      [?INPUTT("submit", "addnewuser", "Add User")]),
			 ?X("td")
			])]),
	       ?P] ++
	      FUsers)].

%% Parse user creation query and try register:
list_users_parse_query(Query, Host) ->
    case lists:keysearch("addnewuser", 1, Query) of
	{value, _} ->
	    {value, {_, Username}} =
		lists:keysearch("newusername", 1, Query),
	    {value, {_, Password}} =
		lists:keysearch("newuserpassword", 1, Query),
	    case jlib:string_to_jid(Username++"@"++Host) of
		error ->
		    error;
		#jid{user = User, server = Server} ->
		    case ejabberd_auth:try_register(User, Server, Password) of
			{error, _Reason} ->
			    error;
			_ ->
			    ok
		    end
	    end;
	false ->
	    nothing
    end.


list_users_in_diapason(Host, Diap, Lang, URLFunc) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
    {ok, [S1, S2]} = regexp:split(Diap, "-"),
    N1 = list_to_integer(S1),
    N2 = list_to_integer(S2),
    Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
    [list_given_users(Sub, "../../", Lang, URLFunc)].

list_given_users(Users, Prefix, Lang, URLFunc) ->
    ?XE("table",
	[?XE("thead",
	     [?XE("tr",
		  [?XCT("td", "User"),
		   ?XCT("td", "Offline Messages"),
		   ?XCT("td", "Last Activity")])]),
	 ?XE("tbody",
	     lists:map(
	       fun(_SU = {Server, User}) ->
		       US = {User, Server},
		       QueueLen = length(mnesia:dirty_read({offline_msg, US})),
		       FQueueLen = [?AC(URLFunc({users_queue, Prefix,
						 User, Server}),
					integer_to_list(QueueLen))],
		       FLast =
			   case ejabberd_sm:get_user_resources(User, Server) of
			       [] ->
				   case mnesia:dirty_read({last_activity, US}) of
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
			   [?XE("td",
				[?AC(URLFunc({user, Prefix,
					      ejabberd_http:url_encode(User),
					      Server}),
				     us_to_list(US))]),
			    ?XE("td", FQueueLen),
			    ?XC("td", FLast)])
	       end, Users)
	    )]).

us_to_list({User, Server}) ->
    jlib:jid_to_string({User, Server, ""}).

su_to_list({Server, User}) ->
    jlib:jid_to_string({User, Server, ""}).


get_stats(global, Lang) ->
    OnlineUsers = mnesia:table_info(session, size),
    RegisteredUsers = mnesia:table_info(passwd, size),
    S2SConns = ejabberd_s2s:dirty_get_connections(),
    S2SConnections = length(S2SConns),
    S2SServers = length(lists:usort([element(2, C) || C <- S2SConns])),
    [?XAE("table", [],
	  [?XE("tbody",
	       [?XE("tr", [?XCT("td", "Registered Users:"),
			   ?XC("td", integer_to_list(RegisteredUsers))]),
		?XE("tr", [?XCT("td", "Online Users:"),
			   ?XC("td", integer_to_list(OnlineUsers))]),
		?XE("tr", [?XCT("td", "Outgoing s2s Connections:"),
			   ?XC("td", integer_to_list(S2SConnections))]),
		?XE("tr", [?XCT("td", "Outgoing s2s Servers:"),
			   ?XC("td", integer_to_list(S2SServers))])
	       ])
	  ])];

get_stats(Host, Lang) ->
    OnlineUsers = length(ejabberd_sm:get_vh_session_list(Host)),
    RegisteredUsers = ejabberd_auth:get_vh_registered_users_number(Host),
    [?XAE("table", [],
	  [?XE("tbody",
	       [?XE("tr", [?XCT("td", "Registered Users:"),
			   ?XC("td", integer_to_list(RegisteredUsers))]),
		?XE("tr", [?XCT("td", "Online Users:"),
			   ?XC("td", integer_to_list(OnlineUsers))])
	       ])
	  ])].


list_online_users(Host, _Lang) ->
    Users = [{S, U} || {U, S, _R} <- ejabberd_sm:get_vh_session_list(Host)],
    SUsers = lists:usort(Users),
    lists:flatmap(
      fun({_S, U} = SU) ->
	      [?AC("../user/" ++ ejabberd_http:url_encode(U) ++ "/",
		   su_to_list(SU)),
	       ?BR]
      end, SUsers).

user_info(User, Server, Query, Lang) ->
    LServer = jlib:nameprep(Server),
    US = {jlib:nodeprep(User), LServer},
    Res = user_parse_query(User, Server, Query),
    Resources = ejabberd_sm:get_user_resources(User, Server),
    FResources =
	case Resources of
	    [] ->
		[?CT("None")];
	    _ ->
		[?XE("ul",
		     lists:map(fun(R) ->
				       FIP = case ejabberd_sm:get_user_ip(
						    User, Server, R) of
						 undefined ->
						     "";
						 {IP, Port} ->
						     " (" ++
							 inet_parse:ntoa(IP) ++
							 ":" ++
							 integer_to_list(Port)
							 ++ ")"
					     end,
				       ?LI([?C(R ++ FIP)])
			       end, lists:sort(Resources)))]
	end,
    Password = ejabberd_auth:get_password_s(User, Server),
    FPassword = [?INPUT("password", "password", Password), ?C(" "),
		 ?INPUTT("submit", "chpassword", "Change Password")],
    UserItems = ejabberd_hooks:run_fold(webadmin_user, LServer, [],
					[User, Server, Lang]),
    [?XC("h1", ?T("User ") ++ us_to_list(US))] ++
	case Res of
	    ok -> [?CT("Submitted"), ?P];
	    error -> [?CT("Bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [?XCT("h3", "Connected Resources:")] ++ FResources ++
	      [?XCT("h3", "Password:")] ++ FPassword ++
	      UserItems ++
	      [?P, ?INPUTT("submit", "removeuser", "Remove User")])].


user_parse_query(User, Server, Query) ->
    case lists:keysearch("chpassword", 1, Query) of
	{value, _} ->
	    case lists:keysearch("password", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, Password}} ->
		    ejabberd_auth:set_password(User, Server, Password),
		    ok;
		_ ->
		    error
	    end;
	_ ->
	    case lists:keysearch("removeuser", 1, Query) of
		{value, _} ->
		    ejabberd_auth:remove_user(User, Server),
		    ok;
		false ->
		    nothing
	    end
    end.



list_last_activity(Host, Lang, Integral, Period) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    case Period of
	"all" ->
	    TS = 0,
	    Days = infinity;
	"year" ->
	    TS = TimeStamp - 366 * 86400,
	    Days = 366;
	_ ->
	    TS = TimeStamp - 31 * 86400,
	    Days = 31
    end,
    case catch mnesia:dirty_select(
		 last_activity, [{{last_activity, {'_', Host}, '$1', '_'},
				  [{'>', '$1', TS}],
				  [{'trunc', {'/',
					      {'-', TimeStamp, '$1'},
					      86400}}]}]) of
	{'EXIT', _Reason} ->
	    [];
	Vals ->
	    Hist = histogram(Vals, Integral),
	    if
		Hist == [] ->
		    [?CT("No Data")];
		true ->
		    Left = if
			       Days == infinity ->
				   0;
			       true ->
				   Days - length(Hist)
			   end,
		    Tail = if
			       Integral ->
				   lists:duplicate(Left, lists:last(Hist));
			       true ->
				   lists:duplicate(Left, 0)
			   end,
		    Max = lists:max(Hist),
		    [?XAE("ol",
			  [{"id", "lastactivity"}, {"start", "0"}],
			  [?XAE("li",
				[{"style",
				  "width:" ++ integer_to_list(
						trunc(90 * V / Max)) ++ "%;"}],
				[{xmlcdata, integer_to_list(V)}])
			   || V <- Hist ++ Tail])]
	    end
    end.

histogram(Values, Integral) ->
    histogram(lists:sort(Values), Integral, 0, 0, []).

histogram([H | T], Integral, Current, Count, Hist) when Current == H ->
    histogram(T, Integral, Current, Count + 1, Hist);
	    
histogram([H | _] = Values, Integral, Current, Count, Hist) when Current < H ->
    if
	Integral ->
	    histogram(Values, Integral, Current + 1, Count, [Count | Hist]);
	true ->
	    histogram(Values, Integral, Current + 1, 0, [Count | Hist])
    end;

histogram([], _Integral, _Current, Count, Hist) ->
    if
	Count > 0 ->
	    lists:reverse([Count | Hist]);
	true ->
	    lists:reverse(Hist)
    end.


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

get_node(global, Node, [], Query, Lang) ->
    Res = node_parse_query(Node, Query),
    MenuItems1 = ejabberd_hooks:run_fold(webadmin_menu_node, [], [Node, Lang]),
    MenuItems2 = [?LI([?AC(MI_uri++"/", MI_name)]) || {MI_uri, MI_name} <- MenuItems1],
    [?XC("h1", ?T("Node ") ++ atom_to_list(Node))] ++
	case Res of
	    ok -> [?CT("Submitted"), ?P];
	    error -> [?CT("Bad format"), ?P];
	    nothing -> []
	end ++
	[?XE("ul",
	     [?LI([?ACT("db/", "Database")]),
	      ?LI([?ACT("backup/", "Backup")]),
	      ?LI([?ACT("ports/", "Listened Ports")]),
	      ?LI([?ACT("stats/", "Statistics")]),
	      ?LI([?ACT("update/", "Update")])
	     ] ++ MenuItems2),
	 ?XAE("form", [{"action", ""}, {"method", "post"}],
	      [?INPUTT("submit", "restart", "Restart"),
	       ?C(" "),
	       ?INPUTT("submit", "stop", "Stop")])
	];

get_node(Host, Node, [], _Query, Lang) ->
    MenuItems1 = ejabberd_hooks:run_fold(webadmin_menu_hostnode, Host, [], [Host, Node, Lang]),
    MenuItems2 = [?LI([?AC(MI_uri++"/", MI_name)]) || {MI_uri, MI_name} <- MenuItems1],
    [?XC("h1", ?T("Node ") ++ atom_to_list(Node)),
     ?XE("ul",
	 [?LI([?ACT("modules/", "Modules")])] ++ MenuItems2)
    ];

get_node(global, Node, ["db"], Query, Lang) ->
    case rpc:call(Node, mnesia, system_info, [tables]) of
	{badrpc, _Reason} ->
	    [?XCT("h1", "RPC Call Error")];
	Tables ->
	    node_db_parse_query(Node, Tables, Query),
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
	    [?XC("h1", ?T("Database Tables at ") ++ atom_to_list(Node))] ++
		[?CT("Submitted"), ?P] ++
		[?XAE("form", [{"action", ""}, {"method", "post"}],
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

get_node(global, Node, ["backup"], Query, Lang) ->
    _Res = node_backup_parse_query(Node, Query),
    [?XC("h1", ?T("Backup of ") ++ atom_to_list(Node)),
     ?XCT("p", "Remark that these options will only backup the builtin Mnesia database. If you are using the ODBC module, you also need to backup your SQL database separately."),
     ?XAE("form", [{"action", ""}, {"method", "post"}],
	  [?XAE("table", [],
		[?XE("tbody",
		     [?XE("tr",
 			  [?XCT("td", "Store binary backup:"),
			   ?XE("td", [?INPUT("text", "storepath",
					     "ejabberd.backup")]),
			   ?XE("td", [?INPUTT("submit", "store",
					      "OK")])
			  ]),
		      ?XE("tr",
 			  [?XCT("td", "Restore binary backup immediately:"),
			   ?XE("td", [?INPUT("text", "restorepath",
					     "ejabberd.backup")]),
			   ?XE("td", [?INPUTT("submit", "restore",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td",
				"Restore binary backup after next ejabberd restart (requires less memory):"),
			   ?XE("td", [?INPUT("text", "fallbackpath",
					     "ejabberd.backup")]),
			   ?XE("td", [?INPUTT("submit", "fallback",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td", "Store plain text backup:"),
			   ?XE("td", [?INPUT("text", "dumppath",
					     "ejabberd.dump")]),
			   ?XE("td", [?INPUTT("submit", "dump",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td", "Restore plain text backup immediately:"),
			   ?XE("td", [?INPUT("text", "loadpath",
					     "ejabberd.dump")]),
			   ?XE("td", [?INPUTT("submit", "load",
					      "OK")])
			  ])
		     ])
		])])];

get_node(global, Node, ["ports"], Query, Lang) ->
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
	    ok -> [?CT("Submitted"), ?P];
	    error -> [?CT("Bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [node_ports_to_xhtml(NewPorts, Lang)])
	];

get_node(Host, Node, ["modules"], Query, Lang) when is_list(Host) ->
    Modules = rpc:call(Node, gen_mod, loaded_modules_with_opts, [Host]),
    Res = case catch node_modules_parse_query(Host, Node, Modules, Query) of
	      submitted ->
		  ok;
	      {'EXIT', Reason} ->
		  ?INFO_MSG("~p~n", [Reason]),
		  error;
	      _ ->
		  nothing
	  end,
    NewModules = lists:sort(
		   rpc:call(Node, gen_mod, loaded_modules_with_opts, [Host])),
    [?XC("h1", ?T("Modules at ") ++ atom_to_list(Node))] ++
	case Res of
	    ok -> [?CT("Submitted"), ?P];
	    error -> [?CT("Bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [node_modules_to_xhtml(NewModules, Lang)])
	];

get_node(global, Node, ["stats"], _Query, Lang) ->
    UpTime = rpc:call(Node, erlang, statistics, [wall_clock]),
    UpTimeS = io_lib:format("~.3f", [element(1, UpTime)/1000]),
    CPUTime = rpc:call(Node, erlang, statistics, [runtime]),
    CPUTimeS = io_lib:format("~.3f", [element(1, CPUTime)/1000]),
    OnlineUsers = mnesia:table_info(session, size),
    TransactionsCommited =
	rpc:call(Node, mnesia, system_info, [transaction_commits]),
    TransactionsAborted =
	rpc:call(Node, mnesia, system_info, [transaction_failures]),
    TransactionsRestarted =
	rpc:call(Node, mnesia, system_info, [transaction_restarts]),
    TransactionsLogged =
	rpc:call(Node, mnesia, system_info, [transaction_log_writes]),
    
    [?XC("h1", io_lib:format(?T("Statistics of ~p"), [Node])),
     ?XAE("table", [],
	  [?XE("tbody",
	       [?XE("tr", [?XCT("td", "Uptime:"),
			   ?XAC("td", [{"class", "alignright"}],
				UpTimeS)]),
		?XE("tr", [?XCT("td", "CPU Time:"),
			   ?XAC("td", [{"class", "alignright"}],
				CPUTimeS)]),
		?XE("tr", [?XCT("td", "Online Users:"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(OnlineUsers))]),
		?XE("tr", [?XCT("td", "Transactions Commited:"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(TransactionsCommited))]),
		?XE("tr", [?XCT("td", "Transactions Aborted:"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(TransactionsAborted))]),
		?XE("tr", [?XCT("td", "Transactions Restarted:"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(TransactionsRestarted))]),
		?XE("tr", [?XCT("td", "Transactions Logged:"),
			   ?XAC("td", [{"class", "alignright"}],
				integer_to_list(TransactionsLogged))])
	       ])
	  ])];

get_node(global, Node, ["update"], Query, Lang) ->
    rpc:call(Node, code, purge, [ejabberd_update]),
    Res = node_update_parse_query(Node, Query),
    rpc:call(Node, code, load_file, [ejabberd_update]),
    {ok, _Dir, UpdatedBeams, Script, LowLevelScript, Check} =
	rpc:call(Node, ejabberd_update, update_info, []),
    Mods =
	case UpdatedBeams of
	    [] ->
		?CT("None");
	    _ ->
		?XE("ul",
		    [?LI([?C(atom_to_list(Beam))]) ||
			Beam <- UpdatedBeams])
	end,
    FmtScript = ?XC("pre", io_lib:format("~p", [Script])),
    FmtLowLevelScript = ?XC("pre", io_lib:format("~p", [LowLevelScript])),
    [?XC("h1", ?T("Update ") ++ atom_to_list(Node))] ++
	case Res of
	    ok -> [?CT("Submitted"), ?P];
	    error -> [?CT("Bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [?INPUTT("submit", "update", "Update"),
	       ?XCT("h2", "Update plan"),
	       ?XCT("h3", "Updated modules"), Mods,
	       ?XCT("h3", "Update script"), FmtScript,
	       ?XCT("h3", "Low level update script"), FmtLowLevelScript,
	       ?XCT("h3", "Script check"), ?C(atom_to_list(Check))])
	];

get_node(Host, Node, NPath, Query, Lang) ->
    {Hook, Opts} = case Host of
		       global -> {webadmin_page_node, [Node, NPath, Query, Lang]};
		       Host -> {webadmin_page_hostnode, [Host, Node, NPath, Query, Lang]}
		   end,
    case ejabberd_hooks:run_fold(Hook, Host, [], Opts) of
	[] -> [?XC("h1", "Not Found")];
	Res -> Res
    end.



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
					  rpc:call(Node, ejabberd_admin,
						   restore, [Path]);
				      "fallback" ->
					  rpc:call(Node, mnesia,
						   install_fallback, [Path]);
				      "dump" ->
					  rpc:call(Node, ejabberd_ctl,
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
		fun({Port, Module, Opts} = _E) ->
			SPort = integer_to_list(Port),
			SModule = atom_to_list(Module),
			%%ID = term_to_id(E),
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
		      rpc:call(Node, ejabberd_listener, delete_listener, [Port]),
		      rpc:call(Node, ejabberd_listener, add_listener, [Port, Module, Opts]),
		      throw(submitted);
		  _ ->
		      case lists:keysearch("delete" ++ SPort, 1, Query) of
			  {value, _} ->
			      rpc:call(Node, ejabberd_listener, delete_listener, [Port]),
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
	    rpc:call(Node, ejabberd_listener, add_listener, [Port, Module, Opts]),
	    throw(submitted);
	_ ->
	    ok
    end.

node_modules_to_xhtml(Modules, Lang) ->
    ?XAE("table", [],
	 [?XE("thead",
	      [?XE("tr",
		   [?XCT("td", "Module"),
		    ?XCT("td", "Options")
		   ])]),
	  ?XE("tbody",
	      lists:map(
		fun({Module, Opts} = _E) ->
			SModule = atom_to_list(Module),
			%%ID = term_to_id(E),
			?XE("tr",
			    [?XC("td", SModule),
			     ?XE("td", [?INPUTS("text", "opts" ++ SModule,
						term_to_string(Opts), "40")]),
			     ?XE("td", [?INPUTT("submit", "restart" ++ SModule,
						"Restart")]),
			     ?XE("td", [?INPUTT("submit", "stop" ++ SModule,
						"Stop")])
			    ]
			   )
		end, Modules) ++
	      [?XE("tr",
		   [?XE("td", [?INPUT("text", "modulenew", "")]),
		    ?XE("td", [?INPUTS("text", "optsnew", "", "40")]),
		    ?XAE("td", [{"colspan", "2"}],
			 [?INPUTT("submit", "start", "Start")])
		   ]
		  )]
	     )]).

node_modules_parse_query(Host, Node, Modules, Query) ->
    lists:foreach(
      fun({Module, _Opts1}) ->
	      SModule = atom_to_list(Module),
	      case lists:keysearch("restart" ++ SModule, 1, Query) of
		  {value, _} ->
		      {value, {_, SOpts}} =
			   lists:keysearch("opts" ++ SModule, 1, Query),
		      {ok, Tokens, _} = erl_scan:string(SOpts ++ "."),
		      {ok, Opts} = erl_parse:parse_term(Tokens),
		      rpc:call(Node, gen_mod, stop_module, [Host, Module]),
		      rpc:call(Node, gen_mod, start_module, [Host, Module, Opts]),
		      throw(submitted);
		  _ ->
		      case lists:keysearch("stop" ++ SModule, 1, Query) of
			  {value, _} ->
			      rpc:call(Node, gen_mod, stop_module, [Host, Module]),
			      throw(submitted);
			  _ ->
			      ok
		      end
	      end
      end, Modules),
    case lists:keysearch("start", 1, Query) of
	{value, _} ->
	    {{value, {_, SModule}},
	     {value, {_, SOpts}}} =
		{lists:keysearch("modulenew", 1, Query),
		 lists:keysearch("optsnew", 1, Query)},
	    Module = list_to_atom(SModule),
	    {ok, Tokens, _} = erl_scan:string(SOpts ++ "."),
	    {ok, Opts} = erl_parse:parse_term(Tokens),
	    rpc:call(Node, gen_mod, start_module, [Host, Module, Opts]),
	    throw(submitted);
	_ ->
	    ok
    end.


node_update_parse_query(Node, Query) ->
    case lists:keysearch("update", 1, Query) of
	{value, _} ->
	    case rpc:call(Node, ejabberd_update, update, []) of
		{ok, _} ->
		    ok;
		{error, Error} ->
		    ?ERROR_MSG("~p~n", [Error]);
		{badrpc, Error} ->
		    ?ERROR_MSG("~p~n", [Error])
	    end;
	_ ->
	    nothing
    end.


pretty_print_xml(El) ->
    lists:flatten(pretty_print_xml(El, "")).

pretty_print_xml({xmlcdata, CData}, Prefix) ->
    [Prefix, CData, $\n];
pretty_print_xml({xmlelement, Name, Attrs, Els}, Prefix) ->
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
					pretty_print_xml(E, [Prefix, "  "])
				end, Els),
		      Prefix, $<, $/, Name, $>, $\n
		     ]
	     end
     end].

element_to_list(X) when is_atom(X) -> atom_to_list(X);
element_to_list(X) when is_integer(X) -> integer_to_list(X).

list_to_element(List) ->
    {ok, Tokens, _} = erl_scan:string(List),
    [{_, _, Element}] = Tokens,
    Element.

url_func({user_diapason, From, To}) ->
    integer_to_list(From) ++ "-" ++ integer_to_list(To) ++ "/";
url_func({users_queue, Prefix, User, _Server}) ->
    Prefix ++ "user/" ++ User ++ "/queue/";
url_func({user, Prefix, User, _Server}) ->
    Prefix ++ "user/" ++ User ++ "/".

last_modified() ->
    {"Last-Modified", "Mon, 25 Feb 2008 13:23:30 GMT"}.
cache_control_public() ->
    {"Cache-Control", "public"}.
