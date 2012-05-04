%%%----------------------------------------------------------------------
%%% File    : ejabberd_web_admin.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Administration web interface
%%% Created :  9 Apr 2004 by Alexey Shchepin <alexey@process-one.net>
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

%%%% definitions

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

-define(INPUTATTRS(Type, Name, Value, Attrs),
	?XA("input", Attrs ++
	    [{"type", Type},
	     {"name", Name},
	     {"value", Value}])).

%%%==================================
%%%% get_acl_access

%% @spec (Path::[string()], Method) -> {HostOfRule, [AccessRule]}
%% where Method = 'GET' | 'POST'

%% All accounts can access those URLs
get_acl_rule([],_) -> {"localhost", [all]};
get_acl_rule(["style.css"],_) -> {"localhost", [all]};
get_acl_rule(["logo.png"],_) -> {"localhost", [all]};
get_acl_rule(["logo-fill.png"],_) -> {"localhost", [all]};
get_acl_rule(["favicon.ico"],_) -> {"localhost", [all]};
get_acl_rule(["additions.js"],_) -> {"localhost", [all]};
%% This page only displays vhosts that the user is admin:
get_acl_rule(["vhosts"],_) -> {"localhost", [all]};

%% The pages of a vhost are only accesible if the user is admin of that vhost:
get_acl_rule(["server", VHost | _RPath], Method)
    when Method=:='GET' orelse Method=:='HEAD' ->
    {VHost, [configure, webadmin_view]};
get_acl_rule(["server", VHost | _RPath], 'POST') -> {VHost, [configure]};

%% Default rule: only global admins can access any other random page
get_acl_rule(_RPath, Method)
    when Method=:='GET' orelse Method=:='HEAD' ->
    {global, [configure, webadmin_view]};
get_acl_rule(_RPath, 'POST') -> {global, [configure]}.

is_acl_match(Host, Rules, Jid) ->
    lists:any(
      fun(Rule) ->
	      allow == acl:match_rule(Host, Rule, Jid)
      end,
      Rules).

%%%==================================
%%%% Menu Items Access

get_jid(Auth, HostHTTP, Method) ->
    case get_auth_admin(Auth, HostHTTP, [], Method) of
        {ok, {User, Server}} ->
	    jlib:make_jid(User, Server, "");
	{unauthorized, Error} ->
	    ?ERROR_MSG("Unauthorized ~p: ~p", [Auth, Error]),
	    throw({unauthorized, Auth})
    end.

get_menu_items(global, cluster, Lang, JID) ->
    {Base, _, Items} = make_server_menu([], [], Lang, JID),
    lists:map(
	fun({URI, Name}) ->
		{Base++URI++"/", Name};
	   ({URI, Name, _SubMenu}) ->
		{Base++URI++"/", Name}
	end,
	Items
    );
get_menu_items(Host, cluster, Lang, JID) ->
    {Base, _, Items} = make_host_menu(Host, [], Lang, JID),
    lists:map(
	fun({URI, Name}) ->
		{Base++URI++"/", Name};
	   ({URI, Name, _SubMenu}) ->
		{Base++URI++"/", Name}
	end,
	Items
    ).
%% get_menu_items(Host, Node, Lang, JID) ->
%%     {Base, _, Items} = make_host_node_menu(Host, Node, Lang, JID),
%%     lists:map(
%% 	fun({URI, Name}) ->
%% 		{Base++URI++"/", Name};
%% 	   ({URI, Name, _SubMenu}) ->
%% 		{Base++URI++"/", Name}
%% 	end,
%% 	Items
%%     ).

is_allowed_path(BasePath, {Path, _}, JID) ->
    is_allowed_path(BasePath ++ [Path], JID);
is_allowed_path(BasePath, {Path, _, _}, JID) ->
    is_allowed_path(BasePath ++ [Path], JID).

is_allowed_path(["admin" | Path], JID) ->
    is_allowed_path(Path, JID);
is_allowed_path(Path, JID) ->
    {HostOfRule, AccessRule} = get_acl_rule(Path, 'GET'),
    is_acl_match(HostOfRule, AccessRule, JID).

%% @spec(Path) -> URL
%% where Path = [string()]
%%       URL = string()
%% Convert ["admin", "user", "tom"] -> "/admin/user/tom/"
%%path_to_url(Path) ->
%%    "/" ++ string:join(Path, "/") ++ "/".

%% @spec(URL) -> Path
%% where Path = [string()]
%%       URL = string()
%% Convert "admin/user/tom" -> ["admin", "user", "tom"]
url_to_path(URL) ->
    string:tokens(URL, "/").
  
%%%==================================
%%%% process/2

process(["doc", LocalFile], _Request) ->
    DocPath = case os:getenv("EJABBERD_DOC_PATH") of
		  P when is_list(P) -> P;
		  false -> "/share/doc/ejabberd/"
	      end,
    %% Code based in mod_http_fileserver
    FileName = filename:join(DocPath, LocalFile),
    case file:read_file(FileName) of
        {ok, FileContents} ->
            ?DEBUG("Delivering content.", []),
            {200,
             [{"Server", "ejabberd"}],
             FileContents};
        {error, Error} ->
            ?DEBUG("Delivering error: ~p", [Error]),
	    Help = " " ++ FileName ++ " - Try to specify the path to ejabberd documentation "
		"with the environment variable EJABBERD_DOC_PATH. Check the ejabberd Guide for more information.",
            case Error of
                eacces -> {403, [], "Forbidden"++Help};
                enoent -> {404, [], "Not found"++Help};
                _Else -> {404, [], atom_to_list(Error)++Help}
            end
    end;

process(["server", SHost | RPath] = Path, #request{auth = Auth, lang = Lang, host = HostHTTP, method = Method} = Request) ->
    Host = jlib:nameprep(SHost),
    case lists:member(Host, ?MYHOSTS) of
	true ->
	    case get_auth_admin(Auth, HostHTTP, Path, Method) of
		{ok, {User, Server}} ->
		    AJID = get_jid(Auth, HostHTTP, Method),
		    process_admin(Host, Request#request{path = RPath,
							auth = {auth_jid, Auth, AJID},
							us = {User, Server}});
		{unauthorized, "no-auth-provided"} ->
		    {401,
		     [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
		     ejabberd_web:make_xhtml([?XCT("h1", "Unauthorized")])};
		{unauthorized, Error} ->
		    {BadUser, _BadPass} = Auth,
		    {IPT, _Port} = Request#request.ip,
		    IPS = inet_parse:ntoa(IPT),
		    ?WARNING_MSG("Access of ~p from ~p failed with error: ~p",
				 [BadUser, IPS, Error]),
		    {401,
		     [{"WWW-Authenticate",
		       "basic realm=\"auth error, retry login to ejabberd\""}],
		     ejabberd_web:make_xhtml([?XCT("h1", "Unauthorized")])}
	    end;
	false ->
            ejabberd_web:error(not_found)
    end;

process(RPath, #request{auth = Auth, lang = Lang, host = HostHTTP, method = Method} = Request) ->
    case get_auth_admin(Auth, HostHTTP, RPath, Method) of
	{ok, {User, Server}} ->
	    AJID = get_jid(Auth, HostHTTP, Method),
	    process_admin(global, Request#request{path = RPath,
						  auth = {auth_jid, Auth, AJID},
						  us = {User, Server}});
        {unauthorized, "no-auth-provided"} ->
	    {401,
	     [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	     ejabberd_web:make_xhtml([?XCT("h1", "Unauthorized")])};
	{unauthorized, Error} ->
	    {BadUser, _BadPass} = Auth,
	    {IPT, _Port} = Request#request.ip,
	    IPS = inet_parse:ntoa(IPT),
	    ?WARNING_MSG("Access of ~p from ~p failed with error: ~p",
			 [BadUser, IPS, Error]),
	    {401,
	     [{"WWW-Authenticate",
	       "basic realm=\"auth error, retry login to ejabberd\""}],
	     ejabberd_web:make_xhtml([?XCT("h1", "Unauthorized")])}
    end.

get_auth_admin(Auth, HostHTTP, RPath, Method) ->
    case Auth of
        {SJID, Pass} ->
	    {HostOfRule, AccessRule} = get_acl_rule(RPath, Method),
            case jlib:string_to_jid(SJID) of
                error ->
                    {unauthorized, "badformed-jid"};
                #jid{user = "", server = User} ->
		    %% If the user only specified username, not username@server
		    get_auth_account(HostOfRule, AccessRule, User, HostHTTP, Pass);
                #jid{user = User, server = Server} ->
		    get_auth_account(HostOfRule, AccessRule, User, Server, Pass)
            end;
	undefined ->
            {unauthorized, "no-auth-provided"}
    end.

get_auth_account(HostOfRule, AccessRule, User, Server, Pass) ->
    case ejabberd_auth:check_password(User, Server, Pass) of
	true ->
	    case is_acl_match(HostOfRule, AccessRule,
				jlib:make_jid(User, Server, "")) of
		false ->
		    {unauthorized, "unprivileged-account"};
		true ->
		    {ok, {User, Server}}
	    end;
	false ->
	    case ejabberd_auth:is_user_exists(User, Server) of
		true ->
		    {unauthorized, "bad-password"};
		false ->
		    {unauthorized, "inexistent-account"}
	    end
    end.

%%%==================================
%%%% make_xhtml

make_xhtml(Els, Host, Lang, JID) ->
    make_xhtml(Els, Host, cluster, Lang, JID).

%% @spec (Els, Host, Node, Lang, JID) -> {200, [html], xmlelement()}
%% where Host = global | string()
%%       Node = cluster | atom()
%%       JID = jid()
make_xhtml(Els, Host, Node, Lang, JID) ->
    Base = get_base_path(Host, cluster), %% Enforcing 'cluster' on purpose here
    MenuItems = make_navigation(Host, Node, Lang, JID),
    {200, [html],
     {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"},
			   {"xml:lang", Lang},
			   {"lang", Lang}],
      [{xmlelement, "head", [],
	[?XCT("title", "ejabberd Web Admin"),
	 {xmlelement, "meta", [{"http-equiv", "Content-Type"},
			       {"content", "text/html; charset=utf-8"}], []},
	 {xmlelement, "script", [{"src", Base ++ "/additions.js"},
				 {"type", "text/javascript"}], [?C(" ")]},
	 {xmlelement, "link", [{"href", Base ++ "favicon.ico"},
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
			    [?ACT("/admin/", "ejabberd Web Admin")]
			   )]),
		  ?XAE("div",
		       [{"id", "navigation"}],
		       [?XE("ul",
			    MenuItems
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
			     "ejabberd (c) 2002-2012 ProcessOne")
		       ])])])
      ]}}.

get_base_path(global, cluster) -> "/admin/";
get_base_path(Host, cluster) -> "/admin/server/" ++ Host ++ "/";
get_base_path(global, Node) -> "/admin/node/" ++ atom_to_list(Node) ++ "/";
get_base_path(Host, Node) -> "/admin/server/" ++ Host ++ "/node/" ++ atom_to_list(Node) ++ "/".

%%%==================================
%%%% css & images

additions_js() ->
"
function selectAll() {
  for(i=0;i<document.forms[0].elements.length;i++)
  { var e = document.forms[0].elements[i];
    if(e.type == 'checkbox')
    { e.checked = true; }
  }
}
function unSelectAll() {
  for(i=0;i<document.forms[0].elements.length;i++)
  { var e = document.forms[0].elements[i];
    if(e.type == 'checkbox')
    { e.checked = false; }
  }
}
".

css(Host) ->
    Base = get_base_path(Host, cluster),
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
  top: 65px;
  left: 0;
  padding: 0 1px 1px 1px;
  margin: 0;
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 8pt;
  font-weight: bold;
  border-top: 1px solid #d47911;
  width: 17em;
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
  border-right: 1px solid #d47911;
  border-bottom: 1px solid #d47911;
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

ul li #navhead a, ul li #navheadsub a, ul li #navheadsubsub a {
  text-align: center;
  border-top: 1px solid #d47911;
  border-bottom: 2px solid #d47911;
  background: #FED6A6;
}

#navheadsub, #navitemsub {
  border-left: 7px solid white;
  margin-left: 2px;
}

#navheadsubsub, #navitemsubsub {
  border-left: 14px solid white;
  margin-left: 4px;
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

#content ul.nolistyle>li {
  list-style-type: none;
}

#content li.big {
  font-size: 10pt;
}

#content {
  font-family: Verdana, Arial, Helvetica, sans-serif; 
  font-size: 10pt;
  padding-left: 17em;
  padding-top: 5px;
}

div.guidelink {
  text-align: right;
  padding-right: 1em;
}

table.withtextareas>tbody>tr>td {
  vertical-align: top;
}

p.result {
  border: 1px;
  border-style: dashed;
  border-color: #FE8A02;
  padding: 1em;
  margin-right: 1em;
  background: #FFE3C9;
}

*.alignright {
  font-size: 10pt;
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
      "iVBORw0KGgoAAAANSUhEUgAAAVcAAAA3CAMAAACPbPnEAAAAAXNSR0IArs4c"
      "6QAAAEtQTFRFcTIA1XcE/YsA/40E/pIH/JYc/5kg/54i/KIu/6U6/apE/61H"
      "/61P/bFX/7Vh/bda/rpq/L5s/8J2/cJ8/8qI/86Y/9aj/9mt/+bJ7EGiPwAA"
      "AZRJREFUeNrt28lug0AQhGHajrPv+/s/aVwpDlgE0gQ3tqO/DhxihMg33VJ7"
      "JmmCVKSJlVJ4bZQ93Jl/zjJv+8tzcMUVV1xxLXIlRfPAZptYrbf5YeW618PW"
      "yvG8w/g9ZwquuJ6Y6+bbdY0rrifhSmrmgUulVXbVDq3H39Zy6Cf9+8c7JNM/"
      "mXeY8+SMRmuIK6644oprkSupmQdulLhQdup1qJKmrmWmVpb5NN9LUyddu7nn"
      "LYkrrrjiimuVK6mZB+6VuFbiXJk8v/bnv0PVa+Yd5tdr/x7vCfqbgPsfV1xx"
      "xRXXKldSMw+8KPGgxJWyU7WZE538p0vOr/lOm/q7dPf+bOVKvVXiUcEVV1xx"
      "xbXMldTMA29KPCtxp7T6XpvxE6/9nm/l987mnG9l5u/8jO4Ot9uTEq8Krrji"
      "iiuuZa6kZh74UFpli3sO61btMfyHyWGv/RMs7wB67ne32/BdwRVXXHHFtcyV"
      "1MwDn0qrbHHvyPT/Dsarla/R/1GpQydYPhf0bqC/A7jz7YkrrrjiimuVK6nI"
      "F5dWoNvcLcs/AAAAAElFTkSuQmCC").

logo_fill() ->
    jlib:decode_base64(
      "iVBORw0KGgoAAAANSUhEUgAAAAYAAAA3BAMAAADdxCZzAAAAAXNSR0IArs4c"
      "6QAAAB5QTFRF1nYO/ooC/o4O/pIS/p4q/q5K/rpq/sqM/tam/ubGzn/S/AAA"
      "AEFJREFUCNdlw0sRwCAQBUE+gSRHLGABC1jAAhbWAhZwC+88XdXOXb4UlFAr"
      "SmwN5ekdJY2BkudEec1QvrVQ/r3xOlK9HsTvertmAAAAAElFTkSuQmCC").

%%%==================================
%%%% process_admin

process_admin(global,
	      #request{path = [],
		       auth = {_, _, AJID},
		       lang = Lang}) ->
    %%Base = get_base_path(global, cluster),
    make_xhtml(?H1GL(?T("Administration"), "toc", "Contents") ++
		[?XE("ul",
		    [?LI([?ACT(MIU, MIN)]) || {MIU, MIN} <- get_menu_items(global, cluster, Lang, AJID)]
		   )
	       ], global, Lang, AJID);

process_admin(Host,
	      #request{path = [],
		       auth = {_, _Auth, AJID},
		       lang = Lang}) ->
    %%Base = get_base_path(Host, cluster),
    make_xhtml([?XCT("h1", "Administration"),
		?XE("ul",
		    [?LI([?ACT(MIU, MIN)]) || {MIU, MIN} <- get_menu_items(Host, cluster, Lang, AJID)]
		   )
	       ], Host, Lang, AJID);

process_admin(Host, #request{path = ["style.css"]}) ->
    {200, [{"Content-Type", "text/css"}, last_modified(), cache_control_public()], css(Host)};

process_admin(_Host, #request{path = ["favicon.ico"]}) ->
    {200, [{"Content-Type", "image/x-icon"}, last_modified(), cache_control_public()], favicon()};

process_admin(_Host, #request{path = ["logo.png"]}) ->
    {200, [{"Content-Type", "image/png"}, last_modified(), cache_control_public()], logo()};

process_admin(_Host, #request{path = ["logo-fill.png"]}) ->
    {200, [{"Content-Type", "image/png"}, last_modified(), cache_control_public()], logo_fill()};

process_admin(_Host, #request{path = ["additions.js"]}) ->
    {200, [{"Content-Type", "text/javascript"}, last_modified(), cache_control_public()], additions_js()};

process_admin(Host,
	      #request{path = ["acls-raw"],
		       q = Query,
		       auth = {_, _Auth, AJID},
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
    ACLs = lists:keysort(2, ets:select(acl, [{{acl, {'$1', Host}, '$2'},
					      [], [{{acl, '$1', '$2'}}]}])),
    {NumLines, ACLsP} = term_to_paragraph(ACLs, 80),
    make_xhtml(?H1GL(?T("Access Control Lists"), "ACLDefinition", "ACL Definition") ++
	       case Res of
		   ok -> [?XREST("Submitted")];
		   error -> [?XREST("Bad format")];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [?TEXTAREA("acls", integer_to_list(lists:max([16, NumLines])), "80", ACLsP++"."),
		      ?BR,
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Host, Lang, AJID);

process_admin(Host,
	      #request{method = Method,
			path = ["acls"],
		        auth = {_, _Auth, AJID},
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
    make_xhtml(?H1GL(?T("Access Control Lists"), "ACLDefinition", "ACL Definition") ++
	       case Res of
		   ok -> [?XREST("Submitted")];
		   error -> [?XREST("Bad format")];
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
	       ], Host, Lang, AJID);

process_admin(Host,
	      #request{path = ["access-raw"],
		       auth = {_, _Auth, AJID},
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
	    ets:select(config,
			       [{{config, {access, '$1', Host}, '$2'},
				 [],
				 [{{access, '$1', '$2'}}]}]),
    {NumLines, AccessP} = term_to_paragraph(Access, 80),
    make_xhtml(?H1GL(?T("Access Rules"), "AccessRights", "Access Rights") ++
	       case Res of
		   ok -> [?XREST("Submitted")];
		   error -> [?XREST("Bad format")];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [?TEXTAREA("access", integer_to_list(lists:max([16, NumLines])), "80", AccessP++"."),
		      ?BR,
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Host, Lang, AJID);

process_admin(Host,
	      #request{method = Method,
		       path = ["access"],
		       q = Query,
		       auth = {_, _Auth, AJID},
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
    make_xhtml(?H1GL(?T("Access Rules"), "AccessRights", "Access Rights") ++
	       case Res of
		   ok -> [?XREST("Submitted")];
		   error -> [?XREST("Bad format")];
		   nothing -> []
	       end ++
	       [?XE("p", [?ACT("../access-raw/", "Raw")])] ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [access_rules_to_xhtml(AccessRules, Lang),
		      ?BR,
		      ?INPUTT("submit", "delete", "Delete Selected")
		     ])
	       ], Host, Lang, AJID);

process_admin(Host,
	      #request{path = ["access", SName],
		       q = Query,
		       auth = {_, _Auth, AJID},
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
		   ok -> [?XREST("Submitted")];
		   error -> [?XREST("Bad format")];
		   nothing -> []
	       end ++
	       [?XAE("form", [{"action", ""}, {"method", "post"}],
		     [access_rule_to_xhtml(Rules),
		      ?BR,
		      ?INPUTT("submit", "submit", "Submit")
		     ])
	       ], Host, Lang, AJID);

process_admin(global,
	      #request{path = ["vhosts"],
		       auth = {_, _Auth, AJID},
		       lang = Lang}) ->
    Res = list_vhosts(Lang, AJID),
    make_xhtml(?H1GL(?T("Virtual Hosts"), "virtualhost", "Virtual Hosting") ++ Res, global, Lang, AJID);

process_admin(Host,
	      #request{path = ["users"],
		       q = Query,
		       auth = {_, _Auth, AJID},
		       lang = Lang}) when is_list(Host) ->
    Res = list_users(Host, Query, Lang, fun url_func/1),
    make_xhtml([?XCT("h1", "Users")] ++ Res, Host, Lang, AJID);

process_admin(Host,
	      #request{path = ["users", Diap],
		       auth = {_, _Auth, AJID},
		       lang = Lang}) when is_list(Host) ->
    Res = list_users_in_diapason(Host, Diap, Lang, fun url_func/1),
    make_xhtml([?XCT("h1", "Users")] ++ Res, Host, Lang, AJID);

process_admin(Host,
	      #request{
		       path = ["online-users"],
		       auth = {_, _Auth, AJID},
		       lang = Lang}) when is_list(Host) ->
    Res = list_online_users(Host, Lang),
    make_xhtml([?XCT("h1", "Online Users")] ++ Res, Host, Lang, AJID);

process_admin(Host,
	      #request{path = ["last-activity"],
		       auth = {_, _Auth, AJID},
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
	       Res, Host, Lang, AJID);

process_admin(Host,
	      #request{path = ["stats"],
		       auth = {_, _Auth, AJID},
		       lang = Lang}) ->
    Res = get_stats(Host, Lang),
    make_xhtml([?XCT("h1", "Statistics")] ++ Res, Host, Lang, AJID);

process_admin(Host,
	      #request{path = ["user", U],
		       auth = {_, _Auth, AJID},
		       q = Query,
		       lang = Lang}) ->
    case ejabberd_auth:is_user_exists(U, Host) of
	true ->
	    Res = user_info(U, Host, Query, Lang),
	    make_xhtml(Res, Host, Lang, AJID);
	false ->
	    make_xhtml([?XCT("h1", "Not Found")], Host, Lang, AJID)
    end;

process_admin(Host,
	      #request{path = ["nodes"],
		       auth = {_, _Auth, AJID},
		       lang = Lang}) ->
    Res = get_nodes(Lang),
    make_xhtml(Res, Host, Lang, AJID);

process_admin(Host,
	      #request{path = ["node", SNode | NPath],
		       auth = {_, _Auth, AJID},
		       q = Query,
		       lang = Lang}) ->
    case search_running_node(SNode) of
	false ->
	    make_xhtml([?XCT("h1", "Node not found")], Host, Lang, AJID);
	Node ->
	    Res = get_node(Host, Node, NPath, Query, Lang),
	    make_xhtml(Res, Host, Node, Lang, AJID)
    end;

%%%==================================
%%%% process_admin default case

process_admin(Host, #request{lang = Lang, 
		       auth = {_, _Auth, AJID}
		    } = Request) ->
    {Hook, Opts} = case Host of
		       global -> {webadmin_page_main, [Request]};
		       Host -> {webadmin_page_host, [Host, Request]}
		   end,
    case ejabberd_hooks:run_fold(Hook, Host, [], Opts) of
	[] -> setelement(1, make_xhtml([?XC("h1", "Not Found")], Host, Lang, AJID), 404);
	Res -> make_xhtml(Res, Host, Lang, AJID)
    end.

%%%==================================
%%%% acl

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


%% @spec (T::any()) -> StringLine::string()
term_to_string(T) ->
    StringParagraph = lists:flatten(io_lib:format("~1000000p", [T])),
    %% Remove from the string all the carriage returns characters
    ejabberd_regexp:greplace(StringParagraph, "\\n ", "").

%% @spec (T::any(), Cols::integer()) -> {NumLines::integer(), Paragraph::string()}
term_to_paragraph(T, Cols) ->
    Paragraph = erl_prettypr:format(erl_syntax:abstract(T), [{paper, Cols}]),
    FieldList = ejabberd_regexp:split(Paragraph, "\n"),
    NumLines = length(FieldList),
    {NumLines, Paragraph}.

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
		     SAccess ++ "\s\t" ++ SACL ++ "\n"
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

%%%==================================
%%%% list_vhosts

list_vhosts(Lang, JID) ->
    Hosts = ?MYHOSTS,
    HostsAllowed = lists:filter(
		     fun(Host) ->
			     is_acl_match(Host, [configure, webadmin_view], JID)
		     end,
		     Hosts
		    ),
    list_vhosts2(Lang, HostsAllowed).

list_vhosts2(Lang, Hosts) ->
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
			     ?XC("td", pretty_string_int(RegisteredUsers)),
			     ?XC("td", pretty_string_int(OnlineUsers))
			    ])
		end, SHosts)
	     )])].

%%%==================================
%%%% list_users

list_users(Host, Query, Lang, URLFunc) ->
    Res = list_users_parse_query(Query, Host),
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
    FUsers =
	case length(SUsers) of
	    N when N =< 100 ->
		[list_given_users(Host, SUsers, "../", Lang, URLFunc)];
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
	ok -> [?XREST("Submitted")];
	error -> [?XREST("Bad format")];
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
    [S1, S2] = ejabberd_regexp:split(Diap, "-"),
    N1 = list_to_integer(S1),
    N2 = list_to_integer(S2),
    Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
    [list_given_users(Host, Sub, "../../", Lang, URLFunc)].

list_given_users(Host, Users, Prefix, Lang, URLFunc) ->
    ModOffline = get_offlinemsg_module(Host),
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
		       QueueLenStr = get_offlinemsg_length(ModOffline, User, Server),
		       FQueueLen = [?AC(URLFunc({users_queue, Prefix,
						 User, Server}),
					QueueLenStr)],
		       FLast =
			   case ejabberd_sm:get_user_resources(User, Server) of
			       [] ->
				   case mod_last:get_last_info(User, Server) of
				       not_found ->
					   ?T("Never");
				       {ok, Shift, _Status} ->
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

get_offlinemsg_length(ModOffline, User, Server) ->
    case ModOffline of
	none -> "disabled";
	_ -> pretty_string_int(ModOffline:get_queue_length(User, Server))
    end.

get_offlinemsg_module(Server) ->
    case gen_mod:is_loaded(Server, mod_offline) of
        true ->
            mod_offline;
        false ->
            none
    end.

get_lastactivity_menuitem_list(Server) ->
    case gen_mod:db_type(Server, mod_last) of
        mnesia -> [{"last-activity", "Last Activity"}];
        _ -> []
    end.

us_to_list({User, Server}) ->
    jlib:jid_to_string({User, Server, ""}).

su_to_list({Server, User}) ->
    jlib:jid_to_string({User, Server, ""}).

%%%==================================
%%%% get_stats

get_stats(global, Lang) ->
    OnlineUsers = mnesia:table_info(session, size),
    RegisteredUsers = lists:foldl(
	fun(Host, Total) ->
	    ejabberd_auth:get_vh_registered_users_number(Host) + Total
	end, 0, ejabberd_config:get_global_option(hosts)),
    S2SConns = ejabberd_s2s:dirty_get_connections(),
    S2SConnections = length(S2SConns),
    S2SServers = length(lists:usort([element(2, C) || C <- S2SConns])),
    [?XAE("table", [],
	  [?XE("tbody",
	       [?XE("tr", [?XCT("td", "Registered Users:"),
			   ?XC("td", pretty_string_int(RegisteredUsers))]),
		?XE("tr", [?XCT("td", "Online Users:"),
			   ?XC("td", pretty_string_int(OnlineUsers))]),
		?XE("tr", [?XCT("td", "Outgoing s2s Connections:"),
			   ?XC("td", pretty_string_int(S2SConnections))]),
		?XE("tr", [?XCT("td", "Outgoing s2s Servers:"),
			   ?XC("td", pretty_string_int(S2SServers))])
	       ])
	  ])];

get_stats(Host, Lang) ->
    OnlineUsers = length(ejabberd_sm:get_vh_session_list(Host)),
    RegisteredUsers = ejabberd_auth:get_vh_registered_users_number(Host),
    [?XAE("table", [],
	  [?XE("tbody",
	       [?XE("tr", [?XCT("td", "Registered Users:"),
			   ?XC("td", pretty_string_int(RegisteredUsers))]),
		?XE("tr", [?XCT("td", "Online Users:"),
			   ?XC("td", pretty_string_int(OnlineUsers))])
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
				       FIP = case ejabberd_sm:get_user_info(
						    User, Server, R) of
						 offline ->
						     "";
						 Info when is_list(Info) ->
						     Node = proplists:get_value(node, Info),
						     Conn = proplists:get_value(conn, Info),
						     {IP, Port} = proplists:get_value(ip, Info),
						     ConnS = case Conn of
								 c2s -> "plain";
								 c2s_tls -> "tls";
								 c2s_compressed -> "zlib";
								 c2s_compressed_tls -> "tls+zlib";
								 http_bind -> "http-bind";
								 http_poll -> "http-poll"
							     end,
						     " (" ++
						         ConnS ++ "://" ++
							 inet_parse:ntoa(IP) ++
							 ":" ++
							 integer_to_list(Port)
						         ++ "#" ++ atom_to_list(Node)
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
    %% Code copied from list_given_users/5:
    LastActivity = case ejabberd_sm:get_user_resources(User, Server) of
		       [] ->
			   case mod_last:get_last_info(User, Server) of
			       not_found ->
				   ?T("Never");
			       {ok, Shift, _Status} ->
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
    [?XC("h1", ?T("User ") ++ us_to_list(US))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [?XCT("h3", "Connected Resources:")] ++ FResources ++
	      [?XCT("h3", "Password:")] ++ FPassword ++
	      [?XCT("h3", "Last Activity")] ++ [?C(LastActivity)] ++
	      UserItems ++
	      [?P, ?INPUTT("submit", "removeuser", "Remove User")])].


user_parse_query(User, Server, Query) ->
    lists:foldl(fun({Action, _Value}, Acc) when Acc == nothing ->
                      user_parse_query1(Action, User, Server, Query);
                   ({_Action, _Value}, Acc) ->
                      Acc
                end, nothing, Query).

user_parse_query1("password", _User, _Server, _Query) ->
    nothing;
user_parse_query1("chpassword", User, Server, Query) ->
    case lists:keysearch("password", 1, Query) of
        {value, {_, undefined}} ->
            error;
        {value, {_, Password}} ->
            ejabberd_auth:set_password(User, Server, Password),
            ok;
        _ ->
            error
    end;
user_parse_query1("removeuser", User, Server, _Query) ->
    ejabberd_auth:remove_user(User, Server),
    ok;
user_parse_query1(Action, User, Server, Query) ->
    case ejabberd_hooks:run_fold(webadmin_user_parse_query, Server, [], [Action, User, Server, Query]) of
         [] -> nothing;
         Res -> Res
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
				[{xmlcdata, pretty_string_int(V)}])
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

%%%==================================
%%%% get_nodes

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
    Base = get_base_path(global, Node),
    MenuItems2 = make_menu_items(global, Node, Base, Lang),
    [?XC("h1", ?T("Node ") ++ atom_to_list(Node))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
	    nothing -> []
	end ++
	[?XE("ul",
	     [?LI([?ACT(Base ++ "db/", "Database")]),
	      ?LI([?ACT(Base ++ "backup/", "Backup")]),
	      ?LI([?ACT(Base ++ "ports/", "Listened Ports")]),
	      ?LI([?ACT(Base ++ "stats/", "Statistics")]),
	      ?LI([?ACT(Base ++ "update/", "Update")])
	     ] ++ MenuItems2),
	 ?XAE("form", [{"action", ""}, {"method", "post"}],
	      [?INPUTT("submit", "restart", "Restart"),
	       ?C(" "),
	       ?INPUTT("submit", "stop", "Stop")])
	];

get_node(Host, Node, [], _Query, Lang) ->
    Base = get_base_path(Host, Node),
    MenuItems2 = make_menu_items(Host, Node, Base, Lang),
    [?XC("h1", ?T("Node ") ++ atom_to_list(Node)),
     ?XE("ul",
	 [?LI([?ACT(Base ++ "modules/", "Modules")])] ++ MenuItems2)
    ];

get_node(global, Node, ["db"], Query, Lang) ->
    case rpc:call(Node, mnesia, system_info, [tables]) of
	{badrpc, _Reason} ->
	    [?XCT("h1", "RPC Call Error")];
	Tables ->
	    ResS = case node_db_parse_query(Node, Tables, Query) of
		       nothing -> [];
		       ok -> [?XREST("Submitted")]
		   end,
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
					pretty_string_int(Size)),
				  ?XAC("td", [{"class", "alignright"}],
					pretty_string_int(Memory))
				 ])
		     end, STables),
	    [?XC("h1", ?T("Database Tables at ") ++ atom_to_list(Node))] ++
		ResS ++
		[?XAE("form", [{"action", ""}, {"method", "post"}],
		      [?XAE("table", [],
			    [?XE("thead",
				 [?XE("tr",
				      [?XCT("td", "Name"),
				       ?XCT("td", "Storage Type"),
				       ?XCT("td", "Elements"), %% Elements/items/records inserted in the table
				       ?XCT("td", "Memory") %% Words or Bytes allocated to the table on this node
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
    HomeDirRaw = case {os:getenv("HOME"), os:type()} of
	{EnvHome, _} when is_list(EnvHome) -> EnvHome;
	{false, {win32, _Osname}} -> "C:/";
	{false, _} -> "/tmp/"
    end,
    HomeDir = filename:nativename(HomeDirRaw),
    ResS = case node_backup_parse_query(Node, Query) of
	       nothing -> [];
	       ok -> [?XREST("Submitted")];
	       {error, Error} -> [?XRES(?T("Error") ++": " ++ io_lib:format("~p", [Error]))]
	   end,
    ?H1GL(?T("Backup of ") ++ atom_to_list(Node), "list-eja-commands", "List of ejabberd Commands") ++
	ResS ++
	[?XCT("p", "Please note that these options will only backup the builtin Mnesia database. If you are using the ODBC module, you also need to backup your SQL database separately."),
     ?XAE("form", [{"action", ""}, {"method", "post"}],
	  [?XAE("table", [],
		[?XE("tbody",
		     [?XE("tr",
 			  [?XCT("td", "Store binary backup:"),
			   ?XE("td", [?INPUT("text", "storepath",
 					     filename:join(HomeDir, "ejabberd.backup"))]),
			   ?XE("td", [?INPUTT("submit", "store",
					      "OK")])
			  ]),
		      ?XE("tr",
 			  [?XCT("td", "Restore binary backup immediately:"),
			   ?XE("td", [?INPUT("text", "restorepath",
 					     filename:join(HomeDir, "ejabberd.backup"))]),
			   ?XE("td", [?INPUTT("submit", "restore",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td",
				"Restore binary backup after next ejabberd restart (requires less memory):"),
			   ?XE("td", [?INPUT("text", "fallbackpath",
 					     filename:join(HomeDir, "ejabberd.backup"))]),
			   ?XE("td", [?INPUTT("submit", "fallback",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td", "Store plain text backup:"),
			   ?XE("td", [?INPUT("text", "dumppath",
 					     filename:join(HomeDir, "ejabberd.dump"))]),
			   ?XE("td", [?INPUTT("submit", "dump",
					      "OK")])
			  ]),
		      ?XE("tr",
			  [?XCT("td", "Restore plain text backup immediately:"),
			   ?XE("td", [?INPUT("text", "loadpath",
 					     filename:join(HomeDir, "ejabberd.dump"))]),
			   ?XE("td", [?INPUTT("submit", "load",
					      "OK")])
 			  ]),
 		      ?XE("tr",
 			  [?XCT("td", "Import users data from a PIEFXIS file (XEP-0227):"),
 			   ?XE("td", [?INPUT("text", "import_piefxis_filepath",
 					     filename:join(HomeDir, "users.xml"))]),
 			   ?XE("td", [?INPUTT("submit", "import_piefxis_file",
 					      "OK")])
 			  ]),
 		      ?XE("tr",
 			  [?XCT("td", "Export data of all users in the server to PIEFXIS files (XEP-0227):"),
 			   ?XE("td", [?INPUT("text", "export_piefxis_dirpath",
 					     HomeDir)]),
 			   ?XE("td", [?INPUTT("submit", "export_piefxis_dir",
 					      "OK")])
 			  ]),
 		      ?XE("tr",
 			  [?XE("td", [?CT("Export data of users in a host to PIEFXIS files (XEP-0227):"),
 			              ?C(" "),
 			              ?INPUT("text", "export_piefxis_host_dirhost", ?MYNAME)]),
 			   ?XE("td", [?INPUT("text", "export_piefxis_host_dirpath", HomeDir)]),
 			   ?XE("td", [?INPUTT("submit", "export_piefxis_host_dir",
 					      "OK")])
 			  ]),
 		      ?XE("tr",
 			  [?XCT("td", "Import user data from jabberd14 spool file:"),
 			   ?XE("td", [?INPUT("text", "import_filepath",
 					     filename:join(HomeDir, "user1.xml"))]),
 			   ?XE("td", [?INPUTT("submit", "import_file",
 					      "OK")])
 			  ]),
 		      ?XE("tr",
 			  [?XCT("td", "Import users data from jabberd14 spool directory:"),
 			   ?XE("td", [?INPUT("text", "import_dirpath",
 					     "/var/spool/jabber/")]),
 			   ?XE("td", [?INPUTT("submit", "import_dir",
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
	      {is_added, ok} ->
		  ok;
	      {is_added, {error, Reason}} ->
		  {error, io_lib:format("~p", [Reason])};
	      _ ->
		  nothing
	  end,
    %% TODO: This sorting does not work when [{{Port, IP}, Module, Opts}]
    NewPorts = lists:sort(
		 rpc:call(Node, ejabberd_config, get_local_option, [listen])),
    H1String = ?T("Listened Ports at ") ++ atom_to_list(Node),
    ?H1GL(H1String, "listened", "Listening Ports") ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
	    {error, ReasonT} -> [?XRES(?T("Error") ++ ": " ++ ReasonT)];
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
    H1String = ?T("Modules at ") ++ atom_to_list(Node),
    ?H1GL(H1String, "modoverview", "Modules Overview") ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
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
    TransactionsCommitted =
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
				pretty_string_int(OnlineUsers))]),
		?XE("tr", [?XCT("td", "Transactions Committed:"),
			   ?XAC("td", [{"class", "alignright"}],
				pretty_string_int(TransactionsCommitted))]),
		?XE("tr", [?XCT("td", "Transactions Aborted:"),
			   ?XAC("td", [{"class", "alignright"}],
				pretty_string_int(TransactionsAborted))]),
		?XE("tr", [?XCT("td", "Transactions Restarted:"),
			   ?XAC("td", [{"class", "alignright"}],
				pretty_string_int(TransactionsRestarted))]),
		?XE("tr", [?XCT("td", "Transactions Logged:"),
			   ?XAC("td", [{"class", "alignright"}],
				pretty_string_int(TransactionsLogged))])
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
		BeamsLis =
		    lists:map(
		      fun(Beam) ->
			      BeamString = atom_to_list(Beam),
			      ?LI([
				   ?INPUT("checkbox", "selected", BeamString),
				   %%?XA("input", [{"checked", ""}, %% Selected by default
					%%	 {"type", "checkbox"},
					%%	 {"name", "selected"},
					%%	 {"value", BeamString}]),
				   ?C(BeamString)])
		      end,
		      UpdatedBeams),
		SelectButtons =
		    [?BR,
		     ?INPUTATTRS("button", "selectall", "Select All",
				 [{"onClick", "selectAll()"}]),
		     ?C(" "),
		     ?INPUTATTRS("button", "unselectall", "Unselect All",
				 [{"onClick", "unSelectAll()"}])],
		%%?XE("ul", BeamsLis)
		?XAE("ul", [{"class", "nolistyle"}], BeamsLis ++ SelectButtons)
	end,
    FmtScript = ?XC("pre", io_lib:format("~p", [Script])),
    FmtLowLevelScript = ?XC("pre", io_lib:format("~p", [LowLevelScript])),
    [?XC("h1", ?T("Update ") ++ atom_to_list(Node))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    {error, ErrorText} -> [?XREST("Error: " ++ ErrorText)];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [
	       ?XCT("h2", "Update plan"),
	       ?XCT("h3", "Modified modules"), Mods,
	       ?XCT("h3", "Update script"), FmtScript,
	       ?XCT("h3", "Low level update script"), FmtLowLevelScript,
	       ?XCT("h3", "Script check"), ?XC("pre", atom_to_list(Check)),
	       ?BR,
	       ?INPUTT("submit", "update", "Update")
	      ])
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

%%%==================================
%%%% node parse

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
		 {unknown, "Remote copy"},
		 {delete_content, "Delete content"},
		 {delete_table, "Delete table"}])).

node_db_parse_query(_Node, _Tables, [{nokey,[]}]) ->
    nothing;
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
				 "delete_content" -> delete_content;
				 "delete_table" -> delete_table;
				 _ -> false
			     end,
		      if
			  Type == false ->
			      ok;
			  Type == delete_content ->
			      mnesia:clear_table(Table);
			  Type == delete_table ->
			      mnesia:delete_table(Table);
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

node_backup_parse_query(_Node, [{nokey,[]}]) ->
    nothing;
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
					  rpc:call(Node, ejabberd_admin,
						   dump_to_textfile, [Path]);
				      "load" ->
					  rpc:call(Node, mnesia,
						   load_textfile, [Path]);
				      "import_piefxis_file" ->
					  rpc:call(Node, ejabberd_piefxis,
						   import_file, [Path]);
				      "export_piefxis_dir" ->
					  rpc:call(Node, ejabberd_piefxis,
						   export_server, [Path]);
				      "export_piefxis_host_dir" ->
					  {value, {_, Host}} = lists:keysearch(Action ++ "host", 1, Query),
					  rpc:call(Node, ejabberd_piefxis,
						   export_host, [Path, Host]);
				      "import_file" ->
					  rpc:call(Node, ejabberd_admin,
						   import_file, [Path]);
				      "import_dir" ->
					  rpc:call(Node, ejabberd_admin,
						   import_dir, [Path])
				  end,
			      case Res of
				  {error, Reason} ->
				      {error, Reason};
				  {badrpc, Reason} ->
				      {badrpc, Reason};
				  _ ->
				      ok
			      end;
			  OtherError ->
			      {error, OtherError}
		      end;
		  _ ->
		      nothing
	      end;
	 (_Action, Res) ->
	      Res
      end, nothing, ["store", "restore", "fallback", "dump", "load", "import_file", "import_dir",
	     "import_piefxis_file", "export_piefxis_dir", "export_piefxis_host_dir"]).

node_ports_to_xhtml(Ports, Lang) ->
    ?XAE("table", [{"class", "withtextareas"}],
	 [?XE("thead",
	      [?XE("tr",
		   [?XCT("td", "Port"),
		    ?XCT("td", "IP"),
		    ?XCT("td", "Protocol"),
		    ?XCT("td", "Module"),
		    ?XCT("td", "Options")
		   ])]),
	  ?XE("tbody",
	      lists:map(
		fun({PortIP, Module, Opts} = _E) ->
			{_Port, SPort, _TIP, SIP, SSPort, NetProt, OptsClean} =
			    get_port_data(PortIP, Opts),
			SModule = atom_to_list(Module),
			{NumLines, SOptsClean} = term_to_paragraph(OptsClean, 40),
			%%ID = term_to_id(E),
			?XE("tr",
			    [?XAE("td", [{"size", "6"}], [?C(SPort)]),
			     ?XAE("td", [{"size", "15"}], [?C(SIP)]),
			     ?XAE("td", [{"size", "4"}], [?C(atom_to_list(NetProt))]),
			     ?XE("td", [?INPUTS("text", "module" ++ SSPort,
						SModule, "15")]),
			     ?XE("td", [?TEXTAREA("opts" ++ SSPort, integer_to_list(NumLines), "35", SOptsClean)]),
			     ?XE("td", [?INPUTT("submit", "add" ++ SSPort,
						"Update")]),
			     ?XE("td", [?INPUTT("submit", "delete" ++ SSPort,
						"Delete")])
			    ]
			   )
		end, Ports) ++
	      [?XE("tr",
		   [?XE("td", [?INPUTS("text", "portnew", "", "6")]),
		    ?XE("td", [?INPUTS("text", "ipnew", "0.0.0.0", "15")]),
		    ?XE("td", [make_netprot_html("tcp")]),
		    ?XE("td", [?INPUTS("text", "modulenew", "", "15")]),
		    ?XE("td", [?TEXTAREA("optsnew", "2", "35", "[]")]),
		    ?XAE("td", [{"colspan", "2"}],
			 [?INPUTT("submit", "addnew", "Add New")])
		   ]
		  )]
	     )]).

make_netprot_html(NetProt) ->
    ?XAE("select", [{"name", "netprotnew"}],
	 lists:map(
	   fun(O) ->
		   Sel = if
			     O == NetProt -> [{"selected", "selected"}];
			     true -> []
			 end,
		   ?XAC("option",
			Sel ++ [{"value", O}],
			O)
	   end, ["tcp", "udp"])).

get_port_data(PortIP, Opts) ->
    {Port, IPT, IPS, _IPV, NetProt, OptsClean} = ejabberd_listener:parse_listener_portip(PortIP, Opts),
    SPort = io_lib:format("~p", [Port]),

    SSPort = lists:flatten(
	       lists:map(
		 fun(N) -> io_lib:format("~.16b", [N]) end,
		 binary_to_list(crypto:md5(SPort++IPS++atom_to_list(NetProt))))),
    {Port, SPort, IPT, IPS, SSPort, NetProt, OptsClean}.
  

node_ports_parse_query(Node, Ports, Query) ->
    lists:foreach(
      fun({PortIpNetp, Module1, Opts1}) ->
	      {Port, _SPort, TIP, _SIP, SSPort, NetProt, _OptsClean} =
		  get_port_data(PortIpNetp, Opts1),
	      case lists:keysearch("add" ++ SSPort, 1, Query) of
		  {value, _} ->
		      PortIpNetp2 = {Port, TIP, NetProt},
		      {{value, {_, SModule}}, {value, {_, SOpts}}} =
			  {lists:keysearch("module" ++ SSPort, 1, Query),
			   lists:keysearch("opts" ++ SSPort, 1, Query)},
		      Module = list_to_atom(SModule),
		      {ok, Tokens, _} = erl_scan:string(SOpts ++ "."),
		      {ok, Opts} = erl_parse:parse_term(Tokens),
		      rpc:call(Node, ejabberd_listener, delete_listener,
			       [PortIpNetp2, Module1]),
		      R=rpc:call(Node, ejabberd_listener, add_listener,
				 [PortIpNetp2, Module, Opts]),
		      throw({is_added, R});
		  _ ->
		      case lists:keysearch("delete" ++ SSPort, 1, Query) of
			  {value, _} ->
			      rpc:call(Node, ejabberd_listener, delete_listener,
				       [PortIpNetp, Module1]),
			      throw(submitted);
			  _ ->
			      ok
		      end
	      end
      end, Ports),
    case lists:keysearch("addnew", 1, Query) of
	{value, _} ->
	    {{value, {_, SPort}},
	     {value, {_, STIP}}, %% It is a string that may represent a tuple
	     {value, {_, SNetProt}},
	     {value, {_, SModule}},
	     {value, {_, SOpts}}} =
		{lists:keysearch("portnew", 1, Query),
		 lists:keysearch("ipnew", 1, Query),
		 lists:keysearch("netprotnew", 1, Query),
		 lists:keysearch("modulenew", 1, Query),
		 lists:keysearch("optsnew", 1, Query)},
	    {ok, Toks, _} = erl_scan:string(SPort ++ "."),
	    {ok, Port2} = erl_parse:parse_term(Toks),
	    {ok, ToksIP, _} = erl_scan:string(STIP ++ "."),
	    STIP2 = case erl_parse:parse_term(ToksIP) of
			{ok, IPTParsed} -> IPTParsed;
			{error, _} -> STIP
		    end,
	    Module = list_to_atom(SModule),
	    NetProt2 = list_to_atom(SNetProt),
	    {ok, Tokens, _} = erl_scan:string(SOpts ++ "."),
	    {ok, Opts} = erl_parse:parse_term(Tokens),
	    {Port2, _SPort, IP2, _SIP, _SSPort, NetProt2, OptsClean} =
		get_port_data({Port2, STIP2, NetProt2}, Opts),
	    R=rpc:call(Node, ejabberd_listener, add_listener,
		       [{Port2, IP2, NetProt2}, Module, OptsClean]),
	    throw({is_added, R});
	_ ->
	    ok
    end.

node_modules_to_xhtml(Modules, Lang) ->
    ?XAE("table", [{"class", "withtextareas"}],
	 [?XE("thead",
	      [?XE("tr",
		   [?XCT("td", "Module"),
		    ?XCT("td", "Options")
		   ])]),
	  ?XE("tbody",
	      lists:map(
		fun({Module, Opts} = _E) ->
			SModule = atom_to_list(Module),
			{NumLines, SOpts} = term_to_paragraph(Opts, 40),
			%%ID = term_to_id(E),
			?XE("tr",
			    [?XC("td", SModule),
			     ?XE("td", [?TEXTAREA("opts" ++ SModule, integer_to_list(NumLines), "40", SOpts)]),
			     ?XE("td", [?INPUTT("submit", "restart" ++ SModule,
						"Restart")]),
			     ?XE("td", [?INPUTT("submit", "stop" ++ SModule,
						"Stop")])
			    ]
			   )
		end, Modules) ++
	      [?XE("tr",
		   [?XE("td", [?INPUT("text", "modulenew", "")]),
		    ?XE("td", [?TEXTAREA("optsnew", "2", "40", "[]")]),
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
	    ModulesToUpdateStrings = proplists:get_all_values("selected",Query),
	    ModulesToUpdate = [list_to_atom(M) || M <- ModulesToUpdateStrings],
	    case rpc:call(Node, ejabberd_update, update, [ModulesToUpdate]) of
		{ok, _} ->
		    ok;
		{error, Error} ->
		    ?ERROR_MSG("~p~n", [Error]),
		    {error, io_lib:format("~p", [Error])};
		{badrpc, Error} ->
		    ?ERROR_MSG("Bad RPC: ~p~n", [Error]),
		    {error, "Bad RPC: " ++ io_lib:format("~p", [Error])}
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

%% Transform 1234567890 into "1,234,567,890"
pretty_string_int(Integer) when is_integer(Integer) ->
    pretty_string_int(integer_to_list(Integer));
pretty_string_int(String) when is_list(String) ->
    {_, Result} = lists:foldl(
		    fun(NewNumber, {3, Result}) ->
			    {1, [NewNumber, $, | Result]};
		       (NewNumber, {CountAcc, Result}) ->
			    {CountAcc+1, [NewNumber | Result]}
		    end,
		    {0, ""},
		    lists:reverse(String)),
    Result.

%%%==================================
%%%% navigation menu

%% @spec (Host, Node, Lang, JID::jid()) -> [LI]
make_navigation(Host, Node, Lang, JID) ->
    Menu = make_navigation_menu(Host, Node, Lang, JID),
    make_menu_items(Lang, Menu).

%% @spec (Host, Node, Lang, JID::jid()) -> Menu
%% where Host = global | string()
%%       Node = cluster | string()
%%       Lang = string()
%%       Menu = {URL, Title} | {URL, Title, [Menu]}
%%       URL = string()
%%       Title = string()
make_navigation_menu(Host, Node, Lang, JID) ->
    HostNodeMenu = make_host_node_menu(Host, Node, Lang, JID),
    HostMenu = make_host_menu(Host, HostNodeMenu, Lang, JID),
    NodeMenu = make_node_menu(Host, Node, Lang),
    make_server_menu(HostMenu, NodeMenu, Lang, JID).

%% @spec (Host, Node, Base, Lang) -> [LI]
make_menu_items(global, cluster, Base, Lang) ->
    HookItems = get_menu_items_hook(server, Lang),
    make_menu_items(Lang, {Base, "", HookItems});

make_menu_items(global, Node, Base, Lang) ->
    HookItems = get_menu_items_hook({node, Node}, Lang),
    make_menu_items(Lang, {Base, "", HookItems});

make_menu_items(Host, cluster, Base, Lang) ->
    HookItems = get_menu_items_hook({host, Host}, Lang),
    make_menu_items(Lang, {Base, "", HookItems});

make_menu_items(Host, Node, Base, Lang) ->
    HookItems = get_menu_items_hook({hostnode, Host, Node}, Lang),
    make_menu_items(Lang, {Base, "", HookItems}).


make_host_node_menu(global, _, _Lang, _JID) ->
    {"", "", []};
make_host_node_menu(_, cluster, _Lang, _JID) ->
    {"", "", []};
make_host_node_menu(Host, Node, Lang, JID) ->
    HostNodeBase = get_base_path(Host, Node),
    HostNodeFixed = [{"modules/", "Modules"}]
    ++ get_menu_items_hook({hostnode, Host, Node}, Lang),
    HostNodeBasePath = url_to_path(HostNodeBase),
    HostNodeFixed2 = [Tuple || Tuple <- HostNodeFixed, is_allowed_path(HostNodeBasePath, Tuple, JID)],
    {HostNodeBase, atom_to_list(Node), HostNodeFixed2}.

make_host_menu(global, _HostNodeMenu, _Lang, _JID) ->
    {"", "", []};
make_host_menu(Host, HostNodeMenu, Lang, JID) ->
    HostBase = get_base_path(Host, cluster),
    HostFixed = [{"acls", "Access Control Lists"},
		 {"access", "Access Rules"},
		 {"users", "Users"},
		 {"online-users", "Online Users"}]
		++ get_lastactivity_menuitem_list(Host) ++
		[{"nodes", "Nodes", HostNodeMenu},
		 {"stats", "Statistics"}]
	++ get_menu_items_hook({host, Host}, Lang),
    HostBasePath = url_to_path(HostBase),
    HostFixed2 = [Tuple || Tuple <- HostFixed, is_allowed_path(HostBasePath, Tuple, JID)],
    {HostBase, Host, HostFixed2}.

make_node_menu(_Host, cluster, _Lang) ->
    {"", "", []};
make_node_menu(global, Node, Lang) ->
    NodeBase = get_base_path(global, Node),
    NodeFixed = [{"db/", "Database"},
		 {"backup/", "Backup"},
		 {"ports/", "Listened Ports"},
		 {"stats/", "Statistics"},
		 {"update/", "Update"}]
	++ get_menu_items_hook({node, Node}, Lang),
    {NodeBase, atom_to_list(Node), NodeFixed};
make_node_menu(_Host, _Node, _Lang) ->
    {"", "", []}.

make_server_menu(HostMenu, NodeMenu, Lang, JID) ->
    Base = get_base_path(global, cluster),
    Fixed = [{"acls", "Access Control Lists"},
	     {"access", "Access Rules"},
	     {"vhosts", "Virtual Hosts", HostMenu},
	     {"nodes", "Nodes", NodeMenu},
	     {"stats", "Statistics"}]
	++ get_menu_items_hook(server, Lang),
    BasePath = url_to_path(Base),
    Fixed2 = [Tuple || Tuple <- Fixed, is_allowed_path(BasePath, Tuple, JID)],
    {Base, "ejabberd", Fixed2}.


get_menu_items_hook({hostnode, Host, Node}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_hostnode, Host, [], [Host, Node, Lang]);
get_menu_items_hook({host, Host}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_host, Host, [], [Host, Lang]);
get_menu_items_hook({node, Node}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_node, [], [Node, Lang]);
get_menu_items_hook(server, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_main, [], [Lang]).


%% @spec (Lang::string(), Menu) -> [LI]
%% where Menu = {MURI::string(), MName::string(), Items::[Item]}
%%       Item = {IURI::string(), IName::string()} | {IURI::string(), IName::string(), Menu}
make_menu_items(Lang, Menu) ->
    lists:reverse(make_menu_items2(Lang, 1, Menu)).

make_menu_items2(Lang, Deep, {MURI, MName, _} = Menu) ->
    Res = case MName of
	      "" -> [];
	      _ -> [make_menu_item(header, Deep, MURI, MName, Lang) ]
	  end,
    make_menu_items2(Lang, Deep, Menu, Res).

make_menu_items2(_, _Deep, {_, _, []}, Res) ->
    Res;

make_menu_items2(Lang, Deep, {MURI, MName, [Item | Items]}, Res) ->
    Res2 = case Item of
	       {IURI, IName} ->
		   [make_menu_item(item, Deep, MURI++IURI++"/", IName, Lang) | Res];
	       {IURI, IName, SubMenu} ->
		   %%ResTemp = [?LI([?ACT(MURI ++ IURI ++ "/", IName)]) | Res],
		   ResTemp = [make_menu_item(item, Deep, MURI++IURI++"/", IName, Lang) | Res],
		   ResSubMenu = make_menu_items2(Lang, Deep+1, SubMenu),
		   ResSubMenu ++ ResTemp
	   end,
    make_menu_items2(Lang, Deep, {MURI, MName, Items}, Res2).

make_menu_item(header, 1, URI, Name, _Lang) ->
    ?LI([?XAE("div", [{"id", "navhead"}], [?AC(URI, Name)] )]);
make_menu_item(header, 2, URI, Name, _Lang) ->
    ?LI([?XAE("div", [{"id", "navheadsub"}], [?AC(URI, Name)] )]);
make_menu_item(header, 3, URI, Name, _Lang) ->
    ?LI([?XAE("div", [{"id", "navheadsubsub"}], [?AC(URI, Name)] )]);
make_menu_item(item, 1, URI, Name, Lang) ->
    ?LI([?XAE("div", [{"id", "navitem"}], [?ACT(URI, Name)] )]);
make_menu_item(item, 2, URI, Name, Lang) ->
    ?LI([?XAE("div", [{"id", "navitemsub"}], [?ACT(URI, Name)] )]);
make_menu_item(item, 3, URI, Name, Lang) ->
    ?LI([?XAE("div", [{"id", "navitemsubsub"}], [?ACT(URI, Name)] )]).

%%%==================================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
