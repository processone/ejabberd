%%%----------------------------------------------------------------------
%%% File    : ejabberd_web_admin.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Administration web interface
%%% Created :  9 Apr 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

%%%% definitions

-module(ejabberd_web_admin).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([process/2, list_users/4,
	 list_users_in_diapason/4, pretty_print_xml/1,
	 term_to_id/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-define(INPUTATTRS(Type, Name, Value, Attrs),
	?XA(<<"input">>,
	    (Attrs ++
	       [{<<"type">>, Type}, {<<"name">>, Name},
		{<<"value">>, Value}]))).

%%%==================================
%%%% get_acl_access

%% @spec (Path::[string()], Method) -> {HostOfRule, [AccessRule]}
%% where Method = 'GET' | 'POST'

%% All accounts can access those URLs
get_acl_rule([], _) -> {<<"localhost">>, [all]};
get_acl_rule([<<"style.css">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"logo.png">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"logo-fill.png">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"favicon.ico">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"additions.js">>], _) ->
    {<<"localhost">>, [all]};
%% This page only displays vhosts that the user is admin:
get_acl_rule([<<"vhosts">>], _) ->
    {<<"localhost">>, [all]};
%% The pages of a vhost are only accesible if the user is admin of that vhost:
get_acl_rule([<<"server">>, VHost | _RPath], Method)
    when Method =:= 'GET' orelse Method =:= 'HEAD' ->
    {VHost, [configure, webadmin_view]};
get_acl_rule([<<"server">>, VHost | _RPath], 'POST') ->
    {VHost, [configure]};
%% Default rule: only global admins can access any other random page
get_acl_rule(_RPath, Method)
    when Method =:= 'GET' orelse Method =:= 'HEAD' ->
    {global, [configure, webadmin_view]};
get_acl_rule(_RPath, 'POST') -> {global, [configure]}.

is_acl_match(Host, Rules, Jid) ->
    lists:any(fun (Rule) ->
		      allow == acl:match_rule(Host, Rule, Jid)
	      end,
	      Rules).

%%%==================================
%%%% Menu Items Access

get_jid(Auth, HostHTTP, Method) ->
    case get_auth_admin(Auth, HostHTTP, [], Method) of
      {ok, {User, Server}} ->
	  jid:make(User, Server, <<"">>);
      {unauthorized, Error} ->
	  ?ERROR_MSG("Unauthorized ~p: ~p", [Auth, Error]),
	  throw({unauthorized, Auth})
    end.

get_menu_items(global, cluster, Lang, JID) ->
    {Base, _, Items} = make_server_menu([], [], Lang, JID),
    lists:map(fun ({URI, Name}) ->
		      {<<Base/binary, URI/binary, "/">>, Name};
		  ({URI, Name, _SubMenu}) ->
		      {<<Base/binary, URI/binary, "/">>, Name}
	      end,
	      Items);
get_menu_items(Host, cluster, Lang, JID) ->
    {Base, _, Items} = make_host_menu(Host, [], Lang, JID),
    lists:map(fun ({URI, Name}) ->
		      {<<Base/binary, URI/binary, "/">>, Name};
		  ({URI, Name, _SubMenu}) ->
		      {<<Base/binary, URI/binary, "/">>, Name}
	      end,
	      Items).

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

is_allowed_path([<<"admin">> | Path], JID) ->
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
url_to_path(URL) -> str:tokens(URL, <<"/">>).

%%%==================================
%%%% process/2

process([<<"doc">>, LocalFile], _Request) ->
    DocPath = case os:getenv("EJABBERD_DOC_PATH") of
		P when is_list(P) -> P;
		false -> <<"/share/doc/ejabberd/">>
	      end,
    FileName = filename:join(DocPath, LocalFile),
    case file:read_file(FileName) of
      {ok, FileContents} ->
	  ?DEBUG("Delivering content.", []),
	  {200, [{<<"Server">>, <<"ejabberd">>}], FileContents};
      {error, Error} ->
	  Help = <<" ", FileName/binary,
		   " - Try to specify the path to ejabberd "
		   "documentation with the environment variable "
		   "EJABBERD_DOC_PATH. Check the ejabberd "
		   "Guide for more information.">>,
	  ?INFO_MSG("Problem '~p' accessing the local Guide file ~s", [Error, Help]),
	  case Error of
	    eacces -> {403, [], <<"Forbidden", Help/binary>>};
	    enoent -> {307, [{<<"Location">>, <<"http://docs.ejabberd.im/admin/guide/configuration/">>}], <<"Not found", Help/binary>>};
	    _Else ->
		{404, [], <<(iolist_to_binary(atom_to_list(Error)))/binary, Help/binary>>}
	  end
    end;
process([<<"server">>, SHost | RPath] = Path,
	#request{auth = Auth, lang = Lang, host = HostHTTP,
		 method = Method} =
	    Request) ->
    Host = jid:nameprep(SHost),
    case lists:member(Host, ?MYHOSTS) of
      true ->
	  case get_auth_admin(Auth, HostHTTP, Path, Method) of
	    {ok, {User, Server}} ->
		AJID = get_jid(Auth, HostHTTP, Method),
		process_admin(Host,
			      Request#request{path = RPath,
					      auth = {auth_jid, Auth, AJID},
					      us = {User, Server}});
	    {unauthorized, <<"no-auth-provided">>} ->
		{401,
		 [{<<"WWW-Authenticate">>,
		   <<"basic realm=\"ejabberd\"">>}],
		 ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					       <<"Unauthorized">>)])};
	    {unauthorized, Error} ->
		{BadUser, _BadPass} = Auth,
		{IPT, _Port} = Request#request.ip,
		IPS = ejabberd_config:may_hide_data(jlib:ip_to_list(IPT)),
		?WARNING_MSG("Access of ~p from ~p failed with error: ~p",
			     [BadUser, IPS, Error]),
		{401,
		 [{<<"WWW-Authenticate">>,
		   <<"basic realm=\"auth error, retry login "
		     "to ejabberd\"">>}],
		 ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					       <<"Unauthorized">>)])}
	  end;
      false -> ejabberd_web:error(not_found)
    end;
process(RPath,
	#request{auth = Auth, lang = Lang, host = HostHTTP,
		 method = Method} =
	    Request) ->
    case get_auth_admin(Auth, HostHTTP, RPath, Method) of
      {ok, {User, Server}} ->
	  AJID = get_jid(Auth, HostHTTP, Method),
	  process_admin(global,
			Request#request{path = RPath,
					auth = {auth_jid, Auth, AJID},
					us = {User, Server}});
      {unauthorized, <<"no-auth-provided">>} ->
	  {401,
	   [{<<"WWW-Authenticate">>,
	     <<"basic realm=\"ejabberd\"">>}],
	   ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					 <<"Unauthorized">>)])};
      {unauthorized, Error} ->
	  {BadUser, _BadPass} = Auth,
	  {IPT, _Port} = Request#request.ip,
	  IPS = ejabberd_config:may_hide_data(jlib:ip_to_list(IPT)),
	  ?WARNING_MSG("Access of ~p from ~p failed with error: ~p",
		       [BadUser, IPS, Error]),
	  {401,
	   [{<<"WWW-Authenticate">>,
	     <<"basic realm=\"auth error, retry login "
	       "to ejabberd\"">>}],
	   ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					 <<"Unauthorized">>)])}
    end.

get_auth_admin(Auth, HostHTTP, RPath, Method) ->
    case Auth of
      {SJID, Pass} ->
	  {HostOfRule, AccessRule} = get_acl_rule(RPath, Method),
	  case jid:from_string(SJID) of
	    error -> {unauthorized, <<"badformed-jid">>};
	    #jid{user = <<"">>, server = User} ->
		get_auth_account(HostOfRule, AccessRule, User, HostHTTP,
				 Pass);
	    #jid{user = User, server = Server} ->
		get_auth_account(HostOfRule, AccessRule, User, Server,
				 Pass)
	  end;
      undefined -> {unauthorized, <<"no-auth-provided">>}
    end.

get_auth_account(HostOfRule, AccessRule, User, Server,
		 Pass) ->
    case ejabberd_auth:check_password(User, Server, Pass) of
      true ->
	  case is_acl_match(HostOfRule, AccessRule,
			    jid:make(User, Server, <<"">>))
	      of
	    false -> {unauthorized, <<"unprivileged-account">>};
	    true -> {ok, {User, Server}}
	  end;
      false ->
	  case ejabberd_auth:is_user_exists(User, Server) of
	    true -> {unauthorized, <<"bad-password">>};
	    false -> {unauthorized, <<"inexistent-account">>}
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
    Base = get_base_path(Host, cluster),
    MenuItems = make_navigation(Host, Node, Lang, JID),
    {200, [html],
     #xmlel{name = <<"html">>,
	    attrs =
		[{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>},
		 {<<"xml:lang">>, Lang}, {<<"lang">>, Lang}]++direction(Lang),
	    children =
		[#xmlel{name = <<"head">>, attrs = [],
			children =
			    [?XCT(<<"title">>, <<"ejabberd Web Admin">>),
			     #xmlel{name = <<"meta">>,
				    attrs =
					[{<<"http-equiv">>, <<"Content-Type">>},
					 {<<"content">>,
					  <<"text/html; charset=utf-8">>}],
				    children = []},
			     #xmlel{name = <<"script">>,
				    attrs =
					[{<<"src">>,
					  <<Base/binary, "/additions.js">>},
					 {<<"type">>, <<"text/javascript">>}],
				    children = [?C(<<" ">>)]},
			     #xmlel{name = <<"link">>,
				    attrs =
					[{<<"href">>,
					  <<Base/binary, "favicon.ico">>},
					 {<<"type">>, <<"image/x-icon">>},
					 {<<"rel">>, <<"shortcut icon">>}],
				    children = []},
			     #xmlel{name = <<"link">>,
				    attrs =
					[{<<"href">>,
					  <<Base/binary, "style.css">>},
					 {<<"type">>, <<"text/css">>},
					 {<<"rel">>, <<"stylesheet">>}],
				    children = []}]},
		 ?XE(<<"body">>,
		     [?XAE(<<"div">>, [{<<"id">>, <<"container">>}],
			   [?XAE(<<"div">>, [{<<"id">>, <<"header">>}],
				 [?XE(<<"h1">>,
				      [?ACT(<<"/admin/">>,
					    <<"ejabberd Web Admin">>)])]),
			    ?XAE(<<"div">>, [{<<"id">>, <<"navigation">>}],
				 [?XE(<<"ul">>, MenuItems)]),
			    ?XAE(<<"div">>, [{<<"id">>, <<"content">>}], Els),
			    ?XAE(<<"div">>, [{<<"id">>, <<"clearcopyright">>}],
				 [{xmlcdata, <<"">>}])]),
		      ?XAE(<<"div">>, [{<<"id">>, <<"copyrightouter">>}],
			   [?XAE(<<"div">>, [{<<"id">>, <<"copyright">>}],
				 [?XE(<<"p">>,
				  [?AC(<<"https://www.ejabberd.im/">>, <<"ejabberd">>),
				   ?C(<<" (c) 2002-2015 ">>),
				   ?AC(<<"https://www.process-one.net/">>, <<"ProcessOne, leader in messaging and push solutions">>)]
                                 )])])])]}}.

direction(ltr) -> [{<<"dir">>, <<"ltr">>}];
direction(<<"he">>) -> [{<<"dir">>, <<"rtl">>}];
direction(_) -> [].

get_base_path(global, cluster) -> <<"/admin/">>;
get_base_path(Host, cluster) ->
    <<"/admin/server/", Host/binary, "/">>;
get_base_path(global, Node) ->
    <<"/admin/node/",
      (iolist_to_binary(atom_to_list(Node)))/binary, "/">>;
get_base_path(Host, Node) ->
    <<"/admin/server/", Host/binary, "/node/",
      (iolist_to_binary(atom_to_list(Node)))/binary, "/">>.

%%%==================================
%%%% css & images

additions_js() ->
    <<"\nfunction selectAll() {\n  for(i=0;i<documen"
      "t.forms[0].elements.length;i++)\n  { "
      "var e = document.forms[0].elements[i];\n "
      "   if(e.type == 'checkbox')\n    { e.checked "
      "= true; }\n  }\n}\nfunction unSelectAll() "
      "{\n  for(i=0;i<document.forms[0].elements.len"
      "gth;i++)\n  { var e = document.forms[0].eleme"
      "nts[i];\n    if(e.type == 'checkbox')\n "
      "   { e.checked = false; }\n  }\n}\n">>.

css(Host) ->
    Base = get_base_path(Host, cluster),
    <<"html,body {\n"
    "  margin: 0;\n"
    "  padding: 0;\n"
    "  height: 100%;\n"
    "  background: #f9f9f9;\n"
    "  font-family: sans-serif;\n"
    "}\n"
    "a {\n"
    "  text-decoration: none;\n"
    "  color: #3eaffa;\n"
    "}\n"
    "a:hover,\n"
    "a:active {\n"
    "  text-decoration: underline;\n"
    "}\n"
    "#container {\n"
    "  position: relative;\n"
    "  padding: 0;\n"
    "  margin: 0 auto;\n"
    "  max-width: 1280px;\n"
    "  min-height: 100%;\n"
    "  height: 100%;\n"
    "  margin-bottom: -30px;\n"
    "  z-index: 1;\n"
    "}\n"
    "html>body #container {\n"
    "  height: auto;\n"
    "}\n"
    "#header h1 {\n"
    "  width: 100%;\n"
    "  height: 50px;\n"
    "  padding: 0;\n"
    "  margin: 0;\n"
    "  background-color: #49cbc1;\n"
    "}\n"
    "#header h1 a {\n"
    "  position: absolute;\n"
    "  top: 0;\n"
    "  left: 0;\n"
    "  width: 100%;\n"
    "  height: 50px;\n"
    "  padding: 0;\n"
    "  margin: 0;\n"
    "  background: url('",Base/binary,"logo.png') 10px center no-repeat transparent;\n"
    "  background-size: auto 25px;\n"
    "  display: block;\n"
    "  text-indent: -9999px;\n"
    "}\n"
    "#clearcopyright {\n"
    "  display: block;\n"
    "  width: 100%;\n"
    "  height: 30px;\n"
    "}\n"
    "#copyrightouter {\n"
    "  position: relative;\n"
    "  display: table;\n"
    "  width: 100%;\n"
    "  height: 30px;\n"
    "  z-index: 2;\n"
    "}\n"
    "#copyright {\n"
    "  display: table-cell;\n"
    "  vertical-align: bottom;\n"
    "  width: 100%;\n"
    "  height: 30px;\n"
    "}\n"
    "#copyright a {\n"
    "  font-weight: bold;\n"
    "  color: #fff;\n"
    "}\n"
    "#copyright p {\n"
    "  margin-left: 0;\n"
    "  margin-right: 0;\n"
    "  margin-top: 5px;\n"
    "  margin-bottom: 0;\n"
    "  padding-left: 0;\n"
    "  padding-right: 0;\n"
    "  padding-top: 5px;\n"
    "  padding-bottom: 5px;\n"
    "  width: 100%;\n"
    "  color: #fff;\n"
    "  background-color: #30353E;\n"
    "  font-size: 0.75em;\n"
    "  text-align: center;\n"
    "}\n"
    "#navigation ul {\n"
    "  position: absolute;\n"
    "  top: 75px;\n"
    "  left: 0;\n"
    "  padding: 0;\n"
    "  margin: 0;\n"
    "  width: 17em;\n"
    "  background: #fff;\n"
    "}\n"
    "#navigation ul li {\n"
    "  list-style: none;\n"
    "  margin: 0;\n"
    "\n"
    "  border-bottom: 1px solid #f9f9f9;\n"
    "  text-align: left;\n"
    "}\n"
    "#navigation ul li a {\n"
    "  margin: 0;\n"
    "  display: inline-block;\n"
    "  padding: 10px;\n"
    "  color: #333;\n"
    "}\n"
    "ul li #navhead a, ul li #navheadsub a, ul li #navheadsubsub a {\n"
    "  font-size: 1.5em;\n"
    "  color: inherit;\n"
    "}\n"
    "#navitemsub {\n"
    "  border-left: 0.5em solid #424a55;\n"
    "}\n"
    "#navitemsubsub {\n"
    "  border-left: 2em solid #424a55;\n"
    "}\n"
    "#navheadsub,\n"
    "#navheadsubsub {\n"
    "  padding-left: 0.5em;\n"
    "}\n"
    "#navhead,\n"
    "#navheadsub,\n"
    "#navheadsubsub {\n"
    "  border-top: 3px solid #49cbc1;\n"
    "  background: #424a55;\n"
    "  color: #fff;\n"
    "}\n"
    "#lastactivity li {\n"
    "  padding: 2px;\n"
    "  margin-bottom: -1px;\n"
    "}\n"
    "thead tr td {\n"
    "  background: #3eaffa;\n"
    "  color: #fff;\n"
    "}\n"
    "td.copy {\n"
    "  text-align: center;\n"
    "}\n"
    "tr.head {\n"
    "  color: #fff;\n"
    "  background-color: #3b547a;\n"
    "  text-align: center;\n"
    "}\n"
    "tr.oddraw {\n"
    "  color: #412c75;\n"
    "  background-color: #ccd4df;\n"
    "  text-align: center;\n"
    "}\n"
    "tr.evenraw {\n"
    "  color: #412c75;\n"
    "  background-color: #dbe0e8;\n"
    "  text-align: center;\n"
    "}\n"
    "td.leftheader {\n"
    "  color: #412c75;\n"
    "  background-color: #ccccc1;\n"
    "  padding-left: 5px;\n"
    "  padding-top: 2px;\n"
    "  padding-bottom: 2px;\n"
    "  margin-top: 0px;\n"
    "  margin-bottom: 0px;\n"
    "}\n"
    "td.leftcontent {\n"
    "  color: #000044;\n"
    "  background-color: #e6e6df;\n"
    "  padding-left: 5px;\n"
    "  padding-right: 5px;\n"
    "  padding-top: 2px;\n"
    "  padding-bottom: 2px;\n"
    "  margin-top: 0px;\n"
    "  margin-bottom: 0px;\n"
    "}\n"
    "td.rightcontent {\n"
    "  color: #000044;\n"
    "  text-align: justify;\n"
    "  padding-left: 10px;\n"
    "  padding-right: 10px;\n"
    "  padding-bottom: 5px;\n"
    "}\n"
    "\n"
    "h1 {\n"
    "  color: #000044;\n"
    "  padding-top: 2px;\n"
    "  padding-bottom: 2px;\n"
    "  margin-top: 0px;\n"
    "  margin-bottom: 0px;\n"
    "}\n"
    "h2 {\n"
    "  color: #000044;\n"
    "  text-align: center;\n"
    "  padding-top: 2px;\n"
    "  padding-bottom: 2px;\n"
    "  margin-top: 0px;\n"
    "  margin-bottom: 0px;\n"
    "}\n"
    "h3 {\n"
    "  color: #000044;\n"
    "  text-align: left;\n"
    "  padding-top: 20px;\n"
    "  padding-bottom: 2px;\n"
    "  margin-top: 0px;\n"
    "  margin-bottom: 0px;\n"
    "}\n"
    "#content ul {\n"
    "  padding-left: 1.1em;\n"
    "  margin-top: 1em;\n"
    "}\n"
    "#content ul li {\n"
    "  list-style-type: disc;\n"
    "  padding: 5px;\n"
    "}\n"
    "#content ul.nolistyle>li {\n"
    "  list-style-type: none;\n"
    "}\n"
    "#content {\n"
    "  padding-left: 19em;\n"
    "  padding-top: 25px;\n"
    "}\n"
    "div.guidelink,\n"
    "p[dir=ltr] {\n"
    "  display: inline-block;\n"
    "  float: right;\n"
    "\n"
    "  margin: 0;\n"
    "  margin-right: 1em;\n"
    "}\n"
    "div.guidelink a,\n"
    "p[dir=ltr] a {\n"
    "  display: inline-block;\n"
    "  border-radius: 3px;\n"
    "  padding: 3px;\n"
    "\n"
    "  background: #3eaffa;\n"
    "\n"
    "  text-transform: uppercase;\n"
    "  font-size: 0.75em;\n"
    "  color: #fff;\n"
    "}\n"
    "table {\n"
    "  margin-top: 1em;\n"
    "}\n"
    "table tr td {\n"
    "  padding: 0.5em;\n"
    "}\n"
    "table tr:nth-child(odd) {\n"
    "  background: #fff;\n"
    "}\n"
    "table.withtextareas>tbody>tr>td {\n"
    "  vertical-align: top;\n"
    "}\n"
    "textarea {\n"
    "  margin-bottom: 1em;\n"
    "}\n"
    "input,\n"
    "select {\n"
    "  font-size: 1em;\n"
    "}\n"
    "p.result {\n"
    "  border: 1px;\n"
    "  border-style: dashed;\n"
    "  border-color: #FE8A02;\n"
    "  padding: 1em;\n"
    "  margin-right: 1em;\n"
    "  background: #FFE3C9;\n"
    "}\n"
    "*.alignright {\n"
    "  text-align: right;\n"
    "}">>.

favicon() ->
    jlib:decode_base64(<<"AAABAAEAEBAAAAEAIAAoBQAAFgAAACgAAAAQAAAAIAAAAAEAIAAAAAAAAAUAAAAAAAAAAAAAAAAAAAAAAAAAAAA1AwMAQwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwMARQUEA+oFAwCOBAQAaAQEAGkEBABpBAQAaQQEAGoFAgBcBAAAOQAAAAoAAAAAAAAAAAAAAAAAAAAAAAAAAAMDAEIHBgX/BwYF/wcGBf8HBgX/BwYF/wcGBf8HBgX/BwYF/wUFA/wEBAHOBQICXgAAAAAAAAAAAAAAAAAAAAADAwBCBwYF/wcGBf8HBgX/BwYF/wcGBf8HBgX/BwYF/wcGBf8HBgX/BwYF/wcGBf8DAwCUAAAABwAAAAAAAAAAAwMAQgcGBf8HBgX/BwYF/wcGBf8FBQPMBAAAaAQAAD8DAwNOAwMDlgUFA/QHBgX/BwYF/wQEAHkAAAAAAAAAAAMDAEIHBgX/BwYF/wcGBf8EBAGeAAAACAAAAAAAAAASAAAABQAAAAAFBQGxBwYF/wcGBf8FBAPvAAAAKAAAAAADAwBCBwYF/wcGBf8EBAHPAAAADQAAACEFBQGuBQQD8AUEAeEFBQGuBQQB9QcGBf8HBgX/BwYF/wQEAH8AAAAAAwMAQgcGBf8HBgX/BgQAbwAAAAADAwOXBQQB3gUFAdgFBQHZBQQB3QUFAdYFBAHhBQUD/gcGBf8EBAK8AAAAAAMDAEIHBgX/BwYF/wQAAD0AAAAAAAAABQAAAAEAAAABAAAAAQAAAAEAAAAFAAAAEQUFArwKBgX/BQMDxQAAAAADAwBCBwYF/wcGBf8DAwBKAAAAAwYDAFAGAwBVBgMAVAYDAFQFAgJZAAAALwAAAAAFBQGuCgYF/wUDA8QAAAAAAAAAKwUEA/QHBgX/AwMDlgAAAAAFAwOIBwYF/wcGBf8HBgX/BQQB5wAAADMAAAAWBQUD5wcGBf8EBAGbAAAAAAAAAAYFBAG9BwYF/wUFA/EDAABAAAAAAAMDA1QDAwOYBQUAhQAAACQAAAAABAQBnQcGBf8HBgX/AwMATQAAAAAAAAAAAwAAQwUFA/oHBgX/BQQB5QYDA1UAAAAAAAAAAAAAAAAAAAAXAwMAlwcGBf8HBgX/BQUBtQAAAAcAAAAAAAAAAAAAAAAEBABzBQUD/gcGBf8HBgX/BQMDyQQEAZwGBAGqBQQB5AcGBf8HBgX/BAQB0QAAACEAAAAAAAAAAAAAAAAAAAAAAAAAAAUFAmQFBAHlBwYF/wcGBf8HBgX/BwYF/wcGBf8FBQP+BQUBsAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHwUFA40FBAHrBwYF/wUFA/4FAwPGBgMAUgAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==">>).

logo() ->
    jlib:decode_base64(<<"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V4NgkhURIAJEgAgQASJQGgSWwNI7QYsRqG4ojdU0lAgQASJABIgAESACRIAIEAEiQAR0EYiiaAwobdmGC/8RdIyuluROBIgAESACRIAIEAEiQASIABEgAqVFAEFnmsB1J677Bmh8aYGj4USACBABIkAEiAARIAJEgAgQASJgB4EUgesduGayHe0ohQgQASJABIgAESACRIAIEAEiQARKj0CCwPUl1D2j9IARACJABIgAESACRIAIEAEiQASIABGwi0DMwPWfUW+4Xc0ojQgQASJABIgAESACRIAIEAEiQASIABAYIHDdhfMXEigiQASIABEgAkSACBABIkAEiAARIAK5IdAkcF2Hc+/NTTEKJgJEgAgQASJABIgAESACRIAIEAEiIAg0CFxX4fgsIkQEiAARIAJEgAgQASJABIgAESACRCB3BOoErqtx7IjcFaMCRIAIEAEiQASIABEgAkSACBABIkAEBIE+gesWfJ9NZIgAESACRIAIEAEiQASIABEgAkSACDiDQI/AdT/+/4AzilERIkAEiAARIAJEgAgQASJABIgAESACgkCPwPXviQgRIAJEgAgQASJABIgAESACRIAIEAHnEKgGrk/hc5BzylEhIkAEiAARIAJEgAgQASJABIgAESACCFhHgo4lEkSACBABIkAEiAARIAJEgAgQASJABIgAESACRIAIEAFjCPx/2P3JeG4VmJoAAAAASUVORK5CYII=">>).

logo_fill() ->
    jlib:decode_base64(<<"iVBORw0KGgoAAAANSUhEUgAAAAYAAAA3BAMAAADdxCZzA"
		    "AAAAXNSR0IArs4c6QAAAB5QTFRF1nYO/ooC/o4O/pIS/p"
		    "4q/q5K/rpq/sqM/tam/ubGzn/S/AAAAEFJREFUCNdlw0s"
		    "RwCAQBUE+gSRHLGABC1jAAhbWAhZwC+88XdXOXb4UlFAr"
		    "SmwN5ekdJY2BkudEec1QvrVQ/r3xOlK9HsTvertmAAAAA"
		    "ElFTkSuQmCC">>).

%%%==================================
%%%% process_admin

process_admin(global,
	      #request{path = [], auth = {_, _, AJID},
		       lang = Lang}) ->
    make_xhtml((?H1GL((?T(<<"Administration">>)), <<"">>,
		      <<"Contents">>))
		 ++
		 [?XE(<<"ul">>,
		      [?LI([?ACT(MIU, MIN)])
		       || {MIU, MIN}
			      <- get_menu_items(global, cluster, Lang, AJID)])],
	       global, Lang, AJID);
process_admin(Host,
	      #request{path = [], auth = {_, _Auth, AJID},
		       lang = Lang}) ->
    make_xhtml([?XCT(<<"h1">>, <<"Administration">>),
		?XE(<<"ul">>,
		    [?LI([?ACT(MIU, MIN)])
		     || {MIU, MIN}
			    <- get_menu_items(Host, cluster, Lang, AJID)])],
	       Host, Lang, AJID);
process_admin(Host,
	      #request{path = [<<"style.css">>]}) ->
    {200,
     [{<<"Content-Type">>, <<"text/css">>}, last_modified(),
      cache_control_public()],
     css(Host)};
process_admin(_Host,
	      #request{path = [<<"favicon.ico">>]}) ->
    {200,
     [{<<"Content-Type">>, <<"image/x-icon">>},
      last_modified(), cache_control_public()],
     favicon()};
process_admin(_Host,
	      #request{path = [<<"logo.png">>]}) ->
    {200,
     [{<<"Content-Type">>, <<"image/png">>}, last_modified(),
      cache_control_public()],
     logo()};
process_admin(_Host,
	      #request{path = [<<"logo-fill.png">>]}) ->
    {200,
     [{<<"Content-Type">>, <<"image/png">>}, last_modified(),
      cache_control_public()],
     logo_fill()};
process_admin(_Host,
	      #request{path = [<<"additions.js">>]}) ->
    {200,
     [{<<"Content-Type">>, <<"text/javascript">>},
      last_modified(), cache_control_public()],
     additions_js()};
process_admin(Host,
	      #request{path = [<<"acls-raw">>], q = Query,
		       auth = {_, _Auth, AJID}, lang = Lang}) ->
    Res = case lists:keysearch(<<"acls">>, 1, Query) of
	    {value, {_, String}} ->
		case erl_scan:string(binary_to_list(String)) of
		  {ok, Tokens, _} ->
		      case erl_parse:parse_term(Tokens) of
			{ok, NewACLs} ->
                            acl:add_list(Host, NewACLs, true);
			_ -> error
		      end;
		  _ -> error
		end;
	    _ -> nothing
	  end,
    ACLs = lists:keysort(2,
			 ets:select(acl,
				    [{{acl, {'$1', Host}, '$2'}, [],
				      [{{acl, '$1', '$2'}}]}])),
    {NumLines, ACLsP} = term_to_paragraph(ACLs, 80),
    make_xhtml((?H1GL((?T(<<"Access Control Lists">>)),
		      <<"acl-definition">>, <<"ACL Definition">>))
		 ++
		 case Res of
		   ok -> [?XREST(<<"Submitted">>)];
		   error -> [?XREST(<<"Bad format">>)];
		   nothing -> []
		 end
		   ++
		   [?XAE(<<"form">>,
			 [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}]++direction(ltr),
			 [?TEXTAREA(<<"acls">>,
				    (iolist_to_binary(integer_to_list(lists:max([16,
										 NumLines])))),
				    <<"80">>, <<(iolist_to_binary(ACLsP))/binary, ".">>),
			  ?BR,
			  ?INPUTT(<<"submit">>, <<"submit">>, <<"Submit">>)])],
	       Host, Lang, AJID);
process_admin(Host,
	      #request{method = Method, path = [<<"acls">>],
		       auth = {_, _Auth, AJID}, q = Query, lang = Lang}) ->
    ?DEBUG("query: ~p", [Query]),
    Res = case Method of
	    'POST' ->
		case catch acl_parse_query(Host, Query) of
		  {'EXIT', _} -> error;
		  NewACLs ->
		      ?INFO_MSG("NewACLs at ~s: ~p", [Host, NewACLs]),
		      acl:add_list(Host, NewACLs, true)
		end;
	    _ -> nothing
	  end,
    ACLs = lists:keysort(2,
			 ets:select(acl,
				    [{{acl, {'$1', Host}, '$2'}, [],
				      [{{acl, '$1', '$2'}}]}])),
    make_xhtml((?H1GL((?T(<<"Access Control Lists">>)),
		      <<"acl-definition">>, <<"ACL Definition">>))
		 ++
		 case Res of
		   ok -> [?XREST(<<"Submitted">>)];
		   error -> [?XREST(<<"Bad format">>)];
		   nothing -> []
		 end
		   ++
		   [?XAE(<<"p">>, direction(ltr), [?ACT(<<"../acls-raw/">>, <<"Raw">>)])] ++
		     [?XAE(<<"form">>,
			   [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}]++direction(ltr),
			   [acls_to_xhtml(ACLs), ?BR,
			    ?INPUTT(<<"submit">>, <<"delete">>,
				    <<"Delete Selected">>),
			    ?C(<<" ">>),
			    ?INPUTT(<<"submit">>, <<"submit">>,
				    <<"Submit">>)])],
	       Host, Lang, AJID);
process_admin(Host,
	      #request{path = [<<"access-raw">>],
		       auth = {_, _Auth, AJID}, q = Query, lang = Lang}) ->
    SetAccess = fun (Rs) ->
			mnesia:transaction(fun () ->
						   Os = mnesia:select(access,
								      [{{access,
									 {'$1',
									  Host},
									 '$2'},
									[],
									['$_']}]),
						   lists:foreach(fun (O) ->
									 mnesia:delete_object(O)
								 end,
								 Os),
						   lists:foreach(fun ({access,
								       Name,
								       Rules}) ->
									 mnesia:write({access,
										       {Name,
											Host},
										       Rules})
								 end,
								 Rs)
					   end)
		end,
    Res = case lists:keysearch(<<"access">>, 1, Query) of
	    {value, {_, String}} ->
		case erl_scan:string(binary_to_list(String)) of
		  {ok, Tokens, _} ->
		      case erl_parse:parse_term(Tokens) of
			{ok, Rs} ->
			    case SetAccess(Rs) of
			      {atomic, _} -> ok;
			      _ -> error
			    end;
			_ -> error
		      end;
		  _ -> error
		end;
	    _ -> nothing
	  end,
    Access = ets:select(access,
			[{{access, {'$1', Host}, '$2'}, [],
			  [{{access, '$1', '$2'}}]}]),
    {NumLines, AccessP} = term_to_paragraph(lists:keysort(2,Access), 80),
    make_xhtml((?H1GL((?T(<<"Access Rules">>)),
		      <<"access-rights">>, <<"Access Rights">>))
		 ++
		 case Res of
		   ok -> [?XREST(<<"Submitted">>)];
		   error -> [?XREST(<<"Bad format">>)];
		   nothing -> []
		 end
		   ++
		   [?XAE(<<"form">>,
			 [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}]++direction(ltr),
			 [?TEXTAREA(<<"access">>,
				    (iolist_to_binary(integer_to_list(lists:max([16,
										 NumLines])))),
				    <<"80">>, <<(iolist_to_binary(AccessP))/binary, ".">>),
			  ?BR,
			  ?INPUTT(<<"submit">>, <<"submit">>, <<"Submit">>)])],
	       Host, Lang, AJID);
process_admin(Host,
	      #request{method = Method, path = [<<"access">>],
		       q = Query, auth = {_, _Auth, AJID}, lang = Lang}) ->
    ?DEBUG("query: ~p", [Query]),
    Res = case Method of
	    'POST' ->
		case catch access_parse_query(Host, Query) of
		  {'EXIT', _} -> error;
		  ok -> ok
		end;
	    _ -> nothing
	  end,
    AccessRules = ets:select(access,
			     [{{access, {'$1', Host}, '$2'}, [],
			       [{{access, '$1', '$2'}}]}]),
    make_xhtml((?H1GL((?T(<<"Access Rules">>)),
		      <<"access-rights">>, <<"Access Rights">>))
		 ++
		 case Res of
		   ok -> [?XREST(<<"Submitted">>)];
		   error -> [?XREST(<<"Bad format">>)];
		   nothing -> []
		 end
		   ++
		   [?XAE(<<"p">>, direction(ltr), [?ACT(<<"../access-raw/">>, <<"Raw">>)])]
		     ++
		     [?XAE(<<"form">>,
			   [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}]++direction(ltr),
			   [access_rules_to_xhtml(AccessRules, Lang), ?BR,
			    ?INPUTT(<<"submit">>, <<"delete">>,
				    <<"Delete Selected">>)])],
	       Host, Lang, AJID);
process_admin(Host,
	      #request{path = [<<"access">>, SName], q = Query,
		       auth = {_, _Auth, AJID}, lang = Lang}) ->
    ?DEBUG("query: ~p", [Query]),
    Name = jlib:binary_to_atom(SName),
    Res = case lists:keysearch(<<"rules">>, 1, Query) of
	    {value, {_, String}} ->
		case parse_access_rule(String) of
		  {ok, Rs} ->
		      ejabberd_config:add_option({access, Name, Host},
							Rs),
		      ok;
		  _ -> error
		end;
	    _ -> nothing
	  end,
    Rules = case ejabberd_config:get_option(
                   {access, Name, Host}, fun(V) -> V end)
		of
	      undefined -> [];
	      Rs1 -> Rs1
	    end,
    make_xhtml([?XC(<<"h1">>,
		    list_to_binary(io_lib:format(
                                     ?T(<<"~s access rule configuration">>),
                                     [SName])))]
		 ++
		 case Res of
		   ok -> [?XREST(<<"Submitted">>)];
		   error -> [?XREST(<<"Bad format">>)];
		   nothing -> []
		 end
		   ++
		   [?XAE(<<"form">>,
			 [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
			 [access_rule_to_xhtml(Rules), ?BR,
			  ?INPUTT(<<"submit">>, <<"submit">>, <<"Submit">>)])],
	       Host, Lang, AJID);
process_admin(global,
	      #request{path = [<<"vhosts">>], auth = {_, _Auth, AJID},
		       lang = Lang}) ->
    Res = list_vhosts(Lang, AJID),
    make_xhtml((?H1GL((?T(<<"Virtual Hosts">>)),
		      <<"virtual-hosting">>, <<"Virtual Hosting">>))
		 ++ Res,
	       global, Lang, AJID);
process_admin(Host,
	      #request{path = [<<"users">>], q = Query,
		       auth = {_, _Auth, AJID}, lang = Lang})
    when is_binary(Host) ->
    Res = list_users(Host, Query, Lang, fun url_func/1),
    make_xhtml([?XCT(<<"h1">>, <<"Users">>)] ++ Res, Host,
	       Lang, AJID);
process_admin(Host,
	      #request{path = [<<"users">>, Diap],
		       auth = {_, _Auth, AJID}, lang = Lang})
    when is_binary(Host) ->
    Res = list_users_in_diapason(Host, Diap, Lang,
				 fun url_func/1),
    make_xhtml([?XCT(<<"h1">>, <<"Users">>)] ++ Res, Host,
	       Lang, AJID);
process_admin(Host,
	      #request{path = [<<"online-users">>],
		       auth = {_, _Auth, AJID}, lang = Lang})
    when is_binary(Host) ->
    Res = list_online_users(Host, Lang),
    make_xhtml([?XCT(<<"h1">>, <<"Online Users">>)] ++ Res,
	       Host, Lang, AJID);
process_admin(Host,
	      #request{path = [<<"last-activity">>],
		       auth = {_, _Auth, AJID}, q = Query, lang = Lang})
    when is_binary(Host) ->
    ?DEBUG("query: ~p", [Query]),
    Month = case lists:keysearch(<<"period">>, 1, Query) of
	      {value, {_, Val}} -> Val;
	      _ -> <<"month">>
	    end,
    Res = case lists:keysearch(<<"ordinary">>, 1, Query) of
	    {value, {_, _}} ->
		list_last_activity(Host, Lang, false, Month);
	    _ -> list_last_activity(Host, Lang, true, Month)
	  end,
    make_xhtml([?XCT(<<"h1">>, <<"Users Last Activity">>)]
		 ++
		 [?XAE(<<"form">>,
		       [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
		       [?CT(<<"Period: ">>),
			?XAE(<<"select">>, [{<<"name">>, <<"period">>}],
			     (lists:map(fun ({O, V}) ->
						Sel = if O == Month ->
							     [{<<"selected">>,
							       <<"selected">>}];
							 true -> []
						      end,
						?XAC(<<"option">>,
						     (Sel ++
							[{<<"value">>, O}]),
						     V)
					end,
					[{<<"month">>, ?T(<<"Last month">>)},
					 {<<"year">>, ?T(<<"Last year">>)},
					 {<<"all">>,
					  ?T(<<"All activity">>)}]))),
			?C(<<" ">>),
			?INPUTT(<<"submit">>, <<"ordinary">>,
				<<"Show Ordinary Table">>),
			?C(<<" ">>),
			?INPUTT(<<"submit">>, <<"integral">>,
				<<"Show Integral Table">>)])]
		   ++ Res,
	       Host, Lang, AJID);
process_admin(Host,
	      #request{path = [<<"stats">>], auth = {_, _Auth, AJID},
		       lang = Lang}) ->
    Res = get_stats(Host, Lang),
    make_xhtml([?XCT(<<"h1">>, <<"Statistics">>)] ++ Res,
	       Host, Lang, AJID);
process_admin(Host,
	      #request{path = [<<"user">>, U],
		       auth = {_, _Auth, AJID}, q = Query, lang = Lang}) ->
    case ejabberd_auth:is_user_exists(U, Host) of
      true ->
	  Res = user_info(U, Host, Query, Lang),
	  make_xhtml(Res, Host, Lang, AJID);
      false ->
	  make_xhtml([?XCT(<<"h1">>, <<"Not Found">>)], Host,
		     Lang, AJID)
    end;
process_admin(Host,
	      #request{path = [<<"nodes">>], auth = {_, _Auth, AJID},
		       lang = Lang}) ->
    Res = get_nodes(Lang),
    make_xhtml(Res, Host, Lang, AJID);
process_admin(Host,
	      #request{path = [<<"node">>, SNode | NPath],
		       auth = {_, _Auth, AJID}, q = Query, lang = Lang}) ->
    case search_running_node(SNode) of
      false ->
	  make_xhtml([?XCT(<<"h1">>, <<"Node not found">>)], Host,
		     Lang, AJID);
      Node ->
	  Res = get_node(Host, Node, NPath, Query, Lang),
	  make_xhtml(Res, Host, Node, Lang, AJID)
    end;
%%%==================================
%%%% process_admin default case
process_admin(Host,
	      #request{lang = Lang, auth = {_, _Auth, AJID}} =
		  Request) ->
    {Hook, Opts} = case Host of
		     global -> {webadmin_page_main, [Request]};
		     Host -> {webadmin_page_host, [Host, Request]}
		   end,
    case ejabberd_hooks:run_fold(Hook, Host, [], Opts) of
      [] ->
	  setelement(1,
		     make_xhtml([?XC(<<"h1">>, <<"Not Found">>)], Host, Lang,
				AJID),
		     404);
      Res -> make_xhtml(Res, Host, Lang, AJID)
    end.

%%%==================================
%%%% acl

acls_to_xhtml(ACLs) ->
    ?XAE(<<"table">>, [],
	 [?XE(<<"tbody">>,
	      (lists:map(fun ({acl, Name, Spec} = ACL) ->
				 SName = iolist_to_binary(atom_to_list(Name)),
				 ID = term_to_id(ACL),
				 ?XE(<<"tr">>,
				     ([?XE(<<"td">>,
					   [?INPUT(<<"checkbox">>,
						   <<"selected">>, ID)]),
				       ?XC(<<"td">>, SName)]
					++ acl_spec_to_xhtml(ID, Spec)))
			 end,
			 ACLs)
		 ++
		 [?XE(<<"tr">>,
		      ([?X(<<"td">>),
			?XE(<<"td">>,
			    [?INPUT(<<"text">>, <<"namenew">>, <<"">>)])]
			 ++ acl_spec_to_xhtml(<<"new">>, {user, <<"">>})))]))]).

acl_spec_to_text({user, {U, S}}) ->
    {user, <<U/binary, "@", S/binary>>};
acl_spec_to_text({user, U}) -> {user, U};
acl_spec_to_text({server, S}) -> {server, S};
acl_spec_to_text({user_regexp, {RU, S}}) ->
    {user_regexp, <<RU/binary, "@", S/binary>>};
acl_spec_to_text({user_regexp, RU}) ->
    {user_regexp, RU};
acl_spec_to_text({server_regexp, RS}) ->
    {server_regexp, RS};
acl_spec_to_text({node_regexp, {RU, RS}}) ->
    {node_regexp, <<RU/binary, "@", RS/binary>>};
acl_spec_to_text({user_glob, {RU, S}}) ->
    {user_glob, <<RU/binary, "@", S/binary>>};
acl_spec_to_text({user_glob, RU}) -> {user_glob, RU};
acl_spec_to_text({server_glob, RS}) ->
    {server_glob, RS};
acl_spec_to_text({node_glob, {RU, RS}}) ->
    {node_glob, <<RU/binary, "@", RS/binary>>};
acl_spec_to_text(all) -> {all, <<"">>};
acl_spec_to_text(Spec) -> {raw, term_to_string(Spec)}.

acl_spec_to_xhtml(ID, Spec) ->
    {Type, Str} = acl_spec_to_text(Spec),
    [acl_spec_select(ID, Type), ?ACLINPUT(Str)].

acl_spec_select(ID, Opt) ->
    ?XE(<<"td">>,
	[?XAE(<<"select">>,
	      [{<<"name">>, <<"type", ID/binary>>}],
	      (lists:map(fun (O) ->
				 Sel = if O == Opt ->
					      [{<<"selected">>,
						<<"selected">>}];
					  true -> []
				       end,
				 ?XAC(<<"option">>,
				      (Sel ++
					 [{<<"value">>,
					   iolist_to_binary(atom_to_list(O))}]),
				      (iolist_to_binary(atom_to_list(O))))
			 end,
			 [user, server, user_regexp, server_regexp, node_regexp,
			  user_glob, server_glob, node_glob, all, raw])))]).

%% @spec (T::any()) -> StringLine::string()
term_to_string(T) ->
    StringParagraph =
	iolist_to_binary(io_lib:format("~1000000p", [T])),
    ejabberd_regexp:greplace(StringParagraph, <<"\\n ">>,
			     <<"">>).

%% @spec (T::any(), Cols::integer()) -> {NumLines::integer(), Paragraph::string()}
term_to_paragraph(T, Cols) ->
    P1 = erl_syntax:abstract(T),
    P2 = erl_prettypr:format(P1, [{paper, Cols}]),
    Paragraph = list_to_binary(P2),
    FieldList = ejabberd_regexp:split(Paragraph, <<"\n">>),
    NumLines = length(FieldList),
    {NumLines, Paragraph}.

term_to_id(T) -> jlib:encode_base64((term_to_binary(T))).

acl_parse_query(Host, Query) ->
    ACLs = ets:select(acl,
		      [{{acl, {'$1', Host}, '$2'}, [],
			[{{acl, '$1', '$2'}}]}]),
    case lists:keysearch(<<"submit">>, 1, Query) of
      {value, _} -> acl_parse_submit(ACLs, Query);
      _ ->
	  case lists:keysearch(<<"delete">>, 1, Query) of
	    {value, _} -> acl_parse_delete(ACLs, Query)
	  end
    end.

acl_parse_submit(ACLs, Query) ->
    NewACLs = lists:map(fun ({acl, Name, Spec} = ACL) ->
				ID = term_to_id(ACL),
				case {lists:keysearch(<<"type", ID/binary>>, 1,
						      Query),
				      lists:keysearch(<<"value", ID/binary>>, 1,
						      Query)}
				    of
				  {{value, {_, T}}, {value, {_, V}}} ->
				      {Type, Str} = acl_spec_to_text(Spec),
				      case
					{iolist_to_binary(atom_to_list(Type)),
					 Str}
					  of
					{T, V} -> ACL;
					_ ->
					    NewSpec = string_to_spec(T, V),
					    {acl, Name, NewSpec}
				      end;
				  _ -> ACL
				end
			end,
			ACLs),
    NewACL = case {lists:keysearch(<<"namenew">>, 1, Query),
		   lists:keysearch(<<"typenew">>, 1, Query),
		   lists:keysearch(<<"valuenew">>, 1, Query)}
		 of
	       {{value, {_, <<"">>}}, _, _} -> [];
	       {{value, {_, N}}, {value, {_, T}}, {value, {_, V}}} ->
		   NewName = jlib:binary_to_atom(N),
		   NewSpec = string_to_spec(T, V),
		   [{acl, NewName, NewSpec}];
	       _ -> []
	     end,
    NewACLs ++ NewACL.

string_to_spec(<<"user">>, Val) ->
    string_to_spec2(user, Val);
string_to_spec(<<"server">>, Val) -> {server, Val};
string_to_spec(<<"user_regexp">>, Val) ->
    string_to_spec2(user_regexp, Val);
string_to_spec(<<"server_regexp">>, Val) ->
    {server_regexp, Val};
string_to_spec(<<"node_regexp">>, Val) ->
    #jid{luser = U, lserver = S, resource = <<"">>} =
	jid:from_string(Val),
    {node_regexp, U, S};
string_to_spec(<<"user_glob">>, Val) ->
    string_to_spec2(user_glob, Val);
string_to_spec(<<"server_glob">>, Val) ->
    {server_glob, Val};
string_to_spec(<<"node_glob">>, Val) ->
    #jid{luser = U, lserver = S, resource = <<"">>} =
	jid:from_string(Val),
    {node_glob, U, S};
string_to_spec(<<"all">>, _) -> all;
string_to_spec(<<"raw">>, Val) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Val/binary, ".">>)),
    {ok, NewSpec} = erl_parse:parse_term(Tokens),
    NewSpec.

string_to_spec2(ACLName, Val) ->
    #jid{luser = U, lserver = S, resource = <<"">>} =
	jid:from_string(Val),
    case U of
      <<"">> -> {ACLName, S};
      _ -> {ACLName, {U, S}}
    end.

acl_parse_delete(ACLs, Query) ->
    NewACLs = lists:filter(fun ({acl, _Name, _Spec} =
				    ACL) ->
				   ID = term_to_id(ACL),
				   not lists:member({<<"selected">>, ID}, Query)
			   end,
			   ACLs),
    NewACLs.

access_rules_to_xhtml(AccessRules, Lang) ->
    ?XAE(<<"table">>, [],
	 [?XE(<<"tbody">>,
	      (lists:map(fun ({access, Name, Rules} = Access) ->
				 SName = iolist_to_binary(atom_to_list(Name)),
				 ID = term_to_id(Access),
				 ?XE(<<"tr">>,
				     [?XE(<<"td">>,
					  [?INPUT(<<"checkbox">>,
						  <<"selected">>, ID)]),
				      ?XE(<<"td">>,
					  [?AC(<<SName/binary, "/">>, SName)]),
				      ?XC(<<"td">>, (term_to_string(Rules)))])
			 end,
			 lists:keysort(2,AccessRules))
		 ++
		 [?XE(<<"tr">>,
		      [?X(<<"td">>),
		       ?XE(<<"td">>,
			   [?INPUT(<<"text">>, <<"namenew">>, <<"">>)]),
		       ?XE(<<"td">>,
			   [?INPUTT(<<"submit">>, <<"addnew">>,
				    <<"Add New">>)])])]))]).

access_parse_query(Host, Query) ->
    AccessRules = ets:select(access,
			     [{{access, {'$1', Host}, '$2'}, [],
			       [{{access, '$1', '$2'}}]}]),
    case lists:keysearch(<<"addnew">>, 1, Query) of
      {value, _} ->
	  access_parse_addnew(AccessRules, Host, Query);
      _ ->
	  case lists:keysearch(<<"delete">>, 1, Query) of
	    {value, _} ->
		access_parse_delete(AccessRules, Host, Query)
	  end
    end.

access_parse_addnew(_AccessRules, Host, Query) ->
    case lists:keysearch(<<"namenew">>, 1, Query) of
      {value, {_, String}} when String /= <<"">> ->
	  Name = jlib:binary_to_atom(String),
	  ejabberd_config:add_option({access, Name, Host},
					    []),
	  ok
    end.

access_parse_delete(AccessRules, Host, Query) ->
    lists:foreach(fun ({access, Name, _Rules} =
			   AccessRule) ->
			  ID = term_to_id(AccessRule),
			  case lists:member({<<"selected">>, ID}, Query) of
			    true ->
				mnesia:transaction(fun () ->
							   mnesia:delete({access,
									  {Name,
									   Host}})
						   end);
			    _ -> ok
			  end
		  end,
		  AccessRules),
    ok.

access_rule_to_xhtml(Rules) ->
    Text = lists:flatmap(fun ({Access, ACL} = _Rule) ->
				 SAccess = element_to_list(Access),
				 SACL = atom_to_list(ACL),
				 [SAccess, " \t", SACL, "\n"]
			 end,
			 Rules),
    ?XAC(<<"textarea">>,
	 [{<<"name">>, <<"rules">>}, {<<"rows">>, <<"16">>},
	  {<<"cols">>, <<"80">>}],
	 list_to_binary(Text)).

parse_access_rule(Text) ->
    Strings = str:tokens(Text, <<"\r\n">>),
    case catch lists:flatmap(fun (String) ->
				     case str:tokens(String, <<" \t">>) of
				       [Access, ACL] ->
					   [{list_to_element(Access),
					     jlib:binary_to_atom(ACL)}];
				       [] -> []
				     end
			     end,
			     Strings)
	of
      {'EXIT', _Reason} -> error;
      Rs -> {ok, Rs}
    end.

%%%==================================
%%%% list_vhosts

list_vhosts(Lang, JID) ->
    Hosts = (?MYHOSTS),
    HostsAllowed = lists:filter(fun (Host) ->
					is_acl_match(Host,
						     [configure, webadmin_view],
						     JID)
				end,
				Hosts),
    list_vhosts2(Lang, HostsAllowed).

list_vhosts2(Lang, Hosts) ->
    SHosts = lists:sort(Hosts),
    [?XE(<<"table">>,
	 [?XE(<<"thead">>,
	      [?XE(<<"tr">>,
		   [?XCT(<<"td">>, <<"Host">>),
		    ?XCT(<<"td">>, <<"Registered Users">>),
		    ?XCT(<<"td">>, <<"Online Users">>)])]),
	  ?XE(<<"tbody">>,
	      (lists:map(fun (Host) ->
				 OnlineUsers =
				     length(ejabberd_sm:get_vh_session_list(Host)),
				 RegisteredUsers =
				     ejabberd_auth:get_vh_registered_users_number(Host),
				 ?XE(<<"tr">>,
				     [?XE(<<"td">>,
					  [?AC(<<"../server/", Host/binary,
						 "/">>,
					       Host)]),
				      ?XC(<<"td">>,
					  (pretty_string_int(RegisteredUsers))),
				      ?XC(<<"td">>,
					  (pretty_string_int(OnlineUsers)))])
			 end,
			 SHosts)))])].

%%%==================================
%%%% list_users

list_users(Host, Query, Lang, URLFunc) ->
    Res = list_users_parse_query(Query, Host),
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
    FUsers = case length(SUsers) of
	       N when N =< 100 ->
		   [list_given_users(Host, SUsers, <<"../">>, Lang,
				     URLFunc)];
	       N ->
		   NParts = trunc(math:sqrt(N * 6.17999999999999993783e-1))
			      + 1,
		   M = trunc(N / NParts) + 1,
		   lists:flatmap(fun (K) ->
					 L = K + M - 1,
					 Last = if L < N ->
						       su_to_list(lists:nth(L,
									    SUsers));
						   true ->
						       su_to_list(lists:last(SUsers))
						end,
					 Name = <<(su_to_list(lists:nth(K,
									SUsers)))/binary,
						  $\s, 226, 128, 148, $\s,
						  Last/binary>>,
					 [?AC((URLFunc({user_diapason, K, L})),
					      Name),
					  ?BR]
				 end,
				 lists:seq(1, N, M))
	     end,
    case Res of
%% Parse user creation query and try register:
      ok -> [?XREST(<<"Submitted">>)];
      error -> [?XREST(<<"Bad format">>)];
      nothing -> []
    end
      ++
      [?XAE(<<"form">>,
	    [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	    ([?XE(<<"table">>,
		  [?XE(<<"tr">>,
		       [?XC(<<"td">>, <<(?T(<<"User">>))/binary, ":">>),
			?XE(<<"td">>,
			    [?INPUT(<<"text">>, <<"newusername">>, <<"">>)]),
			?XE(<<"td">>, [?C(<<" @ ", Host/binary>>)])]),
		   ?XE(<<"tr">>,
		       [?XC(<<"td">>, <<(?T(<<"Password">>))/binary, ":">>),
			?XE(<<"td">>,
			    [?INPUT(<<"password">>, <<"newuserpassword">>,
				    <<"">>)]),
			?X(<<"td">>)]),
		   ?XE(<<"tr">>,
		       [?X(<<"td">>),
			?XAE(<<"td">>, [{<<"class">>, <<"alignright">>}],
			     [?INPUTT(<<"submit">>, <<"addnewuser">>,
				      <<"Add User">>)]),
			?X(<<"td">>)])]),
	      ?P]
	       ++ FUsers))].

list_users_parse_query(Query, Host) ->
    case lists:keysearch(<<"addnewuser">>, 1, Query) of
      {value, _} ->
	  {value, {_, Username}} =
	      lists:keysearch(<<"newusername">>, 1, Query),
	  {value, {_, Password}} =
	      lists:keysearch(<<"newuserpassword">>, 1, Query),
	  case jid:from_string(<<Username/binary, "@",
				    Host/binary>>)
	      of
	    error -> error;
	    #jid{user = User, server = Server} ->
		case ejabberd_auth:try_register(User, Server, Password)
		    of
		  {error, _Reason} -> error;
		  _ -> ok
		end
	  end;
      false -> nothing
    end.

list_users_in_diapason(Host, Diap, Lang, URLFunc) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
    [S1, S2] = ejabberd_regexp:split(Diap, <<"-">>),
    N1 = jlib:binary_to_integer(S1),
    N2 = jlib:binary_to_integer(S2),
    Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
    [list_given_users(Host, Sub, <<"../../">>, Lang,
		      URLFunc)].

list_given_users(Host, Users, Prefix, Lang, URLFunc) ->
    ModOffline = get_offlinemsg_module(Host),
    ?XE(<<"table">>,
	[?XE(<<"thead">>,
	     [?XE(<<"tr">>,
		  [?XCT(<<"td">>, <<"User">>),
		   ?XCT(<<"td">>, <<"Offline Messages">>),
		   ?XCT(<<"td">>, <<"Last Activity">>)])]),
	 ?XE(<<"tbody">>,
	     (lists:map(fun (_SU = {Server, User}) ->
				US = {User, Server},
				QueueLenStr = get_offlinemsg_length(ModOffline,
								    User,
								    Server),
				FQueueLen = [?AC((URLFunc({users_queue, Prefix,
							   User, Server})),
						 QueueLenStr)],
				FLast = case
					  ejabberd_sm:get_user_resources(User,
									 Server)
					    of
					  [] ->
					      case mod_last:get_last_info(User,
									  Server)
						  of
						not_found -> ?T(<<"Never">>);
						{ok, Shift, _Status} ->
						    TimeStamp = {Shift div
								   1000000,
								 Shift rem
								   1000000,
								 0},
						    {{Year, Month, Day},
						     {Hour, Minute, Second}} =
							calendar:now_to_local_time(TimeStamp),
						    iolist_to_binary(io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
										   [Year,
										    Month,
										    Day,
										    Hour,
										    Minute,
										    Second]))
					      end;
					  _ -> ?T(<<"Online">>)
					end,
				?XE(<<"tr">>,
				    [?XE(<<"td">>,
					 [?AC((URLFunc({user, Prefix,
							ejabberd_http:url_encode(User),
							Server})),
					      (us_to_list(US)))]),
				     ?XE(<<"td">>, FQueueLen),
				     ?XC(<<"td">>, FLast)])
			end,
			Users)))]).

get_offlinemsg_length(ModOffline, User, Server) ->
    case ModOffline of
      none -> <<"disabled">>;
      _ ->
	  pretty_string_int(ModOffline:get_queue_length(User,
							Server))
    end.

get_offlinemsg_module(Server) ->
    case gen_mod:is_loaded(Server, mod_offline) of
      true -> mod_offline;
      false -> none
    end.

get_lastactivity_menuitem_list(Server) ->
    case gen_mod:db_type(Server, mod_last) of
      mnesia -> [{<<"last-activity">>, <<"Last Activity">>}];
      _ -> []
    end.

us_to_list({User, Server}) ->
    jid:to_string({User, Server, <<"">>}).

su_to_list({Server, User}) ->
    jid:to_string({User, Server, <<"">>}).

%%%==================================
%%%% get_stats

get_stats(global, Lang) ->
    OnlineUsers = mnesia:table_info(session, size),
    RegisteredUsers = lists:foldl(fun (Host, Total) ->
					  ejabberd_auth:get_vh_registered_users_number(Host)
					    + Total
				  end,
				  0, ?MYHOSTS),
    OutS2SNumber = ejabberd_s2s:outgoing_s2s_number(),
    InS2SNumber = ejabberd_s2s:incoming_s2s_number(),
    [?XAE(<<"table">>, [],
	  [?XE(<<"tbody">>,
	       [?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Registered Users:">>),
		     ?XC(<<"td">>, (pretty_string_int(RegisteredUsers)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Online Users:">>),
		     ?XC(<<"td">>, (pretty_string_int(OnlineUsers)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Outgoing s2s Connections:">>),
		     ?XC(<<"td">>, (pretty_string_int(OutS2SNumber)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Incoming s2s Connections:">>),
		     ?XC(<<"td">>, (pretty_string_int(InS2SNumber)))])])])];
get_stats(Host, Lang) ->
    OnlineUsers =
	length(ejabberd_sm:get_vh_session_list(Host)),
    RegisteredUsers =
	ejabberd_auth:get_vh_registered_users_number(Host),
    [?XAE(<<"table">>, [],
	  [?XE(<<"tbody">>,
	       [?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Registered Users:">>),
		     ?XC(<<"td">>, (pretty_string_int(RegisteredUsers)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Online Users:">>),
		     ?XC(<<"td">>, (pretty_string_int(OnlineUsers)))])])])].

list_online_users(Host, _Lang) ->
    Users = [{S, U}
	     || {U, S, _R} <- ejabberd_sm:get_vh_session_list(Host)],
    SUsers = lists:usort(Users),
    lists:flatmap(fun ({_S, U} = SU) ->
			  [?AC(<<"../user/",
				 (ejabberd_http:url_encode(U))/binary, "/">>,
			       (su_to_list(SU))),
			   ?BR]
		  end,
		  SUsers).

user_info(User, Server, Query, Lang) ->
    LServer = jid:nameprep(Server),
    US = {jid:nodeprep(User), LServer},
    Res = user_parse_query(User, Server, Query),
    Resources = ejabberd_sm:get_user_resources(User,
					       Server),
    FResources =
        case Resources of
            [] -> [?CT(<<"None">>)];
            _ ->
                [?XE(<<"ul">>,
                     (lists:map(
                        fun (R) ->
                                FIP = case
                                          ejabberd_sm:get_user_info(User,
                                                                    Server,
                                                                    R)
                                      of
                                          offline -> <<"">>;
                                          Info
                                            when
                                                is_list(Info) ->
                                              Node =
                                                  proplists:get_value(node,
                                                                      Info),
                                              Conn =
                                                  proplists:get_value(conn,
                                                                      Info),
                                              {IP, Port} =
                                                  proplists:get_value(ip,
                                                                      Info),
                                              ConnS = case Conn of
                                                          c2s ->
                                                              <<"plain">>;
                                                          c2s_tls ->
                                                              <<"tls">>;
                                                          c2s_compressed ->
                                                              <<"zlib">>;
                                                          c2s_compressed_tls ->
                                                              <<"tls+zlib">>;
                                                          http_bind ->
                                                              <<"http-bind">>;
                                                          websocket ->
                                                              <<"websocket">>;
                                                          _ ->
                                                              <<"unknown">>
                                                      end,
                                              <<ConnS/binary,
                                                "://",
                                                (jlib:ip_to_list(IP))/binary,
                                                ":",
                                                (jlib:integer_to_binary(Port))/binary,
                                                "#",
                                                (jlib:atom_to_binary(Node))/binary>>
                                      end,
                                case direction(Lang) of
				    [{_, <<"rtl">>}] -> ?LI([?C((<<FIP/binary, " - ", R/binary>>))]);
				    _ -> ?LI([?C((<<R/binary, " - ", FIP/binary>>))])
                                end
                        end,
                        lists:sort(Resources))))]
        end,
    FPassword = [?INPUT(<<"text">>, <<"password">>, <<"">>),
		 ?C(<<" ">>),
		 ?INPUTT(<<"submit">>, <<"chpassword">>,
			 <<"Change Password">>)],
    UserItems = ejabberd_hooks:run_fold(webadmin_user,
					LServer, [], [User, Server, Lang]),
    LastActivity = case ejabberd_sm:get_user_resources(User,
						       Server)
		       of
		     [] ->
			 case mod_last:get_last_info(User, Server) of
			   not_found -> ?T(<<"Never">>);
			   {ok, Shift, _Status} ->
			       TimeStamp = {Shift div 1000000,
					    Shift rem 1000000, 0},
			       {{Year, Month, Day}, {Hour, Minute, Second}} =
				   calendar:now_to_local_time(TimeStamp),
			       iolist_to_binary(io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
							      [Year, Month, Day,
							       Hour, Minute,
							       Second]))
			 end;
		     _ -> ?T(<<"Online">>)
		   end,
    [?XC(<<"h1">>, list_to_binary(io_lib:format(?T(<<"User ~s">>),
                                                [us_to_list(US)])))]
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	error -> [?XREST(<<"Bad format">>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      ([?XCT(<<"h3">>, <<"Connected Resources:">>)] ++
		 FResources ++
		   [?XCT(<<"h3">>, <<"Password:">>)] ++
		     FPassword ++
		       [?XCT(<<"h3">>, <<"Last Activity">>)] ++
			 [?C(LastActivity)] ++
			   UserItems ++
			     [?P,
			      ?INPUTT(<<"submit">>, <<"removeuser">>,
				      <<"Remove User">>)]))].

user_parse_query(User, Server, Query) ->
    lists:foldl(fun ({Action, _Value}, Acc)
			when Acc == nothing ->
			user_parse_query1(Action, User, Server, Query);
		    ({_Action, _Value}, Acc) -> Acc
		end,
		nothing, Query).

user_parse_query1(<<"password">>, _User, _Server,
		  _Query) ->
    nothing;
user_parse_query1(<<"chpassword">>, User, Server,
		  Query) ->
    case lists:keysearch(<<"password">>, 1, Query) of
      {value, {_, Password}} ->
	  ejabberd_auth:set_password(User, Server, Password), ok;
      _ -> error
    end;
user_parse_query1(<<"removeuser">>, User, Server,
		  _Query) ->
    ejabberd_auth:remove_user(User, Server), ok;
user_parse_query1(Action, User, Server, Query) ->
    case ejabberd_hooks:run_fold(webadmin_user_parse_query,
				 Server, [], [Action, User, Server, Query])
	of
      [] -> nothing;
      Res -> Res
    end.

list_last_activity(Host, Lang, Integral, Period) ->
    TimeStamp = p1_time_compat:system_time(seconds),
    case Period of
      <<"all">> -> TS = 0, Days = infinity;
      <<"year">> -> TS = TimeStamp - 366 * 86400, Days = 366;
      _ -> TS = TimeStamp - 31 * 86400, Days = 31
    end,
    case catch mnesia:dirty_select(last_activity,
				   [{{last_activity, {'_', Host}, '$1', '_'},
				     [{'>', '$1', TS}],
				     [{trunc,
				       {'/', {'-', TimeStamp, '$1'}, 86400}}]}])
	of
      {'EXIT', _Reason} -> [];
      Vals ->
	  Hist = histogram(Vals, Integral),
	  if Hist == [] -> [?CT(<<"No Data">>)];
	     true ->
		 Left = if Days == infinity -> 0;
			   true -> Days - length(Hist)
			end,
		 Tail = if Integral ->
			       lists:duplicate(Left, lists:last(Hist));
			   true -> lists:duplicate(Left, 0)
			end,
		 Max = lists:max(Hist),
		 [?XAE(<<"ol">>,
		       [{<<"id">>, <<"lastactivity">>},
			{<<"start">>, <<"0">>}],
		       [?XAE(<<"li">>,
			     [{<<"style">>,
			       <<"width:",
				 (iolist_to_binary(integer_to_list(trunc(90 * V
									   /
									   Max))))/binary,
				 "%;">>}],
			     [{xmlcdata, pretty_string_int(V)}])
			|| V <- Hist ++ Tail])]
	  end
    end.

histogram(Values, Integral) ->
    histogram(lists:sort(Values), Integral, 0, 0, []).

histogram([H | T], Integral, Current, Count, Hist)
    when Current == H ->
    histogram(T, Integral, Current, Count + 1, Hist);
histogram([H | _] = Values, Integral, Current, Count,
	  Hist)
    when Current < H ->
    if Integral ->
	   histogram(Values, Integral, Current + 1, Count,
		     [Count | Hist]);
       true ->
	   histogram(Values, Integral, Current + 1, 0,
		     [Count | Hist])
    end;
histogram([], _Integral, _Current, Count, Hist) ->
    if Count > 0 -> lists:reverse([Count | Hist]);
       true -> lists:reverse(Hist)
    end.

%%%==================================
%%%% get_nodes

get_nodes(Lang) ->
    RunningNodes = mnesia:system_info(running_db_nodes),
    StoppedNodes = lists:usort(mnesia:system_info(db_nodes)
				 ++ mnesia:system_info(extra_db_nodes))
		     -- RunningNodes,
    FRN = if RunningNodes == [] -> ?CT(<<"None">>);
	     true ->
		 ?XE(<<"ul">>,
		     (lists:map(fun (N) ->
					S = iolist_to_binary(atom_to_list(N)),
					?LI([?AC(<<"../node/", S/binary, "/">>,
						 S)])
				end,
				lists:sort(RunningNodes))))
	  end,
    FSN = if StoppedNodes == [] -> ?CT(<<"None">>);
	     true ->
		 ?XE(<<"ul">>,
		     (lists:map(fun (N) ->
					S = iolist_to_binary(atom_to_list(N)),
					?LI([?C(S)])
				end,
				lists:sort(StoppedNodes))))
	  end,
    [?XCT(<<"h1">>, <<"Nodes">>),
     ?XCT(<<"h3">>, <<"Running Nodes">>), FRN,
     ?XCT(<<"h3">>, <<"Stopped Nodes">>), FSN].

search_running_node(SNode) ->
    search_running_node(SNode,
			mnesia:system_info(running_db_nodes)).

search_running_node(_, []) -> false;
search_running_node(SNode, [Node | Nodes]) ->
    case iolist_to_binary(atom_to_list(Node)) of
      SNode -> Node;
      _ -> search_running_node(SNode, Nodes)
    end.

get_node(global, Node, [], Query, Lang) ->
    Res = node_parse_query(Node, Query),
    Base = get_base_path(global, Node),
    MenuItems2 = make_menu_items(global, Node, Base, Lang),
    [?XC(<<"h1">>,
	 list_to_binary(io_lib:format(?T(<<"Node ~p">>), [Node])))]
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	error -> [?XREST(<<"Bad format">>)];
	nothing -> []
      end
	++
	[?XE(<<"ul">>,
	     ([?LI([?ACT(<<Base/binary, "db/">>, <<"Database">>)]),
	       ?LI([?ACT(<<Base/binary, "backup/">>, <<"Backup">>)]),
	       ?LI([?ACT(<<Base/binary, "ports/">>,
			 <<"Listened Ports">>)]),
	       ?LI([?ACT(<<Base/binary, "stats/">>,
			 <<"Statistics">>)]),
	       ?LI([?ACT(<<Base/binary, "update/">>, <<"Update">>)])]
		++ MenuItems2)),
	 ?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [?INPUTT(<<"submit">>, <<"restart">>, <<"Restart">>),
	       ?C(<<" ">>),
	       ?INPUTT(<<"submit">>, <<"stop">>, <<"Stop">>)])];
get_node(Host, Node, [], _Query, Lang) ->
    Base = get_base_path(Host, Node),
    MenuItems2 = make_menu_items(Host, Node, Base, Lang),
    [?XC(<<"h1">>, list_to_binary(io_lib:format(?T(<<"Node ~p">>), [Node]))),
     ?XE(<<"ul">>,
	 ([?LI([?ACT(<<Base/binary, "modules/">>,
		     <<"Modules">>)])]
	    ++ MenuItems2))];
get_node(global, Node, [<<"db">>], Query, Lang) ->
    case ejabberd_cluster:call(Node, mnesia, system_info, [tables]) of
      {badrpc, _Reason} ->
	  [?XCT(<<"h1">>, <<"RPC Call Error">>)];
      Tables ->
	  ResS = case node_db_parse_query(Node, Tables, Query) of
		   nothing -> [];
		   ok -> [?XREST(<<"Submitted">>)]
		 end,
	  STables = lists:sort(Tables),
	  Rows = lists:map(fun (Table) ->
				   STable =
				       iolist_to_binary(atom_to_list(Table)),
				   TInfo = case ejabberd_cluster:call(Node, mnesia,
							 table_info,
							 [Table, all])
					       of
					     {badrpc, _} -> [];
					     I -> I
					   end,
				   {Type, Size, Memory} = case
							    {lists:keysearch(storage_type,
									     1,
									     TInfo),
							     lists:keysearch(size,
									     1,
									     TInfo),
							     lists:keysearch(memory,
									     1,
									     TInfo)}
							      of
							    {{value,
							      {storage_type,
							       T}},
							     {value, {size, S}},
							     {value,
							      {memory, M}}} ->
								{T, S, M};
							    _ -> {unknown, 0, 0}
							  end,
				   ?XE(<<"tr">>,
				       [?XC(<<"td">>, STable),
					?XE(<<"td">>,
					    [db_storage_select(STable, Type,
							       Lang)]),
					?XAC(<<"td">>,
					     [{<<"class">>, <<"alignright">>}],
					     (pretty_string_int(Size))),
					?XAC(<<"td">>,
					     [{<<"class">>, <<"alignright">>}],
					     (pretty_string_int(Memory)))])
			   end,
			   STables),
	  [?XC(<<"h1">>,
	       list_to_binary(io_lib:format(?T(<<"Database Tables at ~p">>),
                                            [Node]))
	  )]
	    ++
	    ResS ++
	      [?XAE(<<"form">>,
		    [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
		    [?XAE(<<"table">>, [],
			  [?XE(<<"thead">>,
			       [?XE(<<"tr">>,
				    [?XCT(<<"td">>, <<"Name">>),
				     ?XCT(<<"td">>, <<"Storage Type">>),
				     ?XCT(<<"td">>, <<"Elements">>),
				     ?XCT(<<"td">>, <<"Memory">>)])]),
			   ?XE(<<"tbody">>,
			       (Rows ++
				  [?XE(<<"tr">>,
				       [?XAE(<<"td">>,
					     [{<<"colspan">>, <<"4">>},
					      {<<"class">>, <<"alignright">>}],
					     [?INPUTT(<<"submit">>,
						      <<"submit">>,
						      <<"Submit">>)])])]))])])]
    end;
get_node(global, Node, [<<"backup">>], Query, Lang) ->
    HomeDirRaw = case {os:getenv("HOME"), os:type()} of
		   {EnvHome, _} when is_list(EnvHome) -> list_to_binary(EnvHome);
		   {false, {win32, _Osname}} -> <<"C:/">>;
		   {false, _} -> <<"/tmp/">>
		 end,
    HomeDir = filename:nativename(HomeDirRaw),
    ResS = case node_backup_parse_query(Node, Query) of
	     nothing -> [];
	     ok -> [?XREST(<<"Submitted">>)];
	     {error, Error} ->
		 [?XRES(<<(?T(<<"Error">>))/binary, ": ",
			  (list_to_binary(io_lib:format("~p", [Error])))/binary>>)]
	   end,
    [?XC(<<"h1">>, list_to_binary(io_lib:format(?T(<<"Backup of ~p">>), [Node])))]
      ++
      ResS ++
	[?XCT(<<"p">>,
	      <<"Please note that these options will "
		"only backup the builtin Mnesia database. "
		"If you are using the ODBC module, you "
		"also need to backup your SQL database "
		"separately.">>),
	 ?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [?XAE(<<"table">>, [],
		    [?XE(<<"tbody">>,
			 [?XE(<<"tr">>,
			      [?XCT(<<"td">>, <<"Store binary backup:">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"storepath">>,
					   (filename:join(HomeDir,
							  "ejabberd.backup")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"store">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    <<"Restore binary backup immediately:">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"restorepath">>,
					   (filename:join(HomeDir,
							  "ejabberd.backup")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"restore">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    <<"Restore binary backup after next ejabberd "
				      "restart (requires less memory):">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"fallbackpath">>,
					   (filename:join(HomeDir,
							  "ejabberd.backup")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"fallback">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>, <<"Store plain text backup:">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"dumppath">>,
					   (filename:join(HomeDir,
							  "ejabberd.dump")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"dump">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    <<"Restore plain text backup immediately:">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"loadpath">>,
					   (filename:join(HomeDir,
							  "ejabberd.dump")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"load">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    <<"Import users data from a PIEFXIS file "
				      "(XEP-0227):">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>,
					   <<"import_piefxis_filepath">>,
					   (filename:join(HomeDir,
							  "users.xml")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>,
					    <<"import_piefxis_file">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    <<"Export data of all users in the server "
				      "to PIEFXIS files (XEP-0227):">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>,
					   <<"export_piefxis_dirpath">>,
					   HomeDir)]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>,
					    <<"export_piefxis_dir">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XE(<<"td">>,
				   [?CT(<<"Export data of users in a host to PIEFXIS "
					  "files (XEP-0227):">>),
				    ?C(<<" ">>),
				    ?INPUT(<<"text">>,
					   <<"export_piefxis_host_dirhost">>,
					   (?MYNAME))]),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>,
					   <<"export_piefxis_host_dirpath">>,
					   HomeDir)]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>,
					    <<"export_piefxis_host_dir">>,
					    <<"OK">>)])]),
                          ?XE(<<"tr">>,
                              [?XE(<<"td">>,
                                   [?CT(<<"Export all tables as SQL queries "
                                          "to a file:">>),
                                    ?C(<<" ">>),
                                    ?INPUT(<<"text">>,
                                           <<"export_sql_filehost">>,
                                           (?MYNAME))]),
                               ?XE(<<"td">>,
				   [?INPUT(<<"text">>,
                                           <<"export_sql_filepath">>,
					   (filename:join(HomeDir,
							  "db.sql")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"export_sql_file">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    <<"Import user data from jabberd14 spool "
				      "file:">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"import_filepath">>,
					   (filename:join(HomeDir,
							  "user1.xml")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"import_file">>,
					    <<"OK">>)])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    <<"Import users data from jabberd14 spool "
				      "directory:">>),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"import_dirpath">>,
					   <<"/var/spool/jabber/">>)]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"import_dir">>,
					    <<"OK">>)])])])])])];
get_node(global, Node, [<<"ports">>], Query, Lang) ->
    Ports = ejabberd_cluster:call(Node, ejabberd_config,
		     get_local_option, [listen,
                                        {ejabberd_listener, validate_cfg},
                                        []]),
    Res = case catch node_ports_parse_query(Node, Ports,
					    Query)
	      of
	    submitted -> ok;
	    {'EXIT', _Reason} -> error;
	    {is_added, ok} -> ok;
	    {is_added, {error, Reason}} ->
		{error, iolist_to_binary(io_lib:format("~p", [Reason]))};
	    _ -> nothing
	  end,
    NewPorts = lists:sort(ejabberd_cluster:call(Node, ejabberd_config,
				   get_local_option,
                                   [listen,
                                    {ejabberd_listener, validate_cfg},
                                    []])),
    H1String = <<(?T(<<"Listened Ports at ">>))/binary,
		 (iolist_to_binary(atom_to_list(Node)))/binary>>,
    (?H1GL(H1String, <<"listening-ports">>, <<"Listening Ports">>))
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	error -> [?XREST(<<"Bad format">>)];
	{error, ReasonT} ->
	    [?XRES(<<(?T(<<"Error">>))/binary, ": ",
		     ReasonT/binary>>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [node_ports_to_xhtml(NewPorts, Lang)])];
get_node(Host, Node, [<<"modules">>], Query, Lang)
    when is_binary(Host) ->
    Modules = ejabberd_cluster:call(Node, gen_mod,
		       loaded_modules_with_opts, [Host]),
    Res = case catch node_modules_parse_query(Host, Node,
					      Modules, Query)
	      of
	    submitted -> ok;
	    {'EXIT', Reason} -> ?INFO_MSG("~p~n", [Reason]), error;
	    _ -> nothing
	  end,
    NewModules = lists:sort(ejabberd_cluster:call(Node, gen_mod,
				     loaded_modules_with_opts, [Host])),
    H1String = list_to_binary(io_lib:format(?T(<<"Modules at ~p">>), [Node])),
    (?H1GL(H1String, <<"modules-overview">>,
	   <<"Modules Overview">>))
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	error -> [?XREST(<<"Bad format">>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [node_modules_to_xhtml(NewModules, Lang)])];
get_node(global, Node, [<<"stats">>], _Query, Lang) ->
    UpTime = ejabberd_cluster:call(Node, erlang, statistics,
		      [wall_clock]),
    UpTimeS = list_to_binary(io_lib:format("~.3f",
                                           [element(1, UpTime) / 1000])),
    CPUTime = ejabberd_cluster:call(Node, erlang, statistics, [runtime]),
    CPUTimeS = list_to_binary(io_lib:format("~.3f",
                                            [element(1, CPUTime) / 1000])),
    OnlineUsers = mnesia:table_info(session, size),
    TransactionsCommitted = ejabberd_cluster:call(Node, mnesia,
				     system_info, [transaction_commits]),
    TransactionsAborted = ejabberd_cluster:call(Node, mnesia,
				   system_info, [transaction_failures]),
    TransactionsRestarted = ejabberd_cluster:call(Node, mnesia,
				     system_info, [transaction_restarts]),
    TransactionsLogged = ejabberd_cluster:call(Node, mnesia, system_info,
				  [transaction_log_writes]),
    [?XC(<<"h1">>,
	 list_to_binary(io_lib:format(?T(<<"Statistics of ~p">>), [Node]))),
     ?XAE(<<"table">>, [],
	  [?XE(<<"tbody">>,
	       [?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Uptime:">>),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  UpTimeS)]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"CPU Time:">>),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  CPUTimeS)]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Online Users:">>),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(OnlineUsers)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Transactions Committed:">>),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(TransactionsCommitted)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Transactions Aborted:">>),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(TransactionsAborted)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Transactions Restarted:">>),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(TransactionsRestarted)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, <<"Transactions Logged:">>),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(TransactionsLogged)))])])])];
get_node(global, Node, [<<"update">>], Query, Lang) ->
    ejabberd_cluster:call(Node, code, purge, [ejabberd_update]),
    Res = node_update_parse_query(Node, Query),
    ejabberd_cluster:call(Node, code, load_file, [ejabberd_update]),
    {ok, _Dir, UpdatedBeams, Script, LowLevelScript,
     Check} =
	ejabberd_cluster:call(Node, ejabberd_update, update_info, []),
    Mods = case UpdatedBeams of
	     [] -> ?CT(<<"None">>);
	     _ ->
		 BeamsLis = lists:map(fun (Beam) ->
					      BeamString =
						  iolist_to_binary(atom_to_list(Beam)),
					      ?LI([?INPUT(<<"checkbox">>,
							  <<"selected">>,
							  BeamString),
						   ?C(BeamString)])
				      end,
				      UpdatedBeams),
		 SelectButtons = [?BR,
				  ?INPUTATTRS(<<"button">>, <<"selectall">>,
					      <<"Select All">>,
					      [{<<"onClick">>,
						<<"selectAll()">>}]),
				  ?C(<<" ">>),
				  ?INPUTATTRS(<<"button">>, <<"unselectall">>,
					      <<"Unselect All">>,
					      [{<<"onClick">>,
						<<"unSelectAll()">>}])],
		 ?XAE(<<"ul">>, [{<<"class">>, <<"nolistyle">>}],
		      (BeamsLis ++ SelectButtons))
	   end,
    FmtScript = (?XC(<<"pre">>,
		     list_to_binary(io_lib:format("~p", [Script])))),
    FmtLowLevelScript = (?XC(<<"pre">>,
			     list_to_binary(io_lib:format("~p", [LowLevelScript])))),
    [?XC(<<"h1">>,
	 list_to_binary(io_lib:format(?T(<<"Update ~p">>), [Node])))]
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	{error, ErrorText} ->
	    [?XREST(<<"Error: ", ErrorText/binary>>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [?XCT(<<"h2">>, <<"Update plan">>),
	       ?XCT(<<"h3">>, <<"Modified modules">>), Mods,
	       ?XCT(<<"h3">>, <<"Update script">>), FmtScript,
	       ?XCT(<<"h3">>, <<"Low level update script">>),
	       FmtLowLevelScript, ?XCT(<<"h3">>, <<"Script check">>),
	       ?XC(<<"pre">>, (jlib:atom_to_binary(Check))),
	       ?BR,
	       ?INPUTT(<<"submit">>, <<"update">>, <<"Update">>)])];
get_node(Host, Node, NPath, Query, Lang) ->
    {Hook, Opts} = case Host of
		     global ->
			 {webadmin_page_node, [Node, NPath, Query, Lang]};
		     Host ->
			 {webadmin_page_hostnode,
			  [Host, Node, NPath, Query, Lang]}
		   end,
    case ejabberd_hooks:run_fold(Hook, Host, [], Opts) of
      [] -> [?XC(<<"h1">>, <<"Not Found">>)];
      Res -> Res
    end.

%%%==================================
%%%% node parse

node_parse_query(Node, Query) ->
    case lists:keysearch(<<"restart">>, 1, Query) of
      {value, _} ->
	  case ejabberd_cluster:call(Node, init, restart, []) of
	    {badrpc, _Reason} -> error;
	    _ -> ok
	  end;
      _ ->
	  case lists:keysearch(<<"stop">>, 1, Query) of
	    {value, _} ->
		case ejabberd_cluster:call(Node, init, stop, []) of
		  {badrpc, _Reason} -> error;
		  _ -> ok
		end;
	    _ -> nothing
	  end
    end.

db_storage_select(ID, Opt, Lang) ->
    ?XAE(<<"select">>,
	 [{<<"name">>, <<"table", ID/binary>>}],
	 (lists:map(fun ({O, Desc}) ->
			    Sel = if O == Opt ->
					 [{<<"selected">>, <<"selected">>}];
				     true -> []
				  end,
			    ?XACT(<<"option">>,
				  (Sel ++
				     [{<<"value">>,
				       iolist_to_binary(atom_to_list(O))}]),
				  Desc)
		    end,
		    [{ram_copies, <<"RAM copy">>},
		     {disc_copies, <<"RAM and disc copy">>},
		     {disc_only_copies, <<"Disc only copy">>},
		     {unknown, <<"Remote copy">>},
		     {delete_content, <<"Delete content">>},
		     {delete_table, <<"Delete table">>}]))).

node_db_parse_query(_Node, _Tables, [{nokey, <<>>}]) ->
    nothing;
node_db_parse_query(Node, Tables, Query) ->
    lists:foreach(fun (Table) ->
			  STable = iolist_to_binary(atom_to_list(Table)),
			  case lists:keysearch(<<"table", STable/binary>>, 1,
					       Query)
			      of
			    {value, {_, SType}} ->
				Type = case SType of
					 <<"unknown">> -> unknown;
					 <<"ram_copies">> -> ram_copies;
					 <<"disc_copies">> -> disc_copies;
					 <<"disc_only_copies">> ->
					     disc_only_copies;
					 <<"delete_content">> -> delete_content;
					 <<"delete_table">> -> delete_table;
					 _ -> false
				       end,
				if Type == false -> ok;
				   Type == delete_content ->
				       mnesia:clear_table(Table);
				   Type == delete_table ->
				       mnesia:delete_table(Table);
				   Type == unknown ->
				       mnesia:del_table_copy(Table, Node);
				   true ->
				       case mnesia:add_table_copy(Table, Node,
								  Type)
					   of
					 {aborted, _} ->
					     mnesia:change_table_copy_type(Table,
									   Node,
									   Type);
					 _ -> ok
				       end
				end;
			    _ -> ok
			  end
		  end,
		  Tables),
    ok.

node_backup_parse_query(_Node, [{nokey, <<>>}]) ->
    nothing;
node_backup_parse_query(Node, Query) ->
    lists:foldl(fun (Action, nothing) ->
			case lists:keysearch(Action, 1, Query) of
			  {value, _} ->
			      case lists:keysearch(<<Action/binary, "path">>, 1,
						   Query)
				  of
				{value, {_, Path}} ->
				    Res = case Action of
					    <<"store">> ->
						ejabberd_cluster:call(Node, mnesia, backup,
							 [binary_to_list(Path)]);
					    <<"restore">> ->
						ejabberd_cluster:call(Node, ejabberd_admin,
							 restore, [Path]);
					    <<"fallback">> ->
						ejabberd_cluster:call(Node, mnesia,
							 install_fallback,
							 [binary_to_list(Path)]);
					    <<"dump">> ->
						ejabberd_cluster:call(Node, ejabberd_admin,
							 dump_to_textfile,
							 [Path]);
					    <<"load">> ->
						ejabberd_cluster:call(Node, mnesia,
							 load_textfile,
                                                         [binary_to_list(Path)]);
					    <<"import_piefxis_file">> ->
						ejabberd_cluster:call(Node, ejabberd_piefxis,
							 import_file, [Path]);
					    <<"export_piefxis_dir">> ->
						ejabberd_cluster:call(Node, ejabberd_piefxis,
							 export_server, [Path]);
					    <<"export_piefxis_host_dir">> ->
						{value, {_, Host}} =
						    lists:keysearch(<<Action/binary,
								      "host">>,
								    1, Query),
						ejabberd_cluster:call(Node, ejabberd_piefxis,
							 export_host,
							 [Path, Host]);
                                            <<"export_sql_file">> ->
                                                {value, {_, Host}} =
                                                    lists:keysearch(<<Action/binary,
                                                                      "host">>,
                                                                    1, Query),
                                                ejabberd_cluster:call(Node, ejd2odbc,
                                                         export, [Host, Path]);
					    <<"import_file">> ->
						ejabberd_cluster:call(Node, ejabberd_admin,
							 import_file, [Path]);
					    <<"import_dir">> ->
						ejabberd_cluster:call(Node, ejabberd_admin,
							 import_dir, [Path])
					  end,
				    case Res of
				      {error, Reason} -> {error, Reason};
				      {badrpc, Reason} -> {badrpc, Reason};
				      _ -> ok
				    end;
				OtherError -> {error, OtherError}
			      end;
			  _ -> nothing
			end;
		    (_Action, Res) -> Res
		end,
		nothing,
		[<<"store">>, <<"restore">>, <<"fallback">>, <<"dump">>,
		 <<"load">>, <<"import_file">>, <<"import_dir">>,
		 <<"import_piefxis_file">>, <<"export_piefxis_dir">>,
		 <<"export_piefxis_host_dir">>, <<"export_sql_file">>]).

node_ports_to_xhtml(Ports, Lang) ->
    ?XAE(<<"table">>, [{<<"class">>, <<"withtextareas">>}],
	 [?XE(<<"thead">>,
	      [?XE(<<"tr">>,
		   [?XCT(<<"td">>, <<"Port">>), ?XCT(<<"td">>, <<"IP">>),
		    ?XCT(<<"td">>, <<"Protocol">>),
		    ?XCT(<<"td">>, <<"Module">>),
		    ?XCT(<<"td">>, <<"Options">>)])]),
	  ?XE(<<"tbody">>,
	      (lists:map(fun ({PortIP, Module, Opts} = _E) ->
				 {_Port, SPort, _TIP, SIP, SSPort, NetProt,
				  OptsClean} =
				     get_port_data(PortIP, Opts),
				 SModule =
				     iolist_to_binary(atom_to_list(Module)),
				 {NumLines, SOptsClean} =
				     term_to_paragraph(OptsClean, 40),
				 ?XE(<<"tr">>,
				     [?XAE(<<"td">>, [{<<"size">>, <<"6">>}],
					   [?C(SPort)]),
				      ?XAE(<<"td">>, [{<<"size">>, <<"15">>}],
					   [?C(SIP)]),
				      ?XAE(<<"td">>, [{<<"size">>, <<"4">>}],
					   [?C((iolist_to_binary(atom_to_list(NetProt))))]),
				      ?XE(<<"td">>,
					  [?INPUTS(<<"text">>,
						   <<"module", SSPort/binary>>,
						   SModule, <<"15">>)]),
				      ?XAE(<<"td">>, direction(ltr),
					  [?TEXTAREA(<<"opts", SSPort/binary>>,
						     (iolist_to_binary(integer_to_list(NumLines))),
						     <<"35">>, SOptsClean)]),
				      ?XE(<<"td">>,
					  [?INPUTT(<<"submit">>,
						   <<"add", SSPort/binary>>,
						   <<"Restart">>)]),
				      ?XE(<<"td">>,
					  [?INPUTT(<<"submit">>,
						   <<"delete", SSPort/binary>>,
						   <<"Stop">>)])])
			 end,
			 Ports)
		 ++
		 [?XE(<<"tr">>,
		      [?XE(<<"td">>,
			   [?INPUTS(<<"text">>, <<"portnew">>, <<"">>,
				    <<"6">>)]),
		       ?XE(<<"td">>,
			   [?INPUTS(<<"text">>, <<"ipnew">>, <<"0.0.0.0">>,
				    <<"15">>)]),
		       ?XE(<<"td">>, [make_netprot_html(<<"tcp">>)]),
		       ?XE(<<"td">>,
			   [?INPUTS(<<"text">>, <<"modulenew">>, <<"">>,
				    <<"15">>)]),
		       ?XAE(<<"td">>, direction(ltr),
			   [?TEXTAREA(<<"optsnew">>, <<"2">>, <<"35">>,
				      <<"[]">>)]),
		       ?XAE(<<"td">>, [{<<"colspan">>, <<"2">>}],
			    [?INPUTT(<<"submit">>, <<"addnew">>,
				     <<"Start">>)])])]))]).

make_netprot_html(NetProt) ->
    ?XAE(<<"select">>, [{<<"name">>, <<"netprotnew">>}],
	 (lists:map(fun (O) ->
			    Sel = if O == NetProt ->
					 [{<<"selected">>, <<"selected">>}];
				     true -> []
				  end,
			    ?XAC(<<"option">>, (Sel ++ [{<<"value">>, O}]), O)
		    end,
		    [<<"tcp">>, <<"udp">>]))).

get_port_data(PortIP, Opts) ->
    {Port, IPT, IPS, _IPV, NetProt, OptsClean} =
	ejabberd_listener:parse_listener_portip(PortIP, Opts),
    SPort = jlib:integer_to_binary(Port),
    SSPort = list_to_binary(
               lists:map(fun (N) ->
                                 io_lib:format("~.16b", [N])
                         end,
                         binary_to_list(
                           erlang:md5(
                             [SPort, IPS, atom_to_list(NetProt)])))),
    {Port, SPort, IPT, IPS, SSPort, NetProt, OptsClean}.

node_ports_parse_query(Node, Ports, Query) ->
    lists:foreach(fun ({PortIpNetp, Module1, Opts1}) ->
			  {Port, _SPort, TIP, _SIP, SSPort, NetProt,
			   _OptsClean} =
			      get_port_data(PortIpNetp, Opts1),
			  case lists:keysearch(<<"add", SSPort/binary>>, 1,
					       Query)
			      of
			    {value, _} ->
				PortIpNetp2 = {Port, TIP, NetProt},
				{{value, {_, SModule}}, {value, {_, SOpts}}} =
				    {lists:keysearch(<<"module",
						       SSPort/binary>>,
						     1, Query),
				     lists:keysearch(<<"opts", SSPort/binary>>,
						     1, Query)},
				Module = jlib:binary_to_atom(SModule),
				{ok, Tokens, _} =
				    erl_scan:string(binary_to_list(SOpts) ++ "."),
				{ok, Opts} = erl_parse:parse_term(Tokens),
				ejabberd_cluster:call(Node, ejabberd_listener,
					 delete_listener,
					 [PortIpNetp2, Module1]),
				R = ejabberd_cluster:call(Node, ejabberd_listener,
					     add_listener,
					     [PortIpNetp2, Module, Opts]),
				throw({is_added, R});
			    _ ->
				case lists:keysearch(<<"delete",
						       SSPort/binary>>,
						     1, Query)
				    of
				  {value, _} ->
				      ejabberd_cluster:call(Node, ejabberd_listener,
					       delete_listener,
					       [PortIpNetp, Module1]),
				      throw(submitted);
				  _ -> ok
				end
			  end
		  end,
		  Ports),
    case lists:keysearch(<<"addnew">>, 1, Query) of
      {value, _} ->
	  {{value, {_, SPort}}, {value, {_, STIP}},
	   {value, {_, SNetProt}}, {value, {_, SModule}},
	   {value, {_, SOpts}}} =
	      {lists:keysearch(<<"portnew">>, 1, Query),
	       lists:keysearch(<<"ipnew">>, 1, Query),
	       lists:keysearch(<<"netprotnew">>, 1, Query),
	       lists:keysearch(<<"modulenew">>, 1, Query),
	       lists:keysearch(<<"optsnew">>, 1, Query)},
	  {ok, Toks, _} = erl_scan:string(binary_to_list(<<SPort/binary, ".">>)),
	  {ok, Port2} = erl_parse:parse_term(Toks),
	  {ok, ToksIP, _} = erl_scan:string(binary_to_list(<<STIP/binary, ".">>)),
	  STIP2 = case erl_parse:parse_term(ToksIP) of
		    {ok, IPTParsed} -> IPTParsed;
		    {error, _} -> STIP
		  end,
	  Module = jlib:binary_to_atom(SModule),
	  NetProt2 = jlib:binary_to_atom(SNetProt),
	  {ok, Tokens, _} = erl_scan:string(binary_to_list(<<SOpts/binary, ".">>)),
	  {ok, Opts} = erl_parse:parse_term(Tokens),
	  {Port2, _SPort, IP2, _SIP, _SSPort, NetProt2,
	   OptsClean} =
	      get_port_data({Port2, STIP2, NetProt2}, Opts),
	  R = ejabberd_cluster:call(Node, ejabberd_listener, add_listener,
		       [{Port2, IP2, NetProt2}, Module, OptsClean]),
	  throw({is_added, R});
      _ -> ok
    end.

node_modules_to_xhtml(Modules, Lang) ->
    ?XAE(<<"table">>, [{<<"class">>, <<"withtextareas">>}],
	 [?XE(<<"thead">>,
	      [?XE(<<"tr">>,
		   [?XCT(<<"td">>, <<"Module">>),
		    ?XCT(<<"td">>, <<"Options">>)])]),
	  ?XE(<<"tbody">>,
	      (lists:map(fun ({Module, Opts} = _E) ->
				 SModule =
				     iolist_to_binary(atom_to_list(Module)),
				 {NumLines, SOpts} = term_to_paragraph(Opts,
								       40),
				 ?XE(<<"tr">>,
				     [?XC(<<"td">>, SModule),
				      ?XAE(<<"td">>, direction(ltr),
					  [?TEXTAREA(<<"opts", SModule/binary>>,
						     (iolist_to_binary(integer_to_list(NumLines))),
						     <<"40">>, SOpts)]),
				      ?XE(<<"td">>,
					  [?INPUTT(<<"submit">>,
						   <<"restart",
						     SModule/binary>>,
						   <<"Restart">>)]),
				      ?XE(<<"td">>,
					  [?INPUTT(<<"submit">>,
						   <<"stop", SModule/binary>>,
						   <<"Stop">>)])])
			 end,
			 Modules)
		 ++
		 [?XE(<<"tr">>,
		      [?XE(<<"td">>,
			   [?INPUT(<<"text">>, <<"modulenew">>, <<"">>)]),
		       ?XAE(<<"td">>, direction(ltr),
			   [?TEXTAREA(<<"optsnew">>, <<"2">>, <<"40">>,
				      <<"[]">>)]),
		       ?XAE(<<"td">>, [{<<"colspan">>, <<"2">>}],
			    [?INPUTT(<<"submit">>, <<"start">>,
				     <<"Start">>)])])]))]).

node_modules_parse_query(Host, Node, Modules, Query) ->
    lists:foreach(fun ({Module, _Opts1}) ->
			  SModule = iolist_to_binary(atom_to_list(Module)),
			  case lists:keysearch(<<"restart", SModule/binary>>, 1,
					       Query)
			      of
			    {value, _} ->
				{value, {_, SOpts}} = lists:keysearch(<<"opts",
									SModule/binary>>,
								      1, Query),
				{ok, Tokens, _} =
				    erl_scan:string(binary_to_list(<<SOpts/binary, ".">>)),
				{ok, Opts} = erl_parse:parse_term(Tokens),
				ejabberd_cluster:call(Node, gen_mod, stop_module,
					 [Host, Module]),
				ejabberd_cluster:call(Node, gen_mod, start_module,
					 [Host, Module, Opts]),
				throw(submitted);
			    _ ->
				case lists:keysearch(<<"stop", SModule/binary>>,
						     1, Query)
				    of
				  {value, _} ->
				      ejabberd_cluster:call(Node, gen_mod, stop_module,
					       [Host, Module]),
				      throw(submitted);
				  _ -> ok
				end
			  end
		  end,
		  Modules),
    case lists:keysearch(<<"start">>, 1, Query) of
      {value, _} ->
	  {{value, {_, SModule}}, {value, {_, SOpts}}} =
	      {lists:keysearch(<<"modulenew">>, 1, Query),
	       lists:keysearch(<<"optsnew">>, 1, Query)},
	  Module = jlib:binary_to_atom(SModule),
	  {ok, Tokens, _} = erl_scan:string(binary_to_list(<<SOpts/binary, ".">>)),
	  {ok, Opts} = erl_parse:parse_term(Tokens),
	  ejabberd_cluster:call(Node, gen_mod, start_module,
		   [Host, Module, Opts]),
	  throw(submitted);
      _ -> ok
    end.

node_update_parse_query(Node, Query) ->
    case lists:keysearch(<<"update">>, 1, Query) of
      {value, _} ->
	  ModulesToUpdateStrings =
	      proplists:get_all_values(<<"selected">>, Query),
	  ModulesToUpdate = [jlib:binary_to_atom(M)
			     || M <- ModulesToUpdateStrings],
	  case ejabberd_cluster:call(Node, ejabberd_update, update,
			[ModulesToUpdate])
	      of
	    {ok, _} -> ok;
	    {error, Error} ->
		?ERROR_MSG("~p~n", [Error]),
		{error, iolist_to_binary(io_lib:format("~p", [Error]))};
	    {badrpc, Error} ->
		?ERROR_MSG("Bad RPC: ~p~n", [Error]),
		{error,
		 <<"Bad RPC: ", (iolist_to_binary(io_lib:format("~p", [Error])))/binary>>}
	  end;
      _ -> nothing
    end.

pretty_print_xml(El) ->
    list_to_binary(pretty_print_xml(El, <<"">>)).

pretty_print_xml({xmlcdata, CData}, Prefix) ->
    IsBlankCData = lists:all(
                     fun($\f) -> true;
                        ($\r) -> true;
                        ($\n) -> true;
                        ($\t) -> true;
                        ($\v) -> true;
                        ($ ) -> true;
                        (_) -> false
                     end, binary_to_list(CData)),
    if IsBlankCData ->
            [];
       true ->
            [Prefix, CData, $\n]
    end;
pretty_print_xml(#xmlel{name = Name, attrs = Attrs,
			children = Els},
		 Prefix) ->
    [Prefix, $<, Name,
     case Attrs of
       [] -> [];
       [{Attr, Val} | RestAttrs] ->
	   AttrPrefix = [Prefix,
			 str:copies(<<" ">>, byte_size(Name) + 2)],
	   [$\s, Attr, $=, $', xml:crypt(Val) | [$',
                                                 lists:map(fun ({Attr1,
                                                                 Val1}) ->
                                                                   [$\n,
                                                                    AttrPrefix,
                                                                    Attr1, $=,
                                                                    $',
                                                                    xml:crypt(Val1),
                                                                    $']
                                                           end,
                                                           RestAttrs)]]
     end,
     if Els == [] -> <<"/>\n">>;
	true ->
	    OnlyCData = lists:all(fun ({xmlcdata, _}) -> true;
				      (#xmlel{}) -> false
				  end,
				  Els),
	    if OnlyCData ->
		   [$>, xml:get_cdata(Els), $<, $/, Name, $>, $\n];
	       true ->
		   [$>, $\n,
		    lists:map(fun (E) ->
				      pretty_print_xml(E, [Prefix, <<"  ">>])
			      end,
			      Els),
		    Prefix, $<, $/, Name, $>, $\n]
	    end
     end].

element_to_list(X) when is_atom(X) ->
    iolist_to_binary(atom_to_list(X));
element_to_list(X) when is_integer(X) ->
    iolist_to_binary(integer_to_list(X)).

list_to_element(Bin) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Bin)),
    [{_, _, Element}] = Tokens,
    Element.

url_func({user_diapason, From, To}) ->
    <<(iolist_to_binary(integer_to_list(From)))/binary, "-",
      (iolist_to_binary(integer_to_list(To)))/binary, "/">>;
url_func({users_queue, Prefix, User, _Server}) ->
    <<Prefix/binary, "user/", User/binary, "/queue/">>;
url_func({user, Prefix, User, _Server}) ->
    <<Prefix/binary, "user/", User/binary, "/">>.

last_modified() ->
    {<<"Last-Modified">>,
     <<"Mon, 25 Feb 2008 13:23:30 GMT">>}.

cache_control_public() ->
    {<<"Cache-Control">>, <<"public">>}.

%% Transform 1234567890 into "1,234,567,890"
pretty_string_int(Integer) when is_integer(Integer) ->
    pretty_string_int(iolist_to_binary(integer_to_list(Integer)));
pretty_string_int(String) when is_binary(String) ->
    {_, Result} = lists:foldl(fun (NewNumber, {3, Result}) ->
				      {1, <<NewNumber, $,, Result/binary>>};
				  (NewNumber, {CountAcc, Result}) ->
				      {CountAcc + 1, <<NewNumber, Result/binary>>}
			      end,
			      {0, <<"">>}, lists:reverse(binary_to_list(String))),
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
    HostNodeMenu = make_host_node_menu(Host, Node, Lang,
				       JID),
    HostMenu = make_host_menu(Host, HostNodeMenu, Lang,
			      JID),
    NodeMenu = make_node_menu(Host, Node, Lang),
    make_server_menu(HostMenu, NodeMenu, Lang, JID).

%% @spec (Host, Node, Base, Lang) -> [LI]
make_menu_items(global, cluster, Base, Lang) ->
    HookItems = get_menu_items_hook(server, Lang),
    make_menu_items(Lang, {Base, <<"">>, HookItems});
make_menu_items(global, Node, Base, Lang) ->
    HookItems = get_menu_items_hook({node, Node}, Lang),
    make_menu_items(Lang, {Base, <<"">>, HookItems});
make_menu_items(Host, cluster, Base, Lang) ->
    HookItems = get_menu_items_hook({host, Host}, Lang),
    make_menu_items(Lang, {Base, <<"">>, HookItems});
make_menu_items(Host, Node, Base, Lang) ->
    HookItems = get_menu_items_hook({hostnode, Host, Node},
				    Lang),
    make_menu_items(Lang, {Base, <<"">>, HookItems}).

make_host_node_menu(global, _, _Lang, _JID) ->
    {<<"">>, <<"">>, []};
make_host_node_menu(_, cluster, _Lang, _JID) ->
    {<<"">>, <<"">>, []};
make_host_node_menu(Host, Node, Lang, JID) ->
    HostNodeBase = get_base_path(Host, Node),
    HostNodeFixed = [{<<"modules/">>, <<"Modules">>}] ++
		      get_menu_items_hook({hostnode, Host, Node}, Lang),
    HostNodeBasePath = url_to_path(HostNodeBase),
    HostNodeFixed2 = [Tuple
		      || Tuple <- HostNodeFixed,
			 is_allowed_path(HostNodeBasePath, Tuple, JID)],
    {HostNodeBase, iolist_to_binary(atom_to_list(Node)),
     HostNodeFixed2}.

make_host_menu(global, _HostNodeMenu, _Lang, _JID) ->
    {<<"">>, <<"">>, []};
make_host_menu(Host, HostNodeMenu, Lang, JID) ->
    HostBase = get_base_path(Host, cluster),
    HostFixed = [{<<"acls">>, <<"Access Control Lists">>},
		 {<<"access">>, <<"Access Rules">>},
		 {<<"users">>, <<"Users">>},
		 {<<"online-users">>, <<"Online Users">>}]
		  ++
		  get_lastactivity_menuitem_list(Host) ++
		    [{<<"nodes">>, <<"Nodes">>, HostNodeMenu},
		     {<<"stats">>, <<"Statistics">>}]
		      ++ get_menu_items_hook({host, Host}, Lang),
    HostBasePath = url_to_path(HostBase),
    HostFixed2 = [Tuple
		  || Tuple <- HostFixed,
		     is_allowed_path(HostBasePath, Tuple, JID)],
    {HostBase, Host, HostFixed2}.

make_node_menu(_Host, cluster, _Lang) ->
    {<<"">>, <<"">>, []};
make_node_menu(global, Node, Lang) ->
    NodeBase = get_base_path(global, Node),
    NodeFixed = [{<<"db/">>, <<"Database">>},
		 {<<"backup/">>, <<"Backup">>},
		 {<<"ports/">>, <<"Listened Ports">>},
		 {<<"stats/">>, <<"Statistics">>},
		 {<<"update/">>, <<"Update">>}]
		  ++ get_menu_items_hook({node, Node}, Lang),
    {NodeBase, iolist_to_binary(atom_to_list(Node)),
     NodeFixed};
make_node_menu(_Host, _Node, _Lang) ->
    {<<"">>, <<"">>, []}.

make_server_menu(HostMenu, NodeMenu, Lang, JID) ->
    Base = get_base_path(global, cluster),
    Fixed = [{<<"acls">>, <<"Access Control Lists">>},
	     {<<"access">>, <<"Access Rules">>},
	     {<<"vhosts">>, <<"Virtual Hosts">>, HostMenu},
	     {<<"nodes">>, <<"Nodes">>, NodeMenu},
	     {<<"stats">>, <<"Statistics">>}]
	      ++ get_menu_items_hook(server, Lang),
    BasePath = url_to_path(Base),
    Fixed2 = [Tuple
	      || Tuple <- Fixed,
		 is_allowed_path(BasePath, Tuple, JID)],
    {Base, <<"">>, Fixed2}.

get_menu_items_hook({hostnode, Host, Node}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_hostnode, Host,
			    [], [Host, Node, Lang]);
get_menu_items_hook({host, Host}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_host, Host, [],
			    [Host, Lang]);
get_menu_items_hook({node, Node}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_node, [],
			    [Node, Lang]);
get_menu_items_hook(server, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_main, [], [Lang]).

%% @spec (Lang::string(), Menu) -> [LI]
%% where Menu = {MURI::string(), MName::string(), Items::[Item]}
%%       Item = {IURI::string(), IName::string()} | {IURI::string(), IName::string(), Menu}
make_menu_items(Lang, Menu) ->
    lists:reverse(make_menu_items2(Lang, 1, Menu)).

make_menu_items2(Lang, Deep, {MURI, MName, _} = Menu) ->
    Res = case MName of
	    <<"">> -> [];
	    _ -> [make_menu_item(header, Deep, MURI, MName, Lang)]
	  end,
    make_menu_items2(Lang, Deep, Menu, Res).

make_menu_items2(_, _Deep, {_, _, []}, Res) -> Res;
make_menu_items2(Lang, Deep,
		 {MURI, MName, [Item | Items]}, Res) ->
    Res2 = case Item of
	     {IURI, IName} ->
		 [make_menu_item(item, Deep,
				 <<MURI/binary, IURI/binary, "/">>, IName, Lang)
		  | Res];
	     {IURI, IName, SubMenu} ->
		 ResTemp = [make_menu_item(item, Deep,
					   <<MURI/binary, IURI/binary, "/">>,
					   IName, Lang)
			    | Res],
		 ResSubMenu = make_menu_items2(Lang, Deep + 1, SubMenu),
		 ResSubMenu ++ ResTemp
	   end,
    make_menu_items2(Lang, Deep, {MURI, MName, Items},
		     Res2).

make_menu_item(header, 1, URI, Name, _Lang) ->
    ?LI([?XAE(<<"div">>, [{<<"id">>, <<"navhead">>}],
	      [?AC(URI, Name)])]);
make_menu_item(header, 2, URI, Name, _Lang) ->
    ?LI([?XAE(<<"div">>, [{<<"id">>, <<"navheadsub">>}],
	      [?AC(URI, Name)])]);
make_menu_item(header, 3, URI, Name, _Lang) ->
    ?LI([?XAE(<<"div">>, [{<<"id">>, <<"navheadsubsub">>}],
	      [?AC(URI, Name)])]);
make_menu_item(item, 1, URI, Name, Lang) ->
    ?LI([?XAE(<<"div">>, [{<<"id">>, <<"navitem">>}],
	      [?ACT(URI, Name)])]);
make_menu_item(item, 2, URI, Name, Lang) ->
    ?LI([?XAE(<<"div">>, [{<<"id">>, <<"navitemsub">>}],
	      [?ACT(URI, Name)])]);
make_menu_item(item, 3, URI, Name, Lang) ->
    ?LI([?XAE(<<"div">>, [{<<"id">>, <<"navitemsubsub">>}],
	      [?ACT(URI, Name)])]).

%%%==================================


opt_type(access) -> fun (V) -> V end;
opt_type(_) -> [access].

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
