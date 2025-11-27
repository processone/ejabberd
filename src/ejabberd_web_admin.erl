%%%----------------------------------------------------------------------
%%% File    : ejabberd_web_admin.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Administration web interface
%%% Created :  9 Apr 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-author('alexey@process-one.net').

-export([process/2, pretty_print_xml/1,
         make_command/2, make_command/4, make_command_raw_value/3,
         make_table/2, make_table/4,
         make_menu_system/4, make_menu_system_el/4,
         term_to_id/1, id_to_term/1]).

%% Internal commands
-export([webadmin_host_last_activity/3,
         webadmin_node_db_table_page/3]).

-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("logger.hrl").
-include("translate.hrl").

-define(INPUTATTRS(Type, Name, Value, Attrs),
	?XA(<<"input">>,
	    (Attrs ++
	       [{<<"type">>, Type}, {<<"name">>, Name},
		{<<"value">>, Value}]))).

%%%==================================
%%%% get_acl_access

-spec get_acl_rule(Path::[binary()], 'GET' | 'POST') ->
    {HostOfRule::binary(), [AccessRule::atom()]}.

%% All accounts can access those URLs
get_acl_rule([], _) -> {<<"localhost">>, [all]};
get_acl_rule([<<"style.css">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"logo.png">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"favicon.ico">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"additions.js">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"sortable.min.css">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"sortable.min.js">>], _) ->
    {<<"localhost">>, [all]};
%% This page only displays vhosts that the user is admin:
get_acl_rule([<<"vhosts">>], _) ->
    {<<"localhost">>, [all]};
%% The pages of a vhost are only accessible if the user is admin of that vhost:
get_acl_rule([<<"server">>, VHost | _RPath], Method)
    when Method =:= 'GET' orelse Method =:= 'HEAD' ->
    {VHost, [configure]};
get_acl_rule([<<"server">>, VHost | _RPath], 'POST') ->
    {VHost, [configure]};
%% Default rule: only global admins can access any other random page
get_acl_rule(_RPath, Method)
    when Method =:= 'GET' orelse Method =:= 'HEAD' ->
    {global, [configure]};
get_acl_rule(_RPath, 'POST') ->
    {global, [configure]}.

%%%==================================
%%%% Menu Items Access

get_jid(Auth, HostHTTP, Method) ->
    case get_auth_admin(Auth, HostHTTP, [], Method) of
      {ok, {User, Server}} ->
	  jid:make(User, Server);
      {unauthorized, Error} ->
	  ?ERROR_MSG("Unauthorized ~p: ~p", [Auth, Error]),
	  throw({unauthorized, Auth})
    end.

get_menu_items(global, cluster, Lang, JID, Level) ->
    {_Base, _, Items} = make_server_menu([], [], Lang, JID, Level),
    lists:map(fun ({URI, Name}) ->
		      {<<URI/binary, "/">>, Name};
		  ({URI, Name, _SubMenu}) ->
		      {<<URI/binary, "/">>, Name}
	      end,
	      Items);
get_menu_items(Host, cluster, Lang, JID, Level) ->
    {_Base, _, Items} = make_host_menu(Host, [], [], Lang, JID, Level),
    lists:map(fun ({URI, Name}) ->
		      {<<URI/binary, "/">>, Name};
		  ({URI, Name, _SubMenu}) ->
		      {<<URI/binary, "/">>, Name}
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

is_allowed_path(global, RPath, JID) ->
    is_allowed_path([], RPath, JID);
is_allowed_path(Host, RPath, JID) when is_binary(Host) ->
    is_allowed_path([<<"server">>, Host], RPath, JID);

is_allowed_path(BasePath, {Path, _}, JID) ->
    is_allowed_path(BasePath ++ [Path], JID);
is_allowed_path(BasePath, {Path, _, _}, JID) ->
    is_allowed_path(BasePath ++ [Path], JID).

is_allowed_path([<<"admin">> | Path], JID) ->
    is_allowed_path(Path, JID);
is_allowed_path(Path, JID) ->
    {HostOfRule, AccessRule} = get_acl_rule(Path, 'GET'),
    any_rules_allowed(HostOfRule, AccessRule, JID).

%%%==================================
%%%% process/2

process(Path, #request{raw_path = RawPath} = Request) ->
    Continue = case Path of
		   [E] ->
		       binary:match(E, <<".">>) /= nomatch;
		   _ ->
		       false
	       end,
    case Continue orelse binary:at(RawPath, size(RawPath) - 1) == $/ of
	true ->
	    process2(Path, Request);
	_ ->
	    {301, [{<<"Location">>, <<RawPath/binary, "/">>}], <<>>}
    end.

process2([<<"logout">> | _], #request{lang = Lang}) ->
    Text = [?XCT(<<"h1">>, ?T("Logged Out")),
            ?XE(<<"p">>, [?C(<<"Your web browser has logout from WebAdmin. Close this window, or login again in: ">>),
                          ?AC(<<"../">>, <<"ejabberd WebAdmin">>)])],
    {401, [{<<"WWW-Authenticate">>, <<"basic realm=\"ejabberd\"">>}],
     ejabberd_web:make_xhtml(Text)};
process2([<<"server">>, SHost | RPath] = Path,
	#request{auth = Auth, lang = Lang, host = HostHTTP,
		 method = Method} =
	    Request) ->
    Host = jid:nameprep(SHost),
    case ejabberd_router:is_my_host(Host) of
      true ->
	  case get_auth_admin(Auth, HostHTTP, Path, Method) of
	    {ok, {User, Server}} ->
		AJID = get_jid(Auth, HostHTTP, Method),
		process_admin(Host,
			      Request#request{path = RPath,
					      us = {User, Server}},
			      AJID);
	    {unauthorized, <<"no-auth-provided">>} ->
		{401,
		 [{<<"WWW-Authenticate">>,
		   <<"basic realm=\"ejabberd\"">>}],
		 ejabberd_web:make_xhtml(make_unauthorized(Lang))};
	    {unauthorized, Error} ->
		{BadUser, _BadPass} = Auth,
		{IPT, _Port} = Request#request.ip,
		IPS = ejabberd_config:may_hide_data(misc:ip_to_list(IPT)),
		?WARNING_MSG("Access of ~p from ~p failed with error: ~p",
			     [BadUser, IPS, Error]),
		{401,
		 [{<<"WWW-Authenticate">>,
		   <<"basic realm=\"auth error, retry login "
		     "to ejabberd\"">>}],
		 ejabberd_web:make_xhtml(make_unauthorized(Lang))}
	  end;
      false -> ejabberd_web:error(not_found)
    end;
process2(RPath,
	#request{auth = Auth, lang = Lang, host = HostHTTP,
		 method = Method} =
	    Request) ->
    case get_auth_admin(Auth, HostHTTP, RPath, Method) of
	{ok, {User, Server}} ->
	    AJID = get_jid(Auth, HostHTTP, Method),
	    process_admin(global,
			  Request#request{path = RPath,
					  us = {User, Server}},
			  AJID);
	{unauthorized, <<"no-auth-provided">>} ->
	    {401,
	     [{<<"WWW-Authenticate">>,
	       <<"basic realm=\"ejabberd\"">>}],
	     ejabberd_web:make_xhtml(make_unauthorized(Lang))};
	{unauthorized, Error} ->
	    {BadUser, _BadPass} = Auth,
	    {IPT, _Port} = Request#request.ip,
	    IPS = ejabberd_config:may_hide_data(misc:ip_to_list(IPT)),
	    ?WARNING_MSG("Access of ~p from ~p failed with error: ~p",
			 [BadUser, IPS, Error]),
	    {401,
	     [{<<"WWW-Authenticate">>,
	       <<"basic realm=\"auth error, retry login "
		 "to ejabberd\"">>}],
	     ejabberd_web:make_xhtml(make_unauthorized(Lang))}
    end.

make_unauthorized(Lang) ->
    [?XCT(<<"h1">>, ?T("Unauthorized")),
     ?XE(<<"p">>, [?C(<<"There was some problem authenticating, or the account doesn't have privilege.">>)]),
     ?XE(<<"p">>, [?C(<<"Please check the log file for a more precise error message.">>)])].

get_auth_admin(Auth, HostHTTP, RPath, Method) ->
    case Auth of
      {SJID, Pass} ->
	  {HostOfRule, AccessRule} = get_acl_rule(RPath, Method),
	    try jid:decode(SJID) of
		#jid{luser = <<"">>, lserver = User} ->
		    case ejabberd_router:is_my_host(HostHTTP) of
			true ->
			    get_auth_account(HostOfRule, AccessRule, User, HostHTTP,
					     Pass);
			_ ->
			    {unauthorized, <<"missing-server">>}
		    end;
		#jid{luser = User, lserver = Server} ->
		    get_auth_account(HostOfRule, AccessRule, User, Server,
				     Pass)
	    catch _:{bad_jid, _} ->
		    {unauthorized, <<"badformed-jid">>}
	    end;
      invalid -> {unauthorized, <<"no-auth-provided">>};
      undefined -> {unauthorized, <<"no-auth-provided">>}
    end.

get_auth_account(HostOfRule, AccessRule, User, Server,
		 Pass) ->
    case lists:member(Server, ejabberd_config:get_option(hosts)) of
	true -> get_auth_account2(HostOfRule, AccessRule, User, Server, Pass);
	false -> {unauthorized, <<"inexistent-host">>}
    end.

get_auth_account2(HostOfRule, AccessRule, User, Server,
		 Pass) ->
    case ejabberd_auth:check_password(User, <<"">>, Server, Pass) of
      true ->
	  case any_rules_allowed(HostOfRule, AccessRule,
				 jid:make(User, Server))
	      of
	    false -> {unauthorized, <<"unprivileged-account">>};
	    true -> {ok, {User, Server}}
	  end;
      false ->
	  case ejabberd_auth:user_exists(User, Server) of
	    true -> {unauthorized, <<"bad-password">>};
	    false -> {unauthorized, <<"inexistent-account">>}
	  end
    end.

%%%==================================
%%%% make_xhtml

make_xhtml(Els, Host, Request, JID, Level) ->
    make_xhtml(Els, Host, cluster, unspecified, Request, JID, Level).

make_xhtml(Els, Host, Username, Request, JID, Level) when
      (Username == unspecified) or (is_binary(Username)) ->
    make_xhtml(Els, Host, cluster, Username, Request, JID, Level);

make_xhtml(Els, Host, Node, Request, JID, Level) ->
    make_xhtml(Els, Host, Node, unspecified, Request, JID, Level).

-spec make_xhtml([xmlel()],
                 Host::global | binary(),
                 Node::cluster | atom(),
                 Username::unspecified | binary(),
                 Request::http_request(),
                 jid(),
                 Level::integer()) ->
    {200, [html], xmlel()}.
make_xhtml(Els, Host, Node, Username, #request{lang = Lang} = R, JID, Level) ->
    Base = get_base_path_sum(0, 0, Level),
    MenuItems = make_navigation(Host, Node, Username, Lang, JID, Level)
    ++ make_login_items(R, Level),
    {200, [html],
     #xmlel{name = <<"html">>,
	    attrs =
		[{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>},
		 {<<"xml:lang">>, Lang}, {<<"lang">>, Lang}]++direction(Lang),
	    children =
		[#xmlel{name = <<"head">>, attrs = [],
			children =
			    [?XCT(<<"title">>, ?T("ejabberd Web Admin")),
			     #xmlel{name = <<"meta">>,
				    attrs =
					[{<<"http-equiv">>, <<"Content-Type">>},
					 {<<"content">>,
					  <<"text/html; charset=utf-8">>}],
				    children = []},
			     #xmlel{name = <<"script">>,
				    attrs =
					[{<<"src">>,
					  <<Base/binary, "additions.js">>},
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
				    children = []},
			     #xmlel{name = <<"link">>,
				    attrs =
					[{<<"href">>,
					  <<Base/binary, "sortable.min.css">>},
					 {<<"type">>, <<"text/css">>},
					 {<<"rel">>, <<"stylesheet">>}],
				    children = []},
			     #xmlel{name = <<"script">>,
				    attrs =
					[{<<"src">>,
					  <<Base/binary, "sortable.min.js">>},
					 {<<"type">>, <<"text/javascript">>}],
				    children = [?C(<<" ">>)]}]},
		 ?XE(<<"body">>,
		     [?XAE(<<"div">>, [{<<"id">>, <<"container">>}],
			   [?XAE(<<"div">>, [{<<"id">>, <<"header">>}],
				 [?XE(<<"h1">>,
				      [?ACT(Base,
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
				   ?C(<<" ">>), ?C(ejabberd_option:version()),
				   ?C(<<" (c) 2002-2025 ">>),
				   ?AC(<<"https://www.process-one.net/">>, <<"ProcessOne, leader in messaging and push solutions">>)]
                                 )])])])]}}.

direction(<<"he">>) -> [{<<"dir">>, <<"rtl">>}];
direction(_) -> [].

get_base_path(Host, Node, Level) ->
    SumHost = case Host of
        global -> 0;
        _ -> -2
    end,
    SumNode = case Node of
        cluster -> 0;
        _ -> -2
    end,
    get_base_path_sum(SumHost, SumNode, Level).

get_base_path_sum(SumHost, SumNode, Level) ->
    iolist_to_binary(lists:duplicate(Level + SumHost + SumNode, "../")).

%%%==================================
%%%% css & images

additions_js() ->
    case misc:read_js("admin.js") of
	{ok, JS} -> JS;
	{error, _} -> <<>>
    end.

css(Host) ->
    case misc:read_css("admin.css") of
	{ok, CSS} ->
	    Base = get_base_path(Host, cluster, 0),
	    re:replace(CSS, <<"@BASE@">>, Base, [{return, binary}]);
	{error, _} ->
	    <<>>
    end.

favicon() ->
    case misc:read_img("favicon.png") of
	{ok, ICO} -> ICO;
	{error, _} -> <<>>
    end.

logo() ->
    case misc:read_img("admin-logo.png") of
	{ok, Img} -> Img;
	{error, _} -> <<>>
    end.

sortable_css() ->
    case misc:read_css("sortable.min.css") of
	{ok, CSS} -> CSS;
	{error, _} -> <<>>
    end.

sortable_js() ->
    case misc:read_js("sortable.min.js") of
	{ok, JS} -> JS;
	{error, _} -> <<>>
    end.

%%%==================================
%%%% process_admin

process_admin(global, #request{path = [], lang = Lang} = Request, AJID) ->
    Title = ?H1GLraw(<<"">>, <<"">>, <<"home">>),
    MenuItems = get_menu_items(global, cluster, Lang, AJID, 0),
    Disclaimer = maybe_disclaimer_not_admin(MenuItems, AJID, Lang),
    WelcomeText =
        [?BR,
         ?XAE(<<"p">>, [{<<"align">>, <<"center">>}],
              [?XA(<<"img">>, [{<<"src">>, <<"logo.png">>},
                               {<<"style">>, <<"border-radius:10px; background:#49cbc1; padding: 1.1em;">>}])
              ])
         ] ++ Title ++ [
         ?XAE(<<"blockquote">>,
	     [{<<"id">>, <<"welcome">>}],
             [?XC(<<"p">>, <<"Welcome to ejabberd's WebAdmin!">>),
              ?XC(<<"p">>, <<"Browse the menu to navigate your XMPP virtual hosts, "
                              "Erlang nodes, and other global server pages...">>),
              ?XC(<<"p">>, <<"Some pages have a link in the top right corner "
                              "to relevant documentation in ejabberd Docs.">>),
              ?X(<<"hr">>),
              ?XE(<<"p">>,
                  [?C(<<"Many pages use ejabberd's API commands to show information "
                         "and to allow you perform administrative tasks. "
                         "Click on a command name to view its details. "
                         "You can also execute those same API commands "
                         "using other interfaces, see: ">>),
                   ?AC(<<"https://docs.ejabberd.im/developer/ejabberd-api/">>,
                       <<"ejabberd Docs: API">>)
                  ]),
              ?XC(<<"p">>, <<"For example, this is the 'stats' command, "
                              "it accepts an argument and returns an integer:">>),
              make_command(stats, Request)]),
         ?BR],
    make_xhtml(Disclaimer ++ WelcomeText ++
                   [?XE(<<"ul">>,
                        [?LI([?ACT(MIU, MIN)])
                         || {MIU, MIN}
                                <- MenuItems])],
	       global, Request, AJID, 0);
process_admin(Host, #request{path = [], lang = Lang} = R, AJID) ->
    make_xhtml([?XCT(<<"h1">>, ?T("Administration")),
		?XE(<<"ul">>,
		    [?LI([?ACT(MIU, MIN)])
		     || {MIU, MIN}
			    <- get_menu_items(Host, cluster, Lang, AJID, 2)])],
	       Host, R, AJID, 2);

process_admin(Host, #request{path = [<<"style.css">>]}, _) ->
    {200,
     [{<<"Content-Type">>, <<"text/css">>}, last_modified(),
      cache_control_public()],
     css(Host)};
process_admin(_Host, #request{path = [<<"favicon.ico">>]}, _) ->
    {200,
     [{<<"Content-Type">>, <<"image/x-icon">>},
      last_modified(), cache_control_public()],
     favicon()};
process_admin(_Host, #request{path = [<<"logo.png">>]}, _) ->
    {200,
     [{<<"Content-Type">>, <<"image/png">>}, last_modified(),
      cache_control_public()],
     logo()};
process_admin(_Host, #request{path = [<<"additions.js">>]}, _) ->
    {200,
     [{<<"Content-Type">>, <<"text/javascript">>},
      last_modified(), cache_control_public()],
     additions_js()};
process_admin(_Host, #request{path = [<<"sortable.min.css">>]}, _) ->
    {200,
     [{<<"Content-Type">>, <<"text/css">>}, last_modified(),
      cache_control_public()],
     sortable_css()};
process_admin(_Host, #request{path = [<<"sortable.min.js">>]}, _) ->
    {200,
     [{<<"Content-Type">>, <<"text/javascript">>},
      last_modified(), cache_control_public()],
     sortable_js()};

%% @format-begin

process_admin(global, #request{path = [<<"vhosts">> | RPath], lang = Lang} = R, AJID) ->
    Hosts =
        case make_command_raw_value(registered_vhosts, R, []) of
            Hs when is_list(Hs) ->
                Hs;
            _ ->
                {User, Server} = R#request.us,
                ?INFO_MSG("Access to WebAdmin page vhosts/ for account ~s@~s was denied",
                          [User, Server]),
                []
        end,
    Level = 1 + length(RPath),
    HostsAllowed = [Host || Host <- Hosts, can_user_access_host(Host, R)],
    Table =
        make_table(20,
                   RPath,
                   [<<"host">>, {<<"registered users">>, right}, {<<"online users">>, right}],
                   [{make_command(echo,
                                  R,
                                  [{<<"sentence">>, Host}],
                                  [{only, value},
                                   {result_links, [{sentence, host, Level, <<"">>}]}]),
                     make_command(stats_host,
                                  R,
                                  [{<<"name">>, <<"registeredusers">>}, {<<"host">>, Host}],
                                  [{only, value},
                                   {result_links, [{stat, arg_host, Level, <<"users/">>}]}]),
                     make_command(stats_host,
                                  R,
                                  [{<<"name">>, <<"onlineusers">>}, {<<"host">>, Host}],
                                  [{only, value},
                                   {result_links, [{stat, arg_host, Level, <<"online-users/">>}]}])}
                    || Host <- HostsAllowed]),
    VhostsElements =
        [make_command(registered_vhosts, R, [], [{only, presentation}]),
         make_command(stats_host, R, [], [{only, presentation}]),
         ?XE(<<"blockquote">>, [Table])],
    make_xhtml(?H1GL(translate:translate(Lang, ?T("Virtual Hosts")),
                     <<"basic/#xmpp-domains">>,
                     ?T("XMPP Domains"))
               ++ VhostsElements,
               global,
               R,
               AJID,
               Level);
process_admin(Host,
              #request{path = [<<"users">>, <<"diapason">>, Diap | RPath], lang = Lang} = R,
              AJID)
    when is_binary(Host) ->
    Level = 5 + length(RPath),
    RegisterEl = make_command(register, R, [{<<"host">>, Host}], []),
    Res = list_users_in_diapason(Host, Level, 30, RPath, R, Diap, RegisterEl),
    make_xhtml([?XCT(<<"h1">>, ?T("Users"))] ++ Res, Host, R, AJID, Level);
process_admin(Host,
              #request{path = [<<"users">>, <<"top">>, Attribute | RPath], lang = Lang} = R,
              AJID)
    when is_binary(Host) ->
    Level = 5 + length(RPath),
    RegisterEl = make_command(register, R, [{<<"host">>, Host}], []),
    Res = list_users_top(Host, Level, 30, RPath, R, Attribute, RegisterEl),
    make_xhtml([?XCT(<<"h1">>, ?T("Users"))] ++ Res, Host, R, AJID, Level);
process_admin(Host, #request{path = [<<"users">> | RPath], lang = Lang} = R, AJID)
    when is_binary(Host) ->
    Level = 3 + length(RPath),
    RegisterEl = make_command(register, R, [{<<"host">>, Host}], []),
    Res = list_users(Host, Level, 30, RPath, R, RegisterEl),
    make_xhtml([?XCT(<<"h1">>, ?T("Users"))] ++ Res, Host, R, AJID, Level);
process_admin(Host, #request{path = [<<"online-users">> | RPath], lang = Lang} = R, AJID)
    when is_binary(Host) ->
    Level = 3 + length(RPath),
    Set = [make_command(kick_users,
                        R,
                        [{<<"host">>, Host}],
                        [{style, danger}, {force_execution, false}])],
    timer:sleep(200), % small delay after kicking users before getting the updated list
    Get = [make_command(connected_users_vhost,
                        R,
                        [{<<"host">>, Host}],
                        [{table_options, {100, RPath}},
                         {result_links, [{sessions, user, Level, <<"">>}]}])],
    make_xhtml([?XCT(<<"h1">>, ?T("Online Users"))] ++ Set ++ Get, Host, R, AJID, Level);
process_admin(Host,
              #request{path = [<<"last-activity">>],
                       q = Query,
                       lang = Lang} =
                  R,
              AJID)
    when is_binary(Host) ->
    PageH1 =
        ?H1GL(translate:translate(Lang, ?T("Users Last Activity")),
              <<"modules/#mod_last">>,
              <<"mod_last">>),
    Res = make_command(webadmin_host_last_activity,
                       R,
                       [{<<"host">>, Host}, {<<"query">>, Query}, {<<"lang">>, Lang}],
                       []),
    make_xhtml(PageH1 ++ [Res], Host, R, AJID, 3);
process_admin(Host, #request{path = [<<"user">>, U], lang = Lang} = R, AJID) ->
    case ejabberd_auth:user_exists(U, Host) of
        true ->
            Res = user_info(U, Host, R),
            make_xhtml(Res, Host, U, R, AJID, 4);
        false ->
            make_xhtml([?XCT(<<"h1">>, ?T("Not Found"))], Host, R, AJID, 4)
    end;
process_admin(Host, #request{path = [<<"nodes">>]} = R, AJID) ->
    Level =
        case Host of
            global ->
                1;
            _ ->
                3
        end,
    Res = ?H1GLraw(<<"Nodes">>, <<"admin/guide/clustering/">>, <<"Clustering">>)
          ++ [make_command(list_cluster, R, [], [{result_links, [{node, node, 1, <<"">>}]}])],
    make_xhtml(Res, Host, R, AJID, Level);
process_admin(Host,
              #request{path = [<<"node">>, SNode | NPath], lang = Lang} = Request,
              AJID) ->
    case search_running_node(SNode) of
        false ->
            make_xhtml([?XCT(<<"h1">>, ?T("Node not found"))], Host, Request, AJID, 2);
        Node ->
            Res = get_node(Host, Node, NPath, Request#request{path = NPath}),
            Level =
                case Host of
                    global ->
                        2 + length(NPath);
                    _ ->
                        4 + length(NPath)
                end,
            make_xhtml(Res, Host, Node, Request, AJID, Level)
    end;
%%%==================================
%%%% process_admin default case
process_admin(Host, #request{path = Path} = Request, AJID) ->
    {Username, RPath} =
        case Path of
            [<<"user">>, U | UPath] ->
                {U, UPath};
            _ ->
                {unspecified, Path}
        end,
    Request2 = Request#request{path = RPath},
    Res = case {Host, Username} of
              {global, _} ->
                  ejabberd_hooks:run_fold(webadmin_page_main, Host, [], [Request2]);
              {_, unspecified} ->
                  ejabberd_hooks:run_fold(webadmin_page_host, Host, [], [Host, Request2]);
              {_Host, Username} ->
                  ejabberd_hooks:run_fold(webadmin_page_hostuser,
                                          Host,
                                          [],
                                          [Host, Username, Request2])
          end,
    Level =
        case Host of
            global ->
                length(Request#request.path);
            _ ->
                2 + length(Request#request.path)
        end,
    case Res of
        [] ->
            setelement(1,
                       make_xhtml([?XC(<<"h1">>, <<"Not Found">>)], Host, Request, AJID, Level),
                       404);
        _ ->
            make_xhtml(Res, Host, Username, Request, AJID, Level)
    end.
%% @format-end

term_to_id([]) -> <<>>;
term_to_id(T) -> base64:encode((term_to_binary(T))).
id_to_term(<<>>) -> [];
id_to_term(I) -> binary_to_term(base64:decode(I)).

can_user_access_host(Host, #request{auth = Auth,
                                    host = HostHTTP,
                                    method = Method}) ->
    Path = [<<"server">>, Host],
    case get_auth_admin(Auth, HostHTTP, Path, Method) of
      {ok, _} ->
	  true;
      {unauthorized, _Error} ->
	  false
    end.


%%%==================================
%%%% list_vhosts

list_vhosts_allowed(JID) ->
    Hosts = ejabberd_option:hosts(),
    lists:filter(fun (Host) ->
					any_rules_allowed(Host,
						     [configure],
						     JID)
				end,
				Hosts).

maybe_disclaimer_not_admin(MenuItems, AJID, _Lang) ->
    case {MenuItems, list_vhosts_allowed(AJID)} of
        {[_], []} ->
            [?BR,
             ?DIVRES([?C(<<"Apparently your account has no administration rights in "
                            "this server. Please check how to grant admin rights: ">>),
                      ?AC(<<"https://docs.ejabberd.im/admin/install/next-steps/#administration-account">>,
                          <<"ejabberd Docs: Administration Account">>)])
            ];
        _ ->
            []
    end.

%%%==================================
%%%% list_users

%% @format-begin

list_users(Host, Level, PageSize, RPath, R, RegisterEl) ->
    Usernames =
        case make_command_raw_value(registered_users, R, [{<<"host">>, Host}]) of
            As when is_list(As) ->
                As;
            _ ->
                {Aser, Aerver} = R#request.us,
                ?INFO_MSG("Access to WebAdmin page users/ for account ~s@~s was denied",
                          [Aser, Aerver]),
                []
        end,
    case length(Usernames) of
        N when N =< 10 ->
            list_users(Host, Level, PageSize, RPath, R, Usernames, RegisterEl);
        N when N > 10 ->
            list_users_diapason(Host, R, Usernames, N, RegisterEl)
    end.

list_users(Host, Level, PageSize, RPath, R, Usernames, RegisterEl) ->
    IsOffline = gen_mod:is_loaded(Host, mod_offline),
    IsMam = gen_mod:is_loaded(Host, mod_mam),
    IsRoster = gen_mod:is_loaded(Host, mod_roster),
    IsLast = gen_mod:is_loaded(Host, mod_last),
    Columns =
        [<<"user">>,
         list_users_element(IsOffline, column, offline, {}),
         list_users_element(IsMam, column, mam, {}),
         list_users_element(IsRoster, column, roster, {}),
         list_users_element(IsLast, column, timestamp, {}),
         list_users_element(IsLast, column, status, {})],
    Rows =
        [list_to_tuple(lists:flatten([make_command(echo,
                                                   R,
                                                   [{<<"sentence">>,
                                                     jid:encode(
                                                         jid:make(Username, Host))}],
                                                   [{only, raw_and_value},
                                                    {result_links,
                                                     [{sentence, user, Level, <<"">>}]}]),
                                      list_users_element(IsOffline,
                                                         row,
                                                         offline,
                                                         {R, Username, Host, Level}),
                                      list_users_element(IsMam,
                                                         row,
                                                         mam,
                                                         {R, Username, Host, Level}),
                                      list_users_element(IsRoster,
                                                         row,
                                                         roster,
                                                         {R, Username, Host, Level}),
                                      list_users_element(IsLast, row, last, {R, Username, Host})]))
         || Username <- Usernames],
    Table = make_table(PageSize, RPath, lists:flatten(Columns), Rows),
    Result =
        [RegisterEl,
         make_command(registered_users, R, [], [{only, presentation}]),
         list_users_element(IsOffline, presentation, offline, R),
         list_users_element(IsMam, presentation, mam, R),
         list_users_element(IsRoster, presentation, roster, R),
         list_users_element(IsLast, presentation, last, R),
         Table],
    lists:flatten(Result).

list_users_element(false, _, _, _) ->
    [];
list_users_element(_, column, offline, _) ->
    {<<"offline">>, right};
list_users_element(_, column, mam, _) ->
    {<<"mam">>, right};
list_users_element(_, column, roster, _) ->
    {<<"roster">>, right};
list_users_element(_, column, timestamp, _) ->
    {<<"timestamp">>, left};
list_users_element(_, column, status, _) ->
    {<<"status">>, left};
list_users_element(_, row, offline, {R, Username, Host, Level}) ->
    make_command(get_offline_count,
                 R,
                 [{<<"user">>, Username}, {<<"host">>, Host}],
                 [{only, raw_and_value},
                  {result_links,
                   [{value, arg_host, Level, <<"user/", Username/binary, "/queue/">>}]}]);
list_users_element(_, row, mam, {R, Username, Host, Level}) ->
    make_command(get_mam_count,
                 R,
                 [{<<"user">>, Username}, {<<"host">>, Host}],
                 [{only, raw_and_value},
                  {result_links,
                   [{value, arg_host, Level, <<"user/", Username/binary, "/mam/">>}]}]);
list_users_element(_, row, roster, {R, Username, Host, Level}) ->
    make_command(get_roster_count,
                 R,
                 [{<<"user">>, Username}, {<<"host">>, Host}],
                 [{only, raw_and_value},
                  {result_links,
                   [{value, arg_host, Level, <<"user/", Username/binary, "/roster/">>}]}]);
list_users_element(_, row, last, {R, Username, Host}) ->
    [?C(element(1,
                make_command_raw_value(get_last, R, [{<<"user">>, Username}, {<<"host">>, Host}]))),
     ?C(element(2,
                make_command_raw_value(get_last,
                                       R,
                                       [{<<"user">>, Username}, {<<"host">>, Host}])))];
list_users_element(_, presentation, offline, R) ->
    make_command(get_offline_count, R, [], [{only, presentation}]);
list_users_element(_, presentation, mam, R) ->
    make_command(get_mam_count, R, [], [{only, presentation}]);
list_users_element(_, presentation, roster, R) ->
    make_command(get_roster_count, R, [], [{only, presentation}]);
list_users_element(_, presentation, last, R) ->
    make_command(get_last, R, [], [{only, presentation}]).

list_users_diapason(Host, R, Usernames, N, RegisterEl) ->
    URLFunc = fun url_func/1,
    SUsers = [{Host, U} || U <- Usernames],
    NParts = trunc(math:sqrt(N * 6.17999999999999993783e-1)) + 1,
    M = trunc(N / NParts) + 1,
    FUsers =
        lists:flatmap(fun(K) ->
                         L = K + M - 1,
                         Last =
                             if L < N ->
                                    su_to_list(lists:nth(L, SUsers));
                                true ->
                                    su_to_list(lists:last(SUsers))
                             end,
                         Name =
                             <<(su_to_list(lists:nth(K, SUsers)))/binary,
                               $\s,
                               226,
                               128,
                               148,
                               $\s,
                               Last/binary>>,
                         [?AC(URLFunc({user_diapason, K, L}), Name), ?BR]
                      end,
                      lists:seq(1, N, M)),
    [RegisterEl,
     make_command(get_offline_count, R, [], [{only, presentation}]),
     ?AC(<<"top/offline/">>, <<"View Top Offline Queues">>),
     make_command(get_roster_count, R, [], [{only, presentation}]),
     ?AC(<<"top/roster/">>, <<"View Top Rosters">>),
     make_command(get_last, R, [], [{only, presentation}]),
     ?AC(<<"top/last/">>, <<"View Top-Oldest Last Activity">>),
     make_command(registered_users, R, [], [{only, presentation}])]
    ++ FUsers.

list_users_in_diapason(Host, Level, PageSize, RPath, R, Diap, RegisterEl) ->
    Usernames =
        case make_command_raw_value(registered_users, R, [{<<"host">>, Host}]) of
            As when is_list(As) ->
                As;
            _ ->
                {Aser, Aerver} = R#request.us,
                ?INFO_MSG("Access to WebAdmin page users/ for account ~s@~s was denied",
                          [Aser, Aerver]),
                []
        end,
    SUsers = lists:sort([{Host, U} || U <- Usernames]),
    [S1, S2] = ejabberd_regexp:split(Diap, <<"-">>),
    N1 = binary_to_integer(S1),
    N2 = binary_to_integer(S2),
    Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
    Usernames2 = [U || {_, U} <- Sub],
    list_users(Host, Level, PageSize, RPath, R, Usernames2, RegisterEl).

list_users_top(Host, Level, PageSize, RPath, R, Operation, RegisterEl) ->
    Usernames =
        case make_command_raw_value(registered_users, R, [{<<"host">>, Host}]) of
            As when is_list(As) ->
                As;
            _ ->
                {Aser, Aerver} = R#request.us,
                ?INFO_MSG("Access to WebAdmin page users/ for account ~s@~s was denied",
                          [Aser, Aerver]),
                []
        end,
    {Command, Reverse} =
        case Operation of
            <<"roster">> ->
                {get_roster_count, true};
            <<"offline">> ->
                {get_offline_count, true};
            <<"last">> ->
                {get_last, false}
        end,
    UsernamesCounts =
        [{U,
          make_command(Command,
                       R,
                       [{<<"user">>, U}, {<<"host">>, Host}],
                       [{only, raw_value},
                        {result_links,
                         [{value, arg_host, Level, <<"user/", U/binary, "/roster/">>}]}])}
         || U <- Usernames],
    USorted = lists:keysort(2, UsernamesCounts),
    UReversed =
        case Reverse of
            true ->
                lists:reverse(USorted);
            false ->
                USorted
        end,
    Usernames2 = [U || {U, _} <- lists:sublist(UReversed, 100)],
    list_users(Host, Level, PageSize, RPath, R, Usernames2, RegisterEl).

get_lastactivity_menuitem_list(Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
        true ->
            case mod_last_opt:db_type(Server) of
                mnesia ->
                    [{<<"last-activity">>, ?T("Last Activity")}];
                _ ->
                    []
            end;
        false ->
            []
    end.

us_to_list({User, Server}) ->
    jid:encode({User, Server, <<"">>}).

su_to_list({Server, User}) ->
    jid:encode({User, Server, <<"">>}).
%% @format-end

%%%==================================
%%%% last-activity

webadmin_host_last_activity(Host, Query, Lang) ->
    ?DEBUG("Query: ~p", [Query]),
    Month = case lists:keysearch(<<"period">>, 1, Query) of
                {value, {_, Val}} -> Val;
                _ -> <<"month">>
	    end,
    Res = case lists:keysearch(<<"ordinary">>, 1, Query) of
              {value, {_, _}} ->
                  list_last_activity(Host, Lang, false, Month);
              _ -> list_last_activity(Host, Lang, true, Month)
	  end,
       [?XAE(<<"form">>,
             [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
             [?CT(?T("Period: ")),
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
                              [{<<"month">>, translate:translate(Lang, ?T("Last month"))},
                               {<<"year">>, translate:translate(Lang, ?T("Last year"))},
                               {<<"all">>,
                                translate:translate(Lang, ?T("All activity"))}]))),
              ?C(<<" ">>),
              ?INPUTT(<<"submit">>, <<"ordinary">>,
                      ?T("Show Ordinary Table")),
              ?C(<<" ">>),
              ?INPUTT(<<"submit">>, <<"integral">>,
                      ?T("Show Integral Table"))])]
   ++ Res.

%%%==================================
%%%% get_stats

user_info(User, Server, #request{q = Query, lang = Lang} = R) ->
    LServer = jid:nameprep(Server),
    US = {jid:nodeprep(User), LServer},
    Res = user_parse_query(User, Server, Query),
    UserItems = ejabberd_hooks:run_fold(webadmin_user,
					LServer, [], [User, Server, R]),
    Lasts = case gen_mod:is_loaded(Server, mod_last) of
                true ->
                    [make_command(get_last, R,
                                  [{<<"user">>, User}, {<<"host">>, Server}],
                                  []),
                     make_command(set_last, R,
                                  [{<<"user">>, User}, {<<"host">>, Server}],
                                  [])];
                false ->
                    []
            end,
    [?XC(<<"h1">>, (str:translate_and_format(Lang, ?T("User ~ts"),
                                                [us_to_list(US)])))]
      ++
      case Res of
	ok -> [?XREST(?T("Submitted"))];
	error -> [?XREST(?T("Bad format"))];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      ([make_command(user_sessions_info, R,
                             [{<<"user">>, User}, {<<"host">>, Server}],
                             [{result_links, [{node, node, 4, <<>>}]}]),
                make_command(change_password, R,
                             [{<<"user">>, User}, {<<"host">>, Server}],
                             [{style, danger}])] ++
                   Lasts ++
                   UserItems ++
                   [?P,
                make_command(unregister, R,
                             [{<<"user">>, User}, {<<"host">>, Server}],
                             [{style, danger}])
                    ]))].

user_parse_query(User, Server, Query) ->
    lists:foldl(fun ({Action, _Value}, Acc)
			when Acc == nothing ->
			user_parse_query1(Action, User, Server, Query);
		    ({_Action, _Value}, Acc) -> Acc
		end,
		nothing, Query).

user_parse_query1(Action, User, Server, Query) ->
    case ejabberd_hooks:run_fold(webadmin_user_parse_query,
				 Server, [], [Action, User, Server, Query])
	of
      [] -> nothing;
      Res -> Res
    end.

list_last_activity(Host, Lang, Integral, Period) ->
    TimeStamp = erlang:system_time(second),
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
	  if Hist == [] -> [?CT(?T("No Data"))];
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
				 (integer_to_binary(trunc(90 * V / Max)))/binary,
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

search_running_node(SNode) ->
    RunningNodes = ejabberd_cluster:get_nodes(),
    search_running_node(SNode, RunningNodes).

search_running_node(_, []) -> false;
search_running_node(SNode, [Node | Nodes]) ->
    case iolist_to_binary(atom_to_list(Node)) of
      SNode -> Node;
      _ -> search_running_node(SNode, Nodes)
    end.

get_node(global, Node, [], #request{lang = Lang}) ->
    Base = get_base_path(global, Node, 2),
    BaseItems = [{<<"db">>, <<"Mnesia Tables">>},
                 {<<"backup">>, <<"Mnesia Backup">>}],
    MenuItems = make_menu_items(global, Node, Base, Lang, BaseItems),
    [?XC(<<"h1">>,
	 (str:translate_and_format(Lang, ?T("Node ~p"), [Node])))]
      ++
	[?XE(<<"ul">>, MenuItems)];
get_node(Host, Node, [], #request{lang = Lang}) ->
    Base = get_base_path(Host, Node, 4),
    MenuItems2 = make_menu_items(Host, Node, Base, Lang, []),
    [?XC(<<"h1">>, (str:translate_and_format(Lang, ?T("Node ~p"), [Node]))),
     ?XE(<<"ul">>, MenuItems2)];

get_node(global, Node, [<<"db">> | RPath], R) ->
    PageTitle = <<"Mnesia Tables">>,
    Title = ?XC(<<"h1">>, PageTitle),
    Level = length(RPath),
    [Title, ?BR | webadmin_db(Node, RPath, R, Level)];

get_node(global, Node, [<<"backup">>], #request{lang = Lang} = R) ->
    Types = [{<<"#binary">>, <<"Binary">>},
             {<<"#plaintext">>, <<"Plain Text">>},
             {<<"#piefxis">>, <<"PIEXFIS (XEP-0227)">>},
             {<<"#sql">>, <<"SQL">>},
             {<<"#prosody">>, <<"Prosody">>},
             {<<"#jabberd14">>, <<"jabberd 1.4">>}],

    [?XC(<<"h1">>, (str:translate_and_format(Lang, ?T("Backup of ~p"), [Node]))),
     ?XCT(<<"p">>,
          ?T("Please note that these options will "
             "only backup the builtin Mnesia database. "
             "If you are using the ODBC module, you "
             "also need to backup your SQL database "
             "separately.")),
     ?XE(<<"ul">>, [?LI([?AC(MIU, MIN)]) || {MIU, MIN} <- Types]),

     ?X(<<"hr">>),
     ?XAC(<<"h2">>, [{<<"id">>, <<"binary">>}], <<"Binary">>),
     ?XCT(<<"p">>, ?T("Store binary backup:")),
     ?XE(<<"blockquote">>, [make_command(backup, R)]),
     ?XCT(<<"p">>, ?T("Restore binary backup immediately:")),
     ?XE(<<"blockquote">>, [make_command(restore, R, [], [{style, danger}])]),
     ?XCT(<<"p">>, ?T("Restore binary backup after next ejabberd "
                      "restart (requires less memory):")),
     ?XE(<<"blockquote">>, [make_command(install_fallback, R, [], [{style, danger}])]),

     ?X(<<"hr">>),
     ?XAC(<<"h2">>, [{<<"id">>, <<"plaintext">>}], <<"Plain Text">>),
     ?XCT(<<"p">>, ?T("Store plain text backup:")),
     ?XE(<<"blockquote">>, [make_command(dump, R)]),
     ?XCT(<<"p">>, ?T("Restore plain text backup immediately:")),
     ?XE(<<"blockquote">>, [make_command(load, R, [], [{style, danger}])]),

     ?X(<<"hr">>),
     ?XAC(<<"h2">>, [{<<"id">>, <<"piefxis">>}], <<"PIEFXIS (XEP-0227)">>),
     ?XCT(<<"p">>, ?T("Import users data from a PIEFXIS file (XEP-0227):")),
     ?XE(<<"blockquote">>, [make_command(import_piefxis, R)]),
     ?XCT(<<"p">>, ?T("Export data of all users in the server to PIEFXIS files (XEP-0227):")),
     ?XE(<<"blockquote">>, [make_command(export_piefxis, R)]),
     ?XCT(<<"p">>, ?T("Export data of users in a host to PIEFXIS files (XEP-0227):")),
     ?XE(<<"blockquote">>, [make_command(export_piefxis_host, R)]),

     ?X(<<"hr">>),
     ?XAC(<<"h2">>, [{<<"id">>, <<"sql">>}], <<"SQL">>),
     ?XCT(<<"p">>, ?T("Export all tables as SQL queries to a file:")),
     ?XE(<<"blockquote">>, [make_command(export2sql, R)]),

     ?X(<<"hr">>),
     ?XAC(<<"h2">>, [{<<"id">>, <<"prosody">>}], <<"Prosody">>),
     ?XCT(<<"p">>, <<"Import data from Prosody:">>),
     ?XE(<<"blockquote">>, [make_command(import_prosody, R)]),

     ?X(<<"hr">>),
     ?XAC(<<"h2">>, [{<<"id">>, <<"jabberd14">>}], <<"jabberd 1.4">>),
     ?XCT(<<"p">>, ?T("Import user data from jabberd14 spool file:")),
     ?XE(<<"blockquote">>, [make_command(import_file, R)]),
     ?XCT(<<"p">>, ?T("Import users data from jabberd14 spool directory:")),
     ?XE(<<"blockquote">>, [make_command(import_dir, R)])
    ];
get_node(Host, Node, _NPath, Request) ->
    Res = case Host of
	      global ->
		  ejabberd_hooks:run_fold(webadmin_page_node, Host, [],
					  [Node, Request]);
	      _ ->
		  ejabberd_hooks:run_fold(webadmin_page_hostnode, Host, [],
					  [Host, Node, Request])
	  end,
    case Res of
      [] -> [?XC(<<"h1">>, <<"Not Found">>)];
      _ -> Res
    end.

%%%==================================
%%%% node parse

pretty_print_xml(El) ->
    list_to_binary(pretty_print_xml(El, <<"">>)).

pretty_print_xml({xmlcdata, CData}, Prefix) ->
    IsBlankCData = lists:all(
                     fun($\f) -> true;
                        ($\r) -> true;
                        ($\n) -> true;
                        ($\t) -> true;
                        ($\v) -> true;
                        ($\s) -> true;
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
	   [$\s, Attr, $=, $', fxml:crypt(Val) | [$',
                                                 lists:map(fun ({Attr1,
                                                                 Val1}) ->
                                                                   [$\n,
                                                                    AttrPrefix,
                                                                    Attr1, $=,
                                                                    $',
                                                                    fxml:crypt(Val1),
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
		   [$>, fxml:get_cdata(Els), $<, $/, Name, $>, $\n];
	       true ->
		   [$>, $\n,
		    lists:map(fun (E) ->
				      pretty_print_xml(E, [Prefix, <<"  ">>])
			      end,
			      Els),
		    Prefix, $<, $/, Name, $>, $\n]
	    end
     end].

url_func({user_diapason, From, To}) ->
    <<"diapason/", (integer_to_binary(From))/binary, "-",
      (integer_to_binary(To))/binary, "/">>.

last_modified() ->
    {<<"Last-Modified">>,
     <<"Mon, 25 Feb 2008 13:23:30 GMT">>}.

cache_control_public() ->
    {<<"Cache-Control">>, <<"public">>}.

%% Transform 1234567890 into "1,234,567,890"
pretty_string_int(Integer) when is_integer(Integer) ->
    pretty_string_int(integer_to_binary(Integer));
pretty_string_int(String) when is_binary(String) ->
    {_, Result} = lists:foldl(fun (NewNumber, {3, Result}) ->
				      {1, <<NewNumber, $,, Result/binary>>};
				  (NewNumber, {CountAcc, Result}) ->
				      {CountAcc + 1, <<NewNumber, Result/binary>>}
			      end,
			      {0, <<"">>}, lists:reverse(binary_to_list(String))),
    Result.

%%%==================================
%%%% mnesia table view

webadmin_node_db_table_page(Node, STable, PageNumber) ->
    Table = misc:binary_to_atom(STable),
    TInfo = ejabberd_cluster:call(Node, mnesia, table_info, [Table, all]),
    {value, {storage_type, Type}} = lists:keysearch(storage_type, 1, TInfo),
    {value, {size, Size}} = lists:keysearch(size, 1, TInfo),
    PageSize = 500,
    TableContentErl = get_table_content(Node, Table, Type, PageNumber, PageSize),
    TableContent = str:format("~p", [TableContentErl]),
    PagesLinks = build_elements_pages_list(Size, PageNumber, PageSize),
    [?P] ++ PagesLinks ++ [?XC(<<"pre">>, TableContent)].

build_elements_pages_list(Size, PageNumber, PageSize) ->
    PagesNumber = calculate_pages_number(Size, PageSize),
    PagesSeq = lists:seq(1, PagesNumber),
    PagesList = [?AC(<<"../", (integer_to_binary(N))/binary, "/">>,
         <<(integer_to_binary(N))/binary, " ">>)
     || N <- PagesSeq],
    lists:keyreplace(
        [?C(<<(integer_to_binary(PageNumber))/binary, " ">>)],
        4,
        PagesList,
        ?C(<<" [", (integer_to_binary(PageNumber))/binary, "] ">>)).

calculate_pages_number(Size, PageSize) ->
    Remainder = case Size rem PageSize of
                   0 -> 0;
                   _ -> 1
               end,
    case (Size div PageSize) + Remainder of
        1 -> 0;
        Res -> Res
    end.

get_table_content(Node, Table, _Type, PageNumber, PageSize) ->
    Keys1 = lists:sort(ejabberd_cluster:call(Node, mnesia, dirty_all_keys, [Table])),
    FirstKeyPos = 1 - PageSize + PageNumber*PageSize,
    Keys = lists:sublist(Keys1, FirstKeyPos, PageSize),
    Res = [ejabberd_cluster:call(Node, mnesia, dirty_read, [Table, Key])
           || Key <- Keys],
    lists:flatten(Res).

%% @format-begin

webadmin_db(Node, [<<"table">>, TableName, <<"details">> | RPath], R, Level) ->
    Service = <<"Mnesia Tables">>,
    Breadcrumb =
        make_breadcrumb({table_section, Level, Service, TableName, <<"Details">>, RPath}),
    Get = [ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [mnesia_table_details, R, [{<<"table">>, TableName}], []])],
    Breadcrumb ++ Get;
webadmin_db(Node,
            [<<"table">>, TableName, <<"elements">>, PageNumber | RPath],
            R,
            Level) ->
    Service = <<"Mnesia Tables">>,
    Breadcrumb =
        make_breadcrumb({table_section, Level, Service, TableName, <<"Elements">>, RPath}),
    Get = [ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [webadmin_node_db_table_page,
                                  R,
                                  [{<<"node">>, Node},
                                   {<<"table">>, TableName},
                                   {<<"page">>, PageNumber}],
                                  []])],
    Breadcrumb ++ Get;
webadmin_db(Node, [<<"table">>, TableName, <<"change-storage">> | RPath], R, Level) ->
    Service = <<"Mnesia Tables">>,
    Breadcrumb =
        make_breadcrumb({table_section, Level, Service, TableName, <<"Change Storage">>, RPath}),
    Set = [ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [mnesia_table_change_storage, R, [{<<"table">>, TableName}], []])],
    Breadcrumb ++ Set;
webadmin_db(Node, [<<"table">>, TableName, <<"clear">> | RPath], R, Level) ->
    Service = <<"Mnesia Tables">>,
    Breadcrumb =
        make_breadcrumb({table_section, Level, Service, TableName, <<"Clear Content">>, RPath}),
    Set = [ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [mnesia_table_clear,
                                  R,
                                  [{<<"table">>, TableName}],
                                  [{style, danger}]])],
    Breadcrumb ++ Set;
webadmin_db(Node, [<<"table">>, TableName, <<"destroy">> | RPath], R, Level) ->
    Service = <<"Mnesia Tables">>,
    Breadcrumb =
        make_breadcrumb({table_section, Level, Service, TableName, <<"Destroy Table">>, RPath}),
    Set = [ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [mnesia_table_destroy,
                                  R,
                                  [{<<"table">>, TableName}],
                                  [{style, danger}]])],
    Breadcrumb ++ Set;
webadmin_db(_Node, [<<"table">>, TableName | _RPath], _R, Level) ->
    Service = <<"Mnesia Tables">>,
    Breadcrumb = make_breadcrumb({table, Level, Service, TableName}),
    MenuItems =
        [{<<"details/">>, <<"Details">>},
         {<<"elements/1/">>, <<"Elements">>},
         {<<"change-storage/">>, <<"Change Storage">>},
         {<<"clear/">>, <<"Clear Content">>},
         {<<"destroy/">>, <<"Destroy Table">>}],
    Get = [?XE(<<"ul">>, [?LI([?AC(MIU, MIN)]) || {MIU, MIN} <- MenuItems])],
    Breadcrumb ++ Get;
webadmin_db(Node, _RPath, R, _Level) ->
    Service = <<"Mnesia Tables">>,
    Breadcrumb = make_breadcrumb({service, Service}),
    Get = [ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [mnesia_list_tables,
                                  R,
                                  [],
                                  [{result_links, [{name, mnesia_table, 3, <<"">>}]}]])],
    Breadcrumb ++ Get.

make_breadcrumb({service, Service}) ->
    make_breadcrumb([Service]);
make_breadcrumb({table, Level, Service, Name}) ->
    make_breadcrumb([{Level, Service}, separator, Name]);
make_breadcrumb({table_section, Level, Service, Name, Section, RPath}) ->
    make_breadcrumb([{Level, Service}, separator, {Level - 2, Name}, separator, Section
                     | RPath]);
make_breadcrumb(Elements) ->
    lists:map(fun ({xmlel, _, _, _} = Xmlel) ->
                      Xmlel;
                  (<<"sort">>) ->
                      ?C(<<" +">>);
                  (<<"page">>) ->
                      ?C(<<" #">>);
                  (separator) ->
                      ?C(<<" > ">>);
                  (Bin) when is_binary(Bin) ->
                      ?C(Bin);
                  ({Level, Bin}) when is_integer(Level) and is_binary(Bin) ->
                      ?AC(binary:copy(<<"../">>, Level), Bin)
              end,
              Elements).
%% @format-end

%%%==================================
%%%% navigation menu

make_navigation(Host, Node, Username, Lang, JID, Level) ->
    Menu = make_navigation_menu(Host, Node, Username, Lang, JID, Level),
    make_menu_items(Lang, Menu).

-spec make_navigation_menu(Host::global | binary(),
                           Node::cluster | atom(),
                           Username::unspecified | binary(),
                           Lang::binary(), JID::jid(), Level::integer()) ->
    Menu::{URL::binary(), Title::binary()}
    | {URL::binary(), Title::binary(), [Menu::any()]}.
make_navigation_menu(Host, Node, Username, Lang, JID, Level) ->
    HostNodeMenu = make_host_node_menu(Host, Node, Lang,
				       JID, Level),
    HostUserMenu = make_host_user_menu(Host, Username, Lang,
                                       JID, Level),
    HostMenu = make_host_menu(Host, HostNodeMenu, HostUserMenu, Lang,
			      JID, Level),
    NodeMenu = make_node_menu(Host, Node, Lang, Level),
    make_server_menu(HostMenu, NodeMenu, Lang, JID, Level).

make_menu_items(Host, Node, Base, Lang, Acc) ->
    Place = case {Host, Node} of
                {global, cluster} -> server;
                {global, Node} -> {node, Node};
                {Host, cluster} -> {host, Host};
                {Host, Node} -> {hostnode, Host, Node}
            end,
    HookItems = get_menu_items_hook(Place, Lang),
    Items = lists:keysort(2, HookItems ++ Acc),
    make_menu_items(Lang, {Base, <<"">>, Items}).

make_host_node_menu(global, _, _Lang, _JID, _Level) ->
    {<<"">>, <<"">>, []};
make_host_node_menu(_, cluster, _Lang, _JID, _Level) ->
    {<<"">>, <<"">>, []};
make_host_node_menu(Host, Node, Lang, JID, Level) ->
    HostNodeBase = get_base_path(Host, Node, Level),
    HostNodeFixed = get_menu_items_hook({hostnode, Host, Node}, Lang),
    HostNodeFixed2 = [Tuple
		      || Tuple <- HostNodeFixed,
			 is_allowed_path(Host, Tuple, JID)],
    {HostNodeBase, iolist_to_binary(atom_to_list(Node)),
     lists:keysort(2, HostNodeFixed2)}.

make_host_user_menu(global, _, _Lang, _JID, _Level) ->
    {<<"">>, <<"">>, []};
make_host_user_menu(_, unspecified, _Lang, _JID, _Level) ->
    {<<"">>, <<"">>, []};
make_host_user_menu(Host, Username, Lang, JID, Level) ->
    HostNodeBase = get_base_path(Host, Username, Level),
    HostNodeFixed = get_menu_items_hook({hostuser, Host, Username}, Lang),
    HostNodeFixed2 = [Tuple
		      || Tuple <- HostNodeFixed,
			 is_allowed_path(Host, Tuple, JID)],
    {HostNodeBase, Username,
     lists:keysort(2, HostNodeFixed2)}.

make_host_menu(global, _HostNodeMenu, _HostUserMenu, _Lang, _JID, _Level) ->
    {<<"">>, <<"">>, []};
make_host_menu(Host, HostNodeMenu, HostUserMenu, Lang, JID, Level) ->
    HostBase = get_base_path(Host, cluster, Level),
    HostFixed = [{<<"users">>, ?T("Users"), HostUserMenu},
		 {<<"online-users">>, ?T("Online Users")}],
    HostFixedAdditional =
		  get_lastactivity_menuitem_list(Host) ++
		    [{<<"nodes">>, ?T("Nodes"), HostNodeMenu}]
		      ++ get_menu_items_hook({host, Host}, Lang),
    HostFixedAll = HostFixed ++ lists:keysort(2, HostFixedAdditional),
    HostFixed2 = [Tuple
		  || Tuple <- HostFixedAll,
		     is_allowed_path(Host, Tuple, JID)],
    {HostBase, Host, HostFixed2}.

make_node_menu(_Host, cluster, _Lang, _Level) ->
    {<<"">>, <<"">>, []};
make_node_menu(global, Node, Lang, Level) ->
    NodeBase = get_base_path(global, Node, Level),
    NodeFixed = [{<<"db">>, <<"Mnesia Tables">>},
		 {<<"backup">>, <<"Mnesia Backup">>}]
		  ++ get_menu_items_hook({node, Node}, Lang),
    {NodeBase, iolist_to_binary(atom_to_list(Node)),
     lists:keysort(2, NodeFixed)};
make_node_menu(_Host, _Node, _Lang, _Level) ->
    {<<"">>, <<"">>, []}.

make_server_menu(HostMenu, NodeMenu, Lang, JID, Level) ->
    Base = get_base_path(global, cluster, Level),
    Fixed = [{<<"vhosts">>, ?T("Virtual Hosts"), HostMenu},
	     {<<"nodes">>, ?T("Nodes"), NodeMenu}],
    FixedAdditional = get_menu_items_hook(server, Lang),
    FixedAll = Fixed ++ lists:keysort(2, FixedAdditional),
    Fixed2 = [Tuple
	      || Tuple <- FixedAll,
		 is_allowed_path(global, Tuple, JID)],
    {Base, <<"">>, Fixed2}.

get_menu_items_hook({hostnode, Host, Node}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_hostnode, Host,
			    [], [Host, Node, Lang]);
get_menu_items_hook({hostuser, Host, Username}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_hostuser, Host,
			    [], [Host, Username, Lang]);
get_menu_items_hook({host, Host}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_host, Host, [],
			    [Host, Lang]);
get_menu_items_hook({node, Node}, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_node, [],
			    [Node, Lang]);
get_menu_items_hook(server, Lang) ->
    ejabberd_hooks:run_fold(webadmin_menu_main, [], [Lang]).

-spec make_menu_items(Lang::binary(),
                      {MURI::binary(), MName::binary(),
                       Items::[{IURI::binary(), IName::binary()}
                               | {IURI::binary(), IName::binary(), Menu::any()}]}) ->
    [xmlel()].
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

any_rules_allowed(Host, Access, Entity) ->
    lists:any(
      fun(Rule) ->
	      allow == acl:match_rule(Host, Rule, Entity)
      end, Access).

%%%==================================
%%%% login box

%%% @format-begin

make_login_items(#request{us = {Username, Host}} = R, Level) ->
    UserBin =
        jid:encode(
            jid:make(Username, Host, <<"">>)),
    UserEl =
        make_command(echo,
                     R,
                     [{<<"sentence">>, UserBin}],
                     [{only, value}, {result_links, [{sentence, user, Level, <<"">>}]}]),
    UserEl2 =
        case UserEl of
            {xmlcdata, <<>>} ->
                {xmlel, <<"code">>, [], [{xmlel, <<"a">>, [], [{xmlcdata, UserBin}]}]};
            _ ->
                UserEl
        end,
    MenuPost =
        case ejabberd_hooks:run_fold(webadmin_menu_system_post, [], [R, Level]) of
            [] ->
                [];
            PostElements ->
                [{xmlel,
                  <<"div">>,
                  [{<<"id">>, <<"navitemlogin">>}],
                  [?XE(<<"ul">>, PostElements)]}]
        end,
    [{xmlel,
      <<"li">>,
      [{<<"id">>, <<"navitemlogin-start">>}],
      [{xmlel,
        <<"div">>,
        [{<<"id">>, <<"navitemlogin">>}],
        [?XE(<<"ul">>,
             [?LI([?C(unicode:characters_to_binary("👤")), UserEl2]),
              ?LI([?C(unicode:characters_to_binary("🏭")),
                   make_command(echo,
                                R,
                                [{<<"sentence">>, misc:atom_to_binary(node())}],
                                [{only, value},
                                 {result_links, [{sentence, node, Level, <<"">>}]}])])]
             ++ ejabberd_hooks:run_fold(webadmin_menu_system_inside, [], [R, Level])
             ++ [?LI([?C(unicode:characters_to_binary("📤")),
                      ?AC(<<(binary:copy(<<"../">>, Level))/binary, "logout/">>,
                          <<"Logout">>)])])]}]
      ++ MenuPost}].

%%%==================================
%%%% menu_system

-spec make_menu_system(atom(), string(), string(), string()) -> [xmlel()].
make_menu_system(Module, Icon, Text, Append) ->
    [make_menu_system_el(Icon, Text, Append, UrlTuple) || UrlTuple <- get_urls(Module)].

get_urls(Module) ->
    Urls = ejabberd_http:get_auto_urls(any, Module),
    Host = ejabberd_config:get_myname(),
    [{Tls, misc:expand_keyword(<<"@HOST@">>, Url, Host)} || {Tls, Url} <- Urls].

-spec make_menu_system_el(string(), string(), string(), {boolean(), binary()}) -> xmlel().
make_menu_system_el(Icon, Text, Append, {ThisTls, Url}) ->
    LockBinary =
        case ThisTls of
            true ->
                unicode:characters_to_binary("🔒");
            false ->
                unicode:characters_to_binary("❗")
        end,
    AppendBin = iolist_to_binary(Append),
    ?LI([?C(<<(unicode:characters_to_binary(Icon))/binary, LockBinary/binary>>),
         ?XAE(<<"a">>,
              [{<<"href">>, <<Url/binary, AppendBin/binary>>}, {<<"target">>, <<"_blank">>}],
              [?C(unicode:characters_to_binary(Text))])]).

%%%==================================

%%%% make_command: API

-spec make_command(Name :: atom(), Request :: http_request()) -> xmlel().
make_command(Name, Request) ->
    make_command2(Name, Request, [], []).

-spec make_command(Name :: atom(),
                   Request :: http_request(),
                   BaseArguments :: [{ArgName :: binary(), ArgValue :: binary()}],
                   [Option]) ->
                      xmlel() | {xmlcdata, binary()} | {raw_and_value, any(), xmlel()}
    when Option ::
             {only, presentation | without_presentation | button | result | value | raw_and_value} |
             {input_name_append, [binary()]} |
             {force_execution, boolean()} |
             {table_options, {PageSize :: integer(), RemainingPath :: [binary()]}} |
             {result_named, boolean()} |
             {result_links,
              [{ResultName :: atom(),
                LinkType :: host | node | user | room | shared_roster | arg_host | paragraph,
                Level :: integer(),
                Append :: binary()}]} |
             {style, normal | danger}.
make_command(Name, Request, BaseArguments, Options) ->
    make_command2(Name, Request, BaseArguments, Options).

-spec make_command_raw_value(Name :: atom(),
                             Request :: http_request(),
                             BaseArguments :: [{ArgName :: binary(), ArgValue :: binary()}]) ->
                                any().
make_command_raw_value(Name, Request, BaseArguments) ->
    make_command2(Name, Request, BaseArguments, [{only, raw_value}]).

%%%==================================
%%%% make_command: main

-spec make_command2(Name :: atom(),
                    Request :: http_request(),
                    BaseArguments :: [{ArgName :: binary(), ArgValue :: binary()}],
                    [Option]) ->
                       xmlel() | any()
    when Option ::
             {only,
              presentation |
              without_presentation |
              button |
              result |
              value |
              raw_value |
              raw_and_value} |
             {input_name_append, [binary()]} |
             {force_execution, boolean() | undefined} |
             {table_options, {PageSize :: integer(), RemainingPath :: [binary()]}} |
             {result_named, boolean()} |
             {result_links,
              [{ResultName :: atom(),
                LinkType :: host | node | user | room | shared_roster | arg_host | paragraph,
                Level :: integer(),
                Append :: binary()}]} |
             {style, normal | danger}.
make_command2(Name, Request, BaseArguments, Options) ->
    Only = proplists:get_value(only, Options, all),
    ForceExecution = proplists:get_value(force_execution, Options, undefined),
    InputNameAppend = proplists:get_value(input_name_append, Options, []),
    Resultnamed = proplists:get_value(result_named, Options, false),
    ResultLinks = proplists:get_value(result_links, Options, []),
    TO = proplists:get_value(table_options, Options, {999999, []}),
    Style = proplists:get_value(style, Options, normal),
    #request{us = {RUser, RServer}, ip = RIp} = Request,
    CallerInfo =
        #{usr => {RUser, RServer, <<"">>},
          ip => RIp,
          caller_host => RServer,
          caller_module => ?MODULE},
    try {ejabberd_commands:get_command_definition(Name),
         ejabberd_access_permissions:can_access(Name, CallerInfo)}
    of
        {C, allow} ->
            make_command2(Name,
                          Request,
                          CallerInfo,
                          BaseArguments,
                          C,
                          Only,
                          ForceExecution,
                          InputNameAppend,
                          Resultnamed,
                          ResultLinks,
                          Style,
                          TO);
        {_C, deny} ->
            ?DEBUG("Blocked access to command ~p for~n CallerInfo: ~p", [Name, CallerInfo]),
            ?C(<<"">>)
    catch
        A:B ->
            ?INFO_MSG("Problem preparing command ~p: ~p", [Name, {A, B}]),
            ?C(<<"">>)
    end.

make_command2(Name,
              Request,
              CallerInfo,
              BaseArguments,
              C,
              Only,
              ForceExecution,
              InputNameAppend,
              Resultnamed,
              ResultLinks,
              Style,
              TO) ->
    {ArgumentsFormat, _Rename, ResultFormatApi} = ejabberd_commands:get_command_format(Name),
    Method =
        case {ForceExecution, ResultFormatApi} of
            {true, _} ->
                auto;
            {false, _} ->
                manual;
            {_, {_, rescode}} ->
                manual;
            {_, {_, restuple}} ->
                manual;
            _ ->
                auto
        end,
    PresentationEls = make_command_presentation(Name, C#ejabberd_commands.tags),
    Query = Request#request.q,
    {ArgumentsUsed1, ExecRes} =
        execute_command(Name,
                        Query,
                        BaseArguments,
                        Method,
                        ArgumentsFormat,
                        CallerInfo,
                        InputNameAppend),
    ArgumentsFormatDetailed =
        add_arguments_details(ArgumentsFormat,
                              C#ejabberd_commands.args_desc,
                              C#ejabberd_commands.args_example),
    ArgumentsEls =
        make_command_arguments(Name,
                               Query,
                               Only,
                               Method,
                               Style,
                               ArgumentsFormatDetailed,
                               BaseArguments,
                               InputNameAppend),
    Automated =
        case ArgumentsEls of
            [] ->
                true;
            _ ->
                false
        end,
    ArgumentsUsed =
        (catch lists:zip(
                   lists:map(fun({A, _}) -> A end, ArgumentsFormat), ArgumentsUsed1)),
    ResultEls =
        make_command_result(ExecRes,
                            ArgumentsUsed,
                            ResultFormatApi,
                            Automated,
                            Resultnamed,
                            ResultLinks,
                            TO),
    make_command3(Only, ExecRes, PresentationEls, ArgumentsEls, ResultEls).

make_command3(presentation, _ExecRes, PresentationEls, _ArgumentsEls, _ResultEls) ->
    ?XAE(<<"p">>, [{<<"class">>, <<"api">>}], PresentationEls);
make_command3(button, _ExecRes, _PresentationEls, [Button], _ResultEls) ->
    Button;
make_command3(result,
              _ExecRes,
              _PresentationEls,
              _ArgumentsEls,
              [{xmlcdata, _}, Xmlel]) ->
    ?XAE(<<"p">>, [{<<"class">>, <<"api">>}], [Xmlel]);
make_command3(value, _ExecRes, _PresentationEls, _ArgumentsEls, [{xmlcdata, _}, Xmlel]) ->
    Xmlel;
make_command3(value,
              _ExecRes,
              _PresentationEls,
              _ArgumentsEls,
              [{xmlel, _, _, _} = Xmlel]) ->
    Xmlel;
make_command3(raw_and_value,
              ExecRes,
              _PresentationEls,
              _ArgumentsEls,
              [{xmlel, _, _, _} = Xmlel]) ->
    {raw_and_value, ExecRes, Xmlel};
make_command3(raw_value, ExecRes, _PresentationEls, _ArgumentsEls, _ResultEls) ->
    ExecRes;
make_command3(without_presentation,
              _ExecRes,
              _PresentationEls,
              ArgumentsEls,
              ResultEls) ->
    ?XAE(<<"p">>,
         [{<<"class">>, <<"api">>}],
         [?XE(<<"blockquote">>, ArgumentsEls ++ ResultEls)]);
make_command3(all, _ExecRes, PresentationEls, ArgumentsEls, ResultEls) ->
    ?XAE(<<"p">>,
         [{<<"class">>, <<"api">>}],
         PresentationEls ++ [?XE(<<"blockquote">>, ArgumentsEls ++ ResultEls)]).

add_arguments_details(ArgumentsFormat, Descriptions, none) ->
    add_arguments_details(ArgumentsFormat, Descriptions, []);
add_arguments_details(ArgumentsFormat, none, Examples) ->
    add_arguments_details(ArgumentsFormat, [], Examples);
add_arguments_details(ArgumentsFormat, Descriptions, Examples) ->
    lists_zipwith3(fun({A, B}, C, D) -> {A, B, C, D} end,
                   ArgumentsFormat,
                   Descriptions,
                   Examples,
                   {pad, {none, "", ""}}).

-ifdef(OTP_BELOW_26).

lists_zipwith3(Combine, List1, List2, List3, {pad, {DefaultX, DefaultY, DefaultZ}}) ->
    lists_zipwith3(Combine, List1, List2, List3, DefaultX, DefaultY, DefaultZ, []).

lists_zipwith3(_Combine, [], [], [], _DefaultX, _DefaultY, _DefaultZ, Res) ->
    lists:reverse(Res);
lists_zipwith3(Combine,
               [E1 | List1],
               [E2 | List2],
               [E3 | List3],
               DefX,
               DefY,
               DefZ,
               Res) ->
    E123 = Combine(E1, E2, E3),
    lists_zipwith3(Combine, List1, List2, List3, DefX, DefY, DefZ, [E123 | Res]);
lists_zipwith3(Combine, [E1 | List1], [], [], DefX, DefY, DefZ, Res) ->
    E123 = Combine(E1, DefY, DefZ),
    lists_zipwith3(Combine, List1, [], [], DefX, DefY, DefZ, [E123 | Res]);
lists_zipwith3(Combine, [E1 | List1], [], [E3 | List3], DefX, DefY, DefZ, Res) ->
    E123 = Combine(E1, DefY, E3),
    lists_zipwith3(Combine, List1, [], List3, DefX, DefY, DefZ, [E123 | Res]);
lists_zipwith3(Combine, [E1 | List1], [E2 | List2], [], DefX, DefY, DefZ, Res) ->
    E123 = Combine(E1, E2, DefZ),
    lists_zipwith3(Combine, List1, List2, [], DefX, DefY, DefZ, [E123 | Res]).

-endif.

-ifndef(OTP_BELOW_26).

lists_zipwith3(Combine, List1, List2, List3, How) ->
    lists:zipwith3(Combine, List1, List2, List3, How).

-endif.

%%%==================================
%%%% make_command: presentation

make_command_presentation(Name, Tags) ->
    NameBin = misc:atom_to_binary(Name),
    NiceNameBin = nice_this(Name),
    Text = ejabberd_ctl:get_usage_command(atom_to_list(Name), 100, false, 1000000),
    AnchorLink = [?ANCHORL(NameBin)],
    MaybeDocsLink =
        case lists:member(internal, Tags) of
            true ->
                [];
            false ->
                [?GL(<<"developer/ejabberd-api/admin-api/#", NameBin/binary>>, NameBin)]
        end,
    [?XE(<<"details">>,
         [?XAE(<<"summary">>, [{<<"id">>, NameBin}], [?XC(<<"strong">>, NiceNameBin)])]
         ++ MaybeDocsLink
         ++ AnchorLink
         ++ [?XC(<<"pre">>, list_to_binary(Text))])].

nice_this(This, integer) ->
    {nice_this(This), right};
nice_this(This, _Format) ->
    nice_this(This).

-spec nice_this(This :: atom() | string() | [byte()]) -> NiceThis :: binary().
nice_this(This) when is_atom(This) ->
    nice_this(atom_to_list(This));
nice_this(This) when is_binary(This) ->
    nice_this(binary_to_list(This));
nice_this(This) when is_list(This) ->
    list_to_binary(lists:flatten([string:titlecase(Word)
                                  || Word <- string:replace(This, "_", " ", all)])).

-spec long_this(These :: [This :: atom()]) -> Long :: binary().
long_this(These) ->
    list_to_binary(lists:join($/, [atom_to_list(This) || This <- These])).

%%%==================================
%%%% make_command: arguments

make_command_arguments(Name,
                       Query,
                       Only,
                       Method,
                       Style,
                       ArgumentsFormat,
                       BaseArguments,
                       InputNameAppend) ->
    ArgumentsFormat2 = remove_base_arguments(ArgumentsFormat, BaseArguments),
    ArgumentsFields = make_arguments_fields(Name, Query, ArgumentsFormat2),
    Button = make_button_element(Name, Method, Style, InputNameAppend),
    ButtonElement =
        ?XE(<<"tr">>,
            [?X(<<"td">>), ?XAE(<<"td">>, [{<<"class">>, <<"alignright">>}], [Button])]),
    case {(ArgumentsFields /= []) or (Method == manual), Only} of
        {false, _} ->
            [];
        {true, button} ->
            [?XAE(<<"form">>, [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}], [Button])];
        {true, _} ->
            [?XAE(<<"form">>,
                  [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
                  [?XE(<<"table">>, ArgumentsFields ++ [ButtonElement])])]
    end.

remove_base_arguments(ArgumentsFormat, BaseArguments) ->
    lists:filter(fun({ArgName, _ArgFormat, _ArgDesc, _ArgExample}) ->
                    not
                        lists:keymember(
                            misc:atom_to_binary(ArgName), 1, BaseArguments)
                 end,
                 ArgumentsFormat).

make_button_element(Name, _, Style, InputNameAppend) ->
    Id = term_to_id(InputNameAppend),
    NameBin = <<(misc:atom_to_binary(Name))/binary, Id/binary>>,
    NiceNameBin = nice_this(Name),
    case Style of
        danger ->
            ?INPUTD(<<"submit">>, NameBin, NiceNameBin);
        _ ->
            ?INPUT(<<"submit">>, NameBin, NiceNameBin)
    end.

make_arguments_fields(Name, Query, ArgumentsFormat) ->
    lists:map(fun({ArgName, ArgFormat, _ArgDescription, ArgExample}) ->
                 ArgExampleBin = format_result(ArgExample, {ArgName, ArgFormat}),
                 ArgNiceNameBin = nice_this(ArgName),
                 ArgLongNameBin = long_this([Name, ArgName]),
                 ArgValue =
                     case lists:keysearch(ArgLongNameBin, 1, Query) of
                         {value, {ArgLongNameBin, V}} ->
                             V;
                         _ ->
                             <<"">>
                     end,
                 ?XE(<<"tr">>,
                     [?XC(<<"td">>, <<ArgNiceNameBin/binary, ":">>),
                      ?XE(<<"td">>,
                          [?INPUTPH(<<"text">>, ArgLongNameBin, ArgValue, ArgExampleBin)])])
              end,
              ArgumentsFormat).

%%%==================================
%%%% make_command: execute

execute_command(Name,
                Query,
                BaseArguments,
                Method,
                ArgumentsFormat,
                CallerInfo,
                InputNameAppend) ->
    try Args = prepare_arguments(Name, BaseArguments ++ Query, ArgumentsFormat),
        {Args,
         execute_command2(Name, Query, Args, Method, ArgumentsFormat, CallerInfo, InputNameAppend)}
    of
        R ->
            R
    catch
        A:E ->
            {error, {A, E}}
    end.

execute_command2(Name,
                 Query,
                 Arguments,
                 Method,
                 ArgumentsFormat,
                 CallerInfo,
                 InputNameAppend) ->
    AllArgumentsProvided = length(Arguments) == length(ArgumentsFormat),
    PressedExecuteButton = is_this_to_execute(Name, Query, Arguments, InputNameAppend),
    LetsExecute =
        case {Method, PressedExecuteButton, AllArgumentsProvided} of
            {auto, _, true} ->
                true;
            {manual, true, true} ->
                true;
            _ ->
                false
        end,
    case LetsExecute of
        true ->
            catch ejabberd_commands:execute_command2(Name, Arguments, CallerInfo);
        false ->
            not_executed
    end.

is_this_to_execute(Name, Query, Arguments, InputNameAppend) ->
    NiceNameBin = nice_this(Name),
    NameBin = misc:atom_to_binary(Name),
    AppendBin = term_to_id(lists:sublist(Arguments, length(InputNameAppend))),
    ArgumentsId = <<NameBin/binary, AppendBin/binary>>,
    {value, {ArgumentsId, NiceNameBin}} == lists:keysearch(ArgumentsId, 1, Query).

prepare_arguments(ComName, Args, ArgsFormat) ->
    lists:foldl(fun({ArgName, ArgFormat}, FinalArguments) ->
                   %% Give priority to the value enforced in our code
                   %% Otherwise use the value provided by the user
                   case {lists:keyfind(
                             misc:atom_to_binary(ArgName), 1, Args),
                         lists:keyfind(long_this([ComName, ArgName]), 1, Args)}
                   of
                       %% Value enforced in our code
                       {{_, Value}, _} ->
                           [format_arg(Value, ArgFormat) | FinalArguments];
                       %% User didn't provide value in the field
                       {_, {_, <<>>}} ->
                           FinalArguments;
                       %% Value provided by the user in the form field
                       {_, {_, Value}} ->
                           [format_arg(Value, ArgFormat) | FinalArguments];
                       {false, false} ->
                           FinalArguments
                   end
                end,
                [],
                lists:reverse(ArgsFormat)).

format_arg(Value, any) ->
    Value;
format_arg(Value, atom) when is_atom(Value) ->
    Value;
format_arg(Value, binary) when is_binary(Value) ->
    Value;
format_arg(Value, ArgFormat) ->
    ejabberd_ctl:format_arg(binary_to_list(Value), ArgFormat).

%%%==================================
%%%% make_command: result

make_command_result(not_executed, _, _, _, _, _, _) ->
    [];
make_command_result({error, ErrorElement}, _, _, _, _, _, _) ->
    [?DIVRES([?C(<<"Error: ">>),
              ?XC(<<"code">>, list_to_binary(io_lib:format("~p", [ErrorElement])))])];
make_command_result(Value,
                    ArgumentsUsed,
                    {ResName, _ResFormat} = ResultFormatApi,
                    Automated,
                    Resultnamed,
                    ResultLinks,
                    TO) ->
    ResNameBin = nice_this(ResName),
    ResultValueEl =
        make_command_result_element(ArgumentsUsed, Value, ResultFormatApi, ResultLinks, TO),
    ResultEls =
        case Resultnamed of
            true ->
                [?C(<<ResNameBin/binary, ": ">>), ResultValueEl];
            false ->
                [ResultValueEl]
        end,
    case Automated of
        true ->
            ResultEls;
        false ->
            [?DIVRES(ResultEls)]
    end.

make_command_result_element(ArgumentsUsed,
                            ListOfTuples,
                            {_ArgName, {list, {_ListElementsName, {tuple, TupleElements}}}},
                            ResultLinks,
                            {PageSize, RPath}) ->
    HeadElements =
        [nice_this(ElementName, ElementFormat) || {ElementName, ElementFormat} <- TupleElements],
    ContentElements =
        [list_to_tuple([make_result(format_result(V, {ElementName, ElementFormat}),
                                    ElementName,
                                    ArgumentsUsed,
                                    ResultLinks)
                        || {V, {ElementName, ElementFormat}}
                               <- lists:zip(tuple_to_list(Tuple), TupleElements)])
         || Tuple <- ListOfTuples],
    make_table(PageSize, RPath, HeadElements, ContentElements);
make_command_result_element(_ArgumentsUsed,
                            Values,
                            {_ArgName, {tuple, TupleElements}},
                            _ResultLinks,
                            _TO) ->
    ?XE(<<"table">>,
        [?XE(<<"thead">>,
             [?XE(<<"tr">>,
                  [?XC(<<"td">>, nice_this(ElementName))
                   || {ElementName, _ElementFormat} <- TupleElements])]),
         ?XE(<<"tbody">>,
             [?XE(<<"tr">>,
                  [?XE(<<"td">>,
                       [?XAC(<<"span">>,
                             [{<<"style">>, <<"white-space: pre-wrap;">>}],
                             format_result(V, {ElementName, ElementFormat}))])
                   || {V, {ElementName, ElementFormat}}
                          <- lists:zip(tuple_to_list(Values), TupleElements)])])]);
make_command_result_element(ArgumentsUsed,
                            Value,
                            {_ArgName, {list, {ElementsName, ElementsFormat}}},
                            ResultLinks,
                            {PageSize, RPath}) ->
    HeadElements = [nice_this(ElementsName)],
    ContentElements =
        [{make_result(format_result(V, {ElementsName, ElementsFormat}),
                      ElementsName,
                      ArgumentsUsed,
                      ResultLinks)}
         || V <- Value],
    make_table(PageSize, RPath, HeadElements, ContentElements);
make_command_result_element(ArgumentsUsed, Value, ResultFormatApi, ResultLinks, _TO) ->
    Res = make_result(format_result(Value, ResultFormatApi),
                      unknown_element_name,
                      ArgumentsUsed,
                      ResultLinks),
    Res2 =
        case Res of
            [{xmlel, _, _, _} | _] = X ->
                X;
            Z ->
                [Z]
        end,
    ?XE(<<"code">>, Res2).

make_result(Binary, ElementName, ArgumentsUsed, [{ResultName, arg_host, Level, Append}])
    when (ElementName == ResultName) or (ElementName == unknown_element_name) ->
    {_, Host} = lists:keyfind(host, 1, ArgumentsUsed),
    UrlBinary =
        replace_url_elements([<<"server/">>, host, <<"/">>, Append], [{host, Host}], Level),
    ?AC(UrlBinary, Binary);
make_result(Binary, ElementName, _ArgumentsUsed, [{ResultName, host, Level, Append}])
    when (ElementName == ResultName) or (ElementName == unknown_element_name) ->
    UrlBinary =
        replace_url_elements([<<"server/">>, host, <<"/">>, Append], [{host, Binary}], Level),
    ?AC(UrlBinary, Binary);
make_result(Binary,
            ElementName,
            _ArgumentsUsed,
            [{ResultName, mnesia_table, Level, Append}])
    when (ElementName == ResultName) or (ElementName == unknown_element_name) ->
    Node = misc:atom_to_binary(node()),
    UrlBinary =
        replace_url_elements([<<"node/">>, node, <<"/db/table/">>, tablename, <<"/">>, Append],
                             [{node, Node}, {tablename, Binary}],
                             Level),
    ?AC(UrlBinary, Binary);
make_result(Binary, ElementName, _ArgumentsUsed, [{ResultName, node, Level, Append}])
    when (ElementName == ResultName) or (ElementName == unknown_element_name) ->
    UrlBinary =
        replace_url_elements([<<"node/">>, node, <<"/">>, Append], [{node, Binary}], Level),
    ?AC(UrlBinary, Binary);
make_result(Binary, ElementName, _ArgumentsUsed, [{ResultName, user, Level, Append}])
    when (ElementName == ResultName) or (ElementName == unknown_element_name) ->
    Jid = try jid:decode(Binary) of
              #jid{} = J ->
                  J
          catch
              _:{bad_jid, _} ->
                  %% TODO: Find a method to be able to link to this user to delete it
                  ?INFO_MSG("Error parsing Binary that is not a valid JID:~n  ~p", [Binary]),
                  jid:decode(<<"unknown-username@localhost">>)
          end,
    {User, Host, _R} = jid:split(Jid),
    case lists:member(Host, ejabberd_config:get_option(hosts)) of
        true ->
            UrlBinary =
                replace_url_elements([<<"server/">>, host, <<"/user/">>, user, <<"/">>, Append],
                                     [{user, misc:url_encode(User)}, {host, Host}],
                                     Level),
            ?AC(UrlBinary, Binary);
        false ->
            ?C(Binary)
    end;
make_result(Binary, ElementName, _ArgumentsUsed, [{ResultName, room, Level, Append}])
    when (ElementName == ResultName) or (ElementName == unknown_element_name) ->
    Jid = jid:decode(Binary),
    {Roomname, Service, _} = jid:split(Jid),
    Host = ejabberd_router:host_of_route(Service),
    case lists:member(Host, ejabberd_config:get_option(hosts)) of
        true ->
            UrlBinary =
                replace_url_elements([<<"server/">>,
                                      host,
                                      <<"/muc/rooms/room/">>,
                                      room,
                                      <<"/">>,
                                      Append],
                                     [{room, misc:url_encode(Roomname)}, {host, Host}],
                                     Level),
            ?AC(UrlBinary, Binary);
        false ->
            ?C(Binary)
    end;
make_result(Binary,
            ElementName,
            ArgumentsUsed,
            [{ResultName, shared_roster, Level, Append}])
    when (ElementName == ResultName) or (ElementName == unknown_element_name) ->
    First = proplists:get_value(first, ArgumentsUsed),
    Second = proplists:get_value(second, ArgumentsUsed),
    FirstUrlencoded =
        list_to_binary(string:replace(
                           misc:url_encode(First), "%40", "@")),
    {GroupId, Host} =
        case jid:decode(FirstUrlencoded) of
            #jid{luser = <<"">>, server = G} ->
                {G, Second};
            #jid{user = G, lserver = H} ->
                {G, H}
        end,
    UrlBinary =
        replace_url_elements([<<"server/">>,
                              host,
                              <<"/shared-roster/group/">>,
                              srg,
                              <<"/">>,
                              Append],
                             [{host, Host}, {srg, GroupId}],
                             Level),
    ?AC(UrlBinary, Binary);
make_result([{xmlcdata, _, _, _} | _] = Any,
            _ElementName,
            _ArgumentsUsed,
            _ResultLinks) ->
    Any;
make_result([{xmlel, _, _, _} | _] = Any, _ElementName, _ArgumentsUsed, _ResultLinks) ->
    Any;
make_result(Binary,
            ElementName,
            _ArgumentsUsed,
            [{ResultName, paragraph, _Level, _Append}])
    when (ElementName == ResultName) or (ElementName == unknown_element_name) ->
    ?XC(<<"pre">>, Binary);
make_result(Binary, _ElementName, _ArgumentsUsed, _ResultLinks) ->
    ?C(Binary).

replace_url_elements(UrlComponents, Replacements, Level) ->
    Base = get_base_path_sum(0, 0, Level),
    Binary2 =
        lists:foldl(fun (El, Acc) when is_binary(El) ->
                            [El | Acc];
                        (El, Acc) when is_atom(El) ->
                            {El, Value} = lists:keyfind(El, 1, Replacements),
                            [Value | Acc]
                    end,
                    [],
                    UrlComponents),
    Binary3 =
        binary:list_to_bin(
            lists:reverse(Binary2)),
    <<Base/binary, Binary3/binary>>.

format_result(Value, {_ResultName, integer}) when is_integer(Value) ->
    integer_to_binary(Value);
format_result(Value, {_ResultName, string}) when is_list(Value) ->
    Value;
format_result(Value, {_ResultName, string}) when is_binary(Value) ->
    Value;
format_result(Value, {_ResultName, atom}) when is_atom(Value) ->
    misc:atom_to_binary(Value);
format_result(Value, {_ResultName, any}) ->
    Value;
format_result({ok, String}, {_ResultName, restuple}) when is_list(String) ->
    list_to_binary(String);
format_result({error, Type, Code, Desc}, {_ResultName, restuple}) ->
    <<"Error: ",
      (misc:atom_to_binary(Type))/binary,
      " ",
      (integer_to_binary(Code))/binary,
      ": ",
      (list_to_binary(Desc))/binary>>;
format_result([], {_Name, {list, _ElementsDef}}) ->
    "";
format_result([FirstElement | Elements], {_Name, {list, ElementsDef}}) ->
    Separator = ",",
    Head = format_result(FirstElement, ElementsDef),
    Tail =
        lists:map(fun(Element) -> [Separator | format_result(Element, ElementsDef)] end,
                  Elements),
    [Head | Tail];
format_result([], {_Name, {tuple, _ElementsDef}}) ->
    "";
format_result(Value, {_Name, {tuple, [FirstDef | ElementsDef]}}) ->
    [FirstElement | Elements] = tuple_to_list(Value),
    Separator = ":",
    Head = format_result(FirstElement, FirstDef),
    Tail =
        lists:map(fun(Element) -> [Separator | format_result(Element, ElementsDef)] end,
                  Elements),
    [Head | Tail];
format_result(Value, _ResultFormat) when is_atom(Value) ->
    misc:atom_to_binary(Value);
format_result(Value, _ResultFormat) when is_list(Value) ->
    list_to_binary(Value);
format_result(Value, _ResultFormat) when is_binary(Value) ->
    Value;
format_result(Value, _ResultFormat) ->
    io_lib:format("~p", [Value]).

%%%==================================
%%%% make_table

-spec make_table(PageSize :: integer(),
                 RemainingPath :: [binary()],
                 NameOptionList :: [Name :: binary() | {Name :: binary(), left | right}],
                 Values :: [tuple()]) ->
                    xmlel().
make_table(PageSize, RPath, NameOptionList, Values1) ->
    Values =
        case lists:member(<<"sort">>, RPath) of
            true ->
                Values1;
            false ->
                GetXmlValue =
                    fun ({xmlcdata, _} = X) ->
                            X;
                        ({xmlel, _, _, _} = X) ->
                            X;
                        ({raw_and_value, _V, X}) ->
                            X
                    end,
                ConvertTupleToTuple =
                    fun(Row1) -> list_to_tuple(lists:map(GetXmlValue, tuple_to_list(Row1))) end,
                lists:map(ConvertTupleToTuple, Values1)
        end,
    make_table1(PageSize, RPath, <<"">>, <<"">>, 1, NameOptionList, Values).

make_table1(PageSize,
            [<<"page">>, PageNumber | RPath],
            PageUrlBase,
            SortUrlBase,
            _Start,
            NameOptionList,
            Values1) ->
    make_table1(PageSize,
                RPath,
                <<PageUrlBase/binary, "../../">>,
                <<SortUrlBase/binary, "../../">>,
                1 + PageSize * binary_to_integer(PageNumber),
                NameOptionList,
                Values1);
make_table1(PageSize,
            [<<"sort">>, SortType | RPath],
            PageUrlBase,
            SortUrlBase,
            Start,
            NameOptionList,
            Rows1) ->
    ColumnToSort =
        length(lists:takewhile(fun (A) when A == SortType ->
                                       false;
                                   ({A, _}) when A == SortType ->
                                       false;
                                   (_) ->
                                       true
                               end,
                               NameOptionList))
        + 1,
    Direction =
        case lists:nth(ColumnToSort, NameOptionList) of
            {_, right} ->
                descending;
            {_, left} ->
                ascending;
            _ ->
                ascending
        end,
    ColumnToSort = ColumnToSort,
    GetRawValue =
        fun ({xmlcdata, _} = X) ->
                X;
            ({xmlel, _, _, _} = X) ->
                X;
            ({raw_and_value, R, _X}) ->
                R
        end,
    GetXmlValue =
        fun ({xmlcdata, _} = X) ->
                X;
            ({xmlel, _, _, _} = X) ->
                X;
            ({raw_and_value, _R, X}) ->
                X
        end,
    SortTwo =
        fun(A1, B1) ->
           A2 = GetRawValue(element(ColumnToSort, A1)),
           B2 = GetRawValue(element(ColumnToSort, B1)),
           case Direction of
               ascending ->
                   A2 < B2;
               descending ->
                   A2 > B2
           end
        end,
    Rows1Sorted = lists:sort(SortTwo, Rows1),
    ConvertTupleToTuple =
        fun(Row1) -> list_to_tuple(lists:map(GetXmlValue, tuple_to_list(Row1))) end,
    Rows = lists:map(ConvertTupleToTuple, Rows1Sorted),
    make_table1(PageSize,
                RPath,
                PageUrlBase,
                <<SortUrlBase/binary, "../../">>,
                Start,
                NameOptionList,
                Rows);
make_table1(PageSize, [], PageUrlBase, SortUrlBase, Start, NameOptionList, Values1) ->
    Values = lists:sublist(Values1, Start, PageSize),
    Table = make_table(NameOptionList, Values),
    Size = length(Values1),
    Remaining =
        case Size rem PageSize of
            0 ->
                0;
            _ ->
                1
        end,
    NumPages = max(0, Size div PageSize + Remaining - 1),
    PLinks1 =
        lists:foldl(fun(N, Acc) ->
                       NBin = integer_to_binary(N),
                       Acc
                       ++ [?C(<<", ">>),
                           ?AC(<<PageUrlBase/binary, "page/", NBin/binary, "/">>, NBin)]
                    end,
                    [],
                    lists:seq(1, NumPages)),
    PLinks =
        case PLinks1 of
            [] ->
                [];
            _ ->
                [?XE(<<"p">>, [?C(<<"Page: ">>), ?AC(<<PageUrlBase/binary>>, <<"0">>) | PLinks1])]
        end,

    Names =
        lists:map(fun ({Name, _}) ->
                          Name;
                      (Name) ->
                          Name
                  end,
                  NameOptionList),
    [_ | SLinks1] =
        lists:foldl(fun(N, Acc) ->
                       [?C(<<", ">>), ?AC(<<SortUrlBase/binary, "sort/", N/binary, "/">>, N) | Acc]
                    end,
                    [],
                    lists:reverse(Names)),
    SLinks =
        case {PLinks, SLinks1} of
            {_, []} ->
                [];
            {[], _} ->
                [];
            {_, [_]} ->
                [];
            {_, SLinks2} ->
                [?XE(<<"p">>, [?C(<<"Sort all pages by: ">>) | SLinks2])]
        end,

    ?XE(<<"div">>, [Table | PLinks ++ SLinks]).

-spec make_table(NameOptionList :: [Name :: binary() | {Name :: binary(), left | right}],
                 Values :: [tuple()]) ->
                    xmlel().
make_table(NameOptionList, Values) ->
    NamesAndAttributes = [make_column_attributes(NameOption) || NameOption <- NameOptionList],
    {Names, ColumnsAttributes} = lists:unzip(NamesAndAttributes),
    make_table(Names, ColumnsAttributes, Values).

make_table(Names, ColumnsAttributes, Values) ->
    ?XAE(<<"table">>,
         [{<<"class">>, <<"sortable">>}],
         [?XE(<<"thead">>,
              [?XE(<<"tr">>, [?XC(<<"th">>, nice_this(HeadElement)) || HeadElement <- Names])]),
          ?XE(<<"tbody">>,
              [?XE(<<"tr">>,
                   [?XAE(<<"td">>, CAs, [V])
                    || {CAs, V} <- lists:zip(ColumnsAttributes, tuple_to_list(ValueTuple))])
               || ValueTuple <- Values])]).

make_column_attributes({Name, Option}) ->
    {Name, [make_column_attribute(Option)]};
make_column_attributes(Name) ->
    {Name, []}.

make_column_attribute(left) ->
    {<<"class">>, <<"alignleft">>};
make_column_attribute(right) ->
    {<<"class">>, <<"alignright">>}.

%%%==================================
%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
